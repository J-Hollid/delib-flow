;;; delib-flow-ui.el --- UI controller for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer, mode, and interaction control for delib-flow.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)

(require 'delib-flow-config)

(eval-and-compile
  (unless (fboundp 'delib-flow--define-function)
    (defmacro delib-flow--define-function (name args &rest body)
      "Define NAME with ARGS and BODY through a shared wrapper macro."
      (declare (indent defun))
      `(defalias ',name
         (lambda ,args
           ,@body)))))

(defconst delib-flow--action-shortcut-keys
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
    "b" "c" "d" "e" "f" "h" "i" "k" "l"
    "o" "t" "u" "v" "w" "x" "y")
  "Single-key shortcuts reserved for rendered actions.")

(defvar-local delib-flow-control-focus-mode nil
  "Whether the current control buffer is in narrow-screen focus mode.")

(defvar-local delib-flow--surface-kind 'cockpit
  "Surface kind for the current delib-flow buffer.")

(defvar-local delib-flow--changed-heading-overlay nil
  "Transient overlay highlighting the last changed control-buffer heading.")

(defvar-local delib-flow--consequence-preview-kind nil
  "Current consequence preview kind for the active side preview buffer.")

(defvar-local delib-flow--note-part-editor-title nil
  "Current note-part title for the active note-part editor buffer.")

(defvar-local delib-flow--note-part-editor-apply-function nil
  "Function used to apply the current note-part editor contents.")

(defvar-local delib-flow--field-editor-title nil
  "Current field title for the active field editor buffer.")

(defvar-local delib-flow--field-editor-apply-function nil
  "Function used to apply the current field editor contents.")

(defvar delib-flow--sticky-anchor-heading nil
  "Preferred control-buffer anchor preserved across stage-lifecycle rerenders.")

(defvar delib-flow--active-run nil
  "Active run state for the current delib-flow session.")

(defvar delib-flow--in-flight-ui-timer nil
  "Timer used to refresh async in-flight cockpit indicators.")

(defun delib-flow--in-flight-request-id ()
  "Return a fresh request id for async stage execution."
  (format "delib-flow-request-%s" (float-time)))

(defun delib-flow--in-flight-decision-text (stage-id provider model)
  "Return operator-facing current-decision text for in-flight STAGE-ID."
  (format "Waiting on %s %s via %s%s."
          (if (eq provider 'cloud) "cloud" "local LLM")
          (delib-flow--stage-label stage-id)
          provider
          (if (and model (not (string-empty-p model)))
              (format " (%s)" model)
            "")))

(defun delib-flow--mark-stage-in-flight (run stage-id provider model handle request-id started-at)
  "Return RUN marked in-flight for STAGE-ID with PROVIDER and MODEL."
  (let ((session (delib-flow--run-session run)))
    (plist-put
     run :session
     (plist-put
      (plist-put
       (plist-put
        (plist-put
         (plist-put
          (plist-put
           (plist-put
            (plist-put session :current-stage stage-id)
            :current-decision
            (delib-flow--in-flight-decision-text stage-id provider model))
           :in-flight-stage-id stage-id)
          :in-flight-provider provider)
         :in-flight-model model)
        :in-flight-started-at started-at)
       :in-flight-request-id request-id)
      :in-flight-handle handle))))

(defun delib-flow--clear-stage-in-flight (run)
  "Return RUN with any in-flight stage markers cleared."
  (let ((session (delib-flow--run-session run)))
    (plist-put
     run :session
     (plist-put
      (plist-put
       (plist-put
        (plist-put
         (plist-put
          (plist-put session :in-flight-stage-id nil)
          :in-flight-provider nil)
         :in-flight-model nil)
        :in-flight-started-at nil)
       :in-flight-request-id nil)
      :in-flight-handle nil))))

(defun delib-flow--normalize-in-flight-state (run)
  "Return RUN with dead-process in-flight markers cleared."
  (let* ((session (and run (delib-flow--run-session run)))
         (stage-id (plist-get session :in-flight-stage-id))
         (request-id (plist-get session :in-flight-request-id))
         (handle (plist-get session :in-flight-handle)))
    (if (and stage-id request-id
             (processp handle)
             (not (process-live-p handle)))
        (delib-flow--clear-stage-in-flight run)
      run)))

(defun delib-flow--cancel-in-flight-stage (run)
  "Cancel any live in-flight stage process for RUN."
  (when run
    (let ((handle (plist-get (delib-flow--run-session run) :in-flight-handle)))
      (when (processp handle)
        (ignore-errors
          (delete-process handle))))))

(defun delib-flow--control-buffer ()
  "Return the control buffer when it exists."
  (get-buffer delib-flow-control-buffer-name))

(defun delib-flow--filing-workspace-buffer ()
  "Return the focused filing workspace buffer when it exists."
  (get-buffer delib-flow-filing-workspace-buffer-name))

(defun delib-flow--in-flight-request-current-p (request-id)
  "Return non-nil when REQUEST-ID still belongs to `delib-flow--active-run'."
  (and delib-flow--active-run
       (string= request-id
                (or (plist-get (delib-flow--run-session delib-flow--active-run)
                               :in-flight-request-id)
                    ""))))

(defun delib-flow--refresh-in-flight-ui ()
  "Refresh any visible in-flight cockpit indicators."
  (if (and delib-flow--active-run
           (delib-flow--run-in-flight-p delib-flow--active-run))
      (when-let ((buffer (delib-flow--control-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t)
                  (anchor (or delib-flow--sticky-anchor-heading
                              (delib-flow--preferred-anchor-section
                               delib-flow--active-run))))
              (dolist (section '("Now" "Current result"))
                (delib-flow--replace-section-content
                 section
                 (delib-flow--section-content section delib-flow--active-run)))
              (delib-flow--protect-managed-regions)
              (unless (delib-flow--goto-section anchor)
                (goto-char (point-min)))
              (delib-flow--align-heading-top))
            (force-mode-line-update t))))
    (when (timerp delib-flow--in-flight-ui-timer)
      (cancel-timer delib-flow--in-flight-ui-timer)
      (setq delib-flow--in-flight-ui-timer nil))))

(defun delib-flow--ensure-in-flight-ui-timer ()
  "Ensure the cockpit has a live timer for async in-flight indicators."
  (unless (timerp delib-flow--in-flight-ui-timer)
    (setq delib-flow--in-flight-ui-timer
          (run-at-time 0 1 #'delib-flow--refresh-in-flight-ui))))

(defun delib-flow--action-placeholder-p (action)
  "Return non-nil when ACTION is a placeholder."
  (eq (plist-get action :status) 'placeholder))

(defun delib-flow--active-run-conflict-p ()
  "Return non-nil when an active run already exists."
  (and delib-flow--active-run
       (delib-flow--run-active-p delib-flow--active-run)
       (buffer-live-p (delib-flow--control-buffer))))

(defun delib-flow--cleanup-stale-run ()
  "Clear stale active-run state when the control buffer is gone."
  (when (and delib-flow--active-run
             (not (buffer-live-p (delib-flow--control-buffer))))
    (delib-flow--teardown-active-run)))

(defun delib-flow--control-header-line ()
  "Return header-line text for the control buffer."
  (let ((status
         (if (and delib-flow--active-run
                  (delib-flow--run-in-flight-p delib-flow--active-run))
             (let* ((stage-id
                     (delib-flow--run-in-flight-stage-id delib-flow--active-run))
                    (model
                     (delib-flow--run-in-flight-model delib-flow--active-run))
                    (started-at
                     (delib-flow--run-in-flight-started-at delib-flow--active-run)))
               (format " [%s %s%s %ss]"
                       (delib-flow--spinner-frame-for-time started-at)
                       (delib-flow--stage-label stage-id)
                       (if (and model (not (string-empty-p model)))
                           (format " %s" model)
                         "")
                       (delib-flow--elapsed-seconds started-at)))
           "")))
    (format
     (if (eq delib-flow--surface-kind 'filing-workspace)
         " Delib-Flow filing workspace%s%s: n/p sections, TAB actions, RET run, B cockpit, E/I/P/V preview, g refresh, q cockpit "
       " Delib-Flow cockpit%s%s: n/p sections, TAB actions, RET run, F filing, L loop, K preview, U history, z focus, g refresh, q quit ")
     (if (and (eq delib-flow--surface-kind 'cockpit)
              delib-flow-control-focus-mode)
         " [focus]"
       "")
     status)))

(defun delib-flow--editable-block-marker (block)
  "Return the begin marker string for BLOCK."
  (format "#+begin_delib-edit %s"
          (delib-flow--editable-block-kind-name block)))

(defun delib-flow--editable-block-text-in-buffer (buffer block)
  "Return editable block text from BUFFER for BLOCK."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (delib-flow--editable-block-marker block) nil t)
        (forward-line 1)
        (let ((begin (point)))
          (when (search-forward "#+end_delib-edit" nil t)
            (string-remove-suffix
             "\n"
             (buffer-substring-no-properties
              begin
              (match-beginning 0)))))))))

(defun delib-flow--sync-editable-block (run buffer block-id)
  "Return RUN after syncing BLOCK-ID text from BUFFER."
  (let* ((block (delib-flow--editable-block run block-id))
         (text (delib-flow--editable-block-text-in-buffer buffer block)))
    (if text
        (delib-flow--set-editable-block
         run block-id (delib-flow--set-editable-block-text block text))
      run)))

(defun delib-flow--sync-editable-blocks (run buffer)
  "Return RUN after syncing editable block contents from BUFFER."
  (let ((updated-run run))
    (dolist (block-id (plist-get (delib-flow--run-working-context run)
                                 :editable-block-ids)
                      updated-run)
      (setq updated-run
            (delib-flow--sync-editable-block updated-run buffer block-id)))))

(defun delib-flow--section-heading (section)
  "Return the top-level heading text for SECTION."
  (format "** %s" section))

(defun delib-flow--next-section-heading-regexp ()
  "Return the regexp matching the next top-level control section."
  "^\\*\\* [^\n]+$")

(defun delib-flow--section-content-bounds (section)
  "Return the content bounds for SECTION in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (delib-flow--section-heading section) nil t)
      (forward-line 1)
      (let ((start (point)))
        (if (re-search-forward (delib-flow--next-section-heading-regexp) nil t)
            (cons start (match-beginning 0))
          (cons start (point-max)))))))

(defun delib-flow--replace-section-content (section content)
  "Replace SECTION contents in the current buffer with CONTENT."
  (let ((bounds (delib-flow--section-content-bounds section)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert content)
      (unless (bolp)
        (insert "\n")))))

(defun delib-flow--editable-block-body-bounds ()
  "Return bounds for editable block bodies in the current buffer."
  (let (bounds)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_delib-edit [^\n]+$" nil t)
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "^#\\+end_delib-edit$" nil t)
            (push (cons start
                        (match-beginning 0))
                  bounds)))))
    (nreverse bounds)))

(defun delib-flow--protect-managed-regions ()
  "Protect managed buffer regions in generated cockpit surfaces."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t delib-flow-managed t))
    (dolist (bounds (delib-flow--editable-block-body-bounds))
      (add-text-properties
       (car bounds) (cdr bounds)
       '(read-only t
         delib-flow-managed t
         delib-flow-editable nil
         front-sticky nil
         rear-nonsticky (read-only))))))

(defun delib-flow--section-present-p (section)
  "Return non-nil when SECTION heading is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward (delib-flow--section-heading section) nil t)))

(defun delib-flow--editable-block-present-p (run block-id)
  "Return non-nil when BLOCK-ID markers from RUN exist in the current buffer."
  (let ((marker
         (delib-flow--editable-block-marker
          (delib-flow--editable-block run block-id))))
    (save-excursion
      (goto-char (point-min))
      (and (search-forward marker nil t)
           (search-forward "#+end_delib-edit" nil t)))))

(defun delib-flow--managed-region-conflicts (run buffer)
  "Return a list of managed-region conflicts for RUN in BUFFER."
  (with-current-buffer buffer
    (let (conflicts)
      (dolist (section delib-flow--control-sections)
        (unless (delib-flow--section-present-p section)
          (push (format "Missing section: %s" section) conflicts)))
      (dolist (block-id (plist-get (delib-flow--run-working-context run)
                                   :editable-block-ids))
        (unless (delib-flow--editable-block-present-p run block-id)
          (push (format "Missing editable block: %s" block-id) conflicts)))
      (nreverse conflicts))))

(defun delib-flow--set-managed-region-conflicts (run conflicts)
  "Return RUN with managed-region CONFLICTS stored in UI state."
  (plist-put run :ui
             (plist-put (delib-flow--run-ui run)
                        :managed-region-conflicts
                        conflicts)))

(defun delib-flow--note-part-editor-header-line ()
  "Return header-line text for the note-part editor buffer."
  (format
   " Delib-Flow note editor%s: edit text directly, C-c C-c apply, C-c C-k cancel "
   (if (delib-flow--non-empty-string-p delib-flow--note-part-editor-title)
       (format " [%s]" delib-flow--note-part-editor-title)
     "")))

(defun delib-flow--field-editor-header-line ()
  "Return header-line text for the field editor buffer."
  (format
   " Delib-Flow field editor%s: type normally here, C-c C-c apply, C-c C-k cancel "
   (if (delib-flow--non-empty-string-p delib-flow--field-editor-title)
       (format " [%s]" delib-flow--field-editor-title)
     "")))

(defun delib-flow--configure-buffer-for-terminal ()
  "Apply conservative readability defaults for terminal control buffers."
  (unless (display-graphic-p)
    (setq-local truncate-lines nil)
    (setq-local word-wrap t)
    (setq-local line-spacing nil)
    (setq-local bidi-display-reordering nil)
    (setq-local cursor-in-non-selected-windows nil)))

(defun delib-flow--revert-control-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the active control buffer through standard revert semantics."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow-refresh-buffer))

(defvar delib-flow-note-part-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'delib-flow-note-part-editor-apply)
    (define-key map (kbd "C-c C-k") #'delib-flow-note-part-editor-cancel)
    map)
  "Keymap for `delib-flow-note-part-editor-mode'.")

(defvar delib-flow-field-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'delib-flow-field-editor-apply)
    (define-key map (kbd "C-c C-k") #'delib-flow-field-editor-cancel)
    map)
  "Keymap for `delib-flow-field-editor-mode'.")

(defvar delib-flow-control-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'delib-flow-control-refresh)
    (define-key map (kbd "j") #'delib-flow-control-open-audit-run)
    (define-key map (kbd "J") #'delib-flow-control-open-audit-latest-stage)
    (define-key map (kbd "D") #'delib-flow-control-debug-open-latest-stage-inspection)
    (define-key map (kbd "C") #'delib-flow-control-debug-open-comparison)
    (define-key map (kbd "W") #'delib-flow-control-debug-open-walkthrough)
    (define-key map (kbd "H") #'delib-flow-control-debug-apply-helper)
    (define-key map (kbd "N") #'delib-flow-control-debug-walkthrough-next-step)
    (define-key map (kbd "R") #'delib-flow-control-debug-walkthrough-restart-target)
    (define-key map (kbd "F") #'delib-flow-control-open-filing-workspace)
    (define-key map (kbd "B") #'delib-flow-control-return-main-cockpit)
    (define-key map (kbd "n") #'delib-flow-control-next-section)
    (define-key map (kbd "p") #'delib-flow-control-previous-section)
    (define-key map (kbd "TAB") #'delib-flow-control-next-action)
    (define-key map (kbd "<backtab>") #'delib-flow-control-previous-action)
    (define-key map (kbd "RET") #'delib-flow-control-dispatch-action)
    (define-key map (kbd "a") #'delib-flow-control-dispatch-action)
    (define-key map (kbd ".") #'delib-flow-control-context-menu)
    (define-key map (kbd "A") #'delib-flow-control-approve-current)
    (define-key map (kbd "E") #'delib-flow-control-peek-selected-draft)
    (define-key map (kbd "I") #'delib-flow-control-peek-selected-support)
    (define-key map (kbd "P") #'delib-flow-control-peek-filing-target)
    (define-key map (kbd "V") #'delib-flow-control-peek-staged-content)
    (define-key map (kbd "L") #'delib-flow-control-jump-active-loop)
    (define-key map (kbd "K") #'delib-flow-control-jump-latest-preview)
    (define-key map (kbd "U") #'delib-flow-control-jump-stage-history)
    (define-key map (kbd "m") #'delib-flow-control-choose-manual-project)
    (define-key map (kbd "r") #'delib-flow-control-retry-current)
    (define-key map (kbd "q") #'delib-flow-control-abort-run)
    (define-key map (kbd "s") #'delib-flow-control-choose-filing-selection)
    (define-key map (kbd "T") #'delib-flow-control-choose-reference-note-template)
    (define-key map (kbd "z") #'delib-flow-control-toggle-focus-mode)
    (define-key map (kbd "?") #'delib-flow-control-help-command)
    (dolist (key delib-flow--action-shortcut-keys)
      (define-key map (kbd key) #'delib-flow-control-dispatch-shortcut))
    map)
  "Keymap for `delib-flow-control-mode'.")

(defvar delib-flow-filing-workspace-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'delib-flow-control-refresh)
    (define-key map (kbd "F") #'delib-flow-control-open-filing-workspace)
    (define-key map (kbd "B") #'delib-flow-control-return-main-cockpit)
    (define-key map (kbd "q") #'delib-flow-control-return-main-cockpit)
    (define-key map (kbd "n") #'delib-flow-control-next-section)
    (define-key map (kbd "p") #'delib-flow-control-previous-section)
    (define-key map (kbd "TAB") #'delib-flow-control-next-action)
    (define-key map (kbd "<backtab>") #'delib-flow-control-previous-action)
    (define-key map (kbd "RET") #'delib-flow-control-dispatch-action)
    (define-key map (kbd "a") #'delib-flow-control-dispatch-action)
    (define-key map (kbd ".") #'delib-flow-control-context-menu)
    (define-key map (kbd "s") #'delib-flow-control-choose-filing-selection)
    (define-key map (kbd "O") #'delib-flow-control-open-clean-source)
    (define-key map (kbd "E") #'delib-flow-control-peek-selected-draft)
    (define-key map (kbd "I") #'delib-flow-control-peek-selected-support)
    (define-key map (kbd "P") #'delib-flow-control-peek-filing-target)
    (define-key map (kbd "V") #'delib-flow-control-peek-staged-content)
    (define-key map (kbd "?") #'delib-flow-control-help-command)
    (dolist (key delib-flow--action-shortcut-keys)
      (define-key map (kbd key) #'delib-flow-control-dispatch-shortcut))
    map)
  "Keymap for `delib-flow-filing-workspace-mode'.")

(defvar delib-flow-source-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "B") #'delib-flow-control-return-main-cockpit)
    (define-key map (kbd "F") #'delib-flow-control-open-filing-workspace)
    (define-key map (kbd "O") #'delib-flow-control-open-clean-source)
    (define-key map (kbd "g") #'delib-flow-control-refresh)
    map)
  "Keymap for `delib-flow-source-view-mode'.")

(define-derived-mode delib-flow-control-mode org-mode "Delib-Flow"
  "Major mode for the DeliberateFlow control buffer."
  (setq-local header-line-format '(:eval (delib-flow--control-header-line)))
  (setq-local revert-buffer-function #'delib-flow--revert-control-buffer)
  (setq-local show-trailing-whitespace nil)
  (setq-local line-move-ignore-invisible t)
  (setq-local delib-flow--surface-kind 'cockpit)
  (delib-flow--configure-buffer-for-terminal))

(define-derived-mode delib-flow-filing-workspace-mode org-mode "Delib-Flow Filing"
  "Major mode for the DeliberateFlow filing workspace."
  (setq-local header-line-format '(:eval (delib-flow--control-header-line)))
  (setq-local revert-buffer-function #'delib-flow--revert-control-buffer)
  (setq-local show-trailing-whitespace nil)
  (setq-local line-move-ignore-invisible t)
  (setq-local delib-flow--surface-kind 'filing-workspace)
  (delib-flow--configure-buffer-for-terminal))

(define-derived-mode delib-flow-note-part-editor-mode text-mode "Delib-Flow Note Edit"
  "Major mode for editing one selected note part."
  (setq-local header-line-format '(:eval (delib-flow--note-part-editor-header-line)))
  (setq-local show-trailing-whitespace nil)
  (setq-local require-final-newline nil)
  (setq-local delib-flow--surface-kind 'note-part-editor)
  (delib-flow--configure-buffer-for-terminal))

(define-derived-mode delib-flow-field-editor-mode text-mode "Delib-Flow Field Edit"
  "Major mode for editing one operator field."
  (setq-local header-line-format '(:eval (delib-flow--field-editor-header-line)))
  (setq-local show-trailing-whitespace nil)
  (setq-local require-final-newline nil)
  (setq-local delib-flow--surface-kind 'field-editor)
  (delib-flow--configure-buffer-for-terminal))

(define-derived-mode delib-flow-source-view-mode org-mode "Delib-Flow Source"
  "Major mode for the cleaned source view buffer."
  (setq-local header-line-format
              " Delib-Flow clean source: q close, B cockpit, F filing, g refresh ")
  (setq-local show-trailing-whitespace nil)
  (setq-local line-move-ignore-invisible t)
  (setq-local delib-flow--surface-kind 'source-view)
  (delib-flow--configure-buffer-for-terminal))

(put 'delib-flow-control-mode 'mode-class 'special)
(put 'delib-flow-filing-workspace-mode 'mode-class 'special)
(put 'delib-flow-note-part-editor-mode 'mode-class 'special)
(put 'delib-flow-field-editor-mode 'mode-class 'special)
(put 'delib-flow-source-view-mode 'mode-class 'special)

(defun delib-flow--surface-mode-p ()
  "Return non-nil when point is in a delib-flow generated surface buffer."
  (or (derived-mode-p 'delib-flow-control-mode)
      (derived-mode-p 'delib-flow-filing-workspace-mode)
      (derived-mode-p 'delib-flow-note-part-editor-mode)
      (derived-mode-p 'delib-flow-field-editor-mode)))

(defun delib-flow--point-in-editable-block-body-p (&optional position)
  "Return non-nil when POSITION is inside an editable block body."
  (get-text-property (or position (point)) 'delib-flow-editable))

(delib-flow--define-function delib-flow--editable-block-insert-command
  nil
  "Insert the current key event into an editable block."
  (interactive)
  (pcase last-command-event
    ((or 13 10) (newline))
    ((pred characterp)
     (insert-char last-command-event 1))
    (_ (self-insert-command 1))))

(defun delib-flow--control-command-or-edit (command)
  "Run COMMAND unless point is in an editable block, then insert the key."
  (if (delib-flow--point-in-editable-block-body-p)
      (delib-flow--editable-block-insert-command)
    (call-interactively command)))

(defmacro delib-flow--define-control-edit-command (name target docstring)
  "Define NAME as an editable-block-aware command for TARGET with DOCSTRING."
  `(defalias ',name
     (lambda ()
       ,docstring
       (interactive)
       (delib-flow--control-command-or-edit #',target))))

(delib-flow--define-control-edit-command delib-flow-control-refresh delib-flow-refresh
  "Refresh or insert `g' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-open-audit-run delib-flow-open-audit-run
  "Open audit run or insert `j' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-open-audit-latest-stage delib-flow-open-audit-latest-stage
  "Open latest audit stage or insert `J' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-debug-open-latest-stage-inspection delib-flow-debug-open-latest-stage-inspection
  "Open latest inspection or insert `D' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-debug-open-comparison delib-flow-debug-open-comparison
  "Open comparison or insert `C' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-debug-open-walkthrough delib-flow-debug-open-walkthrough
  "Open walkthrough or insert `W' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-debug-apply-helper delib-flow-debug-apply-helper
  "Apply helper or insert `H' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-debug-walkthrough-next-step delib-flow-debug-walkthrough-next-step
  "Advance walkthrough or insert `N' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-debug-walkthrough-restart-target delib-flow-debug-walkthrough-restart-target
  "Restart walkthrough or insert `R' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-open-filing-workspace delib-flow-open-filing-workspace
  "Open the focused filing workspace or insert `F' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-return-main-cockpit delib-flow-return-main-cockpit
  "Return to the main cockpit or insert `B' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-next-section delib-flow-next-section
  "Move to the next top-level cockpit section or insert `n' when editing.")
(delib-flow--define-control-edit-command delib-flow-control-previous-section delib-flow-previous-section
  "Move to the previous top-level cockpit section or insert `p' when editing.")
(delib-flow--define-control-edit-command delib-flow-control-next-action delib-flow-next-action
  "Move to the next rendered action or insert TAB when editing.")
(delib-flow--define-control-edit-command delib-flow-control-previous-action delib-flow-previous-action
  "Move to the previous rendered action or insert backtab when editing.")
(delib-flow--define-control-edit-command delib-flow-control-dispatch-action delib-flow-dispatch-action
  "Dispatch action or insert the typed key when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-dispatch-shortcut delib-flow-dispatch-action-shortcut
  "Dispatch the rendered action bound to the typed shortcut key.")
(delib-flow--define-control-edit-command delib-flow-control-approve-current delib-flow-approve-current
  "Approve current review or insert `A' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-peek-selected-draft delib-flow-peek-selected-draft
  "Peek the selected draft or insert `E' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-peek-selected-support delib-flow-peek-selected-support
  "Peek the selected support excerpt or insert `I' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-peek-filing-target delib-flow-peek-filing-target
  "Peek the first filing target or insert `P' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-peek-staged-content delib-flow-peek-staged-content
  "Peek staged filing content or insert `V' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-choose-manual-project delib-flow-choose-manual-project
  "Choose manual project or insert `m' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-jump-active-loop delib-flow-jump-active-loop
  "Jump to the active loop or insert `L' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-jump-latest-preview delib-flow-jump-latest-preview
  "Jump to the latest preview or insert `K' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-jump-stage-history delib-flow-jump-stage-history
  "Jump to stage history or insert `U' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-retry-current delib-flow-retry-current
  "Retry current review or insert `r' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-abort-run delib-flow-abort-run
  "Abort run or insert `q' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-choose-filing-selection delib-flow-choose-filing-selection
  "Choose filing selection or insert `s' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-open-clean-source delib-flow-open-clean-source
  "Open the cleaned source view or insert `O' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-choose-reference-note-template delib-flow-choose-reference-note-template
  "Choose reference-note template or insert `T' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-toggle-focus-mode delib-flow-toggle-focus-mode
  "Toggle narrow-screen focus mode or insert `z' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-context-menu delib-flow-control-menu
  "Open a local context menu or insert `.' when editing a block.")
(delib-flow--define-control-edit-command delib-flow-control-help-command delib-flow-control-help
  "Show help or insert `?' when editing a block.")

(defun delib-flow--section-heading-position (section)
  "Return buffer position of top-level SECTION heading, if present."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (delib-flow--section-heading section) nil t)
      (line-beginning-position))))

(defun delib-flow--heading-position (heading)
  "Return buffer position of HEADING, if present at any Org depth."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^\\*+ %s$" (regexp-quote heading))
           nil t)
      (line-beginning-position))))

(defun delib-flow--subtree-content-bounds (heading)
  "Return the content bounds for HEADING in the current buffer."
  (save-excursion
    (when-let ((position (delib-flow--heading-position heading)))
      (goto-char position)
      (org-back-to-heading t)
      (forward-line 1)
      (let ((start (point))
            (end (save-excursion
                   (org-end-of-subtree t t)
                   (point))))
        (cons start end)))))

(defun delib-flow--current-section-at-point ()
  "Return the current top-level control section at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (let ((heading (org-get-heading t t t t)))
        (when (member heading delib-flow--control-sections)
          heading)))))

(defun delib-flow--current-top-level-section-at-point ()
  "Return the enclosing top-level control section at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (while (> (or (org-current-level) 0) 2)
        (org-up-heading-safe))
      (let ((heading (org-get-heading t t t t)))
        (when (member heading delib-flow--control-sections)
          heading)))))

(defun delib-flow--current-heading-at-point ()
  "Return the exact Org heading at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (org-get-heading t t t t))))

(defun delib-flow--action-text-line-count (action)
  "Return the rendered line count for ACTION."
  (length (split-string (string-trim-right (delib-flow--format-action-line action))
                        "\n")))

(defun delib-flow--action-help-text (action)
  "Return help text for ACTION."
  (let ((label (plist-get action :label))
        (id (plist-get action :id))
        (status (plist-get action :status))
        (reason (plist-get action :reason))
        (command (plist-get action :handler)))
    (format "%s\nid: %s\nstatus: %s\ncommand: %s%s"
            label
            id
            status
            command
            (if reason
                (format "\nreason: %s" reason)
              ""))))

(defun delib-flow--annotate-rendered-action-group (bounds actions)
  "Attach ACTION metadata within BOUNDS in the current buffer."
  (when bounds
    (save-excursion
      (goto-char (car bounds))
      (dolist (action actions)
        (let ((start (line-beginning-position)))
          (forward-line (delib-flow--action-text-line-count action))
          (add-text-properties
           start (point)
           `(delib-flow-action ,action
                               mouse-face highlight
                               help-echo ,(delib-flow--action-help-text action)
                               follow-link t)))))))

(defun delib-flow--annotate-rendered-compact-action-group (bounds actions)
  "Attach ACTION metadata for compact one-line ACTIONS within BOUNDS."
  (when bounds
    (save-excursion
      (goto-char (car bounds))
      (dolist (action actions)
        (let ((start (line-beginning-position)))
          (forward-line 1)
          (add-text-properties
           start (point)
           `(delib-flow-action ,action
                               mouse-face highlight
                               help-echo ,(delib-flow--action-help-text action)
                               follow-link t)))))))

(defun delib-flow--annotate-action-lines (run)
  "Attach action metadata for RUN to rendered lines in the current buffer."
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Recommended next pass")
   (if-let ((action (delib-flow--recommended-action run)))
       (list action)
     nil))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Quick actions")
   (delib-flow--quick-actions run))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--section-content-bounds "Next actions")
   (delib-flow--sorted-actions run))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Recommended next filing pass")
   (if-let ((action (delib-flow--recommended-filing-action run)))
       (list action)
     nil))
  (dolist (family '(project-proposals actions waiting-fors reference-notes))
    (delib-flow--annotate-rendered-action-group
     (delib-flow--subtree-content-bounds
      (delib-flow--family-local-action-palette-heading family))
     (delib-flow--family-local-actions run family)))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Filing actions")
   (delib-flow--filing-preview-actions run)))

(defun delib-flow--annotate-focused-filing-workspace-action-lines (run)
  "Attach action metadata for RUN to the focused filing workspace."
  (dolist (heading-actions
           `(("Do here: note title"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(edit-selected-reference-note-title)))
             ("Do here: draft body"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(edit-selected-reference-note-draft-body
                        refresh-selected-reference-note-draft-body)))
             ("Do here: source highlights"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(edit-selected-reference-note-source-highlights
                        refresh-selected-reference-note-source-highlights)))
             ("Do here: related material"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(find-support-for-selected-reference-note
                        choose-support-for-selected-reference-note
                        clear-selected-reference-note-support
                        edit-selected-reference-note-related-material
                        refresh-selected-reference-note-related-material)))
             ("Do here: reuse angle"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(edit-selected-reference-note-reuse-angle
                        refresh-selected-reference-note-reuse-angle)))
             ("Do here: whole note rebuild"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(draft-selected-reference-note)))
             ("Do here: revision compare"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(choose-saved-selected-reference-note-draft
                        restore-previous-selected-reference-note-draft)))
             ("Do here: attached support"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(clear-selected-reference-note-support)))
             ("Do here: support suggestions"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(find-support-for-selected-reference-note
                        choose-support-for-selected-reference-note)))
             ("Do here: target controls"
              . ,(delib-flow--reference-note-workspace-actions
                  run '(choose-reference-note-template
                        edit-selected-reference-note-target-path)))))
    (delib-flow--annotate-rendered-compact-action-group
     (delib-flow--subtree-content-bounds (car heading-actions))
     (cdr heading-actions)))
  (delib-flow--annotate-rendered-compact-action-group
   (delib-flow--subtree-content-bounds "Do here: save or discard")
   (delib-flow--reference-note-workspace-actions
    run
    '(select-approved-filing-actions
      file-approved-outputs
      reject-draft-filing-artifact
      resolve-filing-conflict))))

(defun delib-flow--action-at-point ()
  "Return the rendered action object at point, if any."
  (or (get-text-property (point) 'delib-flow-action)
      (get-text-property (line-beginning-position) 'delib-flow-action)))

(defun delib-flow--action-region-end (position)
  "Return the end position of the rendered action region at POSITION."
  (or (next-single-property-change
       position
       'delib-flow-action
       nil
       (point-max))
      (point-max)))

(delib-flow--define-function delib-flow--next-action-position
  (&optional position)
  "Return the next rendered action position after POSITION."
  (save-excursion
    (let ((pos (or position (point))))
      (when (get-text-property pos 'delib-flow-action)
        (setq pos (delib-flow--action-region-end pos)))
      (while (and (< pos (point-max))
                  (not (get-text-property pos 'delib-flow-action)))
        (setq pos (or (next-single-property-change
                       pos 'delib-flow-action nil (point-max))
                      (point-max))))
      (when (< pos (point-max))
        pos))))

(delib-flow--define-function delib-flow--previous-action-position
  (&optional position)
  "Return the previous rendered action position before POSITION."
  (save-excursion
    (let ((pos (max (point-min)
                    (1- (or position (point))))))
      (when (and (> pos (point-min))
                 (get-text-property pos 'delib-flow-action))
        (while (and (> pos (point-min))
                    (get-text-property (1- pos) 'delib-flow-action))
          (setq pos (1- pos)))
        (setq pos (1- pos)))
      (while (and (>= pos (point-min))
                  (not (get-text-property pos 'delib-flow-action)))
        (setq pos (or (previous-single-property-change
                       pos 'delib-flow-action nil (point-min))
                      (1- (point-min)))))
      (when (>= pos (point-min))
        (while (and (> pos (point-min))
                    (get-text-property (1- pos) 'delib-flow-action))
          (setq pos (1- pos)))
        pos))))

(defun delib-flow--dispatch-rendered-action (action)
  "Execute rendered ACTION when it is available."
  (let ((status (plist-get action :status))
        (reason (plist-get action :reason))
        (command (plist-get action :handler)))
    (if (eq status 'available)
        (call-interactively command)
      (user-error "%s"
                  (or reason
                      (format "%s is not available"
                              (plist-get action :label)))))))

(defun delib-flow--preferred-anchor-section (run)
  "Return preferred anchor heading for RUN."
  (delib-flow--active-loop-heading run))

(delib-flow--define-function delib-flow--changed-heading-for-run
  (run)
  "Return the most relevant changed heading for RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (pcase (plist-get entry :stage-id)
        ((or 'inspect-source 'match-project 'run-cloud-stage)
         "Loop update")
        ('manual-project-match
         "Manual project selection")
        ((or 'extract-actions
             'extract-waiting-for
             'suggest-reference-notes
             'draft-selected-reference-note-body
             'draft-selected-reference-note-source-highlights
             'draft-selected-reference-note-related-material
             'draft-selected-reference-note-reuse-angle
             'propose-new-project
             'find-support-for-selected-action
             'find-support-for-selected-waiting-for
             'find-support-for-selected-reference-note
             'find-support-for-selected-project
             'integrate-into-source
             'select-approved-filing-actions
             'reject-draft-filing-artifact
             'file-approved-outputs
             'resolve-filing-conflict)
         "Filing update")
        ((or 'discover-reference-material 'filter-reference-material)
         "Current context")
        (_
         (delib-flow--active-loop-heading run)))
    "Decision strip"))

(defun delib-flow--highlight-changed-heading (run)
  "Highlight the most relevant changed heading for RUN in the current buffer."
  (when (overlayp delib-flow--changed-heading-overlay)
    (delete-overlay delib-flow--changed-heading-overlay)
    (setq-local delib-flow--changed-heading-overlay nil))
  (when-let ((position (delib-flow--heading-position
                        (delib-flow--changed-heading-for-run run))))
    (save-excursion
      (goto-char position)
      (let ((overlay (make-overlay
                      (line-beginning-position)
                      (line-end-position))))
        (overlay-put overlay 'face 'highlight)
        (overlay-put overlay 'priority 1001)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'delib-flow-changed-heading t)
        (setq-local delib-flow--changed-heading-overlay overlay)))))

(delib-flow--define-function delib-flow--apply-visibility-policy
  (run)
  "Apply control-buffer visibility policy for RUN in the current buffer."
  (save-excursion
    (org-overview)
    (dolist (section
             (if delib-flow-control-focus-mode
                 '("Now")
               '("Now" "Current result" "Filing preview" "Next actions"
                 "Current context")))
      (when-let ((position (delib-flow--section-heading-position section)))
        (goto-char position)
        (org-show-subtree)))
    (when delib-flow-control-focus-mode
      (when-let ((position
                  (delib-flow--section-heading-position
                   (cond
                    ((delib-flow--run-in-flight-p run) "Current result")
                    ((or (delib-flow--inspect-review-pending-p run)
                         (delib-flow--match-review-pending-p run))
                     "Current result")
                    ((delib-flow--filing-preview-visible-p run)
                     "Filing preview")
                    (t "Next actions")))))
        (goto-char position)
        (org-show-subtree)))
    (when (delib-flow--filing-preview-visible-p run)
      (dolist (heading
               '("What to do next"
                 "Current filing plan"
                 "Selected action workspace"
                 "Selected waiting-for workspace"
                 "Selected note workspace"
                 "Selected project workspace"
                 "Filing actions"
                 "Staged content preview"))
        (when-let ((position (delib-flow--heading-position heading)))
          (goto-char position)
          (org-show-subtree))))
    (when (delib-flow--filing-selection-active-p run)
      (when-let ((position (delib-flow--heading-position "Artifact selection")))
        (goto-char position)
        (org-show-subtree)))
    (when (delib-flow--filing-conflict-resolution-active-p run)
      (when-let ((position (delib-flow--heading-position "Conflict resolution")))
        (goto-char position)
        (org-show-subtree)))))

(defun delib-flow--apply-focused-filing-workspace-visibility-policy (run)
  "Apply visibility policy for the focused filing workspace for RUN."
  (save-excursion
    (org-overview)
    (dolist (section (delib-flow--filing-workspace-sections-for-run run))
      (when-let ((position (delib-flow--heading-position section)))
        (goto-char position)
        (org-show-subtree)))))

(defun delib-flow--focused-filing-workspace-should-open-p (run)
  "Return non-nil when the focused filing workspace should be shown for RUN."
  (and (delib-flow--focused-filing-workspace-p run)
       (or (delib-flow--filing-workspace-open-p run)
           (not (delib-flow--filing-workspace-suppressed-p run)))))

(defun delib-flow--render-open-filing-workspace-buffer (run)
  "Render the focused filing workspace buffer for RUN when needed."
  (when (delib-flow--focused-filing-workspace-should-open-p run)
    (delib-flow--render-focused-filing-workspace-buffer run)))

(defun delib-flow--goto-section (heading)
  "Move point to HEADING when present."
  (when-let ((position (or (delib-flow--section-heading-position heading)
                           (delib-flow--heading-position heading))))
    (goto-char position)
    t))

(defun delib-flow--align-heading-top ()
  "Place the current heading at the top of any visible window for this buffer."
  (let ((start (save-excursion
                 (org-back-to-heading t)
                 (line-beginning-position))))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window (point))
      (set-window-start window start))))

(defun delib-flow--staged-content-preview-text-available-p (run)
  "Return non-nil when RUN has a meaningful staged-content preview."
  (let ((text (delib-flow--staged-content-preview-text run)))
    (and text
         (not (string-prefix-p "No staged content preview is available yet." text))
         (not (string-prefix-p "Staged content preview is unavailable:" text)))))

(delib-flow--define-function
 delib-flow--finalize-rendered-active-run-buffer
 (run buffer anchor-section)
 "Apply final visibility, anchoring, and conflict recovery for RUN in BUFFER."
 (with-current-buffer buffer
   (delib-flow--apply-visibility-policy run)
   (unless (delib-flow--goto-section anchor-section)
     (goto-char (point-min)))
   (delib-flow--align-heading-top)
   (delib-flow--highlight-changed-heading run))
 (when (and delib-flow--active-run (eq run delib-flow--active-run))
   (let ((recorded
          (plist-get (delib-flow--run-ui run) :managed-region-conflicts)))
     (when recorded
       (setq delib-flow--active-run
             (delib-flow--seed-actions
              (delib-flow--set-managed-region-conflicts
               delib-flow--active-run nil)))
       (setq run delib-flow--active-run)
       (setq buffer (delib-flow--render-control-buffer run))
       (with-current-buffer buffer
         (delib-flow--apply-visibility-policy run)
         (unless (delib-flow--goto-section anchor-section)
           (goto-char (point-min)))
         (delib-flow--align-heading-top)
         (delib-flow--highlight-changed-heading run)))))
 (delib-flow--refresh-staged-content-preview-buffer run)
 buffer)

(defun delib-flow--finalize-rendered-filing-workspace-buffer (run buffer anchor-section)
  "Apply final visibility and anchoring for RUN in focused filing BUFFER."
  (with-current-buffer buffer
    (delib-flow--apply-focused-filing-workspace-visibility-policy run)
    (unless (delib-flow--goto-section anchor-section)
      (goto-char (point-min)))
    (delib-flow--align-heading-top))
  buffer)

(defun delib-flow--render-active-run-buffer (run &optional anchor-section)
  "Return the control buffer freshly rendered from RUN.

When ANCHOR-SECTION is non-nil, move point to that top-level section."
  (let ((buffer (delib-flow--render-control-buffer run)))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
    (delib-flow--finalize-rendered-active-run-buffer run buffer anchor-section)))

(defun delib-flow--render-filing-workspace-buffer (run &optional anchor-section)
  "Return the focused filing workspace freshly rendered from RUN."
  (let ((buffer (delib-flow--render-focused-filing-workspace-buffer run)))
    (delib-flow--finalize-rendered-filing-workspace-buffer
     run
     buffer
     (or anchor-section
         (delib-flow--filing-workspace-anchor run)
         (delib-flow--default-filing-workspace-anchor run)))))

(defun delib-flow--render-clean-source-buffer (run)
  "Return cleaned source review buffer for RUN."
  (let ((buffer (get-buffer-create delib-flow-source-view-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (delib-flow--clean-source-view-text run))
        (goto-char (point-min))
        (delib-flow-source-view-mode)
        (setq-local delib-flow--active-run run)))
    buffer))

(defun delib-flow--set-sticky-anchor (heading)
  "Persist HEADING as the preferred rerender anchor."
  (setq delib-flow--sticky-anchor-heading heading))

(defun delib-flow--clear-sticky-anchor ()
  "Clear any persisted rerender anchor."
  (setq delib-flow--sticky-anchor-heading nil))

(defun delib-flow--refresh-buffer-sections (run buffer)
  "Refresh BUFFER section-by-section from RUN."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (dolist (section delib-flow--control-sections)
        (delib-flow--replace-section-content
         section
         (delib-flow--section-content section run)))
      (delib-flow--protect-managed-regions)
      (goto-char (point-min))))
  buffer)

(defun delib-flow--teardown-active-run ()
  "Clear active run state."
  (when delib-flow--active-run
    (delib-flow--cancel-in-flight-stage delib-flow--active-run)
    (delib-flow--cleanup-debug-fixture
     (plist-get (delib-flow--run-session delib-flow--active-run)
                :debug-fixture)))
  (when (timerp delib-flow--in-flight-ui-timer)
    (cancel-timer delib-flow--in-flight-ui-timer)
    (setq delib-flow--in-flight-ui-timer nil))
  (when (buffer-live-p (delib-flow--filing-workspace-buffer))
    (kill-buffer (delib-flow--filing-workspace-buffer)))
  (delib-flow--clear-sticky-anchor)
  (setq delib-flow--active-run nil))

(defun delib-flow--mark-run-aborted (run)
  "Return RUN with aborted session state."
  (plist-put run :session
             (plist-put
              (plist-put
               (plist-put
                (plist-put (delib-flow--run-session run) :status 'aborted)
                :active nil)
               :aborted t)
              :ended-at (current-time))))

(defun delib-flow--sync-run-from-control-buffer (run)
  "Return RUN after syncing editable blocks from the live control buffer."
  (let ((buffer (delib-flow--control-buffer)))
    (if (buffer-live-p buffer)
        (delib-flow--sync-editable-blocks run buffer)
      run)))

(defun delib-flow--control-buffer-killed ()
  "Handle control buffer teardown."
  (when (bound-and-true-p delib-flow--active-run-buffer)
    (delib-flow--teardown-active-run)))

(defun delib-flow--focused-filing-workspace-buffer-killed ()
  "Handle focused filing workspace teardown."
  (when delib-flow--active-run
    (setq delib-flow--active-run
          (delib-flow--set-filing-workspace-open
           delib-flow--active-run
           nil))))

(delib-flow--define-function delib-flow--rerender-active-run-buffer
  (&optional forced-anchor)
  "Rerender the live control buffer from `delib-flow--active-run'.

When FORCED-ANCHOR is non-nil, anchor to that heading instead of preserving
the current local heading."
  (unless (delib-flow--focused-filing-workspace-p delib-flow--active-run)
    (setq delib-flow--active-run
          (delib-flow--set-filing-workspace-suppressed
           (delib-flow--set-filing-workspace-open
            (delib-flow--set-filing-workspace-anchor
             delib-flow--active-run nil)
            nil)
           nil))
    (when (buffer-live-p (delib-flow--filing-workspace-buffer))
      (kill-buffer (delib-flow--filing-workspace-buffer))))
  (let* ((buffer (delib-flow--control-buffer))
         (preserved-heading
          (when (and (null forced-anchor)
                     (buffer-live-p buffer))
            (with-current-buffer buffer
              (save-excursion
                (delib-flow--current-heading-at-point)))))
         (anchor
          (if forced-anchor
              forced-anchor
            (or delib-flow--sticky-anchor-heading
                (if (delib-flow--run-in-flight-p delib-flow--active-run)
                    (delib-flow--preferred-anchor-section delib-flow--active-run)
                  (or preserved-heading
                      (delib-flow--preferred-anchor-section
                       delib-flow--active-run)))))))
    (let ((buffer
           (delib-flow--render-active-run-buffer delib-flow--active-run anchor)))
      (when (delib-flow--focused-filing-workspace-should-open-p
             delib-flow--active-run)
        (let* ((workspace-buffer (delib-flow--filing-workspace-buffer))
               (workspace-anchor
                (when (buffer-live-p workspace-buffer)
                  (with-current-buffer workspace-buffer
                    (save-excursion
                      (delib-flow--current-heading-at-point))))))
          (delib-flow--render-filing-workspace-buffer
           delib-flow--active-run
           workspace-anchor)
          (unless (buffer-live-p workspace-buffer)
            (pop-to-buffer (delib-flow--filing-workspace-buffer)))))
      buffer)))

(defun delib-flow--rerender-current-result ()
  "Rerender the active control buffer anchored to `Current result'."
  (delib-flow--set-sticky-anchor "Current result")
  (delib-flow--rerender-active-run-buffer "Current result"))

(defun delib-flow-refresh ()
  "Public command to refresh the active control buffer."
  (interactive)
  (delib-flow-refresh-buffer))

(defun delib-flow-dispatch-action ()
  "Execute the rendered action at point in the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--surface-mode-p)
    (user-error "This command only works in a delib-flow surface buffer"))
  (if-let ((action (delib-flow--action-at-point)))
      (delib-flow--dispatch-rendered-action action)
    (user-error "No delib-flow action is available at point")))

(defun delib-flow-refresh-buffer ()
  "Refresh the control buffer for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((buffer (delib-flow--control-buffer))
         (preserved-heading (delib-flow--refresh-buffer-heading buffer))
         (synced-run (delib-flow--refresh-buffer-run buffer))
         (rendered (delib-flow--render-active-run-buffer
                    synced-run
                    (or preserved-heading
                        (delib-flow--preferred-anchor-section synced-run)))))
    (setq delib-flow--active-run synced-run)
    (pop-to-buffer
     (delib-flow--refresh-buffer-destination rendered))))

(defun delib-flow--refresh-buffer-heading (buffer)
  "Return preserved heading from BUFFER for refresh anchoring."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (delib-flow--current-heading-at-point))))

(defun delib-flow--refresh-buffer-run (buffer)
  "Return refreshed active run state using BUFFER edits."
  (delib-flow--seed-actions
   (delib-flow--set-managed-region-conflicts
    (delib-flow--sync-editable-blocks delib-flow--active-run buffer)
    (delib-flow--managed-region-conflicts delib-flow--active-run
                                          buffer))))

(defun delib-flow--refresh-buffer-destination (rendered)
  "Return display destination after refresh using RENDERED cockpit buffer."
  (if (and (eq delib-flow--surface-kind 'filing-workspace)
           (buffer-live-p (delib-flow--filing-workspace-buffer)))
      (delib-flow--filing-workspace-buffer)
    rendered))

(defun delib-flow--menu-entry (label summary command)
  "Return a context-menu entry with LABEL, SUMMARY, and COMMAND."
  (list :label label :summary summary :command command))

(defun delib-flow--context-menu-action-entry (action)
  "Return a local-menu entry for ACTION."
  (delib-flow--menu-entry
   (plist-get action :label)
   (or (plist-get action :reason)
       "Run this workflow action.")
   (plist-get action :handler)))

(defun delib-flow--context-menu-builtins (entries)
  "Return ENTRIES plus standard local control actions."
  (seq-uniq
   (append
    entries
    (list
     (delib-flow--menu-entry
      "Jump to Active Loop"
      "Move point to the most relevant current decision loop."
      #'delib-flow-jump-active-loop)
     (delib-flow--menu-entry
      "Jump to Latest Preview"
      "Move point to the latest consequence preview or current result."
      #'delib-flow-jump-latest-preview)
     (delib-flow--menu-entry
      "Jump to Stage History"
      "Move point to the latest stage history details."
      #'delib-flow-jump-stage-history)
     (delib-flow--menu-entry
      (if delib-flow-control-focus-mode
          "Disable Focus Mode"
        "Enable Focus Mode")
      "Keep only the active decision loop visible on a narrow screen."
      #'delib-flow-toggle-focus-mode)
     (delib-flow--menu-entry
      "Refresh Buffer"
      "Rerender the cockpit from the current run state."
      #'delib-flow-refresh-buffer)
     (delib-flow--menu-entry
      "Control Hints"
      "Show global keys, local actions, recommendation, and blocked reasons."
      #'delib-flow-control-help)))
   (lambda (left right)
     (and (equal (plist-get left :label)
                 (plist-get right :label))
          (eq (plist-get left :command)
              (plist-get right :command))))))

(defun delib-flow--filing-workspace-menu-builtins (entries)
  "Return ENTRIES plus filing-workspace-local control actions."
  (seq-uniq
   (append
    entries
    (list
     (delib-flow--menu-entry
      "Return to Main Cockpit"
      "Go back to the overview cockpit without losing filing state."
      #'delib-flow-return-main-cockpit)
     (delib-flow--menu-entry
      "Refresh Workspace"
      "Rerender the filing workspace from the current run state."
      #'delib-flow-refresh-buffer)
     (delib-flow--menu-entry
      "Control Hints"
      "Show filing-workspace keys, local actions, and current blocked reasons."
      #'delib-flow-control-help)))
   (lambda (left right)
     (and (equal (plist-get left :label)
                 (plist-get right :label))
          (eq (plist-get left :command)
              (plist-get right :command))))))

(defun delib-flow--available-action-menu-entries (actions)
  "Return context-menu entries for ACTIONS."
  (mapcar #'delib-flow--context-menu-action-entry actions))

(defun delib-flow--sort-actions-by-id-order (actions preferred)
  "Return ACTIONS sorted by id order in PREFERRED."
  (sort actions
        (lambda (left right)
          (< (or (seq-position preferred (plist-get left :id)) 999)
             (or (seq-position preferred (plist-get right :id)) 999)))))

(defun delib-flow--consequence-preview-menu-entries (run)
  "Return filing-preview consequence menu entries for RUN."
  (append
   (when (delib-flow--consequence-preview-available-p run 'draft)
     (list
      (delib-flow--menu-entry
       "Peek Selected Draft"
       "Open the selected draft in the sibling consequence pane."
       #'delib-flow-peek-selected-draft)))
   (when (delib-flow--consequence-preview-available-p run 'support)
     (list
      (delib-flow--menu-entry
       "Peek Selected Support"
       "Open focused support for the selected artifact in the sibling consequence pane."
       #'delib-flow-peek-selected-support)))
   (list
    (delib-flow--menu-entry
     "Choose Filing Artifact"
     "Pick one valid filing artifact with minibuffer completion."
     #'delib-flow-choose-filing-selection)
    (delib-flow--menu-entry
     "Peek Filing Target"
     "Open the planned filing target in the sibling consequence pane."
     #'delib-flow-peek-filing-target)
    (delib-flow--menu-entry
     "Peek Staged Content"
     "Open the exact staged filing content in the sibling consequence pane."
     #'delib-flow-peek-staged-content))))

(defun delib-flow--filing-preview-general-menu-entries (run)
  "Return general filing-preview context-menu entries for RUN."
  (append
   (delib-flow--available-action-menu-entries
    (delib-flow--filing-preview-actions run))
   (when (delib-flow--focused-filing-workspace-p run)
     (list
      (delib-flow--menu-entry
       "Open Filing Workspace"
       "Move into the focused filing workspace for the active note loop."
       #'delib-flow-open-filing-workspace)))
   (delib-flow--consequence-preview-menu-entries run)
   (if (delib-flow--reference-note-capture-visible-p run)
       (list
        (delib-flow--menu-entry
         "Choose Reference Note Template"
         "Pick the org-roam template for the current note artifact."
         #'delib-flow-choose-reference-note-template))
     nil)))

(delib-flow--define-function delib-flow--selected-family-workspace-at-point
  nil
  "Return the selected artifact family workspace at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (let ((family nil)
            (continue t))
        (while (and continue (org-current-level))
          (setq family
                (pcase (org-get-heading t t t t)
                  ("Selected action workspace" 'actions)
                  ("Selected waiting-for workspace" 'waiting-fors)
                  ("Selected note workspace" 'reference-notes)
                  ("Selected project workspace" 'project-proposals)
                  (_ family)))
          (setq continue (and (not family) (org-up-heading-safe))))
        family))))

(defun delib-flow--focused-filing-workspace-section-at-point ()
  "Return the current focused filing workspace heading at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (let ((heading (org-get-heading t t t t)))
        (when (member heading
                      (delib-flow--filing-workspace-sections-for-run
                       delib-flow--active-run))
          heading)))))

(defun delib-flow--filing-workspace-sections-for-run (run)
  "Return the top-level filing-workspace headings appropriate for RUN."
  (if (delib-flow--project-focused-filing-workspace-available-p run)
      delib-flow--project-filing-workspace-sections
    delib-flow--filing-workspace-sections))

(defun delib-flow--default-filing-workspace-anchor (run)
  "Return the default top-level filing-workspace anchor for RUN."
  (if (delib-flow--project-focused-filing-workspace-available-p run)
      "Do here now"
    (car (delib-flow--filing-workspace-sections-for-run run))))

(defun delib-flow--focused-filing-workspace-menu-entries (run)
  "Return local context-menu entries for RUN in the focused filing workspace."
  (delib-flow--filing-workspace-menu-builtins
   (append
    (delib-flow--available-action-menu-entries
     (if (delib-flow--project-focused-filing-workspace-available-p run)
         (delib-flow--focused-project-workspace-actions run)
       (delib-flow--family-local-actions run 'reference-notes)))
    (list
     (delib-flow--menu-entry
      "Open Filing Workspace"
      "Keep the focused filing workspace visible for the active drafting loop."
      #'delib-flow-open-filing-workspace))
    (delib-flow--consequence-preview-menu-entries run))))

(defun delib-flow--context-menu-entries-for-current-point (run)
  "Return local context-menu entries for RUN based on point location."
  (if (eq delib-flow--surface-kind 'filing-workspace)
      (delib-flow--focused-filing-workspace-menu-entries run)
    (let ((section (or (delib-flow--current-top-level-section-at-point)
                       "Now")))
      (if (and (equal section "Filing preview")
               (delib-flow--selected-family-workspace-at-point))
          (delib-flow--context-menu-builtins
           (append
            (delib-flow--available-action-menu-entries
             (delib-flow--family-local-actions
              run
              (delib-flow--selected-family-workspace-at-point)))
            (delib-flow--filing-preview-general-menu-entries run)))
        (delib-flow--context-menu-builtins
         (delib-flow--context-menu-entries run section))))))

(defun delib-flow--context-menu-now-entries (run)
  "Return local context-menu entries for RUN in the Now section."
  (append
   (when-let ((action (delib-flow--recommended-action run)))
     (list (delib-flow--context-menu-action-entry action)))
   (delib-flow--available-action-menu-entries
    (delib-flow--quick-actions run))))

(defun delib-flow--context-menu-current-result-entries (&optional _run)
  "Return local context-menu entries for the Current result section."
  (append
   (when (delib-flow--current-reviewable-stage)
     (list
      (delib-flow--menu-entry
       "Approve Current Review"
       "Accept the pending inspect or project-match decision."
       #'delib-flow-approve-current)
      (delib-flow--menu-entry
       "Retry Current Review"
       "Rerun the current pending inspect or project-match stage."
       #'delib-flow-retry-current)))
   (list
    (delib-flow--menu-entry
     "Open Debug Comparison"
     "Compare the latest stage with a fresh replay or previous attempt."
     #'delib-flow-debug-open-comparison)
    (delib-flow--menu-entry
     "Open Latest Stage Inspection"
     "Inspect package, normalized output, and raw stage output."
     #'delib-flow-debug-open-latest-stage-inspection))))

(defun delib-flow--context-menu-next-actions-entries (run)
  "Return local context-menu entries for RUN in the Next actions section."
  (delib-flow--available-action-menu-entries
   (seq-filter
    (lambda (action)
      (eq (plist-get action :status) 'available))
    (delib-flow--sorted-actions run))))

(defun delib-flow--context-menu-current-context-entries (run)
  "Return local context-menu entries for RUN in the Current context section."
  (delq nil
        (list
         (and (delib-flow--manual-project-selection-active-p run)
              (delib-flow--menu-entry
               "Choose Project Manually"
               "Select a project from the current manual shortlist."
               #'delib-flow-choose-manual-project))
         (delib-flow--menu-entry
          "Save Active Audit Run"
          "Archive the active run as a standalone Org audit file."
          #'delib-flow-save-active-audit-run)
         (delib-flow--menu-entry
          "Open Audit Run"
          "Jump to the active run subtree in the audit log."
          #'delib-flow-open-audit-run))))

(defun delib-flow--context-menu-details-entries ()
  "Return local context-menu entries for the Details section."
  (list
   (delib-flow--menu-entry
    "Save Active Audit Run"
    "Archive the active run as a standalone Org audit file."
    #'delib-flow-save-active-audit-run)
   (delib-flow--menu-entry
    "Open Audit Run"
    "Jump to the active run subtree in the audit log."
    #'delib-flow-open-audit-run)
   (delib-flow--menu-entry
    "Open Latest Audit Stage"
    "Jump to the latest stage subtree in the audit log."
    #'delib-flow-open-audit-latest-stage)
   (delib-flow--menu-entry
    "Open Debug Walkthrough"
    "Show validation recipes and helper checkpoints."
    #'delib-flow-debug-open-walkthrough)))

(defconst delib-flow--context-menu-section-dispatch
  '(("Now" . delib-flow--context-menu-now-entries)
    ("Current result" . delib-flow--context-menu-current-result-entries)
    ("Filing preview" . delib-flow--filing-preview-general-menu-entries)
    ("Next actions" . delib-flow--context-menu-next-actions-entries)
    ("Current context" . delib-flow--context-menu-current-context-entries)
    ("Details" . delib-flow--context-menu-details-entries))
  "Mapping from top-level sections to context-menu entry builders.")

(defun delib-flow--context-menu-entries (run section)
  "Return local context-menu entries for RUN in SECTION."
  (delib-flow--context-menu-builtins
   (if-let ((builder (cdr (assoc section delib-flow--context-menu-section-dispatch))))
       (funcall builder run)
     nil)))

(defun delib-flow--local-action-entries-for-surface (run section)
  "Return local action entries for RUN at SECTION on the current surface."
  (cond
   ((eq delib-flow--surface-kind 'filing-workspace)
    (delib-flow--focused-filing-workspace-menu-entries run))
   ((and (derived-mode-p 'delib-flow-control-mode)
         (equal section "Filing preview"))
    (delib-flow--context-menu-entries-for-current-point run))
   (t
    (delib-flow--context-menu-entries run section))))

(defun delib-flow--context-menu-choice-label (entry)
  "Return the user-facing label string for local-menu ENTRY."
  (format "%s - %s"
          (plist-get entry :label)
          (plist-get entry :summary)))

(defun delib-flow--local-actions-summary-text (run section)
  "Return local action summary text for RUN in SECTION."
  (let ((entries (delib-flow--local-action-entries-for-surface run section)))
    (if entries
        (mapconcat
         (lambda (entry)
           (format "- %s: %s"
                   (plist-get entry :label)
                   (plist-get entry :summary)))
         entries
         "\n")
      "- No local actions are available here.")))

(defun delib-flow-open-filing-workspace ()
  "Open the focused filing workspace for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--focused-filing-workspace-p delib-flow--active-run)
    (user-error "No focused filing workspace is available for the current run"))
  (setq delib-flow--active-run
        (delib-flow--set-filing-workspace-suppressed
         (delib-flow--set-filing-workspace-open delib-flow--active-run t)
         nil))
  (pop-to-buffer
   (delib-flow--render-filing-workspace-buffer delib-flow--active-run)))

(defun delib-flow-open-clean-source ()
  "Open the cleaned source view for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (pop-to-buffer
   (delib-flow--render-clean-source-buffer delib-flow--active-run)))

(defun delib-flow-return-main-cockpit ()
  "Return to the main cockpit from the focused filing workspace."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--set-filing-workspace-suppressed
         (delib-flow--set-filing-workspace-open delib-flow--active-run nil)
         t))
  (when (buffer-live-p (delib-flow--filing-workspace-buffer))
    (kill-buffer (delib-flow--filing-workspace-buffer)))
  (pop-to-buffer
   (delib-flow--render-active-run-buffer
    delib-flow--active-run
    (or (delib-flow--preferred-anchor-section delib-flow--active-run)
        "Filing preview"))))

(defun delib-flow-jump-to-active-filing-workspace ()
  "Jump to the active focused filing workspace when one is available."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (if (buffer-live-p (delib-flow--filing-workspace-buffer))
      (pop-to-buffer (delib-flow--filing-workspace-buffer))
    (delib-flow-open-filing-workspace)))

(defun delib-flow--open-note-part-editor (title text apply-function)
  "Open the focused note-part editor for TITLE with TEXT and APPLY-FUNCTION."
  (let ((buffer (get-buffer-create delib-flow-note-part-editor-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (goto-char (point-min))
        (delib-flow-note-part-editor-mode)
        (setq-local delib-flow--note-part-editor-title title)
        (setq-local delib-flow--note-part-editor-apply-function apply-function)
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)))

(defun delib-flow--open-field-editor (title text apply-function)
  "Open the focused field editor for TITLE with TEXT and APPLY-FUNCTION."
  (let ((buffer (get-buffer-create delib-flow-field-editor-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (goto-char (point-min))
        (delib-flow-field-editor-mode)
        (setq-local delib-flow--field-editor-title title)
        (setq-local delib-flow--field-editor-apply-function apply-function)
        (set-buffer-modified-p nil)))
    (pop-to-buffer buffer)))

(defun delib-flow-note-part-editor-apply ()
  "Apply the current note-part editor contents."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (derived-mode-p 'delib-flow-note-part-editor-mode)
    (user-error "This command only works in the note-part editor"))
  (unless (functionp delib-flow--note-part-editor-apply-function)
    (user-error "No note-part apply function is active"))
  (let ((editor (current-buffer))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (funcall delib-flow--note-part-editor-apply-function
                    delib-flow--active-run
                    text)))
    (when (buffer-live-p editor)
      (kill-buffer editor))
    (delib-flow-open-filing-workspace)))

(defun delib-flow--note-part-editor-discard-confirmed-p ()
  "Return non-nil when the current note-part edit may be discarded."
  (or (not (buffer-modified-p))
      (yes-or-no-p "Discard note-part edits? ")))

(defun delib-flow--close-note-part-editor-buffer (buffer)
  "Close note-part editor BUFFER and return to the filing workspace."
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (when delib-flow--active-run
    (delib-flow-open-filing-workspace)))

(defun delib-flow-note-part-editor-cancel ()
  "Cancel the current note-part edit and return to the filing workspace."
  (interactive)
  (unless (derived-mode-p 'delib-flow-note-part-editor-mode)
    (user-error "This command only works in the note-part editor"))
  (unless (delib-flow--note-part-editor-discard-confirmed-p)
    (user-error "Canceled"))
  (delib-flow--close-note-part-editor-buffer (current-buffer)))

(defun delib-flow--field-editor-discard-confirmed-p ()
  "Return non-nil when the current field edit may be discarded."
  (or (not (buffer-modified-p))
      (yes-or-no-p "Discard field edits? ")))

(defun delib-flow--close-field-editor-buffer (buffer)
  "Close field editor BUFFER and rerender the active run."
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (when delib-flow--active-run
    (delib-flow--rerender-active-run-buffer)))

(defun delib-flow-field-editor-apply ()
  "Apply the current field editor contents."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (derived-mode-p 'delib-flow-field-editor-mode)
    (user-error "This command only works in the field editor"))
  (unless (functionp delib-flow--field-editor-apply-function)
    (user-error "No field apply function is active"))
  (let ((editor (current-buffer))
        (text (buffer-substring-no-properties (point-min) (point-max))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (funcall delib-flow--field-editor-apply-function
                    delib-flow--active-run
                    text)))
    (delib-flow--close-field-editor-buffer editor)))

(defun delib-flow-field-editor-cancel ()
  "Cancel the current field edit and return to the active surface."
  (interactive)
  (unless (derived-mode-p 'delib-flow-field-editor-mode)
    (user-error "This command only works in the field editor"))
  (unless (delib-flow--field-editor-discard-confirmed-p)
    (user-error "Canceled"))
  (delib-flow--close-field-editor-buffer (current-buffer)))

(defun delib-flow--set-editable-block-text-directly (run block-id text)
  "Return RUN with editable BLOCK-ID replaced by TEXT."
  (let ((block (delib-flow--editable-block run block-id)))
    (delib-flow--set-editable-block
     run block-id (delib-flow--set-editable-block-text block text))))

(defun delib-flow--set-operator-intent-directly (run text)
  "Return RUN with operator intent updated to TEXT and rerun guidance surfaced."
  (let* ((updated-run
          (delib-flow--set-editable-block-text-directly run 'context-main text))
         (session (delib-flow--run-session updated-run))
         (decision
          (format "Operator intent saved. %s to apply it to future stage results."
                  (delib-flow--operator-intent-next-step-label updated-run))))
    (delib-flow--refresh-run-audit
     (plist-put
      updated-run
      :session
      (plist-put
       (plist-put session :operator-intent text)
       :current-decision decision))
     'operator-intent-edit)))

(defun delib-flow--open-editable-block-editor (run block-id title)
  "Open a dedicated editor for RUN editable BLOCK-ID using TITLE."
  (delib-flow--open-field-editor
   title
   (delib-flow--editable-block-text
    (delib-flow--editable-block run block-id))
   (lambda (updated-run text)
     (delib-flow--set-editable-block-text-directly updated-run block-id text))))

(defun delib-flow-action-edit-operator-intent ()
  "Open a dedicated editor for operator intent."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
  (delib-flow--open-field-editor
   "Operator intent"
   (delib-flow--operator-intent-text delib-flow--active-run)
   #'delib-flow--set-operator-intent-directly))

(defun delib-flow-action-edit-operator-notes ()
  "Open a dedicated editor for operator notes."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
  (delib-flow--open-editable-block-editor
   delib-flow--active-run 'operator-notes "Operator notes"))

(defun delib-flow-action-edit-manual-project-selection ()
  "Open a dedicated editor for manual project selection notes."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
  (delib-flow--open-editable-block-editor
   delib-flow--active-run 'manual-project-selection "Manual project selection"))

(defun delib-flow-action-edit-filing-selection-review ()
  "Open a dedicated editor for filing selection review."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
  (delib-flow--open-editable-block-editor
   delib-flow--active-run 'filing-selection-review "Filing selection review"))

(defun delib-flow-action-edit-filing-conflict-resolution ()
  "Open a dedicated editor for filing conflict resolution."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
  (delib-flow--open-editable-block-editor
   delib-flow--active-run 'filing-conflict-resolution "Filing conflict resolution"))

(defun delib-flow-action-edit-selected-reference-note-title ()
  "Edit the saved note title for the selected note."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (item (delib-flow--reference-note-workspace-item synced-run)))
    (unless (eq (plist-get item :kind) 'reference-note)
      (user-error "No selected note is active"))
    (setq delib-flow--active-run
          (delib-flow--set-reference-note-capture-field
           synced-run
           "Note title"
           (read-string "Saved note title: "
                        (or (delib-flow--reference-note-effective-title item synced-run)
                            ""))))
    (delib-flow--rerender-active-run-buffer)))

(defun delib-flow-action-edit-selected-reference-note-target-path ()
  "Edit the saved target path override for the selected note."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (item (delib-flow--reference-note-workspace-item synced-run)))
    (unless (eq (plist-get item :kind) 'reference-note)
      (user-error "No selected note is active"))
    (setq delib-flow--active-run
          (delib-flow--set-reference-note-capture-field
           synced-run
           "Target path"
           (read-string
            "Saved note target path (relative to org-roam root, without .org): "
            (or (delib-flow--reference-note-effective-target-override synced-run)
                ""))))
    (delib-flow--rerender-active-run-buffer)))

(defun delib-flow-action-edit-selected-reference-note-draft-body ()
  "Open a dedicated editor for the selected note working-draft body."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((run (delib-flow--sync-run-from-control-buffer delib-flow--active-run)))
    (setq delib-flow--active-run run)
    (delib-flow--open-note-part-editor
     "Draft body"
     (delib-flow--selected-reference-note-draft-working-body-text run)
     #'delib-flow--reference-note-edit-draft-body)))

(defun delib-flow--run-selected-reference-note-part-stage (stage-id)
  "Run selected reference-note part STAGE-ID and rerender the result."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run stage-id)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-refresh-selected-reference-note-draft-body ()
  "Regenerate only the working-draft body of the selected note draft."
  (interactive)
  (delib-flow--run-selected-reference-note-part-stage
   'draft-selected-reference-note-body))

(defun delib-flow-action-edit-selected-reference-note-source-highlights ()
  "Open a dedicated editor for the selected note source highlights."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((run (delib-flow--sync-run-from-control-buffer delib-flow--active-run)))
    (setq delib-flow--active-run run)
    (delib-flow--open-note-part-editor
     "Source highlights"
     (delib-flow--selected-reference-note-draft-source-highlights-text run)
     #'delib-flow--reference-note-edit-source-highlights)))

(defun delib-flow-action-refresh-selected-reference-note-source-highlights ()
  "Regenerate only the source highlights section of the selected note draft."
  (interactive)
  (delib-flow--run-selected-reference-note-part-stage
   'draft-selected-reference-note-source-highlights))

(defun delib-flow-action-edit-selected-reference-note-related-material ()
  "Open a dedicated editor for the selected note related material."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((run (delib-flow--sync-run-from-control-buffer delib-flow--active-run)))
    (setq delib-flow--active-run run)
    (delib-flow--open-note-part-editor
     "Related material"
     (delib-flow--selected-reference-note-draft-related-material-text run)
     #'delib-flow--reference-note-edit-related-material)))

(defun delib-flow-action-refresh-selected-reference-note-related-material ()
  "Regenerate only the related material section of the selected note draft."
  (interactive)
  (delib-flow--run-selected-reference-note-part-stage
   'draft-selected-reference-note-related-material))

(defun delib-flow-action-edit-selected-reference-note-reuse-angle ()
  "Open a dedicated editor for the selected note reuse angle."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((run (delib-flow--sync-run-from-control-buffer delib-flow--active-run)))
    (setq delib-flow--active-run run)
    (delib-flow--open-note-part-editor
     "Reuse angle"
     (string-remove-prefix
      "- Reuse angle: "
      (delib-flow--selected-reference-note-draft-reuse-angle-text run))
     #'delib-flow--reference-note-edit-reuse-angle)))

(defun delib-flow-action-refresh-selected-reference-note-reuse-angle ()
  "Regenerate only the reuse angle line of the selected note draft."
  (interactive)
  (delib-flow--run-selected-reference-note-part-stage
   'draft-selected-reference-note-reuse-angle))

(defun delib-flow--current-surface-section-at-point ()
  "Return the current top-level section heading for the active surface."
  (if (eq delib-flow--surface-kind 'filing-workspace)
      (delib-flow--focused-filing-workspace-section-at-point)
    (delib-flow--current-top-level-section-at-point)))

(defun delib-flow--surface-sections ()
  "Return top-level section headings for the active surface."
  (if (eq delib-flow--surface-kind 'filing-workspace)
      (delib-flow--filing-workspace-sections-for-run delib-flow--active-run)
    delib-flow--control-sections))

(defun delib-flow--persist-surface-anchor (target)
  "Persist TARGET as the preferred rerender anchor for the active surface."
  (if (eq delib-flow--surface-kind 'filing-workspace)
      (setq delib-flow--active-run
            (delib-flow--set-filing-workspace-anchor
             delib-flow--active-run target))
    (delib-flow--clear-sticky-anchor)))

(defun delib-flow-next-section ()
  "Move point to the next top-level cockpit section."
  (interactive)
  (unless (delib-flow--surface-mode-p)
    (user-error "This command only works in a delib-flow surface buffer"))
  (let* ((current (delib-flow--current-surface-section-at-point))
         (sections (delib-flow--surface-sections))
         (remaining (cdr (member current sections)))
         (target (or (car remaining) (car sections))))
    (delib-flow--persist-surface-anchor target)
    (unless (delib-flow--goto-section target)
      (user-error "No control section is available"))
    (delib-flow--align-heading-top)))

(defun delib-flow-previous-section ()
  "Move point to the previous top-level cockpit section."
  (interactive)
  (unless (delib-flow--surface-mode-p)
    (user-error "This command only works in a delib-flow surface buffer"))
  (let* ((current (delib-flow--current-surface-section-at-point))
         (sections (delib-flow--surface-sections))
         (before (seq-take-while (lambda (section)
                                   (not (equal section current)))
                                 sections))
         (target (or (car (last before))
                     (car (last sections)))))
    (delib-flow--persist-surface-anchor target)
    (unless (delib-flow--goto-section target)
      (user-error "No control section is available"))
    (delib-flow--align-heading-top)))

(defun delib-flow-next-action ()
  "Move point to the next rendered action in the cockpit."
  (interactive)
  (unless (delib-flow--surface-mode-p)
    (user-error "This command only works in a delib-flow surface buffer"))
  (if-let ((position (delib-flow--next-action-position)))
      (goto-char position)
    (user-error "No later rendered action is available")))

(defun delib-flow-previous-action ()
  "Move point to the previous rendered action in the cockpit."
  (interactive)
  (unless (delib-flow--surface-mode-p)
    (user-error "This command only works in a delib-flow surface buffer"))
  (if-let ((position (delib-flow--previous-action-position)))
      (goto-char position)
    (user-error "No earlier rendered action is available")))

(delib-flow--define-function delib-flow-control-help nil
  "Show the DeliberateFlow control-buffer keybindings."
  (interactive)
  (let* ((buffer (delib-flow--control-buffer))
         (section
          (cond
           ((and (delib-flow--surface-mode-p)
                 (eq delib-flow--surface-kind 'filing-workspace))
            (or (delib-flow--focused-filing-workspace-section-at-point)
                "Focused filing workspace"))
           ((buffer-live-p buffer)
            (with-current-buffer buffer
              (delib-flow--current-top-level-section-at-point)))))
         (run delib-flow--active-run))
    (with-help-window (help-buffer)
      (princ "DeliberateFlow control hints\n\n")
      (princ
       (if (eq delib-flow--surface-kind 'filing-workspace)
           "Use n/p to move between filing workspace sections.\n"
         "Use n/p to move between cockpit sections.\n"))
      (princ "Use TAB/S-TAB to move between rendered actions.\n")
      (princ "Use . to open the local context menu.\n")
      (princ
       (if (eq delib-flow--surface-kind 'filing-workspace)
           "Use B to return to the cockpit and F to reopen the filing workspace.\n"
         "Use L to jump back to the active loop.\n"))
      (princ
       (if (eq delib-flow--surface-kind 'filing-workspace)
           "Use E/I/P/V to preview draft, support, target, or staged output.\n"
         "Use K to jump to the latest preview or result.\n"))
      (unless (eq delib-flow--surface-kind 'filing-workspace)
        (princ "Use U to jump to the latest stage history details.\n")
        (princ "Use z to toggle narrow-screen focus mode.\n"))
      (princ "Action keys are shown inline beside each rendered option.\n\n")
      (princ (format "Current section: %s\n" (or section "unknown")))
      (princ
       (format "Recommended next pass: %s\n"
               (if run
                   (delib-flow--recommended-action-text run)
                 "none")))
      (princ
       (format "Active loop: %s\n"
               (if run
                   (delib-flow--active-loop-location-text run)
                 "unknown")))
      (princ
       (format "Latest change: %s\n"
               (if run
                   (delib-flow--latest-meaningful-change-text run)
                 "unknown")))
      (princ
       (format "Latest preview: %s\n"
               (if run
                   (delib-flow--latest-preview-location-text run)
                 "unknown")))
      (princ
       (format "Current decision: %s\n"
               (if run
                   (or (plist-get (delib-flow--run-session run) :current-decision)
                       "none")
                 "unknown")))
      (princ
       (format "Last consequence: %s\n"
               (if run
                   (delib-flow--latest-consequence-text run)
                 "unknown")))
      (princ
       (format "Blocked by: %s\n\n"
               (if run
                   (delib-flow--current-blockage-text run)
                 "unknown")))
      (princ "Local actions here:\n")
      (princ
       (if run
           (delib-flow--local-actions-summary-text run section)
         "- No active run is available."))
      (princ "\n\nGlobal controls:\n")
      (dolist (entry
               (if (eq delib-flow--surface-kind 'filing-workspace)
                   delib-flow--filing-workspace-key-help-alist
                 delib-flow--control-key-help-alist))
        (princ (format "%-12s %s\n" (car entry) (cdr entry)))))))

(delib-flow--define-function delib-flow-control-menu nil
  "Open a local context menu for the current cockpit section."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--surface-mode-p)
    (user-error "This command only works in a delib-flow surface buffer"))
  (let* ((section
          (if (eq delib-flow--surface-kind 'filing-workspace)
              (or (delib-flow--focused-filing-workspace-section-at-point)
                  "Focused filing workspace")
            (or (delib-flow--current-top-level-section-at-point)
                "Now")))
         (entries
          (delib-flow--context-menu-entries-for-current-point
           delib-flow--active-run)))
    (unless entries
      (user-error "No local actions are available in %s" section))
    (let* ((labels (mapcar #'delib-flow--context-menu-choice-label entries))
           (choice
            (completing-read
             (format "Delib-Flow %s menu: " section)
             labels nil t))
           (index (cl-position choice labels :test #'equal))
           (entry (nth index entries)))
      (unless entry
        (user-error "No local action was selected"))
      (call-interactively (plist-get entry :command)))))

(defun delib-flow-toggle-focus-mode ()
  "Toggle narrow-screen focus mode for the active control buffer."
  (interactive)
  (let ((buffer (delib-flow--control-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "This command only works in the delib-flow control buffer"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'delib-flow-control-mode)
        (user-error "This command only works in the delib-flow control buffer"))
      (setq-local delib-flow-control-focus-mode
                  (not delib-flow-control-focus-mode))
      (delib-flow--apply-visibility-policy delib-flow--active-run)
      (force-mode-line-update)
      (message "Delib-Flow focus mode %s"
               (if delib-flow-control-focus-mode "enabled" "disabled")))))

(defun delib-flow-abort-run ()
  "Abort the active delib-flow run and close the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--refresh-run-audit
         (delib-flow--mark-run-aborted delib-flow--active-run)
         'abort-run))
  (let ((buffer (delib-flow--control-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (delib-flow--teardown-active-run))

(defun delib-flow--call-current-review-command (command-fn error-message)
  "Call current review COMMAND-FN or signal ERROR-MESSAGE."
  (if-let* ((stage-id (delib-flow--current-reviewable-stage))
            (command (funcall command-fn stage-id)))
      (call-interactively command)
    (user-error "%s" error-message)))

(defmacro delib-flow--define-interactive-command (name docstring &rest body)
  "Define NAME with DOCSTRING and BODY as an interactive command."
  `(defalias ',name
     (lambda ()
       ,docstring
       (interactive)
       ,@body)))

(delib-flow--define-interactive-command delib-flow-action-inspect-source
  "Execute the inspect-source stage for the active run."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'inspect-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-match-project ()
  "Execute the match-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--stage-accepted-p delib-flow--active-run 'inspect-source)
    (user-error "Inspect result must be accepted before project matching"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'match-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-accept-inspect-source ()
  "Accept the current inspect-source result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--inspect-review-pending-p delib-flow--active-run)
    (user-error "No inspect result is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-inspect-review-outcome
           (delib-flow--apply-inspect-source-review-edit
            (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
           'accepted
           "Inspect result accepted. You may now match the project or retry inspect.")
          'inspect-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-reject-inspect-source ()
  "Reject the current inspect-source result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--inspect-review-pending-p delib-flow--active-run)
    (user-error "No inspect result is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-inspect-review-outcome
           delib-flow--active-run
           'rejected
           "Inspect result rejected. Retry inspect before matching a project.")
          'inspect-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-accept-match-project ()
  "Accept the current match-project result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--match-review-pending-p delib-flow--active-run)
    (user-error "No project match is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-match-review-outcome
           delib-flow--active-run
           'accepted
           (if (memq (delib-flow--match-status delib-flow--active-run) '(ambiguous no-match))
               "Project match accepted for manual review. Edit the manual project selection block, then choose a project manually or continue with no-match follow-up."
             "Project match accepted. Continue with downstream stages as appropriate."))
          'match-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-reject-match-project ()
  "Reject the current match-project result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--match-review-pending-p delib-flow--active-run)
    (user-error "No project match is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-match-review-outcome
           delib-flow--active-run
           'rejected
           "Project match rejected. Retry project matching before downstream project-dependent stages.")
          'match-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-discover-reference-material ()
  "Execute the discover-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (or (delib-flow--project-decision-ready-p delib-flow--active-run)
              (delib-flow--stage-accepted-p delib-flow--active-run 'inspect-source))
    (user-error "Accept Inspect Source before reference discovery"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'discover-reference-material)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-filter-reference-material ()
  "Execute the filter-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--stage-executed-p delib-flow--active-run
                                        'discover-reference-material)
    (user-error "Reference discovery must run before filtering retained material"))
  (unless (plist-get (delib-flow--run-working-context delib-flow--active-run)
                     :retrieved-candidates)
    (user-error "No discovered reference material is available to filter"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'filter-reference-material)))
  (delib-flow--rerender-current-result))

(delib-flow--define-function delib-flow-choose-manual-project nil
  "Choose a valid manual project candidate with completion."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless
      (or
       (delib-flow--stage-executed-p
        delib-flow--active-run
        'manual-project-match)
       (and
        (memq
         (delib-flow--stage-review-state
          delib-flow--active-run
          'match-project)
         '(accepted rejected))
        (memq
         (delib-flow--match-status
          delib-flow--active-run)
         '(matched ambiguous no-match))))
    (user-error
     "Accept or reject the current project result before manual override"))
  (let* ((synced-run
          (delib-flow--sync-run-from-control-buffer
           delib-flow--active-run))
         (labels
          (delib-flow--manual-project-selection-labels
           synced-run))
         (current
          (delib-flow--manual-project-selection-value
           (delib-flow--stage-input-package
            synced-run 'manual-project-match)))
         (choice
          (completing-read "Manual project: "
                           (mapcar #'car
                                   labels)
                           nil t nil nil
                           (car
                            (rassoc current
                                    labels)))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--set-manual-project-selection-value
            synced-run
            (or (cdr (assoc choice labels))
                choice))))
    (delib-flow--clear-sticky-anchor)
    (delib-flow--rerender-active-run-buffer)))

(delib-flow--define-function delib-flow-action-manual-project-match
    nil
  "Execute the manual-project-match stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless
      (or
       (delib-flow--stage-executed-p
        delib-flow--active-run
        'manual-project-match)
       (and
        (memq
         (delib-flow--stage-review-state
          delib-flow--active-run
          'match-project)
         '(accepted rejected))
        (memq
         (delib-flow--match-status
          delib-flow--active-run)
         '(matched ambiguous no-match))))
    (user-error
     "Accept or reject the current project result before manual override"))
  (let* ((synced-run
          (delib-flow--sync-run-from-control-buffer
           delib-flow--active-run))
         (synced-selection
          (delib-flow--manual-project-selection-value
           (delib-flow--stage-input-package
            synced-run 'manual-project-match)))
         (active-selection
          (delib-flow--manual-project-selection-value
           (delib-flow--stage-input-package
            delib-flow--active-run
            'manual-project-match)))
         (selected-run
          (cond
           ((and
             (delib-flow--stage-executed-p
              delib-flow--active-run
              'manual-project-match)
             (delib-flow--manual-project-selection-valid-p
              delib-flow--active-run)
             (not
              (string-equal
               (or active-selection "")
               (or synced-selection ""))))
            delib-flow--active-run)
           ((delib-flow--manual-project-selection-valid-p
             synced-run)
            synced-run)
           (t delib-flow--active-run))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--execute-local-stage
            selected-run
            'manual-project-match))))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-propose-new-project ()
  "Execute the propose-new-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--project-proposal-ready-p delib-flow--active-run)
    (user-error "A reviewed ambiguous or no-match project decision is required before proposing a new project"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'propose-new-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-extract-actions ()
  "Execute the extract-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--project-extraction-context-ready-p delib-flow--active-run)
    (user-error "%s"
                (delib-flow--project-extraction-blocked-message
                 delib-flow--active-run 'action)))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'extract-actions)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-extract-waiting-for ()
  "Execute the extract-waiting-for stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--project-extraction-context-ready-p delib-flow--active-run)
    (user-error "%s"
                (delib-flow--project-extraction-blocked-message
                 delib-flow--active-run 'waiting-for)))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'extract-waiting-for)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-suggest-reference-notes ()
  "Execute the suggest-reference-notes stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--reference-note-suggestion-ready-p delib-flow--active-run)
    (user-error "Accepted inspect result is required before suggesting reference notes"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'suggest-reference-notes)))
  (delib-flow--rerender-current-result))

(defun delib-flow--run-selected-artifact-stage (stage-id selector expected-kind error-message)
  "Run STAGE-ID for the selected artifact from SELECTOR.
Require the selected artifact to have EXPECTED-KIND or raise ERROR-MESSAGE."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (eq (plist-get (funcall selector delib-flow--active-run) :kind)
              expected-kind)
    (user-error "%s" error-message))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run stage-id)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-draft-selected-action ()
  "Execute the selected-action drafting stage for the active run."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'draft-selected-action
   #'delib-flow--selected-action-candidate-for-drafting
   'next-action
   "Select one action candidate before drafting it"))

(defun delib-flow-action-find-support-for-selected-action ()
  "Execute focused support retrieval for the selected action."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'find-support-for-selected-action
   (lambda (run)
     (delib-flow--selected-family-support-item run 'actions))
   'next-action
   "Select one action before retrieving focused support"))

(defun delib-flow-action-draft-selected-waiting-for ()
  "Execute the selected waiting-for drafting stage for the active run."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'draft-selected-waiting-for
   #'delib-flow--selected-waiting-for-candidate-for-drafting
   'waiting-for
   "Select one waiting-for candidate before drafting it"))

(defun delib-flow-action-find-support-for-selected-waiting-for ()
  "Execute focused support retrieval for the selected waiting-for."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'find-support-for-selected-waiting-for
   (lambda (run)
     (delib-flow--selected-family-support-item run 'waiting-fors))
   'waiting-for
   "Select one waiting-for before retrieving focused support"))

(defun delib-flow-action-draft-selected-reference-note ()
  "Execute the selected-note drafting stage for the active run."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'draft-selected-reference-note
   #'delib-flow--selected-reference-note-candidate-for-drafting
   'reference-note
   "Select one reference-note candidate before drafting it"))

(defun delib-flow-action-find-support-for-selected-reference-note ()
  "Execute focused support retrieval for the selected note."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'find-support-for-selected-reference-note
   (lambda (run)
     (delib-flow--selected-family-support-item run 'reference-notes))
   'reference-note
   "Select one note before retrieving focused support"))

(defun delib-flow-action-choose-support-for-selected-reference-note ()
  "Attach one suggested support item to the selected note draft."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (candidates (delib-flow--artifact-family-available-support-candidates
                      synced-run 'reference-notes)))
    (unless candidates
      (user-error "No support suggestions are available for the selected note"))
    (let* ((labels
            (mapcar (lambda (candidate)
                      (cons (delib-flow--reference-note-support-line candidate)
                            candidate))
                    candidates))
           (choice (completing-read "Attach support: "
                                    (mapcar #'car labels)
                                    nil t))
           (selected (cdr (assoc choice labels))))
      (unless selected
        (user-error "No support suggestion was selected"))
      (setq delib-flow--active-run
            (delib-flow--seed-actions
             (delib-flow--set-artifact-family-selected-support
              synced-run
              'reference-notes
              (list selected)
              (delib-flow--focused-support-context (list selected)))))
      (delib-flow--rerender-current-result))))

(defun delib-flow-action-clear-selected-reference-note-support ()
  "Detach attached support from the selected note draft."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--set-artifact-family-selected-support
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'reference-notes nil nil)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-draft-selected-project ()
  "Execute the selected-project drafting stage for the active run."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'draft-selected-project
   #'delib-flow--selected-project-candidate-for-drafting
   'project
   "Select one project candidate before drafting it"))

(defun delib-flow-action-find-support-for-selected-project ()
  "Execute focused support retrieval for the selected project."
  (interactive)
  (delib-flow--run-selected-artifact-stage
   'find-support-for-selected-project
   (lambda (run)
     (delib-flow--selected-family-support-item run 'project-proposals))
   'project
   "Select one project before retrieving focused support"))

(defun delib-flow--restore-previous-selected-draft (family)
  "Restore the latest archived selected draft for artifact FAMILY."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--artifact-family-draft-history delib-flow--active-run family)
    (user-error "No previous draft is available to restore"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--restore-latest-artifact-family-draft
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          family)))
  (delib-flow--rerender-current-result))

(defconst delib-flow--draft-history-entry-title-accessor-alist
  '((reference-note . :text)
    (project . :title))
  "Preferred plist key for a draft-history entry title by artifact kind.")

(defconst delib-flow--draft-history-entry-default-title-alist
  '((reference-note . "Untitled note draft")
    (project . "Untitled project"))
  "Fallback titles for draft-history entries by artifact kind.")

(defun delib-flow--draft-history-entry-title (entry)
  "Return operator-facing title for draft-history ENTRY."
  (let* ((kind (plist-get entry :kind))
         (title-key (or (alist-get kind delib-flow--draft-history-entry-title-accessor-alist)
                        :text)))
    (or (plist-get entry title-key)
        (alist-get kind delib-flow--draft-history-entry-default-title-alist)
        "Untitled draft")))

(defun delib-flow--saved-draft-revision-labels (run family)
  "Return completion labels and indexes for saved draft revisions in RUN FAMILY."
  (let ((history (delib-flow--artifact-family-draft-history run family))
        labels)
    (dolist (entry history (nreverse labels))
      (push
       (cons
        (format "%s - %s"
                (delib-flow--draft-history-entry-label
                 family
                 (1+ (length labels)))
                (delib-flow--draft-history-entry-title entry))
        (cl-position entry history :test #'equal))
       labels))))

(defun delib-flow--choose-saved-selected-draft (family)
  "Restore a chosen saved selected draft revision for artifact FAMILY."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (labels (delib-flow--saved-draft-revision-labels synced-run family)))
    (unless labels
      (user-error "No saved draft revisions are available to restore"))
    (let* ((choice (completing-read
                    (format "Choose saved %s revision: "
                            (delib-flow--artifact-family-label family))
                    (mapcar #'car labels)
                    nil t))
           (index (cdr (assoc choice labels))))
      (unless (integerp index)
        (user-error "No saved draft revision was selected"))
      (setq delib-flow--active-run
            (delib-flow--seed-actions
             (delib-flow--restore-artifact-family-draft-at-index
              synced-run family index)))
      (delib-flow--rerender-current-result))))

(defmacro delib-flow--define-restore-selected-draft-command (name family docstring)
  "Define NAME to restore selected draft for FAMILY with DOCSTRING."
  `(defalias ',name
     (lambda ()
       ,docstring
       (interactive)
       (delib-flow--restore-previous-selected-draft ',family))))

(defmacro delib-flow--define-choose-selected-draft-command (name family docstring)
  "Define NAME to choose saved selected draft for FAMILY with DOCSTRING."
  `(defalias ',name
     (lambda ()
       ,docstring
       (interactive)
       (delib-flow--choose-saved-selected-draft ',family))))

(delib-flow--define-restore-selected-draft-command
    delib-flow-action-restore-previous-selected-action-draft
  actions
  "Restore the previous selected action draft for the active run.")
(delib-flow--define-choose-selected-draft-command
    delib-flow-action-choose-saved-selected-action-draft
  actions
  "Choose a saved selected action draft revision for the active run.")
(delib-flow--define-restore-selected-draft-command
    delib-flow-action-restore-previous-selected-waiting-for-draft
  waiting-fors
  "Restore the previous selected waiting-for draft for the active run.")
(delib-flow--define-choose-selected-draft-command
    delib-flow-action-choose-saved-selected-waiting-for-draft
  waiting-fors
  "Choose a saved selected waiting-for draft revision for the active run.")
(delib-flow--define-restore-selected-draft-command
    delib-flow-action-restore-previous-selected-reference-note-draft
  reference-notes
  "Restore the previous selected note draft for the active run.")
(delib-flow--define-choose-selected-draft-command
    delib-flow-action-choose-saved-selected-reference-note-draft
  reference-notes
  "Choose a saved selected note draft revision for the active run.")
(delib-flow--define-restore-selected-draft-command
    delib-flow-action-restore-previous-selected-project-draft
  project-proposals
  "Restore the previous selected project draft for the active run.")
(delib-flow--define-choose-selected-draft-command
    delib-flow-action-choose-saved-selected-project-draft
  project-proposals
  "Choose a saved selected project draft revision for the active run.")

(defun delib-flow-action-decide-cloud-pass ()
  "Execute the decide-cloud-pass stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'decide-cloud-pass)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-sanitize-for-cloud ()
  "Execute the sanitize-for-cloud stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'sanitize-for-cloud)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-approve-cloud-send ()
  "Execute the approve-cloud-send stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'approve-cloud-send)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-restart-cloud-path ()
  "Restart the current cloud path for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--restart-cloud-path-run
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run))))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-run-cloud-stage ()
  "Execute the run-cloud-stage stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (if (delib-flow--run-cloud-stage-p 'run-cloud-stage)
            (delib-flow--run-stage-in-cloud
             (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
             'run-cloud-stage)
          (delib-flow--run-stage-locally
           (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
           'run-cloud-stage)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-retry-rerouted-cloud-stage ()
  "Retry the current rerouted cloud target for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--direct-cloud-retry-ready-p delib-flow--active-run)
    (user-error "No rerouted cloud stage is ready for direct retry"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-in-cloud
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'run-cloud-stage)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-resolve-cloud-failure ()
  "Execute the resolve-cloud-failure stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'resolve-cloud-failure)))
  (if (eq (plist-get (delib-flow--run-session delib-flow--active-run) :status)
          'aborted)
      (let ((buffer (delib-flow--control-buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delib-flow--teardown-active-run))
    (delib-flow--rerender-current-result)))

(defun delib-flow-action-approve-candidate-reintegration ()
  "Execute the approve-candidate-reintegration stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'approve-candidate-reintegration)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-integrate-into-source ()
  "Execute the integrate-into-source stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'integrate-into-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-reject-draft-filing-artifact ()
  "Execute the reject-draft-filing-artifact stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((synced-run
         (delib-flow--validate-filing-selection-entry
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--execute-local-stage
            synced-run
            'reject-draft-filing-artifact))))
  (delib-flow--rerender-current-result))

(defun delib-flow-choose-filing-selection ()
  "Choose a valid filing artifact selection with completion."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--filing-selection-active-p delib-flow--active-run)
    (user-error "No draft filing artifacts are available to choose from"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (labels (delib-flow--filing-selection-labels synced-run))
         (current (delib-flow--filing-selection-current-value synced-run labels))
         (choice (completing-read
                  "Filing selection: "
                  (mapcar #'car labels)
                  nil t nil nil
                  (car (rassoc current labels)))))
    (setq delib-flow--active-run
          (delib-flow--set-filing-selection-value
           synced-run
           (or (cdr (assoc choice labels))
               choice)))
    (delib-flow--clear-sticky-anchor)
    (delib-flow--rerender-active-run-buffer)))

(delib-flow--define-function delib-flow-choose-reference-note-template
    nil
  "Choose an org-roam template for the active reference-note filing artifact."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((synced-run
          (delib-flow--sync-run-from-control-buffer
           delib-flow--active-run))
         (item
          (delib-flow--reference-note-preview-item
           synced-run)))
    (unless
        (eq (plist-get item :kind)
            'reference-note)
      (user-error
       "No reference-note filing artifact is currently active"))
    (let* ((options
            (delib-flow--reference-note-capture-template-options
             item))
           (current
            (delib-flow--reference-note-effective-template-key
             item synced-run))
           (choice
            (completing-read
             "Reference note template: "
             (mapcar #'car options) nil t nil
             nil
             (car (rassoc current options))))
           (selected-key
            (or (cdr (assoc choice options))
                choice))
           (updated-run
            (delib-flow--set-reference-note-capture-field
             synced-run "Template key"
             selected-key))
           (path-required
            (delib-flow--reference-note-org-roam-target-requires-path-p
             item updated-run))
           (current-path
            (delib-flow--reference-note-effective-target-override
             updated-run)))
      (when path-required
        (setq updated-run
              (delib-flow--set-reference-note-capture-field
               updated-run "Target path"
               (read-string
                "Reference note target path (relative to org-roam root, without .org): "
                current-path))))
      (unless path-required
        (setq updated-run
              (delib-flow--set-reference-note-capture-field
               updated-run "Target path" "")))
      (setq delib-flow--active-run
            updated-run)
      (delib-flow--clear-sticky-anchor)
      (delib-flow--rerender-active-run-buffer))))

(defun delib-flow-action-select-approved-filing-actions ()
  "Execute the select-approved-filing-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((synced-run
         (delib-flow--validate-filing-selection-entry
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--execute-local-stage
            synced-run
            'select-approved-filing-actions))))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-file-approved-outputs ()
  "Execute the file-approved-outputs stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'file-approved-outputs)))
  (delib-flow--rerender-current-result)
  (delib-flow--show-filed-target-locations delib-flow--active-run))

(defun delib-flow-action-resolve-filing-conflict ()
  "Execute the resolve-filing-conflict stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'resolve-filing-conflict)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-stage-placeholder ()
  "Signal that the selected stage exists but is not yet implemented."
  (interactive)
  (user-error "This stage is not implemented yet"))

(defun delib-flow--inspect-source-action (run)
  "Return the inspect-source action for RUN."
  (delib-flow--make-action
   'inspect-source
   (if (delib-flow--stage-executed-p run 'inspect-source)
       "Retry Inspect Source"
     "Inspect Source")
   'available
   nil
   #'delib-flow-action-inspect-source
   10))

(defun delib-flow--accept-inspect-source-action (run)
  "Return the accept-inspect-source action for RUN."
  (when (eq (delib-flow--stage-review-state run 'inspect-source) 'pending-review)
    (delib-flow--make-action
     'accept-inspect-source
     "Accept Inspect Result"
     'available
     nil
     #'delib-flow-action-accept-inspect-source
     15)))

(defun delib-flow--reject-inspect-source-action (run)
  "Return the reject-inspect-source action for RUN."
  (when (eq (delib-flow--stage-review-state run 'inspect-source) 'pending-review)
    (delib-flow--make-action
     'reject-inspect-source
     "Reject Inspect Result"
     'available
     nil
     #'delib-flow-action-reject-inspect-source
     16)))

(defun delib-flow--match-project-action (run)
  "Return the match-project action for RUN."
  (when (or (delib-flow--stage-accepted-p run 'inspect-source)
            (delib-flow--stage-executed-p run 'match-project))
    (delib-flow--make-action
     'match-project
     (if (delib-flow--stage-executed-p run 'match-project)
         "Retry Match Project"
       "Match Project")
     'available
     nil
     #'delib-flow-action-match-project
     20)))

(defun delib-flow--accept-match-project-action (run)
  "Return the accept-match-project action for RUN."
  (when (eq (delib-flow--stage-review-state run 'match-project) 'pending-review)
    (delib-flow--make-action
     'accept-match-project
     "Accept Project Match"
     'available
     nil
     #'delib-flow-action-accept-match-project
     21)))

(defun delib-flow--reject-match-project-action (run)
  "Return the reject-match-project action for RUN."
  (when (eq (delib-flow--stage-review-state run 'match-project) 'pending-review)
    (delib-flow--make-action
     'reject-match-project
     "Reject Project Match"
     'available
     nil
     #'delib-flow-action-reject-match-project
     22)))

(defun delib-flow--discover-reference-material-action (run)
  "Return the discover-reference-material action for RUN."
  (when (or (delib-flow--project-decision-ready-p run)
            (eq (delib-flow--stage-review-state run 'inspect-source) 'accepted))
    (delib-flow--make-action
     'discover-reference-material
     (if (delib-flow--stage-executed-p run 'discover-reference-material)
         "Retry Discover Relevant Reference Material"
       "Discover Relevant Reference Material")
     'available
     nil
     #'delib-flow-action-discover-reference-material
     30)))

(defun delib-flow--filter-reference-material-action (run)
  "Return the filter-reference-material action for RUN."
  (when (and (delib-flow--stage-executed-p run 'discover-reference-material)
             (plist-get (delib-flow--run-working-context run)
                        :retrieved-candidates))
    (delib-flow--make-action
     'filter-reference-material
     (if (delib-flow--stage-executed-p run 'filter-reference-material)
         "Retry Filter Useful Reference Material"
       "Filter Useful Reference Material")
     'available
     nil
     #'delib-flow-action-filter-reference-material
     35)))

(defun delib-flow--manual-project-match-action (run)
  "Return the manual-project-match action for RUN."
  (when (and (not (delib-flow--selected-project-drafted-p run))
             (or (and (memq (delib-flow--stage-review-state run 'match-project)
                            '(accepted rejected))
                      (memq (delib-flow--match-status run) '(matched ambiguous no-match)))
                 (delib-flow--stage-executed-p run 'manual-project-match)))
    (delib-flow--make-action
     'manual-project-match
     (if (delib-flow--stage-executed-p run 'manual-project-match)
         "Retry Choose Project Manually"
       "Choose Project Manually")
     'available
     nil
     #'delib-flow-action-manual-project-match
     40)))

(defun delib-flow--edit-operator-intent-action (run)
  "Return the edit-operator-intent action for RUN."
  (when (or (delib-flow--project-flow-active-p run)
            (delib-flow--stage-executed-p run 'inspect-source))
    (delib-flow--make-action
     'edit-operator-intent
     "Edit Operator Intent"
     'available
     "Update the dedicated operator-intent field. This strongly steers source interpretation and extraction."
     #'delib-flow-action-edit-operator-intent
     (if (delib-flow--project-extraction-soft-warning run)
         39
       65))))

(defun delib-flow--extract-actions-action (run)
  "Return the extract-actions action for RUN."
  (when (or (eq (delib-flow--accepted-project-status run) 'matched)
            (delib-flow--selected-project-drafted-p run))
    (delib-flow--make-action
     'extract-actions
     (if (delib-flow--stage-executed-p run 'extract-actions)
         "Retry Extract Actions"
       "Extract Actions")
     'available
     nil
     #'delib-flow-action-extract-actions
     50)))

(defun delib-flow--extract-waiting-for-action (run)
  "Return the extract-waiting-for action for RUN."
  (when (or (eq (delib-flow--accepted-project-status run) 'matched)
            (delib-flow--selected-project-drafted-p run))
    (delib-flow--make-action
     'extract-waiting-for
     (if (delib-flow--stage-executed-p run 'extract-waiting-for)
         "Retry Extract Waiting-For"
       "Extract Waiting-For")
     'available
     nil
     #'delib-flow-action-extract-waiting-for
     51)))

(defun delib-flow--suggest-reference-notes-action (run)
  "Return the suggest-reference-notes action for RUN."
  (when (delib-flow--stage-accepted-p run 'inspect-source)
    (delib-flow--make-action
     'suggest-reference-notes
     (if (delib-flow--stage-executed-p run 'suggest-reference-notes)
         "Retry Suggest Reference Notes"
       "Suggest Reference Notes")
     'available
     nil
     #'delib-flow-action-suggest-reference-notes
     52)))

(defun delib-flow--draft-selected-reference-note-action (run)
  "Return the draft-selected-reference-note action for RUN."
  (when-let ((item (delib-flow--selected-reference-note-candidate-for-drafting run)))
    (when (eq (plist-get item :kind) 'reference-note)
      (delib-flow--make-action
       'draft-selected-reference-note
       (if (delib-flow--artifact-family-selected-draft run 'reference-notes)
           "Regenerate Selected Note"
         "Draft Selected Note")
       'available
       nil
       #'delib-flow-action-draft-selected-reference-note
       61))))

(defun delib-flow--find-support-for-selected-reference-note-action (run)
  "Return the find-support-for-selected-reference-note action for RUN."
  (when (delib-flow--selected-reference-note-candidate-for-drafting run)
    (delib-flow--make-action
     'find-support-for-selected-reference-note
     "Find Support for Selected Note"
     'available
     nil
     #'delib-flow-action-find-support-for-selected-reference-note
     62)))

(defun delib-flow--choose-support-for-selected-reference-note-action (run)
  "Return the choose-support-for-selected-reference-note action for RUN."
  (when (delib-flow--artifact-family-available-support-candidates run 'reference-notes)
    (delib-flow--make-action
     'choose-support-for-selected-reference-note
     "Choose Support for This Draft"
     'available
     "Attach only the supporting note(s) you want shaping the selected note draft."
     #'delib-flow-action-choose-support-for-selected-reference-note
     63)))

(defun delib-flow--clear-selected-reference-note-support-action (run)
  "Return the clear-selected-reference-note-support action for RUN."
  (when (delib-flow--artifact-family-selected-support-candidates run 'reference-notes)
    (delib-flow--make-action
     'clear-selected-reference-note-support
     "Detach Selected Support"
     'available
     "Remove attached support from the selected note draft without discarding the draft itself."
     #'delib-flow-action-clear-selected-reference-note-support
     64)))

(defun delib-flow--edit-selected-reference-note-title-action (run)
  "Return the edit-selected-reference-note-title action for RUN."
  (when (delib-flow--reference-note-workspace-item run)
    (delib-flow--make-action
     'edit-selected-reference-note-title
     "Edit Note Title"
     'available
     "Change the saved note title for the currently selected note."
     #'delib-flow-action-edit-selected-reference-note-title
     65)))

(defun delib-flow--choose-reference-note-template-action (run)
  "Return the choose-reference-note-template action for RUN."
  (when (delib-flow--reference-note-workspace-item run)
    (delib-flow--make-action
     'choose-reference-note-template
     "Change Reference Note Template"
     'available
     "Choose the template used when saving the selected note."
     #'delib-flow-action-choose-reference-note-template
     66)))

(defun delib-flow--edit-selected-reference-note-target-path-action (run)
  "Return the edit-selected-reference-note-target-path action for RUN."
  (when (delib-flow--reference-note-workspace-item run)
    (delib-flow--make-action
     'edit-selected-reference-note-target-path
     "Edit Target Path"
     'available
     "Change the saved target path override for the currently selected note."
     #'delib-flow-action-edit-selected-reference-note-target-path
     67)))

(defun delib-flow--edit-selected-reference-note-draft-body-action (run)
  "Return the edit-selected-reference-note-draft-body action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'edit-selected-reference-note-draft-body
     "Edit Draft Body"
     'available
     "Edit only the working-draft body for the current selected note."
     #'delib-flow-action-edit-selected-reference-note-draft-body
     68)))

(defun delib-flow--refresh-selected-reference-note-draft-body-action (run)
  "Return the refresh-selected-reference-note-draft-body action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'refresh-selected-reference-note-draft-body
     "Regenerate Draft Body"
     'available
     "Run the selected-note body drafting stage and replace only the working-draft body."
     #'delib-flow-action-refresh-selected-reference-note-draft-body
     69)))

(defun delib-flow--edit-selected-reference-note-source-highlights-action (run)
  "Return the edit-selected-reference-note-source-highlights action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'edit-selected-reference-note-source-highlights
     "Edit Source Highlights"
     'available
     "Edit only the source highlights section of the current note draft."
     #'delib-flow-action-edit-selected-reference-note-source-highlights
     70)))

(defun delib-flow--refresh-selected-reference-note-source-highlights-action (run)
  "Return the refresh-selected-reference-note-source-highlights action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'refresh-selected-reference-note-source-highlights
     "Regenerate Source Highlights"
     'available
     "Run the source-highlights drafting stage and replace only the source highlights section."
     #'delib-flow-action-refresh-selected-reference-note-source-highlights
     71)))

(defun delib-flow--edit-selected-reference-note-related-material-action (run)
  "Return the edit-selected-reference-note-related-material action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'edit-selected-reference-note-related-material
     "Edit Related Material"
     'available
     "Edit only the related material section of the current note draft."
     #'delib-flow-action-edit-selected-reference-note-related-material
     72)))

(defun delib-flow--refresh-selected-reference-note-related-material-action (run)
  "Return the refresh-selected-reference-note-related-material action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'refresh-selected-reference-note-related-material
     "Regenerate Related Material"
     'available
     "Run the related-material drafting stage and replace only the related material section."
     #'delib-flow-action-refresh-selected-reference-note-related-material
     73)))

(defun delib-flow--edit-selected-reference-note-reuse-angle-action (run)
  "Return the edit-selected-reference-note-reuse-angle action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'edit-selected-reference-note-reuse-angle
     "Edit Reuse Angle"
     'available
     "Edit only the reuse angle line of the current note draft."
     #'delib-flow-action-edit-selected-reference-note-reuse-angle
     74)))

(defun delib-flow--refresh-selected-reference-note-reuse-angle-action (run)
  "Return the refresh-selected-reference-note-reuse-angle action for RUN."
  (when (delib-flow--reference-note-selected-draft run)
    (delib-flow--make-action
     'refresh-selected-reference-note-reuse-angle
     "Regenerate Reuse Angle"
     'available
     "Run the reuse-angle drafting stage and replace only the reuse angle line."
     #'delib-flow-action-refresh-selected-reference-note-reuse-angle
     75)))

(defun delib-flow--restore-previous-selected-reference-note-draft-action (run)
  "Return the restore-previous-selected-reference-note-draft action for RUN."
  (when (and (delib-flow--artifact-family-selected-draft run 'reference-notes)
             (delib-flow--artifact-family-draft-history run 'reference-notes))
    (delib-flow--make-action
     'restore-previous-selected-reference-note-draft
     "Restore Previous Note Draft"
     'available
     "Restore the most recent prior selected-note draft without disturbing the queue."
     #'delib-flow-action-restore-previous-selected-reference-note-draft
     63)))

(defun delib-flow--choose-saved-selected-reference-note-draft-action (run)
  "Return the choose-saved-selected-reference-note-draft action for RUN."
  (when (delib-flow--artifact-family-draft-history run 'reference-notes)
    (delib-flow--make-action
     'choose-saved-selected-reference-note-draft
     "Choose Saved Note Revision"
     'available
     "Restore any stored selected-note revision from local draft history."
     #'delib-flow-action-choose-saved-selected-reference-note-draft
     64)))

(defun delib-flow--draft-selected-waiting-for-action (run)
  "Return the draft-selected-waiting-for action for RUN."
  (when-let ((item (delib-flow--selected-waiting-for-candidate-for-drafting run)))
    (when (eq (plist-get item :kind) 'waiting-for)
      (delib-flow--make-action
       'draft-selected-waiting-for
       (if (delib-flow--artifact-family-selected-draft run 'waiting-fors)
           "Regenerate Selected Waiting-For"
         "Draft Selected Waiting-For")
       'available
       nil
       #'delib-flow-action-draft-selected-waiting-for
       51))))

(defun delib-flow--find-support-for-selected-waiting-for-action (run)
  "Return the find-support-for-selected-waiting-for action for RUN."
  (when (delib-flow--selected-waiting-for-candidate-for-drafting run)
    (delib-flow--make-action
     'find-support-for-selected-waiting-for
     "Find Support for Selected Waiting-For"
     'available
     nil
     #'delib-flow-action-find-support-for-selected-waiting-for
     52)))

(defun delib-flow--restore-previous-selected-waiting-for-draft-action (run)
  "Return the restore-previous-selected-waiting-for-draft action for RUN."
  (when (and (delib-flow--artifact-family-selected-draft run 'waiting-fors)
             (delib-flow--artifact-family-draft-history run 'waiting-fors))
    (delib-flow--make-action
     'restore-previous-selected-waiting-for-draft
     "Restore Previous Waiting-For Draft"
     'available
     "Restore the most recent prior selected waiting-for draft without disturbing the queue."
     #'delib-flow-action-restore-previous-selected-waiting-for-draft
     53)))

(defun delib-flow--choose-saved-selected-waiting-for-draft-action (run)
  "Return the choose-saved-selected-waiting-for-draft action for RUN."
  (when (delib-flow--artifact-family-draft-history run 'waiting-fors)
    (delib-flow--make-action
     'choose-saved-selected-waiting-for-draft
     "Choose Saved Waiting-For Revision"
     'available
     "Restore any stored selected waiting-for revision from local draft history."
     #'delib-flow-action-choose-saved-selected-waiting-for-draft
     54)))

(defun delib-flow--draft-selected-action-action (run)
  "Return the draft-selected-action action for RUN."
  (when-let ((item (delib-flow--selected-action-candidate-for-drafting run)))
    (when (eq (plist-get item :kind) 'next-action)
      (delib-flow--make-action
       'draft-selected-action
       (if (delib-flow--artifact-family-selected-draft run 'actions)
           "Regenerate Selected Action"
         "Draft Selected Action")
       'available
       nil
       #'delib-flow-action-draft-selected-action
       41))))

(defun delib-flow--find-support-for-selected-action-action (run)
  "Return the find-support-for-selected-action action for RUN."
  (when (delib-flow--selected-action-candidate-for-drafting run)
    (delib-flow--make-action
     'find-support-for-selected-action
     "Find Support for Selected Action"
     'available
     nil
     #'delib-flow-action-find-support-for-selected-action
     42)))

(defun delib-flow--restore-previous-selected-action-draft-action (run)
  "Return the restore-previous-selected-action-draft action for RUN."
  (when (and (delib-flow--artifact-family-selected-draft run 'actions)
             (delib-flow--artifact-family-draft-history run 'actions))
    (delib-flow--make-action
     'restore-previous-selected-action-draft
     "Restore Previous Action Draft"
     'available
     "Restore the most recent prior selected-action draft without disturbing the queue."
     #'delib-flow-action-restore-previous-selected-action-draft
     43)))

(defun delib-flow--choose-saved-selected-action-draft-action (run)
  "Return the choose-saved-selected-action-draft action for RUN."
  (when (delib-flow--artifact-family-draft-history run 'actions)
    (delib-flow--make-action
     'choose-saved-selected-action-draft
     "Choose Saved Action Revision"
     'available
     "Restore any stored selected-action revision from local draft history."
     #'delib-flow-action-choose-saved-selected-action-draft
     44)))

(defun delib-flow--draft-selected-project-action (run)
  "Return the draft-selected-project action for RUN."
  (when-let ((item (delib-flow--selected-project-candidate-for-drafting run)))
    (when (eq (plist-get item :kind) 'project)
      (delib-flow--make-action
       'draft-selected-project
       (if (delib-flow--artifact-family-selected-draft run 'project-proposals)
           "Regenerate Selected Project"
         "Draft Selected Project")
       'available
       nil
       #'delib-flow-action-draft-selected-project
       52))))

(defun delib-flow--find-support-for-selected-project-action (run)
  "Return the find-support-for-selected-project action for RUN."
  (when (delib-flow--selected-project-candidate-for-drafting run)
    (delib-flow--make-action
     'find-support-for-selected-project
     "Find Support for Selected Project"
     'available
     nil
     #'delib-flow-action-find-support-for-selected-project
     53)))

(defun delib-flow--restore-previous-selected-project-draft-action (run)
  "Return the restore-previous-selected-project-draft action for RUN."
  (when (and (delib-flow--artifact-family-selected-draft run 'project-proposals)
             (delib-flow--artifact-family-draft-history run 'project-proposals))
    (delib-flow--make-action
     'restore-previous-selected-project-draft
     "Restore Previous Project Draft"
     'available
     "Restore the most recent prior selected-project draft without disturbing the queue."
     #'delib-flow-action-restore-previous-selected-project-draft
     54)))

(defun delib-flow--choose-saved-selected-project-draft-action (run)
  "Return the choose-saved-selected-project-draft action for RUN."
  (when (delib-flow--artifact-family-draft-history run 'project-proposals)
    (delib-flow--make-action
     'choose-saved-selected-project-draft
     "Choose Saved Project Revision"
     'available
     "Restore any stored selected-project revision from local draft history."
     #'delib-flow-action-choose-saved-selected-project-draft
     55)))

(defun delib-flow--propose-new-project-action (run)
  "Return the propose-new-project action for RUN."
  (when (and (delib-flow--project-proposal-ready-p run)
             (not (delib-flow--selected-project-drafted-p run)))
    (delib-flow--make-action
     'propose-new-project
     (if (delib-flow--stage-executed-p run 'propose-new-project)
         "Retry Propose New Project"
       "Propose New Project")
     'available
     nil
     #'delib-flow-action-propose-new-project
     42)))

(defun delib-flow--decide-cloud-pass-action (run)
  "Return the decide-cloud-pass action for RUN."
  (when (and (delib-flow--stage-executed-p run 'inspect-source)
             (not (delib-flow--cloud-failure-active-p run)))
    (delib-flow--make-action
     'decide-cloud-pass
     (if (delib-flow--stage-executed-p run 'decide-cloud-pass)
         "Retry Decide on Cloud Pass"
       "Decide on Cloud Pass")
     'available
     nil
     #'delib-flow-action-decide-cloud-pass
     80)))

(defun delib-flow--sanitize-for-cloud-action (run)
  "Return the sanitize-for-cloud action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (or (eq (plist-get (delib-flow--run-routing run) :cloud-switch-pending) t)
                 (delib-flow--stage-executed-p run 'sanitize-for-cloud)))
    (delib-flow--make-action
     'sanitize-for-cloud
     (if (delib-flow--stage-executed-p run 'sanitize-for-cloud)
         "Retry Sanitize for Cloud"
       "Sanitize for Cloud")
     'available
     nil
     #'delib-flow-action-sanitize-for-cloud
     81)))

(defun delib-flow--approve-cloud-send-action (run)
  "Return the approve-cloud-send action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (or (eq (plist-get (delib-flow--run-routing run) :sanitization-status)
                     'prepared)
                 (delib-flow--stage-executed-p run 'approve-cloud-send)))
    (delib-flow--make-action
     'approve-cloud-send
     (if (delib-flow--stage-executed-p run 'approve-cloud-send)
         "Retry Approve Cloud Send"
       "Approve Cloud Send")
     'available
     nil
     #'delib-flow-action-approve-cloud-send
     82)))

(defun delib-flow--run-cloud-stage-action-label (run)
  "Return action label for running the current cloud stage in RUN."
  (let* ((target-stage (or (plist-get (delib-flow--run-working-context run)
                                      :cloud-returned-stage-id)
                           (delib-flow--cloud-target-stage
                            (delib-flow--run-routing run))))
         (base-label (if (delib-flow--stage-executed-p run 'run-cloud-stage)
                         "Retry Run Cloud Stage"
                       "Run Cloud Stage")))
    (if (eq target-stage 'run-cloud-stage)
        base-label
      (format "%s (%s)" base-label
              (delib-flow--stage-label target-stage)))))

(defun delib-flow--direct-cloud-retry-ready-p (run)
  "Return non-nil when RUN can retry the rerouted cloud stage directly."
  (let* ((routing (delib-flow--run-routing run))
         (target-stage (or (plist-get (delib-flow--run-working-context run)
                                      :cloud-returned-stage-id)
                           (plist-get routing :target-cloud-stage))))
    (and (delib-flow--rerouted-cloud-stage-p target-stage)
         (memq (plist-get routing :sanitization-status)
               '(approved returned)))))

(defun delib-flow--retry-rerouted-cloud-stage-action (run)
  "Return the retry-rerouted-cloud-stage action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (delib-flow--direct-cloud-retry-ready-p run))
    (let ((target-stage (or (plist-get (delib-flow--run-working-context run)
                                       :cloud-returned-stage-id)
                            (delib-flow--cloud-target-stage
                             (delib-flow--run-routing run)))))
      (delib-flow--make-action
       'retry-rerouted-cloud-stage
       (format "Retry %s In Cloud"
               (delib-flow--stage-label target-stage))
       'available
       nil
       #'delib-flow-action-retry-rerouted-cloud-stage
       83))))

(defun delib-flow--restart-cloud-path-ready-p (run)
  "Return non-nil when RUN can restart the reviewed cloud path."
  (let* ((routing (delib-flow--run-routing run))
         (target-stage (or (plist-get (delib-flow--run-working-context run)
                                      :cloud-returned-stage-id)
                           (plist-get routing :target-cloud-stage))))
    (and (not (eq (plist-get routing :cloud-switch-pending) t))
         (delib-flow--rerouted-cloud-stage-p target-stage)
         (not (delib-flow--cloud-failure-active-p run)))))

(defun delib-flow--restart-cloud-path-working-state-p (run)
  "Return non-nil when RUN has cloud state worth restarting."
  (or (plist-get (delib-flow--run-working-context run) :cloud-sanitized-context)
      (plist-get (delib-flow--run-working-context run) :cloud-returned-context)
      (plist-get (delib-flow--run-routing run) :cloud-failure)))

(defun delib-flow--restart-cloud-path-action (run)
  "Return the restart-cloud-path action for RUN."
  (when (and (delib-flow--restart-cloud-path-ready-p run)
             (delib-flow--restart-cloud-path-working-state-p run))
    (delib-flow--make-action
     'restart-cloud-path
     "Restart Cloud Path"
     'available
     nil
     #'delib-flow-action-restart-cloud-path
     84)))

(defun delib-flow--run-cloud-stage-action (run)
  "Return the run-cloud-stage action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (or (eq (plist-get (delib-flow--run-routing run) :sanitization-status)
                     'approved)
                 (delib-flow--stage-executed-p run 'run-cloud-stage)))
    (delib-flow--make-action
     'run-cloud-stage
     (delib-flow--run-cloud-stage-action-label run)
     'available
     nil
     #'delib-flow-action-run-cloud-stage
     87)))

(defun delib-flow--resolve-cloud-failure-action (run)
  "Return the resolve-cloud-failure action for RUN."
  (when (delib-flow--cloud-failure-active-p run)
    (let ((stage-id (delib-flow--cloud-failure-stage
                     (delib-flow--run-routing run))))
      (delib-flow--make-action
       'resolve-cloud-failure
       (if (delib-flow--rerouted-cloud-stage-p stage-id)
           (format "Resolve Cloud Failure (%s)"
                   (delib-flow--stage-label stage-id))
         "Resolve Cloud Failure")
       'available
       nil
       #'delib-flow-action-resolve-cloud-failure
       87))))

(defun delib-flow--approve-candidate-reintegration-action (run)
  "Return the approve-candidate-reintegration action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (or (eq (plist-get (delib-flow--run-routing run) :reintegration-status)
                     'pending-review)
                 (delib-flow--stage-executed-p run 'approve-candidate-reintegration)))
    (delib-flow--make-action
     'approve-candidate-reintegration
     (if (delib-flow--stage-executed-p run 'approve-candidate-reintegration)
         "Retry Approve Candidate Reintegration"
       "Approve Candidate Reintegration")
     'available
     nil
     #'delib-flow-action-approve-candidate-reintegration
     87)))

(defun delib-flow--integrate-into-source-action (run)
  "Return the integrate-into-source action for RUN."
  (when (delib-flow--integration-ready-p run)
    (delib-flow--make-action
     'integrate-into-source
     (if (delib-flow--stage-executed-p run 'integrate-into-source)
         "Retry Integrate into Source"
       "Integrate into Source")
     'available
     nil
     #'delib-flow-action-integrate-into-source
     88)))

(defun delib-flow--select-approved-filing-actions-action (run)
  "Return the select-approved-filing-actions action for RUN."
  (when (delib-flow--draft-items-ready-p run)
    (let* ((selected-item (condition-case nil
                              (delib-flow--selected-filing-item run)
                            (error nil)))
           (action
            (delib-flow--make-action
             'select-approved-filing-actions
             (cond
              ((and (delib-flow--project-flow-active-p run)
                    (or (eq (plist-get selected-item :kind) 'project)
                        (delib-flow--project-package-root-for-filing run)))
               (if (delib-flow--stage-executed-p run 'select-approved-filing-actions)
                   "Refresh Approved Package"
                 "Approve Package"))
              ((delib-flow--stage-executed-p run 'select-approved-filing-actions)
               "Approve Another Filing Artifact")
              (t
               "Select Approved Filing Actions"))
             'available
             nil
             #'delib-flow-action-select-approved-filing-actions
             88)))
      (if-let ((warning (delib-flow--selected-filing-item-draft-warning run)))
          (plist-put
           (plist-put action :status 'blocked)
           :reason
           (plist-get warning :message))
        action))))

(defun delib-flow--reject-draft-filing-artifact-action (run)
  "Return the reject-draft-filing-artifact action for RUN."
  (when (delib-flow--draft-items-ready-p run)
    (delib-flow--make-action
     'reject-draft-filing-artifact
     (if (delib-flow--project-flow-active-p run)
         "Reject Package Item"
       (if (delib-flow--stage-executed-p run 'reject-draft-filing-artifact)
           "Reject Another Filing Artifact"
         "Reject Draft Filing Artifact"))
     'available
     nil
     #'delib-flow-action-reject-draft-filing-artifact
     88)))

(defun delib-flow--file-approved-outputs-action (run)
  "Return the file-approved-outputs action for RUN."
  (when (delib-flow--approved-items-ready-p run)
    (delib-flow--make-action
     'file-approved-outputs
     (cond
      ((plist-get (plist-get run :filing) :conflicts)
       "Retry File Approved Outputs")
      ((delib-flow--approved-project-package-p run)
       "File Package")
      (t
       "File Approved Outputs"))
     'available
     nil
     #'delib-flow-action-file-approved-outputs
     89)))

(defun delib-flow--resolve-filing-conflict-action (run)
  "Return the resolve-filing-conflict action for RUN."
  (when (plist-get (plist-get run :filing) :conflicts)
    (delib-flow--make-action
     'resolve-filing-conflict
     "Resolve Filing Conflict"
     'available
     nil
     #'delib-flow-action-resolve-filing-conflict
     89)))

(defun delib-flow--placeholder-stage-action (id label priority)
  "Return a placeholder stage action for ID with LABEL and PRIORITY."
  (delib-flow--make-action
   id
   label
   'placeholder
   "This stage is not implemented yet."
   #'delib-flow-action-stage-placeholder
   priority))

(defun delib-flow--shortcut-event-string ()
  "Return the current shortcut key as a string."
  (char-to-string last-command-event))

(defun delib-flow--action-for-shortcut (run shortcut)
  "Return the rendered action from RUN bound to SHORTCUT."
  (seq-find
   (lambda (action)
     (string= (plist-get action :shortcut) shortcut))
   (delib-flow--sorted-actions run)))

(defun delib-flow--filing-workspace-action-for-shortcut (run shortcut)
  "Return the focused filing-workspace action from RUN bound to SHORTCUT."
  (seq-find
   (lambda (action)
     (string= (plist-get action :shortcut) shortcut))
   (cond
    ((delib-flow--project-focused-filing-workspace-available-p run)
     (delib-flow--focused-project-workspace-actions run))
    ((delib-flow--focused-filing-workspace-p run)
     (delib-flow--focused-reference-note-workspace-actions run))
    (t
     (delib-flow--filing-preview-actions run)))))

(defun delib-flow--post-inspect-actions (run)
  "Return next legal actions after inspect-source has executed in RUN."
  (seq-remove
   #'null
   (list
    (delib-flow--accept-inspect-source-action run)
    (delib-flow--reject-inspect-source-action run)
    (delib-flow--match-project-action run)
    (delib-flow--discover-reference-material-action run)
    (delib-flow--filter-reference-material-action run)
    (delib-flow--decide-cloud-pass-action run)
    (delib-flow--restart-cloud-path-action run)
    (delib-flow--sanitize-for-cloud-action run)
    (delib-flow--approve-cloud-send-action run)
    (delib-flow--retry-rerouted-cloud-stage-action run)
    (delib-flow--run-cloud-stage-action run)
    (delib-flow--suggest-reference-notes-action run)
    (delib-flow--edit-operator-intent-action run)
    (delib-flow--draft-selected-waiting-for-action run)
    (delib-flow--find-support-for-selected-waiting-for-action run)
    (delib-flow--choose-saved-selected-waiting-for-draft-action run)
    (delib-flow--restore-previous-selected-waiting-for-draft-action run)
    (delib-flow--draft-selected-action-action run)
    (delib-flow--find-support-for-selected-action-action run)
    (delib-flow--choose-saved-selected-action-draft-action run)
    (delib-flow--restore-previous-selected-action-draft-action run)
    (delib-flow--draft-selected-reference-note-action run)
    (delib-flow--find-support-for-selected-reference-note-action run)
    (delib-flow--choose-support-for-selected-reference-note-action run)
    (delib-flow--clear-selected-reference-note-support-action run)
    (delib-flow--edit-selected-reference-note-title-action run)
    (delib-flow--choose-reference-note-template-action run)
    (delib-flow--edit-selected-reference-note-target-path-action run)
    (delib-flow--edit-selected-reference-note-draft-body-action run)
    (delib-flow--refresh-selected-reference-note-draft-body-action run)
    (delib-flow--edit-selected-reference-note-source-highlights-action run)
    (delib-flow--refresh-selected-reference-note-source-highlights-action run)
    (delib-flow--edit-selected-reference-note-related-material-action run)
    (delib-flow--refresh-selected-reference-note-related-material-action run)
    (delib-flow--edit-selected-reference-note-reuse-angle-action run)
    (delib-flow--refresh-selected-reference-note-reuse-angle-action run)
    (delib-flow--choose-saved-selected-reference-note-draft-action run)
    (delib-flow--restore-previous-selected-reference-note-draft-action run)
    (delib-flow--draft-selected-project-action run)
    (delib-flow--find-support-for-selected-project-action run)
    (delib-flow--choose-saved-selected-project-draft-action run)
    (delib-flow--restore-previous-selected-project-draft-action run)
    (delib-flow--resolve-cloud-failure-action run)
    (delib-flow--approve-candidate-reintegration-action run)
    (delib-flow--integrate-into-source-action run)
    (delib-flow--reject-draft-filing-artifact-action run)
    (delib-flow--select-approved-filing-actions-action run)
    (delib-flow--resolve-filing-conflict-action run)
    (delib-flow--file-approved-outputs-action run))))

(defun delib-flow--match-status (run)
  "Return the stored project match status from RUN."
  (plist-get (delib-flow--current-project-decision run) :match-status))

(defun delib-flow--post-match-actions (run)
  "Return next legal actions after match-project has executed in RUN."
  (let ((status (delib-flow--match-status run)))
    (seq-remove
     #'null
     (append
      (list
       (delib-flow--match-project-action run)
       (delib-flow--accept-match-project-action run)
       (delib-flow--reject-match-project-action run)
       (delib-flow--manual-project-match-action run)
       (delib-flow--edit-operator-intent-action run))
      (list
       (delib-flow--suggest-reference-notes-action run)
       (delib-flow--draft-selected-waiting-for-action run)
       (delib-flow--find-support-for-selected-waiting-for-action run)
       (delib-flow--choose-saved-selected-waiting-for-draft-action run)
       (delib-flow--restore-previous-selected-waiting-for-draft-action run)
       (delib-flow--draft-selected-action-action run)
       (delib-flow--find-support-for-selected-action-action run)
       (delib-flow--choose-saved-selected-action-draft-action run)
       (delib-flow--restore-previous-selected-action-draft-action run)
       (delib-flow--draft-selected-reference-note-action run)
       (delib-flow--find-support-for-selected-reference-note-action run)
       (delib-flow--choose-support-for-selected-reference-note-action run)
       (delib-flow--clear-selected-reference-note-support-action run)
       (delib-flow--edit-selected-reference-note-title-action run)
       (delib-flow--choose-reference-note-template-action run)
       (delib-flow--edit-selected-reference-note-target-path-action run)
       (delib-flow--edit-selected-reference-note-draft-body-action run)
       (delib-flow--refresh-selected-reference-note-draft-body-action run)
       (delib-flow--edit-selected-reference-note-source-highlights-action run)
       (delib-flow--refresh-selected-reference-note-source-highlights-action run)
       (delib-flow--edit-selected-reference-note-related-material-action run)
       (delib-flow--refresh-selected-reference-note-related-material-action run)
       (delib-flow--edit-selected-reference-note-reuse-angle-action run)
       (delib-flow--refresh-selected-reference-note-reuse-angle-action run)
       (delib-flow--choose-saved-selected-reference-note-draft-action run)
       (delib-flow--restore-previous-selected-reference-note-draft-action run)
       (delib-flow--draft-selected-project-action run)
       (delib-flow--find-support-for-selected-project-action run)
       (delib-flow--choose-saved-selected-project-draft-action run)
       (delib-flow--restore-previous-selected-project-draft-action run)
       (delib-flow--propose-new-project-action run))
      (if (delib-flow--project-decision-ready-p run)
          (append
           (list (delib-flow--discover-reference-material-action run))
           (if (or (eq status 'matched)
                   (delib-flow--selected-project-drafted-p run))
               (list
                (delib-flow--extract-actions-action run)
                (delib-flow--extract-waiting-for-action run))
             nil))
        (list))
      (list
       (delib-flow--filter-reference-material-action run)
       (delib-flow--decide-cloud-pass-action run)
       (delib-flow--restart-cloud-path-action run)
       (delib-flow--sanitize-for-cloud-action run)
       (delib-flow--approve-cloud-send-action run)
       (delib-flow--retry-rerouted-cloud-stage-action run)
       (delib-flow--run-cloud-stage-action run)
       (delib-flow--resolve-cloud-failure-action run)
       (delib-flow--approve-candidate-reintegration-action run)
       (delib-flow--integrate-into-source-action run)
       (delib-flow--reject-draft-filing-artifact-action run)
       (delib-flow--select-approved-filing-actions-action run)
       (delib-flow--resolve-filing-conflict-action run)
       (delib-flow--file-approved-outputs-action run))))))

(defun delib-flow--restart-cloud-path-run (run)
  "Return RUN reset to restart the reviewed cloud path from sanitization."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (routing (delib-flow--clear-cloud-failure-state
                   (delib-flow--run-routing run)))
         (cleared-routing
          (plist-put
           (plist-put
            (plist-put routing :cloud-switch-pending t)
            :sanitization-status 'required)
           :reintegration-status nil))
         (updated-run
          (plist-put
           (plist-put run :working-context
                      (plist-put
                       (plist-put working :cloud-sanitized-context nil)
                       :cloud-returned-context nil))
           :routing cleared-routing)))
    (plist-put
     updated-run :session
     (plist-put (delib-flow--run-session updated-run)
                :current-decision
                "Cloud path restarted. Review sanitization again before sending another cloud attempt."))))

(defun delib-flow--base-actions (run)
  "Return the base action list for RUN."
  (append
   (list (delib-flow--inspect-source-action run))
   (when (delib-flow--stage-executed-p run 'inspect-source)
     (if (delib-flow--stage-executed-p run 'match-project)
         (delib-flow--post-match-actions run)
       (seq-remove #'null (delib-flow--post-inspect-actions run))))
   (list
    (delib-flow--make-action
     'refresh-buffer
     "Refresh Buffer"
     'available
     nil
     #'delib-flow-refresh-buffer
     90)
    (delib-flow--make-action
     'abort-run
     "Abort Run"
     'available
     nil
     #'delib-flow-abort-run
     100))))

(defun delib-flow--compute-actions (run)
  "Return the current action list."
  (delib-flow--base-actions run))

(defun delib-flow--assign-action-shortcuts (actions)
  "Return ACTIONS with stable rendered shortcut keys assigned."
  (let ((keys delib-flow--action-shortcut-keys))
    (mapcar
     (lambda (action)
       (prog1 (plist-put action :shortcut (car keys))
         (setq keys (cdr keys))))
     (sort (copy-tree actions)
           (lambda (left right)
             (< (plist-get left :priority)
                (plist-get right :priority)))))))

(defun delib-flow--apply-action-conflict-state (action conflicts)
  "Return ACTION adjusted for managed-region CONFLICTS."
  (if (or (null conflicts)
          (memq (plist-get action :id) '(refresh-buffer abort-run)))
      action
    (plist-put
     (plist-put action :status 'blocked)
     :reason
     "Managed-region conflicts must be resolved before this action can run.")))

(defun delib-flow--apply-action-in-flight-state (run action)
  "Return ACTION adjusted for any in-flight stage in RUN."
  (if (or (not (delib-flow--run-in-flight-p run))
          (memq (plist-get action :id) '(refresh-buffer abort-run)))
      action
    (plist-put
     (plist-put action :status 'blocked)
     :reason
     (format "Wait for %s to finish before running another workflow action."
             (delib-flow--stage-label
              (delib-flow--run-in-flight-stage-id run))))))

(defun delib-flow--seed-actions (run)
  "Return RUN with computed actions populated."
  (let* ((run (delib-flow--normalize-in-flight-state run))
         (conflicts (plist-get (delib-flow--run-ui run)
                               :managed-region-conflicts))
         (items (delib-flow--assign-action-shortcuts
                 (mapcar (lambda (action)
                           (delib-flow--apply-action-in-flight-state
                            run
                            (delib-flow--apply-action-conflict-state
                             action conflicts)))
                         (delib-flow--compute-actions run)))))
    (plist-put run :actions
               (plist-put (delib-flow--run-actions run)
                          :items
                          items))))

(defun delib-flow-dispatch-action-shortcut ()
  "Execute the rendered action bound to the typed shortcut key."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--surface-mode-p)
    (user-error "Action shortcuts only work in delib-flow surface buffers"))
  (let ((shortcut (delib-flow--shortcut-event-string)))
    (if-let ((action (if (eq delib-flow--surface-kind 'filing-workspace)
                         (delib-flow--filing-workspace-action-for-shortcut
                          delib-flow--active-run shortcut)
                       (delib-flow--action-for-shortcut
                        delib-flow--active-run shortcut))))
        (delib-flow--dispatch-rendered-action action)
      (user-error "No delib-flow action is bound to `%s` right now" shortcut))))

(defun delib-flow-approve-current ()
  "Approve the current pending inspect or project-match result."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--call-current-review-command
   #'delib-flow--approve-stage-command
   "No inspect or project-match result is currently pending review"))

(defun delib-flow-retry-current ()
  "Retry the current pending inspect or project-match stage."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--call-current-review-command
   #'delib-flow--retry-stage-command
   "No inspect or project-match result is currently pending review"))

(defun delib-flow-start ()
  "Start a delib-flow run from the Org heading at point."
  (interactive)
  (delib-flow--cleanup-stale-run)
  (when (delib-flow--active-run-conflict-p)
    (pop-to-buffer (delib-flow--control-buffer))
    (user-error "A delib-flow run is already active"))
  (unless (derived-mode-p 'org-mode)
    (user-error "delib-flow requires Org mode"))
  (unless (delib-flow--org-heading-at-point-p)
    (user-error "Point must be on an Org heading to start delib-flow"))
  (delib-flow--start-run-from-source
   (delib-flow--snapshot-heading)))

(delib-flow--define-function delib-flow-start-from-inbox nil
  "Start a delib-flow run from a top-level heading in `delib-flow-inbox-file'."
  (interactive)
  (delib-flow--cleanup-stale-run)
  (when (delib-flow--active-run-conflict-p)
    (pop-to-buffer
     (delib-flow--control-buffer))
    (user-error
     "A delib-flow run is already active"))
  (let* ((file
          (delib-flow--configured-inbox-file)))
    (unless file
      (user-error
       "`delib-flow-inbox-file' is not configured"))
    (unless (file-readable-p file)
      (user-error
       "Inbox file is not readable: %s"
       file))
    (let* ((snapshots
            (delib-flow--inbox-heading-snapshots
             file
             delib-flow-inbox-outline-path))
           (labels
            (delib-flow--inbox-selection-labels
             snapshots)))
      (unless labels
        (user-error
         "Inbox source has no selectable headings: %s"
         file))
      (let* ((choice
              (completing-read
               "Delib-Flow inbox entry: "
               (mapcar #'car labels) nil t))
             (source
              (or (cdr (assoc choice labels))
                  (cdar labels))))
        (unless source
          (user-error
           "No inbox entry was selected"))
        (delib-flow--start-run-from-source
         source)))))

(provide 'delib-flow-ui)

;;; delib-flow-ui.el ends here
