;;; delib-flow.el --- Guided AI workflow control for Org -*- lexical-binding: t; -*-

;; Author: J Holliday
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, ai
;; URL: https://example.invalid/delib-flow

;;; Commentary:

;; delib-flow provides a guided control buffer for deliberate AI-assisted
;; workflow execution from an Org heading at point.

;;; Code:

(require 'org)
(require 'subr-x)

(defgroup delib-flow nil
  "Guided AI workflow control for Org."
  :group 'tools
  :prefix "delib-flow-")

(defcustom delib-flow-my-projects-file nil
  "Path to the authoritative My Projects Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-zk-root nil
  "Path to the searchable zettelkasten root."
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom delib-flow-prompt-library-file nil
  "Path to the prompt library Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-example-structures-file nil
  "Path to the example structures Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-audit-log-file nil
  "Path to the audit log Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-default-local-model nil
  "Default local model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-default-cloud-model nil
  "Default cloud model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defconst delib-flow-control-buffer-name "*delib-flow*"
  "Name of the main delib-flow control buffer.")

(defvar delib-flow--active-run nil
  "Active run state for the current delib-flow session.")

(defconst delib-flow--control-sections
  '("Source"
    "Working context"
    "Stage history"
    "Current decision"
    "Valid next actions"
    "Filing preview"
    "Audit status")
  "Top-level sections rendered in the control buffer.")

(defconst delib-flow--initial-section-anchor-alist
  '((source . "delib-section-source")
    (working-context . "delib-section-working-context")
    (stage-history . "delib-section-stage-history")
    (current-decision . "delib-section-current-decision")
    (valid-next-actions . "delib-section-valid-next-actions")
    (filing-preview . "delib-section-filing-preview")
    (audit-status . "delib-section-audit-status"))
  "Stable anchor identifiers for top-level control-buffer sections.")

(defconst delib-flow--section-renderer-alist
  '(("Source" . delib-flow--render-source-section)
    ("Working context" . delib-flow--render-working-context-section)
    ("Stage history" . delib-flow--render-stage-history-section)
    ("Current decision" . delib-flow--render-current-decision-section)
    ("Valid next actions" . delib-flow--render-valid-next-actions-section)
    ("Filing preview" . delib-flow--render-filing-preview-section)
    ("Audit status" . delib-flow--render-audit-status-section))
  "Renderer functions keyed by section title.")

(defun delib-flow--org-heading-at-point-p ()
  "Return non-nil when point is on an Org heading."
  (org-at-heading-p))

(defun delib-flow--snapshot-heading ()
  "Capture a frozen snapshot of the Org heading at point.

Return a plist containing source metadata and content."
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (begin (point))
           (end (save-excursion
                  (org-end-of-subtree t t)
                  (point)))
           (content (buffer-substring-no-properties begin end))
           (file (buffer-file-name))
           (id (org-entry-get (point) "ID"))
           (outline-path (ignore-errors (org-get-outline-path t t))))
      (list :title title
            :file file
            :id id
            :begin begin
            :end end
            :outline-path outline-path
            :content content
            :source-type 'unknown))))

(defun delib-flow--make-editable-block (id kind section heading-path anchor-id)
  "Return an editable block object for ID and KIND in SECTION.

HEADING-PATH is the intended logical location for the block.
ANCHOR-ID is the stable in-buffer anchor for the block."
  (list :id id
        :kind kind
        :section section
        :heading-path heading-path
        :anchor-id anchor-id
        :status 'clean
        :original-text ""
        :current-text ""
        :accepted-text ""
        :validation-status 'valid
        :validation-message nil))

(defun delib-flow--initial-editable-blocks ()
  "Return the initial editable block alist for a new run."
  (list
   (cons 'context-main
         (delib-flow--make-editable-block
          'context-main
          'context
          "Working context"
          '("Working context" "Editable working slice")
          "delib-edit-context-main"))
   (cons 'operator-notes
         (delib-flow--make-editable-block
          'operator-notes
          'notes
          "Current decision"
          '("Current decision" "Operator notes")
          "delib-edit-operator-notes"))))

(defun delib-flow--initial-section-anchors ()
  "Return the initial section-anchor alist for a new run."
  (copy-tree delib-flow--initial-section-anchor-alist))

(defun delib-flow--initial-actions-state ()
  "Return the initial action-state plist for a new run."
  (list :items nil
        :selected-action nil
        :last-action nil))

(defun delib-flow--make-action (id label status reason handler priority)
  "Return an action object for ID with LABEL, STATUS, and HANDLER.

REASON describes blocked or placeholder state.
PRIORITY controls display order."
  (list :id id
        :label label
        :status status
        :reason reason
        :section "Valid next actions"
        :priority priority
        :handler handler
        :requires-clean-managed-regions t
        :requires-valid-edits t))

(defun delib-flow--initial-session-state ()
  "Return the initial session-state plist for a new run."
  (list :status 'active
        :started-at (current-time)
        :ended-at nil
        :current-stage nil
        :current-decision "Review working context and choose next action."
        :run-buffer delib-flow-control-buffer-name
        :active t
        :aborted nil))

(defun delib-flow--initial-ui-state ()
  "Return the initial UI-state plist for a new run."
  (list :section-anchors (delib-flow--initial-section-anchors)
        :editable-blocks (delib-flow--initial-editable-blocks)
        :managed-region-conflicts nil
        :render-version 1
        :last-rendered-at nil))

(defun delib-flow--run-source (run)
  "Return the source plist from RUN."
  (plist-get run :source))

(defun delib-flow--run-working-context (run)
  "Return the working-context plist from RUN."
  (plist-get run :working-context))

(defun delib-flow--run-session (run)
  "Return the session plist from RUN."
  (plist-get run :session))

(defun delib-flow--run-ui (run)
  "Return the UI plist from RUN."
  (plist-get run :ui))

(defun delib-flow--run-actions (run)
  "Return the actions plist from RUN."
  (plist-get run :actions))

(defun delib-flow--editable-block (run block-id)
  "Return editable block BLOCK-ID from RUN."
  (alist-get block-id
             (plist-get (delib-flow--run-ui run) :editable-blocks)))

(defun delib-flow--editable-block-kind-name (block)
  "Return the string kind name for BLOCK."
  (symbol-name (plist-get block :kind)))

(defun delib-flow--editable-block-text (block)
  "Return the preferred text payload for editable BLOCK."
  (or (plist-get block :current-text)
      (plist-get block :accepted-text)
      (plist-get block :original-text)
      ""))

(defun delib-flow--editable-block-status (block text)
  "Return lifecycle status for BLOCK given TEXT."
  (if (equal text (plist-get block :original-text))
      'clean
    'edited))

(defun delib-flow--set-editable-block-text (block text)
  "Return BLOCK updated with TEXT and derived status."
  (plist-put
   (plist-put block :current-text text)
   :status
   (delib-flow--editable-block-status block text)))

(defun delib-flow--replace-editable-block (blocks block-id new-block)
  "Return BLOCKS with BLOCK-ID replaced by NEW-BLOCK."
  (mapcar (lambda (entry)
            (if (eq (car entry) block-id)
                (cons block-id new-block)
              entry))
          blocks))

(defun delib-flow--set-editable-block (run block-id new-block)
  "Return RUN with BLOCK-ID replaced by NEW-BLOCK."
  (let* ((ui (delib-flow--run-ui run))
         (blocks (plist-get ui :editable-blocks))
         (updated-blocks
          (delib-flow--replace-editable-block blocks block-id new-block))
         (updated-ui (plist-put ui :editable-blocks updated-blocks)))
    (plist-put run :ui updated-ui)))

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

(defun delib-flow--render-editable-block (run block-id)
  "Return Org text for editable block BLOCK-ID from RUN."
  (let* ((block (delib-flow--editable-block run block-id))
         (kind (plist-get block :kind))
         (text (delib-flow--editable-block-text block)))
    (format "#+begin_delib-edit %s\n%s\n#+end_delib-edit\n"
            kind text)))

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
            (push (cons (max (point-min) (1- start))
                        (match-beginning 0))
                  bounds)))))
    (nreverse bounds)))

(defun delib-flow--protect-managed-regions ()
  "Protect managed buffer regions while leaving editable block bodies writable."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t delib-flow-managed t))
    (dolist (bounds (delib-flow--editable-block-body-bounds))
      (add-text-properties
       (car bounds) (cdr bounds)
       '(read-only nil
         delib-flow-managed nil
         delib-flow-editable t
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

(defun delib-flow--run-active-p (run)
  "Return non-nil when RUN is currently active."
  (eq (plist-get (delib-flow--run-session run) :status) 'active))

(defun delib-flow--control-buffer ()
  "Return the control buffer when it exists."
  (get-buffer delib-flow-control-buffer-name))

(defun delib-flow--action-placeholder-p (action)
  "Return non-nil when ACTION is a placeholder."
  (eq (plist-get action :status) 'placeholder))

(defun delib-flow--compute-actions (_run)
  "Return the current action list."
  (list
   (delib-flow--make-action
    'inspect-source
    "Inspect Source"
    'placeholder
    "Stage execution is not implemented in Milestone 1."
    #'delib-flow-action-inspect-source
    10)
   (delib-flow--make-action
    'refresh-buffer
    "Refresh Buffer"
    'available
    nil
    #'delib-flow-refresh-buffer
    20)
   (delib-flow--make-action
    'abort-run
    "Abort Run"
    'available
    nil
    #'delib-flow-abort-run
    30)))

(defun delib-flow--apply-action-conflict-state (action conflicts)
  "Return ACTION adjusted for managed-region CONFLICTS."
  (if (or (null conflicts)
          (memq (plist-get action :id) '(refresh-buffer abort-run)))
      action
    (plist-put
     (plist-put action :status 'blocked)
     :reason
     "Managed-region conflicts must be resolved before this action can run.")))

(defun delib-flow--seed-actions (run)
  "Return RUN with computed actions populated."
  (let* ((conflicts (plist-get (delib-flow--run-ui run)
                               :managed-region-conflicts))
         (items (mapcar (lambda (action)
                          (delib-flow--apply-action-conflict-state
                           action conflicts))
                        (delib-flow--compute-actions run))))
    (plist-put run :actions
               (plist-put (delib-flow--run-actions run)
                          :items
                          items))))

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

(defun delib-flow--format-action-line (action)
  "Return a display line for ACTION."
  (let* ((label (plist-get action :label))
         (status (plist-get action :status))
         (reason (plist-get action :reason))
         (line (format "- %s [%s]" label status)))
    (if reason
        (format "%s: %s\n" line reason)
      (format "%s\n" line))))

(defun delib-flow--sorted-actions (run)
  "Return RUN actions sorted by priority."
  (sort (copy-sequence (plist-get (delib-flow--run-actions run) :items))
        (lambda (left right)
          (< (plist-get left :priority)
             (plist-get right :priority)))))

(defun delib-flow--render-source-section (run)
  "Return Org text for the Source section from RUN."
  (let* ((source (delib-flow--run-source run))
         (title (or (plist-get source :title) "Untitled source"))
         (file (or (plist-get source :file) "No file"))
         (id (or (plist-get source :id) "No ID"))
         (content (string-trim (or (plist-get source :content) ""))))
    (format "** Title\n%s\n\n** File\n%s\n\n** ID\n%s\n\n** Snapshot\n#+begin_example\n%s\n#+end_example\n"
            title file id content)))

(defun delib-flow--render-working-context-section (_run)
  "Return Org text for the Working context section."
  (format
   "** Context status\nInspect output: not available.\nRetrieved context: not available.\nFiltered context: not available.\nCloud-sanitized context: not available.\n\n** Retained context\nNo retained context is available yet.\n\n** Editable working slice\n%s"
   (delib-flow--render-editable-block _run 'context-main)))

(defun delib-flow--render-stage-history-section (_run)
  "Return Org text for the Stage history section."
  "** History status\nNo stages have been executed yet.\n")

(defun delib-flow--render-current-decision-section (run)
  "Return Org text for the Current decision section from RUN."
  (format "** Decision status\n%s\n\n** Operator notes\n%s"
          (plist-get (delib-flow--run-session run) :current-decision)
          (delib-flow--render-editable-block run 'operator-notes)))

(defun delib-flow--render-valid-next-actions-section (_run)
  "Return Org text for the Valid next actions section."
  (mapconcat #'delib-flow--format-action-line
             (delib-flow--sorted-actions _run)
             ""))

(defun delib-flow--render-filing-preview-section (_run)
  "Return Org text for the Filing preview section."
  "** Preview status\nNo filing preview is available yet.\n\n** Approved artifacts\nNo approved artifacts are available yet.\n")

(defun delib-flow--render-audit-status-section (_run)
  "Return Org text for the Audit status section."
  "** Run audit state\nAudit logging is not implemented in Milestone 1.\n\n** Stage audit readiness\nStage audit data is not ready yet.\n")

(defun delib-flow--initialize-run (source-snapshot)
  "Create a new run state from SOURCE-SNAPSHOT."
  (delib-flow--seed-actions
   (list :source source-snapshot
         :working-context
         (list :source-snapshot source-snapshot
               :inspect-output nil
               :retrieved-candidates nil
               :filtered-context nil
               :retained-context nil
               :cloud-sanitized-context nil
               :editable-block-ids '(context-main operator-notes))
         :stage-history
         (list :entries nil
               :latest-stage nil
               :latest-status nil)
         :actions
         (delib-flow--initial-actions-state)
         :routing
         (list :default-local-model delib-flow-default-local-model
               :default-cloud-model delib-flow-default-cloud-model
               :stage-models nil
               :cloud-switch-pending nil
               :sanitization-status nil)
         :filing
         (list :draft-items nil
               :approved-items nil
               :preview-text nil
               :target-locations nil)
         :audit
         (list :run-record nil
               :stage-records nil
               :pending-checkpoints nil
               :last-appended-checkpoint nil)
         :session
         (delib-flow--initial-session-state)
         :ui
         (delib-flow--initial-ui-state))))

(defun delib-flow--section-content (section run)
  "Return Org text for SECTION using RUN state."
  (let ((renderer (cdr (assoc section delib-flow--section-renderer-alist))))
    (if renderer
        (funcall renderer run)
      "")))

(defun delib-flow--render-control-buffer (run)
  "Render the control buffer from RUN."
  (let ((buffer (get-buffer-create delib-flow-control-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* delib-flow run\n")
        (dolist (section delib-flow--control-sections)
          (insert (format "** %s\n" section))
          (insert (delib-flow--section-content section run))
          (unless (bolp) (insert "\n")))
        (setq buffer-read-only nil)
        (delib-flow--protect-managed-regions)
        (goto-char (point-min)))
      (setq-local delib-flow--active-run-buffer t))
    buffer))

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

(defun delib-flow--control-buffer-killed ()
  "Handle control buffer teardown."
  (when (bound-and-true-p delib-flow--active-run-buffer)
    (delib-flow--teardown-active-run)))

(defun delib-flow-action-inspect-source ()
  "Placeholder action for inspect-source."
  (interactive)
  (user-error "Inspect source is not implemented in Milestone 1"))

(defun delib-flow-refresh-buffer ()
  "Refresh the control buffer for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((buffer (delib-flow--control-buffer)))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--set-managed-region-conflicts
            (delib-flow--sync-editable-blocks delib-flow--active-run buffer)
            (delib-flow--managed-region-conflicts delib-flow--active-run
                                                  buffer))))
    (let ((updated-buffer
           (if (buffer-live-p buffer)
               (delib-flow--refresh-buffer-sections delib-flow--active-run
                                                   buffer)
             (delib-flow--render-control-buffer delib-flow--active-run))))
      (with-current-buffer updated-buffer
        (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
      (pop-to-buffer updated-buffer))))

(defun delib-flow-abort-run ()
  "Abort the active delib-flow run and close the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--mark-run-aborted delib-flow--active-run))
  (let ((buffer (delib-flow--control-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (delib-flow--teardown-active-run))

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
  (let ((source (delib-flow--snapshot-heading)))
    (setq delib-flow--active-run
          (delib-flow--initialize-run source))
    (let ((buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
      (pop-to-buffer buffer))))

(provide 'delib-flow)
;;; delib-flow.el ends here
