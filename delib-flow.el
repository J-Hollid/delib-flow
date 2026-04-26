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

(defun delib-flow--initialize-run (source-snapshot)
  "Create a new run state from SOURCE-SNAPSHOT."
  (list :working-context
        (list :source-snapshot source-snapshot
              :inspect-output nil
              :retrieved-candidates nil
              :filtered-context nil
              :manual-edits nil
              :merged-context nil
              :cloud-sanitized-context nil)
        :stage-results nil
        :approvals
        (list :approved-stage-outputs nil
              :pending-approvals nil
              :approved-filing-actions nil
              :cloud-send-approvals nil)
        :routing
        (list :default-local-model delib-flow-default-local-model
              :default-cloud-model delib-flow-default-cloud-model
              :stage-models nil
              :cloud-switch-pending nil
              :sanitization-status nil)
        :filing-plan
        (list :draft-items nil
              :approved-items nil
              :project-changes nil
              :task-insertions nil
              :waiting-insertions nil
              :reference-note-creations nil
              :target-locations nil
              :preview-text nil)
        :audit-metadata
        (list :stage-timestamps nil
              :prompt-ids nil
              :model-ids nil
              :input-snapshot-refs nil
              :decision-outcomes nil)
        :session-metadata
        (list :started-at (current-time)
              :run-buffer delib-flow-control-buffer-name
              :current-stage nil
              :current-decision "Milestone 1 placeholder: no stage execution yet."
              :active t
              :aborted nil)))

(defun delib-flow--section-content (section run)
  "Return Org text for SECTION using RUN state."
  (pcase section
    ("Source"
     (let* ((source (plist-get (plist-get run :working-context) :source-snapshot))
            (title (or (plist-get source :title) "Untitled source"))
            (file (or (plist-get source :file) "No file"))
            (id (or (plist-get source :id) "No ID"))
            (content (string-trim (or (plist-get source :content) ""))))
       (format "** Title\n%s\n\n** File\n%s\n\n** ID\n%s\n\n** Snapshot\n#+begin_example\n%s\n#+end_example\n"
               title file id content)))
    ("Working context"
     "Milestone 1 placeholder.\n")
    ("Stage history"
     "No stages have been executed yet.\n")
    ("Current decision"
     (format "%s\n"
             (plist-get (plist-get run :session-metadata) :current-decision)))
    ("Valid next actions"
     "- Inspect source is not implemented yet.\n- Abort run by closing this buffer.\n")
    ("Filing preview"
     "No filing actions available yet.\n")
    ("Audit status"
     "Audit logging is not implemented in Milestone 1.\n")
    (_ "")))

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
        (goto-char (point-min))
        (view-mode 1))
      (setq-local delib-flow--active-run-buffer t))
    buffer))

(defun delib-flow--teardown-active-run ()
  "Clear active run state."
  (setq delib-flow--active-run nil))

(defun delib-flow--control-buffer-killed ()
  "Handle control buffer teardown."
  (when (bound-and-true-p delib-flow--active-run-buffer)
    (delib-flow--teardown-active-run)))

(defun delib-flow-start ()
  "Start a delib-flow run from the Org heading at point."
  (interactive)
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
