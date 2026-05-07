;;; delib-flow-audit.el --- Audit helpers for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Audit state, persistence, navigation, and archive commands.

;;; Code:

(require 'org)
(require 'pp)
(require 'subr-x)

(defun delib-flow--audit-provider (entry)
  "Return audit provider label for stage ENTRY."
  (symbol-name
   (or (plist-get entry :provider)
       (if (delib-flow--run-cloud-stage-p (plist-get entry :stage-id))
           'cloud
         'local))))

(defun delib-flow--audit-model-name (entry)
  "Return audit model name from stage ENTRY."
  (or
   (plist-get (plist-get entry :raw-output) :selected-model)
   (let* ((package (plist-get entry :input-package))
          (routing (plist-get package :routing))
          (provider (or (plist-get entry :provider)
                        (if (delib-flow--run-cloud-stage-p
                             (plist-get entry :stage-id))
                            'cloud
                          'local))))
     (pcase provider
       ('cloud
        (plist-get routing :default-cloud-model))
       (_
        (plist-get routing :default-local-model))))
   "model-unrecorded"))

(defun delib-flow--entry-attempt-number (entries entry)
  "Return 1-indexed attempt number for ENTRY within ENTRIES."
  (let ((attempt-number 0)
        (result 0))
    (dolist (candidate entries result)
      (when (eq (plist-get candidate :stage-id)
                (plist-get entry :stage-id))
        (setq attempt-number (1+ attempt-number)))
      (when (and (eq candidate entry)
                 (= result 0))
        (setq result attempt-number)))))

(defun delib-flow--audit-transport-stage-id (entry)
  "Return audit transport stage id for ENTRY, if any."
  (when (delib-flow--cloud-shadow-entry-p entry)
    'run-cloud-stage))

(defun delib-flow--audit-payload-policy ()
  "Return the current audit payload policy."
  delib-flow-audit-payload-policy)

(defun delib-flow--audit-redaction-profile ()
  "Return the current audit redaction profile."
  delib-flow-audit-redaction-profile)

(defun delib-flow--sanitize-audit-text (text)
  "Return deterministic audit-safe text derived from TEXT."
  (let ((sanitized (delib-flow--sanitize-basic-cloud-text text)))
    (when (eq (delib-flow--audit-redaction-profile) 'strict)
      (setq sanitized
            (replace-regexp-in-string
             "\\[\\[file:[^]]+\\]\\[[^]]*\\]\\]" "[redacted-link]" sanitized t t))
      (setq sanitized
            (replace-regexp-in-string
             "/[^][ \n\t)]+" "[redacted-path]" sanitized t t)))
    sanitized))

(defun delib-flow--redact-audit-value (value)
  "Return VALUE rewritten for redacted audit persistence."
  (cond
   ((stringp value)
    (delib-flow--sanitize-audit-text value))
   ((consp value)
    (cons (delib-flow--redact-audit-value (car value))
          (delib-flow--redact-audit-value (cdr value))))
   ((vectorp value)
    (apply #'vector
           (mapcar #'delib-flow--redact-audit-value value)))
   (t
    value)))

(defun delib-flow--metadata-only-audit-value (label)
  "Return metadata-only audit placeholder for LABEL."
  (list :omitted t
        :reason (format "%s omitted by metadata-only audit policy." label)))

(defun delib-flow--audit-persisted-value-handler (policy)
  "Return handler function for audit payload POLICY."
  (alist-get policy
             '((full . identity)
               (redacted . delib-flow--redact-audit-value)
               (metadata-only . delib-flow--metadata-only-audit-value))))

(defun delib-flow--audit-persisted-value (label value)
  "Return policy-filtered audit VALUE for LABEL."
  (let ((handler
         (delib-flow--audit-persisted-value-handler
          (delib-flow--audit-payload-policy))))
    (if (eq handler #'delib-flow--metadata-only-audit-value)
        (funcall handler label)
      (funcall (or handler #'identity) value))))

(defun delib-flow--make-audit-stage-record (entry)
  "Return audit stage record derived from stage ENTRY."
  (list :stage-id (plist-get entry :stage-id)
        :label (plist-get entry :label)
        :status (plist-get entry :status)
        :review-state (plist-get entry :review-state)
        :attempt-number (plist-get entry :attempt-number)
        :prompt-id (plist-get entry :prompt-id)
        :provider (delib-flow--audit-provider entry)
        :transport-stage-id (delib-flow--audit-transport-stage-id entry)
        :model-name (delib-flow--audit-model-name entry)
        :payload-policy (delib-flow--audit-payload-policy)
        :redaction-profile (delib-flow--audit-redaction-profile)
        :started-at (plist-get entry :started-at)
        :ended-at (plist-get entry :ended-at)
        :input-package
        (delib-flow--audit-persisted-value "Input package"
                                           (plist-get entry :input-package))
        :raw-output
        (delib-flow--audit-persisted-value "Raw output"
                                           (plist-get entry :raw-output))
        :normalized-output
        (delib-flow--audit-persisted-value "Normalized result"
                                           (plist-get entry :normalized-output))))

(defun delib-flow--audit-log-configured-p ()
  "Return non-nil when audit logging is configured."
  (and delib-flow-audit-log-file
       (not (string-empty-p delib-flow-audit-log-file))))

(defun delib-flow--audit-archive-configured-p ()
  "Return non-nil when audit archive saving is configured."
  (and delib-flow-audit-archive-directory
       (not (string-empty-p delib-flow-audit-archive-directory))))

(defun delib-flow--audit-properties-text (pairs)
  "Return Org property drawer text for PAIRS."
  (concat
   ":PROPERTIES:\n"
   (mapconcat
    (lambda (pair)
      (format ":%s: %s"
              (car pair)
              (or (cdr pair) "")))
    pairs
    "\n")
   "\n:END:\n"))

(defun delib-flow--audit-data-block (heading data)
  "Return Org block for HEADING and DATA."
  (format "*** %s\n#+begin_example\n%s#+end_example\n"
          heading
          (pp-to-string data)))

(defun delib-flow--audit-stage-text (record)
  "Return Org subtree text for audit stage RECORD."
  (concat
   (format "** %s (attempt %s)\n"
           (plist-get record :label)
           (plist-get record :attempt-number))
   (delib-flow--audit-properties-text
    `(("STAGE_ID" . ,(symbol-name (plist-get record :stage-id)))
      ("ATTEMPT_NUMBER" . ,(number-to-string
                            (plist-get record :attempt-number)))
      ("STATUS" . ,(symbol-name (plist-get record :status)))
      ("REVIEW_STATE" . ,(symbol-name (plist-get record :review-state)))
      ("PROMPT_ID" . ,(symbol-name (plist-get record :prompt-id)))
      ("MODEL_PROVIDER" . ,(plist-get record :provider))
      ("TRANSPORT_STAGE" . ,(if-let ((stage-id
                                      (plist-get record :transport-stage-id)))
                                (symbol-name stage-id)
                              ""))
      ("MODEL_NAME" . ,(plist-get record :model-name))
      ("PAYLOAD_POLICY" . ,(symbol-name (plist-get record :payload-policy)))
      ("REDACTION_PROFILE" . ,(symbol-name (plist-get record :redaction-profile)))
      ("STARTED_AT" . ,(delib-flow--time-string
                        (plist-get record :started-at)))
      ("ENDED_AT" . ,(delib-flow--time-string
                      (plist-get record :ended-at)))))
   (delib-flow--audit-data-block "Normalized result"
                                 (plist-get record :normalized-output))
   (delib-flow--audit-data-block "Input package"
                                 (plist-get record :input-package))
   (delib-flow--audit-data-block "Raw output"
                                 (plist-get record :raw-output))))

(defun delib-flow--audit-run-heading (record)
  "Return Org heading text for audit run RECORD."
  (format "* %s delib-flow run\n"
          (delib-flow--time-string (plist-get record :started-at))))

(defun delib-flow--audit-run-snapshot-text (run-record)
  "Return compact root snapshot text for audit RUN-RECORD."
  (format
   "** Run snapshot\n- Current decision: %s\n- Operator intent: %s\n"
   (or (plist-get run-record :current-decision) "none")
   (if (delib-flow--non-empty-string-p
        (plist-get run-record :operator-intent))
       (plist-get run-record :operator-intent)
     "none")))

(defun delib-flow--audit-run-text (audit)
  "Return Org subtree text for AUDIT state."
  (let ((run-record (plist-get audit :run-record))
        (stage-records (plist-get audit :stage-records)))
    (concat
     (delib-flow--audit-run-heading run-record)
     (delib-flow--audit-properties-text
      `(("RUN_ID" . ,(plist-get run-record :run-id))
        ("RUN_STATUS" . ,(symbol-name (plist-get run-record :run-status)))
        ("SOURCE_TITLE" . ,(plist-get run-record :source-title))
        ("SOURCE_FILE" . ,(or (plist-get run-record :source-file) ""))
        ("PAYLOAD_POLICY" . ,(symbol-name (delib-flow--audit-payload-policy)))
        ("REDACTION_PROFILE" . ,(symbol-name (delib-flow--audit-redaction-profile)))
        ("STARTED_AT" . ,(delib-flow--time-string
                          (plist-get run-record :started-at)))
        ("ENDED_AT" . ,(if-let ((ended-at (plist-get run-record :ended-at)))
                           (delib-flow--time-string ended-at)
                         ""))))
     (delib-flow--audit-run-snapshot-text run-record)
     (mapconcat #'delib-flow--audit-stage-text stage-records "\n"))))

(defun delib-flow--audit-run-bounds (run-id)
  "Return bounds of audit run subtree matching RUN-ID in current buffer."
  (goto-char (point-min))
  (when (re-search-forward (format "^:RUN_ID: %s$" (regexp-quote run-id)) nil t)
    (save-excursion
      (org-back-to-heading t)
      (let ((begin (point))
            (end (progn (org-end-of-subtree t t) (point))))
        (cons begin end)))))

(defun delib-flow--audit-stage-bounds (run-id stage-id attempt-number)
  "Return bounds of audit STAGE-ID ATTEMPT-NUMBER within RUN-ID."
  (when-let ((run-bounds (delib-flow--audit-run-bounds run-id)))
    (let ((stage-pattern
           (format "^:STAGE_ID: %s$" (regexp-quote (symbol-name stage-id))))
          (attempt-pattern
           (format "^:ATTEMPT_NUMBER: %d$" attempt-number))
          (limit (cdr run-bounds))
          result)
      (goto-char (car run-bounds))
      (while (and (not result)
                  (re-search-forward stage-pattern limit t))
        (org-back-to-heading t)
        (let ((begin (point))
              (end (progn (org-end-of-subtree t t) (point))))
          (goto-char begin)
          (when (and (re-search-forward stage-pattern end t)
                     (re-search-forward attempt-pattern end t))
            (setq result (cons begin end)))
          (goto-char end)))
      result)))

(defun delib-flow--archive-audit-file-name (run-record)
  "Return archive file name for audit RUN-RECORD."
  (let* ((started-at (plist-get run-record :started-at))
         (timestamp (format-time-string "%Y%m%dT%H%M%S" started-at))
         (source-title (or (plist-get run-record :source-title) "untitled-source"))
         (slug (or (delib-flow--slugify source-title) "untitled-source"))
         (run-id (plist-get run-record :run-id)))
    (format "%s--%s--%s.org" timestamp slug run-id)))

(defun delib-flow--archive-audit-file-path (run-record)
  "Return archive file path for audit RUN-RECORD."
  (expand-file-name
   (delib-flow--archive-audit-file-name run-record)
   delib-flow-audit-archive-directory))

(defun delib-flow--write-audit-log-file (audit)
  "Persist AUDIT state into `delib-flow-audit-log-file'."
  (let ((target delib-flow-audit-log-file)
        (run-id (plist-get (plist-get audit :run-record) :run-id)))
    (with-temp-buffer
      (when (file-exists-p target)
        (insert-file-contents target))
      (org-mode)
      (when-let ((bounds (delib-flow--audit-run-bounds run-id)))
        (delete-region (car bounds) (cdr bounds)))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (delib-flow--audit-run-text audit))
      (write-region (point-min) (point-max) target nil 'silent))))

(defun delib-flow--updated-run-record (run)
  "Return refreshed audit run record from RUN."
  (let* ((audit (plist-get run :audit))
         (run-record (plist-get audit :run-record))
         (session (delib-flow--run-session run)))
    (setq run-record
          (plist-put run-record :run-status (delib-flow--audit-run-status run)))
    (setq run-record
          (plist-put run-record :ended-at (plist-get session :ended-at)))
    (setq run-record
          (plist-put run-record :current-decision
                     (plist-get session :current-decision)))
    (setq run-record
          (plist-put run-record :operator-intent
                     (delib-flow--operator-intent-text run)))
    run-record))

(defun delib-flow--updated-audit-state-for-save (run)
  "Return RUN with in-memory audit state refreshed for archive save.

This updates the audit record from current run state without writing to the
rolling audit log."
  (let* ((audit (plist-get run :audit))
         (updated-audit
          (plist-put
           (plist-put audit :run-record (delib-flow--updated-run-record run))
           :stage-records
           (delib-flow--audit-stage-records-from-run run))))
    (plist-put run :audit updated-audit)))

(defun delib-flow--audit-stage-records-from-run (run)
  "Return audit stage records regenerated from RUN stage history."
  (let ((entries (plist-get (delib-flow--run-stage-history run) :entries)))
    (mapcar (lambda (entry)
              (delib-flow--make-audit-stage-record
               (plist-put (copy-tree entry)
                          :attempt-number
                          (delib-flow--entry-attempt-number entries entry))))
            entries)))

(defun delib-flow--sync-audit-state (run checkpoint)
  "Return RUN with audit state synchronized from current run state at CHECKPOINT."
  (let* ((audit (plist-get run :audit))
         (updated-audit
          (plist-put
           (plist-put
            (plist-put audit :run-record (delib-flow--updated-run-record run))
            :stage-records
            (delib-flow--audit-stage-records-from-run run))
           :pending-checkpoints
           (list checkpoint))))
    (plist-put run :audit updated-audit)))

(defun delib-flow--append-audit-record (run entry)
  "Return RUN with audit state updated from stage ENTRY."
  (delib-flow--sync-audit-state run (plist-get entry :stage-id)))

(defun delib-flow--persist-audit-state (run checkpoint)
  "Return RUN after persisting audit CHECKPOINT when configured."
  (let ((audit (plist-get run :audit)))
    (if (delib-flow--audit-log-configured-p)
        (progn
          (delib-flow--write-audit-log-file audit)
          (plist-put
           run :audit
           (plist-put
            (plist-put audit :pending-checkpoints nil)
            :last-appended-checkpoint checkpoint)))
      run)))

(defun delib-flow--finalize-audit-update (run entry)
  "Return RUN after audit updates derived from stage ENTRY."
  (delib-flow--persist-audit-state
   (delib-flow--append-audit-record run entry)
   (plist-get entry :stage-id)))

(defun delib-flow--refresh-run-audit (run checkpoint)
  "Return RUN with refreshed run audit state at CHECKPOINT."
  (delib-flow--persist-audit-state
   (delib-flow--sync-audit-state run checkpoint)
   checkpoint))

(defun delib-flow--audit-latest-stage-record (run)
  "Return the latest persisted audit stage record for RUN."
  (car (last (plist-get (plist-get run :audit) :stage-records))))

(defun delib-flow--open-audit-file-buffer ()
  "Return the audit log buffer, or signal a user error."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--audit-log-configured-p)
    (user-error "Audit logging is not configured"))
  (unless (file-exists-p delib-flow-audit-log-file)
    (user-error "Audit log file does not exist yet"))
  (let ((buffer (find-file-noselect delib-flow-audit-log-file)))
    (with-current-buffer buffer
      (org-mode))
    buffer))

(defun delib-flow--display-audit-buffer-at (buffer position)
  "Display BUFFER at POSITION and return BUFFER."
  (let ((window (display-buffer buffer)))
    (with-current-buffer buffer
      (goto-char position))
    (when (window-live-p window)
      (set-window-point window position)
      (with-selected-window window
        (recenter 1)))
    buffer))

(defun delib-flow--active-run-id ()
  "Return the active run id."
  (plist-get (plist-get (plist-get delib-flow--active-run :audit) :run-record)
             :run-id))

(defun delib-flow--audit-navigation-availability ()
  "Return availability text for audit navigation commands."
  (if (and (delib-flow--audit-log-configured-p)
           delib-flow--active-run
           (file-exists-p delib-flow-audit-log-file))
      "available"
    "not available"))

(defun delib-flow--audit-archive-directory-display ()
  "Return user-facing archive directory display text."
  (if (delib-flow--audit-archive-configured-p)
      delib-flow-audit-archive-directory
    "unconfigured"))

(defun delib-flow--audit-archive-save-availability ()
  "Return availability text for saving an archived audit run."
  (if (and delib-flow--active-run
           (delib-flow--audit-archive-configured-p))
      "available"
    "not available"))

(defun delib-flow-open-audit-run ()
  "Open the persisted audit log at the active run subtree."
  (interactive)
  (let* ((run-id (delib-flow--active-run-id))
         (buffer (delib-flow--open-audit-file-buffer)))
    (with-current-buffer buffer
      (if-let ((bounds (delib-flow--audit-run-bounds run-id)))
          (delib-flow--display-audit-buffer-at buffer (car bounds))
        (user-error "Active run audit subtree is not present in the audit log")))))

(defun delib-flow-open-audit-latest-stage ()
  "Open the persisted audit log at the latest audit stage for the active run."
  (interactive)
  (let* ((run-id (delib-flow--active-run-id))
         (latest-record (delib-flow--audit-latest-stage-record delib-flow--active-run)))
    (unless latest-record
      (user-error "No audit stage records are available yet"))
    (let ((buffer (delib-flow--open-audit-file-buffer)))
      (with-current-buffer buffer
        (if-let ((bounds
                  (delib-flow--audit-stage-bounds
                   run-id
                   (plist-get latest-record :stage-id)
                   (plist-get latest-record :attempt-number))))
            (delib-flow--display-audit-buffer-at buffer (car bounds))
          (user-error "Latest audit stage subtree is not present in the audit log"))))))

(defun delib-flow-save-active-audit-run ()
  "Archive the active run as a standalone audit Org file."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--audit-archive-configured-p)
    (user-error "Audit archive directory is not configured"))
  (let* ((updated-run
          (delib-flow--updated-audit-state-for-save
           (delib-flow--sync-run-from-control-buffer delib-flow--active-run)))
         (audit (plist-get updated-run :audit))
         (run-record (plist-get audit :run-record))
         (target-dir delib-flow-audit-archive-directory)
         (target-file (delib-flow--archive-audit-file-path run-record)))
    (make-directory target-dir t)
    (with-temp-file target-file
      (org-mode)
      (insert (delib-flow--audit-run-text audit)))
    (setq delib-flow--active-run updated-run)
    (message "Archived active audit run to %s" target-file)))

(provide 'delib-flow-audit)

;;; delib-flow-audit.el ends here
