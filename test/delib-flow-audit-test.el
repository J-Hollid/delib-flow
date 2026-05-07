;;; delib-flow-audit-test.el --- Audit tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-test-support)

(ert-deftest delib-flow-stage-execution-updates-audit-state-and-file ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (updated-run (delib-flow--run-stage-locally run 'inspect-source))
           (audit (plist-get updated-run :audit))
           (stage-records (plist-get audit :stage-records)))
      (should (equal 1 (length stage-records)))
      (should (equal 'inspect-source
                     (plist-get (car stage-records) :stage-id)))
      (should (equal 'inspect-source
                     (plist-get audit :last-appended-checkpoint)))
      (should-not (plist-get audit :pending-checkpoints))
      (with-temp-buffer
        (insert-file-contents delib-flow-audit-log-file)
        (should (search-forward ":RUN_ID:" nil t))
        (should (search-forward "** Inspect Source (attempt 1)" nil t))
        (should (search-forward ":ATTEMPT_NUMBER: 1" nil t))
        (should (search-forward ":PAYLOAD_POLICY: full" nil t))
        (should (search-forward "*** Input package" nil t))
        (should (search-forward "*** Raw output" nil t))))))

(ert-deftest delib-flow-audit-model-name-falls-back-to-local-routing-model ()
  (let ((entry (list :stage-id 'inspect-source
                     :provider 'local
                     :raw-output nil
                     :input-package
                     (list :routing
                           (list :default-local-model "llama3"
                                 :default-cloud-model "gpt-5")))))
    (should (equal "llama3" (delib-flow--audit-model-name entry)))))

(ert-deftest delib-flow-stage-execution-redacts-audit-payloads ()
  (delib-flow-test--with-temp-audit-file
    (let* ((delib-flow-audit-payload-policy 'redacted)
           (delib-flow-audit-redaction-profile 'strict)
           (run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nContact alice@example.com\nVisit https://example.com\n[[file:notes.org][Notes]]\n")))
           (updated-run (delib-flow--run-stage-locally run 'inspect-source))
           (stage-record (car (plist-get (plist-get updated-run :audit)
                                         :stage-records))))
      (should (eq 'redacted (plist-get stage-record :payload-policy)))
      (should (string-match-p "\\[redacted-email\\]"
                              (pp-to-string (plist-get stage-record :input-package))))
      (should (string-match-p "\\[redacted-url\\]"
                              (pp-to-string (plist-get stage-record :input-package))))
      (with-temp-buffer
        (insert-file-contents delib-flow-audit-log-file)
        (should (search-forward ":PAYLOAD_POLICY: redacted" nil t))
        (should (search-forward ":REDACTION_PROFILE: strict" nil t))
        (should (search-forward "[redacted-email]" nil t))
        (should (search-forward "[redacted-url]" nil t))))))

(ert-deftest delib-flow-stage-execution-omits-audit-payloads-under-metadata-policy ()
  (delib-flow-test--with-temp-audit-file
    (let* ((delib-flow-audit-payload-policy 'metadata-only)
           (run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (updated-run (delib-flow--run-stage-locally run 'inspect-source))
           (stage-record (car (plist-get (plist-get updated-run :audit)
                                         :stage-records))))
      (should (equal t (plist-get (plist-get stage-record :input-package) :omitted)))
      (should (equal t (plist-get (plist-get stage-record :raw-output) :omitted)))
      (with-temp-buffer
        (insert-file-contents delib-flow-audit-log-file)
        (should (search-forward ":PAYLOAD_POLICY: metadata-only" nil t))
        (should (search-forward "Input package omitted by metadata-only audit policy."
                                nil t))
        (should (search-forward "Raw output omitted by metadata-only audit policy."
                                nil t))))))

(ert-deftest delib-flow-review-outcome-refreshes-audit-stage-state ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (accepted
            (delib-flow--refresh-run-audit
             (delib-flow--apply-inspect-review-outcome
              inspected
              'accepted
              "Inspect result accepted. You may now match the project or retry inspect.")
             'inspect-source))
           (audit (plist-get accepted :audit))
           (stage-record (car (plist-get audit :stage-records))))
      (should (eq 'accepted (plist-get stage-record :review-state)))
      (should (eq 'inspect-source
                  (plist-get audit :last-appended-checkpoint)))
      (with-temp-buffer
        (insert-file-contents delib-flow-audit-log-file)
        (should (search-forward ":REVIEW_STATE: accepted" nil t))))))

(ert-deftest delib-flow-audit-status-rerenders-after-stage-execution ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (delib-flow--active-run run)
           (buffer (delib-flow--render-control-buffer run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-inspect-source)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "*** Audit status" nil t))
              (should (search-forward "Audit log file: " nil t))
              (should (search-forward "Audit payload policy: full" nil t))
              (should (search-forward "Audit redaction profile: strict" nil t))
              (should (search-forward "Recorded stages: 1" nil t))
              (should (search-forward "Last appended checkpoint: inspect-source"
                                      nil t))
              (should (search-forward "Latest recorded stage: inspect-source"
                                      nil t))
              (should (search-forward "** Audit navigation" nil t))
              (should (search-forward "Jump to active run audit: available"
                                      nil t))
              (should (search-forward "Jump to latest audit stage: available"
                                      nil t))
              (should (search-forward "Latest audit attempt: Inspect Source attempt 1"
                                      nil t))
              (should (search-forward "Latest audit provider: local"
                                      nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-retry-refreshes-audit-with-superseded-prior-stage ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (accepted
            (delib-flow-test--accept-inspect
             (delib-flow--refresh-run-audit
              (delib-flow--run-stage-locally run 'inspect-source)
              'inspect-source)))
           (retried (delib-flow--run-stage-locally accepted 'inspect-source))
           (stage-records (plist-get (plist-get retried :audit) :stage-records)))
      (should (= 2 (length stage-records)))
      (should (eq 'superseded
                  (plist-get (nth 0 stage-records) :review-state)))
      (should (eq 'pending-review
                  (plist-get (nth 1 stage-records) :review-state)))
      (with-temp-buffer
        (insert-file-contents delib-flow-audit-log-file)
        (goto-char (point-min))
        (should (search-forward "** Inspect Source (attempt 1)" nil t))
        (goto-char (point-min))
        (should (search-forward "** Inspect Source (attempt 2)" nil t))
        (goto-char (point-min))
        (should (search-forward ":REVIEW_STATE: superseded" nil t))
        (goto-char (point-min))
        (should (search-forward ":REVIEW_STATE: pending-review" nil t))))))

(ert-deftest delib-flow-rerouted-cloud-audit-records-transport-stage ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alice Example"
                       :content "* Alice Example\nContact alice@example.com\nAction items:\n- Draft kickoff follow-up\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (cloud-decided
            (delib-flow--run-stage-locally
             (delib-flow-test--set-cloud-target-stage inspected 'extract-actions)
             'decide-cloud-pass))
           (sanitized
            (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
           (approved
            (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
           (_updated-run (delib-flow--run-stage-in-cloud approved 'run-cloud-stage)))
      (with-temp-buffer
        (insert-file-contents delib-flow-audit-log-file)
        (should (search-forward "** Extract Actions (attempt 1)" nil t))
        (should (search-forward ":TRANSPORT_STAGE: run-cloud-stage" nil t))))))

(ert-deftest delib-flow-file-approved-outputs-audit-captures-target-locations ()
  (delib-flow-test--with-temp-audit-file
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'extract-actions))
             (integrated
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (selected
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-selection integrated "1")
               'select-approved-filing-actions))
             (_updated-run
              (delib-flow--run-stage-locally selected 'file-approved-outputs)))
        (with-temp-buffer
          (insert-file-contents delib-flow-audit-log-file)
          (should (search-forward "** File Approved Outputs" nil t))
          (should (search-forward "Alpha Project" nil t))
          (should (search-forward "Write follow-up note for Alpha Project kickoff"
                                  nil t)))))))

(ert-deftest delib-flow-open-audit-run-jumps-to-active-run-subtree ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (delib-flow--active-run
            (delib-flow--run-stage-locally run 'inspect-source))
           (buffer (delib-flow-open-audit-run)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal delib-flow-audit-log-file buffer-file-name))
            (should (looking-at-p "^\\* "))
            (should (search-forward ":RUN_ID:" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-open-audit-run-errors-when-run-subtree-is-missing ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (delib-flow--active-run run))
      (should-error (delib-flow-open-audit-run) :type 'user-error))))

(ert-deftest delib-flow-open-audit-latest-stage-jumps-to-latest-stage-subtree ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nAction items:\n- Draft follow-up\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run
            (delib-flow--run-stage-locally inspected 'extract-actions))
           (buffer (delib-flow-open-audit-latest-stage)))
      (unwind-protect
          (with-current-buffer buffer
            (should (equal delib-flow-audit-log-file buffer-file-name))
            (should (looking-at-p "^\\*\\* "))
            (should (search-forward ":STAGE_ID: extract-actions" nil t))
            (should (search-forward ":ATTEMPT_NUMBER: 1" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-open-audit-latest-stage-errors-without-records ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (delib-flow--active-run run))
      (should-error (delib-flow-open-audit-latest-stage) :type 'user-error))))

(ert-deftest delib-flow-open-audit-latest-stage-errors-when-stage-subtree-is-missing ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run inspected))
      (with-temp-file delib-flow-audit-log-file
        (insert "* Empty audit\n"))
      (should-error (delib-flow-open-audit-latest-stage) :type 'user-error))))

(ert-deftest delib-flow-save-active-audit-run-writes-standalone-run-file ()
  (delib-flow-test--with-temp-directory-var archive-dir "delib-flow-audit-archive"
    (let* ((run (delib-flow--run-stage-locally
                 (delib-flow--initialize-run
                  (list :title "Example Source"
                        :content "* Example Source\nBody line\n"))
                 'inspect-source))
           (delib-flow-audit-log-file nil)
           (delib-flow-audit-archive-directory archive-dir)
           (delib-flow--active-run run))
      (delib-flow-save-active-audit-run)
      (let ((files (directory-files archive-dir t "\\.org\\'")))
        (should (= 1 (length files)))
        (should (string-match-p
                 "example-source--delib-flow-.*\\.org\\'"
                 (file-name-nondirectory (car files))))
        (with-temp-buffer
          (insert-file-contents (car files))
          (goto-char (point-min))
          (should (looking-at-p "^\\* "))
          (should (search-forward ":RUN_ID:" nil t))
          (should (search-forward ":SOURCE_TITLE: Example Source" nil t))
          (should (search-forward "** Inspect Source (attempt 1)" nil t))
          (should-not (search-forward "** Match Project" nil t)))))))

(ert-deftest delib-flow-save-active-audit-run-overwrites-same-run-file ()
  (delib-flow-test--with-temp-directory-var archive-dir "delib-flow-audit-archive"
    (let* ((run (delib-flow-test--accept-inspect
                 (delib-flow--run-stage-locally
                  (delib-flow--initialize-run
                   (list :title "Overwrite Source"
                         :content "* Overwrite Source\nBody line\n"))
                  'inspect-source)))
           (delib-flow-audit-log-file nil)
           (delib-flow-audit-archive-directory archive-dir)
           (delib-flow--active-run run))
      (delib-flow-save-active-audit-run)
      (let ((first-files (directory-files archive-dir t "\\.org\\'")))
        (should (= 1 (length first-files)))
        (with-temp-file (car first-files)
          (insert "stale"))
        (delib-flow-save-active-audit-run)
        (let ((second-files (directory-files archive-dir t "\\.org\\'")))
          (should (= 1 (length second-files)))
          (should (equal (car first-files) (car second-files)))
          (with-temp-buffer
            (insert-file-contents (car second-files))
            (goto-char (point-min))
            (should (search-forward ":RUN_ID:" nil t))
            (should-not (search-forward "stale" nil t))))))))

(ert-deftest delib-flow-save-active-audit-run-creates-missing-directory ()
  (let* ((archive-dir (expand-file-name
                       "missing-archive"
                       (make-temp-file "delib-flow-audit-parent" t)))
         (run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow-audit-log-file nil)
         (delib-flow-audit-archive-directory archive-dir)
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (should-not (file-directory-p archive-dir))
          (delib-flow-save-active-audit-run)
          (should (file-directory-p archive-dir))
          (should (= 1 (length (directory-files archive-dir t "\\.org\\'")))))
      (when (file-directory-p (file-name-directory archive-dir))
        (delete-directory (file-name-directory archive-dir) t)))))

(ert-deftest delib-flow-save-active-audit-run-errors-without-active-run ()
  (let ((delib-flow--active-run nil)
        (delib-flow-audit-archive-directory "/tmp/archive"))
    (should-error (delib-flow-save-active-audit-run) :type 'user-error)))

(ert-deftest delib-flow-save-active-audit-run-errors-without-archive-directory ()
  (let ((delib-flow--active-run
         (delib-flow--initialize-run
          (list :title "Example"
                :content "* Example\nBody line\n")))
        (delib-flow-audit-archive-directory nil))
    (should-error (delib-flow-save-active-audit-run) :type 'user-error)))

(ert-deftest delib-flow-open-audit-run-displays-active-run-bounds ()
  (let* ((buffer (get-buffer-create "*delib-flow-audit-test*"))
         (delib-flow--active-run
          (list :audit (list :run-record (list :run-id "run-123"))))
         displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'delib-flow--open-audit-file-buffer)
                   (lambda () buffer))
                  ((symbol-function 'delib-flow--audit-run-bounds)
                   (lambda (run-id)
                     (should (equal run-id "run-123"))
                     '(42 . 99)))
                  ((symbol-function 'delib-flow--display-audit-buffer-at)
                   (lambda (buf position)
                     (setq displayed (list buf position))
                     buf)))
          (should (eq buffer (delib-flow-open-audit-run)))
          (should (equal displayed (list buffer 42))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-open-audit-latest-stage-displays-stage-bounds ()
  (let* ((buffer (get-buffer-create "*delib-flow-audit-stage-test*"))
         (latest-record (list :stage-id 'suggest-reference-notes
                              :attempt-number 3))
         (delib-flow--active-run
          (list :audit (list :run-record (list :run-id "run-456")
                             :stage-records (list latest-record))))
         displayed)
    (unwind-protect
        (cl-letf (((symbol-function 'delib-flow--open-audit-file-buffer)
                   (lambda () buffer))
                  ((symbol-function 'delib-flow--audit-latest-stage-record)
                   (lambda (_) latest-record))
                  ((symbol-function 'delib-flow--audit-stage-bounds)
                   (lambda (run-id stage-id attempt-number)
                     (should (equal run-id "run-456"))
                     (should (eq stage-id 'suggest-reference-notes))
                     (should (= attempt-number 3))
                     '(84 . 126)))
                  ((symbol-function 'delib-flow--display-audit-buffer-at)
                   (lambda (buf position)
                     (setq displayed (list buf position))
                     buf)))
          (should (eq buffer (delib-flow-open-audit-latest-stage)))
          (should (equal displayed (list buffer 84))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'delib-flow-audit-test)
