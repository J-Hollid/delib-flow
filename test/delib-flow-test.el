;;; delib-flow-test.el --- Tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'delib-flow)

(defmacro delib-flow-test--with-temp-org (&rest body)
  "Run BODY in a temporary Org buffer."
  `(with-temp-buffer
     (org-mode)
     ,@body))

(defmacro delib-flow-test--with-temp-project-file (content &rest body)
  "Run BODY with a temporary My Projects file containing CONTENT."
  (declare (indent 1))
  `(let ((file (make-temp-file "delib-flow-projects" nil ".org" ,content)))
     (unwind-protect
         (let ((delib-flow-my-projects-file file))
           ,@body)
       (when (file-exists-p file)
         (delete-file file)))))

(defmacro delib-flow-test--with-temp-zk-root (files &rest body)
  "Run BODY with a temporary ZK root populated from FILES.

FILES is an alist of relative path to file content."
  (declare (indent 1))
  `(let ((root (make-temp-file "delib-flow-zk" t)))
     (unwind-protect
         (progn
           (dolist (entry ,files)
             (let* ((relative (car entry))
                    (content (cdr entry))
                    (target (expand-file-name relative root))
                    (dir (file-name-directory target)))
               (make-directory dir t)
               (with-temp-file target
                 (insert content))))
           (let ((delib-flow-zk-root root))
             ,@body))
       (when (file-directory-p root)
         (delete-directory root t)))))

(defmacro delib-flow-test--with-temp-audit-file (&rest body)
  "Run BODY with a temporary audit log file."
  (declare (indent 0))
  `(let ((file (make-temp-file "delib-flow-audit" nil ".org")))
     (unwind-protect
         (let ((delib-flow-audit-log-file file))
           ,@body)
       (when (file-exists-p file)
         (delete-file file)))))

(defun delib-flow-test--accept-inspect (run)
  "Return RUN with inspect-source accepted and actions reseeded."
  (delib-flow--seed-actions
   (delib-flow--apply-inspect-review-outcome
    run
    'accepted
    "Inspect result accepted. You may now match the project or retry inspect.")))

(defun delib-flow-test--accept-match (run)
  "Return RUN with match-project accepted and actions reseeded."
  (delib-flow--seed-actions
   (delib-flow--apply-match-review-outcome
    run
    'accepted
    "Project match accepted. Continue with manual override or downstream stages as appropriate.")))

(defun delib-flow-test--set-manual-project-selection (run selection &optional notes)
  "Return RUN with manual project-selection block set to SELECTION and NOTES."
  (let* ((block (delib-flow--editable-block run 'manual-project-selection))
         (text (format "Selection: %s\nNotes:\n%s\n"
                       selection
                       (or notes ""))))
    (delib-flow--set-editable-block
     run
     'manual-project-selection
     (delib-flow--set-editable-block-text block text))))

(ert-deftest delib-flow-snapshot-heading-captures-title-and-content ()
  (delib-flow-test--with-temp-org
   (insert "* Example heading\nSome body text.\n")
   (goto-char (point-min))
   (let ((snapshot (delib-flow--snapshot-heading)))
     (should (equal "Example heading" (plist-get snapshot :title)))
     (should (string-match-p "Some body text" (plist-get snapshot :content))))))

(ert-deftest delib-flow-execute-inspect-source-classifies-email-sources ()
  (let* ((source (list :title "Re: Alpha Project update"
                       :outline-path '("Inbox")
                       :content "* Re: Alpha Project update\nFrom: Alice Example <alice@example.com>\nTo: Bob Example <bob@example.com>\nSubject: Re: Alpha Project update\nDate: 2026-04-27\n\nQuick status update.\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'email (plist-get raw-output :source-type)))
    (should (equal 2 (plist-get raw-output :contact-email-count)))
    (should (string-match-p "email-style headers"
                            (plist-get raw-output :source-type-reason)))))

(ert-deftest delib-flow-execute-inspect-source-classifies-meeting-note-sources ()
  (let* ((source (list :title "Team sync"
                       :outline-path '("Journal" "2026" "Week 17")
                       :content "* Team sync\nAttendees: Alice, Bob\nAgenda:\n- Review blockers\nNotes:\n- Need follow-up\nNext steps:\n- Send update\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'meeting-note (plist-get raw-output :source-type)))
    (should (= 7 (plist-get raw-output :body-line-count)))
    (should (member "journal outline path"
                    (plist-get raw-output :source-type-signals)))))

(ert-deftest delib-flow-execute-inspect-source-keeps-unknown-when-evidence-is-weak ()
  (let* ((source (list :title "Random note"
                       :outline-path '("Inbox")
                       :content "* Random note\nNeed to think about this later.\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'unknown (plist-get raw-output :source-type)))
    (should (string-match-p "too weak"
                            (plist-get raw-output :source-type-reason)))))

(ert-deftest delib-flow-project-candidates-ignore-state-bucket-headings ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project\nContact alice@example.com\n* Complete\n** Old Project\n* Waiting\n** Beta Project\n"
    (let ((titles
           (mapcar (lambda (candidate)
                     (plist-get candidate :title))
                   (delib-flow--project-candidates-from-file
                    delib-flow-my-projects-file))))
      (should (equal '("Alpha Project" "Old Project" "Beta Project")
                     titles)))))

(ert-deftest delib-flow-make-editable-block-returns-expected-shape ()
  (let ((block (delib-flow--make-editable-block
                'context-main
                'context
                "Current context"
                '("Current context" "Editable working slice")
                "delib-edit-context-main")))
    (should (equal 'context-main (plist-get block :id)))
    (should (equal 'context (plist-get block :kind)))
    (should (equal "Current context" (plist-get block :section)))
    (should (equal 'clean (plist-get block :status)))
    (should (equal 'valid (plist-get block :validation-status)))))

(ert-deftest delib-flow-initial-section-anchors-contains-required-sections ()
  (let ((anchors (delib-flow--initial-section-anchors)))
    (dolist (section '(now
                       next-actions
                       current-result
                       current-context
                       filing-preview
                       details))
      (should (assoc section anchors)))))

(ert-deftest delib-flow-initialize-run-contains-required-top-level-keys ()
  (let* ((snapshot (list :title "Example"))
         (run (delib-flow--initialize-run snapshot)))
    (dolist (key '(:source
                   :working-context
                   :stage-history
                   :actions
                   :routing
                   :filing
                   :audit
                   :session
                   :ui))
      (should (plist-member run key)))))

(ert-deftest delib-flow-initialize-run-stores-source-in-source-domain ()
  (let* ((snapshot (list :title "Example" :content "* Example"))
         (run (delib-flow--initialize-run snapshot)))
    (should (equal snapshot (delib-flow--run-source run)))
    (should (equal snapshot
                   (plist-get (delib-flow--run-working-context run)
                              :source-snapshot)))))

(ert-deftest delib-flow-initialize-run-seeds-editable-blocks ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (working (delib-flow--run-working-context run))
         (ui (delib-flow--run-ui run))
         (block-ids (plist-get working :editable-block-ids))
         (blocks (plist-get ui :editable-blocks)))
    (should (equal '(context-main operator-notes manual-project-selection inspect-source-review cloud-package-review) block-ids))
    (should (assoc 'context-main blocks))
    (should (assoc 'operator-notes blocks))
    (should (assoc 'manual-project-selection blocks))
    (should (assoc 'inspect-source-review blocks))
    (should (assoc 'cloud-package-review blocks))))

(ert-deftest delib-flow-initialize-run-seeds-review-results ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (working (delib-flow--run-working-context run))
         (inspect-review (delib-flow--review-record working 'inspect-source))
         (match-review (delib-flow--review-record working 'match-project)))
    (should inspect-review)
    (should match-review)
    (should (eq 'not-available
                (plist-get inspect-review :candidate-review-state)))
    (should-not (plist-get inspect-review :accepted-output))
    (should (eq 'not-available
                (plist-get match-review :candidate-review-state)))
    (should-not (plist-get match-review :accepted-output))))

(ert-deftest delib-flow-initialize-run-seeds-section-anchors ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (anchors (plist-get (delib-flow--run-ui run) :section-anchors)))
    (dolist (section '(now
                       next-actions
                       current-result
                       current-context
                       filing-preview
                       details))
      (should (assoc section anchors)))))

(ert-deftest delib-flow-initialize-run-seeds-session-state ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (session (delib-flow--run-session run)))
    (should (equal 'active (plist-get session :status)))
    (should (equal "Review working context and choose next action."
                   (plist-get session :current-decision)))))

(ert-deftest delib-flow-initialize-run-seeds-audit-run-record ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (audit (plist-get run :audit))
         (run-record (plist-get audit :run-record)))
    (should (plist-get run-record :run-id))
    (should (equal "Example" (plist-get run-record :source-title)))
    (should (eq 'active (plist-get run-record :run-status)))))

(ert-deftest delib-flow-initialize-run-seeds-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (actions (plist-get (delib-flow--run-actions run) :items))
         (action-ids (mapcar (lambda (action) (plist-get action :id)) actions)))
    (should (equal '(inspect-source refresh-buffer abort-run) action-ids))
    (should (equal 'available
                   (plist-get (car actions) :status)))))

(ert-deftest delib-flow-set-editable-block-text-marks-edited-status ()
  (let* ((block (delib-flow--make-editable-block
                 'context-main
                 'context
                 "Working context"
                 '("Working context" "Editable working slice")
                 "delib-edit-context-main"))
         (updated (delib-flow--set-editable-block-text block "Changed text")))
    (should (equal "Changed text" (plist-get updated :current-text)))
    (should (equal 'edited (plist-get updated :status)))))

(ert-deftest delib-flow-start-requires-heading-at-point ()
  (delib-flow-test--with-temp-org
   (insert "Not a heading\n")
   (goto-char (point-min))
   (should-error (delib-flow-start))))

(ert-deftest delib-flow-render-control-buffer-contains-required-sections ()
  (let* ((snapshot (list :title "Example"
                         :file "/tmp/example.org"
                         :id "abc"
                         :content "* Example\nBody"))
         (run (delib-flow--initialize-run snapshot))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'delib-flow-control-mode))
          (should (derived-mode-p 'org-mode))
          (should-not view-mode)
          (should-not buffer-read-only)
          (should (search-forward "* DeliberateFlow -- Example" nil t))
          (dolist (heading '("** Now"
                             "** Next actions"
                             "** Current result"
                             "** Current context"
                             "** Filing preview"
                             "** Details"))
            (goto-char (point-min))
            (should (search-forward heading nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-control-buffer-renders-structured-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "- Inspect Source [available]" nil t))
          (should (search-forward "Review the source snapshot and propose structured context." nil t))
          (goto-char (line-beginning-position))
          (should (eq 'inspect-source
                      (plist-get (get-text-property (point) 'delib-flow-action)
                                 :id)))
          (goto-char (point-min))
          (should (search-forward "- Refresh Buffer [available]" nil t))
          (goto-char (point-min))
          (should (search-forward "- Abort Run [available]" nil t))
          (goto-char (point-min))
          (should-not (search-forward "{id:" nil t))
          (goto-char (point-min))
          (should-not (search-forward "cmd:" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-control-mode-keybindings-include-public-commands ()
  (should (eq #'delib-flow-refresh
              (lookup-key delib-flow-control-mode-map (kbd "g"))))
  (should (eq #'delib-flow-dispatch-action
              (lookup-key delib-flow-control-mode-map (kbd "RET"))))
  (should (eq #'delib-flow-dispatch-action
              (lookup-key delib-flow-control-mode-map (kbd "a"))))
  (should (eq #'delib-flow-approve-current
              (lookup-key delib-flow-control-mode-map (kbd "A"))))
  (should (eq #'delib-flow-retry-current
              (lookup-key delib-flow-control-mode-map (kbd "r"))))
  (should (eq #'delib-flow-abort-run
              (lookup-key delib-flow-control-mode-map (kbd "q"))))
  (should (eq #'delib-flow-control-help
              (lookup-key delib-flow-control-mode-map (kbd "?")))))

(ert-deftest delib-flow-render-control-buffer-renders-detected-source-type ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Re: Alpha Project update"
                     :content "* Re: Alpha Project update\nFrom: alice@example.com\nSubject: Alpha Project\n\nStatus.\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-control-buffer inspected)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "*** Source snapshot" nil t))
          (should (search-forward "Source type: email" nil t))
          (should (search-forward "email" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-snapshot-heading-strips-text-properties ()
  (delib-flow-test--with-temp-org
   (insert (propertize "* Example heading\nBody line.\n" 'face 'bold))
   (goto-char (point-min))
   (let ((snapshot (delib-flow--snapshot-heading)))
     (should-not (text-properties-at 1 (plist-get snapshot :title)))
     (should-not (text-properties-at 1 (plist-get snapshot :content))))))

(ert-deftest delib-flow-render-control-buffer-renders-editable-blocks ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "** Editable working slice" nil t))
          (should (search-forward "#+begin_delib-edit context" nil t))
          (goto-char (point-min))
          (should (search-forward "** Operator notes" nil t))
          (should (search-forward "#+begin_delib-edit notes" nil t))
          (goto-char (point-min))
          (should (search-forward "**** Reviewed cloud package" nil t))
          (should (search-forward "#+begin_delib-edit cloud-review" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-control-buffer-protects-managed-regions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "** Now")
          (should-error (insert "forbidden")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-active-run-buffer-applies-cockpit-visibility ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-active-run-buffer run "Now")))
    (unwind-protect
        (with-current-buffer buffer
          (should (looking-at-p "\\*\\* Now"))
          (goto-char (point-min))
          (search-forward "*** Source snapshot")
          (should (outline-invisible-p (point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-control-buffer-allows-editable-block-body-edits ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "#+begin_delib-edit context")
          (forward-line 1)
          (insert "allowed")
          (goto-char (point-min))
          (should (search-forward "allowed" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-control-buffer-killed-clears-active-run ()
  (let ((delib-flow--active-run (list :session (list :status 'active))))
    (with-temp-buffer
      (setq-local delib-flow--active-run-buffer t)
      (delib-flow--control-buffer-killed))
    (should (null delib-flow--active-run))))

(ert-deftest delib-flow-refresh-buffer-errors-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-refresh-buffer))))

(ert-deftest delib-flow-abort-run-errors-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-abort-run))))

(ert-deftest delib-flow-refresh-buffer-rerenders-active-run ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t)
            (goto-char (point-min))
            (search-forward "#+begin_delib-edit context")
            (forward-line 1)
            (insert "Retained edit"))
          (delib-flow-refresh-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "- Refresh Buffer [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "Retained edit" nil t)))
          (should (equal "Retained edit"
                         (plist-get
                          (delib-flow--editable-block delib-flow--active-run
                                                      'context-main)
                          :current-text))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-refresh-buffer-preserves-current-section-anchor ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Filing preview")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-refresh-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Filing preview"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-refresh-buffer-does-not-duplicate-top-level-sections ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (dotimes (_ 5)
            (delib-flow-refresh-buffer))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (dolist (heading delib-flow--control-sections)
              (goto-char (point-min))
              (should (= 1
                         (how-many (format "^\\*\\* %s$" (regexp-quote heading))
                                   (point-min)
                                   (point-max)))))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-action-refresh-cycle-does-not-duplicate-top-level-sections ()
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
          (dotimes (_ 3)
            (delib-flow-refresh-buffer))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (dolist (heading delib-flow--control-sections)
              (goto-char (point-min))
              (should (= 1
                         (how-many (format "^\\*\\* %s$" (regexp-quote heading))
                                   (point-min)
                                   (point-max)))))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-sync-editable-blocks-updates-run-state ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "#+begin_delib-edit notes")
          (forward-line 1)
          (insert "Operator note")
          (let* ((updated-run (delib-flow--sync-editable-blocks run buffer))
                 (block (delib-flow--editable-block updated-run
                                                    'operator-notes)))
            (should (equal "Operator note" (plist-get block :current-text)))
            (should (equal 'edited (plist-get block :status)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-full-local-matched-project-workflow-completes ()
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
            (delib-flow--run-stage-locally integrated
                                           'select-approved-filing-actions))
           (filed-run
            (delib-flow--run-stage-locally selected 'file-approved-outputs))
           (history (delib-flow--run-stage-history filed-run))
           (stage-ids (mapcar (lambda (entry) (plist-get entry :stage-id))
                              (plist-get history :entries)))
           (locations (plist-get (plist-get filed-run :filing)
                                 :target-locations)))
      (should (equal '(inspect-source
                       match-project
                       extract-actions
                       integrate-into-source
                       select-approved-filing-actions
                       file-approved-outputs)
                     stage-ids))
      (should (equal 'file-approved-outputs
                     (plist-get history :latest-stage)))
      (should locations)
      (should (string-match-p "Review filed outputs"
                              (plist-get (delib-flow--run-session filed-run)
                                         :current-decision))))))

(ert-deftest delib-flow-full-cloud-assisted-workflow-completes ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nContact alice@example.com\nVisit https://example.com\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (cloud-decided
            (delib-flow--run-stage-locally drafted 'decide-cloud-pass))
           (sanitized
            (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
           (approved-send
            (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
           (cloud-run
            (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
           (approved-reintegration
            (delib-flow--run-stage-locally cloud-run
                                           'approve-candidate-reintegration))
           (integrated
            (delib-flow--run-stage-locally approved-reintegration
                                           'integrate-into-source))
           (selected
            (delib-flow--run-stage-locally integrated
                                           'select-approved-filing-actions))
           (filed-run
            (delib-flow--run-stage-locally selected 'file-approved-outputs))
           (history (delib-flow--run-stage-history filed-run))
           (stage-ids (mapcar (lambda (entry) (plist-get entry :stage-id))
                              (plist-get history :entries)))
           (working (delib-flow--run-working-context filed-run))
           (routing (plist-get filed-run :routing)))
      (should (equal '(inspect-source
                       match-project
                       extract-actions
                       decide-cloud-pass
                       sanitize-for-cloud
                       approve-cloud-send
                       run-cloud-stage
                       approve-candidate-reintegration
                       integrate-into-source
                       select-approved-filing-actions
                       file-approved-outputs)
                     stage-ids))
      (should (string-match-p "Cloud-reviewed context"
                              (plist-get working :retained-context)))
      (should (eq 'approved (plist-get routing :reintegration-status))))))

(ert-deftest delib-flow-refresh-buffer-detects-managed-region-conflicts ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (search-forward "** Details")
              (delete-region (line-beginning-position)
                             (line-end-position))))
          (delib-flow-refresh-buffer)
          (let* ((conflicts
                  (plist-get (delib-flow--run-ui delib-flow--active-run)
                             :managed-region-conflicts))
                 (actions (plist-get (delib-flow--run-actions delib-flow--active-run)
                                     :items))
                 (inspect-action
                  (seq-find (lambda (action)
                              (eq (plist-get action :id) 'inspect-source))
                            actions))
                 (refresh-action
                  (seq-find (lambda (action)
                              (eq (plist-get action :id) 'refresh-buffer))
                            actions)))
            (should conflicts)
            (should (equal 'blocked (plist-get inspect-action :status)))
            (should (equal 'available (plist-get refresh-action :status)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-inspect-source-updates-stage-history-and-context ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\nBody line two\n")))
         (updated-run (delib-flow--run-stage-locally run 'inspect-source))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (plist-get history :entries)))
         (working (delib-flow--run-working-context updated-run))
         (review (delib-flow--review-record working 'inspect-source)))
    (should (equal 'inspect-source (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'inspect-source (plist-get entry :stage-id)))
    (should (plist-get working :inspect-output))
    (should (equal 'pending-review
                   (plist-get review :candidate-review-state)))
    (should (equal 'inspect-source
                   (plist-get review :candidate-stage-id)))
    (should (equal (plist-get entry :raw-output)
                   (plist-get review :candidate-output)))
    (should-not (plist-get review :accepted-output))
    (should (string-match-p "Body lines: 2"
                            (plist-get working :retained-context)))
    (should (plist-get (plist-get working :inspect-output) :analysis))
    (should (plist-member (plist-get working :inspect-output) :content-word-count))
    (should (equal '(inspect-source
                     accept-inspect-source
                     reject-inspect-source
                     decide-cloud-pass
                     refresh-buffer
                     abort-run)
                   (mapcar (lambda (action)
                             (plist-get action :id))
                           (plist-get (delib-flow--run-actions updated-run)
                                      :items))))))

(ert-deftest delib-flow-inspect-source-command-rerenders-history ()
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
            (should (search-forward "**** Inspect Source" nil t))
            (should (search-forward "***** Attempt 1" nil t))
            (should (search-forward "Body lines: 1" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Inspect Source [available]" nil t))
            (should (search-forward "- Accept Inspect Result [available]" nil t))
            (should (search-forward "- Reject Inspect Result [available]" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Match Project [available]" nil t))
            (should-not (search-forward "- Discover Relevant Reference Material [available]" nil t))
            (should (search-forward "- Decide on Cloud Pass [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-stage-history-groups-attempts-by-stage ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (first-inspect (delib-flow--run-stage-locally run 'inspect-source))
           (accepted-inspect (delib-flow-test--accept-inspect first-inspect))
           (second-inspect (delib-flow--run-stage-locally accepted-inspect 'inspect-source))
           (accepted-second-inspect (delib-flow-test--accept-inspect second-inspect))
           (matched (delib-flow--run-stage-locally accepted-second-inspect 'match-project))
           (buffer (delib-flow--render-control-buffer matched)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Stage history" nil t))
            (should (search-forward "**** Inspect Source" nil t))
            (should (search-forward "- Attempts: 2" nil t))
            (should (search-forward "***** Attempt 1" nil t))
            (should (search-forward "- Review state: superseded" nil t))
            (should (search-forward "***** Attempt 2" nil t))
            (should (search-forward "- Review state: accepted" nil t))
            (should (search-forward "**** Match Project" nil t))
            (should (search-forward "- Attempts: 1" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-inspect-source-command-anchors-current-result ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Now")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-inspect-source)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-dispatch-action-executes-rendered-next-action ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Next actions")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "- Inspect Source")
            (goto-char (line-beginning-position)))
          (with-current-buffer buffer
            (delib-flow-dispatch-action))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))
            (goto-char (point-min))
            (should (search-forward "- Stage: Inspect Source" nil t))
            (should (search-forward "- Review state: pending-review" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-inspect-source-current-result-and-context-are-readable ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :file "/tmp/inbox/example.org"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (with-current-buffer buffer
          (let ((current-result
                 (delib-flow--section-content "Current result"
                                              delib-flow--active-run)))
            (should (string-match-p "- Source type: " current-result))
            (should (string-match-p "- Body preview: " current-result))
            (should-not (string-match-p "\\\\n" current-result)))
          (goto-char (point-min))
          (should (search-forward "** Relevant source context" nil t))
          (should (search-forward "** Source type correction" nil t))
          (should-not (search-forward "#(" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-accept-inspect-source-updates-review-state-and-actions ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run
          (delib-flow--seed-actions
           (delib-flow--apply-inspect-review-outcome
            inspected
            'accepted
            "Inspect result accepted. You may now match the project or retry inspect.")))
         (review (delib-flow--review-record
                  (delib-flow--run-working-context updated-run)
                  'inspect-source)))
    (should (eq 'accepted (plist-get review :candidate-review-state)))
    (should (plist-get review :accepted-output))
    (should (string-match-p "Inspect result accepted"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))
    (should (equal '(inspect-source
                     match-project
                     decide-cloud-pass
                     refresh-buffer
                     abort-run)
                   (mapcar (lambda (action)
                             (plist-get action :id))
                           (plist-get (delib-flow--run-actions updated-run)
                                      :items))))
    (should (eq 'accepted (plist-get (car (plist-get (delib-flow--run-stage-history updated-run)
                                                     :entries))
                                     :review-state)))))

(ert-deftest delib-flow-reject-inspect-source-updates-review-state-and-actions ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run
          (delib-flow--seed-actions
           (delib-flow--apply-inspect-review-outcome
            inspected
            'rejected
            "Inspect result rejected. Retry inspect before matching a project.")))
         (review (delib-flow--review-record
                  (delib-flow--run-working-context updated-run)
                  'inspect-source)))
    (should (eq 'rejected (plist-get review :candidate-review-state)))
    (should-not (plist-get review :accepted-output))
    (should (string-match-p "Inspect result rejected"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))
    (should (equal '(inspect-source
                     decide-cloud-pass
                     refresh-buffer
                     abort-run)
                   (mapcar (lambda (action)
                             (plist-get action :id))
                           (plist-get (delib-flow--run-actions updated-run)
                                      :items))))
    (should (eq 'rejected
                (plist-get (car (plist-get (delib-flow--run-stage-history updated-run)
                                           :entries))
                           :review-state)))))

(ert-deftest delib-flow-accept-inspect-source-command-rerenders-actions ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-accept-inspect-source)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "- Accepted inspect result: available" nil t))
            (goto-char (point-min))
            (should (search-forward "- Match Project [available]" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Discover Relevant Reference Material [available]" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Accept Inspect Result [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-approve-current-accepts-pending-inspect-result ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-active-run-buffer delib-flow--active-run
                                                       "Current result")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delib-flow-approve-current))
          (let ((review (delib-flow--review-record
                         (delib-flow--run-working-context delib-flow--active-run)
                         'inspect-source)))
            (should (eq 'accepted (plist-get review :candidate-review-state))))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Next actions"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-accept-inspect-source-command-anchors-next-actions ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-active-run-buffer delib-flow--active-run
                                                       "Current result")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-accept-inspect-source)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Next actions"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-inspect-source-retry-supersedes-prior-reviewed-result ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (first-pass (delib-flow-test--accept-inspect
                      (delib-flow--run-stage-locally run 'inspect-source)))
         (retried (delib-flow--run-stage-locally first-pass 'inspect-source))
         (review (delib-flow--review-record
                  (delib-flow--run-working-context retried)
                  'inspect-source))
         (entries (plist-get (delib-flow--run-stage-history retried) :entries)))
    (should (= 2 (length entries)))
    (should (eq 'superseded
                (plist-get (nth 0 entries) :review-state)))
    (should (eq 'pending-review
                (plist-get (nth 1 entries) :review-state)))
    (should (eq 'pending-review
                (plist-get review :candidate-review-state)))
    (should-not (plist-get review :accepted-output))))

(ert-deftest delib-flow-retry-current-reruns-pending-inspect-stage ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-active-run-buffer delib-flow--active-run
                                                       "Current result")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (delib-flow-retry-current))
          (should (= 2 (length (plist-get (delib-flow--run-stage-history delib-flow--active-run)
                                          :entries))))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-match-project-command-requires-accepted-inspect-result ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (should-error (delib-flow-action-match-project)))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-match-project-updates-stage-history-and-context ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (working (delib-flow--run-working-context updated-run))
           (project-match (plist-get working :project-match))
           (review (delib-flow--review-record working 'match-project)))
      (should (equal 'match-project (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'match-project (plist-get entry :stage-id)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal 'pending-review
                     (plist-get review :candidate-review-state)))
      (should (equal 'match-project
                     (plist-get review :candidate-stage-id)))
      (should (equal (plist-get entry :raw-output)
                     (plist-get review :candidate-output)))
      (should-not (plist-get review :accepted-output))
      (should (equal "Alpha Project"
                     (plist-get (plist-get project-match :best-project) :title)))
      (should (string-match-p "Review project match"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision)))
      (should (equal '(inspect-source
                       match-project
                       accept-match-project
                       reject-match-project
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items)))))))

(ert-deftest delib-flow-working-context-renders-review-state ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (updated-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-control-buffer updated-run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "** Relevant source context" nil t))
          (should (search-forward "- Proposed source type: " nil t))
          (should (search-forward "** Source type correction" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-match-project-ignores-state-bucket-headings ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project\nKickoff work\n* Complete\n** Alpha Archive\n* Waiting\n** Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal "Alpha Project"
                     (plist-get (plist-get project-match :best-project) :title)))
      (should-not (equal "Active"
                         (plist-get (plist-get project-match :best-project)
                                    :title))))))

(ert-deftest delib-flow-match-project-no-match-updates-follow-up-actions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'no-match (plist-get project-match :match-status)))
      (should (equal '(inspect-source
                       match-project
                       accept-match-project
                       reject-match-project
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items)))))))

(ert-deftest delib-flow-match-project-uses-contact-metadata ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\nContact alice@example.com\n* Beta Project\nContact bob@example.com\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Kickoff sync"
                       :content "* Kickoff sync\nPlease follow up with alice@example.com\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal "Alpha Project"
                     (plist-get (plist-get project-match :best-project) :title)))
      (should (string-match-p "metadata-aware match"
                              (plist-get project-match :reason))))))

(ert-deftest delib-flow-match-project-metadata-can-create-ambiguity ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project :shared:\nContact team@example.com\n* Beta Project :shared:\nContact team@example.com\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Shared planning"
                       :content "* Shared planning\nteam@example.com needs a reply\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'ambiguous (plist-get project-match :match-status)))
      (should (= 2 (length (plist-get project-match :candidates))))
      (should (string-match-p "metadata-aware match"
                              (plist-get project-match :reason))))))

(ert-deftest delib-flow-accept-match-project-seeds-manual-selection-template-for-no-match ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (accepted (delib-flow-test--accept-match matched))
           (manual-text
            (delib-flow--editable-block-text
             (delib-flow--editable-block accepted 'manual-project-selection))))
      (should (string-match-p "Selection:" manual-text))
      (should (string-match-p "Candidates:" manual-text))
      (should (string-match-p "Alpha Project" manual-text))
      (should (string-match-p "Beta Project" manual-text)))))

(ert-deftest delib-flow-manual-project-match-updates-project-context ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (selected-run
            (delib-flow-test--set-manual-project-selection
             matched
             "Alpha Project"
             "Operator selected the best fallback project."))
           (updated-run (delib-flow--run-stage-locally selected-run
                                                       'manual-project-match))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match))
           (match-review
            (delib-flow--review-record
             (delib-flow--run-working-context updated-run)
             'match-project)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal 'manual (plist-get project-match :selection-method)))
      (should (equal "Alpha Project"
                     (plist-get (plist-get project-match :best-project) :title)))
      (should (string-match-p "Operator selected"
                              (plist-get project-match :reason)))
      (should (eq 'superseded
                  (plist-get match-review :candidate-review-state)))
      (should-not (plist-get match-review :accepted-output))
      (should (equal '(inspect-source
                       match-project
                       discover-reference-material
                       manual-project-match
                       extract-actions
                       extract-waiting-for
                       suggest-reference-notes
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items)))))))

(ert-deftest delib-flow-manual-project-match-command-rerenders-history ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run
            (delib-flow-test--set-manual-project-selection
             (delib-flow-test--accept-match
              (delib-flow--run-stage-locally inspected 'match-project))
             "Alpha Project"
             "Operator selected the fallback project."))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-manual-project-match)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "** Choose Project Manually" nil t))
              (should (search-forward "Selected project: Alpha Project" nil t))
              (goto-char (point-min))
              (should (search-forward "- Retry Choose Project Manually [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-manual-project-match-can-reject-all-candidates ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (selected-run
            (delib-flow-test--set-manual-project-selection
             matched
             "REJECT"
             "Operator rejected all fallback candidates."))
           (updated-run (delib-flow--run-stage-locally selected-run
                                                       'manual-project-match))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match))
           (match-review
            (delib-flow--review-record
             (delib-flow--run-working-context updated-run)
             'match-project)))
      (should (equal 'no-match (plist-get project-match :match-status)))
      (should (equal 'manual (plist-get project-match :selection-method)))
      (should-not (plist-get project-match :best-project))
      (should (equal "REJECT" (plist-get project-match :operator-selection)))
      (should (string-match-p "rejected all"
                              (plist-get project-match :reason)))
      (should (eq 'superseded
                  (plist-get match-review :candidate-review-state)))
      (should (member 'propose-new-project
                      (mapcar (lambda (action)
                              (plist-get action :id))
                              (plist-get (delib-flow--run-actions updated-run)
                                         :items)))))))

(ert-deftest delib-flow-current-decision-hides-manual-selection-when-inactive ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line one\n")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should-not (search-forward "** Manual project selection" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-working-context-renders-accepted-manual-project-decision ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (selected-run
            (delib-flow-test--set-manual-project-selection
             matched
             "Alpha Project"
             "Operator selected the best fallback project."))
           (manual-run
            (delib-flow--run-stage-locally selected-run 'manual-project-match))
           (buffer (delib-flow--render-control-buffer manual-run)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "- Accepted project decision: Matched: Alpha Project" nil t))
            (should-not (search-forward "** Manual project selection" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-propose-new-project-updates-stage-history-and-filing ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (updated-run (delib-flow--run-stage-locally matched
                                                       'propose-new-project))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (filing (plist-get updated-run :filing))
           (draft-items (plist-get filing :draft-items)))
      (should (equal 'propose-new-project (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'propose-new-project (plist-get entry :stage-id)))
      (should (equal 'project (plist-get (car draft-items) :kind)))
      (should (equal "Completely Different Topic"
                     (plist-get (car draft-items) :title)))
      (should (string-match-p "Review proposed project checklist"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision))))))

(ert-deftest delib-flow-propose-new-project-command-rerenders-filing-preview ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run
            (delib-flow-test--accept-match
             (delib-flow--run-stage-locally inspected 'match-project)))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-propose-new-project)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "** Propose New Project" nil t))
              (should (search-forward "Proposed project title: Completely Different Topic" nil t))
              (goto-char (point-min))
              (should (search-forward "- PROJECT Completely Different Topic" nil t))
              (goto-char (point-min))
              (should (search-forward "- Retry Propose New Project [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-match-project-command-rerenders-history ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run inspected)
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-match-project)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "- Match status: matched" nil t))
              (goto-char (point-min))
              (should (search-forward "- Stage: Match Project" nil t))
              (should (search-forward "- Best project: Alpha Project" nil t))
              (goto-char (point-min))
              (should (search-forward "- Accept Project Match [available]" nil t))
              (goto-char (point-min))
              (should (search-forward "- Reject Project Match [available]" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- Discover Relevant Reference Material [available]" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- Extract Actions [available]" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- Extract Waiting-For [available]" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- Suggest Reference Notes [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-accept-match-project-updates-review-state-and-actions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (updated-run (delib-flow-test--accept-match matched))
           (review (delib-flow--review-record
                    (delib-flow--run-working-context updated-run)
                    'match-project)))
      (should (eq 'accepted (plist-get review :candidate-review-state)))
      (should (plist-get review :accepted-output))
      (should (string-match-p "Project match accepted"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision)))
      (should (equal '(inspect-source
                       match-project
                       discover-reference-material
                       extract-actions
                       extract-waiting-for
                       suggest-reference-notes
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items))))
      (should (eq 'accepted
                  (plist-get (car (last (plist-get (delib-flow--run-stage-history updated-run)
                                                   :entries)))
                             :review-state))))))

(ert-deftest delib-flow-reject-match-project-updates-review-state-and-actions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (updated-run
            (delib-flow--seed-actions
             (delib-flow--apply-match-review-outcome
              matched
              'rejected
              "Project match rejected. Retry project matching before downstream project-dependent stages.")))
           (review (delib-flow--review-record
                    (delib-flow--run-working-context updated-run)
                    'match-project)))
      (should (eq 'rejected (plist-get review :candidate-review-state)))
      (should-not (plist-get review :accepted-output))
      (should (string-match-p "Project match rejected"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision)))
      (should (equal '(inspect-source
                       match-project
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items))))
      (should (eq 'rejected
                  (plist-get (car (last (plist-get (delib-flow--run-stage-history updated-run)
                                                   :entries)))
                             :review-state))))))

(ert-deftest delib-flow-accept-match-project-command-rerenders-actions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run
            (delib-flow--run-stage-locally inspected 'match-project))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-accept-match-project)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "- Accepted project decision: Matched: Alpha Project" nil t))
              (goto-char (point-min))
              (should (search-forward "- Discover Relevant Reference Material [available]" nil t))
              (goto-char (point-min))
              (should (search-forward "- Extract Actions [available]" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- Accept Project Match [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-match-project-retry-supersedes-prior-reviewed-result ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (first-pass (delib-flow-test--accept-match
                        (delib-flow--run-stage-locally inspected 'match-project)))
           (retried (delib-flow--run-stage-locally first-pass 'match-project))
           (review (delib-flow--review-record
                    (delib-flow--run-working-context retried)
                    'match-project))
           (entries (seq-filter (lambda (entry)
                                 (eq 'match-project (plist-get entry :stage-id)))
                               (plist-get (delib-flow--run-stage-history retried)
                                          :entries))))
      (should (= 2 (length entries)))
      (should (eq 'superseded
                  (plist-get (nth 0 entries) :review-state)))
      (should (eq 'pending-review
                  (plist-get (nth 1 entries) :review-state)))
      (should (eq 'pending-review
                  (plist-get review :candidate-review-state)))
      (should-not (plist-get review :accepted-output)))))

(ert-deftest delib-flow-discover-reference-material-updates-stage-history-and-context ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nAgenda\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (updated-run
              (delib-flow--run-stage-locally matched
                                             'discover-reference-material))
             (history (delib-flow--run-stage-history updated-run))
             (entry (car (last (plist-get history :entries))))
             (working (delib-flow--run-working-context updated-run))
             (retrieved (plist-get working :retrieved-candidates)))
        (should (equal 'discover-reference-material
                       (plist-get history :latest-stage)))
        (should (equal 'completed (plist-get history :latest-status)))
        (should (equal 'discover-reference-material (plist-get entry :stage-id)))
        (should retrieved)
        (should (equal "Alpha Project Notes"
                       (plist-get (car retrieved) :title)))
        (should (string-match-p "Review retrieved reference candidates"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (member 'discover-reference-material
                        (mapcar (lambda (action)
                                  (plist-get action :id))
                                (plist-get (delib-flow--run-actions updated-run)
                                           :items))))))))

(ert-deftest delib-flow-discover-reference-material-prioritizes-project-linked-notes ()
  (delib-flow-test--with-temp-zk-root
      '(("linked-note.org" . "#+title: Support Material\nA note linked from the project.\n")
        ("term-heavy.org" . "#+title: Alpha Project Kickoff Notes\nAlpha project kickoff agenda and blockers.\n"))
    (let* ((linked-file (expand-file-name "linked-note.org" delib-flow-zk-root))
           (project-content
            (format "* Alpha Project\nSee [[file:%s][Support]]\n"
                    linked-file)))
      (delib-flow-test--with-temp-project-file
          project-content
        (let* ((run (delib-flow--initialize-run
                     (list :title "Alpha Project kickoff"
                           :content "* Alpha Project kickoff\nAgenda\n")))
               (inspected (delib-flow--run-stage-locally run 'inspect-source))
               (matched (delib-flow--run-stage-locally inspected 'match-project))
               (updated-run
                (delib-flow--run-stage-locally matched
                                               'discover-reference-material))
               (retrieved
                (plist-get (delib-flow--run-working-context updated-run)
                           :retrieved-candidates)))
          (should retrieved)
          (should (equal "Support Material"
                         (plist-get (car retrieved) :title)))
          (should (equal '(linked-project-file text-overlap)
                         (mapcar (lambda (signal)
                                   (plist-get signal :key))
                                 (seq-filter
                                  (lambda (signal)
                                    (> (plist-get signal :contribution) 0))
                                  (plist-get (car retrieved) :signals)))))
          (should (member "linked-project-file=1 (+12)"
                          (plist-get (car retrieved) :reasons))))))))

(ert-deftest delib-flow-discover-reference-material-uses-project-tags-and-contacts ()
  (delib-flow-test--with-temp-zk-root
      '(("contact-note.org" . "#+title: Follow-up material\n#+filetags: :alpha:\nReach alice@example.com about next steps.\n")
        ("plain-note.org" . "#+title: General Notes\nMiscellaneous text.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project :alpha:\nContact alice@example.com\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Kickoff sync"
                         :content "* Kickoff sync\nContact alice@example.com\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (updated-run
              (delib-flow--run-stage-locally matched
                                             'discover-reference-material))
             (retrieved
              (plist-get (delib-flow--run-working-context updated-run)
                         :retrieved-candidates)))
        (should retrieved)
        (should (equal "Follow-up material"
                       (plist-get (car retrieved) :title)))))))

(ert-deftest delib-flow-discover-reference-material-prioritizes-source-linked-notes ()
  (delib-flow-test--with-temp-zk-root
      '(("linked-from-source.org" . "#+title: Linked From Source\nReferenced directly by the source item.\n")
        ("term-heavy.org" . "#+title: Alpha Project Kickoff Notes\nAlpha project kickoff agenda and blockers.\n"))
    (let* ((linked-file (expand-file-name "linked-from-source.org" delib-flow-zk-root))
           (source-file (make-temp-file "delib-flow-source" nil ".org"))
           (source-content
            (format "* Alpha Project kickoff\nAgenda\nSee [[file:%s][Linked]]\n"
                    linked-file)))
      (unwind-protect
          (delib-flow-test--with-temp-project-file
              "* Alpha Project\n"
            (let* ((run (delib-flow--initialize-run
                         (list :title "Alpha Project kickoff"
                               :file source-file
                               :content source-content)))
                   (inspected (delib-flow-test--accept-inspect
                               (delib-flow--run-stage-locally run 'inspect-source)))
                   (matched (delib-flow-test--accept-match
                             (delib-flow--run-stage-locally inspected 'match-project)))
                   (updated-run
                    (delib-flow--run-stage-locally matched
                                                   'discover-reference-material))
                   (retrieved
                    (plist-get (delib-flow--run-working-context updated-run)
                               :retrieved-candidates)))
              (should retrieved)
              (should (equal "Linked From Source"
                             (plist-get (car retrieved) :title)))
              (should (member "linked-source-file=1 (+15)"
                              (plist-get (car retrieved) :reasons)))))
        (delete-file source-file)))))

(ert-deftest delib-flow-discover-reference-material-uses-source-contact-overlap ()
  (delib-flow-test--with-temp-zk-root
      '(("source-contact-note.org" . "#+title: Source Contact Note\nPlease coordinate with alice@example.com.\n")
        ("plain-note.org" . "#+title: General Notes\nMiscellaneous text.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nContact alice@example.com\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (updated-run
              (delib-flow--run-stage-locally matched
                                             'discover-reference-material))
             (retrieved
              (plist-get (delib-flow--run-working-context updated-run)
                         :retrieved-candidates)))
        (should retrieved)
        (should (equal "Source Contact Note"
                       (plist-get (car retrieved) :title)))
        (should (member "source-contact-overlap=1 (+3)"
                        (plist-get (car retrieved) :reasons)))))))

(ert-deftest delib-flow-discover-reference-material-command-rerenders-history ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (delib-flow--active-run matched)
             (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-discover-reference-material)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "Retrieved context: available" nil t))
                (should (search-forward "*** Retrieved candidates" nil t))
                (should (search-forward "Alpha Project Notes" nil t))
                (should (search-forward "title-overlap=" nil t))
                (goto-char (point-min))
                (should (search-forward "- Stage: Discover Relevant Reference Material" nil t))
                (should (search-forward "- Candidate count: 1" nil t))
                (goto-char (point-min))
                (should (search-forward "- Filter Useful Reference Material [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-filter-reference-material-updates-stage-history-and-context ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (discovered
            (delib-flow--run-stage-locally inspected
                                           'discover-reference-material))
           (updated-run
            (delib-flow--run-stage-locally discovered
                                           'filter-reference-material))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (working (delib-flow--run-working-context updated-run))
           (filtered (plist-get working :filtered-context)))
      (should (equal 'filter-reference-material
                     (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'filter-reference-material (plist-get entry :stage-id)))
      (should filtered)
      (should (> (plist-get filtered :retained-count) 0))
      (should (equal 'retained
                     (plist-get (car (plist-get filtered :retained-candidates))
                                :filter-status)))
      (should (member "retained-by-score-threshold"
                      (plist-get (car (plist-get filtered :retained-candidates))
                                 :filter-reasons)))
      (should (string-match-p "Alpha Project Notes"
                              (plist-get working :retained-context)))
      (should (string-match-p "Review retained context"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision)))
      (should (member 'filter-reference-material
                      (mapcar (lambda (action)
                                (plist-get action :id))
                              (plist-get (delib-flow--run-actions updated-run)
                                         :items)))))))

(ert-deftest delib-flow-filter-reference-material-command-rerenders-history ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (discovered
            (delib-flow--run-stage-locally inspected
                                           'discover-reference-material))
           (delib-flow--active-run discovered)
           (buffer (delib-flow--render-control-buffer discovered)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-filter-reference-material)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "Filtered context: available" nil t))
                (should (search-forward "*** Retained context" nil t))
                (should (search-forward "*** Rejected context" nil t))
                (should (search-forward "Retained: 2. Rejected: 0." nil t))
                (should (search-forward "retained-by-score-threshold" nil t))
                (should (search-forward "Alpha Project Notes" nil t))
                (goto-char (point-min))
                (should (search-forward "- Stage: Filter Useful Reference Material" nil t))
                (should (search-forward "- Retained count: 2" nil t))
                (should (search-forward "- Retained candidates:" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Filter Useful Reference Material [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-filter-reference-material-annotates-fallback-retention ()
  (let* ((candidate-a (list :title "Top candidate"
                            :score 1
                            :reasons '("title-overlap=1 (+1)")))
         (candidate-b (list :title "Rejected candidate"
                            :score 1
                            :reasons '("text-overlap=1 (+1)")))
         (package (list :working-context
                        (list :retrieved-candidates (list candidate-a candidate-b))))
         (filtered (delib-flow--filter-reference-material-result package)))
    (should (equal 2 (plist-get filtered :candidate-count)))
    (should (equal 1 (plist-get filtered :retained-count)))
    (should (equal 1 (plist-get filtered :rejected-count)))
    (should (equal '("retained-as-top-fallback")
                   (plist-get (car (plist-get filtered :retained-candidates))
                              :filter-reasons)))
    (should (equal '("rejected-below-score-threshold")
                   (plist-get (car (plist-get filtered :rejected-candidates))
                              :filter-reasons)))))

(ert-deftest delib-flow-working-context-renders-retrieval-review-subsections ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (discovered
            (delib-flow--run-stage-locally inspected
                                           'discover-reference-material))
           (filtered
            (delib-flow--run-stage-locally discovered
                                           'filter-reference-material))
           (buffer (delib-flow--render-control-buffer filtered)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Retrieved candidates" nil t))
            (should (search-forward "*** Retained context" nil t))
            (should (search-forward "*** Rejected context" nil t))
            (should (search-forward "Alpha Project Notes" nil t))
            (should (search-forward "- none" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-filter-reference-material-retains-salient-context ()
  (let ((file (make-temp-file "delib-flow-note" nil ".org"
                              "#+title: Constraint note\nA blocker remains because of a client deadline.\n")))
    (unwind-protect
        (let* ((candidate-a (list :title "Constraint note"
                                  :file file
                                  :score 1
                                  :reasons '("text-overlap=1 (+1)")))
               (candidate-b (list :title "Filler note"
                                  :score 1
                                  :reasons '("text-overlap=1 (+1)")))
               (package (list :working-context
                              (list :retrieved-candidates
                                    (list candidate-a candidate-b))))
               (filtered (delib-flow--filter-reference-material-result package)))
          (should (equal 1 (plist-get filtered :retained-count)))
          (should (equal "Constraint note"
                         (plist-get (car (plist-get filtered :retained-candidates))
                                    :title)))
          (should (member "salient-constraint-context"
                          (plist-get (car (plist-get filtered :retained-candidates))
                                     :filter-reasons)))
          (should (equal '("rejected-below-score-threshold")
                         (plist-get (car (plist-get filtered :rejected-candidates))
                                    :filter-reasons))))
      (delete-file file))))

(ert-deftest delib-flow-extract-actions-updates-stage-history-and-filing ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nAgenda\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (updated-run
              (delib-flow--run-stage-locally filtered 'extract-actions))
             (history (delib-flow--run-stage-history updated-run))
             (entry (car (last (plist-get history :entries))))
             (filing (plist-get updated-run :filing))
             (draft-items (plist-get filing :draft-items)))
        (should (equal 'extract-actions (plist-get history :latest-stage)))
        (should (equal 'completed (plist-get history :latest-status)))
        (should (equal 'extract-actions (plist-get entry :stage-id)))
        (should draft-items)
        (should (string-match-p "Clarify the next step"
                                (plist-get (car draft-items) :text)))
        (should (string-match-p "Review drafted actions"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (string-match-p "TODO"
                                (plist-get filing :preview-text)))))))

(ert-deftest delib-flow-draft-item-preview-line-renders-waiting-keyword ()
  (should (equal "- WAITING Waiting for reply"
                 (delib-flow--draft-item-preview-line
                  (list :kind 'waiting-for
                        :text "Waiting for reply")))))

(ert-deftest delib-flow-draft-item-preview-line-renders-note-keyword ()
  (should (equal "- NOTE Create project support note"
                 (delib-flow--draft-item-preview-line
                  (list :kind 'reference-note
                        :text "Create project support note")))))

(ert-deftest delib-flow-extract-actions-command-rerenders-filing-preview ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (delib-flow--active-run filtered)
             (buffer (delib-flow--render-control-buffer filtered)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-extract-actions)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Extract Actions" nil t))
                (should (search-forward "- Candidate count: 3" nil t))
                (goto-char (point-min))
                (should (search-forward "Draft filing artifacts are available." nil t))
                (should (search-forward "- TODO Clarify the next step for Alpha Project kickoff" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Extract Actions [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-extract-waiting-for-updates-stage-history-and-filing ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nAgenda\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (updated-run
              (delib-flow--run-stage-locally filtered 'extract-waiting-for))
             (history (delib-flow--run-stage-history updated-run))
             (entry (car (last (plist-get history :entries))))
             (filing (plist-get updated-run :filing))
             (draft-items (plist-get filing :draft-items)))
        (should (equal 'extract-waiting-for
                       (plist-get history :latest-stage)))
        (should (equal 'completed (plist-get history :latest-status)))
        (should (equal 'extract-waiting-for (plist-get entry :stage-id)))
        (should draft-items)
        (should (eq 'waiting-for (plist-get (car draft-items) :kind)))
        (should (string-match-p "Waiting for a concrete response"
                                (plist-get (car draft-items) :text)))
        (should (string-match-p "Review drafted waiting-for items"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (string-match-p "WAITING"
                                (plist-get filing :preview-text)))))))

(ert-deftest delib-flow-extract-waiting-for-command-rerenders-filing-preview ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (delib-flow--active-run filtered)
             (buffer (delib-flow--render-control-buffer filtered)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-extract-waiting-for)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Extract Waiting-For" nil t))
                (should (search-forward "- Candidate count: 3" nil t))
                (goto-char (point-min))
                (should (search-forward "Draft filing artifacts are available." nil t))
                (should (search-forward "- WAITING Waiting for a concrete response about Alpha Project kickoff" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Extract Waiting-For [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-suggest-reference-notes-updates-stage-history-and-filing ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nAgenda\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (updated-run
              (delib-flow--run-stage-locally filtered
                                             'suggest-reference-notes))
             (history (delib-flow--run-stage-history updated-run))
             (entry (car (last (plist-get history :entries))))
             (filing (plist-get updated-run :filing))
             (draft-items (plist-get filing :draft-items)))
        (should (equal 'suggest-reference-notes
                       (plist-get history :latest-stage)))
        (should (equal 'completed (plist-get history :latest-status)))
        (should (equal 'suggest-reference-notes
                       (plist-get entry :stage-id)))
        (should draft-items)
        (should (eq 'reference-note (plist-get (car draft-items) :kind)))
        (should (string-match-p "Create general PKM note"
                                (plist-get (car draft-items) :text)))
        (should (string-match-p "Review drafted reference notes"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (string-match-p "NOTE"
                                (plist-get filing :preview-text)))))))

(ert-deftest delib-flow-suggest-reference-notes-command-rerenders-filing-preview ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (delib-flow--active-run filtered)
             (buffer (delib-flow--render-control-buffer filtered)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-suggest-reference-notes)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Suggest Reference Notes" nil t))
                (should (search-forward "- Candidate count: 3" nil t))
                (goto-char (point-min))
                (should (search-forward "Draft filing artifacts are available." nil t))
                (should (search-forward "- NOTE Create general PKM note for Alpha Project kickoff" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Suggest Reference Notes [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-decide-cloud-pass-updates-stage-history-and-routing ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run (delib-flow--run-stage-locally inspected
                                                     'decide-cloud-pass))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (routing (plist-get updated-run :routing)))
    (should (equal 'decide-cloud-pass (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'decide-cloud-pass (plist-get entry :stage-id)))
    (should (plist-get routing :cloud-switch-pending))
    (should (eq 'required (plist-get routing :sanitization-status)))
    (should (equal "cloud-model-unconfigured"
                   (plist-get routing :selected-cloud-model)))
    (should (equal "cloud-model-unconfigured"
                   (plist-get routing :selected-cloud-provider)))
    (should (eq 'standard
                (plist-get routing :cloud-policy-profile)))
    (should (member 'sanitize-for-cloud
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should (string-match-p "Review cloud-routing decision"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-decide-cloud-pass-fails-when-provider-policy-disables-routing ()
  (let* ((delib-flow-default-cloud-model "blocked:model")
         (delib-flow-cloud-provider-policy-alist
          '(("blocked" :enabled nil :policy-profile strict)
            (default :enabled t :policy-profile standard)))
         (run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run (delib-flow--run-stage-locally inspected
                                                     'decide-cloud-pass))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries)))))
    (should (equal 'failed (plist-get history :latest-status)))
    (should (equal 'decide-cloud-pass (plist-get history :latest-stage)))
    (should (equal 'failed (plist-get entry :status)))
    (should (string-match-p "Cloud routing is disabled for provider blocked"
                            (plist-get entry :normalized-output)))
    (should (string-match-p "Review stage failure"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-decide-cloud-pass-command-rerenders-working-context ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run
                                                                'inspect-source))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-decide-cloud-pass)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "- Stage: Decide on Cloud Pass" nil t))
            (should (search-forward "Selected model:" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-sanitized context: pending preparation" nil t))
            (should (search-forward "- Cloud context: Cloud pass selected. Model:" nil t))
            (goto-char (point-min))
            (should (search-forward "- Sanitize for Cloud [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Decide on Cloud Pass [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-sanitize-for-cloud-updates-stage-history-and-context ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (updated-run
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (working (delib-flow--run-working-context updated-run))
         (routing (plist-get updated-run :routing))
         (cloud-context (plist-get working :cloud-sanitized-context)))
    (should (equal 'sanitize-for-cloud (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'sanitize-for-cloud (plist-get entry :stage-id)))
    (should cloud-context)
    (should (string-match-p "\\[redacted-email\\]" cloud-context))
    (should (string-match-p "\\[redacted-url\\]" cloud-context))
    (should (eq 'prepared (plist-get routing :sanitization-status)))
    (should (member 'approve-cloud-send
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should-not (member 'run-cloud-stage
                        (mapcar (lambda (action)
                                  (plist-get action :id))
                                (plist-get (delib-flow--run-actions updated-run)
                                           :items))))
    (should (string-match-p "Review sanitized cloud package"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-sanitize-for-cloud-strict-policy-redacts-links-and-project-terms ()
  (let* ((delib-flow-default-cloud-model "strict:model")
         (delib-flow-cloud-provider-policy-alist
         '(("strict" :enabled t :policy-profile strict)
            (default :enabled t :policy-profile standard)))
         (run (delib-flow--initialize-run
               (list :title "Alpha Project"
                     :content "* Alpha Project\nDiscuss alpha project timeline\nSee [[file:secret.org][secret]]\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (matched (plist-put
                   inspected
                   :working-context
                   (plist-put
                    (delib-flow--run-working-context inspected)
                    :project-match
                    (list :best-project (list :title "Alpha Project")))))
         (cloud-decided
          (delib-flow--run-stage-locally matched 'decide-cloud-pass))
         (updated-run
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (working (delib-flow--run-working-context updated-run))
         (routing (plist-get updated-run :routing))
         (cloud-context (plist-get working :cloud-sanitized-context)))
    (should (eq 'strict (plist-get routing :cloud-policy-profile)))
    (should (string-match-p "\\[redacted-email\\]" cloud-context))
    (should (string-match-p "\\[redacted-url\\]" cloud-context))
    (should (string-match-p "\\[redacted-link\\]" cloud-context))
    (should (string-match-p "\\[redacted-project\\]" cloud-context))
    (should-not (string-match-p "Alpha" cloud-context))
    (should-not (string-match-p "alpha project timeline" cloud-context))
    (should-not (string-match-p "\\[\\[file:secret\\.org\\]\\[secret\\]\\]"
                                cloud-context))))

(ert-deftest delib-flow-sanitize-for-cloud-command-rerenders-working-context ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (delib-flow--active-run
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-sanitize-for-cloud)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "- Stage: Sanitize for Cloud" nil t))
            (should (search-forward "Sanitization status: prepared" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-sanitized context: available" nil t))
            (goto-char (point-min))
            (should (search-forward "- Cloud context: " nil t))
            (goto-char (point-min))
            (should (search-forward "Sanitized source title:" nil t))
            (should (search-forward "[redacted-email]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Approve Cloud Send [available]" nil t))
            (should-not (search-forward "- Run Cloud Stage [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Sanitize for Cloud [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-approve-cloud-send-updates-routing-and-reviewed-package ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (sanitized
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (edited-run
          (delib-flow--set-editable-block
           sanitized
           'cloud-package-review
           (delib-flow--set-editable-block-text
            (delib-flow--editable-block sanitized 'cloud-package-review)
            "Reviewed package for cloud send.")))
         (updated-run
          (delib-flow--run-stage-locally edited-run 'approve-cloud-send))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (working (delib-flow--run-working-context updated-run))
         (routing (plist-get updated-run :routing)))
    (should (equal 'approve-cloud-send (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'approve-cloud-send (plist-get entry :stage-id)))
    (should (equal "Reviewed package for cloud send."
                   (plist-get working :cloud-sanitized-context)))
    (should (eq 'approved (plist-get routing :sanitization-status)))
    (should (member 'run-cloud-stage
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should (string-match-p "Review approved cloud package"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-approve-cloud-send-command-syncs-reviewed-package-edit ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (delib-flow--active-run
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t)
            (goto-char (point-min))
            (search-forward "#+begin_delib-edit cloud-review" nil t)
            (forward-line 1)
            (insert "Approved reviewed cloud package.\n"))
          (delib-flow-action-approve-cloud-send)
          (should (string-match-p
                   "Approved reviewed cloud package."
                   (plist-get (delib-flow--run-working-context delib-flow--active-run)
                              :cloud-sanitized-context)))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "** Approve Cloud Send" nil t))
            (should (search-forward "Sanitization status: approved" nil t))
            (goto-char (point-min))
            (should (search-forward "- Run Cloud Stage [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-run-cloud-stage-updates-stage-history-and-context ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (sanitized
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (approved
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (updated-run (delib-flow--run-stage-in-cloud approved
                                                      'run-cloud-stage))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (working (delib-flow--run-working-context updated-run))
         (routing (plist-get updated-run :routing))
         (cloud-output (plist-get working :cloud-returned-context)))
    (should (equal 'run-cloud-stage (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'run-cloud-stage (plist-get entry :stage-id)))
    (should cloud-output)
    (should (string-match-p "Cloud output for reviewed package" cloud-output))
    (should (eq 'returned (plist-get routing :sanitization-status)))
    (should (eq 'pending-review (plist-get routing :reintegration-status)))
    (should-not (plist-get routing :cloud-switch-pending))
    (should (string-match-p "Review cloud-returned result"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-run-cloud-stage-command-rerenders-working-context ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (delib-flow--active-run
          (delib-flow--run-stage-locally
           (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud)
           'approve-cloud-send))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-run-cloud-stage)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "- Stage: Run Cloud Stage" nil t))
            (should (search-forward "Cloud output for reviewed package" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-returned context: available" nil t))
            (goto-char (point-min))
            (should (search-forward "- Cloud-returned summary: Cloud output for reviewed package" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Run Cloud Stage [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Approve Candidate Reintegration [available]" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Integrate into Source [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-approve-candidate-reintegration-updates-routing ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (sanitized
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (approved-send
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (cloud-run
          (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (updated-run
          (delib-flow--run-stage-locally cloud-run
                                         'approve-candidate-reintegration))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (routing (plist-get updated-run :routing)))
    (should (equal 'approve-candidate-reintegration
                   (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'approve-candidate-reintegration
                   (plist-get entry :stage-id)))
    (should (eq 'approved (plist-get routing :reintegration-status)))
    (should (member 'integrate-into-source
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should (string-match-p "Review approved reintegration candidate"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-approve-candidate-reintegration-command-rerenders-actions ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally inspected 'decide-cloud-pass))
         (sanitized
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (approved-send
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (delib-flow--active-run
          (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-approve-candidate-reintegration)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "** Approve Candidate Reintegration" nil t))
            (should (search-forward "Reintegration status: approved" nil t))
            (goto-char (point-min))
            (should (search-forward "- Integrate into Source [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-integrate-into-source-updates-context-and-filing ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nContact alice@example.com\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'extract-actions))
             (cloud-decided
              (delib-flow--run-stage-locally drafted 'decide-cloud-pass))
             (sanitized
              (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
             (approved-send
              (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
             (cloud-run
              (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
             (approved-reintegration
              (delib-flow--run-stage-locally cloud-run
                                             'approve-candidate-reintegration))
             (updated-run
              (delib-flow--run-stage-locally approved-reintegration
                                             'integrate-into-source))
             (history (delib-flow--run-stage-history updated-run))
             (entry (car (last (plist-get history :entries))))
             (working (delib-flow--run-working-context updated-run))
             (filing (plist-get updated-run :filing))
             (draft-items (plist-get filing :draft-items)))
        (should (equal 'integrate-into-source (plist-get history :latest-stage)))
        (should (equal 'completed (plist-get history :latest-status)))
        (should (equal 'integrate-into-source (plist-get entry :stage-id)))
        (should (string-match-p "Cloud-reviewed context"
                                (plist-get working :retained-context)))
        (should draft-items)
        (should-not (plist-get filing :approved-items))
        (should (equal (length draft-items)
                       (plist-get (plist-get entry :raw-output) :draft-count)))
        (should (string-match-p "Review integrated local result"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))))))

(ert-deftest delib-flow-integrate-into-source-command-rerenders-filing-preview ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nContact alice@example.com\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'extract-actions))
             (cloud-decided
              (delib-flow--run-stage-locally drafted 'decide-cloud-pass))
             (sanitized
              (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
             (approved-send
              (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
             (delib-flow--active-run
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage)
               'approve-candidate-reintegration))
             (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-integrate-into-source)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Integrate into Source" nil t))
                (should (search-forward "Draft artifact count:" nil t))
                (goto-char (point-min))
                (should (search-forward "** Draft artifacts" nil t))
                (should (search-forward "Clarify the next step for Alpha Project kickoff" nil t))
                (goto-char (point-min))
                (should (search-forward "Cloud-reviewed context" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Integrate into Source [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-select-approved-filing-actions-updates-filing-state ()
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
           (updated-run
            (delib-flow--run-stage-locally integrated
                                           'select-approved-filing-actions))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (filing (plist-get updated-run :filing))
           (approved-items (plist-get filing :approved-items))
           (draft-items (plist-get filing :draft-items)))
      (should (equal 'select-approved-filing-actions
                     (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'select-approved-filing-actions
                     (plist-get entry :stage-id)))
      (should approved-items)
      (should (equal 1 (length approved-items)))
      (should-not draft-items)
      (should (equal 1 (plist-get (plist-get entry :raw-output) :selected-count)))
      (should (string-match-p "Review selected filing action"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision))))))

(ert-deftest delib-flow-select-approved-filing-actions-command-rerenders-preview ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (delib-flow--active-run
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-select-approved-filing-actions)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "** Select Approved Filing Actions" nil t))
              (should (search-forward "Selected artifact count:" nil t))
              (goto-char (point-min))
              (should (search-forward "** Approved artifacts" nil t))
              (should (search-forward "Clarify the next step for Alpha Project kickoff" nil t))
              (goto-char (point-min))
              (should (search-forward "- File Approved Outputs [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-file-approved-outputs-updates-stage-history-and-project-file ()
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
            (delib-flow--run-stage-locally integrated
                                           'select-approved-filing-actions))
           (updated-run
            (delib-flow--run-stage-locally selected 'file-approved-outputs))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (filing (plist-get updated-run :filing))
            (locations (plist-get filing :target-locations)))
      (should (equal 'file-approved-outputs (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'file-approved-outputs (plist-get entry :stage-id)))
      (should locations)
      (should-not (plist-get filing :approved-items))
      (should (string-match-p "Alpha Project"
                              (plist-get (car locations) :target)))
      (with-temp-buffer
        (insert-file-contents delib-flow-my-projects-file)
        (should (search-forward "** TODO Clarify the next step for Alpha Project kickoff"
                                nil t)))
      (should (string-match-p "Review filed outputs"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision))))))

(ert-deftest delib-flow-file-approved-outputs-creates-reference-note-file ()
  (delib-flow-test--with-temp-zk-root ()
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted
              (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (integrated
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (selected
              (delib-flow--run-stage-locally integrated
                                             'select-approved-filing-actions))
             (updated-run
              (delib-flow--run-stage-locally selected 'file-approved-outputs))
             (location (car (plist-get (plist-get updated-run :filing)
                                       :target-locations))))
        (should (file-exists-p (plist-get location :target)))
        (with-temp-buffer
          (insert-file-contents (plist-get location :target))
          (should (search-forward "#+title: Alpha Project kickoff" nil t))
          (should (search-forward "Create general PKM note for Alpha Project kickoff"
                                  nil t)))))))

(ert-deftest delib-flow-reference-note-content-uses-project-support-template ()
  (let ((content
         (delib-flow--reference-note-content
          (list :kind 'reference-note
                :text "Create project support note from Alpha Constraints"
                :note-type 'project-support))))
    (should (string-match-p "#\\+title: Alpha Constraints" content))
    (should (string-match-p "#\\+filetags: :project:support:" content))
    (should (string-match-p "Create project support note from Alpha Constraints"
                            content))))

(ert-deftest delib-flow-file-approved-outputs-uses-configured-general-note-template ()
  (let ((delib-flow-general-note-template
         "#+title: ${title}\n#+filetags: :custom:general:\n\nSource: ${source-artifact}\nType: ${note-type}\n"))
    (delib-flow-test--with-temp-zk-root ()
      (delib-flow-test--with-temp-project-file
          "* Alpha Project\n"
        (let* ((run (delib-flow--initialize-run
                     (list :title "Alpha Project kickoff"
                           :content "* Alpha Project kickoff\nBody line\n")))
               (inspected (delib-flow--run-stage-locally run 'inspect-source))
               (matched (delib-flow--run-stage-locally inspected 'match-project))
               (drafted
                (delib-flow--run-stage-locally matched 'suggest-reference-notes))
               (integrated
                (delib-flow--run-stage-locally drafted 'integrate-into-source))
               (selected
                (delib-flow--run-stage-locally integrated
                                               'select-approved-filing-actions))
               (updated-run
                (delib-flow--run-stage-locally selected 'file-approved-outputs))
               (location (car (plist-get (plist-get updated-run :filing)
                                         :target-locations))))
          (with-temp-buffer
            (insert-file-contents (plist-get location :target))
            (should (search-forward "#+filetags: :custom:general:" nil t))
            (should (search-forward "Source: Create general PKM note for Alpha Project kickoff"
                                    nil t))
            (should (search-forward "Type: general-pkm" nil t))))))))

(ert-deftest delib-flow-file-approved-outputs-updates-project-reference-files-for-support-notes ()
  (delib-flow-test--with-temp-zk-root ()
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((package
              (list :working-context
                    (list :project-match
                          (list :best-project
                                (list :title "Alpha Project")))
                    :filing
                    (list :approved-items
                          (list (list :kind 'reference-note
                                      :text "Create project support note from Alpha Constraints"
                                      :note-type 'project-support)))))
             (result (delib-flow--file-approved-outputs-result package))
             (locations (plist-get result :target-locations))
             (note-location (car locations))
             (metadata-location (cadr locations)))
        (should (equal 2 (length locations)))
        (should (file-exists-p (plist-get note-location :target)))
        (should (string-match-p "REFERENCE_FILES"
                                (plist-get metadata-location :target)))
        (with-temp-buffer
          (insert-file-contents delib-flow-my-projects-file)
          (should (search-forward ":REFERENCE_FILES:" nil t))
          (should (re-search-forward
                   "\\[\\[file:[^]]*alpha-constraints\\.org\\]\\[Alpha Constraints\\]\\]"
                   nil t)))))))

(ert-deftest delib-flow-file-approved-outputs-detects-project-child-conflict ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n** TODO Clarify the next step for Alpha Project kickoff\n"
    (let* ((package
            (list :working-context
                  (list :project-match
                        (list :best-project
                              (list :title "Alpha Project")))
                  :filing
                  (list :approved-items
                        (list (list :kind 'next-action
                                    :text "Clarify the next step for Alpha Project kickoff")))))
           (result (delib-flow--file-approved-outputs-result package))
           (conflicts (plist-get result :conflicts)))
      (should (equal 1 (plist-get result :conflict-count)))
      (should conflicts)
      (should-not (plist-get result :target-locations))
      (should (string-match-p "identical project child heading already exists"
                              (plist-get (car conflicts) :reason))))))

(ert-deftest delib-flow-file-approved-outputs-rerenders-filing-conflict-state ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha-project-kickoff.org"
          . "#+title: Alpha Project kickoff\n\nExisting note\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted
              (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (delib-flow--active-run
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-select-approved-filing-actions)
              (delib-flow-action-file-approved-outputs)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Filing conflicts" nil t))
                (should (search-forward "deterministic note target already exists"
                                        nil t))
                (goto-char (point-min))
                (should (search-forward "- File Approved Outputs [available]" nil t))
                (goto-char (point-min))
                (should (search-forward "** Approved artifacts" nil t))
                (should (search-forward "Create general PKM note for Alpha Project kickoff"
                                        nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-file-approved-outputs-command-rerenders-filing-preview ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (delib-flow--active-run
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-select-approved-filing-actions)
            (delib-flow-action-file-approved-outputs)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "** File Approved Outputs" nil t))
              (should (search-forward "Filed count:" nil t))
              (goto-char (point-min))
              (should (search-forward "** Filed target locations" nil t))
              (should (search-forward "Clarify the next step for Alpha Project kickoff" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- File Approved Outputs [available]" nil t))
              (goto-char (point-min))
              (should (search-forward "No approved artifacts are available yet." nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-file-approved-outputs-creates-new-project-from-proposal ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (integrated
            (delib-flow--run-stage-locally proposed 'integrate-into-source))
           (selected
            (delib-flow--run-stage-locally integrated
                                           'select-approved-filing-actions))
           (_updated-run
            (delib-flow--run-stage-locally selected 'file-approved-outputs)))
      (with-temp-buffer
        (insert-file-contents delib-flow-my-projects-file)
        (should (search-forward "* Completely Different Topic" nil t))
        (should (search-forward "** TODO Clarify the first concrete step for Completely Different Topic"
                                nil t))))))

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
        (should (search-forward "** Inspect Source" nil t))
        (should (search-forward "*** Input package" nil t))
        (should (search-forward "*** Raw output" nil t))))))

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
        (should (search-forward ":REVIEW_STATE: superseded" nil t))
        (should (search-forward ":REVIEW_STATE: pending-review" nil t))))))

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
              (delib-flow--run-stage-locally integrated
                                             'select-approved-filing-actions))
             (_updated-run
              (delib-flow--run-stage-locally selected 'file-approved-outputs)))
        (with-temp-buffer
          (insert-file-contents delib-flow-audit-log-file)
          (should (search-forward "** File Approved Outputs" nil t))
          (should (search-forward "Alpha Project" nil t))
          (should (search-forward "Clarify the next step for Alpha Project kickoff"
                                  nil t)))))))

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
              (should (search-forward "Audit log file: configured" nil t))
              (should (search-forward "Recorded stages: 1" nil t))
              (should (search-forward "Last appended checkpoint: inspect-source"
                                      nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-abort-run-appends-audit-status ()
  (delib-flow-test--with-temp-audit-file
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (delib-flow--active-run (delib-flow--run-stage-locally run
                                                                  'inspect-source))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-abort-run)
            (with-temp-buffer
              (insert-file-contents delib-flow-audit-log-file)
              (should (search-forward ":RUN_STATUS: aborted" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-placeholder-stage-action-errors ()
  (should-error (delib-flow-action-stage-placeholder)))

(ert-deftest delib-flow-stage-failure-updates-history ()
  (let* ((delib-flow-local-stage-adapter
          (lambda (_descriptor _package)
            (error "adapter failure")))
         (run (delib-flow--initialize-run (list :title "Example")))
         (updated-run (delib-flow--run-stage-locally run 'inspect-source))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (plist-get history :entries))))
    (should (equal 'failed (plist-get history :latest-status)))
    (should (equal 'failed (plist-get entry :status)))
    (should (string-match-p "adapter failure"
                            (plist-get entry :normalized-output)))
    (should (string-match-p "Review stage failure"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-abort-run-clears-active-run ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (with-current-buffer buffer
      (setq-local delib-flow--active-run-buffer t))
    (delib-flow-abort-run)
    (should (null delib-flow--active-run))
    (should-not (buffer-live-p buffer))))

(ert-deftest delib-flow-start-rejects-second-active-run ()
  (let ((delib-flow--active-run
         (delib-flow--initialize-run (list :title "Existing run"))))
    (with-current-buffer (get-buffer-create delib-flow-control-buffer-name)
      (setq-local delib-flow--active-run-buffer t))
    (unwind-protect
        (delib-flow-test--with-temp-org
         (insert "* New heading\n")
         (goto-char (point-min))
         (should-error (delib-flow-start)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(provide 'delib-flow-test)
;;; delib-flow-test.el ends here
