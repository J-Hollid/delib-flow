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

(ert-deftest delib-flow-snapshot-heading-captures-title-and-content ()
  (delib-flow-test--with-temp-org
   (insert "* Example heading\nSome body text.\n")
   (goto-char (point-min))
   (let ((snapshot (delib-flow--snapshot-heading)))
     (should (equal "Example heading" (plist-get snapshot :title)))
     (should (string-match-p "Some body text" (plist-get snapshot :content))))))

(ert-deftest delib-flow-make-editable-block-returns-expected-shape ()
  (let ((block (delib-flow--make-editable-block
                'context-main
                'context
                "Working context"
                '("Working context" "Editable working slice")
                "delib-edit-context-main")))
    (should (equal 'context-main (plist-get block :id)))
    (should (equal 'context (plist-get block :kind)))
    (should (equal "Working context" (plist-get block :section)))
    (should (equal 'clean (plist-get block :status)))
    (should (equal 'valid (plist-get block :validation-status)))))

(ert-deftest delib-flow-initial-section-anchors-contains-required-sections ()
  (let ((anchors (delib-flow--initial-section-anchors)))
    (dolist (section '(source
                       working-context
                       stage-history
                       current-decision
                       valid-next-actions
                       filing-preview
                       audit-status))
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
    (should (equal '(context-main operator-notes) block-ids))
    (should (assoc 'context-main blocks))
    (should (assoc 'operator-notes blocks))))

(ert-deftest delib-flow-initialize-run-seeds-section-anchors ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (anchors (plist-get (delib-flow--run-ui run) :section-anchors)))
    (dolist (section '(source
                       working-context
                       stage-history
                       current-decision
                       valid-next-actions
                       filing-preview
                       audit-status))
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
          (should (derived-mode-p 'org-mode))
          (should-not view-mode)
          (should-not buffer-read-only)
          (dolist (heading '("** Source"
                             "** Working context"
                             "** Stage history"
                             "** Current decision"
                             "** Valid next actions"
                             "** Filing preview"
                             "** Audit status"))
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
          (should (search-forward "- Refresh Buffer [available]" nil t))
          (should (search-forward "- Abort Run [available]" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-control-buffer-renders-editable-blocks ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "** Editable working slice" nil t))
          (should (search-forward "#+begin_delib-edit context" nil t))
          (should (search-forward "** Operator notes" nil t))
          (should (search-forward "#+begin_delib-edit notes" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-control-buffer-protects-managed-regions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "** Source")
          (should-error (insert "forbidden")))
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

(ert-deftest delib-flow-refresh-buffer-detects-managed-region-conflicts ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (search-forward "** Audit status")
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
         (working (delib-flow--run-working-context updated-run)))
    (should (equal 'inspect-source (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'inspect-source (plist-get entry :stage-id)))
    (should (plist-get working :inspect-output))
    (should (string-match-p "Body lines: 2"
                            (plist-get working :retained-context)))
    (should (equal '(inspect-source
                     match-project
                     discover-reference-material
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
            (should (search-forward "** Inspect Source" nil t))
            (should (search-forward "Body lines: 1" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Inspect Source [available]" nil t))
            (should (search-forward "- Match Project [available]" nil t))
            (should (search-forward "- Discover Relevant Reference Material [available]" nil t))
            (should (search-forward "- Decide on Cloud Pass [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

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
           (project-match (plist-get working :project-match)))
      (should (equal 'match-project (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'match-project (plist-get entry :stage-id)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal "Alpha Project"
                     (plist-get (plist-get project-match :best-project) :title)))
      (should (string-match-p "Review project match"
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
                                        :items)))))))

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
                       discover-reference-material
                       manual-project-match
                       propose-new-project
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items)))))))

(ert-deftest delib-flow-manual-project-match-updates-project-context ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (updated-run (delib-flow--run-stage-locally matched
                                                       'manual-project-match))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal 'manual (plist-get project-match :selection-method)))
      (should (equal "Alpha Project"
                     (plist-get (plist-get project-match :best-project) :title)))
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
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (delib-flow--active-run
            (delib-flow--run-stage-locally inspected 'match-project))
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

(ert-deftest delib-flow-propose-new-project-updates-stage-history-and-filing ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
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
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (delib-flow--active-run
            (delib-flow--run-stage-locally inspected 'match-project))
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
           (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-match-project)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "Project match: matched." nil t))
              (goto-char (point-min))
              (should (search-forward "** Match Project" nil t))
              (should (search-forward "Best project: Alpha Project" nil t))
              (goto-char (point-min))
              (should (search-forward "- Discover Relevant Reference Material [available]" nil t))
              (goto-char (point-min))
              (should (search-forward "- Extract Actions [available]" nil t))
              (should (search-forward "- Extract Waiting-For [available]" nil t))
              (should (search-forward "- Suggest Reference Notes [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-discover-reference-material-updates-stage-history-and-context ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (updated-run
            (delib-flow--run-stage-locally inspected
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
                                         :items)))))))

(ert-deftest delib-flow-discover-reference-material-command-rerenders-history ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("beta.org" . "#+title: Beta Notes\nUnrelated material.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-discover-reference-material)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "Retrieved context: available." nil t))
              (should (search-forward "Alpha Project Notes" nil t))
              (goto-char (point-min))
              (should (search-forward "** Discover Relevant Reference Material" nil t))
              (should (search-forward "- Candidate count: 1" nil t))
              (goto-char (point-min))
              (should (search-forward "- Filter Useful Reference Material [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

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
              (should (search-forward "Filtered context: available." nil t))
              (should (search-forward "Retained: 2. Rejected: 0." nil t))
              (should (search-forward "Alpha Project Notes" nil t))
              (goto-char (point-min))
              (should (search-forward "** Filter Useful Reference Material" nil t))
              (should (search-forward "- Retained count: 2" nil t))
              (goto-char (point-min))
              (should (search-forward "- Retry Filter Useful Reference Material [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-extract-actions-updates-stage-history-and-filing ()
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
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
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
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
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
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
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
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
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
    (should (member 'sanitize-for-cloud
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should (string-match-p "Review cloud-routing decision"
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
            (should (search-forward "** Decide on Cloud Pass" nil t))
            (should (search-forward "Selected model:" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-sanitized context: pending preparation." nil t))
            (should (search-forward "Cloud pass selected. Model:" nil t))
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
    (should (member 'run-cloud-stage
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should (string-match-p "Review sanitized cloud package"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

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
            (should (search-forward "** Sanitize for Cloud" nil t))
            (should (search-forward "Sanitization status: prepared" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-sanitized context: available." nil t))
            (should (search-forward "Sanitized source title:" nil t))
            (should (search-forward "[redacted-email]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Run Cloud Stage [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Sanitize for Cloud [available]" nil t))))
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
         (updated-run (delib-flow--run-stage-in-cloud sanitized
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
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-run-cloud-stage)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "** Run Cloud Stage" nil t))
            (should (search-forward "Cloud output for reviewed package" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-returned context: available." nil t))
            (should (search-forward "** Cloud-returned context" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Run Cloud Stage [available]" nil t))))
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
             (cloud-run
              (delib-flow--run-stage-in-cloud sanitized 'run-cloud-stage))
             (updated-run
              (delib-flow--run-stage-locally cloud-run 'integrate-into-source))
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
             (delib-flow--active-run
              (delib-flow--run-stage-in-cloud sanitized 'run-cloud-stage))
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
              (should (search-forward "** Run audit state" nil t))
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
