;;; delib-flow-debug-test.el --- Debug tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'delib-flow)

(defconst delib-flow-debug-test--local-test-config-file
  (expand-file-name "../delib-flow-local-test-config.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the repo-local local-LLM test config when available.")

(defmacro delib-flow-debug-test--with-local-test-config (&rest body)
  "Load the repo-local local test config, then run BODY.

Skip the test when the local config file is unavailable."
  (declare (indent 0))
  `(if (not (file-readable-p delib-flow-debug-test--local-test-config-file))
       (ert-skip "Local test config not available")
     (let ((delib-flow-my-projects-file nil)
           (delib-flow-zk-root nil)
           (delib-flow-prompt-library-file nil)
           (delib-flow-example-structures-file nil)
           (delib-flow-general-note-template nil)
           (delib-flow-project-support-note-template nil)
           (delib-flow-default-local-model nil)
           (delib-flow-default-cloud-model nil)
           (delib-flow-cloud-policy-profile nil)
           (delib-flow-cloud-provider-policy-alist nil)
           (delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
           (delib-flow-local-stage-async-adapter nil)
           (delib-flow-cloud-stage-adapter #'delib-flow--default-cloud-stage-adapter))
       (load-file delib-flow-debug-test--local-test-config-file)
       (let ((delib-flow-audit-log-file
              (make-temp-file "delib-flow-local-test-audit" nil ".org"))
             (delib-flow-local-test-llm-log-file
              (make-temp-file "delib-flow-local-test-llm" nil ".org")))
         (setq delib-flow--active-run nil)
         (when (boundp 'delib-flow-local-test--last-ollama-response)
           (setq delib-flow-local-test--last-ollama-response nil))
         (when (boundp 'delib-flow-local-test--last-ollama-json-fragment)
           (setq delib-flow-local-test--last-ollama-json-fragment nil))
         (when (boundp 'delib-flow-local-test--last-ollama-parse-error)
           (setq delib-flow-local-test--last-ollama-parse-error nil))
         (unwind-protect
             (progn ,@body)
           (setq delib-flow--active-run nil)
           (when (boundp 'delib-flow-local-test--last-ollama-response)
             (setq delib-flow-local-test--last-ollama-response nil))
           (when (boundp 'delib-flow-local-test--last-ollama-json-fragment)
             (setq delib-flow-local-test--last-ollama-json-fragment nil))
           (when (boundp 'delib-flow-local-test--last-ollama-parse-error)
             (setq delib-flow-local-test--last-ollama-parse-error nil))
           (when (file-exists-p delib-flow-audit-log-file)
             (delete-file delib-flow-audit-log-file))
           (when (file-exists-p delib-flow-local-test-llm-log-file)
             (delete-file delib-flow-local-test-llm-log-file)))))))

(ert-deftest delib-flow-debug-start-scenario-replays-to-artifact-checkpoint ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'artifact-ready)
          (should (equal "Alpha Project kickoff"
                         (plist-get (delib-flow--run-source delib-flow--active-run)
                                    :title)))
          (should (equal 'extract-actions
                         (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                    :stage-id)))
          (should (file-exists-p delib-flow-my-projects-file))
          (should (file-directory-p delib-flow-zk-root))
          (should (file-exists-p delib-flow-audit-log-file))
          (should (plist-get (plist-get delib-flow--active-run :filing)
                             :draft-items)))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-start-scenario-cleans-up-temp-fixtures-on-abort ()
  (let ((delib-flow-my-projects-file "/tmp/original-projects.org")
        (delib-flow-zk-root "/tmp/original-zk")
        (delib-flow-audit-log-file "/tmp/original-audit.org")
        fixture)
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'cloud-ready)
          (setq fixture (plist-get (delib-flow--run-session delib-flow--active-run)
                                   :debug-fixture))
          (should fixture)
          (should (equal 'approve-cloud-send
                         (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                    :stage-id)))
          (delib-flow-abort-run)
          (should (null delib-flow--active-run))
          (should (equal "/tmp/original-projects.org" delib-flow-my-projects-file))
          (should (equal "/tmp/original-zk" delib-flow-zk-root))
          (should (equal "/tmp/original-audit.org" delib-flow-audit-log-file))
          (should-not (file-exists-p (plist-get fixture :source-file)))
          (should-not (file-exists-p (plist-get fixture :projects-file)))
          (should-not (file-exists-p (plist-get fixture :audit-file)))
          (should-not (file-directory-p (plist-get fixture :zk-root))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-open-latest-stage-inspection-renders-package-and-output ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'artifact-ready)
          (delib-flow-debug-open-latest-stage-inspection)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "* Latest stage debug inspection" nil t))
            (should (search-forward "- Stage: Extract Actions" nil t))
            (should (search-forward "** State change summary" nil t))
            (should (search-forward "Draft artifacts: 0 -> 2" nil t))
            (should (search-forward "** Filing diagnostics" nil t))
            (should (search-forward "Draft preview head: Draft kickoff follow-up" nil t))
            (should (search-forward "** Input package" nil t))
            (should (search-forward "** Normalized output" nil t))
            (should (search-forward "** Raw output" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-open-comparison-renders-fresh-replay-summary ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'artifact-ready)
          (delib-flow-debug-open-comparison)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "* Debug comparison" nil t))
            (should (search-forward "** Fresh replay comparison" nil t))
            (should (search-forward "- Scenario: alpha-followup" nil t))
            (should (search-forward "- Checkpoint: artifact-ready" nil t))
            (should (search-forward "No differences detected across the tracked run snapshot." nil t))
            (should (search-forward "** Previous attempt comparison" nil t))
            (should (search-forward "No previous attempt exists for the latest stage." nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-open-comparison-renders-previous-attempt-summary ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'artifact-ready)
          (setq delib-flow--active-run
                (delib-flow--seed-actions
                 (delib-flow--run-stage-locally delib-flow--active-run
                                                'extract-actions)))
          (delib-flow-debug-open-comparison)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "** Fresh replay comparison" nil t))
            (should (search-forward "Stage history count: 5 -> 6" nil t))
            (should (search-forward "Audit stage count: 5 -> 6" nil t))
            (should (search-forward "** Previous attempt comparison" nil t))
            (should (search-forward "- Stage: Extract Actions" nil t))
            (should (search-forward "Attempt numbers: 1 -> 2" nil t))
            (should (search-forward "Input package changed: yes" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-start-walkthrough-loads-target-and-opens-guide ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'cloud-failure)
          (should (eq 'alpha-followup
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-scenario-id)))
          (should (eq 'cloud-failure-ready
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-checkpoint)))
          (should (eq 'cloud-failure
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-walkthrough-target-id)))
          (should (eq 'extract-actions
                      (plist-get (delib-flow--run-routing delib-flow--active-run)
                                 :cloud-failure-stage)))
          (with-current-buffer delib-flow-debug-walkthrough-buffer-name
            (goto-char (point-min))
            (should (search-forward "* Debug walkthrough" nil t))
            (should (search-forward "Walkthrough target: Cloud failure" nil t))
            (should (search-forward "Checkpoint: cloud-failure-ready" nil t))
            (should (search-forward "`D`: inspect the latest package, prompt, and output." nil t))
            (should (search-forward "** Current checkpoint verification" nil t))
            (should (search-forward "The failure should be recorded against Extract Actions" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-open-walkthrough-renders-target-recipes-without-active-run ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-open-walkthrough)
          (with-current-buffer delib-flow-debug-walkthrough-buffer-name
            (goto-char (point-min))
            (should (search-forward "* Debug walkthrough" nil t))
            (should (search-forward "No debug run is active." nil t))
            (should (search-forward "** Verification loop" nil t))
            (should (search-forward "*** Full run from source" nil t))
            (should (search-forward "Inspect Source should be the latest stage" nil t))
            (should (search-forward "*** Newsletter reference notes" nil t))
            (should (search-forward "*** Filing conflict" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-walkthrough-next-step-advances-checkpoint ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'full-run)
          (should (eq 'source
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-checkpoint)))
          (delib-flow-debug-walkthrough-next-step)
          (should (eq 'alpha-followup
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-scenario-id)))
          (should (eq 'full-run
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-walkthrough-target-id)))
          (should (eq 'inspect-reviewed
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-checkpoint)))
          (should (eq 'inspect-source
                      (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                 :stage-id)))
          (with-current-buffer delib-flow-debug-walkthrough-buffer-name
            (goto-char (point-min))
            (should (search-forward "Checkpoint: inspect-reviewed" nil t))
            (should (search-forward "Next checkpoint: Project reviewed" nil t))
            (should (search-forward "`N`: advance to the next walkthrough checkpoint." nil t))
            (should (search-forward "Inspect Source should be the latest stage" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-walkthrough-restart-target-returns-to-baseline ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'cloud-failure)
          (delib-flow-debug-walkthrough-restart-target)
          (should (eq 'cloud-failure
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-walkthrough-target-id)))
          (should (eq 'project-reviewed
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-checkpoint)))
          (should (eq 'match-project
                      (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                 :stage-id)))
          (with-current-buffer delib-flow-debug-walkthrough-buffer-name
            (goto-char (point-min))
            (should (search-forward "Walkthrough target: Cloud failure" nil t))
            (should (search-forward "Checkpoint: project-reviewed" nil t))
            (should (search-forward "Current decision: Walkthrough target cloud-failure restarted from its baseline checkpoint." nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-apply-helper-selects-first-manual-project-candidate ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'manual-project)
          (delib-flow-debug-apply-helper 'choose-first-candidate)
          (should (string-match-p "Selection: Alpha Project"
                                  (delib-flow--manual-project-selection-text
                                   delib-flow--active-run)))
          (should (string-match-p "Debug helper selected the first available candidate"
                                  (delib-flow--manual-project-selection-text
                                   delib-flow--active-run))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-apply-helper-sets-cloud-failure-resolution ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'cloud-failure)
          (delib-flow-debug-apply-helper 'use-local)
          (should (string-match-p "Resolution: USE-LOCAL"
                                  (delib-flow--editable-block-text
                                   (delib-flow--editable-block delib-flow--active-run
                                                               'cloud-failure-review))))
          (should (string-match-p "Debug helper prepared a use-local resolution"
                                  (plist-get (delib-flow--run-session delib-flow--active-run)
                                             :current-decision))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-apply-helper-builds-smart-filing-conflict-fix ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'filing-conflict)
          (delib-flow-debug-apply-helper 'smart-reword-item)
          (should (string-match-p "Resolution: REWORD-ITEM"
                                  (delib-flow--filing-conflict-resolution-block-text
                                   delib-flow--active-run)))
          (should (string-match-p "Debug helper reworded the approved artifact for retry"
                                  (delib-flow--filing-conflict-resolution-block-text
                                   delib-flow--active-run))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-apply-helper-selects-first-blocked-filing-artifact ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'filing-selection-mixed 'filing-ready)
          (delib-flow-debug-apply-helper 'select-first-blocked)
          (should (string-match-p "Selection: 2"
                                  (delib-flow--editable-block-text
                                   (delib-flow--editable-block delib-flow--active-run
                                                               'filing-selection-review))))
          (should (string-match-p "Debug helper selected the first blocked filing artifact"
                                  (plist-get (delib-flow--run-session delib-flow--active-run)
                                             :current-decision))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-open-latest-stage-inspection-renders-project-diagnostics ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'project-reviewed)
          (delib-flow-debug-open-latest-stage-inspection)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "** Project diagnostics" nil t))
            (should (search-forward "Match status: matched" nil t))
            (should (search-forward "Best project: Alpha Project" nil t))
            (should (search-forward "Candidate titles: Alpha Project" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-open-latest-stage-inspection-renders-cloud-diagnostics ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'cloud-ready)
          (delib-flow-debug-open-latest-stage-inspection)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "** Cloud diagnostics" nil t))
            (should (search-forward "Cloud target stage: Run Cloud Stage" nil t))
            (should (search-forward "Sanitization status: approved" nil t))
            (should (search-forward "Reintegration status: none" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-start-scenario-replays-to-manual-project-ready-checkpoint ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'manual-project-override
                                           'manual-project-ready)
          (should (eq 'match-project
                      (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                 :stage-id)))
          (should (eq 'ambiguous
                      (plist-get (plist-get (delib-flow--run-working-context
                                             delib-flow--active-run)
                                            :project-match)
                                 :match-status)))
          (should (member 'manual-project-match
                          (mapcar (lambda (action)
                                    (plist-get action :id))
                                  (plist-get (delib-flow--run-actions
                                              delib-flow--active-run)
                                             :items))))
          (should (string-match-p
                   "Alpha Project"
                   (delib-flow--editable-block-text
                    (delib-flow--editable-block delib-flow--active-run
                                                'manual-project-selection)))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-contact-disambiguates-replays-to-project-reviewed ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'contact-disambiguates
                                           'project-reviewed)
          (let* ((project-match
                  (plist-get (delib-flow--run-working-context delib-flow--active-run)
                             :project-match))
                 (best-project (plist-get project-match :best-project)))
            (should (eq 'match-project
                        (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                   :stage-id)))
            (should (eq 'matched
                        (plist-get project-match :match-status)))
            (should (equal "Alpha Project"
                           (plist-get best-project :title)))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-linked-note-disambiguates-replays-to-project-reviewed ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'linked-note-disambiguates
                                           'project-reviewed)
          (let* ((project-match
                  (plist-get (delib-flow--run-working-context delib-flow--active-run)
                             :project-match))
                 (best-project (plist-get project-match :best-project)))
            (should (eq 'match-project
                        (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                   :stage-id)))
            (should (eq 'matched
                        (plist-get project-match :match-status)))
            (should (equal "Beta Project"
                           (plist-get best-project :title)))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-no-match-clean-replays-to-manual-project-ready ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'no-match-clean
                                           'manual-project-ready)
          (let ((project-match
                 (plist-get (delib-flow--run-working-context delib-flow--active-run)
                            :project-match)))
            (should (eq 'match-project
                        (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                   :stage-id)))
            (should (eq 'no-match
                        (plist-get project-match :match-status)))
            (should (member 'manual-project-match
                            (mapcar (lambda (action)
                                      (plist-get action :id))
                                    (plist-get (delib-flow--run-actions
                                                delib-flow--active-run)
                                               :items))))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-action-rich-midpoint-seeds-strong-context ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'action-rich-midpoint 'context-ready)
          (let* ((working (delib-flow--run-working-context delib-flow--active-run))
                 (filtered (plist-get working :filtered-context))
                 (retained-context (plist-get working :retained-context)))
            (should (eq 'filter-reference-material
                        (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                   :stage-id)))
            (should (equal 2 (plist-get filtered :retained-count)))
            (should (string-match-p "draft the vendor follow-up email"
                                    retained-context))
            (should (string-match-p "staging access"
                                    retained-context))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-reference-note-rich-midpoint-seeds-newsletter-retained-notes ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'reference-note-rich-midpoint 'context-ready)
          (let* ((working (delib-flow--run-working-context delib-flow--active-run))
                 (retained (plist-get (plist-get working :filtered-context)
                                      :retained-candidates)))
            (should (equal '("Advisor briefs as reusable interfaces"
                             "Friction logs for AI systems"
                             "Prompt handoff packets")
                           (mapcar (lambda (candidate)
                                     (plist-get candidate :title))
                                   retained)))
            (should (string-match-p "friction logs"
                                    (plist-get working :retained-context)))
            (should (string-match-p "handoff packets"
                                    (plist-get working :retained-context)))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-reference-note-rich-midpoint-artifact-ready-runs-note-suggestions ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'reference-note-rich-midpoint 'artifact-ready)
          (let* ((latest (delib-flow--latest-stage-entry delib-flow--active-run))
                 (project-match (plist-get (delib-flow--run-working-context
                                            delib-flow--active-run)
                                           :project-match))
                 (draft-items (plist-get (plist-get delib-flow--active-run :filing)
                                         :draft-items))
                 (note-items (seq-filter (lambda (item)
                                           (eq (plist-get item :kind) 'reference-note))
                                         draft-items)))
            (should (eq 'suggest-reference-notes
                        (plist-get latest :stage-id)))
            (should (eq 'no-match
                        (plist-get project-match :match-status)))
            (should (< 1 (length note-items)))
            (should (seq-every-p (lambda (item)
                                   (eq (plist-get item :kind) 'reference-note))
                                 note-items))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-walkthrough-newsletter-reference-notes-loads-no-project-demo ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-walkthrough 'newsletter-reference-notes)
          (should (eq 'reference-note-rich-midpoint
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-scenario-id)))
          (should (eq 'inspect-reviewed
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-checkpoint)))
          (should (eq 'newsletter-reference-notes
                      (plist-get (delib-flow--run-session delib-flow--active-run)
                                 :debug-walkthrough-target-id)))
          (with-current-buffer delib-flow-debug-walkthrough-buffer-name
            (goto-char (point-min))
            (should (search-forward "Walkthrough target: Newsletter reference notes" nil t))
            (should (search-forward "Checkpoint: inspect-reviewed" nil t))
            (should (search-forward "accepted no-match result" nil t))
            (should (search-forward "advisor briefs, friction logs, and handoff packets" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-walkthrough-buffer-name))))))

(ert-deftest delib-flow-debug-start-scenario-filing-selection-mixed-seeds-mixed-draft-items ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'filing-selection-mixed 'artifact-ready)
          (let* ((filing (plist-get delib-flow--active-run :filing))
                 (draft-items (plist-get filing :draft-items)))
            (should (equal 4 (length draft-items)))
            (should (equal "Publish updated launch timeline"
                           (plist-get (nth 0 draft-items) :text)))
            (should (equal 'blocked
                           (delib-flow--draft-item-readiness
                            (nth 1 draft-items))))
            (should (equal 'ready
                           (delib-flow--draft-item-readiness
                            (nth 2 draft-items))))
            (should (equal 'warning
                           (delib-flow--draft-item-readiness
                            (nth 3 draft-items))))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-start-scenario-filing-note-conflict-replays-to-reference-note-conflict ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'filing-note-conflict
                                           'filing-conflict-ready)
          (let* ((filing (plist-get delib-flow--active-run :filing))
                 (approved (car (plist-get filing :approved-items)))
                 (conflict (car (plist-get filing :conflicts))))
            (should (equal 'reference-note (plist-get approved :kind)))
            (should (equal 'reference-note (plist-get conflict :kind)))
            (should (string-match-p "Migration support handoff"
                                    (plist-get conflict :item-text)))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-debug-open-latest-stage-inspection-renders-cloud-failure-diagnostics ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'alpha-followup 'cloud-failure-ready)
          (should (eq 'extract-actions
                      (plist-get (delib-flow--run-routing delib-flow--active-run)
                                 :cloud-failure-stage)))
          (delib-flow-debug-open-latest-stage-inspection)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "- Stage: Run Cloud Stage" nil t))
            (should (search-forward "** Cloud diagnostics" nil t))
            (should (search-forward "Cloud target stage: Extract Actions" nil t))
            (should (search-forward "Cloud failure stage: extract-actions" nil t))
            (should (search-forward "Cloud failure message: debug cloud failure" nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-open-latest-stage-inspection-renders-filing-conflict-diagnostics ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'filing-conflict 'filing-conflict-ready)
          (should (= 1
                     (length (plist-get (plist-get delib-flow--active-run :filing)
                                        :conflicts))))
          (delib-flow-debug-open-latest-stage-inspection)
          (with-current-buffer delib-flow-debug-buffer-name
            (goto-char (point-min))
            (should (search-forward "- Stage: File Approved Outputs" nil t))
            (should (search-forward "** Filing diagnostics" nil t))
            (should (search-forward "Approved artifacts: 1" nil t))
            (should (search-forward "Filing conflicts: 1" nil t))
            (should (search-forward "Draft kickoff follow-up"
                                    nil t))))
      (when delib-flow--active-run
        (delib-flow-abort-run))
      (when (buffer-live-p (get-buffer delib-flow-debug-buffer-name))
        (kill-buffer (get-buffer delib-flow-debug-buffer-name))))))

(ert-deftest delib-flow-debug-start-scenario-rejects-unsupported-checkpoint ()
  (let ((delib-flow--active-run nil))
    (should-error
     (delib-flow-debug-start-scenario 'manual-project-override 'artifact-ready)
     :type 'user-error)
    (should-not delib-flow--active-run)))

(ert-deftest delib-flow-debug-replay-to-checkpoint-uses-deterministic-adapter ()
  (let ((delib-flow--active-run nil)
        (delib-flow-local-stage-adapter
         (lambda (_descriptor _package)
           (error "debug replay should not use active local adapter"))))
    (unwind-protect
        (progn
          (delib-flow-debug-start-scenario 'filing-selection-mixed 'filing-ready)
          (should (eq 'integrate-into-source
                      (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                                 :stage-id)))
          (should (equal 4
                         (length (plist-get (plist-get delib-flow--active-run :filing)
                                            :draft-items)))))
      (when delib-flow--active-run
        (delib-flow-abort-run)))))

(ert-deftest delib-flow-local-test-start-debug-scenario-allows-later-checkpoints ()
  (delib-flow-debug-test--with-local-test-config
    (let (captured)
      (cl-letf (((symbol-function 'delib-flow-debug-start-scenario)
                 (lambda (scenario-id checkpoint)
                   (setq captured (list scenario-id
                                        checkpoint
                                        delib-flow-local-stage-adapter)))))
        (delib-flow-local-test-start-debug-scenario 'alpha-followup 'context-ready)
        (should (equal '(alpha-followup context-ready
                                         delib-flow--default-local-stage-adapter)
                       captured))))))

(ert-deftest delib-flow-local-test-start-context-scenario-loads-context-ready ()
  (delib-flow-debug-test--with-local-test-config
    (let (captured)
      (cl-letf (((symbol-function 'delib-flow-local-test-start-debug-scenario)
                 (lambda (scenario-id &optional checkpoint)
                   (setq captured (list scenario-id checkpoint)))))
        (delib-flow-local-test-start-context-scenario 'alpha-followup)
        (should (equal '(alpha-followup context-ready) captured))))))

(ert-deftest delib-flow-local-test-start-debug-scenario-keeps-ollama-adapter-at-source ()
  (delib-flow-debug-test--with-local-test-config
    (let (captured)
      (cl-letf (((symbol-function 'delib-flow-debug-start-scenario)
                 (lambda (scenario-id checkpoint)
                   (setq captured (list scenario-id
                                        checkpoint
                                        delib-flow-local-stage-adapter)))))
        (delib-flow-local-test-start-debug-scenario 'alpha-followup 'source)
        (should (equal '(alpha-followup source delib-flow-local-test-ollama-adapter)
                       captured))))))

(provide 'delib-flow-debug-test)

;;; delib-flow-debug-test.el ends here
