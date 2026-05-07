;;; delib-flow-test.el --- Tests for delib-flow -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-test-support)
(require 'delib-flow-architecture-test)
(require 'delib-flow-model-test)
(require 'delib-flow-artifacts-test)
(require 'delib-flow-services-test)
(require 'delib-flow-stages-test)
(require 'delib-flow-filing-test)
(require 'delib-flow-render-test)
(require 'delib-flow-ui-test)
(require 'delib-flow-audit-test)
(require 'delib-flow-debug-test)

(defvar jh/ollama-model nil)
(defvar jh/ollama-url nil)

;; Keep the suite off the operator's live audit log by default.
(setq delib-flow-audit-log-file
      (make-temp-file "delib-flow-test-default-audit" nil ".org"))

(defconst delib-flow-test--local-test-config-file
  (expand-file-name "../delib-flow-local-test-config.el"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the repo-local local-LLM test config when available.")

(defmacro delib-flow-test--with-local-test-config (&rest body)
  "Load the repo-local local test config, then run BODY.

Skip the test when the local config file is unavailable."
  (declare (indent 0))
  `(if (not (file-readable-p delib-flow-test--local-test-config-file))
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
       (load-file delib-flow-test--local-test-config-file)
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

(ert-deftest delib-flow-snapshot-heading-captures-title-and-content ()
  (delib-flow-test--with-temp-org
   (insert "* Example heading\nSome body text.\n")
   (goto-char (point-min))
   (let ((snapshot (delib-flow--snapshot-heading)))
     (should (equal "Example heading" (plist-get snapshot :title)))
     (should (string-match-p "Some body text" (plist-get snapshot :content))))))

(ert-deftest delib-flow-local-inspect-raw-output-rejects-datestamp-only-email-classification ()
  (delib-flow-test--with-local-test-config
    (let* ((source (list :title "[2026-02-24 Tue 22:37] Macro trenches"
                         :outline-path '("Inbox")
                         :content "* [2026-02-24 Tue 22:37] Macro trenches\nNeed to review the design notes.\n"))
           (raw-output
            (delib-flow-local-test--inspect-raw-output
             (list :source source)
             '(("source_type" . "email")
               ("source_type_reason" . "Datestamp and subject-like heading.")
               ("source_type_signals" . ["datestamp"])
               ("title" . "Macro trenches")))))
      (should-not (eq 'email (plist-get raw-output :source-type))))))

(ert-deftest delib-flow-set-filing-selection-value-syncs-selected-reference-note ()
  (let* ((note (list :kind 'reference-note
                     :text "Create general PKM note for Durable idea"
                     :note-type 'general-pkm
                     :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-candidates
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list note)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'reference-notes
                (list note))))
         (updated (delib-flow--set-filing-selection-value run "1"))
         (state (plist-get (plist-get updated :artifacts) 'reference-notes)))
    (should (equal (delib-flow--artifact-candidate-id note)
                   (plist-get state :selected-candidate-id)))
    (should-not (plist-get state :selected-draft))))

(ert-deftest delib-flow-set-filing-selection-value-clears-stale-selected-reference-note-draft ()
  (let* ((note-one (list :kind 'reference-note
                         :text "Create general PKM note for Durable idea"
                         :note-type 'general-pkm
                         :warnings nil))
         (note-two (list :kind 'reference-note
                         :text "Create general PKM note for Adjacent pattern"
                         :note-type 'general-pkm
                         :warnings nil))
         (drafted-one (plist-put (copy-tree note-one)
                                 :draft-body
                                 "* Working draft\nOld draft.\n"))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list note-one note-two)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'reference-notes
                (list :candidates (list note-one note-two)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id note-one)
                      :selected-draft drafted-one))))
         (updated (delib-flow--set-filing-selection-value run "2"))
         (state (plist-get (plist-get updated :artifacts) 'reference-notes)))
    (should (equal (delib-flow--artifact-candidate-id note-two)
                   (plist-get state :selected-candidate-id)))
    (should-not (plist-get state :selected-draft))))

(ert-deftest delib-flow-start-requires-heading-at-point ()
  (delib-flow-test--with-temp-org
   (insert "Not a heading\n")
   (goto-char (point-min))
   (should-error (delib-flow-start))))

(ert-deftest delib-flow-start-from-inbox-requires-configured-file ()
  (let ((delib-flow-inbox-file nil))
    (should-error (delib-flow-start-from-inbox))))

(ert-deftest delib-flow-start-from-inbox-starts-from-selected-top-level-heading ()
  (delib-flow-test--with-temp-org-file
      "* First item\nBody\n** Nested child\nNested body\n* Second item\nChosen body\n"
    (let* ((delib-flow-inbox-file (buffer-file-name))
           (picked nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (setq picked collection)
                   (cadr collection))))
        (delib-flow-start-from-inbox))
      (unwind-protect
          (progn
            (should (equal '("[1] First item" "[2] Second item") picked))
            (should delib-flow--active-run)
            (should (equal "Second item"
                           (plist-get (plist-get delib-flow--active-run :source) :title)))
            (should (string-match-p
                     (regexp-quote "* Second item\nChosen body")
                     (plist-get (plist-get delib-flow--active-run :source) :content))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))
        (setq delib-flow--active-run nil)))))

(ert-deftest delib-flow-start-from-inbox-starts-from-selected-outline-path-child ()
  (delib-flow-test--with-temp-org-file
      "* Inbox\n** First queued item\nBody\n** Second queued item\nChosen body\n* Elsewhere\n** Ignored child\n"
    (let* ((delib-flow-inbox-file (buffer-file-name))
           (delib-flow-inbox-outline-path '("Inbox"))
           (picked nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _args)
                   (setq picked collection)
                   (cadr collection))))
        (delib-flow-start-from-inbox))
      (unwind-protect
          (progn
            (should (equal '("[1] First queued item" "[2] Second queued item") picked))
            (should delib-flow--active-run)
            (should (equal "Second queued item"
                           (plist-get (plist-get delib-flow--active-run :source) :title))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))
        (setq delib-flow--active-run nil)))))


(ert-deftest delib-flow-control-mode-keybindings-include-public-commands ()
  (should (eq #'delib-flow-control-refresh
              (lookup-key delib-flow-control-mode-map (kbd "g"))))
  (should (eq #'delib-flow-control-next-section
              (lookup-key delib-flow-control-mode-map (kbd "n"))))
  (should (eq #'delib-flow-control-previous-section
              (lookup-key delib-flow-control-mode-map (kbd "p"))))
  (should (eq #'delib-flow-control-open-audit-run
              (lookup-key delib-flow-control-mode-map (kbd "j"))))
  (should (eq #'delib-flow-control-open-audit-latest-stage
              (lookup-key delib-flow-control-mode-map (kbd "J"))))
  (should (eq #'delib-flow-control-debug-open-latest-stage-inspection
              (lookup-key delib-flow-control-mode-map (kbd "D"))))
  (should (eq #'delib-flow-control-debug-open-comparison
              (lookup-key delib-flow-control-mode-map (kbd "C"))))
  (should (eq #'delib-flow-control-debug-open-walkthrough
              (lookup-key delib-flow-control-mode-map (kbd "W"))))
  (should (eq #'delib-flow-control-debug-apply-helper
              (lookup-key delib-flow-control-mode-map (kbd "H"))))
  (should (eq #'delib-flow-control-debug-walkthrough-next-step
              (lookup-key delib-flow-control-mode-map (kbd "N"))))
  (should (eq #'delib-flow-control-debug-walkthrough-restart-target
              (lookup-key delib-flow-control-mode-map (kbd "R"))))
  (should (eq #'delib-flow-control-dispatch-action
              (lookup-key delib-flow-control-mode-map (kbd "RET"))))
  (should (eq #'delib-flow-control-next-action
              (lookup-key delib-flow-control-mode-map (kbd "TAB"))))
  (should (eq #'delib-flow-control-previous-action
              (lookup-key delib-flow-control-mode-map (kbd "<backtab>"))))
  (should (eq #'delib-flow-control-dispatch-action
              (lookup-key delib-flow-control-mode-map (kbd "a"))))
  (should (eq #'delib-flow-control-context-menu
              (lookup-key delib-flow-control-mode-map (kbd "."))))
  (should (eq #'delib-flow-control-approve-current
              (lookup-key delib-flow-control-mode-map (kbd "A"))))
  (should (eq #'delib-flow-control-peek-selected-draft
              (lookup-key delib-flow-control-mode-map (kbd "E"))))
  (should (eq #'delib-flow-control-peek-selected-support
              (lookup-key delib-flow-control-mode-map (kbd "I"))))
  (should (eq #'delib-flow-control-peek-filing-target
              (lookup-key delib-flow-control-mode-map (kbd "P"))))
  (should (eq #'delib-flow-control-peek-staged-content
              (lookup-key delib-flow-control-mode-map (kbd "V"))))
  (should (eq #'delib-flow-control-jump-active-loop
              (lookup-key delib-flow-control-mode-map (kbd "L"))))
  (should (eq #'delib-flow-control-jump-latest-preview
              (lookup-key delib-flow-control-mode-map (kbd "K"))))
  (should (eq #'delib-flow-control-jump-stage-history
              (lookup-key delib-flow-control-mode-map (kbd "U"))))
  (should (eq #'delib-flow-control-choose-manual-project
              (lookup-key delib-flow-control-mode-map (kbd "m"))))
  (should (eq #'delib-flow-control-retry-current
              (lookup-key delib-flow-control-mode-map (kbd "r"))))
  (should (eq #'delib-flow-control-abort-run
              (lookup-key delib-flow-control-mode-map (kbd "q"))))
  (should (eq #'delib-flow-control-choose-filing-selection
              (lookup-key delib-flow-control-mode-map (kbd "s"))))
  (should (eq #'delib-flow-control-toggle-focus-mode
              (lookup-key delib-flow-control-mode-map (kbd "z"))))
  (should (eq #'delib-flow-control-dispatch-shortcut
              (lookup-key delib-flow-control-mode-map (kbd "1"))))
  (should (eq #'delib-flow-control-dispatch-shortcut
              (lookup-key delib-flow-control-mode-map (kbd "b"))))
  (should (eq #'delib-flow-control-help-command
              (lookup-key delib-flow-control-mode-map (kbd "?")))))

(ert-deftest delib-flow-context-menu-entries-include-local-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (labels (mapcar (lambda (entry) (plist-get entry :label))
                         (delib-flow--context-menu-entries run "Now"))))
    (should (member "Inspect Source" labels))
    (should (member "Jump to Active Loop" labels))
    (should (member "Jump to Latest Preview" labels))
    (should (member "Jump to Stage History" labels))
    (should (member "Enable Focus Mode" labels))
    (should (member "Refresh Buffer" labels))
    (should (member "Control Hints" labels))))

(ert-deftest delib-flow-context-menu-entries-for-filing-preview-include-filing-controls ()
  (let* ((run (delib-flow--seed-actions
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :file "/tmp/example.org"
                       :content "* Alpha Project kickoff\n- Draft kickoff follow-up"))
                'extract-actions)))
         (labels (mapcar (lambda (entry) (plist-get entry :label))
                         (delib-flow--context-menu-entries run "Filing preview"))))
    (should (member "Choose Filing Artifact" labels))
    (should (member "Peek Filing Target" labels))
    (should (member "Peek Staged Content" labels))
    (should (member "Enable Focus Mode" labels))))

(ert-deftest delib-flow-toggle-focus-mode-preserves-active-loop-visibility ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (setq delib-flow--active-run run)
          (should-not delib-flow-control-focus-mode)
          (delib-flow-toggle-focus-mode)
          (should delib-flow-control-focus-mode)
          (should (string-match-p "\\[focus\\]" (delib-flow--control-header-line)))
          (goto-char (point-min))
          (should (search-forward "** Now" nil t))
          (goto-char (point-min))
          (should (search-forward "** Next actions" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-toggle-focus-mode-errors-outside-control-buffer-and-toggles-off ()
  (should-error (delib-flow-toggle-focus-mode) :type 'user-error)
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq delib-flow--active-run run)
            (delib-flow-toggle-focus-mode)
            (should delib-flow-control-focus-mode)
            (delib-flow-toggle-focus-mode)
            (should-not delib-flow-control-focus-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-control-help-renders-current-section-and-local-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (setq delib-flow--active-run run)
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "** Now" nil t)
            (delib-flow-control-help))
          (with-current-buffer (help-buffer)
            (goto-char (point-min))
            (should (search-forward "Current section: Now" nil t))
            (should (search-forward "Recommended next pass: Inspect Source" nil t))
            (should (search-forward "Active loop: Next actions" nil t))
            (should (search-forward "Latest change: No stage result is available yet." nil t))
            (should (search-forward "Latest preview: Now > Decision strip" nil t))
            (should (search-forward "Current decision: Review working context and choose next action." nil t))
            (should (search-forward "Last consequence: No workflow consequence exists yet; start with Inspect Source." nil t))
            (should (search-forward "Local actions here:" nil t))
            (should (search-forward "Inspect Source: Run this workflow action." nil t))
            (goto-char (point-min))
            (should (search-forward "Use L to jump back to the active loop." nil t))
            (goto-char (point-min))
            (should (search-forward "Use K to jump to the latest preview or result." nil t))
            (goto-char (point-min))
            (should (search-forward "Use U to jump to the latest stage history details." nil t))
            (should (search-forward ".            Open the local context menu for the current section." nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-context-menu-current-result-entries-accept-run-argument ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (entries (delib-flow--context-menu-current-result-entries run))
         (labels (mapcar (lambda (entry) (plist-get entry :label)) entries)))
    (should (member "Open Debug Comparison" labels))
    (should (member "Open Latest Stage Inspection" labels))))

(ert-deftest delib-flow-jump-active-loop-goes-to-artifact-selection ()
  (let* ((run (delib-flow--seed-actions
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :file "/tmp/example.org"
                       :content "* Alpha Project kickoff\n- Draft kickoff follow-up"))
                'extract-actions)))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "** Now" nil t))
          (delib-flow-jump-active-loop)
          (with-current-buffer buffer
            (should (equal "Artifact selection"
                           (save-excursion
                             (org-back-to-heading t)
                             (org-get-heading t t t t))))
            (let ((window (get-buffer-window buffer t)))
              (should window)
              (should (= (window-start window)
                         (save-excursion
                           (org-back-to-heading t)
                           (line-beginning-position)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-jump-latest-preview-goes-to-current-result-without-targets ()
  (let* ((run (delib-flow--seed-actions
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :file "/tmp/example.org"
                       :content "* Alpha Project kickoff\n- Draft kickoff follow-up"))
                'extract-actions)))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "** Now" nil t))
          (delib-flow-jump-latest-preview)
          (with-current-buffer buffer
            (should (equal "Current result"
                           (save-excursion
                             (org-back-to-heading t)
                             (org-get-heading t t t t))))
            (let ((window (get-buffer-window buffer t)))
              (should window)
              (should (= (window-start window)
                         (save-excursion
                           (org-back-to-heading t)
                           (line-beginning-position)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-jump-stage-history-goes-to-stage-history-heading ()
  (let* ((run (delib-flow--run-stage-locally
               (delib-flow--initialize-run (list :title "Example"))
               'inspect-source))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "** Now" nil t))
          (delib-flow-jump-stage-history)
          (with-current-buffer buffer
            (should (equal "Stage history"
                           (save-excursion
                             (org-back-to-heading t)
                             (org-get-heading t t t t))))
            (let ((window (get-buffer-window buffer t)))
              (should window)
              (should (= (window-start window)
                         (save-excursion
                           (org-back-to-heading t)
                           (line-beginning-position)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-recommended-action-prefers-accept-inspect-over-retry ()
  (let* ((run (delib-flow--run-stage-locally
               (delib-flow--initialize-run
                (list :title "Example"
                      :content "* Example\nBody line\n"))
               'inspect-source))
         (recommended (delib-flow--recommended-action run)))
    (should (eq (plist-get recommended :id)
                'accept-inspect-source))))

(ert-deftest delib-flow-recommended-action-prefers-match-project-after-accepted-inspect ()
  (let* ((run (delib-flow-test--accept-inspect
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n"))
                'inspect-source)))
         (recommended (delib-flow--recommended-action run)))
    (should (eq (plist-get recommended :id)
                'match-project))))

(ert-deftest delib-flow-discover-reference-material-action-available-after-accepted-inspect-without-project ()
  (let* ((run (delib-flow-test--accept-inspect
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n"))
                'inspect-source)))
         (action (delib-flow--discover-reference-material-action run)))
    (should action)
    (should (eq 'discover-reference-material
                (plist-get action :id)))))

(ert-deftest delib-flow-suggest-reference-notes-action-available-after-accepted-inspect-without-project ()
  (let* ((run (delib-flow-test--accept-inspect
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Presentation idea"
                       :content "* Presentation idea\n- Focus on the end user.\n"))
                'inspect-source)))
         (action (delib-flow--suggest-reference-notes-action run)))
    (should action)
    (should (eq 'suggest-reference-notes (plist-get action :id)))))

(ert-deftest delib-flow-propose-new-project-action-available-after-accepted-ambiguous-match ()
  (let* ((run (delib-flow-test--accept-inspect
               (delib-flow--run-stage-locally
                (delib-flow--initialize-run
                 (list :title "Presentation idea"
                       :content "* Presentation idea\n- Focus on the end user.\n"))
                'inspect-source)))
         (ambiguous-match
          '(:match-status ambiguous
            :best-project nil
            :candidates ((:title "Alpha Project")
                         (:title "Beta Project"))
            :reason "Evidence is split across plausible projects."))
         (run (plist-put run :working-context
                         (plist-put (delib-flow--run-working-context run)
                                    :project-match
                                    ambiguous-match)))
         (run (delib-flow-test--accept-match run))
         (action (delib-flow--propose-new-project-action run)))
    (should action)
    (should (eq 'propose-new-project (plist-get action :id)))))

(ert-deftest delib-flow-rerender-highlights-latest-changed-heading ()
  (let* ((run (delib-flow--run-stage-locally
               (delib-flow--initialize-run (list :title "Example"))
               'inspect-source))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow--rerender-active-run-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (let ((overlays (overlays-at (delib-flow--heading-position "Loop update"))))
              (should (seq-some (lambda (overlay)
                                  (overlay-get overlay 'delib-flow-changed-heading))
                                overlays)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name)))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-snapshot-heading-strips-text-properties ()
  (delib-flow-test--with-temp-org
   (insert (propertize "* Example heading\nBody line.\n" 'face 'bold))
   (goto-char (point-min))
   (let ((snapshot (delib-flow--snapshot-heading)))
     (should-not (text-properties-at 1 (plist-get snapshot :title)))
     (should-not (text-properties-at 1 (plist-get snapshot :content))))))

(ert-deftest delib-flow-control-hotkeys-still-dispatch-at-read-only-editable-blocks ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run))
         refreshed)
    (unwind-protect
        (cl-letf (((symbol-function 'delib-flow-refresh)
                   (lambda ()
                     (interactive)
                     (setq refreshed t))))
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "#+begin_delib-edit context")
            (forward-line 1)
            (let ((last-command-event ?g))
              (call-interactively (key-binding (kbd "g"))))
            (should refreshed)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-control-buffer-killed-clears-active-run ()
  (let ((delib-flow--active-run (list :session (list :status 'active))))
    (with-temp-buffer
      (setq-local delib-flow--active-run-buffer t)
      (delib-flow--control-buffer-killed))
    (should (null delib-flow--active-run))))

(ert-deftest delib-flow-control-help-renders-keybinding-help ()
  (let ((buffer (save-window-excursion
                  (delib-flow-control-help)
                  (help-buffer))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (should (search-forward "DeliberateFlow control hints" nil t))
      (goto-char (point-min))
      (should (search-forward "Use n/p to move between cockpit sections." nil t))
      (goto-char (point-min))
      (should (search-forward "Use TAB/S-TAB to move between rendered actions." nil t))
      (goto-char (point-min))
      (should (search-forward "Use . to open the local context menu." nil t))
      (goto-char (point-min))
      (should (search-forward "Action keys are shown inline beside each rendered option." nil t))
      (goto-char (point-min))
      (should (search-forward "Refresh the control buffer." nil t))
      (goto-char (point-min))
      (should (search-forward "Open completion for manual project selection." nil t))
      (goto-char (point-min))
      (should (search-forward "Open completion for filing artifact selection." nil t))
      (goto-char (point-min))
      (should (search-forward "Show the full control-key help." nil t)))))

(ert-deftest delib-flow-refresh-buffer-errors-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-refresh-buffer))))

(ert-deftest delib-flow-refresh-delegates-to-refresh-buffer ()
  (let (called)
    (cl-letf (((symbol-function 'delib-flow-refresh-buffer)
               (lambda ()
                 (setq called t))))
      (delib-flow-refresh))
    (should called)))

(ert-deftest delib-flow-control-revert-delegates-to-refresh-buffer ()
  (let* ((delib-flow--active-run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run))
         called)
    (unwind-protect
        (with-current-buffer buffer
          (cl-letf (((symbol-function 'delib-flow-refresh-buffer)
                     (lambda ()
                       (setq called t))))
            (funcall revert-buffer-function nil t))
          (should called))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-abort-run-errors-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-abort-run))))

(ert-deftest delib-flow-refresh-buffer-rerenders-active-run ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run
          (delib-flow-test--set-operator-intent run "Retained edit"))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
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

(ert-deftest delib-flow-refresh-buffer-preserves-current-subheading-anchor ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (drafted
            (delib-flow--run-stage-locally
             (delib-flow--run-stage-locally
              (delib-flow--run-stage-locally run 'inspect-source)
              'match-project)
             'extract-actions))
           (delib-flow--active-run drafted)
           (buffer (delib-flow--render-control-buffer drafted)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t)
              (goto-char (point-min))
              (search-forward "*** Artifact selection" nil t)
              (org-back-to-heading t))
            (delib-flow-refresh-buffer)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (org-back-to-heading t)
              (should (equal "Artifact selection"
                             (org-get-heading t t t t)))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

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
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (search-forward "#+begin_delib-edit cloud-review")
            (forward-line 1)
            (insert "Operator note"))
          (let* ((updated-run (delib-flow--sync-editable-blocks run buffer))
                 (block (delib-flow--editable-block updated-run
                                                    'cloud-package-review)))
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
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected
            (delib-flow-test--set-filing-selection integrated "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (filed-run
            (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (history (delib-flow--run-stage-history filed-run))
           (stage-ids (mapcar (lambda (entry) (plist-get entry :stage-id))
                              (plist-get history :entries)))
           (locations (plist-get (plist-get filed-run :filing)
                                 :target-locations)))
      (should (equal '(inspect-source
                       match-project
                       extract-actions
                       integrate-into-source
                       draft-selected-action
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
            (delib-flow--run-stage-locally
             (delib-flow-test--set-filing-selection integrated "1")
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
                     edit-operator-intent
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
            (org-back-to-heading t)
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

(ert-deftest delib-flow-action-shortcut-executes-rendered-next-action ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Next actions")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t)
            (let ((last-command-event ?1))
              (call-interactively (key-binding (kbd "1")))))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))
            (goto-char (point-min))
            (should (search-forward "- Stage: Inspect Source" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-next-section-moves-to-following-top-level-section ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Now")))
    (unwind-protect
        (save-window-excursion
          (pop-to-buffer buffer)
          (with-current-buffer buffer
            (should (equal "Now" (delib-flow--current-section-at-point)))
            (delib-flow-next-section)
            (should (equal "Current result" (delib-flow--current-section-at-point)))
            (let ((window (get-buffer-window buffer t)))
              (should window)
              (should (= (window-start window)
                         (save-excursion
                           (org-back-to-heading t)
                           (line-beginning-position)))))
            (delib-flow-previous-section)
            (should (equal "Now" (delib-flow--current-section-at-point)))
            (let ((window (get-buffer-window buffer t)))
              (should window)
              (should (= (window-start window)
                         (save-excursion
                           (org-back-to-heading t)
                           (line-beginning-position)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-next-action-moves-between-rendered-actions ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Now")))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "- Inspect Source [available]" nil t))
          (goto-char (line-beginning-position))
          (let ((first-point (point))
                (first-action (plist-get (get-text-property (point) 'delib-flow-action) :id)))
            (should (eq 'inspect-source first-action))
            (delib-flow-next-action)
            (should-not (= (point) first-point))
            (should (get-text-property (point) 'delib-flow-action))
            (delib-flow-previous-action)
            (should (= (point) first-point))
            (let ((returned-action
                   (plist-get (get-text-property (point) 'delib-flow-action) :id)))
              (should (eq 'inspect-source returned-action))))
          )
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-local-test-inspect-raw-output-filters-hallucinated-emails-and-keeps-person-entity ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-file-var
        source-file "delib-flow-source" ".org"
        "* Alpha Project kickoff\nFrom: alice@example.com\nSubject: Alpha Project kickoff\n\nNext steps:\n- Draft kickoff follow-up\n- Prepare timeline update\nWaiting for Bob to confirm the launch date.\n[[file:notes/alpha-brief.org][Alpha brief]]\n"
      (let* ((package (list :source
                            (list :title "Alpha Project kickoff"
                                  :file source-file
                                  :content (with-temp-buffer
                                             (insert-file-contents source-file)
                                             (buffer-string))
                                  :outline-path '("Alpha Project kickoff"))))
             (parsed '((source_type . "email")
                       (source_type_reason . "Headers present")
                       (source_type_signals "From:" "Subject:")
                       (title . "Alpha Project kickoff")
                       (outline_path "Alpha Project kickoff")
                       (contact_emails "alice@example.com" "bob@example.com")
                       (org_file_links "file:notes/alpha-brief.org")
                       (summary . "Waiting on Bob.")
                       (entities)
                       (blockers "Waiting for Bob to confirm the launch date.")))
             (raw (delib-flow-local-test--inspect-raw-output package parsed)))
        (should (equal 'email (plist-get raw :source-type)))
        (should (equal '("alice@example.com")
                       (plist-get raw :contact-emails)))
        (should (member "Bob"
                        (plist-get (plist-get raw :analysis) :entities)))))))

(ert-deftest delib-flow-local-test-inspect-raw-output-rejects-hallucinated-email-headers ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-file-var
        source-file "delib-flow-source" ".org"
        "* Open questions from note review\nNeed to reconcile the support note with the current project state.\nPlease review the open blockers and dependencies.\n[[file:notes/beta-brief.org][Beta brief]]\n"
      (let* ((package (list :source
                            (list :title "Open questions from note review"
                                  :file source-file
                                  :content (with-temp-buffer
                                             (insert-file-contents source-file)
                                             (buffer-string))
                                  :outline-path '("Open questions from note review"))))
             (parsed '((source_type . "email")
                       (source_type_reason . "presence of email-style headers and forwarded-message markers")
                       (source_type_signals "From:" "To:" "Subject:")
                       (title . "Open questions from note review")
                       (outline_path "Open questions from note review")
                       (org_file_links "file:notes/beta-brief.org")
                       (summary . "Review blockers and dependencies.")
                       (questions "open questions")
                       (blockers "open blockers")))
             (raw (delib-flow-local-test--inspect-raw-output package parsed)))
        (should (eq 'fleeting-note (plist-get raw :source-type)))
        (should-not (member "From:" (plist-get raw :source-type-signals)))
        (should (member "keyword: question" (plist-get raw :source-type-signals)))
        (should-not (string-match-p "email-style headers"
                                    (or (plist-get raw :source-type-reason) "")))))))

(ert-deftest delib-flow-local-test-inspect-raw-output-accepts-reminder-source-type ()
  (delib-flow-test--with-local-test-config
    (let* ((source
            (list :title "Reminder: follow up on taxes"
                  :content "* Reminder: follow up on taxes\nRemember to send the accountant the missing form.\n"))
           (package (list :source source))
           (parsed '((source_type . "reminder")
                     (source_type_reason . "Explicit reminder wording is present.")
                     (source_type_signals "reminder" "remember to")
                     (title . "Reminder: follow up on taxes")
                     (outline_path)
                     (contact_emails)
                     (org_file_links)
                     (body_preview . "Remember to send the accountant the missing form.")))
           (raw (delib-flow-local-test--inspect-raw-output package parsed)))
      (should (eq 'reminder (plist-get raw :source-type))))))

(ert-deftest delib-flow-local-test-inspect-raw-output-promotes-unknown-to-fleeting-note-fallback ()
  (delib-flow-test--with-local-test-config
    (let* ((source
            (list :title "Idea: orders system for commanders"
                  :content "* Idea: orders system for commanders\nMaybe structure command intent around a lightweight doctrine card.\n"))
           (package (list :source source))
           (parsed '((source_type . "unknown")
                     (source_type_reason . "No clear type.")
                     (source_type_signals)
                     (title . "Idea: orders system for commanders")
                     (outline_path)
                     (contact_emails)
                     (org_file_links)))
           (raw (delib-flow-local-test--inspect-raw-output package parsed)))
      (should (eq 'fleeting-note (plist-get raw :source-type)))
      (should (string-match-p "ideas, thoughts, or open questions"
                              (plist-get raw :source-type-reason))))))

(ert-deftest delib-flow-local-test-inspect-raw-output-replaces-formatting-only-fleeting-note-cues ()
  (delib-flow-test--with-local-test-config
    (let* ((source
            (list :title "[2026-02-24 Tue 22:37] Macro trenches"
                  :content "* [2026-02-24 Tue 22:37] Macro trenches\n8-bit dots fighting each other.\nDefine MVP, up to 8-players.\nAs macro as possible is the USP.\n"))
           (package (list :source source))
           (parsed '((source_type . "fleeting-note")
                     (source_type_reason . "timestamp and bolded title format")
                     (source_type_signals "[2026-02-24 Tue 22:37]" "**")
                     (title . "[2026-02-24 Tue 22:37] Macro trenches")
                     (outline_path "Inbox > [2026-02-24 Tue 22:37] Macro trenches")
                     (contact_emails)
                     (org_file_links)))
           (raw (delib-flow-local-test--inspect-raw-output package parsed)))
      (should (eq 'fleeting-note (plist-get raw :source-type)))
      (should-not (equal '("[2026-02-24 Tue 22:37]" "**")
                         (plist-get raw :source-type-signals)))
      (should-not (string-match-p "timestamp and bolded title format"
                                  (plist-get raw :source-type-reason)))
      (should (equal '("Inbox" "[2026-02-24 Tue 22:37] Macro trenches")
                     (plist-get raw :outline-path))))))

(ert-deftest delib-flow-local-test-inspect-raw-output-prefers-issue-note-fallback-over-fleeting-note-drift ()
  (delib-flow-test--with-local-test-config
    (let* ((source
            (list :title "[2026-02-10 Tue 17:52] Broken steno exercise"
                  :outline-path '("Inbox" "[2026-02-10 Tue 17:52] Broken steno exercise")
                  :content
                  (concat
                   "** [2026-02-10 Tue 17:52] Broken steno exercise\n"
                   "https://joshuagrams.github.io/steno-jig/finger-drills.html?iterations=20&hints=fail&live_wpm=1&show_timer=1&section=15&drill=13\n"
                   "https://joshuagrams.github.io/steno-jig/finger-drills.html?iterations=20&hints=fail&live_wpm=1&show_timer=1&section=16&drill=2\n")))
           (package (list :source source))
           (parsed '((source_type . "fleeting-note")
                     (source_type_reason . "timestamped heading with steno exercise links")
                     (source_type_signals "[2026-02-10 Tue 17:52]" "steno exercise links")
                     (title . "[2026-02-10 Tue 17:52] Broken steno exercise")
                     (outline_path "Inbox > [2026-02-10 Tue 17:52] Broken steno exercise")
                     (contact_emails)
                     (org_file_links)))
           (raw (delib-flow-local-test--inspect-raw-output package parsed)))
      (should (eq 'issue-note (plist-get raw :source-type)))
      (should (string-match-p "problem-to-fix capture"
                              (plist-get raw :source-type-reason)))
      (should (member "keyword: broken" (plist-get raw :source-type-signals))))))

(ert-deftest delib-flow-local-test-inspect-prompt-includes-issue-note-taxonomy ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "[2026-02-10 Tue 17:52] Broken steno exercise"
                        :outline-path '("Inbox")
                        :content
                        "* [2026-02-10 Tue 17:52] Broken steno exercise\nhttps://example.com/drill?id=one\n")))
           (prompt (delib-flow-local-test--inspect-prompt package)))
      (should (string-match-p
               "\"source_type\": \"email|meeting-note|fleeting-note|issue-note|reminder|unknown\""
               prompt))
      (should (string-match-p
               "Use `issue-note` for quick problem-to-fix captures, bug reports"
               prompt))
      (should-not (string-match-p "drill or exercise links" prompt))
      (should-not (string-match-p "website items" prompt)))))

(ert-deftest delib-flow-local-test-inspect-raw-output-preserves-id-links-in-summary ()
  (delib-flow-test--with-local-test-config
    (let* ((source
            (list :title "Presentation or article on focusing on the end user"
                  :content "* Presentation or article on focusing on the end user\nSee [[id:abc-123][Alpha Note]] and [[id:def-456][Beta Note]].\n"))
           (package (list :source source))
           (parsed '((source_type . "fleeting-note")
                     (source_type_reason . "freeform idea capture")
                     (source_type_signals "idea")
                     (title . "Presentation or article on focusing on the end user")
                     (outline_path "Inbox > Presentation or article on focusing on the end user")
                     (contact_emails)
                     (org_file_links . ["[[id:abc-123][Alpha Note]]"
                                        "[[id:def-456][Beta Note]]"])
                     (summary . "Reflect on end-user-focused presentation ideas.")))
           (raw (delib-flow-local-test--inspect-raw-output package parsed))
           (summary (plist-get (plist-get raw :analysis) :summary)))
      (should (equal '("id:abc-123" "id:def-456")
                     (plist-get raw :org-file-links)))
      (should (string-match-p "Referenced materials: id:abc-123, id:def-456\\."
                              summary))
      (should-not (string-match-p "/home/.*/\\[\\[id:" summary)))))

(ert-deftest delib-flow-local-test-inspect-raw-output-ignores-email-file-link-in-summary ()
  (delib-flow-test--with-local-test-config
    (let* ((source
            (list :title "Your Consumption Diet Is Your Moat"
                  :content (concat
                            "* Your Consumption Diet Is Your Moat :email:\n"
                            ":PROPERTIES:\n"
                            ":EMAIL_FILE: /tmp/example-mail.txt\n"
                            ":END:\n"
                            "[[file:/tmp/example-mail.txt][Raw email file]]\n"
                            "Body.\n")))
           (package (list :source source))
           (parsed '((source_type . "email")
                     (source_type_reason . "headers present")
                     (source_type_signals "From:" "Subject:")
                     (title . "Your Consumption Diet Is Your Moat")
                     (outline_path "Inbox > Your Consumption Diet Is Your Moat")
                     (contact_emails "hello@fortelabs.com")
                     (org_file_links . ["file:/tmp/example-mail.txt"])
                     (summary . "Useful summary.")))
           (raw (delib-flow-local-test--inspect-raw-output package parsed))
           (summary (plist-get (plist-get raw :analysis) :summary)))
      (should-not (plist-get raw :org-file-links))
      (should-not (string-match-p "Referenced materials:" summary))
      (should-not (string-match-p "/tmp/example-mail.txt" summary)))))

(ert-deftest delib-flow-valid-source-type-override-accepts-new-note-types ()
  (should (delib-flow--valid-source-type-override-p "fleeting-note"))
  (should (delib-flow--valid-source-type-override-p "issue-note"))
  (should (delib-flow--valid-source-type-override-p "reminder")))

(ert-deftest delib-flow-local-adapter-extract-actions-preserves-operator-intent ()
  (delib-flow-test--with-local-test-config
    (let* ((run (delib-flow--set-operator-intent-directly
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content "* Broken steno exercise\nhttps://example.com/drill?id=one\n"))
                 "I need to figure out how exercises are actually stored on the website"))
           (package (delib-flow--stage-input-package run 'extract-actions))
           (descriptor (delib-flow--stage-descriptor 'extract-actions))
           (prompt (delib-flow-local-test--artifact-prompt package 'actions))
           (raw-output (funcall delib-flow-local-stage-adapter descriptor package)))
      (should (string-match-p
               "Operator intent: I need to figure out how exercises are actually stored on the website"
               prompt))
      (should (equal
               "I need to figure out how exercises are actually stored on the website"
               (plist-get raw-output :operator-intent))))))

(ert-deftest delib-flow-local-test-canonicalize-org-file-links-strips-file-scheme-and-dedupes ()
  (delib-flow-test--with-local-test-config
    (let* ((base-dir "/tmp/")
           (links '("file:notes/alpha-brief.org"
                    "notes/alpha-brief.org"
                    "/tmp/notes/alpha-brief.org"))
           (canonical
            (delib-flow-local-test--canonicalize-org-file-links links base-dir)))
      (should (equal '("/tmp/notes/alpha-brief.org") canonical)))))

(ert-deftest delib-flow-local-test-canonicalize-org-file-links-preserves-id-links ()
  (delib-flow-test--with-local-test-config
    (let* ((base-dir "/tmp/")
           (links '("[[id:abc-123][Linked note]]"
                    "id:abc-123"
                    "[[file:notes/alpha-brief.org][Alpha brief]]"))
           (canonical
            (delib-flow-local-test--canonicalize-org-file-links links base-dir)))
      (should (equal '("id:abc-123"
                       "/tmp/notes/alpha-brief.org")
                     canonical)))))

(ert-deftest delib-flow-local-test-canonicalize-org-file-links-preserves-https-links ()
  (delib-flow-test--with-local-test-config
    (let* ((base-dir "/tmp/")
           (links '("https://example.com/path"
                    "[[https://example.com/path][Example]]"
                    "[[file:notes/alpha-brief.org][Alpha brief]]"))
           (canonical
            (delib-flow-local-test--canonicalize-org-file-links links base-dir)))
      (should (equal '("https://example.com/path"
                       "/tmp/notes/alpha-brief.org")
                     canonical)))))

(ert-deftest delib-flow-local-test-ollama-json-unwraps-nested-envelope ()
  (delib-flow-test--with-local-test-config
    (let ((jh/ollama-model "llama3")
          (jh/ollama-url "http://localhost:11434/api/generate"))
      (cl-letf (((symbol-function 'delib-flow-local-test--http-post-json)
                 (lambda (_payload)
                   "{\"model\":\"llama3\",\"response\":\"{\\\"source_type\\\":\\\"email\\\",\\\"entities\\\":[\\\"Bob\\\"]}\"}")))
        (let ((parsed (delib-flow-local-test--ollama-json "prompt" 'inspect-source)))
          (should (equal "email"
                         (delib-flow-local-test--json-value parsed 'source_type)))
          (should (equal '("Bob")
                         (delib-flow-local-test--string-list
                          (delib-flow-local-test--json-value
                           parsed
                           'entities)))))))))

(ert-deftest delib-flow-local-test-ollama-json-unwraps-nested-envelope-array ()
  (delib-flow-test--with-local-test-config
    (let ((jh/ollama-model "llama3")
          (jh/ollama-url "http://localhost:11434/api/generate"))
      (cl-letf (((symbol-function 'delib-flow-local-test--http-post-json)
                 (lambda (_payload)
                   "{\"model\":\"llama3\",\"response\":\"[\\\"Draft vendor follow-up email\\\",\\\"Publish updated launch timeline\\\"]\"}")))
        (let ((parsed (delib-flow-local-test--ollama-json "prompt" 'extract-actions)))
          (should (equal '("Draft vendor follow-up email"
                           "Publish updated launch timeline")
                         (delib-flow-local-test--json-string-list parsed))))))))

(ert-deftest delib-flow-local-test-inspect-source-falls-back-on-parse-error ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Random note"
                        :content "* Random note\nNeed to think about this later.\n")
                  :working-context
                  (list :editable-block-ids nil)
                  :ui
                  (list :editable-blocks nil)))
           (descriptor (delib-flow--stage-descriptor 'inspect-source))
           raw)
      (cl-letf (((symbol-function 'delib-flow-local-test--ollama-json)
                 (lambda (_prompt _stage)
                   (signal 'json-readtable-error '(46)))))
        (setq raw (delib-flow-local-test--run-ollama-stage descriptor package)))
      (should (eq 'unknown (plist-get raw :source-type)))
      (should (= 1 (plist-get raw :body-line-count)))
      (should (string-match-p "too weak"
                              (plist-get raw :source-type-reason))))))

(ert-deftest delib-flow-local-test-inspect-prompt-uses-email-digest-for-imported-mail ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Feel The Snow from your Steam wishlist is now on sale!"
                        :outline-path '("Inbox")
                        :content
                        (concat
                         "* Feel The Snow from your Steam wishlist is now on sale! :email:\n"
                         ":PROPERTIES:\n"
                         ":FROM: Steam <noreply@steampowered.com>\n"
                         ":DATE: Sat, 02 May 2026 10:36:13 -0700\n"
                         ":EMAIL_FILE: /tmp/steam-mail.txt\n"
                         ":END:\n\n"
                         ":RAW_EMAIL:\n"
                         "From: Steam <noreply@steampowered.com>\n"
                         "To: user@example.com\n"
                         "Subject: Feel The Snow from your Steam wishlist is now on sale!\n"
                         "Content-Type: text/plain; charset=UTF-8\n\n"
                         "Hello CyanDevil!\n"
                         "The following items on your wishlist are on sale:\n"
                         "https://store.steampowered.com/app/538100/Feel_The_Snow/\n"
                         "This email message was auto-generated. Please do not respond.\n"
                         ":END:\n"))
                  :working-context
                  (list :editable-block-ids nil)
                  :ui
                  (list :editable-blocks nil)))
           (prompt (delib-flow-local-test--inspect-prompt package)))
      (should (string-match-p "deterministic email digest" prompt))
      (should (string-match-p "Type hint: transactional notification" prompt))
      (should (string-match-p "Plain-text body:" prompt))
      (should (string-match-p "Hello CyanDevil!" prompt))
      (should (string-match-p "Ignored noise removed:" prompt))
      (should-not (string-match-p "Content-Type: text/plain; charset=UTF-8" prompt))
      (should-not (string-match-p "This email message was auto-generated" prompt))
      (should-not (string-match-p ":RAW_EMAIL:" prompt)))))

(ert-deftest delib-flow-local-test-match-prompt-uses-email-digest-for-imported-mail ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((content
              (concat
               "** TODO Feel The Snow from your Steam wishlist is now on sale! :email:\n"
               ":PROPERTIES:\n"
               ":FROM: Steam <noreply@steampowered.com>\n"
               ":DATE: Sat, 02 May 2026 10:36:13 -0700\n"
               ":EMAIL_FILE: /tmp/steam-mail.txt\n"
               ":END:\n\n"
               ":RAW_EMAIL:\n"
               "From: Steam <noreply@steampowered.com>\n"
               "To: user@example.com\n"
               "Subject: Feel The Snow from your Steam wishlist is now on sale!\n"
               "Content-Type: text/plain; charset=UTF-8\n\n"
               "Hello CyanDevil!\n"
               "The following items on your wishlist are on sale:\n"
               "https://store.steampowered.com/app/538100/Feel_The_Snow/\n"
               "This email message was auto-generated. Please do not respond.\n"
               ":END:\n"))
             (package
              (list :source
                    (list :title "Feel The Snow from your Steam wishlist is now on sale!"
                          :content content)
                    :working-context
                    (list :inspect-output
                          '(:source-type email
                            :analysis (:summary "Wishlist sale notification."))
                          :editable-block-ids nil)
                    :ui
                    (list :editable-blocks nil)))
             (prompt (delib-flow-local-test--match-prompt package)))
        (should (string-match-p "deterministic email digest" prompt))
        (should (string-match-p "transactional notifications such as wishlist alerts" prompt))
        (should (string-match-p "\"type_hint\":\"transactional notification\"" prompt))
        (should-not (string-match-p ":RAW_EMAIL:" prompt))))))

(ert-deftest delib-flow-local-test-inspect-source-async-falls-back-on-parse-error ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Random note"
                        :content "* Random note\nNeed to think about this later.\n")
                  :working-context
                  (list :editable-block-ids nil)
                  :ui
                  (list :editable-blocks nil)))
           (descriptor (delib-flow--stage-descriptor 'inspect-source))
           success
           failure)
      (cl-letf (((symbol-function 'delib-flow-local-test--ensure-ollama-ready) #'ignore)
                ((symbol-function 'delib-flow-local-test--ollama-json-async)
                 (lambda (_prompt _stage on-success on-error)
                   (declare (ignore on-success))
                   (funcall on-error
                            "Ollama envelope response for delib-flow-inspect-source contained no nested JSON object"))))
        (delib-flow-local-test-ollama-async-adapter
         descriptor
         package
         (lambda (raw) (setq success raw))
         (lambda (message) (setq failure message))))
      (should success)
      (should-not failure)
      (should (eq 'unknown (plist-get success :source-type)))
      (should (= 1 (plist-get success :body-line-count)))
      (should (string-match-p "too weak"
                              (plist-get success :source-type-reason))))))

(ert-deftest delib-flow-local-test-artifact-prompt-stays-compact-and-grounded ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Alpha Project kickoff"
                        :content "* Alpha Project kickoff\n- Draft kickoff follow-up\n- Prepare timeline update\n")
                  :working-context
                  (list :project-match
                        (list :best-project (list :title "Alpha Project"))
                        :filtered-context
                        (list :retained-candidates
                              (list (list :title "Alpha brief"
                                          :file nil
                                          :score 5
                                          :reasons '("retained-by-score-threshold"))))
                        :retained-context
                        "- Alpha brief: blocker is waiting on launch confirmation.\n- Alpha brief: timeline update is still needed.")
                  :ui
                  (list :editable-blocks
                        '((context-main :id context-main
                                        :current-text ""
                                        :accepted-text ""
                                        :validation-status valid)))))
           (prompt (delib-flow-local-test--artifact-prompt package 'actions)))
      (should (string-match-p "Source title: Alpha Project kickoff" prompt))
      (should (string-match-p "Matched project: Alpha Project" prompt))
      (should (string-match-p "Retained context:" prompt))
      (should (string-match-p "Deterministic grounding hints:" prompt))
      (should-not (string-match-p "editable-blocks" prompt))
      (should-not (string-match-p "section-anchors" prompt)))))

(ert-deftest delib-flow-local-test-extract-actions-falls-back-on-parse-error ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Alpha Project kickoff"
                        :content "* Alpha Project kickoff\n- Draft kickoff follow-up\n- Prepare timeline update\n")
                  :working-context
                  (list :project-match
                        (list :best-project (list :title "Alpha Project"))
                        :filtered-context
                        (list :retained-candidates
                              (list (list :title "Alpha brief"
                                          :file nil
                                          :score 5
                                          :reasons '("retained-by-score-threshold"))))
                        :retained-context
                        "- Alpha brief: timeline update is still needed.")
                  :ui
                  (list :editable-blocks nil)))
           (descriptor (delib-flow--stage-descriptor 'extract-actions))
           raw)
      (cl-letf (((symbol-function 'delib-flow-local-test--ollama-json)
                 (lambda (_prompt _stage)
                   (signal 'json-readtable-error '(46)))))
        (setq raw (delib-flow-local-test--run-ollama-stage descriptor package)))
      (should (equal 2 (plist-get raw :candidate-count)))
      (should (equal "Draft kickoff follow-up"
                     (plist-get (car (plist-get raw :actions)) :text))))))

(ert-deftest delib-flow-local-test-extract-actions-accepts-bare-item-array ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Q3 launch coordination"
                        :content "* Q3 launch coordination\nNeed an operator pass over launch prep before the external update goes out.\nPlease confirm what still needs to ship this week.\n")
                  :working-context
                  (list :project-match
                        (list :best-project (list :title "Launch Project"))
                        :filtered-context
                        (list :retained-candidates nil)
                        :retained-context
                        "- Concrete next step: draft the vendor follow-up email and publish the updated launch timeline.\n- Open blocker: staging access for the vendor is still pending.")
                  :ui
                  (list :editable-blocks nil)))
           (descriptor (delib-flow--stage-descriptor 'extract-actions))
           raw)
      (cl-letf (((symbol-function 'delib-flow-local-test--ollama-json)
                 (lambda (_prompt _stage)
                   ["Draft vendor follow-up email"
                    "Publish updated launch timeline"
                    "Request staging access for vendor"])))
        (setq raw (delib-flow-local-test--run-ollama-stage descriptor package)))
      (should (equal 3 (plist-get raw :candidate-count)))
      (should (equal "Draft vendor follow-up email"
                     (plist-get (nth 0 (plist-get raw :actions)) :text)))
      (should (equal "Publish updated launch timeline"
                     (plist-get (nth 1 (plist-get raw :actions)) :text)))
      (should (equal "Request staging access for vendor"
                     (plist-get (nth 2 (plist-get raw :actions)) :text))))))

(ert-deftest delib-flow-local-test-reference-note-prompt-stays-compact-and-grounded ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Migration review synthesis"
                        :content "* Migration review synthesis\nCapture the decisions, open questions, and support material that should become durable reference notes for the migration effort.\n")
                  :working-context
                  (list :project-match
                        (list :best-project (list :title "Migration Project"))
                        :filtered-context
                        (list :retained-candidates nil)
                        :retained-context
                        "- Durable decision: keep the staged cutover approach.\n- Durable decision: retire the legacy sync job after validation.\n- Open question: document the rollback checks required for on-call.\n- Support note candidate: capture the migration support handoff for the runbook.")
                  :ui
                  (list :editable-blocks '((context-main :id context-main))))) 
           (prompt (delib-flow-local-test--reference-note-prompt package)))
      (should (string-match-p "Source title: Migration review synthesis" prompt))
      (should (string-match-p "Matched project: Migration Project" prompt))
      (should (string-match-p "Retained context:" prompt))
      (should (string-match-p "Deterministic grounding hints:" prompt))
      (should-not (string-match-p "Package summary:" prompt))
      (should-not (string-match-p ":editable-blocks" prompt))
      (should-not (string-match-p ":section-anchors" prompt)))))

(ert-deftest delib-flow-local-test-reference-note-prompt-uses-email-digest-for-transactional-mail ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Feel The Snow from your Steam wishlist is now on sale!"
                        :content
                        (concat
                         "** TODO Feel The Snow from your Steam wishlist is now on sale! :email:\n"
                         ":PROPERTIES:\n"
                         ":FROM: Steam <noreply@steampowered.com>\n"
                         ":DATE: Sat, 02 May 2026 10:36:13 -0700\n"
                         ":EMAIL_FILE: /tmp/steam-mail.txt\n"
                         ":END:\n\n"
                         ":RAW_EMAIL:\n"
                         "From: Steam <noreply@steampowered.com>\n"
                         "To: user@example.com\n"
                         "Subject: Feel The Snow from your Steam wishlist is now on sale!\n"
                         "Content-Type: text/plain; charset=UTF-8\n\n"
                         "Hello CyanDevil!\n"
                         "The following items on your wishlist are on sale:\n"
                         "Feel The Snow - 90% off!\n"
                         "This email message was auto-generated. Please do not respond.\n"
                         ":END:\n"))
                  :working-context
                  (list :inspect-output
                        '(:source-type email
                          :analysis (:summary "Wishlist sale notification."))
                        :filtered-context nil
                        :retained-context "")
                  :ui
                  (list :editable-blocks nil)))
           (prompt (delib-flow-local-test--reference-note-prompt package)))
      (should (string-match-p "Email type hint: transactional notification" prompt))
      (should (string-match-p "Reduced source body:" prompt))
      (should (string-match-p "Hello CyanDevil!" prompt))
      (should-not (string-match-p ":RAW_EMAIL:" prompt))
      (should-not (string-match-p "This email message was auto-generated" prompt)))))

(ert-deftest delib-flow-local-test-suggest-reference-notes-accepts-bare-note-array ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Migration review synthesis"
                        :content "* Migration review synthesis\nCapture the decisions, open questions, and support material that should become durable reference notes for the migration effort.\n")
                  :working-context
                  (list :project-match
                        (list :best-project (list :title "Migration Project"))
                        :filtered-context
                        (list :retained-candidates nil)
                        :retained-context
                        "- Durable decision: keep the staged cutover approach.\n- Support note candidate: capture the migration support handoff for the runbook.")
                  :ui
                  (list :editable-blocks nil)))
           (descriptor (delib-flow--stage-descriptor 'suggest-reference-notes))
           raw)
      (cl-letf (((symbol-function 'delib-flow-local-test--ollama-json)
                 (lambda (_prompt _stage)
                   [((title_or_focus . "Migration decisions")
                     (note_type . "general-pkm"))
                    ((title_or_focus . "Migration support handoff")
                     (note_type . "project-support"))])))
        (setq raw (delib-flow-local-test--run-ollama-stage descriptor package)))
      (let ((texts (mapcar (lambda (item) (plist-get item :text))
                           (plist-get raw :reference-notes))))
        (should (equal 3 (plist-get raw :candidate-count)))
        (should (member "Create general PKM note for Migration decisions" texts))
        (should (member "Create project support note from Migration support handoff" texts))
        (should (member "Create project support note from Migration review synthesis" texts))))))

(ert-deftest delib-flow-local-test-reference-note-items-fall-back-from-weak-focus ()
  (delib-flow-test--with-local-test-config
    (let* ((fallback-items
            (list (delib-flow--make-draft-reference-note
                   "Create general PKM note for Rollback checklist pattern"
                   'deterministic
                   'general-pkm)
                  (delib-flow--make-draft-reference-note
                   "Create project support note from Vendor support handoff"
                   'deterministic
                   'project-support)))
           (package (list :source (list :title "Example")
                          :working-context
                          (list :project-match
                                (list :best-project
                                      (list :title "Example Project")))))
           (parsed
            [((title_or_focus . "Next steps reminder")
              (note_type . "general-pkm"))
             ((title_or_focus . "Project kickoff note")
              (note_type . "project-support"))])
           items)
      (cl-letf (((symbol-function 'delib-flow--suggest-reference-notes-result)
                 (lambda (_package)
                   (list :reference-notes fallback-items))))
        (setq items (delib-flow-local-test--reference-note-items parsed package)))
      (should (equal "Create general PKM note for Rollback checklist pattern"
                     (plist-get (nth 0 items) :text)))
      (should (equal "Create project support note from Vendor support handoff"
                     (plist-get (nth 1 items) :text))))))

(ert-deftest delib-flow-local-test-reference-note-items-drop-transactional-general-pkm-notes ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source
                  (list :title "Feel The Snow from your Steam wishlist is now on sale!"
                        :content
                        (concat
                         "** TODO Feel The Snow from your Steam wishlist is now on sale! :email:\n"
                         ":PROPERTIES:\n"
                         ":FROM: Steam <noreply@steampowered.com>\n"
                         ":DATE: Sat, 02 May 2026 10:36:13 -0700\n"
                         ":END:\n\n"
                         ":RAW_EMAIL:\n"
                         "From: Steam <noreply@steampowered.com>\n"
                         "Subject: Feel The Snow from your Steam wishlist is now on sale!\n\n"
                         "Hello CyanDevil!\n"
                         "The following items on your wishlist are on sale:\n"
                         "This email message was auto-generated. Please do not respond.\n"
                         ":END:\n"))
                  :working-context
                  (list :inspect-output
                        '(:source-type email
                          :analysis (:summary "Wishlist sale notification."))
                        :filtered-context nil
                        :retained-context "")
                  :ui
                  (list :editable-blocks nil)))
           (parsed [((title_or_focus . "Feel The Snow")
                     (note_type . "general-pkm"))])
           items)
      (cl-letf (((symbol-function 'delib-flow--suggest-reference-notes-result)
                 (lambda (_package)
                   (list :reference-notes nil))))
        (setq items (delib-flow-local-test--reference-note-items parsed package)))
      (should-not items))))

(ert-deftest delib-flow-suggest-reference-notes-result-derives-multiple-email-note-focuses ()
  (let* ((package
          (list :source
                (list :title "Your Consumption Diet Is Your Moat"
                      :content
                      (concat
                       "** Your Consumption Diet Is Your Moat :email:\n"
                       ":PROPERTIES:\n"
                       ":FROM: Forte Labs Newsletter <hello@fortelabs.com>\n"
                       ":DATE: Tue, 28 Apr 2026 16:02:44 +0000\n"
                       ":END:\n\n"
                       ":RAW_EMAIL:\n"
                       "From: Forte Labs Newsletter <hello@fortelabs.com>\n"
                       "Subject: Your Consumption Diet Is Your Moat\n\n"
                       "A quick update on week 2 of the AI Second Brain cohort.\n"
                       "Week 1 was about laying the foundation: the Master Prompt, PARA adapted for the AI era, and a capture system.\n"
                       "This week, we covered a lot of ground across three sessions, but the one I want to highlight here is building personal AI advisors.\n"
                       "----------------------------------\n"
                       "Your Consumption Diet Is Your Moat\n"
                       "----------------------------------\n"
                       ":END:\n"))
                :working-context
                (list :project-match nil)))
         (result (delib-flow--suggest-reference-notes-result package))
         (items (plist-get result :reference-notes))
         (titles (mapcar #'delib-flow--reference-note-title items)))
    (should-not (member "Your Consumption Diet Is Your Moat" titles))
    (should (member "Building personal AI advisors" titles))
    (should (member "Laying the foundation: the Master Prompt, PARA adapted for the AI era, and a capture system"
                    titles))
    (should
     (equal
      "Captures `Building personal AI advisors` as a repeatable pattern that can inform later tool, workflow, or agent design."
      (plist-get
       (seq-find
        (lambda (item)
          (equal "Building personal AI advisors"
                 (delib-flow--reference-note-title item)))
        items)
       :reuse-claim)))))

(ert-deftest delib-flow-suggest-reference-notes-result-promotes-overlapping-final-items ()
  (let* ((strong (list :kind 'reference-note
                       :text "Create general PKM note for Building personal AI advisors for team workflows"
                       :note-type 'general-pkm
                       :candidate-focus "Building personal AI advisors for team workflows"
                       :candidate-identity "Building personal AI advisors for team workflows"
                       :focus-score 12
                       :reuse-claim "Captures the advisor pattern as reusable."))
         (weak (list :kind 'reference-note
                     :text "Create general PKM note for Building personal AI advisors"
                     :note-type 'general-pkm
                     :candidate-focus "Building personal AI advisors"
                     :candidate-identity "Building personal AI advisors"
                     :focus-score 8))
         (distinct (list :kind 'reference-note
                         :text "Create general PKM note for Laying the foundation: the Master Prompt"
                         :note-type 'general-pkm
                         :candidate-focus "Laying the foundation: the Master Prompt"
                         :candidate-identity "Laying the foundation: the Master Prompt"
                         :focus-score 10
                         :reuse-claim "Captures the setup pattern as reusable."))
         (package (list :source (list :title "Source"
                                      :content "* Source\nBody line\n")
                        :working-context nil)))
    (cl-letf (((symbol-function 'delib-flow--source-reference-notes)
               (lambda (_package) (list weak strong distinct)))
              ((symbol-function 'delib-flow--retained-candidate-reference-notes)
               (lambda (_package) nil)))
      (let* ((result (delib-flow--suggest-reference-notes-result package))
             (items (plist-get result :reference-notes))
             (titles (mapcar #'delib-flow--reference-note-title items)))
        (should (equal 2 (length items)))
        (should (member "Building personal AI advisors for team workflows" titles))
        (should (member "Laying the foundation: the Master Prompt" titles))
        (should-not (member "Building personal AI advisors" titles))))))

(ert-deftest delib-flow-local-test-reference-note-items-merge-deterministic-fallback-seeds ()
  (delib-flow-test--with-local-test-config
    (let* ((fallback-items
            (list
             (delib-flow--make-draft-reference-note
              "Create general PKM note for Your Consumption Diet Is Your Moat"
              'source
              'general-pkm)
             (delib-flow--make-draft-reference-note
              "Create general PKM note for Building personal AI advisors"
              'source
              'general-pkm)))
           (package (list :source (list :title "Example")))
           (parsed [((title_or_focus . "AI Second Brain cohort")
                     (note_type . "general-pkm"))])
           items
           titles)
      (cl-letf (((symbol-function 'delib-flow--suggest-reference-notes-result)
                 (lambda (_package)
                   (list :reference-notes fallback-items))))
        (setq items (delib-flow-local-test--reference-note-items parsed package))
        (setq titles (mapcar #'delib-flow--reference-note-title items)))
      (should (member "Your Consumption Diet Is Your Moat" titles))
      (should (member "Building personal AI advisors" titles))
      (should-not (member "AI Second Brain cohort" titles)))))

(ert-deftest delib-flow-local-test-reference-note-items-suppress-project-support-without-project ()
  (delib-flow-test--with-local-test-config
    (let* ((fallback-items
            (list
             (delib-flow--make-draft-reference-note
              "Create general PKM note for Durable pattern"
              'source
              'general-pkm)
             (delib-flow--make-draft-reference-note
              "Create project support note from Retained blocker"
              'retained-context
              'project-support)))
           (package (list :source (list :title "Example")
                          :working-context (list :project-match nil)))
           items
           texts)
      (cl-letf (((symbol-function 'delib-flow--suggest-reference-notes-result)
                 (lambda (_package)
                   (list :reference-notes fallback-items))))
        (setq items (delib-flow-local-test--reference-note-items nil package))
        (setq texts (mapcar (lambda (item) (plist-get item :text)) items)))
      (should (member "Create general PKM note for Durable pattern" texts))
      (should-not (seq-some
                   (lambda (text)
                     (string-match-p "project support note" text))
                   texts)))))

(ert-deftest delib-flow-local-test-reference-note-items-dedupe-near-duplicate-focuses ()
  (delib-flow-test--with-local-test-config
    (let* ((fallback-items
            (list
             (delib-flow--make-draft-reference-note
              "Create general PKM note for Laying the foundation: the Master Prompt, PARA adapted for the AI era, and a capture system"
              'source
              'general-pkm)
             (delib-flow--make-draft-reference-note
              "Create general PKM note for Building personal AI advisors with defined roles and scopes"
              'source
              'general-pkm)))
           (package (list :source (list :title "Example")
                          :working-context (list :project-match nil)))
           (parsed
            [((title_or_focus . "Laying the foundation: the Master Prompt, PARA")
              (note_type . "general-pkm"))
             ((title_or_focus . "Building personal AI")
              (note_type . "general-pkm"))])
           items
           titles)
      (cl-letf (((symbol-function 'delib-flow--suggest-reference-notes-result)
                 (lambda (_package)
                   (list :reference-notes fallback-items))))
        (setq items (delib-flow-local-test--reference-note-items parsed package))
        (setq titles (mapcar #'delib-flow--reference-note-title items)))
      (should (member
               "Laying the foundation: the Master Prompt, PARA adapted for the AI era, and a capture system"
               titles))
      (should (member
               "Building personal AI advisors with defined roles and scopes"
               titles))
      (should-not (member
                   "Laying the foundation: the Master Prompt, PARA"
                   titles))
      (should-not (member
                   "Building personal AI"
                   titles)))))

(ert-deftest delib-flow-local-test-reference-note-prompt-includes-deterministic-note-seeds ()
  (delib-flow-test--with-local-test-config
    (let* ((package
            (list :source (list :title "Example" :content "Body")
                  :working-context (list :project-match nil
                                         :filtered-context nil
                                         :retained-context "")))
           prompt)
      (cl-letf (((symbol-function 'delib-flow--suggest-reference-notes-result)
                 (lambda (_package)
                   (list :reference-notes
                         (list
                          (delib-flow--make-draft-reference-note
                           "Create general PKM note for Your Consumption Diet Is Your Moat"
                           'source
                           'general-pkm)
                          (delib-flow--make-draft-reference-note
                           "Create general PKM note for Building personal AI advisors"
                           'source
                           'general-pkm))))))
        (setq prompt (delib-flow-local-test--reference-note-prompt package)))
      (should (string-match-p
               (regexp-quote
                "- Create general PKM note for Your Consumption Diet Is Your Moat [general-pkm]")
               prompt))
      (should (string-match-p
               (regexp-quote
                "- Create general PKM note for Building personal AI advisors [general-pkm]")
               prompt)))))

(ert-deftest delib-flow-local-test-select-approved-filing-actions-stays-operator-owned ()
  (delib-flow-test--with-local-test-config
    (let* ((run (delib-flow--seed-filing-selection-block
                 (plist-put
                  (delib-flow--initialize-run
                   (list :title "Launch filing triage"
                         :content "* Launch filing triage\nReview the launch follow-up queue.\n"))
                  :filing
                  (list :draft-items
                        (list
                         (delib-flow--make-draft-action
                          "Publish updated launch timeline"
                          'debug-midpoint-fixture)
                         (delib-flow--draft-item-with-warnings
                          (delib-flow--make-draft-action
                           "Keep Friday launch target"
                           'debug-midpoint-fixture)
                          (list
                           (delib-flow--make-artifact-warning
                            'decision-state-action
                            "Reads like a decision or status statement rather than a directly executable next action."
                            'blocking))))
                        :approved-items nil
                        :rejected-items nil
                        :preview-text nil
                        :selection-blocked-item nil
                        :selection-blocking-warnings nil
                        :selection-blocked-selection nil
                        :selection-blocked-notes nil
                        :conflicts nil
                        :target-locations nil))))
           (selected
            (delib-flow-test--set-filing-selection run "1"))
           (drafted-selected
            (delib-flow--set-artifact-family-selected-draft
             selected
             'actions
             (plist-put
              (copy-tree (car (plist-get (plist-get selected :filing) :draft-items)))
              :text "Publish updated launch timeline")))
           (package
            (delib-flow--stage-input-package
             drafted-selected
             'select-approved-filing-actions))
           (descriptor (delib-flow--stage-descriptor 'select-approved-filing-actions))
           called
           raw)
      (cl-letf (((symbol-function 'delib-flow-local-test--http-post-json)
                 (lambda (_payload)
                   (setq called t)
                   (error "Should not call Ollama for operator-owned selection"))))
        (setq raw (delib-flow-local-test-ollama-adapter descriptor package)))
      (should-not called)
      (should (equal 1 (plist-get raw :selected-count)))
      (should (equal "Publish updated launch timeline"
                     (plist-get (car (plist-get raw :approved-items)) :text))))))

(ert-deftest delib-flow-local-test-file-approved-outputs-falls-back-after-prose-envelope ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-zk-root ()
      (delib-flow-test--with-temp-project-file
          "* Launch Project\n"
        (let* ((package
                (list :source
                      (list :title "Launch filing triage"
                            :content "* Launch filing triage\nReview the launch follow-up queue.\n")
                      :working-context
                      (list :project-match
                            (list :best-project
                                  (list :title "Launch Project")))
                      :filing
                      (list :approved-items
                            (list (list :kind 'reference-note
                                        :text "Create project support note from Vendor support handoff"
                                        :source 'debug-midpoint-fixture
                                        :note-type 'project-support
                                        :warnings nil)))))
               (descriptor (delib-flow--stage-descriptor 'file-approved-outputs))
               raw)
          (cl-letf (((symbol-function 'delib-flow-local-test--http-post-json)
                     (lambda (_payload)
                       (concat
                        "{\"model\":\"llama3\",\"response\":"
                        "\"The file-approved-outputs stage result for delib-flow is as follows:\\n\\n"
                        "The approved count is 1, indicating that one artifact has been approved.\\n\\n"
                        "The filed count is 2, indicating that two artifacts have been filed.\\n\\n"
                        "The filed items include a reference note with the text \\\"Create project support note from Vendor support handoff\\\". "
                        "This note is of type project-support and does not have any warnings.\\n\\n"
                        "The target locations for this note are:\\n\\n"
                        "* \\\"/tmp/example-note.org\\\"\\n"
                        "* \\\"/tmp/example-projects.org::Launch Project:REFERENCE_FILES\\\"\\n\\n"
                        "There are no conflicts, and the reason for the approved filing artifacts is that they were inserted into deterministic targets.\"}"))))
            (setq raw (delib-flow-local-test--run-ollama-stage descriptor package)))
          (should (equal 1 (plist-get raw :approved-count)))
          (should (equal 2 (plist-get raw :filed-count)))
          (should (equal 2 (length (plist-get raw :target-locations))))
          (should (string-match-p "vendor-support-handoff\\.org\\'"
                                  (plist-get (car (plist-get raw :target-locations))
                                             :target)))
          (should (string-match-p "Launch Project:REFERENCE_FILES"
                                  (plist-get (cadr (plist-get raw :target-locations))
                                             :target))))))))

(ert-deftest delib-flow-local-test-resolve-filing-conflict-bypasses-ollama-block-rewrite ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n** TODO Write follow-up note for Alpha Project kickoff\n"
      (let* ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
             (run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'extract-actions))
             (selected
              (delib-flow-test--set-filing-selection
               (delib-flow--run-stage-locally drafted 'integrate-into-source)
               "1"))
             (approved
              (delib-flow-test--approve-selected-filing-item selected))
             (conflicted
              (delib-flow--run-stage-locally approved 'file-approved-outputs)))
        (cl-letf (((symbol-function 'delib-flow-local-test--ensure-ollama-ready)
                   (lambda () t))
                  ((symbol-function 'delib-flow-local-test--ollama-json)
                   (lambda (&rest _args)
                     (error "resolve-filing-conflict should not call Ollama in local test config"))))
          (let* ((delib-flow-local-stage-adapter
                  #'delib-flow-local-test-ollama-adapter)
                 (retargeted
                  (delib-flow--run-stage-locally
                   (delib-flow-test--set-filing-conflict-resolution
                    conflicted "RETRY" "" nil "Testing123")
                   'resolve-filing-conflict))
                 (approved (car (plist-get (plist-get retargeted :filing)
                                           :approved-items))))
            (should approved)
            (should (equal "Testing123" (plist-get approved :text)))))))))

(ert-deftest delib-flow-local-test-manual-project-match-bypasses-ollama-block-rewrite ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Project Beacon\n* Project Atlas\n"
      (let* ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
             (run (delib-flow--initialize-run
                   (list :title "Completely Different Topic"
                         :content "* Completely Different Topic\nAgenda\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project))))
        (cl-letf (((symbol-function 'delib-flow-local-test--ensure-ollama-ready)
                   (lambda () t))
                  ((symbol-function 'delib-flow-local-test--ollama-json)
                   (lambda (&rest _args)
                     (error "manual-project-match should not call Ollama in local test config"))))
          (let* ((delib-flow-local-stage-adapter
                  #'delib-flow-local-test-ollama-adapter)
                 (updated-run
                  (delib-flow--run-stage-locally
                   (delib-flow-test--set-manual-project-selection
                    matched
                    "Project Atlas")
                   'manual-project-match))
                 (project-match
                  (plist-get (delib-flow--run-working-context updated-run)
                             :project-match)))
            (should (equal 'manual (plist-get project-match :selection-method)))
            (should (equal "Project Atlas"
                           (plist-get (plist-get project-match :best-project)
                                      :title)))))))))

(ert-deftest delib-flow-local-test-match-project-falls-back-to-deterministic-exact-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Active\n** Project Atlas :example:\n"
      (let* ((package (list :source (list :title "Project Atlas"
                                          :content "* Project Atlas\nBody\n")
                            :working-context nil))
             (raw-output
              (delib-flow-local-test--match-raw-output package nil)))
        (should (eq 'matched (plist-get raw-output :match-status)))
        (should (equal "Project Atlas"
                       (plist-get (plist-get raw-output :best-project) :title)))))))

(ert-deftest delib-flow-local-test-match-raw-output-recovers-project-id-shape ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Active\n** Project Atlas :example:\n"
      (let* ((package (list :source (list :title "Project Atlas"
                                          :content "* Project Atlas\nBody\n")
                            :working-context nil))
             (parsed '((projectId . "Project Atlas")
                       (projectScore . 10)
                       (description . "Distinctive title terms match exactly.")))
             (raw-output
              (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'matched (plist-get raw-output :match-status)))
        (should (equal "Project Atlas"
                       (plist-get (plist-get raw-output :best-project) :title)))
        (should (equal "Exact title overlap with the source."
                       (plist-get raw-output :reason)))))))

(ert-deftest delib-flow-local-test-match-raw-output-recovers-title-shape ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Active\n** Project Atlas :example:\n"
      (let* ((package (list :source (list :title "Project Atlas"
                                          :content "* Project Atlas\nBody\n")
                            :working-context nil))
             (parsed '((title . "Project Atlas")
                       (score . 19)
                       (distinctive_title_terms "project" "atlas")))
             (raw-output
              (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'matched (plist-get raw-output :match-status)))
        (should (equal "Project Atlas"
                       (plist-get (plist-get raw-output :best-project) :title)))))))

(ert-deftest delib-flow-local-test-match-project-repairs-malformed-llm-schema ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Active\n** Project Atlas :example:\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Project Atlas"
                         :content "* Project Atlas\nBody\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (calls nil))
        (cl-letf (((symbol-function 'delib-flow-local-test--ensure-ollama-ready)
                   (lambda () t))
                  ((symbol-function 'delib-flow-local-test--ollama-json)
                   (lambda (_prompt stage)
                     (push stage calls)
                     (cond
                      ((equal stage "delib-flow-match-project")
                       '((projectId . "Project Atlas")
                         (projectScore . 10)
                         (description . "Strong title overlap.")))
                      ((equal stage "delib-flow-match-project-repair")
                        '((match_status . "matched")
                         (best_project_title . "Project Atlas")
                         (candidate_titles . ["Project Atlas"])
                         (reason . "Exact title overlap with the source.")))
                      (t
                       (error "Unexpected Ollama stage: %s" stage))))))
          (let* ((delib-flow-local-stage-adapter
                  #'delib-flow-local-test-ollama-adapter)
                 (matched (delib-flow--run-stage-locally inspected 'match-project))
                 (project-match
                  (plist-get (delib-flow--run-working-context matched)
                             :project-match)))
            (should (equal 'matched (plist-get project-match :match-status)))
            (should (equal "Project Atlas"
                           (plist-get (plist-get project-match :best-project)
                                      :title)))
            (should (equal '("delib-flow-match-project-repair"
                             "delib-flow-match-project")
                           calls))))))))

(ert-deftest delib-flow-local-test-match-prompt-explicitly-prefers-ambiguity-over-forced-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n* Beta Project\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities ("Alpha" "Beta")
                           :summary "Could belong to Alpha or Beta.")))
             (package
              (list :source
                    (list :title "Completely Different Topic"
                          :content "* Completely Different Topic\nAgenda:\n- Clarify whether this belongs to Alpha or Beta work.\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (prompt (delib-flow-local-test--match-prompt package)))
        (should (string-match-p "Prefer ambiguity over a forced match" prompt))
        (should (string-match-p "only overlap is generic wording" prompt))))))

(ert-deftest delib-flow-local-test-proposal-prompt-forbids-workflow-envelope-output ()
  (delib-flow-test--with-local-test-config
    (let* ((inspect-output
            '(:source-type fleeting-note
              :analysis (:summary "Presentation idea about focusing on the end user."
                         :entities ("end user"))
              :org-file-links ("id:alpha")))
           (package
            (list :source
                  (list :title "[2026-02-10 Tue 20:40] Presentation or article on focusing on the end user"
                        :content "* [2026-02-10 Tue 20:40] Presentation or article on focusing on the end user\nBody\n")
                  :working-context
                  (list :inspect-output inspect-output)))
           (prompt (delib-flow-local-test--proposal-prompt package)))
      (should (string-match-p "Do not return package objects, workflow envelopes" prompt))
      (should (string-match-p "Do not invent generic placeholders like `New Project Proposal`" prompt))
      (should (string-match-p "\"deterministic_title_hint\"" prompt)))))

(ert-deftest delib-flow-local-test-match-raw-output-downgrades-weak-split-evidence-to-no-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n* Beta Project\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Could belong to Alpha or Beta work.")))
             (package
              (list :source
                    (list :title "Completely Different Topic"
                          :content "* Completely Different Topic\nAgenda:\n- Clarify whether this belongs to Alpha or Beta work.\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "matched")
                       (best_project_title . "Alpha Project")
                       (candidate_titles "Alpha Project" "Beta Project")
                       (reason . "Matched on alpha mention.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'no-match (plist-get raw :match-status)))
        (should-not (plist-get raw :best-project))
        (should-not (plist-get raw :candidates))))))

(ert-deftest delib-flow-local-test-proposal-raw-output-cleans-title-and-tags-from-malformed-envelope ()
  (delib-flow-test--with-local-test-config
    (let* ((inspect-output
            '(:source-type fleeting-note
              :analysis (:summary "Presentation idea about focusing on the end user."
                         :entities ("end user" "business stakeholders"))))
           (package
            (list :source
                  (list :title "[2026-02-10 Tue 20:40] Presentation or article on focusing on the end user"
                        :content "* [2026-02-10 Tue 20:40] Presentation or article on focusing on the end user\n+ Gather a number of success cases with client satisfaction to showcase\n")
                  :working-context
                  (list :inspect-output inspect-output)))
           (parsed '((project . ((title . "[2026-02-10 Tue 20:40] Presentation or article on focusing on the end user")
                                 (description . "Local LLM drafted a new project proposal.")))
                     (filing . ((draft_items . [((state . "active"))])))))
           (raw (delib-flow-local-test--proposal-raw-output package parsed)))
      (should (equal "Focusing on the end user" (plist-get raw :project-title)))
      (should-not (member "2026" (plist-get raw :tags)))
      (should (member "end_user" (plist-get raw :tags)))
      (should (equal "Gather a number of success cases with client satisfaction to showcase"
                     (plist-get (plist-get raw :first-item) :text))))))

(ert-deftest delib-flow-local-test-propose-new-project-falls-back-on-parse-error ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "[2026-02-10 Tue 20:40] Presentation or article on focusing on the end user"
                         :content "* [2026-02-10 Tue 20:40] Presentation or article on focusing on the end user\n+ Gather a number of success cases with client satisfaction to showcase\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project))))
        (cl-letf (((symbol-function 'delib-flow-local-test--ensure-ollama-ready)
                   (lambda () t))
                  ((symbol-function 'delib-flow-local-test--ollama-json)
                   (lambda (_prompt _stage)
                     (error "synthetic parse failure"))))
          (let* ((delib-flow-local-stage-adapter #'delib-flow-local-test-ollama-adapter)
                 (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
                 (project (car (plist-get (plist-get proposed :filing) :draft-items))))
            (should (equal "Focusing on the end user" (plist-get project :title)))
            (should (equal "Gather a number of success cases with client satisfaction to showcase"
                           (plist-get (plist-get project :first-item) :text)))))))))

(ert-deftest delib-flow-local-test-match-raw-output-rejects-generic-title-term-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Active\n** Write up reflection on my personal workflow :example:\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links ("id:note-a" "id:note-b")
                :analysis (:summary "Presentation or article idea about focusing on the end user.")))
             (package
              (list :source
                    (list :title "Presentation or article on focusing on the end user"
                          :content "* Presentation or article on focusing on the end user\n- Focus on the end user.\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "matched")
                       (best_project_title . "Write up reflection on my personal workflow")
                       (candidate_titles "Write up reflection on my personal workflow")
                       (reason . "Distinctive title-term overlap with the source: on, my.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'ambiguous (plist-get raw :match-status)))
        (should-not (plist-get raw :best-project))
        (should (string-match-p "Evidence is split across plausible projects\\|Evidence is too weak or shared"
                                (plist-get raw :reason)))))))

(ert-deftest delib-flow-local-test-match-raw-output-keeps-match-when-contact-evidence-is-strong ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\nContact alice@example.com\n* Beta Project\nContact bob@example.com\n"
      (let* ((inspect-output
              '(:contact-emails ("alice@example.com")
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Follow up with alice@example.com.")))
             (package
              (list :source
                    (list :title "Kickoff sync"
                          :content "* Kickoff sync\nPlease follow up with alice@example.com\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "matched")
                       (best_project_title . "Alpha Project")
                       (candidate_titles "Alpha Project")
                       (reason . "Contact overlap with alice@example.com.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'matched (plist-get raw :match-status)))
        (should
         (equal "Alpha Project"
                (plist-get (plist-get raw :best-project) :title)))))))

(ert-deftest delib-flow-local-test-match-raw-output-downgrades-weak-title-only-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n* Beta Project\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Evidence is too weak to classify this source beyond unknown.")))
             (package
              (list :source
                    (list :title "Alpha Project kickoff"
                          :content "* Alpha Project kickoff\nBody line\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "matched")
                       (best_project_title . "Alpha Project")
                       (candidate_titles "Alpha Project")
                       (reason . "A single highest-scoring metadata-aware match was found.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'no-match (plist-get raw :match-status)))
        (should-not (plist-get raw :best-project))
        (should-not (plist-get raw :candidates))))))

(ert-deftest delib-flow-local-test-match-raw-output-downgrades-singleton-weak-title-only-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Evidence is too weak to classify this source beyond unknown.")))
             (package
              (list :source
                    (list :title "Alpha Project kickoff"
                          :content "* Alpha Project kickoff\nBody line\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "matched")
                       (best_project_title . "Alpha Project")
                       (candidate_titles "Alpha Project")
                       (reason . "A single highest-scoring metadata-aware match was found.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'no-match (plist-get raw :match-status)))
        (should-not (plist-get raw :best-project))
        (should-not (plist-get raw :candidates))))))

(ert-deftest delib-flow-local-test-match-raw-output-replaces-ungrounded-match-reason ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Project Atlas\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Evidence is too weak to classify this source beyond unknown.")))
              (package
              (list :source
                    (list :title "Project Atlas"
                          :content "* Project Atlas\nBody\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "matched")
                       (best_project_title . "Project Atlas")
                       (candidate_titles "Project Atlas")
                       (reason . "Working on example docs refresh and example analytics migration to improve user experience.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'matched (plist-get raw :match-status)))
        (should (equal "Project Atlas"
                       (plist-get (plist-get raw :best-project) :title)))
        (should (equal "Exact title overlap with the source."
                       (plist-get raw :reason)))))))

(ert-deftest delib-flow-local-test-match-raw-output-promotes-ambiguous-to-match-on-unique-contact-overlap ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\nContact alice@example.com\n* Beta Project\nContact bob@example.com\n"
      (let* ((inspect-output
              '(:contact-emails ("alice@example.com")
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Budget follow-up for alice@example.com.")))
             (package
              (list :source
                    (list :title "Budget follow-up"
                          :content "* Budget follow-up\nPlease reply to alice@example.com\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "ambiguous")
                       (best_project_title . "")
                       (candidate_titles "Alpha Project" "Beta Project")
                       (reason . "Both projects remain plausible.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'matched (plist-get raw :match-status)))
        (should
         (equal "Alpha Project"
                (plist-get (plist-get raw :best-project) :title)))))))

(ert-deftest delib-flow-local-test-match-raw-output-promotes-ambiguous-to-match-on-unique-linked-note-overlap ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-zk-root
        '(("notes/beta-brief.org" . "#+title: Beta Brief\nLinked support note.\n"))
      (let ((linked-file (expand-file-name "notes/beta-brief.org" delib-flow-zk-root)))
        (delib-flow-test--with-temp-project-file
            (format "* Alpha Project\nSee [[file:%s][Alpha support]]\n* Beta Project\nSee [[file:%s][Beta brief]]\n"
                    (expand-file-name "notes/alpha-brief.org" delib-flow-zk-root)
                    linked-file)
          (let* ((inspect-output
                  `(:contact-emails nil
                    :org-file-links (,linked-file)
                    :analysis (:entities nil
                               :summary "Open questions from note review. References beta-brief.")))
                 (package
                  (list :source
                        (list :title "Open questions from note review"
                              :content (format "* Open questions from note review\nSee [[file:%s][Beta brief]]\n"
                                               linked-file))
                        :working-context
                        (list :inspect-output inspect-output)))
                 (parsed '((match_status . "ambiguous")
                           (best_project_title . "")
                           (candidate_titles "Alpha Project" "Beta Project")
                           (reason . "A linked note may be relevant, but the text is otherwise generic.")))
                 (raw (delib-flow-local-test--match-raw-output package parsed)))
            (should (eq 'matched (plist-get raw :match-status)))
            (should
             (equal "Beta Project"
                    (plist-get (plist-get raw :best-project) :title)))))))))

(ert-deftest delib-flow-local-test-match-prompt-handles-linked-note-candidates ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-zk-root
        '(("notes/beta-brief.org" . "#+title: Beta Brief\nLinked support note.\n"))
      (let ((linked-file (expand-file-name "notes/beta-brief.org" delib-flow-zk-root)))
        (delib-flow-test--with-temp-project-file
            (format "* Alpha Project\nSee [[file:%s][Alpha support]]\n* Beta Project\nSee [[file:%s][Beta brief]]\n"
                    (expand-file-name "notes/alpha-brief.org" delib-flow-zk-root)
                    linked-file)
          (let* ((inspect-output
                  `(:contact-emails nil
                    :org-file-links (,linked-file)
                    :analysis (:entities nil
                               :summary "Open questions from note review. References beta-brief.")))
                 (package
                  (list :source
                        (list :title "Open questions from note review"
                              :content (format "* Open questions from note review\nSee [[file:%s][Beta brief]]\n"
                                               linked-file))
                        :working-context
                        (list :inspect-output inspect-output)))
                 (prompt (delib-flow-local-test--match-prompt package)))
            (should (stringp prompt))
            (should (string-match-p "Beta Project" prompt))
            (should (string-match-p "Calibration hints" prompt))))))))

(ert-deftest delib-flow-local-test-match-prompt-shortlists-noisy-candidates ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Project Atlas :example:\n* Example docs refresh :example:docs:\n* Example analytics migration :example:analytics:\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Evidence is too weak to classify this source beyond unknown.")))
              (package
              (list :source
                    (list :title "Project Atlas"
                          :content "* Project Atlas\nBody\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (prompt (delib-flow-local-test--match-prompt package)))
        (should (string-match-p "Shortlisted project candidates" prompt))
        (should (string-match-p "Project Atlas" prompt))
        (should-not (string-match-p "Example docs refresh" prompt))
        (should-not (string-match-p "Example analytics migration" prompt))))))

(ert-deftest delib-flow-local-test-match-raw-output-converts-generic-ambiguous-to-no-match ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\nContact alice@example.com\n* Beta Project\nContact bob@example.com\n"
      (let* ((inspect-output
              '(:contact-emails nil
                :org-file-links nil
                :analysis (:entities nil
                           :summary "Housekeeping reminder about chores and groceries.")))
             (package
              (list :source
                    (list :title "Housekeeping reminder"
                          :content "* Housekeeping reminder\n- Tidy up the desk\n- Buy groceries\n")
                    :working-context
                    (list :inspect-output inspect-output)))
             (parsed '((match_status . "ambiguous")
                       (best_project_title . "")
                       (candidate_titles "Alpha Project" "Beta Project")
                       (reason . "The note is generic and does not clearly identify a project.")))
             (raw (delib-flow-local-test--match-raw-output package parsed)))
        (should (eq 'no-match (plist-get raw :match-status)))
        (should-not (plist-get raw :best-project))
        (should-not (plist-get raw :candidates))))))

(ert-deftest delib-flow-local-test-match-raw-output-converts-broad-ambiguous-candidate-set-to-no-match ()
  (delib-flow-test--with-local-test-config
    (let* ((inspect-output
            '(:contact-emails nil
              :org-file-links nil
              :analysis (:summary "General presentation idea.")))
           (candidates
            (mapcar
             (lambda (title) (list :title title :contacts nil :links nil))
             '("App login tracking"
               "Implement CDP audience variables in app"
               "Data layer validation"
               "Update FastTrack offer solution"
               "Create basic web sandbox environment"
               "Francium availability check updates")))
           (package
            (list :source
                  (list :title "Presentation idea"
                        :content "* Presentation idea\n- Focus on the end user.\n")
                  :working-context
                  (list :inspect-output inspect-output)))
           (parsed
            '((match_status . "ambiguous")
              (best_project_title . "")
              (candidate_titles "App login tracking"
                                "Implement CDP audience variables in app"
                                "Data layer validation"
                                "Update FastTrack offer solution"
                                "Create basic web sandbox environment"
                                "Francium availability check updates")
              (reason . "Evidence is split."))))
      (cl-letf (((symbol-function 'delib-flow--project-candidates)
                 (lambda () candidates)))
        (let ((raw (delib-flow-local-test--match-raw-output package parsed)))
          (should (eq 'no-match (plist-get raw :match-status)))
          (should-not (plist-get raw :candidates)))))))

(ert-deftest delib-flow-local-test-match-raw-output-converts-newsletter-ambiguity-to-no-match ()
  (delib-flow-test--with-local-test-config
    (let* ((inspect-output
            '(:contact-emails ("hello@fortelabs.com")
              :org-file-links nil
              :body-preview "A quick update on week 2 of the AI Second Brain cohort."
              :analysis (:summary "Newsletter update about the AI Second Brain cohort and a future waitlist.")))
           (candidates
            (mapcar
             (lambda (title) (list :title title :contacts nil :links nil))
             '("Write up reflection on my personal workflow"
               "Create basic web sandbox environment"
               "Unspecified subscription type learnings"
               "Enable Auth-Ident"
               "Focusing on the end user"
               "Target digital campaign feedback")))
           (package
            (list :source
                  (list :title "Your Consumption Diet Is Your Moat"
                        :content "* Your Consumption Diet Is Your Moat\nNewsletter body.\n")
                  :working-context
                  (list :inspect-output inspect-output)))
           (parsed
            '((match_status . "ambiguous")
              (best_project_title . "")
              (candidate_titles "Write up reflection on my personal workflow"
                                "Create basic web sandbox environment"
                                "Unspecified subscription type learnings"
                                "Enable Auth-Ident"
                                "Focusing on the end user"
                                "Target digital campaign feedback")
              (reason . "Evidence is split."))))
      (cl-letf (((symbol-function 'delib-flow--project-candidates)
                 (lambda () candidates)))
        (let ((raw (delib-flow-local-test--match-raw-output package parsed)))
          (should (eq 'no-match (plist-get raw :match-status)))
          (should-not (plist-get raw :candidates)))))))

(ert-deftest delib-flow-local-test-match-raw-output-forces-no-match-for-transactional-email-without-project-evidence ()
  (delib-flow-test--with-local-test-config
    (let* ((content
            (concat
             "** TODO Feel The Snow from your Steam wishlist is now on sale! :email:\n"
             ":PROPERTIES:\n"
             ":FROM: Steam <noreply@steampowered.com>\n"
             ":DATE: Sat, 02 May 2026 10:36:13 -0700\n"
             ":END:\n\n"
             ":RAW_EMAIL:\n"
             "From: Steam <noreply@steampowered.com>\n"
             "Subject: Feel The Snow from your Steam wishlist is now on sale!\n\n"
             "The following items on your wishlist are on sale:\n"
             "This email message was auto-generated. Please do not respond.\n"
             ":END:\n"))
           (source
            (list :title "Feel The Snow from your Steam wishlist is now on sale!"
                  :content content))
           (inspect-output
            '(:source-type email
              :contact-emails ("noreply@steampowered.com")
              :org-file-links nil
              :analysis (:summary "Wishlist sale notification.")))
           (candidates
            (mapcar
             (lambda (title) (list :title title :contacts nil :links nil))
             '("App login tracking"
               "Best Ager landing page"
               "Improving app documentation")))
           (package
            (list :source source
                  :working-context (list :inspect-output inspect-output)))
           (parsed
            '((match_status . "ambiguous")
              (best_project_title . "")
              (candidate_titles "App login tracking"
                                "Best Ager landing page"
                                "Improving app documentation")
              (reason . "Evidence is split."))))
      (cl-letf (((symbol-function 'delib-flow--project-candidates)
                 (lambda () candidates)))
        (let ((raw (delib-flow-local-test--match-raw-output package parsed)))
          (should (eq 'no-match (plist-get raw :match-status)))
          (should-not (plist-get raw :candidates)))))))

(ert-deftest delib-flow-local-test-discover-prompt-requires-exact-candidate-titles ()
  (delib-flow-test--with-local-test-config
    (delib-flow-test--with-temp-zk-root
        '(("beta-brief.org" . "#+title: Beta brief\nNeed to reconcile blockers.\n"))
      (delib-flow-test--with-temp-project-file
          "* Beta Project\n[[file:beta-brief.org][Beta brief]]\n"
        (let* ((run (delib-flow--initialize-run
                     (list :title "Open questions from note review"
                           :content "* Open questions from note review\nNeed to reconcile blockers.\n[[file:beta-brief.org][Beta brief]]\n")))
               (inspected (delib-flow-test--accept-inspect
                           (delib-flow--run-stage-locally run 'inspect-source)))
               (matched (delib-flow-test--accept-match
                         (delib-flow--run-stage-locally inspected 'match-project)))
               (package (delib-flow--stage-input-package matched
                                                         'discover-reference-material))
               (prompt (delib-flow-local-test--discover-prompt package)))
          (should (string-match-p "Use only exact titles from the provided candidate list" prompt))
          (should (string-match-p "Do not return the source title, project title" prompt))
          (should (string-match-p "Do not return candidate objects, note metadata objects, markdown fences, or explanatory prose" prompt))
          (should (string-match-p "Invalid example: \\[{" prompt))
          (should (string-match-p "If nothing is relevant, return an empty `candidate_titles` list" prompt))
          (should (string-match-p "Do not reproduce source summary fields, UI fields, cloud fields, or package metadata" prompt))
          (should (string-match-p "\"matched_project_title\"" prompt))
          (should-not (string-match-p "editable-blocks" prompt))
          (should-not (string-match-p "cloud-routing-review" prompt)))))))

(ert-deftest delib-flow-local-test-discover-raw-output-recovers-array-object-titles ()
  (delib-flow-test--with-local-test-config
    (let* ((package '(:source (:title "Open questions from note review")))
           (base-candidate
            '(:title "Beta brief"
              :file "/tmp/notes/beta-brief.org"
              :score 4
              :reasons ("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
           (parsed '(((title . "Beta brief")
                      (reasons "linked-note-overlap" "explicit title cue"))))
           raw candidate)
      (cl-letf (((symbol-function 'delib-flow--discover-reference-material-result)
                 (lambda (_package)
                   (list :search-terms '("beta" "brief")
                         :candidate-count 1
                         :candidates (list (copy-tree base-candidate))))))
      (setq raw (delib-flow-local-test--discover-raw-output package parsed))
      (setq candidate (car (plist-get raw :candidates))))
      (should (equal 1 (plist-get raw :candidate-count)))
      (should (equal "Beta brief" (plist-get candidate :title)))
      (should (equal '("linked-note-overlap" "explicit title cue")
                     (plist-get candidate :reasons))))))

(ert-deftest delib-flow-local-test-discover-raw-output-recovers-nested-note-objects ()
  (delib-flow-test--with-local-test-config
    (let* ((package '(:source (:title "Open questions from note review")))
           (base-candidate
            '(:title "Beta brief"
              :file "/tmp/notes/beta-brief.org"
              :score 4
              :reasons ("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
           (parsed '((notes
                      .
                      [((title . "Beta brief")
                        (reasons "title-overlap=1 (+3)" "text-overlap=1 (+1)"))])
                     (reason . "Recovered from note objects.")))
           raw candidate)
      (cl-letf (((symbol-function 'delib-flow--discover-reference-material-result)
                 (lambda (_package)
                   (list :search-terms '("beta" "brief")
                         :candidate-count 1
                         :candidates (list (copy-tree base-candidate))))))
        (setq raw (delib-flow-local-test--discover-raw-output package parsed))
        (setq candidate (car (plist-get raw :candidates))))
      (should (equal 1 (plist-get raw :candidate-count)))
      (should (equal "Beta brief" (plist-get candidate :title)))
      (should (equal '("title-overlap=1 (+3)" "text-overlap=1 (+1)")
                     (plist-get candidate :reasons))))))

(ert-deftest delib-flow-local-test-discover-raw-output-falls-back-to-top-candidate-only ()
  (delib-flow-test--with-local-test-config
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
               (package (delib-flow--stage-input-package matched
                                                         'discover-reference-material))
               (base (delib-flow--discover-reference-material-result package))
               (base-candidates (plist-get base :candidates))
               (parsed '((candidate_titles "Alpha Project kickoff")
                         (reason . "Using the source title instead of a candidate title.")))
               (raw (delib-flow-local-test--discover-raw-output package parsed)))
          (should (> (length base-candidates) 1))
          (should (equal 1 (plist-get raw :candidate-count)))
          (should (equal (plist-get (car base-candidates) :title)
                         (plist-get (car (plist-get raw :candidates)) :title)))
          (should (string-match-p "did not name valid candidate titles"
                                  (plist-get raw :reason))))))))

(ert-deftest delib-flow-local-test-discover-salvaged-parsed-recovers-titles-from-raw-text ()
  (delib-flow-test--with-local-test-config
    (let* ((package '(:source (:title "Open questions from note review")))
           (base-candidate
            '(:title "Beta brief"
              :file "/tmp/notes/beta-brief.org"
              :score 4
              :reasons ("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
           (delib-flow-local-test--last-ollama-response
            "Here is a valid JSON object:\n\n```json\n{\"notes\":[{\"title\":\"Beta brief\",\"score\":4}],\"ui\":{\"editable_blocks\":[{\"context-main\": ...}]}}\n```")
           (delib-flow-local-test--last-ollama-json-fragment
            "{\"notes\":[{\"title\":\"Beta brief\",\"score\":4}],\"ui\":{\"editable_blocks\":[{\"context-main\": ...}]}}")
           (delib-flow-local-test--last-ollama-parse-error
            '(json-readtable-error 46))
           salvaged)
      (cl-letf (((symbol-function 'delib-flow--discover-reference-material-result)
                 (lambda (_package)
                   (list :search-terms '("beta" "brief")
                         :candidate-count 1
                         :candidates (list (copy-tree base-candidate))))))
        (setq salvaged (delib-flow-local-test--discover-salvaged-parsed package)))
      (should (equal '("Beta brief")
                     (delib-flow-local-test--json-value salvaged 'candidate_titles)))
      (should (string-match-p "Recovered discovery candidate titles"
                              (delib-flow-local-test--json-value salvaged 'reason))))))

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
                     discover-reference-material
                     suggest-reference-notes
                     edit-operator-intent
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
                     edit-operator-intent
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
            (should (search-forward "- Discover Relevant Reference Material [available]" nil t))
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
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-accept-inspect-source-command-preserves-current-result-anchor ()
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
            (should (equal "Current result"
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
                       edit-operator-intent
                       decide-cloud-pass
                       refresh-buffer
                       abort-run)
                     (mapcar (lambda (action)
                               (plist-get action :id))
                             (plist-get (delib-flow--run-actions updated-run)
                                        :items)))))))

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
                       :content "* Completely Different Topic\nDraft kickoff outline for Completely Different Topic\n")))
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
                       edit-operator-intent
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

(ert-deftest delib-flow-match-project-prefers-inspect-title-for-normalized-exact-hit ()
  (delib-flow-test--with-temp-project-file
      "* Project Atlas\n* Another Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "[2026-02-24 Tue 22:37] Project Atlas"
                       :content "** [2026-02-24 Tue 22:37] Project Atlas\nBody\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal "Project Atlas"
                     (plist-get (plist-get project-match :best-project) :title))))))

(ert-deftest delib-flow-match-project-strips-leading-org-timestamp-from-title ()
  (delib-flow-test--with-temp-project-file
      "* Project Atlas\n* Project Comet\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "[2026-02-24 Tue 22:37] Project Atlas"
                       :content "** [2026-02-24 Tue 22:37] Project Atlas\nDefine MVP.\nClarify the core differentiator.\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (updated-run (delib-flow--run-stage-locally inspected 'match-project))
           (project-match
            (plist-get (delib-flow--run-working-context updated-run)
                       :project-match)))
      (should (equal 'matched (plist-get project-match :match-status)))
      (should (equal "Project Atlas"
                     (plist-get (plist-get project-match :best-project) :title))))))

(ert-deftest delib-flow-accept-match-project-seeds-manual-selection-template-for-no-match ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nDraft kickoff outline for Completely Different Topic\n")))
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
                       edit-operator-intent
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

(ert-deftest delib-flow-choose-manual-project-command-populates-selection-block ()
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
           (labels (delib-flow--manual-project-selection-labels delib-flow--active-run))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _args)
                         (caar labels))))
              (delib-flow-choose-manual-project))
            (should (string-match-p "Selection: Alpha Project"
                                    (delib-flow--editable-block-text
                                     (delib-flow--editable-block delib-flow--active-run
                                                                 'manual-project-selection))))
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "Selection: Alpha Project" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-manual-project-selection-renders-candidate-shortlist ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project :example:product:\nContact alice@example.com\n* Beta Project\n"
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
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Manual project selection" nil t))
            (should (search-forward "Current selection: none yet." nil t))
            (should (search-forward "**** Local update" nil t))
            (should (search-forward "**** Candidate shortlist" nil t))
            (should (search-forward "- [1] Alpha Project" nil t))
            (should (search-forward "Signals: 1 contact(s), 0 link(s)" nil t))
            (goto-char (point-min))
            (should (search-forward "- [2] Beta Project" nil t))
            (should (search-forward "**** Fallback block" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-choose-manual-project-reseeds-actions-after-overriding-stale-manual-match ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (manual-run
            (delib-flow--seed-actions
             (delib-flow--run-stage-locally
              (delib-flow-test--set-manual-project-selection
               matched
               "Alpha Project")
              'manual-project-match)))
           (delib-flow--active-run manual-run)
           (labels (delib-flow--manual-project-selection-labels delib-flow--active-run))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _args)
                         (car (rassoc "Beta Project" labels)))))
              (delib-flow-choose-manual-project))
            (should (equal 'not-available
                           (delib-flow--accepted-project-status delib-flow--active-run)))
            (should-not (member 'extract-actions
                                (mapcar (lambda (action)
                                          (plist-get action :id))
                                        (plist-get (delib-flow--run-actions delib-flow--active-run)
                                                   :items)))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-manual-project-match-command-prefers-valid-active-selection-over-stale-buffer ()
  (delib-flow-test--with-temp-project-file
      "* Project Beacon\n* Project Atlas\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (manual-run
            (delib-flow--seed-actions
             (delib-flow--run-stage-locally
              (delib-flow-test--set-manual-project-selection
               matched
               "Project Beacon")
              'manual-project-match)))
           (delib-flow--active-run
            (delib-flow--set-manual-project-selection-value manual-run
                                                            "Project Atlas"))
           (stale-buffer-run manual-run)
           (buffer (delib-flow--render-control-buffer stale-buffer-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-manual-project-match)
            (should (equal "Project Atlas"
                           (plist-get
                            (plist-get
                             (plist-get (delib-flow--run-working-context delib-flow--active-run)
                                        :project-match)
                             :best-project)
                            :title))))
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

(ert-deftest delib-flow-propose-new-project-strips-note-like-title-noise ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "[2026-02-10 Tue 20:40] Presentation or article on focusing on the end user"
                       :content "* [2026-02-10 Tue 20:40] Presentation or article on focusing on the end user\n+ Gather a number of success cases with client satisfaction to showcase\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (project (car (plist-get (plist-get proposed :filing) :draft-items))))
      (should (equal "Focusing on the end user" (plist-get project :title)))
      (should (equal "Gather a number of success cases with client satisfaction to showcase"
                     (plist-get (plist-get project :first-item) :text)))
      (should-not (member "2026" (plist-get project :tags)))
      (should (member "end" (plist-get project :tags))))))

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
              (should (search-forward "*** Project workspace" nil t))
              (goto-char (point-min))
              (should (search-forward "** Project package" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-match-project-command-rerenders-history ()
  (delib-flow-test--with-temp-audit-file
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
                (should (search-forward "- Suggest Reference Notes [available]" nil t))
                (goto-char (point-min))
                (should-not (search-forward "- Discover Relevant Reference Material [available]" nil t))
                (goto-char (point-min))
                (should-not (search-forward "- Extract Actions [available]" nil t))
                (goto-char (point-min))
                (should-not (search-forward "- Extract Waiting-For [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

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
                       manual-project-match
                       extract-actions
                       extract-waiting-for
                       suggest-reference-notes
                       edit-operator-intent
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
                       manual-project-match
                       suggest-reference-notes
                       edit-operator-intent
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

(ert-deftest delib-flow-reject-match-project-allows-manual-project-command ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n* Beta Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAgenda\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (updated-run
            (delib-flow-test--set-manual-project-selection
             (delib-flow--seed-actions
              (delib-flow--apply-match-review-outcome
               matched
               'rejected
               "Project match rejected. Use manual override or retry matching."))
             "Alpha Project")))
      (let ((delib-flow--active-run updated-run))
        (delib-flow-action-manual-project-match)
        (should (equal "Alpha Project"
                       (plist-get
                        (plist-get
                         (plist-get (delib-flow--run-working-context delib-flow--active-run)
                                    :project-match)
                         :best-project)
                        :title)))))))

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

(ert-deftest delib-flow-drafted-proposed-project-surfaces-extraction-actions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "https://example.com/drill?id=two"
                                   "I need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected
                                                   'draft-selected-project))
           (action-ids (mapcar (lambda (action)
                                 (plist-get action :id))
                               (plist-get (delib-flow--run-actions drafted)
                                          :items)))
           (buffer (delib-flow--render-active-run-buffer drafted "Next actions")))
      (should (eq 'no-match (delib-flow--match-status drafted)))
      (should (memq 'extract-actions action-ids))
      (should (memq 'extract-waiting-for action-ids))
      (should (memq 'edit-operator-intent action-ids))
      (should-not (memq 'propose-new-project action-ids))
      (should-not (memq 'manual-project-match action-ids))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Active project loop" nil t))
            (goto-char (point-min))
            (should (search-forward "Primary numbered palette: `Filing preview > Project workspace`" nil t))
            (goto-char (point-min))
            (should (search-forward "Extract Actions" nil t))
            (goto-char (point-min))
            (should (search-forward "Extract Waiting-For" nil t))
            (goto-char (point-min))
            (should (search-forward "Edit Operator Intent" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Retry Propose New Project [available]" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Choose Project Manually [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "*** Project workspace" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-drafted-proposed-project-allows-extract-action-commands ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "https://example.com/drill?id=two"
                                   "I need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected
                                                   'draft-selected-project)))
      (let ((delib-flow--active-run drafted))
        (delib-flow-action-extract-actions)
        (should (eq 'extract-actions
                    (plist-get (delib-flow--run-stage-history delib-flow--active-run)
                               :latest-stage)))
        (should (plist-get (plist-get delib-flow--active-run :artifacts)
                           'actions)))
      (let ((delib-flow--active-run drafted))
        (delib-flow-action-extract-waiting-for)
        (should (eq 'extract-waiting-for
                    (plist-get (delib-flow--run-stage-history delib-flow--active-run)
                               :latest-stage)))
        (should (plist-get (plist-get delib-flow--active-run :artifacts)
                           'waiting-fors))))))

(ert-deftest delib-flow-project-workspace-surfaces-edit-operator-intent-action ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "https://example.com/drill?id=two")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected 'draft-selected-project))
           (text (with-current-buffer
                     (delib-flow--render-focused-filing-workspace-buffer drafted)
                   (buffer-string))))
      (should (string-match-p "Local numbered actions" text))
      (should (string-match-p "Edit Operator Intent" text)))))

(ert-deftest delib-flow-extract-actions-command-guides-selected-project-to-draft-first ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "Need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (delib-flow--active-run selected))
      (condition-case err
          (delib-flow-action-extract-actions)
        (user-error
         (should (string= "Draft the selected project package before extracting actions"
                          (cadr err))))))))

(ert-deftest delib-flow-extract-waiting-for-command-guides-selected-project-to-draft-first ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "Need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (delib-flow--active-run selected))
      (condition-case err
          (delib-flow-action-extract-waiting-for)
        (user-error
         (should (string= "Draft the selected project package before extracting waiting-for items"
                          (cadr err))))))))

(ert-deftest delib-flow-extract-actions-command-requires-project-context-without-match-or-draft ()
  (let ((delib-flow--active-run
         (delib-flow--initialize-run
          (list :title "Example"
                :content "* Example\nBody line\n"))))
    (condition-case err
        (delib-flow-action-extract-actions)
      (user-error
       (should (string= "Accept or manually match a project, or draft a selected proposed project package, before extracting actions"
                        (cadr err)))))))

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
      (should (>= (length entries) 2))
      (let ((latest-two (last entries 2)))
        (should (eq 'superseded
                    (plist-get (nth 0 latest-two) :review-state)))
        (should (eq 'pending-review
                    (plist-get (nth 1 latest-two) :review-state))))
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

(ert-deftest delib-flow-discover-reference-material-command-runs-after-accepted-inspect-without-project ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: AI Advisor Notes\nBuilding personal AI advisors.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Building personal AI advisors"
                       :content "* Building personal AI advisors\nBody line\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (delib-flow--active-run inspected)
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-discover-reference-material)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "Retrieved context: available" nil t))
              (should (search-forward "AI Advisor Notes" nil t))))
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

(ert-deftest delib-flow-filter-reference-material-action-hidden-without-discovery-candidates ()
  (delib-flow-test--with-temp-zk-root
      '(("misc.org" . "#+title: Chores\nGroceries and laundry.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Housekeeping reminder"
                       :content "* Housekeeping reminder\nGroceries and chores\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (discovered
            (delib-flow--run-stage-locally inspected
                                           'discover-reference-material))
           (actions (plist-get (delib-flow--run-actions
                                (delib-flow--seed-actions discovered))
                               :items)))
      (should-not (member 'filter-reference-material
                          (mapcar (lambda (action)
                                    (plist-get action :id))
                                  actions))))))

(ert-deftest delib-flow-filter-reference-material-command-errors-without-discovery-candidates ()
  (delib-flow-test--with-temp-zk-root
      '(("misc.org" . "#+title: Chores\nGroceries and laundry.\n"))
    (let* ((run (delib-flow--initialize-run
                 (list :title "Housekeeping reminder"
                       :content "* Housekeeping reminder\nGroceries and chores\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (discovered
            (delib-flow--run-stage-locally inspected
                                           'discover-reference-material))
           (delib-flow--active-run discovered))
      (should-error (delib-flow-action-filter-reference-material)
                    :type 'user-error))))

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

(ert-deftest delib-flow-local-test-filter-prompt-requests-retained-context ()
  (delib-flow-test--with-local-test-config
    (let* ((candidate (list :title "Beta brief"
                            :file (make-temp-file "delib-flow-note" nil ".org"
                                                  "#+title: Beta brief\nNeed to reconcile blockers.\n")
                            :score 4
                            :reasons '("title-overlap=1 (+3)")))
           (package
            (list :source (list :title "Open questions from note review")
                  :working-context
                  (list :retrieved-candidates (list candidate)
                        :review-results
                        (list
                         (cons 'inspect-source
                               (list :accepted-output
                                     (list :title "Open questions from note review"
                                           :source-type 'meeting-note
                                           :body-preview "Need to reconcile the support note with the current project state."
                                           :analysis
                                           (list :summary "Need to reconcile blockers."
                                                 :blockers '("Open blockers")
                                                 :questions '("What remains open?")
                                                 :contacts nil
                                                 :entities '("Beta"))
                                           :org-file-links '("/tmp/notes/beta-brief.org"))))
                         (cons 'match-project
                               (list :accepted-output
                                     (list :best-project
                                           (list :title "Beta Project"))))))))
           (prompt (delib-flow-local-test--filter-prompt package)))
      (unwind-protect
          (progn
            (should (string-match-p "\"retained_context\": \"short multi-line context text\"" prompt))
            (should (string-match-p "The primary goal is to produce `retained_context`" prompt))
            (should (string-match-p "\"preview\"" prompt))
            (should (string-match-p "\"summary\":\"Need to reconcile blockers.\"" prompt))
            (should (string-match-p "\"body_preview\":\"Need to reconcile the support note with the current project state.\"" prompt))
            (should (string-match-p "\"blockers\":\\[\"Open blockers\"\\]" prompt))
            (should (string-match-p "\"questions\":\\[\"What remains open\\?\"\\]" prompt))
            (should (string-match-p "Synthesize source-side need with the retained note evidence" prompt))
            (should (string-match-p "Do not copy example wording unless the same substance is supported" prompt))
            (should (string-match-p "Relevant note: Candidate title here - short preview grounded in the note" prompt))
            (should (string-match-p "Do not return the source title or project title in `retained_titles`" prompt)))
        (delete-file (plist-get candidate :file))))))

(ert-deftest delib-flow-local-test-filter-raw-output-prefers-retained-context ()
  (delib-flow-test--with-local-test-config
    (let ((file (make-temp-file "delib-flow-note" nil ".org"
                                "#+title: Beta brief\nNeed to reconcile blockers.\n")))
      (unwind-protect
          (let* ((candidate (list :title "Beta brief"
                                  :file file
                                  :score 4
                                  :reasons '("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
                 (package (list :working-context
                                (list :retrieved-candidates (list candidate))))
                 (parsed '((retained_titles "Beta brief")
                           (items
                            (((title . "Beta brief")
                              (reasons "linked note mentions blockers"))))
                           (retained_context
                            . "- Beta brief: reconcile blockers before next step.\n- Open dependency remains.")
                           (reason . "keeps blocker context for downstream extraction")))
                 (raw (delib-flow-local-test--filter-raw-output package parsed)))
            (should (equal "- Beta brief: reconcile blockers before next step.\n- Open dependency remains."
                           (plist-get raw :retained-context)))
            (should (equal 1 (plist-get raw :retained-count))))
        (delete-file file)))))

(ert-deftest delib-flow-local-test-filter-raw-output-enriches-thin-context ()
  (delib-flow-test--with-local-test-config
    (let ((file (make-temp-file "delib-flow-note" nil ".org"
                                "#+title: Beta brief\nCurrent blockers\nFollow up with operations\n")))
      (unwind-protect
          (let* ((candidate (list :title "Beta brief"
                                  :file file
                                  :score 4
                                  :reasons '("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
                 (package
                  (list :source (list :title "Open questions from note review")
                        :working-context
                        (list :retrieved-candidates (list candidate)
                              :review-results
                              (list
                               (cons 'inspect-source
                                     (list :accepted-output
                                           (list :title "Open questions from note review"
                                                 :source-type 'meeting-note
                                                 :body-preview "Need to reconcile the support note with the current project state."
                                                 :analysis
                                                 (list :summary ""
                                                       :blockers '("Review blockers and dependencies")
                                                       :questions nil
                                                       :contacts nil
                                                       :entities '("Beta"))
                                                 :org-file-links '("/tmp/notes/beta-brief.org"))))
                               (cons 'match-project
                                     (list :accepted-output
                                           (list :best-project
                                                 (list :title "Beta Project"))))))))
                 (parsed '((retained_titles "Beta brief")
                           (items
                            (((title . "Beta brief")
                              (reasons "linked note mentions blockers"))))
                           (retained_context
                            . "- Current blockers - Follow up with operations")
                           (reason . "keeps blocker context for downstream extraction")))
                 (raw (delib-flow-local-test--filter-raw-output package parsed))
                 (context (plist-get raw :retained-context)))
            (should (string-match-p "Need to reconcile the support note with the current project state" context))
            (should (string-match-p "Review blockers and dependencies" context))
            (should (string-match-p "Relevant note: Beta brief" context)))
        (delete-file file)))))

(ert-deftest delib-flow-local-test-filter-raw-output-replaces-mismatched-note-context ()
  (delib-flow-test--with-local-test-config
    (let ((file (make-temp-file "delib-flow-note" nil ".org"
                                "#+title: Alpha brief\nKickoff context\n")))
      (unwind-protect
          (let* ((candidate (list :title "Alpha brief"
                                  :file file
                                  :score 5
                                  :reasons '("title-overlap=1 (+3)" "text-overlap=2 (+2)")))
                 (package
                  (list :source (list :title "Alpha Project kickoff")
                        :working-context
                        (list :retrieved-candidates (list candidate)
                              :review-results
                              (list
                               (cons 'inspect-source
                                     (list :accepted-output
                                           (list :title "Alpha Project kickoff"
                                                 :source-type 'email
                                                 :body-preview "Next steps include drafting a follow-up and preparing a timeline update."
                                                 :analysis
                                                 (list :summary "Waiting for Bob to confirm the launch date."
                                                       :blockers '("waiting on confirmation from Bob")
                                                       :questions nil
                                                       :contacts '("alice@example.com")
                                                       :entities '("Bob"))
                                                 :org-file-links '("/tmp/notes/alpha-brief.org"))))
                               (cons 'match-project
                                     (list :accepted-output
                                           (list :best-project
                                                 (list :title "Alpha Project"))))))))
                 (parsed '((retained_titles "Alpha brief")
                           (items
                            (((title . "Alpha brief")
                              (reasons "title overlap"))))
                           (retained_context
                            . "- Relevant note: Beta brief - Current blockers and follow-up points.\n- Need to reconcile the support note with the current project state.")
                           (reason . "bad copied example text")))
                 (raw (delib-flow-local-test--filter-raw-output package parsed))
                 (context (plist-get raw :retained-context)))
            (should (string-match-p "Relevant note: Alpha brief" context))
            (should-not (string-match-p "Beta brief" context))
            (should (string-match-p "Waiting for Bob to confirm the launch date" context)))
        (delete-file file)))))

(ert-deftest delib-flow-local-test-filter-raw-output-replaces-compressed-incomplete-context ()
  (delib-flow-test--with-local-test-config
    (let ((file (make-temp-file "delib-flow-note" nil ".org"
                                "#+title: Beta brief\nCurrent blockers\nFollow up with operations\n")))
      (unwind-protect
          (let* ((candidate (list :title "Beta brief"
                                  :file file
                                  :score 4
                                  :reasons '("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
                 (package
                  (list :source (list :title "Open questions from note review")
                        :working-context
                        (list :retrieved-candidates (list candidate)
                              :review-results
                              (list
                               (cons 'inspect-source
                                     (list :accepted-output
                                           (list :title "Open questions from note review"
                                                 :source-type 'meeting-note
                                                 :body-preview "Need to reconcile the support note with the current project state."
                                                 :analysis
                                                 (list :summary "Reconcile support note with current project state, review open blockers and dependencies."
                                                       :blockers '("open blockers")
                                                       :questions '("Open questions")
                                                       :contacts nil
                                                       :entities nil)
                                                 :org-file-links '("/tmp/notes/beta-brief.org"))))
                               (cons 'match-project
                                     (list :accepted-output
                                           (list :best-project
                                                 (list :title "Beta Project"))))))))
                 (parsed '((retained_titles "Beta brief")
                           (items
                            (((title . "Beta brief")
                              (reasons "linked note mentions blockers"))))
                           (retained_context
                            . "- Current need: reconcile the support note with the current project state. - Open dependency: follow up with operations.")
                           (reason . "compressed but incomplete")))
                 (raw (delib-flow-local-test--filter-raw-output package parsed))
                 (context (plist-get raw :retained-context)))
            (should (string-match-p "\n- Open blockers or dependencies:" context))
            (should (string-match-p "\n- Relevant note: Beta brief" context)))
        (delete-file file)))))

(ert-deftest delib-flow-local-test-filter-raw-output-rejects-non-candidate-retained-titles ()
  (delib-flow-test--with-local-test-config
    (let ((file (make-temp-file "delib-flow-note" nil ".org"
                                "#+title: Beta brief\nNeed to reconcile blockers.\n")))
      (unwind-protect
          (let* ((candidate (list :title "Beta brief"
                                  :file file
                                  :score 4
                                  :reasons '("title-overlap=1 (+3)" "text-overlap=1 (+1)")))
                 (package (list :working-context
                                (list :retrieved-candidates (list candidate))))
                 (parsed '((retained_titles "Open questions from note review")
                           (items
                            (((title . "Beta brief")
                              (reasons "linked note mentions blockers"))))
                           (retained_context
                            . "- Beta brief: reconcile blockers before next step.")
                           (reason . "keeps blocker context for downstream extraction")))
                 (raw (delib-flow-local-test--filter-raw-output package parsed)))
            (should (equal 1 (plist-get raw :retained-count)))
            (should (equal "Beta brief"
                           (plist-get (car (plist-get raw :retained-candidates))
                                      :title))))
        (delete-file file)))))

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
        (should (string-match-p "Write follow-up note"
                                (plist-get (car draft-items) :text)))
        (should (string-match-p "Review drafted actions"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (string-match-p "TODO"
                                (plist-get filing :preview-text)))))))

(ert-deftest delib-flow-extract-actions-adds-structured-quality-warnings ()
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
             (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                          :entries))))
             (raw (plist-get entry :raw-output))
             (actions (plist-get raw :actions)))
        (should (equal 3 (plist-get raw :candidate-count)))
        (should (equal 0 (plist-get raw :warning-count)))
        (should (equal 0 (plist-get raw :warning-item-count)))
        (should (equal 0 (plist-get raw :blocking-warning-count)))
        (should (equal 0 (plist-get raw :blocking-warning-item-count)))
        (should (equal 0 (length (plist-get (car actions) :warnings))))
        (should (string-match-p
                 "Write follow-up note for Alpha Project kickoff"
                 (plist-get (car actions) :text)))))))

(ert-deftest delib-flow-extract-actions-prefers-source-body-action-line ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nNext steps:\n- Send budget update to Alice\n- Review blockers\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (updated-run (delib-flow--run-stage-locally matched 'extract-actions))
           (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                        :entries))))
           (raw (plist-get entry :raw-output))
           (actions (plist-get raw :actions)))
      (should (equal "Send budget update to Alice"
                     (plist-get (car actions) :text)))
      (should (equal 0 (length (plist-get (car actions) :warnings)))))))

(ert-deftest delib-flow-extract-actions-source-title-strips-leading-org-timestamp ()
  (delib-flow-test--with-temp-project-file
      "* Project Atlas\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "[2026-02-24 Tue 22:37] Project Atlas"
                       :content "* [2026-02-24 Tue 22:37] Project Atlas\nContext only.\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (updated-run (delib-flow--run-stage-locally matched 'extract-actions))
           (actions (plist-get (plist-get (car (last (plist-get (plist-get updated-run :stage-history)
                                                                :entries)))
                                          :raw-output)
                               :actions)))
      (should (equal "Write follow-up note for Project Atlas"
                     (plist-get (car actions) :text))))))

(ert-deftest delib-flow-extract-actions-prefers-retained-candidate-action-line ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nConstraint detail.\n- Send revised scope to vendor\n"))
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
             (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                          :entries))))
             (actions (plist-get (plist-get entry :raw-output) :actions)))
        (should (member "Send revised scope to vendor"
                        (mapcar (lambda (item) (plist-get item :text))
                                actions)))))))

(ert-deftest delib-flow-action-warning-decision-state-is-blocking ()
  (let* ((item (delib-flow--make-draft-action "Keep Friday launch target" 'local-llm))
         (warning (delib-flow--action-warning-decision-state item)))
    (should warning)
    (should (eq 'blocking (plist-get warning :severity)))
    (should (eq 'decision-state-action (plist-get warning :code)))))

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

(ert-deftest delib-flow-draft-item-preview-lines-include-status-and-remediation ()
  (let* ((item (list :kind 'reference-note
                     :text "Create project support note from Alpha kickoff"
                     :tag-suggestions '("reference_note" "project_support" "alpha_project")
                     :warnings
                     (list
                      (delib-flow--make-artifact-warning
                       'reference-note-template-title
                       "Configured note template does not include `${title}`, so note-title filing readiness is weak."
                       'blocking))))
         (lines (delib-flow--draft-item-preview-lines item)))
    (should (member "  Status: blocked by 1 filing-readiness issue(s)" lines))
    (should (member "  Suggested tags: reference_note, project_support, alpha_project" lines))
    (should (member "  Blocking: Configured note template does not include `${title}`, so note-title filing readiness is weak." lines))
    (should (member "  Fix: Add `${title}` to the configured note template before approving this note." lines))))

(ert-deftest delib-flow-extract-actions-adds-tag-suggestions ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project :client:\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\n+ Gather launch risks\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (item (car (plist-get (plist-get drafted :filing) :draft-items))))
      (should (member "next_action" (plist-get item :tag-suggestions)))
      (should (member "alpha_project" (plist-get item :tag-suggestions)))
      (should (member "client" (plist-get item :tag-suggestions))))))

(ert-deftest delib-flow-suggest-reference-notes-adds-tag-suggestions ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project :client:\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nDecision: keep rollout checklist.\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (drafted (delib-flow--run-stage-locally matched 'suggest-reference-notes))
           (item (car (plist-get (plist-get drafted :filing) :draft-items))))
      (should (member "reference_note" (plist-get item :tag-suggestions)))
      (should (member "alpha_project" (plist-get item :tag-suggestions))))))

(ert-deftest delib-flow-suggest-reference-notes-uses-focus-tags-over-email-noise ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Your Consumption Diet Is Your Moat"
                     :content (concat
                               "* Your Consumption Diet Is Your Moat :email:\n"
                               ":PROPERTIES:\n"
                               ":FROM: Forte Labs Newsletter <hello@fortelabs.com>\n"
                               ":END:\n\n"
                               ":RAW_EMAIL:\n"
                               "From: Forte Labs Newsletter <hello@fortelabs.com>\n"
                               "To: me@example.com\n"
                               "Subject: Your Consumption Diet Is Your Moat\n"
                               "List-Unsubscribe: <mailto:unsub@example.com>\n\n"
                               "Week 1 was about the Master Prompt, PARA adapted for the AI era, and a capture system.\n"
                               ":END:\n"))))
         (inspected (delib-flow-test--accept-inspect
                     (delib-flow--run-stage-locally run 'inspect-source)))
         (drafted (delib-flow--run-stage-locally inspected 'suggest-reference-notes))
         (item (car (plist-get (plist-get drafted :filing) :draft-items))))
    (should (member "reference_note" (plist-get item :tag-suggestions)))
    (should-not (member "your" (plist-get item :tag-suggestions)))
    (should-not (member "email" (plist-get item :tag-suggestions)))
    (should (seq-some (lambda (tag)
                        (member tag (plist-get item :tag-suggestions)))
                      '("master" "prompt" "para" "capture")))))

(ert-deftest delib-flow-propose-new-project-adds-tag-suggestions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Presentation or article on focusing on the end user"
                       :content "* Presentation or article on focusing on the end user\n+ Gather success cases\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (item (car (plist-get (plist-get proposed :filing) :draft-items))))
      (should (member "project" (plist-get item :tag-suggestions)))
      (should (member "focusing" (plist-get item :tag-suggestions)))
      (should (member "end" (plist-get item :tag-suggestions)))
      (should (member "user" (plist-get item :tag-suggestions))))))

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
                (should (search-forward "- Warning count: 0" nil t))
                (should (search-forward "- Blocking warning count: 0" nil t))
                (goto-char (point-min))
                (should (search-forward "A filing queue is ready." nil t))
                (should (search-forward "- Quality warnings: 0 across 0 artifact(s)." nil t))
                (should (search-forward "- Blocking warnings: 0 across 0 artifact(s)." nil t))
                (should (search-forward "- TODO Write follow-up note for Alpha Project kickoff" nil t))
                (should-not (search-forward "Warning:" nil t))
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
        (should (string-match-p "Waiting for confirmation from project owner on"
                                (plist-get (car draft-items) :text)))
        (should (delib-flow--draft-item-has-warning-code-p
                 (car draft-items)
                 'waiting-for-speculative-owner))
        (should (string-match-p "Review drafted waiting-for items"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (string-match-p "WAITING"
                                (plist-get filing :preview-text)))))))

(ert-deftest delib-flow-draft-stages-accumulate-artifacts-within-one-run ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (actions-run (delib-flow--run-stage-locally matched 'extract-actions))
           (waiting-run (delib-flow--run-stage-locally actions-run 'extract-waiting-for))
           (items (plist-get (plist-get waiting-run :filing) :draft-items))
           (texts (mapcar (lambda (item) (plist-get item :text)) items))
           (stages (mapcar #'delib-flow--draft-item-stage items)))
      (should (= 2 (length items)))
      (should (member "Write follow-up note for Alpha Project kickoff" texts))
      (should (seq-some (lambda (text)
                          (string-match-p "Waiting for confirmation" text))
                        texts))
      (should (member 'extract-actions stages))
      (should (member 'extract-waiting-for stages)))))

(ert-deftest delib-flow-extract-waiting-for-adds-structured-quality-warnings ()
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
             (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                          :entries))))
             (raw (plist-get entry :raw-output))
             (items (plist-get raw :waiting-fors)))
        (should (equal 3 (plist-get raw :candidate-count)))
        (should (= 3 (plist-get raw :warning-count)))
        (should (= 3 (plist-get raw :warning-item-count)))
        (should (equal 0 (plist-get raw :blocking-warning-count)))
        (should (equal 0 (plist-get raw :blocking-warning-item-count)))
        (should (delib-flow--draft-item-has-warning-code-p
                 (car items)
                 'waiting-for-speculative-owner))
        (should-not (delib-flow--draft-item-has-warning-code-p
                     (car items)
                     'waiting-for-speculative-blocker))
        (should (string-match-p
                 "Waiting for confirmation from project owner on Alpha Project kickoff"
                 (plist-get (car items) :text)))))))

(ert-deftest delib-flow-local-waiting-for-items-fall-back-when-llm-items-are-all-blocked ()
  (let* ((package (list :source (list :title "Alpha Project kickoff"
                                      :content "* Alpha Project kickoff\nBody line\n")))
         (items
          (delib-flow-local-test--artifact-items
           '(("items" . ["Need update" "Awaiting review"]))
           'local-llm
           'waiting-fors
           package)))
    (should (equal 1 (length items)))
    (should (equal "Waiting for confirmation from project owner on Alpha Project kickoff"
                   (plist-get (car items) :text)))
    (should (delib-flow--draft-item-has-warning-code-p
             (car items)
             'waiting-for-speculative-owner))))

(ert-deftest delib-flow-waiting-for-missing-owner-is-blocking ()
  (let* ((item (list :kind 'waiting-for
                     :text "Waiting for confirmation on the revised budget"))
         (warning (delib-flow--waiting-for-warning-missing-owner item)))
    (should warning)
    (should (eq 'blocking (plist-get warning :severity)))))

(ert-deftest delib-flow-waiting-for-role-owner-is-accepted ()
  (dolist (text '("Waiting for finance to approve the revised amount."
                  "Waiting for legal to confirm the indemnity clause."
                  "Waiting for the vendor contact to return the signed schedule."))
    (let* ((item (list :kind 'waiting-for :text text))
           (warning (delib-flow--waiting-for-warning-missing-owner item)))
      (should-not warning))))

(ert-deftest delib-flow-waiting-for-vague-blocker-is-blocking ()
  (let* ((item (list :kind 'waiting-for
                     :text "Waiting for a concrete response about Alpha kickoff"))
         (warning (delib-flow--waiting-for-warning-vague-blocker item)))
    (should warning)
    (should (eq 'blocking (plist-get warning :severity)))))

(ert-deftest delib-flow-extract-waiting-for-prefers-source-waiting-line ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nAwaiting Alice confirmation on the revised budget.\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (updated-run
            (delib-flow--run-stage-locally matched 'extract-waiting-for))
           (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                        :entries))))
           (raw (plist-get entry :raw-output))
           (items (plist-get raw :waiting-fors)))
      (should (equal "Waiting for Alice confirmation on the revised budget"
                     (plist-get (car items) :text)))
      (should (equal 0 (length (plist-get (car items) :warnings)))))))

(ert-deftest delib-flow-extract-waiting-for-prefers-retained-candidate-waiting-line ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nWaiting for vendor approval on contract wording.\n"))
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
             (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                          :entries))))
             (items (plist-get (plist-get entry :raw-output) :waiting-fors)))
        (should (member "Waiting for vendor approval on contract wording"
                        (mapcar (lambda (item) (plist-get item :text))
                                items)))))))

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
                (should (search-forward "- Warning count: 3" nil t))
                (should (search-forward "- Blocking warning count: 0" nil t))
                (goto-char (point-min))
                (should (search-forward "A filing queue is ready." nil t))
                (should (search-forward "- Quality warnings: 3 across 3 artifact(s)." nil t))
                (should (search-forward "- Blocking warnings: 0 across 0 artifact(s)." nil t))
                (should (search-forward "- WAITING Waiting for confirmation from project owner on Alpha Project kickoff" nil t))
                (should (search-forward "Warning: Uses the generic owner placeholder `project owner`" nil t))
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
        (should (string-match-p "Create project support note"
                                (plist-get (car draft-items) :text)))
        (should (string-match-p "Review drafted reference notes"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))
        (should (string-match-p "NOTE"
                                (plist-get filing :preview-text)))))))

(ert-deftest delib-flow-suggest-reference-notes-uses-matched-project-context ()
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
              (delib-flow--run-stage-locally filtered
                                             'suggest-reference-notes))
             (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                          :entries))))
             (raw (plist-get entry :raw-output))
             (items (plist-get raw :reference-notes)))
        (should (equal 2 (plist-get raw :candidate-count)))
        (should (equal 0 (plist-get raw :warning-count)))
        (should (equal 0 (plist-get raw :warning-item-count)))
        (should (equal 0 (plist-get raw :blocking-warning-count)))
        (should (equal 0 (plist-get raw :blocking-warning-item-count)))
        (should (eq 'project-support (plist-get (car items) :note-type)))
        (should (equal 0 (length (plist-get (car items) :warnings))))
        (should (string-match-p
                 "Create project support note from Alpha Project kickoff"
                 (plist-get (car items) :text)))))))

(ert-deftest delib-flow-suggest-reference-notes-prefers-retained-candidate-focus-line ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Misc Notes\nDecision: keep vendor scope frozen until approval.\n"))
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
              (delib-flow--run-stage-locally filtered
                                             'suggest-reference-notes))
             (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                          :entries))))
             (items (plist-get (plist-get entry :raw-output) :reference-notes)))
        (should (member "Create project support note from Decision: keep vendor scope frozen until approval"
                        (mapcar (lambda (item) (plist-get item :text))
                                items)))))))

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
                (should (search-forward "- Candidate count: 2" nil t))
                (should (search-forward "- Warning count: 0" nil t))
                (goto-char (point-min))
                (should (search-forward "*** Focused filing workspace" nil t))
                (should (search-forward "Reference-note candidates are ready." nil t))
                (should (search-forward "press `F` to open the focused filing workspace" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Suggest Reference Notes [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-draft-selected-reference-note-action-available-for-selected-note ()
  (let* ((note (list :kind 'reference-note
                     :text "Create general PKM note for Durable idea"
                     :note-type 'general-pkm
                     :warnings nil))
         (run (delib-flow--set-artifact-family-candidates
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody.\n"))
                :filing
                (list :draft-items (list note)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'reference-notes
               (list note)))
         (selected (delib-flow--set-filing-selection-value
                    (delib-flow-test--accept-inspect run)
                    "1"))
         (action (delib-flow--draft-selected-reference-note-action selected)))
    (should action)
    (should (eq 'draft-selected-reference-note (plist-get action :id)))
    (should (equal "Draft Selected Note" (plist-get action :label)))))

(ert-deftest delib-flow-find-support-for-selected-reference-note-action-available-for-selected-note ()
  (let* ((note (list :kind 'reference-note
                     :text "Create general PKM note for Durable idea"
                     :note-type 'general-pkm
                     :warnings nil))
         (run (delib-flow--set-artifact-family-candidates
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody.\n"))
                :filing
                (list :draft-items (list note)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'reference-notes
               (list note)))
         (selected (delib-flow--set-filing-selection-value
                    (delib-flow-test--accept-inspect run)
                    "1"))
         (action (delib-flow--find-support-for-selected-reference-note-action selected)))
    (should action)
    (should (eq 'find-support-for-selected-reference-note
                (plist-get action :id)))
    (should (equal "Find Support for Selected Note"
                   (plist-get action :label)))))

(ert-deftest delib-flow-draft-selected-action-action-available-for-selected-action ()
  (let* ((item (list :kind 'next-action
                     :text "Draft kickoff follow-up"
                     :warnings nil))
         (run (delib-flow--set-artifact-family-candidates
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody.\n"))
                :filing
                (list :draft-items (list item)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'actions
               (list item)))
         (selected (delib-flow--set-filing-selection-value
                    (delib-flow-test--accept-inspect
                     (delib-flow-test--accept-match run))
                    "1"))
         (action (delib-flow--draft-selected-action-action selected)))
    (should action)
    (should (eq 'draft-selected-action (plist-get action :id)))
    (should (equal "Draft Selected Action" (plist-get action :label)))))

(ert-deftest delib-flow-draft-selected-waiting-for-action-available-for-selected-waiting-for ()
  (let* ((item (list :kind 'waiting-for
                     :text "Waiting for Pat to confirm launch date"
                     :warnings nil))
         (run (delib-flow--set-artifact-family-candidates
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody.\n"))
                :filing
                (list :draft-items (list item)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'waiting-fors
               (list item)))
         (selected (delib-flow--set-filing-selection-value
                    (delib-flow-test--accept-inspect
                     (delib-flow-test--accept-match run))
                    "1"))
         (action (delib-flow--draft-selected-waiting-for-action selected)))
    (should action)
    (should (eq 'draft-selected-waiting-for (plist-get action :id)))
    (should (equal "Draft Selected Waiting-For" (plist-get action :label)))))

(ert-deftest delib-flow-draft-selected-project-action-available-for-selected-project ()
  (let* ((item (list :kind 'project
                     :title "Project Atlas"
                     :text "Project Atlas"
                     :state 'active
                     :first-item (list :kind 'next-action
                                       :text "Define first deliverable for Project Atlas")
                     :warnings nil))
         (run (delib-flow--set-artifact-family-candidates
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody.\n"))
                :filing
                (list :draft-items (list item)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'project-proposals
               (list item)))
         (selected (delib-flow--set-filing-selection-value
                    (delib-flow-test--accept-inspect
                     (delib-flow-test--accept-match run))
                    "1"))
         (action (delib-flow--draft-selected-project-action selected)))
    (should action)
    (should (eq 'draft-selected-project (plist-get action :id)))
    (should (equal "Draft Selected Project" (plist-get action :label)))))

(ert-deftest delib-flow-decide-cloud-pass-allows-rerouted-target-stage-selection ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run
          (delib-flow--run-stage-locally
           (delib-flow-test--set-cloud-target-stage inspected 'extract-actions)
           'decide-cloud-pass))
         (routing (plist-get updated-run :routing))
         (entry (car (last (plist-get (delib-flow--run-stage-history updated-run)
                                      :entries)))))
    (should (eq 'extract-actions
                (plist-get routing :cloud-target-stage)))
    (should (eq 'extract-actions
                (plist-get (plist-get entry :raw-output) :target-stage)))
    (should (string-match-p "Target stage: extract-actions"
                            (plist-get entry :normalized-output)))))

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
            (should (search-forward "- Cloud context: Cloud pass selected for Run Cloud Stage. Model:" nil t))
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
         (sanitized
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (delib-flow--active-run
          (delib-flow--set-editable-block
           sanitized
           'cloud-package-review
           (delib-flow--set-editable-block-text
            (delib-flow--editable-block sanitized 'cloud-package-review)
            "Approved reviewed cloud package.")))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
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

(ert-deftest delib-flow-run-cloud-stage-reports-rerouted-target-stage ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alice Example"
                     :content "* Alice Example\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (cloud-decided
          (delib-flow--run-stage-locally
           (delib-flow-test--set-cloud-target-stage inspected 'extract-actions)
           'decide-cloud-pass))
         (sanitized
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (approved
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (updated-run (delib-flow--run-stage-in-cloud approved 'run-cloud-stage))
         (entry (car (last (plist-get (delib-flow--run-stage-history updated-run)
                                      :entries))))
         (cloud-output (plist-get (delib-flow--run-working-context updated-run)
                                  :cloud-returned-context)))
    (should (eq 'extract-actions
                (plist-get (plist-get entry :raw-output) :target-stage)))
    (should (string-match-p "Target stage: extract-actions"
                            (plist-get entry :normalized-output)))
    (should (string-match-p "Cloud output for rerouted stage Extract Actions"
                            cloud-output))))


(ert-deftest delib-flow-run-cloud-stage-rerouted-path-offers-direct-retry-and-restart ()
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
         (updated-run (delib-flow--run-stage-in-cloud approved 'run-cloud-stage))
         (actions (mapcar (lambda (action)
                            (plist-get action :id))
                          (plist-get (delib-flow--run-actions updated-run)
                                     :items))))
    (should (member 'retry-rerouted-cloud-stage actions))
    (should (member 'restart-cloud-path actions))))

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
            (should (search-forward "Cloud output for rerouted stage Run Cloud Stage" nil t))
            (goto-char (point-min))
            (should (search-forward "Cloud-returned context: available" nil t))
            (goto-char (point-min))
            (should (search-forward "- Cloud-returned summary: Cloud output for rerouted stage Run Cloud Stage" nil t))
            (goto-char (point-min))
            (should (search-forward "- Retry Run Cloud Stage [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "- Approve Candidate Reintegration [available]" nil t))
            (goto-char (point-min))
            (should-not (search-forward "- Integrate into Source [available]" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-run-cloud-stage-failure-records-rerouted-cloud-failure-stage ()
  (let* ((delib-flow-cloud-stage-adapter
          (lambda (_descriptor _package)
            (error "cloud timeout")))
         (run (delib-flow--initialize-run
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
         (approved-send
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (updated-run (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (entries (plist-get (delib-flow--run-stage-history updated-run) :entries))
         (shadow-entry
          (seq-find (lambda (item)
                      (and (eq 'extract-actions (plist-get item :stage-id))
                           (plist-get item :cloud-shadow-p)))
                    entries))
         (routing (plist-get updated-run :routing))
         (buffer (delib-flow--render-control-buffer updated-run)))
    (should shadow-entry)
    (should (equal 'failed (plist-get shadow-entry :status)))
    (should (equal 'cloud (plist-get shadow-entry :provider)))
    (should (eq 'extract-actions (plist-get routing :cloud-failure-stage)))
    (should (string-match-p "Extract Actions"
                            (plist-get shadow-entry :normalized-output)))
    (should (string-match-p "Review cloud failure for Extract Actions"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "- Stage: Extract Actions" nil t))
          (goto-char (point-min))
          (should (search-forward "- Transport stage: Run Cloud Stage" nil t))
          (goto-char (point-min))
          (should (search-forward "- Failed cloud target stage: Extract Actions" nil t))
          (goto-char (point-min))
          (should (search-forward "- Resolve Cloud Failure (Extract Actions) [available]" nil t))
          (goto-char (point-min))
          (should (search-forward "- Cloud target stage: Extract Actions" nil t))
          (goto-char (point-min))
          (should (search-forward "- Transport stage: Run Cloud Stage" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-retry-rerouted-cloud-stage-command-reruns-cloud-target ()
  (let* ((delib-flow-cloud-stage-adapter
          (lambda (_descriptor package)
            (let ((raw (delib-flow--cloud-stage-result package)))
              (plist-put
               raw :cloud-output
               (concat (plist-get raw :cloud-output)
                       "\nRetried cloud attempt.")))))
         (run (delib-flow--initialize-run
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
         (delib-flow--active-run
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-run-cloud-stage)
          (delib-flow-action-retry-rerouted-cloud-stage)
          (let* ((entries (plist-get (delib-flow--run-stage-history delib-flow--active-run)
                                     :entries))
                 (shadow-count
                  (length
                   (seq-filter (lambda (item)
                                 (and (eq 'extract-actions (plist-get item :stage-id))
                                      (plist-get item :cloud-shadow-p)))
                               entries))))
            (should (= 2 shadow-count))
            (should (string-match-p
                     "Retried cloud attempt."
                     (plist-get (delib-flow--run-working-context delib-flow--active-run)
                                :cloud-returned-context)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-restart-cloud-path-command-resets-to-sanitization ()
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
         (delib-flow--active-run
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-run-cloud-stage)
          (delib-flow-action-restart-cloud-path)
          (let* ((working (delib-flow--run-working-context delib-flow--active-run))
                 (routing (plist-get delib-flow--active-run :routing))
                 (actions (mapcar (lambda (action)
                                    (plist-get action :id))
                                  (plist-get (delib-flow--run-actions delib-flow--active-run)
                                             :items))))
            (should-not (plist-get working :cloud-returned-context))
            (should-not (plist-get working :cloud-sanitized-context))
            (should (eq 'required (plist-get routing :sanitization-status)))
            (should (plist-get routing :cloud-switch-pending))
            (should-not (plist-get routing :reintegration-status))
            (should (member 'sanitize-for-cloud actions))
            (should-not (member 'retry-rerouted-cloud-stage actions))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-resolve-cloud-failure-command-aborts-run ()
  (let* ((delib-flow-cloud-stage-adapter
          (lambda (_descriptor _package)
            (error "cloud timeout")))
         (run (delib-flow--initialize-run
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
          (delib-flow-test--set-cloud-failure-resolution
           (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage)
           "ABORT"))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-resolve-cloud-failure)
          (should (null delib-flow--active-run))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

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

(ert-deftest delib-flow-integrate-into-source-applies-rerouted-cloud-stage-output ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\n- TODO Draft kickoff follow-up for Alpha Project\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nContact alice@example.com\nAction items:\n- Draft kickoff follow-up for Alpha Project\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (cloud-decided
              (delib-flow--run-stage-locally
               (delib-flow-test--set-cloud-target-stage matched 'extract-actions)
               'decide-cloud-pass))
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
             (working (delib-flow--run-working-context updated-run))
             (filing (plist-get updated-run :filing))
             (draft-items (plist-get filing :draft-items))
             (entries (plist-get (delib-flow--run-stage-history updated-run) :entries))
             (shadow-entry
              (seq-find (lambda (item)
                          (and (eq 'extract-actions (plist-get item :stage-id))
                               (plist-get item :cloud-shadow-p)))
                        entries))
             (actions (mapcar (lambda (action)
                                (plist-get action :id))
                              (plist-get (delib-flow--run-actions updated-run)
                                         :items))))
        (should (eq 'extract-actions
                    (plist-get working :cloud-returned-stage-id)))
        (should shadow-entry)
        (should (plist-get shadow-entry :applied-p))
        (should (equal 'accepted (plist-get shadow-entry :review-state)))
        (should (delib-flow--stage-executed-p updated-run 'extract-actions))
        (should draft-items)
        (should (string-match-p "Draft kickoff follow-up"
                                (plist-get (car draft-items) :text)))
        (should (member 'select-approved-filing-actions actions))
        (should (member 'reject-draft-filing-artifact actions))))))

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
                (should (search-forward "*** Available queue" nil t))
                (should (search-forward "Write follow-up note for Alpha Project kickoff" nil t))
                (goto-char (point-min))
                (should (search-forward "Cloud-reviewed context" nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry Integrate into Source [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))





(ert-deftest delib-flow-select-approved-filing-actions-can-choose-ready-artifact-after-block ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alpha Project kickoff"
                     :content "* Alpha Project kickoff\nAgenda\n")))
         (filing
          (list :draft-items
                (list
                 (list :kind 'reference-note
                       :text "Create project support note from Alpha kickoff"
                       :warnings
                       (list
                        (delib-flow--make-artifact-warning
                         'reference-note-template-title
                         "Configured note template does not include `${title}`, so note-title filing readiness is weak."
                         'blocking)))
                 (list :kind 'next-action
                       :text "Send budget update to Alice"
                       :warnings nil))))
         (prepared
          (delib-flow--seed-filing-selection-block
           (plist-put run :filing filing)))
         (blocked
          (let* ((selected (delib-flow-test--set-filing-selection prepared "1"))
                 (with-draft
                  (delib-flow--set-artifact-family-selected-draft
                   selected
                   'reference-notes
                   (plist-put
                    (copy-tree (car (plist-get filing :draft-items)))
                    :draft-body "* Working draft\nBlocked note.\n"))))
            (delib-flow--run-stage-locally
             with-draft
             'select-approved-filing-actions)))
         (selected
          (delib-flow-test--approve-selected-filing-item
           (delib-flow-test--set-filing-selection blocked "2")))
         (entry (car (last (plist-get (plist-get selected :stage-history)
                                      :entries))))
         (raw (plist-get entry :raw-output))
         (updated-filing (plist-get selected :filing))
         (approved (car (plist-get updated-filing :approved-items))))
    (should approved)
    (should (string-match-p "Send budget update to Alice"
                            (plist-get approved :text)))
    (should-not (plist-get raw :approval-blocked-p))
    (should-not (plist-get raw :blocked-item))
    (should-not (plist-get raw :blocked-item-preview))
    (should-not (plist-get updated-filing :selection-blocking-warnings))
    (should-not (plist-get updated-filing :selection-blocked-item))
    (should-not (plist-get updated-filing :selection-blocked-selection))
    (should-not (plist-get updated-filing :selection-blocked-notes))))

(ert-deftest delib-flow-select-approved-filing-actions-blocks-decision-state-action ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Q3 launch coordination"
                     :content "* Q3 launch coordination\nAgenda\n")))
         (filing
          (list :draft-items
                (list
                 (delib-flow--draft-item-with-warnings
                  (list :kind 'next-action
                        :text "Keep Friday launch target"
                        :source 'local-llm)
                  (list
                   (delib-flow--make-artifact-warning
                    'decision-state-action
                    "Reads like a decision or status statement rather than a directly executable next action."
                    'blocking)))
                 (list :kind 'next-action
                       :text "Draft vendor follow-up email"
                       :warnings nil))))
         (prepared
          (delib-flow--seed-filing-selection-block
           (plist-put run :filing filing)))
         (blocked
          (delib-flow--run-stage-locally
           (delib-flow-test--set-filing-selection prepared "1")
           'select-approved-filing-actions))
         (entry (car (last (plist-get (plist-get blocked :stage-history)
                                      :entries))))
         (raw (plist-get entry :raw-output))
         (blocked-warning (car (plist-get raw :blocking-warnings))))
    (should (plist-get raw :approval-blocked-p))
    (should (equal '(1) (plist-get raw :blocked-selection-indexes)))
    (should (eq 'decision-state-action
                (plist-get blocked-warning :code)))
    (should (string-match-p "decision or status statement"
                            (plist-get blocked-warning :message)))))

(ert-deftest delib-flow-select-approved-filing-actions-renders-blocked-preview-details ()
  (let ((delib-flow-project-support-note-capture-template
         "#+filetags: :project:support:\n\nSource artifact: %(delib-flow-capture-source-artifact)\n"))
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
               (drafted
                (delib-flow--run-stage-locally filtered 'suggest-reference-notes))
               (selected
                (delib-flow--run-stage-locally
                 (delib-flow-test--set-filing-selection
                  (delib-flow--run-stage-locally drafted 'integrate-into-source)
                  "1"
                  "Needs a valid support-note template.")
                 'select-approved-filing-actions))
               (buffer (delib-flow--render-control-buffer selected)))
          (unwind-protect
              (with-current-buffer buffer
                (goto-char (point-min))
                (should (search-forward "*** Current blocked state" nil t))
                (should (search-forward "The last approval attempt is blocked." nil t))
                (should (search-forward "- Operator selection: 1" nil t))
                (should (search-forward "- Operator notes: Needs a valid support-note template." nil t))
                (should (search-forward "- Ready alternative selections: none" nil t))
                (should (search-forward "- Resolution: approve a different ready artifact, fix the blocking warnings, or reject this artifact before retrying approval." nil t))
                (should (search-forward "- Blocked artifact:" nil t))
                (should (search-forward "Create project support note from Alpha Project kickoff" nil t))
                (should (search-forward "Blocking: Configured note template does not include a note-title expansion, so note-title filing readiness is weak." nil t))
                (should (search-forward "Fix: Add `${title}` to the configured note template before approving this note." nil t))
                (goto-char (point-min))
                (should (search-forward "- Blocking warning: Configured note template does not include a note-title expansion, so note-title filing readiness is weak." nil t)))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

(ert-deftest delib-flow-extract-actions-clears-stale-filing-approval-blocks ()
  (let ((delib-flow-project-support-note-capture-template
         "#+filetags: :project:support:\n\nSource artifact: %(delib-flow-capture-source-artifact)\n"))
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
               (blocked
                (delib-flow--run-stage-locally
                 (delib-flow-test--set-filing-selection
                  (delib-flow--run-stage-locally
                   (delib-flow--run-stage-locally filtered 'suggest-reference-notes)
                   'integrate-into-source)
                  "1")
                 'select-approved-filing-actions))
               (rerun
                (delib-flow--run-stage-locally blocked 'extract-actions))
               (filing (plist-get rerun :filing))
               (items (plist-get filing :draft-items))
               (stages (mapcar #'delib-flow--draft-item-stage items)))
          (should-not (plist-get filing :selection-blocking-warnings))
          (should-not (plist-get filing :selection-blocked-item))
          (should-not (plist-get filing :selection-blocked-selection))
          (should-not (plist-get filing :selection-blocked-notes))
          (should (equal 5 (length items)))
          (should (equal 3 (seq-count (lambda (stage)
                                        (eq stage 'extract-actions))
                                      stages)))
          (should (equal 2 (seq-count (lambda (stage)
                                        (eq stage 'suggest-reference-notes))
                                      stages))))))))

(ert-deftest delib-flow-integrate-into-source-renders-filing-selection-block ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (updated-run (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (buffer (delib-flow--render-control-buffer updated-run)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Artifact selection" nil t))
            (should (search-forward "Pick one queue item at a time." nil t))
            (should (search-forward "Multiple indexes such as `1, 3` are invalid here." nil t))
            (should (search-forward "Run `Select Approved Filing Actions` to approve the chosen ready item" nil t))
            (should (search-forward "**** Choose from queue" nil t))
            (should (search-forward "- [1] Ready to approve" nil t))
            (should (search-forward "TODO Write follow-up note for Alpha Project kickoff" nil t))
            (should (search-forward "Files to: Alpha Project" nil t))
            (should (search-forward "Warnings: 0 | Kind: next-action" nil t))
            (should (search-forward "**** Selection form" nil t))
            (should (search-forward "Selection:" nil t))
            (should (search-forward "Draft artifacts:" nil t))
            (should (search-forward "- [1] TODO Write follow-up note for Alpha Project kickoff" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-choose-filing-selection-command-populates-selection-block ()
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
           (labels (delib-flow--filing-selection-labels delib-flow--active-run))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _args)
                         (caar labels))))
              (delib-flow-choose-filing-selection))
            (should (string-match-p "Selection: 1"
                                    (delib-flow--editable-block-text
                                     (delib-flow--editable-block delib-flow--active-run
                                                                 'filing-selection-review))))
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "Selection: 1" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-choose-filing-selection-command-works-from-focused-filing-workspace ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'suggest-reference-notes))
           (delib-flow--active-run
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (labels (delib-flow--filing-selection-labels delib-flow--active-run))
           (expected-id (delib-flow--artifact-candidate-id
                         (car (plist-get (plist-get delib-flow--active-run :filing)
                                         :draft-items))))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-open-filing-workspace)
            (with-current-buffer (get-buffer delib-flow-filing-workspace-buffer-name)
              (should (eq #'delib-flow-control-choose-filing-selection
                          (lookup-key delib-flow-filing-workspace-mode-map
                                      (kbd "s")))))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _args)
                         (caar labels))))
              (with-current-buffer (get-buffer delib-flow-filing-workspace-buffer-name)
                (delib-flow-choose-filing-selection)))
            (should (string-match-p "Selection: 1"
                                    (delib-flow--editable-block-text
                                     (delib-flow--editable-block delib-flow--active-run
                                                                 'filing-selection-review))))
            (should (equal "1"
                           (delib-flow--filing-selection-value delib-flow--active-run)))
            (should (equal expected-id
                           (delib-flow--artifact-family-selected-candidate-id
                            delib-flow--active-run
                            'reference-notes))))
        (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
          (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-selected-workspace-local-action-palette-lines-are-dispatchable ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer selected-run)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "**** Local action palette: action")
            (search-forward "- Draft Selected Action [available]")
            (goto-char (line-beginning-position))
            (let ((action (get-text-property (point) 'delib-flow-action)))
              (should action)
              (should (eq 'draft-selected-action
                          (plist-get action :id)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(ert-deftest delib-flow-file-approved-outputs-uses-project-child-capture-template ()
  (let ((delib-flow-next-action-capture-template
         "%(delib-flow-capture-project-child-heading)\n:PROPERTIES:\n:SOURCE_ARTIFACT: %(delib-flow-capture-source-artifact)\n:END:\n"))
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
              (delib-flow-test--set-filing-selection integrated "1"))
             (enriched
              (delib-flow-test--draft-selected-filing-item selected))
             (approved
              (delib-flow--run-stage-locally enriched
                                             'select-approved-filing-actions))
             (_updated-run
              (delib-flow--run-stage-locally approved 'file-approved-outputs))
             (content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
        (should (string-match-p
                 "\\*\\* TODO Write follow-up note for Alpha Project kickoff"
                 content))
        (should (string-match-p
                 ":SOURCE_ARTIFACT: Write follow-up note for Alpha Project kickoff"
                 content))))))

(ert-deftest delib-flow-file-approved-outputs-default-project-child-metadata-includes-tags-created-and-source ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((action (list :kind 'next-action
                         :text "Analyze website's storage for exercises"
                         :source "https://example.com/drill?id=one"
                         :tag-suggestions '("next_action" "issue_note" "steno")
                         :warnings nil))
           (base-run (delib-flow--initialize-run
                      (list :title "Broken steno exercise"
                            :content "* Broken steno exercise\nBody line\n")))
           (matched-run
            (plist-put
             base-run
             :working-context
             (plist-put (delib-flow--run-working-context base-run)
                        :project-match
                        (list :match-status 'matched
                              :best-project (list :title "Alpha Project")))))
           (run (delib-flow--seed-filing-selection-block
                 (delib-flow--set-artifact-family-state
                  (plist-put
                   matched-run
                   :filing
                   (list :draft-items (list action)
                         :approved-items nil
                         :rejected-items nil
                         :preview-text nil
                         :selection-blocked-item nil
                         :selection-blocking-warnings nil
                         :selection-blocked-selection nil
                         :selection-blocked-notes nil
                         :conflicts nil
                         :target-locations nil))
                  'actions
                  (list :candidates (list action)
                        :selected-candidate-id
                        (delib-flow--artifact-candidate-id action)
                        :selected-draft action))))
           (selected (delib-flow-test--set-filing-selection run "1"))
           (approved (delib-flow--run-stage-locally selected 'select-approved-filing-actions))
           (_updated-run (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
      (should (string-match-p "\\*\\* TODO Analyze website's storage for exercises" content))
      (should (string-match-p ":PROPERTIES:" content))
      (should (string-match-p ":TAGS: next_action issue_note steno" content))
      (should (string-match-p ":CREATED: " content))
      (should (string-match-p ":SOURCE_ARTIFACT: https://example.com/drill\\?id=one" content)))))

(ert-deftest delib-flow-file-approved-new-project-children-include-waiting-metadata ()
  (delib-flow-test--with-temp-project-file
      "* Active :active:\n* Complete :complete:\n* Waiting :waiting:\n"
    (let* ((project (list :kind 'project
                          :title "Steno Exercise Fix"
                          :text "Steno Exercise Fix"
                          :state 'active
                          :child-items
                          (list (list :kind 'next-action
                                      :text "Analyze website's storage for exercises"
                                      :source "https://example.com/drill?id=one"
                                      :tag-suggestions '("next_action" "issue_note" "steno"))
                                (list :kind 'waiting-for
                                      :text "Confirm how exercises are stored in production"
                                      :source "operator intent"
                                      :tag-suggestions '("waiting_for" "issue_note" "steno")))
                          :warnings nil))
           (run (delib-flow--seed-filing-selection-block
                 (delib-flow--set-artifact-family-state
                  (plist-put
                   (delib-flow--initialize-run
                    (list :title "Broken steno exercise"
                          :content "* Broken steno exercise\nBody line\n"))
                   :filing
                   (list :draft-items (list project)
                         :approved-items nil
                         :rejected-items nil
                         :preview-text nil
                         :selection-blocked-item nil
                         :selection-blocking-warnings nil
                         :selection-blocked-selection nil
                         :selection-blocked-notes nil
                         :conflicts nil
                         :target-locations nil))
                  'project-proposals
                  (list :candidates (list project)
                        :selected-candidate-id
                        (delib-flow--artifact-candidate-id project)
                        :selected-draft project))))
           (selected (delib-flow-test--set-filing-selection run "1"))
           (approved (delib-flow--run-stage-locally selected 'select-approved-filing-actions))
           (_updated (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
      (should (string-match-p "\\*\\* Steno Exercise Fix" content))
      (should (string-match-p "\\*\\*\\* TODO Analyze website's storage for exercises" content))
      (should (string-match-p "\\*\\*\\* WAITING Confirm how exercises are stored in production" content))
      (should (string-match-p ":TAGS: waiting_for issue_note steno" content))
      (should (string-match-p ":SOURCE_ARTIFACT: operator intent" content))
      (should (string-match-p ":CREATED: " content)))))

(ert-deftest delib-flow-file-approved-outputs-finds-state-bucket-project-headings-with-tags ()
  (delib-flow-test--with-temp-project-file
      "* Active :active:\n** Project Atlas :example:product:\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Project Atlas"
                       :content "* Project Atlas\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected
            (delib-flow-test--set-filing-selection integrated "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (updated-run
            (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (location (car (plist-get (plist-get updated-run :filing)
                                     :target-locations)))
           (heading (format "*** TODO %s" (plist-get location :item-text))))
      (should (string-match-p
               (regexp-quote heading)
               (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
      (should (string-match-p "Project Atlas"
                              (plist-get location :target))))))


(ert-deftest delib-flow-file-approved-outputs-uses-drafted-selected-reference-note-body ()
  (let ((delib-flow-general-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\n"))
    (delib-flow-test--with-temp-zk-root ()
      (let* ((candidate (list :kind 'reference-note
                              :text "Create general PKM note for Project Atlas Pattern"
                              :note-type 'general-pkm
                              :warnings nil))
             (drafted-item (plist-put (copy-tree candidate)
                                      :draft-body
                                      "* Working draft\nAtlas draft body.\n"))
             (run (delib-flow--seed-filing-selection-block
                   (delib-flow--set-artifact-family-state
                    (plist-put
                     (delib-flow--initialize-run
                      (list :title "Project Atlas source"
                            :content "* Project Atlas source\nBody line.\n"))
                     :filing
                     (list :draft-items (list candidate)
                           :approved-items nil
                           :rejected-items nil
                           :preview-text nil
                           :selection-blocked-item nil
                           :selection-blocking-warnings nil
                           :selection-blocked-selection nil
                           :selection-blocked-notes nil
                           :conflicts nil
                           :target-locations nil))
                    'reference-notes
                    (list :candidates (list candidate)
                          :selected-candidate-id
                          (delib-flow--artifact-candidate-id candidate)
                          :selected-draft drafted-item))))
             (selected
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-selection run "1")
               'select-approved-filing-actions))
             (updated-run
              (delib-flow--run-stage-locally selected 'file-approved-outputs))
             (location (car (plist-get (plist-get updated-run :filing)
                                       :target-locations))))
        (should (string-match-p
                 "\\* Working draft\nAtlas draft body\\."
                 (delib-flow-test--file-buffer-string
                  (plist-get location :target))))))))

(ert-deftest delib-flow-file-approved-outputs-uses-drafted-selected-action-text ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project\n"
    (let* ((candidate (list :kind 'next-action
                            :text "Draft kickoff follow-up"
                            :warnings nil))
           (drafted-item (plist-put (copy-tree candidate)
                                    :text
                                    "Send kickoff follow-up with owners and due dates"))
           (run (delib-flow--set-artifact-family-state
                 (plist-put
                  (plist-put
                   (delib-flow--initialize-run
                    (list :title "Alpha Project source"
                          :content "* Alpha Project source\nBody line.\n"))
                   :working-context
                   (list :project-match
                         (list :match-status 'matched
                               :best-project (list :title "Alpha Project"))))
                  :filing
                  (list :draft-items nil
                        :approved-items (list candidate)
                        :rejected-items nil
                        :preview-text nil
                        :selection-blocked-item nil
                        :selection-blocking-warnings nil
                        :selection-blocked-selection nil
                        :selection-blocked-notes nil
                        :conflicts nil
                        :target-locations nil))
                 'actions
                 (list :candidates (list candidate)
                       :selected-candidate-id
                       (delib-flow--artifact-candidate-id candidate)
                       :selected-draft drafted-item)))
           (updated-run
            (delib-flow--run-stage-locally run 'file-approved-outputs))
           (location (car (plist-get (plist-get updated-run :filing)
                                     :target-locations))))
      (should (string-match-p
               "\\*\\*\\* TODO Send kickoff follow-up with owners and due dates"
               (delib-flow-test--file-buffer-string
                delib-flow-my-projects-file)))
      (should (string-match-p
               "Alpha Project"
               (plist-get location :target))))))

(ert-deftest delib-flow-file-approved-outputs-clears-filed-reference-note-from-selected-draft-state ()
  (let ((delib-flow-general-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\n"))
    (delib-flow-test--with-temp-zk-root ()
      (let* ((candidate-one (list :kind 'reference-note
                                  :text "Create general PKM note for Durable idea"
                                  :note-type 'general-pkm
                                  :warnings nil))
             (candidate-two (list :kind 'reference-note
                                  :text "Create general PKM note for Adjacent pattern"
                                  :note-type 'general-pkm
                                  :warnings nil))
             (drafted-one (plist-put (copy-tree candidate-one)
                                     :draft-body
                                     "* Working draft\nDurable idea draft.\n"))
             (run (delib-flow--seed-filing-selection-block
                   (delib-flow--set-artifact-family-state
                    (plist-put
                     (delib-flow--initialize-run
                      (list :title "Example"
                            :content "* Example\nBody line.\n"))
                     :filing
                     (list :draft-items (list candidate-one candidate-two)
                           :approved-items nil
                           :rejected-items nil
                           :preview-text nil
                           :selection-blocked-item nil
                           :selection-blocking-warnings nil
                           :selection-blocked-selection nil
                           :selection-blocked-notes nil
                           :conflicts nil
                           :target-locations nil))
                    'reference-notes
                    (list :candidates (list candidate-one candidate-two)
                          :selected-candidate-id
                          (delib-flow--artifact-candidate-id candidate-one)
                          :selected-draft drafted-one))))
             (selected
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-selection run "1")
               'select-approved-filing-actions))
             (updated-run
              (delib-flow--run-stage-locally selected 'file-approved-outputs))
             (state (plist-get (plist-get updated-run :artifacts) 'reference-notes))
             (remaining (plist-get state :candidates)))
        (should (equal 1 (length remaining)))
        (should (equal "Create general PKM note for Adjacent pattern"
                       (plist-get (car remaining) :text)))
        (should-not (plist-get state :selected-candidate-id))
        (should-not (plist-get state :selected-draft))))))

(ert-deftest delib-flow-staged-content-preview-uses-drafted-approved-reference-note-body ()
  (let ((delib-flow-general-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\n"))
    (delib-flow-test--with-temp-zk-root ()
      (let* ((candidate (list :kind 'reference-note
                              :text "Create general PKM note for Project Atlas Pattern"
                              :note-type 'general-pkm
                              :warnings nil))
             (drafted-item (plist-put (copy-tree candidate)
                                      :draft-body
                                      "* Working draft\nAtlas draft body.\n"))
             (run (delib-flow--set-artifact-family-state
                   (plist-put
                    (delib-flow--initialize-run
                     (list :title "Project Atlas source"
                           :content "* Project Atlas source\nBody line.\n"))
                    :filing
                    (list :draft-items nil
                          :approved-items (list candidate)
                          :rejected-items nil
                          :preview-text nil
                          :selection-blocked-item nil
                          :selection-blocking-warnings nil
                          :selection-blocked-selection nil
                          :selection-blocked-notes nil
                          :conflicts nil
                          :target-locations nil))
                   'reference-notes
                   (list :candidates (list candidate)
                         :selected-candidate-id
                         (delib-flow--artifact-candidate-id candidate)
                         :selected-draft drafted-item)))
             (preview (delib-flow--staged-content-preview-text run)))
        (should (string-match-p "Atlas draft body" preview))
        (should-not (string-match-p "Capture the core claim from the source in your own words" preview))))))

(ert-deftest delib-flow-file-approved-outputs-uses-org-roam-capture-template-target ()
  (delib-flow-test--with-temp-zk-root ()
    (let ((org-roam-directory delib-flow-zk-root)
          (delib-flow-general-note-org-roam-capture-key "w"))
      (let ((saved-templates (and (boundp 'org-roam-capture-templates)
                                  org-roam-capture-templates)))
        (unwind-protect
            (progn
              (setq org-roam-capture-templates
                    '(("w" "Wiki Note" plain
                       "%?"
                       :if-new (file+head
                                "wiki/%<%Y%m%d%H%M%S>.org"
                                "#+title: ${title}\n#+filetags: :wiki:draft:stub:\n")
                       :unnarrowed t)))
              (let* ((package
                      (list :working-context nil
                            :filing
                            (list :approved-items
                                  (list (list :kind 'reference-note
                                              :text "Create general PKM note for Project Atlas Pattern"
                                              :note-type 'general-pkm)))))
                     (result (delib-flow--file-approved-outputs-result package))
                     (location (car (plist-get result :target-locations)))
                     (content
                     (with-current-buffer (get-file-buffer (plist-get location :target))
                       (buffer-substring-no-properties (point-min) (point-max)))))
                (should (string-match-p "/wiki/[0-9]\\{14\\}\\.org\\'" (plist-get location :target)))
                (should (string-match-p ":ID:" content))
                (should (string-match-p "#\\+title: Project Atlas Pattern" content))
                (should (string-match-p "#\\+filetags: :wiki:draft:stub:" content))
                (should (string-match-p "\\* Working draft" content))
                (should (string-match-p "\\* Source highlights" content))
                (should (string-match-p "\\* Next pass" content))))
          (setq org-roam-capture-templates saved-templates))))))

(ert-deftest delib-flow-file-approved-outputs-allows-operator-target-path-for-function-org-roam-template ()
  (delib-flow-test--with-temp-zk-root ()
    (let ((org-roam-directory delib-flow-zk-root)
          (delib-flow-general-note-org-roam-capture-key "w"))
      (let ((saved-templates (and (boundp 'org-roam-capture-templates)
                                  org-roam-capture-templates)))
        (unwind-protect
            (progn
              (setq org-roam-capture-templates
                    '(("z" "Zettel" plain
                       "%?"
                       :if-new (file+head
                                my/org-roam-prompt-id
                                "#+title: ${title}\n#+filetags: :zettel:draft:\n")
                       :unnarrowed t)))
              (let* ((run (delib-flow--initialize-run
                           (list :title "Source"
                                 :content "* Source\nBody line\n")))
                     (run (plist-put
                           run :filing
                           (plist-put (plist-get run :filing)
                                      :approved-items
                                      (list (list :kind 'reference-note
                                                  :text "Create general PKM note for Project Atlas Pattern"
                                                  :note-type 'general-pkm)))))
                     (run (delib-flow--seed-reference-note-capture-review-block run))
                     (run (delib-flow--set-reference-note-capture-field run "Template key" "z"))
                     (run (delib-flow--set-reference-note-capture-field run "Target path" "1a"))
                     (package (delib-flow--stage-input-package run 'file-approved-outputs))
                     (result (delib-flow--file-approved-outputs-result package))
                     (location (car (plist-get result :target-locations)))
                     (content
                      (with-current-buffer (get-file-buffer (plist-get location :target))
                        (buffer-substring-no-properties (point-min) (point-max)))))
                (should (string-match-p "/1a\\.org\\'" (plist-get location :target)))
                (should (string-match-p ":ID:" content))
                (should (string-match-p "#\\+title: Project Atlas Pattern" content))))
          (setq org-roam-capture-templates saved-templates))))))

(ert-deftest delib-flow-reference-note-content-strips-summary-and-link-noise-from-seeded-body ()
  (let* ((package
          (list :source
                (list :title "Your Consumption Diet Is Your Moat"
                      :content
                      (concat
                       "* Your Consumption Diet Is Your Moat :email:\n"
                       ":PROPERTIES:\n"
                       ":FROM: Forte Labs Newsletter <hello@fortelabs.com>\n"
                       ":END:\n\n"
                       ":RAW_EMAIL:\n"
                       "From: Forte Labs Newsletter <hello@fortelabs.com>\n"
                       "Subject: Your Consumption Diet Is Your Moat\n\n"
                       "(\n"
                       "https://3c5984bd.click.convertkit-mail4.com/example\n"
                       ")\n"
                       "The newsletter highlights building personal AI advisors, with examples from a nutrition coach and adjacent advisor patterns.\n"
                       ":END:\n"))
                :working-context
                (list :inspect-output
                      '(:source-type email
                        :analysis (:summary "The newsletter highlights building personal AI advisors. Referenced materials: /tmp/example-mail.txt"))
                      :filtered-context
                      (list :retained-candidates
                            (list (list :title "Inner Foundation"
                                        :score 5
                                        :filter-reasons '("retained-by-score-threshold"))))
                      :contact-emails '("hello@fortelabs.com"))))
         (content
          (delib-flow--reference-note-content
           (list :kind 'reference-note
                 :text "Create general PKM note for Your Consumption Diet Is Your Moat"
                 :note-type 'general-pkm)
           package)))
    (should (string-match-p "- Durable claim: The newsletter highlights building personal AI advisors" content))
    (should (string-match-p "- Why it matters: The newsletter highlights building personal AI advisors" content))
    (should (string-match-p "- Reuse angle: Reuse this when related notes touch your consumption diet is your moat" content))
    (should-not (string-match-p "Referenced materials:" content))
    (should-not (string-match-p "3c5984bd\\.click\\.convertkit" content))
    (should-not (string-match-p "- (" content))))

(ert-deftest delib-flow-reference-note-source-highlights-stay-extractive-for-named-entities ()
  (let* ((package
          (list :source
                (list :title "Advisor update"
                      :content
                      (concat
                       "* Advisor update :email:\n"
                       ":RAW_EMAIL:\n"
                       "Christiana built a nutrition coach.\n"
                       "Others are working on fitness coaches, financial advisors, and strategy consultants for their businesses.\n"
                       "The idea: instead of a generic chatbot, you create a named advisor with a defined role, a clear scope, and the personal context to give advice that fits your life.\n"
                       ":END:\n"))))
         (item (list :kind 'reference-note
                     :text "Create general PKM note for Building personal AI advisors"
                     :note-type 'general-pkm))
         (highlights (delib-flow--reference-note-source-highlights item package)))
    (should (member "Christiana built a nutrition coach" highlights))
    (should-not (seq-some
                 (lambda (line)
                   (string-match-p "\\bChristian built\\b" line))
                 highlights))
    (should-not (seq-some
                 (lambda (line)
                   (string-match-p "generic chatbots" line))
                 highlights))))

(ert-deftest delib-flow-current-filing-choice-shows-reference-note-preview-before-filing ()
  (delib-flow-test--with-temp-zk-root ()
    (let ((org-roam-directory delib-flow-zk-root)
          (delib-flow-general-note-org-roam-capture-key "w"))
      (let ((saved-templates (and (boundp 'org-roam-capture-templates)
                                  org-roam-capture-templates)))
        (unwind-protect
            (progn
              (setq org-roam-capture-templates
                    '(("w" "Wiki Note" plain
                       "%?"
                       :if-new (file+head
                                "wiki/%<%Y%m%d%H%M%S>.org"
                                "#+title: ${title}\n#+filetags: :wiki:draft:stub:\n")
                       :unnarrowed t)))
              (let* ((run (delib-flow--initialize-run
                           (list :title "Source"
                                 :content "* Source\nBody line\n")))
                     (run (plist-put
                           run :filing
                           (plist-put (plist-get run :filing)
                                      :approved-items
                                      (list (list :kind 'reference-note
                                                  :text "Create general PKM note for Project Atlas Pattern"
                                                  :note-type 'general-pkm)))))
                     (buffer (delib-flow--render-control-buffer run)))
                (unwind-protect
                    (with-current-buffer buffer
                      (goto-char (point-min))
                      (should (search-forward "*** Focused filing workspace" nil t))
                      (should (search-forward "Create general PKM note for Project Atlas Pattern" nil t))
                      (should (search-forward "- Main move here: press `F` to open the focused filing workspace." nil t))
                      (should (search-forward "*** Filing state" nil t))
                      (should-not (search-forward "*** Working draft" nil t)))
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer)))))
          (setq org-roam-capture-templates saved-templates))))))

(ert-deftest delib-flow-file-approved-outputs-uses-configured-general-note-template ()
  (let ((delib-flow-general-note-template
         "#+title: ${title}\n#+filetags: :custom:general:\n\nSource: ${source-artifact}\nType: ${note-type}\n"))
    (delib-flow-test--with-temp-zk-root ()
      (let* ((package
              (list :working-context nil
                    :filing
                    (list :approved-items
                          (list (list :kind 'reference-note
                                      :text "Create general PKM note for Alpha Project kickoff"
                                      :note-type 'general-pkm)))))
             (result (delib-flow--file-approved-outputs-result package))
             (location (car (plist-get result :target-locations))))
        (should (string-match-p
                 "#\\+filetags: :custom:general:"
                 (delib-flow-test--file-buffer-string (plist-get location :target))))
        (should (string-match-p
                 "Source: Create general PKM note for Alpha Project kickoff"
                 (delib-flow-test--file-buffer-string (plist-get location :target))))
        (should (string-match-p
                 "Type: general-pkm"
                 (delib-flow-test--file-buffer-string (plist-get location :target))))))))

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
        (should (buffer-live-p (get-file-buffer (plist-get note-location :target))))
        (should (string-match-p "REFERENCE_FILES"
                                (plist-get metadata-location :target)))
        (let ((content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
          (should (string-match-p ":REFERENCE_FILES:" content))
          (should (string-match-p
                   "\\[\\[file:[^]]*alpha-constraints\\.org\\]\\[Alpha Constraints\\]\\]"
                   content)))))))

(ert-deftest delib-flow-file-approved-outputs-detects-project-child-conflict ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n** TODO Write follow-up note for Alpha Project kickoff\n"
    (let* ((package
            (list :working-context
                  (list :project-match
                        (list :best-project
                              (list :title "Alpha Project")))
                  :filing
                  (list :approved-items
                        (list (list :kind 'next-action
                                    :text "Write follow-up note for Alpha Project kickoff")))))
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
              (delib-flow-test--set-filing-selection
               (delib-flow--run-stage-locally drafted 'integrate-into-source)
               "1"))
             (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-draft-selected-reference-note)
              (delib-flow-action-select-approved-filing-actions)
              (delib-flow-action-file-approved-outputs)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "*** Focused filing workspace" nil t))
                (should (search-forward "note target already exists"
                                        nil t))
                (goto-char (point-min))
                (should (search-forward "- Retry File Approved Outputs [available]" nil t))
                (goto-char (point-min))
                (should (search-forward "- Resolve Filing Conflict [available]" nil t))
                (goto-char (point-min))
                (should (search-forward "*** Current blocked state" nil t))
                (goto-char (point-min))
                (should (search-forward "Create project support note from Alpha Project kickoff"
                                        nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))



(ert-deftest delib-flow-resolve-filing-conflict-can-reject-approved-artifact ()
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
             (drafted (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (selected
              (delib-flow-test--approve-selected-filing-item
               (delib-flow-test--set-filing-selection
                (delib-flow--run-stage-locally drafted 'integrate-into-source)
                "1")))
             (conflicted
              (delib-flow--run-stage-locally selected 'file-approved-outputs))
             (resolved
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-conflict-resolution
                conflicted "REJECT" "Reject duplicate note target.")
               'resolve-filing-conflict))
             (entry (car (last (plist-get (plist-get resolved :stage-history)
                                          :entries))))
             (raw (plist-get entry :raw-output))
             (filing (plist-get resolved :filing)))
        (should (equal "REJECT" (plist-get raw :resolution)))
        (should-not (plist-get filing :approved-items))
        (should-not (plist-get filing :conflicts))
        (should (= 1 (length (plist-get filing :rejected-items))))
        (should
         (string-match-p
          "Create project support note from Alpha Project kickoff"
          (plist-get
           (car (plist-get filing :rejected-items))
           :text)))))))


(ert-deftest delib-flow-resolve-filing-conflict-retry-rejects-retarget-fields ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n** TODO Write follow-up note for Alpha Project kickoff\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected
            (delib-flow-test--approve-selected-filing-item
             (delib-flow-test--set-filing-selection
              (delib-flow--run-stage-locally drafted 'integrate-into-source)
              "1")))
           (conflicted
            (delib-flow--run-stage-locally selected 'file-approved-outputs)))
      (let* ((failed
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-conflict-resolution
                conflicted "RETRY" "Retry without changing target."
                "Draft kickoff follow-up retargeted")
               'resolve-filing-conflict))
             (entry (delib-flow--latest-stage-entry failed)))
        (should (eq 'resolve-filing-conflict (plist-get entry :stage-id)))
        (should (eq 'failed (plist-get entry :status)))
        (should (string-match-p "RETRY does not accept a New title value"
                                (plist-get entry :normalized-output)))))))

(ert-deftest delib-flow-resolve-filing-conflict-command-rerenders-preview ()
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
             (drafted (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (selected
              (delib-flow-test--approve-selected-filing-item
               (delib-flow-test--set-filing-selection
                (delib-flow--run-stage-locally drafted 'integrate-into-source)
                "1")))
             (delib-flow--active-run
              (delib-flow-test--set-filing-conflict-resolution
               (delib-flow--run-stage-locally selected 'file-approved-outputs)
               "RENAME-NOTE" "Retarget support note."
               "Alpha Project kickoff retargeted"))
             (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-resolve-filing-conflict)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Resolve Filing Conflict" nil t))
                (should (search-forward "Resolution: RENAME-NOTE" nil t))
                (goto-char (point-min))
                (should (search-forward "*** Focused filing workspace" nil t))
                (should (search-forward "Create project support note from Alpha Project kickoff retargeted" nil t))
                (goto-char (point-min))
                (should (search-forward "*** Current blocked state" nil t))
                (should (search-forward "No blocked approval details are currently recorded." nil t))
                (goto-char (point-min))
                (should (search-forward "- File Approved Outputs [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-resolve-filing-conflict-can-reword-project-child-and-retry ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n** TODO Write follow-up note for Alpha Project kickoff\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected
            (delib-flow-test--approve-selected-filing-item
             (delib-flow-test--set-filing-selection
              (delib-flow--run-stage-locally drafted 'integrate-into-source)
              "1")))
           (conflicted
            (delib-flow--run-stage-locally selected 'file-approved-outputs))
           (retargeted
            (delib-flow--run-stage-locally
             (delib-flow-test--set-filing-conflict-resolution
              conflicted "REWORD-ITEM" "Reword duplicate child."
              nil "Send budget update to Alice")
             'resolve-filing-conflict))
           (approved (car (plist-get (plist-get retargeted :filing)
                                     :approved-items)))
           (filed
            (delib-flow--run-stage-locally retargeted 'file-approved-outputs)))
      (should approved)
      (should (string-match-p "Send budget update to Alice"
                              (plist-get approved :text)))
      (should-not (plist-get (plist-get retargeted :filing) :conflicts))
      (should (string-match-p
               "\\*\\* TODO Send budget update to Alice"
               (delib-flow-test--file-buffer-string delib-flow-my-projects-file))))))

(ert-deftest delib-flow-resolve-filing-conflict-infers-reword-from-new-text ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n** TODO Write follow-up note for Alpha Project kickoff\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected
            (delib-flow-test--approve-selected-filing-item
             (delib-flow-test--set-filing-selection
              (delib-flow--run-stage-locally drafted 'integrate-into-source)
              "1")))
           (conflicted
            (delib-flow--run-stage-locally selected 'file-approved-outputs))
           (retargeted
            (delib-flow--run-stage-locally
             (delib-flow-test--set-filing-conflict-resolution
              conflicted "RETRY" "Reword duplicate child."
              nil "Testing123")
             'resolve-filing-conflict))
           (entry (delib-flow--latest-stage-entry retargeted))
           (approved (car (plist-get (plist-get retargeted :filing)
                                     :approved-items)))
           (filed
            (delib-flow--run-stage-locally retargeted 'file-approved-outputs)))
      (should (equal "REWORD-ITEM"
                     (plist-get (plist-get entry :raw-output) :resolution)))
      (should approved)
      (should (string-match-p "Testing123" (plist-get approved :text)))
      (should-not (plist-get (plist-get retargeted :filing) :conflicts))
      (should (string-match-p
               "\\*\\* TODO Testing123"
               (delib-flow-test--file-buffer-string delib-flow-my-projects-file))))))

(ert-deftest delib-flow-resolve-filing-conflict-can-retitle-project-and-retry ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project"
                       :content "* Alpha Project\nDraft kickoff outline for Alpha Project\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected
            (delib-flow-test--approve-selected-filing-item
             (delib-flow-test--set-filing-selection
              (delib-flow--run-stage-locally proposed 'integrate-into-source)
              "1")))
           (conflicted
            (delib-flow--run-stage-locally selected 'file-approved-outputs))
           (retargeted
            (delib-flow--run-stage-locally
             (delib-flow-test--set-filing-conflict-resolution
              conflicted "RETITLE-PROJECT" "Retitle duplicate project."
              "Alpha Project follow-up")
             'resolve-filing-conflict))
           (approved (car (plist-get (plist-get retargeted :filing)
                                     :approved-items)))
           (filed
            (delib-flow--run-stage-locally retargeted 'file-approved-outputs)))
      (should approved)
      (should (string-match-p "Alpha Project follow-up"
                              (plist-get approved :title)))
      (should-not (plist-get (plist-get retargeted :filing) :conflicts))
      (should (string-match-p
               "^\\* Alpha Project follow-up$"
               (delib-flow-test--file-buffer-string delib-flow-my-projects-file))))))

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
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
             (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-draft-selected-action)
              (delib-flow-action-select-approved-filing-actions)
              (delib-flow-action-file-approved-outputs)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "** File Approved Outputs" nil t))
              (should (search-forward "Filed count:" nil t))
              (goto-char (point-min))
              (should (search-forward "*** What happens here" nil t))
              (should (search-forward "No filing queue is available yet." nil t))
              (goto-char (point-min))
              (should (search-forward "*** Staged content preview" nil t))
              (should (search-forward "No staged content preview is available yet." nil t))
              (goto-char (point-min))
              (should (search-forward "*** Opened staged targets" nil t))
              (should (search-forward "These target buffers were opened and staged, but nothing has been saved to disk yet." nil t))
              (should (search-forward "Write follow-up note for Alpha Project kickoff" nil t))
              (goto-char (point-min))
              (should-not (search-forward "- File Approved Outputs [available]" nil t))
              (goto-char (point-min))
              (should (search-forward "No targets yet" nil t)))
            (should (buffer-live-p (get-file-buffer delib-flow-my-projects-file))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-suggest-reference-notes-remains-usable-after-filing-one-artifact ()
  (delib-flow-test--with-temp-zk-root ()
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (drafted (delib-flow--run-stage-locally matched 'extract-actions))
             (integrated (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (selected (delib-flow--run-stage-locally
                        (delib-flow-test--set-filing-selection integrated "1")
                        'select-approved-filing-actions))
             (filed (delib-flow--run-stage-locally selected 'file-approved-outputs))
             (suggested (delib-flow--run-stage-locally filed 'suggest-reference-notes))
             (actions (mapcar (lambda (action)
                                (plist-get action :id))
                              (plist-get (delib-flow--run-actions suggested) :items)))
             (items (plist-get (plist-get suggested :filing) :draft-items)))
        (should (member 'extract-waiting-for actions))
        (should (member 'suggest-reference-notes actions))
        (should (seq-some (lambda (item)
                            (eq (plist-get item :kind) 'reference-note))
                          items))
        (should (seq-some (lambda (item)
                            (eq (delib-flow--draft-item-stage item)
                                'suggest-reference-notes))
                          items))))))

(ert-deftest delib-flow-set-filing-selection-seeds-reference-note-capture-preview ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (draft-items
          (list (list :kind 'reference-note
                      :text "Create project support note from Vendor support handoff"
                      :note-type 'project-support)))
         (prepared
          (delib-flow--seed-filing-selection-block
           (plist-put
            run :filing
            (plist-put (plist-get run :filing) :draft-items draft-items))))
         (selected (delib-flow--set-filing-selection-value prepared "1"))
         (block (delib-flow--editable-block selected 'reference-note-capture-review))
         (text (plist-get block :current-text)))
    (should (string-match-p "Note title: Vendor support handoff" text))
    (should-not (string-match-p
                 "No reference-note filing artifact is currently active."
                 text))))

(ert-deftest delib-flow-set-filing-selection-reseeds-reference-note-capture-preview-for-new-note ()
  (let* ((note-one (list :kind 'reference-note
                         :text "Create general PKM note for Building personal AI advisors"
                         :note-type 'general-pkm
                         :warnings nil))
         (note-two (list :kind 'reference-note
                         :text "Create general PKM note for Durable advisor pattern"
                         :note-type 'general-pkm
                         :warnings nil))
         (drafted-one (plist-put (copy-tree note-one)
                                 :draft-body
                                 "* Working draft\nOld draft.\n"))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list note-one note-two)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'reference-notes
                (list :candidates (list note-one note-two)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id note-one)
                      :selected-draft drafted-one))))
         (selected (delib-flow--set-filing-selection-value run "2"))
         (block (delib-flow--editable-block selected 'reference-note-capture-review))
         (text (plist-get block :current-text)))
    (should (string-match-p "Note title: Durable advisor pattern" text))
    (should-not (string-match-p "Note title: Building personal AI advisors" text))))

(ert-deftest delib-flow-reference-note-capture-preview-prefers-selected-draft-over-approved-note ()
  (let* ((approved-note (list :kind 'reference-note
                              :text "Create general PKM note for Older note"
                              :note-type 'general-pkm))
         (selected-note (list :kind 'reference-note
                              :text "Create general PKM note for Building personal AI advisors"
                              :note-type 'general-pkm))
         (drafted-selected (plist-put (copy-tree selected-note)
                                      :draft-body
                                      "* Working draft\nFocused note draft.\n"))
         (run (delib-flow--seed-reference-note-capture-review-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list selected-note)
                       :approved-items (list approved-note)
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'reference-notes
                (list :candidates (list selected-note)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id selected-note)
                      :selected-draft drafted-selected))))
         (block (delib-flow--editable-block run 'reference-note-capture-review))
         (text (plist-get block :current-text)))
    (should (string-match-p "Note title: Building personal AI advisors" text))
    (should-not (string-match-p "Note title: Older note" text))))


(ert-deftest delib-flow-drafted-reference-note-item-adds-drift-warning-for-mismatched-body ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Laying the foundation"
                          :note-type 'general-pkm
                          :candidate-identity "Laying the foundation"
                          :focus-score 12))
         (package (delib-flow--initialize-run
                   (list :title "Example"
                         :content "* Example\nBuilding personal AI advisors.\n")))
         (draft (delib-flow--drafted-reference-note-item
                 candidate package
                 "* Working draft\nBuilding personal AI advisors changes the workflow.\n"
                 "Regenerated only the selected note.")))
    (should (member 'reference-note-draft-drift
                    (mapcar (lambda (warning)
                              (plist-get warning :code))
                            (delib-flow--draft-item-warnings draft))))))

(ert-deftest delib-flow-drafted-reference-note-item-skips-drift-warning-for-matching-body ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Building personal AI advisors"
                          :note-type 'general-pkm
                          :candidate-identity "Building personal AI advisors"
                          :focus-score 12))
         (package (delib-flow--initialize-run
                   (list :title "Example"
                         :content "* Example\nBuilding personal AI advisors.\n")))
         (draft (delib-flow--drafted-reference-note-item
                 candidate package
                 "* Working draft\nBuilding personal AI advisors can become a reusable agent pattern.\n"
                 "Regenerated only the selected note.")))
    (should-not (member 'reference-note-draft-drift
                        (mapcar (lambda (warning)
                                  (plist-get warning :code))
                                (delib-flow--draft-item-warnings draft))))))


(ert-deftest delib-flow-choose-and-clear-selected-reference-note-support-uses-manual-attachment ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body "* Working draft\nAtlas draft body.\n"))
         (support (list :title "Atlas brief"
                        :support-score 8
                        :support-reasons '("title-overlap")))
         (delib-flow--active-run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run (list :title "Source"
                                             :content "* Source\nBody line\n"))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                 :selected-draft drafted-item
                 :available-support-candidates (list support)
                 :available-support-context "- Atlas brief - Constraint summary"
                 :selected-support-candidates nil
                 :selected-support-context nil))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (delib-flow--reference-note-support-line support))))
      (delib-flow-action-choose-support-for-selected-reference-note))
    (let ((state (plist-get (plist-get delib-flow--active-run :artifacts)
                            'reference-notes)))
      (should (equal "Atlas brief"
                     (plist-get (car (plist-get state :selected-support-candidates))
                                :title)))
      (should (string-match-p "Atlas brief"
                              (plist-get state :selected-support-context))))
    (delib-flow-action-clear-selected-reference-note-support)
    (let ((state (plist-get (plist-get delib-flow--active-run :artifacts)
                            'reference-notes)))
      (should-not (plist-get state :selected-support-candidates))
      (should-not (plist-get state :selected-support-context)))))

(ert-deftest delib-flow-refresh-selected-reference-note-draft-body-updates-only-working-section ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Friction logs for AI systems"
                          :note-type 'general-pkm))
         (drafted-item
          (plist-put
           (copy-tree candidate)
           :draft-body
           (string-join
            '("* Working draft"
              "Old summary paragraph."
              "- Durable claim: Old claim."
              "- Why it matters: Old reason."
              "- Reuse angle: Keep this reuse angle."
              ""
              "* Source highlights"
              "- Keep these source highlights."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n")))
         (run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run
            (list :title "Four leverage patterns for working with AI tools"
                  :content
                  (string-join
                   '("* Four leverage patterns for working with AI tools"
                     "Capture friction logs, not just answers."
                     "Friction logs turn one-off annoyances into reusable system improvements.")
                   "\n")))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                 :selected-draft drafted-item)))
         (updated (delib-flow--reference-note-refresh-draft-body run))
         (body (plist-get (delib-flow--artifact-family-selected-draft
                           updated 'reference-notes)
                          :draft-body)))
    (should (string-match-p "This note captures Friction logs for AI systems" body))
    (should (string-match-p "- Durable claim:" body))
    (should (string-match-p "- Why it matters:" body))
    (should (string-match-p "- Reuse angle: Keep this reuse angle\\." body))
    (should (string-match-p "\\* Source highlights\n- Keep these source highlights\\." body))
    (should (string-match-p "\\* Related material to connect\n- Keep this related material\\." body))))

(defun delib-flow-test--reference-note-part-editor-run ()
  "Return a filing-ready run for focused reference-note part editor tests."
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (drafted-item
          (plist-put
           (copy-tree candidate)
           :draft-body
           (string-join
            '("* Working draft"
              "Atlas draft body."
              "- Durable claim: Keep this claim."
              "- Why it matters: Keep this reason."
              "- Reuse angle: Keep this reuse angle."
              ""
              "* Source highlights"
              "- Keep these source highlights."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n"))))
    (delib-flow--seed-actions
     (delib-flow--seed-filing-selection-block
      (delib-flow--set-artifact-family-state
       (plist-put
        (delib-flow--initialize-run
         (list :title "Source"
               :content "* Source\nBody line\n"))
        :filing
        (list :draft-items (list candidate)
              :approved-items nil
              :rejected-items nil
              :preview-text nil
              :selection-blocked-item nil
              :selection-blocking-warnings nil
              :selection-blocked-selection nil
              :selection-blocked-notes nil
              :conflicts nil
              :target-locations nil))
       'reference-notes
       (list :candidates (list candidate)
             :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
             :selected-draft drafted-item))))))

(defun delib-flow-test--reference-note-candidate-only-run ()
  "Return a filing-ready run with a selected reference-note candidate only."
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run
            (list :title "Atlas source"
                  :content
                  (string-join
                   '("* Atlas source"
                     "Project Atlas Pattern turns setup notes into reusable execution scaffolds."
                     "Use this when a recurring project setup pattern keeps reappearing.")
                   "\n")))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)))))
    (delib-flow--seed-actions
     (delib-flow--seed-filing-selection-block run))))

(ert-deftest delib-flow-reference-note-selected-draft-normalizes-plain-draft-body ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (drafted-item (plist-put
                        (copy-tree candidate)
                        :draft-body
                        "Project Atlas Pattern becomes more useful when setup notes turn into reusable execution scaffolds."))
         (run (delib-flow--set-artifact-family-state
               (delib-flow--initialize-run
                (list :title "Atlas source"
                      :content
                      (string-join
                       '("* Atlas source"
                         "Project Atlas Pattern turns setup notes into reusable execution scaffolds."
                         "Use this when a recurring project setup pattern keeps reappearing.")
                       "\n")))
               'reference-notes
               (list :candidates (list candidate)
                     :selected-candidate-id
                     (delib-flow--artifact-candidate-id candidate)
                     :selected-draft drafted-item)))
         (draft (delib-flow--reference-note-selected-draft run))
         (body (plist-get draft :draft-body)))
    (should (string-match-p "^\\* Working draft$" body))
    (should (string-match-p "Project Atlas Pattern becomes more useful" body))
    (should (string-match-p "^\\* Source highlights$" body))
    (should (string-match-p "^\\* Related material to connect$" body))))

(ert-deftest delib-flow-reference-note-normalization-helpers-cover-partial-structure-branches ()
  (let* ((draft-view
          (list :draft-body
                (string-join
                 '("* Working draft"
                   "Atlas draft body."
                   "- Reuse angle: Keep this reuse angle."
                   ""
                   "* Source highlights"
                   "- Existing highlight."
                   ""
                   "* Related material to connect"
                   "- Existing related item.")
                 "\n")))
         (candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (package
          (delib-flow--initialize-run
           (list :title "Atlas source"
                 :content
                 (string-join
                  '("* Atlas source"
                    "Project Atlas Pattern turns setup notes into reusable execution scaffolds."
                    "Use this when a recurring project setup pattern keeps reappearing.")
                  "\n"))))
         (base-body
          (delib-flow--reference-note-draft-body-with-seed
           "Atlas draft body."
           candidate package))
         (restored
          (delib-flow--reference-note-restore-partial-sections
           base-body draft-view))
         (partial-enriched
          (delib-flow--reference-note-enrich-partial-draft-body
           (plist-get draft-view :draft-body)
           candidate package)))
    (should (equal "Atlas draft body."
                   (delib-flow--reference-note-partial-working-body
                    draft-view
                    (plist-get draft-view :draft-body))))
    (should (equal "Atlas draft body."
                   (delib-flow--reference-note-partial-working-body
                    draft-view
                    "Atlas draft body.")))
    (should (equal "- Reuse angle: Keep this reuse angle."
                   (delib-flow--reference-note-partial-reuse-angle
                    draft-view)))
    (should-not
     (delib-flow--reference-note-partial-reuse-angle
      (list :draft-body "* Working draft\nAtlas draft body.\n")))
    (should (string-match-p "\\* Source highlights\n- Existing highlight\\." restored))
    (should (string-match-p "\\* Related material to connect\n- Existing related item\\." restored))
    (should (string-match-p "- Reuse angle: Keep this reuse angle\\." restored))
    (should (string-match-p "\\* Source highlights\n- Existing highlight\\." partial-enriched))
    (should (string-match-p "\\* Related material to connect\n- Existing related item\\." partial-enriched))))


(ert-deftest delib-flow-edit-selected-reference-note-draft-body-opens-note-part-editor ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-action-edit-selected-reference-note-draft-body)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (should (derived-mode-p 'delib-flow-note-part-editor-mode))
            (should (equal "Draft body" delib-flow--note-part-editor-title))
            (should (string-match-p "Atlas draft body\\." (buffer-string)))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name))))))

(ert-deftest delib-flow-edit-operator-intent-opens-dedicated-field-editor ()
  (let ((delib-flow--active-run
         (delib-flow-test--set-operator-intent
          (delib-flow--initialize-run
           (list :title "Broken steno exercise"
                 :content "* Broken steno exercise\nhttps://example.com/drill?id=one\n"))
          "this is about fixing a broken website")))
    (unwind-protect
        (progn
          (delib-flow-action-edit-operator-intent)
          (with-current-buffer (get-buffer delib-flow-field-editor-buffer-name)
            (should (derived-mode-p 'delib-flow-field-editor-mode))
            (should (equal "Operator intent" delib-flow--field-editor-title))
            (should (string-match-p "fixing a broken website" (buffer-string)))))
      (when (buffer-live-p (get-buffer delib-flow-field-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-field-editor-buffer-name))))))

(ert-deftest delib-flow-field-editor-apply-updates-run-state ()
  (delib-flow-test--with-temp-audit-file
    (let ((delib-flow--active-run
           (delib-flow--initialize-run
            (list :title "Broken steno exercise"
                  :content "* Broken steno exercise\nhttps://example.com/drill?id=one\n"))))
      (unwind-protect
          (progn
            (delib-flow-action-edit-operator-intent)
            (with-current-buffer (get-buffer delib-flow-field-editor-buffer-name)
              (erase-buffer)
              (insert "this is about fixing a broken website")
              (execute-kbd-macro "plain typing should not trigger cockpit keys")
              (delib-flow-field-editor-apply))
            (should-not (buffer-live-p (get-buffer delib-flow-field-editor-buffer-name)))
            (should (string-match-p
                     "plain typing should not trigger cockpit keys"
                     (delib-flow--operator-intent-text delib-flow--active-run)))
            (should (string-match-p
                     "Operator intent saved\\. Run Inspect Source to apply it to future stage results\\."
                     (plist-get (delib-flow--run-session delib-flow--active-run)
                                :current-decision)))
            (with-temp-buffer
              (insert-file-contents delib-flow-audit-log-file)
              (should (string-match-p
                       "plain typing should not trigger cockpit keys"
                       (buffer-string)))
              (should (string-match-p
                       "Current decision: Operator intent saved\\."
                       (buffer-string)))))
        (when (buffer-live-p (get-buffer delib-flow-field-editor-buffer-name))
          (kill-buffer (get-buffer delib-flow-field-editor-buffer-name)))))))

(ert-deftest delib-flow-edit-selected-reference-note-draft-body-opens-seeded-editor-before-full-draft ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-candidate-only-run)))
    (unwind-protect
        (progn
          (delib-flow-action-edit-selected-reference-note-draft-body)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (should (derived-mode-p 'delib-flow-note-part-editor-mode))
            (should (equal "Draft body" delib-flow--note-part-editor-title))
            (should (string-match-p "Project Atlas Pattern" (buffer-string)))
            (should (string-match-p "Durable claim" (buffer-string)))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name))))))

(ert-deftest delib-flow-note-part-editor-apply-updates-draft-and-returns-to-workspace ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-action-edit-selected-reference-note-draft-body)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (erase-buffer)
            (insert "Updated body.\n- Durable claim: New claim.\n- Why it matters: New reason.\n")
            (delib-flow-note-part-editor-apply))
          (should-not (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name)))
          (should (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name)))
          (let ((body (plist-get (delib-flow--artifact-family-selected-draft
                                  delib-flow--active-run 'reference-notes)
                                 :draft-body)))
            (should (string-match-p "Updated body\\." body))
            (should (string-match-p "- Durable claim: New claim\\." body))
            (should (string-match-p "- Why it matters: New reason\\." body))
            (should (string-match-p "- Reuse angle: Keep this reuse angle\\." body))
            (should (string-match-p "\\* Source highlights\n- Keep these source highlights\\." body))
            (should (string-match-p "\\* Related material to connect\n- Keep this related material\\." body))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-refresh-selected-reference-note-actions-route-through-part-stages ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run))
        called)
    (cl-letf (((symbol-function 'delib-flow--execute-local-stage)
               (lambda (run stage-id)
                 (push stage-id called)
                 run))
              ((symbol-function 'delib-flow--rerender-current-result)
               #'ignore))
      (delib-flow-action-refresh-selected-reference-note-draft-body)
      (delib-flow-action-refresh-selected-reference-note-source-highlights)
      (delib-flow-action-refresh-selected-reference-note-related-material)
      (delib-flow-action-refresh-selected-reference-note-reuse-angle))
    (should (equal
             '(draft-selected-reference-note-reuse-angle
               draft-selected-reference-note-related-material
               draft-selected-reference-note-source-highlights
               draft-selected-reference-note-body)
             called))))

(ert-deftest delib-flow-note-part-editor-apply-seeds-selected-draft-from-candidate ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-candidate-only-run)))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-action-edit-selected-reference-note-draft-body)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (erase-buffer)
            (insert "Atlas setup notes should become reusable execution scaffolds.\n- Durable claim: Repeated setup can be templated.\n- Why it matters: Future launches should start from a stronger baseline.\n")
            (delib-flow-note-part-editor-apply))
          (let ((draft (delib-flow--artifact-family-selected-draft
                        delib-flow--active-run 'reference-notes)))
            (should (eq 'reference-note (plist-get draft :kind)))
            (should (string-match-p "Atlas setup notes should become reusable execution scaffolds\\." (plist-get draft :draft-body)))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-edit-selected-reference-note-source-highlights-opens-note-part-editor ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-action-edit-selected-reference-note-source-highlights)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (should (derived-mode-p 'delib-flow-note-part-editor-mode))
            (should (equal "Source highlights" delib-flow--note-part-editor-title))
            (should (equal "- Keep these source highlights."
                           (string-trim (buffer-string))))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name))))))

(ert-deftest delib-flow-edit-selected-reference-note-related-material-opens-note-part-editor ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-action-edit-selected-reference-note-related-material)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (should (derived-mode-p 'delib-flow-note-part-editor-mode))
            (should (equal "Related material" delib-flow--note-part-editor-title))
            (should (equal "- Keep this related material."
                           (string-trim (buffer-string))))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name))))))

(ert-deftest delib-flow-edit-selected-reference-note-reuse-angle-opens-note-part-editor ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-action-edit-selected-reference-note-reuse-angle)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (should (derived-mode-p 'delib-flow-note-part-editor-mode))
            (should (equal "Reuse angle" delib-flow--note-part-editor-title))
            (should (equal "Keep this reuse angle."
                           (string-trim (buffer-string))))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name))))))

(ert-deftest delib-flow-note-part-editor-apply-updates-source-highlights-only ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-action-edit-selected-reference-note-source-highlights)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (erase-buffer)
            (insert "- New highlight one.\n- New highlight two.\n")
            (delib-flow-note-part-editor-apply))
          (let ((body (plist-get (delib-flow--artifact-family-selected-draft
                                  delib-flow--active-run 'reference-notes)
                                 :draft-body)))
            (should (string-match-p "\\* Working draft\nAtlas draft body\\." body))
            (should (string-match-p "\\* Source highlights\n- New highlight one\\.\n- New highlight two\\." body))
            (should (string-match-p "\\* Related material to connect\n- Keep this related material\\." body))
            (should (string-match-p "- Reuse angle: Keep this reuse angle\\." body))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-note-part-editor-apply-updates-related-material-and-reuse-angle ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-action-edit-selected-reference-note-related-material)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (erase-buffer)
            (insert "- Link to Atlas execution note.\n")
            (delib-flow-note-part-editor-apply))
          (delib-flow-action-edit-selected-reference-note-reuse-angle)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (erase-buffer)
            (insert "Use this for future project setup notes.")
            (delib-flow-note-part-editor-apply))
          (let ((body (plist-get (delib-flow--artifact-family-selected-draft
                                  delib-flow--active-run 'reference-notes)
                                 :draft-body)))
            (should (string-match-p "\\* Related material to connect\n- Link to Atlas execution note\\." body))
            (should (string-match-p "- Reuse angle: Use this for future project setup notes\\." body))
            (should (string-match-p "\\* Source highlights\n- Keep these source highlights\\." body))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-note-part-editor-cancel-confirmation-paths-work ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-action-edit-selected-reference-note-draft-body)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (insert "\nUnsaved change.")
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) nil)))
              (should-error (delib-flow-note-part-editor-cancel)
                            :type 'user-error))
            (should (buffer-live-p (current-buffer)))
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _) t)))
              (delib-flow-note-part-editor-cancel)))
          (should-not (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name)))
          (should (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-note-part-editor-header-line-renders-title-and-help ()
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (should (string-match-p "C-c C-c apply"
                            (delib-flow--note-part-editor-header-line)))
    (setq-local delib-flow--note-part-editor-title "Draft body")
    (should (string-match-p "\\[Draft body\\]"
                            (delib-flow--note-part-editor-header-line)))))

(ert-deftest delib-flow-note-part-editor-header-line-omits-empty-title-brackets ()
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (setq-local delib-flow--note-part-editor-title nil)
    (should-not (string-match-p "\\[\\]" (delib-flow--note-part-editor-header-line)))
    (should (string-match-p "C-c C-k cancel"
                            (delib-flow--note-part-editor-header-line)))))

(ert-deftest delib-flow-note-part-editor-header-line-omits-blank-title ()
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (setq-local delib-flow--note-part-editor-title "")
    (should-not (string-match-p "\\[\\]" (delib-flow--note-part-editor-header-line)))
    (should (string-match-p "Delib-Flow note editor"
                            (delib-flow--note-part-editor-header-line)))))

(ert-deftest delib-flow-note-part-editor-header-line-omits-whitespace-only-title ()
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (setq-local delib-flow--note-part-editor-title "   ")
    (should-not (string-match-p "\\[" (delib-flow--note-part-editor-header-line)))
    (should (string-match-p "C-c C-c apply"
                            (delib-flow--note-part-editor-header-line)))))

(ert-deftest delib-flow-run-ui-preview-kind-helper-roundtrip ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Source"
                     :content "* Source\nBody line\n")))
         (updated (delib-flow--set-consequence-preview-kind run 'draft))
         (with-history
          (plist-put updated :stage-history
                     (list :latest-stage 'details
                           :entries (list (list :stage-id 'details))))))
    (should (eq 'draft (delib-flow--consequence-preview-kind run)))
    (should (eq 'draft (delib-flow--consequence-preview-kind updated)))
    (should (eq 'details (delib-flow--latest-stage-id with-history)))
    (should-not (delib-flow--run-cloud-stage-p 'inspect-source))
    (should (delib-flow--run-cloud-stage-p 'run-cloud-stage))))

(ert-deftest delib-flow-note-part-editor-command-guards-and-helpers-work ()
  (with-temp-buffer
    (should-error (delib-flow-note-part-editor-apply) :type 'user-error)
    (should-error (delib-flow-note-part-editor-cancel) :type 'user-error))
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (setq-local delib-flow--note-part-editor-title nil)
    (setq delib-flow--active-run nil)
    (should-error (delib-flow-note-part-editor-apply) :type 'user-error)
    (setq delib-flow--active-run (delib-flow-test--reference-note-part-editor-run))
    (should-error (delib-flow-note-part-editor-apply) :type 'user-error))
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (should (delib-flow--note-part-editor-discard-confirmed-p))
    (insert "pending")
    (set-buffer-modified-p t)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) nil)))
      (should-not (delib-flow--note-part-editor-discard-confirmed-p)))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) t)))
      (should (delib-flow--note-part-editor-discard-confirmed-p)))))

(ert-deftest delib-flow-reference-note-editor-helper-normalizes-part-input ()
  (should (equal '("Line one" "" "Line two")
                 (delib-flow--reference-note-editor-lines
                  "Line one\n\nLine two\n")))
  (should (equal "- Reuse angle: Keep this angle."
                 (delib-flow--reference-note-edited-reuse-angle-line
                  "Keep this angle.")))
  (should (equal "- Reuse angle: Already prefixed."
                 (delib-flow--reference-note-edited-reuse-angle-line
                  "- Reuse angle: Already prefixed."))))

(ert-deftest delib-flow-reference-note-edit-helpers-update-selected-draft-directly ()
  (let* ((run (delib-flow-test--reference-note-part-editor-run))
         (stored (delib-flow--reference-note-store-edited-body
                  run
                  "* Working draft\nStored body.\n- Reuse angle: Stored angle.\n"
                  "Stored manually."))
         (section-updated
          (delib-flow--reference-note-edit-section
           (delib-flow-test--reference-note-part-editor-run)
           "Source highlights"
           "- Direct highlight.\n"
           "Edited directly."))
         (line-updated
          (delib-flow--reference-note-edit-working-line
           (delib-flow-test--reference-note-part-editor-run)
           "- Reuse angle:"
           "- Reuse angle: Direct line."
           "Edited line directly."))
         (body-updated
          (delib-flow--reference-note-edit-draft-body
           (delib-flow-test--reference-note-part-editor-run)
           "Direct body.\n- Durable claim: Direct claim.\n- Why it matters: Direct reason.\n"))
         (related-updated
          (delib-flow--reference-note-edit-related-material
           (delib-flow-test--reference-note-part-editor-run)
           "- Direct related material.\n"))
         (reuse-updated
          (delib-flow--reference-note-edit-reuse-angle
           (delib-flow-test--reference-note-part-editor-run)
           "Direct reuse.")))
    (should (string-match-p "Stored body\\."
                            (plist-get (delib-flow--artifact-family-selected-draft
                                        stored 'reference-notes)
                                       :draft-body)))
    (should (string-match-p "\\* Source highlights\n- Direct highlight\\."
                            (plist-get (delib-flow--artifact-family-selected-draft
                                        section-updated 'reference-notes)
                                       :draft-body)))
    (should (string-match-p "- Reuse angle: Direct line\\."
                            (plist-get (delib-flow--artifact-family-selected-draft
                                        line-updated 'reference-notes)
                                       :draft-body)))
    (should (string-match-p "Direct body\\."
                            (plist-get (delib-flow--artifact-family-selected-draft
                                        body-updated 'reference-notes)
                                       :draft-body)))
    (should (string-match-p "\\* Related material to connect\n- Direct related material\\."
                            (plist-get (delib-flow--artifact-family-selected-draft
                                        related-updated 'reference-notes)
                                       :draft-body)))
    (should (string-match-p "- Reuse angle: Direct reuse\\."
                            (plist-get (delib-flow--artifact-family-selected-draft
                                        reuse-updated 'reference-notes)
                                       :draft-body)))))

(ert-deftest delib-flow-note-part-editor-apply-and-close-cover-remaining-branches ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (with-temp-buffer
      (setq delib-flow--active-run delib-flow--active-run)
      (should-error (delib-flow-note-part-editor-apply) :type 'user-error))
    (let ((buffer (generate-new-buffer " *delib-flow-close*")))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (delib-flow--close-note-part-editor-buffer buffer))
            (should-not (buffer-live-p buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))))
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-action-edit-selected-reference-note-draft-body)
          (with-current-buffer (get-buffer delib-flow-note-part-editor-buffer-name)
            (cl-letf (((symbol-function 'yes-or-no-p)
                       (lambda (&rest _)
                         (error "should not prompt"))))
              (delib-flow-note-part-editor-cancel)))
          (should-not (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))))
      (when (buffer-live-p (get-buffer delib-flow-note-part-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-note-part-editor-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-edit-selected-reference-note-title-and-target-path-update-capture-fields ()
  (let ((delib-flow--active-run
         (delib-flow-test--reference-note-part-editor-run)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt initial &rest _)
                 (cond
                  ((string-match-p "Saved note title:" prompt)
                   (should (equal "Project Atlas Pattern" initial))
                   "Atlas Operator Note")
                  ((string-match-p "Saved note target path" prompt)
                   (should (equal "" initial))
                   "atlas/operator-note")
                  (t
                   (error "Unexpected prompt: %s" prompt))))))
      (delib-flow-action-edit-selected-reference-note-title)
      (delib-flow-action-edit-selected-reference-note-target-path))
    (let ((package (delib-flow--stage-input-package
                    delib-flow--active-run
                    'file-approved-outputs)))
      (should (equal "Atlas Operator Note"
                     (delib-flow--reference-note-capture-title-value package)))
      (should (equal "atlas/operator-note"
                     (delib-flow--reference-note-capture-target-path-value package))))))

(ert-deftest delib-flow-refresh-selected-reference-note-actions-update-only-target-parts ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Friction logs for AI systems"
                          :note-type 'general-pkm
                          :focus "Friction logs for AI systems"))
         (support (list :title "Atlas brief"
                        :score 7
                        :reasons '("shared terms")
                        :context "- Atlas brief supports this idea."))
         (drafted-item
          (plist-put
           (copy-tree candidate)
           :draft-body
           (string-join
            '("* Working draft"
              "Old summary paragraph."
              "- Durable claim: Old claim."
              "- Why it matters: Old reason."
              "- Reuse angle: Old reuse angle."
              ""
              "* Source highlights"
              "- Old highlight."
              ""
              "* Related material to connect"
              "- Old related material.")
            "\n")))
         (delib-flow--active-run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run
            (list :title "Four leverage patterns for working with AI tools"
                  :content
                  (string-join
                   '("* Four leverage patterns for working with AI tools"
                     "Capture friction logs, not just answers."
                     "Friction logs turn one-off annoyances into reusable system improvements.")
                   "\n")))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                 :selected-draft drafted-item
                 :selected-support-candidates (list support)))))
    (delib-flow-action-refresh-selected-reference-note-source-highlights)
    (delib-flow-action-refresh-selected-reference-note-related-material)
    (delib-flow-action-refresh-selected-reference-note-reuse-angle)
    (let ((body (plist-get (delib-flow--artifact-family-selected-draft
                            delib-flow--active-run 'reference-notes)
                           :draft-body)))
      (should (string-match-p "\\* Source highlights\n- .*Capture friction logs, not just answers.*" body))
      (should (string-match-p "\\* Related material to connect\n- Atlas brief" body))
      (should (string-match-p "- Reuse angle: Reuse this when related notes touch friction logs for ai systems or adjacent patterns\\." body))
      (should (string-match-p "Old summary paragraph\\." body)))))

(ert-deftest delib-flow-prompt-and-example-file-helpers-read-configured-content ()
  (let* ((prompt-file (make-temp-file "delib-flow-prompt" nil ".org"))
         (example-file (make-temp-file "delib-flow-example" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file prompt-file
            (insert "* Inspect Source\n:PROPERTIES:\n:PROMPT_ID: inspect-source\n:END:\nPrompt body line.\n"))
          (with-temp-file example-file
            (insert " Example structure block \n"))
          (let ((delib-flow-prompt-library-file prompt-file)
                (delib-flow-example-structures-file example-file))
            (should (delib-flow--file-readable-p prompt-file))
            (should-not (delib-flow--file-readable-p ""))
            (should-not (delib-flow--file-readable-p nil))
            (should-not (delib-flow--file-readable-p
                         (expand-file-name "definitely-missing-delib-flow.org" temporary-file-directory)))
            (should (delib-flow--prompt-library-configured-p))
            (should (delib-flow--example-structures-configured-p))
            (should (equal "Prompt body line."
                           (delib-flow--prompt-library-entry-text 'inspect-source)))
            (should-not (delib-flow--prompt-library-entry-text 'missing-stage))
            (should (equal "Example structure block"
                           (delib-flow--example-structures-text)))))
      (delete-file prompt-file)
      (delete-file example-file))))

(ert-deftest delib-flow-start-run-from-source-and-missing-config-helpers-work ()
  (let ((delib-flow--active-run nil)
        (delib-flow-prompt-library-file nil)
        (delib-flow-example-structures-file nil))
    (unwind-protect
        (progn
          (delib-flow--start-run-from-source
           (list :title "Inline source"
                 :content "* Inline source\nBody.\n"))
          (should (equal "Inline source"
                         (plist-get (plist-get delib-flow--active-run :source) :title)))
          (should-not (delib-flow--prompt-library-entry-text 'inspect-source))
          (should-not (delib-flow--example-structures-text))
          (let ((delib-flow-prompt-library-file "/tmp/delib-flow-missing-prompt.org")
                (delib-flow-example-structures-file "/tmp/delib-flow-missing-examples.txt"))
            (should-not (delib-flow--prompt-library-configured-p))
            (should-not (delib-flow--example-structures-configured-p))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-start-run-from-source-renders-now-section ()
  (let ((delib-flow--active-run nil))
    (unwind-protect
        (progn
          (delib-flow--start-run-from-source
           (list :title "Inline source"
                 :content "* Inline source\nBody.\n"))
          (with-current-buffer delib-flow-control-buffer-name
            (goto-char (point-min))
            (should (search-forward "* Now" nil t))
            (should (search-forward "Inline source" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-refresh-selected-reference-note-actions-error-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-action-refresh-selected-reference-note-draft-body)
                  :type 'user-error)
    (should-error (delib-flow-action-refresh-selected-reference-note-source-highlights)
                  :type 'user-error)
    (should-error (delib-flow-action-refresh-selected-reference-note-related-material)
                  :type 'user-error)
    (should-error (delib-flow-action-refresh-selected-reference-note-reuse-angle)
                  :type 'user-error)))

(ert-deftest delib-flow-selected-artifact-stage-actions-route-through-local-stages ()
  (cl-labels
      ((selected-run (family candidate)
         (delib-flow--set-artifact-family-state
          (delib-flow--initialize-run
           (list :title "Example"
                 :content "* Example\nBody.\n"))
          family
          (list :candidates (list candidate)
                :selected-candidate-id
                (delib-flow--artifact-candidate-id candidate)))))
    (let* ((action-run
            (selected-run 'actions
                          (list :kind 'next-action
                                :text "Draft kickoff follow-up")))
           (waiting-run
            (selected-run 'waiting-fors
                          (list :kind 'waiting-for
                                :text "Waiting for owner update")))
           (note-run
            (selected-run 'reference-notes
                          (list :kind 'reference-note
                                :text "Create general PKM note for Project Atlas Pattern"
                                :note-type 'general-pkm)))
           (project-run
            (selected-run 'project-proposals
                          (list :kind 'project
                                :title "Project Atlas"
                                :state 'active)))
           (specs
            (list (list action-run #'delib-flow-action-draft-selected-action
                        'draft-selected-action)
                  (list action-run #'delib-flow-action-find-support-for-selected-action
                        'find-support-for-selected-action)
                  (list waiting-run #'delib-flow-action-draft-selected-waiting-for
                        'draft-selected-waiting-for)
                  (list waiting-run #'delib-flow-action-find-support-for-selected-waiting-for
                        'find-support-for-selected-waiting-for)
                  (list note-run #'delib-flow-action-draft-selected-reference-note
                        'draft-selected-reference-note)
                  (list note-run #'delib-flow-action-find-support-for-selected-reference-note
                        'find-support-for-selected-reference-note)
                  (list project-run #'delib-flow-action-draft-selected-project
                        'draft-selected-project)
                  (list project-run #'delib-flow-action-find-support-for-selected-project
                        'find-support-for-selected-project)))
           called)
      (cl-letf (((symbol-function 'delib-flow--execute-local-stage)
                 (lambda (run stage-id)
                   (push stage-id called)
                   run))
                ((symbol-function 'delib-flow--rerender-current-result)
                 #'ignore))
        (dolist (spec specs)
          (let ((delib-flow--active-run (nth 0 spec)))
            (funcall (nth 1 spec)))))
      (should
       (equal
        '(find-support-for-selected-project
          draft-selected-project
          find-support-for-selected-reference-note
          draft-selected-reference-note
          find-support-for-selected-waiting-for
          draft-selected-waiting-for
          find-support-for-selected-action
          draft-selected-action)
        called)))))

(ert-deftest delib-flow-selected-artifact-stage-actions-error-without-active-run ()
  (let ((delib-flow--active-run nil))
    (dolist (command
             '(delib-flow-action-draft-selected-action
               delib-flow-action-find-support-for-selected-action
               delib-flow-action-draft-selected-waiting-for
               delib-flow-action-find-support-for-selected-waiting-for
               delib-flow-action-draft-selected-reference-note
               delib-flow-action-find-support-for-selected-reference-note
               delib-flow-action-draft-selected-project
               delib-flow-action-find-support-for-selected-project))
      (should-error (funcall command) :type 'user-error))))

(ert-deftest delib-flow-selected-artifact-stage-actions-error-without-selection ()
  (let ((delib-flow--active-run
         (delib-flow--initialize-run
          (list :title "Example"
                :content "* Example\nBody.\n"))))
    (dolist (command
             '(delib-flow-action-draft-selected-action
               delib-flow-action-find-support-for-selected-action
               delib-flow-action-draft-selected-waiting-for
               delib-flow-action-find-support-for-selected-waiting-for
               delib-flow-action-draft-selected-reference-note
               delib-flow-action-find-support-for-selected-reference-note
               delib-flow-action-draft-selected-project
               delib-flow-action-find-support-for-selected-project))
      (should-error (funcall command) :type 'user-error))))


(ert-deftest delib-flow-selected-filing-item-prefers-drafted-reference-note ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body
                                  "* Working draft\nAtlas note body.\n"))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'reference-notes
                (list :candidates (list candidate)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate)
                      :selected-draft drafted-item))))
         (selected-run (delib-flow-test--set-filing-selection run "1"))
         (selected-item (delib-flow--selected-filing-item selected-run)))
    (should (equal "* Working draft\nAtlas note body.\n"
                   (plist-get selected-item :draft-body)))))

(ert-deftest delib-flow-selected-filing-item-prefers-drafted-action ()
  (let* ((candidate (list :kind 'next-action
                          :text "Draft kickoff follow-up"))
         (drafted-item (plist-put (copy-tree candidate)
                                  :text
                                  "Send kickoff follow-up with owners and due dates"))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'actions
                (list :candidates (list candidate)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate)
                      :selected-draft drafted-item))))
         (selected-run (delib-flow-test--set-filing-selection run "1"))
         (selected-item (delib-flow--selected-filing-item selected-run)))
    (should (equal "Send kickoff follow-up with owners and due dates"
                   (plist-get selected-item :text)))))

(ert-deftest delib-flow-selected-filing-item-prefers-drafted-waiting-for ()
  (let* ((candidate (list :kind 'waiting-for
                          :text "Waiting for Pat to confirm launch date"))
         (drafted-item (plist-put (copy-tree candidate)
                                  :text
                                  "Waiting for Pat to confirm the launch date and owner handoff"))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'waiting-fors
                (list :candidates (list candidate)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate)
                      :selected-draft drafted-item))))
         (selected-run (delib-flow-test--set-filing-selection run "1"))
         (selected-item (delib-flow--selected-filing-item selected-run)))
    (should (equal "Waiting for Pat to confirm the launch date and owner handoff"
                   (plist-get selected-item :text)))))

(ert-deftest delib-flow-selected-filing-item-prefers-drafted-project ()
  (let* ((candidate (list :kind 'project
                          :title "Project Atlas"
                          :text "Project Atlas"
                          :state 'active
                          :first-item (list :kind 'next-action
                                            :text "Define first deliverable for Project Atlas")))
         (drafted-item (plist-put (copy-tree candidate)
                                  :first-item
                                  (list :kind 'next-action
                                        :text "Draft launch outline for Project Atlas")))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run (list :title "Example"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'project-proposals
                (list :candidates (list candidate)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate)
                      :selected-draft drafted-item))))
         (selected-run (delib-flow-test--set-filing-selection run "1"))
         (selected-item (delib-flow--selected-filing-item selected-run)))
    (should (equal "Draft launch outline for Project Atlas"
                   (plist-get (plist-get selected-item :first-item)
                              :text)))))

(ert-deftest delib-flow-staged-project-preview-includes-attached-child-items ()
  (let* ((item (list :kind 'project
                     :title "Project Atlas"
                     :text "Project Atlas"
                     :state 'active
                     :first-item (list :kind 'next-action
                                       :text "Draft launch outline for Project Atlas")
                     :child-items
                     (list (list :kind 'next-action
                                 :text "Draft launch outline for Project Atlas")
                           (list :kind 'waiting-for
                                 :text "Waiting for Pat to confirm the launch dependency"))
                     :warnings nil))
         (preview (delib-flow--staged-project-item-preview item
                                                           (delib-flow--initialize-run
                                                            (list :title "Example")))))
    (should (string-match-p "\\* Project Atlas" preview))
    (should (string-match-p "\\*\\* TODO Draft launch outline for Project Atlas" preview))
    (should (string-match-p "\\*\\* WAITING Waiting for Pat to confirm the launch dependency"
                            preview))))

(ert-deftest delib-flow-select-approved-filing-actions-blocks-undrafted-selected-action ()
  (let* ((item (list :kind 'next-action
                     :text "Draft kickoff follow-up"
                     :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-candidates
                (plist-put
                 (delib-flow-test--accept-inspect
                  (delib-flow-test--accept-match
                   (delib-flow--initialize-run
                    (list :title "Example"
                          :content "* Example\nBody line\n"))))
                 :filing
                 (list :draft-items (list item)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'actions
                (list item))))
         (selected-run (delib-flow-test--set-filing-selection run "1"))
         (result (delib-flow--select-approved-filing-actions-result selected-run))
         (warning (car (plist-get result :blocking-warnings))))
    (should (plist-get result :approval-blocked-p))
    (should (eq 'selected-item-needs-draft (plist-get warning :code)))))

(ert-deftest delib-flow-staged-content-preview-waits-for-selected-draft ()
  (let* ((item (list :kind 'project
                     :title "Project Atlas"
                     :text "Project Atlas"
                     :state 'active
                     :first-item (list :kind 'next-action
                                       :text "Define first deliverable for Project Atlas")
                     :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-candidates
                (plist-put
                 (delib-flow-test--accept-inspect
                  (delib-flow-test--accept-match
                   (delib-flow--initialize-run
                    (list :title "Example"
                          :content "* Example\nBody line\n"))))
                 :filing
                 (list :draft-items (list item)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'project-proposals
                (list item))))
         (selected-run (delib-flow-test--set-filing-selection run "1")))
    (should (string-match-p "No staged content is ready yet"
                            (delib-flow--staged-content-preview-status selected-run)))
    (should (string-match-p "still needs an item-local draft"
                            (delib-flow--staged-content-preview-text selected-run)))))

(ert-deftest delib-flow-project-package-approval-bundles-ready-child-items ()
  (let* ((project (list :kind 'project
                        :title "Steno Exercise Fix"
                        :text "Steno Exercise Fix"
                        :state 'active
                        :child-items
                        (list (list :kind 'next-action
                                    :text "Investigate and fix steno exercise"))
                        :warnings nil))
         (action (list :kind 'next-action
                       :text "Investigate exercises storage on the website"
                       :warnings nil))
         (note (list :kind 'reference-note
                     :text "Create general PKM note for Steno exercise storage"
                     :note-type 'general-pkm
                     :draft-stage 'suggest-reference-notes))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content "* Broken steno exercise\nBody line\n"))
                 :filing
                 (list :draft-items (list project action note)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'project-proposals
                (list :candidates (list project)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id project)
                      :selected-draft project))))
         (selected (delib-flow-test--set-filing-selection run "1"))
         (approved (delib-flow--run-stage-locally selected 'select-approved-filing-actions))
         (approved-items (plist-get (plist-get approved :filing) :approved-items))
         (approved-project (car approved-items))
         (remaining (plist-get (plist-get approved :filing) :draft-items)))
    (should (= 1 (length approved-items)))
    (should (eq 'project (plist-get approved-project :kind)))
    (should (equal "Investigate exercises storage on the website"
                   (plist-get (plist-get approved-project :first-item) :text)))
    (should (= 1 (length remaining)))
    (should (eq 'reference-note (plist-get (car remaining) :kind)))))

(ert-deftest delib-flow-file-approved-project-package-uses-active-state-bucket ()
  (delib-flow-test--with-temp-project-file
      "* Active :active:\n* Complete :complete:\n* Waiting :waiting:\n"
    (let* ((project (list :kind 'project
                          :title "Steno Exercise Fix"
                          :text "Steno Exercise Fix"
                          :state 'active
                          :child-items
                          (list (list :kind 'next-action
                                      :text "Investigate and fix steno exercise"))
                          :warnings nil))
           (action (list :kind 'next-action
                         :text "Investigate exercises storage on the website"
                         :warnings nil))
           (run (delib-flow--seed-filing-selection-block
                 (delib-flow--set-artifact-family-state
                  (plist-put
                   (delib-flow--initialize-run
                    (list :title "Broken steno exercise"
                          :content "* Broken steno exercise\nBody line\n"))
                   :filing
                   (list :draft-items (list project action)
                         :approved-items nil
                         :rejected-items nil
                         :preview-text nil
                         :selection-blocked-item nil
                         :selection-blocking-warnings nil
                         :selection-blocked-selection nil
                         :selection-blocked-notes nil
                         :conflicts nil
                         :target-locations nil))
                  'project-proposals
                  (list :candidates (list project)
                        :selected-candidate-id
                        (delib-flow--artifact-candidate-id project)
                        :selected-draft project))))
           (selected (delib-flow-test--set-filing-selection run "1"))
           (approved (delib-flow--run-stage-locally selected 'select-approved-filing-actions))
           (_updated (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
      (should (string-match-p
               (regexp-quote "* Active :active:\n** Steno Exercise Fix")
               content))
      (should (string-match-p
               (regexp-quote "*** TODO Investigate exercises storage on the website")
               content))
      (should (string-match-p ":CREATED: " content))
      (should (string-match-p ":SOURCE_ARTIFACT: Investigate exercises storage on the website"
                              content)))))


(ert-deftest delib-flow-project-package-persists-root-when-child-item-is-selected ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow-test--set-operator-intent
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content (string-join
                                  '("* Broken steno exercise"
                                    "https://example.com/drill?id=one"
                                    "https://example.com/drill?id=two")
                                  "\n")))
                 "I need to figure out how the website stores exercises"))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected-project (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected-project 'draft-selected-project))
           (extracted (delib-flow--run-stage-locally drafted 'extract-actions))
           (selected-child (delib-flow-test--set-filing-selection extracted "2"))
           (workspace (with-current-buffer
                          (delib-flow--render-focused-filing-workspace-buffer selected-child)
                        (buffer-string)))
           (planned (delib-flow--planned-file-locations selected-child))
           (outside (delib-flow--project-package-outside-items selected-child)))
      (should (string-match-p "Project: Fix broken steno website exercises" workspace))
      (should (string-match-p "PROJECT Fix broken steno website exercises" workspace))
      (should (string-match-p "TODO " workspace))
      (should-not (string-match-p "TODO Investigate and fix broken steno website exercises"
                                  workspace))
      (should-not (string-match-p "No selected project package is active" workspace))
      (should (string-match-p "If you file now, delib-flow will stage the active project package bundle into these targets." workspace))
      (should planned)
      (should (string-prefix-p "/tmp/delib-flow-projects"
                               (plist-get (car planned) :target)))
      (should-not (seq-some (lambda (item)
                              (eq (plist-get item :kind) 'project))
                            outside)))))

(ert-deftest delib-flow-project-mode-decision-strip-uses-package-state ()
  (delib-flow-test--with-temp-project-file
      "* Active :active:\n* Complete :complete:\n* Waiting :waiting:\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "https://example.com/drill?id=two"
                                   "I need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected 'draft-selected-project))
           (text (with-current-buffer
                     (delib-flow--render-control-buffer drafted)
                   (buffer-string))))
      (should (string-match-p "Recommended next step: Extract Actions" text))
      (should (string-match-p "Active project: Fix steno exercise" text))
      (should (string-match-p "Active filing item: Project package shell `Fix steno exercise`" text))
      (should (string-match-p "Planned target preview: .*::Fix steno exercise" text))
      (should (string-match-p "Active loop: Filing preview" text))
      (should (string-match-p "\\*\\*\\* Active project loop" text))
      (should (string-match-p "Primary numbered palette: `Filing preview > Project workspace`" text))
      (should (string-match-p "Current step: Extract follow-on work" text))
      (should (string-match-p "Primary next action: Extract Actions" text))
      (should (string-match-p "\\*\\*\\* Recover / reframe" text))
      (should-not (string-match-p "Recommended next step: Discover Relevant Reference Material" text)))))

(ert-deftest delib-flow-selected-proposed-project-does-not-surface-extraction-before-draft ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "Need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (text (with-current-buffer
                     (delib-flow--render-focused-filing-workspace-buffer selected)
                   (buffer-string))))
      (should (eq 'no-match (delib-flow--match-status selected)))
      (should (string-match-p "\\*\\* Do here now" text))
      (should (string-match-p "One project package is selected but not yet drafted" text))
      (should (string-match-p "\\[1\\] Draft Selected Project" text))
      (should-not (string-match-p "\\[[0-9]+\\] Extract Actions" text))
      (should-not (string-match-p "\\[[0-9]+\\] Extract Waiting-For" text))
      (should-not (string-match-p "\\[[0-9]+\\] Suggest Reference Notes" text))
      (should (string-match-p "Draft Selected Project" text)))))

(ert-deftest delib-flow-project-package-approval-replaces-goal-like-seed-with-concrete-action ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow-test--set-operator-intent
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content (string-join
                                  '("* Broken steno exercise"
                                    "https://example.com/drill?id=one"
                                    "https://example.com/drill?id=two")
                                  "\n")))
                 "I need to figure out how the website stores exercises"))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected 'draft-selected-project))
           (extracted (delib-flow--run-stage-locally drafted 'extract-actions))
           (approved (delib-flow--run-stage-locally
                      (delib-flow-test--set-filing-selection extracted "1")
                      'select-approved-filing-actions))
           (project (car (plist-get (plist-get approved :filing) :approved-items)))
           (first-item (plist-get project :first-item)))
      (should (eq 'project (plist-get project :kind)))
      (should (eq 'next-action (plist-get first-item :kind)))
      (should (delib-flow--non-empty-string-p (plist-get first-item :text)))
      (should-not (equal "Investigate and fix broken steno website exercises"
                         (plist-get first-item :text)))
      (should-not (equal (plist-get project :title)
                         (plist-get first-item :text))))))

(ert-deftest delib-flow-project-workspace-surfaces-approve-package-from-active-child-state ()
  (let* ((project (list :kind 'project
                        :title "Steno Exercise Fix"
                        :text "Steno Exercise Fix"
                        :state 'active
                        :child-items
                        (list (list :kind 'next-action
                                    :text "Analyze website's exercise storage mechanism"))
                        :warnings nil))
         (action (list :kind 'next-action
                       :text "Analyze website's exercise storage mechanism"
                       :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content "* Broken steno exercise\nBody line\n"))
                 :filing
                 (list :draft-items (list project action)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'project-proposals
                (list :candidates (list project)
                      :selected-candidate-id nil
                      :selected-draft nil))))
         (selected (delib-flow-test--set-filing-selection run "2"))
         (text (with-current-buffer
                   (delib-flow--render-focused-filing-workspace-buffer selected)
                 (buffer-string))))
    (should (string-match-p "What files together now: project `Steno Exercise Fix` plus 1 included child item" text))
    (should (string-match-p "\\[1\\] Draft Selected Action" text))
    (should (string-match-p "Approve Package" text))
    (should-not (string-match-p "No project-local actions are available yet" text))))

(ert-deftest delib-flow-select-approved-filing-actions-falls-back-to-active-project-package ()
  (let* ((project (list :kind 'project
                        :title "Steno Exercise Fix"
                        :text "Steno Exercise Fix"
                        :state 'active
                        :child-items
                        (list (list :kind 'next-action
                                    :text "Analyze website's exercise storage mechanism"))
                        :warnings nil))
         (action (list :kind 'next-action
                       :text "Analyze website's exercise storage mechanism"
                       :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content "* Broken steno exercise\nBody line\n"))
                 :filing
                 (list :draft-items (list project action)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'project-proposals
                (list :candidates (list project)
                      :selected-candidate-id nil
                      :selected-draft nil))))
         (approved (delib-flow--run-stage-locally run 'select-approved-filing-actions))
         (approved-items (plist-get (plist-get approved :filing) :approved-items)))
    (should (= 1 (length approved-items)))
    (should (eq 'project (plist-get (car approved-items) :kind)))
    (should (equal "Analyze website's exercise storage mechanism"
                   (plist-get (plist-get (car approved-items) :first-item) :text)))))

(ert-deftest delib-flow-project-workspace-surfaces-approve-package-for-embedded-child-only ()
  (let* ((project (list :kind 'project
                        :title "Steno Exercise Fix"
                        :text "Steno Exercise Fix"
                        :state 'active
                        :child-items
                        (list (list :kind 'next-action
                                    :text "Analyze website's exercise storage mechanism"))
                        :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content "* Broken steno exercise\nBody line\n"))
                :filing
                (list :draft-items (list project)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))))
         (text (with-current-buffer
                   (delib-flow--render-focused-filing-workspace-buffer run)
                 (buffer-string))))
    (should (eq 'review-package (delib-flow--project-workflow-current-step run)))
    (should (string-match-p "What files together now: project `Steno Exercise Fix` plus 1 included child item" text))
    (should (string-match-p "Approve Package" text))
    (should (string-match-p "Reject Package Item" text))))

(ert-deftest delib-flow-selected-project-workflow-warns-when-extraction-retries-stay-weak ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "Review steno exercise"
                                   "Practice finger drills")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected 'draft-selected-project))
           (once (delib-flow--run-stage-locally drafted 'extract-actions))
           (twice (delib-flow--run-stage-locally once 'extract-actions))
           (warning (delib-flow--project-extraction-soft-warning twice))
           (text (with-current-buffer
                     (delib-flow--render-focused-filing-workspace-buffer twice)
                   (buffer-string))))
      (should warning)
      (should (eq 'extract-actions (plist-get warning :stage-id)))
      (should (string-match-p "weak or repetitive action candidates" (plist-get warning :message)))
      (should (string-match-p "Edit Operator Intent" text))
      (should (string-match-p "Refinement warning: Extraction retries are yielding weak or repetitive action candidates" text))
      (should (string-match-p "Why this step: Extraction retries are yielding weak or repetitive action candidates" text)))))

(ert-deftest delib-flow-extract-actions-reports-operator-intent-and-proposed-project-context ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow-test--set-operator-intent
                 (delib-flow--initialize-run
                  (list :title "Broken steno exercise"
                        :content (string-join
                                  '("* Broken steno exercise"
                                    "https://example.com/drill?id=one"
                                    "https://example.com/drill?id=two")
                                  "\n")))
                 "this is about fixing a broken website"))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected 'draft-selected-project))
           (updated (delib-flow--run-stage-locally drafted 'extract-actions))
           (entry (car (last (plist-get (delib-flow--run-stage-history updated) :entries))))
           (raw (plist-get entry :raw-output))
           (normalized (plist-get entry :normalized-output))
           (actions (plist-get raw :actions)))
      (should (equal "this is about fixing a broken website"
                     (plist-get raw :operator-intent)))
      (should (eq 'proposed-project-draft
                  (plist-get raw :project-context-kind)))
      (should (equal 0 (plist-get raw :previous-attempt-count)))
      (should-not (plist-get raw :similar-to-previous-p))
      (should (string-match-p "Project context: proposed-project-draft (Fix broken steno website exercises)"
                              normalized))
      (should (string-match-p "Prior attempts: 0" normalized))
      (should (string-match-p "Similar to previous attempt: no" normalized))
      (should (string-match-p "Operator intent: this is about fixing a broken website"
                              normalized))
      (should (string-match-p "Investigate and reproduce broken steno website exercises"
                              (plist-get (car actions) :text))))))

(ert-deftest delib-flow-extract-waiting-for-reports-proposed-project-context-and-speculative-warning ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/drill?id=one"
                                   "https://example.com/drill?id=two")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (selected (delib-flow-test--set-filing-selection proposed "1"))
           (drafted (delib-flow--run-stage-locally selected 'draft-selected-project))
           (updated (delib-flow--run-stage-locally drafted 'extract-waiting-for))
           (entry (car (last (plist-get (delib-flow--run-stage-history updated) :entries))))
           (raw (plist-get entry :raw-output))
           (normalized (plist-get entry :normalized-output))
           (item (car (plist-get raw :waiting-fors))))
      (should (eq 'proposed-project-draft
                  (plist-get raw :project-context-kind)))
      (should (string-match-p "Project context: proposed-project-draft (Fix steno exercise)"
                              normalized))
      (should (delib-flow--draft-item-has-warning-code-p
               item 'waiting-for-speculative-owner))
      (should (string-match-p "project owner" (plist-get item :text))))))

(ert-deftest delib-flow-current-filing-choice-explains-selected-action-next-step ()
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
           (selected-run
            (delib-flow-test--set-filing-selection integrated "1")))
      (should (string-match-p
               "currently selected action candidate"
               (delib-flow--current-filing-choice-status selected-run)))
      (should (string-match-p
               "run `Draft Selected Action` if you want a one-action wording pass before approval"
               (delib-flow--current-filing-choice-text selected-run))))))

(ert-deftest delib-flow-peek-staged-content-opens-preview-buffer ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected (delib-flow-test--approve-selected-filing-item
                      (delib-flow-test--set-filing-selection integrated "1"))))
      (let ((delib-flow--active-run selected))
        (unwind-protect
            (progn
              (delib-flow-peek-staged-content)
              (with-current-buffer delib-flow-staged-content-preview-buffer-name
                (goto-char (point-min))
                (should (derived-mode-p 'org-mode))
                (should visual-line-mode)
                (should (search-forward "* Delib-Flow staged output" nil t))
                (should (search-forward "Current mode: exact staged output" nil t))
                (should (search-forward "Target: " nil t))
                (should (search-forward "State: staged only; not saved" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-staged-content-preview-buffer-name))
            (kill-buffer (get-buffer delib-flow-staged-content-preview-buffer-name))))))))

(ert-deftest delib-flow-peek-selected-draft-opens-consequence-buffer ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected (delib-flow-test--set-filing-selection
                      (delib-flow--run-stage-locally drafted 'integrate-into-source)
                      "1")))
      (let ((delib-flow--active-run selected))
        (unwind-protect
            (progn
              (delib-flow-peek-selected-draft)
              (with-current-buffer delib-flow-consequence-preview-buffer-name
                (goto-char (point-min))
                (should (search-forward "* Delib-Flow selected draft" nil t))
                (should (search-forward "Current mode: selected draft" nil t))
                (should (search-forward "Write follow-up note for Alpha Project kickoff" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-consequence-preview-buffer-name))
            (kill-buffer (get-buffer delib-flow-consequence-preview-buffer-name))))))))

(ert-deftest delib-flow-peek-selected-draft-renders-history-compare ()
         (let* ((candidate (list :kind 'next-action
                          :text "Queue action text"
                          :warnings nil))
         (current (list :kind 'next-action
                        :text "Current drafted wording"
                        :evidence-snapshot
                        (list :draft-reason "Current rationale"
                              :candidate-origin 'source
                              :source-title "Example"
                              :source-excerpts '("- Current source excerpt")
                              :support-items '((:identity "Atlas brief"
                                                :title "Atlas brief"
                                                :score 8
                                                :reason-text "title-overlap")
                                               (:identity "Shared brief"
                                                :title "Shared brief"
                                                :score 8
                                                :reason-text "focused-retained"
                                                :focus "current focus"))
                              :support-lines '("- Atlas brief" "- Shared brief")
                              :support-context-lines '("Atlas context")
                              :family-review-heading "Action-specific checks"
                              :family-review-lines '("Opening verb: `draft`; this reads like a directly executable next step."
                                                     "Outcome cue: `for launch brief`."
                                                     "Scope check: 7 words; this remains task-sized.")
                              :quality-gaps '("- Tighten owner")
                              :support-count 2)))
         (previous (list :kind 'next-action
                         :text "Previous drafted wording"
                         :evidence-snapshot
                         (list :draft-reason "Previous rationale"
                               :candidate-origin 'retained-context
                               :source-title "Example"
                               :source-excerpts '("- Previous source excerpt")
                               :support-items '((:identity "Prior brief"
                                                 :title "Prior brief"
                                                 :score 4
                                                 :reason-text "prior-overlap")
                                                (:identity "Shared brief"
                                                 :title "Shared brief"
                                                 :score 3
                                                 :reason-text "prior-reason"
                                                 :focus "prior focus"))
                               :support-lines '("- Prior brief" "- Shared brief")
                               :support-context-lines '("Prior context")
                               :family-review-heading "Action-specific checks"
                               :family-review-lines '("Opening verb: `clarify`; this still reads more like review or clarification than execution."
                                                      "Outcome cue: no explicit recipient, target, or deliverable is named yet."
                                                      "Scope check: 3 words; this remains task-sized.")
                               :quality-gaps '("- Add due date")
                               :support-count 2)))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line\n"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
	                'actions
	                (list :candidates (list candidate)
	                      :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
	                      :selected-draft current
	                      :draft-history (list previous)))))
	         (selected (delib-flow--set-filing-selection-value run "1")))
    (let ((delib-flow--active-run selected))
      (unwind-protect
          (progn
            (delib-flow-peek-selected-draft)
            (with-current-buffer delib-flow-consequence-preview-buffer-name
              (let ((text (buffer-string)))
                (should (string-match-p "\\*\\* Draft history and compare" text))
                (should (string-match-p "\\*\\* Evidence and quality review" text))
                (should (string-match-p "Why this draft exists" text))
                (should (string-match-p "Source excerpts" text))
                (should (string-match-p "Frozen evidence review" text))
                (should (string-match-p "Frozen evidence changes" text))
                (should (string-match-p "Candidate origin changed: from retained reference material -> directly from the current source" text))
                (should (string-match-p "Frozen support items" text))
                (should (string-match-p "Atlas brief \\[score 8\\]" text))
                (should (string-match-p "Support items removed: Prior brief" text))
                (should (string-match-p "Support details changed: Shared brief (score 3 -> 8, reason prior-reason -> focused-retained, focus prior focus -> current focus)" text))
                (should (string-match-p "Family review changes: added Opening verb: `draft`; this reads like a directly executable next step. \\| Outcome cue: `for launch brief`. \\| Scope check: 7 words; this remains task-sized.; removed Opening verb: `clarify`; this still reads more like review or clarification than execution. \\| Outcome cue: no explicit recipient, target, or deliverable is named yet. \\| Scope check: 3 words; this remains task-sized." text))
                (should (string-match-p "Quality risk changes: introduced - Tighten owner; resolved - Add due date" text))
                (should (string-match-p "Frozen quality gaps" text))
                (should (string-match-p "Current drafted wording" text))
                (should (string-match-p "Previous drafted wording" text)))))
	        (when (buffer-live-p (get-buffer delib-flow-consequence-preview-buffer-name))
	          (kill-buffer (get-buffer delib-flow-consequence-preview-buffer-name)))))))

(ert-deftest delib-flow-peek-selected-support-opens-consequence-buffer ()
  (let* ((candidate (list :kind 'next-action
                          :text "Write follow-up note for Alpha Project kickoff"
                          :warnings nil))
         (support (list :title "Atlas brief"
                        :filter-reasons '("focused-retained")
                        :support-score 3
                        :support-reasons '("title-overlap")))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line\n"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'actions
                (list :candidates (list candidate)
                      :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                      :selected-draft candidate
                      :selected-support-candidates (list support)
                      :selected-support-context "- Atlas brief - Constraint summary"))))
         (selected (delib-flow--set-filing-selection-value run "1")))
    (let ((delib-flow--active-run selected))
      (unwind-protect
          (progn
            (delib-flow-peek-selected-support)
            (with-current-buffer delib-flow-consequence-preview-buffer-name
              (goto-char (point-min))
              (should (search-forward "* Delib-Flow support excerpt" nil t))
              (should (search-forward "Current mode: support excerpt" nil t))
              (should (search-forward "Atlas brief" nil t))
              (should (search-forward "Score: 3" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-consequence-preview-buffer-name))
          (kill-buffer (get-buffer delib-flow-consequence-preview-buffer-name)))))))

(ert-deftest delib-flow-rerender-refreshes-open-staged-preview-buffer ()
  (let ((delib-flow-general-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\n"))
    (delib-flow-test--with-temp-zk-root ()
      (let* ((candidate (list :kind 'reference-note
                              :text "Create general PKM note for Building personal AI advisors"
                              :note-type 'general-pkm
                              :warnings nil))
             (drafted-one (plist-put (copy-tree candidate)
                                     :draft-body
                                     "* Working draft\nFirst draft.\n"))
             (drafted-two (plist-put (copy-tree candidate)
                                     :draft-body
                                     "* Working draft\nSecond draft.\n"))
             (run (delib-flow--seed-filing-selection-block
                   (delib-flow--set-artifact-family-state
                    (plist-put
                     (delib-flow--initialize-run
                      (list :title "Example"
                            :content "* Example\nBody line.\n"))
                     :filing
                     (list :draft-items (list candidate)
                           :approved-items nil
                           :rejected-items nil
                           :preview-text nil
                           :selection-blocked-item nil
                           :selection-blocking-warnings nil
                           :selection-blocked-selection nil
                           :selection-blocked-notes nil
                           :conflicts nil
                           :target-locations nil))
                    'reference-notes
                    (list :candidates (list candidate)
                          :selected-candidate-id
                          (delib-flow--artifact-candidate-id candidate)
                          :selected-draft drafted-one))))
             (run (delib-flow--set-filing-selection-value run "1"))
             (delib-flow--active-run run))
        (unwind-protect
            (progn
              (delib-flow--show-staged-content-preview-buffer run)
              (setq delib-flow--active-run
                    (delib-flow--set-artifact-family-selected-draft
                     delib-flow--active-run
                     'reference-notes
                     drafted-two))
              (delib-flow--render-active-run-buffer delib-flow--active-run "Filing preview")
              (with-current-buffer delib-flow-staged-content-preview-buffer-name
                (should (string-match-p "Second draft\\." (buffer-string)))
                (should-not (string-match-p "First draft\\." (buffer-string)))))
          (when (buffer-live-p (get-buffer delib-flow-staged-content-preview-buffer-name))
            (kill-buffer (get-buffer delib-flow-staged-content-preview-buffer-name))))))))

(ert-deftest delib-flow-rerender-refreshes-open-draft-consequence-buffer ()
  (let* ((candidate (list :kind 'next-action
                          :text "Write follow-up note for Alpha Project kickoff"
                          :warnings nil))
         (drafted-one (plist-put (copy-tree candidate)
                                 :text "Old sharpened wording"))
         (drafted-two (plist-put (copy-tree candidate)
                                 :text "Sharper follow-up wording"))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line.\n"))
                 :filing
                 (list :draft-items (list candidate)
                       :approved-items nil
                       :rejected-items nil
                       :preview-text nil
                       :selection-blocked-item nil
                       :selection-blocking-warnings nil
                       :selection-blocked-selection nil
                       :selection-blocked-notes nil
                       :conflicts nil
                       :target-locations nil))
                'actions
                (list :candidates (list candidate)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate)
                      :selected-draft drafted-one))))
         (run (delib-flow--set-filing-selection-value run "1"))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (delib-flow-peek-selected-draft)
          (setq delib-flow--active-run
                (delib-flow--set-artifact-family-selected-draft
                 delib-flow--active-run
                 'actions
                 drafted-two))
          (delib-flow--render-active-run-buffer delib-flow--active-run "Filing preview")
          (with-current-buffer delib-flow-consequence-preview-buffer-name
            (should (string-match-p "Sharper follow-up wording" (buffer-string)))
            (should-not (string-match-p "Old sharpened wording" (buffer-string)))))
      (when (buffer-live-p (get-buffer delib-flow-consequence-preview-buffer-name))
        (kill-buffer (get-buffer delib-flow-consequence-preview-buffer-name))))))

(ert-deftest delib-flow-restore-latest-artifact-family-draft-swaps-current-and-history ()
  (let* ((current (list :kind 'next-action
                        :text "Current drafted wording"))
         (previous (list :kind 'next-action
                         :text "Previous drafted wording"))
         (older (list :kind 'next-action
                      :text "Older drafted wording"))
         (run (delib-flow--set-artifact-family-state
               (delib-flow--initialize-run (list :title "Example"))
               'actions
               (list :candidates nil
                     :selected-candidate-id "action-1"
                     :selected-draft current
                     :draft-history (list previous older))))
         (updated (delib-flow--restore-latest-artifact-family-draft run 'actions))
         (state (plist-get (plist-get updated :artifacts) 'actions)))
    (should (equal "Previous drafted wording"
                   (plist-get (plist-get state :selected-draft) :text)))
    (should (equal '("Older drafted wording" "Current drafted wording")
                   (mapcar (lambda (item) (plist-get item :text))
                           (plist-get state :draft-history))))
    (should (plist-get (plist-get (plist-get state :selected-draft)
                                  :evidence-snapshot)
                       :source-title))))

(ert-deftest delib-flow-restore-artifact-family-draft-at-index-restores-earlier-revision ()
  (let* ((current (list :kind 'next-action
                        :text "Current drafted wording"))
         (previous (list :kind 'next-action
                         :text "Previous drafted wording"))
         (older (list :kind 'next-action
                      :text "Older drafted wording"))
         (run (delib-flow--set-artifact-family-state
               (delib-flow--initialize-run (list :title "Example"))
               'actions
               (list :candidates nil
                     :selected-candidate-id "action-1"
                     :selected-draft current
                     :draft-history (list previous older))))
         (updated (delib-flow--restore-artifact-family-draft-at-index run 'actions 1))
         (state (plist-get (plist-get updated :artifacts) 'actions)))
    (should (equal "Older drafted wording"
                   (plist-get (plist-get state :selected-draft) :text)))
    (should (equal '("Previous drafted wording" "Current drafted wording")
                   (mapcar (lambda (item) (plist-get item :text))
                           (plist-get state :draft-history))))))

(ert-deftest delib-flow-choose-saved-selected-action-draft-command-restores-chosen-history-entry ()
  (let* ((current (list :kind 'next-action
                        :text "Current drafted wording"))
         (previous (list :kind 'next-action
                         :text "Previous drafted wording"))
         (older (list :kind 'next-action
                      :text "Older drafted wording"))
         (delib-flow--active-run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run (list :title "Example"))
           'actions
           (list :candidates nil
                 :selected-candidate-id "action-1"
                 :selected-draft current
                 :draft-history (list previous older))))
         (labels (delib-flow--saved-draft-revision-labels delib-flow--active-run 'actions)))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args)
                 (caar (last labels)))))
      (delib-flow-action-choose-saved-selected-action-draft))
    (should (equal "Older drafted wording"
                   (plist-get (delib-flow--artifact-family-selected-draft
                               delib-flow--active-run 'actions)
                              :text)))
    (should (equal '("Previous drafted wording" "Current drafted wording")
                   (mapcar (lambda (item) (plist-get item :text))
                           (delib-flow--artifact-family-draft-history
                            delib-flow--active-run 'actions))))))

(ert-deftest delib-flow-family-evidence-review-text-prefers-frozen-draft-snapshot ()
  (let* ((candidate (list :kind 'next-action
                          :text "Queue action text"
                          :source 'source))
         (draft (list :kind 'next-action
                      :text "Current drafted wording"
                      :evidence-snapshot
                      (list :draft-reason "Frozen rationale"
                            :candidate-origin 'retained-context
                            :source-title "Frozen source title"
                            :source-evidence-lines '("Frozen evidence")
                            :source-excerpts '("- L9: Frozen excerpt")
                            :support-items '((:identity "Frozen support note"
                                              :title "Frozen support note"
                                              :score 8
                                              :reason-text "focused-retained"))
                            :support-lines '("- Frozen support note")
                            :support-context-lines '("Frozen support context")
                            :quality-gaps '("- Frozen quality gap"))))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Live source title"
                       :content "* Live source title\n- Live evidence\n"))
                :filing
                (list :draft-items (list candidate)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'actions
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft draft
                     :draft-history nil
                     :selected-support-candidates nil
                     :selected-support-context nil)))
         (text (delib-flow--family-evidence-review-text run 'actions)))
    (should (string-match-p "Frozen rationale" text))
    (should (string-match-p "Selected candidate origin: from retained reference material" text))
    (should (string-match-p "Frozen source title" text))
    (should (string-match-p "Frozen evidence" text))
    (should (string-match-p "L9: Frozen excerpt" text))
    (should (string-match-p "Frozen support note \\[score 8\\]" text))
    (should (string-match-p "Frozen support context" text))
    (should (string-match-p "Frozen quality gap" text))
    (should-not (string-match-p "Live source title" text))))

(ert-deftest delib-flow-family-evidence-review-renders-action-specific-checks ()
  (let* ((candidate (list :kind 'next-action
                          :text "Draft kickoff follow-up"
                          :source 'source))
         (draft (list :kind 'next-action
                      :text "Draft kickoff follow-up for launch brief"))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Live source title"
                       :content "* Live source title\n- Draft kickoff follow-up\n"))
                :filing
                (list :draft-items (list candidate)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'actions
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft draft
                     :draft-history nil
                     :selected-support-candidates nil
                     :selected-support-context nil)))
         (text (delib-flow--family-evidence-review-text run 'actions)))
    (should (string-match-p "Action-specific checks" text))
    (should (string-match-p "Opening verb: `draft`" text))
    (should (string-match-p "Outcome cue: `for launch brief`" text))
    (should (string-match-p "Scope check: 7 words; this remains task-sized" text))
    (should (string-match-p "Source alignment: the current wording still traces back to `Draft kickoff follow-up`" text))))

(ert-deftest delib-flow-family-evidence-review-renders-waiting-for-specific-checks ()
  (let* ((candidate (list :kind 'waiting-for
                          :text "Waiting for Pat to confirm launch date"
                          :source 'source))
         (draft (list :kind 'waiting-for
                      :text "Waiting for Pat to confirm launch date"))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Live source title"
                       :content "* Live source title\n- Waiting for Pat to confirm launch date\n"))
                :filing
                (list :draft-items (list candidate)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'waiting-fors
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft draft
                     :draft-history nil
                     :selected-support-candidates nil
                     :selected-support-context nil)))
         (text (delib-flow--family-evidence-review-text run 'waiting-fors)))
    (should (string-match-p "Waiting-for-specific checks" text))
    (should (string-match-p "Waiting-state phrasing: explicit `Waiting for \\.\\.\\.` wording is present" text))
    (should (string-match-p "Owner cue: `Pat` currently owns the response or dependency" text))
    (should (string-match-p "Blocked outcome: `confirm launch date` is the current missing response or deliverable" text))))

(ert-deftest delib-flow-family-evidence-review-renders-note-specific-checks ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Durable idea"
                          :note-type 'general-pkm
                          :source 'source))
         (draft (list :kind 'reference-note
                      :text "Create general PKM note for Durable idea"
                      :note-type 'general-pkm
                      :draft-body "* Durable idea\nBody\n"
                      :warnings
                      (list
                       (delib-flow--make-artifact-warning
                        'reference-note-reuse-justification
                        "General PKM note does not yet justify broader reuse beyond this single source item."))))
         (support (list :title "Atlas brief"
                        :support-score 8
                        :support-reasons '("title-overlap")))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Live source title"
                       :content "* Live source title\n- Durable idea\n"))
                :filing
                (list :draft-items (list candidate)
                      :approved-items nil
                      :rejected-items nil
                      :preview-text nil
                      :selection-blocked-item nil
                      :selection-blocking-warnings nil
                      :selection-blocked-selection nil
                      :selection-blocked-notes nil
                      :conflicts nil
                      :target-locations nil))
               'reference-notes
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft draft
                     :draft-history nil
                     :selected-support-candidates (list support)
                     :selected-support-context "- Atlas support context")))
         (text (delib-flow--family-evidence-review-text run 'reference-notes)))
    (should (string-match-p "Note-specific checks" text))
    (should (string-match-p "Note type: `general-pkm`" text))
    (should (string-match-p "Title check: `Durable idea` is the current durable note identity" text))
    (should (string-match-p "Evidence balance: 1 source excerpt(s) and 1 focused support item(s) are attached" text))
    (should (string-match-p "Reuse check: the broader reusable idea is still weakly justified" text))))

(ert-deftest delib-flow-file-approved-outputs-creates-new-project-from-proposal ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content
                       "* Completely Different Topic\nDraft kickoff outline for Completely Different Topic\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (integrated
            (delib-flow--run-stage-locally proposed 'integrate-into-source))
           (selected
            (delib-flow-test--set-filing-selection integrated "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (_updated-run
            (delib-flow--run-stage-locally approved 'file-approved-outputs)))
      (let ((content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
        (should (string-match-p "\\* Completely Different Topic" content))
        (should (string-match-p
                 "\\*\\* TODO Draft kickoff outline for Completely Different Topic"
                 content))
        (should (string-match-p
                 "\\* Completely Different Topic\n:PROPERTIES:\n:TAGS: .*?\n:CREATED: .*?\n:END:\n\\*\\* TODO Draft kickoff outline for Completely Different Topic"
                 content))))))

(ert-deftest delib-flow-file-approved-outputs-upgrades-legacy-project-capture-template ()
  (let ((delib-flow-project-capture-template
         "%(delib-flow-capture-project-heading)\n%(delib-flow-capture-project-tags-property)%(delib-flow-capture-project-first-item-heading)\n"))
    (delib-flow-test--with-temp-project-file
        "* Active :active:\n* Complete :complete:\n* Waiting :waiting:\n"
      (let* ((project (list :kind 'project
                            :title "Fix Steno Exercise"
                            :text "Fix Steno Exercise"
                            :state 'active
                            :child-items
                            (list (list :kind 'next-action
                                        :text "Inspect storage mechanism for steno exercises on the website"
                                        :source "local-llm"
                                        :tag-suggestions '("next_action" "broken" "steno" "exercise" "issue_note")))
                            :tags '("issue_note" "fix" "steno")
                            :warnings nil))
             (run (delib-flow--seed-filing-selection-block
                   (delib-flow--set-artifact-family-state
                    (plist-put
                     (delib-flow--initialize-run
                      (list :title "Broken steno exercise"
                            :content "* Broken steno exercise\nBody line\n"))
                     :filing
                     (list :draft-items (list project)
                           :approved-items nil
                           :rejected-items nil
                           :preview-text nil
                           :selection-blocked-item nil
                           :selection-blocking-warnings nil
                           :selection-blocked-selection nil
                           :selection-blocked-notes nil
                           :conflicts nil
                           :target-locations nil))
                    'project-proposals
                    (list :candidates (list project)
                          :selected-candidate-id
                          (delib-flow--artifact-candidate-id project)
                          :selected-draft project))))
             (selected (delib-flow-test--set-filing-selection run "1"))
             (approved (delib-flow--run-stage-locally selected 'select-approved-filing-actions))
             (_updated (delib-flow--run-stage-locally approved 'file-approved-outputs))
             (content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
        (should (string-match-p "\\*\\* Fix Steno Exercise" content))
        (should (string-match-p "\\*\\*\\* TODO Inspect storage mechanism for steno exercises on the website" content))
        (should (string-match-p ":TAGS: next_action broken steno exercise issue_note" content))
        (should (string-match-p ":SOURCE_ARTIFACT: local-llm" content))
        (should (string-match-p ":END:\n\\*\\*\\* TODO Inspect storage mechanism for steno exercises on the website\n:PROPERTIES:" content))))))

(ert-deftest delib-flow-file-approved-outputs-files-new-project-under-active-bucket ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project\n* Waiting\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nDraft kickoff outline for Completely Different Topic\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (integrated (delib-flow--run-stage-locally proposed 'integrate-into-source))
           (selected
            (delib-flow-test--set-filing-selection integrated "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (_filed
            (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (content (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
      (should (string-match-p "^\\* Active\n\\*\\* Alpha Project\n\\*\\* Completely Different Topic" content))
      (should-not (string-match-p "^\\* Completely Different Topic$" content)))))

(ert-deftest delib-flow-file-approved-outputs-promotes-filed-project-to-current-match ()
  (delib-flow-test--with-temp-project-file
      "* Active\n** Alpha Project\n* Waiting\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Completely Different Topic"
                       :content "* Completely Different Topic\nDraft kickoff outline for Completely Different Topic\n")))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (integrated (delib-flow--run-stage-locally proposed 'integrate-into-source))
           (selected
            (delib-flow-test--set-filing-selection integrated "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (filed
            (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (action-ids (mapcar (lambda (action) (plist-get action :id))
                               (delib-flow--sorted-actions filed))))
      (should (eq 'matched (delib-flow--match-status filed)))
      (should (equal "Completely Different Topic"
                     (plist-get (plist-get (delib-flow--current-project-decision filed)
                                           :best-project)
                                :title)))
      (should (member 'extract-actions action-ids))
      (should (member 'extract-waiting-for action-ids))
      (should (member 'suggest-reference-notes action-ids)))))


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

(ert-deftest delib-flow-async-stage-start-rerenders-to-current-result ()
  (let ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
        (delib-flow-local-stage-async-adapter
         (lambda (_descriptor _package _on-success _on-error)
           'fake-handle)))
    (unwind-protect
        (let* ((run (delib-flow--initialize-run
                     (list :title "Example"
                           :content "* Example\nBody line\n")))
               (delib-flow--active-run run)
               (buffer (delib-flow--render-active-run-buffer run "Now")))
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (cl-letf (((symbol-function 'delib-flow--ensure-in-flight-ui-timer) #'ignore)
                    ((symbol-function 'delib-flow--refresh-in-flight-ui) #'ignore))
            (delib-flow-action-inspect-source))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (org-back-to-heading t)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-async-stage-completion-rerenders-to-current-result ()
  (let* (success-callback
         (delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
         (delib-flow-local-stage-async-adapter
          (lambda (_descriptor _package on-success _on-error)
            (setq success-callback on-success)
            'fake-handle)))
    (unwind-protect
        (let* ((run (delib-flow--initialize-run
                     (list :title "Example"
                           :content "* Example\nBody line\n")))
               (delib-flow--active-run run)
               (buffer (delib-flow--render-active-run-buffer run "Now")))
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (cl-letf (((symbol-function 'delib-flow--ensure-in-flight-ui-timer) #'ignore)
                    ((symbol-function 'delib-flow--refresh-in-flight-ui) #'ignore))
            (delib-flow-action-inspect-source)
            (should success-callback)
            (funcall success-callback
                     (funcall #'delib-flow--default-local-stage-adapter
                              (delib-flow--stage-descriptor 'inspect-source)
                              (delib-flow--stage-input-package run 'inspect-source))))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (org-back-to-heading t)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-stage-rerender-keeps-sticky-current-result-anchor ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run (delib-flow--run-stage-locally run 'inspect-source))
         (buffer (delib-flow--render-active-run-buffer delib-flow--active-run "Now"))
         (delib-flow--sticky-anchor-heading nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow--rerender-current-result)
          (delib-flow--rerender-active-run-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (org-back-to-heading t)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (setq delib-flow--sticky-anchor-heading nil)
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-refresh-in-flight-ui-keeps-current-result-anchor ()
  (let* ((process (start-process "delib-flow-test-cat" nil "cat"))
         (run (delib-flow--mark-stage-in-flight
               (delib-flow--initialize-run
                (list :title "Example"
                      :content "* Example\nBody line\n"))
               'inspect-source
               'local
               "llama3"
               process
               "request-1"
               (current-time)))
         (delib-flow--active-run run)
         (delib-flow--sticky-anchor-heading "Current result")
         (buffer (delib-flow--render-active-run-buffer run "Now")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow--refresh-in-flight-ui)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (org-back-to-heading t)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))))
      (setq delib-flow--sticky-anchor-heading nil)
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-abort-run-cancels-in-flight-process ()
  (skip-unless (executable-find "sleep"))
  (delib-flow-test--with-temp-audit-file
    (let* ((process (make-process
                     :name "delib-flow-test-sleep"
                     :command (list "sleep" "30")
                     :noquery t))
           (run (delib-flow--mark-stage-in-flight
                 (delib-flow--initialize-run (list :title "Example"))
                 'inspect-source
                 'local
                 "llama3"
                 process
                 "request-1"
                 (current-time)))
           (delib-flow--active-run run))
      (unwind-protect
          (progn
            (delib-flow-abort-run)
            (should-not (process-live-p process))
            (should (null delib-flow--active-run)))
        (when (process-live-p process)
          (delete-process process))))))

(ert-deftest delib-flow-seed-actions-clears-stale-in-flight-state ()
  (skip-unless (executable-find "sleep"))
  (let* ((process (make-process
                   :name "delib-flow-test-stale-seed"
                   :command (list "sleep" "30")
                   :noquery t))
         (run (delib-flow--mark-stage-in-flight
               (delib-flow--initialize-run (list :title "Example"))
               'discover-reference-material
               'local
               "llama3"
               process
               "request-1"
               (current-time))))
    (unwind-protect
        (progn
          (delete-process process)
          (while (process-live-p process)
            (sleep-for 0.01))
          (let* ((updated-run (delib-flow--seed-actions run))
                 (inspect-action
                  (seq-find
                   (lambda (action)
                     (eq (plist-get action :id) 'inspect-source))
                   (plist-get (delib-flow--run-actions updated-run) :items))))
            (should-not (delib-flow--run-in-flight-p updated-run))
            (should inspect-action)
            (should-not (eq 'blocked (plist-get inspect-action :status)))
            (should-not (plist-get (delib-flow--run-session updated-run)
                                   :in-flight-stage-id))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest delib-flow-execute-local-stage-clears-stale-in-flight-state ()
  (skip-unless (executable-find "sleep"))
  (let ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
        (delib-flow-local-stage-async-adapter nil))
    (let* ((process (make-process
                     :name "delib-flow-test-stale-execute"
                     :command (list "sleep" "30")
                     :noquery t))
           (run (delib-flow--mark-stage-in-flight
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line\n"))
                 'discover-reference-material
                 'local
                 "llama3"
                 process
                 "request-1"
                 (current-time))))
      (unwind-protect
          (progn
            (delete-process process)
            (while (process-live-p process)
              (sleep-for 0.01))
            (let ((updated-run (delib-flow--execute-local-stage run 'inspect-source)))
              (should-not (delib-flow--run-in-flight-p updated-run))
              (should (eq 'inspect-source
                          (plist-get (delib-flow--latest-stage-entry updated-run)
                                     :stage-id)))))
        (when (process-live-p process)
          (delete-process process))))))

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
(ert-deftest delib-flow-reference-note-refresh-source-highlights-stores-no-change-summary ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Friction logs for AI systems"
                          :note-type 'general-pkm))
         (drafted-item
          (plist-put
           (copy-tree candidate)
           :draft-body
           (string-join
            '("* Working draft"
              "Atlas draft body."
              "- Durable claim: Capture friction logs, not just answers."
              "- Why it matters: Friction logs turn annoyances into improvements."
              "- Reuse angle: Keep this reuse angle."
              ""
              "* Source highlights"
              "- Capture friction logs, not just answers."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n")))
         (run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run
            (list :title "Source"
                  :content "* Source\nCapture friction logs, not just answers.\n"))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                 :selected-draft drafted-item)))
         (updated (delib-flow--reference-note-refresh-source-highlights run))
         (outcome (delib-flow--artifact-family-part-outcome
                   updated 'reference-notes 'source-highlights)))
    (should (equal "No meaningful change."
                   (plist-get outcome :summary)))))

(ert-deftest delib-flow-reference-note-store-part-outcome-suppresses-near-duplicate-body-churn ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (updated
          (delib-flow--reference-note-store-part-outcome
           run
           'draft-body
           "Create a named advisor with a defined role, clear scope, and personal context to give advice fitting one's life."
           "Establish a personalized AI advisor with a defined role, clear scope, and context to provide tailored advice for one's life."
           "Refreshed the working draft body for the selected note only."))
         (outcome (delib-flow--artifact-family-part-outcome
                   updated 'reference-notes 'draft-body)))
    (should (equal "No meaningful change."
                   (plist-get outcome :summary)))))

(ert-deftest delib-flow-local-test-selected-reference-note-prompts-follow-filing-selection ()
  (let* ((run (delib-flow-test--reference-note-candidate-only-run))
         (body-prompt
          (delib-flow-local-test--draft-selected-reference-note-prompt run))
         (part-prompt
          (delib-flow-local-test--draft-selected-reference-note-related-material-prompt
           run)))
    (should (string-match-p "Selected note title: Project Atlas Pattern" body-prompt))
    (should (string-match-p "Deterministic seed structure to improve:" body-prompt))
    (should (string-match-p "Selected note title: Project Atlas Pattern" part-prompt))
    (should (string-match-p "Current related material:" part-prompt))))



(ert-deftest delib-flow-reference-note-refresh-related-material-updates-only-that-section ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (support (list :title "Atlas brief"
                        :file "/tmp/atlas-brief.org"
                        :support-score 8
                        :support-reasons '("title-overlap")))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body
                                  (string-join
                                   '("* Working draft"
                                     "Summary."
                                     "- Durable claim: Claim."
                                     "- Why it matters: Matters."
                                     "- Reuse angle: Old angle."
                                     ""
                                     "* Source highlights"
                                     "- Highlight one"
                                     ""
                                     "* Related material to connect"
                                     "- Old support"
                                     ""
                                     "* Source context"
                                     "- Source: Example"
                                     ""
                                     "* Next pass"
                                     "- Distill.")
                                   "\n")))
         (run (delib-flow--set-artifact-family-state
               (delib-flow--initialize-run (list :title "Source"
                                                 :content "* Source\nBody line\n"))
               'reference-notes
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft drafted-item
                     :selected-support-candidates (list support)
                     :selected-support-context "- Atlas brief - Constraint summary")))
         (updated (delib-flow--reference-note-refresh-related-material run))
         (body (plist-get (delib-flow--artifact-family-selected-draft updated 'reference-notes)
                          :draft-body)))
    (should (string-match-p "Summary\\." body))
    (should (string-match-p "- Highlight one" body))
    (should (string-match-p "\\* Related material to connect\n- Atlas brief" body))
    (should-not (string-match-p "- Old support" body))))

(ert-deftest delib-flow-format-source-evidence-excerpt-skips-preview-junk ()
  (let* ((package (list :source
                        (list :title "Source"
                              :content
                              (concat
                               "* Source\n"
                               "Preview:\n"
                               "Building personal AI advisors by defining a named advisor.\n"
                               "View in browser\n"))
                        :working-context nil))
         (excerpt
          (delib-flow--format-source-evidence-excerpt
           package
           "Building personal AI advisors by defining a named advisor")))
    (should (string-match-p "Building personal AI advisors by defining a named advisor" excerpt))
    (should-not (string-match-p "Preview:" excerpt))))
