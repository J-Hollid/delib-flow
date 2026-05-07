;;; delib-flow-render-test.el --- Render tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-test-support)

(ert-deftest delib-flow-render-managed-surface-descriptor-errors-for-unknown-surface ()
  (should-error (delib-flow--managed-surface-descriptor 'missing-surface)
                :type 'error))

(ert-deftest delib-flow-render-managed-surface-descriptor-errors-for-missing-required-key ()
  (let ((delib-flow--managed-surface-descriptor-alist
         '((broken-surface
            :buffer-name " *broken*"
            :mode delib-flow-control-mode
            :title-prefix "Broken"))))
    (should-error (delib-flow--managed-surface-descriptor 'broken-surface)
                  :type 'error)))

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
          (should (eq 'special (get 'delib-flow-control-mode 'mode-class)))
          (should-not view-mode)
          (should (eq #'delib-flow--revert-control-buffer revert-buffer-function))
          (should (equal '(:eval (delib-flow--control-header-line)) header-line-format))
          (should (string-match-p "Delib-Flow cockpit" (delib-flow--control-header-line)))
          (should-not truncate-lines)
          (should word-wrap)
          (should (search-forward "* DeliberateFlow -- Example" nil t))
          (let ((positions
                 (mapcar (lambda (heading)
                           (goto-char (point-min))
                           (search-forward heading nil t))
                         '("** Now"
                           "** Current result"
                           "** Filing preview"
                           "** Next actions"
                           "** Current context"
                           "** Details"))))
            (should (seq-every-p #'identity positions))
            (should (apply #'< positions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-control-buffer-renders-structured-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "- Inspect Source [available]" nil t))
          (should (search-forward "Hotkey: `1`." nil t))
          (should (search-forward "Review the source snapshot and propose structured context." nil t))
          (goto-char (line-beginning-position))
          (should (eq 'inspect-source
                      (plist-get (get-text-property (point) 'delib-flow-action)
                                 :id)))
          (goto-char (point-min))
          (should (search-forward "- Recommended next step: Inspect Source" nil t))
          (goto-char (point-min))
          (should (search-forward "- Refresh Buffer [available]" nil t))
          (goto-char (point-min))
          (should (search-forward "- Abort Run [available]" nil t))
          (goto-char (point-min))
          (should (search-forward "*** Decision strip" nil t))
          (goto-char (point-min))
          (should (search-forward "*** Loop update" nil t))
          (goto-char (point-min))
          (should (search-forward "*** Recommended next pass" nil t))
          (should (search-forward "*** Quick actions" nil t))
          (should (search-forward "*** Recovery snapshot" nil t))
          (should (search-forward "*** Resume guide" nil t))
          (goto-char (point-min))
          (should (search-forward "- Why:" nil t))
          (should (search-forward "- Unblock path:" nil t))
          (goto-char (point-min))
          (should (search-forward "- Jump back: `L` active loop, `K` latest preview" nil t))
          (goto-char (point-min))
          (should (search-forward "- Recovery path: `L` resume loop, `K` consequence preview, `U` stage history, `J` audit stage" nil t))
          (goto-char (point-min))
          (should (search-forward "*** Control keys" nil t))
          (should (search-forward "- `RET/a`: Run the action at point." nil t))
          (should (search-forward "- `E`: Show the selected draft in the consequence pane." nil t))
          (should (search-forward "- `I`: Show focused support for the selected artifact in the consequence pane." nil t))
          (should (search-forward "- `L`: Jump back to the active decision loop." nil t))
          (should (search-forward "- `K`: Jump to the latest consequence preview or result." nil t))
          (should (search-forward "- `U`: Jump to the latest stage history details." nil t))
          (should (search-forward "- `z`: Toggle narrow-screen focus mode for the active decision loop." nil t))
          (goto-char (point-min))
          (should-not (search-forward "{id:" nil t))
          (goto-char (point-min))
          (should-not (search-forward "cmd:" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

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

(ert-deftest delib-flow-render-control-buffer-renders-editable-blocks ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "** Operator intent" nil t))
          (should (search-forward "strongly steers source interpretation and project drafting" nil t))
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

(ert-deftest delib-flow-render-control-buffer-keeps-editable-block-bodies-read-only ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "#+begin_delib-edit context")
          (forward-line 1)
          (should-error (insert "blocked")))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-summary-field-texts-stay-consistent-across-now-surfaces ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (entry (list :stage-id 'inspect-source
                      :label "Inspect Source"
                      :status 'succeeded
                      :review-state 'pending-review)))
    (setq run (plist-put run :stage-history (list :entries (list entry))))
    (setq run (plist-put run :actions
                         (list :items
                               (list (delib-flow--make-action
                                      'inspect-source
                                      "Inspect Source"
                                      'blocked
                                      "waiting on review"
                                      #'ignore
                                      10)))))
    (setq run (plist-put run :session
                         (plist-put (delib-flow--run-session run)
                                    :current-decision
                                    "Review the inspect result before proceeding.")))
    (should (equal (delib-flow--summary-field-text run :latest-change)
                   "Inspect Source (succeeded, pending-review)"))
    (should (equal (delib-flow--summary-field-text run :blocked)
                   "Inspect Source (waiting on review)"))
    (should (equal (delib-flow--summary-field-text run :current-decision)
                   "Review the inspect result before proceeding."))
    (should (string-match-p
             (regexp-quote "Inspect Source (succeeded, pending-review)")
             (delib-flow--resume-guide-text run)))
    (should (string-match-p
             (regexp-quote "Review the inspect result before proceeding.")
             (delib-flow--recovery-snapshot-text run)))))

(ert-deftest delib-flow-decision-strip-renders-blocked-action-reason-and-stage-review-state ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (entry (list :stage-id 'inspect-source
                      :label "Inspect Source"
                      :status 'succeeded
                      :review-state 'pending-review)))
    (setq run (plist-put run :stage-history (list :entries (list entry))))
    (setq run (plist-put run :actions
                         (list :items
                               (list (delib-flow--make-action
                                      'inspect-source
                                      "Inspect Source"
                                      'blocked
                                      "waiting on review"
                                      #'ignore
                                      10)
                                     (delib-flow--make-action
                                      'match-project
                                      "Match Project"
                                      'available
                                      nil
                                      #'ignore
                                      20)))))
    (let ((text (delib-flow--decision-strip-text run)))
      (should (string-match-p
               (regexp-quote "- Current stage: Inspect Source (succeeded, pending-review)")
               text))
      (should (string-match-p
               (regexp-quote "- Blocked: Inspect Source (waiting on review)")
               text)))))

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

(ert-deftest delib-flow-inspect-result-pairs-include-entities ()
  (let* ((output '(:source-type email
                   :source-type-reason "Headers present"
                   :source-type-signals ("From:" "Subject:")
                   :title "Example"
                   :outline-path ("Example")
                   :body-line-count 3
                   :content-word-count 10
                   :contact-emails ("alice@example.com")
                   :org-file-link-count 1
                   :body-preview "Preview"
                   :analysis (:entities ("Bob")
                              :summary "Summary text")))
         (pairs (delib-flow--inspect-result-pairs output))
         (context-pairs (delib-flow--inspect-context-pairs
                         '(:title "Example" :file "/tmp/example.org")
                         output)))
    (should (equal "Bob" (cdr (assoc "Entities" pairs))))
    (should (equal "Bob" (cdr (assoc "Entities" context-pairs))))))

(ert-deftest delib-flow-inspect-output-entities-text-falls-back-to-none ()
  (should (equal "none"
                 (delib-flow--inspect-output-entities-text
                  '(:analysis nil)))))

(ert-deftest delib-flow-current-result-renders-drafted-selected-reference-note-as-fixed-width-preview ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-candidates
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
                'reference-notes
                (list candidate))))
         (run (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-selection run "1")
               'draft-selected-reference-note))
         (text (delib-flow--current-result-text run)))
    (should (string-match-p "Selected note draft" text))
    (should (string-match-p "Result: this is the current drafted body for the selected note only" text))
    (should (string-match-p ": \\* Working draft" text))
    (should (string-match-p ": Example" text))))

(ert-deftest delib-flow-current-result-renders-loop-update ()
  (let* ((run (delib-flow--run-stage-locally
               (delib-flow--initialize-run (list :title "Example"))
               'inspect-source))
         (text (delib-flow--section-content "Current result" run)))
    (should (string-match-p "\\*\\*\\* Loop update" text))
    (should (string-match-p "Local consequence: this source classification now controls the next project-matching pass." text))))

(ert-deftest delib-flow-suggest-reference-notes-current-result-renders-draft-note-preview ()
  (let* ((source-content
          (concat
           "* Your Consumption Diet Is Your Moat :email:\n"
           ":PROPERTIES:\n"
           ":FROM: Forte Labs Newsletter <hello@fortelabs.com>\n"
           ":END:\n\n"
           ":RAW_EMAIL:\n"
           "From: Forte Labs Newsletter <hello@fortelabs.com>\n"
           "Subject: Your Consumption Diet Is Your Moat\n\n"
           "Week 1 was about the Master Prompt, PARA adapted for the AI era, and a capture system.\n"
           "This week focused on building personal AI advisors.\n"
           ":END:\n"))
         (run (delib-flow--initialize-run
               (list :title "Your Consumption Diet Is Your Moat"
                     :content source-content)))
         (inspected (delib-flow-test--accept-inspect
                     (delib-flow--run-stage-locally run 'inspect-source)))
         (drafted (delib-flow--run-stage-locally inspected 'suggest-reference-notes))
         (text (delib-flow--current-result-text drafted)))
    (should (string-match-p "\\*\\*\\* Draft note previews" text))
    (should (string-match-p "Review these draft note bodies before moving into filing" text))
    (should (string-match-p "\\* Working draft" text))
    (should (string-match-p "\\* Source highlights" text))))

(ert-deftest delib-flow-compact-summary-uses-ascii-truncation-marker ()
  (should (equal "Create general PKM note..."
                 (delib-flow--compact-summary
                  "Create general PKM note for a very long source-derived concept"
                  26))))

(ert-deftest delib-flow-audit-status-renders-archive-availability ()
  (delib-flow-test--with-temp-directory-var archive-dir "delib-flow-audit-archive"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Example"
                       :content "* Example\nBody line\n")))
           (delib-flow-audit-archive-directory archive-dir)
           (delib-flow--active-run nil)
           (text (delib-flow--audit-run-state-text run)))
      (should (string-match-p "Audit archive directory:" text))
      (should (string-match-p "Save archived run: not available" text))
      (let ((delib-flow--active-run run))
        (setq text (delib-flow--audit-run-state-text run))
        (should (string-match-p "Save archived run: available" text))))))

(ert-deftest delib-flow-details-section-renders-working-context-and-audit-subsections ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (text (delib-flow--render-details-section run)))
    (should (string-match-p
             (regexp-quote "*** Accepted working context")
             text))
    (should (string-match-p
             (regexp-quote "**** Cloud routing review")
             text))
    (should (string-match-p
             (regexp-quote "*** Audit status")
             text))
    (should (string-match-p
             (regexp-quote "**** Audit navigation")
             text))))

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

(ert-deftest delib-flow-rerouted-cloud-current-result-foregrounds-target-stage ()
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
         (delib-flow--active-run
          (delib-flow--run-stage-in-cloud approved 'run-cloud-stage))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "- Latest stage: Run Cloud Stage (completed)" nil t))
          (goto-char (point-min))
          (should (search-forward "- Operator focus stage: Extract Actions (completed)" nil t))
          (goto-char (point-min))
          (should (search-forward "- Stage: Extract Actions" nil t))
          (goto-char (point-min))
          (should (search-forward "- Transport stage: Run Cloud Stage" nil t))
          (goto-char (point-min))
          (should (search-forward "- Retry Extract Actions In Cloud [available]" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-filing-preview-prioritizes-actions-and-targets ()
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
           (text (delib-flow--render-filing-preview-section selected)))
      (should (< (string-match-p "\\*\\*\\* What to do next" text)
                 (string-match-p "\\*\\*\\* Current filing plan" text)))
      (should (< (string-match-p "\\*\\*\\* Current filing plan" text)
                 (string-match-p "\\*\\*\\* Filing actions" text)))
      (should (< (string-match-p "\\*\\*\\* Selected action workspace" text)
                 (string-match-p "\\*\\*\\* Filing actions" text)))
      (should (< (string-match-p "\\*\\*\\* Filing actions" text)
                 (string-match-p "\\*\\*\\* Staged content preview" text)))
      (should (string-match-p "Consequence pane: `E` draft, `I` support, `P` target, `V` staged" text))
      (should (string-match-p "- If blocked:" text)))))

(ert-deftest delib-flow-render-filing-preview-renders-selected-action-workflow ()
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
            (delib-flow-test--set-filing-selection integrated "1"))
           (text (delib-flow--render-filing-preview-section selected-run)))
      (should (string-match-p "\\*\\*\\* Selected action workspace" text))
      (should (string-match-p
               "One action is selected; draft it next if you want wording help before approval"
               text))
      (should (string-match-p
               "This selected action is the active candidate for this workspace"
               text))
      (should (string-match-p
               "Workspace state: this selected action is still the raw selected candidate"
               text))
      (should (string-match-p
               "Run `Draft Selected Action` to tighten this selected action before approval"
               text))
      (should-not (string-match-p "queue item your current Selection points to" text))
      (should (string-match-p "Draft Selected Action \\[available\\]" text))
      (should (< (string-match-p "\\*\\*\\* Selected action workspace" text)
                 (string-match-p "\\*\\*\\* Filing actions" text))))))

(ert-deftest delib-flow-render-filing-preview-renders-selected-waiting-for-workflow ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nWaiting for Pat to confirm launch date\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-waiting-for))
           (integrated
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected-run
            (delib-flow-test--set-filing-selection integrated "1"))
           (text (delib-flow--render-filing-preview-section selected-run)))
      (should (string-match-p "\\*\\*\\* Selected waiting-for workspace" text))
      (should (string-match-p
               "One waiting-for is selected; draft it next if you want clearer owner or blocker wording before approval"
               text))
      (should (string-match-p
               "This selected waiting-for is the active candidate for this workspace"
               text))
      (should-not (string-match-p "queue item your current Selection points to" text))
      (should (string-match-p
               "Draft Selected Waiting-For \\[available\\]"
               text))
      (should (< (string-match-p "\\*\\*\\* Selected waiting-for workspace" text)
                 (string-match-p "\\*\\*\\* Filing actions" text))))))

(ert-deftest delib-flow-render-filing-preview-renders-selected-project-workflow ()
  (let* ((project (list :kind 'project
                        :title "Project Atlas"
                        :text "Project Atlas"
                        :state 'active
                        :first-item (list :kind 'next-action
                                          :text "Investigate and fix Project Atlas launch breakage")
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
                (list project))))
         (selected-run (delib-flow-test--set-filing-selection run "1"))
         (text (delib-flow--render-filing-preview-section selected-run)))
    (should (string-match-p "\\*\\*\\* Project workspace" text))
    (should (string-match-p "\\*\\* Project package" text))
    (should (string-match-p "Project: Project Atlas" text))
    (should (string-match-p "\\*\\* Do here now" text))
    (should-not (string-match-p "Focused filing workspace" text))))

(ert-deftest delib-flow-project-focused-filing-workspace-surfaces-local-numbered-actions ()
  (let* ((project (list :kind 'project
                        :title "Project Atlas"
                        :text "Project Atlas"
                        :state 'active
                        :child-items
                        (list (list :kind 'next-action
                                    :text "Draft launch outline for Project Atlas"))
                        :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line\n"))
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
         (text (with-current-buffer
                   (delib-flow--render-focused-filing-workspace-buffer selected)
                 (buffer-string))))
    (should (string-match-p "\\*\\* Project package" text))
    (should (string-match-p "Package consequence" text))
    (should (string-match-p "\\*\\* Do here now" text))
    (should (string-match-p "Edit Operator Intent" text))
    (should (string-match-p "Extract Actions" text))
    (should (string-match-p "Approve Package" text))
    (should-not (string-match-p "- \\[[0-9]+\\] File Package" text))))

(ert-deftest delib-flow-render-filing-preview-prioritizes-project-package-before-note-workspace ()
  (let* ((project (list :kind 'project
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
         (note (list :kind 'reference-note
                     :text "Create general PKM note for Team Ritual Prompt"
                     :note-type 'general-pkm
                     :draft-stage 'suggest-reference-notes))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line\n"))
                 :filing
                 (list :draft-items (list project note)
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
                      :selected-draft nil))))
         (text (delib-flow--render-filing-preview-section run)))
    (should (string-match-p "Project workspace" text))
    (should (string-match-p "Project: Project Atlas" text))
    (should (string-match-p "Included child items: 2" text))
    (should (string-match-p "Extracted but not yet included" text))
    (should-not (string-match-p "Focused filing workspace" text))))

(ert-deftest delib-flow-render-filing-preview-guides-drafted-project-toward-extraction ()
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
           (text (with-current-buffer
                     (delib-flow--render-focused-filing-workspace-buffer drafted)
                   (buffer-string))))
      (should (string-match-p "The drafted project package is the current source of truth" text))
      (should (string-match-p "Primary next action: Extract Actions / Waiting / Notes" text))
      (should (string-match-p "\\[now\\] 3 Extract next actions, waiting-fors, and reference notes" text))
      (should (string-match-p "Primary action now: `Extract Actions / Waiting / Notes`" text))
      (should (string-match-p "\\[1\\] Extract Actions" text))
      (should (string-match-p "\\[2\\] Extract Waiting-For" text))
      (should (string-match-p "\\[3\\] Suggest Reference Notes" text))
      (should (string-match-p "Regenerate Selected Project" text))
      (should-not (string-match-p "Retry Propose New Project" text)))))

(ert-deftest delib-flow-render-active-run-buffer-aligns-anchor-heading-to-top ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (buffer (delib-flow--render-active-run-buffer run "Current result")))
    (unwind-protect
        (let ((window (display-buffer buffer)))
          (with-current-buffer buffer
            (goto-char (point-min))
            (delib-flow--render-active-run-buffer run "Current result")
            (should (delib-flow--goto-section "Current result"))
            (let ((expected-start
                   (save-excursion
                     (org-back-to-heading t)
                     (line-beginning-position))))
              (should (= (window-start window) expected-start)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-render-sections-surface-dedicated-editor-hints ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Broken steno exercise"
                     :content "* Broken steno exercise\nhttps://example.com/drill?id=one\n")))
         (context-text (delib-flow--section-content "Current context" run))
         (now-text (delib-flow--section-content "Now" run)))
    (should (string-match-p "This text strongly steers source interpretation and project drafting" context-text))
    (should (string-match-p "Current value: none" context-text))
    (should (string-match-p "Saved operator intent steers future stage runs only" context-text))
    (should (string-match-p "Use the rendered `Edit` action or run `M-x delib-flow-action-edit-operator-intent`" context-text))
    (should (string-match-p "M-x delib-flow-action-edit-operator-intent" context-text))
    (should (string-match-p "M-x delib-flow-action-edit-operator-notes" now-text))))

(ert-deftest delib-flow-render-clean-source-buffer-falls-back-to-source-lines ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Plain source"
                     :content "* Plain source\nPreview:\nUseful line\nView in browser\n")))
         (buffer (delib-flow--render-clean-source-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'delib-flow-source-view-mode))
          (should (eq delib-flow--surface-kind 'source-view))
          (should (equal delib-flow--active-run run))
          (should (string-match-p "Delib-Flow clean source" header-line-format))
          (goto-char (point-min))
          (should (search-forward "- Type: unknown" nil t))
          (goto-char (point-min))
          (should (search-forward "- Contacts: none" nil t))
          (goto-char (point-min))
          (should-not (search-forward "Preview:" nil t))
          (goto-char (point-min))
          (should-not (search-forward "No cleaned source lines are available." nil t))
          (should (eq #'quit-window
                      (key-binding (kbd "q")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-render-wireframe-project-workspace-orders-project-target-and-actions ()
  (let* ((project (list :kind 'project
                        :title "Project Atlas"
                        :text "Project Atlas"
                        :state 'active
                        :child-items
                        (list (list :kind 'next-action
                                    :text "Draft launch outline for Project Atlas"))
                        :warnings nil))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Example"
                        :content "* Example\nBody line\n"))
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
         (text (with-current-buffer
                   (delib-flow--render-focused-filing-workspace-buffer selected)
                 (buffer-string))))
    (should (< (string-match-p "Project: Project Atlas" text)
               (string-match-p "\\*\\* Do here now" text)))
    (should (< (string-match-p "\\*\\* Targets and staged output" text)
               (string-match-p "\\*\\*\\* Do here: file package" text)))
    (should (< (string-match-p "File destination:" text)
               (string-match-p "\\*\\*\\* Do here: file package" text)))))

(provide 'delib-flow-render-test)
;;; delib-flow-render-test.el ends here
