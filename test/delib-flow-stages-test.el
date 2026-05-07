;;; delib-flow-stages-test.el --- Stage tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-test-support)

(ert-deftest delib-flow-stage-command-metadata-errors-for-unknown-stage ()
  (should-error (delib-flow--stage-command-runner-kind 'missing-stage)
                :type 'error))

(ert-deftest delib-flow-stage-command-metadata-errors-for-missing-required-key ()
  (let ((delib-flow--stage-descriptor-alist
         '((broken-stage
            :id broken-stage
            :label "Broken"
            :command-runner-kind run-stage-locally
            :command-sync control-buffer))))
    (should-error (delib-flow--stage-command-rerender 'broken-stage)
                  :type 'error)))

(ert-deftest delib-flow-stage-input-package-resolves-prompt-library-entry ()
  (delib-flow-test--with-temp-file-var prompt-file "delib-flow-prompts" ".org"
      "* Inspect Source\n:PROPERTIES:\n:PROMPT_ID: milestone2-inspect-source\n:END:\nPrompt body text.\n"
    (delib-flow-test--with-temp-file-var example-file "delib-flow-examples" ".org"
        "#+title: Example structures\n- Example output shape\n"
      (let* ((delib-flow-prompt-library-file prompt-file)
             (delib-flow-example-structures-file example-file)
             (run (delib-flow--initialize-run (list :title "Example")))
             (package (delib-flow--stage-input-package run 'inspect-source))
             (prompt (plist-get package :prompt))
             (guidance (plist-get prompt :structured-guidance)))
        (should (eq 'resolved (plist-get prompt :status)))
        (should (equal "Prompt body text."
                       (plist-get prompt :template-text)))
        (should (string-match-p "Stage: Inspect Source"
                                (plist-get prompt :rendered-text)))
        (should (string-match-p "Prompt template:\nPrompt body text."
                                (plist-get prompt :rendered-text)))
        (should (eq 'available
                    (plist-get prompt :example-structures-status)))
        (should (string-match-p "Example output shape"
                                (plist-get prompt :example-structures-text)))
        (should (string-match-p
                 (regexp-quote "Example structures:\n#+title: Example structures")
                 (plist-get prompt :rendered-text)))
        (should (eq 'source-analysis
                    (plist-get guidance :stage-family)))
        (should (member "Summary"
                        (plist-get guidance :response-schema)))))))

(ert-deftest delib-flow-stage-input-package-falls-back-to-descriptor-only-prompt ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (package (delib-flow--stage-input-package run 'inspect-source))
         (prompt (plist-get package :prompt)))
    (should (eq 'descriptor-only (plist-get prompt :status)))
    (should-not (plist-get prompt :template-text))
    (should (string-match-p "Stage: Inspect Source"
                            (plist-get prompt :rendered-text)))
    (should (eq 'not-available
                (plist-get prompt :example-structures-status)))))

(ert-deftest delib-flow-stage-input-package-adds-project-checklist-guidance ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (package (delib-flow--stage-input-package run 'propose-new-project))
         (guidance (plist-get (plist-get package :prompt)
                              :structured-guidance)))
    (should (eq 'project-creation
                (plist-get guidance :stage-family)))
    (should (member "Project title"
                    (plist-get guidance :required-checklist)))
    (should (member "Project summary"
                    (plist-get guidance :optional-checklist)))))

(ert-deftest delib-flow-stage-input-package-adds-artifact-guidance ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (package (delib-flow--stage-input-package run 'extract-actions))
         (guidance (plist-get (plist-get package :prompt)
                              :structured-guidance)))
    (should (eq 'next-action
                (plist-get guidance :artifact-kind)))
    (should (member "Concrete next step or deliverable"
                    (plist-get guidance :quality-rules)))))

(ert-deftest delib-flow-stage-input-package-surfaces-operator-intent ()
  (let* ((run (delib-flow--set-operator-intent-directly
               (delib-flow--initialize-run
                (list :title "Broken steno exercise"
                      :content "* Broken steno exercise\nhttps://example.com/drill?id=one\n"))
               "this is about fixing a broken website"))
         (package (delib-flow--stage-input-package run 'propose-new-project)))
    (should (equal "this is about fixing a broken website"
                   (plist-get (plist-get package :ui) :operator-intent)))))

(ert-deftest delib-flow-cloud-stage-result-reroutes-selected-stage-output ()
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
         (approved-send
          (delib-flow--run-stage-locally sanitized 'approve-cloud-send))
         (package (delib-flow--stage-input-package approved-send 'run-cloud-stage))
         (raw (delib-flow--cloud-stage-result package)))
    (should (eq 'extract-actions (plist-get raw :target-stage)))
    (should (plist-get raw :target-stage-raw-output))
    (should (plist-get raw :target-stage-normalized-output))
    (should (eq 'returned (plist-get raw :sanitization-status)))
    (should (string-match-p "Cloud output for rerouted stage Extract Actions"
                            (plist-get raw :cloud-output)))
    (should (string-match-p "Proposed stage result:"
                            (plist-get raw :cloud-output)))))

(ert-deftest delib-flow-resolve-cloud-failure-retry-clears-failure-and-restores-cloud-action ()
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
         (failed-run (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (resolved-run
          (delib-flow--run-stage-locally
           (delib-flow-test--set-cloud-failure-resolution
            failed-run "RETRY-CLOUD" "retry it")
           'resolve-cloud-failure))
         (routing (plist-get resolved-run :routing))
         (actions (mapcar (lambda (action)
                            (plist-get action :id))
                          (plist-get (delib-flow--run-actions resolved-run)
                                     :items))))
    (should-not (plist-get routing :cloud-failure-stage))
    (should-not (plist-get routing :cloud-failure-message))
    (should-not (plist-get routing :cloud-fallback-mode))
    (should-not (plist-get routing :reintegration-status))
    (should (member 'run-cloud-stage actions))
    (should-not (member 'resolve-cloud-failure actions))))

(ert-deftest delib-flow-resolve-cloud-failure-use-local-enables-integration ()
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
         (failed-run (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (resolved-run
          (delib-flow--run-stage-locally
           (delib-flow-test--set-cloud-failure-resolution
            failed-run "USE-LOCAL" "continue locally")
           'resolve-cloud-failure))
         (routing (plist-get resolved-run :routing))
         (actions (mapcar (lambda (action)
                            (plist-get action :id))
                          (plist-get (delib-flow--run-actions resolved-run)
                                     :items))))
    (should (eq 'use-local (plist-get routing :cloud-fallback-mode)))
    (should (eq 'approved (plist-get routing :reintegration-status)))
    (should-not (plist-get routing :cloud-failure-stage))
    (should (member 'integrate-into-source actions))
    (should-not (member 'approve-candidate-reintegration actions))))

(ert-deftest delib-flow-resolve-cloud-failure-skip-cloud-enables-integration ()
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
         (failed-run (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (resolved-run
          (delib-flow--run-stage-locally
           (delib-flow-test--set-cloud-failure-resolution
            failed-run "SKIP-CLOUD" "skip it")
           'resolve-cloud-failure))
         (routing (plist-get resolved-run :routing)))
    (should (eq 'skip-cloud (plist-get routing :cloud-fallback-mode)))
    (should (eq 'approved (plist-get routing :reintegration-status)))
    (should-not (plist-get routing :cloud-failure-stage))))

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
         (updated-run (delib-flow--run-stage-in-cloud approved 'run-cloud-stage))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (working (delib-flow--run-working-context updated-run))
         (routing (plist-get updated-run :routing))
         (cloud-output (plist-get working :cloud-returned-context)))
    (should (equal 'run-cloud-stage (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'run-cloud-stage (plist-get entry :stage-id)))
    (should cloud-output)
    (should (string-match-p "Cloud output for rerouted stage Run Cloud Stage" cloud-output))
    (should (eq 'returned (plist-get routing :sanitization-status)))
    (should (eq 'pending-review (plist-get routing :reintegration-status)))
    (should-not (plist-get routing :cloud-switch-pending))
    (should (string-match-p "Review cloud-returned result"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-run-cloud-stage-records-rerouted-cloud-history-entry ()
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
         (entries (plist-get (delib-flow--run-stage-history updated-run) :entries))
         (shadow-entry
          (seq-find (lambda (item)
                      (and (eq 'extract-actions (plist-get item :stage-id))
                           (plist-get item :cloud-shadow-p)))
                    entries)))
    (should shadow-entry)
    (should (equal 'cloud (plist-get shadow-entry :provider)))
    (should (equal 'completed (plist-get shadow-entry :status)))
    (should (equal 'pending-review (plist-get shadow-entry :review-state)))
    (should-not (plist-get shadow-entry :applied-p))
    (should (string-match-p "Candidate count:"
                            (plist-get shadow-entry :normalized-output)))
    (should-not (delib-flow--stage-executed-p updated-run 'extract-actions))))

(ert-deftest delib-flow-run-cloud-stage-failure-records-recovery-state ()
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
         (updated-run (delib-flow--run-stage-in-cloud approved-send 'run-cloud-stage))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (routing (plist-get updated-run :routing))
         (working (delib-flow--run-working-context updated-run))
         (actions (mapcar (lambda (action)
                            (plist-get action :id))
                          (plist-get (delib-flow--run-actions updated-run)
                                     :items))))
    (should (equal 'failed (plist-get history :latest-status)))
    (should (equal 'run-cloud-stage (plist-get history :latest-stage)))
    (should (equal 'run-cloud-stage (plist-get entry :stage-id)))
    (should (equal 'run-cloud-stage (plist-get routing :cloud-failure-stage)))
    (should (string-match-p "cloud timeout"
                            (plist-get routing :cloud-failure-message)))
    (should-not (plist-get working :cloud-returned-context))
    (should-not (member 'run-cloud-stage actions))
    (should (member 'resolve-cloud-failure actions))
    (should (string-match-p "Review cloud failure"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-stage-input-package-prefers-saved-operator-intent-state ()
  (let* ((run (delib-flow--set-operator-intent-directly
               (delib-flow--initialize-run
                (list :title "Broken steno exercise"
                      :content "* Broken steno exercise\nhttps://example.com/drill?id=one\n"))
               "this is about fixing a broken website"))
         (stomped
          (delib-flow--set-editable-block-text-directly run 'context-main ""))
         (package (delib-flow--stage-input-package stomped 'extract-actions)))
    (should (equal "this is about fixing a broken website"
                   (plist-get (plist-get package :ui) :operator-intent)))))

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

(ert-deftest delib-flow-apply-inspect-source-entry-filters-noisy-entities ()
  (let* ((run (delib-flow--initialize-run (list :title "Example"
                                                :content "* Example\nBody\n")))
         (entry (list :stage-id 'inspect-source
                      :raw-output
                      (list :source-type 'email
                            :source-type-reason "Headers present"
                            :source-type-signals '("From:" "Subject:")
                            :title "Example"
                            :outline-path '("Inbox")
                            :contact-emails '("hello@example.com")
                            :contact-email-count 1
                            :org-file-links nil
                            :org-file-link-count 0
                            :summary "Useful summary."
                            :analysis
                            (list :entities '("Join" "It" "Christiania" "j-holliday" "Give")
                                  :blockers nil))))
         (updated (delib-flow--apply-inspect-source-entry run entry))
         (entities (plist-get
                    (plist-get
                     (plist-get (delib-flow--run-working-context updated)
                                :inspect-output)
                     :analysis)
                    :entities)))
    (should (equal '("Christiania") entities))))

(ert-deftest delib-flow-apply-manual-project-match-entry-supersedes-accepted-match-review ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (working (delib-flow--run-working-context run))
         (run (plist-put
               run :working-context
               (plist-put
                (plist-put working :project-match
                           (list :match-status 'match
                                 :matched-project-title "Old Project"))
                :review-results
                (list (cons 'match-project
                            (list :candidate-review-state 'accepted
                                  :accepted-output (list :match-status 'match)
                                  :accepted-normalized-output "Accepted"))))))
         (entry (list :stage-id 'manual-project-match
                      :raw-output
                      (list :match-status 'manual
                            :matched-project-title "Manual Project")))
         (updated (delib-flow--apply-manual-project-match-entry run entry))
         (review (delib-flow--stage-review-state updated 'match-project)))
    (should (eq 'superseded review))
    (should (equal "Manual Project"
                   (plist-get (plist-get (delib-flow--run-working-context updated)
                                         :project-match)
                              :matched-project-title)))))

(ert-deftest delib-flow-apply-discovery-entry-stores-retrieved-candidates ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (entry (list :stage-id 'discover-reference-material
                      :raw-output
                      (list :candidates
                            (list (list :title "Alpha brief")
                                  (list :title "Beta brief")))))
         (updated (delib-flow--apply-discovery-entry run entry))
         (retrieved (plist-get (delib-flow--run-working-context updated)
                               :retrieved-candidates)))
    (should (equal '("Alpha brief" "Beta brief")
                   (mapcar (lambda (candidate)
                             (plist-get candidate :title))
                           retrieved)))))

(ert-deftest delib-flow-apply-filter-entry-prefers-raw-retained-context ()
  (let* ((run (list :working-context (list :filtered-context nil :retained-context nil)))
         (entry (list :raw-output
                      (list :retained-candidates
                            (list (list :title "Beta brief"))
                            :retained-context
                            "- Beta brief: reconcile blockers before next step.")))
         (updated (delib-flow--apply-filter-entry run entry)))
    (should (equal "- Beta brief: reconcile blockers before next step."
                   (plist-get (plist-get updated :working-context)
                              :retained-context)))))

(ert-deftest delib-flow-apply-suggest-reference-notes-clears-stale-filed-locations ()
  (let* ((run
          (list :filing
                (list :draft-items
                      (list (list :kind 'waiting-for
                                  :text "Waiting on Bob"))
                      :approved-items nil
                      :rejected-items nil
                      :target-locations
                      (list (list :kind 'waiting-for
                                  :item-text "Waiting on Bob"
                                  :target "/tmp/projects.org::Alpha")))))
         (entry
          (list :raw-output
                (list :reference-notes
                      (list (list :kind 'reference-note
                                  :text "Create general PKM note for Create project timeline"
                                  :note-type 'general-pkm)))))
         (updated (delib-flow--apply-suggest-reference-notes-entry run entry))
         (filing (plist-get updated :filing)))
    (should-not (plist-get filing :target-locations))
    (should (seq-some
             (lambda (item)
               (equal "Create general PKM note for Create project timeline"
                      (plist-get item :text)))
             (plist-get filing :draft-items)))))

(ert-deftest delib-flow-apply-extract-actions-entry-seeds-artifact-candidates ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (run (delib-flow--set-artifact-family-state
               run 'actions
               (list :candidates nil
                     :selected-candidate-id "old-action"
                     :selected-draft (list :text "Old draft"))))
         (entry (list :raw-output
                      (list :actions
                            (list (list :kind 'next-action
                                        :text "Publish timeline")))))
         (updated (delib-flow--apply-extract-actions-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'actions)))
    (should (equal 1 (length (plist-get state :candidates))))
    (should (equal "Publish timeline"
                   (plist-get (car (plist-get state :candidates)) :text)))
    (should-not (plist-get state :selected-candidate-id))
    (should-not (plist-get state :selected-draft))))

(ert-deftest delib-flow-apply-suggest-reference-notes-entry-seeds-artifact-candidates ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (run (delib-flow--set-artifact-family-state
               run 'reference-notes
               (list :candidates nil
                     :selected-candidate-id "old-note"
                     :selected-draft (list :text "Old note draft"))))
         (entry (list :stage-id 'suggest-reference-notes
                      :raw-output
                      (list :reference-notes
                            (list (list :kind 'reference-note
                                        :text "Create general PKM note for Project Atlas Pattern"
                                        :note-type 'general-pkm)))))
         (updated (delib-flow--apply-suggest-reference-notes-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'reference-notes)))
    (should (equal 1 (length (plist-get state :candidates))))
    (should (equal "Create general PKM note for Project Atlas Pattern"
                   (plist-get (car (plist-get state :candidates)) :text)))
    (should-not (plist-get state :selected-candidate-id))
    (should-not (plist-get state :selected-draft))))

(ert-deftest delib-flow-apply-draft-selected-reference-note-entry-stores-selected-draft ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body
                                  "* Working draft\nAtlas draft body.\n"))
         (run (delib-flow--set-artifact-family-candidates
               (delib-flow--initialize-run (list :title "Example"))
               'reference-notes
               (list candidate)))
         (entry (list :raw-output
                      (list :candidate candidate
                            :drafted-item drafted-item
                            :reason "Drafted the selected note.")))
         (updated (delib-flow--apply-draft-selected-reference-note-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'reference-notes)))
    (should (equal (delib-flow--artifact-candidate-id candidate)
                   (plist-get state :selected-candidate-id)))
    (should (string-match-p "Atlas draft body"
                            (plist-get (plist-get state :selected-draft)
                                       :draft-body)))))

(ert-deftest delib-flow-apply-find-support-for-selected-entry-stores-available-family-support ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (support (list :title "Atlas brief"
                        :support-score 8))
         (run (delib-flow--set-artifact-family-candidates
               (delib-flow--initialize-run (list :title "Example"))
               'reference-notes
               (list candidate)))
         (entry (list :raw-output
                      (list :family 'reference-notes
                            :candidate candidate
                            :support-candidates (list support)
                            :support-context "- Atlas brief"))))
    (let* ((updated (delib-flow--apply-find-support-for-selected-entry run entry))
           (state (plist-get (plist-get updated :artifacts) 'reference-notes)))
      (should (equal (delib-flow--artifact-candidate-id candidate)
                     (plist-get state :selected-candidate-id)))
      (should (equal (list support)
                     (plist-get state :available-support-candidates)))
      (should (equal "- Atlas brief"
                     (plist-get state :available-support-context))))))

(ert-deftest delib-flow-apply-draft-selected-reference-note-entry-seeds-plain-cloud-draft ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (run (delib-flow--set-artifact-family-candidates
               (delib-flow--initialize-run (list :title "Example"))
               'reference-notes
               (list candidate)))
         (entry (list :raw-output
                      (list :candidate candidate
                            :drafted-item
                            (list :kind 'reference-note
                                  :text "Create general PKM note for Project Atlas Pattern"
                                  :draft-body "* Working draft\nAtlas draft body.\n")
                            :reason "Drafted the selected note.")))
         (updated
          (delib-flow--apply-draft-selected-reference-note-entry run entry))
         (draft (delib-flow--artifact-family-selected-draft
                 updated 'reference-notes)))
    (should (string-match-p "Atlas draft body"
                            (plist-get draft :draft-body)))
    (should (string-match-p "^\\* Source highlights$"
                            (plist-get draft :draft-body)))
    (should (string-match-p "^\\* Related material to connect$"
                            (plist-get draft :draft-body)))))

(ert-deftest delib-flow-apply-draft-selected-action-entry-stores-selected-draft ()
  (let* ((candidate (list :kind 'next-action
                          :text "Draft kickoff follow-up"))
         (drafted-item (plist-put (copy-tree candidate)
                                  :text
                                  "Send kickoff follow-up with owners and due dates"))
         (run (delib-flow--set-artifact-family-candidates
               (delib-flow--initialize-run (list :title "Example"))
               'actions
               (list candidate)))
         (entry (list :stage-id 'draft-selected-action
                      :raw-output
                      (list :candidate candidate
                            :drafted-item drafted-item
                            :reason "Drafted the selected action.")))
         (updated (delib-flow--apply-draft-selected-action-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'actions)))
    (should (equal (delib-flow--artifact-candidate-id candidate)
                   (plist-get state :selected-candidate-id)))
    (should (equal "Send kickoff follow-up with owners and due dates"
                   (plist-get (plist-get state :selected-draft)
                              :text)))))

(ert-deftest delib-flow-apply-draft-selected-action-entry-archives-prior-draft ()
  (let* ((candidate (list :kind 'next-action
                          :text "Draft kickoff follow-up"))
         (drafted-one (list :kind 'next-action
                            :text "First drafted action wording"))
         (drafted-two (list :kind 'next-action
                            :text "Second drafted action wording"))
         (run (delib-flow--set-artifact-family-state
               (delib-flow--initialize-run (list :title "Example"))
               'actions
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft drafted-one
                     :draft-history nil)))
         (entry (list :stage-id 'draft-selected-action
                      :raw-output
                      (list :candidate candidate
                            :drafted-item drafted-two
                            :reason "Drafted the selected action again.")))
         (updated (delib-flow--apply-draft-selected-action-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'actions)))
    (should (equal "Second drafted action wording"
                   (plist-get (plist-get state :selected-draft) :text)))
    (should (equal '("First drafted action wording")
                   (mapcar (lambda (item) (plist-get item :text))
                           (plist-get state :draft-history))))
    (should (plist-get (plist-get (car (plist-get state :draft-history))
                                  :evidence-snapshot)
                       :source-title))))

(ert-deftest delib-flow-apply-draft-selected-waiting-for-entry-stores-selected-draft ()
  (let* ((candidate (list :kind 'waiting-for
                          :text "Waiting for Pat to confirm launch date"))
         (drafted-item (plist-put (copy-tree candidate)
                                  :text
                                  "Waiting for Pat to confirm the launch date and owner handoff"))
         (run (delib-flow--set-artifact-family-candidates
               (delib-flow--initialize-run (list :title "Example"))
               'waiting-fors
               (list candidate)))
         (entry (list :stage-id 'draft-selected-waiting-for
                      :raw-output
                      (list :candidate candidate
                            :drafted-item drafted-item
                            :reason "Drafted the selected waiting-for.")))
         (updated (delib-flow--apply-draft-selected-waiting-for-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'waiting-fors)))
    (should (equal (delib-flow--artifact-candidate-id candidate)
                   (plist-get state :selected-candidate-id)))
    (should (equal "Waiting for Pat to confirm the launch date and owner handoff"
                   (plist-get (plist-get state :selected-draft)
                              :text)))))

(ert-deftest delib-flow-apply-draft-selected-project-entry-stores-selected-draft ()
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
         (run (delib-flow--set-artifact-family-candidates
               (delib-flow--initialize-run (list :title "Example"))
               'project-proposals
               (list candidate)))
         (entry (list :stage-id 'draft-selected-project
                      :raw-output
                      (list :candidate candidate
                            :drafted-item drafted-item
                            :reason "Drafted the selected project.")))
         (updated (delib-flow--apply-draft-selected-project-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'project-proposals)))
    (should (equal (delib-flow--artifact-candidate-id candidate)
                   (plist-get state :selected-candidate-id)))
    (should (equal "Draft launch outline for Project Atlas"
                   (plist-get (plist-get (plist-get state :selected-draft)
                                         :first-item)
                              :text)))))

(ert-deftest delib-flow-apply-propose-new-project-entry-seeds-artifact-candidates ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (run (delib-flow--set-artifact-family-state
               run 'project-proposals
               (list :candidates nil
                     :selected-candidate-id "old-project"
                     :selected-draft (list :title "Old project draft"))))
         (entry (list :raw-output
                      (list :project
                            (list :kind 'project
                                  :title "Project Atlas"
                                  :state 'active
                                  :first-item (list :text "Publish timeline")))))
         (updated (delib-flow--apply-propose-new-project-entry run entry))
         (state (plist-get (plist-get updated :artifacts) 'project-proposals)))
    (should (equal 1 (length (plist-get state :candidates))))
    (should (equal "Project Atlas"
                   (plist-get (car (plist-get state :candidates)) :title)))
    (should (equal (delib-flow--artifact-candidate-id
                    (car (plist-get state :candidates)))
                   (plist-get state :selected-candidate-id)))
    (should-not (plist-get state :selected-draft))))

(ert-deftest delib-flow-apply-suggest-reference-notes-entry-clears-stale-approved-note-preview-state ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Source"
                     :content "* Source\nBody line\n")))
         (stale-approved (list :kind 'reference-note
                               :text "Create general PKM note for Old Subject Title"
                               :note-type 'general-pkm
                               :draft-stage 'suggest-reference-notes))
         (run (plist-put
               run :filing
               (plist-put (plist-get run :filing) :approved-items (list stale-approved))))
         (entry (list :stage-id 'suggest-reference-notes
                      :raw-output
                      (list :reference-notes
                            (list (list :kind 'reference-note
                                        :text "Create general PKM note for Project Atlas Pattern"
                                        :note-type 'general-pkm)))))
         (updated (delib-flow--apply-suggest-reference-notes-entry run entry))
         (capture-text (delib-flow--editable-block-text
                        (delib-flow--editable-block updated 'reference-note-capture-review))))
    (should-not (plist-get (plist-get updated :filing) :approved-items))
    (should (string-match-p "No reference-note filing artifact is currently active" capture-text))))

(ert-deftest delib-flow-apply-draft-selected-reference-note-entry-stores-part-outcome-summary ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (previous
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
              "- Existing highlight."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n")))
         (updated-item
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
              "- New highlight."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n")))
         (run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run
            (list :title "Source"
                  :content "* Source\nNew highlight.\n"))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                 :selected-draft previous)))
         (entry (list :raw-output
                      (list :candidate candidate
                            :drafted-item updated-item
                            :part-id 'source-highlights
                            :part-text "- New highlight."
                            :reason "Refreshed the source highlights section for the selected note only.")))
         (updated (delib-flow--apply-draft-selected-reference-note-entry run entry))
         (outcome (delib-flow--artifact-family-part-outcome
                   updated 'reference-notes 'source-highlights)))
    (should (equal "Changed: highlight selection updated."
                   (plist-get outcome :summary)))
    (should (string-match-p "Refreshed the source highlights section"
                            (plist-get outcome :reason)))))

(ert-deftest delib-flow-placeholder-stage-action-errors ()
  (should-error (delib-flow-action-stage-placeholder)))

(ert-deftest delib-flow-execute-local-stage-marks-run-in-flight ()
  (let (success-callback error-callback
        (delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
        (delib-flow-local-stage-async-adapter
         (lambda (_descriptor _package on-success on-error)
           (setq success-callback on-success
                 error-callback on-error)
           'fake-handle)))
    (cl-letf (((symbol-function 'delib-flow--rerender-active-run-buffer) #'ignore)
              ((symbol-function 'delib-flow--refresh-in-flight-ui) #'ignore)
              ((symbol-function 'delib-flow--ensure-in-flight-ui-timer) #'ignore))
      (let* ((run (delib-flow--initialize-run
                   (list :title "Example"
                         :content "* Example\nBody line\n")))
             (updated-run (delib-flow--execute-local-stage run 'inspect-source))
             (text (delib-flow--section-content "Current result" updated-run))
             (inspect-action
              (seq-find
               (lambda (action)
                 (eq (plist-get action :id) 'inspect-source))
               (plist-get (delib-flow--run-actions updated-run) :items))))
        (should (delib-flow--run-in-flight-p updated-run))
        (should (eq 'inspect-source
                    (delib-flow--run-in-flight-stage-id updated-run)))
        (should (equal 'fake-handle
                       (plist-get (delib-flow--run-session updated-run)
                                  :in-flight-handle)))
        (should inspect-action)
        (should (eq 'blocked (plist-get inspect-action :status)))
        (should (string-match-p "Status: running" text))
        (should (string-match-p "Inspect Source" text))))))

(ert-deftest delib-flow-async-local-stage-callback-finalizes-run ()
  (let ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter))
    (cl-letf (((symbol-function 'delib-flow--rerender-active-run-buffer) #'ignore)
              ((symbol-function 'delib-flow--refresh-in-flight-ui) #'ignore))
      (let* ((run (delib-flow--initialize-run
                   (list :title "Example"
                         :content "* Example\nBody line\n")))
             (prepared-run (delib-flow--prepare-reviewable-stage-retry
                            run 'inspect-source))
             (package (delib-flow--stage-input-package prepared-run 'inspect-source))
             (started-at (current-time))
             (request-id "request-1")
             (raw-output
              (funcall delib-flow-local-stage-adapter
                       (delib-flow--stage-descriptor 'inspect-source)
                       package)))
        (setq delib-flow--active-run
              (delib-flow--mark-stage-in-flight
               prepared-run 'inspect-source 'local "llama3" nil request-id started-at))
        (delib-flow--complete-async-local-stage
         prepared-run 'inspect-source package started-at request-id raw-output)
        (should delib-flow--active-run)
        (should-not (delib-flow--run-in-flight-p delib-flow--active-run))
        (should (eq 'inspect-source
                    (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                               :stage-id)))
        (should (eq 'completed
                    (plist-get (delib-flow--latest-stage-entry delib-flow--active-run)
                               :status)))))))

(ert-deftest delib-flow-execute-local-stage-handles-immediate-async-completion ()
  (let ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter)
        (delib-flow-local-stage-async-adapter
         (lambda (descriptor package on-success _on-error)
           (funcall on-success
                    (funcall #'delib-flow--default-local-stage-adapter
                             descriptor package))
           'fake-handle)))
    (cl-letf (((symbol-function 'delib-flow--rerender-active-run-buffer) #'ignore)
              ((symbol-function 'delib-flow--refresh-in-flight-ui) #'ignore)
              ((symbol-function 'delib-flow--ensure-in-flight-ui-timer) #'ignore))
      (let* ((run (delib-flow--initialize-run
                   (list :title "Example"
                         :content "* Example\nBody line\n")))
             (updated-run (delib-flow--execute-local-stage run 'inspect-source)))
        (should-not (delib-flow--run-in-flight-p updated-run))
        (should (eq 'inspect-source
                    (plist-get (delib-flow--latest-stage-entry updated-run)
                               :stage-id)))
        (should (eq 'completed
                    (plist-get (delib-flow--latest-stage-entry updated-run)
                               :status)))))))

(provide 'delib-flow-stages-test)

;;; delib-flow-stages-test.el ends here
