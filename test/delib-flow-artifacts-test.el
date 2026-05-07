;;; delib-flow-artifacts-test.el --- Artifact tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)

(ert-deftest delib-flow-source-reference-note-focus-descriptor-keeps-durable-metadata ()
  (let* ((descriptor
          (delib-flow--reference-note-focus-descriptor
           "Laying the foundation: the Master Prompt, PARA adapted for the AI era"))
         (reuse-claim (plist-get descriptor :reuse-claim)))
    (should (equal "Laying the foundation: the Master Prompt, PARA adapted for the AI era"
                   (plist-get descriptor :candidate-identity)))
    (should (> (plist-get descriptor :focus-score) 7))
    (should (string-match-p "adapted as a reusable operating pattern"
                            reuse-claim))))

(ert-deftest delib-flow-reference-note-focus-overlap-distinguishes-separate-concepts ()
  (let ((left (delib-flow--reference-note-focus-descriptor
               "Building personal AI advisors"))
        (right (delib-flow--reference-note-focus-descriptor
                "Laying the foundation: the Master Prompt")))
    (should-not (delib-flow--reference-note-focus-overlap-p left right))))

(ert-deftest delib-flow-reference-note-focus-overlap-detects-shared-concept-variants ()
  (let ((left (delib-flow--reference-note-focus-descriptor
               "Building personal AI advisors"))
        (right (delib-flow--reference-note-focus-descriptor
                "Building personal AI advisors for team workflows")))
    (should (delib-flow--reference-note-focus-overlap-p left right))))

(ert-deftest delib-flow-promote-reference-note-focus-descriptors-keeps-strongest-overlap ()
  (let* ((descriptors
          (list
           (delib-flow--reference-note-focus-descriptor
            "Building personal AI advisors")
           (delib-flow--reference-note-focus-descriptor
            "Building personal AI advisors for team workflows")
           (delib-flow--reference-note-focus-descriptor
            "Laying the foundation: the Master Prompt")))
         (promoted
          (delib-flow--promote-reference-note-focus-descriptors descriptors))
         (focuses (mapcar (lambda (descriptor)
                            (plist-get descriptor :focus))
                          promoted)))
    (should (member "Building personal AI advisors for team workflows" focuses))
    (should (member "Laying the foundation: the Master Prompt" focuses))
    (should (= 2 (length promoted)))))

(ert-deftest delib-flow-reference-note-item-promotion-penalizes-source-local-heading-focus ()
  (let* ((durable (list :kind 'reference-note
                        :text "Create general PKM note for Building personal AI advisors"
                        :note-type 'general-pkm
                        :candidate-focus "Building personal AI advisors"
                        :candidate-identity "Building personal AI advisors"
                        :focus-score 10
                        :reuse-claim "Captures the advisor pattern as reusable."))
         (source-local (list :kind 'reference-note
                             :text "Create general PKM note for Week 1 overview: Building personal AI advisors"
                             :note-type 'general-pkm
                             :candidate-focus "Week 1 overview: Building personal AI advisors"
                             :candidate-identity "Week 1 overview: Building personal AI advisors"
                             :focus-score 10
                             :reuse-claim "Captures the setup pattern as reusable.")))
    (should (> (delib-flow--reference-note-item-promotion-score durable)
               (delib-flow--reference-note-item-promotion-score source-local)))))

(ert-deftest delib-flow-source-reference-note-focus-descriptors-fall-back-to-title ()
  (let* ((package (list :source (list :title "Durable system pattern"
                                      :content "* Durable system pattern\nBody\n")
                        :working-context nil))
         (descriptors (delib-flow--source-reference-note-focus-descriptors package)))
    (should (equal 1 (length descriptors)))
    (should (equal "Durable system pattern"
                   (plist-get (car descriptors) :focus)))))

(ert-deftest delib-flow-reference-note-reuse-claim-for-focus-skips-weak-topic ()
  (should-not
   (delib-flow--reference-note-reuse-claim-for-focus "Weekly update")))

(ert-deftest delib-flow-proposed-project-item-derives-concrete-child-from-breakage-note ()
  (let* ((package (list :source (list :title "broken steno"
                                      :content "* broken steno\nFix broken steno translation in Emacs\n")
                        :working-context nil))
         (project (delib-flow--proposed-project-item package))
         (warnings (mapcar (lambda (warning)
                             (plist-get warning :code))
                           (plist-get project :warnings))))
    (should (equal "Fix broken steno translation in Emacs"
                   (plist-get (delib-flow--project-first-item project) :text)))
    (should (= 2 (length (delib-flow--project-child-items project))))
    (should (member "Investigate and fix steno"
                    (mapcar (lambda (item) (plist-get item :text))
                            (delib-flow--project-child-items project))))
    (should-not (memq 'project-first-item-generic warnings))
    (should (delib-flow--draft-item-ready-p project))))

(ert-deftest delib-flow-proposed-project-item-flags-missing-concrete-child-item ()
  (let* ((package (list :source (list :title "Idea about better triage"
                                      :content "* Idea about better triage\nThis might matter later.\n")
                        :working-context nil))
         (project (delib-flow--proposed-project-item package))
         (warnings (plist-get project :warnings)))
    (should (equal "No concrete child item could be derived from this source yet"
                   (plist-get (delib-flow--project-first-item project) :text)))
    (should (eq 'project-first-item-generic
                (plist-get (car warnings) :code)))
    (should-not (delib-flow--draft-item-ready-p project))))

(ert-deftest delib-flow-proposed-project-item-uses-operator-intent-for-broken-website-issue-note ()
  (let* ((package (list :source (list :title "Broken steno exercise"
                                      :content "* Broken steno exercise\nhttps://example.com/drill?id=one\nhttps://example.com/drill?id=two\n")
                        :working-context
                        (list :inspect-output (list :source-type 'issue-note))
                        :ui (list :operator-intent "this is about fixing a broken website")))
         (project (delib-flow--proposed-project-item package)))
    (should (equal "Fix broken steno website exercises"
                   (plist-get project :title)))
    (should (equal "Investigate and fix broken steno website exercises"
                   (plist-get (delib-flow--project-first-item project) :text)))
    (should (member "issue_note" (plist-get project :tags)))
    (should (delib-flow--draft-item-ready-p project))))

(ert-deftest delib-flow-reference-note-warnings-keep-general-note-warning-without-project-context ()
  (let* ((package
          (list :source (list :title "Standalone Idea")
                :working-context (list :project-match nil)))
         (item (delib-flow--source-title-reference-note package))
         (warnings (delib-flow--reference-note-warnings item package)))
    (should (eq 'general-pkm (plist-get item :note-type)))
    (should (= 2 (length warnings)))
    (should (member 'reference-note-candidate-identity
                    (mapcar (lambda (warning)
                              (plist-get warning :code))
                            warnings)))
    (should (member 'reference-note-reuse-justification
                    (mapcar (lambda (warning)
                              (plist-get warning :code))
                            warnings)))))

(ert-deftest delib-flow-reference-note-warning-weak-identity-skips-strong-durable-focus ()
  (let* ((item (list :kind 'reference-note
                     :text "Create general PKM note for Building personal AI advisors"
                     :note-type 'general-pkm
                     :focus-score 10)))
    (should-not (delib-flow--reference-note-warning-weak-identity item))))

(ert-deftest delib-flow-reference-note-warning-template-title-is-blocking ()
  (let ((delib-flow-project-support-note-capture-template
         "#+filetags: :project:support:\n\nSource artifact: %(delib-flow-capture-source-artifact)\n"))
    (let* ((item (list :kind 'reference-note
                       :text "Create project support note from Alpha Constraints"
                       :note-type 'project-support))
           (warning (delib-flow--reference-note-warning-template-title item)))
      (should warning)
      (should (eq 'blocking (plist-get warning :severity))))))

(ert-deftest delib-flow-normalize-tag-suggestion-strips-noise ()
  (should (equal "building_personal_ai_advisors"
                 (delib-flow--normalize-tag-suggestion
                  "Building personal AI advisors!")))
  (should-not (delib-flow--normalize-tag-suggestion "the"))
  (should-not (delib-flow--normalize-tag-suggestion "1234")))

(ert-deftest delib-flow-normalize-waiting-for-line-normalizes-awaiting ()
  (should (equal "Waiting for Pat to confirm launch date"
                 (delib-flow--normalize-waiting-for-line
                  "Awaiting Pat to confirm launch date."))))

(ert-deftest delib-flow-normalize-propose-new-project-output-renders-summary ()
  (let ((text
         (delib-flow--normalize-propose-new-project-output
          (list :project-title "Project Atlas"
                :project-state 'active
                :first-item (list :text "Draft launch checklist")
                :tags '("launch" "ops")
                :reason "No project matched."))))
    (should (string-match-p "Project Atlas" text))
    (should (string-match-p "Draft launch checklist" text))
    (should (string-match-p "launch, ops" text))))

(ert-deftest delib-flow-normalize-select-approved-filing-actions-output-renders-planned-targets ()
  (let ((text
         (delib-flow--normalize-select-approved-filing-actions-output
          (list :selected-count 1
                :remaining-draft-count 0
                :approval-blocked-p nil
                :ready-selection-indexes '(1)
                :blocked-selection-indexes nil
                :operator-selection "1"
                :operator-notes nil
                :reason "Ready to file."
                :selected-preview "- Preview"
                :planned-target-locations
                (list (list :item-text "Draft launch checklist"
                            :target "/tmp/project.org::Project Atlas"))))))
    (should (string-match-p "Planned file targets" text))
    (should (string-match-p "Draft launch checklist -> /tmp/project.org::Project Atlas"
                            text))))

(ert-deftest delib-flow-source-reference-notes-prune-overlapping-weak-variants ()
  (let* ((package
          (list :source
                (list :title "Weekly update"
                      :content
                      (concat
                       "* Weekly update :email:\n"
                       ":RAW_EMAIL:\n"
                       "Subject: Weekly update\n\n"
                       "The idea: Building personal AI advisors.\n"
                       "----------------------------------\n"
                       "Building personal AI advisors\n"
                       "----------------------------------\n"
                       ":END:\n"))
                :working-context (list :project-match nil)))
         (items (delib-flow--source-reference-notes package))
         (titles (mapcar #'delib-flow--reference-note-title items)))
    (should (equal '("Building personal AI advisors") titles))))

(ert-deftest delib-flow-reference-note-content-merges-suggested-filetags ()
  (let ((delib-flow-general-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\n#+filetags: :wiki:draft:stub:\n"))
    (let* ((item (list :kind 'reference-note
                       :text "Create general PKM note for Your Consumption Diet Is Your Moat (concept)"
                       :note-type 'general-pkm
                       :tag-suggestions '("reference_note" "general_pkm" "consumption" "diet" "moat" "concept")))
           (content (delib-flow--reference-note-content item (list :source (list :title "Example")))))
      (should (string-match-p "#\\+filetags: :wiki:draft:stub:consumption:diet:moat:concept:" content))
      (should-not (string-match-p "reference_note" content))
      (should-not (string-match-p "general_pkm" content)))))

(ert-deftest delib-flow-reference-note-content-enriches-thin-drafted-body-with-structure ()
  (let ((delib-flow-general-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\n#+filetags: :wiki:draft:stub:\n"))
    (let* ((item (list :kind 'reference-note
                       :text "Create general PKM note for Your Consumption Diet Is Your Moat (concept)"
                       :note-type 'general-pkm
                       :tag-suggestions '("consumption" "diet" "moat" "concept")
                       :draft-body "Instead of a generic chatbot, create a named advisor with a defined role, clear scope, and personal context to give advice that fits your life."))
           (package
            (list :source
                  (list :title "Your Consumption Diet Is Your Moat"
                        :content
                        (concat
                         "* Your Consumption Diet Is Your Moat :email:\n"
                         ":RAW_EMAIL:\n"
                         "Your Consumption Diet Is Your Moat\n\n"
                         "The idea: instead of a generic chatbot, you create a named advisor with a defined role, a clear scope, and the personal context to give advice that fits your life.\n"
                         ":END:\n"))))
           (content (delib-flow--reference-note-content item package)))
      (should (string-match-p "\\* Working draft" content))
      (should (string-match-p "\\* Source highlights" content))
      (should (string-match-p "\\* Related material to connect" content))
      (should (string-match-p "\\* Next pass" content)))))

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

(ert-deftest delib-flow-reference-note-content-uses-capture-template-patterns ()
  (let ((delib-flow-project-support-note-capture-template
         "#+title: %(delib-flow-capture-note-title)\nCreated: %<%Y>\nArtifact: %(delib-flow-capture-source-artifact)\n"))
    (let ((content
           (delib-flow--reference-note-content
            (list :kind 'reference-note
                  :text "Create project support note from Alpha Constraints"
                  :note-type 'project-support))))
      (should (string-match-p "#\\+title: Alpha Constraints" content))
      (should (string-match-p "Created: 20[0-9][0-9]" content))
      (should (string-match-p
               "Artifact: Create project support note from Alpha Constraints"
               content)))))

(ert-deftest delib-flow-reference-note-content-seeds-structure-and-supporting-material ()
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
                       "Week 1 was about laying the foundation: the Master Prompt, PARA adapted for the AI era, and a capture system.\n"
                       "This week, we covered building personal AI advisors and a nutrition coach example.\n"
                       ":END:\n"))
                :working-context
                (list :inspect-output
                      '(:source-type email
                        :analysis (:summary "A newsletter about building personal AI advisors and related systems."))
                      :filtered-context
                      (list :retained-candidates
                            (list (list :title "AI advisor notes"
                                        :score 5
                                        :filter-reasons '("retained-by-score-threshold"))))
                      :contact-emails '("hello@fortelabs.com"))))
         (content
          (delib-flow--reference-note-content
           (list :kind 'reference-note
                 :text "Create general PKM note for Building personal AI advisors"
                 :note-type 'general-pkm)
           package)))
    (should (string-match-p "\\* Working draft" content))
    (should (string-match-p "This note captures Building personal AI advisors" content))
    (should (string-match-p "- Durable claim:" content))
    (should (string-match-p "- Why it matters:" content))
    (should (string-match-p "- Reuse angle:" content))
    (should (string-match-p "\\* Source highlights" content))
    (should (string-match-p "building personal AI advisors" content))
    (should (string-match-p "\\* Related material to connect" content))
    (should (string-match-p "Add nearby notes, projects, or references that deepen this idea" content))
    (should (string-match-p "\\* Source context" content))
    (should (string-match-p "Source title: Your Consumption Diet Is Your Moat" content))
    (should (string-match-p "Contact\\(s\\|(s)\\)?: hello@fortelabs.com" content))
    (should (string-match-p "\\* Next pass" content))))

(ert-deftest delib-flow-reference-note-source-highlights-prefer-focused-concept-section ()
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
                       "Subject: Your Consumption Diet Is Your Moat\n\n"
                       "A quick update on week 2 of the AI Second Brain cohort.\n\n"
                       "This week, we covered a lot of ground across three sessions.\n\n"
                       "----------------------------------\n"
                       "Your Consumption Diet Is Your Moat\n"
                       "----------------------------------\n\n"
                       "I've been rethinking what I consume lately, and a clear pattern has emerged: some content is getting more valuable in the AI era.\n\n"
                       "The idea: instead of a generic chatbot, you create a named advisor with a defined role, a clear scope, and the personal context to give advice that fits your life.\n"
                       ":END:\n"))))
         (item (list :kind 'reference-note
                     :text "Create general PKM note for Your Consumption Diet Is Your Moat (concept)"
                     :note-type 'general-pkm))
         (highlights (delib-flow--reference-note-source-highlights item package)))
    (should highlights)
    (should (seq-some
             (lambda (line)
               (string-match-p "instead of a generic chatbot" line))
             highlights))
    (should-not (equal "A quick update on week 2 of the AI Second Brain cohort"
                       (car highlights)))))

(ert-deftest delib-flow-reference-note-support-lines-prefer-selected-focused-support ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (support (list :title "Atlas brief"
                        :filter-reasons '("focused-retained")))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Source"
                       :content "* Source\nBody line\n"))
                :working-context
                (list :retained-candidates
                      (list (list :title "Broad retained note"
                                  :filter-reasons '("retained-by-score-threshold")))))
               'reference-notes
               (list :candidates (list candidate)
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft (plist-put (copy-tree candidate)
                                                :draft-body "* Working draft\nBody.\n")
                     :selected-support-candidates (list support)
                     :selected-support-context "- Atlas brief - Constraint summary"))))
    (should (equal '("Atlas brief (focused-retained)")
                   (delib-flow--reference-note-support-lines run)))))

(ert-deftest delib-flow-retained-candidate-reference-notes-require-effective-project-context ()
  (let ((package (list :working-context nil)))
    (cl-letf (((symbol-function 'delib-flow--retained-candidates)
               (lambda (_package)
                 (list (list :title "Retained blocker")))))
      (should-not (delib-flow--retained-candidate-reference-notes package)))))

(ert-deftest delib-flow-retained-candidate-reference-notes-accept-drafted-proposed-project-context ()
  (let ((package
         (list :working-context nil
               :artifacts
               (list 'project-proposals
                     (list :selected-draft
                           (list :kind 'project
                                 :title "Fix broken steno website exercises"))))))
    (cl-letf (((symbol-function 'delib-flow--retained-candidates)
               (lambda (_package)
                 (list (list :title "Retained blocker")))))
      (should (equal 1
                     (length
                      (delib-flow--retained-candidate-reference-notes
                       package)))))))

(ert-deftest delib-flow-build-draft-evidence-snapshot-captures-structured-support-items ()
  (let* ((candidate (list :kind 'next-action
                          :text "Queue action text"
                          :source 'source))
         (support (list :title "Atlas brief"
                        :support-score 8
                        :support-reasons '("title-overlap")))
         (draft (list :kind 'next-action
                      :text "Current drafted wording"))
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
                     :selected-support-candidates (list support)
                     :selected-support-context "- Atlas support context")))
         (snapshot (delib-flow--build-draft-evidence-snapshot run 'actions draft))
         (support-item (car (plist-get snapshot :support-items))))
    (should (equal "Atlas brief" (plist-get support-item :title)))
    (should (equal 8 (plist-get support-item :score)))
    (should (equal "title-overlap" (plist-get support-item :reason-text)))
    (should (string-match-p "Atlas brief" (plist-get support-item :line)))
    (should (equal '("- Atlas support context")
                   (plist-get snapshot :support-context-lines)))))

(ert-deftest delib-flow-build-draft-evidence-snapshot-captures-family-specific-review-lines ()
  (let* ((candidate (list :kind 'project
                          :title "Project Atlas"
                          :text "Project Atlas"
                          :state 'active
                          :first-item (list :kind 'next-action
                                            :text "Define first deliverable for Project Atlas")
                          :tags '("atlas")
                          :source 'project-proposal))
         (draft (list :kind 'project
                      :title "Project Atlas"
                      :text "Project Atlas"
                      :state 'active
                      :first-item (list :kind 'next-action
                                        :text "Define first deliverable for Project Atlas")
                      :tags '("atlas")))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Project Atlas"
                       :content "* Project Atlas\nContext only.\n"))
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
                     :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                     :selected-draft draft
                     :draft-history nil
                     :selected-support-candidates nil
                     :selected-support-context nil)))
         (snapshot (delib-flow--build-draft-evidence-snapshot run 'project-proposals draft))
         (detail (delib-flow--draft-evidence-snapshot-detail snapshot)))
    (should (equal "Project-specific checks"
                   (plist-get snapshot :family-review-heading)))
    (should (string-match-p "Title identity: `Project Atlas` now reads like the working project name" detail))
    (should (string-match-p "First-item check: `Define first deliverable for Project Atlas` is the current concrete starting move" detail))
    (should (string-match-p "Tag check: `atlas` is the current project tag set" detail))))

(ert-deftest delib-flow-build-draft-evidence-snapshot-captures-quality-gap-identities ()
  (let* ((candidate (list :kind 'next-action
                          :text "Keep Friday launch target"
                          :source 'source))
         (draft (list :kind 'next-action
                      :text "Keep Friday launch target"
                      :warnings
                      (list
                       (delib-flow--make-artifact-warning
                        'decision-state-action
                        "Reads like a decision or status statement rather than a directly executable next action."
                        'blocking))))
         (run (delib-flow--set-artifact-family-state
               (plist-put
                (delib-flow--initialize-run
                 (list :title "Live source title"
                       :content "* Live source title\n- Keep Friday launch target\n"))
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
         (snapshot (delib-flow--build-draft-evidence-snapshot run 'actions draft)))
    (should (equal '("decision-state-action" "no-focused-support")
                   (mapcar (lambda (item) (plist-get item :identity))
                           (plist-get snapshot :quality-gap-items))))
    (should (equal '("decision-state-action" "no-focused-support")
                   (delib-flow--draft-evidence-quality-gap-identities snapshot)))))

(ert-deftest delib-flow-clean-source-view-text-prefers-email-type-and-contacts ()
  (let* ((package (list :source
                        (list :title "Newsletter title"
                              :type "article"
                              :content "* Newsletter title\nUseful line\n")
                        :working-context nil))
         text)
    (cl-letf (((symbol-function 'delib-flow--package-contact-emails)
               (lambda (_) '("editor@example.com" "author@example.com")))
              ((symbol-function 'delib-flow--package-email-digest)
               (lambda (_) (list :type-hint "newsletter")))
              ((symbol-function 'delib-flow--clean-source-view-lines)
               (lambda (_) '("Useful line one" "Useful line two"))))
      (setq text (delib-flow--clean-source-view-text package)))
    (should (string-match-p "- Type: newsletter" text))
    (should (string-match-p "- Contacts: editor@example.com, author@example.com" text))
    (should (string-match-p "- Useful line one" text))
    (should (string-match-p "- Useful line two" text))))

(ert-deftest delib-flow-clean-source-view-body-lines-renders-empty-state ()
  (should (equal '("*** Source text" "- No cleaned source lines are available.")
                 (delib-flow--clean-source-view-body-lines nil))))

(provide 'delib-flow-artifacts-test)

;;; delib-flow-artifacts-test.el ends here
