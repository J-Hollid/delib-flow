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
    (should (string-match-p "reusable"
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

(ert-deftest delib-flow-email-digest-reference-note-focuses-prefer-structural-body-claims ()
  (let* ((digest
          (list :subject "Why metadata programs fail to become operational"
                :plain-body
                (string-join
                 '("This essay is about the gap between agreeing that metadata matters and actually using metadata in day-to-day delivery."
                   ""
                   "A practical framing we liked:"
                   "Metadata maturity becomes real when it changes one handoff, one exception path, or one review ritual."
                   ""
                   "For teams starting from scratch, the advice was blunt:"
                   "Do not launch a \"metadata program.\" Pick one operational loop where missing metadata already hurts.")
                 "\n")))
         (focuses (delib-flow--email-digest-reference-note-focuses digest)))
    (should (member
             "Metadata maturity becomes real when it changes one handoff, one exception path, or one review ritual"
             focuses))
    (should-not (member
                 "Why metadata programs fail to become operational"
                 focuses))))

(ert-deftest delib-flow-email-digest-reference-note-focuses-extract-forwardable-packet-pattern ()
  (let* ((digest
          (list :subject "Escalations that move versus escalations that drift"
                :plain-body
                (string-join
                 '("Escalations that move tend to have:"
                   "- one named owner for the next external update"
                   "- one lightweight packet that can be forwarded without explanation"
                   "- one explicit statement of what is blocked on the vendor versus blocked on us")
                 "\n")))
         (focuses (delib-flow--email-digest-reference-note-focuses digest)))
    (should (member
             "One lightweight packet that can be forwarded without explanation"
             focuses))
    (should-not (member
                 "Escalations that move versus escalations that drift"
                 focuses))))

(ert-deftest delib-flow-reference-note-focus-normalizes-theme-focus-phrasing ()
  (let ((descriptor
         (delib-flow--reference-note-focus-descriptor
          "AI Readiness: Focus on Data Maturity")))
    (should (equal "Data Maturity for AI Readiness"
                   (plist-get descriptor :focus)))
    (should (equal "Data Maturity for AI Readiness"
                   (plist-get descriptor :candidate-identity)))))

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

(ert-deftest delib-flow-proposed-project-item-uses-concrete-meeting-note-fallback-child ()
  (let* ((package (list :source (list :title "AI Kickoff"
                                      :content "* AI Kickoff\n- Session objectives\n  - Where we are now\n")
                        :working-context
                        (list :inspect-output (list :source-type 'meeting-note))))
         (project (delib-flow--proposed-project-item package)))
    (should (equal "Draft follow-up summary for AI Kickoff"
                   (plist-get (delib-flow--project-first-item project) :text)))
    (should (delib-flow--draft-item-ready-p project))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 project 'project-first-item-generic))))

(ert-deftest delib-flow-proposed-project-item-does-not-treat-fitness-as-repair-intent ()
  (let* ((package
          (list :source
                (list :title "Your Consumption Diet Is Your Moat"
                      :content
                      (concat
                       "* Your Consumption Diet Is Your Moat :email:\n"
                       ":RAW_EMAIL:\n"
                       "Others are working on fitness coaches, financial advisors, and strategy consultants.\n"
                       ":END:\n"))
                :working-context
                (list :inspect-output (list :source-type 'email
                                            :analysis nil)
                      :email-inspect-digest
                      (list :subject "Your Consumption Diet Is Your Moat"
                            :type-hint "newsletter or mailing list"
                            :plain-body
                            "Others are working on fitness coaches, financial advisors, and strategy consultants.\n"))))
         (project (delib-flow--proposed-project-item package)))
    (should-not (string-prefix-p "Fix " (plist-get project :title)))
    (should-not (string-prefix-p "Investigate and fix"
                                 (plist-get (delib-flow--project-first-item project)
                                            :text)))))

(ert-deftest delib-flow-proposed-project-item-blocks-conceptual-newsletter-projects ()
  (let* ((package
          (list :source
                (list :title "Your Consumption Diet Is Your Moat"
                      :content
                      (concat
                       "* Your Consumption Diet Is Your Moat :email:\n"
                       ":RAW_EMAIL:\n"
                       "Subject: Your Consumption Diet Is Your Moat\n\n"
                       "The idea: instead of a generic chatbot, you create a named advisor with a defined role, a clear scope, and the personal context to give advice that fits your life.\n"
                       ":END:\n"))
                :working-context
                (list :inspect-output (list :source-type 'email)
                      :email-inspect-digest
                      (list :subject "Your Consumption Diet Is Your Moat"
                            :type-hint "newsletter or mailing list"
                            :plain-body
                            "The idea: instead of a generic chatbot, you create a named advisor with a defined role, a clear scope, and the personal context to give advice that fits your life.\n"))))
         (project (delib-flow--proposed-project-item package)))
    (should (delib-flow--draft-item-has-warning-code-p
             project 'project-conceptual-newsletter))
    (should-not (delib-flow--draft-item-ready-p project))))

(ert-deftest delib-flow-project-drafted-first-item-text-replaces-placeholder-with-concrete-fallback ()
  (let* ((package (list :source (list :title "AI Kickoff"
                                      :content "* AI Kickoff\n- Session objectives\n")
                        :working-context
                        (list :inspect-output (list :source-type 'meeting-note))))
         (item (delib-flow--make-draft-project
                "AI Kickoff"
                'active
                (delib-flow--make-draft-action
                 "No concrete child item could be derived from this source yet"
                 'project-proposal)
                nil)))
    (should (equal "Draft follow-up summary for AI Kickoff"
                   (delib-flow--project-drafted-first-item-text
                    item package
                    "No concrete child item could be derived from this source yet")))))

(ert-deftest delib-flow-proposed-project-item-uses-operator-intent-for-broken-website-issue-note ()
  (let* ((package (list :source (list :title "Broken steno exercise"
                                      :content "* Broken steno exercise\nhttps://example.com/drill?id=one\nhttps://example.com/drill?id=two\n")
                        :working-context
                        (list :inspect-output (list :source-type 'issue-note))
                        :ui (list :operator-intent "this is about fixing a broken website")))
         (project (delib-flow--proposed-project-item package)))
    (should (equal "Fix broken steno exercise on website"
                   (plist-get project :title)))
    (should (equal "Investigate and fix broken steno exercise on website"
                   (plist-get (delib-flow--project-first-item project) :text)))
    (should (member "issue_note" (plist-get project :tags)))
    (should (delib-flow--draft-item-ready-p project))))

(ert-deftest delib-flow-repair-project-focus-text-generalizes-website-fix-targets ()
  (let* ((package (list :source (list :title "Broken invoice export"
                                      :content "* Broken invoice export\n")
                        :working-context
                        (list :inspect-output (list :source-type 'issue-note))
                        :ui (list :operator-intent "Need to fix a broken website flow for this export"))))
    (should (equal "broken invoice export on website"
                   (delib-flow--repair-project-focus-text package)))))

(ert-deftest delib-flow-reference-note-reuse-claim-for-focus-generalizes-beyond-advisor-vocabulary ()
  (let ((claim (delib-flow--reference-note-reuse-claim-for-focus
                "Constraint-first rollout planning for risky launches")))
    (should (string-match-p "reusable" claim))
    (should-not (string-match-p "advisor\\|adapted as" claim))))

(ert-deftest delib-flow-reference-note-highlight-score-prefers-structural-concept-cues ()
  (let* ((terms '("constraint" "rollout" "planning"))
         (durable "Constraint-first rollout planning reduces avoidable launch risk across handoffs.")
         (source-local "Week 2 overview for the rollout cohort.")
         (durable-score (delib-flow--reference-note-highlight-score durable terms))
         (source-local-score (delib-flow--reference-note-highlight-score source-local terms)))
    (should (> durable-score source-local-score))))

(ert-deftest delib-flow-proposed-project-item-prefers-stronger-ready-source-action-over-earlier-reflective-bullet ()
  (let* ((package (list :source (list :title "Presentation or article on focusing on the end user"
                                      :content
                                      (string-join
                                       '("* Presentation or article on focusing on the end user"
                                         "+ No longer treating my role as divorced from the actual analytics dashboards created by the end user."
                                         "+ More than just what are the requirements, put myself into their shoes for how do we build these dashboards."
                                         "+ Gather a number of success cases with client satisfaction to showcase")
                                       "\n"))
                        :working-context nil
                        :ui nil))
         (project (delib-flow--proposed-project-item package))
         (first-item (delib-flow--project-first-item project)))
    (should (equal "Gather a number of success cases with client satisfaction to showcase"
                   (plist-get first-item :text)))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 first-item
                 'introspective-next-action))
    (should (delib-flow--draft-item-ready-p project))))

(ert-deftest delib-flow-source-action-lines-filter-meeting-note-headings ()
  (let* ((package (list :source (list :title "AI Kickoff"
                                      :content
                                      (string-join
                                       '("* AI Kickoff"
                                         "- Session objectives"
                                         "  - Where we are now"
                                         "  - Create value everywhere."
                                         "  - Share the retro notes."
                                         "  - Draft pilot outline for next session.")
                                       "\n"))
                        :working-context
                        (list :inspect-output (list :source-type 'meeting-note))))
         (lines (delib-flow--source-action-lines package)))
    (should-not (member "Session objectives" lines))
    (should-not (member "Where we are now" lines))
    (should-not (member "Create value everywhere" lines))
    (should (member "Share the retro notes" lines))
    (should (member "Draft pilot outline for next session" lines))))

(ert-deftest delib-flow-annotate-draft-actions-blocks-heading-and-strategic-theme-items ()
  (let* ((items (delib-flow--annotate-draft-actions
                 (list (delib-flow--make-draft-action "Session objectives" 'source)
                       (delib-flow--make-draft-action "Create value everywhere" 'source))
                 nil))
         (heading (car items))
         (theme (cadr items)))
    (should (delib-flow--draft-item-has-warning-code-p
             heading 'heading-phrase-action))
    (should (delib-flow--draft-item-has-warning-code-p
             theme 'strategic-theme-action))))

(ert-deftest delib-flow-annotate-draft-actions-blocks-broad-initiative-items ()
  (let* ((item (list :kind 'next-action
                     :text "Conduct focused joint pilots with AI proven in practice"
                     :source 'source))
         (annotated (car (delib-flow--annotate-draft-actions (list item)))))
    (should (delib-flow--draft-item-has-warning-code-p
             annotated 'broad-initiative-action))
    (should-not (delib-flow--draft-item-ready-p annotated))))

(ert-deftest delib-flow-normalize-general-reference-note-item-rewrites-source-local-theme-focus ()
  (let* ((item (list :kind 'reference-note
                     :text "Create general PKM note for AI Readiness: Focus on Data Maturity"
                     :note-type 'general-pkm
                     :candidate-focus "AI Readiness: Focus on Data Maturity"
                     :candidate-identity "AI Readiness: Focus on Data Maturity"))
         (normalized (delib-flow--normalize-general-reference-note-item item)))
    (should (equal "Create general PKM note for Data Maturity for AI Readiness"
                   (plist-get normalized :text)))
    (should (equal "Data Maturity for AI Readiness"
                   (plist-get normalized :candidate-focus)))))

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

(ert-deftest delib-flow-source-title-action-rewrites-introspective-bullet-into-observable-work ()
  (let* ((package (list :source (list :title "Presentation about empathy"
                                      :content "* Presentation about empathy\n- Put myself in the user's shoes\n")
                        :working-context nil
                        :ui nil))
         (item (delib-flow--source-title-action package)))
    (should (equal "Draft interview questions about the user's shoes"
                   (plist-get item :text)))
    (should (string-match-p "rewritten from: Put myself in the user's shoes"
                            (plist-get item :evidence-summary)))))

(ert-deftest delib-flow-source-title-action-rewrites-put-self-variant-into-observable-work ()
  (let* ((package (list :source (list :title "Presentation about empathy"
                                      :content "* Presentation about empathy\n- Put self into end users' shoes for dashboards\n")
                        :working-context nil
                        :ui nil))
         (item (delib-flow--source-title-action package)))
    (should (equal "Draft interview questions about end users' shoes for dashboards"
                   (plist-get item :text)))))

(ert-deftest delib-flow-source-title-action-email-fallback-prefers-structural-focus-over-subject ()
  (let* ((package
          (list :source (list :title "Why metadata programs fail to become operational"
                              :content "* Why metadata programs fail to become operational\n")
                :working-context nil
                :ui nil))
         (digest
          (list :subject "Why metadata programs fail to become operational"
                :plain-body
                (string-join
                 '("This essay is about the gap between agreeing that metadata matters and actually using metadata in day-to-day delivery."
                   "A practical framing we liked:"
                   "Metadata maturity becomes real when it changes one handoff, one exception path, or one review ritual.")
                 "\n")))
         item)
    (cl-letf (((symbol-function 'delib-flow--package-email-digest)
               (lambda (_) digest)))
      (setq item (delib-flow--source-title-action package)))
    (should (equal
             "Draft checklist for one handoff, one exception path, or one review ritual"
             (plist-get item :text)))
    (should (string-match-p
             "source structural fallback"
             (plist-get item :evidence-summary)))))

(ert-deftest delib-flow-annotate-draft-actions-blocks-check-without-deliverable ()
  (let* ((item (list :kind 'next-action
                     :text "Check with design"
                     :source 'source))
         (annotated (car (delib-flow--annotate-draft-actions (list item)))))
    (should (delib-flow--draft-item-has-warning-code-p
             annotated 'exploratory-next-action))
    (should-not (delib-flow--draft-item-ready-p annotated))))

(ert-deftest delib-flow-annotate-draft-actions-blocks-stop-treating-variants ()
  (let* ((item (list :kind 'next-action
                     :text "Stop treating role as divorced from end users"
                     :source 'source))
         (annotated (car (delib-flow--annotate-draft-actions (list item)))))
    (should (delib-flow--draft-item-has-warning-code-p
             annotated 'introspective-next-action))
    (should-not (delib-flow--draft-item-ready-p annotated))))

(ert-deftest delib-flow-annotate-draft-actions-rewrites-linked-exploration-into-concrete-note ()
  (let* ((package (list :source (list :title "Empathy source"
                                      :content
                                      "* Empathy source\n- Look into [[id:empathetic-design][Empathetic design]]\n")
                        :working-context nil
                        :ui nil))
         (item (list :kind 'next-action
                     :text "Explore Empathetic design"
                     :source 'local-llm))
         (annotated (car (delib-flow--annotate-draft-actions (list item) package))))
    (should (equal "Draft applicability note for Empathetic design"
                   (plist-get annotated :text)))
    (should (equal "Explore Empathetic design"
                   (plist-get annotated :action-evidence-line)))
    (should (string-match-p "linked concept: Empathetic design"
                            (or (plist-get annotated :evidence-summary) "")))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 annotated 'exploratory-next-action))
    (should (delib-flow--draft-item-ready-p annotated))))

(ert-deftest delib-flow-annotate-draft-actions-blocks-question-shaped-prompts ()
  (let* ((item (list :kind 'next-action
                     :text "Understand why systems act: where does the user first lose the thread?"
                     :source 'local-llm))
         (annotated (car (delib-flow--annotate-draft-actions (list item)))))
    (should (delib-flow--draft-item-has-warning-code-p
             annotated 'question-shaped-next-action))
    (should-not (delib-flow--draft-item-ready-p annotated))))

(ert-deftest delib-flow-annotate-draft-actions-blocks-article-framing-items ()
  (let* ((item (list :kind 'next-action
                     :text "Design visible checkpoints without reintroducing bureaucracy"
                     :source 'local-llm))
         (annotated (car (delib-flow--annotate-draft-actions (list item)))))
    (should (or (delib-flow--draft-item-has-warning-code-p
                 annotated 'article-framing-action)
                (delib-flow--draft-item-has-warning-code-p
                 annotated 'strategic-theme-action)))
    (should-not (delib-flow--draft-item-ready-p annotated))))

(ert-deftest delib-flow-suppress-duplicate-weak-actions-keeps-strongest-representative ()
  (let* ((winner (list :kind 'next-action
                       :text "Draft interview questions about end-user dashboard workflows"
                       :action-evidence-line "Put myself into end users' shoes for how do we build dashboards"
                       :previous-evidence-line "Put myself into end users' shoes for how do we build dashboards"
                       :materially-different-from-previous-p t
                       :resolved-prior-warning-p nil
                       :warnings
                       (list (delib-flow--make-artifact-warning
                              'broad-action-scope
                              "Still broad."
                              'advisory))))
         (loser (list :kind 'next-action
                      :text "Put myself into end users' shoes for how do we build dashboards"
                      :action-evidence-line "Put myself into end users' shoes for how do we build dashboards"
                      :previous-evidence-line "Put myself into end users' shoes for how do we build dashboards"
                      :materially-different-from-previous-p nil
                      :resolved-prior-warning-p nil
                      :warnings
                      (list (delib-flow--make-artifact-warning
                             'introspective-next-action
                             "Still introspective."
                             'blocking))))
         (result (delib-flow--suppress-duplicate-weak-actions
                  (list winner loser)))
         (actions (plist-get result :actions))
         (suppressed (plist-get result :suppressed-candidates)))
    (should (= 1 (length actions)))
    (should (equal "Draft interview questions about end-user dashboard workflows"
                   (plist-get (car actions) :text)))
    (should (= 1 (length suppressed)))
    (should (equal "Put myself into end users' shoes for how do we build dashboards"
                   (plist-get (car suppressed) :text)))))

(ert-deftest delib-flow-normalize-extract-actions-output-surfaces-retry-truthfully ()
  (let* ((action (list :kind 'next-action
                       :text "Gather success cases with client satisfaction to showcase"
                       :source 'source))
         (text
          (delib-flow--normalize-extract-actions-output
           (list :candidate-count 1
                 :operator-intent ""
                 :project-context-kind 'none
                 :previous-attempt-count 0
                 :similar-to-previous-p t
                 :retry-context-present-p t
                 :retry-context (list :attempt-count 3)
                 :suppressed-candidate-count 2
                 :suppressed-candidates
                 (list (list :text "Put myself into end users' shoes"
                             :representative-text
                             "Gather success cases with client satisfaction to showcase"))
                 :warning-count 0
                 :warning-item-count 0
                 :blocking-warning-count 0
                 :blocking-warning-item-count 0
                 :actions (list action)))))
    (should (string-match-p "Prior attempts: 3" text))
    (should (string-match-p "Prior retry context available: yes" text))
    (should (string-match-p "Suppressed weak duplicates: 2" text))))

(ert-deftest delib-flow-prioritize-draft-items-demotes-approvable-risk-items ()
  (let* ((ready (list :kind 'next-action
                      :text "Draft launch checklist"
                      :warnings nil))
         (risky (list :kind 'waiting-for
                      :text "Waiting for confirmation from project owner"
                      :warnings
                      (list (delib-flow--make-artifact-warning
                             'waiting-for-speculative-owner
                             "Generic owner placeholder."
                             'advisory))))
         (ordered (delib-flow--prioritize-draft-items (list risky ready))))
    (should (equal "Draft launch checklist"
                   (plist-get (car ordered) :text)))
    (should (equal "Waiting for confirmation from project owner"
                   (plist-get (cadr ordered) :text)))))

(ert-deftest delib-flow-draft-item-readiness-text-flags-approvable-risk-items ()
  (let* ((warning (delib-flow--make-artifact-warning
                   'waiting-for-speculative-owner
                   "Uses project owner."
                   'advisory))
         (item (list :kind 'waiting-for
                     :text "Waiting for confirmation from project owner on dashboard review"
                     :warnings (list warning))))
    (should (string-match-p "approvable with 1 advisory warning"
                            (delib-flow--draft-item-readiness-text item)))))

(ert-deftest delib-flow-normalize-suggest-reference-notes-output-flags-source-local-queue ()
  (let* ((warning-a (delib-flow--make-artifact-warning
                     'reference-note-candidate-identity
                     "Still source-local."
                     'advisory))
         (warning-b (delib-flow--make-artifact-warning
                     'reference-note-reuse-justification
                     "Reuse is weak."
                     'advisory))
         (item (list :kind 'reference-note
                     :text "Create general PKM note for Putting Yourself in the End User's Shoes"
                     :note-type 'general-pkm
                     :warnings (list warning-a warning-b)))
         (text (delib-flow--normalize-suggest-reference-notes-output
                (list :candidate-count 1
                      :warning-count 2
                      :warning-item-count 1
                      :blocking-warning-count 0
                      :blocking-warning-item-count 0
                      :reference-notes (list item)))))
    (should (string-match-p "Queue quality: these note candidates are still source-local"
                            text))))

(ert-deftest delib-flow-reference-note-draft-summary-prefers-durable-general-pkm-framing ()
  (let* ((item (list :kind 'reference-note
                     :text "Create general PKM note for User-Centered Analytics Dashboards"
                     :note-type 'general-pkm
                     :reuse-claim
                     "Captures user-centered analytics dashboards as a reusable concept rather than a one-off source summary."))
         (summary (delib-flow--reference-note-draft-summary
                   item
                   (list :source (list :title "Presentation or article on focusing on the end user")
                         :working-context nil))))
    (should (string-match-p "reusable concept" summary))
    (should-not (string-match-p "This note captures" summary))))

(ert-deftest delib-flow-curate-reference-note-items-trims-weak-source-local-queue ()
  (let* ((items
          (list
           (list :kind 'reference-note
                 :text "Create general PKM note for Empathetic design overview"
                 :note-type 'general-pkm
                 :candidate-focus "Empathetic design overview"
                 :candidate-identity "Empathetic design overview"
                 :focus-score 4
                 :warnings
                 (list (delib-flow--make-artifact-warning
                        'reference-note-candidate-identity
                        "Source-local."
                        'advisory)
                       (delib-flow--make-artifact-warning
                        'reference-note-reuse-justification
                        "Needs reuse justification."
                        'advisory)))
           (list :kind 'reference-note
                 :text "Create general PKM note for Empathetic design in dashboards"
                 :note-type 'general-pkm
                 :candidate-focus "Empathetic design in dashboards"
                 :candidate-identity "Empathetic design in dashboards"
                 :focus-score 7
                 :warnings
                 (list (delib-flow--make-artifact-warning
                        'reference-note-candidate-identity
                        "Source-local."
                        'advisory)
                       (delib-flow--make-artifact-warning
                        'reference-note-reuse-justification
                        "Needs reuse justification."
                        'advisory)))
           (list :kind 'reference-note
                 :text "Create general PKM note for Focusing on the end user"
                 :note-type 'general-pkm
                 :candidate-focus "Focusing on the end user"
                 :candidate-identity "Focusing on the end user"
                 :focus-score 5
                 :warnings
                 (list (delib-flow--make-artifact-warning
                        'reference-note-candidate-identity
                        "Source-local."
                        'advisory)
                       (delib-flow--make-artifact-warning
                        'reference-note-reuse-justification
                        "Needs reuse justification."
                       'advisory)))))
         (curated (delib-flow--curate-reference-note-items items))
         (titles (mapcar #'delib-flow--reference-note-title curated)))
    (should (= 1 (length curated)))
    (should (equal '("Empathetic design in dashboards") titles))))

(ert-deftest delib-flow-introspective-next-action-detects-no-longer-treat-variants ()
  (should (delib-flow--introspective-next-action-p
           "No longer treat my role as divorced from actual analytics dashboards"))
  (should (delib-flow--introspective-next-action-p
           "Treat role as not divorced from analytics dashboards")))

(ert-deftest delib-flow-drafted-action-item-recalculates-warnings-from-rewritten-text ()
  (let* ((candidate
          (list :kind 'next-action
                :text "Check with business stakeholders before handing devs instructions"
                :warnings
                (list (delib-flow--make-artifact-warning
                       'exploratory-next-action
                       "Reads like open-ended checking or exploration without naming the concrete deliverable."
                       'blocking)
                      (delib-flow--make-artifact-warning
                       'weak-next-action-verb
                       "Starts with check."
                       'blocking))))
         (package
          (delib-flow--set-artifact-family-state
           (list :source (list :title "Source"
                               :content "* Source\n+ Check with stakeholders\n")
                 :working-context nil
                 :artifacts nil)
           'actions
           (list :candidates (list candidate)
                 :selected-candidate-id
                 (delib-flow--artifact-candidate-id candidate)
                 :selected-draft nil
                 :draft-history nil)))
         (draft
          (delib-flow--drafted-action-item
           candidate
           package
           "Prepare stakeholder review agenda for dashboard requirements"
           "Tightened to a named deliverable.")))
    (should (equal "Prepare stakeholder review agenda for dashboard requirements"
                   (plist-get draft :text)))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 draft 'exploratory-next-action))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 draft 'weak-next-action-verb))))

(ert-deftest delib-flow-drafted-action-item-escalates-to-structural-rewrite-after-repeated-weak-drafts ()
  (let* ((candidate
          (list :kind 'next-action
                :text "Check with end users/business stakeholders before handing devs instructions"
                :warnings
                (list (delib-flow--make-artifact-warning
                       'exploratory-next-action
                       "Reads like open-ended checking or exploration without naming the concrete deliverable."
                       'blocking)
                      (delib-flow--make-artifact-warning
                       'weak-next-action-verb
                       "Starts with check."
                       'blocking))))
         (package
          (delib-flow--set-artifact-family-state
           (list :source (list :title "Source"
                               :content "* Source\n+ Check with end users/business stakeholders before handing devs instructions\n")
                 :working-context nil
                 :artifacts nil)
           'actions
           (list :candidates (list candidate)
                 :selected-candidate-id
                 (delib-flow--artifact-candidate-id candidate)
                 :selected-draft candidate
                 :draft-history (list (copy-tree candidate)
                                      (copy-tree candidate)
                                      (copy-tree candidate)))))
         (draft
          (delib-flow--drafted-action-item
           candidate
           package
           "Check with stakeholders"
           "Tightened wording.")))
    (should (equal "Prepare stakeholder review agenda for stakeholders"
                   (plist-get draft :text)))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 draft 'exploratory-next-action))))

(ert-deftest delib-flow-drafted-action-item-tightens-review-and-extract-into-single-deliverable ()
  (let* ((candidate
          (list :kind 'next-action
                :text "Gather success cases with client satisfaction to showcase"))
         (package
          (list :source (list :title "End-user presentation"
                              :content "* End-user presentation\n")
                :working-context nil))
         (draft
          (delib-flow--drafted-action-item
           candidate
           package
           "Review and extract success cases with client satisfaction to demonstrate empathetic design in action"
           "Tightened wording.")))
    (should (equal "Draft list of success cases with client satisfaction"
                   (plist-get draft :text)))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 draft 'weak-next-action-verb))
    (should-not (delib-flow--draft-item-has-warning-code-p
                 draft 'broad-action-scope))))

(ert-deftest delib-flow-drafted-action-item-tightens-concept-map-summary-artifacts ()
  (let* ((candidate
          (list :kind 'next-action
                :text "Review reversible workflows"))
         (package
          (list :source (list :title "Trust signals newsletter"
                              :content "* Trust signals newsletter\n")
                :working-context nil))
         (draft
          (delib-flow--drafted-action-item
           candidate
           package
           "Draft a concept map illustrating the benefits of reversible workflows, highlighting user trust and visible process as key factors"
           "Tightened wording.")))
    (should (equal "Draft checklist for the benefits of reversible workflows, highlighting user trust and visible process as key factors"
                   (plist-get draft :text)))))

(ert-deftest delib-flow-reference-note-draft-body-with-seed-ignores-seeded-label-lines ()
  (let* ((item (list :kind 'reference-note
                     :text "Create general PKM note for Empathetic design"
                     :note-type 'general-pkm))
         (package
          (list :source
                (list :title "Empathy source"
                      :content "* Empathy source\nLook into Empathetic design.\nEmpathetic design helps teams map workflows to user outcomes.\n")
                :working-context nil))
         (body
          (delib-flow--reference-note-draft-body-with-seed
           (string-join
            '("* Working draft"
              "Empathetic design helps teams map workflows to user outcomes."
              "- Durable claim: Durable claim: Look into Empathetic design"
              "- Why it matters: Durable claim: Look into Empathetic design"
              "- Reuse angle: Keep this reuse angle.")
            "\n")
           item
           package)))
    (should (string-match-p
             "Empathetic design helps teams map workflows to user outcomes"
             body))
    (should-not (string-match-p
                 "Why it matters: Durable claim:"
                 body))
    (should-not (string-match-p
                 "Durable claim: Durable claim:"
                 body))))

(ert-deftest delib-flow-reference-note-draft-body-with-seed-removes-template-summary-prose ()
  (let* ((item (list :kind 'reference-note
                     :text "Create general PKM note for Empathetic design and its application to dashboard building"
                     :note-type 'general-pkm))
         (package
          (list :source
                (list :title "Empathy source"
                      :content "* Empathy source\nLook into Empathetic design.\nEmpathetic design helps teams map workflows to user outcomes.\n")
                :working-context nil))
         (body
          (delib-flow--reference-note-draft-body-with-seed
           (string-join
            '("* Working draft"
              "Empathetic design and its application to dashboard building is a reusable framing for end-user-centered design, workflow, or decision work."
              "It offers a durable concept for comparing adjacent decisions, workflows, or patterns."
              "- Durable claim: Empathetic design and its application to dashboard building is a reusable framing for end-user-centered design, workflow, or decision work."
              "- Why it matters: It offers a durable concept for comparing adjacent decisions, workflows, or patterns."
              "- Reuse angle: Reuse this when you need a durable concept for comparing adjacent decisions, workflows, or patterns.")
            "\n")
           item
           package)))
    (should-not (string-match-p "reusable framing" body))
    (should (string-match-p
             "Empathetic design and its application to dashboard building can inform related workflow, design, or decision work\\."
             body))
    (should (string-match-p
             "- Durable claim: Empathetic design helps teams map workflows to user outcomes"
             body))
    (should (string-match-p
             "- Why it matters: Use this to keep related notes, workflows, or decisions aligned without restating the source each time\\."
             body))))

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

(ert-deftest delib-flow-email-digest-reference-note-focuses-unwrap-soft-wrapped-idea-lines ()
  (let* ((digest
          '(:subject "Your Consumption Diet Is Your Moat"
            :type-hint "newsletter or mailing list"
            :plain-body
            "The idea: instead of a generic chatbot, you create a named\nadvisor with a defined role, a clear scope, and the personal\ncontext to give advice that fits your life.\n"))
         (focuses (delib-flow--email-digest-reference-note-focuses digest)))
    (should (member
             "Instead of a generic chatbot, you create a named advisor with a defined role, a clear scope, and the personal context to give advice that fits your life"
             focuses))
    (should-not (seq-some
                 (lambda (focus)
                   (string-match-p "\\`The idea:" focus))
                 focuses))
    (should-not (seq-some
                 (lambda (focus)
                   (string-suffix-p "create a named" focus))
                 focuses))))

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
    (should (string-match-p
             "Building personal AI advisors can inform related workflow, design, or decision work"
             content))
    (should-not (string-match-p "This note captures Building personal AI advisors"
                                content))
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
