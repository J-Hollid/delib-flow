;;; delib-flow.el --- Guided AI workflow control for Org -*- lexical-binding: t; -*-
;; Author: Jordan Holliday

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, ai
;; URL: https://example.invalid/delib-flow

;;; Commentary:

;; delib-flow provides a guided control buffer for deliberate AI-assisted
;; workflow execution from an Org heading at point.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'pp)
(require 'seq)
(require 'subr-x)

(defgroup delib-flow nil
  "Guided AI workflow control for Org."
  :group 'tools
  :prefix "delib-flow-")

(defcustom delib-flow-my-projects-file nil
  "Path to the authoritative My Projects Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-zk-root nil
  "Path to the searchable zettelkasten root."
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom delib-flow-prompt-library-file nil
  "Path to the prompt library Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-example-structures-file nil
  "Path to the example structures Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-audit-log-file nil
  "Path to the audit log Org file."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-inbox-file nil
  "Path to the inbox Org file used by `delib-flow-start-from-inbox'."
  :type '(choice (const :tag "Unset" nil) file))

(defcustom delib-flow-inbox-outline-path nil
  "Outline path inside `delib-flow-inbox-file' whose child entries form the inbox queue.

When nil, `delib-flow-start-from-inbox' offers top-level headings from the file.
When non-nil, it must be a list of heading titles such as '(\"Inbox\") or
'(\"Capture\" \"Inbox\"), and the picker will offer the direct child headings
under that node."
  :type '(choice (const :tag "Top-level headings" nil)
                 (repeat string)))

(defcustom delib-flow-audit-payload-policy 'full
  "Policy for retaining detailed stage payloads in the audit log.

`full' persists input packages and raw outputs as-is.
`redacted' persists payloads after deterministic string redaction.
`metadata-only' omits payload bodies and keeps only stage metadata."
  :type '(choice (const :tag "Full payloads" full)
                 (const :tag "Redacted payloads" redacted)
                 (const :tag "Metadata only" metadata-only)))

(defcustom delib-flow-audit-redaction-profile 'strict
  "Deterministic redaction profile used when audit payload policy is `redacted'."
  :type '(choice (const :tag "Standard" standard)
                 (const :tag "Strict" strict)))

(defcustom delib-flow-general-note-template
  "#+title: ${title}\n#+filetags: :delib-flow:reference:\n\n- Filed from delib-flow\n- Source artifact: ${source-artifact}\n"
  "Template used for deterministic general PKM note creation.

Supported placeholders are `${title}', `${source-artifact}', and `${note-type}'."
  :type 'string)

(defcustom delib-flow-project-support-note-template
  "#+title: ${title}\n#+filetags: :project:support:\n\n- Filed from delib-flow\n- Source artifact: ${source-artifact}\n"
  "Template used for deterministic project support note creation.

Supported placeholders are `${title}', `${source-artifact}', and `${note-type}'."
  :type 'string)

(defcustom delib-flow-next-action-capture-template
  "%(delib-flow-capture-project-child-heading)\n"
  "Org capture pattern used when filing next-action items into a matched project.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-waiting-for-capture-template
  "%(delib-flow-capture-project-child-heading)\n"
  "Org capture pattern used when filing waiting-for items into a matched project.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-project-capture-template
  "%(delib-flow-capture-project-heading)\n%(delib-flow-capture-project-tags-property)%(delib-flow-capture-project-first-item-heading)\n"
  "Org capture pattern used when filing new project proposals.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-general-note-capture-template
  "#+title: %(delib-flow-capture-note-title)\n#+filetags: :delib-flow:reference:\n\n- Filed from delib-flow\n- Source artifact: %(delib-flow-capture-source-artifact)\n"
  "Org capture pattern used when filing general PKM notes.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-project-support-note-capture-template
  "#+title: %(delib-flow-capture-note-title)\n#+filetags: :project:support:\n\n- Filed from delib-flow\n- Source artifact: %(delib-flow-capture-source-artifact)\n"
  "Org capture pattern used when filing project-support notes.

This template must be non-interactive.  Supported dynamic fields are best
accessed through `%(...)' forms that call `delib-flow-capture-*' helper
functions."
  :type 'string)

(defcustom delib-flow-general-note-org-roam-capture-key nil
  "Org-roam capture template key used when filing general PKM notes.

When non-nil, reference-note filing resolves the target path and staged
content from the matching `org-roam-capture-templates' entry instead of the
package-local deterministic note template."
  :type '(choice (const :tag "Disabled" nil) string))

(defcustom delib-flow-project-support-note-org-roam-capture-key nil
  "Org-roam capture template key used when filing project-support notes.

When non-nil, reference-note filing resolves the target path and staged
content from the matching `org-roam-capture-templates' entry instead of the
package-local deterministic note template."
  :type '(choice (const :tag "Disabled" nil) string))

(defvar org-roam-directory nil
  "Org-roam root directory.

Declared here so delib-flow can safely interact with org-roam in batch
contexts before org-roam itself has been loaded.")

(defcustom delib-flow-default-local-model nil
  "Default local model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-default-cloud-model nil
  "Default cloud model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defconst delib-flow-debug-buffer-name "*delib-flow-debug*"
  "Name of the debug inspection buffer.")

(defconst delib-flow-debug-walkthrough-buffer-name "*delib-flow-walkthrough*"
  "Name of the debug walkthrough buffer.")

(defconst delib-flow-debug-scenarios
  '((alpha-followup
     :label "Alpha follow-up"
     :source-title "Alpha Project kickoff"
     :source-content "* Alpha Project kickoff\nFrom: alice@example.com\nSubject: Alpha Project kickoff\n\nNext steps:\n- Draft kickoff follow-up\n- Prepare timeline update\nWaiting for Bob to confirm the launch date.\n[[file:notes/alpha-brief.org][Alpha brief]]\n"
     :projects-content "* Alpha Project\n:PROPERTIES:\n:CONTACTS: alice@example.com\n:TAGS: alpha kickoff\n:END:\n** TODO Prepare launch checklist\n"
     :zk-files (("notes/alpha-brief.org"
                 . "#+title: Alpha brief\n#+filetags: :alpha:\n\n- Kickoff context\n"))
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready cloud-ready
                                    cloud-failure-ready filing-ready))
    (manual-project-override
     :label "Manual project override"
     :source-title "Completely Different Topic"
     :source-content "* Completely Different Topic\nAgenda:\n- Clarify whether this belongs to Alpha or Beta work.\n"
     :projects-content "* Alpha Project\n* Beta Project\n"
     :zk-files nil
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    manual-project-ready))
    (contact-disambiguates
     :label "Contact disambiguates"
     :source-title "Budget follow-up"
     :source-content "* Budget follow-up\nFrom: alice@example.com\nSubject: Need updated budget numbers\n\nPlease send the revised budget and confirm whether the draft can go out today.\n"
     :projects-content "* Alpha Project\n:PROPERTIES:\n:CONTACTS: alice@example.com\n:TAGS: alpha finance budget\n:END:\n** TODO Send revised budget\n* Beta Project\n:PROPERTIES:\n:CONTACTS: bob@example.com\n:TAGS: beta finance budget\n:END:\n** TODO Review legal terms\n"
     :zk-files nil
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready cloud-ready
                                    cloud-failure-ready filing-ready))
    (linked-note-disambiguates
     :label "Linked note disambiguates"
     :source-title "Open questions from note review"
     :source-content "* Open questions from note review\nNeed to reconcile the support note with the current project state.\nPlease review the open blockers and dependencies.\n[[file:notes/beta-brief.org][Beta brief]]\n"
     :projects-content "* Alpha Project\n:PROPERTIES:\n:TAGS: alpha support\n:END:\nSee [[file:notes/alpha-plan.org][Alpha plan]]\n** TODO Review launch plan\n* Beta Project\n:PROPERTIES:\n:TAGS: beta support blockers\n:END:\nSee [[file:notes/beta-brief.org][Beta brief]]\n** TODO Reconcile support note\n"
     :zk-files (("notes/beta-brief.org"
                 . "#+title: Beta brief\n#+filetags: :beta:\n\n- Current blockers\n- Follow up with operations\n")
                ("notes/alpha-plan.org"
                 . "#+title: Alpha plan\n#+filetags: :alpha:\n\n- Launch plan draft\n"))
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready cloud-ready
                                    cloud-failure-ready filing-ready))
    (no-match-clean
     :label "No-match clean"
     :source-title "Housekeeping reminder"
     :source-content "* Housekeeping reminder\nAgenda:\n- Clean up inbox rules.\n- Archive old receipts.\n- Refile travel paperwork.\n"
     :projects-content "* Alpha Project\n* Beta Project\n"
     :zk-files nil
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    manual-project-ready))
    (action-rich-midpoint
     :label "Action-rich midpoint"
     :source-title "Q3 launch coordination"
     :source-content "* Q3 launch coordination\nNeed an operator pass over launch prep before the external update goes out.\nPlease confirm what still needs to ship this week.\n"
     :projects-content "* Launch Project\n:PROPERTIES:\n:CONTACTS: launch@example.com\n:TAGS: launch rollout q3\n:END:\n** TODO Confirm external launch status\n"
     :zk-files (("notes/launch-brief.org"
                 . "#+title: Launch brief\n#+filetags: :launch:\n\n- TODO Draft vendor follow-up email\n- TODO Publish updated launch timeline\n- Waiting for legal to confirm revised terms.\n")
                ("notes/launch-risks.org"
                 . "#+title: Launch risks\n#+filetags: :launch:risk:\n\n- Blocker: staging access for vendor still pending.\n- Decision: keep Friday launch target unless legal slips.\n"))
     :context-override
     (:retrieved-candidates
      ((:title "Launch brief"
        :path "notes/launch-brief.org"
        :score 7
        :reasons ("debug-midpoint-action-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture"))
       (:title "Launch risks"
        :path "notes/launch-risks.org"
        :score 6
        :reasons ("debug-midpoint-blocker-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture")))
      :retained-context
      "- Concrete next step: draft the vendor follow-up email and publish the updated launch timeline.\n- Open blocker: staging access for the vendor is still pending.\n- Waiting point: legal must confirm revised terms before the external update.\n- Active decision: keep the Friday launch target unless legal slips.")
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready filing-ready))
    (waiting-rich-midpoint
     :label "Waiting-rich midpoint"
     :source-title "Procurement handoff follow-up"
     :source-content "* Procurement handoff follow-up\nCheck which external confirmations are still outstanding before the contract packet moves forward.\n"
     :projects-content "* Procurement Project\n:PROPERTIES:\n:CONTACTS: ops@example.com\n:TAGS: procurement approvals vendor\n:END:\n** TODO Advance contract packet\n"
     :zk-files (("notes/vendor-approvals.org"
                 . "#+title: Vendor approvals\n#+filetags: :procurement:\n\n- Waiting for finance to approve the revised amount.\n- Waiting for legal to confirm the indemnity clause.\n- Waiting for vendor contact to return the signed schedule.\n")
                ("notes/dependencies.org"
                 . "#+title: Procurement dependencies\n#+filetags: :procurement:\n\n- Blocker: packet cannot move until finance and legal approve.\n- Contact: ops@example.com is coordinating the handoff.\n"))
     :context-override
     (:retrieved-candidates
      ((:title "Vendor approvals"
        :path "notes/vendor-approvals.org"
        :score 7
        :reasons ("debug-midpoint-waiting-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture"))
       (:title "Procurement dependencies"
        :path "notes/dependencies.org"
        :score 5
        :reasons ("debug-midpoint-dependency-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture")))
      :retained-context
      "- Waiting for finance to approve the revised amount.\n- Waiting for legal to confirm the indemnity clause.\n- Waiting for the vendor contact to return the signed schedule.\n- Packet cannot move until finance and legal approve.")
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready filing-ready))
    (reference-note-rich-midpoint
     :label "Reference-note-rich newsletter"
     :source-title "Four leverage patterns for working with AI tools"
     :source-content "* Four leverage patterns for working with AI tools\nFrom: Signal Workshop <editor@example-demo.org>\nSubject: Four leverage patterns for working with AI tools\nDate: Mon, 04 May 2026 09:00:00 +0000\n\nThis week we noticed that teams improve fastest when they stop asking for bigger prompts and instead improve the system around the prompt.\n\n1. Named advisors beat generic chatbots\nGive the model a stable role, a clear scope, and examples from your actual work. A release coach that knows your launch checklist produces better output than a blank chat box.\n\n2. Capture friction logs, not just answers\nKeep a running list of where the model confuses domain terms, misses context, or proposes brittle steps. Friction logs turn one-off annoyances into reusable system improvements.\n\n3. Write handoff packets before you need them\nThe best prompt is often a reusable packet: goal, constraints, examples, definitions, and what good output looks like. Teams that prepare handoff packets recover faster when work changes hands.\n\n4. Save examples as assets\nWhen the model produces a great summary, critique, or checklist, save it with the source context and why it worked. Reusable examples compound faster than isolated prompts.\n\nLinks:\n- https://example-demo.org/ai-workflow-patterns\n- https://example-demo.org/handoff-packets\n"
     :projects-content "* Balcony tomato watering rota\n* Summer train trip packing list\n* Bread baking experiments\n"
     :zk-files (("notes/advisor-briefs.org"
                 . "#+title: Advisor briefs as reusable interfaces\n#+filetags: :ai:workflow:\n\n- A named advisor works best when it has a stable role, a clear scope, and concrete examples.\n- Reusable advisor briefs reduce the need to restate context every time.\n")
                ("notes/friction-logs.org"
                 . "#+title: Friction logs for AI systems\n#+filetags: :ai:feedback:\n\n- Log recurring model mistakes, missing context, and brittle suggestions.\n- Review friction logs weekly to improve prompts, definitions, and examples.\n")
                ("notes/handoff-packets.org"
                 . "#+title: Prompt handoff packets\n#+filetags: :ai:operations:\n\n- A good handoff packet includes goals, constraints, examples, definitions, and success criteria.\n- Handoff packets make delegated AI work easier to inspect and reuse.\n"))
     :context-override
     (:retrieved-candidates
      ((:title "Advisor briefs as reusable interfaces"
        :path "notes/advisor-briefs.org"
        :score 8
        :reasons ("debug-midpoint-advisor-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture"))
       (:title "Friction logs for AI systems"
        :path "notes/friction-logs.org"
        :score 7
        :reasons ("debug-midpoint-friction-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture"))
       (:title "Prompt handoff packets"
        :path "notes/handoff-packets.org"
        :score 7
        :reasons ("debug-midpoint-handoff-source")
        :filter-status retained
        :filter-reasons ("retained-by-debug-midpoint-fixture")))
      :retained-context
      "- Durable idea: named advisors outperform generic chat sessions when they have a stable role, scope, and examples.\n- Durable idea: friction logs convert recurring model failures into reusable system improvements.\n- Durable idea: handoff packets preserve goals, constraints, examples, and success criteria so AI work survives transitions.\n- Durable idea: high-quality outputs become reusable assets when saved with context and reasons they worked.")
     :project-override
     (:match-status no-match
      :best-project nil
      :candidates nil
      :reason "No existing project matches this public newsletter fixture. Continue with non-project reference-note extraction.")
     :filing-override
     (:draft-items
      ((:kind reference-note
        :text "Create general PKM note for Advisor briefs as reusable interfaces"
        :note-type general-pkm)
       (:kind reference-note
        :text "Create general PKM note for Friction logs for AI systems"
        :note-type general-pkm)
       (:kind reference-note
        :text "Create general PKM note for Prompt handoff packets"
        :note-type general-pkm)))
     :artifact-stage suggest-reference-notes
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready filing-ready))
    (filing-selection-mixed
     :label "Filing selection mixed"
     :source-title "Launch filing triage"
     :source-content "* Launch filing triage\nReview the launch follow-up queue and separate approval-ready items from draft artifacts that still need work.\n"
     :projects-content "* Launch Project\n:PROPERTIES:\n:CONTACTS: launch@example.com\n:TAGS: launch rollout q3\n:END:\n** TODO Coordinate launch follow-up\n"
     :zk-files nil
     :context-override
     (:retrieved-candidates nil
      :retained-context
      "- Ready artifact: publish the updated launch timeline.\n- Risky artifact: keep the Friday launch target.\n- Support note candidate: capture the vendor support handoff.\n- General note candidate: capture the rollback checklist pattern.")
     :filing-override
     (:draft-items
      ((:kind next-action
        :text "Publish updated launch timeline")
       (:kind next-action
        :text "Keep Friday launch target")
       (:kind reference-note
        :text "Create project support note from Vendor support handoff"
        :note-type project-support)
       (:kind reference-note
        :text "Create general PKM note for Rollback checklist pattern"
        :note-type general-pkm)))
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready filing-ready))
    (filing-note-conflict
     :label "Filing note conflict"
     :source-title "Migration support filing review"
     :source-content "* Migration support filing review\nPrepare the migration support handoff note and verify the deterministic filing path before writing it.\n"
     :projects-content "* Migration Project\n:PROPERTIES:\n:CONTACTS: infra@example.com\n:TAGS: migration infra review\n:END:\n** TODO Capture migration review outcomes\n"
     :zk-files (("migration-support-handoff.org"
                 . "#+title: Migration support handoff\n\nExisting deterministic target to force note conflict.\n"))
     :context-override
     (:retrieved-candidates nil
      :retained-context
      "- Project support note candidate: migration support handoff.\n- Concrete next step: update the migration handoff checklist.")
     :filing-override
     (:draft-items
      ((:kind reference-note
        :text "Create project support note from Migration support handoff"
        :note-type project-support)
       (:kind next-action
        :text "Update migration handoff checklist")))
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready filing-ready
                                    filing-conflict-ready))
    (filing-conflict
     :label "Filing conflict"
     :source-title "Alpha Project kickoff"
     :source-content "* Alpha Project kickoff\nFrom: alice@example.com\nSubject: Alpha Project kickoff\n\nNext steps:\n- Draft kickoff follow-up\n- Prepare timeline update\n"
     :projects-content "* Alpha Project\n:PROPERTIES:\n:CONTACTS: alice@example.com\n:TAGS: alpha kickoff\n:END:\n** TODO Draft kickoff follow-up\n"
     :zk-files nil
     :supported-checkpoints (source inspect-reviewed project-reviewed
                                    context-ready artifact-ready filing-ready
                                    filing-conflict-ready)))
  "Built-in debug scenarios for local verification.

Each entry is keyed by scenario id and provides fixture content for a local
debug run.")

(defconst delib-flow-debug-checkpoints
  '((source . "Source only")
    (inspect-reviewed . "Inspect reviewed")
    (project-reviewed . "Project reviewed")
    (manual-project-ready . "Manual project ready")
    (context-ready . "Context ready")
    (artifact-ready . "Artifact ready")
    (cloud-ready . "Cloud ready")
    (cloud-failure-ready . "Cloud failure ready")
    (filing-conflict-ready . "Filing conflict ready")
    (filing-ready . "Filing ready"))
  "Named debug checkpoints for replaying local verification paths.")

(defconst delib-flow-debug-walkthrough-targets
  '((full-run
     :label "Full run from source"
     :scenario alpha-followup
     :checkpoint source
     :steps (source inspect-reviewed project-reviewed context-ready
                    artifact-ready filing-ready)
     :checks ((source
               :expect "The run should begin from the raw source snapshot with no completed stage history yet.")
              (inspect-reviewed
               :expect "Inspect Source should be the latest stage and its review state should be accepted.")
              (project-reviewed
               :expect "Match Project should be accepted and the selected project should be Alpha Project.")
              (context-ready
               :expect "Retained context should include the Alpha brief plus the accepted source context.")
              (artifact-ready
               :expect "Extract Actions should draft actionable kickoff follow-up artifacts before filing.")
              (filing-ready
               :expect "Integrated context should be present while draft filing artifacts remain available."))
     :objective "Walk the local path end to end from the source snapshot.")
    (project-review
     :label "Project review"
     :scenario alpha-followup
     :checkpoint project-reviewed
     :steps (inspect-reviewed project-reviewed)
     :checks ((inspect-reviewed
               :expect "Inspect Source should already be accepted before project review begins.")
              (project-reviewed
               :expect "Match Project should confirm Alpha Project as the best candidate and latest stage."))
     :objective "Inspect accepted source typing and project-match output.")
    (manual-project
     :label "Manual project override"
     :scenario manual-project-override
     :checkpoint manual-project-ready
     :steps (inspect-reviewed project-reviewed manual-project-ready)
     :checks ((inspect-reviewed
               :expect "Inspect Source should already be accepted so project matching can be evaluated.")
              (project-reviewed
               :expect "Match Project should leave the run ready for manual override rather than auto-accepting a project.")
              (manual-project-ready
               :expect "The manual project selection block should be seeded with candidates and reject-all guidance."))
     :objective "Verify ambiguous project selection and manual override flow.")
    (artifact-drafting
     :label "Artifact drafting"
     :scenario alpha-followup
     :checkpoint artifact-ready
     :steps (project-reviewed context-ready artifact-ready)
     :checks ((project-reviewed
               :expect "Project review should already be accepted before context gathering begins.")
              (context-ready
               :expect "Reference discovery and filtering should leave a narrowed retained-context set.")
              (artifact-ready
               :expect "Extract Actions should produce draft artifacts with warning and readiness metadata."))
     :objective "Inspect draft artifact generation before filing or cloud.")
    (newsletter-reference-notes
     :label "Newsletter reference notes"
     :scenario reference-note-rich-midpoint
     :checkpoint inspect-reviewed
     :steps (inspect-reviewed project-reviewed context-ready artifact-ready)
     :checks ((inspect-reviewed
               :expect "Inspect Source should classify the imported newsletter cleanly before any project decision or retrieval work.")
              (project-reviewed
               :expect "Match Project should stay at an accepted no-match result so the operator can continue the non-project PKM flow.")
              (context-ready
               :expect "Retained context should surface adjacent durable concepts like advisor briefs, friction logs, and handoff packets.")
              (artifact-ready
               :expect "Suggest Reference Notes should draft multiple understandable reference-note candidates from the newsletter without requiring a project match."))
     :objective "Demonstrate a public-safe newsletter-to-PKM flow with no project match.")
    (cloud-failure
     :label "Cloud failure"
     :scenario alpha-followup
     :checkpoint cloud-failure-ready
     :steps (project-reviewed cloud-ready cloud-failure-ready)
     :checks ((project-reviewed
               :expect "Project review should already be accepted before entering the cloud corridor.")
              (cloud-ready
               :expect "The reviewed cloud package should be approved with reintegration still pending.")
              (cloud-failure-ready
               :expect "The failure should be recorded against Extract Actions with retry, local, and skip recovery paths."))
     :objective "Verify rerouted cloud failure capture and fallback handling.")
    (filing-conflict
     :label "Filing conflict"
     :scenario filing-conflict
     :checkpoint filing-conflict-ready
     :steps (project-reviewed context-ready artifact-ready filing-ready
                              filing-conflict-ready)
     :checks ((project-reviewed
               :expect "Project review should already be accepted before drafting and filing begin.")
              (context-ready
               :expect "Retained context should be stable before artifact drafting proceeds.")
              (artifact-ready
               :expect "Draft artifacts should exist and be ready for filing selection review.")
              (filing-ready
               :expect "One artifact should be approved for filing before deterministic writes run.")
              (filing-conflict-ready
               :expect "The conflict should be surfaced with a seeded resolution block and retry options."))
     :objective "Verify deterministic conflict detection and conflict review."))
  "Named walkthrough targets for local verification.")

(defcustom delib-flow-cloud-reroutable-stage-ids
  '(run-cloud-stage filter-reference-material extract-actions
                    extract-waiting-for suggest-reference-notes)
  "Stage identifiers that may be executed through the reviewed cloud path.

Entries must be stage ids present in `delib-flow--stage-descriptor-alist'."
  :type '(repeat symbol))

(defcustom delib-flow-cloud-policy-profile 'standard
  "Default cloud sanitization policy profile.

Supported profiles are `standard' and `strict'."
  :type '(choice (const :tag "Standard" standard)
                 (const :tag "Strict" strict)))

(defcustom delib-flow-cloud-provider-policy-alist
  '((default :enabled t :policy-profile standard))
  "Provider-specific cloud policy configuration.

Each entry is keyed by a provider name string or the symbol `default' and may
include `:enabled' and `:policy-profile' properties."
  :type 'sexp)

(defcustom delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter
  "Function used to execute local stages.

The function receives a stage descriptor and an assembled input package,
and returns raw stage output."
  :type 'function)

(defcustom delib-flow-local-stage-async-adapter nil
  "Optional function used to execute local stages asynchronously.

The function receives a stage descriptor, an assembled input package, an
ON-SUCCESS callback, and an ON-ERROR callback. ON-SUCCESS receives the
raw stage output. ON-ERROR receives a user-facing error string. The
adapter may return an opaque handle such as a process object."
  :type '(choice (const :tag "Disabled" nil) function))

(defcustom delib-flow-cloud-stage-adapter #'delib-flow--default-cloud-stage-adapter
  "Function used to execute cloud stages.

The function receives a stage descriptor and an assembled input package,
and returns raw stage output."
  :type 'function)

(defconst delib-flow-control-buffer-name "*delib-flow*"
  "Name of the main delib-flow control buffer.")

(defvar delib-flow--active-run nil
  "Active run state for the current delib-flow session.")

(defvar delib-flow--in-flight-ui-timer nil
  "Timer used to refresh async in-flight cockpit indicators.")

(defconst delib-flow--spinner-frames ["|" "/" "-" "\\"]
  "Frames used for simple in-flight cockpit spinners.")

(defconst delib-flow--action-shortcut-keys
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
    "b" "c" "d" "e" "f" "h" "i" "k" "l"
    "o" "t" "u" "v" "w" "x" "y")
  "Single-key shortcuts reserved for rendered actions.")

(defvar-local delib-flow-control-focus-mode nil
  "Whether the current control buffer is in narrow-screen focus mode.")

(defvar-local delib-flow--changed-heading-overlay nil
  "Transient overlay highlighting the last changed control-buffer heading.")

(defvar delib-flow--sticky-anchor-heading nil
  "Preferred control-buffer anchor preserved across stage-lifecycle rerenders.")

(defconst delib-flow--artifact-family-keys
  '(actions waiting-fors reference-notes project-proposals)
  "Artifact families that support candidate and selected-draft state.")

(defvar delib-flow-control-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'delib-flow-control-refresh)
    (define-key map (kbd "j") #'delib-flow-control-open-audit-run)
    (define-key map (kbd "J") #'delib-flow-control-open-audit-latest-stage)
    (define-key map (kbd "D") #'delib-flow-control-debug-open-latest-stage-inspection)
    (define-key map (kbd "C") #'delib-flow-control-debug-open-comparison)
    (define-key map (kbd "W") #'delib-flow-control-debug-open-walkthrough)
    (define-key map (kbd "H") #'delib-flow-control-debug-apply-helper)
    (define-key map (kbd "N") #'delib-flow-control-debug-walkthrough-next-step)
    (define-key map (kbd "R") #'delib-flow-control-debug-walkthrough-restart-target)
    (define-key map (kbd "n") #'delib-flow-control-next-section)
    (define-key map (kbd "p") #'delib-flow-control-previous-section)
    (define-key map (kbd "TAB") #'delib-flow-control-next-action)
    (define-key map (kbd "<backtab>") #'delib-flow-control-previous-action)
    (define-key map (kbd "RET") #'delib-flow-control-dispatch-action)
    (define-key map (kbd "a") #'delib-flow-control-dispatch-action)
    (define-key map (kbd ".") #'delib-flow-control-context-menu)
    (define-key map (kbd "A") #'delib-flow-control-approve-current)
    (define-key map (kbd "P") #'delib-flow-control-peek-filing-target)
    (define-key map (kbd "V") #'delib-flow-control-peek-staged-content)
    (define-key map (kbd "L") #'delib-flow-control-jump-active-loop)
    (define-key map (kbd "K") #'delib-flow-control-jump-latest-preview)
    (define-key map (kbd "U") #'delib-flow-control-jump-stage-history)
    (define-key map (kbd "m") #'delib-flow-control-choose-manual-project)
    (define-key map (kbd "r") #'delib-flow-control-retry-current)
    (define-key map (kbd "q") #'delib-flow-control-abort-run)
    (define-key map (kbd "s") #'delib-flow-control-choose-filing-selection)
    (define-key map (kbd "T") #'delib-flow-control-choose-reference-note-template)
    (define-key map (kbd "z") #'delib-flow-control-toggle-focus-mode)
    (define-key map (kbd "?") #'delib-flow-control-help-command)
    (dolist (key delib-flow--action-shortcut-keys)
      (define-key map (kbd key) #'delib-flow-control-dispatch-shortcut))
    map)
  "Keymap for `delib-flow-control-mode'.")

(put 'delib-flow-control-mode 'mode-class 'special)

(defun delib-flow--control-header-line ()
  "Return header-line text for the control buffer."
  (let ((status
         (if (and delib-flow--active-run
                  (delib-flow--run-in-flight-p delib-flow--active-run))
             (let* ((stage-id (delib-flow--run-in-flight-stage-id delib-flow--active-run))
                    (model (delib-flow--run-in-flight-model delib-flow--active-run))
                    (started-at (delib-flow--run-in-flight-started-at delib-flow--active-run)))
               (format " [%s %s%s %ss]"
                       (delib-flow--spinner-frame-for-time started-at)
                       (delib-flow--stage-label stage-id)
                       (if (and model (not (string-empty-p model)))
                           (format " %s" model)
                         "")
                       (delib-flow--elapsed-seconds started-at)))
           "")))
    (format " Delib-Flow cockpit%s%s: n/p sections, TAB actions, RET run, L loop, K preview, U history, z focus, g refresh, q quit "
            (if delib-flow-control-focus-mode
                " [focus]"
              "")
            status)))

(defun delib-flow--configure-buffer-for-terminal ()
  "Apply conservative readability defaults for terminal control buffers."
  (unless (display-graphic-p)
    (setq-local truncate-lines nil)
    (setq-local word-wrap t)
    (setq-local line-spacing nil)
    (setq-local bidi-display-reordering nil)
    (setq-local cursor-in-non-selected-windows nil)))

(defun delib-flow--revert-control-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the active control buffer through standard revert semantics."
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow-refresh-buffer))

(define-derived-mode delib-flow-control-mode org-mode "Delib-Flow"
  "Major mode for the DeliberateFlow control buffer."
  (setq-local header-line-format '(:eval (delib-flow--control-header-line)))
  (setq-local revert-buffer-function #'delib-flow--revert-control-buffer)
  (setq-local show-trailing-whitespace nil)
  (setq-local line-move-ignore-invisible t)
  (delib-flow--configure-buffer-for-terminal))

(defun delib-flow--point-in-editable-block-body-p (&optional position)
  "Return non-nil when POSITION is inside an editable block body."
  (get-text-property (or position (point)) 'delib-flow-editable))

(defun delib-flow--editable-block-insert-command ()
  "Insert the current key event into an editable block."
  (interactive)
  (pcase last-command-event
    ((or ?\r ?\n)
     (newline))
    ((pred characterp)
     (insert-char last-command-event 1))
    (_
     (self-insert-command 1))))

(defun delib-flow--control-command-or-edit (command)
  "Run COMMAND unless point is in an editable block, then insert the key."
  (if (delib-flow--point-in-editable-block-body-p)
      (delib-flow--editable-block-insert-command)
    (call-interactively command)))

(defun delib-flow-control-refresh ()
  "Refresh or insert `g' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-refresh))

(defun delib-flow-control-open-audit-run ()
  "Open audit run or insert `j' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-open-audit-run))

(defun delib-flow-control-open-audit-latest-stage ()
  "Open latest audit stage or insert `J' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-open-audit-latest-stage))

(defun delib-flow-control-debug-open-latest-stage-inspection ()
  "Open latest inspection or insert `D' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit
   #'delib-flow-debug-open-latest-stage-inspection))

(defun delib-flow-control-debug-open-comparison ()
  "Open comparison or insert `C' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-debug-open-comparison))

(defun delib-flow-control-debug-open-walkthrough ()
  "Open walkthrough or insert `W' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-debug-open-walkthrough))

(defun delib-flow-control-debug-apply-helper ()
  "Apply helper or insert `H' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-debug-apply-helper))

(defun delib-flow-control-debug-walkthrough-next-step ()
  "Advance walkthrough or insert `N' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit
   #'delib-flow-debug-walkthrough-next-step))

(defun delib-flow-control-debug-walkthrough-restart-target ()
  "Restart walkthrough or insert `R' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit
   #'delib-flow-debug-walkthrough-restart-target))

(defun delib-flow-control-next-section ()
  "Move to the next top-level cockpit section or insert `n' when editing."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-next-section))

(defun delib-flow-control-previous-section ()
  "Move to the previous top-level cockpit section or insert `p' when editing."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-previous-section))

(defun delib-flow-control-next-action ()
  "Move to the next rendered action or insert TAB when editing."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-next-action))

(defun delib-flow-control-previous-action ()
  "Move to the previous rendered action or insert backtab when editing."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-previous-action))

(defun delib-flow-control-dispatch-action ()
  "Dispatch action or insert the typed key when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-dispatch-action))

(defun delib-flow-control-dispatch-shortcut ()
  "Dispatch the rendered action bound to the typed shortcut key."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-dispatch-action-shortcut))

(defun delib-flow-control-approve-current ()
  "Approve current review or insert `A' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-approve-current))

(defun delib-flow-control-peek-filing-target ()
  "Peek the first filing target or insert `P' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-peek-filing-target))

(defun delib-flow-control-peek-staged-content ()
  "Peek staged filing content or insert `V' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-peek-staged-content))

(defun delib-flow-control-choose-manual-project ()
  "Choose manual project or insert `m' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-choose-manual-project))

(defun delib-flow-control-jump-active-loop ()
  "Jump to the active loop or insert `L' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-jump-active-loop))

(defun delib-flow-control-jump-latest-preview ()
  "Jump to the latest preview or insert `K' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-jump-latest-preview))

(defun delib-flow-control-jump-stage-history ()
  "Jump to stage history or insert `U' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-jump-stage-history))

(defun delib-flow-control-retry-current ()
  "Retry current review or insert `r' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-retry-current))

(defun delib-flow-control-abort-run ()
  "Abort run or insert `q' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-abort-run))

(defun delib-flow-control-choose-filing-selection ()
  "Choose filing selection or insert `s' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-choose-filing-selection))

(defun delib-flow-control-choose-reference-note-template ()
  "Choose reference-note template or insert `T' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-choose-reference-note-template))

(defun delib-flow-control-toggle-focus-mode ()
  "Toggle narrow-screen focus mode or insert `z' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-toggle-focus-mode))

(defun delib-flow-control-context-menu ()
  "Open a local context menu or insert `.' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-control-menu))

(defun delib-flow-control-help-command ()
  "Show help or insert `?' when editing a block."
  (interactive)
  (delib-flow--control-command-or-edit #'delib-flow-control-help))

(defconst delib-flow--control-sections
  '("Now"
    "Current result"
    "Filing preview"
    "Next actions"
    "Current context"
    "Details")
  "Top-level sections rendered in the control buffer.")

(defconst delib-flow--initial-section-anchor-alist
  '((now . "delib-section-now")
    (next-actions . "delib-section-next-actions")
    (current-result . "delib-section-current-result")
    (current-context . "delib-section-current-context")
    (filing-preview . "delib-section-filing-preview")
    (details . "delib-section-details"))
  "Stable anchor identifiers for top-level control-buffer sections.")

(defconst delib-flow--control-key-help-alist
  '(("n / p" . "Move to the next or previous top-level cockpit section.")
    ("TAB / S-TAB" . "Move to the next or previous rendered action.")
    ("1-9/0/letters" . "Run the action with the matching on-screen shortcut.")
    ("RET/a" . "Run the action at point.")
    ("." . "Open the local context menu for the current section.")
    ("A" . "Accept the current inspect or project-match review.")
    ("P" . "Peek the first filing target in a sibling window.")
    ("V" . "Peek the exact staged filing content in a preview buffer.")
    ("L" . "Jump back to the active decision loop.")
    ("K" . "Jump to the latest consequence preview or result.")
    ("U" . "Jump to the latest stage history details.")
    ("m" . "Open completion for manual project selection.")
    ("s" . "Open completion for filing artifact selection.")
    ("z" . "Toggle narrow-screen focus mode for the active decision loop.")
    ("r" . "Retry the current inspect or project-match stage.")
    ("g" . "Refresh the control buffer.")
    ("j" . "Open the audit log at the active run.")
    ("J" . "Open the audit log at the latest stage.")
    ("q" . "Abort the active run.")
    ("?" . "Show the full control-key help.")
    ("D" . "Open the latest stage debug inspection.")
    ("C" . "Open the debug comparison view.")
    ("W" . "Open the debug walkthrough.")
    ("H" . "Apply the debug helper for the active review block.")
    ("N" . "Advance the debug walkthrough to its next checkpoint.")
    ("R" . "Restart the debug walkthrough from baseline."))
  "Control-buffer keys and their user-facing descriptions.")

(defconst delib-flow--section-renderer-alist
  '(("Now" . delib-flow--render-now-section)
    ("Next actions" . delib-flow--render-valid-next-actions-section)
    ("Current result" . delib-flow--render-current-result-section)
    ("Current context" . delib-flow--render-working-context-section)
    ("Filing preview" . delib-flow--render-filing-preview-section)
    ("Details" . delib-flow--render-details-section))
  "Renderer functions keyed by section title.")

(defconst delib-flow--stage-descriptor-alist
  '((inspect-source
     :id inspect-source
     :label "Inspect Source"
     :prompt-id milestone2-inspect-source
     :executor delib-flow--execute-inspect-source
     :normalizer delib-flow--normalize-inspect-source-output)
    (match-project
     :id match-project
     :label "Match Project"
     :prompt-id milestone2-match-project
     :executor delib-flow--execute-match-project
     :normalizer delib-flow--normalize-match-project-output)
    (discover-reference-material
     :id discover-reference-material
     :label "Discover Relevant Reference Material"
     :prompt-id milestone2-discover-reference-material
     :executor delib-flow--execute-discover-reference-material
     :normalizer delib-flow--normalize-discover-reference-material-output)
    (filter-reference-material
     :id filter-reference-material
     :label "Filter Useful Reference Material"
     :prompt-id milestone2-filter-reference-material
     :executor delib-flow--execute-filter-reference-material
     :normalizer delib-flow--normalize-filter-reference-material-output)
    (manual-project-match
     :id manual-project-match
     :label "Choose Project Manually"
     :prompt-id milestone2-manual-project-match
     :executor delib-flow--execute-manual-project-match
     :normalizer delib-flow--normalize-manual-project-match-output)
    (propose-new-project
     :id propose-new-project
     :label "Propose New Project"
     :prompt-id milestone2-propose-new-project
     :executor delib-flow--execute-propose-new-project
     :normalizer delib-flow--normalize-propose-new-project-output)
    (extract-actions
     :id extract-actions
     :label "Extract Actions"
     :prompt-id milestone2-extract-actions
     :executor delib-flow--execute-extract-actions
     :normalizer delib-flow--normalize-extract-actions-output)
    (extract-waiting-for
     :id extract-waiting-for
     :label "Extract Waiting-For"
     :prompt-id milestone2-extract-waiting-for
     :executor delib-flow--execute-extract-waiting-for
     :normalizer delib-flow--normalize-extract-waiting-for-output)
    (suggest-reference-notes
     :id suggest-reference-notes
     :label "Suggest Reference Notes"
     :prompt-id milestone2-suggest-reference-notes
     :executor delib-flow--execute-suggest-reference-notes
     :normalizer delib-flow--normalize-suggest-reference-notes-output)
    (draft-selected-reference-note
     :id draft-selected-reference-note
     :label "Draft Selected Note"
     :prompt-id milestone13-draft-selected-reference-note
     :executor delib-flow--execute-draft-selected-reference-note
     :normalizer delib-flow--normalize-draft-selected-reference-note-output)
    (decide-cloud-pass
     :id decide-cloud-pass
     :label "Decide on Cloud Pass"
     :prompt-id milestone2-decide-cloud-pass
     :executor delib-flow--execute-decide-cloud-pass
     :normalizer delib-flow--normalize-decide-cloud-pass-output)
    (sanitize-for-cloud
     :id sanitize-for-cloud
     :label "Sanitize for Cloud"
     :prompt-id milestone2-sanitize-for-cloud
     :executor delib-flow--execute-sanitize-for-cloud
     :normalizer delib-flow--normalize-sanitize-for-cloud-output)
    (approve-cloud-send
     :id approve-cloud-send
     :label "Approve Cloud Send"
     :prompt-id milestone2-approve-cloud-send
     :executor delib-flow--execute-approve-cloud-send
     :normalizer delib-flow--normalize-approve-cloud-send-output)
    (run-cloud-stage
     :id run-cloud-stage
     :label "Run Cloud Stage"
     :prompt-id milestone2-run-cloud-stage
     :executor delib-flow--execute-run-cloud-stage
     :normalizer delib-flow--normalize-run-cloud-stage-output)
    (resolve-cloud-failure
     :id resolve-cloud-failure
     :label "Resolve Cloud Failure"
     :prompt-id milestone12-resolve-cloud-failure
     :executor delib-flow--execute-resolve-cloud-failure
     :normalizer delib-flow--normalize-resolve-cloud-failure-output)
    (approve-candidate-reintegration
     :id approve-candidate-reintegration
     :label "Approve Candidate Reintegration"
     :prompt-id milestone2-approve-candidate-reintegration
     :executor delib-flow--execute-approve-candidate-reintegration
     :normalizer delib-flow--normalize-approve-candidate-reintegration-output)
    (integrate-into-source
     :id integrate-into-source
     :label "Integrate into Source"
     :prompt-id milestone2-integrate-into-source
     :executor delib-flow--execute-integrate-into-source
     :normalizer delib-flow--normalize-integrate-into-source-output)
    (reject-draft-filing-artifact
     :id reject-draft-filing-artifact
     :label "Reject Draft Filing Artifact"
     :prompt-id milestone5-reject-draft-filing-artifact
     :executor delib-flow--execute-reject-draft-filing-artifact
     :normalizer delib-flow--normalize-reject-draft-filing-artifact-output)
    (select-approved-filing-actions
     :id select-approved-filing-actions
     :label "Select Approved Filing Actions"
     :prompt-id milestone2-select-approved-filing-actions
     :executor delib-flow--execute-select-approved-filing-actions
     :normalizer delib-flow--normalize-select-approved-filing-actions-output)
    (resolve-filing-conflict
     :id resolve-filing-conflict
     :label "Resolve Filing Conflict"
     :prompt-id milestone11-resolve-filing-conflict
     :executor delib-flow--execute-resolve-filing-conflict
     :normalizer delib-flow--normalize-resolve-filing-conflict-output)
    (file-approved-outputs
     :id file-approved-outputs
     :label "File Approved Outputs"
     :prompt-id milestone2-file-approved-outputs
     :executor delib-flow--execute-file-approved-outputs
     :normalizer delib-flow--normalize-file-approved-outputs-output))
  "Stage descriptors keyed by stage identifier.")

(defconst delib-flow--stage-decision-alist
  '((inspect-source
     . "Review inspect result and choose whether to accept, reject, or retry.")
    (match-project
     . "Review project match and choose whether to accept, reject, manually override, or retry.")
    (discover-reference-material
     . "Review retrieved reference candidates and choose next action.")
    (filter-reference-material
     . "Review retained context and choose next action.")
    (manual-project-match
     . "Review manual project selection and choose next action.")
    (propose-new-project
     . "Review proposed project checklist and choose next action.")
    (extract-actions . "Review drafted actions and choose next action.")
    (extract-waiting-for
     . "Review drafted waiting-for items and choose next action.")
    (suggest-reference-notes
     . "Review drafted reference notes and choose next action.")
    (draft-selected-reference-note
     . "Review the selected note draft and decide whether to regenerate or file it.")
    (decide-cloud-pass
     . "Review cloud-routing decision and choose next action.")
    (sanitize-for-cloud
     . "Review sanitized cloud package and choose next action.")
    (approve-cloud-send
     . "Review approved cloud package and choose next action.")
    (run-cloud-stage
     . "Review cloud-returned result and choose next action.")
    (resolve-cloud-failure
     . "Review cloud-failure resolution and choose next action.")
    (approve-candidate-reintegration
     . "Review approved reintegration candidate and choose next action.")
    (integrate-into-source
     . "Review integrated local result and choose next action.")
    (reject-draft-filing-artifact
     . "Review rejected filing artifact and choose next action.")
    (select-approved-filing-actions
     . "Review selected filing action and choose next action.")
    (resolve-filing-conflict
     . "Review filing-conflict resolution and choose next action.")
    (file-approved-outputs
     . "Review filed outputs and choose next action."))
  "Stage-specific current-decision text keyed by stage identifier.")

(defconst delib-flow--stage-apply-function-alist
  '((inspect-source . delib-flow--apply-inspect-source-entry)
    (match-project . delib-flow--apply-match-project-entry)
    (discover-reference-material . delib-flow--apply-discovery-entry)
    (filter-reference-material . delib-flow--apply-filter-entry)
    (manual-project-match . delib-flow--apply-manual-project-match-entry)
    (propose-new-project . delib-flow--apply-propose-new-project-entry)
    (extract-actions . delib-flow--apply-extract-actions-entry)
    (extract-waiting-for . delib-flow--apply-extract-waiting-for-entry)
    (suggest-reference-notes . delib-flow--apply-suggest-reference-notes-entry)
    (draft-selected-reference-note . delib-flow--apply-draft-selected-reference-note-entry)
    (decide-cloud-pass . delib-flow--apply-decide-cloud-pass-entry)
    (sanitize-for-cloud . delib-flow--apply-sanitize-for-cloud-entry)
    (approve-cloud-send . delib-flow--apply-approve-cloud-send-entry)
    (run-cloud-stage . delib-flow--apply-run-cloud-stage-entry)
    (resolve-cloud-failure . delib-flow--apply-resolve-cloud-failure-entry)
    (approve-candidate-reintegration
     . delib-flow--apply-approve-candidate-reintegration-entry)
    (integrate-into-source . delib-flow--apply-integrate-into-source-entry)
    (reject-draft-filing-artifact
     . delib-flow--apply-reject-draft-filing-artifact-entry)
    (select-approved-filing-actions
     . delib-flow--apply-select-approved-filing-actions-entry)
    (resolve-filing-conflict . delib-flow--apply-resolve-filing-conflict-entry)
    (file-approved-outputs . delib-flow--apply-file-approved-outputs-entry))
  "Stage-specific apply functions keyed by stage identifier.")

(defconst delib-flow--reviewable-stage-ids
  '(inspect-source match-project)
  "Stage identifiers that participate in accepted-result review state.")

(defconst delib-flow--meeting-source-keywords
  '("meeting" "standup" "sync" "retro" "planning" "check-in" "kickoff"
    "agenda" "minutes" "attendees" "1:1")
  "Keywords used to classify meeting-note source items.")

(defconst delib-flow--meeting-source-section-labels
  '("agenda:" "attendees:" "notes:" "decisions:" "action items:" "next steps:")
  "Structured section labels used to classify meeting-note source items.")

(defconst delib-flow--reminder-source-keywords
  '("reminder" "remember to" "don't forget" "dont forget" "follow up"
    "follow-up" "ping" "check on")
  "Keywords used to classify reminder source items.")

(defconst delib-flow--fleeting-note-source-keywords
  '("idea" "thought" "brainstorm" "note to self" "question" "questions"
    "wonder if" "maybe" "explore" "possible")
  "Keywords used to classify fleeting-note source items.")

(defconst delib-flow--project-proposal-title-prefix-pattern
  (concat
   "\\`\\(?:"
   "presentation or article on"
   "\\|article on"
   "\\|presentation on"
   "\\|idea[: -]+"
   "\\|thought[: -]+"
   "\\|note to self[: -]+"
   "\\|reminder[: -]+"
   "\\|question[: -]+"
   "\\)\\s-*")
  "Prefix pattern stripped from proposed project titles.")

(defconst delib-flow--project-proposal-tag-stopwords
  '("a" "an" "and" "are" "article" "for" "from" "idea" "in" "into" "my"
    "note" "of" "on" "or" "presentation" "project" "reminder" "the"
    "thought" "to" "up" "with")
  "Words that are too generic to use as proposed project tags.")

(defun delib-flow--org-heading-at-point-p ()
  "Return non-nil when point is on an Org heading."
  (org-at-heading-p))

(defun delib-flow--plain-string (value)
  "Return VALUE as a string without text properties."
  (when value
    (substring-no-properties value)))

(defun delib-flow--plain-value (value)
  "Return VALUE with text properties removed recursively."
  (cond
   ((stringp value)
    (delib-flow--plain-string value))
   ((consp value)
    (cons (delib-flow--plain-value (car value))
          (delib-flow--plain-value (cdr value))))
   ((vectorp value)
    (apply #'vector (mapcar #'delib-flow--plain-value value)))
   (t value)))

(defun delib-flow--non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun delib-flow--snapshot-heading ()
  "Capture a frozen snapshot of the Org heading at point.

Return a plist containing source metadata and content."
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (delib-flow--plain-string (org-get-heading t t t t)))
           (begin (point))
           (end (save-excursion
                  (org-end-of-subtree t t)
                  (point)))
           (content (buffer-substring-no-properties begin end))
           (file (buffer-file-name))
           (id (delib-flow--plain-string (org-entry-get (point) "ID")))
           (outline-path (delib-flow--plain-value
                          (ignore-errors (org-get-outline-path t t)))))
      (list :title title
            :file file
            :id id
            :begin begin
            :end end
            :outline-path outline-path
            :content content
            :source-type 'unknown))))

(defun delib-flow--configured-inbox-file ()
  "Return expanded configured inbox file path, or nil."
  (when (delib-flow--non-empty-string-p delib-flow-inbox-file)
    (expand-file-name delib-flow-inbox-file)))

(defun delib-flow--inbox-outline-path-at-point ()
  "Return current heading outline path at point as plain strings."
  (let ((heading (org-get-heading t t t t)))
    (delib-flow--plain-value
     (append (ignore-errors (org-get-outline-path nil t))
             (list heading)))))

(defun delib-flow--goto-inbox-outline-path (path)
  "Move point to heading matching outline PATH and return non-nil on success."
  (goto-char (point-min))
  (catch 'found
    (while (re-search-forward org-heading-regexp nil t)
      (goto-char (match-beginning 0))
      (when (equal (delib-flow--inbox-outline-path-at-point) path)
        (throw 'found t))
      (forward-line 1))
    nil))

(defun delib-flow--inbox-heading-snapshots (file &optional outline-path)
  "Return frozen snapshots for inbox headings in Org FILE.

When OUTLINE-PATH is nil, return top-level headings.  Otherwise return the
direct child headings beneath the heading matching OUTLINE-PATH."
  (with-temp-buffer
    (let ((buffer-file-name file))
      (insert-file-contents file)
      (org-mode)
      (save-excursion
        (save-restriction
          (widen)
          (let* ((path (mapcar #'delib-flow--plain-string outline-path))
                 (target-level (if path
                                   (1+ (length path))
                                 1))
                 (stack nil)
                 (path-found (null path))
                 snapshots)
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (goto-char (match-beginning 0))
              (let* ((level (or (org-current-level) 0))
                     (title (delib-flow--plain-string
                             (org-get-heading t t t t))))
                (setq stack (append (seq-take stack (max 0 (1- level)))
                                    (list title)))
                (when (equal stack path)
                  (setq path-found t))
                (when (and (= level target-level)
                           (equal (butlast stack) path))
                  (push (delib-flow--snapshot-heading) snapshots)))
              (forward-line 1))
            (unless path-found
              (user-error "Inbox outline path not found: %s"
                          (mapconcat #'identity path " > ")))
            (nreverse snapshots)))))))

(defun delib-flow--inbox-selection-labels (snapshots)
  "Return completion labels mapped to SNAPSHOTS."
  (let ((index 0))
    (mapcar
     (lambda (snapshot)
       (setq index (1+ index))
       (cons
        (format "[%s] %s"
                index
                (or (plist-get snapshot :title) "Untitled heading"))
        snapshot))
     snapshots)))

(defun delib-flow--start-run-from-source (source)
  "Initialize and render a run from frozen SOURCE."
  (setq delib-flow--active-run
        (delib-flow--initialize-run source))
  (pop-to-buffer
   (delib-flow--render-active-run-buffer delib-flow--active-run "Now")))

(defun delib-flow--make-editable-block (id kind section heading-path anchor-id)
  "Return an editable block object for ID and KIND in SECTION.

HEADING-PATH is the intended logical location for the block.
ANCHOR-ID is the stable in-buffer anchor for the block."
  (list :id id
        :kind kind
        :section section
        :heading-path heading-path
        :anchor-id anchor-id
        :status 'clean
        :original-text ""
        :current-text ""
        :accepted-text ""
        :validation-status 'valid
        :validation-message nil))

(defun delib-flow--initial-editable-blocks ()
  "Return the initial editable block alist for a new run."
  (list
   (cons 'context-main
         (delib-flow--make-editable-block
          'context-main
          'context
          "Current context"
          '("Current context" "Editable working slice")
          "delib-edit-context-main"))
   (cons 'operator-notes
         (delib-flow--make-editable-block
          'operator-notes
          'notes
          "Now"
          '("Now" "Operator notes")
          "delib-edit-operator-notes"))
   (cons 'manual-project-selection
         (delib-flow--make-editable-block
          'manual-project-selection
          'manual-selection
          "Now"
          '("Now" "Manual project selection")
          "delib-edit-manual-project-selection"))
   (cons 'filing-selection-review
         (delib-flow--make-editable-block
          'filing-selection-review
          'filing-selection
          "Filing preview"
          '("Filing preview" "Artifact selection")
          "delib-edit-filing-selection-review"))
   (cons 'filing-conflict-resolution
         (delib-flow--make-editable-block
          'filing-conflict-resolution
          'filing-conflict
          "Filing preview"
          '("Filing preview" "Conflict resolution")
          "delib-edit-filing-conflict-resolution"))
   (cons 'reference-note-capture-review
         (delib-flow--make-editable-block
          'reference-note-capture-review
          'reference-note-capture
          "Filing preview"
          '("Filing preview" "Reference note capture")
          "delib-edit-reference-note-capture-review"))
   (cons 'inspect-source-review
        (delib-flow--make-editable-block
          'inspect-source-review
          'inspect-review
          "Current context"
          '("Current context" "Source type correction")
          "delib-edit-inspect-source-review"))
   (cons 'cloud-package-review
         (delib-flow--make-editable-block
          'cloud-package-review
          'cloud-review
          "Details"
          '("Details" "Accepted working context" "Reviewed cloud package")
          "delib-edit-cloud-package-review"))
   (cons 'cloud-routing-review
         (delib-flow--make-editable-block
          'cloud-routing-review
          'cloud-routing
          "Details"
          '("Details" "Accepted working context" "Cloud routing review")
          "delib-edit-cloud-routing-review"))
   (cons 'cloud-failure-review
         (delib-flow--make-editable-block
          'cloud-failure-review
          'cloud-failure
          "Details"
          '("Details" "Accepted working context" "Cloud failure review")
          "delib-edit-cloud-failure-review"))))

(defun delib-flow--initial-section-anchors ()
  "Return the initial section-anchor alist for a new run."
  (copy-tree delib-flow--initial-section-anchor-alist))

(defun delib-flow--initial-actions-state ()
  "Return the initial action-state plist for a new run."
  (list :items nil
        :selected-action nil
        :last-action nil))

(defun delib-flow--make-stage-review-record (stage-id)
  "Return an accepted-result review record for STAGE-ID."
  (list :stage-id stage-id
        :candidate-stage-id nil
        :candidate-output nil
        :candidate-normalized-output nil
        :candidate-review-state 'not-available
        :accepted-stage-id nil
        :accepted-output nil
        :accepted-normalized-output nil))

(defun delib-flow--initial-review-results ()
  "Return the initial accepted-result review records for a new run."
  (mapcar (lambda (stage-id)
            (cons stage-id
                  (delib-flow--make-stage-review-record stage-id)))
          delib-flow--reviewable-stage-ids))

(defun delib-flow--make-action (id label status reason handler priority)
  "Return an action object for ID with LABEL, STATUS, and HANDLER.

REASON describes blocked or placeholder state.
PRIORITY controls display order."
  (list :id id
        :label label
        :status status
        :reason reason
        :section "Valid next actions"
        :priority priority
        :handler handler
        :shortcut nil
        :requires-clean-managed-regions t
        :requires-valid-edits t))

(defun delib-flow--initial-session-state ()
  "Return the initial session-state plist for a new run."
  (list :status 'active
        :started-at (current-time)
        :ended-at nil
        :current-stage nil
        :in-flight-stage-id nil
        :in-flight-provider nil
        :in-flight-model nil
        :in-flight-started-at nil
        :in-flight-request-id nil
        :in-flight-handle nil
        :current-decision "Review working context and choose next action."
        :run-buffer delib-flow-control-buffer-name
        :debug-fixture nil
        :active t
        :aborted nil))

(defun delib-flow--time-string (time)
  "Return stable timestamp string for TIME."
  (format-time-string "%Y-%m-%d %H:%M:%S%z" time))

(defun delib-flow--elapsed-seconds (time)
  "Return elapsed whole seconds since TIME."
  (max 0 (floor (- (float-time) (float-time time)))))

(defun delib-flow--spinner-frame-for-time (time)
  "Return spinner frame derived from elapsed TIME."
  (aref delib-flow--spinner-frames
        (mod (delib-flow--elapsed-seconds time)
             (length delib-flow--spinner-frames))))

(defun delib-flow--make-run-id (time)
  "Return stable run identifier from TIME."
  (format-time-string "delib-flow-%Y%m%dT%H%M%S%N" time))

(defun delib-flow--audit-run-status (run)
  "Return normalized audit run status for RUN."
  (let ((session (delib-flow--run-session run)))
    (cond
     ((plist-get session :aborted) 'aborted)
     ((delib-flow--stage-executed-p run 'file-approved-outputs) 'completed)
     ((plist-get (delib-flow--run-stage-history run) :entries) 'partial)
     (t 'active))))

(defun delib-flow--initial-run-record (source-snapshot session)
  "Return initial audit run record from SOURCE-SNAPSHOT and SESSION."
  (let ((started-at (plist-get session :started-at)))
    (list :run-id (delib-flow--make-run-id started-at)
          :source-title (plist-get source-snapshot :title)
          :source-file (plist-get source-snapshot :file)
          :started-at started-at
          :ended-at nil
          :run-status 'active)))

(defun delib-flow--initial-audit-state (source-snapshot session)
  "Return initial audit state from SOURCE-SNAPSHOT and SESSION."
  (list :run-record (delib-flow--initial-run-record source-snapshot session)
        :stage-records nil
        :pending-checkpoints nil
        :last-appended-checkpoint nil))

(defun delib-flow--initial-ui-state ()
  "Return the initial UI-state plist for a new run."
  (list :section-anchors (delib-flow--initial-section-anchors)
        :editable-blocks (delib-flow--initial-editable-blocks)
        :managed-region-conflicts nil
        :render-version 1
        :last-rendered-at nil))

(defun delib-flow--run-source (run)
  "Return the source plist from RUN."
  (plist-get run :source))

(defun delib-flow--run-working-context (run)
  "Return the working-context plist from RUN."
  (plist-get run :working-context))

(defun delib-flow--run-session (run)
  "Return the session plist from RUN."
  (plist-get run :session))

(defun delib-flow--run-in-flight-p (run)
  "Return non-nil when RUN currently has a stage request in flight."
  (let ((session (delib-flow--run-session run)))
    (and (plist-get session :in-flight-stage-id)
         (plist-get session :in-flight-request-id)
         (let ((handle (plist-get session :in-flight-handle)))
           (or (null handle)
               (not (processp handle))
               (process-live-p handle))))))

(defun delib-flow--run-in-flight-stage-id (run)
  "Return the in-flight stage id from RUN, if any."
  (plist-get (delib-flow--run-session run) :in-flight-stage-id))

(defun delib-flow--run-in-flight-model (run)
  "Return the in-flight model label from RUN, if any."
  (plist-get (delib-flow--run-session run) :in-flight-model))

(defun delib-flow--run-in-flight-provider (run)
  "Return the in-flight provider from RUN, if any."
  (plist-get (delib-flow--run-session run) :in-flight-provider))

(defun delib-flow--run-in-flight-started-at (run)
  "Return the in-flight start time from RUN, if any."
  (plist-get (delib-flow--run-session run) :in-flight-started-at))

(defun delib-flow--run-ui (run)
  "Return the UI plist from RUN."
  (plist-get run :ui))

(defun delib-flow--run-actions (run)
  "Return the actions plist from RUN."
  (plist-get run :actions))

(defun delib-flow--run-stage-history (run)
  "Return the stage-history plist from RUN."
  (plist-get run :stage-history))

(defun delib-flow--review-results (working)
  "Return accepted-result review records from WORKING."
  (plist-get working :review-results))

(defun delib-flow--review-record (working stage-id)
  "Return accepted-result review record for STAGE-ID from WORKING."
  (alist-get stage-id (delib-flow--review-results working)))

(defun delib-flow--replace-review-record (records stage-id new-record)
  "Return RECORDS with NEW-RECORD stored under STAGE-ID."
  (mapcar (lambda (entry)
            (if (eq (car entry) stage-id)
                (cons stage-id new-record)
              entry))
          records))

(defun delib-flow--stage-review-state (run stage-id)
  "Return candidate review-state for STAGE-ID from RUN."
  (delib-flow--review-record-status
   (delib-flow--review-record (delib-flow--run-working-context run) stage-id)))

(defun delib-flow--stage-accepted-p (run stage-id)
  "Return non-nil when STAGE-ID has accepted output in RUN."
  (delib-flow--review-record-accepted-p
   (delib-flow--review-record (delib-flow--run-working-context run) stage-id)))

(defun delib-flow--project-decision-ready-p (run)
  "Return non-nil when RUN has an accepted or manual project decision."
  (not (null (delib-flow--accepted-project-decision run))))

(defun delib-flow--project-status-reviewed-p (run statuses)
  "Return non-nil when RUN has a reviewed project status in STATUSES."
  (and (memq (delib-flow--stage-review-state run 'match-project)
             '(accepted rejected))
       (memq (delib-flow--match-status run) statuses)))

(defun delib-flow--reference-note-suggestion-ready-p (run)
  "Return non-nil when RUN can suggest general reference notes."
  (delib-flow--stage-accepted-p run 'inspect-source))

(defun delib-flow--project-proposal-ready-p (run)
  "Return non-nil when RUN can propose a new project."
  (or (and (delib-flow--project-decision-ready-p run)
           (eq (delib-flow--match-status run) 'no-match))
      (delib-flow--project-status-reviewed-p run '(ambiguous no-match))))

(defun delib-flow--run-routing (run)
  "Return the routing plist from RUN."
  (plist-get run :routing))

(defun delib-flow--run-ui-package (run)
  "Return the UI package plist from RUN."
  (plist-get run :ui))

(defun delib-flow--run-cloud-stage-p (stage-id)
  "Return non-nil when STAGE-ID should execute through the cloud adapter."
  (eq stage-id 'run-cloud-stage))

(defun delib-flow--cloud-reintegrated-stage-ids (run)
  "Return rerouted cloud stage ids already reintegrated into RUN."
  (plist-get (delib-flow--run-routing run) :cloud-reintegrated-stage-ids))

(defun delib-flow--integration-ready-p (run)
  "Return non-nil when RUN has material ready for integration."
  (or (delib-flow--stage-executed-p run 'integrate-into-source)
      (plist-get (plist-get run :filing) :draft-items)
      (and (eq (plist-get (delib-flow--run-routing run) :reintegration-status)
               'approved)
           (or (plist-get (delib-flow--run-working-context run) :cloud-returned-context)
               (delib-flow--cloud-fallback-mode
                (delib-flow--run-routing run))))))

(defun delib-flow--approved-items-ready-p (run)
  "Return non-nil when RUN has approved items ready to file."
  (plist-get (plist-get run :filing) :approved-items))

(defun delib-flow--draft-items-ready-p (run)
  "Return non-nil when RUN has draft items ready for filing review."
  (plist-get (plist-get run :filing) :draft-items))

(defun delib-flow--latest-stage-id (run)
  "Return the latest executed stage identifier from RUN."
  (plist-get (delib-flow--run-stage-history run) :latest-stage))

(defun delib-flow--stage-descriptor (stage-id)
  "Return the descriptor plist for STAGE-ID."
  (cdr (assoc stage-id delib-flow--stage-descriptor-alist)))

(defun delib-flow--stage-label (stage-id)
  "Return the user-facing label for STAGE-ID."
  (plist-get (delib-flow--stage-descriptor stage-id) :label))

(defun delib-flow--stage-prompt-id (stage-id)
  "Return the prompt identifier for STAGE-ID."
  (plist-get (delib-flow--stage-descriptor stage-id) :prompt-id))

(defun delib-flow--file-readable-p (path)
  "Return non-nil when PATH is a readable file."
  (and path
       (not (string-empty-p path))
       (file-readable-p path)))

(defun delib-flow--prompt-library-configured-p ()
  "Return non-nil when the prompt library file is configured."
  (delib-flow--file-readable-p delib-flow-prompt-library-file))

(defun delib-flow--example-structures-configured-p ()
  "Return non-nil when the example structures file is configured."
  (delib-flow--file-readable-p delib-flow-example-structures-file))

(defun delib-flow--org-entry-body-at-point ()
  "Return trimmed Org entry body at point."
  (string-trim
   (buffer-substring-no-properties
    (save-excursion
      (org-end-of-meta-data t)
      (point))
    (save-excursion
      (org-end-of-subtree t t)
      (point)))))

(defun delib-flow--prompt-library-entry-text (prompt-id)
  "Return prompt-library entry text for PROMPT-ID, if available."
  (when (delib-flow--prompt-library-configured-p)
    (with-temp-buffer
      (insert-file-contents delib-flow-prompt-library-file)
      (org-mode)
      (goto-char (point-min))
      (when (re-search-forward
             (format "^:PROMPT_ID:[ \t]*%s$" (regexp-quote (symbol-name prompt-id)))
             nil t)
        (org-back-to-heading t)
        (delib-flow--org-entry-body-at-point)))))

(defun delib-flow--example-structures-text ()
  "Return example-structure text, if configured."
  (when (delib-flow--example-structures-configured-p)
    (with-temp-buffer
      (insert-file-contents delib-flow-example-structures-file)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun delib-flow--resolved-prompt-status (prompt-text)
  "Return prompt-resolution status from PROMPT-TEXT."
  (cond
   (prompt-text 'resolved)
   ((delib-flow--prompt-library-configured-p) 'missing)
   (t 'descriptor-only)))

(defun delib-flow--resolved-example-structures-status (example-text)
  "Return example-structure status from EXAMPLE-TEXT."
  (if example-text 'available 'not-available))

(defun delib-flow--prompt-render-base-lines (stage-id prompt-id prompt-text)
  "Return base rendered-prompt lines for STAGE-ID PROMPT-ID and PROMPT-TEXT."
  (delq nil
        (list
         (format "Stage: %s" (delib-flow--stage-label stage-id))
         (format "Prompt ID: %s" prompt-id)
         (when prompt-text
           (format "Prompt template:\n%s" prompt-text)))))

(defun delib-flow--prompt-render-example-lines (example-text)
  "Return rendered-prompt example lines from EXAMPLE-TEXT."
  (when example-text
    (list (format "Example structures:\n%s" example-text))))

(defun delib-flow--prompt-rendered-text (stage-id prompt-id prompt-text example-text)
  "Return stage-facing rendered prompt text."
  (string-join
   (append
    (delib-flow--prompt-render-base-lines stage-id prompt-id prompt-text)
    (delib-flow--prompt-render-example-lines example-text))
   "\n\n"))

(defconst delib-flow--inspect-response-schema
  '("Source type"
    "Summary"
    "Entities"
    "Contacts"
    "Terms"
    "Commitments"
    "Dates"
    "Blockers"
    "Decisions"
    "Questions"
    "Links"
    "Notable statements")
  "Structured response fields for inspect-source stages.")

(defconst delib-flow--new-project-required-checklist
  '("Project title"
    "Project state as Active or Waiting"
    "One next action or one waiting-for item"
    "Relevant tags"
    "Linked contacts where matching Org-roam person notes exist")
  "Required checklist items for new-project proposals.")

(defconst delib-flow--new-project-optional-checklist
  '("Project summary"
    "Linked reference file"
    "Deadline"
    "Priority"
    "Success criteria"
    "Links to related notes")
  "Optional checklist items for new-project proposals.")

(defun delib-flow--artifact-stage-guidance (artifact-kind quality-rules)
  "Return structured guidance for ARTIFACT-KIND with QUALITY-RULES."
  (list :artifact-kind artifact-kind
        :one-at-a-time-review-p t
        :quality-rules quality-rules))

(defconst delib-flow--project-decision-fields
  '("Match status" "Best project" "Candidates" "Reason")
  "Structured decision fields for project-matching stages.")

(defconst delib-flow--cloud-review-boundaries
  '("Reviewed routing decision"
    "Reviewed sanitization"
    "Explicit send approval"
    "Explicit reintegration approval")
  "Reviewed boundaries for cloud-corridor stages.")

(defconst delib-flow--cloud-prompt-stage-ids
  '(decide-cloud-pass sanitize-for-cloud approve-cloud-send run-cloud-stage
    resolve-cloud-failure approve-candidate-reintegration integrate-into-source)
  "Stage IDs that share the reviewed cloud-corridor prompt contract.")

(defconst delib-flow--filing-review-stage-ids
  '(select-approved-filing-actions reject-draft-filing-artifact
    resolve-filing-conflict file-approved-outputs)
  "Stage IDs that share the filing review prompt contract.")

(defconst delib-flow--stage-prompt-structure-table
  `((inspect-source
     . (:stage-family source-analysis
        :response-schema ,delib-flow--inspect-response-schema
        :review-gate "Accepted inspect output should become trusted source-analysis context."))
    (match-project
     . (:stage-family project-decision
        :decision-fields ,delib-flow--project-decision-fields
        :review-gate "Accepted project decisions unlock downstream retrieval and filing stages."))
    (propose-new-project
     . (:stage-family project-creation
        :required-checklist ,delib-flow--new-project-required-checklist
        :optional-checklist ,delib-flow--new-project-optional-checklist))
    (decide-cloud-pass
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (sanitize-for-cloud
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (approve-cloud-send
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (run-cloud-stage
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (resolve-cloud-failure
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (approve-candidate-reintegration
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (integrate-into-source
     . (:stage-family reviewed-cloud-corridor
        :review-boundaries ,delib-flow--cloud-review-boundaries))
    (select-approved-filing-actions
     . (:stage-family filing-review
        :one-at-a-time-review-p t
        :deterministic-write-boundary-p t))
    (reject-draft-filing-artifact
     . (:stage-family filing-review
        :one-at-a-time-review-p t
        :deterministic-write-boundary-p t))
    (resolve-filing-conflict
     . (:stage-family filing-review
        :one-at-a-time-review-p t
        :deterministic-write-boundary-p t))
    (file-approved-outputs
     . (:stage-family filing-review
        :one-at-a-time-review-p t
        :deterministic-write-boundary-p t)))
  "Stage-aware structured prompt contracts keyed by stage ID.")

(defun delib-flow--artifact-producing-prompt-structure (stage-id)
  "Return the prompt contract for artifact-producing STAGE-ID."
  (alist-get
   stage-id
   '((extract-actions
      next-action
      ("Concrete next step or deliverable"
       "Scoped to one focused work session"
       "Avoid vague follow-up wording"))
     (extract-waiting-for
      waiting-for
      ("Name the blocked dependency owner"
       "Name the exact response, approval, or deliverable"
       "Prefer explicit `Waiting for ...` phrasing"))
     (suggest-reference-notes
      reference-note
      ("Derive a stable note title"
       "Use a supported note type"
       "Preserve reusable project or PKM context")))))

(defun delib-flow--stage-prompt-structure (stage-id)
  "Return stage-aware structured prompt contract for STAGE-ID."
  (or (alist-get stage-id delib-flow--stage-prompt-structure-table)
      (when-let ((artifact-guidance
                  (delib-flow--artifact-producing-prompt-structure stage-id)))
        (apply #'delib-flow--artifact-stage-guidance artifact-guidance))
      (list :stage-family 'generic-stage)))

(defun delib-flow--resolved-prompt (stage-id)
  "Return prompt-resolution metadata for STAGE-ID."
  (let* ((prompt-id (delib-flow--stage-prompt-id stage-id))
         (prompt-text (and prompt-id
                           (delib-flow--prompt-library-entry-text prompt-id)))
         (example-text (delib-flow--example-structures-text)))
    (list :id prompt-id
          :status (delib-flow--resolved-prompt-status prompt-text)
          :library-file delib-flow-prompt-library-file
          :template-text prompt-text
          :example-structures-status
          (delib-flow--resolved-example-structures-status example-text)
          :example-structures-text example-text
          :structured-guidance
          (delib-flow--stage-prompt-structure stage-id)
          :rendered-text
          (delib-flow--prompt-rendered-text
           stage-id prompt-id prompt-text example-text))))

(defun delib-flow--stage-executed-p (run stage-id)
  "Return non-nil when STAGE-ID already appears in RUN history."
  (or (memq stage-id
            (mapcar (lambda (entry)
                      (and (delib-flow--stage-entry-counts-as-executed-p entry)
                           (plist-get entry :stage-id)))
                    (plist-get (delib-flow--run-stage-history run) :entries)))
      (memq stage-id (delib-flow--cloud-reintegrated-stage-ids run))))

(defun delib-flow--stage-input-package (run stage-id)
  "Return assembled input package for STAGE-ID from RUN."
  (delib-flow--plain-value
   (list :stage-id stage-id
         :prompt-id (delib-flow--stage-prompt-id stage-id)
         :prompt (delib-flow--resolved-prompt stage-id)
         :source (delib-flow--run-source run)
         :working-context (delib-flow--run-working-context run)
         :filing (plist-get run :filing)
         :artifacts (plist-get run :artifacts)
         :routing (delib-flow--run-routing run)
         :ui (delib-flow--run-ui-package run))))

(defun delib-flow--string-words (text)
  "Return normalized word list for TEXT."
  (split-string (downcase (or text "")) "[^[:alnum:]]+" t))

(defun delib-flow--shared-word-count (left right)
  "Return shared normalized word count between LEFT and RIGHT."
  (length (seq-intersection (delib-flow--string-words left)
                            (delib-flow--string-words right)
                            #'string=)))

(defun delib-flow--text-emails (text)
  "Return normalized email list found in TEXT."
  (let ((start 0)
        emails)
    (while (string-match
            "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]+"
            (or text "")
            start)
      (push (downcase (match-string 0 text)) emails)
      (setq start (match-end 0)))
    (delete-dups (nreverse emails))))

(defun delib-flow--extract-email-addresses (text)
  "Return normalized email addresses extracted from TEXT."
  (delib-flow--text-emails text))

(defun delib-flow--low-value-email-address-p (email)
  "Return non-nil when EMAIL is low-value routing or list noise."
  (let ((text (downcase (or email ""))))
    (or (string-match-p
         "\\`\\(?:no-\\?reply\\|do-\\?not-\\?reply\\|mailer-daemon\\|postmaster\\|abuse\\|unsubscribe\\)@"
         text)
        (string-match-p "\\`unsub\\+" text)
        (string-match-p "@convertkit-mail" text)
        (string-match-p "@ckespa\\." text)
        (string-match-p "\\`outlook\\.com@" text))))

(defun delib-flow--meaningful-contact-emails (emails)
  "Return EMAILS filtered to likely meaningful correspondents."
  (delete-dups
   (seq-remove #'delib-flow--low-value-email-address-p
               (apply #'append
                      (mapcar #'delib-flow--extract-email-addresses
                              (or emails '()))))))

(defun delib-flow--project-subtree-text ()
  "Return the current project subtree body text."
  (buffer-substring-no-properties
   (save-excursion
     (forward-line 1)
     (point))
   (save-excursion
     (org-end-of-subtree t t)
     (point))))

(defun delib-flow--project-direct-body-text ()
  "Return direct body text for the current project heading only.
This excludes child headings so project matching is driven by project identity
rather than previously filed tasks nested under the project."
  (save-excursion
    (let* ((level (org-outline-level))
           (subtree-end (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
           (start (progn
                    (org-end-of-meta-data t)
                    (point)))
           (child-start
            (save-excursion
              (goto-char start)
              (catch 'found
                (while (re-search-forward org-heading-regexp subtree-end t)
                  (beginning-of-line)
                  (when (= (org-outline-level) (1+ level))
                    (throw 'found (point)))
                  (outline-next-heading))
                nil)))
           (end (or child-start subtree-end)))
      (buffer-substring-no-properties start end))))

(defun delib-flow--text-org-file-links (text base-dir)
  "Return normalized Org file links found in TEXT relative to BASE-DIR."
  (let ((start 0)
        links)
    (while (string-match "\\[\\[file:\\([^]]+\\.org\\)\\]" (or text "") start)
      (push (expand-file-name (match-string 1 text) base-dir) links)
      (setq start (match-end 0)))
    (delete-dups (nreverse links))))

(defun delib-flow--project-candidate-terms (title tags text)
  "Return normalized candidate terms from TITLE, TAGS, and TEXT."
  (delete-dups
   (append (delib-flow--string-words title)
           (mapcar #'downcase tags)
           (delib-flow--string-words text))))

(defun delib-flow--project-candidate (title tags contacts terms links)
  "Return a project candidate object for TITLE, TAGS, CONTACTS, TERMS, and LINKS."
  (list :title title
        :tags tags
        :contacts contacts
        :terms terms
        :links links))

(defun delib-flow--project-state-heading-p (title)
  "Return non-nil when TITLE is a project-state bucket heading."
  (member (downcase (or title ""))
          '("active" "complete" "waiting")))

(defun delib-flow--top-heading-title-at-point ()
  "Return the enclosing top-level heading title at point."
  (save-excursion
    (while (> (org-outline-level) 1)
      (org-up-heading-safe))
    (org-get-heading t t t t)))

(defun delib-flow--project-heading-candidate-p ()
  "Return non-nil when the heading at point should be parsed as a project."
  (let ((title (org-get-heading t t t t))
        (level (org-outline-level)))
    (or (and (= level 1)
             (not (delib-flow--project-state-heading-p title)))
        (and (= level 2)
             (delib-flow--project-state-heading-p
              (delib-flow--top-heading-title-at-point))))))

(defun delib-flow--project-candidate-at-point (base-dir)
  "Return the current top-level project candidate at point using BASE-DIR."
  (let* ((title (org-get-heading t t t t))
         (tags (org-get-tags))
         (text (delib-flow--project-direct-body-text))
         (subtree-text (delib-flow--project-subtree-text))
         (contacts (delib-flow--text-emails subtree-text)))
    (delib-flow--project-candidate
     title
     tags
     contacts
     (delib-flow--project-candidate-terms title tags text)
     (delib-flow--text-org-file-links subtree-text base-dir))))

(defun delib-flow--project-candidates-from-file (file)
  "Return project candidates parsed from Org FILE."
  (with-temp-buffer
    (let ((base-dir (file-name-directory file)))
      (insert-file-contents file)
      (org-mode)
      (let (candidates)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ \\(.+\\)$" nil t)
          (beginning-of-line)
          (when (delib-flow--project-heading-candidate-p)
            (push (delib-flow--project-candidate-at-point base-dir) candidates))
          (outline-next-heading))
        (nreverse candidates)))))

(defun delib-flow--source-match-text (source)
  "Return normalized source text used for project matching from SOURCE."
  (format "%s\n%s"
          (or (plist-get source :title) "")
          (or (plist-get source :content) "")))

(defun delib-flow--normalize-match-source-title (title)
  "Return TITLE normalized for project matching."
  (let ((text (string-trim (or title ""))))
    (string-trim
     (replace-regexp-in-string
      "\\`\\(?:<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*>\\|\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\]\\)\\s-*"
      ""
      text))))

(defun delib-flow--source-match-tags (source)
  "Return normalized tag-like words from SOURCE."
  (delib-flow--string-words
   (delib-flow--normalize-match-source-title
    (plist-get source :title))))

(defun delib-flow--project-match-score (source candidate)
  "Return match score between SOURCE and CANDIDATE."
  (let* ((candidate-title (plist-get candidate :title))
         (source-title (delib-flow--normalize-match-source-title
                        (plist-get source :title)))
         (source-text (delib-flow--source-match-text source))
         (source-tags (delib-flow--source-match-tags source))
         (source-contacts (delib-flow--text-emails source-text))
         (title-shared (delib-flow--shared-word-count source-title candidate-title))
         (term-shared (length (seq-intersection
                               (delib-flow--string-words source-text)
                               (plist-get candidate :terms)
                               #'string=)))
         (tag-shared (length (seq-intersection
                              source-tags
                              (mapcar #'downcase (plist-get candidate :tags))
                              #'string=)))
         (contact-shared (length (seq-intersection
                                  source-contacts
                                  (plist-get candidate :contacts)
                                  #'string=)))
         (exact (string= (downcase (or source-title ""))
                         (downcase (or candidate-title "")))))
    (+ title-shared
       term-shared
       (* 2 tag-shared)
       (* 3 contact-shared)
       (if exact 10 0))))

(defun delib-flow--scored-project-candidates (source candidates)
  "Return scored project CANDIDATES for SOURCE."
  (mapcar (lambda (candidate)
            (plist-put (copy-sequence candidate)
                       :score
                       (delib-flow--project-match-score source candidate)))
          candidates))

(defun delib-flow--sort-project-candidates (candidates)
  "Return CANDIDATES sorted by descending score."
  (sort (copy-sequence candidates)
        (lambda (left right)
          (> (plist-get left :score)
             (plist-get right :score)))))

(defun delib-flow--top-project-candidates (candidates)
  "Return the top-scoring CANDIDATES."
  (let* ((sorted (delib-flow--sort-project-candidates candidates))
         (top-score (plist-get (car sorted) :score)))
    (seq-take-while (lambda (candidate)
                      (= (plist-get candidate :score) top-score))
                    sorted)))

(defun delib-flow--project-match-result (source candidates)
  "Return raw project-match result for SOURCE and CANDIDATES."
  (let* ((scored (delib-flow--scored-project-candidates source candidates))
         (top (delib-flow--top-project-candidates scored))
         (best (car top))
         (score (or (plist-get best :score) 0)))
    (cond
     ((<= score 0)
      (list :match-status 'no-match
            :best-project nil
            :candidates nil
            :reason "No project title, tag, contact, or metadata terms overlapped the source."))
     ((> (length top) 1)
      (list :match-status 'ambiguous
            :best-project nil
            :candidates top
            :reason "Multiple projects tied for the best metadata-aware match."))
     (t
      (list :match-status 'matched
            :best-project best
            :candidates top
            :reason "A single highest-scoring metadata-aware match was found.")))))

(defun delib-flow--manual-project-match-candidates (package)
  "Return fallback candidates for manual project selection from PACKAGE."
  (let* ((working (plist-get package :working-context))
         (project-match (plist-get working :project-match))
         (candidates (plist-get project-match :candidates)))
    (or candidates
        (sort (delib-flow--project-candidates)
              (lambda (left right)
                (string-lessp (plist-get left :title)
                              (plist-get right :title)))))))

(defun delib-flow--manual-project-selection-text-from-package (package)
  "Return editable manual project-selection text from PACKAGE."
  (delib-flow--editable-block-text
   (alist-get 'manual-project-selection
              (plist-get (plist-get package :ui) :editable-blocks))))

(defun delib-flow--manual-project-selection-value (package)
  "Return trimmed Selection value from PACKAGE manual project text."
  (when-let* ((text (delib-flow--manual-project-selection-text-from-package package))
              (_ (string-match "^Selection:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--manual-project-selection-notes (package)
  "Return trimmed Notes text from PACKAGE manual project text."
  (when-let* ((text (delib-flow--manual-project-selection-text-from-package package)))
    (when (string-match
           "^[Nn]otes:[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)\\(?:^Candidates:\\|\\'\\)"
           text)
      (string-trim (or (match-string 1 text) "")))))

(defun delib-flow--manual-project-reject-all-p (selection)
  "Return non-nil when SELECTION explicitly rejects all candidates."
  (member (downcase (or selection ""))
          '("reject" "reject-all" "none" "no-match")))

(defun delib-flow--manual-project-selection-template (candidates)
  "Return editable manual project-selection template for CANDIDATES."
  (concat
   "Selection: \n"
   "Notes:\n"
   "\n\n"
   "Selection help:\n"
   "- Press `m` or run `M-x delib-flow-choose-manual-project` to choose from valid candidates with completion.\n"
   "- Type `REJECT` to keep the current no-match decision.\n"
   "\n"
   "Candidates:\n"
   (if candidates
       (mapconcat (lambda (candidate)
                    (format "- %s" (plist-get candidate :title)))
                  candidates
                  "\n")
     "- No candidates available. Use `REJECT` to keep the result as no-match.")))

(defun delib-flow--table-cell-text (value width)
  "Return VALUE sanitized and truncated for an Org table cell of WIDTH."
  (truncate-string-to-width
   (replace-regexp-in-string "|" "/" (or value ""))
   width nil nil t))

(defun delib-flow--org-table-text (headers rows widths)
  "Return an Org table with HEADERS, ROWS, and column WIDTHS."
  (let ((row-format
         (lambda (cells)
           (concat
            "| "
            (mapconcat
             #'identity
             (cl-mapcar
              (lambda (cell width)
                (format (format "%%-%ds" width)
                        (delib-flow--table-cell-text cell width)))
              cells widths)
             " | ")
            " |"))))
    (concat
     (funcall row-format headers)
     "\n|"
     (mapconcat
      (lambda (width)
        (make-string (+ width 2) ?-))
      widths
      "+")
     "|\n"
     (mapconcat row-format rows "\n"))))

(defun delib-flow--manual-project-selection-status-text (run)
  "Return current manual-project chooser status for RUN."
  (let* ((package (delib-flow--stage-input-package run 'manual-project-match))
         (selection (delib-flow--manual-project-selection-value package)))
    (cond
     ((delib-flow--manual-project-reject-all-p selection)
      "Current selection: REJECT (keep the current no-match decision).")
     ((delib-flow--non-empty-string-p selection)
      (format "Current selection: %s" selection))
     (t
      "Current selection: none yet."))))

(defun delib-flow--manual-project-shortlist-cards (run)
  "Return compact manual-project candidate shortlist text for RUN."
  (let* ((package (delib-flow--stage-input-package run 'manual-project-match))
         (selection (downcase (or (delib-flow--manual-project-selection-value package) "")))
         (candidates (delib-flow--manual-project-match-candidates package))
         (cards nil)
         (index 0))
    (if (null candidates)
        "No project candidates are currently available."
      (dolist (candidate candidates)
        (setq index (1+ index))
        (let* ((selected-p
                (string-equal selection
                              (downcase (plist-get candidate :title))))
               (tags (or (plist-get candidate :tags) nil))
               (tag-text (if tags
                             (string-join tags ", ")
                           "none"))
               (contact-count (length (plist-get candidate :contacts)))
               (link-count (length (plist-get candidate :links))))
          (push
           (format "- [%s]%s %s\n  Tags: %s\n  Signals: %s contact(s), %s link(s)"
                   index
                   (if selected-p " selected" "")
                   (delib-flow--compact-summary (plist-get candidate :title) 48)
                   (delib-flow--compact-summary tag-text 42)
                   contact-count
                   link-count)
           cards)))
      (string-join (nreverse cards) "\n"))))

(defun delib-flow--manual-project-selection-section-text (run)
  "Return the manual-project chooser section text for RUN."
  (format "*** Manual project selection\n- Press `m` or run `M-x delib-flow-choose-manual-project` to choose from valid candidates with completion.\n- Type `REJECT` in the fallback block to keep the current no-match decision.\n- %s\n\n**** Local update\n- Consequence: choosing a manual project replaces the current no-match or ambiguous project decision for downstream drafting.\n- Resume here: choose one candidate, then run Choose Project Manually to unlock drafting.\n\n**** Candidate shortlist\n%s\n\n**** Fallback block\n%s"
          (delib-flow--manual-project-selection-status-text run)
          (delib-flow--manual-project-shortlist-cards run)
          (delib-flow--render-editable-block run 'manual-project-selection)))

(defun delib-flow--replace-selection-line (text selection)
  "Return TEXT with the leading Selection line set to SELECTION."
  (let ((replacement (format "Selection: %s" selection)))
    (if (string-match "^Selection:[^\n]*" text)
        (replace-match replacement t t text)
      (concat replacement "\n" text))))

(defun delib-flow--manual-project-selection-block-text (run)
  "Return manual project-selection block text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'manual-project-selection)))

(defun delib-flow--manual-project-selection-valid-p (run)
  "Return non-nil when RUN has a valid manual project selection."
  (condition-case nil
      (progn
        (delib-flow--manual-project-choice
         (delib-flow--stage-input-package run 'manual-project-match))
        t)
    (error nil)))

(defun delib-flow--manual-project-selection-labels (run)
  "Return display labels to selection values for manual-project chooser in RUN."
  (let* ((package (delib-flow--stage-input-package run 'manual-project-match))
         (candidates (delib-flow--manual-project-match-candidates package))
         (entries (mapcar (lambda (candidate)
                            (cons (plist-get candidate :title)
                                  (plist-get candidate :title)))
                          candidates)))
    (append entries
            '(("REJECT - keep the current no-match decision" . "REJECT")))))

(defun delib-flow--set-manual-project-selection-value (run selection)
  "Return RUN with manual project Selection set to SELECTION."
  (let* ((package (delib-flow--stage-input-package run 'manual-project-match))
         (candidates (delib-flow--manual-project-match-candidates package))
         (block (delib-flow--editable-block run 'manual-project-selection))
         (base-text (let ((current (delib-flow--manual-project-selection-block-text run)))
                      (if (delib-flow--non-empty-string-p current)
                          current
                        (delib-flow--manual-project-selection-template candidates))))
         (updated-text (delib-flow--replace-selection-line base-text selection)))
    (delib-flow--set-editable-block
     run
     'manual-project-selection
     (delib-flow--set-editable-block-text block updated-text))))

(defun delib-flow--manual-project-choice (package)
  "Return chosen manual project candidate from PACKAGE."
  (let* ((selection (delib-flow--manual-project-selection-value package))
         (candidates (delib-flow--manual-project-match-candidates package)))
    (unless selection
      (error "Manual project selection requires a Selection value"))
    (unless (delib-flow--manual-project-reject-all-p selection)
      (or (seq-find (lambda (candidate)
                      (string-equal
                       (downcase selection)
                       (downcase (plist-get candidate :title))))
                    candidates)
          (error "Manual project selection did not match any available candidate: %s"
                 selection)))))

(defun delib-flow--manual-project-match-result (package)
  "Return raw manual-project-match result for PACKAGE."
  (let* ((choice (delib-flow--manual-project-choice package))
         (selection (delib-flow--manual-project-selection-value package))
         (notes (delib-flow--manual-project-selection-notes package))
         (candidates (delib-flow--manual-project-match-candidates package)))
    (if choice
        (list :match-status 'matched
              :selection-method 'manual
              :best-project choice
              :candidates candidates
              :operator-selection selection
              :operator-notes notes
              :reason "Operator selected a concrete project candidate after manual review.")
      (list :match-status 'no-match
            :selection-method 'manual
            :best-project nil
            :candidates candidates
            :operator-selection selection
            :operator-notes notes
            :reason "Operator rejected all available project candidates after manual review."))))

(defun delib-flow--filing-selection-review-block (package)
  "Return filing-selection editable block from PACKAGE."
  (alist-get 'filing-selection-review
             (plist-get (plist-get package :ui) :editable-blocks)))

(defun delib-flow--filing-selection-review-text-from-package (package)
  "Return editable filing-selection text from PACKAGE."
  (delib-flow--editable-block-text
   (delib-flow--filing-selection-review-block package)))

(defun delib-flow--filing-conflict-resolution-block (package)
  "Return filing-conflict-resolution editable block from PACKAGE."
  (alist-get 'filing-conflict-resolution
             (plist-get (plist-get package :ui) :editable-blocks)))

(defun delib-flow--cloud-failure-review-block (package)
  "Return cloud-failure-review editable block from PACKAGE."
  (alist-get 'cloud-failure-review
             (plist-get (plist-get package :ui) :editable-blocks)))

(defun delib-flow--cloud-routing-review-block (package)
  "Return cloud-routing-review editable block from PACKAGE."
  (alist-get 'cloud-routing-review
             (plist-get (plist-get package :ui) :editable-blocks)))

(defun delib-flow--cloud-routing-review-text-from-package (package)
  "Return editable cloud-routing text from PACKAGE."
  (delib-flow--editable-block-text
   (delib-flow--cloud-routing-review-block package)))

(defun delib-flow--cloud-failure-review-text-from-package (package)
  "Return editable cloud-failure text from PACKAGE."
  (delib-flow--editable-block-text
   (delib-flow--cloud-failure-review-block package)))

(defun delib-flow--filing-conflict-resolution-text-from-package (package)
  "Return editable filing-conflict-resolution text from PACKAGE."
  (delib-flow--editable-block-text
   (delib-flow--filing-conflict-resolution-block package)))

(defun delib-flow--reference-note-capture-review-block (package)
  "Return reference-note-capture editable block from PACKAGE."
  (alist-get 'reference-note-capture-review
             (plist-get (plist-get package :ui) :editable-blocks)))

(defun delib-flow--reference-note-capture-review-text-from-package (package)
  "Return editable reference-note-capture text from PACKAGE."
  (delib-flow--editable-block-text
   (delib-flow--reference-note-capture-review-block package)))

(defun delib-flow--filing-selection-value (package)
  "Return trimmed Selection value from PACKAGE filing-selection text."
  (when-let* ((text (delib-flow--filing-selection-review-text-from-package package))
              (_ (string-match "^Selection:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--filing-selection-notes (package)
  "Return trimmed Notes text from PACKAGE filing-selection text."
  (when-let* ((text (delib-flow--filing-selection-review-text-from-package package)))
    (when (string-match
           "^[Nn]otes:[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)\\(?:^Draft artifacts:\\|\\'\\)"
           text)
      (string-trim (or (match-string 1 text) "")))))

(defun delib-flow--reference-note-capture-field (package label)
  "Return trimmed field value for LABEL from PACKAGE reference-note-capture text."
  (when-let* ((text (delib-flow--reference-note-capture-review-text-from-package package))
              (_ (string-match (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote label))
                               text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--reference-note-capture-template-key-value (package)
  "Return selected reference-note capture template key from PACKAGE."
  (delib-flow--reference-note-capture-field package "Template key"))

(defun delib-flow--reference-note-capture-title-value (package)
  "Return selected reference-note capture title from PACKAGE."
  (delib-flow--reference-note-capture-field package "Note title"))

(defun delib-flow--reference-note-capture-target-path-value (package)
  "Return selected reference-note capture target path from PACKAGE."
  (delib-flow--reference-note-capture-field package "Target path"))

(defun delib-flow--filing-conflict-resolution-value (package)
  "Return trimmed Resolution value from PACKAGE conflict-resolution text."
  (when-let* ((text (delib-flow--filing-conflict-resolution-text-from-package package))
              (_ (string-match "^Resolution:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--filing-conflict-resolution-notes (package)
  "Return trimmed Notes text from PACKAGE conflict-resolution text."
  (when-let* ((text (delib-flow--filing-conflict-resolution-text-from-package package)))
    (when (string-match
           "^[Nn]otes:[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)\\(?:^New title:\\|^New text:\\|^Conflict summary:\\|\\'\\)"
           text)
      (string-trim (or (match-string 1 text) "")))))

(defun delib-flow--filing-conflict-resolution-new-title (package)
  "Return trimmed New title value from PACKAGE conflict-resolution text."
  (when-let* ((text (delib-flow--filing-conflict-resolution-text-from-package package))
              (_ (string-match "^New title:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--filing-conflict-resolution-new-text (package)
  "Return trimmed New text value from PACKAGE conflict-resolution text."
  (when-let* ((text (delib-flow--filing-conflict-resolution-text-from-package package))
              (_ (string-match "^New text:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--filing-conflict-resolution-keyword (value)
  "Return normalized conflict-resolution keyword from VALUE."
  (upcase (or value "")))

(defun delib-flow--cloud-failure-resolution-value (package)
  "Return trimmed Resolution value from PACKAGE cloud-failure text."
  (when-let* ((text (delib-flow--cloud-failure-review-text-from-package package))
              (_ (string-match "^Resolution:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--cloud-failure-resolution-notes (package)
  "Return trimmed Notes text from PACKAGE cloud-failure text."
  (when-let* ((text (delib-flow--cloud-failure-review-text-from-package package)))
    (when (string-match
           "^[Nn]otes:[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)\\(?:^Failure summary:\\|^Guidance:\\|\\'\\)"
           text)
      (string-trim (or (match-string 1 text) "")))))

(defun delib-flow--cloud-failure-resolution-keyword (value)
  "Return normalized cloud-failure resolution keyword from VALUE."
  (upcase (or value "")))

(defun delib-flow--cloud-routing-target-stage-value (package)
  "Return trimmed Target stage value from PACKAGE cloud-routing text."
  (when-let* ((text (delib-flow--cloud-routing-review-text-from-package package))
              (_ (string-match "^Target stage:[ \t]*\\(.*\\)$" text)))
    (string-trim (match-string 1 text))))

(defun delib-flow--indexed-draft-item-line (index item)
  "Return numbered filing-selection line for INDEX and draft ITEM."
  (format "- [%d] %s %s"
          index
          (delib-flow--draft-item-keyword item)
          (plist-get item :text)))

(defun delib-flow--indexed-draft-item-lines (index item)
  "Return numbered filing-selection lines for INDEX and draft ITEM."
  (append
   (list (delib-flow--indexed-draft-item-line index item)
         (format "  Status: %s"
                 (delib-flow--draft-item-readiness-text item)))
   (delib-flow--draft-item-warning-lines item)
   (delib-flow--draft-item-remediation-lines item)))

(defun delib-flow--selection-index-list (indexes)
  "Return readable selection INDEXES list."
  (if indexes
      (mapconcat #'number-to-string indexes ", ")
    "none"))

(defun delib-flow--filing-selection-choice-label (index item)
  "Return completion label for filing selection INDEX and ITEM."
  (format "[%d] %s %s (%s)"
          index
          (delib-flow--draft-item-keyword item)
          (plist-get item :text)
          (delib-flow--draft-item-readiness-text item)))

(defun delib-flow--filing-selection-labels (run)
  "Return display labels to selection values for filing chooser in RUN."
  (let ((items (plist-get (plist-get run :filing) :draft-items))
        (index 0)
        labels)
    (dolist (item items (nreverse labels))
      (setq index (1+ index))
      (push (cons (delib-flow--filing-selection-choice-label index item)
                  (number-to-string index))
            labels))))

(defun delib-flow--set-filing-selection-value (run selection)
  "Return RUN with filing Selection set to SELECTION."
  (let* ((items (plist-get (plist-get run :filing) :draft-items))
         (block (delib-flow--editable-block run 'filing-selection-review))
         (base-text (let ((current (delib-flow--filing-selection-block-text run)))
                      (if (delib-flow--non-empty-string-p current)
                          current
                        (delib-flow--filing-selection-template items))))
         (updated-text (delib-flow--replace-selection-line base-text selection))
         (updated-run
          (delib-flow--set-editable-block
           run
           'filing-selection-review
           (delib-flow--set-editable-block-text block updated-text)))
         (selected-item
          (condition-case nil
              (delib-flow--filing-selection-choice updated-run)
            (error nil))))
    (setq updated-run
          (if (eq (plist-get selected-item :kind) 'reference-note)
              (delib-flow--set-artifact-family-selected-candidate-id
               updated-run
               'reference-notes
               (delib-flow--artifact-candidate-id selected-item))
            (delib-flow--set-artifact-family-selected-candidate-id
             updated-run
             'reference-notes
             nil)))
    (let* ((state (delib-flow--artifact-family-state updated-run 'reference-notes))
           (selected-id (plist-get state :selected-candidate-id))
           (selected-draft (plist-get state :selected-draft)))
      (when (and selected-draft
                 (not (equal selected-id
                             (delib-flow--artifact-candidate-id selected-draft))))
        (setq updated-run
              (delib-flow--set-artifact-family-selected-draft
               updated-run
               'reference-notes
               nil))))
    (delib-flow--seed-reference-note-capture-review-block updated-run)))

(defun delib-flow--set-reference-note-capture-field (run label value)
  "Return RUN with reference-note capture LABEL set to VALUE."
  (let* ((block (delib-flow--editable-block run 'reference-note-capture-review))
         (base-text (let ((current (delib-flow--editable-block-text block)))
                      (if (delib-flow--non-empty-string-p current)
                          current
                        (delib-flow--reference-note-capture-template run))))
         (updated-text
          (if (string-match (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote label))
                            base-text)
              (replace-match (format "%s: %s" label (or value "")) t t base-text)
            (concat (format "%s: %s\n" label (or value "")) base-text))))
    (delib-flow--set-editable-block
     run
     'reference-note-capture-review
     (delib-flow--set-editable-block-text block updated-text))))

(defun delib-flow--draft-item-selection-indexes (items predicate)
  "Return 1-based indexes in ITEMS matching PREDICATE."
  (let ((index 0)
        matches)
    (dolist (item items (nreverse matches))
      (setq index (1+ index))
      (when (funcall predicate item)
        (push index matches)))))

(defun delib-flow--filing-selection-guidance-lines (items)
  "Return operator guidance lines for filing selection ITEMS."
  (let ((ready-indexes
         (delib-flow--draft-item-selection-indexes items
                                                   #'delib-flow--draft-item-ready-p))
        (blocked-indexes
         (delib-flow--draft-item-selection-indexes
          items
          (lambda (item)
            (not (delib-flow--draft-item-ready-p item))))))
    (list
     (format "Enter exactly one ready index after Selection:. Example: %s"
             (if ready-indexes
                 (number-to-string (car ready-indexes))
               "1"))
     (format "Ready selections: %s"
             (delib-flow--selection-index-list ready-indexes))
     (format "Blocked selections: %s"
             (delib-flow--selection-index-list blocked-indexes))
     "Selecting a ready artifact approves only that item and leaves the rest in draft state."
     "Blocked artifacts must be fixed, skipped by choosing a different ready item, or rejected from this run.")))

(defun delib-flow--filing-target-summary (item run)
  "Return compact target summary text for filing ITEM in RUN."
  (let ((locations (condition-case nil
                       (delib-flow--planned-file-location item run)
                     (error nil))))
    (if (null locations)
        "unavailable"
      (mapconcat
       (lambda (location)
         (let ((target (plist-get location :target)))
           (cond
            ((string-match "::\\(.+\\)\\'" target)
             (match-string 1 target))
            ((string-match "\\([^/]+\\.org\\)\\'" target)
             (match-string 1 target))
            (t
             target))))
       locations
       " + "))))

(defun delib-flow--filing-readiness-badge (item)
  "Return short readiness badge text for filing ITEM."
  (if (delib-flow--draft-item-ready-p item)
      "READY"
    "BLOCKED"))

(defun delib-flow--filing-selection-shortlist-cards (run)
  "Return compact filing-selection shortlist text for RUN."
  (let* ((items (plist-get (plist-get run :filing) :draft-items))
         (package (delib-flow--stage-input-package run 'select-approved-filing-actions))
         (selection (delib-flow--filing-selection-value package))
         (cards nil)
         (index 0))
    (if (null items)
        "No draft filing artifacts are currently available."
      (dolist (item items)
        (setq index (1+ index))
        (let ((selected-p (string-equal selection (number-to-string index))))
          (push
           (format "- [%s]%s %s\n  %s\n  Files to: %s\n  Warnings: %s | Kind: %s"
                   index
                   (if selected-p " selected" "")
                   (if (delib-flow--draft-item-ready-p item)
                       "Ready to approve"
                     "Needs fixes before approval")
                   (delib-flow--compact-summary
                    (format "%s %s"
                            (delib-flow--draft-item-keyword item)
                            (plist-get item :text))
                    58)
                   (delib-flow--compact-summary
                    (delib-flow--filing-target-summary item run)
                    52)
                   (delib-flow--draft-item-warning-count item)
                   (symbol-name (plist-get item :kind)))
           cards)))
      (string-join (nreverse cards) "\n"))))

(defun delib-flow--filing-selection-section-text (run)
  "Return the filing-selection review section text for RUN."
  (format "*** Artifact selection\n%s\n\n**** Choose from queue\n%s\n\n**** Selection form\n%s\n\n"
          (delib-flow--filing-selection-instructions run)
          (delib-flow--filing-selection-shortlist-cards run)
          (delib-flow--render-editable-block run 'filing-selection-review)))

(defun delib-flow--filing-selection-item-lines (items)
  "Return numbered filing-selection lines for draft ITEMS."
  (let ((index 0)
        lines)
    (dolist (item items lines)
      (setq index (1+ index))
      (setq lines
            (append lines
                    (delib-flow--indexed-draft-item-lines index item))))))

(defun delib-flow--filing-selection-template (items)
  "Return editable filing-selection template for draft ITEMS."
  (concat
   "Selection: \n"
   "Notes:\n"
   "\n"
   "Selection guidance:\n"
   (if items
       (mapconcat #'identity
                  (delib-flow--filing-selection-guidance-lines items)
                  "\n")
     "No ready or blocked selections are available.")
   "\n\n"
   "Draft artifacts:\n"
   (if items
       (mapconcat #'identity
                  (delib-flow--filing-selection-item-lines items)
                  "\n")
     "- No draft artifacts are available.")))

(defun delib-flow--reference-note-item-with-title (item title)
  "Return reference-note ITEM rewritten to use TITLE."
  (plist-put
   (copy-sequence item)
   :text
   (if (eq (plist-get item :note-type) 'project-support)
     (format "Create project support note from %s" title)
     (format "Create general PKM note for %s" title))))

(defun delib-flow--reference-note-capture-guidance-lines (item)
  "Return operator guidance lines for reference-note ITEM capture review."
  (let* ((options (delib-flow--reference-note-capture-template-options item))
         (default-key (delib-flow--reference-note-org-roam-template-key item)))
    (append
     (list
     (format "Default template key: %s"
              (or default-key "none configured"))
      "Choose an org-roam template key before filing when you want a different note workflow."
      "Use `Target path:` when the chosen template normally prompts for a file name or dynamic path."
      "Examples: `1a.org` for a zettel template, or `wiki/custom-topic.org` for an explicit note path."
      "Staged note files are seeded with a working structure, source highlights, and related material when the capture template body is otherwise minimal."
      "Press `T` or run `M-x delib-flow-choose-reference-note-template` to choose from configured templates.")
     (when options
       (list
        (format "Available templates: %s"
                (string-join
                 (mapcar (lambda (option) (cdr option))
                         (mapcar (lambda (pair)
                                   (cons (car pair) (cdr pair)))
                                 options))
                 ", ")))))))

(defun delib-flow--reference-note-capture-template (run)
  "Return editable reference-note capture template for RUN."
  (if-let ((item (delib-flow--reference-note-preview-item run)))
      (let* ((package (delib-flow--stage-input-package run 'file-approved-outputs))
             (template-key (or (delib-flow--reference-note-effective-template-key
                                item package)
                               ""))
             (title (or (delib-flow--reference-note-effective-title item package)
                        ""))
             (target-path (or (delib-flow--reference-note-effective-target-override
                               package)
                              "")))
        (concat
         (format "Template key: %s\n" template-key)
         (format "Note title: %s\n" title)
         (format "Target path: %s\n" target-path)
         "\n"
         "Capture guidance:\n"
         (mapconcat #'identity
                    (delib-flow--reference-note-capture-guidance-lines item)
                    "\n")))
    "Template key: \nNote title: \nTarget path: \n\nCapture guidance:\nNo reference-note filing artifact is currently active."))

(defun delib-flow--project-item-with-title (item title)
  "Return project ITEM rewritten to use TITLE."
  (let ((updated (copy-tree item)))
    (plist-put
     (plist-put
      updated
      :title title)
     :text title)))

(defun delib-flow--project-child-item-with-text (item text)
  "Return project child ITEM rewritten to use TEXT."
  (plist-put (copy-sequence item) :text text))

(defconst delib-flow--conflict-resolution-item-guidance-map
  '((next-action . "- REWORD-ITEM: change the deterministic project-child heading text and keep the artifact approved for retry.")
    (waiting-for . "- REWORD-ITEM: change the deterministic project-child heading text and keep the artifact approved for retry.")
    (project . "- RETITLE-PROJECT: change the deterministic top-level project title and keep the artifact approved for retry.")
    (reference-note . "- RENAME-NOTE: change the deterministic note title and keep the artifact approved for retry.")))

(defun delib-flow--conflict-resolution-item-guidance-lines (item)
  "Return ITEM-specific conflict-resolution guidance lines."
  (when-let ((line (alist-get (plist-get item :kind)
                              delib-flow--conflict-resolution-item-guidance-map)))
    (list line)))

(defun delib-flow--conflict-resolution-guidance-lines (item conflicts)
  "Return operator guidance lines for approved ITEM and current CONFLICTS."
  (append
   '("Allowed resolutions:"
     "- RETRY: keep the approved artifact and retry filing after correcting the target state."
     "- REJECT: move the approved artifact out of the approved slot for this run.")
   (delib-flow--conflict-resolution-item-guidance-lines item)
   (list
    (format "Current conflict count: %s" (length conflicts)))))

(defun delib-flow--conflict-resolution-retarget-fields (item)
  "Return editable retarget field block for approved conflict ITEM."
  (pcase (plist-get item :kind)
    ('reference-note
     (concat
      "New title:\n"
      "\n"
      "New text:\n"
      "[unused for this conflict type]\n"))
    ((or 'next-action 'waiting-for)
     (concat
      "New text:\n"
      "\n"
      "New title:\n"
      "[unused for this conflict type]\n"))
    ('project
     (concat
      "New title:\n"
      "\n"
      "New text:\n"
      "[unused for this conflict type]\n"))
    (_
     (concat
      "New title:\n"
      "[unused for this conflict type]\n"
      "\n"
      "New text:\n"
      "[unused for this conflict type]\n"))))

(defun delib-flow--filing-conflict-resolution-template (run)
  "Return editable conflict-resolution template for RUN."
  (let* ((filing (plist-get run :filing))
         (approved (car (plist-get filing :approved-items)))
         (conflicts (plist-get filing :conflicts)))
    (concat
     "Resolution: RETRY\n"
     "Notes:\n"
     "\n"
     (delib-flow--conflict-resolution-retarget-fields approved)
     "\n"
     "Conflict summary:\n"
     (if conflicts
         (mapconcat #'delib-flow--filing-conflict-line conflicts "\n")
       "- No filing conflicts are currently recorded.")
     "\n\n"
     "Approved artifact:\n"
     (if approved
         (delib-flow--draft-item-preview-text (list approved))
       "- No approved artifacts are available yet.")
     "\n\n"
     "Guidance:\n"
     (mapconcat #'identity
                (delib-flow--conflict-resolution-guidance-lines approved conflicts)
                "\n"))))

(defun delib-flow--cloud-failure-stage (routing)
  "Return recorded cloud-failure stage from ROUTING."
  (plist-get routing :cloud-failure-stage))

(defun delib-flow--cloud-transport-stage-label ()
  "Return operator-facing label for the cloud transport wrapper."
  (delib-flow--stage-label 'run-cloud-stage))

(defun delib-flow--rerouted-cloud-stage-p (stage-id)
  "Return non-nil when STAGE-ID is a rerouted cloud target stage."
  (and stage-id
       (not (eq stage-id 'run-cloud-stage))))

(defun delib-flow--cloud-failure-message (routing)
  "Return recorded cloud-failure message from ROUTING."
  (plist-get routing :cloud-failure-message))

(defun delib-flow--cloud-failure-active-p (run)
  "Return non-nil when RUN has a recorded cloud failure awaiting review."
  (let ((routing (delib-flow--run-routing run)))
    (and (delib-flow--cloud-failure-stage routing)
         (delib-flow--cloud-failure-message routing))))

(defun delib-flow--cloud-fallback-mode (routing)
  "Return explicit cloud fallback mode from ROUTING, if any."
  (plist-get routing :cloud-fallback-mode))

(defun delib-flow--cloud-failure-guidance-lines ()
  "Return operator guidance lines for cloud failure review."
  '("Allowed resolutions:"
    "- RETRY-CLOUD: keep the reviewed cloud package and retry cloud execution."
    "- USE-LOCAL: continue locally without cloud output and allow local reintegration."
    "- SKIP-CLOUD: explicitly skip the cloud branch for this run and continue locally."
    "- ABORT: stop the run after recording the failure review."))

(defun delib-flow--cloud-reroutable-stage-ids ()
  "Return valid reroutable cloud target stages."
  (seq-filter #'delib-flow--stage-descriptor
              delib-flow-cloud-reroutable-stage-ids))

(defun delib-flow--default-cloud-target-stage ()
  "Return default rerouted cloud target stage."
  (or (car (delib-flow--cloud-reroutable-stage-ids))
      'run-cloud-stage))

(defun delib-flow--cloud-target-stage (routing)
  "Return selected cloud target stage from ROUTING."
  (or (plist-get routing :cloud-target-stage)
      (delib-flow--default-cloud-target-stage)))

(defun delib-flow--cloud-target-stage-name (stage-id)
  "Return editable display name for cloud target STAGE-ID."
  (symbol-name stage-id))

(defun delib-flow--cloud-target-stage-choice (package)
  "Return validated cloud target stage chosen in PACKAGE."
  (let* ((value (delib-flow--cloud-routing-target-stage-value package))
         (stage-id (if (delib-flow--non-empty-string-p value)
                       (intern value)
                     (delib-flow--default-cloud-target-stage))))
    (unless (memq stage-id (delib-flow--cloud-reroutable-stage-ids))
      (error "Cloud target stage must be one of %s"
             (mapconcat #'symbol-name
                        (delib-flow--cloud-reroutable-stage-ids)
                        ", ")))
    stage-id))

(defun delib-flow--cloud-routing-review-template (run)
  "Return editable cloud-routing review template for RUN."
  (let* ((routing (delib-flow--run-routing run))
         (target-stage (delib-flow--cloud-target-stage routing)))
    (concat
     (format "Target stage: %s\n"
             (delib-flow--cloud-target-stage-name target-stage))
     "Notes:\n"
     "\n"
     "Allowed target stages:\n"
     (mapconcat
      (lambda (stage-id)
        (format "- %s: %s"
                (delib-flow--cloud-target-stage-name stage-id)
                (delib-flow--stage-label stage-id)))
      (delib-flow--cloud-reroutable-stage-ids)
      "\n"))))

(defun delib-flow--cloud-failure-review-template (run)
  "Return editable cloud-failure review template for RUN."
  (let* ((routing (delib-flow--run-routing run))
         (stage-id (delib-flow--cloud-failure-stage routing))
         (message (delib-flow--cloud-failure-message routing)))
    (concat
     "Resolution: RETRY-CLOUD\n"
     "Notes:\n"
     "\n"
     "Failure summary:\n"
     (format "%s- Message: %s\n\n"
             (if (delib-flow--rerouted-cloud-stage-p stage-id)
                 (format "- Cloud target stage: %s\n- Transport stage: %s\n"
                         (delib-flow--stage-label stage-id)
                         (delib-flow--cloud-transport-stage-label))
               (format "- Stage: %s\n"
                       (if stage-id
                           (delib-flow--stage-label stage-id)
                         "Unknown stage")))
             (or message "No cloud failure message is recorded."))
     "Guidance:\n"
     (mapconcat #'identity
                (delib-flow--cloud-failure-guidance-lines)
                "\n"))))

(defun delib-flow--filing-selection-choice-by-index (selection items)
  "Return selected draft item from SELECTION and draft ITEMS by numeric index."
  (let ((indexes nil)
        (start 0))
    (while (and selection
                (string-match "\\([0-9]+\\)" selection start))
      (push (string-to-number (match-string 1 selection)) indexes)
      (setq start (match-end 0)))
    (setq indexes (nreverse indexes))
    (when (> (length indexes) 1)
      (error "Filing selection must name exactly one artifact index; found: %s"
             (mapconcat #'number-to-string indexes ", ")))
    (when-let ((index (car indexes)))
      (unless (and (> index 0)
                   (<= index (length items)))
        (error "Filing selection index is out of range: %s" selection))
      (nth (1- index) items))))

(defun delib-flow--filing-selection-choice (package)
  "Return selected filing draft item from PACKAGE."
  (let* ((selection (delib-flow--filing-selection-value package))
         (items (delib-flow--draft-items package)))
    (unless (delib-flow--non-empty-string-p selection)
      (error "Filing selection requires a Selection value"))
    (or (delib-flow--filing-selection-choice-by-index selection items)
        (seq-find (lambda (item)
                    (string-equal selection (plist-get item :text)))
                  items)
        (error "Filing selection did not match any available draft artifact: %s"
               selection))))

(defun delib-flow--validate-filing-selection-entry (run)
  "Signal a user-facing error when RUN has an invalid filing selection entry."
  (when (delib-flow--filing-selection-active-p run)
    ;; Parse the live entry before any review adapter can coerce ambiguous
    ;; operator input such as `1, 3` into a single artifact choice.
    (delib-flow--filing-selection-choice run))
  run)

(defun delib-flow--remove-first-matching-item (items selected)
  "Return ITEMS with first occurrence of SELECTED removed."
  (let ((removed nil))
    (seq-remove
     (lambda (item)
       (if (and (not removed)
                (equal item selected))
           (progn
             (setq removed t)
             t)
         nil))
     items)))

(defun delib-flow--source-search-title (package)
  "Return source title for retrieval from PACKAGE."
  (plist-get (plist-get package :source) :title))

(defun delib-flow--project-match-title (package)
  "Return matched project title for retrieval from PACKAGE."
  (let ((project-match
         (plist-get (plist-get package :working-context) :project-match)))
    (plist-get (plist-get project-match :best-project) :title)))

(defun delib-flow--discovery-search-terms (package)
  "Return weighted search terms for PACKAGE."
  (let ((project-match
         (plist-get (plist-get package :working-context) :project-match)))
    (delete-dups
     (append
      (delib-flow--string-words (delib-flow--source-search-title package))
      (delib-flow--string-words (delib-flow--project-match-title package))
      (mapcar #'downcase
              (plist-get (plist-get project-match :best-project) :tags))))))

(defun delib-flow--zk-note-files ()
  "Return note files from `delib-flow-zk-root'."
  (unless (and delib-flow-zk-root
               (file-directory-p delib-flow-zk-root))
    (error "The ZK root is not configured or readable"))
  (directory-files-recursively delib-flow-zk-root "\\.org\\'"))

(defun delib-flow--zk-note-title (file)
  "Return note title derived from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+title: \\(.+\\)$" nil t)
        (string-trim (match-string 1))
      (file-name-base file))))

(defun delib-flow--zk-note-text (file)
  "Return searchable text from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun delib-flow--zk-note-tags (file)
  "Return normalized file tags for FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^#\\+filetags:[ \t]*\\(.+\\)$" nil t)
        (seq-filter
         #'identity
         (mapcar (lambda (tag)
                   (let ((trimmed (string-trim tag)))
                     (unless (string-empty-p trimmed)
                       (downcase trimmed))))
                 (split-string (match-string 1) ":" t)))
      nil)))

(defun delib-flow--zk-note-contacts (file)
  "Return normalized contact addresses found in FILE."
  (delib-flow--text-emails (delib-flow--zk-note-text file)))

(defun delib-flow--zk-note-links (file)
  "Return normalized Org file links found in FILE."
  (delib-flow--text-org-file-links
   (delib-flow--zk-note-text file)
   (file-name-directory file)))

(defun delib-flow--matched-project-metadata (package)
  "Return matched project metadata from PACKAGE."
  (plist-get (plist-get (plist-get package :working-context) :project-match)
             :best-project))

(defun delib-flow--discovery-inspect-output (package)
  "Return inspect output used for discovery from PACKAGE."
  (plist-get (plist-get package :working-context) :inspect-output))

(defun delib-flow--discovery-source-contacts (package)
  "Return source contact addresses for discovery from PACKAGE."
  (plist-get (delib-flow--discovery-inspect-output package)
             :contact-emails))

(defun delib-flow--discovery-source-links (package)
  "Return source Org file links for discovery from PACKAGE."
  (plist-get (delib-flow--discovery-inspect-output package)
             :org-file-links))

(defun delib-flow--discovery-title-score (terms title)
  "Return title-based discovery score for TERMS against TITLE."
  (delib-flow--shared-word-count (mapconcat #'identity terms " ") title))

(defun delib-flow--discovery-text-score (terms text)
  "Return text-based discovery score for TERMS against TEXT."
  (delib-flow--shared-word-count (mapconcat #'identity terms " ") text))

(defun delib-flow--discovery-tag-score (project note-tags)
  "Return tag-based discovery score for PROJECT and NOTE-TAGS."
  (length (seq-intersection (mapcar #'downcase (plist-get project :tags))
                            note-tags
                            #'string=)))

(defun delib-flow--discovery-contact-score (project note-contacts)
  "Return contact-based discovery score for PROJECT and NOTE-CONTACTS."
  (length (seq-intersection (plist-get project :contacts)
                            note-contacts
                            #'string=)))

(defun delib-flow--discovery-source-contact-score (package note-contacts)
  "Return source-contact discovery score for PACKAGE and NOTE-CONTACTS."
  (length (seq-intersection (delib-flow--discovery-source-contacts package)
                            note-contacts
                            #'string=)))

(defun delib-flow--discovery-linked-file-p (project file)
  "Return non-nil when FILE is directly linked from PROJECT."
  (member file (plist-get project :links)))

(defun delib-flow--discovery-source-linked-file-p (package file)
  "Return non-nil when FILE is directly linked from source PACKAGE."
  (member file (delib-flow--discovery-source-links package)))

(defun delib-flow--discovery-project-file-p (file)
  "Return non-nil when FILE is the configured My Projects file."
  (and file
       delib-flow-my-projects-file
       (file-exists-p file)
       (file-exists-p delib-flow-my-projects-file)
       (string= (file-truename file)
                (file-truename delib-flow-my-projects-file))))

(defun delib-flow--discovery-shared-link-score (package note-links)
  "Return shared-link discovery score for PACKAGE and NOTE-LINKS."
  (length
   (seq-intersection
    (append (delib-flow--discovery-source-links package)
            (plist-get (delib-flow--matched-project-metadata package) :links))
    note-links
    #'string=)))

(defun delib-flow--discovery-signal (key value weight)
  "Return weighted discovery signal for KEY with VALUE and WEIGHT."
  (list :key key
        :value value
        :weight weight
        :contribution (* value weight)))

(defun delib-flow--discovery-signals (package file)
  "Return weighted discovery signals for PACKAGE against FILE."
  (let* ((terms (delib-flow--discovery-search-terms package))
         (project (delib-flow--matched-project-metadata package))
         (title (delib-flow--zk-note-title file))
         (text (delib-flow--zk-note-text file))
         (note-contacts (delib-flow--zk-note-contacts file))
         (note-links (delib-flow--zk-note-links file))
         (linked-file-p (if (delib-flow--discovery-linked-file-p project file) 1 0))
         (source-linked-file-p (if (delib-flow--discovery-source-linked-file-p package file) 1 0))
         (title-score (delib-flow--discovery-title-score terms title))
         (text-score (delib-flow--discovery-text-score terms text))
         (tag-score (delib-flow--discovery-tag-score project
                                                     (delib-flow--zk-note-tags file)))
         (contact-score (delib-flow--discovery-contact-score
                         project
                         note-contacts))
         (source-contact-score (delib-flow--discovery-source-contact-score
                                package
                                note-contacts))
         (shared-link-score (delib-flow--discovery-shared-link-score
                             package
                             note-links)))
    (list (delib-flow--discovery-signal 'linked-project-file linked-file-p 12)
          (delib-flow--discovery-signal 'linked-source-file source-linked-file-p 15)
          (delib-flow--discovery-signal 'title-overlap title-score 3)
          (delib-flow--discovery-signal 'project-tag-overlap tag-score 2)
          (delib-flow--discovery-signal 'project-contact-overlap contact-score 2)
          (delib-flow--discovery-signal 'source-contact-overlap source-contact-score 3)
          (delib-flow--discovery-signal 'shared-project-or-source-link shared-link-score 4)
          (delib-flow--discovery-signal 'text-overlap text-score 1))))

(defun delib-flow--discovery-candidate-score (signals)
  "Return retrieval score from discovery SIGNALS."
  (apply #'+ (mapcar (lambda (signal)
                       (plist-get signal :contribution))
                     signals)))

(defun delib-flow--positive-discovery-signals (signals)
  "Return positively contributing discovery SIGNALS."
  (seq-filter (lambda (signal)
                (> (plist-get signal :contribution) 0))
              signals))

(defun delib-flow--signal-description (signal)
  "Return human-readable description for discovery SIGNAL."
  (format "%s=%s (+%s)"
          (plist-get signal :key)
          (plist-get signal :value)
          (plist-get signal :contribution)))

(defun delib-flow--discovery-reasons (signals)
  "Return human-readable discovery reasons from SIGNALS."
  (let ((positive-signals (delib-flow--positive-discovery-signals signals)))
    (if positive-signals
        (mapcar #'delib-flow--signal-description positive-signals)
      '("no-positive-signals"))))

(defun delib-flow--make-discovery-candidate (file signals)
  "Return discovery candidate for FILE with SIGNALS."
  (list :title (delib-flow--zk-note-title file)
        :file file
        :score (delib-flow--discovery-candidate-score signals)
        :signals signals
        :reasons (delib-flow--discovery-reasons signals)))

(defun delib-flow--scored-discovery-candidates (package files)
  "Return scored discovery candidates for PACKAGE across FILES."
  (let (candidates)
    (dolist (file files (nreverse candidates))
      (unless (delib-flow--discovery-project-file-p file)
        (let* ((signals (delib-flow--discovery-signals package file))
               (score (delib-flow--discovery-candidate-score signals)))
          (when (> score 0)
            (push (delib-flow--make-discovery-candidate file signals)
                  candidates)))))))

(defun delib-flow--sort-discovery-candidates (candidates)
  "Return CANDIDATES sorted by descending score."
  (sort (copy-sequence candidates)
        (lambda (left right)
          (> (plist-get left :score)
             (plist-get right :score)))))

(defun delib-flow--take-discovery-candidates (candidates)
  "Return top discovery CANDIDATES for review."
  (seq-take (delib-flow--sort-discovery-candidates candidates) 5))

(defun delib-flow--discover-reference-material-result (package)
  "Return raw retrieval result for PACKAGE."
  (let* ((terms (delib-flow--discovery-search-terms package))
         (candidates
          (delib-flow--take-discovery-candidates
           (delib-flow--scored-discovery-candidates
            package
            (delib-flow--zk-note-files)))))
    (list :search-terms terms
          :candidate-count (length candidates)
          :candidates candidates)))

(defun delib-flow--retrieved-candidates (package)
  "Return retrieved candidates from PACKAGE."
  (plist-get (plist-get package :working-context) :retrieved-candidates))

(defun delib-flow--retained-filter-candidates (candidates)
  "Return retained subset of CANDIDATES."
  (seq-filter (lambda (candidate)
                (> (plist-get candidate :score) 1))
              candidates))

(defun delib-flow--filter-candidate-text (candidate)
  "Return searchable filter text for CANDIDATE."
  (concat
   (or (plist-get candidate :title) "")
   "\n"
   (if-let ((file (plist-get candidate :file)))
       (delib-flow--zk-note-text file)
     "")))

(defun delib-flow--filter-salience-signals (candidate)
  "Return salient filtering signals for CANDIDATE."
  (let ((text (downcase (delib-flow--filter-candidate-text candidate)))
        signals)
    (when (string-match-p "\\b\\(constraint\\|blocker\\|blocked\\|deadline\\|due\\)\\b" text)
      (push "salient-constraint-context" signals))
    (when (string-match-p "\\b\\(decision\\|decided\\|agreed\\)\\b" text)
      (push "salient-decision-context" signals))
    (when (string-match-p "\\b\\(prefer\\|preference\\|requested\\|request\\)\\b" text)
      (push "salient-preference-context" signals))
    (nreverse signals)))

(defun delib-flow--retain-by-score-threshold-p (candidate)
  "Return non-nil when CANDIDATE clears the score retention threshold."
  (> (plist-get candidate :score) 1))

(defun delib-flow--top-fallback-candidate-p (candidate top-candidate)
  "Return non-nil when CANDIDATE should be retained as TOP-CANDIDATE fallback."
  (and top-candidate
       (equal candidate top-candidate)))

(defun delib-flow--filter-base-retain-reasons (candidate)
  "Return non-fallback retention reasons for CANDIDATE."
  (append
   (when (delib-flow--retain-by-score-threshold-p candidate)
     '("retained-by-score-threshold"))
   (delib-flow--filter-salience-signals candidate)))

(defun delib-flow--filter-retain-reasons (candidate top-candidate)
  "Return retention reasons for CANDIDATE given TOP-CANDIDATE."
  (let ((reasons (delib-flow--filter-base-retain-reasons candidate)))
    (if reasons
        reasons
      (when (delib-flow--top-fallback-candidate-p candidate top-candidate)
        '("retained-as-top-fallback")))))

(defun delib-flow--filter-reject-reasons (candidate)
  "Return rejection reasons for CANDIDATE."
  (or (delib-flow--filter-salience-signals candidate)
      '("rejected-below-score-threshold")))

(defun delib-flow--filter-decision-reasons (candidate status top-candidate)
  "Return filter explanation list for CANDIDATE with STATUS and TOP-CANDIDATE."
  (if (eq status 'retained)
      (delib-flow--filter-retain-reasons candidate top-candidate)
    (delib-flow--filter-reject-reasons candidate)))

(defun delib-flow--filter-annotated-candidate (candidate status top-candidate)
  "Return CANDIDATE annotated with filter STATUS and TOP-CANDIDATE."
  (let ((copy (copy-sequence candidate)))
    (plist-put
     (plist-put
      copy
      :filter-status status)
     :filter-reasons
     (delib-flow--filter-decision-reasons candidate status top-candidate))))

(defun delib-flow--annotated-filter-candidates (candidates selected)
  "Return CANDIDATES annotated with retained/rejected decisions from SELECTED."
  (let ((top-candidate (car candidates)))
    (mapcar (lambda (candidate)
              (delib-flow--filter-annotated-candidate
               candidate
               (if (member candidate selected) 'retained 'rejected)
               top-candidate))
            candidates)))

(defun delib-flow--retained-annotated-candidates (candidates)
  "Return retained annotated subset of CANDIDATES."
  (seq-filter (lambda (candidate)
                (delib-flow--filter-retain-reasons candidate nil))
              candidates))

(defun delib-flow--filter-reasons-text (candidate)
  "Return human-readable filter reasons for CANDIDATE."
  (mapconcat #'identity
             (plist-get candidate :filter-reasons)
             ", "))

(defun delib-flow--filter-reference-material-result (package)
  "Return raw filter result for PACKAGE."
  (let* ((candidates (delib-flow--retrieved-candidates package))
         (retained (delib-flow--retained-annotated-candidates candidates))
         (fallback (and candidates (list (car candidates))))
         (selected (or retained fallback))
         (annotated (delib-flow--annotated-filter-candidates
                     candidates
                     selected)))
    (list :candidate-count (length candidates)
          :retained-count (length selected)
          :retained-candidates
          (seq-filter (lambda (candidate)
                        (eq (plist-get candidate :filter-status) 'retained))
                      annotated)
          :rejected-count (- (length candidates) (length selected))
          :rejected-candidates
          (seq-filter (lambda (candidate)
                        (eq (plist-get candidate :filter-status) 'rejected))
                      annotated))))

(defun delib-flow--source-title (package)
  "Return source title from PACKAGE."
  (plist-get (plist-get package :source) :title))

(defun delib-flow--source-display-title (package)
  "Return a cleaned source title suitable for filing artifact text."
  (let* ((raw (delib-flow--source-title package))
         (normalized (delib-flow--normalize-match-source-title raw)))
    (if (string-empty-p normalized)
        (string-trim (or raw ""))
      normalized)))

(defun delib-flow--capitalize-sentence-start (text)
  "Return TEXT with a capitalized first character when possible."
  (let ((clean (string-trim (or text ""))))
    (if (string-empty-p clean)
        clean
      (concat (upcase (substring clean 0 1))
              (substring clean 1)))))

(defun delib-flow--retained-candidates (package)
  "Return retained candidates from PACKAGE."
  (plist-get (plist-get (plist-get package :working-context) :filtered-context)
             :retained-candidates))

(defun delib-flow--package-contact-emails (package)
  "Return best available contact emails from PACKAGE."
  (let* ((working (plist-get package :working-context))
         (inspect-review (delib-flow--review-record working 'inspect-source))
         (accepted-inspect (plist-get inspect-review :accepted-output))
         (candidate-inspect (plist-get inspect-review :candidate-output)))
    (or (plist-get accepted-inspect :contact-emails)
        (plist-get candidate-inspect :contact-emails)
        (delib-flow--text-emails
         (plist-get (plist-get package :source) :content)))))

(defun delib-flow--make-draft-action (text source)
  "Return draft action object for TEXT and SOURCE."
  (list :kind 'next-action
        :text text
        :source source))

(defun delib-flow--make-draft-waiting-for (text source)
  "Return draft waiting-for object for TEXT and SOURCE."
  (list :kind 'waiting-for
        :text text
        :source source))

(defun delib-flow--make-draft-reference-note (text source note-type)
  "Return draft reference-note object for TEXT, SOURCE, and NOTE-TYPE."
  (list :kind 'reference-note
        :text text
        :source source
        :note-type note-type))

(defconst delib-flow--tag-suggestion-stopwords
  '("a" "an" "and" "article" "as" "at" "by" "for" "from" "idea" "in" "into"
    "it" "my" "note" "of" "on" "or" "presentation" "reminder" "the" "to"
    "up" "with" "write" "your" "email")
  "Words ignored when deriving deterministic tag suggestions.")

(defconst delib-flow--entity-noise-terms
  '("it" "join" "give" "want" "week" "hello" "thanks" "regards"
    "newsletter" "cohort" "update")
  "Low-value entity terms filtered from inspect entities and tag suggestions.")

(defun delib-flow--useful-entity-p (entity)
  "Return non-nil when ENTITY is useful for inspect display and tag derivation."
  (let* ((trimmed (string-trim (or entity "")))
         (lower (downcase trimmed)))
    (and (not (string-empty-p trimmed))
         (not (member lower delib-flow--entity-noise-terms))
         (not (string-match-p "@" trimmed))
         (not (string-match-p "\\`[[:digit:][:punct:]_ -]+\\'" trimmed))
         (not (and (string-match-p "[-_]" trimmed)
                   (not (string-match-p "[[:space:]]" trimmed))))
         (or (string-match-p "[[:space:]]" trimmed)
             (string-match-p "\\`[[:upper:]][[:lower:]]\\{2,\\}\\'" trimmed)))))

(defun delib-flow--filtered-inspect-entities (entities)
  "Return inspect ENTITIES with low-value noise removed."
  (delete-dups
   (seq-filter #'delib-flow--useful-entity-p entities)))

(defun delib-flow--normalize-tag-suggestion (text)
  "Return normalized tag suggestion derived from TEXT, or nil."
  (when-let ((trimmed (and text (string-trim text))))
    (let ((tag (replace-regexp-in-string
                "_+"
                "_"
                (replace-regexp-in-string
                 "[^[:alnum:]]+"
                 "_"
                 (downcase trimmed)))))
      (setq tag (string-trim tag "_+" "_+"))
      (unless (or (string-empty-p tag)
                  (member tag delib-flow--tag-suggestion-stopwords)
                  (string-match-p "\\`[0-9_]+\\'" tag))
        tag))))

(defun delib-flow--source-derived-tag-suggestions (package)
  "Return deterministic tag suggestions derived from PACKAGE source context."
  (let* ((working (plist-get package :working-context))
         (inspect-output (plist-get working :inspect-output))
         (source-type (plist-get inspect-output :source-type))
         (entities (delib-flow--filtered-inspect-entities
                    (plist-get (plist-get inspect-output :analysis) :entities)))
         (email-shaped-p (eq source-type 'email))
         (title-words (split-string
                       (downcase (delib-flow--source-display-title package))
                       "[^[:alnum:]]+"
                       t)))
    (delete-dups
     (delq nil
           (append
            (mapcar #'delib-flow--normalize-tag-suggestion entities)
            (unless email-shaped-p
              (mapcar #'delib-flow--normalize-tag-suggestion
                      (seq-filter (lambda (word) (>= (length word) 4)) title-words)))
            (list (and source-type
                       (delib-flow--normalize-tag-suggestion
                        (symbol-name source-type)))))))))

(defun delib-flow--reference-note-source-tags (item package)
  "Return source-derived tags for reference-note ITEM in PACKAGE.

Only keep source tags that overlap with the note focus, so transient source
entities do not pollute concept-note tags."
  (let* ((source-tags (delib-flow--source-derived-tag-suggestions package))
         (focus-tags
          (delq nil
                (mapcar #'delib-flow--normalize-tag-suggestion
                        (delib-flow--string-words
                         (delib-flow--reference-note-title item))))))
    (seq-filter (lambda (tag)
                  (member tag focus-tags))
                source-tags)))

(defun delib-flow--matched-project-tag-suggestions (package)
  "Return deterministic tag suggestions derived from matched project in PACKAGE."
  (let ((project (delib-flow--matched-project package)))
    (delete-dups
     (delq nil
           (append
            (mapcar #'delib-flow--normalize-tag-suggestion
                    (plist-get project :tags))
            (when-let ((title (plist-get project :title)))
              (list (delib-flow--normalize-tag-suggestion title))))))))

(defun delib-flow--draft-item-tag-suggestions (item package)
  "Return deterministic tag suggestions for draft ITEM in PACKAGE."
  (let ((kind-tags
         (pcase (plist-get item :kind)
           ('next-action '("next_action"))
           ('waiting-for '("waiting_for"))
           ('reference-note
           (list "reference_note"
                  (pcase (plist-get item :note-type)
                    ('project-support "project_support")
                    ('general-pkm "general_pkm")
                    (_ nil))))
           ('project '("project"))
           (_ nil)))
        (item-text-tags
         (when (eq (plist-get item :kind) 'reference-note)
           (let ((focus
                  (replace-regexp-in-string
                   "\\`Create \\(?:general PKM\\|project support\\) note from? \\|\\`Create \\(?:general PKM\\|project support\\) note for "
                   ""
                   (or (plist-get item :text) "")
                   t t)))
             (mapcar #'delib-flow--normalize-tag-suggestion
                     (seq-filter (lambda (word) (>= (length word) 4))
                                 (delib-flow--string-words focus)))))))
    (delete-dups
     (delq nil
           (append
            (and (eq (plist-get item :kind) 'project)
                 (mapcar #'delib-flow--normalize-tag-suggestion
                         (plist-get item :tags)))
           kind-tags
            item-text-tags
            (delib-flow--matched-project-tag-suggestions package)
            (if (eq (plist-get item :kind) 'reference-note)
                (delib-flow--reference-note-source-tags item package)
              (delib-flow--source-derived-tag-suggestions package)))))))

(defun delib-flow--draft-item-with-tag-suggestions (item package)
  "Return ITEM annotated with deterministic tag suggestions for PACKAGE."
  (plist-put (copy-tree item)
             :tag-suggestions
             (delib-flow--draft-item-tag-suggestions item package)))

(defun delib-flow--annotate-draft-item-tags (items package)
  "Return ITEMS annotated with deterministic tag suggestions for PACKAGE."
  (mapcar (lambda (item)
            (delib-flow--draft-item-with-tag-suggestions item package))
          items))

(defun delib-flow--draft-item-stage (item)
  "Return originating draft stage for ITEM, if recorded."
  (plist-get item :draft-stage))

(defun delib-flow--with-draft-item-stage (item stage-id)
  "Return ITEM annotated with originating draft STAGE-ID."
  (plist-put (copy-tree item) :draft-stage stage-id))

(defun delib-flow--with-draft-item-stage-list (items stage-id)
  "Return ITEMS annotated with originating draft STAGE-ID."
  (mapcar (lambda (item)
            (delib-flow--with-draft-item-stage item stage-id))
          items))

(defun delib-flow--remove-draft-stage-items (items stage-id)
  "Return ITEMS excluding those that originated from STAGE-ID."
  (seq-remove (lambda (item)
                (eq (delib-flow--draft-item-stage item) stage-id))
              items))

(defun delib-flow--merge-draft-stage-items (filing stage-id new-items)
  "Return FILING with NEW-ITEMS merged for draft STAGE-ID.

Older items from the same STAGE-ID are removed from draft, approved, and
rejected filing state before NEW-ITEMS are added back into draft state."
  (let* ((draft-items (delib-flow--remove-draft-stage-items
                       (plist-get filing :draft-items)
                       stage-id))
         (approved-items (delib-flow--remove-draft-stage-items
                          (plist-get filing :approved-items)
                          stage-id))
         (rejected-items (delib-flow--remove-draft-stage-items
                          (plist-get filing :rejected-items)
                          stage-id))
         (merged-drafts (append draft-items new-items)))
    (plist-put
     (plist-put
     (plist-put filing :draft-items merged-drafts)
      :approved-items approved-items)
     :rejected-items rejected-items)))

(defun delib-flow--empty-artifact-family-state ()
  "Return empty candidate and selected-draft state for one artifact family."
  (list :candidates nil
        :selected-candidate-id nil
        :selected-draft nil))

(defun delib-flow--initial-artifact-state ()
  "Return initial artifact-family state map for a new run."
  (let (state)
    (dolist (family delib-flow--artifact-family-keys state)
      (setq state
            (plist-put state family
                       (delib-flow--empty-artifact-family-state))))))

(defun delib-flow--artifact-family-for-stage (stage-id)
  "Return artifact family keyword associated with finder STAGE-ID."
  (pcase stage-id
    ('extract-actions 'actions)
    ('extract-waiting-for 'waiting-fors)
    ('suggest-reference-notes 'reference-notes)
    ('propose-new-project 'project-proposals)
    (_ nil)))

(defun delib-flow--run-artifacts (run)
  "Return artifact-family state from RUN."
  (plist-get run :artifacts))

(defun delib-flow--artifact-family-state (run family)
  "Return state plist for artifact FAMILY in RUN."
  (plist-get (delib-flow--run-artifacts run) family))

(defun delib-flow--set-artifact-family-state (run family family-state)
  "Return RUN with artifact FAMILY set to FAMILY-STATE."
  (plist-put run :artifacts
             (plist-put (or (delib-flow--run-artifacts run)
                            (delib-flow--initial-artifact-state))
                        family
                        family-state)))

(defun delib-flow--set-artifact-family-candidates (run family candidates)
  "Return RUN with artifact FAMILY candidate state replaced by CANDIDATES.

This also clears any selected candidate or selected draft for FAMILY."
  (delib-flow--set-artifact-family-state
   run family
   (plist-put
    (plist-put
     (plist-put (or (delib-flow--artifact-family-state run family)
                    (delib-flow--empty-artifact-family-state))
                :candidates candidates)
     :selected-candidate-id nil)
    :selected-draft nil)))

(defun delib-flow--artifact-candidate-id (item)
  "Return stable candidate identifier for ITEM."
  (or (plist-get item :candidate-id)
      (pcase (plist-get item :kind)
        ('project (or (plist-get item :title)
                      (plist-get item :text)))
        (_ (or (plist-get item :text)
               (plist-get item :title))))))

(defun delib-flow--artifact-family-selected-candidate-id (run family)
  "Return selected candidate id for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :selected-candidate-id))

(defun delib-flow--artifact-family-selected-draft (run family)
  "Return selected draft for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :selected-draft))

(defun delib-flow--artifact-family-candidates (run family)
  "Return candidate list for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :candidates))

(defun delib-flow--artifact-family-selected-candidate (run family)
  "Return selected candidate object for artifact FAMILY in RUN."
  (let ((selected-id
         (delib-flow--artifact-family-selected-candidate-id run family)))
    (seq-find
     (lambda (item)
       (equal (delib-flow--artifact-candidate-id item)
              selected-id))
     (delib-flow--artifact-family-candidates run family))))

(defun delib-flow--set-artifact-family-selected-candidate-id (run family candidate-id)
  "Return RUN with artifact FAMILY selected candidate set to CANDIDATE-ID.

Changing the selected candidate clears any existing selected draft for FAMILY."
  (let* ((state (or (delib-flow--artifact-family-state run family)
                    (delib-flow--empty-artifact-family-state)))
         (existing-id (plist-get state :selected-candidate-id))
         (state (plist-put state :selected-candidate-id candidate-id))
         (state (if (equal existing-id candidate-id)
                    state
                  (plist-put state :selected-draft nil))))
    (delib-flow--set-artifact-family-state run family state)))

(defun delib-flow--set-artifact-family-selected-draft (run family draft)
  "Return RUN with artifact FAMILY selected draft set to DRAFT."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put state :selected-draft draft))))

(defun delib-flow--make-artifact-warning (code message &optional severity)
  "Return structured artifact warning with CODE, MESSAGE, and SEVERITY."
  (list :code code
        :message message
        :severity (or severity 'advisory)))

(defun delib-flow--draft-item-warnings (item)
  "Return structured warning list from draft ITEM."
  (plist-get item :warnings))

(defun delib-flow--draft-item-warning-messages (item)
  "Return warning messages from draft ITEM."
  (mapcar (lambda (warning)
            (plist-get warning :message))
          (delib-flow--draft-item-warnings item)))

(defun delib-flow--draft-item-warning-severity (warning)
  "Return normalized WARNING severity."
  (or (plist-get warning :severity) 'advisory))

(defconst delib-flow--draft-item-warning-remediations
  '((weak-next-action-verb . "Replace the opening verb with the concrete next step or deliverable.")
    (vague-action-context . "Name the concrete deliverable, recipient, or change instead of generic follow-up wording.")
    (broad-action-scope . "Split this into a smaller next action that fits one focused work session.")
    (decision-state-action . "Rewrite this as a concrete action, or move it back into context if it is only a decision or status statement.")
    (waiting-for-missing-owner . "Name who owns the response or dependency, ideally with a specific person or email.")
    (waiting-for-vague-blocker . "Name the exact response, approval, or deliverable that is blocking progress.")
    (waiting-for-state-phrasing . "Rewrite it to start with `Waiting for ...` so the blocked dependency is explicit.")
    (project-title-timestamp-noise . "Remove timestamp or journal-heading noise so the proposal uses a stable project title.")
    (project-title-note-shape . "Rewrite the title so it names the project itself, not the raw note, presentation idea, or reminder heading.")
    (project-tags-invalid . "Replace numeric/date fragments with meaningful project tags, or leave tags empty until better ones are known.")
    (project-first-item-generic . "Replace the placeholder with the first concrete deliverable or action that would start the project.")
    (reference-note-missing-title . "Edit the note text so a stable note title can be derived before approval.")
    (reference-note-unsupported-type . "Use a supported note type such as `general-pkm` or `project-support`.")
    (reference-note-template-title . "Add `${title}` to the configured note template before approving this note.")
    (reference-note-project-context . "Match or choose a project before approving a project-support note.")
    (reference-note-reuse-justification . "Explain why this note should live as a reusable general PKM note.")))

(defun delib-flow--draft-item-warning-remediation (warning)
  "Return operator-facing remediation text for WARNING."
  (or (alist-get (plist-get warning :code)
                 delib-flow--draft-item-warning-remediations)
      "Revise this artifact until the issue is resolved before approval."))

(defun delib-flow--blocking-artifact-warning-p (warning)
  "Return non-nil when WARNING should block filing approval."
  (eq (delib-flow--draft-item-warning-severity warning) 'blocking))

(defun delib-flow--draft-item-blocking-warnings (item)
  "Return blocking warnings from draft ITEM."
  (seq-filter #'delib-flow--blocking-artifact-warning-p
              (delib-flow--draft-item-warnings item)))

(defun delib-flow--draft-item-blocking-warning-count (item)
  "Return blocking warning count for draft ITEM."
  (length (delib-flow--draft-item-blocking-warnings item)))

(defun delib-flow--item-blocking-warning-total (items)
  "Return total blocking warning count across draft ITEMS."
  (apply #'+ (mapcar #'delib-flow--draft-item-blocking-warning-count items)))

(defun delib-flow--items-with-blocking-warnings-count (items)
  "Return count of ITEMS carrying at least one blocking warning."
  (seq-count (lambda (item)
               (> (delib-flow--draft-item-blocking-warning-count item) 0))
             items))

(defun delib-flow--draft-item-warning-count (item)
  "Return warning count for draft ITEM."
  (length (delib-flow--draft-item-warnings item)))

(defun delib-flow--draft-item-ready-p (item)
  "Return non-nil when draft ITEM has no blocking warnings."
  (= 0 (delib-flow--draft-item-blocking-warning-count item)))

(defun delib-flow--draft-item-readiness (item)
  "Return readiness symbol for draft ITEM."
  (cond
   ((not (delib-flow--draft-item-ready-p item)) 'blocked)
   ((> (delib-flow--draft-item-warning-count item) 0) 'warning)
   (t 'ready)))

(defun delib-flow--draft-item-readiness-text (item)
  "Return operator-facing readiness text for draft ITEM."
  (pcase (delib-flow--draft-item-readiness item)
    ('blocked
     (format "blocked by %s filing-readiness issue(s)"
             (delib-flow--draft-item-blocking-warning-count item)))
    ('warning
     (format "ready with %s advisory warning(s)"
             (delib-flow--draft-item-warning-count item)))
    (_
     "ready for approval")))

(defun delib-flow--draft-item-remediation-lines (item)
  "Return remediation lines for draft ITEM."
  (mapcar
   (lambda (warning)
     (format "  Fix: %s"
             (delib-flow--draft-item-warning-remediation warning)))
   (delib-flow--draft-item-warnings item)))

(defun delib-flow--draft-item-with-warnings (item warnings)
  "Return ITEM annotated with structured WARNING list."
  (plist-put (copy-sequence item) :warnings warnings))

(defun delib-flow--artifact-text-word-count (text)
  "Return rough word count for artifact TEXT."
  (length (split-string (or text "") "[^[:alnum:]]+" t)))

(defun delib-flow--artifact-leading-word (text)
  "Return downcased leading word from TEXT, if any."
  (when (string-match "\\`[[:space:]]*\\([[:alpha:]]+\\)" (or text ""))
    (downcase (match-string 1 text))))

(defun delib-flow--weak-next-action-verb-p (verb)
  "Return non-nil when VERB signals a weak next-action opener."
  (member verb '("clarify" "review" "check" "handle" "consider")))

(defun delib-flow--action-warning-weak-verb (item)
  "Return warning when action ITEM starts with a weak verb."
  (when-let ((verb (delib-flow--artifact-leading-word (plist-get item :text))))
    (when (delib-flow--weak-next-action-verb-p verb)
      (delib-flow--make-artifact-warning
       'weak-next-action-verb
       (format "Starts with \"%s\", which suggests review or clarification rather than a directly executable next action." verb)))))

(defun delib-flow--action-warning-vague-context (item)
  "Return warning when action ITEM uses vague context wording."
  (let ((text (downcase (or (plist-get item :text) ""))))
    (when (or (string-match-p "next step" text)
              (string-match-p "follow-up context" text))
      (delib-flow--make-artifact-warning
       'vague-action-context
       "Uses vague context wording and does not yet identify a concrete deliverable or target outcome."))))

(defun delib-flow--action-warning-broad-scope (item)
  "Return warning when action ITEM appears broad rather than task-sized."
  (when (> (delib-flow--artifact-text-word-count (plist-get item :text)) 12)
    (delib-flow--make-artifact-warning
     'broad-action-scope
     "Looks longer than a pomodoro-sized next action and may need to be narrowed.")))

(defun delib-flow--action-warning-decision-state (item)
  "Return blocking warning when action ITEM reads like a decision or status statement."
  (let ((text (string-trim (downcase (or (plist-get item :text) "")))))
    (when (or (string-match-p
               "\\`\\(keep\\|maintain\\|continue\\|stay\\|remain\\)\\b"
               text)
              (string-match-p
               "\\b\\(decision\\|decided\\|agreed\\|target\\)\\b"
               text))
      (delib-flow--make-artifact-warning
       'decision-state-action
       "Reads like a decision or status statement rather than a directly executable next action."
       'blocking))))

(defun delib-flow--action-warnings (item)
  "Return structured warning list for next-action ITEM."
  (delq nil
        (list
         (delib-flow--action-warning-weak-verb item)
         (delib-flow--action-warning-vague-context item)
         (delib-flow--action-warning-broad-scope item)
         (delib-flow--action-warning-decision-state item))))

(defun delib-flow--waiting-for-warning-missing-owner (item)
  "Return warning when waiting-for ITEM lacks a clear owner."
  (let ((text (string-trim (or (plist-get item :text) ""))))
    (unless (or (string-match-p "\\bfrom\\b[[:space:]]+[^[:space:]]" text)
                (let* ((case-fold-search t)
                       (owner-fragment
                        (when (string-match
                               "\\`waiting for[[:space:]]+\\(.+?\\)\\(?:[[:space:]]+to\\b\\|[[:space:]]*\\.[[:space:]]*\\'\\|\\'\\)"
                               text)
                          (string-trim (match-string 1 text)))))
                  (and owner-fragment
                       (not (string-match-p
                             "\\`\\(?:confirmation\\|approval\\|response\\|update\\|reply\\|sign-off\\|review\\|decision\\)\\b"
                             owner-fragment))
                       (not (string-empty-p owner-fragment)))))
      (delib-flow--make-artifact-warning
       'waiting-for-missing-owner
       "Does not identify who owns the response or dependency."
       'blocking))))

(defun delib-flow--waiting-for-warning-vague-blocker (item)
  "Return warning when waiting-for ITEM uses vague blocker wording."
  (let ((text (downcase (or (plist-get item :text) ""))))
    (when (string-match-p "concrete response about" text)
      (delib-flow--make-artifact-warning
       'waiting-for-vague-blocker
       "Names a waiting state, but the blocked dependency is still phrased too vaguely."
       'blocking))))

(defun delib-flow--waiting-for-warning-state-phrasing (item)
  "Return warning when waiting-for ITEM lacks waiting-state phrasing."
  (let ((text (downcase (or (plist-get item :text) ""))))
    (unless (string-prefix-p "waiting for" text)
      (delib-flow--make-artifact-warning
       'waiting-for-state-phrasing
       "Does not use explicit waiting-state phrasing."))))

(defun delib-flow--waiting-for-warnings (item)
  "Return structured warning list for waiting-for ITEM."
  (delq nil
        (list
         (delib-flow--waiting-for-warning-missing-owner item)
         (delib-flow--waiting-for-warning-vague-blocker item)
         (delib-flow--waiting-for-warning-state-phrasing item))))

(defun delib-flow--annotate-draft-actions (items)
  "Return action ITEMS annotated with structured warnings."
  (mapcar (lambda (item)
            (delib-flow--draft-item-with-warnings
             item
             (delib-flow--action-warnings item)))
          items))

(defun delib-flow--annotate-draft-waiting-fors (items)
  "Return waiting-for ITEMS annotated with structured warnings."
  (mapcar (lambda (item)
            (delib-flow--draft-item-with-warnings
             item
             (delib-flow--waiting-for-warnings item)))
          items))

(defun delib-flow--filter-ready-draft-items (items)
  "Return only filing-ready ITEMS."
  (seq-filter #'delib-flow--draft-item-ready-p items))

(defun delib-flow--annotate-ready-draft-waiting-fors (items)
  "Return waiting-for ITEMS annotated and filtered to filing-ready drafts."
  (delib-flow--filter-ready-draft-items
   (delib-flow--annotate-draft-waiting-fors items)))

(defun delib-flow--reference-note-warning-missing-title (item)
  "Return warning when reference-note ITEM cannot derive a note title."
  (when (string-empty-p
         (string-trim (or (delib-flow--reference-note-title item) "")))
    (delib-flow--make-artifact-warning
     'reference-note-missing-title
     "Does not produce a usable note title for deterministic filing."
     'blocking)))

(defun delib-flow--reference-note-warning-unsupported-type (item)
  "Return warning when reference-note ITEM uses an unsupported note type."
  (unless (memq (plist-get item :note-type) '(general-pkm project-support))
    (delib-flow--make-artifact-warning
     'reference-note-unsupported-type
     "Uses a note type that the current filing path does not recognize."
     'blocking)))

(defun delib-flow--reference-note-warning-template-title (item)
  "Return warning when reference-note ITEM template omits title expansion."
  (unless (delib-flow--reference-note-template-has-title-p item)
    (delib-flow--make-artifact-warning
     'reference-note-template-title
     "Configured note template does not include a note-title expansion, so note-title filing readiness is weak."
     'blocking)))

(defun delib-flow--reference-note-warning-general-reuse (item)
  "Return warning when general PKM reference-note ITEM lacks reuse justification."
  (when (eq (plist-get item :note-type) 'general-pkm)
    (delib-flow--make-artifact-warning
     'reference-note-reuse-justification
     "General PKM note does not yet justify broader reuse beyond this single source item.")))

(defun delib-flow--reference-note-warning-project-context (item package)
  "Return warning when project-support ITEM lacks matched project context in PACKAGE."
  (when (and (eq (plist-get item :note-type) 'project-support)
             (not (delib-flow--matched-project-title package)))
    (delib-flow--make-artifact-warning
     'reference-note-project-context
     "Project-support note is missing matched-project context needed for support-note filing."
     'blocking)))

(defun delib-flow--reference-note-warnings (item package)
  "Return structured warning list for reference-note ITEM in PACKAGE."
  (delq nil
        (list
         (delib-flow--reference-note-warning-missing-title item)
         (delib-flow--reference-note-warning-unsupported-type item)
         (delib-flow--reference-note-warning-template-title item)
         (delib-flow--reference-note-warning-general-reuse item)
         (delib-flow--reference-note-warning-project-context item package))))

(defun delib-flow--annotate-draft-reference-notes (items package)
  "Return reference-note ITEMS annotated with structured warnings for PACKAGE."
  (mapcar (lambda (item)
            (delib-flow--draft-item-with-warnings
             item
             (delib-flow--reference-note-warnings item package)))
          items))

(defun delib-flow--items-with-warnings-count (items)
  "Return count of ITEMS that carry at least one warning."
  (seq-count (lambda (item)
               (> (delib-flow--draft-item-warning-count item) 0))
             items))

(defun delib-flow--item-warning-total (items)
  "Return total warning count across draft ITEMS."
  (apply #'+ (mapcar #'delib-flow--draft-item-warning-count items)))

(defun delib-flow--make-draft-project (title state first-item tags)
  "Return draft project object for TITLE with STATE, FIRST-ITEM, and TAGS."
  (list :kind 'project
        :title title
        :state state
        :first-item first-item
        :tags tags
        :text title))

(defconst delib-flow--action-line-verbs
  '("send" "write" "reply" "schedule" "confirm" "share" "draft" "update"
    "call" "ask" "prepare" "file" "create" "summarize" "review"
    "gather")
  "Verbs treated as concrete action starters in source evidence lines.")

(defun delib-flow--source-body-lines (package)
  "Return trimmed non-empty source body lines from PACKAGE."
  (seq-filter
   (lambda (line)
     (not (string-empty-p line)))
   (mapcar #'string-trim
           (split-string
            (delib-flow--source-body-text
             (plist-get (plist-get package :source) :content))
            "\n"))))

(defun delib-flow--normalize-source-evidence-line (line)
  "Return LINE normalized for draft-artifact reuse."
  (let ((normalized (string-trim (or line ""))))
    (setq normalized
          (replace-regexp-in-string
           "\\`[-+*][[:space:]]*" "" normalized))
    (setq normalized
          (replace-regexp-in-string
           "\\`\\(?:[[:digit:]]+\\.\\|[[:alpha:]]+[:]\\)[[:space:]]*"
           "" normalized))
    (string-trim-right normalized "[[:space:].:;,-]+")))

(defun delib-flow--action-evidence-line-p (line)
  "Return non-nil when LINE looks like a concrete next action."
  (when-let ((verb (delib-flow--artifact-leading-word line)))
    (member verb delib-flow--action-line-verbs)))

(defun delib-flow--source-action-line (package)
  "Return best action-like source line from PACKAGE, or nil."
  (seq-find
   #'delib-flow--action-evidence-line-p
   (mapcar #'delib-flow--normalize-source-evidence-line
           (delib-flow--source-body-lines package))))

(defun delib-flow--normalize-waiting-for-line (line)
  "Return LINE normalized to explicit waiting-for phrasing."
  (let ((normalized (delib-flow--normalize-source-evidence-line line)))
    (cond
     ((string-prefix-p "waiting for" (downcase normalized))
      (concat "Waiting for "
              (string-trim
               (substring normalized (length "waiting for")))))
     ((string-match-p "\\`[Aa]waiting\\b" normalized)
      (replace-regexp-in-string
       "\\`[Aa]waiting\\b" "Waiting for" normalized t t))
     (t nil))))

(defun delib-flow--source-waiting-for-line (package)
  "Return best waiting-for line from PACKAGE source text, or nil."
  (seq-find #'identity
            (mapcar #'delib-flow--normalize-waiting-for-line
                    (delib-flow--source-body-lines package))))

(defun delib-flow--source-reference-note-type (package)
  "Return preferred source-note type for PACKAGE."
  (if (delib-flow--matched-project-title package)
      'project-support
    'general-pkm))

(defconst delib-flow--reference-note-generic-focus-regexp
  (concat
   "\\b\\("
   "cohort\\|newsletter\\|update\\|week [0-9]+\\|summary\\|overview\\|notes?"
   "\\|context\\|reminder"
   "\\)\\b")
  "Regexp matching overly generic reference-note focus terms.")

(defun delib-flow--reference-note-focus-usable-p (focus)
  "Return non-nil when FOCUS is specific enough for a durable note."
  (let ((text (string-trim (or focus ""))))
    (and (not (string-empty-p text))
         (>= (length text) 8)
         (not (string-match-p
               delib-flow--reference-note-generic-focus-regexp
               (downcase text))))))

(defun delib-flow--normalize-reference-note-focus (focus)
  "Return normalized reference-note FOCUS text."
  (let ((normalized (string-trim (or focus ""))))
    (setq normalized
          (replace-regexp-in-string "\\`[[:space:][:punct:]]+" "" normalized))
    (setq normalized
          (replace-regexp-in-string "[[:space:][:punct:]]+\\'" "" normalized))
    (setq normalized
          (replace-regexp-in-string "[[:space:]\n]+" " " normalized))
    (delib-flow--capitalize-sentence-start normalized)))

(defun delib-flow--email-digest-reference-note-focuses (digest)
  "Return deterministic durable note focuses extracted from email DIGEST."
  (let* ((subject (delib-flow--normalize-reference-note-focus
                   (plist-get digest :subject)))
         (lines (split-string (or (plist-get digest :plain-body) "") "\n"))
         body-focuses)
    (while lines
      (let* ((line (car lines))
             (next (cadr lines))
             (trimmed (string-trim line)))
        (cond
         ((and (string-match-p "\\`[-=_[:space:]]\\{3,\\}\\'" trimmed)
               next
               (delib-flow--reference-note-focus-usable-p next))
          (push (delib-flow--normalize-reference-note-focus next) body-focuses))
         ((string-match
           "highlight here is \\(.+?\\)\\(?:[.?!]\\|$\\)"
           trimmed)
          (push
           (delib-flow--normalize-reference-note-focus
            (match-string 1 trimmed))
           body-focuses))
         ((string-match
           "was about \\(.+?\\)\\(?:[.?!]\\|$\\)"
           trimmed)
          (push
           (delib-flow--normalize-reference-note-focus
            (match-string 1 trimmed))
           body-focuses))
         ((string-match
           "\\`The idea:[[:space:]]*\\(.+?\\)\\(?:[.?!]\\|$\\)"
           trimmed)
          (push
           (delib-flow--normalize-reference-note-focus
            (match-string 1 trimmed))
           body-focuses))
         ((string-match
           "\\`Going up in value:[[:space:]]*\\(.+?\\)\\(?:[.?!]\\|$\\)"
           trimmed)
          (push
           (delib-flow--normalize-reference-note-focus
            (match-string 1 trimmed))
           body-focuses))))
      (setq lines (cdr lines)))
    (let* ((normalized-body
            (seq-filter #'delib-flow--reference-note-focus-usable-p
                        (delete-dups (nreverse body-focuses))))
           (subject-focus
            (and (null normalized-body)
                 (delib-flow--reference-note-focus-usable-p subject)
                 (list subject))))
      (seq-take
       (delete-dups
        (append normalized-body subject-focus))
       4))))

(defun delib-flow--source-reference-note-focuses (package)
  "Return deterministic reference-note focuses derived from PACKAGE source."
  (let ((digest (delib-flow--package-email-digest package)))
    (if digest
        (delib-flow--email-digest-reference-note-focuses digest)
      (let ((title (delib-flow--normalize-reference-note-focus
                    (delib-flow--source-display-title package))))
        (if (delib-flow--reference-note-focus-usable-p title)
            (list title)
          nil)))))

(defun delib-flow--source-reference-notes (package)
  "Return deterministic source-derived reference notes for PACKAGE."
  (unless (and (delib-flow--package-transactional-email-p package)
               (not (delib-flow--matched-project-title package)))
    (let ((note-type (delib-flow--source-reference-note-type package)))
      (mapcar
       (lambda (focus)
         (delib-flow--make-draft-reference-note
          (if (eq note-type 'project-support)
              (format "Create project support note from %s" focus)
            (format "Create general PKM note for %s" focus))
          'source
          note-type))
       (delib-flow--source-reference-note-focuses package)))))

(defun delib-flow--cached-email-inspect-digest-for-source (source)
  "Return reduced email digest for SOURCE, or nil when SOURCE is not email-shaped."
  (when (delib-flow--email-source-shape-p source)
    (delib-flow--email-inspect-digest source)))

(defun delib-flow--package-email-digest (package)
  "Return deterministic email digest plist for PACKAGE when source is email-shaped."
  (let* ((working (plist-get package :working-context))
         (cached (plist-get working :email-inspect-digest))
         (source (plist-get package :source)))
    (or cached
        (delib-flow--cached-email-inspect-digest-for-source source))))

(defun delib-flow--package-transactional-email-p (package)
  "Return non-nil when PACKAGE source is a transactional email."
  (let* ((working (plist-get package :working-context))
         (inspect-output (plist-get working :inspect-output))
         (digest (delib-flow--package-email-digest package)))
    (or (string-equal (plist-get digest :type-hint)
                      "transactional notification")
        (and (eq (plist-get inspect-output :source-type) 'email)
             (string-match-p
              "wishlist\\|auto-generated\\|notification"
              (downcase
               (or (plist-get (plist-get inspect-output :analysis) :summary)
                   (plist-get inspect-output :body-preview)
                   "")))))))

(defun delib-flow--candidate-evidence-lines (candidate)
  "Return normalized non-empty evidence lines from retained CANDIDATE."
  (when-let ((file (plist-get candidate :file)))
    (seq-filter
     (lambda (line)
       (and (not (string-empty-p line))
            (not (string-match-p "\\`#\\+" line))
            (not (string-match-p "\\`\\*+ " line))))
     (mapcar #'delib-flow--normalize-source-evidence-line
             (split-string (delib-flow--zk-note-text file) "\n")))))

(defun delib-flow--candidate-action-line (candidate)
  "Return best action-like evidence line from retained CANDIDATE."
  (seq-find #'delib-flow--action-evidence-line-p
            (delib-flow--candidate-evidence-lines candidate)))

(defun delib-flow--candidate-waiting-line (candidate)
  "Return best waiting-for evidence line from retained CANDIDATE."
  (seq-find #'identity
            (mapcar #'delib-flow--normalize-waiting-for-line
                    (delib-flow--candidate-evidence-lines candidate))))

(defun delib-flow--candidate-note-focus-line-p (line)
  "Return non-nil when LINE looks note-worthy for a support note title."
  (string-match-p
   "\\b\\(constraint\\|blocker\\|blocked\\|decision\\|decided\\|agreed\\|deadline\\|due\\|prefer\\|preference\\|requested\\|request\\)\\b"
   (downcase line)))

(defun delib-flow--candidate-note-focus-line (candidate)
  "Return best raw focus line from retained CANDIDATE, or nil."
  (when-let ((file (plist-get candidate :file)))
    (seq-find
     #'delib-flow--candidate-note-focus-line-p
     (seq-filter
      (lambda (line)
        (and (not (string-empty-p line))
             (not (string-match-p "\\`#\\+" line))
             (not (string-match-p "\\`\\*+ " line))))
      (mapcar #'string-trim
              (split-string (delib-flow--zk-note-text file) "\n"))))))

(defun delib-flow--candidate-note-focus (candidate)
  "Return best support-note focus text from retained CANDIDATE."
  (or (when-let ((line (delib-flow--candidate-note-focus-line candidate)))
        (string-trim-right line "[[:space:].:;,-]+"))
      (plist-get candidate :title)))

(defun delib-flow--source-title-action (package)
  "Return a draft action derived from PACKAGE source title."
  (delib-flow--make-draft-action
   (or (delib-flow--source-action-line package)
       (format "Write follow-up note for %s"
               (delib-flow--source-display-title package)))
   'source))

(defun delib-flow--retained-candidate-action (candidate)
  "Return a draft action derived from retained CANDIDATE."
  (delib-flow--make-draft-action
   (or (delib-flow--candidate-action-line candidate)
       (format "Summarize %s into project notes"
               (plist-get candidate :title)))
   'retained-context))

(defun delib-flow--retained-candidate-actions (package)
  "Return retained-candidate draft actions for PACKAGE."
  (mapcar #'delib-flow--retained-candidate-action
          (delib-flow--retained-candidates package)))

(defun delib-flow--waiting-for-owner-label (package)
  "Return owner label for waiting-for drafts derived from PACKAGE."
  (or (car (delib-flow--package-contact-emails package))
      "project owner"))

(defun delib-flow--source-title-waiting-for (package)
  "Return a waiting-for item derived from PACKAGE source title."
  (delib-flow--make-draft-waiting-for
   (or (delib-flow--source-waiting-for-line package)
       (format "Waiting for confirmation from %s on %s"
               (delib-flow--waiting-for-owner-label package)
               (delib-flow--source-display-title package)))
   'source))

(defun delib-flow--retained-candidate-waiting-for (candidate package)
  "Return a waiting-for item derived from retained CANDIDATE in PACKAGE."
  (delib-flow--make-draft-waiting-for
   (or (delib-flow--candidate-waiting-line candidate)
       (format "Waiting for confirmation from %s on %s"
               (delib-flow--waiting-for-owner-label package)
               (plist-get candidate :title)))
   'retained-context))

(defun delib-flow--retained-candidate-waiting-fors (package)
  "Return retained-candidate waiting-for items for PACKAGE."
  (mapcar (lambda (candidate)
            (delib-flow--retained-candidate-waiting-for candidate package))
          (delib-flow--retained-candidates package)))

(defun delib-flow--source-title-reference-note (package)
  "Return a general reference note derived from PACKAGE source title."
  (car (delib-flow--source-reference-notes package)))

(defun delib-flow--retained-candidate-reference-note (candidate)
  "Return a support-note item derived from retained CANDIDATE."
  (delib-flow--make-draft-reference-note
   (format "Create project support note from %s"
           (delib-flow--candidate-note-focus candidate))
   'retained-context
   'project-support))

(defun delib-flow--retained-candidate-reference-notes (package)
  "Return retained-candidate reference-note items for PACKAGE."
  (when (delib-flow--matched-project-title package)
    (mapcar #'delib-flow--retained-candidate-reference-note
            (delib-flow--retained-candidates package))))

(defun delib-flow--extract-actions-result (package)
  "Return raw action-extraction result for PACKAGE."
  (let* ((source-action (delib-flow--source-title-action package))
         (retained-actions (delib-flow--retained-candidate-actions package))
         (actions (delib-flow--annotate-draft-actions
                   (delib-flow--annotate-draft-item-tags
                    (cons source-action retained-actions)
                    package))))
    (list :candidate-count (length actions)
          :warning-count (delib-flow--item-warning-total actions)
          :warning-item-count (delib-flow--items-with-warnings-count actions)
          :blocking-warning-count (delib-flow--item-blocking-warning-total actions)
          :blocking-warning-item-count
          (delib-flow--items-with-blocking-warnings-count actions)
          :actions actions)))

(defun delib-flow--extract-waiting-for-result (package)
  "Return raw waiting-for extraction result for PACKAGE."
  (let* ((source-item (delib-flow--source-title-waiting-for package))
         (retained-items (delib-flow--retained-candidate-waiting-fors package))
         (items (delib-flow--annotate-ready-draft-waiting-fors
                 (delib-flow--annotate-draft-item-tags
                  (cons source-item retained-items)
                  package))))
    (list :candidate-count (length items)
          :warning-count (delib-flow--item-warning-total items)
          :warning-item-count (delib-flow--items-with-warnings-count items)
          :blocking-warning-count (delib-flow--item-blocking-warning-total items)
          :blocking-warning-item-count
          (delib-flow--items-with-blocking-warnings-count items)
          :waiting-fors items)))

(defun delib-flow--suggest-reference-notes-result (package)
  "Return raw reference-note suggestion result for PACKAGE."
  (let* ((source-items (delib-flow--source-reference-notes package))
         (retained-items
          (delib-flow--retained-candidate-reference-notes package))
         (items (delib-flow--annotate-draft-reference-notes
                 (delib-flow--annotate-draft-item-tags
                  (append source-items retained-items)
                  package)
                 package)))
    (list :candidate-count (length items)
          :warning-count (delib-flow--item-warning-total items)
          :warning-item-count (delib-flow--items-with-warnings-count items)
          :blocking-warning-count (delib-flow--item-blocking-warning-total items)
          :blocking-warning-item-count
          (delib-flow--items-with-blocking-warnings-count items)
          :reference-notes items)))

(defun delib-flow--selected-reference-note-candidate-for-drafting (package)
  "Return selected reference-note candidate from PACKAGE for item-local drafting."
  (or (delib-flow--selected-reference-note-candidate-from-package package)
      (let ((item (condition-case nil
                      (delib-flow--filing-selection-choice package)
                    (error nil))))
        (when (eq (plist-get item :kind) 'reference-note)
          item))
      (delib-flow--artifact-family-selected-candidate package 'reference-notes)))

(defun delib-flow--drafted-reference-note-item (item package &optional draft-body reason)
  "Return ITEM enriched as a drafted reference note for PACKAGE.

DRAFT-BODY and REASON override the deterministic defaults when provided."
  (let ((draft (copy-tree item)))
    (setq draft
          (plist-put draft :draft-body
                     (or draft-body
                         (delib-flow--reference-note-seeded-body item package))))
    (setq draft
          (plist-put draft :draft-reason
                     (or reason
                         "Drafted the selected note with seeded structure and source support.")))
    draft))

(defun delib-flow--draft-selected-reference-note-result (package)
  "Return raw drafted-note output for the selected reference-note in PACKAGE."
  (let ((candidate (delib-flow--selected-reference-note-candidate-for-drafting package)))
    (unless (eq (plist-get candidate :kind) 'reference-note)
      (error "Select one reference-note candidate before drafting it"))
    (let ((drafted-item (delib-flow--drafted-reference-note-item candidate package)))
      (list :candidate candidate
            :drafted-item drafted-item
            :reason "Drafted the selected note only. Regenerate this note if you want a new pass without replacing the full note queue."))))

(defun delib-flow--project-proposal-tags (package)
  "Return deterministic project tags derived from PACKAGE."
  (let* ((working (plist-get package :working-context))
         (inspect-output (plist-get working :inspect-output))
         (entities (delib-flow--filtered-inspect-entities
                    (plist-get (plist-get inspect-output :analysis) :entities)))
         (entity-tags
          (mapcar (lambda (entity)
                    (replace-regexp-in-string
                     "[^[:alnum:]]+" "_"
                     (downcase (string-trim entity))))
                  entities))
         (title-words
          (seq-filter
           (lambda (word)
             (and (>= (length word) 3)
                  (not (string-match-p "\\`[0-9]+\\'" word))
                  (not (member word delib-flow--project-proposal-tag-stopwords))))
           (delib-flow--string-words
            (delib-flow--project-proposal-title package)))))
    (seq-take
     (delete-dups
      (append
       (seq-filter (lambda (tag)
                     (not (or (string-empty-p tag)
                              (string-match-p "\\`[0-9_]+\\'" tag))))
                   entity-tags)
       title-words))
     3)))

(defun delib-flow--project-proposal-title (package)
  "Return deterministic project title derived from PACKAGE."
  (let* ((display-title (delib-flow--source-display-title package))
         (stripped (replace-regexp-in-string
                    delib-flow--project-proposal-title-prefix-pattern
                    ""
                    display-title
                    t
                    t))
         (clean (string-trim stripped)))
    (delib-flow--capitalize-sentence-start
     (if (string-empty-p clean)
         display-title
       clean))))

(defun delib-flow--project-proposal-first-item (package)
  "Return deterministic first project item derived from PACKAGE."
  (delib-flow--make-draft-action
   (or (delib-flow--source-action-line package)
       (let ((project-title (delib-flow--project-proposal-title package)))
         (if (string-match-p "\\b\\(presentation\\|article\\)\\b"
                             (downcase (delib-flow--source-display-title package)))
             (format "Draft outline for %s" project-title)
           (format "Define first deliverable for %s" project-title))))
   'project-proposal))

(defun delib-flow--project-proposal-warning-list (item package)
  "Return structured warning list for proposed project ITEM from PACKAGE."
  (let ((title (or (plist-get item :title) ""))
        (tags (or (plist-get item :tags) '()))
        warnings)
    (when (string-match-p
           "\\`\\(?:<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*>\\|\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\]\\)"
           title)
      (push (delib-flow--make-artifact-warning
             'project-title-timestamp-noise
             "Proposed project title still contains source timestamp or journal-heading noise."
             'blocking)
            warnings))
    (when (string-match-p delib-flow--project-proposal-title-prefix-pattern
                          (downcase title))
      (push (delib-flow--make-artifact-warning
             'project-title-note-shape
             "Proposed project title still reads like the raw note heading instead of a stable project identity."
             'blocking)
            warnings))
    (when (seq-some (lambda (tag)
                      (or (string-match-p "\\`[0-9]+\\'" tag)
                          (string-match-p "\\`[0-9]+_[0-9_]*\\'" tag)))
                    tags)
      (push (delib-flow--make-artifact-warning
             'project-tags-invalid
             "Proposed project tags still include numeric/date fragments."
             'blocking)
            warnings))
    (when (string-match-p "\\`Clarify the first concrete step\\.?\\'"
                          (plist-get (plist-get item :first-item) :text))
      (push (delib-flow--make-artifact-warning
             'project-first-item-generic
             "First project item is still a generic placeholder instead of a concrete project-specific step."
             'advisory)
            warnings))
    (nreverse warnings)))

(defun delib-flow--proposed-project-item (package)
  "Return deterministic proposed project artifact derived from PACKAGE."
  (let* ((item
          (delib-flow--make-draft-project
           (delib-flow--project-proposal-title package)
           'active
           (delib-flow--project-proposal-first-item package)
           (delib-flow--project-proposal-tags package))))
    (delib-flow--draft-item-with-warnings
     (delib-flow--draft-item-with-tag-suggestions item package)
     (delib-flow--project-proposal-warning-list item package))))

(defun delib-flow--propose-new-project-result (package)
  "Return raw new-project proposal result for PACKAGE."
  (let* ((project-item (delib-flow--proposed-project-item package))
         (first-item (plist-get project-item :first-item)))
    (list :project project-item
          :project-title (plist-get project-item :title)
          :project-state (plist-get project-item :state)
          :tags (plist-get project-item :tags)
          :first-item first-item
          :reason "No project matched, so a new project checklist was prepared.")))

(defun delib-flow--cloud-model-choice (package)
  "Return selected cloud model for PACKAGE."
  (or (plist-get (plist-get package :routing) :default-cloud-model)
      "cloud-model-unconfigured"))

(defun delib-flow--cloud-provider-name (model)
  "Return provider name derived from MODEL."
  (car (split-string (or model "") "[:/]" t)))

(defun delib-flow--cloud-provider-policy (provider)
  "Return provider policy for PROVIDER."
  (or (cdr (assoc provider delib-flow-cloud-provider-policy-alist))
      (cdr (assoc 'default delib-flow-cloud-provider-policy-alist))
      (list :enabled t :policy-profile delib-flow-cloud-policy-profile)))

(defun delib-flow--cloud-policy-enabled-p (policy)
  "Return non-nil when POLICY allows cloud routing."
  (plist-get policy :enabled))

(defun delib-flow--cloud-policy-profile (policy)
  "Return sanitization policy profile from POLICY."
  (or (plist-get policy :policy-profile)
      delib-flow-cloud-policy-profile))

(defun delib-flow--decide-cloud-pass-result (package)
  "Return raw cloud-routing decision result for PACKAGE."
  (let* ((target-stage (delib-flow--cloud-target-stage-choice package))
         (model (delib-flow--cloud-model-choice package))
         (provider (delib-flow--cloud-provider-name model))
         (policy (delib-flow--cloud-provider-policy provider)))
    (unless (delib-flow--cloud-policy-enabled-p policy)
      (error "Cloud routing is disabled for provider %s" provider))
    (list :route 'cloud
          :target-stage target-stage
          :selected-model model
          :selected-provider provider
          :policy-profile (delib-flow--cloud-policy-profile policy)
          :cloud-switch-pending t
          :sanitization-status 'required
          :reason
          (format "Cloud routing is pending sanitized package preparation for %s."
                  (delib-flow--stage-label target-stage)))))

(defun delib-flow--sanitize-basic-cloud-text (text)
  "Return basic deterministic sanitization for TEXT."
  (let ((sanitized (or text "")))
    (setq sanitized
          (replace-regexp-in-string
           "[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]+" "[redacted-email]"
           sanitized))
    (setq sanitized
          (replace-regexp-in-string
           "https?://[^][ \"\n\t)]+" "[redacted-url]" sanitized))
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       "[A-Z][A-Za-z0-9_-]+" "[redacted-name]" sanitized t t))))

(defun delib-flow--sanitize-project-terms (text package)
  "Return TEXT with project-identifying terms redacted for PACKAGE."
  (let* ((source-title (plist-get (plist-get package :source) :title))
         (project-match
          (plist-get (plist-get package :working-context) :project-match))
         (project-title
          (plist-get (plist-get project-match :best-project) :title))
         (terms (delete-dups
                 (append (delib-flow--string-words source-title)
                         (delib-flow--string-words project-title))))
         (sanitized (or text ""))
         (case-fold-search nil))
    (dolist (term terms sanitized)
      (when (> (length term) 2)
        (setq sanitized
              (replace-regexp-in-string
               (format "\\(^\\|[^[:alnum:]_-]\\)\\(%s\\)\\([^[:alnum:]_-]\\|$\\)"
                       (regexp-quote term))
               "\\1[redacted-project]\\3"
               sanitized
               t))))))

(defun delib-flow--sanitize-strict-cloud-text (text package)
  "Return strict deterministic sanitization for TEXT and PACKAGE."
  (let ((sanitized (delib-flow--sanitize-basic-cloud-text text)))
    (setq sanitized
          (replace-regexp-in-string
           "\\[\\[file:[^]]+\\]\\[[^]]*\\]\\]" "[redacted-link]" sanitized t t))
    (delib-flow--sanitize-project-terms sanitized package)))

(defun delib-flow--sanitize-cloud-text (text package profile)
  "Return deterministically sanitized TEXT for PACKAGE under PROFILE."
  (if (eq profile 'strict)
      (delib-flow--sanitize-strict-cloud-text text package)
    (delib-flow--sanitize-basic-cloud-text text)))

(defun delib-flow--sanitize-cloud-lines (package)
  "Return sanitized package lines for PACKAGE."
  (let* ((source (plist-get package :source))
         (working (plist-get package :working-context))
         (routing (plist-get package :routing))
         (profile (or (plist-get routing :cloud-policy-profile)
                      delib-flow-cloud-policy-profile))
         (source-title (plist-get source :title))
         (source-content (plist-get source :content))
         (retained-context (plist-get working :retained-context)))
    (list
     (format "Sanitized source title: %s"
             (delib-flow--sanitize-cloud-text source-title package profile))
     (format "Sanitized source snapshot: %s"
             (delib-flow--sanitize-cloud-text source-content package profile))
     (format "Sanitized retained context: %s"
             (delib-flow--sanitize-cloud-text retained-context package profile)))))

(defun delib-flow--sanitize-for-cloud-result (package)
  "Return raw cloud-sanitization result for PACKAGE."
  (let ((sanitized-lines (delib-flow--sanitize-cloud-lines package)))
    (list :sanitized-package (mapconcat #'identity sanitized-lines "\n")
          :sanitization-status 'prepared
          :cloud-switch-pending t
          :reason "Sanitized package is ready for review before cloud send.")))

(defun delib-flow--cloud-review-block (package)
  "Return the cloud-review editable block from PACKAGE."
  (alist-get 'cloud-package-review
             (plist-get (plist-get package :ui) :editable-blocks)))

(defun delib-flow--reviewed-cloud-package (package)
  "Return reviewed cloud package text from PACKAGE."
  (delib-flow--editable-block-text
   (delib-flow--cloud-review-block package)))

(defun delib-flow--approve-cloud-send-result (package)
  "Return raw cloud-send approval result for PACKAGE."
  (list :approved-package (delib-flow--reviewed-cloud-package package)
        :sanitization-status 'approved
        :cloud-switch-pending t
        :reason "Reviewed sanitized package is approved for cloud send."))

(defun delib-flow--default-local-stage-adapter (descriptor package)
  "Execute DESCRIPTOR locally with PACKAGE and return raw output."
  (funcall (plist-get descriptor :executor) package))

(defun delib-flow--default-cloud-stage-adapter (descriptor package)
  "Execute DESCRIPTOR through the default cloud adapter with PACKAGE."
  (funcall (plist-get descriptor :executor) package))

(defun delib-flow--normalize-stage-output (stage-id raw-output)
  "Return normalized output for STAGE-ID from RAW-OUTPUT."
  (funcall (plist-get (delib-flow--stage-descriptor stage-id) :normalizer)
           raw-output))

(defun delib-flow--make-stage-entry (stage-id package raw-output normalized-output)
  "Return a completed stage entry for STAGE-ID."
  (list :stage-id stage-id
        :label (delib-flow--stage-label stage-id)
        :status 'completed
        :review-state 'pending-review
        :prompt-id (plist-get package :prompt-id)
        :input-package (delib-flow--plain-value package)
        :raw-output (delib-flow--plain-value raw-output)
        :normalized-output (delib-flow--plain-value normalized-output)
        :provider 'local
        :started-at (current-time)
        :ended-at (current-time)))

(defun delib-flow--make-stage-failure-entry (stage-id package message)
  "Return a failed stage entry for STAGE-ID with MESSAGE."
  (list :stage-id stage-id
        :label (delib-flow--stage-label stage-id)
        :status 'failed
        :review-state 'error
        :prompt-id (plist-get package :prompt-id)
        :input-package (delib-flow--plain-value package)
        :raw-output nil
        :normalized-output (delib-flow--plain-value message)
        :provider 'local
        :started-at (current-time)
        :ended-at (current-time)))

(defun delib-flow--stage-entry-with-provider (entry provider)
  "Return ENTRY updated to record execution PROVIDER."
  (plist-put entry :provider provider))

(defun delib-flow--cloud-shadow-stage-id (entry)
  "Return rerouted cloud target stage id represented by ENTRY, if any."
  (cond
   ((eq (plist-get entry :status) 'completed)
    (plist-get (plist-get entry :raw-output) :target-stage))
   ((eq (plist-get entry :stage-id) 'run-cloud-stage)
    (delib-flow--cloud-target-stage
     (plist-get (plist-get entry :input-package) :routing)))
   (t nil)))

(defun delib-flow--cloud-shadow-entry-p (entry)
  "Return non-nil when ENTRY is a deferred rerouted cloud stage record."
  (plist-get entry :cloud-shadow-p))

(defun delib-flow--stage-entry-counts-as-executed-p (entry)
  "Return non-nil when ENTRY should count as executed for stage legality."
  (if (delib-flow--cloud-shadow-entry-p entry)
      (plist-get entry :applied-p)
    t))

(defun delib-flow--cloud-shadow-normalized-output (entry)
  "Return normalized output text for rerouted cloud shadow ENTRY."
  (if (eq (plist-get entry :status) 'completed)
      (plist-get (plist-get entry :raw-output) :target-stage-normalized-output)
    (format "Cloud execution failed before completing rerouted stage %s.\n%s"
            (delib-flow--stage-label
             (delib-flow--cloud-shadow-stage-id entry))
            (plist-get entry :normalized-output))))

(defun delib-flow--make-cloud-shadow-entry (entry)
  "Return a first-class rerouted cloud stage record derived from ENTRY."
  (when-let ((stage-id (delib-flow--cloud-shadow-stage-id entry)))
    (unless (eq stage-id 'run-cloud-stage)
      (list :stage-id stage-id
            :label (delib-flow--stage-label stage-id)
            :status (plist-get entry :status)
            :review-state (if (eq (plist-get entry :status) 'completed)
                              'pending-review
                            'error)
            :prompt-id (delib-flow--stage-prompt-id stage-id)
            :input-package
            (plist-put (copy-tree (plist-get entry :input-package))
                       :stage-id stage-id)
            :raw-output (if (eq (plist-get entry :status) 'completed)
                            (plist-get (plist-get entry :raw-output)
                                       :target-stage-raw-output)
                          nil)
            :normalized-output
            (delib-flow--cloud-shadow-normalized-output entry)
            :provider 'cloud
            :cloud-shadow-p t
            :applied-p nil
            :started-at (plist-get entry :started-at)
            :ended-at (plist-get entry :ended-at)))))

(defun delib-flow--append-history-entry (history entry)
  "Return HISTORY with ENTRY appended and latest fields updated."
  (let ((entries (append (plist-get history :entries) (list entry))))
    (plist-put
     (plist-put
      (plist-put history :entries entries)
      :latest-stage (plist-get entry :stage-id))
     :latest-status (plist-get entry :status))))

(defun delib-flow--append-stage-entry (run entry)
  "Return RUN with ENTRY appended to stage history."
  (let* ((history (delib-flow--run-stage-history run))
         (shadow-entry (and (eq (plist-get entry :provider) 'cloud)
                            (eq (plist-get entry :stage-id) 'run-cloud-stage)
                            (delib-flow--make-cloud-shadow-entry entry)))
         (updated-history
          (delib-flow--append-history-entry
           (if shadow-entry
               (delib-flow--append-history-entry history shadow-entry)
             history)
           entry)))
    (plist-put run :stage-history updated-history)))

(defun delib-flow--audit-provider (entry)
  "Return audit provider label for stage ENTRY."
  (symbol-name
   (or (plist-get entry :provider)
       (if (delib-flow--run-cloud-stage-p (plist-get entry :stage-id))
           'cloud
         'local))))

(defun delib-flow--audit-model-name (entry)
  "Return audit model name from stage ENTRY."
  (or (plist-get (plist-get entry :raw-output) :selected-model)
      (let* ((package (plist-get entry :input-package))
             (routing (plist-get package :routing))
             (provider (or (plist-get entry :provider)
                           (if (delib-flow--run-cloud-stage-p
                                (plist-get entry :stage-id))
                               'cloud
                             'local))))
        (pcase provider
          ('cloud (plist-get routing :default-cloud-model))
          (_ (plist-get routing :default-local-model))))
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
    (plist-put
     (plist-put run-record :run-status (delib-flow--audit-run-status run))
     :ended-at (plist-get session :ended-at))))

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

(defun delib-flow--editable-block (run block-id)
  "Return editable block BLOCK-ID from RUN."
  (alist-get block-id
             (plist-get (delib-flow--run-ui run) :editable-blocks)))

(defun delib-flow--editable-block-kind-name (block)
  "Return the string kind name for BLOCK."
  (symbol-name (plist-get block :kind)))

(defun delib-flow--editable-block-text (block)
  "Return the preferred text payload for editable BLOCK."
  (or (plist-get block :current-text)
      (plist-get block :accepted-text)
      (plist-get block :original-text)
      ""))

(defun delib-flow--editable-block-status (block text)
  "Return lifecycle status for BLOCK given TEXT."
  (if (equal text (plist-get block :original-text))
      'clean
    'edited))

(defun delib-flow--set-editable-block-text (block text)
  "Return BLOCK updated with TEXT and derived status."
  (plist-put
   (plist-put block :current-text text)
   :status
   (delib-flow--editable-block-status block text)))

(defun delib-flow--seed-editable-block-text (block text)
  "Return BLOCK freshly seeded with TEXT as a clean baseline."
  (let ((seed (or text "")))
    (setq block (plist-put block :original-text seed))
    (setq block (plist-put block :accepted-text seed))
    (setq block (plist-put block :current-text seed))
    (setq block (plist-put block :status 'clean))
    (setq block (plist-put block :validation-status 'valid))
    (plist-put block :validation-message nil)))

(defun delib-flow--replace-editable-block (blocks block-id new-block)
  "Return BLOCKS with BLOCK-ID replaced by NEW-BLOCK."
  (mapcar (lambda (entry)
            (if (eq (car entry) block-id)
                (cons block-id new-block)
              entry))
          blocks))

(defun delib-flow--set-editable-block (run block-id new-block)
  "Return RUN with BLOCK-ID replaced by NEW-BLOCK."
  (let* ((ui (delib-flow--run-ui run))
         (blocks (plist-get ui :editable-blocks))
         (updated-blocks
          (delib-flow--replace-editable-block blocks block-id new-block))
         (updated-ui (plist-put ui :editable-blocks updated-blocks)))
    (plist-put run :ui updated-ui)))

(defun delib-flow--editable-block-marker (block)
  "Return the begin marker string for BLOCK."
  (format "#+begin_delib-edit %s"
          (delib-flow--editable-block-kind-name block)))

(defun delib-flow--editable-block-text-in-buffer (buffer block)
  "Return editable block text from BUFFER for BLOCK."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward (delib-flow--editable-block-marker block) nil t)
        (forward-line 1)
        (let ((begin (point)))
          (when (search-forward "#+end_delib-edit" nil t)
            (string-remove-suffix
             "\n"
             (buffer-substring-no-properties
              begin
              (match-beginning 0)))))))))

(defun delib-flow--sync-editable-block (run buffer block-id)
  "Return RUN after syncing BLOCK-ID text from BUFFER."
  (let* ((block (delib-flow--editable-block run block-id))
         (text (delib-flow--editable-block-text-in-buffer buffer block)))
    (if text
        (delib-flow--set-editable-block
         run block-id (delib-flow--set-editable-block-text block text))
      run)))

(defun delib-flow--sync-editable-blocks (run buffer)
  "Return RUN after syncing editable block contents from BUFFER."
  (let ((updated-run run))
    (dolist (block-id (plist-get (delib-flow--run-working-context run)
                                 :editable-block-ids)
                      updated-run)
      (setq updated-run
            (delib-flow--sync-editable-block updated-run buffer block-id)))))

(defun delib-flow--render-editable-block (run block-id)
  "Return Org text for editable block BLOCK-ID from RUN."
  (let* ((block (delib-flow--editable-block run block-id))
         (kind (plist-get block :kind))
         (text (delib-flow--editable-block-text block)))
    (format "#+begin_delib-edit %s\n%s\n#+end_delib-edit\n"
            kind text)))

(defun delib-flow--section-heading (section)
  "Return the top-level heading text for SECTION."
  (format "** %s" section))

(defun delib-flow--next-section-heading-regexp ()
  "Return the regexp matching the next top-level control section."
  "^\\*\\* [^\n]+$")

(defun delib-flow--section-content-bounds (section)
  "Return the content bounds for SECTION in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (delib-flow--section-heading section) nil t)
      (forward-line 1)
      (let ((start (point)))
        (if (re-search-forward (delib-flow--next-section-heading-regexp) nil t)
            (cons start (match-beginning 0))
          (cons start (point-max)))))))

(defun delib-flow--replace-section-content (section content)
  "Replace SECTION contents in the current buffer with CONTENT."
  (let ((bounds (delib-flow--section-content-bounds section)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (goto-char (car bounds))
      (insert content)
      (unless (bolp)
        (insert "\n")))))

(defun delib-flow--editable-block-body-bounds ()
  "Return bounds for editable block bodies in the current buffer."
  (let (bounds)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_delib-edit [^\n]+$" nil t)
        (forward-line 1)
        (let ((start (point)))
          (when (re-search-forward "^#\\+end_delib-edit$" nil t)
            (push (cons (max (point-min) (1- start))
                        (match-beginning 0))
                  bounds)))))
    (nreverse bounds)))

(defun delib-flow--protect-managed-regions ()
  "Protect managed buffer regions while leaving editable block bodies writable."
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (point-max)
                         '(read-only t delib-flow-managed t))
    (dolist (bounds (delib-flow--editable-block-body-bounds))
      (add-text-properties
       (car bounds) (cdr bounds)
       '(read-only nil
         delib-flow-managed nil
         delib-flow-editable t
         front-sticky nil
         rear-nonsticky (read-only))))))

(defun delib-flow--section-present-p (section)
  "Return non-nil when SECTION heading is present in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward (delib-flow--section-heading section) nil t)))

(defun delib-flow--editable-block-present-p (run block-id)
  "Return non-nil when BLOCK-ID markers from RUN exist in the current buffer."
  (let ((marker
         (delib-flow--editable-block-marker
          (delib-flow--editable-block run block-id))))
    (save-excursion
      (goto-char (point-min))
      (and (search-forward marker nil t)
           (search-forward "#+end_delib-edit" nil t)))))

(defun delib-flow--managed-region-conflicts (run buffer)
  "Return a list of managed-region conflicts for RUN in BUFFER."
  (with-current-buffer buffer
    (let (conflicts)
      (dolist (section delib-flow--control-sections)
        (unless (delib-flow--section-present-p section)
          (push (format "Missing section: %s" section) conflicts)))
      (dolist (block-id (plist-get (delib-flow--run-working-context run)
                                   :editable-block-ids))
        (unless (delib-flow--editable-block-present-p run block-id)
          (push (format "Missing editable block: %s" block-id) conflicts)))
      (nreverse conflicts))))

(defun delib-flow--set-managed-region-conflicts (run conflicts)
  "Return RUN with managed-region CONFLICTS stored in UI state."
  (plist-put run :ui
             (plist-put (delib-flow--run-ui run)
                        :managed-region-conflicts
                        conflicts)))

(defun delib-flow--run-active-p (run)
  "Return non-nil when RUN is currently active."
  (eq (plist-get (delib-flow--run-session run) :status) 'active))

(defun delib-flow--in-flight-request-id ()
  "Return a fresh request id for async stage execution."
  (format "delib-flow-request-%s" (float-time)))

(defun delib-flow--in-flight-decision-text (stage-id provider model)
  "Return operator-facing current-decision text for in-flight STAGE-ID."
  (format "Waiting on %s %s via %s%s."
          (if (eq provider 'cloud) "cloud" "local LLM")
          (delib-flow--stage-label stage-id)
          provider
          (if (and model (not (string-empty-p model)))
              (format " (%s)" model)
            "")))

(defun delib-flow--mark-stage-in-flight (run stage-id provider model handle request-id started-at)
  "Return RUN marked in-flight for STAGE-ID with PROVIDER and MODEL."
  (let ((session (delib-flow--run-session run)))
    (plist-put
     run :session
     (plist-put
      (plist-put
       (plist-put
        (plist-put
         (plist-put
          (plist-put
           (plist-put
            (plist-put session :current-stage stage-id)
            :current-decision
            (delib-flow--in-flight-decision-text stage-id provider model))
           :in-flight-stage-id stage-id)
          :in-flight-provider provider)
         :in-flight-model model)
        :in-flight-started-at started-at)
       :in-flight-request-id request-id)
      :in-flight-handle handle))))

(defun delib-flow--clear-stage-in-flight (run)
  "Return RUN with any in-flight stage markers cleared."
  (let ((session (delib-flow--run-session run)))
    (plist-put
     run :session
     (plist-put
      (plist-put
       (plist-put
        (plist-put
         (plist-put
          (plist-put session :in-flight-stage-id nil)
          :in-flight-provider nil)
         :in-flight-model nil)
        :in-flight-started-at nil)
       :in-flight-request-id nil)
      :in-flight-handle nil))))

(defun delib-flow--normalize-in-flight-state (run)
  "Return RUN with dead-process in-flight markers cleared."
  (let* ((session (and run (delib-flow--run-session run)))
         (stage-id (plist-get session :in-flight-stage-id))
         (request-id (plist-get session :in-flight-request-id))
         (handle (plist-get session :in-flight-handle)))
    (if (and stage-id
             request-id
             (processp handle)
             (not (process-live-p handle)))
        (delib-flow--clear-stage-in-flight run)
      run)))

(defun delib-flow--cancel-in-flight-stage (run)
  "Cancel any live in-flight stage process for RUN."
  (when run
    (let ((handle (plist-get (delib-flow--run-session run) :in-flight-handle)))
      (when (processp handle)
        (ignore-errors
          (delete-process handle))))))

(defun delib-flow--in-flight-request-current-p (request-id)
  "Return non-nil when REQUEST-ID still belongs to `delib-flow--active-run'."
  (and delib-flow--active-run
       (string= request-id
                (or (plist-get (delib-flow--run-session delib-flow--active-run)
                               :in-flight-request-id)
                    ""))))

(defun delib-flow--refresh-in-flight-ui ()
  "Refresh any visible in-flight cockpit indicators."
  (if (and delib-flow--active-run
           (delib-flow--run-in-flight-p delib-flow--active-run))
      (when-let ((buffer (delib-flow--control-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t)
                  (anchor (or delib-flow--sticky-anchor-heading
                              (delib-flow--preferred-anchor-section
                               delib-flow--active-run))))
              (dolist (section '("Now" "Current result"))
                (delib-flow--replace-section-content
                 section
                 (delib-flow--section-content section delib-flow--active-run)))
              (delib-flow--protect-managed-regions)
              (unless (delib-flow--goto-section anchor)
                (goto-char (point-min)))
              (delib-flow--align-heading-top))
            (force-mode-line-update t))))
    (when (timerp delib-flow--in-flight-ui-timer)
      (cancel-timer delib-flow--in-flight-ui-timer)
      (setq delib-flow--in-flight-ui-timer nil))))

(defun delib-flow--ensure-in-flight-ui-timer ()
  "Ensure the cockpit has a live timer for async in-flight indicators."
  (unless (timerp delib-flow--in-flight-ui-timer)
    (setq delib-flow--in-flight-ui-timer
          (run-at-time 0 1 #'delib-flow--refresh-in-flight-ui))))

(defun delib-flow--control-buffer ()
  "Return the control buffer when it exists."
  (get-buffer delib-flow-control-buffer-name))

(defun delib-flow--action-placeholder-p (action)
  "Return non-nil when ACTION is a placeholder."
  (eq (plist-get action :status) 'placeholder))

(defun delib-flow--inspect-source-action (run)
  "Return the inspect-source action for RUN."
  (if (delib-flow--stage-executed-p run 'inspect-source)
      (delib-flow--make-action
       'inspect-source
       "Retry Inspect Source"
       'available
       nil
       #'delib-flow-action-inspect-source
       10)
    (delib-flow--make-action
     'inspect-source
     "Inspect Source"
     'available
     nil
     #'delib-flow-action-inspect-source
     10)))

(defun delib-flow--accept-inspect-source-action (run)
  "Return the accept-inspect-source action for RUN."
  (when (eq (delib-flow--stage-review-state run 'inspect-source) 'pending-review)
    (delib-flow--make-action
     'accept-inspect-source
     "Accept Inspect Result"
     'available
     nil
     #'delib-flow-action-accept-inspect-source
     15)))

(defun delib-flow--reject-inspect-source-action (run)
  "Return the reject-inspect-source action for RUN."
  (when (eq (delib-flow--stage-review-state run 'inspect-source) 'pending-review)
    (delib-flow--make-action
     'reject-inspect-source
     "Reject Inspect Result"
     'available
     nil
     #'delib-flow-action-reject-inspect-source
     16)))

(defun delib-flow--match-project-action (run)
  "Return the match-project action for RUN."
  (when (or (delib-flow--stage-accepted-p run 'inspect-source)
            (delib-flow--stage-executed-p run 'match-project))
    (delib-flow--make-action
     'match-project
     (if (delib-flow--stage-executed-p run 'match-project)
         "Retry Match Project"
       "Match Project")
     'available
     nil
     #'delib-flow-action-match-project
     20)))

(defun delib-flow--accept-match-project-action (run)
  "Return the accept-match-project action for RUN."
  (when (eq (delib-flow--stage-review-state run 'match-project) 'pending-review)
    (delib-flow--make-action
     'accept-match-project
     "Accept Project Match"
     'available
     nil
     #'delib-flow-action-accept-match-project
     21)))

(defun delib-flow--reject-match-project-action (run)
  "Return the reject-match-project action for RUN."
  (when (eq (delib-flow--stage-review-state run 'match-project) 'pending-review)
    (delib-flow--make-action
     'reject-match-project
     "Reject Project Match"
     'available
     nil
     #'delib-flow-action-reject-match-project
     22)))

(defun delib-flow--discover-reference-material-action (run)
  "Return the discover-reference-material action for RUN."
  (when (or (delib-flow--project-decision-ready-p run)
            (eq (delib-flow--stage-review-state run 'inspect-source) 'accepted))
    (delib-flow--make-action
     'discover-reference-material
     (if (delib-flow--stage-executed-p run 'discover-reference-material)
         "Retry Discover Relevant Reference Material"
       "Discover Relevant Reference Material")
     'available
     nil
     #'delib-flow-action-discover-reference-material
     30)))

(defun delib-flow--filter-reference-material-action (run)
  "Return the filter-reference-material action for RUN."
  (when (and (delib-flow--stage-executed-p run 'discover-reference-material)
             (plist-get (delib-flow--run-working-context run)
                        :retrieved-candidates))
    (delib-flow--make-action
     'filter-reference-material
     (if (delib-flow--stage-executed-p run 'filter-reference-material)
         "Retry Filter Useful Reference Material"
       "Filter Useful Reference Material")
     'available
     nil
     #'delib-flow-action-filter-reference-material
     35)))

(defun delib-flow--manual-project-match-action (run)
  "Return the manual-project-match action for RUN."
  (when (or (and (memq (delib-flow--stage-review-state run 'match-project)
                       '(accepted rejected))
                 (memq (delib-flow--match-status run) '(matched ambiguous no-match)))
            (delib-flow--stage-executed-p run 'manual-project-match))
    (delib-flow--make-action
     'manual-project-match
     (if (delib-flow--stage-executed-p run 'manual-project-match)
         "Retry Choose Project Manually"
       "Choose Project Manually")
     'available
     nil
     #'delib-flow-action-manual-project-match
     40)))

(defun delib-flow--extract-actions-action (run)
  "Return the extract-actions action for RUN."
  (when (and (delib-flow--project-decision-ready-p run)
             (eq (delib-flow--match-status run) 'matched))
    (delib-flow--make-action
     'extract-actions
     (if (delib-flow--stage-executed-p run 'extract-actions)
         "Retry Extract Actions"
       "Extract Actions")
     'available
     nil
     #'delib-flow-action-extract-actions
     40)))

(defun delib-flow--extract-waiting-for-action (run)
  "Return the extract-waiting-for action for RUN."
  (when (and (delib-flow--project-decision-ready-p run)
             (eq (delib-flow--match-status run) 'matched))
    (delib-flow--make-action
     'extract-waiting-for
     (if (delib-flow--stage-executed-p run 'extract-waiting-for)
         "Retry Extract Waiting-For"
       "Extract Waiting-For")
     'available
     nil
     #'delib-flow-action-extract-waiting-for
     50)))

(defun delib-flow--suggest-reference-notes-action (run)
  "Return the suggest-reference-notes action for RUN."
  (when (delib-flow--reference-note-suggestion-ready-p run)
    (delib-flow--make-action
     'suggest-reference-notes
     (if (delib-flow--stage-executed-p run 'suggest-reference-notes)
         "Retry Suggest Reference Notes"
       "Suggest Reference Notes")
     'available
     nil
     #'delib-flow-action-suggest-reference-notes
     60)))

(defun delib-flow--draft-selected-reference-note-action (run)
  "Return the draft-selected-reference-note action for RUN."
  (when-let ((item (delib-flow--selected-reference-note-candidate-for-drafting run)))
    (when (eq (plist-get item :kind) 'reference-note)
      (delib-flow--make-action
       'draft-selected-reference-note
       (if (delib-flow--artifact-family-selected-draft run 'reference-notes)
           "Regenerate Selected Note"
         "Draft Selected Note")
       'available
       nil
       #'delib-flow-action-draft-selected-reference-note
       61))))

(defun delib-flow--propose-new-project-action (run)
  "Return the propose-new-project action for RUN."
  (when (delib-flow--project-proposal-ready-p run)
    (delib-flow--make-action
     'propose-new-project
     (if (delib-flow--stage-executed-p run 'propose-new-project)
         "Retry Propose New Project"
       "Propose New Project")
     'available
     nil
     #'delib-flow-action-propose-new-project
     50)))

(defun delib-flow--decide-cloud-pass-action (run)
  "Return the decide-cloud-pass action for RUN."
  (when (delib-flow--stage-executed-p run 'inspect-source)
    (delib-flow--make-action
     'decide-cloud-pass
     (if (delib-flow--stage-executed-p run 'decide-cloud-pass)
         "Retry Decide on Cloud Pass"
       "Decide on Cloud Pass")
     'available
     nil
     #'delib-flow-action-decide-cloud-pass
     80)))

(defun delib-flow--sanitize-for-cloud-action (run)
  "Return the sanitize-for-cloud action for RUN."
  (when (plist-get (delib-flow--run-routing run) :cloud-switch-pending)
    (delib-flow--make-action
     'sanitize-for-cloud
     (if (delib-flow--stage-executed-p run 'sanitize-for-cloud)
         "Retry Sanitize for Cloud"
       "Sanitize for Cloud")
     'available
     nil
     #'delib-flow-action-sanitize-for-cloud
     85)))

(defun delib-flow--approve-cloud-send-action (run)
  "Return the approve-cloud-send action for RUN."
  (when (or (eq (plist-get (delib-flow--run-routing run) :sanitization-status)
                'prepared)
            (delib-flow--stage-executed-p run 'approve-cloud-send))
    (delib-flow--make-action
     'approve-cloud-send
     (if (delib-flow--stage-executed-p run 'approve-cloud-send)
         "Retry Approve Cloud Send"
       "Approve Cloud Send")
     'available
     nil
     #'delib-flow-action-approve-cloud-send
     86)))

(defun delib-flow--run-cloud-stage-action-label (run)
  "Return action label for running the current cloud stage in RUN."
  (let* ((target-stage (delib-flow--cloud-target-stage
                        (delib-flow--run-routing run)))
         (base-label (if (delib-flow--stage-executed-p run 'run-cloud-stage)
                         "Retry Run Cloud Stage"
                       "Run Cloud Stage")))
    (if (eq target-stage 'run-cloud-stage)
        base-label
      (format "%s (%s)" base-label
              (delib-flow--stage-label target-stage)))))

(defun delib-flow--direct-cloud-retry-ready-p (run)
  "Return non-nil when RUN may retry the current rerouted cloud target directly."
  (let* ((routing (delib-flow--run-routing run))
         (status (plist-get routing :sanitization-status)))
    (and (not (eq (delib-flow--cloud-target-stage routing) 'run-cloud-stage))
         (memq status '(approved returned))
         (delib-flow--latest-cloud-shadow-entry run))))

(defun delib-flow--retry-rerouted-cloud-stage-action (run)
  "Return the direct rerouted-cloud retry action for RUN."
  (when (delib-flow--direct-cloud-retry-ready-p run)
    (delib-flow--make-action
     'retry-rerouted-cloud-stage
     (format "Retry %s In Cloud"
             (delib-flow--stage-label
              (delib-flow--cloud-target-stage
               (delib-flow--run-routing run))))
     'available
     nil
     #'delib-flow-action-retry-rerouted-cloud-stage
     87)))

(defun delib-flow--restart-cloud-path-ready-p (run)
  "Return non-nil when RUN may restart the current cloud path."
  (let ((routing (delib-flow--run-routing run)))
    (and (plist-get routing :cloud-target-stage)
         (or (delib-flow--cloud-failure-active-p run)
             (delib-flow--latest-cloud-shadow-entry run)
             (eq (plist-get routing :sanitization-status) 'approved)
             (delib-flow--restart-cloud-path-working-state-p run)))))

(defun delib-flow--restart-cloud-path-working-state-p (run)
  "Return non-nil when RUN has cloud working state worth restarting."
  (let ((working (delib-flow--run-working-context run)))
    (or (plist-get working :cloud-sanitized-context)
        (plist-get working :cloud-returned-context))))

(defun delib-flow--restart-cloud-path-action (run)
  "Return the restart-cloud-path action for RUN."
  (when (delib-flow--restart-cloud-path-ready-p run)
    (delib-flow--make-action
     'restart-cloud-path
     "Restart Cloud Path"
     'available
     nil
     #'delib-flow-action-restart-cloud-path
     84)))

(defun delib-flow--run-cloud-stage-action (run)
  "Return the run-cloud-stage action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (or (eq (plist-get (delib-flow--run-routing run) :sanitization-status)
                     'approved)
                 (delib-flow--stage-executed-p run 'run-cloud-stage)))
    (delib-flow--make-action
     'run-cloud-stage
     (delib-flow--run-cloud-stage-action-label run)
     'available
     nil
     #'delib-flow-action-run-cloud-stage
     87)))

(defun delib-flow--resolve-cloud-failure-action (run)
  "Return the resolve-cloud-failure action for RUN."
  (when (delib-flow--cloud-failure-active-p run)
    (let ((stage-id (delib-flow--cloud-failure-stage
                     (delib-flow--run-routing run))))
    (delib-flow--make-action
     'resolve-cloud-failure
     (if (delib-flow--rerouted-cloud-stage-p stage-id)
         (format "Resolve Cloud Failure (%s)"
                 (delib-flow--stage-label stage-id))
       "Resolve Cloud Failure")
     'available
     nil
     #'delib-flow-action-resolve-cloud-failure
     87))))

(defun delib-flow--approve-candidate-reintegration-action (run)
  "Return the approve-candidate-reintegration action for RUN."
  (when (and (not (delib-flow--cloud-failure-active-p run))
             (or (eq (plist-get (delib-flow--run-routing run) :reintegration-status)
                     'pending-review)
                 (delib-flow--stage-executed-p run 'approve-candidate-reintegration)))
    (delib-flow--make-action
     'approve-candidate-reintegration
     (if (delib-flow--stage-executed-p run 'approve-candidate-reintegration)
         "Retry Approve Candidate Reintegration"
       "Approve Candidate Reintegration")
     'available
     nil
     #'delib-flow-action-approve-candidate-reintegration
     87)))

(defun delib-flow--integrate-into-source-action (run)
  "Return the integrate-into-source action for RUN."
  (when (delib-flow--integration-ready-p run)
    (delib-flow--make-action
     'integrate-into-source
     (if (delib-flow--stage-executed-p run 'integrate-into-source)
         "Retry Integrate into Source"
       "Integrate into Source")
     'available
     nil
     #'delib-flow-action-integrate-into-source
     88)))

(defun delib-flow--select-approved-filing-actions-action (run)
  "Return the select-approved-filing-actions action for RUN."
  (when (delib-flow--draft-items-ready-p run)
    (delib-flow--make-action
     'select-approved-filing-actions
     (if (delib-flow--stage-executed-p run 'select-approved-filing-actions)
         "Approve Another Filing Artifact"
       "Select Approved Filing Actions")
     'available
     nil
     #'delib-flow-action-select-approved-filing-actions
     88)))

(defun delib-flow--reject-draft-filing-artifact-action (run)
  "Return the reject-draft-filing-artifact action for RUN."
  (when (delib-flow--draft-items-ready-p run)
    (delib-flow--make-action
     'reject-draft-filing-artifact
     (if (delib-flow--stage-executed-p run 'reject-draft-filing-artifact)
         "Reject Another Filing Artifact"
       "Reject Draft Filing Artifact")
     'available
     nil
     #'delib-flow-action-reject-draft-filing-artifact
     88)))

(defun delib-flow--file-approved-outputs-action (run)
  "Return the file-approved-outputs action for RUN."
  (when (delib-flow--approved-items-ready-p run)
    (delib-flow--make-action
     'file-approved-outputs
     (if (plist-get (plist-get run :filing) :conflicts)
         "Retry File Approved Outputs"
       "File Approved Outputs")
     'available
     nil
     #'delib-flow-action-file-approved-outputs
     89)))

(defun delib-flow--resolve-filing-conflict-action (run)
  "Return the resolve-filing-conflict action for RUN."
  (when (plist-get (plist-get run :filing) :conflicts)
    (delib-flow--make-action
     'resolve-filing-conflict
     "Resolve Filing Conflict"
     'available
     nil
     #'delib-flow-action-resolve-filing-conflict
     89)))

(defun delib-flow--placeholder-stage-action (id label priority)
  "Return a placeholder stage action for ID with LABEL and PRIORITY."
  (delib-flow--make-action
   id
   label
   'placeholder
   "This stage is not implemented yet."
   #'delib-flow-action-stage-placeholder
   priority))

(defun delib-flow--post-inspect-actions (run)
  "Return next legal actions after inspect-source has executed in RUN."
  (seq-remove
   #'null
     (list
     (delib-flow--accept-inspect-source-action run)
     (delib-flow--reject-inspect-source-action run)
     (delib-flow--match-project-action run)
     (delib-flow--discover-reference-material-action run)
     (delib-flow--filter-reference-material-action run)
     (delib-flow--decide-cloud-pass-action run)
     (delib-flow--restart-cloud-path-action run)
     (delib-flow--sanitize-for-cloud-action run)
     (delib-flow--approve-cloud-send-action run)
     (delib-flow--retry-rerouted-cloud-stage-action run)
     (delib-flow--run-cloud-stage-action run)
     (delib-flow--suggest-reference-notes-action run)
     (delib-flow--draft-selected-reference-note-action run)
     (delib-flow--resolve-cloud-failure-action run)
     (delib-flow--approve-candidate-reintegration-action run)
     (delib-flow--integrate-into-source-action run)
     (delib-flow--reject-draft-filing-artifact-action run)
     (delib-flow--select-approved-filing-actions-action run)
     (delib-flow--resolve-filing-conflict-action run)
     (delib-flow--file-approved-outputs-action run))))

(defun delib-flow--match-status (run)
  "Return the stored project match status from RUN."
  (plist-get (delib-flow--current-project-decision run) :match-status))

(defun delib-flow--post-match-actions (run)
  "Return next legal actions after match-project has executed in RUN."
  (let ((status (delib-flow--match-status run)))
    (seq-remove
     #'null
     (append
     (list
       (delib-flow--match-project-action run)
       (delib-flow--accept-match-project-action run)
       (delib-flow--reject-match-project-action run)
       (delib-flow--manual-project-match-action run))
     (list
      (delib-flow--suggest-reference-notes-action run)
      (delib-flow--draft-selected-reference-note-action run)
      (delib-flow--propose-new-project-action run))
     (if (delib-flow--project-decision-ready-p run)
         (append
          (list (delib-flow--discover-reference-material-action run))
          (if (eq status 'matched)
              (list
               (delib-flow--extract-actions-action run)
               (delib-flow--extract-waiting-for-action run))
            nil))
       (list))
       (list
       (delib-flow--filter-reference-material-action run)
       (delib-flow--decide-cloud-pass-action run)
       (delib-flow--restart-cloud-path-action run)
       (delib-flow--sanitize-for-cloud-action run)
       (delib-flow--approve-cloud-send-action run)
       (delib-flow--retry-rerouted-cloud-stage-action run)
       (delib-flow--run-cloud-stage-action run)
       (delib-flow--resolve-cloud-failure-action run)
       (delib-flow--approve-candidate-reintegration-action run)
       (delib-flow--integrate-into-source-action run)
       (delib-flow--reject-draft-filing-artifact-action run)
       (delib-flow--select-approved-filing-actions-action run)
       (delib-flow--resolve-filing-conflict-action run)
       (delib-flow--file-approved-outputs-action run))))))

(defun delib-flow--restart-cloud-path-run (run)
  "Return RUN reset to restart the reviewed cloud path from sanitization."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (routing (delib-flow--clear-cloud-failure-state
                   (delib-flow--run-routing run)))
         (cleared-routing
          (plist-put
           (plist-put
            (plist-put routing :cloud-switch-pending t)
            :sanitization-status 'required)
           :reintegration-status nil))
         (updated-run
          (plist-put
           (plist-put run :working-context
                      (plist-put
                       (plist-put working :cloud-sanitized-context nil)
                       :cloud-returned-context nil))
           :routing cleared-routing)))
    (plist-put
     updated-run :session
     (plist-put (delib-flow--run-session updated-run)
                :current-decision
                "Cloud path restarted. Review sanitization again before sending another cloud attempt."))))

(defun delib-flow--base-actions (run)
  "Return the base action list for RUN."
  (append
   (list (delib-flow--inspect-source-action run))
   (when (delib-flow--stage-executed-p run 'inspect-source)
     (if (delib-flow--stage-executed-p run 'match-project)
         (delib-flow--post-match-actions run)
       (seq-remove #'null (delib-flow--post-inspect-actions run))))
   (list
    (delib-flow--make-action
     'refresh-buffer
     "Refresh Buffer"
     'available
     nil
     #'delib-flow-refresh-buffer
     90)
    (delib-flow--make-action
     'abort-run
     "Abort Run"
     'available
     nil
     #'delib-flow-abort-run
     100))))

(defun delib-flow--compute-actions (run)
  "Return the current action list."
  (delib-flow--base-actions run))

(defun delib-flow--assign-action-shortcuts (actions)
  "Return ACTIONS with stable rendered shortcut keys assigned."
  (let ((keys delib-flow--action-shortcut-keys))
    (mapcar
     (lambda (action)
       (prog1 (plist-put action :shortcut (car keys))
         (setq keys (cdr keys))))
     (sort (copy-tree actions)
           (lambda (left right)
             (< (plist-get left :priority)
                (plist-get right :priority)))))))

(defun delib-flow--apply-action-conflict-state (action conflicts)
  "Return ACTION adjusted for managed-region CONFLICTS."
  (if (or (null conflicts)
          (memq (plist-get action :id) '(refresh-buffer abort-run)))
      action
    (plist-put
     (plist-put action :status 'blocked)
     :reason
     "Managed-region conflicts must be resolved before this action can run.")))

(defun delib-flow--apply-action-in-flight-state (run action)
  "Return ACTION adjusted for any in-flight stage in RUN."
  (if (or (not (delib-flow--run-in-flight-p run))
          (memq (plist-get action :id) '(refresh-buffer abort-run)))
      action
    (plist-put
     (plist-put action :status 'blocked)
     :reason
     (format "Wait for %s to finish before running another workflow action."
             (delib-flow--stage-label
              (delib-flow--run-in-flight-stage-id run))))))

(defun delib-flow--seed-actions (run)
  "Return RUN with computed actions populated."
  (let* ((run (delib-flow--normalize-in-flight-state run))
         (conflicts (plist-get (delib-flow--run-ui run)
                               :managed-region-conflicts))
         (items (delib-flow--assign-action-shortcuts
                 (mapcar (lambda (action)
                           (delib-flow--apply-action-in-flight-state
                            run
                            (delib-flow--apply-action-conflict-state
                             action conflicts)))
                         (delib-flow--compute-actions run)))))
    (plist-put run :actions
               (plist-put (delib-flow--run-actions run)
                          :items
                          items))))

(defun delib-flow--active-run-conflict-p ()
  "Return non-nil when an active run already exists."
  (and delib-flow--active-run
       (delib-flow--run-active-p delib-flow--active-run)
       (buffer-live-p (delib-flow--control-buffer))))

(defun delib-flow--cleanup-stale-run ()
  "Clear stale active-run state when the control buffer is gone."
  (when (and delib-flow--active-run
             (not (buffer-live-p (delib-flow--control-buffer))))
    (delib-flow--teardown-active-run)))

(defun delib-flow--debug-scenario-ids ()
  "Return available debug scenario ids."
  (mapcar #'car delib-flow-debug-scenarios))

(defun delib-flow--debug-scenario (scenario-id)
  "Return debug scenario plist for SCENARIO-ID."
  (cdr (assq scenario-id delib-flow-debug-scenarios)))

(defun delib-flow--debug-supported-checkpoints (scenario-id)
  "Return supported debug checkpoints for SCENARIO-ID."
  (or (plist-get (delib-flow--debug-scenario scenario-id)
                 :supported-checkpoints)
      (delib-flow--debug-checkpoint-ids)))

(defun delib-flow--debug-checkpoint-supported-p (scenario-id checkpoint)
  "Return non-nil when SCENARIO-ID supports CHECKPOINT."
  (memq checkpoint (delib-flow--debug-supported-checkpoints scenario-id)))

(defun delib-flow--debug-checkpoint-ids ()
  "Return available debug checkpoint ids."
  (mapcar #'car delib-flow-debug-checkpoints))

(defun delib-flow--debug-checkpoint-label (checkpoint)
  "Return display label for CHECKPOINT."
  (alist-get checkpoint delib-flow-debug-checkpoints))

(defun delib-flow--debug-scenario-label (scenario-id)
  "Return display label for SCENARIO-ID."
  (plist-get (delib-flow--debug-scenario scenario-id) :label))

(defun delib-flow--debug-walkthrough-target-ids ()
  "Return available walkthrough target ids."
  (mapcar #'car delib-flow-debug-walkthrough-targets))

(defun delib-flow--debug-walkthrough-target (target-id)
  "Return walkthrough target plist for TARGET-ID."
  (cdr (assq target-id delib-flow-debug-walkthrough-targets)))

(defun delib-flow--debug-walkthrough-target-label (target-id)
  "Return display label for walkthrough TARGET-ID."
  (plist-get (delib-flow--debug-walkthrough-target target-id) :label))

(defun delib-flow--debug-walkthrough-target-steps (target-id)
  "Return ordered checkpoint steps for walkthrough TARGET-ID."
  (plist-get (delib-flow--debug-walkthrough-target target-id) :steps))

(defun delib-flow--debug-walkthrough-target-checks (target-id)
  "Return checkpoint verification checks for walkthrough TARGET-ID."
  (plist-get (delib-flow--debug-walkthrough-target target-id) :checks))

(defun delib-flow--debug-walkthrough-first-step (target-id)
  "Return first checkpoint step for walkthrough TARGET-ID."
  (car (delib-flow--debug-walkthrough-target-steps target-id)))

(defun delib-flow--debug-walkthrough-next-step-id (target-id checkpoint)
  "Return checkpoint after CHECKPOINT for walkthrough TARGET-ID."
  (cadr (member checkpoint
                (delib-flow--debug-walkthrough-target-steps target-id))))

(defun delib-flow--debug-active-walkthrough-target-id ()
  "Return active walkthrough target id from the current run."
  (and delib-flow--active-run
       (plist-get (delib-flow--run-session delib-flow--active-run)
                  :debug-walkthrough-target-id)))

(defun delib-flow--debug-active-walkthrough-checkpoint ()
  "Return active walkthrough checkpoint from the current run."
  (and delib-flow--active-run
       (plist-get (delib-flow--run-session delib-flow--active-run)
                  :debug-checkpoint)))

(defun delib-flow--ensure-active-debug-walkthrough ()
  "Return active walkthrough target id or signal a user error."
  (or (delib-flow--debug-active-walkthrough-target-id)
      (user-error "No active debug walkthrough target")))

(defun delib-flow--debug-set-block-text (run block-id text)
  "Return RUN with editable BLOCK-ID text replaced by TEXT."
  (let ((block (delib-flow--editable-block run block-id)))
    (delib-flow--set-editable-block
     run block-id
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow--debug-set-cloud-target-stage (run stage-id)
  "Return RUN with cloud-routing block updated to STAGE-ID."
  (delib-flow--debug-set-block-text
   run 'cloud-routing-review
   (format "Target stage: %s\nNotes:\nDebug checkpoint reroutes cloud execution here.\n"
           stage-id)))

(defun delib-flow--debug-set-filing-selection (run selection)
  "Return RUN with filing-selection block updated to SELECTION."
  (delib-flow--debug-set-block-text
   run 'filing-selection-review
   (format "Selection: %s\nNotes:\nDebug checkpoint preselected this filing artifact.\n"
           selection)))

(defun delib-flow--debug-source-file (scenario)
  "Create and return a temp source file for SCENARIO."
  (let ((file (make-temp-file "delib-flow-source" nil ".org")))
    (with-temp-file file
      (insert (plist-get scenario :source-content)))
    file))

(defun delib-flow--debug-projects-file (scenario)
  "Create and return a temp projects file for SCENARIO."
  (make-temp-file
   "delib-flow-projects"
   nil
   ".org"
   (or (plist-get scenario :projects-content) "")))

(defun delib-flow--debug-zk-root (scenario)
  "Create and return a temp ZK root for SCENARIO."
  (let ((root (make-temp-file "delib-flow-zk" t)))
    (dolist (entry (plist-get scenario :zk-files))
      (let* ((relative (car entry))
             (content (cdr entry))
             (target (expand-file-name relative root))
             (dir (file-name-directory target)))
        (make-directory dir t)
        (with-temp-file target
          (insert content))))
    root))

(defun delib-flow--debug-audit-file ()
  "Create and return a temp audit log file."
  (make-temp-file "delib-flow-audit" nil ".org"))

(defun delib-flow--activate-debug-fixture (scenario-id)
  "Create and activate a temp debug fixture for SCENARIO-ID."
  (let* ((scenario (delib-flow--debug-scenario scenario-id))
         (source-file (delib-flow--debug-source-file scenario))
         (projects-file (delib-flow--debug-projects-file scenario))
         (zk-root (delib-flow--debug-zk-root scenario))
         (audit-file (delib-flow--debug-audit-file))
         (fixture
          (list :scenario-id scenario-id
                :source-file source-file
                :projects-file projects-file
                :zk-root zk-root
                :audit-file audit-file
                :previous-projects-file delib-flow-my-projects-file
                :previous-zk-root delib-flow-zk-root
                :previous-audit-log-file delib-flow-audit-log-file)))
    (setq delib-flow-my-projects-file projects-file
          delib-flow-zk-root zk-root
          delib-flow-audit-log-file audit-file)
    fixture))

(defun delib-flow--cleanup-debug-file (path)
  "Delete debug file at PATH when it exists."
  (when (and path (file-exists-p path))
    (delete-file path)))

(defun delib-flow--cleanup-debug-directory (path)
  "Delete debug directory at PATH when it exists."
  (when (and path (file-directory-p path))
    (delete-directory path t)))

(defun delib-flow--cleanup-debug-fixture (fixture)
  "Restore configuration and delete temp resources from FIXTURE."
  (when fixture
    (setq delib-flow-my-projects-file (plist-get fixture :previous-projects-file)
          delib-flow-zk-root (plist-get fixture :previous-zk-root)
          delib-flow-audit-log-file (plist-get fixture :previous-audit-log-file))
    (delib-flow--cleanup-debug-file (plist-get fixture :source-file))
    (delib-flow--cleanup-debug-file (plist-get fixture :projects-file))
    (delib-flow--cleanup-debug-file (plist-get fixture :audit-file))
    (delib-flow--cleanup-debug-directory (plist-get fixture :zk-root))))

(defun delib-flow--set-debug-fixture (run fixture)
  "Return RUN with debug FIXTURE stored in session state."
  (plist-put run :session
             (plist-put (delib-flow--run-session run) :debug-fixture fixture)))

(defun delib-flow--debug-source-snapshot (scenario-id fixture)
  "Return source snapshot for SCENARIO-ID using FIXTURE."
  (let* ((scenario (delib-flow--debug-scenario scenario-id))
         (title (plist-get scenario :source-title)))
    (list :title title
          :content (plist-get scenario :source-content)
          :file (plist-get fixture :source-file)
          :outline-path (list title))))

(defun delib-flow--debug-accept-inspect (run)
  "Return RUN with the current inspect result accepted."
  (delib-flow--seed-actions
   (delib-flow--refresh-run-audit
    (delib-flow--apply-inspect-review-outcome
     run
     'accepted
     "Inspect result accepted for debug replay.")
    'inspect-source)))

(defun delib-flow--debug-accept-match (run)
  "Return RUN with the current project match accepted."
  (delib-flow--seed-actions
   (delib-flow--refresh-run-audit
    (delib-flow--apply-match-review-outcome
     run
     'accepted
     "Project match accepted for debug replay.")
    'match-project)))

(defun delib-flow--debug-run-inspect-reviewed (run)
  "Return RUN replayed through accepted inspect review."
  (delib-flow--debug-accept-inspect
   (delib-flow--seed-actions
    (delib-flow--run-stage-locally run 'inspect-source))))

(defun delib-flow--debug-run-project-reviewed (run)
  "Return RUN replayed through accepted project review."
  (delib-flow--seed-actions
   (delib-flow--debug-apply-project-override
    (delib-flow--debug-accept-match
     (delib-flow--seed-actions
      (delib-flow--run-stage-locally
       (delib-flow--debug-run-inspect-reviewed run)
       'match-project))))))

(defun delib-flow--debug-run-manual-project-ready (run)
  "Return RUN replayed to a manual-project selection checkpoint."
  (delib-flow--debug-run-project-reviewed run))

(defun delib-flow--debug-context-override (scenario-id)
  "Return optional seeded context override for SCENARIO-ID."
  (plist-get (delib-flow--debug-scenario scenario-id) :context-override))

(defun delib-flow--debug-filing-override (scenario-id)
  "Return optional seeded filing override for SCENARIO-ID."
  (plist-get (delib-flow--debug-scenario scenario-id) :filing-override))

(defun delib-flow--debug-project-override (scenario-id)
  "Return optional reviewed project override for SCENARIO-ID."
  (plist-get (delib-flow--debug-scenario scenario-id) :project-override))

(defun delib-flow--debug-artifact-stage (scenario-id)
  "Return the artifact-generation stage for SCENARIO-ID."
  (or (plist-get (delib-flow--debug-scenario scenario-id) :artifact-stage)
      'extract-actions))

(defun delib-flow--debug-override-candidate (fixture spec)
  "Return resolved debug override candidate from FIXTURE and SPEC."
  (list :title (plist-get spec :title)
        :file (when-let ((path (plist-get spec :path)))
                (expand-file-name path (plist-get fixture :zk-root)))
        :score (or (plist-get spec :score) 1)
        :signals (plist-get spec :signals)
        :reasons (or (plist-get spec :reasons) '("debug-midpoint-fixture"))
        :filter-status (or (plist-get spec :filter-status) 'retained)
        :filter-reasons (or (plist-get spec :filter-reasons)
                            '("retained-by-debug-midpoint-fixture"))))

(defun delib-flow--debug-apply-context-override (run)
  "Return RUN with any scenario-specific seeded context override applied."
  (let* ((session (delib-flow--run-session run))
         (fixture (plist-get session :debug-fixture))
         (scenario-id (plist-get fixture :scenario-id))
         (override (and scenario-id
                        (delib-flow--debug-context-override scenario-id))))
    (if (null override)
        run
      (let* ((retrieved (mapcar (lambda (spec)
                                  (delib-flow--debug-override-candidate fixture spec))
                                (plist-get override :retrieved-candidates)))
             (retained (seq-filter (lambda (candidate)
                                     (eq (plist-get candidate :filter-status) 'retained))
                                   retrieved))
             (rejected (seq-filter (lambda (candidate)
                                     (eq (plist-get candidate :filter-status) 'rejected))
                                   retrieved))
             (raw (list :candidate-count (length retrieved)
                        :retained-count (length retained)
                        :retained-candidates retained
                        :rejected-count (length rejected)
                        :rejected-candidates rejected
                        :retained-context
                        (or (plist-get override :retained-context)
                            (delib-flow--retained-context-lines retained))))
             (working (delib-flow--run-working-context run)))
        (plist-put
         run :working-context
         (plist-put
         (plist-put
         (plist-put working :retrieved-candidates retrieved)
           :filtered-context raw)
          :retained-context (plist-get raw :retained-context)))))))

(defun delib-flow--debug-apply-project-override (run)
  "Return RUN with any scenario-specific reviewed project override applied."
  (let* ((session (delib-flow--run-session run))
         (fixture (plist-get session :debug-fixture))
         (scenario-id (plist-get fixture :scenario-id))
         (override (and scenario-id
                        (delib-flow--debug-project-override scenario-id))))
    (if (null override)
        run
      (let* ((working (delib-flow--run-working-context run))
             (existing (plist-get working :project-match))
             (project-match
              (plist-put
               (plist-put
                (plist-put
                 (plist-put existing :match-status
                            (or (plist-get override :match-status)
                                (plist-get existing :match-status)))
                 :best-project (plist-get override :best-project))
                :candidates (plist-get override :candidates))
               :reason (or (plist-get override :reason)
                           (plist-get existing :reason))))
             (updated-run
              (delib-flow--set-latest-stage-entry
               run
               'match-project
               (lambda (entry)
                 (plist-put
                  (plist-put entry :raw-output project-match)
                  :normalized-output
                  (delib-flow--normalize-match-project-output project-match))))))
        (plist-put
         updated-run
         :working-context
         (plist-put working :project-match project-match))))))

(defun delib-flow--debug-override-draft-item (package spec)
  "Return debug draft item for PACKAGE built from SPEC."
  (let* ((kind (or (plist-get spec :kind) 'next-action))
         (source (or (plist-get spec :source) 'debug-midpoint-fixture))
         (item
          (pcase kind
            ('reference-note
             (delib-flow--make-draft-reference-note
              (or (plist-get spec :text) "")
              source
              (or (plist-get spec :note-type) 'general-pkm)))
            ('waiting-for
             (delib-flow--make-draft-waiting-for
              (or (plist-get spec :text) "")
              source))
            (_
             (delib-flow--make-draft-action
              (or (plist-get spec :text) "")
              source)))))
    (car
     (pcase kind
       ('reference-note
        (delib-flow--annotate-draft-reference-notes (list item) package))
       ('waiting-for
        (delib-flow--annotate-draft-waiting-fors (list item)))
       (_
        (delib-flow--annotate-draft-actions (list item)))))))

(defun delib-flow--debug-apply-filing-override (run)
  "Return RUN with any scenario-specific seeded filing override applied."
  (let* ((session (delib-flow--run-session run))
         (fixture (plist-get session :debug-fixture))
         (scenario-id (plist-get fixture :scenario-id))
         (override (and scenario-id
                        (delib-flow--debug-filing-override scenario-id))))
    (if (null override)
        run
      (let* ((package (delib-flow--stage-input-package run
                                                       'select-approved-filing-actions))
             (draft-items
              (mapcar (lambda (spec)
                        (delib-flow--debug-override-draft-item package spec))
                      (plist-get override :draft-items)))
             (filing
              (plist-put
               (delib-flow--clear-filing-selection-block-state
                (plist-put
                 (plist-put
                  (plist-put
                   (plist-put (plist-get run :filing) :draft-items draft-items)
                   :approved-items nil)
                  :rejected-items nil)
                 :conflicts nil))
               :preview-text (delib-flow--draft-item-preview-text draft-items))))
        (delib-flow--seed-filing-selection-block
         (plist-put run :filing filing))))))

(defun delib-flow--debug-run-context-ready (run)
  "Return RUN replayed through retrieval and filtering."
  (delib-flow--seed-actions
   (delib-flow--debug-apply-context-override
    (delib-flow--run-stage-locally
     (delib-flow--seed-actions
      (delib-flow--run-stage-locally
       (delib-flow--debug-run-project-reviewed run)
       'discover-reference-material))
     'filter-reference-material))))

(defun delib-flow--debug-run-artifact-ready (run)
  "Return RUN replayed through initial artifact drafting."
  (let* ((session (delib-flow--run-session run))
         (fixture (plist-get session :debug-fixture))
         (scenario-id (plist-get fixture :scenario-id))
         (stage-id (and scenario-id
                        (delib-flow--debug-artifact-stage scenario-id))))
    (delib-flow--seed-actions
     (delib-flow--debug-apply-filing-override
      (delib-flow--run-stage-locally
       (delib-flow--debug-run-context-ready run)
       (or stage-id 'extract-actions))))))

(defun delib-flow--debug-run-cloud-ready (run)
  "Return RUN replayed through reviewed cloud send."
  (let* ((project-reviewed (delib-flow--debug-run-project-reviewed run))
         (decided (delib-flow--seed-actions
                   (delib-flow--run-stage-locally
                    project-reviewed
                    'decide-cloud-pass)))
         (sanitized (delib-flow--seed-actions
                     (delib-flow--run-stage-locally
                      decided
                      'sanitize-for-cloud))))
    (delib-flow--seed-actions
     (delib-flow--run-stage-locally sanitized 'approve-cloud-send))))

(defun delib-flow--debug-run-cloud-failure-ready (run)
  "Return RUN replayed to a recorded cloud-failure checkpoint."
  (let* ((cloud-ready
          (delib-flow--debug-run-cloud-ready
           (delib-flow--debug-set-cloud-target-stage run 'extract-actions)))
         (delib-flow-cloud-stage-adapter
          (lambda (_descriptor _package)
            (error "debug cloud failure"))))
    (delib-flow--seed-actions
     (delib-flow--run-stage-in-cloud cloud-ready 'run-cloud-stage))))

(defun delib-flow--debug-run-filing-ready (run)
  "Return RUN replayed through integration with filing artifacts available."
  (delib-flow--seed-actions
   (delib-flow--run-stage-locally
    (delib-flow--debug-run-artifact-ready run)
    'integrate-into-source)))

(defun delib-flow--debug-run-filing-conflict-ready (run)
  "Return RUN replayed to a filing-conflict checkpoint."
  (let* ((filing-ready (delib-flow--debug-run-filing-ready run))
         (selected
          (delib-flow--seed-actions
           (delib-flow--run-stage-locally
            (delib-flow--debug-set-filing-selection filing-ready "1")
            'select-approved-filing-actions))))
    (delib-flow--seed-actions
     (delib-flow--run-stage-locally selected 'file-approved-outputs))))

(defconst delib-flow--debug-checkpoint-runner-alist
  '((source . identity)
    (inspect-reviewed . delib-flow--debug-run-inspect-reviewed)
    (project-reviewed . delib-flow--debug-run-project-reviewed)
    (manual-project-ready . delib-flow--debug-run-manual-project-ready)
    (context-ready . delib-flow--debug-run-context-ready)
    (artifact-ready . delib-flow--debug-run-artifact-ready)
    (cloud-ready . delib-flow--debug-run-cloud-ready)
    (cloud-failure-ready . delib-flow--debug-run-cloud-failure-ready)
    (filing-conflict-ready . delib-flow--debug-run-filing-conflict-ready)
    (filing-ready . delib-flow--debug-run-filing-ready))
  "Functions used to replay a run to each debug checkpoint.")

(defun delib-flow--debug-replay-to-checkpoint (run checkpoint)
  "Return RUN replayed to CHECKPOINT."
  (if-let ((runner (alist-get checkpoint delib-flow--debug-checkpoint-runner-alist)))
      (let ((delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter))
        (funcall runner run))
    (user-error "Unsupported debug checkpoint: %s" checkpoint)))

(defun delib-flow--format-action-line (action)
  "Return a display line for ACTION."
  (let* ((label (plist-get action :label))
         (status (plist-get action :status))
         (reason (plist-get action :reason))
         (shortcut (plist-get action :shortcut))
         (description
          (alist-get
           (plist-get action :id)
           '((inspect-source . "Review the source snapshot and propose structured context.")
             (accept-inspect-source . "Accept the current inspect result for downstream use.")
             (reject-inspect-source . "Reject the inspect result and require another inspect pass.")
             (match-project . "Compare the source against your projects file.")
             (accept-match-project . "Accept the current project decision.")
             (reject-match-project . "Reject the current project decision.")
             (manual-project-match . "Choose the project manually when the candidate result is insufficient.")
             (discover-reference-material . "Retrieve likely supporting context from your ZK.")
             (filter-reference-material . "Reduce retrieved material to the context worth keeping.")
             (propose-new-project . "Draft a new project from a reviewed no-match decision.")
             (extract-actions . "Draft next actions from the accepted project context.")
             (extract-waiting-for . "Draft waiting-for items from the accepted project context.")
             (suggest-reference-notes . "Draft reference-note candidates from the accepted context.")
             (decide-cloud-pass . "Decide whether to send context through a cloud stage.")
             (restart-cloud-path . "Reset the current cloud path back to sanitization and reviewed send.")
             (sanitize-for-cloud . "Prepare a sanitized cloud package for review.")
             (approve-cloud-send . "Approve the reviewed cloud package before sending it.")
             (retry-rerouted-cloud-stage . "Retry the current rerouted cloud target directly from the approved reviewed package.")
             (run-cloud-stage . "Execute the configured cloud stage.")
             (approve-candidate-reintegration . "Approve reintegration of returned cloud output.")
             (integrate-into-source . "Fold accepted context back into the source working state.")
             (reject-draft-filing-artifact . "Choose which single draft filing artifact to reject from this run.")
             (select-approved-filing-actions . "Choose which single filing artifact to approve next.")
             (file-approved-outputs . "Write the approved artifact to deterministic targets.")
             (refresh-buffer . "Rerender the cockpit from current run state.")
             (abort-run . "Abort the active run and finalize audit state."))))
         (line (format "- %s [%s]" label status)))
    (concat
     line
     "\n"
     (when shortcut
       (format "  Hotkey: `%s`.\n" shortcut))
     (when description
       (format "  %s\n" description))
     (when reason
       (format "  Reason: %s\n" reason)))))

(defun delib-flow--sorted-actions (run)
  "Return RUN actions sorted by priority."
  (sort (copy-sequence (plist-get (delib-flow--run-actions run) :items))
        (lambda (left right)
          (< (plist-get left :priority)
             (plist-get right :priority)))))

(defun delib-flow--render-source-section (run)
  "Return Org text for the source snapshot details from RUN."
  (let* ((source (delib-flow--run-source run))
         (title (or (plist-get source :title) "Untitled source"))
         (file (or (plist-get source :file) "No file"))
         (id (or (plist-get source :id) "No ID"))
         (source-type (delib-flow--display-source-type run))
         (content (string-trim (or (plist-get source :content) ""))))
    (format "*** Source snapshot\n- Title: %s\n- Source type: %s\n- File: %s\n- ID: %s\n- Outline path: %s\n\n#+begin_example\n%s\n#+end_example\n"
            title
            source-type
            file
            id
            (delib-flow--source-outline-path-text source)
            content)))

(defun delib-flow--project-match-status (project-match)
  "Return display status symbol for PROJECT-MATCH."
  (or (plist-get project-match :match-status)
      'not-run))

(defun delib-flow--review-record-status (record)
  "Return candidate review-state for accepted-result RECORD."
  (or (plist-get record :candidate-review-state)
      'not-available))

(defun delib-flow--review-record-accepted-p (record)
  "Return non-nil when accepted-result RECORD has accepted output."
  (plist-get record :accepted-output))

(defun delib-flow--review-record-status-line (label record)
  "Return status line for accepted-result LABEL and RECORD."
  (format "%s: candidate=%s. accepted=%s."
          label
          (delib-flow--review-record-status record)
          (if (delib-flow--review-record-accepted-p record) "available" "not available")))

(defun delib-flow--display-value (value)
  "Return VALUE as readable display text."
  (cond
   ((listp value)
    (delib-flow--display-value-list value))
   (t
    (delib-flow--display-value-scalar value))))

(defun delib-flow--display-value-list (value)
  "Return list VALUE as readable display text."
  (if value
      (mapconcat #'delib-flow--display-value value ", ")
    "nil"))

(defun delib-flow--display-value-scalar (value)
  "Return scalar VALUE as readable display text."
  (cond
   ((stringp value) value)
   (t (delib-flow--display-non-string-scalar value))))

(defun delib-flow--display-non-string-scalar (value)
  "Return non-string scalar VALUE as readable display text."
  (cond
   ((null value) "nil")
   ((symbolp value) (symbol-name value))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun delib-flow--bullet-lines (pairs)
  "Return Org bullet lines for PAIRS."
  (mapconcat
   (lambda (pair)
     (format "- %s: %s" (car pair) (delib-flow--display-value (cdr pair))))
   pairs
   "\n"))

(defun delib-flow--lines-subsection (heading lines)
  "Return subsection HEADING from line list LINES."
  (when lines
    (format "*** %s\n%s\n\n" heading (mapconcat #'identity lines "\n"))))

(defun delib-flow--keyword-plist-p (value)
  "Return non-nil when VALUE looks like a keyword plist."
  (and (listp value)
       (keywordp (car value))))

(defun delib-flow--keyword-plist-pairs (plist)
  "Return display PAIRS from keyword PLIST."
  (let (pairs)
    (while plist
      (push (cons (substring (symbol-name (car plist)) 1)
                  (cadr plist))
            pairs)
      (setq plist (cddr plist)))
    (nreverse pairs)))

(defun delib-flow--render-plain-list (plain)
  "Return readable text for list PLAIN."
  (if (delib-flow--keyword-plist-p plain)
      (delib-flow--bullet-lines
       (delib-flow--keyword-plist-pairs plain))
    (pp-to-string plain)))

(defun delib-flow--render-plain-atom (plain)
  "Return readable text for atomic PLAIN."
  (if (null plain) "nil" (format "%s" plain)))

(defun delib-flow--render-plain-value (plain)
  "Return readable text rendering for sanitized PLAIN value."
  (cond
   ((stringp plain) plain)
   ((listp plain) (delib-flow--render-plain-list plain))
   (t (delib-flow--render-plain-atom plain))))

(defun delib-flow--render-maybe-structured-text (value)
  "Return readable text rendering for VALUE."
  (delib-flow--render-plain-value
   (delib-flow--plain-value value)))

(defun delib-flow--source-outline-path-text (source)
  "Return display text for SOURCE outline path."
  (let ((outline-path (plist-get source :outline-path)))
    (or (and outline-path (mapconcat #'identity outline-path " > "))
        "No outline path")))

(defun delib-flow--inspect-candidate-output (run)
  "Return the current inspect candidate output from RUN, if any."
  (plist-get
   (delib-flow--review-record (delib-flow--run-working-context run) 'inspect-source)
   :candidate-output))

(defun delib-flow--match-candidate-output (run)
  "Return the current project-match candidate output from RUN, if any."
  (plist-get
   (delib-flow--review-record (delib-flow--run-working-context run) 'match-project)
   :candidate-output))

(defun delib-flow--source-has-next-steps-p (source)
  "Return non-nil when SOURCE content mentions next steps."
  (string-match-p "next steps?:" (downcase (or (plist-get source :content) ""))))

(defun delib-flow--source-has-followup-p (source)
  "Return non-nil when SOURCE content mentions follow-up."
  (string-match-p "follow[- ]?up" (downcase (or (plist-get source :content) ""))))

(defun delib-flow--source-has-org-links-p (inspect-output)
  "Return non-nil when INSPECT-OUTPUT reports Org file links."
  (> (or (plist-get inspect-output :org-file-link-count) 0) 0))

(defun delib-flow--source-followup-clues (source inspect-output)
  "Return lightweight follow-up clues from SOURCE and INSPECT-OUTPUT."
  (let ((emails (plist-get inspect-output :contact-emails)))
    (delq nil
          (list
           (when emails
             (format "- Contacts present: %s"
                     (mapconcat #'identity emails ", ")))
           (when (delib-flow--source-has-next-steps-p source)
             "- Mentions explicit next steps.")
           (when (delib-flow--source-has-followup-p source)
             "- Mentions follow-up.")
           (when (delib-flow--source-has-org-links-p inspect-output)
             "- Links to Org files that may provide supporting context.")))))

(defun delib-flow--inspect-source-review-text (run)
  "Return editable inspect review text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'inspect-source-review)))

(defun delib-flow--inspect-source-type-override (run)
  "Return requested source-type override from RUN, if any."
  (when-let* ((text (delib-flow--inspect-source-review-text run))
              (_ (string-match "^Source type override:[ \t]*\\(.*\\)$" text)))
    (let ((value (downcase (string-trim (match-string 1 text)))))
      (unless (string-empty-p value)
        value))))

(defun delib-flow--inspect-source-review-notes (run)
  "Return trimmed inspect review notes from RUN."
  (when-let* ((text (delib-flow--inspect-source-review-text run)))
    (when (string-match "^[Nn]otes:[ \t\n]*\\(\\(?:.\\|\n\\)*\\)\\'" text)
      (string-trim (or (match-string 1 text) "")))))

(defun delib-flow--valid-source-type-override-p (value)
  "Return non-nil when VALUE is a supported source-type override."
  (member value '("email" "meeting-note" "fleeting-note" "reminder" "unknown")))

(defun delib-flow--inspect-warning-missing-reason (inspect-output)
  "Return warning when INSPECT-OUTPUT lacks a source-type reason."
  (unless (delib-flow--non-empty-string-p
           (plist-get inspect-output :source-type-reason))
    "- Source type reason is missing."))

(defun delib-flow--inspect-warning-meeting-note-path (source inspect-output)
  "Return warning when SOURCE path conflicts with INSPECT-OUTPUT type."
  (let ((source-type (plist-get inspect-output :source-type))
        (file (downcase (or (plist-get source :file) ""))))
    (when (and (eq source-type 'meeting-note)
               (string-match-p "inbox\\|mail\\|email" file))
      "- Classified as meeting-note, but the file path suggests inbox or email storage.")))

(defun delib-flow--inspect-warning-empty-preview (source inspect-output)
  "Return warning when SOURCE has content but INSPECT-OUTPUT lacks preview."
  (let ((content (plist-get source :content))
        (body-preview (plist-get inspect-output :body-preview)))
    (when (and (delib-flow--non-empty-string-p content)
               (not (delib-flow--non-empty-string-p body-preview)))
      "- Body preview is empty even though source content exists.")))

(defun delib-flow--inspect-warning-missing-title (inspect-output)
  "Return warning when INSPECT-OUTPUT lacks title."
  (unless (plist-get inspect-output :title)
    "- Title is missing from the inspect result."))

(defun delib-flow--inspect-warning-missing-body-lines (inspect-output)
  "Return warning when INSPECT-OUTPUT lacks body-line count."
  (unless (plist-get inspect-output :body-line-count)
    "- Body-line count is missing from the inspect result."))

(defun delib-flow--inspect-source-quality-base-warnings (source inspect-output)
  "Return baseline inspect warnings from SOURCE and INSPECT-OUTPUT."
  (delq nil
        (list
         (delib-flow--inspect-warning-missing-reason inspect-output)
         (delib-flow--inspect-warning-meeting-note-path source inspect-output)
         (delib-flow--inspect-warning-empty-preview source inspect-output)
         (delib-flow--inspect-warning-missing-title inspect-output)
         (delib-flow--inspect-warning-missing-body-lines inspect-output))))

(defun delib-flow--inspect-source-override-warning (run)
  "Return override warning line for RUN, if any."
  (when-let ((override (delib-flow--inspect-source-type-override run)))
    (if (delib-flow--valid-source-type-override-p override)
        (format "- Pending source-type override: %s." override)
      (format "- Invalid source-type override: %s." override))))

(defun delib-flow--inspect-source-quality-warnings (run inspect-output)
  "Return lightweight quality warnings for INSPECT-OUTPUT in RUN."
  (let ((warnings
         (delib-flow--inspect-source-quality-base-warnings
          (delib-flow--run-source run)
          inspect-output)))
    (if-let ((override-warning (delib-flow--inspect-source-override-warning run)))
        (append warnings (list override-warning))
      warnings)))

(defun delib-flow--manual-project-selection-text (run)
  "Return editable manual project-selection text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'manual-project-selection)))

(defun delib-flow--manual-project-selection-active-p (run)
  "Return non-nil when RUN should surface manual project selection."
  (or (delib-flow--stage-executed-p run 'manual-project-match)
      (and (delib-flow--stage-accepted-p run 'match-project)
           (memq (delib-flow--match-status run) '(ambiguous no-match)))))

(defun delib-flow--manual-project-override-current-p (run)
  "Return non-nil when the current manual override block still matches RUN."
  (let* ((working (delib-flow--run-working-context run))
         (project-match (plist-get working :project-match)))
    (when (eq (plist-get project-match :selection-method) 'manual)
      (let* ((package (delib-flow--stage-input-package run 'manual-project-match))
             (selection (delib-flow--manual-project-selection-value package))
             (notes (delib-flow--manual-project-selection-notes package)))
        (and (delib-flow--non-empty-string-p selection)
             (string-equal selection
                           (or (plist-get project-match :operator-selection) ""))
             (string-equal (or notes "")
                           (or (plist-get project-match :operator-notes) "")))))))

(defun delib-flow--current-project-decision (run)
  "Return the effective current project decision from RUN."
  (let* ((working (delib-flow--run-working-context run))
         (project-match (plist-get working :project-match))
         (match-review (delib-flow--review-record working 'match-project))
         (accepted-match (plist-get match-review :accepted-output)))
    (cond
     ((and (eq (plist-get project-match :selection-method) 'manual)
           (delib-flow--manual-project-override-current-p run))
      project-match)
     ((eq (plist-get project-match :selection-method) 'proposed-filed)
      project-match)
     (accepted-match accepted-match)
     (project-match project-match)
     (t nil))))

(defun delib-flow--project-match-text (project-match)
  "Return display text for PROJECT-MATCH."
  (let ((status (delib-flow--project-match-status project-match)))
    (cond
     ((eq status 'matched)
      (format "Matched: %s"
              (plist-get (plist-get project-match :best-project) :title)))
     ((eq status 'ambiguous)
      "Ambiguous match requires review.")
     ((eq status 'no-match)
      "No project match found.")
     (t
      "Project matching has not run yet."))))

(defun delib-flow--accepted-project-decision (run)
  "Return the effective accepted project decision from RUN."
  (let* ((working (delib-flow--run-working-context run))
         (project-match (plist-get working :project-match))
         (match-review (delib-flow--review-record working 'match-project))
         (accepted-match (plist-get match-review :accepted-output)))
    (cond
     ((and (eq (plist-get project-match :selection-method) 'manual)
           (delib-flow--manual-project-override-current-p run))
      project-match)
     ((eq (plist-get project-match :selection-method) 'proposed-filed)
      project-match)
     (accepted-match accepted-match)
     (t nil))))

(defun delib-flow--accepted-project-status (run)
  "Return accepted project-decision display status from RUN."
  (if-let ((project (delib-flow--accepted-project-decision run)))
      (delib-flow--project-match-status project)
    'not-available))

(defun delib-flow--accepted-project-text (run)
  "Return accepted project-decision display text from RUN."
  (if-let ((project (delib-flow--accepted-project-decision run)))
      (delib-flow--project-match-text project)
    "No accepted project decision is available yet."))

(defun delib-flow--latest-stage-entry (run)
  "Return the latest stage-history entry from RUN, if any."
  (car (last (plist-get (delib-flow--run-stage-history run) :entries))))

(defun delib-flow--latest-cloud-shadow-entry (run)
  "Return the most recent rerouted cloud shadow entry from RUN, if any."
  (seq-find #'delib-flow--cloud-shadow-entry-p
            (reverse (plist-get (delib-flow--run-stage-history run) :entries))))

(defun delib-flow--latest-entry-for-stage (run stage-id)
  "Return the most recent stage-history entry for STAGE-ID in RUN."
  (seq-find (lambda (entry)
              (eq (plist-get entry :stage-id) stage-id))
            (reverse (plist-get (delib-flow--run-stage-history run) :entries))))

(defun delib-flow--accepted-inspect-output (run)
  "Return the accepted inspect output from RUN, if any."
  (plist-get
   (delib-flow--review-record (delib-flow--run-working-context run) 'inspect-source)
   :accepted-output))

(defun delib-flow--accepted-source-type-line (inspect-output)
  "Return accepted source-type line from INSPECT-OUTPUT, if any."
  (when inspect-output
    (format "- Accepted source type: %s"
            (or (plist-get inspect-output :source-type)
                "unknown"))))

(defun delib-flow--accepted-working-context-text (run)
  "Return compact accepted working-context text from RUN."
  (let* ((working (delib-flow--run-working-context run))
         (inspect-output (delib-flow--accepted-inspect-output run))
         (retained-context (plist-get working :retained-context))
         (cloud-returned (plist-get working :cloud-returned-context))
         (lines
          (list
           (format "- Accepted inspect result: %s"
                   (if inspect-output "available" "not available"))
           (format "- Accepted project decision: %s"
                   (delib-flow--accepted-project-text run))
           (format "- Retained context: %s"
                   (if retained-context "available" "not available"))
           (format "- Cloud-returned context: %s"
                   (if cloud-returned "available" "not available"))))
         (source-type-line
          (delib-flow--accepted-source-type-line inspect-output)))
    (when source-type-line
      (setq lines (append lines (list source-type-line))))
    (string-join lines "\n")))

(defun delib-flow--latest-stage-prompt-availability (value)
  "Return user-facing availability text for VALUE."
  (if value "available" "not available"))

(defun delib-flow--latest-stage-rendered-prompt-line (prompt)
  "Return the rendered-prompt summary line for PROMPT."
  (format "- Rendered prompt: %s"
          (delib-flow--latest-stage-prompt-availability
           (delib-flow--non-empty-string-p
            (plist-get prompt :rendered-text)))))

(defun delib-flow--latest-stage-structured-guidance-line (prompt)
  "Return the structured-guidance summary line for PROMPT."
  (format "- Structured prompt guidance: %s"
          (delib-flow--latest-stage-prompt-availability
           (plist-get prompt :structured-guidance))))

(defun delib-flow--latest-stage-prompt-summary-lines (package entry)
  "Return prompt-centric latest-input summary lines for PACKAGE and ENTRY."
  (let ((prompt (plist-get package :prompt)))
    (list
     (format "- Stage: %s" (plist-get entry :label))
     (format "- Prompt ID: %s" (or (plist-get entry :prompt-id) "none"))
     (format "- Prompt status: %s" (or (plist-get prompt :status) "none"))
     (delib-flow--latest-stage-rendered-prompt-line prompt)
     (delib-flow--latest-stage-structured-guidance-line prompt)
     (format "- Example structures: %s"
             (or (plist-get prompt :example-structures-status) "none")))))

(defun delib-flow--latest-stage-workflow-summary-lines (run package)
  "Return workflow-centric latest-input summary lines for RUN and PACKAGE."
  (list
   (format "- Source title: %s"
           (or (plist-get (plist-get package :source) :title)
               "Untitled source"))
   (format "- Accepted project decision: %s"
           (delib-flow--accepted-project-text run))
   (format "- Retrieved candidates: %s"
           (length (or (plist-get (plist-get package :working-context)
                                  :retrieved-candidates)
                       nil)))
   (format "- Draft artifacts: %s"
           (length (or (plist-get (plist-get package :filing)
                                  :draft-items)
                       nil)))))

(defun delib-flow--latest-stage-input-summary-lines (run package entry)
  "Return latest prompt/input summary lines for RUN PACKAGE and ENTRY."
  (append
   (delib-flow--latest-stage-prompt-summary-lines package entry)
   (delib-flow--latest-stage-workflow-summary-lines run package)))

(defun delib-flow--latest-stage-input-text (run)
  "Return latest stage input text from RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (let* ((package (plist-get entry :input-package))
             (prompt (plist-get package :prompt)))
        (format "*** Latest prompt/input summary\n%s\n\n**** Resolved prompt\n#+begin_example\n%s#+end_example\n\n**** Debug raw input\n#+begin_example\n%s\n#+end_example\n"
                (string-join
                 (delib-flow--latest-stage-input-summary-lines
                  run package entry)
                 "\n")
                (or (plist-get prompt :rendered-text)
                    "No resolved prompt text is available.\n")
                (pp-to-string package)))
    "*** Latest prompt/input summary\nNo stage input has been recorded yet.\n"))

(defun delib-flow--latest-stage-raw-output-text (run)
  "Return latest raw-output text from RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (format "*** Latest raw output\n- Stage: %s\n\n%s\n\n**** Debug raw output\n#+begin_example\n%s\n#+end_example\n"
              (plist-get entry :label)
              (delib-flow--render-maybe-structured-text
               (plist-get entry :raw-output))
              (pp-to-string (plist-get entry :raw-output)))
    "*** Latest raw output\nNo raw output has been recorded yet.\n"))

(defun delib-flow--debug-count (items)
  "Return count of ITEMS, treating nil as zero."
  (length (or items nil)))

(defun delib-flow--debug-availability-text (value)
  "Return availability text for VALUE."
  (if value "available" "not available"))

(defun delib-flow--debug-change-line (label before after formatter)
  "Return formatted change line for LABEL from BEFORE to AFTER using FORMATTER."
  (unless (equal before after)
    (format "- %s: %s -> %s"
            label
            (funcall formatter before)
            (funcall formatter after))))

(defun delib-flow--debug-state-change-lines (run entry)
  "Return tracked state-change summary lines for RUN after ENTRY."
  (let* ((package (plist-get entry :input-package))
         (before-working (plist-get package :working-context))
         (after-working (delib-flow--run-working-context run))
         (before-filing (plist-get package :filing))
         (after-filing (plist-get run :filing))
         (before-routing (plist-get package :routing))
         (after-routing (delib-flow--run-routing run))
         (lines
          (delq
           nil
           (list
            (delib-flow--debug-change-line
             "Retrieved candidates"
             (delib-flow--debug-count (plist-get before-working :retrieved-candidates))
             (delib-flow--debug-count (plist-get after-working :retrieved-candidates))
             #'number-to-string)
            (delib-flow--debug-change-line
             "Retained context"
             (plist-get before-working :retained-context)
             (plist-get after-working :retained-context)
             #'delib-flow--debug-availability-text)
            (delib-flow--debug-change-line
             "Cloud-returned context"
             (plist-get before-working :cloud-returned-context)
             (plist-get after-working :cloud-returned-context)
             #'delib-flow--debug-availability-text)
            (delib-flow--debug-change-line
             "Cloud target stage"
             (delib-flow--cloud-target-stage before-routing)
             (delib-flow--cloud-target-stage after-routing)
             #'delib-flow--stage-label)
            (delib-flow--debug-change-line
             "Draft artifacts"
             (delib-flow--debug-count (plist-get before-filing :draft-items))
             (delib-flow--debug-count (plist-get after-filing :draft-items))
             #'number-to-string)
            (delib-flow--debug-change-line
             "Approved artifacts"
             (delib-flow--debug-count (plist-get before-filing :approved-items))
             (delib-flow--debug-count (plist-get after-filing :approved-items))
             #'number-to-string)
            (delib-flow--debug-change-line
             "Blocking warnings"
             (delib-flow--debug-count (plist-get before-filing :selection-blocking-warnings))
             (delib-flow--debug-count (plist-get after-filing :selection-blocking-warnings))
             #'number-to-string)
            (delib-flow--debug-change-line
             "Filing conflicts"
             (delib-flow--debug-count (plist-get before-filing :conflicts))
             (delib-flow--debug-count (plist-get after-filing :conflicts))
             #'number-to-string)))))
    (or lines
        '("- No tracked state changes across core workflow domains."))))

(defun delib-flow--project-candidate-titles (project-match)
  "Return display text for PROJECT-MATCH candidate titles."
  (let ((candidates (plist-get project-match :candidates)))
    (if candidates
        (mapconcat (lambda (candidate)
                     (plist-get candidate :title))
                   candidates
                   ", ")
      "none")))

(defun delib-flow--debug-project-diagnostic-lines (run)
  "Return project-decision diagnostics for RUN."
  (let* ((project-match (plist-get (delib-flow--run-working-context run) :project-match))
         (best (plist-get project-match :best-project)))
    (list
     (format "- Match status: %s"
             (or (plist-get project-match :match-status) 'not-available))
     (format "- Selection method: %s"
             (or (plist-get project-match :selection-method) 'automatic))
     (format "- Best project: %s"
             (or (plist-get best :title) "none"))
     (format "- Candidate titles: %s"
             (delib-flow--project-candidate-titles project-match))
     (format "- Decision reason: %s"
             (or (plist-get project-match :reason) "none")))))

(defconst delib-flow--debug-cloud-diagnostic-specs
  '((:label "Sanitization status"
     :key :sanitization-status)
    (:label "Reintegration status"
     :key :reintegration-status)
    (:label "Cloud failure stage"
     :key :cloud-failure-stage)
    (:label "Cloud fallback mode"
     :key :cloud-fallback-mode)
    (:label "Cloud failure message"
     :key :cloud-failure-message))
  "Simple routing fields to surface in cloud debug diagnostics.")

(defun delib-flow--debug-cloud-diagnostic-lines (run)
  "Return cloud-routing diagnostics for RUN."
  (let ((routing (delib-flow--run-routing run)))
    (cons
     (format "- Cloud target stage: %s"
             (delib-flow--stage-label
              (delib-flow--cloud-target-stage routing)))
     (mapcar (lambda (spec)
               (format "- %s: %s"
                       (plist-get spec :label)
                       (or (plist-get routing (plist-get spec :key))
                           "none")))
             delib-flow--debug-cloud-diagnostic-specs))))

(defun delib-flow--debug-filing-diagnostic-lines (run)
  "Return filing diagnostics for RUN."
  (let* ((filing (plist-get run :filing))
         (draft-items (plist-get filing :draft-items))
         (approved-items (plist-get filing :approved-items))
         (warnings (plist-get filing :selection-blocking-warnings))
         (conflicts (plist-get filing :conflicts)))
    (list
     (format "- Draft artifacts: %d"
             (delib-flow--debug-count draft-items))
     (format "- Approved artifacts: %d"
             (delib-flow--debug-count approved-items))
     (format "- Blocking warnings: %d"
             (delib-flow--debug-count warnings))
     (format "- Filing conflicts: %d"
             (delib-flow--debug-count conflicts))
     (format "- Draft preview head: %s"
             (if draft-items
                 (plist-get (car draft-items) :text)
               "none")))))

(defun delib-flow--debug-state-snapshot (run)
  "Return compact comparable debug-state snapshot from RUN."
  (let* ((working (delib-flow--run-working-context run))
         (routing (delib-flow--run-routing run))
         (filing (plist-get run :filing))
         (latest-entry (delib-flow--latest-stage-entry run))
         (latest-audit (delib-flow--audit-latest-stage-record run))
         (project-match (plist-get working :project-match)))
    (list
     :project-match-status (plist-get project-match :match-status)
     :project-selection-method (plist-get project-match :selection-method)
     :best-project-title
     (plist-get (plist-get project-match :best-project) :title)
     :retrieved-candidate-count
     (delib-flow--debug-count (plist-get working :retrieved-candidates))
     :retained-context-available-p
     (not (null (plist-get working :retained-context)))
     :cloud-returned-context-available-p
     (not (null (plist-get working :cloud-returned-context)))
     :cloud-target-stage (delib-flow--cloud-target-stage routing)
     :sanitization-status (plist-get routing :sanitization-status)
     :reintegration-status (plist-get routing :reintegration-status)
     :cloud-failure-stage (plist-get routing :cloud-failure-stage)
     :cloud-fallback-mode (plist-get routing :cloud-fallback-mode)
     :draft-item-count (delib-flow--debug-count (plist-get filing :draft-items))
     :approved-item-count
     (delib-flow--debug-count (plist-get filing :approved-items))
     :filing-conflict-count (delib-flow--debug-count (plist-get filing :conflicts))
     :target-location-count
     (delib-flow--debug-count (plist-get filing :target-locations))
     :latest-stage-id (plist-get latest-entry :stage-id)
     :latest-stage-status (plist-get latest-entry :status)
     :latest-stage-provider (delib-flow--audit-provider latest-entry)
     :stage-history-count
     (delib-flow--debug-count
      (plist-get (delib-flow--run-stage-history run) :entries))
     :audit-stage-count
     (delib-flow--debug-count (plist-get (plist-get run :audit) :stage-records))
     :latest-audit-stage-id (plist-get latest-audit :stage-id)
     :latest-audit-attempt-number (plist-get latest-audit :attempt-number))))

(defun delib-flow--debug-stage-label-snapshot-key-p (key)
  "Return non-nil when snapshot KEY should render as a stage label."
  (memq key '(:cloud-target-stage :cloud-failure-stage
              :latest-stage-id :latest-audit-stage-id)))

(defun delib-flow--debug-snapshot-value-text (key value)
  "Return comparable display text for snapshot KEY and VALUE."
  (if (delib-flow--debug-stage-label-snapshot-key-p key)
      (if value (delib-flow--stage-label value) "none")
    (if value
        (format "%s" value)
      "none")))

(defconst delib-flow--debug-snapshot-compare-specs
  '((:label "Project match status" :key :project-match-status)
    (:label "Project selection method" :key :project-selection-method)
    (:label "Best project title" :key :best-project-title)
    (:label "Retrieved candidates" :key :retrieved-candidate-count)
    (:label "Retained context available" :key :retained-context-available-p)
    (:label "Cloud-returned context available" :key :cloud-returned-context-available-p)
    (:label "Cloud target stage" :key :cloud-target-stage)
    (:label "Sanitization status" :key :sanitization-status)
    (:label "Reintegration status" :key :reintegration-status)
    (:label "Cloud failure stage" :key :cloud-failure-stage)
    (:label "Cloud fallback mode" :key :cloud-fallback-mode)
    (:label "Draft artifacts" :key :draft-item-count)
    (:label "Approved artifacts" :key :approved-item-count)
    (:label "Filing conflicts" :key :filing-conflict-count)
    (:label "Filed targets" :key :target-location-count)
    (:label "Latest stage" :key :latest-stage-id)
    (:label "Latest stage status" :key :latest-stage-status)
    (:label "Latest stage provider" :key :latest-stage-provider)
    (:label "Stage history count" :key :stage-history-count)
    (:label "Audit stage count" :key :audit-stage-count)
    (:label "Latest audit stage" :key :latest-audit-stage-id)
    (:label "Latest audit attempt" :key :latest-audit-attempt-number))
  "Comparable debug snapshot fields to surface in replay diffs.")

(defun delib-flow--debug-snapshot-diff-lines (current baseline)
  "Return summary diff lines between CURRENT and BASELINE snapshots."
  (let ((lines
         (delq
          nil
          (mapcar
           (lambda (spec)
             (let* ((key (plist-get spec :key))
                    (before (plist-get baseline key))
                    (after (plist-get current key)))
               (unless (equal before after)
                 (format "- %s: %s -> %s"
                         (plist-get spec :label)
                         (delib-flow--debug-snapshot-value-text key before)
                         (delib-flow--debug-snapshot-value-text key after)))))
           delib-flow--debug-snapshot-compare-specs))))
    (or lines
        '("- No differences detected across the tracked run snapshot."))))

(defun delib-flow--previous-stage-entry (run entry)
  "Return the previous attempt for ENTRY in RUN, if any."
  (let ((stage-id (plist-get entry :stage-id))
        (seen nil))
    (seq-find
     (lambda (candidate)
       (when (eq (plist-get candidate :stage-id) stage-id)
         (if seen
             t
           (setq seen t)
           nil)))
     (reverse (plist-get (delib-flow--run-stage-history run) :entries)))))

(defun delib-flow--debug-previous-attempt-lines (run entry)
  "Return previous-attempt comparison lines for RUN and latest ENTRY."
  (if-let ((previous (delib-flow--previous-stage-entry run entry)))
      (let* ((entries (plist-get (delib-flow--run-stage-history run) :entries))
             (previous-attempt (delib-flow--entry-attempt-number entries previous))
             (current-attempt (delib-flow--entry-attempt-number entries entry))
             (package-changed
              (not (equal (plist-get previous :input-package)
                          (plist-get entry :input-package))))
             (normalized-changed
              (not (equal (plist-get previous :normalized-output)
                          (plist-get entry :normalized-output))))
             (raw-changed
              (not (equal (plist-get previous :raw-output)
                          (plist-get entry :raw-output))))
             (lines nil))
        (setq lines
              (delq
               nil
               (list
                (format "- Attempt numbers: %s -> %s"
                        previous-attempt current-attempt)
                (delib-flow--debug-change-line
                 "Status"
                 (plist-get previous :status)
                 (plist-get entry :status)
                 #'identity)
                (delib-flow--debug-change-line
                 "Review state"
                 (plist-get previous :review-state)
                 (plist-get entry :review-state)
                 #'identity)
                (delib-flow--debug-change-line
                 "Provider"
                 (delib-flow--audit-provider previous)
                 (delib-flow--audit-provider entry)
                 #'identity)
                (when package-changed
                  "- Input package changed: yes")
                (when normalized-changed
                  "- Normalized output changed: yes")
                (when raw-changed
                  "- Raw output changed: yes"))))
        (or lines
            '("- The latest stage matches the previous attempt across tracked fields.")))
    '("- No previous attempt exists for the latest stage.")))

(defun delib-flow--debug-replay-run (scenario-id checkpoint)
  "Return a fresh replayed debug run for SCENARIO-ID at CHECKPOINT."
  (let ((fixture (delib-flow--activate-debug-fixture scenario-id)))
    (unwind-protect
        (let* ((source (delib-flow--debug-source-snapshot scenario-id fixture))
               (run (delib-flow--set-debug-fixture
                     (delib-flow--initialize-run source)
                     fixture)))
          (delib-flow--debug-replay-to-checkpoint run checkpoint))
      (delib-flow--cleanup-debug-fixture fixture))))

(defun delib-flow--debug-replay-comparison-text (run)
  "Return fresh-replay comparison text for RUN."
  (let* ((session (delib-flow--run-session run))
         (scenario-id (plist-get session :debug-scenario-id))
         (checkpoint (plist-get session :debug-checkpoint)))
    (if (and scenario-id checkpoint)
        (let* ((baseline (delib-flow--debug-replay-run scenario-id checkpoint))
               (lines
                (delib-flow--debug-snapshot-diff-lines
                 (delib-flow--debug-state-snapshot run)
                 (delib-flow--debug-state-snapshot baseline))))
          (format "** Fresh replay comparison\n- Scenario: %s\n- Checkpoint: %s\n%s\n\n"
                  scenario-id
                  checkpoint
                  (string-join lines "\n")))
      "** Fresh replay comparison\n- No debug scenario/checkpoint metadata is available for replay comparison.\n\n")))

(defun delib-flow--debug-previous-attempt-text (run)
  "Return previous-attempt comparison text for RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (format "** Previous attempt comparison\n- Stage: %s\n%s\n\n"
              (plist-get entry :label)
              (string-join
               (delib-flow--debug-previous-attempt-lines run entry)
               "\n"))
    "** Previous attempt comparison\n- No stage attempts have been recorded yet.\n\n"))

(defun delib-flow--debug-comparison-text (run)
  "Return debug comparison text for RUN."
  (format "* Debug comparison\n%s%s"
          (delib-flow--debug-replay-comparison-text run)
          (delib-flow--debug-previous-attempt-text run)))

(defun delib-flow--debug-walkthrough-value (value)
  "Return VALUE or a readable fallback."
  (or value "none"))

(defun delib-flow--debug-walkthrough-target-name (target-id)
  "Return display label for walkthrough TARGET-ID."
  (delib-flow--debug-walkthrough-value
   (and target-id
        (delib-flow--debug-walkthrough-target-label target-id))))

(defun delib-flow--debug-walkthrough-action-lines (actions)
  "Return formatted walkthrough action lines for ACTIONS."
  (if actions
      (mapconcat
       (lambda (action)
         (format "- %s [%s]"
                 (plist-get action :label)
                 (plist-get action :status)))
       actions
       "\n")
    "- No next actions are currently available."))

(defun delib-flow--debug-walkthrough-next-step-name (target-id checkpoint)
  "Return readable next-step label for TARGET-ID after CHECKPOINT."
  (let ((next (delib-flow--debug-walkthrough-next-step-id target-id checkpoint)))
    (if next
        (delib-flow--debug-checkpoint-label next)
      "none")))

(defun delib-flow--debug-walkthrough-check-expectation (target-id checkpoint)
  "Return verification expectation for TARGET-ID at CHECKPOINT."
  (plist-get (alist-get checkpoint
                        (delib-flow--debug-walkthrough-target-checks target-id))
             :expect))

(defun delib-flow--debug-helper-context (run)
  "Return active debug helper context for RUN."
  (cond
   ((delib-flow--manual-project-selection-active-p run) 'manual-project)
   ((delib-flow--cloud-failure-active-p run) 'cloud-failure)
   ((delib-flow--filing-conflict-resolution-active-p run) 'filing-conflict)
   ((delib-flow--filing-selection-active-p run) 'filing-selection)))

(defun delib-flow--debug-helper-candidates (run)
  "Return manual project helper candidates for RUN."
  (let ((package (delib-flow--stage-input-package run 'manual-project-match)))
    (delib-flow--manual-project-match-candidates package)))

(defun delib-flow--debug-helper-first-ready-index (run)
  "Return first ready filing-selection index for RUN."
  (car (delib-flow--draft-item-selection-indexes
        (plist-get (plist-get run :filing) :draft-items)
        #'delib-flow--draft-item-ready-p)))

(defun delib-flow--debug-helper-first-blocked-index (run)
  "Return first blocked filing-selection index for RUN."
  (car (delib-flow--draft-item-selection-indexes
        (plist-get (plist-get run :filing) :draft-items)
        (lambda (item)
          (not (delib-flow--draft-item-ready-p item))))))

(defun delib-flow--debug-helper-approved-item (run)
  "Return current approved filing item for RUN."
  (car (plist-get (plist-get run :filing) :approved-items)))

(defun delib-flow--debug-helper-smart-conflict-resolution (run)
  "Return smart filing-conflict helper preset for RUN."
  (pcase (plist-get (delib-flow--debug-helper-approved-item run) :kind)
    ('reference-note 'smart-rename-note)
    ('project 'smart-retitle-project)
    (_ 'smart-reword-item)))

(defun delib-flow--debug-helper-option (id label)
  "Return helper option pair for ID and LABEL."
  (cons id label))

(defconst delib-flow--debug-cloud-failure-helper-options
  '((retry-cloud . "Set retry cloud resolution")
    (use-local . "Set use-local resolution")
    (skip-cloud . "Set skip-cloud resolution")
    (abort-run . "Set abort resolution"))
  "Static debug helper options for cloud-failure review.")

(defconst delib-flow--debug-filing-conflict-helper-options
  '((retry-conflict . "Keep approved artifact and retry")
    (reject-conflict . "Reject approved artifact"))
  "Static debug helper options for filing-conflict review.")

(defun delib-flow--debug-helper-options-for-manual-project (run)
  "Return debug helper options for manual-project review in RUN."
  (append
   (when-let ((candidate (car (delib-flow--debug-helper-candidates run))))
     (list
      (delib-flow--debug-helper-option
       'choose-first-candidate
       (format "Choose first candidate: %s" (plist-get candidate :title)))))
   (list (delib-flow--debug-helper-option 'reject-all-candidates
                                          "Reject all candidates"))))

(defun delib-flow--debug-helper-options-for-cloud-failure ()
  "Return debug helper options for cloud-failure review."
  (mapcar (lambda (entry)
            (delib-flow--debug-helper-option (car entry) (cdr entry)))
          delib-flow--debug-cloud-failure-helper-options))

(defun delib-flow--debug-helper-options-for-filing-selection (run)
  "Return debug helper options for filing-selection review in RUN."
  (append
   (when-let ((index (delib-flow--debug-helper-first-ready-index run)))
     (list
      (delib-flow--debug-helper-option
       'select-first-ready
       (format "Select first ready artifact: %s" index))))
   (when-let ((index (delib-flow--debug-helper-first-blocked-index run)))
     (list
      (delib-flow--debug-helper-option
       'select-first-blocked
       (format "Select first blocked artifact: %s" index))))))

(defun delib-flow--debug-helper-options-for-filing-conflict (run)
  "Return debug helper options for filing-conflict review in RUN."
  (append
   (mapcar (lambda (entry)
             (delib-flow--debug-helper-option (car entry) (cdr entry)))
           delib-flow--debug-filing-conflict-helper-options)
   (list
    (delib-flow--debug-helper-option
     (delib-flow--debug-helper-smart-conflict-resolution run)
     "Apply smart conflict fix for the approved artifact"))))

(defconst delib-flow--debug-helper-options-function-alist
  '((manual-project . delib-flow--debug-helper-options-for-manual-project)
    (cloud-failure . delib-flow--debug-helper-options-for-cloud-failure)
    (filing-selection . delib-flow--debug-helper-options-for-filing-selection)
    (filing-conflict . delib-flow--debug-helper-options-for-filing-conflict))
  "Context-specific debug helper option builders.")

(defun delib-flow--debug-helper-options (run)
  "Return available debug helper options for RUN."
  (when-let ((fn (alist-get (delib-flow--debug-helper-context run)
                            delib-flow--debug-helper-options-function-alist)))
    (funcall fn run)))

(defun delib-flow--debug-helper-prefix-stripped (text pattern)
  "Return TEXT with leading PATTERN removed."
  (replace-regexp-in-string pattern "" text))

(defun delib-flow--debug-manual-project-preset-text (run selection notes)
  "Return manual project helper block text for RUN with SELECTION and NOTES."
  (format "Selection: %s\nNotes:\n%s\n\n%s"
          selection
          notes
          (delib-flow--debug-helper-prefix-stripped
           (delib-flow--manual-project-selection-template
            (delib-flow--debug-helper-candidates run))
           "\\`Selection: \nNotes:\n\n")))

(defun delib-flow--debug-filing-selection-preset-text (run selection notes)
  "Return filing-selection helper block text for RUN with SELECTION and NOTES."
  (format "Selection: %s\nNotes:\n%s\n\n%s"
          selection
          notes
          (delib-flow--debug-helper-prefix-stripped
           (delib-flow--filing-selection-template
            (plist-get (plist-get run :filing) :draft-items))
           "\\`Selection: \nNotes:\n\n")))

(defun delib-flow--debug-cloud-failure-preset-text (run resolution notes)
  "Return cloud-failure helper block text for RUN with RESOLUTION and NOTES."
  (format "Resolution: %s\nNotes:\n%s\n\n%s"
          resolution
          notes
          (delib-flow--debug-helper-prefix-stripped
           (delib-flow--cloud-failure-review-template run)
           "\\`Resolution: [^\n]*\nNotes:\n\n")))

(defun delib-flow--debug-filing-conflict-preset-text (run resolution notes new-title new-text)
  "Return filing-conflict helper block text for RUN."
  (format "Resolution: %s\nNotes:\n%s\n\nNew title:\n%s\n\nNew text:\n%s\n\n%s"
          resolution
          notes
          (or new-title "")
          (or new-text "")
          (delib-flow--debug-helper-prefix-stripped
           (delib-flow--filing-conflict-resolution-template run)
           "\\`Resolution: [^\n]*\nNotes:\n\nNew title:\n\nNew text:\n\n")))

(defun delib-flow--debug-walkthrough-current-check-text (target-id checkpoint)
  "Return active verification guidance for TARGET-ID at CHECKPOINT."
  (let ((expectation
         (delib-flow--debug-walkthrough-check-expectation target-id checkpoint)))
    (format "** Current checkpoint verification\n- Expectation: %s\n- Inspect with `D` for prompt/package/output, then `C` if you need a replay diff.\n- Use `H` when you want a valid helper preset for the current manual review block.\n- Confirm persisted audit shape with `j` / `J` before stepping forward.\n\n"
            (delib-flow--debug-walkthrough-value expectation))))

(defun delib-flow--debug-walkthrough-standard-loop-text ()
  "Return the standard operator verification loop text."
  "** Verification loop\n1. Start a walkthrough target with `M-x delib-flow-debug-start-walkthrough` or continue the active target shown above.\n2. At each checkpoint, use `D` to inspect stage payloads and `j` / `J` to confirm the persisted audit subtree.\n3. Use `C` after code changes or retries to compare against the deterministic replay baseline.\n4. Use `N` to move to the next checkpoint once the current expectation matches, or `R` to restart the target from baseline.\n\n")

(defun delib-flow--debug-walkthrough-run-summary (run)
  "Return walkthrough summary text for active RUN."
  (let* ((session (delib-flow--run-session run))
          (scenario-id (plist-get session :debug-scenario-id))
          (checkpoint (plist-get session :debug-checkpoint))
          (target-id (plist-get session :debug-walkthrough-target-id))
          (latest-entry (delib-flow--latest-stage-entry run))
          (actions (seq-take (delib-flow--sorted-actions run) 3))
          (action-lines (delib-flow--debug-walkthrough-action-lines actions)))
    (format "** Active run\n- Walkthrough target: %s\n- Scenario: %s\n- Checkpoint: %s\n- Next checkpoint: %s\n- Latest stage: %s\n- Current decision: %s\n\n** Recommended next checks\n- `N`: advance to the next walkthrough checkpoint.\n- `R`: restart this walkthrough target from its baseline checkpoint.\n- `D`: inspect the latest package, prompt, and output.\n- `C`: compare this run against its replay baseline and prior attempt.\n- `j` / `J`: inspect the persisted audit trail.\n%s\n\n"
            (delib-flow--debug-walkthrough-target-name target-id)
            (delib-flow--debug-walkthrough-value scenario-id)
            (delib-flow--debug-walkthrough-value checkpoint)
            (delib-flow--debug-walkthrough-next-step-name target-id checkpoint)
            (delib-flow--debug-walkthrough-value (plist-get latest-entry :label))
            (delib-flow--debug-walkthrough-value
             (plist-get session :current-decision))
            action-lines)))

(defun delib-flow--debug-walkthrough-target-check-text (target-id checkpoint)
  "Return checkpoint verification text for TARGET-ID recipe entry at CHECKPOINT."
  (format "- %s: %s"
          (delib-flow--debug-checkpoint-label checkpoint)
          (delib-flow--debug-walkthrough-check-expectation target-id checkpoint)))

(defun delib-flow--debug-walkthrough-target-text (target-id)
  "Return walkthrough recipe text for TARGET-ID."
  (let* ((target (delib-flow--debug-walkthrough-target target-id))
         (scenario-id (plist-get target :scenario))
         (checkpoint (plist-get target :checkpoint))
         (checks (mapconcat
                  (lambda (step)
                    (delib-flow--debug-walkthrough-target-check-text target-id step))
                  (delib-flow--debug-walkthrough-target-steps target-id)
                  "\n"))
         (steps (mapconcat #'symbol-name
                           (delib-flow--debug-walkthrough-target-steps target-id)
                           " -> ")))
    (format "*** %s\n- Scenario: %s\n- Checkpoint: %s\n- Steps: %s\n- Objective: %s\n- Start command: `M-x delib-flow-debug-start-walkthrough`\n- Navigate with: `N`, `R`, `D`, `C`, `j`, `J`\n- Verification checks:\n%s\n"
            (plist-get target :label)
            scenario-id
            checkpoint
            steps
            (plist-get target :objective)
            checks)))

(defun delib-flow--debug-walkthrough-recipes-text ()
  "Return walkthrough recipe text for all known targets."
  (mapconcat #'delib-flow--debug-walkthrough-target-text
             (delib-flow--debug-walkthrough-target-ids)
             "\n"))

(defun delib-flow--debug-walkthrough-text ()
  "Return walkthrough guide text for the current debug framework state."
  (format "* Debug walkthrough\n%s%s%s** Walkthrough targets\n%s"
          (if delib-flow--active-run
              (delib-flow--debug-walkthrough-run-summary delib-flow--active-run)
            "** Active run\n- No debug run is active.\n- Start one with `M-x delib-flow-debug-start-walkthrough` or `M-x delib-flow-debug-start-scenario`.\n\n")
          (if delib-flow--active-run
              (delib-flow--debug-walkthrough-current-check-text
               (delib-flow--debug-active-walkthrough-target-id)
               (delib-flow--debug-active-walkthrough-checkpoint))
            "")
          (delib-flow--debug-walkthrough-standard-loop-text)
          (delib-flow--debug-walkthrough-recipes-text)))

(defun delib-flow--open-debug-buffer (text)
  "Open debug buffer with TEXT."
  (let ((buffer (get-buffer-create delib-flow-debug-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert text)
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun delib-flow--debug-stage-diagnostic-sections (run entry)
  "Return stage-specific diagnostic section text for RUN and ENTRY."
  (let ((stage-id (plist-get entry :stage-id)))
    (concat
     (if (memq stage-id '(match-project manual-project-match propose-new-project))
         (format "** Project diagnostics\n%s\n\n"
                 (string-join (delib-flow--debug-project-diagnostic-lines run) "\n"))
       "")
     (if (memq stage-id '(decide-cloud-pass sanitize-for-cloud approve-cloud-send
                          run-cloud-stage resolve-cloud-failure
                          approve-candidate-reintegration integrate-into-source))
         (format "** Cloud diagnostics\n%s\n\n"
                 (string-join (delib-flow--debug-cloud-diagnostic-lines run) "\n"))
       "")
     (if (memq stage-id '(extract-actions extract-waiting-for suggest-reference-notes
                          propose-new-project integrate-into-source
                          select-approved-filing-actions file-approved-outputs
                          resolve-filing-conflict reject-draft-filing-artifact))
         (format "** Filing diagnostics\n%s\n\n"
                 (string-join (delib-flow--debug-filing-diagnostic-lines run) "\n"))
       ""))))

(defun delib-flow--debug-latest-stage-inspection-text (run)
  "Return detailed debug inspection text for the latest stage in RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (let* ((package (plist-get entry :input-package))
             (prompt (plist-get package :prompt))
             (review-record
              (delib-flow--review-record (delib-flow--run-working-context run)
                                         (plist-get entry :stage-id))))
        (format "* Latest stage debug inspection\n- Stage: %s\n- Status: %s\n- Review state: %s\n- Provider: %s\n- Prompt status: %s\n- Structured guidance: %s\n- Accepted review output: %s\n\n** State change summary\n%s\n\n%s** Resolved prompt\n#+begin_example\n%s#+end_example\n\n** Input package\n#+begin_example\n%s\n#+end_example\n\n** Normalized output\n#+begin_example\n%s\n#+end_example\n\n** Raw output\n#+begin_example\n%s\n#+end_example\n"
                (plist-get entry :label)
                (plist-get entry :status)
                (plist-get entry :review-state)
                (delib-flow--audit-provider entry)
                (or (plist-get prompt :status) "none")
                (if (plist-get prompt :structured-guidance)
                    "available"
                  "not available")
                (if (plist-get review-record :accepted-output)
                    "available"
                  "not available")
                (string-join
                 (delib-flow--debug-state-change-lines run entry)
                 "\n")
                (delib-flow--debug-stage-diagnostic-sections run entry)
                (or (plist-get prompt :rendered-text)
                    "No resolved prompt text is available.\n")
                (pp-to-string package)
                (pp-to-string (plist-get entry :normalized-output))
                (pp-to-string (plist-get entry :raw-output))))
    "* Latest stage debug inspection\nNo stage output is available yet.\n"))

(defun delib-flow--current-result-entry (run)
  "Return the stage entry currently relevant for the operator in RUN."
  (let ((latest-entry (delib-flow--latest-stage-entry run)))
    (if (eq (plist-get latest-entry :stage-id) 'run-cloud-stage)
        (or (delib-flow--latest-cloud-shadow-entry run)
            latest-entry)
      latest-entry)))

(defun delib-flow--current-result-summary-lines (entry)
  "Return summary lines for current-result ENTRY."
  (append
   (list
    (format "- Stage: %s" (plist-get entry :label))
    (format "- Status: %s" (plist-get entry :status))
    (format "- Review state: %s" (plist-get entry :review-state)))
   (when (delib-flow--cloud-shadow-entry-p entry)
     (list (format "- Transport stage: %s"
                   (delib-flow--cloud-transport-stage-label))))))

(defun delib-flow--inspect-result-summary-lines (entry)
  "Return summary lines for inspect ENTRY."
  (list
   (format "- Stage: %s" (plist-get entry :label))
   (format "- Status: %s" (plist-get entry :status))
   (format "- Review state: %s" (plist-get entry :review-state))))

(defun delib-flow--inspect-output-outline-path-text (output)
  "Return display text for inspect OUTPUT outline path."
  (or (and (plist-get output :outline-path)
           (mapconcat #'identity (plist-get output :outline-path) " > "))
      "No outline path"))

(defun delib-flow--inspect-output-contact-text (output)
  "Return contact display text for inspect OUTPUT."
  (if-let ((emails (plist-get output :contact-emails)))
      (mapconcat #'identity emails ", ")
    "none"))

(defun delib-flow--inspect-output-body-preview (output)
  "Return body preview display text for inspect OUTPUT."
  (or (plist-get output :body-preview) "No body preview"))

(defun delib-flow--inspect-output-summary-text (output)
  "Return summary display text for inspect OUTPUT."
  (or (plist-get (plist-get output :analysis) :summary)
      (plist-get output :reason)
      "none"))

(defun delib-flow--inspect-output-entities-text (output)
  "Return entity display text for inspect OUTPUT."
  (if-let ((entities (plist-get (plist-get output :analysis) :entities)))
      (mapconcat #'identity entities ", ")
    "none"))

(defun delib-flow--inspect-output-signal-text (output)
  "Return source-type signal display text for inspect OUTPUT."
  (if-let ((signals (plist-get output :source-type-signals)))
      (mapconcat #'identity signals ", ")
    "none"))

(defun delib-flow--inspect-result-pairs (output)
  "Return inspect result display pairs for OUTPUT."
  `(("Source type" . ,(plist-get output :source-type))
    ("Source type reason" . ,(or (plist-get output :source-type-reason) "nil"))
    ("Source type signals" . ,(delib-flow--inspect-output-signal-text output))
    ("Title" . ,(or (plist-get output :title) "Untitled source"))
    ("Outline path" . ,(delib-flow--inspect-output-outline-path-text output))
    ("Body lines" . ,(plist-get output :body-line-count))
    ("Content words" . ,(plist-get output :content-word-count))
    ("Contact emails" . ,(delib-flow--inspect-output-contact-text output))
    ("Org file links" . ,(or (plist-get output :org-file-link-count) 0))
    ("Entities" . ,(delib-flow--inspect-output-entities-text output))
    ("Summary" . ,(delib-flow--inspect-output-summary-text output))
    ("Body preview" . ,(delib-flow--inspect-output-body-preview output))))

(defun delib-flow--inspect-result-meaning-text ()
  "Return accept-meaning text for inspect results."
  "- Accepting this result will unlock project matching and make this source analysis the accepted inspect context.\n")

(defun delib-flow--inspect-result-text (run entry)
  "Return readable current-result text for inspect-source ENTRY in RUN."
  (let* ((output (or (delib-flow--inspect-candidate-output run)
                     (plist-get entry :raw-output)))
         (warnings (delib-flow--inspect-source-quality-warnings run output)))
    (concat
     "*** Summary\n"
     (string-join (delib-flow--inspect-result-summary-lines entry) "\n")
     "\n\n"
     "*** Result\n"
     (delib-flow--bullet-lines
      (delib-flow--inspect-result-pairs output))
     "\n\n"
     (or (delib-flow--lines-subsection "Quality warnings" warnings) "")
     "*** Accepting this result means\n"
     (delib-flow--inspect-result-meaning-text))))

(defun delib-flow--match-result-summary-lines (entry)
  "Return summary lines for match ENTRY."
  (list
   (format "- Stage: %s" (plist-get entry :label))
   (format "- Status: %s" (plist-get entry :status))
   (format "- Review state: %s" (plist-get entry :review-state))))

(defun delib-flow--match-candidate-title-list (candidates)
  "Return display text for match CANDIDATES."
  (if candidates
      (mapconcat (lambda (candidate)
                   (plist-get candidate :title))
                 candidates
                 ", ")
    "none"))

(defun delib-flow--match-result-pairs (output)
  "Return project-match result display pairs for OUTPUT."
  `(("Match status" . ,(plist-get output :match-status))
    ("Best project" . ,(or (plist-get (plist-get output :best-project) :title) "none"))
    ("Candidates" . ,(delib-flow--match-candidate-title-list
                      (plist-get output :candidates)))
    ("Reason" . ,(or (plist-get output :reason) "nil"))))

(defun delib-flow--match-result-meaning (status)
  "Return accept-meaning text for project-match STATUS."
  (cond
   ((eq status 'matched)
    "- Accepting this result will make the matched project available to downstream retrieval and filing stages.")
   ((eq status 'ambiguous)
    "- Accepting this result will preserve the ambiguous candidate set and enable manual project selection.")
   ((eq status 'no-match)
    "- Accepting this result will preserve the no-match decision and enable manual selection or proposing a new project.")
   (t
    "- Review the project decision before continuing.")))

(defun delib-flow--match-result-text (run entry)
  "Return readable current-result text for match-project ENTRY in RUN."
  (let* ((output (or (delib-flow--match-candidate-output run)
                     (plist-get entry :raw-output)))
         (status (plist-get output :match-status)))
    (concat
     "*** Summary\n"
     (string-join (delib-flow--match-result-summary-lines entry) "\n")
     "\n\n"
     "*** Result\n"
     (delib-flow--bullet-lines (delib-flow--match-result-pairs output))
     "\n\n*** Accepting this result means\n"
     (delib-flow--match-result-meaning status)
     "\n")))

(defun delib-flow--current-result-loop-update-text (run entry)
  "Return compact local consequence text for current-result ENTRY in RUN."
  (let ((stage-id (plist-get entry :stage-id))
        (status (plist-get entry :status))
        (review-state (plist-get entry :review-state)))
    (cond
     ((eq status 'failed)
      (format "- Local consequence: %s\n- Resume here: %s"
              (delib-flow--compact-summary
               (delib-flow--latest-consequence-text run)
               (+ 16 (delib-flow--mobile-summary-limit)))
              (delib-flow--active-loop-location-text run)))
     ((eq stage-id 'inspect-source)
      (format "- Local consequence: this source classification now controls the next project-matching pass.\n- Resume here: %s"
              (if (eq review-state 'pending-review)
                  "review this result, then accept or retry"
                "run Match Project or inspect again if needed")))
     ((memq stage-id '(match-project manual-project-match))
      (format "- Local consequence: project context for downstream drafting has changed.\n- Resume here: %s"
              (if (eq review-state 'pending-review)
                  "review this project decision, then accept, retry, or choose manually"
                (delib-flow--active-loop-location-text run))))
     ((memq stage-id '(discover-reference-material filter-reference-material))
      (format "- Local consequence: retained context changed for extraction and note suggestion.\n- Resume here: %s"
              (delib-flow--active-loop-location-text run)))
     ((memq stage-id '(extract-actions extract-waiting-for suggest-reference-notes propose-new-project))
      (format "- Local consequence: draft artifacts were refreshed in Filing preview.\n- Resume here: %s"
              (delib-flow--active-loop-location-text run)))
     ((eq stage-id 'file-approved-outputs)
      (format "- Local consequence: target buffers were staged but not saved.\n- Resume here: %s"
              (delib-flow--latest-preview-location-text run)))
     (t
      (format "- Local consequence: %s\n- Resume here: %s"
              (delib-flow--compact-summary
               (delib-flow--latest-consequence-text run)
               (+ 16 (delib-flow--mobile-summary-limit)))
              (delib-flow--active-loop-location-text run))))))

(defun delib-flow--in-flight-current-result-text (run)
  "Return readable current-result text while RUN has an in-flight stage."
  (let* ((stage-id (delib-flow--run-in-flight-stage-id run))
         (provider (delib-flow--run-in-flight-provider run))
         (model (delib-flow--run-in-flight-model run))
         (started-at (delib-flow--run-in-flight-started-at run))
         (elapsed (delib-flow--elapsed-seconds started-at))
         (spinner (delib-flow--spinner-frame-for-time started-at))
         (heartbeat (make-string (1+ (mod elapsed 6)) ?.)))
    (format
     (concat
      "*** Loop update\n"
      "- Local consequence: %s is currently running and the cockpit will refresh when a result arrives.\n"
      "- Resume here: wait for completion, refresh, or abort the run if you need to stop.\n\n"
      "*** Summary\n"
      "- Live status: %s %s\n"
      "- Stage: %s\n"
      "- Status: running\n"
      "- Provider: %s\n"
      "- Model: %s\n"
      "- Started: %s\n"
      "- Elapsed: %ss\n\n"
      "*** Result\n"
      "Waiting for the model response. The cockpit will refresh this section while the request is in flight.\n\n"
      "*** Activity\n"
      "- Heartbeat: %s\n"
      "- Tip: `j` opens the audit, `g` refreshes, and `q` aborts the run.\n")
     (delib-flow--stage-label stage-id)
     spinner
     (if (string-empty-p heartbeat) "." heartbeat)
     (delib-flow--stage-label stage-id)
     provider
     (or model "unknown")
     (delib-flow--time-string started-at)
     elapsed
     heartbeat)))

(defun delib-flow--default-result-text (entry)
  "Return fallback current-result text for ENTRY."
  (format "*** Summary\n%s\n\n*** Result\n%s\n"
          (string-join
           (delib-flow--current-result-summary-lines entry)
           "\n")
          (delib-flow--render-maybe-structured-text
           (plist-get entry :normalized-output))))

(defun delib-flow--fixed-width-preview-text (text)
  "Return TEXT rendered as fixed-width preview lines."
  (mapconcat (lambda (line)
               (format ": %s" line))
             (split-string (string-trim-right (or text "")) "\n")
             "\n"))

(defun delib-flow--reference-note-preview-block (item package)
  "Return preview block for draft reference-note ITEM in PACKAGE."
  (format
   "*** %s\n- Note type: %s\n- Draft body preview: this is the note delib-flow will stage if you approve and file this item.\n%s"
   (delib-flow--reference-note-title item)
   (or (plist-get item :note-type) 'unknown)
   (delib-flow--fixed-width-preview-text
    (delib-flow--reference-note-content item package))))

(defun delib-flow--reference-note-preview-fragment (item package)
  "Return note-preview fragment for reference-note ITEM in PACKAGE."
  (format
   "**** %s\n- Note type: %s\n- Draft body preview: this is the note delib-flow will stage if you approve and file this item.\n%s"
   (delib-flow--reference-note-title item)
   (or (plist-get item :note-type) 'unknown)
   (delib-flow--fixed-width-preview-text
    (delib-flow--reference-note-content item package))))

(defun delib-flow--suggest-reference-notes-result-text (run entry)
  "Return readable current-result text for suggest-reference-notes ENTRY in RUN."
  (let* ((raw-output (plist-get entry :raw-output))
         (items (plist-get raw-output :reference-notes))
         (package (delib-flow--stage-input-package run 'suggest-reference-notes))
         (preview-items (seq-take items 2)))
    (concat
     (delib-flow--default-result-text entry)
     "\n*** Draft note previews\n"
     "Review these draft note bodies before moving into filing. If the shape or focus is off, rerun Suggest Reference Notes now.\n\n"
     (if preview-items
         (mapconcat (lambda (item)
                      (delib-flow--reference-note-preview-block item package))
                    preview-items
                    "\n\n")
       "No reference-note previews are available.\n"))))

(defun delib-flow--draft-selected-reference-note-result-text (run entry)
  "Return readable current-result text for drafted selected-note ENTRY in RUN."
  (let* ((raw-output (plist-get entry :raw-output))
         (item (plist-get raw-output :drafted-item))
         (package (delib-flow--stage-input-package run 'draft-selected-reference-note))
         (content (delib-flow--reference-note-content item package)))
    (concat
     "*** Summary\n"
     (string-join
      (list
       (format "- Selected note: %s"
               (or (delib-flow--reference-note-title item) "Untitled note"))
       (format "- Note type: %s"
               (or (plist-get item :note-type) 'general-pkm))
       (format "- Reason: %s"
               (or (plist-get raw-output :reason) "No reason recorded."))
       "- Result: this is the current drafted body for the selected note only.")
      "\n")
     "\n\n*** Selected note draft\n"
     "- Review this draft before approval or filing.\n"
     "- Regenerating replaces only this selected note draft.\n\n"
     (delib-flow--fixed-width-preview-text content)
     "\n")))

(defun delib-flow--current-result-text (run)
  "Return current-result text for RUN."
    (if (delib-flow--run-in-flight-p run)
        (delib-flow--in-flight-current-result-text run)
      (if-let ((entry (delib-flow--current-result-entry run)))
      (pcase (plist-get entry :stage-id)
        ('inspect-source
         (concat
          "*** Loop update\n"
          (delib-flow--current-result-loop-update-text run entry)
          "\n\n"
          (delib-flow--inspect-result-text run entry)))
        ('match-project
         (concat
          "*** Loop update\n"
          (delib-flow--current-result-loop-update-text run entry)
          "\n\n"
          (delib-flow--match-result-text run entry)))
        ('suggest-reference-notes
         (concat
          "*** Loop update\n"
          (delib-flow--current-result-loop-update-text run entry)
          "\n\n"
          (delib-flow--suggest-reference-notes-result-text run entry)))
        ('draft-selected-reference-note
         (concat
          "*** Loop update\n"
          (delib-flow--current-result-loop-update-text run entry)
          "\n\n"
          (delib-flow--draft-selected-reference-note-result-text run entry)))
        (_
         (concat
          "*** Loop update\n"
          (delib-flow--current-result-loop-update-text run entry)
          "\n\n"
          (delib-flow--default-result-text entry))))
        (concat
         "*** Loop update\n- Local consequence: no stage result is available yet.\n- Resume here: run Inspect Source to create the first reviewable result.\n\n"
         "*** Summary\nNo stage result is available yet.\n"))))

(defun delib-flow--reviewed-cloud-package-status (text)
  "Return display status for reviewed cloud package TEXT."
  (if (string-empty-p text) "not available" "available"))

(defun delib-flow--retrieved-context-status (working)
  "Return retrieval display status from WORKING."
  (if (plist-get working :retrieved-candidates)
      "available"
    "not available"))

(defun delib-flow--retrieved-context-text (working)
  "Return retrieval display text from WORKING."
  (let ((candidates (plist-get working :retrieved-candidates)))
    (if candidates
        (mapconcat (lambda (candidate)
                     (format "- %s (%s): %s"
                             (plist-get candidate :title)
                             (plist-get candidate :score)
                             (mapconcat #'identity
                                        (plist-get candidate :reasons)
                                        ", ")))
                   candidates
                   "\n")
      "No retrieved candidates are available yet.")))

(defun delib-flow--filtered-context-status (working)
  "Return filtered-context display status from WORKING."
  (if (plist-get working :filtered-context)
      "available"
    "not available"))

(defun delib-flow--filtered-context-text (working)
  "Return filtered-context display text from WORKING."
  (let ((filtered (plist-get working :filtered-context)))
    (if filtered
        (format "Retained: %s. Rejected: %s. Retained decisions: %s. Rejected decisions: %s."
                (plist-get filtered :retained-count)
                (plist-get filtered :rejected-count)
                (mapconcat (lambda (candidate)
                             (format "%s=%s"
                                     (plist-get candidate :title)
                                     (delib-flow--filter-reasons-text candidate)))
                           (plist-get filtered :retained-candidates)
                           ", ")
                (if-let ((rejected (plist-get filtered :rejected-candidates)))
                    (mapconcat (lambda (candidate)
                                 (format "%s=%s"
                                         (plist-get candidate :title)
                                         (delib-flow--filter-reasons-text candidate)))
                               rejected
                               ", ")
                  "none"))
      "No filtered context is available yet.")))

(defun delib-flow--cloud-context-status (run)
  "Return cloud-context display status from RUN."
  (let* ((working (delib-flow--run-working-context run))
         (routing (delib-flow--run-routing run)))
    (cond
     ((plist-get working :cloud-sanitized-context) "available")
     ((plist-get routing :cloud-switch-pending) "pending preparation")
     (t "not available"))))

(defun delib-flow--selected-cloud-model (routing)
  "Return selected cloud model from ROUTING."
  (or (plist-get routing :selected-cloud-model)
      "cloud-model-unconfigured"))

(defun delib-flow--cloud-sanitization-status (routing)
  "Return cloud sanitization status from ROUTING."
  (or (plist-get routing :sanitization-status)
      'required))

(defun delib-flow--pending-cloud-context-text (routing)
  "Return pending cloud-context text from ROUTING."
  (format "Cloud pass selected for %s. Model: %s. Provider: %s. Policy: %s. Sanitization status: %s."
          (delib-flow--stage-label
           (delib-flow--cloud-target-stage routing))
          (delib-flow--selected-cloud-model routing)
          (or (plist-get routing :selected-cloud-provider) "unconfigured")
          (or (plist-get routing :cloud-policy-profile)
              delib-flow-cloud-policy-profile)
          (delib-flow--cloud-sanitization-status routing)))

(defun delib-flow--reviewed-cloud-package-text (run)
  "Return editable reviewed cloud-package text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'cloud-package-review)))

(defun delib-flow--cloud-context-text (run)
  "Return cloud-context display text from RUN."
  (let* ((working (delib-flow--run-working-context run))
         (routing (delib-flow--run-routing run))
         (cloud-context (plist-get working :cloud-sanitized-context)))
    (cond
     (cloud-context cloud-context)
     ((plist-get routing :cloud-switch-pending)
     (delib-flow--pending-cloud-context-text routing))
     (t
      "No cloud-sanitized context is available yet."))))

(defun delib-flow--cloud-returned-context-status (working)
  "Return cloud-returned context status from WORKING."
  (if (plist-get working :cloud-returned-context)
      "available"
    "not available"))

(defun delib-flow--cloud-returned-context-text (working)
  "Return cloud-returned context text from WORKING."
  (or (plist-get working :cloud-returned-context)
      "No cloud-returned context is available yet."))

(defun delib-flow--cloud-failure-status (run)
  "Return cloud-failure display status from RUN."
  (if (delib-flow--cloud-failure-active-p run)
      "requires operator review"
    "not recorded"))

(defun delib-flow--cloud-failure-text (run)
  "Return cloud-failure display text from RUN."
  (let* ((routing (delib-flow--run-routing run))
         (stage-id (delib-flow--cloud-failure-stage routing))
         (message (delib-flow--cloud-failure-message routing)))
    (if (and stage-id message)
        (format "%s- Message: %s"
                (if (delib-flow--rerouted-cloud-stage-p stage-id)
                    (format "- Failed cloud target stage: %s\n- Transport stage: %s\n"
                            (delib-flow--stage-label stage-id)
                            (delib-flow--cloud-transport-stage-label))
                  (format "- Failed stage: %s\n"
                          (delib-flow--stage-label stage-id)))
                message)
      "No cloud failure is currently recorded.")))

(defun delib-flow--inspect-context-pairs (source inspect-output)
  "Return curated inspect-context pairs for SOURCE and INSPECT-OUTPUT."
  `(("Source title" . ,(or (plist-get source :title) "Untitled source"))
    ("Source file" . ,(or (plist-get source :file) "No file"))
    ("Proposed source type" . ,(plist-get inspect-output :source-type))
    ("Source type reason" . ,(or (plist-get inspect-output :source-type-reason) "nil"))
    ("Source type signals" . ,(delib-flow--inspect-output-signal-text inspect-output))
    ("Contacts" . ,(if-let ((emails (plist-get inspect-output :contact-emails)))
                       (mapconcat #'identity emails ", ")
                     "none"))
    ("Entities" . ,(delib-flow--inspect-output-entities-text inspect-output))
    ("Summary" . ,(delib-flow--inspect-output-summary-text inspect-output))
    ("Body preview" . ,(or (plist-get inspect-output :body-preview) "No body preview"))))

(defun delib-flow--inspect-context-correction-text (run)
  "Return inspect source-type correction text for RUN."
  (concat
   "*** Source type correction\nEdit the override below before accepting if the proposed source type is wrong.\n\n"
   (delib-flow--render-editable-block run 'inspect-source-review)))

(defun delib-flow--inspect-context-text (run source inspect-output)
  "Return curated inspect-review context text for RUN, SOURCE, and INSPECT-OUTPUT."
  (concat
   "*** Relevant source context\n"
   (delib-flow--bullet-lines
    (delib-flow--inspect-context-pairs source inspect-output))
   "\n\n"
   (or (delib-flow--lines-subsection
        "Follow-up clues"
        (delib-flow--source-followup-clues source inspect-output))
       "")
   (delib-flow--inspect-context-correction-text run)))

(defun delib-flow--match-context-text (source working)
  "Return curated match-review context text for SOURCE and WORKING."
  (let ((project-match (plist-get working :project-match)))
    (concat
     "*** Relevant match context\n"
     (delib-flow--bullet-lines
      `(("Source title" . ,(or (plist-get source :title) "Untitled source"))
        ("Accepted inspect type" . ,(or (plist-get (plist-get working :inspect-output) :source-type)
                                        "unknown"))
        ("Current project decision" . ,(delib-flow--project-match-text project-match))
        ("Reason" . ,(or (plist-get project-match :reason) "nil"))))
     "\n")))

(defun delib-flow--default-context-summary-pairs (run working)
  "Return default context summary pairs for RUN and WORKING."
  `(("Accepted inspect result" . ,(if (delib-flow--accepted-inspect-output run)
                                      "available"
                                    "not available"))
    ("Accepted project decision" . ,(delib-flow--accepted-project-text run))
    ("Retrieved context" . ,(delib-flow--retrieved-context-status working))
    ("Filtered context" . ,(delib-flow--filtered-context-status working))
    ("Cloud-sanitized context" . ,(delib-flow--cloud-context-status run))
    ("Cloud-returned context" . ,(delib-flow--cloud-returned-context-status working))
    ("Cloud failure" . ,(delib-flow--cloud-failure-status run))))

(defun delib-flow--default-context-retrieved-line (working)
  "Return retrieved-context detail line from WORKING, if any."
  (when (plist-get working :retrieved-candidates)
    (format "- Retrieved notes: %s"
            (mapconcat (lambda (candidate)
                         (plist-get candidate :title))
                       (plist-get working :retrieved-candidates)
                       ", "))))

(defun delib-flow--default-context-filtered-line (working)
  "Return filtered-context detail line from WORKING, if any."
  (when (plist-get working :filtered-context)
    (format "- Filtered context summary: %s"
            (delib-flow--filtered-context-text working))))

(defun delib-flow--default-context-cloud-line (run working)
  "Return cloud-context detail line from RUN and WORKING, if any."
  (when (or (plist-get working :cloud-sanitized-context)
            (plist-get (delib-flow--run-routing run) :cloud-switch-pending))
    (format "- Cloud context: %s"
            (delib-flow--cloud-context-text run))))

(defun delib-flow--default-context-cloud-returned-line (working)
  "Return cloud-returned detail line from WORKING, if any."
  (when (plist-get working :cloud-returned-context)
    (format "- Cloud-returned summary: %s"
            (delib-flow--cloud-returned-context-text working))))

(defun delib-flow--default-context-cloud-failure-line (run)
  "Return cloud-failure detail line from RUN, if any."
  (when (delib-flow--cloud-failure-active-p run)
    (format "- Cloud failure: %s"
            (replace-regexp-in-string "\n" "; "
                                      (delib-flow--cloud-failure-text run)))))

(defun delib-flow--retrieved-context-lines (working)
  "Return reviewable retrieved-candidate lines from WORKING."
  (when-let ((candidates (plist-get working :retrieved-candidates)))
    (mapcar (lambda (candidate)
              (format "- %s [%s]: %s"
                      (plist-get candidate :title)
                      (plist-get candidate :score)
                      (mapconcat #'identity
                                 (plist-get candidate :reasons)
                                 ", ")))
            candidates)))

(defun delib-flow--retained-context-lines-for-review (working)
  "Return reviewable retained-candidate lines from WORKING."
  (when-let ((filtered (plist-get working :filtered-context)))
    (let ((candidates (plist-get filtered :retained-candidates)))
      (if candidates
          (mapcar (lambda (candidate)
                    (format "- %s [%s]: %s"
                            (plist-get candidate :title)
                            (plist-get candidate :score)
                            (delib-flow--filter-reasons-text candidate)))
                  candidates)
        '("- none")))))

(defun delib-flow--rejected-context-lines-for-review (working)
  "Return reviewable rejected-candidate lines from WORKING."
  (when-let ((filtered (plist-get working :filtered-context)))
    (let ((candidates (plist-get filtered :rejected-candidates)))
      (if candidates
          (mapcar (lambda (candidate)
                    (format "- %s [%s]: %s"
                            (plist-get candidate :title)
                            (plist-get candidate :score)
                            (delib-flow--filter-reasons-text candidate)))
                  candidates)
        '("- none")))))

(defun delib-flow--retrieval-review-subsections (working)
  "Return retrieval review subsections for WORKING."
  (concat
   (or (delib-flow--lines-subsection
        "Retrieved candidates"
        (delib-flow--retrieved-context-lines working))
       "")
   (or (delib-flow--lines-subsection
        "Retained context"
        (delib-flow--retained-context-lines-for-review working))
       "")
   (or (delib-flow--lines-subsection
        "Rejected context"
        (delib-flow--rejected-context-lines-for-review working))
       "")))

(defun delib-flow--default-context-detail-lines (run working)
  "Return extra curated context lines for RUN and WORKING."
  (delq nil
        (list
         (delib-flow--default-context-retrieved-line working)
         (delib-flow--default-context-filtered-line working)
         (delib-flow--default-context-cloud-line run working)
         (delib-flow--default-context-cloud-returned-line working)
         (delib-flow--default-context-cloud-failure-line run))))

(defun delib-flow--current-context-stage-id (run)
  "Return the stage id currently driving top-level context for RUN."
  (plist-get (delib-flow--current-result-entry run) :stage-id))

(defun delib-flow--inspect-context-active-p (run)
  "Return non-nil when inspect review should drive top-level context for RUN."
  (and (eq (delib-flow--current-context-stage-id run) 'inspect-source)
       (delib-flow--inspect-review-pending-p run)))

(defun delib-flow--match-context-active-p (run)
  "Return non-nil when project-match review should drive top-level context for RUN."
  (and (eq (delib-flow--current-context-stage-id run) 'match-project)
       (delib-flow--match-review-pending-p run)))

(defun delib-flow--cloud-routing-review-active-p (run)
  "Return non-nil when RUN should surface cloud routing review."
  (or (plist-get (delib-flow--run-routing run) :cloud-switch-pending)
      (plist-get (delib-flow--run-routing run) :cloud-target-stage)
      (delib-flow--stage-executed-p run 'decide-cloud-pass)))

(defun delib-flow--default-context-text (run working)
  "Return default curated context text for RUN and WORKING."
  (concat
   "*** Working context summary\n"
   (delib-flow--bullet-lines
    (delib-flow--default-context-summary-pairs run working))
   "\n\n"
   (delib-flow--retrieval-review-subsections working)
   (or (delib-flow--lines-subsection
        "Current routing and retrieval cues"
        (delib-flow--default-context-detail-lines run working))
       "")
   "*** Editable working slice\n"
   (delib-flow--render-editable-block run 'context-main)))

(defun delib-flow--render-working-context-section (run)
  "Return Org text for the Current context section."
  (let* ((working (delib-flow--run-working-context run))
         (source (delib-flow--run-source run))
         (inspect-output (or (delib-flow--inspect-candidate-output run)
                             (plist-get working :inspect-output))))
    (cond
     ((delib-flow--inspect-context-active-p run)
      (delib-flow--inspect-context-text run source inspect-output))
     ((delib-flow--match-context-active-p run)
      (delib-flow--match-context-text source working))
     (t
      (delib-flow--default-context-text run working)))))

(defun delib-flow--render-stage-entry (entry)
  "Return Org text for stage-history ENTRY."
  (format
   "**** %s\n- Status: %s\n- Review state: %s\n- Provider: %s\n\n%s\n"
   (plist-get entry :label)
   (plist-get entry :status)
   (plist-get entry :review-state)
   (delib-flow--audit-provider entry)
   (delib-flow--render-maybe-structured-text
    (plist-get entry :normalized-output))))

(defun delib-flow--stage-history-groups (entries)
  "Return ENTRIES grouped by stage id, preserving first-seen order."
  (let (groups)
    (dolist (entry entries (nreverse groups))
      (let* ((stage-id (plist-get entry :stage-id))
             (group (assoc stage-id groups)))
        (if group
            (setcdr group (append (cdr group) (list entry)))
          (push (cons stage-id (list entry)) groups))))))

(defun delib-flow--render-stage-attempt (entry attempt-number)
  "Return Org text for stage-history ENTRY ATTEMPT-NUMBER."
  (format
   "***** Attempt %d\n- Status: %s\n- Review state: %s\n- Provider: %s%s\n\n%s\n"
   attempt-number
   (plist-get entry :status)
   (plist-get entry :review-state)
   (delib-flow--audit-provider entry)
   (if (delib-flow--cloud-shadow-entry-p entry)
       (format "\n- Applied locally: %s"
               (if (plist-get entry :applied-p) "yes" "no"))
     "")
   (delib-flow--render-maybe-structured-text
    (plist-get entry :normalized-output))))

(defun delib-flow--render-stage-history-group (group)
  "Return Org text for stage-history GROUP."
  (let ((label (delib-flow--stage-label (car group)))
        (entries (cdr group))
        (attempt-number 0))
    (concat
     (format "**** %s\n- Attempts: %d\n\n" label (length entries))
     (mapconcat
      (lambda (entry)
        (setq attempt-number (1+ attempt-number))
        (delib-flow--render-stage-attempt entry attempt-number))
      entries
      "\n"))))

(defun delib-flow--render-stage-history-section (run)
  "Return Org text for the stage-history details in RUN."
  (let* ((history (delib-flow--run-stage-history run))
         (entries (plist-get history :entries)))
    (if entries
        (concat
         (format "*** Stage history\n- Latest stage: %s\n- Latest status: %s\n\n"
                 (delib-flow--stage-label (plist-get history :latest-stage))
                 (plist-get history :latest-status))
         (mapconcat #'delib-flow--render-stage-history-group
                    (delib-flow--stage-history-groups entries)
                    "\n"))
      "*** Stage history\nNo stages have been executed yet.\n")))

(defun delib-flow--now-status-extra-lines (latest-entry current-entry)
  "Return extra Now status lines from LATEST-ENTRY and CURRENT-ENTRY."
  (append
   (when latest-entry
     (list (format "- Latest stage: %s (%s)"
                   (plist-get latest-entry :label)
                   (plist-get latest-entry :status))))
   (when (and current-entry
              latest-entry
              (not (eq current-entry latest-entry)))
     (list (format "- Operator focus stage: %s (%s)"
                   (plist-get current-entry :label)
                   (plist-get current-entry :status))))))

(defun delib-flow--render-now-optional-section (run active-p heading block-id)
  "Return optional Now subsection from RUN when ACTIVE-P is non-nil."
  (if active-p
      (format "\n\n*** %s\n%s"
              heading
              (delib-flow--render-editable-block run block-id))
    ""))

(defun delib-flow--recommended-action (run)
  "Return the best currently available operator action for RUN."
  (let* ((available-actions
          (seq-filter
           (lambda (action)
             (and (eq (plist-get action :status) 'available)
                  (not (memq (plist-get action :id)
                             '(refresh-buffer abort-run)))))
           (delib-flow--sorted-actions run)))
         (preferred-actions
          (seq-remove
           (lambda (action)
             (let ((id (plist-get action :id)))
               (or (and (eq id 'inspect-source)
                        (delib-flow--stage-executed-p run 'inspect-source)
                        (seq-some
                         (lambda (other)
                           (not (eq (plist-get other :id) 'inspect-source)))
                         available-actions))
                   (and (eq id 'match-project)
                        (delib-flow--stage-executed-p run 'match-project)
                        (seq-some
                         (lambda (other)
                           (not (eq (plist-get other :id) 'match-project)))
                         available-actions)))))
           available-actions)))
    (or (car preferred-actions)
        (car available-actions))))

(defun delib-flow--recommended-action-text (run)
  "Return the current recommendation text for RUN."
  (if-let ((action (delib-flow--recommended-action run)))
      (format "%s%s"
              (plist-get action :label)
              (if-let ((reason (plist-get action :reason)))
                  (format " (%s)" reason)
                ""))
    "No workflow action is currently available beyond refresh or abort."))

(defun delib-flow--mobile-summary-limit ()
  "Return the preferred single-line summary width for the current display."
  (if (display-graphic-p) 88 56))

(defun delib-flow--single-line-text (value)
  "Return VALUE normalized for compact one-line display."
  (replace-regexp-in-string
   "[ \t\n]+"
   " "
   (string-trim (or value ""))))

(defun delib-flow--compact-summary (value &optional width)
  "Return VALUE compressed to one bounded line."
  (truncate-string-to-width
   (delib-flow--single-line-text value)
   (or width (delib-flow--mobile-summary-limit))
   nil nil
   "…"))

(defun delib-flow--recommended-alternative-actions (run)
  "Return nearby alternative actions for RUN after the main recommendation."
  (let ((recommended (delib-flow--recommended-action run)))
    (seq-take
     (seq-remove
      (lambda (action)
        (or (eq action recommended)
            (not (eq (plist-get action :status) 'available))
            (memq (plist-get action :id) '(refresh-buffer abort-run))))
      (delib-flow--sorted-actions run))
     2)))

(defun delib-flow--recommended-action-why-text (run)
  "Return the rationale for the current recommendation in RUN."
  (if-let ((action (delib-flow--recommended-action run)))
      (or (plist-get action :reason)
          "This is the highest-priority currently legal workflow pass.")
    "The run currently has no workflow pass available beyond refresh or abort."))

(defun delib-flow--recommended-alternatives-text (run)
  "Return compact alternative-action text for RUN."
  (if-let ((actions (delib-flow--recommended-alternative-actions run)))
      (mapconcat
       (lambda (action)
         (plist-get action :label))
       actions
       ", ")
    "None nearby."))

(defun delib-flow--current-blocked-action (run)
  "Return the highest-priority blocked action for RUN, if any."
  (seq-find
   (lambda (entry)
     (eq (plist-get entry :status) 'blocked))
   (delib-flow--sorted-actions run)))

(defun delib-flow--filing-unblock-guidance (run)
  "Return compact filing-specific unblock guidance for RUN, or nil."
  (let* ((filing (plist-get run :filing))
         (warnings (plist-get filing :selection-blocking-warnings))
         (ready-indexes
          (delib-flow--draft-item-selection-indexes
           (plist-get filing :draft-items)
           #'delib-flow--draft-item-ready-p)))
    (cond
     (warnings
      (format "Choose a ready selection (%s), reject the blocked artifact, or fix: %s"
              (delib-flow--selection-index-list ready-indexes)
              (delib-flow--compact-summary
               (plist-get (car warnings) :message)
               (+ 10 (delib-flow--mobile-summary-limit)))))
     ((plist-get filing :conflicts)
      "Run Resolve Filing Conflict, then retry filing the approved artifact.")
     (t
      nil))))

(defun delib-flow--unblock-guidance-text (run)
  "Return the most relevant compact unblock path text for RUN."
  (or (delib-flow--filing-unblock-guidance run)
      (if-let ((action (delib-flow--current-blocked-action run)))
          (or (plist-get action :reason)
              (format "Unblock %s before continuing."
                      (plist-get action :label)))
        "No unblock action is currently needed.")))

(defun delib-flow--recommended-next-pass-text (run)
  "Return rendered recommendation text for RUN."
  (if-let ((action (delib-flow--recommended-action run)))
      (format "- Recommended pass: %s\n- Why: %s\n- Unblock path: %s\n- Nearby alternatives: %s"
              (plist-get action :label)
              (delib-flow--compact-summary
               (delib-flow--recommended-action-why-text run)
               (+ 12 (delib-flow--mobile-summary-limit)))
              (delib-flow--compact-summary
               (delib-flow--unblock-guidance-text run)
               (+ 12 (delib-flow--mobile-summary-limit)))
              (delib-flow--recommended-alternatives-text run))
    (format "- Recommended pass: none\n- Why: %s\n- Unblock path: %s\n- Nearby alternatives: none"
            (delib-flow--recommended-action-why-text run)
            (delib-flow--compact-summary
             (delib-flow--unblock-guidance-text run)
             (+ 12 (delib-flow--mobile-summary-limit))))))

(defun delib-flow--active-loop-heading (run)
  "Return the most relevant active-loop heading for RUN."
  (cond
   ((delib-flow--run-in-flight-p run)
    "Current result")
   ((or (delib-flow--inspect-review-pending-p run)
        (delib-flow--match-review-pending-p run))
    "Current result")
   ((and (delib-flow--filing-preview-visible-p run)
         (or (plist-get (plist-get run :filing) :approved-items)
             (delib-flow--preview-selected-filing-items run)))
    "Filing actions")
   ((delib-flow--filing-selection-active-p run)
    "Artifact selection")
   ((delib-flow--filing-conflict-resolution-active-p run)
    "Conflict resolution")
   ((delib-flow--manual-project-selection-active-p run)
    "Manual project selection")
   ((delib-flow--filing-preview-visible-p run)
    "Filing preview")
   (t
    "Next actions")))

(defun delib-flow--active-loop-location-text (run)
  "Return compact active-loop location text for RUN."
  (pcase (delib-flow--active-loop-heading run)
    ("Current result" "Current result")
    ("Next actions" "Next actions")
    ("Manual project selection" "Current context > Manual project selection")
    ("Filing preview" "Filing preview")
    ("Artifact selection" "Filing preview > Artifact selection")
    ("Conflict resolution" "Filing preview > Conflict resolution")
    ("Filing actions" "Filing preview > Filing actions")
    (heading heading)))

(defun delib-flow--latest-meaningful-change-text (run)
  "Return compact latest-change text for RUN."
  (if-let ((entry (delib-flow--current-result-entry run)))
      (format "%s (%s%s)"
              (plist-get entry :label)
              (plist-get entry :status)
              (if-let ((review-state (plist-get entry :review-state)))
                  (format ", %s" review-state)
                ""))
    "No stage result is available yet."))

(defun delib-flow--latest-consequence-text (run)
  "Return compact last-consequence text for RUN."
  (let* ((entry (delib-flow--latest-stage-entry run))
         (stage-id (plist-get entry :stage-id))
         (status (plist-get entry :status))
         (review-state (plist-get entry :review-state))
         (filing (plist-get run :filing)))
    (cond
     ((null entry)
      "No workflow consequence exists yet; start with Inspect Source.")
     ((eq status 'failed)
      (or (plist-get (delib-flow--run-session run) :current-decision)
          "The latest stage failed and needs operator recovery."))
     ((eq stage-id 'file-approved-outputs)
      (if (plist-get filing :target-locations)
          "Filed targets were staged into buffers and are still unsaved."
        "Filing completed without staged target buffers."))
     ((eq stage-id 'select-approved-filing-actions)
      "The approved filing artifact and deterministic targets were updated.")
     ((memq stage-id '(extract-actions extract-waiting-for suggest-reference-notes propose-new-project))
      "Draft artifacts were refreshed in Filing preview.")
     ((memq stage-id '(match-project manual-project-match))
      "Project context changed for downstream drafting and filing.")
     ((memq stage-id '(discover-reference-material filter-reference-material))
      "Relevant context changed for downstream extraction and note suggestions.")
     ((eq review-state 'pending-review)
      "The latest stage changed operator focus to review.")
     (t
      (or (plist-get (delib-flow--run-session run) :current-decision)
          "The latest stage completed and the cockpit is ready for the next pass.")))))

(defun delib-flow--history-status-text (run)
  "Return compact latest-history status text for RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (format "%s (%s)"
              (plist-get entry :label)
              (plist-get entry :status))
    "No stage history yet."))

(defun delib-flow--latest-preview-heading (run)
  "Return the most relevant preview heading for RUN."
  (cond
   ((plist-get (plist-get run :filing) :target-locations)
    "Opened staged targets")
   ((delib-flow--staged-content-preview-text-available-p run)
    "Staged content preview")
   ((delib-flow--planned-file-locations run)
    "Planned file targets")
   ((delib-flow--current-result-entry run)
    "Current result")
   (t
    "Decision strip")))

(defun delib-flow--latest-preview-location-text (run)
  "Return compact latest-preview location text for RUN."
  (pcase (delib-flow--latest-preview-heading run)
    ("Opened staged targets" "Filing preview > Opened staged targets")
    ("Staged content preview" "Filing preview > Staged content preview")
    ("Planned file targets" "Filing preview > Planned file targets")
    ("Current result" "Current result")
    ("Decision strip" "Now > Decision strip")
    (heading heading)))

(defun delib-flow--resume-guide-text (run)
  "Return compact resume/orientation text for RUN."
  (format "- Latest change: %s\n- Active loop: %s\n- Latest preview: %s\n- Jump back: `L` active loop, `K` latest preview"
          (delib-flow--compact-summary
           (delib-flow--latest-meaningful-change-text run)
           (+ 16 (delib-flow--mobile-summary-limit)))
          (delib-flow--active-loop-location-text run)
          (delib-flow--latest-preview-location-text run)))

(defun delib-flow--recovery-snapshot-text (run)
  "Return compact recovery snapshot text for RUN."
  (format "- Current decision: %s\n- Last consequence: %s\n- History status: %s\n- Recovery path: `L` resume loop, `K` consequence preview, `U` stage history, `J` audit stage"
          (delib-flow--compact-summary
           (or (plist-get (delib-flow--run-session run) :current-decision)
               "No current decision is recorded yet.")
           (+ 16 (delib-flow--mobile-summary-limit)))
          (delib-flow--compact-summary
           (delib-flow--latest-consequence-text run)
           (+ 16 (delib-flow--mobile-summary-limit)))
          (delib-flow--history-status-text run)))

(defun delib-flow--current-blockage-text (run)
  "Return the most relevant blocked-state text for RUN."
  (if-let ((action
            (seq-find
             (lambda (entry)
               (eq (plist-get entry :status) 'blocked))
             (delib-flow--sorted-actions run))))
      (format "%s%s"
              (plist-get action :label)
              (if-let ((reason (plist-get action :reason)))
                  (format " (%s)" reason)
                ""))
    "None"))

(defun delib-flow--active-project-text (run)
  "Return active project summary text for RUN."
  (or (delib-flow--matched-project-title run)
      "No project is currently selected."))

(defun delib-flow--active-filing-item (run)
  "Return the currently active filing item for RUN, if any."
  (or (car (plist-get (plist-get run :filing) :approved-items))
      (car (delib-flow--preview-selected-filing-items run))))

(defun delib-flow--active-filing-item-text (run)
  "Return active filing item summary text for RUN."
  (if-let ((item (delib-flow--active-filing-item run)))
      (format "%s %s"
              (delib-flow--draft-item-keyword item)
              (plist-get item :text))
    "No filing artifact is currently selected."))

(defun delib-flow--planned-target-summary-text (run)
  "Return compact planned-target summary text for RUN."
  (if-let ((locations (delib-flow--planned-file-locations run)))
      (let* ((targets (mapcar (lambda (location)
                                (plist-get location :target))
                              locations))
             (first-target (car targets))
             (remaining (length (cdr targets))))
        (if (> remaining 0)
            (format "%s (+%s more)"
                    (delib-flow--compact-summary first-target)
                    remaining)
          (delib-flow--compact-summary first-target)))
    "No planned file targets are available yet."))

(defun delib-flow--decision-strip-text (run)
  "Return compact decision-strip text for RUN."
  (let ((current-entry (delib-flow--current-result-entry run)))
    (format
     "- Run status: %s\n- Current stage: %s\n- Blocked: %s\n- Recommended next step: %s\n- Active project: %s\n- Active filing item: %s\n- Planned target preview: %s"
     (plist-get (delib-flow--run-session run) :status)
     (if current-entry
         (format "%s (%s)"
                 (plist-get current-entry :label)
                 (plist-get current-entry :status))
       "No stage has produced a result yet.")
     (delib-flow--compact-summary (delib-flow--current-blockage-text run))
     (delib-flow--compact-summary (delib-flow--recommended-action-text run))
     (delib-flow--compact-summary (delib-flow--active-project-text run))
     (delib-flow--compact-summary (delib-flow--active-filing-item-text run))
     (delib-flow--planned-target-summary-text run))))

(defun delib-flow--quick-actions (run)
  "Return the highest-priority local work actions for RUN."
  (seq-take
   (seq-filter
    (lambda (action)
      (and (eq (plist-get action :status) 'available)
           (not (memq (plist-get action :id) '(refresh-buffer abort-run)))))
    (delib-flow--sorted-actions run))
   3))

(defun delib-flow--quick-actions-text (run)
  "Return rendered quick actions for RUN."
  (if-let ((actions (delib-flow--quick-actions run)))
      (delib-flow--render-action-lines actions)
    "No quick workflow actions are currently available."))

(defun delib-flow--control-keys-text ()
  "Return rendered control-key help text for the Now section."
  (mapconcat
   (lambda (entry)
     (format "- `%s`: %s" (car entry) (cdr entry)))
   delib-flow--control-key-help-alist
   "\n"))

(defun delib-flow--menu-entry (label summary command)
  "Return a context-menu entry with LABEL, SUMMARY, and COMMAND."
  (list :label label :summary summary :command command))

(defun delib-flow--context-menu-action-entry (action)
  "Return a local-menu entry for ACTION."
  (delib-flow--menu-entry
   (plist-get action :label)
   (or (plist-get action :reason)
       "Run this workflow action.")
   (plist-get action :handler)))

(defun delib-flow--context-menu-builtins (entries)
  "Return ENTRIES plus standard local control actions."
  (seq-uniq
   (append
    entries
    (list
     (delib-flow--menu-entry
      "Jump to Active Loop"
      "Move point to the most relevant current decision loop."
      #'delib-flow-jump-active-loop)
     (delib-flow--menu-entry
      "Jump to Latest Preview"
      "Move point to the latest consequence preview or current result."
      #'delib-flow-jump-latest-preview)
     (delib-flow--menu-entry
      "Jump to Stage History"
      "Move point to the latest stage history details."
      #'delib-flow-jump-stage-history)
     (delib-flow--menu-entry
      (if delib-flow-control-focus-mode
          "Disable Focus Mode"
        "Enable Focus Mode")
      "Keep only the active decision loop visible on a narrow screen."
      #'delib-flow-toggle-focus-mode)
     (delib-flow--menu-entry
      "Refresh Buffer"
      "Rerender the cockpit from the current run state."
      #'delib-flow-refresh-buffer)
     (delib-flow--menu-entry
      "Control Hints"
      "Show global keys, local actions, recommendation, and blocked reasons."
      #'delib-flow-control-help)))
   (lambda (left right)
     (and (equal (plist-get left :label)
                 (plist-get right :label))
          (eq (plist-get left :command)
              (plist-get right :command))))))

(defun delib-flow--context-menu-entries (run section)
  "Return local context-menu entries for RUN in SECTION."
  (pcase section
    ("Now"
     (delib-flow--context-menu-builtins
      (append
       (if-let ((action (delib-flow--recommended-action run)))
           (list (delib-flow--context-menu-action-entry action))
         nil)
       (mapcar #'delib-flow--context-menu-action-entry
               (delib-flow--quick-actions run)))))
    ("Current result"
     (delib-flow--context-menu-builtins
      (append
       (if (delib-flow--current-reviewable-stage)
           (list
            (delib-flow--menu-entry
             "Approve Current Review"
             "Accept the pending inspect or project-match decision."
             #'delib-flow-approve-current)
            (delib-flow--menu-entry
             "Retry Current Review"
             "Rerun the current pending inspect or project-match stage."
             #'delib-flow-retry-current))
         nil)
       (list
        (delib-flow--menu-entry
         "Open Debug Comparison"
         "Compare the latest stage with a fresh replay or previous attempt."
         #'delib-flow-debug-open-comparison)
        (delib-flow--menu-entry
         "Open Latest Stage Inspection"
         "Inspect package, normalized output, and raw stage output."
         #'delib-flow-debug-open-latest-stage-inspection)))))
    ("Filing preview"
     (delib-flow--context-menu-builtins
      (append
       (mapcar #'delib-flow--context-menu-action-entry
               (delib-flow--filing-preview-actions run))
       (list
        (delib-flow--menu-entry
         "Choose Filing Artifact"
         "Pick one valid filing artifact with minibuffer completion."
         #'delib-flow-choose-filing-selection)
        (delib-flow--menu-entry
         "Peek Filing Target"
         "Open the first filing target in a sibling preview window."
         #'delib-flow-peek-filing-target)
        (delib-flow--menu-entry
         "Peek Staged Content"
         "Open the exact staged filing content in a preview buffer."
         #'delib-flow-peek-staged-content))
       (if (delib-flow--reference-note-capture-visible-p run)
           (list
            (delib-flow--menu-entry
             "Choose Reference Note Template"
             "Pick the org-roam template for the current note artifact."
             #'delib-flow-choose-reference-note-template))
         nil))))
    ("Next actions"
     (delib-flow--context-menu-builtins
      (mapcar #'delib-flow--context-menu-action-entry
              (seq-filter
               (lambda (action)
                 (eq (plist-get action :status) 'available))
               (delib-flow--sorted-actions run)))))
    ("Current context"
     (delib-flow--context-menu-builtins
      (delq nil
            (list
             (and (delib-flow--manual-project-selection-active-p run)
                  (delib-flow--menu-entry
                   "Choose Project Manually"
                   "Select a project from the current manual shortlist."
                   #'delib-flow-choose-manual-project))
             (delib-flow--menu-entry
              "Open Audit Run"
              "Jump to the active run subtree in the audit log."
              #'delib-flow-open-audit-run)))))
    ("Details"
     (delib-flow--context-menu-builtins
      (list
       (delib-flow--menu-entry
        "Open Audit Run"
        "Jump to the active run subtree in the audit log."
        #'delib-flow-open-audit-run)
       (delib-flow--menu-entry
        "Open Latest Audit Stage"
        "Jump to the latest stage subtree in the audit log."
        #'delib-flow-open-audit-latest-stage)
       (delib-flow--menu-entry
        "Open Debug Walkthrough"
        "Show validation recipes and helper checkpoints."
        #'delib-flow-debug-open-walkthrough))))
    (_
     (delib-flow--context-menu-builtins nil))))

(defun delib-flow--context-menu-choice-label (entry)
  "Return the user-facing label string for local-menu ENTRY."
  (format "%s - %s"
          (plist-get entry :label)
          (plist-get entry :summary)))

(defun delib-flow--local-actions-summary-text (run section)
  "Return local action summary text for RUN in SECTION."
  (let ((entries (delib-flow--context-menu-entries run section)))
    (if entries
        (mapconcat
         (lambda (entry)
           (format "- %s: %s"
                   (plist-get entry :label)
                   (plist-get entry :summary)))
         entries
         "\n")
      "- No local actions are available here.")))

(defun delib-flow--render-now-section (run)
  "Return Org text for the Now section from RUN."
  (let* ((latest-entry (delib-flow--latest-stage-entry run))
         (current-entry (delib-flow--current-result-entry run)))
    (concat
     (format "*** Decision strip\n%s%s"
             (delib-flow--decision-strip-text run)
             (if-let ((extra-lines
                       (delib-flow--now-status-extra-lines
                        latest-entry current-entry)))
                 (concat "\n" (string-join extra-lines "\n"))
               ""))
     (format "\n\n*** Recommended next pass\n%s"
             (delib-flow--recommended-next-pass-text run))
     (format "\n\n*** Quick actions\n%s"
             (delib-flow--quick-actions-text run))
     (format "\n\n*** Recovery snapshot\n%s"
             (delib-flow--recovery-snapshot-text run))
     (format "\n\n*** Resume guide\n%s"
             (delib-flow--resume-guide-text run))
     (format "\n\n*** Operator notes\n%s"
             (delib-flow--render-editable-block run 'operator-notes))
     (format "\n\n*** Control keys\n%s"
             (delib-flow--control-keys-text))
     (delib-flow--render-now-optional-section
      run
      (delib-flow--cloud-routing-review-active-p run)
      "Cloud routing review"
      'cloud-routing-review)
     (if (delib-flow--manual-project-selection-active-p run)
         (concat "\n\n"
                 (delib-flow--manual-project-selection-section-text run))
       "")
     (delib-flow--render-now-optional-section
      run
     (delib-flow--cloud-failure-active-p run)
      "Cloud failure review"
      'cloud-failure-review))))

(defun delib-flow--current-result-filing-snapshot-text (run)
  "Return compact action-relevant filing snapshot from the current result for RUN."
  (if-let ((entry (delib-flow--current-result-entry run)))
      (format "- Stage: %s\n- Status: %s\n- Review state: %s"
              (plist-get entry :label)
              (plist-get entry :status)
              (plist-get entry :review-state))
    "No stage result is available yet."))

(defun delib-flow--render-current-result-section (run)
  "Return Org text for the Current result section from RUN."
  (delib-flow--current-result-text run))

(defun delib-flow--render-details-section (run)
  "Return Org text for the Details section from RUN."
  (let ((working (delib-flow--run-working-context run))
        (inspect-review (delib-flow--review-record (delib-flow--run-working-context run)
                                                   'inspect-source))
        (match-review (delib-flow--review-record (delib-flow--run-working-context run)
                                                 'match-project)))
    (concat
     (delib-flow--render-source-section run)
     "\n"
     (format "*** Accepted working context\n%s\n- Inspect review state: %s\n- Match review state: %s\n- Retrieved candidate count: %s\n- Filtered context: %s\n- Cloud target stage: %s\n- Cloud-sanitized context: %s\n- Cloud failure: %s\n\n**** Cloud routing review\n%s\n\n**** Reviewed cloud package\n%s\n%s"
             (delib-flow--accepted-working-context-text run)
             (delib-flow--review-record-status inspect-review)
             (delib-flow--review-record-status match-review)
             (length (or (plist-get working :retrieved-candidates) nil))
             (delib-flow--filtered-context-status working)
             (delib-flow--stage-label
              (delib-flow--cloud-target-stage
               (delib-flow--run-routing run)))
             (delib-flow--cloud-context-status run)
             (delib-flow--cloud-failure-status run)
             (delib-flow--render-editable-block run 'cloud-routing-review)
             (delib-flow--render-editable-block run 'cloud-package-review)
             (if (delib-flow--cloud-failure-active-p run)
                 (format "\n**** Cloud failure review\n%s\n"
                         (delib-flow--render-editable-block
                          run 'cloud-failure-review))
               ""))
     (delib-flow--latest-stage-input-text run)
     "\n"
     (delib-flow--latest-stage-raw-output-text run)
     "\n"
     (delib-flow--render-stage-history-section run)
     "\n"
     (format "*** Audit status\n%s\n\n%s\n\n**** Audit navigation\n%s\n"
             (delib-flow--audit-run-state-text run)
             (delib-flow--audit-stage-readiness-text run)
             (delib-flow--audit-navigation-text run)))))

(defun delib-flow--render-valid-next-actions-section (_run)
  "Return Org text for the Valid next actions section."
  (mapconcat #'delib-flow--format-action-line
             (delib-flow--sorted-actions _run)
             ""))

(defun delib-flow--render-action-lines (actions)
  "Return rendered control-buffer lines for ACTIONS."
  (mapconcat #'delib-flow--format-action-line actions ""))

(defun delib-flow--filing-selection-active-p (run)
  "Return non-nil when RUN should surface filing selection review."
  (delib-flow--draft-items-ready-p run))

(defun delib-flow--filing-conflict-resolution-active-p (run)
  "Return non-nil when RUN should surface conflict resolution review."
  (plist-get (plist-get run :filing) :conflicts))

(defun delib-flow--filing-selection-block-text (run)
  "Return editable filing-selection text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'filing-selection-review)))

(defun delib-flow--filing-conflict-resolution-block-text (run)
  "Return editable filing-conflict-resolution text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'filing-conflict-resolution)))

(defun delib-flow--draft-item-preview-line (item)
  "Return preview line for draft ITEM."
  (format "- %s %s"
          (delib-flow--draft-item-keyword item)
          (plist-get item :text)))

(defun delib-flow--draft-item-warning-lines (item)
  "Return indented warning lines for draft ITEM."
  (mapcar
   (lambda (warning)
     (format "  %s: %s"
             (if (delib-flow--blocking-artifact-warning-p warning)
                 "Blocking"
               "Warning")
             (plist-get warning :message)))
   (delib-flow--draft-item-warnings item)))

(defun delib-flow--draft-item-tag-suggestion-line (item)
  "Return tag-suggestion line for draft ITEM, or nil when absent."
  (when-let ((tags (plist-get item :tag-suggestions)))
    (when tags
      (format "  Suggested tags: %s" (string-join tags ", ")))))

(defun delib-flow--draft-item-preview-lines (item)
  "Return preview lines for draft ITEM including warnings."
  (delq nil
        (append
         (list (delib-flow--draft-item-preview-line item)
               (format "  Status: %s"
                       (delib-flow--draft-item-readiness-text item))
               (delib-flow--draft-item-tag-suggestion-line item))
         (delib-flow--draft-item-warning-lines item)
         (delib-flow--draft-item-remediation-lines item))))

(defun delib-flow--draft-item-keyword (item)
  "Return Org keyword prefix for draft ITEM."
  (cond
   ((eq (plist-get item :kind) 'project) "PROJECT")
   ((eq (plist-get item :kind) 'waiting-for) "WAITING")
   ((eq (plist-get item :kind) 'reference-note) "NOTE")
   (t "TODO")))

(defun delib-flow--draft-item-preview-text (items)
  "Return preview text for draft action ITEMS."
  (mapconcat (lambda (item)
               (string-join (delib-flow--draft-item-preview-lines item) "\n"))
             items
             "\n"))

(defun delib-flow--draft-item-warning-summary (items)
  "Return warning summary line for draft ITEMS."
  (let ((warning-count (delib-flow--item-warning-total items))
        (warning-item-count (delib-flow--items-with-warnings-count items))
        (blocking-count (delib-flow--item-blocking-warning-total items))
        (blocking-item-count (delib-flow--items-with-blocking-warnings-count items)))
    (format "- Quality warnings: %s across %s artifact(s).\n- Blocking warnings: %s across %s artifact(s)."
            warning-count
            warning-item-count
            blocking-count
            blocking-item-count)))

(defun delib-flow--draft-item-status (run)
  "Return filing preview status text for RUN."
  (let* ((filing (plist-get run :filing))
         (draft-items (plist-get filing :draft-items))
         (ready-indexes
          (delib-flow--draft-item-selection-indexes
           draft-items
           #'delib-flow--draft-item-ready-p))
         (blocked-indexes
          (delib-flow--draft-item-selection-indexes
           draft-items
           (lambda (item)
             (not (delib-flow--draft-item-ready-p item))))))
    (cond
     ((plist-get filing :selection-blocking-warnings)
      (format "You have filing candidates, but the last approval attempt is blocked. Choose a ready queue item (%s), or reject the blocked item (%s)."
              (delib-flow--selection-index-list ready-indexes)
              (delib-flow--selection-index-list blocked-indexes)))
     ((and draft-items
           (plist-get filing :approved-items))
      (format "One artifact is approved and ready to file. More candidates remain in the queue. Review the planned targets, file the approved artifact, or approve another ready item (%s)."
              (delib-flow--selection-index-list ready-indexes)))
     (draft-items
      (format "A filing queue is ready. Choose one ready item (%s), approve it, then review where it will go before filing."
              (delib-flow--selection-index-list ready-indexes)))
     (t
      "No filing queue is available yet."))))

(defun delib-flow--draft-item-text (run)
  "Return filing preview body text for RUN."
  (let ((items (plist-get (plist-get run :filing) :draft-items)))
    (if items
        (concat
         (delib-flow--draft-item-warning-summary items)
         "\n"
         (delib-flow--draft-item-preview-text items))
      "No draft artifacts are available yet.")))

(defun delib-flow--approved-item-status (run)
  "Return approved-artifact status text for RUN."
  (if (plist-get (plist-get run :filing) :approved-items)
      "Approved filing artifacts are available."
    "No approved artifacts are available yet."))

(defun delib-flow--approved-item-text (run)
  "Return approved-artifact body text for RUN."
  (let ((items (plist-get (plist-get run :filing) :approved-items)))
    (if items
        (delib-flow--draft-item-preview-text items)
      "No approved artifacts are available yet.")))

(defun delib-flow--current-filing-choice-status (run)
  "Return active filing-choice status text for RUN."
  (cond
   ((plist-get (plist-get run :filing) :approved-items)
    "This is the artifact that File Approved Outputs will stage next.")
   ((delib-flow--preview-selected-filing-items run)
    "This is the queue item your current Selection points to.")
   ((delib-flow--non-empty-string-p
     (delib-flow--filing-selection-value run))
    "The current selection does not point to one ready queue item yet.")
   (t
    "No filing artifact is currently selected.")))

(defun delib-flow--current-filing-choice-text (run)
  "Return active filing-choice detail text for RUN."
  (if-let* ((items (delib-flow--planned-file-preview-items run))
            (single-item (and (= (length items) 1) (car items))))
      (if (eq (plist-get single-item :kind) 'reference-note)
          (concat
           (delib-flow--draft-item-preview-text items)
           "\n\n"
           (delib-flow--reference-note-preview-fragment single-item run))
        (delib-flow--draft-item-preview-text items))
    "Choose one ready queue item to preview it here."))

(defun delib-flow--staged-content-preview-status (run)
  "Return staged-content-preview status text for RUN."
  (cond
   ((plist-get (plist-get run :filing) :approved-items)
    "These edits are staged in target buffers only. Review them here or in the opened target buffer before saving anything.")
   ((delib-flow--preview-selected-filing-items run)
    "Preview only: this is the exact text delib-flow will stage if you approve this item and then file it.")
   (t
    "No staged content preview is available yet.")))

(defun delib-flow--reference-note-capture-visible-p (run)
  "Return non-nil when reference-note capture review should be shown for RUN."
  (not (null (delib-flow--reference-note-preview-item run))))

(defun delib-flow--reference-note-capture-status (run)
  "Return status text for reference-note capture review in RUN."
  (if (delib-flow--reference-note-preview-item run)
      "These note-capture settings control which org-roam template will file this note and any extra target-path input it requires."
    "No reference-note filing artifact is currently active."))

(defun delib-flow--matched-project-level (project-title)
  "Return outline level for PROJECT-TITLE in the configured projects file."
  (unless delib-flow-my-projects-file
    (error "The My Projects file is not configured"))
  (with-current-buffer (delib-flow--org-file-buffer delib-flow-my-projects-file)
    (save-excursion
      (save-restriction
        (widen)
        (delib-flow--matched-project-point project-title)))))

(defun delib-flow--staged-project-item-preview (item package)
  "Return exact staged text preview for project ITEM from PACKAGE."
  (if (eq (plist-get item :kind) 'project)
      (delib-flow--fill-capture-template
       (delib-flow--project-item-capture-template item)
       (delib-flow--capture-template-context
        item package (plist-get item :title) nil))
    (let* ((project-title (delib-flow--matched-project-title package))
           (level (1+ (delib-flow--matched-project-level project-title))))
      (delib-flow--fill-capture-template
       (delib-flow--project-item-capture-template item)
       (delib-flow--capture-template-context item package project-title level)))))

(defun delib-flow--staged-reference-note-metadata-preview (item package)
  "Return metadata staging preview for support-note ITEM from PACKAGE, or nil."
  (when-let* ((project-title (delib-flow--matched-project-title package))
              (_ (delib-flow--project-support-note-p item))
              (target (delib-flow--reference-note-file item package)))
    (format ":REFERENCE_FILES: add %s"
            (delib-flow--reference-note-link item target package))))

(defun delib-flow--staged-content-preview-blocks (item package)
  "Return preview blocks for the exact staged content of ITEM from PACKAGE."
  (let ((locations (delib-flow--planned-file-location item package)))
    (cond
     ((eq (plist-get item :kind) 'reference-note)
      (append
       (when-let ((location (car locations)))
         (list
          (format "**** %s\n- Target: %s\n- State: staged only; not saved\n#+begin_example\n%s\n#+end_example"
                  (if (delib-flow--project-support-note-p item)
                      "Support note file"
                    "Reference note file")
                  (plist-get location :target)
                  (string-trim-right
                   (delib-flow--reference-note-content item package)))))
       (when-let ((metadata-preview
                   (delib-flow--staged-reference-note-metadata-preview item package))
                  (metadata-location (cadr locations)))
         (list
          (format "**** Project metadata update\n- Target: %s\n- State: staged only; not saved\n#+begin_example\n%s\n#+end_example"
                  (plist-get metadata-location :target)
                  metadata-preview)))))
     (t
      (when-let ((location (car locations)))
        (list
         (format "**** %s\n- Target: %s\n- State: staged only; not saved\n#+begin_example\n%s\n#+end_example"
                 (if (eq (plist-get item :kind) 'project)
                     "Project heading insert"
                   "Project child insert")
                 (plist-get location :target)
                 (string-trim-right
                  (delib-flow--staged-project-item-preview item package)))))))))

(defun delib-flow--staged-content-preview-text (run)
  "Return staged-content-preview detail text for RUN."
  (if-let ((items (delib-flow--planned-file-preview-items run)))
      (condition-case err
          (mapconcat
           #'identity
           (apply #'append
                  (mapcar (lambda (item)
                            (delib-flow--staged-content-preview-blocks item run))
                          items))
           "\n\n")
        (error
         (format "Staged content preview is unavailable: %s"
                 (error-message-string err))))
    "No staged content preview is available yet."))

(defun delib-flow--selection-block-status (run)
  "Return blocked-approval status text for RUN."
  (if (plist-get (plist-get run :filing) :selection-blocking-warnings)
      "The last approval attempt is blocked."
    "No blocked approval details are currently recorded."))

(defun delib-flow--selection-block-text (run)
  "Return blocked-approval detail text for RUN."
  (let* ((filing (plist-get run :filing))
         (warnings (plist-get filing :selection-blocking-warnings))
         (item (plist-get filing :selection-blocked-item))
         (selection (plist-get filing :selection-blocked-selection))
         (notes (plist-get filing :selection-blocked-notes))
         (draft-items (plist-get filing :draft-items))
         (ready-indexes
          (delib-flow--draft-item-selection-indexes
           draft-items
           #'delib-flow--draft-item-ready-p)))
    (if warnings
        (concat
         (format "- Operator selection: %s\n"
                 (or selection "none"))
         (format "- Operator notes: %s\n"
                 (if (delib-flow--non-empty-string-p notes) notes "none"))
         (format "- Ready alternative selections: %s\n"
                 (delib-flow--selection-index-list ready-indexes))
         "- Resolution: approve a different ready artifact, fix the blocking warnings, or reject this artifact before retrying approval.\n"
         (if item
             (concat "- Blocked artifact:\n"
                     (delib-flow--draft-item-preview-text (list item))
                     "\n")
           "")
      (mapconcat
          (lambda (warning)
            (format "- Blocking warning: %s\n  Fix: %s"
                    (plist-get warning :message)
                    (delib-flow--draft-item-warning-remediation warning)))
          warnings
          "\n"))
      "No blocked approval details are available.")))

(defun delib-flow--filing-selection-instructions (run)
  "Return operator instructions for the filing-selection block in RUN."
  (let* ((items (plist-get (plist-get run :filing) :draft-items))
         (ready-indexes
          (delib-flow--draft-item-selection-indexes
           items
           #'delib-flow--draft-item-ready-p))
         (blocked-indexes
          (delib-flow--draft-item-selection-indexes
           items
           (lambda (item)
             (not (delib-flow--draft-item-ready-p item))))))
    (concat
     (format "- Pick one queue item at a time. Type exactly one ready index after `Selection:`. Example: `%s`.\n"
             (if ready-indexes
                 (format "Selection: %s" (car ready-indexes))
               "Selection: 1"))
     (format "- Ready to approve now: %s\n"
             (delib-flow--selection-index-list ready-indexes))
     (format "- Needs fixes or rejection first: %s\n"
             (delib-flow--selection-index-list blocked-indexes))
     "- Press `s` or run `M-x delib-flow-choose-filing-selection` to choose from valid queue items with completion.\n"
     "- Multiple indexes such as `1, 3` are invalid here.\n"
     "- Add optional text under `Notes:` if you want the reason kept with this approval or rejection.\n"
     (if (plist-get (plist-get run :filing) :approved-items)
         "- Run `Approve Another Filing Artifact` to approve another ready queue item, or run `File Approved Outputs` to stage the approved item now."
       "- Run `Select Approved Filing Actions` to approve the chosen ready item, or run `Reject Draft Filing Artifact` if you want to discard it instead."))))

(defun delib-flow--rejected-item-status (run)
  "Return rejected-artifact status text for RUN."
  (if (plist-get (plist-get run :filing) :rejected-items)
      "Rejected filing artifacts are available."
    "No rejected artifacts are available yet."))

(defun delib-flow--rejected-item-text (run)
  "Return rejected-artifact body text for RUN."
  (let ((items (plist-get (plist-get run :filing) :rejected-items)))
    (if items
        (delib-flow--draft-item-preview-text items)
      "No rejected artifacts are available yet.")))

(defun delib-flow--filed-location-status (run)
  "Return filed-location status text for RUN."
  (if (plist-get (plist-get run :filing) :target-locations)
      "These target buffers were opened and staged, but nothing has been saved to disk yet."
    "No staged target buffers are available yet."))

(defun delib-flow--filed-location-line (location)
  "Return preview line for filed LOCATION."
  (format "- %s -> %s"
          (plist-get location :item-text)
          (plist-get location :target)))

(defun delib-flow--project-file-target (item package)
  "Return deterministic project filing target for ITEM from PACKAGE."
  (if (eq (plist-get item :kind) 'project)
      (format "%s::%s" delib-flow-my-projects-file
              (plist-get item :title))
    (let ((project-title (delib-flow--matched-project-title package)))
      (unless project-title
        (error "Approved project filing requires a matched project"))
      (format "%s::%s" delib-flow-my-projects-file project-title))))

(defun delib-flow--reference-note-metadata-target (item package)
  "Return metadata target string for support-note ITEM from PACKAGE."
  (let ((project-title (delib-flow--matched-project-title package)))
    (when (and project-title
               (delib-flow--project-support-note-p item))
      (format "%s::%s:REFERENCE_FILES" delib-flow-my-projects-file
              project-title))))

(defun delib-flow--planned-reference-note-locations (item package)
  "Return deterministic target preview locations for reference-note ITEM."
  (let ((target (delib-flow--reference-note-file item package))
        (metadata-target (delib-flow--reference-note-metadata-target item package)))
    (delq nil
          (list (delib-flow--filed-location item target)
                (and metadata-target
                     (delib-flow--filed-location item metadata-target))))))

(defun delib-flow--planned-file-location (item package)
  "Return deterministic target preview locations for approved ITEM."
  (if (eq (plist-get item :kind) 'reference-note)
      (delib-flow--planned-reference-note-locations item package)
    (list (delib-flow--filed-location
           item
           (delib-flow--project-file-target item package)))))

(defun delib-flow--preview-selected-filing-items (run)
  "Return currently typed ready filing items from RUN for preview."
  (condition-case nil
      (when-let ((item (delib-flow--selected-filing-item run)))
        (unless (delib-flow--draft-item-blocking-warnings item)
          (list item)))
    (error nil)))

(defun delib-flow--planned-file-preview-items (run)
  "Return filing items that should drive deterministic target preview for RUN."
  (or (delib-flow--approved-items run)
      (and (delib-flow--filing-selection-active-p run)
           (delib-flow--preview-selected-filing-items run))))

(defun delib-flow--planned-file-locations (run)
  "Return deterministic filing target preview locations for RUN."
  (when-let ((items (delib-flow--planned-file-preview-items run)))
    (condition-case err
        (mapcan (lambda (item)
                  (delib-flow--planned-file-location item run))
                items)
      (error
       (list (list :kind 'planning-error
                   :item-text "Planned filing targets are unavailable"
                   :target (error-message-string err)))))))

(defun delib-flow--planned-file-location-status (run)
  "Return planned-target status text for RUN."
  (cond
   ((plist-get (plist-get run :filing) :approved-items)
    "If you file now, delib-flow will stage the approved artifact into these targets.")
   ((delib-flow--preview-selected-filing-items run)
    "If you approve the currently selected queue item, it will file to these targets.")
   ((delib-flow--non-empty-string-p
     (delib-flow--filing-selection-value run))
    "The current Selection does not point to one ready item yet. Enter one ready index to preview its targets.")
   (t
    "Choose one ready queue item to preview where filing will put it.")))

(defun delib-flow--planned-file-location-line (location)
  "Return preview line for planned filing LOCATION."
  (format "- %s -> %s"
          (plist-get location :item-text)
          (plist-get location :target)))

(defun delib-flow--planned-file-location-text (run)
  "Return planned-target detail text for RUN."
  (if-let ((locations (delib-flow--planned-file-locations run)))
      (mapconcat #'delib-flow--planned-file-location-line locations "\n")
    "No planned file targets are available yet."))

(defun delib-flow--filed-location-text (run)
  "Return filed-location body text for RUN."
  (let ((locations (plist-get (plist-get run :filing) :target-locations)))
    (if locations
        (mapconcat #'delib-flow--filed-location-line locations "\n")
      "No filed target locations are available yet.")))

(defun delib-flow--filing-preview-actions (run)
  "Return filing-focused available actions for RUN."
  (seq-filter
   (lambda (action)
     (memq (plist-get action :id)
           '(draft-selected-reference-note
             select-approved-filing-actions
             reject-draft-filing-artifact
             file-approved-outputs
             resolve-filing-conflict)))
   (delib-flow--sorted-actions run)))

(defun delib-flow--filing-preview-actions-text (run)
  "Return rendered filing-focused action lines for RUN."
  (if-let ((actions (delib-flow--filing-preview-actions run)))
      (delib-flow--render-action-lines actions)
    "No filing actions are available yet."))

(defun delib-flow--recommended-filing-action (run)
  "Return the best currently available filing-focused action for RUN."
  (car (delib-flow--filing-preview-actions run)))

(defun delib-flow--recommended-filing-action-text (run)
  "Return rendered recommendation text for the filing loop in RUN."
  (if-let ((action (delib-flow--recommended-filing-action run)))
      (format "- Do this next: %s\n- What it does: %s\n- If blocked: %s"
              (plist-get action :label)
              (delib-flow--compact-summary
               (or (plist-get action :reason)
                   "This is the next local filing action available from the current state.")
               (+ 12 (delib-flow--mobile-summary-limit)))
              (delib-flow--compact-summary
               (delib-flow--unblock-guidance-text run)
               (+ 12 (delib-flow--mobile-summary-limit))))
    (format "- Do this next: none\n- What it does: No filing action is currently available.\n- If blocked: %s"
            (delib-flow--compact-summary
             (delib-flow--unblock-guidance-text run)
             (+ 12 (delib-flow--mobile-summary-limit))))))

(defun delib-flow--current-filing-template-text (run)
  "Return compact template summary text for RUN."
  (if-let* ((item (delib-flow--reference-note-preview-item run))
            (package (delib-flow--stage-input-package run 'file-approved-outputs))
            (key (delib-flow--reference-note-effective-template-key item package)))
      (format "org-roam template `%s`" key)
    "Project capture template"))

(defun delib-flow--filing-save-state-text (run)
  "Return compact save-state summary text for RUN."
  (if (plist-get (plist-get run :filing) :target-locations)
      "Staged in buffers; not saved to disk"
    "Preview only; nothing staged yet"))

(defun delib-flow--filing-target-count-text (run)
  "Return compact target-count summary text for RUN."
  (if-let ((locations (delib-flow--planned-file-locations run)))
      (format "%s target(s)" (length locations))
    "No targets yet"))

(defun delib-flow--filing-workspace-summary-text (run)
  "Return compact workspace summary for the active filing loop in RUN."
  (format "- Selected item: %s\n- Target count: %s\n- Capture template: %s\n- Save state: %s\n- Peek actions: `P` target window, `V` staged content"
          (delib-flow--compact-summary (delib-flow--active-filing-item-text run))
          (delib-flow--filing-target-count-text run)
          (delib-flow--current-filing-template-text run)
          (delib-flow--filing-save-state-text run)))

(defun delib-flow--filing-loop-update-text (run)
  "Return compact local consequence text for the filing loop in RUN."
  (let* ((entry (delib-flow--latest-stage-entry run))
         (stage-id (plist-get entry :stage-id))
         (draft-count (length (or (plist-get (plist-get run :filing) :draft-items) nil)))
         (approved-count (length (or (plist-get (plist-get run :filing) :approved-items) nil)))
         (rejected-count (length (or (plist-get (plist-get run :filing) :rejected-items) nil))))
    (cond
     ((null entry)
      "- Local consequence: filing has not started yet.\n- Resume here: run a drafting pass to create a queue of candidates.")
     ((eq stage-id 'select-approved-filing-actions)
      (format "- Local consequence: one queue item was approved and its target preview was recalculated.\n- Filing state: %s draft, %s approved, %s rejected.\n- Resume here: review targets below, then file it or approve another item."
              draft-count approved-count rejected-count))
     ((eq stage-id 'reject-draft-filing-artifact)
      (format "- Local consequence: one queue item was removed from this run.\n- Filing state: %s draft, %s approved, %s rejected.\n- Resume here: choose another item or run another drafting pass."
              draft-count approved-count rejected-count))
     ((eq stage-id 'file-approved-outputs)
      (format "- Local consequence: target buffers were staged and opened, but not saved.\n- Filing state: %s approved artifact(s) still tracked in this run.\n- Resume here: inspect the staged targets, save if correct, or continue drafting."
              approved-count))
     ((eq stage-id 'resolve-filing-conflict)
      "- Local consequence: conflict handling changed the approved item or retry path.\n- Resume here: retry filing or review the updated approved item.")
     ((memq stage-id '(extract-actions extract-waiting-for suggest-reference-notes propose-new-project))
      (format "- Local consequence: this stage refreshed the filing queue.\n- Filing state: %s draft, %s approved, %s rejected.\n- Resume here: choose one ready item, approve it, then review where it will go."
              draft-count approved-count rejected-count))
     (t
      (format "- Local consequence: %s\n- Resume here: %s"
              (delib-flow--compact-summary
               (delib-flow--latest-consequence-text run)
               (+ 16 (delib-flow--mobile-summary-limit)))
              (delib-flow--active-loop-location-text run))))))

(defun delib-flow--target-location-file-path (target)
  "Return file path portion of filed TARGET."
  (car (split-string target "::")))

(defun delib-flow--target-location-heading (target)
  "Return heading portion of filed TARGET, when present."
  (when (string-match "::\\([^:\n]+\\)" target)
    (match-string 1 target)))

(defun delib-flow--display-filed-target-location (location)
  "Display filed LOCATION and return its buffer, or nil."
  (let* ((target (plist-get location :target))
         (file (and target
                    (delib-flow--target-location-file-path target))))
    (when file
      (let ((buffer (delib-flow--org-file-buffer file)))
        (with-current-buffer buffer
          (when-let ((heading (delib-flow--target-location-heading target)))
            (goto-char (point-min))
            (when (re-search-forward
                   (format "^\\*+ %s\\(?:\n\\|$\\)"
                           (regexp-quote heading))
                   nil t)
              (org-back-to-heading t))))
        (display-buffer-in-side-window
         buffer
         '((side . right)
           (slot . 0)
           (window-width . 0.45)))
        buffer))))

(defvar delib-flow-staged-content-preview-buffer-name "*delib-flow-staged-preview*"
  "Name of the staged content preview buffer.")

(defun delib-flow--populate-staged-content-preview-buffer (buffer run)
  "Populate staged content preview BUFFER from RUN and return BUFFER."
  (let ((text (or (delib-flow--staged-content-preview-text run)
                  "No staged content preview is available yet.")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "* Delib-Flow staged content preview\n\n")
        (insert text)
        (goto-char (point-min))
        (org-mode)
        (setq-local truncate-lines nil)
        (visual-line-mode 1)
        (org-fold-show-all)
        (view-mode 1))))
  buffer)

(defun delib-flow--refresh-staged-content-preview-buffer (&optional run)
  "Refresh the staged content preview buffer from RUN when it is visible."
  (when-let ((buffer (get-buffer delib-flow-staged-content-preview-buffer-name)))
    (delib-flow--populate-staged-content-preview-buffer
     buffer
     (or run delib-flow--active-run))))

(defun delib-flow--show-staged-content-preview-buffer (run)
  "Display staged filing content preview for RUN and return the buffer."
  (let ((text (delib-flow--staged-content-preview-text run)))
    (when (or (null text)
              (string-prefix-p "No staged content preview is available yet." text)
              (string-prefix-p "Staged content preview is unavailable:" text))
      (user-error "%s" (or text "No staged content preview is available")))
    (let ((buffer
           (delib-flow--populate-staged-content-preview-buffer
            (get-buffer-create delib-flow-staged-content-preview-buffer-name)
            run)))
      (display-buffer-in-side-window
       buffer
       '((side . right)
         (slot . 1)
         (window-width . 0.45)))
      buffer)))

(defun delib-flow--show-filed-target-locations (run)
  "Display filed target locations for RUN and announce them."
  (when-let ((locations (plist-get (plist-get run :filing) :target-locations)))
    (delib-flow--display-filed-target-location (car locations))
    (message "Staged filing previews (not saved):\n%s"
             (mapconcat #'delib-flow--normalize-file-target-location
                        locations
                        "\n"))))

(defun delib-flow-peek-filing-target ()
  "Open the first planned or staged filing target in a sibling window."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((locations (or (plist-get (plist-get delib-flow--active-run :filing)
                                  :target-locations)
                       (delib-flow--planned-file-locations delib-flow--active-run))))
    (unless locations
      (user-error "No filing target is available to preview"))
    (unless (delib-flow--display-filed-target-location (car locations))
      (user-error "Could not open the filing target preview"))))

(defun delib-flow-peek-staged-content ()
  "Open the exact staged filing content in a preview buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--show-staged-content-preview-buffer delib-flow--active-run))

(defun delib-flow-jump-active-loop ()
  "Move point to the active decision loop in the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--clear-sticky-anchor)
  (let ((buffer (delib-flow--control-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "No active delib-flow control buffer"))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (delib-flow--goto-section
               (delib-flow--active-loop-heading delib-flow--active-run))
        (user-error "No active loop is currently visible"))
      (delib-flow--align-heading-top))))

(defun delib-flow-jump-latest-preview ()
  "Move point to the latest consequence preview in the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--clear-sticky-anchor)
  (let ((buffer (delib-flow--control-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "No active delib-flow control buffer"))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (delib-flow--goto-section
               (delib-flow--latest-preview-heading delib-flow--active-run))
        (user-error "No latest preview is currently available"))
      (delib-flow--align-heading-top))))

(defun delib-flow-jump-stage-history ()
  "Move point to the latest stage-history details in the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--clear-sticky-anchor)
  (let ((buffer (delib-flow--control-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "No active delib-flow control buffer"))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (unless (delib-flow--goto-section "Stage history")
        (user-error "No stage history is currently available"))
      (delib-flow--align-heading-top))))

(defun delib-flow--filing-conflict-status (run)
  "Return filing-conflict status text for RUN."
  (if (plist-get (plist-get run :filing) :conflicts)
      "Filing conflicts require resolution before approved artifacts can be written."
    "No filing conflicts are currently recorded."))

(defun delib-flow--filing-conflict-line (conflict)
  "Return preview line for filing CONFLICT."
  (format "- %s -> %s"
          (plist-get conflict :item-text)
          (plist-get conflict :reason)))

(defun delib-flow--filing-conflict-text (run)
  "Return filing-conflict body text for RUN."
  (let ((conflicts (plist-get (plist-get run :filing) :conflicts)))
    (if conflicts
        (mapconcat #'delib-flow--filing-conflict-line conflicts "\n")
      "No filing conflicts are currently recorded.")))

(defun delib-flow--render-filing-preview-section (_run)
  "Return Org text for the Filing preview section."
  (let ((reference-note-section
         (if (delib-flow--reference-note-capture-visible-p _run)
             (format "*** Reference note capture\n%s\n%s\n\n"
                     (delib-flow--reference-note-capture-status _run)
                     (delib-flow--render-editable-block
                      _run 'reference-note-capture-review))
           ""))
        (artifact-selection-section
         (if (delib-flow--filing-selection-active-p _run)
             (format "*** Artifact selection\n%s\n\n**** Choose from queue\n%s\n\n**** Selection form\n%s\n\n"
                     (delib-flow--filing-selection-instructions _run)
                     (delib-flow--filing-selection-shortlist-cards _run)
                     (delib-flow--render-editable-block
                      _run 'filing-selection-review))
           ""))
        (conflict-section
         (if (delib-flow--filing-conflict-resolution-active-p _run)
             (format "*** Conflict resolution\n%s\n\n"
                     (delib-flow--render-editable-block
                      _run 'filing-conflict-resolution))
           "")))
    (format "*** What happens here\n%s\n\n*** What to do next\n%s\n\n*** Current filing plan\n%s\n\n*** Filing actions\n%s\n\n*** Current filing choice\n%s\n%s\n\n*** Selected note draft\n%s\n%s\n\n*** Regenerate selected note\n%s\n%s\n\n*** Planned file targets\n%s\n%s\n\n*** Staged content preview\n%s\n%s\n\n%s%s%s*** Available queue\n%s\n\n*** Why approval is blocked\n%s\n\n*** Rejected artifacts\n%s\n\n*** Filing conflicts\n%s\n\n*** Opened staged targets\n%s\n%s\n"
            (delib-flow--draft-item-status _run)
            (delib-flow--recommended-filing-action-text _run)
            (delib-flow--filing-workspace-summary-text _run)
            (delib-flow--filing-preview-actions-text _run)
            (delib-flow--current-filing-choice-status _run)
            (delib-flow--current-filing-choice-text _run)
            (delib-flow--reference-note-draft-preview-status _run)
            (delib-flow--reference-note-draft-preview-text _run)
            (delib-flow--reference-note-regeneration-status _run)
            (delib-flow--reference-note-regeneration-text _run)
            (delib-flow--planned-file-location-status _run)
            (delib-flow--planned-file-location-text _run)
            (delib-flow--staged-content-preview-status _run)
            (delib-flow--staged-content-preview-text _run)
            reference-note-section
            artifact-selection-section
            conflict-section
            (delib-flow--draft-item-text _run)
            (format "%s\n%s"
                    (delib-flow--selection-block-status _run)
                    (delib-flow--selection-block-text _run))
            (delib-flow--rejected-item-text _run)
            (delib-flow--filing-conflict-text _run)
            (delib-flow--filed-location-status _run)
            (delib-flow--filed-location-text _run))))

(defun delib-flow--audit-log-file-display ()
  "Return user-facing audit log file display text."
  (if (delib-flow--audit-log-configured-p)
      delib-flow-audit-log-file
    "unconfigured"))

(defun delib-flow--audit-run-state-text (run)
  "Return run audit status text for RUN."
  (let* ((audit (plist-get run :audit))
         (run-record (plist-get audit :run-record)))
    (format "- Audit log file: %s\n- Audit payload policy: %s\n- Audit redaction profile: %s\n- Run status: %s\n- Run ID: %s"
            (delib-flow--audit-log-file-display)
            (delib-flow--audit-payload-policy)
            (delib-flow--audit-redaction-profile)
            (plist-get run-record :run-status)
            (plist-get run-record :run-id))))

(defun delib-flow--audit-stage-readiness-text (run)
  "Return stage audit readiness text for RUN."
  (let* ((audit (plist-get run :audit))
         (stage-count (length (plist-get audit :stage-records)))
         (pending (plist-get audit :pending-checkpoints))
         (last (plist-get audit :last-appended-checkpoint))
         (latest-stage (plist-get (delib-flow--run-stage-history run) :latest-stage)))
    (format "- Recorded stages: %s\n- Pending checkpoints: %s\n- Last appended checkpoint: %s\n- Latest recorded stage: %s"
            stage-count
            (if pending
                (mapconcat #'symbol-name pending ", ")
              "none")
            (if last (symbol-name last) "none")
            (if latest-stage (symbol-name latest-stage) "none"))))

(defun delib-flow--audit-navigation-text (run)
  "Return audit navigation summary text for RUN."
  (if-let ((latest-record (delib-flow--audit-latest-stage-record run)))
      (format "- Jump to active run audit: %s\n- Jump to latest audit stage: %s\n- Latest audit attempt: %s attempt %d\n- Latest audit provider: %s"
              (delib-flow--audit-navigation-availability)
              (delib-flow--audit-navigation-availability)
              (plist-get latest-record :label)
              (plist-get latest-record :attempt-number)
              (or (plist-get latest-record :provider) "unknown"))
    (format "- Jump to active run audit: %s\n- Jump to latest audit stage: %s\n- Latest audit attempt: none\n- Latest audit provider: none"
            (delib-flow--audit-navigation-availability)
            (delib-flow--audit-navigation-availability))))

(defun delib-flow--render-audit-status-section (_run)
  "Return Org text for the Audit status section."
  (format "** Run audit state\n%s\n\n** Stage audit readiness\n%s\n\n** Audit navigation\n%s\n"
          (delib-flow--audit-run-state-text _run)
          (delib-flow--audit-stage-readiness-text _run)
          (delib-flow--audit-navigation-text _run)))

(defun delib-flow--count-body-lines (content)
  "Return the count of non-empty body lines in CONTENT."
  (length
   (seq-filter
    (lambda (line)
      (not (string-empty-p (string-trim line))))
    (cdr (split-string content "\n")))))

(defun delib-flow--count-text-words (text)
  "Return normalized word count for TEXT."
  (length (delib-flow--string-words text)))

(defun delib-flow--source-body-text (content)
  "Return CONTENT without the heading line."
  (mapconcat #'identity (cdr (split-string (or content "") "\n")) "\n"))

(defun delib-flow--source-org-property (content property)
  "Return Org PROPERTY value from CONTENT, or nil."
  (when (and (stringp content) (stringp property))
    (when (string-match
           (format "^:%s:[[:space:]]*\\(.*\\)$" (regexp-quote property))
           content)
      (let ((value (string-trim (match-string 1 content))))
        (unless (string-empty-p value)
          value)))))

(defun delib-flow--source-section-body (content section-name)
  "Return body text for drawer-like SECTION-NAME in CONTENT, or nil."
  (when (and (stringp content) (stringp section-name))
    (let* ((lines (split-string content "\n"))
           (start-marker (format ":%s:" section-name))
           (collecting nil)
           collected)
      (dolist (line lines)
        (cond
         ((and (not collecting)
               (string= (string-trim-right line) start-marker))
          (setq collecting t))
         ((and collecting
               (string= (string-trim line) ":END:"))
          (setq collecting 'done))
         ((eq collecting t)
          (push line collected))))
      (when collected
        (let ((value (string-trim (mapconcat #'identity (nreverse collected) "\n"))))
          (unless (string-empty-p value)
            value))))))

(defun delib-flow--email-source-shape-p (source)
  "Return non-nil when SOURCE looks like an imported email capture."
  (let* ((content (or (plist-get source :content) ""))
         (file (or (plist-get source :file) "")))
    (or (delib-flow--non-empty-string-p
         (delib-flow--source-org-property content "FROM"))
        (delib-flow--non-empty-string-p
         (delib-flow--source-org-property content "EMAIL_FILE"))
        (delib-flow--non-empty-string-p
         (delib-flow--source-section-body content "RAW_EMAIL"))
        (string-match-p "/mail/" file)
        (string-match-p ":email:" content))))

(defun delib-flow--email-raw-header-value (text header)
  "Return HEADER value from raw email TEXT, or nil."
  (when (and (stringp text) (stringp header))
    (when (string-match
           (format "^%s:[[:space:]]*\\(.*\\)$" (regexp-quote header))
           text)
      (let ((value (string-trim (match-string 1 text))))
        (unless (string-empty-p value)
          value)))))

(defun delib-flow--decode-quoted-printable-text (text)
  "Return TEXT with common quoted-printable artifacts reduced."
  (let ((value (or text "")))
    (setq value (replace-regexp-in-string "=\r?\n" "" value))
    (setq value (replace-regexp-in-string "=0D=0A\\|=0A" "\n" value t t))
    (setq value (replace-regexp-in-string "=09" "\t" value t t))
    (setq value (replace-regexp-in-string "=3D" "=" value t t))
    (setq value (replace-regexp-in-string "=20" " " value t t))
    (setq value (replace-regexp-in-string "=C2=A0" " " value t t))
    (setq value (replace-regexp-in-string "=E2=80=99" "'" value t t))
    (replace-regexp-in-string "=[0-9A-F][0-9A-F]" "" value t)))

(defun delib-flow--email-strip-html-noise (text)
  "Return TEXT with HTML and boilerplate noise reduced."
  (let ((value (or text "")))
    (setq value (replace-regexp-in-string "<[^>]+>" " " value))
    (setq value (replace-regexp-in-string "&nbsp;" " " value t t))
    (setq value (replace-regexp-in-string "&amp;" "&" value t t))
    (setq value (replace-regexp-in-string "https?://[^[:space:]]+" " " value))
    (setq value (replace-regexp-in-string "[[:space:]]+" " " value))
    (string-trim value)))

(defun delib-flow--email-useful-line-p (line)
  "Return non-nil when LINE carries useful email body content."
  (let ((text (string-trim (or line ""))))
    (and (not (string-empty-p text))
         (not (string-match-p
               "\\`\\(?:X-[[:alnum:]-]+\\|Received\\|Authentication-Results\\|Received-SPF\\|DKIM-Signature\\|Return-Path\\|Content-Type\\|Content-Transfer-Encoding\\|MIME-Version\\|boundary\\|--[-[:alnum:]]+\\|<!DOCTYPE\\|<html\\|<body\\|</html>\\|</body>\\)\\(?:[: ].*\\)?\\'"
               text))
         (not (string-match-p
               "\\`\\(?:This email message was auto-generated\\|If you need additional help\\|View this message on the web\\|Update My Preferences\\|Unsubscribe\\|All rights reserved\\|Specific pricing and discounts may be subject to change\\)\\b"
               text))
         (not (string-match-p "\\`=?utf-8\\?" text)))))

(defun delib-flow--email-plain-body-from-raw (raw-email)
  "Return a reduced plain-text body extracted from RAW-EMAIL."
  (let* ((text (or raw-email ""))
         (plain
          (cond
           ((string-match
             "Content-Type:[[:space:]]*text/plain[^\n]*\n\\(?:Content-Transfer-Encoding:[^\n]*\n\\)?\n\\([\0-\377[:nonascii:][:ascii:]]*?\\)\n--[-[:alnum:]]+"
             text)
            (match-string 1 text))
           ((string-match
             "Content-Type:[[:space:]]*text/plain[^\n]*\n\\([\0-\377[:nonascii:][:ascii:]]*\\)"
             text)
            (match-string 1 text))
           (t text)))
         (decoded (delib-flow--decode-quoted-printable-text plain))
         (lines (split-string decoded "\n"))
         kept)
    (dolist (line lines)
      (when (delib-flow--email-useful-line-p line)
        (push (string-trim line) kept)))
    (string-trim
     (mapconcat #'identity (seq-take (nreverse kept) 24) "\n"))))

(defun delib-flow--email-type-hint (from body)
  "Return a deterministic type hint for email FROM and BODY."
  (let ((sender (downcase (or from "")))
        (text (downcase (or body ""))))
    (cond
     ((or (string-match-p "newsletter" sender)
          (string-match-p "cohort\\|waitlist\\|unsubscribe\\|join the next cohort" text))
      "newsletter or mailing list")
     ((or (string-match-p "noreply\\|no-reply\\|do-not-reply" sender)
          (string-match-p "auto-generated\\|notification\\|wishlist\\|unsubscribe" text))
      "transactional notification")
     ((string-match-p "forwarded message\\|reply-to\\|re:" text)
      "correspondence or discussion")
     (t
      "email message"))))

(defun delib-flow--email-inspect-digest (source)
  "Return deterministic inspect digest plist for email-shaped SOURCE."
  (let* ((content (or (plist-get source :content) ""))
         (raw-email (or (delib-flow--source-section-body content "RAW_EMAIL") ""))
         (from (or (delib-flow--source-org-property content "FROM")
                   (delib-flow--email-raw-header-value raw-email "From")))
         (to (delib-flow--email-raw-header-value raw-email "To"))
         (cc (delib-flow--email-raw-header-value raw-email "Cc"))
         (subject (or (delib-flow--email-raw-header-value raw-email "Subject")
                      (plist-get source :title)))
         (date (or (delib-flow--source-org-property content "DATE")
                   (delib-flow--email-raw-header-value raw-email "Date")))
         (reply-to (delib-flow--email-raw-header-value raw-email "Reply-To"))
         (message-id (or (delib-flow--source-org-property content "MESSAGE_ID")
                         (delib-flow--email-raw-header-value raw-email "Message-Id")))
         (email-file (delib-flow--source-org-property content "EMAIL_FILE"))
         (body (delib-flow--email-plain-body-from-raw raw-email))
         (links (delete-dups
                 (append
                  (delib-flow--text-org-file-links content nil)
                  (let ((start 0)
                        found)
                    (while (string-match "https?://[^][()<>[:space:]\"]+" body start)
                      (push (match-string 0 body) found)
                      (setq start (match-end 0)))
                    (nreverse found)))))
        (type-hint (delib-flow--email-type-hint from body)))
    (list
     :subject subject
     :from from
     :to to
     :cc cc
     :date date
     :reply-to reply-to
     :message-id message-id
     :email-file email-file
     :type-hint type-hint
     :plain-body body
     :links links
     :contact-emails
     (delib-flow--meaningful-contact-emails
      (delete-dups
       (delq nil
             (list from reply-to))))
     :ignored-noise
     '("transport headers" "authentication headers" "HTML body" "quoted-printable artifacts" "footer boilerplate"))))

(defun delib-flow--source-metadata-header-count (text)
  "Return count of email-style metadata headers found in TEXT."
  (let ((count 0))
    (dolist (line (split-string (or text "") "\n") count)
      (when (string-match-p
             "^[[:space:]]*\\(From\\|To\\|Cc\\|Bcc\\|Subject\\|Date\\):"
             line)
        (setq count (1+ count))))))

(defun delib-flow--source-meeting-keywords (text)
  "Return meeting keywords detected in TEXT."
  (let ((downcased (downcase (or text ""))))
    (seq-filter (lambda (keyword)
                  (string-match-p (regexp-quote keyword) downcased))
                delib-flow--meeting-source-keywords)))

(defun delib-flow--source-keyword-hits (text keywords)
  "Return KEYWORDS found in TEXT."
  (let ((downcased (downcase (or text ""))))
    (seq-filter (lambda (keyword)
                  (string-match-p (regexp-quote keyword) downcased))
                keywords)))

(defun delib-flow--source-meeting-section-count (text)
  "Return count of meeting-style section labels found in TEXT."
  (let ((count 0)
        (downcased (downcase (or text ""))))
    (dolist (label delib-flow--meeting-source-section-labels count)
      (when (string-match-p (regexp-quote label) downcased)
        (setq count (1+ count))))))

(defun delib-flow--journal-outline-path-p (outline-path)
  "Return non-nil when OUTLINE-PATH suggests a journal-style source."
  (seq-some (lambda (segment)
              (string-match-p "journal\\|daily\\|logbook" (downcase segment)))
            outline-path))

(defun delib-flow--email-source-classification (header-count emails)
  "Return email classification plist for HEADER-COUNT and EMAILS."
  (list :source-type 'email
        :source-type-reason
        "Detected email-style headers and participant addresses."
        :source-type-signals
        (delq nil
              (list (and (> header-count 0)
                         (format "%s email-style headers" header-count))
                    (and (> (length emails) 0)
                         (format "%s participant addresses" (length emails)))))))

(defun delib-flow--meeting-source-classification (journal-path-p meeting-section-count meeting-keywords)
  "Return meeting-note classification plist for detected signals."
  (list :source-type 'meeting-note
        :source-type-reason
        "Detected meeting-note structure from keywords, sections, or journal placement."
        :source-type-signals
        (delq nil
              (append
               (and journal-path-p '("journal outline path"))
               (when (> meeting-section-count 0)
                 (list (format "%s meeting sections" meeting-section-count)))
                   meeting-keywords))))

(defun delib-flow--reminder-source-classification (hits)
  "Return reminder classification plist from keyword HITS."
  (list :source-type 'reminder
        :source-type-reason
        "Detected explicit reminder or follow-up wording in the title or body."
        :source-type-signals
        (delete-dups
         (seq-take
          (mapcar (lambda (hit)
                    (format "keyword: %s" hit))
                  hits)
          3))))

(defun delib-flow--fleeting-note-source-classification (signals)
  "Return fleeting-note classification plist from SIGNALS."
  (list :source-type 'fleeting-note
        :source-type-reason
        "Detected freeform note-capture language for ideas, thoughts, or open questions."
        :source-type-signals
        (delete-dups signals)))

(defun delib-flow--unknown-source-classification ()
  "Return fallback unknown source classification plist."
  (list :source-type 'unknown
        :source-type-reason
        "Evidence is too weak to classify this source beyond unknown."
        :source-type-signals nil))

(defun delib-flow--email-source-p (header-count emails)
  "Return non-nil when HEADER-COUNT and EMAILS indicate an email source."
  (or (>= header-count 2)
      (and (> header-count 0) (> (length emails) 0))))

(defun delib-flow--meeting-source-p (journal-path-p meeting-section-count meeting-keywords)
  "Return non-nil when detected signals indicate a meeting-note source."
  (or (>= meeting-section-count 2)
      (>= (length meeting-keywords) 2)
      (and journal-path-p
           (or (> meeting-section-count 0)
               meeting-keywords))))

(defun delib-flow--maybe-email-source-classification (header-count emails)
  "Return email classification plist when HEADER-COUNT and EMAILS qualify."
  (when (delib-flow--email-source-p header-count emails)
    (delib-flow--email-source-classification header-count emails)))

(defun delib-flow--maybe-meeting-source-classification (journal-path-p meeting-section-count meeting-keywords)
  "Return meeting-note classification plist when detected signals qualify."
  (when (delib-flow--meeting-source-p
         journal-path-p meeting-section-count meeting-keywords)
    (delib-flow--meeting-source-classification
     journal-path-p meeting-section-count meeting-keywords)))

(defun delib-flow--maybe-reminder-source-classification (combined-text)
  "Return reminder classification plist when COMBINED-TEXT qualifies."
  (when-let ((hits (delib-flow--source-keyword-hits
                    combined-text
                    delib-flow--reminder-source-keywords)))
    (delib-flow--reminder-source-classification hits)))

(defun delib-flow--maybe-fleeting-note-source-classification
    (combined-text body-text body-line-count)
  "Return fleeting-note classification plist when evidence is sufficient."
  (let ((hits (delib-flow--source-keyword-hits
               combined-text
               delib-flow--fleeting-note-source-keywords))
        (signals nil))
    (dolist (hit hits)
      (push (format "keyword: %s" hit) signals))
    (when (and (string-match-p "\\?" (or body-text ""))
               (<= body-line-count 4))
      (push "short question-style body" signals))
    (when signals
      (delib-flow--fleeting-note-source-classification
       (nreverse (delete-dups signals))))))

(defun delib-flow--source-type-classification (source)
  "Return deterministic source-type classification plist for SOURCE."
  (let* ((title (plist-get source :title))
         (content (or (plist-get source :content) ""))
         (body-text (delib-flow--source-body-text content))
         (outline-path (plist-get source :outline-path))
         (combined-text (string-join (delq nil (list title body-text
                                                     (and outline-path
                                                          (mapconcat #'identity outline-path " "))))
                                     "\n"))
         (emails (delib-flow--text-emails content))
         (header-count (delib-flow--source-metadata-header-count body-text))
         (meeting-keywords (delete-dups
                            (delib-flow--source-meeting-keywords combined-text)))
         (meeting-section-count
          (delib-flow--source-meeting-section-count body-text))
         (journal-path-p (delib-flow--journal-outline-path-p outline-path))
         (body-line-count (delib-flow--count-body-lines content)))
    (or (delib-flow--maybe-email-source-classification header-count emails)
        (delib-flow--maybe-meeting-source-classification
         journal-path-p meeting-section-count meeting-keywords)
        (delib-flow--maybe-reminder-source-classification combined-text)
        (delib-flow--maybe-fleeting-note-source-classification
         combined-text body-text body-line-count)
        (delib-flow--unknown-source-classification))))

(defun delib-flow--display-source-type (run)
  "Return the best available source type for RUN."
  (let* ((source (delib-flow--run-source run))
         (working (delib-flow--run-working-context run))
         (inspect-review (delib-flow--review-record working 'inspect-source))
         (accepted-inspect (plist-get inspect-review :accepted-output))
         (candidate-inspect (plist-get inspect-review :candidate-output)))
    (or (plist-get accepted-inspect :source-type)
        (plist-get candidate-inspect :source-type)
        (plist-get source :source-type)
        'unknown)))

(defun delib-flow--inspect-source-analysis (source)
  "Return structured inspect analysis for SOURCE."
  (let* ((content (or (plist-get source :content) ""))
         (body-text (delib-flow--source-body-text content))
         (file (plist-get source :file))
         (base-dir (and file (file-name-directory file)))
         (outline-path (plist-get source :outline-path))
         (classification (delib-flow--source-type-classification source))
         (digest (and (delib-flow--email-source-shape-p source)
                      (delib-flow--email-inspect-digest source)))
         (emails (or (plist-get digest :contact-emails)
                     (delib-flow--meaningful-contact-emails
                      (delib-flow--text-emails content))))
         (org-file-links (if base-dir
                             (delib-flow--text-org-file-links content base-dir)
                           nil)))
    (append
     (list :title (plist-get source :title)
           :outline-path outline-path
           :body-line-count (delib-flow--count-body-lines content)
           :content-word-count (delib-flow--count-text-words body-text)
           :has-id (not (null (plist-get source :id)))
           :contact-emails emails
           :contact-email-count (length emails)
           :org-file-links org-file-links
           :org-file-link-count (length org-file-links)
           :body-preview (string-trim (truncate-string-to-width body-text 160 nil nil t)))
     classification)))

(defun delib-flow--execute-inspect-source (package)
  "Return raw inspect-source output for PACKAGE."
  (let* ((source (plist-get package :source))
         (analysis (delib-flow--inspect-source-analysis source)))
    (plist-put (copy-tree analysis) :analysis analysis)))

(defun delib-flow--match-project-source (package)
  "Return the source object used for project matching from PACKAGE."
  (let* ((source (plist-get package :source))
         (working (plist-get package :working-context))
         (inspect-output (or (plist-get working :inspect-output)
                             (plist-get (plist-get working :source-snapshot)
                                        :inspect-output)))
         (inspect-title (delib-flow--normalize-match-source-title
                         (plist-get inspect-output :title))))
    (if (delib-flow--non-empty-string-p inspect-title)
        (plist-put (copy-sequence source) :title inspect-title)
      source)))

(defun delib-flow--project-candidates ()
  "Return parsed project candidates from `delib-flow-my-projects-file'."
  (unless (and delib-flow-my-projects-file
               (file-readable-p delib-flow-my-projects-file))
    (error "The My Projects file is not configured or readable"))
  (delib-flow--project-candidates-from-file delib-flow-my-projects-file))

(defun delib-flow--execute-match-project (package)
  "Return raw match-project output for PACKAGE."
  (delib-flow--project-match-result
   (delib-flow--match-project-source package)
   (delib-flow--project-candidates)))

(defun delib-flow--execute-discover-reference-material (package)
  "Return raw discovery output for PACKAGE."
  (delib-flow--discover-reference-material-result package))

(defun delib-flow--execute-filter-reference-material (package)
  "Return raw filter output for PACKAGE."
  (delib-flow--filter-reference-material-result package))

(defun delib-flow--execute-manual-project-match (package)
  "Return raw manual project-match output for PACKAGE."
  (delib-flow--manual-project-match-result package))

(defun delib-flow--execute-propose-new-project (package)
  "Return raw new-project proposal output for PACKAGE."
  (delib-flow--propose-new-project-result package))

(defun delib-flow--execute-extract-actions (package)
  "Return raw action-extraction output for PACKAGE."
  (delib-flow--extract-actions-result package))

(defun delib-flow--execute-extract-waiting-for (package)
  "Return raw waiting-for extraction output for PACKAGE."
  (delib-flow--extract-waiting-for-result package))

(defun delib-flow--execute-suggest-reference-notes (package)
  "Return raw reference-note suggestion output for PACKAGE."
  (delib-flow--suggest-reference-notes-result package))

(defun delib-flow--execute-draft-selected-reference-note (package)
  "Return raw selected-note drafting output for PACKAGE."
  (delib-flow--draft-selected-reference-note-result package))

(defun delib-flow--execute-decide-cloud-pass (package)
  "Return raw cloud-routing decision output for PACKAGE."
  (delib-flow--decide-cloud-pass-result package))

(defun delib-flow--execute-sanitize-for-cloud (package)
  "Return raw cloud-sanitization output for PACKAGE."
  (delib-flow--sanitize-for-cloud-result package))

(defun delib-flow--execute-approve-cloud-send (package)
  "Return raw cloud approval output for PACKAGE."
  (delib-flow--approve-cloud-send-result package))

(defun delib-flow--cloud-stage-package (package)
  "Return sanitized cloud stage package from PACKAGE."
  (plist-get (plist-get package :working-context) :cloud-sanitized-context))

(defun delib-flow--cloud-target-stage-id-from-package (package)
  "Return selected rerouted cloud target stage from PACKAGE."
  (delib-flow--cloud-target-stage (plist-get package :routing)))

(defun delib-flow--cloud-rerouted-stage-output (package)
  "Return raw output for PACKAGE's selected rerouted cloud stage."
  (let* ((target-stage (delib-flow--cloud-target-stage-id-from-package package))
         (descriptor (delib-flow--stage-descriptor target-stage)))
    (unless descriptor
      (error "No stage descriptor is registered for rerouted cloud stage %s"
             target-stage))
    (unless (eq target-stage 'run-cloud-stage)
      (funcall (plist-get descriptor :executor) package))))

(defun delib-flow--cloud-rerouted-stage-normalized-output (package raw-output)
  "Return normalized rerouted cloud output for PACKAGE and RAW-OUTPUT."
  (when raw-output
    (delib-flow--normalize-stage-output
     (delib-flow--cloud-target-stage-id-from-package package)
     raw-output)))

(defun delib-flow--cloud-stage-result (package)
  "Return raw cloud-stage result for PACKAGE."
  (let* ((sanitized-package (delib-flow--cloud-stage-package package))
         (target-stage (delib-flow--cloud-target-stage-id-from-package package))
         (target-raw-output (delib-flow--cloud-rerouted-stage-output package))
         (target-normalized-output
          (delib-flow--cloud-rerouted-stage-normalized-output
           package target-raw-output)))
    (list :target-stage target-stage
          :target-stage-raw-output target-raw-output
          :target-stage-normalized-output target-normalized-output
          :selected-model (delib-flow--cloud-model-choice package)
          :cloud-output
          (concat
           (format "Cloud output for rerouted stage %s.\nPrompt: %s\n\n%s"
                   (delib-flow--stage-label target-stage)
                   (or (delib-flow--stage-prompt-id target-stage) "none")
                   sanitized-package)
           (if target-normalized-output
               (format "\n\nProposed stage result:\n%s"
                       target-normalized-output)
             ""))
          :cloud-switch-pending nil
          :sanitization-status 'returned
          :reason
          (format "Cloud output for %s is ready for local review and reintegration."
                  (delib-flow--stage-label target-stage)))))

(defun delib-flow--execute-run-cloud-stage (package)
  "Return raw cloud-stage output for PACKAGE."
  (delib-flow--cloud-stage-result package))

(defun delib-flow--retry-cloud-failure-resolution-p (resolution)
  "Return non-nil when RESOLUTION retries cloud execution."
  (or (string-empty-p (or resolution ""))
      (string-equal (delib-flow--cloud-failure-resolution-keyword resolution)
                    "RETRY-CLOUD")))

(defun delib-flow--use-local-cloud-failure-resolution-p (resolution)
  "Return non-nil when RESOLUTION selects local fallback."
  (string-equal (delib-flow--cloud-failure-resolution-keyword resolution)
                "USE-LOCAL"))

(defun delib-flow--skip-cloud-failure-resolution-p (resolution)
  "Return non-nil when RESOLUTION skips the cloud branch."
  (string-equal (delib-flow--cloud-failure-resolution-keyword resolution)
                "SKIP-CLOUD"))

(defun delib-flow--abort-cloud-failure-resolution-p (resolution)
  "Return non-nil when RESOLUTION aborts the run."
  (string-equal (delib-flow--cloud-failure-resolution-keyword resolution)
                "ABORT"))

(defun delib-flow--cloud-failure-resolution-keyword-or-default (resolution)
  "Return normalized cloud failure RESOLUTION keyword."
  (if (delib-flow--non-empty-string-p resolution)
      (delib-flow--cloud-failure-resolution-keyword resolution)
    "RETRY-CLOUD"))

(defun delib-flow--cloud-failure-resolution-valid-p (keyword)
  "Return non-nil when cloud failure KEYWORD is supported."
  (member keyword '("RETRY-CLOUD" "USE-LOCAL" "SKIP-CLOUD" "ABORT")))

(defun delib-flow--cloud-failure-resolution-fallback-mode (keyword)
  "Return fallback mode implied by cloud failure KEYWORD."
  (alist-get keyword '(("USE-LOCAL" . use-local)
                       ("SKIP-CLOUD" . skip-cloud))
            nil nil #'string=))

(defun delib-flow--cloud-failure-resolution-reason (keyword)
  "Return operator-facing explanation for cloud failure KEYWORD."
  (alist-get
   keyword
   '(("USE-LOCAL" . "Operator selected local fallback after cloud failure. Continue with local reintegration only.")
     ("SKIP-CLOUD" . "Operator explicitly skipped the cloud branch after failure. Continue locally without cloud output.")
     ("ABORT" . "Operator aborted the run after reviewing the cloud failure.")
     ("RETRY-CLOUD" . "Cloud failure remains recoverable. Retry cloud execution when ready."))
   nil nil #'string=))

(defun delib-flow--ensure-cloud-failure-recorded (routing)
  "Signal when ROUTING does not contain a reviewable cloud failure."
  (unless (and (delib-flow--cloud-failure-stage routing)
               (delib-flow--cloud-failure-message routing))
    (error "Resolve Cloud Failure requires a recorded cloud failure")))

(defun delib-flow--resolve-cloud-failure-result (package)
  "Return raw cloud-failure resolution result for PACKAGE."
  (let* ((routing (plist-get package :routing))
         (resolution (delib-flow--cloud-failure-resolution-value package))
         (keyword (delib-flow--cloud-failure-resolution-keyword-or-default resolution))
         (notes (delib-flow--cloud-failure-resolution-notes package))
         (fallback-mode
          (delib-flow--cloud-failure-resolution-fallback-mode keyword)))
    (delib-flow--ensure-cloud-failure-recorded routing)
    (unless (delib-flow--cloud-failure-resolution-valid-p keyword)
      (error "Cloud failure resolution must be one of RETRY-CLOUD, USE-LOCAL, SKIP-CLOUD, or ABORT"))
    (list :resolution keyword
          :operator-notes notes
          :retry-cloud-p (string-equal keyword "RETRY-CLOUD")
          :abort-p (string-equal keyword "ABORT")
          :fallback-mode fallback-mode
          :reintegration-status (when fallback-mode
                                  'approved)
          :reason (delib-flow--cloud-failure-resolution-reason keyword))))

(defun delib-flow--approve-candidate-reintegration-result (_package)
  "Return raw reintegration approval result for _PACKAGE."
  (list :reintegration-status 'approved
        :reason "Cloud-returned result is approved for local reintegration."))

(defun delib-flow--execute-approve-candidate-reintegration (package)
  "Return raw reintegration approval output for PACKAGE."
  (delib-flow--approve-candidate-reintegration-result package))

(defun delib-flow--cloud-returned-stage-id (working)
  "Return rerouted cloud target stage id recorded in WORKING."
  (plist-get working :cloud-returned-stage-id))

(defun delib-flow--cloud-returned-stage-raw-output (working)
  "Return rerouted cloud target raw output recorded in WORKING."
  (plist-get working :cloud-returned-stage-raw-output))

(defun delib-flow--cloud-returned-stage-normalized-output (working)
  "Return rerouted cloud target normalized output recorded in WORKING."
  (plist-get working :cloud-returned-stage-normalized-output))

(defun delib-flow--cloud-returned-draft-items (working)
  "Return rerouted cloud draft items recorded in WORKING, if any."
  (let ((stage-id (delib-flow--cloud-returned-stage-id working))
        (raw-output (delib-flow--cloud-returned-stage-raw-output working)))
    (cond
     ((eq stage-id 'extract-actions)
      (plist-get raw-output :actions))
     ((eq stage-id 'extract-waiting-for)
      (plist-get raw-output :waiting-fors))
     ((eq stage-id 'suggest-reference-notes)
      (plist-get raw-output :reference-notes))
     (t nil))))

(defun delib-flow--integration-context-parts (package)
  "Return retained context parts assembled from PACKAGE."
  (seq-remove
   #'string-empty-p
   (list
    (or (plist-get (plist-get package :working-context) :retained-context) "")
    (if-let ((cloud-output
              (plist-get (plist-get package :working-context)
                         :cloud-returned-context)))
        (format "Cloud-reviewed context\n%s" cloud-output)
      ""))))

(defun delib-flow--integrated-context-text (package)
  "Return integrated local context text from PACKAGE."
  (mapconcat #'identity
             (delib-flow--integration-context-parts package)
             "\n\n"))

(defun delib-flow--integrate-into-source-result (package)
  "Return raw integration result for PACKAGE."
  (let* ((working (plist-get package :working-context))
         (integrated-context (delib-flow--integrated-context-text package))
         (draft-items
          (or (delib-flow--cloud-returned-draft-items working)
              (plist-get (plist-get package :filing) :draft-items))))
    (list :integrated-context integrated-context
          :draft-count (length draft-items)
          :cloud-context-integrated-p
          (not (null (plist-get working :cloud-returned-context)))
          :reason "Integrated local context and draft filing artifacts are ready for review.")))

(defun delib-flow--execute-integrate-into-source (package)
  "Return raw integration output for PACKAGE."
  (delib-flow--integrate-into-source-result package))

(defun delib-flow--draft-items (package)
  "Return draft filing items from PACKAGE."
  (plist-get (plist-get package :filing) :draft-items))

(defun delib-flow--selected-filing-item (package)
  "Return the operator-selected filing item from PACKAGE."
  (let ((item (delib-flow--filing-selection-choice package)))
    (if (eq (plist-get item :kind) 'reference-note)
        (let ((selected-id
               (delib-flow--artifact-family-selected-candidate-id
                package
                'reference-notes))
              (draft (delib-flow--selected-reference-note-draft-from-package package)))
          (if (and draft
                   (equal selected-id
                          (delib-flow--artifact-candidate-id item)))
              draft
            item))
      item)))

(defun delib-flow--effective-approved-filing-item (package item)
  "Return ITEM resolved to the best approved filing artifact for PACKAGE."
  (if (eq (plist-get item :kind) 'reference-note)
      (let* ((selected-id
              (delib-flow--artifact-family-selected-candidate-id
               package
               'reference-notes))
             (draft (delib-flow--selected-reference-note-draft-from-package package)))
        (if (and draft
                 (equal (or (delib-flow--artifact-candidate-id item)
                            selected-id)
                        (or selected-id
                            (delib-flow--artifact-candidate-id draft))))
            draft
          item))
    item))

(defun delib-flow--selected-filing-items (package)
  "Return the operator-selected filing item from PACKAGE."
  (if-let ((item (delib-flow--selected-filing-item package)))
      (list item)
    nil))

(defun delib-flow--selected-filing-item-blocking-warnings (package)
  "Return blocking warnings on the operator-selected filing item from PACKAGE."
  (if-let ((item (delib-flow--selected-filing-item package)))
      (delib-flow--draft-item-blocking-warnings item)
    nil))

(defun delib-flow--ready-filing-selection-indexes (package)
  "Return ready filing-selection indexes from PACKAGE."
  (delib-flow--draft-item-selection-indexes
   (delib-flow--draft-items package)
   #'delib-flow--draft-item-ready-p))

(defun delib-flow--blocked-filing-selection-indexes (package)
  "Return blocked filing-selection indexes from PACKAGE."
  (delib-flow--draft-item-selection-indexes
   (delib-flow--draft-items package)
   (lambda (item)
     (not (delib-flow--draft-item-ready-p item)))))

(defun delib-flow--selected-filing-item-approval-blocked-p (package)
  "Return non-nil when the selected filing item in PACKAGE has blocking warnings."
  (not (null (delib-flow--selected-filing-item-blocking-warnings package))))

(defun delib-flow--remaining-draft-items (package)
  "Return unapproved draft filing items from PACKAGE."
  (if-let ((selected (delib-flow--selected-filing-item package)))
      (delib-flow--remove-first-matching-item
       (delib-flow--draft-items package)
       selected)
    (delib-flow--draft-items package)))

(defun delib-flow--rejected-filing-items (package)
  "Return the operator-selected filing item as rejected from PACKAGE."
  (if-let ((item (delib-flow--selected-filing-item package)))
      (list item)
    nil))

(defun delib-flow--approved-filing-item (package)
  "Return the current approved filing item from PACKAGE."
  (car (delib-flow--approved-items package)))

(defun delib-flow--rename-note-resolution-p (resolution)
  "Return non-nil when RESOLUTION requests note retargeting."
  (string-equal (delib-flow--filing-conflict-resolution-keyword resolution)
                "RENAME-NOTE"))

(defun delib-flow--reword-item-resolution-p (resolution)
  "Return non-nil when RESOLUTION requests project-child rewording."
  (string-equal (delib-flow--filing-conflict-resolution-keyword resolution)
                "REWORD-ITEM"))

(defun delib-flow--retitle-project-resolution-p (resolution)
  "Return non-nil when RESOLUTION requests project-title retargeting."
  (string-equal (delib-flow--filing-conflict-resolution-keyword resolution)
                "RETITLE-PROJECT"))

(defun delib-flow--reject-conflict-resolution-p (resolution)
  "Return non-nil when RESOLUTION rejects the approved artifact."
  (string-equal (delib-flow--filing-conflict-resolution-keyword resolution)
                "REJECT"))

(defun delib-flow--retry-conflict-resolution-p (resolution)
  "Return non-nil when RESOLUTION keeps the approved artifact for retry."
  (or (string-empty-p (or resolution ""))
      (string-equal (delib-flow--filing-conflict-resolution-keyword resolution)
                    "RETRY")))

(defun delib-flow--filing-conflict-resolution-keyword-or-default (resolution)
  "Return normalized filing conflict RESOLUTION keyword."
  (if (delib-flow--non-empty-string-p resolution)
      (delib-flow--filing-conflict-resolution-keyword resolution)
    "RETRY"))

(defun delib-flow--inferred-conflict-resolution-keyword (package keyword)
  "Return inferred filing conflict KEYWORD for PACKAGE when operator edits imply one.
This keeps the editable block forgiving when the operator changes the retarget
field but leaves the default `Resolution: RETRY` line untouched."
  (let* ((item (delib-flow--approved-filing-item package))
         (kind (plist-get item :kind))
         (has-title (delib-flow--non-empty-string-p
                     (delib-flow--filing-conflict-resolution-new-title package)))
         (has-text (delib-flow--non-empty-string-p
                    (delib-flow--filing-conflict-resolution-new-text package))))
    (if (not (string-equal keyword "RETRY"))
        keyword
      (cond
       ((and has-text (not has-title) (memq kind '(next-action waiting-for)))
        "REWORD-ITEM")
       ((and has-title (not has-text) (eq kind 'reference-note))
        "RENAME-NOTE")
       ((and has-title (not has-text) (eq kind 'project))
        "RETITLE-PROJECT")
       (t keyword)))))

(defun delib-flow--ensure-approved-item-kind (item kind message)
  "Signal MESSAGE unless ITEM exists and has KIND.
KIND may be a symbol or a list of symbols."
  (unless item
    (error "%s" message))
  (unless (if (listp kind)
              (memq (plist-get item :kind) kind)
            (eq (plist-get item :kind) kind))
    (error "%s" message)))

(defun delib-flow--conflict-resolution-required-title (package keyword)
  "Return required conflict-resolution title from PACKAGE for KEYWORD."
  (let ((new-title (delib-flow--filing-conflict-resolution-new-title package)))
    (unless (delib-flow--non-empty-string-p new-title)
      (error "%s requires a New title value" keyword))
    new-title))

(defun delib-flow--conflict-resolution-required-text (package keyword)
  "Return required conflict-resolution text from PACKAGE for KEYWORD."
  (let ((new-text (delib-flow--filing-conflict-resolution-new-text package)))
    (unless (delib-flow--non-empty-string-p new-text)
      (error "%s requires a New text value" keyword))
    new-text))

(defun delib-flow--ensure-conflict-resolution-unused-title (package keyword)
  "Signal when PACKAGE provides New title for KEYWORD unexpectedly."
  (when (delib-flow--non-empty-string-p
         (delib-flow--filing-conflict-resolution-new-title package))
    (error "%s does not accept a New title value" keyword)))

(defun delib-flow--ensure-conflict-resolution-unused-text (package keyword)
  "Signal when PACKAGE provides New text for KEYWORD unexpectedly."
  (when (delib-flow--non-empty-string-p
         (delib-flow--filing-conflict-resolution-new-text package))
    (error "%s does not accept a New text value" keyword)))

(defun delib-flow--validate-conflict-resolution-fields (package keyword)
  "Validate editable conflict-resolution fields in PACKAGE for KEYWORD."
  (cond
   ((member keyword '("RETRY" "REJECT"))
    (delib-flow--ensure-conflict-resolution-unused-title package keyword)
    (delib-flow--ensure-conflict-resolution-unused-text package keyword))
   ((member keyword '("RENAME-NOTE" "RETITLE-PROJECT"))
    (delib-flow--ensure-conflict-resolution-unused-text package keyword))
   ((string-equal keyword "REWORD-ITEM")
    (delib-flow--ensure-conflict-resolution-unused-title package keyword))))

(defun delib-flow--resolved-approved-item-rename-note (package item)
  "Return ITEM renamed from PACKAGE note conflict resolution."
  (delib-flow--ensure-approved-item-kind
   item 'reference-note
   "Rename-note resolution is only valid for approved reference notes")
  (delib-flow--reference-note-item-with-title
   item
   (delib-flow--conflict-resolution-required-title package
                                                   "Rename-note resolution")))

(defun delib-flow--resolved-approved-item-reword-item (package item)
  "Return ITEM reworded from PACKAGE project-child conflict resolution."
  (delib-flow--ensure-approved-item-kind
   item '(next-action waiting-for)
   "Reword-item resolution is only valid for approved project child artifacts")
  (delib-flow--project-child-item-with-text
   item
   (delib-flow--conflict-resolution-required-text package
                                                  "Reword-item resolution")))

(defun delib-flow--resolved-approved-item-retitle-project (package item)
  "Return ITEM retitled from PACKAGE project conflict resolution."
  (delib-flow--ensure-approved-item-kind
   item 'project
   "Retitle-project resolution is only valid for approved project artifacts")
  (delib-flow--project-item-with-title
   item
   (delib-flow--conflict-resolution-required-title package
                                                   "Retitle-project resolution")))

(defun delib-flow--resolved-approved-item-handler (keyword)
  "Return approved-item rewrite handler for conflict resolution KEYWORD."
  (alist-get keyword
             '(("RENAME-NOTE" . delib-flow--resolved-approved-item-rename-note)
               ("REWORD-ITEM" . delib-flow--resolved-approved-item-reword-item)
               ("RETITLE-PROJECT" . delib-flow--resolved-approved-item-retitle-project))
             nil nil #'string=))

(defun delib-flow--resolved-approved-item (package)
  "Return approved item from PACKAGE after conflict resolution."
  (let* ((keyword
          (delib-flow--inferred-conflict-resolution-keyword
           package
           (delib-flow--filing-conflict-resolution-keyword-or-default
            (delib-flow--filing-conflict-resolution-value package))))
         (item (delib-flow--approved-filing-item package))
         (handler (delib-flow--resolved-approved-item-handler keyword)))
    (if handler
        (funcall handler package item)
      item)))

(defun delib-flow--filing-conflict-resolution-valid-p (keyword)
  "Return non-nil when filing conflict KEYWORD is supported."
  (member keyword '("RETRY" "REJECT" "RENAME-NOTE" "REWORD-ITEM" "RETITLE-PROJECT")))

(defun delib-flow--filing-conflict-resolution-reason (keyword)
  "Return operator-facing explanation for filing conflict KEYWORD."
  (alist-get
   keyword
   '(("REJECT" . "Removed the approved artifact from the filing queue after conflict review.")
     ("RENAME-NOTE" . "Retargeted the approved note by updating its deterministic title. Retry filing when ready.")
     ("REWORD-ITEM" . "Retargeted the approved project child by updating its heading text. Retry filing when ready.")
     ("RETITLE-PROJECT" . "Retargeted the approved project by updating its deterministic title. Retry filing when ready.")
     ("RETRY" . "Cleared recorded conflict state while keeping the approved artifact available for retry after target correction."))
   nil nil #'string=))

(defun delib-flow--filing-conflict-approved-count (rejected-p resolved-item)
  "Return approved artifact count from REJECTED-P and RESOLVED-ITEM."
  (if rejected-p
      0
    (length (delq nil (list resolved-item)))))

(defun delib-flow--filing-conflict-rejected-items (rejected-p approved-item)
  "Return rejected artifact list from REJECTED-P and APPROVED-ITEM."
  (when rejected-p
    (list approved-item)))

(defun delib-flow--resolve-filing-conflict-result (package)
  "Return raw conflict-resolution result for PACKAGE."
  (let* ((keyword
          (delib-flow--inferred-conflict-resolution-keyword
           package
           (delib-flow--filing-conflict-resolution-keyword-or-default
            (delib-flow--filing-conflict-resolution-value package))))
         (notes (delib-flow--filing-conflict-resolution-notes package))
         (approved-item (delib-flow--approved-filing-item package))
         (resolved-item nil)
         (rejected-p (string-equal keyword "REJECT"))
         (rejected-items
          (delib-flow--filing-conflict-rejected-items rejected-p approved-item)))
    (unless (plist-get (plist-get package :filing) :conflicts)
      (error "Resolve Filing Conflict requires a recorded filing conflict"))
    (unless (delib-flow--filing-conflict-resolution-valid-p keyword)
      (error "Conflict resolution must be one of RETRY, REJECT, RENAME-NOTE, REWORD-ITEM, or RETITLE-PROJECT"))
    (delib-flow--validate-conflict-resolution-fields package keyword)
    (setq resolved-item (delib-flow--resolved-approved-item package))
    (list :resolution keyword
          :operator-notes notes
          :resolved-approved-item (unless rejected-p resolved-item)
          :rejected-items rejected-items
          :approved-count
          (delib-flow--filing-conflict-approved-count rejected-p resolved-item)
          :rejected-count (length rejected-items)
          :renamed-p (string-equal keyword "RENAME-NOTE")
          :retry-p (string-equal keyword "RETRY")
          :reason (delib-flow--filing-conflict-resolution-reason keyword))))

(defun delib-flow--select-approved-filing-actions-state (package)
  "Return derived filing-selection state from PACKAGE."
  (let* ((selected-item (delib-flow--selected-filing-item package))
         (blocking-warnings
          (delib-flow--selected-filing-item-blocking-warnings package))
         (approval-blocked-p (not (null blocking-warnings))))
    (list :selected-item selected-item
          :blocking-warnings blocking-warnings
          :approval-blocked-p approval-blocked-p
          :selected-items (unless approval-blocked-p
                            (delib-flow--selected-filing-items package))
          :remaining-items (if approval-blocked-p
                               (delib-flow--draft-items package)
                             (delib-flow--remaining-draft-items package)))))

(defun delib-flow--select-approved-filing-actions-result (package)
  "Return raw filing-selection result for PACKAGE."
  (let* ((selection (delib-flow--filing-selection-value package))
         (notes (delib-flow--filing-selection-notes package))
         (state (delib-flow--select-approved-filing-actions-state package))
         (selected-item (plist-get state :selected-item))
         (blocking-warnings (plist-get state :blocking-warnings))
         (ready-indexes (delib-flow--ready-filing-selection-indexes package))
         (blocked-indexes (delib-flow--blocked-filing-selection-indexes package))
         (approval-blocked-p (plist-get state :approval-blocked-p))
         (selected-items (plist-get state :selected-items))
         (remaining-items (plist-get state :remaining-items))
         (planned-target-locations
          (unless approval-blocked-p
            (condition-case nil
                (mapcan (lambda (item)
                          (delib-flow--planned-file-location item package))
                        selected-items)
              (error nil)))))
    (list :approved-items selected-items
          :approval-blocked-p approval-blocked-p
          :blocking-warnings blocking-warnings
          :ready-selection-indexes ready-indexes
          :blocked-selection-indexes blocked-indexes
          :remaining-draft-items remaining-items
          :selected-count (length selected-items)
          :remaining-draft-count (length remaining-items)
          :operator-selection selection
          :operator-notes notes
          :blocked-item (and approval-blocked-p selected-item)
          :blocked-item-preview
          (and selected-item
               approval-blocked-p
               (delib-flow--draft-item-preview-text (list selected-item)))
          :selected-preview (and selected-items
                                 (delib-flow--draft-item-preview-text
                                  selected-items))
          :planned-target-locations planned-target-locations
          :reason
          (if approval-blocked-p
              "Selected filing artifact still has blocking warnings and cannot be approved yet. Approve a different ready artifact, fix the blocking warnings, or reject this artifact before retrying approval."
            "Approved the operator-selected filing artifact for deterministic filing review."))))

(defun delib-flow--execute-select-approved-filing-actions (package)
  "Return raw filing-selection output for PACKAGE."
  (delib-flow--select-approved-filing-actions-result package))

(defun delib-flow--execute-resolve-filing-conflict (package)
  "Return raw conflict-resolution output for PACKAGE."
  (delib-flow--resolve-filing-conflict-result package))

(defun delib-flow--execute-resolve-cloud-failure (package)
  "Return raw cloud-failure resolution output for PACKAGE."
  (delib-flow--resolve-cloud-failure-result package))

(defun delib-flow--reject-draft-filing-artifact-result (package)
  "Return raw filing-rejection result for PACKAGE."
  (let* ((selection (delib-flow--filing-selection-value package))
         (notes (delib-flow--filing-selection-notes package))
         (rejected-items (delib-flow--rejected-filing-items package))
         (remaining-items (delib-flow--remaining-draft-items package)))
    (list :rejected-items rejected-items
          :remaining-draft-items remaining-items
          :rejected-count (length rejected-items)
          :remaining-draft-count (length remaining-items)
          :operator-selection selection
          :operator-notes notes
          :rejected-preview (and rejected-items
                                 (delib-flow--draft-item-preview-text
                                  rejected-items))
          :reason "Rejected the operator-selected filing artifact while leaving the remaining draft queue active.")))

(defun delib-flow--execute-reject-draft-filing-artifact (package)
  "Return raw filing-rejection output for PACKAGE."
  (delib-flow--reject-draft-filing-artifact-result package))

(defun delib-flow--approved-items (package)
  "Return approved filing items from PACKAGE."
  (mapcar (lambda (item)
            (delib-flow--effective-approved-filing-item package item))
          (plist-get (plist-get package :filing) :approved-items)))

(defun delib-flow--matched-project-title (package)
  "Return matched project title from PACKAGE."
  (plist-get (plist-get (plist-get (plist-get package :working-context)
                                   :project-match)
                        :best-project)
             :title))

(defun delib-flow--matched-project (package)
  "Return matched project plist from PACKAGE."
  (plist-get (plist-get (plist-get package :working-context)
                        :project-match)
             :best-project))

(defun delib-flow--project-item-keyword (item)
  "Return Org keyword for approved ITEM."
  (if (eq (plist-get item :kind) 'waiting-for)
      "WAITING"
    "TODO"))

(defun delib-flow--project-item-heading (item level)
  "Return Org heading text for ITEM at LEVEL."
  (format "%s %s %s\n"
          (make-string level ?*)
          (delib-flow--project-item-keyword item)
          (plist-get item :text)))

(defun delib-flow--project-heading-text (item &optional level)
  "Return Org heading text for project ITEM at LEVEL."
  (format "%s %s\n"
          (make-string (or level 1) ?*)
          (plist-get item :title)))

(defun delib-flow--project-first-item (item)
  "Return first child item from project ITEM."
  (plist-get item :first-item))

(defvar delib-flow--capture-context nil
  "Dynamic capture context used while rendering delib-flow capture templates.")

(defun delib-flow-capture-item-text ()
  "Return current capture item text."
  (plist-get delib-flow--capture-context :item-text))

(defun delib-flow-capture-project-title ()
  "Return current capture project title."
  (plist-get delib-flow--capture-context :project-title))

(defun delib-flow-capture-note-title ()
  "Return current capture note title."
  (plist-get delib-flow--capture-context :note-title))

(defun delib-flow-capture-note-type ()
  "Return current capture note type."
  (plist-get delib-flow--capture-context :note-type))

(defun delib-flow-capture-source-artifact ()
  "Return current capture source artifact text."
  (plist-get delib-flow--capture-context :source-artifact))

(defun delib-flow-capture-project-child-heading ()
  "Return current capture project-child heading."
  (plist-get delib-flow--capture-context :project-child-heading))

(defun delib-flow-capture-project-heading ()
  "Return current capture project heading."
  (plist-get delib-flow--capture-context :project-heading))

(defun delib-flow-capture-project-first-item-heading ()
  "Return current capture new-project first child heading."
  (plist-get delib-flow--capture-context :project-first-item-heading))

(defun delib-flow-capture-project-tags ()
  "Return current capture project tags as a space-separated string."
  (string-join
   (or (plist-get delib-flow--capture-context :project-tags) '())
   " "))

(defun delib-flow-capture-project-tags-property ()
  "Return capture property lines for current project tags."
  (if-let ((tags (plist-get delib-flow--capture-context :project-tags)))
      (if tags
          (format ":PROPERTIES:\n:TAGS: %s\n" (string-join tags " "))
        "")
    ""))

(defun delib-flow--capture-template-interactive-p (template)
  "Return non-nil when capture TEMPLATE would prompt interactively."
  (or (string-match-p "%\\^" template)
      (string-match-p "%\\?" template)))

(defun delib-flow--validate-capture-template (template)
  "Signal an error when capture TEMPLATE is unsupported for delib-flow filing."
  (when (delib-flow--capture-template-interactive-p template)
    (error "delib-flow filing capture templates must be non-interactive; remove %%^ prompts and %%? cursor markers")))

(defun delib-flow--capture-template-context (item package &optional project-title level)
  "Return dynamic capture context for ITEM from PACKAGE.
PROJECT-TITLE and LEVEL describe deterministic project filing targets when
relevant."
  (list :item item
        :package package
        :item-text (plist-get item :text)
        :project-title (or project-title
                           (delib-flow--matched-project-title package)
                           (plist-get item :title))
        :note-title (and (eq (plist-get item :kind) 'reference-note)
                         (delib-flow--reference-note-title item))
        :note-type (and (eq (plist-get item :kind) 'reference-note)
                        (symbol-name (plist-get item :note-type)))
        :source-artifact (plist-get item :text)
        :project-child-heading
        (and level
             (not (eq (plist-get item :kind) 'project))
             (string-trim-right
              (delib-flow--project-item-heading item level)))
        :project-tags
        (and (eq (plist-get item :kind) 'project)
             (or (plist-get item :tags)
                 (plist-get item :tag-suggestions)))
        :project-heading
        (and (eq (plist-get item :kind) 'project)
             (string-trim-right
              (delib-flow--project-heading-text item (or level 1))))
        :project-first-item-heading
        (and (eq (plist-get item :kind) 'project)
             (let ((first (delib-flow--project-first-item item)))
               (when first
                 (string-trim-right
                  (delib-flow--project-item-heading first (1+ (or level 1)))))))))

(defun delib-flow--fill-capture-template (template context)
  "Return capture TEMPLATE expanded against CONTEXT."
  (delib-flow--validate-capture-template template)
  (let ((delib-flow--capture-context context)
        (org-capture-plist (list :default-time (current-time)))
        (org-store-link-plist nil))
    (org-capture-fill-template template)))

(defun delib-flow--project-item-capture-template (item)
  "Return configured capture template for project filing ITEM."
  (pcase (plist-get item :kind)
    ('project delib-flow-project-capture-template)
    ('waiting-for delib-flow-waiting-for-capture-template)
    (_ delib-flow-next-action-capture-template)))

(defun delib-flow--custom-default-value (symbol)
  "Return declared default value for customization SYMBOL."
  (eval (car (get symbol 'standard-value))))

(defun delib-flow--using-default-setting-p (symbol)
  "Return non-nil when SYMBOL still has its declared default value."
  (equal (symbol-value symbol)
         (delib-flow--custom-default-value symbol)))

(defun delib-flow--org-file-buffer (file)
  "Return visiting buffer for Org FILE, creating one when needed."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-mode)
        (org-mode)))
    buffer))

(defun delib-flow--stage-org-file-edit (file edit-fn)
  "Apply EDIT-FN to Org FILE in its visiting buffer without saving."
  (with-current-buffer (delib-flow--org-file-buffer file)
    (save-excursion
      (save-restriction
        (widen)
        (funcall edit-fn)
        (set-buffer-modified-p t)))))

(defun delib-flow--insert-project-child (file project-title item)
  "Insert ITEM as child under PROJECT-TITLE in Org FILE without saving."
  (delib-flow--stage-org-file-edit
   file
   (lambda ()
     (let ((level (1+ (delib-flow--matched-project-point project-title))))
       (org-end-of-subtree t t)
       (unless (bolp)
         (insert "\n"))
       (insert
        (delib-flow--fill-capture-template
         (delib-flow--project-item-capture-template item)
         (delib-flow--capture-template-context item nil project-title level)))
       (unless (bolp)
         (insert "\n"))))))

(defun delib-flow--project-state-bucket-title (state)
  "Return top-level bucket title for project STATE."
  (pcase state
    ('waiting "Waiting")
    ('complete "Complete")
    (_ "Active")))

(defun delib-flow--project-state-bucket-level (file state)
  "Return insertion level for a new project with STATE in FILE."
  (with-current-buffer (delib-flow--org-file-buffer file)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (format "^\\* %s$"
                   (regexp-quote
                    (delib-flow--project-state-bucket-title state)))
           nil t)
          2
        1))))

(defun delib-flow--goto-project-state-bucket (state)
  "Move point to the top-level bucket heading for project STATE."
  (goto-char (point-min))
  (unless (re-search-forward
           (format "^\\* %s$"
                   (regexp-quote
                    (delib-flow--project-state-bucket-title state)))
           nil t)
    (error "Configured project state bucket is not present"))
  (beginning-of-line))

(defun delib-flow--insert-new-project (file item)
  "Insert new project ITEM into Org FILE without saving."
  (delib-flow--stage-org-file-edit
   file
   (lambda ()
     (let ((level (delib-flow--project-state-bucket-level
                   file
                   (plist-get item :state))))
       (if (= level 2)
           (progn
             (delib-flow--goto-project-state-bucket (plist-get item :state))
             (org-end-of-subtree t t))
         (goto-char (point-max)))
       (unless (bolp)
         (insert "\n"))
       (insert
        (delib-flow--fill-capture-template
         (delib-flow--project-item-capture-template item)
         (delib-flow--capture-template-context
          item nil (plist-get item :title) level)))
       (unless (bolp)
         (insert "\n"))))))

(defun delib-flow--slugify (text)
  "Return a filesystem slug for TEXT."
  (let* ((downcased (downcase (or text "")))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" downcased)))
    (string-trim slug "-+" "-+")))

(defun delib-flow--reference-note-title (item)
  "Return deterministic note title from approved ITEM."
  (cond
   ((eq (plist-get item :note-type) 'general-pkm)
    (string-remove-prefix "Create general PKM note for "
                          (plist-get item :text)))
   ((eq (plist-get item :note-type) 'project-support)
    (string-remove-prefix "Create project support note from "
                          (plist-get item :text)))
   (t
    (plist-get item :text))))

(defun delib-flow--selected-reference-note-candidate-from-package (package)
  "Return selected reference-note candidate from PACKAGE, or nil."
  (let* ((artifacts (plist-get package :artifacts))
         (state (plist-get artifacts 'reference-notes))
         (selected-id (plist-get state :selected-candidate-id))
         (selected-item (condition-case nil
                            (delib-flow--filing-selection-choice package)
                          (error nil)))
         (candidates (plist-get state :candidates)))
    (cond
     ((eq (plist-get selected-item :kind) 'reference-note)
      selected-item)
     (selected-id
      (seq-find
       (lambda (item)
         (equal (delib-flow--artifact-candidate-id item)
                selected-id))
       candidates))
     (t nil))))

(defun delib-flow--selected-reference-note-draft-from-package (package)
  "Return selected drafted reference-note item from PACKAGE, or nil."
  (plist-get (plist-get (plist-get package :artifacts) 'reference-notes)
             :selected-draft))

(defun delib-flow--reference-note-preview-item (package)
  "Return active reference-note ITEM from PACKAGE for capture review, or nil."
  (let ((approved (car (delib-flow--approved-items package)))
        (preview (car (delib-flow--planned-file-preview-items package)))
        (selected-draft (delib-flow--selected-reference-note-draft-from-package package))
        (selected-candidate (delib-flow--selected-reference-note-candidate-from-package package)))
    (cond
     ((eq (plist-get selected-draft :kind) 'reference-note) selected-draft)
     ((eq (plist-get selected-candidate :kind) 'reference-note) selected-candidate)
     ((eq (plist-get approved :kind) 'reference-note) approved)
     ((eq (plist-get preview :kind) 'reference-note) preview)
     (t nil))))

(defun delib-flow--note-draft-items (run)
  "Return reference-note draft items from RUN."
  (seq-filter
   (lambda (item)
     (eq (plist-get item :kind) 'reference-note))
   (plist-get (plist-get run :filing) :draft-items)))

(defun delib-flow--reference-note-draft-preview-items (run)
  "Return reference-note items that should be previewed in filing for RUN."
  (or
   (when-let ((draft (delib-flow--artifact-family-selected-draft
                      run
                      'reference-notes)))
     (when (eq (plist-get draft :kind) 'reference-note)
       (list draft)))
   (when-let ((selected (delib-flow--selected-reference-note-candidate-for-drafting run)))
     (when (eq (plist-get selected :kind) 'reference-note)
       (list selected)))
   (let ((planned (delib-flow--planned-file-preview-items run)))
     (when (and planned
                (= (length planned) 1)
                (eq (plist-get (car planned) :kind) 'reference-note))
       planned))
   (seq-take (delib-flow--note-draft-items run) 2)))

(defun delib-flow--reference-note-draft-preview-status (run)
  "Return status text for reference-note draft previews in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'reference-notes)
    "This is the current drafted body for the selected note. Review it before approval or filing.")
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    "This is the selected note candidate. Draft it first if you want delib-flow to enrich the note before approval.")
   ((delib-flow--reference-note-draft-preview-items run)
    "Review these note candidates before selecting one to draft.")
   (t
    "No reference-note draft previews are available from the current queue.")))

(defun delib-flow--reference-note-draft-preview-text (run)
  "Return filing-loop reference-note draft preview text for RUN."
  (if-let ((items (delib-flow--reference-note-draft-preview-items run)))
      (let ((package (delib-flow--stage-input-package run 'draft-selected-reference-note)))
        (mapconcat
         (lambda (item)
           (delib-flow--reference-note-preview-fragment item package))
         items
         "\n\n"))
    "Run Suggest Reference Notes to generate draft note bodies here."))

(defun delib-flow--reference-note-regeneration-status (run)
  "Return status text for note-draft regeneration guidance in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'reference-notes)
    "Use this when the selected note draft above is weak, too generic, or pointed at the wrong concept.")
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    "Draft the selected note before you approve or file it.")
   (t
    "No note-draft regeneration guidance is needed right now.")))

(defun delib-flow--reference-note-regeneration-text (run)
  "Return note-draft regeneration guidance text for RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'reference-notes)
    (string-join
     '("- Run `Regenerate Selected Note` to ask the LLM for a fresh pass on this note only."
       "- This replaces only the selected note draft. It does not replace the rest of the note queue."
       "- If you want different supporting material first, run `Filter Useful Reference Material` or `Discover Relevant Reference Material`, then regenerate the selected note.")
     "\n"))
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    (string-join
     '("- Run `Draft Selected Note` to turn this selected note candidate into a fuller draft."
       "- That draft becomes the version delib-flow will preview, approve, and file for this selected note."
       "- If this is the wrong note to deepen, choose a different note in the queue first.")
     "\n"))
   ((delib-flow--reference-note-draft-preview-items run)
    (string-join
     '("- Choose one note candidate in the queue first."
       "- After selection, use `Draft Selected Note` to deepen only that note instead of regenerating the whole queue.")
     "\n"))
   (t
    "No reference-note drafts are currently active.")))

(defun delib-flow--reference-note-capture-template-options (&optional item)
  "Return completion labels and keys for reference-note org-roam templates."
  (let* ((templates (and (boundp 'org-roam-capture-templates)
                         org-roam-capture-templates))
         (default-key (and item
                           (delib-flow--reference-note-org-roam-template-key item)))
         options)
    (dolist (template templates (nreverse options))
      (when (and (consp template)
                 (stringp (car template)))
        (push (cons (format "%s - %s%s"
                            (car template)
                            (or (nth 1 template) "Unnamed template")
                            (if (equal (car template) default-key)
                                " (default)"
                              ""))
                    (car template))
              options)))))

(defun delib-flow--reference-note-effective-template-key (item package)
  "Return effective org-roam template key for reference-note ITEM in PACKAGE."
  (or (let ((value (delib-flow--reference-note-capture-template-key-value package)))
        (and (delib-flow--non-empty-string-p value) value))
      (delib-flow--reference-note-org-roam-template-key item)))

(defun delib-flow--reference-note-effective-title (item package)
  "Return effective capture title for reference-note ITEM in PACKAGE."
  (or (let ((value (delib-flow--reference-note-capture-title-value package)))
        (and (delib-flow--non-empty-string-p value) value))
      (delib-flow--reference-note-title item)))

(defun delib-flow--reference-note-effective-target-override (package)
  "Return effective target-path override from PACKAGE, or nil."
  (let ((value (delib-flow--reference-note-capture-target-path-value package)))
    (and (delib-flow--non-empty-string-p value) value)))

(defun delib-flow--normalize-reference-note-target-override (path)
  "Return normalized relative reference-note target override PATH."
  (let ((trimmed (string-trim (or path ""))))
    (when (delib-flow--non-empty-string-p trimmed)
      (if (string-match-p "\\.org\\'" trimmed)
          trimmed
        (concat trimmed ".org")))))

(defun delib-flow--reference-note-default-file (item)
  "Return fallback deterministic note file path for approved ITEM."
  (unless (and delib-flow-zk-root
               (file-directory-p delib-flow-zk-root))
    (error "The ZK root is not configured or readable"))
  (expand-file-name
   (format "%s.org" (delib-flow--slugify
                     (delib-flow--reference-note-title item)))
   delib-flow-zk-root))

(defun delib-flow--reference-note-org-roam-template-key (item)
  "Return configured org-roam capture key for reference-note ITEM, or nil."
  (if (eq (plist-get item :note-type) 'project-support)
      delib-flow-project-support-note-org-roam-capture-key
    delib-flow-general-note-org-roam-capture-key))

(defun delib-flow--org-roam-directory-root ()
  "Return the current org-roam root directory, if configured."
  (let ((root (or (and (boundp 'org-roam-directory) org-roam-directory)
                  delib-flow-zk-root)))
    (when root
      (file-name-as-directory (expand-file-name root)))))

(defun delib-flow--ensure-org-roam-load-path ()
  "Attempt to add a local org-roam package directory to `load-path'."
  (let ((root (expand-file-name "~/.emacs.d/elpa/")))
    (when (file-directory-p root)
      (dolist (dir (directory-files root t "^[^.]" t))
        (when (file-directory-p dir)
          (add-to-list 'load-path dir))))))

(defun delib-flow--org-roam-template-entry (key)
  "Return org-roam capture template entry for KEY, or nil."
  (when (and key (boundp 'org-roam-capture-templates))
    (seq-find (lambda (template)
                (equal (car template) key))
              org-roam-capture-templates)))

(defun delib-flow--reference-note-org-roam-template (item &optional package)
  "Return org-roam capture template entry configured for reference-note ITEM."
  (when-let ((key (if package
                      (delib-flow--reference-note-effective-template-key
                       item package)
                    (delib-flow--reference-note-org-roam-template-key item))))
    (or (delib-flow--org-roam-template-entry key)
        (error "Configured org-roam capture template key not found: %s" key))))

(defun delib-flow--reference-note-org-roam-context (item package)
  "Return org-roam expansion context for reference-note ITEM from PACKAGE."
  (let ((title (delib-flow--reference-note-effective-title item package)))
    (list :title title
          :slug (delib-flow--slugify title)
          :default-time (current-time)
          :capture-context
          (delib-flow--capture-template-context
           item package (delib-flow--matched-project-title package) nil))))

(defun delib-flow--fill-org-roam-template-fallback (template context)
  "Return org-roam-style TEMPLATE expanded against CONTEXT without org-roam."
  (let* ((rendered template)
         (title (plist-get context :title))
         (slug (plist-get context :slug))
         (delib-flow--capture-context (plist-get context :capture-context))
         (org-capture-plist (list :default-time
                                  (plist-get context :default-time))))
    (setq rendered
          (replace-regexp-in-string "\\${title}" (or title "") rendered t t))
    (setq rendered
          (replace-regexp-in-string "\\${slug}" (or slug "") rendered t t))
    (replace-regexp-in-string "[\n]*\\'" "" (org-capture-fill-template rendered))))

(defun delib-flow--strip-org-capture-point-markers (text)
  "Return TEXT without interactive org-capture point markers."
  (replace-regexp-in-string "%\\?" "" (or text "") t t))

(defun delib-flow--fill-org-roam-template (template context &optional ensure-newline)
  "Return org-roam TEMPLATE expanded against CONTEXT.

When ENSURE-NEWLINE is non-nil, ensure the rendered text ends in a newline."
  (when (functionp template)
    (error "delib-flow org-roam filing does not support interactive template functions"))
  (unless (stringp template)
    (error "delib-flow org-roam filing requires string-based templates"))
  (let* ((rendered
          (if (and (require 'org-roam-capture nil t)
                   (require 'org-roam-node nil t)
                   (fboundp 'org-roam-capture--fill-template)
                   (fboundp 'org-roam-node-create))
              (let ((delib-flow--capture-context (plist-get context :capture-context))
                    (org-capture-plist (list :default-time
                                             (plist-get context :default-time)))
                    (org-roam-capture--node
                     (org-roam-node-create :title (plist-get context :title)))
                    (org-roam-capture--info
                     (list :title (plist-get context :title)
                           :slug (plist-get context :slug))))
                (org-roam-capture--fill-template template ensure-newline))
            (let ((fallback (delib-flow--fill-org-roam-template-fallback
                             template context)))
              (if ensure-newline
                  (concat fallback "\n")
                fallback)))))
    (delib-flow--strip-org-capture-point-markers rendered)))

(defun delib-flow--reference-note-org-roam-target-path (path context)
  "Return absolute org-roam target PATH expanded against CONTEXT."
  (let* ((rendered (delib-flow--fill-org-roam-template path context))
         (root (delib-flow--org-roam-directory-root)))
    (unless root
      (error "The org-roam directory is not configured"))
    (if (file-name-absolute-p rendered)
        (expand-file-name rendered)
      (expand-file-name rendered root))))

(defun delib-flow--reference-note-org-roam-stage-plan (item package)
  "Return staged org-roam filing plan for reference-note ITEM from PACKAGE."
  (when-let* ((template (delib-flow--reference-note-org-roam-template
                         item package))
              (target-spec (or (plist-get (nthcdr 4 template) :if-new)
                               (plist-get (nthcdr 4 template) :target))))
    (let* ((context (delib-flow--reference-note-org-roam-context item package))
           (override (delib-flow--normalize-reference-note-target-override
                      (delib-flow--reference-note-effective-target-override
                       package)))
           (body-template (nth 3 template))
           (body (delib-flow--fill-org-roam-template body-template context)))
      (pcase target-spec
        (`(file+head ,path ,head)
         (let* ((resolved-path
                 (cond
                  (override override)
                  ((stringp path) (delib-flow--reference-note-org-roam-target-path
                                   path context))
                  ((functionp path)
                   (error "The selected org-roam template requires a target path. Fill `Target path:` before filing this note."))
                  (t
                   (error "Unsupported org-roam target path in template"))))
                (target (if (file-name-absolute-p resolved-path)
                            resolved-path
                          (expand-file-name
                           resolved-path
                           (or (delib-flow--org-roam-directory-root)
                               default-directory)))))
           (list :target target
                 :title (plist-get context :title)
                 :context context
                 :body-template body-template
                 :target-spec `(file+head ,target ,head)
                 :content (concat (delib-flow--fill-org-roam-template head context t)
                                  body))))
        (`(file ,path)
         (let* ((resolved-path
                 (cond
                  (override override)
                  ((stringp path) (delib-flow--reference-note-org-roam-target-path
                                   path context))
                  ((functionp path)
                   (error "The selected org-roam template requires a target path. Fill `Target path:` before filing this note."))
                  (t
                   (error "Unsupported org-roam target path in template"))))
                (target (if (file-name-absolute-p resolved-path)
                            resolved-path
                          (expand-file-name
                           resolved-path
                           (or (delib-flow--org-roam-directory-root)
                               default-directory)))))
           (list :target target
                 :title (plist-get context :title)
                 :context context
                 :body-template body-template
                 :target-spec `(file ,target)
                 :content body)))
        (_
         (error "delib-flow org-roam filing supports only file and file+head targets"))))))

(defun delib-flow--reference-note-org-roam-target-requires-path-p (item package)
  "Return non-nil when ITEM's selected org-roam template requires a path override."
  (when-let* ((template (delib-flow--reference-note-org-roam-template item package))
              (target-spec (or (plist-get (nthcdr 4 template) :if-new)
                               (plist-get (nthcdr 4 template) :target))))
    (pcase target-spec
      (`(file+head ,path ,_) (functionp path))
      (`(file ,path) (functionp path))
      (_ nil))))

(defun delib-flow--reference-note-file (item &optional package)
  "Return target file path for approved reference-note ITEM from PACKAGE."
  (if-let ((plan (and package
                      (delib-flow--reference-note-org-roam-stage-plan item package))))
      (plist-get plan :target)
    (delib-flow--reference-note-default-file item)))

(defun delib-flow--reference-note-template (item)
  "Return configured note template for approved ITEM."
  (if (eq (plist-get item :note-type) 'project-support)
      (if (and (delib-flow--using-default-setting-p
                'delib-flow-project-support-note-capture-template)
               (not (delib-flow--using-default-setting-p
                     'delib-flow-project-support-note-template)))
          delib-flow-project-support-note-template
        delib-flow-project-support-note-capture-template)
    (if (and (delib-flow--using-default-setting-p
              'delib-flow-general-note-capture-template)
             (not (delib-flow--using-default-setting-p
                   'delib-flow-general-note-template)))
        delib-flow-general-note-template
      delib-flow-general-note-capture-template)))

(defun delib-flow--reference-note-template-has-title-p (item)
  "Return non-nil when approved reference-note ITEM template expands a title."
  (let ((template (or (when-let ((org-roam-template
                                  (delib-flow--reference-note-org-roam-template item)))
                        (pcase (or (plist-get (nthcdr 4 org-roam-template) :if-new)
                                   (plist-get (nthcdr 4 org-roam-template) :target))
                          (`(file+head ,_ ,head) head)
                          (_ (nth 3 org-roam-template))))
                      (delib-flow--reference-note-template item))))
    (or (string-match-p "\\${title}" template)
        (string-match-p "delib-flow-capture-note-title" template))))

(defun delib-flow--reference-note-template-bindings (item)
  "Return template bindings for approved ITEM."
  (list (cons "${title}" (delib-flow--reference-note-title item))
        (cons "${source-artifact}" (plist-get item :text))
        (cons "${note-type}" (symbol-name (plist-get item :note-type)))))

(defun delib-flow--reference-note-body-lines (content)
  "Return significant body lines from note CONTENT."
  (with-temp-buffer
    (insert (or content ""))
    (goto-char (point-min))
    (while (looking-at "^#\\+.*\n")
      (forward-line 1))
    (when (looking-at "^:PROPERTIES:\n")
      (when (re-search-forward "^:END:\n?" nil t)
        (goto-char (match-end 0))))
    (seq-filter
     (lambda (line)
       (let ((trimmed (string-trim line)))
         (and (not (string-empty-p trimmed))
              (not (string-match-p "\\`#\\+" trimmed))
              (not (string-match-p "\\`:\\(?:PROPERTIES\\|END\\|ID\\):" trimmed)))))
     (split-string (buffer-substring-no-properties (point) (point-max)) "\n"))))

(defun delib-flow--reference-note-content-needs-seed-p (content)
  "Return non-nil when note CONTENT should receive seeded structure."
  (< (length (delib-flow--reference-note-body-lines content)) 3))

(defun delib-flow--reference-note-title-keywords (item)
  "Return significant title keywords for reference-note ITEM."
  (seq-filter
   (lambda (word)
     (and (>= (length word) 4)
          (not (member (downcase word) delib-flow--tag-suggestion-stopwords))))
   (delib-flow--string-words (delib-flow--reference-note-title item))))

(defun delib-flow--reference-note-draft-body-grounded-p (draft-body item)
  "Return non-nil when DRAFT-BODY is grounded in reference-note ITEM focus."
  (let* ((title-words (delib-flow--reference-note-title-keywords item))
         (body-words (delib-flow--string-words (downcase (or draft-body "")))))
    (or (null title-words)
        (seq-some (lambda (word)
                    (member (downcase word) body-words))
                  title-words))))

(defun delib-flow--reference-note-clean-line (line)
  "Return LINE normalized for reference-note drafting."
  (let ((text (string-trim (or line ""))))
    (setq text
          (replace-regexp-in-string
           "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1" text))
    (setq text
          (replace-regexp-in-string
           "https?://[^][()<>[:space:]\"]+" "" text))
    (setq text
          (replace-regexp-in-string
           "\\(?:/[^[:space:]]+\\)\\(?:\\.org\\|\\.txt\\|\\.md\\)\\b" "" text))
    (setq text
          (replace-regexp-in-string "[[:space:]]+" " " text))
    (string-trim text "[[:space:][:punct:]]*" "[[:space:][:punct:]]*")))

(defun delib-flow--reference-note-useful-source-line-p (line)
  "Return non-nil when cleaned source LINE is useful for note drafting."
  (let ((text (delib-flow--reference-note-clean-line line)))
    (and (not (string-empty-p text))
         (string-match-p "[[:alnum:]]" text)
         (not (string-match-p "\\`[()\\[\\]{}]+\\'" text))
         (not (string-match-p "\\`https?:" (downcase text)))
         (not (string-match-p
               "\\`\\(?:From\\|To\\|Cc\\|Bcc\\|Subject\\|Date\\|Reply-To\\):"
               text))
         (not (string-match-p
               "\\breferenced materials?\\b"
               (downcase text))))))

(defun delib-flow--reference-note-focus-heading (item)
  "Return focus heading text for reference-note ITEM, or nil."
  (when-let ((title (delib-flow--reference-note-title item)))
    (let* ((base (replace-regexp-in-string
                  "[[:space:]]*(concept)\\'" "" title t t))
           (clean (string-trim base)))
      (unless (string-empty-p clean)
        clean))))

(defun delib-flow--reference-note-relevant-source-text (item package)
  "Return source text for reference-note ITEM in PACKAGE, biased to the best section."
  (let* ((digest (delib-flow--package-email-digest package))
         (source (plist-get package :source))
         (text (if digest
                   (plist-get digest :plain-body)
                 (plist-get source :content)))
         (focus (delib-flow--reference-note-focus-heading item)))
    (if (and (delib-flow--non-empty-string-p text)
             (delib-flow--non-empty-string-p focus)
             (string-match (format "(?im)^%s[[:space:]]*$"
                                   (regexp-quote focus))
                           text))
        (substring text (match-end 0))
      text)))

(defun delib-flow--reference-note-source-units (item package)
  "Return cleaned paragraph-like source units for reference-note ITEM in PACKAGE."
  (let* ((text (or (delib-flow--reference-note-relevant-source-text item package) ""))
         (paragraphs (split-string text "\n[[:space:]\n]*\n+" t))
         units)
    (dolist (paragraph paragraphs (nreverse units))
      (let* ((lines (split-string paragraph "\n"))
             (cleaned-lines
              (delq nil
                    (mapcar
                     (lambda (line)
                       (let ((trimmed (string-trim line)))
                         (when (and (not (string-empty-p trimmed))
                                    (not (string-match-p "\\`[-=_[:space:]]\\{3,\\}\\'" trimmed))
                                    (not (string-match-p
                                          "\\b\\(?:unsubscribe\\|view in browser\\|manage preferences\\)\\b"
                                          (downcase trimmed)))
                                    (delib-flow--reference-note-useful-source-line-p trimmed))
                           (delib-flow--reference-note-clean-line trimmed))))
                     lines)))
             (unit (string-trim (string-join cleaned-lines " "))))
        (when (and (not (string-empty-p unit))
                   (string-match-p "[[:alnum:]]" unit))
          (push unit units))))))

(defun delib-flow--reference-note-split-unit-sentences (unit)
  "Return sentence-like fragments from reference-note UNIT."
  (let ((parts (split-string unit "\\(?:[.?!]\\)[[:space:]]+" t)))
    (if (> (length parts) 1)
        (mapcar #'string-trim parts)
      (list (string-trim unit)))))

(defun delib-flow--reference-note-highlight-score (fragment terms)
  "Return heuristic score for note-highlight FRAGMENT against title TERMS."
  (let* ((lower (downcase fragment))
         (words (delib-flow--string-words fragment))
         (term-score (length (seq-intersection terms words #'string=)))
         (concept-score
          (+ (if (string-match-p "\\bthe idea\\b" lower) 6 0)
             (if (string-match-p "\\binstead of\\b" lower) 4 0)
             (if (string-match-p "\\byou create\\b" lower) 4 0)
             (if (string-match-p "\\bfits your\\b" lower) 3 0)
             (if (string-match-p "\\bmore valuable\\b" lower) 3 0)
             (if (string-match-p "\\bgoing up in value\\b" lower) 3 0)
             (if (string-match-p "\\bpersonal ai advisors\\b" lower) 5 0)
             (if (string-match-p "\\bnutrition coach\\b" lower) 3 0)))
         (penalty
          (+ (if (string-match-p "\\bquick update\\b" lower) 4 0)
             (if (string-match-p "\\bweek [0-9]+\\b" lower) 2 0)
             (if (string-match-p "\\bcohort\\b" lower) 2 0))))
    (- (+ term-score concept-score) penalty)))

(defun delib-flow--reference-note-title-terms (item)
  "Return meaningful title terms for reference-note ITEM."
  (seq-filter
   (lambda (word)
     (and (>= (length word) 4)
          (not (member word delib-flow--tag-suggestion-stopwords))))
   (delib-flow--string-words (delib-flow--reference-note-title item))))

(defun delib-flow--reference-note-source-lines (package)
  "Return candidate source lines for reference-note drafting from PACKAGE."
  (delib-flow--reference-note-source-units
   (or (delib-flow--reference-note-preview-item package)
       (delib-flow--selected-reference-note-candidate-for-drafting package)
       (delib-flow--source-title-reference-note package))
   package))

(defun delib-flow--reference-note-source-highlights (item package)
  "Return up to three source highlight lines for reference-note ITEM in PACKAGE."
  (let* ((terms (delib-flow--reference-note-title-terms item))
         (lines (delib-flow--reference-note-source-units item package))
         scored)
    (dolist (line lines)
      (dolist (fragment (delib-flow--reference-note-split-unit-sentences line))
        (let* ((cleaned (delib-flow--reference-note-clean-line fragment))
               (score (delib-flow--reference-note-highlight-score cleaned terms)))
          (when (> score 0)
            (push (cons score cleaned) scored)))))
    (let ((selected
           (mapcar #'cdr
                   (seq-take
                    (sort scored (lambda (left right) (> (car left) (car right))))
                    3))))
      (or (delete-dups selected)
          (seq-take (mapcar #'delib-flow--reference-note-clean-line lines) 3)))))

(defun delib-flow--reference-note-support-candidates (package)
  "Return note-support candidates from PACKAGE."
  (or (delib-flow--retained-candidates package)
      (plist-get (plist-get package :working-context) :retrieved-candidates)
      nil))

(defun delib-flow--reference-note-support-line (candidate)
  "Return support line text for retained CANDIDATE."
  (let* ((title (plist-get candidate :title))
         (reason (cond
                  ((plist-get candidate :filter-reasons)
                   (mapconcat #'identity (plist-get candidate :filter-reasons) ", "))
                  ((plist-get candidate :reasons)
                   (mapconcat #'identity (plist-get candidate :reasons) ", "))
                  (t nil)))
         (focus (and (plist-get candidate :file)
                     (delib-flow--candidate-note-focus-line candidate))))
    (string-trim
     (format "%s%s%s"
             (or title "Untitled note")
             (if focus (format " - %s" (string-trim focus)) "")
             (if reason (format " (%s)" reason) "")))))

(defun delib-flow--reference-note-support-lines (package)
  "Return up to three related-material lines for PACKAGE."
  (seq-take
   (delete-dups
    (delq nil
          (mapcar #'delib-flow--reference-note-support-line
                  (delib-flow--reference-note-support-candidates package))))
   3))

(defun delib-flow--reference-note-draft-summary (item package)
  "Return seeded draft summary paragraph for reference-note ITEM in PACKAGE."
  (let* ((title (delib-flow--reference-note-title item))
         (source-title (delib-flow--source-display-title package))
         (highlights (delib-flow--reference-note-source-highlights item package))
         (summary-text
          (plist-get (plist-get (plist-get (plist-get package :working-context)
                                           :inspect-output)
                                :analysis)
                     :summary))
         (clean-summary
          (and summary-text
               (let ((cleaned
                      (delib-flow--reference-note-clean-line
                       (replace-regexp-in-string
                        "Referenced materials?:.*\\'"
                        ""
                        summary-text))))
                 (unless (string-empty-p cleaned)
                   cleaned)))))
    (cond
     ((eq (plist-get item :note-type) 'project-support)
      (format "This note captures supporting context for %s from %s."
              title source-title))
     (highlights
      (format "This note captures %s. The source frames it as: %s"
              title
              (string-trim-right (car highlights) "[[:space:]]*[.?!]*")))
     (clean-summary
      (format "This note captures %s. The source frames it as: %s"
              title
              (string-trim-right clean-summary "[[:space:]]*[.?!]*")))
     (t
     (format "This note captures %s from %s."
              title source-title)))))

(defun delib-flow--reference-note-reuse-angle (item)
  "Return a seeded reuse-angle line for reference-note ITEM."
  (format "Reuse this when related notes touch %s or adjacent patterns."
          (downcase (delib-flow--reference-note-title item))))

(defun delib-flow--reference-note-filetag-suggestions (item)
  "Return note filetag suggestions for reference-note ITEM."
  (delete-dups
   (seq-filter
    #'identity
    (mapcar
     #'delib-flow--normalize-tag-suggestion
     (seq-remove
      (lambda (tag)
        (member tag '("reference_note" "general_pkm" "project_support"
                      "email" "newsletter" "mailing_list")))
      (plist-get item :tag-suggestions))))))

(defun delib-flow--format-org-filetags (tags)
  "Return TAGS formatted for an Org #+filetags line."
  (format ":%s:" (string-join tags ":")))

(defun delib-flow--reference-note-content-with-tag-suggestions (content item)
  "Return CONTENT with reference-note tag suggestions merged into filetags."
  (let ((suggested-tags (delib-flow--reference-note-filetag-suggestions item)))
    (if (null suggested-tags)
        content
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+filetags:[ \t]*\\(.+\\)$" nil t)
            (let* ((line-beginning (line-beginning-position))
                   (line-end (line-end-position))
                   (existing
                    (seq-filter
                     #'identity
                     (mapcar (lambda (tag)
                               (let ((trimmed (string-trim tag)))
                                 (unless (string-empty-p trimmed)
                                   (downcase trimmed))))
                             (split-string (match-string 1) ":" t))))
                   (merged (delete-dups (append existing suggested-tags))))
              (delete-region line-beginning line-end)
              (goto-char line-beginning)
              (insert (format "#+filetags: %s"
                              (delib-flow--format-org-filetags merged))))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+title:.*\n" nil t)
              (insert (format "#+filetags: %s\n"
                              (delib-flow--format-org-filetags suggested-tags)))
            (insert (format "#+filetags: %s\n"
                            (delib-flow--format-org-filetags suggested-tags)))))
        (buffer-string)))))

(defun delib-flow--reference-note-source-context-lines (package)
  "Return concise source-context bullet lines for PACKAGE."
  (let* ((digest (delib-flow--package-email-digest package))
         (source-title (delib-flow--source-display-title package))
         (contacts (delib-flow--package-contact-emails package))
         (type-hint (and digest (plist-get digest :type-hint))))
    (delq nil
          (list (format "- Source title: %s" source-title)
                (when type-hint
                  (format "- Source type: %s" type-hint))
                (when contacts
                  (format "- Contact(s): %s"
                          (string-join contacts ", ")))))))

(defun delib-flow--reference-note-seeded-body (item package)
  "Return structured seeded body text for reference-note ITEM in PACKAGE."
  (let* ((highlights (delib-flow--reference-note-source-highlights item package))
        (support-lines (delib-flow--reference-note-support-lines package))
        (source-context-lines (delib-flow--reference-note-source-context-lines package))
        (draft-summary (delib-flow--reference-note-draft-summary item package))
        (durable-claim (car highlights))
        (why-it-matters
         (let ((summary-only
                (string-remove-prefix
                 (format "This note captures %s. The source frames it as: "
                         (delib-flow--reference-note-title item))
                 draft-summary)))
           summary-only)))
    (string-join
     (append
      (list "* Working draft"
            draft-summary
            (format "- Durable claim: %s"
                    (or durable-claim
                        "Capture the core claim from the source in your own words."))
            (format "- Why it matters: %s"
                    (or why-it-matters
                        "Explain why this idea matters beyond the immediate source."))
            (format "- Reuse angle: %s"
                    (delib-flow--reference-note-reuse-angle item))
            ""
            "* Source highlights")
      (if highlights
          (append
           (mapcar (lambda (line) (format "- %s" line)) highlights)
           '(""))
        '("- Capture the strongest lines, claims, or examples from the source here."
          ""))
      (list "* Related material to connect")
      (if support-lines
          (append
           (mapcar (lambda (line) (format "- %s" line)) support-lines)
           '(""))
        '("- Add nearby notes, projects, or references that deepen this idea."
          ""))
      (list "* Source context")
      (if source-context-lines
          (append source-context-lines '(""))
        '("- Add the source title, sender, and any context needed for later trust checks."
          ""))
      (list "* Next pass"
            "- Distill the durable idea in your own words."
            "- Explain why this note is reusable beyond the source email."
            "- Link or merge with nearby notes if the concept already exists."))
     "\n")))

(defun delib-flow--reference-note-draft-body-with-seed (draft-body item package)
  "Return DRAFT-BODY enriched with seeded structure for ITEM and PACKAGE."
  (let* ((clean-draft (string-trim (or draft-body "")))
         (highlights (delib-flow--reference-note-source-highlights item package))
         (support-lines (delib-flow--reference-note-support-lines package))
         (source-context-lines (delib-flow--reference-note-source-context-lines package))
         (draft-sentences (delib-flow--reference-note-split-unit-sentences clean-draft))
         (durable-claim (or (car draft-sentences) (car highlights)))
         (why-it-matters (or (cadr draft-sentences) (car highlights))))
    (string-join
     (append
      (list "* Working draft"
            (if (string-empty-p clean-draft)
                (delib-flow--reference-note-draft-summary item package)
              clean-draft)
            (format "- Durable claim: %s"
                    (or durable-claim
                        "Capture the core claim from the source in your own words."))
            (format "- Why it matters: %s"
                    (or why-it-matters
                        "Explain why this idea matters beyond the immediate source."))
            (format "- Reuse angle: %s"
                    (delib-flow--reference-note-reuse-angle item))
            ""
            "* Source highlights")
      (if highlights
          (append
           (mapcar (lambda (line) (format "- %s" line)) highlights)
           '(""))
        '("- Capture the strongest lines, claims, or examples from the source here."
          ""))
      (list "* Related material to connect")
      (if support-lines
          (append
           (mapcar (lambda (line) (format "- %s" line)) support-lines)
           '(""))
        '("- Add nearby notes, projects, or references that deepen this idea."
          ""))
      (list "* Source context")
      (if source-context-lines
          (append source-context-lines '(""))
        '("- Add the source title, sender, and any context needed for later trust checks."
          ""))
      (list "* Next pass"
            "- Distill the durable idea in your own words."
            "- Explain why this note is reusable beyond the source email."
            "- Link or merge with nearby notes if the concept already exists."))
     "\n")))

(defun delib-flow--reference-note-content-with-seed (content item package)
  "Return CONTENT augmented with seeded note structure for ITEM and PACKAGE."
  (if (and package
           (delib-flow--reference-note-content-needs-seed-p content))
      (concat (string-trim-right content)
              "\n\n"
              (delib-flow--reference-note-seeded-body item package)
              "\n")
    content))

(defun delib-flow--render-reference-note-template (template bindings)
  "Return TEMPLATE rendered with BINDINGS."
  (let ((rendered template))
    (dolist (binding bindings rendered)
      (setq rendered
            (replace-regexp-in-string
             (regexp-quote (car binding))
             (or (cdr binding) "")
             rendered
             t
             t)))))

(defun delib-flow--reference-note-content (item &optional package)
  "Return staged note file content for approved ITEM from PACKAGE."
  (let* ((content
          (if-let ((plan (and package
                              (delib-flow--reference-note-org-roam-stage-plan item package))))
              (delib-flow--strip-org-capture-point-markers (plist-get plan :content))
            (let ((template (delib-flow--reference-note-template item)))
              (if (string-match-p "\\${\\(title\\|source-artifact\\|note-type\\)}" template)
                  (delib-flow--render-reference-note-template
                   template
                   (delib-flow--reference-note-template-bindings item))
                (delib-flow--fill-capture-template
                 template
                 (delib-flow--capture-template-context
                  item package (delib-flow--matched-project-title package) nil))))))
         (draft-body (plist-get item :draft-body)))
    (delib-flow--reference-note-content-with-tag-suggestions
     (if (delib-flow--non-empty-string-p draft-body)
         (concat (string-trim-right content)
                 "\n\n"
                 (string-trim-right
                  (if (delib-flow--reference-note-content-needs-seed-p draft-body)
                      (delib-flow--reference-note-draft-body-with-seed draft-body item package)
                    draft-body))
                 "\n")
       (delib-flow--reference-note-content-with-seed content item package))
     item)))

(defun delib-flow--reference-note-org-roam-content-with-id (content)
  "Return CONTENT with an org-roam-style ID drawer added when absent."
  (let ((text (delib-flow--strip-org-capture-point-markers content)))
    (if (string-match-p "^:ID:[[:space:]]+\\(.+\\)$" text)
        text
      (let ((id (org-id-new)))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (looking-at "^#\\+.*\n")
            (forward-line 1))
          (insert ":PROPERTIES:\n:ID: " id "\n:END:\n")
          (unless (looking-at "\n\\|\\'")
            (insert "\n"))
          (buffer-string))))))

(defun delib-flow--reference-note-id-from-content (content)
  "Return org ID from note CONTENT, or nil when absent."
  (when (string-match "^:ID:[[:space:]]+\\(.+\\)$" content)
    (match-string 1 content)))

(defun delib-flow--stage-org-roam-reference-note-file (item package)
  "Stage approved reference-note ITEM from PACKAGE via org-roam-like semantics."
  (delib-flow--ensure-org-roam-load-path)
  (unless (require 'org-id nil t)
    (error "Org ID support is not available"))
  (let* ((plan (or (delib-flow--reference-note-org-roam-stage-plan item package)
                   (error "Reference note org-roam capture plan is unavailable")))
         (target (plist-get plan :target))
         (content (delib-flow--reference-note-org-roam-content-with-id
                   (delib-flow--reference-note-content item package))))
    (when (or (file-exists-p target)
              (get-file-buffer target))
      (error "Deterministic note target already exists"))
    (with-current-buffer (delib-flow--org-file-buffer target)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert content)
      (set-buffer-modified-p t))
    (when-let ((id (delib-flow--reference-note-id-from-content content)))
      (org-id-add-location id target))
    target))

(defun delib-flow--create-reference-note-file (item &optional package)
  "Stage deterministic note file for approved ITEM from PACKAGE without saving."
  (if (and package
           (delib-flow--reference-note-org-roam-template item package))
      (delib-flow--stage-org-roam-reference-note-file item package)
    (let ((target (delib-flow--reference-note-file item package)))
      (when (or (file-exists-p target)
                (get-file-buffer target))
        (error "Deterministic note target already exists"))
      (with-current-buffer (delib-flow--org-file-buffer target)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (delib-flow--reference-note-content item package))
        (set-buffer-modified-p t))
      target)))

(defun delib-flow--reference-note-link (item target &optional package)
  "Return Org file link for reference-note ITEM at TARGET."
  (let ((project-dir (file-name-directory delib-flow-my-projects-file)))
    (format "[[file:%s][%s]]"
            (file-relative-name target project-dir)
            (if package
                (delib-flow--reference-note-effective-title item package)
              (delib-flow--reference-note-title item)))))

(defun delib-flow--matched-project-point (project-title)
  "Move point to matched PROJECT-TITLE in current Org buffer and return its level."
  (goto-char (point-min))
  (catch 'found
    (while (re-search-forward org-heading-regexp nil t)
      (beginning-of-line)
      (when (string= (org-get-heading t t t t) project-title)
        (throw 'found (org-outline-level)))
      (outline-next-heading))
    (error "Matched project heading is not present in the My Projects file")))

(defun delib-flow--property-links (value)
  "Return parsed Org file links from property VALUE."
  (split-string (or value "") "[ \t\n]+" t))

(defun delib-flow--project-reference-files-value (existing-value link)
  "Return updated REFERENCE_FILES property from EXISTING-VALUE with LINK."
  (string-join
   (delete-dups (append (delib-flow--property-links existing-value)
                        (list link)))
   " "))

(defun delib-flow--update-project-reference-files (project-title item target &optional package)
  "Update matched PROJECT-TITLE metadata for reference-note ITEM at TARGET."
  (unless delib-flow-my-projects-file
    (error "The My Projects file is not configured"))
  (delib-flow--stage-org-file-edit
   delib-flow-my-projects-file
   (lambda ()
     (delib-flow--matched-project-point project-title)
     (org-entry-put
      (point)
      "REFERENCE_FILES"
      (delib-flow--project-reference-files-value
       (org-entry-get (point) "REFERENCE_FILES")
       (delib-flow--reference-note-link item target package)))))
  (format "%s::%s:REFERENCE_FILES" delib-flow-my-projects-file project-title))

(defun delib-flow--project-support-note-p (item)
  "Return non-nil when ITEM is a project-support reference note."
  (and (eq (plist-get item :kind) 'reference-note)
       (eq (plist-get item :note-type) 'project-support)))

(defun delib-flow--project-support-note-metadata-location (item package target)
  "Return metadata target after filing support-note ITEM from PACKAGE to TARGET."
  (let ((project-title (delib-flow--matched-project-title package)))
    (when project-title
      (delib-flow--update-project-reference-files
       project-title item target package))))

(defun delib-flow--project-heading-exists-p (file title)
  "Return non-nil when Org FILE already contains project TITLE."
  (with-current-buffer (delib-flow--org-file-buffer file)
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward org-heading-regexp nil t)
          (beginning-of-line)
          (when (and (delib-flow--project-heading-candidate-p)
                     (string= (org-get-heading t t t t) title))
            (throw 'found t))
          (outline-next-heading))
        nil))))

(defun delib-flow--project-child-exists-p (file project-title item)
  "Return non-nil when Org FILE already contains ITEM under PROJECT-TITLE."
  (with-current-buffer (delib-flow--org-file-buffer file)
    (save-excursion
      (condition-case nil
          (progn
            (let ((level (1+ (delib-flow--matched-project-point project-title)))
                  (limit (save-excursion
                           (org-end-of-subtree t t)
                           (point))))
              (re-search-forward
               (format "^%s$"
                       (regexp-quote
                        (string-trim
                         (delib-flow--project-item-heading item level))))
               limit t)))
        (error nil)))))

(defun delib-flow--new-project-conflict (item)
  "Return filing conflict for new project ITEM, or nil."
  (when (delib-flow--project-heading-exists-p
         delib-flow-my-projects-file
         (plist-get item :title))
    (list :kind 'project
          :item-text (plist-get item :title)
          :reason "A project with this title already exists in the My Projects file.")))

(defun delib-flow--matched-project-child-conflict (item package)
  "Return filing conflict for project child ITEM from PACKAGE, or nil."
  (let ((project-title (delib-flow--matched-project-title package)))
    (unless project-title
      (error "Approved project filing requires a matched project"))
    (when (delib-flow--project-child-exists-p delib-flow-my-projects-file
                                              project-title
                                              item)
      (list :kind (plist-get item :kind)
            :item-text (plist-get item :text)
            :reason "An identical project child heading already exists in the matched project."))))

(defun delib-flow--file-project-item-conflict (item package)
  "Return filing conflict for project ITEM from PACKAGE, or nil."
  (unless delib-flow-my-projects-file
    (error "The My Projects file is not configured"))
  (if (eq (plist-get item :kind) 'project)
      (delib-flow--new-project-conflict item)
    (delib-flow--matched-project-child-conflict item package)))

(defun delib-flow--reference-note-conflict (item package)
  "Return filing conflict for reference-note ITEM from PACKAGE, or nil."
  (let ((target (delib-flow--reference-note-file item package)))
    (when (or (file-exists-p target)
              (get-file-buffer target))
      (list :kind 'reference-note
            :item-text (plist-get item :text)
            :reason (format "The note target already exists: %s"
                            target)))))

(defun delib-flow--approved-item-conflict (item package)
  "Return filing conflict for approved ITEM from PACKAGE, or nil."
  (if (eq (plist-get item :kind) 'reference-note)
      (delib-flow--reference-note-conflict item package)
    (delib-flow--file-project-item-conflict item package)))

(defun delib-flow--approved-item-conflicts (package)
  "Return filing conflicts for approved items in PACKAGE."
  (delq nil
        (mapcar (lambda (item)
                  (delib-flow--approved-item-conflict item package))
                (delib-flow--approved-items package))))

(defun delib-flow--file-project-item (item package)
  "File approved ITEM into the matched project from PACKAGE."
  (unless delib-flow-my-projects-file
    (error "The My Projects file is not configured"))
  (if (eq (plist-get item :kind) 'project)
      (progn
        (delib-flow--insert-new-project delib-flow-my-projects-file item)
        (format "%s::%s" delib-flow-my-projects-file
                (plist-get item :title)))
    (unless (file-readable-p delib-flow-my-projects-file)
      (error "The My Projects file is not configured or readable"))
    (let ((project-title (delib-flow--matched-project-title package)))
      (unless project-title
        (error "Approved project filing requires a matched project"))
      (delib-flow--insert-project-child delib-flow-my-projects-file
                                        project-title
                                        item)
      (format "%s::%s" delib-flow-my-projects-file project-title))))

(defun delib-flow--file-reference-note (item &optional package)
  "File approved reference-note ITEM from PACKAGE into the ZK root."
  (delib-flow--create-reference-note-file item package))

(defun delib-flow--filed-project-decision (item)
  "Return synthetic matched-project decision for newly filed project ITEM."
  (list :match-status 'matched
        :best-project
        (list :title (plist-get item :title)
              :tags (or (plist-get item :tags)
                        (plist-get item :tag-suggestions))
              :contacts nil
              :terms (delib-flow--project-candidate-terms
                      (plist-get item :title)
                      (or (plist-get item :tags)
                          (plist-get item :tag-suggestions))
                      "")
              :links nil)
        :candidates nil
        :selection-method 'proposed-filed
        :selected-model "deterministic-filed-project"
        :reason "A newly proposed project was filed and is now the active matched project for this run."))

(defun delib-flow--filed-location (item target)
  "Return target-location object for ITEM filed to TARGET."
  (list :kind (plist-get item :kind)
        :item-text (plist-get item :text)
        :target target))

(defun delib-flow--reference-note-locations (item package)
  "Return target locations after filing reference-note ITEM from PACKAGE."
  (let* ((target (delib-flow--file-reference-note item package))
         (metadata-target
          (and (delib-flow--project-support-note-p item)
               (delib-flow--project-support-note-metadata-location
                item package target))))
    (delq nil
          (list (delib-flow--filed-location item target)
                (and metadata-target
                     (delib-flow--filed-location item metadata-target))))))

(defun delib-flow--file-approved-item (item package)
  "Return target-location objects after filing approved ITEM from PACKAGE."
  (if (eq (plist-get item :kind) 'reference-note)
      (delib-flow--reference-note-locations item package)
    (list (delib-flow--filed-location
           item
           (delib-flow--file-project-item item package)))))

(defun delib-flow--file-approved-output-locations (package)
  "Return filed target locations for approved items in PACKAGE."
  (mapcan (lambda (item)
            (delib-flow--file-approved-item item package))
          (delib-flow--approved-items package)))

(defun delib-flow--file-approved-outputs-result (package)
  "Return raw deterministic filing result for PACKAGE."
  (let* ((approved-items (delib-flow--approved-items package))
         (conflicts (delib-flow--approved-item-conflicts package)))
    (if conflicts
        (list :approved-count (length approved-items)
              :filed-count 0
              :filed-items nil
              :target-locations nil
              :conflict-count (length conflicts)
              :conflicts conflicts
              :reason "Approved filing artifacts were not written because deterministic target conflicts were detected.")
      (let ((locations (delib-flow--file-approved-output-locations package)))
        (list :approved-count (length approved-items)
              :filed-count (length locations)
              :filed-items approved-items
              :target-locations locations
              :conflict-count 0
              :conflicts nil
              :reason "Approved filing artifacts were staged into deterministic target buffers for operator review before saving.")))))

(defun delib-flow--execute-file-approved-outputs (package)
  "Return raw filing output for PACKAGE."
  (delib-flow--file-approved-outputs-result package))

(defun delib-flow--normalize-inspect-source-output (raw-output)
  "Return normalized inspect-source text from RAW-OUTPUT."
  (format
   "- Source type: %s\n- Source type reason: %s\n- Title: %s\n- Outline path: %s\n- Body lines: %s\n- Content words: %s\n- Source has ID: %s\n- Contact emails: %s\n- Org file links: %s\n- Body preview: %s"
   (plist-get raw-output :source-type)
   (plist-get raw-output :source-type-reason)
   (or (plist-get raw-output :title) "Untitled source")
   (or (mapconcat #'identity (plist-get raw-output :outline-path) " > ")
       "No outline path")
   (plist-get raw-output :body-line-count)
   (plist-get raw-output :content-word-count)
   (if (plist-get raw-output :has-id) "yes" "no")
   (if-let ((emails (plist-get raw-output :contact-emails)))
       (mapconcat #'identity emails ", ")
     "none")
   (plist-get raw-output :org-file-link-count)
   (or (plist-get raw-output :body-preview) "No body preview")))

(defun delib-flow--candidate-title-list (candidates)
  "Return a comma-separated title list for CANDIDATES."
  (mapconcat (lambda (candidate)
               (plist-get candidate :title))
             candidates
             ", "))

(defun delib-flow--normalize-match-project-output (raw-output)
  "Return normalized match-project text from RAW-OUTPUT."
  (let ((status (plist-get raw-output :match-status)))
    (cond
     ((eq status 'matched)
      (format "- Match status: matched\n- Best project: %s\n- Reason: %s"
              (plist-get (plist-get raw-output :best-project) :title)
              (plist-get raw-output :reason)))
     ((eq status 'ambiguous)
      (format "- Match status: ambiguous\n- Candidates: %s\n- Reason: %s"
              (delib-flow--candidate-title-list
               (plist-get raw-output :candidates))
              (plist-get raw-output :reason)))
     (t
     (format "- Match status: no-match\n- Reason: %s"
              (plist-get raw-output :reason))))))

(defun delib-flow--normalize-discovery-candidate (candidate)
  "Return normalized text for discovery CANDIDATE."
  (format "- %s [%s]: %s"
          (plist-get candidate :title)
          (plist-get candidate :score)
          (mapconcat #'identity (plist-get candidate :reasons) ", ")))

(defun delib-flow--normalize-filter-candidate (candidate)
  "Return normalized text for filtered CANDIDATE."
  (format "- %s [%s]: %s"
          (plist-get candidate :title)
          (plist-get candidate :score)
          (delib-flow--filter-reasons-text candidate)))

(defun delib-flow--normalize-discover-reference-material-output (raw-output)
  "Return normalized discovery text from RAW-OUTPUT."
  (if (plist-get raw-output :candidates)
      (format "- Search terms: %s\n- Candidate count: %s\n%s"
              (mapconcat #'identity (plist-get raw-output :search-terms) ", ")
              (plist-get raw-output :candidate-count)
              (mapconcat #'delib-flow--normalize-discovery-candidate
                         (plist-get raw-output :candidates)
                         "\n"))
    (format "- Search terms: %s\n- Candidate count: 0\n- No relevant reference material found."
            (mapconcat #'identity (plist-get raw-output :search-terms) ", "))))

(defun delib-flow--normalize-filter-reference-material-output (raw-output)
  "Return normalized filter text from RAW-OUTPUT."
  (format "- Candidate count: %s\n- Retained count: %s\n- Rejected count: %s\n- Retained candidates:\n%s\n- Rejected candidates:\n%s"
          (plist-get raw-output :candidate-count)
          (plist-get raw-output :retained-count)
          (plist-get raw-output :rejected-count)
          (if-let ((retained (plist-get raw-output :retained-candidates)))
              (mapconcat #'delib-flow--normalize-filter-candidate
                         retained
                         "\n")
            "- none")
          (if-let ((rejected (plist-get raw-output :rejected-candidates)))
              (mapconcat #'delib-flow--normalize-filter-candidate
                         rejected
                         "\n")
            "- none")))

(defun delib-flow--normalize-manual-project-match-output (raw-output)
  "Return normalized manual project-match text from RAW-OUTPUT."
  (format "- Match status: %s\n- Selection method: %s\n- Selected project: %s\n- Operator selection: %s\n- Operator notes: %s\n- Reason: %s"
          (plist-get raw-output :match-status)
          (plist-get raw-output :selection-method)
          (or (plist-get (plist-get raw-output :best-project) :title)
              "none")
          (or (plist-get raw-output :operator-selection) "none")
          (or (plist-get raw-output :operator-notes) "none")
          (plist-get raw-output :reason)))

(defun delib-flow--normalize-propose-new-project-output (raw-output)
  "Return normalized new-project proposal text from RAW-OUTPUT."
  (format
   "- Proposed project title: %s\n- Proposed state: %s\n- First item: %s\n- Tags: %s\n- Reason: %s"
   (plist-get raw-output :project-title)
   (plist-get raw-output :project-state)
   (plist-get (plist-get raw-output :first-item) :text)
   (mapconcat #'identity (plist-get raw-output :tags) ", ")
   (plist-get raw-output :reason)))

(defun delib-flow--normalize-draft-action (item)
  "Return normalized text for draft action ITEM."
  (format "- %s" (plist-get item :text)))

(defun delib-flow--normalize-draft-item (item)
  "Return normalized text for draft ITEM with warning annotations."
  (string-join (delib-flow--draft-item-preview-lines item) "\n"))

(defun delib-flow--normalize-draft-items-with-warnings (items)
  "Return normalized warning-aware text for draft ITEMS."
  (mapconcat #'delib-flow--normalize-draft-item items "\n"))

(defun delib-flow--normalize-proposal-warning-summary (raw-output)
  "Return normalized warning summary text from proposal RAW-OUTPUT."
  (format "- Warning count: %s\n- Artifacts with warnings: %s\n- Blocking warning count: %s\n- Artifacts with blocking warnings: %s"
          (or (plist-get raw-output :warning-count) 0)
          (or (plist-get raw-output :warning-item-count) 0)
          (or (plist-get raw-output :blocking-warning-count) 0)
          (or (plist-get raw-output :blocking-warning-item-count) 0)))

(defun delib-flow--normalize-extract-actions-output (raw-output)
  "Return normalized extract-actions text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s\n%s"
          (plist-get raw-output :candidate-count)
          (delib-flow--normalize-proposal-warning-summary raw-output)
          (delib-flow--normalize-draft-items-with-warnings
           (plist-get raw-output :actions))))

(defun delib-flow--normalize-extract-waiting-for-output (raw-output)
  "Return normalized extract-waiting-for text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s\n%s"
          (plist-get raw-output :candidate-count)
          (delib-flow--normalize-proposal-warning-summary raw-output)
          (delib-flow--normalize-draft-items-with-warnings
           (plist-get raw-output :waiting-fors))))

(defun delib-flow--normalize-suggest-reference-notes-output (raw-output)
  "Return normalized reference-note text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s\n%s"
          (plist-get raw-output :candidate-count)
          (delib-flow--normalize-proposal-warning-summary raw-output)
          (delib-flow--normalize-draft-items-with-warnings
           (plist-get raw-output :reference-notes))))

(defun delib-flow--normalize-draft-selected-reference-note-output (raw-output)
  "Return normalized selected-note draft text from RAW-OUTPUT."
  (let* ((item (plist-get raw-output :drafted-item))
         (draft-body (or (plist-get item :draft-body) "")))
    (format "- Selected note: %s\n- Note type: %s\n- Reason: %s\n\n#+begin_example\n%s\n#+end_example"
            (or (delib-flow--reference-note-title item) "Untitled note")
            (or (plist-get item :note-type) 'general-pkm)
            (or (plist-get raw-output :reason) "No reason recorded.")
            (string-trim-right draft-body))))

(defun delib-flow--normalize-decide-cloud-pass-output (raw-output)
  "Return normalized cloud-routing text from RAW-OUTPUT."
  (format "- Route: %s\n- Target stage: %s\n- Selected model: %s\n- Provider: %s\n- Policy profile: %s\n- Sanitization status: %s\n- Reason: %s"
          (plist-get raw-output :route)
          (plist-get raw-output :target-stage)
          (plist-get raw-output :selected-model)
          (plist-get raw-output :selected-provider)
          (plist-get raw-output :policy-profile)
          (plist-get raw-output :sanitization-status)
          (plist-get raw-output :reason)))

(defun delib-flow--normalize-sanitize-for-cloud-output (raw-output)
  "Return normalized cloud-sanitization text from RAW-OUTPUT."
  (format "- Sanitization status: %s\n- Reason: %s\n%s"
          (plist-get raw-output :sanitization-status)
          (plist-get raw-output :reason)
          (plist-get raw-output :sanitized-package)))

(defun delib-flow--normalize-approve-cloud-send-output (raw-output)
  "Return normalized cloud-approval text from RAW-OUTPUT."
  (format "- Sanitization status: %s\n- Reason: %s\n%s"
          (plist-get raw-output :sanitization-status)
          (plist-get raw-output :reason)
          (plist-get raw-output :approved-package)))

(defun delib-flow--normalize-run-cloud-stage-output (raw-output)
  "Return normalized cloud-stage text from RAW-OUTPUT."
  (format "- Target stage: %s\n- Selected model: %s\n- Sanitization status: %s\n- Reason: %s\n%s"
          (plist-get raw-output :target-stage)
          (plist-get raw-output :selected-model)
          (plist-get raw-output :sanitization-status)
          (plist-get raw-output :reason)
          (plist-get raw-output :cloud-output)))

(defun delib-flow--normalize-resolve-cloud-failure-output (raw-output)
  "Return normalized cloud-failure resolution text from RAW-OUTPUT."
  (format "- Resolution: %s\n- Fallback mode: %s\n- Reintegration status: %s\n- Operator notes: %s\n- Abort run: %s\n- Reason: %s"
          (plist-get raw-output :resolution)
          (or (plist-get raw-output :fallback-mode) "none")
          (or (plist-get raw-output :reintegration-status) "not-set")
          (or (plist-get raw-output :operator-notes) "none")
          (if (plist-get raw-output :abort-p) "yes" "no")
          (plist-get raw-output :reason)))

(defun delib-flow--normalize-approve-candidate-reintegration-output (raw-output)
  "Return normalized reintegration-approval text from RAW-OUTPUT."
  (format "- Reintegration status: %s\n- Reason: %s"
          (plist-get raw-output :reintegration-status)
          (plist-get raw-output :reason)))

(defun delib-flow--normalize-integrate-into-source-output (raw-output)
  "Return normalized integration text from RAW-OUTPUT."
  (format "- Draft artifact count: %s\n- Cloud context integrated: %s\n- Reason: %s\n%s"
          (plist-get raw-output :draft-count)
          (if (plist-get raw-output :cloud-context-integrated-p) "yes" "no")
          (plist-get raw-output :reason)
          (or (plist-get raw-output :integrated-context)
              "No integrated context is available yet.")))

(defun delib-flow--normalize-blocked-item-preview (raw-output)
  "Return blocked-item preview text from RAW-OUTPUT."
  (if-let ((preview (plist-get raw-output :blocked-item-preview)))
      (concat "- Blocked artifact preview:\n" preview "\n")
    ""))

(defun delib-flow--normalize-blocking-warning (warning)
  "Return normalized filing blocking WARNING text."
  (format "- Blocking warning: %s\n  Fix: %s"
          (plist-get warning :message)
          (delib-flow--draft-item-warning-remediation warning)))

(defun delib-flow--normalize-blocking-warnings (raw-output)
  "Return normalized blocking warning text from RAW-OUTPUT."
  (if-let ((warnings (plist-get raw-output :blocking-warnings)))
      (concat (mapconcat #'delib-flow--normalize-blocking-warning
                         warnings
                         "\n")
              "\n")
    ""))

(defun delib-flow--normalize-selected-filing-preview (raw-output)
  "Return normalized selected filing preview from RAW-OUTPUT."
  (or (plist-get raw-output :selected-preview)
      "No filing artifacts were selected."))

(defun delib-flow--normalize-select-approved-filing-actions-output (raw-output)
  "Return normalized filing-selection text from RAW-OUTPUT."
  (format "- Selected artifact count: %s\n- Remaining draft artifact count: %s\n- Approval blocked: %s\n- Ready selections: %s\n- Blocked selections: %s\n- Operator selection: %s\n- Operator notes: %s\n- Reason: %s\n%s%s%s%s"
          (plist-get raw-output :selected-count)
          (plist-get raw-output :remaining-draft-count)
          (if (plist-get raw-output :approval-blocked-p) "yes" "no")
          (delib-flow--selection-index-list
           (plist-get raw-output :ready-selection-indexes))
          (delib-flow--selection-index-list
           (plist-get raw-output :blocked-selection-indexes))
          (or (plist-get raw-output :operator-selection) "none")
          (or (plist-get raw-output :operator-notes) "none")
          (plist-get raw-output :reason)
          (delib-flow--normalize-blocked-item-preview raw-output)
          (delib-flow--normalize-blocking-warnings raw-output)
          (delib-flow--normalize-selected-filing-preview raw-output)
          (if-let ((locations (plist-get raw-output :planned-target-locations)))
              (concat "\n- Planned file targets:\n"
                      (mapconcat #'delib-flow--normalize-file-target-location
                                 locations
                                 "\n"))
            "")))

(defun delib-flow--normalize-resolve-filing-conflict-output (raw-output)
  "Return normalized conflict-resolution text from RAW-OUTPUT."
  (format "- Resolution: %s\n- Approved artifact count: %s\n- Rejected artifact count: %s\n- Operator notes: %s\n- Reason: %s\n%s"
          (plist-get raw-output :resolution)
          (plist-get raw-output :approved-count)
          (plist-get raw-output :rejected-count)
          (or (plist-get raw-output :operator-notes) "none")
          (plist-get raw-output :reason)
          (if (plist-get raw-output :resolved-approved-item)
              (delib-flow--draft-item-preview-text
               (list (plist-get raw-output :resolved-approved-item)))
            "No approved artifact remains after conflict resolution.")))

(defun delib-flow--normalize-reject-draft-filing-artifact-output (raw-output)
  "Return normalized filing-rejection text from RAW-OUTPUT."
  (format "- Rejected artifact count: %s\n- Remaining draft artifact count: %s\n- Operator selection: %s\n- Operator notes: %s\n- Reason: %s\n%s"
          (plist-get raw-output :rejected-count)
          (plist-get raw-output :remaining-draft-count)
          (or (plist-get raw-output :operator-selection) "none")
          (or (plist-get raw-output :operator-notes) "none")
          (plist-get raw-output :reason)
          (or (plist-get raw-output :rejected-preview)
              "No filing artifacts were rejected.")))

(defun delib-flow--normalize-file-target-location (location)
  "Return normalized text for filed target LOCATION."
  (format "- %s -> %s"
          (plist-get location :item-text)
          (plist-get location :target)))

(defun delib-flow--normalize-file-approved-outputs-output (raw-output)
  "Return normalized filing text from RAW-OUTPUT."
  (format "- Approved artifact count: %s\n- Filed count: %s\n- Conflict count: %s\n- Reason: %s\n%s%s"
          (plist-get raw-output :approved-count)
          (plist-get raw-output :filed-count)
          (plist-get raw-output :conflict-count)
          (plist-get raw-output :reason)
          (if (plist-get raw-output :conflicts)
              (concat
               (mapconcat #'delib-flow--filing-conflict-line
                          (plist-get raw-output :conflicts)
                          "\n")
               "\n")
            "")
          (mapconcat #'delib-flow--normalize-file-target-location
                     (plist-get raw-output :target-locations)
                     "\n")))

(defun delib-flow--completed-stage-decision (stage-id)
  "Return current-decision text for completed STAGE-ID."
  (or (alist-get stage-id delib-flow--stage-decision-alist)
      "Review stage result and choose next action."))

(defun delib-flow--cloud-failure-decision-text (entry)
  "Return current-decision text for failed cloud stage ENTRY."
  (if-let ((stage-id (delib-flow--cloud-shadow-stage-id entry)))
      (if (delib-flow--rerouted-cloud-stage-p stage-id)
          (format "Review cloud failure for %s and choose whether to retry that cloud stage, use local fallback, skip cloud, or abort."
                  (delib-flow--stage-label stage-id))
        "Review cloud failure and choose whether to retry cloud, use local fallback, skip cloud, or abort.")
    "Review cloud failure and choose whether to retry cloud, use local fallback, skip cloud, or abort."))

(defun delib-flow--next-decision-for-entry (entry)
  "Return updated current-decision text for stage ENTRY."
  (if (eq (plist-get entry :status) 'completed)
      (delib-flow--completed-stage-decision
       (plist-get entry :stage-id))
    (if (eq (plist-get entry :stage-id) 'run-cloud-stage)
        (delib-flow--cloud-failure-decision-text entry)
      "Review stage failure and choose whether to retry or abort.")))

(defun delib-flow--apply-inspect-source-entry (run entry)
  "Return RUN updated from completed inspect-source ENTRY."
  (plist-put
   run :working-context
   (let* ((working (delib-flow--run-working-context run))
          (raw-output (copy-tree (plist-get entry :raw-output)))
          (analysis (plist-get raw-output :analysis))
          (sanitized-analysis
           (if analysis
               (plist-put analysis :entities
                          (delib-flow--filtered-inspect-entities
                           (plist-get analysis :entities)))
             analysis))
          (sanitized-output
           (if analysis
               (plist-put raw-output :analysis sanitized-analysis)
             raw-output))
          (digest (or (plist-get working :email-inspect-digest)
                      (delib-flow--cached-email-inspect-digest-for-source
                       (delib-flow--run-source run)))))
     (plist-put
      (plist-put
       (plist-put working :inspect-output sanitized-output)
       :retained-context (plist-get entry :normalized-output))
      :email-inspect-digest digest))))

(defun delib-flow--apply-match-project-entry (run entry)
  "Return RUN updated from completed match-project ENTRY."
  (plist-put
   run :working-context
   (plist-put (delib-flow--run-working-context run)
              :project-match
              (plist-get entry :raw-output))))

(defun delib-flow--apply-manual-project-match-entry (run entry)
  "Return RUN updated from completed manual-project-match ENTRY."
  (let ((updated-run
         (delib-flow--apply-match-project-entry run entry)))
    (if (delib-flow--stage-accepted-p updated-run 'match-project)
        (delib-flow--supersede-stage-review-state updated-run 'match-project)
      updated-run)))

(defun delib-flow--apply-discovery-entry (run entry)
  "Return RUN updated from completed discovery ENTRY."
  (plist-put
   run :working-context
   (plist-put (delib-flow--run-working-context run)
              :retrieved-candidates
              (plist-get (plist-get entry :raw-output) :candidates))))

(defun delib-flow--retained-context-lines (candidates)
  "Return retained-context text for CANDIDATES."
  (mapconcat (lambda (candidate)
               (format "- %s" (plist-get candidate :title)))
             candidates
             "\n"))

(defun delib-flow--apply-filter-entry (run entry)
  "Return RUN updated from completed filter ENTRY."
  (let* ((working (delib-flow--run-working-context run))
         (raw (plist-get entry :raw-output))
         (retained (plist-get raw :retained-candidates))
         (retained-context
          (or (plist-get raw :retained-context)
              (delib-flow--retained-context-lines retained))))
    (plist-put
     run :working-context
     (plist-put
     (plist-put working :filtered-context raw)
      :retained-context retained-context))))

(defun delib-flow--apply-propose-new-project-entry (run entry)
  "Return RUN updated from completed propose-new-project ENTRY."
  (let* ((working (delib-flow--run-working-context run))
         (filing (plist-get run :filing))
         (project-item (plist-get (plist-get entry :raw-output) :project))
         (draft-items (list project-item))
         (updated-filing
          (plist-put
           (plist-put
            (delib-flow--clear-filing-selection-block-state
             (plist-put
              (plist-put
               (plist-put
                (plist-put filing :draft-items draft-items)
                :approved-items nil)
               :rejected-items nil)
              :target-locations nil))
           :preview-text (delib-flow--draft-item-preview-text draft-items))
           :target-locations nil)))
    (setq run
          (plist-put
           (plist-put run :working-context
                      (plist-put working :project-proposal
                                 (plist-get entry :raw-output)))
           :filing updated-filing))
    (setq run
          (delib-flow--set-artifact-family-candidates
           run 'project-proposals draft-items))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block run))))

(defun delib-flow--apply-extract-actions-entry (run entry)
  "Return RUN updated from completed extract-actions ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items (delib-flow--with-draft-item-stage-list
                       (plist-get (plist-get entry :raw-output) :actions)
                       'extract-actions))
         (updated-filing (delib-flow--merge-draft-stage-items
                          filing
                          'extract-actions
                          draft-items))
         (updated-filing
          (plist-put
           (plist-put
            (delib-flow--clear-filing-selection-block-state updated-filing)
            :preview-text
            (delib-flow--draft-item-preview-text
            (plist-get updated-filing :draft-items)))
           :target-locations nil)))
    (setq run (plist-put run :filing updated-filing))
    (setq run
          (delib-flow--set-artifact-family-candidates
           run 'actions draft-items))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block run))))

(defun delib-flow--apply-extract-waiting-for-entry (run entry)
  "Return RUN updated from completed extract-waiting-for ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items (delib-flow--with-draft-item-stage-list
                       (plist-get (plist-get entry :raw-output) :waiting-fors)
                       'extract-waiting-for))
         (updated-filing (delib-flow--merge-draft-stage-items
                          filing
                          'extract-waiting-for
                          draft-items))
         (updated-filing
          (plist-put
           (plist-put
            (delib-flow--clear-filing-selection-block-state updated-filing)
            :preview-text
            (delib-flow--draft-item-preview-text
            (plist-get updated-filing :draft-items)))
           :target-locations nil)))
    (setq run (plist-put run :filing updated-filing))
    (setq run
          (delib-flow--set-artifact-family-candidates
           run 'waiting-fors draft-items))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block run))))

(defun delib-flow--apply-suggest-reference-notes-entry (run entry)
  "Return RUN updated from completed suggest-reference-notes ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items (delib-flow--with-draft-item-stage-list
                       (plist-get (plist-get entry :raw-output) :reference-notes)
                       'suggest-reference-notes))
         (updated-filing (delib-flow--merge-draft-stage-items
                          filing
                          'suggest-reference-notes
                          draft-items))
         (updated-filing
          (plist-put
           updated-filing
           :approved-items
           (seq-remove
            (lambda (item)
              (and (eq (plist-get item :kind) 'reference-note)
                   (eq (plist-get item :draft-stage) 'suggest-reference-notes)))
            (plist-get updated-filing :approved-items))))
         (updated-filing
          (plist-put
           (plist-put
            (delib-flow--clear-filing-selection-block-state updated-filing)
            :preview-text
            (delib-flow--draft-item-preview-text
            (plist-get updated-filing :draft-items)))
           :target-locations nil)))
    (setq run (plist-put run :filing updated-filing))
    (setq run
          (delib-flow--set-artifact-family-candidates
           run 'reference-notes draft-items))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block run))))

(defun delib-flow--apply-draft-selected-reference-note-entry (run entry)
  "Return RUN updated from completed selected-note draft ENTRY."
  (let* ((raw (plist-get entry :raw-output))
         (candidate (plist-get raw :candidate))
         (drafted-item (plist-get raw :drafted-item))
         (candidate-id (and candidate
                            (delib-flow--artifact-candidate-id candidate))))
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run
           'reference-notes
           candidate-id))
    (setq run
          (delib-flow--set-artifact-family-selected-draft
           run
           'reference-notes
           drafted-item))
    (delib-flow--seed-reference-note-capture-review-block
     run)))

(defun delib-flow--clear-cloud-returned-stage-data (working)
  "Return WORKING with stored rerouted cloud stage data cleared."
  (plist-put
   (plist-put
    (plist-put working :cloud-returned-stage-id nil)
    :cloud-returned-stage-raw-output nil)
   :cloud-returned-stage-normalized-output nil))

(defun delib-flow--mark-cloud-reintegrated-stage (run stage-id)
  "Return RUN with STAGE-ID recorded as reintegrated cloud execution."
  (let* ((routing (delib-flow--run-routing run))
         (stage-ids (delib-flow--cloud-reintegrated-stage-ids run)))
    (plist-put
     run :routing
     (plist-put routing :cloud-reintegrated-stage-ids
                (delete-dups (append stage-ids (list stage-id)))))))

(defun delib-flow--apply-cloud-shadow-stage-state (run stage-id)
  "Return RUN with latest rerouted cloud shadow STAGE-ID marked applied."
  (let* ((history (delib-flow--run-stage-history run))
         (entries (plist-get history :entries))
         (updated nil))
    (plist-put
     run :stage-history
     (plist-put
      history :entries
      (reverse
       (mapcar
        (lambda (entry)
          (if (and (not updated)
                   (eq (plist-get entry :stage-id) stage-id)
                   (delib-flow--cloud-shadow-entry-p entry))
              (progn
                (setq updated t)
                (plist-put
                 (plist-put (copy-sequence entry) :applied-p t)
                 :review-state 'accepted))
            entry))
        (reverse entries)))))))

(defun delib-flow--apply-cloud-returned-stage-entry (run)
  "Return RUN updated from any approved rerouted cloud stage output."
  (let* ((working (delib-flow--run-working-context run))
         (stage-id (delib-flow--cloud-returned-stage-id working))
         (raw-output (delib-flow--cloud-returned-stage-raw-output working)))
    (if (or (null stage-id)
            (eq stage-id 'run-cloud-stage)
            (memq stage-id (delib-flow--cloud-reintegrated-stage-ids run))
            (null raw-output))
        run
      (delib-flow--mark-cloud-reintegrated-stage
       (delib-flow--apply-cloud-shadow-stage-state
        (delib-flow--apply-completed-stage-entry
         run
         (list :stage-id stage-id
               :label (delib-flow--stage-label stage-id)
               :status 'completed
               :review-state 'accepted
               :raw-output raw-output
               :normalized-output
               (delib-flow--cloud-returned-stage-normalized-output working)))
        stage-id)
       stage-id))))

(defun delib-flow--apply-decide-cloud-pass-entry (run entry)
  "Return RUN updated from completed decide-cloud-pass ENTRY."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (routing (delib-flow--run-routing run))
         (raw (plist-get entry :raw-output))
         (updated-routing
          (let ((cleared-routing
                 (delib-flow--clear-cloud-failure-state routing)))
            (setq cleared-routing
                  (plist-put cleared-routing :cloud-switch-pending
                             (plist-get raw :cloud-switch-pending)))
            (setq cleared-routing
                  (plist-put cleared-routing :cloud-target-stage
                             (plist-get raw :target-stage)))
            (setq cleared-routing
                  (plist-put cleared-routing :sanitization-status
                             (plist-get raw :sanitization-status)))
            (setq cleared-routing
                  (plist-put cleared-routing :cloud-policy-profile
                             (plist-get raw :policy-profile)))
            (setq cleared-routing
                  (plist-put cleared-routing :selected-cloud-provider
                             (plist-get raw :selected-provider)))
            (plist-put cleared-routing :selected-cloud-model
                       (plist-get raw :selected-model)))))
    (delib-flow--seed-cloud-routing-review-block
     (plist-put
      (plist-put run :working-context working)
      :routing updated-routing))))

(defun delib-flow--apply-sanitize-for-cloud-entry (run entry)
  "Return RUN updated from completed sanitize-for-cloud ENTRY."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (routing (delib-flow--clear-cloud-failure-state
                   (delib-flow--run-routing run)))
         (raw (plist-get entry :raw-output))
         (sanitized-package (plist-get raw :sanitized-package))
         (block (delib-flow--editable-block run 'cloud-package-review))
         (updated-run
          (delib-flow--set-editable-block
           run
           'cloud-package-review
           (plist-put
            (plist-put
             (plist-put
              (plist-put block :original-text sanitized-package)
              :current-text sanitized-package)
             :accepted-text sanitized-package)
            :status 'clean))))
    (plist-put
     (plist-put
      updated-run :working-context
      (plist-put working :cloud-sanitized-context sanitized-package))
     :routing
     (plist-put
      (plist-put routing :cloud-switch-pending
                 (plist-get raw :cloud-switch-pending))
      :sanitization-status
      (plist-get raw :sanitization-status)))))

(defun delib-flow--apply-approve-cloud-send-entry (run entry)
  "Return RUN updated from completed approve-cloud-send ENTRY."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (routing (delib-flow--clear-cloud-failure-state
                   (delib-flow--run-routing run)))
         (raw (plist-get entry :raw-output)))
    (plist-put
     (plist-put
      run :working-context
      (plist-put working :cloud-sanitized-context
                 (plist-get raw :approved-package)))
     :routing
     (plist-put
      (plist-put routing :cloud-switch-pending
                 (plist-get raw :cloud-switch-pending))
      :sanitization-status
      (plist-get raw :sanitization-status)))))

(defun delib-flow--apply-run-cloud-stage-entry (run entry)
  "Return RUN updated from completed run-cloud-stage ENTRY."
  (let* ((working (delib-flow--run-working-context run))
         (routing (delib-flow--clear-cloud-failure-state
                   (delib-flow--run-routing run)))
         (raw (plist-get entry :raw-output)))
    (plist-put
     (plist-put
      run :working-context
      (plist-put
       (plist-put
        (plist-put
         (plist-put working :cloud-returned-context
                    (plist-get raw :cloud-output))
         :cloud-returned-stage-id
         (plist-get raw :target-stage))
        :cloud-returned-stage-raw-output
        (plist-get raw :target-stage-raw-output))
       :cloud-returned-stage-normalized-output
       (plist-get raw :target-stage-normalized-output)))
     :routing
     (plist-put
      (plist-put
       (plist-put routing :cloud-switch-pending
                  (plist-get raw :cloud-switch-pending))
       :sanitization-status
      (plist-get raw :sanitization-status))
      :reintegration-status 'pending-review))))

(defun delib-flow--apply-run-cloud-stage-failure-entry (run entry)
  "Return RUN updated from failed run-cloud-stage ENTRY."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (routing (delib-flow--run-routing run))
         (failed-stage (or (delib-flow--cloud-shadow-stage-id entry)
                           'run-cloud-stage))
         (updated-run
          (plist-put
           (plist-put
            run :working-context
            (plist-put working :cloud-returned-context nil))
           :routing
           (plist-put
            (plist-put
             (plist-put routing :cloud-failure-stage failed-stage)
             :cloud-failure-message
             (plist-get entry :normalized-output))
            :reintegration-status nil))))
    (delib-flow--seed-cloud-failure-review-block updated-run)))

(defun delib-flow--apply-resolve-cloud-failure-entry (run entry)
  "Return RUN updated from completed resolve-cloud-failure ENTRY."
  (let* ((working (delib-flow--clear-cloud-returned-stage-data
                   (delib-flow--run-working-context run)))
         (raw (plist-get entry :raw-output))
         (routing
          (plist-put
           (plist-put
            (delib-flow--clear-cloud-failure-state
             (delib-flow--run-routing run))
            :cloud-fallback-mode
            (plist-get raw :fallback-mode))
           :reintegration-status
           (plist-get raw :reintegration-status)))
         (updated-run
          (plist-put
           (plist-put
            run :working-context
            (plist-put working :cloud-returned-context nil))
           :routing routing)))
    (if (plist-get raw :abort-p)
        (let* ((aborted-run (delib-flow--mark-run-aborted updated-run))
               (session (delib-flow--run-session aborted-run)))
          (plist-put aborted-run :session
                     (plist-put session :current-decision "Run aborted.")))
      updated-run)))

(defun delib-flow--apply-approve-candidate-reintegration-entry (run entry)
  "Return RUN updated from completed reintegration-approval ENTRY."
  (let* ((routing (delib-flow--run-routing run))
         (raw (plist-get entry :raw-output)))
    (plist-put
     run :routing
     (plist-put routing :reintegration-status
                (plist-get raw :reintegration-status)))))

(defun delib-flow--apply-integrate-into-source-entry (run entry)
  "Return RUN updated from completed integrate-into-source ENTRY."
  (let* ((reintegrated-run (delib-flow--apply-cloud-returned-stage-entry run))
         (working (delib-flow--run-working-context reintegrated-run))
         (routing
          (plist-put
           (plist-put
            (plist-put (delib-flow--run-routing reintegrated-run) :cloud-failure-stage nil)
            :cloud-failure-message nil)
           :cloud-fallback-mode nil))
         (filing (plist-get reintegrated-run :filing)))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block
     (plist-put
      (plist-put
       (plist-put reintegrated-run :routing routing)
       :working-context
       (plist-put working :retained-context
                  (plist-get (plist-get entry :raw-output) :integrated-context)))
      :filing
      (delib-flow--clear-filing-selection-block-state
       (plist-put
        (plist-put filing :approved-items nil)
        :rejected-items nil)))))))

(defun delib-flow--apply-reject-draft-filing-artifact-entry (run entry)
  "Return RUN updated from completed filing-rejection ENTRY."
  (let* ((filing (plist-get run :filing))
         (raw (plist-get entry :raw-output))
         (remaining-items (plist-get raw :remaining-draft-items))
         (existing-rejected (plist-get filing :rejected-items))
         (updated-rejected (append existing-rejected
                                   (plist-get raw :rejected-items))))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block
     (plist-put
      run :filing
      (plist-put
       (delib-flow--clear-filing-selection-block-state
        (plist-put
        (plist-put
         (plist-put
          (plist-put filing :draft-items remaining-items)
          :approved-items (plist-get filing :approved-items))
         :rejected-items updated-rejected)
         :target-locations nil))
       :preview-text (delib-flow--draft-item-preview-text remaining-items)))))))

(defun delib-flow--apply-select-approved-filing-actions-entry (run entry)
  "Return RUN updated from completed filing-selection ENTRY."
  (let* ((filing (plist-get run :filing))
         (raw (plist-get entry :raw-output))
         (remaining-items (plist-get raw :remaining-draft-items))
         (updated-filing
         (plist-put
           (plist-put
            (plist-put
             (plist-put filing :draft-items remaining-items)
             :target-locations nil)
            :rejected-items (plist-get filing :rejected-items))
           :preview-text (delib-flow--draft-item-preview-text remaining-items))))
    (delib-flow--seed-reference-note-capture-review-block
     (delib-flow--seed-filing-selection-block
     (plist-put
      run :filing
      (if (plist-get raw :approval-blocked-p)
          (plist-put
           (plist-put
            (plist-put
             (plist-put
              (plist-put updated-filing :approved-items nil)
              :selection-blocked-item
              (plist-get raw :blocked-item))
             :selection-blocking-warnings
             (plist-get raw :blocking-warnings))
            :selection-blocked-selection
            (plist-get raw :operator-selection))
           :selection-blocked-notes
           (plist-get raw :operator-notes))
          (delib-flow--clear-filing-selection-block-state
           (plist-put updated-filing
                      :approved-items (plist-get raw :approved-items)))))))))

(defun delib-flow--apply-file-approved-outputs-entry (run entry)
  "Return RUN updated from completed file-approved-outputs ENTRY."
  (let* ((filing (plist-get run :filing))
         (raw (plist-get entry :raw-output))
         (conflicts (plist-get raw :conflicts))
         (filed-items (plist-get raw :filed-items))
         (filed-project
          (seq-find (lambda (item)
                      (eq (plist-get item :kind) 'project))
                    filed-items))
         (remaining-items
          (seq-remove
           (lambda (item)
             (seq-some (lambda (filed)
                         (equal (delib-flow--artifact-candidate-id filed)
                                (delib-flow--artifact-candidate-id item)))
                       filed-items))
           (plist-get filing :draft-items)))
         (remaining-note-candidates
          (seq-filter
           (lambda (item)
             (eq (plist-get item :kind) 'reference-note))
           remaining-items))
         (working (delib-flow--run-working-context run)))
    (if conflicts
        (delib-flow--seed-reference-note-capture-review-block
         (delib-flow--seed-filing-conflict-resolution-block
          (plist-put run :filing
                     (plist-put filing :conflicts conflicts))))
      (setq run
            (plist-put
             (plist-put
             run :working-context
             (if filed-project
                  (plist-put working :project-match
                             (delib-flow--filed-project-decision filed-project))
                working))
             :filing
             (plist-put
              (plist-put
               (delib-flow--clear-filing-selection-block-state
                (plist-put
                 (plist-put filing :approved-items nil)
                 :draft-items remaining-items))
               :conflicts nil)
              :target-locations
              (plist-get raw :target-locations))))
      (setq run
            (delib-flow--set-artifact-family-state
             run
             'reference-notes
             (list :candidates remaining-note-candidates
                   :selected-candidate-id nil
                   :selected-draft nil)))
      (delib-flow--seed-reference-note-capture-review-block run))))

(defun delib-flow--apply-resolve-filing-conflict-entry (run entry)
  "Return RUN updated from completed resolve-filing-conflict ENTRY."
  (let* ((filing (plist-get run :filing))
         (raw (plist-get entry :raw-output))
         (resolved (plist-get raw :resolved-approved-item))
         (updated-rejected
          (append (plist-get filing :rejected-items)
                  (plist-get raw :rejected-items))))
    (delib-flow--seed-reference-note-capture-review-block
     (plist-put
      run :filing
      (plist-put
       (plist-put
        (plist-put
         (plist-put filing :approved-items (if resolved (list resolved) nil))
         :target-locations nil)
        :rejected-items updated-rejected)
       :conflicts nil)))))

(defun delib-flow--review-record-from-entry (record entry)
  "Return accepted-result RECORD updated from completed stage ENTRY."
  (plist-put
   (plist-put
    (plist-put
     (plist-put record :candidate-stage-id (plist-get entry :stage-id))
     :candidate-output (plist-get entry :raw-output))
    :candidate-normalized-output (plist-get entry :normalized-output))
   :candidate-review-state (plist-get entry :review-state)))

(defun delib-flow--set-stage-entry-review-state (entries stage-id review-state)
  "Return ENTRIES with the latest STAGE-ID review-state set to REVIEW-STATE."
  (let ((updated nil))
    (mapcar
     (lambda (entry)
       (if (and (not updated)
                (eq (plist-get entry :stage-id) stage-id))
           (progn
             (setq updated t)
             (plist-put (copy-sequence entry) :review-state review-state))
         entry))
     (reverse entries))))

(defun delib-flow--update-stage-history-review-state (run stage-id review-state)
  "Return RUN with latest STAGE-ID history review-state set to REVIEW-STATE."
  (let* ((history (delib-flow--run-stage-history run))
         (entries (plist-get history :entries)))
    (plist-put
     run :stage-history
     (plist-put history :entries
                (reverse
                 (delib-flow--set-stage-entry-review-state
                  entries stage-id review-state))))))

(defun delib-flow--set-review-record-state (record review-state)
  "Return RECORD updated to REVIEW-STATE."
  (let ((candidate-output (plist-get record :candidate-output))
        (candidate-normalized-output
         (plist-get record :candidate-normalized-output))
        (stage-id (plist-get record :candidate-stage-id)))
    (if (eq review-state 'accepted)
        (plist-put
         (plist-put
          (plist-put
           (plist-put record :candidate-review-state 'accepted)
           :accepted-stage-id stage-id)
          :accepted-output candidate-output)
         :accepted-normalized-output candidate-normalized-output)
      (plist-put
       (plist-put
        (plist-put
         (plist-put record :candidate-review-state review-state)
         :accepted-stage-id nil)
        :accepted-output nil)
       :accepted-normalized-output nil))))

(defun delib-flow--set-stage-review-state (run stage-id review-state)
  "Return RUN with STAGE-ID review-state set to REVIEW-STATE."
  (let* ((working (delib-flow--run-working-context run))
         (record (delib-flow--review-record working stage-id))
         (updated-record (delib-flow--set-review-record-state
                          record review-state))
         (updated-records
          (delib-flow--replace-review-record
           (delib-flow--review-results working)
           stage-id
           updated-record))
         (updated-run
         (plist-put run :working-context
                     (plist-put working :review-results updated-records))))
    (delib-flow--update-stage-history-review-state
     updated-run stage-id review-state)))

(defun delib-flow--supersede-stage-review-state (run stage-id)
  "Return RUN with STAGE-ID review state marked superseded and acceptance cleared."
  (let* ((working (delib-flow--run-working-context run))
         (record (delib-flow--review-record working stage-id))
         (updated-record
          (plist-put
           (plist-put
            (plist-put
             (plist-put record :candidate-review-state 'superseded)
             :accepted-stage-id nil)
            :accepted-output nil)
           :accepted-normalized-output nil))
         (updated-records
          (delib-flow--replace-review-record
           (delib-flow--review-results working)
           stage-id
           updated-record))
         (updated-run
          (plist-put run :working-context
                     (plist-put working :review-results updated-records))))
    (delib-flow--update-stage-history-review-state
     updated-run stage-id 'superseded)))

(defun delib-flow--seed-manual-project-selection-block (run)
  "Return RUN with the manual project-selection block populated from current candidates."
  (let* ((package (delib-flow--stage-input-package run 'manual-project-match))
         (candidates (delib-flow--manual-project-match-candidates package))
         (block (delib-flow--editable-block run 'manual-project-selection))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--manual-project-selection-template candidates))))
    (delib-flow--set-editable-block run 'manual-project-selection updated-block)))

(defun delib-flow--seed-filing-selection-block (run)
  "Return RUN with filing-selection block populated from current draft items."
  (let* ((items (plist-get (plist-get run :filing) :draft-items))
         (block (delib-flow--editable-block run 'filing-selection-review))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--filing-selection-template items))))
    (delib-flow--set-editable-block run 'filing-selection-review updated-block)))

(defun delib-flow--seed-filing-conflict-resolution-block (run)
  "Return RUN with conflict-resolution block populated from current filing state."
  (let* ((block (delib-flow--editable-block run 'filing-conflict-resolution))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--filing-conflict-resolution-template run))))
    (delib-flow--set-editable-block run 'filing-conflict-resolution updated-block)))

(defun delib-flow--seed-reference-note-capture-review-block (run)
  "Return RUN with reference-note capture review block populated."
  (let* ((block (delib-flow--editable-block run 'reference-note-capture-review))
         (updated-block
          (delib-flow--seed-editable-block-text
           block
           (delib-flow--reference-note-capture-template run))))
    (delib-flow--set-editable-block
     run 'reference-note-capture-review updated-block)))

(defun delib-flow--seed-cloud-failure-review-block (run)
  "Return RUN with cloud-failure review block populated from current routing state."
  (let* ((block (delib-flow--editable-block run 'cloud-failure-review))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--cloud-failure-review-template run))))
    (delib-flow--set-editable-block run 'cloud-failure-review updated-block)))

(defun delib-flow--seed-cloud-routing-review-block (run)
  "Return RUN with cloud-routing review block populated from current routing state."
  (let* ((block (delib-flow--editable-block run 'cloud-routing-review))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--cloud-routing-review-template run))))
    (delib-flow--set-editable-block run 'cloud-routing-review updated-block)))

(defun delib-flow--clear-filing-selection-block-state (filing)
  "Return FILING with blocked approval state cleared."
  (plist-put
   (plist-put
    (plist-put
     (plist-put filing :selection-blocked-item nil)
     :selection-blocking-warnings nil)
    :selection-blocked-selection nil)
   :selection-blocked-notes nil))

(defun delib-flow--clear-cloud-failure-state (routing)
  "Return ROUTING with recorded cloud failure and fallback state cleared."
  (plist-put
   (plist-put
    (plist-put
     (plist-put routing :cloud-failure-stage nil)
     :cloud-failure-message nil)
    :cloud-fallback-mode nil)
   :reintegration-status nil))

(defun delib-flow--set-latest-stage-entry (run stage-id update-fn)
  "Return RUN with latest STAGE-ID history entry updated by UPDATE-FN."
  (let* ((history (delib-flow--run-stage-history run))
         (entries (plist-get history :entries))
         (updated nil))
    (plist-put
     run :stage-history
     (plist-put
      history
      :entries
      (reverse
       (mapcar
        (lambda (entry)
          (if (and (not updated)
                   (eq (plist-get entry :stage-id) stage-id))
              (progn
                (setq updated t)
                (funcall update-fn (copy-sequence entry)))
            entry))
        (reverse entries)))))))

(defun delib-flow--apply-inspect-source-type-override-to-output (output override notes)
  "Return OUTPUT updated with source-type OVERRIDE and NOTES."
  (let ((override-symbol (intern override)))
    (plist-put
     (plist-put
      (plist-put (copy-tree output) :source-type override-symbol)
      :source-type-reason
      (if (delib-flow--non-empty-string-p notes)
          (format "Operator override during review. %s" notes)
        "Operator override during review."))
     :source-type-signals
     (list "operator override"))))

(defun delib-flow--apply-inspect-source-review-edit (run)
  "Return RUN with any pending inspect review edits applied."
  (let ((override (delib-flow--inspect-source-type-override run)))
    (if (and override
             (delib-flow--valid-source-type-override-p override)
             (delib-flow--inspect-review-pending-p run))
        (let* ((notes (delib-flow--inspect-source-review-notes run))
               (working (delib-flow--run-working-context run))
               (candidate-output (delib-flow--inspect-candidate-output run))
               (updated-output
                (delib-flow--apply-inspect-source-type-override-to-output
                 candidate-output override notes))
               (review-record (delib-flow--review-record working 'inspect-source))
               (updated-record
                (plist-put
                 (plist-put review-record :candidate-output updated-output)
                 :candidate-normalized-output
                 (delib-flow--normalize-inspect-source-output updated-output)))
               (digest (or (plist-get working :email-inspect-digest)
                           (delib-flow--cached-email-inspect-digest-for-source
                            (delib-flow--run-source run))))
               (updated-working
                (plist-put
                 (plist-put
                  (plist-put working :inspect-output updated-output)
                  :email-inspect-digest digest)
                 :review-results
                 (delib-flow--replace-review-record
                  (delib-flow--review-results working)
                  'inspect-source
                  updated-record))))
          (delib-flow--set-latest-stage-entry
           (plist-put run :working-context updated-working)
           'inspect-source
           (lambda (entry)
             (plist-put
              (plist-put entry :raw-output updated-output)
              :normalized-output
              (delib-flow--normalize-inspect-source-output updated-output)))))
      run)))

(defun delib-flow--apply-inspect-review-outcome (run review-state decision)
  "Return RUN after setting inspect REVIEW-STATE and current DECISION."
  (let ((updated-run
         (delib-flow--set-stage-review-state run 'inspect-source review-state)))
    (plist-put
     updated-run :session
     (plist-put (delib-flow--run-session updated-run)
                :current-decision
                decision))))

(defun delib-flow--inspect-review-pending-p (run)
  "Return non-nil when RUN has inspect output pending review."
  (eq (delib-flow--stage-review-state run 'inspect-source) 'pending-review))

(defun delib-flow--apply-match-review-outcome (run review-state decision)
  "Return RUN after setting match REVIEW-STATE and current DECISION."
  (let* ((updated-run
          (delib-flow--set-stage-review-state run 'match-project review-state))
         (prepared-run
         (if (and (eq review-state 'accepted)
                   (memq (delib-flow--match-status updated-run) '(ambiguous no-match)))
              (delib-flow--seed-manual-project-selection-block updated-run)
            updated-run)))
    (plist-put
     prepared-run :session
     (plist-put (delib-flow--run-session prepared-run)
                :current-decision
                decision))))

(defun delib-flow--match-review-pending-p (run)
  "Return non-nil when RUN has a project match pending review."
  (eq (delib-flow--stage-review-state run 'match-project) 'pending-review))

(defun delib-flow--prepare-reviewable-stage-retry (run stage-id)
  "Return RUN prepared for a retry of reviewable STAGE-ID."
  (if (and (memq stage-id delib-flow--reviewable-stage-ids)
           (delib-flow--stage-executed-p run stage-id))
      (delib-flow--supersede-stage-review-state run stage-id)
    run))

(defun delib-flow--apply-reviewable-stage-entry (run entry)
  "Return RUN updated with accepted-result review state from ENTRY."
  (let* ((stage-id (plist-get entry :stage-id))
         (working (delib-flow--run-working-context run)))
    (if (memq stage-id delib-flow--reviewable-stage-ids)
        (let* ((record (delib-flow--review-record working stage-id))
               (updated-record (delib-flow--review-record-from-entry record entry))
               (updated-records
                (delib-flow--replace-review-record
                 (delib-flow--review-results working)
                 stage-id
                 updated-record)))
          (plist-put run :working-context
                     (plist-put working :review-results updated-records)))
      run)))

(defun delib-flow--completed-stage-apply-function (stage-id)
  "Return apply function for completed STAGE-ID."
  (alist-get stage-id delib-flow--stage-apply-function-alist))

(defun delib-flow--apply-completed-stage-entry (run entry)
  "Return RUN updated for completed stage ENTRY."
  (let ((updated-run (delib-flow--apply-reviewable-stage-entry run entry)))
    (if-let ((apply-fn
              (delib-flow--completed-stage-apply-function
               (plist-get entry :stage-id))))
        (funcall apply-fn updated-run entry)
      updated-run)))

(defun delib-flow--apply-stage-entry (run entry)
  "Return RUN updated for completed or failed stage ENTRY."
  (let* ((session (delib-flow--run-session run))
         (updated-run
          (plist-put run :session
                     (plist-put
                      (plist-put session :current-stage nil)
                      :current-decision
                      (delib-flow--next-decision-for-entry entry)))))
    (if (eq (plist-get entry :status) 'completed)
        (delib-flow--apply-completed-stage-entry updated-run entry)
      (if (eq (plist-get entry :stage-id) 'run-cloud-stage)
          (delib-flow--apply-run-cloud-stage-failure-entry updated-run entry)
        updated-run))))

(defun delib-flow--finalize-stage-run (run entry)
  "Return finalized RUN after applying stage ENTRY and audit updates."
  (delib-flow--seed-actions
   (delib-flow--finalize-audit-update
    (delib-flow--apply-stage-entry
     (delib-flow--append-stage-entry run entry)
     entry)
    entry)))

(defun delib-flow--stage-entry-with-times (entry started-at ended-at)
  "Return ENTRY updated with STARTED-AT and ENDED-AT timestamps."
  (plist-put
   (plist-put entry :started-at started-at)
   :ended-at ended-at))

(defun delib-flow--finalize-async-local-stage-success (prepared-run stage-id package started-at raw-output)
  "Return finalized RUN for async local STAGE-ID success."
  (let* ((normalized-output (delib-flow--normalize-stage-output stage-id raw-output))
         (entry (delib-flow--stage-entry-with-provider
                 (delib-flow--stage-entry-with-times
                  (delib-flow--make-stage-entry
                   stage-id package raw-output normalized-output)
                  started-at
                  (current-time))
                 'local)))
    (delib-flow--clear-stage-in-flight
     (delib-flow--finalize-stage-run prepared-run entry))))

(defun delib-flow--finalize-async-local-stage-error (prepared-run stage-id package started-at message)
  "Return finalized RUN for async local STAGE-ID failure MESSAGE."
  (let ((entry (delib-flow--stage-entry-with-provider
                (delib-flow--stage-entry-with-times
                 (delib-flow--make-stage-failure-entry
                  stage-id package message)
                 started-at
                 (current-time))
                'local)))
    (delib-flow--clear-stage-in-flight
     (delib-flow--finalize-stage-run prepared-run entry))))

(defun delib-flow--complete-async-local-stage (prepared-run stage-id package started-at request-id raw-output)
  "Apply RAW-OUTPUT for async local STAGE-ID when REQUEST-ID is current."
  (when (delib-flow--in-flight-request-current-p request-id)
    (setq delib-flow--active-run
          (delib-flow--finalize-async-local-stage-success
           prepared-run stage-id package started-at raw-output))
    (delib-flow--rerender-current-result)
    (delib-flow--refresh-in-flight-ui)))

(defun delib-flow--fail-async-local-stage (prepared-run stage-id package started-at request-id message)
  "Apply async local STAGE-ID failure MESSAGE when REQUEST-ID is current."
  (when (delib-flow--in-flight-request-current-p request-id)
    (setq delib-flow--active-run
          (delib-flow--finalize-async-local-stage-error
           prepared-run stage-id package started-at message))
    (delib-flow--rerender-current-result)
    (delib-flow--refresh-in-flight-ui)))

(defun delib-flow--start-async-local-stage (run stage-id)
  "Return RUN marked in-flight after starting async local STAGE-ID."
  (let* ((prepared-run (delib-flow--prepare-reviewable-stage-retry run stage-id))
         (descriptor (delib-flow--stage-descriptor stage-id))
         (package (delib-flow--stage-input-package prepared-run stage-id))
         (started-at (current-time))
         (request-id (delib-flow--in-flight-request-id))
         (model (or (plist-get package :selected-model)
                    (plist-get package :default-local-model)
                    delib-flow-default-local-model))
         (handle nil)
         (marked-run
          (delib-flow--mark-stage-in-flight
           prepared-run stage-id 'local model nil request-id started-at)))
    (setq delib-flow--active-run marked-run)
    (delib-flow--ensure-in-flight-ui-timer)
    (delib-flow--refresh-in-flight-ui)
    (setq handle
          (funcall
           delib-flow-local-stage-async-adapter
           descriptor
           package
           (lambda (raw-output)
             (delib-flow--complete-async-local-stage
              prepared-run stage-id package started-at request-id raw-output))
           (lambda (message)
             (delib-flow--fail-async-local-stage
              prepared-run stage-id package started-at request-id message))))
    (let ((still-in-flight-p
           (delib-flow--in-flight-request-current-p request-id)))
      (when still-in-flight-p
        (setq marked-run
              (delib-flow--mark-stage-in-flight
               marked-run stage-id 'local model handle request-id started-at))
        (setq delib-flow--active-run marked-run)
        (delib-flow--refresh-in-flight-ui))
      (delib-flow--seed-actions
       (if still-in-flight-p
           marked-run
         delib-flow--active-run)))))

(defun delib-flow--run-stage-locally (run stage-id)
  "Return RUN after executing STAGE-ID through the local stage contract."
  (let* ((prepared-run (delib-flow--prepare-reviewable-stage-retry run stage-id))
         (descriptor (delib-flow--stage-descriptor stage-id))
         (package (delib-flow--stage-input-package prepared-run stage-id)))
    (condition-case err
        (let* ((raw-output (funcall delib-flow-local-stage-adapter
                                    descriptor package))
               (normalized-output
                (delib-flow--normalize-stage-output stage-id raw-output))
               (entry (delib-flow--stage-entry-with-provider
                       (delib-flow--make-stage-entry
                        stage-id package raw-output normalized-output)
                       'local)))
          (delib-flow--finalize-stage-run prepared-run entry))
      (error
       (let ((entry
              (delib-flow--stage-entry-with-provider
               (delib-flow--make-stage-failure-entry
                stage-id package (error-message-string err))
               'local)))
         (delib-flow--finalize-stage-run prepared-run entry))))))

(defun delib-flow--execute-local-stage (run stage-id)
  "Execute STAGE-ID for RUN, using async local execution when configured."
  (setq run (delib-flow--normalize-in-flight-state run))
  (if delib-flow-local-stage-async-adapter
      (progn
        (when (delib-flow--run-in-flight-p run)
          (user-error "Wait for %s to finish before starting another stage"
                      (delib-flow--stage-label
                       (delib-flow--run-in-flight-stage-id run))))
        (delib-flow--start-async-local-stage run stage-id))
    (delib-flow--run-stage-locally run stage-id)))

(defun delib-flow--run-stage-in-cloud (run stage-id)
  "Return RUN after executing STAGE-ID through the cloud stage contract."
  (let* ((prepared-run (delib-flow--prepare-reviewable-stage-retry run stage-id))
         (descriptor (delib-flow--stage-descriptor stage-id))
         (package (delib-flow--stage-input-package prepared-run stage-id)))
    (condition-case err
        (let* ((raw-output (funcall delib-flow-cloud-stage-adapter
                                    descriptor package))
               (normalized-output
                (delib-flow--normalize-stage-output stage-id raw-output))
               (entry (delib-flow--stage-entry-with-provider
                       (delib-flow--make-stage-entry
                        stage-id package raw-output normalized-output)
                       'cloud)))
          (delib-flow--finalize-stage-run prepared-run entry))
      (error
       (let ((entry
              (delib-flow--stage-entry-with-provider
               (delib-flow--make-stage-failure-entry
                stage-id package (error-message-string err))
               'cloud)))
         (delib-flow--finalize-stage-run prepared-run entry))))))

(defun delib-flow--initialize-run (source-snapshot)
  "Create a new run state from SOURCE-SNAPSHOT."
  (delib-flow--clear-sticky-anchor)
  (let ((session (delib-flow--initial-session-state)))
    (delib-flow--seed-actions
     (delib-flow--seed-reference-note-capture-review-block
      (delib-flow--seed-cloud-routing-review-block
      (list :source source-snapshot
         :working-context
         (list :source-snapshot source-snapshot
               :inspect-output nil
               :email-inspect-digest nil
               :project-match nil
               :review-results (delib-flow--initial-review-results)
               :retrieved-candidates nil
               :filtered-context nil
               :retained-context nil
               :cloud-sanitized-context nil
               :cloud-returned-context nil
               :cloud-returned-stage-id nil
               :cloud-returned-stage-raw-output nil
               :cloud-returned-stage-normalized-output nil
               :editable-block-ids '(context-main operator-notes manual-project-selection filing-selection-review filing-conflict-resolution reference-note-capture-review inspect-source-review cloud-package-review cloud-routing-review cloud-failure-review))
         :stage-history
         (list :entries nil
               :latest-stage nil
               :latest-status nil)
         :actions
         (delib-flow--initial-actions-state)
         :artifacts
         (delib-flow--initial-artifact-state)
         :routing
         (list :default-local-model delib-flow-default-local-model
               :default-cloud-model delib-flow-default-cloud-model
               :stage-models nil
               :cloud-switch-pending nil
               :cloud-target-stage nil
               :selected-cloud-provider nil
               :cloud-policy-profile nil
               :cloud-failure-stage nil
               :cloud-failure-message nil
               :cloud-fallback-mode nil
               :cloud-reintegrated-stage-ids nil
               :sanitization-status nil
               :reintegration-status nil)
         :filing
         (list :draft-items nil
               :approved-items nil
               :rejected-items nil
               :preview-text nil
               :selection-blocked-item nil
               :selection-blocking-warnings nil
               :selection-blocked-selection nil
               :selection-blocked-notes nil
               :conflicts nil
               :target-locations nil)
         :audit
         (delib-flow--initial-audit-state source-snapshot session)
         :session
         session
         :ui
         (delib-flow--initial-ui-state)))))))

(defun delib-flow--section-content (section run)
  "Return Org text for SECTION using RUN state."
  (let ((renderer (cdr (assoc section delib-flow--section-renderer-alist))))
    (if renderer
        (funcall renderer run)
      "")))

(defun delib-flow--render-control-buffer (run)
  "Render the control buffer from RUN."
  (let ((buffer (get-buffer-create delib-flow-control-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (preserve-focus-mode delib-flow-control-focus-mode))
        (erase-buffer)
        (delib-flow-control-mode)
        (setq-local delib-flow-control-focus-mode preserve-focus-mode)
        (insert (format "* DeliberateFlow -- %s\n"
                        (or (plist-get (delib-flow--run-source run) :title)
                            "Untitled source")))
        (dolist (section delib-flow--control-sections)
          (insert (format "** %s\n" section))
          (insert (delib-flow--section-content section run))
          (unless (bolp) (insert "\n")))
        (setq buffer-read-only nil)
        (delib-flow--protect-managed-regions)
        (delib-flow--annotate-action-lines run)
        (goto-char (point-min)))
      (setq-local delib-flow--active-run-buffer t))
    buffer))

(defun delib-flow--filing-preview-visible-p (run)
  "Return non-nil when filing preview should be expanded for RUN."
  (let ((filing (plist-get run :filing)))
    (or (plist-get filing :draft-items)
        (plist-get filing :approved-items)
        (plist-get filing :rejected-items)
        (plist-get filing :conflicts)
        (plist-get filing :target-locations))))

(defun delib-flow--section-heading-position (section)
  "Return buffer position of top-level SECTION heading, if present."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (delib-flow--section-heading section) nil t)
      (line-beginning-position))))

(defun delib-flow--heading-position (heading)
  "Return buffer position of HEADING, if present at any Org depth."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^\\*+ %s$" (regexp-quote heading))
           nil t)
      (line-beginning-position))))

(defun delib-flow--subtree-content-bounds (heading)
  "Return the content bounds for HEADING in the current buffer."
  (save-excursion
    (when-let ((position (delib-flow--heading-position heading)))
      (goto-char position)
      (org-back-to-heading t)
      (forward-line 1)
      (let ((start (point))
            (end (save-excursion
                   (org-end-of-subtree t t)
                   (point))))
        (cons start end)))))

(defun delib-flow--current-section-at-point ()
  "Return the current top-level control section at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (let ((heading (org-get-heading t t t t)))
        (when (member heading delib-flow--control-sections)
          heading)))))

(defun delib-flow--current-top-level-section-at-point ()
  "Return the enclosing top-level control section at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (while (> (or (org-current-level) 0) 2)
        (org-up-heading-safe))
      (let ((heading (org-get-heading t t t t)))
        (when (member heading delib-flow--control-sections)
          heading)))))

(defun delib-flow--current-heading-at-point ()
  "Return the exact Org heading at point, if any."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t))
      (org-get-heading t t t t))))

(defun delib-flow--action-text-line-count (action)
  "Return the rendered line count for ACTION."
  (length (split-string (string-trim-right (delib-flow--format-action-line action))
                        "\n")))

(defun delib-flow--action-help-text (action)
  "Return help text for ACTION."
  (let ((label (plist-get action :label))
        (id (plist-get action :id))
        (status (plist-get action :status))
        (reason (plist-get action :reason))
        (command (plist-get action :handler)))
    (format "%s\nid: %s\nstatus: %s\ncommand: %s%s"
            label
            id
            status
            command
            (if reason
                (format "\nreason: %s" reason)
              ""))))

(defun delib-flow--annotate-rendered-action-group (bounds actions)
  "Attach ACTION metadata within BOUNDS in the current buffer."
  (when bounds
    (save-excursion
      (goto-char (car bounds))
      (dolist (action actions)
        (let ((start (line-beginning-position)))
          (forward-line (delib-flow--action-text-line-count action))
          (add-text-properties
           start (point)
           `(delib-flow-action ,action
                               mouse-face highlight
                               help-echo ,(delib-flow--action-help-text action)
                               follow-link t)))))))

(defun delib-flow--annotate-action-lines (run)
  "Attach action metadata for RUN to rendered lines in the current buffer."
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Recommended next pass")
   (if-let ((action (delib-flow--recommended-action run)))
       (list action)
     nil))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Quick actions")
   (delib-flow--quick-actions run))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--section-content-bounds "Next actions")
   (delib-flow--sorted-actions run))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Recommended next filing pass")
   (if-let ((action (delib-flow--recommended-filing-action run)))
       (list action)
     nil))
  (delib-flow--annotate-rendered-action-group
   (delib-flow--subtree-content-bounds "Filing actions")
   (delib-flow--filing-preview-actions run)))

(defun delib-flow--action-at-point ()
  "Return the rendered action object at point, if any."
  (or (get-text-property (point) 'delib-flow-action)
      (get-text-property (line-beginning-position) 'delib-flow-action)))

(defun delib-flow--action-region-end (position)
  "Return the end position of the rendered action region at POSITION."
  (or (next-single-property-change
       position
       'delib-flow-action
       nil
       (point-max))
      (point-max)))

(defun delib-flow--next-action-position (&optional position)
  "Return the next rendered action position after POSITION."
  (save-excursion
    (let ((pos (or position (point))))
      (when (get-text-property pos 'delib-flow-action)
        (setq pos (delib-flow--action-region-end pos)))
      (while (and (< pos (point-max))
                  (not (get-text-property pos 'delib-flow-action)))
        (setq pos (or (next-single-property-change
                       pos
                       'delib-flow-action
                       nil
                       (point-max))
                      (point-max))))
      (when (< pos (point-max))
        pos))))

(defun delib-flow--previous-action-position (&optional position)
  "Return the previous rendered action position before POSITION."
  (save-excursion
    (let ((pos (max (point-min) (1- (or position (point))))))
      (when (and (> pos (point-min))
                 (get-text-property pos 'delib-flow-action))
        (while (and (> pos (point-min))
                    (get-text-property (1- pos) 'delib-flow-action))
          (setq pos (1- pos)))
        (setq pos (1- pos)))
      (while (and (>= pos (point-min))
                  (not (get-text-property pos 'delib-flow-action)))
        (setq pos (or (previous-single-property-change
                       pos
                       'delib-flow-action
                       nil
                       (point-min))
                      (1- (point-min)))))
      (when (>= pos (point-min))
        (while (and (> pos (point-min))
                    (get-text-property (1- pos) 'delib-flow-action))
          (setq pos (1- pos)))
        pos))))

(defun delib-flow--dispatch-rendered-action (action)
  "Execute rendered ACTION when it is available."
  (let ((status (plist-get action :status))
        (reason (plist-get action :reason))
        (command (plist-get action :handler)))
    (if (eq status 'available)
        (call-interactively command)
      (user-error "%s"
                  (or reason
                      (format "%s is not available"
                              (plist-get action :label)))))))

(defun delib-flow--current-reviewable-stage ()
  "Return the current reviewable stage id, if any."
  (cond
   ((delib-flow--inspect-review-pending-p delib-flow--active-run) 'inspect-source)
   ((delib-flow--match-review-pending-p delib-flow--active-run) 'match-project)))

(defun delib-flow--approve-stage-command (stage-id)
  "Return the approve command for reviewable STAGE-ID."
  (alist-get stage-id '((inspect-source . delib-flow-action-accept-inspect-source)
                        (match-project . delib-flow-action-accept-match-project))))

(defun delib-flow--retry-stage-command (stage-id)
  "Return the retry command for reviewable STAGE-ID."
  (alist-get stage-id '((inspect-source . delib-flow-action-inspect-source)
                        (match-project . delib-flow-action-match-project))))

(defun delib-flow--call-current-review-command (command-fn error-message)
  "Call current review COMMAND-FN or signal ERROR-MESSAGE."
  (if-let* ((stage-id (delib-flow--current-reviewable-stage))
            (command (funcall command-fn stage-id)))
      (call-interactively command)
    (user-error "%s" error-message)))

(defun delib-flow--preferred-anchor-section (run)
  "Return preferred anchor heading for RUN."
  (delib-flow--active-loop-heading run))

(defun delib-flow--changed-heading-for-run (run)
  "Return the most relevant changed heading for RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (pcase (plist-get entry :stage-id)
        ((or 'inspect-source 'match-project 'run-cloud-stage)
         "Loop update")
        ('manual-project-match
         "Manual project selection")
        ((or 'extract-actions 'extract-waiting-for 'suggest-reference-notes
             'propose-new-project 'integrate-into-source
             'select-approved-filing-actions 'reject-draft-filing-artifact
             'file-approved-outputs 'resolve-filing-conflict)
         "Filing update")
        ((or 'discover-reference-material 'filter-reference-material)
         "Current context")
        (_
         (delib-flow--active-loop-heading run)))
    "Decision strip"))

(defun delib-flow--highlight-changed-heading (run)
  "Highlight the most relevant changed heading for RUN in the current buffer."
  (when (overlayp delib-flow--changed-heading-overlay)
    (delete-overlay delib-flow--changed-heading-overlay)
    (setq-local delib-flow--changed-heading-overlay nil))
  (when-let ((position (delib-flow--heading-position
                        (delib-flow--changed-heading-for-run run))))
    (save-excursion
      (goto-char position)
      (let ((overlay (make-overlay
                      (line-beginning-position)
                      (line-end-position))))
        (overlay-put overlay 'face 'highlight)
        (overlay-put overlay 'priority 1001)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'delib-flow-changed-heading t)
        (setq-local delib-flow--changed-heading-overlay overlay)))))

(defun delib-flow--apply-visibility-policy (run)
  "Apply control-buffer visibility policy for RUN in the current buffer."
  (save-excursion
    (org-overview)
    (dolist (section (if delib-flow-control-focus-mode
                         '("Now")
                       '("Now" "Current result" "Filing preview" "Next actions" "Current context")))
      (when-let ((position (delib-flow--section-heading-position section)))
        (goto-char position)
        (org-show-subtree)))
    (when delib-flow-control-focus-mode
      (when-let ((position (delib-flow--section-heading-position
                            (cond
                             ((delib-flow--run-in-flight-p run)
                              "Current result")
                             ((or (delib-flow--inspect-review-pending-p run)
                                  (delib-flow--match-review-pending-p run))
                              "Current result")
                             ((delib-flow--filing-preview-visible-p run)
                              "Filing preview")
                             (t
                              "Next actions")))))
        (goto-char position)
        (org-show-subtree)))
    (when (delib-flow--filing-preview-visible-p run)
      (dolist (heading '("What to do next"
                         "Current filing plan"
                         "Current filing choice"
                         "Selected note draft"
                         "Regenerate selected note"
                         "Filing actions"
                         "Planned file targets"
                         "Staged content preview"))
        (when-let ((position (delib-flow--heading-position heading)))
          (goto-char position)
          (org-show-subtree))))
    (when (delib-flow--filing-selection-active-p run)
      (when-let ((position (delib-flow--heading-position "Artifact selection")))
        (goto-char position)
        (org-show-subtree)))
    (when (delib-flow--filing-conflict-resolution-active-p run)
      (when-let ((position (delib-flow--heading-position "Conflict resolution")))
        (goto-char position)
        (org-show-subtree)))))

(defun delib-flow--goto-section (heading)
  "Move point to HEADING when present."
  (when-let ((position (or (delib-flow--section-heading-position heading)
                           (delib-flow--heading-position heading))))
    (goto-char position)
    t))

(defun delib-flow--align-heading-top ()
  "Place the current heading at the top of any visible window for this buffer."
  (let ((start (save-excursion
                 (org-back-to-heading t)
                 (line-beginning-position))))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (set-window-point window (point))
      (set-window-start window start))))

(defun delib-flow--staged-content-preview-text-available-p (run)
  "Return non-nil when RUN has a meaningful staged-content preview."
  (let ((text (delib-flow--staged-content-preview-text run)))
    (and text
         (not (string-prefix-p "No staged content preview is available yet." text))
         (not (string-prefix-p "Staged content preview is unavailable:" text)))))

(defun delib-flow--finalize-rendered-active-run-buffer (run buffer anchor-section)
  "Apply final visibility, anchoring, and conflict recovery for RUN in BUFFER."
  (with-current-buffer buffer
    (delib-flow--apply-visibility-policy run)
    (unless (delib-flow--goto-section anchor-section)
      (goto-char (point-min)))
    (delib-flow--align-heading-top)
    (delib-flow--highlight-changed-heading run))
  (when (and delib-flow--active-run
             (eq run delib-flow--active-run))
    (let ((recorded (plist-get (delib-flow--run-ui run)
                               :managed-region-conflicts)))
      (when recorded
        (setq delib-flow--active-run
              (delib-flow--seed-actions
               (delib-flow--set-managed-region-conflicts
                delib-flow--active-run
                nil)))
        (setq run delib-flow--active-run)
        (setq buffer (delib-flow--render-control-buffer run))
        (with-current-buffer buffer
          (delib-flow--apply-visibility-policy run)
          (unless (delib-flow--goto-section anchor-section)
            (goto-char (point-min)))
          (delib-flow--align-heading-top)
          (delib-flow--highlight-changed-heading run)))))
  (delib-flow--refresh-staged-content-preview-buffer run)
  buffer)

(defun delib-flow--render-active-run-buffer (run &optional anchor-section)
  "Return the control buffer freshly rendered from RUN.

When ANCHOR-SECTION is non-nil, move point to that top-level section."
  (let ((buffer (delib-flow--render-control-buffer run)))
    (with-current-buffer buffer
      (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
    (delib-flow--finalize-rendered-active-run-buffer run buffer anchor-section)))

(defun delib-flow--set-sticky-anchor (heading)
  "Persist HEADING as the preferred rerender anchor."
  (setq delib-flow--sticky-anchor-heading heading))

(defun delib-flow--clear-sticky-anchor ()
  "Clear any persisted rerender anchor."
  (setq delib-flow--sticky-anchor-heading nil))

(defun delib-flow--refresh-buffer-sections (run buffer)
  "Refresh BUFFER section-by-section from RUN."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (dolist (section delib-flow--control-sections)
        (delib-flow--replace-section-content
         section
         (delib-flow--section-content section run)))
      (delib-flow--protect-managed-regions)
      (goto-char (point-min))))
  buffer)

(defun delib-flow--teardown-active-run ()
  "Clear active run state."
  (when delib-flow--active-run
    (delib-flow--cancel-in-flight-stage delib-flow--active-run)
    (delib-flow--cleanup-debug-fixture
     (plist-get (delib-flow--run-session delib-flow--active-run)
                :debug-fixture)))
  (when (timerp delib-flow--in-flight-ui-timer)
    (cancel-timer delib-flow--in-flight-ui-timer)
    (setq delib-flow--in-flight-ui-timer nil))
  (delib-flow--clear-sticky-anchor)
  (setq delib-flow--active-run nil))

(defun delib-flow--mark-run-aborted (run)
  "Return RUN with aborted session state."
  (plist-put run :session
             (plist-put
              (plist-put
               (plist-put
                (plist-put (delib-flow--run-session run) :status 'aborted)
                :active nil)
               :aborted t)
              :ended-at (current-time))))

(defun delib-flow--sync-run-from-control-buffer (run)
  "Return RUN after syncing editable blocks from the live control buffer."
  (let ((buffer (delib-flow--control-buffer)))
    (if (buffer-live-p buffer)
        (delib-flow--sync-editable-blocks run buffer)
      run)))

(defun delib-flow--control-buffer-killed ()
  "Handle control buffer teardown."
  (when (bound-and-true-p delib-flow--active-run-buffer)
    (delib-flow--teardown-active-run)))

(defun delib-flow--rerender-active-run-buffer (&optional forced-anchor)
  "Rerender the live control buffer from `delib-flow--active-run'.

When FORCED-ANCHOR is non-nil, anchor to that heading instead of preserving
the current local heading."
  (let* ((buffer (delib-flow--control-buffer))
         (preserved-heading
          (when (and (null forced-anchor)
                     (buffer-live-p buffer))
            (with-current-buffer buffer
              (save-excursion
                (delib-flow--current-heading-at-point)))))
         (anchor
          (if forced-anchor
              forced-anchor
            (or delib-flow--sticky-anchor-heading
                (if (delib-flow--run-in-flight-p delib-flow--active-run)
                    (delib-flow--preferred-anchor-section delib-flow--active-run)
                  (or preserved-heading
                      (delib-flow--preferred-anchor-section delib-flow--active-run)))))))
    (delib-flow--render-active-run-buffer
     delib-flow--active-run
     anchor)))

(defun delib-flow--rerender-current-result ()
  "Rerender the active control buffer anchored to `Current result'."
  (delib-flow--set-sticky-anchor "Current result")
  (delib-flow--rerender-active-run-buffer "Current result"))

(defun delib-flow-action-inspect-source ()
  "Execute the inspect-source stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
        (delib-flow--execute-local-stage delib-flow--active-run
                                         'inspect-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-match-project ()
  "Execute the match-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--stage-accepted-p delib-flow--active-run 'inspect-source)
    (user-error "Inspect result must be accepted before project matching"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'match-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-accept-inspect-source ()
  "Accept the current inspect-source result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--inspect-review-pending-p delib-flow--active-run)
    (user-error "No inspect result is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-inspect-review-outcome
           (delib-flow--apply-inspect-source-review-edit
            (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
           'accepted
           "Inspect result accepted. You may now match the project or retry inspect.")
          'inspect-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-reject-inspect-source ()
  "Reject the current inspect-source result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--inspect-review-pending-p delib-flow--active-run)
    (user-error "No inspect result is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-inspect-review-outcome
           delib-flow--active-run
           'rejected
           "Inspect result rejected. Retry inspect before matching a project.")
          'inspect-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-accept-match-project ()
  "Accept the current match-project result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--match-review-pending-p delib-flow--active-run)
    (user-error "No project match is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-match-review-outcome
           delib-flow--active-run
           'accepted
           (if (memq (delib-flow--match-status delib-flow--active-run) '(ambiguous no-match))
               "Project match accepted for manual review. Edit the manual project selection block, then choose a project manually or continue with no-match follow-up."
             "Project match accepted. Continue with downstream stages as appropriate."))
          'match-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-reject-match-project ()
  "Reject the current match-project result for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--match-review-pending-p delib-flow--active-run)
    (user-error "No project match is pending review"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--refresh-run-audit
          (delib-flow--apply-match-review-outcome
           delib-flow--active-run
           'rejected
           "Project match rejected. Retry project matching before downstream project-dependent stages.")
          'match-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-discover-reference-material ()
  "Execute the discover-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (or (delib-flow--project-decision-ready-p delib-flow--active-run)
              (delib-flow--stage-accepted-p delib-flow--active-run 'inspect-source))
    (user-error "Accept Inspect Source before reference discovery"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'discover-reference-material)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-filter-reference-material ()
  "Execute the filter-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--stage-executed-p delib-flow--active-run
                                        'discover-reference-material)
    (user-error "Reference discovery must run before filtering retained material"))
  (unless (plist-get (delib-flow--run-working-context delib-flow--active-run)
                     :retrieved-candidates)
    (user-error "No discovered reference material is available to filter"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'filter-reference-material)))
  (delib-flow--rerender-current-result))

(defun delib-flow-choose-manual-project ()
  "Choose a valid manual project candidate with completion."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (or (delib-flow--stage-executed-p delib-flow--active-run 'manual-project-match)
              (and (memq (delib-flow--stage-review-state delib-flow--active-run
                                                         'match-project)
                         '(accepted rejected))
                   (memq (delib-flow--match-status delib-flow--active-run)
                         '(matched ambiguous no-match))))
    (user-error "Accept or reject the current project result before manual override"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (labels (delib-flow--manual-project-selection-labels synced-run))
         (current (delib-flow--manual-project-selection-value
                   (delib-flow--stage-input-package synced-run 'manual-project-match)))
         (choice (completing-read
                  "Manual project: "
                  (mapcar #'car labels)
                  nil t nil nil
                  (car (rassoc current labels)))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--set-manual-project-selection-value
            synced-run
            (or (cdr (assoc choice labels))
                choice))))
    (delib-flow--clear-sticky-anchor)
    (delib-flow--rerender-active-run-buffer)))

(defun delib-flow-action-manual-project-match ()
  "Execute the manual-project-match stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (or (delib-flow--stage-executed-p delib-flow--active-run 'manual-project-match)
              (and (memq (delib-flow--stage-review-state delib-flow--active-run
                                                         'match-project)
                         '(accepted rejected))
                   (memq (delib-flow--match-status delib-flow--active-run)
                         '(matched ambiguous no-match))))
    (user-error "Accept or reject the current project result before manual override"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (synced-selection
          (delib-flow--manual-project-selection-value
           (delib-flow--stage-input-package synced-run 'manual-project-match)))
         (active-selection
          (delib-flow--manual-project-selection-value
           (delib-flow--stage-input-package delib-flow--active-run
                                            'manual-project-match)))
         (selected-run
          (cond
           ((and (delib-flow--stage-executed-p delib-flow--active-run
                                               'manual-project-match)
                 (delib-flow--manual-project-selection-valid-p delib-flow--active-run)
                 (not (string-equal (or active-selection "")
                                    (or synced-selection ""))))
            delib-flow--active-run)
           ((delib-flow--manual-project-selection-valid-p synced-run)
            synced-run)
           (t
            delib-flow--active-run))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--execute-local-stage selected-run
                                            'manual-project-match))))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-propose-new-project ()
  "Execute the propose-new-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--project-proposal-ready-p delib-flow--active-run)
    (user-error "A reviewed ambiguous or no-match project decision is required before proposing a new project"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'propose-new-project)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-extract-actions ()
  "Execute the extract-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (and (delib-flow--project-decision-ready-p delib-flow--active-run)
               (eq (delib-flow--match-status delib-flow--active-run) 'matched))
    (user-error "Accepted or manual project match is required before extracting actions"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'extract-actions)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-extract-waiting-for ()
  "Execute the extract-waiting-for stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (and (delib-flow--project-decision-ready-p delib-flow--active-run)
               (eq (delib-flow--match-status delib-flow--active-run) 'matched))
    (user-error "Accepted or manual project match is required before extracting waiting-for items"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'extract-waiting-for)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-suggest-reference-notes ()
  "Execute the suggest-reference-notes stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--reference-note-suggestion-ready-p delib-flow--active-run)
    (user-error "Accepted inspect result is required before suggesting reference notes"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'suggest-reference-notes)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-draft-selected-reference-note ()
  "Execute the selected-note drafting stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (eq (plist-get (delib-flow--selected-reference-note-candidate-for-drafting
                          delib-flow--active-run)
                         :kind)
              'reference-note)
    (user-error "Select one reference-note candidate before drafting it"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage delib-flow--active-run
                                          'draft-selected-reference-note)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-decide-cloud-pass ()
  "Execute the decide-cloud-pass stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'decide-cloud-pass)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-sanitize-for-cloud ()
  "Execute the sanitize-for-cloud stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'sanitize-for-cloud)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-approve-cloud-send ()
  "Execute the approve-cloud-send stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'approve-cloud-send)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-restart-cloud-path ()
  "Restart the current cloud path for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--restart-cloud-path-run
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run))))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-run-cloud-stage ()
  "Execute the run-cloud-stage stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (if (delib-flow--run-cloud-stage-p 'run-cloud-stage)
            (delib-flow--run-stage-in-cloud
             (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
             'run-cloud-stage)
          (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
           'run-cloud-stage)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-retry-rerouted-cloud-stage ()
  "Retry the current rerouted cloud target for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--direct-cloud-retry-ready-p delib-flow--active-run)
    (user-error "No rerouted cloud stage is ready for direct retry"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-in-cloud
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'run-cloud-stage)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-resolve-cloud-failure ()
  "Execute the resolve-cloud-failure stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'resolve-cloud-failure)))
  (if (eq (plist-get (delib-flow--run-session delib-flow--active-run) :status)
          'aborted)
      (let ((buffer (delib-flow--control-buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (delib-flow--teardown-active-run))
    (delib-flow--rerender-current-result)))

(defun delib-flow-action-approve-candidate-reintegration ()
  "Execute the approve-candidate-reintegration stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'approve-candidate-reintegration)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-integrate-into-source ()
  "Execute the integrate-into-source stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'integrate-into-source)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-reject-draft-filing-artifact ()
  "Execute the reject-draft-filing-artifact stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((synced-run
         (delib-flow--validate-filing-selection-entry
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--execute-local-stage
            synced-run
            'reject-draft-filing-artifact))))
  (delib-flow--rerender-current-result))

(defun delib-flow-choose-filing-selection ()
  "Choose a valid filing artifact selection with completion."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (delib-flow--filing-selection-active-p delib-flow--active-run)
    (user-error "No draft filing artifacts are available to choose from"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (labels (delib-flow--filing-selection-labels synced-run))
         (current (delib-flow--filing-selection-value
                   (delib-flow--stage-input-package synced-run
                                                    'select-approved-filing-actions)))
         (choice (completing-read
                  "Filing selection: "
                  (mapcar #'car labels)
                  nil t nil nil
                  (car (rassoc current labels)))))
    (setq delib-flow--active-run
          (delib-flow--set-filing-selection-value
           synced-run
           (or (cdr (assoc choice labels))
               choice)))
    (delib-flow--clear-sticky-anchor)
    (delib-flow--rerender-active-run-buffer)))

(defun delib-flow-choose-reference-note-template ()
  "Choose an org-roam template for the active reference-note filing artifact."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((synced-run (delib-flow--sync-run-from-control-buffer delib-flow--active-run))
         (package (delib-flow--stage-input-package synced-run 'file-approved-outputs))
         (item (delib-flow--reference-note-preview-item package)))
    (unless (eq (plist-get item :kind) 'reference-note)
      (user-error "No reference-note filing artifact is currently active"))
    (let* ((options (delib-flow--reference-note-capture-template-options item))
           (current (delib-flow--reference-note-effective-template-key item package))
           (choice (completing-read
                    "Reference note template: "
                    (mapcar #'car options)
                    nil t nil nil
                    (car (rassoc current options))))
           (selected-key (or (cdr (assoc choice options))
                             choice))
           (updated-run
            (delib-flow--set-reference-note-capture-field
             synced-run
             "Template key"
             selected-key))
           (selected-package
            (delib-flow--stage-input-package updated-run 'file-approved-outputs))
           (path-required
            (delib-flow--reference-note-org-roam-target-requires-path-p
             item selected-package))
           (current-path
            (delib-flow--reference-note-effective-target-override selected-package)))
      (when path-required
        (setq updated-run
              (delib-flow--set-reference-note-capture-field
               updated-run
               "Target path"
               (read-string
                "Reference note target path (relative to org-roam root, without .org): "
                current-path))))
      (unless path-required
        (setq updated-run
              (delib-flow--set-reference-note-capture-field
               updated-run
               "Target path"
               "")))
      (setq delib-flow--active-run updated-run)
      (delib-flow--clear-sticky-anchor)
      (delib-flow--rerender-active-run-buffer))))

(defun delib-flow-action-select-approved-filing-actions ()
  "Execute the select-approved-filing-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((synced-run
         (delib-flow--validate-filing-selection-entry
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--execute-local-stage
            synced-run
            'select-approved-filing-actions))))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-file-approved-outputs ()
  "Execute the file-approved-outputs stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'file-approved-outputs)))
  (delib-flow--rerender-current-result)
  (delib-flow--show-filed-target-locations delib-flow--active-run))

(defun delib-flow-action-resolve-filing-conflict ()
  "Execute the resolve-filing-conflict stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--execute-local-stage
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'resolve-filing-conflict)))
  (delib-flow--rerender-current-result))

(defun delib-flow-action-stage-placeholder ()
  "Signal that the selected stage exists but is not yet implemented."
  (interactive)
  (user-error "This stage is not implemented yet"))

(defun delib-flow-refresh ()
  "Public command to refresh the active control buffer."
  (interactive)
  (delib-flow-refresh-buffer))

(defun delib-flow-dispatch-action ()
  "Execute the rendered action at point in the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (if-let ((action (delib-flow--action-at-point)))
      (delib-flow--dispatch-rendered-action action)
    (user-error "No delib-flow action is available at point")))

(defun delib-flow--shortcut-event-string ()
  "Return the current shortcut key as a string."
  (char-to-string last-command-event))

(defun delib-flow--action-for-shortcut (run shortcut)
  "Return the rendered action from RUN bound to SHORTCUT."
  (seq-find
   (lambda (action)
     (string= (plist-get action :shortcut) shortcut))
   (delib-flow--sorted-actions run)))

(defun delib-flow-dispatch-action-shortcut ()
  "Execute the rendered action bound to the typed shortcut key."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (let ((shortcut (delib-flow--shortcut-event-string)))
    (if-let ((action (delib-flow--action-for-shortcut delib-flow--active-run
                                                      shortcut)))
        (delib-flow--dispatch-rendered-action action)
      (user-error "No delib-flow action is bound to `%s` right now" shortcut))))

(defun delib-flow-next-section ()
  "Move point to the next top-level cockpit section."
  (interactive)
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (delib-flow--clear-sticky-anchor)
  (let* ((current (delib-flow--current-top-level-section-at-point))
         (sections delib-flow--control-sections)
         (remaining (cdr (member current sections)))
         (target (or (car remaining) (car sections))))
    (unless (delib-flow--goto-section target)
      (user-error "No control section is available"))
    (delib-flow--align-heading-top)))

(defun delib-flow-previous-section ()
  "Move point to the previous top-level cockpit section."
  (interactive)
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (delib-flow--clear-sticky-anchor)
  (let* ((current (delib-flow--current-top-level-section-at-point))
         (sections delib-flow--control-sections)
         (before (seq-take-while (lambda (section)
                                   (not (equal section current)))
                                 sections))
         (target (or (car (last before))
                     (car (last sections)))))
    (unless (delib-flow--goto-section target)
      (user-error "No control section is available"))
    (delib-flow--align-heading-top)))

(defun delib-flow-next-action ()
  "Move point to the next rendered action in the cockpit."
  (interactive)
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (if-let ((position (delib-flow--next-action-position)))
      (goto-char position)
    (user-error "No later rendered action is available")))

(defun delib-flow-previous-action ()
  "Move point to the previous rendered action in the cockpit."
  (interactive)
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (if-let ((position (delib-flow--previous-action-position)))
      (goto-char position)
    (user-error "No earlier rendered action is available")))

(defun delib-flow-approve-current ()
  "Approve the current pending inspect or project-match result."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--call-current-review-command
   #'delib-flow--approve-stage-command
   "No inspect or project-match result is currently pending review"))

(defun delib-flow-retry-current ()
  "Retry the current pending inspect or project-match stage."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--call-current-review-command
   #'delib-flow--retry-stage-command
   "No inspect or project-match result is currently pending review"))

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

(defun delib-flow--read-debug-scenario-id ()
  "Prompt for a debug scenario id."
  (intern
   (completing-read
    "Debug scenario: "
    (mapcar #'symbol-name (delib-flow--debug-scenario-ids))
    nil t nil nil
    (symbol-name (car (delib-flow--debug-scenario-ids))))))

(defun delib-flow--read-debug-checkpoint ()
  "Prompt for a debug checkpoint id."
  (intern
   (completing-read
    "Checkpoint: "
    (mapcar (lambda (entry)
              (symbol-name (car entry)))
            delib-flow-debug-checkpoints)
    nil t nil nil
    (symbol-name 'source))))

(defun delib-flow--read-debug-walkthrough-target-id ()
  "Prompt for a debug walkthrough target id."
  (intern
   (completing-read
    "Walkthrough target: "
    (mapcar #'symbol-name (delib-flow--debug-walkthrough-target-ids))
    nil t nil nil
    (symbol-name (car (delib-flow--debug-walkthrough-target-ids))))))

(defun delib-flow-debug-open-latest-stage-inspection ()
  "Open a debug buffer for the latest stage package and outputs."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--open-debug-buffer
   (delib-flow--debug-latest-stage-inspection-text
    delib-flow--active-run)))

(defun delib-flow-debug-open-comparison ()
  "Open a debug buffer comparing the active run to replay and prior attempts."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (delib-flow--open-debug-buffer
   (delib-flow--debug-comparison-text delib-flow--active-run)))

(defun delib-flow-debug-open-walkthrough ()
  "Open the walkthrough guide for local verification."
  (interactive)
  (let ((buffer (get-buffer-create delib-flow-debug-walkthrough-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (delib-flow--debug-walkthrough-text))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun delib-flow--debug-helper-choice (run)
  "Prompt for a debug helper preset for RUN."
  (let* ((options (delib-flow--debug-helper-options run))
         (labels (mapcar #'cdr options))
         (choice (completing-read "Debug helper preset: " labels nil t)))
    (car (rassoc choice options))))

(defun delib-flow--debug-apply-helper-text (run block-id text decision)
  "Return RUN with BLOCK-ID set to TEXT and current DECISION updated."
  (let ((updated-run (delib-flow--debug-set-block-text run block-id text)))
    (plist-put updated-run :session
               (plist-put (delib-flow--run-session updated-run)
                          :current-decision
                          decision))))

(defun delib-flow--debug-apply-manual-project-helper (run preset-id)
  "Return RUN with manual-project helper PRESET-ID applied."
  (let ((candidate (car (delib-flow--debug-helper-candidates run))))
    (pcase preset-id
      ('choose-first-candidate
       (delib-flow--debug-apply-helper-text
        run
        'manual-project-selection
        (delib-flow--debug-manual-project-preset-text
         run
         (plist-get candidate :title)
         "Debug helper selected the first available candidate.")
        "Debug helper selected the first manual project candidate."))
      ('reject-all-candidates
       (delib-flow--debug-apply-helper-text
        run
        'manual-project-selection
        (delib-flow--debug-manual-project-preset-text
         run
         "REJECT"
         "Debug helper rejected all available candidates.")
        "Debug helper rejected all manual project candidates.")))))

(defconst delib-flow--debug-cloud-failure-resolution-map
  '((retry-cloud . "RETRY-CLOUD")
    (use-local . "USE-LOCAL")
    (skip-cloud . "SKIP-CLOUD")
    (abort-run . "ABORT"))
  "Resolution values for cloud-failure debug helper presets.")

(defconst delib-flow--debug-cloud-failure-note-map
  '((retry-cloud . "Debug helper selected a direct cloud retry.")
    (use-local . "Debug helper selected local continuation.")
    (skip-cloud . "Debug helper selected an explicit cloud skip.")
    (abort-run . "Debug helper selected abort for this failure branch."))
  "Notes values for cloud-failure debug helper presets.")

(defconst delib-flow--debug-cloud-failure-decision-map
  '((retry-cloud . "Debug helper prepared a retry-cloud resolution.")
    (use-local . "Debug helper prepared a use-local resolution.")
    (skip-cloud . "Debug helper prepared a skip-cloud resolution.")
    (abort-run . "Debug helper prepared an abort resolution."))
  "Decision text for cloud-failure debug helper presets.")

(defun delib-flow--debug-cloud-failure-resolution (preset-id)
  "Return cloud-failure resolution string for PRESET-ID."
  (alist-get preset-id delib-flow--debug-cloud-failure-resolution-map))

(defun delib-flow--debug-cloud-failure-note (preset-id)
  "Return cloud-failure notes string for PRESET-ID."
  (alist-get preset-id delib-flow--debug-cloud-failure-note-map))

(defun delib-flow--debug-cloud-failure-decision (preset-id)
  "Return cloud-failure decision string for PRESET-ID."
  (alist-get preset-id delib-flow--debug-cloud-failure-decision-map))

(defun delib-flow--debug-apply-cloud-failure-helper (run preset-id)
  "Return RUN with cloud-failure helper PRESET-ID applied."
  (delib-flow--debug-apply-helper-text
   run
   'cloud-failure-review
   (delib-flow--debug-cloud-failure-preset-text
    run
    (delib-flow--debug-cloud-failure-resolution preset-id)
    (delib-flow--debug-cloud-failure-note preset-id))
   (delib-flow--debug-cloud-failure-decision preset-id)))

(defun delib-flow--debug-apply-filing-selection-helper (run preset-id)
  "Return RUN with filing-selection helper PRESET-ID applied."
  (pcase preset-id
    ('select-first-ready
     (let ((index (delib-flow--debug-helper-first-ready-index run)))
       (delib-flow--debug-apply-helper-text
        run
        'filing-selection-review
        (delib-flow--debug-filing-selection-preset-text
         run
         (number-to-string index)
         "Debug helper selected the first ready artifact.")
        "Debug helper selected the first ready filing artifact.")))
    ('select-first-blocked
     (let ((index (delib-flow--debug-helper-first-blocked-index run)))
       (delib-flow--debug-apply-helper-text
        run
        'filing-selection-review
        (delib-flow--debug-filing-selection-preset-text
         run
         (number-to-string index)
         "Debug helper selected the first blocked artifact to exercise warning handling.")
        "Debug helper selected the first blocked filing artifact.")))))

(defun delib-flow--debug-smart-conflict-title (preset-id)
  "Return debug title text for conflict PRESET-ID."
  (alist-get preset-id
             '((smart-rename-note . "Debug renamed reference note")
               (smart-retitle-project . "Debug retitled project"))
             nil nil #'eq))

(defun delib-flow--debug-smart-conflict-text (preset-id)
  "Return debug text value for conflict PRESET-ID."
  (alist-get preset-id
             '((smart-reword-item . "Debug helper reworded the approved artifact for retry."))
             nil nil #'eq))

(defconst delib-flow--debug-filing-conflict-resolution-map
  '((retry-conflict . "RETRY")
    (reject-conflict . "REJECT")
    (smart-rename-note . "RENAME-NOTE")
    (smart-retitle-project . "RETITLE-PROJECT")
    (smart-reword-item . "REWORD-ITEM"))
  "Resolution values for filing-conflict debug helper presets.")

(defconst delib-flow--debug-filing-conflict-note-map
  '((retry-conflict . "Debug helper kept the approved artifact in place for retry.")
    (reject-conflict . "Debug helper rejected the approved artifact from this run.")
    (smart-rename-note . "Debug helper prepared a stage-appropriate conflict fix.")
    (smart-retitle-project . "Debug helper prepared a stage-appropriate conflict fix.")
    (smart-reword-item . "Debug helper prepared a stage-appropriate conflict fix."))
  "Notes values for filing-conflict debug helper presets.")

(defconst delib-flow--debug-filing-conflict-decision-map
  '((retry-conflict . "Debug helper prepared a retry conflict resolution.")
    (reject-conflict . "Debug helper prepared a reject conflict resolution.")
    (smart-rename-note . "Debug helper prepared a stage-appropriate conflict fix.")
    (smart-retitle-project . "Debug helper prepared a stage-appropriate conflict fix.")
    (smart-reword-item . "Debug helper prepared a stage-appropriate conflict fix."))
  "Decision text for filing-conflict debug helper presets.")

(defun delib-flow--debug-filing-conflict-resolution (preset-id)
  "Return filing-conflict resolution string for PRESET-ID."
  (alist-get preset-id delib-flow--debug-filing-conflict-resolution-map))

(defun delib-flow--debug-filing-conflict-note (preset-id)
  "Return filing-conflict notes string for PRESET-ID."
  (alist-get preset-id delib-flow--debug-filing-conflict-note-map))

(defun delib-flow--debug-filing-conflict-decision (preset-id)
  "Return filing-conflict decision string for PRESET-ID."
  (alist-get preset-id delib-flow--debug-filing-conflict-decision-map))

(defun delib-flow--debug-apply-filing-conflict-helper (run preset-id)
  "Return RUN with filing-conflict helper PRESET-ID applied."
  (delib-flow--debug-apply-helper-text
   run
   'filing-conflict-resolution
   (delib-flow--debug-filing-conflict-preset-text
    run
    (delib-flow--debug-filing-conflict-resolution preset-id)
    (delib-flow--debug-filing-conflict-note preset-id)
    (delib-flow--debug-smart-conflict-title preset-id)
    (delib-flow--debug-smart-conflict-text preset-id))
   (delib-flow--debug-filing-conflict-decision preset-id)))

(defun delib-flow--debug-apply-helper-preset (run preset-id)
  "Return RUN with helper PRESET-ID applied."
  (if-let ((handler
            (alist-get (delib-flow--debug-helper-context run)
                       '((manual-project . delib-flow--debug-apply-manual-project-helper)
                         (cloud-failure . delib-flow--debug-apply-cloud-failure-helper)
                         (filing-selection . delib-flow--debug-apply-filing-selection-helper)
                         (filing-conflict . delib-flow--debug-apply-filing-conflict-helper)))))
      (funcall handler run preset-id)
    run))

(defun delib-flow--debug-clear-active-run ()
  "Clear the current active run and its control buffer."
  (let ((buffer (delib-flow--control-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (delib-flow--teardown-active-run))

(defun delib-flow--debug-load-walkthrough-target (target-id checkpoint decision)
  "Load walkthrough TARGET-ID at CHECKPOINT with DECISION text."
  (let* ((target (delib-flow--debug-walkthrough-target target-id))
         (scenario-id (plist-get target :scenario)))
    (when delib-flow--active-run
      (delib-flow--debug-clear-active-run))
    (delib-flow-debug-start-scenario scenario-id checkpoint)
    (setq delib-flow--active-run
          (plist-put
           delib-flow--active-run
           :session
           (plist-put
            (plist-put
             (delib-flow--run-session delib-flow--active-run)
             :debug-walkthrough-target-id target-id)
            :current-decision decision)))
    (delib-flow--rerender-active-run-buffer)
    (delib-flow-debug-open-walkthrough)))

(defun delib-flow-debug-start-walkthrough (target-id)
  "Start the walkthrough TARGET-ID."
  (interactive
   (list (delib-flow--read-debug-walkthrough-target-id)))
  (let* ((target (delib-flow--debug-walkthrough-target target-id))
         (checkpoint (plist-get target :checkpoint))
         (objective (plist-get target :objective)))
    (delib-flow--debug-load-walkthrough-target
     target-id
     checkpoint
     (format "Walkthrough target %s loaded. %s"
             target-id
             objective))))

(defun delib-flow-debug-walkthrough-next-step ()
  "Advance the active walkthrough to its next checkpoint."
  (interactive)
  (let* ((target-id (delib-flow--ensure-active-debug-walkthrough))
         (checkpoint (delib-flow--debug-active-walkthrough-checkpoint))
         (next (delib-flow--debug-walkthrough-next-step-id target-id checkpoint)))
    (unless next
      (user-error "Walkthrough target %s is already at its final checkpoint"
                  target-id))
    (delib-flow--debug-load-walkthrough-target
     target-id
     next
     (format "Walkthrough target %s advanced to %s."
             target-id
             next))))

(defun delib-flow-debug-walkthrough-restart-target ()
  "Restart the active walkthrough target from its baseline checkpoint."
  (interactive)
  (let ((target-id (delib-flow--ensure-active-debug-walkthrough)))
    (delib-flow--debug-load-walkthrough-target
     target-id
     (delib-flow--debug-walkthrough-first-step target-id)
     (format "Walkthrough target %s restarted from its baseline checkpoint."
             target-id))))

(defun delib-flow-debug-apply-helper (preset-id)
  "Apply debug helper PRESET-ID for the active manual review context."
  (interactive
   (progn
     (unless delib-flow--active-run
       (user-error "No active delib-flow run"))
     (unless (delib-flow--debug-helper-context delib-flow--active-run)
       (user-error "No active debug helper context is available"))
     (list (delib-flow--debug-helper-choice delib-flow--active-run))))
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--debug-apply-helper-preset
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          preset-id)))
  (delib-flow--rerender-active-run-buffer)
  (when (buffer-live-p (get-buffer delib-flow-debug-walkthrough-buffer-name))
    (delib-flow-debug-open-walkthrough)))

(defun delib-flow-debug-start-scenario (scenario-id checkpoint)
  "Start a debug scenario SCENARIO-ID and replay to CHECKPOINT."
  (interactive
   (list (delib-flow--read-debug-scenario-id)
         (delib-flow--read-debug-checkpoint)))
  (delib-flow--cleanup-stale-run)
  (when (delib-flow--active-run-conflict-p)
    (pop-to-buffer (delib-flow--control-buffer))
    (user-error "A delib-flow run is already active"))
  (unless (delib-flow--debug-checkpoint-supported-p scenario-id checkpoint)
    (user-error "Debug scenario %s does not support checkpoint %s"
                scenario-id checkpoint))
  (let* ((fixture (delib-flow--activate-debug-fixture scenario-id))
         (source (delib-flow--debug-source-snapshot scenario-id fixture))
         (run (delib-flow--set-debug-fixture
               (delib-flow--initialize-run source)
               fixture))
         (replayed-run (delib-flow--debug-replay-to-checkpoint run checkpoint)))
    (setq delib-flow--active-run
          (plist-put
           replayed-run
           :session
           (plist-put
            (plist-put
             (plist-put
              (delib-flow--run-session replayed-run)
              :debug-scenario-id scenario-id)
             :debug-checkpoint checkpoint)
            :current-decision
            (format "Debug scenario %s loaded at checkpoint %s."
                    scenario-id
                    checkpoint))))
    (pop-to-buffer
     (delib-flow--render-active-run-buffer delib-flow--active-run "Now"))))

(defun delib-flow-control-help ()
  "Show the DeliberateFlow control-buffer keybindings."
  (interactive)
  (let* ((buffer (delib-flow--control-buffer))
         (section (and (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (delib-flow--current-top-level-section-at-point))))
         (run delib-flow--active-run))
    (with-help-window (help-buffer)
      (princ "DeliberateFlow control hints\n\n")
      (princ "Use n/p to move between cockpit sections.\n")
      (princ "Use TAB/S-TAB to move between rendered actions.\n")
      (princ "Use . to open the local context menu.\n")
      (princ "Use L to jump back to the active loop.\n")
      (princ "Use K to jump to the latest preview or result.\n")
      (princ "Use U to jump to the latest stage history details.\n")
      (princ "Use z to toggle narrow-screen focus mode.\n")
      (princ "Action keys are shown inline beside each rendered option.\n\n")
      (princ (format "Current section: %s\n"
                     (or section "unknown")))
      (princ (format "Recommended next pass: %s\n"
                     (if run
                         (delib-flow--recommended-action-text run)
                       "none")))
      (princ (format "Active loop: %s\n"
                     (if run
                         (delib-flow--active-loop-location-text run)
                       "unknown")))
      (princ (format "Latest change: %s\n"
                     (if run
                         (delib-flow--latest-meaningful-change-text run)
                       "unknown")))
      (princ (format "Latest preview: %s\n"
                     (if run
                         (delib-flow--latest-preview-location-text run)
                       "unknown")))
      (princ (format "Current decision: %s\n"
                     (if run
                         (or (plist-get (delib-flow--run-session run) :current-decision)
                             "none")
                       "unknown")))
      (princ (format "Last consequence: %s\n"
                     (if run
                         (delib-flow--latest-consequence-text run)
                       "unknown")))
      (princ (format "Blocked by: %s\n\n"
                     (if run
                         (delib-flow--current-blockage-text run)
                       "unknown")))
      (princ "Local actions here:\n")
      (princ (if run
                 (delib-flow--local-actions-summary-text run section)
               "- No active run is available."))
      (princ "\n\nGlobal controls:\n")
      (dolist (entry delib-flow--control-key-help-alist)
        (princ (format "%-12s %s\n" (car entry) (cdr entry)))))))

(defun delib-flow-control-menu ()
  "Open a local context menu for the current cockpit section."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (unless (derived-mode-p 'delib-flow-control-mode)
    (user-error "This command only works in the delib-flow control buffer"))
  (let* ((section (or (delib-flow--current-top-level-section-at-point)
                      "Now"))
         (entries (delib-flow--context-menu-entries delib-flow--active-run section)))
    (unless entries
      (user-error "No local actions are available in %s" section))
    (let* ((labels (mapcar #'delib-flow--context-menu-choice-label entries))
           (choice (completing-read
                    (format "Delib-Flow %s menu: " section)
                    labels
                    nil t))
           (index (cl-position choice labels :test #'equal))
           (entry (nth index entries)))
      (unless entry
        (user-error "No local action was selected"))
      (call-interactively (plist-get entry :command)))))

(defun delib-flow-refresh-buffer ()
  "Refresh the control buffer for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let* ((buffer (delib-flow--control-buffer))
         (preserved-heading
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (delib-flow--current-heading-at-point)))))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--set-managed-region-conflicts
            (delib-flow--sync-editable-blocks delib-flow--active-run buffer)
            (delib-flow--managed-region-conflicts delib-flow--active-run
                                                  buffer))))
    (pop-to-buffer
    (delib-flow--render-active-run-buffer
     delib-flow--active-run
      (or preserved-heading
          (delib-flow--preferred-anchor-section delib-flow--active-run))))))

(defun delib-flow-toggle-focus-mode ()
  "Toggle narrow-screen focus mode for the active control buffer."
  (interactive)
  (let ((buffer (delib-flow--control-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "This command only works in the delib-flow control buffer"))
    (with-current-buffer buffer
      (unless (derived-mode-p 'delib-flow-control-mode)
        (user-error "This command only works in the delib-flow control buffer"))
      (setq-local delib-flow-control-focus-mode
                  (not delib-flow-control-focus-mode))
      (delib-flow--apply-visibility-policy delib-flow--active-run)
      (force-mode-line-update)
      (message "Delib-Flow focus mode %s"
               (if delib-flow-control-focus-mode "enabled" "disabled")))))

(defun delib-flow-abort-run ()
  "Abort the active delib-flow run and close the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--refresh-run-audit
         (delib-flow--mark-run-aborted delib-flow--active-run)
         'abort-run))
  (let ((buffer (delib-flow--control-buffer)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer)))
  (delib-flow--teardown-active-run))

(defun delib-flow-start ()
  "Start a delib-flow run from the Org heading at point."
  (interactive)
  (delib-flow--cleanup-stale-run)
  (when (delib-flow--active-run-conflict-p)
    (pop-to-buffer (delib-flow--control-buffer))
    (user-error "A delib-flow run is already active"))
  (unless (derived-mode-p 'org-mode)
    (user-error "delib-flow requires Org mode"))
  (unless (delib-flow--org-heading-at-point-p)
    (user-error "Point must be on an Org heading to start delib-flow"))
  (delib-flow--start-run-from-source
   (delib-flow--snapshot-heading)))

(defun delib-flow-start-from-inbox ()
  "Start a delib-flow run from a top-level heading in `delib-flow-inbox-file'."
  (interactive)
  (delib-flow--cleanup-stale-run)
  (when (delib-flow--active-run-conflict-p)
    (pop-to-buffer (delib-flow--control-buffer))
    (user-error "A delib-flow run is already active"))
  (let* ((file (delib-flow--configured-inbox-file)))
    (unless file
      (user-error "`delib-flow-inbox-file' is not configured"))
    (unless (file-readable-p file)
      (user-error "Inbox file is not readable: %s" file))
    (let* ((snapshots (delib-flow--inbox-heading-snapshots
                       file
                       delib-flow-inbox-outline-path))
           (labels (delib-flow--inbox-selection-labels snapshots)))
      (unless labels
        (user-error "Inbox source has no selectable headings: %s" file))
      (let* ((choice (completing-read
                      "Delib-Flow inbox entry: "
                      (mapcar #'car labels)
                      nil t))
             (source (or (cdr (assoc choice labels))
                         (cdar labels))))
        (unless source
          (user-error "No inbox entry was selected"))
        (delib-flow--start-run-from-source source)))))

(provide 'delib-flow)
;;; delib-flow.el ends here
