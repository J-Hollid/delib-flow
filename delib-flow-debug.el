;;; delib-flow-debug.el --- Debug subsystem for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Debug scenarios, walkthroughs, and inspection commands for delib-flow.

;;; Code:

(require 'org)

(eval-and-compile
  (unless (fboundp 'delib-flow--define-function)
    (defmacro delib-flow--define-function (name args &rest body)
      "Define NAME with ARGS and BODY through a shared wrapper macro."
      (declare (indent defun))
      `(defalias ',name
         (lambda ,args
           ,@body)))))

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
  "Built-in debug scenarios for local verification.")

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
     :label "Cloud failure recovery"
     :scenario alpha-followup
     :checkpoint cloud-failure-ready
     :steps (project-reviewed cloud-ready cloud-failure-ready)
     :checks ((project-reviewed
               :expect "Project review should already be accepted before the cloud route is prepared.")
              (cloud-ready
               :expect "The cloud package should be reviewed and ready to send.")
              (cloud-failure-ready
               :expect "The failure should be recorded against Extract Actions with a resolution block and direct retry or fallback actions."))
     :objective "Verify routed cloud failure handling and recovery choices.")
    (filing-conflict
     :label "Filing conflict recovery"
     :scenario filing-conflict
     :checkpoint filing-conflict-ready
     :steps (project-reviewed context-ready artifact-ready filing-ready
                    filing-conflict-ready)
     :checks ((project-reviewed
               :expect "Project review should already be accepted before artifact drafting begins.")
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
  (let ((updated-run
         (delib-flow--debug-set-block-text
          run 'cloud-routing-review
          (format "Target stage: %s\nNotes:\nDebug checkpoint reroutes cloud execution here.\n"
                  stage-id))))
    (plist-put
     updated-run
     :routing
     (plist-put (delib-flow--run-routing updated-run)
                :cloud-target-stage stage-id))))

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

(delib-flow--define-function delib-flow--debug-override-draft-item
    (package spec)
  "Return debug draft item for PACKAGE built from SPEC."
  (let* ((kind
          (or (plist-get spec :kind)
              'next-action))
         (source
          (or (plist-get spec :source)
              'debug-midpoint-fixture))
         (item
          (pcase kind
            ('reference-note
             (delib-flow--make-draft-reference-note
              (or (plist-get spec :text) "")
              source
              (or (plist-get spec :note-type)
                  'general-pkm)))
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
        (delib-flow--annotate-draft-reference-notes
         (list item) package))
       ('waiting-for
        (delib-flow--annotate-draft-waiting-fors
         (list item)))
       (_
        (delib-flow--annotate-draft-actions
         (list item)))))))

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
  (let ((delib-flow-default-cloud-model
         (or delib-flow-default-cloud-model "debug:model")))
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
       (delib-flow--run-stage-locally sanitized 'approve-cloud-send)))))

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

(delib-flow--define-function
    delib-flow--debug-draft-selected-filing-item (run)
  "Return RUN after drafting the currently selected filing item when applicable."
  (if-let ((item (delib-flow--selected-filing-item run)))
      (pcase (plist-get item :kind)
        ('next-action
         (delib-flow--seed-actions
          (delib-flow--run-stage-locally run 'draft-selected-action)))
        ('waiting-for
         (delib-flow--seed-actions
          (delib-flow--run-stage-locally run
                                         'draft-selected-waiting-for)))
        ('reference-note
         (delib-flow--seed-actions
          (delib-flow--run-stage-locally run
                                         'draft-selected-reference-note)))
        ('project
         (delib-flow--seed-actions
          (delib-flow--run-stage-locally run 'draft-selected-project)))
        (_ run))
    run))

(defun delib-flow--debug-run-filing-conflict-ready (run)
  "Return RUN replayed to a filing-conflict checkpoint."
  (let* ((filing-ready (delib-flow--debug-run-filing-ready run))
         (selected-for-drafting
          (delib-flow--debug-set-filing-selection filing-ready "1"))
         (drafted
          (delib-flow--debug-draft-selected-filing-item selected-for-drafting))
         (selected
          (delib-flow--seed-actions
           (delib-flow--run-stage-locally
            drafted
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
                          draft-selected-reference-note-body
                          draft-selected-reference-note-source-highlights
                          draft-selected-reference-note-related-material
                          draft-selected-reference-note-reuse-angle
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

(provide 'delib-flow-debug)

;;; delib-flow-debug.el ends here
