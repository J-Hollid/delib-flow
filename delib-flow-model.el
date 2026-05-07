;;; delib-flow-model.el --- Model scaffolding for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Run-state construction and pure-ish model helpers for delib-flow.

;;; Code:

(require 'delib-flow-config)
(require 'seq)

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

(defconst delib-flow--artifact-family-keys
  '(actions waiting-fors reference-notes project-proposals)
  "Artifact-family keys used in run state.")

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
        :operator-intent nil
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
        :consequence-preview-kind nil
        :filing-workspace-open nil
        :filing-workspace-suppressed nil
        :filing-workspace-anchor nil
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

(defun delib-flow--consequence-preview-kind (run)
  "Return the active consequence preview kind from RUN."
  (plist-get (delib-flow--run-ui run) :consequence-preview-kind))

(defun delib-flow--filing-workspace-open-p (run)
  "Return non-nil when the focused filing workspace is open for RUN."
  (plist-get (delib-flow--run-ui run) :filing-workspace-open))

(defun delib-flow--filing-workspace-suppressed-p (run)
  "Return non-nil when auto-entry to the filing workspace is suppressed for RUN."
  (plist-get (delib-flow--run-ui run) :filing-workspace-suppressed))

(defun delib-flow--filing-workspace-anchor (run)
  "Return the preserved filing-workspace anchor heading for RUN."
  (plist-get (delib-flow--run-ui run) :filing-workspace-anchor))

(defun delib-flow--set-consequence-preview-kind (run kind)
  "Return RUN with consequence preview KIND stored in UI state."
  (plist-put run :ui
             (plist-put (delib-flow--run-ui run)
                        :consequence-preview-kind
                        kind)))

(defun delib-flow--set-filing-workspace-open (run open)
  "Return RUN with filing-workspace OPEN state stored in UI state."
  (plist-put run :ui
             (plist-put (delib-flow--run-ui run)
                        :filing-workspace-open
                        open)))

(defun delib-flow--set-filing-workspace-suppressed (run suppressed)
  "Return RUN with filing-workspace auto-entry SUPPRESSED state stored in UI."
  (plist-put run :ui
             (plist-put (delib-flow--run-ui run)
                        :filing-workspace-suppressed
                        suppressed)))

(defun delib-flow--set-filing-workspace-anchor (run anchor)
  "Return RUN with filing-workspace ANCHOR stored in UI state."
  (plist-put run :ui
             (plist-put (delib-flow--run-ui run)
                        :filing-workspace-anchor
                        anchor)))

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

(defun delib-flow--review-record-status (record)
  "Return candidate review-state for accepted-result RECORD."
  (or (plist-get record :candidate-review-state)
      'not-available))

(defun delib-flow--review-record-accepted-p (record)
  "Return non-nil when accepted-result RECORD has accepted output."
  (plist-get record :accepted-output))

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

(defun delib-flow--run-active-p (run)
  "Return non-nil when RUN is currently active."
  (eq (plist-get (delib-flow--run-session run) :status) 'active))

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

(defun delib-flow--manual-project-selection-text (run)
  "Return editable manual project-selection text from RUN."
  (delib-flow--editable-block-text
   (delib-flow--editable-block run 'manual-project-selection)))

(defun delib-flow--project-match-status (project-match)
  "Return display status symbol for PROJECT-MATCH."
  (or (plist-get project-match :match-status)
      'not-run))

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
                           (or (plist-get project-match :operator-selection)
                               ""))
             (string-equal (or notes "")
                           (or (plist-get project-match :operator-notes)
                               "")))))))

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
  (let ((ui (copy-tree (plist-get run :ui))))
    (plist-put ui :operator-intent
               (delib-flow--operator-intent-text run))))

(defun delib-flow--operator-intent-text (run)
  "Return trimmed operator intent text from RUN."
  (string-trim
   (or (plist-get (delib-flow--run-session run) :operator-intent)
       (delib-flow--editable-block-text
        (delib-flow--editable-block run 'context-main))
       "")))

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

(defun delib-flow--empty-artifact-family-state ()
  "Return empty state for one artifact family."
  (list :candidates nil
        :selected-candidate-id nil
        :selected-draft nil
        :draft-history nil
        :part-outcomes nil
        :selected-support-candidates nil
        :selected-support-context nil
        :available-support-candidates nil
        :available-support-context nil))

(defun delib-flow--initial-artifact-state ()
  "Return the initial artifact state for a new run."
  (let (state)
    (dolist (family delib-flow--artifact-family-keys state)
      (setq state
            (plist-put state family
                       (delib-flow--empty-artifact-family-state))))))

(defun delib-flow--run-artifacts (run)
  "Return the artifacts plist from RUN."
  (plist-get run :artifacts))

(defun delib-flow--artifact-family-state (run family)
  "Return artifact FAMILY state from RUN."
  (plist-get (delib-flow--run-artifacts run) family))

(defun delib-flow--set-artifact-family-state (run family family-state)
  "Return RUN with FAMILY-STATE stored for artifact FAMILY."
  (plist-put run :artifacts
             (plist-put (or (delib-flow--run-artifacts run)
                            (delib-flow--initial-artifact-state))
                        family
                        family-state)))

(defun delib-flow--set-artifact-family-candidates (run family candidates)
  "Return RUN with artifact FAMILY candidate state replaced by CANDIDATES.

This also clears any selected candidate or selected draft for FAMILY."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (setq state (plist-put state :candidates candidates))
    (setq state (plist-put state :selected-candidate-id nil))
    (setq state (plist-put state :selected-draft nil))
    (setq state (plist-put state :draft-history nil))
    (setq state (plist-put state :part-outcomes nil))
    (setq state (plist-put state :available-support-candidates nil))
    (setq state (plist-put state :available-support-context nil))
    (setq state (plist-put state :selected-support-candidates nil))
    (setq state (plist-put state :selected-support-context nil))
    (delib-flow--set-artifact-family-state run family state)))

(defun delib-flow--artifact-family-selected-candidate-id (run family)
  "Return selected candidate id for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :selected-candidate-id))

(defun delib-flow--artifact-family-selected-draft (run family)
  "Return selected draft for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :selected-draft))

(defun delib-flow--artifact-family-draft-history (run family)
  "Return saved draft history for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :draft-history))

(defun delib-flow--artifact-family-candidates (run family)
  "Return candidates for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :candidates))

(defun delib-flow--artifact-family-selected-candidate (run family)
  "Return selected candidate object for artifact FAMILY in RUN."
  (let ((selected-id
         (delib-flow--artifact-family-selected-candidate-id run family)))
    (seq-find
     (lambda (candidate)
       (equal (delib-flow--artifact-candidate-id candidate)
              selected-id))
     (delib-flow--artifact-family-candidates run family))))

(defconst delib-flow--artifact-family-label-alist
  '((actions . "action")
    (waiting-fors . "waiting-for")
    (reference-notes . "note")
    (project-proposals . "project"))
  "Operator-facing singular labels for artifact families.")

(defun delib-flow--artifact-family-label (family)
  "Return singular operator-facing label for artifact FAMILY."
  (or (alist-get family delib-flow--artifact-family-label-alist)
      "artifact"))

(defun delib-flow--workspace-selected-item (run family)
  "Return active selected item for artifact FAMILY in RUN."
  (or (delib-flow--artifact-family-selected-draft run family)
      (delib-flow--artifact-family-selected-candidate run family)
      (let ((active (delib-flow--active-filing-item run)))
        (when (eq (delib-flow--artifact-family-for-item-kind
                   (plist-get active :kind))
                  family)
          active))))

(defun delib-flow--set-artifact-family-selected-candidate-id (run family candidate-id)
  "Return RUN with artifact FAMILY selected candidate set to CANDIDATE-ID.

Changing the selected candidate clears any existing selected draft for FAMILY."
  (let* ((state (or (delib-flow--artifact-family-state run family)
                    (delib-flow--empty-artifact-family-state)))
         (existing-id (plist-get state :selected-candidate-id))
         (state (plist-put state :selected-candidate-id candidate-id)))
    (unless (equal existing-id candidate-id)
      (setq state (plist-put state :selected-draft nil))
      (setq state (plist-put state :draft-history nil))
      (setq state (plist-put state :part-outcomes nil))
      (setq state (plist-put state :available-support-candidates nil))
      (setq state (plist-put state :available-support-context nil))
      (setq state (plist-put state :selected-support-candidates nil))
      (setq state (plist-put state :selected-support-context nil)))
    (delib-flow--set-artifact-family-state run family state)))

(defun delib-flow--set-artifact-family-selected-draft (run family draft)
  "Return RUN with artifact FAMILY selected DRAFT stored."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put state :selected-draft draft))))

(defun delib-flow--set-artifact-family-draft-history (run family history)
  "Return RUN with artifact FAMILY draft HISTORY stored."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put state :draft-history history))))

(defun delib-flow--artifact-family-part-outcomes (run family)
  "Return selected draft part outcomes for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :part-outcomes))

(defun delib-flow--set-artifact-family-part-outcomes (run family outcomes)
  "Return RUN with artifact FAMILY draft part OUTCOMES stored."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put state :part-outcomes outcomes))))

(defun delib-flow--artifact-family-part-outcome (run family part-id)
  "Return selected draft PART-ID outcome for artifact FAMILY in RUN."
  (alist-get part-id (delib-flow--artifact-family-part-outcomes run family)))

(defun delib-flow--set-artifact-family-part-outcome (run family part-id outcome)
  "Return RUN with selected draft PART-ID OUTCOME stored for artifact FAMILY."
  (let* ((state (or (delib-flow--artifact-family-state run family)
                    (delib-flow--empty-artifact-family-state)))
         (outcomes (plist-get state :part-outcomes)))
    (setq outcomes (assq-delete-all part-id outcomes))
    (setq outcomes (cons (cons part-id outcome) outcomes))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put state :part-outcomes outcomes))))

(defun delib-flow--replace-artifact-family-selected-draft (run family draft)
  "Return RUN with artifact FAMILY selected DRAFT replaced and archived."
  (let* ((state
          (or (delib-flow--artifact-family-state run family)
              (delib-flow--empty-artifact-family-state)))
         (current (plist-get state :selected-draft))
         (current
          (and current
               (delib-flow--draft-with-evidence-snapshot run family current)))
         (history (plist-get state :draft-history))
         (draft
          (and draft
               (delib-flow--draft-with-evidence-snapshot run family draft)))
         (updated-history
          (if (and current
                   (not (equal (delib-flow--draft-revision-signature current)
                               (delib-flow--draft-revision-signature draft))))
              (cons (copy-tree current) history)
            history)))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put
      (plist-put state :selected-draft draft)
      :draft-history updated-history))))

(defun delib-flow--restore-latest-artifact-family-draft (run family)
  "Return RUN with latest archived draft restored for artifact FAMILY."
  (let* ((state
          (or (delib-flow--artifact-family-state run family)
              (delib-flow--empty-artifact-family-state)))
         (current (plist-get state :selected-draft))
         (current
          (and current
               (delib-flow--draft-with-evidence-snapshot run family current)))
         (history (plist-get state :draft-history))
         (previous (car history))
         (previous
          (and previous
               (delib-flow--draft-with-evidence-snapshot run family previous)))
         (remaining (cdr history)))
    (unless previous
      (user-error "No previous draft is available to restore"))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put
      (plist-put state :selected-draft (copy-tree previous))
      :draft-history
      (if current
          (append remaining (list (copy-tree current)))
        remaining)))))

(defun delib-flow--draft-history-with-current-appended (history current)
  "Return HISTORY with CURRENT draft archived at the end when present."
  (if current
      (append history (list (copy-tree current)))
    history))

(defun delib-flow--restore-artifact-family-draft-at-index (run family index)
  "Return RUN with draft-history entry INDEX restored for artifact FAMILY."
  (let* ((state (or (delib-flow--artifact-family-state run family)
                    (delib-flow--empty-artifact-family-state)))
         (current (plist-get state :selected-draft))
         (current (and current
                       (delib-flow--draft-with-evidence-snapshot run family
                                                                 current)))
         (history (plist-get state :draft-history))
         (chosen (nth index history))
         (chosen (and chosen
                      (delib-flow--draft-with-evidence-snapshot run family
                                                                chosen)))
         (remaining (append (seq-take history index)
                            (nthcdr (1+ index) history))))
    (unless chosen
      (user-error "No saved draft revision is available at that position"))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put
      (plist-put state :selected-draft (copy-tree chosen))
      :draft-history
      (delib-flow--draft-history-with-current-appended remaining current)))))

(defun delib-flow--artifact-family-selected-support-candidates (run family)
  "Return selected support candidates for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :selected-support-candidates))

(defun delib-flow--artifact-family-selected-support-context (run family)
  "Return selected support context for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :selected-support-context))

(defun delib-flow--set-artifact-family-selected-support (run family candidates context)
  "Return RUN with selected support CANDIDATES and CONTEXT for artifact FAMILY."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put
      (plist-put state :selected-support-candidates candidates)
      :selected-support-context context))))

(defun delib-flow--artifact-family-available-support-candidates (run family)
  "Return available support candidates for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :available-support-candidates))

(defun delib-flow--artifact-family-available-support-context (run family)
  "Return available support context for artifact FAMILY in RUN."
  (plist-get (delib-flow--artifact-family-state run family)
             :available-support-context))

(defun delib-flow--set-artifact-family-available-support (run family candidates context)
  "Return RUN with available support CANDIDATES and CONTEXT for artifact FAMILY."
  (let ((state (or (delib-flow--artifact-family-state run family)
                   (delib-flow--empty-artifact-family-state))))
    (delib-flow--set-artifact-family-state
     run family
     (plist-put
     (plist-put state :available-support-candidates candidates)
      :available-support-context context))))

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

(defun delib-flow--seed-filing-selection-block (run)
  "Return RUN with filing-selection block populated from current draft items."
  (let* ((items (plist-get (plist-get run :filing) :draft-items))
         (block (delib-flow--editable-block run 'filing-selection-review))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--filing-selection-template items))))
    (delib-flow--set-editable-block run 'filing-selection-review updated-block)))

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

(provide 'delib-flow-model)

;;; delib-flow-model.el ends here
