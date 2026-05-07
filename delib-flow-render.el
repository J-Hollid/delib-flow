;;; delib-flow-render.el --- Rendering helpers for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Render helpers extracted from the delib-flow compatibility facade.

;;; Code:

(require 'org)
(require 'pp)
(require 'seq)
(require 'subr-x)

(eval-and-compile
  (unless (fboundp 'delib-flow--define-function)
    (defmacro delib-flow--define-function (name args &rest body)
      "Define NAME with ARGS and BODY through a shared wrapper macro."
      (declare (indent defun))
      `(defalias ',name
         (lambda ,args
           ,@body)))))

(defconst delib-flow--section-renderer-alist
  '(("Now" . delib-flow--render-now-section)
    ("Next actions" . delib-flow--render-valid-next-actions-section)
    ("Current result" . delib-flow--render-current-result-section)
    ("Current context" . delib-flow--render-working-context-section)
    ("Filing preview" . delib-flow--render-filing-preview-section)
    ("Details" . delib-flow--render-details-section))
  "Renderer functions keyed by section title.")

(defun delib-flow--render-editable-block (run block-id)
  "Return Org text for editable block BLOCK-ID from RUN."
  (let* ((block (delib-flow--editable-block run block-id))
         (kind (plist-get block :kind))
         (text (delib-flow--editable-block-text block)))
    (format "#+begin_delib-edit %s\n%s\n#+end_delib-edit\n"
            kind text)))

(defun delib-flow--editable-block-editor-command (block-id)
  "Return dedicated editor command symbol for BLOCK-ID, or nil."
  (alist-get
   block-id
   '((context-main . delib-flow-action-edit-operator-intent)
     (operator-notes . delib-flow-action-edit-operator-notes)
     (manual-project-selection . delib-flow-action-edit-manual-project-selection)
     (filing-selection-review . delib-flow-action-edit-filing-selection-review)
     (filing-conflict-resolution . delib-flow-action-edit-filing-conflict-resolution))))

(defun delib-flow--editable-block-editor-help-line (block-id)
  "Return dedicated editor help line for BLOCK-ID, or nil."
  (when-let ((command (delib-flow--editable-block-editor-command block-id)))
    (format "- Typing is disabled here. Use the rendered `Edit` action or run `M-x %s`."
            (symbol-name command))))

(defun delib-flow--render-editable-block-with-editor-help (run block-id)
  "Return editable block BLOCK-ID plus dedicated editor help."
  (let ((help-line (delib-flow--editable-block-editor-help-line block-id)))
    (concat
     (if help-line
         (concat help-line "\n")
       "")
     (delib-flow--render-editable-block run block-id))))

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
             (draft-selected-waiting-for . "Draft only the currently selected waiting-for item.")
             (suggest-reference-notes . "Draft reference-note candidates from the accepted context.")
             (draft-selected-action . "Draft only the currently selected action item.")
             (draft-selected-reference-note . "Draft only the currently selected note item.")
             (draft-selected-reference-note-body . "Ask the drafting stage to regenerate only the current note body.")
             (draft-selected-reference-note-source-highlights . "Ask the drafting stage to regenerate only the current note source highlights.")
             (draft-selected-reference-note-related-material . "Ask the drafting stage to regenerate only the current note related material.")
             (draft-selected-reference-note-reuse-angle . "Ask the drafting stage to regenerate only the current note reuse angle.")
             (choose-support-for-selected-reference-note . "Attach only the support note you want shaping the current selected note draft.")
             (clear-selected-reference-note-support . "Detach attached support from the current selected note draft.")
             (edit-selected-reference-note-title . "Edit the saved note title for the current selected note.")
             (choose-reference-note-template . "Choose the template used when saving the current selected note.")
             (edit-selected-reference-note-target-path . "Edit the saved target path override for the current selected note.")
             (edit-selected-reference-note-draft-body . "Edit only the working-draft body for the current selected note.")
             (refresh-selected-reference-note-draft-body . "Regenerate only the working-draft body for the current selected note.")
             (edit-selected-reference-note-source-highlights . "Edit only the source highlights section of the current selected note draft.")
             (refresh-selected-reference-note-source-highlights . "Regenerate only the source highlights section of the current selected note draft.")
             (edit-selected-reference-note-related-material . "Edit only the related material section of the current selected note draft.")
             (refresh-selected-reference-note-related-material . "Regenerate only the related material section of the current selected note draft.")
             (edit-selected-reference-note-reuse-angle . "Edit only the reuse angle line of the current selected note draft.")
             (refresh-selected-reference-note-reuse-angle . "Regenerate only the reuse angle line of the current selected note draft.")
             (draft-selected-project . "Draft only the currently selected project item.")
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
     (when (and reason
                (not (equal reason description)))
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

(defun delib-flow--review-record-status-line (label record)
  "Return status line for accepted-result LABEL and RECORD."
  (format "%s: candidate=%s. accepted=%s."
          label
          (delib-flow--review-record-status record)
          (if (delib-flow--review-record-accepted-p record) "available" "not available")))

(defun delib-flow--accepted-project-text (run)
  "Return accepted project-decision display text from RUN."
  (if-let ((project (delib-flow--accepted-project-decision run)))
      (delib-flow--project-match-text project)
    "No accepted project decision is available yet."))

(defun delib-flow--available-project-workspace-actions (run)
  "Return available project-workspace actions for RUN."
  (seq-filter
   (lambda (action)
     (eq (plist-get action :status) 'available))
   (delib-flow--focused-project-workspace-actions run)))

(defun delib-flow--recommended-project-action (run)
  "Return the best currently available project-local action for RUN."
  (car (delib-flow--available-project-workspace-actions run)))

(defun delib-flow--recommended-project-alternative-actions (run)
  "Return nearby alternative project-local actions for RUN."
  (let ((recommended (delib-flow--recommended-project-action run)))
    (seq-take
     (seq-remove
      (lambda (action)
        (eq action recommended))
      (delib-flow--available-project-workspace-actions run))
     2)))

(defun delib-flow--recommended-action (run)
  "Return the best currently available operator action for RUN."
  (if (delib-flow--project-flow-active-p run)
      (delib-flow--recommended-project-action run)
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
          (car available-actions)))))

(defun delib-flow--render-audit-status-section (_run)
  "Return Org text for the Audit status section."
  (format "** Run audit state\n%s\n\n** Stage audit readiness\n%s\n\n** Audit navigation\n%s\n"
          (delib-flow--audit-run-state-text _run)
          (delib-flow--audit-stage-readiness-text _run)
          (delib-flow--audit-navigation-text _run)))

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
   "..."))

(defun delib-flow--recommended-action-text (run)
  "Return the current recommendation text for RUN."
  (if-let ((action (delib-flow--recommended-action run)))
      (format "%s%s"
              (plist-get action :label)
              (if-let ((reason (plist-get action :reason)))
                  (format " (%s)" reason)
                ""))
    "No workflow action is currently available beyond refresh or abort."))

(defun delib-flow--operator-intent-next-step-label (run)
  "Return next recommended rerun step after saving operator intent in RUN."
  (cond
   ((or (delib-flow--stage-executed-p run 'extract-actions)
        (delib-flow--stage-executed-p run 'extract-waiting-for))
    "Re-run Extract Actions")
   ((delib-flow--selected-project-drafted-p run)
    "Re-run Draft Selected Project")
   ((delib-flow--stage-executed-p run 'propose-new-project)
    "Re-run Propose New Project")
   ((delib-flow--stage-executed-p run 'match-project)
    "Re-run Match Project")
   ((delib-flow--stage-executed-p run 'inspect-source)
    "Re-run Inspect Source")
   (t
    "Run Inspect Source")))

(defun delib-flow--operator-intent-status-lines (run)
  "Return operator-intent summary lines for RUN."
  (let ((intent (delib-flow--operator-intent-text run)))
    (append
     (list
      (format "- Current value: %s"
              (if (delib-flow--non-empty-string-p intent)
                  intent
                "none"))
      "- Saved operator intent steers future stage runs only; existing accepted results stay unchanged until you rerun them.")
     (when (delib-flow--non-empty-string-p intent)
       (list
        (format "- Next after saving: %s"
                (delib-flow--operator-intent-next-step-label run)))))))

(defun delib-flow--recommended-alternative-actions (run)
  "Return nearby alternative actions for RUN after the main recommendation."
  (if (delib-flow--project-flow-active-p run)
      (delib-flow--recommended-project-alternative-actions run)
    (let ((recommended (delib-flow--recommended-action run)))
      (seq-take
       (seq-remove
        (lambda (action)
          (or (eq action recommended)
              (not (eq (plist-get action :status) 'available))
              (memq (plist-get action :id) '(refresh-buffer abort-run))))
        (delib-flow--sorted-actions run))
       2))))

(defun delib-flow--recommended-action-why-text (run)
  "Return the rationale for the current recommendation in RUN."
  (if (delib-flow--project-flow-active-p run)
      (cond
       ((delib-flow--project-extraction-soft-warning run)
        (plist-get (delib-flow--project-extraction-soft-warning run) :message))
       ((delib-flow--selected-project-drafted-p run)
        "The active project package is already drafted. Keep moving inside the local package loop until it is ready to file.")
       ((delib-flow--selected-project-candidate-for-drafting run)
        "One project package is selected. Draft it before drifting into broader recovery or queue work.")
       (t
        "A project package is active. Continue the local project loop instead of the broader cockpit flow."))
    (if-let ((action (delib-flow--recommended-action run)))
        (or (plist-get action :reason)
            "This is the highest-priority currently legal workflow pass.")
      "The run currently has no workflow pass available beyond refresh or abort.")))

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
  (if (delib-flow--project-flow-active-p run)
      (cond
       ((plist-get (plist-get run :filing) :selection-blocking-warnings)
        "Review the package blocking warnings in Filing preview, then adjust the included items or reject the blocked item.")
       ((delib-flow--selected-project-drafted-p run)
        (format "Stay in the active project workspace and continue with `%s`."
                (delib-flow--project-workflow-next-action-label run)))
       ((delib-flow--selected-project-candidate-for-drafting run)
        "Draft the selected project package before extracting work or filing anything.")
       (t
        "Return to the active project package and keep the local authoring loop moving."))
    (or (delib-flow--filing-unblock-guidance run)
        (if-let ((action (delib-flow--current-blocked-action run)))
            (or (plist-get action :reason)
                (format "Unblock %s before continuing."
                        (plist-get action :label)))
          "No unblock action is currently needed."))))

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

(defconst delib-flow--active-loop-heading-location-alist
  '(("Current result" . "Current result")
    ("Next actions" . "Next actions")
    ("Manual project selection" . "Current context > Manual project selection")
    ("Filing preview" . "Filing preview")
    ("Artifact selection" . "Filing preview > Artifact selection")
    ("Conflict resolution" . "Filing preview > Conflict resolution")
    ("Filing actions" . "Filing preview > Filing actions"))
  "Location labels for active-loop headings.")

(defun delib-flow--current-result-loop-active-p (run)
  "Return non-nil when RUN should foreground the current-result loop."
  (or (delib-flow--run-in-flight-p run)
      (delib-flow--inspect-review-pending-p run)
      (delib-flow--match-review-pending-p run)))

(defun delib-flow--filing-actions-loop-active-p (run)
  "Return non-nil when RUN should foreground filing actions."
  (and (delib-flow--filing-preview-visible-p run)
       (or (plist-get (plist-get run :filing) :approved-items)
           (delib-flow--preview-selected-filing-items run))))

(defconst delib-flow--active-loop-heading-specs
  '((delib-flow--current-result-loop-active-p . "Current result")
    ((lambda (run)
       (delib-flow--project-flow-active-p run))
     . "Filing preview")
    (delib-flow--filing-actions-loop-active-p . "Filing actions")
    (delib-flow--filing-selection-active-p . "Artifact selection")
    (delib-flow--filing-conflict-resolution-active-p . "Conflict resolution")
    (delib-flow--manual-project-selection-active-p . "Manual project selection")
    (delib-flow--filing-preview-visible-p . "Filing preview"))
  "Ordered selector specs for active-loop headings.")

(defun delib-flow--first-matching-heading (run specs default-heading)
  "Return first heading from SPECS whose predicate matches RUN.
DEFAULT-HEADING is used when no predicate matches."
  (or (seq-some (lambda (spec)
                  (when (funcall (car spec) run)
                    (cdr spec)))
                specs)
      default-heading))

(defun delib-flow--active-loop-heading (run)
  "Return the most relevant active-loop heading for RUN."
  (delib-flow--first-matching-heading
   run delib-flow--active-loop-heading-specs "Next actions"))

(defun delib-flow--active-loop-location-text (run)
  "Return compact active-loop location text for RUN."
  (or (cdr (assoc (delib-flow--active-loop-heading run)
                  delib-flow--active-loop-heading-location-alist))
      (delib-flow--active-loop-heading run)))

(defun delib-flow--stage-entry-review-suffix (entry)
  "Return rendered review-state suffix for ENTRY."
  (if-let ((review-state (plist-get entry :review-state)))
      (format ", %s" review-state)
    ""))

(defun delib-flow--stage-entry-status-summary-text (entry)
  "Return compact status summary text for ENTRY."
  (format "%s (%s%s)"
          (plist-get entry :label)
          (plist-get entry :status)
          (delib-flow--stage-entry-review-suffix entry)))

(defun delib-flow--latest-meaningful-change-text (run)
  "Return compact latest-change text for RUN."
  (if-let ((entry (delib-flow--current-result-entry run)))
      (delib-flow--stage-entry-status-summary-text entry)
    "No stage result is available yet."))

(defconst delib-flow--latest-consequence-stage-text
  '((select-approved-filing-actions . "The approved filing artifact and deterministic targets were updated.")
    (extract-actions . "Draft artifacts were refreshed in Filing preview.")
    (extract-waiting-for . "Draft artifacts were refreshed in Filing preview.")
    (suggest-reference-notes . "Draft artifacts were refreshed in Filing preview.")
    (draft-selected-reference-note-body . "The selected note draft body changed.")
    (draft-selected-reference-note-source-highlights . "The selected note source highlights changed.")
    (draft-selected-reference-note-related-material . "The selected note related material changed.")
    (draft-selected-reference-note-reuse-angle . "The selected note reuse angle changed.")
    (propose-new-project . "Draft artifacts were refreshed in Filing preview.")
    (match-project . "Project context changed for downstream drafting and filing.")
    (manual-project-match . "Project context changed for downstream drafting and filing.")
    (discover-reference-material . "Relevant context changed for downstream extraction and note suggestions.")
    (filter-reference-material . "Relevant context changed for downstream extraction and note suggestions.")
    (find-support-for-selected-action . "Focused support changed for the selected artifact.")
    (find-support-for-selected-waiting-for . "Focused support changed for the selected artifact.")
    (find-support-for-selected-reference-note . "Focused support changed for the selected artifact.")
    (find-support-for-selected-project . "Focused support changed for the selected artifact."))
  "Stage-specific latest-consequence summary text.")

(defun delib-flow--latest-consequence-default-text (run)
  "Return fallback latest-consequence text for RUN."
  (or (plist-get (delib-flow--run-session run) :current-decision)
      "The latest stage completed and the cockpit is ready for the next pass."))

(defun delib-flow--latest-consequence-failure-text (run)
  "Return failure consequence text for RUN."
  (or (plist-get (delib-flow--run-session run) :current-decision)
      "The latest stage failed and needs operator recovery."))

(defun delib-flow--latest-consequence-file-approved-outputs-text (run)
  "Return filing consequence text for RUN."
  (if (plist-get (plist-get run :filing) :target-locations)
      "Filed targets were staged into buffers and are still unsaved."
    "Filing completed without staged target buffers."))

(defun delib-flow--latest-consequence-stage-override-text (entry)
  "Return stage override consequence text for ENTRY."
  (cdr (assq (plist-get entry :stage-id)
             delib-flow--latest-consequence-stage-text)))

(defun delib-flow--latest-consequence-stage-entry-text (_run entry)
  "Return stage override consequence text for ENTRY."
  (delib-flow--latest-consequence-stage-override-text entry))

(defun delib-flow--latest-consequence-failure-entry-text (run entry)
  "Return failure consequence text for RUN and ENTRY."
  (when (eq (plist-get entry :status) 'failed)
    (delib-flow--latest-consequence-failure-text run)))

(defun delib-flow--latest-consequence-filed-entry-text (run entry)
  "Return filed consequence text for RUN and ENTRY."
  (when (eq (plist-get entry :stage-id) 'file-approved-outputs)
    (delib-flow--latest-consequence-file-approved-outputs-text run)))

(defun delib-flow--latest-consequence-review-entry-text (_run entry)
  "Return review consequence text for ENTRY."
  (when (eq (plist-get entry :review-state) 'pending-review)
    "The latest stage changed operator focus to review."))

(defconst delib-flow--latest-consequence-entry-text-functions
  '(delib-flow--latest-consequence-failure-entry-text
    delib-flow--latest-consequence-filed-entry-text
    delib-flow--latest-consequence-stage-entry-text
    delib-flow--latest-consequence-review-entry-text)
  "Ordered selectors for stage-entry consequence text.")

(defun delib-flow--latest-consequence-entry-text (run entry)
  "Return latest consequence text for RUN using ENTRY."
  (or (seq-some (lambda (fn)
                  (funcall fn run entry))
                delib-flow--latest-consequence-entry-text-functions)
      (delib-flow--latest-consequence-default-text run)))

(defun delib-flow--latest-consequence-text (run)
  "Return compact last-consequence text for RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (delib-flow--latest-consequence-entry-text run entry)
    "No workflow consequence exists yet; start with Inspect Source."))

(defun delib-flow--history-status-text (run)
  "Return compact latest-history status text for RUN."
  (if-let ((entry (delib-flow--latest-stage-entry run)))
      (format "%s (%s)"
              (plist-get entry :label)
              (plist-get entry :status))
    "No stage history yet."))

(defconst delib-flow--latest-preview-location-alist
  '(("Opened staged targets" . "Filing preview > Opened staged targets")
    ("Staged content preview" . "Filing preview > Staged content preview")
    ("Planned file targets" . "Filing preview > Planned file targets")
    ("Current result" . "Current result")
    ("Decision strip" . "Now > Decision strip"))
  "Location labels for latest preview headings.")

(defconst delib-flow--latest-preview-heading-specs
  '(((lambda (run)
       (plist-get (plist-get run :filing) :target-locations))
     . "Opened staged targets")
    (delib-flow--staged-content-preview-text-available-p
     . "Staged content preview")
    (delib-flow--planned-file-locations . "Planned file targets")
    (delib-flow--current-result-entry . "Current result"))
  "Ordered selector specs for latest preview headings.")

(defun delib-flow--latest-preview-heading (run)
  "Return the most relevant preview heading for RUN."
  (delib-flow--first-matching-heading
   run delib-flow--latest-preview-heading-specs "Decision strip"))

(defun delib-flow--latest-preview-location-text (run)
  "Return compact latest-preview location text for RUN."
  (or (cdr (assoc (delib-flow--latest-preview-heading run)
                  delib-flow--latest-preview-location-alist))
      (delib-flow--latest-preview-heading run)))

(defun delib-flow--current-decision-text (run)
  "Return current decision summary text for RUN."
  (or (plist-get (delib-flow--run-session run) :current-decision)
      "No current decision is recorded yet."))

(defconst delib-flow--summary-field-text-functions
  `((:latest-change . delib-flow--latest-meaningful-change-text)
    (:active-loop . delib-flow--active-loop-location-text)
    (:latest-preview . delib-flow--latest-preview-location-text)
    (:current-decision . delib-flow--current-decision-text)
    (:last-consequence . delib-flow--latest-consequence-text)
    (:history-status . delib-flow--history-status-text)
    (:run-status . ,(lambda (run)
                      (plist-get (delib-flow--run-session run) :status)))
    (:current-stage . delib-flow--decision-strip-current-stage-text)
    (:blocked . delib-flow--current-blockage-text)
    (:recommended-next-step . delib-flow--recommended-action-text)
    (:active-project . delib-flow--active-project-text)
    (:active-filing-item . delib-flow--active-filing-item-text)
    (:planned-target-preview . delib-flow--planned-target-summary-text))
  "Function table for shared Now summary fields.")

(defun delib-flow--summary-field-text (run field)
  "Return summary FIELD text for RUN."
  (when-let ((fn (alist-get field delib-flow--summary-field-text-functions)))
    (funcall fn run)))

(defun delib-flow--compact-summary-field-text (run field)
  "Return compact summary FIELD text for RUN."
  (delib-flow--compact-summary
   (delib-flow--summary-field-text run field)
   (+ 16 (delib-flow--mobile-summary-limit))))

(defun delib-flow--resume-guide-text (run)
  "Return compact resume/orientation text for RUN."
  (format "- Latest change: %s\n- Active loop: %s\n- Latest preview: %s\n- Jump back: `L` active loop, `K` latest preview"
          (delib-flow--compact-summary-field-text run :latest-change)
          (delib-flow--summary-field-text run :active-loop)
          (delib-flow--summary-field-text run :latest-preview)))

(defun delib-flow--recovery-snapshot-text (run)
  "Return compact recovery snapshot text for RUN."
  (format "- Current decision: %s\n- Last consequence: %s\n- History status: %s\n- Recovery path: `L` resume loop, `K` consequence preview, `U` stage history, `J` audit stage"
          (delib-flow--compact-summary-field-text run :current-decision)
          (delib-flow--compact-summary-field-text run :last-consequence)
          (delib-flow--summary-field-text run :history-status)))

(defun delib-flow--blocked-action-p (action)
  "Return non-nil when ACTION is blocked."
  (eq (plist-get action :status) 'blocked))

(defun delib-flow--first-blocked-action (run)
  "Return the first blocked action for RUN, if any."
  (seq-find #'delib-flow--blocked-action-p
            (delib-flow--sorted-actions run)))

(defun delib-flow--blocked-action-text (action)
  "Return compact blocked-state text for ACTION."
  (format "%s%s"
          (plist-get action :label)
          (if-let ((reason (plist-get action :reason)))
              (format " (%s)" reason)
            "")))

(defun delib-flow--current-blockage-text (run)
  "Return the most relevant blocked-state text for RUN."
  (if-let ((action (delib-flow--first-blocked-action run)))
      (delib-flow--blocked-action-text action)
    "None"))

(defun delib-flow--active-project-text (run)
  "Return active project summary text for RUN."
  (or (plist-get (delib-flow--project-package-root-for-filing run) :title)
      (delib-flow--effective-project-title run)
      (delib-flow--matched-project-title run)
      "No project is currently selected."))

(defun delib-flow--active-project-package-item (run)
  "Return the active selected item inside the current project package for RUN."
  (or (delib-flow--artifact-family-selected-draft run 'actions)
      (delib-flow--selected-action-candidate-for-drafting run)
      (delib-flow--artifact-family-selected-draft run 'waiting-fors)
      (delib-flow--selected-waiting-for-candidate-for-drafting run)
      (delib-flow--artifact-family-selected-draft run 'reference-notes)
      (delib-flow--selected-reference-note-candidate-for-drafting run)
      (delib-flow--project-package-root-for-filing run)))

(defun delib-flow--active-filing-item (run)
  "Return the currently active filing item for RUN, if any."
  (or (car (plist-get (plist-get run :filing) :approved-items))
      (car (delib-flow--preview-selected-filing-items run))
      (and (delib-flow--project-flow-active-p run)
           (delib-flow--active-project-package-item run))))

(defun delib-flow--active-filing-item-text (run)
  "Return active filing item summary text for RUN."
  (if (delib-flow--project-flow-active-p run)
      (if-let ((item (delib-flow--active-project-package-item run)))
          (pcase (plist-get item :kind)
            ('project
             (format "Project package shell `%s`"
                     (or (plist-get item :title)
                         "Untitled project")))
            ('reference-note
             (format "Package note %s"
                     (or (plist-get item :text) "untitled note")))
            (_
             (format "Package item: %s %s"
                     (delib-flow--draft-item-keyword item)
                     (plist-get item :text))))
        "No package item is currently selected inside the active project package.")
    (if-let ((item (delib-flow--active-filing-item run)))
        (format "%s %s"
                (delib-flow--draft-item-keyword item)
                (plist-get item :text))
      "No filing artifact is currently selected.")))

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
    (if (delib-flow--project-flow-active-p run)
        (if-let ((project (delib-flow--project-package-root-for-filing run)))
            (format "%s bucket in %s"
                    (delib-flow--project-state-bucket-title
                     (or (plist-get project :state) 'active))
                    (file-name-nondirectory delib-flow-my-projects-file))
          "The active project package has no target preview yet.")
      "No planned file targets are available yet.")))

(defun delib-flow--decision-strip-current-stage-text (run)
  "Return current stage summary text for RUN."
  (if-let ((entry (delib-flow--current-result-entry run)))
      (delib-flow--stage-entry-status-summary-text entry)
    "No stage has produced a result yet."))

(defun delib-flow--decision-strip-text (run)
  "Return compact decision-strip text for RUN."
  (format
   "- Run status: %s\n- Current stage: %s\n- Blocked: %s\n- Recommended next step: %s\n- Active project: %s\n- Active filing item: %s\n- Planned target preview: %s"
   (delib-flow--summary-field-text run :run-status)
   (delib-flow--summary-field-text run :current-stage)
   (delib-flow--compact-summary-field-text run :blocked)
   (delib-flow--compact-summary-field-text run :recommended-next-step)
   (delib-flow--compact-summary-field-text run :active-project)
   (delib-flow--compact-summary-field-text run :active-filing-item)
   (delib-flow--summary-field-text run :planned-target-preview)))

(defun delib-flow--quick-actions (run)
  "Return the highest-priority local work actions for RUN."
  (if (delib-flow--project-flow-active-p run)
      (seq-take (delib-flow--available-project-workspace-actions run) 3)
    (seq-take
     (seq-filter
      (lambda (action)
        (and (eq (plist-get action :status) 'available)
             (not (memq (plist-get action :id) '(refresh-buffer abort-run)))))
      (delib-flow--sorted-actions run))
     3)))

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

(defun delib-flow--audit-log-file-display ()
  "Return user-facing audit log file display text."
  (if (delib-flow--audit-log-configured-p)
      delib-flow-audit-log-file
    "unconfigured"))

(defun delib-flow--audit-run-state-text (run)
  "Return run audit status text for RUN."
  (let* ((audit (plist-get run :audit))
         (run-record (plist-get audit :run-record)))
    (format "- Audit log file: %s\n- Audit archive directory: %s\n- Save archived run: %s\n- Audit payload policy: %s\n- Audit redaction profile: %s\n- Run status: %s\n- Run ID: %s"
            (delib-flow--audit-log-file-display)
            (delib-flow--audit-archive-directory-display)
            (delib-flow--audit-archive-save-availability)
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
         (operator-intent (delib-flow--operator-intent-text run))
         (retained-context (plist-get working :retained-context))
         (cloud-returned (plist-get working :cloud-returned-context))
         (lines
          (list
           (format "- Accepted inspect result: %s"
                   (if inspect-output "available" "not available"))
           (format "- Accepted project decision: %s"
                   (delib-flow--accepted-project-text run))
           (format "- Operator intent: %s"
                   (if (delib-flow--non-empty-string-p operator-intent)
                       operator-intent
                     "none"))
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

(delib-flow--define-function
 delib-flow--current-result-loop-update-text (run entry)
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
     (format
      "- Local consequence: this source classification now controls the next project-matching pass.\n- Resume here: %s"
      (if (eq review-state 'pending-review)
          "review this result, then accept or retry"
        "run Match Project or inspect again if needed")))
    ((memq stage-id '(match-project manual-project-match))
     (format
      "- Local consequence: project context for downstream drafting has changed.\n- Resume here: %s"
      (if (eq review-state 'pending-review)
          "review this project decision, then accept, retry, or choose manually"
        (delib-flow--active-loop-location-text run))))
    ((memq stage-id
           '(discover-reference-material filter-reference-material))
     (format
      "- Local consequence: retained context changed for extraction and note suggestion.\n- Resume here: %s"
      (delib-flow--active-loop-location-text run)))
    ((memq stage-id
           '(find-support-for-selected-action
             find-support-for-selected-waiting-for
             find-support-for-selected-reference-note
             find-support-for-selected-project))
     (format
      "- Local consequence: focused support changed for the selected artifact.\n- Resume here: %s"
      (delib-flow--active-loop-location-text run)))
    ((memq stage-id
           '(extract-actions extract-waiting-for
                             suggest-reference-notes
                             propose-new-project))
     (format
      "- Local consequence: draft artifacts were refreshed in Filing preview.\n- Resume here: %s"
      (delib-flow--active-loop-location-text run)))
    ((eq stage-id 'file-approved-outputs)
     (format
      "- Local consequence: target buffers were staged but not saved.\n- Resume here: %s"
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

(defun delib-flow--draft-selected-reference-note-part-result-text (_run entry)
  "Return readable current-result text for drafted selected-note part ENTRY."
  (let* ((raw-output (plist-get entry :raw-output))
         (item (plist-get raw-output :drafted-item))
         (part-id (plist-get raw-output :part-id))
         (part-label (or (delib-flow--reference-note-part-stage-label part-id)
                         "note part"))
         (part-text (or (plist-get raw-output :part-text) "")))
    (concat
     "*** Summary\n"
     (string-join
      (list
       (format "- Selected note: %s"
               (or (delib-flow--reference-note-title item) "Untitled note"))
       (format "- Regenerated part: %s" part-label)
       (format "- Reason: %s"
               (or (plist-get raw-output :reason) "No reason recorded."))
       "- Result: only this selected note part was regenerated.")
      "\n")
     "\n\n*** Selected note part draft\n"
     "- Review this part before approval or filing.\n"
     "- Regenerating here replaces only this part on the selected note draft.\n\n"
     (delib-flow--fixed-width-preview-text part-text)
     "\n")))

(defun delib-flow--draft-selected-action-result-text (_run entry)
  "Return readable current-result text for drafted selected-action ENTRY."
  (let* ((raw-output (plist-get entry :raw-output))
         (candidate (plist-get raw-output :candidate))
         (item (plist-get raw-output :drafted-item)))
    (concat
     "*** Summary\n"
     (string-join
      (list
       (format "- Selected action: %s"
               (or (plist-get item :text) "Untitled action"))
       (format "- Original action: %s"
               (or (plist-get candidate :text) "Untitled action"))
       (format "- Reason: %s"
               (or (plist-get raw-output :reason) "No reason recorded."))
       "- Result: this is the current drafted wording for the selected action only.")
      "\n")
     "\n\n*** Selected action draft\n"
     "- Review this wording before approval or filing.\n"
     "- Regenerating replaces only this selected action draft.\n\n"
     (delib-flow--fixed-width-preview-text
      (or (plist-get item :text) "Untitled action"))
     "\n")))

(defun delib-flow--draft-selected-waiting-for-result-text (_run entry)
  "Return readable current-result text for drafted selected waiting-for ENTRY."
  (let* ((raw-output (plist-get entry :raw-output))
         (candidate (plist-get raw-output :candidate))
         (item (plist-get raw-output :drafted-item)))
    (concat
     "*** Summary\n"
     (string-join
      (list
       (format "- Selected waiting-for: %s"
               (or (plist-get item :text) "Untitled waiting-for"))
       (format "- Original waiting-for: %s"
               (or (plist-get candidate :text) "Untitled waiting-for"))
       (format "- Reason: %s"
               (or (plist-get raw-output :reason) "No reason recorded."))
       "- Result: this is the current drafted wording for the selected waiting-for only.")
      "\n")
     "\n\n*** Selected waiting-for draft\n"
     "- Review this dependency wording before approval or filing.\n"
     "- Regenerating replaces only this selected waiting-for draft.\n\n"
     (delib-flow--fixed-width-preview-text
      (or (plist-get item :text) "Untitled waiting-for"))
     "\n")))

(delib-flow--define-function
 delib-flow--draft-selected-project-result-text (_run entry)
 "Return readable current-result text for drafted selected-project ENTRY."
 (let* ((raw-output (plist-get entry :raw-output))
        (candidate (plist-get raw-output :candidate))
        (item (plist-get raw-output :drafted-item))
        (first-item (plist-get item :first-item)))
   (concat "*** Summary\n"
           (string-join
            (list
             (format "- Selected project: %s"
                     (or (plist-get item :title) "Untitled project"))
             (format "- Original project: %s"
                     (or (plist-get candidate :title)
                         "Untitled project"))
             (format "- State: %s"
                     (or (plist-get item :state) 'active))
             (format "- First item: %s"
                     (or (plist-get first-item :text) "none"))
             (format "- Tags: %s"
                     (if-let ((tags (plist-get item :tags)))
                         (string-join tags ", ")
                       "none"))
             (format "- Reason: %s"
                     (or (plist-get raw-output :reason)
                         "No reason recorded."))
             "- Result: this is the current drafted structure for the selected project only.")
            "\n")
           "\n\n*** Selected project draft\n"
           "- Review this project definition before approval or filing.\n"
           "- Regenerating replaces only this selected project draft.\n\n"
           (delib-flow--fixed-width-preview-text
            (string-join
             (delq nil
                   (list
                    (format "Project title: %s"
                            (or (plist-get item :title)
                                "Untitled project"))
                    (format "State: %s"
                            (or (plist-get item :state) 'active))
                    (format "First item: %s"
                            (or (plist-get first-item :text) "none"))
                    (format "Tags: %s"
                            (if-let ((tags (plist-get item :tags)))
                                (string-join tags ", ")
                              "none"))))
             "\n"))
           "\n")))

(defconst delib-flow--current-result-stage-renderers
  '((inspect-source . delib-flow--inspect-result-text)
    (match-project . delib-flow--match-result-text)
    (suggest-reference-notes . delib-flow--suggest-reference-notes-result-text)
    (draft-selected-action . delib-flow--draft-selected-action-result-text)
    (draft-selected-waiting-for . delib-flow--draft-selected-waiting-for-result-text)
    (draft-selected-reference-note . delib-flow--draft-selected-reference-note-result-text)
    (draft-selected-reference-note-body
     . delib-flow--draft-selected-reference-note-part-result-text)
    (draft-selected-reference-note-source-highlights
     . delib-flow--draft-selected-reference-note-part-result-text)
    (draft-selected-reference-note-related-material
     . delib-flow--draft-selected-reference-note-part-result-text)
    (draft-selected-reference-note-reuse-angle
     . delib-flow--draft-selected-reference-note-part-result-text)
    (draft-selected-project . delib-flow--draft-selected-project-result-text))
  "Stage-specific current-result renderers.")

(defun delib-flow--current-result-stage-renderer (entry)
  "Return renderer function for current-result ENTRY."
  (alist-get (plist-get entry :stage-id)
             delib-flow--current-result-stage-renderers))

(defun delib-flow--current-result-body-text (run entry)
  "Return current-result body text for RUN and completed ENTRY."
  (if-let ((renderer (delib-flow--current-result-stage-renderer entry)))
      (funcall renderer run entry)
    (delib-flow--default-result-text entry)))

(defun delib-flow--completed-current-result-text (run entry)
  "Return readable current-result text for completed ENTRY in RUN."
  (concat "*** Loop update\n"
          (delib-flow--current-result-loop-update-text run entry)
          "\n\n"
          (delib-flow--current-result-body-text run entry)))

(delib-flow--define-function delib-flow--current-result-text (run)
  "Return current-result text for RUN."
  (if (delib-flow--run-in-flight-p run)
      (delib-flow--in-flight-current-result-text
       run)
    (if-let ((entry
              (delib-flow--current-result-entry
               run)))
        (delib-flow--completed-current-result-text
         run entry)
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
   "*** Operator intent\n"
   "- This text strongly steers source interpretation and project drafting.\n"
   (mapconcat #'identity
              (delib-flow--operator-intent-status-lines run)
              "\n")
   "\n"
   (delib-flow--render-editable-block-with-editor-help run 'context-main)))

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
              (delib-flow--render-editable-block-with-editor-help run block-id))
    ""))

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

(delib-flow--define-function delib-flow--source-section-body
			     (content section-name)
			     "Return body text for drawer-like SECTION-NAME in CONTENT, or nil."
			     (when
				 (and (stringp content)
				      (stringp section-name))
			       (let*
				   ((lines (split-string content "\n"))
				    (start-marker
				     (format ":%s:" section-name))
				    (collecting nil) collected)
				 (dolist (line lines)
				   (cond
				    ((and (not collecting)
					  (string=
					   (string-trim-right line)
					   start-marker))
				     (setq collecting t))
				    ((and collecting
					  (string= (string-trim line)
						   ":END:"))
				     (setq collecting 'done))
				    ((eq collecting t)
				     (push line collected))))
				 (when collected
				   (let
				       ((value
					 (string-trim
					  (mapconcat #'identity
						     (nreverse
						      collected)
						     "\n"))))
				     (unless (string-empty-p value)
				       value))))))


(delib-flow--define-function delib-flow--email-source-shape-p (source)
			     "Return non-nil when SOURCE looks like an imported email capture."
			     (let*
				 ((content
				   (or (plist-get source :content) ""))
				  (file
				   (or (plist-get source :file) "")))
			       (or
				(delib-flow--non-empty-string-p
				 (delib-flow--source-org-property
				  content "FROM"))
				(delib-flow--non-empty-string-p
				 (delib-flow--source-org-property
				  content "EMAIL_FILE"))
				(delib-flow--non-empty-string-p
				 (delib-flow--source-section-body
				  content "RAW_EMAIL"))
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

(delib-flow--define-function delib-flow--email-plain-body-from-raw
			     (raw-email)
			     "Return a reduced plain-text body extracted from RAW-EMAIL."
			     (let*
				 ((text (or raw-email ""))
				  (plain
				   (cond
				    ((string-match
				      "Content-Type:[[:space:]]*text/plain[^\n]*\n\\(?:Content-Transfer-Encoding:[^\n]*\n\\)?\n\\([\000-\377[:nonascii:][:ascii:]]*?\\)\n--[-[:alnum:]]+"
				      text)
				     (match-string 1 text))
				    ((string-match
				      "Content-Type:[[:space:]]*text/plain[^\n]*\n\\([\000-\377[:nonascii:][:ascii:]]*\\)"
				      text)
				     (match-string 1 text))
				    (t text)))
				  (decoded
				   (delib-flow--decode-quoted-printable-text
				    plain))
				  (lines (split-string decoded "\n"))
				  kept)
			       (dolist (line lines)
				 (when
				     (delib-flow--email-useful-line-p
				      line)
				   (push (string-trim line) kept)))
			       (string-trim
				(mapconcat #'identity
					   (seq-take (nreverse kept)
						     24)
					   "\n"))))


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

(defun delib-flow--execute-discover-reference-material (package)
  "Return raw discovery output for PACKAGE."
  (delib-flow--discover-reference-material-result package))

(defun delib-flow--execute-filter-reference-material (package)
  "Return raw filter output for PACKAGE."
  (delib-flow--filter-reference-material-result package))

(defun delib-flow--draft-items (package)
  "Return draft filing items from PACKAGE."
  (plist-get (plist-get package :filing) :draft-items))

(delib-flow--define-function delib-flow--selected-filing-item
			     (package)
			     "Return the operator-selected filing item from PACKAGE."
			     (let
				 ((item
				   (or
				    (condition-case nil
					(delib-flow--filing-selection-choice
					 package)
				      (error nil))
				    (and
				     (delib-flow--project-flow-active-p
				      package)
				     (delib-flow--project-package-root-for-filing
				      package)))))
			       (pcase (plist-get item :kind)
				 ('project
				  (let
				      ((selected-id
					(delib-flow--artifact-family-selected-candidate-id
					 package 'project-proposals))
				       (draft
					(delib-flow--selected-project-draft-from-package
					 package)))
				    (if
					(and draft
					     (equal selected-id
						    (delib-flow--artifact-candidate-id
						     item)))
					draft
				      item)))
				 ('reference-note
				  (let
				      ((selected-id
					(delib-flow--artifact-family-selected-candidate-id
					 package 'reference-notes))
				       (draft
					(delib-flow--selected-reference-note-draft-from-package
					 package)))
				    (if
					(and draft
					     (equal selected-id
						    (delib-flow--artifact-candidate-id
						     item)))
					draft
				      item)))
				 ('next-action
				  (let
				      ((selected-id
					(delib-flow--artifact-family-selected-candidate-id
					 package 'actions))
				       (draft
					(delib-flow--selected-action-draft-from-package
					 package)))
				    (if
					(and draft
					     (equal selected-id
						    (delib-flow--artifact-candidate-id
						     item)))
					draft
				      item)))
				 ('waiting-for
				  (let
				      ((selected-id
					(delib-flow--artifact-family-selected-candidate-id
					 package 'waiting-fors))
				       (draft
					(delib-flow--selected-waiting-for-draft-from-package
					 package)))
				    (if
					(and draft
					     (equal selected-id
						    (delib-flow--artifact-candidate-id
						     item)))
					draft
				      item)))
				 (_ item))))


(delib-flow--define-function
 delib-flow--effective-approved-filing-item (package item)
 "Return ITEM resolved to the best approved filing artifact for PACKAGE."
 (pcase (plist-get item :kind)
   ('project
    (or (delib-flow--project-package-root-for-filing package)
        item))
   ('reference-note
    (let*
	((selected-id
	  (delib-flow--artifact-family-selected-candidate-id package
							     'reference-notes))
	 (draft
	  (delib-flow--selected-reference-note-draft-from-package
	   package)))
      (if
	  (and draft
	       (equal
		(or (delib-flow--artifact-candidate-id item)
		    selected-id)
		(or selected-id
		    (delib-flow--artifact-candidate-id draft))))
	  draft
	item)))
   ('next-action
    (let*
	((selected-id
	  (delib-flow--artifact-family-selected-candidate-id package
							     'actions))
	 (draft
	  (delib-flow--selected-action-draft-from-package package)))
      (if
	  (and draft
	       (equal
		(or (delib-flow--artifact-candidate-id item)
		    selected-id)
		(or selected-id
		    (delib-flow--artifact-candidate-id draft))))
	  draft
	item)))
   ('waiting-for
    (let*
	((selected-id
	  (delib-flow--artifact-family-selected-candidate-id package
							     'waiting-fors))
	 (draft
	  (delib-flow--selected-waiting-for-draft-from-package package)))
      (if
	  (and draft
	       (equal
		(or (delib-flow--artifact-candidate-id item)
		    selected-id)
		(or selected-id
		    (delib-flow--artifact-candidate-id draft))))
	  draft
	item)))
   (_ item)))


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

(defun delib-flow--render-focused-filing-workspace-buffer (run)
  "Render the focused filing workspace buffer from RUN."
  (let ((buffer (get-buffer-create delib-flow-filing-workspace-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (delib-flow-filing-workspace-mode)
        (insert (format "* DeliberateFlow Filing -- %s\n"
                        (or (plist-get (delib-flow--run-source run) :title)
                            "Untitled source")))
        (insert
         (if (delib-flow--project-focused-filing-workspace-available-p run)
             (delib-flow--render-selected-project-filing-workspace run)
           (delib-flow--render-selected-note-filing-workspace run)))
        (unless (bolp) (insert "\n"))
        (setq buffer-read-only nil)
        (delib-flow--protect-managed-regions)
        (delib-flow--annotate-focused-filing-workspace-action-lines run)
        (goto-char (point-min)))
      (add-hook 'kill-buffer-hook #'delib-flow--focused-filing-workspace-buffer-killed nil t))
    buffer))

(defun delib-flow--filing-preview-visible-p (run)
  "Return non-nil when filing preview should be expanded for RUN."
  (let ((filing (plist-get run :filing)))
    (or (plist-get filing :draft-items)
        (plist-get filing :approved-items)
        (plist-get filing :rejected-items)
        (plist-get filing :conflicts)
        (plist-get filing :target-locations))))



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
             (delib-flow--render-editable-block-with-editor-help run 'operator-notes))
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

(defun delib-flow--details-cloud-failure-review-text (run)
  "Return cloud-failure review text for RUN."
  (if (delib-flow--cloud-failure-active-p run)
      (format "\n**** Cloud failure review\n%s\n"
              (delib-flow--render-editable-block
               run 'cloud-failure-review))
    ""))

(defun delib-flow--details-working-context-text (run)
  "Return accepted working-context details text for RUN."
  (let ((working (delib-flow--run-working-context run))
        (inspect-review (delib-flow--review-record (delib-flow--run-working-context run)
                                                   'inspect-source))
        (match-review (delib-flow--review-record (delib-flow--run-working-context run)
                                                 'match-project)))
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
            (delib-flow--details-cloud-failure-review-text run))))

(defun delib-flow--details-audit-text (run)
  "Return audit details text for RUN."
  (format "*** Audit status\n%s\n\n%s\n\n**** Audit navigation\n%s\n"
          (delib-flow--audit-run-state-text run)
          (delib-flow--audit-stage-readiness-text run)
          (delib-flow--audit-navigation-text run)))

(defun delib-flow--render-details-section (run)
  "Return Org text for the Details section from RUN."
  (concat
   (delib-flow--render-source-section run)
   "\n"
   (delib-flow--details-working-context-text run)
   (delib-flow--latest-stage-input-text run)
   "\n"
   (delib-flow--latest-stage-raw-output-text run)
   "\n"
   (delib-flow--render-stage-history-section run)
   "\n"
   (delib-flow--details-audit-text run)))

(defun delib-flow--project-recovery-actions (run)
  "Return secondary recovery actions for project mode in RUN."
  (let ((recovery-ids '(inspect-source
                        match-project
                        discover-reference-material
                        manual-project-match
                        decide-cloud-pass
                        integrate-into-source
                        refresh-buffer
                        abort-run)))
    (seq-filter
     (lambda (action)
       (and (eq (plist-get action :status) 'available)
            (memq (plist-get action :id) recovery-ids)))
     (delib-flow--sorted-actions run))))

(defun delib-flow--render-action-summary-lines (actions)
  "Return compact non-hotkey summary lines for ACTIONS."
  (mapconcat
   (lambda (action)
     (let ((reason (plist-get action :reason)))
       (format "- %s%s"
               (plist-get action :label)
               (if reason
                   (format ": %s" reason)
                 ""))))
   actions
   "\n"))

(defun delib-flow--render-valid-next-actions-section (run)
  "Return Org text for the Valid next actions section."
  (if (delib-flow--project-flow-active-p run)
      (string-join
       (delq nil
             (list
              (format "*** Active project loop\n- Primary numbered palette: `Filing preview > Project workspace`\n- Current step: %s\n- Primary next action: %s\n- Use the numbered actions in the project workspace; this section stays summary-only while a project package is active."
                      (substring-no-properties
                       (cdr (assoc (delib-flow--project-workflow-current-step run)
                                   '((propose . "Propose project")
                                     (draft-project . "Draft selected project")
                                     (extract-work . "Extract follow-on work")
                                     (draft-items . "Draft selected extracted item")
                                     (review-package . "Review package")
                                     (file . "File project")))))
                      (delib-flow--project-workflow-next-action-label run))
              (when-let ((actions (delib-flow--project-recovery-actions run)))
                (format "*** Recover / reframe\n%s"
                        (delib-flow--render-action-summary-lines actions)))))
       "\n\n")
    (mapconcat #'delib-flow--format-action-line
               (delib-flow--sorted-actions run)
               "")))

(defun delib-flow--render-action-lines (actions)
  "Return rendered control-buffer lines for ACTIONS."
  (mapconcat #'delib-flow--format-action-line actions ""))

(defun delib-flow--format-compact-action-line (action)
  "Return a compact single-line display line for ACTION."
  (format "- [%s] %s"
          (or (plist-get action :shortcut) "?")
          (plist-get action :label)))

(defun delib-flow--render-compact-action-lines (actions)
  "Return compact rendered lines for ACTIONS."
  (mapconcat #'delib-flow--format-compact-action-line actions "\n"))

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

(delib-flow--define-function delib-flow--filing-family-items
			     (run family)
			     "Return filing candidate ITEMS for artifact FAMILY in RUN."
			     (let
				 ((kind
				   (pcase family
				     ('actions 'next-action)
				     ('waiting-fors 'waiting-for)
				     ('reference-notes 'reference-note)
				     ('project-proposals 'project)
				     (_ nil))))
			       (seq-filter
				(lambda (item)
				  (eq (plist-get item :kind) kind))
				(plist-get (plist-get run :filing)
					   :draft-items))))


(defun delib-flow--filing-family-shortlist-status (run family)
  "Return shortlist status text for artifact FAMILY in RUN."
  (let ((items (delib-flow--filing-family-items run family)))
    (cond
     ((delib-flow--artifact-family-selected-candidate-id run family)
      "The selected item is highlighted first so the remaining shortlist stays secondary.")
     (items
      "These are the remaining shortlist candidates for this family. Pick one to promote into the selected-item loop.")
     (t
      "No shortlist candidates are active for this family right now."))))

(defun delib-flow--filing-family-shortlist-text (run family)
  "Return shortlist detail text for artifact FAMILY in RUN."
  (let* ((items (delib-flow--filing-family-items run family))
         (selected-id (delib-flow--artifact-family-selected-candidate-id run family)))
    (if items
        (mapconcat
         (lambda (item)
           (let ((selected (equal (delib-flow--artifact-candidate-id item)
                                  selected-id)))
             (format "- %s %s%s"
                     (if selected "[selected]" "[ ]")
                     (delib-flow--draft-item-preview-line item)
                     (if selected
                         " <- active item"
                       ""))))
         items
         "\n")
      "No shortlist candidates are available yet.")))

(delib-flow--define-function delib-flow--family-support-action-label
			     (family)
			     "Return focused support action label for artifact FAMILY."
			     (pcase family
			       ('actions
				"Find Support for Selected Action")
			       ('waiting-fors
				"Find Support for Selected Waiting-For")
			       ('reference-notes
				"Find Support for Selected Note")
			       ('project-proposals
				"Find Support for Selected Project")
			       (_ "Find Support for Selected Artifact")))


(defun delib-flow--family-support-status (run family)
  "Return supporting-material status text for artifact FAMILY in RUN."
  (cond
   ((delib-flow--artifact-family-selected-support-candidates run family)
    (format "Selected support is attached to this %s draft. Review it before regenerating if you want the next pass to lean on that material."
            (delib-flow--artifact-family-label family)))
   ((delib-flow--artifact-family-available-support-candidates run family)
    (format "Support suggestions are ready for this selected %s, but none are attached yet. Choose only the notes you want shaping the next draft pass."
            (delib-flow--artifact-family-label family)))
   ((delib-flow--selected-family-support-item run family)
    (format "No selected support is attached yet. Run `%s` if you want item-local support suggestions before the next draft pass."
            (delib-flow--family-support-action-label family)))
   (t
    "Choose a selected item first if you want support suggestions here.")))

(defun delib-flow--support-candidate-detail-lines (candidates)
  "Return detail lines for support CANDIDATES."
  (mapconcat
   (lambda (candidate)
     (format "- %s\n  Score: %s\n  Reasons: %s"
             (delib-flow--reference-note-support-line candidate)
             (or (plist-get candidate :support-score) 0)
             (if-let ((reasons (plist-get candidate :support-reasons)))
                 (string-join reasons ", ")
               "none")))
   candidates
   "\n"))

(defun delib-flow--family-support-text (run family)
  "Return supporting-material detail text for artifact FAMILY in RUN."
  (let ((attached (delib-flow--artifact-family-selected-support-candidates run family))
        (suggested (delib-flow--artifact-family-available-support-candidates run family)))
    (string-join
     (delq nil
           (list
            (if attached
                (format "***** Attached support\n%s"
                        (delib-flow--support-candidate-detail-lines attached))
              "***** Attached support\nNo support is attached to this draft yet.")
            (if suggested
                (format "***** Suggested support to choose from\n%s"
                        (delib-flow--support-candidate-detail-lines suggested))
              "***** Suggested support to choose from\nNo support suggestions are available yet.")))
     "\n\n")))

(delib-flow--define-function delib-flow--draft-history-entry-label
			     (family index)
			     "Return display label for FAMILY draft history entry INDEX."
			     (format "Revision %s (%s)" index
				     (pcase family
				       ('actions "action")
				       ('waiting-fors "waiting-for")
				       ('reference-notes "note")
				       ('project-proposals "project")
				       (_ "artifact"))))


(delib-flow--define-function delib-flow--draft-history-entry-fragment
			     (draft)
			     "Return compact preview fragment for archived DRAFT."
			     (pcase (plist-get draft :kind)
			       ('reference-note
				(format
				 "- Draft title: %s\n#+begin_example\n%s\n#+end_example"
				 (or (plist-get draft :text)
				     "Untitled note draft")
				 (string-trim-right
				  (or (plist-get draft :draft-body)
				      "No draft body is available."))))
			       ('project
				(format
				 "- Project title: %s\n- State: %s\n- First item: %s"
				 (or (plist-get draft :title)
				     "Untitled project")
				 (or (plist-get draft :state) 'active)
				 (or
				  (plist-get
				   (plist-get draft :first-item) :text)
				  "none")))
			       (_
				(format "- Draft text: %s"
					(or (plist-get draft :text)
					    "No draft text is available.")))))


(defun delib-flow--family-draft-history-count-line (history)
  "Return count summary line for artifact draft HISTORY."
  (let ((count (length history)))
    (format "%s prior draft revision%s are stored for this selected item."
            count
            (if (= count 1) "" "s"))))

(defun delib-flow--family-draft-history-next-step-line (history)
  "Return next-step guidance for artifact draft HISTORY."
  (if (cdr history)
      "Compare the current draft against the latest prior pass below, restore the previous draft quickly, or choose any saved revision from the local action palette."
    "Compare the current draft against the latest prior pass below, or restore the previous draft if the current pass regressed."))

(defun delib-flow--family-draft-history-status (run family)
  "Return draft history status text for artifact FAMILY in RUN."
  (let ((history (delib-flow--artifact-family-draft-history run family)))
    (cond
     (history
      (format "%s %s"
              (delib-flow--family-draft-history-count-line history)
              (delib-flow--family-draft-history-next-step-line history)))
     ((delib-flow--artifact-family-selected-draft run family)
      "No prior draft revisions are stored yet. The next regeneration will become reversible here.")
     (t
      "No draft history is available until this selected item has at least one drafted revision."))))

(delib-flow--define-function delib-flow--family-draft-history-text
			     (run family)
			     "Return draft history detail text for artifact FAMILY in RUN."
			     (let
				 ((current
				   (delib-flow--artifact-family-selected-draft
				    run family))
				  (history
				   (delib-flow--artifact-family-draft-history
				    run family)))
			       (cond
				((and current history)
				 (concat "***** Current draft\n"
					 (delib-flow--draft-history-entry-fragment
					  current)
					 (when-let
					     ((snapshot
					       (delib-flow--draft-evidence-snapshot
						run family current)))
					   (concat "\n"
						   (delib-flow--draft-evidence-snapshot-summary
						    snapshot)
						   "\n"
						   (delib-flow--draft-evidence-snapshot-detail
						    snapshot)))
					 "\n\n***** Previous draft\n"
					 (delib-flow--draft-history-entry-fragment
					  (car history))
					 (when-let
					     ((snapshot
					       (delib-flow--draft-evidence-snapshot
						run family
						(car history))))
					   (concat "\n"
						   (delib-flow--draft-evidence-snapshot-summary
						    snapshot)
						   "\n"
						   (delib-flow--draft-evidence-snapshot-detail
						    snapshot)
						   (when-let
						       ((current-snapshot
							 (delib-flow--draft-evidence-snapshot
							  run family
							  current)))
						     (concat "\n\n"
							     (delib-flow--draft-evidence-snapshot-diff-text
							      current-snapshot
							      snapshot)))))
					 (if (cdr history)
					     (concat
					      "\n\n***** Earlier revisions\n"
					      (mapconcat
					       (lambda (entry)
						 (let
						     ((label
						       (delib-flow--draft-history-entry-label
							family
							(+ 2
							   (cl-position
							    entry
							    (cdr
							     history)
							    :test
							    #'equal))))
						      (title
						       (pcase
							   (plist-get
							    entry
							    :kind)
							 ('reference-note
							  (or
							   (plist-get
							    entry
							    :text)
							   "Untitled note draft"))
							 ('project
							  (or
							   (plist-get
							    entry
							    :title)
							   "Untitled project"))
							 (_
							  (or
							   (plist-get
							    entry
							    :text)
							   "Untitled draft")))))
						   (concat
						    (format "- %s: %s"
							    label
							    title)
						    (when-let
							((snapshot
							  (delib-flow--draft-evidence-snapshot
							   run family
							   entry)))
						      (concat "\n  "
							      (string-remove-prefix
							       "- "
							       (delib-flow--draft-evidence-snapshot-summary
								snapshot)))))))
					       (cdr history) "\n"))
					   "")))
				(current
				 "No earlier draft revision is available yet. Regenerate this selected item once to create the first reversible prior draft.")
				(t
				 "No draft history is available yet."))))


(delib-flow--define-function delib-flow--artifact-source-label
			     (source)
			     "Return operator-facing provenance label for artifact SOURCE."
			     (pcase source
			       ('source
				"directly from the current source")
			       ('retained-context
				"from retained reference material")
			       ('project-proposal
				"from project-proposal shaping")
			       (_ "from the current drafting loop")))


(delib-flow--define-function delib-flow--selected-draft-evidence-lines
			     (run family)
			     "Return compact source-evidence lines for selected FAMILY draft in RUN."
			     (let
				 ((candidate
				   (delib-flow--artifact-family-selected-candidate
				    run family)))
			       (delete-dups
				(delq nil
				      (pcase family
					('actions
					 (list
					  (or
					   (plist-get candidate :text)
					   (delib-flow--source-action-line
					    run))
					  (delib-flow--source-action-line
					   run)))
					('waiting-fors
					 (list
					  (or
					   (plist-get candidate :text)
					   (delib-flow--source-waiting-for-line
					    run))
					  (delib-flow--source-waiting-for-line
					   run)))
					('reference-notes
					 (delib-flow--reference-note-source-highlights
					  (or candidate
					      (delib-flow--artifact-family-selected-draft
					       run 'reference-notes))
					  run))
					('project-proposals
					 (list
					  (format
					   "Project title seed: %s"
					   (delib-flow--project-proposal-title
					    run))
					  (format
					   "First item seed: %s"
					   (or
					    (plist-get
					     (delib-flow--project-proposal-first-item
					      run)
					     :text)
					    "none"))
					  (plist-get
					   (plist-get candidate
						      :first-item)
					   :text)))
					(_ nil))))))


(delib-flow--define-function
 delib-flow--format-source-evidence-excerpt (package evidence-line)
 "Return compact source excerpt for PACKAGE matching EVIDENCE-LINE."
 (let ((needle (downcase (string-trim (or evidence-line ""))))
       found)
   (dolist (candidate (delib-flow--source-evidence-candidate-lines package))
     (let* ((display-line (car candidate))
            (match-line (cdr candidate))
            (normalized
             (downcase
              (delib-flow--normalize-source-evidence-line match-line))))
       (when (and (not found)
                  (not (string-empty-p needle))
                  (or (string= normalized needle)
                      (string-match-p (regexp-quote needle)
                                      (downcase match-line))
                      (string-match-p (regexp-quote normalized) needle)))
         (setq found display-line))))
   (or found
       (when (delib-flow--non-empty-string-p evidence-line)
         (format "- Derived evidence: %s" evidence-line)))))

(defun delib-flow--source-evidence-useful-line-p (line)
  "Return non-nil when LINE is a useful source-evidence candidate."
  (let ((text (string-trim (or line ""))))
    (and (delib-flow--non-empty-string-p text)
         (not (string-match-p "\\`Preview:[[:space:]]*" text))
         (not (string-match-p "\\`\\(?:From\\|To\\|Cc\\|Bcc\\|Subject\\|Date\\|Reply-To\\):" text))
         (not (string-match-p "\\b\\(?:unsubscribe\\|view in browser\\|manage preferences\\)\\b"
                              (downcase text))))))

(defun delib-flow--source-evidence-candidate-lines (package)
  "Return ordered source-evidence candidates for PACKAGE.

Each entry is a cons of display text and match text."
  (let ((index 0)
        raw-candidates
        reduced-candidates)
    (dolist (raw-line (delib-flow--source-body-lines package))
      (setq index (1+ index))
      (let ((clean-raw (string-trim raw-line)))
        (when (delib-flow--source-evidence-useful-line-p clean-raw)
          (push (cons (format "- L%s: %s" index clean-raw)
                      clean-raw)
                raw-candidates))))
    (dolist (unit (ignore-errors (delib-flow--reference-note-source-lines package)))
      (let ((clean-unit (string-trim unit)))
        (when (delib-flow--source-evidence-useful-line-p clean-unit)
          (push (cons (format "- Source: %s" clean-unit)
                      clean-unit)
                reduced-candidates))))
    (delete-dups
     (append (nreverse raw-candidates)
             (nreverse reduced-candidates)))))


(defun delib-flow--selected-draft-source-evidence-excerpts (run family)
  "Return quoted source evidence excerpts for selected FAMILY draft in RUN."
  (delete-dups
   (delq nil
         (mapcar (lambda (line)
                   (delib-flow--format-source-evidence-excerpt run line))
                 (delib-flow--selected-draft-evidence-lines run family)))))

(defun delib-flow--selected-draft-support-context-lines (run family)
  "Return focused support context lines for selected FAMILY draft in RUN."
  (when-let ((context (or (plist-get
                           (delib-flow--artifact-family-selected-draft run family)
                           :support-context)
                          (delib-flow--artifact-family-selected-support-context
                           run family))))
    (seq-filter
     (lambda (line)
       (not (string-empty-p line)))
     (mapcar #'string-trim (split-string context "\n")))))

(defun delib-flow--draft-quality-gap-lines (run family draft)
  "Return quality-gap lines for selected FAMILY DRAFT in RUN."
  (let ((warnings (delib-flow--draft-item-warnings draft))
        gaps)
    (dolist (warning warnings)
      (push (format "- %s%s"
                    (plist-get warning :message)
                    (if-let ((fix (delib-flow--draft-item-warning-remediation warning)))
                        (format " Next fix: %s" fix)
                      ""))
            gaps))
    (unless (delib-flow--artifact-family-selected-support-candidates run family)
      (push "- No focused support is attached yet. The current draft still leans on source-local context only."
            gaps))
    (unless gaps
      (setq gaps
            '("- No immediate quality gaps are flagged. This draft is currently supported by the available evidence and warning checks.")))
    (nreverse gaps)))

(defun delib-flow--family-evidence-review-status (run family)
  "Return evidence-review status text for selected FAMILY in RUN."
  (if-let ((draft (delib-flow--artifact-family-selected-draft run family)))
      (let ((warnings (delib-flow--draft-item-warnings draft))
            (support (delib-flow--artifact-family-selected-support-candidates
                      run family)))
        (format
         "This review surface explains why the draft exists, which evidence shaped it, and what still looks weak before filing. %s warning%s and %s focused support item%s are currently attached."
         (length warnings)
         (if (= (length warnings) 1) "" "s")
         (length support)
         (if (= (length support) 1) "" "s")))
    "Draft one selected item first to review its evidence and quality signals here."))

(defun delib-flow--evidence-review-lines (lines fallback &optional transform)
  "Return joined LINES or FALLBACK, optionally applying TRANSFORM."
  (if lines
      (mapconcat (or transform #'identity) lines "\n")
    fallback))

(defun delib-flow--evidence-review-bullets (lines fallback)
  "Return LINES as bullet text, or FALLBACK when empty."
  (delib-flow--evidence-review-lines
   lines
   fallback
   (lambda (line)
     (format "- %s" line))))

(defun delib-flow--evidence-review-section (heading body)
  "Return an evidence-review section with HEADING and BODY."
  (format "***** %s\n%s" heading body))

(defun delib-flow--family-evidence-review-why-section (run candidate snapshot)
  "Return rationale section for CANDIDATE SNAPSHOT in RUN."
  (delib-flow--evidence-review-section
   "Why this draft exists"
   (format "- Draft reason: %s\n- Selected candidate origin: %s\n- Source title: %s"
           (or (plist-get snapshot :draft-reason)
               "No explicit drafting rationale is recorded.")
           (delib-flow--artifact-source-label
            (or (plist-get snapshot :candidate-origin)
                (plist-get candidate :source)))
           (or (plist-get snapshot :source-title)
               (delib-flow--source-display-title run)))))

(defun delib-flow--family-evidence-review-source-lines-section (snapshot)
  "Return compact source-lines section for SNAPSHOT."
  (delib-flow--evidence-review-section
   "Source lines that informed it"
   (delib-flow--evidence-review-bullets
    (plist-get snapshot :source-evidence-lines)
    "- No compact source-evidence lines are available yet.")))

(defun delib-flow--family-evidence-review-source-excerpts-section (snapshot)
  "Return source-excerpts section for SNAPSHOT."
  (delib-flow--evidence-review-section
   "Source excerpts"
   (delib-flow--evidence-review-lines
    (plist-get snapshot :source-excerpts)
    "- No matching source excerpt is available for the current evidence lines.")))

(defun delib-flow--family-evidence-review-support-items-section (snapshot)
  "Return support-items section for SNAPSHOT."
  (delib-flow--evidence-review-section
   "Supporting materials that informed it"
   (delib-flow--evidence-review-lines
    (delib-flow--draft-evidence-snapshot-support-items snapshot)
    "- No focused support material is attached to this selected draft yet.")))

(defun delib-flow--family-evidence-review-support-context-section (snapshot)
  "Return support-context section for SNAPSHOT."
  (delib-flow--evidence-review-section
   "Focused support context"
   (delib-flow--evidence-review-bullets
    (plist-get snapshot :support-context-lines)
    "- No focused support context has been captured for this draft yet.")))

(defun delib-flow--family-evidence-review-family-checks-section (snapshot)
  "Return family-specific checks section for SNAPSHOT."
  (delib-flow--evidence-review-section
   (or (plist-get snapshot :family-review-heading)
       "Artifact-specific checks")
   (delib-flow--evidence-review-bullets
    (plist-get snapshot :family-review-lines)
    "- No family-specific review checks are available for this draft yet.")))

(defun delib-flow--family-evidence-review-quality-gaps-section (snapshot)
  "Return quality-gaps section for SNAPSHOT."
  (delib-flow--evidence-review-section
   "Remaining quality gaps before filing"
   (string-join
    (or (plist-get snapshot :quality-gaps)
        '("- No immediate quality gaps are flagged."))
    "\n")))

(defun delib-flow--family-evidence-review-sections (run candidate snapshot)
  "Return evidence-review sections for CANDIDATE SNAPSHOT in RUN."
  (list
   (delib-flow--family-evidence-review-why-section run candidate snapshot)
   (delib-flow--family-evidence-review-source-lines-section snapshot)
   (delib-flow--family-evidence-review-source-excerpts-section snapshot)
   (delib-flow--family-evidence-review-support-items-section snapshot)
   (delib-flow--family-evidence-review-support-context-section snapshot)
   (delib-flow--family-evidence-review-family-checks-section snapshot)
   (delib-flow--family-evidence-review-quality-gaps-section snapshot)))

(defun delib-flow--family-evidence-review-text (run family)
  "Return evidence-review detail text for selected FAMILY in RUN."
  (if-let* ((draft (delib-flow--artifact-family-selected-draft run family))
            (candidate
             (or (delib-flow--artifact-family-selected-candidate run family)
                 draft))
            (snapshot (delib-flow--draft-evidence-snapshot run family draft)))
      (string-join
       (delib-flow--family-evidence-review-sections run candidate snapshot)
       "\n\n")
    "No evidence review is available until this selected item has a draft."))


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

(delib-flow--define-function delib-flow--draft-item-status (run)
			     "Return filing preview status text for RUN."
			     (let*
				 ((filing (plist-get run :filing))
				  (draft-items
				   (plist-get filing :draft-items))
				  (ready-indexes
				   (delib-flow--draft-item-selection-indexes
				    draft-items
				    #'delib-flow--draft-item-ready-p))
				  (blocked-indexes
				   (delib-flow--draft-item-selection-indexes
				    draft-items
				    (lambda (item)
				      (not
				       (delib-flow--draft-item-ready-p
					item))))))
			       (cond
				((plist-get filing
					    :selection-blocking-warnings)
				 (format
				  "You have filing candidates, but the last approval attempt is blocked. Choose a ready queue item (%s), or reject the blocked item (%s)."
				  (delib-flow--selection-index-list
				   ready-indexes)
				  (delib-flow--selection-index-list
				   blocked-indexes)))
				((and draft-items
				      (plist-get filing
						 :approved-items))
                                 (if (delib-flow--approved-project-package-p run)
                                     "A project package is approved and ready to file. Review the planned targets, file the package, or keep refining the local project loop before filing."
				   (format
				    "One artifact is approved and ready to file. More candidates remain in the queue. Review the planned targets, file the approved artifact, or approve another ready item (%s)."
				    (delib-flow--selection-index-list
				     ready-indexes))))
				(draft-items
                                 (if (delib-flow--project-flow-active-p run)
                                     "A project package loop is active. Keep the selected project in focus, use the local project actions, then approve and file the package when the included work set looks right."
				   (format
				    "A filing queue is ready. Choose one ready item (%s), approve it, then review where it will go before filing."
				    (delib-flow--selection-index-list
				     ready-indexes))))
				(t "No filing queue is available yet."))))


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

(delib-flow--define-function delib-flow--current-filing-choice-status
			     (run)
			     "Return active filing-choice status text for RUN."
			     (cond
			      ((and
				(plist-get (plist-get run :filing)
					   :approved-items)
				(eq
				 (plist-get
				  (delib-flow--active-filing-item run)
				  :kind)
				 'project))
			       "This is the approved project package that will be staged next. File it when the project heading and included work items look right.")
			      ((and
				(delib-flow--preview-selected-filing-items
				 run)
				(eq
				 (plist-get
				  (delib-flow--active-filing-item run)
				  :kind)
				 'project))
			       (if
				   (delib-flow--selected-project-drafted-p
				    run)
				   "This is the currently selected project draft. Approve it if the project definition is ready, or regenerate only this project."
				 "This is the currently selected project candidate. Draft it next if you want help tightening the title, tags, first item, or package framing before approval."))
			      ((and
				(plist-get (plist-get run :filing)
					   :approved-items)
				(eq
				 (plist-get
				  (delib-flow--active-filing-item run)
				  :kind)
				 'waiting-for))
			       "This is the selected waiting-for that will be staged next. File it when the owner and blocked outcome look right.")
			      ((and
				(delib-flow--preview-selected-filing-items
				 run)
				(eq
				 (plist-get
				  (delib-flow--active-filing-item run)
				  :kind)
				 'waiting-for))
			       (if
				   (delib-flow--selected-waiting-for-drafted-p
				    run)
				   "This is the currently selected waiting-for draft. Approve it if the dependency wording is ready, or regenerate only this waiting-for."
				 "This is the currently selected waiting-for candidate. Draft it next if you want help tightening the owner or blocked outcome before approval."))
			      ((and
				(plist-get (plist-get run :filing)
					   :approved-items)
				(eq
				 (plist-get
				  (delib-flow--active-filing-item run)
				  :kind)
				 'next-action))
			       "This is the selected action that will be staged next. File it when the wording looks right.")
			      ((and
				(delib-flow--preview-selected-filing-items
				 run)
				(eq
				 (plist-get
				  (delib-flow--active-filing-item run)
				  :kind)
				 'next-action))
			       (if
				   (delib-flow--selected-action-drafted-p
				    run)
				   "This is the currently selected action draft. Approve it if the wording is ready, or regenerate only this action."
				 "This is the currently selected action candidate. Draft it next if you want help tightening the wording before approval."))
			      ((plist-get (plist-get run :filing)
					  :approved-items)
			       "This is the artifact that File Approved Outputs will stage next.")
			      ((delib-flow--preview-selected-filing-items
				run)
			       "This is the queue item your current Selection points to.")
			      ((delib-flow--non-empty-string-p
				(delib-flow--filing-selection-value
				 run))
			       "The current selection does not point to one ready queue item yet.")
			      (t
			       "No filing artifact is currently selected.")))


(delib-flow--define-function delib-flow--current-filing-choice-text
			     (run)
			     "Return active filing-choice detail text for RUN."
			     (if-let ((items
                                       (delib-flow--planned-file-preview-items
                                        run)))
                                 (if (> (length items) 1)
                                     (concat
                                      (delib-flow--draft-item-preview-text items)
                                      "\n- Bundle state: this project package will stage the project together with the included ready child work items.\n- Next move: review the included items, then file the package when the bundle looks right.")
                                   (let ((single-item (car items)))
				     (if
				         (eq (plist-get single-item :kind)
					     'reference-note)
				         (concat
				          (delib-flow--draft-item-preview-text
				           items)
				          "\n\n"
				          (delib-flow--reference-note-preview-fragment
				           single-item run))
				       (pcase
				           (plist-get single-item :kind)
				         ('project
				          (concat
				           (delib-flow--draft-item-preview-text
				            items)
				           "\n"
				           (if
					       (delib-flow--selected-project-drafted-p
					        run)
					       "- Draft state: this selected project already has a drafted definition.\n- Next move: extract actions, waiting-fors, or reference notes, then approve and file the package as a bundle."
					     "- Draft state: this is still the raw selected project candidate.\n- Next move: run `Draft Selected Project` to lock in the project package before extracting follow-on work.")))
				         ('next-action
				          (concat
				           (delib-flow--draft-item-preview-text
				            items)
				           "\n"
				           (if
					       (delib-flow--selected-action-drafted-p
					        run)
					       "- Draft state: this selected action already has drafted wording. Review or regenerate this action below.\n- Next move: approve it when the wording is clear, or file it if already approved."
					     "- Draft state: this is still the raw selected action candidate.\n- Next move: run `Draft Selected Action` if you want a one-action wording pass before approval.")))
				         ('waiting-for
				          (concat
				           (delib-flow--draft-item-preview-text
				            items)
				           "\n"
				           (if
					       (delib-flow--selected-waiting-for-drafted-p
					        run)
					       "- Draft state: this selected waiting-for already has drafted dependency wording. Review or regenerate this waiting-for below.\n- Next move: approve it when the owner and blocker are clear, or file it if already approved."
					     "- Draft state: this is still the raw selected waiting-for candidate.\n- Next move: run `Draft Selected Waiting-For` if you want a one-waiting-for wording pass before approval.")))
				         (_
				          (delib-flow--draft-item-preview-text
				           items))))))
			       "Choose one ready queue item to preview it here."))

(defun delib-flow--workspace-selected-item-status (run family)
  "Return selected-item workspace status text for artifact FAMILY in RUN."
  (let ((label (delib-flow--artifact-family-label family))
        (item (delib-flow--workspace-selected-item run family)))
    (cond
     ((delib-flow--artifact-family-selected-draft run family)
      (format "This selected %s is the active drafted item for this workspace." label))
     (item
      (format "This selected %s is the active candidate for this workspace. Draft it here if you want item-local refinement before approval." label))
     (t
      (format "No selected %s is active in this workspace yet. Pick one from the shortlist first." label)))))

(defun delib-flow--workspace-selected-item-text (run family)
  "Return selected-item workspace detail text for artifact FAMILY in RUN."
  (let ((label (delib-flow--artifact-family-label family)))
    (if-let ((item (delib-flow--workspace-selected-item run family)))
        (concat
         (delib-flow--draft-item-preview-text (list item))
         "\n"
         (if (delib-flow--artifact-family-selected-draft run family)
             (format "- Workspace state: this selected %s already has drafted content below.\n- Next move: review it, regenerate it, approve it, or file it if already approved."
                     label)
           (format "- Workspace state: this selected %s is still the raw selected candidate.\n- Next move: run the local draft action in this workspace if you want item-local enrichment before approval."
                   label)))
      (format "Select one %s from the shortlist or press `s` to pick it with completion." label))))

(defun delib-flow--selected-action-workspace-status (run)
  "Return selected-item status for the action workspace in RUN."
  (delib-flow--workspace-selected-item-status run 'actions))

(defun delib-flow--selected-action-workspace-text (run)
  "Return selected-item text for the action workspace in RUN."
  (delib-flow--workspace-selected-item-text run 'actions))

(defun delib-flow--selected-waiting-for-workspace-status (run)
  "Return selected-item status for the waiting-for workspace in RUN."
  (delib-flow--workspace-selected-item-status run 'waiting-fors))

(defun delib-flow--selected-waiting-for-workspace-text (run)
  "Return selected-item text for the waiting-for workspace in RUN."
  (delib-flow--workspace-selected-item-text run 'waiting-fors))

(defun delib-flow--selected-note-workspace-status (run)
  "Return selected-item status for the note workspace in RUN."
  (delib-flow--workspace-selected-item-status run 'reference-notes))

(defun delib-flow--selected-note-workspace-text (run)
  "Return selected-item text for the note workspace in RUN."
  (delib-flow--workspace-selected-item-text run 'reference-notes))

(defun delib-flow--selected-project-workspace-status (run)
  "Return selected-item status for the project workspace in RUN."
  (delib-flow--workspace-selected-item-status run 'project-proposals))

(defun delib-flow--selected-project-workspace-text (run)
  "Return selected-item text for the project workspace in RUN."
  (delib-flow--workspace-selected-item-text run 'project-proposals))


(defun delib-flow--staged-content-preview-status (run)
  "Return staged-content-preview status text for RUN."
  (cond
   ((plist-get (plist-get run :filing) :approved-items)
    "These edits are staged in target buffers only. Review them here or in the opened target buffer before saving anything.")
   ((delib-flow--selected-filing-item-draft-warning run)
    "No staged content is ready yet. Draft the selected item first so filing stages the enriched version rather than the raw candidate.")
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

(defun delib-flow--filing-project-preview-level (package)
  "Return preview outline level for project children in PACKAGE."
  (if-let ((project-title (delib-flow--filing-project-title package)))
      (condition-case nil
          (1+ (delib-flow--matched-project-level project-title))
        (error 2))
    2))

(defun delib-flow--staged-project-item-preview (item package)
  "Return exact staged text preview for project ITEM from PACKAGE."
  (if (eq (plist-get item :kind) 'project)
      (concat
       (delib-flow--fill-capture-template
        (delib-flow--project-item-capture-template item)
        (delib-flow--capture-template-context
         item package (plist-get item :title) nil))
       (mapconcat
        (lambda (child)
          (concat "\n" (delib-flow--project-item-entry child 2)))
        (delib-flow--project-additional-child-items item)
        ""))
    (let* ((project-title (delib-flow--filing-project-title package))
           (level (delib-flow--filing-project-preview-level package)))
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
  (if-let ((warning (delib-flow--selected-filing-item-draft-warning run)))
      (format "%s\n  Fix: %s"
              (plist-get warning :message)
              (delib-flow--draft-item-warning-remediation warning))
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
      "No staged content preview is available yet.")))

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
         (if (delib-flow--project-flow-active-p run)
             "- Resolution: adjust the active project package, reject the blocked package item, or approve the package again after the warnings are fixed.\n"
           "- Resolution: approve a different ready artifact, fix the blocking warnings, or reject this artifact before retrying approval.\n")
         (if item
             (concat
              (if (delib-flow--project-flow-active-p run)
                  "- Blocked package item:\n"
                "- Blocked artifact:\n")
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
     (if (delib-flow--project-flow-active-p run)
         (if (delib-flow--approved-project-package-p run)
             "- The selected project package is approved. Run `File Package` to stage the whole bundle, or keep refining the project loop before filing."
           "- Select the project row to approve the drafted project bundle together with its ready child work items. Use the local project workspace actions instead of the broader cockpit queue.")
       (if (plist-get (plist-get run :filing) :approved-items)
           "- Run `Approve Another Filing Artifact` to approve another ready queue item, or run `File Approved Outputs` to stage the approved item now."
         "- Run `Select Approved Filing Actions` to approve the chosen ready item, or run `Reject Draft Filing Artifact` if you want to discard it instead.")))))

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
    (let ((project-title (delib-flow--filing-project-title package)))
      (unless project-title
        (error "Approved project filing requires an active project package"))
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
        (let ((items (delib-flow--selected-filing-items run)))
          (unless (seq-some #'delib-flow--draft-item-blocking-warnings items)
            items)))
    (error nil)))

(defun delib-flow--planned-file-preview-items (run)
  "Return filing items that should drive deterministic target preview for RUN."
  (or (delib-flow--approved-items run)
      (and (delib-flow--project-flow-active-p run)
           (delib-flow--project-package-included-items run))
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
    (if (delib-flow--approved-project-package-p run)
        "If you file now, delib-flow will stage the approved project package into these targets."
      "If you file now, delib-flow will stage the approved artifact into these targets."))
   ((and (delib-flow--project-flow-active-p run)
         (delib-flow--project-package-root-for-filing run))
    "If you file now, delib-flow will stage the active project package bundle into these targets.")
   ((delib-flow--preview-selected-filing-items run)
    (if (delib-flow--project-package-selection-p run (delib-flow--selected-filing-item run))
        "If you approve the currently selected project package, delib-flow will file the project and its ready child work into these targets."
      "If you approve the currently selected queue item, it will file to these targets."))
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

(defconst delib-flow--filing-preview-action-ids
  '(extract-actions
    extract-waiting-for
    suggest-reference-notes
    edit-operator-intent
    draft-selected-action
    find-support-for-selected-action
    restore-previous-selected-action-draft
    draft-selected-waiting-for
    find-support-for-selected-waiting-for
    restore-previous-selected-waiting-for-draft
    draft-selected-reference-note
    find-support-for-selected-reference-note
    choose-support-for-selected-reference-note
    clear-selected-reference-note-support
    edit-selected-reference-note-title
    choose-reference-note-template
    edit-selected-reference-note-target-path
    edit-selected-reference-note-draft-body
    refresh-selected-reference-note-draft-body
    edit-selected-reference-note-source-highlights
    refresh-selected-reference-note-source-highlights
    edit-selected-reference-note-related-material
    refresh-selected-reference-note-related-material
    edit-selected-reference-note-reuse-angle
    refresh-selected-reference-note-reuse-angle
    restore-previous-selected-reference-note-draft
    draft-selected-project
    find-support-for-selected-project
    restore-previous-selected-project-draft
    select-approved-filing-actions
    reject-draft-filing-artifact
    file-approved-outputs
    resolve-filing-conflict)
  "Action ids exposed in the filing-preview control surface.")

(defconst delib-flow--filing-preview-action-priorities
  '((project-proposals
     extract-actions
     extract-waiting-for
     suggest-reference-notes
     edit-operator-intent
     draft-selected-project
     find-support-for-selected-project
     restore-previous-selected-project-draft
     select-approved-filing-actions
     file-approved-outputs
     reject-draft-filing-artifact
     resolve-filing-conflict
     draft-selected-action
     find-support-for-selected-action
     restore-previous-selected-action-draft
     draft-selected-waiting-for
     find-support-for-selected-waiting-for
     restore-previous-selected-waiting-for-draft
     draft-selected-reference-note)
    (waiting-fors
     draft-selected-waiting-for
     find-support-for-selected-waiting-for
     restore-previous-selected-waiting-for-draft
     select-approved-filing-actions
     file-approved-outputs
     reject-draft-filing-artifact
     resolve-filing-conflict
     draft-selected-project
     find-support-for-selected-project
     restore-previous-selected-project-draft
     draft-selected-action
     find-support-for-selected-action
     restore-previous-selected-action-draft
     draft-selected-reference-note)
    (reference-notes
     draft-selected-reference-note
     find-support-for-selected-reference-note
     choose-support-for-selected-reference-note
     clear-selected-reference-note-support
     edit-selected-reference-note-title
     choose-reference-note-template
     edit-selected-reference-note-target-path
     refresh-selected-reference-note-source-highlights
     refresh-selected-reference-note-related-material
     refresh-selected-reference-note-reuse-angle
     restore-previous-selected-reference-note-draft
     select-approved-filing-actions
     file-approved-outputs
     reject-draft-filing-artifact
     resolve-filing-conflict
     draft-selected-action
     draft-selected-waiting-for
     draft-selected-project)
    (actions
     draft-selected-action
     find-support-for-selected-action
     restore-previous-selected-action-draft
     select-approved-filing-actions
     file-approved-outputs
     reject-draft-filing-artifact
     resolve-filing-conflict
     draft-selected-project
     find-support-for-selected-project
     restore-previous-selected-project-draft
     draft-selected-waiting-for
     find-support-for-selected-waiting-for
     restore-previous-selected-waiting-for-draft
     draft-selected-reference-note))
  "Preferred filing-preview action orders by active artifact family.")

(defun delib-flow--filing-preview-action-order (run)
  "Return the preferred filing-preview action order for RUN."
  (or (cdr (assq (delib-flow--active-filing-family run)
                 delib-flow--filing-preview-action-priorities))
      delib-flow--filing-preview-action-ids))

(defun delib-flow--filing-preview-actions (run)
  "Return filing-focused available actions for RUN."
  (delib-flow--sort-actions-by-id-order
   (seq-filter
    (lambda (action)
      (memq (plist-get action :id)
            delib-flow--filing-preview-action-ids))
    (delib-flow--assign-action-shortcuts
     (delib-flow--compute-actions run)))
   (delib-flow--filing-preview-action-order run)))


(defun delib-flow--filing-preview-actions-text (run)
  "Return rendered filing-focused action lines for RUN."
  (if-let ((actions (delib-flow--filing-preview-actions run)))
      (delib-flow--render-action-lines actions)
    "No filing actions are available yet."))

(defun delib-flow--recommended-filing-action (run)
  "Return the best currently available filing-focused action for RUN."
  (car (delib-flow--filing-preview-actions run)))

(defconst delib-flow--filing-loop-why-now-by-family
  '((project-proposals . "The project loop works best by keeping one drafted package in focus, extracting follow-on work from it, and only then approving or filing.")
    (waiting-fors . "The waiting-for loop works best one item at a time: choose one dependency, draft that dependency, then approve or file it.")
    (actions . "The action loop works best one item at a time: choose one action, draft that action, then approve or file it."))
  "Family-specific rationale for why a filing action is next.")

(defconst delib-flow--filing-loop-no-action-why-by-family
  '((project-proposals . "The project loop is waiting for a valid selected project package or its drafted definition before extraction or filing.")
    (waiting-fors . "The waiting-for loop is waiting for a valid selection, a draft, or a review step.")
    (actions . "The action loop is waiting for a valid selection, a draft, or a review step."))
  "Family-specific rationale for why no filing action is available yet.")

(defun delib-flow--filing-loop-why-now (run)
  "Return family-specific rationale for the current filing loop in RUN."
  (or (cdr (assq (delib-flow--active-filing-family run)
                 delib-flow--filing-loop-why-now-by-family))
      "This is the next local filing action available from the current state."))

(defun delib-flow--filing-loop-no-action-why (run)
  "Return family-specific rationale for unavailable filing actions in RUN."
  (or (cdr (assq (delib-flow--active-filing-family run)
                 delib-flow--filing-loop-no-action-why-by-family))
      "No filing action is currently available."))

(defun delib-flow--recommended-filing-action-text (run)
  "Return rendered recommendation text for the filing loop in RUN."
  (if-let ((action (delib-flow--recommended-filing-action run)))
      (format
       "- Do this next: %s\n- What it does: %s\n- Why now: %s\n- If blocked: %s"
       (plist-get action :label)
       (delib-flow--compact-summary
        (or (plist-get action :reason)
            "This is the next local filing action available from the current state.")
        (+ 12 (delib-flow--mobile-summary-limit)))
       (delib-flow--compact-summary
        (delib-flow--filing-loop-why-now run)
        (+ 12 (delib-flow--mobile-summary-limit)))
       (delib-flow--compact-summary
        (delib-flow--unblock-guidance-text run)
        (+ 12 (delib-flow--mobile-summary-limit))))
    (format
     "- Do this next: none\n- What it does: No filing action is currently available.\n- Why now: %s\n- If blocked: %s"
     (delib-flow--filing-loop-no-action-why run)
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
  (format "- Selected item: %s\n- Target count: %s\n- Capture template: %s\n- Save state: %s\n- Consequence pane: `E` draft, `I` support, `P` target, `V` staged"
          (delib-flow--compact-summary (delib-flow--active-filing-item-text run))
          (delib-flow--filing-target-count-text run)
          (delib-flow--current-filing-template-text run)
          (delib-flow--filing-save-state-text run)))

(defun delib-flow--filing-state-counts (run)
  "Return plist of filing-state counts for RUN."
  (list :draft (length (or (plist-get (plist-get run :filing) :draft-items) nil))
        :approved (length (or (plist-get (plist-get run :filing) :approved-items) nil))
        :rejected (length (or (plist-get (plist-get run :filing) :rejected-items) nil))))

(defun delib-flow--filing-state-count-summary (counts)
  "Return compact draft/approved/rejected summary from COUNTS."
  (format "%s draft, %s approved, %s rejected"
          (plist-get counts :draft)
          (plist-get counts :approved)
          (plist-get counts :rejected)))

(defconst delib-flow--filing-loop-stage-consequence-specs
  '((select-approved-filing-actions
     :consequence "one queue item was approved and its target preview was recalculated."
     :resume "review targets below, then file it or approve another item."
     :state counts)
    (reject-draft-filing-artifact
     :consequence "one queue item was removed from this run."
     :resume "choose another item or run another drafting pass."
     :state counts)
    (file-approved-outputs
     :consequence "target buffers were staged and opened, but not saved."
     :resume "inspect the staged targets, save if correct, or continue drafting."
     :state approved-only)
    (resolve-filing-conflict
     :consequence "conflict handling changed the approved item or retry path."
     :resume "retry filing or review the updated approved item.")
    (extract-actions
     :consequence "this stage refreshed the filing queue."
     :resume "choose one ready item, approve it, then review where it will go."
     :state counts)
    (extract-waiting-for
     :consequence "this stage refreshed the filing queue."
     :resume "choose one ready item, approve it, then review where it will go."
     :state counts)
    (suggest-reference-notes
     :consequence "this stage refreshed the filing queue."
     :resume "choose one ready item, approve it, then review where it will go."
     :state counts)
    (propose-new-project
     :consequence "this stage refreshed the filing queue."
     :resume "choose one ready item, approve it, then review where it will go."
     :state counts))
  "Stage-specific local consequence text for the filing loop.")

(defun delib-flow--filing-loop-stage-spec (stage-id)
  "Return filing-loop consequence spec for STAGE-ID."
  (cdr (assq stage-id delib-flow--filing-loop-stage-consequence-specs)))

(defun delib-flow--filing-loop-state-line (run state-key)
  "Return filing-state summary line for RUN using STATE-KEY."
  (let ((counts (delib-flow--filing-state-counts run)))
    (pcase state-key
      ('counts
       (format "- Filing state: %s."
               (delib-flow--filing-state-count-summary counts)))
      ('approved-only
       (format "- Filing state: %s approved artifact(s) still tracked in this run."
               (plist-get counts :approved)))
      (_ nil))))

(defun delib-flow--filing-loop-update-text (run)
  "Return compact local consequence text for the filing loop in RUN."
  (if-let* ((entry (delib-flow--latest-stage-entry run))
            (stage-id (plist-get entry :stage-id))
            (spec (delib-flow--filing-loop-stage-spec stage-id)))
      (string-join
       (delq nil
             (list
              (format "- Local consequence: %s" (plist-get spec :consequence))
              (delib-flow--filing-loop-state-line run (plist-get spec :state))
              (format "- Resume here: %s" (plist-get spec :resume))))
       "\n")
    (if entry
        (format "- Local consequence: %s\n- Resume here: %s"
                (delib-flow--compact-summary
                 (delib-flow--latest-consequence-text run)
                 (+ 16 (delib-flow--mobile-summary-limit)))
                (delib-flow--active-loop-location-text run))
      "- Local consequence: filing has not started yet.\n- Resume here: run a drafting pass to create a queue of candidates.")))


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

(defconst delib-flow-consequence-preview-buffer-name "*delib-flow-consequence*"
  "Name of the unified consequence preview buffer.")

(defvar delib-flow-staged-content-preview-buffer-name
  delib-flow-consequence-preview-buffer-name
  "Backward-compatible alias for the consequence preview buffer name.")

(defun delib-flow--active-filing-family (run)
  "Return the artifact family for the current filing item in RUN."
  (when-let ((item (delib-flow--active-filing-item run)))
    (delib-flow--artifact-family-for-item-kind (plist-get item :kind))))

(defconst delib-flow--consequence-preview-order
  '(draft support target staged)
  "Display order for consequence preview modes.")

(defconst delib-flow--consequence-preview-specs
  '((draft
     :title "Delib-Flow selected draft"
     :label "selected draft"
     :key "E"
     :status delib-flow--consequence-preview-draft-status
     :text delib-flow--consequence-preview-draft-text
     :available delib-flow--consequence-preview-draft-available-p)
    (support
     :title "Delib-Flow support excerpt"
     :label "support excerpt"
     :key "I"
     :status delib-flow--consequence-preview-support-status
     :text delib-flow--consequence-preview-support-text
     :available delib-flow--consequence-preview-support-available-p)
    (target
     :title "Delib-Flow filing target"
     :label "planned target"
     :key "P"
     :status delib-flow--consequence-preview-target-status
     :text delib-flow--consequence-preview-target-text
     :available delib-flow--consequence-preview-target-available-p)
    (staged
     :title "Delib-Flow staged output"
     :label "exact staged output"
     :key "V"
     :status delib-flow--consequence-preview-staged-status
     :text delib-flow--consequence-preview-staged-text
     :available delib-flow--consequence-preview-staged-available-p))
  "Plist specs for consequence preview kinds.")

(defun delib-flow--consequence-preview-spec (kind)
  "Return preview spec plist for KIND."
  (cdr (assq kind delib-flow--consequence-preview-specs)))

(defun delib-flow--consequence-preview-spec-value (kind key)
  "Return preview spec KEY value for KIND."
  (plist-get (delib-flow--consequence-preview-spec kind) key))

(defun delib-flow--consequence-preview-mode-entry (current kind)
  "Return mode-switch entry for KIND, highlighting CURRENT."
  (let ((key (delib-flow--consequence-preview-spec-value kind :key))
        (label (symbol-name kind)))
    (format "%s %s"
            (if (eq current kind)
                (format "[%s]" key)
              (format "`%s`" key))
            label)))

(defun delib-flow--consequence-preview-mode-line (kind)
  "Return compact mode-switch help for consequence preview KIND."
  (format "- Consequence modes: %s\n- Current mode: %s"
          (string-join
           (mapcar (lambda (entry)
                     (delib-flow--consequence-preview-mode-entry kind entry))
                   delib-flow--consequence-preview-order)
           ", ")
          (or (delib-flow--consequence-preview-spec-value kind :label)
              "unknown")))


(defun delib-flow--consequence-preview-draft-status (run)
  "Return draft preview status text for RUN."
  (delib-flow--current-filing-choice-status run))

(defun delib-flow--consequence-preview-draft-text (run)
  "Return draft preview detail text for RUN."
  (let ((base (delib-flow--current-filing-choice-text run)))
    (if-let ((family (delib-flow--active-filing-family run)))
        (concat
         base
         "\n\n** Draft history and compare\n"
         (delib-flow--family-draft-history-text run family)
         "\n\n** Evidence and quality review\n"
         (delib-flow--family-evidence-review-text run family))
      base)))

(defun delib-flow--consequence-preview-support-status (run)
  "Return support preview status text for RUN."
  (if-let ((family (delib-flow--active-filing-family run)))
      (delib-flow--family-support-status run family)
    "No selected artifact is active for support preview."))

(defun delib-flow--consequence-preview-support-text (run)
  "Return support preview detail text for RUN."
  (if-let ((family (delib-flow--active-filing-family run)))
      (delib-flow--family-support-text run family)
    "No focused support material is available."))

(defun delib-flow--consequence-preview-target-status (run)
  "Return target preview status text for RUN."
  (if-let ((family (delib-flow--active-filing-family run)))
      (delib-flow--family-workspace-target-status run family)
    (if (or (plist-get (plist-get run :filing) :target-locations)
            (delib-flow--planned-file-locations run))
        "Planned or staged filing targets are available."
      "No filing target is available to preview.")))

(defun delib-flow--consequence-preview-target-text (run)
  "Return target preview detail text for RUN."
  (let ((locations (or (plist-get (plist-get run :filing) :target-locations)
                       (delib-flow--planned-file-locations run))))
    (if locations
        (concat
         (mapconcat #'delib-flow--planned-file-location-line locations "\n")
         (if (plist-get (plist-get run :filing) :target-locations)
             "\n\n- State: staged in target buffers only; not saved to disk yet."
           "\n\n- State: planned target only; no staged buffer exists yet."))
      "No filing target is available to preview.")))

(defun delib-flow--consequence-preview-staged-status (run)
  "Return staged preview status text for RUN."
  (delib-flow--staged-content-preview-status run))

(defun delib-flow--consequence-preview-staged-text (run)
  "Return staged preview detail text for RUN."
  (delib-flow--staged-content-preview-text run))

(defun delib-flow--consequence-preview-draft-available-p (run)
  "Return non-nil when draft consequence preview is available for RUN."
  (not (string-prefix-p "Choose one ready queue item"
                        (delib-flow--consequence-preview-draft-text run))))

(defun delib-flow--consequence-preview-support-available-p (run)
  "Return non-nil when support consequence preview is available for RUN."
  (not (or (string-prefix-p "No focused support material is attached yet."
                            (delib-flow--consequence-preview-support-text run))
           (string-prefix-p "No focused support material is available."
                            (delib-flow--consequence-preview-support-text run)))))

(defun delib-flow--consequence-preview-target-available-p (run)
  "Return non-nil when target consequence preview is available for RUN."
  (or (plist-get (plist-get run :filing) :target-locations)
      (delib-flow--planned-file-locations run)))

(defun delib-flow--consequence-preview-staged-available-p (run)
  "Return non-nil when staged consequence preview is available for RUN."
  (delib-flow--staged-content-preview-text-available-p run))

(defun delib-flow--consequence-preview-available-p (run kind)
  "Return non-nil when consequence preview KIND is available for RUN."
  (when-let ((available-fn
              (delib-flow--consequence-preview-spec-value kind :available)))
    (funcall available-fn run)))


(defun delib-flow--default-consequence-preview-kind (run)
  "Return the best default consequence preview kind for RUN."
  (or (seq-find (lambda (kind)
                  (delib-flow--consequence-preview-available-p run kind))
                delib-flow--consequence-preview-order)
      'draft))


(defun delib-flow--effective-consequence-preview-kind (run)
  "Return the effective consequence preview kind for RUN."
  (let ((stored (delib-flow--consequence-preview-kind run)))
    (if (delib-flow--consequence-preview-available-p run stored)
        stored
      (delib-flow--default-consequence-preview-kind run))))

(defun delib-flow--consequence-preview-title (kind)
  "Return Org heading title for consequence preview KIND."
  (or (delib-flow--consequence-preview-spec-value kind :title)
      "Delib-Flow consequence preview"))

(defun delib-flow--consequence-preview-component (run kind key fallback)
  "Return preview KEY component for KIND in RUN, or FALLBACK."
  (if-let ((fn (delib-flow--consequence-preview-spec-value kind key)))
      (funcall fn run)
    fallback))

(defun delib-flow--consequence-preview-status (run kind)
  "Return status text for consequence preview KIND in RUN."
  (delib-flow--consequence-preview-component
   run kind :status "No consequence preview is available."))

(defun delib-flow--consequence-preview-text (run kind)
  "Return detail text for consequence preview KIND in RUN."
  (delib-flow--consequence-preview-component
   run kind :text "No consequence preview is available."))


(defun delib-flow--populate-consequence-preview-buffer (buffer run)
  "Populate consequence preview BUFFER from RUN and return BUFFER."
  (let* ((kind (delib-flow--effective-consequence-preview-kind run))
         (status (delib-flow--consequence-preview-status run kind))
         (text (delib-flow--consequence-preview-text run kind)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (setq-local delib-flow--consequence-preview-kind kind)
        (erase-buffer)
        (insert (format "* %s\n\n" (delib-flow--consequence-preview-title kind)))
        (insert (format "%s\n\n%s\n\n%s"
                        (delib-flow--consequence-preview-mode-line kind)
                        status
                        text))
        (goto-char (point-min))
        (org-mode)
        (setq-local truncate-lines nil)
        (visual-line-mode 1)
        (org-fold-show-all)
        (view-mode 1))))
  buffer)

(defun delib-flow--refresh-consequence-preview-buffer (&optional run)
  "Refresh the consequence preview buffer from RUN when it is visible."
  (when-let ((buffer (get-buffer delib-flow-consequence-preview-buffer-name)))
    (let* ((current-run (or run delib-flow--active-run))
           (kind (with-current-buffer buffer
                   delib-flow--consequence-preview-kind)))
      (when current-run
        (delib-flow--populate-consequence-preview-buffer
         buffer
         (if kind
             (delib-flow--set-consequence-preview-kind current-run kind)
           current-run))))))

(defun delib-flow--show-consequence-preview-buffer (run kind)
  "Display consequence preview KIND for RUN and return the buffer."
  (unless (delib-flow--consequence-preview-available-p run kind)
    (user-error "%s" (delib-flow--consequence-preview-text run kind)))
  (setq run (delib-flow--set-consequence-preview-kind run kind))
  (let ((buffer
         (delib-flow--populate-consequence-preview-buffer
          (get-buffer-create delib-flow-consequence-preview-buffer-name)
          run)))
    (display-buffer-in-side-window
     buffer
     '((side . right)
       (slot . 1)
       (window-width . 0.45)))
    buffer))

(defun delib-flow--refresh-staged-content-preview-buffer (&optional run)
  "Refresh the consequence preview buffer from RUN when it is visible."
  (delib-flow--refresh-consequence-preview-buffer run))

(defun delib-flow--show-staged-content-preview-buffer (run)
  "Display staged filing content preview for RUN and return the buffer."
  (delib-flow--show-consequence-preview-buffer run 'staged))

(defun delib-flow--show-filed-target-locations (run)
  "Display filed target locations for RUN and announce them."
  (when-let ((locations (plist-get (plist-get run :filing) :target-locations)))
    (delib-flow--display-filed-target-location (car locations))
    (message "Staged filing previews (not saved):\n%s"
             (mapconcat #'delib-flow--normalize-file-target-location
                        locations
                        "\n"))))

(defun delib-flow-peek-selected-draft ()
  "Open the selected draft in the unified consequence pane."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--set-consequence-preview-kind delib-flow--active-run 'draft))
  (delib-flow--show-consequence-preview-buffer delib-flow--active-run 'draft))

(defun delib-flow-peek-selected-support ()
  "Open focused support for the selected artifact in the consequence pane."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--set-consequence-preview-kind delib-flow--active-run 'support))
  (delib-flow--show-consequence-preview-buffer delib-flow--active-run 'support))

(defun delib-flow-peek-filing-target ()
  "Open the planned filing target in the unified consequence pane."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--set-consequence-preview-kind delib-flow--active-run 'target))
  (delib-flow--show-consequence-preview-buffer delib-flow--active-run 'target))

(defun delib-flow-peek-staged-content ()
  "Open the exact staged filing content in the consequence pane."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--set-consequence-preview-kind delib-flow--active-run 'staged))
  (delib-flow--show-consequence-preview-buffer delib-flow--active-run 'staged))

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

(defun delib-flow--family-workspace-target-status (run family)
  "Return target/consequence status text for artifact FAMILY in RUN."
  (let ((active (delib-flow--active-filing-item run)))
    (if (eq (delib-flow--artifact-family-for-item-kind
             (plist-get active :kind))
            family)
        (delib-flow--planned-file-location-status run)
      "This family does not currently own the active filing target. The latest local consequence is shown below.")))

(defun delib-flow--family-workspace-target-text (run family)
  "Return target/consequence detail text for artifact FAMILY in RUN."
  (let ((active (delib-flow--active-filing-item run)))
    (if (eq (delib-flow--artifact-family-for-item-kind
             (plist-get active :kind))
            family)
        (delib-flow--planned-file-location-text run)
      (delib-flow--filing-loop-update-text run))))

(delib-flow--define-function delib-flow--family-local-action-ids
			     (family)
			     "Return the preferred local action ids for artifact FAMILY."
			     (append
			      (pcase family
				('actions
				 '(draft-selected-action
				   find-support-for-selected-action
				   choose-saved-selected-action-draft
				   restore-previous-selected-action-draft))
				('waiting-fors
				 '(draft-selected-waiting-for
				   find-support-for-selected-waiting-for
				   choose-saved-selected-waiting-for-draft
				   restore-previous-selected-waiting-for-draft))
				('reference-notes
				 '(draft-selected-reference-note
				   find-support-for-selected-reference-note
                                   choose-support-for-selected-reference-note
                                   clear-selected-reference-note-support
                                   edit-selected-reference-note-title
                                   choose-reference-note-template
                                   edit-selected-reference-note-target-path
                                   edit-selected-reference-note-draft-body
                                   refresh-selected-reference-note-draft-body
                                   edit-selected-reference-note-source-highlights
                                   refresh-selected-reference-note-source-highlights
                                   edit-selected-reference-note-related-material
                                   refresh-selected-reference-note-related-material
                                   edit-selected-reference-note-reuse-angle
                                   refresh-selected-reference-note-reuse-angle
                                   choose-saved-selected-reference-note-draft
                                   restore-previous-selected-reference-note-draft))
				('project-proposals
				 '(extract-actions
                                   extract-waiting-for
                                   suggest-reference-notes
                                   edit-operator-intent
                                   draft-selected-project
				   find-support-for-selected-project
				   choose-saved-selected-project-draft
				   restore-previous-selected-project-draft))
				(_ nil))
			      '(select-approved-filing-actions
				reject-draft-filing-artifact
				file-approved-outputs
				resolve-filing-conflict)))


(delib-flow--define-function
 delib-flow--family-local-action-palette-heading (family)
 "Return the unique local action palette heading for FAMILY."
 (pcase family
   ('actions "Local action palette: action")
   ('waiting-fors "Local action palette: waiting-for")
   ('reference-notes "Local action palette: note")
   ('project-proposals "Local action palette: project")
   (_ "Local action palette")))


(defun delib-flow--family-local-actions (run family)
  "Return the compact selected-item local actions for FAMILY in RUN."
  (let ((preferred (delib-flow--family-local-action-ids family)))
    (sort
     (seq-filter
      (lambda (action)
        (memq (plist-get action :id) preferred))
      (delib-flow--filing-preview-actions run))
     (lambda (left right)
       (< (or (seq-position preferred (plist-get left :id)) 999)
          (or (seq-position preferred (plist-get right :id)) 999))))))

(defun delib-flow--family-local-action-palette-status (run family)
  "Return status text for the selected-item local action palette."
  (if-let ((actions (delib-flow--family-local-actions run family)))
      (format "%d item-local action%s are ready here. Activate a line with `RET`, or press `.` at point for the compact workspace menu."
              (length actions)
              (if (= (length actions) 1) "" "s"))
    "No item-local actions are available in this workspace yet. Use the broader filing controls below if you need to change the loop."))

(defun delib-flow--family-local-action-palette-text (run family)
  "Return rendered local action palette lines for FAMILY in RUN."
  (if-let ((actions (delib-flow--family-local-actions run family)))
      (delib-flow--render-action-lines actions)
    "No item-local actions are available yet."))

(defun delib-flow--workspace-subsection (heading status text)
  "Return a workspace subsection with HEADING, STATUS, and TEXT."
  (format "**** %s\n%s\n%s" heading status text))

(defun delib-flow--family-workspace-section
    (run title family workspace-status-fn selected-status-fn selected-text-fn
         draft-status-fn draft-text-fn next-status-fn next-text-fn)
  "Return selected-item workspace section text for artifact FAMILY in RUN."
(string-join
   (list
    (format "*** %s workspace\n%s" title (funcall workspace-status-fn run))
    (delib-flow--workspace-subsection
     "Shortlist"
     (delib-flow--filing-family-shortlist-status run family)
     (delib-flow--filing-family-shortlist-text run family))
    (delib-flow--workspace-subsection
     "Selected item"
     (funcall selected-status-fn run)
     (funcall selected-text-fn run))
    (delib-flow--workspace-subsection
     "Drafted content"
     (funcall draft-status-fn run)
     (funcall draft-text-fn run))
    (delib-flow--workspace-subsection
     "Draft history and restore"
     (delib-flow--family-draft-history-status run family)
     (delib-flow--family-draft-history-text run family))
    (delib-flow--workspace-subsection
     "Supporting material"
     (delib-flow--family-support-status run family)
     (delib-flow--family-support-text run family))
    (delib-flow--workspace-subsection
     "Evidence and quality review"
     (delib-flow--family-evidence-review-status run family)
     (delib-flow--family-evidence-review-text run family))
    (delib-flow--workspace-subsection
     (delib-flow--family-local-action-palette-heading family)
     (delib-flow--family-local-action-palette-status run family)
     (delib-flow--family-local-action-palette-text run family))
    (delib-flow--workspace-subsection
     "Next local actions"
     (funcall next-status-fn run)
     (funcall next-text-fn run))
    (delib-flow--workspace-subsection
     "Planned target or consequence"
     (delib-flow--family-workspace-target-status run family)
     (delib-flow--family-workspace-target-text run family)))
   "\n\n"))

(defconst delib-flow--workspace-family-specs
  '((project-proposals
     :title "Selected project"
     :active delib-flow--project-flow-active-p
     :workspace-status delib-flow--project-workflow-status
     :selected-status delib-flow--selected-project-workspace-status
     :selected-text delib-flow--selected-project-workspace-text
     :draft-status delib-flow--project-draft-preview-status
     :draft-text delib-flow--project-draft-preview-text
     :next-status delib-flow--project-regeneration-status
     :next-text delib-flow--project-regeneration-text)
    (actions
     :title "Selected action"
     :active delib-flow--action-flow-active-p
     :workspace-status delib-flow--action-workflow-status
     :selected-status delib-flow--selected-action-workspace-status
     :selected-text delib-flow--selected-action-workspace-text
     :draft-status delib-flow--action-draft-preview-status
     :draft-text delib-flow--action-draft-preview-text
     :next-status delib-flow--action-regeneration-status
     :next-text delib-flow--action-regeneration-text)
    (waiting-fors
     :title "Selected waiting-for"
     :active delib-flow--waiting-for-flow-active-p
     :workspace-status delib-flow--waiting-for-workflow-status
     :selected-status delib-flow--selected-waiting-for-workspace-status
     :selected-text delib-flow--selected-waiting-for-workspace-text
     :draft-status delib-flow--waiting-for-draft-preview-status
     :draft-text delib-flow--waiting-for-draft-preview-text
     :next-status delib-flow--waiting-for-regeneration-status
     :next-text delib-flow--waiting-for-regeneration-text)
    (reference-notes
     :title "Selected note"
     :active delib-flow--reference-note-workspace-active-p
     :workspace-status delib-flow--reference-note-draft-preview-status
     :selected-status delib-flow--selected-note-workspace-status
     :selected-text delib-flow--selected-note-workspace-text
     :draft-status delib-flow--reference-note-draft-preview-status
     :draft-text delib-flow--reference-note-draft-preview-text
     :next-status delib-flow--reference-note-regeneration-status
     :next-text delib-flow--reference-note-regeneration-text))
  "Specs for selected-item workspace sections.")

(defun delib-flow--reference-note-workspace-active-p (run)
  "Return non-nil when the selected note workspace should render for RUN."
  (or (delib-flow--reference-note-draft-preview-items run)
      (delib-flow--artifact-family-selected-candidate-id run 'reference-notes)))

(defun delib-flow--workspace-family-spec (family)
  "Return workspace spec plist for FAMILY."
  (cdr (assq family delib-flow--workspace-family-specs)))

(defun delib-flow--workspace-family-section (run family)
  "Return selected workspace section for FAMILY in RUN."
  (when-let* ((spec (delib-flow--workspace-family-spec family))
              (active-fn (plist-get spec :active)))
    (when (funcall active-fn run)
      (delib-flow--family-workspace-section
       run
       (plist-get spec :title)
       family
       (plist-get spec :workspace-status)
       (plist-get spec :selected-status)
       (plist-get spec :selected-text)
       (plist-get spec :draft-status)
       (plist-get spec :draft-text)
       (plist-get spec :next-status)
       (plist-get spec :next-text)))))

(defun delib-flow--filing-preview-workspace-sections (run)
  "Return selected-item workspace sections for Filing preview in RUN."
  (delq nil
        (mapcar (lambda (family)
                  (delib-flow--workspace-family-section run family))
                '(project-proposals actions waiting-fors reference-notes))))

(defun delib-flow--reference-note-capture-section (run)
  "Return the reference-note capture section for RUN, or nil."
  (when (delib-flow--reference-note-capture-visible-p run)
    (format "*** Reference note capture\n%s\n%s"
            (delib-flow--reference-note-capture-status run)
            (delib-flow--render-editable-block run 'reference-note-capture-review))))

(defun delib-flow--reference-note-target-summary-status (run)
  "Return status text for the selected note target summary in RUN."
  (if (delib-flow--reference-note-preview-item run)
      "These settings control how the selected note will be saved when you approve and save it."
    "No selected note is active for target editing."))

(defun delib-flow--reference-note-target-summary-text (run)
  "Return target summary text for the selected note in RUN."
  (if-let* ((package (delib-flow--stage-input-package run 'file-approved-outputs))
            (item (delib-flow--reference-note-preview-item package)))
      (string-join
       (list
        (format "- Saved note title: %s"
                (or (delib-flow--reference-note-effective-title item package)
                    "Untitled note"))
        (format "- Template key: %s"
                (or (delib-flow--reference-note-effective-template-key item package)
                    "unconfigured"))
        (format "- Target path override: %s"
                (or (delib-flow--reference-note-effective-target-override package)
                    "default org-roam target"))
        "- Local actions: `Edit Note Title`, `Change Reference Note Template`, `Edit Target Path`.")
       "\n")
    "No selected note is active for target editing."))

(defconst delib-flow--reference-note-workspace-action-order
  '(draft-selected-reference-note
    edit-selected-reference-note-title
    edit-selected-reference-note-draft-body
    refresh-selected-reference-note-draft-body
    edit-selected-reference-note-source-highlights
    refresh-selected-reference-note-source-highlights
    find-support-for-selected-reference-note
    choose-support-for-selected-reference-note
    clear-selected-reference-note-support
    edit-selected-reference-note-related-material
    refresh-selected-reference-note-related-material
    edit-selected-reference-note-reuse-angle
    refresh-selected-reference-note-reuse-angle
    choose-saved-selected-reference-note-draft
    restore-previous-selected-reference-note-draft
    choose-reference-note-template
    edit-selected-reference-note-target-path
    select-approved-filing-actions
    file-approved-outputs
    reject-draft-filing-artifact
    resolve-filing-conflict)
  "Display order for focused filing-workspace note actions.")

(defconst delib-flow--reference-note-workspace-action-builders
  '(delib-flow--draft-selected-reference-note-action
    delib-flow--edit-selected-reference-note-title-action
    delib-flow--edit-selected-reference-note-draft-body-action
    delib-flow--refresh-selected-reference-note-draft-body-action
    delib-flow--edit-selected-reference-note-source-highlights-action
    delib-flow--refresh-selected-reference-note-source-highlights-action
    delib-flow--find-support-for-selected-reference-note-action
    delib-flow--choose-support-for-selected-reference-note-action
    delib-flow--clear-selected-reference-note-support-action
    delib-flow--edit-selected-reference-note-related-material-action
    delib-flow--refresh-selected-reference-note-related-material-action
    delib-flow--edit-selected-reference-note-reuse-angle-action
    delib-flow--refresh-selected-reference-note-reuse-angle-action
    delib-flow--choose-saved-selected-reference-note-draft-action
    delib-flow--restore-previous-selected-reference-note-draft-action
    delib-flow--choose-reference-note-template-action
    delib-flow--edit-selected-reference-note-target-path-action
    delib-flow--select-approved-filing-actions-action
    delib-flow--file-approved-outputs-action
    delib-flow--reject-draft-filing-artifact-action
    delib-flow--resolve-filing-conflict-action)
  "Action-builder functions for the focused filing-workspace note loop.")

(defun delib-flow--reference-note-workspace-item (run)
  "Return the active reference-note artifact for local editing in RUN."
  (or (delib-flow--reference-note-preview-item run)
      (delib-flow--selected-reference-note-candidate-for-drafting run)
      (delib-flow--reference-note-selected-draft run)))

(defun delib-flow--focused-reference-note-workspace-actions (run)
  "Return filing-workspace actions for the selected note in RUN.

These actions are reassigned local shortcuts so the focused filing workspace
uses a compact, coherent action surface instead of inheriting the cockpit's
broader filing numbering."
  (let ((keys delib-flow--action-shortcut-keys))
    (mapcar
     (lambda (action)
       (prog1 (plist-put action :shortcut (car keys))
         (setq keys (cdr keys))))
     (delib-flow--sort-actions-by-id-order
      (copy-tree
       (seq-remove
        #'null
        (mapcar (lambda (builder)
                  (funcall builder run))
                delib-flow--reference-note-workspace-action-builders)))
      delib-flow--reference-note-workspace-action-order))))

(defun delib-flow--selected-note-focused-filing-preview-p (run)
  "Return non-nil when RUN should use the note-focused filing preview."
  (and (delib-flow--reference-note-preview-item run)
       (eq (delib-flow--active-filing-family run) 'reference-notes)))

(defun delib-flow--reference-note-workspace-actions (run ids)
  "Return filing-workspace actions for RUN matching IDS."
  (seq-filter
   (lambda (action)
     (memq (plist-get action :id) ids))
   (delib-flow--focused-reference-note-workspace-actions run)))

(defun delib-flow--reference-note-workspace-action-status (run ids empty-text)
  "Return compact action status for RUN matching IDS or EMPTY-TEXT."
  (if-let ((actions (delib-flow--reference-note-workspace-actions run ids)))
      (if (= (length actions) 1)
          "Do this here:"
        "Do one of these here:")
    empty-text))

(defun delib-flow--reference-note-workspace-action-text (run ids empty-text)
  "Return rendered action text for RUN matching IDS or EMPTY-TEXT."
  (if-let ((actions (delib-flow--reference-note-workspace-actions run ids)))
      (delib-flow--render-reference-note-workspace-action-lines actions)
    empty-text))

(defun delib-flow--reference-note-workspace-action-label (action)
  "Return filing-workspace label for reference-note ACTION."
  (or (alist-get
       (plist-get action :id)
       '((draft-selected-reference-note . "Regenerate Selected Note")
         (refresh-selected-reference-note-draft-body
          . "Regenerate Draft Body")
         (refresh-selected-reference-note-source-highlights
          . "Regenerate Source Highlights")
         (refresh-selected-reference-note-related-material
          . "Regenerate Related Material")
         (refresh-selected-reference-note-reuse-angle
          . "Regenerate Reuse Angle")))
      (plist-get action :label)))

(defun delib-flow--format-reference-note-workspace-action-line (action)
  "Return filing-workspace compact display line for reference-note ACTION."
  (format "- [%s] %s"
          (or (plist-get action :shortcut) "?")
          (delib-flow--reference-note-workspace-action-label action)))

(defun delib-flow--render-reference-note-workspace-action-lines (actions)
  "Return compact filing-workspace lines for reference-note ACTIONS."
  (mapconcat #'delib-flow--format-reference-note-workspace-action-line
             actions
             "\n"))

(defun delib-flow--format-focused-workspace-action-line (action)
  "Return compact focused-workspace display line for ACTION."
  (format "- [%s] %s"
          (or (plist-get action :shortcut) "?")
          (plist-get action :label)))

(defun delib-flow--render-focused-workspace-action-lines (actions)
  "Return compact focused-workspace lines for ACTIONS."
  (mapconcat #'delib-flow--format-focused-workspace-action-line
             actions
             "\n"))

(defconst delib-flow--project-workspace-action-order
  '(extract-actions
    extract-waiting-for
    suggest-reference-notes
    draft-selected-action
    draft-selected-waiting-for
    draft-selected-reference-note
    edit-operator-intent
    draft-selected-project
    select-approved-filing-actions
    file-approved-outputs
    reject-draft-filing-artifact
    resolve-filing-conflict)
  "Display order for focused filing-workspace project actions.")

(defconst delib-flow--project-workspace-action-builders
  '(delib-flow--extract-actions-action
    delib-flow--extract-waiting-for-action
    delib-flow--suggest-reference-notes-action
    delib-flow--draft-selected-action-action
    delib-flow--draft-selected-waiting-for-action
    delib-flow--draft-selected-reference-note-action
    delib-flow--edit-operator-intent-action
    delib-flow--draft-selected-project-action
    delib-flow--select-approved-filing-actions-action
    delib-flow--file-approved-outputs-action
    delib-flow--reject-draft-filing-artifact-action
    delib-flow--resolve-filing-conflict-action)
  "Action-builder functions for the focused filing-workspace project loop.")

(defun delib-flow--focused-project-workspace-actions (run)
  "Return filing-workspace actions for the selected project package in RUN."
  (let ((keys delib-flow--action-shortcut-keys))
    (mapcar
     (lambda (action)
       (prog1 (plist-put action :shortcut (car keys))
         (setq keys (cdr keys))))
     (let* ((actions
             (delib-flow--sort-actions-by-id-order
              (copy-tree
               (seq-remove
                #'null
                (mapcar (lambda (builder)
                          (funcall builder run))
                        delib-flow--project-workspace-action-builders)))
              delib-flow--project-workspace-action-order))
            (visible-ids
             (delib-flow--project-workspace-visible-action-ids run)))
       (if visible-ids
           (delib-flow--sort-actions-by-id-order
            (seq-filter
             (lambda (action)
               (memq (plist-get action :id) visible-ids))
             actions)
            visible-ids)
         actions)))))

(defun delib-flow--project-workspace-action-text (run ids empty-text)
  "Return project workspace action text for RUN matching IDS or EMPTY-TEXT."
  (if-let ((actions
            (if ids
                (seq-filter
                 (lambda (action)
                   (memq (plist-get action :id) ids))
                 (delib-flow--focused-project-workspace-actions run))
              (delib-flow--focused-project-workspace-actions run))))
      (delib-flow--render-focused-workspace-action-lines actions)
    empty-text))

(defun delib-flow--project-workspace-visible-action-ids (run)
  "Return the project-workspace action ids that should stay primary for RUN."
  (pcase (delib-flow--project-workflow-current-step run)
    ('draft-project
     '(draft-selected-project
       edit-operator-intent
       reject-draft-filing-artifact))
    ('extract-work
     '(extract-actions
       extract-waiting-for
       suggest-reference-notes
       edit-operator-intent
       draft-selected-project
       select-approved-filing-actions
       file-approved-outputs
       reject-draft-filing-artifact
       resolve-filing-conflict))
    ('draft-items
     '(draft-selected-action
       draft-selected-waiting-for
       draft-selected-reference-note
       extract-actions
       extract-waiting-for
       suggest-reference-notes
       edit-operator-intent
       draft-selected-project
       select-approved-filing-actions
       file-approved-outputs
       reject-draft-filing-artifact
       resolve-filing-conflict))
    ('review-package
     '(edit-operator-intent
       draft-selected-project
       extract-actions
       extract-waiting-for
       suggest-reference-notes
       select-approved-filing-actions
       file-approved-outputs
       reject-draft-filing-artifact
       resolve-filing-conflict))
    (_
     '(draft-selected-project
       edit-operator-intent
       reject-draft-filing-artifact))))

(defun delib-flow--project-package-included-items (run)
  "Return the current included project package items for RUN."
  (let* ((project (delib-flow--project-package-root-for-filing run))
         (approved (plist-get (plist-get run :filing) :approved-items))
         (preview (and (delib-flow--filing-selection-active-p run)
                       (delib-flow--preview-selected-filing-items run))))
    (cond
     (approved
      (if project
          (delib-flow--project-package-expanded-items
           project
           (seq-remove (lambda (item)
                         (eq (plist-get item :kind) 'project))
                       approved))
        approved))
     ((and project preview)
      (delib-flow--project-package-expanded-items
       project
       (seq-reduce #'delib-flow--remove-first-matching-item
                   (list project)
                   preview)))
     (project
      (delib-flow--project-package-expanded-items project nil))
     (preview
      preview)
     (t nil))))

(defun delib-flow--project-package-outside-items (run)
  "Return draft items in RUN that are still outside the active package."
  (let ((included (delib-flow--project-package-included-items run))
        (drafts (plist-get (plist-get run :filing) :draft-items)))
    (if included
        (seq-reduce #'delib-flow--remove-first-matching-item
                    included
                    drafts)
      drafts)))

(defun delib-flow--project-package-consequence-text (run)
  "Return a compact filing consequence summary for the active project package in RUN."
  (if-let* ((project (delib-flow--project-package-root-for-filing run))
            (included (delib-flow--project-package-included-items run)))
      (let* ((children (seq-remove (lambda (item)
                                     (eq (plist-get item :kind) 'project))
                                   included))
             (outside (delib-flow--project-package-outside-items run)))
        (string-join
         (list
          (format "- Drafting now: %s"
                  (delib-flow--active-filing-item-text run))
          (format "- What files together now: project `%s` plus %s included child item%s"
                  (plist-get project :title)
                  (length children)
                  (if (= (length children) 1) "" "s"))
          (format "- Still outside the package: %s item%s"
                  (length outside)
                  (if (= (length outside) 1) "" "s"))
          (format "- File destination: `%s` > `%s`"
                  (file-name-nondirectory
                   (or delib-flow-my-projects-file "My Projects"))
                  (delib-flow--project-state-bucket-title
                   (or (plist-get project :state) 'active))))
         "\n"))
    "No selected project package is active."))

(defun delib-flow--project-package-summary-text (run)
  "Return compact summary text for the active project package in RUN."
  (let* ((project (delib-flow--project-package-root-for-filing run))
         (included (delib-flow--project-package-included-items run))
         (children (seq-remove (lambda (item)
                                 (eq (plist-get item :kind) 'project))
                               included)))
    (if project
        (string-join
         (list
          (format "- Project: %s" (plist-get project :title))
          (format "- State bucket: %s" (delib-flow--project-state-bucket-title
                                        (or (plist-get project :state) 'active)))
          (format "- Included child items: %s" (length children))
          (format "- Outside the package: %s"
                  (length (delib-flow--project-package-outside-items run)))
          (format "- Package readiness: %s"
                  (if (delib-flow--approved-project-package-p run)
                      "approved bundle ready to file"
                    "draft bundle still being assembled")))
         "\n")
      "No selected project package is active.")))

(defun delib-flow--project-package-items-text (run)
  "Return compact included-item text for the active project package in RUN."
  (if-let ((items (delib-flow--project-package-included-items run)))
      (delib-flow--draft-item-preview-text items)
    "No project package items are included yet."))

(defun delib-flow--project-package-other-candidates-text (run)
  "Return compact excluded-or-remaining item text for RUN."
  (let ((items (delib-flow--project-package-outside-items run)))
    (if items
        (delib-flow--draft-item-preview-text items)
      "No other draft items remain outside the current package.")))

(defun delib-flow--project-workspace-next-step-status (run)
  "Return compact top-of-workspace continuation guidance for RUN."
  (cond
   ((delib-flow--selected-project-drafted-p run)
    "The project package is drafted. Continue from the local numbered actions here instead of the broader cockpit flow.")
   ((delib-flow--selected-project-candidate-for-drafting run)
    "One project package is selected but not yet drafted. Draft it here before extraction, approval, or filing.")
   ((delib-flow--project-flow-active-p run)
    "A project package is active. Continue the local project loop here.")
   (t
    "No selected project package is active right now.")))

(defun delib-flow--render-selected-project-filing-workspace (run)
  "Return project-focused filing workspace text for RUN."
  (string-join
   (list
    (format "** Project package\n%s\n%s"
            (delib-flow--project-workflow-status run)
            (concat
             (delib-flow--project-package-summary-text run)
             "\n\n"
             (delib-flow--project-workflow-text run)))
    (format "** Do here now\n%s\n%s"
            (delib-flow--project-workspace-next-step-status run)
            (delib-flow--project-workspace-action-text
             run nil "No project-local actions are available yet."))
    (format "** Package consequence\n%s\n%s"
            "This tells you what is being drafted now, what is in the package, and what filing will do."
            (delib-flow--project-package-consequence-text run))
    (format "** Included items\n%s\n%s"
            "These items will stage together when you file the package."
            (delib-flow--project-package-items-text run))
    (format "** Extracted but not yet included\n%s\n%s"
            "These items are still outside the active package."
            (delib-flow--project-package-other-candidates-text run))
    (format "** Targets and staged output\n%s\n%s\n\n*** Do here: file package\n%s"
            (delib-flow--planned-file-location-status run)
            (delib-flow--planned-file-location-text run)
            (delib-flow--project-workspace-action-text
             run
             '(select-approved-filing-actions file-approved-outputs reject-draft-filing-artifact resolve-filing-conflict)
             "No package filing action is available yet."))
    (format "** Leave workspace\n- `B` return to cockpit\n- `F` reopen this workspace later\n- Preview: `E` draft, `I` support, `P` target, `V` staged\n- Save state: %s"
            (delib-flow--filing-save-state-text run)))
   "\n\n"))

(defun delib-flow--reference-note-selected-draft (run)
  "Return the selected reference-note draft from RUN, if any."
  (if-let ((draft (delib-flow--artifact-family-selected-draft run 'reference-notes)))
      (delib-flow--reference-note-draft-with-workspace-structure
       draft
       (or (delib-flow--selected-reference-note-candidate-for-drafting run)
           draft)
       run)
    (when-let ((candidate
                (delib-flow--selected-reference-note-candidate-for-drafting
                 run)))
      (when (eq (plist-get candidate :kind) 'reference-note)
        (delib-flow--drafted-reference-note-item
         candidate run nil
         "Seeded the selected note candidate into a visible working draft for local editing.")))))

(defun delib-flow--reference-note-draft-body-text (draft)
  "Return draft-body text for reference-note DRAFT."
  (or (plist-get draft :draft-body)
      ""))

(defun delib-flow--reference-note-draft-section-text (draft heading)
  "Return trimmed reference-note DRAFT section text for HEADING."
  (let ((draft-body (delib-flow--reference-note-draft-body-text draft)))
    (when (delib-flow--non-empty-string-p draft-body)
      (with-temp-buffer
        (insert draft-body)
        (goto-char (point-min))
        (when (re-search-forward
               (format "^\\* %s$" (regexp-quote heading))
               nil t)
          (forward-line 1)
          (let ((start (point))
                (end (or (and (re-search-forward "^\\* " nil t)
                              (line-beginning-position))
                         (point-max))))
            (string-trim-right
             (buffer-substring-no-properties start end))))))))

(defun delib-flow--reference-note-draft-working-body-text (draft)
  "Return the working-draft body text for reference-note DRAFT."
  (let ((section (delib-flow--reference-note-draft-section-text draft "Working draft")))
    (if section
        (string-trim
         (replace-regexp-in-string
          "^- Reuse angle:.*$" ""
          section))
      "No working draft body is available yet. Draft the selected note to create one.")))

(defun delib-flow--reference-note-draft-source-highlights-text (draft)
  "Return source-highlights text for reference-note DRAFT."
  (or (delib-flow--reference-note-draft-section-text draft "Source highlights")
      "No source highlights are available yet. Regenerate this part to pull grounded excerpts from the source."))

(defun delib-flow--reference-note-draft-related-material-text (draft)
  "Return related-material text for reference-note DRAFT."
  (or (delib-flow--reference-note-draft-section-text draft "Related material to connect")
      "No related material is attached yet. Choose support explicitly before regenerating this part."))

(defun delib-flow--reference-note-draft-reuse-angle-text (draft)
  "Return reuse-angle text for reference-note DRAFT."
  (let ((draft-body (delib-flow--reference-note-draft-body-text draft)))
    (if (string-match "^- Reuse angle: .*" draft-body)
        (match-string 0 draft-body)
      "No reuse angle is recorded yet. Regenerate this part to restate the note's reusable purpose.")))

(defun delib-flow--selected-reference-note-draft-working-body-text (run)
  "Return working-draft body text for the selected note in RUN."
  (delib-flow--reference-note-draft-working-body-text
   (delib-flow--reference-note-selected-draft run)))

(defun delib-flow--selected-reference-note-draft-source-highlights-text (run)
  "Return source-highlights text for the selected note in RUN."
  (delib-flow--reference-note-draft-source-highlights-text
   (delib-flow--reference-note-selected-draft run)))

(defun delib-flow--selected-reference-note-draft-related-material-text (run)
  "Return related-material text for the selected note in RUN."
  (delib-flow--reference-note-draft-related-material-text
   (delib-flow--reference-note-selected-draft run)))

(defun delib-flow--selected-reference-note-draft-reuse-angle-text (run)
  "Return reuse-angle text for the selected note in RUN."
  (delib-flow--reference-note-draft-reuse-angle-text
   (delib-flow--reference-note-selected-draft run)))

(defun delib-flow--reference-note-workspace-shortcut (run action-id)
  "Return the local filing-workspace shortcut for ACTION-ID in RUN, if any."
  (when-let ((action
              (seq-find
               (lambda (candidate)
                 (eq (plist-get candidate :id) action-id))
               (delib-flow--focused-reference-note-workspace-actions run))))
    (plist-get action :shortcut)))

(defun delib-flow--reference-note-workspace-outline-line (run label action-ids)
  "Return a compact building-block line for LABEL using ACTION-IDS in RUN."
  (let ((shortcuts
         (delq nil
               (mapcar (lambda (id)
                         (when-let ((shortcut
                                     (delib-flow--reference-note-workspace-shortcut
                                      run id)))
                           (format "`%s`" shortcut)))
                       action-ids))))
    (format "- %s: %s"
            label
            (if shortcuts
                (string-join shortcuts " / ")
              "no local control yet"))))

(defun delib-flow--reference-note-workspace-title-state (_run)
  "Return compact state badge for the selected note title."
  "manual")

(defun delib-flow--reference-note-workspace-draft-body-state (run)
  "Return compact state badge for selected note draft body in RUN."
  (if-let ((draft (delib-flow--reference-note-selected-draft run)))
      (if (string-prefix-p
           "Seeded"
           (or (plist-get draft :draft-reason) ""))
          "seeded"
        "generated")
    "missing"))

(defun delib-flow--reference-note-workspace-section-state (run heading)
  "Return compact state badge for selected note section HEADING in RUN."
  (if (delib-flow--reference-note-draft-section-text
       (delib-flow--reference-note-selected-draft run)
       heading)
      "present"
    "missing"))

(defun delib-flow--reference-note-workspace-reuse-angle-state (run)
  "Return compact state badge for selected note reuse angle in RUN."
  (if (string-match
       "^- Reuse angle: .*"
       (delib-flow--reference-note-draft-body-text
        (delib-flow--reference-note-selected-draft run)))
      "present"
    "missing"))

(defun delib-flow--reference-note-workspace-outline-text (run)
  "Return a compact note-part control outline for the filing workspace in RUN."
  (string-join
   (list
    (delib-flow--reference-note-workspace-outline-line
     run
     (format "Title [%s]"
             (delib-flow--reference-note-workspace-title-state run))
     '(edit-selected-reference-note-title))
    (delib-flow--reference-note-workspace-outline-line
     run
     (format "Draft body [%s]"
             (delib-flow--reference-note-workspace-draft-body-state run))
     '(edit-selected-reference-note-draft-body
       refresh-selected-reference-note-draft-body))
    (delib-flow--reference-note-workspace-outline-line
     run
     (format "Source highlights [%s]"
             (delib-flow--reference-note-workspace-section-state
              run
              "Source highlights"))
     '(edit-selected-reference-note-source-highlights
       refresh-selected-reference-note-source-highlights))
    (delib-flow--reference-note-workspace-outline-line
     run
     (format "Related material [%s]"
             (delib-flow--reference-note-workspace-section-state
              run
              "Related material to connect"))
     '(find-support-for-selected-reference-note
       choose-support-for-selected-reference-note
       clear-selected-reference-note-support
       edit-selected-reference-note-related-material
       refresh-selected-reference-note-related-material))
    (delib-flow--reference-note-workspace-outline-line
     run
     (format "Reuse angle [%s]"
             (delib-flow--reference-note-workspace-reuse-angle-state run))
     '(edit-selected-reference-note-reuse-angle
       refresh-selected-reference-note-reuse-angle))
    (delib-flow--reference-note-workspace-outline-line
     run "Whole note [generated]"
     '(draft-selected-reference-note))
    (delib-flow--reference-note-workspace-outline-line
     run "Revision compare"
     '(choose-saved-selected-reference-note-draft
       restore-previous-selected-reference-note-draft)))
   "\n"))

(defun delib-flow--reference-note-workspace-draft-state-text (run)
  "Return compact draft-state text for RUN."
  (if (delib-flow--artifact-family-selected-draft run 'reference-notes)
      "drafted"
    "seeded"))

(defun delib-flow--reference-note-workspace-next-move-text (run attached draft)
  "Return compact next-move text for RUN with ATTACHED support and DRAFT."
  (cond
   ((not draft) "draft the note or edit one part")
   ((null attached) "refine a part or attach support")
   (t "refine parts, then save or discard")))

(defun delib-flow--reference-note-workspace-save-target-line (run target-item)
  "Return compact save-target line for RUN and TARGET-ITEM."
  (when target-item
    (format "save %s via %s"
            (or (delib-flow--reference-note-effective-target-override run)
                "default org-roam target")
            (or (delib-flow--reference-note-effective-template-key
                 target-item run)
                "unconfigured"))))

(defun delib-flow--reference-note-workspace-selection-line (run)
  "Return a compact selected-note line for RUN."
  (if-let ((item (delib-flow--reference-note-workspace-item run)))
      (format "- Note: %s"
              (delib-flow--compact-summary
               (or (delib-flow--reference-note-effective-title item run)
                   (plist-get item :text)
                   "Untitled note")
               (+ 6 (delib-flow--mobile-summary-limit))))
    "- Note: no selected note"))

(defun delib-flow--reference-note-workspace-source-line (run)
  "Return compact clean-source access line for RUN."
  (if (delib-flow--clean-source-view-lines run)
      "- Clean source: press `O` to reopen the cleaned source while drafting."
    "- Clean source: no cleaned source preview is available yet."))

(defun delib-flow--reference-note-source-evidence-lines (run)
  "Return compact visible source-evidence lines for RUN."
  (let* ((from-highlights
          (delib-flow--selected-draft-source-evidence-excerpts
           run 'reference-notes))
         (fallback
          (mapcar (lambda (line)
                    (format "- Source: %s" line))
                  (seq-take (delib-flow--clean-source-view-lines run) 4))))
    (seq-take (delete-dups (or from-highlights fallback)) 5)))

(defun delib-flow--reference-note-source-evidence-text (run)
  "Return compact source-evidence text for RUN."
  (if-let ((lines (delib-flow--reference-note-source-evidence-lines run)))
      (string-join lines "\n")
    "- No cleaned source evidence is available yet."))

(defun delib-flow--reference-note-source-evidence-status (run)
  "Return source-evidence status text for RUN."
  (if (delib-flow--reference-note-source-evidence-lines run)
      "Keep these cleaned source lines in view while composing the note."
    "No cleaned source evidence is available yet. Open the full source if the draft needs grounding."))

(defun delib-flow--reference-note-workspace-summary-text (run)
  "Return a compact drafting summary for the selected note in RUN."
  (let* ((draft (delib-flow--reference-note-selected-draft run))
         (attached (or (delib-flow--artifact-family-selected-support-candidates
                        run 'reference-notes)
                       nil))
         (suggestions (delib-flow--selected-support-suggestion-candidates run))
         (target-item (delib-flow--reference-note-workspace-item run)))
    (string-join
     (delq nil
           (list
            (format "- State: %s | attached support %d | suggestions %d"
                    (delib-flow--reference-note-workspace-draft-state-text run)
                    (length attached)
                    (length suggestions))
            (delib-flow--reference-note-workspace-source-line run)
            (format "- Save: %s | Next: %s"
                    (or (delib-flow--reference-note-workspace-save-target-line
                         run target-item)
                        "save target not ready")
             (delib-flow--reference-note-workspace-next-move-text
                     run attached draft))))
     "\n")))

(defun delib-flow--reference-note-highlight-item-lines (run highlight)
  "Return display lines for source HIGHLIGHT in RUN."
  (let* ((clean (string-trim (string-remove-prefix "- " (or highlight ""))))
         (source (delib-flow--format-source-evidence-excerpt run clean)))
    (delq nil
          (list (format "- Note line: %s" clean)
                (when source
                  (format "  Source line: %s"
                          (string-trim (string-remove-prefix "- " source))))))))

(defun delib-flow--reference-note-source-highlights-display-text (run)
  "Return source-highlight card text for RUN as note/source pairs."
  (let* ((section (delib-flow--selected-reference-note-draft-source-highlights-text run))
         (lines (seq-filter
                 (lambda (line)
                   (string-prefix-p "-" (string-trim-left line)))
                 (split-string section "\n" t))))
    (if lines
        (mapconcat
         (lambda (line)
           (string-join
            (delib-flow--reference-note-highlight-item-lines run line)
            "\n"))
         lines
         "\n")
      section)))

(defun delib-flow--reference-note-compact-support-suggestions-text (run)
  "Return compact support-suggestion summary text for RUN."
  (let* ((all (delib-flow--selected-support-suggestion-candidates run))
         (shortlist (delib-flow--selected-support-suggestion-shortlist run)))
    (if shortlist
        (string-join
         (append
          (list
           (format "- %d suggestion%s ready. Use `Choose Support for This Draft` to inspect and attach."
                   (length all)
                   (if (= (length all) 1) "" "s")))
          (mapcar (lambda (candidate)
                    (format "  - %s"
                            (or (plist-get candidate :title)
                                (plist-get candidate :text)
                                "Untitled support")))
                  shortlist))
         "\n")
      "- No support suggestions yet. Run focused support retrieval when this note needs external material.")))

(defun delib-flow--reference-note-compact-history-text (run)
  "Return compact history summary for the selected note in RUN."
  (let ((history (delib-flow--artifact-family-draft-history run 'reference-notes)))
    (if history
        (string-join
         (list
          (format "- Saved revisions: %d" (length history))
          (delib-flow--reference-note-revision-compare-summary-text run))
         "\n")
      "- No earlier saved revision yet. Regenerate once to unlock compare.")))

(defun delib-flow--reference-note-compact-target-text (run)
  "Return compact save-target summary for RUN."
  (if-let ((item (delib-flow--reference-note-workspace-item run)))
      (string-join
       (list
        (format "- Title: %s"
                (or (delib-flow--reference-note-effective-title item run)
                    "Untitled note"))
        (format "- Template: %s"
                (or (delib-flow--reference-note-effective-template-key item run)
                    "unconfigured"))
        (format "- Path: %s"
                (or (delib-flow--reference-note-effective-target-override run)
                    "default org-roam target")))
       "\n")
    "- No selected note is active for target editing."))

(defun delib-flow--reference-note-other-candidate-lines (others)
  "Return compact display lines for alternate note candidate OTHERS."
  (mapcar
   (lambda (item)
     (format "  - %s"
             (delib-flow--compact-summary
              (or (plist-get item :text)
                  (plist-get item :title)
                  "Untitled candidate")
              (+ 6 (delib-flow--mobile-summary-limit)))))
   (seq-take others 2)))

(defun delib-flow--reference-note-compact-other-candidates-text (run)
  "Return compact other-candidate summary text for RUN."
  (let* ((candidates (or (delib-flow--artifact-family-candidates run 'reference-notes)
                         nil))
         (selected-id (delib-flow--artifact-family-selected-candidate-id
                       run 'reference-notes))
         (total (length candidates))
         (others
          (seq-remove
           (lambda (item)
             (equal (delib-flow--artifact-candidate-id item) selected-id))
           candidates)))
    (cond
     ((<= total 1)
      "- No alternate note candidate is waiting right now.")
     (t
      (string-join
       (append
        (list
         (format "- %d note candidates ready." total)
         "- Press `s` to choose a different note.")
        (delib-flow--reference-note-other-candidate-lines others))
       "\n")))))

(defun delib-flow--support-candidate-identity (candidate)
  "Return stable candidate identity text for support CANDIDATE."
  (or (plist-get candidate :file)
      (plist-get candidate :title)
      (plist-get candidate :text)
      "unknown-support"))

(defun delib-flow--support-candidate-score (candidate)
  "Return numeric score for support CANDIDATE."
  (or (plist-get candidate :support-score) 0))

(defun delib-flow--selected-support-suggestion-candidates (run)
  "Return support suggestions for the selected note in RUN, excluding attached support."
  (let* ((available (or (delib-flow--artifact-family-available-support-candidates
                         run 'reference-notes)
                        nil))
         (selected (or (delib-flow--artifact-family-selected-support-candidates
                        run 'reference-notes)
                       nil))
         (selected-ids (mapcar #'delib-flow--support-candidate-identity selected)))
    (sort
     (seq-remove
      (lambda (candidate)
        (member (delib-flow--support-candidate-identity candidate) selected-ids))
      available)
     (lambda (left right)
       (> (delib-flow--support-candidate-score left)
          (delib-flow--support-candidate-score right))))))

(defun delib-flow--selected-support-suggestion-shortlist (run)
  "Return the highest-confidence visible support suggestions for RUN."
  (seq-take (delib-flow--selected-support-suggestion-candidates run) 3))

(defun delib-flow--reference-note-attached-support-text (run)
  "Return attached-support text for the selected note in RUN."
  (if-let ((support (delib-flow--artifact-family-selected-support-candidates
                     run 'reference-notes)))
      (mapconcat #'delib-flow--reference-note-support-line support "\n")
    "No support is attached yet. Attached support is what the draft and evidence view currently trust."))

(defun delib-flow--reference-note-support-suggestions-text (run)
  "Return support-suggestions text for the selected note in RUN."
  (let* ((all (delib-flow--selected-support-suggestion-candidates run))
         (support (delib-flow--selected-support-suggestion-shortlist run))
         (hidden (- (length all) (length support))))
    (if support
        (string-join
         (append
          (mapcar #'delib-flow--reference-note-support-line support)
          (when (> hidden 0)
            (list
             (format "- %d lower-confidence suggestion%s hidden here. Use `Choose Support for This Draft` to inspect the full list."
                     hidden
                     (if (= hidden 1) "" "s")))))
         "\n")
      "No additional support suggestions are available yet. Run focused support retrieval if you want more options.")))

(defun delib-flow--reference-note-revision-compare-summary-text (run)
  "Return compact part-level compare summary for the selected note in RUN."
  (let* ((current (delib-flow--reference-note-selected-draft run))
         (previous (car (delib-flow--artifact-family-draft-history
                         run 'reference-notes))))
    (if (and current previous)
        (let ((changed
               (delq nil
                     (list
                      (delib-flow--reference-note-revision-part-change
                       current previous "draft body" #'delib-flow--reference-note-draft-working-body-text)
                      (delib-flow--reference-note-revision-part-change
                       current previous "source highlights" #'delib-flow--reference-note-draft-source-highlights-text)
                      (delib-flow--reference-note-revision-part-change
                       current previous "related material" #'delib-flow--reference-note-draft-related-material-text)
                      (delib-flow--reference-note-revision-part-change
                       current previous "reuse angle" #'delib-flow--reference-note-draft-reuse-angle-text)))))
          (if changed
              (format "- Changed since previous revision: %s."
                      (string-join changed ", "))
            "- Changed since previous revision: no visible note-part changes."))
      "No earlier draft revision is available yet. Regenerate this selected item once to create the first reversible prior draft.")))

(defun delib-flow--reference-note-current-delta-status (run)
  "Return inline current-delta status text for RUN."
  (if (delib-flow--artifact-family-draft-history run 'reference-notes)
      "This is the current-draft delta from the immediately previous saved revision."
    "No earlier saved revision exists yet. Regenerate once to unlock part-level delta tracking."))

(defun delib-flow--reference-note-part-outcome-summary (part-id changed-p)
  "Return short outcome summary for PART-ID using CHANGED-P."
  (if changed-p
      (alist-get part-id
                 '((draft-body . "Changed: body wording tightened.")
                   (source-highlights . "Changed: highlight selection updated.")
                   (related-material . "Changed: related material updated.")
                   (reuse-angle . "Changed: reuse angle updated.")))
    "No meaningful change."))

(defun delib-flow--reference-note-normalize-part-outcome-text (text)
  "Return TEXT normalized for part-outcome comparison."
  (let ((lines (split-string (or text "") "\n" t)))
    (string-join
     (mapcar
      (lambda (line)
        (let ((clean (string-trim line)))
          (setq clean (string-remove-prefix "- " clean))
          (setq clean (replace-regexp-in-string "[[:space:]]+" " " clean))
          (downcase
           (string-trim clean "[[:space:][:punct:]]*" "[[:space:][:punct:]]*"))))
      lines)
     "\n")))

(defun delib-flow--reference-note-outcome-comparison-words (text)
  "Return normalized comparison words from TEXT for semantic change checks."
  (mapcar
   (lambda (word)
     (let ((base (replace-regexp-in-string "\\(?:ing\\|ed\\|es\\|s\\)\\'" "" word)))
       (or (cdr (assoc base '(("personaliz" . "personal")
                              ("tailor" . "personal")
                              ("creat" . "build")
                              ("establish" . "build")
                              ("advisor" . "advisor")
                              ("advis" . "advisor")
                              ("lif" . "life"))))
           base)))
   (seq-filter
    (lambda (word)
      (and (>= (length word) 3)
           (not (member word delib-flow--tag-suggestion-stopwords))))
    (delib-flow--string-words
     (delib-flow--reference-note-normalize-part-outcome-text text)))))

(defun delib-flow--reference-note-outcome-word-similarity (before after)
  "Return overlap ratio between BEFORE and AFTER normalized comparison words."
  (let* ((before-words (delete-dups (delib-flow--reference-note-outcome-comparison-words before)))
         (after-words (delete-dups (delib-flow--reference-note-outcome-comparison-words after)))
         (shared (length (seq-intersection before-words after-words #'string=)))
         (total (max 1 (max (length before-words) (length after-words)))))
    (/ (float shared) total)))

(defun delib-flow--reference-note-part-outcome-meaningful-change-p (part-id before after)
  "Return non-nil when PART-ID changed meaningfully from BEFORE to AFTER."
  (let ((normalized-before
         (delib-flow--reference-note-normalize-part-outcome-text before))
        (normalized-after
         (delib-flow--reference-note-normalize-part-outcome-text after)))
    (if (equal normalized-before normalized-after)
        nil
      (pcase part-id
        ((or 'draft-body 'reuse-angle)
         (< (delib-flow--reference-note-outcome-word-similarity before after) 0.70))
        (_ t)))))

(defun delib-flow--reference-note-store-part-outcome (run part-id before after reason)
  "Return RUN with a stored regeneration outcome for PART-ID from BEFORE and AFTER.

REASON is preserved for the local part card."
  (delib-flow--set-artifact-family-part-outcome
   run 'reference-notes part-id
   (list :summary
         (delib-flow--reference-note-part-outcome-summary
          part-id
          (delib-flow--reference-note-part-outcome-meaningful-change-p
           part-id before after))
         :reason reason)))

(defun delib-flow--reference-note-part-outcome-text (run part-id)
  "Return local regeneration outcome text for PART-ID in RUN."
  (if-let ((outcome
            (delib-flow--artifact-family-part-outcome
             run 'reference-notes part-id)))
      (string-join
       (delq nil
             (list (format "- %s" (plist-get outcome :summary))
                   (when-let ((reason (plist-get outcome :reason)))
                     (format "- Latest regeneration reason: %s" reason))))
       "\n")
    "- No part regeneration has been recorded yet."))

(defun delib-flow--reference-note-revision-part-change (current previous label accessor)
  "Return LABEL when ACCESSOR differs between CURRENT and PREVIOUS."
  (unless (equal (funcall accessor current)
                 (funcall accessor previous))
    label))

(defun delib-flow--reference-note-draft-history-card-status (run)
  "Return revision-compare status text for the selected note in RUN."
  (if (delib-flow--artifact-family-draft-history run 'reference-notes)
      "Current and previous note revisions are visible here. Frozen evidence changes appear when a prior revision exists."
    "No prior revision is stored yet. Regenerate the selected note once to unlock direct comparison.")) 

(defun delib-flow--reference-note-workspace-card
    (heading status text action-heading &optional action-status action-text)
  "Return a filing-workspace card with HEADING, STATUS, TEXT, and actions."
  (when (and (null action-text) action-status)
    (setq action-text action-status))
  (concat
   (format "*** %s\n%s%s"
           heading
           (if status
               (format "%s\n\n" status)
             "")
           text)
   (when action-heading
     (format "\n\n**** %s\n%s"
             action-heading
             action-text))))

(defun delib-flow--note-focused-filing-workspace-available-p (run)
  "Return non-nil when RUN supports the focused note filing workspace."
  (or (and (eq (delib-flow--active-filing-family run) 'reference-notes)
           (or (delib-flow--reference-note-preview-item run)
               (delib-flow--filing-selection-active-p run)))
      (and (delib-flow--reference-note-workspace-active-p run)
           (not (delib-flow--project-flow-active-p run)))))

(defun delib-flow--project-focused-filing-workspace-available-p (run)
  "Return non-nil when RUN supports the focused project filing workspace."
  (delib-flow--project-flow-active-p run))

(defun delib-flow--focused-filing-workspace-p (run)
  "Return non-nil when RUN should use the focused filing workspace flow."
  (or (delib-flow--note-focused-filing-workspace-available-p run)
      (delib-flow--project-focused-filing-workspace-available-p run)))

(defun delib-flow--render-selected-note-filing-workspace (run)
  "Return note-focused filing workspace text for RUN."
  (string-join
   (list
    (format "** Selected artifact\n%s\n%s"
            (delib-flow--selected-note-workspace-status run)
            (concat
             (delib-flow--reference-note-workspace-selection-line run)
             "\n"
             (delib-flow--reference-note-workspace-summary-text run)))
    (format "** Working draft\n%s\n%s"
            (delib-flow--reference-note-draft-preview-status run)
            (string-join
             (list
              (format "*** Draft map\n%s\n%s"
                      "One note canvas, assembled from regenerable parts:"
                      (delib-flow--reference-note-workspace-outline-text run))
              (delib-flow--reference-note-workspace-card
               "Source evidence"
               (delib-flow--reference-note-source-evidence-status run)
               (delib-flow--reference-note-source-evidence-text run)
               "Do here: source evidence"
               (delib-flow--reference-note-workspace-action-text
                run
                nil
                "- Press `O` to open the full cleaned source."))
              (delib-flow--reference-note-workspace-card
               "What changed from last revision"
               (delib-flow--reference-note-current-delta-status run)
               (delib-flow--reference-note-revision-compare-summary-text run)
               "Do here: revision compare"
               (delib-flow--reference-note-workspace-action-text
                run
                '(choose-saved-selected-reference-note-draft
                  restore-previous-selected-reference-note-draft)
                "No revision action is available until at least one prior draft exists."))
              (delib-flow--reference-note-workspace-card
               (format "Note title [%s]"
                       (delib-flow--reference-note-workspace-title-state run))
               nil
               (format "- %s" (delib-flow--reference-note-title
                               (delib-flow--reference-note-workspace-item run)))
               "Do here: note title"
               (delib-flow--reference-note-workspace-action-text
                run
                '(edit-selected-reference-note-title)
                "No title action is available yet."))
              (delib-flow--reference-note-workspace-card
              (format "Draft body [%s]"
                      (delib-flow--reference-note-workspace-draft-body-state run))
              (delib-flow--reference-note-part-outcome-text run 'draft-body)
              (delib-flow--selected-reference-note-draft-working-body-text run)
              "Do here: draft body"
               (delib-flow--reference-note-workspace-action-text
                run
                '(edit-selected-reference-note-draft-body
                  refresh-selected-reference-note-draft-body)
                "No draft-body action is available yet."))
              (delib-flow--reference-note-workspace-card
               (format "Source highlights [%s]"
                       (delib-flow--reference-note-workspace-section-state
                        run
                        "Source highlights"))
               (delib-flow--reference-note-part-outcome-text run 'source-highlights)
               (delib-flow--reference-note-source-highlights-display-text run)
               "Do here: source highlights"
               (delib-flow--reference-note-workspace-action-text
                run
                '(edit-selected-reference-note-source-highlights
                  refresh-selected-reference-note-source-highlights)
                "No source-highlight refresh is available yet."))
              (delib-flow--reference-note-workspace-card
               (format "Related material [%s]"
                       (delib-flow--reference-note-workspace-section-state
                        run
                        "Related material to connect"))
               (delib-flow--reference-note-part-outcome-text run 'related-material)
               (delib-flow--selected-reference-note-draft-related-material-text run)
               "Do here: related material"
               (delib-flow--reference-note-workspace-action-text
                run
                '(find-support-for-selected-reference-note
                  choose-support-for-selected-reference-note
                  clear-selected-reference-note-support
                  edit-selected-reference-note-related-material
                  refresh-selected-reference-note-related-material)
                "No related-material action is available yet."))
              (delib-flow--reference-note-workspace-card
               (format "Reuse angle [%s]"
                       (delib-flow--reference-note-workspace-reuse-angle-state run))
               (delib-flow--reference-note-part-outcome-text run 'reuse-angle)
               (delib-flow--selected-reference-note-draft-reuse-angle-text run)
               "Do here: reuse angle"
               (delib-flow--reference-note-workspace-action-text
                run
                '(edit-selected-reference-note-reuse-angle
                  refresh-selected-reference-note-reuse-angle)
                "No reuse-angle refresh is available yet."))
              (delib-flow--reference-note-workspace-card
               "Whole note rebuild [generated]"
               nil
               (delib-flow--reference-note-regeneration-text run)
               "Do here: whole note rebuild"
               (delib-flow--reference-note-workspace-action-text
                run
                '(draft-selected-reference-note)
                "No whole-note regeneration is available yet."))
              (delib-flow--reference-note-workspace-card
               "Revision compare"
               (delib-flow--reference-note-draft-history-card-status run)
               (delib-flow--reference-note-compact-history-text run)
               "Do here: revision compare"
               (delib-flow--reference-note-workspace-action-text
                run
                '(choose-saved-selected-reference-note-draft
                  restore-previous-selected-reference-note-draft)
                "No revision action is available until at least one prior draft exists.")))
             "\n\n"))
    (format "** Support for this draft\n%s\n%s"
            (delib-flow--family-support-status run 'reference-notes)
            (string-join
             (list
              (delib-flow--reference-note-workspace-card
               "Attached support"
               nil
               (delib-flow--reference-note-attached-support-text run)
               "Do here: attached support"
               (delib-flow--reference-note-workspace-action-text
                run
                '(clear-selected-reference-note-support)
                "No attached-support action is available yet."))
              (delib-flow--reference-note-workspace-card
               "Support suggestions"
               nil
               (delib-flow--reference-note-compact-support-suggestions-text run)
               "Do here: support suggestions"
               (delib-flow--reference-note-workspace-action-text
                run
                '(find-support-for-selected-reference-note
                 choose-support-for-selected-reference-note)
                "No support-suggestion action is available yet.")))
             "\n\n"))
    (format "** Save or discard\n%s\n%s\n\n*** Do here: save or discard\n%s"
            (delib-flow--reference-note-regeneration-status run)
            "- Save only when the visible note looks right."
            (delib-flow--reference-note-workspace-action-text
             run
             '(select-approved-filing-actions
               file-approved-outputs
               reject-draft-filing-artifact
               resolve-filing-conflict)
             "No filing action is available yet."))
    (format "** Final save target\n%s\n%s"
            (delib-flow--reference-note-target-summary-status run)
            (concat
             (delib-flow--reference-note-compact-target-text run)
             "\n\n"
             (delib-flow--reference-note-workspace-card
              "Target controls"
              nil
              "- Change template or path only when the save destination is wrong."
              "Do here: target controls"
              (delib-flow--reference-note-workspace-action-text
               run
               '(choose-reference-note-template
                 edit-selected-reference-note-target-path)
               "No target-edit action is available yet."))))
    (format "** Other candidates\n%s\n%s"
            (delib-flow--filing-family-shortlist-status run 'reference-notes)
            (delib-flow--reference-note-compact-other-candidates-text run))
    (format "** Leave workspace\n- `B` return to cockpit\n- `F` reopen this workspace later\n- Preview: `E` draft, `I` support, `P` target, `V` staged\n- Save state: %s"
            (delib-flow--filing-save-state-text run)))
   "\n\n"))

(defun delib-flow--focused-filing-summary-status (run)
  "Return compact cockpit summary status for the focused filing loop in RUN."
  (cond
   ((delib-flow--project-focused-filing-workspace-available-p run)
    "The selected project package has its own local workflow. Open the focused filing workspace to extract, refine, approve, and file the package from one place.")
   ((delib-flow--artifact-family-selected-draft run 'reference-notes)
    "The selected note has a working draft. Open the filing workspace to refine support, draft parts, and save target in one focused buffer.")
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    "A note candidate is selected for filing. Open the filing workspace to turn it into a working draft and save it.")
   ((delib-flow--reference-note-draft-preview-items run)
    "Reference-note candidates are ready. Open the filing workspace to choose one and work through the note loop without cockpit clutter.")
   (t
    "Focused filing workspace is available when note candidates or a selected note are active.")))

(defun delib-flow--focused-filing-summary-text (run)
  "Return compact cockpit summary text for the focused filing loop in RUN."
  (string-join
   (delib-flow--focused-filing-summary-lines-for-run run)
   "\n"))

(defun delib-flow--focused-filing-summary-lines-for-run (run)
  "Return compact cockpit summary lines for the focused filing loop in RUN."
  (if (delib-flow--project-focused-filing-workspace-available-p run)
      (delq nil
            (list
             (format "- Active project: %s"
                     (delib-flow--compact-summary
                      (or (plist-get (delib-flow--project-package-root-for-filing run) :title)
                          (delib-flow--effective-project-title run)
                          (delib-flow--active-filing-item-text run))))
             (format "- Package status: %s"
                     (if (delib-flow--approved-project-package-p run)
                         "approved bundle ready to file"
                       "drafted package still being refined"))
             (format "- Included items: %s"
                     (length (or (delib-flow--project-package-included-items run) nil)))
             (format "- Final target bucket: %s"
                     (delib-flow--project-state-bucket-title
                      (or (plist-get (delib-flow--project-package-root-for-filing run) :state)
                          'active)))
             "- Main move here: press `F` to open the focused filing workspace."
             "- Numbered actions in that workspace are local to the project package."))
    (let ((support (delib-flow--artifact-family-selected-support-candidates
                    run 'reference-notes))
          (draft (delib-flow--artifact-family-selected-draft run 'reference-notes)))
      (delib-flow--focused-filing-summary-lines
       run
       support
       (delib-flow--focused-filing-support-title support)
       draft
       (delib-flow--focused-filing-draft-snippet draft)))))

(defun delib-flow--focused-filing-support-title (support)
  "Return compact attached SUPPORT title text, or nil."
  (when support
    (or (plist-get (car support) :title)
        (plist-get (car support) :text)
        "selected support")))

(defun delib-flow--focused-filing-draft-snippet (draft)
  "Return compact DRAFT snippet text, or nil."
  (when draft
    (delib-flow--compact-summary
     (or (plist-get draft :draft-body)
         (plist-get draft :text))
     (+ 18 (delib-flow--mobile-summary-limit)))))

(defun delib-flow--focused-filing-summary-lines
    (run support support-title draft draft-snippet)
  "Return compact cockpit summary lines for the focused filing loop in RUN."
  (delq nil
        (list
         (format "- Active item: %s"
                 (delib-flow--compact-summary
                  (delib-flow--active-filing-item-text run)))
         (format "- Working draft: %s"
                 (if draft "present" "not drafted yet"))
         (and draft-snippet
              (format "- Draft preview: %s" draft-snippet))
         (format "- Attached support: %s"
                 (length (or support nil)))
         (and support-title
              (format "- Attached support title: %s" support-title))
         (format "- Final save target: %s"
                 (delib-flow--compact-summary
                  (delib-flow--reference-note-target-summary-text run)
                  (+ 10 (delib-flow--mobile-summary-limit))))
         (format "- Current blocked reason: %s"
                 (delib-flow--compact-summary
                  (delib-flow--selection-block-status run)
                  (+ 10 (delib-flow--mobile-summary-limit))))
         "- Main move here: press `F` to open the focused filing workspace."
         "- Fallback move: use `.` in this section for direct filing actions without opening the workspace.")))

(defun delib-flow--artifact-selection-section (run)
  "Return the artifact-selection section for RUN, or nil."
  (when (and (delib-flow--filing-selection-active-p run)
             (not (delib-flow--project-flow-active-p run)))
    (format "*** Artifact selection\n%s\n\n**** Choose from queue\n%s\n\n**** Selection form\n%s"
            (delib-flow--filing-selection-instructions run)
            (delib-flow--filing-selection-shortlist-cards run)
            (delib-flow--render-editable-block-with-editor-help run 'filing-selection-review))))

(defun delib-flow--filing-conflict-resolution-section (run)
  "Return the filing-conflict resolution section for RUN, or nil."
  (when (delib-flow--filing-conflict-resolution-active-p run)
    (format "*** Conflict resolution\n%s"
            (delib-flow--render-editable-block-with-editor-help run 'filing-conflict-resolution))))

(defun delib-flow--filing-selection-block-summary (run)
  "Return the approval-block summary for RUN."
  (format "%s\n%s"
          (delib-flow--selection-block-status run)
          (delib-flow--selection-block-text run)))

(defun delib-flow--render-filing-preview-section (run)
  "Return Org text for the Filing preview section."
  (if (delib-flow--project-focused-filing-workspace-available-p run)
      (string-join
       (list
        (format "*** Project workspace\n%s"
                (delib-flow--render-selected-project-filing-workspace run))
        (format "*** Current blocked state\n%s"
                (delib-flow--filing-selection-block-summary run)))
       "\n\n")
    (if (delib-flow--focused-filing-workspace-p run)
        (string-join
         (list
          (format "*** Focused filing workspace\n%s\n%s"
                  (delib-flow--focused-filing-summary-status run)
                  (delib-flow--focused-filing-summary-text run))
          (format "*** Filing state\n%s"
                  (delib-flow--filing-workspace-summary-text run))
          (format "*** Current blocked state\n%s"
                  (delib-flow--filing-selection-block-summary run)))
         "\n\n")
      (string-join
       (append
        (list
         (format "*** What happens here\n%s"
                 (delib-flow--draft-item-status run))
         (format "*** What to do next\n%s"
                 (delib-flow--recommended-filing-action-text run))
         (format "*** Current filing plan\n%s"
                 (delib-flow--filing-workspace-summary-text run)))
        (delib-flow--filing-preview-workspace-sections run)
        (list
         (format "*** Filing actions\n%s"
                 (delib-flow--filing-preview-actions-text run))
         (format "*** Staged content preview\n%s\n%s"
                 (delib-flow--staged-content-preview-status run)
                 (delib-flow--staged-content-preview-text run)))
        (delq nil
              (list (delib-flow--reference-note-capture-section run)
                    (delib-flow--artifact-selection-section run)
                    (delib-flow--filing-conflict-resolution-section run)))
        (list
         (format "*** Available queue\n%s"
                 (delib-flow--draft-item-text run))
         (format "*** Why approval is blocked\n%s"
                 (delib-flow--filing-selection-block-summary run))
         (format "*** Rejected artifacts\n%s"
                 (delib-flow--rejected-item-text run))
         (format "*** Filing conflicts\n%s"
                 (delib-flow--filing-conflict-text run))
         (format "*** Opened staged targets\n%s\n%s"
                 (delib-flow--filed-location-status run)
                 (delib-flow--filed-location-text run))))
       "\n\n"))))



(provide 'delib-flow-render)

;;; delib-flow-render.el ends here
