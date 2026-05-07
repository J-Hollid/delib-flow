;;; delib-flow-stages.el --- Workflow stages for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Workflow stage descriptors, prompt packaging, and stage execution helpers.

;;; Code:

(require 'org)
(require 'rx)
(require 'seq)
(require 'subr-x)

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
  (member value '("email" "meeting-note" "fleeting-note" "issue-note" "reminder" "unknown")))

(defun delib-flow--extract-result-signature (items)
  "Return stable comparison signature for extract result ITEMS."
  (mapcar (lambda (item)
            (cons (plist-get item :kind)
                  (downcase (or (plist-get item :text) ""))))
          items))

(defun delib-flow--extract-result-comparison (package stage-id items key)
  "Return comparison metadata for PACKAGE STAGE-ID ITEMS against prior stage KEY."
  (let* ((entries (delib-flow--stage-history-entries-for-stage package stage-id))
         (previous-entry (car (last entries)))
         (previous-items (plist-get (plist-get previous-entry :raw-output) key))
         (similar-p (and previous-items
                         (equal (delib-flow--extract-result-signature items)
                                (delib-flow--extract-result-signature previous-items)))))
    (list :previous-attempt-count (length entries)
          :similar-to-previous-p similar-p)))

(defun delib-flow--extract-actions-result (package)
  "Return raw action-extraction result for PACKAGE."
  (let* ((source-action (delib-flow--source-title-action package))
         (retained-actions (delib-flow--retained-candidate-actions package))
         (actions (delib-flow--annotate-draft-actions
                   (delib-flow--annotate-draft-item-tags
                    (cons source-action retained-actions)
                    package)))
         (comparison
          (delib-flow--extract-result-comparison
           package 'extract-actions actions :actions)))
    (list :candidate-count (length actions)
          :operator-intent (delib-flow--operator-intent-text-from-package package)
          :project-context-kind (delib-flow--effective-project-context-kind package)
          :project-context-title (delib-flow--effective-project-title package)
          :previous-attempt-count (plist-get comparison :previous-attempt-count)
          :similar-to-previous-p (plist-get comparison :similar-to-previous-p)
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
                  package)))
         (comparison
          (delib-flow--extract-result-comparison
           package 'extract-waiting-for items :waiting-fors)))
    (list :candidate-count (length items)
          :operator-intent (delib-flow--operator-intent-text-from-package package)
          :project-context-kind (delib-flow--effective-project-context-kind package)
          :project-context-title (delib-flow--effective-project-title package)
          :previous-attempt-count (plist-get comparison :previous-attempt-count)
          :similar-to-previous-p (plist-get comparison :similar-to-previous-p)
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
                 package))
         (items (delib-flow--promote-reference-note-items items)))
    (list :candidate-count (length items)
          :warning-count (delib-flow--item-warning-total items)
          :warning-item-count (delib-flow--items-with-warnings-count items)
          :blocking-warning-count (delib-flow--item-blocking-warning-total items)
          :blocking-warning-item-count
          (delib-flow--items-with-blocking-warnings-count items)
          :reference-notes items)))

(defun delib-flow--draft-selected-action-result (package)
  "Return raw drafted-action output for the selected next action in PACKAGE."
  (let ((candidate (delib-flow--selected-action-candidate-for-drafting package)))
    (unless (eq (plist-get candidate :kind) 'next-action)
      (error "Select one action candidate before drafting it"))
    (let ((drafted-item (delib-flow--drafted-action-item candidate package)))
      (list :candidate candidate
            :drafted-item drafted-item
            :reason "Drafted the selected action only. Regenerate this action if you want a new pass without replacing the full action queue."))))

(defun delib-flow--draft-selected-waiting-for-result (package)
  "Return raw drafted waiting-for output for the selected waiting-for in PACKAGE."
  (let ((candidate (delib-flow--selected-waiting-for-candidate-for-drafting package)))
    (unless (eq (plist-get candidate :kind) 'waiting-for)
      (error "Select one waiting-for candidate before drafting it"))
    (let ((drafted-item (delib-flow--drafted-waiting-for-item candidate package)))
      (list :candidate candidate
            :drafted-item drafted-item
            :reason "Drafted the selected waiting-for only. Regenerate this waiting-for if you want a new pass without replacing the full waiting-for queue."))))

(defun delib-flow--draft-selected-reference-note-result (package)
  "Return raw drafted-note output for the selected reference-note in PACKAGE."
  (let ((candidate (delib-flow--selected-reference-note-candidate-for-drafting package)))
    (unless (eq (plist-get candidate :kind) 'reference-note)
      (error "Select one reference-note candidate before drafting it"))
    (let ((drafted-item (delib-flow--drafted-reference-note-item candidate package)))
      (list :candidate candidate
            :drafted-item drafted-item
            :reason "Built the selected note only. Rebuild this note if you want a new local pass without replacing the full note queue."))))

(defun delib-flow--draft-selected-reference-note-part-result
    (package part-id update-fn)
  "Return raw drafted selected-note PART-ID output for PACKAGE using UPDATE-FN."
  (let ((candidate (delib-flow--selected-reference-note-candidate-for-drafting package)))
    (unless (eq (plist-get candidate :kind) 'reference-note)
      (error "Select one reference-note candidate before drafting it"))
    (let* ((updated-package (funcall update-fn package))
           (drafted-item
            (delib-flow--reference-note-draft-with-workspace-structure
             (delib-flow--artifact-family-selected-draft
              updated-package 'reference-notes)
             candidate
             updated-package))
           (part-label (delib-flow--reference-note-part-stage-label part-id)))
      (list :candidate candidate
            :drafted-item drafted-item
            :part-id part-id
            :part-text
            (delib-flow--reference-note-part-stage-text drafted-item part-id)
            :reason
            (format "Regenerated only the selected note %s." part-label)))))

(defun delib-flow--draft-selected-reference-note-body-result (package)
  "Return raw drafted selected-note body output for PACKAGE."
  (delib-flow--draft-selected-reference-note-part-result
   package
   'draft-body
   #'delib-flow--reference-note-refresh-draft-body))

(defun delib-flow--draft-selected-reference-note-source-highlights-result (package)
  "Return raw drafted selected-note source-highlights output for PACKAGE."
  (delib-flow--draft-selected-reference-note-part-result
   package
   'source-highlights
   #'delib-flow--reference-note-refresh-source-highlights))

(defun delib-flow--draft-selected-reference-note-related-material-result (package)
  "Return raw drafted selected-note related-material output for PACKAGE."
  (delib-flow--draft-selected-reference-note-part-result
   package
   'related-material
   #'delib-flow--reference-note-refresh-related-material))

(defun delib-flow--draft-selected-reference-note-reuse-angle-result (package)
  "Return raw drafted selected-note reuse-angle output for PACKAGE."
  (delib-flow--draft-selected-reference-note-part-result
   package
   'reuse-angle
   #'delib-flow--reference-note-refresh-reuse-angle))

(defun delib-flow--draft-selected-project-result (package)
  "Return raw drafted-project output for the selected project in PACKAGE."
  (let ((candidate (delib-flow--selected-project-candidate-for-drafting package)))
    (unless (eq (plist-get candidate :kind) 'project)
      (error "Select one project candidate before drafting it"))
    (let ((drafted-item (delib-flow--drafted-project-item candidate package)))
      (list :candidate candidate
            :drafted-item drafted-item
            :reason "Drafted the selected project only. Regenerate this project if you want a new pass without replacing the full project queue."))))

(defun delib-flow--execute-find-support-for-selected-action (package)
  "Return focused support output for the selected action in PACKAGE."
  (delib-flow--find-support-for-selected-action-result package))

(defun delib-flow--execute-find-support-for-selected-waiting-for (package)
  "Return focused support output for the selected waiting-for in PACKAGE."
  (delib-flow--find-support-for-selected-waiting-for-result package))

(defun delib-flow--execute-find-support-for-selected-reference-note (package)
  "Return focused support output for the selected note in PACKAGE."
  (delib-flow--find-support-for-selected-reference-note-result package))

(defun delib-flow--execute-find-support-for-selected-project (package)
  "Return focused support output for the selected project in PACKAGE."
  (delib-flow--find-support-for-selected-project-result package))

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

(defun delib-flow--execute-draft-selected-waiting-for (package)
  "Return raw selected waiting-for drafting output for PACKAGE."
  (delib-flow--draft-selected-waiting-for-result package))

(defun delib-flow--execute-suggest-reference-notes (package)
  "Return raw reference-note suggestion output for PACKAGE."
  (delib-flow--suggest-reference-notes-result package))

(defun delib-flow--execute-draft-selected-action (package)
  "Return raw selected-action drafting output for PACKAGE."
  (delib-flow--draft-selected-action-result package))

(defun delib-flow--execute-draft-selected-reference-note (package)
  "Return raw selected-note drafting output for PACKAGE."
  (delib-flow--draft-selected-reference-note-result package))

(defun delib-flow--execute-draft-selected-reference-note-body (package)
  "Return raw selected-note body drafting output for PACKAGE."
  (delib-flow--draft-selected-reference-note-body-result package))

(defun delib-flow--execute-draft-selected-reference-note-source-highlights (package)
  "Return raw selected-note source-highlights drafting output for PACKAGE."
  (delib-flow--draft-selected-reference-note-source-highlights-result package))

(defun delib-flow--execute-draft-selected-reference-note-related-material (package)
  "Return raw selected-note related-material drafting output for PACKAGE."
  (delib-flow--draft-selected-reference-note-related-material-result package))

(defun delib-flow--execute-draft-selected-reference-note-reuse-angle (package)
  "Return raw selected-note reuse-angle drafting output for PACKAGE."
  (delib-flow--draft-selected-reference-note-reuse-angle-result package))

(defun delib-flow--execute-draft-selected-project (package)
  "Return raw selected-project drafting output for PACKAGE."
  (delib-flow--draft-selected-project-result package))

(defconst delib-flow--stage-command-runner-default-alist
  '((decide-cloud-pass . run-stage-locally)
    (sanitize-for-cloud . run-stage-locally)
    (approve-cloud-send . run-stage-locally)
    (run-cloud-stage . dynamic-cloud-stage)
    (resolve-cloud-failure . run-stage-locally)
    (approve-candidate-reintegration . run-stage-locally))
  "Default UI command runner kinds keyed by stage id.")

(defun delib-flow--default-stage-command-runner-kind (stage-id)
  "Return the default UI command runner kind for STAGE-ID."
  (or (alist-get stage-id delib-flow--stage-command-runner-default-alist)
      'execute-local-stage))

(defun delib-flow--default-stage-command-rerender (stage-id)
  "Return the default rerender policy for STAGE-ID."
  (ignore stage-id)
  'current-result)

(defun delib-flow--stage-descriptor-with-command-metadata (descriptor)
  "Return DESCRIPTOR with default command metadata populated."
  (let ((stage-id (plist-get (cdr descriptor) :id))
        (props (copy-sequence (cdr descriptor))))
    (unless (plist-member props :command-runner-kind)
      (setq props
            (plist-put props
                       :command-runner-kind
                       (delib-flow--default-stage-command-runner-kind stage-id))))
    (unless (plist-member props :command-sync)
      (setq props
            (plist-put props :command-sync 'control-buffer)))
    (unless (plist-member props :command-rerender)
      (setq props
            (plist-put props
                       :command-rerender
                       (delib-flow--default-stage-command-rerender stage-id))))
    (cons (car descriptor) props)))

(defconst delib-flow--stage-descriptor-alist
  (mapcar
   #'delib-flow--stage-descriptor-with-command-metadata
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
    (draft-selected-waiting-for
     :id draft-selected-waiting-for
     :label "Draft Selected Waiting-For"
     :prompt-id milestone15-draft-selected-waiting-for
     :executor delib-flow--execute-draft-selected-waiting-for
     :normalizer delib-flow--normalize-draft-selected-waiting-for-output)
    (find-support-for-selected-waiting-for
     :id find-support-for-selected-waiting-for
     :label "Find Support for Selected Waiting-For"
     :prompt-id milestone18-find-support-for-selected-waiting-for
     :executor delib-flow--execute-find-support-for-selected-waiting-for
     :normalizer delib-flow--normalize-find-support-for-selected-output)
    (suggest-reference-notes
     :id suggest-reference-notes
     :label "Suggest Reference Notes"
     :prompt-id milestone2-suggest-reference-notes
     :executor delib-flow--execute-suggest-reference-notes
     :normalizer delib-flow--normalize-suggest-reference-notes-output)
    (draft-selected-action
     :id draft-selected-action
     :label "Draft Selected Action"
     :prompt-id milestone14-draft-selected-action
     :executor delib-flow--execute-draft-selected-action
     :normalizer delib-flow--normalize-draft-selected-action-output)
    (find-support-for-selected-action
     :id find-support-for-selected-action
     :label "Find Support for Selected Action"
     :prompt-id milestone17-find-support-for-selected-action
     :executor delib-flow--execute-find-support-for-selected-action
     :normalizer delib-flow--normalize-find-support-for-selected-output)
    (draft-selected-reference-note
     :id draft-selected-reference-note
     :label "Draft Selected Note"
     :prompt-id milestone13-draft-selected-reference-note
     :executor delib-flow--execute-draft-selected-reference-note
     :normalizer delib-flow--normalize-draft-selected-reference-note-output)
    (draft-selected-reference-note-body
     :id draft-selected-reference-note-body
     :label "Draft Selected Note Body"
     :prompt-id milestone13-draft-selected-reference-note-body
     :executor delib-flow--execute-draft-selected-reference-note-body
     :normalizer delib-flow--normalize-draft-selected-reference-note-part-output)
    (draft-selected-reference-note-source-highlights
     :id draft-selected-reference-note-source-highlights
     :label "Draft Selected Note Source Highlights"
     :prompt-id milestone13-draft-selected-reference-note-source-highlights
     :executor delib-flow--execute-draft-selected-reference-note-source-highlights
     :normalizer delib-flow--normalize-draft-selected-reference-note-part-output)
    (draft-selected-reference-note-related-material
     :id draft-selected-reference-note-related-material
     :label "Draft Selected Note Related Material"
     :prompt-id milestone13-draft-selected-reference-note-related-material
     :executor delib-flow--execute-draft-selected-reference-note-related-material
     :normalizer delib-flow--normalize-draft-selected-reference-note-part-output)
    (draft-selected-reference-note-reuse-angle
     :id draft-selected-reference-note-reuse-angle
     :label "Draft Selected Note Reuse Angle"
     :prompt-id milestone13-draft-selected-reference-note-reuse-angle
     :executor delib-flow--execute-draft-selected-reference-note-reuse-angle
     :normalizer delib-flow--normalize-draft-selected-reference-note-part-output)
    (find-support-for-selected-reference-note
     :id find-support-for-selected-reference-note
     :label "Find Support for Selected Note"
     :prompt-id milestone19-find-support-for-selected-reference-note
     :executor delib-flow--execute-find-support-for-selected-reference-note
     :normalizer delib-flow--normalize-find-support-for-selected-output)
    (draft-selected-project
     :id draft-selected-project
     :label "Draft Selected Project"
     :prompt-id milestone16-draft-selected-project
     :executor delib-flow--execute-draft-selected-project
     :normalizer delib-flow--normalize-draft-selected-project-output)
    (find-support-for-selected-project
     :id find-support-for-selected-project
     :label "Find Support for Selected Project"
     :prompt-id milestone20-find-support-for-selected-project
     :executor delib-flow--execute-find-support-for-selected-project
     :normalizer delib-flow--normalize-find-support-for-selected-output)
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
     :normalizer delib-flow--normalize-file-approved-outputs-output)))
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
    (draft-selected-waiting-for
     . "Review the selected waiting-for draft and decide whether to regenerate or file it.")
    (find-support-for-selected-waiting-for
     . "Review focused support for the selected waiting-for and decide whether to regenerate or file it.")
    (suggest-reference-notes
     . "Review drafted reference notes and choose next action.")
    (draft-selected-action
     . "Review the selected action draft and decide whether to regenerate or file it.")
    (find-support-for-selected-action
     . "Review focused support for the selected action and decide whether to regenerate or file it.")
    (draft-selected-reference-note
     . "Review the selected note draft and decide whether to regenerate or file it.")
    (draft-selected-reference-note-body
     . "Review the selected note body draft and decide whether to keep regenerating this part or file it.")
    (draft-selected-reference-note-source-highlights
     . "Review the selected note source highlights draft and decide whether to keep regenerating this part or file it.")
    (draft-selected-reference-note-related-material
     . "Review the selected note related material draft and decide whether to keep regenerating this part or file it.")
    (draft-selected-reference-note-reuse-angle
     . "Review the selected note reuse angle draft and decide whether to keep regenerating this part or file it.")
    (find-support-for-selected-reference-note
     . "Review focused support for the selected note and decide whether to regenerate or file it.")
    (draft-selected-project
     . "Review the selected project draft and decide whether to regenerate or file it.")
    (find-support-for-selected-project
     . "Review focused support for the selected project and decide whether to regenerate or file it.")
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
    (draft-selected-waiting-for . delib-flow--apply-draft-selected-waiting-for-entry)
    (find-support-for-selected-waiting-for
     . delib-flow--apply-find-support-for-selected-entry)
    (suggest-reference-notes . delib-flow--apply-suggest-reference-notes-entry)
    (draft-selected-action . delib-flow--apply-draft-selected-action-entry)
    (find-support-for-selected-action
     . delib-flow--apply-find-support-for-selected-entry)
    (draft-selected-reference-note . delib-flow--apply-draft-selected-reference-note-entry)
    (draft-selected-reference-note-body
     . delib-flow--apply-draft-selected-reference-note-entry)
    (draft-selected-reference-note-source-highlights
     . delib-flow--apply-draft-selected-reference-note-entry)
    (draft-selected-reference-note-related-material
     . delib-flow--apply-draft-selected-reference-note-entry)
    (draft-selected-reference-note-reuse-angle
     . delib-flow--apply-draft-selected-reference-note-entry)
    (find-support-for-selected-reference-note
     . delib-flow--apply-find-support-for-selected-entry)
    (draft-selected-project . delib-flow--apply-draft-selected-project-entry)
    (find-support-for-selected-project
     . delib-flow--apply-find-support-for-selected-entry)
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
    (draft-selected-reference-note-body
     . (:stage-family selected-reference-note-part-draft
        :part body
        :quality-rules ("Keep the durable idea grounded in the source."
                        "Do not rename the note."
                        "Improve only the working draft body.")))
    (draft-selected-reference-note-source-highlights
     . (:stage-family selected-reference-note-part-draft
        :part source-highlights
        :quality-rules ("Return only grounded source highlights."
                        "Keep the highlights concise."
                        "Improve only the source highlights section.")))
    (draft-selected-reference-note-related-material
     . (:stage-family selected-reference-note-part-draft
        :part related-material
        :quality-rules ("Use attached support when available."
                        "Avoid unsupported speculation."
                        "Improve only the related material section.")))
    (draft-selected-reference-note-reuse-angle
     . (:stage-family selected-reference-note-part-draft
        :part reuse-angle
        :quality-rules ("State why the note will matter later."
                        "Keep the reuse angle concise."
                        "Improve only the reuse angle line.")))
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

(defconst delib-flow--required-stage-command-metadata-keys
  '(:command-runner-kind :command-sync :command-rerender)
  "Descriptor keys required by the shared stage-command runner.")

(defun delib-flow--validated-stage-descriptor (stage-id)
  "Return validated descriptor plist for STAGE-ID."
  (let ((descriptor (delib-flow--stage-descriptor stage-id)))
    (unless descriptor
      (error "No stage descriptor is registered for %s" stage-id))
    (dolist (key delib-flow--required-stage-command-metadata-keys descriptor)
      (unless (plist-member descriptor key)
        (error "Stage descriptor for %s is missing %s" stage-id key)))))

(defun delib-flow--stage-descriptor (stage-id)
  "Return the descriptor plist for STAGE-ID."
  (cdr (assoc stage-id delib-flow--stage-descriptor-alist)))

(defun delib-flow--stage-label (stage-id)
  "Return the user-facing label for STAGE-ID."
  (plist-get (delib-flow--stage-descriptor stage-id) :label))

(defun delib-flow--stage-command-runner-kind (stage-id)
  "Return UI command runner kind for STAGE-ID."
  (plist-get (delib-flow--validated-stage-descriptor stage-id)
             :command-runner-kind))

(defun delib-flow--stage-command-sync (stage-id)
  "Return UI command sync policy for STAGE-ID."
  (plist-get (delib-flow--validated-stage-descriptor stage-id)
             :command-sync))

(defun delib-flow--stage-command-rerender (stage-id)
  "Return UI command rerender policy for STAGE-ID."
  (plist-get (delib-flow--validated-stage-descriptor stage-id)
             :command-rerender))

(defun delib-flow--stage-prompt-id (stage-id)
  "Return the prompt identifier for STAGE-ID."
  (plist-get (delib-flow--stage-descriptor stage-id) :prompt-id))

(defun delib-flow--file-readable-p (path)
  "Return non-nil when PATH is a readable file."
  (and (delib-flow--non-empty-string-p path)
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
  (when-let ((file (and (delib-flow--prompt-library-configured-p)
                        delib-flow-prompt-library-file)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (and (re-search-forward
            (concat "^:PROMPT_ID:[ \t]*"
                    (regexp-quote (symbol-name prompt-id))
                    "$")
            nil t)
           (progn
             (org-back-to-heading t)
             (delib-flow--org-entry-body-at-point))))))

(defun delib-flow--example-structures-text ()
  "Return example-structure text, if configured."
  (when (delib-flow--example-structures-configured-p)
    (with-temp-buffer
      (insert-file-contents delib-flow-example-structures-file)
      (string-trim (buffer-string)))))

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

(defun delib-flow--artifact-stage-guidance (artifact-kind quality-rules)
  "Return structured guidance for ARTIFACT-KIND with QUALITY-RULES."
  (list :artifact-kind artifact-kind
        :one-at-a-time-review-p t
        :quality-rules quality-rules))

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

(defun delib-flow--cloud-shadow-entry-p (entry)
  "Return non-nil when ENTRY is a deferred rerouted cloud stage record."
  (plist-get entry :cloud-shadow-p))

(defun delib-flow--stage-entry-counts-as-executed-p (entry)
  "Return non-nil when ENTRY should count as executed for stage legality."
  (if (delib-flow--cloud-shadow-entry-p entry)
      (plist-get entry :applied-p)
    t))

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

(defun delib-flow--execute-resolve-cloud-failure (package)
  "Return raw cloud-failure resolution output for PACKAGE."
  (delib-flow--resolve-cloud-failure-result package))

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

(defun delib-flow--clear-cloud-returned-stage-data (working)
  "Return WORKING with stored rerouted cloud stage data cleared."
  (plist-put
   (plist-put
    (plist-put working :cloud-returned-stage-id nil)
    :cloud-returned-stage-raw-output nil)
   :cloud-returned-stage-normalized-output nil))

(defun delib-flow--clear-cloud-failure-state (routing)
  "Return ROUTING with recorded cloud failure and fallback state cleared."
  (plist-put
   (plist-put
    (plist-put
     (plist-put routing :cloud-failure-stage nil)
     :cloud-failure-message nil)
    :cloud-fallback-mode nil)
   :reintegration-status nil))

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
         (project-id (and project-item
                          (delib-flow--artifact-candidate-id project-item)))
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
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run 'project-proposals project-id))
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

(defun delib-flow--apply-draft-selected-action-entry (run entry)
  "Return RUN updated from completed selected-action draft ENTRY."
  (let* ((raw (plist-get entry :raw-output))
         (candidate (plist-get raw :candidate))
         (drafted-item (plist-get raw :drafted-item))
         (candidate-id (and candidate
                            (delib-flow--artifact-candidate-id candidate))))
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run
           'actions
           candidate-id))
    (delib-flow--replace-artifact-family-selected-draft
     run
     'actions
     drafted-item)))

(defun delib-flow--apply-find-support-for-selected-entry (run entry)
  "Return RUN updated from completed focused-support ENTRY."
  (let* ((raw (plist-get entry :raw-output))
         (family (plist-get raw :family))
         (candidate (plist-get raw :candidate))
         (candidate-id (and candidate
                            (delib-flow--artifact-candidate-id candidate)))
         (support-candidates (plist-get raw :support-candidates))
         (support-context (plist-get raw :support-context)))
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run family candidate-id))
    (delib-flow--set-artifact-family-available-support
     run family support-candidates support-context)))

(defun delib-flow--apply-draft-selected-waiting-for-entry (run entry)
  "Return RUN updated from completed selected waiting-for draft ENTRY."
  (let* ((raw (plist-get entry :raw-output))
         (candidate (plist-get raw :candidate))
         (drafted-item (plist-get raw :drafted-item))
         (candidate-id (and candidate
                            (delib-flow--artifact-candidate-id candidate))))
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run
           'waiting-fors
           candidate-id))
    (delib-flow--replace-artifact-family-selected-draft
     run
     'waiting-fors
     drafted-item)))

(defun delib-flow--apply-draft-selected-reference-note-entry (run entry)
  "Return RUN updated from completed selected-note draft ENTRY."
  (let* ((raw (plist-get entry :raw-output))
         (candidate (plist-get raw :candidate))
         (part-id (plist-get raw :part-id))
         (previous-draft (delib-flow--artifact-family-selected-draft
                          run 'reference-notes))
         (drafted-item
          (delib-flow--reference-note-draft-with-workspace-structure
           (plist-get raw :drafted-item)
           (or candidate (plist-get raw :drafted-item))
           run))
         (candidate-id (and candidate
                            (delib-flow--artifact-candidate-id candidate))))
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run
           'reference-notes
           candidate-id))
    (setq run
          (delib-flow--replace-artifact-family-selected-draft
           run
           'reference-notes
           drafted-item))
    (when part-id
      (setq run
            (delib-flow--reference-note-store-part-outcome
             run
             part-id
             (and previous-draft
                  (delib-flow--reference-note-part-stage-text
                   previous-draft part-id))
             (delib-flow--reference-note-part-stage-text drafted-item part-id)
             (or (plist-get raw :reason)
                 "No reason recorded."))))
    (delib-flow--seed-reference-note-capture-review-block
     run)))

(defun delib-flow--apply-draft-selected-project-entry (run entry)
  "Return RUN updated from completed selected-project draft ENTRY."
  (let* ((raw (plist-get entry :raw-output))
         (candidate (plist-get raw :candidate))
         (drafted-item (copy-tree (plist-get raw :drafted-item)))
         (candidate-id (and candidate
                            (delib-flow--artifact-candidate-id candidate))))
    (when candidate-id
      (setq drafted-item
            (plist-put drafted-item :draft-candidate-id candidate-id)))
    (setq run
          (delib-flow--set-artifact-family-selected-candidate-id
           run
           'project-proposals
           candidate-id))
    (delib-flow--replace-artifact-family-selected-draft
     run
     'project-proposals
     drafted-item)))

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

(provide 'delib-flow-stages)

;;; delib-flow-stages.el ends here
