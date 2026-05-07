;;; delib-flow-filing.el --- Filing helpers for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Filing selection, conflict resolution, target planning, and deterministic
;; filing persistence helpers.

;;; Code:

(require 'org)
(require 'org-capture)
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

(defun delib-flow--reference-note-item-identity (item)
  "Return stable identity text for reference-note ITEM."
  (when (eq (plist-get item :kind) 'reference-note)
    (or (plist-get item :candidate-identity)
        (plist-get item :candidate-focus)
        (delib-flow--reference-note-title item)
        (plist-get item :text))))

(defun delib-flow--same-reference-note-item-p (left right)
  "Return non-nil when LEFT and RIGHT refer to the same reference note."
  (and (eq (plist-get left :kind) 'reference-note)
       (eq (plist-get right :kind) 'reference-note)
       (or (delib-flow--same-artifact-item-p left right)
           (equal (delib-flow--reference-note-item-identity left)
                  (delib-flow--reference-note-item-identity right)))))

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

(defun delib-flow--seed-filing-conflict-resolution-block (run)
  "Return RUN with conflict-resolution block populated from current filing state."
  (let* ((block (delib-flow--editable-block run 'filing-conflict-resolution))
         (updated-block
          (delib-flow--set-editable-block-text
           block
           (delib-flow--filing-conflict-resolution-template run))))
    (delib-flow--set-editable-block run 'filing-conflict-resolution updated-block)))

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
         (remaining-action-candidates
          (seq-filter
           (lambda (item)
             (eq (plist-get item :kind) 'next-action))
           remaining-items))
         (remaining-waiting-for-candidates
          (seq-filter
           (lambda (item)
             (eq (plist-get item :kind) 'waiting-for))
           remaining-items))
         (remaining-project-candidates
          (seq-filter
           (lambda (item)
             (eq (plist-get item :kind) 'project))
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
            (delib-flow--set-artifact-family-candidates
             run 'actions remaining-action-candidates))
      (setq run
            (delib-flow--set-artifact-family-candidates
             run 'waiting-fors remaining-waiting-for-candidates))
      (setq run
            (delib-flow--set-artifact-family-candidates
             run 'reference-notes remaining-note-candidates))
      (setq run
            (delib-flow--set-artifact-family-candidates
             run 'project-proposals remaining-project-candidates))
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

(defun delib-flow--filing-selection-current-value (run labels)
  "Return current chooser value for RUN from LABELS.

Prefer the explicit filing-selection block value when present. Fall back to the
currently active filing item so the focused filing workspace can still drive
selection before the legacy Selection block has been seeded."
  (or (delib-flow--filing-selection-value
       (delib-flow--stage-input-package run 'select-approved-filing-actions))
      (when-let* ((current-item (delib-flow--active-filing-item run))
                  (items (plist-get (plist-get run :filing) :draft-items))
                  (index (seq-position
                          items
                          current-item
                          #'delib-flow--same-artifact-item-p)))
        (number-to-string (1+ index)))
      (cdr (car labels))))

(delib-flow--define-function delib-flow--set-filing-selection-value
			     (run selection)
			     "Return RUN with filing Selection set to SELECTION."
			     (let*
				 ((items
				   (plist-get (plist-get run :filing)
					      :draft-items))
				  (block
				   (delib-flow--editable-block run
							       'filing-selection-review))
				  (base-text
				   (let
				       ((current
					 (delib-flow--filing-selection-block-text
					  run)))
				     (if
					 (delib-flow--non-empty-string-p
					  current)
					 current
				       (delib-flow--filing-selection-template
					items))))
				  (updated-text
				   (delib-flow--replace-selection-line
				    base-text selection))
				  (updated-run
				   (delib-flow--set-editable-block run
								   'filing-selection-review
								   (delib-flow--set-editable-block-text
								    block
								    updated-text)))
				  (selected-item
				   (condition-case nil
				       (delib-flow--filing-selection-choice
					updated-run)
				     (error nil))))
			       (setq updated-run
				     (let
					 ((family
					   (delib-flow--artifact-family-for-item-kind
					    (plist-get selected-item
						       :kind))))
				       (dolist
					   (candidate-family
					    '(actions waiting-fors
						      reference-notes
						      project-proposals)
					    updated-run)
					 (setq updated-run
					       (delib-flow--set-artifact-family-selected-candidate-id
						updated-run
						candidate-family
						(and
						 (eq candidate-family
						     family)
						 (delib-flow--artifact-candidate-id
						  selected-item)))))))
			       (delib-flow--seed-reference-note-capture-review-block
				updated-run)))


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
     "Selecting a ready queue artifact approves only that item and leaves the rest in draft state."
     "Blocked artifacts must be fixed, skipped by choosing a different ready item, or rejected from this run.")))

(delib-flow--define-function delib-flow--filing-target-summary
			     (item run)
			     "Return compact target summary text for filing ITEM in RUN."
			     (let
				 ((locations
				   (condition-case nil
				       (delib-flow--planned-file-location
					item run)
				     (error nil))))
			       (if (null locations) "unavailable"
				 (mapconcat
				  (lambda (location)
				    (let
					((target
					  (plist-get location :target)))
				      (cond
				       ((string-match "::\\(.+\\)\\'"
						      target)
					(match-string 1 target))
				       ((string-match
					 "\\([^/]+\\.org\\)\\'" target)
					(match-string 1 target))
				       (t target))))
				  locations " + "))))


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
          (delib-flow--render-editable-block-with-editor-help run 'filing-selection-review)))

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
             (current-title (delib-flow--reference-note-capture-title-value package))
             (preserve-fields-p
              (and (delib-flow--non-empty-string-p current-title)
                   (equal current-title
                          (delib-flow--reference-note-title item))))
             (template-key
              (or (and preserve-fields-p
                       (let ((value
                              (delib-flow--reference-note-capture-template-key-value
                               package)))
                         (and (delib-flow--non-empty-string-p value) value)))
                  (delib-flow--reference-note-org-roam-template-key item)
                  ""))
             (title (or (delib-flow--reference-note-title item) ""))
             (target-path
              (or (and preserve-fields-p
                       (delib-flow--reference-note-effective-target-override
                        package))
                  "")))
        (concat
         (format "Template key: %s\n" template-key)
         (format "Note title: %s\n" title)
         (format "Target path: %s\n" target-path)
         "\n"
         "Target summary:\n"
         "Use the current template and path unless the save destination is wrong.\n"
         "Actions: change template with `T`; edit path with `Edit Target Path`.\n"))
    "Template key: \nNote title: \nTarget path: \n\nTarget summary:\nNo reference-note filing artifact is currently active."))

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

(delib-flow--define-function
 delib-flow--conflict-resolution-retarget-fields (item)
 "Return editable retarget field block for approved conflict ITEM."
 (pcase (plist-get item :kind)
   ('reference-note
    (concat "New title:\n" "\n" "New text:\n"
	    "[unused for this conflict type]\n"))
   ((or 'next-action 'waiting-for)
    (concat "New text:\n" "\n" "New title:\n"
	    "[unused for this conflict type]\n"))
   ('project
    (concat "New title:\n" "\n" "New text:\n"
	    "[unused for this conflict type]\n"))
   (_
    (concat "New title:\n" "[unused for this conflict type]\n" "\n"
	    "New text:\n" "[unused for this conflict type]\n"))))


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

(delib-flow--define-function
 delib-flow--filing-selection-choice-by-index (selection items)
 "Return selected draft item from SELECTION and draft ITEMS by numeric index."
 (let ((indexes nil) (start 0))
   (while
       (and selection (string-match "\\([0-9]+\\)" selection start))
     (push (string-to-number (match-string 1 selection)) indexes)
     (setq start (match-end 0)))
   (setq indexes (nreverse indexes))
   (when (> (length indexes) 1)
     (error
      "Filing selection must name exactly one artifact index; found: %s"
      (mapconcat #'number-to-string indexes ", ")))
   (when-let ((index (car indexes)))
     (unless (and (> index 0) (<= index (length items)))
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

(defun delib-flow--selected-filing-items (package)
  "Return the operator-selected filing item from PACKAGE."
  (if-let ((item (delib-flow--selected-filing-item package)))
      (if (delib-flow--project-package-selection-p package item)
          (delib-flow--project-package-selected-items package item)
        (list item))
    (when-let ((project (and (delib-flow--project-flow-active-p package)
                             (delib-flow--project-package-root-for-filing package))))
      (delib-flow--project-package-selected-items package project))))

(defun delib-flow--project-package-selection-p (package item)
  "Return non-nil when ITEM should approve as a project package in PACKAGE."
  (and (eq (plist-get item :kind) 'project)
       (delib-flow--project-flow-active-p package)))

(defun delib-flow--project-package-bundle-child-p (item)
  "Return non-nil when ITEM should be bundled into project filing."
  (memq (plist-get item :kind) '(next-action waiting-for)))

(defun delib-flow--project-package-bundle-items (package)
  "Return ready child items that should bundle with a selected project in PACKAGE."
  (let* ((project (delib-flow--project-package-root-for-filing package))
         (root-child-items (and project
                                (delib-flow--project-child-items project)))
         (ready-children (delib-flow--project-package-ready-child-items package)))
    (seq-reduce #'delib-flow--remove-first-matching-item
                root-child-items
                ready-children)))

(defun delib-flow--project-package-selected-items (package project)
  "Return the approved project bundle items for PROJECT in PACKAGE."
  (cons (or (delib-flow--project-package-root-for-filing package)
            project)
        (delib-flow--project-package-bundle-items package)))

(defun delib-flow--item-kind-requires-draft-p (kind)
  "Return non-nil when filing KIND should require item-local drafting first."
  (memq kind '(next-action waiting-for reference-note project)))

(defun delib-flow--selected-family-draft-matches-item-p (package family item)
  "Return non-nil when PACKAGE has a selected FAMILY draft for ITEM."
  (let ((draft (delib-flow--artifact-family-selected-draft package family))
        (selected-id (delib-flow--artifact-family-selected-candidate-id package family)))
    (and draft
         (equal (or selected-id
                    (delib-flow--artifact-candidate-id draft))
                (delib-flow--artifact-candidate-id item)))))

(delib-flow--define-function
 delib-flow--selected-filing-item-draft-warning (package)
 "Return blocking warning when selected filing item in PACKAGE still needs drafting."
 (let
     ((item
       (condition-case nil
	   (delib-flow--filing-selection-choice package)
	 (error nil))))
   (when
       (and item
	    (delib-flow--item-kind-requires-draft-p
	     (plist-get item :kind))
	    (not
	     (pcase (plist-get item :kind)
	       ('next-action
		(delib-flow--selected-family-draft-matches-item-p
		 package 'actions item))
	       ('waiting-for
		(delib-flow--selected-family-draft-matches-item-p
		 package 'waiting-fors item))
	       ('reference-note
		(delib-flow--selected-family-draft-matches-item-p
		 package 'reference-notes item))
	       ('project
		(delib-flow--selected-family-draft-matches-item-p
		 package 'project-proposals item))
	       (_ t))))
     (delib-flow--make-artifact-warning 'selected-item-needs-draft
					(format
					 "This selected %s still needs an item-local draft before it can be approved for filing."
					 (pcase (plist-get item :kind)
					   ('next-action "action")
					   ('waiting-for "waiting-for")
					   ('reference-note "note")
					   ('project "project")
					   (_ "artifact")))
					'blocking))))


(defun delib-flow--selected-filing-item-blocking-warnings (package)
  "Return blocking warnings on the operator-selected filing item from PACKAGE."
  (if-let ((item (delib-flow--selected-filing-item package)))
      (delq nil
            (append
             (delib-flow--draft-item-blocking-warnings item)
             (list (delib-flow--selected-filing-item-draft-warning package))))
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
  (if-let ((selected-items (delib-flow--selected-filing-items package)))
      (let* ((selected-item (delib-flow--selected-filing-item package))
             (removal-items
              (if (delib-flow--project-package-selection-p package selected-item)
                  (append selected-items
                          (delib-flow--project-child-items
                           (delib-flow--project-package-root-for-filing package)))
                selected-items)))
        (seq-reduce #'delib-flow--remove-first-matching-item
                    removal-items
                    (delib-flow--draft-items package)))
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

(delib-flow--define-function
 delib-flow--inferred-conflict-resolution-keyword (package keyword)
 "Return inferred filing conflict KEYWORD for PACKAGE when operator edits imply one.\nThis keeps the editable block forgiving when the operator changes the retarget\nfield but leaves the default `Resolution: RETRY` line untouched."
 (let*
     ((item (delib-flow--approved-filing-item package))
      (kind (plist-get item :kind))
      (has-title
       (delib-flow--non-empty-string-p
	(delib-flow--filing-conflict-resolution-new-title package)))
      (has-text
       (delib-flow--non-empty-string-p
	(delib-flow--filing-conflict-resolution-new-text package))))
   (if (not (string-equal keyword "RETRY")) keyword
     (cond
      ((and has-text (not has-title)
	    (memq kind '(next-action waiting-for)))
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

(delib-flow--define-function
 delib-flow--select-approved-filing-actions-result (package)
 "Return raw filing-selection result for PACKAGE."
 (let*
     ((selection (delib-flow--filing-selection-value package))
      (notes (delib-flow--filing-selection-notes package))
      (state
       (delib-flow--select-approved-filing-actions-state package))
      (selected-item (plist-get state :selected-item))
      (blocking-warnings (plist-get state :blocking-warnings))
      (ready-indexes
       (delib-flow--ready-filing-selection-indexes package))
      (blocked-indexes
       (delib-flow--blocked-filing-selection-indexes package))
      (approval-blocked-p (plist-get state :approval-blocked-p))
      (selected-items (plist-get state :selected-items))
      (remaining-items (plist-get state :remaining-items))
      (planned-target-locations
       (unless approval-blocked-p
	 (condition-case nil
	     (mapcan
	      (lambda (item)
		(delib-flow--planned-file-location item package))
	      selected-items)
	   (error nil)))))
   (list :approved-items selected-items :approval-blocked-p
	 approval-blocked-p :blocking-warnings blocking-warnings
	 :ready-selection-indexes ready-indexes
	 :blocked-selection-indexes blocked-indexes
	 :remaining-draft-items remaining-items :selected-count
	 (length selected-items) :remaining-draft-count
	 (length remaining-items) :operator-selection selection
	 :operator-notes notes :blocked-item
	 (and approval-blocked-p selected-item) :blocked-item-preview
	 (and selected-item approval-blocked-p
	      (delib-flow--draft-item-preview-text
	       (list selected-item)))
	 :selected-preview
	 (and selected-items
	      (delib-flow--draft-item-preview-text selected-items))
	 :planned-target-locations planned-target-locations :reason
	 (if approval-blocked-p
	     "Selected filing artifact still has blocking warnings and cannot be approved yet. Approve a different ready artifact, fix the blocking warnings, or reject this artifact before retrying approval."
           (if (and selected-item
                    (delib-flow--project-package-selection-p package selected-item))
               "Approved the selected project package bundle for deterministic filing review."
	     "Approved the operator-selected filing artifact for deterministic filing review.")))))


(defun delib-flow--execute-select-approved-filing-actions (package)
  "Return raw filing-selection output for PACKAGE."
  (delib-flow--select-approved-filing-actions-result package))

(defun delib-flow--execute-resolve-filing-conflict (package)
  "Return raw conflict-resolution output for PACKAGE."
  (delib-flow--resolve-filing-conflict-result package))

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

(defun delib-flow--approved-project-package-p (package)
  "Return non-nil when PACKAGE currently holds an approved project bundle."
  (seq-some (lambda (item)
              (eq (plist-get item :kind) 'project))
            (delib-flow--approved-items package)))

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

(defun delib-flow--effective-project-item (package)
  "Return the effective project context item from PACKAGE."
  (or (delib-flow--matched-project package)
      (delib-flow--selected-project-draft-from-package package)))

(defun delib-flow--effective-project-context-kind (package)
  "Return the source of effective project context in PACKAGE."
  (cond
   ((delib-flow--matched-project package) 'matched-project)
   ((delib-flow--selected-project-draft-from-package package)
    'proposed-project-draft)
   (t nil)))

(defun delib-flow--effective-project-title (package)
  "Return effective project title from PACKAGE."
  (plist-get (delib-flow--effective-project-item package) :title))

(defun delib-flow--approved-project-item (package)
  "Return the approved project item from PACKAGE, if any."
  (seq-find (lambda (item)
              (eq (plist-get item :kind) 'project))
            (plist-get (plist-get package :filing) :approved-items)))

(defun delib-flow--draft-project-item (package)
  "Return the best draft project item from PACKAGE, if any."
  (or (delib-flow--selected-project-draft-from-package package)
      (delib-flow--selected-project-candidate-for-drafting package)
      (seq-find (lambda (item)
                  (eq (plist-get item :kind) 'project))
                (delib-flow--draft-items package))
      (seq-find (lambda (item)
                  (eq (plist-get item :kind) 'project))
                (delib-flow--preview-selected-filing-items package))))

(defun delib-flow--current-project-package-item (package)
  "Return the best current project package item from PACKAGE."
  (or (delib-flow--approved-project-item package)
      (delib-flow--draft-project-item package)))

(defun delib-flow--project-proposal-goal-like-first-item-p (item)
  "Return non-nil when ITEM reads like a project goal instead of a next action."
  (let ((text (downcase (string-trim (or (plist-get item :text) "")))))
    (or (string-prefix-p "investigate and fix " text)
        (string-prefix-p "fix " text)
        (string-prefix-p "address " text))))

(defun delib-flow--project-package-ready-child-items (package)
  "Return ready child items that can file with the active project package in PACKAGE."
  (seq-filter
   (lambda (item)
     (and (delib-flow--project-package-bundle-child-p item)
          (delib-flow--draft-item-ready-p item)
          (null (delib-flow--draft-item-blocking-warnings item))))
   (delib-flow--draft-items package)))

(defun delib-flow--project-package-merge-child-items (project package)
  "Return child items that should stay attached to PROJECT in PACKAGE."
  (let* ((existing (copy-tree (delib-flow--project-child-items project)))
         (ready (copy-tree (delib-flow--project-package-ready-child-items package)))
         (first-item (car existing)))
    (when (and first-item
               (delib-flow--project-proposal-goal-like-first-item-p first-item)
               ready)
      (setq existing (append (cdr existing) ready))
      (setq ready nil))
    (seq-reduce
     (lambda (items child)
       (if (seq-some (lambda (item)
                       (delib-flow--same-artifact-item-p item child))
                     items)
           items
         (append items (list child))))
     ready
     existing)))

(defun delib-flow--project-package-expanded-items (project extra-items)
  "Return PROJECT package items with PROJECT children and EXTRA-ITEMS expanded."
  (when project
    (let ((items (list project)))
      (dolist (child (delib-flow--project-child-items project))
        (unless (seq-some (lambda (item)
                            (delib-flow--same-artifact-item-p item child))
                          items)
          (setq items (append items (list child)))))
      (dolist (item extra-items)
        (unless (or (delib-flow--same-artifact-item-p item project)
                    (seq-some (lambda (existing)
                                (delib-flow--same-artifact-item-p existing item))
                              items))
          (setq items (append items (list item)))))
      items)))

(defun delib-flow--project-package-reviewable-child-p (item)
  "Return non-nil when ITEM counts as real assembled package work."
  (and (memq (plist-get item :kind) '(next-action waiting-for))
       (not (eq (plist-get item :source) 'project-proposal))))

(defun delib-flow--project-package-assembled-p (package)
  "Return non-nil when PACKAGE has an assembled project bundle worth reviewing."
  (when (delib-flow--project-package-root-for-filing package)
    (or (delib-flow--project-package-ready-child-items package)
        (seq-some (lambda (item)
                    (delib-flow--project-package-reviewable-child-p item))
                  (seq-remove (lambda (item)
                                (eq (plist-get item :kind) 'project))
                              (delib-flow--project-package-included-items package))))))

(defun delib-flow--project-package-root-for-filing (package)
  "Return the effective project root item for filing PACKAGE."
  (if-let ((project (delib-flow--current-project-package-item package)))
      (if-let ((child-items
                (delib-flow--project-package-merge-child-items project package)))
          (delib-flow--project-with-child-items project child-items)
        project)
    nil))

(defun delib-flow--filing-project-title (package)
  "Return the project title that filing should use in PACKAGE."
  (or (plist-get (delib-flow--project-package-root-for-filing package) :title)
      (delib-flow--matched-project-title package)
      (delib-flow--effective-project-title package)))

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

(defun delib-flow--project-item-created-timestamp ()
  "Return deterministic CREATED timestamp text for newly filed project items."
  (format-time-string "%Y-%m-%d %a %H:%M" (current-time)))

(defun delib-flow--project-item-properties-text (item)
  "Return property drawer text for filed project child ITEM."
  (let ((tags (or (plist-get item :tags)
                  (plist-get item :tag-suggestions)))
        (source (or (plist-get item :source)
                    (plist-get item :text))))
    (concat
     ":PROPERTIES:\n"
     (when tags
       (format ":TAGS: %s\n" (string-join tags " ")))
     (format ":CREATED: %s\n" (delib-flow--project-item-created-timestamp))
     (when source
       (format ":SOURCE_ARTIFACT: %s\n" source))
     ":END:\n")))

(defun delib-flow--project-item-entry (item level)
  "Return full Org entry text for project child ITEM at LEVEL."
  (concat
   (delib-flow--project-item-heading item level)
   (delib-flow--project-item-properties-text item)))

(defun delib-flow--project-heading-text (item &optional level)
  "Return Org heading text for project ITEM at LEVEL."
  (format "%s %s\n"
          (make-string (or level 1) ?*)
          (plist-get item :title)))

(defun delib-flow--project-first-item (item)
  "Return first child item from project ITEM."
  (car (delib-flow--project-child-items item)))

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

(defun delib-flow-capture-project-first-item-entry ()
  "Return current capture new-project first child entry with metadata."
  (plist-get delib-flow--capture-context :project-first-item-entry))

(defun delib-flow-capture-project-tags ()
  "Return current capture project tags as a space-separated string."
  (string-join
   (or (plist-get delib-flow--capture-context :project-tags) '())
   " "))

(defun delib-flow-capture-project-tags-property ()
  "Return capture property lines for current project tags."
  (if-let ((tags (plist-get delib-flow--capture-context :project-tags)))
      (if tags
          (concat
           ":PROPERTIES:\n"
           (format ":TAGS: %s\n" (string-join tags " "))
           (format ":CREATED: %s\n" (delib-flow--project-item-created-timestamp))
           ":END:\n")
        "")
    ""))

(defun delib-flow-capture-project-child-properties ()
  "Return capture property lines for the current project child item."
  (or (plist-get delib-flow--capture-context :project-child-properties)
      ""))

(defun delib-flow--capture-template-interactive-p (template)
  "Return non-nil when capture TEMPLATE would prompt interactively."
  (or (string-match-p "%\\^" template)
      (string-match-p "%\\?" template)))

(defun delib-flow--validate-capture-template (template)
  "Signal an error when capture TEMPLATE is unsupported for delib-flow filing."
  (when (delib-flow--capture-template-interactive-p template)
    (error "delib-flow filing capture templates must be non-interactive; remove %%^ prompts and %%? cursor markers")))

(delib-flow--define-function delib-flow--capture-template-context
			     (item package &optional project-title
				   level)
			     "Return dynamic capture context for ITEM from PACKAGE.\nPROJECT-TITLE and LEVEL describe deterministic project filing targets when\nrelevant."
			     (list :item item :package package
				   :item-text (plist-get item :text)
				   :project-title
				   (or project-title
				       (delib-flow--matched-project-title
					package)
				       (plist-get item :title))
				   :note-title
				   (and
				    (eq (plist-get item :kind)
					'reference-note)
				    (delib-flow--reference-note-title
				     item))
				   :note-type
				   (and
				    (eq (plist-get item :kind)
					'reference-note)
				    (symbol-name
				     (plist-get item :note-type)))
				   :source-artifact
				   (plist-get item :text)
				   :project-child-heading
				   (and level
					(not
					 (eq (plist-get item :kind)
					     'project))
					(string-trim-right
					 (delib-flow--project-item-heading
					  item level)))
                                   :project-child-properties
                                   (and level
                                        (not
                                         (eq (plist-get item :kind)
                                             'project))
                                        (delib-flow--project-item-properties-text
                                         item))
				   :project-tags
				   (and
				    (eq (plist-get item :kind)
					'project)
				    (or (plist-get item :tags)
					(plist-get item
						   :tag-suggestions)))
				   :project-heading
				   (and
				    (eq (plist-get item :kind)
					'project)
				    (string-trim-right
				     (delib-flow--project-heading-text
				      item (or level 1))))
				   :project-first-item-heading
				   (and
				    (eq (plist-get item :kind)
					'project)
				    (let
					((first
					  (delib-flow--project-first-item
					   item)))
				      (when first
					(string-trim-right
					 (delib-flow--project-item-heading
					  first (1+ (or level 1)))))))
                                   :project-first-item-entry
                                   (and
                                    (eq (plist-get item :kind)
                                        'project)
                                    (let ((first
                                           (delib-flow--project-first-item
                                            item)))
                                      (when first
                                        (string-trim-right
                                         (delib-flow--project-item-entry
                                          first (1+ (or level 1)))))))))


(defun delib-flow--fill-capture-template (template context)
  "Return capture TEMPLATE expanded against CONTEXT."
  (delib-flow--validate-capture-template template)
  (let ((delib-flow--capture-context context)
        (org-capture-plist (list :default-time (current-time)))
        (org-store-link-plist nil))
    (org-capture-fill-template template)))

(defun delib-flow--normalized-project-capture-template (template)
  "Return PROJECT capture TEMPLATE upgraded from legacy heading-only forms."
  (cond
   ((string-match-p "delib-flow-capture-project-first-item-entry" template)
    template)
   ((string-match-p "delib-flow-capture-project-first-item-heading" template)
    (replace-regexp-in-string
     "%(delib-flow-capture-project-first-item-heading)"
     "%(delib-flow-capture-project-first-item-entry)"
     template t t))
   (t template)))

(defun delib-flow--normalized-project-child-capture-template (template)
  "Return project-child TEMPLATE upgraded from legacy heading-only forms."
  (cond
   ((string-match-p "delib-flow-capture-project-child-properties" template)
    template)
   ((string-match-p "delib-flow-capture-project-child-heading" template)
    (replace-regexp-in-string
     "%(delib-flow-capture-project-child-heading)"
     "%(delib-flow-capture-project-child-heading)\n%(delib-flow-capture-project-child-properties)"
     template t t))
   (t template)))

(defun delib-flow--project-item-capture-template (item)
  "Return configured capture template for project filing ITEM."
  (pcase (plist-get item :kind)
    ('project
     (delib-flow--normalized-project-capture-template
      delib-flow-project-capture-template))
    ('waiting-for
     (delib-flow--normalized-project-child-capture-template
      delib-flow-waiting-for-capture-template))
    (_
     (delib-flow--normalized-project-child-capture-template
      delib-flow-next-action-capture-template))))

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
           (delib-flow--project-state-heading-regexp state)
           nil t)
          2
        1))))

(defun delib-flow--goto-project-state-bucket (state)
  "Move point to the top-level bucket heading for project STATE."
  (goto-char (point-min))
  (unless (re-search-forward
           (delib-flow--project-state-heading-regexp state)
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
       (dolist (child (delib-flow--project-additional-child-items item))
         (unless (bolp)
           (insert "\n"))
         (insert (delib-flow--project-item-entry child (1+ level))))
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

(delib-flow--define-function delib-flow--action-draft-preview-items
			     (run)
			     "Return next-action items that should be previewed in filing for RUN."
			     (or
			      (when-let
				  ((draft
				    (delib-flow--artifact-family-selected-draft
				     run 'actions)))
				(when
				    (eq (plist-get draft :kind)
					'next-action)
				  (list draft)))
			      (when-let
				  ((selected
				    (delib-flow--selected-action-candidate-for-drafting
				     run)))
				(when
				    (eq (plist-get selected :kind)
					'next-action)
				  (list selected)))
			      (let
				  ((planned
				    (delib-flow--planned-file-preview-items
				     run)))
				(when
				    (and planned
					 (= (length planned) 1)
					 (eq
					  (plist-get (car planned)
						     :kind)
					  'next-action))
				  planned))
			      (seq-take
			       (seq-filter
				(lambda (item)
				  (eq (plist-get item :kind)
				      'next-action))
				(plist-get (plist-get run :filing)
					   :draft-items))
			       2)))


(defun delib-flow--action-flow-active-p (run)
  "Return non-nil when RUN is in a selected-action drafting loop."
  (or (delib-flow--artifact-family-selected-draft run 'actions)
      (delib-flow--selected-action-candidate-for-drafting run)
      (seq-some (lambda (item)
                  (eq (plist-get item :kind) 'next-action))
                (plist-get (plist-get run :filing) :draft-items))
      (eq (plist-get (delib-flow--active-filing-item run) :kind) 'next-action)))

(defun delib-flow--selected-action-drafted-p (run)
  "Return non-nil when RUN has a drafted selected action."
  (eq (plist-get (delib-flow--artifact-family-selected-draft run 'actions)
                 :kind)
      'next-action))

(defun delib-flow--action-workflow-status (run)
  "Return status text for the selected-action workflow in RUN."
  (cond
   ((delib-flow--selected-action-drafted-p run)
    "This action loop is active. Review the drafted wording, regenerate only this action if needed, then approve and file it.")
   ((delib-flow--selected-action-candidate-for-drafting run)
    "This action loop is active. One action is selected; draft it next if you want wording help before approval.")
   ((delib-flow--action-flow-active-p run)
    "This action loop is active. Choose one action candidate, then draft, approve, and file that one action.")
   (t
    "No selected-action workflow is active right now.")))

(defun delib-flow--action-workflow-text (run)
  "Return step-by-step selected-action workflow text for RUN."
  (cond
   ((delib-flow--selected-action-drafted-p run)
    (string-join
     '("1. Review the selected action draft below."
       "2. If the wording is weak, run `Regenerate Selected Action` for a fresh pass on this action only."
       "3. When the wording looks right, approve it, then file it.")
     "\n"))
   ((delib-flow--selected-action-candidate-for-drafting run)
    (string-join
     '("1. The queue selection already points at one action candidate."
       "2. Run `Draft Selected Action` to tighten only that action."
       "3. Then approve and file the drafted action.")
     "\n"))
   ((delib-flow--action-flow-active-p run)
    (string-join
     '("1. Choose one ready action from `Choose from queue`, or press `s` to pick with completion."
       "2. Run `Draft Selected Action` to deepen only that action instead of the whole queue."
       "3. Approve and file the drafted action when it reads clearly.")
     "\n"))
   (t
    "Run `Extract Actions` to start an action-drafting loop here.")))

(defun delib-flow--action-draft-preview-status (run)
  "Return status text for selected-action draft previews in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'actions)
    "This is the current drafted wording for the selected action. Review it before approval or filing.")
   ((delib-flow--selected-action-candidate-for-drafting run)
    "This is the selected action candidate. Draft it first if you want delib-flow to clarify the wording before approval.")
   ((delib-flow--action-draft-preview-items run)
    "Review these action candidates before selecting one to draft.")
   (t
    "No selected-action draft previews are available from the current queue.")))

(defun delib-flow--action-preview-fragment (item)
  "Return action-preview fragment for ITEM."
  (format
   "**** %s\n- Draft preview: this is the wording delib-flow will use if you approve and file this action.\n- Warnings: %s\n- Suggested tags: %s"
   (or (plist-get item :text) "Untitled action")
   (or (and-let* ((warnings (plist-get item :warnings))
                  ((> (length warnings) 0)))
         (mapconcat (lambda (warning)
                      (plist-get warning :message))
                    warnings
                    "; "))
       "none")
   (or (and-let* ((tags (plist-get item :tag-suggestions))
                  ((> (length tags) 0)))
         (mapconcat #'identity tags ", "))
       "none")))

(defun delib-flow--action-draft-preview-text (run)
  "Return filing-loop selected-action draft preview text for RUN."
  (if-let ((items (delib-flow--action-draft-preview-items run)))
      (mapconcat #'delib-flow--action-preview-fragment items "\n\n")
    "Run Extract Actions to generate action candidates here."))

(defun delib-flow--action-regeneration-status (run)
  "Return status text for selected-action regeneration guidance in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'actions)
    "Use this when the selected action needs clearer wording, tighter scope, or a better action verb.")
   ((delib-flow--selected-action-candidate-for-drafting run)
    "Draft the selected action before you approve or file it.")
   (t
    "No selected-action regeneration guidance is needed right now.")))

(defun delib-flow--action-regeneration-text (run)
  "Return selected-action regeneration guidance text for RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'actions)
    (string-join
     '("- Run `Regenerate Selected Action` to ask the LLM for a fresh pass on this action only."
       "- This replaces only the selected action draft. It does not replace the rest of the action queue."
       "- If the underlying context changed, rerun `Filter Useful Reference Material` or `Discover Relevant Reference Material`, then regenerate the selected action.")
     "\n"))
   ((delib-flow--selected-action-candidate-for-drafting run)
    (string-join
     '("- Run `Draft Selected Action` to tighten this selected action before approval."
       "- That draft becomes the version delib-flow will preview, approve, and file for this selected action."
       "- If this is the wrong action to deepen, choose a different action in the queue first.")
     "\n"))
   ((delib-flow--action-draft-preview-items run)
    (string-join
     '("- Choose one action candidate in the queue first."
       "- After selection, use `Draft Selected Action` to clarify only that action instead of regenerating the whole action queue.")
     "\n"))
   (t
    "No action drafts are currently active.")))

(delib-flow--define-function
 delib-flow--waiting-for-draft-preview-items (run)
 "Return waiting-for items that should be previewed in filing for RUN."
 (or
  (when-let
      ((draft
	(delib-flow--artifact-family-selected-draft run 'waiting-fors)))
    (when (eq (plist-get draft :kind) 'waiting-for) (list draft)))
  (when-let
      ((selected
	(delib-flow--selected-waiting-for-candidate-for-drafting run)))
    (when (eq (plist-get selected :kind) 'waiting-for)
      (list selected)))
  (let ((planned (delib-flow--planned-file-preview-items run)))
    (when
	(and planned (= (length planned) 1)
	     (eq (plist-get (car planned) :kind) 'waiting-for))
      planned))
  (seq-take
   (seq-filter
    (lambda (item) (eq (plist-get item :kind) 'waiting-for))
    (plist-get (plist-get run :filing) :draft-items))
   2)))


(defun delib-flow--waiting-for-flow-active-p (run)
  "Return non-nil when RUN is in a selected waiting-for drafting loop."
  (or (delib-flow--artifact-family-selected-draft run 'waiting-fors)
      (delib-flow--selected-waiting-for-candidate-for-drafting run)
      (seq-some (lambda (item)
                  (eq (plist-get item :kind) 'waiting-for))
                (plist-get (plist-get run :filing) :draft-items))
      (eq (plist-get (delib-flow--active-filing-item run) :kind) 'waiting-for)))

(defun delib-flow--selected-waiting-for-drafted-p (run)
  "Return non-nil when RUN has a drafted selected waiting-for."
  (eq (plist-get (delib-flow--artifact-family-selected-draft run 'waiting-fors)
                 :kind)
      'waiting-for))

(defun delib-flow--waiting-for-workflow-status (run)
  "Return status text for the selected waiting-for workflow in RUN."
  (cond
   ((delib-flow--selected-waiting-for-drafted-p run)
    "This waiting-for loop is active. Review the drafted dependency wording, regenerate only this waiting-for if needed, then approve and file it.")
   ((delib-flow--selected-waiting-for-candidate-for-drafting run)
    "This waiting-for loop is active. One waiting-for is selected; draft it next if you want clearer owner or blocker wording before approval.")
   ((delib-flow--waiting-for-flow-active-p run)
    "This waiting-for loop is active. Choose one waiting-for candidate, then draft, approve, and file that one dependency.")
   (t
    "No selected waiting-for workflow is active right now.")))

(defun delib-flow--waiting-for-workflow-text (run)
  "Return step-by-step selected waiting-for workflow text for RUN."
  (cond
   ((delib-flow--selected-waiting-for-drafted-p run)
    (string-join
     '("1. Review the selected waiting-for draft below."
       "2. If the owner or blocked outcome is weak, run `Regenerate Selected Waiting-For` for a fresh pass on this dependency only."
       "3. When the wording looks right, approve it, then file it.")
     "\n"))
   ((delib-flow--selected-waiting-for-candidate-for-drafting run)
    (string-join
     '("1. The queue selection already points at one waiting-for candidate."
       "2. Run `Draft Selected Waiting-For` to tighten only that dependency."
       "3. Then approve and file the drafted waiting-for.")
     "\n"))
   ((delib-flow--waiting-for-flow-active-p run)
    (string-join
     '("1. Choose one ready waiting-for from `Choose from queue`, or press `s` to pick with completion."
       "2. Run `Draft Selected Waiting-For` to deepen only that dependency instead of the whole queue."
       "3. Approve and file the drafted waiting-for when the owner and blocker read clearly.")
     "\n"))
   (t
    "Run `Extract Waiting-For` to start a waiting-for drafting loop here.")))

(defun delib-flow--waiting-for-draft-preview-status (run)
  "Return status text for selected waiting-for draft previews in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'waiting-fors)
    "This is the current drafted wording for the selected waiting-for. Review it before approval or filing.")
   ((delib-flow--selected-waiting-for-candidate-for-drafting run)
    "This is the selected waiting-for candidate. Draft it first if you want delib-flow to clarify the owner or blocked outcome before approval.")
   ((delib-flow--waiting-for-draft-preview-items run)
    "Review these waiting-for candidates before selecting one to draft.")
   (t
    "No selected waiting-for draft previews are available from the current queue.")))

(defun delib-flow--waiting-for-preview-fragment (item)
  "Return waiting-for preview fragment for ITEM."
  (format
   "**** %s\n- Draft preview: this is the wording delib-flow will use if you approve and file this waiting-for.\n- Warnings: %s\n- Suggested tags: %s"
   (or (plist-get item :text) "Untitled waiting-for")
   (or (and-let* ((warnings (plist-get item :warnings))
                  ((> (length warnings) 0)))
         (mapconcat (lambda (warning)
                      (plist-get warning :message))
                    warnings
                    "; "))
       "none")
   (or (and-let* ((tags (plist-get item :tag-suggestions))
                  ((> (length tags) 0)))
         (mapconcat #'identity tags ", "))
       "none")))

(defun delib-flow--waiting-for-draft-preview-text (run)
  "Return filing-loop selected waiting-for draft preview text for RUN."
  (if-let ((items (delib-flow--waiting-for-draft-preview-items run)))
      (mapconcat #'delib-flow--waiting-for-preview-fragment items "\n\n")
    "Run Extract Waiting-For to generate waiting-for candidates here."))

(defun delib-flow--waiting-for-regeneration-status (run)
  "Return status text for selected waiting-for regeneration guidance in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'waiting-fors)
    "Use this when the selected waiting-for needs a clearer owner, outcome, or blocked dependency wording.")
   ((delib-flow--selected-waiting-for-candidate-for-drafting run)
    "Draft the selected waiting-for before you approve or file it.")
   (t
    "No selected waiting-for regeneration guidance is needed right now.")))

(defun delib-flow--waiting-for-regeneration-text (run)
  "Return selected waiting-for regeneration guidance text for RUN."
  (cond
   ((delib-flow--selected-waiting-for-drafted-p run)
    (string-join
     '("- Run `Regenerate Selected Waiting-For` to ask the LLM for a fresh pass on this waiting-for only."
       "- This replaces only the selected waiting-for draft. It does not replace the rest of the waiting-for queue."
       "- If the underlying context changed, rerun `Filter Useful Reference Material` or `Discover Relevant Reference Material`, then regenerate the selected waiting-for.")
     "\n"))
   ((delib-flow--selected-waiting-for-candidate-for-drafting run)
    (string-join
     '("- Run `Draft Selected Waiting-For` to tighten this selected waiting-for before approval."
       "- That draft becomes the version delib-flow will preview, approve, and file for this selected waiting-for."
       "- If this is the wrong dependency to deepen, choose a different waiting-for in the queue first.")
     "\n"))
   ((delib-flow--waiting-for-draft-preview-items run)
    (string-join
     '("- Choose one waiting-for candidate in the queue first."
       "- After selection, use `Draft Selected Waiting-For` to clarify only that dependency instead of regenerating the whole waiting-for queue.")
     "\n"))
   (t
    "No waiting-for drafts are currently active.")))

(delib-flow--define-function delib-flow--project-draft-preview-items
			     (run)
			     "Return project items that should be previewed in filing for RUN."
			     (or
			      (when-let
				  ((draft
				    (delib-flow--artifact-family-selected-draft
				     run 'project-proposals)))
				(when
				    (eq (plist-get draft :kind)
					'project)
				  (list draft)))
			      (when-let
				  ((selected
				    (delib-flow--selected-project-candidate-for-drafting
				     run)))
				(when
				    (eq (plist-get selected :kind)
					'project)
				  (list selected)))
			      (let
				  ((planned
				    (delib-flow--planned-file-preview-items
				     run)))
				(when
				    (and planned
					 (= (length planned) 1)
					 (eq
					  (plist-get (car planned)
						     :kind)
					  'project))
				  planned))
			      (seq-take
			       (seq-filter
				(lambda (item)
				  (eq (plist-get item :kind) 'project))
				(plist-get (plist-get run :filing)
					   :draft-items))
			       2)))

(defun delib-flow--project-followon-extraction-present-p (run)
  "Return non-nil when RUN already holds extracted follow-on project items."
  (or (delib-flow--action-flow-active-p run)
      (delib-flow--waiting-for-flow-active-p run)
      (delib-flow--reference-note-workspace-active-p run)))

(defun delib-flow--project-workflow-current-step (run)
  "Return the current guided project workflow step for RUN."
  (cond
   ((delib-flow--project-extraction-soft-warning run)
    'review-package)
   ((delib-flow--approved-project-package-p run)
    'review-package)
   ((delib-flow--project-package-assembled-p run)
    (if (delib-flow--project-followon-extraction-present-p run)
        'draft-items
      'review-package))
   ((delib-flow--selected-project-drafted-p run)
    (if (delib-flow--project-followon-extraction-present-p run)
        'draft-items
      'extract-work))
   ((delib-flow--selected-project-candidate-for-drafting run)
    'draft-project)
   ((delib-flow--project-flow-active-p run)
    'propose)
   (t
    'propose)))

(defun delib-flow--project-workflow-next-action-label (run)
  "Return the primary next action label for the active project loop in RUN."
  (pcase (delib-flow--project-workflow-current-step run)
    ('draft-project "Draft Selected Project")
    ('extract-work "Extract Actions / Waiting / Notes")
    ('review-package
     (or (delib-flow--project-refinement-action-label run)
         "Review package"))
    ('draft-items
     (cond
      ((delib-flow--selected-action-candidate-for-drafting run)
       "Draft Selected Action")
      ((delib-flow--selected-waiting-for-candidate-for-drafting run)
       "Draft Selected Waiting-For")
      ((delib-flow--selected-reference-note-candidate-for-drafting run)
       "Draft Selected Note")
      ((or (delib-flow--selected-action-drafted-p run)
           (delib-flow--selected-waiting-for-drafted-p run)
           (delib-flow--reference-note-selected-draft run))
       "Review package")
      (t
       "Choose extracted item")))
    (_ "Propose New Project")))

(defun delib-flow--project-workflow-step-lines (run)
  "Return ordered workflow step lines for the selected project loop in RUN."
  (let ((current (delib-flow--project-workflow-current-step run)))
    (mapcar
     (lambda (step)
       (format "- [%s] %s"
               (if (eq (car step) current) "now" " ")
               (cdr step)))
     '((propose . "1 Propose project with one high-value first action")
       (draft-project . "2 Draft project and adjust title, state, tags, or seed action")
       (extract-work . "3 Extract next actions, waiting-fors, and reference notes")
       (draft-items . "4 Draft selected extracted items")
       (review-package . "5 Review package")
       (file . "6 File project")))))

(defun delib-flow--project-flow-active-p (run)
  "Return non-nil when RUN is in a selected-project drafting loop."
  (or (delib-flow--artifact-family-selected-draft run 'project-proposals)
      (delib-flow--selected-project-candidate-for-drafting run)
      (seq-some (lambda (item)
                  (eq (plist-get item :kind) 'project))
                (plist-get (plist-get run :filing) :draft-items))
      (seq-some (lambda (item)
                  (eq (plist-get item :kind) 'project))
                (plist-get (plist-get run :filing) :approved-items))
      (seq-some (lambda (item)
                  (eq (plist-get item :kind) 'project))
                (delib-flow--preview-selected-filing-items run))))

(defun delib-flow--selected-project-drafted-p (run)
  "Return non-nil when RUN has a drafted selected project."
  (eq (plist-get (delib-flow--artifact-family-selected-draft run 'project-proposals)
                 :kind)
      'project))

(defun delib-flow--project-extraction-context-ready-p (run)
  "Return non-nil when RUN can extract follow-on work from project context."
  (or (and (delib-flow--project-decision-ready-p run)
           (eq (delib-flow--match-status run) 'matched))
      (delib-flow--selected-project-drafted-p run)))

(defun delib-flow--project-extraction-blocked-message (run kind)
  "Return blocked extraction message for RUN and item KIND."
  (let ((label (if (eq kind 'waiting-for)
                   "waiting-for items"
                 "actions")))
    (cond
     ((delib-flow--selected-project-candidate-for-drafting run)
      (format "Draft the selected project package before extracting %s" label))
     (t
      (format "Accept or manually match a project, or draft a selected proposed project package, before extracting %s"
              label)))))

(defun delib-flow--project-context-status-line (run)
  "Return a compact status line describing project context for RUN."
  (pcase (delib-flow--effective-project-context-kind run)
    ('matched-project
     (format "- Project context: matched existing project `%s`."
             (delib-flow--effective-project-title run)))
    ('proposed-project-draft
     (format "- Project context: drafted proposed project package `%s` (original match remains %s)."
             (delib-flow--effective-project-title run)
             (or (delib-flow--match-status run) 'unknown)))
    (_ "- Project context: none yet.")))

(defun delib-flow--stage-history-entries-for-stage (run stage-id)
  "Return stage-history entries for STAGE-ID in RUN."
  (seq-filter
   (lambda (entry)
     (eq (plist-get entry :stage-id) stage-id))
   (plist-get (delib-flow--run-stage-history run) :entries)))

(defun delib-flow--extract-stage-items-from-entry (entry stage-id)
  "Return extracted items from ENTRY for STAGE-ID."
  (let ((raw-output (plist-get entry :raw-output)))
    (pcase stage-id
      ('extract-actions (plist-get raw-output :actions))
      ('extract-waiting-for (plist-get raw-output :waiting-fors))
      (_ nil))))

(defun delib-flow--draft-item-signature (item)
  "Return normalized text signature for draft ITEM."
  (downcase (string-trim (or (plist-get item :text) ""))))

(defun delib-flow--extract-stage-signature (entry stage-id)
  "Return normalized item-text signature list for ENTRY STAGE-ID."
  (mapcar #'delib-flow--draft-item-signature
          (delib-flow--extract-stage-items-from-entry entry stage-id)))

(defun delib-flow--extract-stage-warning-count (entry)
  "Return warning count from extract stage ENTRY."
  (or (plist-get (plist-get entry :raw-output) :warning-count) 0))

(defun delib-flow--extract-stage-weak-p (entry stage-id)
  "Return non-nil when ENTRY for STAGE-ID appears weak."
  (let ((items (delib-flow--extract-stage-items-from-entry entry stage-id)))
    (or (> (delib-flow--extract-stage-warning-count entry) 0)
        (and items
             (seq-every-p
              (lambda (item)
                (> (length (delib-flow--draft-item-warnings item)) 0))
              items)))))

(defun delib-flow--extract-stage-repetitive-p (entries stage-id)
  "Return non-nil when latest two ENTRIES for STAGE-ID are repetitive."
  (when (>= (length entries) 2)
    (let ((latest (car (last entries)))
          (previous (car (last entries 2))))
      (equal (delib-flow--extract-stage-signature latest stage-id)
             (delib-flow--extract-stage-signature previous stage-id)))))

(defun delib-flow--project-refinement-action-label (run)
  "Return best refinement action label for RUN."
  (if (delib-flow--non-empty-string-p (delib-flow--operator-intent-text run))
      "Regenerate Selected Project"
    "Edit Operator Intent"))

(defun delib-flow--project-extraction-soft-warning (run)
  "Return soft-warning plist when project extraction is not converging in RUN."
  (when (delib-flow--selected-project-drafted-p run)
    (or
     (let ((entries (delib-flow--stage-history-entries-for-stage run 'extract-actions)))
       (when (and (>= (length entries) 2)
                  (delib-flow--extract-stage-weak-p (car (last entries)) 'extract-actions)
                  (or (>= (length entries) 3)
                      (delib-flow--extract-stage-repetitive-p entries 'extract-actions)))
         (list :stage-id 'extract-actions
               :message
               (format "Extraction retries are yielding weak or repetitive action candidates. %s before retrying extraction."
                       (delib-flow--project-refinement-action-label run)))))
     (let ((entries (delib-flow--stage-history-entries-for-stage run 'extract-waiting-for)))
       (when (and (>= (length entries) 2)
                  (delib-flow--extract-stage-weak-p (car (last entries)) 'extract-waiting-for)
                  (or (>= (length entries) 3)
                      (delib-flow--extract-stage-repetitive-p entries 'extract-waiting-for)))
         (list :stage-id 'extract-waiting-for
               :message
               (format "Waiting-for extraction retries are yielding weak or repetitive candidates. %s before retrying extraction."
                       (delib-flow--project-refinement-action-label run))))))))

(defun delib-flow--project-extraction-needs-refinement-p (run stage-id)
  "Return non-nil when STAGE-ID should be demoted behind refinement in RUN."
  (eq (plist-get (delib-flow--project-extraction-soft-warning run) :stage-id)
      stage-id))

(defun delib-flow--project-workflow-status (run)
  "Return status text for the selected-project workflow in RUN."
  (cond
   ((delib-flow--selected-project-drafted-p run)
    (string-join
     (append
      (delq nil
            (list
             "This project loop is active. The drafted project package is the current source of truth."
             (format "- Primary next action: %s"
                     (delib-flow--project-workflow-next-action-label run))
             (delib-flow--project-context-status-line run)
             (when-let ((warning (delib-flow--project-extraction-soft-warning run)))
               (format "- Refinement warning: %s" (plist-get warning :message)))
             "- Workflow steps:"))
      (delib-flow--project-workflow-step-lines run))
     "\n"))
   ((delib-flow--selected-project-candidate-for-drafting run)
    (string-join
     (append
      (list
       "This project loop is active. One seeded project package is selected and should stay in focus."
       (format "- Primary next action: %s"
               (delib-flow--project-workflow-next-action-label run))
       (delib-flow--project-context-status-line run)
       "- Workflow steps:")
      (delib-flow--project-workflow-step-lines run))
     "\n"))
   ((delib-flow--project-flow-active-p run)
    (string-join
     (append
      (list
       "This project loop is active. Stay on the selected project package before drifting back to the broader queue."
       (format "- Primary next action: %s"
               (delib-flow--project-workflow-next-action-label run))
       (delib-flow--project-context-status-line run)
       "- Workflow steps:")
      (delib-flow--project-workflow-step-lines run))
     "\n"))
   (t
    "No selected-project workflow is active right now.")))

(defun delib-flow--project-workflow-text (run)
  "Return step-by-step selected-project workflow text for RUN."
  (if (delib-flow--project-flow-active-p run)
      (string-join
      (append
       (delib-flow--project-workflow-step-lines run)
       (list
        ""
         (format "- Why this step: %s"
                 (if-let ((warning (delib-flow--project-extraction-soft-warning run)))
                     (plist-get warning :message)
                   "keep one selected project package in focus until it has a solid project draft and immediate work set."))
         (format "- Primary action now: `%s`"
                 (delib-flow--project-workflow-next-action-label run))))
       "\n")
    "Run `Propose New Project` to start a project-drafting loop here."))

(defun delib-flow--project-draft-preview-status (run)
  "Return status text for selected-project draft previews in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'project-proposals)
    "This is the current drafted structure for the selected project. Review it before approval or filing.")
   ((delib-flow--selected-project-candidate-for-drafting run)
    "This is the selected project candidate. Draft it first if you want delib-flow to refine the title, state, tags, or first item before approval.")
   ((delib-flow--project-draft-preview-items run)
    "Review these project candidates before selecting one to draft.")
   (t
    "No selected-project draft previews are available from the current queue.")))

(delib-flow--define-function delib-flow--project-preview-fragment
			     (item)
			     "Return project-preview fragment for ITEM."
			     (let
				 ((first-item
				   (delib-flow--project-first-item item))
                                  (child-items
                                   (delib-flow--project-child-items item)))
			       (format
				"**** %s\n- Draft preview: this is the project definition delib-flow will use if you approve and file this project.\n- State: %s\n- First item: %s\n- Attached child items: %s\n%s\n- Tags: %s\n- Warnings: %s"
				(or (plist-get item :title)
				    "Untitled project")
				(or (plist-get item :state) 'active)
				(or (plist-get first-item :text)
				    "none")
                                (length child-items)
                                (if child-items
                                    (mapconcat
                                     (lambda (child)
                                       (format "  - %s %s"
                                               (delib-flow--draft-item-keyword child)
                                               (plist-get child :text)))
                                     child-items
                                     "\n")
                                  "  - none")
				(if-let
				    ((tags
				      (or (plist-get item :tags)
					  (plist-get item
						     :tag-suggestions))))
				    (string-join tags ", ")
				  "none")
				(or
				 (and-let*
				     ((warnings
				       (plist-get item :warnings))
				      ((> (length warnings) 0)))
				   (mapconcat
				    (lambda (warning)
				      (plist-get warning :message))
				    warnings "; "))
				 "none"))))


(defun delib-flow--project-draft-preview-text (run)
  "Return filing-loop selected-project draft preview text for RUN."
  (if-let ((items (delib-flow--project-draft-preview-items run)))
      (mapconcat #'delib-flow--project-preview-fragment items "\n\n")
    "Run Propose New Project to generate project candidates here."))

(defun delib-flow--project-regeneration-status (run)
  "Return status text for selected-project regeneration guidance in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'project-proposals)
    "Use this when the selected project needs a clearer title, stronger tags, or a more concrete first item.")
   ((delib-flow--selected-project-candidate-for-drafting run)
    "Draft the selected project before you approve or file it.")
   (t
    "No selected-project regeneration guidance is needed right now.")))

(defun delib-flow--project-regeneration-text (run)
  "Return selected-project regeneration guidance text for RUN."
  (cond
   ((delib-flow--selected-project-drafted-p run)
    (string-join
     (append
      (when-let ((warning (delib-flow--project-extraction-soft-warning run)))
        (list (format "- Soft warning: %s" (plist-get warning :message))))
      '("- Run `Extract Actions`, `Extract Waiting-For`, or `Suggest Reference Notes` next to expand this drafted project package."
        "- Use `Regenerate Selected Project` only when the project title, tags, or seeded first action are still wrong."
        "- Keep this selected project in focus until the project package and immediate work set are usable."))
     "\n"))
   ((delib-flow--selected-project-candidate-for-drafting run)
    (string-join
     '("- Run `Draft Selected Project` to tighten this selected project before approval."
       "- That draft becomes the version delib-flow will preview, approve, and file for this selected project."
       "- After drafting the project, move into extraction instead of blindly regenerating the whole project queue.")
     "\n"))
   ((delib-flow--project-draft-preview-items run)
    (string-join
     '("- Choose one project candidate in the queue first."
       "- After selection, use `Draft Selected Project` to clarify only that project instead of regenerating the whole project queue.")
     "\n"))
   (t
    "No project drafts are currently active.")))

(delib-flow--define-function
 delib-flow--selected-reference-note-candidate-from-package (package)
 "Return selected reference-note candidate from PACKAGE, or nil."
 (let*
     ((artifacts (plist-get package :artifacts))
      (state (plist-get artifacts 'reference-notes))
      (selected-id (plist-get state :selected-candidate-id))
      (selected-item
       (condition-case nil
	   (delib-flow--filing-selection-choice package)
	 (error nil)))
      (candidates (plist-get state :candidates)))
   (cond
    ((eq (plist-get selected-item :kind) 'reference-note)
     selected-item)
    (selected-id
     (seq-find
      (lambda (item)
	(equal (delib-flow--artifact-candidate-id item) selected-id))
      candidates))
    (t nil))))


(defun delib-flow--selected-reference-note-draft-from-package (package)
  "Return selected drafted reference-note item from PACKAGE, or nil."
  (let* ((draft (plist-get (plist-get (plist-get package :artifacts) 'reference-notes)
                           :selected-draft))
         (selected-item
          (condition-case nil
              (delib-flow--filing-selection-choice package)
            (error nil)))
         (selected-candidate
          (delib-flow--selected-reference-note-candidate-from-package package)))
    (when (and (eq (plist-get draft :kind) 'reference-note)
               (or (and (eq (plist-get selected-item :kind) 'reference-note)
                        (delib-flow--same-reference-note-item-p draft selected-item))
                   (and (eq (plist-get selected-candidate :kind) 'reference-note)
                        (delib-flow--same-reference-note-item-p draft selected-candidate))
                   (and (not (eq (plist-get selected-item :kind) 'reference-note))
                        (not (eq (plist-get selected-candidate :kind) 'reference-note)))))
      draft)))

(delib-flow--define-function delib-flow--reference-note-preview-item
			     (package)
			     "Return active reference-note ITEM from PACKAGE for capture review, or nil."
			     (let
				 ((approved
				   (car
				    (delib-flow--approved-items
				     package)))
				  (preview
				   (car
				    (delib-flow--planned-file-preview-items
				     package)))
				  (selected-draft
				   (delib-flow--selected-reference-note-draft-from-package
				    package))
				  (selected-candidate
				   (delib-flow--selected-reference-note-candidate-from-package
				    package)))
			       (cond
				((eq (plist-get selected-draft :kind)
				     'reference-note)
				 selected-draft)
				((eq
				  (plist-get selected-candidate :kind)
				  'reference-note)
				 selected-candidate)
				((eq (plist-get preview :kind)
				     'reference-note)
				 preview)
				((eq (plist-get approved :kind)
				     'reference-note)
				 approved)
				(t nil))))


(defun delib-flow--note-draft-items (run)
  "Return reference-note draft items from RUN."
  (seq-filter
   (lambda (item)
     (eq (plist-get item :kind) 'reference-note))
   (plist-get (plist-get run :filing) :draft-items)))

(delib-flow--define-function
 delib-flow--reference-note-draft-preview-items (run)
 "Return reference-note items that should be previewed in filing for RUN."
 (or
  (when-let
      ((draft
	(delib-flow--artifact-family-selected-draft run
						    'reference-notes)))
    (when (eq (plist-get draft :kind) 'reference-note) (list draft)))
  (when-let
      ((selected
	(delib-flow--selected-reference-note-candidate-for-drafting
	 run)))
    (when (eq (plist-get selected :kind) 'reference-note)
      (list selected)))
  (let ((planned (delib-flow--planned-file-preview-items run)))
    (when
	(and planned (= (length planned) 1)
	     (eq (plist-get (car planned) :kind) 'reference-note))
      planned))
  (seq-take (delib-flow--note-draft-items run) 2)))


(defun delib-flow--reference-note-draft-preview-status (run)
  "Return status text for reference-note draft previews in RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'reference-notes)
    "This is the current note canvas for the selected note. Edit parts directly, regenerate one part at a time, or regenerate the whole note before saving.")
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    "This is a seeded working draft from the selected note candidate. Edit parts directly, regenerate one part at a time, or use whole-note regeneration for a fuller pass before saving.")
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
    "Use this only when the whole note needs a fresh pass.")
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    "A seeded working draft is already visible. Use whole-note regeneration only when part edits or part regeneration are not enough.")
   (t
    "No note-draft regeneration guidance is needed right now.")))

(defun delib-flow--reference-note-regeneration-text (run)
  "Return note-draft regeneration guidance text for RUN."
  (cond
   ((delib-flow--artifact-family-selected-draft run 'reference-notes)
    (string-join
     '("- Run `Regenerate Selected Note` only when the whole note is pointed at the wrong concept or is too weak overall."
       "- The part controls above run selected-note drafting stages that replace only one section on the current note."
       "- Prefer editing or regenerating one part when only one section needs work."
       "- Whole-note regeneration replaces only the selected note draft. It does not replace the rest of the note queue.")
     "\n"))
   ((delib-flow--selected-reference-note-candidate-for-drafting run)
    (string-join
     '("- The selected note already has a seeded working draft here for part-level editing."
       "- Run `Regenerate Selected Note` only when you want delib-flow to regenerate the whole note from the current source and support inputs."
       "- If this is the wrong note to deepen, choose a different note in the queue first.")
     "\n"))
   ((delib-flow--reference-note-draft-preview-items run)
    (string-join
     '("- Choose one note candidate in the queue first."
       "- After selection, use `Regenerate Selected Note` to deepen only that note instead of regenerating the whole queue.")
     "\n"))
   (t
    "No reference-note drafts are currently active.")))

(defun delib-flow--reference-note-draft-with-updated-body (run body reason)
  "Return selected note draft in RUN with BODY and REASON applied."
  (let* ((candidate (delib-flow--selected-reference-note-candidate-for-drafting run))
         (draft (or (delib-flow--selected-reference-note-draft-from-package run)
                    (and candidate
                         (delib-flow--drafted-reference-note-item candidate run)))))
    (unless (eq (plist-get draft :kind) 'reference-note)
      (error "Select one note before editing its working draft"))
    (setq draft (copy-tree draft))
    (setq draft (plist-put draft :draft-body body))
    (setq draft (plist-put draft :draft-reason reason))
    (delib-flow--draft-item-with-selected-support
     run 'reference-notes
     (delib-flow--reference-note-draft-with-workspace-structure
      draft (or candidate draft) run))))

(defun delib-flow--reference-note-next-section-point ()
  "Return point for the next section heading, or nil."
  (and (re-search-forward "^\\* " nil t)
       (line-beginning-position)))

(defun delib-flow--append-reference-note-section (heading replacement-lines)
  "Append section HEADING with REPLACEMENT-LINES to current buffer."
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n"))
  (insert (format "* %s\n%s\n" heading (string-join replacement-lines "\n"))))

(defun delib-flow--replace-existing-reference-note-section (start replacement-lines)
  "Replace current section body from START with REPLACEMENT-LINES."
  (let ((end (or (delib-flow--reference-note-next-section-point)
                 (point-max))))
    (goto-char start)
    (delete-region start end)
    (insert (string-join replacement-lines "\n"))
    (unless (or (eobp) (looking-at-p "\n"))
      (insert "\n"))))

(defun delib-flow--reference-note-replace-section (body heading replacement-lines)
  "Return BODY with section HEADING replaced by REPLACEMENT-LINES."
  (let* ((text (or body ""))
         (pattern (format "^\\* %s$" (regexp-quote heading))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (if (re-search-forward pattern nil t)
          (delib-flow--replace-existing-reference-note-section
           (line-beginning-position 2)
           replacement-lines)
        (delib-flow--append-reference-note-section heading replacement-lines))
      (string-trim-right (buffer-string)))))

(defun delib-flow--reference-note-replace-working-line (body prefix replacement)
  "Return BODY with line PREFIX replaced by REPLACEMENT."
  (with-temp-buffer
    (insert (or body ""))
    (goto-char (point-min))
    (if (re-search-forward (format "^%s.*$" (regexp-quote prefix)) nil t)
        (replace-match replacement t t)
      (when (re-search-forward "^\\* Working draft$" nil t)
        (forward-line 1)
        (insert replacement "\n")))
    (string-trim-right (buffer-string))))

(defun delib-flow--reference-note-current-reuse-angle-line (draft candidate)
  "Return current reuse-angle line from DRAFT, or a seeded one from CANDIDATE."
  (let ((line (delib-flow--reference-note-draft-reuse-angle-text draft)))
    (if (string-prefix-p "- Reuse angle:" line)
        line
      (format "- Reuse angle: %s"
              (delib-flow--reference-note-reuse-angle candidate)))))

(defun delib-flow--reference-note-editor-lines (text)
  "Return TEXT split into editor lines, preserving intentional blank lines."
  (split-string (string-trim-right (or text "")) "\n" nil))

(defun delib-flow--reference-note-edited-reuse-angle-line (text)
  "Return normalized reuse-angle line for editor TEXT."
  (let ((trimmed (string-trim (or text ""))))
    (if (string-prefix-p "- Reuse angle:" trimmed)
        trimmed
      (format "- Reuse angle: %s" trimmed))))

(defun delib-flow--reference-note-store-edited-body (run body reason)
  "Return RUN with selected note BODY stored and annotated with REASON."
  (delib-flow--replace-artifact-family-selected-draft
   run 'reference-notes
   (delib-flow--reference-note-draft-with-updated-body
    run body reason)))

(defun delib-flow--reference-note-edit-section (run section text reason)
  "Return RUN with selected note SECTION replaced by editor TEXT for REASON."
  (let* ((draft (delib-flow--reference-note-selected-draft run))
         (body (delib-flow--reference-note-replace-section
                (plist-get draft :draft-body)
                section
                (delib-flow--reference-note-editor-lines text))))
    (delib-flow--reference-note-store-edited-body run body reason)))

(defun delib-flow--reference-note-edit-working-line (run prefix line reason)
  "Return RUN with selected note working PREFIX replaced by LINE for REASON."
  (let* ((draft (delib-flow--reference-note-selected-draft run))
         (body (delib-flow--reference-note-replace-working-line
                (plist-get draft :draft-body)
                prefix
                line)))
    (delib-flow--reference-note-store-edited-body run body reason)))

(defun delib-flow--reference-note-refreshed-working-draft-lines (run candidate draft)
  "Return refreshed working-draft lines for selected note CANDIDATE and DRAFT in RUN."
  (let* ((summary (delib-flow--reference-note-draft-summary candidate run))
         (highlights (delib-flow--reference-note-source-highlights candidate run))
         (durable-claim (or (car highlights) summary))
         (why-it-matters (or (cadr highlights) summary)))
    (list
     summary
     (format "- Durable claim: %s" durable-claim)
     (format "- Why it matters: %s" why-it-matters)
     (delib-flow--reference-note-current-reuse-angle-line draft candidate))))

(defun delib-flow--reference-note-refreshed-highlights-lines (run candidate)
  "Return refreshed source highlight lines for selected note CANDIDATE in RUN."
  (or (mapcar (lambda (line) (format "- %s" line))
              (delib-flow--reference-note-source-highlights candidate run))
      '("- Capture the strongest lines, claims, or examples from the source here.")))

(defun delib-flow--reference-note-refreshed-related-lines (run)
  "Return refreshed related-material lines for selected note in RUN."
  (or (mapcar (lambda (candidate)
                (format "- %s"
                        (delib-flow--reference-note-support-line candidate)))
              (delib-flow--artifact-family-selected-support-candidates
               run 'reference-notes))
      '("- Add nearby notes, projects, or references that deepen this idea.")))

(defun delib-flow--reference-note-refresh-source-highlights (run)
  "Return RUN with selected note source-highlights refreshed."
  (let* ((candidate (delib-flow--selected-reference-note-candidate-for-drafting run))
         (draft (delib-flow--reference-note-selected-draft run))
         (before (delib-flow--reference-note-draft-source-highlights-text draft))
         (body (delib-flow--reference-note-replace-section
                (plist-get draft :draft-body)
                "Source highlights"
                (delib-flow--reference-note-refreshed-highlights-lines
                 run candidate)))
         (reason "Refreshed the source highlights section for the selected note only.")
         (updated-run
          (delib-flow--replace-artifact-family-selected-draft
           run 'reference-notes
           (delib-flow--reference-note-draft-with-updated-body
            run body reason)))
         (after
          (delib-flow--reference-note-draft-source-highlights-text
           (delib-flow--artifact-family-selected-draft updated-run 'reference-notes))))
    (delib-flow--reference-note-store-part-outcome
     updated-run 'source-highlights before after reason)))

(defun delib-flow--reference-note-refresh-draft-body (run)
  "Return RUN with selected note working-draft body refreshed."
  (let* ((candidate (delib-flow--selected-reference-note-candidate-for-drafting run))
         (draft (delib-flow--reference-note-selected-draft run))
         (before (delib-flow--reference-note-draft-working-body-text draft))
         (body (delib-flow--reference-note-replace-section
                (plist-get draft :draft-body)
                "Working draft"
                (delib-flow--reference-note-refreshed-working-draft-lines
                 run candidate draft)))
         (reason "Refreshed the working draft body for the selected note only.")
         (updated-run
          (delib-flow--replace-artifact-family-selected-draft
           run 'reference-notes
           (delib-flow--reference-note-draft-with-updated-body
            run body reason)))
         (after
          (delib-flow--reference-note-draft-working-body-text
           (delib-flow--artifact-family-selected-draft updated-run 'reference-notes))))
    (delib-flow--reference-note-store-part-outcome
     updated-run 'draft-body before after reason)))

(defun delib-flow--reference-note-edit-draft-body (run text)
  "Return RUN with selected note working-draft body replaced by TEXT."
  (let* ((candidate (delib-flow--selected-reference-note-candidate-for-drafting run))
         (draft (delib-flow--reference-note-selected-draft run))
         (body (delib-flow--reference-note-replace-section
                (plist-get draft :draft-body)
                "Working draft"
                (append
                 (delib-flow--reference-note-editor-lines text)
                 (list (delib-flow--reference-note-current-reuse-angle-line
                        draft candidate))))))
    (delib-flow--reference-note-store-edited-body
     run body
     "Edited the working draft body for the selected note.")))

(defun delib-flow--reference-note-edit-source-highlights (run text)
  "Return RUN with selected note source highlights replaced by TEXT."
  (delib-flow--reference-note-edit-section
   run
   "Source highlights"
   text
   "Edited the source highlights for the selected note."))

(defun delib-flow--reference-note-edit-related-material (run text)
  "Return RUN with selected note related material replaced by TEXT."
  (delib-flow--reference-note-edit-section
   run
   "Related material to connect"
   text
   "Edited the related material for the selected note."))

(defun delib-flow--reference-note-edit-reuse-angle (run text)
  "Return RUN with selected note reuse-angle replaced by TEXT."
  (delib-flow--reference-note-edit-working-line
   run
   "- Reuse angle:"
   (delib-flow--reference-note-edited-reuse-angle-line text)
   "Edited the reuse angle for the selected note."))

(defun delib-flow--reference-note-refresh-related-material (run)
  "Return RUN with selected note related-material refreshed."
  (let* ((before (delib-flow--selected-reference-note-draft-related-material-text run))
         (body
          (delib-flow--reference-note-replace-section
           (plist-get (delib-flow--reference-note-selected-draft run)
                      :draft-body)
           "Related material to connect"
           (delib-flow--reference-note-refreshed-related-lines run)))
         (reason "Refreshed the related material section for the selected note only.")
         (updated-run
          (delib-flow--replace-artifact-family-selected-draft
           run 'reference-notes
           (delib-flow--reference-note-draft-with-updated-body
            run body reason)))
         (after
          (delib-flow--reference-note-draft-related-material-text
           (delib-flow--artifact-family-selected-draft updated-run 'reference-notes))))
    (delib-flow--reference-note-store-part-outcome
     updated-run 'related-material before after reason)))

(defun delib-flow--reference-note-refresh-reuse-angle (run)
  "Return RUN with selected note reuse-angle refreshed."
  (let* ((candidate (delib-flow--selected-reference-note-candidate-for-drafting run))
         (before (delib-flow--selected-reference-note-draft-reuse-angle-text run))
         (updated-line
          (format "- Reuse angle: %s"
                  (delib-flow--reference-note-reuse-angle candidate)))
         (body
          (delib-flow--reference-note-replace-working-line
           (plist-get (delib-flow--reference-note-selected-draft run)
                      :draft-body)
           "- Reuse angle:"
           updated-line))
         (reason "Refreshed the reuse angle for the selected note only.")
         (updated-run
          (delib-flow--replace-artifact-family-selected-draft
           run 'reference-notes
           (delib-flow--reference-note-draft-with-updated-body
            run body reason)))
         (after
          (delib-flow--reference-note-draft-reuse-angle-text
           (delib-flow--artifact-family-selected-draft updated-run 'reference-notes))))
    (delib-flow--reference-note-store-part-outcome
     updated-run 'reuse-angle before after reason)))

(delib-flow--define-function
 delib-flow--reference-note-capture-template-options (&optional item)
 "Return completion labels and keys for reference-note org-roam templates."
 (let*
     ((templates
       (and (boundp 'org-roam-capture-templates)
	    org-roam-capture-templates))
      (default-key
       (and item
	    (delib-flow--reference-note-org-roam-template-key item)))
      options)
   (dolist (template templates (nreverse options))
     (when (and (consp template) (stringp (car template)))
       (push
	(cons
	 (format "%s - %s%s" (car template)
		 (or (nth 1 template) "Unnamed template")
		 (if (equal (car template) default-key) " (default)"
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

(delib-flow--define-function delib-flow--fill-org-roam-template
			     (template context &optional
				       ensure-newline)
			     "Return org-roam TEMPLATE expanded against CONTEXT.\n\nWhen ENSURE-NEWLINE is non-nil, ensure the rendered text ends in a newline."
			     (when (functionp template)
			       (error
				"delib-flow org-roam filing does not support interactive template functions"))
			     (unless (stringp template)
			       (error
				"delib-flow org-roam filing requires string-based templates"))
			     (let*
				 ((rendered
				   (if
				       (and
					(require 'org-roam-capture nil
						 t)
					(require 'org-roam-node nil t)
					(fboundp
					 'org-roam-capture--fill-template)
					(fboundp 'org-roam-node-create))
				       (let
					   ((delib-flow--capture-context
					     (plist-get context
							:capture-context))
					    (org-capture-plist
					     (list :default-time
						   (plist-get context
							      :default-time)))
					    (org-roam-capture--node
					     (org-roam-node-create
					      :title
					      (plist-get context
							 :title)))
					    (org-roam-capture--info
					     (list :title
						   (plist-get context
							      :title)
						   :slug
						   (plist-get context
							      :slug))))
					 (org-roam-capture--fill-template
					  template ensure-newline))
				     (let
					 ((fallback
					   (delib-flow--fill-org-roam-template-fallback
					    template context)))
				       (if ensure-newline
					   (concat fallback "\n")
					 fallback)))))
			       (delib-flow--strip-org-capture-point-markers
				rendered)))


(defun delib-flow--reference-note-org-roam-target-path (path context)
  "Return absolute org-roam target PATH expanded against CONTEXT."
  (let* ((rendered (delib-flow--fill-org-roam-template path context))
         (root (delib-flow--org-roam-directory-root)))
    (unless root
      (error "The org-roam directory is not configured"))
    (if (file-name-absolute-p rendered)
        (expand-file-name rendered)
      (expand-file-name rendered root))))

(delib-flow--define-function
 delib-flow--reference-note-org-roam-stage-plan (item package)
 "Return staged org-roam filing plan for reference-note ITEM from PACKAGE."
 (when-let*
     ((template
       (delib-flow--reference-note-org-roam-template item package))
      (target-spec
       (or (plist-get (nthcdr 4 template) :if-new)
	   (plist-get (nthcdr 4 template) :target))))
   (let*
       ((context
	 (delib-flow--reference-note-org-roam-context item package))
	(override
	 (delib-flow--normalize-reference-note-target-override
	  (delib-flow--reference-note-effective-target-override
	   package)))
	(body-template (nth 3 template))
	(body
	 (delib-flow--fill-org-roam-template body-template context)))
     (pcase target-spec
       (`(file+head ,path ,head)
	(let*
	    ((resolved-path
	      (cond (override override)
		    ((stringp path)
		     (delib-flow--reference-note-org-roam-target-path
		      path context))
		    ((functionp path)
		     (error
		      "The selected org-roam template requires a target path. Fill `Target path:` before filing this note."))
		    (t
		     (error
		      "Unsupported org-roam target path in template"))))
	     (target
	      (if (file-name-absolute-p resolved-path) resolved-path
		(expand-file-name resolved-path
				  (or
				   (delib-flow--org-roam-directory-root)
				   default-directory)))))
	  (list :target target :title (plist-get context :title)
		:context context :body-template body-template
		:target-spec `(file+head ,target ,head) :content
		(concat
		 (delib-flow--fill-org-roam-template head context t)
		 body))))
       (`(file ,path)
	(let*
	    ((resolved-path
	      (cond (override override)
		    ((stringp path)
		     (delib-flow--reference-note-org-roam-target-path
		      path context))
		    ((functionp path)
		     (error
		      "The selected org-roam template requires a target path. Fill `Target path:` before filing this note."))
		    (t
		     (error
		      "Unsupported org-roam target path in template"))))
	     (target
	      (if (file-name-absolute-p resolved-path) resolved-path
		(expand-file-name resolved-path
				  (or
				   (delib-flow--org-roam-directory-root)
				   default-directory)))))
	  (list :target target :title (plist-get context :title)
		:context context :body-template body-template
		:target-spec `(file ,target) :content body)))
       (_
	(error
	 "delib-flow org-roam filing supports only file and file+head targets"))))))


(delib-flow--define-function
 delib-flow--reference-note-org-roam-target-requires-path-p
 (item package)
 "Return non-nil when ITEM's selected org-roam template requires a path override."
 (when-let*
     ((template
       (delib-flow--reference-note-org-roam-template item package))
      (target-spec
       (or (plist-get (nthcdr 4 template) :if-new)
	   (plist-get (nthcdr 4 template) :target))))
   (pcase target-spec
     (`(file+head ,path ,_) (functionp path))
     (`(file ,path) (functionp path)) (_ nil))))


(defun delib-flow--reference-note-file (item &optional package)
  "Return target file path for approved reference-note ITEM from PACKAGE."
  (if-let ((plan (and package
                      (delib-flow--reference-note-org-roam-stage-plan item package))))
      (plist-get plan :target)
    (delib-flow--reference-note-default-file item)))

(delib-flow--define-function delib-flow--reference-note-template
			     (item)
			     "Return configured note template for approved ITEM."
			     (if
				 (eq (plist-get item :note-type)
				     'project-support)
				 (if
				     (and
				      (delib-flow--using-default-setting-p
				       'delib-flow-project-support-note-capture-template)
				      (not
				       (delib-flow--using-default-setting-p
					'delib-flow-project-support-note-template)))
				     delib-flow-project-support-note-template
				   delib-flow-project-support-note-capture-template)
			       (if
				   (and
				    (delib-flow--using-default-setting-p
				     'delib-flow-general-note-capture-template)
				    (not
				     (delib-flow--using-default-setting-p
				      'delib-flow-general-note-template)))
				   delib-flow-general-note-template
				 delib-flow-general-note-capture-template)))


(delib-flow--define-function
 delib-flow--reference-note-template-has-title-p (item)
 "Return non-nil when approved reference-note ITEM template expands a title."
 (let
     ((template
       (or
	(when-let
	    ((org-roam-template
	      (delib-flow--reference-note-org-roam-template item)))
	  (pcase
	      (or (plist-get (nthcdr 4 org-roam-template) :if-new)
		  (plist-get (nthcdr 4 org-roam-template) :target))
	    (`(file+head ,_ ,head) head) (_ (nth 3 org-roam-template))))
	(delib-flow--reference-note-template item))))
   (or (string-match-p "\\${title}" template)
       (string-match-p "delib-flow-capture-note-title" template))))


(defun delib-flow--reference-note-template-bindings (item)
  "Return template bindings for approved ITEM."
  (list (cons "${title}" (delib-flow--reference-note-title item))
        (cons "${source-artifact}" (plist-get item :text))
        (cons "${note-type}" (symbol-name (plist-get item :note-type)))))

(delib-flow--define-function delib-flow--reference-note-body-lines
			     (content)
			     "Return significant body lines from note CONTENT."
			     (with-temp-buffer
			       (insert (or content ""))
			       (goto-char (point-min))
			       (while (looking-at "^#\\+.*\n")
				 (forward-line 1))
			       (when (looking-at "^:PROPERTIES:\n")
				 (when
				     (re-search-forward "^:END:\n?"
							nil t)
				   (goto-char (match-end 0))))
			       (seq-filter
				(lambda (line)
				  (let ((trimmed (string-trim line)))
				    (and
				     (not (string-empty-p trimmed))
				     (not
				      (string-match-p "\\`#\\+"
						      trimmed))
				     (not
				      (string-match-p
				       "\\`:\\(?:PROPERTIES\\|END\\|ID\\):"
				       trimmed)))))
				(split-string
				 (buffer-substring-no-properties
				  (point) (point-max))
				 "\n"))))


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
  (let ((project-title (delib-flow--filing-project-title package)))
    (unless project-title
      (error "Approved project filing requires a matched project"))
    (when (and (not (plist-get (delib-flow--approved-project-item package) :title))
               (delib-flow--project-child-exists-p delib-flow-my-projects-file
                                                   project-title
                                                   item))
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
    (let ((project-title (delib-flow--filing-project-title package)))
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


(provide 'delib-flow-filing)

;;; delib-flow-filing.el ends here
