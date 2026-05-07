;;; validate-complexity.el --- Cyclomatic complexity gate -*- lexical-binding: t; -*-

;;; Commentary:

;; Computes an Emacs-Lisp-specific cyclomatic complexity score for top-level
;; function definitions and fails if any function exceeds the configured limit.

;;; Code:

(require 'cl-lib)

(defconst delib-flow-complexity-threshold 5
  "Maximum allowed cyclomatic complexity per function.")

(defconst delib-flow-complexity-known-violations
  '(delib-flow--reference-note-capture-template
    delib-flow--operator-intent-next-step-label
    delib-flow--action-warning-generic-repair
    delib-flow--reference-note-source-local-heading-penalty
    delib-flow--email-digest-reference-note-focuses
    delib-flow--reference-note-useful-source-line-p
    delib-flow--reference-note-source-units
    delib-flow--reference-note-highlight-score
    delib-flow--reference-note-support-line
    delib-flow--reference-note-draft-summary
    delib-flow--reference-note-seeded-body
    delib-flow--reference-note-draft-body-with-seed
    delib-flow--draft-item-tag-suggestions
    delib-flow--artifact-family-for-stage
    delib-flow--artifact-family-for-item-kind
    delib-flow--artifact-candidate-id
    delib-flow--draft-revision-signature
    delib-flow--repair-project-focus-text
    delib-flow--manual-project-match-action
    delib-flow--select-approved-filing-actions-action
    delib-flow--accepted-working-context-text
    delib-flow--recommended-action-why-text
    delib-flow--unblock-guidance-text
    delib-flow--active-project-package-item
    delib-flow--active-filing-item-text
    delib-flow--selection-block-text
    delib-flow--planned-file-location-status
    delib-flow--project-workspace-visible-action-ids
    delib-flow--project-package-included-items
    delib-flow--reference-note-part-outcome-meaningful-change-p
    delib-flow--focused-filing-summary-status
    delib-flow--focused-filing-summary-lines-for-run
    delib-flow--project-package-expanded-items
    delib-flow--insert-new-project
    delib-flow--project-workflow-current-step
    delib-flow--project-workflow-next-action-label
    delib-flow--project-context-status-line
    delib-flow--project-extraction-soft-warning
    delib-flow--selected-reference-note-draft-from-package
    delib-flow--normalize-inspect-source-output
    delib-flow--apply-draft-selected-reference-note-entry
    delib-flow--inbox-heading-snapshots
    delib-flow--manual-project-shortlist-cards
    delib-flow--low-value-email-address-p
    delib-flow--email-type-hint
    delib-flow--email-inspect-digest
    delib-flow--source-type-classification
    delib-flow--inspect-source-analysis
    delib-flow--support-candidate-snapshot
    delib-flow--selected-action-candidate-from-package
    delib-flow--selected-action-candidate-for-drafting
    delib-flow--selected-waiting-for-candidate-from-package
    delib-flow--selected-waiting-for-candidate-for-drafting
    delib-flow--selected-reference-note-candidate-for-drafting
    delib-flow--selected-project-candidate-from-package
    delib-flow--selected-project-candidate-for-drafting
    delib-flow--drafted-project-item
    delib-flow--useful-entity-p
    delib-flow--waiting-for-warning-missing-owner
    delib-flow--project-proposal-warning-list
    delib-flow--replace-artifact-family-selected-draft
    delib-flow--restore-latest-artifact-family-draft
    delib-flow--audit-model-name
    delib-flow--manual-project-override-current-p
    delib-flow--current-project-decision
    delib-flow--accepted-project-decision
    delib-flow--recommended-action
    delib-flow--normalize-in-flight-state
    delib-flow--refresh-in-flight-ui
    delib-flow--control-header-line)
  "Current monolith functions allowed to exceed the complexity threshold during migration.")

(defconst delib-flow-complexity-source-files
  (sort (cl-remove-if
         (lambda (file)
           (string-match-p "\\`delib-flow-local-test-config\\.el\\'" file))
         (directory-files default-directory nil "\\`delib-flow.*\\.el\\'"))
        #'string<)
  "Repository source files included in complexity validation.")

(defconst delib-flow-complexity-defun-forms
  '(defun defsubst cl-defun cl-defsubst)
  "Top-level forms treated as function definitions for validation.")

(defconst delib-flow-complexity-branching-symbols
  '(if when unless while dolist dotimes catch)
  "Forms that add a single decision point.")

(defconst delib-flow-complexity-multi-branch-symbols
  '(cond pcase pcase-exhaustive condition-case)
  "Forms whose clauses add multiple decision points.")

(defconst delib-flow-complexity-boolean-symbols
  '(and or)
  "Boolean forms whose extra operands add decision points.")

(defun delib-flow-complexity--repo-root ()
  "Return the repository root for complexity validation."
  default-directory)

(defun delib-flow-complexity--expand (relative-path)
  "Return RELATIVE-PATH expanded from the repository root."
  (expand-file-name relative-path (delib-flow-complexity--repo-root)))

(defun delib-flow-complexity--read-forms (file)
  "Return top-level forms read from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (push form forms))
        (end-of-file (nreverse forms))))))

(defun delib-flow-complexity--function-form-p (form)
  "Return non-nil when FORM is a function definition to validate."
  (and (consp form)
       (memq (car form) delib-flow-complexity-defun-forms)
       (symbolp (cadr form))))

(defun delib-flow-complexity--docstring-or-declare-form-p (form)
  "Return non-nil when FORM is docstring or declare metadata."
  (or (stringp form)
      (and (consp form)
           (eq (car form) 'declare))))

(defun delib-flow-complexity--defun-body (form)
  "Return the executable body forms from function definition FORM."
  (let ((forms (cdddr form)))
    (while (and forms
                (delib-flow-complexity--docstring-or-declare-form-p (car forms)))
      (setq forms (cdr forms)))
    forms))

(defun delib-flow-complexity--list-complexity (forms)
  "Return the summed complexity contribution for FORMS."
  (let ((total 0))
    (dolist (form forms total)
      (setq total (+ total (delib-flow-complexity--form-complexity form))))))

(defun delib-flow-complexity--multi-branch-cost (clauses)
  "Return decision cost for CLAUSES in a multi-branch form."
  (max 1 (length clauses)))

(defun delib-flow-complexity--boolean-cost (args)
  "Return decision cost for boolean ARGS."
  (max 0 (1- (length args))))

(defun delib-flow-complexity--quoted-form-p (form)
  "Return non-nil when FORM should not be traversed."
  (and (consp form)
       (memq (car form) '(quote function))))

(defun delib-flow-complexity--form-complexity (form)
  "Return branch contribution for FORM."
  (cond
   ((or (atom form)
        (vectorp form)
        (delib-flow-complexity--quoted-form-p form))
    0)
   ((memq (car form) delib-flow-complexity-branching-symbols)
    (+ 1 (delib-flow-complexity--list-complexity (cdr form))))
   ((memq (car form) delib-flow-complexity-multi-branch-symbols)
    (+ (delib-flow-complexity--multi-branch-cost (cdr form))
       (delib-flow-complexity--list-complexity (cdr form))))
   ((memq (car form) delib-flow-complexity-boolean-symbols)
    (+ (delib-flow-complexity--boolean-cost (cdr form))
       (delib-flow-complexity--list-complexity (cdr form))))
   (t
    (delib-flow-complexity--list-complexity form))))

(defun delib-flow-complexity--function-complexity (form)
  "Return cyclomatic complexity for function definition FORM."
  (+ 1 (delib-flow-complexity--list-complexity
        (delib-flow-complexity--defun-body form))))

(defun delib-flow-complexity--file-results (file)
  "Return complexity results for all functions in FILE."
  (let (results)
    (dolist (form (delib-flow-complexity--read-forms file) (nreverse results))
      (when (delib-flow-complexity--function-form-p form)
        (push (list :file file
                    :name (cadr form)
                    :complexity
                    (delib-flow-complexity--function-complexity form))
              results)))))

(defun delib-flow-complexity--all-results ()
  "Return complexity results for repository source files."
  (let (results)
    (dolist (file delib-flow-complexity-source-files (nreverse results))
      (setq results
            (nconc (nreverse (delib-flow-complexity--file-results
                              (delib-flow-complexity--expand file)))
                   results)))))

(defun delib-flow-complexity--violations (results)
  "Return complexity violations from RESULTS."
  (cl-remove-if-not
   (lambda (result)
     (> (plist-get result :complexity) delib-flow-complexity-threshold))
   results))

(defun delib-flow-complexity--unexpected-violations (results)
  "Return complexity violations from RESULTS that are not in the known baseline."
  (cl-remove-if
   (lambda (result)
     (memq (plist-get result :name) delib-flow-complexity-known-violations))
   (delib-flow-complexity--violations results)))

(defun delib-flow-complexity--print-report (results)
  "Print a complexity report for RESULTS."
  (princ (format "Cyclomatic complexity threshold: %d\n"
                 delib-flow-complexity-threshold))
  (dolist (result results)
    (princ (format "  %s (%s): %d\n"
                   (plist-get result :name)
                   (file-relative-name (plist-get result :file)
                                       (delib-flow-complexity--repo-root))
                   (plist-get result :complexity)))))

(defun delib-flow-complexity-main ()
  "Run the complexity validator."
  (let* ((results (delib-flow-complexity--all-results))
         (violations (delib-flow-complexity--violations results))
         (unexpected (delib-flow-complexity--unexpected-violations results)))
    (delib-flow-complexity--print-report results)
    (when violations
      (princ (format "Known complexity baseline: %s\n"
                     delib-flow-complexity-known-violations)))
    (when unexpected
      (error "Unexpected complexity violations detected: %s"
             (mapcar (lambda (result)
                       (plist-get result :name))
                     unexpected)))))

(delib-flow-complexity-main)

;;; validate-complexity.el ends here
