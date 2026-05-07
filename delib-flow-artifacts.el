;;; delib-flow-artifacts.el --- Artifact helpers for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Deterministic artifact shaping, warnings, and stage-output normalization.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'delib-flow-config)
(require 'delib-flow-model)
(require 'delib-flow-services)

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

(defun delib-flow--operator-intent-text-from-package (package)
  "Return trimmed operator intent text from PACKAGE."
  (string-trim
   (or (plist-get (plist-get package :ui) :operator-intent)
       (delib-flow--editable-block-text
        (alist-get 'context-main
                   (plist-get (plist-get package :ui) :editable-blocks)))
       "")))

(defun delib-flow--operator-intent-present-p (package)
  "Return non-nil when PACKAGE carries explicit operator intent."
  (delib-flow--non-empty-string-p
   (delib-flow--operator-intent-text-from-package package)))

(defun delib-flow--support-candidate-snapshot (candidate)
  "Return frozen support snapshot for retained CANDIDATE."
  (let* ((title (or (plist-get candidate :title)
                    "Untitled note"))
         (focus (and (plist-get candidate :file)
                     (delib-flow--candidate-note-focus-line candidate)))
         (score (plist-get candidate :support-score))
         (reasons (or (plist-get candidate :support-reasons)
                      (plist-get candidate :filter-reasons)
                      (plist-get candidate :reasons)))
         (reason-text (and reasons
                           (string-join reasons ", ")))
         (identity (string-join
                    (delq nil
                          (list title
                                (and focus
                                     (string-trim focus))
                                reason-text))
                    " | ")))
    (list :identity identity
          :title title
          :focus (and focus (string-trim focus))
          :score score
          :reason-text reason-text
          :line (format "- %s"
                        (delib-flow--reference-note-support-line
                         candidate)))))

(defun delib-flow--repair-intent-text-p (text)
  "Return non-nil when TEXT carries repair-oriented cues."
  (let ((normalized (downcase (or text ""))))
    (seq-some
     (lambda (keyword)
       (string-match-p (regexp-quote keyword) normalized))
     delib-flow--issue-note-source-keywords)))

(defun delib-flow--package-source-type (package)
  "Return best available source type from PACKAGE."
  (let* ((working (plist-get package :working-context))
         (inspect-review (delib-flow--review-record working 'inspect-source))
         (accepted-inspect (plist-get inspect-review :accepted-output))
         (candidate-inspect (plist-get inspect-review :candidate-output)))
    (or (plist-get accepted-inspect :source-type)
        (plist-get candidate-inspect :source-type)
        (plist-get (plist-get working :inspect-output) :source-type)
        (plist-get (plist-get package :source) :source-type)
        'unknown)))

(defun delib-flow--package-repair-intent-p (package)
  "Return non-nil when PACKAGE clearly indicates repair-oriented work."
  (or (eq (delib-flow--package-source-type package) 'issue-note)
      (delib-flow--repair-intent-text-p
       (delib-flow--operator-intent-text-from-package package))
      (delib-flow--repair-intent-text-p
       (string-join
        (delq nil
              (list (plist-get (plist-get package :source) :title)
                    (plist-get (plist-get package :source) :content)))
        "\n"))))

(defun delib-flow--make-draft-reference-note (text source note-type)
  "Return draft reference-note object for TEXT, SOURCE, and NOTE-TYPE."
  (list :kind 'reference-note
        :text text
        :source source
        :note-type note-type))

(defun delib-flow--draft-item-with-selected-support (run family draft)
  "Return DRAFT enriched with focused support metadata from RUN FAMILY state."
  (let ((candidates (delib-flow--artifact-family-selected-support-candidates run family))
        (context (delib-flow--artifact-family-selected-support-context run family)))
    (if (or candidates context)
        (plist-put
         (plist-put (copy-tree draft) :support-candidates candidates)
         :support-context context)
      draft)))

(defun delib-flow--selected-action-candidate-from-package (package)
  "Return selected next-action candidate from PACKAGE, or nil."
  (let* ((artifacts (plist-get package :artifacts))
         (state (plist-get artifacts 'actions))
         (selected-id (plist-get state :selected-candidate-id))
         (selected-item
          (condition-case nil
              (delib-flow--filing-selection-choice package)
            (error nil)))
         (candidates (plist-get state :candidates)))
    (cond
     ((eq (plist-get selected-item :kind) 'next-action) selected-item)
     (selected-id
      (seq-find
       (lambda (item)
         (equal (delib-flow--artifact-candidate-id item) selected-id))
       candidates))
     (t nil))))

(defun delib-flow--selected-action-draft-from-package (package)
  "Return selected drafted next-action item from PACKAGE, or nil."
  (plist-get (plist-get (plist-get package :artifacts) 'actions)
             :selected-draft))

(defun delib-flow--selected-action-candidate-for-drafting (package)
  "Return selected next-action candidate from PACKAGE for item-local drafting."
  (or (delib-flow--selected-action-candidate-from-package package)
      (let ((item
             (condition-case nil
                 (delib-flow--filing-selection-choice package)
               (error nil))))
        (when (eq (plist-get item :kind) 'next-action)
          item))
      (delib-flow--artifact-family-selected-candidate package 'actions)))

(defun delib-flow--action-drafted-text (item package &optional draft-text)
  "Return drafted action text for ITEM in PACKAGE.

DRAFT-TEXT overrides the deterministic default when non-empty."
  (let ((clean (and (stringp draft-text)
                    (not (string-empty-p (string-trim draft-text)))
                    (string-trim draft-text))))
    (or clean
        (string-trim
         (or (plist-get item :text)
             (delib-flow--source-title-action package))))))

(defun delib-flow--drafted-action-item (item package &optional draft-text reason)
  "Return ITEM enriched as a drafted next action for PACKAGE.

DRAFT-TEXT and REASON override deterministic defaults when provided."
  (let ((draft (copy-tree item)))
    (setq draft
          (plist-put draft :text
                     (delib-flow--action-drafted-text item package draft-text)))
    (setq draft
          (plist-put draft :draft-reason
                     (or reason
                         (if (delib-flow--artifact-family-selected-support-candidates
                              package 'actions)
                             "Drafted the selected action with a tighter wording pass informed by focused support."
                           "Drafted the selected action with a tighter wording pass."))))
    (delib-flow--draft-item-with-selected-support package 'actions draft)))

(defun delib-flow--selected-waiting-for-candidate-from-package (package)
  "Return selected waiting-for candidate from PACKAGE, or nil."
  (let* ((artifacts (plist-get package :artifacts))
         (state (plist-get artifacts 'waiting-fors))
         (selected-id (plist-get state :selected-candidate-id))
         (selected-item
          (condition-case nil
              (delib-flow--filing-selection-choice package)
            (error nil)))
         (candidates (plist-get state :candidates)))
    (cond
     ((eq (plist-get selected-item :kind) 'waiting-for) selected-item)
     (selected-id
      (seq-find
       (lambda (item)
         (equal (delib-flow--artifact-candidate-id item) selected-id))
       candidates))
     (t nil))))

(defun delib-flow--selected-waiting-for-draft-from-package (package)
  "Return selected drafted waiting-for item from PACKAGE, or nil."
  (plist-get (plist-get (plist-get package :artifacts) 'waiting-fors)
             :selected-draft))

(defun delib-flow--selected-waiting-for-candidate-for-drafting (package)
  "Return selected waiting-for candidate from PACKAGE for item-local drafting."
  (or (delib-flow--selected-waiting-for-candidate-from-package package)
      (let ((item
             (condition-case nil
                 (delib-flow--filing-selection-choice package)
               (error nil))))
        (when (eq (plist-get item :kind) 'waiting-for)
          item))
      (delib-flow--artifact-family-selected-candidate package 'waiting-fors)))

(defun delib-flow--waiting-for-drafted-text (item package &optional draft-text)
  "Return drafted waiting-for text for ITEM in PACKAGE.

DRAFT-TEXT overrides the deterministic default when non-empty."
  (let ((clean (and (stringp draft-text)
                    (not (string-empty-p (string-trim draft-text)))
                    (string-trim draft-text))))
    (or clean
        (string-trim
         (or (plist-get item :text)
             (delib-flow--source-title-waiting-for package))))))

(defun delib-flow--drafted-waiting-for-item (item package &optional draft-text reason)
  "Return ITEM enriched as a drafted waiting-for for PACKAGE.

DRAFT-TEXT and REASON override deterministic defaults when provided."
  (let ((draft (copy-tree item)))
    (setq draft
          (plist-put draft :text
                     (delib-flow--waiting-for-drafted-text item package draft-text)))
    (setq draft
          (plist-put draft :draft-reason
                     (or reason
                         (if (delib-flow--artifact-family-selected-support-candidates
                              package 'waiting-fors)
                             "Drafted the selected waiting-for with a clearer dependency pass informed by focused support."
                           "Drafted the selected waiting-for with a clearer dependency wording pass."))))
    (delib-flow--draft-item-with-selected-support package 'waiting-fors draft)))

(defun delib-flow--selected-reference-note-candidate-for-drafting (package)
  "Return selected reference-note candidate from PACKAGE for item-local drafting."
  (or (delib-flow--selected-reference-note-candidate-from-package package)
      (let ((item
             (condition-case nil
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
                         (if (delib-flow--artifact-family-selected-support-candidates
                              package 'reference-notes)
                             "Drafted the selected note with seeded structure and focused support."
                           "Drafted the selected note with seeded structure and source support."))))
    (when-let ((drift-warning
                (delib-flow--reference-note-draft-drift-warning
                 draft (plist-get draft :draft-body))))
      (setq draft
            (plist-put draft :warnings
                       (append (delib-flow--draft-item-warnings draft)
                               (list drift-warning)))))
    (delib-flow--draft-item-with-selected-support
     package 'reference-notes
     (delib-flow--reference-note-draft-with-workspace-structure
      draft item package))))

(defun delib-flow--reference-note-part-stage-label (part-id)
  "Return operator-facing part label for selected note PART-ID."
  (alist-get
   part-id
   '((draft-body . "draft body")
     (source-highlights . "source highlights")
     (related-material . "related material")
     (reuse-angle . "reuse angle"))))

(defun delib-flow--reference-note-part-stage-text (drafted-item part-id)
  "Return drafted note PART-ID text from DRAFTED-ITEM."
  (if-let ((reader
            (alist-get
             part-id
             '((draft-body . delib-flow--reference-note-draft-working-body-text)
               (source-highlights . delib-flow--reference-note-draft-source-highlights-text)
               (related-material . delib-flow--reference-note-draft-related-material-text)
               (reuse-angle . delib-flow--reference-note-draft-reuse-angle-text)))))
      (funcall reader drafted-item)
    ""))

(defun delib-flow--selected-project-candidate-from-package (package)
  "Return selected project candidate from PACKAGE, or nil."
  (let* ((artifacts (plist-get package :artifacts))
         (state (plist-get artifacts 'project-proposals))
         (selected-id (plist-get state :selected-candidate-id))
         (selected-item
          (condition-case nil
              (delib-flow--filing-selection-choice package)
            (error nil)))
         (candidates (plist-get state :candidates)))
    (cond
     ((eq (plist-get selected-item :kind) 'project) selected-item)
     (selected-id
      (seq-find
       (lambda (item)
         (equal (delib-flow--artifact-candidate-id item)
                selected-id))
       candidates))
     (t nil))))

(defun delib-flow--selected-project-draft-from-package (package)
  "Return selected drafted project item from PACKAGE, or nil."
  (let* ((state (plist-get (plist-get package :artifacts) 'project-proposals))
         (draft (plist-get state :selected-draft))
         (selected-id (plist-get state :selected-candidate-id))
         (draft-id (plist-get draft :draft-candidate-id)))
    (when (and (eq (plist-get draft :kind) 'project)
               (or (null selected-id)
                   (null draft-id)
                   (equal selected-id draft-id)))
      draft)))

(defun delib-flow--selected-project-candidate-for-drafting (package)
  "Return selected project candidate from PACKAGE for item-local drafting."
  (or (delib-flow--selected-project-candidate-from-package package)
      (let ((item
             (condition-case nil
                 (delib-flow--filing-selection-choice package)
               (error nil))))
        (when (eq (plist-get item :kind) 'project)
          item))
      (delib-flow--artifact-family-selected-candidate package
                                                      'project-proposals)))

(defun delib-flow--project-drafted-title (item package &optional draft-title)
  "Return drafted project title for ITEM in PACKAGE.

DRAFT-TITLE overrides the deterministic default when non-empty."
  (let ((clean (and (stringp draft-title)
                    (not (string-empty-p (string-trim draft-title)))
                    (string-trim draft-title))))
    (or clean
        (string-trim
         (or (plist-get item :title)
             (delib-flow--project-proposal-title package))))))

(defun delib-flow--project-drafted-first-item-text (item package &optional draft-text)
  "Return drafted first-item text for ITEM in PACKAGE.

DRAFT-TEXT overrides the deterministic default when non-empty."
  (let ((clean (and (stringp draft-text)
                    (not (string-empty-p (string-trim draft-text)))
                    (string-trim draft-text))))
    (or clean
        (plist-get (or (delib-flow--project-first-item item)
                       (delib-flow--project-proposal-first-item package))
                   :text))))

(defun delib-flow--drafted-project-item
    (item package &optional draft-title draft-state draft-first-item-text
          draft-tags reason)
  "Return ITEM enriched as a drafted project proposal for PACKAGE.

DRAFT-TITLE, DRAFT-STATE, DRAFT-FIRST-ITEM-TEXT, DRAFT-TAGS, and REASON
override deterministic defaults when provided."
  (let* ((title
          (delib-flow--project-drafted-title
           item package draft-title))
         (state
          (or draft-state
              (plist-get item :state) 'active))
         (first-kind
          (or (plist-get (delib-flow--project-first-item item)
                         :kind)
              'next-action))
         (first-text
          (delib-flow--project-drafted-first-item-text
           item package draft-first-item-text))
         (first-item
          (if (eq first-kind 'waiting-for)
              (delib-flow--make-draft-waiting-for
               first-text 'project-proposal)
            (delib-flow--make-draft-action
             first-text 'project-proposal)))
         (child-items
          (cons first-item
                (mapcar #'copy-tree
                        (delib-flow--project-additional-child-items
                         item))))
         (tags
          (or draft-tags
              (plist-get item :tags)
              (delib-flow--project-proposal-tags
               package)))
         (draft
          (delib-flow--make-draft-project
           title state first-item tags)))
    (setq draft
          (delib-flow--project-with-child-items
           draft child-items))
    (setq draft
          (plist-put draft :draft-reason
                     (or reason
                         (if
                             (delib-flow--artifact-family-selected-support-candidates
                              package
                              'project-proposals)
                             "Drafted the selected project with a tighter definition pass informed by focused support."
                           "Drafted the selected project with a tighter project-definition pass."))))
    (setq draft
          (delib-flow--draft-item-with-tag-suggestions
           draft package))
    (setq draft
          (delib-flow--draft-item-with-selected-support
           package 'project-proposals draft))
    (delib-flow--draft-item-with-warnings
     draft
     (delib-flow--project-proposal-warning-list
      draft package))))

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

(defun delib-flow--draft-with-evidence-snapshot (run family draft)
  "Return DRAFT carrying a frozen evidence snapshot for FAMILY in RUN."
  (if (plist-get draft :evidence-snapshot)
      draft
    (plist-put (copy-tree draft)
               :evidence-snapshot
               (delib-flow--build-draft-evidence-snapshot run family draft))))

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
    (waiting-for-speculative-owner . "Replace the generic owner placeholder with a named external person, team, or system, or remove this waiting-for if no outside dependency exists.")
    (waiting-for-vague-blocker . "Name the exact response, approval, or deliverable that is blocking progress.")
    (waiting-for-speculative-blocker . "Name the exact external confirmation, approval, or deliverable, or convert this back into a next action if it is still your own work.")
    (waiting-for-state-phrasing . "Rewrite it to start with `Waiting for ...` so the blocked dependency is explicit.")
    (project-title-timestamp-noise . "Remove timestamp or journal-heading noise so the proposal uses a stable project title.")
    (project-title-note-shape . "Rewrite the title so it names the project itself, not the raw note, presentation idea, or reminder heading.")
    (project-tags-invalid . "Replace numeric/date fragments with meaningful project tags, or leave tags empty until better ones are known.")
    (project-first-item-generic . "Replace the placeholder with the first concrete deliverable or action that would start the project.")
    (selected-item-needs-draft . "Run the item-local `Draft Selected ...` stage first so filing works from the enriched selected artifact rather than the raw candidate.")
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

(defun delib-flow--action-warning-study-activity (item)
  "Return warning when action ITEM looks like study or practice activity."
  (when-let ((verb (delib-flow--artifact-leading-word (plist-get item :text))))
    (when (member verb '("practice" "study" "read" "watch"))
      (delib-flow--make-artifact-warning
       'study-activity-action
       (format "Starts with \"%s\", which reads like study or practice activity rather than concrete repair work." verb)))))

(defun delib-flow--action-warning-generic-repair (item)
  "Return warning when action ITEM uses generic repair wording."
  (let* ((text (string-trim (downcase (or (plist-get item :text) ""))))
         (word-count (delib-flow--artifact-text-word-count text)))
    (when (or (string-match-p "\\`fix errors and typos\\'" text)
              (string-match-p "\\`fix broken\\(?: [[:alnum:]_-]+\\)?\\'" text)
              (and (string-match-p "\\`fix\\b" text)
                   (<= word-count 4)))
      (delib-flow--make-artifact-warning
       'generic-repair-action
       "Uses generic repair wording without naming the broken target or the verification step."))))

(defun delib-flow--action-warning-decision-state (item)
  "Return blocking warning when action ITEM reads like a decision or status statement."
  (let ((text (string-trim (downcase (or (plist-get item :text) "")))))
    (when (or (string-match-p "\\`\\(keep\\|maintain\\|continue\\|stay\\|remain\\)\\b"
                              text)
              (string-match-p "\\b\\(decision\\|decided\\|agreed\\|target\\)\\b"
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
         (delib-flow--action-warning-study-activity item)
         (delib-flow--action-warning-generic-repair item)
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

(defun delib-flow--waiting-for-warning-speculative-owner (item)
  "Return advisory warning when waiting-for ITEM uses a generic owner placeholder."
  (let ((text (downcase (or (plist-get item :text) ""))))
    (when (string-match-p "\\bproject owner\\b" text)
      (delib-flow--make-artifact-warning
       'waiting-for-speculative-owner
       "Uses the generic owner placeholder `project owner`, so this waiting-for still looks speculative."))))

(defun delib-flow--waiting-for-warning-speculative-blocker (item)
  "Return advisory warning when waiting-for ITEM looks internally owned."
  (let ((text (downcase (or (plist-get item :text) ""))))
    (when (or (string-match-p "\\bon .*broken\\b" text)
              (string-match-p "\\bon .*exercise\\b" text))
      (delib-flow--make-artifact-warning
       'waiting-for-speculative-blocker
       "Does not yet name a concrete outside dependency, so this waiting-for may still be your own next action."))))

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
         (delib-flow--waiting-for-warning-speculative-owner item)
         (delib-flow--waiting-for-warning-speculative-blocker item)
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

(defun delib-flow--source-title-action (package)
  "Return a draft action derived from PACKAGE source title."
  (delib-flow--make-draft-action
   (or (delib-flow--source-action-line package)
       (and (delib-flow--package-repair-intent-p package)
            (delib-flow--capitalize-sentence-start
             (format "Investigate and reproduce %s"
                     (delib-flow--repair-project-focus-text package))))
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

(defun delib-flow--repair-project-focus-text (package)
  "Return a concrete repair target phrase derived from PACKAGE."
  (let* ((title (downcase (delib-flow--source-display-title package)))
         (intent (downcase (delib-flow--operator-intent-text-from-package package))))
    (cond
     ((and (string-match-p "steno" title)
           (string-match-p "exercise\\|drill" title)
           (string-match-p "website" intent))
      "broken steno website exercises")
     ((string-match-p "\\`broken\\s-+" title)
      (string-trim (replace-regexp-in-string "\\`broken\\s-+" "" title)))
     ((or (string-match-p "website" intent)
          (string-match-p "website\\|page\\|link" title))
      (format "broken %s"
              (string-trim
               (or (and (string-match-p "website\\|page\\|link" title) title)
                   (delib-flow--source-display-title package)))))
     (t
      (string-trim (delib-flow--source-display-title package))))))

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
       (when (eq (delib-flow--package-source-type package) 'issue-note)
         '("issue_note"))
       title-words))
     3)))

(defun delib-flow--project-proposal-title (package)
  "Return deterministic project title derived from PACKAGE."
  (if (delib-flow--package-repair-intent-p package)
      (delib-flow--capitalize-sentence-start
       (format "Fix %s"
               (delib-flow--repair-project-focus-text package)))
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
         clean)))))

(defun delib-flow--project-proposal-first-item (package)
  "Return deterministic first project item derived from PACKAGE."
  (car (or (delib-flow--project-proposal-derived-child-items package)
           (list (delib-flow--project-proposal-fallback-child-item package)))))

(defun delib-flow--project-proposal-warning-list (item _package)
  "Return structured warning list for proposed project ITEM."
  (let ((title (or (plist-get item :title) ""))
        (tags (or (plist-get item :tags) 'nil))
        warnings)
    (when (string-match-p
           "\\`\\(?:<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*>\\|\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\]\\)"
           title)
      (push
       (delib-flow--make-artifact-warning
        'project-title-timestamp-noise
        "Proposed project title still contains source timestamp or journal-heading noise."
        'blocking)
       warnings))
    (when (string-match-p
           delib-flow--project-proposal-title-prefix-pattern
           (downcase title))
      (push
       (delib-flow--make-artifact-warning
        'project-title-note-shape
        "Proposed project title still reads like the raw note heading instead of a stable project identity."
        'blocking)
       warnings))
    (when (seq-some
           (lambda (tag)
             (or (string-match-p "\\`[0-9]+\\'" tag)
                 (string-match-p "\\`[0-9]+_[0-9_]*\\'" tag)))
           tags)
      (push
       (delib-flow--make-artifact-warning
        'project-tags-invalid
        "Proposed project tags still include numeric/date fragments."
        'blocking)
       warnings))
    (when (delib-flow--project-proposal-placeholder-first-item-p
           (plist-get (car (delib-flow--project-child-items item)) :text))
      (push
       (delib-flow--make-artifact-warning
        'project-first-item-generic
        "First project item is still a generic placeholder or explicit incomplete note instead of a concrete source-derived step."
        'blocking)
       warnings))
    (nreverse warnings)))

(defun delib-flow--propose-new-project-result (package)
  "Return raw new-project proposal result for PACKAGE."
  (let* ((project-item (delib-flow--proposed-project-item package))
         (first-item (car (delib-flow--project-child-items project-item)))
         (ready-p (delib-flow--draft-item-ready-p project-item)))
    (list :project project-item
          :project-title (plist-get project-item :title)
          :project-state (plist-get project-item :state)
          :tags (plist-get project-item :tags)
          :child-items (delib-flow--project-child-items project-item)
          :first-item first-item
          :operator-intent (delib-flow--operator-intent-text-from-package package)
          :reason (if ready-p
                      "No project matched, so a concrete project package was prepared from the source."
                    "No project matched, but the source did not provide a concrete child item yet. Review the proposed project package and refine the first actionable child before filing."))))

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
                      (seq-filter (lambda (word) (>= (length word) 4))
                                  title-words)))
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
             (mapcar
              #'delib-flow--normalize-tag-suggestion
              (seq-filter
               (lambda (word)
                 (>= (length word) 4))
               (delib-flow--string-words focus)))))))
    (delete-dups
     (delq nil
           (append
            (and (eq (plist-get item :kind) 'project)
                 (mapcar
                  #'delib-flow--normalize-tag-suggestion
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

(defun delib-flow--artifact-family-for-stage (stage-id)
  "Return artifact family keyword associated with finder STAGE-ID."
  (pcase stage-id
    ('extract-actions 'actions)
    ('extract-waiting-for 'waiting-fors)
    ('suggest-reference-notes 'reference-notes)
    ('propose-new-project 'project-proposals)
    (_ nil)))

(defun delib-flow--artifact-family-for-item-kind (kind)
  "Return artifact family keyword associated with item KIND."
  (pcase kind
    ('next-action 'actions)
    ('waiting-for 'waiting-fors)
    ('reference-note 'reference-notes)
    ('project 'project-proposals)
    (_ nil)))

(defun delib-flow--artifact-candidate-id (item)
  "Return stable candidate identifier for ITEM."
  (or (plist-get item :candidate-id)
      (pcase (plist-get item :kind)
        ('project
         (or (plist-get item :title)
             (plist-get item :text)))
        (_
         (or (plist-get item :text)
             (plist-get item :title))))))

(defun delib-flow--draft-revision-signature (draft)
  "Return stable comparison signature for DRAFT."
  (when draft
    (pcase (plist-get draft :kind)
      ('reference-note
       (list 'reference-note
             (plist-get draft :text)
             (plist-get draft :draft-body)
             (plist-get draft :note-type)))
      ('project
       (list 'project
             (plist-get draft :title)
             (plist-get draft :state)
             (plist-get (plist-get draft :first-item) :text)
             (or (plist-get draft :tags)
                 (plist-get draft :tag-suggestions))))
      (_
       (list (plist-get draft :kind)
             (plist-get draft :text)
             (or (plist-get draft :tags)
                 (plist-get draft :tag-suggestions)))))))

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

(defun delib-flow--reference-note-warning-weak-identity (item)
  "Return warning when general PKM reference-note ITEM is still source-bound."
  (when (and (eq (plist-get item :note-type) 'general-pkm)
             (< (or (plist-get item :focus-score) 0) 8))
    (delib-flow--make-artifact-warning
     'reference-note-candidate-identity
     "Candidate still reads like a source-local topic; prefer a more durable reusable concept before heavy drafting.")))

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
         (delib-flow--reference-note-warning-weak-identity item)
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
        :child-items (delq nil (list first-item))
        :tags tags
        :text title))

(defconst delib-flow--action-line-verbs
  '("send" "write" "reply" "schedule" "confirm" "share" "draft" "update"
    "call" "ask" "prepare" "file" "create" "summarize" "review"
    "gather" "fix" "repair" "debug" "investigate")
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

(defun delib-flow--repair-oriented-child-item (package)
  "Return a synthesized repair-oriented child item for PACKAGE, or nil."
  (when (delib-flow--package-repair-intent-p package)
    (delib-flow--make-draft-action
     (delib-flow--capitalize-sentence-start
      (format "Investigate and fix %s"
              (delib-flow--repair-project-focus-text package)))
     'project-proposal)))

(defconst delib-flow--project-proposal-meta-first-item-regexp
  (concat
   "\\`\\("
   "define\\(?: the\\| a\\)? \\(?:first deliverable\\|next action\\|next step\\|first step\\)"
   "\\|clarify the first concrete step"
   "\\|identify the first concrete step"
   "\\|plan the project"
   "\\)\\b")
  "Regexp matching meta-planning first items that are too weak for project proposals.")

(defun delib-flow--project-proposal-placeholder-first-item-p (text)
  "Return non-nil when TEXT is a meta placeholder for a project first item."
  (let ((normalized (downcase (string-trim (or text "")))))
    (or (string-match-p
         delib-flow--project-proposal-meta-first-item-regexp
         normalized)
        (string-prefix-p
         "no concrete child item could be derived from this source yet"
         normalized))))

(defun delib-flow--project-child-items (item)
  "Return attached child items from project ITEM."
  (or (plist-get item :child-items)
      (when-let ((first-item (plist-get item :first-item)))
        (list first-item))))

(defun delib-flow--project-additional-child-items (item)
  "Return attached child items beyond the embedded first item from project ITEM."
  (cdr (delib-flow--project-child-items item)))

(defun delib-flow--project-with-child-items (item child-items)
  "Return project ITEM with CHILD-ITEMS attached and first item aligned."
  (let ((items (delq nil (copy-tree child-items))))
    (plist-put
     (plist-put (copy-tree item) :first-item (car items))
     :child-items items)))

(defun delib-flow--project-proposal-derived-child-items (package)
  "Return concrete source-derived child items for a new project PACKAGE."
  (let ((items nil))
    (when-let ((action-text (delib-flow--source-action-line package)))
      (push (delib-flow--make-draft-action action-text 'project-proposal) items))
    (when-let ((waiting-text (delib-flow--source-waiting-for-line package)))
      (push (delib-flow--make-draft-waiting-for waiting-text 'project-proposal)
            items))
    (when-let ((repair-item (delib-flow--repair-oriented-child-item package)))
      (push repair-item items))
    (delete-dups
     (nreverse
      (seq-filter
       (lambda (item)
         (not (delib-flow--project-proposal-placeholder-first-item-p
               (plist-get item :text))))
       items)))))

(defun delib-flow--project-proposal-fallback-child-item (_package)
  "Return explicit incomplete fallback child item for a new project proposal."
  (delib-flow--make-draft-action
   "No concrete child item could be derived from this source yet"
   'project-proposal))

(defun delib-flow--source-reference-note-type (package)
  "Return preferred source-note type for PACKAGE."
  (if (delib-flow--matched-project-title package)
      'project-support
    'general-pkm))

(defconst delib-flow--reference-note-durable-concept-regexp
  (concat
   "\\b\\("
   "adapted\\|advisor\\|advisors\\|approach\\|capture system\\|foundation"
   "\\|framework\\|habit\\|idea\\|model\\|moat\\|pattern\\|playbook"
   "\\|principle\\|prompt\\|strategy\\|system\\|workflow"
   "\\)\\b")
  "Regexp matching durable-concept phrasing for reference-note candidates.")

(defconst delib-flow--reference-note-generic-focus-regexp
  (concat
   "\\b\\("
   "cohort\\|newsletter\\|update\\|week [0-9]+\\|summary\\|overview\\|notes?"
   "\\|context\\|reminder"
   "\\)\\b")
  "Regexp matching overly generic reference-note focus terms.")

(defconst delib-flow--reference-note-heading-framing-regexp
  (concat
   "\\b\\("
   "intro\\(?:duction\\)?\\|overview\\|summary\\|recap\\|update\\|digest"
   "\\|foundations?\\|basics?\\|notes?\\|takeaways?\\|lesson\\(?:s\\)?"
   "\\|module\\|session\\|chapter\\|part\\|issue\\|edition\\|week\\|day"
   "\\)\\b")
  "Regexp matching generic framing terms in source-local headings.")

(defconst delib-flow--reference-note-deictic-focus-regexp
  (concat
   "\\b\\("
   "this\\|today\\|yesterday\\|tomorrow\\|current\\|latest\\|recent"
   "\\)\\b")
  "Regexp matching deictic focus terms that usually signal source-local phrasing.")

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

(defun delib-flow--reference-note-focus-terms (focus)
  "Return significant reusable terms from reference-note FOCUS."
  (seq-filter
   (lambda (word)
     (and (>= (length word) 4)
          (not (member word delib-flow--tag-suggestion-stopwords))
          (not (member word delib-flow--entity-noise-terms))))
   (delib-flow--string-words (downcase (or focus "")))))

(defun delib-flow--reference-note-source-local-heading-penalty (text)
  "Return a generic source-local heading penalty for TEXT."
  (let* ((normalized (downcase (delib-flow--normalize-reference-note-focus text)))
         (prefix (car (split-string normalized ":" t "[[:space:]]*")))
         (prefix-words (delib-flow--string-words (or prefix "")))
         (prefix-count (length prefix-words))
         (framing-count
          (seq-count
           (lambda (word)
             (string-match-p delib-flow--reference-note-heading-framing-regexp word))
           prefix-words))
         (framing-prefix-p
          (and (string-match-p ":" normalized)
               (> prefix-count 0)
               (>= framing-count (max 1 (1- prefix-count))))))
    (+ (if framing-prefix-p 4 0)
       (if (string-match-p delib-flow--reference-note-deictic-focus-regexp
                           normalized)
           3
         0)
       (if (and (string-match-p ":" normalized)
                (string-match-p delib-flow--reference-note-heading-framing-regexp
                                normalized))
           1
         0))))

(defun delib-flow--reference-note-focus-score (focus)
  "Return heuristic durable-concept score for reference-note FOCUS."
  (let* ((normalized (downcase (delib-flow--normalize-reference-note-focus focus)))
         (terms (delib-flow--reference-note-focus-terms normalized))
         (source-local-penalty
          (delib-flow--reference-note-source-local-heading-penalty normalized)))
    (+ (* 2 (length (delete-dups terms)))
       (if (string-match-p ":" normalized) 2 0)
       (if (string-match-p delib-flow--reference-note-durable-concept-regexp
                           normalized)
           4
         0)
       (if (string-match-p "\\b\\(how\\|why\\|instead of\\|when\\)\\b"
                           normalized)
           3
         0)
       (if (string-match-p delib-flow--reference-note-generic-focus-regexp
                           normalized)
           -4
         0)
       (- source-local-penalty))))

(defun delib-flow--reference-note-reuse-claim-for-focus (focus)
  "Return seeded reusable-note claim for reference-note FOCUS, or nil."
  (let ((normalized (delib-flow--normalize-reference-note-focus focus)))
    (when (>= (delib-flow--reference-note-focus-score normalized) 8)
      (cond
       ((string-match-p "\\badapted\\b" (downcase normalized))
        (format "Shows how `%s` can be adapted as a reusable operating pattern."
                normalized))
       ((string-match-p "\\badvisors?\\b" (downcase normalized))
        (format "Captures `%s` as a repeatable pattern that can inform later tool, workflow, or agent design."
                normalized))
       (t
        (format "Captures `%s` as a reusable concept rather than a one-off source summary."
                normalized))))))

(defun delib-flow--reference-note-focus-descriptor (focus)
  "Return structured deterministic descriptor for reference-note FOCUS."
  (let* ((normalized (delib-flow--normalize-reference-note-focus focus))
         (terms (delete-dups
                 (delib-flow--reference-note-focus-terms normalized))))
    (list :focus normalized
          :candidate-identity normalized
          :focus-terms terms
          :focus-score (delib-flow--reference-note-focus-score normalized)
          :reuse-claim (delib-flow--reference-note-reuse-claim-for-focus normalized))))

(defun delib-flow--reference-note-focus-overlap-p (left right)
  "Return non-nil when focus descriptors LEFT and RIGHT overlap materially."
  (let* ((left-terms (plist-get left :focus-terms))
         (right-terms (plist-get right :focus-terms))
         (shared (seq-intersection left-terms right-terms #'string=))
         (shared-count (length shared))
         (min-count (max 1 (min (length left-terms) (length right-terms)))))
    (or (equal (plist-get left :candidate-identity)
               (plist-get right :candidate-identity))
        (and (>= shared-count 2)
             (>= (/ (float shared-count) min-count) 0.6)))))

(defun delib-flow--promote-reference-note-focus-descriptors (descriptors)
  "Return DESCRIPTORS with weaker overlapping focuses removed."
  (let ((ordered (sort (copy-sequence descriptors)
                       (lambda (left right)
                         (> (plist-get left :focus-score)
                            (plist-get right :focus-score)))))
        kept)
    (dolist (descriptor ordered (nreverse kept))
      (unless (seq-some
               (lambda (existing)
                 (delib-flow--reference-note-focus-overlap-p descriptor existing))
               kept)
        (push descriptor kept)))))

(defun delib-flow--reference-note-item-focus-text (item)
  "Return primary focus text for reference-note ITEM."
  (or (plist-get item :candidate-focus)
      (plist-get item :candidate-identity)
      (delib-flow--reference-note-title item)
      (plist-get item :text)))

(defun delib-flow--reference-note-item-focus-score (item descriptor)
  "Return merged focus score for reference-note ITEM and DESCRIPTOR."
  (max (or (plist-get item :focus-score) 0)
       (or (plist-get descriptor :focus-score) 0)))

(defun delib-flow--reference-note-item-candidate-identity (item descriptor)
  "Return candidate identity for reference-note ITEM and DESCRIPTOR."
  (or (plist-get item :candidate-identity)
      (plist-get descriptor :candidate-identity)))

(defun delib-flow--reference-note-item-reuse-claim (item descriptor)
  "Return reuse claim for reference-note ITEM and DESCRIPTOR."
  (or (plist-get item :reuse-claim)
      (plist-get descriptor :reuse-claim)))

(defconst delib-flow--reference-note-item-source-score-alist
  '((local-llm . 3)
    (source . 2)
    (retained-context . 1))
  "Promotion bonus by reference-note candidate source.")

(defun delib-flow--reference-note-item-focus-descriptor (item)
  "Return a comparable focus descriptor derived from reference-note ITEM."
  (let* ((focus (delib-flow--reference-note-item-focus-text item))
         (descriptor (delib-flow--reference-note-focus-descriptor focus)))
    (plist-put
     (plist-put
      (plist-put descriptor
                 :focus-score
                 (delib-flow--reference-note-item-focus-score item descriptor))
      :candidate-identity
      (delib-flow--reference-note-item-candidate-identity item descriptor))
     :reuse-claim
     (delib-flow--reference-note-item-reuse-claim item descriptor))))

(defun delib-flow--reference-note-item-source-score (item)
  "Return promotion bonus for reference-note ITEM source."
  (or (alist-get (plist-get item :source)
                 delib-flow--reference-note-item-source-score-alist)
      0))

(defun delib-flow--reference-note-item-source-local-penalty (item)
  "Return promotion penalty for source-local heading residue in ITEM."
  (delib-flow--reference-note-source-local-heading-penalty
   (delib-flow--reference-note-item-focus-text item)))

(defun delib-flow--reference-note-item-promotion-score (item)
  "Return final-promotion score for reference-note ITEM."
  (let ((descriptor (delib-flow--reference-note-item-focus-descriptor item)))
    (+ (* 3 (or (plist-get descriptor :focus-score) 0))
       (if (plist-get descriptor :reuse-claim) 6 0)
       (delib-flow--reference-note-item-source-score item)
       (- (delib-flow--draft-item-warning-count item))
       (- (delib-flow--reference-note-item-source-local-penalty item)))))

(defun delib-flow--proposed-project-item (package)
  "Return deterministic proposed project artifact derived from PACKAGE."
  (let* ((child-items (or (delib-flow--project-proposal-derived-child-items package)
                          (list (delib-flow--project-proposal-fallback-child-item
                                 package))))
         (item
          (delib-flow--project-with-child-items
           (delib-flow--make-draft-project
            (delib-flow--project-proposal-title package)
            'active
            (car child-items)
            (delib-flow--project-proposal-tags package))
           child-items)))
    (delib-flow--draft-item-with-warnings
     (delib-flow--draft-item-with-tag-suggestions item package)
     (delib-flow--project-proposal-warning-list item package))))

(defun delib-flow--normalize-inspect-source-output (raw-output)
  "Return normalized inspect-source text from RAW-OUTPUT."
  (format
   "- Source type: %s\n- Source type reason: %s\n- Operator intent: %s\n- Operator intent influenced classification: %s\n- Title: %s\n- Outline path: %s\n- Body lines: %s\n- Content words: %s\n- Source has ID: %s\n- Contact emails: %s\n- Org file links: %s\n- Body preview: %s"
   (plist-get raw-output :source-type)
   (plist-get raw-output :source-type-reason)
   (if (plist-get raw-output :operator-intent-present)
       (plist-get raw-output :operator-intent)
     "none")
   (if (plist-get raw-output :operator-intent-influenced) "yes" "no")
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

(defun delib-flow--normalize-find-support-for-selected-output (raw-output)
  "Return normalized focused-support text from RAW-OUTPUT."
  (format "- Family: %s\n- Focused support count: %s\n- Support candidates:\n%s"
          (plist-get raw-output :family)
          (length (or (plist-get raw-output :support-candidates) nil))
          (or (plist-get raw-output :support-context)
              "No focused support material is attached yet.")))

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
  (format "- Candidate count: %s\n- Operator intent: %s\n- Project context: %s%s\n- Prior attempts: %s\n- Similar to previous attempt: %s\n%s\n%s"
          (plist-get raw-output :candidate-count)
          (if (delib-flow--non-empty-string-p (plist-get raw-output :operator-intent))
              (plist-get raw-output :operator-intent)
            "none")
          (or (plist-get raw-output :project-context-kind) 'none)
          (if-let ((title (plist-get raw-output :project-context-title)))
              (format " (%s)" title)
            "")
          (or (plist-get raw-output :previous-attempt-count) 0)
          (if (plist-get raw-output :similar-to-previous-p) "yes" "no")
          (delib-flow--normalize-proposal-warning-summary raw-output)
          (delib-flow--normalize-draft-items-with-warnings
           (plist-get raw-output :actions))))

(defun delib-flow--normalize-extract-waiting-for-output (raw-output)
  "Return normalized extract-waiting-for text from RAW-OUTPUT."
  (format "- Candidate count: %s\n- Operator intent: %s\n- Project context: %s%s\n- Prior attempts: %s\n- Similar to previous attempt: %s\n%s\n%s"
          (plist-get raw-output :candidate-count)
          (if (delib-flow--non-empty-string-p (plist-get raw-output :operator-intent))
              (plist-get raw-output :operator-intent)
            "none")
          (or (plist-get raw-output :project-context-kind) 'none)
          (if-let ((title (plist-get raw-output :project-context-title)))
              (format " (%s)" title)
            "")
          (or (plist-get raw-output :previous-attempt-count) 0)
          (if (plist-get raw-output :similar-to-previous-p) "yes" "no")
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

(defun delib-flow--normalize-draft-selected-action-output (raw-output)
  "Return normalized selected-action draft text from RAW-OUTPUT."
  (let* ((item (plist-get raw-output :drafted-item))
         (candidate (plist-get raw-output :candidate)))
    (format "- Selected action: %s\n- Original action: %s\n- Reason: %s"
            (or (plist-get item :text) "Untitled action")
            (or (plist-get candidate :text) "Untitled action")
            (or (plist-get raw-output :reason) "No reason recorded."))))

(defun delib-flow--normalize-draft-selected-waiting-for-output (raw-output)
  "Return normalized selected waiting-for draft text from RAW-OUTPUT."
  (let* ((item (plist-get raw-output :drafted-item))
         (candidate (plist-get raw-output :candidate)))
    (format "- Selected waiting-for: %s\n- Original waiting-for: %s\n- Reason: %s"
            (or (plist-get item :text) "Untitled waiting-for")
            (or (plist-get candidate :text) "Untitled waiting-for")
            (or (plist-get raw-output :reason) "No reason recorded."))))

(defun delib-flow--normalize-draft-selected-reference-note-output (raw-output)
  "Return normalized selected-note draft text from RAW-OUTPUT."
  (let* ((item (plist-get raw-output :drafted-item))
         (draft-body (or (plist-get item :draft-body) "")))
    (format "- Selected note: %s\n- Note type: %s\n- Reason: %s\n\n#+begin_example\n%s\n#+end_example"
            (or (delib-flow--reference-note-title item) "Untitled note")
            (or (plist-get item :note-type) 'general-pkm)
            (or (plist-get raw-output :reason) "No reason recorded.")
            (string-trim-right draft-body))))

(defun delib-flow--normalize-draft-selected-reference-note-part-output (raw-output)
  "Return normalized selected-note part draft text from RAW-OUTPUT."
  (let* ((item (plist-get raw-output :drafted-item))
         (part-id (plist-get raw-output :part-id))
         (part-label (or (delib-flow--reference-note-part-stage-label part-id)
                         "note part"))
         (part-text (or (plist-get raw-output :part-text) ""))
         (reason (let ((value (plist-get raw-output :reason)))
                   (if (delib-flow--non-empty-string-p value)
                       value
                     (format "Regenerated only the selected note %s."
                             part-label)))))
    (format "- Selected note: %s\n- Regenerated part: %s\n- Reason: %s\n\n#+begin_example\n%s\n#+end_example"
            (or (delib-flow--reference-note-title item) "Untitled note")
            part-label
            reason
            (string-trim-right part-text))))

(defun delib-flow--normalize-draft-selected-project-output (raw-output)
  "Return normalized selected-project draft text from RAW-OUTPUT."
  (let* ((item (plist-get raw-output :drafted-item))
         (first-item (delib-flow--project-first-item item)))
    (format "- Selected project: %s\n- State: %s\n- First item: %s\n- Attached child item count: %s\n- Tags: %s\n- Reason: %s"
            (or (plist-get item :title) "Untitled project")
            (or (plist-get item :state) 'active)
            (or (plist-get first-item :text) "none")
            (length (delib-flow--project-child-items item))
            (if-let ((tags (plist-get item :tags)))
                (string-join tags ", ")
              "none")
            (or (plist-get raw-output :reason) "No reason recorded."))))

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

(defun delib-flow--same-artifact-item-p (left right)
  "Return non-nil when LEFT and RIGHT refer to the same draft artifact."
  (or (equal left right)
      (and left
           right
           (eq (plist-get left :kind) (plist-get right :kind))
           (equal (delib-flow--artifact-candidate-id left)
                  (delib-flow--artifact-candidate-id right)))))

(defun delib-flow--draft-evidence-snapshot-candidate (run family draft)
  "Return the candidate that best explains FAMILY DRAFT in RUN."
  (or (delib-flow--artifact-family-selected-candidate run family)
      draft))

(defun delib-flow--draft-evidence-snapshot-support-candidates (run family draft)
  "Return focused support candidates for FAMILY DRAFT in RUN."
  (or (plist-get draft :support-candidates)
      (delib-flow--artifact-family-selected-support-candidates run family)))

(defun delib-flow--draft-evidence-snapshot-support-context (run family draft)
  "Return focused support context text for FAMILY DRAFT in RUN."
  (or (plist-get draft :support-context)
      (delib-flow--artifact-family-selected-support-context run family)))

(defun delib-flow--draft-evidence-snapshot-source-excerpts (run evidence-lines)
  "Return deduped source excerpts for EVIDENCE-LINES in RUN."
  (let ((matched
         (delete-dups
          (delq nil
                (mapcar (lambda (line)
                          (delib-flow--format-source-evidence-excerpt run line))
                        evidence-lines)))))
    (or matched
        (delete-dups
         (mapcar (lambda (line)
                   (format "- Source: %s" line))
                 (seq-take (delib-flow--clean-source-view-lines run) 3))))))

(defun delib-flow--support-candidate-snapshots (support-candidates)
  "Return frozen support items for SUPPORT-CANDIDATES."
  (and support-candidates
       (mapcar #'delib-flow--support-candidate-snapshot support-candidates)))

(defun delib-flow--draft-evidence-snapshot-support-lines (support-items)
  "Return rendered support lines for SUPPORT-ITEMS."
  (and support-items
       (mapcar (lambda (item)
                 (plist-get item :line))
               support-items)))

(defun delib-flow--draft-evidence-snapshot-context-lines (support-context)
  "Return trimmed support-context lines from SUPPORT-CONTEXT."
  (when support-context
    (seq-filter
     (lambda (line)
       (not (string-empty-p line)))
     (mapcar #'string-trim (split-string support-context "\n")))))

(defun delib-flow--draft-quality-gap-warning-item (warning)
  "Return structured quality-gap item for WARNING."
  (list :identity (symbol-name (plist-get warning :code))
        :line (format "- %s%s"
                      (plist-get warning :message)
                      (if-let ((fix (delib-flow--draft-item-warning-remediation warning)))
                          (format " Next fix: %s" fix)
                        ""))))

(defun delib-flow--draft-quality-gap-warning-items (draft)
  "Return structured warning-derived quality-gap items for DRAFT."
  (mapcar #'delib-flow--draft-quality-gap-warning-item
          (delib-flow--draft-item-warnings draft)))

(defun delib-flow--draft-quality-gap-support-item ()
  "Return structured quality-gap item for missing focused support."
  (list :identity "no-focused-support"
        :line "- No focused support is attached yet. The current draft still leans on source-local context only."))

(defun delib-flow--draft-quality-gap-items (run family draft)
  "Return structured quality-gap items for selected FAMILY DRAFT in RUN."
  (let ((items (delib-flow--draft-quality-gap-warning-items draft)))
    (unless (delib-flow--artifact-family-selected-support-candidates run family)
      (setq items (append items (list (delib-flow--draft-quality-gap-support-item)))))
    items))

(defun delib-flow--draft-item-has-warning-code-p (item code)
  "Return non-nil when ITEM includes warning CODE."
  (seq-some (lambda (warning)
              (eq (plist-get warning :code) code))
            (delib-flow--draft-item-warnings item)))

(defun delib-flow--action-target-fragment (text)
  "Return compact target cue extracted from action TEXT, or nil."
  (when (string-match
         "\\b\\(to [^,.;]+\\|for [^,.;]+\\|with [^,.;]+\\|about [^,.;]+\\)\\b"
         (or text ""))
    (string-trim (match-string 1 text))))

(defun delib-flow--waiting-for-owner-fragment (text)
  "Return compact owner cue extracted from waiting-for TEXT, or nil."
  (let ((value (or text "")))
    (or (when (string-match
               "\\`[Ww]aiting for[[:space:]]+\\(.+?\\)\\(?:[[:space:]]+to\\b\\|[[:space:]]*\\.[[:space:]]*\\'\\|\\'\\)"
               value)
          (string-trim (match-string 1 value)))
        (when (string-match "\\bfrom[[:space:]]+\\([^,.;]+\\)" value)
          (string-trim (match-string 1 value))))))

(defun delib-flow--waiting-for-outcome-fragment (text)
  "Return compact blocked-outcome cue extracted from waiting-for TEXT, or nil."
  (when (string-match "\\bto[[:space:]]+\\([^,.;]+\\)" (or text ""))
    (string-trim (match-string 1 text))))

(defun delib-flow--project-tags-summary (tags)
  "Return readable tag summary for project TAGS."
  (if tags
      (string-join tags ", ")
    "none"))

(defconst delib-flow--family-evidence-review-heading-alist
  '((actions . "Action-specific checks")
    (waiting-fors . "Waiting-for-specific checks")
    (reference-notes . "Note-specific checks")
    (project-proposals . "Project-specific checks"))
  "Per-family headings for selected-draft evidence review.")

(defun delib-flow--action-evidence-review-opening-line (draft)
  "Return opening-verb review line for action DRAFT."
  (if-let ((verb (delib-flow--artifact-leading-word (plist-get draft :text))))
      (format "Opening verb: `%s`; %s"
              verb
              (if (delib-flow--weak-next-action-verb-p verb)
                  "this still reads more like review or clarification than execution."
                "this reads like a directly executable next step."))
    "Opening verb: none detected yet, so execution intent is still weak."))

(defun delib-flow--action-evidence-review-outcome-line (draft)
  "Return outcome-cue review line for action DRAFT."
  (if-let ((target (delib-flow--action-target-fragment (plist-get draft :text))))
      (format "Outcome cue: `%s`." target)
    "Outcome cue: no explicit recipient, target, or deliverable is named yet."))

(defun delib-flow--action-evidence-review-scope-line (draft)
  "Return scope review line for action DRAFT."
  (let ((scope (delib-flow--artifact-text-word-count (plist-get draft :text))))
    (format "Scope check: %s word%s; %s"
            scope
            (if (= scope 1) "" "s")
            (if (> scope 12)
                "this still looks broad for one local next step."
              "this remains task-sized."))))

(defun delib-flow--action-evidence-review-source-line (snapshot)
  "Return source-alignment review line for action SNAPSHOT."
  (if-let ((evidence-line (car (plist-get snapshot :source-evidence-lines))))
      (format "Source alignment: the current wording still traces back to `%s`."
              evidence-line)
    "Source alignment: no compact source action line was captured."))

(defun delib-flow--action-evidence-review-lines (draft snapshot)
  "Return family-specific evidence review lines for action DRAFT and SNAPSHOT."
  (list
   (delib-flow--action-evidence-review-opening-line draft)
   (delib-flow--action-evidence-review-outcome-line draft)
   (delib-flow--action-evidence-review-scope-line draft)
   (delib-flow--action-evidence-review-source-line snapshot)))

(defun delib-flow--waiting-for-evidence-review-phrasing-line (draft)
  "Return waiting-state phrasing review line for waiting-for DRAFT."
  (if (string-prefix-p "waiting for" (downcase (or (plist-get draft :text) "")))
      "Waiting-state phrasing: explicit `Waiting for ...` wording is present."
    "Waiting-state phrasing: explicit `Waiting for ...` wording is still missing."))

(defun delib-flow--waiting-for-evidence-review-owner-line (draft)
  "Return owner review line for waiting-for DRAFT."
  (if-let ((owner (delib-flow--waiting-for-owner-fragment (plist-get draft :text))))
      (format "Owner cue: `%s` currently owns the response or dependency." owner)
    "Owner cue: the responsible person or party is still not clear."))

(defun delib-flow--waiting-for-evidence-review-outcome-line (draft)
  "Return blocked-outcome review line for waiting-for DRAFT."
  (if-let ((outcome (delib-flow--waiting-for-outcome-fragment (plist-get draft :text))))
      (format "Blocked outcome: `%s` is the current missing response or deliverable." outcome)
    "Blocked outcome: the exact response, approval, or deliverable is still vague."))

(defun delib-flow--waiting-for-evidence-review-source-line (snapshot)
  "Return source-alignment review line for waiting-for SNAPSHOT."
  (if-let ((evidence-line (car (plist-get snapshot :source-evidence-lines))))
      (format "Source alignment: the dependency still traces back to `%s`."
              evidence-line)
    "Source alignment: no compact waiting-for evidence line was captured."))

(defun delib-flow--waiting-for-evidence-review-lines (draft snapshot)
  "Return family-specific evidence review lines for waiting-for DRAFT and SNAPSHOT."
  (list
   (delib-flow--waiting-for-evidence-review-phrasing-line draft)
   (delib-flow--waiting-for-evidence-review-owner-line draft)
   (delib-flow--waiting-for-evidence-review-outcome-line draft)
   (delib-flow--waiting-for-evidence-review-source-line snapshot)))

(defun delib-flow--reference-note-evidence-review-type-line (draft)
  "Return note-type review line for reference-note DRAFT."
  (format "Note type: `%s`." (or (plist-get draft :note-type) 'unknown)))

(defun delib-flow--reference-note-evidence-review-title-line (draft)
  "Return title review line for reference-note DRAFT."
  (if (delib-flow--draft-item-has-warning-code-p draft 'reference-note-missing-title)
      "Title check: the note title is still too weak for deterministic filing."
    (format "Title check: `%s` is the current durable note identity."
            (or (delib-flow--reference-note-title draft) "Untitled note"))))

(defun delib-flow--reference-note-evidence-review-balance-line (snapshot)
  "Return evidence-balance review line for reference-note SNAPSHOT."
  (let ((support-count (or (plist-get snapshot :support-count) 0))
        (excerpt-count (length (or (plist-get snapshot :source-excerpts) nil))))
    (if (> support-count 0)
        (format "Evidence balance: %s source excerpt(s) and %s focused support item(s) are attached."
                excerpt-count support-count)
      (format "Evidence balance: %s source excerpt(s) are attached, but this note still relies on source-only evidence."
              excerpt-count))))

(defun delib-flow--reference-note-evidence-review-reuse-line (draft)
  "Return reuse or project-anchoring review line for reference-note DRAFT."
  (let ((note-type (plist-get draft :note-type)))
    (cond
     ((and (eq note-type 'general-pkm)
           (delib-flow--draft-item-has-warning-code-p
            draft 'reference-note-reuse-justification))
      "Reuse check: the broader reusable idea is still weakly justified.")
     ((eq note-type 'project-support)
      "Project anchoring: this draft is shaped as project-support material rather than a standalone evergreen note.")
     (t
      "Reuse check: the current title and evidence shape a reusable note candidate."))))

(defun delib-flow--reference-note-evidence-review-lines (draft snapshot)
  "Return family-specific evidence review lines for reference-note DRAFT and SNAPSHOT."
  (list
   (delib-flow--reference-note-evidence-review-type-line draft)
   (delib-flow--reference-note-evidence-review-title-line draft)
   (delib-flow--reference-note-evidence-review-balance-line snapshot)
   (delib-flow--reference-note-evidence-review-reuse-line draft)))

(defun delib-flow--project-evidence-review-title-line (draft)
  "Return title-identity review line for project DRAFT."
  (let ((title (or (plist-get draft :title) "Untitled project")))
    (if (or (delib-flow--draft-item-has-warning-code-p draft 'project-title-timestamp-noise)
            (delib-flow--draft-item-has-warning-code-p draft 'project-title-note-shape))
        (format "Title identity: `%s` still reads more like source residue than a stable project name." title)
      (format "Title identity: `%s` now reads like the working project name." title))))

(defun delib-flow--project-evidence-review-first-item-line (draft)
  "Return first-item review line for project DRAFT."
  (let ((first-item (or (plist-get (plist-get draft :first-item) :text) "none")))
    (if (delib-flow--draft-item-has-warning-code-p draft 'project-first-item-generic)
        (format "First-item check: `%s` is still a placeholder rather than a concrete start step." first-item)
      (format "First-item check: `%s` is the current concrete starting move." first-item))))

(defun delib-flow--project-evidence-review-tags-line (draft)
  "Return tag review line for project DRAFT."
  (let ((summary (delib-flow--project-tags-summary (plist-get draft :tags))))
    (if (delib-flow--draft-item-has-warning-code-p draft 'project-tags-invalid)
        (format "Tag check: `%s` still includes weak numeric or date-like tags." summary)
      (format "Tag check: `%s` is the current project tag set." summary))))

(defun delib-flow--project-evidence-review-state-line (draft)
  "Return project-state review line for DRAFT."
  (format "Project state: `%s`." (or (plist-get draft :state) 'active)))

(defun delib-flow--project-evidence-review-lines (draft _snapshot)
  "Return family-specific evidence review lines for project DRAFT."
  (list
   (delib-flow--project-evidence-review-title-line draft)
   (delib-flow--project-evidence-review-first-item-line draft)
   (delib-flow--project-evidence-review-tags-line draft)
   (delib-flow--project-evidence-review-state-line draft)))

(defconst delib-flow--family-evidence-review-lines-dispatch
  '((actions . delib-flow--action-evidence-review-lines)
    (waiting-fors . delib-flow--waiting-for-evidence-review-lines)
    (reference-notes . delib-flow--reference-note-evidence-review-lines)
    (project-proposals . delib-flow--project-evidence-review-lines))
  "Per-family renderers for selected-draft review details.")

(defun delib-flow--family-evidence-review-heading (family)
  "Return section heading for evidence review FAMILY."
  (or (alist-get family delib-flow--family-evidence-review-heading-alist)
      "Artifact-specific checks"))

(defun delib-flow--family-evidence-review-lines-for-snapshot (family draft snapshot)
  "Return family-specific evidence-review lines for FAMILY DRAFT and SNAPSHOT."
  (if-let ((renderer (alist-get family delib-flow--family-evidence-review-lines-dispatch)))
      (funcall renderer draft snapshot)
    (list "No family-specific review checks are defined yet.")))

(defun delib-flow--build-draft-evidence-snapshot (run family draft)
  "Return frozen evidence snapshot for FAMILY DRAFT in RUN."
  (let* ((candidate
          (delib-flow--draft-evidence-snapshot-candidate run family draft))
         (support-candidates
          (delib-flow--draft-evidence-snapshot-support-candidates
           run family draft))
         (support-context
          (delib-flow--draft-evidence-snapshot-support-context
           run family draft))
         (evidence-lines (delib-flow--selected-draft-evidence-lines run family))
         (source-excerpts
          (delib-flow--draft-evidence-snapshot-source-excerpts
           run evidence-lines))
         (support-items
          (delib-flow--support-candidate-snapshots
           support-candidates))
         (quality-gap-items
          (delib-flow--draft-quality-gap-items run family draft)))
    (list :draft-reason
          (or (plist-get draft :draft-reason)
              "No explicit drafting rationale is recorded.")
          :candidate-origin
          (plist-get candidate :source)
          :source-title
          (delib-flow--source-display-title run)
          :source-evidence-lines evidence-lines
          :source-excerpts source-excerpts
          :support-items support-items
          :support-lines
          (delib-flow--draft-evidence-snapshot-support-lines support-items)
          :support-context-lines
          (delib-flow--draft-evidence-snapshot-context-lines support-context)
          :family-review-heading
          (delib-flow--family-evidence-review-heading family)
          :family-review-lines
          (delib-flow--family-evidence-review-lines-for-snapshot
           family draft
           (list :source-evidence-lines evidence-lines
                 :source-excerpts source-excerpts
                 :support-items support-items
                 :support-count (length support-candidates)))
          :quality-gap-items quality-gap-items
          :quality-gaps
          (if quality-gap-items
              (mapcar (lambda (item) (plist-get item :line)) quality-gap-items)
            '("- No immediate quality gaps are flagged. This draft is currently supported by the available evidence and warning checks."))
          :warning-count
          (length (delib-flow--draft-item-warnings draft))
          :support-count
          (length support-candidates))))

(defun delib-flow--draft-evidence-snapshot (run family draft)
  "Return evidence snapshot for FAMILY DRAFT in RUN."
  (or (plist-get draft :evidence-snapshot)
      (and draft
           (delib-flow--build-draft-evidence-snapshot run family draft))))

(defun delib-flow--promote-reference-note-items (items)
  "Return reference-note ITEMS with weaker overlapping variants removed."
  (let ((ordered (sort (copy-sequence items)
                       (lambda (left right)
                         (> (delib-flow--reference-note-item-promotion-score left)
                            (delib-flow--reference-note-item-promotion-score right)))))
        kept)
    (dolist (item ordered (nreverse kept))
      (unless (seq-some
               (lambda (existing)
                 (delib-flow--reference-note-focus-overlap-p
                  (delib-flow--reference-note-item-focus-descriptor item)
                  (delib-flow--reference-note-item-focus-descriptor existing)))
               kept)
        (push item kept)))))

(defun delib-flow--email-digest-reference-note-focuses (digest)
 "Return deterministic durable note focuses extracted from email DIGEST."
 (let* ((subject
         (delib-flow--normalize-reference-note-focus
          (plist-get digest :subject)))
        (lines
         (split-string (or (plist-get digest :plain-body) "") "\n"))
        body-focuses)
   (while lines
     (let* ((line (car lines))
            (next (cadr lines))
            (trimmed (string-trim line)))
       (cond
        ((and (string-match-p "\\`[-=_[:space:]]\\{3,\\}\\'" trimmed)
              next (delib-flow--reference-note-focus-usable-p next))
         (push (delib-flow--normalize-reference-note-focus next)
               body-focuses))
        ((string-match "highlight here is \\(.+?\\)\\(?:[.?!]\\|$\\)"
                       trimmed)
         (push
          (delib-flow--normalize-reference-note-focus
           (match-string 1 trimmed))
          body-focuses))
        ((string-match "was about \\(.+?\\)\\(?:[.?!]\\|$\\)" trimmed)
         (push
          (delib-flow--normalize-reference-note-focus
           (match-string 1 trimmed))
          body-focuses))
        ((string-match
          "\\`The idea:[[:space:]]*\\(.+?\\)\\(?:[.?!]\\|$\\)" trimmed)
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
     (seq-take (delete-dups (append normalized-body subject-focus)) 4))))

(defun delib-flow--source-reference-note-focus-descriptors (package)
  "Return structured deterministic source-note focus descriptors for PACKAGE."
  (let ((digest (delib-flow--package-email-digest package)))
    (if digest
        (delib-flow--promote-reference-note-focus-descriptors
         (mapcar #'delib-flow--reference-note-focus-descriptor
                 (delib-flow--email-digest-reference-note-focuses digest)))
      (let ((title (delib-flow--normalize-reference-note-focus
                    (delib-flow--source-display-title package))))
        (if (delib-flow--reference-note-focus-usable-p title)
            (list (delib-flow--reference-note-focus-descriptor title))
          nil)))))

(defun delib-flow--source-reference-notes (package)
  "Return deterministic source-derived reference notes for PACKAGE."
  (unless (and (delib-flow--package-transactional-email-p package)
               (not (delib-flow--matched-project-title package)))
    (let ((note-type (delib-flow--source-reference-note-type package)))
      (mapcar
       (lambda (descriptor)
         (let* ((focus (plist-get descriptor :focus))
                (item
                 (delib-flow--make-draft-reference-note
                  (if (eq note-type 'project-support)
                      (format "Create project support note from %s" focus)
                    (format "Create general PKM note for %s" focus))
                  'source
                  note-type)))
           (setq item (plist-put item :candidate-focus focus))
           (setq item (plist-put item :candidate-identity
                                 (plist-get descriptor :candidate-identity)))
           (setq item (plist-put item :focus-score
                                 (plist-get descriptor :focus-score)))
           (setq item (plist-put item :reuse-claim
                                 (plist-get descriptor :reuse-claim)))
           item))
       (delib-flow--source-reference-note-focus-descriptors package)))))

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
    (when-let ((text (delib-flow--zk-note-text file)))
      (seq-find
       #'delib-flow--candidate-note-focus-line-p
       (seq-filter
        (lambda (line)
          (and (not (string-empty-p line))
               (not (string-match-p "\\`#\\+" line))
               (not (string-match-p "\\`\\*+ " line))))
        (mapcar #'string-trim
                (split-string text "\n")))))))

(defun delib-flow--candidate-note-focus (candidate)
  "Return best support-note focus text from retained CANDIDATE."
  (or (when-let ((line (delib-flow--candidate-note-focus-line candidate)))
        (string-trim-right line "[[:space:].:;,-]+"))
      (plist-get candidate :title)))

(defun delib-flow--retained-candidate-reference-note (candidate)
  "Return a support-note item derived from retained CANDIDATE."
  (delib-flow--make-draft-reference-note
   (format "Create project support note from %s"
           (delib-flow--candidate-note-focus candidate))
   'retained-context
   'project-support))

(defun delib-flow--retained-candidate-reference-notes (package)
  "Return retained-candidate reference-note items for PACKAGE."
  (when (delib-flow--effective-project-title package)
    (mapcar #'delib-flow--retained-candidate-reference-note
            (delib-flow--retained-candidates package))))

(defun delib-flow--reference-note-content-needs-seed-p (content)
  "Return non-nil when note CONTENT should receive seeded structure."
  (< (length (delib-flow--reference-note-body-lines content)) 3))

(defun delib-flow--reference-note-draft-has-workspace-structure-p (draft-body)
  "Return non-nil when DRAFT-BODY already has filing workspace sections."
  (let ((body (or draft-body "")))
    (and (string-match-p "^\\* Working draft$" body)
         (string-match-p "^\\* Source highlights$" body)
         (string-match-p "^\\* Related material to connect$" body))))

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

(defun delib-flow--reference-note-draft-body-shares-identity-p (item draft-body)
  "Return non-nil when ITEM DRAFT-BODY still mentions the selected identity."
  (let* ((identity-terms
          (delib-flow--reference-note-focus-terms
           (or (plist-get item :candidate-identity)
               (delib-flow--reference-note-title item))))
         (body-words (delib-flow--string-words (downcase (or draft-body "")))))
    (seq-intersection identity-terms body-words #'string=)))

(defun delib-flow--reference-note-draft-drift-p (item draft-body)
  "Return non-nil when ITEM DRAFT-BODY drifts from its selected identity."
  (and (delib-flow--non-empty-string-p draft-body)
       (not (delib-flow--reference-note-draft-body-shares-identity-p
             item draft-body))
       (not (delib-flow--reference-note-draft-body-grounded-p
             draft-body item))))

(defun delib-flow--reference-note-draft-drift-warning (item draft-body)
  "Return warning when ITEM DRAFT-BODY drifts away from selected note identity."
  (let ((identity (or (plist-get item :candidate-identity)
                      (delib-flow--reference-note-title item))))
    (when (delib-flow--reference-note-draft-drift-p item draft-body)
      (delib-flow--make-artifact-warning
       'reference-note-draft-drift
       (format
        "Draft body drifts away from the selected note concept `%s`; retitle, fork, or rewrite before saving."
        identity)))))

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
        (not (string-match-p "\\`\\*+" (string-trim-left (or line ""))))
        (string-match-p "[[:alnum:]]" text)
        (not (string-match-p "\\`[()\\[\\]{}]+\\'" text))
        (not (string-match-p "\\`https?:" (downcase text)))
        (not
         (string-match-p
          "\\`\\(?:From\\|To\\|Cc\\|Bcc\\|Subject\\|Date\\|Reply-To\\):"
          text))
        (not
         (string-match-p "\\breferenced materials?\\b" (downcase text))))))

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
             (delib-flow--non-empty-string-p focus))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (let ((case-fold-search t))
            (if (re-search-forward
                 (format "^%s[[:space:]]*$" (regexp-quote focus))
                 nil t)
                (buffer-substring-no-properties (point) (point-max))
              text)))
      text)))

(defun delib-flow--reference-note-source-units (item package)
 "Return cleaned paragraph-like source units for reference-note ITEM in PACKAGE."
 (let* ((text
         (or
          (delib-flow--reference-note-relevant-source-text
           item package)
          ""))
        (paragraphs
         (split-string text
                       "\n[[:space:]\n]*\n+"
                       t))
        units)
   (dolist (paragraph paragraphs (nreverse units))
     (let* ((lines (split-string paragraph "\n"))
            (cleaned-lines
             (delq nil
                   (mapcar
                    (lambda (line)
                      (let ((trimmed (string-trim line)))
                        (when (and (not (string-empty-p trimmed))
                                   (not
                                    (string-match-p
                                     "\\`[-=_[:space:]]\\{3,\\}\\'"
                                     trimmed))
                                   (not
                                    (string-match-p
                                     "\\b\\(?:unsubscribe\\|view in browser\\|manage preferences\\)\\b"
                                     (downcase trimmed)))
                                   (delib-flow--reference-note-useful-source-line-p
                                    trimmed))
                          (delib-flow--reference-note-clean-line
                           trimmed))))
                    lines)))
            (unit (string-trim
                   (string-join cleaned-lines " "))))
       (dolist (cleaned-line cleaned-lines)
         (when (and (not (string-empty-p cleaned-line))
                    (string-match-p "[[:alnum:]]"
                                    cleaned-line))
           (push cleaned-line units)))
       (when (and (not (string-empty-p unit))
                  (string-match-p "[[:alnum:]]"
                                  unit))
         (push unit units))))))

(defun delib-flow--reference-note-split-unit-sentences (unit)
  "Return sentence-like fragments from reference-note UNIT."
  (let ((parts (split-string unit "\\(?:[.?!]\\)[[:space:]]+" t)))
    (if (> (length parts) 1)
        (mapcar #'string-trim parts)
      (list (string-trim unit)))))

(defun delib-flow--reference-note-highlight-fragment-words (fragment)
  "Return word list for source-highlight FRAGMENT."
  (delib-flow--string-words (downcase (or fragment ""))))

(defun delib-flow--reference-note-highlight-fragment-p (fragment)
  "Return non-nil when FRAGMENT is compact enough for source highlights."
  (let* ((cleaned (delib-flow--reference-note-clean-line fragment))
         (lower (downcase cleaned))
         (words (delib-flow--reference-note-highlight-fragment-words cleaned))
         (count (length words)))
    (and (delib-flow--non-empty-string-p cleaned)
         (>= count 4)
         (<= count 32)
         (not (string-match-p "\\b\\(?:quick update\\|view in browser\\|manage preferences\\|unsubscribe\\|preview\\)\\b"
                              lower))
         (not (string-match-p "\\`\\(?:hi\\|hello\\|thanks\\|cheers\\)\\b" lower)))))

(defun delib-flow--reference-note-highlight-fragment-extractive-p (fragment)
  "Return non-nil when FRAGMENT is safe to surface as extractive note evidence."
  (let ((cleaned (delib-flow--reference-note-clean-line fragment)))
    (and (delib-flow--reference-note-highlight-fragment-p cleaned)
         (not (string-match-p "\\b\\(?:week [0-9]+\\|cohort\\|session\\|waitlist\\|join the next cohort\\)\\b"
                              (downcase cleaned))))))

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
            (if (string-match-p "\\bcohort\\b" lower) 2 0)
            (if (string-match-p "\\blaying the foundation\\b" lower) 3 0)
            (if (string-match-p "\\bmaster prompt\\b" lower) 3 0)
            (if (string-match-p "\\bpara\\b" lower) 2 0))))
   (- (+ term-score concept-score) penalty)))

(defun delib-flow--reference-note-highlight-candidates (item package)
  "Return scored extractive source-highlight candidates for ITEM in PACKAGE."
  (let ((terms (delib-flow--reference-note-title-terms item))
        candidates)
    (dolist (line (delib-flow--reference-note-source-units item package))
      (dolist (fragment (delib-flow--reference-note-split-unit-sentences line))
        (let ((cleaned (delib-flow--reference-note-clean-line fragment)))
          (when (delib-flow--reference-note-highlight-fragment-extractive-p cleaned)
            (let ((score (delib-flow--reference-note-highlight-score cleaned terms)))
              (when (> score 0)
                (push (list :score score
                            :text cleaned
                            :word-count
                            (length
                             (delib-flow--reference-note-highlight-fragment-words
                              cleaned)))
                      candidates)))))))
    candidates))

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

(defun delib-flow--clean-source-view-lines (package)
  "Return cleaned source lines for operator review from PACKAGE."
  (or (delib-flow--reference-note-source-lines package)
      (seq-filter
       #'delib-flow--source-evidence-useful-line-p
       (mapcar #'string-trim (delib-flow--source-body-lines package)))))

(defun delib-flow--clean-source-view-header-lines (source type-hint contacts)
  "Return header lines for cleaned SOURCE view using TYPE-HINT and CONTACTS."
  (list
   "** Cleaned source"
   (format "- Title: %s" (or (plist-get source :title) "Untitled source"))
   (format "- Type: %s" type-hint)
   (format "- Contacts: %s"
           (if contacts
               (string-join contacts ", ")
             "none"))
   "- This is the cleaned source used for extraction and note grounding."
   ""))

(defun delib-flow--clean-source-view-body-lines (lines)
  "Return renderable cleaned source body LINES."
  (cons "*** Source text"
        (if lines
            (mapcar (lambda (line) (format "- %s" line)) lines)
          '("- No cleaned source lines are available."))))

(defun delib-flow--clean-source-view-text (package)
  "Return renderable cleaned source text for PACKAGE."
  (let* ((source (plist-get package :source))
         (contacts (delib-flow--package-contact-emails package))
         (type-hint (or (plist-get (delib-flow--package-email-digest package) :type-hint)
                        (plist-get source :type)
                        "unknown"))
         (lines (delib-flow--clean-source-view-lines package)))
    (string-join
     (append (delib-flow--clean-source-view-header-lines source type-hint contacts)
             (delib-flow--clean-source-view-body-lines lines))
     "\n")))

(defun delib-flow--reference-note-source-highlights (item package)
  "Return up to three source highlight lines for reference-note ITEM in PACKAGE."
  (let* ((lines (delib-flow--reference-note-source-units item package))
         (selected
          (mapcar (lambda (entry) (plist-get entry :text))
                  (seq-take
                   (sort (delib-flow--reference-note-highlight-candidates
                          item package)
                         (lambda (left right)
                           (if (= (plist-get left :score)
                                  (plist-get right :score))
                               (< (plist-get left :word-count)
                                  (plist-get right :word-count))
                             (> (plist-get left :score)
                                (plist-get right :score)))))
                   4))))
    (or (delete-dups selected)
        (seq-take
         (seq-filter #'delib-flow--reference-note-highlight-fragment-extractive-p
                     (mapcar #'delib-flow--reference-note-clean-line lines))
         4))))

(defun delib-flow--reference-note-support-candidates (package)
  "Return note-support candidates from PACKAGE."
  (or (plist-get (delib-flow--selected-reference-note-draft-from-package package)
                 :support-candidates)
      (delib-flow--artifact-family-selected-support-candidates package
                                                               'reference-notes)
      nil))

(defun delib-flow--reference-note-support-line (candidate)
  "Return support line text for retained CANDIDATE."
  (let* ((title (plist-get candidate :title))
         (reason
          (cond
           ((plist-get candidate :filter-reasons)
            (mapconcat #'identity
                       (plist-get candidate :filter-reasons)
                       ", "))
           ((plist-get candidate :reasons)
            (mapconcat #'identity
                       (plist-get candidate :reasons)
                       ", "))
           (t nil)))
         (focus
          (and (plist-get candidate :file)
               (delib-flow--candidate-note-focus-line candidate))))
    (string-trim
     (format "%s%s%s"
             (or title "Untitled note")
             (if focus
                 (format " - %s" (string-trim focus))
               "")
             (if reason
                 (format " (%s)" reason)
               "")))))

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
         (highlights
          (delib-flow--reference-note-source-highlights item package))
         (summary-text
          (plist-get
           (plist-get
            (plist-get
             (plist-get package :working-context)
             :inspect-output)
            :analysis)
           :summary))
         (clean-summary
          (and summary-text
               (let ((cleaned
                      (delib-flow--reference-note-clean-line
                       (replace-regexp-in-string
                        "Referenced materials?:.*\\'"
                        "" summary-text))))
                 (unless (string-empty-p cleaned)
                   cleaned)))))
    (cond
     ((eq (plist-get item :note-type) 'project-support)
      (format
       "This note captures supporting context for %s from %s."
       title source-title))
     (highlights
      (format
       "This note captures %s. The source frames it as: %s"
       title
       (string-trim-right (car highlights) "[[:space:]]*[.?!]*")))
     (clean-summary
      (format
       "This note captures %s. The source frames it as: %s"
       title
       (string-trim-right clean-summary "[[:space:]]*[.?!]*")))
     (t
      (format
       "This note captures %s from %s."
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
  (let* ((highlights
          (delib-flow--reference-note-source-highlights item package))
         (support-lines
          (delib-flow--reference-note-support-lines package))
         (source-context-lines
          (delib-flow--reference-note-source-context-lines package))
         (draft-summary
          (delib-flow--reference-note-draft-summary item package))
         (durable-claim (car highlights))
         (why-it-matters
          (string-remove-prefix
           (format
            "This note captures %s. The source frames it as: "
            (delib-flow--reference-note-title item))
           draft-summary)))
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

(defun delib-flow--reference-note-draft-body-with-seed
 (draft-body item package)
 "Return DRAFT-BODY enriched with seeded structure for ITEM and PACKAGE."
 (let* ((clean-draft (string-trim (or draft-body "")))
        (highlights
         (delib-flow--reference-note-source-highlights item package))
        (support-lines
         (delib-flow--reference-note-support-lines package))
        (source-context-lines
         (delib-flow--reference-note-source-context-lines package))
        (draft-sentences
         (delib-flow--reference-note-split-unit-sentences clean-draft))
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
           "" "* Source highlights")
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
     (if source-context-lines (append source-context-lines '(""))
       '("- Add the source title, sender, and any context needed for later trust checks."
         ""))
     (list "* Next pass"
           "- Distill the durable idea in your own words."
           "- Explain why this note is reusable beyond the source email."
           "- Link or merge with nearby notes if the concept already exists."))
    "\n")))

(defun delib-flow--reference-note-draft-with-workspace-structure (draft item package)
  "Return DRAFT normalized to the filing workspace note structure."
  (let* ((current-body (plist-get draft :draft-body))
         (normalized-body
          (cond
           ((not (delib-flow--non-empty-string-p current-body))
            (delib-flow--reference-note-seeded-body item package))
           ((delib-flow--reference-note-draft-has-workspace-structure-p
             current-body)
            current-body)
           (t
            (delib-flow--reference-note-enrich-partial-draft-body
             current-body item package)))))
    (if (equal normalized-body current-body)
        draft
      (plist-put (copy-tree draft) :draft-body normalized-body))))

(defun delib-flow--reference-note-partial-working-body (draft-view current-body)
  "Return working-body text for DRAFT-VIEW and CURRENT-BODY."
  (if (string-match-p "^\\* Working draft$" (or current-body ""))
      (delib-flow--reference-note-draft-working-body-text draft-view)
    current-body))

(defun delib-flow--reference-note-partial-reuse-angle (draft-view)
  "Return existing reuse-angle line from DRAFT-VIEW, or nil."
  (let ((line (delib-flow--reference-note-draft-reuse-angle-text draft-view)))
    (unless (string-prefix-p "No reuse angle is recorded yet" line)
      line)))

(defun delib-flow--reference-note-restore-partial-sections (body draft-view)
  "Return BODY with any existing partial sections from DRAFT-VIEW restored."
  (let ((source-highlights
         (delib-flow--reference-note-draft-section-text
          draft-view "Source highlights"))
        (related-material
         (delib-flow--reference-note-draft-section-text
          draft-view "Related material to connect"))
        (reuse-angle
         (delib-flow--reference-note-partial-reuse-angle draft-view)))
    (when source-highlights
      (setq body
            (delib-flow--reference-note-replace-section
             body "Source highlights"
             (split-string source-highlights "\n" nil))))
    (when related-material
      (setq body
            (delib-flow--reference-note-replace-section
             body "Related material to connect"
             (split-string related-material "\n" nil))))
    (when reuse-angle
      (setq body
            (delib-flow--reference-note-replace-working-line
             body "- Reuse angle:" reuse-angle)))
    body))

(defun delib-flow--reference-note-enrich-partial-draft-body (current-body item package)
  "Return CURRENT-BODY repaired into workspace structure for ITEM and PACKAGE."
  (let* ((draft-view (list :draft-body current-body))
         (working-body
          (delib-flow--reference-note-partial-working-body
           draft-view current-body))
         (body
          (delib-flow--reference-note-draft-body-with-seed
           working-body item package)))
    (delib-flow--reference-note-restore-partial-sections body draft-view)))

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

(defun delib-flow--draft-evidence-snapshot-summary (snapshot)
  "Return compact summary line for draft evidence SNAPSHOT."
  (format "- Frozen evidence: %s source excerpt(s), %s support item(s), %s quality gap(s)."
          (length (or (plist-get snapshot :source-excerpts) nil))
          (or (plist-get snapshot :support-count) 0)
          (length (or (plist-get snapshot :quality-gaps) nil))))

(defun delib-flow--support-snapshot-display-line (item)
  "Return compact operator-facing line for frozen support ITEM."
  (let ((title (or (plist-get item :title) "Untitled note"))
        (focus (plist-get item :focus))
        (score (plist-get item :score))
        (reason-text (plist-get item :reason-text)))
    (string-trim
     (format "- %s%s%s%s"
             title
             (if score (format " [score %s]" score) "")
             (if focus (format " - %s" focus) "")
             (if reason-text (format " (%s)" reason-text) "")))))

(defun delib-flow--draft-evidence-snapshot-support-items (snapshot)
  "Return frozen support item lines for SNAPSHOT."
  (or (and-let* ((items (plist-get snapshot :support-items)))
        (mapcar #'delib-flow--support-snapshot-display-line items))
      (plist-get snapshot :support-lines)))

(defun delib-flow--draft-evidence-support-identities (snapshot)
  "Return stable support identities for SNAPSHOT."
  (or (and-let* ((items (plist-get snapshot :support-items)))
        (mapcar (lambda (item)
                  (or (plist-get item :identity)
                      (plist-get item :title)
                      (plist-get item :line)))
                items))
      (plist-get snapshot :support-lines)))

(defun delib-flow--draft-evidence-quality-gap-identities (snapshot)
  "Return stable quality-gap identities for SNAPSHOT."
  (or (and-let* ((items (plist-get snapshot :quality-gap-items)))
        (mapcar (lambda (item)
                  (or (plist-get item :identity)
                      (plist-get item :line)))
                items))
      (plist-get snapshot :quality-gaps)))

(defun delib-flow--support-snapshot-identity (item)
  "Return stable identity for frozen support ITEM."
  (or (plist-get item :identity)
      (plist-get item :title)
      (plist-get item :line)))

(defun delib-flow--support-snapshot-map (snapshot)
  "Return alist mapping support identity to frozen support item in SNAPSHOT."
  (mapcar (lambda (item)
            (cons (delib-flow--support-snapshot-identity item) item))
          (or (plist-get snapshot :support-items) nil)))

(defun delib-flow--support-snapshot-score-change (current previous)
  "Return score change text between CURRENT and PREVIOUS support items, or nil."
  (unless (equal (plist-get previous :score)
                 (plist-get current :score))
    (format "score %s -> %s"
            (or (plist-get previous :score) "none")
            (or (plist-get current :score) "none"))))

(defun delib-flow--support-snapshot-reason-change (current previous)
  "Return reason change text between CURRENT and PREVIOUS support items, or nil."
  (unless (equal (plist-get previous :reason-text)
                 (plist-get current :reason-text))
    (format "reason %s -> %s"
            (or (plist-get previous :reason-text) "none")
            (or (plist-get current :reason-text) "none"))))

(defun delib-flow--support-snapshot-focus-change (current previous)
  "Return focus change text between CURRENT and PREVIOUS support items, or nil."
  (unless (equal (plist-get previous :focus)
                 (plist-get current :focus))
    (format "focus %s -> %s"
            (or (plist-get previous :focus) "none")
            (or (plist-get current :focus) "none"))))

(defun delib-flow--support-snapshot-change-description (current previous)
  "Return compact change description between CURRENT and PREVIOUS support items."
  (string-join
   (delq nil
         (list
          (delib-flow--support-snapshot-score-change current previous)
          (delib-flow--support-snapshot-reason-change current previous)
          (delib-flow--support-snapshot-focus-change current previous)))
   ", "))

(defun delib-flow--support-snapshot-change-lines (current previous)
  "Return changed-support lines between CURRENT and PREVIOUS snapshots."
  (let ((previous-map (delib-flow--support-snapshot-map previous)))
    (delq nil
          (mapcar
           (lambda (current-entry)
             (let* ((identity (car current-entry))
                    (current-item (cdr current-entry))
                    (previous-item (cdr (assoc identity previous-map))))
               (when previous-item
                 (when-let ((description
                             (let ((text
                                    (delib-flow--support-snapshot-change-description
                                     current-item previous-item)))
                               (unless (string-empty-p text) text))))
                   (format "%s (%s)" identity description)))))
           (delib-flow--support-snapshot-map current)))))

(defun delib-flow--draft-evidence-snapshot-detail (snapshot)
  "Return compact frozen evidence detail text for SNAPSHOT."
  (concat
   "****** Frozen evidence review\n"
   (format "- Draft reason: %s\n"
           (or (plist-get snapshot :draft-reason)
               "No explicit drafting rationale is recorded."))
   (format "- Candidate origin: %s\n"
           (delib-flow--artifact-source-label
            (plist-get snapshot :candidate-origin)))
   (format "- Source title: %s\n"
           (or (plist-get snapshot :source-title)
               "Unknown source"))
   "\n****** Frozen source excerpts\n"
   (if-let ((source-excerpts (plist-get snapshot :source-excerpts)))
       (mapconcat #'identity source-excerpts "\n")
     "- No frozen source excerpts were captured.")
   "\n\n****** Frozen support items\n"
   (if-let ((support-items
             (delib-flow--draft-evidence-snapshot-support-items snapshot)))
       (mapconcat #'identity support-items "\n")
     "- No frozen support items were captured.")
   "\n\n****** Frozen support context\n"
   (if-let ((support-context-lines
             (plist-get snapshot :support-context-lines)))
       (mapconcat (lambda (line) (format "- %s" line))
                  support-context-lines
                  "\n")
     "- No frozen support context was captured.")
   "\n\n****** "
   (or (plist-get snapshot :family-review-heading)
       "Frozen family checks")
   "\n"
   (if-let ((family-review-lines
             (plist-get snapshot :family-review-lines)))
       (mapconcat (lambda (line) (format "- %s" line))
                  family-review-lines
                  "\n")
     "- No frozen family-specific review checks were captured.")
   "\n\n****** Frozen quality gaps\n"
   (if-let ((quality-gaps (plist-get snapshot :quality-gaps)))
       (string-join quality-gaps "\n")
     "- No frozen quality gaps were captured.")))

(defun delib-flow--snapshot-list-diff (current previous key)
  "Return plist of added and removed KEY items between CURRENT and PREVIOUS snapshots."
  (let* ((current-items (or (plist-get current key) nil))
         (previous-items (or (plist-get previous key) nil))
         (added (seq-filter (lambda (item) (not (member item previous-items)))
                            current-items))
         (removed (seq-filter (lambda (item) (not (member item current-items)))
                              previous-items)))
    (list :added added :removed removed)))

(defun delib-flow--draft-evidence-snapshot-diff-section (heading added removed)
  "Return compact diff section for HEADING with ADDED and REMOVED items."
  (concat
   (format "- %s added: %s\n"
           heading
           (if added
               (string-join added " | ")
             "none"))
   (format "- %s removed: %s"
           heading
           (if removed
               (string-join removed " | ")
             "none"))))

(defun delib-flow--draft-evidence-reason-diff-line (current previous)
  "Return draft-reason diff line between CURRENT and PREVIOUS snapshots."
  (let ((current-reason (or (plist-get current :draft-reason)
                            "No explicit drafting rationale is recorded."))
        (previous-reason (or (plist-get previous :draft-reason)
                             "No explicit drafting rationale is recorded.")))
    (if (equal current-reason previous-reason)
        (format "- Draft reason unchanged: %s\n" current-reason)
      (format "- Draft reason changed: %s -> %s\n"
              previous-reason current-reason))))

(defun delib-flow--draft-evidence-origin-diff-line (current previous)
  "Return candidate-origin diff line between CURRENT and PREVIOUS snapshots."
  (let ((current-origin
         (delib-flow--artifact-source-label
          (plist-get current :candidate-origin)))
        (previous-origin
         (delib-flow--artifact-source-label
          (plist-get previous :candidate-origin))))
    (if (equal current-origin previous-origin)
        (format "- Candidate origin unchanged: %s\n" current-origin)
      (format "- Candidate origin changed: %s -> %s\n"
              previous-origin current-origin))))

(defun delib-flow--draft-evidence-support-diff (current previous)
  "Return support-item diff plist between CURRENT and PREVIOUS snapshots."
  (let ((current-support (delib-flow--draft-evidence-support-identities current))
        (previous-support (delib-flow--draft-evidence-support-identities previous)))
    (delib-flow--snapshot-list-diff
     (list :support-items current-support)
     (list :support-items previous-support)
     :support-items)))

(defun delib-flow--draft-evidence-support-change-line (current previous)
  "Return support-details change line between CURRENT and PREVIOUS snapshots."
  (let ((changed-support (delib-flow--support-snapshot-change-lines current previous)))
    (format "- Support details changed: %s"
            (if changed-support
                (string-join changed-support " | ")
              "none"))))

(defun delib-flow--draft-evidence-family-review-change-line (current previous)
  "Return family-review change line between CURRENT and PREVIOUS snapshots."
  (let* ((diff (delib-flow--snapshot-list-diff current previous :family-review-lines))
         (added (plist-get diff :added))
         (removed (plist-get diff :removed)))
    (format "- Family review changes: added %s; removed %s"
            (if added
                (string-join added " | ")
              "none")
            (if removed
                (string-join removed " | ")
              "none"))))

(defun delib-flow--draft-evidence-quality-risk-change-line (current previous)
  "Return quality-risk change line between CURRENT and PREVIOUS snapshots."
  (let* ((diff
          (delib-flow--snapshot-list-diff
           (list :quality-gap-items
                 (delib-flow--draft-evidence-quality-gap-identities current))
           (list :quality-gap-items
                 (delib-flow--draft-evidence-quality-gap-identities previous))
           :quality-gap-items))
         (introduced (plist-get diff :added))
         (resolved (plist-get diff :removed)))
    (format "- Quality risk changes: introduced %s; resolved %s"
            (if introduced
                (string-join introduced " | ")
              "none")
            (if resolved
                (string-join resolved " | ")
              "none"))))

(defun delib-flow--draft-evidence-list-diff-section (current previous key heading)
  "Return diff text for KEY under HEADING between CURRENT and PREVIOUS snapshots."
  (let ((diff (delib-flow--snapshot-list-diff current previous key)))
    (delib-flow--draft-evidence-snapshot-diff-section
     heading
     (plist-get diff :added)
     (plist-get diff :removed))))

(defun delib-flow--draft-evidence-snapshot-diff-text (current previous)
  "Return compact frozen evidence diff text between CURRENT and PREVIOUS snapshots."
  (let ((support-diff (delib-flow--draft-evidence-support-diff current previous)))
    (concat
     "****** Frozen evidence changes\n"
     (delib-flow--draft-evidence-reason-diff-line current previous)
     (delib-flow--draft-evidence-origin-diff-line current previous)
     (delib-flow--draft-evidence-list-diff-section
      current previous :source-excerpts "Source excerpts")
     "\n"
     (delib-flow--draft-evidence-snapshot-diff-section
      "Support items"
      (plist-get support-diff :added)
      (plist-get support-diff :removed))
     "\n"
     (delib-flow--draft-evidence-support-change-line current previous)
     "\n"
     (delib-flow--draft-evidence-family-review-change-line current previous)
     "\n"
     (delib-flow--draft-evidence-quality-risk-change-line current previous)
     "\n"
     (delib-flow--draft-evidence-list-diff-section
      current previous :quality-gaps "Quality gaps"))))

(provide 'delib-flow-artifacts)

;;; delib-flow-artifacts.el ends here
