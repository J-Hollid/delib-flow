;;; delib-flow.el --- Guided AI workflow control for Org -*- lexical-binding: t; -*-

;; Author: J Holliday
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, ai
;; URL: https://example.invalid/delib-flow

;;; Commentary:

;; delib-flow provides a guided control buffer for deliberate AI-assisted
;; workflow execution from an Org heading at point.

;;; Code:

(require 'org)
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

(defcustom delib-flow-default-local-model nil
  "Default local model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-default-cloud-model nil
  "Default cloud model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-local-stage-adapter #'delib-flow--default-local-stage-adapter
  "Function used to execute local stages.

The function receives a stage descriptor and an assembled input package,
and returns raw stage output."
  :type 'function)

(defconst delib-flow-control-buffer-name "*delib-flow*"
  "Name of the main delib-flow control buffer.")

(defvar delib-flow--active-run nil
  "Active run state for the current delib-flow session.")

(defconst delib-flow--control-sections
  '("Source"
    "Working context"
    "Stage history"
    "Current decision"
    "Valid next actions"
    "Filing preview"
    "Audit status")
  "Top-level sections rendered in the control buffer.")

(defconst delib-flow--initial-section-anchor-alist
  '((source . "delib-section-source")
    (working-context . "delib-section-working-context")
    (stage-history . "delib-section-stage-history")
    (current-decision . "delib-section-current-decision")
    (valid-next-actions . "delib-section-valid-next-actions")
    (filing-preview . "delib-section-filing-preview")
    (audit-status . "delib-section-audit-status"))
  "Stable anchor identifiers for top-level control-buffer sections.")

(defconst delib-flow--section-renderer-alist
  '(("Source" . delib-flow--render-source-section)
    ("Working context" . delib-flow--render-working-context-section)
    ("Stage history" . delib-flow--render-stage-history-section)
    ("Current decision" . delib-flow--render-current-decision-section)
    ("Valid next actions" . delib-flow--render-valid-next-actions-section)
    ("Filing preview" . delib-flow--render-filing-preview-section)
    ("Audit status" . delib-flow--render-audit-status-section))
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
    (extract-actions
     :id extract-actions
     :label "Extract Actions"
     :prompt-id milestone2-extract-actions
     :executor delib-flow--execute-extract-actions
     :normalizer delib-flow--normalize-extract-actions-output))
  "Stage descriptors keyed by stage identifier.")

(defconst delib-flow--stage-decision-alist
  '((match-project . "Review project match and choose next action.")
    (discover-reference-material
     . "Review retrieved reference candidates and choose next action.")
    (filter-reference-material
     . "Review retained context and choose next action.")
    (extract-actions . "Review drafted actions and choose next action."))
  "Stage-specific current-decision text keyed by stage identifier.")

(defconst delib-flow--stage-apply-function-alist
  '((inspect-source . delib-flow--apply-inspect-source-entry)
    (match-project . delib-flow--apply-match-project-entry)
    (discover-reference-material . delib-flow--apply-discovery-entry)
    (filter-reference-material . delib-flow--apply-filter-entry)
    (extract-actions . delib-flow--apply-extract-actions-entry))
  "Stage-specific apply functions keyed by stage identifier.")

(defun delib-flow--org-heading-at-point-p ()
  "Return non-nil when point is on an Org heading."
  (org-at-heading-p))

(defun delib-flow--snapshot-heading ()
  "Capture a frozen snapshot of the Org heading at point.

Return a plist containing source metadata and content."
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (begin (point))
           (end (save-excursion
                  (org-end-of-subtree t t)
                  (point)))
           (content (buffer-substring-no-properties begin end))
           (file (buffer-file-name))
           (id (org-entry-get (point) "ID"))
           (outline-path (ignore-errors (org-get-outline-path t t))))
      (list :title title
            :file file
            :id id
            :begin begin
            :end end
            :outline-path outline-path
            :content content
            :source-type 'unknown))))

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
          "Working context"
          '("Working context" "Editable working slice")
          "delib-edit-context-main"))
   (cons 'operator-notes
         (delib-flow--make-editable-block
          'operator-notes
          'notes
          "Current decision"
          '("Current decision" "Operator notes")
          "delib-edit-operator-notes"))))

(defun delib-flow--initial-section-anchors ()
  "Return the initial section-anchor alist for a new run."
  (copy-tree delib-flow--initial-section-anchor-alist))

(defun delib-flow--initial-actions-state ()
  "Return the initial action-state plist for a new run."
  (list :items nil
        :selected-action nil
        :last-action nil))

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
        :requires-clean-managed-regions t
        :requires-valid-edits t))

(defun delib-flow--initial-session-state ()
  "Return the initial session-state plist for a new run."
  (list :status 'active
        :started-at (current-time)
        :ended-at nil
        :current-stage nil
        :current-decision "Review working context and choose next action."
        :run-buffer delib-flow-control-buffer-name
        :active t
        :aborted nil))

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

(defun delib-flow--run-ui (run)
  "Return the UI plist from RUN."
  (plist-get run :ui))

(defun delib-flow--run-actions (run)
  "Return the actions plist from RUN."
  (plist-get run :actions))

(defun delib-flow--run-stage-history (run)
  "Return the stage-history plist from RUN."
  (plist-get run :stage-history))

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

(defun delib-flow--stage-executed-p (run stage-id)
  "Return non-nil when STAGE-ID already appears in RUN history."
  (memq stage-id
        (mapcar (lambda (entry)
                  (plist-get entry :stage-id))
                (plist-get (delib-flow--run-stage-history run) :entries))))

(defun delib-flow--stage-input-package (run stage-id)
  "Return assembled input package for STAGE-ID from RUN."
  (list :stage-id stage-id
        :prompt-id (delib-flow--stage-prompt-id stage-id)
        :source (delib-flow--run-source run)
        :working-context (delib-flow--run-working-context run)))

(defun delib-flow--string-words (text)
  "Return normalized word list for TEXT."
  (split-string (downcase (or text "")) "[^[:alnum:]]+" t))

(defun delib-flow--shared-word-count (left right)
  "Return shared normalized word count between LEFT and RIGHT."
  (length (seq-intersection (delib-flow--string-words left)
                            (delib-flow--string-words right)
                            #'string=)))

(defun delib-flow--project-candidate (title)
  "Return a project candidate object for TITLE."
  (list :title title))

(defun delib-flow--project-candidates-from-file (file)
  "Return project candidates parsed from Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (candidates)
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(.+\\)$" nil t)
        (push (delib-flow--project-candidate
               (string-trim (match-string 1)))
              candidates))
      (nreverse candidates))))

(defun delib-flow--project-match-score (source-title candidate)
  "Return match score between SOURCE-TITLE and CANDIDATE."
  (let* ((candidate-title (plist-get candidate :title))
         (shared (delib-flow--shared-word-count source-title candidate-title))
         (exact (string= (downcase (or source-title ""))
                         (downcase (or candidate-title "")))))
    (+ shared
       (if exact 10 0))))

(defun delib-flow--scored-project-candidates (source-title candidates)
  "Return scored project CANDIDATES for SOURCE-TITLE."
  (mapcar (lambda (candidate)
            (plist-put (copy-sequence candidate)
                       :score
                       (delib-flow--project-match-score source-title candidate)))
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

(defun delib-flow--project-match-result (source-title candidates)
  "Return raw project-match result for SOURCE-TITLE and CANDIDATES."
  (let* ((scored (delib-flow--scored-project-candidates source-title candidates))
         (top (delib-flow--top-project-candidates scored))
         (best (car top))
         (score (or (plist-get best :score) 0)))
    (cond
     ((<= score 0)
      (list :match-status 'no-match
            :best-project nil
            :candidates nil
            :reason "No project title terms overlapped the source title."))
     ((> (length top) 1)
      (list :match-status 'ambiguous
            :best-project nil
            :candidates top
            :reason "Multiple projects tied for the best title match."))
     (t
      (list :match-status 'matched
            :best-project best
            :candidates top
            :reason "A single highest-scoring title match was found.")))))

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
  (delete-dups
   (append
    (delib-flow--string-words (delib-flow--source-search-title package))
    (delib-flow--string-words (delib-flow--project-match-title package)))))

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

(defun delib-flow--discovery-candidate-score (terms file)
  "Return retrieval score for TERMS against FILE."
  (let* ((title (delib-flow--zk-note-title file))
         (text (delib-flow--zk-note-text file))
         (title-score (delib-flow--shared-word-count (mapconcat #'identity terms " ")
                                                     title))
         (text-score (delib-flow--shared-word-count (mapconcat #'identity terms " ")
                                                    text)))
    (+ (* 2 title-score) text-score)))

(defun delib-flow--make-discovery-candidate (file score)
  "Return discovery candidate for FILE with SCORE."
  (list :title (delib-flow--zk-note-title file)
        :file file
        :score score))

(defun delib-flow--scored-discovery-candidates (terms files)
  "Return scored discovery candidates for TERMS across FILES."
  (let (candidates)
    (dolist (file files (nreverse candidates))
      (let ((score (delib-flow--discovery-candidate-score terms file)))
        (when (> score 0)
          (push (delib-flow--make-discovery-candidate file score)
                candidates))))))

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
            terms
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

(defun delib-flow--filter-reference-material-result (package)
  "Return raw filter result for PACKAGE."
  (let* ((candidates (delib-flow--retrieved-candidates package))
         (retained (delib-flow--retained-filter-candidates candidates))
         (fallback (and candidates (list (car candidates))))
         (selected (or retained fallback)))
    (list :candidate-count (length candidates)
          :retained-count (length selected)
          :retained-candidates selected
          :rejected-count (- (length candidates) (length selected)))))

(defun delib-flow--source-title (package)
  "Return source title from PACKAGE."
  (plist-get (plist-get package :source) :title))

(defun delib-flow--retained-candidates (package)
  "Return retained candidates from PACKAGE."
  (plist-get (plist-get (plist-get package :working-context) :filtered-context)
             :retained-candidates))

(defun delib-flow--make-draft-action (text source)
  "Return draft action object for TEXT and SOURCE."
  (list :kind 'next-action
        :text text
        :source source))

(defun delib-flow--source-title-action (package)
  "Return a draft action derived from PACKAGE source title."
  (delib-flow--make-draft-action
   (format "Clarify the next step for %s"
           (delib-flow--source-title package))
   'source))

(defun delib-flow--retained-candidate-action (candidate)
  "Return a draft action derived from retained CANDIDATE."
  (delib-flow--make-draft-action
   (format "Review %s for follow-up context"
           (plist-get candidate :title))
   'retained-context))

(defun delib-flow--retained-candidate-actions (package)
  "Return retained-candidate draft actions for PACKAGE."
  (mapcar #'delib-flow--retained-candidate-action
          (delib-flow--retained-candidates package)))

(defun delib-flow--extract-actions-result (package)
  "Return raw action-extraction result for PACKAGE."
  (let* ((source-action (delib-flow--source-title-action package))
         (retained-actions (delib-flow--retained-candidate-actions package))
         (actions (cons source-action retained-actions)))
    (list :candidate-count (length actions)
          :actions actions)))

(defun delib-flow--default-local-stage-adapter (descriptor package)
  "Execute DESCRIPTOR locally with PACKAGE and return raw output."
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
        :input-package package
        :raw-output raw-output
        :normalized-output normalized-output
        :started-at (current-time)
        :ended-at (current-time)))

(defun delib-flow--make-stage-failure-entry (stage-id package message)
  "Return a failed stage entry for STAGE-ID with MESSAGE."
  (list :stage-id stage-id
        :label (delib-flow--stage-label stage-id)
        :status 'failed
        :review-state 'error
        :prompt-id (plist-get package :prompt-id)
        :input-package package
        :raw-output nil
        :normalized-output message
        :started-at (current-time)
        :ended-at (current-time)))

(defun delib-flow--append-stage-entry (run entry)
  "Return RUN with ENTRY appended to stage history."
  (let* ((history (delib-flow--run-stage-history run))
         (entries (append (plist-get history :entries) (list entry)))
         (updated-history
          (plist-put
           (plist-put
            (plist-put history :entries entries)
            :latest-stage (plist-get entry :stage-id))
           :latest-status (plist-get entry :status))))
    (plist-put run :stage-history updated-history)))

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

(defun delib-flow--match-project-action (run)
  "Return the match-project action for RUN."
  (when (delib-flow--stage-executed-p run 'inspect-source)
    (delib-flow--make-action
     'match-project
     (if (delib-flow--stage-executed-p run 'match-project)
         "Retry Match Project"
       "Match Project")
     'available
     nil
     #'delib-flow-action-match-project
     20)))

(defun delib-flow--discover-reference-material-action (run)
  "Return the discover-reference-material action for RUN."
  (when (delib-flow--stage-executed-p run 'inspect-source)
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
  (when (delib-flow--stage-executed-p run 'discover-reference-material)
    (delib-flow--make-action
     'filter-reference-material
     (if (delib-flow--stage-executed-p run 'filter-reference-material)
         "Retry Filter Useful Reference Material"
       "Filter Useful Reference Material")
     'available
     nil
     #'delib-flow-action-filter-reference-material
     35)))

(defun delib-flow--extract-actions-action (run)
  "Return the extract-actions action for RUN."
  (when (eq (delib-flow--match-status run) 'matched)
    (delib-flow--make-action
     'extract-actions
     (if (delib-flow--stage-executed-p run 'extract-actions)
         "Retry Extract Actions"
       "Extract Actions")
     'available
     nil
     #'delib-flow-action-extract-actions
     40)))

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
    (delib-flow--match-project-action run)
    (delib-flow--discover-reference-material-action run)
    (delib-flow--filter-reference-material-action run)
    (delib-flow--placeholder-stage-action
     'decide-cloud-pass
     "Decide on Cloud Pass"
     40))))

(defun delib-flow--match-status (run)
  "Return the stored project match status from RUN."
  (plist-get (plist-get (delib-flow--run-working-context run) :project-match)
             :match-status))

(defun delib-flow--post-match-actions (run)
  "Return next legal actions after match-project has executed in RUN."
  (let ((status (delib-flow--match-status run)))
    (seq-remove
     #'null
     (append
      (list
       (delib-flow--match-project-action run)
       (delib-flow--discover-reference-material-action run))
     (if (eq status 'matched)
          (list
           (delib-flow--extract-actions-action run)
           (delib-flow--placeholder-stage-action
            'extract-waiting-for
            "Extract Waiting-For"
            50)
           (delib-flow--placeholder-stage-action
            'suggest-reference-notes
            "Suggest Reference Notes"
            60))
        (list
         (delib-flow--placeholder-stage-action
          'manual-project-match
          "Choose Project Manually"
          40)
         (delib-flow--placeholder-stage-action
          'propose-new-project
          "Propose New Project"
          50)))
       (list
       (delib-flow--filter-reference-material-action run)
       (delib-flow--placeholder-stage-action
        'decide-cloud-pass
        "Decide on Cloud Pass"
        80))))))

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

(defun delib-flow--apply-action-conflict-state (action conflicts)
  "Return ACTION adjusted for managed-region CONFLICTS."
  (if (or (null conflicts)
          (memq (plist-get action :id) '(refresh-buffer abort-run)))
      action
    (plist-put
     (plist-put action :status 'blocked)
     :reason
     "Managed-region conflicts must be resolved before this action can run.")))

(defun delib-flow--seed-actions (run)
  "Return RUN with computed actions populated."
  (let* ((conflicts (plist-get (delib-flow--run-ui run)
                               :managed-region-conflicts))
         (items (mapcar (lambda (action)
                          (delib-flow--apply-action-conflict-state
                           action conflicts))
                        (delib-flow--compute-actions run))))
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

(defun delib-flow--format-action-line (action)
  "Return a display line for ACTION."
  (let* ((label (plist-get action :label))
         (status (plist-get action :status))
         (reason (plist-get action :reason))
         (line (format "- %s [%s]" label status)))
    (if reason
        (format "%s: %s\n" line reason)
      (format "%s\n" line))))

(defun delib-flow--sorted-actions (run)
  "Return RUN actions sorted by priority."
  (sort (copy-sequence (plist-get (delib-flow--run-actions run) :items))
        (lambda (left right)
          (< (plist-get left :priority)
             (plist-get right :priority)))))

(defun delib-flow--render-source-section (run)
  "Return Org text for the Source section from RUN."
  (let* ((source (delib-flow--run-source run))
         (title (or (plist-get source :title) "Untitled source"))
         (file (or (plist-get source :file) "No file"))
         (id (or (plist-get source :id) "No ID"))
         (content (string-trim (or (plist-get source :content) ""))))
    (format "** Title\n%s\n\n** File\n%s\n\n** ID\n%s\n\n** Snapshot\n#+begin_example\n%s\n#+end_example\n"
            title file id content)))

(defun delib-flow--project-match-status (project-match)
  "Return display status symbol for PROJECT-MATCH."
  (or (plist-get project-match :match-status)
      'not-run))

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
                     (format "- %s (%s)"
                             (plist-get candidate :title)
                             (plist-get candidate :score)))
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
        (format "Retained: %s. Rejected: %s."
                (plist-get filtered :retained-count)
                (plist-get filtered :rejected-count))
      "No filtered context is available yet.")))

(defun delib-flow--render-working-context-section (_run)
  "Return Org text for the Working context section."
  (let* ((working (delib-flow--run-working-context _run))
         (inspect-output (plist-get working :inspect-output))
         (project-match (plist-get working :project-match))
         (retained-context (plist-get working :retained-context))
         (inspect-status (if inspect-output "available" "not available"))
         (project-status (delib-flow--project-match-status project-match))
         (project-text (delib-flow--project-match-text project-match))
         (retrieved-status (delib-flow--retrieved-context-status working))
         (retrieved-text (delib-flow--retrieved-context-text working))
         (filtered-status (delib-flow--filtered-context-status working))
         (filtered-text (delib-flow--filtered-context-text working))
         (retained-text (or retained-context
                            "No retained context is available yet.")))
    (format
     "** Context status\nInspect output: %s.\nProject match: %s.\nRetrieved context: %s.\nFiltered context: %s.\nCloud-sanitized context: not available.\n\n** Project context\n%s\n\n** Retrieved candidates\n%s\n\n** Filtered context\n%s\n\n** Retained context\n%s\n\n** Editable working slice\n%s"
     inspect-status
     project-status
     retrieved-status
     filtered-status
     project-text
     retrieved-text
     filtered-text
     retained-text
     (delib-flow--render-editable-block _run 'context-main))))

(defun delib-flow--render-stage-entry (entry)
  "Return Org text for stage-history ENTRY."
  (format
   "** %s\n*** Status\n%s\n\n*** Review state\n%s\n\n*** Normalized result\n%s\n"
   (plist-get entry :label)
   (plist-get entry :status)
   (plist-get entry :review-state)
   (plist-get entry :normalized-output)))

(defun delib-flow--render-stage-history-section (_run)
  "Return Org text for the Stage history section."
  (let* ((history (delib-flow--run-stage-history _run))
         (entries (plist-get history :entries)))
    (if entries
        (concat
         (format "** History status\nLatest stage: %s (%s).\n\n"
                 (delib-flow--stage-label (plist-get history :latest-stage))
                 (plist-get history :latest-status))
         (mapconcat #'delib-flow--render-stage-entry entries "\n"))
      "** History status\nNo stages have been executed yet.\n")))

(defun delib-flow--render-current-decision-section (run)
  "Return Org text for the Current decision section from RUN."
  (format "** Decision status\n%s\n\n** Operator notes\n%s"
          (plist-get (delib-flow--run-session run) :current-decision)
          (delib-flow--render-editable-block run 'operator-notes)))

(defun delib-flow--render-valid-next-actions-section (_run)
  "Return Org text for the Valid next actions section."
  (mapconcat #'delib-flow--format-action-line
             (delib-flow--sorted-actions _run)
             ""))

(defun delib-flow--draft-item-preview-line (item)
  "Return preview line for draft ITEM."
  (format "- TODO %s" (plist-get item :text)))

(defun delib-flow--draft-item-preview-text (items)
  "Return preview text for draft action ITEMS."
  (mapconcat #'delib-flow--draft-item-preview-line items "\n"))

(defun delib-flow--draft-item-status (run)
  "Return filing preview status text for RUN."
  (if (plist-get (plist-get run :filing) :draft-items)
      "Draft filing artifacts are available."
    "No filing preview is available yet."))

(defun delib-flow--draft-item-text (run)
  "Return filing preview body text for RUN."
  (let ((items (plist-get (plist-get run :filing) :draft-items)))
    (if items
        (delib-flow--draft-item-preview-text items)
      "No draft artifacts are available yet.")))

(defun delib-flow--render-filing-preview-section (_run)
  "Return Org text for the Filing preview section."
  (format "** Preview status\n%s\n\n** Draft artifacts\n%s\n\n** Approved artifacts\nNo approved artifacts are available yet.\n"
          (delib-flow--draft-item-status _run)
          (delib-flow--draft-item-text _run)))

(defun delib-flow--render-audit-status-section (_run)
  "Return Org text for the Audit status section."
  "** Run audit state\nAudit logging is not implemented in Milestone 1.\n\n** Stage audit readiness\nStage audit data is not ready yet.\n")

(defun delib-flow--count-body-lines (content)
  "Return the count of non-empty body lines in CONTENT."
  (length
   (seq-filter
    (lambda (line)
      (not (string-empty-p (string-trim line))))
    (cdr (split-string content "\n")))))

(defun delib-flow--execute-inspect-source (package)
  "Return raw inspect-source output for PACKAGE."
  (let* ((source (plist-get package :source))
         (content (or (plist-get source :content) ""))
         (outline-path (plist-get source :outline-path)))
    (list :source-type (plist-get source :source-type)
          :title (plist-get source :title)
          :outline-path outline-path
          :body-line-count (delib-flow--count-body-lines content)
          :has-id (not (null (plist-get source :id))))))

(defun delib-flow--match-project-source-title (package)
  "Return the source title used for project matching from PACKAGE."
  (plist-get (plist-get package :source) :title))

(defun delib-flow--project-candidates ()
  "Return parsed project candidates from `delib-flow-my-projects-file'."
  (unless (and delib-flow-my-projects-file
               (file-readable-p delib-flow-my-projects-file))
    (error "The My Projects file is not configured or readable"))
  (delib-flow--project-candidates-from-file delib-flow-my-projects-file))

(defun delib-flow--execute-match-project (package)
  "Return raw match-project output for PACKAGE."
  (delib-flow--project-match-result
   (delib-flow--match-project-source-title package)
   (delib-flow--project-candidates)))

(defun delib-flow--execute-discover-reference-material (package)
  "Return raw discovery output for PACKAGE."
  (delib-flow--discover-reference-material-result package))

(defun delib-flow--execute-filter-reference-material (package)
  "Return raw filter output for PACKAGE."
  (delib-flow--filter-reference-material-result package))

(defun delib-flow--execute-extract-actions (package)
  "Return raw action-extraction output for PACKAGE."
  (delib-flow--extract-actions-result package))

(defun delib-flow--normalize-inspect-source-output (raw-output)
  "Return normalized inspect-source text from RAW-OUTPUT."
  (format
   "- Source type: %s\n- Title: %s\n- Outline path: %s\n- Body lines: %s\n- Source has ID: %s"
   (plist-get raw-output :source-type)
   (or (plist-get raw-output :title) "Untitled source")
   (or (mapconcat #'identity (plist-get raw-output :outline-path) " > ")
       "No outline path")
   (plist-get raw-output :body-line-count)
   (if (plist-get raw-output :has-id) "yes" "no")))

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
  (format "- %s [%s]"
          (plist-get candidate :title)
          (plist-get candidate :score)))

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
  (format "- Candidate count: %s\n- Retained count: %s\n- Rejected count: %s"
          (plist-get raw-output :candidate-count)
          (plist-get raw-output :retained-count)
          (plist-get raw-output :rejected-count)))

(defun delib-flow--normalize-draft-action (item)
  "Return normalized text for draft action ITEM."
  (format "- %s" (plist-get item :text)))

(defun delib-flow--normalize-extract-actions-output (raw-output)
  "Return normalized extract-actions text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s"
          (plist-get raw-output :candidate-count)
          (mapconcat #'delib-flow--normalize-draft-action
                     (plist-get raw-output :actions)
                     "\n")))

(defun delib-flow--completed-stage-decision (stage-id)
  "Return current-decision text for completed STAGE-ID."
  (or (alist-get stage-id delib-flow--stage-decision-alist)
      "Review stage result and choose next action."))

(defun delib-flow--next-decision-for-entry (entry)
  "Return updated current-decision text for stage ENTRY."
  (if (eq (plist-get entry :status) 'completed)
      (delib-flow--completed-stage-decision
       (plist-get entry :stage-id))
    "Review stage failure and choose whether to retry or abort."))

(defun delib-flow--apply-inspect-source-entry (run entry)
  "Return RUN updated from completed inspect-source ENTRY."
  (plist-put
   run :working-context
   (let ((working (delib-flow--run-working-context run)))
     (plist-put
      (plist-put working :inspect-output (plist-get entry :raw-output))
      :retained-context (plist-get entry :normalized-output)))))

(defun delib-flow--apply-match-project-entry (run entry)
  "Return RUN updated from completed match-project ENTRY."
  (plist-put
   run :working-context
   (plist-put (delib-flow--run-working-context run)
              :project-match
              (plist-get entry :raw-output))))

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
         (retained (plist-get raw :retained-candidates)))
    (plist-put
     run :working-context
     (plist-put
      (plist-put working :filtered-context raw)
      :retained-context (delib-flow--retained-context-lines retained)))))

(defun delib-flow--apply-extract-actions-entry (run entry)
  "Return RUN updated from completed extract-actions ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items (plist-get (plist-get entry :raw-output) :actions)))
    (plist-put
     run :filing
     (plist-put
      (plist-put filing :draft-items draft-items)
      :preview-text (delib-flow--draft-item-preview-text draft-items)))))

(defun delib-flow--completed-stage-apply-function (stage-id)
  "Return apply function for completed STAGE-ID."
  (alist-get stage-id delib-flow--stage-apply-function-alist))

(defun delib-flow--apply-completed-stage-entry (run entry)
  "Return RUN updated for completed stage ENTRY."
  (if-let ((apply-fn
            (delib-flow--completed-stage-apply-function
             (plist-get entry :stage-id))))
      (funcall apply-fn run entry)
    run))

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
      updated-run)))

(defun delib-flow--run-stage-locally (run stage-id)
  "Return RUN after executing STAGE-ID through the local stage contract."
  (let* ((descriptor (delib-flow--stage-descriptor stage-id))
         (package (delib-flow--stage-input-package run stage-id)))
    (condition-case err
        (let* ((raw-output (funcall delib-flow-local-stage-adapter
                                    descriptor package))
               (normalized-output
                (delib-flow--normalize-stage-output stage-id raw-output))
               (entry (delib-flow--make-stage-entry
                       stage-id package raw-output normalized-output)))
          (delib-flow--seed-actions
           (delib-flow--apply-stage-entry
            (delib-flow--append-stage-entry run entry)
            entry)))
      (error
       (let ((entry
              (delib-flow--make-stage-failure-entry
               stage-id package (error-message-string err))))
         (delib-flow--seed-actions
          (delib-flow--apply-stage-entry
           (delib-flow--append-stage-entry run entry)
           entry)))))))

(defun delib-flow--initialize-run (source-snapshot)
  "Create a new run state from SOURCE-SNAPSHOT."
  (delib-flow--seed-actions
   (list :source source-snapshot
         :working-context
         (list :source-snapshot source-snapshot
               :inspect-output nil
               :project-match nil
               :retrieved-candidates nil
               :filtered-context nil
               :retained-context nil
               :cloud-sanitized-context nil
               :editable-block-ids '(context-main operator-notes))
         :stage-history
         (list :entries nil
               :latest-stage nil
               :latest-status nil)
         :actions
         (delib-flow--initial-actions-state)
         :routing
         (list :default-local-model delib-flow-default-local-model
               :default-cloud-model delib-flow-default-cloud-model
               :stage-models nil
               :cloud-switch-pending nil
               :sanitization-status nil)
         :filing
         (list :draft-items nil
               :approved-items nil
               :preview-text nil
               :target-locations nil)
         :audit
         (list :run-record nil
               :stage-records nil
               :pending-checkpoints nil
               :last-appended-checkpoint nil)
         :session
         (delib-flow--initial-session-state)
         :ui
         (delib-flow--initial-ui-state))))

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
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* delib-flow run\n")
        (dolist (section delib-flow--control-sections)
          (insert (format "** %s\n" section))
          (insert (delib-flow--section-content section run))
          (unless (bolp) (insert "\n")))
        (setq buffer-read-only nil)
        (delib-flow--protect-managed-regions)
        (goto-char (point-min)))
      (setq-local delib-flow--active-run-buffer t))
    buffer))

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

(defun delib-flow--control-buffer-killed ()
  "Handle control buffer teardown."
  (when (bound-and-true-p delib-flow--active-run-buffer)
    (delib-flow--teardown-active-run)))

(defun delib-flow-action-inspect-source ()
  "Execute the inspect-source stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
        (delib-flow--run-stage-locally delib-flow--active-run
                                       'inspect-source)))
  (delib-flow-refresh-buffer))

(defun delib-flow-action-match-project ()
  "Execute the match-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'match-project)))
  (delib-flow-refresh-buffer))

(defun delib-flow-action-discover-reference-material ()
  "Execute the discover-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'discover-reference-material)))
  (delib-flow-refresh-buffer))

(defun delib-flow-action-filter-reference-material ()
  "Execute the filter-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'filter-reference-material)))
  (delib-flow-refresh-buffer))

(defun delib-flow-action-extract-actions ()
  "Execute the extract-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'extract-actions)))
  (delib-flow-refresh-buffer))

(defun delib-flow-action-stage-placeholder ()
  "Signal that the selected stage exists but is not yet implemented."
  (interactive)
  (user-error "This stage is not implemented yet"))

(defun delib-flow-refresh-buffer ()
  "Refresh the control buffer for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (let ((buffer (delib-flow--control-buffer)))
    (setq delib-flow--active-run
          (delib-flow--seed-actions
           (delib-flow--set-managed-region-conflicts
            (delib-flow--sync-editable-blocks delib-flow--active-run buffer)
            (delib-flow--managed-region-conflicts delib-flow--active-run
                                                  buffer))))
    (let ((updated-buffer
           (if (buffer-live-p buffer)
               (delib-flow--refresh-buffer-sections delib-flow--active-run
                                                   buffer)
             (delib-flow--render-control-buffer delib-flow--active-run))))
      (with-current-buffer updated-buffer
        (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
      (pop-to-buffer updated-buffer))))

(defun delib-flow-abort-run ()
  "Abort the active delib-flow run and close the control buffer."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--mark-run-aborted delib-flow--active-run))
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
  (let ((source (delib-flow--snapshot-heading)))
    (setq delib-flow--active-run
          (delib-flow--initialize-run source))
    (let ((buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
      (pop-to-buffer buffer))))

(provide 'delib-flow)
;;; delib-flow.el ends here
