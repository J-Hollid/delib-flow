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

(defcustom delib-flow-default-local-model nil
  "Default local model identifier."
  :type '(choice (const :tag "Unset" nil) string))

(defcustom delib-flow-default-cloud-model nil
  "Default cloud model identifier."
  :type '(choice (const :tag "Unset" nil) string))

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

(defcustom delib-flow-cloud-stage-adapter #'delib-flow--default-cloud-stage-adapter
  "Function used to execute cloud stages.

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
    (select-approved-filing-actions
     :id select-approved-filing-actions
     :label "Select Approved Filing Actions"
     :prompt-id milestone2-select-approved-filing-actions
     :executor delib-flow--execute-select-approved-filing-actions
     :normalizer delib-flow--normalize-select-approved-filing-actions-output)
    (file-approved-outputs
     :id file-approved-outputs
     :label "File Approved Outputs"
     :prompt-id milestone2-file-approved-outputs
     :executor delib-flow--execute-file-approved-outputs
     :normalizer delib-flow--normalize-file-approved-outputs-output))
  "Stage descriptors keyed by stage identifier.")

(defconst delib-flow--stage-decision-alist
  '((match-project . "Review project match and choose next action.")
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
    (decide-cloud-pass
     . "Review cloud-routing decision and choose next action.")
    (sanitize-for-cloud
     . "Review sanitized cloud package and choose next action.")
    (approve-cloud-send
     . "Review approved cloud package and choose next action.")
    (run-cloud-stage
     . "Review cloud-returned result and choose next action.")
    (approve-candidate-reintegration
     . "Review approved reintegration candidate and choose next action.")
    (integrate-into-source
     . "Review integrated local result and choose next action.")
    (select-approved-filing-actions
     . "Review selected filing action and choose next action.")
    (file-approved-outputs
     . "Review filed outputs and choose next action."))
  "Stage-specific current-decision text keyed by stage identifier.")

(defconst delib-flow--stage-apply-function-alist
  '((inspect-source . delib-flow--apply-inspect-source-entry)
    (match-project . delib-flow--apply-match-project-entry)
    (discover-reference-material . delib-flow--apply-discovery-entry)
    (filter-reference-material . delib-flow--apply-filter-entry)
    (manual-project-match . delib-flow--apply-match-project-entry)
    (propose-new-project . delib-flow--apply-propose-new-project-entry)
    (extract-actions . delib-flow--apply-extract-actions-entry)
    (extract-waiting-for . delib-flow--apply-extract-waiting-for-entry)
    (suggest-reference-notes . delib-flow--apply-suggest-reference-notes-entry)
    (decide-cloud-pass . delib-flow--apply-decide-cloud-pass-entry)
    (sanitize-for-cloud . delib-flow--apply-sanitize-for-cloud-entry)
    (approve-cloud-send . delib-flow--apply-approve-cloud-send-entry)
    (run-cloud-stage . delib-flow--apply-run-cloud-stage-entry)
    (approve-candidate-reintegration
     . delib-flow--apply-approve-candidate-reintegration-entry)
    (integrate-into-source . delib-flow--apply-integrate-into-source-entry)
    (select-approved-filing-actions
     . delib-flow--apply-select-approved-filing-actions-entry)
    (file-approved-outputs . delib-flow--apply-file-approved-outputs-entry))
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
          "delib-edit-operator-notes"))
   (cons 'cloud-package-review
         (delib-flow--make-editable-block
          'cloud-package-review
          'cloud-review
          "Working context"
          '("Working context" "Reviewed cloud package")
          "delib-edit-cloud-package-review"))))

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

(defun delib-flow--time-string (time)
  "Return stable timestamp string for TIME."
  (format-time-string "%Y-%m-%d %H:%M:%S%z" time))

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

(defun delib-flow--run-ui (run)
  "Return the UI plist from RUN."
  (plist-get run :ui))

(defun delib-flow--run-actions (run)
  "Return the actions plist from RUN."
  (plist-get run :actions))

(defun delib-flow--run-stage-history (run)
  "Return the stage-history plist from RUN."
  (plist-get run :stage-history))

(defun delib-flow--run-routing (run)
  "Return the routing plist from RUN."
  (plist-get run :routing))

(defun delib-flow--run-ui-package (run)
  "Return the UI package plist from RUN."
  (plist-get run :ui))

(defun delib-flow--run-cloud-stage-p (stage-id)
  "Return non-nil when STAGE-ID should execute through the cloud adapter."
  (eq stage-id 'run-cloud-stage))

(defun delib-flow--integration-ready-p (run)
  "Return non-nil when RUN has material ready for integration."
  (or (delib-flow--stage-executed-p run 'integrate-into-source)
      (plist-get (plist-get run :filing) :draft-items)
      (and (plist-get (delib-flow--run-working-context run) :cloud-returned-context)
           (eq (plist-get (delib-flow--run-routing run) :reintegration-status)
               'approved))))

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
        :working-context (delib-flow--run-working-context run)
        :filing (plist-get run :filing)
        :routing (delib-flow--run-routing run)
        :ui (delib-flow--run-ui-package run)))

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

(defun delib-flow--project-subtree-text ()
  "Return the current project subtree body text."
  (buffer-substring-no-properties
   (save-excursion
     (forward-line 1)
     (point))
   (save-excursion
     (org-end-of-subtree t t)
     (point))))

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

(defun delib-flow--project-candidate-at-point (base-dir)
  "Return the current top-level project candidate at point using BASE-DIR."
  (let* ((title (org-get-heading t t t t))
         (tags (org-get-tags))
         (text (delib-flow--project-subtree-text))
         (contacts (delib-flow--text-emails text)))
    (delib-flow--project-candidate
     title
     tags
     contacts
     (delib-flow--project-candidate-terms title tags text)
     (delib-flow--text-org-file-links text base-dir))))

(defun delib-flow--project-candidates-from-file (file)
  "Return project candidates parsed from Org FILE."
  (with-temp-buffer
    (let ((base-dir (file-name-directory file)))
    (insert-file-contents file)
    (org-mode)
    (let (candidates)
      (goto-char (point-min))
      (while (re-search-forward "^\\* \\(.+\\)$" nil t)
        (beginning-of-line)
        (push (delib-flow--project-candidate-at-point base-dir) candidates)
        (org-end-of-subtree t t))
      (nreverse candidates)))))

(defun delib-flow--source-match-text (source)
  "Return normalized source text used for project matching from SOURCE."
  (format "%s\n%s"
          (or (plist-get source :title) "")
          (or (plist-get source :content) "")))

(defun delib-flow--source-match-tags (source)
  "Return normalized tag-like words from SOURCE."
  (delib-flow--string-words (plist-get source :title)))

(defun delib-flow--project-match-score (source candidate)
  "Return match score between SOURCE and CANDIDATE."
  (let* ((candidate-title (plist-get candidate :title))
         (source-title (plist-get source :title))
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

(defun delib-flow--manual-project-choice (package)
  "Return chosen fallback project candidate from PACKAGE."
  (car (delib-flow--manual-project-match-candidates package)))

(defun delib-flow--manual-project-match-result (package)
  "Return raw manual-project-match result for PACKAGE."
  (let* ((choice (delib-flow--manual-project-choice package))
         (candidates (delib-flow--manual-project-match-candidates package)))
    (unless choice
      (error "No project candidates are available for manual selection"))
    (list :match-status 'matched
          :selection-method 'manual
          :best-project choice
          :candidates candidates
          :reason "Manual fallback selected a concrete project candidate.")))

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

(defun delib-flow--matched-project-metadata (package)
  "Return matched project metadata from PACKAGE."
  (plist-get (plist-get (plist-get package :working-context) :project-match)
             :best-project))

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

(defun delib-flow--discovery-linked-file-p (project file)
  "Return non-nil when FILE is directly linked from PROJECT."
  (member file (plist-get project :links)))

(defun delib-flow--discovery-candidate-score (package file)
  "Return retrieval score for PACKAGE against FILE."
  (let* ((terms (delib-flow--discovery-search-terms package))
         (project (delib-flow--matched-project-metadata package))
         (title (delib-flow--zk-note-title file))
         (text (delib-flow--zk-note-text file))
         (title-score (delib-flow--discovery-title-score terms title))
         (text-score (delib-flow--discovery-text-score terms text))
         (tag-score (delib-flow--discovery-tag-score project
                                                     (delib-flow--zk-note-tags file)))
         (contact-score (delib-flow--discovery-contact-score
                         project
                         (delib-flow--zk-note-contacts file))))
    (+ (* 5 (if (delib-flow--discovery-linked-file-p project file) 1 0))
       (* 7 (if (delib-flow--discovery-linked-file-p project file) 1 0))
       (* 3 title-score)
       (* 2 tag-score)
       (* 2 contact-score)
       text-score)))

(defun delib-flow--make-discovery-candidate (file score)
  "Return discovery candidate for FILE with SCORE."
  (list :title (delib-flow--zk-note-title file)
        :file file
        :score score))

(defun delib-flow--scored-discovery-candidates (package files)
  "Return scored discovery candidates for PACKAGE across FILES."
  (let (candidates)
    (dolist (file files (nreverse candidates))
      (let ((score (delib-flow--discovery-candidate-score package file)))
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

(defun delib-flow--make-draft-project (title state first-item tags)
  "Return draft project object for TITLE with STATE, FIRST-ITEM, and TAGS."
  (list :kind 'project
        :title title
        :state state
        :first-item first-item
        :tags tags
        :text title))

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

(defun delib-flow--source-title-waiting-for (package)
  "Return a waiting-for item derived from PACKAGE source title."
  (delib-flow--make-draft-waiting-for
   (format "Waiting for a concrete response about %s"
           (delib-flow--source-title package))
   'source))

(defun delib-flow--retained-candidate-waiting-for (candidate)
  "Return a waiting-for item derived from retained CANDIDATE."
  (delib-flow--make-draft-waiting-for
   (format "Waiting for confirmation from %s"
           (plist-get candidate :title))
   'retained-context))

(defun delib-flow--retained-candidate-waiting-fors (package)
  "Return retained-candidate waiting-for items for PACKAGE."
  (mapcar #'delib-flow--retained-candidate-waiting-for
          (delib-flow--retained-candidates package)))

(defun delib-flow--source-title-reference-note (package)
  "Return a general reference note derived from PACKAGE source title."
  (delib-flow--make-draft-reference-note
   (format "Create general PKM note for %s"
           (delib-flow--source-title package))
   'source
   'general-pkm))

(defun delib-flow--retained-candidate-reference-note (candidate)
  "Return a support-note item derived from retained CANDIDATE."
  (delib-flow--make-draft-reference-note
   (format "Create project support note from %s"
           (plist-get candidate :title))
   'retained-context
   'project-support))

(defun delib-flow--retained-candidate-reference-notes (package)
  "Return retained-candidate reference-note items for PACKAGE."
  (mapcar #'delib-flow--retained-candidate-reference-note
          (delib-flow--retained-candidates package)))

(defun delib-flow--extract-actions-result (package)
  "Return raw action-extraction result for PACKAGE."
  (let* ((source-action (delib-flow--source-title-action package))
         (retained-actions (delib-flow--retained-candidate-actions package))
         (actions (cons source-action retained-actions)))
    (list :candidate-count (length actions)
          :actions actions)))

(defun delib-flow--extract-waiting-for-result (package)
  "Return raw waiting-for extraction result for PACKAGE."
  (let* ((source-item (delib-flow--source-title-waiting-for package))
         (retained-items (delib-flow--retained-candidate-waiting-fors package))
         (items (cons source-item retained-items)))
    (list :candidate-count (length items)
          :waiting-fors items)))

(defun delib-flow--suggest-reference-notes-result (package)
  "Return raw reference-note suggestion result for PACKAGE."
  (let* ((source-item (delib-flow--source-title-reference-note package))
         (retained-items
          (delib-flow--retained-candidate-reference-notes package))
         (items (cons source-item retained-items)))
    (list :candidate-count (length items)
          :reference-notes items)))

(defun delib-flow--project-proposal-tags (package)
  "Return deterministic project tags derived from PACKAGE."
  (seq-take (delib-flow--string-words (delib-flow--source-title package)) 3))

(defun delib-flow--project-proposal-first-item (package)
  "Return deterministic first project item derived from PACKAGE."
  (delib-flow--make-draft-action
   (format "Clarify the first concrete step for %s"
           (delib-flow--source-title package))
   'project-proposal))

(defun delib-flow--proposed-project-item (package)
  "Return deterministic proposed project artifact derived from PACKAGE."
  (delib-flow--make-draft-project
   (delib-flow--source-title package)
   'active
   (delib-flow--project-proposal-first-item package)
   (delib-flow--project-proposal-tags package)))

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
  (let* ((model (delib-flow--cloud-model-choice package))
         (provider (delib-flow--cloud-provider-name model))
         (policy (delib-flow--cloud-provider-policy provider)))
    (unless (delib-flow--cloud-policy-enabled-p policy)
      (error "Cloud routing is disabled for provider %s" provider))
    (list :route 'cloud
          :selected-model model
          :selected-provider provider
          :policy-profile (delib-flow--cloud-policy-profile policy)
          :cloud-switch-pending t
          :sanitization-status 'required
          :reason "Cloud routing is pending sanitized package preparation.")))

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

(defun delib-flow--audit-provider (stage-id)
  "Return audit provider label for STAGE-ID."
  (if (delib-flow--run-cloud-stage-p stage-id)
      "cloud"
    "local"))

(defun delib-flow--audit-model-name (entry)
  "Return audit model name from stage ENTRY."
  (or (plist-get (plist-get entry :raw-output) :selected-model)
      "model-unrecorded"))

(defun delib-flow--make-audit-stage-record (entry)
  "Return audit stage record derived from stage ENTRY."
  (list :stage-id (plist-get entry :stage-id)
        :label (plist-get entry :label)
        :status (plist-get entry :status)
        :review-state (plist-get entry :review-state)
        :prompt-id (plist-get entry :prompt-id)
        :provider (delib-flow--audit-provider (plist-get entry :stage-id))
        :model-name (delib-flow--audit-model-name entry)
        :started-at (plist-get entry :started-at)
        :ended-at (plist-get entry :ended-at)
        :input-package (plist-get entry :input-package)
        :raw-output (plist-get entry :raw-output)
        :normalized-output (plist-get entry :normalized-output)))

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
   (format "** %s\n" (plist-get record :label))
   (delib-flow--audit-properties-text
    `(("STAGE_ID" . ,(symbol-name (plist-get record :stage-id)))
      ("STATUS" . ,(symbol-name (plist-get record :status)))
      ("REVIEW_STATE" . ,(symbol-name (plist-get record :review-state)))
      ("PROMPT_ID" . ,(symbol-name (plist-get record :prompt-id)))
      ("MODEL_PROVIDER" . ,(plist-get record :provider))
      ("MODEL_NAME" . ,(plist-get record :model-name))
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

(defun delib-flow--append-audit-record (run entry)
  "Return RUN with audit state updated from stage ENTRY."
  (let* ((audit (plist-get run :audit))
         (stage-record (delib-flow--make-audit-stage-record entry))
         (updated-audit
          (plist-put
           (plist-put
            (plist-put audit :run-record (delib-flow--updated-run-record run))
            :stage-records
            (append (plist-get audit :stage-records) (list stage-record)))
           :pending-checkpoints
           (list (plist-get entry :stage-id)))))
    (plist-put run :audit updated-audit)))

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
  (let* ((audit (plist-get run :audit))
         (updated-run
          (plist-put
           run :audit
           (plist-put audit :run-record (delib-flow--updated-run-record run)))))
    (delib-flow--persist-audit-state updated-run checkpoint)))

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

(defun delib-flow--manual-project-match-action (run)
  "Return the manual-project-match action for RUN."
  (when (or (memq (delib-flow--match-status run) '(ambiguous no-match))
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

(defun delib-flow--extract-waiting-for-action (run)
  "Return the extract-waiting-for action for RUN."
  (when (eq (delib-flow--match-status run) 'matched)
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
  (when (eq (delib-flow--match-status run) 'matched)
    (delib-flow--make-action
     'suggest-reference-notes
     (if (delib-flow--stage-executed-p run 'suggest-reference-notes)
         "Retry Suggest Reference Notes"
       "Suggest Reference Notes")
     'available
     nil
     #'delib-flow-action-suggest-reference-notes
     60)))

(defun delib-flow--propose-new-project-action (run)
  "Return the propose-new-project action for RUN."
  (when (eq (delib-flow--match-status run) 'no-match)
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

(defun delib-flow--run-cloud-stage-action (run)
  "Return the run-cloud-stage action for RUN."
  (when (or (eq (plist-get (delib-flow--run-routing run) :sanitization-status)
                'approved)
            (delib-flow--stage-executed-p run 'run-cloud-stage))
    (delib-flow--make-action
     'run-cloud-stage
     (if (delib-flow--stage-executed-p run 'run-cloud-stage)
         "Retry Run Cloud Stage"
       "Run Cloud Stage")
     'available
     nil
     #'delib-flow-action-run-cloud-stage
     87)))

(defun delib-flow--approve-candidate-reintegration-action (run)
  "Return the approve-candidate-reintegration action for RUN."
  (when (or (eq (plist-get (delib-flow--run-routing run) :reintegration-status)
                'pending-review)
            (delib-flow--stage-executed-p run 'approve-candidate-reintegration))
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
         "Approve Next Filing Action"
       "Select Approved Filing Actions")
     'available
     nil
     #'delib-flow-action-select-approved-filing-actions
     88)))

(defun delib-flow--file-approved-outputs-action (run)
  "Return the file-approved-outputs action for RUN."
  (when (delib-flow--approved-items-ready-p run)
    (delib-flow--make-action
     'file-approved-outputs
     "File Approved Outputs"
     'available
     nil
     #'delib-flow-action-file-approved-outputs
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
     (delib-flow--match-project-action run)
     (delib-flow--discover-reference-material-action run)
     (delib-flow--filter-reference-material-action run)
     (delib-flow--decide-cloud-pass-action run)
     (delib-flow--sanitize-for-cloud-action run)
     (delib-flow--approve-cloud-send-action run)
     (delib-flow--run-cloud-stage-action run)
     (delib-flow--approve-candidate-reintegration-action run)
     (delib-flow--integrate-into-source-action run)
     (delib-flow--select-approved-filing-actions-action run)
     (delib-flow--file-approved-outputs-action run))))

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
           (delib-flow--manual-project-match-action run)
           (delib-flow--extract-actions-action run)
           (delib-flow--extract-waiting-for-action run)
           (delib-flow--suggest-reference-notes-action run))
        (list
         (delib-flow--manual-project-match-action run)
         (delib-flow--propose-new-project-action run)))
       (list
       (delib-flow--filter-reference-material-action run)
       (delib-flow--decide-cloud-pass-action run)
       (delib-flow--sanitize-for-cloud-action run)
       (delib-flow--approve-cloud-send-action run)
       (delib-flow--run-cloud-stage-action run)
       (delib-flow--approve-candidate-reintegration-action run)
       (delib-flow--integrate-into-source-action run)
       (delib-flow--select-approved-filing-actions-action run)
       (delib-flow--file-approved-outputs-action run))))))

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
  (format "Cloud pass selected. Model: %s. Provider: %s. Policy: %s. Sanitization status: %s."
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
         (cloud-status (delib-flow--cloud-context-status _run))
         (cloud-text (delib-flow--cloud-context-text _run))
         (reviewed-cloud-text (delib-flow--reviewed-cloud-package-text _run))
         (returned-status (delib-flow--cloud-returned-context-status working))
         (returned-text (delib-flow--cloud-returned-context-text working))
         (retained-text (or retained-context
                            "No retained context is available yet.")))
    (format
     "** Context status\nInspect output: %s.\nProject match: %s.\nRetrieved context: %s.\nFiltered context: %s.\nCloud-sanitized context: %s.\nReviewed cloud package: %s.\nCloud-returned context: %s.\n\n** Project context\n%s\n\n** Retrieved candidates\n%s\n\n** Filtered context\n%s\n\n** Retained context\n%s\n\n** Cloud-sanitized context\n%s\n\n** Reviewed cloud package\n%s\n\n** Cloud-returned context\n%s\n\n** Editable working slice\n%s"
     inspect-status
     project-status
     retrieved-status
     filtered-status
     cloud-status
     (if (string-empty-p reviewed-cloud-text) "not available" "available")
     returned-status
     project-text
     retrieved-text
     filtered-text
     retained-text
     cloud-text
     (delib-flow--render-editable-block _run 'cloud-package-review)
     returned-text
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
  (format "- %s %s"
          (delib-flow--draft-item-keyword item)
          (plist-get item :text)))

(defun delib-flow--draft-item-keyword (item)
  "Return Org keyword prefix for draft ITEM."
  (cond
   ((eq (plist-get item :kind) 'project) "PROJECT")
   ((eq (plist-get item :kind) 'waiting-for) "WAITING")
   ((eq (plist-get item :kind) 'reference-note) "NOTE")
   (t "TODO")))

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

(defun delib-flow--filed-location-status (run)
  "Return filed-location status text for RUN."
  (if (plist-get (plist-get run :filing) :target-locations)
      "Filed target locations are available."
    "No filed target locations are available yet."))

(defun delib-flow--filed-location-line (location)
  "Return preview line for filed LOCATION."
  (format "- %s -> %s"
          (plist-get location :item-text)
          (plist-get location :target)))

(defun delib-flow--filed-location-text (run)
  "Return filed-location body text for RUN."
  (let ((locations (plist-get (plist-get run :filing) :target-locations)))
    (if locations
        (mapconcat #'delib-flow--filed-location-line locations "\n")
      "No filed target locations are available yet.")))

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
  (format "** Preview status\n%s\n\n** Draft artifacts\n%s\n\n** Approved artifacts\n%s\n\n** Filing conflicts\n%s\n\n** Filed target locations\n%s\n"
          (delib-flow--draft-item-status _run)
          (delib-flow--draft-item-text _run)
          (delib-flow--approved-item-text _run)
          (delib-flow--filing-conflict-text _run)
          (delib-flow--filed-location-text _run)))

(defun delib-flow--audit-run-state-text (run)
  "Return run audit status text for RUN."
  (let* ((audit (plist-get run :audit))
         (run-record (plist-get audit :run-record)))
    (format "- Audit log file: %s\n- Run status: %s\n- Run ID: %s"
            (if (delib-flow--audit-log-configured-p) "configured" "unconfigured")
            (plist-get run-record :run-status)
            (plist-get run-record :run-id))))

(defun delib-flow--audit-stage-readiness-text (run)
  "Return stage audit readiness text for RUN."
  (let* ((audit (plist-get run :audit))
         (stage-count (length (plist-get audit :stage-records)))
         (pending (plist-get audit :pending-checkpoints))
         (last (plist-get audit :last-appended-checkpoint)))
    (format "- Recorded stages: %s\n- Pending checkpoints: %s\n- Last appended checkpoint: %s"
            stage-count
            (if pending
                (mapconcat #'symbol-name pending ", ")
              "none")
            (if last (symbol-name last) "none"))))

(defun delib-flow--render-audit-status-section (_run)
  "Return Org text for the Audit status section."
  (format "** Run audit state\n%s\n\n** Stage audit readiness\n%s\n"
          (delib-flow--audit-run-state-text _run)
          (delib-flow--audit-stage-readiness-text _run)))

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

(defun delib-flow--match-project-source (package)
  "Return the source object used for project matching from PACKAGE."
  (plist-get package :source))

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

(defun delib-flow--cloud-stage-result (package)
  "Return raw cloud-stage result for PACKAGE."
  (let ((sanitized-package (delib-flow--cloud-stage-package package)))
    (list :selected-model (delib-flow--cloud-model-choice package)
          :cloud-output
          (format "Cloud output for reviewed package.\n%s"
                  sanitized-package)
          :cloud-switch-pending nil
          :sanitization-status 'returned
          :reason "Cloud output is ready for local review and reintegration.")))

(defun delib-flow--execute-run-cloud-stage (package)
  "Return raw cloud-stage output for PACKAGE."
  (delib-flow--cloud-stage-result package))

(defun delib-flow--approve-candidate-reintegration-result (_package)
  "Return raw reintegration approval result for _PACKAGE."
  (list :reintegration-status 'approved
        :reason "Cloud-returned result is approved for local reintegration."))

(defun delib-flow--execute-approve-candidate-reintegration (package)
  "Return raw reintegration approval output for PACKAGE."
  (delib-flow--approve-candidate-reintegration-result package))

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
  (let* ((integrated-context (delib-flow--integrated-context-text package))
         (draft-items (plist-get (plist-get package :filing) :draft-items)))
    (list :integrated-context integrated-context
          :draft-count (length draft-items)
          :cloud-context-integrated-p
          (not (null (plist-get (plist-get package :working-context)
                                :cloud-returned-context)))
          :reason "Integrated local context and draft filing artifacts are ready for review.")))

(defun delib-flow--execute-integrate-into-source (package)
  "Return raw integration output for PACKAGE."
  (delib-flow--integrate-into-source-result package))

(defun delib-flow--draft-items (package)
  "Return draft filing items from PACKAGE."
  (plist-get (plist-get package :filing) :draft-items))

(defun delib-flow--selected-filing-items (package)
  "Return the next deterministically selected filing item from PACKAGE."
  (if-let ((item (car (delib-flow--draft-items package))))
      (list item)
    nil))

(defun delib-flow--remaining-draft-items (package)
  "Return unapproved draft filing items from PACKAGE."
  (cdr (delib-flow--draft-items package)))

(defun delib-flow--select-approved-filing-actions-result (package)
  "Return raw filing-selection result for PACKAGE."
  (let* ((selected-items (delib-flow--selected-filing-items package))
         (remaining-items (delib-flow--remaining-draft-items package)))
    (list :approved-items selected-items
          :remaining-draft-items remaining-items
          :selected-count (length selected-items)
          :remaining-draft-count (length remaining-items)
          :selected-preview (and selected-items
                                 (delib-flow--draft-item-preview-text
                                  selected-items))
          :reason "Approved the next filing artifact for deterministic filing review.")))

(defun delib-flow--execute-select-approved-filing-actions (package)
  "Return raw filing-selection output for PACKAGE."
  (delib-flow--select-approved-filing-actions-result package))

(defun delib-flow--approved-items (package)
  "Return approved filing items from PACKAGE."
  (plist-get (plist-get package :filing) :approved-items))

(defun delib-flow--matched-project-title (package)
  "Return matched project title from PACKAGE."
  (plist-get (plist-get (plist-get (plist-get package :working-context)
                                   :project-match)
                        :best-project)
             :title))

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

(defun delib-flow--project-heading-text (item)
  "Return top-level Org heading text for project ITEM."
  (format "* %s\n" (plist-get item :title)))

(defun delib-flow--project-first-item (item)
  "Return first child item from project ITEM."
  (plist-get item :first-item))

(defun delib-flow--insert-project-child (file project-title item)
  "Insert ITEM as child under PROJECT-TITLE in Org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (unless (re-search-forward
             (format "^\\(\\*+\\) %s$" (regexp-quote project-title))
             nil t)
      (error "Matched project heading is not present in the My Projects file"))
    (let ((level (1+ (length (match-string 1)))))
      (org-end-of-subtree t t)
      (unless (bolp)
        (insert "\n"))
      (insert (delib-flow--project-item-heading item level))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun delib-flow--insert-new-project (file item)
  "Insert new project ITEM into Org FILE."
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (org-mode)
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert (delib-flow--project-heading-text item))
    (insert (delib-flow--project-item-heading
             (delib-flow--project-first-item item)
             2))
    (write-region (point-min) (point-max) file nil 'silent)))

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

(defun delib-flow--reference-note-file (item)
  "Return deterministic note file path for approved ITEM."
  (unless (and delib-flow-zk-root
               (file-directory-p delib-flow-zk-root))
    (error "The ZK root is not configured or readable"))
  (expand-file-name
   (format "%s.org" (delib-flow--slugify
                     (delib-flow--reference-note-title item)))
   delib-flow-zk-root))

(defun delib-flow--reference-note-template (item)
  "Return configured note template for approved ITEM."
  (if (eq (plist-get item :note-type) 'project-support)
      delib-flow-project-support-note-template
    delib-flow-general-note-template))

(defun delib-flow--reference-note-template-bindings (item)
  "Return template bindings for approved ITEM."
  (list (cons "${title}" (delib-flow--reference-note-title item))
        (cons "${source-artifact}" (plist-get item :text))
        (cons "${note-type}" (symbol-name (plist-get item :note-type)))))

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

(defun delib-flow--reference-note-content (item)
  "Return note file content for approved ITEM."
  (delib-flow--render-reference-note-template
   (delib-flow--reference-note-template item)
   (delib-flow--reference-note-template-bindings item)))

(defun delib-flow--create-reference-note-file (item)
  "Create deterministic note file for approved ITEM."
  (let ((target (delib-flow--reference-note-file item)))
    (when (file-exists-p target)
      (error "Deterministic note target already exists"))
    (with-temp-file target
      (insert (delib-flow--reference-note-content item)))
    target))

(defun delib-flow--reference-note-link (item target)
  "Return Org file link for reference-note ITEM at TARGET."
  (let ((project-dir (file-name-directory delib-flow-my-projects-file)))
    (format "[[file:%s][%s]]"
            (file-relative-name target project-dir)
            (delib-flow--reference-note-title item))))

(defun delib-flow--matched-project-point (project-title)
  "Move point to matched PROJECT-TITLE in current Org buffer."
  (goto-char (point-min))
  (unless (re-search-forward
           (format "^\\(\\*+\\) %s$" (regexp-quote project-title))
           nil t)
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

(defun delib-flow--update-project-reference-files (project-title item target)
  "Update matched PROJECT-TITLE metadata for reference-note ITEM at TARGET."
  (unless delib-flow-my-projects-file
    (error "The My Projects file is not configured"))
  (with-temp-buffer
    (insert-file-contents delib-flow-my-projects-file)
    (org-mode)
    (delib-flow--matched-project-point project-title)
    (org-entry-put
     (point)
     "REFERENCE_FILES"
     (delib-flow--project-reference-files-value
      (org-entry-get (point) "REFERENCE_FILES")
      (delib-flow--reference-note-link item target)))
    (write-region (point-min) (point-max) delib-flow-my-projects-file nil 'silent))
  (format "%s::%s:REFERENCE_FILES" delib-flow-my-projects-file project-title))

(defun delib-flow--project-support-note-p (item)
  "Return non-nil when ITEM is a project-support reference note."
  (and (eq (plist-get item :kind) 'reference-note)
       (eq (plist-get item :note-type) 'project-support)))

(defun delib-flow--project-support-note-metadata-location (item package target)
  "Return metadata target after filing support-note ITEM from PACKAGE to TARGET."
  (let ((project-title (delib-flow--matched-project-title package)))
    (when project-title
      (delib-flow--update-project-reference-files project-title item target))))

(defun delib-flow--project-heading-exists-p (file title)
  "Return non-nil when Org FILE already contains top-level TITLE."
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (goto-char (point-min))
    (re-search-forward
     (format "^\\* %s$" (regexp-quote title))
     nil t)))

(defun delib-flow--project-child-exists-p (file project-title item)
  "Return non-nil when Org FILE already contains ITEM under PROJECT-TITLE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (when (re-search-forward
           (format "^\\(\\*+\\) %s$" (regexp-quote project-title))
           nil t)
      (let ((level (1+ (length (match-string 1))))
            (limit (save-excursion
                     (org-end-of-subtree t t)
                     (point))))
        (re-search-forward
         (format "^%s$"
                 (regexp-quote
                  (string-trim
                   (delib-flow--project-item-heading item level))))
         limit t)))))

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

(defun delib-flow--reference-note-conflict (item)
  "Return filing conflict for reference-note ITEM, or nil."
  (let ((target (delib-flow--reference-note-file item)))
    (when (file-exists-p target)
      (list :kind 'reference-note
            :item-text (plist-get item :text)
            :reason (format "The deterministic note target already exists: %s"
                            target)))))

(defun delib-flow--approved-item-conflict (item package)
  "Return filing conflict for approved ITEM from PACKAGE, or nil."
  (if (eq (plist-get item :kind) 'reference-note)
      (delib-flow--reference-note-conflict item)
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

(defun delib-flow--file-reference-note (item)
  "File approved reference-note ITEM into the ZK root."
  (delib-flow--create-reference-note-file item))

(defun delib-flow--filed-location (item target)
  "Return target-location object for ITEM filed to TARGET."
  (list :kind (plist-get item :kind)
        :item-text (plist-get item :text)
        :target target))

(defun delib-flow--reference-note-locations (item package)
  "Return target locations after filing reference-note ITEM from PACKAGE."
  (let* ((target (delib-flow--file-reference-note item))
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
              :reason "Approved filing artifacts were inserted into deterministic targets.")))))

(defun delib-flow--execute-file-approved-outputs (package)
  "Return raw filing output for PACKAGE."
  (delib-flow--file-approved-outputs-result package))

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

(defun delib-flow--normalize-manual-project-match-output (raw-output)
  "Return normalized manual project-match text from RAW-OUTPUT."
  (format "- Match status: %s\n- Selection method: %s\n- Selected project: %s\n- Reason: %s"
          (plist-get raw-output :match-status)
          (plist-get raw-output :selection-method)
          (plist-get (plist-get raw-output :best-project) :title)
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

(defun delib-flow--normalize-extract-actions-output (raw-output)
  "Return normalized extract-actions text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s"
          (plist-get raw-output :candidate-count)
          (mapconcat #'delib-flow--normalize-draft-action
                     (plist-get raw-output :actions)
                     "\n")))

(defun delib-flow--normalize-extract-waiting-for-output (raw-output)
  "Return normalized extract-waiting-for text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s"
          (plist-get raw-output :candidate-count)
          (mapconcat #'delib-flow--normalize-draft-action
                     (plist-get raw-output :waiting-fors)
                     "\n")))

(defun delib-flow--normalize-suggest-reference-notes-output (raw-output)
  "Return normalized reference-note text from RAW-OUTPUT."
  (format "- Candidate count: %s\n%s"
          (plist-get raw-output :candidate-count)
          (mapconcat #'delib-flow--normalize-draft-action
                     (plist-get raw-output :reference-notes)
                     "\n")))

(defun delib-flow--normalize-decide-cloud-pass-output (raw-output)
  "Return normalized cloud-routing text from RAW-OUTPUT."
  (format "- Route: %s\n- Selected model: %s\n- Provider: %s\n- Policy profile: %s\n- Sanitization status: %s\n- Reason: %s"
          (plist-get raw-output :route)
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
  (format "- Selected model: %s\n- Sanitization status: %s\n- Reason: %s\n%s"
          (plist-get raw-output :selected-model)
          (plist-get raw-output :sanitization-status)
          (plist-get raw-output :reason)
          (plist-get raw-output :cloud-output)))

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

(defun delib-flow--normalize-select-approved-filing-actions-output (raw-output)
  "Return normalized filing-selection text from RAW-OUTPUT."
  (format "- Selected artifact count: %s\n- Remaining draft artifact count: %s\n- Reason: %s\n%s"
          (plist-get raw-output :selected-count)
          (plist-get raw-output :remaining-draft-count)
          (plist-get raw-output :reason)
          (or (plist-get raw-output :selected-preview)
              "No filing artifacts were selected.")))

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

(defun delib-flow--apply-propose-new-project-entry (run entry)
  "Return RUN updated from completed propose-new-project ENTRY."
  (let* ((working (delib-flow--run-working-context run))
         (filing (plist-get run :filing))
         (project-item (plist-get (plist-get entry :raw-output) :project))
         (draft-items (list project-item)))
    (plist-put
     (plist-put
     run :working-context
      (plist-put working :project-proposal (plist-get entry :raw-output)))
     :filing
     (plist-put
      (plist-put
       (plist-put filing :draft-items draft-items)
       :approved-items nil)
      :preview-text (delib-flow--draft-item-preview-text draft-items)))))

(defun delib-flow--apply-extract-actions-entry (run entry)
  "Return RUN updated from completed extract-actions ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items (plist-get (plist-get entry :raw-output) :actions)))
    (plist-put
     run :filing
     (plist-put
      (plist-put
       (plist-put filing :draft-items draft-items)
       :approved-items nil)
      :preview-text (delib-flow--draft-item-preview-text draft-items)))))

(defun delib-flow--apply-extract-waiting-for-entry (run entry)
  "Return RUN updated from completed extract-waiting-for ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items (plist-get (plist-get entry :raw-output) :waiting-fors)))
    (plist-put
     run :filing
     (plist-put
      (plist-put
       (plist-put filing :draft-items draft-items)
       :approved-items nil)
      :preview-text (delib-flow--draft-item-preview-text draft-items)))))

(defun delib-flow--apply-suggest-reference-notes-entry (run entry)
  "Return RUN updated from completed suggest-reference-notes ENTRY."
  (let* ((filing (plist-get run :filing))
         (draft-items
          (plist-get (plist-get entry :raw-output) :reference-notes)))
    (plist-put
     run :filing
     (plist-put
      (plist-put
       (plist-put filing :draft-items draft-items)
       :approved-items nil)
      :preview-text (delib-flow--draft-item-preview-text draft-items)))))

(defun delib-flow--apply-decide-cloud-pass-entry (run entry)
  "Return RUN updated from completed decide-cloud-pass ENTRY."
  (let* ((routing (delib-flow--run-routing run))
         (raw (plist-get entry :raw-output))
         (updated-routing
          (plist-put
           (plist-put
            (plist-put
             (plist-put routing :cloud-switch-pending
                        (plist-get raw :cloud-switch-pending))
             :sanitization-status
             (plist-get raw :sanitization-status))
            :cloud-policy-profile
            (plist-get raw :policy-profile))
           :selected-cloud-provider
           (plist-get raw :selected-provider))))
    (plist-put
     run :routing
     (plist-put updated-routing
                :selected-cloud-model
                (plist-get raw :selected-model)))))

(defun delib-flow--apply-sanitize-for-cloud-entry (run entry)
  "Return RUN updated from completed sanitize-for-cloud ENTRY."
  (let* ((working (delib-flow--run-working-context run))
         (routing (delib-flow--run-routing run))
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
  (let* ((working (delib-flow--run-working-context run))
         (routing (delib-flow--run-routing run))
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
         (routing (delib-flow--run-routing run))
         (raw (plist-get entry :raw-output)))
    (plist-put
     (plist-put
     run :working-context
      (plist-put working :cloud-returned-context
                 (plist-get raw :cloud-output)))
     :routing
     (plist-put
      (plist-put
       (plist-put routing :cloud-switch-pending
                  (plist-get raw :cloud-switch-pending))
       :sanitization-status
       (plist-get raw :sanitization-status))
      :reintegration-status 'pending-review))))

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
  (let* ((working (delib-flow--run-working-context run))
         (filing (plist-get run :filing)))
    (plist-put
     (plist-put
      run :working-context
      (plist-put working :retained-context
                 (plist-get (plist-get entry :raw-output) :integrated-context)))
     :filing
     (plist-put filing :approved-items nil))))

(defun delib-flow--apply-select-approved-filing-actions-entry (run entry)
  "Return RUN updated from completed filing-selection ENTRY."
  (let* ((filing (plist-get run :filing))
         (raw (plist-get entry :raw-output))
         (remaining-items (plist-get raw :remaining-draft-items)))
    (plist-put
     run :filing
     (plist-put
      (plist-put
       (plist-put filing :draft-items remaining-items)
       :approved-items (plist-get raw :approved-items))
      :preview-text (delib-flow--draft-item-preview-text remaining-items)))))

(defun delib-flow--apply-file-approved-outputs-entry (run entry)
  "Return RUN updated from completed file-approved-outputs ENTRY."
  (let* ((filing (plist-get run :filing))
         (raw (plist-get entry :raw-output))
         (conflicts (plist-get raw :conflicts)))
    (plist-put
     run :filing
     (if conflicts
         (plist-put filing :conflicts conflicts)
       (plist-put
        (plist-put
         (plist-put filing :approved-items nil)
         :conflicts nil)
        :target-locations
        (append (plist-get filing :target-locations)
                (plist-get raw :target-locations)))))))

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

(defun delib-flow--finalize-stage-run (run entry)
  "Return finalized RUN after applying stage ENTRY and audit updates."
  (delib-flow--seed-actions
   (delib-flow--finalize-audit-update
    (delib-flow--apply-stage-entry
     (delib-flow--append-stage-entry run entry)
     entry)
    entry)))

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
          (delib-flow--finalize-stage-run run entry))
      (error
       (let ((entry
              (delib-flow--make-stage-failure-entry
               stage-id package (error-message-string err))))
         (delib-flow--finalize-stage-run run entry))))))

(defun delib-flow--run-stage-in-cloud (run stage-id)
  "Return RUN after executing STAGE-ID through the cloud stage contract."
  (let* ((descriptor (delib-flow--stage-descriptor stage-id))
         (package (delib-flow--stage-input-package run stage-id)))
    (condition-case err
        (let* ((raw-output (funcall delib-flow-cloud-stage-adapter
                                    descriptor package))
               (normalized-output
                (delib-flow--normalize-stage-output stage-id raw-output))
               (entry (delib-flow--make-stage-entry
                       stage-id package raw-output normalized-output)))
          (delib-flow--finalize-stage-run run entry))
      (error
       (let ((entry
              (delib-flow--make-stage-failure-entry
               stage-id package (error-message-string err))))
         (delib-flow--finalize-stage-run run entry))))))

(defun delib-flow--initialize-run (source-snapshot)
  "Create a new run state from SOURCE-SNAPSHOT."
  (let ((session (delib-flow--initial-session-state)))
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
               :cloud-returned-context nil
               :editable-block-ids '(context-main operator-notes cloud-package-review))
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
               :selected-cloud-provider nil
               :cloud-policy-profile nil
               :sanitization-status nil
               :reintegration-status nil)
         :filing
         (list :draft-items nil
               :approved-items nil
               :preview-text nil
               :conflicts nil
               :target-locations nil)
         :audit
         (delib-flow--initial-audit-state source-snapshot session)
         :session
         session
         :ui
         (delib-flow--initial-ui-state)))))

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

(defun delib-flow--rerender-active-run-buffer ()
  "Rerender the live control buffer from `delib-flow--active-run'."
  (let ((buffer (delib-flow--control-buffer)))
    (if (buffer-live-p buffer)
        (delib-flow--refresh-buffer-sections delib-flow--active-run buffer)
      (delib-flow--render-control-buffer delib-flow--active-run))))

(defun delib-flow-action-inspect-source ()
  "Execute the inspect-source stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
        (delib-flow--run-stage-locally delib-flow--active-run
                                       'inspect-source)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-match-project ()
  "Execute the match-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'match-project)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-discover-reference-material ()
  "Execute the discover-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'discover-reference-material)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-filter-reference-material ()
  "Execute the filter-reference-material stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'filter-reference-material)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-manual-project-match ()
  "Execute the manual-project-match stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'manual-project-match)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-propose-new-project ()
  "Execute the propose-new-project stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'propose-new-project)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-extract-actions ()
  "Execute the extract-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'extract-actions)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-extract-waiting-for ()
  "Execute the extract-waiting-for stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'extract-waiting-for)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-suggest-reference-notes ()
  "Execute the suggest-reference-notes stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally delib-flow--active-run
                                        'suggest-reference-notes)))
  (delib-flow--rerender-active-run-buffer))

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
  (delib-flow--rerender-active-run-buffer))

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
  (delib-flow--rerender-active-run-buffer))

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
  (delib-flow--rerender-active-run-buffer))

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
  (delib-flow--rerender-active-run-buffer))

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
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-integrate-into-source ()
  "Execute the integrate-into-source stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'integrate-into-source)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-select-approved-filing-actions ()
  "Execute the select-approved-filing-actions stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'select-approved-filing-actions)))
  (delib-flow--rerender-active-run-buffer))

(defun delib-flow-action-file-approved-outputs ()
  "Execute the file-approved-outputs stage for the active run."
  (interactive)
  (unless delib-flow--active-run
    (user-error "No active delib-flow run"))
  (setq delib-flow--active-run
        (delib-flow--seed-actions
         (delib-flow--run-stage-locally
          (delib-flow--sync-run-from-control-buffer delib-flow--active-run)
          'file-approved-outputs)))
  (delib-flow--rerender-active-run-buffer))

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
  (let ((source (delib-flow--snapshot-heading)))
    (setq delib-flow--active-run
          (delib-flow--initialize-run source))
    (let ((buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook #'delib-flow--control-buffer-killed nil t))
      (pop-to-buffer buffer))))

(provide 'delib-flow)
;;; delib-flow.el ends here
