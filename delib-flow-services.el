;;; delib-flow-services.el --- Service scaffolding for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Deterministic local services will move here during refactoring.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'subr-x)

(unless (fboundp 'delib-flow--define-function)
  (defmacro delib-flow--define-function (name args &rest body)
    "Compatibility wrapper used while service helpers migrate out of the facade."
    (declare (indent defun))
    `(defun ,name ,args ,@body)))

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

When OUTLINE-PATH is nil, return top-level headings. Otherwise return the
direct child headings beneath the heading matching OUTLINE-PATH."
  (with-temp-buffer
    (let ((buffer-file-name file))
      (insert-file-contents file)
      (org-mode)
      (save-excursion
        (save-restriction
          (widen)
          (let* ((path (mapcar #'delib-flow--plain-string outline-path))
                 (target-level (if path (1+ (length path)) 1))
                 (stack nil)
                 (path-found (null path))
                 snapshots)
            (goto-char (point-min))
            (while (re-search-forward org-heading-regexp nil t)
              (goto-char (match-beginning 0))
              (let* ((level (or (org-current-level) 0))
                     (title (delib-flow--plain-string
                             (org-get-heading t t t t))))
                (setq stack
                      (append (seq-take stack (max 0 (1- level)))
                              (list title)))
                (when (equal stack path)
                  (setq path-found t))
                (when (and (= level target-level)
                           (equal (butlast stack) path))
                  (push (delib-flow--snapshot-heading) snapshots)))
              (forward-line 1))
            (unless path-found
              (user-error
               "Inbox outline path not found: %s"
               (mapconcat #'identity path " > ")))
            (nreverse snapshots)))))))

(defun delib-flow--inbox-selection-labels (snapshots)
  "Return completion labels mapped to SNAPSHOTS."
  (let ((index 0))
    (mapcar
     (lambda (snapshot)
       (setq index (1+ index))
       (cons (format "[%s] %s"
                     index
                     (or (plist-get snapshot :title) "Untitled heading"))
             snapshot))
     snapshots)))

(defun delib-flow--start-run-from-source (source)
  "Initialize and render a run from frozen SOURCE."
  (pop-to-buffer
   (delib-flow--render-active-run-buffer
    (setq delib-flow--active-run
          (delib-flow--initialize-run source))
    "Now")))

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
         (selection
          (downcase
           (or (delib-flow--manual-project-selection-value package) "")))
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
               (tag-text (if tags (string-join tags ", ") "none"))
               (contact-count (length (plist-get candidate :contacts)))
               (link-count (length (plist-get candidate :links))))
          (push
           (format
            "- [%s]%s %s\n  Tags: %s\n  Signals: %s contact(s), %s link(s)"
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
          (delib-flow--render-editable-block-with-editor-help
           run
           'manual-project-selection)))

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
         (base-text (let ((current
                           (delib-flow--manual-project-selection-block-text
                            run)))
                      (if (delib-flow--non-empty-string-p current)
                          current
                        (delib-flow--manual-project-selection-template
                         candidates))))
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
      (or (seq-find
           (lambda (candidate)
             (string-equal
              (downcase selection)
              (downcase (plist-get candidate :title))))
           candidates)
          (error
           "Manual project selection did not match any available candidate: %s"
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
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun delib-flow--zk-note-source-title (file)
  "Return embedded source title declared in FILE, if any."
  (when-let ((text (and file (delib-flow--zk-note-text file))))
    (when (string-match "^- Source title:[ \t]*\\(.+\\)$" text)
      (string-trim (match-string 1 text)))))

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

(defun delib-flow--same-existing-file-p (left right)
  "Return non-nil when LEFT and RIGHT name the same existing file."
  (and left
       right
       (file-exists-p left)
       (file-exists-p right)
       (string= (file-truename left)
                (file-truename right))))

(defun delib-flow--discovery-source-file-p (package file)
  "Return non-nil when FILE is the active source file for PACKAGE."
  (delib-flow--same-existing-file-p
   file
   (plist-get (plist-get package :source) :file)))

(defun delib-flow--discovery-source-mirror-note-p (package file)
  "Return non-nil when FILE is a generated mirror of PACKAGE source."
  (let ((candidate-source-title
         (delib-flow--normalize-match-source-title
          (delib-flow--zk-note-source-title file)))
        (active-source-title
         (delib-flow--normalize-match-source-title
          (plist-get (plist-get package :source) :title))))
    (and (delib-flow--non-empty-string-p candidate-source-title)
         (delib-flow--non-empty-string-p active-source-title)
         (string= (downcase candidate-source-title)
                  (downcase active-source-title)))))

(defun delib-flow--discovery-excluded-file-p (package file)
  "Return non-nil when FILE should be excluded from discovery for PACKAGE."
  (or (delib-flow--discovery-project-file-p file)
      (delib-flow--discovery-source-file-p package file)
      (delib-flow--discovery-source-mirror-note-p package file)))

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

(defun delib-flow--remove-first-matching-item (items selected)
  "Return ITEMS with first occurrence of SELECTED removed."
  (let ((removed nil))
    (seq-remove
     (lambda (item)
       (if (and (not removed)
                (delib-flow--same-artifact-item-p item selected))
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

(defun delib-flow--discovery-signals (package file)
  "Return weighted discovery signals for PACKAGE against FILE."
  (let* ((terms (delib-flow--discovery-search-terms package))
         (project (delib-flow--matched-project-metadata package))
         (title (delib-flow--zk-note-title file))
         (text (delib-flow--zk-note-text file))
         (note-contacts (delib-flow--zk-note-contacts file))
         (note-links (delib-flow--zk-note-links file))
         (linked-file-p (if (delib-flow--discovery-linked-file-p project file) 1 0))
         (source-linked-file-p
          (if (delib-flow--discovery-source-linked-file-p package file) 1 0))
         (title-score (delib-flow--discovery-title-score terms title))
         (text-score (delib-flow--discovery-text-score terms text))
         (tag-score
          (delib-flow--discovery-tag-score project
                                           (delib-flow--zk-note-tags file)))
         (contact-score
          (delib-flow--discovery-contact-score project note-contacts))
         (source-contact-score
          (delib-flow--discovery-source-contact-score package note-contacts))
         (shared-link-score
          (delib-flow--discovery-shared-link-score package note-links)))
    (list (delib-flow--discovery-signal 'linked-project-file linked-file-p 12)
          (delib-flow--discovery-signal 'linked-source-file source-linked-file-p 15)
          (delib-flow--discovery-signal 'title-overlap title-score 3)
          (delib-flow--discovery-signal 'project-tag-overlap tag-score 2)
          (delib-flow--discovery-signal 'project-contact-overlap contact-score 2)
          (delib-flow--discovery-signal 'source-contact-overlap source-contact-score 3)
          (delib-flow--discovery-signal
           'shared-project-or-source-link
           shared-link-score
           4)
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
      (unless (delib-flow--discovery-excluded-file-p package file)
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
     (plist-put copy :filter-status status)
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

(delib-flow--define-function delib-flow--selected-family-support-item
                             (package family)
                             "Return the currently selected support target for artifact FAMILY in PACKAGE."
                             (pcase family
                               ('actions
                                (or
                                 (delib-flow--selected-action-draft-from-package
                                  package)
                                 (delib-flow--selected-action-candidate-for-drafting
                                  package)))
                               ('waiting-fors
                                (or
                                 (delib-flow--selected-waiting-for-draft-from-package
                                  package)
                                 (delib-flow--selected-waiting-for-candidate-for-drafting
                                  package)))
                               ('reference-notes
                                (or
                                 (delib-flow--selected-reference-note-draft-from-package
                                  package)
                                 (delib-flow--selected-reference-note-candidate-for-drafting
                                  package)))
                               ('project-proposals
                                (or
                                 (delib-flow--selected-project-draft-from-package
                                  package)
                                 (delib-flow--selected-project-candidate-for-drafting
                                  package)))
                               (_ nil)))

(delib-flow--define-function delib-flow--selected-support-family-label
                             (family)
                             "Return user-facing singular label for artifact FAMILY."
                             (pcase family
                               ('actions "action")
                               ('waiting-fors "waiting-for")
                               ('reference-notes "note")
                               ('project-proposals "project")
                               (_ "artifact")))

(delib-flow--define-function delib-flow--selected-support-query-text
                             (item family)
                             "Return focused support query text for ITEM in FAMILY."
                             (pcase family
                               ('project-proposals
                                (string-join
                                 (delq nil
                                       (list (plist-get item :title)
                                             (plist-get
                                              (plist-get item :first-item)
                                              :text)))
                                 " "))
                               ('reference-notes
                                (or
                                 (delib-flow--reference-note-title item)
                                 (plist-get item :text)
                                 ""))
                               (_
                                (or (plist-get item :text)
                                    (plist-get item :title)
                                    ""))))

(defun delib-flow--selected-support-query-terms (item family)
  "Return meaningful focused-support terms for ITEM in FAMILY."
  (seq-filter
   (lambda (word)
     (and (>= (length word) 4)
          (not (member word
                       (append delib-flow--tag-suggestion-stopwords
                               '("create" "project" "support" "general"
                                 "note" "waiting" "confirm" "selected"))))))
   (delib-flow--string-words
    (delib-flow--selected-support-query-text item family))))

(defun delib-flow--selected-support-candidate-text (candidate)
  "Return searchable support text for CANDIDATE."
  (string-join
   (delq nil
         (list (plist-get candidate :title)
               (and (plist-get candidate :file)
                    (delib-flow--candidate-note-focus-line candidate))
               (and (plist-get candidate :file)
                    (delib-flow--zk-note-text (plist-get candidate :file)))))
   "\n"))

(delib-flow--define-function
 delib-flow--selected-support-candidate-score (candidate terms)
 "Return focused support score for CANDIDATE against TERMS."
 (let*
     ((title-words
       (delib-flow--string-words (or (plist-get candidate :title) "")))
      (focus-words
       (delib-flow--string-words
        (or
         (and (plist-get candidate :file)
              (delib-flow--candidate-note-focus-line candidate))
         "")))
      (body-words
       (delib-flow--string-words
        (delib-flow--selected-support-candidate-text candidate)))
      (title-overlap
       (length (seq-intersection terms title-words #'string=)))
      (focus-overlap
       (length (seq-intersection terms focus-words #'string=)))
      (body-overlap
       (length (seq-intersection terms body-words #'string=)))
      (broad-score (or (plist-get candidate :score) 0)))
   (+ (* title-overlap 4) (* focus-overlap 3) body-overlap
      (if (> broad-score 0) 1 0))))

(delib-flow--define-function delib-flow--selected-support-reasons
                             (candidate terms score)
                             "Return focused support reasons for CANDIDATE from TERMS and SCORE."
                             (let
                                 ((reasons
                                   (delq nil
                                         (list
                                          (when
                                              (>
                                               (length
                                                (seq-intersection
                                                 terms
                                                 (delib-flow--string-words
                                                  (or
                                                   (plist-get candidate :title)
                                                   ""))
                                                 #'string=))
                                               0)
                                            "title-overlap")
                                          (when
                                              (and
                                               (plist-get candidate :file)
                                               (>
                                                (length
                                                 (seq-intersection
                                                  terms
                                                  (delib-flow--string-words
                                                   (or
                                                    (delib-flow--candidate-note-focus-line
                                                     candidate)
                                                    ""))
                                                  #'string=))
                                                0))
                                            "focus-overlap")
                                          (when
                                              (>
                                               (or (plist-get candidate :score) 0)
                                               0)
                                            "broad-retrieval-overlap")))))
                               (or reasons
                                   (when (> score 0)
                                     '("focused-support-fallback")))))

(defun delib-flow--annotated-selected-support-candidate (candidate terms)
  "Return CANDIDATE annotated for focused support using TERMS."
  (let* ((copy (copy-tree candidate))
         (score (delib-flow--selected-support-candidate-score candidate terms)))
    (plist-put
     (plist-put copy :support-score score)
     :support-reasons
     (delib-flow--selected-support-reasons candidate terms score))))

(defun delib-flow--reference-note-support-candidate-strong-p (reasons score)
  "Return non-nil when note support REASONS and SCORE are strong enough."
  (or (member "title-overlap" reasons)
      (member "focus-overlap" reasons)
      (and (>= score 6)
           (not (equal reasons '("broad-retrieval-overlap"))))))

(defun delib-flow--focused-support-candidate-strong-p (candidate family)
  "Return non-nil when focused support CANDIDATE is strong enough for FAMILY."
  (let ((reasons (plist-get candidate :support-reasons))
        (score (or (plist-get candidate :support-score) 0)))
    (pcase family
      ('reference-notes
       (delib-flow--reference-note-support-candidate-strong-p reasons score))
      (_ (> score 0)))))

(defun delib-flow--selected-support-candidate-pool (package)
  "Return broad support candidates available in PACKAGE."
  (or (plist-get (plist-get package :working-context) :retrieved-candidates)
      (plist-get (plist-get (plist-get package :working-context) :filtered-context)
                 :retained-candidates)
      (delib-flow--take-discovery-candidates
       (delib-flow--scored-discovery-candidates
        package
        (delib-flow--zk-note-files)))))

(defun delib-flow--focused-support-candidates (package family item)
  "Return focused support candidates for FAMILY ITEM in PACKAGE."
  (let* ((terms (delib-flow--selected-support-query-terms item family))
         (annotated
          (mapcar (lambda (candidate)
                    (delib-flow--annotated-selected-support-candidate candidate terms))
                  (delib-flow--selected-support-candidate-pool package)))
         (scored
          (seq-filter (lambda (candidate)
                        (delib-flow--focused-support-candidate-strong-p
                         candidate family))
                      annotated)))
    (seq-take
     (sort scored
           (lambda (left right)
             (> (or (plist-get left :support-score) 0)
                (or (plist-get right :support-score) 0))))
     3)))

(defun delib-flow--focused-support-context (candidates)
  "Return compact support context text from focused CANDIDATES."
  (if candidates
      (mapconcat
       (lambda (candidate)
         (format "- %s"
                 (delib-flow--reference-note-support-line candidate)))
       candidates
       "\n")
    "No focused support material is attached yet."))

(defun delib-flow--find-support-for-selected-family-result (package family)
  "Return focused support result for selected artifact FAMILY in PACKAGE."
  (let* ((item (delib-flow--selected-family-support-item package family))
         (label (delib-flow--selected-support-family-label family)))
    (unless item
      (error "Select one %s before retrieving focused support" label))
    (let ((candidates (delib-flow--focused-support-candidates package family item)))
      (list :family family
            :candidate item
            :support-candidates candidates
            :support-context (delib-flow--focused-support-context candidates)
            :reason (if candidates
                        (format "Retrieved focused support for the selected %s only." label)
                      (format "No focused support matched the selected %s." label))))))

(defun delib-flow--find-support-for-selected-action-result (package)
  "Return focused support result for the selected action in PACKAGE."
  (delib-flow--find-support-for-selected-family-result package 'actions))

(defun delib-flow--find-support-for-selected-waiting-for-result (package)
  "Return focused support result for the selected waiting-for in PACKAGE."
  (delib-flow--find-support-for-selected-family-result package 'waiting-fors))

(defun delib-flow--find-support-for-selected-reference-note-result (package)
  "Return focused support result for the selected note in PACKAGE."
  (delib-flow--find-support-for-selected-family-result package 'reference-notes))

(defun delib-flow--find-support-for-selected-project-result (package)
  "Return focused support result for the selected project in PACKAGE."
  (delib-flow--find-support-for-selected-family-result package 'project-proposals))

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
    (cond
     ((not (delib-flow--cloud-policy-enabled-p policy))
      (error "Cloud routing is disabled for provider %s" provider))
     (t
      (list :route 'cloud
            :target-stage target-stage
            :selected-model model
            :selected-provider provider
            :policy-profile (delib-flow--cloud-policy-profile policy)
            :cloud-switch-pending t
            :sanitization-status 'required
            :reason
            (format "Cloud routing is pending sanitized package preparation for %s."
                    (delib-flow--stage-label target-stage)))))))

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
    (or
     (string-match-p
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

(defun delib-flow--project-state-heading-regexp (state)
  "Return regexp matching the project-state bucket heading for STATE."
  (format "^\\* %s\\(?:[ \t]+:[[:alnum:]_@#%%:]+:\\)?$"
          (regexp-quote
           (delib-flow--project-state-bucket-title state))))

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

(defun delib-flow--email-type-hint (from body)
  "Return a deterministic type hint for email FROM and BODY."
  (let ((sender (downcase (or from "")))
        (text (downcase (or body ""))))
    (cond
     ((or
       (string-match-p "newsletter" sender)
       (string-match-p
        "cohort\\|waitlist\\|unsubscribe\\|join the next cohort"
        text))
      "newsletter or mailing list")
     ((or
       (string-match-p "noreply\\|no-reply\\|do-not-reply" sender)
       (string-match-p
        "auto-generated\\|notification\\|wishlist\\|unsubscribe"
        text))
      "transactional notification")
     ((string-match-p "forwarded message\\|reply-to\\|re:" text)
      "correspondence or discussion")
     (t "email message"))))

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
         (links
          (delete-dups
           (append
            (delib-flow--text-org-file-links content nil)
            (let ((start 0)
                  found)
              (while (string-match
                      "https?://[^][()<>[:space:]\"]+"
                      body start)
                (push (match-string 0 body) found)
                (setq start (match-end 0)))
              (nreverse found)))))
         (type-hint (delib-flow--email-type-hint from body)))
    (list :subject subject
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
            (delq nil (list from reply-to))))
          :ignored-noise
          '("transport headers"
            "authentication headers"
            "HTML body"
            "quoted-printable artifacts"
            "footer boilerplate"))))

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

(defun delib-flow--issue-note-source-classification (hits)
  "Return issue-note classification plist from keyword HITS."
  (list :source-type 'issue-note
        :source-type-reason
        "Detected a quick problem-to-fix capture with repair-oriented wording."
        :source-type-signals
        (delete-dups
         (seq-take
          (mapcar (lambda (hit)
                    (format "keyword: %s" hit))
                  hits)
          4))))

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

(defun delib-flow--question-style-body-text (body-text)
  "Return BODY-TEXT with raw URLs removed for question-shape heuristics."
  (replace-regexp-in-string
   "https?://[^][ \"\n\t)]+"
   ""
   (or body-text "")))

(defun delib-flow--question-style-body-p (body-text body-line-count)
  "Return non-nil when BODY-TEXT reads like a short question body."
  (let ((sanitized (delib-flow--question-style-body-text body-text)))
    (and (string-match-p "\\?" sanitized)
         (<= body-line-count 4))))

(defun delib-flow--maybe-issue-note-source-classification (combined-text)
  "Return issue-note classification plist when COMBINED-TEXT qualifies."
  (when-let ((hits (delib-flow--source-keyword-hits
                    combined-text
                    delib-flow--issue-note-source-keywords)))
    (delib-flow--issue-note-source-classification hits)))

(defun delib-flow--maybe-fleeting-note-source-classification (combined-text body-text body-line-count)
  "Return fleeting-note classification plist when evidence is sufficient."
  (let ((hits (delib-flow--source-keyword-hits
               combined-text
               delib-flow--fleeting-note-source-keywords))
        (signals nil))
    (dolist (hit hits)
      (push (format "keyword: %s" hit) signals))
    (when (delib-flow--question-style-body-p body-text body-line-count)
      (push "short question-style body" signals))
    (when signals
      (delib-flow--fleeting-note-source-classification
       (nreverse (delete-dups signals))))))

(defun delib-flow--source-type-classification (source &optional operator-intent)
  "Return deterministic source-type classification plist for SOURCE."
  (let* ((title (plist-get source :title))
         (content (or (plist-get source :content) ""))
         (body-text (delib-flow--source-body-text content))
         (outline-path (plist-get source :outline-path))
         (combined-text
          (string-join
           (delq nil
                 (list title body-text
                       operator-intent
                       (and outline-path
                            (mapconcat #'identity outline-path " "))))
           "\n"))
         (emails (delib-flow--text-emails content))
         (header-count (delib-flow--source-metadata-header-count body-text))
         (meeting-keywords
          (delete-dups
           (delib-flow--source-meeting-keywords combined-text)))
         (meeting-section-count
          (delib-flow--source-meeting-section-count body-text))
         (journal-path-p
          (delib-flow--journal-outline-path-p outline-path))
         (body-line-count
          (delib-flow--count-body-lines content)))
    (or
     (delib-flow--maybe-email-source-classification header-count emails)
     (delib-flow--maybe-meeting-source-classification
      journal-path-p meeting-section-count meeting-keywords)
     (delib-flow--maybe-reminder-source-classification combined-text)
     (delib-flow--maybe-issue-note-source-classification combined-text)
     (delib-flow--maybe-fleeting-note-source-classification
      combined-text body-text body-line-count)
     (delib-flow--unknown-source-classification))))

(defun delib-flow--inspect-source-analysis (package)
  "Return structured inspect analysis for PACKAGE."
  (let* ((source (or (plist-get package :source)
                     package))
         (operator-intent
          (if (plist-get package :source)
              (delib-flow--operator-intent-text-from-package package)
            ""))
         (content (or (plist-get source :content) ""))
         (body-text (delib-flow--source-body-text content))
         (file (plist-get source :file))
         (base-dir (and file
                        (file-name-directory file)))
         (outline-path (plist-get source :outline-path))
         (base-classification
          (delib-flow--source-type-classification source))
         (classification
          (delib-flow--source-type-classification source operator-intent))
         (digest
          (and (delib-flow--email-source-shape-p source)
               (delib-flow--email-inspect-digest source)))
         (emails
          (or (plist-get digest :contact-emails)
              (delib-flow--meaningful-contact-emails
               (delib-flow--text-emails content))))
         (org-file-links
          (if base-dir
              (delib-flow--text-org-file-links content base-dir)
            nil)))
    (append
     (list :title (plist-get source :title)
           :outline-path outline-path
           :body-line-count
           (delib-flow--count-body-lines content)
           :content-word-count
           (delib-flow--count-text-words body-text)
           :has-id
           (not (null (plist-get source :id)))
           :contact-emails emails
           :contact-email-count (length emails)
           :org-file-links org-file-links
           :org-file-link-count (length org-file-links)
           :body-preview
           (string-trim
            (truncate-string-to-width body-text 160 nil nil t))
           :operator-intent operator-intent
           :operator-intent-present
           (delib-flow--non-empty-string-p operator-intent)
           :operator-intent-influenced
           (not (eq (plist-get base-classification :source-type)
                    (plist-get classification :source-type))))
     classification)))

(defun delib-flow--execute-inspect-source (package)
  "Return raw inspect-source output for PACKAGE."
  (let ((analysis (delib-flow--inspect-source-analysis package)))
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

(provide 'delib-flow-services)

;;; delib-flow-services.el ends here
