;;; delib-flow-test-support.el --- Shared test support for delib-flow -*- lexical-binding: t; -*-

(require 'org)
(require 'delib-flow)

(defmacro delib-flow-test--with-temp-org (&rest body)
  "Run BODY in a temporary Org buffer."
  `(with-temp-buffer
     (org-mode)
     ,@body))

(defmacro delib-flow-test--with-temp-org-file (content &rest body)
  "Run BODY with a temporary Org file containing CONTENT."
  (declare (indent 1))
  `(let ((file (make-temp-file "delib-flow-org" nil ".org" ,content)))
     (unwind-protect
         (with-current-buffer (find-file-noselect file)
           (org-mode)
           ,@body)
       (when-let ((buffer (get-file-buffer file)))
         (kill-buffer buffer))
       (when (file-exists-p file)
         (delete-file file)))))

(defmacro delib-flow-test--with-temp-project-file (content &rest body)
  "Run BODY with a temporary My Projects file containing CONTENT."
  (declare (indent 1))
  `(let ((file (make-temp-file "delib-flow-projects" nil ".org" ,content)))
     (unwind-protect
         (let ((delib-flow-my-projects-file file))
           ,@body)
       (when-let ((buffer (get-file-buffer file)))
         (kill-buffer buffer))
       (when (file-exists-p file)
         (delete-file file)))))

(defmacro delib-flow-test--with-temp-zk-root (files &rest body)
  "Run BODY with a temporary ZK root populated from FILES."
  (declare (indent 1))
  `(let ((root (make-temp-file "delib-flow-zk" t)))
     (unwind-protect
         (progn
           (dolist (entry ,files)
             (let* ((relative (car entry))
                    (content (cdr entry))
                    (target (expand-file-name relative root))
                    (dir (file-name-directory target)))
               (make-directory dir t)
               (with-temp-file target
                 (insert content))))
           (let ((delib-flow-zk-root root))
             ,@body))
       (dolist (entry ,files)
         (let ((buffer (get-file-buffer
                        (expand-file-name (car entry) root))))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))
       (when (file-directory-p root)
         (delete-directory root t)))))

(defmacro delib-flow-test--with-temp-audit-file (&rest body)
  "Run BODY with a temporary audit log file."
  (declare (indent 0))
  `(let ((file (make-temp-file "delib-flow-audit" nil ".org")))
     (unwind-protect
         (let ((delib-flow-audit-log-file file))
           ,@body)
       (when (file-exists-p file)
         (delete-file file)))))

(defmacro delib-flow-test--with-temp-directory-var (var prefix &rest body)
  "Bind VAR to a temporary directory during BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file ,prefix t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,var)
         (delete-directory ,var t)))))

(defmacro delib-flow-test--with-temp-file-var (var prefix suffix content &rest body)
  "Bind VAR to a temporary file with CONTENT during BODY."
  (declare (indent 4))
  `(let ((,var (make-temp-file ,prefix nil ,suffix ,content)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(defun delib-flow-test--accept-inspect (run)
  "Return RUN with inspect-source accepted and actions reseeded."
  (delib-flow--seed-actions
   (delib-flow--apply-inspect-review-outcome
    run
    'accepted
    "Inspect result accepted. You may now match the project or retry inspect.")))

(defun delib-flow-test--accept-match (run)
  "Return RUN with match-project accepted and actions reseeded."
  (delib-flow--seed-actions
   (delib-flow--apply-match-review-outcome
    run
    'accepted
    "Project match accepted. Continue with manual override or downstream stages as appropriate.")))

(defun delib-flow-test--set-manual-project-selection (run selection &optional notes)
  "Return RUN with manual project-selection block set to SELECTION and NOTES."
  (let* ((block (delib-flow--editable-block run 'manual-project-selection))
         (text (format "Selection: %s\nNotes:\n%s\n"
                       selection
                       (or notes ""))))
    (delib-flow--set-editable-block
     run
     'manual-project-selection
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow-test--set-filing-selection (run selection &optional notes)
  "Return RUN with filing-selection block set to SELECTION and NOTES."
  (let* ((block (delib-flow--editable-block run 'filing-selection-review))
         (text (format "Selection: %s\nNotes:\n%s\n"
                       selection
                       (or notes ""))))
    (delib-flow--set-editable-block
     run
     'filing-selection-review
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow-test--set-operator-intent (run text)
  "Return RUN with operator intent block set to TEXT."
  (let ((block (delib-flow--editable-block run 'context-main)))
    (delib-flow--set-editable-block
     run
     'context-main
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow-test--draft-selected-filing-item (run)
  "Return RUN after drafting its currently selected filing item."
  (if-let ((item (delib-flow--selected-filing-item run)))
      (pcase (plist-get item :kind)
        ('next-action
         (delib-flow--run-stage-locally run 'draft-selected-action))
        ('waiting-for
         (delib-flow--run-stage-locally run 'draft-selected-waiting-for))
        ('reference-note
         (delib-flow--run-stage-locally run 'draft-selected-reference-note))
        ('project
         (delib-flow--run-stage-locally run 'draft-selected-project))
        (_ run))
    run))

(defun delib-flow-test--approve-selected-filing-item (run)
  "Return RUN after drafting and approving its selected filing item."
  (delib-flow--run-stage-locally
   (delib-flow-test--draft-selected-filing-item run)
   'select-approved-filing-actions))

(defun delib-flow-test--set-filing-conflict-resolution
    (run resolution &optional notes new-title new-text)
  "Return RUN with conflict-resolution block set to RESOLUTION and optional values."
  (let* ((block (delib-flow--editable-block run 'filing-conflict-resolution))
         (text (format "Resolution: %s\nNotes:\n%s\n\nNew title: %s\n\nNew text: %s\n"
                       resolution
                       (or notes "")
                       (or new-title "")
                       (or new-text ""))))
    (delib-flow--set-editable-block
     run
     'filing-conflict-resolution
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow-test--set-cloud-failure-resolution (run resolution &optional notes)
  "Return RUN with cloud-failure review block set to RESOLUTION and NOTES."
  (let* ((block (delib-flow--editable-block run 'cloud-failure-review))
         (text (format "Resolution: %s\nNotes:\n%s\n"
                       resolution
                       (or notes ""))))
    (delib-flow--set-editable-block
     run
     'cloud-failure-review
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow-test--set-cloud-target-stage (run stage-id &optional notes)
  "Return RUN with cloud-routing review block set to STAGE-ID and NOTES."
  (let* ((block (delib-flow--editable-block run 'cloud-routing-review))
         (text (format "Target stage: %s\nNotes:\n%s\n"
                       stage-id
                       (or notes ""))))
    (delib-flow--set-editable-block
     run
     'cloud-routing-review
     (delib-flow--set-editable-block-text block text))))

(defun delib-flow-test--file-buffer-string (file)
  "Return current buffer text for FILE."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(defun delib-flow-test--reference-note-part-editor-run ()
  "Return a filing-ready run for focused reference-note part editor tests."
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (drafted-item
          (plist-put
           (copy-tree candidate)
           :draft-body
           (string-join
            '("* Working draft"
              "Atlas draft body."
              "- Durable claim: Keep this claim."
              "- Why it matters: Keep this reason."
              "- Reuse angle: Keep this reuse angle."
              ""
              "* Source highlights"
              "- Keep these source highlights."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n"))))
    (delib-flow--seed-actions
     (delib-flow--seed-filing-selection-block
      (delib-flow--set-artifact-family-state
       (plist-put
        (delib-flow--initialize-run
         (list :title "Source"
               :content "* Source\nBody line\n"))
        :filing
        (list :draft-items (list candidate)
              :approved-items nil
              :rejected-items nil
              :preview-text nil
              :selection-blocked-item nil
              :selection-blocking-warnings nil
              :selection-blocked-selection nil
              :selection-blocked-notes nil
              :conflicts nil
              :target-locations nil))
       'reference-notes
       (list :candidates (list candidate)
             :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
             :selected-draft drafted-item))))))

(defun delib-flow-test--reference-note-candidate-only-run ()
  "Return a filing-ready run with a selected reference-note candidate only."
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (run
          (delib-flow--set-artifact-family-state
           (delib-flow--initialize-run
            (list :title "Atlas source"
                  :content
                  (string-join
                   '("* Atlas source"
                     "Project Atlas Pattern turns setup notes into reusable execution scaffolds."
                     "Use this when a recurring project setup pattern keeps reappearing.")
                   "\n")))
           'reference-notes
           (list :candidates (list candidate)
                 :selected-candidate-id (delib-flow--artifact-candidate-id candidate)))))
    (delib-flow--seed-actions
     (delib-flow--seed-filing-selection-block run))))

(provide 'delib-flow-test-support)

;;; delib-flow-test-support.el ends here
