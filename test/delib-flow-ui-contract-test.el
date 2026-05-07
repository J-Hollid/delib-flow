;;; delib-flow-ui-contract-test.el --- UI scenario contract tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-debug-test)

(defconst delib-flow-ui-contract-test--file
  (expand-file-name "../docs/ui-scenario-contracts.org"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the executable UI scenario contract document.")

(defconst delib-flow-ui-contract-test--allowed-surfaces
  '(cockpit filing-workspace))

(defconst delib-flow-ui-contract-test--allowed-zones
  '(recommended-next-pass quick-actions next-actions family-local-actions
    filing-actions local-actions))

(defconst delib-flow-ui-contract-test--surface-command-map
  '((open-filing-workspace . delib-flow-open-filing-workspace)
    (return-main-cockpit . delib-flow-return-main-cockpit)))

(defconst delib-flow-ui-contract-test--stable-command-id-map
  '((refresh-buffer . delib-flow-refresh-buffer)
    (abort-run . delib-flow-abort-run)))

(defun delib-flow-ui-contract-test--parse-bool (value)
  "Return boolean parsed from VALUE."
  (pcase (downcase (string-trim (or value "")))
    ("true" t)
    ("false" nil)
    (_ (error "Expected true/false value, got %S" value))))

(defun delib-flow-ui-contract-test--parse-required (value)
  "Return boolean parsed from yes/no VALUE."
  (pcase (downcase (string-trim (or value "")))
    ("yes" t)
    ("no" nil)
    (_ (error "Expected yes/no value, got %S" value))))

(defun delib-flow-ui-contract-test--parse-symbol-cell (value)
  "Return symbol parsed from VALUE, or nil when empty."
  (let ((trimmed (string-trim (or value ""))))
    (unless (string-empty-p trimmed)
      (intern trimmed))))

(defun delib-flow-ui-contract-test--parse-int-cell (value)
  "Return integer parsed from VALUE."
  (string-to-number (string-trim value)))

(defun delib-flow-ui-contract-test--known-action-id-p (action-id)
  "Return non-nil when ACTION-ID is a known action or surface command."
  (or (assq action-id delib-flow-ui-contract-test--surface-command-map)
      (assq action-id delib-flow-ui-contract-test--stable-command-id-map)
      (fboundp (intern (format "delib-flow-action-%s" action-id)))
      (fboundp (intern (format "delib-flow-%s" action-id)))))

(defun delib-flow-ui-contract-test--table-rows (heading columns end)
  "Return parsed rows for HEADING expecting COLUMNS before END."
  (save-excursion
    (unless (re-search-forward
             (format "^\\*\\* %s$" (regexp-quote heading))
             end t)
      (error "Missing %s table" heading))
    (forward-line 1)
    (while (and (< (point) end)
                (not (looking-at-p "^|")))
      (forward-line 1))
    (unless (looking-at-p "^|")
      (error "Missing Org table under %s" heading))
    (let* ((table (org-table-to-lisp))
           (header (car table))
           (data (cl-remove-if (lambda (row) (eq row 'hline)) (cdr table))))
      (unless (equal header columns)
        (error "Unexpected columns for %s: %S" heading header))
      data)))

(defun delib-flow-ui-contract-test--parse-contract ()
  "Return parsed contract at point."
  (let* ((scenario-id (delib-flow-ui-contract-test--parse-symbol-cell
                       (org-entry-get (point) "SCENARIO_ID")))
         (checkpoint (delib-flow-ui-contract-test--parse-symbol-cell
                      (org-entry-get (point) "CHECKPOINT")))
         (surface (delib-flow-ui-contract-test--parse-symbol-cell
                   (org-entry-get (point) "SURFACE")))
         (title (or (org-entry-get (point) "TITLE")
                    (org-get-heading t t t t)))
         (use-case (org-entry-get (point) "USE_CASE"))
         (end (save-excursion (org-end-of-subtree t t)))
         (sections
          (mapcar
           (lambda (row)
             (pcase-let ((`(,order ,heading ,required) row))
               (list :order (delib-flow-ui-contract-test--parse-int-cell order)
                     :heading heading
                     :required
                     (delib-flow-ui-contract-test--parse-required required))))
           (delib-flow-ui-contract-test--table-rows
            "Sections"
            '("order" "heading" "required")
            end)))
         (actions
          (mapcar
           (lambda (row)
             (pcase-let ((`(,zone ,label ,action-id ,required) row))
               (list :zone (delib-flow-ui-contract-test--parse-symbol-cell zone)
                     :label label
                     :action-id
                     (delib-flow-ui-contract-test--parse-symbol-cell action-id)
                     :required
                     (delib-flow-ui-contract-test--parse-required required))))
           (delib-flow-ui-contract-test--table-rows
            "Actions"
            '("zone" "label" "action_id" "required")
            end)))
         (transitions
          (mapcar
           (lambda (row)
             (pcase-let ((`(,action-id ,destination-surface ,destination-anchor
                                       ,latest-stage ,selected-family
                                       ,workspace-open)
                          row))
               (list :action-id
                     (delib-flow-ui-contract-test--parse-symbol-cell action-id)
                     :destination-surface
                     (delib-flow-ui-contract-test--parse-symbol-cell
                      destination-surface)
                     :destination-anchor
                     (let ((value (string-trim destination-anchor)))
                       (unless (string-empty-p value) value))
                     :latest-stage
                     (delib-flow-ui-contract-test--parse-symbol-cell latest-stage)
                     :selected-family
                     (delib-flow-ui-contract-test--parse-symbol-cell
                      selected-family)
                     :workspace-open
                     (unless (string-empty-p (string-trim workspace-open))
                       (delib-flow-ui-contract-test--parse-bool
                        workspace-open)))))
           (delib-flow-ui-contract-test--table-rows
            "Transitions"
            '("action_id" "destination_surface" "destination_anchor"
              "latest_stage" "selected_family" "workspace_open")
            end))))
    (list :scenario-id scenario-id
          :checkpoint checkpoint
          :surface surface
          :title title
          :use-case use-case
          :sections sections
          :actions actions
          :transitions transitions)))

(defun delib-flow-ui-contract-test--load-contracts (&optional file)
  "Return validated contracts loaded from FILE."
  (with-temp-buffer
    (insert-file-contents (or file delib-flow-ui-contract-test--file))
    (org-mode)
    (goto-char (point-min))
    (let (contracts seen)
      (while (re-search-forward "^\\* " nil t)
        (beginning-of-line)
        (let* ((contract (delib-flow-ui-contract-test--parse-contract))
               (scenario-id (plist-get contract :scenario-id))
               (checkpoint (plist-get contract :checkpoint))
               (surface (plist-get contract :surface))
               (key (list scenario-id checkpoint surface)))
          (unless scenario-id
            (error "Contract %S is missing SCENARIO_ID"
                   (plist-get contract :title)))
          (unless checkpoint
            (error "Contract %S is missing CHECKPOINT"
                   (plist-get contract :title)))
          (unless (memq surface delib-flow-ui-contract-test--allowed-surfaces)
            (error "Contract %S has unknown surface %S"
                   (plist-get contract :title) surface))
          (unless (member scenario-id (delib-flow--debug-scenario-ids))
            (error "Contract %S uses unknown debug scenario %S"
                   (plist-get contract :title) scenario-id))
          (unless (memq checkpoint
                        (delib-flow--debug-supported-checkpoints scenario-id))
            (error "Contract %S uses unsupported checkpoint %S for %S"
                   (plist-get contract :title) checkpoint scenario-id))
          (when (member key seen)
            (error "Duplicate contract key %S" key))
          (push key seen)
          (dolist (row (plist-get contract :actions))
            (let ((zone (plist-get row :zone))
                  (action-id (plist-get row :action-id)))
              (unless (memq zone delib-flow-ui-contract-test--allowed-zones)
                (error "Contract %S uses unknown zone %S"
                       (plist-get contract :title) zone))
              (unless (and action-id
                           (delib-flow-ui-contract-test--known-action-id-p
                            action-id))
                (error "Contract %S uses unknown action id %S"
                       (plist-get contract :title) action-id))))
          (dolist (row (plist-get contract :transitions))
            (let ((action-id (plist-get row :action-id))
                  (destination-surface
                   (plist-get row :destination-surface))
                  (selected-family (plist-get row :selected-family)))
              (unless (and action-id
                           (delib-flow-ui-contract-test--known-action-id-p
                            action-id))
                (error "Contract %S uses unknown transition action id %S"
                       (plist-get contract :title) action-id))
              (when (and destination-surface
                         (not (memq destination-surface
                                    delib-flow-ui-contract-test--allowed-surfaces)))
                (error "Contract %S uses unknown destination surface %S"
                       (plist-get contract :title) destination-surface))
              (when (and selected-family
                         (not (memq selected-family
                                    '(actions waiting-fors reference-notes
                                      project-proposals))))
                (error "Contract %S uses unknown selected family %S"
                       (plist-get contract :title) selected-family))))
          (push contract contracts))
        (org-end-of-subtree t t))
      (nreverse contracts))))

(defun delib-flow-ui-contract-test--render-surface (surface)
  "Render SURFACE from the current active run and return its buffer."
  (pcase surface
    ('cockpit
     (delib-flow--render-control-buffer delib-flow--active-run))
    ('filing-workspace
     (delib-flow-open-filing-workspace)
     (get-buffer delib-flow-filing-workspace-buffer-name))
    (_
     (error "Unsupported surface %S" surface))))

(defun delib-flow-ui-contract-test--buffer-for-surface (surface)
  "Return live buffer for SURFACE."
  (pcase surface
    ('cockpit (get-buffer delib-flow-control-buffer-name))
    ('filing-workspace (get-buffer delib-flow-filing-workspace-buffer-name))
    (_ nil)))

(defun delib-flow-ui-contract-test--surface-anchor (surface)
  "Return the active anchor heading for SURFACE in the current buffer."
  (pcase surface
    ('cockpit
     (or (delib-flow--current-section-at-point)
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward "^\\*\\* \\(.+\\)$" nil t)
             (match-string 1)))))
    ('filing-workspace
     (or (delib-flow--current-heading-at-point)
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward "^\\*\\* \\(.+\\)$" nil t)
             (match-string 1)))))
    (_ nil)))

(defun delib-flow-ui-contract-test--action-zone-for-heading (surface heading)
  "Return normalized zone for HEADING on SURFACE."
  (pcase surface
    ('cockpit
     (cond
      ((equal heading "Recommended next pass") 'recommended-next-pass)
      ((equal heading "Quick actions") 'quick-actions)
      ((equal heading "Next actions") 'next-actions)
      ((string-prefix-p "Local action palette:" heading) 'family-local-actions)
      ((equal heading "Filing actions") 'filing-actions)))
    ('filing-workspace
     (when (string-prefix-p "Do here:" heading)
       'local-actions))))

(defun delib-flow-ui-contract-test--collect-rendered-actions (surface)
  "Return rendered action rows for SURFACE in current buffer."
  (let ((pos (point-min))
        rows)
    (while (< pos (point-max))
      (let ((action (get-text-property pos 'delib-flow-action)))
        (if action
            (let* ((heading (save-excursion
                              (goto-char pos)
                              (org-back-to-heading t)
                              (org-get-heading t t t t)))
                   (zone (delib-flow-ui-contract-test--action-zone-for-heading
                          surface heading)))
              (push (list :zone zone
                          :heading heading
                          :action-id (plist-get action :id)
                          :label (plist-get action :label)
                          :position pos
                          :handler (plist-get action :handler))
                    rows)
              (setq pos (or (next-single-property-change
                             pos 'delib-flow-action nil (point-max))
                            (point-max))))
          (setq pos (1+ pos)))))
    (nreverse rows)))

(defun delib-flow-ui-contract-test--find-rendered-action (surface action-id)
  "Return rendered action row for ACTION-ID on SURFACE in current buffer."
  (seq-find (lambda (row)
              (eq (plist-get row :action-id) action-id))
            (delib-flow-ui-contract-test--collect-rendered-actions surface)))

(defun delib-flow-ui-contract-test--invoke-contract-action (surface action-id)
  "Invoke ACTION-ID on SURFACE from the current UI state."
  (let ((buffer (delib-flow-ui-contract-test--buffer-for-surface surface)))
    (unless (buffer-live-p buffer)
      (error "No live buffer for surface %S" surface))
    (with-current-buffer buffer
      (if-let ((row (delib-flow-ui-contract-test--find-rendered-action
                     surface action-id)))
          (progn
            (goto-char (plist-get row :position))
            (funcall (plist-get row :handler)))
        (if-let ((command (cdr (assq action-id
                                     delib-flow-ui-contract-test--surface-command-map))))
            (funcall command)
          (error "No rendered action or surface command found for %S"
                 action-id))))))

(defun delib-flow-ui-contract-test--current-selected-family ()
  "Return selected filing family from the active run, if any."
  (cond
   ((plist-get (plist-get (plist-get delib-flow--active-run :artifacts)
                          'actions)
               :selected-candidate-id)
    'actions)
   ((plist-get (plist-get (plist-get delib-flow--active-run :artifacts)
                          'waiting-fors)
               :selected-candidate-id)
    'waiting-fors)
   ((plist-get (plist-get (plist-get delib-flow--active-run :artifacts)
                          'reference-notes)
               :selected-candidate-id)
    'reference-notes)
   ((plist-get (plist-get (plist-get delib-flow--active-run :artifacts)
                          'project-proposals)
               :selected-candidate-id)
    'project-proposals)))

(defun delib-flow-ui-contract-test--assert-sections (contract)
  "Assert required sections from CONTRACT in the current buffer."
  (let (positions)
    (dolist (row (plist-get contract :sections))
      (when (plist-get row :required)
        (goto-char (point-min))
        (let ((pattern
               (format "^\\*\\* %s$"
                       (regexp-quote (plist-get row :heading)))))
          (unless (re-search-forward pattern nil t)
            (ert-fail (format "Missing required section %S in %S"
                              (plist-get row :heading)
                              (plist-get contract :title))))
          (push (match-beginning 0) positions))))
    (should (apply #'< (nreverse positions)))))

(defun delib-flow-ui-contract-test--assert-actions (contract surface)
  "Assert required rendered actions from CONTRACT on SURFACE."
  (let ((rows (delib-flow-ui-contract-test--collect-rendered-actions surface)))
    (dolist (expected (plist-get contract :actions))
      (when (plist-get expected :required)
        (unless
            (seq-find
             (lambda (row)
               (and (eq (plist-get row :zone)
                        (plist-get expected :zone))
                    (eq (plist-get row :action-id)
                        (plist-get expected :action-id))
                    (equal (plist-get row :label)
                           (plist-get expected :label))))
             rows)
          (ert-fail
           (format "Missing required action %S/%S/%S in %S"
                   (plist-get expected :zone)
                   (plist-get expected :action-id)
                   (plist-get expected :label)
                   (plist-get contract :title))))))))

(defun delib-flow-ui-contract-test--assert-transitions (contract surface)
  "Assert transition rows from CONTRACT on SURFACE."
  (dolist (transition (plist-get contract :transitions))
    (delib-flow-ui-contract-test--invoke-contract-action
     surface
     (plist-get transition :action-id))
    (let ((destination-surface
           (plist-get transition :destination-surface))
          (latest-stage
           (plist-get transition :latest-stage))
          (selected-family
           (plist-get transition :selected-family)))
      (when destination-surface
        (let ((buffer
               (delib-flow-ui-contract-test--buffer-for-surface
                destination-surface)))
          (unless (buffer-live-p buffer)
            (ert-fail
             (format "Destination buffer for %S is not live in %S"
                     destination-surface
                     (plist-get contract :title))))
          (when-let ((anchor (plist-get transition :destination-anchor)))
            (with-current-buffer buffer
              (should (equal anchor
                             (delib-flow-ui-contract-test--surface-anchor
                              destination-surface)))))))
      (when latest-stage
        (should (eq latest-stage
                    (plist-get (delib-flow--latest-stage-entry
                                delib-flow--active-run)
                               :stage-id))))
      (when (plist-member transition :workspace-open)
        (should (eq (plist-get transition :workspace-open)
                    (delib-flow--filing-workspace-open-p
                     delib-flow--active-run))))
      (when selected-family
        (should (eq selected-family
                    (delib-flow-ui-contract-test--current-selected-family)))))))

(ert-deftest delib-flow-ui-contract-doc-loads-and-validates ()
  (let ((contracts (delib-flow-ui-contract-test--load-contracts)))
    (should (= 7 (length contracts)))))

(ert-deftest delib-flow-ui-contract-loader-errors-for_duplicate-key ()
  (let ((file (make-temp-file "delib-flow-ui-contract" nil ".org"
                              "* One\n:PROPERTIES:\n:SCENARIO_ID: alpha-followup\n:CHECKPOINT: source\n:SURFACE: cockpit\n:END:\n** Sections\n| order | heading | required |\n|-------+---------+----------|\n| 1 | Now | yes |\n** Actions\n| zone | label | action_id | required |\n|------+-------+-----------+----------|\n| next-actions | Abort Run | abort-run | yes |\n** Transitions\n| action_id | destination_surface | destination_anchor | latest_stage | selected_family | workspace_open |\n|-----------+---------------------+--------------------+--------------+-----------------+----------------|\n* Two\n:PROPERTIES:\n:SCENARIO_ID: alpha-followup\n:CHECKPOINT: source\n:SURFACE: cockpit\n:END:\n** Sections\n| order | heading | required |\n|-------+---------+----------|\n| 1 | Now | yes |\n** Actions\n| zone | label | action_id | required |\n|------+-------+-----------+----------|\n| next-actions | Abort Run | abort-run | yes |\n** Transitions\n| action_id | destination_surface | destination_anchor | latest_stage | selected_family | workspace_open |\n|-----------+---------------------+--------------------+--------------+-----------------+----------------|\n")))
    (unwind-protect
        (should-error (delib-flow-ui-contract-test--load-contracts file))
      (delete-file file))))

(ert-deftest delib-flow-ui-contract-loader-errors-for_unknown-zone ()
  (let ((file (make-temp-file "delib-flow-ui-contract" nil ".org"
                              "* One\n:PROPERTIES:\n:SCENARIO_ID: alpha-followup\n:CHECKPOINT: source\n:SURFACE: cockpit\n:END:\n** Sections\n| order | heading | required |\n|-------+---------+----------|\n| 1 | Now | yes |\n** Actions\n| zone | label | action_id | required |\n|------+-------+-----------+----------|\n| bad-zone | Abort Run | abort-run | yes |\n** Transitions\n| action_id | destination_surface | destination_anchor | latest_stage | selected_family | workspace_open |\n|-----------+---------------------+--------------------+--------------+-----------------+----------------|\n")))
    (unwind-protect
        (should-error (delib-flow-ui-contract-test--load-contracts file))
      (delete-file file))))

(ert-deftest delib-flow-ui-contract-loader-errors-for_unknown-action-id ()
  (let ((file (make-temp-file "delib-flow-ui-contract" nil ".org"
                              "* One\n:PROPERTIES:\n:SCENARIO_ID: alpha-followup\n:CHECKPOINT: source\n:SURFACE: cockpit\n:END:\n** Sections\n| order | heading | required |\n|-------+---------+----------|\n| 1 | Now | yes |\n** Actions\n| zone | label | action_id | required |\n|------+-------+-----------+----------|\n| next-actions | Bad | missing-action | yes |\n** Transitions\n| action_id | destination_surface | destination_anchor | latest_stage | selected_family | workspace_open |\n|-----------+---------------------+--------------------+--------------+-----------------+----------------|\n")))
    (unwind-protect
        (should-error (delib-flow-ui-contract-test--load-contracts file))
      (delete-file file))))

(ert-deftest delib-flow-ui-contract-scenarios-match-rendered-surfaces ()
  (dolist (contract (delib-flow-ui-contract-test--load-contracts))
    (delib-flow-debug-test--with-local-test-config
      (let ((scenario-id (plist-get contract :scenario-id))
            (checkpoint (plist-get contract :checkpoint))
            (surface (plist-get contract :surface)))
        (unwind-protect
            (progn
              (delib-flow-debug-start-scenario scenario-id checkpoint)
              (let ((buffer (delib-flow-ui-contract-test--render-surface surface)))
                (with-current-buffer buffer
                  (delib-flow-ui-contract-test--assert-sections contract)
                  (delib-flow-ui-contract-test--assert-actions contract surface)
                  (delib-flow-ui-contract-test--assert-transitions contract
                                                                   surface))))
          (when delib-flow--active-run
            (delib-flow-abort-run))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name)))
          (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
            (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))))

(provide 'delib-flow-ui-contract-test)
;;; delib-flow-ui-contract-test.el ends here
