;;; delib-flow-ui-test.el --- UI tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-test-support)

(ert-deftest delib-flow-ui-surface-render-descriptor-errors-for-unknown-surface ()
  (should-error (delib-flow--surface-render-descriptor 'missing-surface)
                :type 'error))

(ert-deftest delib-flow-ui-surface-render-descriptor-errors-for-missing-required-key ()
  (let ((delib-flow--surface-render-descriptor-alist
         '((broken-surface
            :buffer-renderer delib-flow--render-control-buffer))))
    (should-error (delib-flow--surface-render-descriptor 'broken-surface)
                  :type 'error)))

(ert-deftest delib-flow-ui-stage-command-source-run-errors-for-unknown-sync-policy ()
  (let ((delib-flow--active-run (delib-flow--initialize-run (list :title "Example")))
        (delib-flow--stage-descriptor-alist
         '((broken-stage
            :id broken-stage
            :label "Broken"
            :command-runner-kind run-stage-locally
            :command-sync invalid-sync
            :command-rerender current-result))))
    (should-error (delib-flow--stage-command-source-run 'broken-stage)
                  :type 'error)))

(ert-deftest delib-flow-ui-rerender-after-stage-command-errors-for-unknown-policy ()
  (let ((delib-flow--stage-descriptor-alist
         '((broken-stage
            :id broken-stage
            :label "Broken"
            :command-runner-kind run-stage-locally
            :command-sync control-buffer
            :command-rerender invalid-rerender))))
    (should-error (delib-flow--rerender-after-stage-command 'broken-stage)
                  :type 'error)))

(ert-deftest delib-flow-ui-stage-command-runner-errors-for-unregistered-kind ()
  (let ((delib-flow--stage-descriptor-alist
         '((broken-stage
            :id broken-stage
            :label "Broken"
            :command-runner-kind missing-runner
            :command-sync control-buffer
            :command-rerender current-result))))
    (should-error (delib-flow--stage-command-runner 'broken-stage)
                  :type 'error)))

(ert-deftest delib-flow-ui-toggle-focus-mode-preserves-active-loop-visibility ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (setq delib-flow--active-run run)
          (should-not delib-flow-control-focus-mode)
          (delib-flow-toggle-focus-mode)
          (should delib-flow-control-focus-mode)
          (should (string-match-p "\\[focus\\]" (delib-flow--control-header-line)))
          (goto-char (point-min))
          (should (search-forward "** Now" nil t))
          (goto-char (point-min))
          (should (search-forward "** Next actions" nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-ui-toggle-focus-mode-errors-outside-control-buffer-and-toggles-off ()
  (should-error (delib-flow-toggle-focus-mode) :type 'user-error)
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq delib-flow--active-run run)
            (delib-flow-toggle-focus-mode)
            (should delib-flow-control-focus-mode)
            (delib-flow-toggle-focus-mode)
            (should-not delib-flow-control-focus-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (setq delib-flow--active-run nil))))

(ert-deftest delib-flow-ui-render-active-run-buffer-applies-cockpit-visibility ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (buffer (delib-flow--render-active-run-buffer run "Now")))
    (unwind-protect
        (with-current-buffer buffer
          (should (looking-at-p "\\*\\* Now"))
          (goto-char (point-min))
          (search-forward "*** Source snapshot")
          (should (outline-invisible-p (point))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-ui-control-buffer-killed-clears-active-run ()
  (let ((delib-flow--active-run (list :session (list :status 'active))))
    (with-temp-buffer
      (setq-local delib-flow--active-run-buffer t)
      (delib-flow--control-buffer-killed))
    (should (null delib-flow--active-run))))

(ert-deftest delib-flow-ui-refresh-buffer-errors-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-refresh-buffer))))

(ert-deftest delib-flow-ui-refresh-buffer-rerenders-active-run ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run
          (delib-flow-test--set-operator-intent run "Retained edit"))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-refresh-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (goto-char (point-min))
            (should (search-forward "- Refresh Buffer [available]" nil t))
            (goto-char (point-min))
            (should (search-forward "Retained edit" nil t)))
          (should (equal "Retained edit"
                         (plist-get
                          (delib-flow--editable-block delib-flow--active-run
                                                      'context-main)
                          :current-text))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-refresh-buffer-preserves-current-section-anchor ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Filing preview")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-refresh-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Filing preview"
                           (delib-flow--current-section-at-point)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-refresh-buffer-preserves-current-subheading-anchor ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (drafted
            (delib-flow--run-stage-locally
             (delib-flow--run-stage-locally
              (delib-flow--run-stage-locally run 'inspect-source)
              'match-project)
             'extract-actions))
           (delib-flow--active-run drafted)
           (buffer (delib-flow--render-control-buffer drafted)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t)
              (goto-char (point-min))
              (search-forward "*** Artifact selection" nil t)
              (org-back-to-heading t))
            (delib-flow-refresh-buffer)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (org-back-to-heading t)
              (should (equal "Artifact selection"
                             (org-get-heading t t t t)))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-ui-render-active-run-buffer-aligns-anchor-heading-to-top ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (buffer (delib-flow--render-active-run-buffer run "Current result")))
    (unwind-protect
        (let ((window (display-buffer buffer)))
          (with-current-buffer buffer
            (goto-char (point-min))
            (delib-flow--render-active-run-buffer run "Current result")
            (should (delib-flow--goto-section "Current result"))
            (let ((expected-start
                   (save-excursion
                     (org-back-to-heading t)
                     (line-beginning-position))))
              (should (= (window-start window) expected-start)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-refresh-buffer-does-not-duplicate-top-level-sections ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t))
          (dotimes (_ 5)
            (delib-flow-refresh-buffer))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (dolist (heading delib-flow--control-sections)
              (goto-char (point-min))
              (should (= 1
                         (how-many (format "^\\*\\* %s$" (regexp-quote heading))
                                   (point-min)
                                   (point-max)))))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-refresh-buffer-detects-managed-region-conflicts ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (search-forward "** Details")
              (delete-region (line-beginning-position)
                             (line-end-position))))
          (delib-flow-refresh-buffer)
          (let* ((conflicts
                  (plist-get (delib-flow--run-ui delib-flow--active-run)
                             :managed-region-conflicts))
                 (actions (plist-get (delib-flow--run-actions delib-flow--active-run)
                                     :items))
                 (inspect-action
                  (seq-find (lambda (action)
                              (eq (plist-get action :id) 'inspect-source))
                            actions))
                 (refresh-action
                  (seq-find (lambda (action)
                              (eq (plist-get action :id) 'refresh-buffer))
                            actions)))
            (should-not conflicts)
            (should (equal 'available (plist-get inspect-action :status)))
            (should (equal 'available (plist-get refresh-action :status)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-render-active-run-buffer-clears-stale-managed-region-conflicts ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (run (delib-flow--set-managed-region-conflicts
               run
               '("Missing section: Details")))
         (delib-flow--active-run (delib-flow--seed-actions run))
         (buffer (delib-flow--render-active-run-buffer delib-flow--active-run "Now")))
    (unwind-protect
        (let* ((conflicts
                (plist-get (delib-flow--run-ui delib-flow--active-run)
                           :managed-region-conflicts))
               (actions (plist-get (delib-flow--run-actions delib-flow--active-run)
                                   :items))
               (inspect-action
                (seq-find (lambda (action)
                            (eq (plist-get action :id) 'inspect-source))
                          actions)))
          (should (buffer-live-p buffer))
          (should-not conflicts)
          (should (equal 'available (plist-get inspect-action :status))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-dispatch-action-executes-rendered-next-action ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-active-run-buffer run "Next actions")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "- Inspect Source")
            (goto-char (line-beginning-position)))
          (with-current-buffer buffer
            (delib-flow-dispatch-action))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (equal "Current result"
                           (delib-flow--current-section-at-point)))
            (goto-char (point-min))
            (should (search-forward "- Stage: Inspect Source" nil t))
            (should (search-forward "- Review state: pending-review" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-render-clean-source-buffer-falls-back-to-source-lines ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Plain source"
                     :content "* Plain source\nPreview:\nUseful line\nView in browser\n")))
         (buffer (delib-flow--render-clean-source-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'delib-flow-source-view-mode))
          (should (eq delib-flow--surface-kind 'source-view))
          (should (equal delib-flow--active-run run))
          (should (string-match-p "Delib-Flow clean source" header-line-format))
          (goto-char (point-min))
          (should (search-forward "- Type: unknown" nil t))
          (goto-char (point-min))
          (should (search-forward "- Contacts: none" nil t))
          (goto-char (point-min))
          (should-not (search-forward "Preview:" nil t))
          (goto-char (point-min))
          (should-not (search-forward "No cleaned source lines are available." nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


(ert-deftest delib-flow-ui-surface-modes-own-keymaps-and-surface-kinds ()
  (with-temp-buffer
    (delib-flow-control-mode)
    (should (eq delib-flow--surface-kind 'cockpit))
    (should (delib-flow--surface-mode-p))
    (should (eq (lookup-key delib-flow-control-mode-map (kbd ".")) #'delib-flow-control-context-menu)))
  (with-temp-buffer
    (delib-flow-filing-workspace-mode)
    (should (eq delib-flow--surface-kind 'filing-workspace))
    (should (delib-flow--surface-mode-p))
    (should (eq (lookup-key delib-flow-filing-workspace-mode-map (kbd "O")) #'delib-flow-control-open-clean-source)))
  (with-temp-buffer
    (delib-flow-note-part-editor-mode)
    (should (eq delib-flow--surface-kind 'note-part-editor))
    (should (delib-flow--surface-mode-p)))
  (with-temp-buffer
    (delib-flow-field-editor-mode)
    (should (eq delib-flow--surface-kind 'field-editor))
    (should (delib-flow--surface-mode-p)))
  (with-temp-buffer
    (delib-flow-source-view-mode)
    (should (eq delib-flow--surface-kind 'source-view))))

(ert-deftest delib-flow-ui-control-help-reports-current-section-and-local-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "** Now")
          (org-back-to-heading t)
          (delib-flow-control-help)
          (with-current-buffer (help-buffer)
            (goto-char (point-min))
            (should (search-forward "Current section: Now" nil t))
            (goto-char (point-min))
            (should (search-forward "Local actions here:" nil t))
            (goto-char (point-min))
            (should (search-forward "Global controls:" nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p (get-buffer (help-buffer)))
        (kill-buffer (get-buffer (help-buffer)))))))

(ert-deftest delib-flow-ui-control-menu-dispatches-selected-entry ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (buffer (delib-flow--render-control-buffer run))
         selected-command)
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "** Now")
          (org-back-to-heading t)
          (let* ((entries (delib-flow--context-menu-entries-for-current-point run))
                 (label (delib-flow--context-menu-choice-label (car entries))))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) label))
                      ((symbol-function 'call-interactively)
                       (lambda (command &optional _record-flag _keys)
                         (setq selected-command command))))
              (delib-flow-control-menu)))
          (should selected-command))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-ui-field-editor-apply-updates-operator-intent ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (delib-flow--active-run run)
         (control-buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (progn
          (with-current-buffer control-buffer
            (setq-local delib-flow--active-run-buffer t))
          (delib-flow-action-edit-operator-intent)
          (with-current-buffer (get-buffer delib-flow-field-editor-buffer-name)
            (erase-buffer)
            (insert "Intent from editor")
            (delib-flow-field-editor-apply))
          (should (equal "Intent from editor"
                         (delib-flow--editable-block-text
                          (delib-flow--editable-block delib-flow--active-run
                                                      'context-main))))
          (should-not (buffer-live-p (get-buffer delib-flow-field-editor-buffer-name))))
      (when (buffer-live-p (get-buffer delib-flow-field-editor-buffer-name))
        (kill-buffer (get-buffer delib-flow-field-editor-buffer-name)))
      (when (buffer-live-p control-buffer)
        (kill-buffer control-buffer))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-open-filing-workspace-renders-focused-note-buffer ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body
                                  "* Working draft\nAtlas draft body.\n"))
         (run (delib-flow--seed-filing-selection-block
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
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate)
                      :selected-draft drafted-item))))
         (delib-flow--active-run (delib-flow-test--set-filing-selection run "1")))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (with-current-buffer (get-buffer delib-flow-filing-workspace-buffer-name)
            (goto-char (point-min))
            (should (eq delib-flow--surface-kind 'filing-workspace))
            (should (derived-mode-p 'delib-flow-filing-workspace-mode))
            (should (search-forward "Atlas draft body" nil t))
            (goto-char (point-min))
            (should (search-forward "** Selected artifact" nil t))
            (should (search-forward "** Working draft" nil t))
            (should (search-forward "*** Source evidence" nil t))
            (should (search-forward "*** What changed from last revision" nil t))
            (should (search-forward "*** Note title" nil t))
            (should (search-forward "*** Draft body" nil t))
            (should (search-forward "*** Source highlights" nil t))
            (should (search-forward "*** Related material" nil t))
            (should (search-forward "*** Reuse angle" nil t))
            (should (search-forward "*** Whole note rebuild" nil t))
            (should (search-forward "*** Revision compare" nil t))
            (should (search-forward "** Support for this draft" nil t))
            (should (search-forward "*** Attached support" nil t))
            (should (search-forward "*** Support suggestions" nil t))
            (should (search-forward "** Save or discard" nil t))
            (should (search-forward "** Final save target" nil t))
            (should (search-forward "** Leave workspace" nil t))
            (should-not (search-forward "** Next actions" nil t))
            (goto-char (point-min))
            (should (search-forward "**** Do here" nil t))
            (goto-char (point-min))
            (should (search-forward "- [2] Edit Note Title" nil t))
            (goto-char (point-min))
            (should (search-forward "- [4] Regenerate Draft Body" nil t))
            (goto-char (point-min))
            (should (search-forward "- [1] Regenerate Selected Note" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name))))))

(ert-deftest delib-flow-ui-open-filing-workspace-renders-project-buffer-at-do-here-now ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Broken steno exercise"
                       :content (string-join
                                 '("* Broken steno exercise"
                                   "https://example.com/path?id=one"
                                   "Need to fix a couple of exercises on the steno website.")
                                 "\n"))))
           (inspected (delib-flow-test--accept-inspect
                       (delib-flow--run-stage-locally run 'inspect-source)))
           (matched (delib-flow-test--accept-match
                     (delib-flow--run-stage-locally inspected 'match-project)))
           (proposed (delib-flow--run-stage-locally matched 'propose-new-project))
           (delib-flow--active-run (delib-flow-test--set-filing-selection proposed "1")))
      (unwind-protect
          (progn
            (delib-flow-open-filing-workspace)
            (with-current-buffer (get-buffer delib-flow-filing-workspace-buffer-name)
              (goto-char (point-min))
              (should (eq delib-flow--surface-kind 'filing-workspace))
              (should (derived-mode-p 'delib-flow-filing-workspace-mode))
              (should (search-forward "** Project package" nil t))
              (goto-char (point-min))
              (should (search-forward "** Do here now" nil t))
              (goto-char (point-min))
              (should (search-forward "** Package consequence" nil t))
              (goto-char (point-min))
              (should (search-forward "** Included items" nil t))
              (goto-char (point-min))
              (should (search-forward "** Targets and staged output" nil t))
              (goto-char (point-min))
              (should (search-forward "** Leave workspace" nil t))
              (goto-char (point-min))
              (should (search-forward "[1] Draft Selected Project" nil t))
              (goto-char (point-min))
              (should (search-forward "One project package is selected but not yet drafted" nil t))
              (goto-char (point-min))
              (should (search-forward-regexp "^\\*\\* Do here now$" nil t))
              (should (equal "Do here now"
                             (delib-flow--focused-filing-workspace-section-at-point)))))
        (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
          (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))))))

(ert-deftest delib-flow-ui-filing-workspace-keymap-restricts-non-filing-shortcuts ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (run (plist-put
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
                     :target-locations nil)))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (with-current-buffer (get-buffer delib-flow-filing-workspace-buffer-name)
            (should (eq #'delib-flow-control-return-main-cockpit
                        (lookup-key delib-flow-filing-workspace-mode-map
                                    (kbd "q"))))
            (should (eq #'delib-flow-control-return-main-cockpit
                        (lookup-key delib-flow-filing-workspace-mode-map
                                    (kbd "B"))))
            (should (eq #'delib-flow-control-dispatch-shortcut
                        (lookup-key delib-flow-filing-workspace-mode-map
                                    (kbd "1"))))
            (should-not (lookup-key delib-flow-filing-workspace-mode-map
                                    (kbd "U")))
            (should-not (lookup-key delib-flow-filing-workspace-mode-map
                                    (kbd "L")))))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-filing-workspace-context-menu-stays-filing-local ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (run (plist-put
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
                     :target-locations nil)))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (with-current-buffer (get-buffer delib-flow-filing-workspace-buffer-name)
            (let ((labels (mapcar (lambda (entry) (plist-get entry :label))
                                  (delib-flow--context-menu-entries-for-current-point
                                   delib-flow--active-run))))
              (should (member "Choose Filing Artifact" labels))
              (should (member "Return to Main Cockpit" labels))
              (should (member "Refresh Workspace" labels))
              (should-not (member "Jump to Active Loop" labels))
              (should-not (member "Jump to Stage History" labels))
              (should-not (member "Enable Focus Mode" labels))
              (should-not (member "Disable Focus Mode" labels)))))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-return-main-cockpit-closes-focused-filing-workspace ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (run (plist-put
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
                     :target-locations nil)))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (delib-flow-open-filing-workspace)
          (delib-flow-return-main-cockpit)
          (should-not (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name)))
          (should (buffer-live-p (get-buffer delib-flow-control-buffer-name)))
          (should (delib-flow--filing-workspace-suppressed-p delib-flow--active-run)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-open-clean-source-renders-cleaned-source-buffer ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Building personal AI advisors"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (run
          (delib-flow--initialize-run
           (list :title "Your Consumption Diet Is Your Moat"
                 :content
                 (concat
                  "* Your Consumption Diet Is Your Moat :email:\n"
                  ":PROPERTIES:\n:FROM: Forte Labs <hello@fortelabs.com>\n:END:\n\n"
                  ":RAW_EMAIL:\n"
                  "Subject: Your Consumption Diet Is Your Moat\n\n"
                  "Preview:\n"
                  "The idea: Building personal AI advisors.\n"
                  "View in browser\n"
                  ":END:\n"))))
         (run
          (plist-put
           run :filing
           (list :draft-items (list candidate)
                 :approved-items nil
                 :rejected-items nil
                 :preview-text nil
                 :selection-blocked-item nil
                 :selection-blocking-warnings nil
                 :selection-blocked-selection nil
                 :selection-blocked-notes nil
                 :conflicts nil
                 :target-locations nil)))
         (run
          (delib-flow--seed-filing-selection-block
           (delib-flow--set-artifact-family-state
            run
            'reference-notes
            (list :candidates (list candidate)
                  :selected-candidate-id
                  (delib-flow--artifact-candidate-id candidate)))))
         (run (delib-flow-test--set-filing-selection run "1"))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (should (string-match-p "press `O` to reopen the cleaned source"
                                  (delib-flow--reference-note-workspace-summary-text run)))
          (delib-flow-open-clean-source)
          (with-current-buffer (get-buffer delib-flow-source-view-buffer-name)
            (goto-char (point-min))
            (should (derived-mode-p 'delib-flow-source-view-mode))
            (should (search-forward "Building personal AI advisors" nil t))
            (goto-char (point-min))
            (should-not (search-forward "Preview:" nil t))
            (goto-char (point-min))
            (should-not (search-forward "View in browser" nil t))
            (should (eq #'delib-flow-control-open-clean-source
                        (lookup-key delib-flow-filing-workspace-mode-map
                                    (kbd "O"))))))
      (when (buffer-live-p (get-buffer delib-flow-source-view-buffer-name))
        (kill-buffer (get-buffer delib-flow-source-view-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-open-clean-source-errors-without-active-run ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-open-clean-source) :type 'user-error)))

(ert-deftest delib-flow-ui-source-view-mode-configures-surface-and-keys ()
  (with-temp-buffer
    (delib-flow-source-view-mode)
    (should (derived-mode-p 'delib-flow-source-view-mode))
    (should (eq delib-flow--surface-kind 'source-view))
    (should line-move-ignore-invisible)
    (should-not show-trailing-whitespace)
    (should (equal header-line-format
                   " Delib-Flow clean source: q close, B cockpit, F filing, g refresh "))
    (should (eq #'delib-flow-control-open-filing-workspace
                (lookup-key delib-flow-source-view-mode-map (kbd "F"))))
    (should (eq #'delib-flow-control-open-clean-source
                (lookup-key delib-flow-source-view-mode-map (kbd "O"))))
    (should (eq #'delib-flow-control-refresh
                (lookup-key delib-flow-source-view-mode-map (kbd "g"))))))

(ert-deftest delib-flow-ui-open-clean-source-reuses-buffer-and-updates-active-run ()
  (let* ((run-a (delib-flow--initialize-run
                 (list :title "First source"
                       :content "* First source\nUseful line A\n")))
         (run-b (delib-flow--initialize-run
                 (list :title "Second source"
                       :content "* Second source\nUseful line B\n")))
         (delib-flow--active-run run-a)
         buffer)
    (unwind-protect
        (progn
          (setq buffer (delib-flow-open-clean-source))
          (setq delib-flow--active-run run-b)
          (should (eq buffer (delib-flow-open-clean-source)))
          (with-current-buffer buffer
            (should (equal delib-flow--active-run run-b))
            (goto-char (point-min))
            (should (search-forward "Second source" nil t))
            (goto-char (point-min))
            (should (search-forward "Useful line B" nil t))))
      (when (buffer-live-p (get-buffer delib-flow-source-view-buffer-name))
        (kill-buffer (get-buffer delib-flow-source-view-buffer-name))))))

(ert-deftest delib-flow-ui-context-menu-audit-sections-include-save-command ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (current-context
          (delib-flow--context-menu-current-context-entries run))
         (details
          (delib-flow--context-menu-details-entries)))
    (should (member "Save Active Audit Run"
                    (mapcar (lambda (entry) (plist-get entry :label))
                            current-context)))
    (should (member "Save Active Audit Run"
                    (mapcar (lambda (entry) (plist-get entry :label))
                            details)))))

(ert-deftest delib-flow-ui-return-main-cockpit-uses-filing-preview-default-anchor ()
  (let* ((run (delib-flow-test--reference-note-candidate-only-run))
         (delib-flow--active-run run)
         rendered-anchor
         control-buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'delib-flow--preferred-anchor-section)
                   (lambda (_) nil))
                  ((symbol-function 'delib-flow--render-active-run-buffer)
                   (lambda (active-run anchor)
                     (setq rendered-anchor anchor)
                     (setq control-buffer (get-buffer-create "*delib-flow-control-anchor-test*"))
                     (with-current-buffer control-buffer
                       (delib-flow-control-mode)
                       (setq-local delib-flow--active-run active-run))
                     control-buffer)))
          (delib-flow-return-main-cockpit)
          (should (equal rendered-anchor "Filing preview"))
          (should (eq (get-buffer "*delib-flow-control-anchor-test*") control-buffer)))
      (when (buffer-live-p control-buffer)
        (kill-buffer control-buffer))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-jump-active-filing-workspace-opens-or-reuses-buffer ()
  (should-error (delib-flow-jump-to-active-filing-workspace) :type 'user-error)
  (let* ((run (delib-flow-test--reference-note-candidate-only-run))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (delib-flow-jump-to-active-filing-workspace)
          (should (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name)))
          (let ((buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
            (delib-flow-jump-to-active-filing-workspace)
            (should (eq buffer (get-buffer delib-flow-filing-workspace-buffer-name)))))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-ui-return-main-cockpit-errors-without-run-and-rerenders-without-workspace ()
  (let ((delib-flow--active-run nil))
    (should-error (delib-flow-return-main-cockpit) :type 'user-error))
  (let* ((run (delib-flow-test--reference-note-candidate-only-run))
         (delib-flow--active-run run))
    (unwind-protect
        (progn
          (delib-flow-return-main-cockpit)
          (should (buffer-live-p (get-buffer delib-flow-control-buffer-name)))
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (should (derived-mode-p 'delib-flow-control-mode)))
          (should (plist-get (delib-flow--run-ui delib-flow--active-run)
                             :filing-workspace-suppressed)))
      (when (buffer-live-p (get-buffer delib-flow-filing-workspace-buffer-name))
        (kill-buffer (get-buffer delib-flow-filing-workspace-buffer-name)))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(provide 'delib-flow-ui-test)

;;; delib-flow-ui-test.el ends here
