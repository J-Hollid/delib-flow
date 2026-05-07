;;; delib-flow-model-test.el --- Model tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'delib-flow)

(ert-deftest delib-flow-make-editable-block-returns-expected-shape ()
  (let ((block (delib-flow--make-editable-block
                'context-main
                'context
                "Current context"
                '("Current context" "Editable working slice")
                "delib-edit-context-main")))
    (should (equal 'context-main (plist-get block :id)))
    (should (equal 'context (plist-get block :kind)))
    (should (equal "Current context" (plist-get block :section)))
    (should (equal 'clean (plist-get block :status)))
    (should (equal 'valid (plist-get block :validation-status)))))

(ert-deftest delib-flow-initial-section-anchors-contains-required-sections ()
  (let ((anchors (delib-flow--initial-section-anchors)))
    (dolist (section '(now
                       next-actions
                       current-result
                       current-context
                       filing-preview
                       details))
      (should (assoc section anchors)))))

(ert-deftest delib-flow-initialize-run-contains-required-top-level-keys ()
  (let* ((snapshot (list :title "Example"))
         (run (delib-flow--initialize-run snapshot)))
    (dolist (key '(:source
                   :working-context
                   :stage-history
                   :actions
                   :artifacts
                   :routing
                   :filing
                   :audit
                   :session
                   :ui))
      (should (plist-member run key)))))

(ert-deftest delib-flow-initialize-run-seeds-artifact-family-state ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (artifacts (plist-get run :artifacts)))
    (dolist (family '(actions waiting-fors reference-notes project-proposals))
      (let ((state (plist-get artifacts family)))
        (should (plist-member artifacts family))
        (should (equal nil (plist-get state :candidates)))
        (should (equal nil (plist-get state :selected-candidate-id)))
        (should (equal nil (plist-get state :selected-draft)))))))

(ert-deftest delib-flow-initialize-run-stores-source-in-source-domain ()
  (let* ((snapshot (list :title "Example" :content "* Example"))
         (run (delib-flow--initialize-run snapshot)))
    (should (equal snapshot (delib-flow--run-source run)))
    (should (equal snapshot
                   (plist-get (delib-flow--run-working-context run)
                              :source-snapshot)))))

(ert-deftest delib-flow-initialize-run-seeds-editable-blocks ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (working (delib-flow--run-working-context run))
         (ui (delib-flow--run-ui run))
         (block-ids (plist-get working :editable-block-ids))
         (blocks (plist-get ui :editable-blocks)))
    (should (equal '(context-main operator-notes manual-project-selection filing-selection-review filing-conflict-resolution reference-note-capture-review inspect-source-review cloud-package-review cloud-routing-review cloud-failure-review) block-ids))
    (should (assoc 'context-main blocks))
    (should (assoc 'operator-notes blocks))
    (should (assoc 'manual-project-selection blocks))
    (should (assoc 'filing-selection-review blocks))
    (should (assoc 'filing-conflict-resolution blocks))
    (should (assoc 'reference-note-capture-review blocks))
    (should (assoc 'inspect-source-review blocks))
    (should (assoc 'cloud-package-review blocks))
    (should (assoc 'cloud-routing-review blocks))
    (should (assoc 'cloud-failure-review blocks))))

(ert-deftest delib-flow-initialize-run-seeds-reference-note-capture-block-clean ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (block (delib-flow--editable-block run 'reference-note-capture-review)))
    (should (eq 'clean (plist-get block :status)))
    (should (string-match-p
             "No reference-note filing artifact is currently active."
             (plist-get block :current-text)))))

(ert-deftest delib-flow-initialize-run-seeds-review-results ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (working (delib-flow--run-working-context run))
         (inspect-review (delib-flow--review-record working 'inspect-source))
         (match-review (delib-flow--review-record working 'match-project)))
    (should inspect-review)
    (should match-review)
    (should (eq 'not-available
                (plist-get inspect-review :candidate-review-state)))
    (should-not (plist-get inspect-review :accepted-output))
    (should (eq 'not-available
                (plist-get match-review :candidate-review-state)))
    (should-not (plist-get match-review :accepted-output))))

(ert-deftest delib-flow-initialize-run-seeds-section-anchors ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (anchors (plist-get (delib-flow--run-ui run) :section-anchors)))
    (dolist (section '(now
                       next-actions
                       current-result
                       current-context
                       filing-preview
                       details))
      (should (assoc section anchors)))))

(ert-deftest delib-flow-initialize-run-seeds-session-state ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (session (delib-flow--run-session run)))
    (should (equal 'active (plist-get session :status)))
    (should (equal "Review working context and choose next action."
                   (plist-get session :current-decision)))))

(ert-deftest delib-flow-initialize-run-seeds-audit-run-record ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (audit (plist-get run :audit))
         (run-record (plist-get audit :run-record)))
    (should (plist-get run-record :run-id))
    (should (equal "Example" (plist-get run-record :source-title)))
    (should (eq 'active (plist-get run-record :run-status)))))

(ert-deftest delib-flow-initialize-run-seeds-actions ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (actions (plist-get (delib-flow--run-actions run) :items))
         (action-ids (mapcar (lambda (action) (plist-get action :id)) actions)))
    (should (equal '(inspect-source refresh-buffer abort-run) action-ids))
    (should (equal 'available
                   (plist-get (car actions) :status)))))

(ert-deftest delib-flow-set-editable-block-text-marks-edited-status ()
  (let* ((block (delib-flow--make-editable-block
                 'context-main
                 'context
                 "Working context"
                 '("Working context" "Editable working slice")
                 "delib-edit-context-main"))
         (updated (delib-flow--set-editable-block-text block "Changed text")))
    (should (equal "Changed text" (plist-get updated :current-text)))
    (should (equal 'edited (plist-get updated :status)))))

(ert-deftest delib-flow-artifact-family-available-support-getters-read-stored-state ()
  (let* ((run (delib-flow--initialize-run (list :title "Example")))
         (support (list :title "Atlas brief"))
         (updated
          (delib-flow--set-artifact-family-available-support
           run
           'reference-notes
           (list support)
           "- Atlas brief - Constraint summary")))
    (should (equal (list support)
                   (delib-flow--artifact-family-available-support-candidates
                    updated
                    'reference-notes)))
    (should (equal "- Atlas brief - Constraint summary"
                   (delib-flow--artifact-family-available-support-context
                    updated
                    'reference-notes)))))

(ert-deftest delib-flow-artifact-family-available-support-getters-return-nil-without-state ()
  (let ((run (delib-flow--initialize-run (list :title "Example"))))
    (should-not
     (delib-flow--artifact-family-available-support-candidates
      run
      'reference-notes))
    (should-not
     (delib-flow--artifact-family-available-support-context
      run
      'reference-notes))))

(provide 'delib-flow-model-test)
;;; delib-flow-model-test.el ends here
