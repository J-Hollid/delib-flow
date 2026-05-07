;;; delib-flow-filing-test.el --- Filing tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'delib-flow)
(require 'delib-flow-test-support)

(ert-deftest delib-flow-select-approved-filing-actions-updates-filing-state ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (updated-run
            (delib-flow-test--approve-selected-filing-item
             (delib-flow-test--set-filing-selection integrated "1")))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (filing (plist-get updated-run :filing))
           (approved-items (plist-get filing :approved-items))
           (draft-items (plist-get filing :draft-items)))
      (should (equal 'select-approved-filing-actions
                     (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'select-approved-filing-actions
                     (plist-get entry :stage-id)))
      (should approved-items)
      (should (equal 1 (length approved-items)))
      (should-not draft-items)
      (should (equal 1 (plist-get (plist-get entry :raw-output) :selected-count)))
      (should (string-match-p "Review selected filing"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision))))))

(ert-deftest delib-flow-reject-draft-filing-artifact-updates-filing-state ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (drafted (delib-flow--run-stage-locally filtered 'extract-actions))
             (integrated
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (updated-run
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-selection integrated "1" "Too vague for this run.")
               'reject-draft-filing-artifact))
             (history (delib-flow--run-stage-history updated-run))
             (entry (car (last (plist-get history :entries))))
             (raw (plist-get entry :raw-output))
             (filing (plist-get updated-run :filing))
             (rejected-items (plist-get filing :rejected-items))
             (draft-items (plist-get filing :draft-items)))
        (should (equal 'reject-draft-filing-artifact
                       (plist-get history :latest-stage)))
        (should (equal 'completed (plist-get history :latest-status)))
        (should (equal 'reject-draft-filing-artifact
                       (plist-get entry :stage-id)))
        (should rejected-items)
        (should (equal 1 (length rejected-items)))
        (should (equal 2 (length draft-items)))
        (should (equal 1 (plist-get raw :rejected-count)))
        (should (equal "1" (plist-get raw :operator-selection)))
        (should (string-match-p "Too vague for this run."
                                (or (plist-get raw :operator-notes) "")))
        (should (string-match-p "Review rejected filing artifact"
                                (plist-get (delib-flow--run-session updated-run)
                                           :current-decision)))))))

(ert-deftest delib-flow-reject-draft-filing-artifact-can-select-non_head-artifact ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nAgenda\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (drafted (delib-flow--run-stage-locally filtered 'extract-actions))
             (integrated
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (rejected-run
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-selection integrated "2")
               'reject-draft-filing-artifact))
             (rejected (car (plist-get (plist-get rejected-run :filing)
                                       :rejected-items)))
             (remaining (plist-get (plist-get rejected-run :filing)
                                   :draft-items)))
        (should (string-match-p "Alpha Project Notes"
                                (plist-get rejected :text)))
        (should (equal 2 (length remaining)))
        (should (string-match-p "Alpha Project kickoff"
                                (plist-get (car remaining) :text)))))))

(ert-deftest delib-flow-select-approved-filing-actions-can-select-non_head-artifact ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nAgenda\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (drafted (delib-flow--run-stage-locally filtered 'extract-actions))
             (integrated
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (selected-run
              (delib-flow-test--approve-selected-filing-item
               (delib-flow-test--set-filing-selection integrated "2")))
             (raw (plist-get (car (last (plist-get (plist-get selected-run :stage-history)
                                                   :entries)))
                             :raw-output))
             (filing (plist-get selected-run :filing))
             (approved (car (plist-get filing :approved-items)))
             (remaining (plist-get filing :draft-items)))
        (should (equal "2" (plist-get raw :operator-selection)))
        (should (string-match-p "Alpha Project Notes"
                                (plist-get approved :text)))
        (should (equal 2 (length remaining)))
        (should (string-match-p "Alpha Project kickoff"
                                (plist-get (car remaining) :text)))))))

(ert-deftest delib-flow-select-approved-filing-actions-blocks-artifacts-with-blocking-warnings ()
  (let ((delib-flow-project-support-note-capture-template
         "#+filetags: :project:support:\n\nSource artifact: %(delib-flow-capture-source-artifact)\n"))
    (delib-flow-test--with-temp-zk-root
        '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
          ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
      (delib-flow-test--with-temp-project-file
          "* Alpha Project\n"
        (let* ((run (delib-flow--initialize-run
                     (list :title "Alpha Project kickoff"
                           :content "* Alpha Project kickoff\nAgenda\n")))
               (inspected (delib-flow-test--accept-inspect
                           (delib-flow--run-stage-locally run 'inspect-source)))
               (matched (delib-flow-test--accept-match
                         (delib-flow--run-stage-locally inspected 'match-project)))
               (filtered
                (delib-flow--run-stage-locally
                 (delib-flow--run-stage-locally matched
                                                'discover-reference-material)
                 'filter-reference-material))
               (drafted
                (delib-flow--run-stage-locally filtered 'suggest-reference-notes))
               (integrated
                (delib-flow--run-stage-locally drafted 'integrate-into-source))
               (updated-run
                (let* ((selected (delib-flow-test--set-filing-selection integrated "1"))
                       (with-draft
                        (delib-flow--set-artifact-family-selected-draft
                         selected
                         'reference-notes
                         (plist-put
                          (copy-tree (car (plist-get (plist-get selected :filing) :draft-items)))
                          :draft-body "* Working draft\nTemplate title still missing.\n"))))
                  (delib-flow--run-stage-locally
                   with-draft
                   'select-approved-filing-actions)))
               (entry (car (last (plist-get (plist-get updated-run :stage-history)
                                            :entries))))
               (raw (plist-get entry :raw-output))
               (filing (plist-get updated-run :filing)))
          (should-not (plist-get filing :approved-items))
          (should (equal 2 (length (plist-get filing :draft-items))))
          (should (plist-get raw :approval-blocked-p))
          (should (equal 0 (plist-get raw :selected-count)))
          (should (equal nil (plist-get raw :ready-selection-indexes)))
          (should (equal '(1 2) (plist-get raw :blocked-selection-indexes)))
          (should (equal "1" (plist-get filing :selection-blocked-selection)))
          (should (plist-get filing :selection-blocked-item))
          (should (= 1 (length (plist-get filing :selection-blocking-warnings))))
          (should (= 1 (length (plist-get raw :blocking-warnings))))
          (should (eq 'reference-note-template-title
                      (plist-get (car (plist-get raw :blocking-warnings)) :code)))
          (should (string-match-p "blocking warnings"
                                  (plist-get raw :reason))))))))

(ert-deftest delib-flow-filing-selection-template-shows-readiness-guidance ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alpha Project kickoff"
                     :content "* Alpha Project kickoff\nAgenda\n")))
         (filing
          (list :draft-items
                (list
                 (list :kind 'reference-note
                       :text "Create project support note from Alpha kickoff"
                       :warnings
                       (list
                        (delib-flow--make-artifact-warning
                         'reference-note-template-title
                         "Configured note template does not include `${title}`, so note-title filing readiness is weak."
                         'blocking)))
                 (list :kind 'next-action
                       :text "Send budget update to Alice"
                       :warnings nil))))
         (templated
          (delib-flow--seed-filing-selection-block
           (plist-put run :filing filing)))
         (template (delib-flow--editable-block-text
                    (delib-flow--editable-block templated 'filing-selection-review))))
    (should (string-match-p "Selection guidance:" template))
    (should (string-match-p "Ready selections: 2" template))
    (should (string-match-p "Blocked selections: 1" template))
    (should (string-match-p "Status: blocked by 1 filing-readiness issue" template))
    (should (string-match-p "Status: ready for approval" template))
    (should (string-match-p "Fix: Add `${title}` to the configured note template before approving this note." template))))

(ert-deftest delib-flow-select-approved-filing-actions-template-shows-all-blocked-guidance ()
  (let ((delib-flow-project-support-note-capture-template
         "#+filetags: :project:support:\n\nSource artifact: %(delib-flow-capture-source-artifact)\n"))
    (delib-flow-test--with-temp-zk-root
        '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
          ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
      (delib-flow-test--with-temp-project-file
          "* Alpha Project\n"
        (let* ((run (delib-flow--initialize-run
                     (list :title "Alpha Project kickoff"
                           :content "* Alpha Project kickoff\nAgenda\n")))
               (inspected (delib-flow-test--accept-inspect
                           (delib-flow--run-stage-locally run 'inspect-source)))
               (matched (delib-flow-test--accept-match
                         (delib-flow--run-stage-locally inspected 'match-project)))
               (filtered
                (delib-flow--run-stage-locally
                 (delib-flow--run-stage-locally matched
                                                'discover-reference-material)
                 'filter-reference-material))
               (integrated
                (delib-flow--run-stage-locally
                 (delib-flow--run-stage-locally filtered 'suggest-reference-notes)
                 'integrate-into-source))
               (template (delib-flow--editable-block-text
                          (delib-flow--editable-block integrated 'filing-selection-review))))
          (should (string-match-p "Selection guidance:" template))
          (should (string-match-p "Ready selections: none" template))
          (should (string-match-p "Blocked selections: 1, 2" template))
          (should (string-match-p "Status: blocked by 1 filing-readiness issue" template))
          (should (string-match-p "Fix: Add `${title}` to the configured note template before approving this note." template)))))))

(ert-deftest delib-flow-file-approved-outputs-updates-stage-history-and-project-file ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated
            (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected
            (delib-flow-test--set-filing-selection integrated "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (updated-run
            (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (history (delib-flow--run-stage-history updated-run))
           (entry (car (last (plist-get history :entries))))
           (filing (plist-get updated-run :filing))
            (locations (plist-get filing :target-locations)))
      (should (equal 'file-approved-outputs (plist-get history :latest-stage)))
      (should (equal 'completed (plist-get history :latest-status)))
      (should (equal 'file-approved-outputs (plist-get entry :stage-id)))
      (should locations)
      (should-not (plist-get filing :approved-items))
      (should (string-match-p "Alpha Project"
                              (plist-get (car locations) :target)))
      (should (string-match-p
               "\\*\\* TODO Write follow-up note for Alpha Project kickoff"
               (delib-flow-test--file-buffer-string delib-flow-my-projects-file)))
      (should (buffer-modified-p (get-file-buffer delib-flow-my-projects-file)))
      (should (string-match-p "Review filed outputs"
                              (plist-get (delib-flow--run-session updated-run)
                                         :current-decision))))))

(ert-deftest delib-flow-apply-select-approved-filing-actions-clears-stale-filed-locations ()
  (let* ((run (list :filing (list :draft-items nil
                                  :approved-items nil
                                  :rejected-items nil
                                  :target-locations
                                  (list (list :kind 'next-action
                                              :item-text "Old item"
                                              :target "/tmp/old.org::Alpha")))))
         (entry (list :raw-output
                      (list :approval-blocked-p nil
                            :approved-items
                            (list (list :kind 'next-action :text "New item"))
                            :remaining-draft-items nil)))
         (updated (delib-flow--apply-select-approved-filing-actions-entry
                   run entry)))
    (should-not (plist-get (plist-get updated :filing) :target-locations))))

(ert-deftest delib-flow-apply-file-approved-outputs-replaces-filed-locations ()
  (let* ((run (list :filing (list :approved-items
                                  (list (list :kind 'next-action :text "New item"))
                                  :target-locations
                                  (list (list :kind 'next-action
                                              :item-text "Old item"
                                              :target "/tmp/old.org::Alpha")))))
         (entry (list :raw-output
                      (list :conflicts nil
                            :target-locations
                            (list (list :kind 'next-action
                                        :item-text "New item"
                                        :target "/tmp/new.org::Beta")))))
         (updated (delib-flow--apply-file-approved-outputs-entry run entry))
         (locations (plist-get (plist-get updated :filing) :target-locations)))
    (should (equal 1 (length locations)))
    (should (equal "New item" (plist-get (car locations) :item-text)))))

(ert-deftest delib-flow-file-approved-outputs-creates-reference-note-file ()
  (delib-flow-test--with-temp-zk-root ()
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted
              (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (integrated
              (delib-flow--run-stage-locally drafted 'integrate-into-source))
             (selected
              (delib-flow-test--set-filing-selection integrated "1"))
             (enriched
              (delib-flow-test--draft-selected-filing-item selected))
             (approved
              (delib-flow--run-stage-locally enriched
                                             'select-approved-filing-actions))
             (updated-run
             (delib-flow--run-stage-locally approved 'file-approved-outputs))
             (location (car (plist-get (plist-get updated-run :filing)
                                       :target-locations))))
        (should (buffer-live-p (get-file-buffer (plist-get location :target))))
        (should-not (file-exists-p (plist-get location :target)))
        (should (string-match-p
                 "#\\+title: Alpha Project kickoff"
                 (delib-flow-test--file-buffer-string (plist-get location :target))))
        (should (string-match-p
                 "Create project support note from Alpha Project kickoff"
                 (delib-flow-test--file-buffer-string (plist-get location :target))))))))

(ert-deftest delib-flow-filing-conflict-template-prefers-new-text-for-project-child-conflicts ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n** TODO Write follow-up note for Alpha Project kickoff\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (enriched
            (delib-flow-test--draft-selected-filing-item selected))
           (approved
            (delib-flow--run-stage-locally enriched
                                           'select-approved-filing-actions))
           (conflicted
            (delib-flow--run-stage-locally approved 'file-approved-outputs))
           (template (delib-flow--editable-block-text
                      (delib-flow--editable-block conflicted
                                                 'filing-conflict-resolution))))
      (should (string-match-p "New text:\n\nNew title:\n\\[unused for this conflict type\\]" template))
      (should (string-match-p "REWORD-ITEM" template))
      (should-not (string-match-p "RENAME-NOTE: change the deterministic note title" template)))))

(ert-deftest delib-flow-filing-conflict-template-prefers-new-title-for-note-conflicts ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha-project-kickoff.org"
          . "#+title: Alpha Project kickoff\n\nExisting note\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (selected
              (delib-flow-test--set-filing-selection
               (delib-flow--run-stage-locally drafted 'integrate-into-source)
               "1"))
             (enriched
              (delib-flow-test--draft-selected-filing-item selected))
             (approved
              (delib-flow--run-stage-locally enriched
                                             'select-approved-filing-actions))
             (conflicted
              (delib-flow--run-stage-locally approved 'file-approved-outputs))
             (template (delib-flow--editable-block-text
                        (delib-flow--editable-block conflicted
                                                   'filing-conflict-resolution))))
        (should (string-match-p "New title:\n\nNew text:\n\\[unused for this conflict type\\]" template))
        (should (string-match-p "RENAME-NOTE" template))
        (should-not (string-match-p "REWORD-ITEM: change the deterministic project-child heading text" template))))))

(ert-deftest delib-flow-resolve-filing-conflict-can-rename-note-and-retry ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha-project-kickoff.org"
          . "#+title: Alpha Project kickoff\n\nExisting note\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'suggest-reference-notes))
             (selected
              (delib-flow-test--approve-selected-filing-item
               (delib-flow-test--set-filing-selection
                (delib-flow--run-stage-locally drafted 'integrate-into-source)
                "1")))
             (conflicted
              (delib-flow--run-stage-locally selected 'file-approved-outputs))
             (retargeted
              (delib-flow--run-stage-locally
               (delib-flow-test--set-filing-conflict-resolution
                conflicted "RENAME-NOTE" "Retarget support note."
                "Alpha Project kickoff retargeted")
               'resolve-filing-conflict))
             (approved (car (plist-get (plist-get retargeted :filing)
                                       :approved-items)))
             (filed
              (delib-flow--run-stage-locally retargeted 'file-approved-outputs))
             (locations (plist-get (plist-get filed :filing) :target-locations)))
        (should approved)
        (should
         (string-match-p
          "Create project support note from Alpha Project kickoff retargeted"
          (plist-get approved :text)))
        (should-not (plist-get (plist-get retargeted :filing) :conflicts))
        (should (= 2 (length locations)))
        (should
         (string-match-p
          "#\\+title: Alpha Project kickoff retargeted"
          (delib-flow-test--file-buffer-string
           (plist-get
            (seq-find (lambda (location)
                        (string-match-p "retargeted\\.org\\'"
                                        (plist-get location :target)))
                      locations)
            :target))))))))

(ert-deftest delib-flow-filing-preview-renders-reference-note-draft-previews-before-approval ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Source"
                     :content "* Source\nBody line\n")))
         (notes (list (list :kind 'reference-note
                            :text "Create general PKM note for Project Atlas Pattern"
                            :note-type 'general-pkm
                            :draft-stage 'suggest-reference-notes)
                      (list :kind 'reference-note
                            :text "Create general PKM note for Team Ritual Prompt"
                            :note-type 'general-pkm
                            :draft-stage 'suggest-reference-notes)))
         (run (plist-put
               run :filing
               (plist-put (plist-get run :filing) :draft-items notes)))
         (text (delib-flow--render-filing-preview-section run)))
    (should (string-match-p "Focused filing workspace" text))
    (should (string-match-p "Reference-note candidates are ready" text))
    (should (string-match-p "Main move here: press `F` to open the focused filing workspace" text))
    (should-not (string-match-p "Project Atlas Pattern" text))
    (should-not (string-match-p "Team Ritual Prompt" text))
    (should-not (string-match-p "\\*\\*\\* Working draft" text))))

(ert-deftest delib-flow-filing-selection-current-value-prefers-explicit-selection ()
  (let* ((candidate (list :kind 'next-action
                          :text "TODO Write follow-up note"
                          :project-title "Alpha Project"))
         (run (plist-put
               (delib-flow--initialize-run
                (list :title "Alpha Project kickoff"
                      :content "* Alpha Project kickoff\nBody line\n"))
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
         (selected (delib-flow--set-filing-selection-value run "1")))
    (should (equal "1"
                   (delib-flow--filing-selection-current-value
                    selected
                    (delib-flow--filing-selection-labels selected))))))

(ert-deftest delib-flow-filing-selection-current-value-falls-back-to-active-item ()
  (let* ((first (list :kind 'next-action
                      :text "TODO First action"
                      :project-title "Alpha Project"
                      :candidate-id "first"))
         (second (list :kind 'next-action
                       :text "TODO Second action"
                       :project-title "Alpha Project"
                       :candidate-id "second"))
         (run (plist-put
               (delib-flow--initialize-run
                (list :title "Alpha Project kickoff"
                      :content "* Alpha Project kickoff\nBody line\n"))
               :filing
               (list :draft-items (list first second)
                     :approved-items (list second)
                     :rejected-items nil
                     :preview-text nil
                     :selection-blocked-item nil
                     :selection-blocking-warnings nil
                     :selection-blocked-selection nil
                     :selection-blocked-notes nil
                     :conflicts nil
                     :target-locations nil))))
    (should (equal "2"
                   (delib-flow--filing-selection-current-value
                    run
                    (delib-flow--filing-selection-labels run))))))

(ert-deftest delib-flow-filing-selection-preview-shows-planned-targets-before-approval ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer selected-run)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Selected action workspace" nil t))
            (should (search-forward "**** Planned target or consequence" nil t))
            (should (search-forward "If you approve the currently selected queue item, it will file to these targets." nil t))
            (should (search-forward "Write follow-up note for Alpha Project kickoff" nil t))
            (should (search-forward delib-flow-my-projects-file nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-staged-content-preview-renders-exact-project-child-insert ()
  (let ((delib-flow-next-action-capture-template
         "%(delib-flow-capture-project-child-heading)\n:PROPERTIES:\n:SOURCE_ARTIFACT: %(delib-flow-capture-source-artifact)\n:END:\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow--run-stage-locally run 'inspect-source))
             (matched (delib-flow--run-stage-locally inspected 'match-project))
             (drafted (delib-flow--run-stage-locally matched 'extract-actions))
             (selected-run
              (delib-flow-test--draft-selected-filing-item
               (delib-flow-test--set-filing-selection
                (delib-flow--run-stage-locally drafted 'integrate-into-source)
                "1")))
             (buffer (delib-flow--render-control-buffer selected-run)))
        (unwind-protect
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "*** Staged content preview" nil t))
              (should (search-forward "Preview only: this is the exact text delib-flow will stage" nil t))
              (should (search-forward "**** Project child insert" nil t))
              (should (search-forward "- State: staged only; not saved" nil t))
              (should (search-forward "#+begin_example" nil t))
              (should (search-forward "** TODO Write follow-up note for Alpha Project kickoff" nil t))
              (should (search-forward ":SOURCE_ARTIFACT: Write follow-up note for Alpha Project kickoff" nil t)))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(ert-deftest delib-flow-filing-preview-renders-local-filing-actions ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer selected-run)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (should (search-forward "*** Filing actions" nil t))
            (should (search-forward "- Select Approved Filing Actions [available]" nil t)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-filing-preview-renders-selected-workspace-local-action-palette ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (text (delib-flow--render-filing-preview-section selected-run)))
      (should (string-match-p "\\*\\*\\* Selected action workspace" text))
      (should (string-match-p
               "\\*\\*\\*\\* Local action palette: action"
               text))
      (should (string-match-p
               "Draft Selected Action \\[available\\]"
               text))
      (should (string-match-p
               "Select Approved Filing Actions \\[blocked\\]"
               text))
      (should (< (string-match-p "\\*\\*\\*\\* Local action palette: action" text)
                 (string-match-p "\\*\\*\\* Filing actions" text))))))

(ert-deftest delib-flow-filing-preview-renders-draft-history-and-restore-for-selected-action ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected-run (delib-flow-test--set-filing-selection integrated "1"))
           (candidate (car (plist-get (plist-get selected-run :filing) :draft-items)))
           (current (plist-put (copy-tree candidate) :text "Current drafted action wording"))
           (current (plist-put current
                               :evidence-snapshot
                               (list :draft-reason "Current rationale"
                                     :candidate-origin 'source
                                     :source-title "Alpha Project kickoff"
                                     :source-excerpts '("- Current source excerpt")
                                     :support-items '((:identity "Atlas brief"
                                                       :title "Atlas brief"
                                                       :score 8
                                                       :reason-text "title-overlap")
                                                      (:identity "Shared brief"
                                                       :title "Shared brief"
                                                       :score 9
                                                       :reason-text "title-overlap"
                                                       :focus "current focus"))
                                     :support-lines '("- Atlas brief" "- Shared brief")
                                     :support-context-lines '("Atlas context")
                                     :family-review-heading "Action-specific checks"
                                     :family-review-lines '("Opening verb: `draft`; this reads like a directly executable next step."
                                                            "Outcome cue: `for launch brief`."
                                                            "Scope check: 7 words; this remains task-sized.")
                                     :quality-gaps '("- Tighten owner")
                                     :support-count 2)))
           (previous (plist-put (copy-tree candidate) :text "Previous drafted action wording"))
           (previous (plist-put previous
                                :evidence-snapshot
                                (list :draft-reason "Previous rationale"
                                      :candidate-origin 'retained-context
                                      :source-title "Alpha Project kickoff"
                                      :source-excerpts '("- Previous source excerpt")
                                      :support-items '((:identity "Prior brief"
                                                        :title "Prior brief"
                                                        :score 4
                                                        :reason-text "prior-overlap")
                                                       (:identity "Shared brief"
                                                        :title "Shared brief"
                                                        :score 4
                                                        :reason-text "focused-retained"
                                                        :focus "prior focus"))
                                      :support-lines '("- Prior brief" "- Shared brief")
                                      :support-context-lines '("Prior context")
                                      :family-review-heading "Action-specific checks"
                                      :family-review-lines '("Opening verb: `clarify`; this still reads more like review or clarification than execution."
                                                             "Outcome cue: no explicit recipient, target, or deliverable is named yet."
                                                             "Scope check: 3 words; this remains task-sized.")
                                      :quality-gaps '("- Add due date")
                                      :support-count 2)))
           (older (plist-put (copy-tree candidate) :text "Older drafted action wording"))
           (selected-run
            (delib-flow--set-artifact-family-state
             selected-run
             'actions
             (list :candidates (list candidate)
                   :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                   :selected-draft current
                   :draft-history (list previous older)
                   :selected-support-candidates nil
                   :selected-support-context nil)))
           (text (delib-flow--render-filing-preview-section selected-run)))
      (should (string-match-p "\\*\\*\\*\\* Draft history and restore" text))
      (should (string-match-p "Current drafted action wording" text))
      (should (string-match-p "Previous drafted action wording" text))
      (should (string-match-p "Older drafted action wording" text))
      (should (string-match-p "Frozen evidence review" text))
      (should (string-match-p "Frozen source excerpts" text))
      (should (string-match-p "Frozen support items" text))
      (should (string-match-p "Atlas brief \\[score 8\\]" text))
      (should (string-match-p "Frozen evidence:" text))
      (should (string-match-p "Frozen evidence changes" text))
      (should (string-match-p "Draft reason changed: Previous rationale -> Current rationale" text))
      (should (string-match-p "Source excerpts added: - Current source excerpt" text))
      (should (string-match-p "Source excerpts removed: - Previous source excerpt" text))
      (should (string-match-p "Support items added: Atlas brief" text))
      (should (string-match-p "Support details changed: Shared brief (score 4 -> 9, reason focused-retained -> title-overlap, focus prior focus -> current focus)" text))
      (should (string-match-p "Family review changes: added Opening verb: `draft`; this reads like a directly executable next step. \\| Outcome cue: `for launch brief`. \\| Scope check: 7 words; this remains task-sized.; removed Opening verb: `clarify`; this still reads more like review or clarification than execution. \\| Outcome cue: no explicit recipient, target, or deliverable is named yet. \\| Scope check: 3 words; this remains task-sized." text))
      (should (string-match-p "Quality risk changes: introduced - Tighten owner; resolved - Add due date" text))
      (should (string-match-p "Quality gaps removed: - Add due date" text))
      (should (string-match-p "Restore Previous Action Draft \\[available\\]" text)))))

(ert-deftest delib-flow-filing-preview-renders-evidence-review-for-selected-action ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\n- Draft kickoff follow-up\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (integrated (delib-flow--run-stage-locally drafted 'integrate-into-source))
           (selected-run (delib-flow-test--set-filing-selection integrated "1"))
           (candidate (car (plist-get (plist-get selected-run :filing) :draft-items)))
           (current (plist-put (copy-tree candidate) :text "Draft kickoff follow-up today"))
           (selected-run
            (delib-flow--set-artifact-family-state
             selected-run
             'actions
             (list :candidates (list candidate)
                   :selected-candidate-id (delib-flow--artifact-candidate-id candidate)
                   :selected-draft current
                   :draft-history nil
                   :selected-support-candidates nil
                   :selected-support-context nil)))
           (text (delib-flow--render-filing-preview-section selected-run)))
      (should (string-match-p "\\*\\*\\*\\* Evidence and quality review" text))
      (should (string-match-p "Why this draft exists" text))
      (should (string-match-p "Draft reason:" text))
      (should (string-match-p "Selected candidate origin: directly from the current source" text))
      (should (string-match-p "Source lines that informed it" text))
      (should (string-match-p "Source excerpts" text))
      (should (string-match-p "L1: - Draft kickoff follow-up" text))
      (should (string-match-p "Draft kickoff follow-up today" text))
      (should (string-match-p "No focused support is attached yet" text))
      (should (string-match-p "No focused support context has been captured for this draft yet" text)))))

(ert-deftest delib-flow-filing-preview-action-lines-are-dispatchable ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer selected-run)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "*** Filing actions")
            (search-forward "- Select Approved Filing Actions [available]")
            (goto-char (line-beginning-position))
            (let ((action (get-text-property (point) 'delib-flow-action)))
              (should action)
              (should (eq 'select-approved-filing-actions
                          (plist-get action :id)))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest delib-flow-filing-preview-context-menu-at-selected-workspace-prefers-local-palette ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (selected-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer selected-run)))
      (unwind-protect
          (with-current-buffer buffer
            (setq delib-flow--active-run selected-run)
            (goto-char (point-min))
            (search-forward "**** Local action palette: action")
            (forward-line 2)
            (let* ((entries (delib-flow--context-menu-entries-for-current-point
                             selected-run))
                   (labels (mapcar (lambda (entry) (plist-get entry :label))
                                   entries))
                   (summary (delib-flow--local-actions-summary-text
                             selected-run
                             "Filing preview")))
              (should (equal "Draft Selected Action" (car labels)))
              (should (member "Peek Selected Draft" labels))
              (should (member "Choose Filing Artifact" labels))
              (should (string-match-p "Draft Selected Action" summary))
              (should (string-match-p "Choose Filing Artifact" summary))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (setq delib-flow--active-run nil)))))

(ert-deftest delib-flow-filing-selection-validation-rejects-multiple-indexes ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alpha Project kickoff"
                     :content "* Alpha Project kickoff\nAgenda\n")))
         (filing
          (list :draft-items
                (list
                 (list :kind 'next-action
                       :text "Send budget update to Alice"
                       :warnings nil)
                 (list :kind 'next-action
                       :text "Book review call with vendor"
                       :warnings nil))))
         (prepared
          (delib-flow--seed-filing-selection-block
           (plist-put run :filing filing))))
    (should-error
     (delib-flow--validate-filing-selection-entry
      (delib-flow-test--set-filing-selection prepared "1, 2"))
     :type 'error)))

(ert-deftest delib-flow-reject-draft-filing-artifact-command-rerenders-preview ()
  (delib-flow-test--with-temp-zk-root
      '(("alpha.org" . "#+title: Alpha Project Notes\nKickoff agenda and blockers.\n")
        ("gamma.org" . "#+title: Alpha Constraints\nProject constraint detail.\n"))
    (delib-flow-test--with-temp-project-file
        "* Alpha Project\n"
      (let* ((run (delib-flow--initialize-run
                   (list :title "Alpha Project kickoff"
                         :content "* Alpha Project kickoff\nBody line\n")))
             (inspected (delib-flow-test--accept-inspect
                         (delib-flow--run-stage-locally run 'inspect-source)))
             (matched (delib-flow-test--accept-match
                       (delib-flow--run-stage-locally inspected 'match-project)))
             (filtered
              (delib-flow--run-stage-locally
               (delib-flow--run-stage-locally matched
                                              'discover-reference-material)
               'filter-reference-material))
             (drafted (delib-flow--run-stage-locally filtered 'extract-actions))
             (delib-flow--active-run
              (delib-flow-test--set-filing-selection
               (delib-flow--run-stage-locally drafted 'integrate-into-source)
               "1"
               "Reject this placeholder."))
             (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (setq-local delib-flow--active-run-buffer t))
              (delib-flow-action-reject-draft-filing-artifact)
              (with-current-buffer (get-buffer delib-flow-control-buffer-name)
                (goto-char (point-min))
                (should (search-forward "** Reject Draft Filing Artifact" nil t))
                (should (search-forward "Rejected artifact count:" nil t))
                (should (search-forward "Operator selection: 1" nil t))
                (goto-char (point-min))
                (should (search-forward "** Rejected artifacts" nil t))
                (should (search-forward "Write follow-up note for Alpha Project kickoff" nil t))
                (goto-char (point-min))
                (should (search-forward "- Select Approved Filing Actions [available]" nil t))))
          (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
            (kill-buffer (get-buffer delib-flow-control-buffer-name))))))))

(ert-deftest delib-flow-select-approved-filing-actions-command-rerenders-preview ()
  (delib-flow-test--with-temp-project-file
      "* Alpha Project\n"
    (let* ((run (delib-flow--initialize-run
                 (list :title "Alpha Project kickoff"
                       :content "* Alpha Project kickoff\nBody line\n")))
           (inspected (delib-flow--run-stage-locally run 'inspect-source))
           (matched (delib-flow--run-stage-locally inspected 'match-project))
           (drafted (delib-flow--run-stage-locally matched 'extract-actions))
           (delib-flow--active-run
            (delib-flow-test--set-filing-selection
             (delib-flow--run-stage-locally drafted 'integrate-into-source)
             "1"))
           (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (setq-local delib-flow--active-run-buffer t))
            (delib-flow-action-draft-selected-action)
            (delib-flow-action-select-approved-filing-actions)
            (with-current-buffer (get-buffer delib-flow-control-buffer-name)
              (goto-char (point-min))
              (should (search-forward "** Select Approved Filing Actions" nil t))
              (should (search-forward "Selected artifact count:" nil t))
              (should (search-forward "Operator selection: 1" nil t))
              (should (search-forward "Planned file targets:" nil t))
              (should (search-forward delib-flow-my-projects-file nil t))
              (goto-char (point-min))
              (should (search-forward "*** Selected action workspace" nil t))
              (should (search-forward "**** Selected item" nil t))
              (should (search-forward "Write follow-up note for Alpha Project kickoff" nil t))
              (goto-char (point-min))
              (should (search-forward "**** Planned target or consequence" nil t))
              (should (search-forward "If you file now, delib-flow will stage the approved artifact into these targets." nil t))
              (should (search-forward delib-flow-my-projects-file nil t))
              (goto-char (point-min))
              (should (search-forward "- File Approved Outputs [available]" nil t))))
        (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
          (kill-buffer (get-buffer delib-flow-control-buffer-name)))))))

(ert-deftest delib-flow-select-approved-filing-actions-preview-explains-follow-up-when-drafts-remain ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alpha Project kickoff"
                     :content "* Alpha Project kickoff\nAgenda\n")))
         (filing
          (list :draft-items
                (list
                 (list :kind 'next-action
                       :text "Send budget update to Alice"
                       :warnings nil)
                 (list :kind 'next-action
                       :text "Book review call with vendor"
                       :warnings nil))))
         (prepared
          (delib-flow--seed-filing-selection-block
           (plist-put run :filing filing)))
         (selected
          (delib-flow-test--approve-selected-filing-item
           (delib-flow-test--set-filing-selection prepared "1")))
         (buffer (delib-flow--render-control-buffer selected)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "Review the planned targets, file the approved artifact, or approve another ready item (1)." nil t))
          (goto-char (point-min))
          (should (search-forward "Run `Approve Another Filing Artifact` to approve another ready queue item, or run `File Approved Outputs` to stage the approved item now." nil t)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest delib-flow-filing-selection-rerender-anchors-to-artifact-selection ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Alpha Project kickoff"
                     :content "* Alpha Project kickoff\nAgenda\n")))
         (filing
          (list :draft-items
                (list
                 (list :kind 'next-action
                       :text "Send budget update to Alice"
                       :warnings nil)
                 (list :kind 'next-action
                       :text "Book review call with vendor"
                       :warnings nil))))
         (delib-flow--active-run
          (delib-flow--seed-filing-selection-block
           (plist-put run :filing filing)))
         (buffer (delib-flow--render-control-buffer delib-flow--active-run)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local delib-flow--active-run-buffer t)
            (goto-char (point-min))
            (search-forward "*** Artifact selection")
            (org-back-to-heading t))
          (delib-flow--rerender-active-run-buffer)
          (with-current-buffer (get-buffer delib-flow-control-buffer-name)
            (org-back-to-heading t)
            (should (equal "Artifact selection"
                           (org-get-heading t t t t)))))
      (when (buffer-live-p (get-buffer delib-flow-control-buffer-name))
        (kill-buffer (get-buffer delib-flow-control-buffer-name))))))

(ert-deftest delib-flow-filing-preview-renders-note-regeneration-guidance ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Source"
                     :content "* Source\nBody line\n")))
         (notes (list (list :kind 'reference-note
                            :text "Create general PKM note for Project Atlas Pattern"
                            :note-type 'general-pkm
                            :draft-stage 'suggest-reference-notes)))
         (run (plist-put
               run :filing
               (plist-put (plist-get run :filing) :draft-items notes)))
         (text (delib-flow--render-filing-preview-section run)))
    (should (string-match-p "Focused filing workspace" text))
    (should (string-match-p "Current blocked state" text))
    (should (string-match-p "press `F` to open the focused filing workspace" text))))

(ert-deftest delib-flow-filing-preview-renders-focused-support-in-selected-note-workspace ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body
                                  "* Working draft\nAtlas draft body.\n"))
         (support (list :title "Atlas brief"
                        :support-score 8
                        :support-reasons '("title-overlap" "focus-overlap")))
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
                      :selected-draft drafted-item
                      :available-support-candidates (list support)
                      :available-support-context "- Atlas brief - Constraint summary"
                      :selected-support-candidates (list support)
                      :selected-support-context "- Atlas brief - Constraint summary"))))
         (selected (delib-flow-test--set-filing-selection run "1"))
         (text (delib-flow--render-filing-preview-section selected)))
    (should (string-match-p "Focused filing workspace" text))
    (should (string-match-p "Working draft: present" text))
    (should (string-match-p "Attached support: 1" text))
    (should (string-match-p "Atlas brief" text))
    (should (string-match-p "press `F` to open the focused filing workspace" text))))

(ert-deftest delib-flow-filing-preview-focuses-selected-note-draft ()
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
                 (list :draft-items
                       (list candidate
                             (list :kind 'reference-note
                                   :text "Create general PKM note for Team Ritual Prompt"
                                   :note-type 'general-pkm
                                   :draft-stage 'suggest-reference-notes))
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
         (selected (delib-flow-test--set-filing-selection run "1"))
         (text (delib-flow--render-filing-preview-section selected)))
    (should (string-match-p "Focused filing workspace" text))
    (should (string-match-p "Filing state" text))
    (should (string-match-p "Current blocked state" text))
    (should-not (string-match-p "\\*\\*\\* Filing actions" text))
    (should-not (string-match-p "\\*\\*\\* Reference note capture" text))
    (should-not (string-match-p "\\*\\*\\* Artifact selection" text))
    (should (string-match-p "Atlas draft body" text))
    (should (string-match-p "press `F` to open the focused filing workspace" text))))

(ert-deftest delib-flow-filing-workspace-renders-reference-note-compare-summary ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Prompt handoff packets"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (previous (plist-put (copy-tree candidate)
                              :draft-body
                              (string-join
                               '("* Working draft"
                                 "Previous summary."
                                 "- Durable claim: Previous claim."
                                 "- Why it matters: Previous reason."
                                 "- Reuse angle: Previous reuse."
                                 ""
                                 "* Source highlights"
                                 "- Earlier excerpt."
                                 ""
                                 "* Related material to connect"
                                 "- Earlier related item.")
                               "\n")))
         (current (plist-put (copy-tree candidate)
                             :draft-body
                             (string-join
                              '("* Working draft"
                                "Current summary."
                                "- Durable claim: Current claim."
                                "- Why it matters: Current reason."
                                "- Reuse angle: Previous reuse."
                                ""
                                "* Source highlights"
                                "- Current excerpt."
                                ""
                                "* Related material to connect"
                                "- Earlier related item.")
                              "\n")))
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
                      :selected-draft current
                      :draft-history (list previous)))))
         (text (delib-flow--render-selected-note-filing-workspace
                (delib-flow-test--set-filing-selection run "1"))))
    (should (string-match-p "Changed since previous revision: draft body, source highlights\\." text))))

(ert-deftest delib-flow-filing-workspace-renders-source-evidence-card-and-grounded-highlights ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Friction logs for AI systems"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (drafted-item
          (plist-put
           (copy-tree candidate)
           :draft-body
           (string-join
            '("* Working draft"
              "Atlas draft body."
              "- Durable claim: Capture friction logs, not just answers."
              "- Why it matters: Friction logs turn annoyances into improvements."
              "- Reuse angle: Keep this reuse angle."
              ""
              "* Source highlights"
              "- Capture friction logs, not just answers."
              ""
              "* Related material to connect"
              "- Keep this related material.")
            "\n")))
         (base-run
          (plist-put
           (delib-flow--initialize-run
            (list :title "Source"
                  :content
                  (string-join
                   '("* Source"
                     "Capture friction logs, not just answers."
                     "Friction logs turn one-off annoyances into reusable system improvements.")
                   "\n")))
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
         (run
          (delib-flow--seed-filing-selection-block
           (delib-flow--set-artifact-family-state
            base-run
            'reference-notes
            (list :candidates (list candidate)
                  :selected-candidate-id
                  (delib-flow--artifact-candidate-id candidate)
                  :selected-draft drafted-item))))
         (text
          (delib-flow--render-selected-note-filing-workspace
           (delib-flow-test--set-filing-selection run "1"))))
    (should (string-match-p "\\*\\*\\* Source evidence" text))
    (should (string-match-p "Keep these cleaned source lines in view while composing the note" text))
    (should (string-match-p "Capture friction logs, not just answers" text))
    (should (string-match-p "Press `O` to open the full cleaned source" text))
    (should (string-match-p "Note line: Capture friction logs, not just answers" text))
    (should (string-match-p "Source line: L1: Capture friction logs, not just answers" text))))

(ert-deftest delib-flow-filing-workspace-renders-compact-support-suggestions ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Advisor briefs as reusable interfaces"
                          :note-type 'general-pkm
                          :draft-stage 'suggest-reference-notes))
         (drafted-item (plist-put (copy-tree candidate)
                                  :draft-body "* Working draft\nAtlas draft body.\n"))
         (support-a (list :title "Alpha note" :support-score 9))
         (support-b (list :title "Beta note" :support-score 7))
         (support-c (list :title "Gamma note" :support-score 5))
         (support-d (list :title "Low confidence note" :support-score 1))
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
                      :selected-draft drafted-item
                      :available-support-candidates
                      (list support-d support-b support-c support-a)))))
         (text (delib-flow--render-selected-note-filing-workspace
                (delib-flow-test--set-filing-selection run "1"))))
    (should (string-match-p "4 suggestions ready\\. Use `Choose Support for This Draft`" text))
    (should (string-match-p "Alpha note" text))
    (should (string-match-p "Beta note" text))
    (should (string-match-p "Gamma note" text))
    (should-not (string-match-p "Low confidence note" text))))

(ert-deftest delib-flow-filing-workspace-uses-local-reassigned-shortcuts ()
  (let* ((run (delib-flow-test--reference-note-part-editor-run))
         (actions (delib-flow--focused-reference-note-workspace-actions run)))
    (should (equal "1" (plist-get (car actions) :shortcut)))
    (should (eq 'draft-selected-reference-note
                (plist-get (car actions) :id)))
    (should (eq 'edit-selected-reference-note-title
                (plist-get (cadr actions) :id)))))

(ert-deftest delib-flow-filing-workspace-enables-seeded-note-local-controls ()
  (let* ((run (delib-flow-test--reference-note-candidate-only-run))
         (actions (delib-flow--focused-reference-note-workspace-actions run))
         (ids (mapcar (lambda (action) (plist-get action :id)) actions)))
    (should (equal "1" (plist-get (car actions) :shortcut)))
    (should (memq 'draft-selected-reference-note ids))
    (should (memq 'edit-selected-reference-note-title ids))
    (should (memq 'edit-selected-reference-note-draft-body ids))
    (should (memq 'refresh-selected-reference-note-source-highlights ids))
    (should (memq 'choose-reference-note-template ids))
    (should (memq 'edit-selected-reference-note-target-path ids))))

(ert-deftest delib-flow-filing-workspace-renders-draft-map-for-seeded-note ()
  (let ((text (delib-flow--render-selected-note-filing-workspace
               (delib-flow-test--reference-note-candidate-only-run))))
    (should (string-match-p "\\*\\*\\* Draft map" text))
    (should (string-match-p "- Title \\[manual\\]: `2`" text))
    (should (string-match-p "- Draft body \\[seeded\\]: `3` / `4`" text))
    (should (string-match-p "- Whole note \\[generated\\]: `1`" text))
    (should (string-match-p "- Reuse angle \\[present\\]: `0` / `b`" text))))

(ert-deftest delib-flow-filing-workspace-compact-helper-texts-render ()
  (let* ((run (delib-flow-test--reference-note-candidate-only-run))
         (action (car (delib-flow--focused-reference-note-workspace-actions run)))
         (compact-line (delib-flow--format-compact-action-line action))
         (workspace-line
          (delib-flow--format-reference-note-workspace-action-line action))
         (summary (delib-flow--reference-note-workspace-summary-text run))
         (support (delib-flow--reference-note-compact-support-suggestions-text run))
         (history (delib-flow--reference-note-compact-history-text run))
         (target (delib-flow--reference-note-compact-target-text run)))
    (should (string-match-p "^\\- \\[1\\] Draft Selected Note$" compact-line))
    (should (string-match-p "^\\- \\[1\\] Regenerate Selected Note$" workspace-line))
    (should (string-match-p "State: seeded | attached support 0 | suggestions 0" summary))
    (should (string-match-p "Save: save default org-roam target via unconfigured | Next: refine a part or attach support" summary))
    (should (string-match-p "No support suggestions yet" support))
    (should (string-match-p "No earlier saved revision yet\\. Regenerate once to unlock compare" history))
    (should (string-match-p "Title: Project Atlas Pattern" target))
    (should (string-match-p "Template: unconfigured" target))))

(ert-deftest delib-flow-filing-workspace-compact-helper-branches-render ()
  (let* ((candidate-a (list :kind 'reference-note
                            :text "Create general PKM note for Project Atlas Pattern"
                            :note-type 'general-pkm
                            :draft-stage 'suggest-reference-notes))
         (candidate-b (list :kind 'reference-note
                            :text "Create general PKM note for Advisor briefs as reusable interfaces"
                            :note-type 'general-pkm
                            :draft-stage 'suggest-reference-notes))
         (drafted-item (plist-put (copy-tree candidate-a)
                                  :draft-body "* Working draft\nAtlas draft body.\n"))
         (support-a (list :title "Alpha note" :support-score 9))
         (run (delib-flow--seed-filing-selection-block
               (delib-flow--set-artifact-family-state
                (plist-put
                 (delib-flow--initialize-run
                  (list :title "Source"
                        :content "* Source\nBody line\n"))
                 :filing
                 (list :draft-items (list candidate-a candidate-b)
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
                (list :candidates (list candidate-a candidate-b)
                      :selected-candidate-id
                      (delib-flow--artifact-candidate-id candidate-a)
                      :selected-draft drafted-item
                      :available-support-candidates (list support-a)
                      :selected-support-candidates (list support-a)))))
         (summary (delib-flow--reference-note-workspace-summary-text run))
         (other (delib-flow--reference-note-compact-other-candidates-text run)))
    (should (string-match-p "State: drafted | attached support 1 | suggestions 0" summary))
    (should (string-match-p "Next: refine parts, then save or discard" summary))
    (should (string-match-p "2 note candidates ready" other))
    (should (string-match-p "Press `s` to choose a different note" other))
    (should (string-match-p "Advisor briefs as reusable inte" other))))

(provide 'delib-flow-filing-test)

;;; delib-flow-filing-test.el ends here
