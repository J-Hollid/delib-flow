;;; delib-flow-services-test.el --- Service tests for delib-flow -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'delib-flow)

(defmacro delib-flow-services-test--with-temp-org-file (content &rest body)
  "Run BODY with a temporary Org file containing CONTENT."
  (declare (indent 1))
  `(let ((file (make-temp-file "delib-flow-org" nil ".org" ,content)))
     (unwind-protect
         (let ((buffer (find-file-noselect file)))
           (with-current-buffer buffer
             (org-mode)
             ,@body))
       (when-let ((buffer (get-file-buffer file)))
         (kill-buffer buffer))
       (when (file-exists-p file)
         (delete-file file)))))

(defmacro delib-flow-services-test--with-temp-project-file (content &rest body)
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

(defmacro delib-flow-services-test--with-temp-zk-root (files &rest body)
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

(defun delib-flow-services-test--accept-inspect (run)
  "Return RUN with inspect-source accepted and actions reseeded."
  (delib-flow--seed-actions
   (delib-flow--apply-inspect-review-outcome
    run
    'accepted
    "Inspect result accepted. You may now match the project or retry inspect.")))

(defun delib-flow-services-test--accept-match (run)
  "Return RUN with match-project accepted and actions reseeded."
  (delib-flow--seed-actions
   (delib-flow--apply-match-review-outcome
    run
    'accepted
    "Project match accepted. Continue with manual override or downstream stages as appropriate.")))

(ert-deftest delib-flow-execute-inspect-source-classifies-email-sources ()
  (let* ((source (list :title "Re: Alpha Project update"
                       :outline-path '("Inbox")
                       :content "* Re: Alpha Project update\nFrom: Alice Example <alice@example.com>\nTo: Bob Example <bob@example.com>\nSubject: Re: Alpha Project update\nDate: 2026-04-27\n\nQuick status update.\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'email (plist-get raw-output :source-type)))
    (should (equal 2 (plist-get raw-output :contact-email-count)))
    (should (string-match-p "email-style headers"
                            (plist-get raw-output :source-type-reason)))))

(ert-deftest delib-flow-execute-inspect-source-filters-low-value-email-addresses ()
  (let* ((source (list :title "Your Consumption Diet Is Your Moat"
                       :outline-path '("Inbox")
                       :content (concat
                                 "* Your Consumption Diet Is Your Moat :email:\n"
                                 ":PROPERTIES:\n"
                                 ":FROM: Forte Labs Newsletter <hello@fortelabs.com>\n"
                                 ":END:\n\n"
                                 ":RAW_EMAIL:\n"
                                 "From: Forte Labs Newsletter <hello@fortelabs.com>\n"
                                 "To: me@example.com\n"
                                 "Reply-To: hello@fortelabs.com\n"
                                 "List-Unsubscribe: <mailto:unsub+x@example.com>\n"
                                 "X-Report-Abuse: <abuse@kit.com>\n"
                                 "Return-Path: bounces@ckespa.fortelabs.com\n\n"
                                 "Newsletter body.\n"
                                 ":END:\n")))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (equal '("hello@fortelabs.com")
                   (plist-get raw-output :contact-emails)))))

(ert-deftest delib-flow-email-inspect-digest-reduces-imported-mail-noise ()
  (let* ((source
          (list :title "Feel The Snow from your Steam wishlist is now on sale!"
                :content
                (concat
                 "* Feel The Snow from your Steam wishlist is now on sale! :email:\n"
                 ":PROPERTIES:\n"
                 ":FROM: Steam <noreply@steampowered.com>\n"
                 ":DATE: Sat, 02 May 2026 10:36:13 -0700\n"
                 ":EMAIL_FILE: /tmp/steam-mail.txt\n"
                 ":END:\n\n"
                 ":RAW_EMAIL:\n"
                 "From: Steam <noreply@steampowered.com>\n"
                 "To: user@example.com\n"
                 "Subject: Feel The Snow from your Steam wishlist is now on sale!\n"
                 "Content-Type: multipart/alternative; boundary=\"abc\"\n\n"
                 "--abc\n"
                 "Content-Type: text/plain; charset=UTF-8; format=flowed\n"
                 "Content-Transfer-Encoding: quoted-printable\n\n"
                 "=0AHello CyanDevil!\n"
                 "The following items on your wishlist are on sale:\n"
                 "Feel The Snow - 90% off!=0Ahttps://store.steampowered.com/app/538100/Feel_The_Snow/\n"
                 "This email message was auto-generated. Please do not respond.\n"
                 "If you need additional help, please visit Steam Support.\n"
                 "--abc\n"
                 "Content-Type: text/html; charset=UTF-8\n\n"
                 "<html><body>Huge html body</body></html>\n"
                 "--abc\n"
                 ":END:\n")))
         (digest (delib-flow--email-inspect-digest source)))
    (should (equal "Steam <noreply@steampowered.com>" (plist-get digest :from)))
    (should (equal "transactional notification" (plist-get digest :type-hint)))
    (should (string-match-p "Hello CyanDevil!" (plist-get digest :plain-body)))
    (should (string-match-p "wishlist are on sale" (plist-get digest :plain-body)))
    (should-not (string-match-p "Huge html body" (plist-get digest :plain-body)))
    (should-not (string-match-p "auto-generated" (plist-get digest :plain-body)))
    (should (member "https://store.steampowered.com/app/538100/Feel_The_Snow/"
                    (plist-get digest :links)))))

(ert-deftest delib-flow-execute-inspect-source-caches-reduced-email-digest-off-buffer ()
  (let* ((source (list :title "Your Consumption Diet Is Your Moat"
                       :outline-path '("Inbox")
                       :content (concat
                                 "* Your Consumption Diet Is Your Moat :email:\n"
                                 ":PROPERTIES:\n"
                                 ":FROM: Forte Labs Newsletter <hello@fortelabs.com>\n"
                                 ":END:\n\n"
                                 ":RAW_EMAIL:\n"
                                 "From: Forte Labs Newsletter <hello@fortelabs.com>\n"
                                 "Subject: Your Consumption Diet Is Your Moat\n\n"
                                 "Week 1 was about the Master Prompt, PARA adapted for the AI era, and a capture system.\n"
                                 ":END:\n")))
         (run (delib-flow--run-stage-locally
               (delib-flow--initialize-run source)
               'inspect-source))
         (working (delib-flow--run-working-context run))
         (digest (plist-get working :email-inspect-digest))
         (inspect-output (plist-get working :inspect-output)))
    (should digest)
    (should (equal "Your Consumption Diet Is Your Moat"
                   (plist-get digest :subject)))
    (should (string-match-p "Master Prompt"
                            (plist-get digest :plain-body)))
    (should-not (plist-member inspect-output :plain-body))
    (should-not (plist-member inspect-output :ignored-noise))
    (should (equal digest (delib-flow--package-email-digest run)))))

(ert-deftest delib-flow-execute-inspect-source-classifies-meeting-note-sources ()
  (let* ((source (list :title "Team sync"
                       :outline-path '("Journal" "2026" "Week 17")
                       :content "* Team sync\nAttendees: Alice, Bob\nAgenda:\n- Review blockers\nNotes:\n- Need follow-up\nNext steps:\n- Send update\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'meeting-note (plist-get raw-output :source-type)))
    (should (= 7 (plist-get raw-output :body-line-count)))
    (should (member "journal outline path"
                    (plist-get raw-output :source-type-signals)))))

(ert-deftest delib-flow-execute-inspect-source-classifies-reminder-sources ()
  (let* ((source (list :title "Reminder: follow up with vendor"
                       :outline-path '("Inbox")
                       :content "* Reminder: follow up with vendor\nRemember to ask for the revised timeline tomorrow.\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'reminder (plist-get raw-output :source-type)))
    (should (string-match-p "reminder or follow-up wording"
                            (plist-get raw-output :source-type-reason)))))

(ert-deftest delib-flow-execute-inspect-source-classifies-fleeting-note-sources ()
  (let* ((source (list :title "Idea: orders system for commanders"
                       :outline-path '("Inbox")
                       :content "* Idea: orders system for commanders\nMaybe structure command intent around a lightweight doctrine card.\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'fleeting-note (plist-get raw-output :source-type)))
    (should (string-match-p "ideas, thoughts, or open questions"
                            (plist-get raw-output :source-type-reason)))))

(ert-deftest delib-flow-execute-inspect-source-classifies-issue-note-sources ()
  (let* ((source (list :title "Broken steno exercise"
                       :outline-path '("Inbox")
                       :content "* Broken steno exercise\nhttps://example.com/drill?id=one\nhttps://example.com/drill?id=two\n"))
         (raw-output (delib-flow--execute-inspect-source
                      (list :source source
                            :ui (list :operator-intent "this is about fixing a broken website")))))
    (should (eq 'issue-note (plist-get raw-output :source-type)))
    (should-not (member "short question-style body"
                        (plist-get raw-output :source-type-signals)))
    (should (plist-get raw-output :operator-intent-present))
    (should-not (plist-get raw-output :operator-intent-influenced))))

(ert-deftest delib-flow-execute-inspect-source-classifies-timestamped-broken-link-capture-as-issue-note ()
  (let* ((source (list :title "[2026-02-10 Tue 17:52] Broken steno exercise"
                       :outline-path '("Inbox" "[2026-02-10 Tue 17:52] Broken steno exercise")
                       :content
                       (concat
                        "** [2026-02-10 Tue 17:52] Broken steno exercise\n"
                        "https://joshuagrams.github.io/steno-jig/finger-drills.html?iterations=20&hints=fail&live_wpm=1&show_timer=1&section=15&drill=13\n"
                        "https://joshuagrams.github.io/steno-jig/finger-drills.html?iterations=20&hints=fail&live_wpm=1&show_timer=1&section=16&drill=2\n")))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'issue-note (plist-get raw-output :source-type)))
    (should (string-match-p "problem-to-fix capture"
                            (plist-get raw-output :source-type-reason)))
    (should-not (member "short question-style body"
                        (plist-get raw-output :source-type-signals)))))

(ert-deftest delib-flow-execute-inspect-source-keeps-unknown-when-evidence-is-weak ()
  (let* ((source (list :title "Random note"
                       :outline-path '("Inbox")
                       :content "* Random note\nNeed to think about this later.\n"))
         (raw-output (delib-flow--execute-inspect-source (list :source source))))
    (should (eq 'unknown (plist-get raw-output :source-type)))
    (should (string-match-p "too weak"
                            (plist-get raw-output :source-type-reason)))))

(ert-deftest delib-flow-project-candidates-ignore-state-bucket-headings ()
  (delib-flow-services-test--with-temp-project-file
      "* Active\n** Alpha Project\nContact alice@example.com\n* Complete\n** Old Project\n* Waiting\n** Beta Project\n"
    (let ((titles
           (mapcar (lambda (candidate)
                     (plist-get candidate :title))
                   (delib-flow--project-candidates-from-file
                    delib-flow-my-projects-file))))
      (should (equal '("Alpha Project" "Old Project" "Beta Project")
                     titles)))))

(ert-deftest delib-flow-project-candidates-ignore-nested-project-todos ()
  (delib-flow-services-test--with-temp-project-file
      "* Active\n** Alpha Project\n*** TODO Draft kickoff follow-up\n*** WAITING Vendor approval\n* Waiting\n** Beta Project\n*** TODO Schedule review\n"
    (let ((titles
           (mapcar (lambda (candidate)
                     (plist-get candidate :title))
                   (delib-flow--project-candidates-from-file
                    delib-flow-my-projects-file))))
      (should (equal '("Alpha Project" "Beta Project")
                     titles)))))

(ert-deftest delib-flow-project-candidates-ignore-nested-child-terms ()
  (delib-flow-services-test--with-temp-project-file
      "* Active\n** Project Atlas :example:product:\nCore project concept.\n*** TODO Write follow-up note for [2026-02-24 Tue 22:37] Project Atlas\n"
    (let* ((candidate (car (delib-flow--project-candidates-from-file
                            delib-flow-my-projects-file)))
           (terms (plist-get candidate :terms)))
      (should (member "project" terms))
      (should (member "atlas" terms))
      (should-not (member "follow" terms))
      (should-not (member "2026" terms))
      (should-not (member "todo" terms)))))

(ert-deftest delib-flow-inbox-heading-snapshots-ignore-nested-headings ()
  (delib-flow-services-test--with-temp-org-file
      "* First item\nBody\n** Nested child\nNested body\n* Second item\nMore body\n"
    (let ((snapshots (delib-flow--inbox-heading-snapshots
                      (buffer-file-name))))
      (should (equal '("First item" "Second item")
                     (mapcar (lambda (snapshot)
                               (plist-get snapshot :title))
                             snapshots))))))

(ert-deftest delib-flow-inbox-heading-snapshots-can-read-configured-outline-path ()
  (delib-flow-services-test--with-temp-org-file
      "* Inbox\n** First queued item\nBody\n** Second queued item\nMore body\n* Elsewhere\n** Ignored child\n"
    (let ((snapshots (delib-flow--inbox-heading-snapshots
                      (buffer-file-name)
                      '("Inbox"))))
      (should (equal '("First queued item" "Second queued item")
                     (mapcar (lambda (snapshot)
                               (plist-get snapshot :title))
                             snapshots))))))

(ert-deftest delib-flow-inbox-heading-snapshots-do-not-mark-inbox-buffer-modified ()
  (delib-flow-services-test--with-temp-org-file
      "* Inbox\n** First queued item\nBody\n** Second queued item\nMore body\n"
    (let ((buffer (find-file-noselect (buffer-file-name))))
      (unwind-protect
          (with-current-buffer buffer
            (set-buffer-modified-p nil)
            (delib-flow--inbox-heading-snapshots (buffer-file-name) '("Inbox"))
            (should-not (buffer-modified-p)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer))))))

(ert-deftest delib-flow-inbox-and-org-entry-helpers-handle-paths-and-bodies ()
  (let ((delib-flow-inbox-file "~/inbox.org"))
    (should (string-match-p "/inbox\\.org\\'"
                            (delib-flow--configured-inbox-file))))
  (let ((delib-flow-inbox-file ""))
    (should-not (delib-flow--configured-inbox-file)))
  (with-temp-buffer
    (org-mode)
    (insert "* Parent\nBody line one.\nBody line two.\n** Child\nChild body.\n")
    (goto-char (point-min))
    (should (equal "Body line one.\nBody line two.\n** Child\nChild body."
                   (delib-flow--org-entry-body-at-point)))
    (should (equal '("Parent")
                   (delib-flow--inbox-outline-path-at-point)))
    (should (delib-flow--goto-inbox-outline-path '("Parent" "Child")))
    (should (equal '("Parent" "Child")
                   (delib-flow--inbox-outline-path-at-point)))
    (should-not (delib-flow--goto-inbox-outline-path '("Missing")))))

(ert-deftest delib-flow-inbox-selection-labels-number-snapshots-and-fill-missing-title ()
  (let* ((snapshots (list (list :title "First")
                          (list :title nil)))
         (labels (delib-flow--inbox-selection-labels snapshots)))
    (should (equal "[1] First" (caar labels)))
    (should (equal "[2] Untitled heading" (car (cadr labels))))))

(ert-deftest delib-flow-discover-reference-material-excludes-my-projects-file ()
  (let ((root (make-temp-file "delib-flow-zk" t))
        project-file
        note-file)
    (unwind-protect
        (progn
          (setq project-file (expand-file-name "my-projects.org" root))
          (setq note-file (expand-file-name "atlas-notes.org" root))
          (with-temp-file project-file
            (insert "* Project Atlas\n** Notes\n"))
          (with-temp-file note-file
            (insert "#+title: Project Atlas Support\nUseful support context.\n"))
          (let ((delib-flow-zk-root root)
                (delib-flow-my-projects-file project-file))
            (let* ((run (delib-flow--initialize-run
                         (list :title "Project Atlas"
                               :content "* Project Atlas\nUseful support context.\n")))
                   (inspected (delib-flow-services-test--accept-inspect
                               (delib-flow--run-stage-locally run 'inspect-source)))
                   (matched (delib-flow-services-test--accept-match
                             (delib-flow--run-stage-locally inspected 'match-project)))
                   (updated-run
                    (delib-flow--run-stage-locally matched
                                                   'discover-reference-material))
                   (retrieved
                    (plist-get (delib-flow--run-working-context updated-run)
                               :retrieved-candidates))
                   (titles (mapcar (lambda (candidate)
                                     (plist-get candidate :title))
                                   retrieved)))
              (should (member "Project Atlas Support" titles))
              (should-not (member "my-projects" titles)))))
      (when-let ((buffer (and project-file (get-file-buffer project-file))))
        (kill-buffer buffer))
      (when-let ((buffer (and note-file (get-file-buffer note-file))))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest delib-flow-discover-reference-material-excludes-active-source-file ()
  (let ((root (make-temp-file "delib-flow-zk" t))
        project-file
        source-file
        note-file)
    (unwind-protect
        (progn
          (setq project-file (expand-file-name "my-projects.org" root))
          (setq source-file (expand-file-name "inbox-item.org" root))
          (setq note-file (expand-file-name "atlas-support.org" root))
          (with-temp-file project-file
            (insert "* Project Atlas\n"))
          (with-temp-file source-file
            (insert "* Project Atlas\nUseful support context.\n"))
          (with-temp-file note-file
            (insert "#+title: Project Atlas Support\nUseful support context.\n"))
          (let ((delib-flow-zk-root root)
                (delib-flow-my-projects-file project-file))
            (let* ((run (delib-flow--initialize-run
                         (list :title "Project Atlas"
                               :file source-file
                               :content "* Project Atlas\nUseful support context.\n")))
                   (inspected (delib-flow-services-test--accept-inspect
                               (delib-flow--run-stage-locally run 'inspect-source)))
                   (matched (delib-flow-services-test--accept-match
                             (delib-flow--run-stage-locally inspected 'match-project)))
                   (updated-run
                    (delib-flow--run-stage-locally matched
                                                   'discover-reference-material))
                   (retrieved
                    (plist-get (delib-flow--run-working-context updated-run)
                               :retrieved-candidates))
                   (titles (mapcar (lambda (candidate)
                                     (plist-get candidate :title))
                                   retrieved)))
              (should (member "Project Atlas Support" titles))
              (should-not (member "inbox-item" titles)))))
      (when-let ((buffer (and project-file (get-file-buffer project-file))))
        (kill-buffer buffer))
      (when-let ((buffer (and source-file (get-file-buffer source-file))))
        (kill-buffer buffer))
      (when-let ((buffer (and note-file (get-file-buffer note-file))))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest delib-flow-discover-reference-material-excludes-source-mirror-note ()
  (let ((root (make-temp-file "delib-flow-zk" t))
        project-file
        mirror-file
        support-file)
    (unwind-protect
        (progn
          (setq project-file (expand-file-name "my-projects.org" root))
          (setq mirror-file (expand-file-name "mirror-note.org" root))
          (setq support-file (expand-file-name "support-note.org" root))
          (with-temp-file project-file
            (insert "* Your Consumption Diet Is Your Moat\n"))
          (with-temp-file mirror-file
            (insert "#+title: Your Consumption Diet Is Your Moat (concept)\n\n"
                    "* Working draft\n"
                    "- Source title: Your Consumption Diet Is Your Moat\n"
                    "- Body reuse: durable concept extracted from source\n"))
          (with-temp-file support-file
            (insert "#+title: AI advisor patterns\n"
                    "A quick update on week 2 of the AI Second Brain cohort.\n"
                    "Personal AI advisor framing from hello@fortelabs.com.\n"))
          (let ((delib-flow-zk-root root)
                (delib-flow-my-projects-file project-file))
            (let* ((run (delib-flow--initialize-run
                         (list :title "Your Consumption Diet Is Your Moat"
                               :file "/tmp/inbox-item.org"
                               :content "* Your Consumption Diet Is Your Moat\nA quick update on week 2 of the AI Second Brain cohort.\nFrom: hello@fortelabs.com\n")))
                   (inspected (delib-flow-services-test--accept-inspect
                               (delib-flow--run-stage-locally run 'inspect-source)))
                   (matched (delib-flow-services-test--accept-match
                             (delib-flow--run-stage-locally inspected 'match-project)))
                   (updated-run
                    (delib-flow--run-stage-locally matched
                                                   'discover-reference-material))
                   (retrieved
                    (plist-get (delib-flow--run-working-context updated-run)
                               :retrieved-candidates))
                   (titles (mapcar (lambda (candidate)
                                     (plist-get candidate :title))
                                   retrieved)))
              (should (member "AI advisor patterns" titles))
              (should-not (member "Your Consumption Diet Is Your Moat (concept)"
                                  titles)))))
      (when-let ((buffer (and project-file (get-file-buffer project-file))))
        (kill-buffer buffer))
      (when-let ((buffer (and mirror-file (get-file-buffer mirror-file))))
        (kill-buffer buffer))
      (when-let ((buffer (and support-file (get-file-buffer support-file))))
        (kill-buffer buffer))
      (when (file-directory-p root)
        (delete-directory root t)))))

(ert-deftest delib-flow-focused-support-candidates-prioritize-concrete-over-broad-overlaps ()
  (let* ((selected (list :kind 'reference-note
                         :text "Create general PKM note for Project Atlas Pattern"
                         :note-type 'general-pkm))
         (package (delib-flow--set-artifact-family-state
                   (plist-put
                    (delib-flow--initialize-run
                     (list :title "Source"
                           :content "* Source\nBody line\n"))
                    :working-context
                    (list :retrieved-candidates
                          (list (list :title "Project Atlas Pattern"
                                      :score 1)
                                (list :title "Generic Broad Note"
                                      :score 7))))
                   'reference-notes
                   (list :candidates (list selected)
                         :selected-candidate-id
                         (delib-flow--artifact-candidate-id selected))))
         (candidates
          (delib-flow--focused-support-candidates
           package
           'reference-notes
           selected)))
    (should (equal 1 (length candidates)))
    (should (equal "Project Atlas Pattern"
                   (plist-get (car candidates) :title)))
    (should (member "title-overlap"
                    (plist-get (car candidates) :support-reasons)))))

(ert-deftest delib-flow-reference-note-support-candidates-do-not-fall-back-to-broad-discovery ()
  (let* ((candidate (list :kind 'reference-note
                          :text "Create general PKM note for Project Atlas Pattern"
                          :note-type 'general-pkm))
         (package (delib-flow--set-artifact-family-state
                   (plist-put
                    (delib-flow--initialize-run
                     (list :title "Source"
                           :content "* Source\nBody line\n"))
                    :working-context
                    (list :retrieved-candidates
                          (list (list :title "Broad overlap note"
                                      :support-score 2
                                      :support-reasons '("broad-retrieval-overlap")))))
                   'reference-notes
                   (list :candidates (list candidate)
                         :selected-candidate-id
                         (delib-flow--artifact-candidate-id candidate)))))
    (should-not (delib-flow--reference-note-support-candidates package))))

(ert-deftest delib-flow-decide-cloud-pass-updates-stage-history-and-routing ()
  (let* ((run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run (delib-flow--run-stage-locally inspected
                                                     'decide-cloud-pass))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries))))
         (routing (plist-get updated-run :routing)))
    (should (equal 'decide-cloud-pass (plist-get history :latest-stage)))
    (should (equal 'completed (plist-get history :latest-status)))
    (should (equal 'decide-cloud-pass (plist-get entry :stage-id)))
    (should (plist-get routing :cloud-switch-pending))
    (should (eq 'required
                (plist-get routing :sanitization-status)))
    (should (equal "cloud-model-unconfigured"
                   (plist-get routing :selected-cloud-model)))
    (should (equal "cloud-model-unconfigured"
                   (plist-get routing :selected-cloud-provider)))
    (should (eq 'standard
                (plist-get routing :cloud-policy-profile)))
    (should (eq 'run-cloud-stage
                (plist-get routing :cloud-target-stage)))
    (should (member 'sanitize-for-cloud
                    (mapcar (lambda (action)
                              (plist-get action :id))
                            (plist-get (delib-flow--run-actions updated-run)
                                       :items))))
    (should (string-match-p "Route: cloud"
                            (plist-get entry :normalized-output)))
    (should (string-match-p "pending sanitized package preparation"
                            (plist-get entry :normalized-output)))
    (should (string-match-p "Review cloud-routing decision"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-decide-cloud-pass-fails-when-provider-policy-disables-routing ()
  (let* ((delib-flow-default-cloud-model "blocked:model")
         (delib-flow-cloud-provider-policy-alist
          '(("blocked" :enabled nil :policy-profile strict)
            (default :enabled t :policy-profile standard)))
         (run (delib-flow--initialize-run
               (list :title "Example"
                     :content "* Example\nBody line\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (updated-run (delib-flow--run-stage-locally inspected
                                                     'decide-cloud-pass))
         (history (delib-flow--run-stage-history updated-run))
         (entry (car (last (plist-get history :entries)))))
    (should (equal 'failed (plist-get history :latest-status)))
    (should (equal 'decide-cloud-pass (plist-get history :latest-stage)))
    (should (equal 'failed (plist-get entry :status)))
    (should (string-match-p "Cloud routing is disabled for provider blocked"
                            (plist-get entry :normalized-output)))
    (should (string-match-p "Review stage failure"
                            (plist-get (delib-flow--run-session updated-run)
                                       :current-decision)))))

(ert-deftest delib-flow-sanitize-for-cloud-strict-policy-redacts-links-and-project-terms ()
  (let* ((delib-flow-default-cloud-model "strict:model")
         (delib-flow-cloud-provider-policy-alist
          '(("strict" :enabled t :policy-profile strict)
            (default :enabled t :policy-profile standard)))
         (run (delib-flow--initialize-run
               (list :title "Alpha Project"
                     :content "* Alpha Project\nDiscuss alpha project timeline\nSee [[file:secret.org][secret]]\nContact alice@example.com\nVisit https://example.com\n")))
         (inspected (delib-flow--run-stage-locally run 'inspect-source))
         (matched (plist-put
                   inspected
                   :working-context
                   (plist-put
                    (delib-flow--run-working-context inspected)
                    :project-match
                    (list :best-project (list :title "Alpha Project")))))
         (cloud-decided
          (delib-flow--run-stage-locally matched 'decide-cloud-pass))
         (updated-run
          (delib-flow--run-stage-locally cloud-decided 'sanitize-for-cloud))
         (working (delib-flow--run-working-context updated-run))
         (routing (plist-get updated-run :routing))
         (cloud-context (plist-get working :cloud-sanitized-context)))
    (should (eq 'strict (plist-get routing :cloud-policy-profile)))
    (should (string-match-p "\\[redacted-email\\]" cloud-context))
    (should (string-match-p "\\[redacted-url\\]" cloud-context))
    (should (string-match-p "\\[redacted-link\\]" cloud-context))
    (should (string-match-p "\\[redacted-project\\]" cloud-context))
    (should-not (string-match-p "Alpha" cloud-context))
    (should-not (string-match-p "alpha project timeline" cloud-context))
    (should-not (string-match-p "\\[\\[file:secret\\.org\\]\\[secret\\]\\]"
                                cloud-context))))

(provide 'delib-flow-services-test)
;;; delib-flow-services-test.el ends here
