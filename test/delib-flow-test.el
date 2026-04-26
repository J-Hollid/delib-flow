;;; delib-flow-test.el --- Tests for delib-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'delib-flow)

(defmacro delib-flow-test--with-temp-org (&rest body)
  "Run BODY in a temporary Org buffer."
  `(with-temp-buffer
     (org-mode)
     ,@body))

(ert-deftest delib-flow-snapshot-heading-captures-title-and-content ()
  (delib-flow-test--with-temp-org
   (insert "* Example heading\nSome body text.\n")
   (goto-char (point-min))
   (let ((snapshot (delib-flow--snapshot-heading)))
     (should (equal "Example heading" (plist-get snapshot :title)))
     (should (string-match-p "Some body text" (plist-get snapshot :content))))))

(ert-deftest delib-flow-initialize-run-contains-source-snapshot ()
  (let* ((snapshot (list :title "Example"))
         (run (delib-flow--initialize-run snapshot))
         (working (plist-get run :working-context)))
    (should (equal snapshot (plist-get working :source-snapshot)))
    (should (plist-member run :session-metadata))))

(ert-deftest delib-flow-start-requires-heading-at-point ()
  (delib-flow-test--with-temp-org
   (insert "Not a heading\n")
   (goto-char (point-min))
   (should-error (delib-flow-start))))

(ert-deftest delib-flow-render-control-buffer-contains-required-sections ()
  (let* ((snapshot (list :title "Example"
                         :file "/tmp/example.org"
                         :id "abc"
                         :content "* Example\nBody"))
         (run (delib-flow--initialize-run snapshot))
         (buffer (delib-flow--render-control-buffer run)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'org-mode))
          (dolist (heading '("** Source"
                             "** Working context"
                             "** Stage history"
                             "** Current decision"
                             "** Valid next actions"
                             "** Filing preview"
                             "** Audit status"))
            (goto-char (point-min))
            (should (search-forward heading nil t))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'delib-flow-test)
;;; delib-flow-test.el ends here
