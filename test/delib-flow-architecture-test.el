;;; delib-flow-architecture-test.el --- Architecture tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'delib-flow)

(defun delib-flow-test--batch-require-output (feature)
  "Return subprocess status and output for requiring FEATURE in batch Emacs."
  (with-temp-buffer
    (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
           (status
           (call-process
             emacs-bin nil (current-buffer) nil
             "-Q" "--batch" "-L" "."
             "--eval" (format "(progn (setq load-prefer-newer t) (require '%s))"
                              feature))))
      (list :status status
            :output (buffer-string)))))

(ert-deftest delib-flow-require-exposes-public-entrypoints ()
  (dolist (symbol '(delib-flow-start
                    delib-flow-refresh
                    delib-flow-dispatch-action))
    (should (fboundp symbol))))

(ert-deftest delib-flow-internal-modules-require-cleanly ()
  (dolist (feature '(delib-flow-config
                     delib-flow-model
                     delib-flow-artifacts
                     delib-flow-services
                     delib-flow-stages
                     delib-flow-filing
                     delib-flow-render
                     delib-flow-ui
                     delib-flow-audit
                     delib-flow-debug))
    (should (require feature nil t))))

(ert-deftest delib-flow-core-internal-modules-require-without-facade-load-order ()
  (dolist (feature '(delib-flow-services delib-flow-artifacts delib-flow-stages))
    (let* ((result (delib-flow-test--batch-require-output feature))
           (status (plist-get result :status))
           (output (plist-get result :output)))
      (should (equal 0 status))
      (should-not (string-match-p "void-variable\\|void-function\\|error" output)))))

(provide 'delib-flow-architecture-test)

;;; delib-flow-architecture-test.el ends here
