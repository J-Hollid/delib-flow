;;; load-delib-flow-tests.el --- Bootstrap delib-flow tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Load the delib-flow package and its ERT test suite into the current Emacs
;; session so `ert-run-tests-interactively' can discover the tests.

;;; Code:

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory (directory-file-name
                                        (file-name-directory this-file)))))
  (setq load-prefer-newer t)
  (add-to-list 'load-path repo-root)
  (add-to-list 'load-path (expand-file-name "test" repo-root))
  (require 'delib-flow)
  (load-file (expand-file-name "test/delib-flow-architecture-test.el" repo-root))
  (load-file (expand-file-name "test/delib-flow-test.el" repo-root)))

;;; load-delib-flow-tests.el ends here
