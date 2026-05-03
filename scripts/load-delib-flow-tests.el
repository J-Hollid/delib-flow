;;; load-delib-flow-tests.el --- Bootstrap delib-flow tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Load the delib-flow package and its ERT test suite into the current Emacs
;; session so `ert-run-tests-interactively' can discover the tests.

;;; Code:

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory (directory-file-name
                                        (file-name-directory this-file)))))
  (load-file (expand-file-name "delib-flow.el" repo-root))
  (load-file (expand-file-name "test/delib-flow-test.el" repo-root)))

;;; load-delib-flow-tests.el ends here
