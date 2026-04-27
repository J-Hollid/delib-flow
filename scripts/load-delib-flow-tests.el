;;; load-delib-flow-tests.el --- Bootstrap delib-flow tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Load the delib-flow package and its ERT test suite into the current Emacs
;; session so `ert-run-tests-interactively' can discover the tests.

;;; Code:

(load-file "/home/j-holliday/.emacs.d/site-lisp/delib-flow/delib-flow.el")
(load-file "/home/j-holliday/.emacs.d/site-lisp/delib-flow/test/delib-flow-test.el")

;;; load-delib-flow-tests.el ends here
