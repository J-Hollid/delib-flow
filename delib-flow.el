;;; delib-flow.el --- Guided AI workflow control for Org -*- lexical-binding: t; -*-
;; Author: Jordan Holliday

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))
;; Keywords: outlines, tools, ai
;; URL: https://example.invalid/delib-flow

;;; Commentary:

;; delib-flow provides a guided control buffer for deliberate AI-assisted
;; workflow execution from an Org heading at point.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'pp)
(require 'seq)
(require 'subr-x)

(defmacro delib-flow--define-function (name args &rest body)
  "Define NAME with ARGS and BODY through a shared wrapper macro."
  (declare (indent defun))
  `(defalias ',name
     (lambda ,args
       ,@body)))

(require 'delib-flow-config)
(require 'delib-flow-model)
(require 'delib-flow-artifacts)
(require 'delib-flow-services)
(require 'delib-flow-stages)
(require 'delib-flow-filing)
(require 'delib-flow-render)
(require 'delib-flow-ui)
(require 'delib-flow-audit)
(require 'delib-flow-debug)

(provide 'delib-flow)
;;; delib-flow.el ends here
