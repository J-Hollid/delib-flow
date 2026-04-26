;;; load-delib-flow.el --- Local bootstrap for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Evaluate this file in a live Emacs session to load the local delib-flow
;; package from the repository checkout without installing it.

;;; Code:

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (expand-file-name ".." (file-name-directory this-file)))
       (package-file (expand-file-name "delib-flow.el" repo-root)))
  (add-to-list 'load-path repo-root)
  (load-file package-file))

(provide 'load-delib-flow)

;;; load-delib-flow.el ends here
