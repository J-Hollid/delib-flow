;;; load-delib-flow-debug-session.el --- Bootstrap manual debug sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Evaluate this file in a live Emacs session to load the local delib-flow
;; checkout for deterministic manual scenario testing.
;;
;; This bootstrap intentionally keeps configuration minimal:
;; - load the package from the repository
;; - set explicit audit policy defaults
;; - provide stable placeholder model labels for inspection buffers
;; - expose a helper to open the local test guide
;;
;; The built-in debug scenarios create disposable source, project, ZK, and
;; audit fixtures on demand, so they do not require your real filing paths.

;;; Code:

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (expand-file-name ".." (file-name-directory this-file)))
       (bootstrap-file (expand-file-name "scripts/load-delib-flow.el" repo-root)))
  (load-file bootstrap-file)

  (setq delib-flow-audit-payload-policy 'full
        delib-flow-audit-redaction-profile 'strict)

  ;; Scenario testing uses deterministic built-in executors. Placeholder model
  ;; labels keep the run state readable without depending on external adapters.
  (unless delib-flow-default-local-model
    (setq delib-flow-default-local-model "debug/local"))
  (unless delib-flow-default-cloud-model
    (setq delib-flow-default-cloud-model "debug/cloud"))

  (defun delib-flow-debug-open-test-guide ()
    "Open the repo-local manual test guide."
    (interactive)
    (find-file (expand-file-name "docs/test-guide.org" repo-root)))

  (message
   (concat
    "delib-flow debug session loaded. "
    "Open the guide with M-x delib-flow-debug-open-test-guide, "
    "then start with M-x delib-flow-debug-start-walkthrough.")))

(provide 'load-delib-flow-debug-session)

;;; load-delib-flow-debug-session.el ends here
