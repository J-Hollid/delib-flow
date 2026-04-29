;;; load-delib-flow-local-llm-session.el --- Bootstrap local LLM test sessions -*- lexical-binding: t; -*-

;;; Commentary:

;; Evaluate this file in a live Emacs session to load the repo-local
;; delib-flow checkout, then prepare the local LLM manual-test environment.
;;
;; This bootstrap expects your normal Emacs config to provide `jh/ollama-url`
;; and `jh/ollama-model`. It will:
;; - load `delib-flow-local-test-config.el`
;; - clear the local LLM log
;; - clear the audit log
;; - start `ollama serve` if needed
;; - run the local readiness check

;;; Code:

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (expand-file-name ".." (file-name-directory this-file)))
       (local-config (expand-file-name "delib-flow-local-test-config.el" repo-root)))
  (unless (file-readable-p local-config)
    (user-error "Local test config is not readable: %s" local-config))
  (load-file local-config)
  (delib-flow-local-test-setup-session)
  (message
   (concat
    "delib-flow local LLM session loaded. "
    "Start with M-x delib-flow-local-test-start-debug-scenario "
    "or M-x delib-flow-start.")))

(provide 'load-delib-flow-local-llm-session)

;;; load-delib-flow-local-llm-session.el ends here
