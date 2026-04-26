;;; validate-coverage.el --- Coverage gate for delib-flow -*- lexical-binding: t; -*-

;;; Commentary:

;; Runs the ERT suite under testcover instrumentation and fails if repository
;; source coverage drops below the configured threshold.

;;; Code:

(require 'ert)
(require 'testcover)
(require 'cl-lib)

(defconst delib-flow-coverage-threshold 90.0
  "Minimum required source coverage percentage.")

(defconst delib-flow-coverage-source-files
  '("delib-flow.el")
  "Repository source files included in the coverage gate.")

(defconst delib-flow-coverage-test-files
  '("test/delib-flow-test.el")
  "Test files loaded for the coverage gate.")

(defun delib-flow-coverage--repo-root ()
  "Return the repository root for the coverage validator."
  default-directory)

(defun delib-flow-coverage--expand (relative-path)
  "Return RELATIVE-PATH expanded from the repository root."
  (expand-file-name relative-path (delib-flow-coverage--repo-root)))

(defun delib-flow-coverage--instrument-sources ()
  "Instrument repository source files for coverage."
  (dolist (file delib-flow-coverage-source-files)
    (testcover-start (delib-flow-coverage--expand file))))

(defun delib-flow-coverage--load-tests ()
  "Load repository test files."
  (dolist (file delib-flow-coverage-test-files)
    (load-file (delib-flow-coverage--expand file))))

(defun delib-flow-coverage--covered-point-p (value)
  "Return non-nil when coverage VALUE counts as covered."
  (not (eq value 'edebug-unknown)))

(defun delib-flow-coverage--function-stats (symbol)
  "Return coverage stats plist for SYMBOL."
  (let ((coverage (get symbol 'edebug-coverage))
        (covered 0)
        (total 0))
    (dotimes (index (length coverage))
      (setq total (1+ total))
      (when (delib-flow-coverage--covered-point-p (aref coverage index))
        (setq covered (1+ covered))))
    (list :symbol symbol
          :covered covered
          :total total)))

(defun delib-flow-coverage--collect-stats ()
  "Return a list of per-function coverage plists."
  (let (stats)
    (dolist (entry edebug-form-data (nreverse stats))
      (push (delib-flow-coverage--function-stats (car entry)) stats))))

(defun delib-flow-coverage--totals (stats)
  "Return aggregate coverage totals for STATS."
  (let ((covered 0)
        (total 0))
    (dolist (stat stats)
      (setq covered (+ covered (plist-get stat :covered)))
      (setq total (+ total (plist-get stat :total))))
    (list :covered covered :total total)))

(defun delib-flow-coverage--percent (covered total)
  "Return coverage percent for COVERED and TOTAL."
  (if (zerop total)
      0.0
    (* 100.0 (/ (float covered) total))))

(defun delib-flow-coverage--print-report (stats totals)
  "Print a coverage report for STATS and TOTALS."
  (let* ((covered (plist-get totals :covered))
         (total (plist-get totals :total))
         (percent (delib-flow-coverage--percent covered total)))
    (princ (format "Coverage: %.2f%% (%d/%d)\n" percent covered total))
    (dolist (stat stats)
      (let* ((symbol (plist-get stat :symbol))
             (fn-covered (plist-get stat :covered))
             (fn-total (plist-get stat :total))
             (fn-percent (delib-flow-coverage--percent fn-covered fn-total)))
        (princ (format "  %s: %.2f%% (%d/%d)\n"
                       symbol fn-percent fn-covered fn-total))))))

(defun delib-flow-coverage--run-tests ()
  "Run the full ERT suite under coverage instrumentation."
  (let ((results (ert-run-tests-batch t)))
    (unless (zerop (ert-stats-completed-unexpected results))
      (error "Coverage run failed because tests did not pass"))))

(defun delib-flow-coverage-main ()
  "Run the coverage validator."
  (delib-flow-coverage--instrument-sources)
  (delib-flow-coverage--load-tests)
  (delib-flow-coverage--run-tests)
  (let* ((stats (delib-flow-coverage--collect-stats))
         (totals (delib-flow-coverage--totals stats))
         (covered (plist-get totals :covered))
         (total (plist-get totals :total))
         (percent (delib-flow-coverage--percent covered total)))
    (delib-flow-coverage--print-report stats totals)
    (when (< percent delib-flow-coverage-threshold)
      (error "Coverage %.2f%% is below required %.2f%%"
             percent delib-flow-coverage-threshold))))

(delib-flow-coverage-main)

;;; validate-coverage.el ends here
