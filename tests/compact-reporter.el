
(require 'ert) 

(defvar combobulate--test-results nil "List of test results collected during execution.") 

(defun combobulate-compact-listener 
  (event-type &rest event-args) "ERT listener for compact output. EVENT-TYPE is the type of event. EVENT-ARGS are the arguments for the event." 
  (pcase event-type 
    ('test-ended 
      (let* 
        (
          (test (nth 1 event-args)) 
          (result (nth 2 event-args)) 
          (name (ert-test-name test))) 
        (cond 
          (
            (ert-test-passed-p result) 
            (push 
              (list :status 'pass :name name :rank 0) combobulate--test-results)) 
          (
            (ert-test-skipped-p result) 
            (push 
              (list :status 'skip :name name :rank 1) combobulate--test-results)) 
          (t 
            (let* 
              (
                (condition 
                  (ert-test-failed-condition result)) 
                (step 
                  (plist-get (cddr condition) :step))) 
              (push 
                (list :status 'fail :name name :step step :rank 2) combobulate--test-results)))))))) 

(defun combobulate-run-tests-compact (selector) "Run tests matched by SELECTOR and print compact report." (setq ert-quiet t) 
  (let* 
    (
      ;; (inhibit-message t) 
      (combobulate--test-results nil) ; Bind locally
 
      (stats 
        (ert-run-tests selector #'combobulate-compact-listener)) 
      (passed-expected 
        (ert--stats-passed-expected stats)) 
      (passed-unexpected 
        (ert--stats-passed-unexpected stats)) 
      (failed-expected 
        (ert--stats-failed-expected stats)) 
      (failed-unexpected 
        (ert--stats-failed-unexpected stats)) 
      (skipped 
        (ert--stats-skipped stats)) 
      (total 
        (+ passed-expected passed-unexpected failed-expected failed-unexpected skipped)) 
      (passed 
        (+ passed-expected passed-unexpected)) 
      (failed 
        (+ failed-expected failed-unexpected))) 
;; Sort results: PASS (0) < SKIP (1) < FAIL (2)
 
    (setq combobulate--test-results 
      (sort combobulate--test-results 
        (lambda (a b) 
          (let 
            (
              (rank-a (plist-get a :rank)) 
              (rank-b (plist-get b :rank))) 
            (if (= rank-a rank-b) 
              (string< 
                (symbol-name (plist-get a :name)) 
                (symbol-name (plist-get b :name))) (< rank-a rank-b)))))) 
;; Print results
 
    (dolist 
      (res combobulate--test-results) 
      (let 
        (
          (status 
            (plist-get res :status)) 
          (name (plist-get res :name))) 
        (pcase status 
          ('pass 
            (princ 
              (format "PASS %s\n" name))) 
          ('skip 
            (princ 
              (format "SKIP %s\n" name))) 
          ('fail 
            (let 
              (
                (step (plist-get res :step))) 
              (princ 
                (format "FAIL %s%s\n" name 
                  (if step (format " [%s]" step) "")))))))) 
    (princ 
      (format "\nRan %d tests, %d passed, %d failed, %d skipped\n" total passed failed skipped)) 
    (if 
      (zerop 
        (+ failed-unexpected failed-expected)) (kill-emacs 0) (kill-emacs 1)))) 

(provide 'compact-reporter) 
;;; compact-reporter.el ends here
