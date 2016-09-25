;;; refs-bench.el --- measure refs.el performance

;;; Code:

(require 'refs)
(require 'dash)
(require 'shut-up)

(defmacro refs--print-time (form)
  "Evaluate FORM, and print the time taken."
  `(progn
     (message "Timing %s" ',form)
     (-let [(total-time gc-runs gc-time)
            (shut-up (benchmark-run 1 ,form))]
       (message "Elapsed time: %fs (%fs in %d GCs)"
                total-time
                gc-time
                gc-runs))))

;; TODO: benchmark refs-variable (and add a smoke test)
;; TODO: make this more representative by loading more elisp files
;; before searching. Running this in a GUI is also conspicuously
;; slower, which bench.sh doesn't reflect.
(defun refs-bench ()
  "Measure runtime of searching."
  (interactive)
  (refs--report-loc)
  ;; Measure a fairly uncommon function.
  (refs--print-time (refs-function 'mod))
  ;; Measure a common macro
  (refs--print-time (refs-macro 'when))
  ;; Compare with searching for the same symbol without walking
  (refs--print-time (refs-symbol 'when))
  ;; Synthetic test of a large number of results.
  (message "Formatting 10,000 results")
  (let ((forms (-repeat 10000 (list '(ignored) 1 64)))
        (buf (generate-new-buffer " *dummy-refs-buf*")))
    (with-current-buffer buf
      (insert "(defun foo (bar) (if bar nil (with-current-buffer bar))) ;; blah")
      (setq-local refs--path "/tmp/foo.el"))
    (refs--print-time
     (refs--show-results 'foo "foo bar" (list (cons forms buf))))
    (kill-buffer buf)))

(defun refs--report-loc ()
  "Report the total number of lines of code searched."
  (interactive)
  (let* ((loaded-paths (refs--loaded-files))
         (loaded-src-bufs (-map #'refs--contents-buffer loaded-paths))
         (total-lines (-sum (--map (with-current-buffer it
                                     (line-number-at-pos (point-max)))
                                   loaded-src-bufs))))
    ;; Clean up temporary buffers.
    (--each loaded-src-bufs (kill-buffer it))
    (message "Total LOC: %s" (refs--format-int total-lines))))

(provide 'refs-bench)
;;; refs-bench.el ends here
