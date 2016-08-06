(require 'ert)
(require 'refs)

(ert-deftest refs-read-with-positions-whitespace ()
  "We should not include leading whitespace when calculating form
indexes."
  (with-temp-buffer
    (insert " foo ")
    (should
     (equal
      (refs--read-with-positions (current-buffer) 0)
      (list 'foo 1 4)))))

(ert-deftest refs-read-with-positions-recurse ()
  (with-temp-buffer
    (insert "(bar baz)")
    (should
     (equal
      (refs--read-with-positions (current-buffer) 0)
      (list (list (list 'bar 1 4) (list 'baz 5 8)) 0 9)))))

(ert-deftest refs-read-all-with-positions ()
  (with-temp-buffer
    (insert "10 20 30")
    (should
     (equal
      (refs--read-all-with-positions (current-buffer))
      (list
       (list 10 0 2)
       (list 20 3 5)
       (list 30 6 8))))))

(ert-deftest refs-find-calls ()
  "Ensure we can find top level calls and calls inside functions."
  (let (indexed-forms)
    (with-temp-buffer
      (insert "(foo 1)\n")
      (insert "(defun bar () (foo))")
      (setq indexed-forms (refs--read-all-with-positions (current-buffer))))
    (should
     (equal
      (refs--find-calls indexed-forms 'foo)
      (list
       ;; Calling (foo 1)
       '(((foo 1 4) (1 5 6)) 0 7)
       ;; Calling (foo)
       '(((foo 23 26)) 22 27))))))
