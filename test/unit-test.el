(require 'ert)
(require 'refs)
(require 'ht)

(defun refs--read-all-from-string (string)
  "Read all the forms from STRING, along with their offsets."
  (with-temp-buffer
    (insert string)
    (refs--read-all-with-positions (current-buffer))))

(ert-deftest refs-read-with-positions-whitespace ()
  "Form offsets should be where the sexp starts, even if there's whitespace."
  (-let [(forms offsets) (refs--read-all-from-string " foo ")]
    (should
     (equal forms '(foo)))
    (should
     (ht-equal? offsets (ht ('foo (list 1 4)))))))

(ert-deftest refs-read-with-positions-recurse ()
  "Ensure we return offsets of list items too."
  (-let [(forms offsets) (refs--read-all-from-string "(bar baz)")]
    (should
     (equal forms '((bar baz))))
    (should
     (ht-equal? offsets (ht ('(bar baz) (list 0 9))
                            ('bar (list 1 4))
                            ('baz (list 5 8)))))))

(ert-deftest refs-read-with-positions-multiple ()
  "Ensure we read multiple sexps from the buffer."
  (-let [(forms offsets) (refs--read-all-from-string "10 20 30")]
    (should
     (equal forms '(10 20 30)))
    (should
     (ht-equal? offsets (ht (10 (list 0 2))
                            (20 (list 3 5))
                            (30 (list 6 8)))))))

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
