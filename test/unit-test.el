(require 'ert)
(require 'refs)

(ert-deftest refs-find-calls-returns-forms ()
  (should
   (equal
    (refs--find-calls '(x 1 (y 2) (aa (bb cc))) 'bb)
    (list '(bb cc)))))

(ert-deftest refs-find-calls-form-depth ()
  (should
   (equal
    (refs--find-calls '((((x 1)))) 'x)
    (list '(x 1)))))

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
