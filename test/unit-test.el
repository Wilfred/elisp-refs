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
