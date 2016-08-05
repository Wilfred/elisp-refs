(require 'ert)
(require 'refs)

(ert-deftest refs-find-calls-returns-forms ()
  (should
   (equal
    (refs--find-calls '(x 1 (y 2) (aa (bb cc))) 'bb)
    '((bb cc)))))

