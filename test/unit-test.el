(require 'ert)
(require 'refs)

(ert-deftest refs-function ()
  "Smoke test for `refs-function'."
  (refs-function 'format))
