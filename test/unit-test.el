(require 'ert)
(require 'refs)
(require 'ht)

(defmacro with-temp-buffer-contents (string &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,string)
     ,@body))

(ert-deftest refs-paren-positions-basic ()
  (with-temp-buffer-contents "( (foo) (bar) )"
    (should
     (equal
      (refs--paren-positions (current-buffer) (point-min) (point-max))
      '((3 8) (9 14))))))

(ert-deftest refs-paren-positions-quoted ()
  (with-temp-buffer-contents "(defcustom lispy-no-space nil\n  :group 'lispy)"
    (should
     (null
      (refs--paren-positions (current-buffer) (point-min) (point-max))))))

(ert-deftest refs-paren-positions-require ()
  (with-temp-buffer-contents "(require 'foo)"
    (should
     (null
      (refs--paren-positions (current-buffer) (point-min) (point-max))))))

;; (ert-deftest refs-find-calls ()
;;   "Ensure we can find top level calls and calls inside functions."
;;   (-let [(forms offsets) (refs--read-all-with-offsets "(foo 1)\n(defun bar () (foo))")]
;;     (should
;;      (equal
;;       (refs--find-calls forms 'foo)
;;       (list
;;        '(foo 1)
;;        '(foo))))))
