(require 'ert)
(require 'refs)

(defmacro with-temp-backed-buffer (contents &rest body)
  "Create a temporary file with CONTENTS, and evaluate BODY
whilst visiting that file."
  (let ((filename-sym (make-symbol "filename"))
        (buf-sym (make-symbol "buf")))
    `(let* ((,filename-sym (make-temp-file "with-temp-buffer-and-file"))
            (,buf-sym (find-file-noselect ,filename-sym)))
       (unwind-protect
           (with-current-buffer ,buf-sym
             (insert ,contents)
             (shut-up (save-buffer))
             ,@body)
         (kill-buffer ,buf-sym)
         (delete-file ,filename-sym)))))

(ert-deftest refs--sexp-positions ()
  "Ensure we handle comments correctly when calculating sexp positions."
  (with-temp-backed-buffer
   "(while list
  ;; take the head of LIST
  (setq len 1))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (sexp-positions
           (refs--sexp-positions refs-buf (point-min) (point-max))))
     ;; The position of the setq should take into account the comment.
     (should
      (equal (nth 2 sexp-positions) '(42 54))))))

(ert-deftest refs--find-calls-funcall ()
  "Find calls that use funcall."
  (with-temp-backed-buffer
   "(funcall #'foo)"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (forms (refs--read-all-buffer-forms refs-buf))
          (calls (refs--find-calls forms refs-buf 'foo)))
     ;; The position of the setq should take into account the comment.
     (should
      (equal calls (list (list '(funcall #'foo) 1 16)))))))

(ert-deftest refs--find-calls-apply ()
  "Find calls that use apply."
  (with-temp-backed-buffer
   "(apply #'foo '(1 2))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (forms (refs--read-all-buffer-forms refs-buf))
          (calls (refs--find-calls forms refs-buf 'foo)))
     ;; The position of the setq should take into account the comment.
     (should
      (equal calls (list (list '(apply #'foo '(1 2)) 1 21)))))))

(ert-deftest refs--unindent-rigidly ()
  "Ensure we unindent by the right amount."
  ;; Take the smallest amount of indentation, (2 in this case), and
  ;; unindent by that amount.
  (should
   (equal
    (refs--unindent-rigidly "   foo\n  bar\n    baz")
    " foo\nbar\n  baz"))
  ;; If one of the lines has no indent, do nothing.
  (should
   (equal
    (refs--unindent-rigidly "foo\n bar")
    "foo\n bar"))
  ;; Consider tabs to be equivalent to `tab-width' spaces.
  (let ((tab-width 8))
    (should
     (equal
      (refs--unindent-rigidly "\tx\n    y")
      "    x\ny"))))

(ert-deftest refs-function ()
  "Smoke test for `refs-function'."
  (refs-function 'format))

(ert-deftest refs-function ()
  "Smoke test for `refs-macro'."
  (refs-macro 'when))
