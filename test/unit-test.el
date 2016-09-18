(require 'ert)
(require 'refs)

;; For some reason, travis CI is recursing more deeply, meaning we hit
;; recursion limits that I can't reproduce locally
(when (getenv "TRAVIS")
  (message "max-specpdl-size: %s max-lisp-eval-depth: %s "
           max-specpdl-size
           max-lisp-eval-depth)
  (setq max-specpdl-size 2500)
  (setq max-lisp-eval-depth 1000))

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

(ert-deftest refs--format-int ()
  "Ensure we format thousands correctly in numbers."
  (should (equal (refs--format-int 123) "123"))
  (should (equal (refs--format-int -123) "-123"))
  (should (equal (refs--format-int 1234) "1,234"))
  (should (equal (refs--format-int -1234) "-1,234"))
  (should (equal (refs--format-int 1234567) "1,234,567")))

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

(ert-deftest refs--find-calls-basic ()
  "Find simple function calls."
  (with-temp-backed-buffer
   "(foo)"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should
      (equal calls (list (list '(foo) 1 6)))))))

(ert-deftest refs--find-calls-nested ()
  "Find nested function calls."
  (with-temp-backed-buffer
   "(baz (bar (foo)))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should
      (equal calls (list (list '(foo) 11 16)))))))

(ert-deftest refs--find-calls-funcall ()
  "Find calls that use funcall."
  (with-temp-backed-buffer
   "(funcall 'foo)"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should
      (equal calls (list (list '(funcall 'foo) 1 15)))))))

(ert-deftest refs--find-calls-apply ()
  "Find calls that use apply."
  (with-temp-backed-buffer
   "(apply 'foo '(1 2))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should
      (equal calls (list (list '(apply 'foo '(1 2)) 1 20)))))))

(ert-deftest refs--find-calls-params ()
  "Function or macro parameters should not be considered function calls."
  (with-temp-backed-buffer
   "(defun bar (foo)) (defsubst bar (foo)) (defmacro bar (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should (null calls)))))

(ert-deftest refs--find-calls-let-without-assignment ()
  "We shouldn't confuse let declarations with function calls."
  (with-temp-backed-buffer
   "(let (foo)) (let* (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should (null calls)))))

(ert-deftest refs--find-calls-let-with-assignment ()
  "We shouldn't confuse let assignments with function calls."
  (with-temp-backed-buffer
   "(let ((foo nil))) (let* ((foo nil)))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should (null calls)))))

(ert-deftest refs--find-calls-let-with-assignment-call ()
  "We should find function calls in let assignments."
  ;; TODO: actually check positions, this is error-prone.
  (with-temp-backed-buffer
   "(let ((bar (foo)))) (let* ((bar (foo))))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should
      (equal (length calls) 2)))))

(ert-deftest refs--find-calls-let-body ()
  "We should find function calls in let body."
  (with-temp-backed-buffer
   "(let (bar) (foo)) (let* (bar) (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should (equal (length calls) 2)))))

(ert-deftest refs--find-macros-basic ()
  "Find simple function calls."
  (with-temp-backed-buffer
   "(foo)"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--macro-p)))
     (should
      (equal calls (list (list '(foo) 1 6)))))))

(ert-deftest refs--find-macros-params ()
  "Find simple function calls."
  (with-temp-backed-buffer
   "(defun bar (foo)) (defsubst bar (foo)) (defmacro bar (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--macro-p)))
     (should (null calls)))))

(ert-deftest refs--find-macros-let-without-assignment ()
  "We shouldn't confuse let declarations with macro calls."
  (with-temp-backed-buffer
   "(let (foo)) (let* (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--macro-p)))
     (should (null calls)))))

(ert-deftest refs--find-macros-let-with-assignment ()
  "We shouldn't confuse let assignments with macro calls."
  (with-temp-backed-buffer
   "(let ((foo nil) (foo nil))) (let* ((foo nil) (foo nil)))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--macro-p)))
     (should (null calls)))))

(ert-deftest refs--find-macros-let-with-assignment-call ()
  "We should find macro calls in let assignments."
  (with-temp-backed-buffer
   "(let ((bar (foo)))) (let* ((bar (foo))))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--macro-p)))
     (should
      (equal (length calls) 2)))))

(ert-deftest refs--find-calls-let-body ()
  "We should find macro calls in let body."
  (with-temp-backed-buffer
   "(let (bar) (foo)) (let* (bar) (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--macro-p)))
     (should (equal (length calls) 2)))))

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
  (refs-function 'format)
  (should
   (equal (buffer-name) "*refs: format*")))

(ert-deftest refs-macro ()
  "Smoke test for `refs-macro'."
  (refs-macro 'when)
  (should
   (equal (buffer-name) "*refs: when*")))

(ert-deftest refs-variable ()
  "Smoke test for `refs-variable'."
  (refs-variable 'case-fold-search)
  (should
   (equal (buffer-name) "*refs: case-fold-search*")))

(ert-deftest refs-symbol ()
  "Smoke test for `refs-symbol'."
  (refs-symbol 'format-message)
  (should
   (equal (buffer-name) "*refs: format-message*")))
