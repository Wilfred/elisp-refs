(require 'ert)
(require 'refs)

;; TODO: searching for quoted forms, like `-map-first', is showing
;; incorrect forms highlighted

;; For Travis CI is recursing more deeply, meaning we hit recursion
;; limits. I suspect this is due to undercover collecting coverage
;; metrics.
(when (getenv "TRAVIS")
  (message "Updating recursion limits from: max-specpdl-size: %s max-lisp-eval-depth: %s "
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

(ert-deftest refs--unindent-split-properties ()
  "Ensure we can still unindent when properties are split
into separate region. Regression test for a very subtle bug."
  (let ((s #("e.\n" 0 2 (refs-start-pos 0) 2 3 (refs-start-pos 0))))
    (refs--unindent-rigidly s)))

(ert-deftest refs--sexp-positions ()
  "Ensure we calculate positions correctly when we're considering
the whole buffer."
  (with-temp-backed-buffer
   "(while list (setq len 1))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (sexp-positions
           (refs--sexp-positions refs-buf (point-min) (point-max))))
     (should
      (equal sexp-positions (list '(1 26)))))))

(ert-deftest refs--sexp-positions-comments ()
  "Ensure we handle comments correctly when calculating sexp positions."
  (with-temp-backed-buffer
   "(while list
  ;; take the head of LIST
  (setq len 1))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (sexp-positions
           (refs--sexp-positions refs-buf (1+ (point-min)) (1- (point-max)))))
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

(ert-deftest refs--find-calls-in-backquote ()
  "Find function calls in backquotes.
Useful for finding references in macros, but this is primarily a
regression test for bugs where we miscalculated position with
backquote forms."
  (with-temp-backed-buffer
   "(baz `(biz (foo 1)))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'foo
                                      #'refs--function-p)))
     (should
      (equal calls (list (list '(foo 1) 12 19)))))))

(ert-deftest refs--find-macros-improper-list ()
  "We shouldn't crash if the source code contains improper lists."
  (with-temp-backed-buffer
   "(destructuring-bind (start . end) region\n  (when foo\n    (bar)))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (calls (refs--read-and-find refs-buf 'when
                                      #'refs--macro-p)))
     (should
      (equal calls (list (list '(when foo (bar)) 44 64)))))))

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
   "(defun bar (foo)) (defsubst bar (foo)) (defmacro bar (foo)) (cl-defun bar (foo))"
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

(ert-deftest refs--find-symbols ()
  "We should find symbols, not their containing forms."
  (with-temp-backed-buffer
   "(foo foo)"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (matches (refs--read-and-find-symbol refs-buf 'foo)))
     (should
      (equal
       matches
       (list '(foo 2 5) '(foo 6 9)))))))

(ert-deftest refs--find-var-basic ()
  "Test the base case of finding variables"
  (with-temp-backed-buffer
   "foo"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (matches (refs--read-and-find refs-buf 'foo
                                        #'refs--variable-p)))
     (should
      (equal
       matches
       (list '(foo 1 4)))))))

(ert-deftest refs--find-var-ignores-calls ()
  "Function calls are not variable references."
  (with-temp-backed-buffer
   "(baz (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (matches (refs--read-and-find refs-buf 'foo
                                        #'refs--variable-p)))
     (should (null matches)))))

(ert-deftest refs--find-var-let-without-assignments ()
  "We should recognise let variables as variable references."
  (with-temp-backed-buffer
   "(let (foo)) (let* (foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (matches (refs--read-and-find refs-buf 'foo
                                        #'refs--variable-p)))
     (should
      (equal
       matches
       (list '(foo 7 10) '(foo 20 23)))))))

(ert-deftest refs--find-var-let-with-assignments ()
  "We should recognise let variables as variable references."
  (with-temp-backed-buffer
   "(let ((foo 1))) (let* ((foo 2)))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (matches (refs--read-and-find refs-buf 'foo
                                        #'refs--variable-p)))
     (should
      (equal
       matches
       (list '(foo 8 11) '(foo 25 28)))))))

(ert-deftest refs--find-var-let-body ()
  "We should recognise let variables as variable references."
  (with-temp-backed-buffer
   "(let ((x (1+ foo))) (+ x foo))"
   (let* ((refs-buf (refs--contents-buffer (buffer-file-name)))
          (matches (refs--read-and-find refs-buf 'foo
                                        #'refs--variable-p)))
     (should
      (equal
       (length matches)
       2)))))

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
  ;; We should have set refs-unindented correctly.
  (should
   (equal
    (get-text-property
     0
     'refs-unindented
     (refs--unindent-rigidly "  foo"))
    2))
  ;; We should still have refs-path properties in the entire string.
  (let ((result (refs--unindent-rigidly
                 (propertize " foo\n bar" 'refs-path "/foo"))))
    (cl-loop for i from 0 below (length result) do
             (should
              (get-text-property i 'refs-path result)))))

(ert-deftest refs--replace-tabs ()
  "Ensure we replace all tabs in STRING."
  (let ((tab-width 4))
    ;; zero tabs
    (should (equal (refs--replace-tabs " a ") " a "))
    ;; many tabs
    (should (equal (refs--replace-tabs "a\t\tb") "a        b"))))

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

(ert-deftest refs-special ()
  "Smoke test for `refs-special'."
  (refs-special 'prog2)
  (should
   (equal (buffer-name) "*refs: prog2*")))

(ert-deftest refs-symbol ()
  "Smoke test for `refs-symbol'."
  (refs-symbol 'format-message)
  (should
   (equal (buffer-name) "*refs: format-message*")))
