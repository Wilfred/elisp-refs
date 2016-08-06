;;; refs.el --- find callers of elisp functions or macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: lisp
;; Package-Requires: ((dash "2.12.0") (f "0.18.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package for finding callers of elisp functions or macros. Really
;; useful for finding examples.

;;; Code:

;; `read' can move through a buffer
;; `read-from-string' returns final index.

(require 'dash)
(require 'f)

;; TODO: if point is in the middle of a symbol, this confusingly moves
;; backwards.
(defun refs--next-sexp-start ()
  "Put point at the start of the next sexp.
Signals 'end if there are no more sexps."
  ;; TODO: write in terms of `scan-sexps'.
  (forward-sexp 1)
  (when (equal (point) (point-max))
    (throw 'after-last-sexp nil))
  (forward-sexp -1))

(defun refs--read-with-positions (buffer start)
  "Read a from from BUFFER, starting from offset START.
Assumes that START is not inside a string or comment.

For each form, return a list \(form start-index end-index\). Each
subform has the same structure."
  (with-current-buffer buffer
    (goto-char (1+ start))
    (condition-case _err
        (let* (start-pos
               ;; `read' moves point to the end of the current form.
               (form (read buffer))
               (end-pos (1- (point))))
          ;; TODO: write in terms of `scan-sexps'.
          (forward-sexp -1)
          (setq start-pos (1- (point)))
          ;; TODO: Handle vector literals.
          ;; TODO: handle ' and `.
          (if (consp form)
              ;; Recursively read the subelement of the form.
              (let ((subforms nil)
                    (next-subform (refs--read-with-positions buffer (1+ start-pos))))
                (while next-subform
                  (push next-subform subforms)
                  (setq next-subform
                        (refs--read-with-positions
                         buffer (-last-item next-subform))))
                (list (nreverse subforms) start-pos end-pos))
            ;; This form is an atom, so we're done.
            (list form start-pos end-pos)))
      ;; reached a closing paren.
      (invalid-read-syntax nil))))

(defun refs--read-all-with-positions (buffer)
  "Read all the forms from BUFFER. For each form, return
a list \(form start-index end-index\) recursively."
  (let ((pos 0)
        (forms nil))
    ;; Read forms until we hit EOF, which raises an error.
    (ignore-errors
      (while t
        (let ((indexed-form (refs--read-with-positions buffer pos)))
          (push indexed-form forms)
          (setq pos (-last-item indexed-form)))))
    (nreverse forms)))

(defun refs--read-all-forms (buffer)
  "Return a list of all the forms in BUFFER, with the string
indexes where each form starts and ends."
  (let ((forms nil))
    (with-current-buffer buffer
      (catch 'after-last-sexp
        (goto-char (point-min))
        (refs--next-sexp-start)
        ;; Loop until we have no more forms to read.
        (while t
          ;; String indexing is zero-indexed, but point is
          ;; one-indexed.
          (let* ((sexp-start (1- (point)))
                 (form-with-pos (read-from-string (buffer-string) sexp-start))
                 (form (car form-with-pos))
                 (sexp-end (cdr form-with-pos)))
            (push (list form sexp-start sexp-end) forms)
            (goto-char (1+ sexp-end)))
          (refs--next-sexp-start))))
    (nreverse forms)))

(defun refs--find-calls (form symbol)
  "If FORM contains any calls to SYMBOL, return those subforms.
Returns nil otherwise.

This is basic static analysis, so indirect function calls are
ignored."
  ;; TODO: Handle funcall to static symbols too.
  (cond
   ;; Base case: are we looking at (symbol ...)?
   ((and (consp form) (eq (car form) symbol))
    (list form))
   ;; Recurse, so we can find (... (symbol ...) ...)
   ((consp form)
    (-non-nil (--mapcat (refs--find-calls it symbol) form)))
   ;; If it's not a cons cell, it's not a call.
   (t
    nil)))

(defun refs--functions ()
  "Return a list of all symbols that are variables."
  (let (symbols)
    (mapatoms (lambda (symbol)
                (when (functionp symbol)
                  (push symbol symbols))))
    symbols))

(defun refs--loaded-files ()
  "Return a list of all files that have been loaded in Emacs.
Where the file was a .elc, return the path to the .el file instead."
  (let ((elc-paths (-map #'-first-item load-history)))
    (-non-nil
     (--map
      (if (s-ends-with-p ".el" it) it
        (let ((el-name (format "%s.el" (f-no-ext it)))
              (el-gz-name (format "%s.el.gz" (f-no-ext it))))
          (cond ((f-exists? el-name) el-name)
                ((f-exists? el-gz-name) el-gz-name)
                ;; Ignore files where we can't find a .el file.
                (t nil))))
      elc-paths))))

(defun refs-function (symbol)
  "Display all the references to SYMBOL, a function."
  (interactive
   ;; TODO: default to function at point.
   (list (read (completing-read "Function: " (refs--functions)))))

  (let* ((buf (get-buffer-create "refs.el"))
         (forms (refs--read-all-forms buf))
         (matching-forms (-non-nil
                          (--mapcat (refs--find-calls it symbol)
                                    forms))))
    (message "Found: %s" matching-forms)))

(provide 'refs)
;;; refs.el ends here
