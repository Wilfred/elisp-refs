;;; callers.el --- find callers of elisp functions or macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: lisp
;; Package-Requires: ((dash "2.12.0"))

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

(defun callers--next-sexp-start ()
  "Put point at the start of the next sexp.
Signals 'end if there are no more sexps."
  ;; TODO: write in terms of `scan-sexps'.
  (forward-sexp 1)
  (when (equal (point) (point-max))
    (throw 'after-last-sexp nil))
  (forward-sexp -1))

(defun callers--read-all-forms (buffer)
  "Return a list of all the forms in BUFFER, with the string
indexes where each form starts and ends."
  (let ((forms nil))
    (with-current-buffer buffer
      (catch 'after-last-sexp
        (goto-char (point-min))
        (callers--next-sexp-start)
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
          (callers--next-sexp-start))))
    (nreverse forms)))

(defun callers--has-call-p (form symbol)
  "Return t if FORM contains a call to SYMBOL.
This is basic static analysis, so indirection function calls are
ignored."
  ;; TODO: Handle funcall to static symbols too.
  (cond
   ;; Base case: are we looking at (symbol ...)?
   ((and (consp form) (eq (car form) symbol))
    t)
   ;; Recurse, so we can find (... (symbol ...) ...)
   ((consp form)
    (--any-p (callers--has-call-p it symbol) form))
   ;; If it's not a cons cell, it's not a call.
   (t
    nil)))

(provide 'callers)
;;; callers.el ends here
