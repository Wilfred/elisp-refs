;;; refs.el --- find callers of elisp functions or macros  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: lisp
;; Package-Requires: ((dash "2.12.0") (f "0.18.2") (ht "2.1") (list-utils "0.4.4") (loop "2.1"))

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

(require 'list-utils)
(require 'dash)
(require 'f)
(require 'ht)
(require 'loop)
(eval-when-compile (require 'cl-lib))

(defsubst refs--start-pos (end-pos)
  "Find the start position of form ending at END-FORM
in the current buffer."
  (scan-sexps end-pos -1))

(defun refs--sexp-positions (buffer start-pos end-pos)
  "Return a list of start and end positions of all the sexps
between START-POS and END-POS (excluding ends) in BUFFER."
  (let ((positions nil)
        (current-pos (1+ start-pos)))
    (with-current-buffer buffer
      (condition-case _err
          ;; Loop until we can't read any more.
          (loop-while t
            (let* ((sexp-end-pos (scan-sexps current-pos 1))
                   (sexp-start-pos (refs--start-pos sexp-end-pos)))
              (if (< sexp-end-pos end-pos)
                  ;; This sexp is inside the range requested.
                  (progn
                    (push (list sexp-start-pos sexp-end-pos) positions)
                    (setq current-pos sexp-end-pos))
                ;; Otherwise, we've reached end the of range.
                (loop-break))))
        ;; Terminate when we see "Containing expression ends prematurely"
        (scan-error (nreverse positions))))))

(defun refs--read-buffer-form ()
  "Read a form from the current buffer, starting at point.
Returns a list (form start-pos end-pos).

Positions are 1-indexed, consistent with `point'."
  (let* ((form (read (current-buffer)))
         (end-pos (point))
         (start-pos (refs--start-pos end-pos)))
    (list form start-pos end-pos)))

(defun refs--read-all-buffer-forms (buffer)
  "Read all the forms in BUFFER, along with their positions."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case err
          (while t
            (push (refs--read-buffer-form) forms))
        (error
         (if (equal err '(end-of-file))
             ;; Reached end of file, we're done.
             (nreverse forms)
           ;; Some unexpected error, propagate.
           (error err)))))))

(defun refs--find-start-offset (string sexp-end)
  "Find the matching start offset in STRING for
sexp ending at SEXP-END."
  (with-temp-buffer
    (insert string)
    ;; Point is 1-indexed.
    (goto-char (1+ sexp-end))
    ;; TODO: write in terms of `scan-sexps'.
    (forward-sexp -1)
    (when (or (looking-back "'") (looking-back "`"))
      (forward-char -1))
    (1- (point))))

(defun refs--end-offset (form offsets)
  "Given a hash table OFFSETS generated by `refs--read-all-with-offsets',
return the end offset of FORM."
  (-last-item (ht-get offsets form)))

(defun refs--read-with-offsets (string offsets start-offset)
  "Read a form from STRING, starting from offset START-OFFSET.
Assumes that START-OFFSET is not inside a string or comment.

For each subform, record the start and end offsets in hash table
OFFSETS."
  (condition-case _err
      (let (form-start-offset)
        (-let* ((read-with-symbol-positions t)
                ((form . end-offset) (read-from-string string start-offset)))
          ;; `read-from-string' sets `read-symbol-positions-list' for
          ;; symbols, so we don't need to any more work.
          (-if-let (sym-and-start-offset (assoc form read-symbol-positions-list))
              (setq form-start-offset (+ (cdr sym-and-start-offset) start-offset))
            (setq form-start-offset (refs--find-start-offset string end-offset)))
          ;; TODO: Handle vector literals.
          ;; TODO: handle sharp quotes, i.e. #'foo
          ;; TODO: test for handling dotted lists.
          ;; TODO: we should still recurse on the cdr of quoted lists
          (when (and (consp form) (not (memq (car form) '(quote \`))))
            ;; TODO: don't recursve if car or cdr are symbols.
            ;; Recursively read the subelements of the form.
            (let ((next-subform (refs--read-with-offsets
                                 string offsets (1+ form-start-offset))))
              (while next-subform
                (setq next-subform
                      (refs--read-with-offsets
                       string offsets (refs--end-offset next-subform offsets))))))
          ;; This is lossy: if we read multiple identical forms, we
          ;; only store the position of the last one. TODO: store all.
          (ht-set! offsets form (list form-start-offset end-offset))
          form))
    ;; reached a closing paren.
    (invalid-read-syntax nil)))

(defun refs--read-all-with-offsets (string)
  "Read all the forms from STRING.
We return a list of all the forms, along with a hash table
mapping each form to its start and end offset."
  (let ((offsets (ht-create))
        (pos 0)
        (forms nil))
    ;; Read forms until we hit EOF, which raises an error.
    (ignore-errors
      (while t
        (let ((form (refs--read-with-offsets string offsets pos)))
          (push form forms)
          (setq pos (refs--end-offset form offsets)))))
    (list (nreverse forms) offsets)))

(defun refs--find-calls-1 (buffer form start-pos end-pos symbol)
  "If FORM contains any calls to SYMBOL, return those subforms, along
with their start and end positions.

Returns nil otherwise.

This is basic static analysis, so indirect function calls are
ignored."
  ;; TODO: Handle funcall to static symbols too.
  ;; TODO: Handle sharp-quoted function references.
  ;; TODO: (defun foo (bar baz)) is not a function call to bar.
  (cond
   ;; Base case: are we looking at (symbol ...)?
   ((and (consp form) (eq (car form) symbol))
    (list (list form start-pos end-pos)))
   ;; Recurse, so we can find (... (symbol ...) ...)
   ((and (consp form) (not (list-utils-improper-p form)))
    (let ((sexp-positions (refs--sexp-positions buffer start-pos end-pos))
          (found-calls nil))
      ;; Iterate through the subforms, calculating matching paren
      ;; positions so we know where we are in the source.
      (dolist (subform-and-pos (-zip form sexp-positions))
        (let ((subform (car subform-and-pos))
              (subform-start-end (cdr subform-and-pos)))
          (when (consp subform)
            (let* ((subform-start (car subform-start-end))
                   (subform-end (cadr subform-start-end)))
              (push
               (refs--find-calls-1 buffer subform
                                   subform-start subform-end symbol)
               found-calls)))))
      ;; Concat any results from the subforms.
      (-non-nil (apply #'append (nreverse found-calls)))))
   ;; If it's not a cons cell, it's definitely not a call.
   (t
    nil)))

(defun refs--find-calls (forms-with-positions buffer symbol)
  "If FORMS-WITH-POSITIONS contain any calls to SYMBOL,
return those subforms, along with their positions."
  (--mapcat
   ;; TODO: Use -let and destructuring to simplify this, and likewise
   ;; with cadr above.
   (let ((form (nth 0 it))
         (start-pos (nth 1 it))
         (end-pos (nth 2 it)))
     (refs--find-calls-1 buffer form start-pos end-pos symbol))
   forms-with-positions))

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
      (let ((el-name (format "%s.el" (f-no-ext it)))
            (el-gz-name (format "%s.el.gz" (f-no-ext it))))
        (cond ((f-exists? el-name) el-name)
              ;; TODO: make refs--file-contents handle gzipped files.
              ;; ((f-exists? el-gz-name) el-gz-name)
              ;; Ignore files where we can't find a .el file.
              (t nil)))
      elc-paths))))

(defun refs--contents-buffer (path)
  "Read PATH into a disposable buffer, and return it."
  (let ((fresh-buffer (generate-new-buffer (format "refs-%s" path))))
    (with-current-buffer fresh-buffer
      (insert-file-contents-literally path))
    fresh-buffer))

(defun refs--show-results (results)
  "Given a list where each element takes the form \(path . forms\),
render a friendly results buffer."
  ;; TODO: separate buffer per search.
  (let ((buf (get-buffer-create "*refs*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert (format "? results in %s files.\n" (length results)))
    (--each results
      (-let [(path . forms) it]
        (insert (format "File: %s\n" (f-short path)))
        (--each forms
          (insert (format "%s\n" it)))
        (insert "\n")))))

;; suggestion: format is a great function to use
(defun refs-function (symbol)
  "Display all the references to SYMBOL, a function."
  (interactive
   ;; TODO: default to function at point.
   (list (read (completing-read "Function: " (refs--functions)))))

  ;; TODO: build an index, since reading with offsets for >5 KLOC is slow.
  ;; TODO: profile refs--read-all-with-offsets
  ;; TODO: use the full loaded file list.
  (let* ((loaded-paths (-slice (refs--loaded-files) 0 3))
         (loaded-sources (-map #'refs--file-contents loaded-paths))
         (loaded-forms-and-offsets (-map #'refs--read-all-with-offsets loaded-sources))
         (loaded-forms (-map #'-first-item loaded-forms-and-offsets))
         (matching-forms (--map (refs--find-calls it symbol) loaded-forms))
         (all-paths-and-matches (-zip loaded-paths matching-forms))
         (paths-and-matches (--filter (consp (cdr it)) all-paths-and-matches)))
    (refs--show-results paths-and-matches)))

(defun refs--bench-read ()
  "Measure runtime of reading a large file."
  (interactive)
  (let ((start-time (current-time))
        end-time)
    (refs--read-all-with-offsets
     (refs--file-contents "/home/wilfred/src/lispy/lispy.el"))
    (setq end-time (current-time))
    (let ((start-time-secs (+ (cl-second start-time)
                              (/ (float (cl-third start-time)) 1000000)))
          (end-time-secs (+ (cl-second end-time)
                            (/ (float (cl-third end-time)) 1000000))))
      (message "Took %s seconds"
               (- end-time-secs start-time-secs)))))

(provide 'refs)
;;; refs.el ends here
