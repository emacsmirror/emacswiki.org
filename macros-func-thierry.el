;;; macros-func-thierry.el --- My lisp macros and functions

;; Author: Thierry Volpiatto

;; Copyright (C) 2008 Thierry Volpiatto
;;
;; this file is NOT part of GNU Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA


;;; Code:

(eval-when-compile (require 'cl))

;; Files processing

;; (defun tv-list-directory (dirname &optional abs)
;;   "Use directory-files without these \".\" \"..\"
;; if abs use absolute path"
;;   (cddr (directory-files dirname abs)))

(defmacro tv-list-directory (dirname &optional abs)
  "Use directory-files without these \".\" \"..\".
If abs is non-nil use absolute path.
Check a second time with mapcar we have not \".\" or \"..\"
in case we have a directory with crappy files
This to avoid infinite loop in walk"
  `(let ((clean-dir (cddr (directory-files ,dirname ,abs))))
     (mapcar #'(lambda (x)
                 (when (and (not (equal (file-name-nondirectory x)
                                        "."))
                            (not (equal (file-name-nondirectory x)
                                        "..")))
                   x))
             clean-dir)))

;; TODO: walk==>add func to manage subdir in args
;; TODO: walk==>excludes args should be externals func?
;; TODO: walk==>function should return t or nil

(defun tv-walk-directory (dirname file-fn &optional exclude-files exclude-dirs)
    "Walk through dirname and use file-fn function
on each file found.
`dirname' ==> we start in this directory
`file-fn' ==> function to apply to FILES
`excludes-files' ==> list of .ext to ignore  
`exclude-dirs' ==> list of directory to ignore

Example:

,----
| (tv-walk-directory \"~/labo/traverse-work\"
|                    #'(lambda (y)
|                        (princ y)
|                        (terpri))
|                    '(\".elc\" \".el\")
|                    '(\".hg\"))
`----

"
    (labels
        ((walk (name)
           (cond ((and (file-directory-p name) ;; DIR PROCESSING
                       (not (file-symlink-p name))) ;; don't follow symlinks
                  (if exclude-dirs
                      (dolist (x (tv-list-directory name t))
                        (if x ;; be sure x is a string and not nil
                            (unless (member (file-name-nondirectory x) exclude-dirs)
                              (walk x)))) ;; Return to TOP and take the good cond
                      (dolist (x (tv-list-directory name t))
                        (if x
                            (walk x))))) ;; Return to TOP and take the good cond
                 ((and (file-regular-p name) ;; FILE PROCESSING
                       (not (file-symlink-p name))) ;; don't follow symlinks
                  (if exclude-files
                      (unless (member (file-name-extension name t) exclude-files)
                        (funcall file-fn name))
                      (funcall file-fn name))))))
      (walk (expand-file-name dirname))))
      ;;(garbage-collect)))

(defmacro tv-readlines (file &optional delete-empty-lines)
  "Return a list where elements are the lines of a file
\\(emulate object.readlines() of python)"
  `(let* ((my-string (with-temp-buffer
                       (insert-file-contents ,file)
                       (buffer-string)))
          (my-read-list (split-string my-string "\n")))
     (when ,delete-empty-lines
       (dolist (i my-read-list)
         (when (equal i "")
           (delete i my-read-list))))
     my-read-list))

;;;###autoload
(defun tv-find-first-regex-in-file (regex file)
  "Stop at the first match of regex and return line
as string"
  (let ((file-list (tv-readlines file)))
    (catch 'break
      (dolist (i file-list)
        (when (string-match regex i)
          (throw 'break
            i))))))

;; TODO: find-all-regex==>Fix position in file is wrong. 
;;;###autoload
(defun tv-find-all-regex-in-file (regex file)
  "Return a list with elements of the form :
'(matched-line char-pos line-pos)
Example:
,----
| ELISP> (tv-find-all-regex-in-file \"ligne\" \"~/toto\")
| ((\"avec une premiere ligne\" 62 2)
|  (\"une deuxieme ligne\" 93 3)
|  (\"une troisieme ligne\" 126 4))
`----
"
  (let ((infile-list (tv-readlines file))
        (outfile-list nil)
        (count-line 0)
        (count-char 0)
        (pos-match)
        (tmp-count-char 0))
    (dolist (i infile-list)
      (setq count-line (+ count-line 1))
      (setq pos-match (string-match regex i)) 
      (setq count-char (+ count-char (if (equal i "")
                                         1 ;; "\n"
                                         (length i))))
      (when pos-match
        (setq tmp-count-char (+ (length regex)
                                (- count-char (- (length i) pos-match))))
        (add-to-list 'outfile-list `(,i ,tmp-count-char ,count-line) t)))
    outfile-list))
        
;; Processing lists
;;;###autoload
(defun tv-index-elt-list (elm lis)
  "return index of `elm' in `lis'
`elm' is an element of the list `lis'"
  (let ((n 0)
        (index 0))
    (if (member elm lis)
        (progn
          (dolist (x lis)
            (when (equal x elm)
              (setq index n))
            (setq n (+ n 1)))
          index)
      (error "No element %s in %s" elm lis)))) ; TODO corriger pour CL

;;;###autoload
(defun tv-move-element-in-list (name-elm lis where)
  "move element `name-elm' of list `lis' to index `where'
in list `lis'.
`name-elm' have the form of element in list.
`lis' is a LIST
`where' is an INTEGER"
  (let* ((index-elm (tv-index-elt-list name-elm lis))
         (start-part-list (subseq lis 0 where))
         (mod-list (append (remove name-elm start-part-list)
                           (cons name-elm
                                 (remove name-elm (subseq lis where))))))
    mod-list))

;;;###autoload
(defun tv-move-assk-element-in-list (name-ass-key-elm lis where)
  "move element `name-ass-key-elm' of list `lis' to index `where'
in list `lis'.
`name-ass-key-elm' is the key of an ALIST.
`lis' is a ALIST
`where' is an INTEGER"
  (let* ((index-elm (tv-index-elt-list (assoc name-ass-key-elm lis) lis))
         (start-part-list (subseq lis 0 where))
         (name-elm (assoc name-ass-key-elm lis))
         (mod-list (append (remove name-elm start-part-list)
                           (cons name-elm
                                 (remove name-elm (subseq lis where))))))
    mod-list))

;;;###autoload
(defun tv-add-to-list-at-ind (elm lis where)
  "Add `elm' in `lis' at index `where'"
  (let ((cons-list (cons elm lis))
        (appended-list nil))
    (setq appended-list
          (tv-move-element-in-list elm cons-list (+ 1 where)))
    appended-list))

;;;###autoload
(defun tv-add-at-ind-in-subtree (elm lis where &optional subtree)
  "Add `elm' in `lis' or sublist at index `where'

elm ==> any element of a list
lis ==> the main list
where ==> a number, index to use of list or sublist
subtree ==> the sublist:
any quoted list or function that return a sublist of lis

Example:

,----
| ELISP> (setq A '(a b c d e f g h))
| (a b c d e f g h)
| ELISP> (setq B (cons '(1 2 3) A))
| ((1 2 3)
|  a b c d e f g h)
| ELISP> (tv-add-at-ind-in-subtree 'i B 2 '(1 2 3))
| ((1 2 i 3)
|  a b c d e f g h)
| ;; If subtree is not given:
| ELISP> (tv-add-at-ind-in-subtree 'i A 2)
| (a b i c d e f g h)
`----

"
  (let* ((subtree-index (when subtree
                          (tv-index-elt-list subtree lis)))
         (list-to-use (if subtree
                          (nth subtree-index lis)
                        lis))
        (cons-list (cons elm list-to-use))
        (appended-list nil))
    (setq appended-list
          (tv-move-element-in-list elm cons-list (+ 1 where)))
    (if subtree
        (tv-add-to-list-at-ind appended-list (remove subtree lis) subtree-index)
    appended-list)))

;;;###autoload
(defun tv-move-elm-in-list-or-sublist (name-elm lis where &optional subtree)
  "move element `name-elm' of list `lis' to index `where' in list `lis'

elm ==> any element of a list
lis ==> the main list
where ==> a number, index to use of list or sublist
subtree ==> the sublist:
any quoted list or function that return a sublist of lis

Example:
TODO insert examples

"
  (let* ((subtree-index (when subtree
                          (tv-index-elt-list subtree lis)))
         (list-to-use (if subtree
                          (nth subtree-index lis)
                        lis))
         (modif-list (if (member name-elm lis)
                         (tv-add-to-list-at-ind name-elm list-to-use where)
                       (tv-move-element-in-list name-elm list-to-use where))))
    (if subtree
        (cond ((member name-elm lis)
               (tv-add-to-list-at-ind modif-list (remove subtree (remove name-elm lis)) subtree-index))
              ((member name-elm subtree)
               (let ((append-list (tv-add-to-list-at-ind name-elm (remove subtree lis) where)))
                 (tv-add-to-list-at-ind (remove name-elm subtree) append-list subtree-index)))
              (t
               (tv-add-to-list-at-ind modif-list (remove subtree lis) subtree-index)))
      modif-list)))

;; TODO implement moving from a sub to a sub (will be useful for dvc)
;; TODO correct that nil that appear when moving to sub
;; TODO if index == length tree or subtree ??? fixit

;; (defun tv-move-elm-in-sub-to-sub (elm-to-move where main-list sublist1 sublist2)
;;   (let ((pos-sub1 (tv-index-elt-list sublist1 main-list))
;;         (pos-sub2 (tv-index-elt-list sublist2 main-list))
;;         (sub1)
;;         (sub2)
;;         (tmp-list))
;;     (setq sub2 (tv-add-to-list-at-ind elm-to-move (nth pos-sub2 main-list) where))
;;     (setq sub1 (remove elm-to-move (nth pos-sub1 main-list)))
;;     (setq tmp-list (tv-add-to-list-at-ind sub1 (remove sublist1 (remove sublist2 main-list)) pos-sub1))
;;     (setq main-list (tv-add-to-list-at-ind sub2 tmp-list pos-sub2))
;;     main-list))

;;; Hash-tables -- emulate here methods of python dictionaries

;; return t if key exist nil otherwise (i use memq here: test ==> eq)
;; member is needed to work on cl.
(defmacro hash-has-key (key hash-table)
  "check if hash-table have key key
key here must be a symbol and not a string"
  `(let ((keys-list (hash-get-symbol-keys ,hash-table)))
     (if (memq ,key keys-list)
         t
       nil)))

;; get items of hash table -- return a list with (key value) as elements
(defmacro hash-get-items (hash-table)
  "Get the list of all keys/values of hash-table
values are given under string form"
  `(let ((li-items nil)) 
    (maphash #'(lambda (x y) (push (list x y) li-items))
             ,hash-table)
    li-items))

;; get values of hash-table in string form (they are already in string form)
(defmacro hash-get-values (hash-table)
  "Get the list of all values of hash-table
values are given under string form"
  `(let ((li-values nil)
         (li-all (hash-get-items ,hash-table)))
     (setq li-values (mapcar #'cadr li-all))
     li-values))

;; get keys of hash-table in symbol form
(defmacro hash-get-symbol-keys (hash-table)
  "Get the list of all the keys in hash-table
keys are given under string form"
  `(let ((li-keys nil)
         (li-all (hash-get-items ,hash-table)))
     (setq li-keys (mapcar #'car li-all))
     li-keys))

;; get keys of hash-table in string form
(defmacro hash-get-string-keys (hash-table)
  "Get the list of all the keys in hash-table
keys are given under string form"
  `(let ((li-keys nil)
         (li-all (hash-get-items ,hash-table))
         (li-keys-str nil))
     (setq li-keys (mapcar #'car li-all))
     (dolist (i li-keys)
       (push (symbol-name i) li-keys-str))
     li-keys-str))

;; define elisp puthash for cl
(defmacro cl-put-hash (key table value)
  `(setf (gethash ,key ,table) ,value))

;;; Time functions
;;;###autoload
(defun tv-time-date-in-n-days (days)
  "Return the date in string form in n +/-days
like ==>2008.03.16"
  (let* ((days-in-sec (* 3600 (* (+ days) 24)))
         (interval-days-sec (if `(< ,days 0)
                                (+ (float-time (current-time)) days-in-sec)
                              (- (float-time (current-time)) days-in-sec)))
         (sec-to-time (seconds-to-time interval-days-sec))
         (time-dec (decode-time sec-to-time))
         (new-date ""))
    (setq new-date (concat
                    (int-to-string (nth 5 time-dec))
                    "."
                    (substring (int-to-string (/ (float (nth 4 time-dec)) 100)) 2)
                    "."
                    (if (< (length (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2)) 2)
                        (concat (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2) "0")
                      (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2))))
    new-date))

;;;###autoload
(defun tv-cur-date-string ()
  "Return current date under string form ==>2008.03.16"
  (interactive)
  (let ((year (nth 5 (decode-time (current-time))))
        (month (nth 4 (decode-time (current-time))))
        (day (nth 3 (decode-time (current-time))))
        (str-day-date ""))
    (setq str-day-date
          (concat (int-to-string year)
                  "."
                  (substring (int-to-string (/ (float month) 100)) 2)
                  "."
                  (if (< (length (substring (int-to-string (/ (float day) 100)) 2)) 2)
                      (concat (substring (int-to-string (/ (float day) 100)) 2) "0")
                    (substring (int-to-string (/ (float day) 100)) 2))))
    str-day-date))

;;; end of file
