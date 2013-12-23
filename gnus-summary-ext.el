;;; gnus-summary-ext.el --- Extra limit and process mark commands for the gnus summary buffer

;; Filename: gnus-summary-ext.el
;; Description: Extra limit and process mark commands for the gnus summary buffer
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-12-23 00:06:16
;; Version: 0.1
;; Last-Updated: 2013-12-23 00:06:16
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/gnus-summary-ext
;; Keywords: comm
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((macro-utils "1.0"))
;;
;; Features that might be required by this library:
;;
;; gnus, macro-utils, cl
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 16mhw12vvtzCwQyvvLAqCSBvf8i41CfKhK
;;
;; This library provides extra limit commands for filtering the gnus summary buffer,
;; some commands for performing actions on MIME parts in articles,
;; and some general functions for evaluating elisp code in all articles that have the
;; process mark. See the documentation of the individual commands & functions for more
;; details.
;;;;

;;; Commands:
;;
;; Below is a complete command list:
;;
;;  `gnus-summary-ext-limit-to-mime-type'
;;    Limit the summary buffer to articles containing MIME parts with types matching REGEX.
;;  `gnus-summary-ext-apply-to-marked-safely'
;;    Evaluate any lisp expression for all articles that are process/prefixed.
;;  `gnus-summary-ext-apply-to-marked'
;;    Evaluate any lisp expression for all articles that are process/prefixed.
;;  `gnus-summary-ext-limit-to-num-parts'
;;    Limit the summary buffer to articles containing between MIN & MAX attachments.
;;  `gnus-summary-ext-limit-to-size'
;;    Limit the summary buffer to articles of size between MIN and MAX bytes.
;;  `gnus-summary-ext-limit-to-filename'
;;    Limit the summary buffer to articles containing attachments with names matching REGEX.
;;  `gnus-summary-ext-mime-action-on-parts'
;;    Do something with all MIME parts in the current buffer for which PRED evaluates to non-nil.    
;;  `gnus-summary-ext-act-on-parts-in-marked'
;;    Do something with all MIME parts in articles that are process/prefixed.

;;; Installation:
;;
;; Put gnus-summary-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'gnus-summary-ext)


;;; Change log:
;;	
;; 2013/12/23
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require
(require 'gnus)
(require 'macro-utils)
(eval-when-compile 'cl)

;;; Code:

(defun gnus-summary-ext-match-mime-types (regex)
  "Return list of MIME media types matching REGEX."
  (remove-if-not (lambda (x) (string-match regex x))
                 (mailcap-mime-types)))


;;;###autoload
(defun gnus-summary-ext-limit-to-mime-type (regex &optional reverse)
  "Limit the summary buffer to articles containing MIME parts with types matching REGEX.
If REVERSE (the prefix), limit to articles that don't match."
  (interactive "sMatch MIME type (regexp): \nP")
  (gnus-summary-limit-to-bodies
   (concat "Content-Type: " 
           (regexp-opt (gnus-summary-ext-match-mime-types regex))) reverse))


(defmacro gnus-summary-ext-iterate-articles-safely (articles &rest body)
  "Loop over all ARTICLES and perform BODY within each article buffer.
All hooks will be disabled before selecting each article."
  `(let ((gnus-select-article-hook nil)	;Disable hook.
         (gnus-article-prepare-hook nil)
         (gnus-use-article-prefetch nil)
         (gnus-keep-backlog nil)
         (gnus-break-pages nil)
         (gnus-summary-display-arrow nil)
         (gnus-updated-mode-lines nil)
         (gnus-auto-center-summary nil)
         (gnus-display-mime-function nil))
     (dolist (article ,articles)
       (let (gnus-mark-article-hook)
         (gnus-summary-select-article t t nil article))
       (with-current-buffer gnus-article-buffer
         ,@body))))

;;;###autoload
(defun gnus-summary-ext-apply-to-marked-safely (arg sexp)
  "Evaluate any lisp expression for all articles that are process/prefixed.
This will evaluate SEXP after selecting each article, but will not run any hooks.

See `gnus-summary-apply-to-marked' if you want to run the appropriate hooks after
selecting each article, and see `gnus-summary-iterate' for iterating over articles
without selecting them."
  (interactive "P\nxLisp expression: ")
  (gnus-summary-ext-iterate-articles-safely
   (gnus-summary-work-articles arg)
   (article-goto-body)
   (let (gnus-newsgroup-processable)
     (eval sexp))
   (gnus-summary-remove-process-mark article))
  (gnus-summary-position-point))

;;;###autoload
(defun gnus-summary-ext-apply-to-marked (arg sexp)
  "Evaluate any lisp expression for all articles that are process/prefixed.
This will evaluate SEXP after selecting each article, and running any hooks.

See `gnus-summary-ext-apply-to-marked-safely' for selecting each article without running hooks,
and see `gnus-summary-iterate' for iterating over articles without selecting them."
  (interactive "P\nxLisp expression: ")
  (dolist (article (gnus-summary-work-articles arg))
    (gnus-summary-select-article t t nil article)
    (with-current-buffer gnus-article-buffer
      (article-goto-body)
      (eval sexp))))

;;;###autoload
(defun gnus-summary-ext-limit-to-num-parts (min max &optional reverse)
  "Limit the summary buffer to articles containing between MIN & MAX attachments.
If MIN/MAX is nil then limit to articles with at most/least MAX/MIN attachments respectively.
If REVERSE (the prefix), limit to articles that don't match."
  (interactive (list (read-number "Min parts: " 1)
                     (read-number "Max parts: " 1000)
                     current-prefix-arg))
  (let ((min (or min 1))
        (max (or max 1000))
        articles)
    (gnus-summary-ext-iterate-articles-safely
     (mapcar 'car gnus-newsgroup-data)
     (article-goto-body)
     (let ((num (- (length (or gnus-article-mime-handles
                               (mm-dissect-buffer nil gnus-article-loose-mime)
                               (and gnus-article-emulate-mime
                                    (mm-uu-dissect)))) 2)))
       (when (and (>= num min) (<= num max))
         (push article articles))))
    (if (not articles)
        (message "No messages matched")
      (gnus-summary-limit articles)))
  (gnus-summary-position-point))

;;;###autoload
(defun gnus-summary-ext-limit-to-size (min max &optional reverse)
  "Limit the summary buffer to articles of size between MIN and MAX bytes.
If MIN/MAX is nil then limit to sizes below/above MAX/MIN respectively.
If REVERSE (the prefix), limit to articles that don't match.

Note: the articles returned might not match the size constraints exactly, but it should be fairly close."
  (interactive (list (read-number "Min bytes: " 0)
                     (read-number "Max bytes: " 999999999999)
                     current-prefix-arg))
  (let ((min (or min -1))
        (max (or max 999999999999))
        articles)
    (gnus-summary-ext-iterate-articles-safely
     (mapcar 'car gnus-newsgroup-data)
     (article-goto-body)
     (let ((size (buffer-size)))
       (when (and (>= size min) (<= size max))
         (push article articles))))
    (if (not articles)
        (message "No messages matched")
      (gnus-summary-limit articles)))
    (gnus-summary-position-point))

;;;###autoload
(defun gnus-summary-ext-limit-to-filename (regex &optional reverse)
  "Limit the summary buffer to articles containing attachments with names matching REGEX.
If REVERSE (the prefix), limit to articles that don't match.
Note: REGEX should match the whole filename, so you may need to put .* at the beginning and end."
  (interactive "sMatch filename (regexp): \nP")
  (gnus-summary-limit-to-bodies
   (concat "Content-Disposition: attachment; filename=" regex) reverse))

;;;###autoload
(defun* gnus-summary-ext-mime-action-on-parts (action &optional (pred t))
  "Do something with all MIME parts in the current buffer for which PRED evaluates to non-nil.
PRED should be a form that evaluates to non-nil for parts to be acted on.
By default PRED is t, and so all parts are acted on.
PRED will be placed within a let form where handle is bound to the handle for the part,
size is the number of chars in the part, type is the MIME type (e.g. \"image/png\"),
subtype is the subtype (e.g. \"png\"), supertype is the supertype (e.g. \"image\"),
and filename is the filename."
  (interactive
   (list (gnus-completing-read "Action" (mapcar 'car gnus-mime-action-alist) t)
         (let ((val (read-from-minibuffer "Variables available in lisp expression:
handle = handle for part, size = No. of chars in part, type = MIME type (e.g. \"image/png\")
subtype = subtype (e.g. \"png\"), supertype = supertype (e.g. \"image\")

Lisp expression matching parts (default t): "
                                          nil nil nil 'read-expression-history)))
           (if (equal val "") t (read val)))))
  (gnus-article-check-buffer)
  (let* ((action-pair (assoc action gnus-mime-action-alist))
         (n 2))
    (if action-pair
        (while (gnus-article-goto-part n)
          (let* ((handle (get-text-property (point) 'gnus-data))
                 (size (buffer-size (mm-handle-buffer handle)))
                 (type (mm-handle-media-type handle))
                 (subtype (mm-handle-media-subtype handle))
                 (supertype (mm-handle-media-supertype handle))
                 (filename (mm-handle-filename handle)))
            (if (eval pred) (funcall (cdr action-pair))))
          (setq n (1+ n))))))

;;;###autoload
(defun gnus-summary-ext-act-on-parts-in-marked (arg &optional action pred)
  "Do something with all MIME parts in articles that are process/prefixed.
Only MIME parts for which PRED evaluates to non-nil will be acted on.
See `gnus-summary-ext-mime-action-on-parts' for a description of the ACTION and PRED args.
This command just applies that function to the articles."
  (interactive
   (list current-prefix-arg
         (gnus-completing-read "Action" (mapcar 'car gnus-mime-action-alist) t)
         (let ((val (read-from-minibuffer "Variables available in lisp expression:
handle = handle for part, size = No. of chars in part, type = MIME type (e.g. \"image/png\")
subtype = subtype (e.g. \"png\"), supertype = supertype (e.g. \"image\")

Lisp expression matching parts (default t): "
                                          nil nil nil 'read-expression-history)))
           (if (equal val "") t (read val)))))
  (gnus-summary-ext-apply-to-marked arg `(gnus-summary-ext-mime-action-on-parts ,action ',pred)))

(provide 'gnus-summary-ext)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "gnus-summary-ext.el" (buffer-name) (buffer-string) "update")

;;; gnus-summary-ext.el ends here
