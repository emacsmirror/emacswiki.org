;;; wikidoc.el --- use elisp doc strings to make other documentation

;; Copyright (C) 2010  Nic Ferrier

;; Author: Nic Ferrier <nic@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nic@ferrier.me.uk>
;; Created: 5th October 2010
;; Version: 0.1
;; Keywords: lisp

;; This file is NOT part of GNU Emacs.

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
;;
;; This is for making documentation from Elisp doc strings. It is
;; intended as an alternative to using info.
;;
;; Right now, the only output format is creole-wiki. But that could
;; change.

;;; Source code
;;
;; The wikidoc source code is not in a public repo yet.

;;; Style note
;;
;; This codes uses the emacs style of:
;;
;;    wikidoc---private-function 
;;
;; for private functions.
;;
;;; Examples
;; 
;; This is the main paragraph converter:
;;
;; (wikidoc--convert "this is a function.
;;  this is an example
;;  this should 'not' be changed to a link
;; This is the main text. It can include references to other 'functions'.
;; When there are 'two' references 'on the same line' it should
;; still work.")

;;;###autoload
(defun wikidoc-grab-list (prefix &optional no-private)
  "Grab a list or functions matching PREFIX possibly with NO-PRIVATE"
  (let ((prefix-sym (symbol-name prefix))
        (res '()))
    (mapatoms 
     (lambda (atom)
       (let ((sn (symbol-name atom)))
         (and (fboundp atom)
              (if (string-prefix-p prefix-sym sn)
                  (if no-private
                      (if (not (string-match "[^-]+--.*" sn))
                          (setq res (cons atom res)))
                    (setq res (cons atom res)))))))
     obarray)
    res))

(defun wikidoc--convert-line (line)
  "Converts a single LINE of function documentation.

This deals with things like quoted LISP forms which can be turned into links"
  (save-match-data
    (while (string-match ".*\\('[^']+'\\).*" line)
      (setq line (replace-match 
                  (format "[[%s]]" (let ((name (match-string 1 line)))
                                     (save-match-data 
                                       (string-match "'\\([^']+\\)'" name)
                                       (match-string 1 name))))
                  nil nil line 1))) line))

(defun wikidoc--convert (str)
  "Convert function documentation type doc STR to creole."
  (let (in-pre
        )
    (mapconcat
     (lambda (line)
       (cond
        ((string-match "^ " line)
         (if in-pre
             line
           (progn 
             (setq in-pre 't)
             (concat "{{{\n" (wikidoc--convert-line line)))))
        ((and in-pre (not (string-match "^ " line)))
         (setq in-pre nil)         
         (concat "}}}\n" (wikidoc--convert-line line)))
        ('t 
         (wikidoc--convert-line line))))
     (split-string str "\n")
     "\n")))

;;;###autoload
(defun wikidoc-insert (elisp-prefix buffer)
  "Make creole doc for functions beginning with ELISP-PREFIX in BUFFER.

When called interactively with a PREFIX argument this function
will use the current buffer for BUFFER. 

Otherwise the BUFFER will be created named like:

 *wikidoc-ELISP-PREFIX*

If Transient Mark mode is set in the specified buffer the active
region is killed before the new wiki text is inserted.
"
  (interactive 
   (let ((elisp-prefix (completing-read "elisp prefix: " obarray 't nil nil nil)))
     (list (intern elisp-prefix)    
           (if current-prefix-arg
               (current-buffer)
             nil))))
  (let* ((lst (sort 
               (wikidoc-grab-list elisp-prefix 't)
               'string-lessp))
         ;; We're relying on dynamic scope here to set buffer later
         (mapfn (lambda (fn)
                  (with-current-buffer buffer
                    (insert (format "=== %s %s ===\n\n%s\n\n\n" 
                                    (symbol-name fn) 
                                    ;; Could also capture the keywords here in some sort of hash
                                    ;; so we can refer to them easily
                                    (let ((args (help-function-arglist fn)))
                                      (upcase (format "%s" args)))
                                    (wikidoc--convert (documentation fn))))))))
    (if (not (bufferp buffer))
        (progn
          (setq buffer (get-buffer-create (format "*wikidoc-%s*" elisp-prefix)))
          (mapc mapfn lst)
          (switch-to-buffer buffer))
      (progn
        (if (use-region-p)
            (delete-region (region-beginning) (region-end)))
        (mapc mapfn lst)))))

;; End
