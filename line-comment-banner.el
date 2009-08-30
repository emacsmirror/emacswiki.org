;;; line-comment-banner.el --- Comment a line in a banner style.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Commentary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This  file is basically  to define  the command  `line-comment-banner', which
;; takes a line and comments it in a "banner style", which I use.
;;
;; For example, if we are in emacs-lisp-mode, so comment-start is ";", we might
;; want a line that looked like
;;
;;        |Hello world                                    |
;;
;; where | denotes the start and end of the line, to look something like
;;
;;        |;;;;;; Hello world ;;;;;;                      |
;;
;; which is the same message, but surrounded by comment characters (with a
;; prefix argument of 25, to set the fill width).
;;
;; Without a prefix argument, we default to a width of fill-column.
;;
;; I'd love to hear if anyone uses this! Let me know at rswarbrick@gmail.com.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Installing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To install this code, place it somewhere in your load path and either use the
;; simple
;;
;;   (require 'line-comment-banner)
;;
;; or an autoload:
;;
;;  (autoload 'line-comment-banner "line-comment-banner" nil t)
;;
;; Then, unless you like typing long M-x commands, use global-set-key for
;; something like
;;
;;  (global-set-key (kbd "C-;") 'line-comment-banner)
;;
;; (the logic behind this keybinding is that M-; does comment-dwim)


;;;;;;;;;;;;;;;;;;; Customising for complicated major modes ;;;;;;;;;;;;;;;;;;;;
;;
;; The code has  a slight difficulty with some  major modes, since comment-start
;; and comment-end  don't tell  you what "fill  character" to use.  For example,
;; when writing  C code, comment-start  is "/* ",  comment-end is " */".  How on
;; earth do we work out that the fill character is normally "*"?
;;
;; Well,  the  answer is  that  we  don't. However,  you  can  do  the work  for
;; us.  Setting the  variable comment-fill  will customise  this  behaviour. You
;; probably want to  make it buffer-local. To  get C to do the  right thing, use
;; the following incantation:
;;
;;   (add-hook 'c-mode-common-hook
;;             (lambda () (make-local-variable 'comment-fill)
;;                        (setq comment-fill "*")))
;;
;; and behold:
;;    /******* Comment *******/


;;;;;;;;;;;;;;;;;;;;;;;;;;; Copyright notice (yawn): ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright 2008 Rupert Swarbrick, except the chomp function, which was
;; retrieved from the EmacsWiki and is thus presumably under GPLv2.
;;
;; As far as the rest of the code is concerned:
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either  version 3 of the  License, or (at your  option) any later
;; version.
;;
;; This program is  distributed in the hope that it will  be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A PARTICULAR  PURPOSE.   See the  GNU  General Public  License for  more
;; details.
;;
;; You should have received a copy  of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; History ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Version 0.0.1
;;    17/08/08. Rupert Swarbrick. rswarbrick@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;; Code:
(defun on-blank-line-p ()
  "Return true if the current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[[:space:]]*$")))

(defun looking-at-move (regexp)
  "Call `looking-at', but then move forward through REGEXP."
  (when (looking-at regexp)
   (re-search-forward regexp)))

(defun looking-back-move (regexp)
  "Call `looking-back', but then move back through REGEXP."
  (when (looking-back regexp)
    (re-search-backward regexp)))

;; Courtesy of http://www.emacswiki.org/cgi-bin/wiki/ElispCookbook#toc5
(defun chomp (str)
  "Perl-like chomp function to return a version of STR with no whitespace."
  (let ((s (if (symbolp str)(symbol-name str) str)))
    (save-excursion
      (while (and
              (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
              (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
        (setq s (replace-match "" t nil s)))
      (while (and
              (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
              (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
        (setq s (replace-match "" t nil s))))
    s))

(defun skip-these-regexps (res &optional direction)
  "Skip buffer data given by RES.

Skip in the buffer forward or backward depending on DIRECTION
through the regular expressions specified by the list RES."
  (setf direction (or direction :forward))

  (let ((still-going t)
        (looking-func (cond
                       ((eq direction :forward) #'looking-at-move)
                       ((eq direction :backward) #'looking-back-move)
                       (t (error "DIRECTION not :forward or :backward")))))

    (while still-going
      (let ((found-something))
        (dolist (re res)
          (when (funcall looking-func re)
            (setf found-something t)))
        (unless found-something (setf still-going nil))))))

(defun line-comment-banner (&optional width)
  "Comment the current line as a banner.

Set the current line's contents inside comment characters so that
we create a banner, filling up to WIDTH if it's defined or
otherwise `fill-column'.  Indentation is inferred from the
position of the first non-white character on the line, so if the
line had 2 spaces and then a 'a', we would presume that the
comment should be indented to the 3rd column."

  (interactive "p")
  ;; no prefix argument => (= width 1)
  (unless (> width 1)
    (setf width fill-column))

  (when (on-blank-line-p)
    (error "Empty line to comment"))

  (unless comment-start
    (error "No comment syntax"))

  (let* ((our-comment-start (chomp comment-start))
         (our-comment-end (if (string= comment-end "")
                              our-comment-start
                            (chomp comment-end)))
         (our-comment-fill (if (and (boundp 'comment-fill)
                                    (symbol-value 'comment-fill)
                                    (not (string= (symbol-value 'comment-fill)
                                                  "")))
                               (symbol-value 'comment-fill)
                             our-comment-start)))
    
    (save-excursion
      ;; Calculate both the indent of the line (first non-blank) and the
      ;; positions of the start and end of the non-comment data.
      (beginning-of-line)
      (let ((indent-pos (or (looking-at-move "[[:blank:]]+")
                            (point)))

            (data-start-pos
             (progn (skip-these-regexps (list "[[:blank:]]+"
                                              (regexp-quote our-comment-start)
                                              (regexp-quote our-comment-fill))
                                        :forward)
                    (point)))

            (data-end-pos
             (progn (end-of-line)
                    (skip-these-regexps (list "[[:blank:]]+"
                                              (regexp-quote our-comment-end)
                                              (regexp-quote our-comment-fill))
                                        :backward)
                    (point))))

        ;; Calculate whether we can do the job. We use comment-end, not
        ;; our-comment-end, since insertion of the ending comment is sometimes
        ;; optional.
        (let* ((contents-width (- data-end-pos data-start-pos))
               (comment-width (+ contents-width
                                 (length our-comment-start)
                                 (min (length comment-end)
                                      (length our-comment-end))))
               (line-width (- width (- indent-pos (line-beginning-position)))))
          
          (when (> comment-width line-width)
            (error "The comment will be too big (%d) to fit in width %d"
                   comment-width line-width))
        
          (let* ((lfilling (/ (- line-width contents-width) 2))
                 (rfilling (+ lfilling (% (- line-width contents-width) 2))))

            ;; The error here shouldn't be able to be triggered: we know we have
            ;; enough space in total. So try to squeeze in some more space on
            ;; the left if necessary.
            (while (< lfilling (length our-comment-start))
              (when (= rfilling 0)
                (error "Can't fit in comment start"))
              (setf lfilling (1+ lfilling)
                    rfilling (1- rfilling)))
          
            ;; Delete the random cruft and put ourselves at the start of the
            ;; data.
            (delete-region data-end-pos (line-end-position))
            (delete-region indent-pos data-start-pos)
            (goto-char indent-pos)

            ;; Fill up the left comment chars and one or more spaces.
            (insert our-comment-start)
            (setf lfilling (- lfilling (length our-comment-start)))
            (while (> (- lfilling (length our-comment-fill)) 0)
              (setf lfilling (- lfilling (length our-comment-fill)))
              (insert our-comment-fill))
            (while (> lfilling 0)
              (setf lfilling (1- lfilling))
              (insert " "))

            ;; Now go to the end of the line and fill up with comment-fill
            ;; characters, finishing with a comment-end.
            (end-of-line)
            (when (>= rfilling (length our-comment-end))
              (save-excursion
                (insert our-comment-end))
              (setf rfilling (- rfilling (length our-comment-end))))
            (save-excursion
              (while (> (- rfilling (length our-comment-fill)) 0)
                (setf rfilling (- rfilling (length our-comment-fill)))
                (insert our-comment-fill)))
            (while (> rfilling 0)
              (setf rfilling (1- rfilling))
              (insert " "))))))))

(provide 'line-comment-banner)

;;; line-comment-banner.el ends here
