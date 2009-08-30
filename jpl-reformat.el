;;; jpl-reformat.el --- Reformat source code, especially parameter
;;; lists and whitespace
;;
;; Copyright (C) 2008, Johan Lindstrom
;;
;; Author: Johan Lindstrom <johanl [AT] buzzwordninja.com>
;; Created: 2008-01-03
;;
;; Version: 0.1
;;
;;
;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.
;;
;;
;;
;;; Commentary:
;;
;;
;; Reformat parameter lists easily from single line to multiple
;; lines and back again. E.g. between this:
;;
;;     API::Link->new({rel => 'meta:version', href => $uri->as_string, type => 'application/xml'}),
;;
;; and this:
;;
;;     API::Link->new({
;;         rel => 'meta:version',
;;         href => $uri->as_string,
;;         type => 'application/xml',
;;     }),
;;
;; The parameters are specified by the enclosing element, with either
;; () or {} braces.
;;
;; The multiline format can also be aligned properly within the
;; enclosing braced element, to end up like this:
;;
;;     API::Link->new({
;;         rel  => 'meta:version',
;;         href => $uri->as_string,
;;         type => 'application/xml',
;;     }),
;;
;;
;;
;; The user interface for this package consists of the following commands:
;;
;;     jpl-reformat-mark-enclosing-block
;;     jpl-reformat-align-enclosing-block
;;     jpl-reformat-parameter-list-to-single-line
;;     jpl-reformat-parameter-list-to-multi-line
;;     jpl-reformat-parameter-list-toggle-multiple-single
;;
;;
;;
;;; Setup:
;;
;; (require 'jpl-reformat)
;; (global-set-key (kbd "C-S-u") 'jpl-reformat-mark-enclosing-block)
;; (global-set-key (kbd "\C-o m a") 'jpl-reformat-align-enclosing-block)
;; (global-set-key (kbd "\C-o m p") 'jpl-reformat-parameter-list-toggle-multiple-single)
;;
;; or whatever keys you'r like to use. If you use PerlySense
;;
;;   http://search.cpan.org/dist/Devel-PerlySense/lib/Devel/PerlySense.pm
;;
;; then the above key bindings are suitable, otherwise you probably
;; want to assign other C-c prefixed keys.
;; 



;;; Code:



(require 'cperl-mode)
(require 'thingatpt)
(require 'newcomment)



(defun jpl-reformat-parameter-list-toggle-multiple-single ()
  "Reformat a list of parameters enclosed in () or {} into a
nicely formatted single lines, or multiple lines depending on
what it currently is."
  (interactive)

  (if (save-excursion
        (jpl-reformat-mark-enclosing-block)
        (= (jpl-reformat-line-of-pos (point)) (jpl-reformat-line-of-pos (mark)))
        )
      (jpl-reformat-parameter-list-to-multi-line nil)
    (jpl-reformat-parameter-list-to-single-line)
    )
  )


(defun jpl-reformat-parameter-list-to-single-line ()
  "Reformat a list of parameters enclosed in () or {} into a
nicely formatted single line. Trailing comments are stacked on
top."
  (interactive)

  (jpl-reformat-mark-enclosing-block)

  (save-excursion
    (goto-char (+ 1 (point)))

    ;; After opening brace. Bring up the first line
    (jpl-reformat-collapse-all-whitespace)

    (while (search-forward-regexp "," (mark) t)
      (if (not (or (jpl-reformat-in-comment-p) (in-string-p)))
          (progn
            (if (looking-back " " 2) (jpl-reformat-collapse-all-whitespace))
            (jpl-reformat-collapse-whitespace)
            )
        )
      )

    ;; Remove possibly trailing ,
    (goto-char (- (mark) 1))
    (if (looking-back ", " 3) (delete-backward-char 2))
    (jpl-reformat-collapse-all-whitespace)
    )

  (forward-char 1)
  (save-excursion
    (jpl-reformat-mark-enclosing-block)
    (goto-char (+ 1 (point)))

    (while (search-forward-regexp "[ ]*=>[ ]*" (mark) t)
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0)))
        (if (not (or (jpl-reformat-in-comment-p) (in-string-p)))
            (progn
              (delete-region match-beg match-end)
              (goto-char match-beg)
              (insert " => ")
              )
          )
        )
      )
    )
  )



(defun jpl-reformat-parameter-list-to-multi-line (arg)
  "Reformat a list of parameters enclosed in () or {} into nicely
formatted multiple lines. Line breaks after each parameter."
  (interactive "P")

  (save-excursion
    (jpl-reformat-mark-enclosing-block)
    (goto-char (+ 1 (point)))

    ;; After opening brace. Move down the first line
    (jpl-reformat-collapse-whitespace)
    (newline-and-indent)

    ;; Note: mark will move as we insert new lines, indentation
    ;; etc. That's good.
    (while (search-forward-regexp "," (mark) t)
      (if (not (or (jpl-reformat-in-comment-p) (in-string-p)))
          (progn
            (if (looking-back "\ " 5)
                (progn   (backward-char 1)
                         (jpl-reformat-collapse-all-whitespace)))

            (indent-according-to-mode)

            (if (save-excursion
                  (and (search-forward-regexp "[,#]" (point-at-eol) t)
                       (looking-back "#" 2)))
                (progn
                  (next-line)
                  (beginning-of-line))
              (progn
                (jpl-reformat-collapse-whitespace)
                (newline-and-indent))
              )
            )
        )
      )

    ;; Last line
    (goto-char (- (mark) 1))
    (jpl-reformat-collapse-whitespace)
    (newline-and-indent)

    ;; Possibly add trailing ,
    (previous-line-nomark)
    (cperl-to-comment-or-eol)
    (if (not (search-backward-regexp ",\\ *" (point-at-bol) t))
        (progn
          (if (search-backward-regexp "[^ ]" (point-at-bol) t)
              (progn
                (forward-char 1)
                (insert ",")
                )
            )
          )
      )

    )

  ;; Possibly align stuff in the block
  (if arg (jpl-reformat-align-enclosing-block))

  )



(defun jpl-reformat-in-comment-p ()
  "Return a true value if point is in a comment, else nil"
  (save-excursion
    (comment-beginning)
    )
  )



(defun jpl-reformat-collapse-whitespace ()
   "Reduce all whitespace surrounding point to a single space."
   ;; @@ This seems to be quite buggy at the moment
   (interactive)
   (kill-region (progn (re-search-backward "[^ \t\r\n]")
                       (forward-char)
                       (point))
                (progn (re-search-forward "[^ \t\r\n]")
                       (backward-char)
                       (point)))
   (insert-char ?\  1)
   )



(defun jpl-reformat-collapse-all-whitespace ()
  "Like jpl-reformat-collapse-whitespace, but remove all whitespace"
  (interactive)
  (jpl-reformat-collapse-whitespace)
  (if (looking-back " ") (delete-char -1))
  (forward-char 1)
  )



(defun jpl-reformat-mark-enclosing-block ()
  "Select the enclosing block, () or {}"
  (interactive)

  (while (in-string-p) (backward-char 1))
  (backward-up-list -1)  ;;Move to end of enclosing braces
  (push-mark)
  (ensure-mark)
  (forward-list -1)      ;;Move to beginning of enclosing braces
  )



(defun jpl-reformat-align-enclosing-block ()
  "Align the enclosing block"
  (interactive)
  (save-excursion
    (jpl-reformat-mark-enclosing-block)
    (narrow-to-region (point) (mark))
    (forward-line)
    (align-current)
    (widen)
    )
  )



(defun jpl-reformat-line-of-pos (pos)
  "Return the vertical position of pos"
  (+ (count-lines 1 pos)
     (if (= (current-column) 0) 1 0)
     )
  )



(provide 'jpl-reformat)



;;;; Test data
;;     my $mapper_obj = $mapper_class->new({


;;         "sd",                                      #sdsf
;;         "fdsfkj" =>  43, dfj => qq/sdf, fdsfasdkjfsdkj/,


;;         "abc",
;;         123, 456,

;;         53, schema => "htrht, rhty hjyt"  ,   #df, sdf
;;         import_row => $self->import_row    #dfsdkasd, dude
;;     });



