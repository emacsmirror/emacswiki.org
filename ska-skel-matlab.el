;;; ska-skel-matlab.el --- my matlab skeletons
;; $Id: $
;; Copyright (C) 2003 by Stefan Kamphausen
;; Author: Stefan Kamphausen <kamphausen@insiligence.com>
;; Keywords: languages, tools
;; This file is not part of XEmacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.


;;; Commentary:
;; 

;;; Code:

(defun ska-skel-matlab-function (fname)
  "Magic matlab function inserting.
First the user is asked for a function name.
When the function-name doesn't match the current filename the user is
asked for the creation of a new file.  If a matlab entry exists in
`auto-insert-alist' this might then take effect. If no new file is
created we offers to erase the current buffer. 

The next step is the definition of output arguments.  If an argument
is empty the definition of output argument is considered done.  The
output list should be nicely formatted:
  - nothing between the 'function' keyword and the function-name when
    no output argument
  - the single name of a single argument
  - several output arguments put in to [...].

The definition of input arguments works the same.  If any are defined
they are put into parens.

Finally the documentation is prepared:
  - A Matlab H1-line
  - Usage-line copied from the function definitino
  - a list of input and output arguments
  - copyright and author notice using auto-insert-copyright"
  (interactive "sFunction name: ")
  ;; preparing buffer (new or erase or just insert at point)
  (if (or
       (not (buffer-file-name))
       (not (equal fname
                   (replace-in-string
                    (file-name-nondirectory (buffer-file-name))
                    ".m" ""))))
    (if (y-or-n-p "create new file? ")
        (find-file (concat fname ".m"))
      (when (not (equal (point-min) (point-max)))
        (if (y-or-n-p "erase current file contents? ")
            (erase-buffer)))))
  (insert "function [")
  ;; insert output arguments
  (let ((gotopos)
        (outarg)
        (hasoutargp nil)
        (outargs)
        (inarg)
        (hasinargp nil)
        (inargs))
    (while (> (length (setq outarg
                            (read-string
                             "output argument (ENTER to finish) ")))
              0)
      (insert (concat outarg ", "))
      (setq outargs (cons outarg outargs))
      (setq hasoutargp t))
    ;; cleanup output list
    (if (not hasoutargp)
        (backward-delete-char 1) ;; remove '['
      (backward-delete-char 2)   ;; remove ', '
      (insert "] = ")
      ;; remove [] around a single output argument
      (when (= (length outargs) 1)
        (save-excursion
          (beginning-of-line)
          (while (re-search-forward "[][]" (point-at-eol) t)
            (replace-match "")))))
    (insert fname " (")
    ;; insert input arguments
    (while (> (length (setq inarg
                            (read-string
                             "input argument (ENTER to finish) ")))
              0)
      (insert (concat inarg ", "))
      (setq inargs (cons inarg inargs))
      (setq hasinargp t))
    ;; clean up input list
    (if (not hasinargp)
        (backward-delete-char 1) ;; opening paren
      (backward-delete-char 2)   ;; ', '
      (insert ")"))
    ;; Matlab H1-line
    (insert (concat "\n% " (upcase fname) " "))
    (setq gotopos (point))
    (insert "<H1 line>\n")
    ;; Usage line (copied from above)
    (insert "%\n% Usage: ")
    (insert (save-excursion
              (search-backward-regexp "function ")              
              (search-forward-regexp "function ")
              (buffer-substring (point) (point-at-eol))))
    (insert "\n%\n")
    ;; argument documentation
    (when hasoutargp
        (insert "% Returns\n% -------\n")
      (mapc #'(lambda (oa)
                (insert (concat "% " oa ": \n")))
            (reverse outargs)))
    (when hasinargp
      (insert "%\n% Expects\n% -------\n")
      (mapc #'(lambda (ia)
                (insert (concat "% " ia ": \n")))
            (reverse inargs)))
    ;; copyright and authors notice
    (insert (concat
             "%\n%\n% Copyright (C) "
             (substring (current-time-string) -4) " by "
             auto-insert-copyright "\n"))
    (insert (concat "% Author: " (user-full-name) "\n%\n"))
    ;; move to H1-line to make user describe the function
    (goto-char gotopos)
    (mark-end-of-line nil)  
    ))
          
(provide 'ska-skel-matlab)

;;; ska-skel-matlab.el ends here
