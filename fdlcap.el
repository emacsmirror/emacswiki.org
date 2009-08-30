;;; fdlcap.el --- Cycle through case and capitalization of words.

;; Copyright (C) 2007  Aaron S. Hawley

;; Author: Aaron S. Hawley
;; Keywords: convenience, wp
;; Version: %Id: 1%
;; RCS Version: $Id: fdlcap.el,v 1.1 2007/12/17 03:17:47 ashawley Rel $
;; URL: http://www.emacswiki.org/elisp/fdlcap.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To change the case of the current word with `Shift-F3', put
;; the following in your .emacs file:

;; (global-set-key (quote [S-f3]) 'fdlcap-change-case-current-word)

;; The order of change is lowercase, to capitalized, to
;; uppercase and then back to lowercase.  To do reverse the
;; order of operations, type `C-u' before hitting `Shift-F3'.

;; To change the case of the following word(s) forward with
;; Ctrl-Shift-F3, add the following in your .emacs file:

;; (global-set-key (quote [S-C-f3]) 'fdlcap-change-case-word)

;; Successively hitting `Ctrl-Shift-F3' will change the case of
;; the same number of words from the previous execution.

;; There currently is no way to reverse the order of this latter
;; command.  The ability to easily repeat the command should
;; take care of that.  The command is just an aggregate of the
;; Emacs commands `M-l', `M-c' and `M-u' -- the functions
;; `downcase-word', `capitalize-word' and `upcase-word'
;; respectively.

;;; History

;; Written on 16 December, 2007 in South Burlington, Vermont, USA.

;; Proposal and code posted by Maverick Woo to the Emacs Wiki on 7
;; November, 2007.

;;; Code:

(defun fdlcap-change-case-current-word (&optional arg)
  "Fiddle the capitalization of the current word.

If \\[universal-argument], then change the case in the reverse
order specified by `fdlcap-case-fiddling-order'."
  (interactive "*P")
  (let ((word (current-word t)))
    (if (null word)
        (error "No word at point")
      (let ((word-length (length word))
            (opoint (point)))
        (save-excursion
          ;; Avoid signaling error when moving beyond buffer.
          (if (>= 0 (- (point) word-length))
              (beginning-of-buffer)
            (forward-char (- (length word))))
          (if (search-forward word (+ opoint (length word))
                              'noerror)
              (fdlcap-change-case-region (match-beginning 0)
                                         (match-end 0))
            (error (format "Unable to locate word: %s"
                           word))))))))

(defun fdlcap-change-case-region (beg end &optional arg)
  "Fiddle the capitalization in region of BEG and END.

If \\[universal-argument], then change the case in the reverse
order specified by `fdlcap-case-fiddling-order'."
  (interactive "*r\nP")
  (let* ((case-fold-search nil)
         (current-case
          (if (save-excursion
                (goto-char beg)
                (not (re-search-forward "[[:upper:]]" end
                                        'noerror)))
              'downcase
            (if (save-excursion
                  (goto-char beg)
                  (not (re-search-forward "[[:lower:]]" end
                                          'noerror)))
                'upcase
              'capitalize)))
         (op-order (if arg
                       (reverse fdlcap-case-fiddling-order)
                     fdlcap-case-fiddling-order))
         (next-case (cadr (memq current-case op-order)))
         (next-op (intern (concat (symbol-name next-case)
                                  "-region"))))
    (funcall next-op beg end)))

(defconst fdlcap-case-fiddling-order
  '(downcase capitalize upcase downcase)
  "Circular list to determine order of fiddling capitalization.

downcase -> capitalize -> upcase -> downcase -> ...

The list can retrieve the next element with:

  (cadr (memq 'downcase fdlcap-case-fiddling-order))
  ==> 'capitalize

  (cadr (memq 'upcase fdlcap-case-fiddling-order))
  ==> 'downcase

  (cadr (memq 'unknown-case fdlcap-case-fiddling-order))
  ==> nil

Reverse the list to get the *previous*:

  (cadr (memq 'downcase (reverse fdlcap-case-fiddling-order)))
  ==> 'upcase

See `fdlcap-change-case-region'.")

(defun fdlcap-change-case-word (&optional arg)
  "Fiddle the capitalization of the next ARG words.

The order of changing case is specified by
`fdlcap-case-fiddling-order'.

If command is executed repeatedly, then previous value of ARG is
used."
  (interactive "*P")
  (let ((arg (if (and (null arg)
                      (eq last-command 'fdlcap-change-case-word)
                      (numberp fdlcap-change-case-word-last-arg))
                 fdlcap-change-case-word-last-arg
               (if arg arg 1))))
    (setq fdlcap-change-case-word-last-arg arg)
    (fdlcap-change-case-region
     (point) (progn (mark-word arg) (region-end)))))

(defvar fdlcap-change-case-word-last-arg nil
  "Value of ARG for last call of `fdlcap-change-case-word'.")

(provide 'fdlcap)
;;; fdlcap.el ends here
