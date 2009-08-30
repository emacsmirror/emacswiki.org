;;; perltidy.el --- Tidy perl code

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 22 Dec 2007
;; Version: 0.01
;; Keywords: tools, convenience, languages

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; As the PBP(Perl Best Practice) suggest, put this to your ~/.perltidyrc:
;; ## .perltidyrc --- configuration for perltidy
;; # Max line width is 78 cols
;; -l=78   
;; # Indent level is 4 cols
;; -i=4    
;; # Continuation indent is 4 cols
;; -ci=4   
;; # Output to STDOUT
;; -st     
;; # Errors to STDERR
;; -se     
;; # Maximal vertical tightness
;; -vt=2   
;; # No extra indentation for closing brackets
;; -cti=0  
;; # Medium parenthesis tightness
;; -pt=1   
;; # Medium brace tightness
;; -bt=1   
;; # Medium square bracket tightness
;; -sbt=1  
;; # Medium block brace tightness
;; -bbt=1  
;; # No space before semicolons
;; -nsfs   
;; # Don't outdent long quoted strings
;; -nolq   
;; # Break before all operators
;; -wbb="% + - * / x != == >= <= =~ !~ < > | & >= < = **= += *= &= <<= &&= -= /= |= >>= ||= .= %= ^= x="

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perltidy)

;;; Code:

(eval-when-compile
  (require 'cl))
(defvar perltidy-program "perltidy"
  "*Program name of perltidy")
(defvar perltidy-buffer-name "*perltidy*"
  "*Name of the temporary perltidy buffer.")
(defvar perltidy-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'perltidy-write)
    map)
  "*keymap for *perltidy* buffer.")

(defvar perltidy-last-buffer nil
  "Internal variable.")

(defmacro perltidy-save-point (&rest body)
  (declare (indent 0) (debug t))
  `(let ((old-point (point)))
     ,@body
     (goto-char old-point)))
    
(defun perltidy-region (beg end)
  "Tidy perl code in the region."
  (interactive "r")
  (perltidy-save-point
    (call-process-region beg end perltidy-program t t)))

(defun perltidy-buffer ()
  "Call perltidy for whole buffer."
  (interactive)
  (perltidy-region (point-min) (point-max)))

(defun perltidy-subroutine ()
  "Call perltidy for subroutine at point."
  (interactive)
  (perltidy-region (progn (beginning-of-defun) (point))
                   (progn (end-of-defun) (point))))

;;;###autoload 
(defun perltidy-dwim (arg)
  "Perltidy Do What I Mean.
If with prefix argument, just show the result of perltidy.
You can use C-x C-s to save the tidy result.
If region is active call perltidy on the region. If inside
subroutine, call perltidy on the subroutine, otherwise call
perltidy for whole buffer."
  (interactive "P")
  (let ((buf (current-buffer))
        beg end)
    (cond ((and mark-active transient-mark-mode)
           (setq beg (region-beginning)
                 end (region-end)))
          ((save-excursion
             (beginning-of-defun)
             (when (looking-at "\\s-*sub\\s-+")
               (setq beg (point)
                     end (progn (end-of-defun) (point))))))
          (t (setq beg (point-min)
                   end (point-max))))
    (when arg
      (set-buffer (get-buffer-create perltidy-buffer-name))
      (erase-buffer)
      (insert (with-current-buffer buf
                (buffer-substring beg end)))
      (setq perltidy-last-buffer (list buf beg end))
      (setq beg (point-min)
            end (point-max))
      (perl-mode)
      (use-local-map perltidy-map)
      (pop-to-buffer (current-buffer))
      (message "Press C-x C-s to apply the tidy result."))
    (perltidy-region beg end)))

(defun perltidy-write ()
  (interactive)
  (if perltidy-last-buffer
      (let ((buf (get-buffer perltidy-buffer-name)))
        (if (buffer-live-p buf)
            (if (buffer-live-p (car perltidy-last-buffer))
                (with-current-buffer (car perltidy-last-buffer)
                  (goto-char (cadr perltidy-last-buffer))
                  (perltidy-save-point
                    (delete-region (cadr perltidy-last-buffer)
                                   (nth 2 perltidy-last-buffer)))
                  (insert-buffer-substring buf)
                  ;; don't write again
                  (setq perltidy-last-buffer nil))
              (error "The perltidy associated buffer is not exists!"))
          (error "No perltidy buffer exists!")))
    (message "Do you forget perform perltidy or already write to the associated buffer")))

(provide 'perltidy)
;;; perltidy.el ends here
