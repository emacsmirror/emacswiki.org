;;;

;; VIEW-KILL
;; Displays the entire kill ring as a single page menu of numbered items.
;; M-x view-kill
;; Copyright (C) 2010 jason bonthron

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; VIEW-KILL
;; Displays the entire kill ring as a single page menu of numbered items.
;; To run use:
;; M-x view-kill

;; Select a number to insert that item into your current buffer.
;; Use <SPACE> to scroll.
;; The length of your kill ring is controlled by the variable `kill-ring-max';
;; Any input other than a recongized item number will simply exit the menu

;; To install: save this file in your elisp directory
;; Add to your .emacs file: 
;;  (load "view-kill")
;; OR, it's short enough, just copy the whole function into your .emacs file


(defun view-kill ()
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((working-buffer (current-buffer))
            (new-buffer (get-buffer-create "kill-ring-view"))
            (count 0)
            (custom-map (copy-keymap minibuffer-local-map))
            (selection nil)
            )      
        (unwind-protect
            (progn
              (define-key custom-map " " 'scroll-other-window)
              
              (switch-to-buffer new-buffer t)
              (delete-other-windows)
              
              (dolist (x kill-ring)
                (insert (concat "----- " 
                                (number-to-string count) 
                                " -----"))
                (newline)
                (insert x)
                (newline)
                (newline)
                (setq count (+ count 1))
                )
              (goto-char (point-min))
              
              (let ((choice (read-from-minibuffer "choose: " nil custom-map t nil)))
                (and (numberp choice)
                     (< choice count)
                     (progn
                       (set-buffer working-buffer)
                       (insert (nth choice kill-ring))
                       (setq selection choice)
                       ))
                ))
          (kill-buffer new-buffer) ; unwind-protect clean-up form
          )
        selection
        ))))
