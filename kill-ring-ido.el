;;; kill-ring-ido.el --- command for kill-ring browsing with ido
;;
;; Copyright 2012 Horishnii Oleksii
;;
;; Author: Horishnii Oleksii <desu@horishniy.org.ua>
;; Version: 0.1
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Commentary:
;;
;; This package provides `kill-ring-ido', that allows you to browse
;; the kill-ring with ido. Obviously, need ido to work. With arg,
;; `kill-ring-ido' can browse throung secondary-selection. If previous
;; command was yank, it will act like `yank-pop'(default M-y).
;; All choise's length cut to `kill-ring-ido-shortage-length'. Number
;; between shows how many characters are cut.
;;
;; You *can* search text to yank typing some text, just like in
;; ido. It also searches in the cutted areas.
;;; WARNING: Because of that behaviour, it have defadvice around
;;; `ido-set-matches-1'.
;;
;; kill-ring-ido works natively with ido-preview, which allows to view
;; killed text as it is. It is obtainable here:
;; http://www.emacswiki.org/emacs/ido-preview.el
;;
;; Add this in your .emacs to bind it to M-y:
;; (global-set-key (kbd "M-y") 'kill-ring-ido)
;;
;; Add this in your .emacs if you wish to change maximum length:
;; (setq kill-ring-ido-shortage-length 6) ; where 6 is your value
;;
;; For haxorz: you can also use this function with your own ring:
;; (kill-ring-ido nil "Choose something: " (list "long long" "loooooooooooooooooooooooooong" "short"))
;;
;; Please, send bug reports to my email.
;;
;; References:
;; http://www.emacswiki.org/emacs/kill-ring-ido.el -- self-reference
;; http://www.emacswiki.org/emacs/InteractivelyDoThings -- ido main page
;; http://emacswiki.org/emacs/BrowseKillRing -- alternative solutions in kill ring browsing
;;
;;; Code:

(defvar kill-ring-ido-shortage-length 10)

(defface kill-ring-ido-number-face
  '((((class color) (background dark))
      :foreground "black"
      :background "gray80")
     (((class color) (background light))
       :foreground "gray80"
       :background "black"))
  "Font for shorten number between beginning and end of text")

(defadvice ido-set-matches-1(around this-is-needed-to-search-through-long-strings)
  "I need to search through long string(with is in cadr, not in car), but outside ido-set-matches, ido-name should be default."
  (flet ((ido-name (item) (if (consp item) (cadr item) item)))
    ad-do-it))

(defun kill-ring-ido(&optional arg prompt ring insert)
  "With RING is nil:
With ARG, use `secondary-selection-ring', else, use `kill-ring'.
Default prompt is set to \"kill-ring\" and \"secondary-kill-ring\" accordingly.
If RING is not nil:
Use RING instead. Default prompt is \"ring\"."
  (interactive "P")
  (if (and (not ring) (member last-command '(yank yank-secondary yank-pop yank-pop-commands)))
    (if (fboundp 'yank-pop-commands)
      (yank-pop-commands) ; second-sel.el
      (yank-pop))
    ;; else
    (if ring
      (unless prompt (setq prompt "ring: "))
      ;; else
      (setq ring (if arg secondary-selection-ring kill-ring))
      (unless prompt (setq prompt (if arg "secondary-selection-ring: " "kill-ring: "))))
    (let ((choices
            (mapcar
              (lambda(arg)
                (list
                  (if (> (length (replace-regexp-in-string "\n" "" (replace-regexp-in-string "^[   ]+" "" arg))) (+ (* 2 kill-ring-ido-shortage-length) 1))
                    (concat
                      (substring (replace-regexp-in-string "\n" "" (replace-regexp-in-string "^[   ]+" "" arg)) 0 kill-ring-ido-shortage-length)
                      (propertize (format "#%d#" (- (length arg) (* 2 kill-ring-ido-shortage-length))) 'face 'kill-ring-ido-number-face)
                      (substring (replace-regexp-in-string "\n" "" (replace-regexp-in-string "^[   ]+" "" arg)) (- kill-ring-ido-shortage-length)))
                    (replace-regexp-in-string "\n" "" (replace-regexp-in-string "^[   ]+" "" arg)))
                  arg))
              ring)))
      (ad-activate 'ido-set-matches-1)
      (prog1
        (let ((answer (progn
                        (ido-completing-read (format (concat "(%d)" prompt) (length ring)) choices)
                        (cadar ido-matches))))
          (insert answer))
        (ad-deactivate 'ido-set-matches-1))
      )))

(provide 'kill-ring-ido)
