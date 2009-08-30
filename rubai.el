;;; rubai.el -- random display of collection of strings (rhymes, rubai ...)
;;              with the help of popup menu.

;; Copyright (C) 2003 Eugene V. Markov

;; Author: Eugene V. Markov
;; Version: 0.1
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; It use popup menu. Create the file rubai-text.el (for example).
;; 
;; rubai-text.el:
;; (setq rubai-list
;;       '(
;;         (
;;          "Title text"
;;          )
;;         (              ;;---.
;;          "String_1 1"  ;;   |  
;;          "String_1 2"  ;;   | 1 rhyme  
;;          ...           ;;   |
;;          "String_1 N"  ;;   |
;;          )             ;;---'
;;         ...
;;         (              ;;---.
;;          "String_M 1"  ;;   |
;;          "String_M 2"  ;;   | M rhyme
;;          ...           ;;   |
;;          "String_M N"  ;;   |
;;          )             ;;---'
;;         ))

;; Just put this files (rubai-text.el and rubai.el) somewhere on your
;; Emacs load path and add following lines to end your .emacs file:

;; (load "rubai-text")
;; (load "rubai")
;; (add-hook 'window-setup-hook 'rubai-up)


(defun rubai-up ()
  "Random display collection of strings in popup menu and
clean RUBAI-LIST variable."
  (interactive)
  (rubai t))

(defun rubai (&optional clean)
  "Random display collection of strings in popup menu.
 CLEAN - if t, clean RUBAI-LIST variable."
  (if rubai-list
      (progn
        (while (rubai-message-box
                (car (car rubai-list))
                (nth (1+ (mod (nth 1 (current-time)) (1- (length rubai-list)))) rubai-list)))
        (if clean
            (setq rubai-list nil))
        )))


(defun rubai-message-box (title lr)
  "Display popup menu with strings collection.
 TITLE - title popup menu,
 LR - list of strings."
  (let (li)
    (while lr
      (setq li
            (nconc li (list (cons (car lr) nil))))
      (setq lr (cdr lr))
      )
    (setq li (nconc (list title (cons " " nil)) li))
    (setq li (nconc li (list (cons " " nil) (cons "Next..." t))))
    (with-timeout (2 nil) (x-popup-dialog t li))
    ))


;;!!! Generator of rubai list !!!
;; (defun rubai-list-generator()
;;   ""
;;   (interactive)
;;   (let ((b (get-buffer-create "rubai-buf"))
;;         (cb (current-buffer))
;;         str count)
;;     (set-buffer b)
;;     (erase-buffer)
;;     (goto-char (point-min))

;;     (insert "      \'(\n")

;;     (setq str t)
;;     (while str
;;       (insert "        (\n")
;;       (setq count 1)
;;       (while (> count 0)
;;         (save-excursion
;;            (set-buffer cb)
;;            (if (re-search-forward "^ *\\(.+\\)$" nil 1)
;;                (setq str (match-string-no-properties 1))
;;              (setq str nil)))
;;         (if str
;;             (progn
;;               (insert (format "         \"%s\"\n" str))
;;               (setq count (1- count))
;;               )
;;           (setq count 0))
;;         )
;;       (insert "         )\n")
;;       )
;;     (insert "        )\n")
;;     ))

(provide 'rubai)

;;; rubai.el ends here.
