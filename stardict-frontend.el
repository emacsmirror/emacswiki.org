;;; stardict-frontend.el --- simple startdict.el minibuffer frontend

;; Copyright (C) 2026 whisky

;; Author: Whisky Basing <basingwhisk@elektrine.com>
;; Created: 2026-09-05
;; Version: 0.1
;; Keywords: stardict

;; This file is *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to andyetitmoves@gmail.com)
;; or from the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;; Example:
;; 
;; (require 'stardict)
;; (require 'stardict-frontend)
;; (setq dict
;;       (stardict-open "~/.stardict/dic/stardict-oxford-gb-2.4.2"
;;                      "oxford-gb"))
;; 
;; (global-set-key (kbd "C-c c") #'stardict-lookup-at-point)

;; M-x stardict-query RET anywords

;;; Code:

(defun stardict-lookup-base (word)
  "Strip common inflection endings of word."
        ;;Identify the common inflected forms of English words, excluding irregular forms. Dictionaries usually include irregular forms.
  (let* ((y-stem (when (string-match "\\(?:ies\\|ied\\|ier\\|iest\\)\\'" word)
                   (substring word 0 -3)))
         (strips (delq nil
                       (mapcar (lambda (suf)
                                 (and (string-match (concat suf "\\'") word)
                                      (substring word 0 (- (length word) (length suf)))))
                               '("s" "es" "ed" "ing" "er" "est"))))
         (undoubled (delq nil
                          (mapcar (lambda (s)
                                    (and (> (length s) 2)
                                         (eql (aref s (- (length s) 1))
                                              (aref s (- (length s) 2)))
                                         (substring s 0 -1)))
                                   strips)))
         (with-e (delq nil
                       (mapcar (lambda (s)
                                 (when (and (> (length s) 2)
                                            (string-match "[^aeiou]\\'" s))
                                   (concat s "e")))
                               strips)))
         (cands (append (when y-stem (list (concat y-stem "y")))
                        with-e
                        strips
                        undoubled)))
    (catch 'hit
      (dolist (try cands)
        (when (and (> (length try) 1)
                   (stardict-word-exist-p dict try))
          (throw 'hit (stardict-lookup dict try)))))))


(defun stardict-query ()
  "Query a word interactively and display its definition in minibuffer."
  (interactive)
  (let ((word (read-string "Word to translate: ")))
    (cond
     ((string-empty-p word)
      (message "No word entered"))
     ((stardict-word-exist-p dict (downcase word))
      (let ((definition (stardict-lookup dict (downcase word))))
        (if definition
            (message "%s: %s" word definition)
          (message "Definition not found for: %s" word))))
     (t
      (let ((definition (stardict-lookup-base (downcase word))))
        (if definition
            (message "%s: %s" word definition)
          (message "Word '%s' not found in dictionary" word)))))))

(defun stardict-lookup-at-point ()
  "Lookup the word at cursor position and display in minibuffer."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (cond
     ((not word)
      (message "No word at cursor position"))
     ((stardict-word-exist-p dict (downcase word))
      (let ((definition (stardict-lookup dict (downcase word))))
        (if definition
            (message "%s: %s" word definition)
          (message "Definition not found for: %s" word))))
     (t
      (let ((definition (stardict-lookup-base (downcase word))))
        (if definition
            (message "%s: %s" word definition)
      (message "Word '%s' not found in dictionary" word)))))))


(provide 'stardict-frontend)

;;; stardict-frontend.el ends here
