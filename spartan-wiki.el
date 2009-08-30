;;; spartan-wiki.el --- a really spartan personal wiki

;; Copyright (C) 2008  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A single-file personal wiki for quick structured note taking.
;;
;; There is only one markup character: `<word> becomes a link
;; to a page automatically.
;;
;; If you want spaces in the link the close it with an other `
;;
;;       `word1 word2 word3`
;;
;; Links are case-insensitive. The link `main page` is reserved for
;; the main page.
;;
;; To search the whole wiki you can use isearch as usual.
;; 
;;
;; Put this in your .emacs file to turn on the wiki mode automaticaly
;; for .sw (spartan wiki) files:
;;
;;     (require 'spartan-wiki)
;;     (add-to-list 'auto-mode-alist '("\\.sw$" . spartan-mode))
;;


;; Tested on Emacs 22 

;;; Code:

(defvar spartan-history-length 20
  "How many items are kept in the history.")

(defvar spartan-history-display 5
  "How many items are shown on the button bar.")

(defvar spartan-page-name-face 'header-line)

(defvar spartan-link-face 'highlight)

(defvar spartan-button-face 'lazy-highlight)

(defvar spartan-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'spartan-follow-link)
    (define-key map [mouse-2] 'spartan-follow-link)
    map)
  "Keymap used by buttons.")

(defvar spartan-link-markup-char "`")

(defvar spartan-main-page-name "main page")

;;----------------------------------------------------------------------

(require 'cl)


(defvar spartan-page-separator "\f")

(defvar spartan-page-regexp (concat "^" spartan-page-separator))

(defvar spartan-link-props `((rear-nonsticky t)
                             (keymap ,spartan-map)))

(defvar spartan-link-attrs `(face ,spartan-link-face
                                  ,@(mapcan 'identity
                                            spartan-link-props)))

(define-derived-mode spartan-mode fundamental-mode "Spartan"
  (make-local-variable 'font-lock-extra-managed-props)
  (setq font-lock-extra-managed-props
        (append font-lock-extra-managed-props (mapcar 'car spartan-link-props)))

  (setq font-lock-defaults `(((,(concat spartan-link-markup-char
                                        "\\(\\sw\\([^"
                                        spartan-link-markup-char
                                        "\n]*\\sw\\)?\\)"
                                        spartan-link-markup-char)
                               . (1 spartan-link-attrs))
                              (,(concat spartan-link-markup-char
                                        "\\(\\sw+\\)")
                               . (1 spartan-link-attrs))
                              (,(format "\\(%s\\)\\(.*\n\\)" spartan-page-regexp)
                               ;; the face for the 1st subexp is a
                               ;; placeholder dummy, so that the text
                               ;; properties can be added
                               ;;
                               ;; note that the facespec list is
                               ;; double quoted (quoted in an already
                               ;; quoted list), because it needs to be
                               ;; an evalable expression
                               . ((1 '(face highlight invisible t))
                                  (2 spartan-page-name-face))))))

  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'spartan-remove-buttons)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'spartan-add-buttons)

  (spartan-show-current))


(defvar spartan-buttons nil)

(make-variable-buffer-local 'spartan-history)
(defvar spartan-history nil)

(defvar spartan-isearch-start-pos nil)


(defun spartan-follow-link ()
  (interactive)

  (let ((page (buffer-substring-no-properties
               (previous-single-property-change (1+ (point)) 'keymap)
               (or (next-single-property-change (point) 'keymap
                                                nil (line-end-position))
                   (save-excursion
                     (end-of-line)
                     (assert (eobp))
                     (point))))))
    (spartan-go-to-link page)))


(defun spartan-go-to-link (page &optional norecord)
  (unless norecord
    (spartan-store-current-page-in-history))

  (spartan-remove-buttons)

  (widen)
  (goto-char (point-min))

  (unless (spartan-main-page-p page)
    (if (let ((case-fold-search t))
          (re-search-forward (concat spartan-page-regexp page "$") nil t))
        (forward-line)

      (goto-char (point-max))
      (insert "\n" spartan-page-separator page "\n")
      (message "New page.")))

  (spartan-show-current))


(defun spartan-main-page-p (page)
  (equal (downcase page) spartan-main-page-name))


(defun spartan-store-current-page-in-history ()
  (push (save-excursion
          (if (re-search-backward (concat spartan-page-regexp
                                          "\\s-*\\(.*?\\)\\s-*$") nil t)
              (match-string-no-properties 1)
            spartan-main-page-name))
        spartan-history)
  (if (> (length spartan-history) spartan-history-length)
      (setq spartan-history (nbutlast spartan-history 1))))


(defun spartan-add-buttons ()
  (save-restriction
    (widen)
    (save-excursion
      (let ((start (re-search-backward spartan-page-regexp nil t)))
        (let ((modified (buffer-modified-p)))
          (if (not start)
              ;; main page has no header
              (goto-char (point-min))
            (goto-char start)
            (forward-line))
          ;; can't use insert-text-button here, because font-lock
          ;; does not allow to change the face of text properties

          (when start
            (push (insert-button "[main]"
                                 'action 'spartan-go-to-main
                                 'face spartan-button-face)
                  spartan-buttons)
            (insert " "))

          (push (insert-button "[goto]"
                               'action 'spartan-go-to-page
                               'face spartan-button-face)
                spartan-buttons)

          (when (> (length spartan-history) 0)
            (insert " back:")

            (dotimes (i (min (length spartan-history)
                             spartan-history-display))
              (insert " ")
              (push (insert-button (concat "[" (nth i spartan-history)"]")
                                   'action (lexical-let ((i i))
                                             (lambda (button)
                                               (spartan-go-back i)))
                                   'face spartan-button-face)
                  spartan-buttons)))

          (insert "\n")
          (set-buffer-modified-p modified))))))


(defun spartan-remove-buttons ()
  (save-restriction
    (widen)
    (save-excursion
      (let ((start (re-search-backward spartan-page-regexp nil t)))
        (dolist (button spartan-buttons)
          (delete-overlay button))
        (setq spartan-buttons nil)

        (let ((modified (buffer-modified-p)))
          (if (not start)
              ;; main page has no header
              (goto-char (point-min))
            (goto-char start)
            (forward-line))
          (let ((start (point)))
            (forward-line)
            (delete-region start (point)))
          (set-buffer-modified-p modified))))))


(defun spartan-go-to-main (button)
  (spartan-go-to-link spartan-main-page-name))


(defun spartan-go-to-page (button)
  (let ((pages (list spartan-main-page-name)))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (concat spartan-page-regexp
                                          "\\(.*\\)" "$")
                                  nil t)
          (push (match-string-no-properties 1) pages))))
    (spartan-go-to-link (completing-read "go to page: " pages nil t))))


(defun spartan-go-back (which)
  (let ((page (nth which spartan-history)))
    (setq spartan-history (nthcdr (1+ which) spartan-history))
    (spartan-go-to-link page t)))

  
(defun spartan-show-current ()
  (spartan-add-buttons)
  ;; put cursor on first line
  (forward-line)
  (let ((bounds (spartan-get-page-bounds)))
    (narrow-to-region (or (car bounds) (point-min))
                      (or (cdr bounds) (point-max)))))


(defun spartan-get-page-bounds ()
  (cons (or (save-excursion
              (re-search-backward spartan-page-regexp nil t))
            (point-min))
        (let ((end (save-excursion
                     (re-search-forward spartan-page-regexp nil t))))
          (if end
              ;; treat the final newline as part of the header of the
              ;; next page
              (1- (match-beginning 0))
            (point-max)))))


(defadvice isearch-forward (before
                            spartan-isearch-forward
                            activate)
  (spartan-isearch-start))

(defadvice isearch-backward (before
                             spartan-isearch-backward
                             activate)
  (spartan-isearch-start))

(defadvice isearch-forward-regexp (before 
                                   spartan-isearch-forward-regexp
                                   activate)
  (spartan-isearch-start))

(defadvice isearch-backward-regexp (before
                                    spartan-isearch-backward-regexp
                                    activate)
  (spartan-isearch-start))


(defun spartan-isearch-start ()
    (when (eq major-mode 'spartan-mode)
      (spartan-remove-buttons)    
      (widen)
      (setq spartan-isearch-start-pos (point))))


(defun spartan-isearch-end ()
  (when (eq major-mode 'spartan-mode)
    (unless isearch-mode-end-hook-quit
      (save-excursion
        (goto-char spartan-isearch-start-pos)
        (spartan-store-current-page-in-history)))
    (spartan-show-current)))

(add-hook 'isearch-mode-end-hook 'spartan-isearch-end)


(provide 'spartan-wiki)
;;; spartan-wiki.el ends here
