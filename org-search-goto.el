;;; org-search-goto.el --- Use search to go to locations in your org buffers

;; Copyright (C) 2011  

;; Author:  
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; 
;;; 
;;; Usage: M-x osg, then start typing
;;; 
;;; Select from the matches with up/down/pgup/pgdown and press enter
;;; to go that location
;;; (you can navigate the history with M-p/M-n) 
;;; 
;;; If the search string contains several strings separated by spaces then
;;; these substrings can appear in any order in the results.
;;;

;; 

;;; Code:


(require 'cl)


(defvar osg-idle-delay 0.2)

(defvar osg-max-results 100)

(defvar osg-match-face 'match)

(defvar osg-header-face 'underline)

(defvar osg-warning-face 'font-lock-warning-face)

(defvar osg-map 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'osg-next-line)
    (define-key map (kbd "<up>") 'osg-previous-line)
    (define-key map (kbd "<prior>") 'osg-previous-page)
    (define-key map (kbd "<next>") 'osg-next-page)
   map))





(defvar osg-buffer-name "*org search goto*")

(defvar osg-history-list nil)

(defvar osg-org-buffers nil)

(defvar osg-history-list nil)

(defvar osg-line-info nil)

(defvar osg-orig-window nil)

(defvar osg-orig-buffer nil)




(defun osg-previous-line ()
  (interactive)
  (osg-move-selection 'forward-line -1))
 
 
(defun osg-next-line ()
  (interactive)
  (osg-move-selection 'forward-line 1))
 
 
(defun osg-previous-page ()
  (interactive)
  (osg-move-selection 'scroll-down))
 
 
(defun osg-next-page ()
  (interactive)
  (osg-move-selection 'scroll-up))
 
 
(defun osg-move-selection (movefunc &optional movearg)
  (let ((win (get-buffer-window osg-buffer-name)))
    (if win
        (with-selected-window win
          (condition-case nil
              (funcall movefunc movearg)
            (beginning-of-buffer (goto-char (point-min)))
            (end-of-buffer (goto-char (point-max))))

          (setq osg-line-info (get-text-property (line-beginning-position) 'osg-line-info))))))


;; adapted from EmacsWiki: http://www.emacswiki.org/emacs/StringPermutations
(defun osg-list-permutations (l)
  (if (null l)
      (list '())
    (mapcan #'(lambda( a )
                (mapcan #'(lambda( p )
                            (list (cons a p)))
                        (osg-list-permutations (remove* a l :count 1))))
            l)))



(defun osg-check-input ()
  (when (sit-for osg-idle-delay)
    (let ((input (split-string (minibuffer-contents) " " t)))
      (unless (equal input osg-current-input)
        (setq osg-current-input input)

        (with-selected-window (get-buffer-window osg-buffer-name)
          (erase-buffer))

        (when input
          (let ((number-of-words (length input))
                (result-count 0)
                (buffers osg-org-buffers))
            (setq input (mapconcat (lambda (l)
                                     (mapconcat 'identity
                                                (mapcar (lambda (i)
                                                          (concat "\\(" (regexp-quote i) "\\)"))
                                                        l)
                                                ".*?"))
                                   (osg-list-permutations input)
                                   "\\|"))

            (while (and buffers
                        (< result-count osg-max-results))
              (let ((buffer (pop buffers)))
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-min))
                    (let ((header-not-printed (buffer-name)))
                      (while (and (< result-count osg-max-results)
                                  (re-search-forward input nil t))
                        (let ((match (buffer-substring (line-beginning-position)
                                                       (line-end-position)))
                              (paren-index 1))

                          (while (not (match-string paren-index))
                            (setq paren-index (+ paren-index number-of-words)))

                          (dotimes (i number-of-words)
                            (put-text-property (- (match-beginning paren-index) (line-beginning-position))
                                               (- (match-end paren-index) (line-beginning-position))
                                               'face osg-match-face
                                               match)
                            (incf paren-index))

                          (let ((line-num (count-lines (point) (point-min))))
                            (with-current-buffer osg-buffer-name
                              (when header-not-printed                            
                                (insert (propertize header-not-printed 'face osg-header-face) "\n")
                                (setq header-not-printed nil))

                              (insert (format "%7d:" line-num) match)
                              (put-text-property (line-beginning-position) (1+ (line-beginning-position))
                                                 'osg-line-info (list 'buffer buffer 'line line-num))
                              (insert "\n")))

                          (forward-line 1)
                          (incf result-count))))))))

            (with-selected-window (get-buffer-window osg-buffer-name)
              (goto-char (point-min))
              (osg-next-line))

            (if (and (= result-count osg-max-results)
                     (sit-for 0.2))
                (message (propertize "Too many matches, keep typing to narrow it down more"
                                     'face osg-warning-face)))))))))



(defun osg ()
  (interactive)

  (setq osg-org-buffers
        (delete-if 'null
                   (mapcar (lambda (b)
                             (with-current-buffer b
                               (if (eq major-mode 'org-mode)
                                   b)))
                           (buffer-list))))

  (let ((cursor-in-non-selected-windows 'box))
    (save-window-excursion
      (add-hook 'post-command-hook 'osg-check-input)

      (setq osg-current-input nil)
      (setq osg-line-info nil)
      (setq osg-orig-window (selected-window))
      (setq osg-orig-buffer (current-buffer))

      (if (equal (buffer-name) osg-buffer-name)
          (kill-buffer))
      (save-selected-window
        (pop-to-buffer osg-buffer-name)
        (erase-buffer))      

      (unwind-protect
          (let ((minibuffer-local-map osg-map))
            (read-string "Search for: " nil 'osg-history-list))
 
        (remove-hook 'post-command-hook 'osg-check-input))))

  (if (not osg-line-info)
      (message "No match on this line.")

    (switch-to-buffer (plist-get osg-line-info 'buffer))
    (goto-line (plist-get osg-line-info 'line))
    (when (outline-invisible-p)
      (save-excursion
        (outline-previous-visible-heading 1)
        (org-show-subtree)))))





(provide 'org-search-goto)
;;; org-search-goto.el ends here
