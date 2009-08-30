;;; eev-thierry.el --- 
;; 
;; Filename: eev-thierry.el
;; Description: 
;; Author: 
;; Maintainer: 
;; Created: sam déc 20 18:54:44 2008 (+0100)
;; Version: 
;; Last-Updated: lun mar  2 15:31:05 2009 (+0100)
;;           By: thierry
;;     Update #: 57
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (tv-ee-index-create)
;;;;«INDEX»
;;; «.Multiply-a-string-by-a-number» (to "Multiply-a-string-by-a-number")
;;; «.Tag-a-section»                 (to "Tag-a-section")
;;; «.Index-eev»                     (to "Index-eev")
;;; «.Copy-links»                    (to "Copy-links")
;;; «.Yank-links»                    (to "Yank-links")
;;; «.Global-keys»                   (to "Global-keys")

;;;==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'cl)
(require 'eev-all)
(require 'iterator)

;; «Multiply-a-string-by-a-number» (to ".Multiply-a-string-by-a-number")
(defmacro *string (str num)
  "* a string with a number.
Do the same than `make-string'"
  `(let ((str-lis))
    (dotimes (n ,num)
      (push ,str str-lis))
    (mapconcat #'(lambda (i) i) str-lis "")))

;; «Tag-a-section» (to ".Tag-a-section")
;;;###autoload
(defun tv-eev-tag-entry ()
  "Tag entry in file with eev glyph"
  (interactive)
  (if (and ee-anchor-format
           eev-comment-prefix)
      (let ((entry (replace-regexp-in-string (format "^[%s ]+"
                                                     eev-comment-prefix)
                                             ""
                                             (thing-at-point 'line)))
            (new-entry))
        (setq entry (replace-regexp-in-string "\n" "" entry))
        (setq new-entry (replace-regexp-in-string " " "-" entry))
        (end-of-line)
        (re-search-backward entry)
        (delete-region (point) (line-end-position))
        (insert (concat (format ee-anchor-format new-entry)
                        " (to \"." new-entry "\")")))
      (message "Did you forget to set up ee-anchor-format and eev-comment-prefix?")))

(global-set-key (kbd "C-M-,") 'tv-eev-tag-entry)


;; «Index-eev» (to ".Index-eev")
;;;###autoload
(defun tv-ee-index-create ()
  "Create or update an index of all tagged entries found in file.
Put the definition of function in comment at start of index
and eval it with \"M-e\""
  (flet ((create ()
           (let ((entry-list nil)
                 (new-entry nil)
                 (start-pos)
                 (end-pos))
             (forward-line)
             (insert (concat (*string eev-comment-prefix 4)
                             (format ee-anchor-format "INDEX") "\n"))
             (setq start-pos (point))
             (while (not (eobp))
               (forward-line 1)
               (when (re-search-forward (concat (format "^%s*"
                                                        eev-comment-prefix)
                                                " *"
                                                (format ee-anchor-format ".+")) nil t)
                 (setq new-entry
                       (replace-regexp-in-string (format "%s* *" eev-comment-prefix)
                                                           ""
                                                           (match-string 0)))
                 (push new-entry entry-list)))
             (setq entry-list (reverse entry-list))
             ;; we have now a list with elmts like «elmt»
             (goto-char start-pos)
             (dolist (i entry-list)
              (let ((new-entry (substring i 1 (1- (length i)))))
                (insert (concat (*string eev-comment-prefix 3)
                                " "
                                (format ee-anchor-format
                                        (concat "." new-entry))
                                (concat " (to \""
                                        new-entry
                                        "\")"
                                        "\n")))))
             (setq end-pos (point))
             (and (fboundp 'align-regexp)
                  (align-regexp start-pos end-pos "\\(\\s-*\\)(" 1 1 nil))
             (insert "\n")
             (insert (concat (*string eev-comment-prefix 3)
                             "==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)\n"))))
         (update ()
           (forward-line)
           (let ((beg (point))
                 (end))
             (when (re-search-forward (format "^%s\\{3\\}==UPDATE-EEV-BOUNDARY==" eev-comment-prefix) nil t)
               (end-of-line)
               (setq end (point)))
             (delete-region beg end)
             (create))))
    (let ((action))
      (save-excursion
        (if (re-search-forward (format "^%s\\{3\\}==UPDATE-EEV-BOUNDARY==" eev-comment-prefix) nil t)
            (setq action 'update)
            (setq action 'create)))
        (when (and ee-anchor-format
                   eev-comment-prefix)
          (funcall action)))))

;; «Copy-links» (to ".Copy-links")

(defvar tv-eev-ring nil)
(defun copy-eev-link-from-cur-sexp ()
  (interactive)
  (let* ((cur-line (thing-at-point 'sexp))
         (link (concat
                "(find-fline \""
                (abbreviate-file-name
                 (buffer-file-name (current-buffer)))
                "\" \""
                cur-line
                "\")")))
    (push link tv-eev-ring)
    (message "%s copied to eev-ring" cur-line)))

(defun copy-eev-link-from-region (beg end)
  (interactive "r")
  (let* ((cur-line (buffer-substring beg end))
         (link (concat
                "(find-fline \""
                (abbreviate-file-name
                 (buffer-file-name (current-buffer)))
                "\" \""
                cur-line
                "\")")))
    (push link tv-eev-ring)
    (message "%s copied to eev-ring" cur-line)))

;; «Yank-links» (to ".Yank-links")
(defun yank-eev-link ()
  (interactive)
  (let* ((iter-eev-ring (iter-list tv-eev-ring))
         (last-elm)
         (eev-flag-move nil)
         (action)
         (first-elm (iter-next iter-eev-ring)))
    (if first-elm
        (progn
          (insert first-elm)
          (setq last-elm first-elm))
        (message "eev-kill-ring Empty"))
    (catch 'break
      (while 1
        (setq action (read-event "(n)ext (b)ack (q)uit"))
        (case action
          ('?n
           (if eev-flag-move
               (progn
                 (setq iter-eev-ring (sub-iter-next tv-eev-ring last-elm))
                 (setq eev-flag-move t)
                 (let ((elm-to-yank (iter-next iter-eev-ring)))
                   (if elm-to-yank
                       (progn
                         (beginning-of-line)
                         (delete-region (point) (+ (point) (length last-elm)))
                         (insert elm-to-yank)
                         (setq last-elm elm-to-yank))
                       (message "eev-kill-ring Empty"))))

               (let ((elm-to-yank (iter-next iter-eev-ring)))
                 (if elm-to-yank
                     (progn
                       (beginning-of-line)
                       (delete-region (point) (+ (point) (length last-elm)))
                       (insert elm-to-yank)
                       (setq last-elm elm-to-yank))
                     (message "eev-kill-ring Empty")))))
          ('?b
           (setq iter-eev-ring (sub-iter-prec tv-eev-ring last-elm))
           (setq eev-flag-move t)
           (let ((elm-to-yank (iter-next iter-eev-ring)))
             (if elm-to-yank
                 (progn
                   (beginning-of-line)
                   (delete-region (point) (+ (point) (length last-elm)))
                   (insert elm-to-yank)
                   (setq last-elm elm-to-yank))
                 (message "eev-kill-ring Empty"))))
          ('?q
           (throw 'break nil)))))))


;; «Global-keys» (to ".Global-keys")
(global-set-key (kbd "C-c M-c") 'copy-eev-link-from-cur-sexp)
(global-set-key (kbd "C-c M-r") 'copy-eev-link-from-region)
(global-set-key (kbd "C-c M-y") 'yank-eev-link)

(provide 'eev-thierry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eev-thierry.el ends here
;; Local Variables:
;; eev-comment-prefix: ";"
;; ee-anchor-format: "«%s»"
;; End:
