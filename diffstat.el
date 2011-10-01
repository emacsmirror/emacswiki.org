;;; diffstat.el --- a mode for showing the summary of diff

;; Copyright (C) 2011 Hans(Hyeonseung) Jang

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; Keywords: convenience patch diff

;;; Commentary:
;;
;; Diffstat is a simple implementation of diffstat unix tool in Emacs.  This
;; mode can be used as standalone for a certain diff file but it will be more
;; useful with vc mode.
;;
;; You can simply do M-x diffstat in diff-mode buffer to get diffstat.
;;
;; C-q / q      : Quit
;; C-c / C-q    : Go back to the place in the diff file corresponding
;;              : to file on the current line
;;
;;
;; You might want to add the following in your .emacs:
;; 
;; (require 'diffstat)
;; (add-hook 'diff-mode-hook (lambda () (local-set-key "\C-c\C-l" 'diffstat)))
;;
;;
;; NOTE : It only works with unified-diff format.
;;

(defgroup diffstat nil
  "Show summary and histogram of modifications of diff."
  :group 'tools
  :group 'diff)


(defcustom diffstat-line-width 120
  "This determines how many spaces are  used for + and - marks."
  :type 'number
  :group 'diffstat)


(define-derived-mode diffstat-mode fundamental-mode "Diffstat"
  "Major mode for showing summary and histogram of modifications of diff file.")

(defcustom diff-mode-hook nil
  "Run after setting up the `diffstat-mode' major mode."
  :type 'hook
  :group 'diff-mode)

(easy-mmode-defmap diffstat-mode-map
  `(("\C-c\C-l" . diffstat-goto-diff)
    ("\C-c\C-c" . diffstat-goto-diff)
    ([return] . diffstat-goto-diff)
    ("\C-c\C-q" . diffstat-quit)
    ("q" . diffstat-quit)
    )
  "Keymap for `diffstat-mode'.")


(defconst diffstat-file-header-re
  "^--- \\([^[:blank:]]+\\).*\n\\+\\+\\+ \\([^[:blank:]]+\\).*\n@@ -\\([0-9,]+\\) \\+\\([0-9,]+\\) @@")

(defun diffstat()
  (interactive)
  (let ((oldbuf (current-buffer))
        (current-point (point))
        (max-filename-width 0)
        (max-changed-lines 0)
        result-list scale-factor)
    (save-excursion
      (goto-char 0)

      ;; Loop per each file
      (while (re-search-forward diffstat-file-header-re nil t)
        (let* ((file-begin-point (match-beginning 0))
               (file-begin-end-point (match-end 0))
               file-end-point
               (src-file (match-string-no-properties 1))
               (dest-file (match-string-no-properties 2))
               (src-range (match-string-no-properties 3))
               (dest-range (match-string-no-properties 4))
               (file-changed
                (cond ((equal src-range "0,0")
                       (concat "+ " dest-file))
                      ((equal dest-range "0,0")
                       (concat "- " src-file))
                      (t 
                       (concat "  " src-file))))
               (count-plus 0)
               (count-minus 0))

          ;; Find the end of current file or the end of buffer if it is the last
          ;; file.
          (goto-char file-begin-end-point)
          (setq file-end-point
                (if (re-search-forward diffstat-file-header-re nil t)
                    (- (match-beginning 0) 1)
                  (buffer-end 1)))

          ;; Count the number of modified lines which starts with '+' and '-'
          (goto-char file-begin-end-point)
          (while (re-search-forward "^\\(\\+\\)\\|^\\(\\-\\)" file-end-point t)
            (if (match-beginning 1)
                (setq count-plus (1+ count-plus))
              (setq count-minus (1+ count-minus))))

          (setq result-list 
                (append 
                 result-list 
                 (list (list file-changed file-begin-point file-end-point 
                             count-plus count-minus)))
                max-changed-lines 
                (max (+ count-plus count-minus) max-changed-lines)
                max-filename-width 
                (max (length file-changed) max-filename-width))
          (goto-char file-end-point)))
      
      (unless result-list
        (error "Nothing to parse. Maybe not in diff mode."))

      (when result-list
        (switch-to-buffer-other-window
         (set-buffer (get-buffer-create
		      ;; Append '-stat' to diff buffer for the diffstat buffer
		      (let ((name (buffer-name)))
			(if (string-match "\\*\\(.*\\)\\*" name)
			    (format "*%s-stat*" (match-string 1 name))
			  (concat name "-stat"))))))


        (setq buffer-read-only nil)
        (erase-buffer)

        ;; Calculate scale factor to squeeze number of markers into a fixed
        ;; space
        (let ((available-width-for-markers 
               (- diffstat-line-width max-filename-width 7)))
          (setq scale-factor 
                (if (>= available-width-for-markers max-changed-lines) 1
                  (/ available-width-for-markers (float max-changed-lines)))))

        (mapcar (lambda(i)
                  (let ((filename (nth 0 i))
                        (begin-point (nth 1 i))
                        (end-point (nth 2 i))
                        (count-plus (nth 3 i))
                        (count-minus (nth 4 i)))
                    (insert 
                     (concat
                      (if (and (<= current-point end-point)
                               (>= current-point begin-point)) "*" " ")
                      (format (format "%%-%ds"
                                      max-filename-width) filename)
                      (format " |%4d " (+ count-plus count-minus))
                      (make-string 
                       (max 0 (ceiling (* count-plus scale-factor))) ?+)
                      (make-string 
                       (max 0 (ceiling (* count-minus scale-factor))) ?-)
                      "\n"
                      )))) result-list)

        (diffstat-mode)
        (set (make-local-variable 'diffstat-diff-buffer) oldbuf)
        (set (make-local-variable 'diffstat-buffer) (current-buffer))
        (toggle-truncate-lines t)
        (setq buffer-read-only t)

        ;; Center point to the diffstat line corresponding to the current point
        ;; position in diff buffer.
        (goto-char 0)
        (if (re-search-forward "^\\*.+" nil t)
            (goto-char (match-beginning 0)))
        (recenter)))))


(defun diffstat-goto-diff ()
  "Jump to the corresponding diff line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward ".\\(.\\).\\([^[:blank:]]+\\)")
        (let* ((type (match-string-no-properties 1))
               (filename (match-string-no-properties 2))
               (file-re 
                (if (or (equal type " ") (equal type "-"))
                    (format "^--- %s.*\n\\+\\+\\+ [^[:blank:]]+.*\n@@"
                            filename)
                  (format "^--- [^[:blank:]]+.*\n\\+\\+\\+ %s.*\n@@" 
                          filename))))
          (switch-to-buffer-other-window diffstat-diff-buffer)
          (goto-char 0)
          (when (re-search-forward file-re nil t)
	    (goto-char (match-beginning 0))
	    (recenter))))))

(defun diffstat-quit ()
  (interactive)
  (let ((buffer diffstat-buffer))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(provide 'diffstat)

;;; diffstat.el ends here
