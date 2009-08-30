;;; contentswitch.el --- switch to buffer/file by content

;; Copyright (C) 2008  Tamas Patrovics

;; $LastChangedDate: 2008-08-22 19:58:23 +0200 (P, 22 aug. 2008) $

;; Contributors:
;;     Ted Zlatanov

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

;; Since the advent of timid.el, anything.el and similar tools one can
;; quickly open a recent file by typing a part of its name without
;; knowing in which directory the file is located. It's very
;; convenient, but what if one doesn't remember the name either, only
;; some of the file contents?
;;
;; This package provides a command `contentswitch' which allows the
;; user to switch to a buffer or an unopened file by typing a part of
;; its content, instead of its name, and selecting a file from the
;; result list with the cursor keys and ENTER. (By default the
;; substring is also tested on the file/buffer name, but the purists
;; can disable it with an option.) Note that the search works for
;; files only if you (as all other sane people) use recentf, savehist
;; or a similar package.
;;
;; Matching on the file content can be useful if one can come up with
;; a fairly unique string from the file, but what if the string is not
;; unique enough? Usually, the first thing popping into one's mind is
;; the direct context of what he was working on. E.g. I began to
;; implement that for loop for counting things. "for" is not a really
;; unique word, so typing it probably results in lots of unwanted
;; matches. To avoid this the package can list those matches first
;; which have the given string within the context of the current
;; position of point in the file. See option
;; `contentswitch-context-bias' about tuning this behavior. The
;; context search requires saveplace.el, otherwise point location
;; cannot be determined for unopened files.
;; 
;; See additional configuration options below.
;; 

;; Tested on Emacs 22

;;; Code:

(defgroup contentswitch nil
  "The contentswitch package."
  :version "22.1")

(defcustom contentswitch-context-bias (if (featurep 'saveplace) 100)
  "If nil then matching files are simply listed in LRU order.

If t then the closer the match to the current position of point in
the buffer is, the higher it is listed in the results.

If it is a number then it is the same as the previous option with
the difference that those files which have a match within the
specified distance (in characters) from the current position of
point are given the same priority and listed in LRU
order. Probably this is what most people want, since the user
tends to remember what he did last time in a file, but didn't
know which word was the closest to point, so the most recent file
with a match within the given context is very likely the one he
is after.

If you use a non-nil value for this variable then you should also
use saveplace.el, because saveplace information is used to
restore point location for unopened files. Without that
information this feature useless."
  :group 'contentswitch
  :type '(radio (const :format "LRU order " nil)
		(const :format "Distance from point" t)
		(integer :format "Match within distance: %v")))

(defcustom contentswitch-enable-name-matches t
  "Match the query string against the buffer and file name as well."
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-jump-to-match-location nil
  "After switching, put point at match."
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-file-history-variable
  (if (featurep 'recentf)
      'recentf-list
    'file-name-history)
  "The file names are taken from this variable.

If you use recentf then make sure it is loaded before this
package, so the default can be initialized properly.

If you don't use recentf and fall back to the standard variable
`file-name-history' then it is recommended to use savehist.el or
a similar package, so that the list of recently opened files is
restored when Emacs is restarted."
  :group 'contentswitch
  :type  '(choice :tag "File history"
		  (const :tag "recentf" recentf-list)
		  (const :tag "file name history" file-name-history)
		  (const :tag "Empty"  nil)))

(defcustom contentswitch-ignore '("^*")
  "List of regexps for excluding buffer and files from the results."
  :group 'contentswitch
  :type '(repeat (regexp :tag "Regular expression")))

(defcustom contentswitch-ignore-remote-files t
  "Do not search remote unopened files for matches."
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-ignore-encrypted-files t
  "Do not search encrypted unopened files for matches."
  :group 'contentswitch
  :type 'boolean)

(defcustom contentswitch-max-files-from-history 30
  "Do not show more matches from file history than this limit."
  :group 'contentswitch
  :type 'integer)

(defcustom contentswitch-file-completion-delay 0.3
  "Delay before showing completions from file history."
  :group 'contentswitch
  :type 'float)

(defcustom contentswitch-max-name-length 25
  "Width of the name column in the result list."
  :group 'contentswitch
  :type 'integer)

(defcustom contentswitch-before-context-length 10
  "Number of characters from context shown before the actual match."
  :group 'contentswitch
  :type 'integer)

(defcustom contentswitch-selection-face 'highlight
  "Face for selection."
  :group 'contentswitch
  :type 'face)

(defcustom contentswitch-context-face 'header-line
  "Face for match context."
  :group 'contentswitch
  :type 'face)

(defcustom contentswitch-match-face 'lazy-highlight
  "Face for match."
  :group 'contentswitch
  :type 'face)


(defvar contentswitch-map 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'contentswitch-next-line)
    (define-key map (kbd "<up>") 'contentswitch-previous-line)
    (define-key map (kbd "<prior>") 'contentswitch-previous-page)
    (define-key map (kbd "<next>") 'contentswitch-next-page)
    (define-key map (kbd "<RET>") 'exit-minibuffer)
    map)
  "Keymap.")

;;; end of user configuration

(require 'cl)


(defconst contentswitch-buffer "*contentswitch*"
  "Buffer used for finding files.")
 
(defvar contentswitch-overlay nil
  "Overlay used to highlight the current selection.")

(defvar contentswitch-current-input ""
  "The previous input substring used for searching.")
 
(defvar contentswitch-idle-timer nil
  "Idle timer for file matches.")
 


(defun contentswitch-mark-current-line ()
  "Mark current line with a distinctive color."
  (move-overlay contentswitch-overlay (point-at-bol) (1+ (point-at-eol))))
 
 
(defun contentswitch-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (contentswitch-move-selection 'next-line -1))
 
 
(defun contentswitch-next-line ()
  "Move selection to the next line."
  (interactive)
  (contentswitch-move-selection 'next-line 1))
 
 
(defun contentswitch-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (contentswitch-move-selection 'scroll-down nil))
 
 
(defun contentswitch-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (contentswitch-move-selection 'scroll-up nil))
 
 
(defun contentswitch-move-selection (movefunc movearg)
  "Move the selection marker to a new position determined by
MOVEFUNC and MOVEARG."
  (unless (= (buffer-size (get-buffer contentswitch-buffer)) 0)
    (save-selected-window
      (select-window (get-buffer-window contentswitch-buffer))
      (condition-case nil
          (funcall movefunc movearg)
        (beginning-of-buffer (goto-char (point-min)))
        (end-of-buffer (goto-char (point-max))))

      ;; if line end is point-max then it's either an incomplete line or
      ;; the end of the output, so move up a line
      (if (= (point-at-eol) (point-max))
          (next-line -1))

      (contentswitch-mark-current-line)

      (let* ((object (plist-get (contentswitch-get-jump-info) 'object))
             (file (if (bufferp object)
                       (if (buffer-file-name object)
                           (expand-file-name (buffer-file-name object)))
                     object)))
        (when file
          (message "File path: %s" file))))))
      

(defun contentswitch-get-buffer-info (buffer)
  "Return match information in BUFFER or nil if there is no match."
  (append (with-current-buffer buffer
            (if (if contentswitch-context-bias
                    (let* ((point (point))
                           (backward (save-excursion
                                       (if (search-backward 
                                            contentswitch-current-input nil t)
                                           (cons (abs (- point (point))) 
                                                 (match-data)))))
                           (forward (save-excursion
                                      (if (search-forward 
                                           contentswitch-current-input nil t)
                                          (cons (abs (- point (point))) 
                                                (match-data))))))
                      (if backward
                          (if forward
                              (progn
                                (set-match-data 
                                 (if (< (car forward) (car backward))
                                     (cdr forward)
                                   (cdr backward)))
                                t)

                            (set-match-data  (cdr backward))
                            t)

                        (when forward
                          (set-match-data  (cdr forward))
                          t)))
                  
                  (save-excursion
                    (goto-char (point-min))
                    (search-forward contentswitch-current-input nil t)))

                (save-excursion
                  (goto-char (match-beginning 0))
                  (list
                   'line (buffer-substring-no-properties
                          (point-at-bol)
                          (point-at-eol))
                   'line-start (point-at-bol)
                   'start (match-beginning 0)
                   'end (match-end 0)))))

          (if contentswitch-enable-name-matches
              (let ((start (string-match 
                            (regexp-quote contentswitch-current-input)
                            (buffer-name buffer))))
                (if start                    
                    (list 'name-start start
                          'name-end (match-end 0)))))))


(defun contentswitch-check-input ()
  "Check input string and update the list of matching files."
  (unless (equal (minibuffer-contents) contentswitch-current-input)
    (setq contentswitch-current-input (minibuffer-contents))

    (when contentswitch-idle-timer
      (cancel-timer contentswitch-idle-timer)
      (setq contentswitch-idle-timer nil))

    (with-current-buffer contentswitch-buffer
      (erase-buffer))

    (contentswitch-display-matches
     (delete-if-not
      (lambda (info)
        info)

      (mapcar (lambda (buffer)
                (let ((info (unless (equal contentswitch-current-input "")
                              (contentswitch-get-buffer-info buffer))))
                  (if (or info
                          (equal contentswitch-current-input ""))
                      (plist-put (plist-put info 'object buffer)
                                 'point (with-current-buffer buffer
                                          (point))))))

              (delete-if (lambda (buffer)
                           (or (eq (aref (buffer-name buffer) 0) ?\ )
                               (some (lambda (regex)
                                       (string-match
                                        regex (buffer-name buffer)))
                                     contentswitch-ignore)
                               (eq buffer (get-buffer contentswitch-buffer))))
                           
                         (buffer-list)))))

      
    (unless (equal contentswitch-current-input "")
      (setq contentswitch-idle-timer
            (run-with-idle-timer 
             contentswitch-file-completion-delay nil 
             (lambda ()
               (setq contentswitch-idle-timer nil)
               (contentswitch-display-matches
                (let (infos files)
                  (dolist (file (symbol-value contentswitch-file-history-variable))
                    (unless (or (get-file-buffer file) ; opened files
                                                       ; are dealt
                                                       ; with above
                                (member file files)
                                (file-directory-p file)
                                (and contentswitch-ignore-remote-files
                                     (file-remote-p file))
                                (and contentswitch-ignore-encrypted-files
                                     (featurep 'epa-file)
                                     (string-match epa-file-name-regexp file))
                                (not (file-readable-p file))
                                (some (lambda (regex)
                                        (string-match regex file))
                                     contentswitch-ignore))
                      (let* ((buffer (find-file-noselect file t))
                             (point (with-current-buffer buffer
                                      (if (and contentswitch-context-bias
                                               (featurep 'saveplace))
                                          ;; restore saved point position
                                          (save-place-find-file-hook))
                                      (point)))
                             (info (contentswitch-get-buffer-info buffer)))

                        ;; since we suppressed the opening ceremony
                        ;; above there no need to performing the
                        ;; closing one
                        (let ((kill-buffer-hook nil))
                          (kill-buffer buffer))

                        (when info
                          (push file files)
                          (push (plist-put (plist-put info 'object file) 
                                           'point point)
                                infos)
                          (if (> (length files)
                                 contentswitch-max-files-from-history)
                              (return))))))

                  ;; because of push
                  (nreverse infos)))))))))
         

(defun contentswitch-display-matches (infos)
  "Display INFOS in the result buffer."
  (unless (equal contentswitch-current-input "")
    (if contentswitch-context-bias
        (setq infos 
              (sort infos 
                    (lambda (info1 info2)
                      (if (plist-get info1 'start)
                          (if (plist-get info2 'start)
                              (let ((diff1 (abs (- (plist-get info1 'point)
                                                   (plist-get info1 'start))))
                                    (diff2 (abs (- (plist-get info2 'point)
                                                   (plist-get info2 'start)))))
                                (if (numberp contentswitch-context-bias)
                                    (if (<= diff1 contentswitch-context-bias)
                                        (> diff2 contentswitch-context-bias)

                                      (unless (<= diff2 contentswitch-context-bias)
                                        (< diff1 diff2)))

                                  (< diff1 diff2)))

                            ;; info2 has no content match, so info1
                            ;; is ranked higher
                            t)

                        ;; if info2 has content match then
                        ;; it is ranked higher
                        (plist-get info2 'start)))))))

  (let ((window-width (window-width (selected-window))))
    (with-current-buffer contentswitch-buffer
      (save-excursion
        (goto-char (point-max))

        (dolist (info infos)
          (let* ((name (if (bufferp (plist-get info 'object))
                           (buffer-name (plist-get info 'object))
                         (file-name-nondirectory (plist-get info 'object))))
                 padded-name match)

            (if (not (plist-get info 'line))
                (insert name)
                     
              (setq padded-name (concat 
                                 name
                                 (make-string
                                  (+ (max 0 (- contentswitch-max-name-length
                                               (length name)))
                                     4)
                                  ? )))

              (let ((start-in-line (- (plist-get info 'start)
                                      (plist-get info 'line-start))))
                (setq match (concat 
                             padded-name
                             (substring
                              (plist-get info 'line)
                              (max 0 (- start-in-line
                                        contentswitch-before-context-length))
                              start-in-line)))

                (insert match)
                (insert (substring (plist-get info 'line)
                                   start-in-line
                                   (+ start-in-line
                                      (min (- (length (plist-get info 'line))
                                              start-in-line)
                                           (- window-width 1 (length match))))))))

            (put-text-property (point-at-bol) (point-at-eol) 
                               'contentswitch-jump-info info)

            (insert "\n")

            (save-excursion
              (forward-line -1)

              (when match
                (let ((overlay (make-overlay (+ (point-at-bol) (length padded-name))
                                             (1+ (point-at-eol)))))
                  (overlay-put overlay 'face contentswitch-context-face))

                (let ((overlay (make-overlay (+ (point-at-bol)
                                                (length match))
                                             (+ (point-at-bol)
                                                (length match)
                                                (- (plist-get info 'end)
                                                   (plist-get info 'start))))))
                  (overlay-put overlay 'face contentswitch-match-face)))

              (if (plist-get info 'name-start)                    
                  (let ((overlay (make-overlay 
                                  (+ (point-at-bol)
                                     (plist-get info 'name-start))
                                  (+ (point-at-bol)
                                     (plist-get info 'name-end)))))
                    (overlay-put overlay 'face contentswitch-match-face)))))))))


  (if (and (> (with-current-buffer contentswitch-buffer (buffer-size)) 0)
           (= (overlay-start contentswitch-overlay) ; no selection yet
              (overlay-end contentswitch-overlay)))
      (save-selected-window
        (select-window (get-buffer-window contentswitch-buffer))
        (goto-char (point-min))
        (contentswitch-mark-current-line))))


(defun contentswitch-get-jump-info ()
  "Get the jump information for the current line."
  (with-current-buffer  contentswitch-buffer
    (get-text-property (overlay-start contentswitch-overlay)
                       'contentswitch-jump-info)))


(defun contentswitch-do ()
  (erase-buffer)
  (setq mode-name "Switch by Content")

  (if contentswitch-overlay
      ;; make sure the overlay belongs to the contentswitch buffer if
      ;; it's newly created
      (move-overlay contentswitch-overlay (point-min) (point-min)
                    (get-buffer contentswitch-buffer))

    (setq contentswitch-overlay (make-overlay (point-min) (point-min)))
    (overlay-put contentswitch-overlay 'face contentswitch-selection-face))

  (setq contentswitch-current-input nil)
  (add-hook 'post-command-hook 'contentswitch-check-input)

  (setq contentswitch-idle-timer nil)
 
  (with-current-buffer contentswitch-buffer
    (setq cursor-type nil))
 
  (unwind-protect
      (let ((minibuffer-local-map contentswitch-map))
        (read-string "string: "))
 
    (remove-hook 'post-command-hook 'contentswitch-check-input)
 
    (with-current-buffer contentswitch-buffer
      (setq cursor-type t))))


(defun contentswitch ()
  (interactive)
  (let ((winconfig (current-window-configuration))
        ;; we need coding system and format conversion for
        ;; `find-file-noselect', but don't need any of the
        ;; `after-find-file' nonsense, so the definition of
        ;; `after-find-file' is suppressed temporarily
        (orig-fun (symbol-function 'after-find-file)))

    (fset 'after-find-file (lambda (&optional error warn noauto
                                              after-find-file-from-revert-buffer
                                              nomodes)))
    (pop-to-buffer contentswitch-buffer)

    (unwind-protect
        (contentswitch-do) 

      (fset 'after-find-file orig-fun)
      (set-window-configuration winconfig)))
 
  (unless (= (buffer-size (get-buffer contentswitch-buffer)) 0)
    (let* ((info (contentswitch-get-jump-info))
           (object (plist-get info 'object)))
      (if (bufferp object)
          (switch-to-buffer object)
        (find-file object))
      (if (and contentswitch-jump-to-match-location
               (plist-get info 'start))
          (goto-char (plist-get info 'start))))))
        


(put 'contentswitch 'timid-completion 'disabled)

(provide 'contentswitch)
;;; contentswitch.el ends here
