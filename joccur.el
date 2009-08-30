;;; joccur.el --- An improved 'occur' implementation.

;; Author: Javier Oviedo <email_joviedo@yahoo.com>
;; Created:  17 Feb 2004

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This package implements the functionality of "occur" but with some
;; modifications. The *JOccur* buffer will display the lines containing
;; the matching regexp with the original text properties.
;;
;; I used the occur-mode code in replace.el(GNU Emacs 21.3.50.8) as the model
;; for this package. Indeed, a few helper functions are copied directly from
;; replace.el and renamed. The remaining helper functions I have reimplemented.
;; In addition, the actual "occur" engine has been modified extensively.
;;
;; Comments and/or constructive criticism is always welcome.
;;

;; Installation:
;;
;; 1. Place joccur.el in your emacs load-path
;; 2. Add (require 'joccur) to your .emacs file
;;

;; Usage:
;;
;; M-x joccur regexp
;;

;;; Change Log:
;;
;; Version 1.3.1 03/11/2004  Javier Oviedo
;; - Fixed a joccur-mode-mouse-goto/joccur-mode-goto-occurrence bug. 
;;   These functions are bound to enter, mouse-2, etc.
;; - Added marker and text property for the entire line in the joccur buffer.
;;   Previously there were only markers in the joccur buffer on the matches.
;;   Now it is possible to jump to the line when the cursor is not on an
;;   exact regexp match.
;; - Added additional key definitions to the joccur-mode-map.
;;
;; Version 1.3 03/08/2004  Javier Oviedo
;; - Restructered joccur-engine using markers for each individual occurance
;;   of REGEXP...modified to better support multiple occurances per line.
;;
;; Version 1.2.3 03/04/2004  Javier Oviedo
;; - Cleaned up byte compiler. Added let-bindings and defvars.
;;
;; Version 1.2.2 02/25/2004  Javier Oviedo
;; - Made a joccur-regexp buffer-local variable. This allows switching between
;;   multiple joccur buffers with different regexp.
;; - Made a joccur-input-buffer buffer-local variable.
;; - Made overlays buffer-local. This allows each buffer to maintain the
;;   current highlighted/overlay match when switching between multiple joccur
;;   buffers.
;;
;; Version 1.2.1 02/24/2004  Javier Oviedo
;; - Joccur now handles the generic case of any font-lock-support-mode,
;;   instead of just lazy-lock-mode.
;;
;; Version 1.2 02/23/2004  Javier Oviedo
;; - Implemented multi-occur for joccur...called multi-joccur
;; - Handle multiple occurances of REGEXP in a line
;; - Various functionality improvements(additional overlays, etc.)
;;
;; Version 1.1 02/18/2004  Javier Oviedo
;; - Adding highlight of regexp when displaying occurance in originating buffer.
;; - Changed joccur-match-face.
;; - Added joccur-display-face and joccur-title-face
;; - Overlays are destroyed on killing/deleting of window/buffer
;; - Unique Joccur buffer created every time...based on search buffer and regexp.
;; - Introduced "Change Log" and "Usage" sections.
;;
;; Version 1.0 02/17/2004
;; - Introduced joccur package.
;;


(defconst joccur-version "1.3.1"
  "This version of joccur.el.")

(defconst joccur-buf "*JOccur*"
  "Name of joccur buffer to use.")

(defvar joccur-buf-cur nil
  "Name of current joccur buffer to use.")

(defvar joccur-input-buffer nil
  "Name of input buffer to run joccur on.")

(defvar joccur-input-buffer-local nil
  "Name of input buffer for the current joccur buffer.")

(defvar joccur-match-overlay nil
  "The overlay used to highlight matched text in the *JOccur* buffer.")

(defvar joccur-cur-match-overlay nil
  "The overlay used to highlight matched text in the *JOccur* buffer.")

(defvar joccur-display-overlay nil
  "The overlay used to highlight matched text in the originating buffer.")

(defvar joccur-regexp nil
  "The current joccur regexp.")

(defvar joccur-regexp-local nil
  "The regexp for the current joccur buffer.")


(defvar joccur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" 'joccur-mode-display-occurrence)
    (define-key map "p" 'joccur-prev)
    (define-key map "n" 'joccur-next)
    (define-key map [mouse-2] 'joccur-mode-mouse-goto)
    (define-key map "\C-c\C-c" 'joccur-mode-goto-occurrence)
    (define-key map "\C-m" 'joccur-mode-goto-occurrence)
    (define-key map "o" 'joccur-mode-goto-occurrence-other-window)
    (define-key map "\C-o" 'joccur-mode-display-occurrence)
    (define-key map "\M-n" 'joccur-next)
    (define-key map "\M-p" 'joccur-prev)
    (define-key map "r" 'joccur-rename-buffer)
    (define-key map "c" 'clone-buffer)
    (define-key map "g" 'revert-buffer)
    (define-key map "q" 'joccur-delete-window)
    (define-key map "z" 'joccur-kill-this-buffer)
    map)
  "Keymap for `joccur-mode'.")

(defface joccur-title-face
  '((t (:slant italic :underline t)))
  "Face for Joccur mode."
  :group 'joccur)

(defface joccur-match-face
  '((t (:weight bold :slant italic :foreground "black" :background "SteelBlue1")))
  "Face for Joccur mode."
  :group 'joccur)

(defface joccur-cur-match-face
  '((t (:weight bold :slant italic :foreground "black" :background "yellow")))
  "Face for Joccur mode."
  :group 'joccur)

(defface joccur-display-face
  '((t (:weight bold :slant italic :foreground "black" :background "pink")))
  "Face for Joccur mode."
  :group 'joccur)

(defcustom joccur-title-highlight 'joccur-title-face
  "*Face used for title line in the *JOccur* buffer.
If the value is nil, don't highlight."
  :type 'face
  :group 'matching)

(defcustom joccur-match-highlight 'joccur-match-face
  "*Face used to higlight regexp matches in the *JOccur* buffer.
If the value is nil, don't highlight."
  :type 'face
  :group 'matching)

(defcustom joccur-cur-match-highlight 'joccur-cur-match-face
  "*Face used to higlight regexp matches in the *JOccur* buffer.
If the value is nil, don't highlight."
  :type 'face
  :group 'matching)

(defcustom joccur-display-highlight 'joccur-display-face
  "*Face used to higlight regexp matches in the *JOccur* buffer.
If the value is nil, don't highlight."
  :type 'face
  :group 'matching)

(defcustom joccur-mode-hook '(turn-on-font-lock)
  "Hook run when entering JOccur mode."
  :type 'hook
  :group 'matching)


(put 'joccur-mode 'mode-class 'special)
(defun joccur-mode ()
  "Major mode for output from \\[joccur].
\\<joccur-mode-map>Move point to one of the items in this buffer, then use
\\[joccur-mode-goto-joccurrence] to go to the joccurrence that the item refers to.
Alternatively, click \\[joccur-mode-mouse-goto] on an item to go to it.
\\{joccur-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map joccur-mode-map)
  (setq major-mode 'joccur-mode)
  (setq mode-name "Joccur")
  (setq case-fold-search t)
  (run-hooks 'joccur-mode-hook))

(defun joccur-mode-mouse-goto (event)
  "In JOccur mode, go to the occurrence whose line you click on."
  (interactive "e")
  (let (pos)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq pos (joccur-mode-find-occurrence))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)))

(defun joccur-mode-find-occurrence ()
  (let ((pos (get-text-property (point) 'joccur-target)))
    (unless pos
      (error "No occurrence on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this occurrence was killed"))
    (setq joccur-buf-cur (buffer-name))
    (joccur-mode-display-highlight pos)
    pos))

(defun joccur-mode-goto-occurrence ()
  "Go to the occurrence the current line describes."
  (interactive)
  (let ((pos (joccur-mode-find-occurrence)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)))

(defun joccur-mode-goto-occurrence-other-window ()
  "Go to the occurrence the current line describes, in another window."
  (interactive)
  (let ((pos (joccur-mode-find-occurrence)))
    (switch-to-buffer-other-window (marker-buffer pos))
    (goto-char pos)))

(defun joccur-mode-display-occurrence ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (let ((pos (joccur-mode-find-occurrence))
	window
	;; Bind these to ensure `display-buffer' puts it in another window.
	same-window-buffer-names
	same-window-regexps)
    (setq window (display-buffer (marker-buffer pos)))
    ;; This is the way to set point in the proper window.
    (save-selected-window
      (select-window window)
      (goto-char pos))))

(defun joccur-mode-display-highlight(pos)
  "Create or move the `joccur-display-highlight' overlay."
  (interactive)
  (save-excursion
    (with-current-buffer (marker-buffer pos)
      (goto-char pos)
      (with-current-buffer joccur-buf-cur
        (setq joccur-regexp joccur-regexp-local))
      (re-search-forward joccur-regexp nil t)
      (let ((matchbeg (match-beginning 0))
            (matchend (match-end 0)))
        (if joccur-display-overlay
            (move-overlay joccur-display-overlay matchbeg matchend)
          (set (make-local-variable 'joccur-display-overlay) t)
          (setq joccur-display-overlay (make-overlay matchbeg matchend))
          (overlay-put joccur-display-overlay 'face joccur-display-highlight)
          (overlay-put joccur-display-overlay 'priority 1))))))

(defun joccur-mode-display-unhighlight ()
  "Delete the `joccur-display-highlight' overlay."
  (interactive)
  (with-current-buffer joccur-input-buffer-local
    (if joccur-display-overlay
        (delete-overlay joccur-display-overlay))
    (setq joccur-display-overlay nil)))

(defun joccur-mode-match-unhighlight ()
  "Delete the `joccur-display-highlight' overlay."
  (interactive)
  (if joccur-match-overlay
      (delete-overlay joccur-match-overlay))
  (setq joccur-match-overlay nil))

(defun joccur-delete-window ()
  (interactive)
  (joccur-mode-display-unhighlight)
  (delete-window))

(defun joccur-kill-this-buffer ()
  (interactive)
  (joccur-mode-display-unhighlight)
  (kill-buffer-and-window))

(defun joccur-next ()
  "Move to the next match in a JOccur mode buffer."
  (interactive)
  (let (joccur-next-match)
    (if (eq (count-lines (point-min) (point)) 0)
        (forward-line 1))
    (setq joccur-regexp joccur-regexp-local)
    (setq joccur-next-match (re-search-forward joccur-regexp nil t))
    (if (not joccur-next-match)
        (let ()
          (error "No more matches"))
      (let ((matchbeg (match-beginning 0))
            (matchend (match-end 0)))
        (goto-char (1- (point)))
        (setq joccur-buf-cur (current-buffer))
        (if joccur-cur-match-overlay
            (move-overlay joccur-cur-match-overlay
                          matchbeg matchend joccur-buf-cur)
          (set (make-local-variable 'joccur-cur-match-overlay) t)
          (setq joccur-cur-match-overlay
                (make-overlay matchbeg matchend joccur-buf-cur))
          (overlay-put joccur-cur-match-overlay
                       'face joccur-cur-match-highlight)
          (overlay-put joccur-cur-match-overlay 'priority 2))))))

(defun joccur-prev ()
  "Move to the previous match in a JOccur mode buffer."
  (interactive)
  (setq joccur-regexp joccur-regexp-local)
  (re-search-backward joccur-regexp nil t)
  (if (eq (count-lines (point-min) (point)) 1)
      (let ()
        (forward-line 1)
        (joccur-next)
        (error "No earlier Matches"))
    (let ((matchbeg (match-beginning 0))
          (matchend (match-end 0)))
      (setq joccur-buf-cur (current-buffer))
      (if joccur-cur-match-overlay
          (move-overlay joccur-cur-match-overlay
                        matchbeg matchend joccur-buf-cur)
        (set (make-local-variable 'joccur-cur-match-overlay) t)
        (setq joccur-cur-match-overlay
              (make-overlay matchbeg matchend joccur-buf-cur))
        (overlay-put joccur-cur-match-overlay
                     'face joccur-cur-match-highlight)
        (overlay-put joccur-cur-match-overlay 'priority 2)))))

(defun joccur-read-primary-args ()
  (list (let* ((default (car regexp-history))
	       (input
		(read-from-minibuffer
		 (if default
		     (format "List lines matching regexp (default `%s'): "
			     default)
		   "List lines matching regexp: ")
		 nil
		 nil
		 nil
		 'regexp-history)))
	  (if (equal input "")
	      default
	    input))
	(when current-prefix-arg
	  (prefix-numeric-value current-prefix-arg))))

(defun joccur-rename-buffer (&optional unique-p)
  "Rename the current *JOccur* buffer to *JOccur: original-buffer-name*.
Here `original-buffer-name' is the buffer name were occur was originally run.
When given the prefix argument, the renaming will not clobber the existing
buffer(s) of that name, but use `generate-new-buffer-name' instead.
You can add this to `joccur-hook' if you always want a separate *JOccur*
buffer for each buffer where you invoke `joccur'."
  (interactive "P")
  (with-current-buffer
      (if (eq major-mode 'joccur-mode)
          (current-buffer)
        (get-buffer "*JOccur*"))
    (let (joccur-buf-unique)
      (setq joccur-buf-unique (concat "*JOccur: "
                                      joccur-input-buffer
                                      " - "
                                      joccur-regexp
                                      "*"))
      (rename-buffer joccur-buf-unique unique-p))))

(defun joccur (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

The lines are shown in a buffer named `*JOccur*'.
It serves as a menu to find any of the occurrences in this buffer.
\\<joccur-mode-map>\\[describe-mode] in that buffer will explain how.

Searches are done in a case-insensitive manner"
  (interactive (joccur-read-primary-args))
  (joccur-1 regexp (list (current-buffer)) t))


(defun joccur-1 (regexp bufs-list joccur-unique-buffer)
  (setq joccur-regexp regexp)
  (get-buffer-create joccur-buf)
  (let (active-bufs)
    (setq active-bufs (delq nil (mapcar #'(lambda (buf)
                                            (when (buffer-live-p buf) buf))
                                        bufs-list)))
    (dolist (input-buf active-bufs)
      (joccur-engine input-buf))
    (display-buffer joccur-buf)
    (pop-to-buffer joccur-buf)
    (setq truncate-lines t)
    (set (make-local-variable 'joccur-regexp-local) joccur-regexp)
    (set (make-local-variable 'joccur-input-buffer-local) joccur-input-buffer)
    (beginning-of-buffer)
    (if joccur-unique-buffer
        (joccur-rename-buffer joccur-unique-buffer))))

  
(defun joccur-engine (input-buf &optional unique-buf)
  (setq joccur-input-buffer (buffer-name input-buf))
  (let ((lines 1)
        (matches 0)
        (marker nil)
        (match-line-marker nil)
        (joccur-font-lock-support-mode nil)
        (joccur-buf-start 0)
        (matchpt nil)
        (matchbeg-input nil)
        (matchend-input nil)
        (matchbeg-output nil)
        (matchend-output nil)
        (beg-output nil)
        (end-output nil)
        (curMatch nil)
        (joccur-buf-output nil))
    (save-excursion
      (with-current-buffer joccur-buf
        (joccur-mode)
        (setq buffer-read-only nil)
        (if unique-buf
            (erase-buffer))
        (goto-char (setq joccur-buf-start (point-max)))
        (with-current-buffer joccur-input-buffer
          (setq joccur-font-lock-support-mode font-lock-support-mode)
          (if joccur-font-lock-support-mode
              (let ()
                (setq font-lock-support-mode nil)
                (font-lock-mode)
                (font-lock-mode)))
          (goto-char (point-min)) ;; begin searching in the buffer
          (while (not (eobp))
            (when (setq matchpt (re-search-forward joccur-regexp nil t))
              (setq matches (1+ matches)) ;; increment match count
              (setq curMatch (buffer-substring (line-beginning-position)
                                               (line-end-position)))
              (setq lines (count-lines (point-min) matchpt))
              (setq joccur-buf-output
                    (concat (format "%7d: " lines) curMatch "\n"))
              (setq match-line-marker (make-marker))
              (set-marker match-line-marker (point-at-bol))
              (with-current-buffer joccur-buf
                (setq beg-output (point))
                (insert joccur-buf-output)
                (setq end-output (point))
                (add-text-properties
                 beg-output (1- (point)) '(mouse-face highlight))
                (add-text-properties
                 beg-output end-output
                 `(joccur-target ,match-line-marker
                                 help-echo
                                 "mouse-2: go to this occurrence"))
                (goto-char beg-output))
              (save-excursion
                (goto-char (point-at-bol))
                (while (re-search-forward joccur-regexp (point-at-eol) t)
                  (setq matchbeg-input (match-beginning 0)
                        matchend-input (match-end 0))
                  (setq marker (make-marker))
                  (set-marker marker matchbeg-input)
                  (with-current-buffer joccur-buf
                    (re-search-forward joccur-regexp (point-at-eol) t)
                    (setq matchbeg-output (match-beginning 0)
                          matchend-output (match-end 0))
                    (add-text-properties
                     matchbeg-output matchend-output
                     `(joccur-target ,marker
                                     help-echo
                                     "mouse-2: go to this occurrence"))
                    (set (make-local-variable 'joccur-match-overlay) t)
                    (setq joccur-match-overlay
                          (make-overlay matchbeg-output matchend-output))
                    (overlay-put joccur-match-overlay
                                 'face joccur-match-highlight)
                    (overlay-put joccur-match-overlay 'priority 1))))
              (with-current-buffer joccur-buf
                (goto-char end-output)))
            (forward-line 1))
          (if joccur-font-lock-support-mode
              (setq font-lock-support-mode joccur-font-lock-support-mode)))
        (insert "\n")
        (goto-char joccur-buf-start)
        (save-excursion
          (let ((beg joccur-buf-start)
                end)
            (insert (format "%d lines matching \"%s\" in buffer: %s\n"
                            matches joccur-regexp joccur-input-buffer))
            (setq end (point))
            (add-text-properties
             beg end
             (append `(font-lock-face ,joccur-title-highlight)))))
        (setq buffer-read-only t)))))

(defun multi-joccur (bufs regexp &optional nlines)
  "Show all lines in buffers BUFS containing a match for REGEXP.
This function acts on multiple buffers; otherwise, it is exactly like
`joccur'."
  (interactive
   (cons
    (let* ((bufs (list (read-buffer "First buffer to search: "
				    (current-buffer) t)))
	   (buf nil)
	   (ido-ignore-item-temp-list bufs))
      (while (not (string-equal
		   (setq buf (read-buffer
			      (if (eq read-buffer-function 'ido-read-buffer)
				  "Next buffer to search (C-j to end): "
				"Next buffer to search (RET to end): ")
			      nil t))
		   ""))
	(add-to-list 'bufs buf)
	(setq ido-ignore-item-temp-list bufs))
      (nreverse (mapcar #'get-buffer bufs)))
    (joccur-read-primary-args)))
  (joccur-1 regexp bufs nil))


(provide 'joccur)

;;; joccur.el ends here
