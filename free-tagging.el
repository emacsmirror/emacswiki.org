;;; free-tagging.el --- A global Status bar for emacs
;;*jonathan
(defconst tagging-version "0.5")
;; Public Domain
;; Original Author: Conrad Barski, M.D.
;; Modifications and Updates: Jonathan Arkell <jonnay@jonnay.net>

;; Some logic for this program was taken from hide-lines.el from Mark Hulme-Jones.
;; decended from  file at http://lisperati.com/tagging.html by Conrad Barski, M.D.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;;; Commentary:
(defgroup tagging '()
  "This package allows you to tag certian locations in a buffer.

You can search a buffer for tags, or view the buffers tag cloud.")

;;; Installation:
;; Put free-tagging.el somewhere in your load-path.
;; (Use M-x show-variable RET load-path to see what your load path is.)
;; Add this to your emacs init file.
;   (require 'free-tagging)

;; Because each language has its own method of doing comments, you
;; might want to do something like this:
;;  (add-hook 'java-mode-hook (lambda () (setq tagging-tagline-indicator "^\s*\*\s*@tags ")
;; This will set up the java mode to use the syntax "* @tags " which should
;; fit nicely with the tagging plugin for eclipse.

;; For my setup, I went even harder core:

;; (defvar c-style-programming-modes
;;   '(c-mode c++-mode objc-mode java-mode php-mode css-mode js2-mode)
;;   "List of c-like modes")

;; (defun convert-mode-name-to-hook (mode-name)
;;   "Converts a mode name into the symbol for its hook"
;;   (intern (concat (symbol-name mode-name) "-hook")))

;; (defun add-c-style-hook (hook)
;;   "Add a hook across all c-style modes."
;;   (mapc (lambda (mode) (add-hook (convert-mode-name-to-hook mode) hook))
;; 		c-style-programming-modes))

;; (add-c-style-hook (lambda () (set (make-local-variable 'tagging-tagline-indicator) "^//\\*")))
;; (add-c-style-hook 'tagging-minor-mode)

;; ;; this will hopefully supercede the previous setting of the tagline indicator. 
;; (add-hook java-mode (lambda () (set (make-local-variable 'tagging-tagline-indicator) "@tag ")))

;; ;; this part could be done better.
;; (defun add-lispy-hook (thing)
;;   "Adds a hook to lisp-mode, emacs-lisp-mode and scheme-mode."
;;   (add-hook 'lisp-mode-hook thing)
;;   (add-hook 'emacs-lisp-mode-hook thing)
;;   (add-hook 'scheme-mode-hook thing))

;; (add-lispy-hook (lambda () (set (make-local-variable 'tagging-tagline-indicator) "^;;\\*")))

;; If you want, just select this area as a region, copy, and yank it, and uncomment, and
;; you should be good to go.

;; note that this file is chock full of tags, both as a personal proof of concept, and
;; as an example for others.  One of the coolest things about tagging code like this
;; is that you can see which code was Conrads, and which code I contributed to,
;; and which code was Conrads that I later hacked.  (filter by first name.  Filter
;; both names to see which code I modified.)


;;; TODO:
;; - Project based tagging, allow for tags to be retrieved from somewhere
;;   other then just the file.  (all tags in a directory, ec.)
;; - Anything integration
;; - tag cloud info (filtered tags, all tags)
;;   - reset button on tag cloud
;; - Tag cloud actions
;;   - right click, subtract from filter, change color of text (red) 

;;; CHANGELOG:
;; v 0.1 - Initial release by Conrad Barski, M.D.
;; v 0.2 - Added showing of tags
;;       - Tag indicators are buffer local, so they can be changed on a per-mode
;;         per project basis
;;       - If ECB is activated, then an ecb tagging window is made.  Note that
;;         you will need to make a layout for ECB that includes that type. (tagging)
;; v 0.3 - Mis-versioned as v 0.4
;; v 0.4 - Fixed dumb startup bugs. (sorry)
;;       - Renamed the file from tagging to free-tagging, (to avoid confusion from
;;         conrads version).
;;       - Added provides
;;       - Released under the GPL
;;       - Extra customizations for debugging info
;;       - Proper word wrapping on the taglist. (It's not 100% yet)
;; v 0.5 - Added faces for subtracted and added filters on the tag cloud
;;       - fixed mouse events
;;       - renamed tagging-weight debug to tagging-debug-weight for consistency sake
;;       - fixed word wrap on tag cloud to be intelligent.  (whoohoo!)

;;; BUGS:
;; - the keyboard actions on the view tags buffer does not work.
;; - subtraction on the tag-cloud isn't working quite right.
;; - changing a tag doesn't re-update the filters

(require 'cl)

;;*macro ecb jonathan
(defmacro tagging-edit-readonly (buffer &rest body)
  "Wrap the entire section of code so that it toggles read-only.
If ecb is available, it will use the ecb macro, otherwise a
simple version."
  (if (featurep 'ecb)
	  `(ecb-with-readonly-buffer ,buffer ,@body)
	  `(save-excursion
		(set-buffer ,buffer)
		(toggle-read-only -1)
		,@forms		
		(toggle-read-only 1))))

;;*variable tagging-definition conrad
(defvar tagging-tagline-indicator "^\\*")
(defvar tagging-tag-characters "a-zA-Z0-9_:-")
(defvar tagging-tag-definition (concat "[" tagging-tag-characters "]+"))
(defvar tagging-tag-input-definition (concat "-?" tagging-tag-definition))

;;*varible filter conrad
(defvar tagging-filter-cur nil)
(defvar tagging-invisible-areas ())

;;*variable buffer jonathan
(defconst tagging-taglist-buffer-name
  (if (featurep 'ecb)
	  " *Tags in current buffer*"
	  "*Tags in current buffer*")
  "Name of the tagging buffer.  If ecb is activated, we need to make the buffer hidden.")

(defvar tagging-taglist-buffer (get-buffer-create tagging-taglist-buffer-name)
  "Buffer that holds the taglist")

;;*tagging-tags-hash variable jonathan
(defvar tagging-tags-hash (make-hash-table)
  "Hashtable that contains the current tags for this file.
This variable is buffer local if set in any way.")
(make-variable-buffer-local 'tagging-tags-hash)


;;*variable show custom jonathan
(defface tagging-taglist-face
  '((default
	  :family "helv"
	  :height 192))
  "Font face that is used for the tags listing.

Note that the height of this face is variable.  (betwen 0.25 and 1.25 of the
size defined here)."
  :group 'tagging)

(defface tagging-taglist-info-face
  '((default
       :inherit 'header-line))
  "Font face to display the currently selected tags")

(defface tagging-taglist-sub-face
  '((default
	  :foreground "firebrick"
	  :inherit 'tagging-taglist-face))
  "Font face for a tag that has been subtracted from the filters")

(defface tagging-taglist-add-face
  '((default
	  :foreground "lime green"
	  :inherit 'tagging-taglist-face))
  "Font face for a tag that has been added to the filters.")

(defcustom tagging-debug nil
  "Debug general debug output for tagging"
  :type 'boolean
  :group 'tagging)

;;*variable show debug custom jonathan
(defcustom tagging-debug-weight nil
   "Spit out relative weight after the tag"
   :type 'boolean
   :group 'tagging)

;;*variable show debug custom jonathan
(defcustom tagging-debug-wrap nil
  "Show obscenely verbose debug info for wrapping"
  :type 'boolean
  :group 'tagging)
;;*jonathan tagging-tags-hash show todo:update todo:optimize
(defun tagging-show-tags ()
  "Shows all the tags in a buffer.

Development notes:
  A few ideas for updating:
    - Check to see if the buffer has changed since last tag scan"
  (interactive)
  (tagging-scan-file)
  (setq tagging-taglist-buffer (get-buffer-create tagging-taglist-buffer-name))
  (tagging-edit-readonly (get-buffer-create tagging-taglist-buffer-name)
	(goto-char (point-min))
	(erase-buffer)
	(insert (propertize (if (null tagging-filter-cur)
							"No filters applied\n"
							(format "Filter: %s\n" tagging-filter-cur))
						'face 'tagging-taglist-info-face))
	(let ((tagging-largest-tag-weight 0)) 
	  (maphash (lambda (k v) (when (> v tagging-largest-tag-weight)
								   (setq tagging-largest-tag-weight (float v))))
			   tagging-tags-hash)
	  (maphash 'tagging-show-tag
			   tagging-tags-hash))))

;;*jonathan show todo:fix event
(defun tagging-show-tag (tag weight)
   "Shows the tag at a particular weight.

This uses a totally evil non-local variable `tagging-largest-tag-weight'
for which I appologize profusely for using."
   (let ((scale (+ 0.25 (/ weight tagging-largest-tag-weight)))
		 (face (cond ((member (cons (symbol-name tag) t)
							  tagging-filter-cur)
					  'tagging-taglist-add-face)
					 ((member (cons (symbol-name tag) nil)
							  tagging-filter-cur)
					  'tagging-taglist-sub-face)
			 		 (t 'tagging-taglist-face)))
		 (map (make-sparse-keymap)))
	 (define-key map [mouse-1] 'tagging-mouse-event)
	 (define-key map [mouse-2] 'tagging-mouse-event)
	 (define-key map [mouse-3] 'tagging-mouse-event)
	 (define-key map [enter] 'tagging-keyboard-add)
	 (define-key map [delete] 'tagging-keyboard-subtract)
	 (insert " " (propertize (symbol-name tag)
							 'face `(:inherit ,face
									 :height ,scale)
							 'pointer 'hand
							 'keymap map))
	 (when tagging-debug-wrap
		   (message "Pos: %s %s -- %s"
					(posn-x-y (posn-at-point (point) (get-buffer-window tagging-taglist-buffer)))
					(window-inside-pixel-edges (get-buffer-window tagging-taglist-buffer))
					(posn-at-point (point) (get-buffer-window tagging-taglist-buffer))))
	 (when (= (car (posn-x-y (posn-at-point (point)
											(get-buffer-window tagging-taglist-buffer))))
			  0)
		   (backward-char (length (symbol-name tag)))
		   (insert "\n")
		   (forward-char (length (symbol-name tag))))
	 (when tagging-debug-weight
		   (insert ":" (format "%s" scale)))))

;;*jonathan show commands event filter ecb ecb-opt
(defun tagging-mouse-event (event)
  "Handle a mouse event"
  (interactive "e")
  (when event
		(let ((tag-action (if (equal 'mouse-1 (car event))
							  'tagging-filter-add
							  'tagging-filter-subtract))
			  (tag (save-excursion
					(select-window (posn-window (event-start event)))
					(goto-char (posn-point (event-start event)))
					(tagging-tag-at-point))))
		  (when tagging-debug (message "Captured event, %s tag %s" tag-action tag))
		  (if (featurep 'ecb)
			  (ecb-select-edit-window)
			  (other-buffer 't))
		  (funcall tag-action tag))))


;;*jonathan ecb show
(defun tagging-ecb-sync-tagbuffer ()
  "Sync up the tags with ecb"
  (ecb-do-if-buffer-visible-in-ecb-frame
    'tagging-taglist-buffer-name
	(tagging-show-tags)))

;;*jonathan at-point
(defun tagging-tag-at-point ()
  "Retrieve the current tag at the point"
  (interactive)
  (save-excursion
   (let* ((match-start (re-search-backward (concat "[^" tagging-tag-characters "]")))
		  (match-end (progn (forward-char)
							(re-search-forward (concat "[^" tagging-tag-characters "]")))))
	 (if (and match-start match-end)
		 (buffer-substring-no-properties (+ 1 match-start) (- match-end 1))
		 nil))))

;;*jonathan ecb
(when (featurep 'ecb) 
	  (defecb-window-dedicator ecb-set-tagging-buffer
		tagging-taglist-buffer
		"Set the buffer in the current window to the bufferinfo-buffer and make this window dedicated for this buffer."
		(switch-to-buffer (get-buffer-create
						   tagging-taglist-buffer-name))
		(setq buffer-read-only t))
	  (add-hook 'ecb-current-buffer-sync-hook 'tagging-ecb-sync-tagbuffer)
	  )

;;*search conrad tagging-filter-cur
(defun tagging-parse-tags (str) ;(tagging-parse-tags "foo -bar 340fdfv sd9fwe8dcmm _--3")
  (labels ((F (pos)
			  (let ((x (string-match tagging-tag-input-definition str pos)))
				(if x
					(cons (if (string= (substring str x (+ x 1)) "-")
							  (cons (substring str (+ x 1) (match-end 0)) nil)
							  (cons (substring str x (match-end 0)) t))
						  (F (match-end 0)))))))
		  (F 0)))

;;*search conrad
(defun tagging-tags-to-string (tags) ;(tagging-tags-to-string '((t . "foo") (nil . "bar")))
  (apply #'concat (mapcar (lambda (tag)
			    (concat (if (cdr tag)
					""
					"-")
				    (car tag)
				    " "))
			  tags)))

;;*jonathan scan tagging-tags-hash todo:optimize
; This is a dumb scan.  A check should be made to make sure we don't
; always scan the buffer.  (why scan a buffer that hasn't changed?)
(defun tagging-scan-file ()
  "Scans the buffer for all tags, and stores it in `tagging-tags-hash'."
  (interactive)
  (clrhash tagging-tags-hash)
  (save-excursion
   (goto-char (point-min))
   (while (re-search-forward tagging-tagline-indicator nil t)
		  (tagging-add-tags-to-hashmap (tagging-get-tags-on-line))
		  (forward-line 1))))

;;*jonathan scan tagging-tags-hash
(defun tagging-get-tags-on-line ()
  "Search current line from point, and grab all tags."
  (let ((result '()))
	(while (re-search-forward (concat "\\(" tagging-tag-definition "\\)+") (line-end-position) t)
		   (let ((pos 1))
			 (while (match-string pos)
					(setq result (cons (match-string-no-properties pos) result))
					(setq pos (+ 1 pos)))))
	result))

;;*jonathan tagging-tags-hash
(defun tagging-add-tags-to-hashmap (tags)
  "Add a list of tags to the `tagging-tags-hash'."
  (when (not (null tags))
		(mapc (lambda (tag)
				(puthash tag
						 (+ 1 (gethash tag tagging-tags-hash 0))
						 tagging-tags-hash))
			  (mapcar 'intern tags))))

;;*search conrad tagging-filter-cur jonathan todo:optimize
; Now that the fagging-filter-cur list is set as a proper alist, I bet this
; might be better optimized somehow.  
(defun tagging-search (tags)
  (let ((fails t))
    (while fails
		   (setq fails nil)
		   (if (re-search-forward tagging-tagline-indicator nil t)
			   (progn
				(beginning-of-line)
				(mapc (lambda (tag)
						(when (let ((x (re-search-forward (concat tagging-tagline-indicator
																  "\\(.* \\)?"
																  (car tag)
																  "\\( .*\\)?$")
														  (point-at-eol)
														  t)))
								(or (and (cdr tag) (not x)) (and (not (cdr tag)) x)))
							  (setq fails t))
						(beginning-of-line))
					  tags)
				(if fails
					(forward-line 1)))
			   (goto-char (point-max))))))

;;*search conrad overlay
(defun tagging-add-invisible-overlay (start end) 
  (let ((overlay (make-overlay start end))) 
    (setq tagging-invisible-areas (cons overlay tagging-invisible-areas)) 
    (overlay-put overlay 'invisible 'hl)))

;;*search conrad overlay
(defun tagging-make-visible () 
  (mapcar (lambda (overlay) (delete-overlay overlay))  
          tagging-invisible-areas) 
  (setq tagging-invisible-areas ()))

;;*search conrad tagging-filter-cur
(defun tagging-perform-filter (tags)
  (save-excursion
    (tagging-make-visible)
    (goto-char (point-min))
    (while (not (eq (point) (point-max)))
      (let ((x (point)))
	(tagging-search tags)
	(tagging-add-invisible-overlay x (point))
	(forward-line 1)
	(if (re-search-forward tagging-tagline-indicator nil t)
	    (beginning-of-line)
	    (goto-char (point-max)))))))

;;*conrad search
(defun tagging-show-all () 
  (interactive)
  (tagging-make-visible)
  (setq tagging-filter-cur nil))

(defalias 'tagging-filter-none 'tagging-show-all)

;;*conrad search filter
(defun tagging-filter-set (tags)
  (interactive (list (read-from-minibuffer "Tags: " (tagging-tags-to-string tagging-filter-cur))))
  (setq tagging-filter-cur (tagging-parse-tags tags))
  (tagging-perform-filter tagging-filter-cur)
  (message "Set tagging filter to %s" tagging-filter-cur))

;;*conrad search filter
(defun tagging-filter-add (tags)
  (interactive (list (read-from-minibuffer "Tags: " (thing-at-point 'word))))
  (setq tagging-filter-cur (append tagging-filter-cur (tagging-parse-tags tags)))
  (tagging-perform-filter tagging-filter-cur)
  (message "Adding tag %s to filter %s" (tagging-parse-tags tags) tagging-filter-cur))

;;*conrad search filter
(defun tagging-filter-subtract (tags)
  (interactive (list (read-from-minibuffer "Tags: " (thing-at-point 'word))))
  (mapc (lambda (x)
	  (setq tagging-filter-cur (delete x tagging-filter-cur)))
	(tagging-parse-tags tags))
  (tagging-perform-filter tagging-filter-cur)
  (message "Removing tag %s to filter %s" (tagging-parse-tags tags) tagging-filter-cur))

;;*conrad mode
(define-derived-mode tagging-mode fundamental-mode "Tagging"
    "Major mode for using del.icio.us- like tagging."
    (setq comment-start tagging-tagline-indicator)
    (local-set-key "\C-c\C-a" 'tagging-show-all)
    (local-set-key "\C-c\C-t" 'tagging-filter-set)
    (local-set-key "\C-c\C-s" 'tagging-filter-add)
    (local-set-key "\C-c\C-d" 'tagging-filter-subtract)
    (setq tagging-font-lock-keywords
          (list `(,(concat tagging-tagline-indicator ".*$")
                  . font-lock-keyword-face)))
    (font-lock-mode)
     (setq font-lock-keywords tagging-font-lock-keywords))

(add-to-list 'auto-mode-alist '(".tagged\\'" . tagging-mode))

;;*conrad mode
(define-minor-mode tagging-minor-mode
  "Toggle Tagging Minor mode."
  nil
  " Tagging"
  ;; The minor mode bindings.
  `(("\C-c\C-a" . tagging-show-all)
    ("\C-c\C-t" . tagging-filter-set)
    ("\C-c\C-s" . tagging-filter-add)
    ("\C-c\C-d" . tagging-filter-subtract)))

(provide 'tagging)
(provide 'free-tagging)
