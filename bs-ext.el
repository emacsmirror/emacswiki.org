;;; bs-ext.el --- Extensions to emacs buffer-selection library (bs.el) TEST2

;; Filename: bs-ext.el
;; Description: Extensions to emacs buffer-selection library (bs.el)
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-06-28 14:23:56
;; Version: 0.2
;; Last-Updated: 2012-06-28 14:23:56
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/bs-ext.el
;; Keywords: convenience files
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; bs.el, color-moccur.el
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; Bitcoin donations gratefully accepted: 13NyoPq3iusGsCtHNRT9xfA9jsqPjYtyyE
;;
;; Extensions to emacs buffer-selection library (bs.el)
;; This extension allows you to bind keys to buffer selection configurations (using `bs-ext-config-keys'),
;; and optionally displays the configuration names and associated keybindings in the header line of the
;; *buffer-selection* buffer.
;; It also creates a new config called "regexp". When the "/" key is pressed the user is prompted first for
;; a field to filter on, and then for a regular expression or number to use as the filter. Any buffers in the
;; currently displayed config that match the filter on the specified field will be placed in the "regexp" config
;; which will then be displayed. If the user decides to filter on the "Size" field then a number must be entered,
;; and all buffers with size greater than that number will be selected. If a prefix arg is used, all buffers with
;; size smaller than the number will be selected. You can apply successive filters to narrow down the list.
;;
;; The following new keybindings are defined:
;;
;; /        : prompt user for a field and regexp/value to filter buffers with, placing matching buffers in "regexp" config
;; <left>   : select previous config using `bs-ext-select-previous-configuration'
;; <right>  : select next config using `bs-ext-select-next-configuration'
;; x        : kill marked buffers or buffer on current line using `bs-delete'
;; M        : mark all buffers 
;; U        : unmark all buffers
;; =        : toggle status of showing flag for buffer in current line
;;
;; Also if you have color-moccur installed you can use M-O to find regexp matches in marked buffers.


;;; Installation:
;;
;; Put bs-ext.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'bs-ext)

;;; Customize:
;;
;;  bs-ext-show-configs-header : whether or not to show the configs header line
;;  bs-ext-config-keys : alist of keybindings and associated config names  
;;
;; All of the above can customized by:
;;      M-x customize-group RET bs RET
;;

;;; Change log:
;;	
;; 2012/06/28
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;; 
;; Exclude empty groups? Allow manually adding buffers to a group.
;; Create "fast" group that rebinds up/down arrow keys so that the buffers are show in the other window automatically?

;;; Require
(require 'bs)

;;; Code:

(defun bs-ext-set-keys (symb val)
  "Set the key-bindings for the different configurations.
This function is used for setting the keys after saving the customization buffer for `bs-ext-config-keys'.
If called in other code then SYMB should be 'bs-ext-config-keys and val should be bs-ext-config-keys."
  (loop for (key . name) in val
        do (define-key bs-mode-map (read-kbd-macro key)
             `(lambda nil (interactive) (bs--show-with-configuration ,name))))
  (set-default symb val))

(defcustom bs-ext-config-keys nil
  "Alist of (KEY . CONFIG) pairs.
CONFIG is the name of a configuration listed in `bs-configurations', and KEY is a key that loads that config when pressed
in the *buffer-selection* buffer."
  :type '(alist :key-type (string :tag "Key"
                                  :help-echo (lambda (w) (concat "The key to press to load this configuration. Must not be one of the following (already used) keys:\n"
                                                                 (mapconcat 'identity (loop for key being the key-codes of bs-mode-map
                                                                                            collect (single-key-description key)) " ")))
                                  :match (lambda (w key)
                                           (or (member key (mapcar 'car bs-ext-config-keys))
                                               (not (member key (loop for key being the key-codes of bs-mode-map
                                                                      collect (single-key-description key)))))))
                :value-type (string :tag "Config name"
                                    :help-echo (lambda (w)
                                                 (concat "The name of the configuration to load. Must be one of the following:\n"
                                                         (mapconcat 'car bs-configurations " ")))
                                    :match (lambda (w config) (member config (mapcar 'car bs-configurations)))))
  :set 'bs-ext-set-keys
  :group 'bs)

(defun bs-ext-prev-config-aux (start-name lst)
  "Get the previous assoc before START-NAME in list LST.
Will return the last if START-NAME is at start."
  (let ((assocs lst)
	(l (length lst))
	pos)
    (while (and assocs (not pos))
      (when (string= (caar assocs) start-name)
	(setq pos (- l (length assocs))))
      (setq assocs (cdr assocs)))
    (if (eq pos 0)
	(nth (1- l) lst)
      (nth (1- pos) lst))))

(defun bs-ext-prev-config (name)
  "Return previous configuration with respect to configuration with name NAME."
  (bs-ext-prev-config-aux name bs-configurations))

(defun bs-ext-select-previous-configuration (&optional start-name)
  "Apply previous configuration to START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let ((config (bs-ext-prev-config (or start-name bs-current-configuration))))
    (bs-set-configuration (car config))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log "Selected configuration: %s" (car config))))

(defun bs-ext-select-next-configuration (&optional start-name)
  "Apply next configuration START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let ((config (bs-next-config (or start-name bs-current-configuration))))
    (bs-set-configuration (car config))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log "Selected configuration: %s" (car config))))

(defvar bs-ext-regexp ".*"
  "Regexp with which to match buffer names for buffer show `regexp' configuration.")

(defvar bs-ext-regexp-history '()
  "History list for use in aleblanc/bs-set-regexp.")

(defvar bs-ext-regexp-field 'name
  "What field `bs-ext-regexp' will be matched on.")

(defun bs-ext-set-regexp (field regexp)
  "Set the value of bs-ext-regexp - a regexp to match buffer names for the regexp configuration."
  (interactive (let* ((f (read-key "Press key to indicate filter type: b/n = buffer name, f = filename, s = size, m = mode, > = marked, % = readonly, * = modified"))
		      (r (if (not (memq f '(?> ?% ?*))) 
			     (read-string "Regexp/value: " nil 'bs-ext-regexp-history))))
		 (list f r)))
  (setq bs-ext-regexp-field field bs-ext-regexp regexp)
  (add-to-list 'bs-ext-regexp-history 'regexp))

(defun bs-ext-regexp-filter (buf)
  (let* ((func (case bs-ext-regexp-field
                 (?m 'bs--get-mode-name)
                 (?f 'bs--get-file-name)
                 (?s 'bs--get-size-string)
		 (?> (lambda (buf lst)
		       (member buf bs--marked-buffers)))
		 (?% (lambda (buf lst) buffer-read-only))
		 (?* (lambda (buf lst) (buffer-modified-p)))
                 (t 'bs--get-name)))
         (value (with-current-buffer buf
                  (and (member buf bs-current-list)
                       (funcall func buf bs-current-list)))))
    (if (stringp value)
	(if (eql bs-ext-regexp-field 115)
	    (funcall (if current-prefix-arg '< '>)
		     (string-to-number value)
		     (string-to-number bs-ext-regexp))
	  (string-match bs-ext-regexp value))
      value)))

(defvar bs-ext-regexp-config '("regexp" nil
                               bs-ext-regexp-filter
                               nil
                               (lambda (buf) (not (bs-ext-regexp-filter buf)))
                               nil)
  "Buffer selection configuration for matching buffer names by regular expressions.")

(defvar bs-ext-mode-line-format
  '("%e" "%e"
    #("-" 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
    #("   " 0 3
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    mode-line-position
    #("  " 0 2
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    "Press ? for help, q to quit"
    (global-mode-string
     ("" global-mode-string
      #(" " 0 1
        (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
    (:eval
     (unless
         (display-graphic-p)
       #("-%-" 0 3
         (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")))))
  "The `mode-line-format' for the *buffer-selection* buffer.")

(defun bs-ext-limit-by-regexp nil
  (interactive)
  (call-interactively 'bs-ext-set-regexp)
  (if (not (assoc "regexp" bs-configurations))
      (add-to-list 'bs-configurations bs-ext-regexp-config))
  (bs--show-with-configuration "regexp"))

;; Set some new keys
(define-key bs-mode-map (kbd "<left>") 'bs-ext-select-previous-configuration)
(define-key bs-mode-map (kbd "<right>") 'bs-ext-select-next-configuration)
(define-key bs-mode-map (kbd "x") 'bs-ext-delete)
(define-key bs-mode-map (kbd "/") 'bs-ext-limit-by-regexp)
(define-key bs-mode-map (kbd "R") 'bs-ext-rename)
(define-key bs-mode-map (kbd "U") 'bs-ext-unmark-all)
(define-key bs-mode-map (kbd "M") 'bs-ext-mark-all)
(define-key bs-mode-map (kbd "+") 'bs-toggle-current-to-show)
(define-key bs-mode-map (kbd "=") 'bs-toggle-current-to-show)
(define-key bs-mode-map (kbd "W") 'bs-ext-write)
(define-key bs-mode-map (kbd ":") 'bs-ext-apply-function)
(define-key bs-mode-map (kbd "?") 'bs-ext-help)
(define-key bs-mode-map (kbd "<") 'beginning-of-buffer)
(define-key bs-mode-map (kbd ">") 'end-of-buffer)
(define-key bs-mode-map (kbd "C-x C-s") 'bs-ext-save)
(if (featurep 'color-moccur)
    (define-key bs-mode-map (kbd "M-O") 'bs-ext-moccur-marked-buffers))
;; Set the config keys
(bs-ext-set-keys 'bs-ext-config-keys bs-ext-config-keys)

(defcustom bs-ext-show-configs-header t
  "Whether or not to show the configs header line."
  :type 'boolean
  :group 'bs)

(defun bs-ext-mode-line nil
  "Set the `mode-line-format' for `bs-mode'."
  (setq mode-line-format bs-ext-mode-line-format
	header-line-format (if bs-ext-show-configs-header
			       (mapconcat (lambda (conf)
					    (let* ((name (car conf))
						   (key (car (rassoc name bs-ext-config-keys)))
						   (item (if key (concat name "(" key ")")
							   (if (equal name "regexp") "regexp(/)"
							     name))))
					      (if (equal name bs-current-configuration)
						  (propertize item 'face font-lock-comment-face)
						item)))
					  bs-configurations " "))))

;; Set the mode-line
(add-hook 'bs-mode-hook 'bs-ext-mode-line)

;; This variable is used purely for the documentation string.
(defvar bs-ext-help nil
  "Major mode for editing a subset of Emacs' buffers.
\\<bs-mode-map>
Aside from two header lines each line describes one buffer.
Move to a line representing the buffer you want to edit and select
buffer by \\[bs-select] or SPC.  Abort buffer list with \\[bs-kill].
There are many key commands similar to `Buffer-menu-mode' for
manipulating the buffer list and buffers.
For faster navigation each digit key is a digit argument.
Different configurations are displayed in the header line. These can
be navigated using \\[bs-ext-select-previous-configuration] and \\[bs-ext-select-next-configuration],
or by pressing the associated shortcut keys displayed in brackets after
the config names (see `bs-ext-config-keys').

\\[bs-select] or SPACE -- select current line's buffer and other marked buffers.
\\[bs-toggle-show-all]  -- toggle between all buffers and a special subset.
\\[bs-select-other-window] -- select current line's buffer in other window.
\\[bs-tmp-select-other-window] -- make another window display that buffer and
    remain in Buffer Selection Menu.
\\[bs-mouse-select] -- select current line's buffer and other marked buffers.
\\[bs-save] -- save current line's buffer immediately.
\\[bs-delete] -- kill current line's buffer immediately.
\\[bs-toggle-readonly] -- toggle read-only status of current line's buffer.
\\[bs-clear-modified] -- clear modified-flag on that buffer.
\\[bs-mark-current] -- mark current line's buffer to be displayed.
\\[bs-unmark-current] -- unmark current line's buffer to be displayed.
\\[bs-show-sorted] -- display buffer list sorted by next sort aspect.
\\[bs-set-configuration-and-refresh] -- ask user for a configuration and \
apply selected configuration.
\\[bs-ext-select-next-configuration] -- select and apply next \
available Buffer Selection Menu configuration.
\\[bs-ext-select-previous-configuration] -- select and apply previous \
available Buffer Selection Menu configuration.
\\[bs-ext-limit-by-regexp] -- display buffers with names matching regexp (prompt).
\\[bs-kill] -- leave Buffer Selection Menu without a selection.
\\[bs-toggle-current-to-show] -- toggle status of appearance.
\\[bs-set-current-buffer-to-show-always] -- mark current line's buffer \
to show always.
\\[bs-visit-tags-table] -- call `visit-tags-table' on current line's buffer.
\\[bs-ext-moccur-marked-buffers] -- run multi-occur/moccur on marked buffers
\\[bs-help] -- display this help text.")

;; redefine 'bs-help so that it includes information about new keybindings
(defun bs-ext-help ()
  "Help for `bs-show'."
  (interactive)
  (describe-variable 'bs-ext-help))

(defun bs-ext-moccur-marked-buffers (regexp buffers)
  "Apply `moccur' on marked buffers."
  (interactive (list (read-regexp "List lines matching regexp") bs--marked-buffers))
  (if (featurep 'color-moccur)
      (progn
        (moccur-kill-buffer t)
        (moccur-search regexp t bs--marked-buffers))
    (multi-occur buffers regexp)
    (bs-kill)
    (switch-to-buffer "*Occur*")))

(defun bs-ext-unmark-all nil
  "Unmark all marked buffers."
  (interactive)
  (setq bs--marked-buffers nil)
  (bs--redisplay t))

(defun bs-ext-mark-all nil
  "Mark all unmarked buffers."
  (interactive)
  (setq bs--marked-buffers bs-current-list)
  (bs--redisplay t))

(defun bs-ext-apply-function (fn)
  "Apply function FN to marked buffers or buffer on current line.
The function FN will be called with `funcall' within each buffer.
This can be used for changing the `major-mode' of buffers for example."
  (interactive (list (read-minibuffer "Function: ")))
  (let ((current (bs--current-buffer))
	(inhibit-read-only t))
    (if (y-or-n-p (format "Apply \"%S\" to the marked buffers? " fn))
	(if bs--marked-buffers
	    (dolist (buf bs--marked-buffers)
	      (with-current-buffer buf (funcall fn)))
	  (with-current-buffer current (funcall fn))))
    (bs--redisplay t)))

(defun bs-ext-rename (regexp newname &optional literal)
  "Rename marked buffers or buffer on current line.
Name of buffer is matched with REGEXP and renamed to NEWNAME which
may refer to parenthesised subexpressions in REGEXP (see `replace-match').
If REGEXP is left as an empty string then it will match the whole buffer
name with no subexpressions.
If a prefix argument is used or LITERAL is non-nil and there is only 1 buffer
to be renamed, then NEWNAME will be treated literally and no substitution of
subexpressions will take place."
  (interactive (list (read-regexp "regexp matching old name (default .*)")
		     (read-regexp (if (and current-prefix-arg
					   (< (length bs--marked-buffers) 2)) "New name"
				    "New name (\\& = whole match, \\N = Nth subexpression)"))
		     current-prefix-arg))
  (bs-ext-apply-function
   (lambda nil
     (let ((bufname (buffer-name)))
       (string-match regexp bufname)
       (rename-buffer
	(match-substitute-replacement newname nil
				      (and literal (< (length bs--marked-buffers) 2))
				      bufname))))))

(defun bs-ext-write (regexp filename)
  "Write buffer on current line or marked buffers to files.
Name of buffer is matched with REGEXP and written to FILENAME which
may refer to parenthesised subexpressions in REGEXP (see `replace-match').
If REGEXP is left as an empty string then it will match the whole buffer
name with no subexpressions."
  (interactive (list (read-regexp "regexp matching old name (default .*)")
		     (read-regexp (if (and current-prefix-arg
					   (< (length bs--marked-buffers) 2)) "Filename"
				    "Filename (\\& = whole match, \\N = Nth subexpression)"))))
  (bs-ext-apply-function
   (lambda nil
     (let ((bufname (buffer-name)))
       (string-match regexp bufname)
       (write-file
	(match-substitute-replacement filename nil nil bufname))))))

(defun bs-ext-delete ()
  "Kill marked buffers or buffer on current line."
  (interactive)
  (if (not bs--marked-buffers)
      (bs-delete)
    (unless (not (y-or-n-p
		  (format "Delete %d marked buffers? "
			  (length bs--marked-buffers))))
      (goto-char (point-min))
      (while (re-search-forward "^>" nil t)
	(bs-delete))
      (setq bs--marked-buffers nil))))

(defun bs-ext-save ()
  "Save marked buffers or buffer on current line."
  (interactive)
  (bs-ext-apply-function 'save-buffer))

(provide 'bs-ext)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "bs-ext.el" (buffer-name) (buffer-string) "update")

;;; bs-ext.el ends here

