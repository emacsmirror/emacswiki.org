;;; bs-ext.el --- Extensions to emacs buffer-selection library (bs.el)

;; Filename: bs-ext.el
;; Description: Extensions to emacs buffer-selection library (bs.el)
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-06-28 14:23:56
;; Version: 0.1
;; Last-Updated: 2012-06-28 14:23:56
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/bs-ext.el
;; Keywords: convenience files
;; Compatibility: GNU Emacs 24.0.50.2
;;
;; Features that might be required by this library:
;;
;; bs.el
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
;; Extensions to emacs buffer-selection library (bs.el)
;; This extension allows you to bind keys to buffer selection configurations (using `bs-ext-config-keys'),
;; and optionally displays the configuration names and associated keybindings in the header line of the
;; *buffer-selection* buffer.
;; It also creates a new config called "regexp". When the "/" key is pressed the user is prompted for a regular
;; expression and any buffers with matching names are added to the "regexp" config.
;;
;; The following new keybindings are defined:
;;
;; /        : prompt user for regular expression, place matching buffers in "regexp" config, and change to that config
;; <left>   : select previous config using `bs-ext-select-previous-configuration'
;; <right>  : select next config using `bs-select-next-configuration'
;; x        : kill buffer on current line using `bs-delete'

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
;; 

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

(defun bs-ext-prev-config-aux (start-name list)
  "Get the previous assoc before START-NAME in list LIST.
Will return the last if START-NAME is at start."
  (let ((assocs list)
	(length (length list))
	pos)
    (while (and assocs (not pos))
      (when (string= (car (car assocs)) start-name)
	(setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (if (eq pos 0)
	(nth (1- length) list)
      (nth (1- pos) list))))

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

(defvar bs-ext-regexp ".*"
  "Regexp with which to match buffer names for buffer show `regexp' configuration.")

(defvar bs-ext-regexp-history '()
  "History list for use in aleblanc/bs-set-regexp.")

(defun bs-ext-set-regexp (regexp)
  "Set the value of bs-ext-regexp - a regexp to match buffer names for the regexp configuration."
  (interactive (list (read-string "Regexp to match buffer names: " nil 'bs-ext-regexp-history)))
  (setq bs-ext-regexp regexp)
  (add-to-list 'bs-ext-regexp-history 'regexp))

(defvar bs-ext-regexp-config '("regexp" nil
                               (lambda (buf) (string-match bs-ext-regexp (buffer-name buf)))
                               nil
                               (lambda (buf) (not (string-match bs-ext-regexp (buffer-name buf))))
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
;    mode-line-modes
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
  "The mode-line-format for the *buffer-selection* buffer.")

;; Set some new keys
(define-key bs-mode-map (kbd "<left>") 'bs-ext-select-previous-configuration)
(define-key bs-mode-map (kbd "<right>") 'bs-select-next-configuration)
(define-key bs-mode-map (kbd "x") 'bs-delete)
(define-key bs-mode-map (kbd "/") (lambda nil (interactive)
                                    (call-interactively 'bs-ext-set-regexp)
                                    (if (not (assoc "regexp" bs-configurations))
                                             (add-to-list 'bs-configurations bs-ext-regexp-config))
                                    (bs--show-with-configuration "regexp")))
;; Set the config keys
(bs-ext-set-keys 'bs-ext-config-keys bs-ext-config-keys)


(defcustom bs-ext-show-configs-header t
  "Whether or not to show the configs header line."
  :type 'boolean
  :group 'bs)

;; Set the mode-line
(add-hook 'bs-mode-hook
          (lambda nil
            (setq mode-line-format bs-ext-mode-line-format)
            (setq header-line-format (if bs-ext-show-configs-header
                                         (mapconcat (lambda (conf)
                                                      (let* ((name (car conf))
                                                             (key (car (rassoc name bs-ext-config-keys)))
                                                             (item (if key (concat name "(" key ")") name)))
                                                        (if (equal name bs-current-configuration)
                                                            (propertize item 'face font-lock-comment-face) 
                                                          item)))
                                                    bs-configurations " ")))))


(provide 'bs-ext)

;;; bs-ext.el ends here

