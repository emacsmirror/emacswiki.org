;;; help-macro+.el --- Extensions to `help-macro.el'.
;;
;; Filename: help-macro+.el
;; Description: Extensions to `help-macro.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Tue Aug 24 15:36:18 1999
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Feb 23 07:34:41 2017 (-0800)
;;           By: dradams
;;     Update #: 126
;; URL: https://www.emacswiki.org/emacs/download/help-macro%2b.el
;; Doc URL: http://emacswiki.org/HelpPlus
;; Keywords: help
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `backquote', `help-macro', `naked'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help-macro.el'.
;;
;;
;;  ***** NOTE: The following macro defined in `help-macro.el' has
;;              been REDEFINED HERE:
;;
;;  `make-help-screen'
;;
;;
;;-> ***********************  Example of use *********************************
;;->
;;->(make-help-screen help-for-empire-redistribute-map
;;->              "c:civ m:mil p:population f:food ?"
;;->              "You have discovered the GEET redistribution commands
;;->   From here, you can use the following options:
;;->
;;->c   Redistribute civs from overfull sectors into connected underfull ones
;;->      The functions typically named by empire-ideal-civ-fcn control
;;->          based in part on empire-sector-civ-threshold
;;->m   Redistribute military using levels given by empire-ideal-mil-fcn
;;->p   Redistribute excess population to highways for max pop growth
;;->      Excess is any sector so full babies will not be born.
;;->f   Even out food on highways to highway min and leave levels
;;->      This is good to pump max food to all warehouses/dist pts
;;->
;;->
;;->Use \\[help-for-empire-redistribute-map] for help on redistribution.
;;->Use \\[help-for-empire-extract-map] for help on data extraction.
;;->Please use \\[describe-key] to find out more about any of the other keys."
;;->              empire-shell-redistribute-map)
;;->
;;->  (define-key c-mp "\C-h" 'help-for-empire-redistribute-map)
;;->  (define-key c-mp help-character 'help-for-empire-redistribute-map)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/10/07 dadams
;;     Added soft require of naked.el.
;;     make-help-screen: Use naked-key-description if available.
;; 2011/01/04 dadams
;;     Added autoload cookie for defmacro.
;; 2007/12/14 dadams
;;     make-help-screen:
;;       Don't delete the frame if there are unread-command-events (mouse click).
;;     Removed require of cl.el at compile time for Emacs 19.
;; 2007/12/13 dadams
;;     make-help-screen:
;;       Updated wrt Emacs 22: bind inhibit-read-only around inserting help-screen.
;;       Delete frame at end (don't iconify).
;; 2007/12/09 dadams
;;     make-help-screen: fit-frame if one-window-p.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'help-macro)
(require 'backquote)

(require 'naked nil t) ;; (no error if not found): naked-key-description

;;;;;;;;;;;;;;;;;;;;


;; REPLACES ORIGINAL in `help-macro.el':
;; Fits frame if `one-window-p'.
;; Does not iconify *Help* frame.
;;
;;;###autoload
(defmacro make-help-screen (fname help-line help-text helped-map)
  "Construct help-menu function name FNAME.
When invoked, FNAME shows HELP-LINE and reads a command using HELPED-MAP.
If the command is the help character, FNAME displays HELP-TEXT
and continues trying to read a command using HELPED-MAP.
If HELP-TEXT contains the sequence `%THIS-KEY%', that is replaced
with the key sequence that invoked FNAME.
When FNAME finally does get a command, it executes that command
and then returns."
  (let ((doc-fn (intern (concat (symbol-name fname) "-doc"))))
    `(progn
       (defun ,doc-fn () ,help-text)
       (defun ,fname ()
         "Help command."
         (interactive)
         (let ((line-prompt
                (substitute-command-keys ,help-line)))
           (if three-step-help
               (message "%s" line-prompt))
           (let* ((help-screen (documentation (quote ,doc-fn)))
                  ;; We bind overriding-local-map for very small
                  ;; sections, *excluding* where we switch buffers
                  ;; and where we execute the chosen help command.
                  (local-map (make-sparse-keymap))
                  (minor-mode-map-alist nil)
                  (prev-frame (selected-frame))
                  config new-frame key char)
             (if (string-match "%THIS-KEY%" help-screen)
                 (setq help-screen
                       (replace-match
                        (if (fboundp 'naked-key-description)
                            (naked-key-description (substring (this-command-keys) 0 -1))
                          (key-description (substring (this-command-keys) 0 -1)))
                        t t help-screen)))
             (unwind-protect
                 (progn
                   (setcdr local-map ,helped-map)
                   (define-key local-map [t] 'undefined)
                   ;; Make the scroll bar keep working normally.
                   (define-key local-map [vertical-scroll-bar]
                     (lookup-key global-map [vertical-scroll-bar]))
                   (if three-step-help
                       (progn
                         (setq key (let ((overriding-local-map local-map))
                                     (read-key-sequence nil)))
                         ;; Make the HELP key translate to C-h.
                         (if (lookup-key function-key-map key)
                             (setq key (lookup-key function-key-map key)))
                         (setq char (aref key 0)))
                     (setq char ??))
                   (if (or (eq char ??) (eq char help-char)
                           (memq char help-event-list))
                       (progn
                         (setq config (current-window-configuration))
                         (switch-to-buffer-other-window "*Help*")
                         (and (fboundp 'make-frame)
                              (not (eq (window-frame (selected-window)) prev-frame))
                              (setq new-frame (window-frame (selected-window))
                                    config    nil))
                         (setq buffer-read-only nil)
                         (let ((inhibit-read-only t))
                           (erase-buffer)
                           (insert help-screen))
                         (help-mode)
                         (when (and (fboundp 'fit-frame) (one-window-p t)) (fit-frame))
                         (goto-char (point-min))
                         (while (or (memq char (append help-event-list
                                                       (cons help-char
                                                             '(?? ?\C-v ?\ ?\177
                                                               delete backspace
                                                               vertical-scroll-bar
                                                               ?\M-v))))
                                    (eq (car-safe char) 'switch-frame)
                                    (equal key "\M-v"))
                           (condition-case nil
                               (progn
                                 (if (eq (car-safe char) 'switch-frame)
                                     (handle-switch-frame char))
                                 (if (memq char '(?\C-v ?\ ))
                                     (scroll-up))
                                 (if (or (memq char '(?\177 ?\M-v delete backspace))
                                         (equal key "\M-v"))
                                     (scroll-down)))
                             (error nil))
                           (let ((cursor-in-echo-area t)
                                 (overriding-local-map local-map))
                             (setq key (read-key-sequence
                                        (format "Type one of the options listed%s: "
                                                (if (pos-visible-in-window-p (point-max))
                                                    "" ", or SPACE or DEL to scroll")))
                                   char (aref key 0)))
                   ;; If this is a scroll bar command, just run it.
                   (when (eq char 'vertical-scroll-bar)
                     (command-execute (lookup-key local-map key) nil key)))))
           ;; We don't need the prompt any more.
           (message "")
           ;; Mouse clicks are not part of the help feature,
           ;; so reexecute them in the standard environment.
           (if (listp char)
               (setq unread-command-events (cons char unread-command-events)
                     config                nil)
             (let ((defn (lookup-key local-map key)))
               (if (not defn)
                   (ding)
                 (when config
                   (set-window-configuration config)
                   (setq config nil))
                 (when new-frame (delete-frame new-frame))
                 (call-interactively defn)))))
               (unless unread-command-events
                 (when new-frame (delete-frame new-frame)))
               (when config (set-window-configuration config)))))))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help-macro+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help-macro+.el ends here


