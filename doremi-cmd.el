;;; doremi-cmd.el --- Miscellaneous Do Re Mi commands
;;
;; Filename: doremi-cmd.el
;; Description: Miscellaneous Do Re Mi commands
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2004-2009, Drew Adams, all rights reserved.
;; Created: Sun Sep 12 17:13:58 2004
;; Version: 21.0
;; Last-Updated: Sun Nov 22 10:34:47 2009 (-0800)
;;           By: dradams
;;     Update #: 319
;; URL: http://www.emacswiki.org/cgi-bin/wiki/doremi-cmd.el
;; Keywords: keys, cycle, repeat
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `color-theme', `cus-face', `doremi', `easymenu', `mwheel',
;;   `reporter', `ring', `ring+', `sendmail', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Miscellaneous Do Re Mi commands.
;;
;;  During Do Re Mi commands, you can press and hold an up/down arrow
;;  key, or rotate the mouse wheel, to change face attributes or frame
;;  parameters.  For more info, see file `doremi.el' and the
;;  doc-string for function `doremi' in particular.
;;
;;  NOTE: Functions and variables in this library have the prefix
;;        `doremi-'.  In order to more easily distinguish commands
;;        that iterate in Do Re Mi fashion from other functions in the
;;        library, the iterative commands are suffixed with `+'.
;;
;;  If you also use library `crosshairs.el' (which requires libraries
;;  `hl-line.el', `hl-line+.el', `vline.el', and `col-highlight.el'),
;;  then commands `doremi-marks+' and `doremi-global-marks+' use
;;  crosshairs to highlight the mark positions when you visit them.
;;
;;  Note on saving changes made with the commands defined here:
;;
;;    Some of the commands defined here change face and frame
;;    properties. You can save any changes you have made, by using
;;    Customize. To visit a Customize buffer of all unsaved changes
;;    you have made, use command `customize-customized'.
;;
;;    Frame parameter changes, such as background color, can be saved
;;    for future use by all frames or all frames of a certain
;;    kind. For that, you must change the frame parameters of the
;;    correponding frame-alist variable.
;;
;;    There is no single variable for saving changes to parameters of
;;    the current frame. Instead, there are several different
;;    frame-alist variables, which you can use to define different
;;    kinds of frames. These include: `default-frame-alist',
;;    `initial-frame-alist', and `special-display-frame-alist'. The
;;    complete list of such frame alist variables is available using
;;    function `frame-alist-var-names', defined in library
;;    `frame-cmds.el'.
;;
;;    Example: Suppose you change the background color of a frame and
;;    want to make that the default background color for new frames in
;;    the future. You will need to update the value of variable
;;    `default-frame-alist' to use the `background-color' parameter
;;    setting of the changed frame.
;;
;;    You can easily copy one or all parameter values from any given
;;    frame to any frame alist (such as `default-frame-alist'), by
;;    using the commands `set-frame-alist-parameter-from-frame' and
;;    `set-all-frame-alist-parameters-from-frame'. Those commands are
;;    defined in library `frame-cmds.el'.
;;
;;
;;  User option defined here:
;;
;;    `doremi-color-themes'.
;;
;;  Commands defined here:
;;
;;    `doremi-bookmarks+', `doremi-buffers+', `doremi-color-themes+',
;;    `doremi-global-marks+', `doremi-marks+',
;;    `doremi-window-height+', `doremi-window-width+'.
;;
;;  Non-interactive functions defined here:
;;
;;    `doremi-buffers-1', `doremi-color-themes-1',
;;    `doremi-global-marks-1', `doremi-marks-1'.
;;
;;
;;  Add this to your initialization file (~/.emacs or ~/_emacs):
;;
;;    (require 'doremi-cmd)
;;
;;
;;  See also these related Do Re Mi libraries:
;;
;;    `doremi-frm.el' - Do Re Mi commands to adjust frame properties.
;;
;;    `doremi-mac.el' - Macro to define Do Re Mi commands and
;;                      automatically add them to a Do Re Mi menu.
;;
;;
;;  Suggested bindings:
;;
;;   (defalias 'doremi-prefix (make-sparse-keymap))
;;   (defvar doremi-map (symbol-function 'doremi-prefix)
;;     "Keymap for Do Re Mi commands.")
;;   (define-key global-map "\C-xt" 'doremi-prefix)
;;   (define-key doremi-map "b" 'doremi-buffers+)
;;   (define-key doremi-map "g" 'doremi-global-marks+)
;;   (define-key doremi-map "m" 'doremi-marks+)
;;   (define-key doremi-map "r" 'doremi-bookmarks+) ; reading books?
;;   (define-key doremi-map "s" 'doremi-color-themes+) ; color schemes
;;   (define-key doremi-map "w" 'doremi-window-height+)
;;
;;  Customize the menu. Uncomment this to try it out.
;;
;;   (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
;;   (define-key global-map [menu-bar doremi]
;;     (cons "Do Re Mi" menu-bar-doremi-menu))
;;   (define-key menu-bar-doremi-menu [doremi-color-themes]
;;     '(menu-item "Color Themes" . doremi-color-themes+
;;       :help "Successively cycle among color themes: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-global-marks]
;;     '(menu-item "Global Marks" . doremi-global-marks+
;;       :help "Successively cycle among global marks: `up'/`down'")))
;;   (define-key menu-bar-doremi-menu [doremi-marks]
;;     '(menu-item "Marks in Buffer" . doremi-marks+
;;       :help "Successively cycle among marks in this buffer: `up'/`down'")))
;;   (define-key menu-bar-doremi-menu [doremi-bookmarks]
;;     '(menu-item "Bookmarks" . doremi-bookmarks+
;;       :help "Successively cycle among bookmarks: `up'/`down'")))
;;   (define-key menu-bar-doremi-menu [doremi-buffers]
;;     '(menu-item "Buffers" . doremi-buffers+
;;       :help "Successively cycle among buffers: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-window-height]
;;     '(menu-item "Window Size" doremi-window-height+
;;       :help "Resize window incrementally: `up'/`down'/`left'/`right'"
;;       :enable (not (one-window-p)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2009/11/22 dadams
;;     Use color-theme-initialize instead of load-library, to load themes.
;; 2009/11/21 dadams
;;     Added: doremi-color-themes-1, doremi-buffers-1, doremi(-global)-marks-1.
;;     doremi-(color-themes|buffers|(-global)marks)+: Let C-g restore .  Use *-1.
;;     doremi-color-themes: Load color-theme-library.el also, for version 6.6.0+.
;;     doremi(-global)-marks(+|-1): Highlight position with crosshairs if we can.
;; 2009/11/07 dadams
;;     Renamed all Do Re Mi iterative commands by appending +.
;; 2009/06/26 dadams
;;     doremi-window-width: Use new key-list options, doremi-...-keys (not -key).
;; 2007/11/01 dadams
;;     Added: doremi-window-height, doremi-window-width.
;; 2007/10/08 dadams
;;     Use lowercase for defgroup groups.
;; 2006/01/07 dadams
;;     Added :link for sending bug report.
;; 2006/01/06 dadams
;;     Renamed group.  Added :link.
;; 2005/07/26 dadams
;;     Added :prefix to defgroup.
;;     Variable doremi-color-themes: Soft require of color-theme.
;; 2005/01/18 dadams
;;     Added Note on saving changes.
;; 2005/01/02 dadams
;;     Added: doremi-marks, doremi-global-marks.
;; 2005/01/01 dadams
;;     defvar -> defcustom. Added (defgroup doremi-cmd).
;;     Removed vestigial require of doremi-mac.el.
;; 2004/12/30 dadams
;;     doremi-color-themes (var): Use global color-themes list.
;; 2004/09/26 dadams
;;     Renamed do-re-mi* to doremi*.
;;     Prefixed everything here with doremi-.
;; 2004/09/19 dadams
;;     Added doremi-buffers.
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

(require 'doremi) ;; doremi

;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User Options (Variables)

(defgroup doremi-misc-commands nil
  "Miscellaneous Do Re Mi commands."
  :prefix "doremi-" :group 'doremi :group 'color-theme
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
doremi-cmd.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/doremi-cmd.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/cgi-bin/wiki/Doremi")
  :link '(emacs-commentary-link :tag "Commentary" "doremi-cmd")
  )

;; Replace this by your favorite color themes. Each must be a defined function.
;; By default, this includes all color themes defined globally (`color-themes').
;;
;; NOTE: We need the `condition-case' because of a BUG in `directory-files' for
;; Emacs 20.  Bug reported to `color-theme.el' maintainer 2009-11-22.  The problem
;; is that the default value of `color-theme-libraries' concats
;; `file-name-directory', which ends in `/', with `/themes', not with `themes'.
;; So the result is `...//themes'.  That is tolerated by Emacs 21+
;; `directory-files', but not for Emacs 20.  Until this `color-theme.el' bug is
;; fixed, Emacs 20 users will need to manually load `color-theme-libraries.el'.
;;;###autoload
(defcustom doremi-color-themes (and (prog1 (require 'color-theme nil t)
                                      (when (and (fboundp 'color-theme-initialize)
                                                 (not color-theme-initialized))
                                        (condition-case nil
                                            (let ((color-theme-load-all-themes  t))
                                              (color-theme-initialize)
                                              (setq color-theme-initialized  t))
                                          (error nil))))
                                    (delq 'bury-buffer (mapcar 'car color-themes)))
  "*List of color themes to cycle through using `doremi-color-themes+'."
  :type 'hook :group 'doremi-misc-commands)
 
;;; Commands (Interactive Functions)

;;;###autoload
(defun doremi-color-themes+ ()
  "Successively cycle among color themes.
The themes used for cycling are those in option `doremi-color-themes'.

You can use `C-g' to quit and cancel changes made so far.
Alternatively, after using `doremi-color-themes+' you can use
`color-theme-select' and choose pseudo-theme `[Reset]' - that does the
same thing.  Note that in either case, some things might not be
restored."
  (interactive)
  (unless (require 'color-theme nil t)
    (error "This command requires library `color-theme.el'"))
  ;; Create the snapshot, if not available.  Do this so users can undo using
  ;; pseudo-theme `[Reset]'.
  (when (or (not (assq 'color-theme-snapshot color-themes))
            (not (commandp 'color-theme-snapshot)))
    (fset 'color-theme-snapshot (color-theme-make-snapshot))
    (setq color-themes  (delq (assq 'color-theme-snapshot color-themes)
                              color-themes)
          color-themes  (delq (assq 'bury-buffer color-themes) color-themes)
          color-themes  (append '((color-theme-snapshot
                                   "[Reset]" "Undo changes, if possible.")
                                  (bury-buffer "[Quit]" "Bury this buffer."))
                                color-themes)))
  (let ((snapshot  (if (or (assq 'color-theme-snapshot color-themes)
                           (commandp 'color-theme-snapshot))
                       (symbol-function 'color-theme-snapshot)
                     (color-theme-make-snapshot))))
    (condition-case nil
        (doremi-color-themes-1)
      (quit (funcall snapshot)))))

(defun doremi-color-themes-1 ()
  "Helper function for `doremi-color-themes+'."
  (doremi (lambda (newval) (funcall newval) newval) ; update fn - just call theme
          (car (last doremi-color-themes)) ; start with last theme
          nil                           ; ignored
          nil                           ; ignored
          doremi-color-themes))         ; themes to cycle through

;;;###autoload
(defun doremi-bookmarks+ ()
  "Successively cycle among all bookmarks."
  (interactive)
  (doremi (lambda (newval) (bookmark-jump newval) newval)
          (or (and (boundp 'bookmark-current-bookmark)
                   bookmark-current-bookmark)
              (bookmark-buffer-name))
          nil                           ; ignored
          nil                           ; ignored
          (bookmark-all-names)
          t))

;;;###autoload
(defun doremi-buffers+ ()
  "Successively cycle among all existing buffers.
You can use `C-g' to quit and return to the original buffer."
  (interactive)
  (let ((curr-buff  (current-buffer)))
    (condition-case nil
        (doremi-buffers-1)
      (quit (switch-to-buffer curr-buff)))))

(defun doremi-buffers-1 ()
  "Helper-function for `doremi-buffers+'."
  (doremi (lambda (newval) (switch-to-buffer newval 'norecord) newval)
          (current-buffer)
          nil                           ; ignored
          nil                           ; ignored
          (buffer-list)))

;;;###autoload
(defun doremi-marks+ ()
  "Successively cycle among all marks in the `mark-ring'.
You can use `C-g' to quit and return to the original position.
If library `crosshairs.el' is used, highlight each visited mark
position temporarily."
  (interactive)
  (unless mark-ring (error "No marks in this buffer"))
  (unwind-protect
       (let ((curr-pos  (point-marker)))
         (condition-case nil
             (doremi-marks-1)
           (quit (goto-char curr-pos))))
    (when (fboundp 'crosshairs-unhighlight)
      (crosshairs-unhighlight 'even-if-frame-switch))))

(defun doremi-marks-1 ()
  "Helper function for `doremi-marks+'."
  (doremi (lambda (newval)
            (set-mark-command t)
            (when (fboundp 'crosshairs-unhighlight)
              (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))
            newval)
          (car mark-ring)
          nil                           ; ignored
          nil                           ; ignored
          mark-ring))

;;;###autoload
(defun doremi-global-marks+ ()
  "Successively cycle among all marks in the `global-mark-ring'.
You can use `C-g' to quit and return to the original position.
If library `crosshairs.el' is used, highlight each visited mark
position temporarily."
  (interactive)
  (unless global-mark-ring (error "No global marks"))
  (unwind-protect
       (let ((curr-pos  (point-marker)))
         (condition-case nil
             (doremi-global-marks-1)
           (quit (switch-to-buffer (marker-buffer curr-pos))
                 (goto-char curr-pos))))
    (when (fboundp 'crosshairs-unhighlight)
      (crosshairs-unhighlight 'even-if-frame-switch))))

(defun doremi-global-marks-1 ()
  "Helper function for `doremi-global-marks+'."
  (doremi (lambda (newval)
            (pop-global-mark)
            (when (fboundp 'crosshairs-unhighlight)
              (when (fboundp 'crosshairs-highlight) (crosshairs-highlight)))
            newval)
          (car (last global-mark-ring))
          nil                           ; ignored
          nil                           ; ignored
          global-mark-ring))

;; $$$ (defalias 'window-resize+ 'doremi-window-height+)
;;;###autoload
(defun doremi-window-height+ (&optional increment window)
  "Change height of WINDOW incrementally.
INCREMENT is the size increment.
WINDOW is selected.  WINDOW defaults to the selected window."
  (interactive "p")
  (select-window (or window (selected-window)))
  (doremi (lambda (incr) (shrink-window incr) (window-height))
          (window-height) (- increment) t)
  (when (member (car unread-command-events)
                '(left right M-left M-right))
    (doremi-window-width+ increment window)))

;; $$$ (defalias 'window-resize-horizontally+ 'doremi-window-width+)
;;;###autoload
(defun doremi-window-width+ (&optional increment window)
  "Change width of WINDOW incrementally.
INCREMENT is the size increment.
WINDOW is selected.  WINDOW defaults to the selected window."
  (interactive "p")
  (select-window (or window (selected-window)))
  (let ((doremi-up-keys         '(left))
        (doremi-boost-up-keys   '(M-left))
        (doremi-down-keys       '(right))
        (doremi-boost-down-keys '(M-right)))
    (doremi (lambda (incr) (shrink-window-horizontally incr)
                    (window-width))
            (window-width) nil t))
  (when (member (car unread-command-events)
                (append doremi-up-keys   doremi-boost-up-keys 
                        doremi-down-keys doremi-boost-down-keys))
    (doremi-window-height+ increment window)))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doremi-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doremi-cmd.el ends here
