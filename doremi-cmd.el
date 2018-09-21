;;; doremi-cmd.el --- Miscellaneous Do Re Mi commands
;;
;; Filename: doremi-cmd.el
;; Description: Miscellaneous Do Re Mi commands
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2004-2018, Drew Adams, all rights reserved.
;; Created: Sun Sep 12 17:13:58 2004
;; Version: 0
;; Package-Requires: ((doremi "0"))
;; Last-Updated: Fri Sep 21 13:47:27 2018 (-0700)
;;           By: dradams
;;     Update #: 521
;; URL: https://www.emacswiki.org/emacs/download/doremi-cmd.el
;; Doc URL: https://www.emacswiki.org/emacs/DoReMi
;; Keywords: keys, cycle, repeat
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   `cus-theme', `doremi', `mwheel', `ring'.
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
;;  User options defined here:
;;
;;    `doremi-color-themes', `doremi-custom-themes' (Emacs 24+),
;;    `doremi-custom-themes-accumulate-flag' (Emacs 24+),
;;    `doremi-themes-update-flag'.
;;
;;  Commands defined here:
;;
;;    `doremi-bookmarks+', `doremi-buffers+', `doremi-color-themes+',
;;    `doremi-custom-themes+' (Emacs 24+), `doremi-global-marks+',
;;    `doremi-marks+', `doremi-window-height+', `doremi-windows+'
;;    (Emacs 22+), `doremi-window-width+'.
;;
;;  Non-interactive functions defined here:
;;
;;    `doremi--pop-to-buffer-same-window', `doremi-buffers-1',
;;    `doremi-color-themes-1', `doremi-custom-themes-1' (Emacs 24+),
;;    `doremi-global-marks-1', `doremi-marks-1', `doremi-windows-1'.
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
;;   (define-key doremi-map "s" 'doremi-custom-themes+) ; custom schemes
;;   (define-key doremi-map "w" 'doremi-window-height+)
;;
;;  Customize the menu. Uncomment this to try it out.
;;
;;   (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
;;   (define-key global-map [menu-bar doremi]
;;     (cons "Do Re Mi" menu-bar-doremi-menu))
;;   (define-key menu-bar-doremi-menu [doremi-custom-themes]
;;     '(menu-item "Custom Themes" . doremi-custom-themes+
;;       :help "Successively cycle among custom themes: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-global-marks]
;;     '(menu-item "Global Marks" . doremi-global-marks+
;;       :help "Successively cycle among global marks: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-marks]
;;     '(menu-item "Marks in Buffer" . doremi-marks+
;;       :help "Successively cycle among marks in this buffer: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-bookmarks]
;;     '(menu-item "Bookmarks" . doremi-bookmarks+
;;       :help "Successively cycle among bookmarks: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-buffers]
;;     '(menu-item "Buffers" . doremi-buffers+
;;       :help "Successively cycle among buffers: `up'/`down'"))
;;   (define-key menu-bar-doremi-menu [doremi-windows]
;;     '(menu-item "Windows" doremi-windows+
;;       :help "Successively cycle among windows: `up'/`down'"
;;       :enable (not (one-window-p))))
;;   (define-key menu-bar-doremi-menu [doremi-window-height]
;;     '(menu-item "Window Size" doremi-window-height+
;;       :help "Resize window incrementally: `up'/`down'/`left'/`right'"
;;       :enable (not (one-window-p))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2018/09/21 dadams
;;     Added: doremi--pop-to-buffer-same-window.
;;     doremi-buffers+, doremi-buffers-1, doremi-global-marks+:
;;       Use idoremi--pop-to-buffer-same-window, not switch-to-buffer.
;; 2016/05/12 dadams
;;     Added: doremi-window+, doremi-windows-1.
;; 2013/10/27 dadams
;;     Added: doremi-custom-themes-accumulate-flag.
;;     doremi-custom-themes-1: Respect doremi-custom-themes-accumulate-flag.
;; 2013/10/25 dadams
;;     Added: doremi-themes-update-flag.
;;     doremi-custom-themes: Use custom-available-themes, not custom-theme-p.
;;     doremi-(color|custom)-themes+: Added prefix arg - flips theme-saving flag.
;;     doremi-custom-themes+: Pass initial-theme to doremi-custom-themes-1.
;;                            Move chosen theme to the front.
;;                            Added message to echo final choice.
;;     doremi-custom-themes-1: Start with first theme, or last if no cycling yet.
;; 2013/10/23 dadams
;;     Added: doremi-custom-themes, doremi-custom-themes+, doremi-custom-themes-1.
;;     Thx to Kawabata Taichi.
;; 2011/01/04 dadams
;;     Added autoload cookie for defgroup.
;; 2010/03/09 dadams
;;     doremi-color-themes: Initialize to ().
;;     doremi-color-themes+: Do the real init of var doremi-color-themes here.
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

;; Quiet the byte-compiler.
(defvar color-theme-initialized)
(defvar color-themes)
(defvar custom-enabled-themes)          ; In `custom.el'.
(defvar doremi-custom-themes)           ; Here, Emacs 24+
(defvar doremi-custom-themes-accumulate-flag) ; Here, Emacs 24+

;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; User Options (Variables)

;;;###autoload
(defgroup doremi-misc-commands nil
  "Miscellaneous Do Re Mi commands."
  :prefix "doremi-" :group 'doremi :group 'color-theme
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
doremi-cmd.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/doremi-cmd.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Doremi")
  :link '(emacs-commentary-link :tag "Commentary" "doremi-cmd")
  )

;;;###autoload
(defcustom doremi-themes-update-flag nil
  "*Non-nil means choosing a theme saves the updated list of themes.
This applies to commands `doremi-custom-themes+' and
`doremi-color-themes+' and their respective options,
`doremi-custom-themes' and `doremi-color-themes'.

A prefix arg to the command flips the option value for the duration of
the command.")

;; Replace this by your favorite custom themes.
;;
;; Emacs 22-23 `cus-themes.el' has no `provide', and only Emacs 24 version
;; has `custom-available-themes'.
(when (condition-case nil (require 'cus-theme nil t) (error nil)) ; Emacs 24+
  (defcustom doremi-custom-themes ()
    "*List of custom themes to cycle through using `doremi-custom-themes+'."
    :type '(repeat (restricted-sexp
                    :match-alternatives
                    ((lambda (symb) (memq symb (custom-available-themes))))))
    :group 'doremi-misc-commands)

  (defcustom doremi-custom-themes-accumulate-flag nil
    "*Non-nil does not disable other custom themes when cycling to a theme.
Note: Setting this to non-nil can considerably slow down cycling.  The
more themes you cycle through, the slower it gets."
    :type 'boolean :group 'doremi-misc-commands))

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
(defcustom doremi-color-themes ()
  "*List of color themes to cycle through using `doremi-color-themes+'."
  :type 'hook :group 'doremi-misc-commands)
 
;;; Commands (Interactive Functions)

;; Emacs 22-23 `cus-themes.el' has no `provide', and only Emacs 24 version
;; has `custom-available-themes'.
(when (condition-case nil (require 'cus-theme nil t) (error nil)) ; Emacs 24+
  (defun doremi-custom-themes+ (&optional flip)
    "Successively cycle among custom themes.
The themes used for cycling are those in option `doremi-custom-themes'.

You can use `C-g' to quit and cancel changes made so far.  Note,
however, that some things might not be restored.  `C-g' can only
disable any themes that you applied.  It cannot restore other
customizations that enabling a theme might have overruled.

Note: Having a lot of frames present can slow down this command
considerably.

Option `doremi-custom-themes-accumulate-flag' determines whether
cycling accumulates themes or disables all themes other than the
current one.  Note: A non-nil value (accumulating) can considerably
slow down cycling.

Option `doremi-themes-update-flag' determines whether the updated
value of `doremi-custom-themes' is saved.  A prefix arg to this command
flips the option value for the current invocation of the command."
    (interactive "P")
    ;; Delete `nil' that gets added by `enable-theme'.
    (let ((orig-themes  (delq nil (copy-sequence custom-enabled-themes))))
      (unless doremi-custom-themes
        (setq doremi-custom-themes  (custom-available-themes)))
      (condition-case nil
          (progn
            (doremi-custom-themes-1 (car orig-themes))
            ;; `enable-theme' -> `custom-reevaluate-setting' adds `nil'.
            (setq doremi-custom-themes  (delq nil doremi-custom-themes))
            ;; Move chosen theme to the front.
            (setq doremi-custom-themes  (delq (car custom-enabled-themes)
                                              doremi-custom-themes))
            (setq doremi-custom-themes  (cons (car custom-enabled-themes)
                                              doremi-custom-themes))
            (message "Chosen theme: `%s'" (car doremi-custom-themes))
            (when (or (and flip        (not doremi-themes-update-flag))
                      (and (not flip)  doremi-themes-update-flag))
              (customize-save-variable 'doremi-custom-themes
                                       doremi-custom-themes)))
        ((quit error)
         (condition-case nil
             (progn (mapc #'disable-theme custom-enabled-themes)
                    (mapc #'enable-theme orig-themes))
           (error nil))))))

  (defun doremi-custom-themes-1 (initial-theme)
    "Helper function for `doremi-custom-themes+'."
    (doremi (lambda (theme)             ; Enable it (SETTER-FN)
              (condition-case nil
                  (progn
                    (unless doremi-custom-themes-accumulate-flag
                      (mapc #'disable-theme custom-enabled-themes))
                    (if (custom-theme-p theme)
                        (enable-theme theme)
                      (load-theme theme t))
                    (run-hooks 'doremi-custom-theme-hook))
                (error (condition-case nil (disable-theme theme) (error nil))))
              theme)                    ; Return it, for next iteration.
            ;; Start with the first theme, or the last one if no cycling done yet.
            (or initial-theme  (car (last doremi-custom-themes)))
            nil                         ; Ignored (INCR)
            nil                         ; Ignored (GROWTH-FN)
            doremi-custom-themes)))     ; ENUM - enumeration

;;;###autoload
(defun doremi-color-themes+ (&optional flip)
  "Successively cycle among color themes.
The themes used for cycling are those in option `doremi-color-themes'.

You can use `C-g' to quit and cancel changes made so far.
Alternatively, after using `doremi-color-themes+' you can use
`color-theme-select' and choose pseudo-theme `[Reset]' - that does the
same thing.  Note that in either case, some things might not be
restored.

Option `doremi-themes-update-flag' determines whether the updated
value of `doremi-color-themes' is saved.  A prefix arg to this command
flips the option value for the current invocation of the command.

To use this command, you must have loaded library `color-theme.el',
available from http://www.nongnu.org/color-theme.  See also:
https://www.emacswiki.org/emacs/ColorTheme."
  (interactive "P")
  (unless (prog1 (require 'color-theme nil t)
            (when (and (fboundp 'color-theme-initialize)
                       (not color-theme-initialized))
              (condition-case nil
                  (let ((color-theme-load-all-themes  t))
                    (color-theme-initialize)
                    (setq color-theme-initialized  t))
                (error nil))))
    (error "This command requires library `color-theme.el'"))
  (unless doremi-color-themes
    (setq doremi-color-themes  (delq 'bury-buffer (mapcar 'car color-themes))))
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
        (progn
          (doremi-color-themes-1)
          (when (or (and flip        (not doremi-themes-update-flag))
                    (and (not flip)  doremi-themes-update-flag))
            (customize-save-variable 'doremi-color-themes
                                     doremi-color-themes)))
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
      (quit (doremi--pop-to-buffer-same-window curr-buff)))))

(defun doremi-buffers-1 ()
  "Helper-function for `doremi-buffers+'."
  (doremi (lambda (newval)
            (doremi--pop-to-buffer-same-window newval 'norecord) newval)
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
           (quit (doremi--pop-to-buffer-same-window (marker-buffer curr-pos))
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

(when (fboundp 'window-list)            ; Emacs 22+
  (defun doremi-windows+ ()
    "Use up and down arrow keys to cycle among windows of selected frame.
    You can use `C-g' to quit and return to the originally selected window."
    (interactive)
    (let ((sel-win  (selected-window)))
      (condition-case nil
          (doremi-windows-1)
        (quit (select-window sel-win)))))

  (defun doremi-windows-1 ()
    "Helper-function for `doremi-windows+'."
    (doremi (lambda (newval) (select-window newval 'norecord) newval)
            (selected-window)
            nil
            nil
            (reverse (window-list)))))

(if (fboundp 'pop-to-buffer-same-window)
    (defalias 'doremi--pop-to-buffer-same-window 'pop-to-buffer-same-window)
  (defalias 'doremi--pop-to-buffer-same-window 'switch-to-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'doremi-cmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; doremi-cmd.el ends here
