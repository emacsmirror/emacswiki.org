;;; tool-bar+.el --- Extensions to standard library tool-bar.el
;;
;; Filename: tool-bar+.el
;; Description: Extensions to standard library tool-bar.el
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2004-2015, Drew Adams, all rights reserved.
;; Created: Tue Oct 05 17:02:16 2004
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sun Jan  4 17:02:07 2015 (-0800)
;;           By: dradams
;;     Update #: 246
;; URL: http://www.emacswiki.org/tool-bar%2b.el
;; Doc URL: http://emacswiki.org/ToolBar
;; Keywords: tool-bar, convenience, mouse, button, frame
;; Compatibility: GNU Emacs: 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to standard library tool-bar.el.
;;
;;  New commands defined here:
;;
;;    `show-tool-bar-for-one-command', `tool-bar-here-mode',
;;    `tool-bar-pop-up-mode'.
;;
;;
;;  New key bound here: [menu-bar pop-up-tool-bar]
;;
;;
;;  Usage:
;;
;;    Load this library: (require 'tool-bar+).
;;    Turn on tool-bar pop-up mode: M-x tool-bar-pop-up-mode.
;;
;;    Click "Buttons" in the menu-bar to access the tool-bar when you
;;    need it.  This displays the tool-bar buttons just long enough
;;    for one command: after you click a tool-bar button, the tool-bar
;;    disappears again.
;;
;;    The advantage of `tool-bar-pop-up-mode' is that you do not lose
;;    frame real estate to the tool-bar -- you have it when you need
;;    it, at the cost of an extra click ("Buttons").
;;
;;    In addition to defining minor mode `tool-bar-pop-up-mode', this
;;    library defines minor mode `tool-bar-here-mode', which is the
;;    same as the global `tool-bar-mode' except that it affects only
;;    the current frame.
;;
;;    The advantage of `tool-bar-here-mode' is (again) that it saves
;;    real estate on frames other than the ones with the tool-bar.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;  (require 'tool-bar+) ; load this library
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/07/23 dadams
;;     Remove make-variable-frame-local for tool-bar-here-mode, since deprecated.
;;     tool-bar-here-mode: Add tool-bar-here-mode as frame parameter.
;;     tool-bar-pop-up-mode:
;;       Test frame param tool-bar-here-mode, not frame-local var tool-bar-here-mode.
;; 2011/01/04 dadams
;;     Added autoload cookies (for commands).
;; 2008/04/24 dadams
;;     Use add-to-list, not setq, for menu-bar-final-items.
;; 2006/09/15 dadams
;;     show-tool-bar-for-one-command:
;;       Clear keystrokes echoed so far and prevent echoing, so can see tool-tips.
;; 2006/01/07 dadams
;;     Added :link.
;; 2004/10/10 dadams
;;     Added condition-case in show-tool-bar-for-one-command.
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


 
;;; Local Tool Bar Mode -------------------------

;;;###autoload
(define-minor-mode tool-bar-here-mode
    "Toggle use of the tool bar on this frame only.
With numeric ARG, display the tool bar if and only if ARG is positive.

See `tool-bar-add-item' and `tool-bar-add-item-from-menu' for
conveniently adding tool bar items."
  :init-value nil :global t :group 'mouse :group 'frames

  ;; Emacs 24 allows this, but we want things to work also for Emacs 22-23:
  ;; :variable (frame-parameter nil 'tool-bar-here-mode)

  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
tool-bar+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/tool-bar+.el")
  :link '(url-link :tag "Description"
          "http://www.emacswiki.org/ToolBar#Tool-Bar%20Plus")
  :link '(emacs-commentary-link :tag "Commentary" "tool-bar+")
  (and (display-images-p)
       (let ((lines (if tool-bar-here-mode 1 0)))
         ;; Alter existing frame...
         (modify-frame-parameters (selected-frame)
                                  `((tool-bar-lines . ,lines)
                                    (tool-bar-here-mode . ,tool-bar-here-mode))))
       (if (and tool-bar-here-mode
                (display-graphic-p)
                (= 1 (length (default-value 'tool-bar-map)))) ; not yet set up
           (tool-bar-setup))))
 
;;; Pop-Up Tool Bar Mode ------------------------

;; If either of the normal tool-bar modes is turned on, then
;; `tool-bar-popup-mode' is not available.
(define-key global-map [menu-bar pop-up-tool-bar]
  '(menu-item
    "Buttons" show-tool-bar-for-one-command
    :visible (and tool-bar-pop-up-mode
              (not tool-bar-mode)
              (not (frame-parameter nil 'tool-bar-here-mode)))
    :enable  (and tool-bar-pop-up-mode
              (not tool-bar-mode)
              (not (frame-parameter nil 'tool-bar-here-mode)))
    :help "Use tool bar for one command"))

(add-to-list 'menu-bar-final-items 'pop-up-tool-bar 'append)


;;;###autoload
(define-minor-mode tool-bar-pop-up-mode
  "Toggle tool-bar pop-up.
With numeric ARG, turn on tool-bar pop-up if and only if ARG is positive.

Note: Command `tool-bar-pop-up-mode' functions as a toggle only
      if neither `tool-bar-mode' nor `tool-bar-here-mode' is on.

      If either of those modes is on, then command
      `tool-bar-pop-up-mode' turns them both off and turns
      `tool-bar-pop-up-mode' on."
  :init-value nil :global t :group 'mouse :group 'frames
  (when (or tool-bar-mode (frame-parameter nil 'tool-bar-here-mode))
    (setq tool-bar-pop-up-mode t)
    (tool-bar-mode -99)
    (tool-bar-here-mode -99)))



;; Note: This treats mouse events specially: it scrolls the buffer
;; text (window) down to compensate for the disappearance of the
;; tool-bar.  That is, it replaces the tool-bar with an equivalent
;; number of lines of buffer text.
;;
;; This is so that the place where you click the mouse when the
;; tool-bar is visible corresponds to the place where the mouse is
;; after the tool-bar disappears.  Otherwise, the buffer text moves
;; up, relative to the mouse, and a region is selected (without ever
;; physically moving the mouse).
;;
;;;###autoload
(defun show-tool-bar-for-one-command ()
  "Pop up the tool bar so you can click a button.
The tool bar stays visible until one command is executed
\(whether or not it was initiated by clicking a button)."
  (interactive)
  (unless tool-bar-pop-up-mode
    (error "You must turn on `tool-bar-pop-up-mode' to use this command"))
  (let (evnt tb-lines)
    (unwind-protect
         (let ((echo-keystrokes nil))
           (tool-bar-here-mode 99)      ; Show tool-bar
           (message (current-message))  ; Show tool-tip through keystrokes echoed so far
           (setq evnt (read-event))
           (push evnt unread-command-events))
      (when (and (consp evnt)
                 (member (event-basic-type (car evnt)) '(mouse-1 mouse-2 mouse-3)))
        (setq tb-lines (cdr (assq 'tool-bar-lines (frame-parameters (selected-frame)))))
        (condition-case nil
            (when tb-lines (scroll-down tb-lines))
          (error (tool-bar-here-mode -99)))) ; E.g. "Beginning of buffer" error
      (tool-bar-here-mode -99))))            ; Hide tool-bar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tool-bar+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tool-bar+.el ends here
