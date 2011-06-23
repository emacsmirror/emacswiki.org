;;; sunrise-x-popviewer.el --- floating viewer window for the Sunrise Commander

;; Copyright (C) 2008-2010 José Alfredo Romero L.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 20 Aug 2008
;; Version: 2
;; RCS Version: $Rev: 376 $
;; Keywords: sunrise commander, windows, accessibility, viewer
;; URL: http://www.emacswiki.org/emacs/sunrise-x-popviewer.el
;; Compatibility: GNU Emacs 22+

;; This file is *NOT* part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension advises several Sunrise Commander functions in order to make
;; the viewer window "float", i.e. instead of having a dedicated window sitting
;; under the panes all the time, a new frame is displayed whenever the user
;; requests to view a file (by pressing "o" or "v") or to open a command line in
;; the current directory.

;; WARNING: This code and the Buttons extension (sunrise-x-buttons) do NOT mix
;; together, if you're using the Buttons extension remove it first from your
;; .emacs file.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) If you are currently using the Buttons extension (sunrise-x-buttons),
;; remove it first from your .emacs file.

;; 2) Add a (require 'sunrise-x-popviewer) expression to your .emacs file
;; somewhere after the (require 'sunrise-commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) Use `sr-popviewer-mode' to toggle the functionality.

;; 5) The next time you invoke the Sunrise Commander, only two panes will be
;; displayed. If you press o (or v) on a file inside any of them, it will be
;; displayed in a new frame. If you press C-c t to open a terminal in the
;; current directory, it'll be opened in a new frame too.

;; 6) Enjoy ;-)

;;; Code:

(require 'sunrise-commander)

(defcustom sr-popviewer-enabled t
  "Whether the popviewer extension should be active at startup."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-popviewer-reuse-frame nil
  "If non-nil, always display the viewer window in the same frame (if any).
Only makes sense when `sr-popviewer-mode' is enabled."
  :group 'sunrise
  :type 'boolean)

;; Putting this directly into the advice definition would produce a runtime
;; error due to mysterious behaviour of the byte-compiler. Advice welcome.
(defun sr-popviewer-setup-windows ()
  "`sr-setup-windows' replacement for `sr-popviewer-mode'."
  (bury-buffer)
  (delete-other-windows)

  (case sr-window-split-style
    (horizontal (split-window-horizontally))
    (vertical   (split-window-vertically))
    (top        (ignore))
    (t (error "Sunrise: don't know how to split this window: %s" sr-window-split-style)))

  ;;setup sunrise on both panes
  (sr-setup-pane left)
  (unless (eq sr-window-split-style 'top)
    (other-window 1)
    (sr-setup-pane right))

  ;;select the correct window
  (sr-select-window sr-selected-window)

  (sr-force-passive-highlight)
  (run-hooks 'sr-start-hook))

(defadvice sr-setup-windows
  (around sr-popviewer-advice-setup-windows)
  "Set up the Sunrise window configuration (two windows in `sr-mode')."
  (sr-popviewer-setup-windows))

(defun sr-popviewer-quick-view (&optional arg)
  "Quickly view the currently selected item.
On regular files, it opens the file in a separate frame, on
directories visits the selected directory in the passive pane,
and on symlinks follows the file the link points to in the
passive pane."
  (interactive "P")
  (let ((sr-popviewer-reuse-frame
         (if arg (not sr-popviewer-reuse-frame) sr-popviewer-reuse-frame)))
    (sr-quick-view (and (null window-system) arg))
    (sr-select-window sr-selected-window)))

(defadvice sr-select-viewer-window
  (around sr-popviewer-advice-select-viewer-window)
  "Try to select a window that is not a SC pane in a separate frame."
  (interactive)
  (if (null window-system)
      (let ((sr-selected-window sr-selected-window))
        (sr-select-window (sr-other)))
    (let* ((frame-name "Sunrise Viewer")
           (vframe  (cdr (assoc frame-name (make-frame-names-alist))))
           (target-frame))
      (when vframe
        (select-frame vframe)
        (if sr-popviewer-reuse-frame
            (setq target-frame vframe)
          (set-frame-name (buffer-name))))
      (unless target-frame
        (setq other-window-scroll-buffer nil)
        (setq target-frame (make-frame `((name . ,frame-name)))))
      (select-frame target-frame)
      (raise-frame))))

(define-minor-mode sr-popviewer-mode "Use a floating viewer window."
  :global t
  :group 'sunrise
  :lighter ""
  (let ((hookfun (if sr-popviewer-mode 'remove-hook 'add-hook))
        (viewfun (if sr-popviewer-mode 'sr-popviewer-quick-view
                   'sr-quick-view))
        (adfun (if sr-popviewer-mode 'sr-ad-enable 'sr-ad-disable)))
    (funcall hookfun 'window-size-change-functions 'sr-lock-window)
    (define-key sr-mode-map "o" viewfun)
    (define-key sr-mode-map "v" viewfun)
    (funcall adfun "^sr-popviewer-")
    (if sr-running (sr-lock-panes))))

(defun sunrise-x-popviewer-unload-function ()
  (sr-popviewer-mode -1)
  (sr-ad-disable "^sr-popviewer-"))

(sr-popviewer-mode (if sr-popviewer-enabled 1 -1))
(provide 'sunrise-x-popviewer)

;;;###autoload (require 'sunrise-x-popviewer)

;;; sunrise-x-popviewer.el ends here
