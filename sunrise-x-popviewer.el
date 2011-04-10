;;; sunrise-x-popviewer.el --- Floating viewer window for the Sunrise Commander.

;; Copyright (C) 2008-2010 José Alfredo Romero L.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 20 Aug 2008
;; Version: 1
;; RCS Version: $Rev: 364 $
;; Keywords: Sunrise Commander Emacs File Manager Accessibility Viewer
;; URL: http://www.emacswiki.org/emacs/sunrise-x-popviewer.el
;; Compatibility: GNU Emacs 22+

;; This file is *NOT* part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation,  either  version  3 of the License, or (at your option) any later
;; version.
;;
;; This  program  is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See the GNU General Public License for more de-
;; tails.

;; You  should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension redefines several Sunrise Commander functions in order to make
;; the viewer window "float", i.e. instead of having a dedicated window  sitting
;; under  the  panes  all  the  time, a new frame is displayed whenever the user
;; requests to view a file (by pressing "o" or "v") or to open a command line in
;; the current directory.

;; WARNING:  This  code and the Buttons extension (sunrise-x-buttons) do NOT mix
;; together, if you're using the Buttons extension remove  it  first  from  your
;; .emacs file.

;;; Installation and Usage:

;; 1) Put this file somewhere in your emacs load-path.

;; 2)  If  you  are  currently  using the Buttons extension (sunrise-x-buttons),
;; remove it first from your .emacs file.

;; 2)  Add  a  (require 'sunrise-x-popviewer)   expression  to  your .emacs file
;; somewhere after the (require 'sunrise-commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart emacs.

;; 4)  The  next  time  you invoke the Sunrise Commander, only two panes will be
;; displayed. If you press o (or v) on a file inside any of  them,  it  will  be
;; displayed  in  a  new  frame.  If  you  press C-c t to open a terminal in the
;; current directory, it'll be opened in a new frame too.

;; 5) Enjoy ;-)

;;; Code:

(require 'sunrise-commander)

(remove-hook 'window-size-change-functions 'sr-lock-window)
(define-key sr-mode-map "o" 'sr-popviewer-quick-view)
(define-key sr-mode-map "v" 'sr-popviewer-quick-view)

(defcustom sr-popviewer-reuse-frame nil
  "Whether to display the viewer window always in the same frame (as long as it 
  remains open)."
  :group 'sunrise
  :type 'boolean)

(defun sr-setup-windows()
  "Setup the Sunrise window configuration (two windows in sr-mode.)"

  (bury-buffer)
  (delete-other-windows)

  (cond 
   ((equal sr-window-split-style 'horizontal) (split-window-horizontally))
   ((equal sr-window-split-style 'vertical)   (split-window-vertically))
   ((equal sr-window-split-style 'top)        (ignore))
   (t (error "ERROR: Don't know how to split this window: %s" sr-window-split-style)))

  ;;setup sunrise on both panes
  (sr-setup-pane left)
  (unless (equal sr-window-split-style 'top)
    (other-window 1)
    (sr-setup-pane right))

  ;;select the correct window
  (sr-select-window sr-selected-window)

  (sr-force-passive-highlight)
  (run-hooks 'sr-start-hook))

(defun sr-popviewer-quick-view (&optional arg)
  "Allows to quick-view the currently selected  item: on regular files, it opens
  the file in a separate frame,  on directories visits the selected directory in
  the passive pane, and  on symlinks follows the file the link  points to in the
  passive pane."
  (interactive "P")
  (let ((sr-popviewer-reuse-frame
         (if arg (not sr-popviewer-reuse-frame) sr-popviewer-reuse-frame)))
    (sr-quick-view (and (null window-system) arg))
    (sr-select-window sr-selected-window)))

(defun sr-select-viewer-window (&optional force-setup)
  "Tries to select a window that is not a SC pane in a separate frame."
  (interactive)
  (if (null window-system)
      (let ((sr-selected-window sr-selected-window))
        (sr-select-window (sr-other)))
    (let* ((frame-name "Sunrise Viewer Frame")
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

(provide 'sunrise-x-popviewer)

;;;###autoload (require 'sunrise-x-popviewer)

;;; sunrise-x-popviewer.el ends here.
