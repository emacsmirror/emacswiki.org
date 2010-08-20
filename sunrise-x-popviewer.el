;;; sunrise-x-popviewer.el --- Floating viewer window for the Sunrise Commander.

;; Copyright (C) 2008-2010 José Alfredo Romero L.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 20 Aug 2008
;; Version: 1
;; RCS Version: $Rev: 315 $
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

;; This is version 1 $Rev: 315 $ of the Sunrise Commander PopViewer Extension.

;; This  piece  of code is still in alpha stage. If you find it useful and think
;; you may contribute to it with suggestions of even more code,  please  let  me
;; know  by sending a message to the comp.emacs newsgroup.

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

(defun sr-quick-view (&optional arg)
  "Allows to quick-view the currently selected item: on regular files, it opens
  the file in a new frame, on directories, visits the selected directory in the
  passive pane, and on symlinks follows the file the link points to."
  (interactive)
  (let ((name (dired-get-filename nil t)))
    (cond ((file-directory-p name) (sr-quick-view-directory name))
          ((file-symlink-p name) (sr-quick-view-symlink name))
          (t (find-file-other-frame (dired-get-file-for-visit))))))

(defun sr-select-viewer-window (&optional force-setup)
  "Tries to select a window that is not a sr pane."
  (interactive)
  (make-frame '((name . "Sunrise Viewer Frame")))
  (select-frame-by-name "Sunrise Viewer Frame"))

(provide 'sunrise-x-popviewer)

;;;###autoload (require 'sunrise-x-popviewer)

;;; sunrise-x-popviewer.el ends here.
