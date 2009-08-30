;;; screencast-graphviz-dot-mode.el

;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: esbenandreasen <esbenandreasen@gmail.com>(new)

;; Keywords: screencast

;; This file is not an official part of emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Code:

(require 'screencast-record)
(defun screencast-graphviz-dot (&optional arg)
  (interactive "P")
  (apply
   (if arg
       'screencast-record 
     'screencast)
   '(
     "Hello, and welcome to the screencast for using graphviz-dot-mode in Emacs."n
     "Let's switch to the mode and start typing a graph."
     (graphviz-dot-mode)
     (i "dig")
     "Autocompletion:"
     (graphviz-dot-complete-word)
     (i "{")
     "Indentation rules:"
     (electric-graphviz-dot-terminate-line)
     (i "//")
     "comment-dwim support (with prefix to remove)"
     (comment-dwim t)
     (indent-for-tab-command)
     "font-locking:"n
     (i "the [color=blue]")
     (progn (font-lock-fontify-buffer)
            (electric-graphviz-dot-terminate-line)
            (electric-graphviz-dot-terminate-line)
            (electric-graphviz-dot-close-brace)
            (previous-line 1)
            (indent-for-tab-command)
            (split-window-vertically)
            (save-buffer)
            )
     "default compile command: dot -Tpng $PWD/name.dot > $PWD/name.png"
                                        ; here "dot -Tpng  graphviz-dot-mode.dot > graphviz-dot-mode.png"
     (compile compile-command)
     "Inlined preview:"
     (graphviz-dot-preview)
     "Compilation error handling:"
     (i "b [label=end}")
     (progn (save-buffer))
     (compile compile-command)
     "Go to the error and fix it:"
     (next-error)
     (end-of-line)
     (delete-backward-char 1)
     (i "]")
     (progn (save-buffer))
     (compile compile-command)
     "View in external tool:"
     (graphviz-dot-view)
     )
   "graphviz-dot-mode.dot"
   1 ()
   )
)

