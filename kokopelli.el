;;; kokopelli.el --- List function declaration and jump to it  -*-mode: Emacs-Lisp; tab-width: 4;-*- .

;; Copyright (C) 2009, Kobayashi Takaaki <kobapan at gmail dot com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Date       : 2009-12-04 00:10:00
;; Author     : Kobayashi Takaaki <kobapan at gmail dot com>

;;; Commentary:

;; Installation
;;
;; Add kokopelli.el to your load path
;; add your .emacs
;;
;; (require 'kokopelli)
;; (define-key global-map [f12] 'kokopelli-sing)
;;

;; Usage
;;
;; Type F12 to list up functions.
;;
;; To jump to it, type SPACE or ENTER or double left click on any function in the list.
;;
;; Type q to quit kokopelli.
;;

;; Options
;;
;; If you want to close kokopelli window with your selecting the function, add your .emacs
;;
;; (setq kokopelli-auto-quit t)
;;
;; If you want to change the position where the function you selected will appear, use
;;
;; kokopelli-margin-top
;;
;; eg. With 0 , the function will appear at the top of the window.
;; (setq kokopelli-margin-top 0)
;;
;;

;;; Code:

(defcustom kokopelli-auto-quit nil
"t : kokopelli window close when you select a function.
nil : kokopelli window remains untill you run `q' command.")

(defcustom kokopelli-margin-top 4
"the top margin of the function to be shown.
0 : no margin")

(defvar kokopelli-window-to-back nil
"the window that the cursor appears in when kokopelli-sing run.")

(defun kokopelli-sing ()
  "interactive to create buffer listing class and function"
  (interactive)
  (setq kokopelli-window-to-back (selected-window))
  (let (listing-regexp
        kokopelli-buffer
        (mode (downcase mode-name))
        (file-path (if (not (null buffer-file-name))
                       buffer-file-name
                     (buffer-name)))
        (init-kokopelli
         (lambda (file-name)
           (let ((buffer-to-back (current-buffer))
                 (kokopelli-buffer-name "*kokopelli*")
                 (pop-up-frames nil)
                 w)
             (save-excursion
               (if (not (get-buffer kokopelli-buffer-name))
                   (progn
                     (setq w (split-window nil nil t))
                     (select-window w)
                     (switch-to-buffer kokopelli-buffer-name))
                 (set-buffer kokopelli-buffer-name))
               (setq buffer-read-only nil) ; unlock
               (erase-buffer)
               (insert file-name "\n")
               (setq buffer-read-only t)   ; lock
               (goto-char (point-max))
               (select-window kokopelli-window-to-back)
               (pop-to-buffer buffer-to-back)
               (get-buffer kokopelli-buffer-name)))))
        (insert-into-kokopelli
         (lambda (kokopelli-buffer string place)
           (let ((buffer-to-back (current-buffer))
                 start
                 end)
             (set-buffer kokopelli-buffer)
             (setq buffer-read-only nil) ; unlock
             (setq start (point))
             (insert string)
             (insert "\n")
             (setq end (point))
             (put-text-property start end 'goto place)
             (setq buffer-read-only t)   ; lock
             (set-buffer buffer-to-back))))
        (kokopelli-mode
         (lambda (kokopelli-buffer)
           (let ((map (make-sparse-keymap)))
             (pop-to-buffer kokopelli-buffer)
             (setq mode-name "kokopelli-mode")
             (define-key map [double-mouse-1] 'kokopelli-jump)
             (define-key map "\r" 'kokopelli-jump)
             (define-key map " " 'kokopelli-jump)
             (define-key map "q" 'kokopelli-quit)
             (use-local-map map)))))
    (setq listing-regexp (cond ((or (equal mode "emacs-lisp")
                                    (equal mode "e-lisp")
                                    (equal mode "el")
                                    (equal mode "lisp")
                                    (equal mode "lisp-mode")
                                    (equal mode "scheme")
                                    (equal mode "scheme-mode")
                                    (equal mode "gauche")
                                    (equal mode "gauche-mode")
                                    (equal mode "lisp interaction"))
                                "^\\([ \t]*(\\(defun\\|defmacro\\|define\\|defvar\\|defconst\\|defconstant\\|defclass\\|defcustom\\|defgroup\\)[ \t]+\\([^ ;\t\n]*\\)\\([ ;\t\n]+.*\\)\\)$")
                               ((or (equal mode "c++")
                                    (equal mode "c")
                                    (equal mode "cpp")
                                    (equal mode "cc")
                                    (equal mode "c/l")
                                    (equal mode "c++/l"))
                                "\\(^\\([^ \t\n#/\\*]+\\)\\([0-9a-zA-Z_ \t\\*]+\\)([^\n;]*\\)$")
                               ((or (equal mode "perl")
                                    (equal mode "perl-mode")
                                    (equal mode "cperl")
                                    (equal mode "pl"))
                                "^\\([ \t]*\\(sub\\)[ \t]+\\(.*\\)\\)$")
                               ((or (equal mode "ruby")
                                    (equal mode "ruby-mode")
                                    (equal mode "rb"))
                                "^\\([ \t]*\\(class\\|module\\|def\\|alias\\)[ \t]+\\(.*\\)\\)$")
                               ((or (equal mode "js")
                                    (equal mode "javascript"))
                                "^\\([ \t]*\\(.*\\)[ \t]*=?[ \t]*\\(function\\)[ \t]*=?[ \t]*\\(.*\\)\\|\\([ \t]*\\(.*\\)[ \t]*=[ \t]*{\\)\\)$")
                               ((equal mode "php")
                                "^\\([ \t]*\\(\\(public\\|private\\|static\\|protected\\)[ \t]+\\)*\\(function\\|class\\)[ \t]+\\([^\(\{]*\\).*\\)$")
                               ((equal mode "html")
                                "^\\([ \t]*<[Hh][123456].*\\|[ \t]*<[Hh][Ee][Aa][Dd].*\\|[ \t]*<[Bb][Oo][Dd][Yy].*\\|[ \t]*<[Ff][Oo][Rr][Mm].*\\)$")
                               ((equal mode "text")
                                "^\\([ \t]*[1234567890]+[\.]+.*\\)$")
                               ((equal mode "tex")
                                "^\\(\\\\chapter.*\\|\\\\section.*\\|\\\\subsection.*\\|\\\\subsubsection.*\\)$")
                               ((or (equal mode "pascal")
                                    (equal mode "pas"))
                                "^\\([Pp][Rr][Oo][Cc][Ee][Dd][Uu][Rr][Ee].*\\)$")
                               ((or (equal mode "java")
                                    (equal mode "java-mode")
                                    (equal mode "jav"))
                                "\\(^[ \t]*[^\n#/\\*=]+[0-9a-zA-Z_ \t\\*,\.()]+{[^\n;]*\\)$")
                               ((or (equal mode "basic")
                                    (equal mode "basic-mode")
                                    (equal mode "bas"))
                                "^\\(\\([Pp][Rr][Ii][Vv][Aa][Tt][Ee]\\|[Pp][Uu][Bb][Ll][Ii][Cc]\\|[Ss][Uu][Bb]\\|[F][U][N][C][T][I][O][N]\\)[ \t]+.*\\)$")
                               (t
                                (error (concat "sorry. mode not supported [" mode "].")))))
    (setq kokopelli-buffer (funcall init-kokopelli file-path))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward listing-regexp nil t)
        (funcall insert-into-kokopelli
                 kokopelli-buffer
                 (buffer-substring (match-beginning 1) (match-end 1))
                 (match-beginning 1))))
    (funcall kokopelli-mode kokopelli-buffer)))

(defun kokopelli-jump ()
  "interactive function jump from kokopelli buffer to searching class or function"
  (interactive)
  (let ((buffer-to-back (current-buffer))
        aim-point
        file-name)
    (save-excursion
      (when (setq aim-point (get-text-property (point) 'goto))
        (progn
          (goto-char (point-min))
          (setq file-name (buffer-substring (point) (progn (end-of-line) (point))))
          (select-window kokopelli-window-to-back)
          (pop-to-buffer (find-file-noselect file-name))
          (goto-char aim-point)
          (if (>= (string-to-number emacs-version) 23)
              (recenter-top-bottom kokopelli-margin-top)
            (recenter kokopelli-margin-top))
          (pop-to-buffer buffer-to-back)
          (when kokopelli-auto-quit (kokopelli-quit)))))))

(defun kokopelli-quit ()
  "interactive function quit kokopelli mode"
  (interactive)
  (let ((kokopelli-buffer-name "*kokopelli*")
        file-name)
    (set-buffer kokopelli-buffer-name)
    (goto-char (point-min))
    (setq file-name (buffer-substring (point) (progn (end-of-line) (point))))
    (delete-windows-on kokopelli-buffer-name)
    (kill-buffer kokopelli-buffer-name)
    (select-window kokopelli-window-to-back)
    (pop-to-buffer (find-file-noselect file-name))))


(provide 'kokopelli)
;;; kokopelli.el
