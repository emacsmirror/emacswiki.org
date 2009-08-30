;;; icicles-iswitchb.el --- Using iswitchb with Icicles
;;
;; $Id: icicles-iswitchb.el,v 1.7 2007/01/10 14:33:05 rubikitch Exp $
;; Author: rubikitch
;; Maintainer: rubikitch
;; Copyright (C) 2006, rubikitch, all rights reserved.
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-iswitchb.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           iswitchb, apropos, completion, matching, regexp, command
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Integration of [[Icicles]] and [[IswitchBuffers]].
;;
;; If you find that the file you are after is not in a buffer, you can
;; press C-x C-f to immediately drop into `find-file' in iswitchb.
;; But because `icicle-find-file' hijacks `iswitchb-find-file',
;; this feature is not usable when `icicle-mode' is enabled.
;; This package fixes this problem.
;;
;; You can show *Completion* buffer in iswitchb by pressing TAB.
;; This package enhances this feature.
;; Pressing TAB enters `icicle-buffer'.
;; Then you can cycle and narrow candidates.
;;
;;; Installation:
;;
;; (require 'icicles)
;; (require 'icicles-iswitchb)
;; (iswitchb-default-keybindings)
;; (icy-mode)
;;
;; In Emacs 22 or later, you can use `iswitchb-mode' instead of
;; `iswitchb-default-keybindings'.  The order of loading the libraries
;; is unimportant, as is the order of activating the modes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Test:
;; (eevnow "emacs-snapshot -no-site-file -q -L ~/emacs/lisp/ -L ~/emacs/lisp/icicles -l icicles -l icicles-iswitchb -f iswitchb-default-keybindings")
;; (eevnow "emacs-snapshot -no-site-file -q -L ~/emacs/lisp/ -L ~/emacs/lisp/icicles -l icicles -l icicles-iswitchb -f iswitchb-default-keybindings -f icicle-mode")
;;
;;; Change log:
;;
;; 2007/01/10 dadams
;; Updated commentary. The order of Icicles and Iswitchb no longer matters.
;; $Log: icicles-iswitchb.el,v $
;; Revision 1.7  2007/01/10 14:33:05  rubikitch
;; small bugfix
;;
;; Revision 1.6  2007/01/10 14:17:40  rubikitch
;; 2006/12/31 dadams
;; Updated commentary about order of Icicles and Iswitchb.
;;
;; Revision 1.5  2007/01/10 14:12:03  rubikitch
;; small refactoring
;;
;; Revision 1.4  2006/12/28 04:11:16  rubikitch
;; Added installation.
;;
;; Revision 1.3  2006/12/27 19:06:11  rubikitch
;; added Commentary
;;
;; Revision 1.2  2006/12/27 18:40:07  rubikitch
;; *** empty log message ***
;;
;; Revision 1.1  2006/12/27 18:39:11  rubikitch
;; Initial revision
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'iswitchb)

(defvar iswitchb-icicles-regexp nil)
(defvar iswitchb-invalid-regexp)

;;; REPLACE ORIGINAL `iswitchb' defined in `iswitchb.el',
(defun iswitchb ()
  "Switch to buffer matching a substring.
As you type in a string, all of the buffers matching the string are
displayed.  When you have found the buffer you want, it can then be
selected.  As you type, most keys have their normal keybindings,
except for the following:
\\<iswitchb-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[iswitchb-select-buffer-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[iswitchb-next-match] Put the first element at the end of the list.
\\[iswitchb-prev-match] Put the last element at the start of the list.
\\[iswitchb-complete] Complete a common suffix to the current string that
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[iswitchb-toggle-regexp] Toggle regexp searching.
\\[iswitchb-toggle-case] Toggle case-sensitive searching of buffer names.
\\[iswitchb-completion-help] Show list of matching buffers in separate window.
\\[iswitchb-find-file] Exit iswitchb and drop into `find-file'.
\\[iswitchb-kill-buffer] Kill buffer at head of buffer list."
  ;;\\[iswitchb-toggle-ignore] Toggle ignoring certain buffers (see \
  ;;`iswitchb-buffer-ignore')

  (setq iswitchb-exit nil)
  (let* ((prompt "iswitch ")
         iswitchb-invalid-regexp
	 (buf (iswitchb-read-buffer prompt)))
    (if (iswitchb-exit-handler)
        (current-buffer)
      (cond (iswitchb-invalid-regexp
             (message "Won't make invalid regexp named buffer"))
            (t
             ;; View the buffer
             ;;(message "go to buf %s" buf)
             ;; Check buf is non-nil.
             (if buf
                 (if (get-buffer buf)
                     ;; buffer exists, so view it and then exit
                     (iswitchb-visit-buffer buf)
                   ;; else buffer doesn't exist
                   (iswitchb-possible-new-buffer buf)))
             )))))

;;; REPLACE ORIGINAL `iswitchb-complete' defined in `iswitchb.el',
;;; saving it for restoration when you toggle `icicle-mode'.
(or (fboundp 'old-iswitchb-complete)
(fset 'old-iswitchb-complete (symbol-function 'iswitchb-complete)))

(defun icicle-iswitchb-complete ()
  "Select current pattern with `icicle-buffer' amongst the buffer names."
  (interactive)
  (setq iswitchb-icicles-regexp
        (regexp-quote (buffer-substring (minibuffer-prompt-end) (point-max))))
  (setq iswitchb-exit 'iswitchb-icicle-buffer)
  (exit-minibuffer))

(defun iswitchb-icicle-buffer ()
  (let ((icicle-show-Completions-initially-flag t)
        (icicle-buffer-match-regexp iswitchb-icicles-regexp)
        (curbuf (current-buffer)))
    (icicle-buffer)
    (if (equal curbuf (current-buffer))
        (keyboard-quit)
      (current-buffer))))

(defadvice iswitchb-read-buffer (around icicle-iswitchb)
  "Hijack icicle-find-file in iswitchb-read-buffer."
  (let ((olddef (symbol-function 'icicle-find-file)))
    (unwind-protect
        (progn
          (fset 'icicle-find-file (symbol-function 'iswitchb-find-file))
          ad-do-it)
      (fset 'icicle-find-file olddef))))
;; (progn (ad-disable-advice 'iswitchb-read-buffer 'around 'icicle-iswitchb) (ad-update 'iswitchb-read-buffer))

(defun icicle-mode-hook--icicle-iswitchb ()
  "Toggle icicle-iswitchb extension. It is specified by `icicle-mode-hook'."
  (if icicle-mode
      (icicle-iswitchb-activate)
    (icicle-iswitchb-deactivate)))
(add-hook 'icicle-mode-hook 'icicle-mode-hook--icicle-iswitchb)

(defun icicle-iswitchb-activate ()
  (defalias 'iswitchb-complete (symbol-function 'icicle-iswitchb-complete))
  (ad-activate-regexp "icicle-iswitchb"))
(defun icicle-iswitchb-deactivate ()
  (defalias 'iswitchb-complete (symbol-function 'old-iswitchb-complete))
  (ad-deactivate-regexp "icicle-iswitchb"))

(defun iswitchb-exit-handler ()
  "Handle `find-file' and `icicle-buffer' within iswitchb."
  (cond ((eq iswitchb-exit 'findfile)
         (call-interactively 'find-file))
        ((and (symbolp iswitchb-exit) (fboundp iswitchb-exit))
         (funcall iswitchb-exit))
        (t
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-iswitchb)

;; How to save (DO NOT REMOVE!!)
;; (let ((oddmuse-wiki "EmacsWiki")(oddmuse-page-name "icicles-iswitchb.el")) (call-interactively 'oddmuse-post))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-iswitchb.el ends here
