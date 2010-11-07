;;; auto-indent-mode.el --- Auto indent Minor mode
;;
;; Filename: auto-indent-mode.el
;; Description: Auto Indent text on Yank/Paste
;; Author: Matthew L. Fidler & Others
;; Maintainer: Matthew L. Fidler
;; Created: Sat Nov  6 11:02:07 2010 (-0500)
;; Version: 0.1
;; Last-Updated: Sun Nov  7 00:54:41 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 181
;; URL: http://www.emacswiki.org/emacs/auto-indent-mode.el
;; Keywords: Auto Indentation
;; Compatibility: Tested with Emacs 23.x
;;
;; Features that might be required by this library:
;;
;;   `backquote', `bytecomp', `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Provides auto-indentation minor mode.  This allows the following:
;;
;;  (1) Return automatically indents the code appropriately.
;;
;;  (2) Pasting/Yanking indents the appropriately
;;
;;  (3) Killing line will take off unneeded spaces
;;
;;  (4) On visit file, indent appropriately, but DONT SAVE. (Pretend like
;;  nothing happened)
;;
;;  (5) On save, optionally unttabify, remove trailing white-spaces, and
;;  definitely indent the file.
;;
;;  All of these options can be customized. (customize auto-indent)
;;
;;  To use put this in your load path and then put the following in your emacs
;;  file:
;;
;;  (require 'auto-indent-mode)
;;
;;  If you (almost) always want this on, add the following to ~/.emacs:
;;
;;  (auto-indent-global-mode)
;;
;;  Excluded modes are defined in `auto-indent-disabled-modes-list'
;;
;;  If you only want this on for a single mode, you would add the following to
;;  ~/.emacs
;;
;;  (add-hook 'emacs-lisp-mode-hook 'auto-indent-minor-mode)
;;
;;  You could always turn on the minor mode with the command
;;  `auto-indent-minor-mode'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 07-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sun Nov  7 00:54:07 2010 (-0500) #180 (Matthew L. Fidler)
;;    Bug fix where backspace on indented region stopped working.
;; 07-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sun Nov  7 00:30:54 2010 (-0500) #167 (Matthew L. Fidler)
;;    Another small bug fix.
;; 07-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sun Nov  7 00:21:38 2010 (-0500) #154 (Matthew L. Fidler)
;;
;;    Added bugfix and also allow movement on blank lines to be
;;    automatically indented to the correct position.
;;
;; 06-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Sat Nov  6 17:39:59 2010 (-0500) #113 (Matthew L. Fidler)
;;    Initial release.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'cl))

;; Keymap for auto-indent-mode.  Replace return with the appropriate command.


(defgroup auto-indent nil
  "* Auto Indent Mode Customizations"
  :group 'editing)

(defcustom auto-indent-on-yank-or-paste 't
  "* Indent pasted or yanked region."
  :type 'boolean
  :group 'auto-indent
  )
(defcustom auto-indent-mode-untabify-on-yank-or-paste 't
  "* Untabify pasted or yanked region."
  :type 'boolean
  :group 'auto-indent
  )
(defcustom auto-indent-on-visit-file 't
  "* Auto Indent file upon visit."
  :type 'boolean
  :group 'auto-indent
  )
(defcustom auto-indent-on-save-file 't
  "* Auto Indent on visit file."
  :type 'boolean
  :group 'auto-indent
  )
(defcustom auto-indent-untabify-on-visit-file nil
  "* Automatically convert tabs into spaces when visiting a file."
  :type 'boolean
  :group 'auto-indent
  )

(defcustom auto-indent-delete-trailing-whitespace-on-visit-file nil
  "* Automatically remove trailing whitespace when visiting  file"
  :type 'boolean
  :group 'auto-indent
  )

(defcustom auto-indent-untabify-on-save-file 't
  "* Change tabs to spaces on file-save."
  :type 'boolean
  :group 'auto-indent
  )

(defcustom auto-indent-delete-trailing-whitespace-on-save-file nil
  "* When saving file delete trailing whitespace. "
  :type 'boolean
  :group 'auto-indent
  )

(defcustom auto-indent-on-visit-pretend-nothing-changed 't
  "* When modifying the file on visit, pretend nothing changed."
  :type 'boolean
  :group 'auto-indent
  )

(defcustom auto-indent-kill-line-removes-extra-spaces 't
  "* When killing lines, remove extra spaces before killing the line."
  :type 'boolean
  :group 'auto-indent
  )

(defcustom auto-indent-minor-mode-symbol 't
  "* When true, Auto Indent puts AI on the mode line."
  :type 'boolean
  :group 'auto-indent
  )
(defcustom auto-indent-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode org-mode text-mode dired-mode)
  "* List of modes disabled when global auto-indent-mode is on."
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled: "
  :group 'auto-indent)

(defcustom auto-indent-indentation-function 'reindent-then-newline-and-indent
  "* Auto indentation function for the return key."
  :type 'function
  :tag "Indentation function for AutoComplete mode.  While `reindent-then-newline-and-indent' is a likely candidate `newline-and-indent' also works.  ")

(defcustom auto-indent-blank-lines-on-move 't
  "*Auto indentation on moving cursor to blank lines."
  :type 'boolean
  :group 'auto-indent)

(defvar auto-indent-minor-mode-map nil
  "* Auto Indent mode map.")

(unless auto-indent-minor-mode-map
  (setq auto-indent-minor-mode-map (make-sparse-keymap))
  (define-key auto-indent-minor-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  )

(define-minor-mode auto-indent-minor-mode
  "Auto Indent minor mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

When auto-indent-minor-mode minor mode is enabled, yanking or pasting automatically indents

Fall back to default, non-indented yanking by preceding the yanking commands with C-u.

Based on auto-indentation posts, slightly redefined to allow it to be a minor mode

http://www.emacswiki.org/emacs/AutoIndentation

"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.  Nothing.
  (if auto-indent-minor-mode-symbol
      " AI"
    "")
  :group 'auto-indent
  (cond (auto-indent-minor-mode
         ;; Setup
         (make-local-variable 'find-file-hook)
         (add-hook 'find-file-hook 'auto-indent-file-when-visit nil 't)
         (add-hook 'write-contents-hooks 'auto-indent-file-when-save)
         (add-hook 'after-save-hook 'auto-indent-mode-post-command-hook nil 't)
         (add-hook 'post-command-hook 'auto-indent-mode-post-command-hook nil 't)
         (add-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook nil 't)
         )
        (t
         ;; Kill
         (remove-hook 'write-contents-hook 'auto-indent-file-when-save)
         (remove-hook 'find-file-hook 'auto-indent-file-when-visit 't)
         (remove-hook 'after-save-hook 'auto-indent-mode-post-command-hook 't)
         (remove-hook 'post-command-hook 'auto-indent-mode-post-command-hook 't)
         (remove-hook 'pre-command-hook 'auto-indent-mode-pre-command-hook 't)
         )
        ))

(defun auto-indent-minor-mode-on ()
  "* Turn on auto-indent minor mode."
  (unless (or (minibufferp)
              (memq major-mode auto-indent-disabled-modes-list))
    (auto-indent-minor-mode 1))
  )

(define-globalized-minor-mode auto-indent-global-mode auto-indent-minor-mode auto-indent-minor-mode-on
  :group 'auto-indent
  :require 'auto-indent-mode)

;; Define advices for yank and yank-pop.
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after auto-indent-minor-mode-advice activate)
           (and (not current-prefix-arg) auto-indent-minor-mode)
           (let ((mark-even-if-inactive transient-mark-mode))
             (if auto-indent-on-yank-or-paste
                 (indent-region (region-beginning) (region-end) nil))
             (if auto-indent-mode-untabify-on-yank-or-paste
                 (untabify (region-beginning) (region-end)))))))


(defun auto-indent-whole-buffer (&optional save)
  "Auto-indent whole buffer and untabify it"
  (interactive)
  (when (or
         (and save auto-indent-delete-trailing-whitespace-on-save-file)
         (and (not save) auto-indent-delete-trailing-whitespace-on-visit-file)
         )
    (delete-trailing-whitespace))
  (when (or
         (and save auto-indent-on-save-file)
         (and (not save) auto-indent-on-visit-file))
    (indent-region (point-min) (point-max) nil))
  (when (or
         (and (not save) auto-indent-untabify-on-visit-file)
         (and save auto-indent-untabify-on-save-file))
    (untabify (point-min) (point-max))))

(defun auto-indent-file-when-save ()
  "* Auto-indent file when save."
  (if (buffer-file-name)
      (auto-indent-whole-buffer 't)))

(defun auto-indent-file-when-visit ()
  "* auto-indent file when visit."
                                        ;(make-local-variable 'find-file-hook)
  (when (buffer-file-name)
    (auto-indent-whole-buffer)
    (when auto-indent-on-visit-pretend-nothing-changed
      (set-buffer-modified-p nil) ; Make the buffer appear "not modified"
      )))
(defadvice kill-line (before auto-indent-mode activate)
  "If at end of line, join with following; otherwise kill line.
     Deletes whitespace at join."
  (if (and auto-indent-minor-mode
           auto-indent-kill-line-removes-extra-spaces)
      (if (and (eolp) (not (bolp)))
          (progn (delete-indentation 't)
                 (when (looking-at " $")
                   (delete-char 1))))))
(defvar auto-indent-mode-pre-command-hook-line nil)
(make-variable-buffer-local 'auto-indent-mode-pre-command-hook-line)

(defun auto-indent-mode-pre-command-hook()
  "Hook for auto-indent-mode to tell if the point has been moved"
  (setq auto-indent-mode-pre-command-hook-line (line-number-at-pos))
  )

(defun auto-indent-mode-post-command-hook ()
  "Hook for auto-indent-mode to go to the right place when moving around and the whitespace was deleted from the line."
  (condition-case err
      (when (and auto-indent-minor-mode auto-indent-blank-lines-on-move)
        (when (and auto-indent-mode-pre-command-hook-line (not (= (line-number-at-pos) auto-indent-mode-pre-command-hook-line)))
          (when (and (looking-back "^") (looking-at "$"))
            (indent-according-to-mode))))
    (error (message "[Auto-Indent-Mode]: Ignored indentation error in `auto-indent-post-command-hook'"))
    ))
(provide 'auto-indent-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-indent-mode.el ends here
