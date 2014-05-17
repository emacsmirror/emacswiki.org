;;; narrow-indirect.el --- Narrow using an indirect buffer that is a clone
;;
;; Filename: narrow-indirect.el
;; Description: Narrow using an indirect buffer that is a clone
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2014, Drew Adams, all rights reserved.
;; Created: Sun May 11 08:05:59 2014 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sat May 17 16:21:01 2014 (-0700)
;;           By: dradams
;;     Update #: 82
;; URL: http://www.emacswiki.org/narrow-indirect.el
;; Doc URL: http://www.emacswiki.org/NarrowIndirect
;; Keywords: narrow indirect buffer clone view multiple-modes
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Narrow using an indirect buffer that is a clone of the current
;;  buffer (which becomes the base buffer for the clone).
;;
;;  Such an indirect buffer gives you a different view of a portion of
;;  the buffer, or even of the whole buffer (use `C-x h C-x 4 n n').
;;  It always has the same text and text properties, but otherwise it
;;  is pretty independent.
;;
;;  In particular, you can kill an indirect buffer without affecting
;;  its base buffer.  You will likely want to kill indirect narrowed
;;  buffers rather than widening them.
;;
;;  Note: Because the indirect clone shares text properties with it
;;  base buffer, if you give it a different major mode that uses
;;  different font-locking then the font-locking of the base buffer
;;  changes the same way.  However, you can restore the font-locking
;;  appropriate to the base buffer, by just toggling `font-lock-mode'
;;  off and on again there.
;;
;;  See the Emacs manual, node `Indirect Buffers'.
;;
;;  It is helpful to be able to easily distinguish indirect buffers
;;  from non-indirect buffers.  This library offers two ways to do
;;  this, for the indirect buffers it creates:
;;
;;  * The buffer name of an indirect narrowed buffer starts with a
;;    prefix that you can set using option `ni-buf-name-prefix'.  The
;;    default value is `I-'.
;;
;;  * The name of an indirect narrowed buffer is highlighted in the
;;    mode line using face `ni-mode-line-buffer-id' instead of face
;;    `mode-line-buffer-id'.  To turn this off, just customize the
;;    former to be the same as the latter.
;;
;;  By default, the name of an indirect narrowed buffer reflects the
;;  name of its base buffer and the text of the narrowed region (or
;;  the name of the defined object, in the case of
;;  `ni-narrow-to-defun-other-window').  But you can control this in
;;  several ways.  See the command doc strings.
;;
;;  To customize the behavior of this library, do this:
;;
;;    M-x customize-group Narrow-Indirect
;;
;;  Suggested key bindings:
;;
;;   (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-other-window)
;;   (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-other-window)
;;   (define-key ctl-x-4-map "np" 'ni-narrow-to-page-other-window)
;;
;;
;;  User options defined here:
;;
;;    `ni-buf-name-prefix', `ni-narrowed-buf-name-max'.
;;
;;  Faces defined here:
;;
;;    `ni-mode-line-buffer-id'.
;;
;;  Commands defined here:
;;
;;    `ni-narrow-to-defun-other-window',
;;    `ni-narrow-to-page-indirect-other-window',
;;    `ni-narrow-to-region-other-window'.
;;
;;  Acknowledgments:
;;
;;   The idea and original code for a command that combines narrowing
;;   with cloning a buffer as an indirect-buffer is due to Zane Ashby:
;;   http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/.
;;
;;   In Emacs bug thread #17401, Phil Sainty proposed adding three
;;   commands to Emacs based on this approach.  Lennart Borgman
;;   contributed code that uses, in the cloned buffer name, some text
;;   based on the narrowed region.
;;
;;   The code in `narrow-indirect.el' extends this a bit and provides
;;   a couple of user options and some alternative (prefix-argument)
;;   behavior.  It is doubtful that Emacs Dev will ever adopt features
;;   such as those defined here, and if they do then this library can
;;   at least help for Emacs versions prior to their addition.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014/05/17 dadams
;;     Added autoload cookies.
;; 2014/05/11 dadams
;;     Created.
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

(defgroup Narrow-Indirect nil
  "Narrow using an indirect buffer."
  :prefix "ni-" :group 'editing
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
narrow-indirect.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/DrewsElispLibraries")
  :link '(url-link :tag "Download" "http://www.emacswiki.org/narrow-indirect.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/NarrowIndirect")
  :link '(emacs-commentary-link :tag "Commentary" "narrow-indirect"))

(defcustom ni-narrowed-buf-name-max 60
  "Max length of cloned indirect buffer name, for narrowing commands."
  :type '(restricted-sexp :tag "Max length of buffer name"
          :match-alternatives ((lambda (x) (and (integerp x)  (> x 0))))
          :value ignore)
  :group 'Narrow-Indirect)

(defcustom ni-buf-name-prefix "I-"
  "Name prefix for indirect buffer cloned by `narrow-*-indirect*' commands.
Using a non-empty prefix lets you easily distinguish the indirect
buffers from the original."
  :type 'string :group 'Narrow-Indirect)

(defface ni-mode-line-buffer-id '((t ; (:overline "green" :underline "green")
                                   (:box (:line-width 1 :color "green"))))
  "Like `mode-line-buffer-id', but for a narrowed indirect clone buffer."
  :group 'Narrow-Indirect :group 'mode-line-faces :group 'basic-faces)

;;;###autoload
(defun ni-narrow-to-defun-other-window (&optional full-name text)
  "`narrow-to-defun' in a cloned indirect buffer in the other window.
The name of the indirect buffer depends on the use of a prefix arg:

* No prefix arg: the current buffer name, but with ` / NAME'
  appended, where NAME is the name of the object defined by the defun.

* Prefix arg < 0 : like no prefix arg, but you are prompted for NAME.

* Prefix arg >= 0: you are prompted for the full buffer name.

However, the buffer name is in any case truncated at
`ni-narrowed-buf-name-max' chars.

Non-interactively:
* FULL-NAME is the full buffer name, and TEXT is ignored.
* TEXT is used for NAME, if FULL-NAME is nil.

See `clone-indirect-buffer'."
  (interactive
   (list (and current-prefix-arg
              (natnump (prefix-numeric-value current-prefix-arg))
              (read-string "Buffer name: "))
         (and current-prefix-arg
              (< (prefix-numeric-value current-prefix-arg) 0)
              (read-string "Buffer name suffix: "))))
  (require 'which-func)
  (let ((here  (point)))
    (mark-defun)
    (ni-narrow-to-region-other-window
     (region-beginning) (region-end) here full-name
     (and (not full-name)  (or text  (which-function))))))

;;;###autoload
(defun ni-narrow-to-region-other-window (start end here
                                               &optional full-name text msgp)
  "`narrow-to-region' in a cloned indirect buffer in the other window.
The indirect buffer is named the same as the current buffer, except:

 * It is prefixed by the value of option `ni-buf-name-prefix'.
 * It is suffixed by ` / TEXT', where TEXT is the region text.

However, the buffer name is in any case truncated at
`ni-narrowed-buf-name-max' chars.

Non-interactively:
START and END are the region beginning and end.
HERE is where to place the cursor, relative to START.
TEXT is prefixed by ` / ' and appended to the original
 buffer name, which is appended to `ni-buf-name-prefix' to name
 the new buffer.
If FULL-NAME is a string then it is used as the complete indirect
buffer name.  (TEXT is then ignored.)

See `clone-indirect-buffer'."
  (interactive
   (list (region-beginning) (region-end) (point)
         (and current-prefix-arg  (read-string "Buffer name: "))
         nil 'MSGP))
  (if (and (= start end)  msgp)
      (message "Region is empty")
    (deactivate-mark)
    (let* ((buf  (or full-name
                     text
                     (replace-regexp-in-string
                      "\\(\s \\)+" "\1"
                      (replace-regexp-in-string
                       "\\`\s+\\|\s+\\'" ""
                       (buffer-substring-no-properties start end)))))
           (buf  (or full-name
                     (concat ni-buf-name-prefix (buffer-name) " / " buf)))
           (buf  (or full-name
                     (substring buf 0 (min (length buf)
                                           ni-narrowed-buf-name-max))))
           (buf  (clone-indirect-buffer buf nil)))
      (with-current-buffer buf (narrow-to-region start end) (goto-char here))
      (pop-to-buffer buf)
      (setq mode-line-buffer-identification
            (list (propertize (car mode-line-buffer-identification)
                              'face 'ni-mode-line-buffer-id))))))

;;;###autoload
(defun ni-narrow-to-page-indirect-other-window (&optional arg)
  "`narrow-to-page' in a cloned indirect buffer in the other window.

See `clone-indirect-buffer'."
  (interactive "P")
  (let ((buf  (clone-indirect-buffer nil nil)))
    (with-current-buffer buf (narrow-to-page arg))
     (pop-to-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'narrow-indirect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; narrow-indirect.el ends here
