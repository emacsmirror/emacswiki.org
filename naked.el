;;; naked.el --- Provide for naked key descriptions: no angle brackets.
;;
;; Filename: naked.el
;; Description: Provide for naked key descriptions: no angle brackets.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2011, Drew Adams, all rights reserved.
;; Created: Fri Oct  7 13:12:52 2011 (-0700)
;; Version: 21.0
;; Last-Updated: Sat Oct  8 00:06:57 2011 (-0700)
;;           By: dradams
;;     Update #: 97
;; URL: http://www.emacswiki.org/cgi-bin/wiki/
;; Keywords: lisp, key, print, format, help
;; Compatibility: GNU Emacs 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Lets you use key-sequence descriptions that do not bother with
;;  angle brackets: `<' and `>'.
;;
;;  Prior to Emacs 21, vanilla GNU Emacs did not bother with angle
;;  brackets either, but someone around the turn of the century got
;;  the idea that Emacs could not do without them.  So instead of the
;;  `delete' key we now have the `<delete>' key.  And instead of`C-x
;;  M-delete' we now have `C-x M-<delete>'.  "On n'arrete pas le
;;  progres !"
;;
;;  Angle brackets are not needed - why?  Because we already use
;;  spaces to separate keys in a key-sequence description (we use
;;  `SPC' to indicate the SPACE key).
;;
;;  To be fair, it is true that sometimes people have taken the
;;  shortcut when writing about user input of writing, e.g., `M-x
;;  forward-char RET' instead of writing `M-x f o r w a r d - c h a
;;  r'. And if you write `forward' that way to stand for an input
;;  sequence of seven chars, then you cannot also expect `forward' to
;;  stand for a function key named `forward', can you?
;;
;;  Well, yes you can, if the context makes things clear enough.  And
;;  I for one (maybe the only one), find `C-M-<insert>' butt-ugly
;;  (likewise `<C-M-insert>') - and, more importantly, insulting to
;;  Occam.
;;
;;  So go ahead and go NaKeD - shed your angles.
;;
;;  Here's what you need.  It won't completely purge Emacs from
;;  insulting you with the occasional pair of angle brackets, but it
;;  at least lets you DTRT in code you write:
;;
;;  * Use function `naked-key-description' instead of
;;    `key-description'.
;;
;;    The former outputs naked key descriptions: no angle brackets
;;    around function keys.  E.g., if KEY is the sequence of events
;;    produced by holding the Shift key while hitting the Insert key,
;;    then `(naked-key-description KEY)' returns "S-insert" (and not
;;    "S-<insert>").  (Internally, this sequence of events is the
;;    vector [S-insert].)
;;
;;  * Use macro `naked' instead of macro `kbd'.
;;
;;    The former expects its argument key-sequence description to use
;;    naked keys, not clothed in angle brackets.  E.g., (naked
;;    "C-M-delete") has the same effect as (kbd "C-M-<delete>").
;;
;;  * Use command `naked-read-kbd-macro' instead of `read-kbd-macro'.
;;
;;  * Use function `naked-edmacro-parse-keys' instead of
;;    `edmacro-parse-keys' (if you happen to use that lower-level
;;    function).
;;
;;  But you can also have it both ways if or when you might need to:
;;  All of these accept an optional argument ANGLES which, if
;;  non-`nil', returns the behavior to the vanilla one, expecting
;;  function keys to be fully clothed in angle brackets.  E.g.: (naked
;;  "C-M-<delete>" t).
;;
;;  Enjoy!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2011/10/07 dadams
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

(defun naked-key-description (keys &optional prefix angles)
  "Like `key-description', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means use angle brackets."
  (let ((result  (if (< emacs-major-version 22)
                     (key-description keys)
                   (key-description keys prefix))))
    (unless angles                      ; Assume that spaces separate function keys.
      (setq result  (replace-regexp-in-string "<\\([^>]+\\)>" "\\1" result 'fixed-case)))
    result))

(defun naked-edmacro-parse-keys (string &optional need-vector angles)
  "Like `edmacro-parse-keys', but does not use angle brackets, by default.
Non-nil optional arg ANGLES means to use angle brackets, exactly like
`edmacro-parse-keys'.  See `naked-read-kbd-macro' for more about
ANGLES."
  (let ((case-fold-search  nil)
	(len               (length string)) ; We won't alter string in the loop below.
        (pos               0)
        (res               []))
    (while (and (< pos len)  (string-match "[^ \t\n\f]+" string pos))
      (let* ((word-beg  (match-beginning 0))
	     (word-end  (match-end 0))
	     (word      (substring string word-beg len))
	     (times     1)
             (key       nil))
        (if (and angles  (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word))
            (setq word  (match-string 0 word)
                  pos   (+ word-beg (match-end 0)))
          (setq word  (substring string word-beg word-end)
                pos   word-end))
        (when (string-match "\\([0-9]+\\)\\*." word)
          (setq times  (string-to-number (substring word 0 (match-end 1)))
                word   (substring word (1+ (match-end 1)))))
        (cond ((string-match "^<<.+>>$" word)
               (setq key  (vconcat (if (eq (key-binding [?\M-x])
                                           'execute-extended-command)
                                       [?\M-x]
                                     (or (car (where-is-internal
                                               'execute-extended-command))
                                         [?\M-x]))
                                   (substring word 2 -2) "\r")))
              ((or (equal word "REM") (string-match "^;;" word))
               (setq pos  (string-match "$" string pos)))
              ((and (string-match (if angles
                                      "^\\(\\([ACHMsS]-\\)*\\)<\\(..+\\)>$"
                                    "^\\(\\([ACHMsS]-\\)*\\)\\([^ \t\f\n][^ \t\f\n]+\\)$")
                                  word)
                    (or angles
                        ;; Do not count `C-' etc. when at end of string.
                        (save-match-data (not (string-match "\\([ACHMsS]-.\\)+$" word))))
                    (progn
                      (setq word  (concat (substring word (match-beginning 1) (match-end 1))
                                          (substring word (match-beginning 3) (match-end 3))))
                      (not (string-match "\\<\\(NUL\\|RET\\|LFD\\|ESC\\|SPC\\|DEL\\)$" word))))
               (setq key  (list (intern word))))
              (t
               (let ((orig-word  word)
                     (prefix     0)
                     (bits       0))
                 (while (string-match "^[ACHMsS]-." word)
                   (incf bits (cdr (assq (aref word 0) '((?A . ?\A-\^@) (?C . ?\C-\^@)
                                                         (?H . ?\H-\^@) (?M . ?\M-\^@)
                                                         (?s . ?\s-\^@) (?S . ?\S-\^@)))))
                   (incf prefix 2)
                   (callf substring word 2))
                 (when (string-match "^\\^.$" word)
                   (incf bits ?\C-\^@)
                   (incf prefix)
                   (callf substring word 1))
                 (let ((found  (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                             ("LFD" . "\n") ("TAB" . "\t")
                                             ("ESC" . "\e") ("SPC" . " ")
                                             ("DEL" . "\177")))))
                   (when found (setq word  (cdr found))))
                 (when (string-match "^\\\\[0-7]+$" word)
                   (loop for ch across word
                         for n = 0 then (+ (* n 8) ch -48)
                         finally do (setq word  (vector n))))
                 (cond ((= bits 0) (setq key  word))
                       ((and (= bits ?\M-\^@) (stringp word)  (string-match "^-?[0-9]+$" word))
                        (setq key  (loop for x across word collect (+ x bits))))
                       ((/= (length word) 1)
                        (error "%s must prefix a single character, not %s"
                               (substring orig-word 0 prefix) word))
                       ((and (/= (logand bits ?\C-\^@) 0) (stringp word)
                             ;; Used to accept `.' and `?' here, but `.' is simply wrong,
                             ;; and `C-?' is not used (so use `DEL' instead).
                             (string-match "[@-_a-z]" word))
                        (setq key  (list (+ bits (- ?\C-\^@) (logand (aref word 0) 31)))))
                       (t (setq key  (list (+ bits (aref word 0)))))))))
        (when key (loop repeat times do (callf vconcat res key)))))
    (when (and (>= (length res) 4)  (eq (aref res 0) ?\C-x)  (eq (aref res 1) ?\()
               (eq (aref res (- (length res) 2)) ?\C-x)  (eq (aref res (- (length res) 1)) ?\)))
      (setq res  (edmacro-subseq res 2 -2)))
    (if (and (not need-vector)
	     (loop for ch across res
		   always (and (if (fboundp 'characterp)  (characterp ch)  (char-valid-p ch))
			       (let ((ch2  (logand ch (lognot ?\M-\^@))))
				 (and (>= ch2 0)  (<= ch2 127))))))
	(concat (loop for ch across res collect (if (= (logand ch ?\M-\^@) 0)  ch  (+ ch 128))))
      res)))

;;;###autoload
(defun naked-read-kbd-macro (start &optional end angles)
  "Read the region as a keyboard macro definition.
Like `read-kbd-macro', but does not use angle brackets, by default.

With a prefix arg use angle brackets, exactly like `read-kbd-macro'.
That is, with non-nil arg ANGLES, expect key descriptions to use angle
brackets (<...>).  Otherwise, expect key descriptions not to use angle
brackets.  For example:

 (naked-read-kbd-macro  \"mode-line\"  t)   returns [mode-line]
 (naked-read-kbd-macro \"<mode-line>\" t t) returns [mode-line]"
  (interactive "r\P")
  (if (stringp start)
      (naked-edmacro-parse-keys start end angles)
    (setq last-kbd-macro  (naked-edmacro-parse-keys (buffer-substring start end) nil angles))))

(defmacro naked (keys &optional angles)
  "Like `kbd', but does not use angle brackets, by default.
With non-nil optional arg ANGLES, expect key descriptions to use angle
brackets (<...>), exactly like `kbd'.  Otherwise, expect key
descriptions not to use angle brackets.  For example:

 (naked \"mode-line\")     returns [mode-line]
 (naked \"<mode-line>\" t) returns [mode-line]

The default behavior lets you use, e.g., \"C-x delete\" and \"C-delete\"
instead of \"C-x <delete>\" and \"C-<delete>\"."
  (naked-read-kbd-macro keys nil angles))

;;;;;;;;;;;;;;;;;

(provide 'naked)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; naked.el ends here
