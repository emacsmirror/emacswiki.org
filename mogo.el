;;; mogo.el --- Simple MoGo client

;; Copyright (C) 2008  Mathias Kegelmann

;; Author: Mathias Kegelmann
;; Keywords: games

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This client runs "mogo" in a buffer *mogo* and sets the buffer's
;; major mode to mogo-mode.  MoGo mode redefines RET such that the
;; player can directly use it on a board to submit the corresponding
;; move.  If RET is pressed on the last line, then the line is sent to
;; mogo - after a number of convenience replacements have been made.
;;
;; Currently, mogo's output is not filtered: it simply appears in the
;; buffer as mogo sends it.


;;; Installation:

;; Put (load-library "PATH_TO_MOGO/mogo") in your .emacs file.  This
;; loads mogo.elc if a byte compiled version is found or mogo.el,
;; otherwise.
;; Alternatively, put mogo.el(c) in a directory on the load-path and
;; say (require 'mogo).


;;; TODO:
;; - change thinking time (i.e. mogo's strength)
;;   - workaround: change mogo-comand
;;
;; - use a unit test framework like ert.el?
;;   - or at least exclude test suite when compiling?
;; - more fancy stuff like gnugo.el's SGF import and export?
;; - merge with gnugo.el?

(require 'cl)

;;; Constants:

(defconst mogo-version "1.0.2 (2008-05-08)")

(defconst mogo-buffer-name "*mogo*" "Name of the mogo buffer.")
(defconst mogo-process-name "mogo" "Name of the mogo process.")

(defconst mogo-handicap2 "g7 c3")
(defconst mogo-handicap3 "g7 c3 c7")
(defconst mogo-handicap4 "g7 c3 c7 g3")
(defconst mogo-handicap5 "g7 c3 c7 g3 e5")
(defconst mogo-handicap6 "g7 c3 c7 g3 c5 g5")
(defconst mogo-handicap7 "g7 c3 c7 g3 c5 g5 e5")
(defconst mogo-handicap8 "g7 c3 c7 g3 c5 g5 e3 e7")
(defconst mogo-handicap9 "g7 c3 c7 g3 c5 g5 e3 e7 e5")


;;; Variables:
    
(defvar mogo-command "~/bin/mogo" 
  "*Command to run to start the subordinate mogo process.
To run mogo over ssh, set mogo-command to \"ssh\" and do't forget
the option \"-t\" in `mogo-command-options'")

(defvar mogo-command-options nil "*Parameters for the mogo command.
See also `mogo-command'.")

(defvar mogo-mode-hook nil "*Hook run when entering MoGo mode.")

;; devel helper: (makunbound 'mogo-mode-map)
(defvar mogo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'mogo-send-dwim)
    (define-key map (kbd "C-c ?") 'mogo-help)
    (define-key map (kbd "C-c RET") 'mogo-send-command)
    (define-key map (kbd "C-c C-s") 'mogo-show-board)
    (define-key map (kbd "C-c C-w") 'mogo-swap-sides)
    (define-key map (kbd "C-c C-g") 'mogo-genmove)
    (define-key map (kbd "C-c C-p") 'mogo-pass)
    (define-key map (kbd "C-c C-c") 'mogo-show-player-color)
    (define-key map [right] 'mogo-forward)
    (define-key map [left] 'mogo-backward)
    (define-key map (kbd "M-<up>") 'mogo-back-to-board)
    (define-key map (kbd "M-p") 'mogo-back-to-board)
    map)
  "Keymap for MoGo mode.")


(defface mogo-border-face
  '((t (:foreground "blue" :weight bold)))
  "Face for the goban border.")

(defface mogo-black-face
  '((((class color) (background light)) (:foreground "black" :weight bold))
    (((class color) (background dark)) (:foreground "green" :weight bold)))
  "Face for the black stones.")

(defface mogo-white-face
  '((((class color) (background light)) (:foreground "red" :weight bold))
    (((class color) (background dark)) (:foreground "white" :weight bold)))
  "Face for the white stones.")

;; devel helper: (makunbound 'mogo-mode-font-lock-keywords)
(defvar mogo-mode-font-lock-keywords
  '(("^ [0-9]   \\(|\\)[O@. ()]*|" . (1 'mogo-border-face))
    ("^ [0-9]   |[O@. ()]*\\(|\\)" . (1 'mogo-border-face))
    ("^     \\+---------------------------\\+" . 'mogo-border-face)
    ("O" . 'mogo-white-face)
    ("@" . 'mogo-black-face))
  "*MoGo mode font lock patterns.")

(defvar mogo-player-color "b" "The player's color: b or w.")

;;; Code:

(defun mogo ()
  "Setup *mogo* buffer and switch to `mogo-mode'."
  (interactive)
  (switch-to-buffer mogo-buffer-name)
  (mogo-mode))

(defun mogo-mode ()
  "Major mode to interact with a mogo process whose output is in the same buffer.

\\<mogo-mode-map>\\[mogo-send-dwim] sends a command or move to the mogo process.  
For details see `mogo-send-dwim'.

To (re-)display the board, for example if it has been garbled,
press \\[mogo-show-board].

Keymap:
\\{mogo-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mogo-mode)
  (setq mode-name "MoGo mode")
  (use-local-map mogo-mode-map)
  (setq font-lock-defaults '(mogo-mode-font-lock-keywords 't))
  (put 'mogo-mode 'mode-class 'special)
  (run-hooks 'mogo-mode-hook)
  (unless (mogo-get-process) (mogo-start-mogo)))
  

(defun mogo-get-process ()
  "Get mogo process by name."
  (get-process mogo-process-name))


(defun mogo-start-mogo ()
  "Start mogo process."
  (interactive)
  (insert "Starting mogo...\n\n")
  (eval (append '(start-process mogo-process-name mogo-buffer-name mogo-command)
		mogo-command-options))
  (mogo-show-board))


(defun mogo-send-dwim ()
  "Send a command or move to the mogo process.  

When the command is invoked in a grid point, then the corresponding move is 
sent to mogo and mogo is requested to answer the move. 

\"?\" and \"help\" send \"list_commands\".
\"handicap2\" - \"handicap9\" place n fixed handicap stones.
\"swap\" changes sides.

Coordinates (including \"pass\") are passed on to mogo-move.

Everything else is passed on to mogo as it is."
  (interactive)
  (cond 
   ((mogo-legal-move-on-board-p)
    (mogo-move (mogo-coordinates-from-point)))
   ((= (point-at-eol) (point-max))
    (let ((line (mogo-current-line)))
      (cond
       ((or (equal "help" line) (equal "?" line)) (mogo-help))
       ((equal "swap" line) (mogo-swap-sides))
       ((equal "handicap2" line) (mogo-handicap mogo-handicap2))
       ((equal "handicap3" line) (mogo-handicap mogo-handicap3))
       ((equal "handicap4" line) (mogo-handicap mogo-handicap4))
       ((equal "handicap5" line) (mogo-handicap mogo-handicap5))
       ((equal "handicap6" line) (mogo-handicap mogo-handicap6))
       ((equal "handicap7" line) (mogo-handicap mogo-handicap7))
       ((equal "handicap8" line) (mogo-handicap mogo-handicap8))
       ((equal "handicap9" line) (mogo-handicap mogo-handicap9))
       ((mogo-coordinates-p line) (mogo-move line))
       (t (mogo-send-command line)))))
   ;; everything else is simply ignored
   ))
    
(defun mogo-move-on-board-p ()
  "Is the cursor on a line that that displays the board?"
  (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
    (string-match "^ [0-9]   |[O@. ()]*|" line)))

(defun mogo-legal-move-on-board-p ()
  "Is the cursor on an empty intersection over the board?"
  (and (mogo-move-on-board-p) (eq ?. (char-after))))

  
(defun mogo-current-line ()
  "Get current line, strip leading \"> \" and trim the rest."
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (cmd (if (string-match "^> " line) (substring line 2) line)))
    (string-match "^\\s-*\\(.*?\\)\\s-*$" cmd)
    (match-string 1 cmd)))


(defun mogo-coordinates-from-point ()
  "Compute the coordinates from the point position."
  (let* ((xc (+ ?a (/ (- (current-column) 7) 3)))
         (x (if (= ?i xc) "j" (string xc)))
         (bol (point-at-bol))
         (y (buffer-substring (+ bol 1) (+ bol 2))))
    (concat x y)))

(defun mogo-coordinates-p (cmd)
  "Is the command string a coordinate string or \"pass\"?"
  (string-match "^\\([aAbBcCdDeEfFgGhHjJ][123456789]\\|pass\\)$" cmd))


(defun mogo-send-command (cmd)
  "Send cmd plus carriage newline."
  (interactive "sCommand: ")
  (goto-char (point-max))
  (insert "\n")
  (set-marker (process-mark (mogo-get-process)) (point))
  (insert "> ")
  (process-send-string mogo-buffer-name (concat cmd "\n")))

(defun mogo-move (coords)
  "Send move with given coordinates and ask mogo for a reply."
  (mogo-send-command (concat "play " mogo-player-color " " coords))
  (mogo-show-board)
  (mogo-genmove))

(defun mogo-genmove ()
  "Make mogo play.  Equivalent to pass. Needed when playing black
in a handicap game."
  (interactive)
  (mogo-send-command (concat "genmove " (mogo-ai-color))))

(defalias 'mogo-pass 'mogo-genmove)

(defun mogo-ai-color () 
  "The opposite color of mogo-player-color."
  (mogo-opposite-color mogo-player-color))

(defun mogo-opposite-color (color)
  "Map \"b\" to \"w\" and vice versa."
  (case (intern color)
    ('b "w") 
    ('w "b")
    (t (assert nil nil "Illegal argument: %s" color))))


(defun mogo-help ()
  "Get command list from mogo."
  (interactive)
  (mogo-send-command "list_commands"))

(defun mogo-show-board ()
  "Make mogo show the board."
  (interactive)
  (mogo-send-command "showboard"))


(defun mogo-swap-sides ()
  "Player and AI swap colors.  Also, make mogo make a move."
  (interactive)
  (setq mogo-player-color (mogo-opposite-color mogo-player-color))
  (mogo-show-player-color))

(defun mogo-show-player-color ()
  "Show the player's color."
  (interactive)
  (goto-char (point-max))
  (insert "\nYou are playing " mogo-player-color ".\n> "))


(defun mogo-handicap (coords)
  "Place handicap stones at the given coordinates."
  (mogo-send-command (concat "set_free_handicap " coords))
  (mogo-show-board)
  (mogo-show-player-color))


(defun mogo-forward ()
  "Move cursor forward by one position or one grid position."
  (interactive)
  (if (mogo-move-on-board-p) (progn (search-forward " .") (backward-char))
    (forward-char)))

(defun mogo-backward ()
  "Move cursor backward by one position or one grid position."
  (interactive)
  (if (mogo-move-on-board-p) (search-backward ".") (backward-char)))

(defun mogo-back-to-board ()
  "Move cursor backward to the previous board."
  (interactive)
  (search-backward-regexp "^ 1   |")
  (mogo-forward))

(provide 'mogo)

;;; ----------------------------------------------------------------------

;;; DIY Test Suite:

;; For the time being we use a minimalistic unit test "framework".
;; To run it evaluate: (mogo-run-tests)

(defun mogo-run-tests ()
  "Run all tests."
  (interactive)
  (mogo-test-opposite-color)
  (mogo-test-coordinates-p)
  (mogo-test-current-line)
  (mogo-test-coordinates-from-point)
  (mogo-test-move-on-board-p)
  (mogo-test-legal-move-on-board-p)
  )

(defun mogo-assert-error (form)
  "Assert that a quoted form, when evaluated, throws an error."
  (let ((ok nil))
    (condition-case nil (eval form)
      (error (setq ok 't)))
    (assert ok)))


(defun mogo-test-opposite-color ()
  "Run tests for mogo-opposite-color."
  (interactive)
  (mogo-test-opposite-color-w)
  (mogo-test-opposite-color-b)
  (mogo-test-opposite-color-assert))

(defun mogo-test-opposite-color-w ()
  (assert (equal "b" (mogo-opposite-color "w"))))

(defun mogo-test-opposite-color-b ()
  (assert (equal "w" (mogo-opposite-color "b"))))

(defun mogo-test-opposite-color-assert ()
  (mogo-assert-error '(mogo-opposite-color "B")))


(defun mogo-test-coordinates-p ()
  "Run tests for mogo-coordinates-p."
  (interactive)
  (mogo-test-coordinates-p-true)
  (mogo-test-coordinates-p-false-out-of-bounds)
  (mogo-test-coordinates-p-false-noise)
  (mogo-test-coordinates-p-false-i))

(defun mogo-test-coordinates-p-true ()
  (assert (mogo-coordinates-p "e4"))
  (assert (mogo-coordinates-p "F8"))
  (assert (mogo-coordinates-p "a1"))
  (assert (mogo-coordinates-p "j9")))

(defun mogo-test-coordinates-p-false-out-of-bounds ()
  (assert (not (mogo-coordinates-p "e0")))
  (assert (not (mogo-coordinates-p "e10")))
  (assert (not (mogo-coordinates-p "k4")))
  (assert (not (mogo-coordinates-p "z0"))))

(defun mogo-test-coordinates-p-false-noise ()
  (assert (not (mogo-coordinates-p "")))
  (assert (not (mogo-coordinates-p "5e")))
  (assert (not (mogo-coordinates-p "e5x")))
  (assert (not (mogo-coordinates-p "hello"))))

(defun mogo-test-coordinates-p-false-i ()
  (assert (not (mogo-coordinates-p "i5")))
  (assert (not (mogo-coordinates-p "I5"))))


(defun mogo-test-current-line ()
  "Run tests for mogo-current-line on the following test lines:
MAGIC-current-line
hello world
   another test !
  space on both ends  
>  line with prompt
"
  (interactive)
  (save-excursion
    (set-buffer "mogo.el")
    (goto-char (point-min))
    (search-forward "MAGIC-current-line")
    (save-excursion (mogo-test-current-line-simple))
    (save-excursion (mogo-test-current-line-trim))
    (save-excursion (mogo-test-current-line-trim-both-ends))
    (save-excursion (mogo-test-current-line-with-prompt))))

(defun mogo-test-current-line-simple ()
  (search-forward "world")
  (assert (equal "hello world" (mogo-current-line))))
  
(defun mogo-test-current-line-trim ()
  (search-forward "another")
  (assert (equal "another test !" (mogo-current-line))))
  
(defun mogo-test-current-line-trim-both-ends ()
  (search-forward "space")
  (assert (equal "space on both ends" (mogo-current-line))))
  
(defun mogo-test-current-line-with-prompt ()
  (search-forward "prompt")
  (assert (equal "line with prompt" (mogo-current-line))))
  

(defun mogo-test-coordinates-from-point ()
  "Run tests for mogo-coordinates-from-point on the following test lines:
MAGIC-board
       A  B  C  D  E  F  G  H  J  
     +---------------------------+
 9   | .  .  .  .  .  .  .  .  Y |
 8   | .  .  .  .  .  .  .  .  . |
 7   | .  .  .  .  .  .  .  .  . |
 6   | .  .  .  .  .  .  @  .  . |
 5   | .  .  .  . (O) .  .  .  . |
 4   | .  .  .  .  .  .  .  .  . |
 3   | .  .  .  .  .  .  .  .  . |
 2   | .  .  .  .  .  .  .  .  . |
 1   | X  .  .  .  .  .  .  .  . |
     +---------------------------+"
  (interactive)
  (save-excursion
    (set-buffer "mogo.el")
    (goto-char (point-min))
    (search-forward "MAGIC-board")
    (save-excursion (mogo-test-coordinates-from-point-o))
    (save-excursion (mogo-test-coordinates-from-point-at))
    (save-excursion (mogo-test-coordinates-from-point-x))
    (save-excursion (mogo-test-coordinates-from-point-y))))

(defun mogo-test-coordinates-from-point-o ()
  (search-forward "O")
  (assert (equal "e5" (mogo-coordinates-from-point))))

(defun mogo-test-coordinates-from-point-at ()
  (search-forward "@")
  (assert (equal "g6" (mogo-coordinates-from-point))))

(defun mogo-test-coordinates-from-point-x ()
  (search-forward "X")
  (assert (equal "a1" (mogo-coordinates-from-point))))

(defun mogo-test-coordinates-from-point-y ()
  (search-forward "Y")
  (assert (equal "j9" (mogo-coordinates-from-point))))


(defun mogo-test-move-on-board-p ()
  "Test mogo-move-on-board-p on the MAGIC-board board above."
  (interactive)
  (save-excursion
    (set-buffer "mogo.el")
    (goto-char (point-min))
    (search-forward "MAGIC-board")
    (save-excursion (mogo-test-move-on-board-p-o))
    (save-excursion (mogo-test-move-on-board-p-a))
    (save-excursion (mogo-test-move-on-board-p-plus))
    (save-excursion (mogo-test-move-on-board-p-plus2))))

(defun mogo-test-move-on-board-p-o ()
  (search-forward "O")
  (assert (mogo-move-on-board-p)))

(defun mogo-test-move-on-board-p-a ()
  (search-forward "A")
  (assert (not (mogo-move-on-board-p))))

(defun mogo-test-move-on-board-p-plus ()
  (search-forward "+")
  (assert (not (mogo-move-on-board-p))))

(defun mogo-test-move-on-board-p-plus2 ()
  (search-forward "O")
  (search-forward "+")
  (assert (not (mogo-move-on-board-p))))


(defun mogo-test-legal-move-on-board-p ()
  "Test mogo-legal-move-on-board-p on the MAGIC-board board above."
  (interactive)
  (save-excursion
    (set-buffer "mogo.el")
    (goto-char (point-min))
    (search-forward "MAGIC-board")
    (save-excursion (mogo-test-legal-move-on-board-p-o))
    (save-excursion (mogo-test-legal-move-on-board-p-dot))))

(defun mogo-test-legal-move-on-board-p-o ()
  (search-forward "O")
  (assert (not (mogo-legal-move-on-board-p))))

(defun mogo-test-legal-move-on-board-p-dot ()
  (search-forward "O")
  (search-forward ".")
  (backward-char)
  (assert (mogo-legal-move-on-board-p)))

;;; mogo.el ends here

;; (setq debug-on-error 't)
;; (setq debug-on-error nil)
