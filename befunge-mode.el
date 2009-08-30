;;; befunge-mode.el

;; Author: DavidEddyshaw - see http://www.emacswiki.org
;; Version: 0.99
;; Last modification: Fri Jul 19 19:21:12 GMT 2002
;; Keywords: games

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is an interpreter/debugger for  Chris Pressey's wonderful 
;; 2D Befunge programming language.
;; See http://www.catseye.mb.ca/esoteric/befunge/ for details
;; and Befunge sources.
;; Befunge just cries out to be run in an Emacs buffer; there are 
;; faster ways of running it but this mode provides a nice
;; visual environment for debugging or development :-)
;;
;; This mode is for "Classic" Befunge-93 only (IMHO much the best
;; version, on account of its beautiful Scheme-like minimalism).
;;
;; In order for the examples sinus.bf and mandel.bf to work,
;; the stack has to be of >= 32 bit signed longs, as in the full Befunge
;; specification (as deduced from the behaviour of Chris Pressey's
;; C version). To get this to work I have used the bignum implementation
;; from the calc package, which must be installed. (Thanks
;; to Deepak Goel's tip on the EmacsWiki for this.) If for some
;; reason you don't have calc installed, you'll need to replace all
;; the math-* functions by ordinary equivalents, i.e -
;;
;;         (car (math-idivmod x y)) --> (/ x y)
;;         (math-add x y)           --> (+ x y)
;;         (math-sub x y)           --> (- x y)
;;         (math-mul x y)           --> (* x y)
;;         (math-imod x y)          --> (% x y)
;;         (math-lessp x y)         --> (< x y)
;;         (math-equal x y)         --> (= x y)
;;         (math-format-number x)   --> x
;;         (math-read-number x)     --> (string-to-number x)
;;
;; Installing calc is very simple with Emacs 20; get calc-2.02f.tar.gz
;; untar and follow the instructions. It's not so trivial with
;; Emacs 21 as you need to apply a patch to the calc sources first
;; (although in fact the parts of calc needed for Befunge do not
;; require this; but it seems possible that you might want to
;; use calc for some other purposes too). 
;;
;; Incidentally, sinus.bf needs to be able to write and retrieve
;; a -1 as a character in FungeSpace, so signed characters must be 
;; emulated; see the befunge-get function below.
;; 
;; I actually wrote this interpreter partly for an iPAQ running 
;; Rainer Keuchel's amazing Emacs 20 port (http://www.rainer-keuchel.de 
;; for this and many other good things ported to wince). In the 
;; function befunge-run-finished-p there are a couple of adaptations
;; to help with running the interpreter on a PocketPC.
;; It is mainly developed using Emacs 20.7 but runs under 21.2,
;; though with one or two problems to do with the way focus behaves.
;; It doesn't work with XEmacs.
;;
;; See also the docstring for befunge-mode below.
;;
;; I'd be most grateful for any improvements, extensions &c.
;; Any Emacs gurus reading this won't need to be told that I'm no elisp
;; expert; if anybody sees how to do part or all of it better, that
;; would be wonderful.

;;; Usage:
 
;; Put this file (preferably byte-compiled) in your Emacs load-path
;; and then add something like this to .emacs :
;;
;;  (autoload 'befunge "befunge-mode" "Befunge Interpreter/Debugger" t)
;;  (autoload 'befunge-mode "befunge-mode" "Befunge mode"   t)
;;  (setq auto-mode-alist
;;       (cons (cons "\\.be?f$" 'befunge-mode) auto-mode-alist))
;; 
;; You must have calc installed for this file to work (see above).
;; I suggest you do (blink-cursor-mode 0) in Emacs 21.

;;; Code:

(require 'cl)
(require 'hscroll)
(require 'calc)
(require 'calc-ext)
(defvar calc-command-flags)

;; there is a mode for Befunge source, interpreter-mode  for the interpreter
;; and interaction-mode for the output buffer. This is "Interactive" in that
;; keyboard input is echoed to befunge-output. Lastly there is a mode
;; of its own for the befunge-stack display buffer. 

(defvar befunge-mode-map              nil)
(defvar befunge-mode-hook             nil)
(defvar befunge-interpreter-mode-map  nil)
(defvar befunge-interpreter-mode-hook nil)
(defvar befunge-interaction-mode-map  nil)
(defvar befunge-interaction-mode-hook nil)
(defvar befunge-stack-mode-map        nil)
(defvar befunge-stack-mode-hook       nil)

;; variables used by the Befunge interpreter

(defvar befunge-buffer                nil) ; Interpreter buffer
(defvar befunge-output                nil) ; Output/Interaction buffer
(defvar befunge-input                 nil) ; for 'batch' input
(defvar befunge-stack                 nil) ; Befunge stack

(defvar befunge-stack-count             0) ; number of items on stack
(defvar befunge-pc-position             1)      
(defvar befunge-direction               1) ; program counter direction
(defvar befunge-delay                   0) ; see (befunge-run)

;; these are part of the faster interpreter loop which operates
;; on the vector of strings "befunge-field" instead of on the interpreter
;; buffer itself; conversion between the two representations is 
;; managed by befunge-buffer-to-field and befunge-field-to-buffer

(defvar befunge-field          nil)     ; for befunge-fast-run
(defvar befunge-x                0)
(defvar befunge-y                0)

(defvar befunge-number-store    "")     ; see (befunge-read-number-from-stdin)

;; These are flags used for alerting the *-run functions that the
;; FungeSpace or the stack display has been altered, so that
;; appropriate processing of the changes can be carried out before running.
;; (If befunge-pc-position is 0, this similarly means that the PC
;;  position has been altered.)

(defvar befunge-edited         nil)     ; see (befunge-buffer-accept-edits)
(defvar befunge-stack-edited   nil)     ; see (befunge-stack-edit)

;; These flags show that one of the *-run functions is going; they're
;; to control menu displays

(defvar befunge-running        nil) 
(defvar befunge-running-fast   nil) 

;; These flags remember if the stack-display and output-display subviews
;; were displayed in the interpreter so that they can be put back
;; when switching back from the interaction bufferm, which never
;; has them.

(defvar befunge-output-line-shown nil) 
(defvar befunge-stack-shown       nil)
  
;; These variables belong to the windows for displaying the stack
;; and part of the output buffer while in the interpreter buffer
;; Both windows use timers to keep their contents updated
;; (this is what the *-handle variables are for).

(defvar befunge-output-line-buffer          nil)
(defvar befunge-output-line-window          nil)
(defvar befunge-output-line-updater-handle  nil)

(defvar befunge-stack-buffer                nil)
(defvar befunge-stack-window                nil)
(defvar befunge-stack-updater-handle        nil)

;; These variables are part of the fixes to get the same
;; horizontal scrolling behaviour in Emacs 21 as in Emacs 20
 
(defvar befunge-hscroll-mode                  0)
(unless (string-match "^21\\." emacs-version)
  (defvar automatic-hscrolling nil))

;; Font-locking stuff

(defun befunge-bk-highlighter (x)
  "Colours breakpoints red.

Used in befunge-interpreter-font-lock-keywords."

  (do
      ((done nil) 
       (res  nil))
      ((or (eobp) done) res)
    (cond ((get-text-property (point) 'breakpoint)
           (setq done t)
           (setq res  t)
           (set-match-data (list (point-marker) (1+ (point-marker))))
           (forward-char))
          ((eq (point) x)
           (setq res  nil)
           (setq done t))
          (t (forward-char)))))

(defun befunge-pc-highlighter (x)
  "Colours the position of the PC green.

Used in befunge-interpreter-font-lock-keywords."

  (do
      ((done nil) 
       (res  nil))
      ((or (eobp) done) res)
    (cond ((eq (point) befunge-pc-position)
           (setq done t)
           (setq res  t)
           (set-match-data (list (point-marker) (1+ (point-marker))))
           (forward-char))
          ((eq (point) x)
           (setq res  nil)
           (setq done t))
          (t (forward-char)))))

(defvar befunge-mode-font-lock-keywords 
  '(("[0-9]"                . 'font-lock-constant-face)
    ("\""                   . 'font-lock-string-face)
    ("[pg:$\\]"             . 'font-lock-keyword-face)
    ("[v^<>_|#?]"           . 'font-lock-comment-face)
    ("[*+-/%!`]"            . 'font-lock-function-name-face)
    ("@"                    . 'font-lock-warning-face)))

(defvar befunge-interpreter-font-lock-keywords 
  '((befunge-bk-highlighter . 'befunge-breakpoint-face)
    (befunge-pc-highlighter . 'befunge-pc-face)
    ("[0-9]"                . 'font-lock-constant-face)
    ("\""                   . 'font-lock-string-face)
    ("[pg:$\\]"             . 'font-lock-keyword-face)
    ("[v^<>_|#?]"           . 'font-lock-comment-face)
    ("[*+-/%!`]"            . 'font-lock-function-name-face)
    ("@"                    . 'font-lock-warning-face)))

(defface befunge-breakpoint-face
  `((((type tty) (class color))
     (:background "red" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "red"))
    (((class color) (background light))
     (:background "red"))
    (t (:background "red")))
  "Face for highlighting Befunge breakpoints.")

(defface befunge-pc-face
  `((((type tty) (class color))
     (:background "green" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "green"))
    (((class color) (background light))
     (:background "green"))
    (t (:background "green")))
  "Face for highlighting the PC position.")

;; Keymaps.

(unless befunge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (control r)] #'befunge-go)
    (define-key map [(control c) (control b)] #'befunge)
    (define-key map [(control c) (control i)] #'befunge-input-file)
    (define-key map [(control c) (control q)] #'befunge-quit)
    (setq befunge-mode-map map)))

(unless befunge-interpreter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (control r)] #'befunge-run)
    (define-key map [(control c) (control d)] #'befunge-set-delay)
    (define-key map [(control c) (control h)] #'befunge-hscroll-mode)
    (define-key map [(control c) (control t)] #'befunge-toggle-buffers)
    (define-key map [(control c) (control f)] #'befunge-load-source)
    (define-key map [(control c) (control i)] #'befunge-input-file)
    (define-key map [(control c) (control o)] #'befunge-output-line-toggle)
    (define-key map [(control c) (control s)] #'befunge-stack-toggle)
    (define-key map [(control c) (meta e)] #'befunge-buffer-edit)
    (define-key map [(control c) (meta p)] #'befunge-pc-shift)
    (define-key map [(control c) (meta w)] #'befunge-get-direction)
    (define-key map [(control c) (meta d)] #'befunge-alter-direction)
    (define-key map [(control c) (meta b)] #'befunge-set-breakpoint)
    (define-key map [(control c) (meta c)] #'befunge-clear-breakpoint)
    (define-key map [(control c) (meta a)] #'befunge-clear-all-breakpoints)
    (define-key map [(control c) (meta s)] #'befunge-single-step)
    (define-key map [(control c) (meta r)] #'befunge-resume)
    (define-key map [(control c) (meta i)] #'befunge-show-char)
    (define-key map [(control c) (control q)] #'befunge-quit)
    (setq befunge-interpreter-mode-map map)))

(unless befunge-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (control r)] #'befunge-fast-run)
    (define-key map [(control c) (control t)] #'befunge-toggle-buffers)
    (define-key map [(control c) (control q)] #'befunge-quit)
    (setq befunge-interaction-mode-map map)))

(unless befunge-stack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'befunge-stack-edit)
    (define-key map "q" #'befunge-stack-quit)
    (setq befunge-stack-mode-map map)))

;; Menu definitions.

(easy-menu-define befunge-mode-menu befunge-mode-map 
  "Befunge source buffer menu."
  '("Befunge"
    ["Run"                  befunge-go          t]
    ["Befunge Interpreter"  befunge             t]
    ["Input   File"         befunge-input-file  t]
    ["---"                  nil
     :visible
     (or
      (and befunge-output (buffer-name befunge-output))
      (and befunge-buffer (buffer-name befunge-buffer)))]
    ["Kill Interpreter"     befunge-quit        
     :visible
     (or
      (and befunge-output (buffer-name befunge-output))
      (and befunge-buffer (buffer-name befunge-buffer)))]))

;; Note that although various menu options are displayed when Befunge
;; is running, they are in fact not called via the menu callbacks!
;; Processing both of menu selections and control-key selections 
;; inside *-run is actually carried out by befunge-run-finished-p
;; below, which captures events at the top of the interpreter loop
;; and also within the character and number input functions

(easy-menu-define befunge-interpreter-mode-menu befunge-interpreter-mode-map 
  "Befunge interpreter menu."
  '("Befunge"
    ["Run"                  befunge-run
     :style    toggle
     :selected befunge-running ]
    ["Toggle Buffers"       befunge-toggle-buffers      t]
    "---"
    ["Delay"                befunge-set-delay           t]
    ["HScroll"              befunge-hscroll-mode 
     :style    toggle 
     :selected (eq 1 befunge-hscroll-mode)]
    ["Stack  View"          befunge-stack-toggle
     :style    toggle
     :selected befunge-stack-updater-handle ]
    ["Output View"          befunge-output-line-toggle
     :style    toggle
     :selected befunge-output-line-updater-handle ]
    "---"
    ["Load  Befunge"    befunge-load-source    (not befunge-running)]
    ["Input File"       befunge-input-file     (not befunge-running)]
    ("Debugging" :active (not befunge-running)
     ["Set    Breakpoint"       befunge-set-breakpoint        t]
     ["Clear  breakpoint"       befunge-clear-breakpoint      t]
     ["Clear  All breakpoints"  befunge-clear-all-breakpoints t]
     ["Single Step"             befunge-single-step           t]
     ["Resume from breakpoint"  befunge-resume                t]
     "---"
     ["Edit   FungeSpace"       befunge-buffer-edit           t]
     ["Show   Ascii at point"   befunge-show-char             t]
     "---"
     ["Show   PC Direction"    befunge-get-direction         t]
     ["Change PC Direction"    befunge-alter-direction       t]
     ["Change PC Position"     befunge-pc-shift              t])
    ["---"                  nil           :visible  (not befunge-running)]
    ["Quit"                 befunge-quit  :visible  (not befunge-running)]))

(easy-menu-define befunge-interaction-mode-menu befunge-interaction-mode-map 
  "Befunge interaction buffer menu."
  '("Befunge"
    ["Run"                befunge-fast-run        
     :style    toggle
     :selected befunge-running-fast ]
    ["Toggle Buffers"     befunge-toggle-buffers  t]
    ["---"                nil          :visible  (not befunge-running-fast)]
    ["Quit"               befunge-quit :visible  (not befunge-running-fast)]))

(easy-menu-define
 befunge-stack-mode-menu befunge-stack-mode-map 
"Befunge stack display menu."
 '("Stack"
   ["Edit  stack"            befunge-stack-edit  t]
   ["Close stack display"    befunge-stack-quit  t]))

(put 'befunge-mode             'mode-class 'special)
(put 'befunge-interpreter-mode 'mode-class 'special)
(put 'befunge-interaction-mode 'mode-class 'special)
(put 'befunge-stack-mode       'mode-class 'special)

;; Befunge functions.

;; Various utilities and supporting functions

(defsubst befunge-kill-buffer (buf-name)
  (if buf-name (kill-buffer buf-name)))

(defmacro befunge-step ()
  `(case befunge-direction
     (1 (forward-char  (if (zerop (mod (1+ (point)) 81)) -79 1)))
     (2 (forward-char  (if (>= (point) 1944) -1944 81)))
     (3 (backward-char (if (zerop (mod (1- (point)) 81)) -79 1)))
     (t (backward-char (if (< (point) 81) -1944 81)))))

(defmacro befunge-pop (n)
  `(cond ((zerop befunge-stack-count) (setq ,n 0)) 
         (t (setq befunge-stack-count (1- befunge-stack-count))
            (setq ,n (aref befunge-stack befunge-stack-count)))))

(defsubst befunge-push (n)
  (aset befunge-stack befunge-stack-count n) 
  (setq befunge-stack-count (1+ befunge-stack-count)))

(defsubst befunge-put (x y chr)
  "Put a char in the FungeSpace.

The 'disguised property is used to mark a char 9 as being really
a 10; the substitution is made because otherwise the 10 will
display as a newline and disrupt the display of the FungeSpace.
It is necessary to alter befunge-get to interpret these special
cases properly; and to add converters called by the functions
befunge-field-to-buffer and befunge-buffer-to-field to restore
the normal 10's for befunge-field"

  (let ((place (+ 1 x (* 81 y)))
        (new-char (mod chr 256)))
    (subst-char-in-region
     place
     (1+ place)
     (char-after place)
     (cond ((eq new-char 10)
            (add-text-properties    place (1+ place) '(disguised t))
            9)
           (t
            (remove-text-properties place (1+ place) '(disguised nil))
            new-char)))))

(defsubst befunge-get (x y)
  "Note that this simulates signed chars.

This is needed for sinus.bf to work, for example; that example
puts -1 with 'p' into the FungeSpace and expects to get -1 
back with 'g'."

  (let ((place (+ 1 x (* 81 y))))
  (if (get-text-property place 'disguised)
      (befunge-push 10)
    (let ((new (char-after place)))
      (if (> new 127)
          (befunge-push (- new 256))
        (befunge-push new))))))

(defsubst befunge-print-char (c)
  (cond ((eq c ?\^M) (princ "\n" befunge-output))
        ((eq c ?\^J) (princ "\n" befunge-output))
        ((< c 32)     nil)
        ((> c 127)    nil)
        (t     (princ (string c) befunge-output))))

(defun befunge-read-char ()
  "Befunge character input.

The complication here is that -read-char needs to effectively
suspend itself if the user types C-c or makes a menu selection.
See the comment for (befunge-run-finished-p)"

  (condition-case nil
      (let ((c nil))
        (cond ((and befunge-input (buffer-name befunge-input))
               (with-current-buffer befunge-input
                 (cond ((eobp) (setq c -1))
                       (t (setq c (char-after))
                          (forward-char 1)))))
              (t
               (setq c (read-char " "))
               (befunge-print-char c)))
        (if (= c ?\^M) (setq c ?\^J)) 
        (if (= c ?\^D) (setq c -1))
        (if (= c ?\^Z) (setq c -1)) ; for Windows
        (cond ((= c ?\^C) 
               (setq unread-command-events 
                     (cons ?\^C unread-command-events))
               (if (befunge-run-finished-p)
                   (throw 'done t)
                 (befunge-read-char)))
              (t  c)))
    (error (if (befunge-run-finished-p)
               (throw 'done t)
             (befunge-read-char)))))

(defun befunge-read-number-from-stdin ()
  "Befunge number input.

This function keeps track of the characters making up
the number representation in the global befunge-number-store;
this is so reading resumes correctly after switching buffers
or changing speed"

  (condition-case nil
  (let ((c (read-char " ")))
    (if (= c ?\^M) (setq c ?\^J))
    (cond ((= c ?\^C) 
           (setq unread-command-events 
                 (cons ?\^C unread-command-events))
           (if (befunge-run-finished-p)
               (throw 'done t))
           (befunge-read-number-from-stdin))
          ((or (and (>= c ?0) (<= c ?9)) (= c ?-) (= c ?+))
           (princ (string c) befunge-output)
           (setq befunge-number-store
                 (concat befunge-number-store (string c)))
           (befunge-read-number-from-stdin))
          (t 
           (princ (string c) befunge-output)
           (let ((res (string-to-number befunge-number-store)))
             (setq befunge-number-store "")
             res))))
  (error (if (befunge-run-finished-p)
             (throw 'done t)
           (befunge-read-number-from-stdin)))))

(defsubst befunge-read-number ()
  (if (and befunge-input (buffer-name befunge-input))
      (read befunge-input)  
    (befunge-read-number-from-stdin)))
    
(defun befunge-make-display-table ()
  "Makes table with char 215 for control characters.

Used by the Befunge interpreter buffer and the output buffer."

  (let ((disptab (make-display-table)))
  (let ((i 0))
    (while (< i 32)
      (or
       (eq i ?\n)
       (aset disptab i (vector 215)))
      (setq i (1+ i))))
  (let ((i 127))
    (while (< i 256)
      (aset disptab i (vector 215))
      (setq i (1+ i))))
  (setq buffer-display-table disptab)))

(defun befunge-fit-to-buffer ()
  "Fits source into FungeSpace.

Pads out with spaces to make the full Befunge 80x25 playing field,
lopping off the ends of any lines longer then 80 characters.
Used by befunge-initialize and after editing of befunge-buffer.
Because of the latter use, needs to preserve point position"

  (let ((linecount 0)
        (old-point (point)))
  (goto-char (point-max))
  (unless (eq ?\n (char-after)) (insert ?\n))
  (goto-char (point-min))
  (do ((tally 0))
      ((eobp))
    (if (eq (char-after) ?\n) 
        (progn (if (> old-point (point))
                   (setq old-point (+ old-point (- 80 tally))))
               (insert-char ?\  (- 80 tally)) 
               (setq linecount (1+ linecount))
               (setq tally -1)))
    (cond ((= tally 80) 
           (if (> old-point (point))
               (setq old-point (1- old-point)))
           (delete-char 1))             ; lops lines if > 80
          (t (setq tally (1+ tally))    ;   characters
             (forward-char))))
  (do ()
      ((> linecount 24))
    (insert-before-markers (make-string 80 ?\ ) ?\n)
    (setq linecount (1+ linecount)))
  (delete-region 2025 (point-max))      ; lops extra lines
  (goto-char old-point)))

(defun befunge-buffer-accept-edits ()
  "Redo befunge-buffer after the changes made since the last run"
  (with-current-buffer befunge-buffer
  (if befunge-edited (befunge-fit-to-buffer))
  (setq befunge-edited nil)
  (if (zerop befunge-pc-position)       ; must've been edited 
      (setq befunge-pc-position (point)))
  (goto-char befunge-pc-position)))

(defsubst befunge-divide (x y)
  "Befunge response to division by zero.

This is added to conform with the Befunge-93 spec;
none of the Befunge sources depend on it"

  (cond ((= y 0)
         (call-interactively 'befunge-read-div-by-zero))
  (t (car (math-idivmod x y)))))

(defun befunge-read-div-by-zero (res)
  (interactive "nWhat should the result of x/0  be? ")
  res)

;; Modes

;;;###autoload
(defun befunge-mode ()
  "A mode for Befunge source files.

In befunge-mode, you get a menu option to start a Befunge
interpreter. This has two buffers: the interpreter itself,
befunge-buffer, and an output/interaction buffer, 
befunge-output. Once in the interpreter you can load a different
Befunge source if you want. In the interaction buffer you have
menu options to stop or start Befunge or to switch to the
interpreter buffer. The interpreter buffer has many more
options; you can introduce a delay to slow down the visuals,
pop up windows displaying the Befunge stack or the end of the
interaction buffer, and debug by editing the FungeSpace, altering
the PC direction or position, and setting breakpoints.

The stack display window has its own menu, with the options just
 to quit or to edit the stack.

You can feed the contents of a buffer into Befunge as input
instead of typing in input interactively using the <Use File 
for Input> option in the interpreter or in the befunge-mode menu; 
thus you can easily run cat.bf or rev.bf on a file.

There's a bit of stuff for horizontal scrolling in the Interpreter
window, with a menu option to toggle  hscroll-mode; the right & left 
keys move the _view_ right or left - opposite of the text movement!
when Befunge is running. This is included for the convenience
of Pocket PC users.

The key bindings for befunge-mode are:

\\{befunge-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (use-local-map befunge-mode-map)
  (setq major-mode 'befunge-mode
        mode-name  "Befunge")
  (setq truncate-lines t)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(befunge-mode-font-lock-keywords t nil nil nil)) 
  (run-hooks 'befunge-mode-hook))

(defun befunge-interpreter-mode ()
  "A mode for the Befunge interpreter buffer

The key bindings for befunge-interpreter-mode are:

\\{befunge-interpreter-mode-map}"
  (kill-all-local-variables)
  (use-local-map befunge-interpreter-mode-map)
  (setq major-mode 'befunge-interpreter-mode
        mode-name  "Befunge Interpreter")
  (set-buffer-multibyte nil)
  (setq truncate-lines t)
  ;; For Emacs 21, set automatic-hscrolling to nil. 
  (make-local-variable 'automatic-hscrolling)
  (setq automatic-hscrolling nil)
  (make-local-variable 'befunge-hscroll-mode)
  (befunge-hscroll-mode 0)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
        '(befunge-interpreter-font-lock-keywords t nil nil nil)) 
  (befunge-make-display-table)
  (font-lock-mode 1)
  (run-hooks 'befunge-interpreter-mode-hook))

(defun befunge-interaction-mode ()
  "A mode for the Befunge interaction buffer

The key bindings for befunge-interaction-mode are:

\\{befunge-interaction-mode-map}"
  (kill-all-local-variables)
  (use-local-map befunge-interaction-mode-map)
  (setq major-mode 'befunge-interaction-mode
        mode-name  "Befunge Interaction")
  (setq truncate-lines t)
  ;; For Emacs 21, set automatic-hscrolling to nil. 
  (make-local-variable 'automatic-hscrolling)
  (setq automatic-hscrolling nil)
  (make-local-variable 'befunge-hscroll-mode)
  (befunge-hscroll-mode 1)                                 
;  (befunge-make-display-table)
  (run-hooks 'befunge-interaction-mode-hook))

(defun befunge-stack-mode ()
  "A mode for displaying the Befunge stack.

The key bindings for befunge-stack-mode are:

\\{befunge-stack-mode-map}"
  (kill-all-local-variables)
  (use-local-map befunge-stack-mode-map)
  (setq major-mode       'befunge-stack-mode
        mode-name        "Befunge Stack" 
        buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (setq truncate-lines  t)
  (befunge-hscroll-mode 1)
  (run-hooks 'befunge-stack-mode-hook))

;; Interpreter setup

;;;###autoload
(defun befunge ()
  "Create a new Befunge interpreter. 

There are two buffers, one for the Interpreter, *befunge-space*, and 
one for interaction/output, *befunge-interaction*"

  (interactive)
;  (setq befunge-input nil)
  (setq befunge-buffer (get-buffer-create "*befunge-space*"))
  (befunge-load-from-buffer)
  (setq befunge-output (get-buffer-create "*befunge-interaction*"))
  (switch-to-buffer befunge-output)
  (befunge-interaction-mode)
  (switch-to-buffer befunge-buffer)
  (befunge-interpreter-mode))

(defun befunge-go ()
  "Start Befunge immediately from befunge-mode menu"
  (interactive)
;  (setq befunge-input nil)
  (setq befunge-buffer (get-buffer-create "*befunge-space*"))
  (befunge-load-from-buffer)
  (switch-to-buffer befunge-buffer)
  (befunge-interpreter-mode)
  (setq befunge-output (get-buffer-create "*befunge-interaction*"))
  (switch-to-buffer befunge-output)
  (befunge-interaction-mode)
  (befunge-fast-run))

;; Functions called from menus

(defun befunge-run ()
  "The Befunge Interpreter loop. 

Slow version, running in the befunge-buffer itself and modifying 
the text as it goes. Good for debugging and entertaining to watch."

  (interactive)
  (setq befunge-running t)
  (befunge-buffer-accept-edits)
  (setq befunge-pc-position 0)          ; turn off display of pc position
  (font-lock-fontify-buffer)
  (befunge-stack-accept-edits)
  (setq buffer-read-only nil)
  (sit-for 0)
  (catch 'done
  (let ((temp 0)
        (temq 0)
        (temr 0))
    (unwind-protect
        (do ((c (char-after) (char-after)))
            ((or (eq c ?@)
                 (get-text-property (point) 'breakpoint) ; breakpoint 
                 (and (input-pending-p)
                      (befunge-run-finished-p))) 
             (and (eq c ?@) (befunge-kill-buffer befunge-input)))
          (hscroll-window-maybe)
          (sit-for befunge-delay)
          (case c
            ((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0) (befunge-push (- c 48)))
            (?+  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-add temq temp)))
            (?-  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-sub temq temp)))
            (?*  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-mul temq temp)))
            (?/  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (befunge-divide temq temp)))
            (?%  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-imod temq temp)))
            (?!  (befunge-pop temp)
                 (if (zerop temp) (befunge-push 1) (befunge-push 0))) 
            (?`  (befunge-pop temp)
                 (befunge-pop temq)
                 (if (or
                      (math-lessp temq temp)
                      (math-equal temq temp))
                     (befunge-push 0) (befunge-push 1)))
            (?>  (setq befunge-direction 1))
            (?<  (setq befunge-direction 3)) 
            (?^  (setq befunge-direction 4))  
            (?v  (setq befunge-direction 2))  
            (??  (setq befunge-direction (1+ (random 4))))
            (?_  (befunge-pop temp)
                 (if (zerop temp) 
                     (setq befunge-direction 1) 
                   (setq befunge-direction 3))) 
            (?|  (befunge-pop temp)
                 (if (zerop temp)
                     (setq befunge-direction 2) 
                   (setq befunge-direction 4))) 
            (34  (befunge-step) (string 34) ;"  
                 (do ((temp (char-after) (char-after)))
                     ((= 34 temp)) 
                   (sit-for befunge-delay)
                   (befunge-push temp)
                   (befunge-step)))
            (?:  (befunge-pop temp)
                 (befunge-push temp)
                 (befunge-push temp))
            (92  (befunge-pop temp)     ; \
                 (befunge-pop temq) 
                 (befunge-push temp)
                 (befunge-push temq))
            (?$  (befunge-pop temp))
            (?.  (befunge-pop temp)
                 (princ (math-format-number temp) befunge-output)
                 (princ " "  befunge-output))
            (?,  (befunge-pop temp)
                 (befunge-print-char temp))
            (?#  (befunge-step))  
            (?g  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-get temq temp))
            (?p  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-pop temr)
                 (befunge-put temq temp temr))
            (?&  (befunge-push (befunge-read-number)))
            (?~  (befunge-push (befunge-read-char))))
          (befunge-step))
      (befunge-cleanup-after-run)))))
  
(defun befunge-run-finished-p ()
  "User interrupt handler.

This function finds out if befunge(-fast)-run has been stopped,
other than by coming to @. When Befunge is running, this function
is called at the top of the interpreter loop if input is pending,
and inside the input reading functions. If the user has requested
toggling of the interpreter and interaction buffers, this function
returns t, which leads to the interpreter stopping, but it backs
up the list of unread commands so that Befunge immediately restarts
in the new buffer, giving the illusion that the run has not been
interrupted.
   
This function does the actual callbacks when a menu selection is
made while Befunge is running; it also handles ^C+key options
and needs to check that they correspond to menu options that
are actually displayed, e.g. not C-c C-o when we're in the
interaction buffer.

FIXME:
Menu-bar events don't seem to be generated in Emacs 21 within
a running Befunge if the toolbar is disabled; if you get
rid of the toolbar by setting tool-bar-lines to nil, it prevents
you selecting a menu until Befunge is stopped.
The ^C options _do_ work. I've not been able to work this out."

  (let ((event (read-event " ")))
    (cond ((eq event ?\^G)  
           t) 
          ((eq event ?\^C)    
           (let ((next-event (read-event " ")))
             (case next-event
                   (?\^D (if befunge-running
                             (call-interactively 'befunge-set-delay)) 
                         nil)
                   (?\^O (if befunge-running
                             (befunge-output-line-toggle))
                         nil)
                   (?\^S (if befunge-running
                             (befunge-stack-toggle))
                         nil)
                   (?\^R                   
                         t)
                   (?\^H (if befunge-running (befunge-hscroll-mode))
                         nil)
                   (?\^T (befunge-toggle-buffers)        
                         (setq unread-command-events 
                               (cons ?\^C (cons ?\^R unread-command-events)))
                         t))))
          ((and (listp event) 
                (or (eq (car event) 'C-down-mouse-3)
                    (eq (car event) 'C-S-down-mouse-1))) ; for WinCE
           (setq unread-command-events
                 (cons
                  (car (x-popup-menu
                        t
                        (if befunge-running-fast
                            befunge-interaction-mode-menu  
                          befunge-interpreter-mode-menu)))
                  unread-command-events))
           nil)
          ((eq event 'right)     
           (scroll-left 1)              ; 'left' sic
           nil)
          ((eq event 'kp-right)         ; for WinCE
           (scroll-left 1)              ; 'left' sic
           nil)
          ((eq event 'left)    
           (scroll-right 1)             ; 'right' sic
           nil)
          ((eq event 'kp-left)          ; for WinCE
           (scroll-right 1)             ; 'right' sic
           nil)
          ((eq event 'Run)
           t)
          ((eq event 'Delay)
           (call-interactively 'befunge-set-delay)
           nil)
          ((eq event 'HScroll)
           (befunge-hscroll-mode)
           nil)
          ((eq event 'Stack\ \ View)
           (befunge-stack-toggle)
           nil)
          ((eq event 'Output\ View)
           (befunge-output-line-toggle)
           nil)
          ((eq event 'Toggle\ Buffers)
           (befunge-toggle-buffers)
           (setq unread-command-events
                 (cons ?\^C (cons ?\^R unread-command-events)))
           t))))

(defun befunge-cleanup-after-run ()
  "Cleanup after befunge-run"
  (with-current-buffer befunge-buffer   ; might have been toggled
    (setq befunge-pc-position (point))
    (unless (get-text-property (point) 'breakpoint)
      (put-text-property (point) (1+ (point)) 'face 'befunge-pc-face))
    (font-lock-fontify-buffer)
    (setq buffer-read-only t))
  (setq befunge-running  nil))

(defun befunge-initialize ()
  "Initialization of the interpreter.
  
Always called from befunge-buffer"

  (befunge-fit-to-buffer)
  (random t)
  (if befunge-stack-buffer       (befunge-stack-quit))
  (if befunge-output-line-buffer (befunge-output-line-quit))
  (setq befunge-number-store                 ""
  befunge-edited                      nil 
  befunge-stack-edited                nil 
  befunge-running                     nil 
  befunge-running-fast                nil 
  befunge-output-line-buffer          nil
  befunge-output-line-window          nil
  befunge-output-line-shown           nil 
  befunge-stack-buffer                nil
  befunge-stack-window                nil
  befunge-stack-shown                 nil
  befunge-field       (make-vector 25 "")
  befunge-x                             0
  befunge-y                             0
  befunge-direction                     1
  befunge-stack     (make-vector 10000 0)
  befunge-stack-count                   0
  befunge-delay                         0
  befunge-pc-position                   1
  buffer-read-only                      t)
  (goto-char (point-min)))

(defun befunge-load-source (file-name)
  "Loads a source file into the interpreter.
 
Called only from the Befunge interpreter buffer." 

  (interactive "fBefunge Source File: ")
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert-file-contents file-name)
  (befunge-initialize))

(defun befunge-load-from-buffer ()
  "Loads the interpreter buffer from a source.

Called from the source buffer."

  (let ((old-buffer (current-buffer)))
  (set-buffer befunge-buffer)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert-buffer-substring old-buffer)
  (befunge-initialize)
  (set-buffer old-buffer)))

(defun befunge-alter-direction (new-direction)
  "Interactive setting of the Program Counter direction"
  (interactive "cPC direction (N,E,S or W): ")
  (case new-direction
  ((?n ?N) (setq befunge-direction 4))
  ((?e ?E) (setq befunge-direction 1))
  ((?s ?S) (setq befunge-direction 2))
  ((?w ?W) (setq befunge-direction 3))))

(defun befunge-toggle-buffers ()
  "Switches bewteen Interpreter buffer and Interaction buffer.

The complications below are to ensure that 
we remember whether the stack and/or output view buffers were
on when we switched from the interpreter buffer to the
interaction buffer so we can put them back again on switching
back." 

  (interactive)
  (cond ((eq befunge-output (current-buffer))
         (switch-to-buffer befunge-buffer)
         (if befunge-output-line-shown (befunge-output-line-display))
         (if befunge-stack-shown       (befunge-stack-display))
         (setq befunge-stack-shown       nil)
         (setq befunge-output-line-shown nil))
  (t (switch-to-buffer befunge-output)
     (if befunge-stack-updater-handle
         (progn
           (befunge-stack-quit)
           (setq befunge-stack-shown t)))
     (if befunge-output-line-updater-handle
         (progn
           (befunge-output-line-quit)
           (setq befunge-output-line-shown t)))
     (goto-char (point-max)))))

(defun befunge-set-delay (new-delay)
  "Set a delay in the interpreter." 
  (interactive "nDelay in seconds: ")
  (if (and (>= new-delay 0)
           (<= new-delay 2))
  (setq befunge-delay new-delay)
  (message "Invalid delay")))

(defun befunge-quit ()
  "Quit the Befunge interpreter."
  (interactive)
  (befunge-stack-quit)
  (befunge-output-line-quit)
  (befunge-kill-buffer befunge-input)
  (befunge-kill-buffer befunge-buffer)
  (befunge-kill-buffer befunge-output))

(defun befunge-input-file (file-name)
  (interactive "fTake Befunge input from file: ")
  (if (file-directory-p file-name)      ; i.e. zero selection
  (setq befunge-input nil)
  (progn
    (setq befunge-input (find-file-noselect file-name))
    (cond ((eq befunge-input (current-buffer))
           (setq befunge-input nil))
          (t
           (with-current-buffer befunge-input
             (goto-char (point-min))))))))

(defun befunge-buffer-edit () 
  "Allows befunge-buffer to be modified by the user until next run"
  (interactive)
  (setq buffer-read-only nil)
                                        ;  (setq overwrite-mode 'overwrite-mode-textual)
  (setq befunge-edited t))  

(defun befunge-pc-shift ()
  "Tells (befunge-run) to take new PC position from point,

by setting -pc-position to zero."

  (interactive)
  (setq befunge-pc-position 0)          ; turn off display of pc position
  (font-lock-fontify-buffer))           ; get rid of green position marker

(defun befunge-get-direction ()
  (interactive)
  (case befunge-direction
  (4 (message "north"))
  (1 (message "east"))
  (2 (message "south"))
  (3 (message "west"))))

;; Utilities for befunge-fast-run

(defmacro befunge-fast-step ()
  `(case befunge-direction
  (1 (setq befunge-x (mod (1+ befunge-x) 80)))
  (2 (setq befunge-y (mod (1+ befunge-y) 25)))
  (3 (setq befunge-x (mod (1- befunge-x) 80)))
  (t (setq befunge-y (mod (1- befunge-y) 25)))))

(defsubst befunge-fast-put (x y chr)
  (aset (aref befunge-field y) x (mod chr 256)))

(defsubst befunge-fast-get (x y) 
  (let ((new (aref (aref befunge-field y) x)))
  (if (> new 127)
      (befunge-push (- new 256))
    (befunge-push new))))

(defun befunge-disguised-to-newlines (str)
  "Converts the buffer's defused newline characters

disguised as 9's with the text property 'disguised, back into 10's"

  (do
  ((i 0 (1+ i)))
  ((= i 80))
  (if (get-text-property i 'disguised str)
      (progn
        (aset str i 10)
        (remove-text-properties i (1+ i) '(disguised nil) str)))))

(defun befunge-buffer-to-field ()
  "Initializes befunge-field from befunge-buffer.

Called from befunge-output by befunge-fast-run"

  (set-buffer befunge-buffer)
  (do ((stuff (buffer-string))
       (i 0 (1+ i)))
  ((= i 25)) 
  (aset befunge-field i (substring stuff (* i 81) (+ 80 (* i 81))))
  (befunge-disguised-to-newlines (aref befunge-field i))
  (remove-text-properties 0 80 '(face nil) (aref befunge-field i))
  (remove-text-properties 0 80 '(fontified nil) (aref befunge-field i)))  
                                        ; the last line is needed for Emacs 21
                                        ; Now only property left is 'breakpoint
  (setq befunge-x (mod (1- (point)) 81))
  (setq befunge-y (/   (1- (point)) 81))
  (set-buffer befunge-output))

(defun befunge-newlines-to-disguised (str)
  "Converts 10 characters into the form used in the buffer

which is 9's with the text property 'disguised. This is done to
stop 10's displaying as newlines and spoiling the display."

  (do
  ((i 0 (1+ i)))
  ((= i 80))
  (if (eq (aref str i) 10)
      (progn 
        (aset str i 9)
        (add-text-properties i (1+ i) '(disguised t) str)))))

(defun befunge-field-to-buffer ()
  "Reloads befunge-buffer from befunge-field. 

It is called by befunge-cleanup-after-fast-run"

  (with-current-buffer befunge-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (do ((i 0 (1+ i)))
        ((> i 24))
      (befunge-newlines-to-disguised (aref befunge-field i))
      (insert (aref befunge-field i) ?\n))
    (setq befunge-pc-position (+ 1 befunge-x (* befunge-y 81)))
    (goto-char befunge-pc-position)
    (if (get-text-property (point) 'breakpoint)
        (ding)
      (put-text-property (point) (1+ (point)) 'face 'befunge-pc-face))
    (font-lock-fontify-buffer)
    (setq buffer-read-only t)))

(defun befunge-fast-run ()
  "Version of befunge-run working on befunge-field. 

Much faster.  Always started from befunge-output buffer"

  (interactive)
  (setq befunge-running-fast t)
  (befunge-buffer-accept-edits)
  (befunge-stack-accept-edits)
  (befunge-buffer-to-field)
  (sit-for 0)
  (catch 'done
  (let ((temp 0)
        (temq 0)
        (temr 0))
    (unwind-protect
        (do ((c (aref (aref befunge-field befunge-y) befunge-x)
                (aref (aref befunge-field befunge-y) befunge-x)))
            ((or (eq c ?@)
                 (text-properties-at    ; can only be 'breakpoint
                  befunge-x             ; because we stripped out other props
                  (aref befunge-field befunge-y)) ; in -buffer-to-field
                 (and (input-pending-p)
                      (befunge-run-finished-p))) 
             (if (eq c ?@) 
                 (befunge-kill-buffer befunge-input)))
          (case c
            ((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0) (befunge-push (- c 48)))
            (?+  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-add temq temp)))
            (?-  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-sub temq temp)))
            (?*  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-mul temq temp)))
            (?/  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (befunge-divide temq temp)))
            (?%  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-imod temq temp)))
            (?!  (befunge-pop temp)
                 (if (zerop temp) (befunge-push 1) (befunge-push 0))) 
            (?`  (befunge-pop temp)
                 (befunge-pop temq)
                 (if (or
                      (math-lessp temq temp)
                      (math-equal temq temp))
                     (befunge-push 0) (befunge-push 1)))
            (?>  (setq befunge-direction 1))
            (?<  (setq befunge-direction 3)) 
            (?^  (setq befunge-direction 4))  
            (?v  (setq befunge-direction 2))  
            (??  (setq befunge-direction (1+ (random 4))))
            (?_  (befunge-pop temp)
                 (if (zerop temp) 
                     (setq befunge-direction 1) 
                   (setq befunge-direction 3))) 
            (?|  (befunge-pop temp)
                 (if (zerop temp)
                     (setq befunge-direction 2) 
                   (setq befunge-direction 4))) 
            (34  (befunge-fast-step) (string 34) ;"  
                 (do ((temp (aref (aref befunge-field befunge-y) befunge-x)
                            (aref (aref befunge-field befunge-y) befunge-x)))
                     ((= 34 temp)) 
                   (befunge-push temp)
                   (befunge-fast-step)))
            (?:  (befunge-pop temp)
                 (befunge-push temp)
                 (befunge-push temp))
            (92  (befunge-pop temp)     ; \
                 (befunge-pop temq) 
                 (befunge-push temp)
                 (befunge-push temq))
            (?$  (befunge-pop temp))
            (?.  (befunge-pop temp)
                 (princ (math-format-number temp) befunge-output)
                 (princ " "  befunge-output)
                 (sit-for 0))
            (?,  (befunge-pop temp)
                 (befunge-print-char temp)
                 (sit-for 0))
            (?#  (befunge-fast-step))  
            (?g  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-fast-get temq temp))
            (?p  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-pop temr)
                 (befunge-fast-put temq temp temr))
            (?&  (befunge-push (befunge-read-number)))
            (?~  (befunge-push (befunge-read-char))))
          (befunge-fast-step))
      (befunge-cleanup-after-fast-run)))))

(defun befunge-cleanup-after-fast-run ()
  "Cleanup after befunge-fast-run"
  (befunge-field-to-buffer)
  (setq befunge-running-fast nil))

;; Functions to make a window at the bottom of the interpreter
;; display containing the last few lines of in interaction buffer

(defun befunge-output-line-display ()
  "Display the last line of the output buffer while in the interpreter buffer"
  (interactive)
  (let ((window-min-height 5)
        (old-window (selected-window)))
  (unless (window-live-p befunge-output-line-window)
    (setq befunge-output-line-window
          (split-window-vertically (- (window-height) 5))))
  (setf (selected-window) befunge-output-line-window)
  (setq befunge-output-line-buffer
        (make-indirect-buffer befunge-output "*befunge-output*"))
  (set-window-buffer (selected-window) befunge-output-line-buffer)
  (setq truncate-lines t)
  (befunge-hscroll-mode 1)
  (unless befunge-output-line-updater-handle
    (setq befunge-output-line-updater-handle
          (run-at-time 
           nil 
           .1
           #'befunge-output-line-updater)))
  (setf (selected-window) old-window)))  

(defun befunge-output-line-updater ()
  "Update the befunge-output-line-window display."
  (let ((old-window (selected-window)))
  (setf (selected-window) befunge-output-line-window)
  (with-current-buffer befunge-output-line-buffer
    (goto-char (1- (point-max))))
  (setf (selected-window) old-window)))

(defun befunge-output-line-quit ()
  "Close the Befunge output line display."
  (interactive)
  (when befunge-output-line-updater-handle
  (cancel-timer befunge-output-line-updater-handle)
  (setq befunge-output-line-updater-handle nil))
  (befunge-kill-buffer befunge-output-line-buffer)
  (if befunge-output-line-window
  (delete-window befunge-output-line-window)))

(defun befunge-output-line-toggle ()
  "Toggle whether the output-line-display window is on."
  (interactive)
  (if befunge-output-line-updater-handle
  (befunge-output-line-quit)
  (befunge-output-line-display)))

;; Functions to make a stack display window

(defun befunge-stack-display ()
  "Display the stack."
  (interactive)
  (let ((window-min-height 2)
        (old-window (selected-window)))
  (unless (window-live-p befunge-stack-window)
    (setq befunge-stack-window
          (split-window-vertically (- (window-height) 2))))
  (setf (selected-window) befunge-stack-window)
  (setq befunge-stack-buffer (switch-to-buffer "*befunge-stack*"))
  (befunge-stack-mode)
  (unless befunge-stack-updater-handle
    (setq befunge-stack-updater-handle
          (run-at-time 
           nil 
           .1
           #'befunge-stack-updater)))
  (setf (selected-window) old-window)))

(defun befunge-stack-updater ()
  "Update the Befunge stack display."
  (with-current-buffer befunge-stack-buffer
  (let ((buffer-read-only nil))
    (setf (buffer-string) "")
    (do ((i 0 (1+ i)))
        ((= i befunge-stack-count))
      (princ (math-format-number (aref befunge-stack i))
             befunge-stack-buffer)
      (princ " "                  
             befunge-stack-buffer)))))

(defun befunge-stack-quit ()
  "Close the Befunge stack display."
  (interactive)
  (when befunge-stack-updater-handle
  (cancel-timer befunge-stack-updater-handle)
  (setq befunge-stack-updater-handle nil))
  (befunge-kill-buffer befunge-stack-buffer)
  (if befunge-stack-window
  (delete-window befunge-stack-window)))

(defun befunge-stack-toggle ()
  "Toggle whether the -stack-display window is there"
  (interactive)
  (if befunge-stack-updater-handle
  (befunge-stack-quit)
  (befunge-stack-display)))

(defun befunge-stack-edit ()
  "Enables stack editing.

Sets a flag read by befunge-stack-accept-edits at the
beginning of *-run."

  (interactive)
  (setq buffer-read-only nil)
  (cancel-timer befunge-stack-updater-handle)
  (setq befunge-stack-edited t))

(defun befunge-stack-accept-edits ()
  "Reconstitutes the stack from the user-edited stack display.

Called before Befunge runs."

  (if befunge-stack-edited
  (progn
    (setq befunge-stack-edited nil)
    (with-current-buffer befunge-stack-buffer
      (setq buffer-read-only t)
      (setq befunge-stack (make-vector 10000 0))
      (do ((i 0 (1+ i))
           (stuff (split-string (buffer-string)) (cdr stuff)))
          ((null stuff) (setq befunge-stack-count i))
        (aset befunge-stack i (math-read-number (car stuff)))))
    (setq befunge-stack-updater-handle
          (run-at-time 
           nil 
           .1
           #'befunge-stack-updater)))))

;; Functions for debugging

(defun befunge-set-breakpoint ()
  "Sets a breakpoint.

Breakpoints use text properties; the interpreters
treat a character with property 'breakpoint as a stop."

  (interactive)
  (let ((buffer-read-only nil))
  (add-text-properties (point) (1+ (point)) '(breakpoint t))))

(defun befunge-clear-breakpoint ()
  (interactive)
  (let ((buffer-read-only nil))
    (remove-text-properties (point) (1+ (point)) '(breakpoint nil))
    (font-lock-fontify-buffer)))

(defun befunge-clear-all-breakpoints ()
  (interactive)
  (let ((buffer-read-only nil))
    (remove-text-properties (point-min) (point-max) '(breakpoint nil))
    (font-lock-fontify-buffer)))

(defun befunge-single-step ()
  (interactive)
  (setq befunge-running t)
  (befunge-buffer-accept-edits)
  (befunge-stack-accept-edits)
  (setq buffer-read-only nil)
  (catch 'done
  (let ((temp 0)
        (temq 0)
        (temr 0)
        (c (char-after)))
    (unwind-protect
        (progn
          (hscroll-window-maybe)
          (sit-for befunge-delay)
          (case c
            ((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0) (befunge-push (- c 48)))
            (?@  (befunge-kill-buffer befunge-input))
            (?+  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-add temq temp)))
            (?-  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-sub temq temp)))
            (?*  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-mul temq temp)))
            (?/  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (befunge-divide temq temp)))
            (?%  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-push (math-imod temq temp)))
            (?!  (befunge-pop temp)
                 (if (zerop temp) (befunge-push 1) (befunge-push 0))) 
            (?`  (befunge-pop temp)
                 (befunge-pop temq)
                 (if (or
                      (math-lessp temq temp)
                      (math-equal temq temp))
                     (befunge-push 0) (befunge-push 1)))
            (?>  (setq befunge-direction 1))
            (?<  (setq befunge-direction 3)) 
            (?^  (setq befunge-direction 4))  
            (?v  (setq befunge-direction 2))  
            (??  (setq befunge-direction (1+ (random 4))))
            (?_  (befunge-pop temp)
                 (if (zerop temp) 
                     (setq befunge-direction 1) 
                   (setq befunge-direction 3))) 
            (?|  (befunge-pop temp)
                 (if (zerop temp)
                     (setq befunge-direction 2) 
                   (setq befunge-direction 4))) 
            (34  (befunge-step) (string 34)  ;"  
                 (do ((temp (char-after) (char-after)))
                     ((= 34 temp)) 
                   (sit-for befunge-delay)
                   (befunge-push temp)
                   (befunge-step)))
            (?:  (befunge-pop temp)
                 (befunge-push temp)
                 (befunge-push temp))
            (92  (befunge-pop temp) ; \
                 (befunge-pop temq) 
                 (befunge-push temp)
                 (befunge-push temq))
            (?$  (befunge-pop temp))
            (?.  (befunge-pop temp)
                 (princ (math-format-number temp) befunge-output)
                 (princ " "  befunge-output))
            (?,  (befunge-pop temp)
                 (befunge-print-char temp))
            (?#  (befunge-step))  
            (?g  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-get temq temp))
            (?p  (befunge-pop temp)
                 (befunge-pop temq)
                 (befunge-pop temr)
                 (befunge-put temq temp temr))
            (?&  (befunge-push (befunge-read-number)))
            (?~  (befunge-push (befunge-read-char))))
          (befunge-step))
      (befunge-cleanup-after-run)))))
 
(defun befunge-resume ()
  "Uses single-step to move off the breakpoint and then continues."
  (interactive)
  (befunge-single-step)
  (befunge-run))

(defun befunge-show-char ()
  (interactive)
  (princ "Ascii value of ")
  (princ (char-after)))

(defun befunge-hscroll-mode (&optional n)
  (interactive)
  (if (null n) 
      (setq befunge-hscroll-mode (- 1 befunge-hscroll-mode))
    (setq befunge-hscroll-mode n))
  (cond ((string-match "^21\\." emacs-version)
         (setq automatic-hscrolling (eq befunge-hscroll-mode 1)))
        (t (hscroll-mode befunge-hscroll-mode))))

(provide 'befunge)
(provide 'befunge-mode)

;;; befunge-mode.el ends here





