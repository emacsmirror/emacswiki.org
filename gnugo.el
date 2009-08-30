;;; gnugo.el
;;;
;;; Rel:standalone-gnugo-el-2-2-12
;;;
;;; Copyright (C) 1999,2000,2002,2003,2004,2005,2006 Thien-Thi Nguyen
;;; This file is part of ttn's personal elisp library, released under GNU
;;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.

;;; Description: Run GNU Go in a buffer.

;;; Commentary:

;; Playing
;; -------
;;
;; This file provides the command `gnugo' which allows you to play the game of
;; go against the external program "gnugo" (http://www.gnu.org/software/gnugo)
;; in a dedicated Emacs buffer, or to resume a game in progress.  NOTE: In
;; this file, to avoid confusion w/ elisp vars and funcs, we use the term "GNU
;; Go" to refer to the process object created by running the external program.
;;
;; At the start of a new game, you can pass additional command-line arguments
;; to GNU Go to specify level, board size, color, komi, handicap, etc.  By
;; default GNU Go plays at level 10, board size 19, color white, and zero for
;; both komi and handicap.
;;
;; To play a stone, move the cursor to the desired vertice and type `SPC' or
;; `RET'; to pass, `P' (note: uppercase); to quit, `q'; to undo one of your
;; moves (as well as a possibly intervening move by GNU Go), `u'.  To undo
;; back through an arbitrary stone that you played, place the cursor on a
;; stone and type `U' (note: uppercase).  Other keybindings are described in
;; the `gnugo-board-mode' documentation, which you may view with the command
;; `describe-mode' (normally `C-h m') in that buffer.  The buffer name shows
;; the last move and who is currently to play.  Capture counts and other info
;; are shown on the mode line immediately following the major mode name.
;;
;; While GNU Go is pondering its next move, certain commands that rely on its
;; assistence will result in a "still waiting" error.  Do not be alarmed; that
;; is normal.  When it is your turn again you may retry the command.  In the
;; meantime, you can use Emacs for other tasks, or start an entirely new game
;; with `C-u M-x gnugo'.  (NOTE: A new game will slow down all games. :-)
;;
;; If GNU Go should crash during a game the mode line will show "no process".
;; Please report the event to the GNU Go maintainers so that they can improve
;; the program.
;;
;; This code was tested with:
;; - GNU Emacs: 21.3 / 22.0.50 (from CVS)
;; - GNU Go: 3.3.15 / 3.4 / 3.6-pre3
;;
;;
;; Meta-Playing (aka Customizing)
;; ------------------------------
;;
;; Customization is presently limited to
;;   vars:                 `gnugo-program'
;;                         `gnugo-animation-string'
;;                         `gnugo-mode-line'
;;                         `gnugo-xpms'
;;   normal hooks:         `gnugo-board-mode-hook'
;;                         `gnugo-post-move-hook'
;;   and the keymap:       `gnugo-board-mode-map'
;;
;; The variable `gnugo-xpms' is a special case.  To set it you need to load
;; gnugo-xpms.el (http://www.emacswiki.org) or some other library w/ congruent
;; interface.
;;
;;
;; Meta-Meta-Playing (aka Hacking)
;; -------------------------------
;;
;; You may wish to first fix the bugs:
;; - `gnugo-toggle-dead-group' only half-complete; see docstring for details
;; - probably sgf handling is not 100% to spec
;; - subprocess should provide scoring details, gnugo.el not yet blissful
;;
;; Otherwise (we can live w/ some bugs), here are some ideas:
;; - talk GTP over the network
;; - "assist minor mode" (see gnugo-extra.el for work in progress)
;; - using assist minor mode, gnugo-v-gnugo (ibid)
;; - extract GNUGO Board mode and sgf stuff into sgf.el; make gnugo.el use it
;; - make gnugo (the external program) support query (read-only) thread
;;   so as to be able to lift "still waiting" restriction
;; - alternatively, extend GNUGO Board mode to manage another subprocess
;;   dedicated to analysis (no genmove)
;; - command `C' to add a comment to the sgf tree
;; - command `C-u =' to label a position
;; - sgf tree display, traversal (belongs in sgf.el); review game history
;;   in another buffer; branch subgame tree at arbitrary point
;; - subgame branch matriculation (maturity: child leaves the family)
;; - dribble the sgf tree
;; - "undo undo undoing"; integrate Emacs undo, GTP undo, subgame branching
;; - make buffer name format configurable (but enforce uniqueness)
;; - more tilde escapes for `gnugo-mode-line'
;; - make veneration configurable
;; - make animation more configurable; lift same-color-stones-only
;;   restriction; allow sequencing rather than lock-step; include sound
;; - [your hacking ideas here]
;;
;; Some gnugo.el hackers update http://www.emacswiki.org -- check it out!
;;
;;
;; History
;; -------
;;
;; Originally gnugo.el was written to interact w/ "gnugo --mode text" and then
;; "gnugo --mode emacs" as the subprocess.  Those versions were released as
;; 1.x, w/ x < 14.  In Novemeber 2002, gnugo.el was changed to interact w/
;; "gnugo --mode gtp", but was released as 1.14 through 1.26, even though the
;; proper versions should be 2.0.x for "--mode gtp", and 2.1.x for XPM image
;; support.  (Sorry about the confusion.)
;;
;; Thus we arrive at at the current version.  The first gnugo.el to be
;; released w/ a `gnugo-version' variable is "2.2.0".  The versioning scheme
;; is strictly monotonically increasing numbers and dots, no letters or other
;; suffixes (and none of this even/odd crap).  Here we list, aside from the
;; bugfixes, some of the notable changes introduced in each released version:
;;
;; 2.2.x -- uncluttered, letters and numbers hidden, board centered
;;          buffer name shows last move and current player
;;          mode-line customization (var `gnugo-mode-line')
;;          new commands: `=', `h', `s', `F', `R', `l', `U', `C-c C-p'
;;          program option customization (var `gnugo-program')
;;          new hooks (vars `gnugo-post-move-hook', `gnugo-board-mode-hook')
;;          multiple independent buffers/games
;;          XPM set can be changed on the fly (global and/or local)
;;          font-locking for "X", "O", "[xo]"
;;          undo by N moves, by "move pair", or by board position
;;
;;
;; History Predicted
;; -----------------
;;
;; If you are an elisp programmer, this section might not apply to you;
;; the GPL allows you to define the future of the code you receive under
;; its terms, as long as you do not deny that freedom to subsequent users.
;;
;; For users who are not elisp programmers, you can look forward to gradual
;; refinement in 2.x, splitting into gnugo.el and sgf.el in 3.x, and then
;; eventual merging into GNU Emacs for 4.x (if RMS gives it the thumbs-up).
;; If it is not accepted into Emacs at that time, a new maintainer will be
;; sought.  In any case, it will no longer be bundled w/ ttn-pers-elisp.

;;; Code:

(require 'cl)                           ; use the source luke!
(ignore-errors (require 'time-date))    ; for `time-subtract'

;;;---------------------------------------------------------------------------
;;; Political arts

(defconst gnugo-version "2.2.12"
  "Version of gnugo.el currently loaded.
Note that more than two dots in the value indicates \"pre-release\",
or \"alpha\" or \"hackers-invited-all-else-beware\"; use at your own risk!
The more dots the more courage/foolishness you must find to continue.
See source code for a history of what means what version-wise.")

;;;---------------------------------------------------------------------------
;;; Variables for the uninquisitive programmer

(defvar gnugo-program "gnugo"
  "*Command to start an external program that speaks GTP, such as \"gnugo\".
The value may also be in the form \"PROGRAM OPTIONS...\" in which case the
the command `gnugo' will prefix OPTIONS in its default offering when it
queries you for additional options.  It is an error for \"--mode\" to appear
in OPTIONS.

For more information on GTP and GNU Go, feel free to visit:
http://www.gnu.org/software/gnugo")

(defvar gnugo-board-mode-map nil
  "Keymap for GNUGO Board mode.")

(defvar gnugo-board-mode-hook nil
  "*Hook run when entering GNUGO Board mode.")

(defvar gnugo-post-move-hook nil
  "*Normal hook run after a move and before the board is refreshed.
Hook functions can prevent the call to `gnugo-refresh' by evaluating:
  (setq inhibit-gnugo-refresh t)
Initially, when `run-hooks' is called, the current buffer is the GNUGO
Board buffer of the game.  Hook functions that switch buffers must take
care not to call (directly or indirectly through some other function)
`gnugo-put' or `gnugo-get' after the switch.")

(defvar gnugo-animation-string
  (let ((jam "*#") (blink " #") (spin "-\\|/") (yada "*-*!"))
    (concat jam jam jam jam jam
            ;; "SECRET MESSAGE HERE"
            blink blink blink blink blink blink blink blink
            ;; Playing go is like fighting ignorance: when you think you have
            ;; surrounded something by knowing it very well it often turns
            ;; out that in the time you spent deepening this understanding,
            ;; other areas of ignorance have surrounded you.
            spin spin spin spin spin spin spin spin spin
            ;; Playing go is not like fighting ignorance: what one person
            ;; knows many people may come to know; knowledge does not build
            ;; solely move by move.  Wisdom, on the other hand...
            yada yada yada))
  "*String whose individual characters are used for animation.
Specifically, the `gnugo-worm-stones' and `gnugo-dragon-stones' commands
render the stones in their respective (computed) groups as the first
character in the string, then the next, and so on until the string (and/or
the viewer) is exhausted.")

(defvar gnugo-mode-line "~b ~w :~m :~u"
  "*A `mode-line-format'-compliant value for GNUGO Board mode.
If a single string, the following special escape sequences are
replaced with their associated information:
  ~b,~w  black,white captures (a number)
  ~p     current player (black or white)
  ~m     move number
  ~t     time waiting for the current move
  ~u     time taken for the Ultimate (most recent) move
The times are in seconds, or \"-\" if that information is not available.
For ~t, the value is a snapshot, use `gnugo-refresh' to update it.")

(defvar gnugo-font-lock-keywords
  '(("X" . font-lock-string-face)
    ("O" . font-lock-builtin-face))
  "*Font lock keywords for `gnugo-board-mode'.")

;;;---------------------------------------------------------------------------
;;; Variables for the inquisitive programmer

(defvar gnugo-option-history nil)

(defvar gnugo-state nil)                ; hint: C-c C-p

(eval-when-compile
  (defvar gnugo-xpms nil))

;;;---------------------------------------------------------------------------
;;; In case Emacs is lacking

(unless (fboundp 'delete-dups)
  (defun delete-dups (list)             ; from repo 2004-10-29
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

(unless (fboundp 'time-subtract)
  (defun time-subtract (t1 t2)          ; from repo 2004-10-29
    "Subtract two time values.
Return the difference in the format of a time value."
    (let ((borrow (< (cadr t1) (cadr t2))))
      (list (- (car t1) (car t2) (if borrow 1 0))
            (- (+ (if borrow 65536 0) (cadr t1)) (cadr t2))))))

(unless (fboundp 'window-edges)
  (defun window-edges (&optional window)
    (let ((cw (/ (frame-pixel-width) (frame-width)))
          (ch (/ (frame-pixel-height) (frame-height)))
          (pix-edges (window-pixel-edges window)))
      (list (/ (nth 0 pix-edges) cw)
            (/ (nth 1 pix-edges) ch)
            (/ (nth 2 pix-edges) cw)
            (/ (nth 3 pix-edges) ch)))))

;;;---------------------------------------------------------------------------
;;; Support functions

(put  'gnugo-put 'lisp-indent-function 1)
(defun gnugo-put (key value) (puthash key value gnugo-state))
(defun gnugo-get (key)       (gethash key gnugo-state))

(let ((docs "Put or get move/game/board-specific properties.
\(This docstring is shared by `gnugo-put' and `gnugo-get'.\)

There are many properties, each named by a keyword, that record and control
how gnugo.el manages each game.  Each GNUGO Board buffer has its own set
of properties, stored in the hash table `gnugo-state'.  Here we document
some of the more stable properties.  You may wish to use them as part of
a `gnugo-post-move-hook' function, for example.  Be careful to preserve
the current buffer as `gnugo-state' is made into a buffer-local variable.
NOTE: In the following, \"see foo\" actually means \"see foo source or
you may never really understand to any degree of personal satisfaction\".

 :proc -- subprocess named \"gnugo\", \"gnugo<1>\" and so forth

 :diamond -- the part of the subprocess name after \"gnugo\", may be \"\"

 :game-over -- nil until game over at which time its value is set to
               the alist `((live GROUP ...) (dead GROUP ...))'

 :sgf-collection -- after a `loadsgf' command, entire parse tree of file,
                    a simple list of one or more gametrees, updated in
                    conjunction w/ :sgf-gametree and :monkey

 :sgf-gametree -- one of the gametrees in :sgf-collection

 :monkey -- vector of three elements: LOC, a pointer to a node on the
            :sgf-gametree representing the most recently-played move
            (the next move modifies the cdr of LOC); MEM, the simple
            reverse-chronological list of previous LOC pointers; and
            COUNT, the number of moves from the beginning of the game

 :gnugo-color -- either \"black\" or \"white\"
 :user-color
 :last-mover

 :last-waiting  -- seconds and time value, respectively; see `gnugo-push-move'
 :waiting-start

 :black-captures -- these are strings since gnugo.el doesn't do anything
 :white-captures    w/ the information besides display it in the mode line;
                    gory details in functions `gnugo-propertize-board-buffer'
                    and `gnugo-merge-showboard-results' (almost more effort
                    than they are worth!)

 :display-using-images -- XPMs, to be precise; see functions `gnugo-yy',
                          `gnugo-toggle-image-display' and `gnugo-refresh',
                          as well as gnugo-xpms.el (available elsewhere)

 :all-yy -- list of 46 keywords used as the `category' text property
            (so that their plists, typically w/ property `display' or
            `do-not-display') are consulted by the Emacs display engine;
            46 = 9 places * (4 moku + 1 empty) + 1 hoshi; see functions
            `gnugo-toggle-image-display', `gnugo-yy' and `gnugo-yang'

 :lparen-ov -- overlays shuffled about to indicate the last move; only
 :rparen-ov    one is used when displaying using images

 :last-user-bpos -- board position; keep the hapless human happy

As things stabilize probably more info will be added to this docstring."))
  (put 'gnugo-put 'function-documentation docs)
  (put 'gnugo-get 'function-documentation docs))

(defun gnugo-describe-internal-properties ()
  "Pretty-print `gnugo-state' properties in another buffer.
Handle the big, slow-to-render, and/or uninteresting ones specially."
  (interactive)
  (let ((buf (current-buffer))
        (d (gnugo-get :diamond))
        acc)
    (maphash (lambda (&rest args)
               (case (car args)
                 ((:xpms :local-xpms)
                  (setcdr args (format "hash: %X (%d images)"
                                       (sxhash (cadr args))
                                       (length (cadr args)))))
                 (:sgf-collection
                  (setcdr args (length (cadr args))))
                 (:monkey
                  (let* ((value (cadr args))
                         (loc (aref value 0)))
                    (setcdr args (list
                                  (length (aref value 1))
                                  (length (cdr loc))
                                  (car loc))))))
               (setq acc (cons args acc)))
             gnugo-state)
    (switch-to-buffer (get-buffer-create
                       (format "%s*GNUGO Board Properties*"
                               (gnugo-get :diamond))))
    (erase-buffer)
    (emacs-lisp-mode)
    (setq truncate-lines t)
    (save-excursion
      (let ((standard-output (current-buffer)))
        (pp (reverse acc)))
      (goto-char (point-min))
      (let ((rx (format "overlay from \\([0-9]+\\).+\n%s\\s-+"
                        (if (string= "" d)
                            ".+\n"
                          ""))))
        (while (re-search-forward rx (point-max) t)
          (let ((pos (get-text-property (string-to-number (match-string 1))
                                        'gnugo-position buf)))
            (delete-region (+ 2 (match-beginning 0)) (point))
            (insert (format " %S" pos))))))
    (message "%d properties" (length acc))))

(defun gnugo-board-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a GNUGO Board buffer."
  (with-current-buffer (or buffer (current-buffer)) gnugo-state))

(defun gnugo-board-user-play-ok-p (&optional buffer)
  "Return non-nil if BUFFER is a GNUGO Board buffer ready for a user move."
  (with-current-buffer (or buffer (current-buffer))
    (and gnugo-state (not (gnugo-get :waitingp)))))

(defun gnugo-other (color)
  (if (string= "black" color) "white" "black"))

(defun gnugo-gate (&optional in-progress-p)
  (unless (gnugo-board-buffer-p)
    (error "Wrong buffer -- try M-x gnugo"))
  (unless (gnugo-get :proc)
    (error "No \"gnugo\" process!"))
  (when (gnugo-get :waitingp)
    (error "Not your turn yet -- please wait for \"\(%s to play\)\""
           (gnugo-get :user-color)))
  (when (and (gnugo-get :game-over) in-progress-p)
    (error "Sorry, game over")))

(defun gnugo-sentinel (proc string)
  (let ((status (process-status proc)))
    (when (or (eq status 'exit)
              (eq status 'signal))
      (let ((buf (process-buffer proc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq mode-line-process '( " [%s]"))
            (when (eq proc (gnugo-get :proc))
              (gnugo-put :proc nil))))))))

(defun gnugo-send-line (line)
  (let ((proc (gnugo-get :proc)))
    (process-send-string proc line)
    (process-send-string proc "\n")))

(defun gnugo-synchronous-send/return (message)
  "Return (TIME . STRING) where TIME is that returned by `current-time' and
STRING omits the two trailing newlines.  See also `gnugo-query'."
  (when (gnugo-get :waitingp)
    (error "Sorry, still waiting for %s to play" (gnugo-get :gnugo-color)))
  (gnugo-put :sync-return "")
  (let ((proc (gnugo-get :proc)))
    (set-process-filter
     proc (lambda (proc string)
            (let* ((so-far (gnugo-get :sync-return))
                   (start  (max 0 (- (length so-far) 2))) ; backtrack a little
                   (full   (gnugo-put :sync-return (concat so-far string))))
              (when (string-match "\n\n" full start)
                (gnugo-put :sync-return
                           (cons (current-time) (substring full 0 -2)))))))
    (gnugo-send-line message)
    (let (rv)
      ;; type change => break
      (while (stringp (setq rv (gnugo-get :sync-return)))
        (accept-process-output proc))
      (gnugo-put :sync-return "")
      rv)))

(defun gnugo-query (message-format &rest args)
  "Return cleaned-up value of a call to `gnugo-synchronous-send/return'.
The TIME portion is omitted as well as the first two characters of the STRING
portion (corresponding to the status indicator in the Go Text Protocol).  Use
this function when you are sure the command cannot fail.  The first arg is
a format string applied to the rest of the args."
  (substring (cdr (gnugo-synchronous-send/return
                   (apply 'format message-format args)))
             2))

(defun gnugo-lsquery (message-format &rest args)
  (split-string (apply 'gnugo-query message-format args)))

(defsubst gnugo-treeroot (prop)
  (cdr (assq prop (car (gnugo-get :sgf-gametree)))))

(defun gnugo-goto-pos (pos)
  "Move point to board position POS, a letter-number string."
  (goto-char (point-min))
  (forward-line (- (1+ (gnugo-treeroot :SZ))
                   (string-to-number (substring pos 1))))
  (forward-char 1)
  (forward-char (+ (if (= 32 (following-char)) 1 2)
                   (* 2 (- (let ((letter (aref pos 0)))
                             (if (> ?I letter)
                                 letter
                               (1- letter)))
                           ?A)))))

(defun gnugo-f (frag)
  (intern (format ":gnugo-%s%s-props" (gnugo-get :diamond) frag)))

(defun gnugo-yang (c)
  (case c
    (?+ 'hoshi)
    (?. 'empty)
    (?X '(bmoku . bpmoku))
    (?O '(wmoku . wpmoku))
    (t (error "badness"))))

(defun gnugo-yy (yin yang &optional momentaryp)
  (gnugo-f (format "%d-%s"
                   yin (cond ((and (consp yang) momentaryp) (cdr yang))
                             ((consp yang) (car yang))
                             (t yang)))))

(defun gnugo-toggle-image-display ()
  (unless (and (fboundp 'display-images-p) (display-images-p))
    (error "Display does not support images, sorry"))
  (require 'gnugo-xpms)
  (unless (and (boundp 'gnugo-xpms) gnugo-xpms)
    (error "Could not load `gnugo-xpms', sorry"))
  (let ((fresh (or (gnugo-get :local-xpms) gnugo-xpms)))
    (unless (eq fresh (gnugo-get :xpms))
      (gnugo-put :xpms fresh)
      (gnugo-put :all-yy nil)))
  (let* ((new (not (gnugo-get :display-using-images)))
         (act (if new 'display 'do-not-display)))
    (mapc (lambda (yy)
            (setcar (symbol-plist yy) act))
          (or (gnugo-get :all-yy)
              (gnugo-put :all-yy
                         (prog1 (mapcar (lambda (ent)
                                          (let* ((k (car ent))
                                                 (yy (gnugo-yy (cdr k) (car k))))
                                            (setplist yy `(not-yet ,(cdr ent)))
                                            yy))
                                        (gnugo-get :xpms))
                           (let ((imul (image-size (get (gnugo-yy 5 (gnugo-yang ?+))
                                                        'not-yet))))
                             (gnugo-put :w-imul (car imul))
                             (gnugo-put :h-imul (cdr imul)))))))
    (setplist (gnugo-f 'ispc) (and new
                                   ;; `(display (space :width 0))'
                                   ;; works as well, for newer emacs
                                   '(invisible t)))
    (gnugo-put :highlight-last-move-spec
               (if new
                   '((lambda (p)
                       (get (gnugo-yy (get-text-property p 'gnugo-yin)
                                      (get-text-property p 'gnugo-yang)
                                      t)
                            'display))
                     0 delete-overlay)
                 (gnugo-get :default-highlight-last-move-spec)))
    ;; a kludge to be reworked another time perhaps by another gnugo.el lover
    (dolist (group (cdr (assq 'dead (gnugo-get :game-over))))
      (mapc 'delete-overlay (cdar group))
      (setcdr (car group) nil))
    (gnugo-put :wmul (if new (gnugo-get :w-imul) 1))
    (gnugo-put :hmul (if new (gnugo-get :h-imul) 1))
    (gnugo-put :display-using-images new)))

(defun gnugo-propertize-board-buffer ()
  (erase-buffer)
  (insert (substring (cdr (gnugo-synchronous-send/return "showboard")) 3))
  (let* ((size (gnugo-treeroot :SZ))
         (size-string (number-to-string size)))
    (goto-char (point-min))
    (put-text-property (point) (1+ (point)) 'category (gnugo-f 'tpad))
    (forward-line 1)
    (put-text-property (point-min) (point) 'invisible t)
    (while (looking-at "\\s-*\\([0-9]+\\)[ ]")
      (let* ((row (match-string-no-properties 1))
             (edge (match-end 0))
             (other-edge (+ edge (* 2 size) -1))
             (top-p (string= size-string row))
             (bot-p (string= "1" row)))
        (put-text-property (point) (1- edge) 'category (gnugo-f 'lpad))
        (do ((p edge (+ 2 p)) (ival 'even (if (eq 'even ival) 'odd 'even)))
            ((< other-edge p))
          (let* ((position (format "%c%s" (aref [?A ?B ?C ?D ?E ?F ?G ?H
                                                    ?J ?K ?L ?M ?N ?O ?P
                                                    ?Q ?R ?S ?T]
                                                (ash (- p edge) -1))
                                   row))
                 (yin (let ((A-p (= edge p))
                            (Z-p (= (1- other-edge) p)))
                        (cond ((and top-p A-p) 1)
                              ((and top-p Z-p) 3)
                              ((and bot-p A-p) 7)
                              ((and bot-p Z-p) 9)
                              (top-p 2)
                              (bot-p 8)
                              (A-p 4)
                              (Z-p 6)
                              (t 5))))
                 (yang (gnugo-yang (char-after p))))
            (add-text-properties p (1+ p)
                                 `(gnugo-position
                                   ,position
                                   gnugo-yin
                                   ,yin
                                   gnugo-yang
                                   ,yang
                                   category
                                   ,(gnugo-yy yin yang)
                                   front-sticky
                                   (gnugo-position gnugo-yin))))
          (unless (= (1- other-edge) p)
            (add-text-properties (1+ p) (+ 2 p)
                                 `(category
                                   ,(gnugo-f 'ispc)
                                   rear-nonsticky
                                   t))
            (put-text-property p (+ 2 p) 'intangible ival)))
        (goto-char (+ other-edge (length row) 1))
        (when (looking-at "\\s-+\\(WH\\|BL\\).*capt.* \\([0-9]+\\).*$")
          (let ((prop (if (string= "WH" (match-string 1))
                          :white-captures
                        :black-captures))
                (beg (match-beginning 2))
                (end (match-end 2)))
            (put-text-property beg end :gnugo-cf (cons (- end beg) prop))
            (gnugo-put prop (match-string-no-properties 2))))
        (end-of-line)
        (put-text-property other-edge (point) 'category (gnugo-f 'rpad))
        (forward-char 1)))
    (put-text-property (1- (point)) (point-max) 'invisible t)))

(defun gnugo-merge-showboard-results ()
  (let ((aft (substring (cdr (gnugo-synchronous-send/return "showboard")) 3))
        (adj 1)                   ; string to buffer position adjustment
        (sync "[0-9]+ stones$")
        (bef (buffer-substring-no-properties (point-min) (point-max)))
        (bef-start 0) (bef-idx 0)
        (aft-start 0) (aft-idx 0)
        aft-sync-backtrack mis inc cut new very-strange)
    (while (numberp (setq mis (compare-strings bef bef-start nil
                                               aft aft-start nil)))
      (setq aft-sync-backtrack nil
            inc (if (> 0 mis)
                    (- (+ 1 mis))
                  (- mis 1))
            bef-idx (+ bef-start inc)
            aft-idx (+ aft-start inc)
            bef-start (if (eq bef-idx (string-match sync bef bef-idx))
                          (match-end 0)
                        (1+ bef-idx))
            aft-start (if (and (eq aft-idx (string-match sync aft aft-idx))
                               (let ((peek (1- aft-idx)))
                                 (while (not (= 32 (aref aft peek)))
                                   (setq peek (1- peek)))
                                 (setq aft-sync-backtrack (1+ peek))))
                          (match-end 0)
                        (1+ aft-idx))
            cut (+ bef-idx adj
                   (if aft-sync-backtrack
                       (- aft-sync-backtrack aft-idx)
                     0)))
      (goto-char cut)
      (if aft-sync-backtrack
          (let* ((asb aft-sync-backtrack)
                 (l-p (get-text-property cut :gnugo-cf))
                 (old-len (car l-p))
                 (capprop (cdr l-p))
                 (keep (text-properties-at cut)))
            (setq new (substring aft asb (string-match " " aft asb)))
            (plist-put keep :gnugo-cf (cons (length new) capprop))
            (gnugo-put capprop new)
            (delete-char old-len)
            (insert (apply 'propertize new keep))
            (setq adj (+ adj (- (length new) old-len))))
        (setq new (aref aft aft-idx))
        (insert-and-inherit (char-to-string new))
        (let ((yin (get-text-property cut 'gnugo-yin))
              (yang (gnugo-yang new)))
          (add-text-properties cut (1+ cut)
                               `(gnugo-yang
                                 ,yang
                                 category
                                 ,(gnugo-yy yin yang))))
        (delete-char 1)
        ;; do this last to avoid complications w/ font lock
        ;; (this also means we cannot include `intangible' in `front-sticky')
        (when (setq very-strange (get-text-property (1+ cut) 'intangible))
          (put-text-property cut (1+ cut) 'intangible very-strange))))))

(defun gnugo-move-history (&optional rsel)
  "Determine and return the game's move history.
Optional arg RSEL controls side effects and return value.
If nil, display the history in the echo area as \"(N moves)\"
followed by the space-separated list of moves.  When called
interactively with a prefix arg (i.e., RSEL is `(4)'), display
similarly, but suffix with the mover (either \":B\" or \":W\").
If RSEL is the symbol `car' return the most-recent move; if
`cadr', the next-to-most-recent move; if `count' the number of
moves thus far.

For all other values of RSEL, do nothing and return nil."
  (interactive "P")
  (let ((size (gnugo-treeroot :SZ))
        col
        (mem (aref (gnugo-get :monkey) 1))
        acc node mprop move)
    (flet ((as-pos (cc) (if (string= "tt" cc)
                            "PASS"
                          (setq col (aref cc 0))
                          (format "%c%d"
                                  (+ ?A (- (if (> ?i col) col (1+ col)) ?a))
                                  (- size (- (aref cc 1) ?a)))))
           (next (byp) (when (setq node (caar mem)
                                   mprop (or (assq :B node)
                                             (assq :W node)))
                         (setq move (as-pos (cdr mprop))
                               mem (cdr mem))
                         (push (if byp
                                   (format "%s%s" move (car mprop))
                                 move)
                               acc)))
           (tell () (message "(%d moves) %s"
                             (length acc)
                             (mapconcat 'identity (nreverse acc) " "))))
      (cond
       ((not rsel)        (while (next nil)) (tell))
       ((equal '(4) rsel) (while (next t))   (tell))
       ((eq 'car rsel)             (car (next nil)))
       ((eq 'cadr rsel) (next nil) (car (next nil)))
       ((eq 'count rsel) (aref (gnugo-get :monkey) 2))))))

(defun gnugo-note (property value &optional movep mogrifyp)
  (when mogrifyp
    (let ((sz (gnugo-treeroot :SZ)))
      (flet ((mog (pos) (if (string= "PASS" pos)
                            "tt"
                          (let* ((col (aref pos 0))
                                 (one (+ ?a (- col (if (< ?H col) 1 0) ?A)))
                                 (two (+ ?a (- sz (string-to-number
                                                   (substring pos 1))))))
                            (format "%c%c" one two)))))
        (setq value (if (consp value)
                        (mapcar 'mog value)
                      (mog value))))))
  (let* ((fruit (list (cons property value)))
         (monkey (gnugo-get :monkey))
         (loc (aref monkey 0)))
    (if movep
        (let ((mem (aref monkey 1)))
          ;; todo: do variation check/merge/branch here.
          (setcdr loc (list fruit))
          (aset monkey 0 (setq loc (cdr loc)))
          (aset monkey 1 (cons loc mem))
          (aset monkey 2 (1+ (aref monkey 2))))
      (setcdr (last (car loc)) fruit))))

(defun gnugo-close-game (end-time resign)
  (gnugo-put :game-end-time end-time)
  (let ((now (or end-time (current-time))))
    (gnugo-put :scoring-seed (logior (ash (logand (car now) 255) 16)
                                     (cadr now))))
  (gnugo-put :game-over
             (if (or (eq t resign)
                     (and (stringp resign)
                          (string-match "[BW][+][Rr]esign" resign)))
                 (flet ((ls (color) (mapcar
                                     (lambda (x)
                                       (cons (list color)
                                             (split-string x)))
                                     (split-string
                                      (gnugo-query "worm_stones %s" color)
                                      "\n"))))
                   (let ((live (append (ls "black") (ls "white"))))
                     `((live ,@live)
                       (dead))))
               (let ((dd (gnugo-query "dragon_data"))
                     (start 0) mem color ent live dead)
                 (while (string-match "\\(.+\\):\n[^ ]+[ ]+\\(black\\|white\\)\n"
                                      dd start)
                   (setq mem (match-string 1 dd)
                         color (match-string 2 dd)
                         start (match-end 0)
                         ent (cons (list color)
                                   (sort (gnugo-lsquery "dragon_stones %s" mem)
                                         'string<)))
                   (string-match "\nstatus[ ]+\\(\\(ALIVE\\)\\|[A-Z]+\\)\n"
                                 dd start)
                   (if (match-string 2 dd)
                       (push ent live)
                     (push ent dead))
                   (setq start (match-end 0)))
                 `((live ,@live)
                   (dead ,@dead))))))

(defun gnugo-push-move (userp move)
  (let* ((color (gnugo-get (if userp :user-color :gnugo-color)))
         (start (gnugo-get :waiting-start))
         (now (current-time))
         (resignp (string= "resign" move))
         (passp (string= "PASS" move))
         (head (gnugo-move-history 'car))
         (onep (and head (string= "PASS" head)))
         (donep (or resignp (and onep passp))))
    (unless passp
      (gnugo-merge-showboard-results))
    (gnugo-put :last-mover color)
    (when userp
      (gnugo-put :last-user-bpos (and (not passp) (not resignp) move)))
    (gnugo-note (if (string= "black" color) :B :W) move t (not resignp))
    (when resignp
      (gnugo-note :EV "resignation"))
    (when start
      (gnugo-put :last-waiting (cadr (time-subtract now start))))
    (when donep
      (gnugo-close-game now resignp))
    (gnugo-put :waiting-start (and (not donep) now))
    donep))

(defun gnugo-venerate (yin yang)
  (let* ((fg-yy (gnugo-yy yin yang))
         (fg-disp (or (get fg-yy 'display)
                      (get fg-yy 'do-not-display)))
         (fg-data (plist-get (cdr fg-disp) :data))
         (bg-yy (gnugo-yy yin (gnugo-yang ?.)))
         (bg-disp (or (get bg-yy 'display)
                      (get bg-yy 'do-not-display)))
         (bg-data (plist-get (cdr bg-disp) :data))
         (bop (lambda (s)
                (let* ((start 0)
                       (ncolors
                        (when (string-match "\\([0-9]+\\)\\s-+[0-9]+\"," s)
                          (setq start (match-end 0))
                          (string-to-number (match-string 1 s)))))
                  (while (and (<= 0 ncolors) (string-match ",\n" s start))
                    (setq start (match-end 0)
                          ncolors (1- ncolors)))
                  (string-match "\"" s start)
                  (match-end 0))))
         (new (copy-sequence fg-data))
         (lx (length fg-data))
         (lb (length bg-data))
         (sx (funcall bop fg-data))
         (sb (funcall bop bg-data))
         (color-key (aref new sx)))     ; blech, heuristic
    (while (< sx lx)
      (when (and (not (= color-key (aref new sx)))
                 (< 0 (random 4)))
        (aset new sx (aref bg-data sb)))
      (incf sx)
      (incf sb))
    (create-image new 'xpm t :ascent 'center)))

(defun gnugo-refresh (&optional nocache)
  "Update GNUGO Board buffer display.
While a game is in progress, parenthesize the last-played stone (no parens
for pass).  If the buffer is currently displayed in the selected window,
recenter the board (presuming there is extra space in the window).  Update
the mode line.  Lastly, move point to the last position played by the user,
if that move was not a pass.

Prefix arg NOCACHE requests complete reconstruction of the display, which may
be slow.  (This should normally be unnecessary; specify it only if the display
seems corrupted.)  NOCACHE is silently ignored when GNU Go is thinking about
its move."
  (interactive "P")
  (when (and nocache (not (gnugo-get :waitingp)))
    (gnugo-propertize-board-buffer))
  (let* ((last-mover (gnugo-get :last-mover))
         (other (gnugo-other last-mover))
         (move (gnugo-move-history 'car))
         (game-over (gnugo-get :game-over))
         window last)
    ;; last move
    (when move
      (let ((l-ov (gnugo-get :lparen-ov))
            (r-ov (gnugo-get :rparen-ov)))
        (if (member move '("PASS" "resign"))
            (mapc 'delete-overlay (list l-ov r-ov))
          (gnugo-goto-pos move)
          (let* ((p (point))
                 (hspec (gnugo-get :highlight-last-move-spec))
                 (display-value (nth 0 hspec))
                 (l-offset (nth 1 hspec))
                 (l-new-pos (+ p l-offset))
                 (r-action (nth 2 hspec)))
            (overlay-put l-ov 'display
                         (if (functionp display-value)
                             (funcall display-value p)
                           display-value))
            (move-overlay l-ov l-new-pos (1+ l-new-pos))
            (if r-action
                (funcall r-action r-ov)
              (move-overlay r-ov (+ l-new-pos 2) (+ l-new-pos 3)))))))
    ;; buffer name
    (rename-buffer (concat (gnugo-get :diamond)
                           (if game-over
                               (format "%s(game over)"
                                       (if (string= move "resign")
                                           (concat move "ation ")
                                         ""))
                             (format "%s(%s to play)"
                                     (if move (concat move " ") "")
                                     other))))
    ;; pall of death
    (when game-over
      (let ((live (cdr (assq 'live game-over)))
            (dead (cdr (assq 'dead game-over)))
            p pall)
        (unless (eq game-over (get-text-property 1 'game-over))
          (dolist (group (append live dead))
            (dolist (pos (cdr group))
              (gnugo-goto-pos pos)
              (setq p (point))
              (put-text-property p (1+ p) 'group group)))
          (put-text-property 1 2 'game-over game-over))
        (dolist (group live)
          (when (setq pall (cdar group))
            (mapc 'delete-overlay pall)
            (setcdr (car group) nil)))
        (dolist (group dead)
          (unless (cdar group)
            (let (ov pall c (color (caar group)))
              (setq c (if (string= "black" color) "x" "o"))
              (dolist (pos (cdr group))
                (gnugo-goto-pos pos)
                (setq p (point) ov (make-overlay p (1+ p)))
                (overlay-put
                 ov 'display
                 (if (gnugo-get :display-using-images)
                     ;; respect the dead individually; it takes more time
                     ;; but that's not a problem (for them)
                     (gnugo-venerate (get-text-property p 'gnugo-yin)
                                     (gnugo-yang (aref (upcase c) 0)))
                   (propertize c 'face 'font-lock-warning-face)))
                (push ov pall))
              (setcdr (car group) pall))))))
    ;; window update
    (when (setq window (get-buffer-window (current-buffer)))
      (let* ((size (gnugo-treeroot :SZ))
             (h (ash (- (window-height window)
                        (round (* size (gnugo-get :hmul)))
                        1)
                     -1))
             (edges (window-edges window))
             (right-w-edge (nth 2 edges))
             (avail-width (- right-w-edge (nth 0 edges)))
             (w (/ (- avail-width
                      (+ (* size (gnugo-get :wmul))
                         (if (symbol-plist (gnugo-f 'ispc))
                             0
                           (1- size)))
                      2)
                   2.0)))
        (dolist (pair `((tpad . ,(if (and h (< 0 h))
                                     `(display ,(make-string h 10))
                                   '(invisible t)))
                        (lpad . ,(if (< 0 w)
                                     `(display (space :align-to ,w))
                                   '(invisible t)))
                        (rpad . (display
                                 (space :align-to ,(1- avail-width))))))
          (setplist (gnugo-f (car pair)) (cdr pair)))))
    ;; mode line update
    (let ((cur (gnugo-get :mode-line)))
      (unless (equal cur gnugo-mode-line)
        (setq cur gnugo-mode-line)
        (gnugo-put :mode-line cur)
        (gnugo-put :mode-line-form
                   (cond ((stringp cur)
                          (setq cur (copy-sequence cur))
                          (let (acc cut c)
                            (while (setq cut (string-match "~[bwpmtu]" cur))
                              (aset cur cut ?%)
                              (setq cut (1+ cut) c (aref cur cut))
                              (aset cur cut ?s)
                              (push
                               `(,(intern (format "squig-%c" c))
                                 ,(case c
                                    (?b '(or (gnugo-get :black-captures) 0))
                                    (?w '(or (gnugo-get :white-captures) 0))
                                    (?p '(gnugo-other (gnugo-get :last-mover)))
                                    (?t '(let ((ws (gnugo-get :waiting-start)))
                                           (if ws
                                               (cadr (time-since ws))
                                             "-")))
                                    (?u '(or (gnugo-get :last-waiting) "-"))
                                    (?m '(gnugo-move-history 'count))))
                               acc))
                            `(let ,(delete-dups (copy-sequence acc))
                               (format ,cur ,@(reverse (mapcar 'car acc))))))
                         (t cur))))
      (let ((form (gnugo-get :mode-line-form)))
        (setq mode-line-process
              (and form
                   ;; this dynamicism is nice but excessive in its wantonness
                   ;;- `(" [" (:eval ,form) "]")
                   ;; this dynamicism is ok because the user triggers it
                   (list (format " [%s]" (eval form))))))
      (force-mode-line-update))
    ;; last user move
    (when (setq last (gnugo-get :last-user-bpos))
      (gnugo-goto-pos last))))

;;;---------------------------------------------------------------------------
;;; Game play actions

(defun gnugo-get-move-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let* ((so-far (gnugo-get :get-move-string))
           (full   (gnugo-put :get-move-string (concat so-far string))))
      (when (string-match "^= \\(.+\\)\n\n" full)
        (let ((pos-or-pass (match-string 1 full)))
          (gnugo-put :get-move-string nil)
          (gnugo-put :waitingp nil)
          (gnugo-push-move nil pos-or-pass)
          (let ((buf (current-buffer)))
            (let (inhibit-gnugo-refresh)
              (run-hooks 'gnugo-post-move-hook)
              (unless inhibit-gnugo-refresh
                (with-current-buffer buf
                  (gnugo-refresh))))))))))

(defun gnugo-get-move (color)
  (gnugo-put :waitingp t)
  (set-process-filter (gnugo-get :proc) 'gnugo-get-move-insertion-filter)
  (gnugo-send-line (concat "genmove " color))
  (accept-process-output))

(defun gnugo-cleanup ()
  (when (gnugo-board-buffer-p)
    (unless (= 0 (buffer-size))
      (message "Thank you for playing GNU Go."))
    (mapc (lambda (sym)
            (setplist sym nil)    ; "...is next to fordliness." --Huxley
            (unintern sym))
          (append (gnugo-get :all-yy)
                  (mapcar 'gnugo-f
                          '(anim
                            tpad
                            lpad
                            rpad
                            ispc))))
    (setq gnugo-state nil)))

(defun gnugo-position ()
  (or (get-text-property (point) 'gnugo-position)
      (error "Not a proper position point")))

(defun gnugo-move ()
  "Make a move on the GNUGO Board buffer.
The position is computed from current point.
Signal error if done out-of-turn or if game-over.
To start a game try M-x gnugo."
  (interactive)
  (gnugo-gate t)
  (let* ((buf (current-buffer))
         (pos (gnugo-position))
         (move (format "play %s %s" (gnugo-get :user-color) pos))
         (accept (cdr (gnugo-synchronous-send/return move))))
    (unless (= ?= (aref accept 0))
      (error accept))
    (gnugo-push-move t pos)         ; value always nil for non-pass move
    (let (inhibit-gnugo-refresh)
      (run-hooks 'gnugo-post-move-hook)
      (unless inhibit-gnugo-refresh
        (with-current-buffer buf
          (gnugo-refresh))))
    (with-current-buffer buf
      (gnugo-get-move (gnugo-get :gnugo-color)))))

(defun gnugo-mouse-move (e)
  "Do `gnugo-move' at mouse location."
  (interactive "@e")
  (mouse-set-point e)
  (when (looking-at "[.+]")
    (gnugo-move)))

(defun gnugo-pass ()
  "Make a pass on the GNUGO Board buffer.
Signal error if done out-of-turn or if game-over.
To start a game try M-x gnugo."
  (interactive)
  (gnugo-gate t)
  (let ((accept (cdr (gnugo-synchronous-send/return
                      (format "play %s PASS" (gnugo-get :user-color))))))
    (unless (= ?= (aref accept 0))
      (error accept)))
  (let ((donep (gnugo-push-move t "PASS"))
        (buf (current-buffer)))
    (let (inhibit-gnugo-refresh)
      (run-hooks 'gnugo-post-move-hook)
      (unless inhibit-gnugo-refresh
        (with-current-buffer buf
          (gnugo-refresh))))
    (unless donep
      (with-current-buffer buf
        (gnugo-get-move (gnugo-get :gnugo-color))))))

(defun gnugo-mouse-pass (e)
  "Do `gnugo-pass' at mouse location."
  (interactive "@e")
  (mouse-set-point e)
  (gnugo-pass))

(defun gnugo-resign ()
  (interactive)
  (gnugo-gate t)
  (if (not (y-or-n-p "Resign? "))
      (message "(not resigning)")
    (gnugo-push-move t "resign")
    (gnugo-refresh)))

(defun gnugo-animate-group (command)
  (message "Computing %s ..." command)
  (let* ((pos (gnugo-position))
         (stones (if (memq (char-after) '(?X ?O))
                     (gnugo-lsquery "%s %s" command pos)
                   (error "No stone at %s" pos))))
    (message "Computing %s ... %s in group." command (length stones))
    (setplist (gnugo-f 'anim) nil)
    (let* ((spec (let ((spec
                        ;; `(split-string gnugo-animation-string "" t)'
                        ;; works as well, for newer emacs versions
                        (delete "" (split-string gnugo-animation-string ""))))
                   (cond ((gnugo-get :display-using-images)
                          (let* ((yin (get-text-property (point) 'gnugo-yin))
                                 (yang (gnugo-yang (char-after)))
                                 (up (get (gnugo-yy yin yang t) 'display))
                                 (dn (get (gnugo-yy yin yang) 'display))
                                 flip-flop)
                            (mapcar (lambda (c)
                                      (if (setq flip-flop (not flip-flop))
                                          dn up))
                                    (mapcar 'string-to-char spec))))
                         (t spec))))
           (cell (list spec))
           (ovs (save-excursion
                  (mapcar (lambda (pos)
                            (gnugo-goto-pos pos)
                            (let* ((p (point))
                                   (ov (make-overlay p (1+ p))))
                              (overlay-put ov 'category (gnugo-f 'anim))
                              (overlay-put ov 'priority most-positive-fixnum)
                              ov))
                          stones))))
      (setplist (gnugo-f 'anim) (cons 'display cell))
      (while (and (cdr spec)          ; let last linger lest levity lost
                  (sit-for 0.08675309)) ; jenny jenny i got your number...
        (setcar cell (setq spec (cdr spec)))
        (set-buffer-modified-p t))
      (sit-for 5)
      (mapc 'delete-overlay ovs)
      t)))

(defun gnugo-display-group-data (command buffer-name)
  (message "Computing %s ..." command)
  (let ((data (cdr (gnugo-synchronous-send/return
                    (format "%s %s" command (gnugo-position))))))
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert data))
  (message "Computing %s ... done." command))

(defun gnugo-worm-stones ()
  "In the GNUGO Board buffer, animate \"worm\" at current position.
Signal error if done out-of-turn or if game-over.
See variable `gnugo-animation-string' for customization."
  (interactive)
  (gnugo-gate)
  (gnugo-animate-group "worm_stones"))

(defun gnugo-worm-data ()
  "Display in another buffer data from \"worm\" at current position.
Signal error if done out-of-turn or if game-over."
  (interactive)
  (gnugo-gate)
  (gnugo-display-group-data "worm_data" "*gnugo worm data*"))

(defun gnugo-dragon-stones ()
  "In the GNUGO Board buffer, animate \"dragon\" at current position.
Signal error if done out-of-turn or if game-over.
See variable `gnugo-animation-string' for customization."
  (interactive)
  (gnugo-gate)
  (gnugo-animate-group "dragon_stones"))

(defun gnugo-dragon-data ()
  "Display in another buffer data from \"dragon\" at current position.
Signal error if done out-of-turn or if game-over."
  (interactive)
  (gnugo-gate)
  (gnugo-display-group-data "dragon_data" "*gnugo dragon data*"))

(defun gnugo-toggle-dead-group ()
  "In a GNUGO Board buffer, during game-over, toggle a group as dead.
The group is selected from current position (point).  Signal error if
not in game-over or if there is no group at that position.

In the context of GNU Go, a group is called a \"dragon\" and may be
composed of more than one \"worm\" (set of directly-connected stones).
It is unclear to the gnugo.el author whether or not GNU Go supports
 - considering worms as groups in their own right; and
 - toggling group aliveness via GTP.
Due to these uncertainties, this command is only half complete; the
changes you may see in Emacs are not propagated to the gnugo subprocess.
Thus, GTP commands like `final_score' may give unexpected results.

If you are able to expose via GTP `change_dragon_status' in utils.c,
you may consider modifying the `gnugo-toggle-dead-group' source code
to enable full functionality."
  (interactive)
  (let ((game-over (or (gnugo-get :game-over)
                       (error "Sorry, game still in play")))
        (group (or (get-text-property (point) 'group)
                   (error "No stone at that position")))
        (now (current-time)))
    (gnugo-put :scoring-seed (logior (ash (logand (car now) 255) 16)
                                     (cadr now)))
    (let ((live (assq 'live game-over))
          (dead (assq 'dead game-over))
          bef now)
      (if (memq group live)
          (setq bef live now dead)
        (setq bef dead now live))
      (setcdr bef (delq group (cdr bef)))
      (setcdr now (cons group (cdr now)))
      ;; disabled permanently -- too wrong
      (when nil
        (flet ((populate (group)
                         (let ((color (caar group)))
                           (dolist (stone (cdr group))
                             (gnugo-query "play %s %s" color stone)))))
          (if (eq now live)
              (populate group)
            ;; drastic (and wrong -- clobbers capture info, etc)
            (gnugo-query "clear_board")
            (mapc 'populate (cdr live)))))
      ;; here is the desired interface (to be enabled Some Day)
      (when nil
        (gnugo-query "change_dragon_status %s %s"
                     (cadr group) (if (eq now live)
                                      'alive
                                    'dead)))))
  (save-excursion
    (gnugo-refresh)))

(defun gnugo-estimate-score ()
  "Display estimated score of a game of GNU Go.
Output includes number of stones on the board and number of stones
captured by each player, and the estimate of who has the advantage (and
by how many stones)."
  (interactive)
  (message "Est.score ...")
  (let ((black (length (gnugo-lsquery "list_stones black")))
        (white (length (gnugo-lsquery "list_stones white")))
        (black-captures (gnugo-query "captures black"))
        (white-captures (gnugo-query "captures white"))
        (est (gnugo-query "estimate_score")))
    ;; might as well update this
    (gnugo-put :black-captures black-captures)
    (gnugo-put :white-captures white-captures)
    (message "Est.score ... B %s %s | W %s %s | %s"
             black black-captures white white-captures est)))

(defun gnugo-write-sgf-file (filename)
  "Save the game history to FILENAME (even if unfinished).
If FILENAME already exists, Emacs confirms that you wish to overwrite it."
  (interactive "FWrite game as SGF file: ")
  (when (and (file-exists-p filename)
             (not (y-or-n-p "File exists. Continue? ")))
    (error "Not writing %s" filename))
  (gnugo/sgf-write-file (gnugo-get :sgf-collection) filename))

(defun gnugo-read-sgf-file (filename)
  "Load the first game tree from FILENAME, a file in SGF format."
  (interactive "fSGF file to load: ")
  (when (file-directory-p filename)
    (error "Cannot load a directory (try a filename with extension .sgf)"))
  (let (ans play wait samep coll)
    ;; problem: requiring GTP `loadsgf' complicates network subproc support;
    ;; todo: skip it altogether when confident about `gnugo/sgf-read-file'
    (unless (= ?= (aref (setq ans (cdr (gnugo-synchronous-send/return
                                        (format "loadsgf %s"
                                                (expand-file-name filename)))))
                        0))
      (error ans))
    (setq play (substring ans 2)
          wait (gnugo-other play)
          samep (string= (gnugo-get :user-color) play))
    (gnugo-put :last-mover wait)
    (unless samep
      (gnugo-put :gnugo-color wait)
      (gnugo-put :user-color play))
    (gnugo-put :sgf-collection (setq coll (gnugo/sgf-read-file filename)))
    (gnugo-put :sgf-gametree
               (nth (let ((n (length coll)))
                      ;; This is better:
                      ;; (if (= 1 n)
                      ;;     0
                      ;;   (let* ((q (format "Which game? (1-%d)" n))
                      ;;          (choice (1- (read-number q 1))))
                      ;;     (if (and (< -1 choice) (< choice n))
                      ;;         choice
                      ;;       (message "(Selecting the first game)")
                      ;;       0)))
                      ;; but this is what we use (for now) to accomodate
                      ;; (aka faithfully mimic) GTP `loadsgf' limitations:
                      (unless (= 1 n)
                        (message "(Selecting the first game)"))
                      0)
                    coll))
    (let* ((tree (gnugo-get :sgf-gametree))
           (loc tree)
           (count 0)
           mem node play game-over)
      (while (setq node (car loc))
        (when (setq play (or (assq :B node)
                             (assq :W node)))
          ;; SGF[4] allows "" to mean PASS.  For now,
          ;; we normalize here instead of at the lower layer.
          (when (string= "" (cdr play))
            (setcdr play "tt"))
          (incf count)
          (push loc mem))
        (setq loc (cdr loc)))
      (gnugo-put :game-over
                 (setq game-over
                       (or (cdr (assq :RE (car tree)))
                           (and (cdr mem)
                                (equal '("tt" "tt")
                                       (let ((order (if (string= "black" wait)
                                                        '(:B :W)
                                                      '(:W :B))))
                                         (mapcar (lambda (pk)
                                                   (cdr (assq (funcall pk order)
                                                              (car (funcall pk mem)))))
                                                 '(car cadr))))
                                'two-passes))))
      (gnugo-put :monkey
                 (vector (or (car mem) (gnugo-get :sgf-gametree))
                         mem
                         count))
      (when (and game-over
                 ;; (maybe) todo: user var to inhibit (can be slow)
                 t)
        (gnugo-close-game nil game-over)))
    (gnugo-refresh t)
    (message "GNU Go %splays as %s, you as %s (%s)"
             (if samep "" "now ")
             wait play (if samep
                           "as before"
                         "NOTE: this is a switch!"))))

(defun gnugo-magic-undo (spec &optional noalt)
  "Undo moves on the GNUGO Board, based on SPEC, a string or number.
If SPEC is a string in the form of a board position (e.g., \"T19\"),
check that the position is occupied by a stone of the user's color,
and if so, remove moves from the history until that position is clear.
If SPEC is a positive number, remove exactly that many moves from the
history, signaling an error if the history is exhausted before finishing.
If SPEC is not recognized, signal \"bad spec\" error.

Refresh the board for each move undone.  If (in the case where SPEC is
a number) after finishing, the color to play is not the user's color,
schedule a move by GNU Go.

After undoing the move(s), schedule a move by GNU Go if it is GNU Go's
turn to play.  Optional second arg NOALT non-nil inhibits this."
  (gnugo-gate)
  (let* ((n 0)
         (monkey (gnugo-get :monkey))
         (mem (aref monkey 1))
         (count (aref monkey 2))
         done ans)
    (cond ((and (numberp spec) (< 0 spec))
           (setq n spec done (lambda () (= 0 n))))
          ((string-match "^[a-z]" spec)
           (let ((pos (upcase spec)))
             (setq done `(lambda ()
                           (gnugo-goto-pos ,pos)
                           (memq (char-after) '(?. ?+))))
             (when (funcall done)
               (error "%s already clear" pos))
             (let ((u (gnugo-get :user-color)))
               (when (= (save-excursion
                          (gnugo-goto-pos pos)
                          (char-after))
                        (if (string= "black" u)
                            ?O
                          ?X))
                 (error "%s not occupied by %s" pos u)))))
          (t (error "bad spec: %S" spec)))
    (when (gnugo-get :game-over)
      (gnugo-put :game-over nil))
    (while (not (funcall done))
      (setq ans (cdr (gnugo-synchronous-send/return "undo")))
      (unless (= ?= (aref ans 0))
        (error ans))
      (aset monkey 2 (decf count))
      (aset monkey 1 (setq mem (cdr mem)))
      (aset monkey 0 (or (car mem) (gnugo-get :sgf-gametree)))
      (gnugo-put :last-mover (gnugo-other (gnugo-get :last-mover)))
      (gnugo-merge-showboard-results)   ; all
      (gnugo-refresh)                   ; this
      (decf n)                          ; is
      (sit-for 0)))                     ; eye candy
  (let* ((ulastp (string= (gnugo-get :last-mover) (gnugo-get :user-color)))

         (ubpos (gnugo-move-history (if ulastp 'car 'cadr))))
    (gnugo-put :last-user-bpos (if (and ubpos (not (string= "PASS" ubpos)))
                                   ubpos
                                 (gnugo-get :center-position)))
    (gnugo-refresh t)
    ;; preserve restricted-functionality semantics (todo: remove restriction)
    (setcdr (aref (gnugo-get :monkey) 0) nil)
    (when (and ulastp (not noalt))
      (gnugo-get-move (gnugo-get :gnugo-color)))))

(defun gnugo-undo-one-move ()
  "Undo exactly one move (perhaps GNU Go's, perhaps yours).
Do not schedule a move by GNU Go even if it is GNU Go's turn to play.
See also `gnugo-undo-two-moves'."
  (interactive)
  (gnugo-gate)
  (gnugo-magic-undo 1 t))

(defun gnugo-undo-two-moves ()
  "Undo a pair of moves (GNU Go's and yours).
However, if you are the last mover, undo only one move.
Regardless, after undoing, it is your turn to play again."
  (interactive)
  (gnugo-gate)
  (gnugo-magic-undo (if (string= (gnugo-get :user-color)
                                 (gnugo-get :last-mover))
                        1
                      2)))

(defun gnugo-display-final-score ()
  "Display final score and other info in another buffer (when game over).
If the game is still ongoing, Emacs asks if you wish to stop play (by
making sure two \"pass\" moves are played consecutively, if necessary).
Also, add the `:RE' SGF property to the root node of the game tree."
  (interactive)
  (unless (or (gnugo-get :game-over)
              (and (not (gnugo-get :waitingp))
                   (y-or-n-p "Game still in play. Stop play now? ")))
    (error "Sorry, game still in play"))
  (unless (gnugo-get :game-over)
    (flet ((pass (userp)
                 (message "Playing PASS for %s ..."
                          (gnugo-get (if userp :user-color :gnugo-color)))
                 (sit-for 1)
                 (gnugo-push-move userp "PASS")))
      (unless (pass t)
        (pass nil)))
    (gnugo-refresh)
    (sit-for 3))
  (let ((b=  "   Black = ")
        (w=  "   White = ")
        (res (let* ((node (car (aref (gnugo-get :monkey) 0)))
                    (event (and node (cdr (assq :EV node)))))
               (and event (string= "resignation" event)
                    (if (assq :B node) "black" "white"))))
        blurb result)
    (if res
        (setq blurb (list
                     (format "%s wins.\n"
                             (substring (if (= ?b (aref res 0)) w= b=)
                                        3 8))
                     "The game is over.\n"
                     (format "Resignation by %s.\n" res))
              result (concat (upcase (substring (gnugo-other res) 0 1))
                             "+Resign"))
      (message "Computing final score ...")
      (let* ((live   (cdr (assq 'live (gnugo-get :game-over))))
             (dead   (cdr (assq 'dead (gnugo-get :game-over))))
             (seed   (gnugo-get :scoring-seed))
             (terr-q (format "final_status_list %%s_territory %d" seed))
             (terr   "territory")
             (capt   "captures")
             (b-terr (length (gnugo-lsquery terr-q "black")))
             (w-terr (length (gnugo-lsquery terr-q "white")))
             (b-capt (string-to-number (gnugo-get :black-captures)))
             (w-capt (string-to-number (gnugo-get :white-captures)))
             (komi   (gnugo-treeroot :KM)))
        (setq blurb (list "The game is over.  Final score:\n")
              result (gnugo-query "final_score %d" seed))
        (cond ((string= "Chinese" (gnugo-treeroot :RU))
               (dolist (group live)
                 (let ((count (length (cdr group))))
                   (if (string= "black" (caar group))
                       (setq b-terr (+ b-terr count))
                     (setq w-terr (+ w-terr count)))))
               (dolist (group dead)
                 (let* ((color (caar group))
                        (count (length (cdr group))))
                   (if (string= "black" color)
                       (setq w-terr (+ count w-terr))
                     (setq b-terr (+ count b-terr)))))
               (push (format "%s%d %s = %3.1f\n" b= b-terr terr b-terr) blurb)
               (push (format "%s%d %s + %3.1f %s = %3.1f\n" w=
                             w-terr terr komi 'komi (+ w-terr komi))
                     blurb))
              (t
               (dolist (group dead)
                 (let* ((color (caar group))
                        (adjust (* 2 (length (cdr group)))))
                   (if (string= "black" color)
                       (setq w-terr (+ adjust w-terr))
                     (setq b-terr (+ adjust b-terr)))))
               (push (format "%s%d %s + %s %s = %3.1f\n" b=
                             b-terr terr
                             b-capt capt
                             (+ b-terr b-capt))
                     blurb)
               (push (format "%s%d %s + %s %s + %3.1f %s = %3.1f\n" w=
                             w-terr terr
                             w-capt capt
                             komi 'komi
                             (+ w-terr w-capt komi))
                     blurb)))
        (push (if (string= "0" result)
                  "The game is a draw.\n"
                (format "%s wins by %s.\n"
                        (substring (if (= ?B (aref result 0)) b= w=) 3 8)
                        (substring result 2)))
              blurb)
        (message "Computing final score ... done")))
    ;; extra info
    (when (gnugo-get :game-end-time)
      (push "\n" blurb)
      (dolist (spec '(("Game start" . :game-start-time)
                      ("       end" . :game-end-time)))
        (push (format-time-string
               (concat (car spec) ": %Y-%m-%d %H:%M:%S %z\n")
               (gnugo-get (cdr spec)))
              blurb)))
    (setq blurb (apply 'concat (nreverse blurb)))
    (let* ((root (car (gnugo-get :sgf-gametree)))
           (cur (assq :RE root)))
      (if cur
          (setcdr cur result)
        (setcdr (last root) (list (cons :RE result)))))
    (switch-to-buffer (format "%s*GNUGO Final Score*" (gnugo-get :diamond)))
    (erase-buffer)
    (insert blurb)))

;;;---------------------------------------------------------------------------
;;; Command properties and gnugo-command

;; GTP commands entered by the user are never issued directly to GNU Go;
;; instead, their behavior and output are controlled by the property
;; `:gnugo-gtp-command-spec' hung off of each (interned/symbolic) command.
;; The value of this property is a sub-plist, w/ sub-properties as follows:
;;
;; :full -- completely interpret the command string; the value is a
;;          func that takes the list of words derived from splitting the
;;          command string (minus the command) and handles everything.
;;
;; :output -- either a keyword specifying the preferred output method:
;;              :message -- show output in minibuffer
;;              :discard -- sometimes you just don't care;
;;            or a function that takes one arg, the output string, and
;;            handles it completely.   default is to switch to buffer
;;            "*gnugo command output*" if the output has a newline,
;;            otherwise use `message'.
;;
;; :post-hook -- normal hook run after output processing (at the very end).

(defun gnugo-command (command)
  "Send the Go Text Protocol COMMAND (a string) to GNU Go.
Output and Emacs behavior depend on which command is given (some
commands are handled completely by Emacs w/o using the subprocess;
some commands have their output displayed in specially prepared
buffers or in the echo area; some commands are instrumented to do
gnugo.el-specific housekeeping).

For example, for the command \"help\", Emacs visits the
GTP command reference info page.

NOTE: At this time, GTP command handling specification is still
      incomplete.  Thus, some commands WILL confuse gnugo.el."
  (interactive "sCommand: ")
  (if (string= "" command)
      (message "(no command given)")
    (let* ((split (split-string command))
           (cmd (intern (car split)))
           (spec (get cmd :gnugo-gtp-command-spec))
           (full (plist-get spec :full))
           (last-message nil))
      (if full
          (funcall full (cdr split))
        (message "Doing %s ..." command)
        (let* ((ans (cdr (gnugo-synchronous-send/return command)))
               (where (plist-get spec :output)))
          (if (string-match "unknown.command" ans)
              (message ans)
            (cond ((functionp where) (funcall where ans))
                  ((eq :discard where) (message ""))
                  ((or (eq :message where)
                       (not (string-match "\n" ans)))
                   (message ans))
                  (t (switch-to-buffer "*gnugo command output*")
                     (erase-buffer)
                     (insert ans)
                     (message "Doing %s ... done." command)))
            (let ((hook
                   ;; do not elide this binding; `run-hooks' needs it
                   (plist-get spec :post-hook)))
              (run-hooks 'hook))))))))

;;;---------------------------------------------------------------------------
;;; Major mode for interacting with a GNUGO subprocess

(put 'gnugo-board-mode 'mode-class 'special)
(defun gnugo-board-mode ()
  "Major mode for playing GNU Go.
Entering this mode runs the normal hook `gnugo-board-mode-hook'.
In this mode, keys do not self insert.  Default keybindings:

  ?             View this help.

  RET or SPC    Run `gnugo-move'.

  q or Q        Quit (the latter without confirmation).

  R             Resign.

  u             Run `gnugo-undo-two-moves'.

  U             Pass to `gnugo-magic-undo' either the board position
                at point (if no prefix arg), or the prefix arg converted
                to a number.  E.g., to undo 16 moves: `C-u C-u U' (see
                `universal-argument'); to undo 42 moves: `M-4 M-2 U'.

  C-l           Run `gnugo-refresh'.

  _ or M-_      Bury the Board buffer (when the boss is near).

  P             Run `gnugo-pass'.

  i             Toggle display using XPM images (if supported).

  w             Run `gnugo-worm-stones'.
  d             Run `gnugo-dragon-stones'.

  W             Run `gnugo-worm-data'.
  D             Run `gnugo-dragon-data'.

  t             Run `gnugo-toggle-dead-group'.

  !             Run `gnugo-estimate-score'.

  : or ;        Run `gnugo-command' (for GTP commands to GNU Go).

  =             Display board position under point (if valid).

  h             Run `gnugo-move-history'.

  F             Run `gnugo-display-final-score'.

  s             Run `gnugo-write-sgf-file'.
  or C-x C-w
  or C-x C-s

  l             Run `gnugo-read-sgf-file'."
  (switch-to-buffer (generate-new-buffer "(Uninitialized GNUGO Board)"))
  (buffer-disable-undo)                 ; todo: undo undo undoing
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map gnugo-board-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(gnugo-font-lock-keywords t))
  (setq major-mode 'gnugo-board-mode)
  (setq mode-name "GNUGO Board")
  (add-hook 'kill-buffer-hook 'gnugo-cleanup nil t)
  (set (make-local-variable 'gnugo-state)
       (make-hash-table :size (1- 42) :test 'eq))
  (mapc (lambda (prop)
          (gnugo-put prop nil))   ; todo: separate display/game aspects;
        '(:game-over                 ;       move latter to func `gnugo'
          :waitingp
          :last-waiting
          :black-captures
          :white-captures
          :mode-line
          :mode-line-form
          :display-using-images
          :xpms
          :local-xpms
          :all-yy))
  (let ((name (if (string-match "[ ]" gnugo-program)
                  (let ((p (substring gnugo-program 0 (match-beginning 0)))
                        (o (substring gnugo-program (match-end 0)))
                        (h (or (car gnugo-option-history) "")))
                    (when (string-match "--mode" o)
                      (error "Found \"--mode\" in `gnugo-program'"))
                    (when (and o (< 0 (length o))
                               h (< 0 (length o))
                               (or (< (length h) (length o))
                                   (not (string= (substring h 0 (length o))
                                                 o))))
                      (push (concat o " " h) gnugo-option-history))
                    p)
                gnugo-program))
        (args (read-string "GNU Go options: "
                           (car gnugo-option-history)
                           'gnugo-option-history))
        (rules "Japanese")
        board-size user-color handicap komi minus-l infile)
    (mapc (lambda (x)
            (apply (lambda (var default opt &optional rx)
                     (set var
                          (or (when (string-match opt args)
                                (let ((start (match-end 0)) s)
                                  (string-match (or rx "[0-9.]+") args start)
                                  (setq s (match-string 0 args))
                                  (if rx s (string-to-number s))))
                              default)))
                   x))
          '((board-size      19 "--boardsize")
            (user-color "black" "--color" "\\(black\\|white\\)")
            (handicap         0 "--handicap")
            (komi           0.0 "--komi")
            (minus-l        nil "\\([^-]\\|^\\)-l[ ]*" "[^ ]+")
            (infile         nil "--infile" "[ ]*[^ ]+")))
    (gnugo-put :user-color user-color)
    (when (string-match "--chinese-rules" args)
      (setq rules "Chinese"))
    (let ((proc-args (split-string args)))
      (gnugo-put :proc-args proc-args)
      (gnugo-put :proc (apply 'start-process "gnugo" nil name
                              "--mode" "gtp" "--quiet"
                              proc-args)))
    (when (or minus-l infile)
      (mapc (lambda (x)
              (apply (lambda (prop q)
                       (set prop (string-to-number (gnugo-query q))))
                     x))
            '((board-size "query_boardsize")
              (komi       "get_komi")
              (handicap   "get_handicap"))))
    (gnugo-put :diamond (substring (process-name (gnugo-get :proc)) 5))
    (gnugo-put :gnugo-color (gnugo-other (gnugo-get :user-color)))
    (gnugo-put :highlight-last-move-spec
               (gnugo-put :default-highlight-last-move-spec '("(" -1 nil)))
    (gnugo-put :lparen-ov (make-overlay 1 1))
    (gnugo-put :rparen-ov (let ((ov (make-overlay 1 1)))
                            (overlay-put ov 'display ")")
                            ov))
    (gnugo-put :sgf-gametree (list (list '(:FF . 4) '(:GM . 1))))
    (let ((tree (gnugo-get :sgf-gametree)))
      (gnugo-put :sgf-collection (list tree))
      (gnugo-put :monkey (vector tree nil 0)))
    (let ((g-blackp (string= "black" (gnugo-get :gnugo-color))))
      (mapc (lambda (x) (apply 'gnugo-note x))
            `((:SZ ,board-size)
              (:DT ,(format-time-string "%Y-%m-%d"))
              (:RU ,rules)
              (:AP ("gnugo.el" . ,gnugo-version))
              (:KM ,komi)
              (,(if g-blackp :PW :PB) ,(user-full-name))
              (,(if g-blackp :PB :PW) ,(concat "GNU Go "
                                               (gnugo-query "version")))
              ,@(when (not (= 0 handicap))
                  `((:HA ,handicap)
                    (:AB ,(gnugo-lsquery "fixed_handicap %d" handicap)
                         nil t)))))))
  (set-process-sentinel (gnugo-get :proc) 'gnugo-sentinel)
  (set-process-buffer (gnugo-get :proc) (current-buffer))
  (gnugo-put :waiting-start (current-time))
  (gnugo-put :hmul 1)
  (gnugo-put :wmul 1)
  (run-hooks 'gnugo-board-mode-hook)
  (gnugo-refresh t))

;;;---------------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun gnugo (&optional new-game)
  "Run gnugo in a buffer, or resume a game in progress.
Prefix arg means skip the game-in-progress check and start a new
game straight away.

You are queried for additional command-line options (Emacs supplies
\"--mode gtp --quiet\" automatically).  Here is a list of options
that gnugo.el understands and handles specially:

    --boardsize num   Set the board size to use (5--19)
    --color <color>   Choose your color ('black' or 'white')
    --handicap <num>  Set the number of handicap stones (0--9)

If there is already a game in progress you may resume it instead of
starting a new one.  See `gnugo-board-mode' documentation for more info."
  (interactive "P")
  (let* ((all (let (acc)
                (dolist (buf (buffer-list))
                  (when (gnugo-board-buffer-p buf)
                    (push (cons (buffer-name buf) buf) acc)))
                acc))
         (n (length all)))
    (if (and (not new-game)
             (< 0 n)
             (y-or-n-p (format "GNU Go game%s in progress, resume play? "
                               (if (= 1 n) "" "s"))))
        ;; resume
        (switch-to-buffer
         (cdr (if (= 1 n)
                  (car all)
                (let ((sel (completing-read "Which one? " all nil t)))
                  (if (string= "" sel)
                      (car all)
                    (assoc sel all))))))
      ;; set up a new board
      (gnugo-board-mode)
      (let ((half (ash (1+ (gnugo-treeroot :SZ)) -1)))
        (gnugo-goto-pos (format "A%d" half))
        (forward-char (* 2 (1- half)))
        (gnugo-put :last-user-bpos
                   (gnugo-put :center-position
                              (get-text-property (point) 'gnugo-position))))
      ;; first move
      (gnugo-put :game-start-time (current-time))
      (let ((g (gnugo-get :gnugo-color))
            (n (or (gnugo-treeroot :HA) 0))
            (u (gnugo-get :user-color)))
        (gnugo-put :last-mover g)
        (when (or (and (string= "black" u) (< 1 n))
                  (and (string= "black" g) (< n 2)))
          (gnugo-put :last-mover u)
          (gnugo-refresh t)
          (gnugo-get-move g))))))

;;;---------------------------------------------------------------------------
;;; Load-time actions

(unless gnugo-board-mode-map
  (setq gnugo-board-mode-map (make-sparse-keymap))
  (suppress-keymap gnugo-board-mode-map)
  (mapc (lambda (pair)
          (define-key gnugo-board-mode-map (car pair) (cdr pair)))
        '(("?"        . describe-mode)
          ("\C-m"     . gnugo-move)
          (" "        . gnugo-move)
          ("P"        . gnugo-pass)
          ("R"        . gnugo-resign)
          ("q"        . (lambda () (interactive)
                          (if (or (gnugo-get :game-over)
                                  (y-or-n-p "Quit? "))
                              (kill-buffer nil)
                            (message "(not quitting)"))))
          ("Q"        . (lambda () (interactive)
                          (kill-buffer nil)))
          ("U"        . (lambda (x) (interactive "P")
                          (gnugo-magic-undo
                           (cond ((numberp x) x)
                                 ((consp x) (car x))
                                 (t (gnugo-position))))))
          ("u"        . gnugo-undo-two-moves)
          ("\C-l"     . gnugo-refresh)
          ("\M-_"     . bury-buffer)
          ("_"        . bury-buffer)
          ("h"        . gnugo-move-history)
          ("i"        . (lambda () (interactive)
                          (gnugo-toggle-image-display)
                          (save-excursion (gnugo-refresh))))
          ("w"        . gnugo-worm-stones)
          ("W"        . gnugo-worm-data)
          ("d"        . gnugo-dragon-stones)
          ("D"        . gnugo-dragon-data)
          ("t"        . gnugo-toggle-dead-group)
          ("!"        . gnugo-estimate-score)
          (":"        . gnugo-command)
          (";"        . gnugo-command)
          ("="        . (lambda () (interactive)
                          (message (gnugo-position))))
          ("s"        . gnugo-write-sgf-file)
          ("\C-x\C-s" . gnugo-write-sgf-file)
          ("\C-x\C-w" . gnugo-write-sgf-file)
          ("l"        . gnugo-read-sgf-file)
          ("F"        . gnugo-display-final-score)
          ;; mouse
          ([(down-mouse-1)] . gnugo-mouse-move)
          ([(down-mouse-2)] . gnugo-mouse-move) ; mitigate accidents
          ([(down-mouse-3)] . gnugo-mouse-pass)
          ;; delving into the curiosities
          ("\C-c\C-p" . gnugo-describe-internal-properties))))

(unless (get 'help :gnugo-gtp-command-spec)
  (flet ((sget (x) (get x :gnugo-gtp-command-spec))
         (jam (cmd prop val) (put cmd :gnugo-gtp-command-spec
                                  (plist-put (sget cmd) prop val)))
         (add (cmd prop val) (jam cmd prop (let ((cur (plist-get
                                                       (sget cmd)
                                                       prop)))
                                             (append (delete val cur)
                                                     (list val)))))
         (defgtp (x &rest props) (dolist (cmd (if (symbolp x) (list x) x))
                                   (let ((ls props))
                                     (while ls
                                       (funcall (if (eq :post-hook (car ls))
                                                    'add
                                                  'jam)
                                                cmd (car ls) (cadr ls))
                                       (setq ls (cddr ls)))))))

    (defgtp 'help :full
      (lambda (sel)
        (info "(gnugo)GTP command reference")
        (when sel (setq sel (intern (car sel))))
        (let (buffer-read-only pad cur spec output found)
          (flet ((note (s) (insert pad "[NOTE: gnugo.el " s ".]\n")))
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^ *[*] \\([a-zA-Z_]+\\)\\(:.*\\)*\n"
                                        (point-max) t)
                (unless pad
                  (setq pad (make-string (- (match-beginning 1)
                                            (match-beginning 0))
                                         32)))
                (when (plist-get
                       (setq spec
                             (get (setq cur (intern (match-string 1)))
                                  :gnugo-gtp-command-spec))
                       :full)
                  (note "handles this command completely"))
                (when (setq output (plist-get spec :output))
                  (cond ((functionp output)
                         (note "handles the output specially"))
                        ((eq :discard output)
                         (note "discards the output"))
                        ((eq :message output)
                         (note "displays the output in the echo area"))))
                (when (eq sel cur)
                  (setq found (match-beginning 0))))))
          (cond (found (goto-char found))
                ((not sel))
                (t (message "(no such command: %s)" sel))))))

    (defgtp 'final_score :full
      (lambda (sel) (gnugo-display-final-score)))

    (defgtp '(boardsize
              clear_board
              fixed_handicap)
      :output :discard
      :post-hook (lambda ()
                   (gnugo-put :game-over nil)
                   (gnugo-put :last-mover nil)
                   (gnugo-refresh t)))

    (defgtp 'loadsgf :full
      (lambda (sel) (gnugo-read-sgf-file (car sel))))

    (defgtp '(undo gg-undo) :full
      (lambda (sel) (gnugo-magic-undo
                     (let (n)
                       (cond ((not sel) 1)
                             ((< 0 (setq n (string-to-number (car sel)))) n)
                             (t (car sel)))))))))

(provide 'gnugo)

 
;;;---------------------------------------------------------------------------
;;; The remainder of this file defines a simplified SGF-handling library.
;;; When/if it should start to attain generality, it should be split off into
;;; a separate file (probably named sgf.el) w/ funcs and vars renamed sans the
;;; "gnugo/" prefix.

(defconst gnugo/sgf-*r4-properties*
  '((AB "Add Black"       setup list stone)
    (AE "Add Empty"       game  list point)
    (AN "Annotation"      game  simpletext)
    (AP "Application"     root  (simpletext . simpletext))
    (AR "Arrow"           -     list (point . point))
    (AS "Who adds stones" -     simpletext) ; (LOA)
    (AW "Add White"       setup list stone)
    (B  "Black"           move  move)
    (BL "Black time left" move  real)
    (BM "Bad move"        move  double)
    (BR "Black rank"      game  simpletext)
    (BT "Black team"      game  simpletext)
    (C  "Comment"         -     text)
    (CA "Charset"         root  simpletext)
    (CP "Copyright"       game  simpletext)
    (CR "Circle"          -     list point)
    (DD "Dim points"      -     elist point) ; (inherit)
    (DM "Even position"   -     double)
    (DO "Doubtful"        move  none)
    (DT "Date"            game  simpletext)
    (EV "Event"           game  simpletext)
    (FF "Fileformat"      root  [number (1 . 4)])
    (FG "Figure"          -     (or none (number . simpletext)))
    (GB "Good for Black"  -     double)
    (GC "Game comment"    game  text)
    (GM "Game"            root  [number (1 . 20)])
    (GN "Game name"       game  simpletext)
    (GW "Good for White"  -     double)
    (HA "Handicap"        game  number) ; (Go)
    (HO "Hotspot"         -     double)
    (IP "Initial pos."    game  simpletext) ; (LOA)
    (IT "Interesting"     move  none)
    (IY "Invert Y-axis"   game  simpletext)          ; (LOA)
    (KM "Komi"            game  real)                ; (Go)
    (KO "Ko"              move  none)
    (LB "Label"           -     list (point . simpletext))
    (LN "Line"            -     list (point . point))
    (MA "Mark"            -     list point)
    (MN "set move number" move  number)
    (N  "Nodename"        -     simpletext)
    (OB "OtStones Black"  move  number)
    (ON "Opening"         game  text)
    (OT "Overtime"        game  simpletext)
    (OW "OtStones White"  move  number)
    (PB "Player Black"    game  simpletext)
    (PC "Place"           game  simpletext)
    (PL "Player to play"  setup color)
    (PM "Print move mode" -     number) ; (inherit)
    (PW "Player White"    game  simpletext)
    (RE "Result"          game  simpletext)
    (RO "Round"           game  simpletext)
    (RU "Rules"           game  simpletext)
    (SE "Markup"          -     point)  ; (LOA)
    (SL "Selected"        -     list point)
    (SO "Source"          game  simpletext)
    (SQ "Square"          -     list point)
    (ST "Style"           root  [number (0 . 3)])
    (SU "Setup type"      game  simpletext) ; (LOA)
    (SZ "Size"            root  (or number (number . number)))
    (TB "Territory Black" -     elist point) ; (Go)
    (TE "Tesuji"          move  double)
    (TM "Timelimit"       game  real)
    (TR "Triangle"        -     list point)
    (TW "Territory White" -     elist point) ; (Go)
    (UC "Unclear pos"     -     double)
    (US "User"            game  simpletext)
    (V  "Value"           -     real)
    (VW "View"            -     elist point) ; (inherit)
    (W  "White"           move  move)
    (WL "White time left" move  real)
    (WR "White rank"      game  simpletext)
    (WT "White team"      game  simpletext)
    (LT "Lose on time"    setup simpletext))
  ;; r4-specific notes
  ;; - changed: DT FG LB RE RU SZ
  ;; - added: AP AR AS DD IP IY LN OT PM SE SQ ST SU VW
  "List of SGF[4] properties, each of the form (PROP NAME CONTEXT SPEC...).")

(defun gnugo/sgf-read-file (filename)
  "Return the collection (list) of gametrees in SGF[4] file FILENAME."
  (let ((keywords (mapcar (lambda (full)
                            (cons (car full)
                                  (intern (format ":%s" (car full)))))
                          gnugo/sgf-*r4-properties*))
        (specs (mapcar (lambda (full)
                         (cons (car full) (cdddr full)))
                       gnugo/sgf-*r4-properties*)))
    (flet ((sw () (skip-chars-forward " \t\n"))
           (x (end) (let ((beg (point))
                          (endp (case end
                                  (:end (lambda (char) (= ?\] char)))
                                  (:mid (lambda (char) (= ?\: char)))
                                  (t (lambda (char) (or (= ?\: char)
                                                        (= ?\] char))))))
                          c)
                      (while (not (funcall endp (setq c (char-after))))
                        (cond ((= ?\\ c)
                               (delete-char 1)
                               (if (eolp)
                                   (kill-line 1)
                                 (forward-char 1)))
                              ((looking-at "\\s-+")
                               (delete-region (point) (match-end 0))
                               (insert " "))
                              (t (forward-char 1))))
                      (buffer-substring-no-properties beg (point))))
           (one (type end) (unless (eq 'none type)
                             (forward-char 1)
                             (let ((s (x end)))
                               (case type
                                 ((stone point move simpletext color) s)
                                 ((number real double) (string-to-number s))
                                 ((text) s)
                                 (t (error "Unhandled type: %S" type))))))
           (val (spec) (cond ((symbolp spec)
                              (one spec :end))
                             ((vectorp spec)
                              ;; todo: check range here.
                              (one (aref spec 0) :end))
                             ((eq 'or (car spec))
                              (let ((v (one (cadr spec) t)))
                                (if (= ?\] (char-after))
                                    v
                                  (forward-char 1)
                                  ;; todo: this assumes `spec' has the form
                                  ;;         (or foo (foo . bar))
                                  ;; i.e., foo is not rescanned.  e.g., `SZ'.
                                  ;; probably this assumption is consistent
                                  ;; w/ the SGF authors' desire to make the
                                  ;; parsing easy, but you never know...
                                  (cons v (one (cdaddr spec) :end)))))
                             (t (cons (one (car spec) :mid)
                                      (one (cdr spec) :end)))))
           (short (who) (when (eobp)
                          (error "Unexpected EOF while reading %s" who)))
           (atvalp () (= ?\[ (char-after)))
           (PROP () (let (name spec ltype)
                      (sw) (short 'property)
                      (when (looking-at "[A-Z]")
                        (setq name (read (current-buffer))
                              spec (cdr (assq name specs)))
                        (sw)
                        (cons
                         (cdr (assq name keywords))
                         (prog1 (if (= 1 (length spec))
                                    (val (car spec))
                                  (unless (memq (setq ltype (car spec))
                                                '(elist list))
                                    (error "Bad spec: %S" spec))
                                  (if (and (eq 'elist ltype) (sw)
                                           (not (atvalp)))
                                      nil
                                    (let ((type (cadr spec))
                                          mo ls)
                                      (while (and (sw) (atvalp)
                                                  (setq mo (val type)))
                                        (push mo ls)
                                        (forward-char 1))
                                      (forward-char -1)
                                      (nreverse ls))))
                           (forward-char 1))))))
           (NODE () (let (prop props)
                      (sw) (short 'node)
                      (when (= ?\; (char-after))
                        (forward-char 1)
                        (while (setq prop (PROP))
                          (push prop props))
                        (nreverse props))))
           (TREE () (let (nodes)
                      (while (and (sw) (not (eobp)))
                        (case (char-after)
                          (?\; (push (NODE) nodes))
                          (?\( (forward-char 1)
                               (push (TREE) nodes))
                          (?\) (forward-char 1))))
                      (nreverse nodes))))
      (with-temp-buffer
        (insert-file-contents filename)
        (let (trees)
          (while (and (sw) (not (eobp)) (= 40 (char-after))) ; left paren
            (forward-char 1)
            (push (TREE) trees))
          (nreverse trees))))))

(defun gnugo/sgf-write-file (collection filename)
  ;; take responsibility for our actions
  (dolist (tree collection)
    (let* ((root (car tree))
           (who (assq :AP root))
           (fruit (cons "gnugo.el" gnugo-version)))
      (if who
          (setcdr who fruit)
        (setcdr (last root) (list (cons :AP fruit))))))
  ;; write it out
  (let ((aft-newline-appreciated '(:AP :GN :PB :PW :HA :KM :RU :RE))
        (specs (mapcar (lambda (full)
                         (cons (intern (format ":%s" (car full)))
                               (cdddr full)))
                       gnugo/sgf-*r4-properties*))
        p name v spec)
    ;; todo: escape special chars for `text' and `simpletext'.
    (flet ((>>one (v) (insert (format "[%s]" v)))
           (>>two (v) (insert (format "[%s:%s]" (car v) (cdr v))))
           (>>nl () (cond ((memq name aft-newline-appreciated)
                           (insert "\n"))
                          ((< 60 (current-column))
                           (save-excursion
                             (goto-char p)
                             (insert "\n"))))))
      (with-temp-buffer
        (dolist (tree collection)
          (insert "(")
          (dolist (node tree)
            (insert ";")
            (dolist (prop node)
              (setq p (point)
                    name (car prop)
                    v (cdr prop))
              (insert (substring (symbol-name name) 1))
              (cond ((not v))
                    ((and (consp v)
                          (memq (car (setq spec (cdr (assq name specs))))
                                '(list elist)))
                     (>>nl)
                     (let ((>> (if (consp (cadr spec))
                                   '>>two
                                 '>>one)))
                       (dolist (little-v v)
                         (setq p (point)) (funcall >> little-v) (>>nl))))
                    ((consp v)
                     (>>two v) (>>nl))
                    (t
                     (>>one v) (>>nl)))))
          (insert ")\n"))
        (write-file filename)))))

;;; ttn-sez: worth-compiling
;;; gnugo.el ends here
