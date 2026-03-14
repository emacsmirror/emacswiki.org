;;; Emvaders -- clone of a famous arcade game program.
;; Copyright (C) 1989 by MAEDA Atusi
;; Originally written by MAEDA Atusi
;; Modified by Mirko Link Thu Feb 13 14:19:26 2001
;; Modified by Hideto Sazuka Thu Jun 29 12:09:36 1989
;; Modified by MAEDA Atusi Thu Jun 29 20:50:16 1989
;; Modified by MAEDA Atusi Wed Jul  5 20:21:31 1989

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;; src: https://web.archive.org/web/20060430085749/http://www.emacs.mirkolinkonline.de/download/emvaders.el [2026-03-14]


(provide 'emvaders)
;(require 'boss)

;;; User customizable variables.

(defvar emvaders-enemy-amount 60
  "*The number of enemies in each level.  Should preferably be a multiple of
10.")

(defvar emvaders-tick-size 10
  "*How long in milliseconds between each update of the screen and reading of
input.")

(defvar emvaders-initial-speed emvaders-enemy-amount
  "*How many ticks (of emvaders-tick-size each) between each move of the
enemies.  Normally equal to the amount of enemies, so the last one is extremely
fast.")

(defvar emvaders-shot-speed 2
  "*How fast (in emvader-tick-size milliseconds) a players shots move.")

(defvar emvaders-bomb-speed 5
  "*How fast (in emvader-tick-size milliseconds) the enemies bombs move.")

(defvar emvaders-high-score-file
  (or (getenv "EMVADERSFILE")
      "$HOME/.emvaders")
  "*File name where top ten scores of emvaders are recorded.
Initialized from EMVADERSFILE environment variable.
Nil means does not record scores.")

(defvar emvaders-width 18
  "*Width of emvaders board (number of blocks).  Each block occupies three
column width on window, plus one space.")

(defvar emvaders-use-full-window t
  "*Non-nil means that starting Emvaders game deletes other windows.")

(defun emvaders ()
  "Clone of a famous arcade game program."
  (interactive)
  (setq emvaders-previous-window-configuration
	(current-window-configuration))
  (switch-to-buffer "*Emvaders*")
  (emvaders-mode)
  (emvaders-startup))

;;; Internal variables.

(defvar emvaders-command-vector nil
  "Vector of functions which maps character to emvaders command.")

(defvar emvaders-mode-map nil)

(defvar emvaders-ship-string "/^\\"
  "*The string used to represent the ship")

(defvar emvaders-shot-string "|"
  "*The string used to represent a shot")

(defvar emvaders-bomb-string "Y"
  "*The string used to represent a falling bomb")

(defvar emvaders-enemy-strings [("/M\\" . "\\W/") ("<H>" . ">H<") ("`^´" . "´v`")]
"*List of enemy ship types.  Each is a pair of the two 'styles' a ship can be.")

(defvar emvaders-enemies-data nil
"List of enemies remaining in this level.  Each element is a three-vector
of the ship type and its x,y position.")

(defvar emvaders-left-margin)
(defvar emvaders-height)
(defvar emvaders-previous-window-configuration nil)
(defvar emvaders-blank-line)
(defvar emvaders-complete-line)
(defvar emvaders-line-length)

(defun emvaders-startup ()
  (setq buffer-read-only nil)
  (erase-buffer)
  (goto-char (point-min))
  (insert (substitute-command-keys "

<<< E M V A D E R S >>>

Clone of a famous action game.

Written by
Lars Clausen
elascurn@daimi.aau.dk

Based on the Getric code written by
MAEDA Atusi
mad@nakanishi.math.keio.junet


<Type \\[emvaders-mode-help] for help, \\[emvaders-start] to start game.>
"))
  (center-region (point-min) (point-max))
  (setq buffer-read-only t))

(defun emvaders-mode-help ()
  (interactive)
  (message (concat
	    (substitute-command-keys "\\[emvaders-mode-help]:Print this  ")
	    (substitute-command-keys "\\[emvaders-start]:New game  ")
	    (substitute-command-keys "\\[emvaders-help]:List keys  ")
	    (substitute-command-keys "\\[boss-has-come]:Boss has come!  ")
	    (substitute-command-keys "\\[emvaders-exit]:Exit"))))

(or emvaders-mode-map
(progn
  (setq emvaders-mode-map (make-sparse-keymap))
  (define-key emvaders-mode-map "?" 'emvaders-mode-help)
  (define-key emvaders-mode-map "\C-m" 'emvaders-start)
  (define-key emvaders-mode-map "h" 'emvaders-help)
  (define-key emvaders-mode-map "\e" 'boss-has-come)
  (define-key emvaders-mode-map "q" 'emvaders-exit)))

(defun emvaders-help ()
  (interactive)
  (message "j:Left  k:Fire  l:Right  ESC:Escape  q:Exit"))

(or emvaders-command-vector
    (progn
      (setq emvaders-command-vector (make-vector 256 'emvaders-help))
      (aset emvaders-command-vector ?j 'emvaders-move-left)
      (aset emvaders-command-vector ?k 'emvaders-fire)
      (aset emvaders-command-vector ?l 'emvaders-move-right)
      (aset emvaders-command-vector ?q 'emvaders-quit)
      (aset emvaders-command-vector ?\e 'emvaders-boss-has-come)))

(defun emvaders-mode ()
  "Major mode for playing emvaders game.
\\{emvaders-mode-map}
Type \\[emvaders-help] for key action in the game.
Entry to this mode calls the value of emvaders-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'global-mode-string)
  (setq major-mode 'emvaders-mode)
  (setq mode-name "Emvaders")
  (use-local-map emvaders-mode-map)
  (buffer-disable-undo (current-buffer))
  (setq buffer-read-only t)
  (emvaders-mode-help)
  (run-hooks 'emvaders-mode-hook))

(defun emvaders-start ()
  (interactive)
  (switch-to-buffer "*Emvaders*")
  (overwrite-mode 1)
  (if emvaders-use-full-window
      (delete-other-windows)
    ;; Enlarge window size if necessary.
    (progn
      (emvaders-get-window-size)
      (if (< emvaders-left-margin 1)
	  (enlarge-window (1+ (* 4 (- 1 emvaders-left-margin))) t))
      (if (< emvaders-height 20)
	  (enlarge-window (- 20 emvaders-height)))))
  (emvaders-get-window-size)		;again
  (if (or (< emvaders-height 20)
	  (< emvaders-left-margin 1))
      (error "Window size too small for emvaders."))
  (setq emvaders-height 20)
;  (let ((left-margin-space ""));(make-string (1- emvaders-left-margin) ? )))
  (let ((left-margin-space (make-string (1- emvaders-left-margin) ? )))
    (setq emvaders-blank-line
	  (concat left-margin-space "||"
	  (make-string (* 4 emvaders-width) ? ) "||\n"))
    (setq emvaders-line-length (length emvaders-blank-line))
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((i 0))
      (while (< i emvaders-height)
      (insert emvaders-blank-line)
      (setq i (1+ i))))
    (insert (concat left-margin-space
    (make-string (+ 4 (* 4 emvaders-width)) ?=))))
  (random t)				;randomize by current time
  (catch 'emvaders-quit-tag
    (emvaders-main-loop)
    (emvaders-mode-help)))

(defun emvaders-get-window-size ()
  (setq emvaders-height (- (window-height) 2))
  (setq emvaders-left-margin
	(/ (- (window-width)
	      (* 4 emvaders-width)
	      4)
	   2)))

(defun emvaders-repeat-string (string times)
  (let ((result ""))
    (while (> times 0)
      (setq result (concat string result))
      (setq times (1- times)))
    result))

(defun emvaders-exit ()
  (interactive)
  (set-window-configuration emvaders-previous-window-configuration))

(defun abs (number)
  (if (< number 0)
      (- number)
    number))

(defvar level nil nil)
(defvar score nil nil)
(defvar enemy-hit nil nil)
(defvar enemies nil nil)
(defvar shot nil nil)
(defvar speed nil nil)

(defun emvaders-main-loop ()
  (setq level 1)
  (setq score 0)
  (let ((lives 3)
	(ship-pos (cons (+ (* emvaders-width 2)
			   emvaders-left-margin)
			(- emvaders-height 1)))
	(playing t))
    ;;; Main loop of levels
    (while playing
      (progn
	(setq enemies (emvaders-setup-game))
	(setq speed (+ 2 (length enemies)))
	(let (direction
	      bomb-list
	      (count 1)
	      (player-wait-time 0)
	      flip-state
	      player-hit)
	  (setq emvaders-last-update (cdr (current-time)))
	  (emvaders-set-field ship-pos emvaders-ship-string)
	  (mapcar 'emvaders-set-enemy enemies)
	  (while (and (>= lives 0) enemies)
	    (setq emvaders-debug bomb-list)
	    (if (and shot (= (% count emvaders-shot-speed) 0))
		(progn
		  (emvaders-move-shot)
		  (if enemy-hit (emvaders-remove-enemy))))
	    (if (= (% count speed) 0)
		(progn
		  (emvaders-move-enemies)
		  (if enemy-hit (emvaders-remove-enemy))))
	    (if (= (% count emvaders-bomb-speed) 0)
		(emvaders-move-bombs))
	    (if player-hit
		(progn
		  ;; Kill the player (ouch)
		  (ding t)
		  (emvaders-set-field ship-pos "   ")
		  (setq ship-pos (cons (* emvaders-width 2)
				       (- emvaders-height 1)))
		  (setq player-wait-time 40)
		  (setq lives (1- lives))
		  (if (numberp player-hit) ;; Invaded -- you lost
		      (setq lives -1))
		  (setq player-hit nil)
		  ))
	    (setq count (1+ count))
	    (if (= player-wait-time 0)
		(if (input-pending-p)
		    (funcall (aref emvaders-command-vector (read-char))))
	      (while (input-pending-p) (read-char))
	      (setq player-wait-time (1- player-wait-time))
	      (if (= player-wait-time 0)
		  (emvaders-set-field ship-pos emvaders-ship-string)))
	    (emvaders-pause emvaders-tick-size)
	    (emvaders-show-score)
	    )
	  (if (< lives 0) (setq playing nil)
	    (setq level (1+ level)))
	  )))
    (emvaders-set-field (cons (- (* 2 emvaders-width) 8)
			      (/ emvaders-height 2))
			"*** GAME OVER ***")
    (setq buffer-read-only t)
    (if emvaders-high-score-file
	(emvaders-show-high-score))))

(defun filter (pred l)
  (nreverse
   (let (nl)
     (while l
       (if (eval (list pred '(car l)))
	   (setq nl (cons (car l) nl)))
       (setq l (cdr l)))
     nl)))

(defun first (pred l)
  (while (and l (not (eval (list pred '(car l)))))
    (setq l (cdr l)))
  (if l (car l)))

;; Kill an enemy ship
(defun emvaders-remove-enemy ()
  (progn
    (setq score (+ score (* 100 (- 3 (car enemy-hit)))))
    (emvaders-unset-enemy enemy-hit)
    (setq enemies (filter '(lambda (e) (not (eq e enemy-hit))) enemies))
    (setq speed (1- speed))
    (setq enemy-hit nil)
    (setq shot nil)
    ))


(defun emvaders-setup-game ()
  ;;;  Insert ships at a certain level
  (let ((kind 0)
	shiplist
	(x emvaders-left-margin))
    (while (< kind 6)
      (setq shiplist (cons `(,(/ kind 2) ,(+ x 10) . ,(+ (* 2 kind) level))
			   shiplist))
      (setq x (+ 4 x))
      (if (> x (+ 40 emvaders-left-margin)) (progn (setq x emvaders-left-margin) (setq kind (1+ kind)))))
    shiplist
  ))

(defmacro emvaders-goto-x-y (x y)
  (`(goto-char (+ (* (, y) emvaders-line-length)
		  (, x)
		  1))))

(defun emvaders-char-at (pos)
  (char-after (+ (* (cdr pos) emvaders-line-length) (car pos) 1)))

(defvar emvaders-last-update nil
  "The last time we made a delay.  This is the last two parts of the result
of current-time")

(defun emvaders-pause (n)
  "Pause for n milliseconds since last update.  Uses 600 microseconds for
internal calculations, so this shouldn't be used for a clock.  It does not
pause when called the first time, unless emvaders-last-update is set
explicitly to (cdr (current-time))."
  (emvaders-goto-x-y 0 0)
  (if emvaders-last-update
      (let ((time-now (cdr (current-time))))
	(let ((millis-remaining (- n (* (- (car time-now) (car emvaders-last-update)) 1000)
				   (/ (- (car (cdr time-now)) (car (cdr emvaders-last-update))) 1000))))
	  (if (> millis-remaining 0)
	      (sleep-for 0 millis-remaining)))))
  (setq emvaders-last-update (cdr (current-time))))

(defun emvaders-check-shot (e)
  (if (and (= (cdr (cdr e)) (cdr shot))
	   (>= (car shot) (car (cdr e)))
	   (<= (car shot) (+ (car (cdr e)) 2)))
      e
    nil))

(defun emvaders-move-enemies ()
  (setq flip-state (not flip-state))
  (let ((bomb-at (random (* 4 emvaders-width))))
    (if direction
	(let ((min-pos (+ emvaders-left-margin emvaders-width)))
	  (mapcar '(lambda (e) (setq min-pos (min min-pos (car (cdr e)))))
		  enemies)
	  (if (= min-pos (+ 3 emvaders-left-margin))
	      (progn
		(setq direction nil)
		(setq enemies (mapcar 'emvaders-down-enemy enemies))
		(if (= (cdr (cdr (car enemies))) (- emvaders-height 1))
		    (setq player-hit 1)))
	    ;; Move their positions
	    (setq enemies (mapcar 'emvaders-move-enemy enemies))
	    ))
      (let ((max-pos emvaders-left-margin))
	(mapcar '(lambda (e) (setq max-pos (max max-pos (car (cdr e)))))
		enemies)
	(if (= max-pos (1- (- (+ emvaders-left-margin (* 4 emvaders-width)) 4)))
	    (progn
	      (setq direction t)
	      (setq enemies (mapcar 'emvaders-down-enemy enemies))
	      (if (= (cdr (cdr (car enemies))) (- emvaders-height 1))
		  (setq player-hit 1)))
	  ;; Move their positions
	  (setq enemies (mapcar 'emvaders-move-enemy enemies)))))
    )
;  (message (concat "First enemy at " (int-to-string (car (cdr (car enemies))))
;		   ", " (int-to-string (cdr (cdr (car enemies))))))
  )

(defun emvaders-set-field (pos string)
  (emvaders-goto-x-y (car pos) (cdr pos))
  (delete-char (length string))
  (insert string))

(defun emvaders-delete-field (pos chars)
  (emvaders-goto-x-y (car pos) (cdr pos))
  (delete-char chars))

(defun emvaders-insert-field (pos string)
  (emvaders-goto-x-y (car pos) (cdr pos))
  (insert string))

(defun emvaders-move-enemy (e)
  (emvaders-delete-field (cdr e) 3)
  (let ((pos (cdr e)))
    (setcar pos (if direction (1- (car pos)) (1+ (car pos))))
    (if (and shot (emvaders-check-shot e))
	(progn
	  (setq enemy-hit e)
	  (emvaders-insert-field pos "   ")
	  (emvaders-set-field shot " "))
      (if (= bomb-at (car pos))
	  (let ((newbomb (cons (1+ bomb-at) (1+ (cdr pos)))))
	    (message "Bombs away!")
	    (if (and shot (equal newbomb shot))
		(progn
		  (emvaders-set-field shot " ")
		  (setq shot nil))
	      (setq bomb-list (cons newbomb bomb-list))
	      (setq bomb-at 0)
	      (emvaders-set-field newbomb emvaders-bomb-string))))
      (emvaders-insert-field pos
			     (if flip-state
				 (car (aref emvaders-enemy-strings (car e)))
			       (cdr (aref emvaders-enemy-strings (car e))))))
    e)
  )

(defun emvaders-down-enemy (e)
  (emvaders-unset-enemy e)
  (let ((new-e (cons (car e) (cons (car (cdr e)) (1+ (cdr (cdr e)))))))
    (if (and shot (emvaders-check-shot new-e))
	(setq enemy-hit new-e))
    (message "Downed")
    (emvaders-set-enemy new-e))
  )

(defun emvaders-unset-enemy (e)
  (emvaders-set-field (cdr e) "   ")
  )

(defun emvaders-set-enemy (e)
  (if flip-state
      (emvaders-set-field (cdr e) (car (aref emvaders-enemy-strings (car e))))
    (emvaders-set-field (cdr e) (cdr (aref emvaders-enemy-strings (car e))))
    )
  e)

(defun emvaders-show-score ()
  (setq global-mode-string (format "Score: %d  Lives: %d" score lives))
  ;; Is this to give the other buffer a chance?
  ;;(save-excursion (set-buffer (other-buffer)))
  ;;(set-buffer-modified-p (buffer-modified-p))
  (sit-for 0)
)

(defun emvaders-show-high-score ()
  (let ((file (substitute-in-file-name emvaders-high-score-file)))
    (find-file-other-window file)
    (goto-char (point-max))
    (insert (format "  %08d %20s at %s on %s\n"
		    score
		    (user-full-name)
		    (current-time-string)
		    (system-name)))
    (sort-fields -1 (point-min) (point-max))
    (goto-line 11)
    (move-to-column 0)
    (delete-region (point) (point-max))
    (write-file file)
    (goto-char (point-min))
    (pop-to-buffer "*Emvaders*")))

(defun emvaders-move-left ()
  (if (> (car ship-pos) (1+ emvaders-left-margin))
      (progn
	(emvaders-delete-field ship-pos 3)
	(setcar ship-pos (1- (car ship-pos)))
	(if (not (= (emvaders-char-at ship-pos) ? ))
	    (progn
	      (setq player-hit t)
	      (setcar ship-pos (1+ (car ship-pos)))
	      (emvaders-insert-field ship-pos emvaders-ship-string))
	  (emvaders-insert-field ship-pos emvaders-ship-string)))))

(defun emvaders-move-right ()
  (if (< (car ship-pos) (1- (1- (+ emvaders-left-margin (* 4 emvaders-width)))))
      (progn
	(emvaders-delete-field ship-pos 3)
	(if (not (= (emvaders-char-at ship-pos) ? ))
	    (progn
	      (setq player-hit t)
	      (emvaders-insert-field ship-pos emvaders-ship-string))
	  (setcar ship-pos (1+ (car ship-pos)))
	  (emvaders-insert-field ship-pos emvaders-ship-string)))))

(defun emvaders-fire ()
  (if shot
      nil
    (setq shot (cons (1+ (car ship-pos)) (1- (cdr ship-pos))))
    (emvaders-set-field shot emvaders-shot-string)))

(defun emvaders-remove-bomb (bomb)
  (emvaders-set-field bomb " ")
  (setq bomb-list (filter '(lambda (b) (not (equal b bomb))) bomb-list)))

(defun emvaders-bomb-at (pos)
  (let ((blist bomb-list) res)
    (while blist
      (if (equal pos (car blist))
	  (setq res (car blist) blist nil)
	(setq blist (cdr blist))))
    res))

(defun emvaders-move-bombs ()
  (setq bomb-list
	(filter
	 'identity
	 (mapcar
	  '(lambda (b)
	     (emvaders-set-field b " ")
;	     (message b)
;	     (message "Bomb moving")
	     (let ((new-pos (cons (car b) (1+ (cdr b)))))
;	       (message new-pos)
	       (if (not (= (emvaders-char-at new-pos) ? ))
		   (cond ((and shot (equal shot b))
			  (emvaders-set-field shot " ")
			  (setq shot nil)
			  nil)
			 ((and (= (cdr new-pos) (- emvaders-height 1))
			       (>= (car new-pos) (car ship-pos))
			       (<= (car new-pos) (+ (car ship-pos) 2)))
			  (setq player-hit t)
			  nil)
			 ((> (cdr new-pos) (- emvaders-height 0))
			  nil) ;;  Shot disappears
			 ((< (cdr new-pos) (- emvaders-height 0))
			  nil) ;; Shot disappears -- must have hit a turret
			 (t nil))
		 (emvaders-set-field new-pos emvaders-bomb-string)
		 new-pos)))
	  bomb-list)))
  )

(defun emvaders-move-shot ()
  (if shot
      (progn
	(emvaders-set-field shot " ")
	(setq shot (cons (car shot) (1- (cdr shot))))
	(if (< (cdr shot) 0)
	    (setq shot nil)
	  (if (not (= (emvaders-char-at shot) ? ))
	      ;; Something is hit
	      (let ((bomb (emvaders-bomb-at shot)))
		(if bomb
		    (progn
		      (emvaders-remove-bomb bomb)
		      (setq shot nil))
		  (let ((enemy (first 'emvaders-check-shot enemies)))
		    (if enemy
			(setq enemy-hit enemy)
		      (setq shot nil) ;; Turret -- not defined yet
		      ))))
	    (emvaders-set-field shot emvaders-shot-string))))
    )
  )

(defun emvaders-quit ()
  (if (y-or-n-p "Are you sure to quit Emvaders? ")
      (progn
	(setq buffer-read-only t)
	(throw 'emvaders-quit-tag (emvaders-exit)))))

(defun emvaders-boss-has-come ()
  ;; Need improvement.
  (save-window-excursion
    (boss-has-come)
    (local-set-key "\C-c\C-c" 'emvaders-boss-goes-away)
    (recursive-edit)))

(defun emvaders-boss-goes-away ()
  (interactive)
  (boss-goes-away)
  (exit-recursive-edit))

