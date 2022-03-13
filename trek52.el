;;; trek52.el --- Star Trek game for Emacs

;; Copyright (c) 2022 Christopher Leyon

;; Version:     0.1
;; Time-stamp:  <2022-03-13 16:03:50 cleyon>
;; Author:      Christopher Leyon <cleyon@gmail.com>
;; Created:     <2022-03-13 15:59:18 cleyon>
;; Keywords:    games

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Installation:
;;;
;;; 1. Add "trek52.el" to a directory in your load-path.
;;;    Byte-compilation is optional but recommended.
;;; 2. `M-x trek52' and follow the instructions.

;;; To do:

;;; Change Log:

;;; Code:

(defconst trek52--version "V06B-02"
  "Trek52 version.
\(For reference, RSTS/E V06B was released January 1977;
version 6C was released July 1978.)")


(with-no-warnings
  (require 'cl))
(require 'ecurses)

(random t)

(defun trek52--rnd ()
  (/ (random most-positive-fixnum)
     (float  most-positive-fixnum)))

(defun trek52--random (n)
  "Generate a random integer in the range 1..N."
  (1+ (random n)))

(defconst trek52--circle/8              (/ float-pi 4.0))
(defconst trek52--initial-energy        3000)
(defconst trek52--initial-shields       0) ; not used
(defconst trek52--initial-torpedoes     10)
(defconst trek52--klingon-initial-energy 1500)
(defconst trek52--password              "SPOCK")

(defconst trek52--max-COORD             8)
(defconst trek52--max-KLINGONS          9)
(defconst trek52--max-SYSTEMS           9)

(defconst trek52--flag-ACTIVE           0)
(defconst trek52--flag-WIN              1)
(defconst trek52--flag-LOSE            -1)

(defconst trek52--obj-EMPTY             0)
(defconst trek52--obj-ENTERPRISE        1)
(defconst trek52--obj-KLINGON           2)
(defconst trek52--obj-STARBASE          3)
(defconst trek52--obj-STAR              4)

(defconst trek52--rpt-UNKNOWN           0)
(defconst trek52--rpt-DAMAGE            1)
(defconst trek52--rpt-BATTLE            2)

(defconst trek52--sys-ENGINES           1)
(defconst trek52--sys-SR-SENSORS        2)
(defconst trek52--sys-LR-SENSORS        3)
(defconst trek52--sys-PHASERS           4)
(defconst trek52--sys-TORPEDOS          5)
(defconst trek52--sys-DAMAGE-RPT        6)
(defconst trek52--sys-SHIELDS           7)
(defconst trek52--sys-COMPUTERS         8)
(defconst trek52--sys-BATTLE-RPT        9)


 
(defvar trek52--condition "Green "
  "Enterprise status color.  Values:
  Green  = Normal operation
  Yellow = Less than 10% energy remaining
  Orange = Klingons in an adjacent quadrant
  Red    = Klingons in current quadrant
  Docked = Docked at starbase")

(defvar trek52--current-energy)
(defvar trek52--current-report)
(defvar trek52--current-shields)        ; not used
(defvar trek52--current-stardate)
(defvar trek52--current-torpedoes)

(defvar trek52--galaxy
  (make-vector (* trek52--max-COORD trek52--max-COORD) 0)
  "Galaxy data; matrix of int[8,8] containing quadrant codes (KBS).
  K = Number of klingons
  B = Number of starbases
  S = Number of stars")

(defvar trek52--initial-stardate)

(defvar trek52--klingon-data
  (make-vector (* trek52--max-KLINGONS 3) 0)
  "Klingons in this sector; vector of int[3].
  [0,1] = Position (y,x)
  [2]   = Energy")
(defvar trek52--klingons-in-game)
(defvar trek52--klingons-in-quadrant  0)
(defvar trek52--klingon-likelihood
  (make-vector trek52--max-KLINGONS 0.0)
  "Vector of 'likelihood' of N Klingons in this quadrant.")

(defvar trek52--mission-duration)
(defvar trek52--privileged-p nil
  "Non-nil if the player currently has privilege to use Xtended commands.")

(defvar trek52--quadrant
  (make-vector (* trek52--max-COORD trek52--max-COORD) 0)
  "Quadrant data; matrix of int[8,8] containing sector objects.
  0 = Empty
  1 = Enterprise
  2 = Klingon
  3 = Starbase
  4 = Star")
(defvar trek52--quadrant-x)
(defvar trek52--quadrant-y)

(defvar trek52--sector-x)
(defvar trek52--sector-y)

(defvar trek52--show-vector-p t
  "Non-nil if the vector to the Klingon is shown in the battle report.")

(defvar trek52--starbases-in-quadrant 0)
(defvar trek52--stars-in-quadrant     0)

(defvar trek52--sys-name
  (make-vector trek52--max-SYSTEMS "")
  "Vector containing system names.")
(defvar trek52--sys-state
  (make-vector trek52--max-SYSTEMS 0)
  "Vector containing system state values.
Zero indicates normal status, any other value indicates damage.")

(defvar trek52--used-x-command-p)

(defvar trek52--win-flag trek52--flag-ACTIVE
  "Status of game.
  1 = Win
  0 = Game in progress / Giving up
 -1 = Lose")

(defvar trek52--x)
(defvar trek52--x*)
(defvar trek52--x-incr)
(defvar trek52--y)
(defvar trek52--y*)
(defvar trek52--y-incr)


(defsubst trek52--convert-8-8-to-N (x y)
  "Convert coordinates (1..8, 1..8) into a zero-based-offset"
  (+ (* 8 (1- x)) (1- y)))

(defun trek52--get-at-coord (array x y)
  (when (not (trek52--coord-in-bounds-p x y))
    (user-error "trek52--get-at-coord: Coordinates out of bounds"))
  (aref array (trek52--convert-8-8-to-N x y)))

(defun trek52--set-at-coord (array x y newelt)
  (when (not (trek52--coord-in-bounds-p x y))
    (user-error "trek52--set-at-coord: Coordinates out of bounds"))
  (aset array (trek52--convert-8-8-to-N x y) newelt))

(defun trek52--get-array (array x)
  (when (or (< x 1) (> x 9))
    (user-error "trek52--get-array: X out of bounds (%d)" x))
  (aref array (1- x)))

(defun trek52--set-array (array x newelt)
  (when (or (< x 1) (> x 9))
    (user-error "trek52--set-array: X out of bounds (%d)" x))
  (aset array (1- x) newelt))

(defsubst trek52--convert-9-3-to-N (x y)
  (+ (* 3 (1- x)) (1- y)))

(defun trek52--get-klingon-data (x y)
  (when (or (< x 1) (> x trek52--max-KLINGONS)
            (< y 1) (> y 3))
    (user-error "trek52--get-klingon-data: Out of bounds"))
  (aref trek52--klingon-data (trek52--convert-9-3-to-N x y)))

(defun trek52--set-klingon-data (x y newelt)
  (when (or (< x 1) (> x trek52--max-KLINGONS)
            (< y 1) (> y 3))
    (user-error "trek52--set-klingon-data: Out of bounds"))
  (aset trek52--klingon-data (trek52--convert-9-3-to-N x y) newelt))

(defun trek52--klingon-y (k)
  (trek52--get-klingon-data k 1))
(defun trek52--klingon-x (k)
  (trek52--get-klingon-data k 2))
(defun trek52--klingon-energy (k)
  (trek52--get-klingon-data k 3))

(defun trek52--set-klingon-energy (k e)
  (trek52--set-klingon-data k 3 e))

(defun trek52--klingon-exists-p (k)
  (/= (trek52--klingon-y k) 0))

(defun trek52--course-valid-p (course)
  (and (>= course 1) (< course 9)))

(defun trek52--docked-p ()
  (string= trek52--condition "Docked"))

(defun trek52--len (s)
  (length s))

(defun trek52--left (s n)
  (if (> n (trek52--len s))
      s
    (substring s 0 n)))
(defun trek52--right (s n)
  (if (> n (trek52--len s))
      s
    (substring s (1- n) (trek52--len s))))
(defun trek52--mid (s start len)
  (trek52--left (trek52--right s start) len))

(defmacro trek52--input (var)
  `(progn
     (curs_set 1)
     (getstr ,var)
     (curs_set 0)))
(defalias 'trek52--val 'string-to-number)

(defvar trek52-mode-map
  (let ((map (make-sparse-keymap)))
  (suppress-keymap map 'nodigits)

  ;; These are the canonical Trek52 command keys
  (define-key map "b"    #'trek52--cmd-b-battle-report)
  (define-key map "d"    #'trek52--cmd-d-damage-control-report)
  (define-key map "e"    #'trek52--cmd-e-end-mission)
  (define-key map "l"    #'trek52--cmd-l-long-range-sensor-scan)
  (define-key map "p"    #'trek52--cmd-p-phasers)
  (define-key map "s"    #'trek52--cmd-s-short-range-sensor-scan)
  (define-key map "t"    #'trek52--cmd-t-torpedos)
  (define-key map "w"    #'trek52--cmd-w-warp-drive)
  (define-key map "x"    #'trek52--cmd-x-xtended-command)

  ;; These other keys invoke functions which are not listed in
  ;; `trek52--list-commands'
  (define-key map "?"    #'trek52--list-commands)
  (define-key map "c"    #'trek52--redraw-display)
  (define-key map "\C-g" #'trek52--no-cmd)
  (define-key map "q"    #'bury-buffer)
  (define-key map "z"    #'trek52--show-time)

  map))

(define-derived-mode trek52-mode fundamental-mode
  "Trek52"
  (setq buffer-read-only nil)
  (initscr 80 24 t)
  (echo))

(defun trek52 ()
  "Rid the galaxy of the deadly Klingon menace."
  (interactive)
  (switch-to-buffer "*TREK52*")
  (trek52-mode)
  (trek52--play-game))

(defun trek52--play-game ()
  (curs_set 0)
  (addstr "\n\n\n")
  (addstr (concat "TREK     " trek52--version "      DEC, El Segundo"))
  (flushinp)
  (sit-for 3)

  (trek52--initialize-variables)
  (trek52--initialize-galaxy)
  (trek52--show-orders)
  (trek52--redraw-display))

(defun trek52--clear-screen ()
  (move 0 0)
  (clrtobot))

(defun trek52--cursor-move-to-screen-pos (x y)
  (move (1- x) (1- y)))

(defun trek52--flash-char (char times interval)
  (loop for x from 1 to times do
        (addstr " ")  (mvleft) (sit-for interval)
        (addstr char) (mvleft) (sit-for interval)))

(defun trek52--cursor-move-to-sector-xy (x y)
  (trek52--cursor-move-to-screen-pos (+ 41 (* (- y 1) 2)) (+ x 3)))

(defun trek52--cursor-move-to-galaxy-xy (x y)
  (trek52--cursor-move-to-screen-pos (* 4 y) (+ (* 2 x) 2)))

(defun trek52--print-at-screen-pos (x y &rest args)
  (trek52--cursor-move-to-screen-pos x y)
  (apply 'printw args))

(defun trek52--msg (&rest args)
  (trek52--clear-msg-area)
  (apply 'trek52--print-at-screen-pos 6 20 args))

(defun trek52--msg2 (s)
  (trek52--print-at-screen-pos 10 21 s))

(defun trek52--distance-to-klingon (k)
  (let ((d1 (- (trek52--klingon-y k) trek52--sector-y))
        (d2 (- (trek52--klingon-x k) trek52--sector-x)))
    (sqrt (+ (* d1 d1)
             (* d2 d2)))))

(defun trek52--coord-in-bounds-p (x y)
  "Non-nil if both X and Y in range 1..8."
  (and (>= x 1) (<= x trek52--max-COORD)
       (>= y 1) (<= y trek52--max-COORD)))

(defun trek52--clip (x)
  (cond ((< x 1) 1)
        ((> x trek52--max-COORD) trek52--max-COORD)
        (t       x)))

(defun trek52--clear-msg-area ()
  (message "")
  (trek52--cursor-move-to-screen-pos 6 20)
  (clrtoeol)
  (trek52--cursor-move-to-screen-pos 1 21)
  (clrtoeol)
  (mvdown)
  (clrtoeol))

(defun trek52--initialize-variables ()
  (setq trek52--initial-stardate        (+ 24000 (truncate (* (trek52--rnd) 600))))
  (setq trek52--mission-duration        (+ 300 (truncate (* (trek52--rnd) 200))))
  (setq trek52--klingons-in-game        0)

  (setq trek52--current-energy          trek52--initial-energy)
  (setq trek52--current-report          trek52--rpt-UNKNOWN)
  (setq trek52--current-shields         trek52--initial-shields) ; not used
  (setq trek52--current-stardate        trek52--initial-stardate)
  (setq trek52--current-torpedoes       trek52--initial-torpedoes)

  (trek52--set-array trek52--sys-name trek52--sys-ENGINES    "WARP ENGINES")
  (trek52--set-array trek52--sys-name trek52--sys-SR-SENSORS "S.R. SENSORS")
  (trek52--set-array trek52--sys-name trek52--sys-LR-SENSORS "L.R. SENSORS")
  (trek52--set-array trek52--sys-name trek52--sys-PHASERS    "PHASER BEAM")
  (trek52--set-array trek52--sys-name trek52--sys-TORPEDOS   "TORPEDO TUBES")
  (trek52--set-array trek52--sys-name trek52--sys-DAMAGE-RPT "DAMAGE REPORT")
  (trek52--set-array trek52--sys-name trek52--sys-SHIELDS    "SHIELDS")
  (trek52--set-array trek52--sys-name trek52--sys-COMPUTERS  "COMPUTERS")
  (trek52--set-array trek52--sys-name trek52--sys-BATTLE-RPT "BATTLE REPORT")

  (loop for sys from 1 to trek52--max-SYSTEMS do
        (trek52--set-system-state sys 0)))

  (trek52--set-array trek52--klingon-likelihood 1  0.0001)
  (trek52--set-array trek52--klingon-likelihood 2  0.01)
  (trek52--set-array trek52--klingon-likelihood 3  0.03)
  (trek52--set-array trek52--klingon-likelihood 4  0.08)
  (trek52--set-array trek52--klingon-likelihood 5  0.28)
  (trek52--set-array trek52--klingon-likelihood 6  1.28)
  (trek52--set-array trek52--klingon-likelihood 7  3.28)
  (trek52--set-array trek52--klingon-likelihood 8  6.28)
  (trek52--set-array trek52--klingon-likelihood 9 13.28)

(defun trek52--system-name (sys)
  (trek52--get-array trek52--sys-name sys))

(defun trek52--system-state (sys)
  (trek52--get-array trek52--sys-state sys))

(defun trek52--set-system-state (sys val)
  (trek52--set-array trek52--sys-state sys val))

(defun trek52--system-damaged-p (sys)
  "Non-nil if system is damaged."
  (/= (trek52--system-state sys) 0))

(defun trek52--obj-at-sector (x y)
  (trek52--get-at-coord trek52--quadrant x y))

(defun trek52--set-obj-at-sector (x y obj)
  (trek52--set-at-coord trek52--quadrant x y obj))

(defun trek52--sector-empty-p (x y)
  (= (trek52--obj-at-sector x y) trek52--obj-EMPTY))

(defun trek52--sector-sprite (x y)
  (aref [" " "E" "K" "B" "*"] (trek52--obj-at-sector x y)))

(defun trek52--quadrant-code (x y)
  (trek52--get-at-coord trek52--galaxy x y))

(defun trek52--set-quadrant-code (x y code)
  (trek52--set-at-coord trek52--galaxy x y code))

(defun trek52--initialize-galaxy ()
  (let ((starbases 0))
    ;; Set quadrant code for each quadrant in galaxy
    (loop for i from 1 to trek52--max-COORD do
          (loop for j from 1 to trek52--max-COORD do
                (setq trek52--klingons-in-quadrant 0)
                (loop for k from 1 to trek52--max-KLINGONS do
                      (when (< (* (trek52--rnd) 64)
                               (trek52--get-array trek52--klingon-likelihood k))
                        (setq trek52--klingons-in-quadrant (1+ trek52--klingons-in-quadrant))))
                (setq trek52--klingons-in-game (+ trek52--klingons-in-game trek52--klingons-in-quadrant))
                (setq trek52--starbases-in-quadrant (if (> (trek52--rnd) 0.90) 1 0))
                (setq starbases (+ starbases trek52--starbases-in-quadrant))
                (trek52--set-quadrant-code i j
                  (+ (* 100 trek52--klingons-in-quadrant)
                     (*  10 trek52--starbases-in-quadrant)
                           (trek52--random 8)))))

    ;; Ensure there's at least one starbase in game
    (when (zerop starbases)
      (let ((i (trek52--random trek52--max-COORD))
            (j (trek52--random trek52--max-COORD)))
        (trek52--set-quadrant-code i j
          (+ 10 (trek52--quadrant-code i j)))))

    ;; Position Enterprise
    (setq trek52--quadrant-x (trek52--random trek52--max-COORD))
    (setq trek52--quadrant-y (trek52--random trek52--max-COORD))
    (setq trek52--sector-x   (trek52--random trek52--max-COORD))
    (setq trek52--sector-y   (trek52--random trek52--max-COORD))

    (trek52--set-up-quadrant)))

(defun trek52--update-stardate (&optional n)
  (when n
    (setq trek52--current-stardate (+ trek52--current-stardate n)))
  (trek52--print-at-screen-pos 49 16 " %.1f" (/ trek52--current-stardate 10.0)))

(defun trek52--update-condition ()
  (setq trek52--condition "Green ")
  (when (< trek52--current-energy (/ trek52--initial-energy 10.0))
    (setq trek52--condition "Yellow"))
  (loop for k from (1- trek52--quadrant-y) to (1+ trek52--quadrant-y) do
        (loop for l from (1- trek52--quadrant-x) to (1+ trek52--quadrant-x) do
              (when (>= (trek52--quadrant-code (trek52--clip k)
                                               (trek52--clip l))
                        100)
                (setq trek52--condition "Orange"))))
  (when (> trek52--klingons-in-quadrant 0)
    (setq trek52--condition "Red   "))
  (loop for k from (1- trek52--sector-y) to (1+ trek52--sector-y) do
        (loop for l from (1- trek52--sector-x) to (1+ trek52--sector-x) do
              (when (= (trek52--obj-at-sector (trek52--clip k)
                                              (trek52--clip l))
                       trek52--obj-STARBASE)
                (setq trek52--condition "Docked"))))
  (trek52--print-at-screen-pos 71 16 trek52--condition))

(defun trek52--update-quadrant-coord ()
  (trek52--print-at-screen-pos 49 17
                               " %d - %d" trek52--quadrant-x trek52--quadrant-y))

(defun trek52--update-sector-coord ()
  (trek52--print-at-screen-pos 49 18
                               " %d - %d" trek52--sector-x trek52--sector-y))

(defun trek52--update-klingons (&optional n)
  (when n
    (setq trek52--klingons-in-quadrant (- trek52--klingons-in-quadrant n))
    (setq trek52--klingons-in-game     (- trek52--klingons-in-game     n)))
  (trek52--print-at-screen-pos 49 19 " %-2d" trek52--klingons-in-game)
  (when (<= trek52--klingons-in-game 0)
    (setq trek52--win-flag trek52--flag-WIN)))

(defun trek52--update-torpedoes (&optional n)
  (when n
    (setq trek52--current-torpedoes (- trek52--current-torpedoes n)))
  (trek52--print-at-screen-pos 71 19 "%-2d" trek52--current-torpedoes))

(defun trek52--update-energy (&optional n)
  (when n
    (setq trek52--current-energy (- trek52--current-energy n)))
  (when (<= trek52--current-energy 0)
    (setq trek52--win-flag trek52--flag-LOSE))
  (trek52--update-condition)
  (trek52--print-at-screen-pos 71 17 "%4d" trek52--current-energy))

(defun trek52--update-shields (&optional n)
  (when n
    (setq trek52--current-shields (+ trek52--current-shields n)))
  (when (< trek52--current-shields 0)
    (setq trek52--current-shields 0))
  (trek52--print-at-screen-pos 71 18 "%d" trek52--current-shields))

(defun trek52--show-orders ()
  (trek52--clear-screen)
  (trek52--cursor-move-to-screen-pos 1 7)
  (mvtab 10)
  (addstr "ORDERS:")
  (mvtab 40)
  (printw "STARDATE = %.1f\n" (/ trek52--current-stardate 10.0))
  (addstr "\n")
  (addstr "AS COMMANDER OF THE UNITED STARSHIP ENTERPRISE, YOUR MISSION\n")
  (addstr "IS TO RID THE GALAXY OF THE DEADLY KLINGON MENACE.  TO DO THIS,\n")
  (printw "YOU MUST DESTROY THE KLINGON INVASION FORCE OF %d BATTLE\n"
          trek52--klingons-in-game)
  (printw "CRUISERS.  YOU HAVE %.1f SOLAR YEARS TO COMPLETE YOUR MISSION.\n"
          (/ trek52--mission-duration 10.0))
  (printw "(I.E. UNTIL STARDATE %.1f)"
          (/ (+ trek52--initial-stardate trek52--mission-duration) 10.0))
  (trek52--cursor-move-to-screen-pos 1 22)
  (addstr "GIVE COMMAND 'E' TO STOP THE GAME EARLY.")
  (trek52--cursor-move-to-screen-pos 1 15)
  (addstr "DO YOU REQUIRE FURTHER INSTRUCTIONS (Y/N)? ")
  (curs_set 1)
  (when (string= (upcase (char-to-string (getch))) "Y")
    (trek52--further-instructions)))

(defun trek52--redraw-display ()
  (interactive)
  (trek52--clear-screen)
  (trek52--print-at-screen-pos 1 23 "COMMAND:")
  (trek52--print-at-screen-pos 1 20 "MSG:")
  (trek52--print-galaxy-map)
  (trek52--print-status-report)
  (trek52--print-quadrant-map)
  (setq trek52--current-report trek52--rpt-UNKNOWN)
  (trek52--cmd-d-damage-control-report))

(defun trek52--update-system-damage (sys &optional n)
  (when n
    (trek52--set-system-state sys (- (trek52--system-state sys) n)))
  (when (> (trek52--system-state sys) 0)
    (trek52--set-system-state sys 0))
  (when (and (= trek52--current-report trek52--rpt-DAMAGE)
             (not (trek52--system-damaged-p trek52--sys-DAMAGE-RPT)))
    (trek52--print-at-screen-pos 75 (+ 3 sys)
                                 " %2d" (trek52--system-state sys)))
  (when (and (trek52--system-damaged-p sys)
             (>= (+ (trek52--system-state sys) (or n 0)) 0))
    (cond ((= sys trek52--sys-SR-SENSORS)
           (trek52--clear-sr-scan))
          ((and (= sys trek52--sys-DAMAGE-RPT)
                (= trek52--current-report trek52--rpt-DAMAGE))
           (trek52--clear-screen-for-report))
          ((and (= sys trek52--sys-BATTLE-RPT)
                (= trek52--current-report trek52--rpt-BATTLE))
           (trek52--clear-screen-for-report))
          ((= sys trek52--sys-COMPUTERS)
           (trek52--computers-damaged)))))

(defun trek52--further-instructions ()
  (let ((help-window-select t))
    (with-help-window "*Trek52 instructions*"
      (princ (substitute-command-keys "\
Command W - Warp drive                         4  3  2
  'Course' is in a circular numerical           \\ | /
  vector arrangement as shown.                   \\|/
  Integer and real values may be             5 ---E--- 1
  used.  Therefore course 1.5 is                 /|\\
  halfway between 1 and 2.                      / | \\
                                               6  7  8
  A vector of 9 is undefined, but
  values may approach 9.                       COURSE

  One 'Warp Factor' is the size of one quadrant on the galaxy map.
  Therefore to get from quadrant 6-5 to 6-3 you would use course 3,
  warp factor 2.

Command L - Long-range sensor scan
  Shows conditions in space for one quadrant on each side of the
  Enterprise in the middle of the scan.  The scan is coded in the
  form KBS, where the K digit is the number of Klingons, the B digit
  is the number of starbases, and the S digit is the number of stars.

Command S - Short-range sensor scan
  Displays the quadrant you are currently in, showing symbols for
  the Enterprise, Klingons, stars, and starbases.
      E = Enterprise          * = Star
      K = Klingon             B = Starbase

Command D - Damage control report
  Gives state of repair of all systems.  A state of repair other
  than zero indicates damage to that device.

Command B - Battle analysis report
  Gives tactical information on all Klingons in the quadrant,
  including sector, distance, energy, and torpedo vector.

Command P - Phaser beam control
  Allows you to destroy the Klingons by hitting them with suitably
  large numbers of energy units to deplete their shield power.
  Keep in mind that when you shoot at them, he gonna do it to you.

Command T - photon Torpedo control
  Course is the same as used in warp engine control.  If you hit
  the Klingon, he is destroyed and cannot fire back at you.  If
  you miss, he will shoot his phasers at you.

Command E - End mission

(Press `\\[quit-window]' to dismiss this help text.)")))))

(defun trek52--post-cmd ()
  (when (/= trek52--win-flag trek52--flag-ACTIVE)
    (trek52--end-game))
  (trek52--cursor-move-to-screen-pos 1 24)
  (clrtoeol)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (curs_set 1)
  (setq buffer-read-only t))

(defun trek52--pre-cmd ()
  (setq buffer-read-only nil)
  (setq trek52--used-x-command-p nil)
  (curs_set 0)
  (trek52--clear-msg-area))

(defun trek52--no-cmd ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--msg "!-!-! PLEASE ENTER A VALID COMMAND - TYPE ? TO LIST COMMANDS !-!-!")
  (trek52--post-cmd))

(defun trek52--show-time ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--msg "THE TIME IS %s" (format-time-string "%I:%M %p"))
  (trek52--post-cmd))

(defun trek52--cmd-e-end-mission ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "END MISSION")
  (trek52--print-at-screen-pos 40 23 "CONFIRM (Y/N)? ")
  (curs_set 1)
  (let (confirm)
    (setq confirm (upcase (char-to-string (getch))))
    (curs_set 0)
    (if (string= confirm "Y")
        (trek52--end-game)
      (trek52--msg "COMMAND CANCELLED !")
      (trek52--post-cmd))))

(defun trek52--list-commands ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--msg "VALID COMMANDS ARE: W,L,S,D,B,P,T AND E")
  (trek52--post-cmd))

(defun trek52--cmd-x-xtended-command ()
  (interactive)
  (trek52--pre-cmd)
  (setq trek52--used-x-command-p t)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "XTENDED INSTRUCTIONS")
  (if trek52--privileged-p
      (trek52--x-sub-command)
    (trek52--password-prompt)
    (when trek52--privileged-p
      (trek52--x-sub-command)))
  (trek52--post-cmd))

(defun trek52--x-sub-command ()
  (trek52--cursor-move-to-screen-pos 40 23)
  (clrtoeol)
  (addstr "SUB-COMMAND: ")
  (let (sub-command)
    (trek52--input sub-command)
    (setq sub-command (upcase (string-trim sub-command)))
    (let ((xcmd-letter (trek52--left sub-command 1)))
      (cond ((string= xcmd-letter "C") (trek52--xcmd-c-reset))
            ((string= xcmd-letter "D") (trek52--xcmd-d-repair-damage sub-command))
            ((string= xcmd-letter "E") (trek52--xcmd-e-erase-galaxy-map))
            ((string= xcmd-letter "F") (trek52--xcmd-f-find sub-command))
            ((string= xcmd-letter "G") (trek52--print-galaxy-map))
            ((string= xcmd-letter "L") (trek52--xcmd-l-show-galaxy-map))
            ((string= xcmd-letter "Q") (trek52--print-quadrant-map))
            ((string= xcmd-letter "R") (trek52--print-status-report))
            ((string= xcmd-letter "V") (trek52--xcmd-v-toggle-vector sub-command))
            ((string= xcmd-letter "W") (trek52--xcmd-w-direct-warp sub-command))
            ((string= xcmd-letter "X") (trek52--xcmd-x-disable-privileges))
            ((> (trek52--len sub-command) 0)
             (trek52--msg "!! UNRECOGNIZED SUB-COMMAND (%s) !!" sub-command))))))

(defun trek52--xcmd-c-reset ()
  (trek52--msg "ENERGY AND TORPEDOES RESET")
  (trek52--update-energy    (- trek52--current-energy    trek52--initial-energy))
  (trek52--update-torpedoes (- trek52--current-torpedoes trek52--initial-torpedoes)))

(defun trek52--xcmd-d-repair-damage (sub-command)
  (if (/= (trek52--len sub-command) 3)
      (trek52--msg "!! INVALID SUB-COMMAND (%s) !!" sub-command)
    (let ((x (trek52--val (trek52--mid   sub-command 2 1)))
          (y (trek52--val (trek52--right sub-command 3))))
      (trek52--update-system-damage x (- y))
      (trek52--msg "STATE OF %s SET TO %d" (trek52--system-name x) (trek52--system-state x)))))

(defun trek52--password-prompt ()
  (trek52--print-at-screen-pos 40 23 "PASSWORD? ")
  (let ((ecurses--echo ?\*)
        response)
    (trek52--input response)
    (setq response (upcase response))
    (if (string= response trek52--password)
        (setq trek52--privileged-p t)
      (trek52--msg "!!! ACCESS  DENIED !!!")
      (trek52--msg2 "CONTACT THE STARSHIP COMMANDER FOR APPROVAL OF ACCESS PRIVILEGES"))))

(defun trek52--xcmd-v-toggle-vector (sub-command)
  (cond ((string= (trek52--right sub-command 2) "ON")
         (setq trek52--show-vector-p t)
         (trek52--msg "** VECTOR ON"))
        ((string= (trek52--right sub-command 2) "OFF")
         (setq trek52--show-vector-p nil)
         (trek52--msg "** VECTOR OFF")))
  (trek52--display-battle-report))

(defun trek52--xcmd-l-show-galaxy-map ()
  (loop for i from 1 to trek52--max-COORD do
        (loop for j from 1 to trek52--max-COORD do
              (trek52--print-single-quadrant-code i j))))

(defun trek52--xcmd-e-erase-galaxy-map ()
  (loop for i from 1 to trek52--max-COORD do
        (loop for j from 1 to trek52--max-COORD do
              (trek52--cursor-move-to-galaxy-xy i j)
              (addstr "   ")))
  (trek52--cursor-move-to-galaxy-xy trek52--quadrant-y trek52--quadrant-x)
  (addstr " E "))

(defun trek52--xcmd-x-disable-privileges ()
  (setq trek52--privileged-p nil)
  (trek52--msg "*** PRIVILEGES TERMINATED ***"))

(defun trek52--xcmd-f-find (sub-command)
  (let ((find-char (trek52--mid sub-command 2 1))
        offset)
    (cond ((string= find-char "K") (setq offset 0))
          ((string= find-char "B") (setq offset 1))
          ((string= find-char "*") (setq offset 2))
          (t (trek52--msg "- FIND CHARACTER MUST BE 'K', 'B' OR '*' -")))
    (when offset
      (loop for i from 1 to trek52--max-COORD do
            (loop for j from 1 to trek52--max-COORD do
                  (when (or (/= trek52--quadrant-y i)
                            (/= trek52--quadrant-x j))
                    (let* ((code (format "%03d" (trek52--quadrant-code i j)))
                           (desired (substring code offset (1+ offset))))
                      (trek52--cursor-move-to-galaxy-xy i j)
                      (if (string= desired "0")
                          (addstr "   ")
                        (printw " %s " desired)))))))))

(defun trek52--xcmd-w-direct-warp (sub-command)
  (if (/= (trek52--len sub-command) 5)
      (trek52--msg "!! INVALID SUB-COMMAND (%s) !!" sub-command)
    (trek52--clean-up-old-quadrant)
    (setq trek52--quadrant-x (trek52--val (trek52--mid sub-command 2 1)))
    (setq trek52--quadrant-y (trek52--val (trek52--mid sub-command 3 1)))
    (setq trek52--sector-x   (trek52--val (trek52--mid sub-command 4 1)))
    (setq trek52--sector-y   (trek52--val (trek52--mid sub-command 5 1)))
    (trek52--set-up-in-new-quadrant)))

(defun trek52--cmd-w-warp-drive ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "WARP DRIVE")
  (let (course      good-course
        warp-factor good-warp-factor)
    (while (or (not good-course)
               (not good-warp-factor))
      (unless good-course
        (trek52--print-at-screen-pos 30 23 "COURSE? ")
        (trek52--input course)
        (setq course (trek52--val course))
        (setq good-warp-factor nil))
      (while (not good-warp-factor)
        (trek52--print-at-screen-pos 50 23 "WARP FACTOR? ")
        (clrtoeol)
        (trek52--input warp-factor)
        (setq warp-factor (trek52--val warp-factor))
        (cond ((zerop (* warp-factor course))
               (trek52--msg "COMMAND CANCELLED !")
               (setq good-course t
                     good-warp-factor t))
              ((or (not (trek52--course-valid-p course))
                   (<= warp-factor 0)
                   (>  warp-factor 9))
               (trek52--msg "COURSE DATA DOES NOT COMPUTE - PLEASE REENTER")
               (trek52--cursor-move-to-screen-pos 20 23)
               (clrtoeol)
               (setq good-course nil
                     good-warp-factor t))
              ((and (trek52--system-damaged-p trek52--sys-ENGINES)
                    (> warp-factor .2))
               (trek52--msg "WARP ENGINES ARE DAMAGED")
               (mvtab 10)
               (addstr "MAX SPEED (WARP FACTOR) = 0.2")
               (setq good-course t
                     good-warp-factor nil))
              (t (setq good-course t
                       good-warp-factor t)))))
    (when (and (> warp-factor 0)
               (> course 0))
      (catch 'new-quadrant
        (trek52--engage-engines course warp-factor))))
  (trek52--post-cmd))

(defun trek52--engage-engines (course warp-factor)
  (when (< (trek52--rnd) .25)
    (trek52--travel-disasters))
  (loop for sys from 1 to trek52--max-SYSTEMS do
        (when (trek52--system-damaged-p sys)
          (trek52--update-system-damage sys -1)))
  (let ((sectors-to-travel (truncate (* warp-factor trek52--max-COORD))))
    (let ((travel-time      sectors-to-travel)
          (travel-energy (* sectors-to-travel 20)))
      (when (>= sectors-to-travel trek52--max-COORD)
        (setq travel-time    10
              travel-energy 100))
      (trek52--update-stardate travel-time)
      (trek52--update-energy   travel-energy))
    (when (or (<= trek52--current-energy 0)
              (> trek52--current-stardate (+ trek52--initial-stardate trek52--mission-duration)))
      (setq trek52--win-flag trek52--flag-LOSE)
      (trek52--end-game))
    (trek52--set-obj-at-sector trek52--sector-y trek52--sector-x trek52--obj-EMPTY)
    (trek52--cursor-move-to-sector-xy trek52--sector-y trek52--sector-x)
    (addstr " ")
    (setq trek52--y (float trek52--sector-y))
    (setq trek52--x (float trek52--sector-x))
    (trek52--calculate-travel-increments course)
    (loop for i from 1 to sectors-to-travel do
          (setq trek52--y (+ trek52--y trek52--y-incr))
          (setq trek52--x (+ trek52--x trek52--x-incr))
          (let ((y* (truncate (+ trek52--y .5)))
                (x* (truncate (+ trek52--x .5))))
            (when (not (trek52--coord-in-bounds-p y* x*))
              (trek52--travel-to-new-quadrant warp-factor)
              (throw 'new-quadrant t))
            (when (not (trek52--sector-empty-p y* x*))
              (trek52--msg "ENTERPRISE BLOCKED BY OBJECT AT SECTOR %d-%d" x* y*)
              (setq trek52--y (- trek52--y trek52--y-incr))
              (setq trek52--x (- trek52--x trek52--x-incr))
              (setq trek52--y* (truncate (+ trek52--y .5)))
              (setq trek52--x* (truncate (+ trek52--x .5)))
              (trek52--travel-within-quadrant)
              (cl-return))))
    (trek52--travel-within-quadrant)))

(defun trek52--travel-within-quadrant ()
  (setq trek52--sector-y (trek52--clip (truncate (+ trek52--y .5))))
  (setq trek52--sector-x (trek52--clip (truncate (+ trek52--x .5))))
  (trek52--set-obj-at-sector trek52--sector-y trek52--sector-x trek52--obj-ENTERPRISE)
  (when (not (trek52--system-damaged-p trek52--sys-SR-SENSORS))
    (trek52--cursor-move-to-sector-xy trek52--sector-y trek52--sector-x)
    (addstr "E"))
  (trek52--update-sector-coord)
  (trek52--update-condition)
  (if (trek52--docked-p)
      (trek52--dock-at-starbase)
    (trek52--travel-disasters)
    (trek52--klingons-attack)))

(defun trek52--travel-to-new-quadrant (warp-factor)
  (trek52--clean-up-old-quadrant)
  (trek52--calculate-new-position warp-factor)
  (trek52--set-up-in-new-quadrant))

(defun trek52--clean-up-old-quadrant ()
  (trek52--cursor-move-to-galaxy-xy trek52--quadrant-y trek52--quadrant-x)
  (printw "%03d" (trek52--quadrant-code trek52--quadrant-y trek52--quadrant-x))
  (trek52--clear-sr-scan))

(defun trek52--calculate-new-position (warp-factor)
  (setq trek52--y (+ (float trek52--quadrant-y) (* warp-factor trek52--y-incr) (/ (- (float trek52--sector-y) 0.5) 8.0)))
  (setq trek52--x (+ (float trek52--quadrant-x) (* warp-factor trek52--x-incr) (/ (- (float trek52--sector-x) 0.5) 8.0)))
  (setq trek52--y* (truncate trek52--y))
  (setq trek52--x* (truncate trek52--x))
  (setq trek52--sector-y (trek52--clip (truncate (+ (* 8.0 (- trek52--y trek52--y*)) 0.5))))
  (setq trek52--sector-x (trek52--clip (truncate (+ (* 8.0 (- trek52--x trek52--x*)) 0.5))))
  (setq trek52--quadrant-y (trek52--clip trek52--y*))
  (setq trek52--quadrant-x (trek52--clip trek52--x*)))

(defun trek52--set-up-in-new-quadrant ()
  (trek52--cursor-move-to-galaxy-xy trek52--quadrant-y trek52--quadrant-x)
  (addstr " E ")
  (trek52--update-quadrant-coord)
  (trek52--update-sector-coord)
  (trek52--set-up-quadrant)
  (trek52--display-sr-scan)
  (trek52--update-condition)
  (when (= trek52--current-report trek52--rpt-BATTLE)
    (trek52--clear-report-area)
    (trek52--display-battle-report))
  (if (trek52--docked-p)
      (trek52--dock-at-starbase)
    (trek52--travel-disasters))
  (trek52--klingons-attack))

(defun trek52--travel-disasters ()
  (when (and (<= (trek52--rnd) .20)
             (not trek52--used-x-command-p))
    (if (> (trek52--rnd) .30)
        (trek52--space-storms)
      (trek52--temporary-truce))
    (sit-for 3)))

(defun trek52--space-storms ()
  (let ((sys (trek52--random trek52--max-SYSTEMS)))
    (trek52--msg "*** SPACE STORM - %s DAMAGED ***" (trek52--system-name sys))
    (trek52--update-system-damage sys (trek52--random 5))))

(defun trek52--temporary-truce ()
  (let (sys-to-repair)
    (loop for sys from 1 to trek52--max-SYSTEMS do
          (when (trek52--system-damaged-p sys)
            (setq sys-to-repair sys)
            (cl-return)))
    (when sys-to-repair
      (trek52--update-system-damage sys-to-repair (truncate (- (* (trek52--rnd) (trek52--system-state sys-to-repair)) 1.0)))
      (trek52--msg "*** TEMPORARY TRUCE - %s STATE OF REPAIR IMPROVED ***" (trek52--system-name sys-to-repair)))))

(defun trek52--print-galaxy-map ()
  (move 0 0)
  (mvtab 10)
  (addstr ">>> GALAXY  MAP <<<\n")
  (addstr "  ")
  (loop for i from 1 to trek52--max-COORD do
        (printw "  %d " i))
  (addstr "\n")
  (addstr "  ")
  (loop for i from 1 to trek52--max-COORD do
        (addstr "+---"))
  (addstr "+\n")
  (loop for i from 1 to trek52--max-COORD do
        (printw "%2d" i)
        (loop for j from 1 to trek52--max-COORD do
              (addstr "|   "))
        (addstr "|\n")
        (addstr "  ")
        (loop for j from 1 to trek52--max-COORD do
              (addstr "+---"))
        (addstr "+\n"))
  (trek52--cursor-move-to-galaxy-xy trek52--quadrant-y trek52--quadrant-x)
  (addstr " E "))

(defun trek52--print-quadrant-map ()
  (trek52--print-at-screen-pos 39 1 " >>QUADRANT  MAP<<")
  (trek52--print-at-screen-pos 39 2 " ")
  (loop for i from 1 to trek52--max-COORD do
        (printw " %d" i))
  (trek52--print-at-screen-pos 39 3 " +---------------+")
  (loop for i from 1 to trek52--max-COORD do
        (trek52--cursor-move-to-screen-pos 39 (+ 3 i))
        (printw "%d" i)
        (addstr "|")
        (trek52--print-at-screen-pos 56 (+ 3 i) "|"))
  (trek52--print-at-screen-pos 39 12 " +---------------+"))

(defun trek52--cmd-l-long-range-sensor-scan ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "LONG RANGE SENSOR SCAN")
  (if (trek52--system-damaged-p trek52--sys-LR-SENSORS)
      (trek52--msg "LONG RANGE SENSORS ARE INOPERABLE")
    (trek52--update-energy 5)
    (loop for i from (- trek52--quadrant-y 1) to (+ trek52--quadrant-y 1) do
          (loop for j from (- trek52--quadrant-x 1) to (+ trek52--quadrant-x 1) do
                (trek52--print-single-quadrant-code i j))))
  (trek52--post-cmd))

(defun trek52--print-single-quadrant-code (i j)
  (unless (or (not (trek52--coord-in-bounds-p i j))
              (and (= i trek52--quadrant-y)
                   (= j trek52--quadrant-x)))
    (trek52--cursor-move-to-galaxy-xy i j)
    (printw "%03d" (trek52--quadrant-code i j))))

(defun trek52--print-status-report ()
  (trek52--print-at-screen-pos 40 14 ">> STARSHIP ENTERPRISE STATUS REPORT <<")
  (trek52--print-at-screen-pos 40 16 "STARDATE:")
  (trek52--print-at-screen-pos 60 16 "CONDITION:")
  (trek52--print-at-screen-pos 40 17 "QUADRANT:")
  (trek52--print-at-screen-pos 60 17 "ENERGY:    ")
  (trek52--print-at-screen-pos 40 18 "SECTOR:  ")
  (trek52--print-at-screen-pos 60 18 "SHIELDS:")
  (trek52--print-at-screen-pos 40 19 "KLINGONS:")
  (trek52--print-at-screen-pos 60 19 "TORPEDOS:")
  (trek52--update-stardate)
  (trek52--update-quadrant-coord)
  (trek52--update-sector-coord)
  (trek52--update-condition)
  (trek52--update-energy)
  (trek52--update-shields)
  (trek52--update-torpedoes)
  (trek52--update-klingons))

(defun trek52--cmd-s-short-range-sensor-scan ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "SHORT RANGE SENSOR SCAN")
  (if (trek52--system-damaged-p trek52--sys-SR-SENSORS)
      (trek52--msg "SHORT RANGE SENSORS ARE INOPERABLE")
    (trek52--display-sr-scan))
  (trek52--post-cmd))

(defun trek52--cmd-d-damage-control-report ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "DAMAGE CONTROL REPORT")
  (cond ((trek52--system-damaged-p trek52--sys-DAMAGE-RPT)
         (trek52--msg "DAMAGE CONTROL SYSTEM IS NOT OPERATIONAL"))
        ((= trek52--current-report trek52--rpt-DAMAGE)
         (trek52--msg "DAMAGE REPORT IS ON SCREEN AND BEING UPDATED"))
        (t (trek52--display-damage-report)))
  (trek52--post-cmd))

(defun trek52--display-damage-report ()
  (trek52--clear-screen-for-report)
  (setq trek52--current-report trek52--rpt-DAMAGE)
  (trek52--print-at-screen-pos 60 1 ">>DAMAGE  REPORT<<")
  (trek52--print-at-screen-pos 60 2 "SYSTEM")
  (trek52--print-at-screen-pos 73 2 "STATE")
  (trek52--print-at-screen-pos 60 3 "------")
  (trek52--print-at-screen-pos 73 3 "-----")
  (loop for sys from 1 to trek52--max-SYSTEMS do
        (trek52--print-at-screen-pos 60 (+ 3 sys)
                                     (trek52--system-name sys)))
  (loop for s from 1 to trek52--max-SYSTEMS do
        (trek52--update-system-damage s 0)))

(defun trek52--cmd-b-battle-report ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "BATTLE ANALYSIS REPORT")
  (trek52--display-battle-report)
  (trek52--post-cmd))

(defun trek52--display-battle-report ()
  (if (trek52--system-damaged-p trek52--sys-BATTLE-RPT)
      (trek52--msg "BATTLE REPORT SYSTEM IS NOT OPERATIONAL")
    (when (/= trek52--current-report trek52--rpt-BATTLE)
      (trek52--clear-screen-for-report)
      (setq trek52--current-report trek52--rpt-BATTLE)
      (trek52--print-at-screen-pos 60 1 ">>BATTLE  REPORT<<")
      (trek52--print-at-screen-pos 60 2 "SEC DIST VECT ENER")
      (trek52--print-at-screen-pos 60 3 "--- ---- ---- ----"))
    (loop for k from 1 to trek52--max-KLINGONS do
          (when (trek52--klingon-exists-p k)
            (trek52--print-klingon-details k)
            (trek52--update-klingon-energy k 0)))))

(defun trek52--cmd-p-phasers ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "PHASER BEAM CONTROL")
  (cond ((trek52--system-damaged-p trek52--sys-PHASERS)
         (trek52--msg "PHASER CONTROL IS DISABLED"))
        ((zerop trek52--klingons-in-quadrant)
         (trek52--msg "NO KLINGONS WITHIN PHASER RANGE"))
        (t (let (phaser-energy good)
             (trek52--msg "PHASERS LOCKED ON TARGET")
             (while (not good)
               (trek52--print-at-screen-pos 40 23 "ENERGY UNITS? ")
               (trek52--input phaser-energy)
               (setq phaser-energy (trek52--val phaser-energy))
               (when (<= phaser-energy 0)
                 (setq phaser-energy 0)
                 (setq good t))
               (if (<= phaser-energy trek52--current-energy)
                   (setq good t)
                 (trek52--msg "ENERGY AVAILABLE IS ONLY %d UNITS" trek52--current-energy)))
             (when (> phaser-energy 0)
               (trek52--fire-phasers phaser-energy)))))
  (trek52--post-cmd))

(defun trek52--fire-phasers (phaser-energy)
  (trek52--update-energy phaser-energy)
  (loop for k from 1 to trek52--max-KLINGONS do
        (when (> (trek52--klingon-energy k) 0)
          (let ((energy-received-by-klingon (truncate (* (/ phaser-energy (trek52--distance-to-klingon k)) (+ 2 (trek52--rnd))))))
            (trek52--toggle-klingon-pointer k t)
            (trek52--msg "%d UNIT HIT ON KLINGON AT SECTOR %d-%d"
                         energy-received-by-klingon (trek52--klingon-x k)
                                                    (trek52--klingon-y k))
            (trek52--update-klingon-energy k energy-received-by-klingon)
            (if (> (trek52--klingon-energy k) 0)
                (trek52--print-at-screen-pos 12 21 "- KLINGON SURVIVED ATTACK -")
              (trek52--print-at-screen-pos 12 21 "!!! KLINGON DESTROYED !!!")
              (trek52--remove-klingon-from-game k))
            (sit-for 2)
            (trek52--toggle-klingon-pointer k nil))))
  (trek52--update-condition)
  (trek52--klingons-attack))

(defun trek52--cmd-t-torpedos ()
  (interactive)
  (trek52--pre-cmd)
  (trek52--cursor-move-to-screen-pos 9 23)
  (clrtoeol)
  (addstr "PHOTON TORPEDO CONTROL")
  (cond ((trek52--system-damaged-p trek52--sys-TORPEDOS)
         (trek52--msg "PHOTON TUBES ARE NOT OPERATIONAL"))
        ((<= trek52--current-torpedoes 0)
         (trek52--msg "ALL PHOTON TORPEDOS HAVE BEEN USED"))
        (t (trek52--msg "PHOTON TUBES LOADED")
           (let (course good-course)
             (while (not good-course)
               (trek52--cursor-move-to-screen-pos 40 23)
               (clrtoeol)
               (addstr "COURSE? ")
               (trek52--input course)
               (setq course (trek52--val course))
               (when (zerop course)
                 (setq good-course t))
               (if (trek52--course-valid-p course)
                   (setq good-course t)
                 (trek52--msg "INVALID COURSE VECTOR GIVEN - PLEASE REENTER")))
             (when (and good-course (> course 0))
               (trek52--fire-torpedos course)))))
  (trek52--post-cmd))

(defun trek52--fire-torpedos (course)
  (trek52--calculate-travel-increments course)
  (setq trek52--y trek52--sector-y)
  (setq trek52--x trek52--sector-x)
  (trek52--msg "TORPEDO FIRED !!!")  (ding)
  (trek52--update-torpedoes 1)
  (trek52--track-torpedo)
  (when (trek52--coord-in-bounds-p trek52--y* trek52--x*)
    (let* ((hit-obj  (trek52--obj-at-sector trek52--y* trek52--x*))
           (obj-char (trek52--sector-sprite trek52--y* trek52--x*)))
      (trek52--cursor-move-to-sector-xy trek52--y* trek52--x*)
      (trek52--flash-char obj-char 5 0.2)
      (addstr " ")
      (cond ((string= obj-char "E") (trek52--destroy-enterprise))
            ((string= obj-char "K") (trek52--destroy-klingon))
            ((string= obj-char "B") (trek52--destroy-starbase))
            ((string= obj-char "*") (trek52--destroy-star))
            (t (error "Invalid sector object '%s'" obj-char)))
      (sit-for 1)
      (trek52--set-obj-at-sector trek52--y* trek52--x* trek52--obj-EMPTY)
      (trek52--set-quadrant-code trek52--quadrant-y trek52--quadrant-x
        (+ (* 100 trek52--klingons-in-quadrant)
           (*  10 trek52--starbases-in-quadrant)
                  trek52--stars-in-quadrant))))
  (trek52--update-condition)
  (trek52--klingons-attack))

(defun trek52--track-torpedo ()
  (let (done)
    (while (not done)
      (setq trek52--y (+ trek52--y trek52--y-incr))
      (setq trek52--x (+ trek52--x trek52--x-incr))
      (setq trek52--y* (truncate (+ trek52--y .5)))
      (setq trek52--x* (truncate (+ trek52--x .5)))
      (if (not (trek52--coord-in-bounds-p trek52--y* trek52--x*))
          (progn
            (trek52--msg "TORPEDO MISSED TARGET !")  (ding)
            (sit-for 2)
            (setq done t))
        (trek52--cursor-move-to-sector-xy trek52--y* trek52--x*) (addstr ".") (mvleft) (napms 150)
        (trek52--cursor-move-to-sector-xy trek52--y* trek52--x*) (addstr " ") (mvleft) (napms 150)
        (when (not (trek52--sector-empty-p trek52--y* trek52--x*))
          (setq done t))))))

(defun trek52--destroy-star ()
  (trek52--msg2 "STAR DESTROYED")
  (setq trek52--stars-in-quadrant (1- trek52--stars-in-quadrant)))

(defun trek52--destroy-starbase ()
  (trek52--msg2 "*** STARBASE DESTROYED - CONGRATULATIONS ***")
  (setq trek52--starbases-in-quadrant (1- trek52--starbases-in-quadrant)))

(defun trek52--destroy-enterprise ()
  (trek52--msg2 "ENTERPRISE DESTROYED - WHAT A FEAT !!!")
  (setq trek52--win-flag trek52--flag-LOSE)
  (trek52--end-game))

(defun trek52--destroy-klingon ()
  (trek52--msg2 "*** KLINGON KREAMED ***")
  (trek52--update-klingons 1)
  (loop for k from 1 to trek52--max-KLINGONS do
        (when (and (= (trek52--klingon-y k) trek52--y*)
                   (= (trek52--klingon-x k) trek52--x*))
          (trek52--update-klingon-energy k (trek52--klingon-energy k)))))

(defun trek52--set-up-quadrant ()
  (setq trek52--klingons-in-quadrant  0)
  (setq trek52--starbases-in-quadrant 0)
  (setq trek52--stars-in-quadrant     0)

  ;; Set variables based on quadrant code
  (when (trek52--coord-in-bounds-p trek52--quadrant-y trek52--quadrant-x)
    (let ((code (trek52--quadrant-code trek52--quadrant-y trek52--quadrant-x)))
      (setq trek52--klingons-in-quadrant  (truncate (/ code 100.0)))
      (setq trek52--starbases-in-quadrant (- (truncate (/ code 10.0)) (* trek52--klingons-in-quadrant 10)))
      (setq trek52--stars-in-quadrant     (- code (* (truncate (/ code 10.0)) 10)))))

  ;; Clear quadrant
  (loop for x from 1 to trek52--max-COORD do
        (loop for y from 1 to trek52--max-COORD do
              (trek52--set-obj-at-sector x y trek52--obj-EMPTY)))

  ;; Position Enterprise
  (trek52--set-obj-at-sector trek52--sector-y trek52--sector-x trek52--obj-ENTERPRISE)

  ;; Initialize Klingon data for new quadrant
  (loop for k from 1 to trek52--max-KLINGONS do
        (trek52--set-klingon-data k 1 0)
        (trek52--set-klingon-data k 2 0)
        (trek52--set-klingon-energy k   0))

  ;; Position Klingons
  (loop for k from 1 to trek52--klingons-in-quadrant do
        (let* ((sector (trek52--find-random-empty-sector))
               (r1 (car sector))
               (r2 (cdr sector)))
          (trek52--set-obj-at-sector r1 r2 trek52--obj-KLINGON)
          (trek52--set-klingon-data k 1 r1)
          (trek52--set-klingon-data k 2 r2)
          (trek52--set-klingon-energy k (+ trek52--klingon-initial-energy
                                           (truncate (* (trek52--rnd) 1000))))))
  ;; Position starbases
  (loop for b from 1 to trek52--starbases-in-quadrant do
        (let ((sector (trek52--find-random-empty-sector)))
          (trek52--set-obj-at-sector (car sector) (cdr sector) trek52--obj-STARBASE)))

  ;; Position stars
  (loop for s from 1 to trek52--stars-in-quadrant do
        (let ((sector (trek52--find-random-empty-sector)))
          (trek52--set-obj-at-sector (car sector) (cdr sector) trek52--obj-STAR))))

(defun trek52--find-random-empty-sector ()
  (let (r1 r2 done)
    (while (not done)
      (setq r1 (trek52--random trek52--max-COORD))
      (setq r2 (trek52--random trek52--max-COORD))
      (when (trek52--sector-empty-p r1 r2)
        (setq done t)))
    (cons r1 r2)))

(defun trek52--remove-klingon-from-game (k)
  (trek52--cursor-move-to-sector-xy (trek52--klingon-y k) (trek52--klingon-x k))
  (trek52--flash-char "K" 5 0.25)
  (trek52--set-obj-at-sector (trek52--klingon-y k) (trek52--klingon-x k) trek52--obj-EMPTY)
  (trek52--set-quadrant-code trek52--quadrant-y trek52--quadrant-x
                             (- (trek52--quadrant-code trek52--quadrant-y trek52--quadrant-x) 100))
  (trek52--cursor-move-to-sector-xy (trek52--klingon-y k) (trek52--klingon-x k))
  (addstr " ")
  (trek52--update-klingons 1))

(defun trek52--calculate-travel-increments (course)
  (let ((angle (* (1- course) trek52--circle/8)))
    (setq trek52--y-incr (- (sin angle)))
    (setq trek52--x-incr    (cos angle))))

(defun trek52--klingons-attack ()
  (when (and (> trek52--klingons-in-quadrant 0)
             (not trek52--used-x-command-p))
    (if (trek52--docked-p)
        (progn
          (trek52--msg "** KLINGON ATTACK IN PROGRESS **")
          (trek52--print-at-screen-pos 6 21 "- ENTERPRISE PROTECTED BY STARBASE FORCE FIELD -"))
      (let ((total-attack-energy  0)
            (attacking-ship-count 0)
            k-attack-energy
            enterprise-absorbed-energy)
        (loop for k from 1 to trek52--max-KLINGONS do
              (when (and (> (trek52--klingon-energy k) 0)
                         (>= (trek52--rnd) 0.3))
                (setq k-attack-energy (/ (trek52--klingon-energy k) (+ (* (trek52--rnd) 5) 2)))
                (setq enterprise-absorbed-energy (truncate (* (/ k-attack-energy (trek52--distance-to-klingon k)) (+ 2 (trek52--rnd)))))
                (setq total-attack-energy (+ total-attack-energy enterprise-absorbed-energy))
                (setq attacking-ship-count (1+ attacking-ship-count))
                (trek52--toggle-klingon-pointer k t)
                (trek52--msg "*** ATTACK FROM KLINGON AT SECTOR %d-%d ***"
                     (trek52--klingon-x k) (trek52--klingon-y k))
                (trek52--update-klingon-energy k k-attack-energy)
                (trek52--cursor-move-to-sector-xy (trek52--klingon-y k) (trek52--klingon-x k))
                (trek52--flash-char "K" 7 0.1)
                (when (trek52--system-damaged-p trek52--sys-SR-SENSORS)
                  (addstr " "))
                (trek52--update-energy enterprise-absorbed-energy)
                (trek52--print-at-screen-pos 6 21 " - HIT OF %d ENERGY UNITS SUSTAINED -" enterprise-absorbed-energy)
                (sit-for 3)
                (trek52--toggle-klingon-pointer k nil)))
        (when (/= trek52--win-flag trek52--flag-ACTIVE)
          (trek52--end-game))
        (when (>= attacking-ship-count 2)
          (trek52--msg "!! ATTACK HAS CEASED - RETURN TO DUTY STATIONS !!")
          (trek52--print-at-screen-pos 6 21 "%d ENERGY UNITS ABSORBED DURING ATTACK BY %d ENEMY SHIPS -"
                                            total-attack-energy attacking-ship-count))))))

(defun trek52--clear-sr-scan ()
  (loop for i from 1 to trek52--max-COORD do
        (loop for j from 1 to trek52--max-COORD do
              (when (not (trek52--sector-empty-p i j))
                (trek52--cursor-move-to-sector-xy i j)
                (addstr " ")))))

(defun trek52--display-sr-scan ()
  (when (not (trek52--system-damaged-p trek52--sys-SR-SENSORS))
    (loop for i from 1 to trek52--max-COORD do
          (loop for j from 1 to trek52--max-COORD do
                (when (not (trek52--sector-empty-p i j))
                  (trek52--cursor-move-to-sector-xy i j)
                  (addstr (trek52--sector-sprite i j)))))))

(defun trek52--dock-at-starbase ()
  (trek52--msg "** ENTERPRISE SUCCESSFULLY DOCKED AT STARBASE **")
  (trek52--print-at-screen-pos 6 21 "-- CREW AUTHORIZED 3 DAYS GROUND LEAVE -- NEW SUPPLIES BEING LOADED --")
  (trek52--update-energy (- trek52--current-energy trek52--initial-energy))
  (trek52--update-torpedoes (- trek52--current-torpedoes trek52--initial-torpedoes))
  (when (> (trek52--rnd) 0.5)
    (trek52--klingons-attack)))

(defun trek52--clear-screen-for-report ()
  (setq trek52--current-report trek52--rpt-UNKNOWN)
  (loop for k from 1 to 3 do
        (trek52--cursor-move-to-screen-pos 60 k)
        (clrtoeol))
  (trek52--clear-report-area))

(defun trek52--clear-report-area ()
  (loop for k from 4 to (+ 3 trek52--max-SYSTEMS) do
        (trek52--cursor-move-to-screen-pos 60 k)
        (clrtoeol)))

(defun trek52--computers-damaged ()
  (when (= trek52--current-report trek52--rpt-BATTLE)
    (trek52--display-battle-report))
  (trek52--xcmd-e-erase-galaxy-map))

(defun trek52--update-klingon-energy (k e)
  (trek52--set-klingon-energy k (- (trek52--klingon-energy k) e))
  (when (and (= trek52--current-report trek52--rpt-BATTLE)
             (not (trek52--system-damaged-p trek52--sys-BATTLE-RPT)))
    (if (> (trek52--klingon-energy k) 0)
        (trek52--print-at-screen-pos 74 (+ 3 k) "%4d" (trek52--klingon-energy k))
      (trek52--cursor-move-to-screen-pos 60 (+ 3 k))
      (clrtoeol))))

(defun trek52--print-klingon-details (k)
  (when (and (= trek52--current-report trek52--rpt-BATTLE)
             (not (trek52--system-damaged-p trek52--sys-BATTLE-RPT))
             (> (trek52--klingon-energy k) 0))
    (trek52--print-at-screen-pos 60 (+ 3 k) "%d-%d" (trek52--klingon-x k) (trek52--klingon-y k))
    (if (trek52--system-damaged-p trek52--sys-COMPUTERS)
        (addstr "          ")
      (printw " %4.1f" (trek52--distance-to-klingon k))
      (if trek52--show-vector-p
          (printw " %1.2f" (trek52--vector-to-klingon k))
        (addstr "     ")))))

(defun trek52--toggle-klingon-pointer (k show-pointer)
  (when (and (= trek52--current-report trek52--rpt-BATTLE)
             (not (trek52--system-damaged-p trek52--sys-BATTLE-RPT)))
    (trek52--print-at-screen-pos 59 (+ 3 k)
                                 (if show-pointer
                                     ">"  " "))))

(defun trek52--vector-to-klingon (k)
  (let ((v1 trek52--sector-y)
        (v2 trek52--sector-x)
        (v3 (trek52--klingon-y k))
        (v4 (trek52--klingon-x k))
        absv2 absv4)
    (setq v4 (- v4 v2))
    (setq v2 (- v1 v3))
    (setq absv2 (float (abs v2)))
    (setq absv4 (float (abs v4)))
    (cond ((< v4 0) (if (> v2 0)
                        (trek52--vcalc2 3 absv2 absv4)
                      (if (= v4 0)
                          (trek52--vcalc2 7 absv2 absv4)
                        (trek52--vcalc1 5 absv2 absv4))))
          ((< v2 0) (trek52--vcalc2 7 absv2 absv4))
          ((> v4 0) (trek52--vcalc1 1 absv2 absv4))
          ((= v2 0) (trek52--vcalc1 5 absv2 absv4))
          (t        (trek52--vcalc1 1 absv2 absv4)))
    ))

(defun trek52--vcalc1 (v1 abs-v2 abs-v4)
  (if (<= abs-v2 abs-v4)
      (+ v1 (/ abs-v2 abs-v4))
    (+ v1 (/ (+ (- abs-v2 abs-v4) abs-v2) abs-v2))))

(defun trek52--vcalc2 (v1 abs-v2 abs-v4)
  (if (>= abs-v2 abs-v4)
      (+ v1 (/ abs-v4 abs-v2))
    (+ v1 (/ (+ (- abs-v4 abs-v2) abs-v4) abs-v4))))

(defun trek52--end-game ()
  (sit-for 2)
  (trek52--clear-screen)
  (trek52--cursor-move-to-screen-pos 10 8)
  (cond ((= trek52--win-flag trek52--flag-WIN)    (addstr "YOU WIN!"))
        ((= trek52--win-flag trek52--flag-ACTIVE) (addstr "YOU GIVE UP TOO EASILY"))
        ((= trek52--win-flag trek52--flag-LOSE)   (addstr "YOU LOSE!")))

  (sit-for 2)
  (trek52--clear-screen)
  (addstr "END TREK52\n\n\n")
  (fundamental-mode)
  (curs_set 1))


(provide 'trek52)

;;; trek52.el ends here
