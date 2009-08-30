;;; erobot.el --- game with elisp functions trying to survive

;; Copyright (C) 2000  Alexander Schroeder

;; Emacs Lisp Archive Entry
;; Filename: erobot.el
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 2.1.0
;; Keywords: games
;; Description: game with elisp functions trying to survive
;; URL: http://www.geocities.com/kensanata/emacs.html

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This game implements a very basic version of the many robot variants
;; out there.  Players compete by writing a defun (called candidates,
;; erobots, or ebots).  The last ebot remaining wins the game.  Start
;; the game with `erobot'.

;; Bugs: No multiple copies of the same defun allowed (use defalias or
;; fset to create copies)

;; Testing the game: Run `erobot-tour'.

;;; Code:

(defgroup erobot nil
  "The game erobot -- elisp robots"
  :group 'games)

(defcustom erobot-map '("**************"
			"*            *"
			"*            *"
			"*            *"
			"*            *"
			"**************")
  "Map used in a game of `erobot'.
The map is a list of strings, where each character in the string is
one of:
 `*' solid
 ` ' empty space
The 0 0 coordinate is the first character of the first string, ie. the
top left corner."
  :type '(repeat string)
  :group 'erobot)

(defcustom erobot-verbosity 1
  "Level of detail for messages in the *Messages* buffer.
0  no messages
1  kill messages
2  movement messages
3  reasoning messages (if the erobot supports it)
4  debug messages"
  :type '(choice (const :tag "no messages" 0)
		 (const :tag "kill messages" 1)
n		 (const :tag "movement messages" 2)
		 (const :tag "reasoning messages" 3)
		 (const :tag "debug messages" 4))
  :group 'erobot)

(defcustom erobot-combat-system 'simple
  "Combat system used by candidates.

simple     moving into an enemy kills it
neighbors  moving into an enemy kills it, if there are at least as
           many team members as there are enemy team members in the
           four neighboring positions
area       moving into an enemy kills it, if there are at least as
           many team members as there are enemy team members in the
           eight positions close by

Note that pRobot (the same game implemented in Python)
uses the area combat system."
  :type '(choice (const :tag "moving into an enemy kills it" 'simple)
		 (const :tag "need more team members in four neighboring positions" 'neighbors)
		 (const :tag "need more team members in eight close positions" 'area))
  :group 'erobot)

(defcustom erobot-map-name "* ELISP ROBOT ARENA *"
  "Name for the `erobot' buffer."
  :type 'string
  :group 'erobot)

(defcustom erobot-max-turns 1000
  "Maximum number of turns to run before calling it a draw."
  :type 'integer
  :group 'erobot)

(defcustom erobot-delays '((50 . 0.05)
			   (100 . 0.02))
  "Alist of delays to use depending on the turn number.
A list of elements of the form (LIMIT . DELAY), each meaning to `sit-for'
DELAY as long as the turn number is below LIMIT.  The elements are examined
from first to last, therefore the alist should be sorted by LIMIT.  The
default DELAY is 0, should no LIMIT exceed the turn number."
  :type '(repeat (cons (integer :tag "Limit") (number :tag "Delay")))
  :group 'erobot)

(defcustom erobot-team-faces '(erobot-face-1
			       erobot-face-2
			       erobot-face-3
			       erobot-face-4
			       erobot-face-5)
  "The team faces available, should any teams enter the game.
If the team symbol is a face, then the team members will be visualized
using that face.  The number of symbols in this list automatically
limits the number of teams recognized by the system."
  :type '(repeat face)
  :group 'erobot)

;; faces

(defface erobot-face-1
  '((t (:bold t)))
  "Face for the 1st erobot team."
  :group 'erobot)

(defface erobot-face-2
  '((t (:bold t :foreground "forest green")))
  "Face for the 2nd erobot team."
  :group 'erobot)

(defface erobot-face-3
  '((t (:bold t :foreground "firebrick")))
  "Face for the 3rd erobot team."
  :group 'erobot)

(defface erobot-face-4
  '((t (:bold t :foreground "blue")))
  "Face for the 4th erobot team."
  :group 'erobot)

(defface erobot-face-5
  '((t (:bold t :foreground "orange red")))
  "Face for the 5th erobot team."
  :group 'erobot)

;; Internal variables

(defvar erobot-commands '(erobot-move erobot-pass)
  "List of valid erobot command DEFUNs.
A valid erobot command is a list (DEFUN PARAM1 PARAM2 ...).
DEFUN is on of the erobot command defuns and the rest are
any parameters used when calling DEFUN.")

(defvar erobot-candidates nil
  "List of currently active erobot candidates.")

(defvar erobot-current-candidate nil
  "Currently active erobot candidate.")

(defvar erobot-visual t
  "Determines if the current game is visualized or not.
Bind this to nil if you want to run games without visualization.
It usually makes no sense to actually set this variable to nil.")

;; Main routine and visualization

(defun erobot (&rest candidates)
  "Starts a game of erobot.
CANDIDATES is an optional list of teams and candidates.  A team is a
list of candidates.  Each candidate is a function symbol that will be
called whenever the candidate's turn has arrived.

If CANDIDATES is nil the user will be asked for two candidates
interactively.

The rules for writing an erobot are these: the internal datastructures
are not to be modified (the code currently does not examine the
candidate code).  The erobot should return a command as described in the
documentation for `erobot-commands'.  The erobot may examine the entire
map as desired.

Usefull functions for writing an erobot:

 `erobot-candidate-at' (x y)
 `erobot-candidate-dir' (dir &optional dist)

 `erobot-neighbors-at' (&optional x y)
 `erobot-close-neighbors-at' (&optional x y)
 `erobot-neighbors-dir' (dir &optional dist)
 `erobot-no-neighbors-dir' (dir &optional dist)

 `erobot-free-space-at' (x y)
 `erobot-free-space-dir' (dir &optional dist)

 `erobot-direction' (x1 y1 x2 y2)
 `erobot-direction-to' (candidate)
 `erobot-distance-to' (candidate)
 `erobot-distance-between' (candidate x y)
 `erobot-distance-to-non-member' (dir)
 `erobot-closest-candidate' (&optinal conditions)

 `erobot-random-dir' (&optinal conditions)
 `erobot-opposite-dir' (dir)

 `erobot-get-x' (&optional candidate)
 `erobot-get-y' (&optional candidate)

 `erobot-same-team' (candidates)
 `erobot-other-team' (candidates)
 `erobot-get-team-members' (&optional team)
 `erobot-team-members' (candidates team)

 `erobot-info-share' (key value &optional distance)
 `erobot-info-get' (key)

Note that erobots have all the computing time they want before they
return a command.

The game ends if only candidates of one team remain on the map, if
`erobot-max-turns' has been exceeded, or if the char q is pressed
while the game is running.  When the game ends, the candidates on
the map are returned in a list."
  (interactive "i\n\naCandidate A: \naCandidate B: ")
  ;; Place candidates on the map and set erobot-candidates
  (setq erobot-candidates nil)
  (erobot-initialize candidates)
  ;; Process commands until victory conditions reached
  (let ((candidates-this-turn erobot-candidates)
	(turn 1)
	command)
    (while (and (> (length (erobot-teams)) 1)
		(< turn erobot-max-turns)
		(not (and (input-pending-p)
			  (= (read-char) ?q))))
      (when (null candidates-this-turn)
	(if erobot-visual (erobot-visualize turn))
	(setq turn (1+ turn))
	(setq candidates-this-turn erobot-candidates))
      (setq erobot-current-candidate (car candidates-this-turn)
	    candidates-this-turn (cdr candidates-this-turn)
	    command (funcall erobot-current-candidate))
      (if (not (memq (car command) erobot-commands))
	  (error "%S: %S is not a supported command" erobot-current-candidate (car command)))
      (apply (car command) (cdr command)))
    ;; Display end of game
    (when erobot-visual
      (erobot-visualize turn)
      (view-buffer (get-buffer erobot-map-name) 'kill-buffer))
    erobot-candidates))

(defun erobot-visualize (turn)
  "Visualize the erobot map."
  (unless (eq (window-buffer) (get-buffer-create erobot-map-name))
    (switch-to-buffer (get-buffer erobot-map-name)))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((map erobot-map))
    (while map
      (insert (car map) "\n")
      (setq map (cdr map))))
  (insert " " (number-to-string turn))
  (let ((candidates erobot-candidates)
	candidate)
    (while candidates
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (goto-char (point-min))
      (forward-line (erobot-get-y candidate))
      (forward-char (erobot-get-x candidate))
      (delete-char 1)
      (insert (get candidate 'char))
      (let ((team (erobot-get-team candidate)))
	(if (facep team)
	    (put-text-property (1- (point)) (point) 'face team)))))
  (goto-char (point-min))
  (let ((delays erobot-delays)
	delay)
    (while delays
      (setq delay (car delays)
	    delays (cdr delays))
      (when (< turn (car delay))
	(setq delays nil)
	(sit-for (cdr delay))))))

(defun erobot-teams ()
  "Return list of teams."
  (let ((candidates erobot-candidates)
	candidate
	teams)
    (while candidates
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (add-to-list 'teams (erobot-get-team candidate)))
    teams))

;; Setup

(defun erobot-initialize (candidates)
  "Initialize CANDIDATES.
CANDIDATES is a list of candidates or teams.  A team is itself a list of
candidates."
  (let ((team-faces erobot-team-faces)
	 candidate)
    (while candidates
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (if (listp candidate)
	  (let ((team candidate)
		(face (car team-faces)))
	    (setq team-faces (cdr team-faces))
	    (while team
	      (setq candidate (car team)
		    team (cdr team))
	      (erobot-distribute candidate face)))
	(erobot-distribute candidate)))))

(defun erobot-distribute (candidate &optional team)
  "Initialize and distribute CANDIDATE on `erobot-map'.
If the optional parameter TEAM is given, the candidate is considered
part of that team, else the candidate is considered to be on his own.

The property list of the candidate is used to record information
about it:
 x and y are the position on the map
 char is the character used to visualize the position on the map
 team is either a color string or the candidate itself
 brain is an alist with information shared amongst team members"
  (let* ((y (random (length erobot-map)))
	 (x (random (length (elt erobot-map y))))
	 (char (erobot-get-unused-char)))
    ;; repeat placement until legal
    (while (not (and (erobot-free-space-at x y)
		     (null (erobot-candidate-at x y))
		     (null (erobot-neighbors-at x y))))
      (setq y (random (length erobot-map))
	    x (random (length (elt erobot-map y)))))
    (put candidate 'y y)
    (put candidate 'x x)
    (put candidate 'char char)
    (put candidate 'team (or team candidate))
    (put candidate 'brain nil)
    (add-to-list 'erobot-candidates candidate)))

(defun erobot-get-unused-char ()
  "Return a character not yet used by any candidate in `erobot-candidates'."
  (let ((candidates erobot-candidates)
	(char ?A) candidate)
    (while candidates
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (if (>= (get candidate 'char) char)
	  (setq char (1+ (get candidate 'char)))))
    char))

;; Tournamets

(defun erobot-best-of-n (n &rest candidates)
  "Run a best-of-n tournament.
The candidate with the most victories after N games wins."
  (let ((winners (apply 'nconc (mapcar (function (lambda (x) (apply 'erobot candidates)))
				       (make-list n nil))))
	winner result candidates candidate count)
    (insert (format "TOURNAMENT: BEST-OF-%d\n" n))
    (while winners
      (setq winner (car winners)
	    candidates winners
	    count 0)
      (while candidates
	(setq candidate (car candidates)
	      candidates (cdr candidates))
	(if (eq winner candidate)
	    (setq count (1+ count))))
      (add-to-list 'result (cons winner count))
      (setq winners (delq winner winners)))
    result))

(defun erobot-best-of-5 (&rest candidates)
  "Run a best-of-5 tournament.
The candidate with the most victories after five games wins."
  (apply 'erobot-best-of-n 5 candidates))

(defun erobot-best-of-100 (&rest candidates)
  "Run a best-of-100 tournament.
The candidate with the most victories after 100 games wins."
  (apply 'erobot-best-of-n 100 candidates))

(defun erobot-run-tournament (tournament &rest candidates)
  "Run function TOURNAMENT for all CANDIDATES.
The result of the tournament function must be an alist
with elements \(CANDIDATE . SCORE)."
  (switch-to-buffer (get-buffer-create erobot-map-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (message (format "Running %s..." tournament))
  (let* ((erobot-verbosity 0)
	 (erobot-visual nil)
	 (result (apply tournament candidates)))
    (setq result (sort result (function (lambda (a b) (>= (cdr a) (cdr b))))))
    (insert "\nScore  Candidate\n\n")
    (let ((scores result))
      (while scores
	(insert (format "%5d  %S\n" (cdar scores) (caar scores)))
	(setq scores (cdr scores))))
    (when (memq t (mapcar 'listp candidates))
      (insert "\nScore  Team\n\n")
      (let ((teams candidates)
	    team)
	(while teams
	  (setq team (car teams)
		teams (cdr teams))
	  (if (listp team)
	      (insert (format "%5d  %S\n"
			      (apply '+ (mapcar (function (lambda (c)
							    (or (cdr (assq c result)) 0)))
						team))
			      team)))))))
  (message (format "Running %s...done" tournament))
  (view-buffer (current-buffer) 'kill-buffer))

;; Commands

(defun erobot-pass ()
  "Calling this function does nothing."
  (if (>= erobot-verbosity 2)
      (message "%S passes" erobot-current-candidate)))

(defun erobot-move (dir)
  "Causes `erobot-current-candidate' to move in the given direction.
Direction DIR must be one of `up', `down', `left' or `right'.
If team members are in the way, the current candidate will jump over them."
  (let ((dist (erobot-distance-to-non-member dir)))
    (when (and (> dist 0); eliminate suicide, happens if dir eq nil
	       (erobot-free-space-dir dir dist))
      (if (>= erobot-verbosity 2)
	  (message "%S moves %d steps %S" erobot-current-candidate dist dir))
      (if (erobot-kill-dir dir dist)
	  (erobot-set-position-dir dir dist)))))

;; do stuff on the map based on position X Y

(defun erobot-set-position-at (x y)
  "Set position of `erobot-current-candidate' to X and Y.
This function may not be called by candidates."
  (if (erobot-candidate-at x y)
      (error "Illegal move by %S" erobot-current-candidate))
  (put erobot-current-candidate 'y y)
  (put erobot-current-candidate 'x x))

(defun erobot-kill-at (x y)
  "Kills the candidate at position X Y.
This function may not be called by candidates.
The effect is controlled by `erobot-combat-system'.
The dead candidate is removed from `erobot-candidates'.
Returns t if nothing to kill, or the candidate killed, or nil."
  (let ((candidate (erobot-candidate-at x y))
	attackers defenders neighbors)
    (if (null candidate)
	t
      (if (or
	   ;; simple: attacker wins
	   (eq erobot-combat-system 'simple)
	   ;; neighbors: four neighboring positions are examined
	   ;; area: eight neighboring positions are examined
	   (progn
	     (setq neighbors (cond ((eq erobot-combat-system 'neighbors)
				    (erobot-neighbors-at x y))
				   ((eq erobot-combat-system 'area)
				    (erobot-close-neighbors-at x y)))
		   attackers (erobot-team-members neighbors (erobot-get-team))
		   defenders (erobot-team-members neighbors (erobot-get-team candidate)))
	     (> attackers defenders)))
	  (progn
	    (if (>= erobot-verbosity 1)
		(message "%S kills %S" erobot-current-candidate candidate))
	    (if (and attackers defenders (>= erobot-verbosity 4))
		(message "ratio %d:%d amongst %S" attackers defenders neighbors))
	    (setq erobot-candidates (delq candidate erobot-candidates))
	    candidate)))))

(defun erobot-team-members (candidates team)
  "Return the number of CANDIDATES in TEAM.
CANIDATES is a list of candidates.  TEAM is a team such
as returned by `erobot-get-team'."
  (let ((count 0)
	(cands candidates)
	cand)
    (while cands
      (setq cand (car cands)
	    cands (cdr cands))
      (if (eq team (erobot-get-team cand))
	  (setq count (1+ count))))
    count))

(defun erobot-candidate-at (x y)
  "Return candidate at position X Y."
  (let ((candidates erobot-candidates)
	result candidate)
    (while (and candidates (not result))
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (if (and (= x (erobot-get-x candidate))
	       (= y (erobot-get-y candidate)))
	  (setq result candidate)))
    result))

(defun erobot-free-space-at (x y)
  "Look at position X Y in `erobot-map'.
Return nil if the terrain is impassable.
Return t if the terrain is passable."
  (condition-case nil
      (let ((terrain (elt (elt erobot-map y) x)))
	(eq terrain ? ))
    (args-out-of-range)))

(defun erobot-neighbors-at (&optional x y)
  "Return list of candidates around position X Y.
All candidates in the list have distance d = 1 to position X Y.
The list never includes the candidate at position X Y,
and the list never includes `erobot-current-candidate'.
If X and Y are not given, the position of `erobot-current-candidate'
is used."
  (let (result)
    (setq x (or x (erobot-get-x))
	  y (or y (erobot-get-y)))
    (mapcar (function (lambda (cand)
			(if (and (= (erobot-distance-between cand x y) 1)
				 (not (eq cand erobot-current-candidate)))
			    (add-to-list 'result cand))))
	    erobot-candidates)
    result))

(defun erobot-close-neighbors-at (&optional x y)
  "Return list of candidates close to position X Y.
All candidates in the list touch position X Y, either
with an edge or with a corner.  This function is only
used for a certain value of `erobot-combat-system'.
The list never includes the candidate at position X Y,
and the list never includes `erobot-current-candidate'.
If X and Y are not given, the position of `erobot-current-candidate'
is used."
  (let (result target)
    (setq x (or x (erobot-get-x))
	  y (or y (erobot-get-y))
	  target (erobot-candidate-at x y))
    ;; don't use `erobot-distance-between' with distance 2 because we
    ;; don't want to include non-touching positions at distance 2.
    (mapcar (function (lambda (cand)
			(if (and (<= (abs (- x (erobot-get-x cand))) 1)
				 (<= (abs (- y (erobot-get-y cand))) 1)
				 (not (eq cand erobot-current-candidate))
				 (not (eq cand target)))
			    (add-to-list 'result cand))))
	    erobot-candidates)
    result))

;; convenience functions that translate arguments based on the position
;; of `erobot-current-candidate', a direction and a distance into X and
;; Y coordinates.  The actual translation is done in
;; `erobot-directional'.

(defun erobot-directional (func &optional dir dist)
  "Call function FUNC with parameters x and y based on DIR and DIST.
FUNC can be any function taking an x and a y position.
The position examined is determined by taking the position of
`erobot-current-candidate' and looking at the neighbouring cell in
`erobot-map'.

Direction DIR must be one of `up', `down', `left' or `right'.  If
DIR is anything else, then the position of `erobot-current-candidate' is
used.

Optional argument DIST specifies how may steps in direction DIR to take.
If DIST is not a number, DIST is determined by skipping all positions
with team members in them.  If DIST is not given, it defaults to 1, ie.
the immediate neighbour cell is examined."
  (let ((x (erobot-get-x))
	(y (erobot-get-y))
	(dist (or dist 1)))
  (cond ((eq dir 'up)
	 (funcall func x (- y dist)))
	((eq dir 'down)
	 (funcall func x (+ y dist)))
	((eq dir 'left)
	 (funcall func (- x dist) y))
	((eq dir 'right)
	 (funcall func (+ x dist) y))
	(t (funcall func x y)))))

(defun erobot-set-position-dir (dir &optional dist)
  "Move `erobot-current-candidate' in direction DIR.
This function may not be called by candidates.  Candidates must use the
`erobot-move' command in order to move around.
Same effect as `erobot-set-position-at'.  The position examined is
determined by taking the position of `erobot-current-candidate' and
looking at the neighbouring cell in `erobot-map'.
Direction DIR must be one of `up', `down', `left' or `right'.
The optional argument DIST specifies how may steps in direction DIR to
take.  DIST defaults to 1, ie. the immediate neighbour cell is
examined."
  (erobot-directional 'erobot-set-position-at dir dist))

(defun erobot-kill-dir (dir &optional dist)
  "Kill candidate in direction DIR.
This function may not be called by candidates.
Same effect as `erobot-kill-at'.  The position examined is determined by
taking the position of `erobot-current-candidate' and looking at the
neighbouring cell in `erobot-map'.
Direction DIR must be one of `up', `down', `left' or `right'.
If DIR is nil, then `erobot-current-candidate' will be killed.
The optional argument DIST specifies how may steps in direction DIR to
take.  DIST defaults to 1, ie. the immediate neighbour cell is
examined."
  (erobot-directional 'erobot-kill-at dir dist))

(defun erobot-candidate-dir (dir &optional dist)
  "Return candidate in direction DIR.
Return the same information as `erobot-candidate-at'.  The
position examined is determined by taking the position of
`erobot-current-candidate' and looking at the neighbouring cell in
`erobot-map'.
Direction DIR must be one of `up', `down', `left' or `right'.
If DIR is nil, then `erobot-current-candidate' will be returned.
The optional argument DIST specifies how may steps in direction DIR to
take.  DIST defaults to 1, ie. the immediate neighbour cell is
examined."
  (erobot-directional 'erobot-candidate-at dir dist))

(defun erobot-neighbors-dir (dir &optional dist)
  "Return list of neighbors in direction DIR.
Return the same information as `erobot-neighbors-at'.  The
position examined is determined by taking the position of
`erobot-current-candidate' and looking at the neighbouring cell in
`erobot-map'.
Direction DIR must be one of `up', `down', `left' or `right'.
If DIR is nil, then the position of `erobot-current-candidate' is
examined.
The optional argument DIST specifies how may steps in direction DIR to
take.  DIST defaults to 1, ie. the immediate neighbour cell is
examined."
  (erobot-directional 'erobot-neighbors-at dir dist))

(defun erobot-free-space-dir (dir &optional dist)
  "Look in direction DIR in `erobot-map'.
Return the same information as `erobot-free-space-at'.  The position examined
is determined by taking the position of `erobot-current-candidate' and
looking at the neighbouring cell in `erobot-map'.
Direction DIR must be one of `up', `down', `left' or `right'.
If DIR is nil, then the position of `erobot-current-candidate' is
examined.
The optional argument DIST specifies how may steps in direction DIR to take.
DIST defaults to 1, ie. the immediate neighbour cell is examined."
  (erobot-directional 'erobot-free-space-at dir dist))

(defun erobot-random-dir (&optional conditions)
  "Return a random direction symbol.
Optional argument CONDITIONS is a list of conditions.  Each condition is a
function that takes one parameter, the direction, and returns non-nil if the
condition is satisfied.  If no direction satisfies all the conditions, nil
is returned."
  ;; Choose random direction, call each function in CONDITIONS for dir
  ;; and check wether any returned nil.  If so, choose another from the
  ;; remaing directions.
  (let ((dirs (copy-sequence '(up down left right)))
	dir)
    (while (and dirs (not dir))
      (setq dir (elt dirs (random (length dirs)))
	    dirs (delq dir dirs))
      (if (memq nil (mapcar (function (lambda (f) (funcall f dir))) conditions))
	  (setq dir nil)))
    dir))

(defun erobot-valid-dir (dir)
  "Return t if DIR is one of up, down, left, or right."
  (memq dir '(up down left right)))

(defun erobot-opposite-dir (dir)
  "Return the opposite of direction DIR."
  (cdr (assq dir '((up . down)
		   (down . up)
		   (left . right)
		   (right . left)))))

;; other helper functions

(defun erobot-distance-to-non-member (dir)
  "Returns distance to position in direction DIR not occupied by team members.
Direction DIR must be one of `up', `down', `left' or `right'."
  (if (erobot-valid-dir dir)
      (let ((dist 1))
	(while (erobot-same-team (erobot-candidate-dir dir dist))
	  (setq dist (1+ dist)))
	dist)
    0))

;; information concerning the position of other candidates

(defun erobot-direction (x1 y1 x2 y2)
  "Return direction from x1 y1 to x2 y2.
Direction DIR is one of `up', `down', `left' or `right'."
  (cond ((and (> (- y1 y2) (abs (- x1 x2))))
	 'up)
	((and (> (- y2 y1) (abs (- x1 x2))))
	 'down)
	((and (> (- x1 x2) (abs (- y1 y2))))
	 'left)
	((and (> (- x2 x1) (abs (- y1 y2))))
	 'right)
	((and (= (- y1 y2) (- x1 x2)))
	 (if (> y1 y2)
	     (if (= 1 (random 2))
		 'up
	       'left)
	   (if (= 1 (random 2))
	       'down
	     'right)))
	((and (= (- y1 y2) (- x2 x1)))
	 (if (> y1 y2)
	     (if (= 1 (random 2))
		 'up
	       'right)
	   (if (= 1 (random 2))
	       'down
	     'left)))))

(defun erobot-direction-to (candidate)
  "Return direction of CANDIDATE relative to `erobot-current-candidate'.
This can be used to chase CANDIDATE."
  (erobot-direction (erobot-get-x)
		    (erobot-get-y)
		    (erobot-get-x candidate)
		    (erobot-get-y candidate)))

(defun erobot-distance-between (candidate x y)
  "Return distance between CANDIDATE and position X Y."
  (+ (abs (- x (erobot-get-x candidate)))
     (abs (- y (erobot-get-y candidate)))))

(defun erobot-distance-to (candidate)
  "Return distance of CANDIDATE relative to `erobot-current-candidate'."
  (erobot-distance-between candidate (erobot-get-x) (erobot-get-y)))

(defun erobot-closest-candidate (&optional conditions)
  "Return the closest candidate to `erobot-current-candidate'.
If optional argument CONDITIONS is given, only candidates satisfying
the conditions are returned.  Each condition is a function that takes
one parameter, the candidate, and returns non-nil if the condition is
satisfied.  If no candidate satisfies all the conditions, nil is
returned."
  (let ((candidates erobot-candidates)
	candidate distance result)
    (while candidates
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (if (and (not (eq candidate erobot-current-candidate))
	       (not (memq nil (mapcar (function (lambda (f) (funcall f candidate))) conditions)))
	       (or (null distance)
		   (< (erobot-distance-to candidate) distance)))
	  (setq distance (erobot-distance-to candidate)
		result candidate)))
    result))

;; Functions that are often used as conditions

(defun erobot-no-neighbors-dir (dir &optional dist)
  "Return non-nil if there are no neighbors in direction DIR distance DIST."
  (null (erobot-neighbors-dir dir dist)))

;; Accessing erobot attributes (instead of using `get')

(defun erobot-get-x (&optional candidate)
  "Get the x position of CANDIDATE.
Use `erobot-current-candidate' if CANDIDATE is nil."
  (get (or candidate erobot-current-candidate) 'x))

(defun erobot-get-y (&optional candidate)
  "Get the y position of CANDIDATE.
Use `erobot-current-candidate' if CANDIDATE is nil."
  (get (or candidate erobot-current-candidate) 'y))

(defun erobot-get-team (&optional candidate)
  "Get the team CANDIDATE is member of.
Use `erobot-current-candidate' if CANDIDATE is nil."
  (get (or candidate erobot-current-candidate) 'team))

(defun erobot-same-team (candidates)
  "Return non-nil if all CANDIDATES are on the same team as `erobot-current-candidate'.
CANDIDATES may also be one CANDIDATE instead of a list of candidates."
  (and candidates
       (if (listp candidates)
	   (not (memq nil (mapcar 'erobot-same-team candidates)))
	 (equal (erobot-get-team) (erobot-get-team candidates)))))

(defun erobot-other-team (candidates)
  "Return non-nil if any CANDIDATES are not on the same team as `erobot-current-candidate'.
CANDIDATES may also be one CANDIDATE instead of a list of candidates."
  (and candidates
       (not (erobot-same-team candidates))))

(defun erobot-get-team-members (&optional team)
  "Return list of TEAM members.
If no TEAM is given, the team members of `erobot-current-candidate' are returned."
  (setq team (or team (erobot-get-team)))
  (delq nil (mapcar (function (lambda (c)
				(if (eq team (erobot-get-team c))
				    c))) erobot-candidates)))

;; Sharing information among team members

(defun erobot-info-share (key value &optional distance)
  "Share information with team members of `erobot-current-candidate'.
The piece of information will be stored in a plist with
key KEY and value VALUE.  If optional argument DISTANCE
is given, the information will only be shared with team
members in that distance or less.  Pieces of information
can be retrieved using `erbot-info-get'."
  (mapcar (function (lambda (c)
		     (if (or (not distance)
			     (<= (erobot-distance-to c) distance))
			 (put c 'brain (plist-put (get c 'brain) key value)))))
	 (erobot-get-team-members)))

(defun erobot-info-get (key)
  "Get information from `erobot-current-candidate'.
The piece of information is stored in a plist with
key KEY.  Pieces of information can be stored using
`erbot-info-share'."
  (plist-get (get erobot-current-candidate 'brain) key))

;; Sample implementations

(defun erobot-candidate-noop ()
  "A possible candidate for a game of `erobot'.
This candidate does nothing."
  '(erobot-pass))

(defun erobot-candidate-random-walker ()
  "A possible candidate for a game of `erobot'.
This candidate moves around in random directions without walking into walls."
  (list 'erobot-move (erobot-random-dir '(erobot-free-space-dir))))

(defun erobot-candidate-cautious-walker ()
  "A possible candidate for a game of `erobot'.
This candidate walks only into cells with no neighbouring candidates.
This candidate will eat neighbors, however, thereby defeating simple
chasers."
  (let ((dir (erobot-random-dir '(erobot-free-space-dir erobot-no-neighbors-dir))))
    (if dir
	(list 'erobot-move dir)
      '(erobot-pass))))

(defun erobot-candidate-run-away ()
  "A possible candidate for a game of `erobot'.
This candidate runs away from the closest candidate."
  (let (dir)
    (setq dir (erobot-opposite-dir (erobot-direction-to (erobot-closest-candidate))))
    ;; if that direction is not passable, select a random direction that
    ;; is passable and has not neighbouring candidates
    (if (erobot-free-space-dir dir)
	(list 'erobot-move dir)
      (list 'erobot-move
	    (erobot-random-dir '(erobot-free-space-dir erobot-no-neighbors-dir))))))

(defun erobot-candidate-chaser ()
  "A possible candidate for a game of `erobot'.
This candidate chases other erobots.
This candidate is no good in a team as it will chase team members as well."
  (list 'erobot-move (erobot-direction-to (erobot-closest-candidate))))

(defun erobot-candidate-pack-chaser ()
  "A possible candidate for a game of `erobot'.
This candidate chases other erobots."
  (list 'erobot-move
	(erobot-direction-to (erobot-closest-candidate '(erobot-other-team)))))

(defun erobot-candidate-wicked-walker ()
  "A possible candidate for a game of `erobot'.
This candidate wanders around but bites any neighbors."
  (let* ((victim (erobot-closest-candidate))
	 (distance (erobot-distance-to victim)))
    (cond ((= distance 1)
	   (list 'erobot-move (erobot-direction-to victim)))
	  ((= distance 2)
	   (list 'erobot-pass))
	  (t
	   (list 'erobot-move (erobot-random-dir))))))

(defun erobot-candidate-social ()
  "A possible candidate for a game of `erobot'.
This candidate prefers the company of team members, avoids
other candidates but bites should anybody come too close."
  ;; get closest team member and walk towards it if possible
  (let* ((closest (erobot-closest-candidate))
	 (dir (erobot-direction-to closest))
	 (distance (erobot-distance-to closest)))
    (cond
     ;; move closer to the herd if deemed safe
     ((and (erobot-same-team closest)
	   (> distance 1)
	   (erobot-free-space-dir dir)
	   (not (erobot-candidate-dir dir))
	   (erobot-same-team (erobot-neighbors-dir dir)))
      (list 'erobot-move dir))
     ;; do nothing if we have joined the herd and no danger nearby
     ;; (ie. there is no neighbour from another team)
     ((let ((neighbors (erobot-neighbors-at)))
	(and neighbors (erobot-same-team neighbors)))
	  '(erobot-pass))
     ;; bite if others are close
     ((= distance 1)
      (list 'erobot-move dir))
     ;; make sure we don't approach evil doers
     ((= distance 2)
      (list 'erobot-pass))
     ;; last option: random walk
     (t
      (list 'erobot-move (erobot-random-dir))))))

;; Tour

(defun erobot-tour ()
  "Introduction to some of the erobots included in the package.
This function runs some erobot games and tournaments for you."
  (interactive)
  (let ((erobot-max-turns 1000)
	(erobot-delays '((30 . 0.3)
			 (500 . 0.01))))
    (let ((erobot-map-name "A wandering ebot eats the other"))
      (erobot 'erobot-candidate-random-walker 'erobot-candidate-noop)
      (erobot 'erobot-candidate-random-walker 'erobot-candidate-noop)
      (kill-buffer (get-buffer erobot-map-name)))
    (let ((erobot-map-name "An agressive erobot chases a cautious walker"))
      (erobot 'erobot-candidate-chaser 'erobot-candidate-cautious-walker)
      (erobot 'erobot-candidate-chaser 'erobot-candidate-cautious-walker)
      (kill-buffer (get-buffer erobot-map-name)))
    (let ((erobot-map-name "A agressive erobot chases a runner"))
      (erobot 'erobot-candidate-chaser 'erobot-candidate-run-away)
      (erobot 'erobot-candidate-chaser 'erobot-candidate-run-away)
      (kill-buffer (get-buffer erobot-map-name)))
    (let ((erobot-map-name "An agressive erobot walks into the trap of the wicked walker"))
      (erobot 'erobot-candidate-chaser 'erobot-candidate-wicked-walker)
      (erobot 'erobot-candidate-chaser 'erobot-candidate-wicked-walker)
      (kill-buffer (get-buffer erobot-map-name)))
    (let ((erobot-map-name "Several agressive erobots beat the trap because they stay together"))
      (fset 'bot1 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot2 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot3 (symbol-function 'erobot-candidate-chaser))
      (erobot 'erobot-candidate-wicked-walker '(bot1 bot2 bot3))
      (erobot 'erobot-candidate-wicked-walker '(bot1 bot2 bot3))
      (kill-buffer (get-buffer erobot-map-name)))
    (let ((erobot-map-name "Several smart and agressive erobots don't fare as well"))
      (fset 'bot1 (symbol-function 'erobot-candidate-pack-chaser))
      (fset 'bot2 (symbol-function 'erobot-candidate-pack-chaser))
      (fset 'bot3 (symbol-function 'erobot-candidate-pack-chaser))
      (erobot 'erobot-candidate-wicked-walker '(bot1 bot2 bot3))
      (erobot 'erobot-candidate-wicked-walker '(bot1 bot2 bot3))
      (kill-buffer (get-buffer erobot-map-name))
      (setq erobot-map-name "The same thing as a tournament...")
      (erobot-run-tournament 'erobot-best-of-5
			     '(erobot-candidate-wicked-walker)
			     '(bot1 bot2 bot3))
      (sit-for 5)
      (kill-buffer (get-buffer erobot-map-name)))
    (let ((erobot-map-name "Several agressive erobots try to fight sociobots"))
      (fset 'bot1 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot2 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot3 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot4 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot5 (symbol-function 'erobot-candidate-chaser))
      (fset 'bot6 (symbol-function 'erobot-candidate-social))
      (fset 'bot7 (symbol-function 'erobot-candidate-social))
      (fset 'bot8 (symbol-function 'erobot-candidate-social))
      (fset 'bot9 (symbol-function 'erobot-candidate-social))
      (erobot '(bot1 bot2 bot3 bot4 bot5)
	      '(bot6 bot7 bot8 bot9))
      (erobot '(bot1 bot2 bot3 bot4 bot5)
	      '(bot6 bot7 bot8 bot9))
      (kill-buffer (get-buffer erobot-map-name))
      (setq erobot-map-name "The same thing as a tournament...")
      (erobot-run-tournament 'erobot-best-of-5
			     '(bot1 bot2 bot3 bot4 bot5)
			     '(bot6 bot7 bot8 bot9))
      (sit-for 5)
      (kill-buffer (get-buffer erobot-map-name)))))

(provide 'erobot)

;;; erobot.el ends here
