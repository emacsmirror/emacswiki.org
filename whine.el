;;; whine.el --- complaint generator for GNU Emacs

;; Author: Bard Bloom <bard@theory.lcs.mit.edu>
;; Maintainer: Ulrich Mueller <ulm@gentoo.org>
;; Version: 20091120
;; Keywords: games

;; This file isn't part of much of anything.

;; This is distributed in the hope that it will be amusing, but it has
;; NO WARRANTY WHATEVER. It probably has lots of bugs in it. It may
;; well do damage to all kinds of things, although I believe that it
;; will not. It will definitely will degrade system performance.
;; The author takes no responsibility for anything.

;;; Commentary:

;; This modifies a lot of the commands bound to the main Emacs keys,
;; like SPC and C-f and RET and C-x C-s. They still do the same things
;; they did before, but they whine about it, printing messages which do
;; not in general have anything to do with reality. The main lossage is
;; C-l, which refreshes the screen and then whines about it so that you
;; can't get a clear message line.
;;
;; M-x whinify turns it on
;; M-x unwhinify turns it off

;;; Code:

(defvar whine-commands-alist nil)

(defun whine (key complaints)
  "Add KEY, its function, and COMPLAINTS to whine-commands-alist."
  (let ((fn (global-key-binding key)))
    (if (symbolp fn)
	(add-to-list 'whine-commands-alist
		     (list fn key complaints)))))

(defun whine-say (s)
  (cond
   ((stringp s) s)
   ((symbolp s) (symbol-name s))
   ((functionp s) (funcall s))
   ((consp s)
    (cond
     ((eq (car s) 'CAT)
      (mapconcat (function whine-say) (cdr s) " "))
     ((eq (car s) 'CATNS)
      (mapconcat (function whine-say) (cdr s) ""))
     ((eq (car s) 'lambda)
      (funcall s))
     (t
      (whine-say (whine-random-element-of s)))))
   (t (prin1-to-string s))))

(defun do-whine (s)
  "Whines a string determined by S.
string --> whines S
list --> if the car is `CAT', it whines each other element of S and
               CATenates them;
         CATNS catenates without spaces
         else it picks an element and whines it
symbol --> whines its print representation
function --> funcalls it without arguments and whines the result."
  (message (whine-say s)))

(defun whine-random-element-of (l)
  (nth (random (length l)) l))

(defun whine-get-word ()
  (save-excursion
    (backward-word)
    (let ((p (point)))
      (forward-word)
      (buffer-substring p (point)))))

(defun whine-initialize ()
  (let* ((noun
	  '("robot" "dweeb" "person" "child" "creep" "sleaze creature"
	    "colorizer" "dentist" "snake" "communist" "capitalist"
	    "fink" "panderer" "undergraduate" "dog" "thief" "Winkie"
	    "millionaire" "porcupine" "nerd" "toad" "computer scientist"
	    "mathematician" "logician" "waiter" "stenographer"
	    "hacker" "MP" "Munchkin" "economist" "dog catcher"
	    "hooker" "car dealer" "occultist" "Republican"
	    "Democrat" "sheik" "architect" "masochist" "playboy"
	    "devil worshipper" "owlbear" "pallbearer" "fungus"
	    "superhero" "poisoner" "congressman" "adulterer" "frog"
	    "slug" "eel" "hyena" "pteranodon" "fish" "virus" "saint"
	    "archangel" "astrophysicist" "biologist" "politician"
	    "demagogue" "programmer" "disk jockey" "infant" "lawyer"
	    "computer" "limousine driver" "hamster" "rodent"))
	 (adj
	  `("strange" "smelly" "Baltic" "masochistic" "creepy"
	    "illusory" "toxic" "idiotic" "charismatic" "godly"
	    "pneumatic" "well-painted" "overweight" "underpaid"
	    "overbearing" "underfed" "robotic" "psychic" "deceptive"
	    "lurking" "radiant" "turgid" "ironclad" "vicious"
	    "foolish" "nerdly" "isotonic" "bald" "iridescent"
	    "polygonal" "freaky" "drugged-out" "catatonic" "fainting"
	    "typical" "random" "horrible" "noxious" "slimy" "puerile"
	    "human" "toadlike" "wormlike" "ghastly" "goody-goody"
	    "wombat-like" "vapid" "stressed-out" "blessed" "saintly"
	    "heinous" "inedible" "droopy" "half-dead" "blissed-out"
	    "grossed-out" "knocked-out" "out-and-out"
	    (CATNS ,noun "-like")
	    (CATNS ,noun "-loving")
	    (CATNS ,noun "-hating")
	    (CATNS ,noun "-worshipping")
	    (CATNS ,noun "-fearing")
	    (CATNS ,noun "-hunting")
	    "political" "non-political" "communist" "indecisive"
	    "apathetic" "pathetic" "dyslexic" "dreaming" "anorexic"
	    "rich" "painted" "orthodox" "lunatic" "fishlike" "impure"
	    "virginal" "decrepit" "moronic" "despondent" "hygienic"
	    "unhygienic" "intolerable" "grease-coated" "sluggish"
	    "wimpy" "ponderous" "unlucky" "brilliant" "unclothed"
	    "heretical" "undergraduate" "melting" "green" "nosy"))
	 (whine-insults
	  `(CAT "You" ,adj ,adj (CATNS ,noun "!")))
	 (whine-pain
	  `(("Ouch!"
	     "Don't do that!"
	     "That hurts"
	     "Get off my foot, you goon!"
	     "Argh!"
	     "Eek!"
	     "Woe is me!"
	     "Alas and alack!"
	     "Have pity on me!"
	     "Life sucks."
	     "This jobs is *REALLY* *BORING*."
	     "I knew there'd be days like this."
	     "I hate being a computer!"
	     (CAT ("I wish I were a"
		   "I should have been a"
		   "Why, oh why, didn't I become a"
		   "I wish I weren't a"
		   "Alas!  It is my ill fortune to be a"
		   "Have pity on me, for I am a")
		  ,adj
		  ,noun)
	     "Stop it!"
	     "Oh! the pain, the pain!"
	     (CAT
	      ("Don't do that, you"
	       "That hurt, you"
	       "C'mon, you"
	       "Stop acting like a"
	       "Get off my foot, you"
	       "You"
	       "What a"
	       "Don't make me call a"
	       "Are you a human or a"
	       (CAT
		"You"
		("look" "act" "smell" "type" "program" "sound" "hesitate")
		"like a")
	       (CAT
		"You must"
		("love" "be" "know" "find" "worship" "have been")
		"a")
	       "Don't press that key again, you")
	      ,adj
	      ,adj
	      ,noun))))
	 (insult-word
	  `(CATNS
	    ("Don't say `"
	     "Don't you know how to spell `"
	     "You don't mean `"
	     (CAT
	      ("Only a")
	      ,adj
	      ,noun
	      ("would use a word like" "would say")
	      "`")
	     "What do you mean by `"
	     "How can you say `")
	    ,(symbol-function 'whine-get-word)
	    "'")))
    (whine "\C-p"  (append whine-pain
			   '("Up, up, and away!"
			     "This sure is an exciting line!"
			     "Gee!  Let's do that again!"
			     "Beginning of buffer")))
    (whine "\C-n"  (append whine-pain
			   '("Don't make me go that way"
			     "I don't want to go there!"
			     "What an unpleasant line!"
			     "Beginning of buffer"
			     "End of buffer"
			     "File not found")))
    (whine "\C-d"  (append whine-pain
			   '("Now what was that character again?"
			     "NOW look what you've done!"
			     "Buffer is read-only")))
    (whine "\C-_"  '("Oh, no!  I've forgotten what I did!"))
    (whine "\C-xu" '("Oh, no!  I've forgotten what I did!"))
    (whine "\C-k"  '("Please don't make me KILL again!"))
    (whine "\C-f"  (append whine-pain '("Forward ho!" "Oh, no!")))
    (whine "\C-b"  (append
		    whine-pain
		    '("Please, Mr. Human Person, don't make me go that way!"
		      "I am taken aback!"
		      "Backwards ha!"
		      "Catastrophic error -- world aborted."
		      "Career aborted -- unable to restart.")))
    (whine "\C-w"  (append whine-pain '("kill! Kill! KILL!")))
    (whine "\C-xb" (append whine-pain
			   '("This buffer is horrible!"
			     "I don't want to be here!")))
    (whine "\C-l"  '("You redrew the screen, you goon!"
		     "No, you can't have a clear message line."
		     "This is getting boring."
		     "Give up and go play Rogue."
		     "You just typed control-L to refresh the screen."))
    (whine "\M-%"  '("Oh wow!  I couldn't have done it better myself!"))
    (whine "\C-i"  '("Let's get the hell out of here!"))
    (whine "\M->"  (append whine-pain
			   '("It's off to the ends of the earth!")))
    (whine "\M-<"  (append whine-pain
			   '("It's off to the beginnings of the earth!")))
    (whine "\M-$"  insult-word)
    (whine " "     insult-word)
    (whine "\C-a"  whine-insults)
    (whine "\C-e"  whine-insults)
    (whine "\M-b"  whine-insults)
    (whine "\M-f"  whine-insults)
    (whine "\C-x2" (append whine-pain '("That just cuts me up.")))
    (whine "\M-x"  '("You shouldn't have typed that command!"
		     "That's not a legal command."))
    (whine "\C-x\C-s" '("Filesystem not writable."
			"File not saved."
			"File system full."
			"File system stupid."
			"Good heavens.  You don't seriously intend me to write on *that*, do you!"
			"File saved.  Hallelujah!"
			"File accidentally deleted -- please retype."))
    (whine "\C-m"  (append whine-pain
			   '("If you touch that key again I shall explode!"
			     "If you touch that key again I shall freak out!"
			     "That's the space bar, not the return key!"
			     "Press `c-c c-#' to abort."
			     "I won't RETURN!"
			     "You shouldn't have done that..."
			     "Now look what you've done!")))
    (whine "\C-v"  whine-pain)
    (whine "\M-v"  whine-pain)))

(defvar whine-last-complaints nil)

(defun whine-post-command-hook ()
  (let* ((command (assq this-command whine-commands-alist))
	 (complaints
	  (and (or (not (eq (car command) 'self-insert-command))
		   (equal (nth 1 command) (this-command-keys)))
	       (nth 2 command))))
    (cond
     ((> (minibuffer-depth) 0)
      ;; Save message for later display if we are in the minibuffer.
      ;; NB: M-x calls post-command-hook _before_ the function name is read.
      (or whine-last-complaints
	  (setq whine-last-complaints complaints)))
     (complaints
      (do-whine complaints)
      (setq whine-last-complaints nil))
     (whine-last-complaints
      (do-whine whine-last-complaints)
      (setq whine-last-complaints nil)))))

(defun whinify ()
  "Make certain global-mode keys whine.
The command `unwhinify' makes them normal again."
  (interactive)
  (or whine-commands-alist
      (whine-initialize))
  (add-hook 'post-command-hook 'whine-post-command-hook))

(defun unwhinify ()
  "Hopefully, remove the whine-bindings on the whiny keys."
  (interactive)
  (remove-hook 'post-command-hook 'whine-post-command-hook))

(provide 'whine)

;;; whine.el ends here
