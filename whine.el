;;; whine.el --- complaint generator for GNU Emacs  -*- lexical-binding: t; -*-

;; Author: Bard Bloom <bard@theory.lcs.mit.edu>
;;         Ulrich Müller <ulm@gentoo.org>
;; Maintainer: Ulrich Müller <ulm@gentoo.org>
;; Version: 20231020
;; Keywords: games

;; This file is not part of GNU Emacs.

;; This was inspired by a program posted by bard@theory.lcs.mit.edu
;; to comp.emacs in 1988. Apart from reusing the original lists of
;; words and phrases, the code below has been completely rewritten.

;; This work is marked with CC0 1.0 Universal. To view a copy of this
;; license, visit https://creativecommons.org/publicdomain/zero/1.0/

;;; Commentary:

;; From the initial announcement:
;; "It rebinds a lot of the main emacs keys, like space and c-f and
;; return and c-x c-s.  They still do the same things they did before,
;; but they whine about it, printing messages which do not in general
;; have anything to do with reality.  The main lossage is c-l, which
;; refreshes the screen and then whines about it so that you can't get
;; a clear message line."

;; Instead of rebinding the keys, the rewrite adds a function to
;; post-command-hook. This intercepts the main editing commands and
;; displays messages in the echo area.

;; Enable it with "M-x whinify", disable with "M-x unwhinify".

;;; Code:

(defvar whine-commands-alist
  (let* ((noun
	  '("adulterer" "archangel" "architect" "astrophysicist"
	    "biologist" "capitalist" "car dealer" "child" "colorizer"
	    "communist" "computer scientist" "computer" "congressman"
	    "creep" "demagogue" "Democrat" "dentist" "devil worshipper"
	    "disk jockey" "dog" "dog catcher" "dweeb" "economist" "eel"
	    "fink" "fish" "frog" "fungus" "hacker" "hamster" "hooker"
	    "hyena" "infant" "lawyer" "limousine driver" "logician"
	    "masochist" "mathematician" "millionaire" "MP" "Munchkin"
	    "nerd" "occultist" "owlbear" "pallbearer" "panderer"
	    "person" "playboy" "poisoner" "politician" "porcupine"
	    "programmer" "pteranodon" "Republican" "robot" "rodent"
	    "saint" "sheik" "sleaze creature" "slug" "snake"
	    "stenographer" "superhero" "thief" "toad" "undergraduate"
	    "virus" "waiter" "Winkie"))
	 (adj
	  `("anorexic" "apathetic" "bald" "Baltic" "blessed"
	    "blissed-out" "brilliant" "catatonic" "charismatic"
	    "communist" "creepy" "deceptive" "decrepit" "despondent"
	    "dreaming" "droopy" "drugged-out" "dyslexic" "fainting"
	    "fishlike" "foolish" "freaky" "ghastly" "godly"
	    "goody-goody" "grease-coated" "green" "grossed-out"
	    "half-dead" "heinous" "heretical" "horrible" "human"
	    "hygienic" "idiotic" "illusory" "impure" "indecisive"
	    "inedible" "intolerable" "iridescent" "ironclad" "isotonic"
	    "knocked-out" "lunatic" "lurking" "masochistic" "melting"
	    "moronic" "nerdly" "non-political" "nosy" "noxious"
	    "orthodox" "out-and-out" "overbearing" "overweight"
	    "painted" "pathetic" "pneumatic" "political" "polygonal"
	    "ponderous" "psychic" "puerile" "radiant" "random" "rich"
	    "robotic" "saintly" "slimy" "sluggish" "smelly" "strange"
	    "stressed-out" "toadlike" "toxic" "turgid" "typical"
	    "unclothed" "underfed" "undergraduate" "underpaid"
	    "unhygienic" "unlucky" "vapid" "vicious" "virginal"
	    "well-painted" "wimpy" "wombat-like" "wormlike"
	    (:catns ,noun "-fearing")
	    (:catns ,noun "-hating")
	    (:catns ,noun "-hunting")
	    (:catns ,noun "-like")
	    (:catns ,noun "-loving")
	    (:catns ,noun "-worshipping")))
	 (pain
	  `("Alas and alack!"
	    "Argh!"
	    "Don't do that!"
	    "Eek!"
	    "Get off my foot, you goon!"
	    "Have pity on me!"
	    "I hate being a computer!"
	    "I knew there'd be days like this."
	    "Life sucks."
	    "Oh! the pain, the pain!"
	    "Ouch!"
	    "Stop it!"
	    "That hurts."
	    "This job is *REALLY* *BORING*."
	    "Woe is me!"
	    (:cat ("Alas!  It is my ill fortune to be a"
		   "Have pity on me, for I am a"
		   "I should have been a"
		   "I wish I were a"
		   "I wish I weren't a"
		   "Why, oh why, didn't I become a")
		  ,adj
		  (:catns ,noun "."))
	    (:cat
	     ("Are you a human or a"
	      "C'mon, you"
	      "Don't do that, you"
	      "Don't make me call a"
	      "Don't press that key again, you"
	      "Get off my foot, you"
	      "Stop acting like a"
	      "That hurt, you"
	      "What a"
	      "You"
	      (:cat "You"
		    ("act" "hesitate" "look" "program" "smell" "sound" "type")
		    "like a")
	      (:cat "You must"
		    ("be" "find" "have been" "know" "love" "worship")
		    "a"))
	     ,adj
	     ,adj
	     (:catns ,noun "."))))
	 (insult
	  `(:cat "You" ,adj ,adj (:catns ,noun "!")))
	 (insult-word
	  `((:cat ("Don't say"
		   "You don't mean"
		   (:cat "Only a"
			 ,adj
			 ,noun
			 ("would say" "would use a word like")))
		  (:catns "`" whine-current-word "'."))
	    (:cat ("Don't you know how to spell"
		   "How can you say"
		   "What do you mean by")
		  (:catns "`" whine-current-word "'?"))))
	 (keys-alist
	  `((" "     ,insult-word)
	    ("\C-i"  ("Let's get the hell out of here!"))
	    ("\C-m"  (,pain
		      "If you touch that key again I shall explode!"
		      "If you touch that key again I shall freak out!"
		      "That's the space bar, not the return key!"
		      "Press `C-c C-#' to abort."
		      "I won't RETURN!"
		      "You shouldn't have done that..."
		      "Now look what you've done!"))
	    ;; Moving point.
	    ("\C-f"  (,pain "Forward ho!" "Oh, no!"))
	    ("\C-b"  (,pain
		      "Please, Mr. Human Person, don't make me go that way!"
		      "I am taken aback!"
		      "Backwards ha!"
		      "Catastrophic error -- world aborted."
		      "Career aborted -- unable to restart."))
	    ;; The right and left arrow keys are different
	    ;; from C-f and C-b in Emacs 24 and later.
	    ([right] (,pain "Ahoy, to starboard!"))
	    ([left]  (,pain "Ahoy, to port!"))
	    ("\C-n"  (,pain
		      "Don't make me go that way!"
		      "I don't want to go there!"
		      "What an unpleasant line!"
		      "Beginning of buffer"
		      "End of buffer"
		      "File not found"))
	    ("\C-p"  (,pain
		      "Up, up, and away!"
		      "This sure is an exciting line!"
		      "Gee!  Let's do that again!"
		      "Beginning of buffer"))
	    ("\C-a"  ,insult)
	    ("\C-e"  ,insult)
	    ("\M-f"  ,insult)
	    ("\M-b"  ,insult)
	    ([M-right] ,insult)
	    ([M-left]  ,insult)
	    ("\M->"  (,pain "It's off to the ends of the earth!"))
	    ("\M-<"  (,pain "It's off to the beginnings of the earth!"))
	    ("\C-v"  ,pain)
	    ("\M-v"  ,pain)
	    ("\M-gg" (,pain
		      "I don't want to go there!"
		      "What an unpleasant line!"))
	    ;; Erasing text.
	    ("\C-?"  (,pain "kill! Kill! KILL!"))
	    ("\C-d"  (,pain
		      "Now what was that character again?"
		      "NOW look what you've done!"
		      "Buffer is read-only"))
	    ("\C-k"  (,pain "kill! Kill! KILL!"))
	    ("\C-w"  ("Please don't make me KILL again!"))
	    ;; Miscellaneous commands.
	    ("\C-_"  ("Oh, no!  I've forgotten what I did!"))
	    ("\C-l"  ("You redrew the screen, you goon!"
		      "No, you can't have a clear message line."
		      "This is getting boring."
		      "Give up and go play Rogue."
		      "You just typed control-L to refresh the screen."))
	    ("\M-$"  ,insult-word)
	    ("\M-%"  ("Oh wow!  I couldn't have done it better myself!"))
	    ("\M-x"  ("You shouldn't have typed that command!"
		      "That's not a legal command."))
	    ("\C-x\C-s" ("Filesystem not writable."
			 "File not saved."
			 "File system full."
			 "File system stupid."
			 "Good heavens.  You don't seriously intend me to write on *that*, do you!"
			 "File saved.  Hallelujah!"
			 "File accidentally deleted -- please retype."))
	    ("\C-x2" (,pain "That just cuts me up."))
	    ("\C-xb" (,pain
		      "This buffer is horrible!"
		      "I don't want to be here!")))))
    (assq-delete-all
     nil
     (mapcar (lambda (e)
	       (cons (global-key-binding (car e)) e))
	     keys-alist)))
  "Alist of commands, their key sequences, and whiny complaints.")

(defun whine-say (obj)
  "Return a string computed from OBJ.
OBJ can be one of the following types of Lisp object:
- a string: return it.
- a function: call it without arguments and return the result.
- a nonempty list, if the first element is:
  - `:cat': recursively apply `whine-say' to each remaining element,
     and concatenate the results separated by spaces;
  - `:catns': same as `:cat', but concatenate without spaces;
  - otherwise, apply `whine-say' to a randomly selected element."
  (cond
   ((stringp obj) obj)
   ((functionp obj) (funcall obj))
   ((atom obj) (signal 'wrong-type-argument (list obj)))
   ((memq (car obj) '(:cat :catns))
    (mapconcat #'whine-say (cdr obj)
	       (if (eq (car obj) :cat) " " "")))
   (t (whine-say (nth (random (length obj)) obj)))))

(defun whine-current-word ()
  "Return the word at or near point."
  (or (current-word nil 'really-word)
      ;; No word found, try harder.
      (save-excursion
	(skip-syntax-backward "^w")
	(current-word nil 'really-word))))

(defvar whine-last-complaints nil)

(defun whine-post-command-hook ()
  "Hook function that is run after execution of each command."
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
      (message (whine-say complaints))
      (setq whine-last-complaints nil))
     (whine-last-complaints
      (message (whine-say whine-last-complaints))
      (setq whine-last-complaints nil)))))

;;;###autoload
(defun whinify ()
  "Make certain interactive commands whine.
The command `unwhinify' makes them normal again."
  (interactive)
  (add-hook 'post-command-hook 'whine-post-command-hook))

(defun unwhinify ()
  "Make the whiny commands normal again."
  (interactive)
  (remove-hook 'post-command-hook 'whine-post-command-hook))

(provide 'whine)

;;; whine.el ends here
