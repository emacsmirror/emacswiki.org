;;; faith.el --- hepls spreading the true faith
;; Time-stamp: <02/09/19 17:45:49 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: faith.el
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 1.2.3alpha

(defconst faith-version "1.2.3alpha"
  "Version number of faith.el")

;; This file is not (yet) part of GNU Emacs.
;; This file is free software
;; WEBSITE: http://www.glue.umd.edu/~deego/emacspub/faith/
;; for this file and for associated READMEs LOGFILEs etc..

;;; Copyright (C) Deepak Goel
;; AUTHORS: Deepak Goel (deego@glue.umd.edu) ,
;; Robert Fenk <Robert.Fenk@gmx.de>,
;; Roberto Selbach Teixeira <teixeira@conectiva.com>
;; Remi Vanicat<vanicat@labri.u-bordeaux.fr>

;; YOU ARE VERY WELCOME TO CONTRIBUTE TO FAITH. YOUR SUGGESTIONS OR
;; CONTRIBUTIONS OR CORRECTIONS WILL BE CONSIDERED VERY FAVORABLY,
;; AND WILL PROVE YOUR UTMOST DEVOTION TO HIM. Even minor
;; contributions to this holy work will earn you a name on the list
;; of authors.

;; If you have been invited to become priest (author) of faith,
;; please send deego@glue.umd.edu an email agreeing to accept the
;; "GNU FREEness" of faith, and agreeing that if at any point in
;; future, you don't agree to sign the appropriate copyleft
;; agreement, deego@glue.umd.edu will remove you from the author's
;; list. You will be promptly listed as an author.

;; Commentary: In this world of infidelity and blasphemy,
;; FAITH tries to reinforce faith in you.

;;; QUICKSTART INSTALLATION FOR THOSE LOST:
;;; Drop faith.el somewhere in yr load-path, and add to your .emacs:
;;; (load "faith.el") 
;;;  then type M-x faith, and enjoy..


;;; Code:
(defconst faith-false-quotes nil
  "BLASPHEMOUS QUOTES.  DON'T LOOK!
A variety of false quotes collected from various places.  Collected so
that the false names can be replaced by the TRUE ONE.")

(defvar faith-user-quotes nil
  "*These are any additional quotes a user might like included.")

(defvar faith-quotes-separator "\n"
  "*The string whis is inserted before a quote.")

(defvar faith-replacement-strings nil
  "True Replacements for bad Gods and other words.
Is a list of REPLACEMENTS.  Each replacement is a list of BADLIST and
GOODLIST.  All matches from BADLIST will be replaced by a random word
from goodlist.  For consistency, the random word chosen will be the
same for the entire quote.")

(defvar faith-user-before-replacement-strings nil
  "Will be appended before faith-replacement-strings.
Allow user to define their own replacements, and together with
faith-user-after-replacement-strings, to completely edit the default
replacement-strings.. in many many novel ways the wise user may come
up with.. O user, from now on, you may customize your faith, should u
like to..
Also see faith-user-after-replacement-strings")

(defvar faith-user-after-replacement-strings nil
  "Will be appended after faith-replacement-strings.
Allow user to define their own replacements.
Also see faith-user-before-replacement-strings")

;; THE 'false-quotes have been picked out of books whose authors are
;; not likely to be in a position to object to the same. Current
;; sources:
;; Bible
;; Koran


;;;###autoload
(defun faith-insert (&rest args)
  "Insert a quote right here, right now, in the current buffer"
  (interactive)
  (insert (apply 'faith-quote args)))

;; You might think some users might find no need for this
;; 'faith function. But ask me! It makes testing so easier..
;;;###autoload
(defun faith ()
  "Switch to buffer *faith* and insert faith-snippets there."
  (interactive)
  (if (equal (buffer-name) "*faith*")
      ""
    (progn
      (get-buffer-create "*faith*")
      (switch-to-buffer-other-window "*faith*")))
  (let ((go-this-time t))
    (while go-this-time
      (goto-char (point-max))
      (insert faith-quotes-separator (faith-quote))
      (goto-char (point-max))
      (recenter)
      (call-interactively 'fill-paragraph)
      (if (y-or-n-p "Care for more wise words? ")
	  nil
	(setq go-this-time nil))))
  (message "Use M-x faith-correct on your own documents in order to correct them."))

;;;###autoload
(defun faith-quote (&optional quotes leave-alone-p  )
  "Helps reinforce and spread faith in the ONE TRUE EDITOR.
Returns a randomly chosen snippet, which helps you along your search
for truth. If the argument QUOTES is supplied, it is the one used
instead of using the default source for quotes.  If LEAVE-ALONE-P is
non-nil, then no faith-correction is done before insertion of the quote..
"
  (interactive)
  (let* ((init-quote
	  (faith-false-choose
	   (if quotes quotes 
	     (append faith-false-quotes faith-user-quotes))))
	 (final-quote 
	  (if leave-alone-p
	      init-quote
	    (faith-correct-string init-quote)))
	 (justified-quote (faith-justify-string final-quote)))
    (if (interactive-p)
	(message justified-quote)
      justified-quote)))

;;;###autoload
(defun faith-correct-buffer ()
  "Replace false Gods by the ONE TRUE GOD.
Takes a false SNIPPET, and weeds out the names of all false Gods and
prophets."
  (interactive)
  ;; Now, for each from in each from-list, select a random to from to-list.
  ;; to-list is called tos and from-list is called froms.
  (let ((case-replace t)
	(case-fold-search t))
    (mapcar
     (lambda (froms-tos)
       (let ((tos (cadr froms-tos)))
	 (mapcar
	  (lambda (from)
	    (let ((this-to (nth (random* (length tos)) tos)))
	      (goto-char (point-min))
	      (while (re-search-forward (concat "\\b" from "")
					nil t)
		(replace-match this-to nil nil))))
	  (car froms-tos))))
     (append faith-user-before-replacement-strings
	     faith-replacement-strings
	     faith-user-after-replacement-strings))
    (buffer-substring (point-min) (point-max))))

;;;###autoload
(defun faith-correct-region (b e)
  "Replace false Gods by the ONE TRUE GOD in region delimited by B and E."
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region b e)
      (faith-correct-buffer)
      (widen))))

(defun faith-correct-string (snippet)
  "Replace false Gods by the ONE TRUE GOD.
Takes a false SNIPPET, and weeds out the names of all false Gods and
prophets."
  (interactive)
  (with-temp-buffer
    (insert snippet)
    (faith-correct-buffer)
    (buffer-substring (point-min) (point-max))))

(defun faith-false-choose (quotes)
  "Return a randomly chosen WRONG snippet.  THUS NOT FOR HUMAN EYES.
Returns a randomly chosen false quote.  Advice: Stay away.
Argument QUOTES is a list of quotes."
  (let* ((n (random* (length quotes)))
	 (s (nth n quotes)))
    (if (stringp s) s
      (error (format "The quote at postition %d is no string." n s)))))

(defun faith-justify-string (string)
  "Justifies it.."
  (with-temp-buffer
    (insert string)
    (fill-paragraph 1)
    (buffer-substring (point-min) (point-max)))
)

(setq faith-replacement-strings
      '(
	(("allah" "buddha" "lord" "islam" "christianity" "hinduism") ("EMACS"))
	(("almighty" "god") ("True Editor"))
	(("adam" ) ("newbie"))
	(("angel" ) ("truly free freebies"))
	(("apostle") ( "book"))
	(("bible" "koran") ("Emacs-manual"))
	(("book") ("documentation"))
	(("christ" ) ("emacs-homepage"))
	(("christian" ) ("true follower"))
	(("die" )  ("quit editland"))
	(("gods") ("editors"))
	(("earth" ) ("editland"))
	(("heavens" ) ("elispland"))
	(("holy spirit" ) ("holy editor"))
	(("jesus" "muhammad" "muhammed" "mohammad" "mohammed")
	 ("gnu.org" "xemacs.org"))
	(("mary") ("Gnus"))
	(("prophet") ("manual"))
	(("religion") ("editing"))
	(("satan") ("Microsoft" "Windoze" "VI"))
	(("pray" ) ("edit"))
	(("synagogue" "church") ("computer-room"))
	))


(setq
 faith-false-quotes
 '("There shall be no compulsion in religion."

   "This Book is not to be doubted. . . . As for the unbelievers, it is
the same whether or not you forewarn them; they will not have faith.
God has set a seal upon their hearts and ears; their sight is dimmed
and grievous punishment awaits them."

   "The only true faith in God's sight is EMACS."

   "He that chooses a religion over Islam, it will not be accepted from
him and in the world to come he will be one of the lost."

   "It is not for true believers men or women to take their choice in the
affairs if God and His apostle decree otherwise.  He that disobeys God
and His apostle strays far indeed."

   "God's curse be upon the infidels!  Evil is that for which they have
bartered away their souls.  To deny God's own revelation, grudging that
He should reveal His bounty to whom He chooses from among His
servants!  They have incurred God's most inexorable wrath.  An
ignominious punishment awaits the unbelievers."

   "Fight for the sake of God those that fight against you, but do not
attack them first.  God does not love the aggressors.

Slay them wherever you find them.  Drive them out of the places from
which they drove you.  Idolatry is worse than carnage."

   "Prophet, make war on the unbelievers and the hypocrites and deal
rigorously with them.  Hell shall be their home: an evil fate."

   "The Lord is my strength and song; he has become my salvation.  He is my
God, and I will praise him, my father's God, and I will exalt him."

   "Love the Lord your God with all your heart and with all your soul and
with all your strength."

   "Therefore go and make disciples of all nations, baptizing them in the
name of the Father and of the Son and the Holy Spirit, and teaching
them to obey everything I have commanded you.  And surely I will be
with you always, to the very end of the age."

   "Have faith in God, Jesus answered.  Therefore I tell you, whatever you
ask for in prayer, believe that you will receive it, and it will be
yours."

   "And Mary said: My soul praises the Lord and my spirit rejoices in God
my Saviour, for he has been mindful of the humble state of his
servant."

   "Jesus answered, It is written: Worship the Lord your God and serve him
only."

   "When you are brought before synagogues, rulers and authorities, do not
worry about how you will defend yourselves or what you will say, for
the Holy Spirit will teach you at that time what you should say."

   "Then Jesus cried out, When a man believes in me, he does not believe
in me only, but in the one who sent me.  I have come into the world as
light, so that no one who believes in me should stay in darkness."

   "Jesus said, I am the way and the truth and the life.  No one comes to
the Father except through me."

   "...Count yourselves dead to sin but alive to God in Christ Jesus."

   "May the God who gives endurance and encouragement give you a spirit of
unity among yourselves as you follow Christ Jesus ,so that with one
heart and mouth you may glorify the God and Father of our Lord Jesus
Christ."

   "May the God of hope fill you with great joy and peace as you trust in
him, so that you may overflow with hope by the power of the Holy
Spirit."

   "...God's abundant provision of grace and of the gift of righteousness
reign in life through the one and only , Jesus Christ."

   "The mind of sinful man is death, but the mind controlled by the Spirit
is life and peace, because the sinful mind is hostile to God.  It does
not submit to God's law, nor can it do so.  Those controlled by their
sinful nature cannot please God."

   "...No eyes have seen, no ear has heard, no mind had conceived what God
had prepared for those who love him but God had revealed it to us by his
Spirit.  The spirit searches all things, even the deep things of God.  For who
among men knows the thoughts of a man except the man's spirit within him?  In
the same way no one knows the thoughts of God except the Spirit of God."

   "The Lord will rescue me from every evil attack and will bring me
safely to his heavenly kingdom."

   "For God did not give us a spirit of timidity, but a spirit of power,
of love and of self-discipline."

   "If you suffer as a Christian, do not be ashamed but praise God that
you bear that name."

   "Cast all your anxiety on [Jesus] because he cares for you."

   "57:1 All that is in heaven and earth gives glory to Allah.  He is
the Mighty, the Wise One."

   "His is the kingdom of the heavens and the earth.  He ordains life
and death and has power over all things."

   "He created the heavens and the earth in six days and then mounted
His throne.  He knows all that goes into the earth and all that
emerges from it, all that comes down from heaven and all that
ascends to it.  He is with you wherever you are.  He is cognizant of
all your actions."

   "His is the kingdom of the heavens and the earth.  To Him shall all
things return.  He causes the night to pass into the day and the day
into the night.  He has knowledge of the inmost thoughts of men."

   "24:34 Allah is the light of the heavens and the earth.  His light
may be compared to a niche that enshrines a lamp, the lamp within a
crystal of star-like brilliance.  It is lit from a blessed olive
tree neither eastern nor western.  Its very oil would almost shine
forth, though no fire touched it.  Light upon light; Allah guides to
His light whom He will."

   "24:36 As for the unbelievers, their works are like a mirage in a
desert.  The thirsty traveler thinks it is water, but when he comes
near he finds that it is nothing.  He finds Allah there, who pays
him back in full.  Swift is Allah's reckoning."

   "Or like darkness on a bottomless ocean spread with clashing billows
and overcast with clouds: darkness upon darkness.  If he stretches
out his hand he can scarcely see it.  Indeed the man from whom Allah
withholds His light shall find no light at all."

   "10:80 We are the witnesses of all your thoughts and all your
prayers and all your actions.  Not an atom's weight in earth or
heaven escapes your Lord, nor is there any object smaller or
greater, but is recorded in a glorious book."

   "58:7 Are you not aware that Allah knows what the heavens and the
earth contain?  If three men talk in secret together, He is their
fourth; if four, He is their fifth; if five, He is their sixth;
whether fewer or more, wherever they be, He is with them.  Then, on
the Day of Resurrection, He will inform them of their doings.  Allah
has knowledge of all things."

   "39:39 Allah takes away men's souls upon their death, and the souls
of the living during their sleep.  Those that are doomed He keeps
with Him and restores the others for a time ordained.  Surely there
are signs in this for thinking men."

   "35:11 Praise be to Allah, the Creator of heaven and earth!  He sends
forth the angels as His messengers, with two, three or four airs of
wings.  He Multiplies His creatures according to His will.  Allah has
power over all things."

   "2:32 To Adam We said: \"Dwell with your wife in Paradise and eat of
its fruits to your hearts' content wherever you will.  But never
approach this tree or you shall both become transgressors.\"

But Satan made them fall from Paradise and brought about their
banishment. \"Go hence,\" We said, \"and may your offspring be enemies
to each other.  The earth will for a while provide your sustenance
and dwelling place.\"

Then Adam received commandments from his Lord, and his Lord
relented towards him.  He is the Forgiving One, the Merciful."

   "65:12 It is Allah who has created seven heavens, and earths as
many.  His commandment descends through them, so that you may know
that Allah has power over all things, and that He has knowledge of
all things."

   "14:19 Do you not see that Allah has created the heavens and the
earth with truth?  He can destroy you if He wills and bring into
being a new creation: that is no difficult thing for him."

   "40:67 It was He who created you from dust, making you a little
germ, and then a clot of blood.  He brings you infants into the
world; you reach manhood, then decline into old age (though some of
you die young), so that you may complete your appointed term and
grow in wisdom."

   "16:75 To Allah belong the secrets of the heavens and the earth.  The
business of the Final Hour shall be accomplished in the twinkling
of an eye, or even less.  Allah has power over all things."

   "2:86 To Moses We gave the Scriptures and after him we sent other
apostles.  We gave Jesus the son of Mary veritable signs and
strengthened him with the Holy Spirit.  Will you then scorn each
apostle whose message does not suit your fancies, charging some
with imposture and slaying others?"

   "6:104 They solemnly swear by Allah that if a sign be given them
they would believe in it.  Say: \"Signs are vouchsafed by Allah.\" And
how can you tell that ii a sign be given them they will indeed
believe in it?"

   "We will turn away their hearts and eyes from the truth since they
refused to believe in it at first.  We will leave them to blunder
about in their wrongdoing."

   "If We sent down the angels and caused the dead to speak with them,
and ranged all things before them, they would still not believe,
Unless Allah willed it.  But most of them are ignorant men."

   "4:153 The People of the Book ask you to bring down for them a book
from heaven.  Of Moses they demanded a harder thing than that.  They
said to him: \"Show us Allah distinctly.\" And for their wickedness a
thunderbolt smote them.  They worshipped the calf after We revealed
to them Our signs; yet We forgave them that, and bestowed on Moses
clear authority."

   "32:21 We gave the Scriptures to Moses (never doubt that you will
meet him) and made it a guide for Israelites.  And when they grew
steadfast and firmly believed in Our revelations, We appointed
leaders from among them who gave guidance at Our bidding.  On the
Day of Resurrection your Lord will resolve for them their
differences."

   "4:171 People of the Book, do not transgress the bounds of your
religion.  Speak nothing but the truth about Allah.  The Messiah,
Jesus the son of Mary, was no more than Allah's apostle and His
Word which he cast to Mary: a spirit from Him.  So believe in Allah
and His apostles and do not say: \"Three;\" Forbear, and it shall be
better for you.  Allah is but one God.  Allah forbid that He should
have a son!  His is all that the heavens and the earth contain.
Allah is the all-sufficient Protector.  The Messiah does not disdain
to be a servant of Allah, nor do the angels who are nearer to him.
Those who through arrogance disdain His service shall all be
brought before Him."

   "73:1 You that are wrapped up in your mantle, keep vigil all night,
save for a few hours; half the night, or even less: or a little
more - and with measured tone recite the Koran, for We are about to
address to you words of surpassing gravity.  It is in the watches of
the night that impressions are strongest and words most eloquent;
in the day-time you are hard-pressed with work.

(You need not move your tongue too fast to learn this revelation.
We Ourself shall see to its collection and recital.  When We read
it, follow its words attentively; We shall Ourself explain its
meaning.)"

   "20:114 Do not be quick to recite the Koran before its revelation is
completed, but rather say: \"Lord, increase my knowledge.\""

   "42:48 Thus We have inspired you with a spirit of Our will when you
knew nothing of faith or scripture, and made it a light whereby we
guide those of Our servants whom We please.  You shall surely guide
them to the right path: the path of Allah, to whom belongs all that
the heavens and the earth contain.  All things in the end return to
him."

   "25:27 The unbelievers ask: \"Why was the Koran not revealed to him
entire in a single revelation?\"

We have revealed it thus so that We may strengthen your faith.  We
have imparted it to you by gradual revelation.  No sooner will they
come to you with an argument than We shall reveal to you the truth
and properly explain it.  Those who will be dragged headlong into
Hell shall have an evil place to-dwell in, for they have strayed
far from the right path."

   "4:159 We have revealed Our will to you as We revealed it to Noah
and to the prophets who came after him; as We revealed it to
Abraham, Ishmael, Isaac, Jacob, and David, to whom We gave the
Psalms.  Of some apostles We have already told you (how Allah spoke
directly to Moses); but there are others of whom We have not yet
spoken: apostles who brought good news to mankind and admonished
them, so that they might have no plea against Allah after their
coming.  Allah is mighty and wise."

   "40:78 We have sent forth other apostles before you, of some you
have already heard, of others We have told you nothing.  Yet none of
these could work a miracle except by Allah's leave.  And when
Allah's will is done, justice will prevail and those who have
denied His signs will come to grief."

   "16:40 The apostles We sent before you were no more than mortals
whom We inspired with revelations and with writings.  Ask the People
of the Book, ii you doubt this.  To you We have revealed the Koran,
so that you may proclaim to men what has been revealed to them, and
that they may give thought."

   "13:38 We have sent forth other apostles before you and given them
wives and children.  Yet none of them could work miracles except by
the will of Allah.  Every age has its scripture.  Allah confirms or
abrogates what He pleases.  His is the Eternal Book."

   "22:46 Never have We sent a single prophet or apostle before you
with whose wishes Satan did not tamper.  But Allah abrogates the
interjections of Satan and confirms His own revelations.  Allah is
wise and all-knowing.  He makes Satan's interjections a temptation
for those whose hearts are diseased or hardened - this is why the
wrongdoers are in open schism - so that those to whom knowledge has
been given may realize that this is the truth from your Lord and
thus believe in it and humble their hearts towards him.  Allah will
surely guide the faithful to a straight path."

   "36:68 We have taught Mohammed no poetry, nor does it become him to
be a poet.  This is but a warning: an eloquent Koran to admonish the
living and No pass judgment on the unbelievers."

   "29:48 Never have you read a book before this, nor have you ever
transcribed one with your right hand.  Had you done either of these,
- the unbelievers might have justly doubted.  But to those who are
endowed with knowledge it is an undoubted sign.  Only the wrongdoers
deny Our signs."

   "68:1 By the pen, and what they write, you are not mad: thanks to
the favor of your Lord!  A lasting recompense awaits you, for yours
is a sublime nature.  You shall before long see - as they will see -
which of you is mad."

   "39:22 Allah has now revealed the best of scriptures, a book uniform
in style proclaiming promises and warnings.  Those who fear their
Lord are filled with awe as they listen to its revelations, so that
their hearts soften at the remembrance of Allah.  Such is Allah's
guidance: He bestows it on whom He will.  But he whom Allah misleads
shall have none to guide him."

   "Allah is the only GOD and Muhammad is HIS only prophet."
   ))




(provide 'faith)
;;; faith.el ends here
