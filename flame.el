;;; flame.el --- automatic generation of flamage, as if we needed more

;; Copyright (C) 1986, 1987 Ian G. Batten
;; Copyright (C) 1994, 2002 Noah S. Friedman

;; Author: Ian G. Batten <batten@uk.ac.bham.multics>
;;         Noah Friedman <friedman@splode.com>
;; Maintainer: Noah Friedman <friedman@splode.com>
;; Keywords: games

;; $Id: flame.el,v 1.6 2015/06/30 20:27:12 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program was inspired from a version written in 1986 by
;; Ian G. Batten <batten@uk.ac.bham.multics>, which had been derived from
;; an ancient yacc program.  Other than reusing the same sentence fragment
;; templates for the grammar below, this is a complete rewrite.

;; The comments from the original version read as follows:
;;
;;         "Flame" program.  This has a chequered past.
;;
;;         The original was on a Motorola 286 running Vanilla V.1,
;;         about 2 years ago.  It was couched in terms of a yacc (I think)
;;         script.  I pulled the data out of it and rewrote it as a piece
;;         of PL/1 on Multics.  Now I've moved it into an emacs-lisp
;;         form.  If the original author cares to contact me, I'd
;;         be very happy to credit you!
;;
;;         Ian G. Batten, Batten@uk.ac.bham.multics
;;
;; On 1994-01-09, I discovered that rms dropped this file from the Emacs 19
;; distribution; this was sometime before 19.7 was released.  He made no
;; ChangeLog entry and didn't keep the source file around (by convention,
;; we usually renamed files we wanted to keep but not go into official
;; distributions so that they started with `=', e.g. `=flame.el').  This is
;; all he had to say about it when I asked:
;;
;;       I think I decided I was unhappy with the legal papers for it.
;;       Removing it took less time than trying to deal with it
;;       any other way.
;;
;; At the time, I was unable to locate Ian Batten though he has sinced
;; resurfaced via the web.

;;; Code:

(defvar flame-grammar
  '((sentence
     "*adjective *group-adj *der-term!"
     "*der-term!"
     "*statement, huh?  So, *statement?"
     "*statement, right?"
     "*statement."
     "ban *thing^s!"
     "ban the war on *war-thing!"
     "don't you realise that *statement?"
     "furthermore, you *der-term, *statement."
     "how can you be so *adjective?"
     "how can you say that *statement?"
     "I can't believe how *adjective you are."
     "I couldn't care less about your *thing."
     "I don't want to hear about your *thing."
     "I firmly believe that *statement."
     "I have many *group-adj friends."
     "I mean, *sentence"
     "I'll bet you think that *thing^s are *adjective."
     "I've never heard anything as ridiculous as the idea that *statement."
     "it takes a *group-adj *der-term like you to say that *statement."
     "it's well known that *statement."
     "let me tell you something, you *der-term, *statement."
     "man, *sentence"
     "no nukes!"
     "only a *der-term like you would say that *statement."
     "save the *thing^s!"
     "why, *statement!"
     "you have the *quality of a *der-term."
     "you know, *statement."
     "you make me sick."
     "you must be a real *der-term to think that *statement."
     "you sound like a real *der-term."
     "you *adjective *group-adj *der-term!"
     "you're a typical *group-adj person, totally *adjective."
     "you're always totally wrong."
     "you're probably *group-adj yourself."
     "your *quality is matched only by your *quality."
     "your *quality reminds me of a *thing.")

    (quality
     "*adjective *quality"
     "bad grammar"
     "depravity"
     "dishonesty"
     "ignorance"
     "lack of common decency"
     "lack of intelligence"
     "lousiness"
     "narrow-mindedness"
     "nastiness"
     "poor hygiene"
     "prejudice"
     "rotten spelling"
     "stupidity"
     "subtlety"
     "ugliness"
     "worthlessness")

    (adjective
     "*adjective and *adjective"
     "abusive"
     "as *adjective as a *thing"
     "bad"
     "blasphemous"
     "bloated"
     "boring"
     "braindamaged"
     "cheesy"
     "crass"
     "criminal"
     "depressing"
     "disgusting"
     "dogmatic"
     "dumb"
     "egocentric"
     "egregious"
     "fallacious"
     "fatuous"
     "felonious"
     "flatulent"
     "foul"
     "gratuitous"
     "heretical"
     "hilarious"
     "histrionic"
     "hypocritical"
     "idiotic"
     "ignorant"
     "improper"
     "incompetent"
     "insipid"
     "intolerable"
     "libelous"
     "lousy"
     "ludicrous"
     "malignant"
     "misanthropic"
     "nasty"
     "nonsensical"
     "odious"
     "pathetic"
     "pedantic"
     "perverted"
     "phoney"
     "pompous"
     "ponderous"
     "preposterous"
     "presumptuous"
     "primitive"
     "psychotic"
     "puerile"
     "repellent"
     "repugnant"
     "sadistic"
     "satanic"
     "selfish"
     "shallow"
     "sick"
     "sophomoric"
     "stupid"
     "superfluous"
     "unpleasant"
     "vacuous")

    (der-term
     "*adjective *der-term"
     "*der-term *der-term"
     "arsehole"
     "beast"
     "capitalist"
     "coward"
     "creep"
     "cretin"
     "DAF driver"
     "disease carrier"
     "dogmatist"
     "drunk"
     "fascist"
     "fathead"
     "fool"
     "geek"
     "goof"
     "idiot"
     "ignoramus"
     "incompetent"
     "jerk"
     "lamer"
     "lunkhead"
     "luser"
     "maniac"
     "marxist"
     "meathead"
     "moron"
     "Nazi"
     "Neanderthal"
     "pea brain"
     "peasant"
     "quiche-eater"
     "racist"
     "rat"
     "sadist"
     "sexist"
     "slimebag"
     "spaz"
     "SysThug"
     "terrorist"
     "twit"
     "wally"
     "wanker"
     "weakling"
     "weasel"
     "whimpering scumbag"
     "wimp")

    (thing
     "*adjective *thing"
     "bug fix"
     "business plan"
     "cardboard cut-out"
     "computer"
     "copyright"
     "cruise missile"
     "disease"
     "dog"
     "drug habit"
     "graphics editor"
     "Honeywell dps8"
     "Jewish grandmother"
     "lawyer"
     "license agreement"
     "Lisp Machine"
     "mental problem"
     "MicroVAX II"
     "MP3"
     "operation"
     "PAD"
     "processed lunch meat"
     "prostitute"
     "punk haircut"
     "random frob"
     "real-time clock"
     "religion"
     "right wing death squad"
     "sexist joke"
     "sexual fantasy"
     "sexual problem"
     "shopping mall"
     "source license"
     "surfboard"
     "system call"
     "ten-incher"
     "terrorist"
     "text editor"
     "TI 99/4a"
     "vegetable"
     "venereal disease"
     "venture capitalist"
     "VIC-20"
     "whale"
     "wood-burning stove")

    (group-adj
     "*adjective"
     "*thing^-loving"
     "black"
     "Chinese"
     "communist"
     "crazy"
     "dead"
     "fascist"
     "feminist"
     "foreign"
     "French"
     "funny-looking"
     "gay"
     "homosexual"
     "intellectual"
     "Iranian"
     "Italian"
     "Jewish"
     "lesbian"
     "mentally retarded"
     "Mexican"
     "Moonie"
     "Nicaraguan"
     "old"
     "Polish"
     "poor"
     "Puerto Rican"
     "religious"
     "rich"
     "short"
     "unborn"
     "underpriviledged"
     "white"
     "working"
     "young")

    (statement
     "*group-adj people are all *adjective *der-term^s"
     "*group-adj people are all *adjective because *reason"
     "*group-adj people are inherently superior to *group-adj people"
     "*group-adj people are *adjective"
     "*group-adj people are *adjective, and *reason"
     "*group-adj people don't get married to *group-adj people because *reason"
     "*person's ghost is living in your *thing"
     "*person's *quality is *adjective"
     "*person is a cool dude"
     "*person is a *der-term"
     "*person is just a figment of your imagination"
     "*person is secretly *group-adj"
     "*person is *adjective"
     "*person is *group-adj"
     "*person killed *person"
     "*person was *group-adj"
     "*quality is pretty fun"
     "*statement, and *statement"
     "*statement, because *statement"
     "*statement, but *statement"
     "*thing^s are fun"
     "a *group-adj man ain't got nothing in the world these days"
     "all *group-adj dudes should get *thing^s"
     "breathing causes cancer"
     "every *group-adj person is a *der-term"
     "everything is really a *thing"
     "God is Dead"
     "I was *person in a previous life"
     "I wish I had a *thing"
     "I wish I were a *thing"
     "if you've seen one *thing, you've seen them all"
     "it's a wonderful day"
     "it's fun to be really *adjective"
     "it's OK to run down *group-adj people"
     "just because you read it in the *paper that doesn't mean it's true"
     "Lassie was *group-adj"
     "life is a *thing"
     "life is *quality"
     "men are inherently superior to women"
     "most *group-adj people have *thing^s"
     "*computer-os is a really *adjective operating system"
     "my *thing is pretty good"
     "people are dying every day"
     "technology is evil"
     "the *conspirators killed *person"
     "the Earth is flat"
     "the Martians are coming"
     "the more *thing^s you have, the better"
     "the oceans are full of dirty fish"
     "the *conspirators are tapping your phone"
     "the sexual revolution is over"
     "the system staff is fascist"
     "the world is full of *der-term^s"
     "the *group-adj culture is fascinating"
     "the *group-adj people have really got it all together"
     "the *paper is always right"
     "the *war-thing is great"
     "there is life after death"
     "there's a *thing in *person's brain"
     "trees are *adjective"
     "we should end the war on *war-thing"
     "we should fight the war on *war-thing"
     "when ya gotta go ya gotta go"
     "women are inherently superior to men"
     "you have a *thing"
     "you hope that *statement"
     "you look like a *thing"
     "you must be a *adjective *der-term to think that *person said *statement"
     "you remind me of *person"
     "you should have a *thing"
     "you think that *statement"
     "you wish you were a *thing"
     "you wish you were *group-adj"
     "you're a *der-term"
     "you're *group-adj"
     "your *thing is great")

    (paper
     "Beano"
     "Boston Glob"
     "Centre Bulletin"
     "Daily Express"
     "Daily Mail"
     "Daily Mirror"
     "Daily Telegraph"
     "Multics Manual"
     "New York Times"
     "Pravda"
     "Sun"
     "Wall Street Journal")

    (person
     "\"Head-for-the-mountains\" Bush"
     "Alaric the Visigoth"
     "Attilla the Hun"
     "Bill Gates"
     "Caesar"
     "Fidel Castro"
     "Dennis Ritchie"
     "Gadaffi"
     "Groucho"
     "Heidegger"
     "Henry Kissinger"
     "Hitler"
     "JFK"
     "Karl Marx"
     "Ken Thompson"
     "Michael Jackson"
     "Napoleon"
     "Nietzsche"
     "Nixon"
     "Reagan"
     "Thatcher"
     "the Pope"
     "Yassir Arafat")

    (conspirators
     "CIA"
     "Cubans"
     "Democrats"
     "drug cartels"
     "FBI"
     "Illuminati"
     "Israelis"
     "KGB"
     "Mafia"
     "Martians"
     "NSA"
     "PLO"
     "Republicans"
     "Russians"
     "Yakuza")

    (computer-os
     "AIX"
     "CP/M"
     "Genera"
     "ITS"
     "Linux"
     "MS-DOG"
     "Multics"
     "MVS"
     "OS/9"
     "OSX"
     "Primos"
     "Unix"
     "Windoze"
     "VMS")

    (war-thing
     "*der-term^s"
     "democracy"
     "drugs"
     "freedom"
     "homelessness"
     "poverty"
     "spammers"
     "terrorism")

    (reason
     "*person wouldn't have done it"
     "they can't afford *thing^s"
     "they can't spray paint that small"
     "they can't tell them apart from *group-adj dudes"
     "they don't have *thing^s"
     "they don't know how"
     "they don't want their children to grow up to be too lazy to steal"
     "they're too *adjective")))

 
;; public functions

;;;###autoload
(defun flame (&optional n insertp)
  "Generate a random flame.
Prefix argument N means generated a flame of N sentences.

If called interactively, the results are displayed in a temporary buffer.
 Use \\[insert-flame] to insert a flame into the current buffer.

If called from lisp, a list of N sentences is returned.
If optional arg INSERTP is non-nil, the sentences are inserted into the
 current buffer at point, formatted as a paragraph.
 \(Use `flame-paragraph' to obtain a formatted paragraph as a string.\)"
  (interactive "p")
  (or n (setq n 1))
  (let ((l nil))
    (while (not (zerop n))
      (setq l (cons (flame-string) l))
      (setq n (1- n)))
    (and (or insertp (interactive-p))
         (flame-display (mapconcat 'identity l "  ") "*Flame*" insertp))
    l))

;;;###autoload
(defun insert-flame (&optional n)
  "Insert a flame into the current buffer.
Prefix argument N means insert N sentences, formatted as a paragraph."
  (interactive "p")
  (flame n t))

;;;###autoload
(defun flame-string ()
  "Generate an inflammatory statement, and return it."
  (flame-capitalize (flame-substitute 'sentence) t))

;;;###autoload
(defun flame-paragraph (&optional n)
  "Generate a paragraph of inflammatory stagements, and return it.
Argument N determines how many sentences are in the paragraph."
  (let ((buf (generate-new-buffer " *flame-paragraph*")))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          (insert-flame n)
          ;; don't return final newline
          (buffer-substring (point-min) (1- (point-max))))
      (kill-buffer buf))))

;;;###autoload
(defun psychoanalyze-flamer ()
  "Mr. Angry goes to the analyst."
  (interactive)
  (doctor)                              ; start the psychotherapy
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (insert-flame (1+ (random 2)))
    (insert "\n")
    (sit-for 0)
    (doctor-ret-or-read 1)))

 
(defun flame-display (string temp-buffer-name insertp)
  (let ((temp-buffer-show-function temp-buffer-show-function)
        (temp-buffer-show-hook
         (function (lambda ()
                     (fill-paragraph nil)))))

    (cond (insertp
           (save-restriction
             (narrow-to-region (point) (point))
             (insert string)
             (fill-paragraph nil)))

          (t
           ;; Play nice with JBW's temp-buffer window height
           ;; minimization hacks
           (and (eq temp-buffer-show-function 'show-temp-buffer)
                (setq temp-buffer-show-function
                      (function (lambda (buf)
                                  (save-excursion
                                    (set-buffer buf)
                                    (fill-paragraph nil))
                                  (show-temp-buffer buf)))))
           (with-output-to-temp-buffer temp-buffer-name
             (princ string))))))

;; Return a random member of list L.
(defsubst flame-random-member (l)
  (nth (random (length l)) l))

;; Tokenize sentence so substitution words are separated.
;; The character `^' can be used to join suffixes to the end of a
;; substitution token, but do not show up in the resulting list.
;; e.g. "Many *word^s with *sub, with 2^32 at *end."
;;      => ("Many " "*word" "s with " "*sub" ", with 2^32 at " "*end" ".")
(defun flame-split (s)
  (let ((l nil)
        (p 0)
        (len (length s))
        (md (match-data)))
    (while (and p (string-match "[*!][a-z-]+" s p))
      (and (< p (match-beginning 0))
           (setq l (cons (substring s p (match-beginning 0)) l)))
      (setq l (cons (substring s (match-beginning 0) (match-end 0)) l))
      (setq p (match-end 0))
      (cond ((>= p len)
             (setq p nil))
            ((char-equal ?^ (aref s p))
             (setq p (1+ p)))))
    (and p
         (< p len)
         (setq l (cons (substring s p) l)))
    (store-match-data md)
    (nreverse l)))

;; Rudimentary pluralization correction
(defun flame-plural-correct (l)
  (let ((p l)
        elt end)
    (while (and p (cdr p))
      (cond ((char-equal ?s (aref (car (cdr p)) 0))
             (setq elt (car p))
             (setq end (aref elt (1- (length elt))))
             (cond ((char-equal end ?y)
                    (setcar p (concat (substring elt 0 -1) "ie")))
                   ((char-equal end ?x)
                    (setcar p (concat elt "e"))))))
      (setq p (cdr p))))
  l)

;; Iterate over string STR, replacing all substrings beginning
;; with a '*' or '!'  with a random selection from the appropriate list.
(defun flame-iterate-list (str)
  (let* ((l (flame-split str))
         (p l)
         elt)
    (while p
      (setq elt (car p))
      (cond ((= (length elt) 1))
            ((= (aref elt 0) ?*)
             (setcar p (flame-substitute elt)))
            ((= (aref elt 0) ?!)
             (setcar p (flame-capitalize (flame-substitute elt)))))
      (setq p (cdr p)))
    (mapconcat 'identity (flame-plural-correct l) "")))

(defun flame-substitute (category)
  (let ((tbl (flame-category category)))
    (if tbl
        (flame-iterate-list (flame-random-member tbl))
      category)))

(defun flame-category (category)
  (and (stringp category)
       (setq category (intern (substring category 1))))
  (cdr (assq category flame-grammar)))

(defun flame-capitalize (s &optional destructive)
  (or destructive
      (setq s (copy-sequence s)))
  (aset s 0 (upcase (aref s 0)))
  s)

(provide 'flame)
(provide 'friedman-flame)

;; flame.el ends here.

