<pre>
; To show French holidays (and only those) in your Emacs calendar,
; put the following lines into your .emacs:
;   (require 'french-holidays)
;   (setq calendar-holidays holiday-french-holidays)

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar holiday-french-holidays nil
  "French holidays")

(setq holiday-french-holidays
      `((holiday-fixed 1 1 "Jour de l'an")
	(holiday-fixed 1 6 "Épiphanie")
	(holiday-fixed 2 2 "Chandeleur")
	(holiday-fixed 2 14 "Saint Valentin")
	(holiday-fixed 5 1 "Fête du travail")
	(holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
	(holiday-fixed 6 21 "Fête de la musique")
	(holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
	(holiday-fixed 8 15 "Assomption (Religieux)")
	(holiday-fixed 11 11 "Armistice de 1918")
	(holiday-fixed 11 1 "Toussaint")
	(holiday-fixed 11 2 "Commémoration des fidèles défunts")
	(holiday-fixed 12 25 "Noël")
        ;; fetes a date variable
	(holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc -47 "Mardi gras")
	(holiday-float 5 0 4 "Fête des mères")
	;; dernier dimanche de mai ou premier dimanche de juin si c'est le
	;; même jour que la pentecôte TODO
	(holiday-float 6 0 3 "Fête des pères"))) ;; troisième dimanche de juin

(provide 'french-holidays)
</pre>
