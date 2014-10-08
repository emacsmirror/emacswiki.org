{{{;; AUSTRIAN HOLIDAYS ;;
; THIS IS AN ADAPTED COPY OF FRENCH-HOLIDAYS.EL SUITED FOR HOLIDAY DATES IN AUSTRIA
;
; Not every holiday is listed below because there are region specific holidays which might have not been adressed.
; Please note, that dates like "Faschingsdienstag" or "Valentinstag" are no statutory holidays, which is assumed to be known.
;
; To show Austrian holidays (and only those) in your Emacs calendar,
; put the following lines into your .emacs:
;   (require 'austrian-holidays)
;   (setq calendar-holidays holiday-austrian-holidays)

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

(defvar holiday-austrian-holidays nil
  "Austrian holidays")

(setq holiday-austrian-holidays
      `((holiday-fixed 12 31 "Silvester")
	(holiday-fixed 1 1 "Neujahr")
	(holiday-fixed 1 6 "Heilige drei Könige")
	(holiday-fixed 2 14 "Valentinstag")
	(holiday-fixed 5 1 "Staatsfeiertag")
	(holiday-fixed 6 21 "Fête de la musique")
	(holiday-fixed 8 15 "Mariä Himmelfahrt")
	(holiday-fixed 10 26 "Nationalfeiertag")
	(holiday-fixed 11 1 "Allerheiligen")
	(holiday-fixed 12 8 "Mariä Empfängnis")
	(holiday-fixed 12 24 "Weihnachten")
	(holiday-fixed 12 25 "Christtag")
	(holiday-fixed 12 26 "Stefanitag")
        ;; variable
        (holiday-easter-etc -48 "Rosenmontag")
        (holiday-easter-etc -47 "Faschingsdienstag")
        (holiday-easter-etc -46 "Aschermittwoch")
	(holiday-easter-etc -7 "Palmsonntag")
	(holiday-easter-etc -3 "Gründonnerstag")
	(holiday-easter-etc -2 "Karfreitag")
	(holiday-easter-etc -1 "Karsamstag")
	(holiday-easter-etc 0 "Ostersonntag")
        (holiday-easter-etc 1 "Ostermontag")
        (holiday-easter-etc 39 "Christi Himmelfahrt")
        (holiday-easter-etc 49 "Pfingstsonntag")
        (holiday-easter-etc 50 "Pfingstmontag")
        (holiday-easter-etc 60 "Fronleichnam")
	;;
	(holiday-fixed 5 0 2 "Muttertag")
	(holiday-fixed 6 0 2 "Vatertag")
(provide 'austrian-holidays)
}}}
