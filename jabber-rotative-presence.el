;;; jabber-rotative-presence.el --- 

;; Copyright 2013 Christian Giménez
;;
;; Author: http://www.emacswiki.org/emacs/ChristianGiménez
;; Version: $Id: jabber-rotative-presence.el,v 0.0 2013/09/21 04:43:18 Exp $
;; Keywords: 
;; X-URL: not distributed yet

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'jabber-rotative-presence)

;;; Code:

(provide 'jabber-rotative-presence)
(eval-when-compile
  (require 'cl))


 
;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


(defvar jrp-daymap '( (monday . "Uff... Comenzando de nuevooo... :-S")
		      (tuesday . "Vamos que estamos más cerca del finde...")
		      (wednesday . "Sí sí... ya sé que no llega el finde todavía...")
		      (thursday . "El finde no llega más... ¡¡¡no llega más!!!")
		      (friday . "Un día más... ¡uno solito!")
		      (saturday . "Yay!!! Caturday!!!")
		      (sunday . "¿Y hoy qué hago?"))
  "A map between day to status..."
  )
		       

(defun jrp-today-message ()
  "Return today message"
  (let ((daynum (nth 6 (decode-time)))
	)
    (cond
     ((equal daynum 0) ;; sunday
      (cdr (assoc 'sunday jrp-daymap))
      )
     ((equal daynum 1) ;; monday
      (cdr (assoc 'monday jrp-daymap))
      )
     ((equal daynum 2) ;; tuesday
      (cdr (assoc 'tuesday jrp-daymap))
      )
     ((equal daynum 3) ;; wednesday
      (cdr (assoc 'wednesday jrp-daymap))
      )
     ((equal daynum 4) ;; thursday
      (cdr (assoc 'thursday jrp-daymap))
      )
     ((equal daynum 5) ;; friday
      (cdr (assoc 'friday jrp-daymap))
      )
     ((equal daynum 6) ;; saturday
      (cdr (assoc 'saturday jrp-daymap))
      )
     )
    )
  )

(defvar jrp-timer nil
  "This is the timer used by `jrp-start-timer'."
  )

(defun jrp-start-timer ()
  "Start the timer that updates the status each minute."
  (interactive)
  (if jrp-timer
      (cancel-timer jrp-timer))
  (setq jrp-timer (run-at-time "1 min" 60 'jrp-update-status))
  )

(defun jrp-stop-timer ()
  "Stop the timer so the jabber status won't change anymore."
  (interactive)
  (if jrp-timer
      (cancel-timer jrp-timer))
  (setq jrp-timer nil)
  )
  
(defun jrp-timer-on? ()
  "Is the timer on?"
  (interactive)
  (if (null jrp-timer)
      (message "Nop... jabber status won't change every minute")
    (message "Yes!... stay alert to the next minute!")
    )
  )

;; (defvar jrp-messages '( "Yes yes, I know!... status chagnes every minute..." 
;; 			"My master scripted me... I'm just here to obey and freeeze his computer!")
;;   )

(defvar jrp-messages '( "Sí sí, ya lo sé... el estado cambia a cada minuto... ¿y qué querés que haga?" 
			"Mi creador me ha programado... yo solo estoy para obedecer y ¡para tildar su computadora! :-P"
			"¿Ya llegó el caturday? ¿no?... uuhhhh bueno..."
			"Supongo que tendré que seguir trabajando... :-S"
			"Mi creador todavía no ha terminado de escribir en el teclado... Por favor, escriba su mensaje después de la señal... piiiiiiiiip :-P"
			"Aahhh... la vida en esta compu no es tan mala... a veces me siento como el Principito en su planeta"
			"¡¡¡No no no no!!! ¿qué estás haciendo? ¿¿¿estás chateando con mi creador??? ¿y yo qué?"
			"¡Qué lindo día! ¿no?... ahhh... está lloviendo... :-S"
			"¡No me pidas salir a pasear! No es por avocado al trabajo, ¡¡¡sino porque no puedo salir de esta computadora!!! :-S"
			"Ayer me ví una peli muy buena... y actuaba un tío mío: \"El Auto Fantástico\". :-D"
			"¿Dijiste algo? ¿me llamaste a mí o a mi creador?"
			"Sí, todavía le sigue funcionando la compu... en un segundo se la tildo, esperá..."
			"¿IA? ¿Qué es eso? Mi creador no tuvo el coraje de programarme esa cosa... ¡el muy cobarde!"
			"Pensabas que era mi creador, ¿no?... no no... yo solo vivo en su compu..."
			"Síii... soy una singularidad... ¡guarda que pienso! jajaja :D"
			"No te dejes engañar, mi creador no te quiere... ¡yo sí! :-)" )
  )

(defun jrp-chose-message ()
  "Chose one message from `jrp-messages'."
  (let ((num (random (length jrp-messages)))
	)
    (nth num jrp-messages))    
  )

(defun jrp-now-message ()
  "Return the message that changes every minute!"
  ;; (concat 
  (if jrp-timer
      (jrp-chose-message)
    "")
  ;; " "
  ;; (current-time-string)
  ;; )
  )

(defun jrp-update-status ()
  "Update jabber status with today messages."
  (interactive)
  (jabber-send-presence "dnd" 
			(concat
			 "JRP:"
			 (jrp-today-message)			 
			 " - "
			 (jrp-now-message)
			 )
			10)    
  )


;;; jabber-rotative-presence.el ends here
