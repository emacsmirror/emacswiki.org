;;; org-notify.el --- 
;; 
;; Filename: org-notify.el
;; Description: 
;; Author: Christian Giménez
;; Maintainer: 
;; Created: dom jul  3 17:05:13 2011 (-0300)
;; Version: 
;; Last-Updated: dom mar 10 18:48:19 2013 (-0300)
;;           By: Christian
;;     Update #: 6
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `todochiku', `xcowsay'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Org-notify permite generar notificaciones al ejecutar
;; interactivamente `org-notify'. 
;; Justamente, genera un listado de los "deadlines" que se están por
;; venir en 14 días. De este listado, mostrará todos los títulos
;; visibles por medio de la función `todochiku-message'. 
;; Está hecho para mostrar de a varios títulos separados por un
;; retorno de carro cada uno, por cada notificación. Por defecto son 5
;; títulos por notificación, sin embargo puede "customizarse".
;; 
;; También se reproduce un sonido que puede definirse. Yo usé el de
;; ekiga "newmessage.wav" sin embargo usted puede cambiarlo.
;; Inclusive, puede editar el reproductor que usé por otro más
;; adecuado cambiando el comando del reproductor en la función
;; `notificar-lista'. Observese que los parámetros se pone entre
;; comillas separado argumento por argumento. 
;; Ejemplo:
;; "aplay" "-argumento 1" "-argumento 2" "-argumento 3" 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'todochiku)
(require 'xcowsay)

(defgroup org-notify nil 
  "Org-notify es un notificador para el org-mode de emacs.
Busca los deadlines y los muestra notificándolos por medio de `todochiku'."
  :group 'org
  :version "23.0"
  :tag "org notify org-notify"
  )

(defcustom org-notify-archivo-sonido
  "/usr/share/sounds/ekiga/newmessage.wav"
  "Archivo de sonido a ejecutar por el reproductor `aplay'. 
Intente que este archivo esté en formato \"wav\"."
  :group 'org-notify
  :type '(file :must-match t)
  )

(defcustom org-notify-archivo-icono
  "/home/cng/emacsstuff/todochiku/todochiku-icons/bell.png"
  "Archivo de icono que se utilizara para `todochiku-message'. 
Será el icono que figurará en el mensaje de notificación."
  :group 'org-notify
  :type '(file :must-match t)
  )

(defcustom org-notify-cant-eltos
  5
  "Cantidad de títulos(deadlines) a mostrar por cada notificación."
  :group 'org-notify
  :type 'integer
  )

(defcustom org-notify-enabled
  t
  "Habilitar o no org-notify."
  :group 'org-notify
  :type 'boolean
  )


(defun org-notify-disable ()  
  "Deshabilitar temporalmente org-notify."
  (interactive)
  (setq org-notify-enabled nil)
  )

(defun org-notify-enable ()
  "Habilitar temporalmente org-notify."
  (interactive)
  (setq org-notify-enabled t))

;;;###autoload
(defun org-notify ()
  "Notifica en pantalla(usando `todochiku-message') todos los deadlines dentro de los 14 días."
  (interactive)
  
  (when org-notify-enabled
    (org-check-deadlines 14)
    (beginning-of-buffer)
    (setq lst (buscar-deadlines-todos '()))
    (notificar-lista lst)
    )
  )

;;TODO: Hacer que muestre de a cinco TODOs por notificación.
(defun primeros-eltos (lista n)
  "Devuelve los primeros n elementos de lista"
  (if (and lista (> n 0))
      (progn 
	(cons 
	 (car lista)
	 (primeros-eltos (cdr lista) (- n 1))
	 )
	)
    )
  )
      
(defun concatenar-eltos (lista &optional separador)
  "Concatena todos los elementos de la lista formando un solo string.
Utilizar el separador para separar entre strings al concatenar"
  (when lista
    (concat (car lista) separador (concatenar-eltos (cdr lista) separador))
    )
  )



(defun notificar-usando-todochiku (lista)
  "Usando `todochiku-message' notificar todos los elementos de la lista(que se esperan que sean strings) uno por uno"
  (if lista
      (progn
	;;buscar los primeros 5 eltos.
	(setq aux-lst (primeros-eltos lista org-notify-cant-eltos))
	;; Concatenar y formar un solo string
	(setq str (concatenar-eltos aux-lst "\n"))

	;; Quitar los 5 primeros elementos para llamar de nuevo
	(setq lista-nueva (last lista 
				(- (length lista) org-notify-cant-eltos)
				))
	
	;;(todochiku-message (buffer-name) str org-notify-archivo-icono)	
	;; No me anda notify-send!... :-S
	(xcowsay (format "%s\n"
			 str))

	;; notificar los otros cinco
	(notificar-usando-todochiku lista-nueva)
	)
    )      
  )

(defun notificar-lista (lista)
  "Notificar uno por uno cada elemento(se esperan que sean strings) de la lista usando `todochiku-message'."
  (when lista
    ;; reproducimos un sonido
    (start-process "*org-notify-sound*" "*org-notify-sound*" "aplay"
		   "-q" org-notify-archivo-sonido)
    ;; mostrar una notificación en pantalla
    (notificar-usando-todochiku lista)
    )    
  )

(defun ir-sig-deadline ()
  "Mover el cursor al siguiente deadline del buffer."
  (if (search-forward "DEADLINE:" nil t)
      ;; Se encontró un DEADLINE...
      ;; ¿es invisible?
      (if (get-char-property (match-beginning 0) 'invisible)
	  (ir-sig-deadline)  ;; ¡sí! Ignorarlo... ir al siguiente:    
	(goto-char (match-beginning 0)) ;; No... éste es el siguiente...
	)
    ;; No hay DEADLINE visible... devolver nil
    nil
    )
  )

(defun obtener-heading ()
  "Devuelve el encabezado del texto en donde está el cursor."
  ;; Buscar el título(tip: empieza con asterisco) y devolver lo que encuentre.
  (search-backward-regexp "^[\\*]+ .*")
  (match-string-no-properties 0)
  ;; ¿le sacaría los asteriscos? ¡por ahora nah!
  )

(defun buscar-deadlines-todos (lst)  
  "Buscar todos los deadlines y agregar en la lista lst los títulos a los que corresponden.
Devolver esa lista de títulos de los deadlines visuales."  
  ;; Ir al siguiente deadline...
  (if (ir-sig-deadline)      
      (progn
	;; Si hay... obtener el título y agregarlo a la lista...
	(setq titulo (obtener-heading))
	(setq lst (push titulo lst))
	(ir-sig-deadline)
	(move-end-of-line 1)
	(buscar-deadlines-todos lst)
	)
    ;; No hay otro deadline...
    lst
    )
  )
     

(provide 'org-notify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-notify.el ends here
