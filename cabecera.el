;;; cabecera.el , add interpreter headers and comments at beginning of script

;; Author: Javier Ballesteros
;;$Id: cabecera.el,v 1.3 2003/05/14 07:48:41 heraclito Exp $

;; Copyright (C) 2003 Javier Ballesteros, jballes@molina.subs.map.es

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; Javier Ballesteros 23/04/2003
;; funcion en lisp para generar una cabecera de programa cada vez que se
;; abre un fichero en perl, bash o expect , la idea es generar una
;; lista de patrones para que esto sea valido en cualquier lenguaje,
;; es decir valga para cualquier intérprete.


;; TODO: Aadir mas lenguajes de programacion y seleccionar el tipo de
;; comentario, por ahora solo vale para programas cuyos comentarios
;; sean '#' 

;;INSTALL
;; Para que funcione de forma automática hay que poner las siguientes
;; líneas al fichero .emacs y siempre suponiendo que el fichero se
;; ecuentre en el path de busqueda de emacs:
;;
;;   Add this to your .emacs
;;
;;(load "cabecera.el")
;;
;; (add-hook 'perl-mode-hook 
;; 	  '(lambda () (modo-prog "perl")))

;; (add-hook 'sh-mode-hook 
;; 	  '(lambda () (modo-prog "bash")))


(defun modo-prog (modo)
"Add headers and comments at beginnig of file, in bash or perl

If the script headers doesn't exist add the rigth interpreter header. Perl and bash are supported and expect is planned, until an expect mode is writted"

(interactive "s")
(defconst expectv "#!/usr/bin/expect" "location of expect interpreter")
(defconst bashv "#!/bin/bash" "location of bash interpreter")
(defconst perlv "#!/usr/bin/perl" "location of perl interpreter")

(if (or 
 (progn 
   (if (string= modo "perl")
       (setq manda-modo perlv)
     ()))
 (progn 
   (if (string= modo "expect")
       (setq manda-modo expectv)
     ()))
 (progn 
   (if (string= modo "bash")
       (setq manda-modo bashv)
     ())))
;;Entonces el modo es correcto
;;    (message "La cosa es %s" manda-modo)
    (inserta-cadena manda-modo)
(message "%s Unknown mode [C-h f modo-prog]" modo)))


(defun inserta-cadena (elmodo)
"Inserts the interpreter string in the current buffer, if is not present already"
(interactive "s")

;; No realizamos comprobacion de los parametros, ya que la funcion es
;; llamda dentro del mismo programa. 

(save-excursion
  (goto-char (point-min))
(if (re-search-forward elmodo 100 t)
    ()
  (progn 
     (insert elmodo)
     (setq nombre user-full-name)
     (setq hora (concat "\n" "# " (current-time-string) "\n"))
     (setq cadena (concat "# " nombre ", " user-mail-address "\n"))
     (insert hora)
     (insert cadena))))
(goto-line 4))

;;Entry log

;;$Log: cabecera.el,v $
;;Revision 1.3  2003/05/14 07:48:41  heraclito
;;Added comments in english, an corrected some comments in defvars
;;
;;Revision 1.2  2003/05/13 11:23:41  heraclito
;;Se ha creado la función que genera la cadena y la añade, además se ha creado de forma correcta la función principal, que comprueba los parámetros y luego llama a inserta-cadena.
;;
;;Revision 1.1  2003/05/13 10:30:00  heraclito
;;Initial revision
;;
(provide 'cabecera)
;;; cabecera.el ends here
