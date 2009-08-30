A proposition for a enhanced pstricks.el for using pstricks (http://tug.org/PSTricks/main.cgi/). I don't use it anymore intensively but feel free to work on it - jpgeorget

    ;; pstricks.el - makes writing pstricks's macros more easily... I hope so ;-)
    ;;
    ;; Copyright (C) 2002 Jean-Philippe Georget
    ;; Author: Jean-Philippe Georget <jpg2006 arob jpgeorget.net>
    ;;                                   ^^^^actual year
    ;; Created: 20 jan 2002
    ;; Version: $Revision: 1.1 $
    ;; Keywords: latex, pstricks, auctex, emacs
    
    
    ;; Based on AUC Tex
    ;; Copyright 1991 Kresten Krab Thorup
    ;; Copyright 1993, 1994, 1995, 1996, 1997, 1999, 2000 Per Abrahamsen <auc-tex@sunsite.dk>
    ;; X-URL: http://sunsite.dk/auctex
    
    
    ;;; This file is not part of AUC TeX or Emacs.
    
    ;; This program is free software; you can redistribute it and/or
    ;; modify it under the terms of the GNU General Public License
    ;; as published by the Free Software Foundation; either version 2
    ;; of the License, or (at your option) any later version.
    ;; 
    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.
    ;; 
    ;; You should have received a copy of the GNU General Public License
    ;; along with this program; if not, write to the Free Software
    ;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    
    
    
    ;; ****************************************************************
    ;; A faire :
    ;;
    ;; 
    ;; - rendre les historiques locaux aux buffers ? en option ?
    ;; - historique des dimensions (unit, xunit, yunit)
    ;; - gérer le formatage des coordonnées multiples comme pour \psline
    ;;   au-delà de 2 (les deux premières ont des historiques),
    ;;   (Ok pour psline et pspolygon
    ;; - psbezier est mal programmée (cf détails dans la fonction)
    ;; - gérer \SpecialCoor (coordonnées polaires qui demande "(r;d)")
    ;;   détecter sa présence qui donne la valeur "t" à une variable
    ;;   et ajouter une fonction qui gère la demande de coordonnées
    ;;   peut-être le biais de référencement de variable. Il faudrait
    ;;   un historique 
    ;; - gérer les commandes de création de nouvelles couleurs pour
    ;;   les ajouter à la liste des couleurs disponibles (il faudrait
    ;;   aussi le gérer à l'ouverture d'un fichier qui contient de telles
    ;;   définitions
    ;; - L'argument de LaTeX-pst-parameters pourrait servir à avoir plusieurs
    ;;   types de paramètres. 
    ;; - laisser la possibilité de redéfinir un objet avec C-u C-c RET 
    ;;   par exemple (utiliser les arguments des fonctions ?)
    ;; - [option] au (re)chargement (C-c C-n par exemple), redéfinir les historiques
    
    
    ;; A voir :
    ;;
    ;;- L'historique des rotations ne devraient peut-être pas être confondu avec les angles A et B
    
    ;; WISH LIST
    ;;
    ;; - tout reprogrammer ? Une unique fonction avec un argument
    ;;   du genre pst-macro(x0, y0, ... stuff...), c'est à dire
    ;;   avec une liste des infos demandées à l'utilisateur.
    ;;   Une unique fonction pour chaque macro
    ;;   \rput => pst-macro(refpoint, rotation, x0, yO, stuff) puis
    ;;   mise en forme des valeurs obtenues.
    ;;   Que se passe-t-il alors pour xn, yn par exemple ?
    ;; - "Customize" variables like parameter's name's list
    ;; - add argument's list for certain  parameters like
    ;;   dotstyle, arrows, boolean argumen, etc. It should be easy to
    ;;   add new list for a new parameter (list of list or
    ;;   hashes table ?)
    ;;****************************************************************
    
    
    ;;; Coordinates for objects
    (defvar LaTeX-pst-x0-list nil
      "A list of values for x0 in pstricks.")
    
    (defvar LaTeX-pst-x0-default '("0")
      "A default value for x0 in pstricks.")
    
    (defvar LaTeX-pst-x0-history nil
      "History of values for x0 in pstricks.")
    
    (defun LaTeX-pst-add-x0 (x0)
      "Add x0 in LaTeX-pst-x0-list if not present"
      ;; add new x0 if not in LaTeX-pst-x0-list
      (let ((entry (assoc x0 LaTeX-pst-x0-list)))
        (if (null entry) 
    	(setq LaTeX-pst-x0-list (cons (list x0) LaTeX-pst-x0-list)))))
    
    (defun LaTeX-pst-x0 (&optional arg)
      "Ask for a x0 and manage many x0's list"
      ;; arg is default value for x0
      (if (not (null arg))
        (setq LaTeX-pst-x0-default arg)
        (LaTeX-pst-add-x0 arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-x0-list) 
          (setq LaTeX-pst-x0-list (list LaTeX-pst-x0-default)))
      ;; ask for x0
      (setq x0 (completing-read  (concat "x0 : " )
    				 LaTeX-pst-x0-list
    				 nil nil LaTeX-pst-x0-default
    				 'LaTeX-pst-x0-history))
      (LaTeX-pst-add-x0 x0)
      ;; set new default for x0
      (setq LaTeX-pst-x0-default x0))
    
    
    (defvar LaTeX-pst-y0-list nil
      "A list of values for y0 in pstricks.")
    
    (defvar LaTeX-pst-y0-default '("0")
      "A default value for y0 in pstricks.")
    
    (defvar LaTeX-pst-y0-history nil
      "History of values for y0 in pstricks.")
    
    (defun LaTeX-pst-add-y0 (y0)
      "Add y0 in LaTeX-pst-y0-list if not present"
      ;; add new y0 if not in LaTeX-pst-y0-list
      (let ((entry (assoc y0 LaTeX-pst-y0-list)))
        (if (null entry) 
    	(setq LaTeX-pst-y0-list (cons (list y0) LaTeX-pst-y0-list)))))
    
    (defun LaTeX-pst-y0 (&optional arg)
      "Ask for a y0 and manage many y0's list"
      ;; arg is default value for y0
      (if (not (null arg))
        (setq LaTeX-pst-y0-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-y0-list) 
          (setq LaTeX-pst-y0-list (list LaTeX-pst-y0-default)))
      ;; ask for y0
      (setq y0 (completing-read  (concat "y0 : " )
    				 LaTeX-pst-y0-list
    				 nil nil LaTeX-pst-y0-default
    				 'LaTeX-pst-y0-history))
      (LaTeX-pst-add-y0 y0)
      ;; set new default for y0
      (setq LaTeX-pst-y0-default y0))
    
    
    (defvar LaTeX-pst-x1-list nil
      "A list of values for x1 in pstricks.")
    
    (defvar LaTeX-pst-x1-default nil
      "A default value for x1 in pstricks.")
    
    (defvar LaTeX-pst-x1-history nil
      "History of values for x1 in pstricks.")
    
    (defun LaTeX-pst-add-x1 (x1)
      "Add x1 in LaTeX-pst-x1-list if not present"
      ;; add new x1 if not in LaTeX-pst-x1-list
      (let ((entry (assoc x1 LaTeX-pst-x1-list)))
        (if (null entry) 
    	(setq LaTeX-pst-x1-list (cons (list x1) LaTeX-pst-x1-list)))))
    
    (defun LaTeX-pst-x1 (&optional arg)
      "Ask for a x1 and manage many x1's list"
      ;; arg is default value for x1
      (if (not (null arg))
        (setq LaTeX-pst-x1-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-x1-list) 
          (setq LaTeX-pst-x1-list (list LaTeX-pst-x1-default)))
      ;; ask for x1
      (setq x1 (completing-read  (concat "x1 : " )
    				 LaTeX-pst-x1-list
    				 nil nil LaTeX-pst-x1-default
    				 'LaTeX-pst-x1-history))
      (LaTeX-pst-add-x1 x1)
      ;; set new default for x1
      (setq LaTeX-pst-x1-default x1))
    
    
    (defvar LaTeX-pst-y1-list nil
      "A list of values for y1 in pstricks.")
    
    (defvar LaTeX-pst-y1-default nil
      "A default value for y1 in pstricks.")
    
    (defvar LaTeX-pst-y1-history nil
      "History of values for y1 in pstricks.")
    
    (defun LaTeX-pst-add-y1 (y1)
      "Add y1 in LaTeX-pst-y1-list if not present"
      ;; add new y1 if not in LaTeX-pst-y1-list
      (let ((entry (assoc y1 LaTeX-pst-y1-list)))
        (if (null entry) 
    	(setq LaTeX-pst-y1-list (cons (list y1) LaTeX-pst-y1-list)))))
    
    (defun LaTeX-pst-y1 (&optional arg)
      "Ask for a y1 and manage many y1's list"
      ;; arg is default value for y1
      (if (not (null arg))
        (setq LaTeX-pst-y1-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-y1-list) 
          (setq LaTeX-pst-y1-list (list LaTeX-pst-y1-default)))
      ;; ask for y1
      (setq y1 (completing-read  (concat "y1 : " )
    				 LaTeX-pst-y1-list
    				 nil nil LaTeX-pst-y1-default
    				 'LaTeX-pst-y1-history))
      (LaTeX-pst-add-y1 y1)
      ;; set new default for y1
      (setq LaTeX-pst-y1-default y1))
    
    (defvar LaTeX-pst-x2-list nil
      "A list of values for x2 in pstricks.")
    
    (defvar LaTeX-pst-x2-default nil
      "A default value for x2 in pstricks.")
    
    (defvar LaTeX-pst-x2-history nil
      "History of values for x2 in pstricks.")
    
    (defun LaTeX-pst-add-x2 (x2)
      "Add x2 in LaTeX-pst-x2-list if not present"
      ;; add new x2 if not in LaTeX-pst-x2-list
      (let ((entry (assoc x2 LaTeX-pst-x2-list)))
        (if (null entry) 
    	(setq LaTeX-pst-x2-list (cons (list x2) LaTeX-pst-x2-list)))))
    
    (defun LaTeX-pst-x2 (&optional arg)
      "Ask for a x2 and manage many x2's list"
      ;; arg is default value for x2
      (if (not (null arg))
        (setq LaTeX-pst-x2-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-x2-list) 
          (setq LaTeX-pst-x2-list (list LaTeX-pst-x2-default)))
      ;; ask for x2
      (setq x2 (completing-read  (concat "x2 : " )
    				 LaTeX-pst-x2-list
    				 nil nil LaTeX-pst-x2-default
    				 'LaTeX-pst-x2-history))
      (LaTeX-pst-add-x2 x2)
      ;; set new default for x2
      (setq LaTeX-pst-x2-default x2))
    
    
    (defvar LaTeX-pst-y2-list nil
      "A list of values for y2 in pstricks.")
    
    (defvar LaTeX-pst-y2-default nil
      "A default value for y2 in pstricks.")
    
    (defvar LaTeX-pst-y2-history nil
      "History of values for y2 in pstricks.")
    
    (defun LaTeX-pst-add-y2 (y2)
      "Add y2 in LaTeX-pst-y2-list if not present"
      ;; add new y2 if not in LaTeX-pst-y2-list
      (let ((entry (assoc y2 LaTeX-pst-y2-list)))
        (if (null entry) 
    	(setq LaTeX-pst-y2-list (cons (list y2) LaTeX-pst-y2-list)))))
    
    (defun LaTeX-pst-y2 (&optional arg)
      "Ask for a y2 and manage many y2's list"
      ;; arg is default value for y2
      (if (not (null arg))
        (setq LaTeX-pst-y2-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-y2-list) 
          (setq LaTeX-pst-y2-list (list LaTeX-pst-y2-default)))
      ;; ask for y2
      (setq y2 (completing-read  (concat "y2 : " )
    				 LaTeX-pst-y2-list
    				 nil nil LaTeX-pst-y2-default
    				 'LaTeX-pst-y2-history))
      (LaTeX-pst-add-y2 y2)
      ;; set new default for y2
      (setq LaTeX-pst-y2-default y2))
    
    ;;; Angles
    (defvar LaTeX-pst-angleA-list nil
      "A list of values for angleA in pstricks.")
    
    (defvar LaTeX-pst-angleA-default nil
      "A default value for angleA in pstricks.")
    
    (defvar LaTeX-pst-angleA-history nil
      "History of values for angleA in pstricks.")
    
    (defun LaTeX-pst-add-angleA (angleA)
      "Add angleA in LaTeX-pst-angleA-list if not present"
      ;; add new angleA if not in LaTeX-pst-angleA-list
      (let ((entry (assoc angleA LaTeX-pst-angleA-list)))
        (if (null entry) 
    	(setq LaTeX-pst-angleA-list (cons (list angleA) LaTeX-pst-angleA-list)))))
    
    (defun LaTeX-pst-angleA (&optional arg)
      "Ask for a angleA and manage many angleA's list"
      ;; arg is default value for angleA
      (if (not (null arg))
        (setq LaTeX-pst-angleA-default arg)
        (LaTeX-pst-add-angleA arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-angleA-list) 
          (setq LaTeX-pst-angleA-list (list LaTeX-pst-angleA-default)))
      ;; ask for angleA
      (setq angleA (completing-read  (concat "angleA : " )
    				 LaTeX-pst-angleA-list
    				 nil nil LaTeX-pst-angleA-default
    				 'LaTeX-pst-angleA-history))
      (LaTeX-pst-add-angleA angleA)
      ;; set new default for angleA
      (setq LaTeX-pst-angleA-default angleA))
    
    
    (defvar LaTeX-pst-angleB-list nil
      "A list of values for angleB in pstricks.")
    
    (defvar LaTeX-pst-angleB-default nil
      "A default value for angleB in pstricks.")
    
    (defvar LaTeX-pst-angleB-history nil
      "History of values for angleB in pstricks.")
    
    (defun LaTeX-pst-add-angleB (angleB)
      "Add angleB in LaTeX-pst-angleB-list if not present"
      ;; add new angleB if not in LaTeX-pst-angleB-list
      (let ((entry (assoc angleB LaTeX-pst-angleB-list)))
        (if (null entry) 
    	(setq LaTeX-pst-angleB-list (cons (list angleB) LaTeX-pst-angleB-list)))))
    
    (defun LaTeX-pst-angleB (&optional arg)
      "Ask for a angleB and manage many angleB's list"
      ;; arg is default value for angleB
      (if (not (null arg))
        (setq LaTeX-pst-angleB-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-angleB-list) 
          (setq LaTeX-pst-angleB-list (list LaTeX-pst-angleB-default)))
      ;; ask for angleB
      (setq angleB (completing-read  (concat "angleB : " )
    				 LaTeX-pst-angleB-list
    				 nil nil LaTeX-pst-angleB-default
    				 'LaTeX-pst-angleB-history))
      (LaTeX-pst-add-angleB angleB)
      ;; set new default for angleB
      (setq LaTeX-pst-angleB-default angleB))
    
    ;;; Radius
    (defvar LaTeX-pst-radius-list nil
      "A list of values for radius in pstricks.")
    
    (defvar LaTeX-pst-radius-default nil
      "A default value for radius in pstricks.")
    
    (defvar LaTeX-pst-radius-history nil
      "History of values for radius in pstricks.")
    
    (defun LaTeX-pst-add-radius (radius)
      "Add radius in LaTeX-pst-radius-list if not present"
      ;; add new radius if not in LaTeX-pst-radius-list
      (let ((entry (assoc radius LaTeX-pst-radius-list)))
        (if (null entry) 
    	(setq LaTeX-pst-radius-list (cons (list radius) LaTeX-pst-radius-list)))))
    
    (defun LaTeX-pst-radius (&optional arg)
      "Ask for a radius and manage many radius's list"
      ;; arg is default value for radius
      (if (not (null arg))
        (setq LaTeX-pst-radius-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-radius-list) 
          (setq LaTeX-pst-radius-list (list LaTeX-pst-radius-default)))
      ;; ask for radius
      (setq radius (completing-read  (concat "Radius : " )
    				 LaTeX-pst-radius-list
    				 nil nil LaTeX-pst-radius-default
    				 'LaTeX-pst-radius-history))
      (LaTeX-pst-add-radius radius)
      ;; set new default for radius
      (setq LaTeX-pst-radius-default radius))
    
    (defvar LaTeX-pst-arrows-list '(("->") ("<-") ("<->") (">-<") (">-") ("-<")
    			      ("<<->>") ("<<-") ("->>") ("|-|") ("|-") ("-|")
    			      ("|*-|*") ("[-]") ("[-") ("-]")
    			      ("(-)") ("(-") ("-)")
    			      ("*-*") ("*-") ("-*")
    			      ("0-0") ("0-") ("-0")
    			      ("c-c") ("c-") ("-c")
    			      ("C-C") ("C-") ("-C")
    			      ("cc-cc") ("cc-") ("-cc")
    			      ("|<->|") ("|<-") ("->|")
    			      ("|<*->|*") ("|<*-") ("->|*")
    			      ("-"))
      "A list of values for arrows in pstricks.")
    
    (defvar LaTeX-pst-arrows-default nil
      "A default value for arrows in pstricks.")
    
    (defvar LaTeX-pst-arrows-history nil
      "History of values for arrows in pstricks.")
    
    (defun LaTeX-pst-add-arrows (arrows)
      "Add arrows in LaTeX-pst-arrows-list if not present"
      ;; add new arrows if not in LaTeX-pst-arrows-list
      (let ((entry (assoc arrows LaTeX-pst-arrows-list)))
        (if (null entry) 
    	(setq LaTeX-pst-arrows-list 
    	      (cons (list arrows) LaTeX-pst-arrows-list)))))
    
    (defun LaTeX-pst-arrows (&optional arg)
      "Ask for a arrows and manage many arrows's list"
      ;; arg is default value for arrows
      (if (not (null arg))
        (setq LaTeX-pst-arrows-default arg)
        (LaTeX-pst-add-arrows arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-arrows-list) 
          (setq LaTeX-pst-arrows-list (list LaTeX-pst-arrows-default)))
      ;; ask for arrows
      (setq arrows (completing-read  (concat "arrows : " )
    				 LaTeX-pst-arrows-list
    				 nil nil LaTeX-pst-arrows-default
    				 'LaTeX-pst-arrows-history))
      (LaTeX-pst-add-arrows arrows)
      ;; set new default for arrows
      (setq LaTeX-pst-arrows-default arrows))
    
    ;;; Coordinates for pspicture environment
    (defvar LaTeX-pst-x0-pspicture-list nil
      "A list of values for x0-pspicture in pstricks.")
    
    (defvar LaTeX-pst-x0-pspicture-default '("0")
      "A default value for x0-pspicture in pstricks.")
    
    (defvar LaTeX-pst-x0-pspicture-history nil
      "History of values for x0-pspicture in pstricks.")
    
    (defun LaTeX-pst-add-x0-pspicture (x0-pspicture)
      "Add x0-pspicture in LaTeX-pst-x0-pspicture-list if not present"
      ;; add new x0-pspicture if not in LaTeX-pst-x0-pspicture-list
      (let ((entry (assoc x0-pspicture LaTeX-pst-x0-pspicture-list)))
        (if (null entry) 
    	(setq LaTeX-pst-x0-pspicture-list 
    	      (cons (list x0-pspicture) LaTeX-pst-x0-pspicture-list)))))
    
    (defun LaTeX-pst-x0-pspicture (&optional arg)
      "Ask for a x0-pspicture and manage many x0-pspicture's list"
      ;; arg is default value for x0-pspicture
      (if (not (null arg))
        (setq LaTeX-pst-x0-pspicture-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-x0-pspicture-list) 
          (setq LaTeX-pst-x0-pspicture-list (list LaTeX-pst-x0-pspicture-default)))
      ;; ask for x0-pspicture
      (setq x0-pspicture (completing-read  (concat "x0-pspicture : " )
    				 LaTeX-pst-x0-pspicture-list
    				 nil nil LaTeX-pst-x0-pspicture-default
    				 'LaTeX-pst-x0-pspicture-history))
      (LaTeX-pst-add-x0-pspicture x0-pspicture)
      ;; set new default for x0-pspicture
      (setq LaTeX-pst-x0-pspicture-default x0-pspicture))
    
    
    (defvar LaTeX-pst-y0-pspicture-list nil
      "A list of values for y0-pspicture in pstricks.")
    
    (defvar LaTeX-pst-y0-pspicture-default '("0")
      "A default value for y0-pspicture in pstricks.")
    
    (defvar LaTeX-pst-y0-pspicture-history nil
      "History of values for y0-pspicture in pstricks.")
    
    (defun LaTeX-pst-add-y0-pspicture (y0-pspicture)
      "Add y0-pspicture in LaTeX-pst-y0-pspicture-list if not present"
      ;; add new y0-pspicture if not in LaTeX-pst-y0-pspicture-list
      (let ((entry (assoc y0-pspicture LaTeX-pst-y0-pspicture-list)))
        (if (null entry) 
    	(setq LaTeX-pst-y0-pspicture-list 
    	      (cons (list y0-pspicture) LaTeX-pst-y0-pspicture-list)))))
    
    (defun LaTeX-pst-y0-pspicture (&optional arg)
      "Ask for a y0-pspicture and manage many y0-pspicture's list"
      ;; arg is default value for y0-pspicture
      (if (not (null arg))
        (setq LaTeX-pst-y0-pspicture-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-y0-pspicture-list) 
          (setq LaTeX-pst-y0-pspicture-list (list LaTeX-pst-y0-pspicture-default)))
      ;; ask for y0-pspicture
      (setq y0-pspicture (completing-read  (concat "y0-pspicture : " )
    				 LaTeX-pst-y0-pspicture-list
    				 nil nil LaTeX-pst-y0-pspicture-default
    				 'LaTeX-pst-y0-pspicture-history))
      (LaTeX-pst-add-y0-pspicture y0-pspicture)
      ;; set new default for y0-pspicture
      (setq LaTeX-pst-y0-pspicture-default y0-pspicture))
    
    
    (defvar LaTeX-pst-x1-pspicture-list nil
      "A list of values for x1-pspicture in pstricks.")
    
    (defvar LaTeX-pst-x1-pspicture-default nil
      "A default value for x1-pspicture in pstricks.")
    
    (defvar LaTeX-pst-x1-pspicture-history nil
      "History of values for x1-pspicture in pstricks.")
    
    (defun LaTeX-pst-add-x1-pspicture (x1-pspicture)
      "Add x1-pspicture in LaTeX-pst-x1-pspicture-list if not present"
      ;; add new x1-pspicture if not in LaTeX-pst-x1-pspicture-list
      (let ((entry (assoc x1-pspicture LaTeX-pst-x1-pspicture-list)))
        (if (null entry) 
    	(setq LaTeX-pst-x1-pspicture-list 
    	      (cons (list x1-pspicture) LaTeX-pst-x1-pspicture-list)))))
    
    (defun LaTeX-pst-x1-pspicture (&optional arg)
      "Ask for a x1-pspicture and manage many x1-pspicture's list"
      ;; arg is default value for x1-pspicture
      (if (not (null arg))
        (setq LaTeX-pst-x1-pspicture-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-x1-pspicture-list) 
          (setq LaTeX-pst-x1-pspicture-list (list LaTeX-pst-x1-pspicture-default)))
      ;; ask for x1-pspicture
      (setq x1-pspicture (completing-read  (concat "x1-pspicture : " )
    				 LaTeX-pst-x1-pspicture-list
    				 nil nil LaTeX-pst-x1-pspicture-default
    				 'LaTeX-pst-x1-pspicture-history))
      (LaTeX-pst-add-x1-pspicture x1-pspicture)
      ;; set new default for x1-pspicture
      (setq LaTeX-pst-x1-pspicture-default x1-pspicture))
    
    
    (defvar LaTeX-pst-y1-pspicture-list nil
      "A list of values for y1-pspicture in pstricks.")
    
    (defvar LaTeX-pst-y1-pspicture-default nil
      "A default value for y1-pspicture in pstricks.")
    
    (defvar LaTeX-pst-y1-pspicture-history nil
      "History of values for y1-pspicture in pstricks.")
    
    (defun LaTeX-pst-add-y1-pspicture (y1-pspicture)
      "Add y1-pspicture in LaTeX-pst-y1-pspicture-list if not present"
      ;; add new y1-pspicture if not in LaTeX-pst-y1-pspicture-list
      (let ((entry (assoc y1-pspicture LaTeX-pst-y1-pspicture-list)))
        (if (null entry) 
    	(setq LaTeX-pst-y1-pspicture-list
    	      (cons (list y1-pspicture) LaTeX-pst-y1-pspicture-list)))))
    
    (defun LaTeX-pst-y1-pspicture (&optional arg)
      "Ask for a y1-pspicture and manage many y1-pspicture's list"
      ;; arg is default value for y1-pspicture
      (if (not (null arg))
        (setq LaTeX-pst-y1-pspicture-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-y1-pspicture-list) 
          (setq LaTeX-pst-y1-pspicture-list (list LaTeX-pst-y1-pspicture-default)))
      ;; ask for y1-pspicture
      (setq y1-pspicture (completing-read  (concat "y1-pspicture : " )
    				 LaTeX-pst-y1-pspicture-list
    				 nil nil LaTeX-pst-y1-pspicture-default
    				 'LaTeX-pst-y1-pspicture-history))
      (LaTeX-pst-add-y1-pspicture y1-pspicture)
      ;; set new default for y1-pspicture
      (setq LaTeX-pst-y1-pspicture-default y1-pspicture))
    
    
    ;;; Algorithme de sélection de paramètres
    ;;;    Le paramètre en cours de saisie est placé dans la variable "parameter"
    ;;; 
    ;;;  Le nom du paramètre peut-être choisi dans une liste.
    ;;;  Les noms et les valeurs des paramètres sont stockées dans deux historiques spécifiques.
    ;;;  Les couples paramètre=valeur sont stockées dans un troisième historique spécifique.
    ;;;
    ;;;  Au moment du choix du paramètre, on peut tout taper à la main,
    ;;;  utiliser l'historique, taper entrée deux fois pour ne pas mettre
    ;;;  de paramètre  ou bien taper entrée pour saisir des couples
    ;;;  nom=valeur à l'aide du complétement pour le nom des paramètres et
    ;;;  bénéficier d'historiques séparés pour les noms et les valeurs.
    
    (defvar LaTeX-pst-parameters-default nil
      "Default parameters in pstricks.")
    
    (defvar LaTeX-pst-parameters-history nil
      "History of values for jpgparam in pstricks.")
    
    ;; Listes pour les valeurs de certains paramètres
    (defvar LaTeX-pst-parameters-value-default nil
      "A default value for parameters' value in pstricks.")
    
    (defvar LaTeX-pst-parameters-value-list nil
      "A list of parameters' values  in pstricks.")
    
    (defvar LaTeX-pst-dotstyle-value-list '(("*") ("o") ("+") ("|")
    				      ("triangle") ("triangle*")
    				      ("square") ("square*")
    				      ("pentagon") ("pentagon*"))
      "A list of values for dotstyle  in pstricks.")
    
    (defvar LaTeX-pst-color-value-list '(("red") ("green") ("blue") 
    				     ("cyan") ("magenta") ("yellow") 
    				     ("black") ("darkgray") ("gray") 
    				     ("lightgray") ("white"))
      "A list of values for *color in pstricks.")
    
    (defvar LaTeX-pst-linestyle-value-list '(("solid") ("none")
    					 ("dashed") ("dotted"))
      "A list of values for linestyle in pstricks.")
    
    (defvar LaTeX-pst-fillstyle-value-list '(("solid") ("none")
    					 ("vlines") ("vlines*")
    					 ("hlines") ("hlines*")
    					 ("crosshatch") ("crosshatch*"))
      "A list of values for fillstyle in pstricks.")
    
    (defvar LaTeX-pst-refpoint-list '(("l") ("r")
    				  ("t") ("tl") ("tr") 
    				  ("b") ("bl") ("br")
    				  ("B") ("Bl") ("Br"))
      "A list of values for refpoint in pstricks.")
    
    (defvar LaTeX-pst-refpoint-history nil
      "History of refpoint's values in pstricks.")
    
    (defvar LaTeX-pst-refpoint-default nil
      "A default value for refpoint value in pstricks.")
    
    (defun LaTeX-pst-add-refpoint (refpoint)
      "Add refpoint in LaTeX-pst-refpoint-list if not present"
      ;; add new refpoint if not in LaTeX-pst-refpoint-list
      (let ((entry (assoc refpoint LaTeX-pst-refpoint-list)))
        (if (null entry) 
    	(setq LaTeX-pst-refpoint-list (cons (list refpoint) LaTeX-pst-refpoint-list)))))
    
    (defun LaTeX-pst-refpoint (&optional arg)
      "Ask for a refpoint and manage many refpoint's list"
      ;; arg is default value for refpoint
      (if (not (null arg))
        (setq LaTeX-pst-refpoint-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-refpoint-list) 
          (setq LaTeX-pst-refpoint-list (list LaTeX-pst-refpoint-default)))
      ;; ask for refpoint
      (setq refpoint (completing-read  (concat "Refpoint : " )
    				 LaTeX-pst-refpoint-list
    				 nil nil LaTeX-pst-refpoint-default
    				 'LaTeX-pst-refpoint-history))
      (LaTeX-pst-add-refpoint refpoint)
      ;; set new default for refpoint
      (setq LaTeX-pst-refpoint-default refpoint))
    
    (defvar LaTeX-pst-stuff-list nil
      "A list of values for stuff in pstricks.")
    
    (defvar LaTeX-pst-stuff-history nil
      "History of stuff's values in pstricks.")
    
    (defvar LaTeX-pst-stuff-default nil
      "A default value for stuff value in pstricks.")
    
    (defun LaTeX-pst-add-stuff (stuff)
      "Add stuff in LaTeX-pst-stuff-list if not present"
      ;; add new stuff if not in LaTeX-pst-stuff-list
      (let ((entry (assoc stuff LaTeX-pst-stuff-list)))
        (if (null entry) 
    	(setq LaTeX-pst-stuff-list (cons (list stuff) LaTeX-pst-stuff-list)))))
    
    (defun LaTeX-pst-stuff (&optional arg)
      "Ask for a stuff and manage many stuff's list"
      ;; arg is default value for stuff
      (if (not (null arg))
        (setq LaTeX-pst-stuff-default arg))
      ;; add default value in list if there is none
      (if (null LaTeX-pst-stuff-list) 
          (setq LaTeX-pst-stuff-list (list LaTeX-pst-stuff-default)))
      ;; ask for stuff
      (setq stuff (completing-read  (concat "Stuff : " )
    				 LaTeX-pst-stuff-list
    				 nil nil LaTeX-pst-stuff-default
    				 'LaTeX-pst-stuff-history))
      (LaTeX-pst-add-stuff stuff)
      ;; set new default for stuff
      (setq LaTeX-pst-stuff-default stuff))
    
    (defvar LaTeX-pst-parameters-value-history nil
      "History of parameters' values in pstricks.")
    
    (defvar LaTeX-pst-parameters-name-default nil
      "A default value for parameters' name in pstricks.")
    
    (defvar LaTeX-pst-parameters-name-list '(("unit") ("xunit") ("yunit") ("runit")
    				       ("linewidth") ("linecolor")
    				       ("showpoints") ("linearc") ("framearc")
    				       ("cornersize") ("arcsepA") ("arcsepB")
    				       ("arcsep") ("curvature")
    				       ("dotstyle") ("dotscale") ("dotangle")
    				       ("gridwidth") ("gridcolor") ("griddots")
    				       ("gridlabels") ("gridlabelcolor")
    				       ("subgriddiv") ("subgridwidth") 
    				       ("subgridcolor") ("subgriddots")
    				       ("plotstyle") ("plotpoints") ("origin")
    				       ("swapaxes") ("linestyle")
    				       ("dash") ("dotsep")
    				       ("border") ("bordercolor")
    				       ("doubleline") ("doublesep") ("doublecolor")
    				       ("shadow") ("shadowsize") 
    				       ("shadowangle") ("shadowcolor")
    				       ("dimen") ("fillstyle") ("fillcolor")
    				       ("hatchwidth") ("hatchsep") 
    				       ("hatchcolor") ("hatchangle")
    				       ("arrows") ("arrowsize") 
    				       ("arrowlength") ("arrowinset")
    				       ("tbarsize")
    				       ("bracketlength") ("rbracketlength")
    				       ("dotsize") ("arrowscale")
    				       ("linetype")
    				       ("liftpen")
    				       ("labelsep")
    				       ("labels")
    				       ("showorigin")
    				       ("ticks")
    				       ("ticksize")
    				       ("tickstyle")
    				       ("axesstyle")
    				       ("framesep")
    				       ("boxsep")
    				       ("nodesep")
    				       ("offset")
    				       ("arm")
    				       ("angle")
    				       ("arcangle")
    				       ("ncurv")
    				       ("loopsize")
    				       ("coilwidth")
    				       ("coilheigth")
    				       ("coilarm")
    				       ("coilaspect")
    				       ("coilinc")
    				       ("gradbegin")
    				       ("gradend")
    				       ("gradlines")
    				       ("gradmidpoint")
    				       ("gradangle")
    				       ("bbllx")
    				       ("bblly")
    				       ("bburx")
    				       ("bbury")
    				       ("headerfile")
    				       ("headers")
    				       ("gangle")
    				       ("trimode")
    				       ("href")
    				       ("vref"))
      "A list of parameters' name in pstricks.")
    
    (defvar LaTeX-pst-parameters-name-history nil
      "History of parameters' name in pstricks.")
    
    
    (defun LaTeX-pst-add-parameters-name (arg)
      "Add ARG in LaTeX-pst-parameters-name-list if not present"
      ;; add new parameters' name if not in LaTeX-pst-parameters-name-list
      (let ((entry (assoc arg LaTeX-pst-parameters-name-list)))
        (if (null entry) 
    	(setq  LaTeX-pst-parameters-name-list (cons (list arg)  LaTeX-pst-parameters-name-list)))))
    
    (defun LaTeX-pst-parameters (&optional arg)
      "Ask for a parameter and manage many param*'s list"
      ;; ask for parameter
      (let  ((paramvalue nil)
    	 (param nil)
    	 (value nil))
        ;; if none is given then ask for a parameter's name
        (if (zerop (length 
    		(setq paramvalue (completing-read 
    				  (concat "Parameter (use history, return for choices, return twice for no parameter)  : " )
    				  LaTeX-pst-parameters-name-list ;; il faudrait peut-être mettre l'historique ?
    				  ;; mais il faudrait formater l'historique pour que ce soit une liste de singletons 
    				  ;; et non une liste de string
    				  nil nil nil
    				  'LaTeX-pst-parameters-history))))
    	;; ask for parameter's names while none is given
       	(while (not (zerop (length
    			    (setq param (completing-read
    					 (concat "Parameter's name (enter to stop) : " )
    					 LaTeX-pst-parameters-name-list
    					 nil nil nil ;;LaTeX-pst-parameters-name-default
    					 'LaTeX-pst-parameters-name-history)))))
    	  (setq LaTeX-pst-parameters-name-default param)
    	  ;; ask for value
    	  ;;;
    	  ;;; TODO
    	  ;;; 
    	  ;;; - it should be easy to add a list of values for a new parameter with customize
    	  ;;; 
    	  
    	  ;; affectation d'un liste de valeurs en fonction du paramètre
    	  ;; programmation sûrement améliorable cat il n'est pas simple
    	  ;; d'ajouter un nouveau cas
    	  ;; idée : une fonction qui renvoie un nom de liste à partir
    	  ;; du nom du paramètre mais comment ?
    	  (cond 
    	   ((string= LaTeX-pst-parameters-name-default "arrows") (setq LaTeX-pst-parameters-value-list LaTeX-pst-arrows-list))
    	   ((string= LaTeX-pst-parameters-name-default "linecolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "fillcolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "bordercolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "hatchcolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "shadowcolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "subgridcolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "gridlabelcolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "gridcolor") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "gradbegin") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "gradend") (setq LaTeX-pst-parameters-value-list LaTeX-pst-color-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "linestyle") (setq LaTeX-pst-parameters-value-list LaTeX-pst-linestyle-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "fillstyle") (setq LaTeX-pst-parameters-value-list LaTeX-pst-fillstyle-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "dotstyle") (setq LaTeX-pst-parameters-value-list LaTeX-pst-dotstyle-value-list))
    	   ((string= LaTeX-pst-parameters-name-default "showpoints") (setq LaTeX-pst-parameters-value-list (list '("true") '("false"))))
    	   (t (setq LaTeX-pst-parameters-value-list nil )))
    	  ;; le cas par défaut devrait être l'historique des valeurs
    	  
    	  (setq value (completing-read  (concat param " = ")
    					LaTeX-pst-parameters-value-list
    					nil nil LaTeX-pst-parameters-value-default
    					'LaTeX-pst-parameters-value-history))
    	  ;; concat param=value with other ones
    	  (setq paramvalue (concat paramvalue 
    				   (if (not (zerop (length paramvalue))) (format ","))
    				   param "=" value))))
        ;; add the last parameter to the history
        (setq  LaTeX-pst-parameters-history 
    	   (cons  paramvalue LaTeX-pst-parameters-history))
        (message paramvalue)))
    
    
    ;;; Macros
    (defun LaTeX-pst-macro-pscircle (&optional arg)
      "Macro to draw circle with pscircle."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(x0 (LaTeX-pst-x0))
            (y0 (LaTeX-pst-y0))
    	(radius (LaTeX-pst-radius)))
        ;; insert \pscircle argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (format "(%s,%s)" x0 y0)
    		    (format "{%s}" radius))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-rput (&optional arg)
      "Macro to place stuff with rput."
      ;; ask for parameters if there is
      (let (
    	(refpoint (LaTeX-pst-refpoint))
    	(rotation (LaTeX-pst-angleA))
     	(x0 (LaTeX-pst-x0))
            (y0 (LaTeX-pst-y0))
    	(stuff (LaTeX-pst-stuff)))
        ;; insert \rput argument's
        (insert (concat (if (not (zerop (length refpoint)))
    			(format "[%s]" refpoint))
    		    (if (not (zerop (length rotation)))
    			(format "{%s}" rotation))
    		    (format "(%s,%s)" x0 y0)
    		    (format "{%s}" stuff))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-qdisk (&optional arg)
      "Macro to draw circle with qdisk."
      ;; ask for parameters if there is
      (let ((x0 (LaTeX-pst-x0))
            (y0 (LaTeX-pst-y0))
    	(radius (LaTeX-pst-radius)))
        ;; insert \qdisk argument's
        (insert (concat (format "(%s,%s)" x0 y0)
    		    (format "{%s}" radius))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-psline (&optional arg)
      "Macro to draw line with psline, pscurve,psccurve or psecurve."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1))
    	(LaTeX-pst-x_i-list nil)
    	(LaTeX-pst-y_i-list nil)
    	(LaTeX-pst-x_i nil)
    	(LaTeX-pst-y_i nil))
        ;; on fait deux listes LaTeX-pst-x_i-list et LaTeX-pst-y_i-list avec les autres coordonnées LaTeX-pst-x_i et LaTeX-pst-y_i
        ;; en utilisant les listes liées à x1 et y1. 
        ;; Faut-il des listes spécifiques ?
        (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    	(while 
    	    (progn 
    	      (setq LaTeX-pst-x_i (completing-read  (concat  "x_i : ")
    						    LaTeX-pst-x1-list
    						    nil nil LaTeX-pst-x1-default
    						    'LaTeX-pst-x1-history))
    	      (setq LaTeX-pst-x_i-list (cons LaTeX-pst-x_i LaTeX-pst-x_i-list))
    	      (setq LaTeX-pst-y_i (completing-read  (concat  "y_i : ")
    						    LaTeX-pst-y1-list
    						    nil nil LaTeX-pst-x1-default
    						    'LaTeX-pst-y1-history))
    	      (setq LaTeX-pst-y_i-list (cons LaTeX-pst-y_i LaTeX-pst-y_i-list))
    	      (not (and (zerop (length LaTeX-pst-x_i)) (zerop (length LaTeX-pst-y_i)))))))
        
        ;; insert \psline argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (if (not (zerop (length arrows)))
    			(format "{%s}" arrows))
    		    (format "(%s,%s)" x0 y0)
    		    ;; if x1 or y1 = 0  then it's like \psline(0,0)(x0,y0) (cf. pstricks doc)
    		    (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    			(format "(%s,%s)" x1 y1))))
        
        ;; LaTeX-pst-x_i-list et LaTeX-pst-y_i-list commencent par "" donc on l'enlève
        (setq LaTeX-pst-x_i-list (cdr LaTeX-pst-x_i-list))
        (setq LaTeX-pst-y_i-list (cdr LaTeX-pst-y_i-list))
        (while (not (and (zerop (length LaTeX-pst-x_i-list)) (zerop (length LaTeX-pst-y_i-list))))
          (insert (format "(%s,%s)" (car LaTeX-pst-x_i-list) (car LaTeX-pst-y_i-list)))
          (setq LaTeX-pst-x_i-list (cdr LaTeX-pst-x_i-list))
          (setq LaTeX-pst-y_i-list (cdr LaTeX-pst-y_i-list))))		    
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-psdots (&optional arg)
      "Macro to put dots at each ccordinate."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1)))
        ;; insert \psdots argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (if (not (zerop (length arrows)))
    			(format "{%s}" arrows))
    		    (format "(%s,%s)" x0 y0)
    		    (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    			(format "(%s,%s)" x1 y1)))))
      (newline-and-indent))
    
    
    (defun LaTeX-pst-macro-parabola (&optional arg)
      "Macro to draw a parabola with parabola (pstricks)."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1)))
        ;; insert \parabola argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (if (not (zerop (length arrows)))
    			(format "{%s}" arrows))
    		    (format "(%s,%s)" x0 y0)
    		    (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    			(format "(%s,%s)" x1 y1)))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-psellipse (&optional arg)
      "Macro to draw ellipse with psellipse."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1)))
        ;; insert \psellipse argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (if (not (zerop (length arrows)))
    			(format "{%s}" arrows))
    		    (format "(%s,%s)" x0 y0)
    		    (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    			(format "(%s,%s)" x1 y1)))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-psbezier (&optional arg)
      "Macro to draw bezier curve with psbezier."
      ;; ****************
      ;; TODO
      ;;
      ;; Ne prend pas bien en charge cette macro !
      ;; Il faut gérer 4 couples de coordonnées qui
      ;; ne sont pas de même nature.
      ;; 
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1)))
        ;; insert \psbezier argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (if (not (zerop (length arrows)))
    			(format "{%s}" arrows))
    		    (format "(%s,%s)" x0 y0)
    		    (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    			(format "(%s,%s)" x1 y1)))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-pspolygon (&optional arg)
      "Macro to draw polygon with pspolygon."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1))
    	(LaTeX-pst-x_i-list nil)
    	(LaTeX-pst-y_i-list nil)
    	(LaTeX-pst-x_i nil)
    	(LaTeX-pst-y_i nil))
        ;; on fait deux listes LaTeX-pst-x_i-list et LaTeX-pst-y_i-list avec les autres coordonnées LaTeX-pst-x_i et LaTeX-pst-y_i
        ;; en utilisant les listes liées à x1 et y1. 
        ;; Faut-il des listes spécifiques ?
        (if (not (and (zerop (length x1)) (zerop (length y1)))) 
    	(while 
    	    (progn 
    	      (setq LaTeX-pst-x_i (completing-read  (concat  "x_i : ")
    						    LaTeX-pst-x1-list
    						    nil nil LaTeX-pst-x1-default
    						    'LaTeX-pst-x1-history))
    	      (setq LaTeX-pst-x_i-list (cons LaTeX-pst-x_i LaTeX-pst-x_i-list))
    	      (setq LaTeX-pst-y_i (completing-read  (concat  "y_i : ")
    						    LaTeX-pst-y1-list
    						    nil nil LaTeX-pst-x1-default
    						    'LaTeX-pst-y1-history))
    	      (setq LaTeX-pst-y_i-list (cons LaTeX-pst-y_i LaTeX-pst-y_i-list))
    	      (not (and (zerop (length LaTeX-pst-x_i)) (zerop (length LaTeX-pst-y_i)))))))
        
        ;; insert \pspolygon argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (format "(%s,%s)" x0 y0)
    		    (format "(%s,%s)" x1 y1)))
        ;; LaTeX-pst-x_i-list et LaTeX-pst-y_i-list commencent par "" donc on l'enlève
        (setq LaTeX-pst-x_i-list (cdr LaTeX-pst-x_i-list))
        (setq LaTeX-pst-y_i-list (cdr LaTeX-pst-y_i-list))
        (while (not (and (zerop (length LaTeX-pst-x_i-list)) (zerop (length LaTeX-pst-y_i-list))))
          (insert (format "(%s,%s)" (car LaTeX-pst-x_i-list) (car LaTeX-pst-y_i-list)))
          (setq LaTeX-pst-x_i-list (cdr LaTeX-pst-x_i-list))
          (setq LaTeX-pst-y_i-list (cdr LaTeX-pst-y_i-list))))		    
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-qline (&optional arg)
      "Macro to draw line with qline."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1)))
        ;; insert \qline argument's
        (insert (concat (format "(%s,%s)" x0 y0)
    		    (format "(%s,%s)" x1 y1))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-psframe (&optional arg)
      "Macro to draw rectangle with psframe."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(x0 (LaTeX-pst-x0))
    	(y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1)))
        ;; insert \psframe argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (format "(%s,%s)" x0 y0)
    		    (format "(%s,%s)" x1 y1))))
      (newline-and-indent))
    
    (defun LaTeX-pst-macro-psset (&optional arg)
      "Macro to draw circle with psset."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters)))
        ;; insert \psset argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "{%s}" parameters)))))
      (newline-and-indent))
    
    
    (defun LaTeX-pst-macro-psgrid (&optional arg)
      "Macro to draw  psgrid."
      ;; ask for parameters if there is
      (let ((parameters (LaTeX-pst-parameters))
    	(arrows (LaTeX-pst-arrows))
    	(x0 (LaTeX-pst-x0))
            (y0 (LaTeX-pst-y0))
    	(x1 (LaTeX-pst-x1))
    	(y1 (LaTeX-pst-y1))
    	(x2 (LaTeX-pst-x2))
    	(y2 (LaTeX-pst-y2)))
        ;; insert \psgrid argument's
        (insert (concat (if (not (zerop (length parameters)))
    			(format "[%s]" parameters))
    		    (format "(%s,%s)" x0 y0)
    		    (format "(%s,%s)" x1 y1)
    		    (if (not (and (zerop (length x2)) (zerop (length y2))))
    			(format "(%s,%s)" x2 y2)))))
      (newline-and-indent))
    
    
    ;;; Environments
    (defun LaTeX-env-pspicture (&optional ignore)
      "Create new pspicure environment."
      ;; insertion de l'environnement
      (LaTeX-insert-environment "pspicture")
      ;; fin du begin{pspicture} pour écrire les coordonnées
      (end-of-line 0)
      ;; définitions des coordonnées de l'image
      (let ((x0 (LaTeX-pst-x0-pspicture))
    	(y0 (LaTeX-pst-y0-pspicture))
    	(x1 (LaTeX-pst-x1-pspicture))
    	(y1 (LaTeX-pst-y1-pspicture)))
        ;; insertion des coordonnées de l'image
        (insert (concat (format "(%s,%s)" x0 y0)
    		    (format "(%s,%s)" x1 y1))))
      (next-line 1))
    
    
    (TeX-add-style-hook "pstricks"
    		    (function 
    		     (lambda ()
    		       (LaTeX-add-environments 
    			"overlaybox"
    			'("pspicture" LaTeX-env-pspicture)
    			"psclip")
    		       (TeX-add-symbols
    			"AltClipMode"
    			"AltOverlayMode"
    			"Cartesian"
    			"Cput"
    			"DontKillGlue"
    			"KillGlue"
    			"NewPsput"
    			"NormalCoor"
    			"OldPsput"
    			"PSTricksLoaded"
    			"PSTricksOff"
    			"Polar"
    			"Rput"
    			"SpecialCoor"
    			"altcolormode"
    			"arrows"
    			"clipbox"
    			"closedshadow"
    			"closepath"
    			"code"
    			"coor"
    			"cput"
    			"curveto"
    			"degrees"
    			"dim"
    			"endpsclip"
    			"file"
    			"fill"
    			"grestore"
    			"gsave"
    			"lineto"
    			"movepath"
    			"moveto"
    			"mrestore"
    			"msave"
    			"multips"
    			"multirput"
    			"newcmykcolor"
    			"newgray"
    			"newhsbcolor"
    			"newpath"
    			"newpsfontdot"
    			"newpsfontdotH"
    			"newpsobject"
    			"newpsstyle"
    			"newrgbcolor"
    			"oldpsput"
    			"openshadow"
    			'("parabola" LaTeX-pst-macro-parabola) 
    			'("parabola*" LaTeX-pst-macro-parabola) 
    			"psCircle"
    			"psaddtolength"
    			"psarc"
    			"psarcn"
    			'("psbezier" LaTeX-pst-macro-psbezier) 
    			'("psbezier*" LaTeX-pst-macro-psbezier) 
    			"pscbezier"
    			'("pscircle" LaTeX-pst-macro-pscircle) 
    			'("psccurve" LaTeX-pst-macro-psline) 
    			'("psccurve*" LaTeX-pst-macro-psline) 
    			"pscirclebox"
    			'("pscurve*" LaTeX-pst-macro-psline) 
    			"pscustom"
    			"psdblframebox"
    			"psdiabox"
    			"psdiamond"
    			"psdot"
    			'("psdots" LaTeX-pst-macro-psdots) 
    			"psdoublesep"
    			'("psecurve" LaTeX-pst-macro-psline) 
    			'("psecurve*" LaTeX-pst-macro-psline) 
    			'("psellipse" LaTeX-pst-macro-psellipse) 
    			'("psellipse*" LaTeX-pst-macro-psellipse) 
    			"psfillcolor"
    			'("psframe" LaTeX-pst-macro-psframe) 
    			'("psframe*" LaTeX-pst-macro-psframe) 
    			"psframebox"
    			"psframesep"
    			'("psgrid" LaTeX-pst-macro-psgrid) 
    			"pslabelsep"
    			'("psline" LaTeX-pst-macro-psline) 
    			"pslinearc"
    			"pslinecolor"
    			"pslinestyle"
    			"pslinetype"
    			"pslinewidth"
    			"pslongbox"
    			"psovalbox"
    			"psoverlay"
    			'("pspolygon" LaTeX-pst-macro-pspolygon) 
    			'("pspolygon*" LaTeX-pst-macro-pspolygon) 
    			"psput"
    			"psrunit"
    			'("psset" LaTeX-pst-macro-psset)
    			"pssetlength"
    			"pssetxlength"
    			"pssetylength"
    			"psshadowbox"
    			"pstVerb"
    			"pstVerb"
    			"pstcustomize"
    			"pstdriver"
    			"pstheader"
    			"pstriangle"
    			"pstribox"
    			"pstrotate"
    			"pstverb"
    			"pstverbscale"
    			"psunit"
    			"psverbboxfalse"
    			"psverbboxtrue"
    			"pswedge"
    			"psxunit"
    			"psyunit"
    			"putoverlaybox"
    			'("qdisk" LaTeX-pst-macro-qdisk)
    			'("qline" LaTeX-pst-macro-qline)
    			"radians"
    			"rcoor"
    			"rcurveto"
    			"rlineto"
    			"rotate"
    			"rotatedown"
    			"rotateleft"
    			"rotateright"
    			'("rput" LaTeX-pst-macro-rput)
    			'("rput*" LaTeX-pst-macro-rput)
    			"scale"
    			"scalebox"
    			"scaleboxto"
    			"setcolor"
    			"showpointstrue"
    			"stroke"
    			"swapaxes"
    			"translate"
    			"typeout"
    			"unitlength"
    			"uput" )
    		       (TeX-run-style-hooks
    			"pstricks"))))
    
    ;;****************************************************************
    ;;  valeurs par défaut de variables 
    ;; ****************************************************************
    
    (defun LaTeX-pst-current-macro (&optional arg)
      "test du C-u pour relire les arguments d'une macro ou d'un environnement
    La suite vient de latex.el
    
    If ARG is nil or missing, use the current level.
    If ARG is a list (selected by C-u), go downward one level.
    If ARG is negative, go up that many levels.
    If ARG is positive or zero, use absolute level:
    
      0 : part
      1 : chapter
      2 : section
      3 : subsection
      4 : subsubsection
      5 : paragraph
      6 : subparagraph "
    ;  (save-excursion 
        ;; chercher le nom de la macro
        (re-search-backward "\\\\[a-zA-Z]*")
        (let* ((macro-name (substring (match-string-no-properties 0) 1 (length (match-string-no-properties 0)))))
    	    (message macro-name)
    	    (setq debut (point-marker))
    	    ;; on suppose que la macro se termine à la fin par un " ".
    	    ;;; Ne marche pas : prend toute la ligne ?
    	    (search-forward-regexp "[\$\|\" \"]")
    ;;	    (search-forward-regexp "\\\$")
    	    (setq fin (+ 1 (point-marker)))
    	    ;; aller au début de la macro
    	    (goto-char debut)
    	    (delete-char 1) ;;; DOIT ETRE ENLEVE QUAND LES SEARCH-REPLACE fonctionneront correctement
    	    ;; remplacer la macro par "argh"
    	    ;; reste à analyser la chaîne pour récupérer les arguments et
    	    ;; ensuite à lancer la recréation de la macro
    	    (perform-replace ".*" "argh" debut fin t nil nil)
    	))
    
    
    ; extrait de latex.el : comment utiliser C-u et ARG
    ;   (interactive "*P")
    ;   (let* ((macro-name (prefix-numeric-value arg))
    ; 	 (level (cond ((null arg)
    ; 		       (LaTeX-current-section))
    ; 		      ((listp arg)
    ; 		       (LaTeX-down-section))
    ; 		      ((< val 0)
    ; 		       (LaTeX-up-section (- val)))
    ; 		      (t val)))
    ; 	 (name (LaTeX-section-name level))
    ; 	 (toc nil)
    ; 	 (title "")
    ; 	 (done-mark (make-marker)))
    ;     (newline)
    ;     (run-hooks 'LaTeX-section-hook)
    ;     (newline)
    ;     (if (marker-position done-mark)
    ; 	(goto-char (marker-position done-mark)))
    ;     (set-marker done-mark nil)))
    
    
    ;; ****************************************************************

