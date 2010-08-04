;;; ac-lilypond.el --- Set up the use of LilyPond with auto-complete

;; Copyright (C) 2009  Shelagh Manton <shelagh.manton@gmail.com>

;; Author: Shelagh Manton <shelagh.manton@gmail.com>
;; Keywords: LilyPond auto-completion convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:
;; A file to set up the use of LilyPond with auto-complete which can be found
;; at http://www.cx4a.org/pub/auto-complete.el

;; Lilypond has functions to extract keywords etc from the lilypond-words.el file which in
;; turn is extracted by a python script from the code itself. I wish it had a list of the
;; lilypond scheme functions too.

;; Just add
;; (eval-after-load "LilyPond-mode" (load-library "ac-lilypond))
;; to your .emacs file. Happy auto-completing!

;;; bugs: 
;; need to look at why the words beginning with \ do not show up. Needs to be part of
;; lilypond syntax table?
;; another thing which might be nice is to have is a hook in with auto-overlays package so that
;; the code completion is context sensitive. the regexps already exsist in lilypond code.

;;; Code:

(require 'auto-complete)
 
;(modify-syntax-entry ?\\ "w" LilyPond-mode-syntax-table)
; A failed experiment.

(defvar ac-lilypond-identifiers
  '((candidates
     . (lambda ()
	 (all-completions ac-target LilyPond-identifiers)))))

;; I need to do a mapcar on the list of keywords to put an extra \\ in front of the \\ that is there already.
;; This will probably done if any LaTeX is used for auto-complete.

(defvar ni-LilyPond-keywords
  (mapcar (lambda (x) (concat "\\" x)) LilyPond-keywords))

(defvar ac-lilypond-keywords
  '((candidates
     . (lambda ()
	 (all-completions ac-target ni-LilyPond-keywords)))))
 
(defvar ac-lilypond-Creserved-words
 '((candidates
     . (lambda ()
	 (all-completions ac-target LilyPond-Capitalized-Reserved-Words)))))

 (defvar ac-lilypond-ncreserved-words
   '((candidates
      . (lambda ()
	  (all-completions ac-target LilyPond-non-capitalized-reserved-words)))))

(add-hook 'LilyPond-mode-hook
	  (lambda () "Makes autocomplete work in LilyPond mode (mostly)"
	    (make-local-variable 'ac-sources)
	    (setq ac-sources '(ac-lilypond-ncreserved-words
			       ac-lilypond-Creserved-words
			       ac-lilypond-keywords 
			       ac-lilypond-identifiers ;these don't work. why?
			       ac-source-abbrev
			       ac-source-words-in-buffer))
	    (auto-complete-mode 1)))


(provide 'ac-lilypond)
;;; ac-lilypond.el ends here
