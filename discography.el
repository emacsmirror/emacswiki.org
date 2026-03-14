;;; discography.el * Major mode for editing discographies using a variation of BibTeX.

;; note: this file was extracted from mms.tgz, found here: https://web.archive.org/web/20060409062508/http://meltin.net/hacks/emacs/
;; It does not include any copyright or authorship info.  Based on context, I've added the likely data, but please let me know if authorship or dating is incorrect. [2026-3-14 Cena]

;; author: Martin Schwenke
;; copyright: 2000

;; code:

;; Put "-*- mode: discography -*-" at the top of a BibTeX-like file
;; and this stuff will be used.
;;

(defvar discography-mode-user-optional-fields nil
"*List of optional fields that user want to have as always present
when making a discography entry.  See bibtex-mode-user-optional-fields
for more information.")

(defun discography-mode ()
"Major mode for editing discographies.  Discography files use a
variation of BibTeX, and this mode is a layer on top of bibtex-mode.
The entry definitions have been changed to reflect information about
compact discs.  See documentation for bibtex-mode for more
information."

  (interactive)
  (bibtex-mode)
  (make-local-variable 'bibtex-entry-field-alist)
  (setq bibtex-entry-field-alist
	;; Artist and Band are the same here, but are treated
	;; differently by the software.
	'(
	  ("Artist" . (((("name" "Name of artist")
			 ("title" "Title of CD"))
			(("company" "Recording company")
			 ("label" "Recording label")
			 ("number" "CD number")
			 ("price" "Retail price of CD")
			 ("year" "Year of publication")))))
	  ("Band" . (((("name" "Name of band")
		       ("title" "Title of CD"))
			(("company" "Recording company")
			 ("label" "Recording label")
			 ("number" "CD number")
			 ("price" "Retail price of CD")
			 ("year" "Year of publication")))))
	  ("Composer" . (((("name" "Name of composer")
			   ("title" "Title of CD"))
			  (("conductor" "Conductor or individual musician")
			    ("company" "Recording company")
			    ("label" "Recording label")
			    ("number" "CD number")
			    ("price" "Retail price of CD")
			    ("year" "Year of publication")))))))
  (make-local-variable 'bibtex-include-OPTcrossref)
  (setq bibtex-include-OPTcrossref nil)
  (make-local-variable 'bibtex-mode-user-optional-fields)
  (setq bibtex-mode-user-optional-fields
	discography-mode-user-optional-fields)
  (setq major-mode 'discography-mode)
  (setq mode-name "Discography")

  (run-hooks 'discography-mode-hook))

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     'discography-mode
     (cdr (cdr (assoc 'bibtex-mode hilit-patterns-alist)))))

(provide 'discography)
