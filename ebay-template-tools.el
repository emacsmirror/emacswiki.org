;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; no-byte-compile: t; -*-
;;; this is ebay-template-tools.el
;;; ================================================================
;;; DESCRIPTION:
;;; ebay-template-tools 
;;; Stripped down version of MON auction related utils for working w/
;;; ebay-template-mode.el
;;;
;;; FUNCTIONS:►►►
;;;
;;; FUNCTIONS:◄◄◄
;;;
;;; CONSTANTS or VARIABLES:
;;;
;;; MACROS:
;;;
;;; SUBST or ALIASES:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; Following path-vars moved to -> "../mon-dir-locals-alist.el"
;;; <Timestamp: Tuesday June 16, 2009 @ 04:21.18 PM - by MON KEY>
;;; `*nef-scan-path*'           -> ../mon-dir-locals-alist.el
;;; `*nef-scan-nefs-path*'      -> ../mon-dir-locals-alist.el
;;; `*nef-scan-nef2-path*'      -> ../mon-dir-locals-alist.el
;;; `*ebay-images-path*'        -> ../mon-dir-locals-alist.el
;;; `*ebay-images-bmp-path*'    -> ../mon-dir-locals-alist.el
;;; `*ebay-images-jpg-path*'    -> ../mon-dir-locals-alist.el
;;; `*ebay-images-temp-path*'   -> ../mon-dir-locals-alist.el
;;;
;;; REQUIRES:
;;; 'cl -> xml-gen
;;; 'mon-insertion-utils
;;; 'mon-dir-utils
;;; 'mon-dir-locals-alist
;;; 'naf-mode-replacements
;;; 'naf-url-utils
;;; 'mon-css-color
;;; 'mon-time-utils
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;; 
;;;
;;; THIRD PARTY CODE:
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; FILE-CREATED:
;;; <Timestamp: Tuesday May 05, 2009 @ 06:38.57 PM - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; ©opyright (C) MON KEY - 2009
;;; ============================
;;; CODE:

(require 'xml-gen)
(require 'html-lite)

;;; ==============================
(defun mon-insert-ebay-template ()
  "BUILD YOUR HTML HERE OR USE A HANDMADE TEMPLATE."
  (let ((make-html 
	 (html-lite-write-tree
	  ;;(html-link :rel "stylesheet" :type "text/css" :media "screen" :href "styles/ebay/ebay-template-N.css")
	  (html-table :border "0"  :cellpadding "0"   :cellspacing "0"  :width "100%"
		      (html-tbody (html-tr :valign "top" 
					   (html-td :bgcolor "#2c7700" :width "100%" ;;; bgcolor needs var
						    ;; Initial Table
						    (html-table :border "0" :cellpadding "5" :cellspacing "5" :width"100%"
								(html-tbody 
								 (html-tr :valign "middle"
									  (html-a :name "up" "")))))))))))
    make-html))

;;; ==============================
;;; TODO: take an optional STARTING-DIR arg to default-dir
;;; (file-relative-name buffer-file-name *nef-scan-path*)
;;; (expand-file-name "Nef_Drive2/EBAY/BMP-Scans/e1193/e1193.dbc" *nef-scan-path*)
;;; CREATED: <Timestamp: Monday April 20, 2009 @ 04:31.54 PM - by MON KEY>
;;; ==============================
(defvar *insert-ebay-template* 'nil
"Used in `mon-insert-ebay-dirs', `mon-insert-ebay-bmps-in-file'.
See also; `mon-insert-ebay-photo-per-scan-descr', `*insert-ebay-template*'.")
;;
(unless *insert-ebay-template*
  (let ((ebay-template
'";; -*- mode: EBAY-TEMPLATE; -*-
;;; =====================================
;;; Copyright ©2009 - <YOUR-NAME>
;;; =====================================
;;; this file is:
;;; %s
;;; ==============================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~¦
ebay-item-title:
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~¦
ebay-item-number:
ebay-item-start-date:
ebay-item-end-date:
ebay-item-listing-duration:
ebay-item-start-price:
ebay-item-reserve:
ebay-item-listing-fee:
ebay-item-buy-it-now-fee:
ebay-item-paypal-fee:
ebay-item-shipping-charged:
ebay-item-shipping-weight:
ebay-item-ship-to:
ebay-item-high-bidder-id:
ebay-item-times-listed:
ebay-item-page-views:
ebay-item-watchlist-count:
ebay-item-offers:
ebay-item-listed-in-category:
ebay-item-notes:
;;; ==============================\n
<!-- html-template-starts-here -->\n\n%s\n\n<!-- html-template-ends-here -->\n
;;; URLs inserted below for photo:
;;; %s-N\n;;; thru;\n;;; %s-N\n
;;; To get ebay photo paths evaluate-me with either:
;;; M-x mon-insert-ebay-bmps-in-file
;;; (mon-insert-ebay-bmps-in-file) <-- C-x C-e\n
;;; M-x mon-insert-ebay-jpgs-in-file
;;; (mon-insert-ebay-jpgs-in-file) <-- C-x C-e\n
;;; To get an eBay photo description inserted:
;;; M-x mon-insert-ebay-photo-per-scan-descr
;;; (mon-insert-ebay-photo-per-scan-descr COUNT ITEM-NUMBER\) <-- C-x C-e\n
;;; e.g.(mon-insert-ebay-photo-per-scan-descr 3 '1143) <-- C-x C-e\n
;;; ==============================
;;; %s.dbc ends here
;;; ==============================
;;; EOF"))
    (setq *insert-ebay-template* ebay-template)))


;;;test-me;(symbol-value '*insert-ebay-template*)
;;; (progn (makunbound '*insert-ebay-template*) (unintern '*insert-ebay-template*))

;;;;;;;
;(format *insert-ebay-template* put-file make-html file-no-ext file-no-ext file-no-ext)))
;; (format *insert-ebay-template* 
;;         put-file ;this file is:\n%s 
;;         make-html ;<!-- html-template-starts-here
;;         file-no-ext ;URLs inserted below for photo:
;;         file-no-ext ;URLs inserted below for photo:
;;         file-no-d ;%s.dbc ends here
;;         )


;;; ==============================
;;; TODO: both `mon-choose-ebay-delims' and `mon-choose-ebay-accounts'
;;; should do completion ala ido, auto-complete, imenu etc.
;;; CREATED: <Timestamp: Thursday May 14, 2009 @ 12:57.52 PM - by MON KEY>
;;; ==============================
(defun mon-choose-ebay-delims ()
"Choose from a list of delims to use for the ebay template.
Choices include:
 ◊◊◊◊◊ ♦♦♦♦♦ ΠΠΠΠΠΠ ΦΦΦΦΦ πππππ §§§§§ ≡≡≡≡≡ ˆˆˆˆˆ ΞΞΞΞΞ ººººº
 °°°°° ΔΔΔΔΔ ±±±±± ••••• ····· …………… ∞∞∞∞∞ ¤¤¤¤¤ ‹‹‹››› ««»»»
 ←———→ ××××× ˜˜˜˜˜ -----\n
See also; `mon-choose-ebay-account'"
  (let* ((delim-choice '("◊◊◊◊◊" "♦♦♦♦♦" "ΠΠΠΠΠΠ" "ΦΦΦΦΦ" "πππππ" "§§§§§" "≡≡≡≡≡" "ˆˆˆˆˆ" "ΞΞΞΞΞ" "ººººº"
       		       "°°°°°" "ΔΔΔΔΔ" "±±±±±" "•••••" "·····" "……………" "∞∞∞∞∞" "¤¤¤¤¤" "‹‹‹›››" "««»»»"
       		       "←———→" "×××××" "˜˜˜˜˜" "-----"))
	 (chosen (completing-read "Which delim shall we use :" delim-choice nil t "◊◊◊◊◊")))
    chosen))

;;;test-me;(mon-choose-ebay-delims)


;;; ==============================
;;; TODO: This should do completion ala ido, auto-complete, imenu etc.
(defun mon-choose-ebay-account ()
  "Choose from a list of ebay-accounts to insert for the ebay template.
Account choices include: <YOUR ACCOUNT NAME HERE>.
See also; `mon-choose-ebay-delims'"
  (let* ((accnt-choice '("<YOUR ACCOUNT NAME HERE>"))
	 (chosen (completing-read "Which ebay account are we using :" accnt-choice nil t "<YOUR ACCOUNT NAME HERE>")))
    chosen))
	     
;;;test-me;(mon-choose-ebay-account)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:14.59 PM - by MON KEY>
(defun mon-make-html-tree ()
  "Make an html tree for insertion but don't write to buffer."
  (interactive)
  (let ((html-temp)
	(html-string))
    (setq html-string
	  (with-temp-buffer 
	    (mon-insert-ebay-template)
	    (setq html-temp (buffer-string))))
    ;;(message "%s" html-string)))
    html-string))

;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:15.08 PM - by MON KEY>
(defun mon-insert-ebay-html-tree ()
  "Inserts _only_ the ebay-html-tree in `current-buffer' at point.
Use: `mon-insert-ebay-dbc-template' for the full ebay template insertion utility.
This may be deprecated once the additional templating call-outs are incorporated."
  (interactive)
  (format "%s" (mon-insert-ebay-template)))

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 09, 2009 @ 05:03.17 PM - by MON KEY>
(defun mon-make-ebay-dir-list (start-from end-at &optional ff-prefix)
  "Build a numeric list of dir names to use for inserting in a directory.
START-FROM \(a number\) to END-AT \(a number\).  Optional arg FF-PREFIX \(a string\)
prefixes list with string default prfx is `e'.
Helper function for `mon-insert-ebay-dirs'.\nSee also; `*ebay-images-temp-path*'."
  (let ((put-dirs)
	(starting)
	(prfx (cond ((stringp ff-prefix) ff-prefix)
		    ((or (not ff-prefix) (not (stringp ff-prefix)))
			  "e"))))
    (setq starting start-from)
    (setq put-dirs ())
    (while (not (= starting (1+ end-at)))
      (setq put-dirs (cons (format "%s%d" prfx starting) put-dirs))
      (setq starting (1+ starting)))
    (setq put-dirs  (nreverse  put-dirs))
   put-dirs))

;;; ==============================
(defun mon-make-ebay-dir-list-2 (start-from end-at &optional ff-prefix ff-suffix ff-incr)
  "Build a numeric list of dir names to use for inserting in a directory.
START-FROM (a number) to END-AT (a number).  Optional arg FF-PREFIX (a string)
prefixes list with string default prfx is `e'.
Helper function for `mon-insert-ebay-dirs'
See also; `*ebay-images-temp-path*'."
  (let* ((put-dirs)
	 (starting)
	 (prfx (cond ((stringp ff-prefix) 
		      (let ((fix-pre ff-prefix))
			(setq fix-pre 
			      (replace-regexp-in-string "\\([[:space:]_]\\)" "-" fix-pre))
			(setq fix-pre 
			      (replace-regexp-in-string "\\(--\\)" "-" fix-pre))
			(setq fix-pre 
			      (replace-regexp-in-string "\\(\\([[:alnum:]]\\)\\([^-]$\\)\\)" "\\2\\3-" fix-pre nil nil))				 fix-pre))
		     ((or (not ff-prefix) (not (stringp ff-prefix)))
		      "e")))
	 (sffx (cond ((stringp ff-suffix) 
		      (let ((fix-sfx ff-suffix))
			(setq fix-sfx 
			      (replace-regexp-in-string "\\([[:space:]_]\\)" "-" fix-sfx))
			(setq fix-sfx 
			      (replace-regexp-in-string "\\(--\\)" "-" fix-sfx))
			(setq fix-sfx 
			      (replace-regexp-in-string "\\(\\([[:alnum:]]\\)\\([^-]$\\)\\)" "\\2\\3-" fix-sfx nil nil))				 fix-sfx))
		     ((numberp ff-suffix) (format "-%d-" ff-suffix)))))
    ;;(if numberp sffx)
    ;;	(fincr (cond ((numberp ff-incr) ff-incr)
    ;; 	    ((or (not ff-prefix) (not (stringp ff-prefix)))
    ;; 		  "e")))
	 
					;`(,prfx ,sffx)))
					;(mon-make-ebay-dir-list 1 3 "Big Bubba " 3)
    (setq starting start-from)
    (setq put-dirs ())
    (while (not (= starting (1+ end-at)))
      (setq put-dirs (cons (format "%s%d" prfx starting) put-dirs))
      (setq starting (1+ starting)))
    (setq put-dirs  (nreverse  put-dirs))
    put-dirs))

;;;(mon-make-ebay-dir-list 1 3 "Big Bubba " 3)
;;; ==============================
;; (interactive
;;   (list (let ((starting (read-number "Ebay Item no. to start from :"))
;; 	       (ending (read-number "Ebay Item no. to start from :"))
;; 	       (alt-prfx (cond ((yes-or-no-p "Use an alt dir prefix (default is 'e') : ")
;; 			       (let (str)
;; 				 (setq str (read-string "Prefix files and folders with: "))
;; 				 (setq str (replace-regexp-in-string "\\([[:space:]_]\\)" "-" str))
;; 				 (setq str (replace-regexp-in-string "\\(--\\)" "-" str))
;; 				 (setq str (replace-regexp-in-string "\\(\\([[:alnum:]]\\)\\([^-]$\\)\\)" "\\2\\3-" str nil nil))
;; 				 str)))))
;; 	   (mon-make-ebay-dir-list starting ending alt-prfx))


;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:16.06 PM - by MON KEY>
(defun mon-insert-ebay-dirs (dir-list)
  "Inserts ebay photo dirs in `*ebay-images-temp-path*'.
Builds dirs from elts in DIR-LIST in ../BMP-Scans/ and ../BIG-cropped-jpg.
DIR-LIST should be a list of strings e.g. incrementing from last #.
\(setq my-make-dir-list '(\"eNNNN\" \"eNNNN\" \"eNNNN\" \"eNNNN\" \"eNNNN\"))\n
Each newly created dir in ../BMP-Scans/ has a file temlate eNNNN.dbc inserted
to aid in tracking listing developments.\n
Used with: `mon-make-ebay-dir-list' interactive list builder.
And,  `mon-insert-ebay-dbc-template' to insert a buffer local template.
Path determined according to globar var `*ebay-images-temp-path*'.
See also; `*insert-ebay-template*', `mon-insert-ebay-bmps-in-file',
`mon-insert-ebay-photo-per-scan-descr'."
  (interactive
   (list (let ((starting (read-number "Ebay Item no. to start from :"))
	       (ending (read-number "Ebay Item no. to end with :"))
	       (alt-prfx 
		(cond ((yes-or-no-p "Use an alt dir prefix (default is 'e') : ")
		       (let (str)
			 (setq str (read-string "Prefix files and folders with: "))
			 (setq str (replace-regexp-in-string "\\([[:space:]_]\\)" "-" str))
			 (setq str (replace-regexp-in-string "\\(--\\)" "-" str))
			 (setq str (replace-regexp-in-string "\\(\\([[:alnum:]]\\)\\([^-]$\\)\\)" "\\2\\3-" str nil nil))
			 str)))))
	   (mon-make-ebay-dir-list starting ending alt-prfx))))
  (let* ((make-list dir-list)
	 (into-path *ebay-images-temp-path*))
    (while make-list
      (let ((put-list (car make-list)))
	(make-directory (concat into-path "/BMP-Scans/" put-list) t)
	(let ((put-file (concat into-path "/BMP-Scans/" put-list "/" put-list ".dbc"))
              (make-html "(insert (mon-make-html-tree))" ))
          ;;(make-html (mon-make-html-tree)))
	  ;; setting buffer-local val b/c of bugs, 
	  ;; `with-temp-file' isn't setting unix-lf'sand  gettting 
's in file :(
	  (with-temp-file put-file 
	    (set (make-local-variable 'buffer-file-coding-system) 'utf-8-unix)	    
	    (insert (format *insert-ebay-template* put-file make-html put-list put-list put-list ))))
	(make-directory (concat into-path "/BIG-cropped-jpg/" put-list) t))
      (setq make-list (cdr make-list))))
  (message "ok finished - NOTE! files are in the temp path."))

;;;test-me;(setq my-make-dir-list '("eNNNq" "eNNNw" "eNNNz" "eNNNy"))
;;;test-me;(mon-insert-ebay-dirs my-make-dir-list)
;;;(concat "../" (directory-file-name (file-name-directory *ebay-scan-path*))
;;;(directory-file-name (file-name-directory dirname))

;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:15.57 PM - by MON KEY>
(defun mon-insert-ebay-dbc-file (dir-list)
  "Abriged file only (no-dir) version of `mon-insert-ebay-dirs'. 
Don't call this after evaluating it to create the file - it will clobber your files.
Use `mon-insert-ebay-dbc-template' to insert a buffer local template."
  (let* ((make-list dir-list)
	 (into-path (concat *ebay-images-path* "/")))
    (while make-list
      (let ((put-list (car make-list)))
	(let ((put-file (concat into-path "BMP-Scans/" put-list "/" put-list ".dbc"))
              ;;(make-html (mon-make-html-tree)))          
              (make-html "(insert (mon-make-html-tree))" ))
	  (with-temp-file put-file (insert (format *insert-ebay-template*
						   put-file make-html put-list put-list put-list)))))
      (setq make-list (cdr make-list)))))

;;; ==============================
;;; Subroutine for curr-dir above.
;;; CREATED: <Timestamp: Wednesday July 22, 2009 @ 08:35.33 PM - by MON KEY>
(defun mon-check-ebay-template-path ()
  "Subroutine for `mon-insert-ebay-dbc-template'."
  (let (check-the-check)
    (while (not check-the-check) 
      (let ((check-path 
             (cond ((and (not (mon-buffer-written-p))
                         (or (string-match (concat *ebay-images-temp-path* "/BMP-Scans/*") default-directory)
                             (string-match (concat *ebay-images-bmp-path* "/*") default-directory)))
                    (if (yes-or-no-p (format 
                                      "Buffer not writen use filename: %s :"
                                      (concat (file-name-nondirectory (directory-file-name default-directory)) ".dbc")))
                        (concat default-directory (file-name-nondirectory (directory-file-name default-directory))".dbc")
                      default-directory))
                   ;; ((and (mon-buffer-written-p)
                   ;;       (or (string-match (concat *ebay-images-temp-path* "/BMP-Scans/*") default-directory)
                   ;;           (string-match  (concat *ebay-images-bmp-path* "/*") default-directory))
                   ;;default-directory))
                   ((and (mon-buffer-written-p)
                         (or (string-match (concat *ebay-images-temp-path* "/BMP-Scans/*") default-directory)
                             (string-match  (concat *ebay-images-bmp-path* "/*") default-directory)))
                    (concat default-directory 
                            (file-name-nondirectory (directory-file-name default-directory)) ".dbc"))
                   (t (let ((start-from (cond ((string-match (concat *ebay-images-temp-path*) default-directory)
                                               default-directory)
                                              ((string-match  (concat *ebay-images-bmp-path*) default-directory)
                                               default-directory)
                                              ((string-match  *ebay-images-path* default-directory)
                                               default-directory)
                                              (t (concat *ebay-images-temp-path* "/BMP-Scans/"))))
                            ;;*ebay-images-bmp-path*)))
                            (new-dir))
                        (setq new-dir (read-directory-name 
                                       (concat "Not a good directory name to use w/ template insertion. "
                                               "You can either:\n"
                                               "a) Choose a better path (tab completes);\n"
                                               "b) Enter new path udner this (or other EBAY directory) to create.\n :")
                                       start-from))
                        (if (file-exists-p new-dir)
                            new-dir
                          (let* ((dir-file-str (concat (directory-file-name new-dir) "/"))
                                 (file-str (concat (or (when (buffer-file-name)
                                                         (file-name-nondirectory 
                                                          (file-name-sans-extension (buffer-file-name))))
                                                       (buffer-name))
                                                   ".dbc"))
                                 (chz-123 
                                  (completing-read
                                   (concat "The directory-chosen doesn't exist. Should we create the "
                                           "directory and write this buffer to a file there?\n"
                                           "Choice (3) prompts a new choice of directory/file pair.\n"   
                                           "Choice (2) creates the new directory and prompts alternate file name.\n"
                                           "Choice (1) creates the new directory and the auto provided filename.\n"
                                           "->" ;dir-file-str file-str "  :")
                                           "DIR-FILE-STR FILE-STR" "  :")
                                   '("1" "2" "3") nil t nil nil "1"))
                                 (made-a-new))
                            (cond ((string= chz-123 "3")
                                   nil)
                                  ((string= chz-123 "2")
                                   (progn 
                                     (setq made-a-new
                                           (concat dir-file-str
                                                   (file-name-sans-extension 
                                                    (read-string 
                                                     (concat "Provide an alternate filename \n" dir-file-str " :")
                                                     file-str nil file-str)) ".dbc"))
                                     made-a-new))
                                  ((string= chz-123 "1")
                                   (progn
                                     (setq made-a-new(concat dir-file-str file-str))
                                     made-a-new)))
                            (when made-a-new
                              (unwind-protect 
                                  (if (and (not (mon-buffer-written-p))
                                           (not (file-exists-p made-a-new)))
                                      (progn
                                        (make-directory (file-name-directory made-a-new) t)
                                        (write-file (concat dir-file-str file-str) t))
                                    (error "something went wrong writing %s" made-a-new))
                                made-a-new))  )))))))
        (when check-path (setq check-the-check check-path))))
    check-the-check))

;test-me;(mon-check-ebay-template-path)

;;; ==============================
;;; NOTES: right now we're doing a query replace to catch the temp path.
;;; It might be easier to simply make the path relative e.g.
;;; (concat *nef-scan-path* "/" (file-relative-name buffer-file-name *nef-scan-path*))
;;; (file-relative-name buffer-file-name *nef-scan-path*)
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:15.48 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: Wednesday July 22, 2009 @ 08:34.55 PM - by MON KEY>
(defun mon-insert-ebay-dbc-template (&optional insertp intrp) ;vars bound below may take &optional
  "Buffer local version of ebay template insertion utility.
Evaluates the global var `*insert-ebay-template*' and inserts ebay html with
`mon-make-html-tree'. Use: `mon-insert-ebay-html-tree' to insert _only_ 
the ebay-html-tree in `current-buffer' at point.
See also: `mon-insert-ebay-dirs' (build dirs and file) and;
`mon-insert-ebay-dirs' (file no-dir)."
  (interactive "i\np")
  (let* ((curr-dir (mon-check-ebay-template-path))
         (catch-path 
          (concat "\\(\\(" *ebay-images-temp-path* "/BMP-Scans\\)" "\\(/.*\\)\\)"))
         (fix-catch *ebay-images-bmp-path*)
         (put-file-or-dir (if (mon-buffer-written-p)
                              (buffer-file-name)
                            curr-dir))
         ;; convert: *ebay-images-temp-path* -> *ebay-images-bmp-path* 
         (put-file 
          ;;(if (= (string-match catch-path put-file-or-dir) 0)
          ;;(replace-regexp-in-string catch-path fix-catch put-file-or-dir nil nil 2)
          ;;put-file-or-dir))
          (cond ((not (string-match catch-path put-file-or-dir))
                 put-file-or-dir)
                ((= (string-match catch-path put-file-or-dir) 0)
                 (replace-regexp-in-string catch-path fix-catch put-file-or-dir nil nil 2))))
	 (put-dir (file-name-directory put-file)) ;save for later - will prob. need
	 (file-no-d (file-name-nondirectory put-file)) ;save for later - will prob. need
	 (file-no-ext (file-name-sans-extension put-file))
	 ;;(make-html (mon-make-html-tree))
	 (make-html "(insert (mon-make-html-tree))"))
    (let ((put-temp (format 
                     *insert-ebay-template* 
                     put-file           ;this file is:\n%s 
                     make-html          ;<!-- html-template-starts-here
                     put-dir            ;file-no-ext ;URLs inserted below for photo:
                     put-dir            ;file-no-ext ;URLs inserted below for photo:
                     (file-name-sans-extension file-no-d)          ;%s.dbc ends here
                    )))
           (if (or insertp intrp)
               (insert put-temp)
             put-temp))))

;;;test-me;(mon-insert-ebay-dbc-template)

;;; ==============================
;;; TODO: needs to take an interactive arg with `mon-get-imgs-in-dir-int' see below.
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 04:03.46 PM - by MON KEY>
(defun mon-get-ebay-bmps-in-dir (&optional full-path alt-path)
  "Returns a list of .bmps in files directory.
When FULL-PATH is non-nil, return absolute file names. 
When ALT-PATH is non-nil use dir-path as value instead of current-buffers.
Used-by: `mon-insert-ebay-bmps-in-file' to build a .bmp insertion list.
See also; `mon-get-ebay-nefs-in-dir', `mon-insert-ebay-bmps-in-file', 
`mon-get-ebay-jpgs-list', `mon-insert-ebay-jpgs-in-file', 
`mon-get-ebay-bmps-count', `mon-get-ebay-jpgs-count', 
`mon-get-ebay-bmps-in-dir'."
  (let (this-dir get-files)
    (if (and alt-path
	(file-exists-p alt-path))
	(setq this-dir (file-name-as-directory alt-path));(file-name-directory alt-path))
      (setq this-dir (file-name-directory buffer-file-name)))
    (setq get-files (directory-files this-dir full-path "\\.bmp"))
    get-files))

(defalias 'get-bmps-in-dir 'mon-get-ebay-bmps-in-dir)
(defalias 'mon-get-ebay-bmps-list 'mon-get-ebay-bmps-in-dir)

;;;test-me; (mon-get-ebay-bmps-in-dir)
;;;test-me; (mon-get-ebay-bmps-in-dir t)

;;; ==============================
;;; TODO: Needs to take an interactive arg with `mon-get-imgs-in-dir-int' see below.
;;; CREATED: <Timestamp: Wednesday May 06, 2009 @ 04:32.45 PM - by MON KEY>
(defun mon-get-nefs-in-dir (&optional full-path alt-path)
  "Returns a list of .nefs in buffers' directory.
When FULL-PATH is non-nil, return absolute file names. 
When ALT-PATH is non-nil use dir-path as value instead of current-buffers.  
Used by `mon-insert-ebay-bmps-in-file' to build a .nef insertion list.
See also; `mon-get-ebay-bmps-in-dir', `mon-insert-ebay-bmps-in-file', 
`mon-get-ebay-jpgs-list', `mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-jpgs-count', `mon-get-ebay-bmps-in-dir'."
  (let (this-dir get-files)
    (if (and alt-path
	(file-exists-p alt-path))	
	(setq this-dir (file-name-as-directory alt-path));(file-name-directory alt-path))
    (setq this-dir (file-name-directory buffer-file-name)))
    (setq get-files (directory-files this-dir full-path "\\.nef"))
    get-files))

(defalias 'get-nefs-in-dir 'mon-get-nefs-in-dir)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 04:12.25 PM - by MON KEY>
(defun mon-insert-ebay-bmps-in-file (&optional full-path)
  "Insert in buffer names of .bmp files in buffers' directory.
When FULL-PATH non-nil inserts full-path of .bmp files.
See; `*insert-ebay-template*', `mon-insert-ebay-dirs', 
`mon-insert-ebay-photo-per-scan-descr', `mon-get-ebay-bmps-in-dir'."
  (interactive "p")
  (let ((bmp-files 
	 (if full-path
	     (mon-get-ebay-bmps-in-dir t)
	   (mon-get-ebay-bmps-in-dir nil))))
    (while bmp-files 
      (insert (format "\n%s" (car bmp-files)))
	(setq bmp-files (cdr bmp-files)))))

;;;test-me; (mon-insert-ebay-bmps-in-file)
;;;test-me; (mon-insert-ebay-bmps-in-file t)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:14.36 PM - by MON KEY>
(defun mon-get-ebay-jpgs-list (&optional full-path) 
  "Called from an ebay-template file finds items' .jpg path from relative dir.
See also; `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list', 
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-jpgs-count', `mon-get-ebay-bmps-in-dir'."
  (let ((jpg-path (concat 
		   (expand-file-name "../../") 
		   "BIG-cropped-jpg/" 
		   (file-name-nondirectory (directory-file-name default-directory))))	
	(this-dir (file-name-nondirectory (directory-file-name default-directory)))
	(collect-jpg-path))
    (if (file-exists-p jpg-path)
	(let ((in-path (directory-files jpg-path full-path ".jpg"))
	      (walk-files))
	  (setq walk-files in-path)
	  (while walk-files
	    (add-to-list 'collect-jpg-path (car walk-files))
	    (setq walk-files (cdr walk-files))))
      nil)
    collect-jpg-path))

;;;test-me;(mon-get-ebay-jpgs-list t)
;;;test-me;(mon-get-ebay-jpgs-list)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-09-05T15:51:57-04:00Z}#{09366} - by MON>
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:14.41 PM - by MON KEY>
(defun mon-insert-ebay-jpgs-in-file ()
  "Called from an ebay-template file inserts the items' jpg path from relative dir.
Returns error message if items jpg path doesn't exist. 
See also; `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list'."
  (interactive)
  (let ((find-jpgs (nreverse (mon-get-ebay-jpgs-list)))
	(jpg-dir (concat *ebay-images-jpg-path* "/"
			 ;;WAS: (expand-file-name "../../")  "BIG-cropped-jpg/" 
			 (file-name-nondirectory (directory-file-name default-directory)) "/"))
	(gather-jpgs))
    (while find-jpgs
      (princ
       (format "\n%s%s" jpg-dir (car find-jpgs))
       (current-buffer))
      (setq find-jpgs (cdr find-jpgs)))))
    
;;;test-me;(mon-insert-ebay-jpgs-in-file)  

;;; ==============================
(defun mon-get-ebay-jpgs-count ()
  "Returns count of jpgs associated with ebay templates' buffer.
See also; `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list', 
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-bmps-in-dir'."
  (interactive)
  (let ((jpg-count  (length (mon-get-ebay-jpgs-list t))))
    (message "%d" jpg-count)))

;;;test-me: (mon-get-ebay-jpgs-count)

;;; ==============================
(defun mon-get-ebay-bmps-count ()
  "Returns count of bmps associated with ebay templates' buffer.
See also; `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list', 
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-jpgs-count', 
`mon-get-ebay-bmps-in-dir'."
  (interactive)
  (let ((bmp-count  (length (mon-get-ebay-bmps-in-dir))))
    (message "%d" bmp-count)))

;;;test-me:(mon-get-ebay-bmp-count)

;;; ==============================
(defun mon-get-ebay-img-count-verify ()
  "Returns message and t or nil for image counts (bmp|jpg) of ebay buffers' dir.
See also; `mon-insert-ebay-bmps-in-file', `mon-get-ebay-jpgs-list', 
`mon-insert-ebay-jpgs-in-file', `mon-get-ebay-bmps-count', 
`mon-get-ebay-jpgs-count'."
  (interactive)
  (if (equal (mon-get-ebay-bmps-count) (mon-get-ebay-jpgs-count))
      (prog1
	  (message "Counts match. There are %s .bmps, and %s .jpg files."  
		   (mon-get-ebay-bmps-count) (mon-get-ebay-jpgs-count))
	t)
    (prog1
	(message "Counts _DO NOT_ match. There are %s .bmps, and %s .jpg files."  
		 (mon-get-ebay-bmps-count) (mon-get-ebay-jpgs-count))
      nil)))

;;; ==============================
;;; TODO: build function to walk list of jpgs and insert for each a linkified list
;; (defun mon-ebay-image-linkify-all
;;   (let ((linkify-jpgs (mon-get-ebay-jpgs-list t)
;;      (walk-linkify))
;;       (setq walk-linkify
;; 	    (while linkify-jpgs
;; 	      )))))

;;; ==============================
;;; TODO: function to capture region between `►►►' `◄◄◄'
;;; (skip-chars-forward "^►►►" (1- (point-max))) 
;;; (skip-chars-forward "►►►" (1- (point-max)))
;;; (skip-chars-forward "^◄◄◄" (1- (point-max)))
;;; (skip-chars-forward "◄◄◄" (1- (point-max)))
;;; <Timestamp: Friday May 22, 2009 @ 06:39.18 PM - by MON KEY>
;;; ==============================

;;; ==============================
;;; CREATED: <Timestamp: Friday May 22, 2009 @ 06:39.18 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: Monday July 27, 2009 @ 01:53.42 PM - by MON KEY>
(defun mon-ebay-field-trigger (&optional insertp right-only left-only intrp)
  "Return ebay temmplate field delimiter. one of `►►►', `◄◄◄', `►►►'<nwln>`◄◄◄'.
Callled interactively inserts `►►►'<nwln>`◄◄◄' at point.
When INSERTP is non-nil insert delimiter\(s\) according to args.
When LEFT-AND-RIGHT is non-nil insert as per interactive
When RIGHT-ONLY is non-nil insert `►►►' - starting field trigger.
When LEFT-ONLY is non-nil insert optional `◄◄◄' - ending field trigger.\n
Name: BLACK RIGHT-POINTING POINTER
►►► | code point: 0x25BA
character: ► (9658, #o22672, #x25ba) 
\(mon-insert-unicode \"25BA\")\n\n
Name: BLACK LEFT-POINTING POINTER
◄◄◄ | code point: 0x25C4
character: ◄ (9668, #o22704, #x25c4)
\(mon-insert-unicode \"25C4\")\n
See also; Convienence functions `mon-insert-ebay-field-trigger-r',
`mon-insert-ebay-field-trigger-l', `mon-insert-ebay-field-trigger-l-and-r'."
(interactive "i\ni\ni\np")
  (let ((field-trigger-r "►►►")
	(field-trigger-l "◄◄◄"))
    (cond (intrp  (insert (format "%s\n%s" field-trigger-r field-trigger-l)))
          (insertp (cond (right-only (insert (format "%s" field-trigger-r)))
                         (left-only (insert (format "%s" field-trigger-l)))
                         (t (insert (format "%s\n%s" field-trigger-r field-trigger-l)))))
          (t (cond (right-only (format "%s" field-trigger-r))
                   (left-only (format "%s" field-trigger-l))
                   (t (format "%s\n%s" field-trigger-r field-trigger-l)))))))
          
;;;test-me;(mon-ebay-field-trigger)
;;;test-me;(mon-ebay-field-trigger t)
;;;test-me;(mon-ebay-field-trigger t t)
;;;(mon-ebay-field-trigger t nil t)
;;;(mon-ebay-field-trigger nil t)
;;;(mon-ebay-field-trigger nil nil t)
;;;(call-interactively 'mon-ebay-field-trigger)
;;;(unintern 'mon-ebay-field-trigger)

;;; ==============================
(defun mon-insert-ebay-field-trigger-r ()
  "Insert the right point ebay field delim `►►►'.
Name: BLACK RIGHT-POINTING POINTER
►►► | code point: 0x25BA
character: ► (9658, #o22672, #x25ba) 
\(mon-insert-unicode \"25BA\")
Convienence function for `mon-ebay-field-trigger'.
See also; `mon-insert-ebay-field-trigger-r',
`mon-insert-ebay-field-trigger-l-and-r'."
  (interactive)
  (mon-ebay-field-trigger t t))

;;
(defun mon-insert-ebay-field-trigger-l ()
  "Insert the right point ebay field delim. `◄◄◄'
Name: BLACK LEFT-POINTING POINTER
◄◄◄ | code point: 0x25C4
character: ◄ (9668, #o22704, #x25c4)
\(mon-insert-unicode \"25C4\")\n
Convienence function for `mon-ebay-field-trigger'
See also; `mon-insert-ebay-field-trigger-r',
`mon-insert-ebay-field-trigger-l-and-r'."
  (interactive)
  (mon-ebay-field-trigger t nil t))

;;
(defun mon-insert-ebay-field-trigger-l-and-r ()
  "Insert at point the right and left ebay field delim.
◄◄◄\n►►►
Convienence function for `mon-ebay-field-trigger'.
See also; `mon-insert-ebay-field-trigger-r', 
`mon-insert-ebay-field-trigger-l'."
  (interactive)
  (mon-ebay-field-trigger t))

;;; ==============================
;;; TODO: incorporate: 
;;; mon-insert-ebay-bmps-in-file
;;; (mon-get-ebay-bmps-list)
;;; (mon-get-ebay-bmps-in-dir t)
;;; mon-insert-ebay-jpgs-in-file
;;; mon-get-ebay-jpgs-list
;;; mon-get-ebay-bmps-count mon-get-ebay-jpgs-count
;;; mon-get-ebay-img-count-verify
;;;
;;; CREATED: <Timestamp: Wednesday April 29, 2009 @ 03:14.49 PM - by MON KEY>
(defun mon-insert-ebay-photo-per-scan-descr (times-to-put item-number)
  "Inserts template to describe items according to their eBay scan number.
Interactively Minibuffer prompts for TIMES-TO-PUT arg:\n
    \'Number of image descriptions needed :\'\n
and ITEM-NUMBER arg;\n
    \'Item number being described :\'\n
Each item description inserted step item number count eNNNN-N by 1 from 0 e.g.:\n
 ---\n e1018-0.bmp\n Image-title: \n Image-Description: 
See also; `mon-insert-ebay-bmps-in-file', `mon-insert-ebay-dirs',
`mon-get-ebay-jpgs-list', `mon-get-ebay-bmps-in-dir'."
  (interactive "nNumber of image descriptions needed :\nnItem number being described :")
  (let ((times times-to-put)
        (item-num item-number)
        (count 0))
    (while (>= times count)
      (insert 
       (format "\n►►►\nebay-scan: e%d-%d.bmp\nImage-title:\nImage-description:\n◄◄◄\n" item-num  count))
      (setq count (1+ count)))))

;;;test-me;(mon-insert-ebay-photo-per-scan-descr 3 1143)
;;;test-me;M-x mon-insert-ebay-photo-per-scan-descr

;;; ==============================
;;; Courtesy: Xah Lee (URL `http://xahlee.org/emacs/elisp_image_tag.html')
;;; MODIFICATIONS: <Timestamp: Saturday April 25, 2009 @ 05:35.54 PM - by MON KEY>
;;; Regexp for `image-name' var, subst for `myResult'
(defun mon-ebay-image-linkify ()
  "Replace the path to image file with an HTML <img> tag.
Example, if cursor is on the word “emacs_logo.png”, then it will become:
\“<img src=\"emacs_logo.png\" alt=\"emacs logo\" width=\"123\" height=\"456\">\”.

This function requires the \"identify\" command from (URL `ImageMagick.com')
See also: `mon-ebay-image-linkify-lite'."
(interactive)
  (let (img-file-path 
	bounds 
	image-name 
	image-name-no-ext 
	image-no
	img-dim 
	img-md5
	width 
	height 
	altText 
	myResult)
    (setq img-file-path (thing-at-point 'filename))
    (setq bounds (bounds-of-thing-at-point 'filename))
    (setq image-name (replace-regexp-in-string 
		      (concat *YOUR-FILE-PATH* ".*\\(/e[0-9]\\{4,4\\}/\\)") "" img-file-path))
    (setq image-name-no-ext (replace-regexp-in-string "\\-[0-9]\\.jpg" "" image-name))
    (setq image-no (replace-regexp-in-string "\\e[0-9]\\{4,4\\}-" "" image-name))
    (setq image-no (replace-regexp-in-string "\\.jpg" "" image-no))
    (setq altText (replace-regexp-in-string "\\.[A-Za-z]\\{3,4\\}$" "" image-name t t)) 
    (setq altText (replace-regexp-in-string "_" " " altText t t))
    (setq altText (concat 
		   "Photo: " image-no
		   " this ebay-item-no: " 
		   (replace-regexp-in-string "\\-[0-9]\\.jpg" "" image-name)))
    (setq img-dim (mon-get-image-dimensions img-file-path))
    (setq img-md5 (car (mon-get-image-md5 img-file-path)))
    (setq width (number-to-string (car img-dim)))
    (setq height (number-to-string (car (last img-dim))))
    (setq myResult  
	  (concat "<img src=\"http://www.some-domain-name.com/photos/ebay/"  image-name "\""
		  "\n     class=\"int_" image-name-no-ext "_" image-no "_my-domain.com\""
		  "\n     id=\"ΡΗ0T0-no-" image-no "¦"  image-name-no-ext		  
		  "\n     MD=\"Δ_md5¦" img-md5 "¦_my_checksum_Δ\""
		  "\n     longdesc=\"item→" image-name-no-ext "_eßay_ΡΗΟτΟ_No_" image-no 
		  "\n     width=\"" width "\" "
		  "\n     height=\"" height "\""			    
		  "\n     alt=\"" altText "\""			  
		  "\n     title=\"THIS IMGAGE" image-name-no-ext "_photo_" image-no "_of_ebay-listing""\"/>"))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (insert myResult))))

;;; ==============================
(defun mon-ebay-image-linkify-lite ()
  "Replace the path to image file with an HTML <img> tag.
This function requires the \“identify\” command from (URL `ImageMagick.com').
See also: `mon-ebay-image-linkify'."
  (interactive)
  (let (img-file-path 
	bounds 
	image-name 
	image-name-no-ext 
	image-no
	img-dim 
	img-md5
	width 
	height 
	altText 
	myResult)
    (setq img-file-path (thing-at-point 'filename))
    (setq bounds (bounds-of-thing-at-point 'filename))
    (setq image-name 
	  (replace-regexp-in-string 
	   (concat *ebay-images-jpg-path* ".*\\(/e[0-9]\\{4,4\\}/\\)") "" img-file-path))
    (setq image-name-no-ext (replace-regexp-in-string "\\-[0-9]\\.jpg" "" image-name))
    (setq image-no (replace-regexp-in-string "\\e[0-9]\\{4,4\\}-" "" image-name))
    (setq image-no (replace-regexp-in-string "\\.jpg" "" image-no))
    (setq altText (replace-regexp-in-string "\\.[A-Za-z]\\{3,4\\}$" "" image-name t t)) 
    (setq altText (replace-regexp-in-string "_" " " altText t t))
    (setq altText (concat 
		   "Photo: " image-no
		   " this ebay-item-no: " 
		   (replace-regexp-in-string "\\-[0-9]\\.jpg" "" image-name)))
    (setq img-dim (mon-get-image-dimensions img-file-path))
    (setq img-md5 (car (mon-get-image-md5 img-file-path)))
    (setq width (number-to-string (car img-dim)))
    (setq height (number-to-string (car (last img-dim))))
    (setq myResult  
	  (concat "(html-img :src \"http://www.my-domain.com/photos/ebay/" image-name "\""
     		  "\n     :class \"int_" image-name-no-ext "_" image-no "_my-domain.com\""
		  "\n     :id \"ΡΗ0T0-no-" image-no "¦"  image-name-no-ext  
		  "\n     :MD \"Δ_md5¦" img-md5 "¦_my-domain_checksum_Δ\""
		  "\n     :longdesc \"item→" image-name-no-ext "_eßay_ΡΗΟτΟ_No_" image-no "→WωW·MY-DOMAIN.CΘΜ…\""
		  "\n     :width \"" width "\" "
		  "\n     :height \"" height "\""			    
		  "\n     :alt \"" altText "\""			  
		  "\n     :title \"◊MY-DOMAIN·com◊_item_" image-name-no-ext "_photo_" image-no "_of_ebay-listing\")"
			    ))
    (save-excursion
      (delete-region (car bounds) (cdr bounds))
      (insert myResult))))


;;; ==============================
(provide 'ebay-template-tools)
;;; ==============================

;;; ================================================================
;;; ebay-template-tools.el ends here
;;; EOF
