;;; This is mon-dir-utils.el
;;; ================================================================
;;; DESCRIPTION:
;;; mon-dir-utils.el
;;;
;;; CONSTANTS or VARIABLES:
;;; `*img-hash*' and (testing version `*temp-hash*')
;;; `*nef-img-hash*', `*jpg-img-hash*', `*bmp-img-hash*'
;;;
;;; MACROS:
;;;
;;; FUNCTIONS:►►►
;;; `mon-dired-srt-alph',`mon-dired-srt-chrn',`mon-dired-srt-type'
;;; `mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'
;;; `dired-up-directory-this-buffer', `dired-insert-dirs-recursive',
;;; `naf-explorer-artist', `naf-explorer-brand', `naf-dired-artist-letter',
;;; `naf-dired-brand-letter', `mon-save-current-directory',
;;; `mon-save-current-directory-to-file', `mon-add-subdirs-to-list',
;;; `mon-insert-subdirs-in-buffer', `mon-serial-rename', `mon-map-file-lines',
;;; `mon-path', `mon-copy-file-path', `mon-insert-path',
;;; `mon-get-buffers-directories', `mon-proc-buffers-directories',
;;; `mon-get-proc-buffers-directories', `mon-get-buffers-parent-dir'
;;; `mon-buffer-written-p', `mon-split-string-buffer-name',
;;; `mon-split-string-buffer-parent-dir', `mon-check-image-type'
;;; `mon-ebay-image-directory-not-ok', `mon-ebay-image-directory-ok-p'
;;; `mon-truncate-path-for-prompt', `mon-get-most-common-path'
;;; `mon-walk-buff-or-dir-path', `mon-dired-nef-dir', `mon-nef-dir-big', 
;;; `mon-nef-dir-converge', `mon-nef-dir-keep-3',`mon-nef-dir-fldr',
;;; `mon-nef-dir-conc-ranges', `mon-nef-dir-ranges',`mon-nef-dir-conc-dups',
;;; `mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt', 
;;; `mon--local-url-for-bug' `mon-local-url-for-bug'
;;; `mon-toggle-dired-dwim-target'
;;; FUNCTIONS:◄◄◄
;;;
;;; ALIASES
;;; `dired-up-here' -> `dired-up-directory-this-buffer' 
;;; 
;;; DEPRECATED OR MOVED:
;;; Naf-mode related Dired path-vars `*artist-naf-path*', `*brand-naf-path*'
;;; moved to: `./mon-dir-locals-alist.el'
;;; <Timestamp: Tuesday June 16, 2009 @ 04:42.01 PM - by MON KEY>
;;;
;;; Following 4(Four) functions moved to: "./mon-time-utils.el"
;;; `mon-comp-times-flt-pnt', `mon-conv-time-flt-pnt'
;;; `mon-get-file-mod-times', `mon-file-older-than-file-p'
;;;
;;; Following 6(Six) Dired switches functions in: `mon-dir-utils-switches.el'
;;; `mon-dired-srt-alph', `mon-dired-srt-chrn', 
;;; `mon-dired-srt-type', `mon-dired-srt-type-alph', 
;;; `mon-dired-srt-type-chrn', `dired-up-directory-this-buffer', 
;;;
;;; REQUIRES:
;;; `cl.el' -> `mon-multi-read-name', `mon-reduce-file-name'
;;; `mon-dir-locals-alist.el' -> `*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'
;;; `mon-hash-utils.el'
;;; `naf-mode-replacements.el'
;;; `mon-rename-image-utils.el'
;;;
;;; TODO:
;;;
;;; NOTES:
;;; 
;;; FILE-CREATED:
;;; <Timestamp: Monday May 11, 2009 @ 11:13.47 AM - by MON KEY>
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
;;; Copyright (C) 2009 MON KEY
;;; ==========================
;;; CODE:

;;; needed for: `mon-multi-read-name', `mon-reduce-file-name'
(eval-when-compile (require 'cl)) 

;;; ==============================
(load "mon-dir-locals-alist")
;;(require 'mon-dir-locals-alist)
(require 'mon-hash-utils)
(require 'naf-mode-replacements)
(require 'mon-rename-image-utils)
;;; ==============================

;;; ==============================
(defvar *img-hash* nil
  "Hash-table for holding images in image directories.
Called by: `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.
Image directories defined in global vars: 
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.")
;;
(when (not (bound-and-true-p *img-hash*))
  (setq *img-hash* (make-hash-table :test 'equal)))

;;; (boundp '*img-hash*)
;;; (unintern '*img-hash*)

;;; ==============================
(defvar *nef-img-hash* nil
  "Hash-table for holding images in image directories.
Called by: `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.
Image directories defined in global vars `*nefs_photos_nefs-alist*'
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.")
;;
(when (not (bound-and-true-p *nef-img-hash*))
  (setq *nef-img-hash* (make-hash-table :test 'equal)))

;;; (assoc ".nef1" *ebay-images-lookup-path*)
;;; (assoc ".nef2" *ebay-images-lookup-path*)
;;; (boundp '*nef-img-hash*)
;;; (makunbound '*nef-img-hash*)
;;; (unintern '*nef-img-hash*)
;;; ==============================

;;; ==============================
(defvar *bmp-img-hash* nil
  "Hash-table for holding images in image `*ebay-images-bmp-path*'.
Called by: `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.
Image directories defined in global vars: 
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.")
;;
(when (not (bound-and-true-p *bmp-img-hash*))
  (setq *bmp-img-hash* (make-hash-table :test 'equal)))

;;; (assoc ".bmp" *ebay-images-lookup-path*)
;;; (boundp '*bmp-img-hash*)
;;; (makunbound '*bmp-img-hash*)
;;; (unintern '*bmp-img-hash*)
;;; ==============================

;;; ==============================
(defvar *jpg-img-hash* 'nil
  "Hash-table for holding images in image `*ebay-images-bmp-path*'.
Called by: `mon-hash-img-dir', `mon-try-comp-dir', `mon-complete-hashed-dir'.
Image directories defined in global vars: 
`*nef-scan-path*', `*nef-scan-nefs-path*',`*nef-scan-nef2-path*',
`*ebay-images-path*',`*ebay-images-bmp-path*',`*ebay-images-jpg-path*',
`*ebay-images-temp-path*'.")
;;
(when (not (bound-and-true-p *jpg-img-hash*))
  (setq *jpg-img-hash* (make-hash-table :test 'equal)))

;;;test-me; *jpg-img-hash*
;;;(boundp '*jpg-img-hash*)
;;
;;;(progn (makunbound '*jpg-img-hash*) (unintern '*jpg-img-hash*)


;;; ==============================
;;; Directory and Dired Related:
;;; ==============================

;;; ==============================
(defun mon-toggle-dired-dwim-target (&optional intrp)
  "Toggle `dired-dwim-target'.\n
See also `dired-dwim-target-directory'"
  (interactive "p")
  (let ((toggle-dwim-target 
         (if (bound-and-true-p dired-dwim-target)
             (progn
               (setq dired-dwim-target nil)
               (message "dired-dwim-target turned off.\nCall mon-toggle-dired-dwim-target to toggle on."))
           (progn
             (setq dired-dwim-target t)
             (message "dired-dwim-target turned on.\nCall mon-toggle-dired-dwim-target to toggle off.")))))
    toggle-dwim-target))

;;;test-me;(mon-toggle-dired-dwim-target)
;;;test-me;(mon-toggle-dired-dwim-target)

;;;*nef-scan-nef2-path*

;;; ==============================
;;; CREATED: <Timestamp: Tuesday July 21, 2009 @ 05:36.07 PM - by MON KEY>
(defun mon-file-path-for-bug (&optional file-name-path insertp yankp intrp)
  "Provide portable file-name-path for BUGd systems.
FILE-NAME-PATH \(a string\) should be a full pathname string and be located 
beneath `mon-emacs-root'.
If *bug-HG-path* is local  then return a path suitable for the remote machine.
If *bug-HG-path* is remote then return a path suitable for the local machine."
  (interactive "i\ni\nP\np")
  (let* ((fnp-tst (cond ((not file-name-path) "Path not under ")
                         ((and (not intrp) file-name-path)
                          (if (and (file-exists-p file-name-path)
                                   (string-match mon-emacs-root file-name-path))
                              file-name-path
                            "Path not under "))))
         (fnp (cond ((not (string-match "Path not" fnp-tst)) fnp-tst)
                    ((or intrp (string-match "Path not" fnp-tst))
               (expand-file-name
                      (read-file-name 
                       (if (string-match "Path not" fnp-tst)
                           (concat fnp-tst mon-emacs-root " :")
                         "Which path shall we build? :")
                                      (expand-file-name default-directory)
                                      (if (mon-buffer-written-p)
                                          buffer-file-name
                                        (expand-file-name default-directory))
                                      t
                                      (if (mon-buffer-written-p)
                                          (file-name-nondirectory buffer-file-name)))))))
         (fnp-rel (concat "/" (file-relative-name fnp mon-emacs-root))))
    (cond (insertp
           (if yankp
               (progn
                 (kill-new (concat *bug-HG-path* fnp-rel))
                 (insert (format "(find-file \"%s\")" (concat *bug-HG-path* fnp-rel))))
             (insert (format "(find-file \"%s\")" (concat *bug-HG-path* fnp-rel)))
             ))
          ((or intrp yankp)
             (progn
               (kill-new (concat *bug-HG-path* fnp-rel))
               (when intrp
                 (message "Yank path to insert: %s "(concat *bug-HG-path* fnp-rel)))))
          (t (format "%s%s"*bug-HG-path* fnp-rel)))))
          
;;;test-me;(mon-file-path-for-bug)
;;;test-me;(mon-file-path-for-bug nil t t)
;;;test-me;(mon-file-path-for-bug "bubba" nil t)
;;;test-me;(mon-file-path-for-bug (concat mon-naf-mode-notes "/latin-lorum-ipsum.dbc") t t)
;;;test-me;(mon-file-path-for-bug (concat mon-naf-mode-notes "/latin-lorum-ipsum.dbc" t) 

;;; ==============================
;;; CREATED: <Timestamp: Wednesday July 15, 2009 @ 02:19.43 PM - by MON KEY>
(defun mon--local-url-for-bug (is-url file-string)
  "Helper function for interactive `mon-local-url-for-bug'.
See also; `mon-local-url-for-bug' and `*bug-HG-path*'"
  (concat "file:///" *bug-HG-path* "/" file-string))
;;
(defun mon-local-url-for-bug (file-string)
  "Prompt for a url to concat onto the common path shared between the MON KEY 
machine and the BUGd system.\nSee also; `mon--local-url-for-bug',`*bug-HG-path*'"
  (interactive (list (read-string (format "Url beneath file: %s" 
                                  (concat (nth 6 (assoc 3 *mon-emacsd*)) "/")))))
  (prin1 (mon--local-url-for-bug file-string) (current-buffer)))

;;test-me;(mon--local-url-for-bug "bubba")
;;;test-me;(call-interactively 'mon-local-url-for-bug)

;;; ==============================
(defun mon-dired-srt-alph ()
  "Set ls switch to sort Dired direcotry alphebetically.\n
See also; `mon-dired-srt-chrn', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'."
  (interactive)
  (dired-sort-other "-la"))

;;; =======================
(defun mon-dired-srt-chrn ()
  "Set ls switch to sort Dired direcotry chronologically.\n
See also; `mon-dired-srt-alph', `mon-dired-srt-type', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'."
  (interactive)
  (dired-sort-other "-lt")) ;;mon-dired-srt-alph

;;; =======================
(defun mon-dired-srt-type ()
  "Set ls switch to sort Dired direcotry by type.\n
See also; `mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type',
`mon-dired-srt-type-alph', `mon-dired-srt-type-chrn'."
  (interactive)
  (dired-sort-other "-lX"))

;;; ==============================
(defun mon-dired-srt-type-alph ()
  "Set ls switch to sort Dired direcotry by type -> alphabetically.\n
See also; `mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type', 
`mon-dired-srt-type', `mon-dired-srt-type-chrn'."
  (interactive)
  (dired-sort-other "-lXa"))

;;; ==============================
(defun mon-dired-srt-type-chrn ()
  "Set ls switch to sort Dired direcotry by type -> chronologically.\n
See also; `mon-dired-srt-alph', `mon-dired-srt-chrn', `mon-dired-srt-type', 
`mon-dired-srt-type', `mon-dired-srt-type-alph'."
  (interactive)
  (dired-sort-other "-lXt"))

;;; ==============================
;;(define-key dired-mode-map "\177" 'dired-up-directory-this-buffer)
;;; COURTESY: Stefan Reichor, <stefan@xsteve.at> HIS: xsteve-functions.el
(defun dired-up-directory-this-buffer ()
"Move up the directory tree eg `../' to a new dired buffer killing current one."
  (interactive)
  (let ((buffer))
    (setq buffer (current-buffer))
    (dired-up-directory)
    (kill-buffer buffer)))
;;
(defalias 'dired-up-here 'dired-up-directory-this-buffer)

;;; ==============================
;;;(define-key dired-mode-map [(meta i)] 'dired-insert-dirs-recursive)
;;; COURTESY: Stefan Reichor, stefan@xsteve.at HIS: xsteve-functions.el VERSION: 2001-03-28
(defun dired-insert-dirs-recursive (dirname)
"In dired recursively inserts the subdirs of dir at point."
  (interactive
   (list (dired-get-filename)))
  (dired-maybe-insert-subdir dirname "-laR"))

;;; ==============================
;;; CREATED: <Timestamp: Monday June 22, 2009 @ 02:54.20 PM - by MON KEY>
(defun mon-copy-files-in-sub-dirs (gather-dir destination-dir)
  "For each sub-directory 'sd' of GATHR-DIR copies files of 'sd' to DESTINATION-DIR.
GATHER-DIR and DESTINATION-DIR are full paths. 
Copies _only_ files not directories. 
Does not recursively descend 'sd' subdirectories. 
Does not copy sub-directories of 'sd' to DESTINATION-DIR."
  ;;first gather sub-dirs
  (let* ((gthr-dir (if (and (file-exists-p gather-dir) (file-directory-p gather-dir))
                       (directory-file-name gather-dir)
                     (error "The directory provided doesn't exist or names a file: \n%s" gather-dir)))
         (to-dir (if (and (file-exists-p destination-dir) (file-directory-p destination-dir))
                     (directory-file-name destination-dir)
                   (error "The directory provided doesn't exist or names a file: \n%s" destination-dir)))
         ;;(to-dir-fname (car (last (split-string to-dir "/" t))))
         (gthr-in gthr-dir)
         (walk-dir (directory-files gthr-in t))
         (gthrd))
    (setq gthrd ())
    (mapcar '(lambda (x)
               (let ((in-here (if (and (file-directory-p x)
                                       (not 
                                        (or (string= (concat gthr-in "/.") x)
                                            (string= (concat gthr-in "/..") x)
                                            (string= to-dir x))))
                                  x)))
                 (when in-here  (setq gthrd (cons in-here gthrd))))) walk-dir)
    ;;Now copy files per subdir to dest-dir.
    (mapcar (lambda (x) 
              (let* ((in-dir x)
                     (walk-sub (directory-files in-dir t)))
                (mapcar '(lambda (y)
                           (if (and (not (file-directory-p y)) 
                                    (not (or (string= (concat in-dir "/" ".") y) 
                                             (string= (concat in-dir "/" "..") y))))
                               (copy-file y to-dir t)))
                        walk-sub)))
            gthrd)))

;;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: tv-utils WAS: `lmcp'
(defun mon-multi-copy-file (file &optional list-of-dir)
  "Copy `file' in multiple directories.
At each prompt for a directory add + to input another directory name.
When '+' is not added to directory name input is finished and function returns.
See; also `mon-copy-files-in-sub-dirs'."
  (interactive "fFile: ")
  (let* ((dest-list nil)
         (final-list
          (if list-of-dir
              list-of-dir
              (mon-multi-read-name 'read-directory-name))))
    (loop for i 
          in final-list
	  do (copy-file file i t))))

;;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: tv-utils WAS: `multi-read-name'
(defun* mon-multi-read-name (&optional (fn 'read-string))
  "Prompt indefinely whild a is `+' suffixed to read value.
Return a list of all inputs in `var'.
Accepts specification of an alternate input function to use."
  (labels ((multiread ()
             (let ((stock)
                   (str (funcall fn (cond ((eq fn 'read-string)
                                           "String(add + to repeat): ")
                                          ((eq fn 'read-directory-name)
                                           "Directory(add + to repeat): ")
                                          (t
                                           "File(add + to repeat): ")))))
               (push (replace-regexp-in-string "\+" "" str) stock)
               (cond ((string-match "\+" str)
                      (push (car stock) var)
                      (multiread))
                     (t
                      (push (car stock) var)
                      (nreverse var))))))
    (let (var)
      (multiread))))

;;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: tv-utils WAS: `cat'
(defmacro mon-cat (file)
  `(let ((file-contents (with-temp-buffer
                          (insert-file-contents ,file)
                          (buffer-string))))
     file-contents))

;;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: tv-utils WAS: `tv-reduce-file-name'
;;; MODIFICATIONS: <Timestamp: #{2009-09-01T20:39:35-04:00Z}#{09363} - by MON KEY>
(defun* mon-reduce-file-name (fname level &key unix-close expand)
    "Reduce file-name by LEVEL (an integer) depending on LEVEL's value.
If LEVEL is positive reduce by end else by beginning.
:UNIX-CLOSE (a boolean) non-nil close filename with '/'.
:EXPAND (a boolean) when non-nil `expand-file-name' of FNAME."
  (let* ((exp-fname (expand-file-name fname))
         (fname-list (split-string (if expand exp-fname fname) "/" t))
         (len (length fname-list))
         (pop-list (if (< level 0)
                       (subseq fname-list (* level -1))
                       (subseq fname-list 0 (- len level))))
         (result (mapconcat #'(lambda (x) x) pop-list "/")))
     (if unix-close
         (if expand
             (if (< level 0)
                 (concat "../" result "/")
                 (concat "/" result "/"))
             (if (string-match "~/" result)
                 (concat result "/")
                 (if (< level 0)
                     (concat "../" result "/")
                     (concat "/" result "/"))))
         (if expand
             (if (< level 0)
                 (concat "../" result "/")
                 (concat "/" result "/"))
             (if (string-match "~/" result)
                 (concat result "/")
                 (if (< level 0)
                     (concat "../" result "/")
                     (concat "/" result "/")))))
     ;;standard w32 Mods
     ;;MON KEY w32 MODs
     (if (and (eql system-type 'windows-nt)
              (string-match "\\(/[[:alpha:]]:\\)" (subseq result 0 3)))
         (subseq result 1))
     result))

;;;test-me(mon-reduce-file-name data-directory 1)

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 30, 2009 @ 02:42.31 PM - by MON KEY>
(defun mon-build-path (expand-path suffix-path &rest more-paths)
  "Returns a path with EXPAND-PATH concatenated to SUFFIX-PATH
When MORE-PATHS is non-nil each additional string is appended to path.
Throws an error if any of the args aren't in the path.\n
EXAMPLE:
\(mon-build-path *artist-naf-path* \"C-Artists names\" \"Cappiello \(Leonetto\)\"\)
\(mon-build-path *artist-naf-path* \"C-Artists names\"\)\n
EXAMPLE: {CALLING PROGRAMATICALLY}
\(apply 'mon-build-path *artist-naf-path* \"C-Artists names\"
\(split-string \"Cappiello \(Leonetto\)/Aux Trois Quartier/mmm\" \"/\" t\)\)"
  (let (tst-pth stack-pth f-pth)
    (setq tst-pth '(lambda (tst &optional in-sub) 
		  (if (and (not (file-symlink-p tst))
			   (or (file-exists-p tst) (file-directory-p tst)))
		      t
		    (cond (in-sub
			   (error "\nThe dir/file named `%s'\nIsn't in the path `%s/'" 
				  (file-name-nondirectory tst) in-sub))
			  ((not in-sub)
			   (error "\nThe dir/file named `%s' isn't in the path" 
				  tst in-sub))))))
    (setq stack-pth  '(lambda (in-pth sub-pth)
			(let* ((t-sub sub-pth)
			       (t-whole (concat (directory-file-name in-pth) "/" sub-pth)))
			  (when (funcall tst-pth t-whole in-pth)
			    (setq f-pth t-whole)))))
    (funcall stack-pth (directory-file-name expand-path) nil)
    (funcall stack-pth f-pth suffix-path)
    (when more-paths
      (let ((mr-pth more-paths))
	(while mr-pth
	  (let* ((nxt-sub (car mr-pth))
		 (parent f-pth)
		 (wlk-rst (funcall stack-pth parent nxt-sub)))
		 (if wlk-rst
		     (progn
		       (setq f-pth wlk-rst)
		       (setq mr-pth (cdr mr-pth)))
		   (setq mr-pth nil))))))
    f-pth))

;;;test-me;(mon-build-path *artist-naf-path* "C-Artists names" "Cappiello (Leonetto)")
;;;test-me;(mon-build-path *artist-naf-path* "C-Artists names")
;;;test-me;(apply 'mon-build-path *artist-naf-path* "C-Artists names"
;;;        (split-string "Cappiello (Leonetto)/Aux Trois Quartier/mmm" "/" t))
       
;;; ==============================
;;; COURTESY: Stefan Reichor, stefan@xsteve.at HIS: xsteve-functions.el VERSION: 2001-03-28
;;; MODIFICATIONS: <Timestamp: Monday February 09, 2009 @ 09:34.29 PM - by MON KEY>
;;; TODO: Make a global var for this and default to it. 
;;; TODO: Add post save hook to record the directory for new or modified .naf's.
(defun mon-save-current-directory ()
  "Save the current directory to a file.
\(concat mon-emacs-root \"/current-directory\")."
  (interactive)
  (let* ((current-sys-type (concat mon-emacs-root "/current-directory"))
	 (dir default-directory)
         (file-name (shell-quote-argument (expand-file-name dir))))
    (with-current-buffer (find-file-noselect current-sys-type)
   ;;;(delete-region (point-min) (point-max)) ;overwrites the file -as commented appends
     (when (equal system-type 'windows-nt)
      (setq file-name (replace-regexp-in-string "/" "\\\\" file-name)))
      (goto-char (point-max))
      (princ file-name (current-buffer)) ; (insert file-name)
      (newline)
      (save-buffer)
      (message "Saved directory '%s' to %s" file-name current-sys-type)
      (kill-buffer (current-buffer)))))

;;; ==============================
;;; MODIFICATIONS: <Timestamp: #{2009-08-11T18:12:53-04:00Z}#{09332} - by MON KEY>
(defun mon-save-current-directory-to-file (&optional intrp)
  "Save the current files directory path to a file. 
Default file is help by global var `*mon-record-current-directory*'."
  (interactive "p")
  (let* ((you-rang intrp) ;;(interactive-p))
	 (caller-wants (when (and you-rang)
			 (read-directory-name "Gimme a directory path:")))
	 (alt-file  (when (and you-rang)
		      ;;(read-file-name
		      (read-string
		       "Gimme a filename to hold this weight:"
		       ;;caller-wants
		       nil
		       (concat "naf-directory-list-" (format-time-string "%m-%d-%Y") ".dbc")
		       )))
	 (build-file (when (and you-rang)
		       (concat caller-wants alt-file)))
	 (current-sys-type (if (and you-rang)
			       build-file
                             ;;  (concat mon-emacs-root "/current-directory")))
                             *mon-record-current-directory*))
	 (dir default-directory)
	 (file-name (shell-quote-argument (expand-file-name dir))))
    (with-current-buffer (find-file-noselect current-sys-type)
      ;;(delete-region (point-min) (point-max)) ;overwrites the file -as commented appends
      (when (equal system-type 'windows-nt)
	(setq file-name (replace-regexp-in-string "/" "\\\\" file-name)))
      (goto-char (point-max))
      (princ file-name (current-buffer)) ; (insert file-name)
      (newline)
      (save-buffer)
      (message "Saved directory '%s' to %s" file-name current-sys-type)
      (kill-buffer (current-buffer)))))

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 11:38.18 AM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T11:38:50-0400Z - by MON KEY>
;;; REMOVED: Best I can see the (and * t) is totally pointless; removed it.
(defun mon-buffer-written-p (&optional insertp intrp)
  "True if current buffer has been written to a file or created with `find-file'
and _can_ be written in current directory (whether it has been or not).\n
See also: `mon-get-buffers-parent-dir', `mon-split-string-buffer-name'.
`mon-split-string-buffer-parent-dir', `file-exists-p'."
  (interactive "P\np")
  (let* ((written-p (buffer-file-name))
         ;;was: (and (buffer-file-name) t)) ;and w/ t is not needed! why was this here?
	 (has-or-not (if written-p "has or can be"  "_hasn't or can't_ be")))
    (when intrp
      (message "buffer `%s' %s written to file." (buffer-name) has-or-not))
    (when insertp 
      (insert (format "#Buffer `%s' %s written to file." (buffer-name) has-or-not)))
    written-p))

;;;test-me;(mon-buffer-written-p)
;;;test-me;(mon-buffer-written-p)
;;;test-me;(mon-buffer-written-p t)
;;;test-me;(call-interactively 'mon-buffer-written-p) 

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 11:50.56 AM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON KEY>
;;; ADDED: optional args insertp intrp
(defun mon-split-string-buffer-name (&optional insertp intrp)
  "Return current `buffer-name' as a list with split-string.
When INSERTP is non-nil or called-interactively with prefix arg insert the list
of split strings at point.\n
See also: `mon-get-buffers-parent-dir', `mon-get-buffers-parent-dir'
`mon-split-string-buffer-parent-dir', `mon-split-string-buffer-parent-dir-quick'."
  (interactive "P\np")
(let ((buf-split
      (if (mon-buffer-written-p)
	  (split-string (buffer-file-name) "/" t)
	(split-string default-directory "/" t))))
    (when intrp
      (message "%S" buf-split))
    (when insertp 
      (insert (format "%S" buf-split)))
    ;(message "%S" buf-split)
    buf-split))

;;;test-me;(mon-split-string-buffer-name)
;;;test-me;(mon-split-string-buffer-name t)
;;;test-me;(call-interactively 'mon-split-string-buffer-name)

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 08:17.10 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON KEY>
;;; ADDED: optional args insertp intrp
(defun mon-split-string-buffer-parent-dir-quick (&optional insertp intrp)
  "Like `mon-split-string-buffer-parent-dir' but with less checks.
See also `mon-get-buffers-parent-dir'."
  (interactive "P\np") 
(let ((sss-bpdq 
       (split-string (directory-file-name (expand-file-name "./"))"/" t )))
  (when insertp 
      (insert (format "%S" sss-bpdq)))
  sss-bpdq))

;;;test-me;(mon-split-string-buffer-parent-dir-quick)
;;;test-me;(mon-split-string-buffer-parent-dir-quick t)
;;;test-me;(call-interactively 'mon-split-string-buffer-parent-dir-quick)

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 12:31.43 PM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T11:48:58-0400Z - by MON KEY>
;;; ADDED: optional args insertp intrp
;;; REMOVED: (message "%S" rmvd) ;;message call is redundant
(defun mon-split-string-buffer-parent-dir (&optional insertp intrp)
  "Return buffers parent sans buffer's file name as a split-string list.
When `buffer-file-name' is nil return parents of buffers `default-directory'
\(inclusive=) as list of strings.\n
Like `mon-split-string-buffer-name' but does not strip tail of buffer's 
`default-directory' when `mon-buffer-written-p' is nil.
Unlike =(file-name-nondirectory buffer-file-name\) which does not check if buffer 
has a file name - throws an error instead.\n
Could also be accomplished with `mon-split-string-buffer-parent-dir-quick'.\n
e.g.:\n\(split-string \(directory-file-name \(expand-file-name \"./\"\)\)\"/\" t \)\n
See also: `mon-get-buffers-parent-dir'."
(interactive "P\np")
  (let* ((is-written (mon-buffer-written-p))
	 (l-mod (if is-written
		    (split-string (buffer-file-name) "/" t)
		  (split-string default-directory "/" t)))
	 (l-last  (if (and 
		       ;;file exists in dir
		       is-written  
		       ;;don't delete top level dir
		       (not (< (length l-mod) 1)))
		      ;; get count for deleting last string in path
		      (nth (- `,(length l-mod) 1) l-mod)
		    ;;don't remove objects eq nil below.
		    '()))		
	 (rmvd))
    (setq rmvd (remq l-last l-mod))
    (when insertp (insert (format "%S" rmvd)))
    ;;(message "%S" rmvd) ;;message call is redundant
    rmvd))

;;; test-me;(mon-split-string-buffer-parent-dir)
;;; test-me;(mon-split-string-buffer-parent-dir t)
;;; test-me;(call-interactively 'mon-split-string-buffer-parent-dir)

;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 11:28.41 AM - by MON KEY>
;;; MODIFICATIONS: <Timestamp: 2009-08-01-W31-6T11:26:49-0400Z - by MON KEY>
;;; ADDED: optional insertp, intrp args 
(defun mon-get-buffers-parent-dir (&optional full insertp intrp)
  "Returns buffers parent directory as a string.
By default returns buffer's parent directory _only_.
When FULL is non-nil return full path of buffers parent directory as string.
If we are in a buffer which has been written to a file or _can be_ return files
parent, else return parent of buffers `default-directory'.\n
When called-intereactively or INSERTP is non-nil insert buffers parent directory.
Could also be accomplished with: \n
\(car \(last \(split-string \(directory-file-name (expand-file-name \"./\"\)\) \"/\" t\)\)\)\n
But, not without the checks or a facility for sanity checks in programmatic
situations where `default-directory' of a non-written buffer may not evaluate to
what is expected. This is esp. the case where a calling function(s) has or might
`cd' to some alien path to do some stuff. We don't neccesarily want to blindly
write a buffer assuming that it will wind up in 'the' current directory.
It might not.\n
See also: `mon-buffer-written-p', `mon-split-string-buffer-name',
`mon-split-string-buffer-parent-dir', `mon-split-string-buffer-parent-dir-quick'."
  (interactive "i\ni\n\p")
  (let* ((is-written (mon-buffer-written-p))
	 (ret-buf-dir (if is-written
			  (if full
			      (directory-file-name (file-name-directory (buffer-file-name)))
			    (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
			(if full
			    (directory-file-name default-directory)
			  (file-name-nondirectory (directory-file-name default-directory))))))
    (if is-written
	(progn 
          (message "buffer: `%s' parent dir is `%s'."  (buffer-name) ret-buf-dir)
          (if (or insertp intrp)
              (insert ret-buf-dir)
            ret-buf-dir))
      (progn
	(message "buffer: `%s' not written to file yet, parent of buffer's default-directory is `%s'." 
		 (buffer-name) ret-buf-dir)
        (if (or insertp intrp)
            (insert ret-buf-dir)
          ret-buf-dir)))))

;;;test-me;(mon-get-buffers-parent-dir)
;;;test-me;(mon-get-buffers-parent-dir t)
;;;test-me;(mon-get-buffers-parent-dir t t)
;;;test-me;(mon-get-buffers-parent-dir nil t)
;;;test-me;(call-interactively 'mon-get-buffers-parent-dir)

;;; ==============================
;;; CREATED: <Timestamp: Friday May 29, 2009 @ 07:26.02 PM - by MON KEY>
(defun mon-truncate-path-for-prompt (&optional intrp)
  "Return a truncated path strog of current buffers path.
Useful for passing around to helper functions that prompt."
(interactive "p")
  (let* ((trunc-pth (directory-file-name (expand-file-name "./")))
	 (trunc-s (split-string trunc-pth "/"))
	 (trunc-l (length trunc-s))
	 (bld-lst))
    (setq bld-lst 
	  (cond ((>= trunc-l 3)(last trunc-s 3))
		((>= trunc-l 2)(last trunc-s 2))
		((>= trunc-l 1)(last trunc-s))))
    (setq bld-lst
	  (mapconcat 'identity bld-lst "/"))
    (if intrp
	(message "truncated path: %s" bld-lst)
      bld-lst)))

;;;test-me;(mon-truncate-path-for-prompt)
    
;;; ==============================
;;; CREATED: <Timestamp: Saturday May 23, 2009 @ 08:37.50 PM - by MON KEY>
(defun mon-walk-buff-or-dir-path (&optional alt-path rev)
  "Return a rercusively split list of directory paths as strings.
Use to walk up the directory or buffers path.
Default is to split buffer's current directory. 
When non-nil ALT-PATH is a directory name supplied as a string.
Throws an error if ALT-PATH doesn't exist.
When non-nil REV returns the result in reversed format - 
The REV arg is useful for faster comparison of two trees.\n
EXAMPLE:\n\(mon-walk-buff-or-dir-path\)"
  (interactive)
  (let* ((alt-p (if alt-path
		  (if (file-exists-p (directory-file-name alt-path))
		      (directory-file-name alt-path)
		    (error "path name provided doesn't exist"))
		  nil))
	 (walk-buf-dirs (if alt-p
			    (split-string alt-p "/" t)
			  (mon-split-string-buffer-parent-dir)))
	 (walking))
    (setq walking '())
    (while walk-buf-dirs
      (push (mapconcat 'identity walk-buf-dirs "/") 
	    walking)
      (setq walk-buf-dirs (nreverse walk-buf-dirs))
      (pop walk-buf-dirs)
      (setq walk-buf-dirs (nreverse walk-buf-dirs)))
    (if rev
	(nreverse walking)
      walking)))

;;;test-me;(mon-walk-buff-or-dir-path) 
;;;test-me;(mon-walk-buff-or-dir-path nil t)
;;;test-me(mon-walk-buff-or-dir-path (expand-file-name "~/"))
;;;test-me(mon-walk-buff-or-dir-path (expand-file-name "~/") t)

;;; ==============================
;;; CREATED: <Timestamp: Wednesday May 27, 2009 @ 04:28.57 PM - by MON KEY>
(defun mon-get-most-common-path (path-is path-in)
  "Given two paths return an acending list of the most common parents of two.
Comparison made as a test if PATH-IS (the unknown) has a common parent directory in
PATH-IN (target). 
Take the car of the list returned for the first deepest directory.
Take the last elt of the list returned for least deepest directory.\n
EXAMPLE:\n\(mon-get-most-common-path *ebay-images-bmp-path* *ebay-images-path*\)"
  (let ((path-a (mon-walk-buff-or-dir-path path-is t)) ;longer - reversed
	(path-b (mon-walk-buff-or-dir-path path-in t)) ;check if is-in - reversed
	(caught))
    (while (and path-a (not caught))
      (let* ((path-a-hd (car path-a))
	     (look (member path-a-hd path-b)))
	(when look (setq caught look)))
      (setq path-a (cdr path-a)))
    (when (not caught)
	(message "no common paths found for:\n%s and\n %s" path-is path-in))
      caught))

;;;test-me:(mon-get-most-common-path *ebay-images-bmp-path* *ebay-images-path*)

;;; =======================
;;; (URL `http://www.emacswiki.org/emacs/SubdirsToList')
(defun mon-add-subdirs-to-list (my-directory my-list)
  "Add all immediate subdirectories of `my-directory' to `my-list'.
More precisely, this uses only the subdirectories whose names start with
letters or digits; it excludes any subdirectory named `RCS' or `CVS', and any
subdirectory that contains a file named `.nosearch'.\n
See also; `mon-copy-files-in-sub-dirs', `mon-insert-subdirs-in-buffer'
`mon-path', `mon-copy-file-path', `mon-insert-path', 
`mon-get-buffers-directories', `mon-proc-buffers-directories',
`mon-get-proc-buffers-directories'."
  ;; This is a derivation of normal-top-level-add-subdirs-to-load-path see startup.el
  ;; and there is still some crud left from it.
  (let ((dirs)
    	(attrs)
    	(pending (list my-directory))
    	(subdirs-inode-list)
	(my-add-subdirs-inode-list))
    (let* ((this-dir (car pending))
    	   (contents (directory-files this-dir))
    	   (default-directory this-dir)
    	   (canonicalized (and (eq system-type 'windows-nt)
    			       (untranslated-canonical-name this-dir))))
      ;; The Windows version doesn't report meaningful inode
      ;; numbers, so use the canonicalized absolute file name of the
      ;; directory instead.
      (setq attrs (or canonicalized
		      (nthcdr 10 (file-attributes this-dir))))
      (unless (member attrs subdirs-inode-list)
	(setq my-add-subdirs-inode-list
	      (cons attrs subdirs-inode-list))
	(while contents
	  ;; The lower-case variants of RCS and CVS are for DOS/Windows.
	  (unless (member (car contents) '("." ".." "RCS" "CVS" "rcs" "cvs"))
	    (when (and (string-match "\\`[[:alnum:]]" (car contents))
		       ;; Avoid doing a `stat' when it isn't necessary
		       ;; because that can cause trouble when an NFS server
		       ;; is down.
		       (not (string-match "\\.elc?\\'" (car contents)))
		       (file-directory-p (car contents)))
	      (let ((expanded (expand-file-name (car contents))))
		(unless (file-exists-p (expand-file-name ".nosearch"
							 expanded))
		  (setq dirs (nconc dirs (list expanded)))))))
	  (setq contents (cdr contents)))))
    (list my-list)
    (dolist (my-dir dirs)
      (add-to-list my-list my-dir)))
  (eval my-list)) 

;;; ================================================================
;;; (ULR `http://www.emacswiki.org/emacs/SubdirsToList') - no-author.
;;; MODIFICATIONS: <Timestamp: Tuesday February 17, 2009 @ 05:50.26 PM - by MON KEY>
(defun mon-insert-subdirs-in-buffer (&optional pth-to-l)
  "Insert at point the top-level subdirs found in PTH-TO-L. 
PTH-TO-L is nil or called-interactively prompt for a path name.
Ignores dirs with .nosearch. 
CALLED-BY; `mon-add-subdirs-to-list'.
See also; `mon-copy-files-in-sub-dirs'.
See alternative implementation using; `mon-get-buffers-directories', 
`mon-get-proc-buffers-directories', `mon-proc-buffers-directories',
`mon-cln-blank-lines'."
  (interactive)
  (save-excursion
    (let* ((path-list (if (and pth-to-l)
			  pth-to-l
			(read-directory-name "Gimme a directory path:")))
	   (hold-list ())
	   (get-list (mon-add-subdirs-to-list path-list 'hold-list))
	   (to-print get-list))
      (while to-print
	(progn
	  (newline)
	  (princ (car to-print) (current-buffer)))
	(setq to-print (cdr to-print))))))

;; ==============================
;;; COURTESY: Thierry Volpiatto HIS: thumb-page.el WAS: `tv-serial-rename'
;;; (URL `http://www.emacswiki.org/emacs/SerialRename')
;;; CREATED: <Timestamp: Sunday April 05, 2009 @ 01:14.27 PM - by MON KEY>
(defun mon-serial-rename (dir ext name start)
  "Rename sequentially a set of file(s) with extension EXT in a directory DIR
with NAME starting from number START."
  (interactive "fDir: \nsExt(no dot): \nsName: \nnStart: ")
  (find-file dir)
  (let (ls-dir new-ls-dir n c)
    (setq ls-dir (file-expand-wildcards (format "*.%s" ext) t))
    (setq new-ls-dir nil)
    (setq n 0)
    (while (< n (length ls-dir))
      (if (< start 10)
	  (push (concat dir name (format "0%s" start) "." ext) new-ls-dir)
	(push (concat dir name (format "%s" start) "." ext) new-ls-dir))
      (setq start (+ start 1))
      (setq n (+ n 1)))
    (setq ls-dir (reverse ls-dir))
    (setq c 0)
    (dolist (i ls-dir)
      (rename-file i (nth c new-ls-dir))
      (setq c (+ c 1)))))

;;;test-me;(mon-serial-rename )

;;; ==============================
;;; NOTE: (getenv "TEMP")
;;;    (map-file-lines f "/tmp/test" (lambda (line num) (message "%d: %s" line)))
;;; COURTESY: Ted Zlatanov <tzz@lifelogs.com>
;;; Message-ID: <86wsc87o3c.fsf@lifelogs.com>
;;; emacs-devel Date: Mon, 02 Feb 2009 11:20:07 -0600
(defun mon-map-file-lines (file func &optional startline count bufsize)
  "Iterate over all the lines of ARG file with ARG func.
Read a block of data in one shot processing it linewise.
Intended for use with _BIG_ files.\nEXAMPLE:\n
\(map-file-lines \"/tmp/test\" \(lambda \(line num\) \(message \"%d: %s\" line\)\)\)"
  (let ((filepos 0)
        (linenum 0)
        (bufsize (or bufsize (* 128 1024))))
    (with-temp-buffer
      (while
          (let*
              ((inserted (insert-file-contents
                          file nil
                          filepos (+ filepos bufsize) 
                          t))
               (numlines (count-lines (point-min) (point-max)))
               (read (nth 1 inserted))
               (done (< 1 read))
               result line-end)
            (dotimes (n (count-lines (point-min) (point-max)))
	      (goto-char (point-min))
	      (setq line-end (line-end-position)
		    result (if (and startline (< linenum startline))
			       ()
			     (if (and count (>= (- linenum startline) count))
				 (return)
			       (funcall func 
					(buffer-substring 
					 (line-beginning-position)
					 line-end)
					linenum)))
		    done (and done result))
	      (incf filepos line-end)
	      (forward-line)
	      (incf linenum))
            done)))
    linenum))

;;;test-me;(map-file-lines "/tmp/test" (lambda (line num) (message "%d: %s" num line)))
;;;test-me;(map-file-lines "/tmp/test" (lambda (line num) (message "%d: %s" num line)) 100)
;;;test-me;(map-file-lines "/tmp/test"(lambda (line num) (message "%d: %s" num line)) 100 10)

;;; ==============================
;;; CREATED: <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON KEY>
(defun naf-explorer-artist (prefix)
  "Open an w32 explorer window in alphabetic NAF drive Artists Folder.
PREFIX is a one letter string A-Z.
When called-interactively, prompt for an alphabetic directory PREFIX.
Default path held by global var: `*artist-naf-path*'.
See also; `naf-explorer-brand',`naf-dired-artist-letter',
`naf-dired-brand-letter', `mon-open-explorer'.\nUsed in `naf-mode'."

  (interactive "p")
  (when (and win32p)
    (let ((dl (format "%s-Artists names"
                       (if (numberp prefix)
		       (upcase (read-string "Alphabetic Artists Directory:"))
                       (upcase prefix))))
	   (naf-alph-path))             ;;win32 explorer needs this backslashed
      (setq naf-alph-path (concat *artist-naf-path* "/" dl))
      (setq naf-alph-path (subst-char-in-string 47 92 naf-alph-path))
      (w32-shell-execute  "open" "explorer" (concat "/e, " naf-alph-path)))))

;;;test-me;(naf-explorer-artist "b")

;;; ==============================
;;; CREATED: <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON KEY>
(defun naf-explorer-brand (prefix)
 "Open an w32 explorer window in alphabetic NAF drive Brand Folder.
PREFIX is a one letter string A-Z.
When called-interactively, prompt for an alphabetic naf brand directory.
Default path held in var: `*brand-naf-path*'.
See also; `naf-explorer-artist',`naf-dired-artist-letter',
`naf-dired-brand-letter', `naf-dired-image-dir'`mon-open-explorer'.
Used in `naf-mode'."
  (interactive "p")
  (when (and win32p)
    (let* ((dl (format "%s-Brand-NAFs"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Artists Directory:"))
                         (upcase prefix))))
	   (naf-alph-path))             ;;win32 explorer needs this backslashed
      (setq naf-alph-path (concat *brand-naf-path* "/" dl))
      (setq naf-alph-path (subst-char-in-string 47 92 naf-alph-path))
      (w32-shell-execute  "open" "explorer" (concat "/e, " naf-alph-path)))))

;;;test-me;(naf-explorer-brand "b")

;;; ==============================
;;; TODO: figure out how to take a second argument that heuristically
;;;       puts point at an appropriate location in the returned dired buffer.
;;; CREATED: <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON KEY>
(defun naf-dired-artist-letter (prefix)
  "Dired visit the alphabetic Artist naf-directory by letter PREFIX. 
PREFIX is a one letter string A-Z. 
When Called interactively prompts for:\"Alphabetic Brand Directory :\" 
concatatenating \"LETTER-Artist names/\" to the default naf path.
Default naf path held by the var: `*artist-naf-path*'.\n
EXAMPLE:\n(naf-dired-artist-letter \"b\")\n
See also; `naf-explorer-artist',`naf-explorer-brand', `naf-dired-brand-letter',
`naf-dired-image-dir', `mon-open-explorer'.\nUsed in `naf-mode'."
  (interactive "p")
  (let* ((dl (format "/%s-Artists names/"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Artists Directory :"))
                         (upcase prefix))))
	 (naf-alph-path (concat *artist-naf-path* dl))
	 (default-directory naf-alph-path))
    (dired-other-window naf-alph-path)))

;;;test-me;(naf-dired-artist-letter "b")

;;; ==============================
;;; CREATED: <Timestamp: Monday February 09, 2009 @ 09:35.31 PM - by MON KEY>
(defun naf-dired-brand-letter (prefix); &optional intrp)
  "Dired the alphabetic Brand naf-directory by letter PREFIX. 
PREFIX is a one letter string A-Z.
When Called interactively prompts for:\"Alphabetic Brand Directory :\" 
concatatenating \"LETTER-Brand-NAFS/\" to the default naf path.
Default naf path held by the var: `*brand-naf-path*'.\n
EXAMPLE:\n(naf-dired-brand-letter \"b\")\n
See also; `naf-dired-artist-letter', `naf-explorer-artist',
`naf-explorer-brand', `naf-dired-image-dir', `mon-open-explorer'.\n
Used in `naf-mode'."
  (interactive "p")
  (let* ((dl (format "/%s-Brand-NAFs/"
                       (if (numberp prefix)
                           (upcase (read-string "Alphabetic Brand Directory :"))
                         (upcase prefix))))
	 (naf-alph-path (concat *brand-naf-path* dl))
	 (default-directory naf-alph-path))
    (dired-other-window naf-alph-path)))

;;;test-me;(naf-dired-brand-letter  "b")

;;; ==============================
;;; CREATED: <Timestamp: Thursday June 25, 2009 @ 06:03.54 PM - by MON KEY>
(defun naf-dired-image-dir (pth-nm &optional intrp)
  "Dired to an image directory.
PTH-NM is an image directory udner one of the following strings:
\"nefs-archived\", \"nefs-working\", \"ebay-bmp\", \"ebay-jpg\".
These are bound as alist keys in the vars: `*nef-scan-nefs-path*', 
`*nef-scan-nef2-path*',`*ebay-images-bmp-path*', `*ebay-images-jpg-path*'
When Called interactively complete the key to dired to the directory val.\n
EXAMPLE:\n(naf-dired-image-dir \"ebay-bmp\")\n
See also; `naf-dired-artist-letter', `naf-explorer-artist', 
`naf-explorer-brand', `mon-open-explorer', `mon-dired-nef-dir',
`mon-nef-dir-big', `*nefs_photos_nefs-alist*'."
(interactive "p\nP")
(let* ((img-pths 
        (pairlis  ;; CL 
         '("nefs-archived" "nefs-working" "ebay-bmp"  "ebay-jpg")               
         `(,*nef-scan-nefs-path* ,*nef-scan-nef2-path* ,*ebay-images-bmp-path* ,*ebay-images-jpg-path*)))
       (in-d (cond (intrp (completing-read "which path :" img-pths nil t))
                   ((assoc pth-nm img-pths) pth-nm)
                   ((not (assoc pth-nm img-pths))
                    (completing-read "which path :" img-pths nil t))))
      (to-d (cdr (assoc in-d img-pths))))
  ;; complete on each directories of nefs-archived with a cached alist:
  (if (string= in-d "nefs-archived")
      (let ((nef-alist (mon-nef-dir-big *nefs_photos_nefs-alist*))
            (get-nef-dir))
        (setq get-nef-dir
              (cadr (assoc-string 
                     (completing-read "Which nef directory (tab-completes):" nef-alist) 
                     nef-alist)))
        (dired (concat *nef-scan-nefs-path* "/" get-nef-dir)))
    (dired (cdr (assoc in-d  img-pths))))))

;;;test-me(naf-dired-image-dir "nefs-archived")

;;; ==============================
;;; DEPRECATE: use `mon-copy-file-path'
(defun mon-path (&optional intrp)
  "DEPRECATE: use `mon-copy-file-path'
Returns the current file path as a message.
Unlike `mon-copy-file-path' path doesn't copy to file's path kill ring.
See also; `mon-copy-file-path', `mon-insert-path', `mon-add-subdirs-to-list'." 
  (interactive "p")
(if intrp
  (message "%s" buffer-file-name)
  (buffer-file-name)))

;;;test-me;(mon-path)
;;;test-me;(mon-path t)
;;;test-me;(call-interactively 'mon-path)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: Tuesday July 21, 2009 @ 05:19.11 PM - by MON KEY>
(defun mon-copy-file-path (&optional insertp intrp)
  "Copy current buffer's file path to kill-ring. Returns path value as message.
When INSERTP is non-nil or called with prefix arg insert path at point.
See also; `mon-insert-path', `mon-path', `mon-add-subdirs-to-list',
`mon-copy-files-in-sub-dirs'."
  (interactive "P\np")
  (let (scfp)
    (if (and (mon-buffer-written-p)
              (file-readable-p (buffer-file-name)))
        (progn
          (setq scfp (buffer-file-name))
          (kill-new (buffer-file-name))
          (when (or intrp (not insertp))
            (progn
              (message "The-path-is: %s" scfp)
              (sit-for 1))))
      ;;else
      (progn
        (setq scfp (buffer-name))
        (kill-new (format "#P/not-written/not-readable/not-exisistent/buffer-name-is/%s" (buffer-name)))
        (cond ((or intrp (not insertp))
               (message "The path doesn't exist buffer-name is: %s" (buffer-name))
               (sit-for 1))
              ((and insertp (not intrp))
              (setq scfp (format "#P/not-written/not-readable/not-exisistent/buffer-name-is/%s" (buffer-name)))
               ))))
    (if insertp (insert scfp))))

;;;test-me;(mon-copy-file-path)
;;;test-me;(mon-copy-file-path t )
;;;test-me;(mon-copy-file-path t t)
;;;test-me;(call-interactively 'mon-copy-file-path)

;;; ==============================
;;; MODIFICATIONS: <Timestamp: Tuesday July 21, 2009 @ 05:21.05 PM - by MON KEY>
 (defun mon-insert-path ()
   "DEPRECATE: use `mon-copy-file-path'\nInserts current file's path at point.\n
See also; `mon-copy-file-path', `mon-path', `mon-add-subdirs-to-list',
`mon-copy-files-in-sub-dirs'."
   (interactive)
   (mon-copy-file-path t))

;;;test-me;(mon-insert-file-path)

;;; ==============================
;;; CREATED: <Timestamp: Friday May 08, 2009 @ 12:12.46 PM - by MON KEY>
(defun mon-get-buffers-directories (&optional opt-dir)
  "Return buffer list for buffer' directories sub-dirs.
CALLED-BY: `mon-proc-buffers-directories', `mon-get-proc-buffers-directories',
`mon-cln-blank-lines'.\nCALLS-VARIABLE: `*nef-scan-path*'.
For alternative implementation See also; `mon-insert-subdirs-in-buffer',
`mon-add-subdirs-to-list'."
  (let ((get-dirs)
	(df-dir (file-name-as-directory (file-name-directory default-directory)))
	(this-dir))
    (cond ((and (not (buffer-file-name)) (not opt-dir))
	   (if (yes-or-no-p "Supply an alternate directory path: ")
	       (setq this-dir (file-name-directory 
			       (file-name-as-directory
				(read-directory-name "List subdirs of dir :" *nef-scan-path* df-dir nil t))))
	    (setq this-dir df-dir)))
	    ;;(error "Can't call from an unsaved-buffer. Buffer `%s' is not associated with a directory" (buffer-name))))
	  (opt-dir 
	   (if (file-exists-p (file-name-directory (file-name-as-directory opt-dir)))
	       (setq this-dir (file-name-directory  (file-name-as-directory opt-dir)))
	     (error "Directory doesn't exist, is not readable, or is a file")))
	  ((and (not opt-dir) (buffer-file-name))
	   (setq this-dir (file-name-as-directory (file-name-directory buffer-file-name)))))
    (setq get-dirs
          (with-temp-buffer
	    (insert-directory this-dir "-gloRBd --dired a" nil t)
	    (buffer-string)))
    get-dirs))

;;; ==============================
;;; CREATED: <Timestamp: Thursday May 07, 2009 @ 08:24.16 PM - by MON KEY>
(defun mon-proc-buffers-directories (&optional opt-dir) 
  "Return directory list of buffers' directories sub-directories.
CALLED-BY:: `mon-get-buffers-directories', `mon-get-proc-buffers-directories',
`mon-cln-blank-lines'.\nFor alternative implementation See also;\n
`mon-insert-subdirs-in-buffer', `mon-add-subdirs-to-list'
See also; `mon-copy-files-in-sub-dirs'."
  (let ((proc-result)
	(to-proc))
    (setq to-proc (mon-get-buffers-directories opt-dir)) 
    (setq proc-result
	  (with-temp-buffer
	    (insert to-proc)
	    (let* ((mrkr-start)
		   (mrkr-end)
		   (mrkr-start (make-marker))
		   (mrkr-end (make-marker)))
	      ;;Need to check here if there are actually any directories in this path
	      ;;"^drwxrwxrwx  1 Everyone Everyone        0 05-15 16:14 "\\([^\.+]\\)
	      (goto-char (point-min))
	      (set-marker mrkr-start (progn (search-forward "total used in directory") (point-at-bol)))
	      (set-marker mrkr-end (point-max))
	      (let* ((start-point (marker-position  mrkr-start)) ;(start-point (point))
		     (end-point (marker-position  mrkr-end))
		     (dirs '("^\\(-.*\\)$" "^\\(total .*\\)$" "^\\(d.*\\)$" ":$"))
		     (dir-swp))
		(while dirs
		  (goto-char start-point)
		  (setq dir-swp (car dirs))
		  (while (search-forward-regexp dir-swp nil t)
		    (replace-match ""))
		  (setq dirs (cdr dirs)))))
	    (goto-char (point-min))
	    (while (search-forward-regexp "^$" nil t)
	      (replace-match "#"))
	    (goto-char (point-min))
	    (while (search-forward-regexp "^#$" (point-max) t)
	      (cond ((and (eolp) (not (bobp))
			  (not (not (char-before)))
			  (not (not (char-before (1- (point))))))
		     (if (and (= (char-before) 35)
			      (= (line-beginning-position) (1- (point)))
			      (= (char-before (1- (point))) 10)
			      (not (= (char-before (- (point) 2)) 10)))
			 (delete-char -2)))))
	    (goto-char (point-min))
	    (progn
	      (search-forward-regexp "^#$" nil t)
	      (forward-char 1)
	      (delete-char -2))
	    (buffer-string)))
    proc-result)) ;;(insert proc-result)))


;;;(progn (makunbound ''mon-proc-buffers-directories) (unintern 'mon-proc-buffers-directories)   

;;; ==============================
;;; NOTE: Can `subst-char-in-string' be used here instead?
;;; Did this function get screwed up at commit 533:5c6419be867a ?
;;; I think it is fixed, but...
;;; CREATED: <Timestamp: Thursday May 07, 2009 @ 09:40.22 PM - by MON KEY>
(defun mon-get-proc-buffers-directories (&optional intrp opt-dir) 
  "Return dir list of buffer files directories. 
Directory list acquired with: `mon-get-buffers-directories' and
`mon-proc-buffers-directories'. List is cleaned with`mon-cln-blank-lines'.
For alternative implementation See also; `mon-insert-subdirs-in-buffer',
`mon-add-subdirs-to-list'\nSee also; `mon-copy-files-in-sub-dirs'."
  (interactive "p")
  (let ((result) (proc))
    (setq proc (mon-proc-buffers-directories opt-dir))
    (setq result (with-temp-buffer
		   (insert proc)
		   (mon-cln-blank-lines (point-min) (point-max))
		   (let* ((file-n (bounds-of-thing-at-point 'filename))
			  (bound-s (car file-n))
			  (bound-e (cdr file-n))
			  (file-ns (buffer-substring bound-s bound-e))
			  (file-list) (f-name))
                     ;;(save-excursion
		     (goto-char (point-max))
		     (while (mon-spacep) 
		       (delete-char -1))
		     (goto-char (point-min))
		     (while (mon-spacep nil t)
		       (delete-char 1))
		     (goto-char (point-min))
		     (mon-cln-spc-tab-at-eol-in-region (point-min) (point-max))
		     (goto-char (point-min))
		     (while (search-forward-regexp " " (point-max) t) ;;best-to-seperate-to dedicated-function
		       (if  (not (= (line-end-position)(car (cddddr (mon-line-test-content 'whitespace t)))))
			   (progn    
			     (beginning-of-line)
			     (search-forward-regexp 
			      "\\(\\(\\([[:alnum:]]\\)\\([[:space:]]\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" nil t)
			     (replace-match "\\3_#_~_#_\\5"))))
		     (goto-char (point-min))
		     (setq file-list ())
		     (while (not (eobp))
		       (setq f-name (thing-at-point 'filename))
		       (cond ((or (mon-line-bol-is-eol) (mon-spacep-is-bol))
			      (forward-thing 'line))
			     ((string-match  "\\(\\(\\([[:alnum:]]\\)\\(_#_~_#_\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" f-name)
			      (setq f-name (replace-regexp-in-string 
					    "\\(\\(\\([[:alnum:]]\\)\\(_#_~_#_\\)\\([[:alnum:]]\\)\\)\\{1,1\\}\\)" 
					    "\\3 \\5"  f-name)))
			     (t f-name))
		       (setq file-list (cons f-name file-list))	;;(thing-at-point 'filename) file-list)))
		       (forward-thing 'line))
		     (setq file-list (nreverse file-list)) ;;(newline) ;;(prin1 file-list (current-buffer)))
		     file-list)))
    (if intrp 
	(progn 
	  (newline)
	  (prin1 result (current-buffer)))
      result)))

;;;test-me;(progn (newline)(prin1 (mon-get-proc-buffers-directories) (current-buffer)))

;;; ==============================
;;; CREATED: <Timestamp: Thursday May 21, 2009 @ 08:06.42 PM - by MON KEY>
(defun mon-build-dir-list (dir &optional not-concat-path)
  "Return a _list_ of directories in DIR.
When non-nil NOT-CONCAT-PATH returns a list _without_ the leading path.
See also; `mon-try-comp-dir', `mon-complete-hashed-dir', `""', 
`mon-hash-img-dir'."
(save-excursion
    (save-window-excursion
      (let ((temp-string)
	    (curr-buff (get-buffer (current-buffer)))
	    (in-dir dir)
	    (rtn-dir))
	(setq temp-string    
	      (with-temp-buffer
		(let ((this-buff)
		      (that-buff)
		      (ss))
		  (setq this-buff (get-buffer (current-buffer)))
		  (list-directory dir t)
		  (setq that-buff (get-buffer "*Directory*"))
		  (set-buffer that-buff)
		  (setq ss (buffer-substring-no-properties (point-min) (point-max)))
		  (set-buffer this-buff)
		  (kill-buffer that-buff)
		  (insert ss)
		  (goto-char (point-min))
		  (keep-lines "^d.*[0-9][0-9]:[0-9][0-9] .*$")
		  (goto-char (point-min))
		  (while
		      (search-forward-regexp "\\(\\(^d.*[0-9][0-9]:[0-9][0-9][[:space:]]\\)\\(.*$\\)\\)" nil t)
		    (replace-match "\\3" ))
		  (mon-cln-trail-whitespace)
		  (goto-char (point-min))
		  (while (search-forward-regexp "^\\(.*\\)$" nil t)
		    (if (and (mon-line-bol-is-eol) (not (eobp)))
			(delete-char 1)
		      (replace-match "\\1|")))
		  (while (search-backward-regexp "^\|$" nil t)
		    (if (= (char-after) 124)
		      (delete-char 1)))
		  (goto-char (point-min))
		  (mon-delete-back-up-list (point-min) (point-max))
		  (buffer-substring-no-properties (point-min) (point-max)))))
	(set-buffer curr-buff)
	(setq rtn-dir
	      (split-string temp-string "| "))
	(setq rtn-dir (delete "" rtn-dir))
	(if (not not-concat-path)
	    (setq rtn-dir
		  (let ((map-dir rtn-dir)
			(conc-dir (concat in-dir "/")))
		    (mapcar '(lambda (x) (concat conc-dir x)) map-dir)))
	  rtn-dir)
	;;(prin1 rtn-dir (current-buffer))
	rtn-dir))))

;;;test-me;(mon-build-dir-list mon-emacs-root)
;;;test-me;(mon-build-dir-list mon-emacs-root t)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:01.56 PM - by MON KEY>
(defun mon-update-nef-photos-alist ()
  "Refresh the alist contents in  var `*nefs_photos_nefs-alist*'. 
Called-by: `mon-dired-nef-dir', `mon-nef-dir-big'.
Assumes directories of `*nef-scan-nefs-path*' formatted as one of following:\n
NNN_Name-of-folder_(NNNN-NNNN)\n1...2*.............3..........\n
NNN_(NNNN-NNNN)\n1...3..........\n
NNN_Name-of-folder_(NNNN-NNNN, NNNN,NNNN,NNNN)
1...2*.............3.........................\n
NNN_(NNNN-NNNN, NNNN)\n1...3................\n
1) [Image-directory-number
    :Integer {3}
    :Underscore: \"_\" char: 95]\n
2) [Folder-name-string                       *Optional
    :Namestring ::= [A-z0-9,]
    :Namestring-delimiter ::= \"-\" char: 45]\n
3) [Image-Range
    :Underscore \"_\" char: 95
    :Opening-Paren ::= char: 40
    :Integer ::= {1,5}
    :Range-delimter ::= delimit with \"-\" char: 45
    :Multi-range ::= delimit second with \", \"  char: 44 char 32
    :Subsequent-multi-ranges ::=  \",=\" char: 44
    :Integer ::= {2,5}]\n
EXAMPLE: If directory Contains the two folders:
\"001_Lesson-Humming-Brds_\(1-21\)\"\n\"242_\(12390-12400\)\"\n
Return an alist as:
\(\(\"001_Lesson-Humming-Birds_\(1-21\)\" \"001\" \"Lesson-Humming-Birds\" \"\(1-21\)\"\)
\(\"242_\(12390-12400\)\" \"242\" \"\(12390-12400\)\)\"\)\)"
  (mapcar (lambda (x) 
            (let  (split split-name)
              (setq split (split-string x "_" ))
              (setq split-name
                    (cond ((= (length split) 3) 
                           `(,x
                             ,(car split)
                             ,(subst-char-in-string 45 32 (cadr split))
                             ,(caddr split)))
                          ((= (length split) 2)
                           `(,x ,(car split) ,(cadr split))) 
                          ;;we should never see this
                          (t `(,x ,split))))
              split-name))
          (directory-files *nef-scan-nefs-path* nil "^\\([0-9]\\{2,3\\}_.*\\)")))

;; (let ((new-alist (mon-update-nef-photos-alist)))
;;   (setq *nefs_photos_nefs-alist* new-alist))

;;; DON'T SNARF IF ON REMOTE MACHINES
(cond ((or IS-MON-P-W32 IS-BUG-P)
       (when (not (bound-and-true-p *nefs_photos_nefs-alist*))
         (let ((new-alist (mon-update-nef-photos-alist)))
           (setq *nefs_photos_nefs-alist* new-alist)))))


;;;(progn (makunbound '*nefs_photos_nefs-alist*) (unintern '*nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-ranges (folder-alist)
  "Return FOLDER-ALIST folder ranges as two string alist with range in
head position. Parens are stripped from ranges.\nEXAMPLE:\n
\(mon-nef-dir-ranges  *nefs_photos_nefs-alist*\)\n
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',`mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'."
  (let (range>)
    (setq range>
          (mapcar (lambda (x) 
                    (let* ((this-x (assoc (car x) folder-alist))
                           (len (length x))
                           (pth (car this-x))
                           (range (cond ((= len 3)(caddr this-x))
                                        ((= len 4)(cadddr this-x)))))
                      (setq range (replace-regexp-in-string "(" "" range)
                            range (replace-regexp-in-string ")" "" range))
                      `(,range ,pth)))
                  folder-alist))  
    range>))

;;;test-me;(mon-nef-dir-ranges  *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-fldr (folder-alist)
  "Return FOLDER-ALIST as two string alist. 
Return with the directory name in head position.\n
EXAMPLE:\n\(mon-nef-dir-fldr *nefs_photos_nefs-alist*\)\n
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-conc-ranges',`mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt', 
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'."
  (let (folder>)
    (setq folder> 
          (mapcar (lambda (x) 
                    (let* ((this-x (assoc (car x) folder-alist))
                           (pth (car this-x))
                           (fld-num (cadr this-x)))
                      `(,fld-num ,pth)))
                  folder-alist))  
    folder>))
    
;;;test-me;(mon-nef-dir-fldr *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-rmv-empt (folder-alist)
  "Return FOLDER-ALIST folder names as string as two string alist. 
Return with empty `nil' folders removed, e.g. folders formatted as:\n
NNN_(NNNN-NNNN)\n1...3..........\n
This is required before we can filter out duplicate folders named with
`mon-nef-dir-find-dups' otherwise nil entries will appear as well.\n
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups', `*nefs_photos_nefs-alist*',
`*nef-scan-nefs-path*'."
  (let (freename>1)
    (setq freename>1
          (mapcar (lambda (x) 
                    (let* ((this-x (assoc (car x) folder-alist))
                           (len (length x))
                           (pth (car this-x))
                           (free (third this-x)))
                      (when (= len 4)
                        `(,free ,pth))))
                  folder-alist)) 
    (setq freename>1 (delq '() freename>1))))

;;;test-me(mon-nef-dir-rmv-empt *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-find-dups (folder-alist)
  "Find duplicated folder names in the alist generated with 
`mon-nef-dir-rmv-empt' and rotate tail-position to head-position.\n
EXAMPLE:\n\(mon-nef-dir-find-dups *nefs_photos_nefs-alist*\)\n
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups',`*nefs_photos_nefs-alist*',
 `*nef-scan-nefs-path*'."
  (let (comp freename>2) 
    (setq comp (mon-nef-dir-rmv-empt folder-alist))
    (mapcar (lambda (free)
              (let* ((free-hd (car free))
                     (free-tl (cadr free))
                     (match (assoc free-hd comp)))
                (when (and match (not (string= free-tl (cadr match))))
                  (when (not (member match freename>2))
                    (setq freename>2 (cons match freename>2)))
                  (when (not (member free freename>2))
                    (setq freename>2 (cons free freename>2))
                    (setq comp (delq match comp))))))
            comp)
    (setq freename>2 (mapcar (lambda (x) (nreverse x))  freename>2))))

;;;test-me;(mon-nef-dir-find-dups *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-conc-dups (folder-alist) ;*nefs_photos_nefs-alist*
  "Concat directory name identifiers (integer) onto duplicate directory names in
alist generated with `mon-nef-dir-find-dups'. Each folder with a namestring 
matching an existing namestring needs a unique name so build it.
EXAMPLE:\n\(mon-nef-dir-conc-dups *nefs_photos_nefs-alist*\)\n
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-rmv-empt',`*nefs_photos_nefs-alist*',
`*nef-scan-nefs-path*'."
  (let ((freename>4 (mon-nef-dir-find-dups folder-alist))
        (fldrname> ;;bind mon-nef-dir-fldr in wrapping defun
         (mapcar (lambda (x) (nreverse x)) (mon-nef-dir-fldr folder-alist)))
        (freename>3))
    (mapcar (lambda (x) 
              (let ((mk-new (assoc (car x) fldrname>))
                    (new))
                (when mk-new 
                  (setq new  `(,(car mk-new) 
                               ,(concat  
                                 (cadr x)
                                 " |In Folder-> "
                                 (cadr mk-new))))
                  (delq (assoc (car x) freename>3) freename>3)
                  (setq freename>3 (cons new freename>3)))))
            freename>4)
    freename>3))

;;;test-me;(mon-nef-dir-conc-dups *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-converge (folder-alist) 
  "Return FOLDER-ALIST with renamed unambiguated duplicate directory names.
Directory names of FOLDER-ALIST are folded back into the surronding alist.
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-keep-3',
`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges', `mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups', `mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'."
  (let ((big-dupd  
         (mapcar (lambda (x) 
                   (nreverse x)) (mon-nef-dir-rmv-empt folder-alist)))
        (dups-only (mon-nef-dir-conc-dups folder-alist))
        (no-dups))
    (mapcar (lambda (x)
              (let* ((is-dup (car x))
                     (old-dup (assoc is-dup big-dupd)))
                (when old-dup 
                  (setq big-dupd (delq old-dup big-dupd))
                  (setq big-dupd (cons x big-dupd)))))
            dups-only)
    (setq no-dups
          (sort big-dupd 
                (lambda (x y) (string< (car x) (car y)))))
    (setq no-dups 
          (mapcar (lambda (x) (nreverse x))  no-dups))
    no-dups))

;;;test-me(mon-nef-dir-converge *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-conc-ranges (folder-alist) ;*nefs_photos_nefs-alist*
  "Returns FOLDER-ALIST as two string alist with the directory name in head position.
followed by its range.\nSee also; `mon-dired-nef-dir', `mon-nef-dir-big',
`mon-nef-dir-converge', `mon-nef-dir-keep-3',`mon-nef-dir-fldr',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',
`mon-nef-dir-rmv-empt', `*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'."
  (let ((not-empt (mapcar (lambda (x) (nreverse x)) (mon-nef-dir-rmv-empt folder-alist)))
        (ranges (mapcar (lambda (x) (nreverse x)) (mon-nef-dir-ranges  folder-alist)))
        (rangename>))
    (mapcar  (lambda (x)
               (let ((mk-rngnm (assoc (car x) not-empt))
                     (new))
                 (when mk-rngnm 
                   (setq new `(,(concat  
                                 (cadr mk-rngnm) 
                                 " |In-Range-> " 
                                 (cadr x)) 
                               ,(car mk-rngnm)))
                   (setq rangename> (cons new rangename>)))))
             ranges)
    rangename>))

;;;test-me;(mon-nef-dir-conc-ranges *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-keep-3 (folder-alist)
  "Return FOLDER-ALIST folder names as string as two string alist.
Retrun FOLDER-ALIST names identified as two elt 'empty' `nil' directories,
e.g. those formatted as:\nNNN_(NNNN-NNNN)\n1...3..........\n
These were removed from surrounding alist by `mon-nef-dir-rmv-empt' for use by
`mon-nef-dir-find-dups'. We need them back, so get them.\n
EXAMPLE:\n\(mon-nef-dir-keep-3 *nefs_photos_nefs-alist*\)\n
\nSee also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',`mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',`mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'."
  (let* ((l1 (mapcar (lambda (x) (cadr x)) (mon-nef-dir-rmv-empt folder-alist))) 
         (l2 (mapcar (lambda (x) (nreverse x)) (mon-nef-dir-ranges folder-alist)))
         (l3 (mapcar (lambda (x) (car x)) l2))
         (l4 (set-difference l3 l1))    ; CL
         (l5 (mapcar (lambda (x) (assoc x l2)) l4))
         (l6 (mapcar (lambda (x) (nreverse x)) (mon-nef-dir-fldr folder-alist)))
         (l7))
    (mapcar (lambda (x)
              (let ((range-match (cadr (assoc x l5)))
                    (folder-match (cadr (assoc x l6))))
                (when (and folder-match range-match)
                  (setq l7 (cons `(,(concat range-match " |In Folder-> " folder-match) ,x) l7)))))
            l4) l7))

;;;test-me;(mon-nef-dir-keep-3 *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-nef-dir-big (folder-alist)
  "Return FOLDER-ALIST as one 'BIG' combined alist. 
Retrun value includes those formatted with: 
`mon-nef-dir-converge', `mon-nef-dir-conc-ranges', and `mon-nef-dir-keep-3'. 
CALLED-BY: `mon-dired-nef-dir',`naf-dired-image-dir' for completing-read prompts.
See also; `mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-ranges',
`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',`mon-nef-dir-rmv-empt',
`*nefs_photos_nefs-alist*', `*nef-scan-nefs-path*'."
  (let (newp)
    (setq newp (nconc 
                (mon-nef-dir-converge folder-alist)
                (mon-nef-dir-conc-ranges folder-alist)
                (mon-nef-dir-keep-3 folder-alist)
                newp))
    newp))

;;;test-me;(mon-nef-dir-big *nefs_photos_nefs-alist*)

;;; ==============================
;;; CREATED: <Timestamp: Saturday June 27, 2009 @ 04:27.24 PM - by MON KEY>
(defun mon-dired-nef-dir ()
  "Dired to one of the files in `*nef-scan-nefs-path*'.
Prompts for a directory using completions generated from
`*nefs_photos_nefs-alist*'. Use `naf-dired-image-dir' for exteneded dir options.\n
See also; `mon-dired-nef-dir', `mon-nef-dir-big', `mon-nef-dir-converge',
`mon-nef-dir-keep-3',`mon-nef-dir-fldr',`mon-nef-dir-conc-ranges',
`mon-nef-dir-ranges',`mon-nef-dir-conc-dups',`mon-nef-dir-find-dups',
`mon-nef-dir-rmv-empt'."
  (interactive)
  (let ((nef-alist (mon-nef-dir-big *nefs_photos_nefs-alist*))
        (get-nef-dir))
    (setq get-nef-dir
          (cadr (assoc-string 
                 (completing-read "Which nef directory (tab-completes):" nef-alist) 
                 nef-alist)))
    (dired (concat *nef-scan-nefs-path* "/" get-nef-dir))))

;;;test-me;(mon-dired-nef-dir)
;;;test-me;(call-interactively 'mon-dired-nef-dir)

;;; ==============================
;;; TODO: incorporate: (file-directory-p filename)
;;; WORKING: NOT-FINISHED-AS-OF:
;;; CREATED: <Timestamp: Friday May 15, 2009 @ 04:49.17 PM - by MON KEY>
(defun mon-hash-img-dir (&optional dir-to-hash)
  "Hash the img dirs in path.
When non-nil DIR-TO-HASH supplies an alternate img directory to hash.
Defaults to value in global var: `*img-hash*'.
See also; `mon-try-comp-dir', `mon-complete-hashed-dir', `mon-build-dir-list',
`mon-hash-img-dir'."
  (let* ((dir-list (mon-get-proc-buffers-directories nil dir-to-hash))
	 (h-size (length dir-list)) ;to use with for the keyword arg to make-hash-table: `:size h-size' 
	 (img-hash)
	 (with-bmp)
	 ;;(let (
	 (last-hashed "last-hashed")
	 (hashed-on (current-time)) ;=>(HIGH LOW MICRO)
	 ;; )(unless (gethash "last-hashed" *temp-hash*)  (puthash last-hashed hashed-on *temp-hash*)))
	 )
    (setq img-hash *img-hash*) ;;(setq img-hash *temp-hash*) 
    (while dir-list
      (let ((in-dir (car dir-list)))
	(puthash in-dir '() img-hash)
	(setq dir-list (cdr dir-list))))
    (setq dir-list (mon-hash-all-keys *img-hash*)) ;*img-hash* ;*temp-hash*
    (while dir-list
      (let* ((in-dir (car dir-list))
     	     (has-imgs (mon-get-ebay-bmps-in-dir t in-dir)) ;get partial-path of the .bmps in directory
	     ;;(file-expand-wildcards (concat (file-name-as-directory in-dir "*.bmp")) 
	     ;;(gethash 'last-hashed *img-hash*) ;*temp-hash*
	     ;; check the file for modification since last
	     ;;(file-attributes file-to-examine
	     ;;		 (> mod-times now-time)
	     ;; (decode-time (cadr (gethash "u:/NEFS_PHOTOS/Nef_Drive2/EBAY/BMP-Scans/e1140" *temp-hash*))) 
	     ;; (decode-time (cadr (gethash SOME-HASH-ELT *temp-hash*))) 
	     )
	(if has-imgs
	    (puthash in-dir `(,has-imgs ,(current-time)) img-hash)
	  (puthash in-dir `(,() ,(current-time)) img-hash))
	(setq dir-list (cdr dir-list))))
    ;;unless is _NOT_ what we want here - unless (> curr-time some-heuristic)
    (unless (gethash "last-hashed" img-hash);*img-hash*) ;*temp-hash* 
      (puthash last-hashed hashed-on img-hash)) 
    img-hash))

;;; ==============================
;;; WORKING: NOT-FINISHED-AS-OF:
;;; CREATED: <Timestamp: Tuesday May 19, 2009 @ 05:55.58 PM - by MON KEY>
(defun mon-complete-hashed-dir (comp-hsh dir-string common-string)
  "Return a buffer displaying possible completions for DIR-STRING in COMP-HSH.
COMP-HSH (a hash-table) should contain a common substring COMMON-STRING.
See also; `mon-hash-img-dir', `mon-try-comp-dir', `mon-build-dir-list'."
  (let ((compl (mon-hash-all-keys comp-hsh)) ;*temp-hash*))
	(dir-st dir-string)		 ;*ebay-images-path*
	(comm-st common-string))	 ;"e1")) 
					;(save-window-excursion
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list
       (all-completions		      ;display-completion-list' completions
	dir-st			      ;all-completions' string 
	compl)			      ;all-completions' collection 
       comm-st))		      ;display-completion-list' common-substring
    ))			      ;(sit-for 60)))    

;;;test-me;(mon-complete-hashed-dir *img-hash* (concat *ebay-images-path* "/") "e1")

;;; ==============================
;;; TODO: Better heuristics on PTH and COLLECTION.
;;;       COLLECTION should build an a-list rather than defaulting to the hash.
;;; STILL-TESTING: AS-OF:
;;; CREATED: <Timestamp: Thursday May 21, 2009 @ 02:28.56 PM - by MON KEY>
(defun mon-try-comp-dir (str &optional pth collection)
  "Best completion of string STR in directory PTH using COLLECTION.
When non-nil PTH is a path name default is `*ebay-images-bmp-path*'.
When non-nil COLLECTION is a list of direcotories in PTH built with `mon-build-dir-list'
per completion specs, default is `*img-hash*'.
See also; `mon-complete-hashed-dir',`mon-hash-img-dir'."
  (let* ((comp-str str)
	 (path (if pth 
		   (directory-file-name pth)
		 (directory-file-name *ebay-images-bmp-path*)))
	 (combo (concat path "/" str))
	 (in-coll (if collection (mon-build-dir-list pth) *img-hash*))) ; *temp-hash*
    (try-completion combo in-coll)))

;;;test-me;(mon-try-comp-dir "e1" *ebay-images-jpg-path* t)

;;;(unintern 'mon-try-comp-dir)

;;; ==============================
(provide 'mon-dir-utils)
;;; ==============================

;;; ==============================
;;; mon-dir-utils.el ends here
;;; EOF
