;;; msf-abbrev.el --- maintain abbrevs in a directory tree

;;;_* Copyright
;; Copyright (C) 2004,2005,2006 Free Software Foundation, Inc.

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;; Version: 0.99a
;; Keywords: abbrev convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;;_* Credits 
;;;_ , Benjamin Rutt <brutt@bloomington.in.us>  ;;;_  . Original Author
;;;_ , Victor Hugo Borja <vic@thehatcher.com>
;;;_  . almost a complete re-implementation
;;;_  . msf-abbrev minor mode
;;;_  . more customizable variables
;;;_  . faces and categories for each field type
;;;_  . linked fields
;;  modification of a linked field is replicated to other linked
;;  fields with the same name in the template.
;;  It can be unliked using C-c u, thus becoming just another 
;;  *real* field
;;;_  . multiple field resurrection (even if linked)
;;;_  . added/changed some key bindings and commands
;;;_  . the trim tag used to remove/replace blanks in templates
;;;_  . choose fields can use `completing-read' like functions
;;;_  . improvements to the choose-selection buffer
;;;_  . minor support for blank characters between <choice> tags
;;;_  . a bit of code organization using `allout-mode'
;;;_  . another bit of documentation
;;;_  . custom per major-mode elisp initialization file.
;;  see `msf-abbrev-init-mode'
;;;_  . by using `msf-abbrev-install-locally' you can include
;; abbrevs from other modes without if your file system does not
;; support sym-links.
;;;_  . selecting a region and M-x `msf-cmd-define' inserts the
;; selection to the abbrev file.

;;;_* Commentary
;;
;; This package allows you to place your abbrevs into your filesystem,
;; in a special directory tree. At least that was the original purpose 
;; but recently it has gained some cool features, making from it an
;; alternative templating system for emacs.
;;
;; More information and an impressive demo are available at
;; http://www.bloomington.in.us/~brutt/msf-abbrev.html

;;;_* Usage
;; 
;; Put this file in your load-path and in your .emacs
;;   (require 'msf-abbrev)
;; 
;;   (global-msf-abbrev-mode t) ;; for all modes with abbrevs or
;;   ;; M-x msf-abbrev-mode RET ;; for only one buffer
;;
;;   ;; You may also want to make some bindings:
;;   (global-set-key (kbd "C-c l") 'msf-cmd-goto-root)
;;   (global-set-key (kbd "C-c a") 'msf-cmd-define)
;;
;; While editing in your favorite mode (say ruby =) just make:
;;   M-x msf-cmd-define RET
;; 
;; Enter a name for the new abbrev, eg. "edr" (each_do_render)
;; and in the new created buffer <edr> type:
;;
;;   <field "@collection">.each do |<field link="e" "element">|
;;     <linked "e">.<endpoint>
;;   end
;;   render :<choose link="r">
;;            <choice "nothing">
;;            <choice "inline">
;;            <choice "text">
;;           </choose> => "rendereing using :<linked "r">"
;;
;; Of course this just an example =
;; This is just ruby code, but some text is tagged, this are the
;; modificable fields of this template (not valid sgml format).
;; Save the "edr" buffer and return to your original working
;; buffer, now type the abbreviation name followed with a space to
;; expand the abbreviation:
;;
;;   edr SPC
;;
;; You may as well define a key binding for it's expansion function
;;
;;   (define-key ruby-mode-map 
;;               (kbd "C-c e") 'msf-expand/ruby-mode/edr)
;;   
;; Now the abbreviation is expanded with the template, and you are
;; moved to the first field, so you can navigate between
;; them using TAB or S-TAB, type something while over a linked field,
;; delete one, then recreate it in the same position, press RET
;; over "nothing", when finished either TAB one more time after
;; the last field, or press M-RET over a field to finish with this
;; template, expand another abbrev, experiment a bit and see if 
;; this can help you while coding.
;;
;; There are many key-bindings mapped for each field, to know them
;; place the cursor over a certain field and press: C-h b
;; And take a look after `keymap' Property Bindings.
;; Try doing this while over a chooseable field or a linked field.
;; 
;;
;; Many abbreviation files for popular major-modes are available at 
;; http://www.bloomington.in.us/~brutt/msf-abbrev.html
;; Checkout the documentation and demo in the same page.

;;;_* TODO and Wishlist
;;;_ , A better parser (and sintax) for abbrev files
;;;_ ? Some fields being dependant on a chooseable field's value
;;     (Triggers on fields change, maybe to delete/insert other templates)
;;;_ ! many more users and contributors =)

;;;_* Known Bugs and Not Intended Features
;; 

;;;_* Implementation & Developer Notes
;;;_ , I(vic) re-implemented almost 90% of version 0.98a using 
;; overlays as the *form* containing *fields*, this reduced many
;; searches in the whole buffer.
;; This way the form overlay can serve as a medium for 
;; data interchange between fields and/or functions. 
;;;_ , Also modification hooks are implemented by the function
;; `msf-form-on-modification' that dispatches to other functions.
;; Using overlays allow us to monitor changes only between the form
;; boundaries as anything that happens outside is of no interest
;; to us.
;;;_ , The advice for `yank' became simpler, so I expect pasting
;; large text yanking can be faster.
;;;_ , Resurrection data, cons of (markers . properties) are saved
;; in the form overlay. 
;;;_ , Each field can have a custom modification hook.

;;;_* Code:
(require 'cl)
;;;_* Minor mode

(define-minor-mode msf-abbrev-mode
  "File based abbrevs"
  :init-value nil
  :global nil
  :group   'msf-abbrev
  :lighter ""
  :keymap  (make-sparse-keymap)
  (if msf-abbrev-mode
      (progn (msf-abbrev-install-locally major-mode)
	     (abbrev-mode msf-abbrev-mode))
    (msf-abbrev-uninstall-locally major-mode)
    (abbrev-mode msf-abbrev-mode)))

(define-global-minor-mode global-msf-abbrev-mode
  msf-abbrev-mode msf-abbrev-maybe-enable
  :group 'msf-abbrev)
  

;;;_* Customizable Variables

;;;_ = msf-abbrev-root
(defcustom msf-abbrev-root (expand-file-name 
			    ".emacs.d/mode-abbrevs")
  "*Root directory of user abbreviation files.

This directory should have subdirectories such as c-mode, lisp-mode, etc."
  :group 'msf-abbrev
  :type 'directory)

(defcustom msf-abbrev-always-rescan nil
  "*Whether rescan directory when every time enter a major-mode."
  :group 'msf-abbrev
  :type 'boolean)

(defcustom msf-abbrev-mode-alias nil
  "*Definition of mode alias"
  :group 'msf-abbrev
  :type 'lisp)

(defcustom msf-abbrev-indent-after-expansion nil
  "*Whether to indent the region inserted after the abbrev is expanded."
  :group 'msf-abbrev
  :type 'boolean)

(defcustom msf-abbrev-first-insertion-replaces t
  "*Whether to replace a field's content when first inserting text at
  it.

If non-nil, and the first change to a field is text insertion then 
replace the default content with the inserted text."
  :group 'msf-abbrev
  :type 'boolean)

(defcustom msf-abbrev-first-deletion-removes nil
 "*Whether to remove a fields content when first deleting text.

If non-nil, and the first change to a field is text deletion then
the full field content is removed."
  :group 'msf-abbrev
  :type 'boolean)

(defcustom msf-abbrev-goto-next-loops nil
  "*Whether to loop after the last field in a form.

If non-nil, `msf-cmd-goto-next' goes to the first field in the
form instead of deleting the form and going to the end point."
  :group 'msf-abbrev
  :type 'boolean)

(defcustom msf-abbrev-after-expansion-hook nil
  "Hook called after expansion of an msf abbrev."
  :group 'msf-abbrev
  :type 'hook)

(defcustom msf-abbrev-choose-method
  'msf-choose-selecting
  "Function used to get the value for a multiple choice field.

If you specify the name of some another function, please ensure it
has an argument list like `completing-read' does."
  :group 'msf-abbrev
  :type '(choice 
          (function-item :tag "Using the msf selection buffer."
                         msf-choose-selecting)
          (function-item :tag "Detect a completion function."
                         msf-choose-completing)
          (function-item :tag "Builtin basic completion"
                         completing-read)
          (function-item :tag "IDO completion"
                         ido-completing-read)
          (function :tag "User specified function"
                    msf-choose-completing)))

;;;_ , Keymaps

;;;_  = msf-fld-field-keymap
(defvar msf-fld-field-keymap 
  (let ((map (make-sparse-keymap)))
    (define-key map 
      (kbd "TAB") 'msf-cmd-next-real)
    (define-key map
      (kbd "S-TAB") 'msf-cmd-previous-real)
    (define-key map 
      (kbd "C-c n") 'msf-cmd-next-real)
    (define-key map
      (kbd "C-c p") 'msf-cmd-previous-real)
    (define-key map
      (kbd "<S-iso-lefttab>") 'msf-cmd-previous-real)
    (define-key map
      (kbd "C-c f") 'msf-cmd-next-fld)
    (define-key map
      (kbd "C-c b") 'msf-cmd-previous-fld)
    (define-key map
      (kbd "C-c >") 'msf-cmd-goto-end)
    (define-key map
      (kbd "C-c <") 'msf-cmd-goto-start)
    (define-key map
      (kbd "M-RET") 'msf-cmd-end-form)
    (define-key map
      (kbd "C-c e") 'msf-cmd-make-editable)
    (define-key map
      (kbd "C-c q") 'msf-cmd-clean-field)
    (define-key map
      (kbd "C-c Q") 'msf-cmd-clean-form)
;;    (define-key map
;;       (kbd "C-c d") 'fld-delete)
;;     (define-key map
;;       (kbd "C-c q") 'fld-cleanup-form-at-point)
    map)
  "Keymap active on form simple fields")


;;;_  = msf-fld-linked-keymap
(defvar msf-fld-linked-keymap 
  (let ((map (copy-keymap msf-fld-field-keymap)))
    (define-key map
      (kbd "C-c u") 'msf-cmd-unlink)
    map)
  "Keymap active on form linked fields")


;;;_  = msf-fld-choice-keymap
(defvar msf-fld-choice-keymap 
  (let ((map (copy-keymap msf-fld-field-keymap)))
    (define-key map
      (kbd "RET") 'msf-cmd-make-choice)
    map)
  "Keymap active on form choice fields")

;;;_  . Faces

;;;_   , msf-fld-field-face
(defface msf-fld-field-face 
  (list (cons t (cons :inherit (list 'highlight))))
  "Face used to highlight form simple fields"
  :group 'msf-abbrev)

;;;_   , msf-fld-linked-face
(defface msf-fld-linked-face 
  (list (cons t (cons :inherit (list 'lazy-highlight))))
  "Face used to highlight form linked fields"
  :group 'msf-abbrev)

;;;_   , msf-fld-choice-face
(defface msf-fld-choice-face 
  (list (cons t (cons :inherit (list 'button))))
  "Face used to highlight form choice fields"
  :group 'msf-abbrev)

;;;_* Variables

;;;_ , global variables
(defvar msf-abbrev-scan-time nil)

;;;_  . msf-abbrev-form-priority
(defvar msf-abbrev-form-priority 0
  "Priority counter used for form overlays")

;;;_  . msf-abbrev-mode-abbrevs
(defvar msf-abbrev-mode-abbrevs nil
  "Global alist of mode abbrevs")

;;;_  . field modification hooks
(defvar msf-fld-before-deletion-hook 
  nil
  "Hook to be run before a text deletion ocurrs in a field")
(defvar msf-fld-after-deletion-hook 
  'msf-fld-after-deletion
  "Hook to be run after a text deletion ocurrs in a field")

(defvar msf-fld-before-insertion-hook 
  nil
  "Hook to be run before a text insertion ocurrs in a field")
(defvar msf-fld-after-insertion-hook 
  'msf-fld-after-insertion
  "Hook to be run after a text insertion ocurrs in a field")

(defvar msf-fld-before-resurrection-hook 
  'msf-fld-before-resurrection
  "Hook to be run before a field will be killed")
(defvar msf-fld-after-resurrection-hook 
  'msf-fld-after-resurrection
  "Hook to be run after a field has been resurrected")

(defvar msf-fld-linked-modification-hooks
  '((fld-after-insertion-hook
     msf-fld-linked-replicate)
    
    (fld-after-deletion-hook
     msf-fld-linked-replicate)
    
    (fld-before-resurrection-hook
     msf-fld-linked-before-resurrection)
    
    (fld-after-resurrection-hook
     msf-fld-linked-after-resurrection))
  "Hooks to be run on modification to a linked field")

;;;_ , buffer local variables

;;;_ , fields categories

;;;_  . msf-fld-field-category
(setplist 'msf-fld-field-category
	  `(face msf-fld-field-face 
		 keymap ,msf-fld-field-keymap))

;;;_  . msf-fld-linked-category		 
(setplist 'msf-fld-linked-category
	  `(face msf-fld-linked-face 
		 fld-modification-hooks 
		 msf-fld-linked-modification-hooks
		 keymap ,msf-fld-linked-keymap))

;;;_  . msf-fld-choice-category		 
(setplist 'msf-fld-choice-category
	  `(face msf-fld-choice-face 
		 keymap ,msf-fld-choice-keymap))

;;;_* Functions (msf-abbrev)

;;;_ , Helpers

;;;_  > msf-abbrev-directory-files (dir)
(defun msf-abbrev-directory-files (dir)
  "List abbrev files on DIR"
  (delq nil
	(mapcar
	 (lambda (x)
	   (if (or (string-match "^\\." (file-name-nondirectory x))
		   (string-match "~$" (file-name-nondirectory x)))
	       nil x))
	 (directory-files dir t))))

;;;_  > msf-abbrev-eval (lisp)
(defun msf-abbrev-eval(lisp)
  (eval (cond ((consp lisp) lisp)
	      ((and (stringp lisp) (file-exists-p lisp))
	       (with-temp-buffer
		 (insert-file-contents lisp)
		 (read (buffer-substring-no-properties
			(point-min) (point-max)))))
	      ((stringp lisp) (read lisp))
	      (t (error "Invalid argument type: %s"
			(type-of lisp))))))
      
(defun msf-abbrev-add-to-list (list element &optional append)
  (let ((old (assoc (car element) (symbol-value list))))
    (if old (setcdr old (cdr element))
      (add-to-list list element append))))

(defsubst msf-abbrev-file-mtime (file &optional id-format)
  (nth 5 (file-attributes file id-format)))

(defun msf-abbrev-mode-alias (modename)
  (let ((mode (intern-soft modename)))
    (setq mode (assoc-default mode msf-abbrev-mode-alias))
    (if mode
        (symbol-name mode)
      (let ((cpls
             (file-name-all-completions
              (concat modename ".aliases.") msf-abbrev-root))
            (dest modename))
        (when cpls
          (assert (string-match "\\(.*\\)\\.aliases\\.\\(.*\\)" (car cpls)))
          (setq dest (match-string 2 (car cpls))))
        dest))))

(defun msf-abbrev-locate-mode-dir (modename)
  (concat (file-name-as-directory msf-abbrev-root)
          (msf-abbrev-mode-alias modename)))

(defsubst msf-abbrev-file-newer-than-p (file time)
  (time-less-p time (msf-abbrev-file-mtime file)))

(defun msf-abbrev-major-mode-heuristic (symbol)
  (and (fboundp symbol)
       (string-match ".*-mode$" (symbol-name symbol))))

;;;_ , Initialization

;;;_  > msf-abbrev-maybe-enable
(defun msf-abbrev-maybe-enable nil
  "Called if `globa-msf-abbrev-mode' is enabled

Enable `msf-abbrev-mode' only for those buffer whose
major-mode has an abbrev directory or init file."
  (when (or (assq major-mode msf-abbrev-mode-abbrevs)
            (file-exists-p 
             (format "%s%s"
                     (file-name-as-directory msf-abbrev-root)
                     major-mode))
            (file-directory-p
             (format "%s%s"
                     (file-name-as-directory msf-abbrev-root)
                     major-mode)))
    (msf-abbrev-mode 1)))

;;;_  > Is two function needed?
;; ;;;_  > msf-abbrev-init-all-modes nil
;; (defun msf-abbrev-init-all-modes nil
;;   "Initialize msf-abbrev and setup mode-specific hooks"
;;   (let (major_mode)
;;     (mapc (lambda (mode-dir)
;; 	    (setq major_mode
;; 		  (intern (file-name-nondirectory mode-dir)))
;; 	    (eval-after-load 
;; 		(cond ((eq 'global major_mode) 'emacs)
;; 		      (t major_mode))
;; 	      `(msf-abbrev-init-mode ',major_mode)))
;; 	  (msf-abbrev-directory-files 
;; 	   (expand-file-name msf-abbrev-root)))))

;; ;;;_  > msf-abbrev-init-mode (&optional modename)
;; (defun msf-abbrev-init-mode (&optional modename)
;;   "Load the file <MODENAME>.el if present in the `msf-abbrev-root'
;; directory. 

;; This file can use the variable `modename' and the 
;; `msf-abbrev-define-abbrev' function."
;;   (or modename (setq modename major-mode))
;;   (assert (symbolp modename))
;;   (let ((init-file (format "%s%s.el" 
;;                            (file-name-as-directory msf-abbrev-root)
;;                            modename)))
;;     (and (file-exists-p init-file)
;;          (msf-abbrev-eval init-file))))

;;;_  > msf-abbrev-scan-mode (&optional modename replace)
(defun msf-abbrev-scan-mode (&optional modename replace)
  "Scan abbrevs and generate interactive functions from abbrev 
files in the corresponding directory of MODENAME.

MODENAME must be a symbol like 'ruby-mode, defaults to the 
current value of the variable `major-mode'

If REPLACE is non-nil then the abbrevs for MODENAME are removed 
before scanning the directory."
  (interactive
   (list (intern (completing-read
                  "Scan mode: "
                  obarray 'msf-abbrev-major-mode-heuristic
                  t (symbol-name major-mode)))
         t))
  (let (mode table)
    (if (or (null modename) (eq modename major-mode))
        (setq modename (msf-abbrev-mode-alias
                        (symbol-name major-mode))
              table local-abbrev-table)
      (setq modename (msf-abbrev-mode-alias
                      (symbol-name modename))
            table (symbol-value (intern (format "%S-abbrev-table" modename)))))
    (setq mode (intern-soft modename))
    (assert (symbolp mode))
    (let ((last-time (or (cdr (assq mode msf-abbrev-scan-time))
                         '(0 0)))
          (mode-dir (msf-abbrev-locate-mode-dir modename))
          (modeab (assq mode msf-abbrev-mode-abbrevs))
          init-file)
      (when (or (null modeab) replace)
        (setq modeab (cdr modeab))
        (setq init-file (concat mode-dir ".el"))
        (if (and (file-exists-p init-file)
                 (msf-abbrev-file-newer-than-p init-file last-time))
            (load init-file))
        (when (file-directory-p mode-dir)
          ;; define a function for each abbrev on mode directory
          (mapc (lambda (abbr-file)
                  (msf-abbrev-define-abbrev
                   (intern (file-name-nondirectory 
                            (file-name-sans-extension 
                             abbr-file)))
                   (msf-abbrev-eval 
                    `(lambda nil
                       ,(format "Expand content of file %s"
                                abbr-file)
                       (msf-abbrev-expand-file ,abbr-file)))
                   mode))
                (remove-if (lambda (file)
                             (and (assoc (intern (file-name-nondirectory file)) modeab)
                                  (not (msf-abbrev-file-newer-than-p file last-time))))
                           (msf-abbrev-directory-files mode-dir))))
        (msf-abbrev-define-on-table mode table)
        (msf-abbrev-add-to-list 'msf-abbrev-scan-time
                                (cons mode (current-time)))))))

;;;_  > msf-abbrev-install-locally (mode)
(defun msf-abbrev-install-locally (mode)
  "Install abbrevs for MODE in the `local-abbrev-table'"
  (msf-abbrev-scan-mode mode msf-abbrev-always-rescan))

;;;_  > msf-abbrev-uninstall-locally (mode)
(defun msf-abbrev-uninstall-locally (mode)
  "Uninstall abbrevs for MODE from the `local-abbrev-table'"
  (msf-abbrev-define-on-table mode local-abbrev-table 'undefine))

;;;_ , Abbrevs

;;;_  > msf-abbrev-define-on-table (&optional mode table undefine)
(defun msf-abbrev-define-on-table (&optional mode table undefine)
  "Install msf-abbrevs defined for MODE in TABLE

MODE defaults to the current value of `major-mode'.
TABLE defaults to the local variable `local-abbrev-table'.
If UNDEFINE is non-nil undefine MODE's abbrevs from TABLE"
  (or mode (setq mode major-mode))
  (or table (setq table local-abbrev-table))
  (let ((mode-abbrevs (assq mode msf-abbrev-mode-abbrevs)))
    (when mode-abbrevs
      (mapc (lambda (name-function)
	      (let ((name (car name-function))
		    (expand-function (cdr name-function)))
		(define-abbrev table
		  (symbol-name name)
		  (if undefine nil "")
		  expand-function 0 t)))
	    (cdr mode-abbrevs)))))

;;;_  > msf-abbrev-choose-abbrev 
(defun msf-abbrev-choose-abbrev (&optional mode requires-match)
  (let ((abbrevs (assq (or mode major-mode) 
                       msf-abbrev-mode-abbrevs))
        (choice nil)
        (choices nil))
    (when abbrevs
      (mapc (lambda (abbrev)
              (setq choices (cons (format "%s" (car abbrev))
                                  choices)))
            (cdr abbrevs))
      (msf-choose-completing "Abbrev name: " choices nil
                             requires-match))))

;;;_  > msf-abbrev-define-abbrev (name function &optional mode)
(defun msf-abbrev-define-abbrev (name function &optional mode)
  "Define an abbrev for MODE inserted by FUNCTION and 
named NAME.

MODE defaults to the current value of the variable `major-mode'.
FUNCTION is called with no arguments by the generated wrapper.

Return the name of the wrapper function with format:
`msf-expand/MODE/NAME'"
  (and (stringp name) (setq name (intern name)))
  (or mode (setq mode major-mode))
  (let (funname fundoc funobj mode-abbrevs)
    (setq funname 
	  (intern (format "msf-expand/%s/%s" mode name)))
    (setq fundoc 
	  (format "Expansion function for \"%s\" in %s" 
		  name mode))
    (if (symbolp function)
      (setq fundoc (format "%s wrapping the `%s' function" 
			   fundoc function))
      (when (documentation function)
	(setq fundoc 
	      (format "%s\n\nDocumentation:\n\n%s"
		      fundoc
		      (documentation function)))))
    (setq funobj 
	  (msf-abbrev-eval 
	   `(defun ,funname nil ,fundoc
	      (interactive)
	      (apply ,function nil)
	      (msf-abbrev-just-expanded ',funname)
	      ',funname)))
    (put funname 'no-self-insert t)
    (put funname 'abbrev-name name)
    (setq mode-abbrevs 
	  (or (assq mode msf-abbrev-mode-abbrevs)
	      (list mode)))
    (setcdr mode-abbrevs
	    (append (cdr mode-abbrevs)
		    (list (cons name funname))))
    (setq msf-abbrev-mode-abbrevs
          (assq-delete-all mode msf-abbrev-mode-abbrevs))
    (add-to-list 'msf-abbrev-mode-abbrevs mode-abbrevs)))

;;;_  > msf-abbrev-expand-abbrev (name &optional mode)
(defun msf-abbrev-expand-abbrev (name &optional mode)
  (or mode (setq mode major-mode))
  (or (symbolp name) (setq name (intern name)))
  (let ((mode-abbrevs (assq mode msf-abbrev-mode-abbrevs)))
    (if mode-abbrevs
	(let ((expand-function (assq name (cdr mode-abbrevs))))
	  (if expand-function
	      (apply (cdr expand-function) nil)
	    (error "No such abbrev %s for %s" name mode)))
      (error "No msf-abbrevs defined for %s" mode))))

;;;_  > msf-abbrev-expand-file (file)
(defun msf-abbrev-expand-file (file)
  "Insert FILE contents as a form or eval it if the file name
ends with .el"
  (if (and (string-match "\\.el$" file)
           (file-exists-p file))
      (msf-abbrev-eval file)
    (msf-form-insert
     (with-temp-buffer 
       (insert-file-contents file)
       (buffer-string)))))

;;;_  > msf-abbrev-just-expanded (function)
(defun msf-abbrev-just-expanded (function)
  "Notify that FUNCTION has just made an expansion.

Run `msf-abbrev-after-expansion-hook'"
  (run-hooks 'msf-abbrev-after-expansion-hook))

;;;###autoload
(defun msf-abbrev-generate-init-file (mode)
  (interactive
   (list (intern (completing-read
                  "Scan mode: "
                  msf-abbrev-mode-abbrevs
                  nil t))))
  (let ((modeab (assq mode msf-abbrev-mode-abbrevs))
        mode-dir name func form-file)
    (setq mode (symbol-name mode)
          mode-dir (msf-abbrev-locate-mode-dir mode))
    (with-current-buffer (get-buffer-create (concat "*" mode "*"))
      (erase-buffer)
      (setq buffer-file-name (concat mode-dir ".el"))
      (insert ";; -*- mode: emacs-lisp -*-\n")
      (emacs-lisp-mode)
      (dolist (ab (cdr modeab))
        (setq name (symbol-name (car ab))
              func (symbol-name (cdr ab))
              form-file (concat mode-dir "/" name))
        (insert "(defun " func " ()\n"
                "\"Expandsion function for " name " in " mode ".\"\n"
                "(interactive)\n"
                "(msf-form-insert "
                (if (file-exists-p form-file)
                    (format "%S"
                            (with-temp-buffer
                              (insert-file-contents form-file)
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                  (with-temp-buffer
                    (insert (format "%S"
                                    (symbol-function (cdr ab))))
                    (goto-char (point-min))
                    (re-search-forward "msf-form-insert")
                    (buffer-substring (point) (scan-sexps (point) 1))))
                ")\n"
                "(msf-abbrev-just-expanded (quote " func "))\n"
                "(quote " func "))\n\n"))
      (insert "(add-to-list 'msf-abbrev-mode-abbrevs\n  '("
              mode "\n")
      (dolist (ab (cdr modeab))
        (setq name (symbol-name (car ab))
              func (symbol-name (cdr ab)))
        (insert "  (" name " . " func ")\n"))
      (insert "    ))")
      (switch-to-buffer (current-buffer))
      (message "Press C-x C-s to save init file"))))

;;;_ , Forms Methods

;;;_  > msf-form-insert (string)
(defun msf-form-insert (str)
  "Insert STR and generate editable fields"
  (let ((inhibit-modification-hooks t)
	(case-fold-search t)
	(form nil))
    (goto-char
     (save-excursion
       (save-match-data
	 (setq form
	       (make-overlay (point)
			     (save-excursion (insert str)
					     (point))))
	 (msf-form-put form 'priority 
		       (incf msf-abbrev-form-priority))
	 (msf-form-put form 'msf-form msf-abbrev-form-priority)
	 (run-hook-with-args 'msf-form-insert-hook form)
	 (msf-form-parse-comments form)
	 (msf-form-parse-trims form)
	 (msf-form-parse-elisp form)
	 (msf-form-parse-vars  form)
	 (msf-form-parse-choices form)
	 (msf-form-parse-fields form)
	 (msf-form-parse-links form)
	 (msf-form-parse-queries form)
	 (msf-form-parse-endpoint form))
       (when msf-abbrev-indent-after-expansion
         (indent-region (msf-form-start form)
                        (msf-form-end form)))
       (msf-form-put form 'modification-hooks
		    (list 'msf-form-on-modification))
       (msf-form-put form 'insert-in-front-hooks
		    (list 'msf-form-on-modification))
       (msf-form-put form 'insert-behind-hooks
		    (list 'msf-form-on-modification))
       (msf-form-put form 'evaporate t)       
       (msf-form-start-point form)))
    (unless (msf-form-first-fld form)
      (msf-form-delete form))))

;;;_  > msf-form-parse-endpoint (form)
(defun msf-form-parse-endpoint (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (re-search-forward "<\\(cursor\\|start-?point\\)>"
			      (msf-form-end form) t)
      (replace-match "" nil t)
      (msf-form-put form 'start-point (point-marker)))
    (goto-char (msf-form-start form))
    (while (re-search-forward "<end-?point>"
			      (msf-form-end form) t)
      (replace-match "" nil t)
      (msf-form-put form 'end-point (point-marker)))))

;;;_  > msf-form-parse-comments (form)
(defun msf-form-parse-comments (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (and (< (point) (msf-form-end form))
		(re-search-forward "<comment +\".*?\">"
				   (msf-form-end form) t))
      (replace-match "<trim \"\n\">" nil t))))

;;;_  > msf-form-parse-trims (form)
(defun msf-form-parse-trims (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (and (< (point) (msf-form-end form))
		(re-search-forward 
		 (concat "[ \t\n\r\f]*<\\(trim\\)"
			 "\\([ \t\n\r\f]+\"\\(.*?\\|"
			 "[ \t\n\r\f]+\\)\"\\)?>"
			 "[\t\n\r\f]*")
		 (msf-form-end form) t))
      (let* ((newsep (match-string 3))
             (spaces (and newsep (string-to-int newsep))))
        (replace-match "")
        (cond ((> spaces 0)
               (insert-char ?  spaces))
              ((stringp newsep)
               (insert newsep))
              (t nil))))))

;;;_  > msf-form-parse-vars (form)
(defun msf-form-parse-vars (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (and (< (point) (msf-form-end form))
		(re-search-forward 
		 "<\\(varlookup\\)[ \t]+\"\\(.*?\\)\">"
		 (msf-form-end form) t))
      (let ((v (match-string 2)))
	(replace-match (format "%s" (eval (intern v)) 
                               nil t))))))

;;;_  > msf-form-parse-elisp (form)
(defun msf-form-parse-elisp (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (and (< (point) (msf-form-end form))
		(re-search-forward 
		 "<\\(elisp\\|eval\\)[ \t]+\"\\(.*?\\)\">"
		 (msf-form-end form) t))
      (let ((v (match-string 2)))
	(replace-match "")
	(eval (read v))))))

;;;_  > msf-form-parse-queries (form)
(defun msf-form-parse-queries (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (let ((query-alist nil))
      (while (and (< (point) (msf-form-end form))
		  (re-search-forward 
		   "\\(<\\(query\\) \"\\(.*?\\)\">\\)"
		   (msf-form-end form) t))
	(let ((key (match-string 3)))
	  (when (not (assoc key query-alist))
	    (setq query-alist
		  (cons (list key (read-from-minibuffer key))
			query-alist)))
	  (replace-match (cadr (assoc key query-alist))))))))

;;;_  > msf-form-parse-fields (form)
(defun msf-form-parse-fields (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (and (< (point) (msf-form-end form))
		(re-search-forward 
		 (concat "<\\(formjump\\|field\\)"
			 "\\([ \t\n\r\f]+link=\"\\(..*?\\)\"\\)?"
			 "[ \t\n\r\f]+\"\\(..*?\\)\">")
		 (msf-form-end form) t))
      (let ((link (match-string 3))
	    (txt (match-string 4))
	    (properties '(category msf-fld-field-category)))
	(replace-match "" nil t)
	(when link
	  (setq properties
		(append (list 'fld-link link
			      'fld-modification-hooks
			      'msf-fld-linked-modification-hooks)
			properties)))
	(msf-fld-insert txt form properties)))))

;;;_  > msf-form-parse-links (form)
(defun msf-form-parse-links (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (while (and (< (point) (msf-form-end form))
		(re-search-forward 
		 "<link\\(ed\\)?[ \t]+\"\\(..*?\\)\">"
		 (msf-form-end form) t))
      (let (name source)
	(setq name (match-string 2))
	(replace-match "" nil t)
	(setq source (msf-form-first-fld 
		      form nil nil
		      (list 'fld-link name)))
	(if source
	    (msf-fld-insert 
	     (buffer-substring-no-properties(msf-fld-start source)
					    (msf-fld-end source))
	     form
	     (list 'category 'msf-fld-linked-category
		   'fld-link  name))
	  ;; no link source exists, create a normal field
	  (msf-fld-insert 
	   name form 
	   (list 'category 'msf-fld-field-category
		 'fld-link name
		 'fld-modification-hooks
		 'msf-fld-linked-modification-hooks)))))))

;;;_  > msf-form-parse-choices (form)
(defun msf-form-parse-choices (form)
  (save-excursion
    (goto-char (msf-form-start form))
    (let ((choice-start nil)
	  (choice-end   nil)
	  (choices nil)
	  (link nil)
	  (properties '(category msf-fld-choice-category)))
      (while (and (< (point) (msf-form-end form))
		  (re-search-forward 
		   (concat "<choose"
			   "\\([ \t]+link=\"\\(..*?\\)\"[ \t]*\\)?>"
			   "[ \t\n\r\f]*")
		   (msf-form-end form) t))
	(setq link (match-string 2))
	(and link 
	     (setq properties
		   (append (list 'fld-link link
				 'fld-modification-hooks
				 'msf-fld-linked-modification-hooks)
			   properties)))
	(replace-match "" nil t)
	(setq choice-start (point-marker))
	(assert (re-search-forward 
		 "[ \t\n\r\f]*</choose>"
		 (msf-form-end form) t))
	(replace-match "" nil t)
	(setq choice-end (point-marker))
	(goto-char choice-start)
	(setq choices nil)
	(while (and (< (point) (msf-form-end form))
		    (re-search-forward
		     (concat "[ \t\n\r\f]*<choice +"
			     "\"\\(..*?\\)\">[ \t\n\r\f]*")
		     choice-end t))
	  (setq choices (cons (match-string 1) choices))
	  (replace-match "" nil t))
	(setq choices (reverse choices))
	(setq properties (append (list 'fld-choices choices)
				 properties))
	(msf-fld-insert (car choices) form properties)))))

;;;_  > msf-form-start-point (form)
(defun msf-form-start-point (form)
  "Return the starting point for the user in this FORM.

If a point was defined with <start-point> then it is returned,
otherwise the first real (not linked) field position. If no
fields are present in the FORM return `msf-form-end-point'"
  (or (msf-form-get form 'start-point)
      (msf-form-first-real form)
      (msf-form-end-point form)))

;;;_  > msf-form-end-point (form)
(defun msf-form-end-point (form)
  "Return the end point of FORM"
  (or (msf-form-get form 'end-point)
      (msf-form-end form)))

;;;_  > msf-form-first-fld (form &optional start end properties category)
(defun msf-form-first-fld (form &optional start end properties category)
  "Find the first field position in FORM, if not found return nil.

Search is restricted to FORM boundaries or START and END if they
are given.
If PROPERTIES is given, it must be a list used to restrict search
to those fields wich text-properties has PROPERTIES as a subset.
If CATEGORY is given it must match the field's 'category property"
  (catch :field
    (let (field)
      (or start (setq start (msf-form-start form)))
      (or end (setq end (msf-form-end form)))
      (while (setq field
		   (text-property-any start end 'fld-form form))
	(cond ((and (subsetp properties (msf-fld-properties field)
                             :test 'equal)
                    (cond ((functionp category)
                           (apply category
                                  (msf-fld-get 'category field)
                                  nil))
                          (category
                           (member (msf-fld-get 'category field)
                                   (if (listp category) category
                                     (list category))))
                          (t t)))
	       (throw :field (msf-fld-start field)))
	      ((> end (msf-fld-end field)) ;; can search again
	       (setq start (msf-fld-end field)))
	      (t ;; not found in boundary
	       (throw :field nil)))))))

;;;_  > msf-form-last-fld (form &optional start end properties category)
(defun msf-form-last-fld (form &optional start end properties category)
  "Like `msf-form-first-fld' but search is made backwards."
  (save-excursion
    (catch :field
      (or start (setq start (msf-form-start form)))
      (or end (setq end (msf-form-end form)))
      (goto-char end)
      (while (and (setq field
			(previous-single-property-change
			 (point) 'fld-form nil start)))
	(cond ((and (eq (msf-fld-form field) form)
		    (subsetp properties 
			     (msf-fld-properties field)
			     :test 'equal)
                    (cond ((functionp category)
                           (apply category 
                                  (msf-fld-get 'category field)
                                  nil))
                          (category
                           (member (msf-fld-get 'category field)
                                   (if (listp category) category
                                     (list category))))
                          (t t)))
	       (throw :field (msf-fld-start field)))
	      ((> field start)
	       (goto-char field))
	      (t (throw :field nil)))))))

;;;_  > msf-form-first-link
(defsubst msf-form-first-link (form &optional start end properties)
  "Return the first link in FORM"
  (msf-form-first-fld form start end properties 
                      'msf-fld-linked-category))

;;;_  > msf-form-last-link
(defsubst msf-form-last-link (form &optional start end properties)
  "Return the last link in FORM"
  (msf-form-last-fld form start end properties
                     'msf-fld-linked-category))

;;;_  > msf-form-first-real
(defsubst msf-form-first-real (form &optional start end properties)
  "Return the first REAL field (not a link) in FORM."
  (msf-form-first-fld 
   form start end properties
   (lambda (c) (not (eq c 'msf-fld-linked-category)))))

;;;_  > msf-form-last-real
(defsubst msf-form-last-real (form &optional start end properties)
  "Return the last REAL field (not a link) in FORM"
  (msf-form-last-fld
   form start end properties
   (lambda (c) (not (eq c 'msf-fld-linked-category)))))

;;;_  > msf-form-delete (form)
(defun msf-form-delete (form)
  "Destroy FORM and make all fields on it simple text"
  (let ((inhibit-modification-hooks t)
	(field nil)
	(properties nil))
    (while (setq field (msf-form-first-fld form field))
      (msf-fld-set default-text-properties field))
    (delete-overlay form)))

;;;_  > msf-form-start (form)
(defsubst msf-form-start (form)
  (overlay-start form))

;;;_  > msf-form-end (form)
(defsubst msf-form-end (form)
  (overlay-end form))

;;;_  > msf-form-get (form prop)
(defsubst msf-form-get (form prop)
  (overlay-get form prop))

;;;_  > msf-form-put (form prop value)
(defsubst msf-form-put (form prop value)
  (overlay-put form prop value))

(defsubst msf-form-at (point)
  "Return the form at POINT.

If a field is located at POINT return it's form, otherwise returnp
the form with highest priority if any."
  (or (msf-fld-form point)
      (let (form (priority -1))
	(mapc (lambda (overlay)
		(when (and (msf-form-get overlay 'msf-form)
			   (< priority
			      (msf-form-get overlay 'priority)))
		  (setq form overlay)))
	      (overlays-at point))
	form)))

;;;_   , Modification hooks

;;;_    > msf-form-on-modification
(defun msf-form-on-modification (form after begin end &optional count)
  "Run when a modification is made to FORM"
  (let ((fld-pos (cond ((eq (msf-fld-form begin) form) 
			(set-marker (make-marker) begin))
		       ((eq (msf-fld-form end) form) 
			(set-marker (make-marker) end))
		       ((eq (msf-fld-form (1- end)) form)
			(set-marker (make-marker) (1- end)))))
	(inhibit-modification-hooks t)
	(begin (set-marker (make-marker) begin))
	(end (set-marker (make-marker) end))
	(hook-name nil)
	(fld-hook nil)
	(args nil))

    (setq args
	  (cond ((and fld-pos after (= begin end)) ;;after deletion
		 (setq hook-name 'fld-after-deletion-hook)
		 (list form fld-pos begin count))
		((and fld-pos after) ;; after insertion
		 (setq hook-name 'fld-after-insertion-hook)
		 (list form fld-pos begin end))
		((and fld-pos (= begin end)) ;; before insertion
		 (setq hook-name 'fld-before-insertion-hook)
		 (list form fld-pos begin))
		((and fld-pos  ;; before resurrection
		      (= (msf-fld-start fld-pos) begin)
		      (= (msf-fld-end fld-pos) end))
		 (setq hook-name 'fld-before-resurrection-hook)
		 (list form begin end))
		(fld-pos  ;; before deletion
		 (setq hook-name 'fld-before-deletion-hook)
		 (list form fld-pos begin end))
		((and after (/= begin end)  ;; after resurrection
		      (let ((res (msf-form-get 
				  form 
				  'fld-resurrection)))
			(and res (assoc begin res))))
		 (setq hook-name 'fld-after-resurrection-hook)
		 (list form begin end))))
    
    ;; for debugging
    ;; (require 'warnings)
    ;; (setq warning-minimum-log-level :debug)
    (and (boundp 'warning-minimum-log-level)
         (lwarn 'msf-abbrev :debug  "msf-form-on-modification %s"
                (list form after begin end count)))
    
    (when (and hook-name args (not undo-in-progress))
      
      (and (boundp 'warning-minimum-log-level)
           (lwarn 'msf-abbrev :debug "%s %s" hook-name args))

      (catch :fld-end-modification
        (setq fld-hook ;; get the field specific hook alist
              (or (and fld-pos
                       (msf-fld-get 'fld-modification-hooks fld-pos))
                  (msf-fld-get 'fld-modification-hooks begin)
                  (msf-fld-get 'fld-modification-hooks end)))
        
        (eval `(run-hook-with-args
                ',(intern (format "msf-%s" hook-name)) ,@args))
        
        (unless fld-hook ;; for ressurection the hook did not
          (setq fld-hook ;; exist before adding the properties
                (or (and fld-pos
                         (msf-fld-get 'fld-modification-hooks 
                                      fld-pos))
                    (msf-fld-get 'fld-modification-hooks begin)
                    (msf-fld-get 'fld-modification-hooks end))))
        
        (when (and (symbolp fld-hook) (boundp fld-hook))
          (setq fld-hook (symbol-value fld-hook)))
        
        (mapc (lambda (hook) (apply hook args))
              (cdr (assoc hook-name fld-hook)))
        
        ))))

;;;_    % yank advice
(defadvice yank (around msf-fld-around-yank activate)
  (let ((form (msf-fld-form)))
    (if (or form
	    (some (lambda (overlay)
		    (and (setq form
			       (msf-form-get 
				overlay 'fld-resurrection))
			 (assoc (point-marker) form)
			 (setq form overlay)))
		  (overlays-at (point))))
	(let ((inhibit-modification-hooks t)
	      (yank-excluded-properties t)
	      (begin (point)))
	  (msf-form-on-modification form nil begin begin)
	  ad-do-it
	  (msf-form-on-modification form 'after begin (point)))
      ad-do-it)))

;;;_   , Field modification hooks

;;;_    > msf-fld-before-insertion
;;;_    > msf-fld-after-insertion
(defun msf-fld-after-insertion (form fld-pos begin end)
  (set-text-properties begin end 
		       (msf-fld-properties fld-pos))
  (unless (msf-fld-get 'fld-state fld-pos)
    (msf-fld-add '(fld-state typed) fld-pos)
    (when msf-abbrev-first-insertion-replaces
      (save-excursion
        (and (> begin (msf-fld-start fld-pos))
             (delete-region (msf-fld-start fld-pos) begin))
        (and (> (msf-fld-end fld-pos) end)
             (delete-region end (msf-fld-end fld-pos)))))
    (move-marker fld-pos begin)))

;;;_    > msf-fld-before-deletion

;;;_    > msf-fld-after-deletion
(defun msf-fld-after-deletion (form fld-pos begin count)
  (when (and (not (msf-fld-get 'fld-state))
             msf-abbrev-first-deletion-removes)
    (let ((inhibit-modification-hooks nil))
      (throw :fld-end-modification
             (delete-region (msf-fld-start fld-pos)
                            (msf-fld-end fld-pos)))))
  (msf-fld-add '(fld-state untyped) fld-pos))

;;;_    > msf-fld-before-resurrection
(defun msf-fld-before-resurrection (form begin end)
  ;; save resurrection point and properties on the form
  (msf-form-put form 'fld-resurrection 
	       (cons (cons begin 
			   (msf-fld-properties begin))
		     (msf-form-get form 'fld-resurrection))))

;;;_    > msf-fld-after-resurrection
(defun msf-fld-after-resurrection (form begin end)
  (let* ((res-info (msf-form-get form 'fld-resurrection))
	 (old-properties (assoc begin res-info)))
    (set-text-properties begin end (cdr old-properties))
    (add-text-properties begin end '(fld-state resurrected))
    ;; delete old-properties from the form's resurrection info 
    (msf-form-put form 'fld-resurrection 
		 (delq old-properties res-info))))

;;;_ , Functions

;;;_  > msf-fld-insert (text form &optional properties)
(defun msf-fld-insert (text form &optional properties)
  (let ((begin (point-marker)))
    (insert text)
    (and properties
	 (add-text-properties begin (point) properties))
    (add-text-properties begin (point)
			 `(fld-form ,form))
    (when (> (point) (msf-form-end form))
      (move-overlay form (msf-form-start form) (point)))))

;;;_  > msf-fld-form (&optional pos)
(defun msf-fld-form (&optional pos)
  (msf-fld-get 'fld-form (or pos (point))))

;;;_  > msf-fld-start (&optional pos)
(defun msf-fld-start (&optional pos)
  (or pos (setq pos (point)))
  (let ((form (msf-fld-form pos)))
    (assert form)
    (if (eq (msf-fld-form (1- pos)) form)
	(previous-single-property-change 
	 pos 'fld-form nil (msf-form-start form))
      pos)))

;;;_  > msf-fld-end (&optional pos)
(defun msf-fld-end (&optional pos)
  (or pos (setq pos (point)))
  (let ((form (msf-fld-form pos)))
    (assert form)
    (next-single-property-change
     pos 'fld-form nil (msf-form-end form))))

;;;_  > msf-fld-set-text (text &optional pos)
(defun msf-fld-set-text (text &optional pos)
  (save-excursion
    (let ((form (msf-fld-form pos))
	  (old-properties (msf-fld-properties pos))
          (text (with-temp-buffer
                  (goto-char (point-min)) (insert text)
                  (buffer-substring-no-properties (point-min)
                                                  (point-max))))
	  (inhibit-modification-hooks t))
      (goto-char (msf-fld-start pos))
      (delete-region (msf-fld-start pos) (msf-fld-end pos))
      (msf-fld-insert text form old-properties))))
  
;;;_  > msf-fld-text (&optional pos)
(defsubst msf-fld-text (&optional pos)
  (buffer-substring (msf-fld-start pos) (msf-fld-end pos)))

;;;_  > msf-fld-text-no-properties (&optional pos)
(defsubst msf-fld-text-no-properties (&optional pos)
  (buffer-substring-no-properties (msf-fld-start pos)
				  (msf-fld-end pos)))

;;;_  > msf-fld-put (property value &optional pos)
(defsubst msf-fld-put (property value &optional pos)
  (put-text-property (msf-fld-start pos)
                     (msf-fld-end pos)
                     property value))

;;;_  > msf-fld-get (property &optional pos)
(defsubst msf-fld-get (property &optional pos)
  (get-text-property (or pos (point)) property))

;;;_  > msf-fld-set (properties &optional pos)
(defsubst msf-fld-set (properties &optional pos)
  (set-text-properties (msf-fld-start pos)
		       (msf-fld-end pos)
		       properties))

;;;_  > msf-fld-add (properties &optional pos)
(defsubst msf-fld-add (properties &optional pos)
  (add-text-properties (msf-fld-start pos)
		       (msf-fld-end pos)
		       properties))

;;;_  > msf-fld-remove (properties &optional pos)
(defsubst msf-fld-remove (properties &optional pos)
  (remove-text-properties (msf-fld-start pos)
                          (msf-fld-end pos)
                          properties))

;;;_  > msf-fld-properties (&optional pos)
(defsubst msf-fld-properties (&optional pos)
  (text-properties-at (or pos (point))))

;;;_  > msf-fld-forget (field)
(defun msf-fld-forget (field)
  (let ((form (msf-fld-form field))
        (link (msf-fld-get 'fld-link field))
        (category (msf-fld-get 'category field))
        (inhibit-modification-hooks t))
    (assert (and form link))
    (when (and link (not (eq category 'msf-fld-linked-category)))
      (msf-fld-make-real (msf-form-first-link 
                          form nil nil
                          (list 'fld-link link))))
    (msf-fld-set default-text-properties field)
    (unless (msf-form-first-fld form)
      (msf-form-delete form))))
  
;;;_  > msf-fld-make-real (field)
(defun msf-fld-make-real (field)
  (let ((form (msf-fld-form field))
        (link (msf-fld-get 'fld-link field))
        (inhibit-modification-hooks t)
        (real nil))
    (assert (and form link))
    (setq real (msf-form-first-real 
                form nil nil (list 'fld-link link)))
    (assert real)
    (msf-fld-set-text (msf-fld-text-no-properties real) field)
    (msf-fld-set (msf-fld-properties real) field)))

;;;_  > msf-fld-unlink (field)
(defun msf-fld-unlink (field)
  (let ((inhibit-modification-hooks t))
    (msf-fld-make-real field)
    (msf-fld-remove '(fld-link t fld-modification-hooks t) field)))

;;;_  > msf-fld-linked-replicate (form fld-pos &rest ignored)
(defun msf-fld-linked-replicate (form fld-pos &rest ignored)
  "Update the text for other linked fields with the same link name.

Called after text insertion/deletion on a linked field"
  (let ((new-txt (buffer-substring-no-properties
		  (msf-fld-start fld-pos)
		  (msf-fld-end fld-pos)))
	(link (msf-fld-get 'fld-link fld-pos))
	(field (msf-form-start form))
	(properties nil))
    (save-excursion
      (while (setq field (msf-form-first-fld
			   form field nil (list 'fld-link link)))
	(goto-char (setq field (msf-fld-start field)))
	(if (= (msf-fld-start fld-pos) field)
	    (setq field (msf-fld-end fld-pos)) ;; skip my self
	  (setq properties (msf-fld-properties field))
	  (delete-region (msf-fld-start field)
			 (msf-fld-end field))
	  (msf-fld-insert new-txt form
			  (append properties
				  '(fld-state replicated)))
	  (setq field (point)))))))

;;;_  > msf-fld-linked-before-resurrection
(defun msf-fld-linked-before-resurrection (form begin end)
  "Save information for other linked fields and delete them.

This must be called AFTER a linked field has been fully deleted"
  (let ((res-info (msf-form-get form 'fld-resurrection))
	(link (msf-fld-get 'fld-link begin))
	(field (msf-form-start form))
	(properties nil))
    (save-excursion
      (while (setq field (msf-form-first-fld
			  form field nil (list 'fld-link link)))
	(goto-char (setq field (msf-fld-start field)))
	(setq properties (msf-fld-properties field))
	(delete-region (msf-fld-start field)
		       (msf-fld-end field))
	(add-to-list 'res-info (cons (point-marker) properties))
	(setq field (point))))
    (msf-form-put form 'fld-resurrection res-info)))

;;;_  > msf-fld-linked-after-resurrection
(defun msf-fld-linked-after-resurrection (form begin end)
  "Resurrect other linked fields on a linked field resurrection"
  (let ((new-txt (buffer-substring-no-properties begin end))
	(res-info (msf-form-get form 'fld-resurrection))
	(link (msf-fld-get 'fld-link begin))
	(predicate nil))    
    (setq predicate 
	  (lambda (field)
	    (when (equal link (cadr (member 'fld-link 
					    (cdr field))))
	      (unless (= begin (car field))
		(save-excursion
		  (goto-char (car field))
		  (msf-fld-insert 
		   new-txt form 
		   (append (cdr field)
			   '(fld-state resurrected)))))
	      t)))    
    (msf-form-put form 'fld-resurrection
		 (delete-if predicate res-info))))

;;;_  > msf-choose-completing
(defun msf-choose-completing (prompt table &optional 
					  predicate require-match
                                          initial-input hist def 
                                          inherit-input-method)
  "Use a `completing-read' like function to make a selection"
  (let ((fun (cond 
              ((featurep 'ido) 'ido-completing-read)
              (t 'completing-read))))
    (apply fun prompt table predicate require-match 
           initial-input hist def inherit-input-method)))

;;;_  > msf-choose-selecting
(defun msf-choose-selecting (prompt choices-list &optional 
				    predicate initial-input 
				    hist existing-choice 
				    inherit-im)
  "Choose an item from a list using a simple selection buffer.

  This creates a new selection buffer with very limited 
  key-bindings:

  C-n     : Next option
  <down>  : Next option

  C-p     : Previous option
  <up>    : Previous option

  RET     : Select current option
  TAB     : Select current option
  "
  (let* (i map done o choice-index choice-info out)
    (setq map (make-sparse-keymap))
    (setq i 0)
    (setq choice-index 0)
    (kill-buffer (get-buffer-create " fld-choose"))
    (with-current-buffer (get-buffer-create " fld-choose")
      (erase-buffer)
      (insert "Make your choice, C-g aborts:\n\n")
      (mapc
       (lambda (c)
	 (insert c)
	 (setq choice-info (cons (cons i (list (line-number-at-pos)
						       (length c)))
					 choice-info))
	 (when (equal existing-choice c)
	   (setq choice-index i))
	 (setq i (1+ i))
	 (insert "\n"))
       choices-list)
      (setq choice-info (reverse choice-info))
      (goto-char (point-min))
      (forward-line 2)
      (forward-line choice-index)
      (setq o (make-overlay (line-beginning-position)
			    (+ (line-beginning-position)
			       (cadr (cdr (assoc choice-index choice-info))))))
      (overlay-put o 'face 'bold-italic))
    (save-window-excursion
      (delete-other-windows)
      (let ((buf (get-buffer " fld-choose")))
	(fit-window-to-buffer (display-buffer buf))
	(while (not done)
	  (let (
;; 		(cursor-in-echo-area t)
		(keys nil))
	    (setq keys (read-key-sequence-vector prompt))
;; 	    (message "keys are %s" keys)
	    (cond
	     ((or (equal keys [16]) ;; C-p
                  (equal keys [up]))
	      (when (> choice-index 0)
		(set-buffer buf)
		(setq choice-index (1- choice-index))
		(goto-line (car (cdr (assoc choice-index choice-info))))
		(move-overlay o
			      (line-beginning-position)
			      (+ (line-beginning-position)
				 (cadr (cdr (assoc choice-index
						  choice-info)))))))
	     ((or (equal keys [14]) ;; C-n
                  (equal keys [down]))
	      (when (< choice-index (1- i))
		(set-buffer buf)
		(setq choice-index (1+ choice-index))
		(goto-line (car (cdr (assoc choice-index choice-info))))
		(move-overlay o
			      (line-beginning-position)
			      (+ (line-beginning-position)
				 (cadr (cdr (assoc choice-index
						  choice-info)))))))
	     ((or (equal keys [9])   ;; tab
                  (equal keys [13])) ;; ret
	      (setq out (nth choice-index choices-list)
		    done t))
	     ((equal keys [7])
	      (setq done t))
	     (t nil))))))
    (message "")
    out))

;;;_ * Commands

;;;_  > msf-cmd-goto-root nil
(defun msf-cmd-goto-root nil
  (interactive)
  (let ((current-mode-str (format "%s" major-mode)))
    (if (assq major-mode msf-abbrev-mode-abbrevs)
	(dired (concat (file-name-as-directory msf-abbrev-root)
		       current-mode-str))
      (dired msf-abbrev-root))))

;;;_  > msf-cmd-define nil
(defun msf-cmd-define ()
  (interactive)
  (let* ((current-mode-str
	  (cond
	   ;; create an exception case for AUCTeX
	   ((and
	     (eq major-mode 'latex-mode)
	     (boundp 'AUCTeX-version))
	    "LaTeX-mode")
	   ((and
	     (eq major-mode 'tex-mode)
	     (boundp 'AUCTeX-version))
	    "TeX-mode")
	   (t (format "%s" major-mode))))
	 (d (concat (file-name-as-directory msf-abbrev-root)
		    current-mode-str)))
    (when (or (file-exists-p d)
	      (and (y-or-n-p
		    (format
		     "Could not find directory %s, create it? " d))
		   (progn
		     (make-directory d)
		     t)))
      (let ((name (msf-abbrev-choose-abbrev major-mode nil))
            (reloader
             (eval `(lambda nil 
                      (with-current-buffer ,(current-buffer)
                        (msf-abbrev-scan-mode major-mode 'replace)
                        (msf-abbrev-define-on-table 
                         major-mode local-abbrev-table)
                        (message "Abbrevs for %s reloaded." 
                                 major-mode)))))
            (find-file-hook
             (eval `(lambda nil 
                      (make-variable-buffer-local 
                       'after-save-hook)
                      (setq after-save-hook reloader)
                      (goto-char (point-max))
                      (insert ,(if mark-active
                                   (buffer-substring-no-properties
                                    (region-beginning)
                                    (region-end))
                                 ""))))))
	(find-file (concat (file-name-as-directory d) name))))))


;;;_  > msf-cmd-expand ()
(defun msf-cmd-expand ()
  (interactive)
  (let ((abbrev (msf-abbrev-choose-abbrev major-mode t)))
    (if abbrev
        (msf-abbrev-expand-abbrev abbrev major-mode)
      (message "Expansion canceled"))))

;;;_  > msf-cmd-goto-start
(defun msf-cmd-goto-start nil
  (interactive)
  (goto-char (msf-form-start-point (msf-form-at (point)))))

;;;_  > msf-cmd-goto-end
(defun msf-cmd-goto-end nil
  (interactive)
  (goto-char (msf-form-end-point (msf-form-at (point)))))

;;;_  > msf-cmd-unlink
(defun msf-cmd-unlink nil
  (interactive)
  (let (form field)
    (setq form (or (msf-fld-form (setq field (point)))
                   (msf-fld-form (setq field (1- (point))))))
    (assert (and form field))
    (save-excursion
      (msf-fld-unlink field))))

;;;_  > msf-cmd-clean-field
(defun msf-cmd-clean-field nil
  (interactive)
  (let (form field)
    (setq form (or (msf-fld-form (setq field (point)))
                   (msf-fld-form (setq field (1- (point))))))
    (assert (and form field))
    (save-excursion
      (msf-fld-forget field))))

;;;_  > msf-cmd-end-form 
(defun msf-cmd-end-form nil
  (interactive)
  (let ((form (msf-form-at (point))))
    (goto-char (msf-form-end-point form))
    (msf-form-delete form)))

;;;_  > msf-cmd-clean-form
(defun msf-cmd-clean-form nil
  (interactive)
  (let ((form (msf-form-at (point))))
    (msf-form-delete form)))

;;;_  . msf-cmd-goto-next (scanner)
(defun msf-cmd-goto-next (scanner)
  (let ((form (msf-form-at (point)))
	(start (if (msf-fld-form (point))
		   (msf-fld-end (point))
		 (point)))
	(next nil))
    (assert form)
    (if (setq next (apply scanner (list form start)))
	(goto-char (msf-fld-start next))
      (if msf-abbrev-goto-next-loops
          (goto-char (apply scanner (list form)))
        ;; No more fields in the form, delete the form 
        ;; and go to the end point
        (call-interactively 'msf-cmd-end-form)))))

;;;_  . msf-cmd-goto-previous
(defun msf-cmd-goto-previous (scanner)
  (let (form end prev)
    (setq form (or (msf-fld-form (setq end (point)))
                   (msf-fld-form (setq end (1- (point))))))
    (assert form)
    (setq end (msf-fld-start end))
    (if (setq prev (apply scanner (list form nil end)))
	(goto-char (msf-fld-start prev))
      ;; No previous field, go to the last one
      (setq prev (apply scanner (list form)))
      (if prev
	  (goto-char (msf-fld-start prev))
	(display-warning 'msf-abbrev
                         "No previous field in form"
                         :warning)))))

;;;_  > msf-cmd-next-fld
(defun msf-cmd-next-fld nil
  (interactive)
  (msf-cmd-goto-next 'msf-form-first-fld))

;;;_  > msf-cmd-previous-fld
(defun msf-cmd-previous-fld nil
  (interactive)
  (msf-cmd-goto-previous 'msf-form-last-fld))

;;;_  > msf-cmd-next-real
(defun msf-cmd-next-real nil
  "Go to the next real field in the form at point.

If no field exists after point, go to the end point and
delete the form."
  (interactive)
  (msf-cmd-goto-next 'msf-form-first-real))

;;;_  > msf-cmd-previous-real
(defun msf-cmd-previous-real nil
  "Go to the previous real field in FORM"
  (interactive)
  (msf-cmd-goto-previous 'msf-form-last-real))

;;;_  > msf-cmd-make-choice
(defun msf-cmd-make-choice nil
  (interactive)
  (let (form field choices selection)
    (setq form (or (msf-fld-form (setq field (point)))
		   (msf-fld-form (setq field (1- (point))))))
    (assert (and form field))
    (setq field (set-marker (make-marker) field))
    (setq selection (msf-fld-text-no-properties field))
    (setq choices (msf-fld-get 'fld-choices field))
    (setq selection
	  (apply msf-abbrev-choose-method
                 (concat "Choose a field value and press RET."
                         " Navigate using C-n and C-p")
                 (copy-list choices) nil t
                 nil nil selection nil))
    (when (member selection choices)
      (msf-fld-set-text selection field)
      (msf-form-on-modification ;; notify about replacement
       form 'after (msf-fld-start field)(msf-fld-end field) 0)
      (goto-char (msf-fld-end field)))))

;;;_  > msf-cmd-make-editable
(defun msf-cmd-make-editable nil
  "Like setting `msf-abbrev-first-insertion-replaces' to nil just 
for the field at point."
  (interactive)
  (let (form field (inhibit-modification-hooks t))             
    (setq form (or (msf-fld-form (setq field (point)))
                   (msf-fld-form (setq field (1- (point))))))
    (assert (and form field))
    (msf-fld-add '(fld-state editable) field)))
    
;;;_* Provide
(provide 'msf-abbrev)

;;;_* Local variables
;; Local Variables:
;;  indent-tabs-mode: nil
;;  allout-layout: (1 -1 1 1 1 -1 :)
;; End:

;;; msf-abbrev.el ends here
