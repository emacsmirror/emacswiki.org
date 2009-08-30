;;; ectags-select.el --- Select from multiple tags

;; Copyright (C) 2007  Scott Frazer and (C) 2008 John Connors

;; Author: John Connors <johnc@yagc.ndo.co.uk>
;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: John Connors <johnc@yagc.ndo.co.uk>
;;
;; Keywords: exuberant-ctags ectags tag select

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;; Reworking of etags-select to work with exuberant-ctags and eldoc.
;; ectags-tag-directory will generate a tag file in the current directory
;; with the specified language using exuberant-ctags extended tag format
;; which contains useful information about the current tag such as 
;; the signature, it's parent class, which class it is a member of. Eldoc
;; displays this information. At present it does not do a very good job
;; of finding the best candidate tag in an OO language where there may
;; be multiple tags with the same name. It tries, however. The tag file
;; needs to be in a specific format, hence the ectags-tag-directory-command.
;; ectags-visit-tags-table is used to load in a tag table.

;; Open a buffer with file/lines of exact-match tags shown.  Select one by
;; going to a line and pressing return.  pop-tag-mark still works with this
;; code.
;;
;; An up to date version of this code lives on repo.or.cz 
;; at git://repo.or.cz/ectags.git

;; 
;;; Code:

;
;; TO DO : tag completion DONE
;; TO DO : tags search DONE
;; TO DO : eldoc integration DONE
;; TO DO : more than one lang per dir DONE
;; TO DO : compile window for ectags DONE
;; TO DO : do sthing about ginormous tag files
;; TO DO : .. use abbreviated info
;; TO DO : .. use gzipped tag files
;; TO DO : use search for locating tag rather than line #
;; TO DO : some kind of higlighting for select buffer
;; TO DO : include line matched when searching for references
;; TO DO : investigate CEDET to see if it can guide eldoc to a better
;;        match for the  tag under the cursor

(require 'cl)
(require 'custom)
(require 'easymenu)

;; Asm      *.asm *.ASM *.s *.S *.A51 *.29[kK] *.[68][68][kKsSxX] *.[xX][68][68]
;; Asp      *.asp *.asa
;; Awk      *.awk *.gawk *.mawk
;; BETA     *.bet
;; C        *.c
;; C++      *.c++ *.cc *.cp *.cpp *.cxx *.h *.h++ *.hh *.hp *.hpp *.hxx *.C *.H
;; C#       *.cs
;; Cobol    *.cbl *.cob *.CBL *.COB
;; Eiffel   *.e
;; Erlang   *.erl *.ERL *.hrl *.HRL
;; Fortran  *.f *.for *.ftn *.f77 *.f90 *.f95 *.F *.FOR *.FTN *.F77 *.F90 *.F95
;; HTML     *.htm *.html
;; Java     *.java
;; JavaScript *.js
;; Lisp     *.cl *.clisp *.el *.l *.lisp *.lsp *.ml
;; Lua      *.lua
;; Make     *.mak *.mk [Mm]akefile
;; Pascal   *.p *.pas
;; Perl     *.pl *.pm *.plx *.perl
;; PHP      *.php *.php3 *.phtml
;; Python   *.py *.python
;; REXX     *.cmd *.rexx *.rx
;; Ruby     *.rb *.ruby
;; Scheme   *.SCM *.SM *.sch *.scheme *.scm *.sm
;; Sh       *.sh *.SH *.bsh *.bash *.ksh *.zsh
;; SLang    *.sl
;; SML      *.sml *.sig
;; SQL      *.sql
;; Tcl      *.tcl *.tk *.wish *.itcl
;; Vera     *.vr *.vri *.vrh
;; Verilog  *.v
;; Vim      *.vim
;; YACC     *.y

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff


;;;###autoload
(defgroup ectags nil
  "Exuberant Ctags Support for Emacs"
  :version "22.1.1"
  :group 'tools)

;;;###autoload  
(defcustom ectags-command "ectags"
  "Name of the exuberant ctags executable on your system"
  :type 'string
  :group 'ectags)

;;;###autoload
(defcustom ectags-config-file "~/.ectags"
  "Name of the exuberant-ctags configuration file."
  :type 'string
  :group 'ectags)

;;;###autoload
(defcustom ectags-language-file-suffix-alist
  '(( "asp"    .  ( "*.asp" "*.asa" ))
    ( "awk"    .  ( "*.awk" "*.gawk" "*.mawk"))
    ( "c"      .  ( "*.c" "*.h" ))
    ( "c++"    .  ( "*.c++" "*.cc" "*.cp" "*.cpp"  "*.cxx" "*.h" "*.h++" "*.hh" "*.hp" "*.hpp" "*.hxx" "*.c"  "*.C" "*.h" "*.H"))
    ( "c#"     .  ( "*.cs" ))
    ( "java"   .  ( "*.java " ))
    ( "lisp"   .  (  "*.cl" "*.clisp" "*.el" "*.l" "*.lisp" "*.lsp" "*.ml"))
    ( "python" .  ( "*.py" "*.python" ))
    ( "SQL"    .  (  "*.sql" ))
    ( "Tcl"    .  ( "*.tcl" "*.tk" "*.wish" "*.itcl" )))
  "Association list defining file masks for languages"
  :type 'alist
  :group 'ectags)

;;;###autoload
(defcustom ectags-system-tag-table-list nil
  "List of tags tables that include system headers"
  :type 'list
  :group 'ectags)

;;;###autoload
(defcustom ectags-api-files
  '(( "wx"    .  "/usr/local/include/wx" )
    ( "gtk"    .  "/usr/include/gtk-2.0" )
    ( "glib"      . "/usr/include/glib-2.0" ))
  "Association list mapping apis to directories"
  :type 'alist
  :group 'ectags)


;;;###autoload
(defcustom ectags-select-mode-hook nil
  "*List of functions to call on entry to ectags-select-mode mode."
  :group 'ectags
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar *ectags-matches* nil
  "List of candiate tag matches")

(defvar *ectags-regexp* nil
  "Holds regexp currently being sought in tags")

(defvar *ectags-max-candidates* 7
  "How many candidates to select between")

(defvar *ectags-case-sensitive* t
  "Is the tag matching case sensitive?")

(defvar *ectags-autopop-tag* t
  "If non-nil, automatically pop the tag off the tag stack when jumped to")

(defvar *ectags-tag-stack* nil
  "Stack of tag positions for browsing.")

(defvar *ectags-obarray* nil
  "Obarray used for ectags completions.")

(defvar *ectags-select-buffer-name* "*ectags-select*"
  "ectags-select buffer name.")

(defvar *ectags-reference-buffer-name* "*ectag References*"
  "ectags-reference buffer-name")


(defvar ectags-select-mode-font-lock-keywords nil
  "ectags-select font-lock-keywords.")

(defvar *ectags-select-source-buffer* nil
  "ectags-select source buffer tag was found from.")

(defvar *ectags-reference-source-buffer* nil
  "ectags-reference source buffer tag was found from.")

(defvar *ectags-select-opened-window* nil
  "ectags-select opened a select window.")

(defvar *ectags-reference-opened-window* nil
  "ectags-referecnce opened a reference window.")


(defvar *ectags-scan-marks* nil
  "Holds markers where matches found.")

(defconst ectags-select-non-tag-regexp "\\(\\s-*$\\|In:\\|Finding tag:\\)"
  "ectags-select non-tag regex.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

;; Klaus Berndl <klaus.berndl@sdm.de>: we have to take account that GNU Emacs
;; > 21.3 has changed its split-string function! For the new split-string is
;; (cdr (split-string ...)) not nil (at least in our context below), for GNU
;; Emacs <= 21.3 nil!
(defun ectags-left-trim (str)
  "Return a string stripped of all leading whitespaces of STR."
  (let ((split-result (split-string str "^[\n\t ]*")))
    (or (or (and (cdr split-result) ;; GNU Emacs > 21.3
                 (car (cdr split-result)))
            (car split-result))
        "")))

(defun ectags-right-trim (str)
  "Return a string stripped of all trailing whitespaces of STR."
  (or (car (split-string str "[\n\t ]*$")) ""))

(defun ectags-trim (str)
  "Applies `ectags-right-trim' and `ectags-left-trim' to STR."
  (ectags-left-trim (ectags-right-trim str)))

(defun ectags-full-trim (str)
  "Applies `ectags-trim' and `ectags-middle-trim' to STR."
  (ectags-excessive-trim (ectags-trim str)))

;; creating tag files  ------------------------------------------------------------------------------------
          

(defun make-suffix-clauses (languages)
  (mapcar (lambda (l)
            (mapcar (lambda (s)
                      (concat  " -iname \"" s "\""))
                    (cdr (assoc-string l ectags-language-file-suffix-alist)))) 
          (split-string languages)))

(defun make-shell-command-prefix (directory)
  (concat "find " (expand-file-name directory)))

(defun make-tag-file-name (directory)
  (expand-file-name (concat directory (string directory-sep-char) "tags")))

(defun ectag-directory-command (directory languages)
  "Produce a command needed to scan the given directory for files
   of the given language and produce tags"
  (let*
      ((suffix-clauses
        (car (make-suffix-clauses languages)))
       (shell-command-prefix 
        (make-shell-command-prefix directory))
       (shell-command-suffix 
        (concat " | " ectags-command " -o "  (make-tag-file-name directory) " --options=" 
                (expand-file-name ectags-config-file) " --verbose --excmd=n --extra=+fq --fields=+afiKlmnsSzt --file-scope=no -L -")))
    (concat shell-command-prefix
            (car suffix-clauses)
            (apply 'concat 
                   (mapcar (lambda (s)
                             (concat " -o" s))
                           (cdr suffix-clauses)))
            shell-command-suffix)))



;;;###autoload
(defun ectags-tag-directory ()
  "Prompt for a directory and a langage and create a tag file."
  (interactive)
  ;; prompt for directory
  (let ((tag-directory
         (read-directory-name "Directory to tag? " default-directory))
        (tag-languages (completing-read "Languages to tag? " ectags-language-file-suffix-alist nil nil)))    
    (add-to-list 'compilation-error-regexp-alist
		 '("^\\([^:]+\\) confusing argument declarations beginning at line \\([0-9]+\\))" 1 2))
    (compile (ectag-directory-command tag-directory tag-languages) t)))


;; building tag completion obarray --------------------------------------------------

(defun extract-ectags (&optional tag-buffer obarray)
  "Extract a list of tags from a buffer"
  (save-excursion
    (when tag-buffer
      (set-buffer tag-buffer))
    (goto-char (point-min))
    (forward-line 5)
    ;; now point is at first tag
    (while (/= (point) (point-max))
      (forward-line)
      (beginning-of-line)
      (let* ((start (point-marker))
             (end (progn (forward-word) (point-marker))))
        (intern-soft (buffer-substring-no-properties start end) obarray)
      (end-of-line))))
  obarray)

(defun extract-ectags-files (&optional tag-buffer)
  "Extract a list of tags from a tag-buffer"
  (let ((result nil))
    (save-excursion
      (when tag-buffer
        (set-buffer tag-buffer))
      (goto-char (point-min))
      (forward-line 5)
      ;; now point is at first tag
      (while (search-forward "kind:file" (point-max) t)
        (beginning-of-line)
        (when (search-forward "	" (point-max) t)          
          (let* ((start 
                  (point-marker))
                 (end (progn 
                        (search-forward "	" (point-max) t) 
                        (backward-char) 
                        (point-marker))))
            (add-to-list 'result (buffer-substring-no-properties start end)))
          (end-of-line))))      
      result))
        

(defun make-ectags-obarray ()
  (let ((result (make-vector 65535 0)))
    (mapcar (lambda (b)
              (when (bufferp b)
                (save-excursion 
                  (extract-ectags b result))))
              (ectags-table-list))
    (setq *ectags-obarray* result)))
    
(defun flatten-file-list (l)
  (let (result stack)
    (while (or stack l)
      (if l
          (if (consp l)
              (setq stack (cons (cdr l)
                                stack)
                    l (car l))
            (setq result (cons l result)
                  l nil))
        (setq l (car stack)
              stack (cdr stack))))
    result)) 

(defun make-ectags-file-list ()
  "Create a list of all files in the tags"
  (let ((result nil))
    (setq result
           (mapcar (lambda (b) (extract-ectags-files b)) (ectags-table-list)))
    (flatten-file-list result)))

;; tags table mode ------------------------------------------------------------------------------------

(defun ectags-table-list ()
  "Return a list of available tag tables."
  (let (tags-table-list)
    (dolist (buffer (buffer-list) tags-table-list)
      (when (assoc 'is-ectag-table (buffer-local-variables buffer))
        (push buffer tags-table-list)))
    tags-table-list))


(defvar ectags-table-mode-syntax-table 
  (let ((ectags-syntax-table text-mode-syntax-table))
    (modify-syntax-entry ?_ "w" ectags-syntax-table)
    (modify-syntax-entry ?- "w" ectags-syntax-table)
    (modify-syntax-entry ?# "w" ectags-syntax-table)
    (modify-syntax-entry ?! "w" ectags-syntax-table)
    (modify-syntax-entry ?\" "w" ectags-syntax-table)
    (modify-syntax-entry ?& "w" ectags-syntax-table)
    (modify-syntax-entry ?< "w" ectags-syntax-table)
    (modify-syntax-entry ?\( "w" ectags-syntax-table)
    (modify-syntax-entry ?\) "w" ectags-syntax-table)
    (modify-syntax-entry ?: "w" ectags-syntax-table)
    (modify-syntax-entry ?\; "w" ectags-syntax-table)
    (modify-syntax-entry ?? "w" ectags-syntax-table)
    (modify-syntax-entry ?@ "w" ectags-syntax-table)
    (modify-syntax-entry ?\ "w" ectags-syntax-table)
    (modify-syntax-entry ?\[ "w" ectags-syntax-table)
    (modify-syntax-entry ?\] "w" ectags-syntax-table)
    (modify-syntax-entry ?\{ "w" ectags-syntax-table)
    (modify-syntax-entry ?\} "w" ectags-syntax-table)    
    (modify-syntax-entry ?| "w" ectags-syntax-table)
    (modify-syntax-entry ?\' "w" ectags-syntax-table)
    (modify-syntax-entry ?^ "w" ectags-syntax-table)
    (modify-syntax-entry ?, "w" ectags-syntax-table)
    (modify-syntax-entry ?` "w" ectags-syntax-table)
    (modify-syntax-entry ?~ "w" ectags-syntax-table)
    ectags-syntax-table)
  "Punctuation free table")
  

;;;###autoload
(defun ectags-table-mode ()
  "Major mode for exuberant ctags table file buffers."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ectags-table-mode)
  (set-syntax-table ectags-table-mode-syntax-table)
  (setq mode-name "ECTags Tags Table")
  (set (make-local-variable 'is-ectag-table) t))



;; removing tags tables ------------------------------------------------------------------------------------

(defun ectags-wipe-tag-tables ()
  "Wipe out all ectags tables"
  (interactive)
  (mapcar 
   (lambda (x) 
     (when (bufferp x) (progn (bury-buffer x) (kill-buffer x)))) 
   (ectags-table-list))  
  (setq *ectags-obarray* nil))

;; -- adding tags tables ---------------------------------------------------------------------------------


;; Expand tags table name FILE into a complete file name.
(defun ectags-expand-table-name (file)
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (expand-file-name "tags" file)
    file))

;; Return non-nil iff the current buffer is a valid etags TAGS file.
(defun ectags-verify-tags-table ()
  "Is current buffer actually an ectags table."
  ;; Use eq instead of = in case char-after returns nil.
  (goto-char (point-min))
  (looking-at "!_TAG_FILE_FORMAT"))

(defun ectags-verify-table (file)
  "Given a file, read it in to a buffer and validate it as a tags table."
  (save-excursion
    (message "Validating tags table %s " file)
    (if (get-file-buffer file)  
        (progn
          (set-buffer (get-file-buffer file))
          (unless (ectags-verify-tags-table)
            (fundamental-mode)))
      (when (file-exists-p file)
        (progn 
          (set-buffer (find-file-noselect file))
          (when (ectags-verify-tags-table)
            (ectags-table-mode)))))
    (assoc 'is-ectag-table (buffer-local-variables))))

;;;###autoload
(defun ectags-visit-tags-table (name)
  "Visit an exuberant ctags file and add it to the current list of tags tables."
  (interactive
   (list (read-file-name "Visit tags table (default tags): "
				     default-directory
				     (expand-file-name "tags"
						       default-directory)
				     t)))
  (let ((curbuf (current-buffer))
        (local-tags-filename (ectags-expand-table-name name)))
    (if (ectags-verify-table local-tags-filename)
        ;; We have a valid tags table.
        (progn (message "Valid tags table")
               (setq *ectags-obarray* (make-ectags-obarray)))
      ;; The buffer was not valid.  Don't use it again.
      (progn (error "Not a valid tags table")))))


;; -- saving and reloading sets of tag tables ------------------------------------------------
  

(defun ectags-output (tag-buffer)
  "Output a line needed to restore this table to the tags buffer list"
  (insert "(add-working-ectags-table " (buffer-file-name tag-buffer) ")\n"))

(defun save-working-ectags-tables (fname)
  "Save the current working list of ectags tables in a file"
  (interactive "fFile to save tags tables in?:")
  (save-excursion
    (with-temp-buffer
      (insert
       ";; -*- mode: fundamental; coding: emacs-mule; -*-\n"
       ";; Created " (current-time-string) "\n"
       ";; Emacs version " emacs-version "\n\n"
       (dolist (tagbuff (ectags-table-list))
         (ectags-output tagbuff))
       "\;;")
      (write-region (point min) (point-max) fname nil 'nomessage))))

(defun read-working-ectags-tables (fname)
  "Read the current working list of ectags tables in a file"
  (interactive "fFile to read tags tables from?:")  
  (load fname))


;; actually finding tags and so forth ------------------------------------------------------------------------------------------------

(defun ectags-match-tagname (tag-match)
  (nth 1 tag-match))

(defun ectags-match-filename (tag-match)
  (nth 2 tag-match))

(defun ectags-match-linenumber (tag-match)
  (nth 3 tag-match))

(defun ectags-match-tag-info (tag-match)
  (nth 4 tag-match))

(defun ectags-fname (tag-match))


(defun match-ectags (tag fname lnumber info)
  "Given a tags match, rank it (via regexp match length) and
plonk it in the match candidates."
  (let*
      ((saved-fold-search case-fold-search)
       (case-fold-search (not *ectags-case-sensitive*))
       (match-rank (string-match *ectags-regexp* tag)))
    (when match-rank
      (let 
         ((full-match-rank (- (length tag) (length *ectags-regexp*))))
;;        (message (format "Found %s ranking %d " tag full-match-rank))
        (add-to-list '*ectags-matches* 
                     (list 
                      full-match-rank 
                      tag 
                      fname 
                      (string-to-number lnumber) 
                      (ectags-trim info)))))
    (setq case-fold-search saved-fold-search)))

(defun scan-ectag (fn tag-buffer)
  "Scan a tag table buffer for a match with a tag. Applies fn to all matches."
  (save-excursion
    (set-buffer tag-buffer)
    (goto-char (point-min))
    (while (re-search-forward (format "^\\([^	]*%s[^	]*\\)	\\([^	]+\\)	\\([0-9]+\\);\"\\(.+\\)$" *ectags-regexp*)  nil t)
      (apply fn (list (match-string-no-properties 1)
                      (match-string-no-properties 2)
                      (match-string-no-properties 3)
                      (match-string-no-properties 4))))))

(defun find-ectag (fn tag-buffer)
  "Scan a tag table buffer for an exact match with a tag"
  (save-excursion
    (set-buffer tag-buffer)
    (goto-char (point-min))
    (while (re-search-forward (format "^\\(%s\\)	\\([^	]+\\)	\\([0-9]+\\);\"\\(.+\\)$" *ectags-regexp*)  nil t)
      (apply fn (list (match-string-no-properties 1)
                      (match-string-no-properties 2)
                      (match-string-no-properties 3)
                      (match-string-no-properties 4))))))

(defun seek-ectag  (regexp locate-fn)
  "Seek a match for the current regexp with the tags in the current tag table buffer"
  (setq *ectags-matches* nil)
  (setq *ectags-regexp* regexp)
  (dolist (tags-buffer (ectags-table-list))
    (funcall locate-fn 'match-ectags tags-buffer)
    (setq *ectags-matches*
          (sort *ectags-matches* '(lambda (x y)
                                    (< (car x) (car y)))))))
  

 ;; hiipe expand tag ----------------------------------------------------------
(defun he-ectag-beg ()
  (let ((p
         (save-excursion 
           (backward-word 1)
           (point))))
     p))

;;;###autoload
(defun try-expand-ectag (old)
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list 
          (sort
           (all-completions he-search-string *ectags-obarray*) 'string-lessp))
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table))
      (setq he-expand-list (cdr he-expand-list))))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))



;; ectags search  ---------------------------------------------------------------------------------------------


(defun ectags-file-scan (file-list tag)
  "Scan the list of files for the tag and return a list of markers where it is found"
  (let ((result)
        (found))
    (loop
     for file in file-list
     do (save-excursion
          (message "Scanning %s " file)
          (find-file file)
          (setq found nil)
          (while (search-forward tag (point-max) t)
            (setq found t)
            (message "Found in %s " file)
            (add-to-list 'result (list file (line-number-at-pos (point)))))          
          (kill-buffer nil)))
    result))

(defun reference-ectag (tag)
  "Scan all currently tagged files for a tag and return a list of markers"
  (let*  ((file-list (make-ectags-file-list)))
    (setq *ectags-scan-marks* (ectags-file-scan file-list tag))))
    
(defun next-ectag-reference ()
  "Goto next ectag reference in current list, used as with tags-loop-continue"
  (interactive)
  (if (not (zerop (length *ectags-scan-marks*)))
    (let ((mark (car *ectags-scan-marks*)))
      (find-file (car mark))
      (forward-line (cadr mark))
      (setq *ectags-scan-marks* (cdr *ectags-scan-marks*)))
    (let ((ref-tag
           (or  (find-tag-default)
                (completing-read "Tag to reference " *ectags-obarray*))))
      (reference-ectag (tag))
      (when (not (zerop (length *ectags-scan-marks*)))
        (next-ectag-reference)))))


(defun insert-ectag-references (tagname)
  "Insert a refererence to a tag in an ectags-select buffer"
  (loop
   for index from 0 below (length *ectags-scan-marks*)
   do 
   (let ((mark (nth index *ectags-scan-marks*))) 
     (insert "<" (int-to-string index) ">:["
             tagname " in " 
             (car mark) "@" 
             (int-to-string (cadr mark))  "]\n" 
             "*"  "\n"))))

(defun list-ectag-references (tag)
  "List all references to the tag in a suitable buffer"
  (setq *ectags-scan-marks* nil)
  (setq *ectags-reference-source-buffer* (buffer-name))
  (get-buffer-create *ectags-reference-buffer-name*)
  (set-buffer *ectags-reference-buffer-name*)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Finding tag: " tagname "\n")
  (reference-ectag tag)
  (if (not (zerop  (length *ectags-scan-marks*)))
      (progn
        (insert-ectag-references tag)
        (set-buffer *ectags-reference-buffer-name*)
        (goto-char (point-min))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (setq *ectags-reference-opened-window* (selected-window))
        (unless (get-buffer-window *ectags-reference-buffer-name*)
          (select-window (split-window-vertically))
          (switch-to-buffer *ectags-reference-buffer-name*)
          (ectags-select-mode))
        (shrink-window-if-larger-than-buffer))
    (progn
      (message "Failed to find any references to tag %s " tagname)
      (ding))))
  

;; ectags mode ------------------------------------------------------------------------------------------------

(defun ectags-select-case-fold-search ()
  (when (boundp 'tags-case-fold-search)
    (if (memq tags-case-fold-search '(nil t))
        tags-case-fold-search
      case-fold-search)))

(defun ectags-select-insert-matches (tagname)
  (when *ectags-matches* 
    (set-buffer *ectags-select-buffer-name*)
    (loop for index from 0 below (min (length *ectags-matches*) *ectags-max-candidates*)
          do
          (let ((mtch (nth index *ectags-matches*)))
            (insert "<" (int-to-string index) ">:["
                    (ectags-match-tagname mtch) " in " 
                    (ectags-match-filename mtch) "@" 
                    (int-to-string (ectags-match-linenumber mtch))  "]\n" 
                    "*" (ectags-match-tag-info mtch) "\n")))))

(defun ectags-select-find (tagname)
  "Actually find a list of tags and push them into the tags select buffer"
  (setq *ectags-select-source-buffer* (buffer-name))
  (get-buffer-create *ectags-select-buffer-name*)
  (set-buffer *ectags-select-buffer-name*)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Finding tag: " tagname "\n")
  (seek-ectag tagname 'scan-ectag)
  (if (>  (length *ectags-matches*) 0)
      (progn  (ectags-select-insert-matches tagname)
              (set-buffer *ectags-select-buffer-name*)
              (goto-char (point-min))
              (ectags-select-next-tag)
              (set-buffer-modified-p nil)
              (setq buffer-read-only t)
              (setq *ectags-select-opened-window* (selected-window))
              (unless (get-buffer-window *ectags-select-buffer-name*)
                (select-window (split-window-vertically))
                (switch-to-buffer *ectags-select-buffer-name*)
                (ectags-select-mode))
              (shrink-window-if-larger-than-buffer))
    (progn
      (message "Failed to find tag: %s " tagname)
      (ding))))


(defun ectags-select-goto-tag ()
  "Goto the tag we currently have the point over in an ectags select mode window"
  (interactive)
  (let ((case-fold-search (not *ectags-case-sensitive*)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Finding tag: \\(.*\\)$")
      (setq tagname (match-string-no-properties 1)))
    (beginning-of-line)
    (if (not (looking-at "<"))
        (message "Please put the cursor on a line with a tag")
      (setq tag-point (point))
      (setq overlay-arrow-position (point-marker))
      (re-search-forward "\\[\\([^ ]+\\) in \\([^@]+\\)@\\([0-9]+\\)")
      (let ((tag (match-string-no-properties 1))
            (fname (match-string-no-properties 2))
            (lnno (match-string-no-properties 3)))
        (ring-insert find-tag-marker-ring (point-marker))
        (find-file-other-window fname)
        (goto-char (point-min))
        (forward-line (1- (string-to-int lnno)))))))


(defun ectags-select-next-tag ()
  "Move to next tag in buffer."
  (interactive)
  (beginning-of-line)
  (forward-line))


(defun ectags-select-previous-tag ()
  "Move to previous tag in buffer."
  (interactive)
  (beginning-of-line)
  (forward-line -1))


(defun ectags-select-quit ()
  "Quit ectags-select buffer."
  (interactive)
  (kill-buffer nil)
  (delete-window))

(defun ectags-select-by-tag-number (first-digit)
  (let ((tag-num (read-from-minibuffer "Tag number? " first-digit))
        (current-point (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^<" tag-num ">") nil t)
;; TODO -- need to push tag and close window
        (ectags-select-goto-tag)
      (goto-char current-point)
      (message (concat "Couldn't find tag number " tag-num))
      (ding))))


;; user commands ------------------------------------------------------------------------------------------------------

;;;###autoload
(defun ectags-select-find-tag-at-point ()
  "Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tag-to-find 
         (or (find-tag-default)
             (completing-read "Tag to find" *ectags-obarray*))))
  (ectags-select-find tag-to-find)))

;;;###autoload
(defun ectags-select-reference-tag-at-point ()
  "Do a search for tag in all files in tags tables and list all hits"
  (interactive)
  (let ((tag-to-find 
         (or (find-tag-default)
             (completing-read "Tag to find" *ectags-obarray*))))
    (list-ectag-references tag-to-find)))


;;;###autoload
(defun ectags-select-find-tag ()
  "Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tagname (read-from-minibuffer
                  (format "Find tag (default %s): " (find-tag-default)) nil nil 
                  nil 'find-tag-history)))
    (when (string= tagname "")
      (setq tagname (find-tag-default)))
    (ectags-select-find tagname)))

;;;###autoload
(defun ectags-select-reference-tag ()
  "Do a find-tag, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tagname (read-from-minibuffer
                  (format "Find tag (default %s): " (find-tag-default)) nil nil 
                  nil 'find-tag-history)))
    (when (string= tagname "")
      (setq tagname (find-tag-default)))
    (list-ectag-references tagname)))

;; eldoc mode ------------------------------------------------------------------------------------------------


;;;###autoload
(defun c-eldoc-scope ()
  "Try to figure out our scope"
  (save-excursion
    (c-end-of-defun)
    (c-beginning-of-defun-1)
    (forward-line -1)
    (c-syntactic-re-search-forward "::")
    (backward-char 2)
    (when (c-on-identifier)
      (let* ((id-end (point))
             (id-start (progn (backward-char 1) (c-beginning-of-current-token) (point))))
            (buffer-substring-no-properties id-start id-end)))))
    
;; finds the current function and position in argument list
;;;###autoload
(defun c-eldoc-function (&optional limit)
  (let* ((literal-limits (c-literal-limits))
         (literal-type (c-literal-type literal-limits)))
    (save-excursion
      ;; if this is a string, move out to function domain
      (when (eq literal-type 'string)
        (goto-char (car literal-limits))
        (setq literal-type nil))
      (if literal-type
          nil
        (when (c-on-identifier)
          (let* ((id-on (point-marker))
                 (id-start 
                  (progn (c-beginning-of-current-token) 
                         ;; are we looking at a double colon?
                         (if (and (= (char-before)  ?:)
                                  (= (char-before (1- (point))) ?:))
                             (progn
                               (backward-char 3)
                               (c-beginning-of-current-token)
                               (point-marker))
                           (point-marker))))
                 (id-end 
                  (progn 
                    (goto-char id-on)
                    (forward-char) 
                    (c-end-of-current-token) 
                    (point-marker))))
            (buffer-substring-no-properties id-start id-end)))))))

;; non scoped verison for more conservative languages
;;;###autoload
(defun ectags-eldoc-print-current-symbol-info ()
  "Print the ectags info associated with the current eldoc symbol"
  (let* ((eldoc-sym (c-eldoc-function (- (point) 1000))))
    (seek-ectag eldoc-sym 'find-ectag)
    (if (> (length *ectags-matches*) 0)
        (ectags-match-tag-info (car *ectags-matches*))     
      (format "Unknown %s " eldoc-sym))))

;; scoped version for cpp and the like : tries to find symbol in current scope first
;; scope format is a format string that concatenates the cureend scope and the symbol with the scope operator
;; eg "%s::%s" for c++
;;;###autoload
(defun ectags-eldoc-print-current-scoped-symbol-info ()
  "Try to find the meaning of the symbol in the current scope. Probably only useful for cpp mode"
  (let* ((eldoc-scope (c-eldoc-scope))
         (eldoc-sym (c-eldoc-function (- (point) 1000))))
    (when eldoc-sym
      (seek-ectag (format "%s::%s" eldoc-scope eldoc-sym) 'find-ectag)
      (if (> (length *ectags-matches*) 0)
          (format "%s::%s %s" eldoc-scope eldoc-sym (ectags-match-tag-info (car *ectags-matches*)))
        (progn
          (seek-ectag eldoc-sym 'find-ectag)
          (if (> (length *ectags-matches*) 0)
              (format "%s %s" eldoc-sym (ectags-match-tag-info (car *ectags-matches*)))
            (if eldoc-scope
                (format "Scope %s " eldoc-scope))
            (format "Unknown %s " eldoc-sym)))))))

;;;###autoload
(defun ectags-turn-on-eldoc-mode (&optional scope-format)
  (interactive)
  (if scope-format
      (set (make-local-variable 'eldoc-documentation-function)
           'ectags-eldoc-print-current-scoped-symbol-info)
      (set (make-local-variable 'eldoc-documentation-function)
           'ectags-eldoc-print-current-symbol-info)
  (turn-on-eldoc-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap


(defvar ectags-select-mode-map nil "'ectags-select-mode' keymap.")
(if (not ectags-select-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(return)] 'ectags-select-goto-tag)
      (define-key map [(down)] 'ectags-select-next-tag)
      (define-key map [(up)] 'ectags-select-previous-tag)
      (define-key map [(q)] 'ectags-select-quit)
      (define-key map "0" (lambda () (interactive) (ectags-select-by-tag-number "0")))
      (define-key map "1" (lambda () (interactive) (ectags-select-by-tag-number "1")))
      (define-key map "2" (lambda () (interactive) (ectags-select-by-tag-number "2")))
      (define-key map "3" (lambda () (interactive) (ectags-select-by-tag-number "3")))
      (define-key map "4" (lambda () (interactive) (ectags-select-by-tag-number "4")))
      (define-key map "5" (lambda () (interactive) (ectags-select-by-tag-number "5")))
      (define-key map "6" (lambda () (interactive) (ectags-select-by-tag-number "6")))
      (define-key map "7" (lambda () (interactive) (ectags-select-by-tag-number "7")))
      (define-key map "8" (lambda () (interactive) (ectags-select-by-tag-number "8")))
      (define-key map "9" (lambda () (interactive) (ectags-select-by-tag-number "9")))
      (setq ectags-select-mode-map map)))



 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode startup
(defun ectags-select-mode ()
  "ectags-select-mode is a mode for browsing through exuberant ctags.\n\n
\\{ectags-select-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'ectags-select-mode)
  (setq mode-name "Ectags Select")
  (set-syntax-table text-mode-syntax-table)  
  (use-local-map ectags-select-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq ectags-select-mode-font-lock-keywords
        (list 
         (list "^<\\([0-9]+\\)>:\\[\\([^ ]+\\) in \\([^@]+\\)@\\([0-9]+)\\)\\]" 
               '(1 font-lock-warning-face) '(2 font-lock-function-name-face) '(3 font-lock-keyword-face) '(4 font-lock-warning-face))))
  (setq font-lock-defaults '(ectags-select-mode-font-lock-keywords))
  (setq overlay-arrow-position nil)
  (run-hooks 'ectags-select-mode-hook))


(provide 'ectags)
(provide 'ectags-select)
;;; ectags-select.el ends here
