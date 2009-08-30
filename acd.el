;;; acd.el --- A major mode to edit emboss acd files

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: 0.01
;; Keywords: languages, convenience

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
;;
;;; Comments:
;;   When in `acd-mode', C-h m to see useful information for the mode.
;; 
;; Put this file into your load-path and the following into your ~/.emacs:
;;    (add-to-list 'auto-mode-alist '("\\.acd$" . acd-mode))
;;    (autoload 'acd-mode "acd" "Major mode to edit acd files" t)
;;   

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'conf-mode)
(require 'tempo)

(defgroup acd nil
  "Major mode for editing acd files"
  :group 'languages)

(defcustom acd-indent-level 2
  "*Indentation of acd statements"
  :type 'integer
  :group 'acd)

(defcustom acd-auto-newline t
  "*Non-nil means automatically newline after brackets."
  :type 'boolean
  :group 'acd)

(defvar acd-imenu-generic-expression
  '(("Sections" "^\\s-*section\\s-*[:=]\\s-*\\(\\sw+\\)" 1)
    (nil "^\\s-*\\sw+\\s-*[:=]\\s-*\\(\\sw+\\)\\s-*\\[" 1))
  "Imenu generic expression for Acd mode.  See `imenu-generic-expression'.")

(defvar acd-attr-alist
  '(("align" "type" "taglist" "minseqs" "maxseqs" "multiple" "nulldefault" "nullok")
    ("application" "documentation" "groups" "keywords" "gui" "batch"
     "obsolete" "embassy" "external" "cpu" "supplier" "version" "nonemboss"
     "executable" "template" "comment")
    ("array" "minimum" "maximum" "increment" "precision" "warnrange" "size"
     "sum" "sumtest" "tolerance")
    ("boolean" )
    ("codon" "name" "nullok")
    ("cpdb" "nullok")
    ("datafile" "name" "extension" "directory" "nullok")
    ("" "default" "information" "prompt" "code" "help" "parameter"
     "standard" "additional" "missing" "valid" "expected" "needed"
     "knowntype" "relations" "outputmodifier" "style" "qualifier"
     "template" "comment")
    ("directory" "fullpath" "nulldefault" "nullok" "extension")
    ("dirlist" "fullpath" "nullok" "extension")
    ("discretestates" "length" "size" "characters" "nullok")
    ("distances" "size" "nullok" "missval")
    ("endsection" )
    ("features" "type" "nullok")
    ("featout" "name" "extension" "type" "multiple" "nulldefault" "nullok")
    ("filelist" "nullok" "binary")
    ("float" "minimum" "maximum" "increment" "precision" "warnrange")
    ("frequencies" "length" "size" "continuous" "genedata" "within" "nullok")
    ("graph" "nulldefault" "nullok")
    ("xygraph" "multiple" "nulldefault" "nullok")
    ("infile" "nullok" "binary")
    ("integer" "minimum" "maximum" "increment" "warnrange")
    ("list" "minimum" "maximum" "button" "casesensitive" "header"
     "delimiter" "codedelimiter" "values")
    ("matrix" "pname" "nname" "protein")
    ("matrixf" "pname" "nname" "protein")
    ("outcodon" "name" "extension" "nulldefault" "nullok")
    ("outcpdb" "nulldefault" "nullok")
    ("outdata" "type" "nulldefault" "nullok" "binary")
    ("outdir" "fullpath" "nulldefault" "nullok" "extension" "binary" "temporary")
    ("outdiscrete" "nulldefault" "nullok")
    ("outdistance" "nulldefault" "nullok")
    ("outfile" "name" "extension" "append" "nulldefault" "nullok" "binary")
    ("outfileall" "name" "extension" "nulldefault" "nullok" "binary")
    ("outfreq" "nulldefault" "nullok")
    ("outmatrix" "nulldefault" "nullok")
    ("outmatrixf" "nulldefault" "nullok")
    ("outproperties" "nulldefault" "nullok")
    ("outscop" "nulldefault" "nullok")
    ("outtree" "name" "extension" "nulldefault" "nullok")
    ("pattern" "minlength" "maxlength" "maxsize" "upper" "lower" "type")
    ("properties" "length" "size" "characters" "nullok")
    ("range" "minimum" "maximum" "size" "minsize")
    ("regexp" "minlength" "maxlength" "maxsize" "upper" "lower" "type")
    ("relation" "relations")
    ("report" "type" "taglist" "multiple" "precision" "nulldefault" "nullok")
    ("scop" "nullok")
    ("section" "information" "type" "comment" "border" "side" "folder")
    ("select" "minimum" "maximum" "button" "casesensitive" "header"
     "delimiter" "values")
    ("sequence" "type" "features" "entry" "nullok")
    ("seqall" "type" "features" "entry" "minseqs" "maxseqs" "nullok")
    ("seqout" "name" "extension" "features" "type" "nulldefault" "nullok")
    ("seqoutall" "name" "extension" "features" "type" "minseqs" "maxseqs"
     "nulldefault" "nullok")
    ("seqoutset" "name" "extension" "features" "type" "minseqs" "maxseqs"
     "nulldefault" "nullok" "aligned")
    ("seqset" "type" "features" "aligned" "minseqs" "maxseqs" "nulldefault" "nullok")
    ("seqsetall" "type" "features" "aligned" "minseqs" "maxseqs" "minsets"
     "maxsets" "nulldefault" "nullok")
    ("string" "minlength" "maxlength" "pattern" "upper" "lower" "word")
    ("toggle" )
    ("tree" "size" "nullok")
    ("variable" ))
  "Specific attributes for data types used in acd.")

(defvar acd-calc-attr-alist
  '(("discrete" "discretelength" "discretesize" "discretecount")
    ("distances" "distancecount" "distancesize" "replicates" "hasmissing")
    ("features" "fbegin" "fend" "flength" "fprotein" "fnucleic" "fname"
     "fsize")
    ("frequencies" "freqlength" "freqsize" "freqloci" "freqgenedata"
     "freqcontinuous" "freqwithin")
    ("properties" "propertylength" "propertysize")
    ("regexp" "length")
    ("sequence" "begin" "end" "length" "protein" "nucleic" "name" "usa")
    ("seqall" "begin" "end" "length" "protein" "nucleic" "name" "usa")
    ("seqset" "begin" "end" "length" "protein" "nucleic" "name" "usa"
     "totweight" "count")
    ("seqsetall" "begin" "end" "length" "protein" "nucleic" "name" "usa"
     "totweight" "count" "multicount")
    ("string" "length")
    ("tree" "treecount" "speciescount" "haslengths"))
  "Calculated attributes for data types used in acd.")

(defvar acd-groups
  '("Utils:Misc" "Utils:Database indexing" "Utils:Database creation"
    "Test" "Protein:Profiles" "Protein:Mutation"
    "Protein:Motifs" "Protein:Composition" "Protein:3D structure"
    "Protein:2D structure" "Phylogeny:Misc" "Phylogeny:Tree drawing"
    "Phylogeny:Molecular sequence" "Phylogeny:Gene frequencies"
    "Phylogeny:Distance matrix" "Phylogeny:Discrete characters"
    "Phylogeny:Continuous characters" "Phylogeny:Consensus"
    "Nucleic:Translation" "Nucleic:Transcription" "Nucleic:Restriction"
    "Nucleic:RNA folding" "Nucleic:Repeats" "Nucleic:Primers"
    "Nucleic:Profiles" "Nucleic:Mutation" "Nucleic:Motifs"
    "Nucleic:Gene finding" "Nucleic:CpG islands" "Nucleic:Composition"
    "Nucleic:Codon usage" "Nucleic:2D structure" "Menus" "Information"
    "HMM" "Feature tables" "Enzyme Kinetics" "Edit" "Display"
    "Alignment:Multiple" "Alignment:Local" "Alignment:Global"
    "Alignment:Dot plots" "Alignment:Differences" "Alignment:Consensus"
    "Acd")
  "Standard groups")

(defvar acd-font-lock-keywords
  `((,(concat (regexp-opt '("application" "section" "endsection") t)
              "[:=]\\s-*\\(\\sw*\\)\\s-*\\[")
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,(concat (regexp-opt (delete "" (mapcar 'car acd-attr-alist)) t)
              "[:=]\\s-*\\(\\sw*\\)\\s-*\\[")
     (1 font-lock-type-face)
     (2 font-lock-variable-name-face))
    ("^\\s-*\\(endsection\\)\\s-*[:=]\\s-*\\(\\sw*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("^\\s-*\\(\\sw*\\)\\s-*[:=]\\s-*"
     (1 font-lock-builtin-face)))
  "Font-lock keywords used in `acd-mode'")

(defvar acd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-n" 'tempo-forward-mark)
    (define-key map "\C-c\C-p" 'tempo-backward-mark)
    (define-key map "\C-c\C-i" 'tempo-complete-tag)
    (define-key map "[" 'acd-electric-bracket)
    (define-key map "]" 'acd-electric-bracket)
    (define-key map "\t" 'acd-completion-function)
    (define-key map "\C-c/" 'acd-endsection)
    map)
  "Keymap used in `acd-mode'")

(defvar acd-mode-syntax-table
  (make-syntax-table conf-unix-mode-syntax-table)
  "Syntax table in use in `acd-mode' buffers.")

(defvar acd-abbrevs
  '(("appl"
     ("application" (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
     ("documentation"
      "groups"))
    ("sec"
     ("section" "input" "additional" "output" "required" "advanced")
     ("information"
      ("type" "page"))))
  "Abbreviations used in `acd-mode'.
Each element should look like:
 (ABBREV (TOKEN COLLECTIONS) ATTRIBUTES ADD)

See also `acd-define-tempo-template'")

(defvar acd-tempo-tags nil)
(defvar acd-tempo-template nil
  "Template using in `acd-complete-token'")

(defun acd-tempo-more-attribute (token)
  (let ((collections
         (append (assoc token acd-attr-alist)
                 (cdr (assoc "" acd-attr-alist))))
        attr)
    (while (and (setq attr (completing-read "Attribute: " collections))
                (> (length attr) 0))
      (setq collections (delete attr collections))
      (mapc (lambda (elt)
              (tempo-insert elt nil))
            (list attr ": \"" 'p '(p "Value: ") "\"" 'n>)))))

(defun acd-define-tempo-template (abbr token attr &optional add)
  "Define an abbreviation using tempo.
ABBR is the abbreviation to invoke the template. TOKEN is a list
looks like (TOKEN COLLECTIONS). COLLECTIONS can be null which
will create a mark for point after expansion. Or it can be a
S-expression to eval to set the token name. Or it can be a list
of string which can be completion read from minibuffer.
ATTR are the default attributes to insert for the token when
expansion. It can be a string or a list contain default value
for the attribute.
If ADD is non-nil, mean more attributes will add.
A mark will be created between quotes. You can navigate between marks
using `tempo-forward-mark' and `tempo-complete-tag'.

See `acd-abbrevs' for example.
"
  (tempo-define-template
   (concat "acd-" abbr)
   (append
    (list (concat (car token) ": ")
          (cond ((null (cdr token)) 'p)
                ((stringp (cdr token)) (cdr token))
                ((listp (cdr token))
                 (if (listp (cadr token))
                     (cadr token)
                   (list 'completing-read (concat (capitalize (car token)) ": ")
                         `',(cdr token)))))
          " [" 'n>)
    (apply 'append
           (mapcar (lambda (name)
                     (let ((def ""))
                       (if (listp name)
                           (setq def (cadr name)
                                 name (car name)))
                       (list (concat name ": \"") 'p (concat def "\"") 'n>)))
                   attr))
    (and add `((acd-tempo-more-attribute ,(car token))))
    (list "]" '>))
   abbr
   (format "Insert acd '%s'" (car token))
   'acd-tempo-tags)
  (add-to-list 'acd-tempo-template
               (cons (car token) (intern (concat "tempo-template-acd-" abbr)))))

(defun acd-tempo-install-template ()
  "Install templates using `tempo.el'."
  (unless acd-tempo-tags
    (dolist (template acd-abbrevs)
      (apply 'acd-define-tempo-template template)))
  (tempo-use-tag-list 'acd-tempo-tags))

;;;###autoload 
(define-derived-mode acd-mode conf-mode "Acd"
  "Major mode to edit emboss acd files.

Features:
 1. As an major mode, it can highlight keywords and auto-indent
    code.
    You can change `acd-indent-level' to set indent level.
    `acd-auto-newline' affect whether insert new line after input
    '[' or ']'
 
 2. Abbreviations. There is so many libraries about templates,
    tempo, skeleton, snippet, msf-abbrev and so on. I choose tempo,
    since it comes with emacs, and has more feature than skeleton.
    Although, you can use tempo with `abbrev-mode', tempo can 
    use anthor way to expand the abbrev. You should define
    a new command in your .emacs:
    
      (defun tempo-space ()
        \"Expand tempo or insert space.\"
        (interactive \"*\")
        (or (tempo-expand-if-complete)
            (call-interactively 'self-insert-command)))
      (global-set-key \" \" 'tempo-space)

    Note here binding a global key to SPACE, which is a bad manner
    for a library to do that. So it is better to copy that to
    .emacs by yourself.
    You can try in the acd-mode, and input \"appl\" and type SPACE,
    it should expand to:
       application: filename [
         documentation: \"\"
         groups: \"\"
       ]
    See `acd-abbrevs' and `acd-define-tempo-template' to define
    more tempo abbrevs.

 3. Completion for tokens, attributes, groups and variables
    Acd has a lot of tokens and attributes to remember. `acd-mode'
    provide an useful command to complete all these things.
    For example, you can input \"int\" and type TAB, it will insert:

      integer:  [

    And ask for the attributes for the integer type. When you enter
    an empty string, it will stop and place the cursor before '['.
    In fact all abbrev describe above can be expand by TAB.
    
    You can complete attributes too. For example, input \"def\" and
    type TAB, it will expand to \"default\".
    
    Groups can be complete too. When you input:
       groups: \"N_\"
    The underline indicate the cursor, when you type TAB, it will
    complete to \"Nucleic:\".
    
    You can also complet variables. For example, there is an
    variable sequence seqa before point, and you are writing:
       default: \"$(s_\"
    Type TAB, I think you can get a \"seqa\".

    The calculated attribute can be complete too:
       default: \"$(seqa).l_\"
    Type TAB, it will expand to \"length\".

Key bindings:
\\{acd-mode-map}"
  (conf-mode-initialize "#" 'acd-font-lock-keywords)
  (set (make-local-variable 'beginning-of-defun-function) 'acd-begin-block)
  (set (make-local-variable 'end-of-defun-function) 'acd-end-block)
  (set (make-local-variable 'indent-line-function) 'acd-indent-line)
  (acd-tempo-install-template)
  (set (make-local-variable 'imenu-generic-expression)
       acd-imenu-generic-expression))

(defun acd-begin-block ()
  "Go to beginning of block."
  (interactive)
  (re-search-backward "^\\s-*\\sw+\\s-*[:=]\\s-*\\sw*\\s-*\\[" nil 'move))

(defun acd-end-block ()
  "Go to end of block."
  (interactive)
  (let ((first t)
        (pos (point)))
    (while (and (not (eobp))
                (progn
                  (if (and first
                           (progn
                             (end-of-line 1)
                             (acd-begin-block)))
                      nil
                    (or (bobp) (forward-char -1))
                    (acd-end-block))
                  (setq first nil)
                  (forward-list 1)
                  (skip-chars-forward " \t")
                  (if (looking-at "\\s<\\|\n")
                      (forward-line 1))
                  (<= (point) pos))))))

(defun acd-current-token ()
  (save-excursion
    (acd-begin-block)
    (if (looking-at "^\\s-*\\(\\sw+\\)\\s-*[:=]\\s-*\\(\\sw+\\)")
        (match-string 1))))

(defun acd-data-type (name)
  "Search the data type for the variable with name NAME.
Note that the name should declare before point."
  (save-excursion
    (if (re-search-backward (concat "^\\s-*\\(\\sw+\\)\\s-*[:=]\\s-*"
                                    (regexp-quote name)
                                    "\\s-*\\[") nil t)
        (match-string 1))))

(defun acd-variables ()
  "Return all variables that declared before point."
  (save-excursion
    (acd-begin-block)
    (let (vars)
      (while (re-search-backward "^\\s-*\\(\\sw+\\)\\s-*[:=]\\s-*\\(\\sw+\\)\\s-*\\[" nil t)
        (unless (member (match-string 1) '("section" "application"))
          (push (match-string 2) vars)))
      vars)))

(defun acd-indent-line ()
  (let* ((oldpos (point))
         (state (save-excursion (parse-partial-sexp
                                 (progn (beginning-of-defun) (point))
                                 oldpos)))
         (indent 0))
    (cond ((looking-at "^\\s-*]\\s-*$") ; end of block
           (beginning-of-defun)
           (setq indent (current-indentation)))
          ((looking-at "^\\s-*endsection\\s-*[:=]")
           (let ((stack 1))
             (while (and (not (bobp))
                         (> stack 0)
                         (re-search-backward "^\\s-*\\(end\\)?section\\s-*[:=]"))
               (if (match-string 1)
                   (setq stack (1+ stack))
                 (setq stack (1- stack))))
             (if (= stack 0)
                 (setq indent (current-indentation))
               (error "Unblance section declare!"))))
          ((looking-at "^\\s-*#")
           (forward-line -1)
           (setq indent (current-indentation)))
          ((or (nth 3 state) (nth 4 state)) ; string or comment
           (goto-char (nth 8 state))
           (setq indent (1+ (current-column))))
          ((> (car state) 0)            ; in unblance bracket
           (goto-char (nth 1 state))
           (setq indent (+ (current-indentation) acd-indent-level)))
          ((save-excursion
             (when (re-search-backward "^\\s-*section\\s-*[:=]" nil t)
               (setq indent (current-indentation))
               (if (re-search-forward "^\\s-*endsection\\s-*[:=]" oldpos t)
                   (while (re-search-forward "^\\s-*endsection\\s-*[:=]" oldpos t)
                     (setq indent (current-indentation)))
                 (setq indent (+ indent acd-indent-level)))))))
    (goto-char oldpos)
    ;; adjust previous line
    (save-excursion
      (forward-line -1)
      (cond ((looking-at "^\\s-*]\\s-*$")
             (let ((offset
                    (save-excursion
                      (beginning-of-defun)
                      (current-indentation))))
               (delete-horizontal-space)
               (indent-to offset)))
            ((looking-at "^\\s-*endsection\\s-*[:=]")
             (delete-horizontal-space)
             (indent-to indent))))
    (delete-horizontal-space)
    (indent-to indent)))

(defun acd-completion-function ()
  "Completing current token."
  (interactive)
  (require 'complete)
  (let ((end (point))
        (minibuffer-completion-predicate 'identity)
        (PC-not-minibuffer t)
        (beg (save-excursion
               (if (looking-back "\\sw")
                   (progn (forward-word -1) (point))
                 (point))))
        minibuffer-completion-table state
        handler)
    (cond
     ;; completion table for variables
     ((looking-back "\\$(\\sw*")
      (setq minibuffer-completion-table
            (acd-variables)))
     ;; completion table for calculated attributes of variable
     ((looking-back "\\$(\\(\\sw+\\))\\.\\sw*")
      (setq minibuffer-completion-table
            (cdr (assoc (acd-data-type (match-string 1))
                        acd-calc-attr-alist))))
     ;; completion for standard groups
     ((looking-back "^\\s-*groups:\\s-*\\(\"\\).*")
      (setq beg (match-end 1)
            minibuffer-completion-table acd-groups))
     (t
      (save-excursion
        (setq state (parse-partial-sexp (progn (acd-begin-block) (point)) end)))
      (unless (or (nth 3 state) (nth 4 state)) ; not in string or comment
        (if (> (car state) 0)                  ; in block
            ;; completion table for specific attributes
            (setq minibuffer-completion-table
                  (append (cdr (assoc "" acd-attr-alist))
                          (cdr (assoc (acd-current-token)
                                      acd-attr-alist))))
          ;; use acd-completion-token to fancy complete 
          (setq handler 'acd-complete-token)))))
    (if handler
        (funcall handler)
      (if (equal last-command 'PC-lisp-complete-symbol)
          (PC-do-completion nil beg PC-lisp-complete-end t)
        (if PC-lisp-complete-end
            (move-marker PC-lisp-complete-end end)
          (setq PC-lisp-complete-end (copy-marker end t)))
        (PC-do-completion nil beg end t)))))

(defun acd-complete-token ()
  (let* ((beg (save-excursion
                (if (looking-back "\\sw+")
                    (backward-word 1))
                (point)))
         (end (point))
         (name (buffer-substring beg end))
         (completions (all-completions name acd-attr-alist))
         (len (length completions))
         pos)
    (cond ((= len 0) nil)
          ((= len 1)
           (let* ((token (car completions))
                  (acd-template (cdr (assoc token acd-tempo-template))))
             (if acd-template
                 (setq acd-template (symbol-value acd-template))
               (setq acd-template
                     `(,token ": " p " [" n>
                              (acd-tempo-more-attribute ,token)
                              "]" > p)))
             (delete-region beg end)
             (tempo-insert-template 'acd-template nil)))
          ((> len 1)
           (with-output-to-temp-buffer "*Completions*"
             (display-completion-list completions name))))))

(defun acd-endsection ()
  "Close section."
  (interactive)
  (let ((oldpos (point))
        (stack 1))
    (while (and (not (bobp))
                (> stack 0)
                (re-search-backward "^\\s-*\\(end\\)?section\\s-*[:=]"))
      (if (match-string 1)
          (setq stack (1+ stack))
        (setq stack (1- stack))))
    (if (/= stack 0)
        (error "Unblance section declare!"))
    (when (looking-at "^\\s-*section\\s-*[:=]\\s-*\\(\\sw+\\)")
      (goto-char oldpos)
      (insert "endsection: " (match-string 1))
      (newline-and-indent))))

(defun acd-electric-bracket ()
  "Autoinsert newline after insertion."
  (interactive)
  (call-interactively 'self-insert-command)
  (if acd-auto-newline
      (newline-and-indent)))

(provide 'acd)
;;; acd.el ends here
