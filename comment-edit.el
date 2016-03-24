;;; comment-edit.el --- create/edit shell like comments

;; date:    2016-03-24
;; version: 0.1

;; Copyright (C) 2013-present  gabriele balducci

;; Author:     gabriele balducci <balducci@units.it>
;; Maintainer: gabriele balducci <balducci@units.it>
;; Keywords:   tools, comment, programming

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; INSTALLATION
;; ============
;; Put this file  into your load-path, optionally byte  compile it (with:
;; "emacs  -batch -f  batch-byte-compile comment-edit.el")  and plop  the
;; following into your ~/.emacs:
;; (require 'comment-edit)
;; 
;; USAGE AND DESCRIPTION
;; =====================
;; This code allows to create and/or edit shell-like (aka end-of-line)
;; comments.
;; 
;; Comments are created/edited in a dedicated buffer which is popped up
;; when the comment-edit command is invoked; the user edits the comment
;; as ordinary text in the edit buffer; when finished, C-cC-c pops down
;; the edit buffer and copies back the text commented according to the
;; current comment style into the original buffer.
;; 
;; Briefly:
;; 
;; M-x comment-edit           create/edit a comment using current style
;; C-uN M-x comment-edit      create/edit a comment using style N
;;                            and set style N as the current
;;                            style (N=1,4; comment styles shown below)
;; C-cC-c                     type C-cC-c in the edit buffer to end editing
;; C-u M-x comment-edit       ask in the minibuffer for the style to be
;;                            set as default for future calls
;; 
;; (I bind comment-edit to C-ce: (global-set-key "\C-ce"  'comment-edit) )
;; 
;; The code was inspired by (and partially stolen from) c-comment-edit2,
;; which does the same service for the block comments of the C language.
;; 
;; At present, there are 4 possible (fairly customizable) comment styles
;; (examples below are for the unix shell language):
;; 
;; Style n. 1: "simplest"
;; 
;;     # comment line 1
;;     # comment line 2
;;     # comment line 3
;; 
;; Style n. 2: "simple"
;; 
;;     #
;;     # comment line 1
;;     # comment line 2
;;     # comment line 3
;;     #
;; 
;; Style n. 3: "narrow-box"
;; 
;;     ##################
;;     # comment line 1 #
;;     # comment line 2 #
;;     # comment line 3 #
;;     ##################
;; 
;; Style n. 4: "wide-box"
;; 
;;     ########################################################################
;;     # comment line 1                                                       #
;;     # comment line 2                                                       #
;;     # comment line 3                                                       #
;;     ########################################################################
;; 
;; Creation of a new comment versus editing an existing comment is determined
;; by the cursor location at invocation time:
;; => if the cursor is already on a commented text region, then that whole
;;    commented region is copied into the edit buffer for editing
;; => if the cursor is not on a commented text region, then a new comment
;;    is assumed and a virgin comment edit buffer is popped up
;;    -> if the cursor was on a non-empty, non-comment line, the comment
;;       will be inserted before that line, indented as much as the initial
;;       cursor position
;;    -> if the cursor was on an empty line, the comment is inserted at
;;       that one line, indentation being determined again by the initial
;;       cursor position
;; 
;; In every case, comment editing can be aborted at any time with
;; C-uC-cC-c in the edit buffer: when this is the case, the original
;; buffer will be unmodified.
;; 
;; NOTE: comments following code on the same line are NOT supported; e.g.
;; 
;;     a=1  # comment-edit does not handle this comment
;; 
;; CUSTOMIZATION
;; Comments have the following general structure:
;; 
;;        <s1><b2><c><c>...<c><c><b3>   (*-box styles only)
;;        <s1><s2>comment line 1 <s3>   (<s3> present for *-box styles only)
;;        <s1><s2>comment line 2 <s3>
;;                .....
;;        <s1><s2>comment line n <s3>
;;        <s1><b2><c><c>...<c><c><b3>   (*-box styles only)
;; 
;; where the various components are stored into customizable variables:
;; 
;; comp.     variable                   explanation
;; -----------------------------------------------------------------------------
;; <s1> comment-edit-comment-string  this is the comment string as defined
;;                                   by the language (i.e. # for shell, %
;;                                   for latex, ; for lisp etc). NOTE: it
;;                                   is a string (not a char), since for
;;                                   some languages this can have >1 chars
;;                                   (e.g. "REM") or the user might want
;;                                   to define this to multiple comment
;;                                   chars, e.g. "###"
;; <s2> comment-edit-comment-leader  this string is inserted *AFTER*
;;                                   comment-edit-comment-string at the
;;                                   beginning of each line of the
;;                                   comment text
;; <s3> comment-edit-comment-trailer this string is used for *-box style
;;                                   comments only and is inserted at the
;;                                   end of each line of the comment text
;; <b2> comment-edit-line-leader     this string is used for *-box style
;;                                   comments only and is inserted
;;                                   *AFTER* comment-edit-comment-string
;;                                   at the beginning of the FIRST and
;;                                   LAST lines of the comment text (see
;;                                   above)
;; <b3> comment-edit-line-trailer    this string is used for *-box style
;;                                   comments only and is inserted at the
;;                                   end of the FIRST and LAST lines of
;;                                   the comment text (see above)
;; <c>  comment-edit-line-filler     this is a char (not a string) used
;;                                   for *-box style comments only; the
;;                                   FIRST and LAST lines of the boxed
;;                                   comment will be filled with this
;;                                   character
;; 
;; Example
;; 
;; If:
;; 
;; comment-edit-comment-string   "REM"
;; comment-edit-comment-leader   " "
;; comment-edit-comment-trailer  " |"
;; comment-edit-line-leader      " + "
;; comment-edit-line-filler      ?-
;; comment-edit-line-trailer     " +"
;; 
;; then:
;; 
;; REM I am a comment
;; REM in the simplest style
;; 
;; 
;; REM
;; REM I am a comment
;; REM in the simple style
;; REM
;; 
;; 
;; REM +-------------------- +
;; REM I am a comment in the |
;; REM narrow-box style      |
;; REM +-------------------- +
;; 
;; 
;; REM +------------------------------------------------------------------- +
;; REM I am a comment in the                                                |
;; REM wide-box style                                                       |
;; REM +------------------------------------------------------------------- +
;; 
;; The variables defining the different comment styles are set according
;; to the buffer mode and, optionally, to the extension of the name of
;; the file being edited or the name of the buffer (if the buffer is not
;; visiting any file). Basically, the buffer mode and optionally the file or
;; buffer name are matched against corresponding regexes: if the match
;; succeeds, then the variables are set accordingly. All the pieces
;; needed for this mechanism are stored into the user customizable
;; comment-edit-language-selector variable. This is a list of 2-elements
;; lists: the first element of each is a list of 1 or 2 elements, the
;; second element is a list of 6 elements:
;; 
;;         comment-edit-language-selector:
;; 
;;          '(
;;            ( (regex1 [regex2]) (s1 s2 s3 s4 c s5) )
;;           ...
;;            ( (regex1 [regex2]) (s1 s2 s3 s4 c s5) )
;;           )
;; 
;; (regex1 [regex2])   is a list of 1 or 2 elements
;;                     The first element is a regex (string) to be
;;                     matched against
;;                     (format-mode-line mode-line-format)
;;                     If the second element is
;;                     present, it is interpreted as a regex (string)
;;                     to be matched against either the extension of
;;                     the name of the file being visited or the
;;                     buffer name
;;                     If both elements are present, then both matches
;;                     must be successful
;; (s1 s2 s3 s4 c s5)  is a list of 6 elements: s1 s2 s3 s4 and s5 are
;;                     strings, while c is a char.
;;                     These are the values of the variables defining the
;;                     comment styles for the buffer matching the
;;                     conditions defined by the (regex1 [regex2])
;;                     list. More specifically:
;;                     s1  ->  comment-edit-comment-string   
;;                     s2  ->  comment-edit-comment-leader   
;;                     s3  ->  comment-edit-comment-trailer  
;;                     s4  ->  comment-edit-line-leader      
;;                     c   ->  comment-edit-line-filler      
;;                     s5  ->  comment-edit-line-trailer     
;; 
;; The tests defined by the elements of comment-edit-language-selector
;; are performed sequentially and the loop terminates on the first
;; success.
;; 
;; A default comment-edit-language-selector value is set in this code,
;; but the user is expected to define her own
;; comment-edit-language-selector variable, based on the languages
;; usually spoken and preferences for the comment appearance.
;; 
;; 
;; Other customizable variables are:
;; comment-edit-other-buffer      t => edit buffer is popped up but other
;;                                     buffers still remain in the frame
;;                                nil => edit buffer is popped up and
;;                                       occupies the whole frame
;; comment-edit-mode-map          mode map to be used in the edit buffer
;; comment-edit-edit-buffer-mode  the edit buffer will be set in this
;;                                mode
;; comment-edit-style             default comment style (this can be changed
;;                                with C-u M-x comment-edit or C-u[1-4]
;;                                M-x comment-edit, see above )
;; 
;; TODO
;; ====
;; => more comment styles?

;;; Code:


(provide 'comment-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   User settable variables start here                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defcustom comment-edit-language-selector
'(
(("(Shell-script")("#"" "" #"""?#""))
(("(Tcl")("#"" "" #"""?#""))
(("(Fundamental")("#"" "" #"""?#""))
(("(Text")("#"" "" #"""?#""))
(("(Octave")("#"" "" #"""?#""))
(("(Lisp")(";;" " " " ;;" "" ?; ""))
(("(Emacs-Lisp")(";;" " " " ;;" "" ?; ""))
(("(TeX")("%%" " " " %%" "" ?% ""))
(("(LaTeX")("%%" " " " %%" "" ?% ""))
(("(BibTeX")("%%" " " " %%" "" ?% ""))
(("(Texinfo")("@c" " " " |" "" ?= "|"))
(("(C\+\+")("//" " " " |" "" ?= "|"))
(("(Fortran")("C" " " " |" "" ?= "|"))
(("(F90")("!" " " " |" "" ?= "|"))
)
  "This is a list used to set the appropriate comment variables
based on mode-line and/or file extension match
The format is:
'\(
\( \(regex1 [regex2]\) \(s1 s2 s3 s4 c s5\) \)
...
\( \(regex1 [regex2]\) \(s1 s2 s3 s4 c s5\) \)
\)
where:
\(regex1 [regex2]\) is a list of 1 or 2 elements
                    The first element is a regex string to be matched
                    against \(format-mode-line mode-line-format\)
                    If the second element is present, it is
                    interpreted as a regex string to be matched
                    against the extension of the name of the file
                    being visited or the buffer name
                    If both elements are present, then both matches
                    have to succeed
\(s1 s2 s3 s4 c s5\) is a list of 6 elements: s1 s2 s3 s4 and s5 are
                    strings, while c is a char.
                    These are the values of the variables defining the
                    comment styles for the buffer matching the
                    conditions defined by the \(regex1 [regex2]\)
                    list. More specifically:
                    s1  ->  comment-edit-comment-string   
                    s2  ->  comment-edit-comment-leader   
                    s3  ->  comment-edit-comment-trailer  
                    s4  ->  comment-edit-line-leader      
                    c   ->  comment-edit-line-filler      
                    s5  ->  comment-edit-line-trailer     

"
  :group 'customize
  )

(defcustom comment-edit-other-buffer t
  "Set to nil if you want to edit in full buffer"
  :group 'customize
  )

(defcustom comment-edit-mode-map nil
  "Mode map to be used in the edit buffer"
  :group 'customize
  )

(defcustom comment-edit-edit-buffer-mode 'indented-text-mode
  "comment-edit-edit-buffer will be set in this mode
Note: this is a function to be called w/ funcall"
  :group 'customize
  )

(defcustom comment-edit-style "simple"
  "Comment style (a string). Currently, one of:
     - simplest
     - simple
     - narrow-box
     - wide-box

simplest:

# first line
# second line
# third line

simple:

#
# first line
# second line
# third line
#

narrow-box:

################################
# This is a narrow-box comment #
################################

wide-box:

########################################################################
# This is a wide-box comment                                           #
########################################################################

"
  :group 'customize
  )

(defcustom comment-edit-window-register ?w
  "Save window configuration in this register (Note: this is a char)"
  :group 'customize
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     User settable variables end here                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar comment-edit-comment-string "#"
  "
This is the comment string as defined by the language (i.e. # for
shell, % for latex etc). NOTE: it is a string (not a char). It is
useful to have it in a variable. It is used in several places 
and is set in comment-edit-select-mode for each mode appropriately

In general, each comment line has the form:

      <s1><s2>blah blah<s3>

where:
<s1> =  comment-edit-comment-string
<s2> =  comment-edit-comment-leader
<s3> =  comment-edit-comment-trailer (for *-box comment styles only)

For example, if:
comment-edit-comment-string=\"#\"
comment-edit-comment-leader=\" \"
comment-edit-comment-trailer=\" #\"

then a commented line will appear like this:
1. non *-box style comment:
# blah blah
2. *-box style comment:
# blah blah #
"
  )

(defvar comment-edit-comment-leader " "
  "
This string is inserted *AFTER* comment-edit-comment-string at the
beginning of each line upon C-cC-c in comment-edit-edit-buffer

In general, each comment line has the form:

      <s1><s2>blah blah<s3>

where:
<s1> =  comment-edit-comment-string
<s2> =  comment-edit-comment-leader
<s3> =  comment-edit-comment-trailer (*-box comment styles only)

For example, if:
comment-edit-comment-string=\"#\"
comment-edit-comment-leader=\" \"
comment-edit-comment-trailer=\"#\"

then a commented line will appear like this:
1. non *-box style comment:
# blah blah
2. *-box style comment:
# blah blah #
"
  )

(defvar comment-edit-comment-trailer " #"
  "
This string is inserted at the end of each line upon C-cC-c in
comment-edit-edit-buffer, when comment-edit-style == *-box

In general, each comment line has the form:

      <s1><s2>blah blah<s3>

where:
<s1> =  comment-edit-comment-string
<s2> =  comment-edit-comment-leader
<s3> =  comment-edit-comment-trailer (*-box comment styles only)

For example, if:
comment-edit-comment-string=\"#\"
comment-edit-comment-leader=\" \"
comment-edit-comment-trailer=\" #\"

then a commented line will appear like this:
1. non *-box style comment:
# blah blah
2. *-box style comment:
# blah blah #
"
  )

(defvar comment-edit-line-leader ""
  "
This string is inserted *AFTER* comment-edit-comment-string at
the beginning of the FIRST and LAST lines upon C-cC-c in
comment-edit-edit-buffer and when in *-box style

In general, the FIRST and LAST lines of *-box style comments have the form:

      <s1><s2><c><c>...<c><c><s3>

where:
<s1> =  comment-edit-comment-string
<s2> =  comment-edit-line-leader
<c>  =  comment-edit-line-filler
<s3> =  comment-edit-line-trailer

For example, if:
comment-edit-comment-string=\"REM\"
comment-edit-line-leader=\" + \"
comment-edit-line-filler=?-
comment-edit-line-trailer=\" +\"

then the FIRST and LAST lines of *-box style comment will appear like
this:
REM + -------------------------------- +
"
  )

(defvar comment-edit-line-filler ?#
  "This is a char (not a string) used to fill first/last line in
narrow-box and wide-box style comments

In general, the FIRST and LAST lines of *-box style comments have the form:

      <s1><s2><c><c>...<c><c><s3>

where:
<s1> =  comment-edit-comment-string
<s2> =  comment-edit-line-leader
<c>  =  comment-edit-line-filler
<s3> =  comment-edit-line-trailer

For example, if:
comment-edit-comment-string=\"REM\"
comment-edit-line-leader=\" + \"
comment-edit-line-filler=?-
comment-edit-line-trailer=\" +\"

then the FIRST and LAST lines of *-box style comment will appear like
this:
REM + -------------------------------- +
"
  )

(defvar comment-edit-line-trailer "#"
  "This string is inserted at the end of the first and last lines
upon C-cC-c in comment-edit-edit-buffer and when in *-box style

In general, the FIRST and LAST lines of *-box style comments have the form:

      <s1><s2><c><c>...<c><c><s3>

where:
<s1> =  comment-edit-comment-string
<s2> =  comment-edit-line-leader
<c>  =  comment-edit-line-filler
<s3> =  comment-edit-line-trailer

For example, if:
comment-edit-comment-string=\"REM\"
comment-edit-line-leader=\" + \"
comment-edit-line-filler=?-
comment-edit-line-trailer=\" +\"

then the FIRST and LAST lines of *-box style comment will appear like
this:
REM + -------------------------------- +
"
  )

(defvar comment-edit-main-buffer ""
  "The buffer containing the code (not the comment being
created/edited)"
  )

(defvar comment-edit-edit-buffer nil
  "The buffer where the comment is created/edited"
  )

(defvar comment-edit-style-completion-alist
  '(
    ("simplest" . 1)
    ("simple" . 1)
    ("narrow-box" . 1)
    ("wide-box" . 1)
    )
  )

(defvar comment-edit-style-history
  nil
  )

(defvar comment-edit-style-default "simple"
  "Used to display the default choice in C-u comment-edit"
  )

(defvar comment-edit-return-point nil
  "Where in comment-edit-main-buffer comment-edit was called from
(this is a marker)"
)

(defvar comment-edit-comment-start nil
  "Pre-existing comment starts here (this is a marker)")

(defvar comment-edit-comment-end nil
  "Pre-existing comment ends here (this is a marker)")

(defvar comment-edit-fill-column fill-column
  "For handling pre-existing indented comments"
  )

(defvar comment-edit-indention 0
  "For handling preexisting indented comments"
  )


;;
;; Stolen from c-comment-edit
;; Set the keymap to be used while editing the comment
;; (only if user has not defined his own)
;;
(if comment-edit-mode-map
    nil
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 2012-01-02                                                           ;;
  ;; This is unclear to me.                                               ;;
  ;;                                                                      ;;
  ;; I was defining the keymap to be used in comment-edit-edit-buffer     ;;
  ;; like this:                                                           ;;
  ;;                                                                      ;;
  ;;    (setq comment-edit-mode-map (copy-keymap (current-global-map)))   ;;
  ;;                                                                      ;;
  ;; The rationale was to have all my usual bindings working in           ;;
  ;; comment-edit-edit-buffer (since I pulled a *copy* of                 ;;
  ;; (current-global-map)), with just \C-c\C-c redefined to call          ;;
  ;; comment-edit-end-or-abort. But I must have misunderstood the         ;;
  ;; copy-keymap function: apparently, \C-c\C-c got redefined *globally*, ;;
  ;; with nasty consequences (since I usually have \C-c\C-c bound to      ;;
  ;; comment-region and use it quite often). As a workaround, I saved and ;;
  ;; restored the old \C-c\C-c binding like this:                         ;;
  ;;                                                                      ;;
  ;;     (setq comment-edit-old-ctl-c-binding                             ;;
  ;;           (lookup-key(current-global-map)"\C-c\C-c"))                ;;
  ;;                                                                      ;;
  ;;     ....                                                             ;;
  ;;                                                                      ;;
  ;;     (global-set-key "\C-c\C-c" comment-edit-old-ctl-c-binding)       ;;
  ;;                                                                      ;;
  ;;                                                                      ;;
  ;; After reviewing the code in c-comment-edit, I found that the keymap  ;;
  ;; for editing is defined with:                                         ;;
  ;;                                                                      ;;
  ;;    (setq c-com-mode-map (make-sparse-keymap))                        ;;
  ;;                                                                      ;;
  ;; and, more importantly, all my usual bindings work nicely in the      ;;
  ;; c-comment-edit buffer!                                               ;;
  ;;                                                                      ;;
  ;; So: I was using copy-keymap to be sure that the returned keymap had  ;;
  ;; all bindings inherited by (current-global-map), but, for some        ;;
  ;; obscure reason (to me), all works as if the returned keymap is       ;;
  ;; not a copy, but it is the (current-global-map) itself (since         ;;
  ;; the changed binding propagates everywhere!)                          ;;
  ;;                                                                      ;;
  ;; On the other hand, the keymap returned by (make-sparse-keymap)       ;;
  ;; apparently inherits all the bindings from the global-map, since I    ;;
  ;; can use them in the c-comment-edit buffer.                           ;;
  ;;                                                                      ;;
  ;; I do not understand this from the info docs!                         ;;
  ;;                                                                      ;;
  ;; However: use make-sparse-keymap and NOT copy-keymap                  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  (setq comment-edit-mode-map (make-sparse-keymap))
  )


;;
;; Portability issue: I discovered that on cineca's sp6 (emacs-21.3.1)
;; line-number-at-pos is undefined: this was directly stolen from
;; emacs-23.3.1's simple.el
;;
(eval-and-compile ;; NOTE: not eval-when-compile!
  (if (< emacs-major-version 23)
      (defun line-number-at-pos (&optional pos)
        "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
Counting starts at (point-min), so the value refers
to the contents of the accessible portion of the buffer."
        (let ((opoint (or pos (point))) start)
          (save-excursion
            (goto-char (point-min))
            (setq start (point))
            (goto-char opoint)
            (forward-line 0)
            (1+ (count-lines start (point))))))
    ) ;; if (< emacs-major-version 23)
  ) ;; eval-and-compile


;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun replace-in-string  (regexp string replacement)
"Just a shortcut for string-match/replace-match"
  (if (string-match regexp string)
      (replace-match replacement t nil string)
    ) ;; if
  ) ;; replace-in-string ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-select-mode  ()
  (let (
        (The-mode-line (format-mode-line mode-line-format))
        (Old-case-fold-search case-fold-search)
        regex-list
        mode-line-regex
        extension-regex
        definition-list
        the-condition
        the-extension
        )
    ;; By default, searches in Emacs ignore the case of the text they are
    ;; searching through    
    (setq case-fold-search nil)

    (catch 'exit  ;; implement a break jump, see below
      (dolist (elt comment-edit-language-selector)
        (setq regex-list (nth 0 elt))
        (setq definition-list (nth 1 elt))
        (setq mode-line-regex (nth 0 regex-list))
        ;; Build the condition to be eval'ed below:
        ;; => either single test on mode line
        ;; => or test on mode line _AND_ test on file extension                
        (setq the-condition '(string-match mode-line-regex The-mode-line))
        (when (= (safe-length regex-list) 2)
          (setq extension-regex "")
          (setq extension-regex (nth 1 regex-list))
          ;; the-extension is set, in this order:
          ;; => extension of the name of the file being visited, or
          ;; => extension of the name of the current buffer, or
          ;; => empty string          
          (setq the-extension
                (file-name-extension (or buffer-file-name (buffer-name) "")))
          (setq the-condition ;; double test
                '(and (string-match mode-line-regex The-mode-line)
                      (string-match extension-regex the-extension))
                ) ;; setq the-condition
          ) ;; when
        (when (eval the-condition) ;; test the-condition
          (setq  comment-edit-comment-string   (nth  0  definition-list))  
          (setq  comment-edit-comment-leader   (nth  1  definition-list))  
          (setq  comment-edit-comment-trailer  (nth  2  definition-list))  
          (setq  comment-edit-line-leader      (nth  3  definition-list))  
          (setq  comment-edit-line-filler      (nth  4  definition-list))  
          (setq  comment-edit-line-trailer     (nth  5  definition-list))  
          (throw 'exit t) ;; aka break
          ) ;; when
        ) ;; dolist
      ) ;; catch
    ) ;; let
  ) ;; comment-edit-select-mode ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-select-mode-OLD  ()
  (let (
        (The-mode-line (format-mode-line mode-line-format))
        (Old-case-fold-search case-fold-search)
        )

    ;;
    ;; Find the mode we are in and set variables accordingly
    ;; NOTE: By default, searches in Emacs ignore the case of the text
    ;;       they are searching through
    ;;    
    (setq case-fold-search nil)
    (cond

     ((or (string-match "(Shell-script" The-mode-line)
          (string-match "(000README" The-mode-line)
          (string-match "(Tcl" The-mode-line)
          (string-match "(Fundamental" The-mode-line)
          (string-match "(Text" The-mode-line)
          (string-match "(Octave" The-mode-line)
       )
      (setq comment-edit-comment-string "#")
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " #")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?#)
      (setq comment-edit-line-trailer "")
      ) ;; "Shell-script/..."

     ((or (string-match "(Lisp" The-mode-line)
          (string-match "(Emacs-Lisp" The-mode-line)
          )
      (setq comment-edit-comment-string ";;" )
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " ;;")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?;)
      (setq comment-edit-line-trailer "")
      ) ;; "Lisp"

     ((or (string-match "(LaTeX" The-mode-line)
          (string-match "(TeX" The-mode-line)
          (string-match "(BibTeX" The-mode-line)
          )
      (setq comment-edit-comment-string "%")
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " %%")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?%)
      (setq comment-edit-line-trailer "")
      ) ;; "Latex/TeX/BibTeX"

     ((or (string-match "(SHELX" The-mode-line)
          )
      (let((the-extension (file-name-extension buffer-file-name)))
        (cond
         ((equal the-extension "ins")
          (setq comment-edit-comment-string "REM")
          (setq comment-edit-comment-leader " ")
          (setq comment-edit-comment-trailer " REM")
          (setq comment-edit-line-leader " ")
          (setq comment-edit-line-filler ?-)
          (setq comment-edit-line-trailer " REM")
          ) ;; "ins"
         ((equal the-extension "res")
          (setq comment-edit-comment-string "REM")
          (setq comment-edit-comment-leader " ")
          (setq comment-edit-comment-trailer " REM")
          (setq comment-edit-line-leader " ")
          (setq comment-edit-line-filler ?-)
          (setq comment-edit-line-trailer " REM")
          ) ;; "res"
         ((equal the-extension "sir")
          (setq comment-edit-comment-string "!")
          (setq comment-edit-comment-leader " ")
          (setq comment-edit-comment-trailer " !")
          (setq comment-edit-line-leader "")
          (setq comment-edit-line-filler ?-)
          (setq comment-edit-line-trailer " ")
          ) ;; "sir"
         ) ;; cond
        ) ;; let
      ) ;; "SHELX"

     ((or (string-match "(Texinfo" The-mode-line)
          )
      (setq comment-edit-comment-string "@c")
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " |")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?=)
      (setq comment-edit-line-trailer "|")
      ) ;; "Texinfo"

     ((or (string-match "(C\+\+" The-mode-line)
          )
      (setq comment-edit-comment-string "//")
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " |")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?=)
      (setq comment-edit-line-trailer "|")
      ) ;; "Texinfo"

     ((or (string-match "(Fortran" The-mode-line)
          )
      (setq comment-edit-comment-string "C")
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " |")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?=)
      (setq comment-edit-line-trailer "|")
      ) ;; "Fortran"

     ((or (string-match "(F90" The-mode-line)
          )
      (setq comment-edit-comment-string "!")
      (setq comment-edit-comment-leader " ")
      (setq comment-edit-comment-trailer " |")
      (setq comment-edit-line-leader "")
      (setq comment-edit-line-filler ?=)
      (setq comment-edit-line-trailer "|")
      ) ;; "F90"

     ) ;; cond

     ;;
     ;; restore original value of case-fold-search
     ;;
     (setq case-fold-search Old-case-fold-search)

     ;;
     ;; allow user to call her own version
     ;;     
     (if (fboundp 'comment-edit-my-select-mode)
         (funcall 'comment-edit-my-select-mode))

    ) ;; let
  ) ;; comment-edit-select-mode ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-save-state  ()
  "Save window configuration."
  ;;
  ;; I got this from c-comment-edit, but apparently
  ;; point-to-register-compatibility-binding is old and does not exist
  ;; any longer in recent versions of emacs: this code is never
  ;; executed
  ;;  
  (if (fboundp 'point-to-register-compatibility-binding)
      (funcall
       (symbol-function 'point-to-register-compatibility-binding)
       comment-edit-window-register
       'window-config
       ))
  ;;
  ;; This works
  ;;  
  (setq comment-edit-main-buffer (current-buffer))
  (window-configuration-to-register comment-edit-window-register)
  ;;
  ;; redefine "\C-c\C-c" binding
  ;;
  (define-key comment-edit-mode-map "\C-c\C-c" 'comment-edit-end-or-abort)
  ) ;; comment-edit-save-state ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-restore-state  ()
  "Restore window configuration."
  ;;
  ;; I got this from c-comment-edit, but apparently
  ;; jump-to-register-compatibility-binding is old and does not exist
  ;; any longer in recent versions of emacs: this code is never
  ;; executed
  ;;  
  (if (and comment-edit-window-register
           (get-register comment-edit-window-register)
           (fboundp 'jump-to-register-compatibility-binding)
           )
      (funcall
       (symbol-function 'jump-to-register-compatibility-binding)
       comment-edit-window-register
       ))
  ;;
  ;; This works
  ;;  
  (jump-to-register comment-edit-window-register)
  ) ;; comment-edit-restore-state ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-hash-on-current-line ()
  "See whether there is a comment-edit-comment-string (but NOT an
escaped \comment-edit-comment-string) on the current line"
  (let (Point regex limit Return)
    (setq Point (point))
    (beginning-of-line)
    (setq limit (line-end-position))
    ;;
    ;; comment-edit-comment-string preceeded by (a NON-backslash) or (at BOL)
    ;; note: [^\\\\] doubly escaped: for regex and for lisp
    ;; note: \\( \\| etc. DITTO
    ;;    
    (setq regex (concat "\\(\\([^\\\\]\\)\\|^\\)"
                        comment-edit-comment-string))
    (setq Return (re-search-forward regex limit t))
    (goto-char Point)
    Return
   )
  );; comment-edit-hash-on-current-line ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-start-end-commented-line ()
  "Find the start/end of a commented region containing the
current (commented) line: return (list start end)
Return nil if we are not on a comment line
"
  (let (Point)
    (setq comment-edit-comment-start nil)
    (setq comment-edit-comment-end nil)
    (setq Point (point))
    ;;
    ;; go upwards until a non commented line or beginning of buffer
    ;;
    (setq  comment-edit-comment-start
           (catch 'upwards-loop-exit
             (if(not(comment-edit-pure-comment-line))
                 (throw 'upwards-loop-exit nil))
             (while (= (forward-line -1)0) ;; not beg-of-buffer
               (if(not (comment-edit-pure-comment-line))
                   (progn
                     (forward-line 1)(beginning-of-line)
                     (re-search-forward
                      (concat "^[[:space:]]*"
                              comment-edit-comment-string
                              ) (line-end-position) t)
                     (backward-char (length comment-edit-comment-string))
                     (throw 'upwards-loop-exit (point-marker))
                     ) ;; progn
                 ) ;; if
               ) ;; while
             ;;
             ;; come here if beginning of buffer was reached
             ;;             
             (beginning-of-line)
             (re-search-forward 
              (concat "^[[:space:]]*"
                      comment-edit-comment-string)
              (line-end-position) t)
             (backward-char (length comment-edit-comment-string))
             (point-marker)
             ) ;; catch
           ) ;; setq
    ;;
    ;; update the following variables:
    ;; => comment-edit-indention
    ;; => comment-edit-fill-column
    ;; => comment-edit-return-point
    ;;    
    (if comment-edit-comment-start
        (progn
          (setq comment-edit-indention (current-column))
          (setq comment-edit-fill-column
                (- fill-column (current-column)))
          ;;
          ;; Note: we set comment-edit-return-point at beginning of
          ;; line: any indention will be already set up in
          ;; comment-edit-edit-buffer
          ;;         
          (beginning-of-line)
          (setq comment-edit-return-point (point-marker))
          )
      )
    ;;
    ;; go downwards until a non commented line or end of buffer
    ;;
    (setq  comment-edit-comment-end
           (catch 'downwards-loop-exit
             (if(not(comment-edit-pure-comment-line))
                 (throw 'downwards-loop-exit nil)) ;; not on a comment
             (while (= (forward-line 1)0) ;; not end-of-buffer
               (if(not (comment-edit-pure-comment-line))
                   (progn
                     (forward-line -1)(end-of-line)
                     (throw 'downwards-loop-exit (point-marker))
                     ) ;; progn
                 ) ;; if
               ) ;; while
             ;;
             ;; come here if end of buffer was reached
             ;;             
             (beginning-of-line)
             (re-search-forward 
              (concat "^[[:space:]]*"
                      comment-edit-comment-string)
              (line-end-position) t)
             (point-marker)
             ) ;; catch
           ) ;; setq 
    (goto-char Point)
    ) ;; let
  );; comment-edit-start-end-commented-line ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-pure-comment-line ()
  ;;
  ;; See whether the first non-blank char on the current line is
  ;; comment-edit-comment-string
  ;; E.g., for comment-edit-comment-string==#:
  ;; 
  ;;         # this is a pure comment:                         ==> return t
  ;;     a='some value' # shell comment after a shell command: ==> return nil
  ;; 
  ;; IMPORTANT: empty lines or lines containing only white space are
  ;; considered NON pure comment lines; and this has to be like that!
  ;; In other words, in order to be a pure comment line, the presence of
  ;; comment-edit-comment-string as the first non blank char is MANDATORY.
  ;;  
  (let (Point regex limit Return)
    (setq Point (point))
    (beginning-of-line)
    (setq limit (line-end-position))
    ;;
    ;; NOTE: A `[:' and balancing `:]' enclose a character class
    ;; inside a character alternative.
    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ;; so the syntax is [[:space:]], NOT simply: [:space:]
    ;;    
    (setq regex
          (concat "^[[:space:]]*" comment-edit-comment-string)
          )
    (setq Return (re-search-forward regex limit t))
    (goto-char Point)
    Return
   ) ;; let
  );; comment-edit-hash-on-current-line ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-first-last-simple-line-p ()
  "Check whether current line is the first/last line of a simple
style comment "
  (beginning-of-line)
  (re-search-forward
   (concat "^ *" comment-edit-comment-string
           (replace-in-string " *$" comment-edit-comment-leader "")
           " *$")
   (line-end-position) t)
  ) ;; comment-edit-simple-line-p ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-first-last-box-line-p ()
  "check whether current line is the first/last line of a *-box style
comment "
  (beginning-of-line)
  (re-search-forward
   (concat "^ *" comment-edit-comment-string
           comment-edit-line-leader
           (make-string 1 comment-edit-line-filler) "*" 
           (replace-in-string " *$" comment-edit-line-trailer "")
           " *$")
   (line-end-position) t)
  ) ;; comment-edit-box-line-p ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-popup-edit-buffer ()
  "pops up the buffer where the comment is edited
"
  ;;
  ;; stolen and adapted from c-comment-edit
  ;;  
  (let()

    ;;
    ;; get new buffer
    ;;    
    (setq comment-edit-edit-buffer
          (generate-new-buffer "comment-edit-edit-buffer"))

    ;;
    ;; copy commented region, if we are into one
    ;;    
    (if(and comment-edit-comment-start comment-edit-comment-end)
        (copy-to-buffer comment-edit-edit-buffer
                        comment-edit-comment-start
                        comment-edit-comment-end)
        ) ;; if

    ;;
    ;; switch to edit buffer
    ;;    
    (if (null comment-edit-other-buffer)
        (switch-to-buffer comment-edit-edit-buffer)
      (switch-to-buffer-other-window comment-edit-edit-buffer)
      )
    (funcall comment-edit-edit-buffer-mode)
    (use-local-map comment-edit-mode-map)
    (setq fill-column comment-edit-fill-column)

    ;;
    ;; get rid of any comment starters and lines containing only
    ;; comment-edit-comment-string (this depends upon comment-edit-style
    ;; in charge)
    ;;    
    (cond

     ;; -----------------------------
     ;; simplest
     ;; -----------------------------
     ((string= comment-edit-style "simplest")
      ;; Tolerate simple or *-box style: i.e. allow editing a simple or
      ;; *-box style comment into a simplest style comment      
      (when (or
             (comment-edit-first-last-simple-line-p)
             (progn
               (goto-char (point-min)) ;; must reset cursor position
               (comment-edit-first-last-box-line-p)
               ) ;; progn
             ) ;; or
        (goto-char (point-min))
        (forward-line)(kill-region(point-min)(point))
        ) ;; when
      ;; handle last line
      (goto-char (point-max))
      (if(bolp)(forward-line -1))
      (beginning-of-line)
      ;; ditto for the last line
      (when (or
             (comment-edit-first-last-simple-line-p)
             (progn
               (beginning-of-line) ;; must reset cursor position
               (comment-edit-first-last-box-line-p)
               ) ;; progn
             ) ;; or
        (goto-char (point-max))(forward-line -1)
        (end-of-line)(kill-region(point)(point-max))
        ) ;; when
      ;; handle bulk comment text
      (goto-char (point-min))
      (perform-replace
       (concat "^ *" comment-edit-comment-string comment-edit-comment-leader)
       "" nil t nil)
      ;; Handle the case when the comment was of type *-box and we edit
      ;; it into a simplest style comment: delete trailers coming from
      ;; *-box style comments: is this a good idea?      
      (goto-char(point-min))
      (perform-replace
       (concat " *"
               (replace-in-string " *$" comment-edit-comment-trailer "") " *$")
       "" nil t nil)
      ;; all done
      (goto-char(point-min))

      );;(string= comment-edit-style "simplest")

     ;; -----------------------------
     ;; simple
     ;; -----------------------------
     ((string= comment-edit-style "simple")
      ;; handle first line
      (goto-char (point-min))
      ;; tolerate *-box style: i.e. allow editing a *-box style comment
      ;; into a simple style comment      
      (when (or
             (comment-edit-first-last-simple-line-p)
             (progn
               (goto-char (point-min)) ;; must reset cursor position
               (comment-edit-first-last-box-line-p)
               ) ;; progn
             ) ;; or
        (goto-char (point-min))
        (forward-line)(kill-region(point-min)(point))
        ) ;; when
      ;; handle last line
      (goto-char (point-max))
      (if(bolp)(forward-line -1))
      (beginning-of-line)
      ;; ditto for the last line
      (when (or
             (comment-edit-first-last-simple-line-p)
             (progn
               (comment-edit-first-last-box-line-p)
               ) ;; progn
             ) ;; or
        (goto-char (point-max))(forward-line -1)
        (end-of-line)(kill-region(point)(point-max))
        ) ;; when
      ;; handle bulk comment text
      (goto-char(point-min))
      (perform-replace
       (concat "^ *" comment-edit-comment-string comment-edit-comment-leader) 
       "" nil t nil)
      ;; Handle the case when the comment was of type *-box and we edit
      ;; it into a simple style comment: delete trailers coming from
      ;; *-box style comments: is this a good idea?      
      (goto-char(point-min))
      (perform-replace
       (concat " *"
               (replace-in-string " *$" comment-edit-comment-trailer "") " *$")
       "" nil t nil)
      ;; all done
      (goto-char(point-min))
      );;(string= comment-edit-style "simple")

     ;; -----------------------------
     ;; *-box
     ;; -----------------------------
     ((or (string= comment-edit-style "narrow-box")
          (string= comment-edit-style "wide-box"))
      ;;
      ;; handle first line
      ;;      
      (goto-char (point-min))
      ;;
      ;; tolerate non-box style
      ;;      
      (when (or
             (comment-edit-first-last-simple-line-p)
             (progn
               (comment-edit-first-last-box-line-p)
               ) ;; progn
             ) ;; or
        (goto-char (point-min))
        (forward-line)(kill-region(point-min)(point))
        ) ;; when
      ;;
      ;; handle last line
      ;;      
      (goto-char (point-max))
      (if(bolp)(forward-line -1))
      (beginning-of-line)
      ;;
      ;; ditto for the last line
      ;;      
      (when (or
             (comment-edit-first-last-simple-line-p)
             (progn
               (comment-edit-first-last-box-line-p)
               ) ;; progn
             ) ;; or
        (goto-char (point-max))(forward-line -1)
        (end-of-line)(kill-region(point)(point-max))
        ) ;; when
      ;;
      ;; handle bulk comment text delete comment-edit-comment-string chars
      ;; at bol
      ;;      
      (goto-char(point-min))
      (perform-replace
       (concat "^ *" comment-edit-comment-string comment-edit-comment-leader)
       "" nil t nil)
      ;;
      ;; delete comment-edit-comment-trailer chars at eol
      ;;      
      (goto-char(point-min))
      (perform-replace
       (concat "[ 	]*"
               (replace-in-string " *$" comment-edit-comment-trailer "")
               "[ 	]*$")
       "" nil t nil)
      (goto-char(point-min))
      );;(string= comment-edit-style "box")

     );; cond

    );; let
  );; comment-edit-popup-edit-buffer ends here


;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-end-or-abort (&optional abort-flag)
  (interactive "P")
  (if abort-flag
      (comment-edit-abort)
    (comment-edit-end)
    )
  ) ;; comment-edit-end-or-abort ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-these-many-filler-chars (Max-length)
"
Compute how many comment-edit-line-filler chars are to be inserted in
the first/last line of *-box style comments
"
(-
 (+ (length comment-edit-comment-leader)
    Max-length
    (length comment-edit-comment-trailer))
 (+ (length comment-edit-line-leader)(length comment-edit-line-trailer))
 ) ;; -
) ;; comment-edit-these-many-filler-chars ends here

;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
(defun comment-edit-end ()
  "This is called by \C-c\C-c in the edit buffer:
It has to perform the following tasks:
=> insert comment chars at bol (eol for box style)
=> copy the contents of the edit buffer back into the calling buffer
=> kill the edit buffer (and restore the emacs window)
=> maybe other things too
"
  (interactive)
  (let(total-lines)

    ;;
    ;; insert comments at every bol
    ;; according to comment-edit-style
    ;;    
    (cond

     ;; -----------------------------
     ;; simplest
     ;; -----------------------------
     ((string= comment-edit-style "simplest")
      (goto-char (point-min))
      (while(not(eobp))
        (beginning-of-line)
        (insert comment-edit-comment-string comment-edit-comment-leader)
        (forward-line)
        );; while(not(eobp))
      ;;
      ;; if last char is a newline, strip it off
      ;;      
      (goto-char(point-max))
      (if(bolp)(delete-region(1-(point-max))(point-max)))
      );;(string= comment-edit-style "simplest")

     ;; -----------------------------
     ;; simple
     ;; -----------------------------
     ((string= comment-edit-style "simple")
      (goto-char (point-min))
      (insert comment-edit-comment-string)
      (insert (replace-in-string " *$" comment-edit-comment-leader ""))
      (insert "\n")
      (while(not(eobp))
        (beginning-of-line)
        (insert comment-edit-comment-string comment-edit-comment-leader)
        (forward-line)
        );; while(not(eobp))
      (if(not(bolp))(progn(insert "\n")(forward-line)))
      ;; last line
      (insert comment-edit-comment-string)
      (insert (replace-in-string " *$" comment-edit-comment-leader ""))
      );;(string= comment-edit-style "simple")

     ;; -----------------------------
     ;; narrow-box
     ;; -----------------------------
     ((string= comment-edit-style "narrow-box")
      (let((Max-length 0) Current-length)
        ;;
        ;; determine the max line length
        ;;        
        (goto-char (point-min))
        (while(not(eobp))
          (setq Current-length(-(line-end-position)(line-beginning-position)))
          (if(> Current-length Max-length)
              (setq Max-length Current-length))
          (forward-line)
          )
        ;;
        ;; build the comment, then
        ;;        
        (goto-char (point-min))
        ;;
        ;; First line, of the form, e.g.:
        ;; 
        ;; #+---------------------------|
        ;; 
        ;; with:
        ;;     #   comment-edit-comment-string
        ;;     +   comment-edit-line-leader
        ;;     -   comment-edit-line-filler
        ;;     |   comment-edit-line-trailer
        ;;        
        (insert comment-edit-comment-string)
        (insert comment-edit-line-leader)
        ;; take comment-edit-comment-leader and
        ;; comment-edit-line-leader into account
        (insert-char comment-edit-line-filler
                     (comment-edit-these-many-filler-chars Max-length))
        (insert comment-edit-line-trailer)
        (insert "\n")
        ;;
        ;; now the body: e.g. # blah blah #
        ;;        
        (while(not(eobp))
          (beginning-of-line)
          (move-to-column Max-length t) ;; first the trailing...
          (insert comment-edit-comment-trailer)
          (beginning-of-line)
          (insert comment-edit-comment-string
           comment-edit-comment-leader) ;; ...and then the starting comment
          (forward-line)
          );; while(not(eobp))
        (if(not(bolp))(progn(insert "\n")(forward-line)))
        ;;
        ;; Last line, of the form, e.g.:
        ;; 
        ;; #+---------------------------|
        ;; 
        ;; with:
        ;;     #   comment-edit-comment-string
        ;;     +   comment-edit-line-leader
        ;;     -   comment-edit-line-filler
        ;;     |   comment-edit-line-trailer
        ;;        
        (insert comment-edit-comment-string)
        (insert comment-edit-line-leader)
        ;; take comment-edit-comment-leader and
        ;; comment-edit-line-leader into account
        (insert-char comment-edit-line-filler
                     (comment-edit-these-many-filler-chars Max-length))
        (insert comment-edit-line-trailer)
        ) ;; let
      );;(string= comment-edit-style "narrow-box")

     ;; -----------------------------
     ;; wide-box
     ;; -----------------------------
     ((string= comment-edit-style "wide-box")
      (let((Max-length 0)Current-length)
        ;;
        ;; determine the max line length
        ;;        
        (goto-char (point-min))
        (while(not(eobp))
          (setq Current-length(-(line-end-position)(line-beginning-position)))
          (if(> Current-length Max-length)
              (setq Max-length Current-length))
          (forward-line)
          )
        ;;
        ;; We now set Max-length to
        ;; max((- fill-column length(comment-edit-comment-trailer)),Max-length)
        ;; (so that we allow for insertion of comment-edit-comment-trailer)
        ;;        
        (when(> fill-column (+ Max-length (length comment-edit-comment-trailer)))
          (setq Max-length
                (- fill-column (length comment-edit-comment-trailer))))
        ;;
        ;; build the comment, then
        ;;        
        (goto-char (point-min))
        ;;
        ;; First line, of the form, e.g.:
        ;; 
        ;; #+---------------------------|
        ;; 
        ;; with:
        ;;     #   comment-edit-comment-string
        ;;     +   comment-edit-line-leader
        ;;     -   comment-edit-line-filler
        ;;     |   comment-edit-line-trailer
        ;;        
        (insert comment-edit-comment-string)
        (insert comment-edit-line-leader)
        ;; take comment-edit-comment-leader and
        ;; comment-edit-line-leader into account
        (insert-char comment-edit-line-filler
                     (comment-edit-these-many-filler-chars Max-length))
        (insert comment-edit-line-trailer)
        (insert "\n")
        ;;
        ;; now the body: # blah blah     #
        ;;        
        (while(not(eobp))
          (beginning-of-line)
          (move-to-column Max-length t)
          (insert comment-edit-comment-trailer) ;; first the trailing...
          (beginning-of-line)
          (insert comment-edit-comment-string
           comment-edit-comment-leader) ;; ...and then the starting comment
          (forward-line)
          );; while(not(eobp))
        (if(not(bolp))(progn(insert "\n")(forward-line)))
        ;;
        ;; Last line, of the form, e.g.:
        ;; 
        ;; #+---------------------------|
        ;; 
        ;; with:
        ;;     #   comment-edit-comment-string
        ;;     +   comment-edit-line-leader
        ;;     -   comment-edit-line-filler
        ;;     |   comment-edit-line-trailer
        ;;        
        (insert comment-edit-comment-string)
        (insert comment-edit-line-leader)
        ;; take comment-edit-comment-leader and
        ;; comment-edit-line-leader into account
        (insert-char comment-edit-line-filler
                     (comment-edit-these-many-filler-chars Max-length))
        (insert comment-edit-line-trailer)
        ) ;; let
      );;(string= comment-edit-style "wide-box")

     );; cond

    ;;
    ;; indent lines, if needed
    ;;    
    (if (> comment-edit-indention 0)
        (progn
          (goto-char (point-min))
          (while (not (eobp))
            (indent-to comment-edit-indention)
            (forward-line))
          ) ;; progn
      ) ;; if (> comment-edit-indention 0)

    ;;
    ;; get number of lines in comment-edit-edit-buffer: we need this to
    ;; go to the final position below
    ;;    
    (setq total-lines(count-lines(point-min)(point-max)))

    ;;
    ;; insert contents of comment-edit-edit-buffer into
    ;; comment-edit-main-buffer
    ;;    
    (save-excursion
      (switch-to-buffer comment-edit-main-buffer)
      ;;
      ;; delete previous commented region, if any
      ;;      
      (if(and comment-edit-comment-start comment-edit-comment-end)
          (kill-region comment-edit-comment-start comment-edit-comment-end)
        ) ;; if
      (goto-char comment-edit-return-point)
      ;;
      ;; if comment-edit-return-point is on the last line, open a new
      ;; line, or we will not able to move one line after the end of the
      ;; just inserted comment
      ;;      
      (if(=(line-number-at-pos)(line-number-at-pos(point-max)))
          (progn(open-line 1))
        )
      (insert-buffer-substring comment-edit-edit-buffer)
      );; save-excursion

    ;;
    ;; now we can kill comment-edit-edit-buffer
    ;;    
    (kill-buffer(current-buffer))
    (comment-edit-restore-state)
    ;;
    ;; switch to the sh buffer
    ;;    
    (if (get-buffer-window comment-edit-main-buffer)
        (select-window (get-buffer-window comment-edit-main-buffer))
      (switch-to-buffer comment-edit-main-buffer))

    ;;
    ;; move to the end of the just inserted comment
    ;; (message (format "%d" total-lines))
    ;;    
    (goto-char comment-edit-return-point)
    (forward-line total-lines)
    (beginning-of-line)

    ) ;; let


  );; comment-edit-end ends here


;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
;;;###autoload
(defun comment-edit-abort ()
  "This is called by \C-u\C-c\C-c in the edit buffer:
user wants to abort the edit
"
  (interactive)
  (let()

    ;;
    ;; kill comment-edit-edit-buffer
    ;;    
    (kill-buffer(current-buffer))
    (comment-edit-restore-state)
    ;;
    ;; switch to the sh buffer
    ;;    
    (if (get-buffer-window comment-edit-main-buffer)
        (select-window (get-buffer-window comment-edit-main-buffer))
      (switch-to-buffer comment-edit-main-buffer)
      (goto-char comment-edit-return-point) ;; take me home
      )
    ) ;; let
  ) ;; comment-edit-abort ends here


;;;;;;;;;;;;;;;;
;;; FUNCTION ;;;
;;;;;;;;;;;;;;;;
;;;###autoload
(defun comment-edit (&optional ask-or-set-style)

  ;;
  ;; This is the main command:
  ;; => when called without raw prefix:
  ;;    edit a comment:
  ;;    -> a brand new comment if the cursor is not on a comment
  ;;    -> an existing comment if the cursor is on it
  ;; => when called with raw prefix C-u (non numeric, though):
  ;;    just ask explicitly for the comment style, which can be one
  ;;    of:
  ;;    -> simplest
  ;;    -> simple
  ;;    -> narrow-box
  ;;    -> wide-box
  ;; => when called with a numeric prefix (e.g. C-u1): set comment style
  ;;    without asking, according to the following map:
  ;;        num.pref.    comment style
  ;;         1             simplest
  ;;         2             simple
  ;;         3             narrow-box
  ;;         4             wide-box
  ;;  

  ;; This catches the prefix arg (numeric or not) into ask-or-set-style
  ;; (see (&optional ask-or-set-style) above)  
  (interactive "P")

  (let((Style ""))

    ;;
    ;; -------------------------------------
    ;; Set style according to numeric prefix
    ;; -------------------------------------
    ;;    
    (if (integerp ask-or-set-style)
        (progn
          ;; (read-from-minibuffer "ask-or-set-style: " 
          ;;                       (prin1-to-string ask-or-set-style))
          (cond
           ((= ask-or-set-style 1) (progn
                                     (setq Style "simplest")
                                     (setq comment-edit-style Style)
                                     (setq comment-edit-style-default Style)
                                     )
            )
           ((= ask-or-set-style 2) (progn
                                     (setq Style "simple")
                                     (setq comment-edit-style Style)
                                     (setq comment-edit-style-default Style)
                                     )
            )
           ((= ask-or-set-style 3) (progn
                                     (setq Style "narrow-box")
                                     (setq comment-edit-style Style)
                                     (setq comment-edit-style-default Style)
                                     )
            )
           ((= ask-or-set-style 4) (progn
                                     (setq Style "wide-box")
                                     (setq comment-edit-style Style)
                                     (setq comment-edit-style-default Style)
                                     )
            )
           ) ;; cond
          (comment-edit) ;; recursion
          ) ;; progn
      ) ;; (if (integerp ask-or-set-style)

    ;;
    ;; -------------------------------------------------------
    ;; C-u (no numeric): ask explicitly for comment-edit-style
    ;; -------------------------------------------------------
    ;;    
    (if (and ask-or-set-style (listp ask-or-set-style))
        (progn
          ;; (read-from-minibuffer "ask-or-set-style: "
          ;;                       (prin1-to-string ask-or-set-style))
          (setq Style (completing-read
                       (concat "style [" comment-edit-style-default "]: ")
                       comment-edit-style-completion-alist
                       nil ;; predicate
                       nil ;; require-match
                       nil ;; initial
                       comment-edit-style-default ;; hist
                       comment-edit-style-default ;; default
                       nil ;; inherit-input-method
                       ))
          (if (or
               (string= Style "simplest")
               (string= Style "simple")
               (string= Style "narrow-box")
               (string= Style "wide-box")
               ) ;; or
              (progn
                (setq comment-edit-style Style)
                (setq comment-edit-style-default Style)
                ) ;; progn
            ) ;; if
          ) ;; progn
      ) ;; (if (listp ask-or-set-style)

    ;;
    ;; --------------------------------
    ;; Normal behavior: edit a comment
    ;; --------------------------------
    ;;    
    (if (not ask-or-set-style)
        (progn
          ;; (read-from-minibuffer "ask-or-set-style: "
          ;;                       (prin1-to-string ask-or-set-style))
          ;; (read-from-minibuffer "ask-or-set-style: "
          ;;                       (prin1-to-string (listp nil)))
          ;;
          ;; see which mode we are in and set relevant variables accordingly
          ;;      
          (comment-edit-select-mode)
          ;;
          ;; keep track of current position
          ;;      
          (setq comment-edit-indention (current-column))
          (setq comment-edit-fill-column
                (- fill-column (current-column)))
          ;;
          ;; If this is not a blank line and it is not a pure comment line,
          ;; then the comment will be inserted just above it; so, if this is
          ;; the case, create an empty line just above the current line and
          ;; set comment-edit-return-point at its bol
          ;;      
          (beginning-of-line)
          (if (not (re-search-forward "^[[:space:]]*$" (line-end-position) t))
              (if (not (comment-edit-pure-comment-line))
                  (progn
                    (beginning-of-line)
                    (insert "\n")
                    (forward-line -1)
                    )
                ) ;; (if (not (comment-edit-pure-comment-line))
            ) ;; (if (not (re-search-forward ...
          (beginning-of-line)
          (setq comment-edit-return-point (point-marker))
          ;;
          ;; If we are inside a commented region, then get its limits
          ;;      
          (comment-edit-start-end-commented-line)
          ;;
          ;; save current window state
          ;;      
          (comment-edit-save-state)
          ;;
          ;; go!
          ;;      
          (comment-edit-popup-edit-buffer)
          ) ;; progn
      ) ;; (if (not ask-or-set-style)

    );; let
  );; comment-edit ends here

;;; comment-edit.el ends here
