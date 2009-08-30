;;;

;;;
;;; PLT Completions

;;; The overall idea behind this is to provide completion
;;; functionality when using plt scheme.
;; The initial plan is similar to what help desk does at the
;; moment and load the doc keywords into memory, and then use them to
;;; help with completion. 

;; In the future in an ideal world I would like to either hook
;; mzscheme into slime or use a similar technique to hook into a
;; running mzscheme process allowing interrogation of active objects.
;; One thing I have noticed with slime is that it tended to bork when
;; running in vmware, which is not very handy considering how much
;; development can happen using these tools.  Just possibly this is
;; related to the use of sockets.

;; This document contains two different pieces of functionality, the
;; mode for displaying the completions and the code to actually
;; calculate the completions.

;; USAGE: 

;; NOTE: This package uses the common lisp package, which creates a
;; certain amount of namespace pollution.  If you are not comfortable
;; with this, I would suggest using the fabulous help-desk provided by
;; the plt scheme distribution instead.  Being a common lisp slut, I
;; have this set on by default anyway.

;; The pivotal function for this library is plt-completion-command so
;; all you need to do is bind this to a key you would find convenient
;; when you want completion.  Here is what sits in my .emacs
;; (require 'plt-completions)
;; (define-key scheme-mode-map [(f3)] 'plt-completion-command)
;; (define-key scheme-mode-map (kbd "TAB") (indent-or-complete plt-completion-command))

;; If you would prefer that the completions window if it needs to be
;; created should split vertically (one above the other) add this:
;; (setq *plt-buffer-split-horizontally* nil)

;; If you would like to only match from the beginning of a symbol
;; instead of any match add this:
;; (setq *plt-match-all* nil)

;; The code from the last binding is here:
;; (defmacro indent-or-complete (&rest complete)
;;   "A useful pattern for binding the tab (or other) key to, but needs
;; to be able to take specific function names and calls."
;;   `(lambda ()
;;      "Complete if point is at the end of a word, otherwise indent line."
;;      (interactive)
;; ;;     (if (looking-at "\\_>")
;;      (cond ((thing-at-point 'symbol)
;;             (,@complete))
;;            ((thing-at-point 'whitespace)
;;             (indent-for-tab-command)))))

;; NOTE: This sometimes threw an error, but has decided to stop, so no
;; guarantees, but it is very useful, if it behaving well.

;; LIMITATIONS

;; As this was developed to serve a very specific purpose and is very
;; much alpha software, I have only tested it on the platform I
;; developed it on: Emacs 22 (cvs & multi-tty).  It should work with
;; Emacs 21 as there is nothing cutting edge in the code but I have
;; not tested it. As for xemacs, I don't use it so assume that this
;; code only works by coincidence, but I am happy to apply any patches
;; to make it work with xemacs as long as it does not break anything
;; in emacs.

;; FEATURES
;; I am not sure if you are supposed to list features but to make sure
;; there are no surprises for someone who actually has the patience to
;; read through all the preamble.  Not very much to it really, but I
;; have found it very handy, when developing.

;; * As soon as you hit your bound key in a relevant buffer (read
;;   scheme) it will look for your plt docs directory, and if it can't
;;   find one will ask you to locate it for it, and then go about its
;;   business.

;; * When creating a completion it will split your buffer according to
;;   the setting of *plt-buffer-split-horizontally*.  If it is set to
;;   true (the default) it will split the frame horizontally (split
;;   into two lengthwise chunks, I can never get the terminology
;;   sorted out), and display the possible completions in the other
;;   buffer.  If the frame is already split it will just reuse an open
;;   window. To have the window split vertically, just set this
;;   variable to nil.  (Thanks to jao (Jose A. Ortega Ruiz) for
;;   feedback on this)

;; * When the display is showing, hitting the completion command again
;;   will check if there is a change to the search pattern, and if not
;;   will just scroll the completion buffer.  If you reach the end of
;;   the buffer, on the next completion request it will cycle to the
;;   top of the buffer and start down again.

;; * When the cursor is in a plt completions buffer, there are two key
;;   combinations that actually do something. 
;;
;;   * C-return when sitting on a line with a completion will attempt
;;     to find the documentation (using browse-url-at-point) for that
;;     item, using the local documentation.  Note: sometimes the decs
;;     will not be there, because only the keyword file has been
;;     installed.  To make this more useful install the rest of the
;;     docs.  A todo is to make this more robust, so if it cannot find
;;     the reference it looks on line.
;;   * Return - This replaces the symbol at point in the buffer that
;;     called for the completion with the completion where point is.
;;     This is not a pretty system at the moment, and improving it is
;;     on the TODO list.

;; At the moment (since I have only just released it, this is a one
;; person effort.  I hope you find it useful, and please let me know
;; if anything is not working for you, and I will see what I can do to
;; fix it (time permitting).


(require 'derived)
(require 'cl)
(require 'thingatpt)
(require 'scheme)

(defconst plt-completions-version "0.02")
(defconst plt-completions-author-email "rohan.nicholls at gmail.com")
(defconst plt-completions-legal-notice
  "This is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.  This is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See the GNU General Public License for more details.  You
should have received a copy of the GNU General Public License along with Emacs;
see the file `COPYING'.  If not, write to the Free Software Foundation, Inc.,
59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

;;;###autoload
(define-derived-mode plt-completions-mode completion-list-mode "PLT Completions"
                     "Major mode for handling completions in plt scheme
Special commands:
\\{plt-completions-mode-map}"
)
(define-key plt-completions-mode-map (kbd "RET") 'plt-return-completion)
(define-key plt-completions-mode-map [(control return)] 'plt-goto-reference)

;;; The completion building code

;; **NOTE** This needs unit tests, but I don't know where to find a
;; package, and am too lazy at this point to dig up the port I did of
;; Peter Siebel's 26 line setup, which really needs to be sorted so
;; the tests can live elsewhere

;; TODOS:

;; * Even better would be making things even more granular, so if in
;;   srfi, then indicate which one the reference uses (for easy
;;   library requiring).

;; * Revamp the browse-url-at-point code - in order to shorten the
;;   line of code that is actually being put in the buffer (visible
;;   and invisible) have the name, and its section and then find the
;;   entry in the appropriate hash table, and build the url
;;   dynamically, and then call it.  This will allow the entries to be
;;   put into columns easily, instead of only one entry per line.  So
;;   also have some form of string-match 

;; * Convert to using hash-tables - should make the section bits
;;   easier, and finding needed information faster (e.g. for building
;;   the browse-url parts).

(load-library "utilities")

(defvar *plt-doc-dir* nil
  "The root directory of the plt docs (usually found under the
  PLTHOME directory) but not always as the debian package
  installs them under /usr/share/plt/doc")

(defvar *plt-exclude-manuals*
  '("beginning" "advanced" "beginning-abbr" "intermediate" 
    "intermediate-lambda" "profj-advanced" "profj-beginner" 
    "profj-intermediate" "release-notes" "teachpack" "teachpack-htdc"
    "tour" "." "..")
  "Directories for which you do not want to get completions from.
  This will differ according to needs, but I have tried to remove ;
  redundancies by excluding all the teaching languages.
  Excluding as much as possible has the nice side effect of
  really speeding things up as well")

(defvar *plt-match-all* t
  "If t match any substrings that are given, if nil, only symbols
  starting with this match. NOTE: NOT IMPLEMENTED YET - at the moment
  it matches everything." )

(defvar *plt-doc-dirs*  nil
  "List of subdirectories within doc, minus anything found in
  *plt-exclude-manuals*")
;; And yes I am well aware of how horribly inefficient this is. ;)

(defvar *plt-categories* nil
  "A list of relative paths to the different directories that are
  included in the matching")

;; This is now populated in the plt-completion-command
;; Meaning slightly slow startup on first request for a completion
;; but also means that in order to repopulate the list
;; just set this value to nil
(defvar *plt-completions* nil
  "The complete list of keywords for all the documented parts of
  plt.  If you have installed new documentation and wish to reset
  the completions so the new doc is added, just set this to nil,
  and the next call for completions will repopulate the
  database")

(defvar *plt-last-search* nil
  "The last item searched for, if it matches the present search,
then do nice things with the display of completions rather than
looking for a new match")

(defvar *plt-active-buffer* nil
  "The buffer for which the present completion is being searched")

(defvar *plt-buffer-split-horizontally* t
  "If the completions buffer is created, split the frame
  horizontally (side by side) or vertically (one above the
  other).  If non-nil the frame will split horizontally")

(defun plt-get-doc-dir (&optional plt-doc-dir)
  "Obtain the location of the plt doc directory through various means"
  (or plt-doc-dir 
      (let ((pltdocs (getenv "PLT_DOCS")))
        (and pltdocs 
             (file-directory-p pltdocs) 
             pltdocs))
      (let ((plthome (getenv "PLT_HOME")))
        (and plthome 
             (file-directory-p (concat (file-name-as-directory (getenv "PLT_HOME")) "doc"))
             (setenv "PLT_DOCS" (concat (file-name-as-directory plthome) "doc"))))
      *plt-doc-dir*
      (let ((obtained (read-file-name "Please let me know where the plt doc directory is: ")))
        (setenv "PLT_DOCS" obtained))))

;; Possible improvement to have the parameter be appended or intersected with the 
;; *plt-exclude-manuals* list i.e. add new, remove already existing etc.)
(defun plt-get-doc-dirs (&optional relative exclude-list)
  "Obtain list of subdirectories within doc, minus anything found
  in *plt-exclude-manuals*"
  (let ((exclude (if exclude-list (remove-duplicates (append exclude-list 
                                                             *plt-exclude-manuals*))
                     *plt-exclude-manuals*)))
    (remove-if #'(lambda (f)
                   (member f (mapcar #'(lambda (dir)
                                         (if relative
                                             dir
                                             (concat 
                                              (file-name-as-directory *plt-doc-dir*) dir)))
                                     exclude)))
               (directory-files *plt-doc-dir* (not relative)))))

;; It would be a good idea to do the same for the hdindex files in the
;; same directory if they exist, but only use it for a comprehensive
;; search.  NOTE: This needs to be broken down and made neater,
;; esp. for hdindex
;; For building the hash-table
;; list-for-one-keyword: car - hash-name, assoc-list for the rest
(defun plt-get-keywords (dir)
  "Collect the keywords from a keyword file in a particular directory"
  (save-excursion
    (let* ((file (concat dir "/keywords"))
           (buf (and (file-exists-p file) (find-file-noselect file))))
      (cond (buf
             (goto-char (point-min))
;;             (print (format "parsing %s " dir))
             (let ((keyword-list (condition-case tmp
                                     (read buf)
                                   (error nil))))
               (kill-buffer buf)
               (cons dir keyword-list)))))))

;; The structure of the read in items seems to be:
;; (car keyword-list) -> path to directory
;; &rest keyword-list -> list with this structure
;; (first kw-item) -> name
;; (second kw-item) -> calling convention (if any)
;; (third kw-item) -> doc html page name
;; (fourth kw-item) -> anchor name
;; (fifth kw-item) -> not really sure, maybe the chapter name
;; e.g.
;; ("=/c" "(=/c number)" "mzlib-Z-H-12.html" "node_kw_definition=/c" "Flat Contracts")



;;; What needs to be done:
;;; for a directory, need to know the directory (section)
;;; Search for a whole word first:
(defun plt-walk-the-list (word list &optional match-begin)
  "Search for a partial match for word in the first element of
each sublist, and return the lists containing the matches as a
list, if match-begin is set to non-nil or *plt-match-all* is nil
then only finds matches that begin with the pattern"
  (let ((search-for (if (or match-begin (eq *plt-match-all* nil))
                        (concat "^" word) word)))
    (remove-if #'(lambda (elem)
                   (or (null elem)
                       (plt-one-elemp elem)
                       (not (consp elem))))
               (mapcar (lambda (lst)
                         (cons 
                          (if lst (format "%s" (file-name-nondirectory (car lst))))
                          (loop for item being each element of (cdr lst)
                             with lib = (car lst)
                             if (and (string-match search-for (car item))
                                     (not (null item)))
                             collect (list (car item) lib item))))
                       list))))

;; TODO: check the possible settings of the
;; browse-url-browser-function, and adjust the url as needed:
;; w3m on this machine: file://c/path
;; firefox on this machine: file:///c:/path
;; don't know how it works on the mac, will have to check, and 
;; for IE on windows, if anyone using this has that horror as their
;; default browser
(defun plt-os-dependent-url (base page anchor)
  "Create a url that will work in the browser and on the platform 
that is set in the browse-url package"
  (let* ((windows-p (string-match "mingw" (emacs-version)))
         (base-url (if windows-p 
                       (delete-if #'(lambda (char)
                                      (equal char ?\:)) base)
                       base)))
  (format "file://%s/%s%s" base-url page 
                           (if anchor (concat "#" anchor) ""))))

(defun plt-docs-url-for-symbol (symbol-list)
  "Return the url to the documentation for the item named in the
first element of the symbol list"
  (let* ((base-url (second symbol-list))
         (item-detail (third symbol-list))
         (html-page (third item-detail))
         (anchor (if (not (equal (fourth item-detail) ""))
                     (fourth item-detail)))
         (full-url (plt-os-dependent-url base-url html-page anchor)))
    full-url))

;; create [[name][href]] - done in plt-completions-buffer-display
;; Render 3 -> end of name visible the rest invisible
;; figure out onclick event handling, when you click - go to end of link - 
;; backup two chars, and browse-url-at-point - primitive but effective
;; This is handled with a new major mode for plt completions
;; NOTE: This also could be broken down into neater chunks. 

(defun plt-completions-buffer-display (list)
  "Display the results of searching for a match in a buffer, and 
display the buffer either in an existing other window, or split 
the window and display the buffer"
  (plt-display-in-new-buffer 
   "*PLT Completions*" 
   plt-completions-mode
   *plt-buffer-split-horizontally*
    (progn
;;      (setq other-window-scroll-buffer (get-buffer "*PLT Completions*"))
      (loop for cat in list
         concat (plt-format-category-matches cat)))))


(defun plt-format-category-matches (lst)
  (let ((name (car lst))
        (matches (cdr lst)))
    (concat (format "%s\n" (upcase name)) ;; add the section name
            (loop for match in matches
                 concat (let* ((func-name (car match))
                              (call-syntax (second (third match)))
                              (display-text (concat func-name " ::-  " call-syntax))
                              (link (format "[[%s][%s]]\n" display-text
                                             (plt-docs-url-for-symbol match))))
                    (put-text-property 0 2 'invisible t link)
                    (put-text-property (+ 2 (length display-text)) (- (length link) 1)
                                        'invisible t link)
                    link))
            "\n\n")))
;; e.g.
;;(plt-format-category-matches (car (plt-walk-the-list "box" *plt-completions* t)))

(defun plt-completion-command ()
  "Create a completion list in the plt-completions buffer and
display it"
  (interactive)
  (if (not *plt-completions*)
      (progn
        (setq *plt-doc-dir* (plt-get-doc-dir))
        (setq *plt-doc-dirs* (plt-get-doc-dirs))
        (setq *plt-completions* (mapcar #'plt-get-keywords *plt-doc-dirs*))))
  ;; check if the search has changed or not
  (let ((search-for (thing-at-point 'symbol)))
;;    (debug)
    (cond ((and (equal search-for *plt-last-search*)
                (plt-buffer-showing-p "*plt completions*"))
               ;; scroll the *plt completions* buffer
               ;; if at the bottom of the buffer, go to beginning of buffer.
           (plt-scroll-buffer-or-point-min "*PLT Completions*"))
          (t 
           (setq *plt-active-buffer* (current-buffer))
           (plt-completions-buffer-display
            (plt-walk-the-list (thing-at-point 'symbol)
                           *plt-completions*))
           (setq other-window-scroll-buffer (get-buffer "*PLT Completions*"))
           (setq *plt-last-search* search-for)))))
;; e.g.
;; (setq 
;; *plt-doc-dir* nil
;; *plt-doc-dirs* nil
;; *plt-completions* nil)
;; (plt-completion-command)


(defun plt-goto-reference ()
  "When sitting on a line in the plt completions buffer, calling this will
take you to html documentation for that page."
  (interactive)
  (end-of-line)
  (backward-char 2)
  (if (thing-at-point 'url)
      (progn
        (print (format "%s" (thing-at-point 'url)))
        (browse-url-at-point))))

;; ;; Set a couple of keys to do completion for us.  F3 is a backup
;; ;; as the tab stuff can be twitchy.
;; (define-key scheme-mode-map [(f3)] 
;;   'plt-completion-command)

;; (define-key scheme-mode-map (kbd "TAB")
;;   '(lambda ()
;;     (interactive)
;;     (if (looking-at "\\_>")
;;         (plt-completion-command)
;;         (indent-for-tab-command))))
;; Also need to allow that if tab is hit again, you start to cycle
;; through the list of possible completions.

(defun plt-extract-name (line)
  "Extract the name from the text making up the completion"
  (if (string-match "\\[\\[\\(.*\\) ::-" line)
      (substring-no-properties line (match-beginning 1) (match-end 1))))
;;e.g.
;;(plt-extract-name "[[ctype-basetype ::-  (ctype-basetype ctype)][file:///usr/share/plt/doc/foreign/foreign-Z-H-2.html#node_kw_definitionctype-basetype]]")
;; (plt-extract-name "[[color-prefs:build-color-selection-panel ::-  (color-prefs:build-color-selection-panel parent pref-sym style-name example-text)][file:///usr/share/plt/doc/framework/framework-Z-H-823.html#node_kw_fFramework_Functionscolor-prefs_C_build-color-selection-panelg1066]]")

(defun plt-remove-replacement (replace)
  "Very boring function.  Given an object at point, remove it
from just behind point in the current buffer"
  (interactive)
  (let ((back (length replace)))
    (set-mark (point))
    (goto-char (- (point) back))
    (kill-region (point) (mark))))
;;e.g.
;;(plt-remove-replacement "blah")

(defun plt-return-completion ()
  "Take the entry for that line in the completions buffer, and insert it at point in the buffer calling for the completion, replacing the partial completion that had been searched for, and return to the calling buffer."
  (interactive)
  (let ((comp (plt-extract-name (thing-at-point 'line))))
    (cond (comp
           (switch-to-buffer-other-window *plt-active-buffer*)
           (plt-remove-replacement *plt-last-search*)
           (insert (format "%s" comp))))))
  ;; check *plt-active-buffer* for buffer name

;;; UTILITIES 

;; generic version of displaying a list in a new buffer
(defmacro plt-display-in-new-buffer (new-buf-name mode split-horiz &rest code)
  "Display text in a new buffer.  The buffer name and the mode it
  should run are passed in as the first two arguments, the rest
  of the arguments are spliced into an insert statement, so their
  product should be a string.  This is very simple, but does the
  trick.  The buffer that is displayed, either re-uses an already
  open window, or creates a new window splitting the window
  horizontally (equivalent of \"C-x 3\") "
  `(progn
    (interactive)
    (let ((cur-buf (current-buffer)))
      (save-excursion
        (switch-to-buffer (get-buffer-create ,new-buf-name))
        (,mode)
        (erase-buffer)
        (insert
         ,@code)
        (if (<= (length (window-list)) 1)
            (split-window nil nil ,split-horiz))
        (switch-to-buffer cur-buf)
        (other-window 1)
        (switch-to-buffer ,new-buf-name)
        (other-window -1)
        (goto-char (point-min))))))

(defun plt-one-elemp (lst)
  "Test if item is a list of one element"
  (cond ((not (consp lst)) nil)
        ((and (consp lst)
             (null (cdr lst))))
        (t nil)))

(defun plt-buffer-showing-p (name)
  "Is the buffer named by name showing in the current frame"
  (member-if (lambda (w)
               (string= (downcase(buffer-name (window-buffer w))) 
                        (downcase name)))
             (window-list)))

(defun plt-scroll-buffer-or-point-min (&optional buf)
  "Scroll the buffer named buf, or if it is at the end of the
  buffer, return point to the top"
  (interactive)
  (let* ((curr (current-buffer))
         (buf (switch-to-buffer-other-window (or buf (current-buffer)))))
    (unwind-protect
         (if (pos-visible-in-window-p (point-max))
             (goto-char (point-min))
             (scroll-up nil))
      (switch-to-buffer-other-window curr))))

(provide 'plt-completions)

