;;; cfml-helper-mode.el --- Major mode for composing cfml files.
;;; mostly html-helper-mode.el
;;; v 3.0.4jolly

;; Mantainer : Gian Uberto "Saint" Lauri <address@bogus.example.com>
;;                                       <address@bogus.example.com>*
;;                                * works only from DEI, Padova.
;;             http://www.gest.unipd.it/~saint/
;; Original Author: Nelson Minar <address@bogus.example.com>
;; Original version Maintainer: Nelson Minar <address@bogus.example.com>
;; Changes by by: Gian Uberto Lauri <address@bogus.example.com>, <address@bogus.example.com>*
;;                                * works only from DEI, Padova.
;; Credits : Larry Smith and Tony Graham for the ASP/PHP matching regexp
;;           prototype.
;;           Stan Lanning for the defadvice code that prevents indenting
;;           of <PRE></PRE>, for the defadvice code that leaves the cursor
;;           where it is during narrowing to script code, enhancments to
;;           the scripting narrowing
;;           Charles Curley for the commentary of tempo.el behaviour
;;           Samir Barjoud for giving me the luser cap when I didn't notice
;;           that *ALL* I needed to write  cfml-helper-match-asp-php was in
;;           font-lock.el.
;;           Theodore A. Jump for fixing fold tags in this source (after I
;;           broke them
;;           David J. Biesack for suggesting a good version checking.

;; URL: http://www.gest.unipd.it/~saint/cfml-helper-mode.el.gz

;; Created: 01 Feb 1994
;; $Id: cfml-helper-mode.el,v 3.0.4jolly 1998/08/06 18:53:03 nelson Exp $
;; Keywords: HTML major-mode

;; LCD Archive Entry:
;; cfml-helper-mode|Gian Uberto Lauri|address@bogus.example.com|
;; Major mode for editing HTML.|
;; 26-Oct-99|Version 3.?.?|http://www.gest.unipd.it/~saint/cfml-helper-mode.el.gz

;; Copyright (C) 1994 Nelson Minar
;; Copyright (C) 1995 Nelson Minar and Ulrik Dickow
;; Copyright (C) 1999 Nelson Minar, Ulrik Dickow and Gian Uberto Lauri

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;{{{ Commentary

;; Installation:
;;   Add this line in your .emacs:
;;     (autoload 'cfml-helper-mode "cfml-helper-mode" "Yay HTML" t)
;;   To invoke cfml-helper-mode automatically on .html files, do this:
;;     (setq auto-mode-alist (cons '("\\.html$" . cfml-helper-mode) auto-mode-alist))
;;   To invoke cfml-helper-mode automatically on .asp files, do this:
;;     (setq auto-mode-alist (cons '("\\.asp$" . cfml-helper-mode) auto-mode-alist))
;;   To invoke cfml-helper-mode automatically on .phtml files, do this:
;;     (setq auto-mode-alist (cons '("\\.phtml$" . cfml-helper-mode) auto-mode-alist))

;;
;;   This mode requires another lisp file, tempo.el. This can be
;;     retrieved from ftp://ftp.lysator.liu.se/pub/emacs/tempo.el
;;   Xemacs users need to have auc-menu installed.
;;   Emacs 18 users need to have auc-menu and add-hook installed.
;;   If your OS has broken 14 character filenames
;;      this mode will also work with the name "html-mode.el".

;; Configuration:
;;   see the "user variables" section, or the documentation on configuration
;;   in http://www.santafe.edu/~nelson/tools/. There are variables you want to
;;   configure, particularly cfml-helper-address-string and
;;   cfml-helper-use-expert-menu

;; See also: http://www.gest.unipd.it/~saint/hth.html for further details
;; regarding server code support.

;; Description:
;;   cfml-helper-mode makes it easier to write HTML documents. This mode
;;   handles inserting HTML codes in a variety of ways (keybindings, menus,
;;   completion in the buffer). It also supports indentation, timestamps,
;;   skeletons for new documents, hilit19 patterns, and a variety of other
;;   things. For the full skinny, see the HTML documentation that comes
;;   with the package or is at http://www.santafe.edu/~nelson/tools/

;; Thank yous:
;;   David Kågedal <address@bogus.example.com> for the tempo code which
;;     forms the core of the HTML insertion, as well as the HTML+ tag.
;;   Marc Hedlund <address@bogus.example.com> for general encouragement and
;;     many helpful suggestions, especially with HTML/2.0 compliance
;;     and form design.
;;   Ulrik Dickow <address@bogus.example.com> for the font-lock code
;;   Denis Howe <address@bogus.example.com> for writing browse-url.
;;   Magnus Homann <address@bogus.example.com> and Jamshid Afshar
;;     <address@bogus.example.com> for timestamp suggestions.
;;   Everyone who sent me a version of menus (16 in all!)
;;   Marc Andreessen <address@bogus.example.com> for writing the original html-mode

;; The newest version of cfml-helper-mode should always be available from
;;   http://www.gest.unipd.it/~saint/hth.html

;; Changes moved to hhm-changelog

;; This code was writting using folding.el, a wonderful folding editor
;; minor mode for emacs. That's what the strange {{{ comments are for.

;;}}}

;;{{{ Code:

(defconst cfml-helper-mode-version
  (progn
    (let ((revs "$Revision: 3.0.4jolly$")
   (lastchar 0))
      ; revs is a string of single byte characters
      (set 'lastchar (1- (string-width revs)))
      (substring revs 11 lastchar))))

;;{{{ user variables

;;{{{ defcustoms
(defgroup cfml-helper nil
  "Customizing cfml-helper-mode"
  :group 'languages
  :group 'hypermedia
  :group 'local)

(defgroup cfml-helper-faces nil
  "Customizing cfml-helper-mode custom faces"
  :group 'cfml-helper
  :group 'faces)

;; Default distribution doesn't include visual-basic-mode
(defcustom cfml-helper-mode-uses-visual-basic nil
  "Non nil to require visual-basic-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'cfml-helper
  :require 'cfml-helper-mode)

;; Default distribution doesn't include jde
;;
;; Suggestion by :
;; Jari Aalto <address@bogus.example.com>
;;
;;    I think that people that have installed JDE, use it, so
;;    it would be logical to preset this automatically using
;;    `locate-library'
(defcustom cfml-helper-mode-uses-JDE (locate-library "jde")
  "No nil to use jde instead of java-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'cfml-helper
  :require 'cfml-helper-mode)

(defcustom cfml-helper-mode-uses-bold-italic nil
  "Non nil to use the bold-italic font (if your font supports it)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'cfml-helper
  :require 'cfml-helper-mode)

(defcustom cfml-helper-mode-uses-KG-style nil
  "Non nil to make Emacs consider PHP/ASP code blocks beginning in the first column"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'cfml-helper
  :require 'cfml-helper-mode)

(defcustom cfml-helper-mode-global-JSP-not-ASP t
  "Non nil to make Emacs consider <% %> blocks as JSP (global default behaviour)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'cfml-helper
  :require 'cfml-helper-mode)

(defcustom cfml-helper-mode-insert-attributes-always nil
  "non nil to make Emacs insert empty tag attributes when tempo-interactive is nil"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'cfml-helper
  :require 'cfml-helper-mode)
;;}}}

;;{{{ defvars...
(defvar cfml-helper-mode-local-JSP-not-ASP
  cfml-helper-mode-global-JSP-not-ASP
  "Non nil to make Emacs consider <% %> blocks as JSP (buffer local behaviour)")

(defvar cfml-helper-mode-run-the-mode t "When t, make the local variables, else skip")
;; Visual basic mode is not in the standard distribution, so I let the user
;; override cfml-helper-mode-uses-visual-basic with a nil value.
(cond (cfml-helper-mode-uses-visual-basic (require 'visual-basic-mode)))
(cond (cfml-helper-mode-uses-JDE (require 'jde)))
(require 'cc-mode)
(require 'cl)

;; Set this to be whatever signature you want on the bottom of your pages.
(defvar cfml-helper-address-string ""
  "*The default author string of each file.")

;; Features; these are all good to have on. (see also tempo.el)

(defvar cfml-helper-use-expert-menu t
  "*If not nil, then use the full HTML menu.")

(defvar cfml-helper-do-write-file-hooks t
  "*If not nil, then modify `local-write-file-hooks' to do timestamps.")

(defvar cfml-helper-build-new-buffer t
  "*If not nil, then insert `cfml-helper-new-buffer-strings' for new
buffers.")

;; variables to configure (these defaults are reasonable.)

(defvar cfml-helper-htmldtd-version "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"
  "*Version of HTML DTD you're using.")

(defvar cfml-helper-user-menu nil
  "*Extra items to put in the HTML expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus).")

(defvar cfml-helper-basic-offset 2
  "*Basic indentation size used for list indentation")

;; Wed Jan 10 09:35:53 2001 Saint
;;
;; fixing indentation
;;(defvar cfml-helper-item-continue-indent 2
;;  "*Indentation of lines that follow a <li> item.
;;Default is 2, the length of things like \"<li>\" and \"<dd>\".")

(defvar cfml-helper-never-indent nil
  "*If not nil, the indentation code for cfml-helper is turned off.")

;; hooks (see also tempo.el)

;; On prompts... Charles Curley (http://w3.trib.com/~ccurley/): It took
;; some time to figure this out... The (p "prompt: ") and (r "prompt:
;; ") entries indicate where the prompting mode should prompt for a
;; field in the tag. (p ) indicates a parameter, such as the color of a
;; <font> tag. (r ) indicates a region, where the text to be surrounded
;; by the tag should go, such as the text to be turned that color. The
;; difference is this: when prompting mode is turned off and the user
;; is surrounding a region with the tag, the (r ) (region) parameter
;; indicates where the surrounded region will go. The first (p )
;; (parameter) is where the cursor will go, ready to input the first
;; parameter to the tag.

;; So when you have prompting on, and use the font with color and size
;; tag, put the cursor where you want the modified text to go. Start
;; inserting the tag. You will be prompted for the color, the size, and
;; then the text to display that way. When you have prompting turned
;; off, and don't have a region blocked, insert the font tag, and the
;; cursor will be at the the first parameter. Then tab over to the
;; space between the two parts of the tag, and type in your text. If
;; you have region blocked, C-u followed by the tag will surround the
;; region with the tag. The blocked region goes into the (r )
;; parameter. Then the cursor is placed at the first (p ) location,
;; ready for you to type in a parameter, such as the color of the text.

(defvar cfml-helper-mode-hook nil
  "*Hook run when cfml-helper-mode is started.")

(defvar cfml-helper-load-hook nil
  "*Hook run when cfml-helper-mode is loaded.")

(defvar cfml-helper-timestamp-hook 'cfml-helper-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")

;; strings you might want to change

(defvar cfml-helper-new-buffer-template
  '(cfml-helper-htmldtd-version
    "<html> <head>\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "<h1>" p "</h1>\n\n"
    p
    "\n\n<hr>\n"
    "<address>" cfml-helper-address-string "</address>\n"
    cfml-helper-timestamp-start
    cfml-helper-timestamp-end
    "\n</body> </html>\n")
  "*Template for new buffers.
Inserted by `cfml-helper-insert-new-buffer-strings' if
`cfml-helper-build-new-buffer' is set to t")

(defvar cfml-helper-new-ASP-buffer-template
  '("<%@ LANGUAGE=\"" p "\" %>\n"
    "<html> <head>\n"
    "<%\n\n%>\n"
    "</head><body>\n"
    "<% '<!-- " cfml-helper-timestamp-start "  " cfml-helper-timestamp-end
    " --> %>\n"
    "\n</body></html>\n")
"*Template for new ASP buffers.
Inserted by `cfml-helper-insert-new-ASP-buffer-strings' if
`cfml-helper-build-new-buffer' is set to t")

(defvar cfml-helper-new-PHP-buffer-template
  '("<html> <head>\n"
    "<? PHP\n\n?>"
    "</head><body>\n"
    "<? /* " cfml-helper-timestamp-start "\n\n"
    cfml-helper-timestamp-end
    " */ ?>\n"
    "\n</body></html>\n"
    )
"*Template for new PHP buffers.
Inserted by `cfml-helper-insert-new-PHP-buffer-strings' if
`cfml-helper-build-new-buffer' is set to t")

;; Someone has some better idea ?
(defvar cfml-helper-new-JSP-buffer-template
  '("<html> <head>\n"
    "<%\n\n%>\n"
    "</head><body>\n"
    "<% '<!-- " cfml-helper-timestamp-start "  " cfml-helper-timestamp-end
    " --> %>\n"
    "\n</body></html>\n")
"*Template for new JSP buffers.
Inserted by `cfml-helper-insert-new-ASP-buffer-strings' if
`cfml-helper-build-new-buffer' is set to t")

(defvar cfml-helper-timestamp-start "<!-- hhmts start --> "
  "*Start delimiter for timestamps.
Everything between `cfml-helper-timestamp-start' and
`cfml-helper-timestamp-end' will be deleted and replaced with the output
of the functions `cfml-helper-timestamp-hook' if
`cfml-helper-do-write-file-hooks' is t")

(defvar cfml-helper-timestamp-end "<!-- hhmts end -->"
  "*End delimiter for timestamps.
Everything between `cfml-helper-timestamp-start' and
`cfml-helper-timestamp-end' will be deleted and replaced with the output
of the function `cfml-helper-insert-timestamp' if
`cfml-helper-do-write-file-hooks' is t")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar cfml-helper-types-to-install
  '(anchor list header logical phys textel entity image table head form
script)
  "*List of tag types to install when cfml-helper-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

;; emacs18 detection.

(defvar cfml-helper-emacs18
  (and (boundp 'emacs-version)
       (or (and (boundp 'epoch::version) epoch::version)
           (string-lessp emacs-version "19")))
  "I'll do minimal emacs18 support, grumble.")
;;}}} end of defvars

;;}}} end of user variables

;;{{{ Prologue

(require 'tempo)   ;essential part of cfml-helper-mode
(condition-case nil   ;menu support, standard in emacs19
    (require 'auc-menu)   ;add-on for XEmacs. *why* does this
  (error (require 'easymenu)))  ;package have to have two names?

;; Wed Mar 28 09:55:29 2001 Saint
;;
;; Placing here (require 'psgml-html ) after a suggestion from Graham
;; Gough who explored the faces problem with XEmacs (many thanks)
(if (string-match "XEmacs\\|Lucid" (emacs-version))
      ;; This will initialize cfml-helper-bold-face
      (require 'psgml-html))

(require 'font-lock)
;;}}}

;;{{{ cfml-helper-mode-syntax-table and cfml-helper-mode-abbrev-table

;; emacs doesn't seem to be able to really handle SGML like syntax. In
;; particular, comments are a loss.
;; We do try this, though: give < and > matching semantics

(defvar cfml-helper-mode-syntax-table nil
  "Syntax table for cfml-helper.")

(if cfml-helper-mode-syntax-table
    ()
  (setq cfml-helper-mode-syntax-table (make-syntax-table
text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " cfml-helper-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " cfml-helper-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " cfml-helper-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " cfml-helper-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " cfml-helper-mode-syntax-table))

(defvar cfml-helper-mode-abbrev-table nil
  "Abbrev table used while in cfml-helper-mode.")
(define-abbrev-table 'cfml-helper-mode-abbrev-table ())

;;}}}

;;{{{ type based keymap and menu variable and function setup

;; Our basic keymap.
(defvar cfml-helper-mode-map (make-sparse-keymap)
  "Keymap for cfml-helper")
(defvar cfml-helper-mode-menu nil
  "Menu for cfml-helper. Clobbered and rebuilt by
`cfml-helper-install-menu'")

;; cfml-helper-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu.
;; Types can be added to the system after cfml-helper has been loaded,
;; briefly by doing cfml-helper-add-type-to-alist, then
;; cfml-helper-install-type, then cfml-helper-add-tag (for each tag)
;; then cfml-helper-rebuild-menu. See the mode documentation for more detail.

(defconst cfml-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `cfml-helper-add-type-to-alist'.")

(defun cfml-helper-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type cfml-helper-type-alist))))

(defun cfml-helper-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type cfml-helper-type-alist))))

(defun cfml-helper-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type cfml-helper-type-alist))))

(defun cfml-helper-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type cfml-helper-type-alist))))

(defun cfml-helper-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (cfml-helper-menu-string-for type)
 (eval (cfml-helper-menu-for type))))

(defun cfml-helper-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq cfml-helper-type-alist (cons type cfml-helper-type-alist)))

;; Here are the types provided by cfml-helper-mode.
(mapcar 'cfml-helper-add-type-to-alist
  '((entity  . (nil nil cfml-helper-entity-menu "Insert Character Entities"))
    (textel  . (nil nil cfml-helper-textel-menu "Insert Text Elements"))
    (head    . (cfml-helper-head-map "\C-c\C-h"
         cfml-helper-head-menu
         "Insert Structural Elements"))
    (header  . (cfml-helper-header-map "\C-c\M-h"
           cfml-helper-header-menu
           "Insert Headers"))
    (anchor  . (cfml-helper-anchor-map "\C-c\C-a"
           cfml-helper-anchor-menu
           "Insert Hyperlinks"))
    (logical . (cfml-helper-logical-map "\C-c\M-l"
     cfml-helper-logical-menu
     "Insert Logical Styles"))
    (phys    . (cfml-helper-phys-map "\C-c\C-p"
         cfml-helper-phys-menu
         "Insert Physical Styles"))
    (list    . (cfml-helper-list-map "\C-c\C-l"
         cfml-helper-list-menu
         "Insert List Elements"))
    (form    . (cfml-helper-form-map "\C-c\C-f"
         cfml-helper-form-menu
         "Insert Form Elements"))
    (image   . (cfml-helper-image-map "\C-c\C-i"
          cfml-helper-image-menu
          "Insert Inlined Images"))
    (table   . (cfml-helper-table-map "\C-c\C-t"
          cfml-helper-table-menu
          "Insert Tables"))
    (script  . (cfml-helper-script-map "\C-c\C-s"
           cfml-helper-script-menu
           "Insert Scripts"))
    ))

;; Once cfml-helper-mde is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst cfml-helper-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

;; For easy ASP/JSP switch
(defun cfml-helper-use-JSP-this-buffer ()
  (interactive)
  (setq cfml-helper-mode-local-JSP-not-ASP t)
  (setq mode-name "HTML/JSP helper")
  (setq major-mode 'jsp-cfml-helper-mode))

(defun cfml-helper-use-ASP-this-buffer ()
  (interactive)
  (cond (cfml-helper-mode-uses-visual-basic
  (setq cfml-helper-mode-local-JSP-not-ASP nil)
  (setq mode-name "HTML/ASP helper")
  (setq major-mode 'asp-cfml-helper-mode))
 (t (error "Visual basic mode required for ASP"))))

(defun cfml-helper-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with cfml-helper-add-type-to-alist."
  (setq cfml-helper-installed-types (cons type cfml-helper-installed-types))
  (let ((keymap (cfml-helper-keymap-for type))
 (key (cfml-helper-key-for type))
 (menu (cfml-helper-menu-for type))
 (menu-string (cfml-helper-menu-string-for type)))
    (and key
  (progn
    (set keymap nil)
    (define-prefix-command keymap)
    (if cfml-helper-emacs18
        (progn
   (set keymap (make-sparse-keymap))
   (define-key cfml-helper-mode-map key (eval keymap)))
      (define-key cfml-helper-mode-map key keymap))))
    (and menu
  (progn
    (set menu nil)))))

;; install the default types.
(mapcar 'cfml-helper-install-type cfml-helper-types-to-install)

;; special mode keys
(mapcar
 (function (lambda (l) (define-key cfml-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;; Extra commands that HTML helper supports that aren't insertions
(defvar cfml-helper-mode-functions-map nil
  "Keymap for extra HTML mode functions")

(define-prefix-command 'cfml-helper-mode-functions-map)
(define-key cfml-helper-mode-map "\C-c\C-z"
  'cfml-helper-mode-functions-map)
(define-key cfml-helper-mode-functions-map "t"
  'cfml-helper-insert-timestamp-delimiter-at-point)
(define-key cfml-helper-mode-functions-map "a"
  'html-script-narrow-to-asp)
(define-key cfml-helper-mode-functions-map "p"
  'html-script-narrow-to-php)
(define-key cfml-helper-mode-functions-map "b"
  'html-script-narrow-to-vbscript)
(define-key cfml-helper-mode-functions-map "j"
  'html-script-narrow-to-javascript)

;; indentation keys - only rebind these if the user wants indentation
(if cfml-helper-never-indent
    ()
  (define-key cfml-helper-mode-map "\t" 'cfml-helper-indent-command)
  (define-key cfml-helper-mode-map "\C-m" 'newline-and-indent))

;; browse url stuff
(if (fboundp 'browse-url-of-file)
    (define-key cfml-helper-mode-functions-map "v" 'browse-url-of-file))

;; Fri Jan 12 18:30:32 2001 Saint
;;
;; Jack Vinson supplies this code to handle the case when
;; browse-url-browser-function is a list and not a function (it can be
;; a list, says its documentation)
;;
;; Replacing :
;; (if (and (boundp 'browse-url-browser-function)
;;   (fboundp browse-url-browser-function))
;;     (define-key cfml-helper-mode-functions-map "u"
;;       browse-url-browser-function))

(if (boundp 'browse-url-browser-function)
    (let ((bf browse-url-browser-function)
   re)
      (while (consp bf)
 (setq re (car (car bf))
       bf (if (string-match re "http")
       (cdr (car bf)) ; The function
     (cdr bf))))  ; More pairs
      (or bf (error "No browser in browse-url-browser-function for general URL's"))
      (fboundp bf)
      (define-key cfml-helper-mode-functions-map "u" bf)
      ))

;;}}}

;;{{{ cfml-helper-add-tag function for building basic tags

(defvar cfml-helper-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun cfml-helper-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
  (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
   (aset s l ?\-))
      (setq l (1- l)))
    (concat "html-" (downcase s))))


(defun cfml-helper-add-tag (l)
  "Add a new tag to cfml-helper-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(cfml-helper-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
  (keymap (cfml-helper-keymap-for type))
  (menu (cfml-helper-menu-for type))
  (key (nth 1 l))
  (completer (nth 2 l))
  (name (nth 3 l))
  (tag (nth 4 l))
  (doc (nth 5 l))
  (command (if (string-equal completer "function")
        (nth 4 l)
        (tempo-define-template (cfml-helper-string-to-symbol name)
      tag completer doc
      'cfml-helper-tempo-tags))))

    (if (null (memq type cfml-helper-installed-types))    ;type loaded?
 t                                                 ;no, do nothing.
      (if (stringp key)                     ;bind key somewhere?
   (if keymap                     ;special keymap?
       (define-key (eval keymap) key command)      ;t:   bind to prefix
     (define-key cfml-helper-mode-map key command));nil: bind to global
 t)
      (if menu                      ;is there a menu?
   (set menu                     ;good, cons it in
        (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'cfml-helper-add-cookie 'cfml-helper-add-tag)

;;}}}

;;{{{ most of the HTML tags


(defun cfml-helper-insert-or-wipe (string)
  "Propmts for the value of an optional attribute named STRING and
inserts it in the current buffer. Inserts nothing if the users replies
with a null string."
  (cond ((and (stringp string)
       tempo-interactive)
  (let ((val (read-from-minibuffer (concat string " :"))))
    (cond ((> (string-width val) 0)
    (insert-string (concat " " string "=\"" val "\"" )))
   )))
 ;; just to tell that there's something weird in the calling
 ;; code... But behaves like a no op if tempo-interactive is
 ;; nil
 (tempo-interactive
  (error (concat "Wrong type argument: stringp, " string)))
 ;; Wed Mar 28 10:06:24 2001 Saint
 ;;
  ;; To insert empty attributes if
 ;; cfml-helper-mode-insert-attributes-always is non nil
 ((and (stringp string)
       cfml-helper-mode-insert-attributes-always)
  (insert-string (concat " " string "=\"\"" )))
       ))

;; These tags are an attempt to be HTML/2.0 compliant, with the exception
;; of container <p>, <li>, <dd>, <dt> (we adopt 3.0 behaviour).
;; For reference see <URL:http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html>

;; order here is significant: within a tag type, menus and mode help
;; go in the reverse order of what you see here. Sorry about that, it's
;; not easy to fix.

(mapcar
 'cfml-helper-add-tag
 '(
   ;;entities
   (entity  "\C-c#"   "&#"              "Ascii Code"      ("&#" (r "Ascii:
") ";"))
   (entity  "\C-c\""  "&quot;"          "Quotation mark"  ("&quot;"))
   (entity  "\C-c$"   "&reg;"           "Registered"      ("&reg;"))
   (entity  "\C-c@"   "&copy;"          "Copyright"       ("&copy;"))
   (entity  "\C-c-"   "&shy;"           "Soft Hyphen"     ("&shy;"))
   (entity  "\C-c "   "&nbsp;"  "Nonbreaking Space"  ("&nbsp;"))
   (entity  "\C-c&"   "&amp;"  "Ampersand"   ("&amp;"))
   (entity  "\C-c>"   "&gt;"    "Greater Than"       ("&gt;"))
   (entity  "\C-c<"   "&lt;"  "Less Than"   ("&lt;"))
   ;; letters with accents common in italian
   (entity  "\C-ca"   "&agrave;"        "a` (&&agrave;)"
("&agrave;"))
   (entity  "\C-ce"   "&egrave;"        "e` (&&egrave;)"
("&egrave;"))
   (entity  "\C-cE"   "&eacute;"        "e' (&&eacute;)"
("&eacute;"))
   (entity  "\C-co"   "&ograve;"        "o` (&&ograve;)"
("&ograve;"))
   (entity  "\C-ci"   "&igrave;"        "i` (&&igrave;)"
("&igrave;"))
   (entity  "\C-cu"   "&ugrave;"        "u` (&&ugrave;)"
("&ugrave;"))

   ;; logical styles
   (logical "b"       "<blockquote>" "Blockquote"
     ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c"       "<code>"  "Code"
     ("<code>" (r "Code: ") "</code>"))
   (logical "x"       "<samp>"  "Sample"
     ("<samp>" (r "Sample code") "</samp>"))
   (logical "r"       "<cite>"  "Citation"
     ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k"       "<kbd>"  "Keyboard Input"
     ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v"       "<var>"  "Variable"
     ("<var>" (r "Variable: ") "</var>"))
   (logical "d"       "<dfn>"  "Definition"
     ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a"       "<address>" "Address"
     ("<address>" r "</address>"))
   (logical "e"       "<em>"  "Emphasized"
     ("<em>" (r "Text: ") "</em>"))
   (logical "s"       "<strong>" "Strong"
     ("<strong>" (r "Text: ") "</strong>"))
   (logical "p"       "<pre>"  "Preformatted"
     ("<pre>" (r "Text: ") "</pre>"))

   ;;physical styles
   (phys    "s"       "<strike>" "Strikethru"
     ("<strike>" (r "Text: ") "</strike>"))
   (phys    "u"       "<u>"  "Underline"
     ("<u>" (r "Text: ") "</u>"))
   (phys    "i"       "<i>"  "Italic"
     ("<i>" (r "Text: ") "</i>"))
   (phys    "b"       "<b>"      "Bold"
     ("<b>" (r "Text: ") "</b>"))
   (phys    "f"       "<tt>"  "Fixed"
     ("<tt>" (r "Text: ") "</tt>"))
   (phys    "c"       "<center>"        "Center"
     ("<center>" (r "Text: ") "</center>"))

;; html4.0 stuff, omitted

;    (phys    "5" "<span style=" "Spanning style"
;       ("<span style=\"" (p "style: ") "\">" 'r "</span>"))
;    (phys    "l" "<span class=" "Spanning class"
;       ("<span class=\"" (p "class: ") "\">" 'r "</span>"))


   ;;headers
   (header  "6"       "<h6>"  "Header 6"
     ("<h6>" (r "Header: ") "</h6>"))
   (header  "5"       "<h5>"  "Header 5"
     ("<h5>" (r "Header: ") "</h5>"))
   (header  "4"       "<h4>"  "Header 4"
     ("<h4>" (r "Header: ") "</h4>"))
   (header  "3"       "<h3>"  "Header 3"
     ("<h3>" (r "Header: ") "</h3>"))
   (header  "2"       "<h2>"  "Header 2"
     ("<h2>" (r "Header: ") "</h2>"))
   (header  "1"       "<h1>"      "Header 1"
     ("<h1>" (r "Header: ") "</h1>"))

   ;; forms
   (form    "o"       "<option>"        "Option"
     (& "<option>" > ))
   (form    "v"       "<option value"   "Option with Value"
     (& "<option value=\"" (r "Value: ") "\">" >))
   (form    "s"       "<select"  "Selections"
     ("<select"
      (cfml-helper-insert-or-wipe "name") ">\n<option>" >
"\n</select>")"<select")
   (form    "z"       "<input"  "Reset Form"
     ("<input type=\"RESET\""
      (cfml-helper-insert-or-wipe "value") ">"))
   (form    "m"       "<input"  "Submit Form"
     ("<input type=\"SUBMIT\""
      (cfml-helper-insert-or-wipe "value") ">"))
   (form    "b"       "<input"          "Button"
     ("<input type=\"BUTTON\""
      (cfml-helper-insert-or-wipe "value")
      (cfml-helper-insert-or-wipe "name") ">"))
   (form    "i"       "<input"  "Image Field"
     ("<input type=\"IMAGE\""
      (cfml-helper-insert-or-wipe "Name")
      (cfml-helper-insert-or-wipe "src") ">"))
   (form    "h"       "<input"          "Hidden Field"
     ("<input type=\"HIDDEN\""
      (cfml-helper-insert-or-wipe "name")
      (cfml-helper-insert-or-wipe "value") ">"))
   (form    "p"       "<textarea" "Text Area"
     ("<textarea"
      (cfml-helper-insert-or-wipe "name")
      (cfml-helper-insert-or-wipe "rows")
      (cfml-helper-insert-or-wipe "cols") ">" r "</textarea>"))
   (form    "c"       "<input"  "Checkbox"
     ("<input type=\"CHECKBOX\""
      (cfml-helper-insert-or-wipe "name") ">"))
   (form    "r"       "<input"  "Radiobutton"
     ("<input type=\"RADIO\""
      (cfml-helper-insert-or-wipe "Name") ">"))
   (form    "t"       "<input"  "Text Field"
     ("<input type=\"TEXT\""
      (cfml-helper-insert-or-wipe "name")
      (cfml-helper-insert-or-wipe "size") ">"))
   (form    "f"       "<form"           "Form"
     ("<form"
      (cfml-helper-insert-or-wipe "action")
      (cfml-helper-insert-or-wipe "method") ">\n</form>\n"))

   ;;lists
   (list    "t"       "<dt>"            "Definition Item"
     (& "<dt>" > (r "Term: ") "\n<dd>" >
        (r "Definition: ")))
   (list    "l"       "<li>"            "List Item"
     (& "<li>" > (r "Item: ") > "</li>"))
   (list    "r"       "<dir>"  "DirectoryList"
     (& "<dir>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</dir>" >))
   (list    "m"       "<menu>"  "Menu List"
     (& "<menu>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</menu>" >))
   (list    "o"       "<ol>"  "Ordered List"
     (& "<ol>" > "\n<li>" > (r "Item: ") "\n</li>" > "\n</ol>" >))
   (list    "d"       "<dl>"  "Definition List"
     (& "<dl>" > "\n<dt>" >
        (p "Term: ") "\n<dd>" >
        (r "Definition: ") "\n</dl>" >))
   (list    "u"       "<ul>"  "Unordered List"
     (& "<ul>" > "\n<li>" > (r "Item: ") "\n</li>\n</ul>" >))

   ;;anchors
   (anchor  "n"       "<a name=" "Link Target"
     ("<a " (cfml-helper-insert-or-wipe "name") ">"
      (r "Anchor text: ") "</a>"))
   (anchor  "l"       "<a href="        "Hyperlink"
     ("<a href=\"" (p "href: ") "\" >"
      (r "Anchor text: ") "</a>"))

   ;;graphics
;    (image   "a"       nil               "Aligned Image"
;      ("<img align=\""
;       (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\">"))
;    (image   "i"       "<img src=" "Image"
;      ("<img src=\""
;       (r "Image URL: ") "\">"))
;    (image   "e"       "<img align="     "Aligned Image With Alt. Text"
;      ("<img align=\""
;       (r "Alignment: ") "\" src=\""
;       (r "Image URL: ") "\" alt=\""
;       (r "Text URL: ") "\">"))
;    (image   "t"       "<img alt=" "Image With Alternate Text"
;      ("<img alt=\""
;       (r "Text URL: ") "\" src=\""
;       (r "Image URL: ") "\">"))
;; New, (almost) all including, single menu item entry
;; src has to be there!
   (image "a" nil "Image"
  ("<img src=\""
   (p "src" ) "\" "
   (cfml-helper-insert-or-wipe  "alt" )
   (cfml-helper-insert-or-wipe  "height" )
   (cfml-helper-insert-or-wipe  "width" )
   (cfml-helper-insert-or-wipe  "align" )
   ">"))
   ;; table
   (table   "t"       "<table>"         "Table"
     ("<table"
      (cfml-helper-insert-or-wipe  "border" )
      (cfml-helper-insert-or-wipe "width" )">\n</table>"))
   (table   "r"       "<tr>"         "Table Row"
     ("<TR>\n</TR>"))
   (table   "h"       "<th>"         "Table Header"
     ("<TH"
      (cfml-helper-insert-or-wipe "rowspan" )
      (cfml-helper-insert-or-wipe "colspan")">\n</TH>"))
   (table   "d"       "<td>"         "Table Data"
     ("<TD"
      (cfml-helper-insert-or-wipe "align" )
      (cfml-helper-insert-or-wipe "colspan")"></TD>"))
   (table   "p" "<caption>" "html table caption"
     ("<caption>" (r . "Table: ") "</caption>"))
   ;;text elements
   (textel  "\C-c="    nil  "Horizontal Line"
     (& "<hr>\n"))
   (textel  "\C-c\C-m" nil  "Line Break"
     ("<br>\n"))
   (textel  "\e\C-m"  nil  "Paragraph"
     ("<p>"
      (r "Text: ") "</p>"))

   ;;head elements
   (head    "H"       "<head>"          "Head"
     ("<head>\n" "</head>\n"))
   (head    "B"       "<body>"          "Body"
     ("<body>\n" "</body>\n"))
   (head    "i"       "<isindex>" "Isindex"
     ("<isindex>\n"))
   (head    "n"       "<nextid>" "Nextid"
     ("<nextid>\n"))
   (head    "h"       "<meta http-equiv=" "HTTP Equivalent"
     ("<meta"
      (cfml-helper-insert-or-wipe "http-equiv") " content=\""
      (r "Content: ") "\">\n"))
   (head    "m"       "<meta name="     "Meta Name"
     ("<meta"
      (cfml-helper-insert-or-wipe "name") " content=\""
      (r "Content: ") "\">\n"))
   (head    "l"       "<link"  "Link"
     ("<link href=\"" p "\">"))
   (head    "b"       "<base"  "Base"
     ("<base href=\"" r "\">"))
   (head    "t"       "<title>"  "Title"
     ("<title>" (r "Document title: ") "</title>"))
   ;; scripting elements
   (script  "j"    "<SCRIPT>"       "JavaScript"
     ("<SCRIPT TYPE=\"text/javascript\">\n"
      (r "Script") "</SCRIPT>"))
   (script  "v"    "<SCRIPT>"       "VBScript"
     ("<SCRIPT TYPE=\"text/vbscript\">\n"
      (r "Script") "</SCRIPT>"))
   (script  "%"    "<%="            "ASP output"
     ("<%="(p " Variabile: ")"%>"))
   (script  "a"    "<%xx%>"         "JSP/ASP code"
     ("<%\n"(r "Code: " )"\n%>"))
   (script  "<"    "<%xx%>"         "JSP/ASP break"
     ("%>\n"(r "Code: " )"\n<%"))
   (script  "="    "<?="            "PHP output"
     ("<?="(p " Variabile: ")"?>"))
   (script  "p"    "<?xx?>"         "PHP code"
     ("<? PHP\n"(r "Code: " )"\n?>"))
   (script  "?"    "<?xx?>"         "PHP break"
     ("?>\n"(r " Code: " )"\n<? PHP"))
   ))

;;}}}

;;{{{ cfml-helper-smart-insert-item

;; there are two different kinds of items in HTML - those in regular
;; lists <li> and those in dictionaries <dt>..<dd>
;; This command will insert the appropriate one depending on context.

(defun cfml-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  (interactive "*P")
  (let ((case-fold-search t))
    (if
        (save-excursion
          (re-search-backward
"<li>\\|<dt>\\|<ul>\\|<ol>\\|<dd>\\|<menu>\\|<dir>\\|<dl>" nil t)
          (looking-at "<dt>\\|<dl>\\|<dd>"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-list-item arg))))

;; special keybindings in the prefix maps (not in the list of tags)
(and (boundp 'cfml-helper-list-map)
     (define-key cfml-helper-list-map "i" 'cfml-helper-smart-insert-item))

;; and, special menu bindings
(and (boundp 'cfml-helper-list-menu)
     (setq cfml-helper-list-menu
    (cons '["List Item" cfml-helper-smart-insert-item t]
cfml-helper-list-menu)))

;;}}}

;;{{{ menu support

;; menus are built for easymenu. cfml-helper-add-tag builds
;; submenus based on tag type, the expert menu code lumps them
;; together into one list and calls easy-menu-define

(defvar cfml-helper-novice-menu
  '("HTML"
    ["Insert Paragraph" tempo-template-html-paragraph t]
    ["Insert Hyperlink" tempo-template-html-hyperlink t]
    ["Insert Big Header" tempo-template-html-header-2 t]
    ["Insert Unordered List" tempo-template-html-unordered-list t]
    ["Insert List Item" cfml-helper-smart-insert-item t]
    ["Insert Inlined Image" tempo-template-html-image-with-alternate-text t]
    ["Turn on Expert Menu" cfml-helper-toggle-expert-menu t])
  "Menu for novices, only installed if `cfml-helper-use-expert-menu is
nil'")

(defun cfml-helper-menu nil
  "Return the proper menu. Looks at `cfml-helper-use-expert-menu'"
  (if cfml-helper-use-expert-menu
      (cfml-helper-expert-menu)
    cfml-helper-novice-menu))

(defun cfml-helper-rebuild-menu nil
  "Rebuild and install the HTML menu (using `easy-menu-define').
If `cfml-helper-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (cfml-helper-menu)))
    (easy-menu-remove menu)
    (easy-menu-define cfml-helper-mode-menu-symbol
        cfml-helper-mode-map "HTML menus" menu)
    (easy-menu-add menu cfml-helper-mode-map)))

(defun cfml-helper-toggle-expert-menu (&optional arg)
  "Toggle full HTML menus. Optional arg acts like minor-mode args."
  (interactive "P")
  (setq cfml-helper-use-expert-menu
 (if (null arg) (not cfml-helper-use-expert-menu)
   (> (prefix-numeric-value arg) 0)))
  (cfml-helper-rebuild-menu))

;; If browse-url loaded, add this in the novice menu.
(if (fboundp 'browse-url-of-file)
    (setq cfml-helper-novice-menu
   (append cfml-helper-novice-menu
    (list ["Load This Buffer in Browser" browse-url-of-file t]))))

;; Narrrowing to scripts, this don't use tempo because has to call functions
;; and not insert templates


;; Expert menus: consed up out of cfml-helper-installed-types
(defun cfml-helper-expert-menu ()
  "This menu is based on the current value of `cfml-helper-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; first, reset this so we can call this again and again.
  (setq cfml-helper-mode-menu nil)

  ;; Cons in the toggle of the menu
  (setq cfml-helper-mode-menu
 (cons '["Turn on Novice Menu"
  cfml-helper-toggle-expert-menu t]
       cfml-helper-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq cfml-helper-mode-menu
 (append cfml-helper-user-menu cfml-helper-mode-menu))

  ;; Now cons in the browse-url functions
  (if (fboundp 'browse-url-of-file)
    (setq cfml-helper-mode-menu
   (cons '["Load this Buffer in Browser" browse-url-of-file t]
  cfml-helper-mode-menu)))

  ;; Mon Jan 15 07:49:39 2001 Saint
  ;;
  ;; Jack Vinson supplied this code to handle the case where
  ;; browse-url-browser-function is a list, too
  (if (boundp 'browse-url-browser-function)
      (let ((bf browse-url-browser-function)
     re)
 (while (consp bf)
   (setq re (car (car bf))
  bf (if (string-match re "http")
         (cdr (car bf)) ; The function
       (cdr bf))))  ; More pairs
 (or bf
     (error
      "No browser in browse-url-browser-function for general URL's"))
 (fboundp bf)
 (setq cfml-helper-mode-menu
       (cons (vector "Browse URL at point" bf t)
      cfml-helper-mode-menu))))

  ;; cons in the timestamp delimiters
  (setq cfml-helper-mode-menu
 (cons '["Insert Timestamp Delimiter"
  cfml-helper-insert-timestamp-delimiter-at-point t]
       cfml-helper-mode-menu))

  ;; cons script narrowing
  (setq cfml-helper-mode-menu
 (append cfml-helper-mode-menu
  (list ["Narrow to ASP" html-script-narrow-to-asp t])))
  (setq cfml-helper-mode-menu
 (append cfml-helper-mode-menu
  (list ["Narrow to PHP" html-script-narrow-to-php t])))
  (setq cfml-helper-mode-menu
 (append cfml-helper-mode-menu
  (list ["Narrow to VBScript" html-script-narrow-to-vbscript t])))
   (setq cfml-helper-mode-menu
  (append cfml-helper-mode-menu
   (list ["Narrow to JavaScript" html-script-narrow-to-javascript t])))
   (setq cfml-helper-mode-menu
  (append cfml-helper-mode-menu
   (list ["Use ASP" cfml-helper-use-ASP-this-buffer  t])))

   (setq cfml-helper-mode-menu
  (append cfml-helper-mode-menu
   (list ["Use JSP" cfml-helper-use-JSP-this-buffer t])))

  ;; now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
        (setq cfml-helper-mode-menu
       (cons (cfml-helper-normalized-menu-for type)
      cfml-helper-mode-menu))))
   cfml-helper-installed-types)

  ;; now tack on our name
  (setq cfml-helper-mode-menu (cons "HTML" cfml-helper-mode-menu))
  cfml-helper-mode-menu)

(cfml-helper-rebuild-menu)

;;}}}

;;{{{ context guessing

;; guess where we are in indented lists based on the last list token.
;; it would be much better to try to match </ul> to <ul>, and </ol> to <ol>
;; etc, but that is pretty unwieldy and slow.
;; Note, we make select/option look like a list structure too, so indentation
;; works. This is a bit weird, but it's ok.

;; Wed Jan 10 09:01:06 2001 Saint
;;
;; Changed regexps to handle tags with attributes
(defvar cfml-helper-any-list-item-start
  ;;  "<li>\\|<dt>\\|<dd>\\|<option\\|<th>\\|<td>")
  "<li\W\\|<dt\\|<dd\\|<option\\|<th\\|<td")
(defvar cfml-helper-any-list-item-end
"</li>\\|</dt>\\|</dd>\\|</th>\\|</td>")
(defvar cfml-helper-any-list-start
  ;;  "<dl>\\|<ul>\\|<ol>\\|<menu>\\|<dir>\\|<select\\|<table\\|<tr>")
  ;; "<dl\\|<ul\\|<ol\\|<menu\\|<dir\\|<select\\|<table\\|<tr")

"<dl\\|<ul\\|<ol\\|<menu\\|<dir\\|<select\\|<head>\\|<title>\\|<cfinsert>\\|
<table\\|<tr\\|<input\\|<cfoutput\\|<h1>\\|<h2>\\|<h3>\\|<h4>\\|<h5>\\|<h6>\
\|<center>\\|<frameset\\|<cfquery\\|<cfif\\|<cfelse\\|<cfloop\\|<cftransacti
on>")
(defvar cfml-helper-any-list-end

"</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</select>\\|</head>\\|</title>\\
|</cfinsert>\\|</table>\\|</tr>\\|</cfoutput>\\|</h1>\\|</h2>\\|</h3>\\|</h4
>\\|</h5>\\|</h6>\\|</center>\\|</frameset\\|</cfquery>\\|</cfif>\\|<\cfif>\
\|</cfloop>\\|</cftransaction>")
; "</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</select>\\|</table>\\|</tr>")
(defvar cfml-helper-any-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
   cfml-helper-any-list-start
   cfml-helper-any-list-end
   cfml-helper-any-list-item-start
   cfml-helper-any-list-item-end))
;; Wed Jan 10 09:50:53 2001 Saint
;;
;; New indentation. As for other modes leave a single indentation
;; sensible tag on each line.
(defvar cfml-helper-indentation-list cfml-helper-any-list)
  ;;  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
  ;;   cfml-helper-any-list-start
  ;;   cfml-helper-any-list-end
  ;;   cfml-helper-any-list-item-start))
(defvar cfml-helper-search-limit 2000 "limit on how far back we search")

(defun cfml-helper-context-symbol ()
  "Return the symbol the last match (against `cfml-helper-any-list') found."
  (cond ((match-beginning 1) 'list-start)
 ((match-beginning 2) 'list-end)
 ((match-beginning 3) 'item-start)
 ((match-beginning 4) 'item-end)
 (t 'error)))

; Wed Jan 10 09:53:46 2001 Saint
;
; Doesn't ignore item-end any more.
(defun cfml-helper-guess-prev-context ()
  "Figure out the last list-type tag before point relevant to indentation.
Returns 'item-start if the last list tag is a list item start
        'list-start if the last list tag is the start of a list
        'item-end   if the last list tag is the end of a list item
        'list-end   if the last list tag is the end of a list."
  (save-excursion
    (let* ((lim (max (point-min) (- (point) cfml-helper-search-limit)))
    (context (if (re-search-backward cfml-helper-indentation-list lim t)
   (cfml-helper-context-symbol)
        nil)))
      (cons context (current-indentation)))))

(defun cfml-helper-print-prev-context ()
  (interactive)
  (message "%s" (cfml-helper-guess-prev-context)))

;;}}}

;;{{{ indentation
(defvar cfml-helper-buffers nil "buffers using cfml-helper-mode alist")

(defvar cfml-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun cfml-helper-indent-command ()
  "Command for indenting HTML to the appropriate column.
Calls `cfml-helper-indent' which tries to examine how many levels down
in nested lists we are and does the appropriate indentation.'
See also `cfml-helper-basic-offset' and `cfml-helper-never-indent'."
  (interactive)
  (cfml-helper-indent))

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

;; code to never indent <PRE></PRE> sections. Many thanks to
;; Stan Lanning <address@bogus.example.com>
(defun cfml-helper-indent-leave-alone-p ()
  (let* ((pre (save-excursion (search-backward "<pre>" nil t)))
  (endpre (save-excursion (search-backward "</pre>" pre t))))
    (and pre (null endpre))))

(defadvice cfml-helper-indent (around leave-pre-alone activate)
  (cond ((not (cfml-helper-indent-leave-alone-p))
  ad-do-it)
 (cfml-helper-print-indent-info
  (message "In <pre> -- skipping indentation"))
 (t nil)))

(defun cfml-helper-indent ()
  "Indentation workhorse function."
  (if cfml-helper-never-indent
      ()
    (let ((m (point-marker))
   (bol (progn (beginning-of-line) (point))))

      ;; unindent the line
      (delete-region (point) (progn (back-to-indentation) (point)))

      (let* ((where (cfml-helper-guess-prev-context))
      (prev-context (car where))
      (this-context nil)
      (previ (cdr where))
      (newi (cond
      ((eq prev-context 'list-end) previ)
      ((eq prev-context 'item-start) previ)
      ((eq prev-context 'list-start) (+ previ cfml-helper-basic-offset))
      (t previ))))

 ;; newi is set to the basic indentation, now adjust indentation
 ;; based on what the current line is.
 (if (looking-at cfml-helper-any-list)
     (progn
       (setq this-context (cfml-helper-context-symbol))
       (cond
        ;; item start or end and last line was a list-end: go backwards
        ((and
   (or (eq this-context 'item-start) (eq this-context 'item-end))
   (eq prev-context 'list-end))
  (setq newi (- newi cfml-helper-basic-offset)))

        ;; end of list and last line was an end: go backwards twice
        ((and (eq this-context 'list-end) (eq prev-context 'list-end))
  ;; Wed Jan 10 09:35:53 2001 Saint
  ;;
  ;; fixing indentation
  ;;  (setq newi (- newi cfml-helper-basic-offset
cfml-helper-item-continue-indent)))
  (setq newi (- newi cfml-helper-basic-offset)))

        ;; Any other end of list? Indent negative
        ((and (eq this-context 'list-end))
  (setq newi (- newi cfml-helper-basic-offset)))

        ;; start of list and last line beginning of item, go forwards
        ((and (eq this-context 'list-start)
       (eq prev-context 'item-start))
  (setq newi (+ newi cfml-helper-basic-offset)))))

   ;; default: no special case, indent forward for text
   (cond
    ;; last line an item? Beginning of continued item - go forward
    ((eq prev-context 'item-start)
     (setq newi (+ newi cfml-helper-basic-offset)))))

 (if cfml-helper-print-indent-info
     (message
      "Last Context: %s, This Context: %s, Previous: %s New: %s"
      prev-context this-context previ newi))

 ;; just in case
 (if (< newi 0)
     (setq newi 0))
 (indent-to newi newi)

 ;; adjust point to where it was before, or at start of indentation
 (goto-char (marker-position m))
 (if (< (current-column) (current-indentation))
     (back-to-indentation))))))

;;}}}

;;{{{ completion finder for tempo

(defvar cfml-helper-completion-finder
  (if cfml-helper-emacs18
      'cfml-helper-emacs18-completion-finder
    "\\(\\(<\\|&\\).*\\)\\=")
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags HTML might complete.
;; emacs18 doesn't have the \= for regexps, though, so we do something
;; more hackish.

(defun cfml-helper-emacs18-completion-finder ()
  "Unfortunately emacs18 doesn't support \\= in regexps, so we do this hack.
If you have problems with it, maybe you should upgrade to emacs19 :-)"
  (let* ((where nil)
         (s (buffer-substring
             (point)
             (setq where (save-excursion
                           (re-search-backward "<\\|&" (min (point-min) 100)
t)
                           (point))))))
    (cons s where)))

;;}}}

;;{{{ timestamps

(defun cfml-helper-update-timestamp ()
  "Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`cfml-helper-timestamp-start', deletes all text up to
`cfml-helper-timestamp-end', and runs `cfml-helper-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward cfml-helper-timestamp-start nil t))
 (message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length cfml-helper-timestamp-start)))
     (ts-end (if (search-forward cfml-helper-timestamp-end nil t)
   (- (point) (length cfml-helper-timestamp-end))
        nil)))
 (if (not ts-end)
     (message "timestamp delimiter end was not found. Type C-c C-t to insert
one.")
   (delete-region ts-start ts-end)
   (goto-char ts-start)
   (run-hooks 'cfml-helper-timestamp-hook)))))
  nil)

(defun cfml-helper-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (current-time-string)))
    (insert "Last modified: "
     (substring time 0 20)
     (nth 1 (current-time-zone))
     " "
     (substring time -4)
     " ")))

(defun cfml-helper-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert cfml-helper-timestamp-start)
  (insert cfml-helper-timestamp-end))

;;}}}

;;{{{ cfml-helper-insert-new-buffer-strings

(tempo-define-template "html-skeleton" cfml-helper-new-buffer-template
         nil
         "Insert a skeleton for a HTML document")

(tempo-define-template "ASP-skeleton" cfml-helper-new-ASP-buffer-template
         nil
         "Insert a skeleton for a ASP document")

(tempo-define-template "PHP-skeleton" cfml-helper-new-PHP-buffer-template
         nil
         "Insert a skeleton for a PHP document")

(tempo-define-template "JSP-skeleton" cfml-helper-new-JSP-buffer-template
         nil
         "Insert a skeleton for a JSP document")

(defun cfml-helper-insert-new-buffer-strings ()
  "Insert `cfml-helper-new-buffer-strings'."
  (tempo-template-html-skeleton))

(defun cfml-helper-insert-new-ASP-buffer-strings ()
  "Insert `cfml-helper-new-ASP-buffer-strings' or
`cfml-helper-new-JSP-buffer-string'."
  (cond (cfml-helper-mode-local-JSP-not-ASP
  (tempo-template-JSP-skeleton))
 (t
  (tempo-template-ASP-skeleton))))

(defun cfml-helper-insert-new-PHP-buffer-strings ()
  "Insert `cfml-helper-new-PHP-buffer-strings'."
  (tempo-template-PHP-skeleton))

;;}}}

;;{{{ cfml-helper-mode

(defun base-cfml-helper-mode (mode)
  "basic mode, called by the exported modes with MODE telling what
is the mode to run (that's the skeleton to insert in empty files)"
  (kill-all-local-variables)

  (use-local-map cfml-helper-mode-map)
  (setq local-abbrev-table cfml-helper-mode-abbrev-table)
  (set-syntax-table cfml-helper-mode-syntax-table)

  (cond (cfml-helper-mode-run-the-mode
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'cfml-helper-count)
  (make-local-variable 'cfml-helper-mode-local-JSP-not-ASP)
  (make-variable-buffer-local 'cfml-helper-mode-run-the-mode)
  (set 'cfml-helper-mode-run-the-mode nil)
  (cfml-helper-add-buffer (current-buffer) mode)))

  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <address@bogus.example.com>.  (Last update: 05-Sep-1995).
  (cond ((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
  (put major-mode 'font-lock-keywords-case-fold-search t)
  )
 ;; XEmacs (19.13, at least) guesses the rest correctly.
 ;; If any older XEmacsen don't, then tell me.
 ;;
 ((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(cfml-helper-font-lock-keywords t t)))
 ;;
 (t ; Emacs 19.28 and older
  (make-local-variable 'font-lock-keywords-case-fold-search)
  (make-local-variable 'font-lock-keywords)
  (make-local-variable 'font-lock-no-comments)
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-keywords cfml-helper-font-lock-keywords)
  (setq font-lock-no-comments t)))

  (setq comment-start "<!--- "
 comment-end " --->"
 comment-start-skip "<!--[ \t]*"
 comment-column 0
 indent-line-function 'cfml-helper-indent)

  (tempo-use-tag-list 'cfml-helper-tempo-tags cfml-helper-completion-finder)

  (if cfml-helper-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'cfml-helper-update-timestamp))

  (if (and cfml-helper-build-new-buffer (zerop (buffer-size)))
      (cond ((string= mode "HTML")
      (cfml-helper-insert-new-buffer-strings))
     ((string= mode "ASP")
      (cfml-helper-insert-new-ASP-buffer-strings))
     ((string= mode "PHP")
      (cfml-helper-insert-new-PHP-buffer-strings))
     ))

  (easy-menu-add (cfml-helper-menu) cfml-helper-mode-map)

  (run-hooks 'text-mode-hook)
  (run-hooks 'html-mode-hook)
;; qui i keybinding
  (run-hooks 'cfml-helper-mode-hook))

(cond (cfml-helper-mode-uses-visual-basic
       (defun asp-cfml-helper-mode ()
  "Mode for editing HTML documents with ASP server scripts.

The main function cfml-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode'  for ASP e VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates
Supports server (actually ASP & PHP, JSP) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`cfml-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode
and activates ASP and VBScript support functions
`cfml-helper-mode-uses-bold-italic' : non nil creates a bold italic face
(could fail if there's not such face available)
`cfml-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP
code blocks beginning in the first colum
`cfml-helper-mode-global-JSP-not-ASP' : non nil to make Emacs consider <% %>
sequence as JSP blocks by default in cfml-helper-mode, set to nil in
asp-cfml-helper-mode, set to t in jsp-cfml-helper-mode.
Alter the behaviour locally by changing cfml-helper-mode-local-JSP-not-ASP
value


Special command (not in menu - default bound to [f4]): attempts a smart
narrowing to the current scripting block. Fails in client script containing
server script sections.
\\{cfml-helper-mode-map}
Written by address@bogus.example.com, http://www.santafe.edu/~nelson/
Mantained by address@bogus.example.com, http:/www.gest.unipd.it/~saint/"
  (interactive)
  (cfml-helper-add-buffer (current-buffer) "ASP")
  (asp-cfml-helper-mode-run)))
)

;;
(defun asp-cfml-helper-mode-run ()
  (base-cfml-helper-mode "ASP")
  (setq cfml-helper-mode-local-JSP-not-ASP nil)
  (setq mode-name "HTML/ASP helper")
  ;; Mon Jun 25 16:14:44 2001 Saint
  ;;
  ;; Supporto imenu (da cui discende il supporto Speedbar)
  (setq imenu-generic-expression
 '(( "VBA Variables" "\\([Dd][Ii][Mm][ \t]+\\)\\([a-z0-9]+\\)" 2)
   ( "ASP Output" "^.*\\(<%=[ \t]*\\)\\([a-z0-9]+\\)" 2)
   ( "ASP Block" "^.*\\(<%[^=][\t\n ]+\\)\\([a-z0-9]+\\)" 2)
   ( "VBA Sub" "^.*\\([sS]ub[\t\n ]+\\)\\([a-z0-9]+\\)" 2)
   ( "VBA Function" "^.*\\([fF]unciton[\t\n ]+\\)\\([a-z0-9]+\\)" 2)))
  (speedbar-add-supported-extension ".asp")
  ;;/Saint
  (setq major-mode 'asp-cfml-helper-mode))

(defun jsp-cfml-helper-mode ()
  "Mode for editing HTML documents with ASP server scripts.

The main function cfml-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode'  for ASP e VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates
Supports server (actually ASP & PHP, JSP) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`cfml-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode
and activates ASP and VBScript support functions
`cfml-helper-mode-uses-bold-italic' : non nil creates a bold italic face
(could fail if there's not such face available)
`cfml-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP
code blocks beginning in the first colum
`cfml-helper-mode-global-JSP-not-ASP' : non nil to make Emacs consider <% %>
sequence as JSP blocks by default in cfml-helper-mode, set to nil in
asp-cfml-helper-mode, set to t in jsp-cfml-helper-mode.
Alter the behaviour locally by changing cfml-helper-mode-local-JSP-not-ASP
value


Special command (not in menu - default bound to [f4]): attempts a smart
narrowing to the current scripting block. Fails in client script containing
server script sections.
\\{cfml-helper-mode-map}
Written by address@bogus.example.com, http://www.santafe.edu/~nelson/
Mantained by address@bogus.example.com, http:/www.gest.unipd.it/~saint/"
  (interactive)
  (cfml-helper-add-buffer (current-buffer) "JSP")
  (jsp-cfml-helper-mode-run)
)

(defun jsp-cfml-helper-mode-run ()
  (interactive)
  (base-cfml-helper-mode "JSP")
  (setq cfml-helper-mode-local-JSP-not-ASP t)
  (setq mode-name "HTML/JSP helper")
  (setq major-mode 'jsp-cfml-helper-mode))

(defun cfml-helper-mode ()
  "Mode for editing HTML documents.

The main function cfml-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' (optional - see below ) for ASP and VBScript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates

Supports server (actually ASP & PHP, JSP) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`cfml-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode
and activates ASP and VBScript support functions
`cfml-helper-mode-uses-bold-italic' : non nil creates a bold italic face
(could fail if there's not such face available)
`cfml-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP
code blocks beginning in the first colum
`cfml-helper-mode-global-JSP-not-ASP' : non nil to make Emacs consider <% %>
sequence as JSP blocks by default in cfml-helper-mode, set to nil in
asp-cfml-helper-mode, set to t in jsp-cfml-helper-mode.
Alter the behaviour locally by changing cfml-helper-mode-local-JSP-not-ASP
value

Special command (not in menu - default bound to [f4]): attempts a smart
narrowing to the current scripting block. Fails in client script containing
server script sections.
\\{cfml-helper-mode-map}
Written by address@bogus.example.com, http://www.santafe.edu/~nelson/
Mantained by address@bogus.example.com, http:/www.gest.unipd.it/~saint/
"
  (interactive)
  (cfml-helper-add-buffer (current-buffer) "HTML")
  (cfml-helper-mode-run))

(defun cfml-helper-mode-run ()
  (interactive)
  (base-cfml-helper-mode "HTML")
  (setq mode-name "HTML helper")
  (setq cfml-helper-mode-local-JSP-not-ASP
cfml-helper-mode-global-JSP-not-ASP)
  (setq major-mode 'cfml-helper-mode))

(defun php-cfml-helper-mode ()
  "Mode for editing HTML documents with PHP server scripts.

The main function cfml-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' (optional) for ASP e VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates

Supports server (actually ASP & PHP, JSP) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`cfml-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode
and activates ASP and VBScript support functions
`cfml-helper-mode-uses-bold-italic' : non nil creates a bold italic face
(could fail if there's not such face available)
`cfml-helper-mode-uses-KG-style' : nil to make Emacs consider PHP/JSP/ASP
code blocks beginning in the first colum
`cfml-helper-mode-global-JSP-not-ASP' : non nil to make Emacs consider <% %>
sequence as JSP blocks by default in cfml-helper-mode, set to nil in
asp-cfml-helper-mode, set to t in jsp-cfml-helper-mode.
Alter the behaviour locally by changing cfml-helper-mode-local-JSP-not-ASP
value

Special command (not in menu - default bound to [f4]): attempts a smart
narrowing to the current scripting block. Fails in client script containing
server script sections.
\\{cfml-helper-mode-map}
Written by address@bogus.example.com, http://www.santafe.edu/~nelson/
Mantained by address@bogus.example.com, http:/www.gest.unipd.it/~saint/
"
(interactive)
  (cfml-helper-add-buffer (current-buffer) "HTML")
  (php-cfml-helper-mode-run))

(defun php-cfml-helper-mode-run ()
  (interactive)
  (base-cfml-helper-mode "PHP")
  (setq mode-name "HTML/PHP helper")
  (setq major-mode 'php-cfml-helper-mode))

;;}}}

;;{{{ text faces

;; By Ulrik Dickow <address@bogus.example.com>.
;;
;; Originally aimed at Emacs 19.29.  Later on disabled syntactic fontification
;; and reordered regexps completely, to be compatible with XEmacs (it doesn't
;; understand OVERRIDE=`keep').
;;
;; We make an effort on handling nested tags intelligently.

(make-face 'info-menu-6)

;; font-lock compatibility with XEmacs/Lucid and older Emacsen (<19.29).
;;
(if (string-match "XEmacs\\|Lucid" (emacs-version))
    ;; XEmacs/Lucid
    ;; Make needed faces if the user hasn't already done so.
    ;; Respect X resources (`make-face' uses them when they exist).
    (let ((change-it
     (function (lambda (face)
          (or (if (fboundp 'facep)
           (facep face)
         (memq face (face-list)))
       (make-face face))
          (not (face-differs-from-default-p face))))))
      (if (funcall change-it 'cfml-helper-bold-face)
    (progn (make-face 'cfml-helper-bold-face)
    (make-face-bold 'cfml-helper-bold-face)
   (set-face-foreground cfml-helper-bold-face "peru")))
      (if (funcall change-it 'cfml-helper-italic-face)
    (progn (make-face 'cfml-helper-italic-face)
    (make-face-italic 'cfml-helper-italic-face)
   (set-face-foreground 'cfml-helper-italic-face "medium sea green")))
      (if (funcall change-it 'cfml-helper-underline-face)
    (set-face-underline-p 'cfml-helper-underline-face t))
      (if (funcall change-it 'font-lock-variable-name-face)
    (set-face-foreground 'font-lock-variable-name-face "salmon"))
      (if (funcall change-it 'font-lock-reference-face)
    (set-face-foreground 'font-lock-reference-face "violet"))
      ; experimental code
      (if (funcall change-it 'cfml-helper-bold-italic-face)
    (progn (cond (cfml-helper-mode-uses-bold-italic
   (make-face 'cfml-helper-bold-italic-face)
   (make-face-bold-italic 'cfml-helper-bold-italic-face)
   (set-face-foreground 'cfml-helper-bold-italic-face "orange")))))
      (if (funcall change-it 'cfml-helper-underline-face)
   (progn (make-face 'cfml-helper-underline-face)
   (set-face-underline-p 'cfml-helper-underline-face t)
   (set-face-foreground cfml-helper-underline-face "goldenrod")))
      (if (funcall change-it 'html-tag-face)
   (progn (make-face 'html-tag-face)
   (make-face-bold 'html-tag-face)
   (set-face-foreground html-tag-face "dodger blue")))
 ;; PETER Neergaard <address@bogus.example.com> says
 ;;
 ;; "Another issue I just noticed is that font-lock-builtin-face
 ;; is not a face defined Xemacs; instead they use
 ;; font-lock-preprocessor-face (I too fail to see any good
 ;; reasons that they have made this design choice).  I did not
 ;; notice this at first because I hack define
 ;; font-lock-builtin-face in my .emacs as I had some packages
 ;; using font-lock-builtin-face when I started using Xemacs,
 ;; but this has been changed now."  Then suggests this change :
      (make-face 'cfml-helper-builtin-face)
      (copy-face 'font-lock-preprocessor-face
   'cfml-helper-builtin-face)))
  ;; Emacs
  ;;
  ;; Note that Emacs evaluates the face entries in `font-lock-keywords',
  ;; while XEmacs doesn't.  So XEmacs doesn't use the following *variables*,
  ;; but instead the faces with the same names as the variables.

  ;; New predicate on suggestion by "David J. Biesack" <address@bogus.example.com>
  (if (or (not (boundp 'emacs-major-version)) ; t if prior to 19.23
    (< emacs-major-version 20)         ; t if prior to 20.0.0
    (and (= emacs-major-version 20)    ; t if prior to 20.4.1
  (< emacs-minor-version 4)))
       (progn
 (defvar cfml-helper-bold-face
   (make-face 'cfml-helper-bold-face))
 (make-face-bold 'cfml-helper-bold-face)
 (defvar cfml-helper-italic-face
   (make-face 'cfml-helper-italic-face))
 (make-face-italic 'cfml-helper-italic-face)
 (cond (cfml-helper-mode-uses-bold-italic
        (defvar cfml-helper-bold-italic-face
   (make-face 'cfml-helper-bold-italic-face))
        (make-face-bold-italic 'cfml-helper-bold-italic-face)))
 (defvar cfml-helper-underline-face
   (make-face 'cfml-helper-underline-face))
 (set-face-underline-p 'cfml-helper-underline-face t)
 (defvar html-tag-face
   (make-face 'html-tag-face))
 (make-face-bold 'html-tag-face)

 (defvar cfml-helper-builtin-face
   (make-face 'cfml-helper-builtin-face))
 (copy-face 'font-lock-builtin-face
     'cfml-helper-builtin-face)
 ;; Support for both old font-lock-background-mode and new
 ;; frame-background-mode, plus a default value if neither of the two
 ;; is non nil
 (let ((internal-background-mode
        (or (if (boundp 'font-lock-background-mode)
                  font-lock-background-mode frame-background-mode)
              (setq internal-background-mode 'light))))
   (progn
     (cond ((eq internal-background-mode 'light)
     (set-face-foreground html-tag-face "dodger blue"))
    ((eq internal-background-mode 'dark)
     (set-face-foreground html-tag-face "deep sky blue")))
     (cond ((eq internal-background-mode 'light)
     (set-face-foreground cfml-helper-bold-face "peru"))
    ((eq internal-background-mode 'dark)
     (set-face-foreground 'cfml-helper-bold-face "wheat")))
     (cond ((eq internal-background-mode 'light)
     (set-face-foreground 'cfml-helper-italic-face "medium sea green"))
    ((eq internal-background-mode 'dark)
     (set-face-foreground 'cfml-helper-italic-face "spring green")))
     (cond (cfml-helper-mode-uses-bold-italic
     (cond ((eq internal-background-mode 'light)
     (set-face-foreground 'cfml-helper-bold-italic-face "orange"))
    ((eq internal-background-mode 'dark)
     (set-face-foreground 'cfml-helper-bold-italic-face "peachpuff")))))
     (cond ((eq internal-background-mode 'light)
     (set-face-foreground cfml-helper-underline-face "goldenrod"))
    ((eq internal-background-mode 'dark)
     (set-face-foreground 'cfml-helper-underline-face "cornsilk"))))))
    ;; Use customization. I don't recall if earier version support it...
    (progn
      (defvar html-tag-face
 (defface html-tag-face
   '((((class color)
       (background dark))
      (:foreground "deep sky blue" :bold t))
     (((class color)
       (background light))
      (:foreground "dodger blue" :bold t))
     (t
      (:foreground "dodger blue" :bold t)))
   "Face to use for HTML tags."
   :group 'cfml-helper-faces))
      (defvar cfml-helper-bold-face
 (defface cfml-helper-bold-face
   '((((class color)
       (background dark))
      (:foreground "wheat" :bold t))
     (((class color)
       (background light))
      (:foreground "peru" :bold t))
     (t
      (:foreground "peru" :bold t)))
   "Custom bold face."
   :group 'cfml-helper-faces))
   (defvar cfml-helper-italic-face
     (defface cfml-helper-italic-face
       '((((class color)
    (background dark))
   (:foreground "spring green" :italic t))
  (((class color)
    (background light))
   (:foreground "medium sea green" :italic t))
  (t
   (:foreground "medium sea green" :italic t)))
       "Custom italic face."
       :group 'cfml-helper-faces))
   (cond (cfml-helper-mode-uses-bold-italic
   (defvar cfml-helper-bold-italic-face
     (defface cfml-helper-bold-italic-face
       '((((class color)
    (background dark))
   (:foreground "peachpuff" :bold t:italic t))
         (((class color)
    (background light))
   (:foreground "orange" :bold t :italic t))
         (t
   (:foreground "orange" :bold t :italic t)))
       "Custom bold italic face."
       :group 'cfml-helper-faces))))
   (defvar cfml-helper-underline-face
     (defface cfml-helper-underline-face
       '((((class color)
    (background dark))
   (:foreground "cornsilk" :underline t))
  (((class color)
    (background light))
   (:foreground "goldenrod" :underline t))
  (t
   (:foreground "goldenrod" :underline t)))
       "Custom underline face."
       :group 'cfml-helper-faces))
   (defvar cfml-helper-builtin-face
     (defface cfml-helper-builtin-face
       '((((class color)
    (background dark))
   (:foreground "light goldenrod" :underline nil))
  (((class color)
    (background light))
   (:foreground "dark goldenrod" :underline nil))
  (t
   (:foreground "light goldenrod" :underline nil)))
       "Custom Server Script face."
       :group 'cfml-helper-faces))))

  ;;
  (if (string-lessp "19.28.89" emacs-version)
      ()
    ;; Emacs 19.28 and older
    ;; Define face variables that don't exist until Emacs 19.29.
    (defvar cfml-helper-builtin-face (cfml-helper-emacs-19-build-face)
    (defvar font-lock-variable-name-face 'font-lock-doc-string-face
      "Face to use for variable names -- and some HTML keywords.")
    (defvar font-lock-reference-face 'underline ; Ugly at line breaks
      "Face to use for references -- including HTML hyperlink texts.")))

(defun cfml-helper-emacs-19-build-face ()
  (let ((x (make-face 'font-lock-builtin-face)))
    (set-face-foreground x "Orchid")
    x))


; (defvar cfml-helper-builtin-face
;   (let ((bux (make-face 'cfml-helper-builtin-face)))
;     (copy-face
;      ;; XEmacs doesn't have font-lock-builtin-face
;      (cond ((string-match "XEmacs\\|Lucid" emacs-version)
;      font-lock-preprocessor-face)
;     ;; GNU Emacs 19 doesn't have it either
;     ((string-match "GNU Emacs 19" emacs-version)
;      cfml-helper-emacs-19-build-face)
;     ;; Emacs
;     (t font-lock-builtin-face)) bux  )
;     bux))

(copy-face
 (cond ((string-match "XEmacs\\|Lucid" emacs-version)
 font-lock-preprocessor-face)
       ;; GNU Emacs 19 doesn't have it either
       ((string-match "GNU Emacs 19" emacs-version)
 cfml-helper-emacs-19-build-face)
       ;; Emacs
       (t font-lock-builtin-face)) cfml-helper-builtin-face)

;; Tue Jan 09 12:29:45 2001 Saint
;;
;; Time to KISS syntax highlight.
;;
;; All tags get the same highlight with attributes highlighted
; (defvar cfml-helper-font-lock-keywords
;   (let (;; Titles and H1's, like function defs.
;  ;;   We allow for HTML 3.0 attributes, like `<h1 align=center>'.
;  ;; All tokens get the same higlighting
;  (tword "\\(h1\\|title\\)\\([ \t\n]+[^>]+\\)?")
;  ;; Names of tags to boldify.
;  (bword "\\(b\\|h[2-4]\\|strong\\)\\([ \t\n]+[^>]+\\)?")
;  ;; Names of tags to italify.
;  (iword "\\(address\\|cite\\|em\\|i\\|var\\)\\([ \t\n]+[^>]+\\)?")
;  ;; Regexp to match shortest sequence that surely isn't a bold end.
;  ;; We simplify a bit by extending "</strong>" to "</str.*".
;  ;; Do similarly for non-italic and non-title ends.
;  (not-bend (concat "\\([^<%?]\\|<\\([^/]\\|/\\([^bhs]\\|"
;      "b[^>]\\|"
;      "h\\([^2-4]\\|[2-4][^>]\\)\\|"
;      "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
;  (not-iend (concat "\\([^<%?]\\|<\\([^/]\\|/\\([^aceiv]\\|"
;      "a\\([^d]\\|d[^d]\\)\\|"
;      "c\\([^i]\\|i[^t]\\)\\|"
;      "e\\([^m]\\|m[^>]\\)\\|"
;      "i[^>]\\|"
;      "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
;  (not-tend (concat "\\([^<%?]\\|<\\([^/]\\|/\\([^ht]\\|"
;      "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
;     (list
;      ;; Avoid use of `keep', since XEmacs will treat it the same as `t'.
;      ;; First fontify the text of a HREF anchor.  It may be overridden later.
;      ;; Anchors in headings will be made bold, for instance
;      '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
;        1 font-lock-warning-face t)
;      ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
;      (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
;     0 'font-lock-function-name-face t)
;      ;; Underline is rarely used. Only handle it when no tags inside.
;      '("<u>\\([^<]*\\)</u>" 1 cfml-helper-underline-face t)
;      ;; Forms, anchors & images (also fontify strings inside)
;      '("<\\(i\\(mg\\|nput\\)\|a\\)\\>[^>\n]*>"
;        0  font-lock-constant-face t)
;      ;; Any tag, general rule, just after bold/italic stuff.
;      ;; w3 org says that a tag is <element-name> not < element-name>
;      '("\\(<[^%a=> \t][^>]*>\\)" 1 font-lock-function-name-face t)
;      '("\\(<[^%a=> \t][^>\n]*>\\)" 1 html-tag-face t)
;      ;; Large-scale structure keywords (like "program" in Fortran).
;      ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
;      '("\\(</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)[^>\n]*>\\)"
;        0 font-lock-variable-name-face t)
;      ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
;      '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
;        0 font-lock-keyword-face t)
;      ;; Paint [PA][HS]P skripts in font-lock-builtin-face,
;      '("<[?%]=\\([^%?]\\|[?%][^>]\\)*[%?]>" 0 cfml-helper-builtin-face t t)
;      '(cfml-helper-match-asp-php 0 cfml-helper-builtin-face t t)
;      ;; This one is to pick
;      ;; Tag pairs like <b>...</b> etc.
;      ;; Cunning repeated fontification to handle common cases of overlap.
;      ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
;      (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
;     0 'cfml-helper-bold-face t)
;      ;; Italic complex --- possibly with arbitrary non-italic kept inside.
;      (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
;     0 'cfml-helper-italic-face t)
;      ;; Bold simple --- first fontify bold regions with no tags inside.
;      (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
;     0 'cfml-helper-bold-face t)
;      ;; string stuff is pretty weird with asp. You can have strings
;      ;; containing asp code containing strings and empty
;      ;; strings. Replaced original [^\"] with this one...
;      '("[=(&]?[ \t\n]*\\(\"[^\"\n]*<%[^\"\n]*\\(\"[^\"\n]*\"\\)[^\"\n]*%>[^\"\n]*\\)" 1 font-lock-string-face t)
;      '("[=(&]?[ \t\n]*\\(\"[^\"\n]*\"\\)"  1 font-lock-string-face t)
;      ;; HTML special characters
;      '("&[a-zA-Z0-9#]+;" 0 font-lock-warning-face t)
;      ; after painting strings, you have to restore asp stuff inside strings
;      '("\\(<%=\\w\\)" 1 cfml-helper-builtin-face t)
;      '("\\(\")[^\"\n]*%>\\)" 1 cfml-helper-builtin-face t)
;      '("\\(<%=[^%]*%>\\)" 1 cfml-helper-builtin-face t)
;      '("\\(<\\?=\\w\\)" 1 cfml-helper-builtin-face t)
;      '("\\(\")[^\"\n]*\\?>\\)" 1 cfml-helper-builtin-face t)
;      '("\\(<\\?=[^%]*\\?>\\)" 1 cfml-helper-builtin-face t)
;      ;; That's krazy, strings higlight matches ) too, so i paint
;      ;; parantheses...
;      '("\\(<%\\|\\s(\\)" 1 font-lock-function-name-face t)
;      '("\\(\\s)\\|%>\\)" 1 font-lock-function-name-face t)
;      '("\\(<\\?\\|\\s(\\)" 1 font-lock-function-name-face t)
;      '("\\(\\s)\\|\\?>\\)" 1 font-lock-function-name-face t)
;      '("\\([\"]\\)" 0 font-lock-string-face t)
;      ;; Comment declarations according to the HTML 2.0 spec at
;      ;; <URL:http://www.w3.org/pub/WWW/MarkUp/html-spec/html-spec_3.html>.
;      ;; Usually `<!-- ... -->', but also e.g the single, complete declaration
;      ;; `<!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >'.
;      ;; Note that e.g. Netscape 3.01 Gold doesn't fully live up to the spec.

;      ;; A Regexp doesn't work well with big blocks...
;      ;;      '("<!--\\(.\\|[\n]\\--[ \t]*[^>]\\)*--[ \t]*>" 0
;      ;; font-lock-comment-face t)))
;      '(cfml-helper-match-comments 0 font-lock-comment-face t t)
;      '(cfml-helper-match-attributes 0 font-lock-variable-face t t)))
;     "Additional expressions to highlight in HTML helper mode.")

;; New highlighting
(defvar cfml-helper-font-lock-keywords
    (list
     ;; Avoid use of `keep', since XEmacs will treat it the same as `t'.
     ;; First fontify the text of a HREF anchor.  It may be overridden later.
     ;; Anchors in headings will be made bold, for instance
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
       1 font-lock-warning-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 cfml-helper-underline-face t)
     '(cfml-helper-match-bold
    0 'cfml-helper-bold-face t)
     ;; Italic
     '(cfml-helper-match-italics
    0 'cfml-helper-italic-face t)
     ;; w3 org says that a tag is <element-name> not < element-name>
     ;; I don't know of any non alphabetic HTML entity, if you know
     ;; about one, please drop me a mail
     ;;      Saint
     '("\\(</?[A-Za-z0-9]+\\)" 1 html-tag-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
       0 font-lock-keyword-face t)
     ;; Paint [PA][HS]P skripts in font-lock-builtin-face,
     '("<[?%]=\\([^%?]\\|[?%][^>]\\)*[%?]>" 0 cfml-helper-builtin-face t t)
     '(cfml-helper-match-asp-php 0 cfml-helper-builtin-face t t)
     ;; string stuff is pretty weird with asp. You can have strings
     ;; containing asp code containing strings and empty
     ;; strings. Replaced original [^\"] with this one...
     '("[=(&]?[
\t\n]*\\(\"[^\"\n]*<%[^\"\n]*\\(\"[^\"\n]*\"\\)[^\"\n]*%>[^\"\n]*\\)" 1
font-lock-string-face t)
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*\"\\)"  1 font-lock-string-face t)
     ;; after painting strings, you have to restore asp stuff inside strings
     '("\\(<%=\\w\\)" 1 cfml-helper-builtin-face t)
     '("\\(\")[^\"\n]*%>\\)" 1 cfml-helper-builtin-face t)
     '("\\(<%=[^%]*%>\\)" 1 cfml-helper-builtin-face t)
     '("\\(<\\?=\\w\\)" 1 cfml-helper-builtin-face t)
     '("\\(\")[^\"\n]*\\?>\\)" 1 cfml-helper-builtin-face t)
     '("\\(<\\?=[^%]*\\?>\\)" 1 cfml-helper-builtin-face t)
     ;; Comment declarations according to the HTML 2.0 spec at
     ;; <URL:http://www.w3.org/pub/WWW/MarkUp/html-spec/html-spec_3.html>.
     ;; Usually `<!-- ... -->', but also e.g the single, complete declaration
     ;; `<!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >'.
     ;; Note that e.g. Netscape 3.01 Gold doesn't fully live up to the spec.

     ;; That's krazy, strings higlight matches ) too, so i paint
     ;; parantheses...
     '("\\(<%\\|\\s(\\)" 1 font-lock-function-name-face t)
     '("\\(\\s)\\|%>\\)" 1 font-lock-function-name-face t)
     '("\\(<\\?\\|\\s(\\)" 1 font-lock-function-name-face t)
     '("\\(\\s)\\|\\?>\\)" 1 font-lock-function-name-face t)
     '("\\([\"]\\)" 0 font-lock-string-face t)
     ;; A Regexp doesn't work well with big blocks...
     ;;      '("<!--\\(.\\|[\n]\\--[ \t]*[^>]\\)*--[ \t]*>" 0
     ;; font-lock-comment-face t)))
     '(cfml-helper-match-comments 0 font-lock-comment-face t t)
     '(cfml-helper-match-attributes 0 font-lock-variable-name-face t t)
     ;; HTML special characters
     '("&[a-zA-Z0-9#]+;" 0 font-lock-warning-face t))
    "Additional expressions to highlight in HTML helper mode.")

;; internal variables

(defvar cfml-helper-count 0 "Counter during server script matching")

(defvar cfml-helper-verbose t
  "Non nil to show a counter during server script matching")

(defun cfml-helper-ticker ()
  "Returns the next prop image"
  (set 'cfml-helper-count (mod (incf cfml-helper-count) 8))
  (make-string cfml-helper-count 46))

;; Function to match an asp script (hopefully) without overflowing the
;; regexp stack (not inline <%= ... %>)
;;
;;
(defun cfml-helper-match-asp-php (last)
  (cond (cfml-helper-verbose
  (message "Fontifying %s... (PHP/ASP %s)" bufname
    (cfml-helper-ticker))))
  (cond ((search-forward-regexp "[^\"]<[?%][^=]" last t) ; match inline
elsewhere
  (backward-char 1)
  (let ((beg (point)))
;    (cond ((search-forward-regexp
"\\([^%?\n]\\|[%?][^>]\\|\\(\"\\([%?]>\\|.\\)*\"\\)\\)*[?%]>" last t)
    (cond ((search-forward-regexp "\\([^\%\?\n\"]\\|[%?][^>]\\)*[?%]>" last
t)
    (set-match-data (list beg (point)))
    t )
   (t nil))))
 (t nil)))

;; Html comments can overflow the buffer if used to hide the code
;; from older browser
(defun  cfml-helper-match-comments (last)
  "Matches comments in HTML from point to LAST"
  (cond (cfml-helper-verbose
  (message "Fontifying %s... (Comments..%s)" bufname
    (cfml-helper-ticker))))
  (cond ((search-forward "<!--" last t) ; match inline elsewhere
  (backward-char 4)
  (let ((beg (point)))
    (cond ((search-forward-regexp "--[ \t]*>" last t)
    (set-match-data (list beg (point)))
    t )
   (t nil))))
 (t nil)))

;; match html tag attributes
(defun cfml-helper-match-attributes (last)
  "Matches tag attributes in HTML from point to LAST"
  (cond (cfml-helper-verbose
  (message "Fontifying %s... (Attributes %s)" bufname
    (cfml-helper-ticker))))
  (cond ((search-forward-regexp "[A-Za-z]+=" last t)
  (let ((endolo (point))
        (beg (progn (backward-word 1)(point)))
        (firstop (cond ((search-backward-regexp
    "<\\(@\\|[A-Za-z]+\\)" 1 t)
          (point))
         (t nil)))
        (firstclo (cond ((search-forward ">" last t)
          (point))
         (t nil))))
    (cond (firstop
    (cond (firstclo
    (cond ((and (< firstop beg)
         (> firstclo endolo))
    (goto-char endolo)
    (set-match-data (list beg endolo))
    t )
          (t
    (goto-char endolo)
    (set-match-data nil)
    t)))
   (t (goto-char endolo)
      (set-match-data (list beg endolo))
      t)))
    (t
     (goto-char endolo)
     (set-match-data nil)
     t))))
 (t nil)))

(defun cfml-helper-match-bold (last)
  (cfml-helper-match-ib
   "\\(<\\(b\\|h[1-4]\\|strong\\title\\)\\(>\\|\\W\[^>]*>\\)\\)"
   "</\\(b\\|h[1-4]\\|strong\\title\\)" last))

(defun cfml-helper-match-italics (last)
  (cfml-helper-match-ib "\\(<\\(i\\|em\\)\\(>\\|\\W\[^>]*>\\)\\)"
   "</\\(i\\|em\\)"
   last))

(defun cfml-helper-match-ib (bmat emat last)
  "Matches text between BMAT and EMAT from point to LAST"
  (cond (cfml-helper-verbose
  (message "Fontifying %s... (Bold/Italics %s)" bufname
    (cfml-helper-ticker))))
  (cond ((search-forward-regexp bmat last t)
  (let ((beg (point))
        (endolo (cond ((search-forward-regexp emat last t)
         (backward-word 1)
         (backward-char 2)
         (point))
        (t nil))))
    (cond (endolo
    (set-match-data (list beg endolo))))
    t))
 (t nil)))

(defun cfml-helper-fontify-region (beg end verbose)
  (setq cfml-helper-count 0)
  (setq cfml-helper-count2 0)
  (let ((loudly (and verbose
       (> (- end beg) (/ (buffer-size) 2)))))
    (setq cfml-helper-verbose loudly)
    (font-lock-default-fontify-region beg end loudly)))

(set (make-local-variable font-lock-fontify-region-function)
     'cfml-helper-fontify-region)

(defun cfml-helper-fontify-buffer ()
  (setq cfml-helper-count 0)
  (setq cfml-helper-count2 0)
  (setq cfml-helper-verbose (if (numberp font-lock-verbose)
    (> (buffer-size) font-lock-verbose)
         font-lock-verbose))
  (font-lock-default-fontify-buffer))

(set (make-local-variable font-lock-fontify-buffer-function)
     'cfml-helper-fontify-buffer)

;;}}} faces

;;{{{ patterns for hilit19

;; Define some useful highlighting patterns for the hilit19 package.
;; These will activate only if hilit19 has already been loaded.
;; Thanks to <address@bogus.example.com> for some pattern suggestions

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     'cfml-helper-mode
     '(("<!--" "-->" comment)
       ("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>" nil comment) ;<!DOCTYPE
...>
       ("<title>" "</title>" defun)
       ("<h[1-6]>" "</h[1-6]>" bold) ;only colour inside tag
       ("<a\\b" ">" define)
       ("</a>" nil define)
       ("<img\\b" ">" include)
       ("<option\\|</?select\\|<input\\|</?form\\|</?textarea" ">" include)
       ;; First <i> highlighting just handles unnested tags, then do nesting
       ("<i>[^<]*</i>" nil italic)
       ("<b>" "</b>" bold)
       ("<i>" "</i>" italic)
       ("<u>" "</u>" underline)
       ("&[^;\n]*;" nil string)
       ;; w3 org says that a tag is <element-name> not < element-name>
       ("<[^ \t]" ">" keyword))
     nil 'case-insensitive)
  nil)
;;}}}

;;{{{ indentation

(defvar cfml-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

;;}}}

;;{{{ Script Narrowing and mode switch
;; These are for supporting html-script. With small changes can be useful for
;; javascript

;; Stan Lanning <address@bogus.example.com> wrote these defadvice to preserve
;; cursor position. Thank you very much Stan!
(defadvice html-script-narrow-to-asp (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-php (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-vbscript (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-javascript (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-release-region (around save-excursion activate)
  (save-excursion
    ad-do-it))

(defun html-script-narrow-to-asp ()
  "Narrows to an JSP/ASP script and switches to either java-mode/JDE or
visual-basic-mode.
Does nothing if both cfml-helper-mode-uses-visual-basic and
cfml-helper-mode-local-JSP-not-ASP are nil"
  (interactive)
  (cond ((html-script-choose-mode)
  (html-script-search-start-tag)
  (let ((beg (point)))
    (html-script-search-end-tag)
    (narrow-to-region beg (point)))
  (html-script-choose-server-mode)
  (goto-char 0))))


(defun html-script-search-start-tag ()
 (cond ( cfml-helper-mode-uses-KG-style
   (search-backward-regexp "^<%") )
        (t (search-backward "<%" ))))

(defun html-script-search-end-tag ()
  (cond ( cfml-helper-mode-uses-KG-style
   (search-forward-regexp "^%>" ))
 ( t (search-forward "%>" nil t))))

(defun html-script-choose-mode ()
  (or cfml-helper-mode-uses-visual-basic
      cfml-helper-mode-local-JSP-not-ASP))

(defun html-script-choose-server-mode ()
  (cond (cfml-helper-mode-local-JSP-not-ASP
  (cond (cfml-helper-mode-uses-JDE (cfml-helper-enters-jde-mode))
        (t (java-mode))))
 (t
  (visual-basic-mode))))

(defun html-script-narrow-to-php ()
  "Narrows to an ASP script and setups c-mode"
  (interactive)
  (search-backward "<?")
  (let ((beg (point)))
    (search-forward "?>" nil t)
    (narrow-to-region beg (point)))
  (c-mode)
  (goto-char 0))

(defun html-script-narrow-to-vbscript ()
  "Narrows to a VB Script script and setups visual basic mode. Does nothing
if cfml-helper-mode-uses-visual-basic is nil"
  (interactive)
  (cond (cfml-helper-mode-uses-visual-basic
  (search-backward-regexp "<SCRIPT[
\t]+\\(LANGUAGE=\"VBScript\"\\|TYPE=\"text/vbscript\"\\)[ \t]*>")
  (let ((beg (point)))
    (search-forward "</SCRIPT>" nil t)
    (narrow-to-region beg (point)))
  (visual-basic-mode)
  (goto-char 0))))

(defun html-script-narrow-to-javascript ()
  "Narrows to a JavaScript script and setups java mode"
  (interactive)
  (search-backward-regexp "<SCRIPT[
\t]+\\(LANGUAGE=\"JavaScript\"\\|TYPE=\"text/javascript\"\\)[ \t]*>")
  (let ((beg (point)))
    (search-forward "</SCRIPT>" nil t)
    (narrow-to-region beg (point)))
  (cond (cfml-helper-mode-uses-JDE (jde-mode))
 (t java-mode))
  (goto-char 0))

(defun cfml-helper-add-buffer (buffer tag)
  (cond ((and cfml-helper-buffers
       (cfml-helper-buffer-listed))
  (set 'cfml-helper-buffers
       (cons (cons buffer tag) cfml-helper-buffers )))
 (t (set 'cfml-helper-buffers (list (cons buffer tag))))))

;; Fri Aug 03 18:12:14 2001 Saint
;;
;; This function checks if the current buffer is not in
;; html-herlper-buffer list
(defun cfml-helper-buffer-listed ()
  (let ((retval t))
    (mapcar (lambda (a)
     (cond ((eq (current-buffer) (car a))
     (set 'retval nil))))
     cfml-helper-buffers)
    retval))
;;/Saint

(defun cfml-helper-remove-buffer ()
  (let ((nl nil))
    (while cfml-helper-buffers
      (cond ((not (eq (current-buffer)
       (car (car cfml-helper-buffers))))
      (set 'nl (cons (car cfml-helper-buffers) nl))
      (set 'cfml-helper-buffers (cdr cfml-helper-buffers)))
     (t (set 'cfml-helper-buffers (cdr cfml-helper-buffers)))))
    (set 'cfml-helper-buffers nl)))

(defun html-script-release-region ()
   "widens the window to the complete buffer and runs cfml-helper-mode. MUST
be interactive."
  (interactive)
  (mapcar (lambda (a)
     (cond ((eq (current-buffer) (car a))
     (cfml-helper-seletct-appropriate-mode (cdr a)))))
   cfml-helper-buffers))

(defun cfml-helper-seletct-appropriate-mode( cfml-helper-used-mode)
   (interactive)
   (goto-char 0)
   (widen)
   (cond ((string= "HTML" cfml-helper-used-mode)
   (cfml-helper-mode-run))
  ((string= "ASP" cfml-helper-used-mode)
   (asp-cfml-helper-mode-run))
  ((string= "JSP" cfml-helper-used-mode)
   (jsp-cfml-helper-mode-run))
  ((string= "PHP" cfml-helper-used-mode)
   (php-cfml-helper-mode-run))))

(defun html-script-release-setup()
  (interactive)
  (local-set-key html-script-toggle-key 'html-script-release-region))

(cond (cfml-helper-mode-uses-visual-basic
      (cond
       (visual-basic-mode-hook
 (add-hook 'visual-basic-mode-hook 'html-script-release-setup))
       (t (setq visual-basic-mode-hook 'html-script-release-setup)))))

(cond
 (c-mode-hook
  (add-hook 'c-mode-hook 'html-script-release-setup))
 (t (setq c-mode-hook 'html-script-release-setup)))

;; Very Very ALPHA!!!
;;
;; Adding html-script-release-setup to jde-entering-java-buffer-hooks
;;
(cond (cfml-helper-mode-uses-JDE
       (if (and cfml-helper-mode-uses-JDE (fboundp 'jde-mode))
       (add-hook 'jde-mode-hook 'html-script-release-setup)))
       (t
 (cond
  (java-mode-hook
   (add-hook 'java-mode-hook
      'html-script-release-setup))
  (t (setq java-mode-hook
    'html-script-release-setup))))
       )

(defun cfml-helper-enters-jde-mode()
  (interactive)
  (and cfml-helper-mode-uses-JDE (fboundp 'jde-mode))
   (add-hook 'jde-mode-hook 'html-script-release-setup)
  (jde-mode))

;; Still from Stan Lanning here it comes the code for a "smart switch" to
;; the appropriate scripting mode.

(defvar html-script-toggle-key [f4])

(defvar html-script-narrow-alist
  `((,(regexp-quote "<%") . html-script-narrow-to-asp)
    (,(regexp-quote "<?") . html-script-narrow-to-php)
    ("<SCRIPT[ \t]+LANGUAGE=\"VBScript\"[ \t]*>" .
html-script-narrow-to-vbscript)
    ("<SCRIPT[ \t]+TYPE=\"text/vbscript\"[ \t]*>" .
html-script-narrow-to-vbscript)
    ("<SCRIPT[ \t]+LANGUAGE=\"JavaScript\"[ \t]*>" .
html-script-narrow-to-javascript)
    ("<SCRIPT[ \t]+TYPE=\"text/javascript\"[ \t]*>" .
html-script-narrow-to-javascript)))

(defvar html-script-start-regexp
  (concat "\\(" (mapconcat (lambda (x) (car x)) html-script-narrow-alist
"\\|") "\\)"))

(defun html-script-toggle-narrow ()
  (interactive)
  (let ((handler-fn (save-excursion
        (if (re-search-backward html-script-start-regexp nil t)
     (catch 'handler-found
       (mapcar (lambda (p)
          (if (looking-at (car p))
       (throw 'handler-found (cdr p))))
        html-script-narrow-alist)
       nil)))))
    (if handler-fn
 (apply handler-fn nil)
      (error "No script tag found"))))

(defun html-script-install-toggle-key ()
  (local-set-key html-script-toggle-key 'html-script-toggle-narrow))

(add-hook 'cfml-helper-mode-hook 'html-script-install-toggle-key)

(defadvice html-script-release-setup (after key-binding activate)
  (local-set-key html-script-toggle-key 'html-script-release-region))
;;}}}

;; folding tags: End of code tree
;;}}}

;;{{{ Epilogue

(provide 'cfml-helper-mode)
;;; (provide 'php-cfml-helper-mode)
;;; (provide 'asp-cfml-helper-mode)
;;; (provide 'jsp-cfml-helper-mode)
;;; (provide 'html-mode)   ;for 14 character filename
(cond ((boundp 'kill-buffer-hook)
       (add-hook 'kill-buffer-hook 'cfml-helper-remove-buffer))
      (t (set 'kill-buffer-hook 'cfml-helper-remove-buffer)))
(run-hooks 'html-load-hook)
(run-hooks 'cfml-helper-load-hook)

;;}}}

;;; cfml-helper-mode.el ends here



(cfml-helper-add-type-to-alist
 '(cf . (cf-html-map "\C-c\C-c" cf-html-menu
       "Insert Cold Fusion Elements")))
(cfml-helper-install-type 'cf)

(cfml-helper-add-tag
 '(cf "u" "<cfupdate>" "cf update" ("<cfupdate datasource = \"" (p
         "Datasource: ") "\" tablename = \"" (p "Table name: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "t" "<cftable>" "cf table" ("<cftable query = \"" (p "Table query: ")
      "\">" (p "Table definition: ") "\n</cftable>\n")))

(cfml-helper-add-tag
 '(cf "s" "<cfset>" "cf set" ("<cfset " (p "Equation: ") ">\n")))

(cfml-helper-add-tag
 '(cf "r" "<cfreport>" "cf report" ("<cfreport report = \"" (p "Report
path: ") "\">" (p "Body: ") "\n</cfreport>\n")))

(cfml-helper-add-tag
 '(cf "q" "<cfquery>" "cf query" ("<cfquery name = \"" (p "Query name: ")
      "\" datasource = \"" (p "Datasource: ") "\">" (p "Body: ")
      "\n</cfquery>\n")))

(cfml-helper-add-tag
 '(cf "p" "<cfparam>" "cf param" ("<cfparam name = \"" (p "Parameter name:
") "\"default = \"" (p "Default value: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "o" "<cfoutput>" "cf output" ("<cfoutput>" (p "Text: ")
        "\n</cfoutput>\n")))

(cfml-helper-add-tag
 '(cf "m" "<cfmail>" "cf mail" ("<cfmail to = \"" (p "To: ") "\" from = \""
    (p "From: ") "\" subject = \"" (p "Subject: ") "\">" (p "Body: ")
    "\n</cfmail>\n")))

(cfml-helper-add-tag
 '(cf "Q" "<cfloop>" "cf loop, query" ("<cfloop query = \"" (p "Query to
index: ") ">" (p "Body: ") "\n</cfloop>\n")))

(cfml-helper-add-tag
 '(cf "L" "<cfloop>" "cf loop, list" ("<cfloop index = \"" (p "List index:
") ">" (p "Body: ") "\n</cfloop>\n")))

(cfml-helper-add-tag
 '(cf "I" "<cfloop>" "cf loop, index" ("<cfloop index = \"" (p "Loop index:
") "\"from = \"" (p "From: ") "\"to = \"" (p "To: ") "\">" (p "Body: ")
"\n</cfloop>\n")))

(cfml-helper-add-tag
 '(cf "C" "<cfloop>" "cf loop, conditional" ("<cfloop conditional = \"" (p
          "Expression: ") ">" (p "Body: ") "\n</cfloop>\n")))

(cfml-helper-add-tag
 '(cf "w" "<cflocation>" "cf location" ("<cflocation url = \"" (p "URL: ")
     "\">\n")))

(cfml-helper-add-tag
 '(cf "d" "<cfinsert>" "cf insert" ("<cfinsert datasource = \"" (p
         "Datasource: ") "\" tablename = \"" (p "Database table name: ")
"\">\n")))

(cfml-helper-add-tag
 '(cf "n" "<cfinclude>" "cf include" ("<cfinclude template = \"" (p
          "Template filename: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "i" "<cfif>" "cf if" ("<cfif " (p "Expression: ") ">" (p "Body: ")
       "\n</cfif>\n")))

(cfml-helper-add-tag
 '(cf "h" "<cfheader>" "cf header" ("<cfheader name = \"" (p "Header name:
") "\" value = "\" (p "Header value: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "f" "<cffile>" "cf file" ("<cffile action = \"" (p "File action: ")
    "\">\n")))

(cfml-helper-add-tag
 '(cf "e" "<cferror>" "cf error" ("<cferror type = \"" (p "Error type: ")
      "\" template = \"" (p "Error template file: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "k" "<cfcookie>" "cf cookie" ("<cfcookie name = \"" (p ": ") "\"name
= \"" (p "Cookie name: ") "\" value = \"" (p "Cookie value: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "k" "<cfcontent>" "cf content" ("<cfcontent type = \"" (p "Content
type: ") "\" deletefile = \"" (p "Delete File (yes/no): ") "\" file = \""
(p "Filename: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "c" "<cfcol>" "cf col" ("<cfcol header = \"" (p "Column Header: "\")
         " width = " (p "Column width: ") " align = " (p "Column Alignment:
") "
text = " (p "Text: "\") "\">\n")))

(cfml-helper-add-tag
 '(cf "a" "<cfbreak>" "cf abort" ("<cfbreak>\n")))

(cfml-helper-add-tag
 '(cf "t" "<cfapplication>" "cf application" ("<cfapplication name = \"" (p
           "Application name: ") "\" ClientManagement = \"" (p "Store client
variables?: ") "\">\n")))

(cfml-helper-add-tag
 '(cf "a" "<cfabort>" "cf abort" ("<cfabort>\n")))

(cfml-helper-rebuild-menu)
