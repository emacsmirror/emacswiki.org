;;; linkd.el --- Make hypertext with active links in any buffer
;;
;; Filename: linkd.el
;; Description: Make hypertext with active links in any buffer
;; Author: David O'Toole <dto@gnu.org>
;;   Additional code by Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Shaun Johnson <shaun@slugfest.demon.co.uk>
;; Copyright (C) 2007, David O'Toole.
;; Copyright (C) 2008-2009, Drew Adams.
;; Copyright (C) 2009, Shaun Johnson.
;; Created: Fri Mar 14 07:56:32 2008 (Pacific Daylight Time)
;; Version: $Id: linkd.el,v 1.64 2008/03/14 $
;; Last-Updated: Sun Mar  7 11:48:30 2010 (-0800)
;;           By: dradams
;;     Update #: 629
;; Package-Version: 0.9
;; Website, original version: http://dto.github.com/notebook/linkd.html
;; URL: http://www.emacswiki.org/cgi-bin/wiki/linkd.el
;; URL: http://www.emacswiki.org/emacs/linkd.tar.gz
;; Keywords: hypermedia help 
;; Compatibility: GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `easymenu'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Make hypertext with active links in any buffer
;;
;;
;;(@* "Overview") ----------------------------------------------------
;;
;;  Linkd-mode is a major mode that automatically recognizes and
;;  processes certain S-expressions, called "links", embedded in plain
;;  text files.  Links may be followed by invoking certain interactive
;;  functions when point is on the link text.  Links may also be
;;  interpreted as marking up the surrounding text.  Different types
;;  of links have different behaviors when followed, and they may have
;;  different interpretations as markup.
;;
;;  With Linkd mode, you can do the following:
;;  * Embed hyperlinks to files, webpages, or documentation into
;;    any type of text file in any major mode.
;;  * Delimit and name regions of text ("blocks") in these text files.
;;    See (@> "Stars")
;;  * Extract and send blocks to other programs for processing.
;;    See (@> "Processing blocks")
;;  * Identify and mark locations and concepts in source code.
;;    See (@> "Tags")
;;  * Embed active data objects ("datablocks") into text files.
;;    See (@> "Datablocks")
;;  * Convert Lisp source-code listings to LaTeX for publication.
;;    See (@> "Exporting to LaTeX")
;;  * Define new link behaviors.
;;
;;  For detailed information about using linkd-mode, see the online
;;  manual: http://dto.github.com/notebook/linkd.html.
;;
;;
;;(@* "TODO") --------------------------------------------------------
;;
;;  * Should have a proper history of link navigation, like in Info,
;;    for forward and backward link following, instead of just saving
;;    the previous location.
;;
;;  * Should have a link follow behavior that takes you from @> to the
;;    corresponding @*, not just to the next link (@* or @>).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2010/03/07 dadams
;;     linkd-render-link:
;;       Don't render unless the (@...) is really a function call.  Thx to eeeickythump.
;; 2010/02/28 dadams
;;     linkd-match: Incorporated bug fix from Emacs Wiki by eeeickythump: Ensure sexp is symbol.
;;     Incorporated addition of autoloads by Daniel Hackney (from Emacs Wiki 2010-02-06).
;; 2009/03/12 sjohnson
;;     Updated embedded URLs.
;; 2009/02/17 sjohnson
;;     Removed test for linkd-mode from menu - un-needed.
;; 2009/02/16 dadams
;;     linkd-html-export: Do nothing if htmlize.el is not available.
;;     Show Linkd menu only in Linkd mode.
;;     linkd-enable-linkd-mode-in-target: Added :tags
;;     linkd-use-menu: Changed default value to t.
;; 2009/02/15 sjohnson
;;     Added: linkd-use-menu, linkd-enable-linkd-mode-in-target, linkd-maybe-enable-in-target,
;;            linkd-menu.
;;     Restored require of easymenu - used now.
;; 2009/02/10 dadams
;;     Renamed: linkd-insertion-schemes to linkd-type-keywords-alist,
;;              linkd-export-formats    to linkd-export-formats-alist.
;;     Changed defvars to defcustoms: linkd-use-icons, linkd-icons-directory,
;;       linkd-generic-regexp, linkd-type-keywords-alist, linkd-default-bullet-string,
;;       linkd-star-search-string, linkd-block-file-name, linkd-shell-buffer-name,
;;       linkd-export-heading-regexp, linkd-export-commentary-regexp, linkd-export-link-regexp,
;;       linkd-export-formats-alist, linkd-file-handler-alist, linkd-wiki-extensions,
;;       linkd-wiki-directory.
;;     linkd-file-handler-alist:
;;       Default value no longer nil - now covers .el files, find-library, finder-commentary.
;;     @file: Treat :to also for the handler case (since handler just opens the file).
;;            Turn on Linkd mode for the target file.
;;     Removed: (require 'easymenu) - doesn't seem to be used.
;; 2008/04/18 dadams
;;     linkd-overlay:
;;       Put keymap property back on the overlay (for RET etc.).  Thx to Shaun Johnson.
;; 2008/04/16 dadams
;;     linkd-overlay: Add keymap property of linkd-overlay-map to the display property.
;;                    Remove keymap property from the overlay itself.
;;     linkd-map: Removed linkd-follow-mouse binding to mouse-2.
;; 2008/03/21 dadams
;;     linkd-back: Reset linkd-previous-point.
;;     linkd-map: Bind mouse-2 here also, as workaround for Emacs bug.  Remove when bug fixed.
;; 2008/03/14 dadams
;;     linkd-follow-mouse: Go to the buffer of clicked window.
;;     linkd(-overlay)-map: Bound linkd-follow-mouse to mouse-2 and linkd-back to mouse-4.
;;     linkd-(enable|disable):
;;       Ensure add/remove text props doesn't count as buffer modification.
;;     linkd-overlay: Added mouse-face to links.
;;     Renamed faces, to remove -face suffix and be more specific.
;;     Removed all face variables - just use faces.
;;     Changed face default definitions, to be less gaudy.  Still needs work (dark/light bg).
;;     linkd-send-block-to-shell: goto-char point-max instead of end-of-buffer.
;;     Changed require cl to eval-when-compile require.
;;     linkd-activate-datablock: Added missing right paren.  Removed extra one elsewhere.
;;     linkd-use-datablocks: defvar, not defun (!).
;;     Collected defvars together and gave them doc strings.
;;     Added doc strings, cleaned up doc strings (still some missing or unclear).
;;     Use header2.el header.
;;     Code cleanup (cosmetic).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; This file is not part of GNU Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; block, case
(require 'easymenu) ;; easy-menu-define

;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (@* "Faces") ------------------------------------------------------

(defgroup linkd nil
  "Hypertext links."
  :prefix "linkd-"
  :group 'convenience :group 'help
  :link '(url-link :tag "Download" "http://www.emacswiki.org/cgi-bin/wiki/linkd.el")
  :link '(url-link :tag "Download (with icons)" " http://www.emacswiki.org/emacs/linkd.tar.gz")
  :link '(emacs-commentary-link :tag "Doc" "linkd"))

(defface linkd-generic-link '((t (:foreground "blue")))
  "Face for linkd links." :group 'linkd :group 'faces)

(defface linkd-generic-link-name '((t (:foreground "blue")))
  "Face for linkd links." :group 'linkd :group 'faces)

(defface linkd-star `((t (:foreground ,(frame-parameter nil 'background-color))))
  "Face for star delimiters." :group 'linkd :group 'faces)

(defface linkd-star-name '((t (:foreground "blue" :background "Pink")))
  "Face for star names." :group 'linkd :group 'faces)

(defface linkd-tag `((t (:foreground ,(frame-parameter nil 'background-color))))
  "Face for tags." :group 'linkd :group 'faces)

(defface linkd-tag-name '((t (:foreground "blue" :underline t)))
  "Face for tag names." :group 'linkd :group 'faces)

(defface linkd-icon '((t (:underline nil)))
  "Face for icons." :group 'linkd :group 'faces)

(defface linkd-wiki '((t (:foreground "FireBrick" :underline t)))
  "Face for camel-case wiki links." :group 'linkd :group 'faces)

(defface linkd-command '((t (:foreground "red" :underline t)))
  "Face for command links." :group 'linkd :group 'faces)


;; (@* "User Options") -----------------------------------------------

(defcustom linkd-use-icons nil
  "Non-nil means icons, instead of text bullets, are displayed for links."
  :type 'boolean :group 'linkd)

(defcustom linkd-icons-directory "~/.linkd-icons" "Directory where linkd's icons are kept."
  :type 'directory :group 'linkd)

(defcustom linkd-use-menu t
  "Non-nil means show the Linkd menu in the menu bar."
  :type 'boolean :group 'linkd)

(defcustom linkd-enable-linkd-mode-in-target t
  "Whether to turn on Linkd mode for the target of a @file link.
* t - turn linkd mode on unconditionally.

* nil - don't turn linkd mode on.

* A list of major mode symbols, Turn on linkd mode if the target
  buffer's mode is in this list.

* A function to be called in the context of the target buffer.
  Turn on linkd mode if it returns a non-nil value."
  :type '(choice
          (const :tag "Turn on Linkd mode unconditionally" t)
          (const :tag "Do not turn on Linkd mode"          nil)
          (repeat :tag "Modes to use Linkd"
           (symbol :tag "Major mode for which to turn on Linkd mode"))
          (function :tag "Turn on Linkd mode if this function returns non-nil"))
  :group 'linkd)

(defcustom linkd-generic-regexp (concat "\(" "@" "[^)]*\)")
  "Regexp to find links."
  :type 'regexp :group 'linkd)

(defcustom linkd-type-keywords-alist '(("file" :file-name :to :display)
                                       ("man"  :page :to :display)
                                       ("info" :file-name :node :to :display)
                                       ("url"  :file-name :display))
  "Alist of possible link types and their associated Linkd keywords.
Each key is a link type name.
Each value is a list of Linkd keywords to use for that type (key)."
  :type '(alist
          :key-type   (string :tag "Link type")
          :value-type (repeat (symbol :tag "Linkd keywords for this type")))
  :group 'linkd)

(defcustom linkd-default-bullet-string "."
  "Default string to use to display a bullet."
  :type 'string :group 'linkd)

(defcustom linkd-star-search-string (concat "\(" "\@\*")
  "Regexp that matches a Linkd star."
  :type 'string :group 'linkd)

(defcustom linkd-block-file-name "~/.linkd-block"
  "File where temporary block text is stored for external processing."
  :type 'file :group 'linkd)

(defcustom linkd-shell-buffer-name "*Linkd Shell*"
  "Name of shell buffer used by Linkd."
  :type 'string :group 'linkd)

;; Used for export to LaTeX and HTML.
(defcustom linkd-export-heading-regexp (concat "(" "@\\* \"\\([^\"]*\\)\")")
  "Regexp to match section headings in the buffer."
  :type 'regexp :group 'linkd)

;; Used for export to LaTeX and HTML.
(defcustom linkd-export-commentary-regexp "^;;"
  "Regexp to match commentary lines in a buffer."
  :type 'string :group 'linkd)

;; Used for export to LaTeX and HTML.
;; Of course no regexp can correctly recognize matched parentheses.
;; But our links are always on a single line, so we can sort of make it work.
(defcustom linkd-export-link-regexp (concat "(" "@" ".*)$")
  "Regexp to match Linkd links."
  :type 'string :group 'linkd)

;; Used for export to LaTeX and HTML.
(defcustom linkd-export-formats-alist '(("html" . linkd-html-export)
                                        ("tex" . linkd-latex-export))
  "Alist of file extensions and associated export formats, for Linkd."
  :type '(alist
          :key-type   (string :tag "File-name extension")
          :value-type (symbol :tag "Export function"))
  :group 'linkd)

(defcustom linkd-file-handler-alist
  '(("el" . (lambda (file-name)
              (let ((curr-mode  major-mode))
                (condition-case nil
                    (if (eq curr-mode 'finder-mode)
                        (condition-case nil
                            (finder-commentary file-name)
                          (error (find-library file-name)))
                      (find-library file-name))
                  (error (find-file file-name)))))))
  "Alist that maps file extensions to functions that open files.
Each such function should accept a file name as its argument."
  :type '(alist
          :key-type   (string :tag "File extension (no period)")
          :value-type (symbol :tag "Handler function for such files"))
  :group 'linkd)

(defcustom linkd-wiki-extensions '("linkd" "org" "el")
  "List of file-name extensions to try, to look for a given wiki page."
  :type '(repeat string) :group 'linkd)

(defcustom linkd-wiki-directory "~/linkd-wiki"
  "Default directory to look for wiki pages in."
  :type 'directory :group 'linkd)


;; (@* "Internal Variables") -----------------------------------------

(defvar linkd-previous-buffer nil "Last buffer being shown.")

(defvar linkd-previous-point nil "Value of point before link following.")

;; We may attach keybindings to an overlay, so that the keybindings
;; are in effect whenever point is within the overlay.  For rapid
;; navigation, we will eventually attach some quick single-character
;; commands to the links, using the following keymap:
(defvar linkd-overlay-map nil "Keymap for Linkd overlays.")
(unless linkd-overlay-map
  (setq linkd-overlay-map (make-sparse-keymap))
  (define-key linkd-overlay-map (kbd "RET") 'linkd-follow-at-point)
  ;; $$$$(define-key linkd-overlay-map [down-mouse-2] 'ignore)
  (define-key linkd-overlay-map [mouse-2] 'linkd-follow-mouse)
  (define-key linkd-overlay-map [mouse-4] 'linkd-back)
  (define-key linkd-overlay-map (kbd "b") 'linkd-back)
  (define-key linkd-overlay-map (kbd "l") 'linkd-back)
  (define-key linkd-overlay-map (kbd "[") 'linkd-previous-link)
  (define-key linkd-overlay-map (kbd "]") 'linkd-next-link))

(defvar linkd-process-block-function nil
  "Function called by `linkd-process-block'.
Argument is the contents of the block around point as a string.
You can set this in the `Local Variables' section of a file.")
(make-variable-buffer-local 'linkd-process-block-function)

(defvar linkd-use-datablocks nil "When non-nil, Linkd uses datablocks in the current buffer.")
(make-variable-buffer-local 'linkd-use-datablocks)

(defvar linkd-datablocks-activated nil "When non-nil, Linkd activates datablocks.")
(make-variable-buffer-local 'linkd-datablocks-activated)

;; Used for export to LaTeX.
(defvar linkd-latex-in-verbatim nil "Non-nil means we are inside a LaTeX verbatim section.")

(defvar linkd-map nil "Keymap used by Linkd mode.")
(when (null linkd-map)
  (setq linkd-map (make-sparse-keymap))
  (define-key linkd-map (kbd "C-c *")   'linkd-process-block)
  (define-key linkd-map (kbd "C-c [")   'linkd-previous-link)
  (define-key linkd-map (kbd "C-c ]")   'linkd-next-link)
  (define-key linkd-map (kbd "C-c '")   'linkd-follow-at-point)
  (define-key linkd-map [mouse-4]       'linkd-back)
  (define-key linkd-map (kbd "C-c , b") 'linkd-back)
  (define-key linkd-map (kbd "C-c , ,") 'linkd-insert-link)
  (define-key linkd-map (kbd "C-c , t") 'linkd-insert-tag)
  (define-key linkd-map (kbd "C-c , s") 'linkd-insert-star)
  (define-key linkd-map (kbd "C-c , w") 'linkd-insert-wiki)
  (define-key linkd-map (kbd "C-c , l") 'linkd-insert-lisp)
  (define-key linkd-map (kbd "C-c , e") 'linkd-edit-link-at-point)
  (define-key linkd-map (kbd "C-c , x") 'linkd-escape-datablock))

;; Linkd menu for menu bar.
(easy-menu-define linkd-menu linkd-map "Linkd"
  '("Linkd"
	:visible linkd-use-menu
	["Follow" linkd-follow-at-point :active (get-char-property (point) 'linkd)]
	["Back" linkd-back :active (get-char-property (point) 'linkd)]
	["Previous link" linkd-previous-link :active (get-char-property (point) 'linkd)]
	["Next link" linkd-next-link :active (get-char-property (point) 'linkd)]
	("Insert"
	 ["Tag" linkd-insert-tag]
	 ["Star" linkd-insert-star]
	 ["Link" linkd-insert-link])
	["Edit" 	linkd-edit-link-at-point :active (get-char-property (point) 'linkd)]))


;; (@* "Versioning") -------------------------------------------------

;;;###autoload
(defun linkd-version ()
  "Display Linkd version."
  (interactive)
  ;; (message "$Id: linkd.el,v 1.63 2007/05/19 00:16:17 dto Exp dto $"))
  (message "$Id: linkd.el,v 1.64 2008/03/14 $"))


;; (@* "Recognizing Links") ------------------------------------------
;;
;; In working with Emacs' font-lock code to obtain automatic
;; recognition of a construct, one typically uses a regular expression
;; to match the construct. But recall that we are looking to match
;; S-expressions, which cannot be matched by any regular
;; expression. To overcome this difficulty, we can supply font-lock
;; with a function to perform the search, instead of a regular
;; expression. If this function uses the system's built-in Lisp
;; reader, we can then match proper S-expressions.
;;
;; Below is a function that Emacs' font-locking can use to find and
;; highlight links. See (@> "Fontlocking") below.

(defun linkd-match (limit)
  "Try to read link sexp between point and LIMIT.
Return non-nil if a link is found.  Set match-data appropriately."
  (let ((sexp nil))
    (when (search-forward (concat "(" "@") limit t) (backward-char 2))
    (let ((begin-point (point)))
      (condition-case nil (setq sexp (read (current-buffer))) ((error nil)))
      (when (and (symbolp (car-safe sexp))
                 (string-match "@.*" (symbol-name (car-safe sexp))))
        (let ((begin-marker (make-marker))
              (end-marker (make-marker)))
          (set-marker begin-marker begin-point)
          (set-marker end-marker (point))
          (set-match-data (list begin-marker end-marker)))
        t))))

;; Function to extract link data from plain text.  It determines the
;; presence of a link by searching for the `linkd' text property,
;; instead of using the regular expression given above. This is
;; because of the way link rendering works.  When the activation of
;; Linkd mode triggers fontification of a buffer containing links, the
;; links are matched by the font-locking code, and marked with the
;; `linkd' text property.  All the other functions that deal with
;; links can then use the `linkd' text property, which is simpler than
;; using regexps throughout.  See (@> "Rendering links with overlays")
;; and (@> "Fontlocking").

(defun linkd-link-at-point ()
  "Return link around point as a sexp.  Return nil if no link found."
  (when (get-char-property (point) 'linkd)
    (save-excursion (read (current-buffer)))))


;; (@* "Following Links") --------------------------------------------
;;
;; Each link is an S-expression.  When this S-expression is evaluated,
;; the result is a property list whose keys represent possible user
;; actions, and whose values are functions to be invoked when the
;; corresponding key is chosen.  To follow a link, we evaluate the
;; link's S-expression and invoke the function corresponding to the
;; `:follow' property in the resulting property list.
;;
;; The results of following a link will often change the currently
;; displayed buffer, so we remember which is the current buffer before
;; switching, and provide a function, `linkd-back', to return to the
;; old buffer.

(defun linkd-follow (sexp)
  "Follow the link represented by SEXP."
  (let* ((plist (eval sexp))
         (follower (plist-get plist :follow)))
    (when follower
      ;; save current spot so that we can go back if needed
      (setq linkd-previous-buffer (current-buffer))
      (setq linkd-previous-point (point))
      (funcall follower))))

;;;###autoload
(defun linkd-back ()
  "Return to the buffer being viewed before the last link was followed."
  (interactive)
  (when linkd-previous-buffer
    (switch-to-buffer linkd-previous-buffer)
    (let ((start (point)))
      (goto-char linkd-previous-point)
      (setq linkd-previous-point start))))

;;;###autoload
(defun linkd-follow-at-point ()
  "Follow the link at point."
  (interactive)
  (linkd-follow (linkd-link-at-point)))

(defun linkd-follow-mouse (event)
  "Follow the clicked link."
  (interactive "e")
  (when event
    (select-window (posn-window (event-start event)))
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    ;;; $$$$ (beginning-of-line)
    (linkd-follow (linkd-link-at-point))))

(defun linkd-maybe-enable-in-target ()
  "Conditionally enable linkd mode in the target of an @file link."
  (when (or (and (booleanp linkd-enable-linkd-mode-in-target)
                 linkd-enable-linkd-mode-in-target)
            (and (functionp linkd-enable-linkd-mode-in-target)
                 (funcall linkd-enable-linkd-mode-in-target))
            (and (listp linkd-enable-linkd-mode-in-target)
                 (memq major-mode linkd-enable-linkd-mode-in-target)))
    (linkd-mode 1)))

;; (@* "Navigating Links") -------------------------------------------
;;
;; Instead of manually positioning point on each link, we can navigate
;; directly between links. The following interactive functions jump
;; from link to link.

;;;###autoload
(defun linkd-next-link ()
  "Move point to the next link, if any."
  (interactive)
  (forward-char 1)
  (let ((inhibit-point-motion-hooks nil))
    ;; get out of the current overlay if needed
    (when (get-char-property (point) 'linkd)
      (while (and (not (eobp)) (get-char-property (point) 'linkd))
        (goto-char (min (next-overlay-change (point))
                        (next-single-char-property-change (point) 'linkd)))))
    ;; now find the next linkd overlay
    (while (and (not (eobp)) (not (get-char-property (point) 'linkd)))
      (goto-char (min (next-overlay-change (point))
                      (next-single-char-property-change (point) 'linkd))))))

;;;###autoload
(defun linkd-previous-link ()
  "Move point to the previous link, if any."
  (interactive)
  (let ((inhibit-point-motion-hooks nil))
    ;; get out of the current overlay if needed
    (when (get-char-property (point) 'linkd)
      (while (and (not (bobp)) (get-char-property (point) 'linkd))
        (goto-char (max (previous-overlay-change (point))
                        (previous-single-char-property-change (point) 'linkd)))))
    ;; now find the previous linkd overlay
    (while (and (not (bobp)) (not (get-char-property (point) 'linkd)))
      (goto-char (max (previous-overlay-change (point))
                      (previous-single-char-property-change (point) 'linkd))))))


;; (@* "Inserting and Editing Links Interactively") ------------------
;;
;; It is not necessary to type the links manually. With these
;; functions, the user may create and edit links interactively.

;;;###autoload
(defun linkd-insert-single-arg-link (type-string argument)
  "Insert a link containing ARGUMENT."
  (insert (if (not (string= "" argument))
              (format (concat "(" "@%s %S)") type-string argument)
            (format (concat "(" "@%s)") type-string))))

;;;###autoload
(defun linkd-insert-tag (tag-name)
  "Insert a tag."
  (interactive "sTag name: ")
  (linkd-insert-single-arg-link ">" tag-name))

;;;###autoload
(defun linkd-insert-star (star-name)
  "Insert a star."
  (interactive "sStar name: ")
  (linkd-insert-single-arg-link "*" star-name))

;;;###autoload
(defun linkd-insert-wiki (wiki-name)
  "Insert a wiki link."
  (interactive "sWiki page: ")
  (linkd-insert-single-arg-link "!" wiki-name))

;;;###autoload
(defun linkd-insert-lisp (sexp)
  "Insert a Lisp sexp."
  (interactive "xLisp expression: ")
  (linkd-insert-single-arg-link "L" sexp))

;;;###autoload
(defun linkd-insert-link (&optional type current-values)
  "Insert a link.
Optional arg TYPE is the link type.
Optional arg CURRENT-VALUES is a property list of current values."
  (interactive)
  (let* ((type (or type (completing-read "Link type: " linkd-type-keywords-alist)))
         (keys (cdr (assoc type linkd-type-keywords-alist)))
         (key (car keys))
         (link-args nil))
    (while key
      ;; read an argument value
      (let ((value (read-from-minibuffer (format "%S " key) (plist-get current-values key))))
        (when (not (string= "" value)) (setq link-args (plist-put link-args key value))))
      ;; next
      (setq keys (cdr keys))
      (setq key (car keys)))
    ;; format and insert the link
    (insert (format (concat "(" "@%s %s)") type (mapconcat (lambda (sexp) (format "%S" sexp))
                                                           link-args
                                                           " ")))))

;;;###autoload
(defun linkd-edit-link-at-point ()
  "Edit the Linkd link at point."
  (interactive)
  (let ((link (linkd-link-at-point)))
    (when link
      (if (keywordp (car (cdr link)))
          (save-excursion       ; it's a general link. drop the @ sign
            (linkd-insert-link (substring (format "%S" (car link)) 1) (cdr link)))
        ;; it's a single-arg link
        (let ((new-value (read-from-minibuffer "New value: " (car (cdr link)))))
          (insert (format "%S" (list (car link) new-value)))))
      ;; now erase old link
      (re-search-backward linkd-generic-regexp)
      (delete-region (match-beginning 0) (match-end 0)))))


;; (@* "Rendering Links with Overlays") ------------------------------
;;
;; Emacs' overlays allow us to render a link onscreen in ways that make
;; the meaning of the link clearer. We can do this by hiding the somewhat
;; ugly link syntax, color-coding the text, and optionally by
;; displaying graphical icons to help in determining the type of link.
;;
;; This is one of the trickiest parts of linkd-mode, as the use of
;; overlays requires attention to detail in order for things to work
;; right.
;;
;; First some preliminary definitions.

(defun linkd-insert (string)
  "Insert STRING, removing its text properties."
  (insert (substring-no-properties string)))

;; The following utility function is our standard way of applying
;; linkd-style overlays to the text of a link.

(defun linkd-overlay (beg end display-text
                      &optional display-face bullet-text bullet-face bullet-icon)
  "Apply Linkd overlay to link text.
$$$$$ FIXME: document args."
  (let ((overlay (make-overlay beg end)))
    (overlay-put
     overlay 'display (propertize display-text
                                  'face (or display-face 'linkd-generic-link-name)
                                  'keymap linkd-overlay-map)) ; add speed-navigation keys
    (overlay-put overlay 'mouse-face 'highlight)
    (overlay-put overlay 'linkd t) ; mark overlay so that we can find it later
    (overlay-put overlay 'keymap linkd-overlay-map) ; add speed-navigation keys
    (when bullet-text                   ; add bullet, if appropriate
      (let* ((face (if (and bullet-icon linkd-use-icons) 'linkd-icon bullet-face))
             (b1 (if face (propertize bullet-text 'face face) bullet-text))
             (b2 (if (and bullet-icon linkd-use-icons)
                     (propertize b1 'display
                                 `(image :file ,bullet-icon :type xpm :ascent center))
                   b1)))
        (overlay-put overlay 'before-string (concat b2 " "))))
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks ; defontify if the user edits the text
                 (list (lambda (ov foo beg end &rest ignore)
                         (delete-overlay ov)
                         (remove-text-properties (point-at-bol) (point-at-eol)
                                                 (list 'fontified nil
                                                       'linkd-fontified nil
                                                       'linkd nil)))))))


;; (@* "Decorating Links with Graphical Icons") ----------------------
;;
;; I have drawn a set of 16x16 icons for use with linkd-mode. When the
;; icon feature is enabled, an appropriate icon is displayed to the
;; left of the link.
;;
;; The icons are included in the linkd download at:
;;   http://www.emacswiki.org/emacs/linkd.tar.gz

(defun linkd-icon (icon-name)
  "Returns the name of the icon file for ICON-NAME."
  (concat (file-name-as-directory linkd-icons-directory) "linkd-" icon-name ".xpm"))

(defun linkd-file-icon (file-name)
  "Choose an appropriate icon for FILE-NAME based on the name or extension.
Returns the file-name to the icon image file."
  (let* ((dir (file-name-as-directory linkd-icons-directory))
         (icon (concat dir "linkd-file-" (file-name-extension file-name) ".xpm")))
    (if (file-exists-p icon)
        icon
      (concat dir "linkd-file-generic.xpm"))))


;; (@* "Stars") ------------------------------------------------------
;;
;; Stars delimit (and optionally name) blocks of text. A block of text
;; is the region between one star and the next. We may think of blocks
;; as dividing a text file into sections.

(defun @* (&optional star-name)
  "$$$$$$$$$$$$ FIXME"
  `(:follow
    (lambda () (linkd-find-next-tag-or-star ,star-name))
    :render
    (lambda (beg end)
      (linkd-overlay
       beg end
       ,(if star-name star-name " ")    ; leave space so fontified link won't disappear
       ',(if star-name 'linkd-star-name 'default)
       "*" 'linkd-star ,(linkd-icon "star")))))


;; (@* "Tags") -------------------------------------------------------
;;
;; Tags can be used to navigate within source code.  You can mark
;; those parts of a program that relate to a given concept with a
;; `tag' link that names the concept.
;;
;; Following a `tag' link navigates to the next tag (or star) with the
;; same name, cycling to the beginning of the buffer when the end is
;; reached.  You can think of following tag links as tracing a concept
;; through different parts of a program by jumping between related
;; pieces of code.

(defun linkd-find-next-tag-or-star (name)
  "Find next Linkd tag or star."
  (let* ((regexp (concat "\(\@\\(\*\\|>\\) \"" name))
         (found-position
          (save-excursion
            (goto-char (point-at-eol))
            (if (re-search-forward regexp nil t)
                (match-beginning 0)
              (goto-char (point-min)) ; start over at the beginning of the buffer
              (when (re-search-forward regexp nil t) (match-beginning 0))))))
    (when found-position (goto-char found-position))))

(defun @> (tag-name)
  "$$$$$$$$ FIXME"
  `(:follow
    (lambda () (linkd-find-next-tag-or-star ,tag-name))
    :render
    (lambda (beg end) (linkd-overlay beg end ,tag-name 'linkd-tag-name
                                ">" 'linkd-tag ,(linkd-icon "tag")))))


;; (@* "Processing Blocks") ------------------------------------------
;;
;; You can divide a text file into sections using stars, and then
;; selectively process certain of those blocks of text, perhaps with
;; an external program.  You can use this facility to experiment with
;; such external programs or to develop interactive scripts.  For
;; example, you can send a block of shell-script commands to a shell
;; window for immediate execution.
;;
;; The operation to be performed is determined by the value of the
;; buffer-local variable `linkd-process-block-function'.  You can set
;; this to an appropriate value in a file's `Local Variables' section.

(defun linkd-block-around-point ()
  "Return the block around point as a string."
  (interactive)
  (let ((beg (save-excursion
               (search-backward linkd-star-search-string) (beginning-of-line) (point)))
        (end (save-excursion (search-forward linkd-star-search-string) (point))))
    (buffer-substring-no-properties beg end)))

(defun linkd-write-block-to-file (block-text)
  "Write the BLOCK-TEXT to the file named by linkd-block-file-name."
  (interactive)
  (with-temp-buffer
    (insert block-text)
    (write-file linkd-block-file-name)))

(defun linkd-process-block ()
  "Process the Linkd block around point."
  (interactive)
  (funcall linkd-process-block-function (linkd-block-around-point)))

(defun linkd-send-block-to-shell (block-text)
  "Send the Linkd block around point to the shell."
  (interactive)
  ;; create shell if needed, but not in this window
  (unless (get-buffer-window linkd-shell-buffer-name)
    (save-window-excursion (shell linkd-shell-buffer-name))
    (display-buffer linkd-shell-buffer-name))
  (linkd-write-block-to-file block-text)
  (save-selected-window
    (select-window (get-buffer-window linkd-shell-buffer-name))
    (goto-char (point-max))
    (insert (concat ". " linkd-block-file-name)) ; make the shell source the temp file
    (call-interactively (key-binding "\r"))))


;; (@* "Datablocks") -------------------------------------------------
;;
;; A datablock is an embedded object of a user-defined type.  It
;; consists of a "type symbol" followed by a printed representation of
;; a Lisp object called the "embedded object".  The type symbol is a
;; symbol whose `symbol-function' determines the appearance and
;; behavior of the region of the buffer that contains the embedded
;; object.  By convention, a type symbol's name begins with a caret
;; (`^').
;;
;; When a datablock is "activated", the embedded object is read from
;; the buffer and fed to the type symbol's function.  This function
;; can temporarily replace the region with an interactive
;; representation of the embedded object, which can then be
;; manipulated by the user.  The behavior of this representation may
;; be effected by various uses of Emacs' text properties.
;;
;; When a datablock is "deactivated", the interface is replaced with a
;; plain-text representation of the new embedded object.  You can
;; arrange for the automatic activation and deactivation of datablocks
;; - for example, upon saving and loading files that contain them.
;;
;; Datablocks must be activated on a per-file basis via a `Local
;; Variables' section in the file.

;; Function to extract the embedded object at point.
(defun linkd-datablock-object-at-point ()
  "Returns the Linkd datablock object at point."
  (get-text-property (point) 'linkd-datablock-object))

;; A function to insert a template datablock. This is what you use to
;; create new datablocks with specified contents.

(defun linkd-insert-datablock-template (&optional object)
  "Insert a new datablock with OBJECT as the printed contents."
      (insert (format "(^begin ^cell)\n%S\n(^end)" object)))

;; This function governs the interaction of linkd-mode's datablock
;; system with the ``modules'' that implement various types of
;; embedded objects. First the type symbol and embedded object are
;; read in from the text. The function value of the module's type
;; symbol is obtained, and the embedded object is fed to the function
;; in order to activate or deactivate the datablock as needed. The
;; function is also passed some markers that delimit the region to
;; which the module should confine its rendering activity.

(defun linkd-activate-datablock (action)
  "When ACTION is :begin, activate the current datablock. When
ACTION is :end, deactivate the datablock."
  (interactive)
  (when (search-forward (concat "(^" "begin ") nil t)
    ;; first read in the datablock
    (let* ((type-symbol (read (current-buffer)))
           (datablock-begin (match-beginning 0))
           (datablock-object (progn (forward-line) (read (current-buffer))))
           (datablock-end (progn (search-forward "(^end)") (match-end 0)))
           (activate (symbol-function type-symbol)))
      (goto-char datablock-begin)
      (case action
        (:begin ; insert markers; datablock display happens in between them
         (let* ((inhibit-read-only t)
                (beg (make-marker))
                (end (make-marker)))
           (set-marker beg (save-excursion (goto-char datablock-begin) (point-at-eol)))
           (set-marker end (save-excursion (goto-char datablock-end) (point-at-bol)))
           ;; make the delimiters invisible
           (add-text-properties datablock-begin beg '(invisible t))
           (add-text-properties end datablock-end '(invisible t))
           ;; start the datablock going, tell it what region it is to manage
           (let ((object (funcall activate :begin datablock-object beg end)))
             (when (null object) (error "Null object."))
             ;; save datablock details for later lookup
             (add-text-properties beg end (list 'linkd-datablock-object object)))
           ;; move into the region
           (goto-char (+ 1 datablock-begin))
           (message "%S" (linkd-datablock-object-at-point))))
        (:end       ; stop managing the region and write the sexp back
         (forward-line)
         (let ((object (funcall activate :end datablock-object))
               (inhibit-read-only t)
               (inhibit-point-motion-hooks t))
           (delete-region datablock-begin datablock-end)
           (insert (format (concat "(^" "begin %S)\n%S\n(^end)") type-symbol object))))))))

(defun linkd-begin-datablock ()
  "Begin a Linkd datablock."
  (linkd-activate-datablock :begin))

(defun linkd-end-datablock ()
  "End a Linkd datablock."
  (linkd-activate-datablock :end))

(defun linkd-escape-datablock ()
  "Find the previous datablock beginning."
  (interactive)
  (search-backward (concat "(" "^begin "))
  (forward-line -1))

(defun linkd-activate-all-datablocks ()
  "Activate all Linkd datablocks."
  (interactive)
  (when (and linkd-use-datablocks (not linkd-datablocks-activated))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (linkd-begin-datablock)
        (forward-line))
      (setq linkd-datablocks-activated t))))

(defun linkd-deactivate-all-datablocks ()
  "Deactivate all Linkd datablocks."
  (interactive)
  (when (and linkd-use-datablocks linkd-datablocks-activated)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (linkd-end-datablock)
        (forward-line))
      (setq linkd-datablocks-activated nil))))


;; (@* "Exporting to Other Formats") ---------------------------------
;;
;; Linkd supports export to LaTeX and HTML. What follows are some
;; functions basic to the export process.

(defun linkd-export (export-function output-file-name)
  "Export the current-buffer using EXPORT-FUNCTION and write
  output to OUTPUT-FILE-NAME. EXPORT-FUNCTION should convert to
  the output format, do any required postprocessing, and return
  the buffer with the ouput."
  (with-current-buffer (funcall export-function)
    (write-file (expand-file-name output-file-name))))

;;;###autoload
(defun linkd-export-default ()
  "Export the current buffer with default settings to all available formats."
  (interactive)
  (dolist (format linkd-export-formats-alist)
    (let* ((extension (car format))
           (output-file (concat (buffer-file-name) "." extension))
           (export-function (cdr format)))
      (linkd-export export-function output-file))))

;; (@* "Exporting to LaTeX") -----------------------------------------
;;
;; This section contains routines to transform Lisp source code files
;; into beautiful LaTeX documents in (roughly) the style of Donald
;; Knuth's "Literate Programming".  To take advantage of this feature,
;; the source code to be transformed should contain alternating
;; regions of commentary and code, with appropriate star headings to
;; group these regions into document sections.
;;
;; FIXME: There is no such function: `linkd-latex-render'
;;
;; The interactive function `linkd-latex-render' transforms the source
;; code in a temporary buffer and writes the result to a corresponding
;; LaTeX file.  Where tags appear in Commentary, they are prettified
;; in the LaTeX output.
;;
;; The purist might object that true literate programming requires a
;; tool capable of resequencing code fragments and performing macro
;; expansion, neither of which are implemented here.  In response to
;; this objection I (David O'Toole) point out the following: (i) there
;; is little need for resequencing in a language like Lisp, where
;; declarations can be ordered more or less as you please; (ii) Lisp
;; already has a powerful macro expansion facility; and (iii) there is
;; no reason why a system that deviates somewhat from the
;; traditionally accepted definition of literate programming should
;; not still contribute to the writing of better programs.
;;
;; FIXME: No such `require' in this file: The `fancyvrb' package is
;; required.

(defun linkd-latex-begin-verbatim ()
  "Insert LaTeX `Verbatim' start tag."
  (setq linkd-latex-in-verbatim t)
  (insert (concat "\\" "begin{Verbatim}[fontsize=\\small]\n")))

(defun linkd-latex-end-verbatim ()
  "Insert LaTeX `Verbatim' end tag."
  (setq linkd-latex-in-verbatim nil)
  (insert (concat "\\" "end{Verbatim}\n")))

(defun linkd-latex-do-section (title)
  "Insert LaTeX section tag."
  (insert (format "\\section{%s}\n" title)))

(defun linkd-latex-toggle-verbatim ()
  "Insert LaTeX `Verbatim' begin or end tag, as needed."
  (if linkd-latex-in-verbatim (linkd-latex-end-verbatim) (linkd-latex-begin-verbatim)))

;;;###autoload
(defun linkd-latex-export ()
  "Render a buffer as a LaTeX book chapter."
  (interactive)
  (let* ((output-buffer (get-buffer-create "*linkd-litprog*"))
         (source-buffer (current-buffer)))
    (with-current-buffer output-buffer
      (let ((linkd-use-datablocks nil))
        (delete-region (point-min) (point-max)) ; clean up any previous output
        (insert-buffer-substring-no-properties source-buffer) ; make a copy of the source
        ;; delete everything before first heading
        (goto-char (point-min))
        (re-search-forward linkd-export-heading-regexp)
        (previous-line)
        (end-of-line)
        (delete-region (point-min) (point))
        ;; now process each block in turn.
        (while (and (not (eobp)) (re-search-forward linkd-export-heading-regexp nil nil))
          (let ((title (match-string 1)))
            (delete-region (point-at-bol) (point-at-eol))
            (linkd-latex-do-section title)
            (forward-line)
            (block processing
              (while (not (eobp))
                (cond ((string-match linkd-export-heading-regexp ; heading
                                     (buffer-substring (point-at-bol) (point-at-eol)))
                       (when linkd-latex-in-verbatim (linkd-latex-end-verbatim))
                       (return-from processing))
                      ((looking-at linkd-export-commentary-regexp) ; commentary
                       ;; get rid of comment delimiter
                       (delete-region (match-beginning 0) (match-end 0))
                       (when linkd-latex-in-verbatim (linkd-latex-end-verbatim)))
                      (t                ; code
                       (when (null linkd-latex-in-verbatim) (linkd-latex-begin-verbatim))))
                (forward-line 1)))
            (when linkd-latex-in-verbatim ; close verbatim environment
              (linkd-latex-end-verbatim))))
        ;; render linkd's tags nicely
        (let ((tag-regexp "\(\@> \"\\(.*\\)\")"))
          (goto-char (point-min))
          (while (and (not (eobp)) (re-search-forward tag-regexp nil t))
            (replace-match (format "$\\\\Rightarrow ${\\\\bf %s}" (match-string 1)))))
        (current-buffer)))))


;; (@* "Exporting to HTML") ------------------------------------------
;;
;; This functionality is built on top of Hrvoje Niksic's htmlize.el:
;; http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el

(defun linkd-html-export ()
  "Convert the current buffer to HTML using htmlize.el and some
extra rules. Return the buffer."
  (when (require 'htmlize nil t)
    (let* ((source-buffer (current-buffer))
           (output-buffer (htmlize-buffer source-buffer)))
      ;; now postprocess it
      (with-current-buffer output-buffer
        (goto-char (point-min))
        (let ((star-regexp
               (concat "<span class=\"linkd-generic\">(" "@" "\\* \"\\(.*\\)\")</span>"))
              (sexp-regexp
               (concat "<span class=\"linkd-generic\">(" "@" "[^ ].* \"\\(.*\\)\")</span>")))
          (while (re-search-forward star-regexp nil t)
            (replace-match
             (concat "<img src=\"/images/linkd-star.xpm.png\"> "
                     "<span style=\"color: #ffff00; background-color: #698b22;\">\\1</span>")))))
      ;; return the buffer
      output-buffer)))


;; (@* "Links to Files") ---------------------------------------------
;;
;; Since Emacs works mainly with in text files, one of the most common
;; uses for a link is in navigating from one text file to another.
;; The following declarations define such file links.  (Note how the
;; function `@file' returns the type of property list discussed in
;; section (@> "Following links").
;;
;; You can also associate a Lisp function with each type of file, and
;; then arrange for the function to be used to open the file (instead
;; of visiting it within Emacs using `find-file'.)

(defun @file (&rest p)
  "$$$$$$$ FIXME"
  (let ((file-name  (plist-get p :file-name))
        (to         (plist-get p :to))
        (display    (plist-get p :display)))
    `(:follow
      (lambda ()
        (let ((handler    (cdr (assoc (file-name-extension ,file-name)
                                      linkd-file-handler-alist))))
          (if handler
              (funcall handler ,file-name)
            ;; default action is find-file
            (find-file ,file-name))
          (when ,to
            (beginning-of-buffer)
            (search-forward ,to)))
        (linkd-maybe-enable-in-target))
      :render
      (lambda (beg end)
        (linkd-overlay beg end ,(or display (concat file-name (if to (concat " : " to) "")))
                       nil linkd-default-bullet-string nil
                       ,(linkd-file-icon file-name))))))


;; (@* "Other Link Types") -------------------------------------------
;;
;; Here are more examples of link type definitions. These link types
;; navigate to UNIX manual pages, GNU Info documentation, and to
;; webpages.

(defun @man (&rest p)
  "$$$$$$$$$$ FIXME"
  (let ((page (plist-get p :page))
        (to (plist-get p :to))
        (display (plist-get p :display)))
    `(:follow
      (lambda ()
        (man ,page)
        (when ,to
          (beginning-of-buffer)
          (search-forward ,to)))
      :render
      (lambda (beg end)
        (linkd-overlay
         beg end ,(or display (concat page " manual" (if to (concat " : " to) "")))
         nil linkd-default-bullet-string nil ,(linkd-icon "man"))))))

(defun @info (&rest p)
  "$$$$$$$$$$ FIXME"
  (let ((file (plist-get p :file-name))
        (node (plist-get p :node))
        (to (plist-get p :to))
        (display (plist-get p :display)))
    `(:follow
      (lambda ()
        (info (concat "(" ,file ")" ,node))
        (when ,to
          (beginning-of-buffer)
          (search-forward ,to)))
      :render
      (lambda (beg end)
        (linkd-overlay
         beg end ,(or display (concat file " manual" (if to (concat " : " to) "")))
         'linkd-generic-link-name linkd-default-bullet-string nil ,(linkd-icon "info"))))))

(defun @url (&rest p)
  "$$$$$$$$$$ FIXME"
  (let ((file-name (plist-get p :file-name))
        (display (plist-get p :display)))
    `(:follow
      (lambda ()
        (browse-url ,file-name))
      :render
      (lambda (beg end)
        (linkd-overlay beg end ,(or display file-name) 'linkd-generic-link-name
                       linkd-default-bullet-string nil ,(linkd-icon "url"))))))


;; (@* "Lisp Links") -------------------------------------------------

(defun @L (sexp)
  "$$$$$$$$$$ FIXME"
  `(:follow
    (lambda ()
      (message "%S" (eval ,sexp)))
    :render
    (lambda (beg end)
      (linkd-overlay beg end ,(format "%S" sexp) 'linkd-command
                     linkd-default-bullet-string nil ,(linkd-icon "url")))))


;; (@* "Wiki Features") ----------------------------------------------
;;
;; When using Emacs, you typically build up a library of text files.
;; You can turn this collection into a hypertext wiki by inserting
;; wiki links from one file to another.  Wiki names LookLikeThis.

;;;###autoload
(defun linkd-wiki-find-page (page-name)
  "Find Linkd wiki page named PAGE-NAME."
  (interactive "s")
  (let ((page-file
         (block testing
           (dolist (extension linkd-wiki-extensions)
             (let ((test-filename (concat (file-name-as-directory linkd-wiki-directory)
                                          page-name "." extension)))
               (if (file-exists-p test-filename)
                   (return-from testing test-filename)
                 (return-from testing nil)))))))
    (if page-file
        (find-file page-file)
      ;; otherwise, query the user which file extension to create
      (let ((ext (completing-read "Create wiki page with extension: " linkd-wiki-extensions)))
        (find-file
         (concat (file-name-as-directory linkd-wiki-directory) page-name "." ext))))))

(defun @! (page)
  "$$$$$$$$$$ FIXME"
  `(:follow
    (lambda () (linkd-wiki-find-page ,page))
    :render
    (lambda (beg end) (linkd-overlay beg end ,page 'linkd-wiki))))


;; (@* "Minor Mode for Linkd") ---------------------------------------
;;
;; When Linkd minor mode is active, links are displayed using
;; overlays, and keybindings are available for common Linkd functions.
;; The keybindings are in accord with the convention for minor-modes:
;; `C-c' followed by one of a set of reserved punctuation characters.

(define-minor-mode linkd-mode
    "Create or follow hypertext links.
These link navigation commands are available:

\\<linkd-map>\\[linkd-follow-at-point]		- follow link under cursor
\\[linkd-follow-mouse]	- follow clicked link
\\[linkd-back]		- return to last link followed
\\[linkd-next-link]		- go to next link in buffer
\\[linkd-previous-link]		- go to previous link in buffer

These key bindings are in effect on a link:\n
\\{linkd-overlay-map}These key bindings are effect everywhere:\n
\\{linkd-map}"
  nil :lighter " Linkd" :keymap linkd-map (if linkd-mode (linkd-enable) (linkd-disable)))

(defun linkd-enable ()
  "Enable Linkd mode."
  (let ((modified-p (buffer-modified-p)))
    (add-hook 'before-save-hook 'linkd-deactivate-all-datablocks :append :local)
    (add-hook 'after-save-hook 'linkd-activate-all-datablocks :append :local)
    (linkd-do-font-lock 'font-lock-add-keywords)
    (font-lock-fontify-buffer)
    (set-buffer-modified-p modified-p)))

(defun linkd-disable ()
  "Disable Linkd mode."
  (let ((modified-p (buffer-modified-p)))
    (remove-hook 'before-save-hook 'linkd-deactivate-all-datablocks)
    (remove-hook 'after-save-hook 'linkd-activate-all-datablocks)
    ;; remove all linkd's overlays
    (mapcar (lambda (overlay)
              (when (get-text-property (overlay-start overlay) 'linkd-fontified)
                (delete-overlay overlay)))
            (overlays-in (point-min) (point-max)))
    ;; remove font-lock rules, textprops, and then refontify the buffer
    (linkd-do-font-lock 'font-lock-remove-keywords)
    (remove-text-properties (point-min) (point-max) '(linkd-fontified))
    (font-lock-fontify-buffer)
    (set-buffer-modified-p modified-p)))


;; (@* "Font-Locking") -----------------------------------------------
;;
;; Each link type can execute arbitrary code to render itself.  In the
;; typical case, we use `(linkd-overlay)' to render the link using
;; overlays and possibly icons.
;; See also (@> "Rendering links with overlays").
;;
;; The following function invokes a link's rendering code.

(defun linkd-render-link (beg end)
  "Invoke a link's rendering code."
  (unless (get-text-property beg 'linkd-fontified)
    (save-excursion
      (goto-char beg)
      (let ((sexp  (read (current-buffer))))
        ;; For a Linkd link, the sexp is always a list whose car is a function
        ;; name that begins with `@'.
        (when (and sexp (fboundp (car sexp)))
          (add-text-properties beg (+ beg 1) (list 'linkd-fontified t))
          (let* ((plist     (eval sexp))
                 (renderer  (plist-get plist :render)))
            (unless renderer (error "No renderer for link."))
            (funcall renderer beg end)))))))

;; Interface with the Emacs font-locking system.  You can configure
;; `linkd-do-font-lock' to add or remove font-locking rules that cause
;; Linkd's links to be fontified.

(defun linkd-do-font-lock (add-or-remove)
  "Add or remove font-lock rules for Linkd."
  (funcall add-or-remove nil `((linkd-match 0 (let ((beg (match-beginning 0))
                                                    (end (match-end 0)))
                                                (linkd-render-link beg end)
                                                'linkd-generic-link)
                                            prepend))))

;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'linkd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; linkd.el ends here
