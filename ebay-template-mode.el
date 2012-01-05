;;; ebay-template-mode.el --- A handy template for editing auction listings.
;;; ================================================================
;;; DESCRIPTION:
;;; ebay-template-mode provides a handy template for editing auction listings.
;;;
;;; FUNCTIONS:►►►
;;; 
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; CONSTANTS:
;;; `ebay-template-mode-version'
;;;
;;; VARIABLES:
;;; `*ebay-template-font-lock-keywords*', `*ebay-field-entry*',
;;; `*ebay-field-delims*', `*ebay-line-delims*'
;;; `ebay-template-mode-map', `ebay-template-mode-hook'
;;; `ebay-template-mode-syntax-table'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED: 
;;;
;;; MOVED:
;;;
;;; REQUIRES:
;;; `ebay-template-tools', 
;;; If `ebay-template-html-utils' isn't installed then you will need:
;;; `html-lite', and eventually `xml-gen' (we are transitioning from html-lite)
;;;
;;; TODO:
;;; Incorporate button code per Jared D.'s discussion of file-editor @:
;;; (URL `http://curiousprogrammer.wordpress.com/2009/06/21/enabling-your-users/')
;;; (require 'button)
;;; (define-button-type 'open-dir
;;;   'action 'file-editor-open-dir
;;;   'follow-link t
;;;   'help-echo "Open Directory")
;;;
;;; (define-button-type 'open-file
;;;   'action 'file-editor-open-file
;;;   'follow-link t
;;;   'help-echo "Open Configuration File")
;;;   {... FUNCS FOR FILES ...}
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; THIRD PARTY CODE:
;;; `xml-gen' 
;;; (URL `http://www.shellarchive.co.uk/content/emacs.html')
;;; `html-lite'
;;; (URL `http://www.emacswiki.org/cgi-bin/wiki/download/html-lite.el')
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: 
;;; (URL `http://www.emacswiki.org/emacs/ebay-template-mode.el')
;;; FILE-PUBLISHED: <Timestamp: #{2009-09-20} - by MON KEY>
;;;
;;; FILE-CREATED:
;;; <Timestamp: #{2009-08-19T15:11:11-04:00Z}#{09343} - by MON>
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

;;; ==============================
(eval-when-compile (require 'cl))
;;
(require 'ebay-template-tools)
;;
(if (or IS-MON-P IS-BUG-P)
      (require 'ebay-template-html-utils)
      (require 'smith-poster-utils)
      ;; EMACS-WIKI:
      ;; If `ebay-template-html-utils' isn't installed then you will need:
      ;; `html-lite', `xml-gen'
      (unless (featurep 'ebay-template-html-utils)
	(unless (featurep 'xmlgen) (require 'xmlgen))
	;;(URL `http://www.shellarchive.co.uk/content/emacs.html')
	;;(URL `http://repo.or.cz/w/ShellArchive.git?a=blob_plain;hb=HEAD;f=xmlgen.el')
	(unless (featurep 'html-lite) (require 'html-lite))))
	;; (URL `http://www.emacswiki.org/cgi-bin/wiki/download/html-lite.el')

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T12:46:08-04:00Z}#{09325} - by MON KEY>
(defgroup ebay-template-mode nil
  "Customization of `ebay-template-mode'."
  ;; :link (url-link URL)
  ;; :link (file-link FILE)
  :group 'local)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T12:46:05-04:00Z}#{09325} - by MON KEY>
(defgroup ebay-template-mode-faces nil
  "Customization of `ebay-template-mode' font-locking faces."
  ;; :link (file-link FILE)
  :group 'faces
  :group 'ebay-template-mode)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T12:46:00-04:00Z}#{09325} - by MON KEY>
(defconst ebay-template-mode-version "September 2009"
  "Return current version of `ebay-template-mode'.")

;;; ==============================
(defcustom e-comment-prefix ";;; "
  "*String used by `comment-region' to comment out region.
Used in `ebay-template-mode'."
  :type 'string
  :group 'ebay-template-mode)

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T11:03:21-04:00Z}#{09325} - by MON KEY>
(defvar *ebay-field-entry* nil
  "List of ebay fields which are fontlocked in `ebay-template-mode'.
'ebay-item-title:'; \n'ebay-item-number:'; \n'ebay-item-start-date:'; 
'ebay-item-end-date:'; \n 'ebay-item-listing-duration:'; 
'ebay-item-start-price:'; \n'ebay-item-reserve:'; \n'ebay-item-listing-fee:';
\n 'ebay-item-buy-it-now-fee:'; \n'ebay-item-paypal-fee:'; 
'ebay-item-shipping-charged:'; \n'ebay-item-shipping-weight:'; 
'ebay-item-ship-to:'; \n'ebay-item-high-bidder-id:'; 
'ebay-item-times-listed:'; \n'ebay-item-page-views:'; 
'ebay-item-watchlist-count:'; \n'ebay-item-offers:'; 
'ebay-item-listed-in-category:'; \n'ebay-item-notes:'; \n
See also; `*ebay-template-font-lock-keywords*', `*ebay-field-delims*',
`*ebay-line-delims*'.")
;;
(when (not (bound-and-true-p *ebay-field-entry*))
  (setq *ebay-field-entry*
        (let ((ebay-field-entry-list
               '("ebay-item-title:"
                 "ebay-item-number:"
                 "ebay-item-start-date:"
                 "ebay-item-end-date:"
                 "ebay-item-listing-duration:"
                 "ebay-item-start-price:"
                 "ebay-item-reserve:"
                 "ebay-item-listing-fee:"
                 "ebay-item-buy-it-now-fee:"
                 "ebay-item-paypal-fee:"
                 "ebay-item-shipping-charged:"  ;ebay-item-shipping-cost:
                 "ebay-item-shipping-weight:"
                 "ebay-item-ship-to:"
                 "ebay-item-high-bidder-id:"
                 "ebay-item-times-listed:"
                 "ebay-item-page-views:"
                 "ebay-item-watchlist-count:"
                 "ebay-item-offers:"
                 "ebay-item-listed-in-category:"
                 "ebay-item-notes:"
                 )))
          (concat "^" (regexp-opt ebay-field-entry-list 'paren)))))

;;;test-me; *ebay-field-entry*
;;;(progn (makunbound '*ebay-field-entry*) (unintern '*ebay-field-entry*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T11:12:21-04:00Z}#{09325} - by MON KEY>
(defvar *ebay-field-delims* nil
  "Regexp for fontlocking delims `◄◄◄' `►►►' in ebay-dbc-files.
These delims are are fontlocked in `ebay-template-mode'.\n
See also; `*ebay-template-font-lock-keywords*', `*ebay-field-entry*', 
`*ebay-line-delims*', .")
;;
(when (not (bound-and-true-p *ebay-field-delims*))
  (setq *ebay-field-delims* "^\\(◄◄◄\\|►►►\\)"))

;;;test-me; *ebay-field-delims*
;;;(progn (makunbound '*ebay-field-delims*) (unintern '*ebay-field-delims*))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T11:55:31-04:00Z}#{09325} - by MON KEY>
(defvar *ebay-line-delims* nil
  "Regexp for fontlocking line delims in ebay-dbc-files.
These delims are are fontlocked in `ebay-template-mode'
REGEXP:
....1..2.........................3.............4...................
\"^\\\\\(\\\\\(;;; =\\\\\\=\{30,37\\\\\}$\\\\\)\\\\|\\\\\(;;; $\\\\\)\\\\|\\\\\(~\\\\\\=\{71,71\\\\}¦$\\\\\)\\\\\)\"
MATCH GROUPS:
\";;; \" ;-> grp3
\";;; ==============================\"        ;-> grp2  ;'count of `=' chars -> 30
\";;; =====================================\" ;-> grp2 ;'count of `=' chars -> 37
\"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~¦\";-> grp4\n
See also; `*ebay-template-font-lock-keywords*', `*ebay-field-entry*', 
`*ebay-line-delims*'.")
;;
 (when (not (bound-and-true-p *ebay-line-delims*))
   (setq *ebay-line-delims* 
         "^\\(\\(;;; =\\{30,37\\}$\\)\\|\\(;;; $\\)\\|\\(~\\{71,71\\}¦$\\)\\)"))

;;;test-me; *ebay-line-delims*
;;;test-me
;;;(search-forward-regexp *ebay-line-delims*)
;;;;..1..2.........................3............4...................
;;;"^\\(\\(;;; =\\{30,37\\}$\\)\\|\\(;;; $\\)\\|\\(~\\{71,71\\}¦$\\)\\)")
;;; Not at end of line
;;;"^\\(\\(;;; =\\{30,37\\}\\)\\|\\(;;; \\)\\|\\(~\\{71,71\\}¦\\)\\)")
;;;
;;; <uncomment below from `;;; ' e.g. column 4
;;; (match-beginning 1)
;;; (match-beginning 2)
;;; ;;; ==============================
;;; ;;; =====================================
;;; (match-beginning 3)
;;; ;;; 
;;; (match-beginning 4)
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~¦
;; (defvar *ebay-line-delims* nil
;; "See also; `*ebay-template-font-lock-keywords*', `*ebay-field-entry*', 
;; `*ebay-line-delims*'.")
;;
;; (when (not (bound-and-true-p *ebay-line-delims*))
;;   (setq *ebay-line-delims* 
;;         (concat
;;          "^\\(" ";;; =============================="  "\\|" ";;; ====================================="
;;          "\\|" "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~¦"
;;          "//)")))

;;;(progn (makunbound '*ebay-line-delims*) (unintern '*ebay-line-delims*))
;;;(defun test
;;; ==============================
(defvar *ebay-template-font-lock-keywords* nil
  "Fontlock keywords for ebay-dbc-file templates using `ebay-template-mode'.\n
See also; `*ebay-field-entry*', `*ebay-field-delims*', `*ebay-line-delims*'.")
;;
(when (not (bound-and-true-p *ebay-template-font-lock-keywords*))
  (setq *ebay-template-font-lock-keywords*
        `((,*ebay-field-delims* 0 font-lock-keyword-face t)
          (,*ebay-field-entry* 0 naf-mode-field-fface t)
          (,*ebay-line-delims* 0 font-lock-function-name-face t)
          (,(concat "\\(" naf-mode-timestamp-flag "\\)") 0 naf-mode-timestamp-fface t))))

;;;test-me; *ebay-field-entry*
;;;test-me; *ebay-field-delims*
;;;test-me; *ebay-line-delims*
;;;test-me; naf-mode-timestamp-flag (concat "\\(" naf-mode-timestamp-flag "\\)")

;;;test-me *ebay-template-font-lock-keywords*

;;;(progn (makunbound '*ebay-template-font-lock-keywords*)(unintern '*ebay-template-font-lock-keywords*))

;;; ==============================
;;; Copy the map then modify it with:
;;; (let ebay-template-map (copy-keymap naf-mode-map)) 
;;; (let ((ebay-template-map (make-sparse-keymap)))
;;; (set-keymap-parent ebay-template-map naf-mode-map)
;;; (keymap-parent ebay-template-mode-map)
;;; CREATED: <Timestamp: #{2009-08-07T12:40:59-04:00Z}#{09325} - by MON KEY>
(defvar ebay-template-mode-map
  (let ((ebay-template-map (if (bound-and-true-p naf-mode-map) ;;EMACS-WIKI
			       (copy-keymap naf-mode-map))))
  ;; ADD KEYBINDING:
    (define-key ebay-template-map "\C-c\M-cet" 'mon-cln-ebay-time-string)
    (define-key ebay-template-map "\C-c\M-lr"  'mon-insert-ebay-field-trigger-l-and-r)
    (define-key ebay-template-map "\C-c\M-lf"  'mon-insert-ebay-field-trigger-l)
    (define-key ebay-template-map "\C-c\M-rt"  'mon-insert-ebay-field-trigger-r)
    (define-key ebay-template-map "\C-c\M-sre" 'mon-set-smith-poster-register-e)
    ebay-template-map)
  "Keymap for `ebay-template-mode'.
Some keys bound also global in `mon-keybindings'.
Globals are included here in order that `describe-mode' in `*Help*'
buffers can show the global-bindings too as they are still used most by naf-mode.
Typically `naf-mode' binds ``\C-c\\M-##\''.")
;;
;;;(progn (makunbound 'ebay-template-mode-map) (unintern 'ebay-template-mode-map))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-27T12:30:53-04:00Z}#{09354} - by MON>
;;                ;;SYMBOL                ;;MAPS
(easy-menu-define ebay-template-mode-menu ebay-template-mode-map ;naf-mode-map
  "Menu for working with ebay template files."  ;;DOC
  ;;MENU
  '("Ebay-Template-mode"
    ["Ebay clean time string" mon-cln-ebay-time-string :help "eBay timestring -> local fromat"]
    ["Ebay insert ►►► ◄◄◄" mon-insert-ebay-field-trigger-l-and-r :help "Insert ►►► ◄◄◄ delimters"]
    ["Ebay insert ►►►"  mon-insert-ebay-field-trigger-r :help "insert ►►► delimiter"]
    ["Ebay insert ◄◄◄"  mon-insert-ebay-field-trigger-l :help "Insert ◄◄◄ delimiter"]
    ["Ebay Smith sale template" mon-set-smith-poster-register-e :help "Insert Smith Poster Sales template"]
    "---"))

;;;test-me; ebay-template-mode-menu
;;;(progn (makunbound 'ebay-template-mode-menu) (unintern 'ebay-template-mode-menu))

;;; ==============================
;;; CREATED: <Timestamp: #{2009-08-07T12:42:25-04:00Z}#{09325} - by MON KEY>
;;; NOTE: `define-derived-mode' automatically creates the hook `ebay-template-mode-hook'
;;; e.g. (derived-mode-hook-name 'ebay-template-mode)
;;
(defvar ebay-template-mode-hook nil
 ;; :type 'hook when we make the defvar it a (defcustom
  "Hook called by `ebay-template-mode'.")

;;; ==============================
;;; (defvar ebay-template-mode-imenu-generic-expression
;;;   ...)

;;; ==============================
(defvar ebay-template-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    ;; Add `p' so M-c on `hello' leads to `Hello', not `hello'.
    (modify-syntax-entry ?' "W p" st)
    st)
  "Syntax table used while in `ebay-template-mode'.")

;;; ==============================
;;; FROM: TTN's ricette-mode.el - needed?
;;; (defvar ebay-template-mode-font-lock-defaults
;;;  '(*ebay-template-font-lock-keywords* t))	; t => solo "keywords"
;;; ==============================
;;; CREATED: <Timestamp: Tuesday June 02, 2009 @ 11:35.25 AM - by MON KEY>
(define-derived-mode ebay-template-mode fundamental-mode "eBay-Template"
  "Major mode to edit eBay templates. \\(ebay-template-mode-map}"
  ;;from Sample-mode.el -> Stephan Monnier   
   :group 'ebay-template-mode
  ;;  :link
  ;;automatically generated as `ebay-template-mode-syntax-table'   ;; :syntax-table TABLE  
  ;;automatically generated as `ebay-template-mode-abbrev-table'   ;; :abbrev-table TABLE
  ;; (set (make-local-variable 'imenu-generic-expression)
  ;;      sample-imenu-generic-expression)
   (set (make-local-variable 'font-lock-defaults) 
        '(*ebay-template-font-lock-keywords*))
   (set (make-local-variable 'font-lock-keywords)
        '(*ebay-template-font-lock-keywords*))

   (message "Loading eBay-Tamplate mode.")
  (run-mode-hooks 'ebay-template-mode-hook))

;;;(progn (makunbound 'ebay-template-mode)(unintern 'ebay-template-mode))

  ;; NOTE:
  ;; which is having the more righter?
  ;; ==============================
  ;;(setq font-lock-defaults *ebay-template-font-lock-keywords*)
  ;;      'ebay-template-mode-font-lock-defaults)

  ;; (set (make-local-variable 'font-lock-keywords)
  ;;      '(*ebay-template-font-lock-keywords*))
  ;;
  ;; FROM: ricette-mode.el -> Thien Thi Nguyen -> TTN 
  ;;  (make-local-variable 'font-lock-defaults)
  ;;  (setq font-lock-defaults ricette-font-lock-defaults)
  ;; ==============================

  
;;; ==============================
;; (add-hook 'ebay-template-mode-hook 
;;           (function (lambda () (message "Load eBay-Tamplate mode"))))


;;; ==============================
(provide 'ebay-template-mode)
;;; ==============================

;;; ================================================================
;;; ebay-template-mode.el ends here
;;; EOF
