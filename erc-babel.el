;;; erc-babel.el --- Communicate with ERC in different languages using online translation services

;; Filename: erc-babel.el
;; Description: Communicate with ERC in different languages using google translate
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2010
;; Version: 0.1
;; Last-Updated: 2013-05-13 18:23:24
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/erc-babel
;; Keywords: 
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((babel "") (erc "5.3"))
;;
;; Features that might be required by this library:
;;
;; babel erc
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 199uU1gHyepvBmrQGyGdCWPCrqtZ6G4pbh
;;
;; This library integrates babel (http://www.emacswiki.org/emacs/babel.el) with ERC 
;; (an emacs IRC client: http://www.emacswiki.org/cgi-bin/wiki/ERC) to provide automatic
;; translation of messages. It requires those two libraries are installed.
;; All sent and received messages will be translated if possible.
;; Different languages combinations can be set for different buffers, and translation can be toggled on or off.

;; It has been tested on emacs 23, with ERC version 5.3.

;; To toggle translation on/off (it is off by default) call `erc-babel-toggle'.

;; To set the languages to translate from and to for a particular buffer, call `erc-babel-set-languages'
;; The language combinations can be different for different buffers/channels.
;; To remember them between emacs sessions you could use session.el by Christopher Wedler 
;; (available here: http://emacs-session.sourceforge.net/), and add `erc-babel-alist' to session-globals-include.
;; To remove translation for a particular buffer, set the from and to languages to the same value.

;; To include the original text with the translated message make sure `erc-babel-include-orig' is non-nil
;; This can be turned on/off by calling `erc-babel-include-orig-toggle'.

;; You can choose which webservice to use for doing the actual translation by customizing `erc-babel-backend' to
;; one of those listed. By default it uses Google, but you can change it if that doesn't work for you (check you 
;; have the latest version of babel.el)

;; You can also change the modeline display by customizing `erc-babel-modeline-type'. 
;; It shows you the languages being translated, and in which direction(s). 
;; By default it uses the full language names but you can change it to use short abbreviations.

;; You may also want to bind some keys in erc-mode like this:

;; (define-key erc-mode-map (kbd "C-c C-S-t") 'erc-babel-toggle)
;; (define-key erc-mode-map (kbd "C-c C-S-o") 'erc-babel-include-orig-toggle)

;; which can be put somewhere in your .emacs 
;; 
;;;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `erc-babel-toggle'
;;    Toggle automatic translation on/off.
;;  `erc-babel-include-orig-toggle'
;;    Toggle inclusion of original text in translations.
;;  `erc-babel-set-languages'
;;    Set translation languages for `buf' for use with erc-babel-translate-msg.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `erc-babel-enable'
;;    Whether messages should be automatically translated or not
;;    default = nil
;;  `erc-babel-include-orig'
;;    Whether or not to include original text with translations.
;;    default = t
;;  `erc-babel-backend'
;;    Webservice backend to use for translations.
;;    default = "Google"
;;  `erc-babel-modeline-type'
;;    What do display in modeline:
;;    default = "long"
;;
;;
;; All of the above can customized by:
;;      M-x customize-group RET erc-babel RET
;;

;;; Installation:
;;
;; Put erc-babel.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'erc-babel)


;;; Change log:
;;	
;; 2013/05/13
;;      * Use google-translate.el instead of babel.el (since that no longer works and is not available
;;        in the repositories)
;; 
;; 2010
;;      * First released

;;; Acknowledgements:
;;
;; Juergen Hoetzel <juergen@hoetzel.info> - creator of babel.el
;;

;;; TODO
;;
;; 
;;

;;; Require
(require 'babel)
(require 'erc)

;;; Code:

(defgroup erc-babel nil
  "Variables related to babel usage."
  :group 'erc)

(defcustom erc-babel-enable nil
  "Whether messages should be automatically translated or not"
  :type 'boolean
  :group 'erc-babel)

(defvar erc-babel-alist nil
  "List of buffers names and their associated language translation variables.
Each element of this list should look like (\"bufname\" \"from\" \"to\" out in) where 
\"bufname\" is the name of the buffer, \"from\" is the language to translate outgoing message from, 
\"to\" is the language to translate outgoing messages to, `out' and `in' are boolean values (t or nil)
indicating whether outgoing/incoming messages will be translated respectively.
For incoming messages the roles of the \"from\" and \"to\" languages are reversed.")

(defvar erc-babel-message-regexp "^<[^>
]*> \\([^
]*\\)$"
  "regexp to use for matching incoming messages in the narrowed buffer used by erc-babel-translate-incoming
You shouldn't need to change this, but it is included as a variable just in case.
If the incoming translated messages are not printing properly, you may need to change it.")

(defcustom erc-babel-include-orig t
  "Whether or not to include original text with translations.
 (original text will be appended after translation in parenthesis)."
  :type 'boolean
  :group 'erc-babel)

(defcustom erc-babel-backend "Google"
  "Webservice backend to use for translations."
  :type `(choice ,@(mapcar (lambda (s) `(const ,(car s))) babel-backends))
  :group 'erc-babel)

(defcustom erc-babel-modeline-type "long"
  "What do display in modeline:
`long' = full names of languages
`short' = short abbreviations for languages (e.g. en for english)
`none' = no modeline display.

For `short' and `long' the format will be [from<->to] where from is the language typed at your end of the 
conversation, and to is the language at the other end. If translation is only one way (because the backend is
only capable of translation in one direction), the middle arrow will be displayed accordingly (e.g. -> or <-)."
  :type '(choice (const "long") (const "short") (const "none"))
  :group 'erc-babel)

(defun erc-babel-modeline nil
  "Return string to put in modeline to indicate from and to translation languages"
  (if (or (not erc-babel-enable) (equal erc-babel-modeline-type "none")) nil
    (let* ((set (assoc (buffer-name (current-buffer)) erc-babel-alist))
	   (toabbrev (car (cdr (cdr set))))
	   (fromabbrev (car (cdr set)))
	   (out (car (cdr (cdr (cdr set)))))
	   (in (car (cdr (cdr (cdr (cdr set))))))
	   (join (cond ((and in out) "<->") (in "<-") (out "->"))) 
	   to from backend languages)
      (if (or (not join) (equal fromabbrev toabbrev)) nil
	(if (equal erc-babel-modeline-type "short") (concat "[" fromabbrev join toabbrev "]")
	  (setq backend (symbol-name (cdr (assoc erc-babel-backend babel-backends))))
	  (setq languages (eval (intern-soft (concat "babel-" backend "-languages"))))
	  (setq to (if (equal backend "google") (car (rassoc toabbrev languages))
		     (cdr (assoc toabbrev languages))))
	  (setq from (if (equal backend "google") (car (rassoc fromabbrev languages))
		     (cdr (assoc fromabbrev languages))))
	  (concat "[" from join to "]"))))))

;; alter modeline for erc-babel
(add-to-list 'mode-line-modes '(:eval (erc-babel-modeline)) t)

(defun erc-babel-toggle nil
  "Toggle automatic translation on/off." 
  (interactive)
  (if erc-babel-enable
      (progn (message "erc-babel disabled")
	     (setq erc-babel-enable nil))
    (message "erc-babel enabled")
    (setq erc-babel-enable t))
  (force-mode-line-update t))

(defun erc-babel-include-orig-toggle nil
  "Toggle inclusion of original text in translations." 
  (interactive)
  (if erc-babel-include-orig
      (progn (message "erc-babel : not including original messages")
             (setq erc-babel-include-orig nil))
    (message "erc-babel : including original messages")
    (setq erc-babel-include-orig t)))

(defun erc-babel-set-languages (buf from to)
  "Set translation languages for `buf' for use with erc-babel-translate-msg.
`from' is the language to translate outgoing messages from, and `to' is the language to translate 
outgoing messages to (for incoming messages the roles are reversed).
 The languages should be a valid translation pair for erc-babel-backend, this can be checked by examining 
 babel-google-translations, babel-free-translations, etc.).
It will add/alter the entry in erc-babel-alist corresponding to this buffer."
  (interactive (list (buffer-name (current-buffer))
		     (ido-completing-read "From language: " 
					  (let ((backend (symbol-name (cdr (assoc erc-babel-backend babel-backends)))))
					    (mapcar (if (equal backend "google") 'car 'cdr)
						    (eval (intern-soft (concat "babel-" backend "-languages")))))
                                          nil t)
		     (ido-completing-read "To language: " 
					  (let ((backend (symbol-name (cdr (assoc erc-babel-backend babel-backends)))))
					    (mapcar (if (equal backend "google") 'car 'cdr)
						    (eval (intern-soft (concat "babel-" backend "-languages")))))
                                          nil t)))
  (let* ((backend (symbol-name (cdr (assoc erc-babel-backend babel-backends))))
	 (langsym (intern-soft (concat "babel-" backend "-languages")))
	 (languages (eval langsym))
	 (transsym (intern-soft (concat "babel-" backend "-translations")))
	 (translations (if (and transsym (listp (eval transsym))) 
			   (eval transsym)))
	 (out (if (or (not translations) (member (concat from "/" to) translations)) t))
	 (in (if (or (not translations) (member (concat to "/" from) translations)) t))
	 (from2 (if (equal backend "google") (cdr (assoc from languages)) (car (rassoc from languages))))
	 (to2 (if (equal backend "google") (cdr (assoc to languages)) (car (rassoc to languages)))))
    (setq erc-babel-alist (assq-delete-all (car (assoc buf erc-babel-alist)) erc-babel-alist))
    (if (not (equal from2 to2))
	(progn
	  (if (not out) (message (concat "Cant translate from " from " to " to " with " erc-babel-backend)))
	  (if (not in) (message (concat "Cant translate from " to " to " from " with " erc-babel-backend)))
	  (add-to-list 'erc-babel-alist (cons buf (list from2 to2 out in)))))
    (force-mode-line-update)))


(defun erc-babel-translate-outgoing-msg (msg)
  "Translate outgoing `msg' using Babel if possible. The languages to translate between are obtained 
from the element of erc-babel-alist corresponding to the current buffer."
  (if (and erc-babel-enable (not (equal (substring msg 0 1) "/")))  
      (let* ((backend (symbol-name (cdr (assoc erc-babel-backend babel-backends))))
	     (fetchfunc (intern (concat "babel-" backend "-fetch")))
	     (washfunc (intern (concat "babel-" backend "-wash")))
	     (set (assoc (buffer-name (current-buffer)) erc-babel-alist))
	     (to (car (cdr (cdr set))))
	     (from (car (cdr set)))
	     (out (car (cdr (cdr (cdr set))))))
	(if (and from to out)
	    (if erc-babel-include-orig
		(setq str (concat (babel-work msg from to fetchfunc washfunc) " (" msg ")"))
	      (setq str (babel-work msg from to fetchfunc washfunc)))))))

(defun erc-babel-translate-incoming-msg nil
  "Translate incoming ERC messages using babel if possible. The languages to translate between are obtained 
from the element of erc-babel-alist corresponding to the current buffer."
  (if erc-babel-enable
      (let* ((backend (symbol-name (cdr (assoc erc-babel-backend babel-backends))))
	     (fetchfunc (intern (concat "babel-" backend "-fetch")))
	     (washfunc (intern (concat "babel-" backend "-wash")))
	     (set (assoc (buffer-name (current-buffer)) erc-babel-alist))
	     (from (car (cdr (cdr set))))
	     (to (car (cdr set)))
	     (in (car (cdr (cdr (cdr (cdr set))))))
	     start end msg text)
	(goto-char (point-min))
	(if (and from to in (re-search-forward erc-babel-message-regexp nil t))
	    (progn 
	      (setq start (match-beginning 1))
	      (setq end (match-end 1))
	      (setq msg (match-string 1))
	      (setq text (if erc-babel-include-orig
			     (concat (babel-work msg from to fetchfunc washfunc) " (" msg ")")
			   (babel-work msg from to fetchfunc washfunc)))
	      (kill-region start end)
	      (insert text))))))

;; add translation function to erc-send-pre-hook to translate outgoing messages
(add-hook 'erc-send-pre-hook 'erc-babel-translate-outgoing-msg t)

;; add translation function to start of erc-insert-modify-hook to translate incoming messages
(add-hook 'erc-insert-modify-hook 'erc-babel-translate-incoming-msg)


(provide 'erc-babel)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "erc-babel.el" (buffer-name) (buffer-string) "update")

;;; erc-babel.el ends here

