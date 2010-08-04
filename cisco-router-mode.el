;;; cisco-router-mode.el --- Major mode for editing Cisco router configuration files
;;
;; Copyright (C) 2004 Noufal Ibrahim <nkv at hcoop period net>
;;
;; This program is not part of Gnu Emacs
;;
;; cisco-router-mode.el is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(defvar cisco-router-mode-hook nil
  "Hook called by \"cisco-router-mode\"")

(defvar cisco-router-mode-map
  (let 
      ((cisco-router-mode-map (make-keymap)))
    (define-key cisco-router-mode-map "\C-j" 'newline-and-indent)
    cisco-router-mode-map)
  "Keymap for Cisco router configuration major mode")

;; Font locking definitions. 
(defvar cisco-router-command-face 'cisco-router-command-face "Face for basic router commands")
(defvar cisco-router-toplevel-face 'cisco-router-toplevel-face "Face for top level commands")
(defvar cisco-router-no-face 'cisco-router-no-face "Face for \"no\"")
(defvar cisco-router-ipadd-face 'cisco-router-ipadd-face "Face for IP addresses")

(defface cisco-router-ipadd-face
  '(
    (((type tty) (class color)) (:foreground "yellow"))
    (((type graphic) (class color)) (:foreground "LightGoldenrod"))
    (t (:foreground "LightGoldenrod" ))
    )
  "Face for IP addresses")

(defface cisco-router-command-face 
  '(
    (((type tty) (class color)) (:foreground "cyan"))
    (((type graphic) (class color)) (:foreground "cyan"))
    (t (:foreground "cyan" ))
    )
  "Face for basic router commands")

(defface cisco-router-toplevel-face
  '(
    (((type tty) (class color)) (:foreground "blue"))
    (((type graphic) (class color)) (:foreground "lightsteelblue"))
    (t (:foreground "lightsteelblue" ))
    )
  "Face for basic router commands")

(defface cisco-router-no-face
  '(
    (t (:underline t))
    )
  "Face for \"no\"")


;; (regexp-opt '("interface" "ip vrf" "controller" "class-map" "redundancy" "line" "policy-map" "router" "access-list" "route-map") t)
;; (regexp-opt '("diagnostic" "hostname" "logging" "service" "alias" "snmp-server" "boot" "card" "vtp" "version" "enable") t)

(defconst cisco-router-font-lock-keywords
  (list
   '( "\\<\\(access-list\\|c\\(?:lass-map\\|ontroller\\)\\|i\\(?:nterface\\|p vrf\\)\\|line\\|policy-map\\|r\\(?:edundancy\\|oute\\(?:-map\\|r\\)\\)\\)\\>". cisco-router-toplevel-face)
   '( "\\<\\(alias\\|boot\\|card\\|diagnostic\\|^enable\\|hostname\\|logging\\|s\\(?:ervice\\|nmp-server\\)\\|v\\(?:ersion\\|tp\\)\\)\\>" . cisco-router-command-face)
   '("\\<\\(no\\)\\>" . cisco-router-no-face)
   '("\\<\\([0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\)\\>" . cisco-router-ipadd-face)
   )
  "Font locking definitions for cisco router mode")

;; Imenu definitions. 
(defvar cisco-router-imenu-expression
  '(
    ("Interfaces"        "^[\t ]*interface *\\(.*\\)" 1)
    ("VRFs"              "^ip vrf *\\(.*\\)" 1)
    ("Controllers"       "^[\t ]*controller *\\(.*\\)" 1)
    ("Routing protocols" "^router *\\(.*\\)" 1)
    ("Class maps"        "^class-map *\\(.*\\)" 1)
    ("Policy maps"       "^policy-map *\\(.*\\)" 1)
    ))
  
;; Indentation definitions.
(defun cisco-router-indent-line ()
  "Indent current line as cisco router config line"
  (let ((indent0 "^interface\\|redundancy\\|^line\\|^ip vrf \\|^controller\\|^class-map\\|^policy-map\\|router\\|access-list\\|route-map")
	(indent1 " *main-cpu\\| *class\\W"))
    (beginning-of-line)
    (let ((not-indented t)
	  (cur-indent 0))
      (cond ((or (bobp) (looking-at indent0) (looking-at "!")) ; Handles the indent0 and indent1 lines
;	     (message "Indent0")
	     (setq not-indented nil
		   cur-indent 0))
	    ((looking-at indent1)
;	     (message "Indent1")
	     (setq not-indented nil
		   cur-indent 1)))
      (save-excursion ; Indents regular lines depending on the block they're in.
	(while not-indented
	  (forward-line -1)
	  (cond ((looking-at indent1)
;		 (message "Indent1 block")
		 (setq cur-indent 2
		       not-indented nil))
		((looking-at indent0)
;		 (message "Indent0 block")
		 (setq cur-indent 1
		       not-indented nil))
		((looking-at "!")
;		 (message "Reached !")
		 (setq cur-indent 0
		       not-indented nil))
		((bobp) 
;		 (message "Buffer beginning reached")
		 (setq cur-indent 0
		       not-indented nil)))))
      (indent-line-to cur-indent))))


;; Custom syntax table
(defvar cisco-router-mode-syntax-table (make-syntax-table) 
  "Syntax table for cisco router mode")

(modify-syntax-entry ?_ "w" cisco-router-mode-syntax-table) ;All _'s are part of words. 
(modify-syntax-entry ?- "w" cisco-router-mode-syntax-table) ;All -'s are part of words. 
(modify-syntax-entry ?! "<" cisco-router-mode-syntax-table) ;All !'s start comments. 
(modify-syntax-entry ?\n ">" cisco-router-mode-syntax-table) ;All newlines end comments.
(modify-syntax-entry ?\r ">" cisco-router-mode-syntax-table) ;All linefeeds end comments.

;; Entry point
(defun cisco-router-mode  ()
  "Major mode for editing Cisco router configuration files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table cisco-router-mode-syntax-table)
  (use-local-map cisco-router-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(cisco-router-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'cisco-router-indent-line)
  (set (make-local-variable 'comment-start) "!")
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)!+ *")
  (setq imenu-case-fold-search nil)  
  (set (make-local-variable 'imenu-generic-expression) cisco-router-imenu-expression)
  (imenu-add-to-menubar "Imenu")
  (setq major-mode 'cisco-router-mode
	mode-name "Cisco router configuration")
  (run-hooks cisco-router-mode-hook))

(add-to-list 'auto-mode-alist '("\\.cfg\\'" . cisco-router-mode))

(provide 'cisco-router-mode)

;;; cisco-router-mode.el ends here
