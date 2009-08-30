;;; browse-urn.el --- Functionality for browsing URNs

;; Copyright (C) 2006 Felix E. Klee <felix.klee@inka.de>

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This package provides functionality for browsing URNs - Uniform Resource
;; Names as defined in <urn:ietf:rfc:2141>.
 
;; The EMACS 21.4.2 packages browse-url.el and thingatpt.el served as a basis
;; for this mode, i.e. code was taken from these packages and modified.

;;; Example installation:

;; 1. Put this package into a directory in your EMACS load path and optionally
;;   compile it.

;; 2. Load the package in your "~/.emacs" and set up key bindings:

;;     (load "browse-urn")
;;     (define-key global-map [(control q)] 'browse-urn-at-point)
;;     (define-key global-map [(control shift q)] 'browse-urn)

;; 3. Customize the package (customization group: browse-urn).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defgroup browse-urn nil
  "Use a function to resolve a URN."
  :prefix "browse-urn-"
  :link '(emacs-commentary-link "browse-urn")
  :group 'hypermedia)

(defcustom browse-urn-function '(("urn:ietf:rfc" . find-file-urn:ietf:rfc))
  "*List of pairs \(REGEXP . FUNCTION) describing how to handle which URN.  The
function called will be the one associated with the first REGEXP which matches
the current URN.  The function is passed the URN."
  :type '(alist :tag "Regexp/function association list"
                :key-type regexp :value-type function)
  :group 'browse-urn)

(defcustom find-file-urn:ietf:rfc-dirname "/usr/local/share/doc/rfc"
  "*Directory containing RFC files.  This is used by `find-file-urn:ietf:rfc'."
  :type 'string
  :group 'browse-urn)

(defun find-file-urn:ietf:rfc (urn)
  "Opens the text document associated with the given URN in the name space
urn:ietf:rfc.  See also <urn:ietf:rfc:2648>."
  (let ((file-path (concat find-file-urn:ietf:rfc-dirname "/rfc"
                           (substring urn 13) ".txt")))
    (if (file-readable-p file-path)
        (find-file-other-window file-path)
      (error "File %s cannot be read" file-path))))

(defun browse-urn-interactive-arg (prompt)
  "Read a URN from the minibuffer, prompting with PROMPT.  Default to the URN
at or before point.  If invoked with a mouse button, set point to the position
clicked first.  Return a list for use in `interactive' containing just the
URN."
  (let ((event (elt (this-command-keys) 0)))
    (and (listp event) (mouse-set-point event)))
  (list (read-string prompt (browse-urn-urn-at-point))))

;;;###autoload
(defun browse-urn (urn)
  "Ask a function to load URN.  Prompts for a URN, defaulting to the URN at or
before point.  Variable `browse-urn-function' says which function to use."
  (interactive (browse-urn-interactive-arg "URN: "))
  ;; Look down for the first match and apply the function (which might be a
  ;; lambda).
  (catch 'done
    (dolist (bf browse-urn-function)
      (when (string-match (car bf) urn)
        (funcall (cdr bf) urn)
        (throw 'done t)))
    (error "No browse-urn-function matching URN %s"
           urn)))

;;;###autoload
(defun browse-urn-at-point (&optional arg)
  "Ask a function to load URN at or before point.  Doesn't let you edit the URN
like `browse-urn'.  Variable `browse-urn-function' says which function to use."
  (interactive "P")
  (let ((urn (browse-urn-urn-at-point)))
    (if urn
        (browse-urn urn)
      (error "No URN found"))))

(defun browse-urn-urn-at-point ()
  (interactive "P")
  (let ((urn (thing-at-point 'urn)))
    (set-text-properties 0 (length urn) nil urn)
    urn))

(defvar thing-at-point-urn-regexp
  "urn:[a-zA-Z0-9 \t\n\r][a-zA-Z0-9- \t\n\r]\\{1,31\\}:\\([a-zA-Z0-9()+,.:=@;$_!*- \t\n\r]\\|%[0-9a-fA-F \t\n\r]\\{2\\}\\)*"
  "A regular expression matching a URN according to <urn:ietf:rfc:2141>.  White
space characters, never being part of a URN, are stripped before interpretation
of the URN.")

(put 'urn 'thing-at-point 'thing-at-point-urn-at-point)
(defun thing-at-point-urn-at-point ()
  "Return the URN around or before point.

Search backwards for the start of a URN ending at or after point.  If
no URN found, return nil."
  (let ((urn ""))
    (if (thing-at-point-looking-at thing-at-point-urn-regexp)
        (progn
          (setq urn (buffer-substring-no-properties (match-beginning 0)
                                                    (match-end 0)))
          ;; strip whitespace
          (while (string-match "[ \t\n\r]+" urn)
            (setq urn (replace-match "" t t urn)))
          (if (string-equal "" urn)
              nil
            urn)))))

(provide 'browse-urn)

;;; browse-urn.el ends here
