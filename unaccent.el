;;; unaccent.el --- Functions dealing with accented characters.
;;
;; Filename: unaccent.el
;; Description: Functions dealing with accented characters.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1999-2017, Drew Adams, all rights reserved.
;; Created: Fri Sep  3 11:02:14 1999
;; Version: 20.0
;; Last-Updated: Sun Jan  1 11:53:18 2017 (-0800)
;;           By: dradams
;;     Update #: 63
;; URL: http://www.emacswiki.org/unaccent.el
;; Keywords: i18n, language, accents
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   `misc-fns', `strings', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Functions dealing with accented characters.
;;
;;  New functions defined here:
;;
;;    `accented-char-p', `unaccent-char', `unaccent-region', `unaccent-word'.
;;
;;  New variable defined here:
;;
;;    `reverse-iso-chars-alist'.
;;
;;  Suggested bindings:
;;
;;   (global-set-key [(meta ?\")] 'unaccent-word)
;;   (define-key ctl-x-map [\"] 'unaccent-region)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 1999/09/03 dadams
;;     1. reverse-iso-chars-alist: defconst -> defvar.
;;     2. unaccent-word: defsubst -> defun.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'strings nil t) ;; (no error if not found): region-description

;;;;;;;;;;;;;;;;;;;;;;;;


(defvar reverse-iso-chars-alist
  '(;; Trema/umlaut (äëïöü) (ÄËÏÖÜ)
    (?\344 . ?a)(?\353 . ?e)(?\357 . ?i)(?\366 . ?o)(?\374 . ?u)
    (?\304 . ?A)(?\313 . ?E)(?\317 . ?I)(?\326 . ?O)(?\334 . ?U)
    ;; Circumflex (âêîôû) (ÂÊÎÔÛ)
    (?\342 . ?a)(?\352 . ?e)(?\356 . ?i)(?\364 . ?o)(?\373 . ?u)
    (?\302 . ?A)(?\312 . ?E)(?\316 . ?I)(?\324 . ?O)(?\333 . ?U)
    ;; Grave (àèìòù) (ÀÈÌÒÙ)
    (?\340 . ?a)(?\350 . ?e)(?\354 . ?i)(?\362 . ?o)(?\371 . ?u)
    (?\300 . ?A)(?\310 . ?E)(?\314 . ?I)(?\322 . ?O)(?\331 . ?U)
    ;; Acute (áéíóúý) (ÁÉÍÓÚÝ)
    (?\341 . ?a)(?\351 . ?e)(?\355 . ?i)(?\363 . ?o)(?\372 . ?u)(?\375 . ?y)
    (?\301 . ?A)(?\311 . ?E)(?\315 . ?I)(?\323 . ?O)(?\332 . ?U)(?\335 . ?Y)
    (?\347 . ?c)(?\307 . ?C)            ; Cedilla (çÇ)
    ;; Tilde (ñãõÑÃÕ)
    (?\361 . ?n)(?\343 . ?a)(?\365 . ?o)(?\321 . ?N)(?\303 . ?A)(?\325 . ?O)
    (?\337 . "ss")                      ; S-zed (Beta) (ß)
    (?\253 . ?")(?\273 . ?")            ; Guillemets -> double quotes («»)
    (?\346 . "ae")(?\306 . "AE")        ; ae, AE (æÆ)
    (?\370 . ?o)(?\330 . ?O)            ; Slashed O (øØ)
    (?\260 . ?@)(?\345 . ?a)(?\305 . ?A) ; Angstrom (degree) (°åÅ)
    (?\277 . ??)                        ; Upside-down question mark (¿)
    (?\241 . ?!)                        ; Upside-down exclamation mark (¡)
    ))

;;;###autoload
(defun unaccent-word (num)
  "Move curseur forward NUM (prefix arg) words, removing accents.
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!."
  (interactive "p")
  (let ((start (point)))
    (forward-word num)
    (unaccent-region start (point) nil)))

;;;###autoload
(defun unaccent-region (start end display-msgs)
  "Replace accented chars between START and END by unaccented chars.
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!.
When called from a program, third arg DISPLAY-MSGS non-nil means to
display in-progress messages."
  (interactive "r\nd")                  ; Display-msgs non-nil => interactive-p
  (when (> start end)
    (let ((temp end))
      (setq end start)
      (setq start temp)))
  (when display-msgs
    (if (fboundp 'region-description)
        (message
         (region-description
          120
          "Removing accents in region:   -||  " "  ||-   ...      " start end))
      (message "Removing accents in region...")))
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (unaccent-char)
      (forward-char)))
  (when display-msgs
    (if (fboundp 'region-description)
        (message
         (region-description
          120
          "Removing accents in region:   -||  " "  ||-   ... done." start end))
      (message "Removing accents in region...done"))))

(defsubst accented-char-p (char)
  "Non-nil iff CHAR is an accented character."
  (and (>= char ?\240)(<= char ?\377))) ; SPC <= char <= ÿ

;;;###autoload
(defun unaccent-char ()
  "Replace accented char at curser by corresponding unaccented char(s).
Guillemet -> quote, degree -> @, s-zed -> ss, upside-down ?! -> ?!."
  (interactive)
  (when (accented-char-p (following-char))
    (let ((sans-accent (assoc (following-char) reverse-iso-chars-alist)))
      (delete-char 1)
      (insert (cdr sans-accent))
      (backward-char))))


;;;;;;;;;;;;;;;;;;;;;;;

(provide 'unaccent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unaccent.el ends here
