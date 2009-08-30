;;; completion-ui-more-sources.el --- Completion-UI more completion sources
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;;     except where specified otherwise
;; Maintainer: Henry G. Weller
;;
;; Created: Tue Mar 17 10:48:35 2009 (+0000)
;; Version: 0.1
;; Last-Updated: Tue Mar 17 21:45:10 2009 (+0000)
;;           By: Henry Weller
;;     Update #: 1
;; URL: http://www.emacswiki.org/emacs/completion-ui-more-sources.el
;; Keywords: completion, ui, user interface, sources
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; ----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; ----------------------------------------------------------------------------
;;; Change Log:
;;
;; Version 0.1
;; * initial release
;;
;; ----------------------------------------------------------------------------
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
;; ----------------------------------------------------------------------------
;;; Code:

(provide 'completion-ui-more-sources)

(require 'completion-ui)
(require 'thingatpt-ext)

;; ----------------------------------------------------------------------------
;;; Dabbrev-expand
;; by StianSelnes updated by TobyCubitt:
;; http://www.emacswiki.org/emacs/CompletionUI

(require 'dabbrev)

(defun dabbrev--wrapper-ordered (prefix &optional maxnum)
  "A wrapper for dabbrev that returns a list of expansions of
  PREFIX ordered in the same way dabbrev-expand find expansions.
  First, expansions from the current point and up to the beginning
  of the buffer is listed. Second, the expansions from the current
  point and down to the bottom of the buffer is listed. Last,
  expansions in other buffers are listed top-down. The returned
  list has at most MAXNUM elements."
  (dabbrev--reset-global-variables)
  (let ((all-expansions nil)
        (i 0)
        (ignore-case nil)
        expansion)
    ;; Search backward until we hit another buffer or reach max num
    (save-excursion
      (while (and (or (null maxnum) (< i maxnum))
                  (setq expansion (dabbrev--find-expansion prefix 1 ignore-case))
                  (not dabbrev--last-buffer))
        (setq all-expansions (nconc all-expansions (list expansion)))
        (setq i (+ i 1))))
    ;; If last expansion was found in another buffer, remove of it from the
    ;; dabbrev-internal list of found expansions so we can find it when we
    ;; are supposed to search other buffers.
    (when (and expansion dabbrev--last-buffer)
      (setq dabbrev--last-table (delete expansion dabbrev--last-table)))
    ;; Reset to prepeare for a new search
    (let ((table dabbrev--last-table))
      (dabbrev--reset-global-variables)
      (setq dabbrev--last-table table))
    ;; Search forward in current buffer and after that in other buffers
    (save-excursion
      (while
          (and (or (null maxnum) (< i maxnum))
               (setq expansion (dabbrev--find-expansion prefix -1 ignore-case)))
        (setq all-expansions (nconc all-expansions (list expansion)))
        (setq i (+ i 1))))
    all-expansions))

(completion-ui-register-source
 (lambda (prefix)
   (dabbrev--wrapper-ordered prefix))
 :name 'dabbrev-ordered)

(completion-ui-register-source
 (lambda (prefix)
   (dabbrev--wrapper-ordered prefix))
 :word-thing 'symbol
 :name 'dabbrev-symbol)

;; ----------------------------------------------------------------------------
;;; Ispell  spelling correction

(require 'flyspell)

(defgroup completion-ui-ispell nil
  "Completion-UI ispell completion source."
  :group 'completion-ui)

(defcustom completion--ispell-sort-corrections t
  "When non-nil, `complete-ispell' will sort ispell suggestions."
  :group 'completion-ui-ispell
  :type 'boolean)

(defun completion--ispell-wrapper (word)
  (let (poss ; possibilities offered by ispell
        ispell-filter)
    ;; Now check spelling of word.
    (ispell-send-string "%\n") ; put in verbose mode
    (ispell-send-string (concat "^" word "\n")) ; lookup the word
    ;; Wait until ispell has processed word.
    (while (progn
             (accept-process-output ispell-process)
             (not (string= "" (car ispell-filter)))))
    ;; Remove leading empty element
    (setq ispell-filter (cdr ispell-filter))
    ;; ispell process should return something after word is sent.
    ;; Tag word as valid (i.e., skip) otherwise
    (or ispell-filter
        (setq ispell-filter '(*)))
    (when (consp ispell-filter)
      (setq poss (ispell-parse-output (car ispell-filter))))
    (cond
     ((or (eq poss t) (stringp poss))
      (message "Ispell: %s is correct" word)
      nil)
     ((null poss)
      (error "Ispell: error in Ispell process")
      nil)
     (t
      ;; The word is incorrect, we have to propose replacements
      (if completion--ispell-sort-corrections
          (sort (car (cdr (cdr poss))) 'string<)
        (car (cdr (cdr poss))))))))

(completion-ui-register-source
 (lambda (prefix)
   (completion--ispell-wrapper prefix))
 :word-thing 'word-regexp
 :non-prefix-completion t
 :name 'ispell)

;; ----------------------------------------------------------------------------
;;; Ispell lookup and sub-string lookup

(require 'ispell)

(defun completion--ispell-lookup-wrapper (prefix &optional interior-frag)
  "Try to complete the prefix or regexp.
If optional INTERIOR-FRAG is non-nil then the prefix may be a character
sequence inside of a word."
  (let ((case-fold-search-val case-fold-search)
        (completions
         (lookup-words
          (concat (and interior-frag "*") prefix
                  (if (or interior-frag (null ispell-look-p)) "*"))
          ispell-complete-word-dict)))
    (cond ((null completions)
           (message "Ispell: no match for \"%s\"" prefix)
           completions)
          (t                            ; There are matches
           (setq case-fold-search nil)  ; Try and respect case of word.
           (cond
            ((string-equal (upcase prefix) prefix)
             (setq completions (mapcar 'upcase completions)))
            ((eq (upcase (aref prefix 0)) (aref prefix 0))
             (setq completions
                   (mapcar (function
                            (lambda (pos)
                              (if (eq (aref prefix 0) (aref pos 0)) pos
                                (capitalize pos))))
                           completions))))
           (setq case-fold-search case-fold-search-val)
           ))
    completions))

(completion-ui-register-source
 (lambda (prefix)
   (completion--ispell-lookup-wrapper prefix nil))
 :word-thing 'word-regexp
 :non-prefix-completion t
 :name 'ispell-lookup)

(completion-ui-register-source
 (lambda (prefix)
   (completion--ispell-lookup-wrapper prefix t))
 :word-thing 'word-regexp
 :non-prefix-completion t
 :name 'ispell-lookup-substring)

;; ----------------------------------------------------------------------------
;;; BBDB Email address lookup/completion

(require 'bbdb)

(defun completion--bbdb-wrapper (orig)
  "Generate completions from the bbdb for completion-ui.
To be used as a `completion--wrapper'."
  (require 'bbdb)
  (let* ((end (point))
         (beg (save-excursion
                (backward-char (length orig))
                (point)))
         (typed (downcase orig))
         (pattern (bbdb-string-trim typed))
         (ht (bbdb-hashtable))
         ;;(ht (bbdb-with-db-buffer (bbdb-records nil t) bbdb-hashtable))
         ;; Make a list of possible completion strings (all-the-completions)
         (only-one-p t)
         (all-the-completions nil)
         (pred
          (lambda (sym)
            (when (bbdb-completion-predicate sym)
              (or
               ;; not sure about this. more than one record
               ;; attached to the symbol? does that happen?
               (> (length (symbol-value sym)) 1)
               ;; this is the doozy, though. multiple syms
               ;; which all match the same record
               (delete t (mapcar (lambda(x)
                                   (equal (symbol-value x)
                                          (symbol-value sym)))
                                 all-the-completions)))
              (if (not (memq sym all-the-completions))
                  (setq all-the-completions (cons sym all-the-completions))))))
         (completion (progn (all-completions pattern ht pred)
                            (try-completion pattern ht)))
         (exact-match (eq completion t)))
    ;; Check if any completions were found
    (cond
     ;; No matches found
     ((null completion)
      (message "No completions")
      completion)

     ;; Exact match to one or more records
     (t
      (message "One or more completions")
      (let (dwim-completions uniq nets net name lfname akas)
        ;; Now collect all the dwim-addresses for each completion, but only
        ;; once for each record!  Add it if the net is part of the completions
        (bbdb-mapc
         (lambda (sym)
           (bbdb-mapc
            (lambda (rec)
              (when (not (member rec uniq))
                (setq uniq (cons rec uniq)
                      nets (bbdb-record-net rec)
                      name (downcase (or (bbdb-record-name rec) ""))
                      lfname (downcase (or (bbdb-record-lfname rec) ""))
                      akas (mapcar 'downcase (bbdb-record-aka rec)))
                (while nets
                  (setq net (car nets))
                  (when (cond
                         ;; primary
                         ((and (member bbdb-completion-type
                                       '(primary primary-or-name))
                               (member (intern-soft (downcase net) ht)
                                       all-the-completions))
                          (setq nets nil)
                          t)
                         ;; name
                         ((and name (member bbdb-completion-type
                                            '(nil name primary-or-name))
                               (let ((cname (symbol-name sym)))
                                 (or (string= cname name)
                                     (string= cname lfname)
                                     (member cname akas))))
                          (setq name nil)
                          t)
                         ;; net
                         ((and (member bbdb-completion-type
                                       '(nil net))
                               (member (intern-soft (downcase net) ht)
                                       all-the-completions)))
                         ;; (name-or-)primary
                         ((and (member bbdb-completion-type
                                       '(name-or-primary))
                               (let ((cname (symbol-name sym)))
                                 (or (string= cname name)
                                     (string= cname lfname)
                                     (member cname akas))))
                          (setq nets nil)
                          t)
                         )
                    (setq dwim-completions
                          (cons (bbdb-dwim-net-address rec net)
                                dwim-completions))
                    (if exact-match (setq nets nil)))
                  (setq nets (cdr nets)))))
            (symbol-value sym)))
         all-the-completions)
        dwim-completions)))))

(completion-ui-register-source
 (lambda (prefix)
   (completion--bbdb-wrapper prefix))
 :word-thing 'filename
 :non-prefix-completion t
 :name 'bbdb)

;; ----------------------------------------------------------------------------
;;; Pcomplete completion

(require 'pcomplete)

(defun completion--pcomplete-prefix-wrapper ()
  "Get the prefix using `pcomplete-parse-arguments'."
  (when (pcomplete-parse-arguments)
    (car (last (split-string (pcomplete-arg 'last) "/")))))

(defun completion--pcomplete-wrapper (prefix)
  "Wrapper around `pcomplete-completions' to use as a `completion--wrapper'."

  ;; Delete the current tentative completion to avoid it being picked-up as
  ;; part of the prefix when `pcomplete-completion-function' is called by
  ;; `completion-show-browser-menu'.
  (let ((overlay (completion-ui-overlay-at-point)))
    (when overlay
      (delete-region (overlay-start overlay) (overlay-end overlay))
      ))

  ;; Get the completions trapping the exception if necessary
  (let (completions)
    (catch 'pcompleted
      (setq completions (pcomplete-completions)))
    completions))

(completion-ui-register-source
 (lambda (prefix)
   (completion--pcomplete-wrapper prefix))
 :word-thing 'filename
 :prefix-function completion--pcomplete-prefix-wrapper
 :name 'pcomplete)

;; ----------------------------------------------------------------------------
;;; Org-mode completion (experimental hack)

(require 'org)

(defvar completion--org-completions nil
  "Current list of completions used by completion-ui in org-mode
to transfer the completions generated in `completion--org-prefix-wrapper'
to `completion--org-wrapper'.")

(defun completion--org-prefix-wrapper ()
  "Substantially `org-complete', please see original documentation for details.
Unique completions are handle by the original method but multiple choices are
transfered to `completion-ui'"
  (setq completion--org-completions nil)
  (org-without-partial-completion
   (catch 'exit
     (let* ((a nil)
            (end (point))
            (beg1 (save-excursion
                    (skip-chars-backward (org-re "[:alnum:]_@"))
                    (point)))
            (beg (save-excursion
                   (skip-chars-backward "a-zA-Z0-9_:$")
                   (point)))
            (confirm (lambda (x) (stringp (car x))))
            (searchhead (equal (char-before beg) ?*))
            (struct
             (when (and (member (char-before beg1) '(?. ?<))
                        (setq a (assoc (buffer-substring beg1 (point))
                                       org-structure-template-alist)))
               (org-complete-expand-structure-template (1- beg1) a)
               (throw 'exit t)))
            (tag (and (equal (char-before beg1) ?:)
                      (equal (char-after (point-at-bol)) ?*)))
            (prop (and (equal (char-before beg1) ?:)
                       (not (equal (char-after (point-at-bol)) ?*))))
            (texp (equal (char-before beg) ?\\))
            (link (equal (char-before beg) ?\[))
            (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
                                          beg)
                        "#+"))
            (startup (string-match "^#\\+STARTED:.*"
                                   (buffer-substring (point-at-bol) (point))))
            (completion-ignore-case opt)
            (type nil)
            (tbl nil)
            (table (cond
                    (opt
                     (setq type :opt)
                     (require 'org-exp)
                     (append
                      (mapcar
                       (lambda (x)
                         (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                         (cons (match-string 2 x) (match-string 1 x)))
                       (org-split-string (org-get-current-options) "\n"))
                      (mapcar 'list org-additional-option-like-keywords)))
                    (startup
                     (setq type :startup)
                     org-startup-options)
                    (link (append org-link-abbrev-alist-local
                                  org-link-abbrev-alist))
                    (texp
                     (setq type :tex)
                     org-html-entities)
                    ((string-match "\\`\\*+[ \t]+\\'"
                                   (buffer-substring (point-at-bol) beg))
                     (setq type :todo)
                     (mapcar 'list org-todo-keywords-1))
                    (searchhead
                     (setq type :searchhead)
                     (save-excursion
                       (goto-char (point-min))
                       (while (re-search-forward org-todo-line-regexp nil t)
                         (push (list
                                (org-make-org-heading-search-string
                                 (match-string 3) t))
                               tbl)))
                     tbl)
                    (tag (setq type :tag beg beg1)
                         (or org-tag-alist (org-get-buffer-tags)))
                    (prop (setq type :prop beg beg1)
                          (mapcar 'list (org-buffer-property-keys nil t t)))
                    (t (progn
                         (call-interactively org-completion-fallback-command)
                         (throw 'exit nil)))))
            (pattern (buffer-substring-no-properties beg end))
            (completion (try-completion pattern table confirm)))
       (cond ((eq completion t)
              (if (not (assoc (upcase pattern) table))
                  (message "Already complete")
                (if (and (equal type :opt)
                         (not (member (car (assoc (upcase pattern) table))
                                      org-additional-option-like-keywords)))
                    (insert (substring (cdr (assoc (upcase pattern) table))
                                       (length pattern)))
                  (if (memq type '(:tag :prop)) (insert ":")))))
             ((null completion)
              (message "Can't find completion for \"%s\"" pattern)
              (ding))
             ((not (string= pattern completion))
              (delete-region beg end)
              (if (string-match " +$" completion)
                  (setq completion (replace-match "" t t completion)))
              (insert completion)
              (if (assoc completion table)
                  (if (eq type :todo) (insert " ")
                    (if (memq type '(:tag :prop)) (insert ":"))))
              (if (and (equal type :opt) (assoc completion table))
                  (message
                   "%s"
                   (substitute-command-keys
                    "Press \\[org-complete] again to insert example settings"
                    ))))
             (t
              (setq completion--org-completions
                    (sort (all-completions pattern table confirm) 'string<))))
       ;;(set-text-properties 0 (length pattern) nil pattern)
       pattern))))

(defun completion--org-wrapper (prefix)
  completion--org-completions)

(completion-ui-register-source
 (lambda (prefix)
   (completion--org-wrapper prefix))
 :prefix-function completion--org-prefix-wrapper
 :name 'org)

;; ----------------------------------------------------------------------------
;;; Anything sources

(defmacro completion-ui-anything-register-source (source)
  "Register the `anything' SOURCE as a Completion-UI source
by creating the appropriate wrapping for `completion-ui-register-source'.

The source name is set to SOURCE less `anything-c-source-'.

The completion function name is set to `complete-' + the name of the source.

The appropriate `word-thing' is assumed to be `word-regexp' consistent with the
normal `anything' interface."
  `(completion-ui-register-source
    (lambda (prefix)
      (require 'anything-config-ext)
      (setq anything-pattern prefix)
      (anything-aif (assoc-default 'init ,source)
          (anything-funcall-with-source ,source it))
      (anything-compute-matches ,source))
    :word-thing 'word-regexp
    :name ,(intern (let ((source-name (symbol-name source)))
                     (if (string-match "^anything-c-source-+" source-name)
                         (substring source-name (match-end 0))
                       source-name)))))

(completion-ui-anything-register-source anything-c-source-file-name-history)
(completion-ui-anything-register-source anything-c-source-files-in-current-dir)
(completion-ui-anything-register-source anything-c-source-file-journal)
(completion-ui-anything-register-source anything-c-source-emacs-functions)
(completion-ui-anything-register-source anything-c-source-emacs-variables)

;; ----------------------------------------------------------------------------
;;; completion-ui-more-sources.el ends here
