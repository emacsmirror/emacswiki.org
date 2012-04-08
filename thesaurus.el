;;; thesaurus.el --- replace a word with a synonym looked up in a web service.
;;
;; Author: Dino Chiesa, Alex Henning
;; Created: Thu, 29 Mar 2012  09:18
;; Package-Requires: ()
;; URL: http://www.emacswiki.org/cgi-bin/wiki/thesaurus.el
;; X-URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=thesaurus.el
;; Version: 2012.4.7
;; Keywords: thesaurus synonym
;; License: New BSD

;;; Commentary:

;; This module allows a user to look up synonyms for a word in
;; a web-accessible thesaurus.

;; This code started with a basic version posted without license at
;; http://alexhenning.github.com/blog/2010/11/01/synonym.el/

;; I standardized the naming, re-factored, wrote some documentation,
;; introduced the dependency on dropdown-list.el and x-popup-menu, added
;; a license, introduced some error handling, and polished it.

;; Right now it depends on a web service from Big Huge Labs. It
;; is not tied to that service, and could be expanded to use other
;; services, and to even dynamically choose which service to access.

;; To use, first go to http://words.bighugelabs.com/ and register (no
;; cost) to get an API key.  Then, put thesaurus.el in your emacs load
;; path and modify your .emacs to do this:

;;   (require 'thesaurus)
;;   (setq thesaurus-bhl-api-key "XXXXXXXXXXXX")  ;; from registration
;;   ;; optional key binding
;;   (define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

;; This module currently relies on a BigHugeLabs thesaurus service. The
;; service is currently free, and has a limit of 10,000 lookups per
;; day. If the service changes, or becomes unavailable, or if anyone
;; exceeds the limit, it shouldn't be difficult to expand this module to
;; support other online thesaurus services. Wolfram Alpha is one
;; possible option; there is a free API. Surely there are others.
;;

;;; Revisions:
;;
;; 2012.4.7  2012-April-07 Dino Chiesa  PENDING
;;    Fixup the customization group.
;;    Also serialize and de-serialize the cache as a list, not as a
;;    hash. To avoid the problem reported by Takafumi Arakaki. (Thanks!)
;;
;; 1.0.4  2012-March-30  Dino Chiesa
;;    use message-box to notify users when they need to acquire an api
;;    key, and pop a browser to do so. Use a special message-box on
;;    Windows.  Also, handle the case where no synonyms are found - for
;;    example, when looking up a mis-spelled or non-existent word.
;;
;; 1.0.3  2012-March-30  Dino Chiesa
;;    include x-popup-menu as the default prompting mechanism.
;;    dropdown-list is sort of a pain to work with. I don't know
;;    what this means for tty users.
;;
;; 1.0.2  2012-March-29  Dino Chiesa
;;    tiny doc change
;;
;; 1.0.1  2012-March-29  Dino Chiesa
;;    Initial version
;;

;;; License
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2008-2012, Dino Chiesa
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;


(defgroup thesaurus nil
  "Provides a facility to look up synonyms."
  :group 'Editing)

(defcustom thesaurus-bhl-api-key nil
  "The api key for connecting to BigHugeLabs.com.

Get one by visiting  http://words.bighugelabs.com/getkey.php

"
  :type 'string
  :group 'thesaurus)

(defcustom thesaurus-prompt-mechanism 'x-popup-menu
  "The mechanism used to prompt the user for his choice of
synonym. Options: 'x-popup-menu, or 'dropdown-list.  When setting
this, set it to the symbol, not to the string or the actual
function.  Eg

  (setq thesaurus-prompt-mechanism 'x-popup-menu)

"
  :type 'symbol
  :options '('x-popup-menu 'dropdown-list)
  :group 'thesaurus)

(defvar thesaurus---load-path (or load-file-name "~/thesaurus.el")
  "For internal use only. ")

(defvar thesaurus-cache-dir (file-name-directory thesaurus---load-path))
(defvar thesaurus-cache-basefilename ".thesaurus.cache")

(defvar thesaurus-can-save-cache-p (or
       (and (>= emacs-major-version 23)
            (>= emacs-minor-version 1)
            (null (string-match "23.1.1" (version))))
       (> emacs-major-version 23))
  "Whether it is possible to save the cache")

(defvar thesaurus-cache nil)
(defvar thesaurus-bounds-of-looked-up-word nil)

(defun thesaurus-cache-filename ()
  (concat thesaurus-cache-dir thesaurus-cache-basefilename))

(defun thesaurus-cache-initialize ()
  (make-hash-table :test 'equal))

(defun thesaurus-cache-get (key)
  (gethash key thesaurus-cache))

(defun thesaurus-cache-put (key value)
  (when value
    (puthash key value thesaurus-cache)
    ;; saving the cache may get expensive as it gets larger
    (thesaurus-cache-save))
  value)


(defun thesaurus-hashtable-to-alist (hash)
  "Return an association-list representation of the hashtable HASH."
   (let ((alist nil))
     (maphash
      (lambda (key value)
        (setq alist (cons (cons key value) alist)))
      hash)
     alist))


(defun thesaurus-hashtable-from-alist (alist &rest options)
  "Build a hashtable from the values in the association list ALIST."
  (let ((ht (apply 'make-hash-table options)))
    (mapc
     (lambda (kv-pair) (puthash (car kv-pair) (cdr kv-pair) ht))
     alist)
     ht))

(defun thesaurus-cache-save ()
  (with-temp-buffer
    (let (print-level print-length)
      (insert (pp-to-string (thesaurus-hashtable-to-alist thesaurus-cache)))
      (write-region (point-min) (point-max) (thesaurus-cache-filename)))))


(defun thesaurus-cache-load ()
  (thesaurus-hashtable-from-alist
   (with-temp-buffer
     (insert-file-contents (thesaurus-cache-filename))
     (car (read-from-string (buffer-substring-no-properties
                             (point-min) (point-max)))))
   :test 'equal))


(defun thesaurus-get-buffer-for-word (word)
  "Retrieve a list of synonyms for the given word, from the
web service."
  ;; could insert a chooser function here
  (thesaurus-get-buffer-for-word-bhl word))

(defun thesaurus-msgbox (msg)
  "Display a message in a dialog box."
  (if (thesaurus-want-msgbox-via-powershell)
      (thesaurus-msgbox-via-powershell msg)
    (message-box msg)))

(defun thesaurus-path-of-powershell-exe ()
  "get location of powershell exe."
  (concat
   (or (getenv "windir") "c:\\windows")
   "\\System32\\WindowsPowerShell\\v1.0\\powershell.exe"))

(defun thesaurus-want-msgbox-via-powershell ()
  "Determine if we want to display a message box
using Windows powershell."
  (and (eq system-type 'windows-nt)
       (file-exists-p (thesaurus-path-of-powershell-exe))))


(defun thesaurus-msgbox-via-powershell (format-string &rest args)
  "Display a message box via powershell and Windows Forms.

The `message-box' fn works poorly on Windows; it does not allow
the encoding of newlines and also the UI generally looks like a
silly toy.

This can be used in place of `message-box' on Windows.

This is used within `thesaurus.el' in only one case: to notify
the user that he needs to register for an API key.

"
  (flet ((rris (a1 a2 s) (replace-regexp-in-string a1 a2 s)))
    (let* ((msg (format format-string args))
           (ps-cmd
            ;; This is a command to be passed on the cmd.exe line.
            ;; Newlines encoded as \n or `n do not display properly. This
            ;; code transforms splits the string on newlines, then joins,
            ;; using [char]0x000D as the "glue".  Also - need to perform
            ;; special escaping of single and double quotes.  All this
            ;; because we are passing a script to powershell on the
            ;; command line.
            ;;
            ;; creating a file with script code in it, then passing
            ;; that file to powershell, would avoid the need for
            ;; special escaping.  But that is not feasible, since by
            ;; default powershell prohibits executing scripts. But
            ;; powershell allows running script passed as -Command. So.
            (concat "[void][System.Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms');"
                    "[Windows.Forms.MessageBox]::Show("
                    (mapconcat '(lambda (elt)
                                  (rris (char-to-string 34)
                                        (char-to-string 39)
                                        (pp-to-string
                                         (rris (char-to-string 34)
                                               "'+[char]0x0022+'"
                                               (rris (char-to-string 39)
                                                     "'+[char]0x0027+'"
                                                     elt)
                                               ))))
                               (split-string msg "\n" nil)
                               "+[char]0x000D+")
                    ",'Message from Emacs',"
                    "[Windows.Forms.MessageBoxButtons]::OK,"
                    "[Windows.Forms.MessageBoxIcon]::Information)"))
           (shell-command
            (format "%s -Command %s"
                    (thesaurus-path-of-powershell-exe)
                    (concat "\"& {" ps-cmd "}\""))))
      (shell-command-on-region (point) (point)
                               shell-command
                               nil nil nil))))


(defun thesaurus-get-buffer-for-word-bhl (word)
  "retrieve a list of synonyms for the given word, from the
BigHugeLabs web service."
  (if (not (and (boundp 'thesaurus-bhl-api-key)
                (stringp thesaurus-bhl-api-key)))
      (let ((msg (concat "You need to get an \"api key\" from BigHugeLabs.\n"
                     "Then, set it in your .emacs with a statement like:\n"
                     "    (setq thesaurus-bhl-api-key \"XXXXXXXXXXXX\") \n")))
        (thesaurus-msgbox msg)
        (browse-url "http://words.bighugelabs.com/getkey.php")
        nil)
  (url-retrieve-synchronously
   (concat "http://words.bighugelabs.com/api/2/"
           thesaurus-bhl-api-key "/" word "/"))))



(defun thesaurus-process-http-headers ()
  "In the buffer created by `url-retrieve-synchronously',
there are HTTP headers, and content. This fn removes the headers
from the buffer, parsing the Content-Length header to verify that
a substantive response was received.

This implementation deletes each line until finding a blank line,
which in correctly-formatted HTTP messages signals the end of the
headers and the beginning of the message content.
"
  (let ((clength -1))
    (while (/= (point) (line-end-position))
      (when (and (< clength 0)
               (re-search-forward "^[Cc]ontent-[Ll]ength ?: *\\(.*\\)$" (line-end-position) t))
          (setq clength (string-to-number (match-string 1)))
          (goto-char (line-beginning-position)))
      (delete-region (point) (line-end-position))
      (delete-char 1))
    (delete-char 1)
    clength))




(defun thesaurus-parse-one-line ()
  "Parse one line in the buffer created by `url-retrieve-synchronously'.
The format of each line is expected to be:

   form|flavor|word

where
   form = {adjective,verb,noun,etc}
   flavor  = {syn,sim,ant,rel}
   word = the actual word

The return value is a list, with those three items in it,
in that order.

"
  (let (start end s parts)
    (setq start  (point)
          end (line-end-position)
          s (buffer-substring-no-properties start end)
          parts (split-string s "|"))
    (delete-region start end)
    (delete-char 1)
    parts))


(defun thesaurus-fetch-synonyms (word)
  "fetch synonyms for the given word, from a remote source."
  (let ((synonym-list nil)
        (buf (thesaurus-get-buffer-for-word word)))
    (if buf
        (progn
          (with-current-buffer buf
            (rename-buffer (concat "*thesaurus* - " word) t)
            (goto-char (point-min))
            (if (> (thesaurus-process-http-headers) 0)
                (while (not (= (point-min) (point-max)))
                  (let ((elt (thesaurus-parse-one-line)))
                    (if elt
                        (add-to-list 'synonym-list elt))))
              (message-box "No synonyms found.")))
          (kill-buffer buf)
          (nreverse synonym-list)))))


;;;###autoload
(defun thesaurus-get-synonyms (word)
  "retrieve synonyms for the given word, either from the cache,
or, if there is no cache hit, then from the remote service.
"
  (or (thesaurus-cache-get word)
      (thesaurus-cache-put word (thesaurus-fetch-synonyms word))))


(defun thesaurus-get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


(defun thesaurus--generate-menu (candidates)
  "Generate a menu suitable for use in `x-popup-dialog' from the
list of candidates. Each item in the list of candidates is a
list, (FORM FLAVOR WORD), where FORM is one of {adjective, verb,
noun, etc}, FLAVOR is {syn, sim, rel, ant, etc}, and WORD is the
actual word.

"
  (let ((items (mapcar '(lambda (elt)
                          (cons
                           (concat (nth 2 elt) " (" (nth 0 elt) ")")
                           (nth 2 elt)))
                       candidates)))

    ;; this works with x-popup-menu
    (setq items (cons "Ignored pane title" items))
    (list "Replace with..." items)))



(defun thesaurus-prompt-user-with-choices (candidates)
  "prompt the user with the available replacement choices.
In this context the list of choices is the list of synonyms.

See `thesaurus-prompt-mechanism'.

"
  (cond
   ((not candidates)
    nil)
   ((and (eq thesaurus-prompt-mechanism 'dropdown-list)
         (featurep 'dropdown-list))
    (let ((choice-n (dropdown-list (mapcar '(lambda (elt) (nth 2 elt)) candidates))))
      (if choice-n
          (nth choice-n candidates)
        (keyboard-quit))))

   (t
    ;; NB:
    ;; x-popup-menu displays in the proper location, near
    ;; the cursor.
    ;;
    ;; x-popup-dialog always displays in the center
    ;; of the frame, which makes for an annoying
    ;; user-experience.
    (x-popup-menu (thesaurus-get-menu-position)
                  (thesaurus--generate-menu candidates)))))


(defun thesaurus-word-at-point ()
  "Uses `bounds-of-thing-at-point', to find and return the word at point.

As a side effect, this fn stores the bounds of the word that is found.
This allows this module to delete the word later, when the user chooses
a replacement word.
"
  (if (get 'word 'thing-at-point)
      (funcall (get 'word 'thing-at-point))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if bounds
          (progn
            (setq thesaurus-bounds-of-looked-up-word bounds)
            (buffer-substring-no-properties (car bounds) (cdr bounds)))))))


;;;###autoload
(defun thesaurus-choose-synonym-and-replace (word)
  "The main interactive entry point into the `thesaurus.el' capability.
Invoke this interactively, and the fn will prompt the user to
confirm the word to be looked up.  It will then retrieve a list
of synonyms for the word, either from the cache or from a remote
service, and prompt the user with a list of possible
replacements.  If the user chooses a replacement, the original
word in the buffer will be removed and the replacement will be
inserted in its place.

"
  (interactive (list (read-string "word: " (thesaurus-word-at-point))))
  (let ((chosen (thesaurus-prompt-user-with-choices
                 (thesaurus-get-synonyms word))))
    (when chosen
          (goto-char (car thesaurus-bounds-of-looked-up-word))
          (delete-region (car thesaurus-bounds-of-looked-up-word)
                         (cdr thesaurus-bounds-of-looked-up-word))
          (insert chosen))))


(defun thesaurus-install ()
  "install `thesaurus.el'"
  (setq thesaurus-cache
        (if (file-exists-p (thesaurus-cache-filename))
            (thesaurus-cache-load)
          (thesaurus-cache-initialize))))


(eval-when-compile (require 'dropdown-list nil t))

(thesaurus-install)

(provide 'thesaurus)

;;; thesaurus.el ends here
