;;; thesaurus.el --- replace a word with a synonym looked up in a web service.
;;
;; Author: Dino Chiesa, Alex Henning
;; Created: Thu, 29 Mar 2012  09:18
;; Package-Requires: ((dropdown-list "1.45"))
;; URL: http://www.emacswiki.org/cgi-bin/wiki/thesaurus.el
;; X-URL: http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=thesaurus.el
;; Version: 1.0.1
;; Keywords: thesaurus
;; License: New BSD

;;; Commentary
;;
;; This module allows a user to look up synonyms for a word in
;; a web-accessible thesaurus.
;;
;; This code started with a basic version posted without license at
;; http://alexhenning.github.com/blog/2010/11/01/synonym.el/
;;
;; I standardized the naming, re-factored, wrote some documentation,
;; introduced the dependency on dropdown-list.el, added a license,
;; and polished it.
;;
;; Right now it depends on a web service from big huge labs. It
;; is not tied to that service, and could be expanded to use other
;; services, and to even dynamically choose which service to access.
;;
;; To use, first go to http://words.bighugelabs.com/ and register (no
;; cost) to get an API key.  Then, put thesaurus.el in your emacs load
;; path and modify your .emacs to do this:
;;
;;   (require 'thesaurus)
;;   (setq thesaurus-bhl-api-key "XXXXXXXXXXXX")  ;; from registration
;;   ;; optional key binding
;;   (define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)
;;
;; This module currently relies on a BigHugeLabs thesaurus service. The
;; service is currently free, and has a limit of 10,000 lookups per
;; month. If anyone exceeds this, it shouldn't be difficult to expand
;; this module to support other online thesaurus services. Wolfram Alpha
;; is one possible option; there is a free API. I'm sure there are
;; others.
;;

;;; Revisions:
;;
;; 1.0.1  2012-March-29  Initial version
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
;;



(defcustom thesaurus-bhl-api-key nil
  "The api key for connecting to BigHugeLabs.com"
  :group 'thesaurus)

(defvar thesaurus---load-path (or load-file-name "~/thesaurus.el")
  "For internal use only. ")

(defvar thesaurus-cache-dir (file-name-directory thesaurus---load-path))
(defvar thesaurus-cache-basefilename "thesaurus.cache")

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

(defun thesaurus-cache-make ()
  (make-hash-table :test 'equal))

(defun thesaurus-cache-get (key)
  (gethash key thesaurus-cache))

(defun thesaurus-cache-put (key value)
  (puthash key value thesaurus-cache)
  (when thesaurus-can-save-cache-p (thesaurus-cache-save))
  value)

(defun thesaurus-cache-save ()
  (with-temp-buffer
    (insert (with-output-to-string (princ thesaurus-cache)))
    (write-region (point-min) (point-max) (thesaurus-cache-filename))))

(defun thesaurus-cache-load ()
  (let ((filename  (thesaurus-cache-filename)))
  (with-temp-buffer
    (insert-file-contents filename)
    (car (read-from-string (buffer-substring-no-properties
                            (point-min) (point-max)))))))

(defun thesaurus-get-buffer-for-word (word)
  "retrieve a list of synonyms for the given word, from the
web service."
  ;; could insert a choose function here
  (thesaurus-get-buffer-for-word-bhl word))

(defun thesaurus-get-buffer-for-word-bhl (word)
  "retrieve a list of synonyms for the given word, from the
BigHugeLabs web service."
  (if (not (and (boundp 'thesaurus-bhl-api-key)
                (stringp thesaurus-bhl-api-key)))
    (error "Invalid api key."))
  (url-retrieve-synchronously
   (concat "http://words.bighugelabs.com/api/2/"
           thesaurus-bhl-api-key "/" word "/")))


(defun thesaurus-dump-http-headers ()
  "In the buffer created by `url-retrieve-synchronously',
there are HTTP headers. This fn removes them. It deletes each
line until finding a blank line, which in normal HTTP signals the
end of the headers and the beginning of the message content.
"
  (while (/= (point) (line-end-position))
    (delete-region (point) (line-end-position))
    (delete-char 1))
  (delete-char 1))


(defun thesaurus-parse-one-line ()
  "Parse one line in the buffer created by `url-retrieve-synchronously'.
The format of each line is expected to be:

   form|flavor|word

where
   form = {adjective,verb,noun,etc}
   flavor  = {syn,sim,ant,rel}
   word = the actual word

"
  (let ((start (point))
        (end (line-end-position))
        s frag flav parts)

    (setq s (buffer-substring-no-properties start end)
          parts (split-string s "|"))

    (if (> (length parts) 1)
        (setq
          flav (nth 1 parts)
          frag (concat (nth 2 parts)
              (if (not (or (string= flav "syn")
                           (string= flav "sim")))
                  (concat " (" flav ")")
                ""))))

    (delete-region start end)
    (delete-char 1)
    frag))


(defun thesaurus-fetch-synonyms (word)
  "fetch synonyms for the given word, from a remote source."
  (let ((synonym-list nil)
        (buf (thesaurus-get-buffer-for-word word)))
    (with-current-buffer buf
      (rename-buffer (concat "*thesaurus* - " word) t)
      (goto-char (point-min))
      (thesaurus-dump-http-headers)
      (while (not (= (point-min) (point-max)))
        (let ((elt (thesaurus-parse-one-line)))
          (if elt
              (add-to-list 'synonym-list elt)))))
    (kill-buffer buf)
    (nreverse synonym-list)))


;;;###autoload
(defun thesaurus-get-synonyms (word)
  "retrieve synonyms for the given word, either from the cache,
or, if there is no cache hit, then from the remote service.
"
  (or (thesaurus-cache-get word)
      (thesaurus-cache-put word (thesaurus-fetch-synonyms word))))


(defun thesaurus-prompt-user-with-choices (candidates)
  "prompt the user with the available replacement choices.
In this context the list of choices is the list of synonyms.
"
  (let ((choice-n (dropdown-list candidates)))
    (if choice-n
        (nth choice-n candidates)
      (keyboard-quit))))


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
  (let ((chosen (thesaurus-prompt-user-with-choices (thesaurus-get-synonyms word))))
    (if chosen
        (progn
          (goto-char (car thesaurus-bounds-of-looked-up-word))
          (delete-region (car thesaurus-bounds-of-looked-up-word)
                         (cdr thesaurus-bounds-of-looked-up-word))
          (insert chosen)))))


(defun thesaurus-install ()
  "install `thesaurus.el'"
  (setq thesaurus-cache
        (if (and (file-exists-p (thesaurus-cache-filename))
                 thesaurus-can-save-cache-p)
            (thesaurus-cache-load)
          (thesaurus-cache-make))))


(require 'dropdown-list)
(thesaurus-install)

(provide 'thesaurus)

;;; thesaurus.el ends here
