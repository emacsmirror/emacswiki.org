;;; texmate-import.el --- Import Texmate macros into yasnippet syntax
;; 
;; Filename: texmate-import.el
;; Description: Import Texmate macros into yasnippet syntax
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Oct 20 15:08:50 2010 (-0500)
;; Version: 0.1 
;; Last-Updated: Wed Oct 27 23:12:22 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 344
;; URL: http://www.emacswiki.org/emacs/texmate-import.el
;; Keywords: Yasnippet
;; Compatibility: Tested with Windows Emacs 23.2
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  This library allows you to import Texmate bundle snippets to Yasnippet
;;
;;  To use, put in a directory in the load path, like ~/elisp and put the
;;  following in ~/.emacs
;;
;;  (autoload 'texmate-import-bundle "texmate-import" "* Import TeXMate files" 't)
;;  (autoload 'texmate-import-svn-url "texmate-import" "* Import TeXMate snippets from svn.textmate.org" 't)
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 23:11:33 2010 (-0500) #342 (Matthew L. Fidler)
;;    Added fix to allow files to pass for directories in `texmate-import-bundle'
;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 15:58:57 2010 (-0500) #338 (Matthew L. Fidler)
;;    Added import from svn.texmate.org using url package.  Use `texmate-import-svn-url'
;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 14:34:53 2010 (-0500) #259 (Matthew L. Fidler)
;;    Added a guess-mode function to take out prompting for modes.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 10:17:48 2010 (-0500) #110 (Matthew L. Fidler)
;;    Bug fix for .yas-parents.
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 10:12:22 2010 (-0500) #97 (Matthew L. Fidler)
;;    Changed import rmate and stata to mirror new texmate-import function
;; 25-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Oct 25 09:59:31 2010 (-0500) #94 (Matthew L. Fidler)
;;    Changed parent-mode to a prompt and uses .yas-parents as in SVN trunk of yasnippet.
;; 22-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Oct 22 09:42:57 2010 (-0500) #82 (Matthew L. Fidler)
;;    Bugfix for ${1:default} expressions
;; 22-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Oct 22 09:34:06 2010 (-0500) #79 (Matthew L. Fidler)
;;    Added ability to choose mode by function or mode-name
;; 21-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 21 16:10:52 2010 (-0500) #61 (Matthew L. Fidler)
;;    Selected text bugfix
;; 21-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 21 15:54:16 2010 (-0500) #56 (Matthew L. Fidler)
;;    Now handles key-bindings as well.
;; 21-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 21 13:34:30 2010 (-0500) #26 (Matthew L. Fidler)
;;    Added a fix to take out spaces in texmate bundles file name translations.
;; 21-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 21 13:29:00 2010 (-0500) #19 (Matthew L. Fidler)
;;
;;    Updated import to find groupings before or after orderings in
;;    the info.plist.
;;
;; 21-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 21 09:05:30 2010 (-0500) #9 (Matthew L. Fidler)
;;
;;    Added a yas/root-directory of the current directory if
;;    undefined.  Allows to be run from the command line by just
;;    loading this file
;;
;; 21-Oct-2010    Matthew L. Fidler  
;;    Added optional transformation function.
;; 20-Oct-2010    Matthew L. Fidler  
;;    Bug fix -- added mode.
;; 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:
(require 'yasnippet nil 't)
(require 'url)
(if (not (boundp 'yas/root-directory))
    (setq yas/root-directory "./") ; Should already be defined by yasnippet.
  )
(defun texmate-import-get-property (name start stop)
  "* Get property from plist"
  (let ( (val-start nil) (val-stop nil) (content nil) )
    (goto-char start)
    (when (search-forward (concat "<key>" name "</key>") stop 't)
      (when (search-forward "<string>")
        (setq val-start (point))
        (when (search-forward "</string>")
          (setq val-stop (match-beginning 0))
          (setq content (buffer-substring val-start val-stop))
          )
        )
      )
    (symbol-value 'content)
    )
  )
(setq texmate-import-convert-known-expressions
  '(
    ("&lt;" "<")
    ("&gt;" ">")
    ("[$][{]\\([0-9]+\\):[$]TM_SELECTED_TEXT[}]" "${\\1:`yas/selected-text`}")
    ("[$][{]\\([0-9]+\\)[}]" "$\\1")
    ("[$][{]TM_SELECTED_TEXT:\\([^\\}]*\\)[}]" "`(or yas/selected-text \"\\1\")`")
    ("[$][{]TM_SELECTED_TEXT[}]" "`(or yas/selected-text \"\")`")
    ("[$]TM_SELECTED_TEXT" "`(or yas/selected-text \"\")`")
    )
;  "*Texmate import convert known expressions"
  
  )
(defun texmate-import-convert-template (template)
  "* Converts template to Yasnippet template"
  (let (ret max )
    (with-temp-buffer
      (insert template)
      (mapc (lambda(x)
              (goto-char (point-min))
              (while (re-search-forward (nth 0 x) nil t)
                (replace-match (nth 1 x) 't nil))
              )
            texmate-import-convert-known-expressions
            )
      (goto-char (point-min))
      (setq max "0")
      (while (re-search-forward "[$][{]?\\([0-9]+\\)" nil t)
        (setq max (match-string 1))
        )
      (setq max (+ 1 (string-to-int max)))
      (while (search-forward "`(or yas/selected-text \"\")`" nil t)
        (replace-match (format "${%s:`yas/selected-text`}" max) 't 't))
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (symbol-value 'ret)
    )
  )
(defun texmate-get-group (uuid plist)
  "* Gets group from textmate info.plist file"
  (let (group start stop)
    (with-temp-buffer
      (insert plist)
      (goto-char (point-min))
      (when (search-forward (concat "<string>" uuid "</string>") nil t)
        (when (search-backward "<dict>")
          (setq start (point))
          )
        (when (search-forward "</dict>")
          (setq stop (point))
          )
        (setq group (texmate-import-get-property "name" start stop))
        )
      )
    (symbol-value 'group)
    )
  )
(defun texmate-import-file (file new-dir &optional mode original-author plist transform-function parent-modes)
  "* Imports texmate file"
  (message "Importing %s " file)
  (with-temp-buffer
    (insert-file-contents file)
    (texmate-import-current-buffer new-dir plist file original-author mode transform-function parent-modes)
    )
  )
(defun texmate-import-guess-possiblities (p-quote match-string)
  "* Guesses possible modes..."
  (add-to-list p-quote (intern (concat match-string "-mode")))
  (add-to-list p-quote (intern (concat (downcase match-string) "-mode")))
  (add-to-list p-quote (intern (concat (upcase match-string) "-mode")))
  (when (< 1 (length match-string))
    (add-to-list p-quote (intern (concat (upcase (substring match-string 0 1))
                                         (downcase (substring match-string 1)) "-mode")))
    )
  )
(defvar texmate-import-saved-guesses '()
  "Saved guesses for texmate import"
  )
(defvar texmate-import-saved-ess '())
(defun texmate-import-guess-mode (scope-o &optional snippet-q)
  "* Guesses mode based on Texmate scope."
  (if (assoc scope-o texmate-import-saved-guesses)
      (let (
            (ret (nth 1 (assoc scope-o texmate-import-saved-guesses)))
            )
        (when (memq 'ess-mode ret)
          (when (string-match "# *scope: *.*" (symbol-value snippet-q))
            (set snippet-q
                 (replace-match
                  (concat
                   (match-string 0 (symbol-value snippet-q))
                   (format "\n# condition: (string= \"%s\" ess-language)"
                           (nth 1 (assoc scope-o texmate-import-saved-ess))))
                  't 't (symbol-value snippet-q)
                  )
                 )
            )
          ;; Take out any Ess keybindings.  They are hard to translate...
          (when (string-match "\n# *binding:.*" (symbol-value snippet-q))
            (set snippet-q (replace-match "" 't 't (symbol-value snippet-q))))
          )
        (symbol-value 'ret)
        )
    (let (
          (possible-modes '())
          (tmp '())
          (scope scope-o)
          )
      (when (string-match "\\([A-Za-z0-9]+\\)[.]tmbundle" scope)
        (texmate-import-guess-possiblities 'possible-modes (match-string 1 scope))
        )
      (while (string-match "[.]\\([A-Za-z0-9]+\\)\\>" scope)
        (texmate-import-guess-possiblities 'possible-modes (match-string 1 scope))
        (setq scope (replace-match "" nil nil scope))
        )
      (setq tmp (remove-if-not
                 #'(lambda(x) (fboundp x)) possible-modes))
      (setq possible-modes '())
      (mapc (lambda(x)
              (with-temp-buffer
                (funcall x)
                (add-to-list 'possible-modes major-mode)
                ;; Handle Ess's strange handling of modes.
                (when (and snippet-q (eq 'ess-mode major-mode))
                  (add-to-list 'texmate-import-saved-ess (list scope-o ess-language))
                  (when (string-match "# *scope: *.*" (symbol-value snippet-q))
                    (set snippet-q
                         (replace-match
                          (concat
                           (match-string 0 (symbol-value snippet-q))
                           (format "\n# condition: (string= \"%s\" ess-language)" ess-language))
                          't 't (symbol-value snippet-q)
                          )
                         )
                    )
                  ;; Take out any Ess keybindings.  They are hard to translate...
                  (when (string-match "\n# *binding:.*" (symbol-value snippet-q))
                    (set snippet-q (replace-match "" 't 't (symbol-value snippet-q))))
                  )
                )
              )
            tmp
            )
      (unless possible-modes
        (setq possible-modes (list (intern (completing-read (format "Emacs Mode (Texmate scope: %s): " scope-o) '()))))
        )
      (add-to-list 'texmate-import-saved-guesses (list scope-o possible-modes))
      (symbol-value 'possible-modes)
      )
    )
  )

(defun texmate-import-current-buffer (new-dir &optional plist  buffer-name original-author mode-string-or-function   transform-function parent-modes)
  "* Changes Texmate (current buffer) plist to yas snippet."
  (let (
        (start nil)
        (stop nil)
        (val-start nil)
        (val-stop nil)
        (content nil)
        (trigger nil)
        (uuid nil)
        (name nil)
        (scope nil)
        (group nil)
        (snippet "")
        (binding "")
        (mode "")
        (bfn (or buffer-name (buffer-file-name)))
        )
    (when (string-match "/?\\([^/]*\\)[.][^.]*$" bfn)
      (setq bfn (concat (match-string 1 bfn) ".yasnippet"))
      )
    (while (string-match "[^A-Z0-9_]" bfn)
      (setq bfn (replace-match "_" nil nil bfn))
      )
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "<dict>" nil t)
        (setq start (point))
        (when (search-forward "</dict>" nil t)
          (setq stop (point))
          (setq content (texmate-import-get-property "content" start stop))
          (setq key (texmate-import-get-property "tabTrigger" start stop))
          (setq uuid (texmate-import-get-property "uuid" start stop))
          (setq name (texmate-import-get-property "name" start stop))
          (setq scope (texmate-import-get-property "scope" start stop))
          (setq group (texmate-get-group uuid plist))
          (setq binding (texmate-import-get-property "keyEquivalent" start stop))
          (when binding
            (setq binding (downcase binding))
            (while (string-match "[@]+" binding)
              (setq binding (replace-match "" nil nil binding)))
            (while (string-match "\\^+" binding)
              (setq binding (replace-match "C-" nil nil binding)))
            )
          (setq snippet (concat "# -*- mode: snippet -*-"
                                "\n# uuid: " uuid
                                "\n# contributor: Translated from textmate snippet by texmate-import.el"
                                "\n# contributor: Imported by " (user-full-name)
                                (if original-author
                                    (concat "\n# contributor: Original Author " original-author)
                                  "")
                                "\n# name: " name
                                (if (not key)
                                    ""
                                  (concat "\n# key: " key)
                                  )
                                (if (not binding)
                                    ""
                                  (concat "\n# binding: \"C-c C-y " binding "\"")
                                  )
                                "\n# scope: " scope
                                (if group
                                    (concat "\n# group: " group)
                                  "")
                                "\n# --\n"
                                (texmate-import-convert-template content)
                                )
                )
          (when transform-function
            (setq snippet (apply transform-function (list snippet)))
            )
          (cond
           ( (functionp mode-string-or-function)
             (setq mode (list (funcall mode-string-or-function snippet)))
             )
           ( (stringp mode-string-or-function)
             (setq mode (list mode-string-or-function))
             )
           ( 't
             (setq mode (mapcar (lambda(x) (format "%s" x)) (texmate-import-guess-mode scope 'snippet)))
             )
           )
          ;; (setq new-dir (concat new-dir mode))
          (mapc (lambda(m)
                  (unless (string= m "")
                    (setq m (concat m "/"))
                    )
                  (when (not (file-exists-p (concat new-dir m)))
                    (make-directory (concat new-dir m) 't)
                    )
                  (with-temp-file (concat new-dir m "/" bfn)
                    (insert snippet)
                    )
                  (when (and parent-modes (not (string= parent-modes "")))
                    (unless (file-exists-p (concat new-dir m "/.yas-parents"))
                      (with-temp-file (concat new-dir m "/.yas-parents")
                        (insert parent-modes)
                        )
                      )
                    )
                  )
                mode
                )
          )
        )
      )
    )
  )
(defun texmate-import-bundle (dir parent-modes &optional original-author yas-dir mode transform-function)
  "Imports texmate bundle to new-dir.  Mode may be a string or a function determining which mode to place files in..."
  (interactive "fTexmate Bundle Directory: \nsParent Modes: ")
  (setq dir (file-name-directory dir)) 
  (unless (string= "/" (substring dir -1))
    (setq dir (concat dir "/")))
  (let (snip-dir snips plist (new-dir (if (eq (type-of 'yas/root-directory) 'symbol)
                                          yas/root-directory
                                        (nth 0 yas/root-directory)
                                        )))
    (when (file-exists-p (concat dir "info.plist"))
      (setq plist (with-temp-buffer (insert-file-contents (concat dir "info.plist"))
                                    (buffer-substring (point-min) (point-max))))
      )
    (setq snip-dir (concat dir "Snippets/"))
    (when (file-exists-p snip-dir)
      (setq snips (file-expand-wildcards (concat snip-dir "*.tmSnippet")))
      (unless (not (file-exists-p new-dir))
        (mapc (lambda(x)
                (texmate-import-file x new-dir mode original-author plist transform-function parent-modes)
                )
              snips
              )
        )
      )
    )
  )
(defun texmate-import-stata (dir &optional new-dir)
  "*Example function for importing Sata snippets into Yasnippet"
  (message "Importing Stata bundle dir %s" dir)
  (texmate-import-bundle dir "text-mode" "Timothy Beatty" new-dir)
  )
(defun texmate-import-rmate (dir &optional new-dir)
  "* Example Function for importing Rmate into Yasnippet"
  (message "Importing Rmate Bundle dir %s" dir)
  (texmate-import-bundle dir "text-mode" "Hans-Peter Suter" new-dir)
  )
(defvar texmate-import-svn-url "http://svn.textmate.org/trunk/Bundles/"
  "* Url for Texmate svn"
  )
(defvar texmate-import-svn-pkgs-cache nil
  "* Cached list of Texmate svn bundles"
  )
(defun texmate-import-svn-get-pkgs ()
  "* Imports texmate snippets from TexMate bundles svn"
  (if texmate-import-svn-pkgs-cache
      (symbol-value 'texmate-import-svn-pkgs-cache)
  (let (
        (buf (url-retrieve-synchronously texmate-import-svn-url))
        (lst '())
        )
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward "\"\\([%A-Z0-9_a-z]+\\)[.]tmbundle/\"" nil t)
        (add-to-list 'lst (match-string 1)))
      (kill-buffer (current-buffer))
      )
    (setq texmate-import-svn-pkgs-cache (mapcar (lambda(x) (replace-in-string x "%20" " ")) lst))
    (symbol-value 'texmate-import-svn-pkgs-cache)
    )
  ))
(defun texmate-import-snippets-supported (texmate-url)
  "Check to see if snippets are supported"
  (let (
        (buf (url-retrieve-synchronously texmate-url))
        (ret nil)
        )
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (setq ret (re-search-forward "\"Snippets/\"" nil t))
      (kill-buffer (current-buffer))
      )
    )
  )
(defun texmate-import-svn-snippets (snippet-url plist)
  "*Imports snippets based on texmate svn tree."
  (message "Fetching %s" snippet-url)
  (let (
        buf
        (snippets '())
        (new-dir (if (eq (type-of 'yas/root-directory) 'symbol)
                     yas/root-directory
                   (nth 0 yas/root-directory)
                   ))
        )
    (setq buf (url-retrieve-synchronously snippet-url))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"]*[.]tmSnippet\\)\"" nil 't)
        (add-to-list 'snippets (match-string 1))
        )
      (kill-buffer (current-buffer))
      )
    (mapc (lambda(x)
            (message "Fetching %s" (concat snippet-url x))
            (setq buf (url-retrieve-synchronously (concat snippet-url x)))
            (save-excursion
              (set-buffer buf)
              (texmate-import-current-buffer new-dir plist
                                             (replace-in-string (replace-in-string x "%20" " ") "%3c" "<")
                                             )
              (kill-buffer (current-buffer))
              )
            )
          snippets)
    )
  )
(defun texmate-import-svn-url ()
  "* Imports a texmate bundle and extracts snippets from `texmate-import-svn-url'"
  (interactive)
  (let (
        (texmate-name (completing-read "Texmate package: " (texmate-import-svn-get-pkgs) nil 't))
        textmate-url
        temp-dir
        buf
        plist
        )
    (setq texmate-url (concat texmate-import-svn-url (replace-in-string texmate-name " " "%20") ".tmbundle/"))
    (if (not (texmate-import-snippets-supported texmate-url))
        (progn
          (setq texmate-import-svn-pkgs-cache (remove-if
                                               #'(lambda(x) (string= texmate-name x))
                                               texmate-import-svn-pkgs-cache))
          (error "This Texmate package has no snippets")
          )
      (message "Fetching %s" (concat texmate-url "info.plist"))
      (setq buf (url-retrieve-synchronously (concat texmate-url "info.plist")))
      (save-excursion
        (set-buffer buf)
        (setq plist (buffer-substring (point-min) (point-max)))
        (kill-buffer (current-buffer))
        )
      (texmate-import-svn-snippets (concat texmate-url "Snippets/") plist)
      )
    )
  )
;(setq debug-on-error 't)
;(texmate-import-rmate "c:/tmp/swissr-rmate.tmbundle-v0.4.2-0-g7d026da/swissr-rmate.tmbundle-7d026da/")
;(texmate-import-stata "c:/tmp/Stata.tmbundle/")
(provide 'texmate-import)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; texmate-import.el ends here
