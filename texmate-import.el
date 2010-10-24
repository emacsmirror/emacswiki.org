;;; texmate-import.el --- Import Texmate macros into yasnippet syntax
;; 
;; Filename: texmate-import.el
;; Description: Import Texmate macros into yasnippet syntax
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Oct 20 15:08:50 2010 (-0500)
;; Version: 0.1 
;; Last-Updated: Sat Oct 23 23:42:37 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 89
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
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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
(defvar yas/root-directory "./") ; Should already be defined by yasnippet.
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
(defun texmate-import-file (file mode new-dir &optional original-author plist transform-function)
  "* Imports texmate file"
  (message "Importing %s" file)
  (with-temp-buffer
    (insert-file-contents file)
    (texmate-import-current-buffer mode new-dir original-author file plist transform-function)
    )
  )
(defun texmate-import-current-buffer (mode-string-or-function new-dir &optional original-author buffer-name plist transform-function)
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
    (when (string-match "/\\([^/]*\\)[.][^.]*$" bfn)
      (setq bfn (concat (match-string 1 bfn) ".yasnippet"))
      )
    (while (string-match "[ \t]+" bfn)
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
          (when (functionp mode-string-or-function)
            (setq mode (funcall mode-string-or-function snippet))
            )
          (when (stringp mode-string-or-function)
            (setq mode mode-string-or-function)
            )
          (unless (string= mode "")
            (setq mode (concat mode "/"))
            )
          (setq new-dir (concat new-dir "/text-mode/" mode))
          (when (not (file-exists-p new-dir))
            (make-directory new-dir 't)
            )
          (with-temp-file (concat new-dir "/" bfn)
            (insert snippet)
            )
          )
        )
      )
    )
  )
(defun texmate-import-bundle (dir mode &optional original-author transform-function yas-dir)
  "Imports texmate bundle to new-dir.  Mode may be a string or a function determining which mode to place files in..."
  (interactive "fTexmate Bundle Directory: \nsMode: ")
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
                (texmate-import-file x mode new-dir original-author plist transform-function)
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
  (texmate-import-bundle dir
                         "ess-mode"
                         "Timothy Beatty"
                         (lambda(template)
                         (let (
                               (ret template)
                               )
                           (with-temp-buffer
                             (insert template)
                             (goto-char (point-min))
                             (while (re-search-forward "# *scope: *source.stata$" nil t)
                               (end-of-line)
                               (insert "\n# condition: (string= \"STA\" ess-language)")
                               )
                             ;; Ess mode doesn't have keybindings....
                             (goto-char (point-min))
                             (while (re-search-forward "# *binding:.*" nil t)
                               (replace-match ""))
                             (setq ret (buffer-substring (point-min) (point-max)))
                             )
                           (symbol-value 'ret)
                           )
                         )
                       new-dir
                       )
  )
(defun texmate-import-rmate (dir &optional new-dir)
  "* Example Function for importing Rmate into Yasnippet"
  (message "Importing Rmate Bundle dir %s" dir)
  (texmate-import-bundle dir
                         (lambda(template)
                           (if (string-match "# *scope: *source.rd$" template)
                               "Rd-mode"
                             "ess-mode"))
                       "Hans-Peter Suter"
                       (lambda(template)
                         (let (
                               (ret template)
                               )
                           (with-temp-buffer
                             (insert template)
                             (goto-char (point-min))
                             (when (re-search-forward "# *scope: *source.rd$" nil t)
                               (goto-char (point-min))
                               (while (re-search-forward "# group:.*\n" nil t)
                                 (replace-match ""))
                               )
                             (goto-char (point-min))
                             (while (re-search-forward "# *scope: *source.r$" nil t)
                               (end-of-line)
                               (insert "\n# condition: (string= \"S\" ess-language)")
                               )
                             (goto-char (point-min))
                             (while (re-search-forward "# *key: *rd[.]" nil t)
                               (replace-match "# key: "))
                             ;; Ess mode doesn't have keybindings....
                             (goto-char (point-min))
                             (while (re-search-forward "# *binding:.*" nil t)
                               (replace-match ""))
                             (setq ret (buffer-substring (point-min) (point-max)))
                             )
                           (symbol-value 'ret)
                           )
                         )
                       new-dir
                       )
  )
;(texmate-import-rmate "c:/tmp/rmate.tmbundle-7d026da/")
;(texmate-import-stata "c:/tmp/Stata.tmbundle/")
(provide 'texmate-import)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; texmate-import.el ends here
