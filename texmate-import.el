;;; texmate-import.el --- Import Texmate macros into yasnippet syntax
;; 
;; Filename: texmate-import.el
;; Description: Import Texmate macros into yasnippet syntax
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Oct 20 15:08:50 2010 (-0500)
;; Version: 
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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
    ("[$][{]\\([0-9]+\\)" "$\\1")
    ("[$][{]TM_SELECTED_TEXT:\\([^\\}]*\\)[}]" "`(or (yas/selected-text) \"\\1\")`")
    )
;  "*Texmate import convert known expressions"
  
  )
(defun texmate-import-convert-template (template)
  "* Converts template to Yasnippet template"
  (let (ret)
    (with-temp-buffer
      (insert template)
      (mapc (lambda(x)
              (goto-char (point-min))
              (while (re-search-forward (nth 0 x) nil t)
                (replace-match (nth 1 x)))
              )
            texmate-import-convert-known-expressions
            )
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (symbol-value 'ret)
    )
  )
(defun texmate-get-group (uuid plist)
  "* Gets group from textmate info.plist file"
  (let (group)
    (with-temp-buffer
      (insert plist)
      (goto-char (point-min))
      (when (search-forward (concat "<string>" uuid "</string>") nil t)
        (setq group (texmate-import-get-property "name" (point) (point-max)))
        )
      )
    (symbol-value 'group)
    )
  )
(defun texmate-import-file (file new-dir &optional original-author plist)
  "* Imports texmate file"
  (message "Importing %s" file)
  (with-temp-buffer
    (insert-file-contents file)
    (texmate-import-current-buffer new-dir original-author file plist)
    )
  )
(defun texmate-import-current-buffer (new-dir &optional original-author buffer-name plist)
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
        (bfn (or buffer-name (buffer-file-name)))
        )
    (when (string-match "/\\([^/]*\\)[.][^.]*$" bfn)
      (setq bfn (concat (match-string 1 bfn) ".yasnippet"))
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
          (setq snippet (concat "# -*- mode: snippet -*-"
                                "\n# uuid: " uuid
                                "\n# contributor: Translated from textmate snippet by texmate-import.el"
                                "\n# contributor: Imported by " (user-full-name)
                                (if original-author
                                    (concat "\n# contributor: Original Author" original-author)
                                  "")
                                "\n# name: " name 
                                "\n# key: " key
                                "\n# scope: " scope
                                (if group
                                    (concat "\n# group: " group)
                                  "")
                                "\n# --\n"
                                (texmate-import-convert-template content)
                                )
                )
          (with-temp-file (concat new-dir "/" bfn)
            (insert snippet)
            )
          )
        )
      )
    )
  )
(defun texmate-import-bundle (dir mode &optional original-author yas-dir)
  "Imports texmate bundle to new-dir."
  (let (snip-dir snips plist (new-dir (if (eq (type-of 'yas/root-directory) 'symbol)
                                          yas/root-directory
                                        (nth 0 yas/root-directory)
                                        )))
    (setq new-dir (concat new-dir "/text-mode/" mode "/"))
    (when (file-exists-p (concat dir "info.plist"))
      (setq plist (with-temp-buffer (insert-file-contents (concat dir "info.plist"))
                                    (buffer-substring (point-min) (point-max))))
      )
    (setq snip-dir (concat dir "Snippets/"))
    (when (file-exists-p snip-dir)
      (setq snips (file-expand-wildcards (concat snip-dir "*.tmSnippet")))
      (when (not (file-exists-p new-dir))
        (make-directory new-dir 't)
        )
      (unless (not (file-exists-p new-dir))
        (mapc (lambda(x)
                (texmate-import-file x new-dir original-author plist)
                )
              snips
              )
        )
      )
    )
  )
(setq debug-on-error 't)
;(texmate-import-bundle "c:/tmp/rmate.tmbundle-7d026da/" "ess-mode")

(provide 'texmate-import)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; texmate-import.el ends here
