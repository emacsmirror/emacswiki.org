;;; texmate-import.el --- Import Texmate macros into yasnippet syntax
;; 
;; Filename: texmate-import.el
;; Description: Import Texmate macros into yasnippet syntax
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Oct 20 15:08:50 2010 (-0500)
;; Version: 0.1 
;; Last-Updated: Fri Nov  5 14:37:51 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 1051
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
;;  (autoload 'texmate-import-svn-from-url "texmate-import" "* Import TeXMate snippets from svn.textmate.org" 't)
;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 05-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Nov  5 14:33:00 2010 (-0500) #1050 (Matthew L. Fidler)
;;
;;    Added better texmate support by providing translations for
;;    mirrors. Requires the directive # type: command available in the
;;    SVN version of yasnippet.
;;
;; 05-Nov-2010      
;;    Last-Updated: Fri Nov  5 09:59:30 2010 (-0500) #898 (US041375)
;;    Changed texmate-replace-in-string with replace-regexp-in-string
;; 04-Nov-2010      
;;    Last-Updated: Thu Nov  4 12:38:32 2010 (-0500) #535 (us041375)
;;    Changed extension from .yasnippet to what the package is in a svn-import.
;; 04-Nov-2010      
;;    Last-Updated: Thu Nov  4 10:55:27 2010 (-0500) #525 (us041375)
;;    replace-in-string changed to texmate-replace-in-string.  May be missing on some systems.
;; 01-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Nov  1 16:19:16 2010 (-0500) #447 (Matthew L. Fidler)
;;    Bug fix for expand-env
;; 01-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Nov  1 15:16:01 2010 (-0500) #442 (Matthew L. Fidler)
;;    Added more supported tags.
;; 01-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Nov  1 13:27:13 2010 (-0500) #413 (Matthew L. Fidler)
;;    Took out #scope pseudo-directive. 
;; 01-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Nov  1 12:04:30 2010 (-0500) #385 (Matthew L. Fidler)
;;    Added more file extensions.
;; 28-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 28 14:45:28 2010 (-0500) #375 (Matthew L. Fidler)
;;    Removed bindings.  They are currently causing problems...
;; 28-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 28 11:14:35 2010 (-0500) #354 (Matthew L. Fidler)
;;    Added completed import of svn bundle message.
;; 28-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Oct 28 10:56:55 2010 (-0500) #348 (Matthew L. Fidler)
;;    Bug fix to allow files to be .yasnippet instead of _yasnippet files.
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

(defgroup texmate-import nil
  "* Texmate import"
  )


(defcustom texmate-menu-definition 0
  "* Defines the type of menu definition that is implemented:

Possible choices are:
  Group -- Just use the group that the menu is located in.
  define -- Define using (yas/define-menu).  Currently unimplemented.
"
  :group 'texmate-import)

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
(setq texmate-regexp-to-emacs-regexp-known '(;TexMate  Emacs
                                           ;    ("\\w" "\\w")
                                               ("\\s" "\\s-")
                                               ("\\S" "\\S-")
                                               ("\\d" "[0-9]")
                                               ("\\D" "[^0-9]")
                                               ("\\p{Alphanum}" "[A-Za-z0-9]")
                                               ("\\p{^Alphanum}" "[^A-Za-z0-9]")
                                               ("\\h" "[0-9a-fA-F]")
                                               ("\\H" "[^0-9a-fA-F]")
                                               )
  )
(defun texmate-regexp-to-emacs-regexp (rexp)
  "* Convert a texmate regular expression to an emacs regular expression (as much as possible)"
  (let (ret
        case-fold-search 
        )
    (with-temp-buffer
        ;; Emacs http://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-of-Regexps.html#Syntax-of-Regexps
  ;; Texmate http://manual.macromates.com/en/drag_commands#drag_commands

  ;; \w => \w
  ;; \W => \W
  ;; \s => \s- (Whitespace)
  ;; \S => \S-
  ;; \d => [0-9] Decimal digit character
  ;; \D => [^0-9] Non-decimal digit character
  ;; \h => [0-9a-fA-F] Hexadecimal digit character
  ;; \H => [^0-9a-fA-F] Not Hexadecimal digit character
  ;; \p{Alnum} => [A-Za-z0-9] Alphanumeirc
  ;; \p{^Alnum} => [^A-Za-z0-9] Alphanumeirc
  ;; {n,m} => \{n,m\}
  ;; {,n} => \{,n\}
  ;; \{n,\} => \{n,\}
      (insert rexp)
      ;; The following needs to be changed (){} |
      (goto-char (point-min))
      (while (re-search-forward (eval-when-compile
                                  (regexp-opt '("\\("
                                                "\\)"
                                                "\\|"
                                                "\\{"
                                                "\\}"
                                                ) 't
                                                  )) nil t)
        (replace-match (string (cond 
                                ( (string= (match-string 0) "\\(") ?\C-a)
                                ( (string= (match-string 0) "\\)") ?\C-b)
                                ( (string= (match-string 0) "\\{") ?\C-c)
                                ( (string= (match-string 0) "\\}") ?\C-d)
                                ( (string= (match-string 0) "\\|") ?\C-e)
                                )) nil 't)
        )
      (goto-char (point-min))
      (while (re-search-forward (eval-when-compile (regexp-opt '("(" ")" "{" "}" "|") 't)) nil t)
        (replace-match (concat "\\" (match-string 0)) nil 't))
      (goto-char (point-min))
      (while (re-search-forward (eval-when-compile (regexp-opt (list (string ?\C-a) (string ?\C-b) (string ?\C-c) (string ?\C-d) (string ?\C-e)) 't)) nil t)
        (replace-match (cond 
                             ( (string= (match-string 0) (string ?\C-a)) "(")
                             ( (string= (match-string 0) (string ?\C-b)) ")")
                             ( (string= (match-string 0) (string ?\C-c)) "{")
                             ( (string= (match-string 0) (string ?\C-d)) "}")
                             ( (string= (match-string 0) (string ?\C-e)) "|")) 't 't)
                             
        )
      (mapc
       (lambda(x)
         (goto-char (point-min))
         (while (re-search-forward (regexp-quote (nth 0 x)) nil 't)
           (replace-match (nth 1 x) 't 't))
         )
       texmate-regexp-to-emacs-regexp-known
       )
      (setq ret (buffer-substring-no-properties (point-min) (point-max)))
      )
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
    ("`date +[+]\\(.*?\\)`" "`(format-time-string \"\\1\")`")
    ;; See http://manual.macromates.com/en/environment_variables.html

    ("[$]TM_CURRENT_LINE" "`yas/current-line`")
    ("[$]TM_CURRENT_WORD" "`yas/current-word`")
    ("[$]TM_DIRECTORY" "`yas/current-dir`")
    ("[$]TM_FILEPATH" "`yas/current-path`")
    ("[$]TM_LINE_INDEX" "`yas/current-column`")
    ;; Unsupported:
    ;; TM_SOFT_TABS, TM_SUPPORT_PATH, TM_TAB_SIZE

    ;; There are situations where we want our placeholder text
    ;; mirrored but with slight changes or where we want some text to
    ;; appear depending on the value/presence of a placeholder.

    ;; We can accomplish this by doing a regular expression
    ;; substitution on the placeholder text (when mirroring it). The
    ;; syntax for this is: ${<<tab stop>>/<<regexp>>/<<format>>/<<options>>}.

    
    ;; Also see http://manual.macromates.com/en/drag_commands#drag_commands
    
    ;; TM_DROPPED_FILE -- relative path of the file dropped (relative
    ;; to the document directory, which is also set as the current
    ;; directory).

    ;; TM_DROPPED_FILEPATH -- the absolute path of the file dropped.

    ;; TM_MODIFIER_FLAGS -- the modifier keys which were held down
    ;; when the file got dropped. This is a bitwise OR in the form:
    ;; SHIFT|CONTROL|OPTION|COMMAND (in case all modifiers were down).


    ("[$]TM_FULLNAME" "`(user-full-name)`")

    ;; Unknown environment commands.  They can be taught!
    
    ("[$][{]\\([A-Za-z].*?\\):\\(\\(?:.*?[\\\\][}]\\)*.*?\\)[}]" "`(or (yas/getenv \"\\1\") \"\\2\")`") ;
    ("[$][{]\\([A-Za-z].*?\\)[}]" "`(or (yas/getenv \"\\1\") \"\")`")
    )
;  "*Texmate import convert known expressions"
  
  )
(defvar texmate-import-convert-env-lst '()
  "List to convert Texmate Environmental variables to customizable fields."
  )
(defun texmate-import-convert-template-t (begin-text max)
  "* Subroutine to convert regular expressions to (yas/t expressions)"
  (let (
        (str (buffer-substring-no-properties (point) max))
        (lst '())
        ret
        )
    (delete-region (point) max)
    (replace-match (format "%s(yas/t/ " begin-text))
    (while (string-match (eval-when-compile (regexp-quote "\\/")) str)
      (setq str (replace-match (string ?\C-a) 't 't str)))
    (setq lst (split-string str "/"))
    (setq ret
          (mapconcat
           (lambda(x)
             (let ((val x))
               (while (string-match (eval-when-compile (regexp-quote (string ?\C-a))) val)
                 (setq val (replace-match "\\/" 't 't val)))
               (while (string-match "\"" val)
                 (setq val (replace-match "\\\"" 't 't val)))
               (setq val (concat "\"" val "\""))
               (symbol-value 'val)
               )
             )
           lst " ")
          )
    (insert ret)
    )
  )
(defun texmate-import-convert-template (template)
  "* Converts template to Yasnippet template"
  (let (ret max p1 not-found txt i lst)
    (with-temp-buffer
      (insert template)
      (mapc (lambda(x)
              (goto-char (point-min))
              (while (re-search-forward (nth 0 x) nil t)
                (when (save-match-data (string-match "yas/getenv" (nth 1 x)))
                  (add-to-list 'texmate-import-convert-env-lst (match-string 1)))
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
      ;; Now replace TexMate mirrors $(1/reg/expr)
      (goto-char (point-min))
      (while (re-search-forward "\\([$][{][0-9]+\\)/" nil t)
        (setq max (save-match-data (save-excursion
                    (goto-char (match-beginning 0))
                    (forward-char 1)
                    (with-syntax-table text-mode-syntax-table
                      (forward-sexp 1))
                    (backward-char 1)
                    (insert ")")
                    (- (point) 1)
                    )))
        (texmate-import-convert-template-t "\\1:$" max )
        )
      ;; Now replace (yas/t/ "".*) with the appropriate list
      (setq i 0)
      (goto-char (point-min))           
      (setq lst "(setq yas/t-lst '(")
      (while (re-search-forward "(yas/t/ \"" nil t)
        (setq p1 (- (point) 1))
        (goto-char (match-beginning 0))
        (with-syntax-table text-mode-syntax-table
          (forward-sexp 1))
        (setq lst (concat lst "\n\t(" (buffer-substring-no-properties p1 (point))))
        (delete-region (match-beginning 0) (point))
        (insert (format "(apply 'yas/t/ (nth %s yas/t-lst))" i))
        (setq i (+ i 1))
        )
      (unless (string= "(setq yas/t-lst '(" lst)
        (setq lst (concat lst "))\n"))
        (goto-char (point-min))
        (insert "(yas/expand-snippet \"")
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" 't 't))
        (goto-char (point-max))
        (insert "\")")
        (goto-char (point-min))
        (insert lst)
        )
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
  (if (not scope)
      '(text-mode)
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
        (message "Guessed the possible modes: %s" possible-modes)
        (symbol-value 'possible-modes)
        )
      )
    )
  )

(defun texmate-import-current-buffer (new-dir &optional plist  buffer-name original-author mode-string-or-function   transform-function parent-modes ext)
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
        (type "")
        (snippet "")
        (binding "")
        (mode "")
        (env "")
        (bfn (or buffer-name (buffer-file-name)))
        (yas (or ext ".yasnippet"))
        )
    (when (string-match "/?\\([^/]*\\)[.][^.]*$" bfn)
      (setq bfn (concat (match-string 1 bfn) yas))
      )
    (while (string-match "[^A-Z0-9_.]" bfn)
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
;          (setq binding (texmate-import-get-property "keyEquivalent" start stop))
          (when binding
            ;; Need to convert bindings.
            )
          (setq snippet (texmate-import-convert-template content))
          ;; Get Environment
          (when (string-match "\\<yas/current-line\\>" snippet)
            (setq env (concat env " (yas/current-line (buffer-substring (point-at-bol) (point-at-eol))) ")))
          (when (string-match "\\<yas/current-word\\>" snippet)
            (setq env (concat env " (yas/current-word (buffer-substring (save-excursion (skip-syntax-backward \"w\") (point) (save-excursion (skip-syntax-forward \"w\") (point))) ")))
          (when (string-match "\\<yas/current-dir\\>" snippet)
            (setq env (concat env " (yas/current-dir (if (buffer-file-name) (file-name-directory (buffer-file-name)) \"\")) ")))
          (when (string-match "\\<yas/current-path\\>" snippet)
            (setq env (concat env " (yas/current-path (if (buffer-file-name) (buffer-file-name) \"\")) ")))
          (when (string-match "\\<yas/current-column\\>" snippet)
            (setq env (concat env " (yas/current-column (if (current-column) (current-column) \"\")) ")))
          (when (string-match "(yas/expand-snippet" snippet)
            (setq type "\n# type: command")
            ;; Add environment to expand/snippet on command snippets.
            (unless (string= "" env)
              (goto-char (point-max))
              (when (re-search-backward ")" nil t)
                (insert "nil nil \"")
                (insert (replace-regexp-in-string "\"" "\\\"" env 't 't))
                (insert "\"")
                )
              )
            )
          (setq snippet (concat "# -*- mode: snippet -*-"
                                "\n# uuid: " uuid
                                "\n# contributor: Translated from textmate snippet by texmate-import.el"
                                "\n# contributor: Imported by " (user-full-name)
                                (if original-author
                                    (concat "\n# contributor: Original Author " original-author)
                                  "")
                                (if (string= env "") ""
                                  (concat "\n# expand-env : (" env ")")
                                  )
                                "\n# name: " name
                                (if (not key)
                                    ""
                                  (concat "\n# key: " key)
                                  )
;                                (if (not binding)
;                                    ""
;                                  (concat "\n# binding: C-c C-y " binding)
;                                  )
                                "\n# scope: " scope
                                type
                                (if group
                                    (concat "\n# group: " group)
                                  "")
                                "\n# --\n"
                                snippet
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
                    (set-buffer-file-coding-system 'raw-text)
                    (insert snippet)
                    (goto-char (point-min))
                    (when (re-search-forward "# *scope:.*\n" nil t)
                      (replace-match "")
                      )
                    )
                  (if (not parent-modes)
                      (setq parent-modes "text-mode")
                    )
                  (when (and parent-modes (not (string= parent-modes "")))
                    (unless (file-exists-p (concat new-dir m "/.yas-parents"))
                      (with-temp-file (concat new-dir m "/.yas-parents")
                        (insert parent-modes)
                        )
                      )
                    )
                  (when (and texmate-import-convert-env-lst (> (length texmate-import-convert-env-lst) 0))
                        (let (
                              (fc "")
                              (defg (format "(defgroup yas/%s nil \"%s snippet options\" :group 'yasnippet)" m  m))
                              (defc (format "(defcustom yas/%senv/%%s nil \"%s environment variable %%s.  May be customized here instead of having the environment value specified.  This customization takes precedence over any environmental variable.\" :type 'string :group 'yas/%s)" m m m))
                              )
                          (when (file-exists-p (concat new-dir m "/.yas-setup.el"))
                            (setq fc (with-temp-buffer (insert-file-contents (concat new-dir m "/.yas-setup.el"))
                                                       (buffer-substring (point-min) (point-max)))))
                          (with-temp-file (concat new-dir m "/.yas-setup.el")
                            (insert fc)
                            (goto-char (point-max))
                            (unless (search-backward "(require 'texmate-import)" nil t)
                              (insert "(require 'texmate-import)\n")
                              )
                            (goto-char (point-max))
                            (unless (search-backward defg nil t)
                              (insert defg)
                              (insert "\n")
                              )
                            (mapc (lambda(txt)
                                    (goto-char (point-max))
                                    (unless (search-backward (format defc txt txt) nil t)
                                      (insert (format defc txt txt))
                                      (insert "\n")
                                      )
                                    )
                                  texmate-import-convert-env-lst
                                  )
                            )
                          )
                      )
                    )
                mode
                )
          (setq textmate-import-convert-env-lst '())
          )
        )
      )
    )
  )
(defun texmate-import-bundle (dir parent-modes &optional original-author yas-dir mode transform-function)
  "Imports texmate bundle to new-dir.  Mode may be a string or a function determining which mode to place files in..."
  (interactive "fTexmate Bundle Directory: \nsParent Modes: ")
  (setq texmate-import-convert-env-lst '())
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
      (setq snips (apply 'append (mapcar #'(lambda (ext)
                                             (file-expand-wildcards (concat snip-dir "*." ext))
                                             )
                                       (list
                                       "tmSnippet"
                                       "plist"
                                       "tmCommand"
                                       "tmMacro")
                                       )
                         )
            )
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
  "* Gets texmate bundles from svn"
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
    (setq texmate-import-svn-pkgs-cache (mapcar (lambda(x) (replace-regexp-in-string "%20" " " x)) lst))
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
(defun texmate-import-svn-snippets (snippet-url plist texmate-name)
  "*Imports snippets based on texmate svn tree."
  (message "Fetching %s" snippet-url)
  (let (
        (ext texmate-name)
        buf
        (snippets '())
        (new-dir (if (eq (type-of 'yas/root-directory) 'symbol)
                     yas/root-directory
                   (nth 0 yas/root-directory)
                   ))
        (default-buffer-file-coding-system 'utf-8)
        (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
        )
    (setq buf (url-retrieve-synchronously snippet-url))
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"]*[.]\\(?:tmSnippet\\|plist\\|tmCommand\\|tmMacro\\)\\)\"" nil 't)
        (add-to-list 'snippets (match-string 1))
        )
      (kill-buffer (current-buffer))
      )
    (while (string-match "[^A-Za-z0-9_.]+" ext)
      (setq ext (replace-match "_" 't 't ext)))
    (setq ext (concat "." ext))
    (mapc (lambda(x)
            (message "Fetching %s" (concat snippet-url x))
            (setq buf (url-retrieve-synchronously (concat snippet-url x)))
            (save-excursion
              (set-buffer buf)
              (texmate-import-current-buffer new-dir plist
                                             (replace-regexp-in-string "%3c" "<"
                                                                       (replace-regexp-in-string "%20" " " x))
                                             nil
                                             nil
                                             nil
                                             nil
                                             ext
                                             )
              (kill-buffer (current-buffer))
              )
            (message "Imported %s" (replace-regexp-in-string "%3c" "<"
                                                             (replace-regexp-in-string
                                                              "%20" " " x)))
            (sleep-for 1)
            )
          snippets)
    (yas/reload-all)
    )
  )
;;;###autoload 
(defun texmate-import-svn-from-url ()
  "* Imports a texmate bundle and extracts snippets from `texmate-import-svn-url'"
  (interactive)
  (let (
        (texmate-name (completing-read "Texmate package: " (texmate-import-svn-get-pkgs) nil 't))
        textmate-url
        temp-dir
        buf
        plist
        )
    (setq texmate-url (concat texmate-import-svn-url (replace-regexp-in-string " " "%20" texmate-name) ".tmbundle/"))
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
      (sleep-for 1)
      (texmate-import-svn-snippets (concat texmate-url "Snippets/") plist texmate-name)
      (message "Completed loading snippets from texmate package %s" texmate-name)  
      )
    )
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet helper functions.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas/getenv (var)
  "* Gets environment variable or customized variable for Textmate->Yasnippet conversion"
  (let (
        (bvar (intern (format "yas/%s/env/%s" (or yas/mode-symbol major-mode) var)))
        )
    (if (boundp bvar)
        (if (symbol-value bvar)
            bvar
          (getenv var)
          )
      (getenv var)
      )
    )
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros for yas/replace-match
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro yas/format-match-ulm (match-number &optional string downcase)
  "* Helper macro to change texmate match-string \\u$1 to the correct expression" 
  `(if (> (length (match-string ,match-number ,string)) 1)
       (concat (,(if downcase 'downcase 'upcase) (substring (match-string ,match-number ,string) 0 1))
               (substring (match-string ,match-number ,string) 1))
     (upcase (match-string ,match-number ,string))))
(defmacro yas/format-match-UE (text &optional string downcase)
  "* Helper macro to emulate Texmate case folding in replacement that is \\U\\E and \\U\\L"
  (let (
        (lst (make-symbol "lst"))
        (ret (make-symbol "ret"))
        (md2 (make-symbol "md2"))
        (md (make-symbol "md"))
        (lst2 (make-symbol "lst2"))
        (ret2 (make-symbol "ret2"))
        (start (make-symbol "start"))
        (num (make-symbol "num"))
        (mtch (make-symbol "mtch"))
        )
    `(let (
           case-fold-search
           ,lst
           ,ret
           (,md (match-data))
           ,md2
           )
       (setq ,lst (split-string ,text ,(if downcase "\\\\L" "\\\\U") 't))
       (setq ,ret (pop ,lst))
       (setq ,ret (concat ,ret
                          (mapconcat
                           (lambda(x)
                             (let (,lst2 ,ret2 (,start 0) ,num ,mtch)
                               (setq ,lst2 (split-string x "\\\\E" 't))
                               (setq ,ret2 (,(if downcase 'downcase 'upcase) (pop ,lst2)))
                               (while (string-match "[$]\\([0-9]+\\)" ,ret2 ,start)
                                 (setq ,num (string-to-number (match-string 1 ,ret2)))
                                 (setq ,md2 (match-data)) ; Save match in current string
                                 (set-match-data ,md) ; Set match to actual match we are replacing.
                                 (setq ,mtch (,(if downcase 'downcase 'upcase) (match-string ,num ,string))) ; get the match data and make it upper case.
                                 (set-match-data ,md2) ; Put the match data in ret2 back.
                                 (setq ,start (+ (match-beginning 0) (length ,mtch)))
                                 (setq ,ret2 (replace-match ,mtch 't 't ,ret2))
                                 )
                               ;; Put extra \E values back in.
                               (setq ,ret2 (concat ,ret2 (mapconcat (lambda(y) y) ,lst2 "\\E")))
                               (symbol-value ',ret2)
                               )
                             )
                           ,lst
                           ""
                           )
                          )
             )
       (set-match-data ,md)
       (symbol-value ',ret)
       )
    ))
(defmacro yas/format-match-u (text &optional string downcase)
  "* Macro to replace \\u$1 (or \\l$1) with the correct expansion"
  (let (
        (md (make-symbol "md"))
        (md2 (make-symbol "md2"))
        (num (make-symbol "num"))
        (ret (make-symbol "ret"))
        (start (make-symbol "start"))
        (mtch (make-symbol "mtch"))
        )
    `(let (
           (,md (match-data))
           ,md2 ,num ,mtch
           case-fold-search
           (,ret ,text)
           (,start 0)
           )
       (while (string-match ,(if downcase "\\\\l[$]\\([0-9]+\\)" "\\\\u[$]\\([0-9]+\\)") ,ret ,start)
         (setq ,num (string-to-number (match-string 1 ,ret)))
         (setq ,md2 (match-data))
         (set-match-data ,md)
         (setq ,mtch (yas/format-match-ulm ,num ,string ,(if downcase 't 'nil))) ;;downcase
         (set-match-data ,md2)
         (setq start (+ (match-beginning 0) (length ,mtch)))
         (setq ,ret (replace-match ,mtch 't 't ,ret))
         )
       (set-match-data ,md)
       (symbol-value ',ret)
       )
    ))
(defmacro yas/format-match-? (text &optional string)
  "* Replaces conditional statements (?3:insertion:otherwise) or (?3:insertion)"
  (let ((md (make-symbol "md"))
        (md2 (make-symbol "md2"))
        (num (make-symbol "num"))
        (ret (make-symbol "ret"))
        (start (make-symbol "start"))
        (insert (make-symbol "insert"))
        (other (make-symbol "other"))
        (mtch (make-symbol "mtch"))
        )
    `(let (
           (,md (match-data))
           ,md2 ,num ,mtch
           (,ret ,text)
           (,start 0)
           (,insert "")
           (,other "")
           )
       (while (string-match "([?]\\([0-9]+\\)[:]\\(\\(?:.\\|\n\\)*?\\)\\(?:[:]\\(\\(?:.\\|\n\\)*?\\))\\|)\\)" ,ret ,start)
         (setq ,other "")
         (setq ,num (string-to-number (match-string 1 ,ret)))
         (setq ,insert (match-string 2 ,ret))
         (if (match-string 3 ,ret)
             (setq ,other (match-string 3 ,ret)))
         (setq ,md2 (match-data))
         (set-match-data ,md)
         (if (match-string ,num ,string)
             (if (string= "" (match-string ,num ,string))
                 (setq ,mtch ,other)
               (setq ,mtch ,insert)
               )
           (setq ,mtch ,other)
           )
         (set-match-data ,md2)
         (setq ,start (+ (match-beginning 0) (length ,mtch)))
         (setq ,ret (replace-match ,mtch 't 't ,ret))
         )
       (set-match-data ,md)
       (symbol-value ',ret)
       )
    ))
(defmacro yas/format-match-$ (text &optional string)
  "* Replace $1 with the appropriate match."
  (let (
        (ret (make-symbol "ret"))
        (md (make-symbol "md"))
        (start (make-symbol "start"))
        (md2 (make-symbol "md2"))
        (num (make-symbol "num"))
        (mtch (make-symbol "mtch"))
        )
  `(let (
         (,ret ,text)
         (,md (match-data))
         (,start 0)
         ,md2 ,num ,mtch)
       (while (string-match "[$]\\([0-9]+\\)" ,ret ,start)
         (setq ,num (string-to-number (match-string 1 ,ret)))
         (setq ,md2 (match-data))
         (set-match-data ,md)
         (setq ,mtch (match-string ,num ,string))
         (set-match-data ,md2)
         (setq ,start (+ (match-beginning 0) (length ,mtch)))
         (setq ,ret (replace-match ,mtch 't 't ,ret))
         )
       (set-match-data ,md)
     (symbol-value ',ret)
     )
  ))
(defmacro yas/format-match (text &optional string)
  "* Use Texmate style format strings to replace match data."
  (let ((ret (make-symbol "ret")))
  `(let (,ret)
     (setq ,ret (yas/format-match-UE ,text ,string))
     (setq ,ret (yas/format-match-UE ,ret ,string 't))
     (setq ,ret (yas/format-match-u ,ret ,string))
     (setq ,ret (yas/format-match-u ,ret ,string 't))
     (setq ,ret (yas/format-match-? ,ret ,string))
     (setq ,ret (yas/format-match-$ ,ret ,string))
     (symbol-value ',ret)
     )))
(defun yas/replace-match (text &optional string subexp)
  "* yas/replace-match is similar to emacs replace-match but using Texmate formats"
  (replace-match (yas/format-match text string) 't 't string subexp))
;(defmacro yas/replace-match (match &optional string)
;  "* Replaces match Texmate style."
;  )
(defun yas/t/ (texmate-reg texmate-rep &optional texmate-option t-text)
  "* Texmate like mirror.  Uses texmate regular expression and texmate formatting."
  (let (
        (ret (or t-text yas/text))
        (start 0)
        (fix "")
        (reg (texmate-regexp-to-emacs-regexp texmate-reg))
        mtch
        )
    (cond
     ( (string-match "[gG]" texmate-option) ;; Global replace
       (while (string-match reg ret start)
         (setq mtch (yas/format-match texmate-rep ret))
         (setq ret (replace-match mtch 't 't ret))
         (setq start (+ (match-beginning 0) (length mtch)))
         (setq ret (replace-match mtch 't 't ret))
         )
       )
     ( 't ;; Replace first occurrence
       (when (string-match reg ret)
         (setq ret (yas/replace-match texmate-rep ret))
         )
       )
     )
    (symbol-value 'ret)
    )
  )
(defvar yas/t-lst '()
  "Variable for expanding texmate transformations with Yasnippet")

;(texmate-import-rmate "c:/tmp/swissr-rmate.tmbundle-v0.4.2-0-g7d026da/swissr-rmate.tmbundle-7d026da/")
;(texmate-import-stata "c:/tmp/Stata.tmbundle/")


;;https://github.com/subtleGradient/javascript-tools.tmbundle

(provide 'texmate-import)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; texmate-import.el ends here
