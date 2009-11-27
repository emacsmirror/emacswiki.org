;;; screencast-nxml.el
;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: esbenandreasen <esbenandreasen@gmail.com>(new)

;; Keywords: screencast

;; This file is not an official part of emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Code:


(require 'screencast-record)
(require 'rng-valid)
(require 'nxml-mode)
(defun screencast-nxml-mode (&optional arg)
  (interactive "P")
  (apply (if arg
             'screencast-record
           'screencast)
         '(
           "Hello, and welcome to the screencast for using the nxml-mode in Emacs."n
           p
           "The mode has completion, validation, formatting and outlining capabilities."n
           "Let's load the mode and see how it is used:"n
           (nxml-mode)
           "The xml-declaration - according to the buffer file-coding system:"
           (nxml-insert-xml-declaration)
           "Let's create a html-document."n
           (i "<html ")
           (progn (rng-validate-clear)
                  (message ""))
           "Notice the red underline, that's a wellformedness error (more about errors later)." n
           "We want html validation and completion also - the mode can guess the schema to load"
           (rng-auto-set-schema-and-validate)
           (i "xmln")
           "Attribute completions:"
           (nxml-complete)
           "(if you want the standard Emacs context-completion key binding, you can set the variable nxml-bind-meta-tab-to-complete-flag to a non-nil value)"n
           "Namespace completion:"
           (nxml-complete)
           "Insert an end tag, and put point between the start and end tag on a new line."
           (nxml-balanced-close-start-tag-block)
           b
           (i "<h")
           "We can complete upon tag-names as well:"
           (nxml-complete)
           "Unlike before, we want the end tag to be on the same line:"
           (nxml-balanced-close-start-tag-inline)
           "We can also close the nearest unclosed tag:"
           (i "<title>My title")
           (nxml-finish-element)
           b
           "The standard s-expression navigation is also possible between tags:"
           (forward-sexp)
           (backward-sexp)
           "(As are the kill- and mark-sexp)"n
           "An improvement is navigation in the tree-structure:"
           (i "<link/>")
           (nxml-backward-up-element)
           (nxml-down-element)
           b
           "We still have a red underline, let's see what is wrong (minibuffer):"
           (rng-next-error 1)
           p p p
           "There's one more way to close tags, the nxml-slash-auto-complete-flag, we set it to a non-nil value. Now a \"</\" will end the closest tag:" 
           (progn (setq nxml-slash-auto-complete-flag t))
           (i "<body>\n\n<")
           (screencast-producer-set-last-char "/" '(nxml-electric-slash nil))  
           b
           "Let's put some content:"n
           (progn
             (newline)
             (previous-line 2)
             (insert
              "<div>
<h1>Lorem Ipsum</h1>
Lorem ipsum dolor sit amet,
consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis 
nostrud exercitation 
ullamco laboris nisi ut aliquip ex ea commodo
consequat. <b>Duis</b> aute irure
dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum.
</div>
<div>
<h1>Dolor sit</h1>
</div>")
             (indent-region (point-min) (point-max))
             (previous-line 14)
             (forward-word 1)
             (backward-word 1)
             )
           "The text-block is a mess, fortunately we can compact-uncompact it - _within_ the tags."
           (compact-uncompact-block)
           (compact-uncompact-block)
           (progn (search-forward "</h1>" (point-max) nil 2)
                  (goto-char (match-beginning 0))
                  )
           b
           "We would like to copy the div-block, and therefore use split to copy the h1" n
           "We will repeat the split immediately, and since last command was also split, the new split will extend the old split (exactly like killing text multiple times):"
           (nxml-split-element)
           (screencast-producer-set-last-command  '(nxml-split-element) 'nxml-split-element)
           "(notice how the cursor position remains the same in the new div-block)"n
           "Another useful copy-like feature is auto completion of surrounding tags."n
           "The function looks at which tags surround the other occurrences of the word before point."
           (i "Duis")
           (screencast-producer-blink-regions
            (list (cons (- (point) 4) (point))
                  (save-excursion
                    (search-backward "<b>Duis</b>")
                    (cons (match-beginning 0) (match-end 0)))))
           (nxml-dynamic-markup-word)
           b
           "We also have easy access to Unicode characters. "
           ;; we have to wrap to avoid the interactive prompt.
           (flet ((nxml-insert-named-char
                   (name arg)
                   (screencast-nxml-insert-named-char name arg)
                   )
                  )
             (nxml-insert-named-char "ANKH" nil)
             "(The glyph is _not_ present in the file - it is present for your eyes only)"
             n
             "You can also insert the glyph itself by using the prefix-argument:"
             (nxml-insert-named-char "ANKH" t)
             )
           b
           "Finally, nxml-mode makes use of outline-minor-mode, where certain tags make up headings."n
           "And other tags make up the titles to put in those headings."n
           "But we have to set which those are first :("n
           "(setq nxml-section-element-name-regexp \"html\\\\|body\\\\|div\")"n
           "(setq nxml-heading-element-name-regexp \"title\\\\|head\\\\|h1\")"n
           (progn (setq nxml-section-element-name-regexp "html\\|body\\|div")
                  (setq nxml-heading-element-name-regexp "title\\|head\\|h1"))
           "There's 11 outline-commands in total, please see the outline-mode screencast for full explanation"n
           "We can hide \"everything else\":"
           (nxml-hide-other)
           "(notice that you can use the mouse on collapsed elements)"n
           "(notice the titles of the collapsed elements)"n
           "Let's show everything again:"
           (nxml-show-all)
           "For a quick overview, it is useful to hide all text:"
           (nxml-hide-all-text-content)
           b
           "That's it! "n
           "One last note: if you ever need to put tables in html, take a look at the screencast \"Can your editor do this - explained\"."
           )
         "nxml-mode"
         1.1
         ())
  )


(defun screencast-nxml-insert-named-char (name arg)
  "Non-interactive wrapper."
  (mapcar 'nxml-maybe-load-char-name-set nxml-autoload-char-name-set-list)
  (let (
        (alist nxml-char-name-alist)
        elt code)
    (while (and alist (not code)
      (setq elt (assoc name alist))
      (if (get (cddr elt) 'nxml-char-name-set-enabled)
          (setq code (cadr elt))
        (setq alist (cdr (member elt alist)))))
    (when code
      (insert (if arg
                  (or (decode-char 'ucs code)
                      (error "Character %x is not supported by Emacs"
                             code))
                (format "&#x%X;" code)))))))
