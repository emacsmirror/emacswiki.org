;;; toggle.el --- quickly open corresponding file (eg test vs impl).

;; Copyright (C) 2006-2007 by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Version 1.3.1
;; Keywords: files, extensions, convenience
;; Created: 2006-03-22
;; Compatibility: Emacs 22, 21?
;; URL(en): http://seattlerb.rubyforge.org/

;;; Posted using:
;; (setq emacs-wiki-name "RyanDavis")
;; (wikiput-buffer "update")

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package provides the ability to quickly open a corresponding
;; file for the current buffer by using a bi-directional mapping of
;; regular expression pairs. You can select a mapping style from
;; `toggle-mapping-styles' using the `toggle-style' function or set
;; your default style via the `toggle-mapping-style' variable.

;; There are 4 different mapping styles in this version: zentest,
;; rails, and ruby. Feel free to submit more and I'll incorporate
;; them.

;;; Example Mapping (ruby style):
;;
;; blah.rb <-> test_blah.rb
;; lib/blah.rb <-> test/test_blah.rb

;;; History:

;; 1.3.1 2008-09-25 Fixed doco & typo in rspec patterns.
;; 1.3.0 2007-05-10 Added tab completion to toggle-style. Suggested by TingWang.
;; 1.2.0 2007-04-06 Interleave bidirectional mappings. Fixed interactive setter.
;;                  Added rspec mappings.
;; 1.1.0 2007-03-30 Initial release to emacswiki.org. Added named styles and bidi.
;; 1.0.0 2006-03-22 Birfday.

(require 'cl)

;; TODO:
;; It would also be great to be able to toggle between a model and
;; the equivalent controller:
;;   app/models/\\1.rb     => app/controllers/\\1.rb
;; and from the controller to views:
;;   app/controller/\\1.rb => app/views/\\1.rb/
;;
;; I can't do that yet... but we should be able to make the mapping
;; styles list any number of patterns to rotate through. I'll talk to
;; PhilHagelberg about it and try it.

(defcustom toggle-mapping-styles
  '((zentest . (("app/controllers/\\1.rb" . "test/controllers/\\1_test.rb")
                ("app/views/\\1.rb"       . "test/views/\\1_test.rb")
                ("app/models/\\1.rb"      . "test/unit/\\1_test.rb")
                ("lib/\\1.rb"             . "test/unit/test_\\1.rb")))
    (rspec   . (("app/models/\\1.rb"      . "spec/models/\\1_spec.rb")
                ("app/controllers/\\1.rb" . "spec/controllers/\\1_spec.rb")
                ("app/views/\\1.rb"       . "spec/views/\\1_spec.rb")
                ("app/helpers/\\1.rb"     . "spec/helpers/\\1_spec.rb")))
    (rails   . (("app/controllers/\\1.rb" . "test/functional/\\1_test.rb")
                ("app/models/\\1.rb"      . "test/unit/\\1_test.rb")
                ("lib/\\1.rb"             . "test/unit/test_\\1.rb")))
    (ruby    . (("lib/\\1.rb"             . "test/test_\\1.rb")
                ("\\1.rb"                 . "test_\\1.rb"))))
  "A list of (name . toggle-mapping) rules used by toggle-filename."
  :group 'toggle
  :type '(repeat (cons string string)))

(defcustom toggle-mapping-style
  'ruby
  "The default toggle mapping style to load when initialized."
  :group 'toggle
  :type '(symbol))

(defun toggle-style (name)
  (interactive (list (completing-read "Style: "
                                      (mapcar
                                       #'symbol-name
                                       (mapcar #'car toggle-mapping-styles))
                                      nil t "")))
  (let* ((style (if (stringp name) (intern name) name))
         (pairs (cdr (assoc style toggle-mapping-styles))))
    (if pairs
        (let ((mappings
               (mapcar (lambda (pair)
                         (cons
                          (replace-regexp-in-string
                           "\\\\1" "\\\\(.*\\\\)"
                           (replace-regexp-in-string ; special case for "\\1.ext"
                            "^\\\\1" "\\\\([^/]*\\\\)" (car pair)))
                          (cdr pair)))
                       (mapcan 'list
                               pairs
                               (mapcar (lambda (pair)
                                         (cons (cdr pair) (car pair)))
                                       pairs)))))
          (if (interactive-p)
              (setq toggle-mappings mappings)
            mappings))
      nil)))

(defvar toggle-mappings (toggle-style toggle-mapping-style)
  "*The current file mappings for `toggle-filename' to use.")

(defun toggle-filename (path rules)
  "Transform a matching subpath in PATH as given by RULES.
Each element in RULES is a pair (RE . TRANS). If the regular
expression RE matches PATH, then replace-match is invoked with
TRANS. After the first successful match, this returns. If no rule
matches, it returns nil"
  (cond ((null rules) nil)
    ((string-match (caar rules) path)
     (replace-match (cdar rules) nil nil path))
    (t (toggle-filename path (rest rules)))))

(defun toggle-buffer ()
  "Opens a related file to the current buffer using matching rules.
Matches the current buffer against rules in toggle-mappings. If a
match is found, switches to that buffer."
  (interactive)
  (let ((new-name (toggle-filename (buffer-file-name) toggle-mappings)))
    (if new-name
        (find-file new-name)
      (message (concat "Match not found for " (buffer-file-name))))))

(provide 'toggle)
