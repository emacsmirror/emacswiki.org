;;; rainbow-delimiters.el --- Colorize nested delimiters: () [] {}
;; Copyright (C) 2010  Jeremy Rayman

;; Author/Maintainer: Jeremy Rayman <jeremy.rayman@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; 1. Place rainbow-delimiters.el on your emacs load-path.
;;
;; 2. Compile the file (necessary for speed):
;; M-x byte-compile-file <location of rainbow-delimiters.el>
;;
;; 3. Add the following to your dot-emacs/init file:
;; (require 'rainbow-delimiters)
;;
;; 4. Add hooks for modes where you want it enabled, for example:
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;;
;; - To activate rainbow-delimiters mode temporarily in a buffer:
;; M-x rainbow-delimiters-mode

;;; Customization:

;; To customize options including faces for each type of delimiter:
;; M-x customize-group rainbow-delimiters
;;
;; All-delimiter faces group:
;; The group of faces called 'rainbow-parens-all-delimiters-faces'
;; allow you to make changes to the coloring of all types of
;; delimiters at once. Each type of delimiter inherits its faces
;; from this all-delimiter face group.
;;
;; Changing color scheme for a single type of delimiter:
;; If you prefer a separate color scheme for a certain delimiter,
;; e.g. you want brackets to have a different color scheme,
;; you can simply add the attributes you want to the faces for
;; that type of delimiter. Any changes made to delimiter-specific
;; faces will override the defaults provided by the
;; 'rainbow-delimiters-all-delimiters-faces' group.
;; You don't need to remove the :inherit option, and you don't
;; have to change all the face colors - if you only want to change
;; the color for depth 1, for example, you can do so and leave
;; the others untouched. The other faces will continue to use the
;; default colors.
;;
;; Color-theme.el users:
;; If you use the color-theme package, you can specify custom colors
;; by adding the appropriate faces to your theme.
;; - If you want to use  a single color scheme for all delimiter types,
;;   add faces of the form:
;;   rainbow-delimiters-all-delimiters-depth-1-face
;;   With depths numbered 1 through 12.
;; - If you want to have individual color schemes for each type of
;;   delimiter, those face names have the following format:
;;   rainbow-delimiters-paren-depth-1-face
;;   rainbow-delimiters-bracket-depth-1-face
;;   rainbow-delimiters-brace-depth-1-face
;;   With each type of delimiter having 12 faces total, numbered 1 to 12.
;; You can mix-and-match: customize the 'all-delimiters' faces with your
;; own color scheme, then add any specific changes for a certain depth
;; of an individual delimiter by just including that one face in your
;; color-theme.el.

;;; Commentary:

;; This is a "rainbow parentheses" mode which includes support for
;; parens "()", brackets "[]", and braces "{}". It conveys nesting
;; depth by using a different face for each level. It correctly
;; colorizes statements of the same depth - if two statements are the
;; same level, they will be the same color.

;; Great care has been taken to make this mode FAST. You should see
;; no discernable change in scrolling or editing speed when using it
;; with delimiter-rich languages such as Clojure, Lisp and Scheme.
;;
;; The only exception is with extremely large nested data structures
;; having hundreds of delimiters; in that case there will be a brief
;; pause to colorize the structure the very first time it is
;; displayed on screen; from then on editing this structure will
;; perform at full speed.

;; Default colors come from ZenBurn vim/emacs theme, available here:
;; http://slinky.imukuppi.org/zenburnpage/

;; Help would be appreciated in improving the default color scheme.
;; Since Emacs does not currently have color correction, colors
;; appear differently wide gamut displays compared to how they look
;; on most SRGB monitors. I use a wide-gamut display; I have tried
;; to select the best default colors I can from popular SRGB themes.
;; If you make a change to the default color scheme, please send it
;; to (Jeremy Rayman, jeremy.rayman.public@gmail.com).  A list of
;; colors in any form would be fine; a proper patch is not required.
;; This would be a useful contribution.

;;; Changelog:
;; 1.0 - Initial public release.
;; 1.1 - Stop tracking each delimiter's depth independently.
;;       This had lead to confusing results when viewing clojure
;;       code. Instead, just color based on current nesting inside
;;       of all delimiters combined.
;;     - Add 'all-delimiters' faces to apply a color scheme to all
;;       delimiter faces at once. Delimiter specific faces now
;;       inherit from this group.

;;; TODO:
;; - Provide option to colorize unmatched delimiters with a special face.
;; - Add support for nested tags (XML, HTML)

;;; Issues:
;; - Rainbow-delimiters does not change the appearance of delimiters when
;;   org-mode is enabled. Cause is unknown.

;;; Code:

(eval-when-compile (require 'cl))


;;; Customize interface:

(defgroup rainbow-delimiters nil
  "Color nested sets of delimiters according to depth."
  :prefix "rainbow-delimiters-"
  :link '(url-link :tag "Website for rainbow-delimiters (EmacsWiki)"
                   "http://www.emacswiki.org/emacs/RainbowDelimiters")
  :group 'applications)

(defgroup rainbow-delimiters-all-delimiters-faces nil
  "Color scheme for all delimiters; delimiter-specific faces inherit these.

Changing a color here will make it default for all delimiter types.
You can still change the faces in an individual delimiter's group if you want
a special color scheme for it."
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :prefix 'rainbow-delimiters-all-delimiters-faces-)

(defgroup rainbow-delimiters-paren-faces nil
  "Faces to use in coloring parentheses.

Attributes set here override the defaults inherited from the all-delimiters group."
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :prefix 'rainbow-delimiters-paren-faces-)

(defgroup rainbow-delimiters-bracket-faces nil
  "Faces to use in coloring brackets.

Attributes set here override the defaults inherited from the all-delimiters group."
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :prefix 'rainbow-delimiters-bracket-faces-)

(defgroup rainbow-delimiters-brace-faces nil
  "Faces to use in coloring braces.

Attributes set here override the defaults inherited from the all-delimiters group."
  :group 'rainbow-delimiters
  :link '(custom-group-link "rainbow-delimiters")
  :prefix 'rainbow-delimiters-brace-faces-)


;;; Faces for colorizing delimiters at each level:

;; NOTE: The use of repetitious definitions for depth faces is temporary.
;; Once the emacs 24 color theme support comes in, this will be reevaluated.

;; All Delimiters - face that all other delimiters inherit from.
;; Changing the all-delimiters faces affect the faces of all delimiters.
(defface rainbow-delimiters-all-delimiters-depth-1-face
  '((t (:foreground "grey55")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 1 - the outermost pair."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-2-face
  '((t (:foreground "#7F9F7F")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 2."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-3-face
  '((t (:foreground "#8CD0D3")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 3."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-4-face
  '((t (:foreground "#DCA3A3")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 4."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-5-face
  '((t (:foreground "#385F38")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 5."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-6-face
  '((t (:foreground "#F0DFAF")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 6."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-7-face
  '((t (:foreground "#BCA3A3")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 7."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-8-face
  '((t (:foreground "#C0BED1")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 8."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-9-face
  '((t (:foreground "#FFCFAF")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 9."
  :group 'rainbow-delimiters-all-delimiters-faces)

;; Emacs doesn't sort face names by number correctly above 1-9; trick it into
;; proper sorting by prepending a _ before the faces with depths over 10.
(defface rainbow-delimiters-all-delimiters-depth-_10-face
  '((t (:foreground "#F0EFD0")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 10."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-_11-face
  '((t (:foreground "#F0DFAF")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 11."
  :group 'rainbow-delimiters-all-delimiters-faces)

(defface rainbow-delimiters-all-delimiters-depth-_12-face
  '((t (:foreground "#DFCFAF")))
  "Face inherited by all delimiters in rainbow-delimiters mode, depth 12."
  :group 'rainbow-delimiters-all-delimiters-faces)

;; Variable aliases for all-delimiters faces 10+:
;; We prepend an underline to numbers 10+ to force customize to sort correctly.
;; Here we define aliases without the underline for use everywhere else.
(put 'rainbow-delimiters-all-delimiters-depth-10-face
     'face-alias
     'rainbow-delimiters-all-delimiters-depth-_10-face)
(put 'rainbow-delimiters-all-delimiters-depth-11-face
     'face-alias
     'rainbow-delimiters-all-delimiters-depth-_11-face)
(put 'rainbow-delimiters-all-delimiters-depth-12-face
     'face-alias
     'rainbow-delimiters-all-delimiters-depth-_12-face)


;; Parentheses - ()

(defface rainbow-delimiters-paren-depth-1-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-1-face)))
  "Face for rainbow-delimiters mode `paren' depth 1, the outermost pair."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-2-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-2-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 2."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-3-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-3-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 3."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-4-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-4-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 4."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-5-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-5-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 5."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-6-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-6-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 6."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-7-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-7-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 7."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-8-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-8-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 8."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-9-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-9-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 9."
  :group 'rainbow-delimiters-paren-faces)

;; Emacs doesn't sort face names by number correctly above 1-9; trick it into
;; proper sorting by prepending a _ before the faces with depths over 10.
(defface rainbow-delimiters-paren-depth-_10-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-10-face))) ;; HERE
  "Face for rainbow-delimiters mode `paren' nested depth 10."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-_11-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-11-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 11."
  :group 'rainbow-delimiters-paren-faces)

(defface rainbow-delimiters-paren-depth-_12-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-12-face)))
  "Face for rainbow-delimiters mode `paren' nested depth 12."
  :group 'rainbow-delimiters-paren-faces)


;; Brackets - []

(defface rainbow-delimiters-bracket-depth-1-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-1-face)))
  "Face for rainbow-delimiters mode `bracket' depth 1, the outermost pair."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-2-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-2-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 2."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-3-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-3-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 3."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-4-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-4-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 4."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-5-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-5-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 5."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-6-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-6-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 6."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-7-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-7-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 7."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-8-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-8-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 8."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-9-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-9-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 9."
  :group 'rainbow-delimiters-bracket-faces)

;; Emacs doesn't sort face names by number correctly above 1-9; trick it into
;; proper sorting by prepending a _ before the faces with depths over 10.
(defface rainbow-delimiters-bracket-depth-_10-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-10-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 10."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-_11-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-11-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 11."
  :group 'rainbow-delimiters-bracket-faces)

(defface rainbow-delimiters-bracket-depth-_12-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-12-face)))
  "Face for rainbow-delimiters mode `bracket' nested depth 12."
  :group 'rainbow-delimiters-bracket-faces)


;; Braces - {}

(defface rainbow-delimiters-brace-depth-1-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-1-face)))
  "Face for rainbow-delimiters mode `brace' depth 1, the outermost pair."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-2-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-2-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 2."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-3-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-3-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 3."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-4-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-4-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 4."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-5-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-5-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 5."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-6-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-6-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 6."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-7-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-7-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 7."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-8-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-8-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 8."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-9-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-9-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 9."
  :group 'rainbow-delimiters-brace-faces)

;; Emacs doesn't sort face names by number correctly above 1-9; trick it into
;; proper sorting by prepending a _ before the faces with depths over 10.
(defface rainbow-delimiters-brace-depth-_10-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-10-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 10."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-_11-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-11-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 11."
  :group 'rainbow-delimiters-brace-faces)

(defface rainbow-delimiters-brace-depth-_12-face
  '((t (:inherit rainbow-delimiters-all-delimiters-depth-12-face)))
  "Face for rainbow-delimiters mode `brace' nested depth 12."
  :group 'rainbow-delimiters-brace-faces)


;;; Aliases for faces over depth 10:

;; Because Emacs doesn't sort face names by number correctly above 1-9, we
;; trick it into proper sorting by prepending a _ before the face number
;; for faces with depths over 10. Here we define aliases without the underline
;; for use outside the customize interface.

;; Parentheses
(put 'rainbow-delimiters-paren-depth-10-face
     'face-alias
     'rainbow-delimiters-paren-depth-_10-face)
(put 'rainbow-delimiters-paren-depth-11-face
     'face-alias
     'rainbow-delimiters-paren-depth-_11-face)
(put 'rainbow-delimiters-paren-depth-12-face
     'face-alias
     'rainbow-delimiters-paren-depth-_12-face)
;; Brackets
(put 'rainbow-delimiters-bracket-depth-10-face
     'face-alias
     'rainbow-delimiters-bracket-depth-_10-face)
(put 'rainbow-delimiters-bracket-depth-11-face
     'face-alias
     'rainbow-delimiters-bracket-depth-_11-face)
(put 'rainbow-delimiters-bracket-depth-12-face
     'face-alias
     'rainbow-delimiters-bracket-depth-_12-face)
;; Braces
(put 'rainbow-delimiters-brace-depth-10-face
     'face-alias
     'rainbow-delimiters-brace-depth-_10-face)
(put 'rainbow-delimiters-brace-depth-11-face
     'face-alias
     'rainbow-delimiters-brace-depth-_11-face)
(put 'rainbow-delimiters-brace-depth-12-face
     'face-alias
     'rainbow-delimiters-brace-depth-_12-face)


;;; Face utility functions

;; inlining this function for speed:
;; see: http://www.gnu.org/s/emacs/manual/html_node/elisp/Compilation-Tips.html
;; this will cause problems with debugging. To debug, change defsubst -> defun.
(defsubst rainbow-delimiters-depth-face (delim-type depth)
  "Return face name corresponding to DELIM-TYPE and DEPTH.

DELIM-TYPE is a keyword, one of :paren :bracket :brace.
DEPTH is the number of nested levels deep for the delimiter being colorized.

Returns a face of the form 'rainbow-delimiters-DELIM-TYPE-depth-DEPTH-face',
e.g. 'rainbow-delimiters-paren-depth-1-face'."
  (concat "rainbow-delimiters-" (substring-no-properties
                                 (symbol-name delim-type) 1)
          "-depth-" (number-to-string depth) "-face"))


;;; Nesting level

;; syntax-table: recognize parens, brackets, braces; used with parse-partial-sexp.
(defvar rainbow-delimiters-delim-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    table)
  "Syntax table for counting brace depth, ignoring other delimiter types.")

(defun rainbow-delimiters-depth (point)
  "Return 3-elt list of nesting depths at POINT for parens, brackets, and braces.

Return depths as a list of the form (paren-depth, bracket-depth, brace-depth)."
  (save-excursion
      (beginning-of-defun)
      (with-syntax-table rainbow-delimiters-delim-syntax-table
               (car (parse-partial-sexp (point) point)))))


;;; Text properties

;; inlining this function for speed:
;; see: http://www.gnu.org/s/emacs/manual/html_node/elisp/Compilation-Tips.html
;; this will cause problems with debugging. To debug, change defsubst -> defun.
(defsubst rainbow-delimiters-propertize-delimiter (point delim-type depth)
  "Colorize DELIM-TYPE at POINT according to DEPTH.

POINT is the point of character to propertize.
DELIM-TYPE specifies which delimiter is being colorized for face selection.
DEPTH specifies which face number depth to select.

Sets text properties:
`font-lock-face' to the corresponding delimiter face.
`rear-nonsticky' to prevent color from bleeding into subsequent characters typed by the user."
  (with-silent-modifications
    (let ((delim-face (rainbow-delimiters-depth-face delim-type depth)))
      ;; (when (eq depth -1) (message "Unmatched delimiter at char %s." point))
      (add-text-properties point (1+ point)
                           `(font-lock-face ,delim-face
                             rear-nonsticky t)))))


(defun rainbow-delimiters-unpropertize-delimiter (point)
  "Remove text properties set by rainbow-delimiters mode from char at POINT."
  (remove-text-properties point (1+ point)
                          '(font-lock-face nil
                            rear-nonsticky nil)))


(defun rainbow-delimiters-char-ineligible-p (point)
  "Return t if char at POINT should be skipped, e.g. if inside a comment.

Returns t if char at point meets one of the following conditions:
- Inside a string.
- Inside a comment.
- Is an escaped char, e.g. ?\)"
  (let ((parse-state (save-excursion
                       (beginning-of-defun)
                       ;; (point) is at beg-of-defun; point is the char location
                       (parse-partial-sexp (point) point))))
    (or
     (nth 3 parse-state)                ; inside string?
     (nth 4 parse-state)                ; inside comment?
     (and (eq (char-before point) ?\\)  ; escaped char, e.g. ?\) - not counted
          (and (not (eq (char-before (1- point)) ?\\)) ; special-case: ignore ?\\
               (eq (char-before (1- point)) ?\?))))))
;; standard char read syntax '?)' is not tested for because emacs manual states
;; that punctuation such as delimiters should _always_ use escaped '?\)' form.


;;; JIT-Lock functionality

;; Used to skip delimiter-by-delimiter `rainbow-delimiters-propertize-region'.
(defvar rainbow-delimiters-delim-regex "\\(\(\\|\)\\|\\[\\|\\]\\|\{\\|\}\\)"
  "Regex matching all opening and closing delimiters we intend to colorize.")

;; main function called by jit-lock:
(defun rainbow-delimiters-propertize-region (start end)
  "Colorize delimiters in region between START and END.

Used by jit-lock for dynamic highlighting."
  (save-excursion
    (goto-char start)
    ;; START can be anywhere in buffer; begin depth counts from values at START.
    (let* ((depth (rainbow-delimiters-depth start)))
      (while (and (< (point) end)
                  (re-search-forward rainbow-delimiters-delim-regex end t))
        (backward-char) ; re-search-forward places point after delim; go back.
        (unless (rainbow-delimiters-char-ineligible-p (point))
          (let ((delim (char-after (point))))
            (cond ((eq ?\( delim)       ; (
                   (setq depth (1+ depth))
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            :paren
                                                            depth))
                  ((eq ?\) delim)       ; )
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            :paren
                                                            depth)
                   (setq depth (1- depth)))
                  ((eq ?\[ delim)       ; [
                   (setq depth (1+ depth))
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            :bracket
                                                            depth))
                  ((eq ?\] delim)       ; ]
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            :bracket
                                                            depth)
                   (setq depth (1- depth)))
                  ((eq ?\{ delim)       ; {
                   (setq depth (1+ depth))
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            :brace
                                                            depth))
                  ((eq ?\} delim)       ; }
                   (rainbow-delimiters-propertize-delimiter (point)
                                                            :brace
                                                            depth)
                   (setq depth (1- depth))))))
        ;; move past delimiter so re-search-forward doesn't pick it up again
        (forward-char)))))

(defun rainbow-delimiters-unpropertize-region (start end)
  "Remove colorizing text properties from all delimiters in buffer."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward rainbow-delimiters-delim-regex end t))
      ;; re-search-forward places point 1 further than the delim matched:
      (rainbow-delimiters-unpropertize-delimiter (1- (point))))))


;;; Minor mode:

(define-minor-mode rainbow-delimiters-mode
  "Colorize nested delimiters according to depth. Works with (), [], {}."
  nil "" nil ; No modeline lighter - it's already obvious when the mode is on.
  (if (not rainbow-delimiters-mode)
      (progn
        (jit-lock-unregister 'rainbow-delimiters-propertize-region)
         (rainbow-delimiters-unpropertize-region (point-min) (1- (point-max))))
    (jit-lock-register 'rainbow-delimiters-propertize-region t)))


(provide 'rainbow-delimiters)

;;; Other possible delimiter colors to use: (wide-gamut)
;; "#7f7f7f"
;; "#7f7f91"
;; "#7f7fa1"
;; "#95aabc"
;; "#9d9d9d"
;; "#9d9d9d"
;; "#8d929b"
;; "#7f8f9d"
;; "#73818c"
;; "#91b3d0"
;; "#91b3d0"
;; "#91b3d0"
;; "#7ea7c9"
;; "#6b9ac2"
;; "gray100"
;; "gray85"
;; "gray70"
;; "#6093be"
;; "#588dba"

;;; Excellent, subtle set of colors for 92% wide gamut screens:
;;; (e.g. HP LP3065)
;;   "grey55"
;;   "#7f967f"
;;   "#7199a1"
;;   "#917f7f"
;;   "#91937f"
;;   "#7f9691"
;;   "#949191"
;;   "#919991"
;;   "#949194"
;;   "#949494"

;; personal/custom design for default srgb colors:
;; #2B5469 - deep tone blue
;; #69402B - brown

;;; rainbow-delimiters.el ends here


