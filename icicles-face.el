;;; icicles-face.el --- Faces for Icicles
;;
;; Filename: icicles-face.el
;; Description: Faces for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 1996-2017, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 09:19:43 2006
;; Last-Updated: Wed Jul 26 08:18:37 2017 (-0700)
;;           By: dradams
;;     Update #: 744
;; URL: https://www.emacswiki.org/emacs/download/icicles-face.el
;; Doc URL: https://www.emacswiki.org/emacs/Icicles
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines
;;  customization groups and faces.  For Icicles documentation, see
;;  `icicles-doc1.el' and `icicles-doc2.el'.
;;
;;  Groups defined here:
;;
;;    `Icicles', `Icicles-Buffers', `Icicles-Completions-Display',
;;    `Icicles-Files', `Icicles-Key-Bindings',
;;    `Icicles-Key-Completion', `Icicles-Matching',
;;    `Icicles-Minibuffer-Display', `Icicles-Miscellaneous',
;;    `Icicles-Searching'.
;;
;;  Faces defined here:
;;
;;    `icicle-annotation', `icicle-candidate-part',
;;    `icicle-common-match-highlight-Completions',
;;    `icicle-complete-input', `icicle-completion',
;;    `icicle-Completions-instruction-1',
;;    `icicle-Completions-instruction-2',
;;    `icicle-current-candidate-highlight', `icicle-extra-candidate',
;;    `icicle-historical-candidate',
;;    `icicle-historical-candidate-other',
;;    `icicle-input-completion-fail',
;;    `icicle-input-completion-fail-lax', `icicle-key-complete-menu',
;;    `icicle-key-complete-menu-local',
;;    `icicle-match-highlight-Completions',
;;    `icicle-match-highlight-minibuffer', `icicle-mode-line-help',
;;    `icicle-msg-emphasis', `icicle-multi-command-completion',
;;    `icicle-mustmatch-completion', `icicle-proxy-candidate',
;;    `icicle-saved-candidate', `icicle-search-context-level-1',
;;    `icicle-search-context-level-2',
;;    `icicle-search-context-level-3',
;;    `icicle-search-context-level-4',
;;    `icicle-search-context-level-5',
;;    `icicle-search-context-level-6',
;;    `icicle-search-context-level-7',
;;    `icicle-search-context-level-8', `icicle-search-current-input',
;;    `icicle-search-main-regexp-current',
;;    `icicle-search-main-regexp-others', `icicle-special-candidate',
;;    `icicle-whitespace-highlight', `minibuffer-prompt'.
;;
;;  For descriptions of changes to this file, see `icicles-chg.el'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  https://www.emacswiki.org/emacs/download/linkd.el.
;;
;;  (@> "Icicles Commands for Other Packages")
;;  (@> "Groups, Organized Alphabetically")
;;  (@> "Faces, Organized Alphabetically")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Groups, Organized Alphabetically")

;;; Groups, Organized Alphabetically ---------------------------------

(defgroup Icicles nil
  "Minibuffer input completion and cycling of completion candidates."
  :prefix "icicle-"
  :group 'completion :group 'convenience :group 'help :group 'apropos
  :group 'dabbrev :group 'matching :group 'minibuffer :group 'recentf
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Buffers nil
  "Icicles preferences related to buffers."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Files nil
  "Icicles preferences related to files."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Completions-Display nil
  "Icicles preferences related to display of completion candidates."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Key-Bindings nil
  "Icicles preferences related to key bindings."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Key-Completion nil
  "Icicles preferences related to key completion (`icicle-complete-keys')."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Matching nil
  "Icicles preferences related to matching input for completion."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Minibuffer-Display nil
  "Icicles preferences related to minibuffer display during completion."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Miscellaneous nil
  "Miscellaneous Icicles preferences."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )

(defgroup Icicles-Searching nil
  "Icicles preferences related to searching."
  :prefix "icicle-" :group 'Icicles
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle"
                   ".com?subject=icicles.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Icicles library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "https://www.emacswiki.org/emacs/download/icicles.el")
  :link '(url-link :tag "Description"
          "https://www.emacswiki.org/emacs/Icicles")
  :link '(emacs-commentary-link :tag "Doc-Part2" "icicles-doc2")
  :link '(emacs-commentary-link :tag "Doc-Part1" "icicles-doc1")
  )
 
;;(@* "Faces, Organized Alphabetically")

;;; Faces, Organized Alphabetically ----------------------------------

(defface icicle-annotation              ; Same grays as for `shadow'.
    '((((background dark)) (:foreground "gray70"))
      (t (:foreground "gray50")))
  "*Face used to highlight a candidate annotation in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-candidate-part
    '((((background dark)) (:background "#451700143197")) ; a very dark magenta
      (t (:background "#EF84FFEAF427"))) ; A light green.
  "*Face used to highlight part(s) of a candidate in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-common-match-highlight-Completions
    '((((background dark)) (:foreground "#2017A71F2017")) ; a dark green
      (t (:foreground "magenta3")))
  "*Face used to highlight candidates common match, in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-complete-input
    '((((background dark)) (:foreground "#B19E6A64B19E")) ; a dark magenta
      (t (:foreground "DarkGreen")))
  "*Face used to highlight input when it is complete."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-completion
    '((((background dark)) (:foreground "#0000D53CD53C")) ; a dark cyan
      (t (:foreground "Red")))          ; red
  "*Face used to indicate minibuffer completion.
It highlights the minibuffer indicator and the `Icy' minor-mode
lighter during completion.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'Icicles-Miscellaneous :group 'faces)

(defface icicle-Completions-instruction-1
    '((((background dark)) (:foreground "#AC4AAC4A0000")) ; a dark yellow
      (t (:foreground "Blue")))
  "*Face used to highlight first line of `*Completions*' buffer."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-Completions-instruction-2
    '((((background dark)) (:foreground "#0000D53CD53C")) ; a dark cyan
      (t (:foreground "Red")))
  "*Face used to highlight second line of `*Completions*' buffer."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-current-candidate-highlight
    '((((background dark)) (:background "#69D40A460000")) ; a red brown
      (t (:background "CadetBlue1")))
  "*Face used to highlight the current candidate, in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-extra-candidate
    '((((background dark)) (:background "#4517305D0000")) ; a dark brown
      (t (:background "#C847D8FEFFFF"))) ; a light blue
  "*Face used to highlight `*Completions*' candidates that are extra.
This means that they belong to list `icicle-extra-candidates'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-historical-candidate
    '((((background dark)) (:foreground "#DBD599DF0000")) ; a dark orange
      (t (:foreground "Blue")))
  "*Face used to highlight `*Completions*' candidates that have been used."
  :group 'Icicles-Completions-Display :group 'faces)

(when (> emacs-major-version 21)
  (defface icicle-historical-candidate-other '((t (:inherit icicle-historical-candidate
                                                   :underline t)))
    "*Face to highlight `*Completions*' candidates that were used indirectly.
That is, you might or might not have entered these candidates but in
some sense you have used or visited them.  Whether or not such
highlighting is done is governed by option
`icicle-highlight-historical-candidates-flag'.

Example:
Index topics that point to Info nodes that you have visited.  Whether
such highlighting occurs automatically for Info-node candidates is
governed by option `icicle-Info-highlight-visited-nodes'.  But you can
highlight the nodes on demand, using `C-x C-M-l'."
    :group 'Icicles-Completions-Display :group 'faces))

(defface icicle-input-completion-fail
    '((((background dark)) (:background "#22225F5F2222")) ; a dark green
      (t (:foreground "Black" :background "Plum")))
  "*Face for highlighting failed part of input during strict completion."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-input-completion-fail-lax
    '((((background dark)) (:background "#00005E3B5A8D")) ; a dark cyan
      (t (:foreground "Black" :background "#FFFFB8C4BB87")))
  "*Face for highlighting failed part of input during lax completion."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;; By default, these two faces have the same backgrounds as faces
;; `icicle-candidate-part' and `icicle-special-candidate', respectively.  They differ
;; only in being boxed as well.
(when (fboundp 'map-keymap)             ; Emacs 22+.
  (defface icicle-key-complete-menu
      '((((background dark))
         (:background "#451700143197"   ; a very dark magenta
          :box (:line-width 1 :color "#FA6CC847FFFF"))) ; a light magenta box
        (t (:background "#EF84FFEAF427" ; a light green.
            :box (:line-width 1 :color "#34F393F434F3")))) ; a green box
    "*Face used to highlight non-local menu items when completing keys.
Non-local keys that are not menu items are highlighted with face
`icicle-candidate-part'.  Menu items for the current mode (i.e., local
keymap) are highlighted with face `icicle-key-complete-menu-local'."
    :group 'Icicles-Searching :group 'faces)

  (defface icicle-key-complete-menu-local
      '((((background dark))
         (:background "#176900004E0A"   ; a dark blue
          :box (:line-width 1 :color "#E1E1EAEAFFFF"))) ; a light blue box
        (t (:background "#EF47FFFFC847" ; a light yellow.
            :box (:line-width 1 :color "#AC4AAC4A0000")))) ; a dark yellow box
    "*Face used to highlight local menu items when completing keys.
Local keys that are not menu items are highlighted with face
`icicle-special-candidate'.  Non-local menu items (i.e., not for the
current mode) are highlighted with face `icicle-key-complete-menu'."
    :group 'Icicles-Searching :group 'faces))

(defface icicle-match-highlight-Completions
    '((((background dark)) (:foreground "#1F1FA21CA21C")) ; a very dark cyan
      (t (:foreground "Red3")))
  "*Face used to highlight root that was completed, in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-match-highlight-minibuffer '((t (:underline t)))
  "*Face used to highlight root that was completed, in minibuffer."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-mode-line-help
    '((((background dark)) (:foreground "#AC4AAC4A0000")) ; a dark yellow
      (t (:foreground "Blue")))
  "*Face used to highlight help shown in the mode-line."
  :group 'Icicles-Completions-Display :group 'Icicles-Miscellaneous :group 'faces)

(defface icicle-msg-emphasis
    '((((background dark)) (:foreground "#B19E6A64B19E")) ; a dark magenta
      (t (:foreground "DarkGreen")))
  "*Face used to emphasize (part of) a message."
  :group 'Icicles-Minibuffer-Display :group 'faces)

(defface icicle-multi-command-completion
    '((((background dark))              ; a dark cyan on a dark magenta
       (:foreground "#0000D53CD53C" :background "#8B3500007533"))
      (t (:foreground "Red" :background "#78F6FFFF8E4F"))) ; red on a light green
  "*Face used to indicate Icicles multi-command completion.
It highlights the minibuffer indicator and the `Icy+' minor-mode
lighter during multi-command completion.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'Icicles-Miscellaneous :group 'faces)

(defface icicle-mustmatch-completion
    '((((type x w32 mac graphic) (class color))
       (:box (:line-width -2 :color "Blue"))) ; blue box
      (t (:inverse-video t)))
  "*Face used to indicate strict minibuffer completion.
It highlights the minibuffer indicator and the `Icy' or `Icy+'
minor-mode lighter during strict completion.
Not used for versions of Emacs before version 21."
  :group 'Icicles-Minibuffer-Display :group 'Icicles-Miscellaneous :group 'faces)

(defface icicle-proxy-candidate
    '((((background dark)) (:background "#316B22970000")) ; a very dark brown
      (t (:background "#E1E1EAEAFFFF"   ; A light blue.
          :box (:line-width 2 :color "White" :style released-button))))
  "*Face used to highlight proxy candidates in `*Completions*'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-saved-candidate
    '((((background dark)) (:background "gray20")) ; a dark gray
      (t (:background "gray80")))       ; a light gray
  "*Face used to highlight `*Completions*' candidates that have been saved."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-search-main-regexp-current
    '((((background dark)) (:background "#00004AA652F1")) ; a dark cyan
      (t (:background "misty rose")))
  "*Face used to highlight current match of your search context regexp.
This highlighting is done during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-1
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-saturation
                            (hexrgb-increment-hue context-bg 0.80) 0.10)
                           "#071F473A0000"))) ; a dark green
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-saturation
                              (hexrgb-increment-hue context-bg 0.80) 0.10)
                             "#FA6CC847FFFF"))))) ; a light magenta
  "*Face used to highlight level (subgroup match) 1 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-2
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-saturation
                            (hexrgb-increment-hue context-bg 0.40) 0.10)
                           "#507400002839"))) ; a dark red
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-saturation
                              (hexrgb-increment-hue context-bg 0.40) 0.10)
                             "#C847FFFFE423"))))) ; a light cyan
  "*Face used to highlight level (subgroup match) 2 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-3
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-saturation
                            (hexrgb-increment-hue context-bg 0.60) 0.10)
                           "#4517305D0000"))) ; a dark brown
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-saturation
                              (hexrgb-increment-hue context-bg 0.60) 0.10)
                             "#C847D8FEFFFF"))))) ; a light blue
  "*Face used to highlight level (subgroup match) 3 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-4
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-saturation
                            (hexrgb-increment-hue context-bg 0.20) 0.10)
                           "#176900004E0A"))) ; a dark blue
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-saturation
                              (hexrgb-increment-hue context-bg 0.20) 0.10)
                             "#EF47FFFFC847"))))) ; a light yellow
  "*Face used to highlight level (subgroup match) 4 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-5
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-hue context-bg 0.80)
                           "#04602BC00000"))) ; a very dark green
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-hue context-bg 0.80)
                             "#FCFCE1E1FFFF"))))) ; a light magenta
  "*Face used to highlight level (subgroup match) 5 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-6
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-hue context-bg 0.40)
                           "#32F200001979"))) ; a very dark red
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-hue context-bg 0.40)
                             "#E1E1FFFFF0F0"))))) ; a light cyan
  "*Face used to highlight level (subgroup match) 6 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-context-level-7
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-hue context-bg 0.60)
                           "#316B22970000"))) ; a very dark brown
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-hue context-bg 0.60)
                             "#E1E1EAEAFFFF"))))) ; a light blue
  "*Face used to highlight level (subgroup match) 7 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

;; NO AUTOLOAD COOKIE HERE.
(defface icicle-search-context-level-8
    (let ((context-bg  (face-background 'icicle-search-main-regexp-current)))
      `((((background dark))
         (:background ,(if (fboundp 'hexrgb-increment-saturation)
                           (hexrgb-increment-hue context-bg 0.20)
                           "#12EC00003F0E"))) ; a very dark blue
        (t (:background ,(if (fboundp 'hexrgb-increment-saturation)
                             (hexrgb-increment-hue context-bg 0.20)
                             "#F6F5FFFFE1E1"))))) ; a light yellow
  "*Face used to highlight level (subgroup match) 8 of your search context.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-current-input
    '((((background dark))
       (:foreground "White" :background "#7F0D00007F0D")) ; a dark magenta
      (t (:foreground "Black" :background "Green")))
  "*Face used to highlight what your current input matches.
This highlighting is done during Icicles searching whenever
`icicle-search-highlight-context-levels-flag' is non-nil and the
search context corresponds to the entire regexp."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-search-main-regexp-others
    '((((background dark)) (:background "#348608690000")) ; a very dark brown
      (t (:background "CadetBlue1")))
  "*Face used to highlight other matches of your search context regexp.
If user option `icicle-search-highlight-threshold' is less than one,
then this face is not used.
This highlighting is done during Icicles searching."
  :group 'Icicles-Searching :group 'faces)

(defface icicle-special-candidate
    '((((background dark)) (:background "#176900004E0A")) ; a dark blue
      (t (:background "#EF47FFFFC847"))) ; A light yellow.
  "*Face used to highlight `*Completions*' candidates that are special.
The meaning of special is that their names match
`icicle-special-candidate-regexp'."
  :group 'Icicles-Completions-Display :group 'faces)

(defface icicle-whitespace-highlight
    '((((background dark)) (:background "#000093F402A2")) ; a medium green
      (t (:background "Magenta")))
  "*Face used to highlight initial whitespace in minibuffer input."
  :group 'Icicles-Minibuffer-Display :group 'faces)

;; This is defined in `faces.el', Emacs 22.  This is for Emacs < 22.  This is used
;; only for versions of Emacs that have `propertize' but don't have this face.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "*Face for minibuffer prompts."
    :group 'basic-faces))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-face.el ends here
