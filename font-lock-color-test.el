;;; font-lock-color-test.el --- Tool to test new font lock colors.
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-07-31 Fri
;; Last-Updated: 2009-08-04 Tue
;; URL: http://www.emacswiki.org/emacs/font-lock-color-test.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is for testing new font-lock-* faces colors, see functions
;;
;;   `flct-show-colors-suggestions'
;;   `flct-list-colors-display'
;;
;;   `flct-use-suggestion'
;;   `flct-reset-faces-default-colors'
;;
;; To see current suggestions do `M-x flct-show-colors-suggestions'.
;; To use one of them do `M-x flct-use-suggestion'.
;;
;; You are welcome to add your own suggestions in the variable
;; `flct-faces-color-suggestions' below.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst flct-faces-color-suggestions
  '((light ;; light background

     ;; Chong's colors are currently in CVS Emacs:
     ("chong1" (
                (font-lock-builtin-face       "MediumOrchid4")
                (font-lock-keyword-face       "Purple")
                (font-lock-preprocessor-face  "MediumOrchid4")
                (font-lock-comment-face       "Firebrick")
                (font-lock-warning-face       "Red1")
                (font-lock-constant-face      "dark cyan") ;; 4.1
                (font-lock-type-face          "ForestGreen")
                (font-lock-doc-face           "VioletRed4")
                (font-lock-string-face        "VioletRed4")
                (font-lock-variable-name-face "sienna")
                (font-lock-function-name-face "Blue1")
                ))

     ;; I have tested a lot of colors. It is quite hard to find colors
     ;; which are both distinguishable and have good contrast. It
     ;; often looks quite hard and dull. All the nice colors seems to
     ;; have too low contrast.
     ;;
     ;; So I decided to use colored backgrounds for those parts where
     ;; it fits. It fits very well with comments and strings, since
     ;; these parts consist of blocks of words so to say.
     ("lennart1" (
                  (font-lock-builtin-face      "Orchid4")

                  (font-lock-preprocessor-face "DeepPink3") ;; Make it stand out
                  (font-lock-warning-face      "red2")

                  (font-lock-comment-face      ("Firebrick" . "#fff5d5"))
                  (font-lock-constant-face      "#00765b")
                  (font-lock-constant-face      "cyan4") ;; 4.1
                  (font-lock-constant-face      "turquoise4")

                  (font-lock-doc-face  ("gold4" . "#eeffa6"))
                  (font-lock-doc-face  "chocolate4")
                  (font-lock-doc-face  "#797900") ;;
                  (font-lock-doc-face  "ForestGreen")
                  (font-lock-doc-face  "DarkGreen")
                  (font-lock-doc-face  "RosyBrown4") ;; clashes builtin, var ..
                  (font-lock-doc-face  "#9b6900") ;;
                  (font-lock-doc-face  "yellow4") ;; 3.6 clashes var...
                  (font-lock-doc-face  "#9C6969") ;; 4.5, darker RosyBrown, clashes builtin, var ...
                  (font-lock-doc-face  "#357800") ;;
                  (font-lock-doc-face  "chartreuse4") ;; 4.2 clashes type, var ...
                  (font-lock-doc-face  "green4") ;; clashes type...
                  (font-lock-doc-face  "DarkOliveGreen4") ;; 3.9 clashes type, var...
                  (font-lock-doc-face  "OliveDrab3") ;; 1.9
                  (font-lock-doc-face  "OliveDrab4") ;; 3.9 clashes var, type ...
                  (font-lock-doc-face  "VioletRed4") ;; clashes comment, builtin, pre ...
                  (font-lock-doc-face  "maroon") ;; clashes comment, pre, builtin ...
                  (font-lock-doc-face  "MistyRose4") ;; 3.9, clashes builtin, var...
                  (font-lock-doc-face  "PeachPuff4") ;; clashes builtin, var ...
                  (font-lock-doc-face  "SaddleBrown") ;; clashes var, comment, builtin ...
                  (font-lock-doc-face  "burlywood4") ;; clashes builtin, var...
                  (font-lock-doc-face  "#8B7D7B") ;; MistyRose4
                  (font-lock-doc-face  "#BC8F8F") ;; RosyBrown
                  (font-lock-doc-face  "#8B726a")
                  (font-lock-doc-face  "#8B6D7B")
                  (font-lock-doc-face  "orange4") ;; Too similar to var face
                  (font-lock-doc-face  "Salmon4") ;; 3.5
                  (font-lock-doc-face  "VioletRed3") ;; Too outstanding
                  (font-lock-doc-face  "chocolate3") ;; 3.8
                  (font-lock-doc-face  "orange3") ;; 3.0
                  (font-lock-doc-face  "Salmon3") ;; 3.5
                  (font-lock-doc-face  "RosyBrown3") ;; 2.4

                  (font-lock-string-face  ("#797900" . "#f4ffff")) ;;
                  (font-lock-string-face  "#9b6900") ;;
                  (font-lock-variable-name-face "#9b6900")
                  (font-lock-variable-name-face "#797900")
                  (font-lock-variable-name-face "goldenrod4")
                  (font-lock-variable-name-face "tan4")
                  ))

     ("drew" (;; I use a LightBlue background.
              (font-lock-warning-face       "Yellow")        ; And bold
              (font-lock-constant-face      "#00006DE06DE0") ; A dark cyan
              (font-lock-doc-face           "Magenta4")      ; Inherits from string-face
              (font-lock-string-face        "Magenta4")
              (font-lock-function-name-face "Red")
              (font-lock-keyword-face       "Blue3")))
     )
    (dark ;; dark background
     ))
  "Suggestions for fon-lock-* faces colors.
You are welcome to enter your own suggestions here!

The suggestions are divided in those for light and dark
backgrounds.

Each suggestion should have a name, like \"chong1\" in the code
for this variable.

You can enter just one color for each face or a cons in which
case it should be foreground and background colors.

Note that just the first suggestion for a font lock face is
used. \(This should make testing a bit more convenient.)")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Don't edit below this line (unless you have a good reason...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hexrgb nil t)

;;(flct-get-matching-suggestions)
(defun flct-get-matching-suggestions ()
  "Get color suggestions for current background mode."
  (cdr (assoc (frame-parameter nil 'background-mode)
              flct-faces-color-suggestions)))

(defun flct-check-suggestions-colors ()
  (let ((matching-suggestions (flct-get-matching-suggestions)))
    (dolist (sug matching-suggestions)
      (let ((face-colors (cadr sug)))
        (dolist (face-color face-colors)
          (let ((clrs (cadr face-color)))
            (unless (or (and (consp clrs)
                             (color-values (car clrs))
                             (color-values (cdr clrs)))
                        (color-values clrs))
              (message "Bad color value in %s: %s" sug face-color))))))))
(flct-check-suggestions-colors)

(defconst flct-less-important-font-lock-color-differences
  ;; Are the faces shown close to each other? Do they conceptually differ?
  '(;;font-lock-comment-delimiter-face ;; Does not everyone expect it to be comment-face?
    (font-lock-comment-face font-lock-comment-delimiter-face)
    ))

(defconst flct-min-lumosity-diff-between-faces 130)

(defun flct-use-suggestion (which)
  "Try out suggestion with name WHICH.
Suggestion WHICH is one of those in `flct-faces-color-suggestions'.

You can reset font lock faces afterwards with
`flct-reset-faces-default-colors'."
  (interactive (list (flct-read-colors-suggestion-name "Which suggestion? ")))
  (flct-save-face-default-colors)
  (flct-reset-faces-default-colors)
  (let ((sug-colors (cadr (assoc which (flct-get-matching-suggestions))))
        done)
    (dolist (flf-color sug-colors)
      (unless (memq (car flf-color) done)
        (setq done (cons (car flf-color) done))
        (let* ((clrs (nth 1 flf-color))
               (fg (if (consp clrs) (car clrs) clrs))
               (bg (when (consp clrs) (cdr clrs))))
          (set-face-foreground (car flf-color) fg)
          (when t ;bg
            (set-face-background (car flf-color) bg)))))))

(defun flct-unimportant-font-lock-diff (f1 f2)
  (let ((a1 (assoc f1 flct-less-important-font-lock-color-differences))
        (a2 (assoc f2 flct-less-important-font-lock-color-differences))
        )
    (or (and a1 (memq f2 a1))
        (and a2 (memq f1 a2)))))

(defvar flct-faces-default-colors nil
  "Save the face colors before testing here.")

(defun flct-save-face-default-colors ()
  "Save default font lock face colors."
  ;; Fix-me: bg
  (unless flct-faces-default-colors
    (dolist (face flct-font-lock-faces-symbols)
      (setq flct-faces-default-colors
            (cons (cons face
                        (list (face-foreground face)
                              (face-background face)
                              (flct-face-foreground face)
                              (flct-face-background face)))
                  flct-faces-default-colors)))))

(defun flct-reset-faces-default-colors ()
  "Reset default font lock faces colors.
Use this after testing with `flct-use-suggestion'."
  (interactive)
  (dolist (ff flct-faces-default-colors)
    (set-face-foreground (car ff) (nth 0 (cdr ff)))
    (set-face-background (car ff) (nth 1 (cdr ff)))))

(defconst flct-font-lock-faces-symbols
  '(;; Order these so that faces that are hard to distinguish
    ;; are shown close to each other.
    font-lock-builtin-face
    font-lock-keyword-face
    font-lock-preprocessor-face

    ;;font-lock-comment-delimiter-face ;; Does not everyone expect it to be comment-face?
    font-lock-comment-face
    font-lock-warning-face

    font-lock-constant-face
    font-lock-type-face

    font-lock-doc-face
    font-lock-string-face
    font-lock-variable-name-face

    font-lock-function-name-face

    ;; font-lock-negation-char-face
    ;; font-lock-regexp-grouping-backslash
    ;; font-lock-regexp-grouping-construct
    ))

(defvar flct-show-colors-suggestions-hist nil)

(defun flct-read-colors-suggestion-name (prompt)
  (completing-read prompt
                   (flct-get-matching-suggestions)
                   nil ;; pred
                   t   ;; require match
                   nil ;; initial
                   'flct-show-colors-suggestions-hist
                   ))

(defun flct-show-colors-suggestions (suggestions)
  "Show suggestions SUGGESTIONS for new font lock colors.
Chong suggested changing the font-lock-* faces.  This is a tool
to help with that.  For more information see URL
`http://lists.gnu.org/archive/html/emacs-devel/2009-07/msg01577.html'.

You are welcome to add your own suggestions to
`flct-faces-color-suggestions' in this file on EmacsWiki."
  (interactive (let (choices
                     (choice "dummy"))
                 (while (not (string= choice ""))
                   (setq choice (completing-read (if choices "Show suggestion: "
                                                   "Show suggestion (just RET for all): ")
                                                 (flct-get-matching-suggestions)
                                                 nil ;; pred
                                                 t   ;; require match
                                                 nil ;; initial
                                                 'flct-show-colors-suggestions-hist
                                                 ))
                   (unless (string= choice "")
                     (add-to-list 'choices choice t)))
                 (unless choices
                   (setq choices (mapcar (lambda (rec)
                                           (car rec))
                                         (flct-get-matching-suggestions))))
                 (list choices)))
  (let ((flf flct-font-lock-faces-symbols)
        (buf (get-buffer-create "Font lock face test"))
        (try-these (mapcar (lambda (suggestion)
                             (assoc suggestion (flct-get-matching-suggestions)))
                           ;;(cadr (assoc "chong1" (flct-get-matching-suggestions)))
                           suggestions))
        (background-color (frame-parameter nil 'background-color))
        )
    (with-current-buffer buf
      (erase-buffer)
      (setq show-trailing-whitespace nil)
      (insert "
Faces luminosity contrast ratio towards current background
color. A luminosity contrast ratio higher than 4.5:1 is ok
according to WACG.  These values seems useful, see for example
http://www.snook.ca/technical/colour_contrast/colour.html or
http://juicystudio.com/article/luminositycontrastratioalgorithm.php

")
      (insert (make-string 35 ?\ )
              (format "%-20s%-20s" "Emacs22" "Current"))
      (dolist (suggestion suggestions)
        (insert (flct-display-color-bg-difference background-color nil))
        (insert (format "%-20s" suggestion)))
      (insert "\n\n")
      (setq flct-current-font-lock-luminosity-contrast-ratios nil)
      (dolist (f flf)
        (setq flct-current-font-lock-luminosity-contrast-ratios
              (cons
               (cons f
                     (flct-display-color-luminosity-contrast-ratio (or (flct-face-background f)
                                                                       background-color)
                                                                   (flct-face-foreground f)))
               flct-current-font-lock-luminosity-contrast-ratios
               )))
      (flct-save-face-default-colors)
      (dolist (f flf)
        (let* ((orig-clr (assoc f flct-faces-default-colors))
               (orig-fg (or (nth 1 orig-clr) (nth 3 orig-clr)))
               (orig-bg (or (nth 2 orig-clr))) ;; (nth 4 orig-clr)))
               (orig-dsp (if orig-bg (format "%s.%s" orig-fg orig-bg)
                           orig-fg))
               (clr22 (cadr (assoc f flct-emacs22-colors))))
          (when (> (length orig-dsp) 14) (setq orig-dsp (substring orig-dsp 0 14)))
          (insert
           (format "%-33s" f)
           ;;(propertize "abcdef" 'face f)

           (flct-display-color-luminosity-contrast-ratio background-color clr22)
           (flct-display-color-hue clr22)
           (flct-display-color-bg-difference background-color clr22)
           (format " %-14s" (propertize clr22 'face `(:foreground ,clr22)))

           (flct-display-color-luminosity-contrast-ratio (or orig-bg background-color) orig-fg)
           (flct-display-color-hue orig-fg)
           (flct-display-color-bg-difference (or orig-bg background-color) orig-fg)
           (format " %-14s" (propertize orig-dsp 'face `(:foreground ,orig-fg :background ,orig-bg))))

          (dolist (try-this try-these)
            (let* ((try-face (assoc f (cadr try-this)))
                   (new-face (cadr try-face))
                   (new-fg (if (consp new-face) (car new-face) new-face))
                   (new-bg (if (consp new-face) (cdr new-face) background-color)))
              (insert
               (if new-face
                   (flct-display-color-luminosity-contrast-ratio new-bg new-fg)
                 (flct-display-current-font-lock-luminosity-contrast f))
               ;;(if new-face (flct-display-color-hue new-face) (flct-display-color-hue orig-fg))
               (flct-display-color-bg-difference new-bg new-fg)
               (if new-face
                   ;;(propertize " abcdef" 'face `(:foreground ,new-face))
                   (let ((str (if (consp new-face)
                                  (format "%s.%s" new-fg new-bg)
                                (format "%s" new-fg))))
                     (when (> (length str) 14) (setq str (substring str 0 14)))
                     (format " %-14s" (propertize str 'face `(:foreground ,new-fg :background ,new-bg))))
                 (make-string 15 ?\ ))
               ))))
        (insert "\n"))
      (insert "
To test any of the suggestions use

   M-x flct-use-suggestion
   M-x flct-reset-faces-default-colors")

      (when nil
      ;; Color differences
      (insert "

The color differences below should not be taken too seriously.  I
have tried to implement a better algorithm for it, but have
failed so far to find fully understandable information.  \(If
anyone wonder and have information I was looking for CEILuv or
something similar.)
")
      (let ((schemes (cons nil try-these)))
        (dolist (scheme schemes)
          (let ((min-diff flct-min-lumosity-diff-between-faces)
                tail f1 f2 cdiff
                (scheme-faces (cadr scheme))
                ffg1 ffg2
                diff-list)
            (dolist (f1 flf)
              (dolist (f2 (cdr (memq f1 flf)))
                (unless (flct-unimportant-font-lock-diff f1 f2)
                  (setq ffg1 (or (cadr (assoc f1 scheme-faces))
                                 (flct-face-foreground f1)))
                  (setq ffg2 (or (cadr (assoc f2 scheme-faces))
                                 (flct-face-foreground f2)))
                  (setq ffg1 (or (and (consp ffg1) (car ffg1)) ffg1))
                  (setq ffg2 (or (and (consp ffg2) (car ffg2)) ffg2))
                  (setq cdiff (flct-color-difference ffg1 ffg2))
                  (when (< cdiff min-diff)
                    (setq diff-list
                          (cons
                           (cons cdiff
                                 (format
                                  ;;" diff=%s < %s: %s <-> %s (%s <-> %s)\n"
                                  " diff=%s < %s: %s <-> %s\n"
                                  cdiff min-diff
                                  (propertize (symbol-name f1) 'face `(:foreground ,ffg1))
                                  (propertize (symbol-name f2) 'face `(:foreground ,ffg2))
                                  ;;(flct-color-to-sh6 ffg1)
                                  ;;(flct-color-to-sh6 ffg2)
                                  ))
                           diff-list))))))
            (when diff-list
              (insert (format "\nColor differences that are small for %s faces:\n"
                              (if (not scheme) "current" (car scheme))))
              (dolist (diff (sort diff-list (lambda (a b) (< (car a) (car b)))))
                (insert (cdr diff))))))))

      (goto-char (point-min)))
    (display-buffer buf)
    ))

(defvar flct-current-font-lock-luminosity-contrast-ratios nil)
(defun flct-display-current-font-lock-luminosity-contrast (face-name)
  (cdr (assoc face-name flct-current-font-lock-luminosity-contrast-ratios)))
;;(flct-face-foreground 'font-lock-function-name-face)
;;(flct-face-foreground 'font-lock-preprocessor-face)
;;(flct-face-foreground 'font-lock-doc-face)

(defun flct-face-foreground (face)
  "Get foreground color of face FACE.
Similar to `face-foreground' but also checks :inherit.
Also shows a message."
  (interactive (list (read-face-name "Face")))
  (let ((color (face-attribute face :foreground))
        (inherit (face-attribute face :inherit)))
    (while (and (eq color 'unspecified)
                (not (eq inherit 'unspecified)))
      (setq face inherit)
      (setq color (face-attribute face :foreground))
      (setq inherit (face-attribute face :inherit)))
    (when (called-interactively-p)
      (message "Foreground color of face %s: %s (%s, %s)" face color (flct-color-to-sh6 color)
               (propertize "example" 'face face)))
    ;; Fix-me:
    (if (eq color 'unspecified) nil color)
    ))

(defun flct-face-background (face)
  "Get background color of face FACE.
Similar to `face-background' but also checks :inherit.
Also shows a message."
  (interactive (list (read-face-name "Face")))
  (let ((color (face-attribute face :background))
        (inherit (face-attribute face :inherit)))
    (while (and (eq color 'unspecified)
                (not (eq inherit 'unspecified)))
      (setq face inherit)
      (setq color (face-attribute face :background))
      (setq inherit (face-attribute face :inherit)))
    (when (called-interactively-p)
      (message "Foreground color of face %s: %s (%s, %s)" face color (flct-color-to-sh6 color)
               (propertize "example" 'face face)))
    ;; Fix-me:
    (if (eq color 'unspecified) nil color)
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Luminosity contrast

(defun flct-relative-luminance (color-str)
  "Relative luminance of color COLOR-STR.
The relative brightness of any point in a colorspace, normalized
to 0 for darkest black and 1 for lightest white.

Note 1: For the sRGB colorspace, the relative luminance of a
color is defined as L = 0.2126 * R + 0.7152 * G + 0.0722 * B
where R, G and B are defined as:

  if RsRGB <= 0.03928 then R = RsRGB/12.92 else R = ((RsRGB+0.055)/1.055) ^ 2.4
  if GsRGB <= 0.03928 then G = GsRGB/12.92 else G = ((GsRGB+0.055)/1.055) ^ 2.4
  if BsRGB <= 0.03928 then B = BsRGB/12.92 else B = ((BsRGB+0.055)/1.055) ^ 2.4

and RsRGB, GsRGB, and BsRGB are defined as:

  RsRGB = R8bit/255
  GsRGB = G8bit/255
  BsRGB = B8bit/255

See URL `http://www.w3.org/TR/2008/REC-WCAG20-20081211/#relativeluminancedef'."
  (let* ((rgb (mapcar (lambda (val)
                        (let ((rel-val (/ val (* 256.0 256.0))))
                          (if (<= rel-val 0.03928)
                              (/ rel-val 12.92)
                            (expt (/ (+ rel-val 0.055)
                                     1.055)
                                  2.4))))
                      (color-values color-str)))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb)))
    (+ (* 0.2126 r)
       (* 0.7152 g)
       (* 0.0722 b))))

(defun flct-luminance-contrast-ratio (l1 l2)
  "Contrast ratio between relative luminances L1 and L2.
Defined as

  (L1 + 0.05) / (L2 + 0.05)

where

  L1 is the relative luminance of the lighter of the colors, and
  L2 is the relative luminance of the darker of the colors.

See URL `http://www.w3.org/TR/2008/REC-WCAG20-20081211/#contrast-ratiodef'."
  (let* ((l-dark (if (> l1 l2) l2 l1))
         (l-bright (if (> l1 l2) l1 l2))
         (ratio (/ (+ l-bright 0.05)
                   (+ l-dark   0.05))))
    ;; Fix-me: There is something wrong in the formulas, ratio max is
    ;; 21, but I get 21.4 ...? Turned out to be an error in w3
    ;; description of algorithm. You should of course divide by 256,
    ;; not 255.
    ratio))

(defun flct-color-luminosity-contrast-ratio (color1 color2)
  "Compute color contrast between colors COLOR1 and COLOR2."
  (let ((lum1 (flct-relative-luminance color1))
        (lum2 (flct-relative-luminance color2)))
    (flct-luminance-contrast-ratio lum1 lum2)))

(defun flct-display-color-luminosity-contrast-ratio (color1 color2)
  "Return string with contrast between colors COLOR1 and COLOR2."
  (if (not (and color1 color2))
      (if color1
          "testi"
        "     ")
    (let* ((ratio (flct-color-luminosity-contrast-ratio color1 color2))
           (str (format "%5.1f" ratio)))
      (cond
       ((< ratio 3.8)
        (propertize str 'face '(:foreground "#dd0000")))
       ((< ratio 4.25)
        (propertize str 'face '(:foreground "#aa0000")))
       ((< ratio 4.45) ;; rounding on display
        (propertize str 'face '(:foreground "#770000")))
       (t
        str)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Color difference

;;(flct-color-difference "#ffffff" "#000000")
(defun flct-color-difference (color1 color2)
  (let ((rgb1 (color-values color1))
        (rgb2 (color-values color2))
        (diff 0)
        )
    (dolist (n '(0 1 2))
      (setq diff (+ diff (/ (abs (- (nth n rgb1)
                                    (nth n rgb2)))
                            256))))
    diff))

(defun flct-display-color-bg-difference (color1 color2)
  ;; Using this seems to spoil much of the possibilities to use
  ;; different colors.  If I understand Gez correctly (see
  ;; http://juicystudio.com/services/aertcolourcontrast.php) this
  ;; algorithm is depreceated in favor of the luminosity based
  ;; algorithms.
  (if t
      "" ;; Don't display color difference
    (if (not (and color1 color2))
        "    "
      (let* ((diff (flct-color-difference color1 color2))
             (str (format " %3d" diff)))
        (cond
         ((< diff 400)
          (propertize str 'face 'font-lock-warning-face))
         ((< diff 450)
          (propertize str 'face '(:foreground "red3")))
         ((< diff 500)
          (propertize str 'face '(:foreground "red4")))
         (t str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers

;;(flct-color-to-sh6 "black")
;;(flct-color-to-sh6 "white")
;;(flct-color-to-sh6 "#fe0000")
(defun flct-color-to-sh6 (color)
  "Return color in #..... format."
  (interactive (list (read-color "Color: ")))
  (let* ((rgb (if (listp color)
                  color
                (mapcar (lambda (val) (/ val 256)) (color-values color))))
         (rgb-color (format "#%02X%02X%02X"
                            (nth 0 rgb)
                            (nth 1 rgb)
                            (nth 2 rgb))))
    (when (called-interactively-p)
      (message "%s" rgb-color))
    rgb-color))

;; (flct-color-luminosity-contrast-ratio "white" "DarkGoldenrod")
;; (flct-color-luminosity-contrast-ratio "white" "black")
;; (flct-color-luminosity-contrast-ratio "white" "#050505")

(defun flct-face-color (face) (flct-color-to-sh6 (face-foreground face)))

;; (flct-face-color font-lock-builtin-face)
;; (flct-face-color font-lock-keyword-face)
;; (flct-face-color font-lock-preprocessor-face)

;; (flct-face-color font-lock-comment-face)
;; (flct-face-color font-lock-warning-face)

;; (flct-face-color font-lock-constant-face)
;; (flct-face-color font-lock-type-face)

;; (flct-face-color font-lock-doc-face)
;; (flct-face-color font-lock-string-face)
;; (flct-face-color font-lock-variable-name-face)

;; (flct-face-color font-lock-function-name-face)
(defun flct-list-flf-fg-hue ()
  (let ((fs-h nil))
    (dolist (fs flct-font-lock-faces-symbols)
      (setq fs-h (cons
                  (cons fs (car (apply 'hexrgb-rgb-to-hsv
                                       (mapcar (lambda (v)
                                                 (/ v 256.0))
                                               (color-values (face-foreground fs))))))
                  fs-h)))
    (setq fs-h (sort fs-h (lambda (a b)
                            (< (cdr a) (cdr b)))))
    (dolist (fh fs-h)
      (message "%s => %s" (car fh) (cdr fh)))))

;;(flct-test-hue-change-lum-contrast font-lock-doc-face 0.2)
(defun flct-test-hue-change-lum-contrast (face-or-color new-h)
  "For face or color FACE-OR-COLOR test new hue NEW-H.
Display a message with new luminosity color contrast."
  (interactive (list (read-face-name "Face: ")
                     (read-number "New h (0-1): ")))
  (let* ((hex (hexrgb-color-name-to-hex (cond
                                         ((stringp face-or-color)
                                          face-or-color)
                                         ((facep face-or-color)
                                          (face-foreground face-or-color)))))
         (hsv (hexrgb-hex-to-hsv hex))
         new-rgb new-hex
         new-lum
         new-rel-lum)
    (setcar hsv new-h)
    (setq new-rgb (mapcar (lambda (v)
                            (* 1.0 256.0 v))
                          (apply 'hexrgb-hsv-to-rgb hsv)))
    (setq new-hex (flct-color-to-sh6 new-rgb))
    (setq new-lum (flct-relative-luminance new-hex))
    (setq new-rel-lum (flct-color-luminosity-contrast-ratio new-hex "white"))
    (message "new-rgb=%s, new-rel-lum=%s" new-hex new-rel-lum)))

;;(flct-color-hue "green")
(defun flct-color-hue (color)
  (when (featurep 'hexrgb)
    (when color
      (let* ((hex (hexrgb-color-name-to-hex color))
             (hsv (hexrgb-hex-to-hsv hex)))
        (car hsv)))))

(defun flct-display-color-hue (color)
  (if nil ;;(featurep 'hexrgb) ;; gets too cluttered
      (if color
          (let* ((hex (hexrgb-color-name-to-hex color))
                 (hsv (hexrgb-hex-to-hsv hex)))
            (format " %3d" (round (* 100 (car hsv)))))
        "    ")
    ""))

(defun flct-list-colors-display (&optional list buffer-name)
  "Display names of defined colors, and show what they look like.
This is a modified version of `list-colors-display' that tries to
list the colors in a way useful for deciding font lock faces
colors.

The following changes have been made to make it easier to see
what may be useful for font lock faces colors:

- Gray colors have been deleted from the list.
- Luminosity color contrast ratio against foreground and background
  have been added.
- Colors are sorted according to luminosity color contrast ratio
  and hue.  \(Hue is only used if hexrgb.el is loaded.)

Note that the luminosity color contrast should be at least 4.5
against the background according to WCAG, see for example URL
`http://juicystudio.com/article/luminositycontrastratioalgorithm.php'.


Default doc string:
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list of
colors that the current display can handle.  If the optional
argument BUFFER-NAME is nil, it defaults to *Colors*."
  (interactive)
  (when (and (null list) (> (display-color-cells) 0))
    (setq list (list-colors-duplicates (defined-colors)))
    (when (memq (display-visual-class) '(gray-scale pseudo-color direct-color))
      ;; Don't show more than what the display can handle.
      (let ((lc (nthcdr (1- (display-color-cells)) list)))
	(if lc
	    (setcdr lc nil)))))
  (with-help-window (or buffer-name "*Colors*")
    (save-excursion
      (set-buffer standard-output)
      (setq truncate-lines t)
      (if temp-buffer-show-function
	  (flct-list-colors-print list)
	;; Call flct-list-colors-print from temp-buffer-show-hook
	;; to get the right value of window-width in flct-list-colors-print
	;; after the buffer is displayed.
	(add-hook 'temp-buffer-show-hook
		  (lambda () (flct-list-colors-print list)) nil t)))))


(defun flct-list-colors-print (list)
  (let ((fg (frame-parameter nil 'foreground-color))
        (bg (frame-parameter nil 'background-color)))
    (setq list (delq nil (mapcar (lambda (c)
                                   (unless (color-gray-p (car c))
                                     c))
                                 list)))
    (setq list (sort list (lambda (a b)
                            (let ((luma (flct-color-luminosity-contrast-ratio (car a) bg))
                                  (lumb (flct-color-luminosity-contrast-ratio (car b) bg))
                                  lum-diff
                                  (huea (when (featurep 'hexrgb) (flct-color-hue (car a))))
                                  (hueb (when (featurep 'hexrgb) (flct-color-hue (car b)))))
                              (setq lum-diff (round (* 3 (- luma lumb))))
                              (if nil
                                  (progn
                                    (if (and huea hueb (= huea hueb))
                                        (> luma lumb)
                                      (< huea hueb)))
                                (if (and huea hueb (= 0 lum-diff))
                                    (< huea hueb)
                                  (> luma lumb)))))))
    (dolist (color list)
      (if (consp color)
          (if (cdr color)
              (setq color (sort color (lambda (a b)
                                        (string< (downcase a)
                                                 (downcase b))))))
        (setq color (list color)))
      (put-text-property
       (prog1 (point)
         (insert (car color))
         (indent-to 22))
       (point)
       'face (list ':background (car color)))
      (insert (flct-display-color-luminosity-contrast-ratio (car color) fg))
      (insert (apply 'format " #%02x%02x%02x"
                     (mapcar (lambda (c) (lsh c -8))
                             (color-values (car color)))))
      (insert (flct-display-color-luminosity-contrast-ratio (car color) bg))
      (put-text-property
       (prog1 (point)
         (insert " " (if (cdr color)
                         (mapconcat 'identity (cdr color) ", ")
                       (car color))))
       (point)
       'face (list ':foreground (car color)))
      ;;(indent-to (min 75 (max (- (window-width) 8) 44)))
      (insert "\n")))
  (goto-char (point-min)))

;; ;;(flct-get-current-font-lock-fg-colors)
;; (defun flct-get-current-font-lock-fg-colors ()
;;   "Just a helper to pick them from Emacs 22."
;;   (dolist (fs flct-font-lock-faces-symbols)
;;     (message "(%s %S)" fs (flct-face-color fs))))

(defvar flct-emacs22-colors
  '(
    (font-lock-builtin-face       "Orchid")
    (font-lock-keyword-face       "Purple")
    (font-lock-preprocessor-face  "Orchid")
    (font-lock-comment-face       "FireBrick")
    (font-lock-warning-face       "Red1")
    (font-lock-constant-face      "CadetBlue")
    (font-lock-type-face          "ForestGreen")
    (font-lock-doc-face           "RosyBrown")
    (font-lock-string-face        "RosyBrown")
    (font-lock-variable-name-face "DarkGoldenrod")
    (font-lock-function-name-face "Blue1")

    ))

(defvar flct-emacs23-CVS-colors
  '(
    (font-lock-builtin-face       "MediumOrchid4")
    (font-lock-keyword-face       "Purple")
    (font-lock-preprocessor-face  "MediumOrchid4")
    (font-lock-comment-face       "FireBrick")
    (font-lock-warning-face       "Red1")
    (font-lock-constant-face      "dark cyan")
    (font-lock-type-face          "ForestGreen")
    (font-lock-doc-face           "VioletRed4")
    (font-lock-string-face        "VioletRed4")
    (font-lock-variable-name-face "sienna")
    (font-lock-function-name-face "Blue1")

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-color-test.el ends here
