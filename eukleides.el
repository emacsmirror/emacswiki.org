;;; eukleides.el --- Major mode to edit eukleides files.

;;{{{ Copyright (C) 2006 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zelg@yandex.ru>
;; Created: Mon Apr 10 21:13:45 MSD 2006
;; Keywords: math

;; This file is NOT part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;}}}
;;{{{ Commentary:

;; eukleides.el is full features Emacs interface to Eukleides - A
;; Euklidean Geometry Drawing Language (http://www.eukleides.org/).

;; Requires external programs:
;;   - eukleides
;;   - tex
;;   - dvips
;;   - convert from ImageMagick

;; Eukleides major mode features:
;;   - Font-locking
;;   - Compilation (with `next-error' support)
;;   - `mode-compile' support
;;   - Sketch preview (optional, requires external programs)
;;   - Contextual help (`C-c C-h')
;;   - Eldoc (use `turn-on-eldoc-mode' to use it)
;;   - Support for `info-lookup-symbol'
;;   - Imenu
;;   - Examples in Abbrevs (psaxes, sample1, sample2, ..)
;;   - Eukleides Interactive forms!
;;   - Easy menu
;;   - Contextual smart completion
;;   - TODO: printing

;;}}}
;;{{{ Interactive states use:

;; Eukleides code like:

;;   c = interactive(<start>, <increment>, "<state-name>", right)

;; specifies interactive variable `c', which starts from <start> value
;; if interactive <state-name> is activated with
;; `eukleides-interactive-start', increases by <increment> value on
;; `eukleides-interactive-increase' and decreases by <increment> value
;; on `eukleides-interactive-decrease'.  This interactive form is
;; substituted with actual value on `eukleides-interactive-commit'
;; command.  Here is example of use:

;;    A B C triangle
;;    c = interactive(.2, 0.1, 0, 0, "myhomo", right)
;;    D = homothecy(B, A, c)
;;    draw(A, B, C)
;;    draw(segment(C,D))
;;    draw(D); label(D, 270:)

;; Then compile it and press `C-<up> myhomo RET' to start myhomo
;; state, then press `C-<left>' and `C-<right>' to modify c value,
;; once you've found appropriate value for it press `C-<down>' to
;; commit it.

;;}}}
;;{{{ TODO:

;;    - PostScript printer  (Done 7 June 2006)
;;    - Code translator

;;}}}

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'derived)
(require 'completer)

(defvar eukleides-version "1.0-rc3")

;;{{{ `-- Custom variables

(defgroup eukleides nil
  "Group to customize eukleides major mode."
  :version eukleides-version
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki/eukleides.el")
  :prefix "eukleides-"
  :tag "Eukleides")

(defcustom eukleides-auto-preview t
  "*Non-nil mean show sketch preview image after each compilation."
  :type 'boolean
  :group 'eukleides)

(defcustom eukleides-image-type (cdar image-formats-alist)
  "Image type used to display sketch."
  :type (list 'choice
              (mapcar (lambda (fmt)
                        (list 'const fmt))
                      image-formats-alist))
  :group 'eukleides)

(defcustom eukleides-scale-factor 2.0
  "Scale value."
  :type 'number
  :group 'eukleides)

(defcustom eukleides-interactive-commit-query t
  "*Non-nil to query confirmation for interactive state commit."
  :type 'boolean
  :group 'eukleides)

(defcustom eukleides-complete-keyword-with-parens t
  "*Non-nil means append parens when completing eukleides keyword."
  :type 'boolean
  :group 'eukleides)

(defface eukleides-angle-mark-face
  `((((class color) (background light))
     (:foreground "white" :bold t))
    (((class color) (background dark))
     (:foreground "white" :bold t))
    (t (:bold t)))
  "Face to display angle marks `:' and `<'."
  :group 'eukleides)

;;}}}

;;{{{ +-  Mode variables
;;{{{   `-- Eukleides mode map

(defvar eukleides-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(meta tab)] 'eukleides-complete)
    (define-key map [(control ?c) (control ?l)] 'eukleides-compile)
    (define-key map [(control ?c) (control ?p)] 'eukleides-preview)
    (define-key map [(control ?c) (control ?h)] 'eukleides-help)
    ;; Interactive operations
    (define-key map [(control left)] 'eukleides-interactive-decrease)
    (define-key map [(control right)] 'eukleides-interactive-increase)
    (define-key map [(control up)] 'eukleides-interactive-start)
    (define-key map [(control down)] 'eukleides-interactive-commit)
    (define-key map [(control print)] 'eukleides-print)
    map)
  "Keymap for `eukleides-mode'.")

;;}}}
;;{{{   `-- Syntax table

(defvar eukleides-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?. "w" st)
    (modify-syntax-entry ?: "w" st)
    (modify-syntax-entry ?= "." st)

    ;; PSTricks uses square brackets
    (modify-syntax-entry ?[ "(" st)
    (modify-syntax-entry ?] ")" st)
    st)
  "Syntax table for `eukleides-mode'.")

;;}}}
;;{{{   `-- Abbrev table with examples

(defvar eukleides-mode-abbrev-table nil
  "Abbrev table used by eukleides mode.")
(define-abbrev-table 'eukleides-mode-abbrev-table
  '(("psaxes" "p1 = point(0, 0)
p2 = point(5, 4)
\\psaxes[arrows=\"->\", linecolor=\"gray\", linewidth=\"0.01\"](p1,p2)" nil 0)
    ("sample1" "A B C triangle
draw(A, B, C); draw(incircle(A, B, C)); draw(bisector(B, A, C), dotted)
draw(bisector(A, B, C), dotted); draw(bisector(B, C, A), dotted)
" nil 0)
    ("sample2" "A B C triangle
a = projection(A, line(B, C))
b = projection(B, line(A, C))
c = projection(C, line(A, B))
draw(A, B, C); draw(a); draw(b); draw(c); draw(segment(A, a), dotted)
draw(segment(B, b), dotted); draw(segment(C, c), dotted)
draw(barycenter(A, B)); draw(barycenter(B, C))
draw(barycenter(C, A)); draw(circle(a, b, c))
" nil 0)
    ("sample3" "A B C isosceles
H = projection(C, line(A, B))
draw(A, B, C); draw(H); draw(segment(C, H), dashed)
mark(B, H, C, right); mark(segment(A, H))
mark(segment(B, H)); mark(segment(A, C), cross)
mark(segment(C, B), cross); mark(B, A, C, double); mark(C, B, A, double)
" nil 0)
    ("sample4" "A B C D square
A B E equilateral(4)
B F G equilateral(4, 30:)
draw(A, B, C, D); draw(A, B, E); draw(B, F, G); draw(line(E, F), dotted)
" nil 0)
    ("sample5" "A B C D parallelogram(6, 4, 103:)
draw(A, B, C, D)
mark(B, A, D); mark(D, C, B); mark(C, B, A, double)
mark(A, D, C, double)
draw(A, C, dotted); draw(B, D, dotted)
thickness(3)
E = barycenter(A, B, C, D)
draw(A); draw(B); draw(C); draw(D); draw(E)
label(A, 250:); label(B, 310:); label(C, 45:); label(D, 120:)
label(E, 0.5, 270:)
" nil 0)
    ("sample6" "A B C D parallelogram
draw(segment(A, B), full, arrow); draw(segment(A, C), full, arrow)
draw(segment(A, D), full, arrow); draw(segment(B, C), dotted)
draw(segment(D, C), dotted)
" nil 0)
    ("sample7" "O = point(2, 2)
C = circle(O, 2)
A = point(6.5, 2)
c = circle(O, A)
I J intersection(C, c)
color(lightgray)
draw(line(A, I)); draw(line(A, J))
color(black)
draw(O, plus); draw(A); draw(C); draw(c, dotted)
" nil 0)
    ("sample8" "frame(-5, -4, 5, 4)
trace(t, 0, 3*360)
{ point(t/360, t:) }
" nil 0)
    ("sample9" "F = point(3, 1.5)
D = line(point(1, 0.5), -65:)
C = parabola(F, D)
draw(F); draw(D); draw(C)
" nil 0)
    ))

;;}}}
;;{{{   `-- Eukleides types/keywords

(defconst eukleides-types '(:number :angle :point :segment :line :vector
                                    :circle :conic-curve :string)
  "List of valid eukleides variables types.")

(defconst eukleides-keywords
  '((abs :number ((x :number)))
    (abscissa :line
              ((A :point))
              ((u :vector))
              ((l :line x :number)
               "Point of abscissa x on line l."))
    (acos :number ((x :number)))
    (altitude :segment
              ((A :point B :point C :point)
               "Altitude of triangle ABC containing vertex A."))
    (angle :number
           ((u :vector) "Polar angle of the object.")
           ((l :line) "Polar angle of the object.")
           ((s :segment) "Polar angle of the object.")
           ((cc :conic-curve)
            "Polar angle of the principal axis of conic curve cc.")
           ((u :vector v :vector)
            "Measure of the angle between vector u and vector v.")
           ((A :point B :point C :point)
            "Measure of the angle under ABC. \
\(The angle under ABC is the angular sector in which one can draw \
counterclockwise an arc of center B from point A to line BC. Therefore \
the angle under ABC is different from the angle under CBA.\)"))
    (arg :number
         ((c :circle A :point)
          "Argument of point A in respect of circle c. \
If A isn't on c, arg returns the argument of the image of A by a projection \
on c in the direction of its center.")
         ((cc :conic-curve A :point)
          "Argument of point A in respect of the internal parametric \
representation of cc. If A isn't on cc, arg returns the argument of the image \
of A by a projection on cc. The direction of the projection is the one of the \
principal axis if cc is a parabola, of the center if cc is an ellipse, of the \
major axis if cc is an hyperbola."))
    (asin :number ((x :number)))
    (atan :number ((x :number)))
    (barycenter :point
                ((A :point (optional x :number) B :point
                    (optional y :number))
                 "Barycenter of A and B with coefficient x and y \
\(default: 1 and 1\). The syntax is similar for 3 or 4 points."))
    (begin :point ((s :segment) "Beginning of segment."))
    (bisector :line
              ((s :segment) "Perpendicular bisector of segment s.")
              ((A :point B :point C :point)
               "Bisector of angle under ABC.")
              ((l :line "l'" :line)
               "Bisector of the sharp angle formed by l and l'."))
    (box nil ((x :number y :number "x'" :number "y'" :number
                 (optional z :number))))
    (ceil :number ((x :number)))
    (center :point
            ((c :circle))
            ((cc :conic-curve)
             "If cc is a parabola, the result is its vertex."))
    (circle :circle
            ((A :point B :point) "Circle of diameter AB.")
            ((A :point B :point C :point) "Circumcircle of triangle ABC.")
            ((A :point x :number) "Circle of center A and radius x."))
    (clamp :number
           ((x :number y :number z :number)
            "Returns x if y <=x<=z, y if x<y, z if x>z"))
    (color nil
           ((f (enum "black" "darkgray" "gray" "lightgray" "white" "red"
                     "green" "blue" "cyan" "magenta" "yellow"))
            "Sets color to f. Permitted values of f are black (default), \
darkgray, gray,lightgray, white, red, green, blue, cyan, magenta and yellow."))
    (conic :conic-curve
           ((A :point l :line x :number)
            "Conic curve of focus A, directrix l and eccentricity x.")
           ((A :point B :point x :number)
            "Conic curve of foci A and B. Number x is the half of the distance \
between the vertices (i.e. major axis for ellipses, or real axis for \
hyperbolas)."))
    (cos :number ((x :number)))
    (deg :number ((x :number) "Converts radians to degrees"))
    (distance :number
              ((A :point B :point))
              ((A :point l :line)))
    (draw nil
          ((A :point (optional f (enum "dot" "disc" "box" "cross" "plus")
                               (optional x :number)))
           "Draws point A with a shape corresponding to f and a scaling ratio \
x \(default: 1\). Permitted values of f are dot, disc, box, cross and plus \
\(default: dot\).")
          ((u :vector A :point (optional f (enum "full" "dashed" "dotted")))
           "Draws vector u from point A. If f is given, it overrides the \
current drawing style. Permitted values of f are full, dashed and dotted.")
          ((l :line (optional f (enum "full" "dashed" "dotted")
                              (optional "f'" (enum "entire" "halfline"
                                                   "backhalfline"))))
           "Draws line l. If f is given, it overrides the current drawing \
style. Permitted values of f are full, dashed and dotted. Permitted values \
of f'are entire, halfline and backhalfline (default: entire).")
          ((s :segment
              (optional f (enum "full" "dashed" "dotted")
                        (optional "f'" (enum "noarrow" "arrow"
                                             "backarrow" "doublearrow"))))
           "Draws segment s. If f is given, it overrides the current drawing \
style. Permitted values of f are full, dashed and dotted. Permitted values of \
f'are noarrow, arrow, backarrow and doublearrow (default: noarrow).")
          ((c :circle (optional f (enum "full" "dashed" "dotted")))
           "Draws circle c. If f is given, it overrides the current drawing \
style. Permitted values of f are full, dashed and dotted.")
          ((c :circle a :angle "a'" :angle
              (optional f (enum "full" "dashed" "dotted")
                        (optional "f'" (enum "noarrow" "arrow"
                                             "backarrow" "doublearrow"))))
           "Draws an arc of circle c from point of argument a to point of \
argument a'. If f is given, it overrides the current drawing style. Permitted \
values of f are full, dashed and dotted. Permitted values of f' are noarrow, \
arrow,backarrow and doublearrow (default: noarrow).")
          ((cc :conic-curve (optional f (enum "full" "dashed" "dotted")))
           "Draws conic curve cc. If f is given, it overrides the current \
drawing style. Permitted values of f are full, dashed and dotted.")
          ((cc :conic-curve x :number y :number
               (optional f (enum "full" "dashed" "dotted")))
           "Draws an arc of conic curve cc. Number x is the argument of the \
start point (in respect of the internal parametric representation of cc), \
number y is the argument of the end point. If f is given, it overrides the \
current drawing style. Permitted values of f are full, dashed and dotted.")
          ((A :point B :point C :point
              (optional f (enum "full" "dashed" "dotted")))
           "Draws triangle ABC. If f is given, it overrides the current \
drawing style. Permitted values of f are full, dashed and dotted.")
          ((A :point B :point C :point D :point
              (optional f (enum "full" "dashed" "dotted")))
           "Draws quadrilateral ABCD. If f is given, it overrides the current \
drawing style. Permitted values of f are full, dashed and dotted.")
          ((A :point B :point C :point D :point E :point
              (optional f (enum "full" "dashed" "dotted")))
           "Draws pentagon ABCDE. If f is given, it overrides the current \
drawing style. Permitted values of f are full, dashed and dotted.")
          ((A :point B :point C :point D :point E :point F :point
              (optional f (enum "full" "dashed" "dotted")))
           "Draws hexagon ABCDEF. If f is given, it overrides the current \
drawing style. Permitted values of f are full, dashed and dotted.")
          ((str :string A :point (optional x :number) a :angle)
           "Prints the text contained in str at a distance x \(default: 0.3\) \
and with argument a in respect of point A.")
          ((str :string s :segment (optional x :number) a :angle)
           "Prints the text contained in str at a distance x \(default: 0.3\) \
and with argument a in respect of the midpoint of segment s.")
          ((x :number (optional str :string) A :point (optional y :number)
              a :angle)
           "Prints the value of x optionally formated with str \(using the C \
language syntax\) at a distance y \(default: 0.3\) and with argument a in \
respect of point A.")
          ((x :number (optional str :string) s :segment (optional y :number)
              a :angle)
           "Prints the value of x optionally formated with str \(using the C \
language syntax\) at a distance y \(default: 0.3\) and with argument a in \
respect of the midpoint of segment s.")
          ((x :number "x'" :number str :string A :point (optional y :number)
              a :angle)
           "Prints the values of x and x' formated with str \(using the C \
language syntax\) at a distance y \(default: 0.3\) and with argument a in \
respect of point A.")
          ((x :number "x'" :number str :string s :segment (optional y :number)
              a :angle)
           "Prints the values of x and x' formated with str \(using the C \
language syntax\) at a distance y \(default: 0.3\) and with argument a in \
respect of the midpoint of segment s."))
    (eccentricity :point ((cc :conic-curve)))
    (ellipse :conic-curve
             ((A :point x :number y :number a :angle)
              "Ellipse of center A, such that x equals the half of the major \
axis, y the half of the minor axis and a the polar angle of the principal \
axis."))
    (end :point ((s :segment)))
    (equilateral (list :point :point :point)
                 ((x :number (optional b :angle))
                  "Defines an equilateral triangle of side length x \
\(default: 6\)."))
    (exp :number ((x :number)))
    (floor :number ((x :number)))
    (foci (list :point :point)
          ((cc :conic-curve)))
    (frame nil
           ((x :number y :number "x'" :number "y'" :number (optional z :number))
            "Sets the visible frame. The lower-left corner has coordinates \
\(x, y\) and the upper-right corner \(x', y'\), the optional parameter sets \
the unit length to z cm \(default: 1\)."))
    (height :number
            ((A :point B :point C :point)
             "Distance between point B and line AC."))
    (hexagon (list :point :point :point :point :point :point)
             ((G :point x :number a :angle)
              "It defines an hexagon of center G, side length x and such \
as polar angle of segment GA equals a."))
    (homothecy :as-first-argument
               ((A :point B :point x :number)
                "Homothecy, expansion or dilation, of center B and ratio x.")
               ((l :line A :point x :number)
                "Homothecy, expansion or dilation, of center A and ratio x.")
               ((s :segment A :point x :number)
                "Homothecy, expansion or dilation, of center A and ratio x.")
               ((c :circle A :point x :number)
                "Homothecy, expansion or dilation, of center A and ratio x.")
               ((cc :conic-curve A :point x :number)
                "Homothecy, expansion or dilation, of center A and ratio x."))
    (hyperbola :conic-curve
               ((A :point x :number :y number a :angle)
                "Hyperbola of center A, such that x equals the half of the \
real axis, y the half of the imaginary axis and a the polar angle of the \
principal axis."))
    (incircle :circle
              ((A :point B :point C :point)
               "Defines circle inscribed into A,B,C triangle."))
    (interactive :number
                 ((y :number z :number min :number max :number
                     str :string f (enum "right" "left"))
                  "It allows, while viewing, to modify the value of the \
numerical variable x using the arrow keys. The initial value of x is y, the \
increment is z. The string str is name of the interactive state. To modify x, \
one has to press first start interactive state with C-up.  After interactive \
form is started you can increase x with C-right and decrease it with \
C-left.  Once appropriate value for x is found press C-down to commit it."))
    (intersection (list :point :point)
                  ((l :line "l'" :line)
                   "Intersection point of lines l and l'.")
                  ((l :line c :circle)
                   "Intersection point of line l and circle c.")
                  ((c :circle "c'" :circle)
                   "Intersection point of two circles.")
                  ((l :line cc :conic-curve)))
    (isosceles (list :point :point :point)
               ((x :number a :angle (optional b :angle))
                "Defines an isosceles triangle with AB = x and measure of \
the angle under BAC and angle under CBA equal a (default: 6 and 39:).")
               ((x :number y :number (optional b :angle))
                "Defines an isosceles triangle with AB = x and AC = BC = y.")
               ((a :angle)
                "Defines an isosceles triangle with measure of the angle under \
BAC and angle under CBA equal a.")
               ((A :point B :point C :point)
                "Defines an isosceles triangle with AC = BC = x."))
    (label nil ((A :point (optional x :number) a :angle)
                "Prints the name of variable A at a distance x \
\(default: 0.3\) and with argument a in respect of point A."))
    (length :number ((u :vector)) ((s :segment)))
    (line :line
          ((A :point B :point)) ((A :point u :vector)) ((s :segment))
          ((A :point a :angle)
           "Line of polar angle a, containing point A.")
          ((c :circle a :angle)
           "Tangent line of circle c at point of argument a \(in respect of \
the center of c\).")
          ((cc :conic-curve x :number)
           "Tangent line of cc at point of argument x \(in respect of the \
internal parametric representation\). For parabolas, the representation is \
based on square function; for ellipses, on cosine and sine; for hyperbolas, \
on cosecant and tangent."))
    (ln :number ((x :number)))
    (major :number
           ((cc :conic-curve)
            "- Half of the major axis if cc is an ellipse;
- Half of the real axis if cc is an hyperbola;
- Semifocal chord if cc is a parabola."))
    (mark nil
          ((s :segment (optional f (enum "simple" "double" "triple" "cross")
                                 (optional x :number)))
           "Marks the segment s with a symbol corresponding to f and a scaling \
ratio x \(default: 1\). Permitted values of f are simple, double, triple and \
cross \(default: simple\).")
          ((A :point B :point C :point
              (optional f (enum "simple" "double" "triple" "dashed" "right"
                                "dot" "dotted" "arrow" "backarrow")
                        (optional x :number)))
           "Marks the angle under ABC\(in anticlockwise direction\) with a \
symbol corresponding to f and a scaling ratio x \(default: 1\). Permitted \
values of f are simple, double, triple,dashed \(which yields a curved angle \
mark with a dash\), right, dot (which yields a curved angle mark with a \
centered dot),dotted \(which yields a right angle mark with a centered dot\), \
arrow and backarrow \(default: simple\)."))
    (max :number ((x :number y :number)))
    (median :segment
            ((A :point B :point C :point)
             "Median of triangle ABC containing vertex A."))
    (midpoint :point
              ((s :segment) "Middle point of segement."))
    (min :number ((x :number y :number)))
    (minor :number
           ((cc :conic-curve)
            "- Half of the minor axis if cc is an ellipse;
- Half of the imaginary axis if cc is an hyperbola;
- Zero if cc is a parabola."))
    (ordinate :point
              ((A :point)) ((u :vector))
              ((l :line y :number) "Point of ordinate y on line l."))
    (orthocenter :point
                 ((A :point B :point C :point) "Orthocenter of triangle ABC."))
    (parabola :conic-curve
              ((A :point l :line)
               "Parabola of focus A and directrix l.")
              ((A :point x :number a :angle)
               "Parabola of center A, such that x equals the semifocal chord \
and a the measure of the angle between the principal axis and the horizontal \
direction."))
    (parallel :as-first-argument
              ((l :line A :point))
              ((s :segment A :point)))
    (parallelogram (list :point :point :point :point)
                   ((x :number y :number a :angle (optional b :angle))
                    "Defines a parallelogram with AB = x, AD = y and angle \
under BAD equals a \(default: 5, 4 and 75:\).")
                   ((u :vector v :vector (optional b :angle))
                    "Defines a parallelogram with vector from A to B equals \
u and vector from A to D equals v.")
                   ((x :number a :angle)
                    "Defines a parallelogram with AD = x and angle under BAD \
equals a."))
    (pentagon (list :point :point :point :point :point)
              ((F :point x :number a :angle)
               "It defines a pentagon of center F, radius x and such as polar \
angle of segment FA equals a."))
    (perpendicular :line
                   ((l :line A :point) (s :segment A :point)))
    (pi :number)
    (point :point
           ((x :number y :number)
            "Point of abscissa x and ordinate y.")
           ((x :number a :angle)
            "Point of radial coordinate x and polar angle a.")
           ((l :line x :number)
            "Point of abscissa x on the graduated line l. \(With Eukleides, \
lines have an implicit orientation.\)")
           ((s :segment x :number)
            "Point of abscissa x on the graduated line containing segment s. \
\(The origin is set to the begin of s. Like lines, segments have an implicit \
orientation.\)")
           ((c :circle a :angle)
            "Point of argument a \(in degrees\) on circle c.")
           ((cc :conic-curve x :number)
            "Point on conic curve cc. Number x is the argument for the \
internal parametric representation of cc. For parabolas, the representation \
is based on square function; for ellipses, on cosine and sine; for hyperbolas, \
on cosecant and tangent."))
    (projection :point
                ((A :point l :line (optional "l'" :line))
                 "Projection of point A on line l in direction of line l' \
\(default: perpendicular to line l\)."))
    (rad :number ((x :number) "Converts degrees to radians"))
    (radius :number ((c :circle)))
    (rectangle (list :point :point :point :point)
               ((x :number y :number (optional b :angle))
                "Defines a rectangle with AB = x and AD = y \(default: 6 \
and 6 / `golden ratio'\).")
               ((x :number) "Defines a rectangle with AD = x."))
    (reflection :as-first-argument
                ((A :point l :line))
                ((l :line "l'" :line) "Reflection in respect of line l'.")
                ((s :segment l :line))
                ((c :circle l :line))
                ((c :conic-curve l :line)))
    (right (list :point :point :point)
           ((x :number y :number (optional a :angle))
            "Defines a right triangle \(right angle in A\) with AB = x and \
AC = y \(default: 6 and 4.5\).")
           ((x :number a :angle (optional b :angle))
            "Defines a right triangle \(right angle in B\) with AB = x and \
measure of the angle under BAC equals a.")
           ((x :number)
            "Defines a right triangle \(right angle in A\) with AC = x.")
           ((a :angle)
            "Defines a right triangle \(right angle in B\) with measure of \
the angle under BAC equals a."))
    (rotation :as-first-argument
              ((u :vector a :angle))
              ((A :point B :point (optional a :angle))
               "Rotation of center B and angle a \(default: 180:\).")
              ((l :line A :point (optional a :angle))
               "Rotation of center A and angle a \(default: 180:\).")
              ((s :segment A :point (optional a :angle))
               "Rotation of center A and angle a \(default: 180:\).")
              ((c :circle A :point (optional a :angle))
               "Rotation of center A and angle a \(default: 180:\).")
              ((cc :conic-curve A :point (optional a :angle))
               "Rotation of center A and angle a \(default: 180:\)."))
    (round :number ((x :number)))
    (segment :segment
             ((A :point B :point)) ((A :point u :vector))
             ((A :point x :number a :angle)
              "Segment of length x and polar angle a, beginning with A.")
             ((c :circle a :angle)
              "Segment from center of circle c to point of argument a \
\(in respect of the center of c\)."))
    (sign :number
          ((x :number)
           "Returns -1 if x<0, 0 if x=0, 1 if x>0"))
    (sin :number ((x :number)))
    (sqrt :number ((x :number)))
    (square (list :point :point :point :point)
            ((x :number (optional b :angle))
             "Defines a square with AB = x \(default: 4\)."))
    (strokes nil
             ((x :number)
              "Sets the number of strokes drawn by a trace command \
\(default: 150\). The minimal number of strokes is 3 and the maximal number \
is 300."))
    (style nil
           ((f (enum "full" "dashed" "dotted"))
            "Sets the current drawing style to f. Permitted values of f are \
full, dashed and dotted. The default drawing style is full."))
    (tan :number ((x :number)))
    (thickness nil
               ((x :number)
                "Multiplies the current linewidth by x. The default linewidth \
is 0.5 pt."))
    (trace nil
           ((x :number "x'" :number "x''" :number
               (optional f (enum "full" "dashed" "dotted")))
            "This statement has to be followed by at least one point valued \
expression enclosed in braces \(depending on variable x\). Parameters x' and \
x\" are the lower and upper bounds of x. The expression has to be defined for \
each value of the interval. If not, it may lead to spurious results or \
sometimes \(with eukleides\) to an error message. Within the braces, in order \
to perform intermediate computations, it is possible to add any sequence of \
statements before the expression. If f is given, it overrides the current \
drawing style. Permitted values of f are full, dashed and dotted."))
    (translation :as-first-argument
                 ((A :point u :vector)) ((l :line u :vector))
                 ((s :segment u :vector)) ((c :circle u :vector))
                 ((cc :conic-curve u :vector)))
    (triangle (list :point :point :point)
              ((x :number (optional b :angle))
               "Defines a scalene triangle with AB = x \(default: 6\). The \
triangle is an optimal scalene triangle \(i.e. an acute triangle which shape \
is as far as possible from the shape of right or isosceles triangles\).")
              ((x :number y :number z :number (optional b :angle))
               "Defines a scalene triangle with AB = x, BC = y and AC = z.")
              ((x :number a :angle "a'" :angle (optional b :angle))
               "Defines a scalene triangle with AB = x, measure of the angle \
under BAC equals a, measure of the angle under CBA equals a'.")
              ((x :number y :number)
               "Defines a scalene triangle with BC = x and AC = y.")
              ((a :angle "a'" :angle)
               "Defines a scalene triangle with measure of the angle under \
BAC equals a,measure of the angle under CBA equals a'."))
    (tricks nil
            ("tricks str"
             "Adds an initial `\' to str and puts it verbatim to the output. \
It has to be valid TeX code."))
    (vector :vector
            ((x :number y :number) "Vector of abscissa x and ordinate y.")
            ((x :number a :angle) "Vector of length x and argument a.")
            ((A :point B :point) "Vector from point A to point B.")
            ((l :line) "Unit length vector with same direction than line l.")
            ((s :segment) "Vector from begin of segment s to end of s."))
    (vertices (list :point :point)
              ((cc :conic-curve))))
  "List of all Eukleides keywords.
Notation used in brief ref and documentation:
  * x, y, z : number.
  * a, b : angular parameter.
  * A, B, C, D, E, F, G : point.
  * u, v : vector.
  * l : line.
  * s : segment.
  * c : circle.
  * cc : conic curve.
  * str : string.
  * f : flag.

An angular parameter is a number valued expression followed by `:'
\(degrees\) or `<' \(radians\) ")

(defun eukleides-keyword-name (kw)
  "Return name of eukleides keyword structure KW."
  (symbol-name (car kw)))

(defalias 'eukleides-keyword-type 'second)
(defalias 'eukleides-keyword-args 'cddr)

;;}}}
;;{{{   `-- Font lock keywords

(defvar eukleides-font-lock-keywords
  (list (list (concat "\\<\\("
                      (regexp-opt
                       (mapcar #'eukleides-keyword-name eukleides-keywords))
                      "\\)\\>")
              1 'font-lock-builtin-face)
        (list (concat "\\<"
                      (regexp-opt
                       ;; XXX
                       '("black" "darkgray" "gray" "lightgray" "white" "red"
                         "green" "blue" "cyan" "magenta" "yellow""full"
                         "dashed" "dotted" "dot" "disc" "box" "cross" "plus"
                         "entire" "halfline" "backhalfline" "noarrow" "arrow"
                         "backarrow" "doublearrow" "simple" "double" "triple"
                         "cross" "simple" "double" "triple" "dashed" "right"
                         "dot" "dotted" "arrow" "backarrow") t)
                      "\\>") 1 font-lock-type-face)
        '("\\\\\\s-*\\w*\\(\\[[^]]*\\]\\)?" 0 nil) ; ???
        '("\\([0-9.]\\|\\w\\)+\\(:\\|<\\)" 2 eukleides-angle-mark-face)
;        '("\\<\\([a-zA-Z]\\w*\\)\\>" 1 font-lock-variable-name-face)
        )
  "Fontlock keywords for `eukleides-mode'.")

;;}}}
;;}}}

;;{{{ `-- Imenu

(defun eukleides-imenu-create-gexp (mae)
  "Create list to be part of `imenu-generic-expression'."
  (let ((ret nil))
    (dotimes (i (cdr mae) ret)
      (setq ret (cons
                 (list nil
                       (concat (apply #'concat "\\(^\\|;\\)"
                                      (make-list (cdr mae) "\\s-*\\(\\w*\\)"))
                               "\\s-+"
                               (eukleides-keyword-name mae))
                       (+ i 2))
                 ret)))))

(defun eukleides-imenu-create-mexp (kw)
  "Create part of imenu for eukleides keyword KW."
  (save-excursion
    (goto-char (point-min))
    (let ((args (1- (length (eukleides-keyword-type kw))))
          (ret nil))
      (while (re-search-forward
              (concat (apply #'concat "\\(^\\|;\\)"
                             (make-list args "\\s-*\\(\\w*\\)"))
                      "\\s-+"
                      (eukleides-keyword-name kw))
              nil t)
        (dotimes (i args)
          (push (cons (match-string (+ i 2))
                      (match-beginning (+ i 2))) ret)))
      ret)))

(defun eukleides-generate-variables (start end)
  "Generate list of defined eukleides variables in Imenu format.
START and END specifies point restrictions."
  (save-restriction
    (narrow-to-region start end)
    (nreverse
     (delete-duplicates
      ;; Reverse the list, so if duplicates are removed the last
      ;; definition is reminded, after removing duplicates nreverse
      ;; again, so most recent variables occurs first in returned list
      (nreverse
       (apply #'append
              (let ((ret nil))
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward
                          "\\(^\\|;\\)\\s-*\\(\\w*\\)\\s-*=" nil t)
                    (push (cons (match-string 2) (match-beginning 2)) ret))
                  ret))
              (mapcar #'eukleides-imenu-create-mexp
                      (loop for kw in eukleides-keywords
                        when (and (consp (eukleides-keyword-type kw))
                                  (eq 'list (car (eukleides-keyword-type kw))))
                        collect kw))))
      :key 'car :test 'string=))))

(defun eukleides-imenu-create-function ()
  "Imenu generator for Eukleides files."
  (eukleides-generate-variables (point-min) (point-max)))

;;}}}
;;{{{ `-- Eukleides Menu

(defvar eukleides-menu nil
  "Menu for Eukleides Mode.
This menu will get created automatically if you have the `easymenu'
package. Note that the latest (S)XEmacs releases contain this package.")

(ignore-errors
  (require 'easymenu)
  (easy-menu-define
   eukleides-menu eukleides-mode-map "Eukleides major mode"
   '("Eukleides"
     ["Comment Region" comment-region (mark)]
     ["Uncomment Region" (lambda () (interactive) (comment-region '(4))) (mark)]
     "-"
     ["Compile" eukleides-compile t]
     ["Preview" eukleides-preview t]
     "-"
     ["Interactive start" eukleides-interactive-start]
     ["Interactive +" eukleides-interactive-increase]
     ["Interactive -" eukleides-interactive-decrease]
     ["Interactive commit" eukleides-interactive-commit]
     "-"
     ["Customize..." eukleides-customize t]
     )))

;;}}}
;;{{{ `-- Eukleides major mode

;;;###autoload
(define-derived-mode eukleides-mode fundamental-mode "Eukleides"
  "A major mode for editing Eukleides files."
  :abbrev-table eukleides-mode-abbrev-table
  (set (make-local-variable 'comment-start) "% ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "%+\\s-*")
  (put 'eukleides-mode 'font-lock-defaults
       '(eukleides-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) #'eukleides-indent-line)

  ;; Imenu support
  (when (ignore-errors (require 'imenu))
    (setq imenu-create-index-function 'eukleides-imenu-create-function)
    (if (fboundp 'imenu-add-to-menubar)
	(imenu-add-to-menubar (format "%s-%s" "IM" mode-name))))
  ;; Add menu
  (if eukleides-menu
      (easy-menu-add eukleides-menu))
  )

(defun eukleides-indent-line ()
  "Indent current line for eukleides mode."
  (interactive)
  (indent-line-to 0))

;;}}}
;;{{{ `-- Contextual completer

(defmacro eukleides-statement-at-point ()
  "Return statement at point."
  `(save-excursion
     (goto-char (eukleides-statement-beginning))
     (current-word t)))

(defmacro eukleides-statement-intern (stat)
  "Return eukleides keyword definition for STAT."
  `(and ,stat (assq (intern ,stat) eukleides-keywords)))

(defun eukleides-statement-beginning ()
  "Return beginning of current statement."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-at-bol) (point-at-eol))
      (let ((sl (scan-lists (point) -1 1 nil t))
            (sc (and (or (search-backward ";" nil t)
                         (search-backward "=" nil t))
                     (match-end 0))))
        (cond ((and sl sc (> sc sl)) sc)
              ((or (and sl sc (> sl sc))
                   (and (not sc) sl))
               (goto-char sl)
               (backward-word)
               (point))
              ((and (not sc) (not sl))
               (point-at-bol))
              ((and (not sl) sc)
               (goto-char sc)
               (and (looking-at "\\s-*\\(.\\)")
                    (match-beginning 1)))
              (t (error "Unknown place: sl=%S sc=%S" sl sc)))))))

(defun eukleides-statement-clean-type (clean-statement)
  "Get eukleides type from CLEAN-STATEMENT."
  (eukleides-keyword-type
   (find clean-statement eukleides-keywords
         :test #'string= :key #'eukleides-keyword-name)))

(defun eukleides-statement-type (statement)
  "Return STATEMENT's type."
  ;; TODO: process :as-first-argument keywords
  (or (and (string-match "^[0-9.]+:$" statement) :angle)
      (and (string-match "^[0-9.]+$" statement) :number)
      (and (string-match "^\"[^\"]*\"$" statement) :string)
      (let* ((kwn (mapcar #'eukleides-keyword-name eukleides-keywords))
             (kwr (concat (regexp-opt kwn t) "\\s-*("))
             ;; Adjust statement, in case of angle
             (statement (replace-in-string statement ":$" "")))
        (or (and (string-match kwr statement)
                 (eukleides-statement-clean-type (match-string 1 statement)))
            ;; Search across single assignments(`=')
            (save-excursion
              (when (re-search-backward
                     (concat "\\<" statement "\\>"
                             "\\s-*=\\s-*\\(\\w+\\)\\s-*(") nil t)
                ;; XXX intersection is special case
                (if (string= (match-string 1) "intersection")
                    :point
                  (eukleides-statement-clean-type (match-string 1)))))
            ;; Search across multiple assignments
            ;; NOTE: Every multiple assignment command returns only
            ;; points
            (save-excursion
              (when (re-search-backward
                     (concat
                      "\\<" statement "\\>"
                      "\\(?:"
                      "\\s-*=\\s-*\\(\\w+\\)\\s-*("
                      "\\|"
                      "\\(?:\\s-+\\w\\)*\\s-\\(\\w+\\)\\s-*\\((\\|\\s-*$\\)"
                      "\\)"
                      ) nil t)
                (let ((ms (or (match-string 1) (match-string 2)))
                      (masreg
                       (regexp-opt
                        (loop for kw in eukleides-keywords
                          when (and (consp (eukleides-keyword-type kw))
                                    (eq 'list
                                        (car (eukleides-keyword-type kw))))
                          collect (eukleides-keyword-name kw)))))
                  (and (string-match masreg ms) :point))))
            ))))

(defun eukleides-arguments-format (args)
  "Format eukleides keyword arguments ARGS."
  (concat
   (let ((el (car args)))
     (cond ((null args) "")
           ((or (symbolp el) (stringp el))
            (setq args (cddr args))
            (format "%s" el))
           ((and (consp el) (eq (car el) 'optional))
            (setq args (cdr args))
            (format "[%s]" (eukleides-arguments-format (cdr el))))
           ((and (consp el) (eq (car el) 'enum))
            (setq args (cdr args))
            (format "%s" el))
           (t (error "Unknown argument %S" el))))
   (when args
     (concat ", " (eukleides-arguments-format args)))))

(defun eukleides-arguments-fetch ()
  "Fetch arguments of command.
Return list in form:
\(number-of-args point-at-arg arg1 arg2 ..\)"
  (let* ((cp (point))                   ; current point
         (abp (scan-lists (point) -1 1 nil t)) ; args beginning point
         (aep (scan-lists (point) 1 1 nil t)) ; args ending point
         (last-point abp)
         (args nil) (args-count 0) (paa nil))
    ;; AEP is not necessary, only ABP is required
    (when abp
      (save-excursion
        (goto-char (1+ abp))
        (while (and (> (point) last-point) (or (null aep) (< (point) aep))
                    (or (and
                         (let ((gp (scan-lists (point) 1 0 nil t)))
                           (when (and gp (or (null aep) (< gp aep)))
                             (save-excursion
                               (goto-char gp)
                               (or (looking-at "\\(\\s-*\\),")
                                   (and aep (looking-at "\\(\\s-*\\))")))))))
                        (and aep (looking-at "\\([^);,\n]*\\)\\()\\|,\\)"))
                        (and (null aep) (looking-at "\\([^);\n]*\\)$"))
                        ))
          (let ((mae0 (match-end 0))
                (mae1 (match-end 1)))
            (when (and (<= (point) cp) (>= mae1 cp))
              (setq paa args-count))
            (push (replace-in-string
                   (replace-in-string
                    (buffer-substring (point) mae1)
                    "^\\s-+" "")
                   "\\s-+$" "")
                  args)
            (incf args-count)

            ;; Advance point
            (setq last-point (point))
            (goto-char mae0)

            ;; XXX break in case of eol
            (when (looking-at "$")
              (setq last-point (point))
              (unless paa
                (setq paa args-count)
                (incf args-count)
                (push "" args)))
            ))))
    (nconc (list args-count paa) (nreverse args))))

(defun eukleides-types-list-fixate (kw-types)
  (cond ((and (consp kw-types)
              (eq 'optional (car kw-types)))
         (cons 'optional (eukleides-types-list-fixate (cdr kw-types))))
        ((and (consp kw-types)
              (eq 'enum (car kw-types)))
         kw-types)
        ((consp kw-types)
         (let ((ret nil))
           (dolist (type kw-types ret)
             (cond ((memq type eukleides-types)
                    (push type ret))
                   ((consp type)
                    (push (eukleides-types-list-fixate type) ret))))
           (nreverse ret)))))

(defun eukleides-types-list-length (types)
  (- (length types)
     (let ((lt (car (last types))))
       (if (and (consp lt) (eq 'optional (car lt)))
           (- 1 (eukleides-types-list-length (cdr lt)))
         0))))

(defun eukleides-arguments-match-p (args kw-args)
  "Return non-nil if ARGS matches keyword's arguments KW-ARGS.
ARGS in form (number-of-args point-at-arg arg1 arg2 ..)."
  (let ((stats (cddr args))
        (types kw-args)
        (last-type (car (last kw-args)))
        (current-arg (second args)))
    (and (<= (first args) (eukleides-types-list-length kw-args))
         (progn
           (while (and stats types
                       (cond ((and (consp (car types))
                                   (eq 'optional (first (car types))))
                              (cond ((eq (car types) last-type)
                                     (prog1
                                         (eukleides-arguments-match-p
                                          (cons (length stats)
                                                (cons current-arg stats))
                                          (cdr last-type))
                                       (setq stats nil)))
                                    ((zerop current-arg) t)
                                    (t
                                     ;; otherwise traverse types until match
                                     (while (and types (consp (car types))
                                                 (eq 'optional (first (car types)))
                                                 (not (eq (eukleides-statement-type
                                                           (car stats))
                                                          (second (car types)))))
                                       (setq types (cdr types)))
                                     (not (null types)))))
                             ((zerop current-arg) t)
                             ((and (consp (car types))
                                   (eq 'enum (car types)))
                              (member (car stats)
                                      (cdr (car types))))
                             (t (eq (eukleides-statement-type (car stats))
                                    (car types)))))
           (decf current-arg)
           (setq stats (cdr stats)
                 types (cdr types)))
         (not stats)))
  ))

(defun eukleides-keyword-with-parens (kw)
  (concat (eukleides-keyword-name kw)
          (if eukleides-complete-keyword-with-parens
              "()" "")))

(defun eukleides-completion-list (type)
  "Return list of completions for given eukleides TYPE."
  (cond ((and (consp type) (eq (car type) 'optional))
         (eukleides-completion-list (cadr type)))
        ((and (consp type) (eq (car type) 'enum))
         (cdr type))
        ((keywordp type)
         (let ((compv
                (mapcar #'car
                        (filter
                         #'(lambda (v)
                             (let ((vt (eukleides-statement-type (car v))))
                               (or (eq type vt)
                                   ;; Accept numbers as completion for
                                   ;; angles
                                   (and (eq type :angle) (eq vt :number)))))
                         (eukleides-generate-variables (point-min) (point)))))
               (compk
                (mapcar #'eukleides-keyword-with-parens
                        (filter #'(lambda (kw)
                                    (eq type (eukleides-keyword-type kw)))
                                eukleides-keywords))))
           (nconc compv compk)))
        (t (error "Unknown eukleides type: %S" type))))

(defun eukleides-complete-symbols ()
  "Return list of possible completions at point."
  (let* ((kw (eukleides-statement-intern (eukleides-statement-at-point)))
         (cmd-args (and kw (eukleides-arguments-fetch)))
         (types-list nil))
    (dolist (type (mapcar #'car (cddr kw)) types-list)
      (let ((ftype (eukleides-types-list-fixate type)))
        (when (and (not (memq (nth (second cmd-args) ftype)
                              types-list))
                   (eukleides-arguments-match-p cmd-args ftype))
          (push (nth (second cmd-args) ftype) types-list))))

     (or (apply #'nconc
                (mapcar #'eukleides-completion-list
                        (nreverse types-list)))
         ;; Default completion is keyword completion
         (mapcar #'eukleides-keyword-with-parens
                 eukleides-keywords))
    ))

(defun eukleides-complete ()
  "Complete at point according to context."
  (interactive)
  (completer-complete-goto "^ \t\n\(\)[]{}," completer-words
                           (mapcar #'list (eukleides-complete-symbols)) nil)
  )

;;}}}

;;{{{ `-- Compilation & Preview

(require 'compile)

(defun eukleides-setup-compilation (compfile &optional srcfile)
  "Setup compilation buffer."
  (with-current-buffer (get-buffer-create "*compilation*")
    (erase-buffer)
    ;; XXX fake compilation process
    (insert "cd " (file-name-directory srcfile) "\n")
    (insert "eukleides " (file-name-nondirectory srcfile) "\n")
    (insert (file-name-nondirectory srcfile) ": ")

    (insert-file compfile)
    (goto-char (point-min))
    ;; Convert warning/error messages to supported format
    (while (re-search-forward " \\(?:Error\\|Warning\\) at line \\([0-9]+\\): \
\\(parse error\\|undefined points?\\|\\(?:undefined\\|invalid\\) \
angle\\|parallel lines\\|undefined barycenter\\|undefined conic \
curve\\|invalid length\\)" nil t)
      (replace-match (concat (match-string 1) ": " (match-string 2))))
    (let ((outwin (display-buffer (current-buffer))))
      (goto-char (point-max))

      (compilation-mode)
      (setq default-directory (file-name-directory srcfile)
            compilation-directory-stack (list default-directory))
      (compilation-set-window-height outwin)
      ;; Make it so the next C-x ` will use this buffer.
      (setq compilation-last-buffer (current-buffer)))))

(defun eukleides-euk-to-tex (eukfile)
  "Convert Eukleides file EUKFILE to TeX."
  (let ((texfile (concat (file-name-sans-extension eukfile) ".tex"))
        (errfile (concat (file-name-sans-extension eukfile) ".err"))
        (cbuff (current-buffer)))
    (with-temp-buffer
      (unwind-protect
          (unless (zerop (call-process "eukleides" nil
                                       (list (current-buffer) errfile)
                                       nil eukfile))
            ;; Mark compilation finished
            (with-current-buffer cbuff
              (setq compilation-in-progress nil))
            (eukleides-setup-compilation errfile (buffer-file-name cbuff))
            (error "Failed compiling eukleides file: %s"
                   (buffer-file-name cbuff)))
        (delete-file errfile))
      (goto-char (point-min))
      (insert "\\input pst-eps\\input pst-plot\\input pst-node\\TeXtoEPS\n")
      (goto-char (point-max))
      (insert "\\endTeXtoEPS\\nopagenumbers\\end\n")
      (let ((executing-kbd-macro []))   ; XXX avoid "Wrote .." messages
        (write-file texfile))
    texfile)))

(defun eukleides-tex-to-dvi (texfile)
  "Convert TeX file TEXFILE to DVI."
  (let ((dvifile (concat (file-name-sans-extension texfile) ".dvi")))
    (unless (zerop (call-process "tex" nil (get-buffer-create "mytexoutput") nil
                                 "--interaction=nonstopmode"
                                 "-output-directory"
                                 (file-name-directory texfile) texfile))
      (error "Failed compiling tex file"))
    dvifile))

(defun eukleides-dvi-to-eps (dvifile)
  "Convert DVIFILE to EPS file."
  (let ((epsfile (concat (file-name-sans-extension dvifile) ".eps")))
    (unless (zerop (call-process "dvips" nil (current-buffer) nil
                                 "-q" "-f" "-E" "-o" epsfile dvifile))
      (error "Failed converting DVI to EPS"))
    epsfile))

(defun eukleides-eps-extract-bb (filename)
  "Extract EPS bounding box vector from FILENAME.
Returns a list of bounding box, width, and height."
  (with-temp-buffer
    (insert-file-contents-literally filename nil 0 1024 t)
    (goto-char (point-min))
    (when (search-forward-regexp "%%BoundingBox:\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)" nil t)
      (let ((bb (vector
                 (floor (string-to-number (match-string 1)))
                 (floor (string-to-number (match-string 2)))
                 (ceiling (string-to-number (match-string 3)))
                 (ceiling (string-to-number (match-string 4))))))
        (list bb
              (- (aref bb 2) (aref bb 0))
              (- (aref bb 3) (aref bb 1)))))))

(defun eukleides-eps-scale (file bb scale)
  "Scale the eps image in FILE with factor SCALE.
BB is the bounding box of the image.  Returns a list of new bounding
box, width, and height."
  (multiple-value-bind (llx lly urx ury) (append bb nil)
    (let ((x (round (* (- urx llx) scale)))
          (y (round (* (- ury lly) scale)))
	  (buff (find-file-noselect file)))
      (unwind-protect
	  (with-current-buffer buff
	    (goto-char (point-min))
	    (search-forward "%%BoundingBox")
	    (delete-region (line-beginning-position) (line-end-position))
	    (insert (format "%%%%BoundingBox: 0 0 %d %d\n" x y))
	    (search-forward "%%EndComments")
	    (forward-line)
	    (insert "%%BeginProcSet: eukleides 1 0\ngsave\n")
	    (insert (format "%f %f translate\n"
			    (- (* llx scale))
			    (- (* lly scale))))
	    (insert (format "%f %f scale\n" scale scale))
	    (insert "%%EndProcSet\n")
	    (goto-char (point-max))
	    (insert "\ngrestore")
            (let ((executing-kbd-macro [])) ; XXX avoid "Wrote .." messages
              (basic-save-buffer)))
	(kill-buffer buff))
      (list (vector 0 0 x y) x y))))

(defun eukleides-eps-to-image (epsfile)
  "Convert EPS file EPSFILE to image."
  (let ((imgfile (format "%s.%s" (file-name-sans-extension epsfile)
                         eukleides-image-type)))
    ;; Scale EPS file
    (eukleides-eps-scale
     epsfile (car (eukleides-eps-extract-bb epsfile))
     eukleides-scale-factor)
    (unless (zerop (call-process "convert" nil nil nil epsfile imgfile))
      (error "Failed converting from EPS to %S" eukleides-image-type))
    imgfile))

(defun eukleides-make-image (texfile)
  "Create image from TEXFILE.
Image createid in `eukleides-image-type' format."
  (let* ((dvifile (eukleides-tex-to-dvi texfile))
         (epsfile (eukleides-dvi-to-eps dvifile))
         (imgfile (eukleides-eps-to-image epsfile)))
    (unwind-protect
        (make-glyph (vector eukleides-image-type :file imgfile))
      (mapc #'delete-file
            (list texfile dvifile epsfile imgfile
                  (concat (file-name-sans-extension texfile) ".log"))))))

(defun eukleides-make-image-from-str (str)
  "Create image for eukleides code string STR."
  (let ((eukfile (expand-file-name
                  (concat (make-temp-name "euk") ".euk")
                  temporary-file-directory)))
    (with-temp-buffer
      (insert str)
      (let ((executing-kbd-macro []))   ; XXX avoid "Wrote .." messages
        (write-file eukfile)))
    (unwind-protect
        (let ((texfile (eukleides-euk-to-tex eukfile)))
          (when eukleides-auto-preview
            (eukleides-make-image texfile)))
      (delete-file eukfile))))


(defun eukleides-compile (&optional start end)
  "Compile eukleide file.
If region is selected, then compile only eukleides code of the region."
  (interactive (and (region-active-p)
                    (list (region-beginning) (region-end))))

  (let ((gl (eukleides-make-image-from-str
             (eukleides-interactive-substitute (buffer-substring start end))))
        (buffer-name (format "Preview `%s'" (buffer-name))))
    (when gl
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (set-extent-end-glyph
         (or (extent-at 1 nil nil nil 'at) (make-extent 1 1)) gl))
      (save-selected-window
        (switch-to-buffer-other-window buffer-name)
        (enlarge-window-pixels
         (- (glyph-height gl) (window-pixel-height)))))))

(defun eukleides-mc-compile ()
  "Compile command for `mode-compile'."
  (flet ((tcrt (time)
           (+ (* (nth 0 time) 65536.0) (nth 1 time)
              (/ (nth 2 time) 1000000.0))))
    (let ((st (tcrt (current-time))))
      (call-interactively #'eukleides-compile)
      (message "Compiled without errors in %f seconds."
        (- (tcrt (current-time)) st)))))

(defun eukleides-preview ()
  "Preview eukleides file.
Preview it even if `eukleides-auto-preview' is nil."
  (interactive)
  (let ((eukleides-auto-preview t))
    (call-interactively #'eukleides-compile)))

;; Add mode-compile support
(ignore-errors
  (require 'mode-compile)
  (push '(eukleides-mode eukleides-mc-compile keyboard-quit)
        mode-compile-modes-alist))

;;}}}

(defun eukleides-lpr-print (file)
  (let ((fbuf (find-file-noselect file)))
  (unwind-protect
      (with-current-buffer fbuf
        (lpr-region (point-min) (point-max)))
    (kill-buffer fbuf))))

(defun eukleides-print (&optional file)
  "Print current eukleides file.
If FILE is specified, then print to FILE."
  (interactive (list (when current-prefix-arg (read-file-name "Print to file: "))))
  (let* ((texfile (eukleides-euk-to-tex
                   (buffer-file-name (current-buffer))))
         (dvifile (eukleides-tex-to-dvi texfile))
         (epsfile (eukleides-dvi-to-eps dvifile)))
    (eukleides-eps-scale
     epsfile (car (eukleides-eps-extract-bb epsfile))
     eukleides-scale-factor)
    (unwind-protect
        (if file
            (rename-file epsfile file t)
          (eukleides-lpr-print epsfile))
      (ignore-errors
        (delete-file texfile)
        (delete-file dvifile)
        (delete-file epsfile)))))

;;{{{ `-- Interactive forms

(defvar eukleides-interactive-state nil
  "Current interactive state.")
(make-variable-buffer-local 'eukleides-interactive-state)

(defvar eukleides-interactive-values nil
  "List of interactive values.
List of elements in form \(STATE VALUE INCREMENT LOWER UPPER\).")
(make-variable-buffer-local 'eukleides-interactive-values)

(defun eukleides-interactive-value ()
  "Return current interactive value."
  (second (assoc eukleides-interactive-state
                 eukleides-interactive-values)))
(defsetf eukleides-interactive-value () (val)
  `(setf (second (assoc eukleides-interactive-state
                        eukleides-interactive-values))
         ,val))

(defun eukleides-interactive-substitute (str)
  "Replace interative forms in STR with its actual values."
  (let ((ivals eukleides-interactive-values)
        (sivals nil))
    (prog1
        (with-temp-buffer
          (insert str)
          (goto-char (point-min))
          (while (re-search-forward
                  "interactive(\\([^,]+\\),\\([^,]+\\),[^\"]+\"\
\\([^\"]+\\)\"[^)]+)" nil t)
            (let* ((istate (match-string 3))
                   (ival (assoc istate ivals)))
              (if ival
                  (replace-match (format "%S" (second ival)))
                ;; Register new state
                (let* ((val (string-to-int (match-string 1)))
                       (inc (string-to-int (match-string 2)))
                       (nival (list istate val inc nil nil)))
                  (push nival sivals)
                  (replace-match (format "%S" (second nival)))))))
          (buffer-substring))
      (dolist (siv (nreverse sivals))
        (push siv eukleides-interactive-values)))))

(defun eukleides-interactive-increase (arg)
  "Increase interactive value."
  (interactive "p")
  (unless eukleides-interactive-state
    (error "Interactive state is not set"))
  (let ((ival (assoc eukleides-interactive-state
                     eukleides-interactive-values)))
    (unless ival
      (error "Invalid interactive state"))
    (dotimes (i (abs arg))
      (incf (eukleides-interactive-value)
            (* (signum arg) (third ival))))
    (eukleides-compile)
    (message "Interactive state `%s', value `%S'"
             eukleides-interactive-state
             (eukleides-interactive-value))
    ))

(defun eukleides-interactive-decrease (arg)
  "Decrease interactive value."
  (interactive "p")
  (eukleides-interactive-increase (- arg)))

(defun eukleides-interactive-start (state)
  "Start interactive STATE."
  (interactive (list (completing-read
                      "Interactive state: "
                      eukleides-interactive-values
                      nil t)))
  (setq eukleides-interactive-state state)
  (message "State `%s' started with value `%S'"
           eukleides-interactive-state (eukleides-interactive-value)))

(defun eukleides-interactive-commit ()
  "Commit current state value.
Substitute interactive form with current value."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         (format "interactive([^\"]+\"%s\"[^)]+)" eukleides-interactive-state)
         nil t)
        (when (or (not eukleides-interactive-commit-query)
                  (y-or-n-p (format "Commit state `%s' with `%S' value? "
                                    eukleides-interactive-state
                                    (eukleides-interactive-value))))
          (replace-match (format "%S" (eukleides-interactive-value)))
          (setq eukleides-interactive-values
                (delete (assoc eukleides-interactive-state
                               eukleides-interactive-values)
                        eukleides-interactive-values))
          (setq eukleides-interactive-state nil))
      (error "Interactive form for `%s' state not found"
             eukleides-interactive-state))))

;;}}}

;;{{{ `-- Help commands

(defun eukleides-help-subject (subject)
  "Popup help buffer for SUBJECT."
  (flet ((euk-form-keyword (kw)
           (insert (format "* %s(%s)\n" subject
                           (eukleides-arguments-format (car kw))))
           (when (cadr kw)
             (fill-region
              (point) (progn (insert (cadr kw) "\n") (point))))
           (insert "\n")))
    (let ((eukk (eukleides-statement-intern subject)))
      (unless eukk
        (error "No help for subject: %S" subject))
      (with-displaying-help-buffer
        #'(lambda ()
            (set-buffer standard-output)
            (insert (format "== %s() -> %S ==\n" (first eukk)
                            (or (second eukk) "")))
            (insert "\n")
            (mapc #'euk-form-keyword (cddr eukk))
            (buffer-substring))
        subject))))

(defun eukleides-help (&optional arg)
  "Provide help for word and point."
  (interactive "P")
  (let* ((kw (eukleides-statement-intern (current-word t)))
         (curw (or (and kw (eukleides-keyword-name kw))
                   (eukleides-statement-at-point)))
         (subj (if arg
                   curw
                 (completing-read
                  (format "Eukleides help [%s]: " curw)
                  (mapcar #'(lambda (kw)
                              (list (eukleides-keyword-name kw)))
                          eukleides-keywords)
                  nil t nil nil curw))))
    (eukleides-help-subject subj)))

;;}}}
;;{{{ `-- `info-lookup-symbol' support

(ignore-errors
  (require 'info-look)
  (info-lookup-maybe-add-help
   :mode 'eukleides-mode
   :regexp "[^][()'\" \t\n]+"
   :doc-spec '(("(eukleides)Command Index"))))

;;}}}
;;{{{ `-- Eldoc

(defun eukleides-eldoc-print-current-symbol-info ()
  "Print information using `eldoc-message' while in function `eldoc-mode'.
You can override the info collecting part with `eldoc-current-symbol-info'."
  (let* ((cw (current-word t))
         (kd (and cw (eukleides-statement-type cw))))
    (if kd
        (eldoc-message (format "%s %S" cw kd))
      ;; Try command
      (setq cw (eukleides-statement-at-point)
            kd (eukleides-statement-intern cw))
      ;; Format statement string for eldoc
      (when kd
        (eldoc-message
         (concat cw " "
         (mapconcat #'(lambda (kwa)
                        (concat
                         "\("
                         (eukleides-arguments-format (car kwa))
                         "\)"))
                    (cddr kd) "  "))))
      )))

(defadvice eldoc-print-current-symbol-info (around eukleides-eldoc activate)
  "Enable ELDOC in for eukleides mode."
  (if (not (eq major-mode 'eukleides-mode))
      ad-do-it
    (when (eldoc-display-message-p)
      (eukleides-eldoc-print-current-symbol-info))))

;;}}}

;;;###autoload
(unless (rassq 'eukleides-mode auto-mode-alist)
  (push '("\\.euk$" . eukleides-mode) auto-mode-alist))

(provide 'eukleides)

;;; eukleides.el ends here
