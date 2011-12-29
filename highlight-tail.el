;;; highlight-tail.el --- draw a colourful "tail" while you write



;; Author: Rafal Jedruszek <necui-emahte@nic-nac-project.net>

;; Keywords: highlight tail eye-candy visual-effect light color burn

;; Web page: http://nic-nac-project.net/~necui/ht.html



(defconst highlight-tail-version "1.3.9"

  "The current version of `highlight-tail-mode'.")



;; This file is not part of GNU Emacs.



;; This program is free software; you can redistribute it and/or

;; modify it under the terms of the GNU General Public License as

;; published by the Free Software Foundation; either version 2, or (at

;; your option) any later version.



;; This program is distributed in the hope that it will be useful, but

;; WITHOUT ANY WARRANTY; without even the implied warranty of

;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU

;; General Public License for more details.



;; You should have received a copy of the GNU General Public License

;; along with GNU Emacs; see the file COPYING.  If not, write to the

;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,

;; Boston, MA 02111-1307, USA.



;;; Commentary:



;; === WHAT IS IT?

;;

;; This minor-mode draws a tail in real time, when you write.  It

;; changes the background color of some last typed characters and

;; smoothly fade them out to the background color.

;;

;; So from now on, your Emacs will be even more sexy! ;o )

;;

;; If you do not understand what I mean, check the animation:

;; http://nic-nac-project.net/~necui/img/htshow.gif

;;

;; === INSTALLATION

;;

;; Place this file in your load-path and add

;;

;; (require 'highlight-tail)

;; (message "Highlight-tail loaded - now your Emacs will be even more sexy!")

;; ;

;; ; [ here some setq of variables - see CONFIGURATION section below ]

;; ;

;; (highlight-tail-mode)

;;

;; to your ~/.emacs

;;

;; === CONFIGURATION

;;

;; The default configuration is quite good, but you could and should

;; customize it to your own needs.  Here are some examples.  It's hard

;; for me to explain what you will see when use them, so just give

;; them a try.  Place these setq in your .emacs.el file *before* the

;; (highlight-tail-mode) function call.

;;

;; 1. -----

;;

;; (setq highlight-tail-colors '(("black" . 0)

;;                               ("#bc2525" . 25)

;;                               ("black" . 66)))

;; 

;; 2. -----

;;

;; (setq highlight-tail-steps 14

;;       highlight-tail-timer 1)

;; 

;; 3. -----

;; 

;; (setq highlight-tail-posterior-type 'const)

;;

;; These are all customizable variables.  I think you get the idea

;; how to customize this mode for best fit.

;; 

;; ATTENTION

;;

;; You will often need to run (highlight-tail-reload) function to make

;; changes work :).

;;

;; === SPEED

;;

;; From version 0.8 this mode doesn't use much CPU power.  There were

;; problems with this in earlier versions.  Now it takes about 2-8% of

;; my Athlon XP 2000+ power in normal speed typing (maybe it is still

;; too much? - if you have suggestions how to make it eat less - mail

;; me).  When I press a key and hold it down it eats approximately 15%

;; (in comparison to prior version (100%) it is a very good result).

;; The CPU eaten depends mainly on two variables:

;; `highlight-tail-steps' and `highlight-tail-timer'.  So combine with

;; these two to achieve satisfactory performance.

;;

;; === Terminals

;;

;; Highlight-tail doesn't work on terminals (only in window-systems

;; like X11)



;;; History:



;; * 1.3.9 (02 Apr 2007)

;;      + highlight-tail-mode function does do start highlight-tail

;;        now.  You can change (highlight-tail-reload) call to

;;        (highlight-tail-mode) in you .emacs file, but it's not

;;        necessary.  Thanks to Daniel Engeler for reporting.

;;

;; * 1.3.8 (17 Nov 2006)

;;      + Overlays that had fade out are now reused later instead of

;;        being left to the garbage collector.  Thanks to Markus

;;        Triska for reporting.

;; 

;; * 1.3.7 (14 Nov 2006)

;;      + Made `highlight-tail-reload' function interactive, so it can

;;        be evaluated with `execute-extended-command' (M-x).  Thanks

;;        to Markus Triska for reporting.

;;      + No line exceeds 79 column, better coding-style in few places.

;;

;; * 1.3.6 (11 Nov 2006)

;;      + No line exceeds 80 column.

;; 

;; * 1.3.5 (11 Nov 2006)

;;      + Do not activate if run under terminal (because it leads to

;;        errors).

;;

;; * 1.3.4 (01 Nov 2006)

;;      + Use `color-values' function instead of

;;        `color-name-rgb-alist' variable so that highlight-tail

;;        should work in Emacs on Windows and is simpler.  Thanks to

;;        Kevin Rodgers for reporting.

;;      + cl is now used only at compile time.  Thanks to Richard

;;        Stallman for reporting.

;;

;; * 1.3.3 (30 Oct 2006)

;;      + Added (require 'cl).  Thanks to Arnaldo Mandel and Kevin

;;        Rodgers for reporting.

;;      + Many less warnings on compilation.  Thanks to Kevin Rodgers.

;; 

;; * 1.3.2 (29 Oct 2006)

;;      + Fixed the way the number of steps between every two colors

;;        are computed.

;;      + Added a test on `highlight-tail-colors' to enforce its

;;        correctness.

;; 

;; * 1.3.1 (28 Oct 2006)

;;      + Minor documentation corrections.

;;

;; * 1.3 (28 Oct 2006)

;;      + Fading out to the *current* background color.  You no longer

;;        need to specify background color (the one at 100%) in

;;        `highlight-tail-colors' variable.  highlight-tail-mode will

;;        discover it on its own and is vigilant about future changes:

;;        3sec. timer holds an eye on it.

;;      + What's more: if there is some overlay or text property that

;;        change background color, highlight-tail will discover it and

;;        fade out to this color.

;;      + Some corrections in the documentation and comments.

;;      

;; * 1.2 (25 Aug 2006)

;;      + Now works with org-mode and orgtbl-mode.  Thanks to William

;;        Xu for reporting.

;;      + You can now choose a color by name in XEmacs.

;;      + XEmacs related bug with itimer fixed.

;;      + Fixed some typos and errors in the documentation.

;; 

;; * 1.1.1

;;      + Updated information about web page (up; new address (the old

;;        one works though)).

;;

;; * 1.1

;;      + Fixed serious bug in the documentation (in installation section).

;;        Thanks to Mathias Dahl for reporting.

;;

;; * 1.0

;;      + Nothing new, I just think 0.9.3 is so stable to honor it with

;;        round version number.

;;

;; * 0.9.3

;;      + Added information about highlight-tail.el web page.

;;      + Front highlighting removed completely... It wasn't nice and

;;        there were too many troubles with it.

;;      + highlight-tail.el taken under subversion control

;;

;; * 0.9.2

;;      + Colors can be passed with uppercase letters in.

;;      + If you choose a color, that doesn't exist in

;;        `color-name-rgb-alist', mode will call out an error instead of

;;        running "illegible" debugger ;).

;;      + The variable (i)timer-list is now cleaned from old timers on

;;        `highlight-tail-reload' call.

;;      + All overlays are deleted on `highlight-tail-reload' call.

;;      + Several minor bugs fixed.

;;

;; * 0.9.1

;;      + Region kept alive in XEmacs, when going to the end of line.

;;        Thanks to Nate Drake for reporting this bug.

;;      + Functions broken into smaller parts.

;;

;; * 0.9

;;      + Now under GPL.

;;      + Changed start, middle and end colors to a list of indefinable length

;;      + Added front and constant highlighting options.

;;      + Changed lists to hashes and vectors for speed (I don't really know

;;        did this help.)

;;

;; * 0.8

;;      + Eats a lot less of CPU cycles than before.  Thanks to

;;        Phillip Lord for some advices how to achieve it.

;;      + Works good under XEmacs.

;;      + Added middle color option

;;

;; * 0.7

;;      + First public version.



;;; TODO:



;; * Maybe some optimizations for speed.

;; * Make some way for highlight-tail to perfectly work with modes

;;   that rebind "normal" keys (like c-electric-mode).  Unfortunately

;;   this could be very hard or impossible.  Right now there is no

;;   highlighting on rebinded keys (like: colons, parens etc. in

;;   c-electric-mode).

;;

;; You are free to mail me, what would you like to see in this

;; mode, or maybe you have idea for new emacs eye-candy minor-mode.

;; necui-emahte@nic-nac-project.net



;;; Variables:



(defvar highlight-tail-colors '(("#d8971d" . 0)

                                ("#ddee1f" . 20))

  "*List of colors through which the fading will last.

The argument for every color is a percent at which this color

will have greatest intensity.



For instance:

'((\"#c1e156\" . 0)

  (\"#b8ff07\" . 25)

  (\"#00c377\" . 60)))



Verbal color names are possible.  These names are case independent.



For instance:

'((\"Black\" . 0)

  (\"white\" . 100))")



(defvar highlight-tail-steps 80

  "*Number of fading out steps.  The color will be changed that many

times.  One change (step) every `highlight-tail-timer'.")



(defvar highlight-tail-timer 0.04

  "*Number of seconds between fade out steps.

It can be sth like this: 0.2 or maybe better 0.02")



(defvar highlight-tail-const-width 10

  "*Number of chars to highlight when constant highlighting.

Makes sense only when `highlight-tail-posterior-type' is 'const.")



(defvar highlight-tail-posterior-type t

  "*Type of highlighting.

t      - normal highlighting

const  - highlight constant width of characters

         (see `highlight-tail-const-width')")



(defvar highlight-tail-stepsperfade-vector nil

  "Vector of fade steps number between `highlight-tail-colors-with-100' elems.



Please do not change this variable.")



(defvar highlight-tail-colors-with-100 nil

  "`highlight-tail-colors' that always end on 100%.



If `highlight-tail-colors' ends at 100% then this variable is

identical.  If not, then this variable is `highlight-tail-colors' +

'(null . 100).



Please do not change this variable.")



(defvar highlight-tail-colors-fade-list nil

  "Association list containing list of colors for fading out.



This variable is a association list, which means it contains lists

like (key . value).



Key could be: 

- 'start

- 'default

- hex value



And value is a list of colors for fading out in hex format.



- 'start list



This list of colors is used to build the others. It contains fade

table from the first color to the last one (from `highlight-tail-colors'

variable).



- 'default list



This list is used to fade out to the default background color. By

default background color I mean that there are no overlays nor text

properties that change the background color at the point of fading

out. This list is a join of start list and a fade table from the last

color to the background one.



- hex value list



Such list is always generated on runtime when highlight-tail discovers

a new background color. This list is a join of start list and a fade

table from the last color to the discovered one.



This list is automatically computed.

Please do not change this variable.")



(defvar highlight-tail-overlays-hash nil

  "Hash of actually being displayed highlight-tail's overlays.



Hash is indexed by overlays.  Values contains:

 - background color hex or 'default

 - face number (the step of fading out)



This hash is changed on every `highlight-tail-fade-out-step'.



Please do not change this variable.")



(defvar highlight-tail-const-overlays-list nil

  "Vector of lists containing overlays and their step at fading out.



Only when 'const highlighting is on.



The vector consists of lists like this one: (overlay . step).  Step is

a number from which parser will build a face name.  For example color

number 5 will amount to \"highlight-tail-face-default-5\".  The list is

changed on every `highlight-tail-fade-out-step' call.



Please do not change this variable.")



(defvar highlight-tail-update-const-overlays-to-this-list nil

  "A vector of numbers that const highlighting cannot exceed.



Every number int this vector is in correlation with element at the

same position in `highlight-tail-const-overlays-list'.  Such number

means that related element fading intensity cannot be larger than this

number.



Both this and `highlight-tail-const-overlays-list' variables are

somewhat in reverse order.  The element at 0 index is the element that

stands right next to the cursor on inserting.



Please do not change this variable.")



(defvar highlight-tail-deleted-overlays-list nil

  "Deleted (detached in Xemacs) overlays to be reused.



Please do not change this variable.")



(defvar highlight-tail-default-background-color nil

  "The actual background color computed at every

`highlight-tail-reload' and checked for being up-to-date at every 3

seconds.



Please do not change this variable.")



(defvar highlight-tail-nonhtfaces-bgcolors nil

  "List with car=face-name and cadr=background-color-hex

It is computed on runtime for every not known face.



Please do not change this variable.")



(defvar highlight-tail-face-max nil

  "Number of \"max face\" (equal to `highlight-tail-steps').

Max face means the color completely faded out.



Please do not change this variable.")



(defvar highlight-tail-fading-timer nil

  "Timer that executes color changing.

Every tick of this timers will change colors of overlays.

This variable is attributed in the program.



Please do not change this variable.")



(defvar highlight-tail-defaultbgcolor-timer nil

  "Timer that executes `highlight-tail-check-if-defaultbgcolor-changed'



One tick every 3 seconds.")



(defvar highlight-tail-mode nil

  "*The highlight-tail-mode state.")



;;; Code:



(eval-when-compile

  (require 'cl))



(if (featurep 'xemacs)

    (progn

      (defalias 'highlight-tail-make-overlay 'make-extent)

      (defalias 'highlight-tail-overlays-at 'extents-at)

      (defalias 'highlight-tail-overlay-put 'set-extent-property)

      (defalias 'highlight-tail-overlay-get 'extent-property)

      (defalias 'highlight-tail-delete-overlay 'detach-extent)

      (defalias 'highlight-tail-move-overlay 'set-extent-endpoints)

      (defalias 'highlight-tail-overlay-end 'extent-end-position)

      (defalias 'highlight-tail-overlay-start 'extent-start-position)

      (defalias 'highlight-tail-overlayp 'extent-live-p))

  (progn

    (defalias 'highlight-tail-make-overlay 'make-overlay)

    (defalias 'highlight-tail-overlays-at 'overlays-at)

    (defalias 'highlight-tail-overlay-put 'overlay-put)

    (defalias 'highlight-tail-overlay-get 'overlay-get)

    (defalias 'highlight-tail-delete-overlay 'delete-overlay)

    (defalias 'highlight-tail-move-overlay 'move-overlay)

    (defalias 'highlight-tail-overlay-end 'overlay-end)

    (defalias 'highlight-tail-overlay-start 'overlay-start)

    (defalias 'highlight-tail-overlayp 'overlayp)))



(defsubst highlight-tail-make-new-overlays ()

  "Create or update overlays.

This is called by `highlight-tail-post-command'."

  (when highlight-tail-posterior-type

    (if (eq highlight-tail-posterior-type 'const)

        (progn

          ;; first run - make overlays

          (unless highlight-tail-const-overlays-list

            (highlight-tail-make-const-overlays-list))

          ;; done

          (highlight-tail-update-const-overlays-list))

      ;; not const highlighting - make new overlay in the current place

      ;; with face-value of 1 (brightest)

      (highlight-tail-make-new-overlay))))



(defun highlight-tail-post-command ()

  "Check for the last command and decide to refresh highlighting or not."

  (when (and highlight-tail-mode

             (or (equal this-command 'self-insert-command)

                 (equal this-command 'org-self-insert-command)

                 (equal this-command 'orgtbl-self-insert-command)))

    (highlight-tail-make-new-overlays)))



(defun highlight-tail-check-if-defaultbgcolor-changed ()

  "Check if default background color changed.



Check the background color, compare it with the last remembered and

eventually change the last remembered to the current one."

  (let ((background-color-name

         (if (featurep 'xemacs)

             (face-background-name 'default)

           (cdr (assoc 'background-color (frame-parameters))))))

    (when (not (eq background-color-name

                   highlight-tail-default-background-color))

      (setq highlight-tail-default-background-color background-color-name)

      (highlight-tail-add-colors-fade-table 'default)

      (highlight-tail-make-faces

       (highlight-tail-get-colors-fade-table-with-key 'default)))))



(defsubst highlight-tail-get-face-background (face)

  "Get FACE's background"

  (if (featurep 'xemacs)

      (face-background-name face)

    (face-attribute face :background)))



(defsubst highlight-tail-hex-from-RGB (red green blue)

  "Build a color like #00FF00 from given RED, GREEN and BLUE.

For example: 0 255 0 will result in #00FF00."

  (format "#%02X%02X%02X" (round red) (round green) (round blue)))



(defsubst highlight-tail-color-in-hex-format (color)

  "Find out if COLOR is in hex format or not."

  (string-equal (if (featurep 'xemacs)

                    (replace-in-string color

                                       "#[0-9a-fA-F]\\{6\\}"

                                       "")

                  (replace-regexp-in-string "#[0-9a-fA-F]\\{6\\}"

                                            ""

                                            color))

                ""))



(defsubst highlight-tail-hex-from-colorname (color)

  "Build a color like #00FF00 from \"green\" or return COLOR if already in hex"

  (let ((return-color

         (if (highlight-tail-color-in-hex-format color)

             color

           (let ((highlight-tail-color-from-system-list

                  (highlight-tail-get-RGB-from-systemlist color)))

             (highlight-tail-hex-from-RGB

              (nth 1 highlight-tail-color-from-system-list)

              (nth 2 highlight-tail-color-from-system-list)

              (nth 3 highlight-tail-color-from-system-list))))))

    return-color))



(defun highlight-tail-get-bgcolor-hex (point)

  "Get the background color of point.



Do not take highlight-tail's overlays into consideration.  This means

that if there is ht's overlay at at the top then return 'default"

  (let ((point-face (get-char-property point 'face))

        point-face-from-cache

        point-face-bgcolor

        point-face-bgcolor-hex)

    (if point-face

        (progn

          (when (listp point-face) (setq point-face (car point-face)))

          ;; This is weird because for howm-reminder-today-face, the

          ;; (get-char-property) function returns a list:

          ;; (howm-reminder-today-face), so it's needed to get car of

          ;; it...

          (when (stringp point-face) (setq point-face (intern point-face)))

          ;; This is weird because for faces used by ediff, the

          ;; (get-char-property) function returns a string:

          ;; "xxx-face", so it's needed to intern it...

          (setq point-face-from-cache

                (assoc point-face highlight-tail-nonhtfaces-bgcolors))

          (if point-face-from-cache

              (setq point-face-bgcolor-hex (cdr point-face-from-cache))

            (setq point-face-bgcolor

                  (highlight-tail-get-face-background point-face))

            (when (or (eq point-face-bgcolor nil)

                      (eq point-face-bgcolor 'unspecified))

              (setq point-face-bgcolor 'default))))

      (setq point-face-bgcolor 'default))

    (when (not point-face-bgcolor-hex)  ; not read from cache

      (if (eq point-face-bgcolor 'default)

          (setq point-face-bgcolor-hex 'default)

        ;; else

        (setq point-face-bgcolor-hex

              (highlight-tail-hex-from-colorname point-face-bgcolor))

        (setq highlight-tail-nonhtfaces-bgcolors

              (cons (cons point-face point-face-bgcolor-hex)

                    highlight-tail-nonhtfaces-bgcolors))

        (highlight-tail-add-colors-fade-table point-face-bgcolor-hex)

        (highlight-tail-make-faces

         (highlight-tail-get-colors-fade-table-with-key

          point-face-bgcolor-hex))))

    ;; return value

    point-face-bgcolor-hex))



(defun highlight-tail-make-new-overlay ()

  "Make new highlight in the current point."

  (let* ((end-point (point))

         (start-point (1- end-point))

         (point-face-bgcolor-hex nil))

    ;; remove any highlight-tail's overlays at point

    (let ((overlays-at-start-point (highlight-tail-overlays-at start-point))

          highlight-tail-overlay)

      (mapcar '(lambda (overlay)

                 (when (highlight-tail-overlay-get overlay 'highlight-tail)

                   (setq highlight-tail-overlay overlay)))

              overlays-at-start-point)

      (when highlight-tail-overlay

        (add-to-list 'highlight-tail-deleted-overlays-list

                     highlight-tail-overlay)

        (remhash highlight-tail-overlay highlight-tail-overlays-hash)

        (highlight-tail-delete-overlay highlight-tail-overlay)))

    ;; do we need to fade out to default color or any other

    (setq point-face-bgcolor-hex (highlight-tail-get-bgcolor-hex start-point))

    ;; add the overlay with good ending color

    (let (highlight-tail-overlay)

      (if (> (length highlight-tail-deleted-overlays-list) 0)

          ;; get overlay from list of deleted overlays

          (progn

            (setq highlight-tail-overlay

                  (highlight-tail-move-overlay

                   (car highlight-tail-deleted-overlays-list)

                   start-point end-point

                   (current-buffer))) ; Xemacs needs it (or will highlight in

                                        ; other buffer)

            (setq highlight-tail-deleted-overlays-list

                  (cdr highlight-tail-deleted-overlays-list)))

        ;; make new overlay

        (setq highlight-tail-overlay

              (highlight-tail-make-overlay start-point end-point))

        (highlight-tail-overlay-put

         highlight-tail-overlay 'evaporate t)

        (highlight-tail-overlay-put

         highlight-tail-overlay 'highlight-tail t))

      (puthash highlight-tail-overlay

               (list point-face-bgcolor-hex

                     1)             ; first step in fading-out

               highlight-tail-overlays-hash)

      (highlight-tail-overlay-put

       highlight-tail-overlay 'face

       (intern

        (concat "highlight-tail-face-"

                (format "%s" point-face-bgcolor-hex)

                "-1")))))) ; first step in fading out



(defun highlight-tail-make-const-overlays-list ()

  "Make constant overlays list, that will be later operated on.

\(by `highlight-tail-fade-out-step')



It is executed only when `highlight-tail-posterior-type' is 'const."

  ;; we are going from end

  (let ((iterator (1- highlight-tail-const-width))

        ;; difference between neighbouring elements of

        ;; `highlight-tail-update-const-overlays-to-this-list'

        (fading-intensity-step (/ highlight-tail-face-max

                                  (float highlight-tail-const-width)))

        (fading-intensity highlight-tail-face-max))

    (setq highlight-tail-const-overlays-list

          (make-vector highlight-tail-const-width nil))

    (setq highlight-tail-update-const-overlays-to-this-list

          (make-vector highlight-tail-const-width nil))

    (while (>= iterator 0)

      ;; make overlay in array of const overlays

      (aset highlight-tail-const-overlays-list

            iterator

            (cons (highlight-tail-make-overlay (point) (point))

                  highlight-tail-face-max)) ; last step in fading out



      ;; decrease the fading intensity by constant factor

      (setq fading-intensity (- fading-intensity fading-intensity-step))

      (aset highlight-tail-update-const-overlays-to-this-list

            iterator

            (round fading-intensity))

      (let ((highlight-tail-overlay

             (car (elt highlight-tail-const-overlays-list iterator))))

        (highlight-tail-overlay-put highlight-tail-overlay 'highlight-tail t)

        (highlight-tail-overlay-put

         highlight-tail-overlay

         'face

         (intern

          ;; detecting bgcolor will be done later

          (concat "highlight-tail-face-default-"

                  ;; last step in fading out

                  (number-to-string highlight-tail-face-max)))))

      (setq iterator (1- iterator)))))



(defun highlight-tail-update-const-overlays-list ()

  "Update constant overlays list (colors, positions etc.)



It only occurs when `highlight-tail-posterior-type' is 'const."

  (let ((iterator 0))

    ;; iterate through `highlight-tail-const-overlays-list'

    (while (< iterator (length highlight-tail-const-overlays-list))

      (let ((overlay-point (- (point) iterator)))

        (if (< (- (point) overlay-point) (current-column))

            (highlight-tail-move-overlay

             (car (elt highlight-tail-const-overlays-list iterator))

             overlay-point (1- overlay-point)

             (current-buffer))

          ;; move to current-buffer to not blink in other buffer

          ;; it is good for minibuffer

          (highlight-tail-move-overlay

           (car (elt highlight-tail-const-overlays-list iterator))

           1 1

           (current-buffer))))

      ;; change the intesity of fading

      (let ((new-value

             (round (- (cdr (elt highlight-tail-const-overlays-list iterator))

                       (* (- highlight-tail-face-max

                             (elt

                              highlight-tail-update-const-overlays-to-this-list

                              iterator))

                          0.15)))))

        ;; new-value = (curface)-(0.15*(maxface-updateto))

        (when (< new-value (elt

                            highlight-tail-update-const-overlays-to-this-list

                            iterator))

          (setq new-value (elt

                           highlight-tail-update-const-overlays-to-this-list

                           iterator)))

        (setcdr (elt highlight-tail-const-overlays-list iterator) new-value))

      (setq iterator (1+ iterator)))))



(defun highlight-tail-fade-out-step ()

  "Go through all overlays and make sth with them.

Such as compute new faces, purge old overlays etc.



This is called every `highlight-tail-timer' amount of time."

  (sit-for 0)

  ;; if mode had been just disabled - delete all overlays

  ;; and cancel timers

  (if (not highlight-tail-mode)

      (highlight-tail-tide-up))



  ;; if there are some overlays

  (when highlight-tail-posterior-type

    (if (eq highlight-tail-posterior-type 'const)

        ;; if const highlighting

        (when highlight-tail-const-overlays-list

          (let ((iterator 0))

            ;; iterate through elements of `highlight-tail-const-overlays-list'

            (while (< iterator highlight-tail-const-width)

              (let ((cur-face-number

                     (cdr (elt highlight-tail-const-overlays-list iterator))))

                ;; number < `highlight-tail-face-max'

                (if (not (= cur-face-number

                            highlight-tail-face-max))

                    (progn

                      (setq cur-face-number (1+ cur-face-number))

                      (setcdr (elt highlight-tail-const-overlays-list iterator)

                              cur-face-number)

                      (highlight-tail-overlay-put

                       (car (elt highlight-tail-const-overlays-list iterator))

                       'face

                       (intern

                        (concat "highlight-tail-face-default-"

                                (number-to-string cur-face-number)))))

                  (highlight-tail-move-overlay

                   (car (elt highlight-tail-const-overlays-list iterator))

                   1 1

                   (current-buffer)))



                (setq iterator (1+ iterator))))))

      ;; if not const-highlighting

      (when (> (hash-table-count highlight-tail-overlays-hash) 0)

        (maphash 'highlight-tail-fade-out-step-process-overlay

                 highlight-tail-overlays-hash)))))

      

(defun highlight-tail-fade-out-step-process-overlay (key value)

  "Process every KEY in `highlight-tail-overlays-hash'."

  (let ((cur-face-number (car (last value))))

    (if (< cur-face-number highlight-tail-face-max)

        (progn

          (setq cur-face-number (1+ cur-face-number))

          (setcar (last value) cur-face-number)

          ;; fade out to the background color

          (highlight-tail-overlay-put

           key

           'face

           (intern

            (concat "highlight-tail-face-" (format "%s" (car value)) "-"

                    (number-to-string cur-face-number)))))

      ;; end of highlighting here

      (add-to-list 'highlight-tail-deleted-overlays-list key)

      (highlight-tail-delete-overlay key)

      (remhash key highlight-tail-overlays-hash))))





(defmacro highlight-tail-get-colors-fade-table-with-key (for-what)

  (list 'assoc for-what 'highlight-tail-colors-fade-list))



(defun highlight-tail-add-colors-fade-table (for-what)

  "Compute list of colors that will smoothly change from one to another.

The list is stored in variable `highlight-tail-colors-fade-list'.



If FOR-WHAT is 'start then the list is computed for all colors from

`highlight-tail-colors'.  If FOR-WHAT is a hex color or 'default then

the list is a join of 'start fade-table and a fade-table computed from

the last color in `highlight-tail-colors' to the given one.



Where 'default is `highlight-tail-default-background-color'"

  (let ((colors-fade-elem (highlight-tail-get-colors-fade-table-with-key

                           for-what)))

    ;; preper colors-fade-elem

    (if colors-fade-elem

        (setf (cadr colors-fade-elem) nil)

      ;; else (no colors-fade-elem in list)

      (add-to-list 'highlight-tail-colors-fade-list

                   (list for-what nil))

      (setq colors-fade-elem (highlight-tail-get-colors-fade-table-with-key

                              for-what)))

    ;; compute the list

    (let* ((iter 0)

           (for-what-is-start (equal for-what 'start))

           (for-what (if (eq for-what 'default)

                         ;; 'default is not a color, so we need to

                         ;; replace it with default's hex

                         highlight-tail-default-background-color

                       for-what))

           (for-what-colors-list

            (let ((temp (mapcar         ; *copy* elements of

                                        ; `highlight-tail-colors-with-100'

                         '(lambda (elem) elem) ; to temporary variable

                         highlight-tail-colors-with-100)))

              (setcar (last temp) (cons for-what 100))

              temp))

           (spfv-length (length highlight-tail-stepsperfade-vector))

           result)

      (if for-what-is-start

          (while (< iter (1- spfv-length))

            (setq result

                  (append result

                          (highlight-tail-find-colors-fade-table-part

                           (car (nth iter for-what-colors-list))

                           (car (nth (1+ iter) for-what-colors-list))

                           (elt highlight-tail-stepsperfade-vector iter))))

            (setq iter (1+ iter)))

        ;; if (not for-what-is-start) copy starting elements from

        ;; 'start list

        (setq result

              (cadr (highlight-tail-get-colors-fade-table-with-key 'start)))

        (setq result

              (append result

                      (highlight-tail-find-colors-fade-table-part

                       (car (nth (1- spfv-length) for-what-colors-list))

                       (car (nth spfv-length for-what-colors-list))

                       (elt highlight-tail-stepsperfade-vector

                            (1- spfv-length))))))

      (setf (cadr colors-fade-elem) result))))



(defun highlight-tail-find-colors-fade-table-part (color-from

                                                   color-to

                                                   steps-count)

  "Create a list of smoothly changed colors.

From COLOR-FROM to COLOR-TO             ; STEPS-COUNT length."

  (let (color-from-red color-from-green color-from-blue  

                       color-to-red color-to-green color-to-blue    

                       color-temp-red color-temp-green color-temp-blue       

                       color-step-red ; the color is smoothly changing

                                        ; we'll calculate a value

                       color-step-green ; that will be added to COLOR-FROM

                                        ; at every single step,

                       color-step-blue  ; multiplied by current step

                                        ; number of course.

                       ;; will values be positive or negative

                       color-step-red-positive color-step-green-positive

                       color-step-blue-positive

                       ;; differences between FROM and TO values

                       color-red-difference color-green-difference

                       color-blue-difference

                       result-list

                       (step 1))

    ;; Get red, green and blue intensities from give colors.

    (if (highlight-tail-color-in-hex-format color-from)

        (setq

         color-from-red (string-to-number (substring color-from 1 3) 16)

         color-from-green (string-to-number (substring color-from 3 5) 16)

         color-from-blue (string-to-number (substring color-from 5 7) 16))

      (let ((temp-color (highlight-tail-get-RGB-from-systemlist color-from)))

        (setq color-from-red (nth 1 temp-color))

        (setq color-from-green (nth 2 temp-color))

        (setq color-from-blue (nth 3 temp-color))))

    (if (highlight-tail-color-in-hex-format color-to)

        (setq color-to-red (string-to-number (substring color-to 1 3) 16)

              color-to-green (string-to-number (substring color-to 3 5) 16)

              color-to-blue (string-to-number (substring color-to 5 7) 16))

      (let ((temp-color (highlight-tail-get-RGB-from-systemlist color-to)))

        (setq color-to-red (nth 1 temp-color))

        (setq color-to-green (nth 2 temp-color))

        (setq color-to-blue (nth 3 temp-color))))



    (setq ;; compute difference of COLOR-FROM and COLOR-TO

     color-red-difference (abs (- color-from-red color-to-red))

     color-green-difference (abs (- color-from-green color-to-green))

     color-blue-difference (abs (- color-from-blue color-to-blue))

     ;; compute what every single step of fading will change

     color-step-red (/ (float color-red-difference) steps-count)

     color-step-green (/ (float color-green-difference) steps-count)

     color-step-blue (/ (float color-blue-difference) steps-count)

     ;; check if step values should be positive or negative

     color-step-red-positive (>= color-to-red color-from-red)

     color-step-green-positive (>= color-to-green color-from-green)

     color-step-blue-positive (>= color-to-blue color-from-blue))

    

    ;; if desirable - make values negative

    (if (not color-step-red-positive) (setq color-step-red

                                            (* color-step-red -1)))

    (if (not color-step-green-positive) (setq color-step-green

                                              (* color-step-green -1)))

    (if (not color-step-blue-positive) (setq color-step-blue

                                             (* color-step-blue -1)))



    ;; now compute the list ;;

    ;; we have colors in red, green and blue values;

    (setq color-temp-red color-to-red

          color-temp-green color-to-green

          color-temp-blue color-to-blue)

    (while (<= step steps-count)

      (setq color-temp-red (- color-temp-red color-step-red)

            color-temp-green (- color-temp-green color-step-green)

            color-temp-blue (- color-temp-blue color-step-blue)

            result-list

            (cons (highlight-tail-hex-from-RGB color-temp-red color-temp-green

                                               color-temp-blue)

                  result-list)

            step (1+ step)))

    result-list))



(defun highlight-tail-get-RGB-from-systemlist (color-name)

  "Find a COLOR-NAME by using `color-values' (Emacs) or

`color-rgb-components' (Xemacs)."

  (let ((color-name (downcase color-name))

        colors-list-to-return)

    (setq colors-list-to-return

          (if (featurep 'xemacs)

              (color-rgb-components (make-color-specifier color-name))

            (color-values color-name)))

    ;; color intensities take two bits, and we want them to take one

    (setq colors-list-to-return

          (mapcar '(lambda (elem) (round (* (/ elem 65535.0) 255)))

                  colors-list-to-return))

    (add-to-list 'colors-list-to-return color-name)

    colors-list-to-return))



(defun highlight-tail-make-faces (colors-fade-table-with-key)

  "Make faces from list of colors.



Faces will be named: highlight-tail-face-X-Y, where X is a color name

from highlight-tail-colors-fade-list and Y is a number from 1 to

length of colors-fade-table from COLORS-FADE-TABLE-WITH-KEY"

  (let ((face-name-color-part (format "%s" (car colors-fade-table-with-key)))

        (colors-list (cadr colors-fade-table-with-key))

        (count 1)

        face-name)

    (while (<= count (length colors-list))

      (setq face-name

            (intern (concat "highlight-tail-face-"

                            face-name-color-part

                            "-"

                            (number-to-string count))))

      (make-face face-name)

      (set-face-background face-name (nth (1- count) colors-list))

      (setq count (1+ count)))))



(defun highlight-tail-tide-up ()

  "Delete all overlays, cancel timers, and so on (clean up)..."

  (let ((count 0))

    (when (hash-table-p highlight-tail-overlays-hash)

      (maphash 'highlight-tail-overlays-hash-delete-overlay-map

               highlight-tail-overlays-hash)

      (clrhash highlight-tail-overlays-hash))

    (setq highlight-tail-deleted-overlays-list nil)

    (setq count 0)

    (while (< count (length highlight-tail-const-overlays-list))

      (highlight-tail-delete-overlay

       (car (elt highlight-tail-const-overlays-list count)))

      (setq count (1+ count)))

    (setq highlight-tail-colors-fade-list nil

          highlight-tail-stepsperfade-vector nil

          highlight-tail-overlays-hash nil

          highlight-tail-default-background-color nil

          highlight-tail-nonhtfaces-bgcolors nil

          highlight-tail-const-overlays-list nil

          highlight-tail-update-const-overlays-to-this-list nil

          highlight-tail-face-max nil)

    (highlight-tail-cancel-timers)))



(defun highlight-tail-cancel-timers ()

  "Cancel timers"

  (if (featurep 'xemacs)

      (when (itimerp highlight-tail-fading-timer)

        (delete-itimer highlight-tail-fading-timer)

        (delete-itimer highlight-tail-defaultbgcolor-timer))

    (when (timerp highlight-tail-fading-timer)

      (cancel-timer highlight-tail-fading-timer)

      (cancel-timer highlight-tail-defaultbgcolor-timer))))



(defsubst highlight-tail-overlays-hash-delete-overlay-map (key value)

  "Deletes the overlay from VALUE."

  (if (highlight-tail-overlayp key)

      (highlight-tail-delete-overlay key)))



(defun highlight-tail-reload ()

  (interactive)

  (highlight-tail-mode 1))



(defun highlight-tail-start ()

  "Recreate color-fade-tables, faces, hook, turn on `highlight-tail-mode', etc.

Run it, when you've made changes to some highlight-tail-mode variables."

  ;; check that `highlight-tail-colors' variable has been defined

  ;; correctly by the user

  (let ((previous-elem-value -1)   ;first elem should be 0 and 0>-1 :)

        (httmp-signal-error-function

         '(lambda (elem explanation)

            (error

             (format "%s element in `highlight-tail-colors' is wrong! %s"

                     elem explanation)))))

    ;; Check that first element is at 0%.

    (when (not (= (cdar highlight-tail-colors) 0))

      (apply httmp-signal-error-function

             (format "First (%s)" (car highlight-tail-colors))

             (list "Value should be zero.")))

    ;; Check that every element is greater than previous one.

    (mapcar '(lambda (elem)

               (if (<= (cdr elem) previous-elem-value)

                   (apply httmp-signal-error-function

                          elem

                          (list

                           "Value should be greater than previous element."))

                 (setq previous-elem-value (cdr elem))))

            highlight-tail-colors)

    ;; Check that last element is <= 100%.

    (when (not (<= (cdar (last highlight-tail-colors)) 100))

      (apply httmp-signal-error-function

             (format "Last (%s)" (car (last highlight-tail-colors)))

             (list "Value should be less than or equal to 100."))))

  

  ;; if there is a color name in `highlight-tail-colors', that doesn't

  ;; exist in systemlists - call out an error

  (when (member

         nil

         ;; create a list of "t"s, color lists '(red 255 0 0) and nils

         ;; in place of colors that doesn't exist

         (mapcar

          '(lambda (elem)

             (let ((color-name (car elem)))

               (if (highlight-tail-color-in-hex-format color-name)

                   ;; does not need to be on system list

                   t

                 ;; try to get from system list

                 (if (featurep 'xemacs)

                     (color-rgb-components (make-color-specifier color-name))

                   (color-values color-name))))) highlight-tail-colors))

    (error "Some color doesn't exist"))



  (highlight-tail-tide-up)



  (setq highlight-tail-overlays-hash (make-hash-table))



  (let* ((background-color-name (if (featurep 'xemacs)

                                    (face-background-name 'default)

                                  (cdr (assoc 'background-color

                                              (frame-parameters)))))

         (background-color-hex (highlight-tail-hex-from-colorname

                                background-color-name)))

    (setq highlight-tail-default-background-color background-color-name))

  

  (setq highlight-tail-colors-with-100

        (if (= (cdr (nth (1- (length highlight-tail-colors))

                         highlight-tail-colors))

               100)

            highlight-tail-colors

          (append highlight-tail-colors (list '(null . 100)))))



  ;; compute the `highlight-tail-stepsperfade-vector'

  (let* (iter

         (colors-with-100-length (length highlight-tail-colors-with-100))

         (percents-vector (make-vector colors-with-100-length nil))

         ;; below: scaled to `highlight-tail-steps'

         (percents-vector-scaled (make-vector colors-with-100-length nil)))

    (setq percents-vector (mapcar '(lambda (elem)

                                     (cdr elem))

                                  highlight-tail-colors-with-100))

    (setq highlight-tail-stepsperfade-vector

          (make-vector (1- colors-with-100-length) nil))

    (setq iter 0)

    (while (< iter colors-with-100-length)

      (setf (elt percents-vector-scaled iter)

            ;; (elem%)*steps

            (round (* (/ (float (nth iter percents-vector))

                         100)

                      highlight-tail-steps)))

      (setq iter (1+ iter)))

    (setq iter 1)

    (while (< iter colors-with-100-length)

      (setf (elt highlight-tail-stepsperfade-vector (1- iter))

            (- (elt percents-vector-scaled iter)

               (elt percents-vector-scaled (1- iter))))

      (setq iter (1+ iter))))



  (highlight-tail-add-colors-fade-table 'start)

  (highlight-tail-add-colors-fade-table 'default)

  (setq highlight-tail-face-max highlight-tail-steps)

  (highlight-tail-make-faces

   (highlight-tail-get-colors-fade-table-with-key 'default))

  

  (setq highlight-tail-fading-timer

        (if (featurep 'xemacs)

            (start-itimer "highlight-tail-fade-out-step"

                          'highlight-tail-fade-out-step

                          highlight-tail-timer

                          highlight-tail-timer)

          (run-at-time nil highlight-tail-timer

                       'highlight-tail-fade-out-step)))

  (setq highlight-tail-defaultbgcolor-timer

        (if (featurep 'xemacs)

            (start-itimer "highlight-tail-check-if-defaultbgcolor-changed"

                          'highlight-tail-check-if-defaultbgcolor-changed

                          3

                          3)

          (run-at-time nil 3

                       'highlight-tail-check-if-defaultbgcolor-changed)))

  (add-hook 'post-command-hook 'highlight-tail-post-command))



(defun highlight-tail-mode (&optional arg)

  "Draw a \"tail\" while you're typing.



This minor-mode draws a tail in real time, when you write.  It

changes the background color of some last typed characters and

smoothly fade them out.



If ARG is 0 or less than zero then the mode will be disabled.

If ARG is nil then the mode will be switched.

If ARG is greater than zero then this mode will be turned on."

  (interactive "P")

  (if (equal window-system nil)

      (progn

        (setq highlight-tail-mode nil))

    (setq highlight-tail-mode

          (if (null arg) (not highlight-tail-mode)

            (> (prefix-numeric-value arg) 0))))

  (add-to-list 'minor-mode-alist '(highlight-tail-mode " ht"))

  (if highlight-tail-mode

      (progn

        (highlight-tail-start)

        (message "Highlight tail mode enabled"))

    (progn

      (highlight-tail-tide-up)

      (if (equal window-system nil)

          (message

           "Highlight-tail-mode will not start because not running under \

window system.")

        (message "Highlight tail mode disabled")))))



(provide 'highlight-tail)



;;; highlight-tail.el ends here
