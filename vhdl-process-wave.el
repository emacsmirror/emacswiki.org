;;; whdl-process-wave.el --- processes waveform saved of modelsim

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author:  wandad guscheh <wandad.guscheh@fh-hagenberg.at>
;; Keywords: vhdl

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file can be used with saved modelsim wave files. Just invoke
;; vhdl-process-wave command while in saved buffer, save the buffer,
;; reload waveform into modelsim. Waveform will look much better now.

;; For customization have a look at
;; M-x customize-group whdl-process-wave-general
;; M-x customize-group whdl-process-wave-project

;; It is possible to define rules how to convert the waveform on
;; project basis (delete this signal, put this signal on top, let
;; signal xyz have radix decimal, make signal zyx purple etc.)
;;
;; See customization description for more information.

;;; Code:

(defgroup whdl-process-wave-general nil "Settings how long the divider is, which part of the WF should be seen etc." :group 'whdl)

(defgroup whdl-process-wave-project nil "Here you can set color etc. for as many projects as you like." :group 'whdl)

(defcustom whdl-length-of-divider 35
  "*Divider will be at maximum this length. If divider would be longer, the first part of the string will be cut off, because most often divider strings are equal at the beginnning."
  :type 'integer
  :group 'whdl-process-wave-general)

(defcustom whdl-delete-header-and-footer t
  "*Modelsim writes two lines of information at the beginning of the file and some lines at the end. To delete them set to t."
  :type 'boolean
  :group 'whdl-process-wave-general)

(defcustom whdl-use-label-for-signal t
  "*If t, only the signal names will be displayed in the wave form window, the path will not be visible anymore."
  :type 'boolean
  :group 'whdl-process-wave-general)

(defcustom whdl-namecolwidth 230
  "*Used to adjust the width of the name in the wavewindow. I don't know what unit this is .. will be used as it is for modelsim. Standard modelsim
value is 150"
  :type 'integer
  :group 'whdl-process-wave-general)

(defcustom whdl-valuecolwidth 130
  "*Used to adjust the width of the value in the wavewindow. I don't know what unit this is .. will be used as it is for modelsim. Standard modelsim
value is 100"
  :type 'integer
  :group 'whdl-process-wave-general)

(defcustom whdl-time-to-run '(("david" 100000) ("generalram" 1000))
  "*Time that will be run after start. If non nil it has to an integer in ns."
  :type '(repeat
           (list :tag "time to run to" :indent 2
                 (string :tag "name       ")
                 (choice (integer :tag "run to     ")
                         (string :tag "run to     "))))
  :group 'whdl-process-wave-project)

(defcustom whdl-zoom-ns '(("david" 70000 83000) ("generalram" 7000 8000))
  "*WaveWidnow will show wave starting at whdl-zoom-from-ns, until ZoomToNS. Be careful, WaveWindow changes Zoom if it changes size. Be sure that you have opened it in the desired size to let it work properly. If nil no information will be included, if t the value of existing wave.do will be used."
  :type '(repeat
           (list :tag "zoom to start from" :indent 2
                 (string  :tag "name       ")
                 (integer :tag "zoom from  ")
                 (integer :tag "zoom to    ")))
  :group 'whdl-process-wave-project)

(defcustom whdl-names-to-delete '()
  "*All signals containing one of these strings will be deleted. Each string is seen as regexp!! Insert '/' at beginning and end of string to ensure that only the component is deleted, and not signals containing this name."
  :type '(repeat
          (list :tag "project" :indent 2
                (string :tag "name             ")
                (repeat
                 (string :tag "regexp     "))))
  :group 'whdl-process-wave-project)

(defcustom whdl-names-not-to-delete '()
  "*All signals containing one of these strings will be deleted. Each string is seen as regexp!! Insert '/' at beginning and end of string to ensure that only the component is deleted, and not signals containing this name."
  :type '(repeat
          (list :tag "project" :indent 2
                (string :tag "name             ")
                (repeat
                 (string :tag "regexp     "))))
  :group 'whdl-process-wave-project)


(defcustom whdl-highlight '(("project" (("cyan" ("generalram" "anotherword")) ("yellow" ("greenwitch" "yetanotherword")))) ("another project" (("cyan" ("generalram" "anotherword")) ("yellow" ("greenwitch" "yetanotherword")))))
  "*Each list in this list as to contain a color as first argument (must be known by modelsim) and regexps.All signals matching one of the regexps will be highlighted. If regexp ends with Ampersand (eg 'state&') only singals with exact the same name will be highlited (equal is used instead of string-match). Set nil if you do not want to use highlighting."
  :type '(repeat
       (list :tag "project" :indent 2
             (string :tag "name            ")
             (repeat
              (list :tag "highlight-rule" :indent 4
                    (string :tag "color              ")
                    (repeat
                     (string :tag "regexp      "))))))
  :group 'whdl-process-wave-project)


(defcustom whdl-radix '()
  "*Each list in this list as to contain a color as first argument
(must be known by modelsim) and regexps.All signals matching one of the regexps will be highlighted. If regexp ends with Ampersand (eg 'state&') only singals with exact the same name will be highlited (equal is used instead of string-match). Set nil if you do not want to use highlighting."
  :type '(repeat
       (list :tag "project" :indent 2
             (string :tag "name            ")
             (repeat
              (list :tag "One set" :indent 4
                    (choice :tag "Choose Radix  "
                            (const :tag "symbolic" "smbolic")
                            (const :tag "binary" "binary")
                            (const :tag "octal" "octal")
                            (const :tag "decimal" "decimal")
                            (const :tag "unsigned" "unsigned")
                            (const :tag "hex(default)" "hex")
                            (const :tag "ascii" "ascii"))
                    (repeat
                      (string :tag "regexp      "))))))
  :group 'whdl-process-wave-project)

(defcustom whdl-names-to-front '()
  "*All signals containing one of these strings will be put in front of the list, so they appear on the beginning of the waveform. Each string is seen as regexp!! Insert '/' at beginning and end of string to ensure that only the component is deleted, and not signals containing this name."
  :type '(repeat
          (list :tag "project" :indent 2
                (string :tag "name             ")
                (repeat
                 (string :tag "regexp     "))))
  :group 'whdl-process-wave-project)


(defun whdl-get-projects-alist ()
  (let ((ret-val '()) (counter 0))
    (while (nth counter whdl-zoom-ns)
      (setq ret-val (delete (cons (car (nth counter whdl-zoom-ns)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-zoom-ns)) '1))) counter (1+ counter)))
    (setq counter 0)
    (while (nth counter whdl-time-to-run)
      (setq ret-val (delete (cons (car (nth counter whdl-time-to-run)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-time-to-run)) '1))) counter (1+ counter)))
    (setq counter 0)
    (while (nth counter whdl-names-to-delete)
      (setq ret-val (delete (cons (car (nth counter whdl-names-to-delete)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-names-to-delete)) '1))) counter (1+ counter)))
    (setq counter 0)
    (while (nth counter whdl-highlight)
      (setq ret-val (delete (cons (car (nth counter whdl-highlight)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-highlight)) '1))) counter (1+ counter)))
    (setq counter 0)
    (while (nth counter whdl-radix)
      (setq ret-val (delete (cons (car (nth counter whdl-radix)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-radix)) '1))) counter (1+ counter)))
    (setq counter 0)
    (while (nth counter whdl-names-not-to-delete)
      (setq ret-val (delete (cons (car (nth counter whdl-names-not-to-delete)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-names-not-to-delete)) '1))) counter (1+ counter)))
    (setq counter 0)
    (while (nth counter whdl-names-to-front)
      (setq ret-val (delete (cons (car (nth counter whdl-names-to-front)) '1) ret-val))
      (setq ret-val (append ret-val (list (cons (car (nth counter whdl-names-to-front)) '1))) counter (1+ counter)))
  ret-val))


(defun vhdl-process-wave ()
"Functions processes .do file saved of Modelsim wave window to make nicer waveform.
Generally this function inserts divider for each new entity and makes all signals be shown as hexadecimal values.
Additionally it is possible to delete sinal pathes if they get too long. This information will not be needed,
because pathname is used as divider string (last 40 characters).
Different options are available for this function. See description at top of file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((already-done 0) (projects (whdl-get-projects-alist)) project nr-of-entities path nextPath the-time-string)
      (if (> (length projects) 0)
          (setq project (completing-read "Of which project is this file? " (whdl-get-projects-alist) nil t nil (whdl-get-projects-alist))))
      (whdl-delete-signals project)
      ;;(whdl-delete-header)
      (setq nr-of-entities (whdl-tcl-count-entities))
      (setq already-done 0)
      (while (< already-done nr-of-entities)
        (search-forward "/")
        (beginning-of-line)
        (setq path (whdl-tcl-extract-path))
        (whdl-insert-divider path)
        (whdl-tcl-insert-ral project)
        (forward-line)
        (setq nextPath (whdl-tcl-extract-path))
        (while (equal path nextPath)
          (whdl-tcl-insert-ral project)
          (forward-line)
          (setq nextPath (whdl-tcl-extract-path)))
        (insert "\C-j\C-j")
        (setq already-done (1+ already-done)))
      (setq the-time-string (whdl-get-timestring project))
      (if whdl-delete-header-and-footer
          (delete-region (point) (point-max)))
      (insert (concat "configure wave -namecolwidth " (number-to-string whdl-namecolwidth) "\n"))
      (insert (concat "configure wave -valuecolwidth " (number-to-string whdl-valuecolwidth) "\n" ))
      (if (setq current-time (whdl-get-project project whdl-time-to-run))
          (if (numberp (nth 1 current-time))
              (insert (concat "run " (number-to-string (nth 1 current-time)) " ns\n"))
            (insert (concat "run " (nth 1 current-time) " \n"))))
      (whdl-zooming-to project)
      (whdl-signals-to-top project)
      )))

(defun whdl-get-project (project variable)
  (let ((counter 0) found)
    (while (and (nth counter variable) (not found))
      (if (equal (car (nth counter variable)) project)
          (setq found t)
        (setq counter (1+ counter))))
    (nth counter variable)))

(defun whdl-zooming-to (project)
  (let ((current-zoom (whdl-get-project project whdl-zoom-ns)))
    (if current-zoom
        (if (not (integerp (cadr current-zoom)))
            (insert the-time-string)
          (insert (concat "WaveRestoreZoom {" (number-to-string (nth 1 current-zoom)) " ns} {" (number-to-string (nth 2 current-zoom)) " ns}\n"))))))

(defun whdl-delete-signals (project)
  (let ((current-not-to-delete (nth 1 (whdl-get-project project whdl-names-not-to-delete))) (counter 0))
    (if (not (equal (length current-not-to-delete) 0))
        (save-excursion
        (let ((counter 0) (counter1 0) (counter2 0) found)
          (goto-char (point-min))
          (while (not (equal (point) (point-max)))
            (end-of-line)
            (setq endofl (point))
            (beginning-of-line)
            (setq begofl (point))
            (while (and (not found) (nth counter current-not-to-delete))
              (setq pfound (re-search-forward (nth counter current-not-to-delete) nil t nil))
              (if (and pfound (> pfound begofl) (< pfound endofl))
                  (setq found t)
                (goto-char begofl))
              (setq counter (1+ counter)))
            (setq counter 0)
            (if (not found)
                (kill-region begofl (1+ endofl))
              (progn
                (setq found nil)
                (forward-line)))))))
    (save-excursion
      (let ((current-to-delete (nth 1 (whdl-get-project project whdl-names-to-delete))) (counter 0))
        (while (nth counter current-to-delete)
          (goto-char (point-min))
          (whdl-tcl-delete-specific-signals (nth counter current-to-delete))
          (setq counter (1+ counter))))
      (goto-char (point-min)))))

(defun whdl-tcl-delete-specific-signals (name)
"deletes all signals containing name"
  (setq SomethingFound t)
  (save-excursion
    (while SomethingFound
      (setq SomethingFound (re-search-forward name nil t nil))
      (if SomethingFound
      (progn
        (end-of-line 0)
        (setq FirstPoint (point))
        (end-of-line 2)
        (delete-region FirstPoint (point)))))))

(defun whdl-signals-to-top (project)
  "moves signals defined on top of waveform"
  (let ((current-to-top (nth 1 (whdl-get-project project whdl-names-to-front))) (counter 0) A)
    (if (not (equal (length current-to-top) 0))
        (save-excursion
          (goto-char (point-min))
          (search-forward "add wave" nil t nil)
          (beginning-of-line)
          (insert "add wave -noupdate -divider signals-on-top\C-j")
          (setq A (point))
          (while (nth counter current-to-top)
            (insert (whdl-get-signals-for-top (nth counter current-to-top)))
            (setq counter (1+ counter)))
          (insert "\C-j")))))

(defun whdl-get-signals-for-top (name)
  "collects all strings containing name"
  (setq something-found t ret-val "")
  (save-excursion
    (while something-found
      (setq something-found (re-search-forward name nil t nil))
      (if something-found
          (progn
            (beginning-of-line)
            (setq tp (point))
            (end-of-line)
            (setq temp-val (buffer-substring-no-properties tp (point)))
            (setq ret-val (concat ret-val temp-val))
            (setq ret-val (concat ret-val "\C-j"))
            (delete-region tp (1+ (point)))))))
  ret-val)


(defun whdl-delete-header ()
  (if whdl-delete-header-and-footer
      (progn
        (setq A (point))
        (forward-line 2)
        (delete-region A (point)))))

(defun whdl-insert-divider (path)
  (if (< (length path) whdl-length-of-divider)
      (insert "add wave -noupdate -divider " path "\C-j")
    (insert "add wave -noupdate -divider " (substring path (- 0 whdl-length-of-divider) nil)"\C-j") ))

(defun whdl-get-timestring (project)
  (if (and (whdl-get-project project whdl-zoom-ns) (not (integerp (nth 1 (whdl-get-project project whdl-zoom-ns)))))
      (save-excursion
        (if (re-search-forward "WaveRestoreZoom {" nil t nil)
            (progn
              (beginning-of-line)
              (setq start (point))
              (end-of-line)
              (buffer-substring-no-properties start (point)))))))

(defun whdl-tcl-count-entities ()
"This function counts entities in the .do file"
  (setq Counter 1)
  (save-excursion
   (goto-char (point-min))
   (search-forward "/")
   (beginning-of-line)
   (setq first-path (whdl-tcl-extract-path))
   (forward-line)
   (setq next-path (whdl-tcl-extract-path))
   (forward-line)
   (while (not (eobp))
     (if (and (not (equal next-path "")) (not (equal first-path next-path)))
       (setq Counter (1+ Counter)))
     (setq first-path next-path)
     (setq next-path (whdl-tcl-extract-path))
     (forward-line)))
  (if (equal Counter 0)
      (setq Counter 1))
  Counter)


(defun whdl-tcl-extract-path ()
"Searches for next path and returns it."
  (save-excursion
     (setq Found (search-forward "/" nil t nil))
     (if Found
         (progn
           (setq A (point))
           (end-of-line)
           (search-backward "/" nil t nil)
           (setq RetVal (buffer-substring-no-properties (point) A)))
       (setq RetVal ""))
     RetVal))

(defun whdl-tcl-insert-ral (project)
"insert ral means insert radix and label. This function can also extract signal from path
before inserting radix and label. Label is optional"
  (save-excursion
    (let (A Varname Line (current-highlight (whdl-get-project project whdl-highlight)) (current-radix (whdl-get-project project whdl-radix)))
      (end-of-line)
      (setq A (point))
      (search-backward "/")
      (forward-char)
      (setq VarName (buffer-substring-no-properties A (point)))
      (beginning-of-line)
      (setq Line (buffer-substring-no-properties A (point)))
      (search-forward "/")
      (search-backward " ")
      (whdl-insert-radix Line (nth 1 current-radix))
      (if whdl-use-label-for-signal
          (insert " -label {" VarName "}"))
      (whdl-highlight-name VarName (nth 1 current-highlight))))
  (indent-to-column 2)
  (beginning-of-line))

(defun whdl-insert-radix (var-name current-radix)
  (if current-radix
      (let ((counter-outer 0) (counter-inner 1) found current-list)
        (while (and (not found) (setq current-list (cadr (nth counter-outer current-radix))))
          (setq counter-inner 0)
          (while (and (not found) (nth counter-inner current-list))
            (if (string-match "&" (nth counter-inner current-list))
                (setq found (equal (car (split-string (nth counter-inner current-list) "&")) var-name))
              (if (string-match (nth counter-inner current-list) var-name)
                  (setq found t)))
            (setq counter-inner (1+ counter-inner)))
          (setq counter-outer (1+ counter-outer)))
        (if found
            (insert (concat " -radix " (nth 0 (nth (- counter-outer 1) current-radix))))
          (insert " -radix HEX")))
    (insert " -radix HEX")))


(defun whdl-highlight-name (var-name current-highlight)
  (if current-highlight
      (let ((counter-outer 0) (counter-inner 1) found current-list)
        (while (and (not found) (setq current-list (cadr (nth counter-outer current-highlight))))
          (setq counter-inner 0)
          (while (and (not found) (nth counter-inner current-list))
            (if (string-match "&" (nth counter-inner current-list))
                (setq found (equal (car (split-string (nth counter-inner current-list) "&")) var-name))
              (if (string-match (nth counter-inner current-list) var-name)
                  (setq found t)))
            (setq counter-inner (1+ counter-inner)))
          (setq counter-outer (1+ counter-outer)))
        (if found
            (insert (concat " -color " (nth 0 (nth (- counter-outer 1) current-highlight))))))))


(provide 'vhdl-process-wave)
;;; vhdl-process-wave.el ends here
