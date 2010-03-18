;;; sml-modeline.el --- Show position in a scrollbar like way in mode-line
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-03-16 Tue
;; Version:
;; Last-Updated:
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
;; Show scrollbar like position indicator in mode line.
;;
;; Idea and part of this code is adapted from David Engster's and Drew
;; Adam's code in these mail messages:
;;
;;   http://lists.gnu.org/archive/html/emacs-devel/2010-03/msg00523.html
;;   http://permalink.gmane.org/gmane.emacs.devel/122038
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
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

;;;###autoload
(defgroup sml-modeline nil
  "Customization group for `sml-mode'."
  :group 'frames)

(defcustom sml-len 12
  "Mode line indicator total length."
  :type 'integer
  :group 'sml-modeline)

(defcustom sml-borders nil
  "Indicator borders.
This is a pair of indicators, like [] or nil."
  :type '(choice (const :tag "None" nil)
                 (cons (string :tag "Left border")
                       (string :tag "Right border")))
  :group 'sml-modeline)

(defface sml-end-face
  '((t (:inherit match)))
  "Face for invisible buffer parts."
  :group 'sml-modeline)
;; 'face `(:background ,(face-foreground 'mode-line-inactive)
;;         :foreground ,(face-background 'mode-line))

(defface sml-vis-face
  '((t (:inherit region)))
  "Face for invisible buffer parts."
  :group 'sml-modeline)
;; 'face `(:background ,(face-foreground 'mode-line)
;;         :foreground ,(face-background 'mode-line))

;;(sml-create)
(defun sml-create ()
  (let* ((wstart (window-start))
         (wend (window-end))
         (real-point-max (save-restriction (widen) (point-max)))
         (percentage-beg (/ (float wstart) (float real-point-max)))
         (percentage-end (/ (float wend) (float real-point-max)))
         (sml-begin (or (car sml-borders) ""))
         (sml-end   (or (cdr sml-borders) ""))
         (inner-len (- sml-len (length sml-begin) (length sml-end)))
         bpad-len
         epad-len
         (start (floor (* percentage-beg inner-len)))
         (end (ceiling (* percentage-end inner-len)))
         string)
    (if (not (or (< wend real-point-max) (> wstart 1)))
        ""
      (setq string
            (concat (format "%02d" (round (* percentage-beg 100)))
                    "-"
                    (format "%02d" (round (* percentage-end 100))) "%%"))
      (setq bpad-len (floor (/ (- inner-len (length string)) 2.0)))
      (setq epad-len (- inner-len (length string) bpad-len))
      (setq string (concat sml-begin
                           (make-string bpad-len 32)
                           string
                           (make-string epad-len 32)
                           sml-end))
      ;;(assert (= (length string) sml-len) t)
      (when (= start sml-len) (setq start (1- start)))
      (setq start (+ start (length sml-begin)))
      (put-text-property start end 'face 'sml-vis-face string)
      (when (and (= 0 (length sml-begin))
                 (= 0 (length sml-end)))
        (put-text-property 0 start 'face 'sml-end-face string)
        (put-text-property end sml-len 'face 'sml-end-face string))
      string)))

(defvar sml-old-car-mode-line-position nil)

;;;###autoload
(define-minor-mode sml-mode
  "Show buffer size and position like scrollbar in mode line."
  :global t
  :group 'sml-modeline
  (if sml-mode
      (progn
        (unless sml-old-car-mode-line-position
          (setq sml-old-car-mode-line-position (car mode-line-position)))
        (setcar mode-line-position '(:eval (list (sml-create)))))
    (setcar mode-line-position sml-old-car-mode-line-position)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sml-modeline.el ends here
