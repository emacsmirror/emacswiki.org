;;; bmi.el --- Calculate body mass index (BMI)

;; Copyright (C) 2007  Aaron Hawley

;; Author: Aaron S. Hawley
;; Keywords: games, health

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.  See <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To calculate your BMI from your weight in pounds, and your height
;; in feet and inches, run:

;;   M-x bmi-pounds-feet-inches RET

;; To calculate your BMI from your weight in pounds, and your height
;; in inches, run:

;;   M-x bmi-pounds-inches RET

;; To calculate your BMI from your weight in kilograms, and your
;; height in meters, run:

;;   M-x bmi-kilos-meters RET

;; To calculate your BMI from your weight in kilograms, and your
;; height in centimeters, run:

;;   M-x bmi-kilos-centimeters RET

;; An example result is:

;;   Body mass index (BMI): 24.5 ("Normal")

;; The categories are for adults 20 and older.

;; See http://www.cdc.gov/nccdphp/dnpa/bmi/

;;; History:
;; Written 1 October, 2007 by Aaron S. Hawley in Burlington, Vermont.

;;; Code:

(defun bmi-pounds-inches (weight height)
  "Calculate body mass index from WEIGHT and HEIGHT."
  (interactive "nPounds:\nnInches:")
  ;; Cast potential integer values to floating point to avoid
  ;; expressions being truncated to zero.
  (bmi-message (* (/ (float weight) (* height height)) 703)))
               ;; BMI = 703 * pounds / inches^2

(defun bmi-pounds-feet (weight feet inches)
  "Calculate body mass index from WEIGHT pounds, FEET and INCHES."
  (interactive "nPounds:\nnFeet:\nnInches:")
  
  (bmi-pounds-inches weight (+ (* 12 feet) inches)))
                            ;; total inches = 12 * feet + inches

(defun bmi-kilos-meters (weight height)
  "Calculate body mass index from WEIGHT kilograms and HEIGHT meters."
  (interactive "nKilograms:\nnMeters:")
  ;; Convert integers to float for type coercion.
  (bmi-message (/ (float weight) (* height height))))
               ;; BMI = kilos / meters^2

(defun bmi-kilos-centimeters (weight height)
  "Calculate body mass index from WEIGHT kilograms and HEIGHT centimeters."
  (interactive "nKilograms:\nnCentimeters:")
  ;; Convert integers to float for type coercion.
  (bmi-kilos-meters weight (/ (float height) 100)))
                           ;; meters = centimeters / 100

(defun bmi-message (bmi-number)
  "Return body mass index message for BMI-NUMBER."
  (let ((bmi-status (cond
                     ((< bmi-number 15.0) "Starvation")
                     ((< bmi-number 18.5) "Underweight")
                     ((< bmi-number 25.0) "Normal")
                     ((< bmi-number 30.0) "Overweight")
                     ((< bmi-number 40.0) "Obese")
                     (t "Morbidly obese"))))
    (message
     (format "Body mass index (BMI): %.1f (\"%s\")"
             bmi-number bmi-status))
    ;; Return `bmi-number' as function's return value.
    bmi-number))

(provide 'bmi)

;;; bmi.el ends here
