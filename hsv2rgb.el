;;; hsv2rgb.el --- Transformation from HSV to RGB
;;;
;;; Adrian Aichner <adrian@xemacs.org>, The XEmacs Project, 2005-08-28.
;;;
;;; Following information from http://en.wikipedia.org/wiki/HSV_color_space
;;; 
;;; Transformation from HSV to RGB
;;; 
;;; Given a color defined by (H, S, V) values, with H, varying between 0.0
;;; and 360.0, indicating the angle, in degrees, around the color circle
;;; where the hue is located, and with S and V, varying between 0.0 and
;;; 1.0, representing the saturation and value, respectively, a
;;; corresponding (R, G, B) color can be determined through a series of
;;; formulas.
;;; 
;;; First, if S is equal to 0.0, then the resulting color is achromatic,
;;; or grey. In this special case, R, G, and B are simply equal to V. As
;;; above, H is irrelevant in this situation.
;;; 
;;; When S is non-zero, the following formulas can be used:
;;; 
;;; H_i =  floor (H / 60)
;;; 
;;; f = ( H / 60 ) - H_i
;;; 
;;; p = V * ( 1 - S )
;;; 
;;; q = V * ( 1 - f * S )
;;; 
;;; t = V * ( 1 - ( 1 - f ) * S )
;;; 
;;; if H_i = 0 -> R = V, G = t, B = p
;;; 
;;; if H_i = 1 -> R = q, G = V, B = p
;;; 
;;; if H_i = 2 -> R = p, G = V, B = t
;;; 
;;; if H_i = 3 -> R = p, G = q, B = V
;;; 
;;; if H_i = 4 -> R = t, G = p, B = V
;;; 
;;; if H_i = 5 -> R = V, G = p, B = q

(defun hsv2rgb (h s v)
  "Convert a point in the Hue, Saturation, Value (aka Brightness)
color space to list of normalized Red, Green, Blue values.

HUE is an angle in the range of 0.0 degrees inclusive to 360.0
exclusive.  The remainder of division by 360.0 is used for
out-of-range values.
SATURATION is in the range of 0.0 to 1.0.
VALUE is in the range of 0.0 to 1.0.
Returns a list of values in the range of 0.0 to 1.0.
"
  ;; Coerce to float and get hue into range.
  (setq h (mod h 360.0)
        s (float s)
        v (float v))
  (let* ((hi (floor h 60.0))
         (f (- (/ h 60.0) hi))
         (p (* v (- 1.0 s)))
         (q (* v (- 1.0 (* f s))))
         ;; cannot use variable t, obviously.
         (u (* v (- 1.0 (* (- 1.0 f) s))))
         r g b)
    (cond
     ((= hi 0)
      (setq r v
            g u
            b p))
     ((= hi 1)
      (setq r q
            g v
            b p))
     ((= hi 2)
      (setq r p
            g v
            b u))
     ((= hi 3)
      (setq r p
            g q
            b v))
     ((= hi 4)
      (setq r u
            g p
            b v))
     ((= hi 5)
      (setq r v
            g p
            b q)))
    (list r g b)))

(defun hsv-find-close-colors ()
  "Find color names \"close\" to certain HSV numbers.

This implementation is inadequate."
  (interactive)
  (with-displaying-temp-buffer
      "*find close colors*"
    (mapcar
     (lambda (cn)
       (let ((rgb-comp
              (color-rgb-components (make-color-specifier (car cn))))
             (close 10000))
         (do ((hue 0 (setq hue (+ hue 36))) (match nil))
             ((>= hue 360))
           (do ((sat 5 (setq sat (+ sat 5))))
               ((or match (>= sat 35)))
             (do ((val 255 (setq val (- val 5))))
                 ((or match (<= val 180)))
               (let ((rgb-list
                      (mapcar
                       (lambda (norm-value)
                         (truncate (* 65535.0 norm-value)))
                       (hsv2rgb hue (/ sat 255.0) (/ val 255.0)))))
                 (when (and
                        (< (abs (- (first rgb-list) (first rgb-comp))) close)
                        (< (abs (- (second rgb-list) (second rgb-comp))) close)
                        (< (abs (- (third rgb-list) (third rgb-comp))) close))
                   (princ
                    (format "hsv %d %f %f, rgb %S close to color %S (%S)\n"
                            hue (/ sat 255.0) (/ val 255.0) rgb-list (car cn) rgb-comp))
                   (setq match t))))))))
     (read-color-completion-table))))

;;;###autoload
(defun Hsv2ColoredText (h s v)
  "Present an HSV color example as informative text with background of
that color."
  (interactive
   (list
    (read-number "hue [0.0 360.0) ")
    (read-number "saturation [0.0 1.0) ")
    (read-number "value (brightness) [0.0 1.0) ")))
  (let* ((rgb
          (apply #'format
                 "#%02x%02x%02x"
                 (mapcar
                  (lambda (n) (truncate (* n 255)))
                  (hsv2rgb h s v))))
         (hsv-face (make-face (gensym))))
    (set-face-background hsv-face rgb)
    (put-text-property 0 (length rgb) 'face hsv-face rgb)
    (princ (format "%3d %5.3f %5.3f " h s v))
    (princ rgb)
    (princ "\n")))

;;;###autoload
(defun Hsv2ColoredTextSpectrum (HueFrom HueTo HueStep
                                SatFrom SatTo SatStep
                                ValFrom ValTo ValStep)
  "Present HSV Color spectrum in temp buffer."
  (interactive
   (let* ((HueFrom
           (read-number
            "hue from [0.0 360.0) " t))
          (HueTo
           (read-number
            "hue to [0.0 360.0) " t))
          (HueStep
           (read-number
            (format "hue step [%s1 %d) "
                    (if (> HueTo HueFrom) "" "-")
                    (- HueTo HueFrom)) t))
          (SatFrom
           (float (read-number
                   "saturation from [0.0 1.0) ")))
          (SatTo
           (float (read-number
                   "saturation to [0.0 1.0) ")))
          (SatStep
           (read-number
            (format "saturation step [%.4f %.4f) "
                    (/ (- SatTo SatFrom) 100.0)
                    (- SatTo SatFrom))))
          (ValFrom
           (float (read-number
                   "value (brightness) from [0.0 1.0) ")))
          (ValTo
           (float (read-number
                   "value (brightness) to [0.0 1.0) ")))
          (ValStep
           (read-number
            (format "value (brightness) step [%.4f %.4f) "
                    (/ (- ValTo ValFrom) 100.0)
                    (- ValTo ValFrom)))))
     (list
      HueFrom HueTo HueStep
      SatFrom SatTo SatStep
      ValFrom ValTo ValStep)))
  (with-displaying-temp-buffer
      "*Hsv2ColoredTextSpectrum*"
    (princ
     (format
      "Produced by\n%s %d %d %d %.4f %.4f %.4f %.4f %.4f %.4f)\nfrom %s\n"
      "(Hsv2ColoredTextSpectrum"
      HueFrom HueTo HueStep SatFrom SatTo SatStep ValFrom ValTo ValStep
      "http://www.emacswiki.org/cgi-bin/emacs/hsv2rgb.el"))
    (princ "Hue Satur Value RGB Sample\n")
    (do ((hue HueFrom (setq hue (+ hue HueStep))))
        ((if (>= HueTo HueFrom) (> hue HueTo) (< hue HueTo)))
      (do ((sat SatFrom (setq sat (+ sat SatStep))))
          ((if (>= SatTo SatFrom) (> sat SatTo) (< sat SatTo)))
        (do ((val ValFrom (setq val (+ val ValStep))))
            ((if (>= ValTo ValFrom) (> val ValTo) (< val ValTo)))
          (Hsv2ColoredText hue sat val))))))

; (Hsv2ColoredText 0 1 1)

(provide 'hsv2rgb)

;;; hsv2rgb.el ends here.
