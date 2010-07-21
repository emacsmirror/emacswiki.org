;;; ppm-gen.el --- simple & slow ppm generation in emacs.


;; Copyright (C) 2010 Joyer Huang

;; Author: Joyer Huang <collger@eyou.com>
;; Version: 0.0.4
;; Keywords: draw, ppm, ray tracing, bitmap, generator
;; URL: http://slimeweb.com

;; v0.0.4 add ppm-scale-bilinear and ppm-cubic-interp
;; v0.0.3 add ppm-scale and ppm-scale-bilinear
;; v0.0.2 add BMP file support
;;        ppm-show is faster now
;;        draw-line stub
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; This package provide a set of PPM file generation function, you can
;; now use emacs as a canvas to draw thing, and output to a buffer,
;; view it with image-mode-maybe. emacs are slow on this kind of massive
;; math operations, by I managed to write a ray tracer with ppm-gen.
(require 'eieio-base)
(require 'image-mode)
(require 'eieio)
(eval-when-compile
  (require 'cl)
  (require 'eieio-comp))

(defclass ppm () ((width :initarg :w)
                  (height :initarg :h)
                  (pixels :initarg :pixels)))

(defun ppm-make (width height)
  "make a ppm object
WIDTH is ppm width(number)
HEIGHT is ppm height(number)
"
  (ppm nil :w width :h height
    :pixels (make-vector (* width height 3) 0)))

(defconst ppm-cv [
          "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "70" "71" "72" "73" "74" "75" "76" "77" "78" "79" "80" "81" "82" "83" "84" "85" "86" "87" "88" "89" "90" "91" "92" "93" "94" "95" "96" "97" "98" "99" "100" "101" "102" "103" "104" "105" "106" "107" "108" "109" "110" "111" "112" "113" "114" "115" "116" "117" "118" "119" "120" "121" "122" "123" "124" "125" "126" "127" "128" "129" "130" "131" "132" "133" "134" "135" "136" "137" "138" "139" "140" "141" "142" "143" "144" "145" "146" "147" "148" "149" "150" "151" "152" "153" "154" "155" "156" "157" "158" "159" "160" "161" "162" "163" "164" "165" "166" "167" "168" "169" "170" "171" "172" "173" "174" "175" "176" "177" "178" "179" "180" "181" "182" "183" "184" "185" "186" "187" "188" "189" "190" "191" "192" "193" "194" "195" "196" "197" "198" "199" "200" "201" "202" "203" "204" "205" "206" "207" "208" "209" "210" "211" "212" "213" "214" "215" "216" "217" "218" "219" "220" "221" "222" "223" "224" "225" "226" "227" "228" "229" "230" "231" "232" "233" "234" "235" "236" "237" "238" "239" "240" "241" "242" "243" "244" "245" "246" "247" "248" "249" "250" "251" "252" "253" "254" "255" 
          ])

(defun ppm-show (ppm &optional kill p3)
  "show a ppm-object in other buffer window
PPM is the ppm object to show"
  (with-slots ((w width)
               (h height)
               pixels) ppm
    (when kill (kill-buffer (get-buffer-create "*ppm gen*")))
    (switch-to-buffer-other-window (get-buffer-create (generate-new-buffer-name "*ppm gen*")))
    (set-buffer-multibyte nil)
    (insert (format (if (not p3) "P6\r%d %d\r255\r" "P3\r%d %d\r255\r") w h))
    (if (not p3) (insert (concat pixels))
      (dotimes (idx (* w h 3))
        (insert (aref ppm-cv (aref pixels idx)) " ")
        (if (eq (% (1+ idx) w) 0)
            (insert "\r"))))
    (image-mode)))

(defun ppm-plot (ppm x y color)
  "plot a color at ppm object
X,Y position to plot
COLOR is the RGB value to plot, no alpha support"
  (with-slots ((w width)
               (h height)
               pixels) ppm
    (let* ((idx (* (+ (* y w) x) 3)))
      (aset pixels idx (logand (lsh color -16) 255))
      (aset pixels (+ 1 idx) (logand (lsh color -8) 255))
      (aset pixels (+ 2 idx) (logand color 255)))))

(defun ppm-pixels-plot (pixels x y color width)
  (let ((idx (* (+ (* y width) x) 3)))
    (aset pixels idx (logand (lsh color -16) 255))
    (aset pixels (+ 1 idx) (logand (lsh color -8) 255))
    (aset pixels (+ 2 idx) (logand color 255))))

(defun ppm-rgb (r g b)
  (logior (lsh (min (floor r) 255) 16) (lsh (min (floor g) 255) 8) (min (floor b) 255)))

(defun ppm-draw-line (ppm x1 y1 x2 y2 color)
  (with-slots ((w width)
               (h height)
               (pxs pixels)) ppm
    (let ((yd (- y2 y1))
          (xd (- x2 x1))
          (yi y1)
          (ya 0))
      (loop for xi from x1 to x2 do
            (ppm-pixels-plot pxs xi yi color w)
            (incf ya yd)
            (when (>= ya xd)
              (incf yi)
              (decf ya xd))))))

(provide 'ppm-gen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Test Ground

;(ppm-show (ppm-make 200 100))

;; ; test draw line and show
;; (let* ((w 256) (h 256)
;;        (ppm (ppm-make w h t))
;;        (pixels (oref ppm pixels)))
;;   ;; (loop for x from 0 to (1- w) do
;;   ;;       (loop for y from 0 to (1- h) do
;;   ;;             (ppm-pixels-plot pixels x y (ppm-rgb (/ (* x 255) w) (/ (* y 255) h) 0) w)))
;;   (loop for k from 0 to 255 by 3 do
;;         (ppm-draw-line ppm 0 0 255 k 255))
;;   (ppm-show ppm)
;;   )

(defclass v3  () ((x :initfrom 0. :initarg :x)
                  (y :initfrom 0. :initarg :y)
                  (z :initfrom 0. :initarg :z)))
(defun makei-v3 (x y z)
  (v3 nil :x x :y y :z z))
(defun len-v3 (pv3)
  (with-slots (x y z) pv3
    (sqrt (+ (* x x) (* y y) (* z z)))))
(defun lensq-v3 (pv3)
  (with-slots (x y z) pv3
    (+ (* x x) (* y y) (* z z))))
(defun norm-v3 (pv3)
  (with-slots (x y z) pv3
    (let ((inv (/ 1.0 (len-v3 pv3))))
      (makei-v3 (* inv x) (* inv y) (* inv z)))))
(defun neg-v3 (pv3)
  (with-slots (x y z) pv3
    (makei-v3 (- x) (- y) (- z))))
(defun add-v3 (pv3 other)
  (with-slots (x y z) pv3
    (with-slots ((xo x) (yo y) (zo z)) other
    (makei-v3 (+ x xo) (+ y yo) (+ z zo)))))
(defun sub-v3 (pv3 other)
  (with-slots (x y z) pv3
    (with-slots ((xo x) (yo y) (zo z)) other
    (makei-v3 (- x xo) (- y yo) (- z zo)))))
(defun mul-v3 (pv3 f)
  (with-slots (x y z) pv3
    (makei-v3 (* x f) (* y f) (* z f))))
(defun div-v3 (pv3 d)
  (with-slots (x y z) pv3
    (let ((f (/ 1 d)))
      (makei-v3 (* x f) (* y f) (* z f)))))
(defun dot-v3 (pv3 other)
  (with-slots (x y z) pv3
    (with-slots ((xo x) (yo y) (zo z)) other
      (+ (* x xo) (* y yo) (* z zo)))))
(defun vxv-v3 (pv3 other)
  (with-slots (x y z) pv3
    (with-slots ((xo x) (yo y) (zo z)) other
      (makei-v3 (+ (* (- z) yo) (* y zo))
                   (- (* z xo) (* x zo))
                   (+ (* (- yo) x) (* x yo))))))

(defconst v3-zero (makei-v3 0 0 0))

(defclass r3 () ((origin :initarg :origin)
                 (direction :initarg :direction)))
(defun makei-r3 (origin direction)
  (r3 nil :origin origin :direction direction))

(defun pt-at-r3 (pr3 T)
  (add-v3 (oref pr3 origin)
             (mul-v3 (oref pr3 direction) T)))

(defclass isect-info () ((geometry :initarg :geometry)
                      (distance :initarg :distance)
                      (position :initarg :position)
                      (normal :initarg :normal)))
(defun makei-isect-info ()
  (isect-info nil :geometry nil :distance 0 :position v3-zero :normal v3-zero))
(defconst isect-no-hit (makei-isect-info))

(defclass sphere3 () ((center :initarg :center)
                      (radius :initarg :radius)
                      (s_radius_sq)
                      (material)
                      ))
(defun makei-sphere3 (center radius)
  (let ((sp (sphere3 nil :center center :radius radius)))
    (oset sp s_radius_sq (* (oref sp radius) (oref sp radius)))
    sp))
(eval-when-compile 
(defmethod intersect3 ((sp sphere3) ray)
  (let* ((v (sub-v3 (oref ray origin) (oref sp center)))
         (a0 (- (lensq-v3 v) (oref sp s_radius_sq)))
         (dotV (dot-v3 (oref ray direction) v)))
    (if (> dotV 0.0)
        isect-no-hit
      (let ((discr (- (* dotV dotV) a0)))
        (if (< discr 0)
            isect-no-hit
          (let ((iinfo (makei-isect-info)))
            (oset iinfo geometry sp)
            (oset iinfo distance (- 0 dotV (sqrt discr)))
            (oset iinfo position
                  (pt-at-r3 ray (oref iinfo distance)))
            (oset iinfo normal
                  (norm-v3 (sub-v3 (oref iinfo position) (oref sp center))))
            iinfo)))))))

(defclass camera3 () ((eye :initarg :eye)
                      (front :initarg :front)
                      (up)
                      (fov :initarg :fov)
                      (right)
                      (fovscale)
                      (refup :initarg :refup)))

(defun makei-camera3 (eye front up fov)
  (let ((cm (camera3 nil :eye eye :front front :refup up :fov fov)))
    (with-slots (eye front up fov right fovscale refup) cm
      (setq right (vxv-v3 front refup))
      (setq up (vxv-v3 right front))
      (setq fovscale (* (tan (/ (* fov 0.5 pi) 180) ) 2))
      cm)))
(defun gen-ray (cm x y)
  (with-slots (eye front up fov right fovscale refup) cm
    (let ((r (mul-v3 right (* (- x 0.5) fovscale)))
          (u (mul-v3 up (* (- y 0.5) fovscale))))
    (makei-r3 eye (norm-v3 (add-v3 (add-v3 front r) u))))))

(defun render-depth () ;& render normal
  (interactive)
  (let* ((w 50) (h 50)
         (ppm (ppm-make w h))
         (pixels (oref ppm pixels))
         (cm (makei-camera3 (makei-v3  0 10 10) (makei-v3 0 0 -1) (makei-v3 0 1 0) 90))
         (scene (makei-sphere3 (makei-v3 0. 10. -10.) 10.0))
         sx
         sy
         (max-depth 20)
         )
    (loop for x from 0 to (1- w) do
          (setq sx (/ (float x) w))
          (loop for y from 0 to (1- h) do
                (setq sy (- 1 (/ (float y) h)))
                (let* ((ray (gen-ray cm sx sy))
                       (iinfo (intersect3 scene ray))
                       (normal (oref iinfo normal)))
                  (if (oref iinfo geometry)
                      ;; ; depth
                      (let ((depth (- 255 (min 255 (* (/ (oref iinfo distance) max-depth) 255)))) )
                        (ppm-pixels-plot pixels x y (ppm-rgb depth depth depth) w))
                      ;; ; normal
                      ;; (ppm-pixels-plot pixels x y (ppm-rgb
                      ;;                              (* 128 (1+ (v3-x normal)))
                      ;;                              (* 128 (1+ (v3-y normal)))
                      ;;                              (* 128 (1+ (v3-z normal)))
                      ;;                              )

                      ;;                  w)
                    ))
                ))
    (ppm-show ppm)
    (message "done!")))

(defclass c3 () ((r :initarg :r)
                 (g :initarg :g)
                 (b :initarg :b))) ;color
(defun makei-c3 (r g b)
  (c3 nil :r (float r) :g (float g) :b (float b)))
(defun add-c3 (pc3 other)
  (with-slots (r g b) pc3
    (with-slots ((ro r) (go g) (bo b)) other
      (makei-c3 (+ r ro) (+ g go) (+ b bo)))))
(defun mul-c3 (pc3 s)
  (with-slots (r g b) pc3
    (makei-c3 (* r s) (* g s) (* b s))))
(defun mod-c3 (pc3 other)
  (with-slots (r g b) pc3
    (with-slots ((ro r) (go g) (bo b)) other
      (makei-c3 (* r ro) (* g go) (* b bo)))))
(defun sat-c3 (pc3)
  (with-slots (r g b) pc3
    (setq r (min r 1.0))
    (setq g (min g 1.0))
    (setq b (min b 1.0))
    pc3))
(defun rgb-c3 (pc3)
  (with-slots (r g b) pc3
    (ppm-rgb (* r 255) (* g 255) (* b 255))))
(defconst c3-black (makei-c3 0 0 0))
(defconst c3-white (makei-c3 1. 1. 1.))
(defconst c3-red (makei-c3 1. 0 0))
(defconst c3-green (makei-c3 0 1. 0))
(defconst c3-blue (makei-c3 0 0 1.))

(defclass checker-material () ((scale :initarg :scale)
                               (reflectiveness :initarg :reflectiveness)))
(defun makei-checker (scale reflectiveness)
  (checker-material nil :scale scale :reflectiveness reflectiveness))

(eval-when-compile
(defmethod sample3 ((mat checker-material) ray position normal)
  (if (= (abs (mod (+ (ffloor (* (oref position x) 0.1))
                    (ffloor (* (oref position z) (oref mat scale))))
                 2))
         1)
      c3-black
    c3-white))
)

(defclass phong-material () ((diffuse :initarg :diffuse)
                             (specular :initarg :specular)
                             (shininess :initarg :shininess)
                             (reflectiveness :initarg :reflectiveness)))
(defun makei-phong (diffuse specular shininess reflectiveness)
  (phong-material nil :diffuse diffuse :specular specular :shininess shininess :reflectiveness reflectiveness))

(defconst ppm-light-dir (norm-v3 (makei-v3 1 1 1)))
(defconst ppm-light-c3 c3-white)
(eval-when-compile
(defmethod sample3 ((mat phong-material) ray position normal)
  (let* ((NdotL (dot-v3 normal ppm-light-dir))
         (H (norm-v3 (sub-v3 ppm-light-dir (oref ray direction))))
         (NdotH (dot-v3 normal H))
         (diffuseTerm (mul-c3 (oref mat diffuse) (max NdotL 0.0)))
         (specularTerm (mul-c3 (oref mat specular)
                            (expt (max NdotH 0)
                                 (oref mat shininess)))))
    (mod-c3 ppm-light-c3 (sat-c3 (add-c3 diffuseTerm specularTerm)))
    ))
)

(defclass plane3 () ((normal :initarg :normal)
                     (d :initarg :d)
                     (material)
                     (position)))
(defun makei-plane3 (normal d)
  (let ((p (plane3 nil :normal normal :d d)))
    (oset p position (mul-v3 (oref p normal) d))
    p))
(eval-when-compile              
(defmethod intersect3 ((p plane3) ray)
  (with-slots (normal d position) p
    (let* ((a (dot-v3 (oref ray direction) normal)))
      (if (>= a 0)
          isect-no-hit
        (let ((b (dot-v3 normal (sub-v3 (oref ray origin) position)))
              (iinfo (makei-isect-info)))
          (oset iinfo geometry p)
          (oset iinfo distance (- (/ b a)))
          (oset iinfo position (pt-at-r3 ray (oref iinfo distance)))
          (oset iinfo normal normal)
          iinfo)))))
)


(defclass union3 () ((geometries :initarg :geometries)))
(defun makei-union3 (geometries)
  (union3 nil :geometries geometries))

(defconst BIG-DISTANCE 1.0e20)
(loop for a across [1 2 3 [4 5]] collect a  )
(eval-when-compile
(defmethod intersect3 ((p union3) ray)
  (let ((min-distance BIG-DISTANCE)
        iinfo
        (min-iinfo isect-no-hit))
    (loop for o across (oref p geometries) do
          (setq iinfo (intersect3 o ray))
          (when (and (oref iinfo geometry) (< (oref iinfo distance) min-distance))
            (setq min-distance (oref iinfo distance))
            (setq min-iinfo iinfo)))
    min-iinfo))
)

(defun ray-tracing (scene ray max-depth)
  (let ((iinfo (intersect3 scene ray)))
    (with-slots (geometry normal position) iinfo
      (if geometry
          (let ((reflect (oref (oref geometry material) reflectiveness))
                (color (sample3 (oref geometry material) ray position normal)))
            (if (and (> reflect 0) (> max-depth 0))
                (let* ((r (add-v3 (mul-v3 normal (* -2
                                                    (dot-v3 normal (oref ray direction))))  (oref ray direction)))
                       (ray (makei-r3 position r))
                       (reflect-color (ray-tracing scene ray (1- max-depth))))
                  (setq color (add-c3 color (mul-c3 reflect-color reflect)))
                  color)
              color))
        c3-black))))

(defun render-material () ;& render ray-racing
  (interactive)
  (let* ((w 80) (h 80)
         (ppm (ppm-make w h))
         (pixels (oref ppm pixels))
         (cm (makei-camera3 (makei-v3  0 10 10) (makei-v3 0 0 -1) (makei-v3 0 1 0) 90))
         (sp (makei-sphere3 (makei-v3 -8. 11. -10.) 5.0))
         (sp1 (makei-sphere3 (makei-v3 7. 10. -10.) 6.0))
         (pl (makei-plane3 (makei-v3 0. 1. 0.) 0.0))
         scene
         sx
         sy
         (max-depth 20)
         )
    (oset sp material (makei-phong c3-red c3-white 16.0 0.25))
    (oset sp1 material (makei-phong c3-blue c3-white 14.0 0.4))
    (oset pl material (makei-checker 0.1 0.5))
    (setq scene (makei-union3 (vector sp pl sp1)))
    (loop for x from 0 to (1- w) do
          (setq sx (/ (float x) w))
          (loop for y from 0 to (1- h) do
                (setq sy (- 1 (/ (float y) h)))
                (let* ((ray (gen-ray cm sx sy))
                       (iinfo (intersect3 scene ray))
                       (normal (oref iinfo normal)))
                  (if (oref iinfo geometry)
                      ;(let ((c3 (sample3 (oref (oref iinfo geometry) material) ray (oref iinfo position) (oref iinfo normal)) ))
                      (let ((c3 (ray-tracing scene ray 3)))
                        (ppm-pixels-plot pixels x y (rgb-c3 c3) w))
                    ))
                ))
    (ppm-show ppm)
    (message "done!")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Test Ground END.

;BMP loader

(defun ppm--read (file beg size)
  (with-temp-buffer (insert-file-contents-literally file nil beg (+ beg size))
                    (buffer-string)))
(defun ppm--int4 (s)
  (logior (get-byte 0 s) (lsh (get-byte 1 s) 8) (lsh (get-byte 2 s) 16) (lsh (get-byte 3 s) 24)))
(defun ppm--int3 (s)
  (logior (get-byte 0 s) (lsh (get-byte 1 s) 8) (lsh (get-byte 2 s) 16)))
(defun ppm--int1 (s)
  (get-byte 0 s))
(defun ppm-from-bmp (filename)
  "make a ppm object from a .bmp file. only 24bit .bmp is supportted."
  (let* ((offset (ppm--int4 (ppm--read filename 10 4)))
         (headsize (ppm--int4 (ppm--read filename 14 4)))
         (width (ppm--int4 (ppm--read filename 18 4)))
         (height (ppm--int4 (ppm--read filename 22 4)))
         (rowbytes (lsh (lsh (+ (* width 3) 3) -2) 2))
         (ppm (ppm-make width height))
         (bytes (ppm--read filename offset (* rowbytes height)))
         (idx)
         (pixels (oref ppm pixels))
         )
    (loop for y from 0 to (1- height) do
          (loop for x from 0 to (1- width) do
                (setq idx (+ (* rowbytes y) (* 3 x)))
                (ppm-pixels-plot pixels x (- height y 1) (ppm-rgb (get-byte (+ idx 2) bytes)
                                                                  (get-byte (+ idx 1) bytes)
                                                                  (get-byte idx bytes))
                                 width)
                ))
    ppm
    ))

(defun ppm-from-ppm (filename)
  (let (binary w h appm) 
    (save-excursion 
      (switch-to-buffer (generate-new-buffer  "*ppm reader*"))
      (insert-file-contents-literally filename t)
      (goto-char (point-min))
      (when (looking-at "P6") (setq binary t))
      (when (search-forward-regexp "\\b[0-9]+\\b" (buffer-end 1))
        (setq w (number-at-point)))
      (when (search-forward-regexp "\\b[0-9]+\\b" (buffer-end 1))
        (setq h (number-at-point)))
      (search-forward-regexp "\\b[0-9]+\\b" (buffer-end 1))
      (unless (= (number-at-point) 255)
        (error "not a 24bit PPM file"))
      (setq appm (ppm-make w h))
      (if binary (progn
                   (oset appm pixels (vconcat (buffer-substring-no-properties (1+ (point))  (buffer-end 1))))
                   (kill-buffer "*ppm reader*")
                   appm)
        (let ((lst '()))
          (while (not (eobp))
            (when (search-forward-regexp "\\b[0-9]+\\b" (buffer-end 1))
              (setq lst (cons (number-at-point) lst)))
            (forward-char))
          (oset appm pixels (vconcat (reverse lst)))
          (kill-buffer "*ppm reader*")
          appm)))))

(defun ppm-pixels-get (pxs x y width)
  (let ((idx (* (+ (* y width) x) 3)))
    (ppm-rgb (aref pxs idx) (aref pxs (+ 1 idx)) (aref pxs (+ 2 idx)))))

(defun ppm-clamp-color (c)
  (if (<= c 0)
      0
    (logand c 255)))
(defun ppm-color-avg (c1 c2 u)
  (let ((r1 (ppm-clamp-color (lsh c1 -16)))
        (g1 (ppm-clamp-color (lsh c1 -8)))
        (b1 (ppm-clamp-color c1))
        (r2 (ppm-clamp-color (lsh c2 -16)))
        (g2 (ppm-clamp-color (lsh c2 -8)))
        (b2 (ppm-clamp-color c2)))
    (ppm-rgb (+ (* u r2) (* (- 1 u) r1))
             (+ (* u g2) (* (- 1 u) g1))
             (+ (* u b2) (* (- 1 u) b1)))))

(defun ppm-scale (src-ppm dst-width dst-height &optional aspect)
  (when aspect
    (setq dst-height (/ (* dst-width (oref src-ppm height)) (oref src-ppm width)))
    )
  (let* ((ppm (ppm-make dst-width dst-height))
         (src-pxs (oref src-ppm pixels))
         (src-width (oref src-ppm width))
         (src-height (oref src-ppm height))
         (pxs (oref ppm pixels))
         )
    (loop for x from 0 to (1- dst-width) do
          (loop for y from 0 to (1- dst-height) do
                (ppm-pixels-plot pxs x y
                                 (ppm-pixels-get src-pxs (/ (* x src-width) dst-width) (/ (* y src-height) dst-height) src-width)
                                 dst-width)))
    ppm))

(defun ppm-scale-bilinear (src-ppm dst-width dst-height &optional aspect)
  (when aspect
    (setq dst-height (/ (* dst-width (oref src-ppm height)) (oref src-ppm width)))
    )
  (let* ((ppm (ppm-make dst-width dst-height))
         (src-pxs (oref src-ppm pixels))
         (src-width (oref src-ppm width))
         (src-height (oref src-ppm height))
         (pxs (oref ppm pixels))
         )
    (loop for x from 0 to (1- dst-width) do
          (loop for y from 0 to (1- dst-height) do
                (let* ((xt (/ (* (float x) src-width) dst-width))
                       (xp1 (floor xt))
                       (xp2 (min (1+ xp1) (1- src-width)))
                       (xi  (- xt xp1))
                       (yt (/ (* (float y) src-height) dst-height))
                       (yp1 (floor yt))
                       (yp2 (min (1+ yp1) (1- src-height)))
                       (yi  (- yt yp1))
                       (c1 (ppm-color-avg (ppm-pixels-get src-pxs xp1 yp1 src-width)
                                          (ppm-pixels-get src-pxs xp2 yp1 src-width)
                                          xi))
                       (c2 (ppm-color-avg (ppm-pixels-get src-pxs xp1 yp2 src-width)
                                          (ppm-pixels-get src-pxs xp2 yp2 src-width)
                                          xi))
                       (c3 (ppm-color-avg c1 c2 yi)))
                  (ppm-pixels-plot pxs x y c3 dst-width))))
    ppm))


(defun ppm-cubic-interp (p0 p1 p2 p3 x)
  (let ((a (+ (* 0.5 (- p0)) (* 1.5 p1)  (* 1.5 (- p2)) (* 0.5 p3)))
        (b (+ p0 (* 2.5 (- p1))  (* 2.0 p2) (* 0.5 (- p3))))
        (c (+ (* 0.5 (- p0)) (* 0.5 p2)))
        (d p1)
        (x2 (* x x)))
    (+ (* a (* x2 x)) (* b x2) (* c x) d)))

(defun ppm-color-cubic (c1 c2 c3 c4 u)
  (let ((r1 (ppm-clamp-color (lsh c1 -16)))
        (g1 (ppm-clamp-color (lsh c1 -8)))
        (b1 (ppm-clamp-color c1))
        (r2 (ppm-clamp-color (lsh c2 -16)))
        (g2 (ppm-clamp-color (lsh c2 -8)))
        (b2 (ppm-clamp-color c2))
        (r3 (ppm-clamp-color (lsh c3 -16)))
        (g3 (ppm-clamp-color (lsh c3 -8)))
        (b3 (ppm-clamp-color c3))
        (r4 (ppm-clamp-color (lsh c4 -16)))
        (g4 (ppm-clamp-color (lsh c4 -8)))
        (b4 (ppm-clamp-color c4)))
    (ppm-rgb (max 0 (ppm-cubic-interp r1 r2 r3 r4 u))
             (max 0 (ppm-cubic-interp g1 g2 g3 g4 u))
             (max 0 (ppm-cubic-interp b1 b2 b3 b4 u)))))

(defun ppm-scale-bicubic (src-ppm dst-width dst-height &optional aspect)
  (when aspect
    (setq dst-height (/ (* dst-width (oref src-ppm height)) (oref src-ppm width)))
    )
  (let* ((ppm (ppm-make dst-width dst-height))
         (src-pxs (oref src-ppm pixels))
         (src-width (oref src-ppm width))
         (src-height (oref src-ppm height))
         (pxs (oref ppm pixels))
         )
    (loop for x from 0 to (1- dst-width) do
          (loop for y from 0 to (1- dst-height) do
                (let* ((xt (/ (* (float x) src-width) dst-width))
                       (xp1 (floor xt))
                       (xp0 (max (1- xp1) 0))
                       (xp2 (min (1+ xp1) (1- src-width)))
                       (xp3 (min (1+ xp2) (1- src-width)))
                       (xi  (- xt xp1))
                       (yt (/ (* (float y) src-height) dst-height))
                       (yp1 (floor yt))
                       (yp0 (max (1- yp1) 0))
                       (yp2 (min (1+ yp1) (1- src-height)))
                       (yp3 (min (1+ yp2) (1- src-height)))
                       (yi  (- yt yp1))
                       
                       (c0 (ppm-color-cubic (ppm-pixels-get src-pxs xp0 yp0 src-width)
                                            (ppm-pixels-get src-pxs xp1 yp0 src-width)
                                            (ppm-pixels-get src-pxs xp2 yp0 src-width)
                                            (ppm-pixels-get src-pxs xp3 yp0 src-width)
                                            xi))
                       (c1 (ppm-color-cubic (ppm-pixels-get src-pxs xp0 yp1 src-width)
                                            (ppm-pixels-get src-pxs xp1 yp1 src-width)
                                            (ppm-pixels-get src-pxs xp2 yp1 src-width)
                                            (ppm-pixels-get src-pxs xp3 yp1 src-width)
                                            xi))
                       (c2 (ppm-color-cubic (ppm-pixels-get src-pxs xp0 yp2 src-width)
                                            (ppm-pixels-get src-pxs xp1 yp2 src-width)
                                            (ppm-pixels-get src-pxs xp2 yp2 src-width)
                                            (ppm-pixels-get src-pxs xp3 yp2 src-width)
                                            xi))
                       (c3 (ppm-color-cubic (ppm-pixels-get src-pxs xp0 yp3 src-width)
                                            (ppm-pixels-get src-pxs xp1 yp3 src-width)
                                            (ppm-pixels-get src-pxs xp2 yp3 src-width)
                                            (ppm-pixels-get src-pxs xp3 yp3 src-width)
                                            xi))
                       (c4 (ppm-color-cubic c0 c1 c2 c3 yi)))
                  
                  (ppm-pixels-plot pxs x y c4 dst-width))))
    ppm))

(defun gamma-adjust (appm gamma)
  (with-slots (width height pixels) appm
    (let* ((max-value (expt 255 gamma)))
      (loop for idx from 0 to (* (1- width) (1- height) 3) do
            (let ((oldv (aref pixels idx)))
              (aset pixels idx (floor
                                (* (/ (expt oldv gamma) max-value) 255)))
              ))))
  appm)
