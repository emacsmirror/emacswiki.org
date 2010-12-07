;;; celssc-mode.el --- Major mode for editing celestia script files
;; 
;; Filename: celssc-mode.el
;; Description: Major mode for editing celestia's catalog files.
;; Author: Olexandr Sydorchuk
;; Maintainer: 
;; Copyright Author: Olexandr Sydorchuk <olexandr_syd [at] users dot sourceforge dot net>.
;; Created: Sun Dec  7 22:34:36 2008
;; Version: 
;; Last-Updated: Wed Nov 24 22:16:39 2010 (0 UTC)
;;           By: Olexandr Sydorchuk
;;     Update #: 3
;; URL: 
;; Keywords: celestia major-mode
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; Major mode for editing celestia catalog files.
;; This minor mode is based on 
;; tutorial that can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html
;;
;; If celssc-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;;
;; (autoload 'celssc-mode "celssc-mode" "Major mode for editing celesita's solar system catalog files" t)
;; (setq auto-mode-alist (cons '("\\.ssc" . celssc-mode)
;;                                   auto-mode-alist))
;; (autoload 'celssc-mode "celssc-mode" "Major mode for editing celesita's star catalog files" t)
;; (setq auto-mode-alist (cons '("\\.stc" . celssc-mode)
;;                                   auto-mode-alist))
;; (autoload 'celssc-mode "celssc-mode" "Major mode for editing celesita's deep sky catalog files" t)
;; (setq auto-mode-alist (cons '("\\.dsc" . celssc-mode)
;;                                   auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 24-Nov-2010    Olexandr Sydorchuk  
;;    Fix indent
;; 9-Feb-2009    Olexandr Sydorchuk  
;;    Some highlight of patch for ScriptedOrbits 
;;    (http://www.shatters.net/forum/viewtopic.php?f=9&t=13428)
;; 7-Dec-2008    Olexandr Sydorchuk
;;    Fix indent 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; hs-minor-mode (hideshow)
(add-to-list 'hs-special-modes-alist '(celssc "{" "}" "/[*/]" nil nil))

(defvar celssc-mode-hook nil)
(defvar celssc-mode-map
  (let ((celssc-mode-map (make-keymap)))
    (define-key celssc-mode-map "\C-j" 'newline-and-indent)
    (define-key celssc-mode-map "\C-m" 'newline-and-indent)
    (define-key celssc-mode-map "}" 'celssc-paren-close)
    (define-key celssc-mode-map "]" 'celssc-parenb-close)
    celssc-mode-map)
  "Keymap for celestia major mode")

(defconst celssc-imenu-modifer-expression
  "\\(Add\\|Replace\\|Modify\\|\\)[ \t]*"
  )

(defconst celssc-imenu-name-expression
  "[\"]\\(\\(\\sw:\\|\\sw_\\|\\sw\\.\\|\\sw\\|[ ]\\)+\\)[\"]"
  )

(defconst celssc-imenu-class-expression
  "[ \t]+.*[\n][ \t\n]*{[ \t\n]*.*Class[ \t]+[\"]"
  )

(defvar celssc-imenu-generic-expression
  `(
    ("Barycenters"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "Barycenter[ \t]+"
       "[0-9 \t]*"                    ;skip number
       celssc-imenu-name-expression
       ) 2)
    ("Star"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "Star[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("Galaxy"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "Galaxy[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("Nebula"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "Nebula[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("OpenCluster"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "OpenCluster[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("Globular"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "Globular[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("AltSurface"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "AltSurface[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("Locations"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "Location[ \t]+" celssc-imenu-name-expression
       ) 2)
    ("Reference Points"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       "ReferencePoint[ \t]+" celssc-imenu-name-expression
       ) 2)
    ;; All objects, but no galaxy, openclusters, locations, nebulars, globulars, ref points
    (
     "all objects" 
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression
       celssc-imenu-name-expression
       ) 2)
    ("Comets"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "comet" "\""
       ) 2)
    ("Moons"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "moon" "\""
       ) 2)
    ("Planets"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "planet" "\""
       ) 2)
    ("Dwarf planets"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "dwarfplanet" "\""
       ) 2)
    ("Minor moons"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "minormoon" "\""
       ) 2)
    ("Asteroids"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "asteroid" "\""
       ) 2)
    ("Spacecrafts"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "spacecraft" "\""
       ) 2)
    ("Surface feature"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "surfacefeature" "\""
       ) 2)
    ("Components"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "component" "\""
       ) 2)
    ("Diffuse"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "diffuse" "\""
       ) 2)
    ("Invisible"
     ,(concat
       "^[ \t]*"
       celssc-imenu-modifer-expression 
       celssc-imenu-name-expression
       celssc-imenu-class-expression "invisible" "\""
       ) 2)
    )
  "Imenu generic expression for celssc-mode.  See `imenu-generic-expression'."
  ) 

(defconst celssc-font-lock-keywords-1
  (list
   (list
    (concat "\\<\\("
	    (regexp-opt '(
			  "true" "false" 
			  "Add" "Replace" "Modify"
			  "planet" "dwarfplanet" "moon" "minormoon"
			  "comet" "asteroid" "spacecraft" "invisible"
			  "surfacefeature" "component" "diffuse"
			  ) t)
            "\\)\\>")
    1 'font-lock-constant-face 'prepend)    
   )
  "Constant expressions for celestia mode.")

(defconst celssc-font-lock-keywords-2
  (append celssc-font-lock-keywords-1

	  (list
	   (list
	    (concat "\\<\\("
		    (regexp-opt '(
				  "AbsMag" "Absorption" "Acceleration" "AddonPath" "Albedo" "Angle" "AppMag" "ArgOfPericenter"
				  "AscendingNode" "Axis" "BaseFrame" "Beginning" "BlendTexture"
				  "BoundingRadius" "BumpHeight" "BumpMap" "CoreRadius" "CustomTemplate" "Center" "Class"
				  "Clickable" "CloudHeight" "CloudMap" "CloudNormalMap" "CloudShadowDepth"
				  "CloudSpeed" "Color" "CompressTexture" "CustomOrbit" "Dec" "Detail"
				  "Distance" "DoublePrecision" "Eccentricity" "Emissive" "EndColor"
				  "EndOpacity" "EndSize" "Ending" "Epoch" "EquatorAscendingNode" "Frame"
				  "Freeze" "Function"  "HazeColor" "HazeDensity" "Heading" "Height" "Importance"
				  "Inclination" "InfoURL" "Inner" "Interpolation" "KingConcentration" "Kernel" "LabelColor"
				  "Lifetime" "LongLat" "Loop" "LongOfPericenter" "Lower" "LunarLambert" "Mass"
				  "MaxAngle" "MaxRotationRate" "MaxSpeed" "MeanAnomaly" "MeanLongitude" "MeridianAngle"
				  "Mesh" "MeshCenter" "MeshScale" "Mie" "MieAsymmetry" "MieScaleHeight" "MinAngle"
				  "MinRotationRate" "MinSpeed" "Module" "NightLightRadiance" "NightTexture"
				  "NormalMap" "NormalizeMesh" "Object" "Oblateness" "Obliquity" "Observer"
				  "OrbitBarycenter" "OrbitColor" "Orientation"
				  "Origin" "Outer" "OverlayTexture" "PericenterDistance"
				  "Period" "Planetocentric" "Planetographic" "Point1"  "Point2" "Position"
				  "PrecessionPeriod" "PrecessionRate" "RA" "Radius" 
				  "Rate" "Rayleigh" "Rectangular" "Roll" "RotationEpoch"
				  "RotationOffset" "RotationPeriod" "SampledOrbit" "SemiAxes" "SemiMajorAxis" "Sigma"
				  "Size" "Sky" "Source" "SpectralType" "SpecularColor" "SpecularPower" "SpecularTexture"
				  "SpiceOrbit" "StartColor"  "StartOpacity" "StartSize" "Sunset" "Target" "Texture"
				  "Tilt" "TimeBase" "Type" "Upper" "Vector" "Visible"
				  ) t)
		    "\\)\\>")
	    1 'font-lock-variable-name-face )
	   (list
	    (concat "\\<\\("
		    (regexp-opt '(
				  "Atmosphere" "BodyFixed" "BodyFrame" "Box" "Cone" "Constant"
				  "ConstoantVector" "CustomRotation" "EclipticJ2000"
				  "EllipsoidSurface" "EllipticalOrbit" "Emitter" "EquatorJ2000"
				  "FixedAttitude" "FixedPosition" "FixedRotation"
				  "Frame" "GaussianDisc" "InitialPosition" "InitialVelocity"
				  "Line" "MeanEquator" "OrbitFrame" "PrecessingRotation"
				  "Primary" "RelativePosition" "Rings" "SampledOrientation"
				  "SampledTrajectory" "ScriptedOrbit" "ScriptedRotation"
				  "Secondary" "SpiceOrbit" "SpiceRotation"
				  "Timeline" "Topocentric" "TwoVector" "UniformRotation"
				  ) t)
		    "\\)\\>")
	    1 'font-lock-function-name-face )
	   ))
  "Keywords (options) to highlight in celestia mode.")

(defconst celssc-font-lock-keywords-3
  (append celssc-font-lock-keywords-2
	  (list
	   (list
	    (concat "\\<\\("
		    (regexp-opt '(
				  "AltSurface" "Barycenter" "Body" "Galaxy"
				  "Globular" "Location" "Nebula" "OpenCluster"
				  "ReferencePoint" "Star" "SurfaceObject"
				  ) t)
		    "\\)\\>")
	    1 'font-lock-keyword-face ) 
	   ))
  "Additional Keywords (types) to highlight in celestia mode.")

(defconst celssc-font-lock-keywords-4
  (append celssc-font-lock-keywords-3
	  (list
	   (list
	    (concat "\\<\\("
		    "Pos[0-9]+"
		    "\\)\\>")
	    1 'font-lock-variable-name-face ) 
	   ))
  "Additional Pos# in ScriptedOrbit to highlight in celestia mode.")

(defvar celssc-font-lock-keywords celssc-font-lock-keywords-4
  "Default highlighting keywords expressions for celestia mode.")

(defun celssc-indent-line ()
  "Indent current line as celssc code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*\\(}\\|\\]\\)")
          (if (looking-at "^[ \t]*\\(}\\|\\]\\)")
              (progn
                (save-excursion
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) tab-width)))
                (if (< cur-indent 0)
                    (setq cur-indent 0)))
            (progn
              (save-excursion
                (forward-line -1))))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*\\(}\\|\\]\\)")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
	      (if (looking-at "^[ \t]*[a-zA-Z0-9 \t]*\\({\\|\\[\\)\\s-*$")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))
              ))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defvar celssc-mode-syntax-table
  (let ((celssc-mode-syntax-table (make-syntax-table)))
    
    ;; This is added so entity names with underscores can be more easily parsed
    (modify-syntax-entry ?_ "w" celssc-mode-syntax-table)
    (modify-syntax-entry ?- "w" celssc-mode-syntax-table)
    (modify-syntax-entry ?. "w" celssc-mode-syntax-table)
    (modify-syntax-entry ?/ "w" celssc-mode-syntax-table)
    (modify-syntax-entry ?& "w" celssc-mode-syntax-table)	
    (modify-syntax-entry ?#
			 "<" celssc-mode-syntax-table)
    (modify-syntax-entry ?\n
			 ">" celssc-mode-syntax-table)
    celssc-mode-syntax-table)
  "Syntax table for celssc-mode")

(defun celssc-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map celssc-mode-map)
  (set-syntax-table celssc-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(celssc-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'celssc-indent-line)  
  (setq major-mode 'celssc-mode)
  (setq mode-name "celssc")
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'imenu-generic-expression)
       celssc-imenu-generic-expression) 
  (run-hooks 'celssc-mode-hook))

(defun celssc-paren-close ()
  (interactive)
  (newline-and-indent)
  (delete-backward-char 1)
  (insert "}"))

(defun celssc-parenb-close ()
  (interactive)
  (newline-and-indent)
  (delete-backward-char 1)
  (insert "]"))



(provide 'celssc-mode)

;;; celssc-mode.el ends here
