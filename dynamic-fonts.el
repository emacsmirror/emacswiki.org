;;; dynamic-fonts.el --- Set faces based on available fonts
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/dynamic-fonts
;; URL: http://raw.github.com/rolandwalker/dynamic-fonts/master/dynamic-fonts.el
;; Version: 0.6.4
;; Last-Updated: 22 Oct 2013
;; EmacsWiki: DynamicFonts
;; Keywords: faces, frames
;; Package-Requires: ((font-utils "0.7.0") (persistent-soft "0.8.8") (pcache "0.2.3"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'dynamic-fonts)
;;
;;     (dynamic-fonts-setup)     ; finds "best" fonts and sets faces:
;;                               ; default, fixed-pitch, variable-pitch
;;
;; Explanation
;;
;; Dynamic-fonts.el makes font configuration more portable between
;; machines.  When Emacs is starting up, dynamic-fonts chooses fonts
;; for your basic faces based on which fonts are actually available
;; on your system.
;;
;; You may set a list of fonts in order of preference using customize.
;;
;; See Also
;;
;;     M-x customize-group RET dynamic-fonts RET
;;     M-x customize-group RET font-utils RET
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Requires font-utils.el
;;
;; Bugs
;;
;;     Checking for font availability is slow on most systems.  This
;;     library can add up to several seconds to startup time.
;;     Workaround: where supported, font information will be cached
;;     to disk.  See customization options for font-utils.
;;
;; TODO
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requirements

(autoload 'font-utils-first-existing-font "font-utils" "Return the (normalized) first existing font name from FONT-NAMES.")

;;; customizable variables

;;;###autoload
(defgroup dynamic-fonts nil
  "Set faces based on available fonts."
  :version "0.6.4"
  :link '(emacs-commentary-link :tag "Commentary" "dynamic-fonts")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/dynamic-fonts")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/DynamicFonts")
  :prefix "dynamic-fonts-"
  :group 'faces)

(defcustom dynamic-fonts-set-proportional-faces '(variable-pitch)
  "List of faces to set with the best-found proportional font.

It is best to keep this list small, and let other proportional
faces inherit from these faces."
  :type '(repeat face)
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-set-monospace-faces '(fixed-pitch default)
  "List of faces to set with the best-found monospace font.

It is best to keep this list small, and let other monospace
faces inherit from these faces."
  :type '(repeat face)
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-set-alternatives t
  "Whether to set `face-font-family-alternatives'.

If non-nil, the default entries in `face-font-family-alternatives'
will be supplemented with the preferred monospace and proportional
fonts set in customize."
  :type 'boolean
  :group 'dynamic-fonts)

;;;###autoload
(defgroup dynamic-fonts-preferred-fonts nil
  "Preferred font choices"
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-preferred-proportional-fonts '(
                                                        "Lucida Grande"
                                                        "Segoe UI"
                                                        "DejaVu Sans"
                                                        "Bitstream Vera"
                                                        "Tahoma"
                                                        "Verdana"
                                                        "Helvetica"
                                                        "Arial Unicode MS"
                                                        "Arial"
                                                        )
"A list of proportional fonts in order of preference.

The first font which is present on the system will be used as the
default for variable-width faces."
  :type '(repeat string)
  :group 'dynamic-fonts-preferred-fonts)

(defcustom dynamic-fonts-preferred-proportional-point-size 12
  "Basic proportional fonts will be set at this size if possible."
  :type 'number
  :group 'dynamic-fonts-preferred-fonts)

(defcustom dynamic-fonts-preferred-monospace-fonts '(
                                                     "Monaco"
                                                     "Consolas"
                                                     "Menlo"                        ; based on Bitstream Vera
                                                     "DejaVu Sans Mono"             ; based on Bitstream Vera
                                                     "Droid Sans Mono Pro"
                                                     "Droid Sans Mono"
                                                     "Inconsolata"
                                                     "Source Code Pro"
                                                     "Lucida Console"
                                                     "Envy Code R"
                                                     "Andale Mono"
                                                     "Lucida Sans Typewriter"
                                                     "monoOne"
                                                     "Lucida Typewriter"
                                                     "Panic Sans"                   ; based on Bitstream Vera
                                                     "Bitstream Vera Sans Mono"
                                                     "HyperFont"
                                                     "PT Mono"
                                                     "Ti92Pluspc"
                                                     "Excalibur Monospace"
                                                     "Courier New"
                                                     "Courier"
                                                     "Cousine"
                                                     "Fira Mono"
                                                     "Lekton"
                                                     "Ubuntu Mono"
                                                     "Liberation Mono"
                                                     "BPmono"
                                                     "Free Mono"
                                                     "Anonymous Pro"
                                                     "ProFont"
                                                     "ProFontWindows"
                                                     "Latin Modern Mono"
                                                     "Code 2002"
                                                     "ProggyCleanTT"
                                                     "ProggyTinyTT"
                                                     )
"A list of monospace fonts in order of preference.

The first font which is present on the system will be used as the
default for fixed-width faces."
  :type '(repeat string)
  :group 'dynamic-fonts-preferred-fonts)

(defcustom dynamic-fonts-preferred-monospace-point-size 12
  "Basic monospace fonts will be set to this size if possible."
  :type 'number
  :group 'dynamic-fonts-preferred-fonts)

(defun dynamic-fonts-set-face (face families &optional point-size)
  "Set FACE to the first existing font name in FAMILIES.

Point size may be specified by the optional variable POINT-SIZE.
If POINT-SIZE is not present, size may be specified by a
fontconfig-style specification on the members of FAMILIES.  In
order of preference:

    Font Name:size=<points>
    Font Name-<points>"
  (when (display-multi-font-p)
    (save-match-data
      (let ((font-name (font-utils-first-existing-font families 'no-normalize)))
        (when font-name
          (when (and (not point-size)
                     (or (string-match ":.*\\<size=\\([0-9.]+\\)" font-name)
                         (string-match "-\\([0-9.]+\\)\\(?::\\|\\'\\)" font-name)))
            (setq point-size (string-to-number
                              (match-string-no-properties 1 font-name))))
          (setq font-name (font-utils-first-existing-font (list font-name))) ; normalize
          (when (eq face 'default)
            (let ((args (list t t)))
              (when (not (and (>= emacs-major-version 24) (>= emacs-minor-version 1)))
                (pop args))
              (apply 'set-frame-font font-name args)))
          (set-face-attribute face nil :family font-name)
          (when point-size
            (set-face-attribute face nil :height (round (* 10 point-size)))))))))

;;; interactive commands

;;;###autoload
(defun dynamic-fonts-setup ()
  "Set up `fixed-pitch', `variable-pitch', and `default' faces.

The font face and size is determined dynamically, by comparing
the following values

    `dynamic-fonts-preferred-monospace-fonts'
    `dynamic-fonts-preferred-monospace-point-size'
    `dynamic-fonts-preferred-proportional-fonts'
    `dynamic-fonts-preferred-proportional-point-size'

with the fonts available on your system.

When `dynamic-fonts-set-alternatives' is set, also amends
the standard value of `face-font-family-alternatives', providing
the values above as alternatives."
  (interactive)
  (when (display-multi-font-p)

    (dolist (face dynamic-fonts-set-monospace-faces)
      (dynamic-fonts-set-face face dynamic-fonts-preferred-monospace-fonts dynamic-fonts-preferred-monospace-point-size))

    (dolist (face dynamic-fonts-set-proportional-faces)
      (dynamic-fonts-set-face face dynamic-fonts-preferred-proportional-fonts  dynamic-fonts-preferred-proportional-point-size))

    (when dynamic-fonts-set-alternatives
      (setq face-font-family-alternatives
            (list (append '("Monospace") dynamic-fonts-preferred-monospace-fonts '("courier" "fixed"))
                  (append '("courier" "Courier New" "Lucida Sans Typewriter" "CMU Typewriter Text") dynamic-fonts-preferred-monospace-fonts '("fixed"))
                  (append '("Sans Serif" "helv" "helvetica" "arial") dynamic-fonts-preferred-proportional-fonts dynamic-fonts-preferred-monospace-fonts '("fixed"))
                  (append '("helv" "helvetica" "arial") dynamic-fonts-preferred-proportional-fonts dynamic-fonts-preferred-monospace-fonts '("fixed")))))))

(provide 'dynamic-fonts)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords:  DynamicFonts XQuartz Lucida callf Segoe DejaVu Arial
;; LocalWords:  Bitstream Tahoma Verdana Helvetica Consolas Menlo
;; LocalWords:  Inconsolata Andale Cousine Lekton BPmono Garamond
;; LocalWords:  ProFontWindows Hiragino XLFD demi fontconfig Grande
;; LocalWords:  helvetica
;;

;;; dynamic-fonts.el ends here
