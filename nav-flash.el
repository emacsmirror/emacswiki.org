;;; nav-flash.el --- Briefly highlight the current line
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/nav-flash
;; URL: http://raw.github.com/rolandwalker/nav-flash/master/nav-flash.el
;; Version: 1.1.0
;; Last-Updated: 25 Oct 2013
;; EmacsWiki: NavFlash
;; Keywords: extensions, navigation, interface
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'nav-flash)
;;
;;     (nav-flash-show)
;;
;; Explanation
;;
;; Nav-flash temporarily highlights the line containing the point,
;; which is sometimes useful for orientation after a navigation
;; command.
;;
;; To use nav-flash, place the nav-flash.el library somewhere Emacs
;; can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'nav-flash)
;;
;; There is no user-level interface for this library; it is only used
;; by other Lisp libraries.  However, you might find it useful to call
;; `nav-flash-show' in your ~/.emacs file.  For example, the following
;; hook causes a flash to appear after navigating via imenu:
;;
;;     (add-hook 'imenu-after-jump-hook 'nav-flash-show nil t)
;;
;; See Also
;;
;;     M-x customize-group RET nav-flash RET
;;     M-x customize-group RET pulse RET
;;
;; Notes
;;
;;     This library reuses a timer and overlay defined in compile.el,
;;     but should not affect the normal use of compile.el / `next-error'.
;;
;;     Pulse.el provides similar functionality and is included with
;;     Emacs.  This library can use pulse.el, but does not do so by
;;     default, because pulse.el uses `sit-for', breaking this type
;;     of construction:
;;
;;         (nav-flash-show)
;;         (with-temp-message "message here"
;;            (sit-for 2))
;;
;;     When using an overlay and timer for cleanup (as nav-flash does
;;     by default) the flash and message appear simultaneously.
;;
;;     Nav-flash.el is also simpler than pulse.el.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes, with some limitations
;;     GNU Emacs version 21.x and lower : unknown
;;
;;     No external dependencies
;;
;; Bugs
;;
;;     No known bugs.
;;
;; TODO
;;
;;     Check pulse period on other platforms.
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

;; for callf
(require 'cl)

;; may use pulse.el if available
(require 'pulse nil t)
(require 'compile)

;;; customizable variables

;;;###autoload
(defgroup nav-flash nil
  "Briefly highlight the current line."
  :version "1.1.0"
  :link '(emacs-commentary-link :tag "Commentary" "nav-flash")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/nav-flash")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/NavFlash")
  :prefix "nav-flash-"
  :group 'navigation
  :group 'extensions)

(defcustom nav-flash-use-pulse nil
  "Use the `pulse' library if present."
  :type '(choice
          (const :tag "Never"     nil)
          (const :tag "GUI Only"  gui-only)
          (const :tag "Always"    t))
  :group 'nav-flash)

(defcustom nav-flash-delay .5
  "How many seconds to flash `nav-flash-face' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'nav-flash)

;; note nav-flash-face is customizable, but nav-flash-pulse-face is not
(defface nav-flash-face
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'nav-flash)

;;; utility functions

(defun nav-flash-use-pulse-p ()
  "Return t if pulse.el should be used."
  (and (fboundp 'pulse-available-p)
       (pulse-available-p)
       (or (eq nav-flash-use-pulse t)
           (and (eq nav-flash-use-pulse 'gui-only)
                (display-graphic-p)))))

;;;###autoload
(defun nav-flash-show (&optional pos end-pos face delay)
  "Flash a temporary highlight to help the user find something.

POS is optional, and defaults to the current point.

If optional END-POS is set, flash the characters between the two
points, otherwise flash the entire line in which POS is found.

The flash is normally not inclusive of END-POS.  However, when
POS is equal to END-POS, the single character at POS will flash.

Optional FACE defaults to `nav-flash-face'.  Optional DELAY
defaults to `nav-flash-delay' seconds.  Setting DELAY to 0 makes
this function a no-op."
  (callf or pos (point))
  (unless end-pos
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
        (goto-char pos)
        (beginning-of-visual-line)
        (setq pos (point))
        (end-of-visual-line)
        (setq end-pos (1+ (point))))))
  (when (eq pos end-pos)
    (incf end-pos))
  (callf or delay nav-flash-delay)
  (callf or face 'nav-flash-face)
  (when (and (numberp delay)
             (> delay 0))
    (if (nav-flash-use-pulse-p)
         (progn
           ;; create a face with explicit background, merging inherited attributes
           (copy-face face 'nav-flash-pulse-face)
           (set-face-attribute 'nav-flash-pulse-face nil :background (face-attribute face :background nil t))
           ;; these numbers look incorrect, but empirically they make pulse follow a similar
           ;; delay to the plain overlay (on Cocoa Emacs at least)
           (let ((pulse-iterations (round (/ delay .15)))
                 (pulse-delay .01))
             (when (<= pulse-iterations 0)
               (setq pulse-iterations 1))
             (pulse-momentary-highlight-region pos end-pos 'nav-flash-pulse-face)))
       ;; else
       (when (timerp next-error-highlight-timer)
         (cancel-timer next-error-highlight-timer))
       (setq compilation-highlight-overlay (or compilation-highlight-overlay
                                               (make-overlay (point-min) (point-min))))
       (overlay-put compilation-highlight-overlay 'face face)
       (overlay-put compilation-highlight-overlay 'priority 10000)
       (move-overlay compilation-highlight-overlay pos end-pos)
       (add-hook 'pre-command-hook 'compilation-goto-locus-delete-o)
       (setq next-error-highlight-timer
             (run-at-time delay nil 'compilation-goto-locus-delete-o)))))

(provide 'nav-flash)

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
;; LocalWords:  NavFlash imenu callf
;;

;;; nav-flash.el ends here
