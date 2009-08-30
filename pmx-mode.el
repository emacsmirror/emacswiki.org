;;; pmx-mode.el -- Generic mode for font-lock support and pmx.

;; Copyright (C) 2003-2004 Jeremy Cowgar

;; Author: Jeremy Cowgar <jeremy@cowgar.com>
;; Maintainer: Jeremy Cowgar <jeremy@cowgar.com>
;; Version: 1.0.0
;; Keywords: PMX Music Typesetting
;; URL: http://jeremy.cowgar.com/EmacsPmxMode.html

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;History
;; 1.0.0
;;  -- Created

;;; Code:

(require 'generic-x)

(define-generic-mode 'pmx-mode
  '("%")            ;; comment
  '()               ;; no keywords? hum.
  '(
    ;;
    ;; TeX commands
    ;;
    ("^\\\\\.*"    . 'font-lock-preprocessor-face)

    ;;
    ;; Directives (must be at the begining of the line
    ;;
    ("^[AIT]\.*"   . 'font-lock-constant-face)

    ;;
    ;; Any dynamic
    ;;
    ("D.*?\\( \\|$\\)"      . 'font-lock-preprocessor-face)

    ;;
    ;; Key change, repeat bar, meter change
    ;;
    ("[KRm].*?\\( \\|$\\)"      . 'font-lock-preprocessor-face)

    ;;
    ;; These items must be at the beginning of the line
    ;;
    ;; movement, line change
    ;;
    ("^[ML].*?\\( \\|$\\)"    . 'font-lock-preprocessor-face)

    ;;
    ;; Simple items
    ;;
    ("\|"          . 'font-lock-function-name-face)          ;; bar sep
    ("[()]"        . 'font-lock-function-name-face)          ;; slurs
    ("\\["         . 'font-lock-function-name-face)          ;; bracket
    ("\\]"         . 'font-lock-function-name-face)          ;; bracket
    ("/"           . 'font-lock-function-name-face)          ;; end of input
    )
  '(".pmx\\'")
  nil)

(provide 'pmx-mode)
;;; pmx-mode.el ends here
