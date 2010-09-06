;;; gwt-mumamo.el --- Multi major modes for Google Web Toolkit code

;; Copyright (C) 2010 Felix H. Dahlke

;; Author: Felix H. Dahlke <fhd@ubercode.de>
;; Version: 1.1
;; Keywords: languages, gwt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The Google Web Toolkit (GWT) allows you to write client-side code for
;; web applications in Java, which is translated to JavaScript by the GWT
;; compiler. It is possible to write inline JavaScript via the JavaScript
;; Native Interface (JSNI), and this mode uses MuMaMo from nXhtml to
;; provide support for both Java and inline JavaScript in the same file.

;;; Installation:

;; In order to use gwt-mumamo, you will need nXhtml, get it here:
;;   http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html

;; If your Emacs version is 23.1 or below, you will need espresso-mode:
;;   http://www.nongnu.org/espresso/
;; You also need to add an alias to it, add the following to your init
;; file:
;;   (defalias 'js-mode 'espresso-mode)

;; Add this file to your load-path and add the following line to your
;; init file:
;;   (autoload 'gwt-mumamo-mode "gwt-mumamo" "" t)
;; You can now activate gwt-mumamo by invoking "gwt-mumamo-mode"

;;; License:

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

;;; Code:

(require 'mumamo-fun)

(defun mumamo-chunk-gwt-jsni (pos min max)
  "Find /*- ... -*/, return range and js-mode."
  (mumamo-quick-static-chunk pos min max "/*-{" "}-*/" nil 'js-mode nil))

(define-mumamo-multi-major-mode gwt-mumamo-mode
  "Turn on multiple major modes for Google Web Toolkit code.
The main mode is `java-mode', `js-mode' is used for JSNI blocks."
  ("GWT Family" java-mode (mumamo-chunk-gwt-jsni)))

;;; gwt-mumamo.el ends here
