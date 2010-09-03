;;; gwt-mumamo.el --- Multi major modes for Google Web Toolkit code

;; Copyright (C) 2010 Felix H. Dahlke

;; Author: Felix H. Dahlke <fhd@ubercode.de>
;; Version: 1.0
;; Keywords: languages, gwt

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Google Web Toolkit code is essentially Java code that will be compiled
;; to JavaScript by the GWT compiler. It is also possible to write inline
;; JavaScript (JSNI), and this mode uses nXhtml's MuMaMo to provide support
;; for both Java and JavaScript in the same file.

;; In order to use gwt-mumamo, you will need the following:
;; * nXhtml (http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html)
;; * espresso-mode (http://www.nongnu.org/espresso/)

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
(require 'java-mode)
(require 'espresso-mode)

(defun mumamo-chunk-gwt-jsni (pos min max)
  "Find /*- ... -*/, return range and espresso-mode."
  (mumamo-quick-static-chunk pos min max "/*-{" "}-*/" nil 'espresso-mode nil))

(define-mumamo-multi-major-mode gwt-mumamo-mode
  "Turn on multiple major modes for Google Web Toolkit code.
The main mode is `java-mode', `espresso-mode' is used for JSNI blocks."
  ("GWT Family" java-mode (mumamo-chunk-gwt-jsni)))

;;; gwt-mumamo.el ends here
