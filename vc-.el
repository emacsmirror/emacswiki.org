;;; vc-.el --- Extensions to `vc.el'.
;;
;; Filename: vc-.el
;; Description: Extensions to `vc.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2000-2017, Drew Adams, all rights reserved.
;; Created: Thu Sep 14 09:47:26 2000
;; Version: 20.0
;; Last-Updated: Sun Jan  1 11:55:10 2017 (-0800)
;;           By: dradams
;;     Update #: 35
;; URL: http://www.emacswiki.org/vc-.el
;; Keywords: internal, tools, unix, local
;; Compatibility: GNU Emacs 20.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Extensions to `vc.el'.
;;
;; Note: This code is quite old, and is likely obsolete now.  You
;;       might find it useful in some way to mine - or not. ;-)
;;
;; -------------------------------------------------------------------
;;
;;  See also the companion file `vc+.el'.
;;        `vc-.el' should be loaded before `vc.el'.
;;        `vc+.el' should be loaded after `vc.el'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar vc-dired-terse-display nil
  "*If non-nil, show only locked files in VC Dired.")


;;;;;;;;;;;;;;;;;;

(provide 'vc-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vc-.el ends here
