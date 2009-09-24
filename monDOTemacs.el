;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is MON .emacs
;;; ================================================================
;;; DESCRIPTION:
;;; MON .emacs provides load-paths/code specific to the site local Emacs 
;;; allowing for for better portability/granularity across MON systems.
;;; 
;;; Replace `<LOCAL-PATH-TO>' with a path appropriate to local system.
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/monDOTemacs.el)
;;; FIRST-PUBLISHED: <Timestamp: #{2009-09-23T11:40:39-04:00Z}#{09393} - by MON>
;;; 
;;; FILE-CREATED: Autumn 2008
;;; HEADER-ADDED: <Timestamp: #{2009-09-23T11:29:10-04:00Z}#{09393} - by MON>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; ©opyright (C) - MON KEY - 2009
;;; ==============================
;;; CODE:

(server-start)
;; (setq debug-on-error t) 

;; Replace <LOCAL-PATH-TO> below with an appropriate path to these files.

;; Where MON stores elisp that encapsulates private user data. 
;; This allows exposing more code in public forums:
;;
(load-file "<LOCAL-PATH-TO>/mon-site-local-defaults.el")

;; Bootstrap core load-paths.
;; The constants/variables established are needed across all MON systems.
;; 
(load-file "<LOCAL-PATH-TO>/mon-default-loads.el")

;; Finish loading required packages and site-local procedures.
;; The here routines are evaluated conditional to current MON system. 
;;
(load-file "<LOCAL-PATH-TO>/mon-default-start-loads.el")


;;; ================================================================
;;; MON .emacs ends here
;;; EOF
