;;; @(#) xsocks.el -- emacs sockets extender

;; This file is not part of Emacs

;; Copyright (C) 2009 by Alessandro Di Marco
;; Author:          Alessandro Di Marco (dmr@c0nc3pt.com)
;; Maintainer:      Alessandro Di Marco (dmr@c0nc3pt.com)
;; Created:         January 18, 2009
;; Keywords:        socks
;; Latest Version:

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;  xsocks extends the socks behavior with a new feature `socks-doproxy', which
;;  is the dual of `socks-noproxy'. Notice how `socks-doproxy' gets considered
;;  only in case `socks-doproxy' is nil (i.e. this last one does have the
;;  precedence over the first).

;;; Installation:
;;
;;  Put this file on your Emacs-Lisp load path.

;;; Usage:
;;
;;  Do you really need help for this?

;;; Known Bugs:
;;
;;  Too simple to have one (hopefully ;-)

;;; Comments:
;;
;;  Any comments, suggestions, bug reports or upgrade requests are welcome.
;;  Please send them to Alessandro Di Marco (dmr@c0nc3pt.com).
;;
;;  This version of xsocks was developed and tested with GNU Emacs 23.0.60.1
;;  under Linux x86_64. Please, let me know if it works with other OS and
;;  versions of Emacs.

(require 'socks)

(defvar socks-doproxy nil
  "*List of regexps matching hosts that we should socksify connections to")

(defun do-socks-find-route (host service args)
  (multiple-value-bind
      (route server proxy) args
    (while proxy
      (if (eq ?! (aref (car proxy) 0))
	  (if (string-match (substring (car proxy) 1) host)
	      (setq proxy nil))
	(if (string-match (car proxy) host)
	    (setq route server
		  proxy nil)))
      (setq proxy (cdr proxy)))
    route))

(defun socks-find-route (host service)
  (do-socks-find-route 
   host service 
   (if socks-noproxy
       (list socks-server nil socks-noproxy)
     (list nil socks-server socks-doproxy))))

(provide 'xsocks)
