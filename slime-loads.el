;;; this is slime-loads-GNU.el --- stub to indirect slime/swank configs on GNU
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2009, 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: slime-loads-GNU.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2009-09-06T10:19:19-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: environment, external, lisp, programming, processes

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; slime-loads-GNU is a stub. It indirects loading of slime/swank configs on GNU
;; systems and is an attempt at easing transitions when slime-devels make
;; breaking changes.
;; :SEE :FILE slime-loads-GNU-clbuild.el
;; :SEE (URL `http://www.emacswiki.org/emacs/')
;;
;; FUNCTIONS:►►►
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;; 
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/slime-loads-GNU.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-09-07T11:31:22-04:00Z}#{10362} - by MON>
;;
;; EMACSWIKI: { URL of an EmacsWiki describing slime-loads-GNU. }
;;
;; FILE-CREATED: <Timestamp: #{2009-09-06T10:19:19-04:00Z}#{09367} - by MON KEY>
;; 
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2009 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; Stub everything is elswhere now.
;;; :CHANGESET 1917
;;; :CREATED <Timestamp: #{2010-06-23T21:08:20-04:00Z}#{10253} - by MON KEY>
(unless (featurep 'slime-loads-GNU-clbuild)
  (require 'slime-loads-GNU-clbuild))

;;; ==============================
(provide 'slime-loads-GNU)
;;; ==============================

 
;; Local Variables:
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; slime-loads-GNU.el ends here
;;; EOF
