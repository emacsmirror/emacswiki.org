;;; filesets+.el --- Extensions to `filesets.el'.
;;
;; Filename: filesets+.el
;; Description: Extensions to `filesets.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2008-2013, Drew Adams, all rights reserved.
;; Created: Tue Sep 16 14:11:36 2008 (-0700)
;; Version: 22.0
;; Last-Updated: Fri Dec 28 09:27:02 2012 (-0800)
;;           By: dradams
;;     Update #: 42
;; URL: http://www.emacswiki.org/filesets+.el
;; Doc URL: http://www.emacswiki.org/FileSets
;; Keywords:
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x
;;
;; Features that might be required by this library:
;;
;;   `easymenu', `filesets'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `filesets.el'.
;;
;;  This library provides some fixes to standard library
;;  `filesets.el'.  The reference version of that library is 1.8.4,
;;  but I believe the same fixes are appropriate for other versions
;;  (e.g. 2.2, named `filesets2.el', which is the latest version by
;;  the original author, at
;;  http://members.a1.net/t.link/CompEmacsFilesets.html).
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'filesets+)
;;    (filesets-init) ; Enable filesets
;;
;;  You should also customize the following two options, to put the
;;  `Filesets' menu on the `File' menu just before item `Open
;;  File...':
;;
;;   `filesets-menu-path'   - value should be ("file")
;;   `filesets-menu-before' - value should be "Open File..."
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2008/09/16 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;; Floor, Boston, MA 02110-1301, USA.;;; Code:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl)) ;; case

(require 'filesets)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; REPLACE ORIGINAL in `filesets.el'.
;;; For `:tree':
;;;  * First get the tree from the ENTRY.
;;;  * Return all matching files under the directory, including in subdirs up to
;;;
(defun filesets-get-filelist (entry &optional mode event)
  "Get all files for fileset ENTRY.
Assume MODE (see `filesets-entry-mode'), if provided."
  (let* ((mode (or mode (filesets-entry-mode entry)))
         (fl (case mode
               ((:files)   (filesets-entry-get-files entry))
               ((:file)    (list (filesets-entry-get-file entry)))
               ((:ingroup)
                (let ((entry (expand-file-name
                              (if (stringp entry)
                                  entry
                                (filesets-entry-get-master entry)))))
                  (cons entry (filesets-ingroup-cache-get entry))))
               ((:tree)
                (let* ((dirpatt  (filesets-entry-get-tree entry))
                       (dir      (nth 0 dirpatt))
                       (patt     (nth 1 dirpatt))
                       (depth    (or (filesets-entry-get-tree-max-level entry)
                                     filesets-tree-max-level)))
                  (filesets-files-under 0 depth entry dir patt)))
               ((:pattern)
                (let ((dirpatt (filesets-entry-get-pattern entry)))
                  (if dirpatt
                      (let ((dir (filesets-entry-get-pattern--dir dirpatt))
                            (patt (filesets-entry-get-pattern--pattern dirpatt)))
                        (filesets-directory-files dir patt ':files t))
                    (filesets-error 'error "Filesets: malformed entry: "
                                    entry)))))))
    (filesets-filter-list
     fl (lambda (file) (not (filesets-filetype-property file event))))))

(defun filesets-files-under (level depth entry dir patt &optional relativep)
  "Files under DIR that match PATT.
LEVEL is the current level under DIR.
DEPTH is the maximal tree scanning depth for ENTRY.
ENTRY is a fileset.
DIR is a directory.
PATT is a regexp that included file names must match.
RELATIVEP non-nil means use relative file names."
  (and (or (= depth 0) (< level depth))
       (let* ((dir         (file-name-as-directory dir))
              (files-here  (filesets-directory-files
                            dir patt nil (not relativep)
                            (filesets-entry-get-filter-dirs-flag entry)))
              (subdirs     (filesets-filter-dir-names files-here))
              (files
               (filesets-filter-dir-names
                (apply #'append
                       files-here
                       (mapcar
                        (lambda (subdir)
                          (let* ((subdir (file-name-as-directory subdir))
                                 (full-subdir  (concat dir subdir)))
                            (filesets-files-under (+ level 1) depth entry
                                         full-subdir patt)))
                        subdirs))
                t)))
         files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'filesets+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filesets+.el ends here
