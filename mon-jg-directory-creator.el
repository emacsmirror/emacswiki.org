;;; mon-jg-directory-creator.el --- utils for massive directory hierarchy creation
;; -*- mode: EMACS-LISP; -*-

;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-jg-directory-creator.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-03-16T15:54:29-04:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: convenience, dired, data

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-jg-directory-creator provides file/directory creation loop for massive
;; directory hiearchy creation and subesequent file insertion. Hierarchy are
;; suitable for use as the backbone of an Artists' archive of images, photos,
;; text, video etc. Hierarchy consists of nested numbered sub-directories.  
;; "When a directory number is divisible by 10 descend"
;; 
;; FUNCTIONS:►►►
;; `mon-write-jg-file-in-path'
;; `mon-make-jg-dir-in-path'
;; `mon-format-jg-file-for-write'
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
;; `*mon-jg-file-template*'
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
;; URL: http://www.emacswiki.org/emacs/mon-jg-directory-creator.el
;; FIRST-PUBLISHED: <Timestamp: #{2010-03-16T19:46:03-04:00Z}#{10112} - by MON>
;;
;; EMACSWIKI: 
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-03-16T15:54:29-04:00Z}#{10112} - by MON>
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
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-16T15:46:20-04:00Z}#{10112} - by MON>
(defvar *mon-jg-file-template* nil
  "*A JG file template for inserting into JG archive directories.\n
:CALLED-BY `mon-format-jg-file-for-write'\n
:SEE-ALSO `mon-write-jg-file-in-path', `mon-make-jg-dir-in-path'.\n►►►")
;;
(unless (bound-and-true-p *mon-jg-file-template*)
  (setq *mon-jg-file-template*
        '(":FILE-NAME %s"
          ":FILE-TYPE {TEXT, DRAWING, PHOTO, VIDEO}"
          "-----------------------------------"
          ":TITLE"
          ":DESCRIPTION"
          "-----------------------------------"
          ":CONCEPTUAL-NODE"
          ":X-REFERENCES-TO"
          ":PART-OF-SERIES"
          "-----------------------------------"
          ":COMMISSIONED-BY"
          ":APPROX-DATE-INITIATED"
          ":APPROX-DATE-COMPLETED"
          "-----------------------------------"
          ":OTHER-NOTES"
          "-----------------------------------\n")))
;;
;;(progn (makunbound '*mon-jg-file-template*) (unintern '*mon-jg-file-template*) )

;;; ==============================
;; :NOTE Should we be stupid enough to write more than 10k files/dirs at once,
;; Let bind local var pad-str after testing: (length (format "%d" folder-cnt))
;; (pad-str (concat "%0" (number-to-string (length (format "%d" folder-cnt))) "d"))
;;; :CREATED <Timestamp: #{2010-03-16T15:44:04-04:00Z}#{10112} - by MON>
(defun* mon-make-jg-dir-in-path (in-dir folder-cnt &key no-write fl-pfx fl-sfx)
  "Create a directory tree IN-DIR with FOLDER-CNT.\n
IN-DIR names an existing dir to create directory tree under.\n
FOLDER-CNT specifies the number of directories to create. 
It is an integer multiple of 1000.\n
When key NO-WRITE is non-nil do not make directories do not
write files. Default is t.\n
When keyword args FL-PFX and FL-SFX are non-nil these name file-name prefix and
suffiexes per `mon-write-jg-file-in-path' to prepend and append to files created
in bottom most directories.\n
Return a log of the files directory created in buffer named:
 *MON-JG-FILE-DIR-LOG*\n
When keyword arg NO-WRITE is non-nil also write the log to the file named:\n
 <IN-DIR>-YYYY-MM-DD\n
:EXAMPLE\n\n\(mon-make-jg-dir-in-path 
 \"/some-path/phylum-root\" 2000 
 :no-write t :fl-pfx \"bubba-\" :fl-sfx \".umf\"\)\n
¦ 0000\n¦ 0000/0000\n¦ 0000/0000/0000\n¦ 0000/0000/0000/0000\n¦ 0000/0000/0000/0001
¦ 0000/0000/0000/0002\n¦ 0000/0000/0000/0003\n¦
¦  { ... LOTS OF DIRECTORIES ELIDED ... }\n¦
¦ 0000/0000/0000/0000/bubba-0000.umf\n¦ 0000/0000/0000/0001/bubba-0001.umf
¦ 0000/0000/0000/0002/bubba-0002.umf\n¦ 0000/0000/0000/0003/bubba-0003.umf\n¦
¦  { ... LOTS OF FILES ELIDED ... }\n
:SEE-ALSO `mon-write-jg-file-in-path', `mon-make-jg-dir-in-path',
`mon-format-jg-file-for-write', `*mon-jg-file-template*'.\n►►►"
  (let ((th (number-sequence 
             0 
             (if (< folder-cnt 10000)
                 folder-cnt
                 (error ":FUNCTION `mon-make-jg-dir-in-path' - Arg FOLDER-CNT greater than 10000"))
             1000))
        (mult-map #'(lambda (mlt) ;; :NOTE Usage (funcall mult-map 100))
                    (mapcar #'(lambda (var) (* var mlt)) (number-sequence 0 9))))        
        (pad-dpth #'(lambda (dpth &rest args)
                      (let ((pad-str "%04d"))
                        (push (eval `(format
                                 ,(case dpth
                                        (0 pad-str)
                                        (1 (concat pad-str "/" pad-str))
                                        (2 (concat pad-str "/" pad-str "/" pad-str))
                                        (3 (concat pad-str "/" pad-str "/" pad-str "/" pad-str)))
                                 ,@args)) frmt))))
        frmt)
    (when (not no-write)
      (if (file-exists-p in-dir)
          (cd in-dir)
          (if (file-writable-p in-dir)
              (progn
                (mkdir in-dir)
                (cd in-dir))
              (error ":FUNCTION `mon-make-jg-dir-in-path' - Arg IN-DIR unreadable or does not exist"))))
    (dolist (k th)
      (let ((pp (pop th)))
        (funcall pad-dpth 0 pp)
        (dolist (H (funcall mult-map 100))
          (funcall pad-dpth 1 pp (+ pp H))
          (dolist (C (funcall mult-map 10))
            (funcall pad-dpth 2 pp (+ pp H) (+ pp H C))
            (dolist (D (funcall mult-map 1))
              (funcall pad-dpth 3 pp (+ pp H) (+ pp H C) (+ pp H C D)))))))
    (setq frmt (nreverse frmt))
    (when (not no-write)
      (dolist (f frmt) (mkdir f t)))
    (let (fl-created)
      (dolist (w frmt (setq fl-created (nreverse fl-created)))
        (let ((mwjfip (mon-write-jg-file-in-path w fl-pfx fl-sfx no-write)))
          (when mwjfip (push mwjfip fl-created))))
      (setq frmt (nconc 
                  (unless no-write
                    (list (concat ";;\n;; " 
                                  (if (fboundp 'mon-stamp)
                                      (mon-stamp)
                                      (format-time-string "%Y-%m-%d"))
                                  "\n;;\n")))
                  '(";;\n;; :FILES-CREATED\n;;\n") fl-created 
                  '("\n\n;;\n;; :DIRECTORIES-CREATED\n;;\n\n") frmt))
      (with-current-buffer (get-buffer-create "*MON-JG-FILE-DIR-LOG*")
        (erase-buffer)
        (princ (mapconcat #'identity frmt "\n")(current-buffer))
        (unless no-write
          (write-file (concat in-dir "/FILES-HERE-" 
                              (format-time-string "%Y-%m-%d"))))
        (display-buffer "*MON-JG-FILE-DIR-LOG*" t)))))
;; 
;;; :TEST-ME (mon-make-jg-dir-in-path 
;;;              "/some-path/phylum-root" 2000 
;;;              :no-write t :fl-pfx "bubba-" :fl-sfx ".umf")

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-16T15:38:04-04:00Z}#{10112} - by MON>
(defun mon-write-jg-file-in-path (fl-pth &optional fl-prefix fl-suffix no-write-file)
  "Write a JG file template to FL-PTH. Return name of file written.\n
FL-PTH is a string of 19 chars from which to extract a file name for
`write-file'.\n
File content inserted wit `mon-format-jg-file-for-write' which maps the list
of variable `*mon-jg-file-template*'.\n
FL-PREFIX is a string to prepend to generated file-name. Default is \"jg-photo-\".\n
FL-SUFFIX is a string to append to generated file-name, Default is \".txt\"\n
When optional arg NO-WRITE-FILE is non-nil do not write file.\n
:CALLED-BY `mon-make-jg-dir-in-path'\n
:SEE-ALSO \n►►►"
  (let* ((dir-len (length fl-pth))
         (dir-fl (when (> dir-len 15)
                   (concat (directory-file-name fl-pth) "/"
                           (or (file-name-nondirectory (directory-file-name fl-prefix)) "jg-photo-")
                           (substring fl-pth (- dir-len 4))
                           (or fl-suffix ".txt")))))
    (when dir-fl
      (when (not no-write-file)
        (with-temp-file dir-fl (mon-format-jg-file-for-write dir-fl)))
      dir-fl)))

;;; ==============================
;;; :CREATED <Timestamp: #{2010-03-16T15:37:19-04:00Z}#{10112} - by MON>
(defun mon-format-jg-file-for-write (fname-to-write)
  "Map newlines across value of variable `*mon-jg-file-template*'.\n
Return formatted string and insert in buffer at point. Moves point.\n
:CALLED-BY `mon-write-jg-file-in-path'
:SEE-ALSO `mon-make-jg-dir-in-path'.\n►►►"
  (let ((tmpl (mapconcat #'identity *mon-jg-file-template*
               "\n")))
    (insert (format tmpl fname-to-write))))
;;
;;; :TEST-ME (mon-write-jg-file-in-path "0000/0000/0020/0029")

;;; ==============================
(provide 'mon-jg-directory-creator)
;;; ==============================

;;; ================================================================
;;; mon-jg-directory-creator.el ends here
;;; EOF

