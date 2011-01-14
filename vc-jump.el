;;; vc-jump.el --- jump to status buffer for the current VC

;; Copyright (C) 2011  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky at gmail dot com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;
;;; INSTALLATION: In your ~/.emacs,
;;;
;;; (require 'vc-jump)
;;; (global-set-key [f12] 'vc-jump)   ; for your convenience
;;;

;;;
;;; `vc-jump' switches the current buffer (possibly in another window)
;;; to the Version Controlled status buffer. (inspired by `dired-jump')
;;; 
;;; For example, if you're editing a file controlled by CVS, `vc-jump'
;;; executes `cvs-status' in the file's directory.  Likewise, if the
;;; current file is controlled by GIT, `vc-jump' executes `git-status'
;;; in the file's directory.
;;;

;;; TODO:
;;;
;;;   - In a CVS/SVN controlled file, launching `vc-jump' in a file in
;;;     the sub-directory of a VC controlled directory creates a VC
;;;     status buffer in that directory, not the VC controlled root
;;;     directory.  (except GIT)
;;;
;;;   - Support for other VC systems.
;;;

 
;;; Code:

(require 'vc)

(eval-when-compile
  (require 'cl))

(autoload #'svn-status "psvn" "Entry point into svn-status mode" t)
(autoload #'cvs-status "pcvs" "Entry point into cvs mode" t) 
(autoload #'git-status "git" "Entry point into git-status mode" t)

(when nil
  ;; Currently not used 
  (defvar vc-jump-on-failed nil               ; #'dired-jump
    "This function is called if `vc-jump' failed")
  (defvar vc-buffer-names
    (concat "\\*git-status\\*\\(<[0-9]*>\\)?")
    "Regexp for a VC status buffer")

  (defun find-buffer-regexp (regexp &optional frame)
    "Return the first buffer that has a name matches to REGEXP"
    (let ((buflist (buffer-list frame)))
      (flet ((find-buffer-impl (regexp &optional buffers)
                               (if (null buffers)
                                   nil
                                 (let ((buf (car buffers)))
                                   (if (string-match regexp (buffer-name buf))
                                       buf
                                     (find-buffer-impl regexp
                                                       (cdr buffers)))))))
        (find-buffer-impl regexp buflist)))))

(defvar vc-status-assoc
      '((Git . git-status)
        (SVN . svn-status)
        (CVS . (lambda (dir) 
                 (cvs-status dir 
                             (cvs-flags-query 'cvs-status-flags
                                              "cvs status flags")))))
      "association list for (VC-SYSTEM . STATUS-FUNCTION)")

      
(defun vc-responsible-backend-noerror (file)
  (catch 'found
    ;; First try: find a responsible backend.  If this is for registration,
    ;; it must be a backend under which FILE is not yet registered.
    (dolist (backend vc-handled-backends)
      (and (vc-call-backend backend 'responsible-p file)
           (throw 'found backend)))))

(defun vc-status-function (file)
  "Return the VC status function associated with FILE"
  ;;  (let ((fname (buffer-file-name)))
  ;;    (if fname
  (let* ((vcsys (vc-responsible-backend-noerror file))
         (vcpair (assoc vcsys vc-status-assoc))
         (vcfunc (if vcpair (cdr vcpair) nil)))
    vcfunc))

(defun vc-jump ()
  (interactive)
  (let* ((fname (buffer-file-name))
         (dname (if fname
                    (if (file-directory-p fname) 
                        fname
                      (file-name-directory fname))
                  default-directory)))
    (message "fname(%s) dname(%s)" fname dname)
    (if (or fname dname)
        (let ((func (vc-status-function (or fname dname))))
          (if func
              (apply func (list dname)))))))


(provide 'vc-jump)
;;; vc-jump.el ends here
