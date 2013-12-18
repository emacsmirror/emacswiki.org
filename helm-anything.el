;;; helm-anything.el --- Bridge between anything and helm

;; Filename: helm-anything.el
;; Description: Bridge between anything and helm
;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Copyright (C) 2013, rubikitch, all rights reserved.
;; Time-stamp: <2013-12-18 11:32:24 rubikitch>
;; Created: 2013-03-15 08:59:37
;; Version: 0.2
;; Package-Requires: ((helm "20130406") (anything "20120101"))
;; URL: http://www.emacswiki.org/emacs/download/helm-anything.el
;; Keywords: helm, anything, convenience, search, tools
;; Compatibility: GNU Emacs 24.2.2
;;
;; Features that might be required by this library:
;;
;; `helm', `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary: 
;; 
;; Bridge between anything and helm.
;;
;; M-x `helm-anything-resume' replaces M-x `anything-resume' and M-x
;; `helm-resume'.
;;
;; `helm-anything/funcall', `helm-anything/set' and
;; `helm-anything/get' handles helm or anything functions and
;; variables.
;;
;; `helm-anything/define-key' binds key to `helm-map' and `anything-map'.
;;
;;; Installation:
;;
;; Put helm-anything.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'helm-anything)
;; ;; Replace helm-resume and anything-resume with helm-anything-resume
;; (helm-anything-set-keys)
;;
;; No need more.
;;
;;; Examples:
;;
;; (defun helm-anything-occur-search-forward ()
;;   (interactive)
;;   (helm-anything/funcall 'next-line) ;; or (anything-previous-line)
;;   (helm-anything/funcall 'execute-persistent-action))
;;
;; (setq helm-testvar 0)
;; (setq anything-testvar 100)
;; (defun helm-anything-test ()
;;   (interactive)
;;   (helm-anything/set 'testvar (1+ (helm-anything/value 'testvar)))
;;   (message "%s" (helm-anything/value 'testvar)))
;; (define-key helm-map (kbd "C-x C-z") 'helm-anything-test)
;; (define-key anything-map (kbd "C-x C-z") 'helm-anything-test)

;;; Require
(require 'helm)
(require 'anything)

;;; Code:

;;;; Resume both helm and anything buffers.
(defvar helm-anything-resume-function nil)

;;;###autoload
(defun helm-anything-resume (parg)
  "Resurrect previously invoked `helm' or `anything'.
Called with a prefix arg, allow choosing among all existing
helm/anything buffers.  i.e choose among various helm/anything sessions.
Called from lisp, you can specify a buffer-name as a string with PARG."
  (interactive "P")
  (cond (parg
         (setq current-prefix-arg nil)
         (helm :sources '(((name . "Resume helm buffer")
                           (candidates . helm-buffers)
                           (action . helm-resume))
                          ((name . "Resume anything buffer")
                           (candidates . anything-buffers)
                           (action . anything-resume)))
               :resume 'noresume
               :buffer "*helm/anything resume*"))
        ((memq (car helm-anything-resume-function) '(helm-resume anything-resume))
         (eval helm-anything-resume-function))
        (t
         (error "No helm/anything buffers for resume"))))

(defadvice helm-initialize (after helm-anything-resume activate)
  (unless (eq (ad-get-arg 0) 'noresume)
    (setq helm-anything-resume-function (list 'helm-resume helm-last-buffer))))

(defadvice anything-initialize (after helm-anything-resume activate)
  (unless (eq (ad-get-arg 0) 'noresume)
    (setq helm-anything-resume-function (list 'anything-resume anything-last-buffer))))

(defun helm-anything-set-keys ()
  "Replace `anything-resume' and `helm-resume' with `helm-anything-resume'."
  (interactive)
  (global-set-key [remap anything-resume] 'helm-anything-resume)
  (global-set-key [remap helm-resume] 'helm-anything-resume))

;;;; Compatibility code.
(defun helm-anything/funcall (func &rest args)
  "Call helm or anything function FUNC with ARGS.
If you are using helm, call helm-FUNC, otherwise anything-FUNC."
  (apply (intern (format "%s-%s" (if (helm-alive-p) 'helm 'anything) func)) args))
(defun helm-anything/set (varname value)
  "Set helm or anything variable VARNAME to VALUE.
If you are using helm, set helm-VARNAME, otherwise anything-VARNAME."
  (set (intern (format "%s-%s" (if (helm-alive-p) 'helm 'anything) varname)) value))
(defun helm-anything/value (varname)
  "Get helm or anything variable VARNAME.
If you are using helm, set helm-VARNAME, otherwise anything-VARNAME."
  (symbol-value (intern (format "%s-%s" (if (helm-alive-p) 'helm 'anything) varname))))
(defun helm-anything/define-key (key command)
  "Define KEY to COMMAND both `helm-map' and `anything-map'."
  (define-key helm-map key command)
  (define-key anything-map key command))

(provide 'helm-anything)

;;; helm-anything.el ends here
