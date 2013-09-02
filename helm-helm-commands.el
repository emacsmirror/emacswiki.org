;;; helm-helm-commands.el --- List all helm commands with helm

;; Filename: helm-helm-commands.el
;; Description: List all helm commands with helm
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-09-02 15:25:22
;; Version: 0.2
;; Last-Updated: 2013-09-02 15:25:22
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/helm-helm-commands
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((helm "1.5.4"))
;;
;; Features that might be required by this library:
;;
;; helm
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
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 16iXhMdmTBcmrJAcDyv1H371mjXZNfDg7z
;;
;; This provides a single command `helm-helm-commands' which will present a helm buffer 
;; containing a list of helm commands and short descriptions. You can press C-z on an item
;; to see a longer description of the command, and RET to execute the command.
;;
;; Note: you can achieve the same thing with the `helm-M-x' command which comes with helm,
;; but that requires a few more keystrokes and doesn't show descriptions by default.
;;
;;;;


;;; Installation:
;;
;; Put helm-helm-commands.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'helm-helm-commands)

;;; Change log:
;;	
;; 2013/09/02
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; 
;;

;;; Require
(require 'helm)

;;; Code:

(defvar helm-helm-commands-source-buffer "*helm source select*")

(defvar helm-source-helm-commands
  `((name . "Helm commands")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda nil
         (loop for symname in (all-completions "helm-" obarray)
               for sym = (intern symname)
               if (commandp sym) collect
               (cons
                (concat
                 (propertize (format "%s" symname)
                             'face 'font-lock-function-name-face)
                 (propertize (format " %s"
                                     (or (and (documentation sym)
                                              (car (split-string
                                                    (documentation sym) "\n\\|\\.")))
                                         "Not documented"))
                             'face 'font-lock-doc-face))
                sym))))
    (action . (("Execute helm command" .
                (lambda (candidate)
                  (call-interactively candidate)))
               ("Describe command" . describe-command)))
    (persistent-action . describe-command)))

;;;###autoload
(defun helm-helm-commands nil
  "Select from helm commands to execute."
  (interactive)
  (helm :sources 'helm-source-helm-commands
        :buffer helm-helm-commands-source-buffer))

(provide 'helm-helm-commands)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "helm-helm-commands.el" (buffer-name) (buffer-string) "update")

;;; helm-helm-commands.el ends here
