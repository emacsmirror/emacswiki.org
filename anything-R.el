;;; anything-R.el --- anything-sources and some utilities for GNU R.

;; Copyright (C) 2010 myuhe <yuhei.maeda_at_gmail.com>
;; Author: <yuhei.maeda_at_gmail.com>
;; Keywords: convenience,anything, GNU R

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; It is necessary to Some Anything and ESS Configurations for using R before

;;; Installation:
;;
;; Put the anything-R.el, anything.el and ESS to your
;; load-path.
;; Add to .emacs:
;; (require 'anything-R)
;;

;;; Command:
;;  `anything-for-R'

;;  Anything sources defined :
;; `anything-c-source-R-help'     (manage help function)
;; `anything-c-source-R-local'    (manage object))
;; `anything-c-source-R-localpkg' (manage local packages)
;; `anything-c-source-R-repospkg' (manage repository packages)

;;; Code:
(require 'anything)
(require 'ess-site)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;anything-c-source-R-help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq anything-c-source-R-help
      '((name . "R objects / help")
        (init . (lambda ()
                  ;; this grabs the process name associated with the buffer
                  (setq anything-c-ess-local-process-name ess-local-process-name)))
        (candidates . (lambda ()
                        (condition-case nil
                            (ess-get-object-list anything-c-ess-local-process-name)
                          (error nil))))
        (action
         ("help" . ess-display-help-on-object)
         ("head (10)" . (lambda(obj-name)
                          (ess-execute (concat "head(" obj-name ", n = 10)\n") nil (concat "R head: " obj-name))))
         ("head (100)" . (lambda(obj-name)
                           (ess-execute (concat "head(" obj-name ", n = 100)\n") nil (concat "R head: " obj-name))))
         ("tail" . (lambda(obj-name)
                     (ess-execute (concat "tail(" obj-name ", n = 10)\n") nil (concat "R tail: " obj-name))))
         ("str" . (lambda(obj-name)
                    (ess-execute (concat "str(" obj-name ")\n") nil (concat "R str: " obj-name))))
         ("summary" . (lambda(obj-name)
                        (ess-execute (concat "summary(" obj-name ")\n") nil (concat "R summary: " obj-name))))
         ("view source" . (lambda(obj-name)
                            (ess-execute (concat "print(" obj-name ")\n") nil (concat "R object: " obj-name))))
         ("dput" . (lambda(obj-name)
                     (ess-execute (concat "dput(" obj-name ")\n") nil (concat "R dput: " obj-name)))))
        (volatile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything-c-source-R-local
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq anything-c-source-R-local
      '((name . "R local objects")
        (init . (lambda ()
                  ;; this grabs the process name associated with the buffer
                  (setq anything-c-ess-local-process-name ess-local-process-name)
                  ;; this grabs the buffer for later use
                  (setq anything-c-ess-buffer (current-buffer))))
        (candidates . (lambda ()
                        (let (buf)
                          (condition-case nil
                              (with-temp-buffer
                                (progn
                                  (setq buf (current-buffer))
                                  (with-current-buffer anything-c-ess-buffer
                                    (ess-command "print(ls.str(), max.level=0)\n" buf))
                                  (split-string (buffer-string) "\n" t)))
                            (error nil)))))
        (display-to-real . (lambda (obj-name) (car (split-string obj-name " : " t))))
        (action
         ("str" . (lambda(obj-name)
                    (ess-execute (concat "str(" obj-name ")\n") nil (concat "R str: " obj-name))))
         ("summary" . (lambda(obj-name)
                        (ess-execute (concat "summary(" obj-name ")\n") nil (concat "R summary: " obj-name))))
         ("head (10)" . (lambda(obj-name)
                          (ess-execute (concat "head(" obj-name ", n = 10)\n") nil (concat "R head: " obj-name))))
         ("head (100)" . (lambda(obj-name)
                           (ess-execute (concat "head(" obj-name ", n = 100)\n") nil (concat "R head: " obj-name))))
         ("tail" . (lambda(obj-name)
                     (ess-execute (concat "tail(" obj-name ", n = 10)\n") nil (concat "R tail: " obj-name))))
         ("print" . (lambda(obj-name)
                      (ess-execute (concat "print(" obj-name ")\n") nil (concat "R object: " obj-name))))
         ("dput" . (lambda(obj-name)
                     (ess-execute (concat "dput(" obj-name ")\n") nil (concat "R dput: " obj-name)))))
        (volatile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; func for action
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anything-ess-marked-install (candidate)
  (dolist (i (anything-marked-candidates))
    (ess-execute (concat "install.packages(\"" i "\")\n") t)))

(defun anything-ess-marked-remove (candidate)
  (dolist (i (anything-marked-candidates))
    (ess-execute (concat "remove.packages(\"" i "\")\n") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything-c-source-R-localpkg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq anything-c-source-R-localpkg
      '((name . "R-local-packages")
        (init . (lambda ()
                  ;; this grabs the process name associated with the buffer
                  (setq anything-c-ess-local-process-name ess-local-process-name)
                  ;; this grabs the buffer for later use
                  (setq anything-c-ess-buffer (current-buffer))))
        (candidates . (lambda ()
                        (let (buf)
                          (condition-case nil
                              (with-temp-buffer
                                (progn
                                  (setq buf (current-buffer))
                                  (with-current-buffer anything-c-ess-buffer
                                    (ess-command "writeLines(paste('', sort(.packages(all.available=TRUE)), sep=''))\n" buf))

                                  (split-string (buffer-string) "\n" t)))
                            (error nil)))))

        (action
         ("load packages" . (lambda(obj-name)
                              (ess-execute (concat "library(" obj-name ")\n") t )))
         ("remove packages" . (lambda(obj-name)
                                (ess-execute (concat "remove.packages(\"" obj-name "\")\n") t)))
         ("remove marked packages" . anything-ess-marked-remove))    
        (volatile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything-c-source-R-repospkg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq anything-c-source-R-repospkg
      '((name . "R-repos-packages")
        (init . (lambda ()
                  ;; this grabs the process name associated with the buffer
                  (setq anything-c-ess-local-process-name ess-local-process-name)
                  ;; this grabs the buffer for later use
                  (setq anything-c-ess-buffer (current-buffer))))
        (candidates . (lambda ()
                        (let (buf)
                          (condition-case nil
                              (with-temp-buffer
                                (progn
                                  (setq buf (current-buffer))
                                  (with-current-buffer anything-c-ess-buffer
                                    (ess-command "writeLines(paste('', rownames(available.packages(contriburl=contrib.url(\"http://cran.md.tsukuba.ac.jp/\"))), sep=''))\n" buf))
                                  ;; (ess-command "writeLines(paste('', sort(.packages(all.available=TRUE)), sep=''))\n" buf))
                                  (split-string (buffer-string) "\n" t)))
                            (error nil)))))

        (action
         ("install packages" . (lambda(obj-name)
                                 (ess-execute (concat "install.packages(\"" obj-name "\")\n") t)))
         ("install marked packages" . anything-ess-marked-install))    
        (volatile)))

(defcustom anything-for-R-list '(anything-c-source-R-help 
                                 anything-c-source-R-local 
                                 anything-c-source-R-repospkg 
                                 anything-c-source-R-localpkg)
  "Your prefered sources to GNU R."
  :type 'list
  :group 'anything-R)

(defun anything-for-R ()
  "Preconfigured `anything' for GNU R."
  (interactive)
  (anything-other-buffer anything-for-R-list "*anything for GNU R*"))


(provide 'anything-R)
