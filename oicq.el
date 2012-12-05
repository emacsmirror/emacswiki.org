;;; oicq.el --- A front-end to perl OICQ

;; Author: jerry <unidevel@yahoo.com.cn>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, jerry, all rights reserved.
;; Created: 2003
;; Version: 1.0
;; Last-Updated: 2008-06-12 18:55:44
;; URL: http://www.emacswiki.org/emacs/download/oicq.el
;; Keywords: QQ, OICQ, Tecent, China
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;;
;;

;;; Installation:
;;
;; Copy oicq.el to your load-path and add to your ~/.emacs
;;
;;  (require 'oicq)
;;
;; If you want to use this extension, you should install oicq in your system,
;; and installation method is below:
;; 1>   Download    (use Google)
;;  Digest-MD5-2.36.tar.gz
;;  TermReadKey-2.14.tar.gz
;;  ANSIColor-1.12.tar.gz
;;  Crypt-OICQ.1.1.tgz
;;  Net-OICQ-1.6.tgz
;;
;; 2>   The installation method of
;; Digest-MD5-2.36.tar.gz TermReadKey-2.14.tar.gz ANSIColor-1.12.tar.gz
;; is same:
;;  $ perl Makefile.PL
;;  $ make test
;;  $ sudo make install
;;
;;  When `make test' TermReadKey-2.14.tar.gz, will have error information,
;;  ignore it.
;;
;; 3>   Test Digest::MD5 Modules whether installed success
;;  $ perl -MDigest::MD5
;;  if no output, is success
;;
;; 4>   Install Crypt-OICQ.1.1.tgz
;;  $ perl Makefile.PL
;;  $ make test
;;  $ su
;;  # make install
;;
;; 5>   Install Net-OICQ-1.6.tgz
;;  $ perl Makefile.PL
;;  $ make test
;;  $ su
;;  # make install
;;  # cp ./bin/qq /usr/local/bin/oicq
;;  # chmod a+x /usr/local/bin/oicq
;;
;; 6>   Hey, it's ok.
;;  Enjoy! :)


;;; Commentary:
;;
;; A front-end of perl OICQ, to use QQ (Tecent) in Emacs.
;;

;;; Change log:
;;
;; 2008/06/12
;;
;;          Add buffer-process-coding-system to make Emacs display correctly coding
;;         Add sentinel for buffer process to handler close buffer when quit OICQ
;;

;;; Acknowledgments:
;;
;;      jerry   <unidevel@yahoo.com.cn> for created this extension
;;

;;; TODO
;;
;; None
;;

;;; Code:

(defconst OICQ-BUFFER "*OICQ BUFFER [%s]*"
  "Oicq buffer name format.")

(defgroup oicq nil
  "Group for QQ"
  :prefix "oicq-"
  :group 'external)

(defcustom oicq-default-user ""
  "The user name of QQ."
  :type 'string
  :group 'oicq)

(defcustom oicq-default-passwd ""
  "The password of QQ."
  :type 'string
  :group 'oicq)

(defun oicq (qq passwd)
  "Login oicq.
Argument QQ account of QQ.
Argument PASSWD password of QQ."
  (interactive "sQQ: \nsPassword: ")
  (let ((oicqp nil))
    (setenv "OICQ_PW" passwd)
    (make-comint-in-buffer "oicq" (format OICQ-BUFFER qq) "oicq" nil qq)
    (switch-to-buffer (format OICQ-BUFFER qq))
    (add-hook 'comint-output-filter-functions
              'oicq-filter-colors nil t)
    (comint-read-input-ring t)
    (set-buffer-process-coding-system 'utf-8 'utf-8) ;make correctly coding system of sub-process
    (handler-buffer-exit-close)         ;handler buffer close automatically when sub-process is quit
    ))

(defun oicq-auto-entry ()
  "Login oicq automatically."
  (interactive)
  (oicq oicq-default-user oicq-default-passwd))

(defun oicq-filter-colors (string)
  "Filter all color sequences."
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char (or comint-last-output-start (point-min)))
      (while (re-search-forward " \\[[0-9]*m" pmark t)
        (replace-match "")))))


(provide 'oicq)

;;; oicq.el ends here

;;; LocalWords:  oicq utilties Makefile sudo su cp passwd qq sQQ nsPassword utf
;;; LocalWords:  oicqp pmark
