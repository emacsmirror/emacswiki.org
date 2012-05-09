;;; screenshot.el --- Take a screenshot in Emacs
;; $Id: screenshot.el,v 1.9 2010/05/04 08:59:08 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: images, hypermedia
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/screenshot.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;; Take a screenshot by ImageMagick in Emacs easily. Then send the
;; image to remote host by scp (optional). Finally the URL or filename
;; is in the kill-ring. Execute 
;;
;; M-x screenshot
;;
;; See docstring for detail.
;; [EVAL IT] (describe-function 'screenshot)
;; [EVAL IT] (describe-function 'screenshot-take)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `screenshot'
;;    Prepare to take a screenshot to FILENAME with SCHEME.
;;  `screenshot-take'
;;    Take a screenshot configured by `screenshot' command.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `screenshot-schemes'
;;    *Screenshot configuration list.
;;    default = (quote (("local" :dir "~/images/") ("current-directory" :dir default-directory) ("remote-ssh" :dir "/tmp/" :ssh-dir "www.example.org:public_html/archive/" ...) ("EmacsWiki" :dir "~/.yaoddmuse/EmacsWiki/" :yaoddmuse "EmacsWiki") ("local-server" :dir "~/public_html/" :url "http://127.0.0.1/")))
;;  `screenshot-default-scheme'
;;    *Default scheme name of screenshot.el.
;;    default = nil
;;  `screenshot-take-delay'
;;    *Delay time to take a screenshot.
;;    default = 0.5

;;; Installation:
;;
;; You must have installed ImageMagick and optional scp.
;; 
;; If you want to post images to EmacsWiki, you must install yaoddmuse.el .
;; http://www.emacswiki.org/cgi-bin/wiki/download/yaoddmuse.el
;; 
;; Put screenshot.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'screenshot)
;; (setq screenshot-schemes
;;       '(
;;         ;; To local image directory
;;         ("local"
;;          :dir "~/images/")            ; Image repository directory
;;         ;; To current directory
;;         ("current-directory"          ; No need to modify
;;          :dir default-directory)
;;         ;; To remote ssh host
;;         ("remote-ssh"
;;          :dir "/tmp/"                 ; Temporary saved directory
;;          :ssh-dir "www.example.org:public_html/archive/" ; SSH path
;;          :url "http://www.example.org/archive/")  ; Host URL prefix
;;         ;; To EmacsWiki (need yaoddmuse.el)
;;         ("EmacsWiki"                 ; Emacs users' most familiar Oddmuse wiki
;;          :dir "~/.yaoddmuse/EmacsWiki/"  ; same as yaoddmuse-directory
;;          :yaoddmuse "EmacsWiki")         ; You can specify another Oddmuse Wiki
;;         ;; To local web server
;;         ("local-server"
;;          :dir "~/public_html/"           ; local server directory
;;          :url "http://127.0.0.1/")))     ; local server URL prefix
;; (setq screenshot-default-scheme "local")
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET screenshot RET
;;


;;; Bug Report:
;;
;; If you have problem, send a bug report via M-x screenshot-send-bug-report.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of screenshot.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "screenshot.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) M-x screenshot-send-bug-report and M-x insert-buffer *Backtrace*
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)

;;; History:

;; $Log: screenshot.el,v $
;; Revision 1.9  2010/05/04 08:59:08  rubikitch
;; Added bug report command
;;
;; Revision 1.8  2010/05/04 08:58:41  rubikitch
;; bugfix
;;
;; Revision 1.7  2009/03/07 18:11:40  rubikitch
;; screenshot-take: use `run-with-idle-timer' instead of `sit-for'
;;
;; Revision 1.6  2009/03/06 02:35:51  rubikitch
;; * Set default value `screenshot-default-scheme' to nil.
;;   Use last-used scheme by default.
;; * Fix doc
;;
;; Revision 1.5  2009/03/06 02:29:23  rubikitch
;; * Add Command/Option list
;; * Image file existence check
;;
;; Revision 1.4  2009/03/02 09:01:52  rubikitch
;; * yaoddmuse.el integration.
;;   You can post images to Oddmuse wiki such as EmacsWiki.
;; * improve document.
;;
;; Revision 1.3  2009/02/22 09:19:58  rubikitch
;; * Added parent group
;; * Current directory scheme
;;
;; Revision 1.2  2009/02/22 05:33:42  rubikitch
;; release
;;
;; Revision 1.1  2009/02/22 04:49:16  rubikitch
;; Initial revision
;;

;;; Code:

(defvar screenshot-version "$Id: screenshot.el,v 1.9 2010/05/04 08:59:08 rubikitch Exp $")
(eval-when-compile (require 'cl))
(require 'yaoddmuse nil t)              ;optional

;;; (@* "Customize Variables")

(defgroup screenshot nil
  "screenshot"
  :group 'external)

(defcustom screenshot-schemes
  '(("local"
     :dir "~/images/")
    ("current-directory"
     :dir default-directory)
    ("remote-ssh"
     :dir "/tmp/"
     :ssh-dir "www.example.org:public_html/archive/"
     :url "http://www.example.org/archive/")
    ("EmacsWiki"
     :dir "~/.yaoddmuse/EmacsWiki/"
     :yaoddmuse "EmacsWiki")
    ("local-server"
     :dir "~/public_html/"
     :url "http://127.0.0.1/"))
  "*Screenshot configuration list."
  :type 'list  
  :group 'screenshot)
(defcustom screenshot-default-scheme nil
  "*Default scheme name of screenshot.el.
If nil, the last-used scheme is used."
  :type 'string  
  :group 'screenshot)
(defcustom screenshot-take-delay 0.5
  "*Delay time to take a screenshot.
It is recommend to have a delay time to enable us to take a screenshot other than emacs."
  :type 'float
  :group 'screenshot)

;;; (@* "Internal Variables")
(defvar screenshot-image-filename nil)
(defvar screenshot-current-scheme nil)
(defvar screenshot-prepare-minor-mode nil)
(defvar screenshot-last-scheme-name nil)
(defvar screenshot-prepare-minor-mode-map (make-sparse-keymap))
(define-key screenshot-prepare-minor-mode-map "\C-c\C-c" 'screenshot-take)
(add-to-list 'minor-mode-map-alist
             (cons 'screenshot-prepare-minor-mode screenshot-prepare-minor-mode-map))

;;; (@* "Commands")
;;;###autoload
(defun screenshot (filename &optional scheme nomsg)
  "Prepare to take a screenshot to FILENAME with SCHEME.
After pressing C-c C-c, executing `screenshot-take'.
See also `screenshot-take' docstring. "
  (interactive "sScreenshot image filename: ")
  (unless scheme
    (setq scheme (if (>= emacs-major-version 23)
		     (completing-read "Scheme: " (mapcar 'car screenshot-schemes)
                                  nil t (or screenshot-default-scheme
					    screenshot-last-scheme-name))
		   (completing-read "Scheme: " (mapcar 'car screenshot-schemes)
                                  nil t nil nil (or screenshot-default-scheme
						    screenshot-last-scheme-name)))))
  (setq screenshot-last-scheme-name scheme)
  (setq screenshot-current-scheme (cdr (assoc scheme screenshot-schemes)))
  (setq screenshot-image-filename
        (expand-file-name
         filename
         (screenshot-get-directory (plist-get screenshot-current-scheme :dir))))
  (if (and (file-exists-p screenshot-image-filename)
           (y-or-n-p (format "%s already exists. Retry? " screenshot-image-filename)))
      (call-interactively 'screenshot)
    (setq screenshot-prepare-minor-mode t)
    (or nomsg (message "Press C-c C-c to take a screenshot!"))))

;;;###autoload
(defun screenshot-take ()
  "Take a screenshot configured by `screenshot' command.

- Taking a screenshot is delayed by `screenshot-take-delay'.
- Execute import command (ImageMagick).
- If the scheme has :ssh-dir property, use scp to send image to remote host.
  It is executed asynchronously to suppress a wait.
- Finally put the image URL or filename to kill-ring.
  Press C-y to input the image URL.
  If the scheme has :yaoddmuse, [[image:FileName]] is yanked.
"
  (interactive)
  (run-with-idle-timer screenshot-take-delay nil 'screenshot-take-internal))

;;; (@* "Utility Functions")
(defun screenshot-get-directory (arg)
  (cond ((stringp arg) arg)
        ((boundp arg) (symbol-value arg))
        ((functionp arg) (funcall arg))))

(defun screenshot-take-internal ()
  (screenshot-do-import screenshot-image-filename)
  (setq screenshot-prepare-minor-mode nil)
  (let (dir url wiki pagename)
    (cond ((setq dir (plist-get screenshot-current-scheme :ssh-dir))
           (screenshot-do-scp screenshot-image-filename dir))
          ((setq wiki (plist-get screenshot-current-scheme :yaoddmuse))
           (yaoddmuse-post-file screenshot-image-filename wiki
                                (setq pagename
                                      (file-name-sans-extension
                                       (file-name-nondirectory screenshot-image-filename)))
                                "Screenshot by screenshot.el")))
    (let ((img-url (cond ((setq url (plist-get screenshot-current-scheme :url))
			  (concat url (file-name-nondirectory screenshot-image-filename)))
			 (pagename
			  (format "[[image:%s]]" pagename))
			 (t
			  screenshot-image-filename))))
	 (kill-new img-url)
	 (message "Image URL: %s" img-url))))

(defun screenshot-do-import (filename)
  (call-process "import" nil nil nil filename))
(defun screenshot-do-scp (filename dir)
  (start-process "scp" "scp" "scp" filename dir))

;;;; Bug report
(defvar screenshot-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar screenshot-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of screenshot.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"screenshot.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defun screenshot-send-bug-report ()
  (interactive)
  (reporter-submit-bug-report
   screenshot-maintainer-mail-address
   "screenshot.el"
   (apropos-internal "^screenshot-" 'boundp)
   nil nil
   screenshot-bug-report-salutation))

;;;; (@* "unit test")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "screenshot")
      (expect "/path/to/test.png"
        (let ((screenshot-schemes '(("local" :dir "/path/to/")))
              screenshot-prepare-minor-mode)
          (screenshot "test.png" "local")
          screenshot-image-filename))
      (expect '(:dir "/path/to/")
        (let ((screenshot-schemes '(("local" :dir "/path/to/")))
              screenshot-prepare-minor-mode)
          (screenshot "test.png" "local")
          screenshot-current-scheme))
      (expect t
        (let ((screenshot-schemes '(("local" :dir "/path/to/")))
              screenshot-prepare-minor-mode)
          (screenshot "test.png" "local")
          screenshot-prepare-minor-mode))
      (expect "local"
        (let (screenshot-default-scheme
              (screenshot-schemes '(("local" :dir "/path/to/")))
              screenshot-prepare-minor-mode)
          (screenshot "test.png" "local")
          screenshot-last-scheme-name))
      (desc "screenshot-get-directory")
      (expect "/path/to/"
        (screenshot-get-directory "/path/to/"))
      (expect "/images/"
        (let ((default-directory "/images/"))
          (screenshot-get-directory 'default-directory)))
      (expect "/img/"
        (flet ((getdir () "/img/"))
          (screenshot-get-directory 'getdir)))

      (desc "screenshot-take-internal")
      (expect (mock (screenshot-do-import "/path/to/test.png"))
        (let ((screenshot-image-filename "/path/to/test.png"))
          (screenshot-take-internal)))
      (expect nil
        (stub screenshot-do-import)
        screenshot-prepare-minor-mode)
      (expect (mock (kill-new "/path/to/test.png"))
        (stub screenshot-do-import)
        (let ((screenshot-image-filename "/path/to/test.png"))
          (screenshot-take-internal)))
      (expect (mock (screenshot-do-scp "/path/to/test.png" "remote:pulic_html/archive/"))
        (stub screenshot-do-import)
        (stub kill-new)
        (let ((screenshot-image-filename "/path/to/test.png")
              (screenshot-current-scheme '(:dir "/path/to/"
                                           :ssh-dir "remote:pulic_html/archive/"
                                           :url "http://www.example.com/archive/")))
          (screenshot-take-internal)))
      (expect (mock (kill-new "http://www.example.com/archive/test.png"))
        (stub screenshot-do-import)
        (stub screenshot-do-scp)
        (let ((screenshot-image-filename "/path/to/test.png")
              (screenshot-current-scheme '(:dir "/path/to/"
                                           :ssh-dir "remote:pulic_html/archive/"
                                           :url "http://www.example.com/archive/")))
          (screenshot-take-internal)))
      (desc "post with yaoddmuse")
      (expect (mock (yaoddmuse-post-file
                     "/path/to/TestImage.png" "EmacsWiki"
                     "TestImage" "Screenshot by screenshot.el"))
        (stub screenshot-do-import)
        (stub screenshot-do-scp)
        (let ((screenshot-image-filename "/path/to/TestImage.png")
              (screenshot-current-scheme '(:dir "/path/to/"
                                           :yaoddmuse "EmacsWiki")))
          (screenshot-take-internal)))
      (expect (mock (kill-new "[[image:TestImage]]"))
        (stub screenshot-do-import)
        (stub screenshot-do-scp)
        (stub yaoddmuse-post-file)
        (let ((screenshot-image-filename "/path/to/TestImage.png")
              (screenshot-current-scheme '(:dir "/path/to/"
                                           :yaoddmuse "EmacsWiki")))
          (screenshot-take-internal)))
      )))


(provide 'screenshot)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "screenshot.el")
;;; screenshot.el ends here
