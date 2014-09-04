;;; w32-msgbox.el --- open a message box using VBS MsgBox function

;; Copyright (C) 2004 Mathias Dahl

;; Version: 0.3
;; Keywords: w32, message box, vbscript
;; Author: Mathias Dahl <mathias@SPAM_IS_BAD.VERY_BAD.dahl.net>
;; Maintainer: Mathias Dahl
;; URL: http://groups.google.com/groups?q=w32-msgbox

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; w32-msgbox is mostly a toy I created when I was bored. Maybe
;; someone will find it useful for a real-world application? It
;; provides a way to display message boxes of different styles. The
;; message box will return a value making it possible to interact with
;; the user.

;;; Prerequistites:

;; * It needs cscript.exe to be able to execute the VBS-script doing
;;   the acutal work.

;;; Usage:

;; Simple example:
;; (w32-msgbox "My message" "My title" 'vb-yes-no-cancel 'vb-question 
;; 'vb-default-button-2 t)

;;; History:

;; New in version 0.3
;; * Dave Pearson did the code much more lispish and fixed the problem
;;   with multiple instances of w32-msgbox using the same temp file.

;; New in version 0.2
;; * Added all the stuff I wanted, like the different options to
;;   MsgBox, and a way to check what the user press.

;;; Bugs:

;; No known bugs.

;;; Future:

;; Well, I'm open for all suggestions.

(defvar w32-msgbox-types
  '((vb-ok-only . 0)
    (vb-ok-cancel . 1)
    (vb-abort-retry-ignore . 2)
    (vb-yes-no-cancel . 3)
    (vb-yes-no . 4)
    (vb-retry-cancel . 5)))

(defvar w32-msgbox-icons
  '((vb-critical . 16)
    (vb-question . 32)
    (vb-exclamation . 48)
    (vb-information . 64)))

(defvar w32-msgbox-default-buttons
  '((vb-default-button-1 . 0)
    (vb-default-button-2 . 256)
    (vb-default-button-3 . 512)
    (vb-default-button-4 . 768)))

(defvar w32-msgbox-results
  '((1 . vb-ok)
    (2 . vb-cancel)
    (3 . vb-abort)
    (4 . vb-retry)
    (5 . vb-ignore)
    (6 . vb-yes)
    (7 . vb-no)))

(defvar w32-msgbox-system-modal 4096)

(defun w32-msgbox (MESSAGE &optional TITLE TYPE ICON DEFAULTBUTTON
                           SYSTEMMODAL)
  "Use vbscript's MsgBox to create a message box with the text MESSAGE
and with optional window title TITLE.

TYPE, a symbol, may be one of the following:

vb-ok-only                  Display OK button only.
vb-ok-cancel                Display OK and Cancel buttons.
vb-abort-retry-ignore       Display Abort, Retry, and Ignore buttons.
vb-yes-no-cancel            Display Yes, No, and Cancel buttons.
vb-yes-no                   Display Yes and No buttons.
vb-retry-cancel             Display Retry and Cancel buttons.

ICON, a symbol, may be one of the following:

vb-critical                Display Critical Message icon.
vb-question                Display Warning Query icon.
vb-exclamation             Display Warning Message icon.
vb-information             Display Information Message icon.

DEFAULTBUTTON, a symbol, may be one of the following:

vb-default-button-1          First button is default.
vb-default-button-2          Second button is default.
vb-default-button-3          Third button is default.
vb-default-button-4          Fourth button is default.

The last argument, SYSTEMMODAL, if not `nil', determines if the
message box will be system modal or not.

The return value is one of the following symbols:

vb-ok
vb-cancel
vb-abort
vb-retry
vb-ignore
vb-yes
vb-no

"
  (with-temp-buffer
    (let ((options (+ 0
                      (if TYPE
                          (cdr (assoc TYPE w32-msgbox-types)) 0)
                      (if ICON
                          (cdr (assoc ICON w32-msgbox-icons)) 0)
                      (if DEFAULTBUTTON
                          (cdr (assoc DEFAULTBUTTON
                                      w32-msgbox-default-buttons)) 0)
                      (if SYSTEMMODAL   w32-msgbox-system-modal 0)))
          (vbs (make-temp-file "w32-msgbox")))
      (unwind-protect
          (progn
            (insert (concat "WScript.Quit(MsgBox(\"" MESSAGE
                            "\"" ", "
                            (int-to-string options) ", \""
                            (if (not TITLE)
                                "MsgBox 0.3"
                              TITLE) "\"))"))
            (write-region (point-min) (point-max) vbs)
            (cdr (assoc (call-process "cscript" nil
                                      "*cscript*" t "//e:vbscript"
                                      vbs) w32-msgbox-results)))
        (delete-file vbs)))))

(provide 'w32-msgbox)

;;; w32-msgbox.el ends here
