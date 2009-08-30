;;; telnet-contrib.el --- Telnet commands for Emacs on MS Windows.
;; 
;; Filename: telnet-contrib.el
;; Description: Telnet commands for Emacs on MS Windows.
;; Author: Zoltan Kemenczy, Ngai Kim Hoong
;; Maintainer: Drew Adams
;; Created: Tue Mar 16 15:56:00 2004
;; Version: 
;; Last-Updated: Fri Aug 04 01:06:45 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 33
;; Keywords: unix, comm
;; Compatibility: Emacs 20.x, Emacs 21.x, Emacs 22.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Two alternative versions of telnet functions for MS Windows.
;;
;; The first, called `telnet' here, uses a Windows version of telnet
;; by Igor Milavec that was modified to use stdio by Naftali Ramati
;; <naftali@harmonic.co.il> (includes source).  Unfortunately, this
;; telnet program requires that the host be specified on the command
;; line, but standard `telnet-mode' wants to specify the host using
;; the "open" command once telnet has started. Zoltan Kemenczy
;; <zoltan@nabu.isg.mot.com> wrote this version of `telnet-mode',
;; which does this.
;; 
;; The second, called `telnet-sailor' here, was written by Ngai Kim
;; Hoong <wsailor@hotpop.com> for use with Cygwin. (This one doesn't
;; hide your password.)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(require 'telnet)

;;; Changed `pop-to-buffer' to `switch-to-buffer-other-window'.
(defun telnet (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*telnet-HOST*'.
Normally input is edited in Emacs and sent a line at a time."
  (interactive "sOpen telnet connection to host: ")
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
         (name (concat "telnet-" (comint-arguments host 0 nil) ))
	 (buffer (get-buffer (concat "*" name "*")))
	 process)
    (cond ((string-equal system-type "windows-nt")
           (setq telnet-new-line "\n")))
    (if (and buffer (get-buffer-process buffer))
	(pop-to-buffer (concat "*" name "*"))
      (switch-to-buffer-other-window
       (make-comint name telnet-program nil host))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'telnet-initial-filter)
      (accept-process-output process)
      (telnet-mode)
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count))))

;;; Changed `pop-to-buffer' to `switch-to-buffer-other-window'.
(defun telnet-sailor (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*PROGRAM-HOST*'
where PROGRAM is the telnet program being used.  This program
is controlled by the contents of the global variable
`telnet-host-properties', falling back on the value of the
global variable `telnet-program'. Normally input is edited
in Emacs and sent a line at a time."
  (interactive "sOpen connection to host: ")
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
         (properties (cdr (assoc host telnet-host-properties)))
         (telnet-program (if properties (car properties) telnet-program))
         (name (concat telnet-program "-" (comint-arguments host 0 nil) ))
         (buffer (get-buffer (concat "*" name "*")))
         (telnet-options (if (cdr properties) (cons "-l" (cdr properties))))
         process)
    (if (and buffer (get-buffer-process buffer))
        (pop-to-buffer (concat "*" name "*"))
      (switch-to-buffer-other-window
       (apply 'make-comint name telnet-program nil telnet-options))
      (setq process (get-buffer-process (current-buffer)))
      ;;(set-process-filter process 'telnet-initial-filter)
      ;; Don't send the `open' cmd till telnet is ready for it.
      ;;(accept-process-output process)
      (erase-buffer)
      (send-string process (concat "open " host "\n"))
      (telnet-mode)
      (setq telnet-remote-echoes nil)
      (setq telnet-new-line "\n");; needed for cygwin 1.3.11
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count)
      (setq comint-process-echoes t)
      )))

(provide 'telnet-contrib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `telnet-contrib.el' ends here
