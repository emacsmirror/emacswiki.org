;;; screencast-record.el --- record your screencasts as movies

;; Copyright (C) 2009 ESBEN Andreasen <esbenandreasen@gmail.com>

;; Authors: esbenandreasen <esbenandreasen@gmail.com>(new)

;; Keywords: demo movie screencast

;; This file is not an official part of emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file allows you record your screencasts as movies in the .ogv format.

;;; Usage:

;; Requires wmctrl and recordmydesktop to be installed!

;; Install this file to an appropriate directory in your load-path,
;; and add these expressions to your ~/.emacs

;; (auto-load 'screencast-record "screencast-record")

;; Try it out by evaluating 

;; (screencast-record
;;  '("This is the test of the screencast-record" n
;;    "You can check the result in ~/screencast-record-test.ogv")
;;  "screencast-record-test")

;; THIS WILL MOST LIKELY REQUIRE A CHANGE TO THE VALUES OF THE FOLLOWING VARIABLES:

;; screencast-record-fill-column 
;; screencast-record-font

;; Your own screencast files which should be recorded should have a (require 'screencast-record)

;; A convenient way of making a screencast recordable is to use the prefix-argument

;; (defun my-screencast (&optional arg)
;;   (apply (if arg
;;              'screencast-record
;;            'screencast)
;;          list name ()))

;;; Code:
(require 'screencast)
(defvar screencast-record-fill-column 60 "The fill column value to be used during a recording session")
(defvar screencast-record-font
  "-Misc-Fixed-Medium-R-Normal--20-200-75-75-C-100-ISO8859-1"
  "The font to use during a recording session.")
(defvar screencast-record-dir "~/" "The directory to save recordings to")

(defun screencast-record (list name version)
  "Records the screencast as an .ogv-file. 
Refer to `screencast' for argument usage.
Changes the colors and font-size temporarily for better youtube-quality.
Requires wmctrl and recordmydesktop to be installed.  

A pause of a few seconds is injected into the beginning of the screencast:
- recordmydesktop can start slowly
- the video watcher might need a little time to prepare for the video.
The same is true for the end of the screencast."
  (let* (
         (frame-title-format-new "screencast-recording")
         (frame-title-format-old frame-title-format)
         (record-command-1  (concat
                             "recordmydesktop"
                             " "
                             "--no-sound"
                             " "
                             "--no-cursor"
                             " "
                             ;; "--windowid $(wmctrl -l | awk '/"frame-title-format-new"/ {print $1}')"
                             ;; " "
                             "--no-frame"
                             " "
                             "--overwrite"
                             " "
                             "-o "
                             ))
         (record-command-file
          (expand-file-name (concat screencast-record-dir name ".ogv")))
         (record-command (concat record-command-1 record-command-file " &"))
         (stop-record-command "kill -INT $(pgrep recordmydesktop)")
         (record-output-buffer (get-buffer-create "record-output-buffer"))
         (background-old (cdr (assoc 'background-color (frame-parameters))))
         (foreground-old (cdr (assoc 'foreground-color (frame-parameters))))
         (background-new "white")
         (foreground-new "black")
         (font-old (cdr (assoc 'font (frame-parameters))))
         (font-new screencast-record-font)
         (fill-column-old fill-column)
         (fill-column-new screencast-record-fill-column)
         (fill-column-function 
          `(progn (setq-default fill-column ,fill-column-new)))
         (list-with-fill-column (append 
                                 (list fill-column-function) list))
         (pauselist '(p p))
         (list-with-pauses (append pauselist list-with-fill-column pauselist))
         (init (list fill-column-function))
         )
    ;; change look
    (setq frame-title-format frame-title-format-new)
    (set-background-color background-new)
    (set-foreground-color foreground-new)
    (set-frame-font font-new)
    (message "Recording in 3 ...")
    (sit-for 1)
    (message "Recording in 2 ...")
    (sit-for 1)
    (message "Recording in 1 ...")
    (sit-for 1)
    (message "And action!")
    (shell-command record-command
                   record-output-buffer)
    (screencast list-with-pauses name version 0 init)
    (shell-command stop-record-command)
    ;; reset look
    (set-background-color background-old)
    (set-foreground-color foreground-old)
    (set-frame-font font-old)
    (setq frame-title-format frame-title-format-old)
    ;; show encoding progress
    (switch-to-buffer record-output-buffer)
    (message (concat "Recording being saved to" record-command-file))
    )
  )

(provide 'screencast-record)

