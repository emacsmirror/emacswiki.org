;;; rcov.el --- colorize untested ruby code

;; Copyright (C) 2008 by Ryan Davis

;; Author: Ryan Davis <ryand-ruby@zenspider.com>
;; Version 1.2
;; Keywords: no-freakin-clue
;; Created: 2008-01-14
;; Compatibility: Emacs 23, 22, 21?
;; URL(en): http://seattlerb.rubyforge.org/

;;; Posted using:
;; (emacswiki-post "rcov-overlay.el")

;;; The MIT License:

;; http://en.wikipedia.org/wiki/MIT_License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package provides the ability to highlight untested code
;; according to rcov by using rcov's coverage.info serialized
;; data. Also provided are two rake tasks to help generate the needed
;; data.

;; The function `overlay-current-buffer-with-command` is actually
;; quite flexible as it will execute an external command that returns
;; json data specifying regions and colors. It could be used for all
;; sorts of mischief.

;; Special thanks to Phil Hagelberg for writing augment, a much more
;; complete package for doing similar things (but still alpha enough
;; that I needed something working now).

;;; History:

;; 1.2 2009-10-21 Added customizable overlay background color.
;; 1.1 2008-12-01 Added find-project-dir to fix path generation issues.
;; 1.0 2008-01-14 Birfday.

(require 'cl)
(require 'json) ;; From: http://edward.oconnor.cx/2006/03/json.el

;; (global-set-key (kbd "C-c C-r")   'rcov-buffer)

;; 
;; If you use hoe and autotest with the autotest/rcov plugin, all of
;; this works straight up. Just fire up autotest, let it do its thing,
;; and you can trigger rcov-buffer to see the coverage on the file.

;;
;; If you do NOT use hoe, then you should add the following to your rake tasks:
;; Add this to your Rakefile:
;;
;; task :rcov_info do
;;   ruby "-Ilib -S rcov --text-report --save coverage.info test/test_*.rb"
;; end

;; task :rcov_overlay do
;;   rcov, eol = Marshal.load(File.read("coverage.info")).last[ENV["FILE"]], 1
;;   puts rcov[:lines].zip(rcov[:coverage]).map { |line, coverage|
;;     bol, eol = eol, eol + line.length
;;     [bol, eol, "#ffcccc"] unless coverage
;;   }.compact.inspect
;; end

(defcustom rcov-overlay-fg-color
  "#ffcccc"
  "The default background color."
  :group 'rcov-overlay
  :type 'color)

(defun find-project-dir (file &optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir file))
      dir
    (if (equal dir "/")
        nil
      (find-project-dir file (expand-file-name (concat dir "../"))))))

(defun overlay-current-buffer-with-command (cmd)
  "cmd must output serialized json of the form [[start stop color] ...]"
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json (shell-command-to-string cmd))
         (ranges (json-read-from-string json)))
    (rcov-clear)
    (dolist (range ranges)
    (overlay-put
     (make-overlay (car range) (cadr range))
     'face (cons 'background-color rcov-overlay-fg-color)))))

(defun rcov-buffer (buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (overlay-current-buffer-with-command
     (concat "rake -s rcov_overlay FILE=\""
             (file-relative-name
              (buffer-file-name)
              (find-project-dir "coverage.info")) "\" 2>/dev/null"))))

(defun rcov-clear ()
  (interactive)
  (remove-overlays))

(provide 'rcov-overlay)
