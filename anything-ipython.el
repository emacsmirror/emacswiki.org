;;; anything-ipython.el --- 
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; 
;; Created: sam. juil. 25 18:48:31 2009 (+0200)
;; Version: 
;; X-URL: http://mercurial.intuxication.org/hg/anythingipython
;; Keywords: ipython, python, completion. 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary: 
;;  ==========
;;
;; Tested on emacs23.1 with python2.6, ipython-9.1 and python-mode.el.
;; This file fix also normal completion (tab without anything) in the ipython-shell.
;; This file reuse some code of ipython.el.
;;
;;  Dependencies:
;;  ============
;;
;;  ipython (http://ipython.scipy.org/), ipython.el, python-mode.el.
;;  It's better to have rlcompleter2 (http://codespeak.net/rlcompleter2/) 
;;  Note that to use rlcompleter2, you have to add these lines in your
;;  your ~/.ipython/ipy_user_conf.py
;;
;;     import rlcompleter2
;;     rlcompleter2.setup()
;;
;;  You may want to use also anything-show-completion.el:(facultative)
;;  http://www.emacswiki.org/cgi-bin/emacs/anything-show-completion.el
;;
;;  Install: 
;;  =======
;;
;; Setup anything python:
;; Put this file in your load path.
;; Add to .emacs:
;;
;; (require 'anything-ipython)
;; (add-hook 'python-mode-hook #'(lambda ()
;;                                 (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
;; (add-hook 'ipython-shell-hook #'(lambda ()
;;                                   (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
;;
;; If you want to use anything-show-completion.el,(facultative)
;; <http://www.emacswiki.org/cgi-bin/emacs/anything-show-completion.el>
;; add these lines:
;;
;; (when (require 'anything-show-completion nil t)
;;   (use-anything-show-completion 'anything-ipython-complete
;;                                 '(length initial-pattern)))
;;
;;  Usage: 
;;  =====
;; 1) From your *.py file, start interpreter with C-c !
;; 2) Import module(s) you need for completion from interpreter.
;;    e.g "import os"
;;    You can also import all import entries of your current *.py file
;;    with `anything-ipython-import-modules-from-buffer'.
;;    Note that `py-execute-buffer' (C-c C-c) will load also all modules
;;    of your .py file.
;; 3) Use M-x anything-ipython-complete or M-<tab> to have completion.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; http://mercurial.intuxication.org/hg/anythingipython
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; <2009-07-25 Sam. 18:03>

(eval-when-compile (require 'cl))
(require 'ipython)

;; Fix some bugs in ipython.el:
(define-key py-shell-map (kbd "\t") 'ipython-complete)
(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")

(defadvice ipython-shell-hook (after unset-completion-key () activate)
  (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete))

;; Modify original `ipython-complete' to fit with anything.
(defun anything-ipython-completion-list (pattern)
  "Try to complete the python symbol before point.
Only knows about the stuff in the current *Python* session.
Return a completion list according to `pattern'."
  (interactive)
  (let* ((ugly-return                       nil)
         (sep                               ";")
         (ipy-shell-proc                    (get-buffer-process (current-buffer)))
         (loc-py-buff-proc                  (get-process py-which-bufname))
         (python-process                    (or ipy-shell-proc loc-py-buff-proc)) 
         (cmd-args                          (format ipython-completion-command-string pattern))
         (completions                       nil)
         (completion-table                  nil)
         ;; Filter out all SGR control sequences from string.
         ;; i.e transform all shell color char from the ipython output
         ;; in text properties understandable by lisp.
         (comint-preoutput-filter-functions (append
                                             comint-preoutput-filter-functions 
                                             '(ansi-color-filter-apply
                                               (lambda (string) 
                                                 (setq ugly-return (concat ugly-return string))
                                                 "")))))
    (process-send-string python-process cmd-args)
    (accept-process-output python-process)
    (setq completions  ; ipython completion return string like a;b;c;d;e\n
          (split-string (substring ugly-return 0 (position ?\n ugly-return)) sep))
                                        ; (message (format "DEBUG completions: %S" completions))
    (setq completion-table (loop for str in completions
                              collect (list str nil)))
                                        ; (message (format "DEBUG completions: %S" completion-table))
    (all-completions pattern completion-table)))

(defun anything-ipyton-default-action (elm)
  "Insert completion at point."
  (let ((initial-pattern (anything-ipython-get-initial-pattern)))
    (delete-char (- (length initial-pattern)))
    (insert elm)))

(defvar anything-source-ipython
  '((name . "Ipython completion")
    (candidates . (lambda ()
                    (condition-case nil
                        (anything-ipython-completion-list anything-pattern)
                      (error nil))))
    (action . anything-ipyton-default-action)
    (volatile)
    (requires-pattern . 2)))

;; (anything 'anything-source-ipython)

(defun anything-ipython-get-initial-pattern ()
  "Get the pattern to complete from."
  (let ((beg (save-excursion
               (skip-chars-backward "a-z0-9A-Z_./" (point-at-bol))
               (point))) 
        (end (point)))
    (buffer-substring-no-properties beg end)))

(defun anything-ipython-complete ()
  "Preconfigured anything for ipython completions."
  (interactive)
  (delete-other-windows)
  (let ((initial-pattern (anything-ipython-get-initial-pattern)))
    (anything 'anything-source-ipython initial-pattern)))

(defun anything-ipython-import-modules-from-buffer ()
  "Allow user to execute only the import lines of the current *.py file."
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (catch 'break
      (while (not (eobp))
        (catch 'continue
          (if (re-search-forward "import .*" (point-max) t)
              (progn
                (sit-for 0.5)
                (py-execute-region (point-at-bol) (point-at-eol))
                (throw 'continue nil))
              (throw 'break nil))))))
  (message "All imports from `%s' done" (buffer-name)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide
(provide 'anything-ipython)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; anything-ipython.el ends here
