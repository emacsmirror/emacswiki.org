;;; anything-c-shell-history.el --- shell history anything.el interface

;; Copyright (C) 2008-2009 by 101000code/101000LAB, all rights reserved.
;; Copyright (C) 2009, IMAKADO, all rights reserved.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
;;
;; Version: 0.0.8
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;;         IMAKADO
;; URL: http://code.101000lab.org, http://trac.codecheck.in http://d.hatena.ne.jp/IMAKADO

;;; Commentary:
;;
;; This package use `anything' as a interface to execute *sh_history.
;;
;; You can make this package integrate with `anything',
;; just setup like below:
;;
;; (setq anything-sources
;;       (list
;;        anything-c-source-shell-history
;;        ))

;;; Installation:
;;
;; Put anything-etags.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-c-shell-history)
;;
;; No need more.

;;; Change log:
;;
;; 0.0.8
;;   * k1LoW:
;;      * Applied IMAKADO's big patch to New Action.
;;      * New Action Execute History Asynchronous whitch excecute command asynchronous.
;;
;; 0.0.7
;;   * k1LoW:
;;      * Fix doc.
;;
;; 0.0.6
;;   * k1LoW:
;;      * Test support .zsh_history.
;;
;; 0.0.5
;;   * k1LoW:
;;      * display current directory, when "Edit And Execute"
;;      * Refactor code.
;;
;; 0.0.4
;;   * k1LoW:
;;      * support shell-history.el http://www.emacswiki.org/cgi-bin/wiki/download/shell-history.el
;;
;; 0.0.3
;;   * k1LoW:
;;      * Refactor code. ref http://d.hatena.ne.jp/rubikitch/20080908/anything
;;
;; 0.0.2
;;   * k1LoW:
;;      * New anything action "Edit And Execute" which can edit and execute .*sh_history.
;;
;; 0.0.1
;;   * k1LoW:
;;      * First released.

;;; TODO
;; use ansi-term ?

;;; Code:

(require 'anything)
(require 'timer)
(defvar anything-c-shell-history-file "~/.bash_history")

(defun anything-c-shell-history-command()
  "Make command"
  (if (string-match "zsh_history" anything-c-shell-history-file)
      (concat "cut -d ';' -f 2 " anything-c-shell-history-file)
    (concat "less " anything-c-shell-history-file)))

(defvar anything-c-source-shell-history
  '((name . "Shell History")
    (init
     . (lambda ()
         (call-process-shell-command
          (anything-c-shell-history-command) nil (anything-candidate-buffer 'global))))
    (candidates-in-buffer)
    (search-from-end)
    (action
     ("Execute History" . (lambda (candidate)
                            (save-excursion
                              (message "wait....")
                              (call-process-shell-command candidate nil (get-buffer-create "*Shell Command*"))
                              (if (featurep 'shell-history)
                                  (add-to-shell-history candidate))
                              (switch-to-buffer "*Shell Command*")
                              (message (delete-and-extract-region (point-min) (point-max)))
                              (set-buffer-modified-p (buffer-modified-p))
                              (kill-buffer "*Shell Command*"))))
     ("Edit and Execute" . (lambda (candidate)
                             (let ((command ""))
                               (save-excursion
                                 (setq command (read-from-minibuffer (concat "command:(" default-directory ") ") candidate))
                                 (message "wait....")
                                 (call-process-shell-command command nil (get-buffer-create "*Shell Command*"))
                                 (if (featurep 'shell-history)
                                     (add-to-shell-history command))
                                 (switch-to-buffer "*Shell Command*")
                                 (message (delete-and-extract-region (point-min) (point-max)))
                                 (set-buffer-modified-p (buffer-modified-p))
                                 (kill-buffer "*Shell Command*")))))
     ("Execute History Asynchronous" . anything-c-shell-history-execute-history-async))))

(defun anything-c-shell-history-execute-history-async (candidate)
  (anything-c-shell-history-async-do
   :command candidate
   :buffer-name "*Shell Command*"
   :args nil
   :callback (lambda ()
               (kill-new (format "%s" (current-buffer)))
               (run-with-timer ;; called after with-current-buffer... in `anything-c-shell-history-async-do'
                0.1
                nil
                (switch-to-buffer (current-buffer))))))

(defun* anything-c-shell-history-async-do
    (&key command args buffer-name
          (callback 'identity)
          (errorback (lambda() (message (buffer-string)))))
  (lexical-let ((buf (get-buffer-create buffer-name))
                (callback callback)
                (errorback errorback))
    (lexical-let
      ((sentinel (lambda (proc event)
         (cond ((and (string= event "finished\n")
                     (= (process-exit-status proc) 0))
                (with-current-buffer buf
                  (funcall callback)))
               ((and (string= event "finished\n")
                     (/= (process-exit-status proc) 0))
                (with-current-buffer buf
                  (funcall errorback)))))))
      (set-process-sentinel (apply 'start-process-shell-command command buf command args) sentinel))))

(provide 'anything-c-shell-history)

;;; Code ends
