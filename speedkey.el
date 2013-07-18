;;; speedkey.el -- simple keybindings with documentation
;;
;; Copyright (C) 2013  Sam Flint <swflint@flintfam.org>
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(defvar speedkey-help-text "SpeedKey Help

KeySequence \t Help Text" "SpeedKey Help Text")

(defvar speedkey-prefix)
(defvar speedkey-prefix-text-rep)

(defun speedkey-create-binding (key command help) "Takes the key with which to bind the command, and help"
  (global-set-key (concat speedkey-prefix key) command)
  (setq speedkey-help-text (concat speedkey-help-text "\n" speedkey-prefix-text-rep " " key "\t" help)))

(defun speedkey-help () "Displays SpeedKey Help"
  (interactive)
  (generate-new-buffer "*SpeedKey Help*")
  (with-current-buffer "*SpeedKey Help*"
    (erase-buffer)
    (insert speedkey-help-text)(display-buffer "*SpeedKey Help*")))

(defun speedkey-init (prefix-key prefix-key-text-rep) "Initializes SpeedKey"
  (setq speedkey-prefix prefix-key)
  (setq speedkey-prefix-text-rep prefix-key-text-rep)
  (speedkey-create-binding "h" 'speedkey-help "Open the SpeedKey Help")
  (message "SpeedKey Initialized"))

(provide 'speedkey)
;;; speedkey.el ends here
