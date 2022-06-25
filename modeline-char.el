;;; modeline-char.el --- In the mode-line, show the value of the character after point.
;;
;; Filename: modeline-char.el
;; Description: In the mode-line, show the value of the character after point.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2015-2022, Drew Adams, all rights reserved.
;; Created: Tue Jul  7 10:52:36 2015 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Sat Jun 25 10:13:29 2022 (-0700)
;;           By: dradams
;;     Update #: 171
;; URL: https://www.emacswiki.org/emacs/download/modeline-char.el
;; Doc URL: https://www.emacswiki.org/emacs/ModeLineCharacterInfo
;; Keywords: mode-line, character
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x, 26.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  In the mode-line, show the value of the character after point.
;;
;;  Use minor mode `mlc-char-in-mode-line-mode' to show the value of
;;  the character after point.  Use global minor mode
;;  `mlc-char-in-mode-line-mode-global' to do this in every buffer.
;;
;;  The character is shown (in the mode-line) highlighted with face
;;  `mlc-mode-line-char-format', followed by `=', followed by the
;;  hexadecimal Unicode code point for the character, highlighted with
;;  face `mlc-mode-line-char-format-code'.
;;
;;  For example, with point before the character `e' it shows:
;;
;;    e=000065
;;
;;  with `e' and `000065' highlighted.
;;
;;  You thus have a dynamically updated view at all times of what the
;;  character at the cursor is.
;;
;;  In addition:
;;
;;   * If you click `mouse-1' on the lighter then full information
;;     about the character is shown in buffer `*Help*', including the
;;     font and faces used (for the character in the buffer, not for
;;     the lighter in the mode-line).
;;
;;   * If you click `mouse-2' on the lighter then the character is
;;     copied to the secondary selection.
;;
;;   * If you click `mouse-3' on the lighter then the character is
;;     shown enlarged in a tooltip.
;;
;;
;;  Commands defined here:
;;
;;    `mlc-char-in-mode-line-mode', `mlc-char-in-mode-line-mode-global'.
;;
;;  Faces defined here:
;;
;;    `mlc-mode-line-char-format', `mlc-mode-line-char-format-code'.
;;
;;  Non-interactive functions defined here:
;;
;;    `mlc-copy-char-to-second-sel',
;;    `mlc-turn-on-char-in-mode-line-mode'.
;;
;;  Internal variables defined here:
;;
;;    `mlc-mode-line-char-format',
;;    `mlc-char-in-mode-line-mode-initialized'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2022/06/25 dadams
;;     mlc-char-in-mode-line-mode: Removed optional args for define-minor-mode - use only keywords.
;; 2016/12/10 dadams
;;     mlc-copy-char-to-second-sel: x-set-selection -> gui-set-selection for Emacs 25+.
;; 2015/07/10 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defface mlc-mode-line-char-format
  '((t (:foreground "Red" :background "LightGreen")))
  "Mode-line face for character after point."
  :group 'Modeline)

(defface mlc-mode-line-char-format-code
  '((t (:foreground "Blue")))
  "Mode-line face for code point of character after point."
  :group 'Modeline)

;;;###autoload (put 'mlc-mode-line-char-format 'risky-local-variable t)
(defvar mlc-mode-line-char-format
  '(:eval (and mlc-char-in-mode-line-mode
           (let* ((ch   (following-char))
                  (str  (format (if (= ?% ch) "[%%%c=%06x] " "[%c=%06x] ") ch ch))
                  (map  (make-sparse-keymap)))
             (define-key map [mode-line down-mouse-1] nil)
             (define-key map [mode-line mouse-1] (lambda (ev) (interactive "e") (describe-char (point))))
             (define-key map [mode-line down-mouse-2] nil)
             (define-key map [mode-line mouse-2] (lambda (ev)
                                                   (interactive "e")
                                                   (mlc-copy-char-to-second-sel (point) t)))
             (define-key map [mode-line down-mouse-3] nil)
             (define-key map [mode-line mouse-3] (lambda (ev)
                                                   (interactive "e")
                                                   (x-show-tip
                                                    (propertize (string (char-after))
                                                                'face `(:foreground "red" :height 400)))))
             (add-text-properties 1 2                  '(face mlc-mode-line-char-format) str)
             (add-text-properties 3 (- (length str) 2) '(face mlc-mode-line-char-format-code) str)
             (add-text-properties 1 (- (length str) 2) `(mouse-face mode-line-highlight
                                                                    help-echo "mouse-1: info; mouse-2: \
copy to second sel; mouse-3: large tooltip"
                                                                    local-map ,map)
                                  str)
             str)))
  "Mode-line format spec to display a character.")

(defvar mlc-char-in-mode-line-mode-initialized nil
  "Non-nil if `mlc-char-in-mode-line-mode' has been called.")

;;;###autoload
(define-minor-mode mlc-char-in-mode-line-mode
  "Show char after point in mode line, at start of `global-mode-string'."
  :group 'Modeline
  (unless mlc-char-in-mode-line-mode-initialized
    (setq mlc-char-in-mode-line-mode-initialized  t)
    (setq global-mode-string  (cond ((consp global-mode-string)
                                     (add-to-list 'global-mode-string mlc-mode-line-char-format))
                                    ((not global-mode-string)
                                     (list mlc-mode-line-char-format))
                                    ((stringp global-mode-string)
                                     (list mlc-mode-line-char-format global-mode-string))))))

;;;###autoload
(define-globalized-minor-mode mlc-char-in-mode-line-mode-global
    mlc-char-in-mode-line-mode mlc-turn-on-char-in-mode-line-mode)

(defun mlc-turn-on-char-in-mode-line-mode ()
  "Turn on `mlc-char-in-mode-line-mode'."
  (mlc-char-in-mode-line-mode 1))

;; Same as `apu-copy-char-to-second-sel' in `apu.el'.
(defun mlc-copy-char-to-second-sel (position msgp)
  "Copy char at POSITION in current buffer to secondary selection.
If you have library `second-sel.el' then this also copies it to the
`secondary-selection-ring'."
  (let* ((char  (char-after position))
         (strg  (string char)))
    (if (fboundp 'gui-set-selection)
        (gui-set-selection 'SECONDARY strg) ; Emacs 25.1+.
      (x-set-selection 'SECONDARY strg))
    (if mouse-secondary-overlay
        (move-overlay mouse-secondary-overlay position (1+ position) (current-buffer))
      (setq mouse-secondary-overlay  (make-overlay position (1+ position) (current-buffer)))
      (overlay-put mouse-secondary-overlay 'face 'secondary-selection))
    (when (require 'second-sel nil t) (add-secondary-to-ring strg))
    (when msgp (message "Copied char `%s' to secondary selection%s"
                        strg (if (require 'second-sel nil t) " ring" "")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'modeline-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; modeline-char.el ends here
