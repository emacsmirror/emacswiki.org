;;; key-choices.el --- Key Choices -- Also Viper has different colors in different modes
;;
;; Filename: key-choices.el
;; Description: Key Choices -- Different modes have different colors
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Nov  4 00:06:18 2010 (-0500)
;; Version:  0.201
;; Package-Requires: ((color-theme-vim-insert-mode "0.1") (color-theme-emacs-revert-theme "0.1"))
;; Last-Updated: Tue Feb  8 11:16:06 2011 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 14
;; URL:
;; Keywords: Viper, Ergoemacs, CUA, Crisp, EDT, TPU
;; Compatibility: 23.x
;;
;; Features that might be required by this library:
;;
;;   `cl', `color-theme', `cus-face', `easymenu', `wid-edit'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Put the library key-choices.el in the load path and then put
;; (require 'key-choices)
;; in your ~/.emacs file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 11-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Thu Nov 11 11:59:06 2010 (-0600) #6 (Matthew L. Fidler)
;;    Required vimpulse.  Fixed some error in R-mode (and possibly others)
;; 10-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Wed Nov 10 08:35:17 2010 (-0600) #2 (Matthew L. Fidler)
;;    Make sure to require `ergoemacs-mode' before loading (ergoemacs-mode)
;; 04-Nov-2010
;;    Initial Revision
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; Code:

(require 'color-theme)

(require 'easymenu)
;;(require 'vimpulse) ;; Require first.  For some reason if you don't there is some error in R.  It has to do with advices.

(defvar key-choices-emacs-default (current-global-map))

(defgroup key-choices nil
  "* Key choices menu"
  :group 'emulations
  )

(defcustom key-choices-keybinding 1
  "* Keybinding for emacs"
  :type '(choice (integer :tag "Emacs Keys" :value 1)
                 (integer :tag "VI Keys" :value 2)
                 (integer :tag "Windows Keys" :value 3)
                 (integer :tag "Ergonomic" :value 4)
                 (integer :tag "Ergonomic + Windows Keys"  :value 5)
                 (integer :tag "CRiSP/Breif" :value 6)
                 (integer :tag "EDT (DEC VMS editor)" :value 7)
                 (integer :tag "TPU (DEC VMS editor)" :value 8)
                 )
  :group 'key-choices
  )

(defcustom key-choices-keyboard "us"
  " * Type of keyboard that you are using"
  :type '(choice (string :tag "QWERTY" :value "us")
                 (string :tag "Dvorak" :value "dv")
                 (string :tag "Colemak" :value "colemak")
                 )
  :group 'key-choices
  )


(defvar key-choices-mode-menu-keys nil
  "Extra portions of a menu added to the main NONMEM menu."
  )
(setq key-choices-mode-menu-keys
      '("Key Bindings"
        ["CRiSP/Breif Keys" (key-choices-crisp) :style toggle :selected (= key-choices-keybinding 6)]
        ["EDT Keys" (key-choices-edt) :style toggle :selected (= key-choices-keybinding 7)]
        ["TPU Keys" (key-choices-tpu) :style toggle :selected (= key-choices-keybinding 8)]
        ["CUA Keys + Ergonomic Keys" (key-choices-ergonomic-cua) :style toggle :selected (= key-choices-keybinding 5) ]
        ["CUA Keys" (key-choices-cua) :style toggle :selected (= key-choices-keybinding 3)]
        ["Emacs Keys" (key-choices-emacs-keys) :style toggle :selected (= key-choices-keybinding 1)]
        ["Ergonomic Keys" (key-choices-ergonomic) :style toggle :selected (= key-choices-keybinding 4) ]
        ["VI Keys" (key-choices-viper) :style toggle :selected (= key-choices-keybinding 2)]
        "--"
        ("Assumed Ergonomic Keyboard"
         ["QWERTY" (key-choices-ergonomic-qwerty) :style toggle :selected (string= key-choices-keyboard "us") ]
         ["Dvorak" (key-choices-ergonomic-dvorak) :style toggle :selected (string= key-choices-keyboard "dv") ]
         ["Colemak" (key-choices-ergonomic-colemak) :style toggle :selected (string= key-choices-keyboard "colemak") ]
         )
        "--"
        ["Customize Color themes and options for keybindings." (customize-group "key-choices")]
        ))
(easy-menu-add-item nil '("Options" ) key-choices-mode-menu-keys "Shift movement mark region (CUA)")
(defun key-choices-unload-keys ()
  (when (and (boundp 'viper-mode) viper-mode)
    (toggle-viper-mode)
    )
  (if (and (fboundp 'cua-mode) cua-mode)
      (cua-mode -1))
  (if (fboundp 'ergoemacs-mode-version)
      (ergoemacs-mode nil)
    )
  (if (fboundp 'crisp-mode)
      (crisp-mode -1)
    )
  (if (and (fboundp 'edt-emulation-off)
           (boundp 'edt-orig-transient-mark-mode))
      (edt-emulation-off)
    )
  (if (fboundp 'tpu-edt-off)
      (tpu-edt-off)
    )
  (setq global-map key-choices-emacs-default)
  )
(defun key-choices-emacs-keys ()
  (interactive)
  (key-choices-unload-keys)
  (unless (= key-choices-keybinding 1)
    (setq key-choices-keybinding 1)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    (customize-save-customized)
    )
  (color-theme-emacs-revert-theme)
  (with-temp-buffer
    (insert key-choices-viper-emacs-color-theme)
    (eval-buffer)
    )
  )
(defun key-choices-cua ()
  (interactive)
  (key-choices-unload-keys)
  (cua-mode 1)
  (unless (= key-choices-keybinding 3)
    (setq key-choices-keybinding 3)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    (customize-save-customized)
    )
  (color-theme-emacs-revert-theme)
  (with-temp-buffer
    (insert key-choices-viper-cua-color-theme)
    (eval-buffer)
    )
  )
(defun key-choices-ergonomic ()
  (interactive)
  (key-choices-unload-keys)
  (unless (= key-choices-keybinding 4)
    (setq key-choices-keybinding 4)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    )
  (setenv "ERGOEMACS_KEYBOARD_LAYOUT" key-choices-keyboard) ; US layout
  (require 'ergoemacs-mode)
  (ergoemacs-mode 1)
  )
(defun key-choices-crisp ()
  (interactive)
  (key-choices-unload-keys)
  (unless (= key-choices-keybinding 6)
    (setq key-choices-keybinding 6)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    )
  (crisp-mode 1)
  )
(defun key-choices-edt ()
  (interactive)
  (key-choices-unload-keys)
  (unless (= key-choices-keybinding 7)
    (setq key-choices-keybinding 7)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    )
  (edt-emulation-on)
  )

(defun key-choices-tpu ()
  (interactive)
  (key-choices-unload-keys)
  (unless (= key-choices-keybinding 8)
    (setq key-choices-keybinding 8)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    )
  (tpu-edt-on)
  )

(defun key-choices-ergonomic-cua ()
  (interactive)
  (require 'ergoemacs-mode)
  (key-choices-unload-keys)
  (unless (= key-choices-keybinding 5)
    (setq key-choices-keybinding 5)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    )
  (setenv "ERGOEMACS_KEYBOARD_LAYOUT" key-choices-keyboard) ; US layout
  (cua-mode 1)
  (ergoemacs-mode 1)
  )
(defun key-choices-ergonomic-qwerty ()
  (interactive)
  (require 'ergoemacs-mode)
  (unless (string= key-choices-keyboard "us")
    (setq key-choices-keyboard "us")
    (customize-save-variable 'key-choices-keyboard key-choices-keyboard)
    (customize-save-customized)
    )
  (color-theme-emacs-revert-theme)
  )
(defun key-choices-ergonomic-dvorak ()
  (interactive)
  (require 'ergoemacs-mode)
  (unless (string= key-choices-keyboard "dv")
    (setq key-choices-keyboard "dv")
    (customize-save-variable 'key-choices-keyboard key-choices-keyboard)
    (customize-save-customized)
    )
  (color-theme-emacs-revert-theme)
  )
(defun key-choices-ergonomic-colemak ()
  (interactive)
  (require 'ergoemacs-mode)
  (unless (string= key-choices-keyboard "colemak")
    (setq key-choices-keyboard "colemak")
    (customize-save-variable 'key-choices-keyboard key-choices-keyboard)
    (customize-save-customized)
    )
  (color-theme-emacs-revert-theme)
  )
(defgroup key-choices-color nil
  "* Options for Viper Color theme."
  :group 'key-choices
  )
(defvar key-choices-viper-last-mode 0
  "* Defines the last mode for Viper")
(defcustom key-choices-viper-revert-color-theme 't
  "* Defines if the color theme is reverted before appling the new color theme."
  :type 'boolean
  :group 'key-choices-color
  )
(defcustom key-choices-viper-force-it 't
  "* Defines if viper is forced to be on if it it selected for."
  :type 'boolean
  :group 'key-choices-color)

(defcustom key-choices-viper-insert-color-theme "(color-theme-vim-insert-mode)"
  "* Defines the color-theme used for Insert mode."
  :type 'string
  :group 'key-choices-color
  )
(defcustom key-choices-viper-emacs-color-theme "(color-theme-emacs-revert-theme)"
  "* Defines the color-theme used for Emacs mode."
  :type 'string
  :group 'key-choices-color
  )
(defcustom key-choices-viper-cua-color-theme "(color-theme-sitaramv-nt)"
  "* Defines the color-theme used for CUA-mode."
  :type 'string
  :group 'key-choices-color)
(defcustom key-choices-viper-vi-color-theme "(color-theme-vim-colors)"
  "* Defines the color-theme used for VI mode."
  :type 'string
  :group 'key-choices-color
  )
(defcustom key-choices-viper-vi-visual-color-theme "(color-theme-vim-colors)"
  "* Defines the color-theme used for VI visual mode."
  :type 'string
  :group 'key-choices-viper
  )
(defun key-choices-viper-pre-command-hook ()
  (interactive)
  (condition-case error
      (let ( (deactivate-mark nil) )
        (when (not (minibufferp))
          (when (and key-choices-viper-force-it (not (= 2 key-choices-keybinding))
                     (and (boundp 'viper-mode) (eq viper-mode t)))
            (viper-go-away))
          (when (and (= 2 key-choices-keybinding)
                     (boundp 'viper-current-state)
                     viper-current-state)
            (when (and key-choices-viper-force-it (not (and (boundp 'viper-mode) (eq viper-mode t))))
              (require 'color-theme)
              (when (not (fboundp 'color-theme-robin-hood))
                (color-theme-initialize))
              (require 'color-theme-emacs-revert-theme)
              (require 'color-theme-vim-insert-mode)

              (viper-mode))
            (when (and (not (= 1 key-choices-viper-last-mode))
                       (eq viper-current-state 'emacs-state)
                       )
              ;; Emacs
              (setq key-choices-viper-last-mode 1)
              (when key-choices-viper-revert-color-theme
                (color-theme-emacs-revert-theme))
              (with-temp-buffer
                (insert key-choices-viper-emacs-color-theme)
                (eval-buffer)
                )
              )
            (when (and (not (= 2 key-choices-viper-last-mode))
                       (eq viper-current-state 'insert-state)
                       )
              ;; Insert
              (when key-choices-viper-revert-color-theme
                (color-theme-emacs-revert-theme))
              (setq key-choices-viper-last-mode 2)
              (with-temp-buffer
                (insert key-choices-viper-insert-color-theme)
                (eval-buffer)
                )
              )
            (when (and (not (= 3 key-choices-viper-last-mode))
                       (eq viper-current-state 'vi-state)
                       (or (not (boundp 'viper-visual-mode))
                           (not viper-visual-mode))
                       )
              ;; Vi Normal
              (when key-choices-viper-revert-color-theme
                (color-theme-emacs-revert-theme))
              (setq key-choices-viper-last-mode 3)
              (with-temp-buffer
                (insert key-choices-viper-vi-color-theme)
                (eval-buffer)
                )
              )
            (when (and (not (= 4 key-choices-viper-last-mode))
                       (eq viper-current-state 'vi-state)
                       (boundp 'viper-visual-mode)
                       viper-visual-mode)
              ;; Vi visual
              (when key-choices-viper-revert-color-theme
                (color-theme-emacs-revert-theme))
              (setq key-choices-viper-last-mode 4)
              (with-temp-buffer
                (insert key-choices-viper-vi-visual-color-theme)
                (eval-buffer)
                )
              )
            )
          )
        )
    (error nil)))
(add-hook 'pre-command-hook 'key-choices-viper-pre-command-hook)
(add-hook 'post-command-hook 'key-choices-viper-pre-command-hook)
(defun key-choices-viper ()
  (interactive)
  (key-choices-unload-keys)
  (unless (= 2 key-choices-keybinding)
    (setq key-choices-keybinding 2)
    (customize-save-variable 'key-choices-keybinding key-choices-keybinding)
    (customize-save-customized)
    )
  (if (boundp 'viper-mode)
      (toggle-viper-mode)
    (setq viper-mode 't)
    (require 'viper)
    (require 'vimpulse)
    (require 'viper-in-more-modes)
    (require 'color-theme)
    (when (not (fboundp 'color-theme-robin-hood))
      (color-theme-initialize))
    (require 'color-theme-emacs-revert-theme)
    (require 'color-theme-vim-insert-mode)
    (setq viper-always 't)
    (setq-default viper-auto-indent   t)
    (setq-default viper-electric-mode t)
    (setq woman-use-own-frame nil)
    (setq woman-use-topic-at-point t)
    (setq-default viper-vi-style-in-minibuffer t)
    (setq-default viper-case-fold-search t)
    (setq-default viper-search-wrap-around t)
    (add-hook 'eshell-mode-hook
              (lambda ()
                (interactive)
                (when (and (boundp 'viper-mode) viper-mode)
                  (make-variable-buffer-local 'viper-mode-auto-indent)
                  (setq viper-mode-auto-indent nil)
                  )
                ))
    (add-hook 'viper-vi-state-hook (lambda ()
                                     (interactive)
                                     (deactivate-mark)))
    (add-hook 'viper-insert-state-hook (lambda ()
                                         (interactive)
                                         (deactivate-mark)))
    (setq ex-token-alist (append '( ("anything" (anything) )
                                    ("a" (anything))
                                    ("e" (ex-edit))
                                    ("eb" (eval-buffer))
                                    ("xx" (execute-extended-command ""))
                                    ("only" (delete-other-windows))
                                    ("close" (delete-window))
                                    ) ex-token-alist))

    (define-key viper-vi-global-user-map "\C-wq" 'delete-window)
    )
  )
(defun key-choices-viper-ini-fn ()
  (interactive)
  (cond
   ((= 3 key-choices-keybinding)
    (key-choices-cua))
   ( (= 2 key-choices-keybinding)
     (let (
           (ob (current-buffer))
           (first 't)
           )
       (mapc (lambda (buf)
               (switch-to-buffer buf)
               (if first
                   (key-choices-viper))
               (viper-mode)
               (viper-change-state-to-vi)
               (setq first nil)
               )
             (buffer-list)
             )
       (switch-to-buffer ob)
       )
     )
   ( (= 4 key-choices-keybinding)
     (key-choices-ergonomic)
     )
   ( (= 5 key-choices-keybinding)
     (key-choices-ergonomic-cua)
     )
   ( (= 6 key-choices-keybinding)
     (key-choices-crisp)
     )
   ( (= 7 key-choices-keybinding)
     (key-choices-edt)
     )
   ( (= 8 key-choices-keybinding)
     (key-choices-tpu)
     )
   )
  )

(add-hook 'after-change-major-mode-hook 'key-choices-viper-ini-fn)
(key-choices-viper-ini-fn)

(provide 'key-choices)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key-choices.el ends here
