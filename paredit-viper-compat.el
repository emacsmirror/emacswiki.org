;;; paredit-viper-compat.el: viper-mode modifications to support paredit
;;
;; Copyright (c) 2009, Jeremy L. Rayman.
;; Author: Jeremy Rayman
;; URL:  http://www.emacswiki.org/emacs/paredit-viper-compat.el
;; Site: http://www.emacswiki.org/emacs/ParEditViperCompat
;; Status: Beta - not fully tested.

;; This mode allows paredit to work when viper is enabled.
;;
;; Usage:
;; Place the following in your .emacs (or init.el):
;;
;; (eval-after-load 'paredit
;;   '(progn
;;      (require 'paredit-viper-compat)
;;      (paredit-viper-compat))
;;
;; This will apply the paredit-viper-compat fixes only when paredit
;; is active.
;;
;; User key customizations:
;; If you want to add your own keybindings in addition to the ones
;; provided by paredit-viper-compat, you can do so using
;; the function paredit-viper-add-local-keys.
;; Usage is (paredit-viper-add-local-keys STATE KEYS);
;; where STATE is one of 'all-states, 'vi-state, 'insert-state, or 'emacs-state.
;; Insert a statement like the following inside the (eval-after-load ...)
;; form above:
;;
;; (paredit-viper-add-local-keys 'all-states  ; or 'insert-state, etc.
;;                               '(("\C-w" . paredit-backward-kill-word)
;;                                 ;; ... more keys ...
;;                                 ))
;;
;; Complete setup combining the above examples:
;;
;; (eval-after-load 'paredit
;;   '(progn
;;      (require 'paredit-viper-compat)
;;      (paredit-viper-compat)
;;      (paredit-viper-add-local-keys 'all-states
;;                                    '(("\C-w" . paredit-backward-kill-word)
;;                                      ;; ... more keys ...
;;                                      ))))
;;
;; Changelog:
;; 0.1 - 2009-08-15 - Initial release.
;;
;; TODO:
;; * XEmacs compatibility - paredit uses a variable `paredit-forward-delete-keys'
;;   which provides different values for GNU Emacs vs XEmacs. I could
;;   not figure out how to use that value correctly in the code below,
;;   so ended up hardcoding the values for GNU Emacs ("<delete>" and
;;   "<deletechar>").
;;
;;
;;
;; Copyright (c) 2009, Jeremy L. Rayman
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; * Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.
;;
;; * Neither the names of the authors nor the names of contributors
;;   may be used to endorse or promote products derived from this
;;   software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(require 'paredit)

;;; Paredit built-in keys:

(defconst paredit-viper-compat-insert-state-keys
  '(("(" . paredit-open-round)
    (")" . paredit-close-round)
    ("\M-)" . paredit-close-round-and-newline)
    ("[" . paredit-open-square)
    ("]" . paredit-close-square)
    ("\"" . paredit-doublequote)
    ("\M-\"" . paredit-meta-doublequote)
    ("\\" . paredit-backslash)
    ; Viper Fixes
    ([(backspace)] . paredit-backward-delete)
    ("\"" . paredit-doublequote))
  "Paredit keys to be available in insert mode only.

  Stored as an alist of (key-str . func) pairs:
  '((key-str . func) (key-str . func)) ")

(defconst paredit-viper-compat-all-states-keys
      `(; Basic Insertion Commands (most are in a different map though)
        ([(meta ?\;)] . paredit-comment-dwim)
        ; Deleting & Killing:
        ; xemacs users note: the two lines below have
        ; hardcoded values for <delete> which need to be changed for
        ; you. Look at `paredit-forward-delete-keys', which has the
        ; characters used for paredit-forward-delete in xemacs. I couldn't figure
        ; out how to get this right - patches welcome.  You may also want
        ; to look at `paredit-commands' for usage of the
        ; `paredit-forward-delete-keys' variable.
        ("\C-j" . paredit-newline)
        ("\C-d <delete>" . paredit-forward-delete)
        ("\C-d <deletechar>" . paredit-forward-delete)
        (,(format "<%s>" paredit-backward-delete-key) . paredit-backward-delete)
        ("\C-k" . paredit-kill)
        ("\M-d" . paredit-forward-kill-word)
        (,(format "\M-%s" paredit-backward-delete-key) . paredit-backward-kill-word)
        ("\C-\M-f" . paredit-forward)
        ("\C-\M-b" . paredit-backward)
        ("\C-\M-p" . backward-down-list)
        ("\C-\M-n" . up-list)
        ; Depth-Changing Commands
        ("\M-(" . paredit-wrap-round)
        ("\M-s" . paredit-splice-sexp)
        ("\M-<up>" . paredit-splice-sexp-killing-backward)
        ("ESC <up>" . paredit-splice-sexp-killing-backward)
        ("\M-<down>" . paredit-splice-sexp-killing-forward)
        ("ESC <down>" . paredit-splice-sexp-killing-forward)
        ; Barfage and Slurpage
        ([(control ?\))] . paredit-forward-slurp-sexp)
        ([(control right)] . paredit-forward-slurp-sexp)
         ;"\C-<right>" . paredit-forward-slurp-sexp)
        ([(control ?\})] . paredit-forward-barf-sexp)
                                        ;"\C-}" . paredit-forward-barf-sexp)
        ([(control left)] . paredit-forward-barf-sexp)
                                        ;"\C-<left>"
        ([(control ?\()] . paredit-backward-slurp-sexp)
                                        ;"\C-(" . paredit-backward-slurp-sexp)
        ([(control meta left)] . paredit-backward-slurp-sexp)
        ;; ([(escape control left)] . paredit-backward-slurp-sexp)
        ([(control ?\{)] . paredit-backward-barf-sexp)
        ([(control meta right)] . paredit-backward-barf-sexp)
        ;; ([(escape control right)] . paredit-backward-barf-sexp)
        ("\M-r" . paredit-raise-sexp)
        ("M-;" . paredit-comment-dwim)
        ; Miscellaneous Commands
        ("\M-S" . paredit-split-sexp)
        ("\M-J" . paredit-join-sexps)
        ("\C-c \C-\M-l" . paredit-recentre-on-sexp)
        ("\M-q" . paredit-reindent-defun))
  "Paredit keys to be available in all viper states.

Stored as an alist of (key-str . func) pairs.")


;;; Functions for setting up paredit-viper-compat:

(defun paredit-viper-compat ()
  "Enable paredit keybindings for viper-mode.

This should be called from a hook to a major mode or
on a per buffer basis."
  (viper-add-local-keys 'insert-state paredit-viper-compat-insert-state-keys)
  (viper-add-local-keys 'insert-state paredit-viper-compat-all-states-keys)
  (viper-add-local-keys 'vi-state paredit-viper-compat-all-states-keys)
  (viper-add-local-keys 'emacs-state
                         paredit-viper-compat-all-states-keys))

(defun paredit-viper-add-local-keys (state alist)
  "Add custom keys to viper-mode when paredit is enabled.

STATE is one of: 'all-states (add to all viper states),
                 'insert-state, 'vi-state, or 'emacs-state.
ALIST is of the form ((key . func) (key . func) ...)
Normally, this would be called from a hook to a major mode or
on a per buffer basis.
Usage:
  (paredit-viper-add-local-keys state '((key-str . func) (key-str . func)...))"
  (eval-after-load "viper-mode"
    (if (equal state 'all-states)
        (progn
          (viper-add-local-keys 'emacs-state alist)
          (viper-add-local-keys 'vi-state alist)
          (viper-add-local-keys 'insert-state alist))
      (viper-add-local-keys state alist))))


(provide 'paredit-viper-compat)


;; End paredit-viper-compat.el.

;; Extra notes about how this all works:
;;
;; Emacs minor modes are all listed in `minor-mode-map-alist', and
;; when the keys from two minor modes overlap, the keybinding from the
;; minor mode at the front of the list is preferred.
;;
;; However, Viper maintains its own separate hierarchy of minor
;; modes. It turns out Viper is implemented as a whole series of minor
;; modes which give you the various states - insert mode, vi-mode,
;; etc. This list is stored in a separate `emulation-mode-map-alists',
;; which takes priority over the keybindings of all the minor modes in
;; the regular `minor-mode-map-alist'.
;;
;; So that's why we were getting a collision. Viper-mode was getting
;; priority over paredit's keybindings.
;;
;; Viper provides a couple empty keymaps for users to customize which
;; let you set your own keybindings. You can set them on a mode-local
;; basis, i.e. only in effect when paredit is enabled. You just add a
;; hook on load of paredit-mode that sets the right keybindings.  -
;;
;; (Actually, you have to use viper-add-local-keys to add keys, not
;; insert the bindings into these maps directly. That's why the keys are
;; all in an alist instead of being added to a mode-map: viper wants
;; an alist).
;;
;; viper-add-local-keys - per-buffer keybinding modification which
;; overshadows the viper keybindings. Accepts an alist.
;; Fills in some empty keymaps set aside by viper for user
;; customizations.
;;
;; - minor-mode-map-alist and emulation-mode-map-alists:
;; emulation-mode-map-alists takes precedence over
;; minor-mode-map-alist, which is why you don't see paredit's
;; keybindings "winning" in the precedence order. Paredit is first in
;; minor-mode-map-alist, so its key bindings override other minor
;; modes, but that still doesn't fix things because viper uses
;; `emulation-mode-map-alists' which has precedence over the entire
;; minor-mode-map-alist hierarchy.
