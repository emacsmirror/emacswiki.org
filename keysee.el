;;; keysee.el --- Key and menu completion.   -*- lexical-binding:t -*-
;;
;; Filename: keysee.el
;; Description: Key and menu completion.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Created: Fri May 22 12:21:59 2020 (-0700)
;; Version: 1
;; Package-Requires: ()
;; Last-Updated: Thu Jun  4 16:19:25 2020 (-0700)
;;           By: dradams
;;     Update #: 332
;; URL: https://www.emacswiki.org/emacs/download/keysee.el
;; Doc URL: https://www.emacswiki.org/emacs/KeySee
;; Keywords: key completion sorting
;; Compatibility: GNU Emacs 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;; Commentary:
;;
;;    Key and menu completion.
;;
;;  Turn on minor mode `kc-mode' to enable key and menu completion.
;;  Or just complete keys or menu-bar menus on demand, using command
;;  `kc-complete-keys' or `kc-complete-menu-bar', respectively.
;;
;;  If option `kc-auto-flag' is non-nil then completion is automatic
;;  when you use a prefix key.  (The default value is t.)
;;
;;  You can also complete on demand, using `S-TAB'.  (You can
;;  customize the keys for this, using option `kc-completion-keys'.)
;;
;;  You can always complete on demand at top level.  If `kc-auto-flag'
;;  is nil then you can also complete on demand after a prefix key.
;;
;;  Completion shows you all possible keys you can use, at any level.
;;  When completions are shown, the completion candidates have one of
;;  these forms:
;;
;;    ..
;;    PREFIX-KEY  =  ...
;;    KEY  =  COMMAND
;;
;; * The first of these, `..', is shown only when completing a prefix
;;   key.  Choosing it takes you back up one level, to the parent
;;   prefix key, or to the top level if there is no parent.
;;
;;   For example, if you are currently completing prefix key `C-x 4',
;;   then `..' takes you back up to the completions for prefix key
;;   `C-x'.  Using `..' there then takes you up from that level to the
;;   top level.  When `..' is a candidate it is the default, so you
;;   can just hit `RET' to go up a level.
;;
;;  * Choosing `PREFIX-KEY  =  ...' takes you down a level, to the
;;    keys on that PREFIX-KEY.  For example, at top level, choosing
;;    completion candidate `C-x  =  ...' takes you to completions for
;;    prefix key `C-x'.
;;
;;  * Choosing `KEY  =  COMMAND' invokes COMMAND.
;;
;;  Completion uses the completion styles defined by option
;;  `kc-completion-styles', not those of standard option
;;  `completion-styles'.  By default, it favors substring or flex
;;  (Emacs 27 or later) completion.
;;
;;  You can use option `kc-separator' to customize separator `  =  '.
;;  The default value has 2 space chars on each side of `=', so you
;;  can more easily match menu candidates that contain a space char.
;;
;;  Completion is as usual in vanilla Emacs: type characters to match
;;  completion candidates, use `TAB' to complete, use `RET' to accept
;;  a completion.
;;
;;  The following completion-candidate sort orders are available.  You
;;  can cycle among them during completion using the key that is the
;;  value of option `sorti-cycle-key' (`C-,' by default).
;;
;;  * By key name alphabetically, prefix keys first
;;  * By key name alphabetically, local keys first
;;  * By command name alphabetically
;;  * Off - no sorting
;;
;;  You can cycle candidates using `TAB', according to option
;;  `completion-cycle-threshold'.

;;  For top-level key completion (keys in `kc-completion-keys',
;;  e.g. `S-TAB'), you can use option `kc-keymaps-for-key-completion'
;;  to choose which keymaps in which to bind `S-TAB' to
;;  `kc-complete-keys'.  Normally, if a key in
;;  `kc-keymaps-for-key-completion' is already bound in a map listed
;;  in that option value then that binding is respected - the key is
;;  not bound to `kc-complete-keys'.  You can override this by
;;  customizing option `kc-bind-completion-keys-anyway-flag' to
;;  non-nil.
;;
;;
;;  Suggested key binding:
;;
;;    (global-set-key (kbd "S-<f10>") kc-complete-menu-bar)
;;
;;
;;  Commands defined here:
;;
;;    `kc-complete-keys', `kc-complete-menu-bar', `kc-mode'.
;;
;;  Faces defined here:
;;
;;    `kc-key-local', `kc-key-non-local', `kc-menu-local',
;;    `kc-menu-non-local'.
;;
;;  User options defined here:
;;
;;    `kc-auto-delay', `kc-auto-flag',
;;    `kc-bind-completion-keys-anyway-flag', `kc-completion-keys',
;;    `kc-completion-styles', `kc-ignored-prefix-keys',
;;    `kc-keymaps-for-key-completion', `kc-prefix-in-mode-line-flag',
;;    `kc-self-insert-ranges', `kc-separator'.
;;
;;  Non-interactive functions defined here:
;;
;;    `kc-add-key+cmd', `kc-auto',
;;    `kc-bind-key-completion-keys-for-map-var',
;;    `kc-bind-key-completion-keys-in-keymaps-from',
;;    `kc-case-string-less-p', `kc-collection-function',
;;    `kc-command-names-alphabetic-p', `kc-complete-keys-1',
;;    `kc-complete-keys-action', `kc-keys+cmds-w-prefix',
;;    `kc-lighter', `kc-local-key-binding-p', `kc-local-keys-first-p',
;;    `kc-prefix-keys-first-p', `kc-remove-lighter',
;;    `kc-remove-lighter-unless-completing-prefix',
;;    `kc-same-vector-keyseq-p', `kc-some', `kc-sort-by-command-name',
;;    `kc-sort-function-chooser', `kc-sort-local-keys-first',
;;    `kc-sort-prefix-keys-first', `kc-this-command-keys-prefix',
;;    `kc-unbind-key-completion-keys-for-map-var',
;;    `kc-unbind-key-completion-keys-in-keymaps-from', `kc-unlist'.
;;
;;  Internal variables defined here:
;;
;;    `kc-auto-idle-timer', `kc-current-order', `kc-keys-alist',
;;    `kc-orig-current-order', `kc-orig-sort-fn-chooser',
;;    `kc-orig-sort-order', `kc-orig-sort-orders-alist',
;;    `kc-orig-sort-orders-ring', `kc-sort-orders-alist',
;;    `kc-sort-orders-ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2020/06/04 dadams
;;     Version 1.
;;     Factored out sorting and cycling stuff as new library sortie.el.  Require that.
;;     Added: kc-current-order, kc-orig-current-order, kc-orig-sort-order, kc-orig-sort-orders-alist,
;;            kc-orig-sort-orders-ring, kc-sort-orders-alist.
;;     Renamed: kc-completion-function to kc-collection-function,
;;              kc-sort-function to kc-sort-function-chooser,
;;              kc-sort-orders to kc-sort-orders-ring.
;;     Removed: kc-bind-sort-cycle-key, kc-cycle-sort-order, kc-minibuffer-setup, kc-msg-emphasis,
;;              kc-reverse-order, kc-sort-cycle-key, kc-sort-order.  Use sorti- versions instead.
;;     kc-mode: kc-sort-cycle-key -> sorti-cycle-key.
;;              Backup and restore:
;;                sorti-current-order, sorti-sort-orders-alist, sorti-sort-orders-ring,
;;                sorti-sort-function-chooser.
;;              Message shows current sort order.  Use sorti-msg-emphasis.
;;     kc-complete-keys-1: Use sorti-minibuffer-setup.
;;                         Just use completing-read, not completing-read-default.
;;     kc-sort-function-chooser: kc-sort-order -> sorti-current-order.
;; 2020/06/03 dadams
;;     Added: kc-reverse-order, kc-sort-function.
;;     kc-cycle-sort-order: Prefix arg reverses sort order (doesn't cycle).  Use kc-sort-function.
;;     kc-completion-function: Use kc-sort-function.
;; 2020/05/25 dadams
;;     Added: kc-sort-cycle-key, kc-sort-order, kc-sort-orders, kc-minibuffer-setup,
;;            kc-bind-sort-cycle-key, kc-cycle-sort-order, kc-completion-function,
;;            kc-sort-prefix-keys-first, kc-sort-local-keys-first, kc-sort-by-command-name,
;;            kc-prefix-keys-first-p, kc-local-keys-first-p, kc-local-key-binding-p,
;;            kc-case-string-less-p, kc-command-names-alphabetic-p.
;;     kc-mode: Bind sort-cycling key, and restore.
;;     kc-complete-keys-1: Use kc-minibuffer-setup, completing-read-default, kc-completion-function.
;;     kc-msg-emphasis: Changed default appearance.
;; 2020/05/24 dadams
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

(require 'cl-macs) ;; cl-case, cl-loop
(require 'sortie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup keysee nil
  "Key completion preferences."
  :prefix "kc-" :group 'completion
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=keysee.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Keysee library versions."))
  :link '(url-link :tag "Other Libraries by Drew" "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/keysee.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/KeySee"))


(defface kc-key-local
  '((((background dark)) (:background "#176900004E0A")) ; a dark blue
    (t (:background "#EF47FFFFC847"))) ; A light yellow.
  "Face to highlight local keys when completing keys.
This means local keys that are not menu items.
Local keys that are menu items are highlighted instead with face
`kc-menu-local'.

Non-local keys (i.e., not for the current mode) are highlighted with
face `kc-key-non-local'."
  :group 'keysee :group 'faces)

(defface kc-key-non-local
  '((((background dark)) (:background "#451700143197")) ; a very dark magenta
    (t (:background "#EF84FFEAF427"))) ; A light green.
  "Face to highlight non-local keys when completing keys.
This means non-local keys (i.e., not for the current mode) that are
not menu items.
Non-local keys that are menu items are highlighted instead with face
`kc-menu-non-local'.

Local keys are highlighted with face `kc-key-local'."
  :group 'keysee :group 'faces)

(defface kc-menu-local
  '((((background dark))
     (:background "#176900004E0A"       ; a dark blue
                  :box (:line-width 1 :color "#E1E1EAEAFFFF"))) ; a light blue box
    (t (:background "#EF47FFFFC847"     ; a light yellow.
                    :box (:line-width 1 :color "#AC4AAC4A0000")))) ; a dark yellow box
  "Face to highlight local menus and menu items when completing keys.
Local keys that are not menus or menu items are highlighted instead
with face `kc-key-local'.

Non-local menus and menu items (i.e., not for the current mode) are
highlighted with face `kc-menu-non-local'."
  :group 'keysee :group 'faces)

(defface kc-menu-non-local
  '((((background dark))
     (:background "#451700143197"       ; a very dark magenta
                  :box (:line-width 1 :color "#FA6CC847FFFF"))) ; a light magenta box
    (t (:background "#EF84FFEAF427"     ; a light green.
                    :box (:line-width 1 :color "#34F393F434F3")))) ; a green box
  "Face to highlight non-local menus and menu items when completing keys.
This means non-local keys (i.e., not for the current mode) that are
menus or menu items.
Non-local keys that are not menus or menu items are highlighted
instead with face `kc-key-non-local'.

Local menus and menu items (i.e., not for the current mode) are
highlighted with face `kc-menu-non-local'."
  :group 'keysee :group 'faces)


(defcustom kc-auto-delay 1.0
  "Seconds to wait when idle, before automatically completing a key.
This has an effect only when `kc-mode' is on and `kc-auto-flag' is
non-nil."
  :type 'number :group 'keysee)

(defcustom kc-auto-flag t
  "Non-nil means `kc-mode' automatically shows prefix-key completions.
If you change the value while `kc-mode' is on, then you must toggle
`kc-mode', to have the new value take effect."
  :type 'boolean :group 'keysee)

(defcustom kc-bind-completion-keys-anyway-flag nil
  "Non-nil means bind `S-TAB' for key completion even if already bound.
If nil, then each of the keys in `kc-completion-keys' is bound to
`kc-complete-keys' in each keymap of `kc-keymaps-for-key-completion'
only if `S-TAB' is not already bound in the keymap."
  :type 'boolean :group 'keysee)

(defcustom kc-completion-keys '([backtab])
  ;; `backtab' is the canonical key that represents both `S-tab' and `S-iso-lefttab',
  ;; so it is used in the default value.  If, for some reason, `backtab' is not
  ;; translated to `S-tab' and `S-iso-lefttab' on your platform, you might want to
  ;; customize the value to ([S-tab] [S-iso-lefttab]).
  "Key sequences to bind to `kc-complete-keys'.
A list of values that each has the same form as a key-sequence
argument to `define-key'.  It is a list mainly in order to accommodate
different keyboards - for example, `S-tab' and `S-iso-lefttab'.

The keys are bound in `kc-keymaps-for-key-completion'.  If the
option value is nil then `kc-mode' does not bind any keys, in any
of the maps, to `kc-complete-keys'."
  :type '(repeat sexp) :group 'keysee)

(defcustom kc-completion-styles
  `(substring
    ,@(and (> emacs-major-version 26)  '(flex))
    partial-completion
    emacs22)
  "List of completion styles to use with `kc-mode'.
Key completion binds `completion-styles' (which see) to this
value for the duration."
  :type completion--styles-type)

(defcustom kc-ignored-prefix-keys ()
  "Prefix keys for `kc-complete-keys' to ignore.
The value is a list of key sequences assumed to be prefix keys.

If `kc-complete-keys' is bound to a key on one of these prefix keys
then it does not complete that prefix key.  Instead, it acts as though
invoked at the top level.

This lets you bind key completion to a key sequence that starts with a
prefix key, so you can invoke it from there just as if you had invoked
it at top level.

Here is an example of adding two such prefix keys using Customize:

 Click [INS] to insert a new, empty entry.
 Type `M-s' in the edit field.

 Click [INS] to insert another new, empty entry.
 Type `<menu-bar> <edit>' in the edit field.

 Use the `State' menu to update the option value to reflect the edit.

This adds these two prefix keys to the option value:

 * [134217843], which is represented externally by `M-s'
 * [menu-bar edit], which represents the `Edit' menu-bar menu.

This example means that if you bind `kc-complete-keys' to a key on
either of these prefix keys then invoking that key sequence will
complete keys at the top level - it will not complete that prefix
key."
  :type '(repeat (key-sequence :tag "Prefix Key" :value [ignore]))
  :group 'keysee)

(defcustom kc-keymaps-for-key-completion
  '(apropos-mode-map bookmark-bmenu-mode-map bmkp-jump-map bmkp-jump-other-window-map
                     calendar-mode-map dired-mode-map facemenu-keymap help-mode-map
                     jde-mode-map jde-jdb-mode-map senator-mode-map srecode-mode-map
                     synonyms-mode-map vc-dired-mode-map)
  "List of keymaps in which to bind `S-TAB' to `kc-complete-keys'.
List elements are symbols that are bound to keymaps.

Each keymap should have at least one prefix key.  `S-TAB' is bound in
each keymap, so that you can use it to complete the prefix keys.

If one of the keymaps is not defined when KC mode is entered, then it
is ignored.  If you later define it, then just exit and reenter KC
mode, to bind `S-TAB' in the newly defined map.  For example, use `M-x
icy-mode' twice after entering Calendar mode, to be able to complete
`calendar-mode' prefix keys such as `A'.

Do not add `global-map' or any keymaps, such as `ctl-x-map', that are
accessible from the global keymap to the list - they are already
treated, by default.

Do not add any of the translation keymaps, `function-key-map',
`key-translation-map', or `iso-transl-ctl-x-8-map' to the list - that
will not work."
  :type '(repeat symbol) :group 'keysee)

(defcustom kc-prefix-in-mode-line-flag t
  "Whether to show current prefix key in mode-line during key completion.
It is shown as the minor-mode lighter for KC mode."
  :type 'boolean :group 'keysee)

(defcustom kc-self-insert-ranges ()
  "Non-nil means `kc-complete-keys' includes self-inserting keys.
That means keys bound to `self-insert-command'.

There are thousands of self-inserting keys, so it's not practical to
allow all as candidates.  Instead, a non-nil value is a list of
character ranges of the form (MIN . MAX).  Characters in the inclusive
range MIN through MAX are possible key-completion candidates.

if you use a non-nil value then use only small ranges for better
performance, e.g., `((0 . 687))' covers Latin characters.

In general, leave the value as nil.  Use `C-x 8 RET' to insert
characters by completing against their Unicode names.

For reference, below are the ranges supported by `C-x 8 RET'.  But
unless you have a very powerful computer, choose only only one or two
small ranges of characters you actually might use.

BMP ranges:
 (0 . 13311)       = (#x0000 . #x33FF)
 (19904 . 19967)   = (#x4DC0 . #x4DFF)
 (40960 . 55295)   = (#xA000 . #x0D7FF)
 (64256 . 65533)   = (#xFB00 . #xFFFD)

Upper ranges:
 (65536 . 79103)   = (#x10000 . #x134FF)
 (118784 . 131071) = (#x1D000 . #x1FFFF)
 (917504 . 918015) = (#xE0000 . #xE01FF)"
  :type '(alist :key-type integer :value-type integer) :group 'keysee)

(defcustom kc-separator "  =  "
  "Characters that separate keys from their commands, in key completions."
  :type 'string :group 'keysee)

(defvar kc-auto-idle-timer nil
  "Timer used to automatically complete a key sequence when Emacs is idle.")

(defvar kc-current-order 'prefix-keys
  "Current sort order for `kc-mode' key candidates.
It is one of these symbols, with the indicated meanings:
 `prefix-keys': Sort by key name, prefix keys first.
 `local-keys' : Sort by keynme, local keys first.
 `command'    : Sort by command name.
 `nil'        : Do not sort.")

(defvar kc-keys-alist ()
  "Alist of keys and their bindings.
Each alist element is of the form (NAME KEY . BINDING), where:
 NAME is a string naming the key and its binding, whose name has form:
   KEYNAME  =  BINDING-NAME
     KEYNAME is the `key-description' of KEY
     BINDING-NAME description of BINDING
 KEY is the actual key sequence
 BINDING is the actual binding of KEY.

\(The separator between KEY and BINDING-NAME is the value of option
`kc-separator'.  Its default value is \"  =  \".)")

(defvar kc-orig-current-order nil
  "Original value of `sorti-current-order', before `kc-mode'.
Used to restore `sorti-sort-orders-ring', after `kc-mode'.")

(defvar kc-orig-sort-fn-chooser 'ignore
  "Original value of `sorti-sort-function-chooser', before `kc-mode'.
Used to restore `sorti-sort-function-chooser', after `kc-mode'.")

(defvar kc-orig-sort-order ()
  "Original value of `sorti-current-order' before `kc-mode'.
Used to restore `sorti-current-order', after `kc-mode'.")

(defvar kc-orig-sort-orders-alist ()
  "Original value of `sort-orders-alist', before `kc-mode'.
Used to restore `sorti-sort-orders-alist', after `kc-mode'.")

(defvar kc-orig-sort-orders-ring nil
  "Original value of `sorti-sort-orders-ring', before `kc-mode'.
Used to restore `sorti-sort-orders-ring', after `kc-mode'.")

(defvar kc-sort-orders-alist '((prefix-keys . "prefix keys first")
                               (local-keys  . "local keys first")
                               (command     . "command name"))
  "Alist of sort-orders for completing keys.
Used for `sorti-sort-orders-alist'.")

(defvar kc-sort-orders-ring (let ((rng  (make-ring 4)))
                              (ring-insert rng nil)
                              (ring-insert rng 'command)
                              (ring-insert rng 'local-keys)
                              (ring-insert rng 'prefix-keys)
                              rng)
  "Ring of key-completion sort orders for `sorti-bind-cycle-key' cycling.")


(defun kc-auto ()
  "Auto-complete the last key."
  (let* ((this-c-k-vector  (this-command-keys-vector))
         (this-event       (and (not (equal [] this-c-k-vector))  (elt this-c-k-vector 0))))
    (unless (or (functionp (key-binding this-c-k-vector))
                (not this-event)
                ;; Prevent `down-mouse-1' etc. from auto-completing.
                (mouse-movement-p this-event)
                (mouse-event-p this-event))
      (kc-complete-keys-1 this-c-k-vector 'NO-ERROR)
      (clear-this-command-keys))))

(define-minor-mode kc-mode "KC mode: complete keys.
If option `kc-auto-flag' is non-nil then completion is automatic
when you use a prefix key.  (The default value is t.)

You can also complete on demand, using `S-TAB'.  (You can customize
the keys for this, using option `kc-completion-keys'.)

You can always complete on demand at top level.  If `kc-auto-flag' is
nil then you can also complete on demand after a prefix key.
Completion shows you all possible keys you can use, at any level.

When completions are shown, the completion candidates have one of
these forms:

  ..
  PREFIX-KEY  =  ...
  KEY  =  COMMAND

 * The first of these, `..', is shown only when completing a prefix
   key.  Choosing it takes you back up one level, to the parent prefix
   key, or to the top level if there is no parent.

   For example, if you are currently completing prefix key `C-x 4',
   then `..' takes you back up to the completions for prefix key
   `C-x'.  Using `..' there then takes you up from that level to the
   top level.  When `..' is a candidate it is the default, so you can
   just hit `RET' to go up a level.

 * Choosing `PREFIX-KEY  =  ...' takes you down a level, to the keys
   on that PREFIX-KEY.  For example, at top level, choosing completion
   candidate `C-x  =  ...' takes you to completions for `C-x'.

 * Choosing `KEY  =  COMMAND' invokes COMMAND.

Completion uses the completion styles defined by option
`kc-completion-styles', not those of standard option
`completion-styles'.  By default, it favors substring completion.

You can use option `kc-separator' to customize the separator
`  =  '.  The default value has 2 space chars on each side of `=',
so you can more easily match menu candidates that contain a space
char.

Completion is as usual in vanilla Emacs: type characters to match
completion candidates, use `TAB' to complete, use `RET' to accept
a completion.

The following completion-candidate sort orders are available.  You can
cycle among them during completion using the key that is the value of
option `sorti-cycle-key' (`C-,' by default).

 * By key name alphabetically, prefix keys first
 * By key name alphabetically, local keys first
 * By command name alphabetically
 * Off - no sorting

You can cycle candidates using `TAB', according to option
`completion-cycle-threshold'.

For top-level key completion (keys in `kc-completion-keys',
e.g. `S-TAB'), you can use option `kc-keymaps-for-key-completion'
to choose which keymapsin which to bind `S-TAB' to
`kc-complete-keys'.  Normally, if a key in `kc-completion-keys' is
already bound in a map listed in that option value then that
binding is respected - the key is not bound to `kc-complete-keys'.
You can override this by customizing option
`kc-bind-completion-keys-anyway-flag' to non-nil."
  :global t :group 'keysee :init-value nil
  (when (timerp kc-auto-idle-timer) (cancel-timer kc-auto-idle-timer))
  (let* ((cycle-key-binding  (lookup-key minibuffer-local-map sorti-cycle-key))
         (unbind-cycle-key   (lambda ()
                               (define-key minibuffer-local-map
                                 sorti-cycle-key cycle-key-binding))))
    (cond (kc-mode
           (when kc-auto-flag
             (setq kc-auto-idle-timer  (run-with-idle-timer kc-auto-delay t 'kc-auto)))
           (setq kc-orig-sort-order           sorti-current-order
                 kc-orig-sort-orders-alist    sorti-sort-orders-alist
                 kc-orig-sort-orders-ring     sorti-sort-orders-ring
                 kc-orig-sort-fn-chooser      sorti-sort-function-chooser
                 sorti-current-order          kc-current-order
                 sorti-sort-orders-alist      kc-sort-orders-alist
                 sorti-sort-orders-ring       kc-sort-orders-ring
                 sorti-sort-function-chooser  'kc-sort-function-chooser)
           (kc-bind-key-completion-keys-in-keymaps-from (current-global-map))
           (dolist (map  kc-keymaps-for-key-completion) (kc-bind-key-completion-keys-for-map-var map))
           (add-hook 'minibuffer-inactive-mode-hook #'kc-remove-lighter-unless-completing-prefix)
           (add-hook 'minibuffer-inactive-mode-hook unbind-cycle-key)
           (advice-add 'abort-recursive-edit :before 'kc-lighter))
          (t
           (setq sorti-current-order          kc-orig-sort-order
                 sorti-sort-orders-alist      kc-orig-sort-orders-alist
                 sorti-sort-function-chooser  kc-orig-sort-fn-chooser
                 sorti-sort-orders-ring       kc-orig-sort-orders-ring)
           (kc-unbind-key-completion-keys-in-keymaps-from (current-global-map))
           (dolist (map  kc-keymaps-for-key-completion) (kc-unbind-key-completion-keys-for-map-var map))
           (remove-hook 'minibuffer-inactive-mode-hook #'kc-remove-lighter-unless-completing-prefix)
           (remove-hook 'minibuffer-inactive-mode-hook unbind-cycle-key)
           (advice-remove 'abort-recursive-edit 'kc-lighter))))
  (message "Turning %s %sKC mode..."
           (propertize (if kc-mode "ON" "OFF") 'face 'sorti-msg-emphasis)
           (if kc-auto-flag "AUTO " ""))
  (run-hooks 'kc-mode-hook)
  (message "%sKC mode is now %s%s"
           (if kc-auto-flag "AUTO " "")
           (propertize (if kc-mode "ON" "OFF") 'face 'sorti-msg-emphasis)
           (if (not kc-mode)
               ""
             (format ".  Sorting: %s%s"
                     (if sorti-current-order
                         (format "%s" (propertize
                                       (cdr (assq sorti-current-order sorti-sort-orders-alist))
                                       'face 'sorti-msg-emphasis))
                       (format "turned %s" (propertize "OFF" 'face 'sorti-msg-emphasis)))
                     (if (advice-member-p 'sorti-reverse-order (funcall sorti-sort-function-chooser))
                         " REVERSED"
                       "")))))

(defun kc-bind-key-completion-keys-for-map-var (keymap-var &optional keys)
  "Bind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored.

Actually, by default, it is the keys in `kc-completion-keys' that are
bound, not `S-TAB'.  And if optional arg KEYS is non-nil then it, not
`kc-completion-keys', is the list of keys that are bound."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp  (symbol-value temp))
      (when (keymapp temp) (kc-bind-key-completion-keys-in-keymaps-from temp keys)))))

(defun kc-bind-key-completion-keys-in-keymaps-from (map &optional keys)
  "Bind keys in `kc-completion-keys' to `kc-complete-keys'.
Each key in `kc-completion-keys' (or optional arg KEYS, if non-nil) is
bound in all keymaps accessible from keymap MAP."
  (dolist (key+map  (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      ;; We could try to exclude menu maps, by testing (not (keymap-prompt map)).
      ;; But we want to include at least some menu maps - those, such as `facemenu-keymap',
      ;; that are bound to keyboard keys. (when (and (keymapp map)  (not (keymap-prompt map)))...)
      (when (keymapp map)
        (dolist (key  (or keys  kc-completion-keys))
          (when (or kc-bind-completion-keys-anyway-flag  (not (lookup-key map key)))
            (ignore-errors (define-key map key 'kc-complete-keys))))))))

(defun kc-unbind-key-completion-keys-for-map-var (keymap-var &optional keys)
  "Unbind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored.

Actually, by default, it is the keys in `kc-completion-keys' that are
unbound, not `S-TAB'.  And if optional arg KEYS is non-nil then it,
not `kc-completion-keys', is the list of keys that are unbound."
  (let ((temp  keymap-var))
    (when (boundp temp)
      (setq temp  (symbol-value temp))
      (when (keymapp temp) (kc-unbind-key-completion-keys-in-keymaps-from temp keys)))))

(defun kc-unbind-key-completion-keys-in-keymaps-from (map &optional keys)
  "Unbind `kc-completion-keys' in keymaps accessible from MAP.
Each key in `kc-completion-keys' (or optional arg KEYS, if non-nil) is
unbound in all keymaps accessible from keymap MAP."
  (dolist (key+map  (accessible-keymaps map))
    (let ((map  (cdr key+map)))
      (while (and (symbolp map)  (fboundp map)) (setq map  (symbol-function map))) ; Get a list.
      (when (and (keymapp map)
                 (not (eq 'autoload (car-safe map))) ; Skip autoload keymaps.
                 (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (dolist (key  (or keys  kc-completion-keys))
          (when (eq (lookup-key map key) 'kc-complete-keys)
            (ignore-errors (define-key map key nil))))))))

(defun kc-lighter (&optional prefix-vect)
  "Update minor-mode mode-line lighter to reflect current prefix key.
Do nothing, if `kc-prefix-in-mode-line-flag' is nil.
PREFIX-VECT is the vector for that key."
  (when kc-prefix-in-mode-line-flag
    (kc-remove-lighter)
    (when prefix-vect
      (let ((pkey  (key-description prefix-vect nil)))
        (add-to-list 'minor-mode-alist
                     `(kc-mode ,(if (equal "" pkey) "" (format " %s" pkey))))))))

(defun kc-remove-lighter-unless-completing-prefix ()
  "Remove KC mode mode-line lighter, unless completing prefix key or `..'.
Does nothing also if `kc-prefix-in-mode-line-flag' is nil."
  (unless (and (eq last-command 'kc-complete-keys)  kc-prefix-in-mode-line-flag)
    (kc-remove-lighter)))

(defun kc-remove-lighter ()
  "Remove KC mode mode-line lighter."
  (setq minor-mode-alist  (cl-remove-if (lambda (xx) (eq (car xx) 'kc-mode)) minor-mode-alist)))

(defun kc-complete-menu-bar ()
  "Complete a menu-bar menu.
This is just a restriction of `kc-complete-keys' to menu-bar menus and
submenus."
  (interactive)
  (let ((completion-ignore-case  t)) ; Not case-sensitive, by default.
    (kc-complete-keys-1 [menu-bar])))

(defun kc-complete-keys ()
  "Complete a key sequence for the currently invoked prefix key.
By default, key completion is case insensitive.

You can navigate the key-binding hierarchy (prefix-key hierarchy),
just as would navigate a file-system hierarchy (to complete directory
and file names) or a menu hierarchy (to complete submenu and menu-item
names).

Completion candidates generally have the form `KEY  =  COMMAND'.  (Use
option `kc-separator' to change the separator from the default value
of \"  =  \".)

If COMMAND is `...', then KEY is a prefix key; choosing it updates the
completion candidates list to the keys under that prefix.  For
example, choosing `C-x  =  ...' changes the candidates to those with
prefix `C-x'.

The special candidate `..' means to go up one level of the key-binding
hierarchy and show the candidates there.  For example, if you are
currently completing prefix key `C-x 4', and you choose candidate
`..', then you will be completing prefix `C-x', the parent of `C-x 5'.

Except at the top level, the default value for completion is `..'.

If option `kc-self-insert-ranges' is non-`nil', then some keys that
are bound to `self-insert-command' are included as possible
key-completion candidates; otherwise they are not.  The default value
is `nil'.

There are thousands of self-inserting keys, so it is not practical to
allow all as candidates.  Instead, a non-`nil' value is a list of
character ranges of the form (MIN . MAX).  Characters in the inclusive
range MIN through MAX are possible key-completion candidates.

Most of the thousands of self-inserting characters are Unicode
characters.  For a self-inserting character CHAR, the completion
candidate is generally `CHAR  =  self-insert-command', but if CHAR is
a Unicode character then it is `CHAR  =  UNICODE-NAME', where
UNICODE-NAME is the name of the Unicode character.  This is so that
you can complete against the name.

If you use a non-`nil' value for `kc-self-insert-ranges' then it's
best to use only small ranges for good performance.  In general, you
will want to leave this option value as `nil' and use command
`ucs-insert' (`C-x 8 RET') to insert characters."
  (interactive)
  (let ((completion-ignore-case  t)) ; Not case-sensitive, by default.
    (kc-complete-keys-1 (kc-this-command-keys-prefix))))

(defun kc-this-command-keys-prefix ()
  "Return the prefix of the currently invoked key sequence.
But IF (a) this command is `kc-complete-keys' and
       (b) it was invoked from a prefix key that is a member of option
           `kc-ignored-prefix-keys'
  THEN return [], as if `kc-complete-keys' was invoked at top level,
       i.e., with no prefix key."
  (let* ((this-key-sequence  (this-command-keys-vector))
         (this-prefix        (substring this-key-sequence 0 (1- (length this-key-sequence)))))
    (when (and (eq this-command 'kc-complete-keys)
               (kc-some kc-ignored-prefix-keys this-prefix #'kc-same-vector-keyseq-p))
      (setq this-prefix []))
    this-prefix))

(defun kc-some (lst arg2 predicate)
  "Apply binary PREDICATE successively to an item of list LST and ARG2.
Return the first non-nil value returned by PREDICATE, or nil if none.
PREDICATE must be a function with two required arguments."
  (let ((result  nil))
    (catch 'kc-some
      (dolist (arg1  lst)
        (when (setq result  (funcall predicate arg1 arg2))  (throw 'kc-some result))))
    result))

(defun kc-same-vector-keyseq-p (key1 key2)
  "Return non-nil if KEY1 and KEY2 represent the same key sequence.
Each is a vector."
  ;; Roundtrip through `key-description' and `kbd' so treat [ESC ...] and [27 ...] the same.
  (let ((desc1  (key-description (apply #'vector (mapcar #'kc-unlist key1))))
        (desc2  (key-description (apply #'vector (mapcar #'kc-unlist key2)))))
    (equal (kbd desc1) (kbd desc2))))

(defun kc-unlist (object)
  "If OBJECT is a cons, return its car; else return OBJECT."
  (if (consp object) (car object) object))

(defun kc-complete-keys-1 (prefix-vect &optional no-error)
  "Complete a key sequence for prefix key PREFIX-VECT (a vector).
Non-nil optional arg NO-ERROR means do not raise an error if no
completions are found for PREFIX-VECT."
  (kc-keys+cmds-w-prefix prefix-vect)
  (unless (or kc-keys-alist  no-error) (user-error "No keys for prefix `%s'" prefix-vect))
  (kc-lighter prefix-vect)
  (when kc-keys-alist
    (let* ((keydesc  (key-description prefix-vect nil))
           (prompt   (concat "Complete keys"
                             (and (not (string= "" keydesc)) (concat " " keydesc))
                             ": ")))
      (let ((completion-styles  kc-completion-styles))
        (minibuffer-with-setup-hook #'sorti-minibuffer-setup
          (kc-complete-keys-action
           (completing-read prompt (kc-collection-function kc-keys-alist)
                            nil t nil nil (if (equal [] prefix-vect) nil ".."))
           prefix-vect
           (this-command-keys-vector))))))) ; For error report - e.g. mouse command.

(defun kc-complete-keys-action (candidate prefix-vect this-cmd-keys-vect)
  "Complete CANDIDATE, invoking its command.
Handles `..' by going to the parent prefix key (or top level if none).
Handles `PREFIX-VECT  =  ...' by showing its keys as the candidates.
The default value when completing a prefix key is `..'."
  (let* ((key+binding    (cdr-safe (assoc candidate kc-keys-alist)))
         (key            (car-safe key+binding))
         (binding        (cdr-safe key+binding))
         (cmd-name       nil)
         (action-window  (selected-window)))
    (unwind-protect
        (progn
          (if (member candidate '(".." ""))
              (setq cmd-name  "..")
            (unless (and (string-match (concat "\\(.+\\)" kc-separator "\\(.+\\)") candidate)
                         (match-beginning 2))
              (user-error "No match"))
            (setq cmd-name  (substring candidate (match-beginning 2) (match-end 2))))
          (cond ((string= ".." cmd-name) ; Go back up to parent.
                 (setq last-command  'kc-complete-keys)
                 (kc-complete-keys-1 (vconcat (nbutlast (append prefix-vect ())))))
                ((and key  (string= "..." cmd-name)) ; Go down to prefix key's candidates.
                 (setq last-command  'kc-complete-keys)
                 (kc-complete-keys-1 (vconcat prefix-vect key)))
                (t
                 (setq this-command  binding
                       last-command  binding)
                 (when (eq 'self-insert-command binding)
                   (unless key (error "Cannot insert `%s'" key))
                   (setq last-command-event  (aref key 0)))
                 (when (eq 'digit-argument binding)
                   (setq last-command-event  (aref key 0))
                   (message "Numeric argument"))
                 (when (eq 'negative-argument binding)
                   (message "Negative argument"))
                 (setq last-nonmenu-event  1) ; So `*Completions*' mouse-click info is ignored.
                 ;; Bind so vanilla context when invoke chosen command.
                 (call-interactively binding nil this-cmd-keys-vect))))
      (select-window action-window))))

(defun kc-collection-function (items)
  "Key-completion function for `kc-mode'.
Provides metadata for display and cycle sorting, based on
`kc-sort-function-chooser'.  And provides metadata category `key'."
  (lambda (string pred action)
    (if (eq action 'metadata)
        (let ((order  (kc-sort-function-chooser)))
          `(metadata ,@(and order  `((display-sort-function . ,order)
                                     (cycle-sort-function . ,order)))
                     (category . key)))
      (complete-with-action action items string pred))))

(defun kc-sort-function-chooser ()
  "Return the sort function for the current value of `sorti-current-order'."
  (cl-case sorti-current-order
    (prefix-keys 'kc-sort-prefix-keys-first)
    (local-keys  'kc-sort-local-keys-first)
    (command     'kc-sort-by-command-name)
    (t           nil)))

(defun kc-sort-prefix-keys-first (candidates)
  "Sort CANDIDATES (strings) so that prefix keys are first."
  (let ((cands  (copy-sequence candidates)))
    (sort cands #'kc-prefix-keys-first-p)))

(defun kc-sort-local-keys-first (candidates)
  "Sort CANDIDATES (strings) so that local keys are first."
  (let ((cands  (copy-sequence candidates)))
    (sort cands #'kc-local-keys-first-p)))

(defun kc-sort-by-command-name (candidates)
  "Sort CANDIDATES (strings) by command name, alphabetically."
  (let ((cands  (copy-sequence candidates)))
    (sort cands #'kc-command-names-alphabetic-p)))

(defun kc-prefix-keys-first-p (s1 s2)
  "Non-nil if S1 is a prefix key and S2 is not or S1 < S2 (alphabet).
For this function, a prefix key is represented by a string that ends
in \"...\".

When used as a comparison function for completion candidates, this
makes prefix keys that match your input available first (at the top of
buffer `*Completions*').  Candidates are effectively in two groups,
each of which is sorted alphabetically separately: prefix keys,
followed by non-prefix keys.  Letter case is ignored.

The special key representation \"..\" is, however, less than all other
keys, including prefix keys."
  (let* ((prefix-string           (concat kc-separator "\\.\\.\\.$"))
         (parent-string           "..")
         (s1-prefix-p             (string-match-p prefix-string s1))
         (s2-prefix-p             (string-match-p prefix-string s2))
         (completion-ignore-case  t))
    (and (not (string= parent-string s2))
         (or (string= parent-string s1)
             (and (not s1-prefix-p)  (not s2-prefix-p)  (kc-case-string-less-p s1 s2))
             (and s1-prefix-p  (not s2-prefix-p))
             (and s1-prefix-p  s2-prefix-p  (kc-case-string-less-p s1 s2))))))

(defun kc-local-keys-first-p (s1 s2)
  "Non-nil if S1 is a local key and S2 is not or S1 < S2 (alphabet).
This makes local keys that match your input available first (at the
top of buffer `*Completions*').  Candidates are effectively in two
groups, each of which is sorted alphabetically separately: local keys,
followed by non-prefix keys.  Letter case is ignored.

The special key representation \"..\" is, however, less than all other
keys, including local keys."
  (or (string= ".." s1)
      (and (not (string= ".." s2))
           (let ((s1-local  (kc-local-key-binding-p s1))
                 (s2-local  (kc-local-key-binding-p s2)))
             (or (and s1-local  (not s2-local))
                 (and (not s1-local)  (not s2-local)  (kc-case-string-less-p s1 s2))
                 (and      s1-local        s2-local   (kc-case-string-less-p s1 s2)))))))

(defun kc-local-key-binding-p (candidate)
  "Return non-nil if CANDIDATE is a local key binding."
  (let ((fprop  (get-text-property 0 'face candidate)))
    (if (facep fprop)
        (or (eq 'kc-key-local fprop)  (eq   'kc-menu-local fprop))
      (or (memq 'kc-key-local fprop)  (memq 'kc-menu-local fprop)))))

(defun kc-case-string-less-p (s1 s2)
  "`string-<', but respect `case-fold-search', `completion-ignore-case'."
  (when (or case-fold-search  completion-ignore-case)
    (setq s1  (upcase s1)
          s2  (upcase s2)))
  (string-lessp s1 s2))

(defun kc-command-names-alphabetic-p (s1 s2)
  "Non-nil if command name of S1 is `kc-case-string-less-p' that of S2.
Prefix keys are sorted after candidates with command names, and they
are alphabetically."
  (or (string= ".." s1)
      (and (not (string= ".." s2))
           (let* ((pref-pat   (concat kc-separator "\\.\\.\\.$"))
                  (s1-pre-fp  (string-match-p pref-pat s1))
                  (s2-pref-p  (string-match-p pref-pat s2)))
             (or (and (not s1-pre-fp)  s2-pref-p) ; K1  =  cmd, K2  =  ...
                 (cond ((and s1-pre-fp  (not s2-pref-p)) nil) ; K1  =  ..., K2  =  cmd
                       ((and s1-pre-fp  s2-pref-p) ; K1  =  ..., K2  =  ...
                        (let* ((key-pat  (concat "\\(^.*\\)" kc-separator))
                               (___1     (string-match key-pat s1))
                               (key1     (match-string 1 s1))
                               (___2     (string-match key-pat s2))
                               (key2     (match-string 1 s2)))
                          (kc-case-string-less-p key1 key2)))
                       (t               ; K1  =  cmd1, K2  =  cmd2
                        (let* ((rhs-pat  (concat kc-separator "\\(.+\\)"))
                               (___1     (string-match rhs-pat s1))
                               (cmd1     (match-string 1 s1))
                               (___2     (string-match rhs-pat s2))
                               (cmd2     (match-string 1 s2)))
                          (kc-case-string-less-p cmd1 cmd2)))))))))

(defun kc-keys+cmds-w-prefix (prefix-vect)
  "Fill `kc-keys-alist' for prefix key PREFIX-VECT (a vector)."
  (let ((prefix-map nil))
    (setq kc-keys-alist  ())
    (dolist (active-map  (current-active-maps t))
      (setq prefix-map  (lookup-key active-map prefix-vect))
      (when (keymapp prefix-map)
        (map-keymap (lambda (evnt def)
                      (kc-add-key+cmd evnt def prefix-vect active-map))
                    prefix-map)))
    (unless (or (equal [] prefix-vect)  (null kc-keys-alist))
      (push (list (propertize ".." 'face 'match)) kc-keys-alist))))

(defun kc-add-key+cmd (event binding prefix-vect active-map)
  "Add completion for EVENT and BINDING to `kc-keys-alist'.
PREFIX-VECT is a prefix key (a vector) in ACTIVE-MAP.
EVENT AND BINDING are on that prefix key."
  (let ((bndg   binding)
        (mitem  nil))
    (cond
     ;; (menu-item ITEM-STRING): non-selectable item - skip it.
     ((and (eq 'menu-item (car-safe bndg))  (null (cdr-safe (cdr-safe bndg))))
      (setq bndg  nil))             ; So `keymapp' test, below, fails.

     ;; (ITEM-STRING): non-selectable item - skip it.
     ((and (stringp (car-safe bndg))  (null (cdr-safe bndg)))
      (setq bndg  nil))             ; So `keymapp' test, below, fails.

     ;; (menu-item ITEM-STRING REAL-BINDING . PROPERTIES)
     ((eq 'menu-item (car-safe bndg))
      (let ((enable-condition  (memq ':enable (cdr-safe (cdr-safe (cdr-safe bndg))))))
        (if (or (not enable-condition)
                (ignore-errors ; Don't enable if we can't check the condition.
                  (eval (cadr enable-condition))))
            (setq mitem  (car-safe (cdr-safe bndg))
                  bndg   (car-safe (cdr-safe (cdr-safe bndg))))
          (setq bndg  nil))))

     ;; (ITEM-STRING . REAL-BINDING) or
     ;; (ITEM-STRING [HELP-STRING] . REAL-BINDING) or
     ;; (ITEM-STRING [HELP-STRING] (KEYBD-SHORTCUTS) . REAL-BINDING)
     ((stringp (car-safe bndg))
      (setq mitem  (car bndg)
            bndg   (cdr bndg))
      ;; Skip HELP-STRING
      (when (stringp (car-safe bndg)) (setq bndg  (cdr bndg)))
      ;; Skip (KEYBD-SHORTCUTS): cached key-equivalence data for menu items.
      (when (and (consp bndg)  (consp (car bndg))) (setq bndg  (cdr bndg)))))

    (cond ((and (eq bndg 'self-insert-command) ; Insert `self-insert-command' char ranges.
                kc-self-insert-ranges
                (consp event)           ; Char range.
                (characterp (car event)))
           (let ((chr1  (car event))
                 (chr2  (cdr event)))
             (cl-loop for range in kc-self-insert-ranges do
                      (cl-loop for char from (max chr1 (car range)) to (min chr2  (cdr range)) do
                               (let* ((key-desc     (single-key-description char t))
                                      (name.char    (rassq char (ucs-names)))
                                      (cand-strg  (concat key-desc
                                                          kc-separator
                                                          (if name.char
                                                              (car name.char)
                                                            "self-insert-command")))
                                      (candidate  (and (or (not name.char)
                                                           (not (string= "" (car name.char))))
                                                       (propertize
                                                        cand-strg
                                                        'face
                                                        (if (eq active-map (current-local-map))
                                                            'kc-key-local
                                                          'kc-key-non-local)))))
                                 (when candidate
                                   (push (cons candidate (cons (vector char) 'self-insert-command))
                                         kc-keys-alist)))))))
          ((and
            (or (keymapp bndg)
                (and (commandp bndg)
                     (equal bndg (key-binding (vconcat prefix-vect (vector event)) nil 'NO-REMAP))
                     (not (eq bndg 'kc-complete-keys))))
            ;; Include BINDING if it is not `self-insert-command' or user has OK'd use of such.
            (or (not (eq bndg 'self-insert-command)) ; Command, keymap.
                (and kc-self-insert-ranges  (characterp event))))

           (when (and (characterp event) ; `ESC' -> `M-'.
                      (eq event meta-prefix-char)
                      (keymapp bndg))
             (map-keymap (lambda (key bndg)
                           (when (characterp key)
                             (kc-add-key+cmd (event-apply-modifier key 'meta 27 "M-")
                                             bndg prefix-vect active-map)))
                         bndg))
           (when (and (functionp bndg)  (commandp bndg)) ; Follow remapped command to target.
             (setq bndg  (key-binding (vconcat prefix-vect (vector event)))))
           (let* ((key-desc   (if (and (stringp mitem)  (keymapp bndg))
                                  mitem ; Menu item.
                                (single-key-description (if (stringp mitem) mitem event) t)))
                  (cand-strg  (concat key-desc
                                      kc-separator
                                      (if (keymapp bndg)
                                          "..."
                                        (with-output-to-string (princ bndg))))) ; Use `princ'
                  (candidate  (propertize cand-strg
                                          'face
                                          (if (and (stringp mitem)  (keymapp bndg))
                                              (if (eq active-map (current-local-map))
                                                  'kc-menu-local
                                                'kc-menu-non-local)
                                            (if (eq active-map (current-local-map))
                                                'kc-key-local
                                              'kc-key-non-local)))))
             ;; Skip keys bound to `undefined'.
             (unless (string= "undefined" (with-output-to-string (princ bndg)))
               (push (cons candidate (cons (vector event) bndg)) kc-keys-alist)))))))

;;;;;;;;;;;;;;;;;;;;;;

(provide 'keysee)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keysee.el ends here
