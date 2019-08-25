;;;; keycentric.el --- Define keys for *all* keymaps all at once  -*- lexical-binding: t; fill-column: 80 -*-
;;
;; Copyright (C) 2019  Hai Nguyen
;;
;; Author: Hai Nguyen <hainguyenuniverse@gmail.com>
;; Created: 2019-08-19
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.2"))
;; Keywords: dotemacs startup keymap
;; URL: <https://www.emacswiki.org/Keycentric>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;;
;; * Keycentric -- putting key-bindings of different keymaps into one place
;; 
;; In my Emacs key-bindings, a single key-event (e.g. "<f5>") is bound in different minor-mode's keymaps, and usually the functions bound to each key perform similar tasks. To change the binding for a single key-event, I need to visit different places in my file to change it.
;; 
;; 
;; This library seek to centralize all key-bindings into one place, by combining all key-bindings to the same keyevents in different keymaps into one single place (please see the example above for illustration).
;; 
;; 
;; This library does not bind multiple commands to one key, nor does it compose keymaps into a single one as does the built-in function `make-composed-keymap'.
;; 
;; 
;; Below is a (small) example:
;; 
;; 
;; #+BEGIN_SRC emacs-lisp
;;   (when (require 'keycentric nil t)
;;     (keycentric
;;      `(("<f5>" (nil (global-map . backward-up-list)))
;;        ("<f6>" (nil (global-map . (lambda () (interactive)
;;                                     (and (revert-buffer nil t)
;;                                          (message "buffer reverted."))))))
;;        ("<f7>" (nil (global-map . isearch-forward-regexp))
;;                (isearch (isearch-mode-map . isearch-repeat-forward)))
;;        ("<f8>" (eshell (:eval (add-hook 'eshell-mode-hook
;;                                         (lambda () (define-key eshell-mode-map
;;                                                      keycentric-key
;;                                                      #'view-echo-area-messages)))))))))
;; #+END_SRC
;; 
;; 
;; In the example, "nil" in "(nil (global-map . backward-up-list))" means the subsequent keymaps (the global-map in this example) are supposed to be available at the time this form gets executed.
;; 
;; 
;; The :eval form (the last key-binding in the example) is provided for cases when there is no other ways in the library to define the key-binding. In this :eval form, the key-event to be bound could be replaced with the variable `keycentric-key', which is provided as a convenience (user can still re-type the key-event inside the form instead of using the variable `keycentric-key'). Key-Binding for eshell is used in the example because eshell-mode-map is a local-buffer map that is only activated when eshell-mode is activated, thus to define a keymapping for eshell-mode-map one may need to add-hook as in the example.
;; 
;; 
;; * Features
;; 
;; - Key-binding based on key-event instead of on the keymap.
;; - Delaying the keymap binding until the specified feature is loaded.
;; - An :eval keyword to define a form to handle any extraordinary case.
;; 
;; * Limitations
;; 
;; - Single point of failure. This library seeks to centralize all key-bindings into one place. However, this library will ignore any error during execution so as to avoid choking in any one place.
;; - Using `eval-after-load' for delayed key-binding for key-map unavailable at the time of execution: any misconfiguration on the key-binding may remain hidden until the feature gets loaded and error is raised.
;; - Repetition of the keymap names for each key, due to the 1-many mapping of each key to multiple maps.
;; 
;; * Installation
;; 
;; After downloading this library to somewhere on your computer (let's call it path-to-keycentric in the code below):
;; 
;; 
;; #+BEGIN_SRC emacs-lisp
;; 
;; (add-to-list 'load-path path-to-keycentric)
;; (require 'keycentric)
;; #+END_SRC
;; 
;; * References
;; 
;; - [[https://github.com/jwiegley/use-package][bind-key (linked to use-package github page)]]


;;; Code:
(defconst keycentric-version "0.0.2")


(defun keycentric--get-map-value (map-form)
  "Returns the value of the MAP-FORM.

MAP-FORM is either a symbol of the keymap (e.g. 'global-map), or a function-symbol that return the keymap (e.g. 'current-local-map), or a pair of a hook symbol and a keymap symbol (e.g. '(eshell-mode-hook . eshell-mode-map))."
  (cond
   ((symbolp map-form)
    (if (boundp map-form)
        (symbol-value map-form)
      nil))
   ((listp map-form) map-form)
   (t (user-error "Unhandled case: map form `%s' of type `%s'"
                  map-form (type-of map-form)))))


(defun keycentric--define-key (map-form key value)
  "Evaluate the given MAP-FORM to define KEY with VALUE in it.
MAP-FORM: either a function form or a hook."
  (let ((map-or-hook-and-map (keycentric--get-map-value map-form)))
    (cond
     ((null map-or-hook-and-map) nil)
     ((keymapp map-or-hook-and-map)
      (define-key map-or-hook-and-map key value)
      map-or-hook-and-map)
     ((listp map-or-hook-and-map)
      (let ((hook-symbol (car map-or-hook-and-map))
            hook
            (map-symbol (cdr map-or-hook-and-map))
            map)
        (cond
         ((null (boundp hook-symbol))
          (error "hook symbol not yet bound: `%s'!" hook-symbol))
         ((null (boundp map-symbol))
          (error "map symbol not yet bound: `%s'!" map-symbol)))
        (setf hook (symbol-value hook-symbol)
              map (symbol-value map-symbol))
        (cond
         (;; there is no built-in way to check for type hook, so checking for list here:
          (null (listp hook))
          (error "hook symbol `%s' should be of type list, but it is of type `%s'!"
                 hook-symbol (type-of hook)))
         ((null (keymapp map))
          (error "map symbol `%s' should be of type keymap, but it is of type `%s'!"
                 map-symbol (type-of map))))
        (add-hook hook (lambda () (define-key map key value))))))))


(defun keycentric (mapping-list)
  "MAPPING-LIST a list of forms, each form takes the same arguments as below:

KEY: either a vector of key sequence or a string (which will be fed to the `kbd' function).
FUN: the function symbol/lambda form to be mapped to the key."
  (let (key-arg
        key
        form
        package-define-keys-form
        define-key-form
        map-form
        map
        package-symbol
        fun-symbol)
      (while (car mapping-list)
        (setf form (pop mapping-list)
              key-arg (pop form)
              key (cond ((vectorp key-arg) key-arg)
                        ((stringp key-arg) (kbd key-arg))
                        (t (user-error "Wrong argument's datatype: KEY (%s) is of type `%s' instead of type vector or string!" key-arg (type-of key-arg)))))
          (while (car form)
            (setf package-define-keys-form (pop form)
                  package-symbol (pop package-define-keys-form))
            (while (car package-define-keys-form)
              (setf define-key-form (pop package-define-keys-form))
              ;; if after the package-form follows a form starting with the keyword :eval, just `eval' the remaining of the form, otherwise parse it and evaluate
              (ignore-errors
                (if (eq (car define-key-form) :eval)
                  (progn
                    (pop define-key-form)
                    (eval-after-load package-symbol
                      `(let ((keycentric-key ,key))
                         ,@define-key-form)))
                  (progn ; do some more parsing and eval it:
                    (setf map-form (pop define-key-form)
                          fun-symbol define-key-form)
                    (cond
                     ((keycentric--define-key map-form key fun-symbol))
                     ((null (featurep package-symbol))
                      (eval-after-load package-symbol
                        `(keycentric--define-key ',map-form ,key ',fun-symbol)))
                     (t (user-error "Package `%s' has already been loaded but keymap `%s' is `%s', which is not a keymap!" package-symbol map-form map)))))))))))


(provide 'keycentric)
;;; keycentric.el ends here
