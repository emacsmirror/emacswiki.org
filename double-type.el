;;; double-type.el --- keyboard operation method corresponding to double click on a mouse.

;; Copyright (C) 2010 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: Fri Dec 24 02:33:06 2010 JST
;; Keywords: abbrev convenience emulations wp
;; Revision: $Id$
;; URL: https://github.com/k-talo/double-type.el
;; GitHub: http://github.com/k-talo/double-type.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; NOTE
;;
;; This library is just tested on Emacs 23.2.1 on Mac OS X 10.6.5,
;; and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; Overview
;; ========
;; This library provides a keyboard operation method corresponding
;; to double click on a mouse.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'double-type)
;;
;;
;; USING
;; =====
;; You can bind commands to keyboard events "single type" and
;; "double type" on a key via a function `double-type/define-key'
;; which is provided by this library.
;;
;;   Examples)
;;
;;     ;; Save buffer with key <double-C-s>.
;;     (double-type/define-key global-map "\C-s"
;;                             :on-single-type 'isearch-forward
;;                             :on-double-type 'save-buffer)
;;     
;;     ;; Other window with key <double-C-o>.
;;     (double-type/define-key global-map "\C-o"
;;                             :on-single-type 'open-line
;;                             :on-double-type 'other-window)
;;     
;;     ;; Open RE-Builder with key <double-C-r>.
;;     (double-type/define-key global-map "\C-r"
;;                             :on-single-type 'isearch-backward
;;                             :on-double-type 're-builder)
;;     
;;     ;; Insert current time with key <double-M-t>.
;;     (double-type/define-key esc-map "t"
;;                             :on-single-type 'transpose-words
;;                             :on-double-type (lambda ()
;;                                               (interactive)
;;                                               (insert (current-time-string))))
;;     
;;     ;; Insert mail address by keyboard macro with key <double-@>.
;;     (double-type/define-key global-map "@"
;;                             :on-single-type 'self-insert-command
;;                             :on-double-type "my-name@example.com")
;;     
;;     ;; Use <double-C-w> as prefix key `ctl-x-4-map'.
;;     (double-type/define-key global-map "\C-w"
;;                             :on-single-type 'kill-region
;;                             :on-double-type ctl-x-4-map)
;;
;;   NOTE: You had better not to assign "double type" event to the
;;         keys which will be typed repeatedly, like "C-f", "C-b"
;;         and so on. (Such assignment may be annoying to you...)
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; - Codes aside, this document should be rewritten.
;;   My English sucks.
;;
;; 
;; WISH LIST
;; =========
;; - Show "single-type" and "double-type" bindings by `where-is'.

;;; Change Log:

;;; Code:

(provide 'double-type)

(defconst double-type/version "0.9.0")

(eval-when-compile
  (require 'cl))

(def double-type/timeout 0.4)
(defvar double-type/use-prompt t)

 
;;; ===========================================================================
;;;
;;;  Public Functions.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-type/define-key keymap key &key on-single-type on-double-type)
;;                                      => HANDLER COMMAND ASSIGNED TO THE KEY
;; ----------------------------------------------------------------------------
(defun* double-type/define-key (keymap key
                                       &key
                                       on-single-type
                                       on-double-type)
  "In KEYMAP, define key sequence KEY as ON-SINGLE-TYPE and
ON-DOUBLE-TYPE.

KEY is a string or a vector of symbols and characters meaning a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.
Using [t] for KEY creates a default definition, which applies to any
event type that has no other definition in this keymap.

ON-SINGLE-TYPE ON-DOUBLE-TYPE are anything that can be a
key's definition:

 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.).

See also `define-key'."
  (let ((fn-name (intern
                  (format "double-type/cmd-%s"
                          (gensym)))))
    (put fn-name 'double-type/cmd-p t)
    
    ;; Bind a closure, which handles event by the KEY, to a KEY.
    (setf (symbol-function fn-name)
          (lexical-let ((on-single-type on-single-type)
                        (on-double-type on-double-type))
            #'(lambda ()
                (interactive)
                (funcall 'double-type/.track-event
                         (list :single-type on-single-type
                               :double-type on-double-type)))))
    (define-key keymap key fn-name)))

 
;;; ===========================================================================
;;;
;;;  Private Functions.
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (double-type/.track-event EV-data) => VOID
;; ----------------------------------------------------------------------------
(defun double-type/.track-event (ev-data)
  "Track key event.

EV-DATA is a list like:

  (:single-type BINDING
   :double-type BINDING)

EV-DATA will be used to handle a key event."
  ;; Start tracking "double-type" key event of a key.
  ;;
  (let* ((ev-tracking last-command-event)
         (ev-read     nil)
         (ev-next     (with-timeout
                          (double-type/timeout 'timeout)
                        (setq ev-read
                              (read-event nil)))))
    (message "TRACK: %s, NEXT: %s, READ: %s" ev-tracking ev-next ev-read)
    (cond
     ;; Got "double type" event.
     ;;
     ((eq ev-next
          ev-tracking)
      (double-type/.do-key :ev-tracking ev-tracking
                           :ev-kind     :double-type
                           :ev-data     ev-data))
     ;; Timeout, or got another event.
     ;;
     (t
      (double-type/.do-key :ev-tracking ev-tracking
                           :ev-kind     :single-type
                           :ev-data     ev-data)
      ;; Unread another event if any.
      (when ev-read
        (push ev-read unread-command-events))))))

;; ----------------------------------------------------------------------------
;;  (double-type/.do-key &key ev-tracking ev-kind ev-data) => VOID
;; ----------------------------------------------------------------------------
(defun* double-type/.do-key (&key ev-tracking ev-kind ev-data)
  "Run a thing bound to current key event."
  ;; FIXME: Write codes which handles `indirect entry'.
  (let ((binding  (cadr (memq ev-kind ev-data)))
        (key-desc (format "%s%s%s"
                          (if (eq ev-kind :double-type) "<double-" "")
                          (single-key-description ev-tracking)
                          (if (eq ev-kind :double-type) ">" ""))))
    ;; When binding is a function, get `symbol-function'
    ;; of a binding.
    (setq binding
          (double-type/.do-key/aux/expand-symbol-function
           binding key-desc))
    
    ;; When binding is a prefix key, read next event
    ;; then find binding from the prefix key.
    (setq binding
          (double-type/.do-key/aux/read-with-prefix-key
           binding key-desc))
    
    (cond
     ;; Binding is a Command.
     ;;
     ((and (functionp binding)
           (commandp binding))
      (let ((this-command binding))
        (call-interactively binding)))
     
     ;; Binding is a Keyboard Macro.
     ;;
     ((arrayp binding)
      (execute-kbd-macro binding))

     ((null binding)
      ;; Do nothing
      )
     (t
      (error "Unknown binding type: \"%s\"."
             binding)))))

;; ----------------------------------------------------------------------------
;;  (double-type/.do-key/aux/expand-symbol-function binding key-desc) => OBJECT
;; ----------------------------------------------------------------------------
(defun double-type/.do-key/aux/expand-symbol-function (binding key-desc)
  "Returns symbol function of BINDING if it is a symbol."
  (let (seen)
    (while (and (not (commandp binding))
                (symbolp binding)
                (not (eq (symbol-function binding)
                         binding)))
      (when (memq binding seen)
        (error "Loop in binding of %s." key-desc))
      (pushnew binding seen)
      (setq binding (symbol-function binding)))
    binding))

;; ----------------------------------------------------------------------------
;;  (double-type/.do-key/aux/read-with-prefix-key binding key-desc) => OBJECT
;; ----------------------------------------------------------------------------
(defun double-type/.do-key/aux/read-with-prefix-key (binding key-desc)
  "Read key then returns binding of the key in BINDING
when it is a prefix key."
  (when (keymapp binding)
    (let* ((global-map nil)
           (overriding-local-map binding)
           (key (read-key-sequence
                 (cond (double-type/use-prompt
                        (format "[double-type] %s-"
                                key-desc))
                       (t nil))))
           (key-def (and key
                         (lookup-key binding key))))
      (when (not key-def)
        (error (format "%s %s is undefined"
                       key-desc
                       (key-description key))))
      (setq binding key-def)))
  binding)

;;; double-type.el ends here
