;;; old-fashioned-undo.el --- Undo/Redo in very simple, and old fashioned way.

;; Copyright (C) 2001-2003, 2010, 2013 K-talo Miyazaki, all rights reserved.
;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: Fri Nov 19 14:13:19 2010 JST
;; Keywords: emulation wp
;; Revision: $Id: 3440ab0bde58a26852d3f12858b2885e48e3014b $
;; URL: 
;; GitHub: http://github.com/k-talo/old-fashioned-undo.el
;; Version: 6.0

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
;; This library is just tested on Emacs 24.3.1 on Ubuntu 10.04
;; and Mac OS X 10.8.3, and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; Overview
;; ========
;;
;; This library provides minor mode `old-fashioned-undo-mode' which
;; makes `undo' and `redo' command working in an old fashioned way.
;;
;; The default `undo/redo' commands provided by emacs records each 
;; `undo/redo' operations on `buffer-undo-list'. This behavior may
;; make undo/redo operations perfect, but I feel this behavior is
;; too much verbose and little bit annoying.
;;
;; The undo/redo command provided this library never records
;; `undo/redo' operation on `buffer-undo-list' so that we can
;; `undo/redo' in intuitive way.
;;
;; Additionally, while `old-fashioned-undo-mode' is on, the number
;; of pending `undo/redo' operation will be displayed in the
;; minibuffer when each `undo/redo' command is executed.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'old-fashioned-undo)
;;    (old-fashioned-undo-mode t)
;;
;;
;; USING
;; =====
;; To toggle old-fashioned-undo feature, just type:
;;
;;   `M-x old-fashioned-undo-mode RET'
;;
;;
;; Key map Examples
;; ================
;; (global-set-key [(control z)] 'undo)
;; (global-set-key [(control Z)] 'redo)
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; - Cursor position won't be set properly after undo/redo when cua-mode
;;   is on.

;;; Change Log:
;; v6.0 04/04/2013  Renamed to `old-fashioned-undo.el'.
;; v5.2 02/04/2012  Fixed compiler errors.
;;                  Fixed errors which occurs to XEmacs.
;; v5.1 12/27/2010  Fixed compiler errors and a typo.
;; v5.0 11/19/2010  Renamed to `linear-undo.el'.
;;                  Heavily arranged codes.
;;                  Moved repository from subversion to git.
;; v4.0 10/14/2010  Moved codes regarding to highlighting modified text to
;;                  new library `volatile-highlights.el'.
;; v3.1 10/17/2003  Arranged name of functions and variables.
;;                  (No bug fix and new feature.)
;; v3.0 10/12/2003  emacs21 support.
;; v2.7 10/07/2001  Fixed a bug that undo/redo elements is not counted properly
;;                  when saving information exists in the buffer-undo/redo-list.
;; v2.6 10/06/2001  Fixed a bug that highlights are not removed when frame
;;                  is switched just after undo/redo command.
;;                  (Just make 'old-fashioned-undo/buffer-highlight-list' global variable.)
;; v2.5 10/05/2001  Made optional variable customizable.
;;                  I think all of the feature, that I wanted when I started
;;                  to write this library, are implemented in this version.
;; v2.4 10/05/2001  Inhibit unintended cursor movement by undo/redo.
;; v2.3 10/05/2001  Remove highlights automatically.
;; v2.2 10/04/2001  Display count of undo/redo elements which remains in
;;                  buffer-undo/redo-list.
;;                  New option `allow-remove-boundary-p'.
;;                  Fixed a few bugs.
;; v2.1 10/04/2001  New function 'old-fashioned-undo/lst/get-next'.
;;                  (to prepare for implementing new feature, counting
;;                   undo/redo elements in buffer-undo/redo-list.)
;;                  Fixed a few bugs.
;; v2.0 10/03/2001  Highlight text which is inserted by undo/redo command.
;;                  (very experimental)
;;                  Codes are heavily modified.
;; v1.2 10/02/2001  Fixed some bugs.
;; v1.1 09/29/2001  Menu-bar support.
;; v1.0 09/28/2001  Initial version.


;;; Code:
(eval-when-compile
  (require 'cl))

(defvar old-fashioned-undo/version "6.0")
(defun old-fashioned-undo/version ()
  (interactive)
  (message "old-fashioned-undo version %s" old-fashioned-undo/version))

 
;;;============================================================================
;;;
;;;  Private Variables.
;;;
;;;============================================================================

(eval-and-compile
  (defconst old-fashioned-undo/.xemacsp (string-match "XEmacs" emacs-version)
    "A flag if the emacs is xemacs or not."))

 
;;;============================================================================
;;;
;;;  Suppress compiler warnings regarding to emacs/xemacs private functions.
;;;
;;;============================================================================
(eval-when-compile
  (dolist (func (cond (old-fashioned-undo/.xemacsp
                       '(minibufferp
                         define-key-after))
                      (t
                       '(add-menu-button
                         delete-menu-button
                         delete-menu-item))))
      (when (not (fboundp func))
        (setf (symbol-function func) (lambda (&rest args))))))

(eval-and-compile
  (cond
   ;; XEmacs
   (old-fashioned-undo/.xemacsp
    (defun old-fashioned-undo/.minibufferp (buf)
      (string-match "^ \\*Minibuf-[0-9]+\\*$"
                    (cond
                     ((stringp buf)
                      buf)
                     ((bufferp buf)
                      (buffer-name buf))
                     (t
                      (error "Wrong type argument bufferp %S" 123))))))
   ;; GNU Emacs
   (t
    (defun old-fashioned-undo/.minibufferp (buf)
      (funcall 'minibufferp buf)))))

;;  
;;; ===========================================================================
;;;
;;;  Customazable things.
;;;
;;; ===========================================================================

(defgroup old-fashioned-undo nil
  "Undo/Redo in an old fashioned way."
  :tag "Old Fashioned Undo"
  :group 'undo)

(defcustom old-fashioned-undo/allow-remove-boundary-p nil
  "Usually, `undo' command with numeric argument removes undo boundaries
in a series of the undo operation from `buffer-undo-list'.

When `nil' is set to this variable, undo buouaries will not removed
even if prefix argument is passed or not."
  :type 'boolean
  :group 'old-fashioned-undo)

 
;;; ===========================================================================
;;;
;;;  Variables
;;;
;;; ===========================================================================

;; We use original `buffer-undo-list' in this library.

(defvar old-fashioned-undo/buffer-redo-list nil
  "List of redo entries in current buffer.

See also `buffer-undo-list'.")
(make-variable-buffer-local 'old-fashioned-undo/buffer-redo-list)

(defvar old-fashioned-undo/.last-buffer-undo-list nil
  "Private variable to detect if a buffer is edited or not
after last undo/redo command.")
(make-variable-buffer-local 'old-fashioned-undo/.last-buffer-undo-list)

 
;;; ===========================================================================
;;;
;;;  Commands
;;;
;;; ===========================================================================

;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/undo &optional arg) => VOID
;; ----------------------------------------------------------------------------
(defun old-fashioned-undo/undo (&optional arg)
  "Undo in an old fashioned way."
  (interactive "*p")
  (old-fashioned-undo/undo-aux :count (or arg 1) :by-chunk-p t))

;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/undo-1 &optional arg) => VOID
;; ----------------------------------------------------------------------------
(defun old-fashioned-undo/undo-1 (&optional arg)
  "Undo just one element in an old fashioned way."
  (interactive "*p")
  (old-fashioned-undo/undo-aux :count (or arg 1) :by-chunk-p nil))

;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/redo &optional arg)
;; ----------------------------------------------------------------------------
(defun old-fashioned-undo/redo (&optional arg)
  "Redo in an old fashioned way."
  (interactive "*p")
  (old-fashioned-undo/redo-aux :count (or arg 1) :by-chunk-p t))

;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/redo-1 &optional arg)
;; ----------------------------------------------------------------------------
(defun old-fashioned-undo/redo-1 (&optional arg)
  "Redo just one element in an old fashioned way."
  (interactive "*p")
  (old-fashioned-undo/redo-aux :count (or arg 1) :by-chunk-p nil))

 
;;; ===========================================================================
;;;
;;;  Functions
;;;
;;; ===========================================================================

 
;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/undo-aux &key count by-chunk-p) => VOID
;; ----------------------------------------------------------------------------
(defun* old-fashioned-undo/undo-aux (&key count by-chunk-p)
  "Auxiliary function for `old-fashioned-undo/undo' and `old-fashioned-undo/undo-1'."
  
  (if (eq buffer-undo-list t)
      (error "No undo information in this buffer"))
  
  ;; When `buffer-undo-list' is empty, quit with error message.
  (or buffer-undo-list
      (error "No further undo information"))
  
  ;; Unknown error.
  (or (null (car buffer-undo-list))
      (error "Can't undo! Something is wrong with `buffer-undo-list'!"))
  
  ;; Display message.
  (when (not (old-fashioned-undo/.minibufferp (window-buffer (selected-window))))
    (message "Undo..."))
  
  (let ((buf-modefied-since-last-undo-p
         (not (eq buffer-undo-list old-fashioned-undo/.last-buffer-undo-list)))
        (orig-modefied-p (buffer-modified-p))
        (recent-save (recent-auto-save-p)))
    
    ;; When buffer is modified since last undo or redo,
    ;; reset the `buffer-redo-list'.
    (and buf-modefied-since-last-undo-p
         (setq old-fashioned-undo/buffer-redo-list nil))
    
    ;; Do the undo operation.
    (old-fashioned-undo/run-primitive-undo :count count
                                    :undo-lst-name 'buffer-undo-list
                                    :redo-lst-name 'old-fashioned-undo/buffer-redo-list
                                    :by-chunk-p by-chunk-p)
    
    ;; Save current undo state for next undo/redo operation.
    (setq old-fashioned-undo/.last-buffer-undo-list buffer-undo-list)
    
    ;; Remove auto-save file when it has no meanings.
    (and orig-modefied-p (not (buffer-modified-p))
         (delete-auto-save-file-if-necessary recent-save)))
  
  (old-fashioned-undo/display-finish-info :cmd-name "Undo" :count count :by-chunk-p by-chunk-p))

 
;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/redo-aux &key count by-chunk-p) => VOID
;; ----------------------------------------------------------------------------
(defun* old-fashioned-undo/redo-aux (&key count by-chunk-p)
  "Auxiliary function for `old-fashioned-undo/redo' and `old-fashioned-undo/redo-1'."

  (if (eq buffer-undo-list t)
      (error "No redo information in this buffer"))
  
  ;; When `old-fashioned-undo/buffer-redo-list' is empty, quit with error message.
  (or old-fashioned-undo/buffer-redo-list
      (error "No further redo information"))
  
  ;; Unknown error.
  (or (null (car old-fashioned-undo/buffer-redo-list))
      (error "Can't redo! Something is wrong with the `old-fashioned-undo/buffer-redo-list'!"))

  ;; Display message.
  (when (not (old-fashioned-undo/.minibufferp (window-buffer (selected-window))))
      (message "Redo..."))

  (let ((buf-modefied-since-last-undo-p
         (not (eq buffer-undo-list old-fashioned-undo/.last-buffer-undo-list)))
        (orig-modefied-p (buffer-modified-p))
        (recent-save (recent-auto-save-p)))
    
    ;; When buffer is modified since last undo or redo,
    ;; quit with error message.
    (and buf-modefied-since-last-undo-p
         (progn (setq old-fashioned-undo/buffer-redo-list nil)
                (error "Buffer modified since last undo/redo, cannot redo")))
    
    ;; Do the redo operation.
    (old-fashioned-undo/run-primitive-undo :count count
                                    :undo-lst-name 'old-fashioned-undo/buffer-redo-list
                                    :redo-lst-name 'buffer-undo-list
                                    :by-chunk-p by-chunk-p)

    ;; Save current undo state for next undo/redo operation.
    (setq old-fashioned-undo/.last-buffer-undo-list buffer-undo-list)
    
    ;; Remove auto-save file when it has no meanings.
    (and orig-modefied-p (not (buffer-modified-p))
         (delete-auto-save-file-if-necessary recent-save)))

  (old-fashioned-undo/display-finish-info :cmd-name "Redo" :count count :by-chunk-p by-chunk-p))

 
;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/display-finish-info COMMAND_NAME REPEAT_COUNT) => VOID
;; ----------------------------------------------------------------------------
(defun* old-fashioned-undo/display-finish-info (&key cmd-name count by-chunk-p)
  "Display the number of pending undo/redo."
  (or (eq (selected-window) (minibuffer-window))
      (message "%s%s%s! [Undo: %d%s / Redo: %d%s]"
               ;; Name of current command.
               cmd-name
                 
               ;; When undoing by one element, append "-1" to the command name.
               (if by-chunk-p "" "-1")
                 
               ;; When the prefix argument is greater than 1,
               ;; display the number.
               (if (> count 1) (format "(%s)" count) "")

               ;; Display count of the undo chunks remaining.
               (old-fashioned-undo/lst/get-count buffer-undo-list
                                          :chunk-p t)
                 
               ;; Display count of undo elements remaining.
               (if (not by-chunk-p)
                 (format "(%d)"
                         (old-fashioned-undo/lst/get-count buffer-undo-list
                                                    :chunk-p nil))
                 "")

               ;; Display count of the redo chunks remaining.
               (old-fashioned-undo/lst/get-count old-fashioned-undo/buffer-redo-list
                                          :chunk-p t)

               ;; Display count of redo elements remaining.
               (if (not by-chunk-p)
                   (format "(%d)"
                           (old-fashioned-undo/lst/get-count old-fashioned-undo/buffer-redo-list
                                                      :chunk-p nil))
                 ""))))

 
;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/run-primitive-undo COUNT SYM_LIST_1 SYM_LIST_2) => VOID
;; ----------------------------------------------------------------------------
(defun* old-fashioned-undo/run-primitive-undo (&key count undo-lst-name redo-lst-name by-chunk-p)
  "Execute undo/redo by primitive-undo."
  ;; Prepare for undo/redo BY ONE STEP.
  (when (not by-chunk-p)
    (old-fashioned-undo/lst/split (symbol-value undo-lst-name) :limit count))
    
  (let ((chunk-lst-to-redo (old-fashioned-undo/run-primitive-undo-aux :count count
                                                               :undo-lst-name undo-lst-name)))
    ;; Remove `POSITION' elements in canceling chunks, because a comment
    ;; in 'simple.el' says:
    ;;
    ;;   ;; Don't specify a position in the undo record for the undo command.
    ;;   ;; Instead, undoing this should move point to where the change is.
    ;;
    ;; This prevents from unintended movement of point caused by undo operations.
    ;; (This block is added in v2.4)
    (setq chunk-lst-to-redo 
          (remove-if #'integerp chunk-lst-to-redo))
    ;; XXX: Experimental 2010/11/5
    (setq chunk-lst-to-redo
          (remove-if (lambda (elt) (markerp (car elt))) chunk-lst-to-redo))
    
    ;; Push `chunk-lst-to-redo' to `redo-lst'.
    (set (symbol-value 'redo-lst-name)
         (nconc chunk-lst-to-redo
                (symbol-value redo-lst-name)))))

 
;; ----------------------------------------------------------------------------
;;  (old-fashioned-undo/run-primitive-undo-aux &key count undo-lst-name by-chunk-p)
;;                                                         => chunk-lst-to-redo
;; ----------------------------------------------------------------------------
(defun* old-fashioned-undo/run-primitive-undo-aux (&key count undo-lst-name by-chunk-p)
  ""
  (flet ((1st-is-not-boundary (lst)
                              (not (null (car lst))))
         (ldiff! (lst sublst)
                 ;; Remove SUBLIST from LIST then return LIST.
                 ;; 
                 ;; Use this function like nconc:
                 ;; (setq list (old-fashioned-undo/ldiff! list sublist))
                 (let ((idx 0))
                   (while (and (consp (nthcdr idx lst)) (not (eq (nthcdr idx lst) sublst)))
                     (setq idx (1+ idx)))
                   (setf (nthcdr idx lst) nil)
                   lst)))
    (let ((chunk-lst-to-redo nil)
          (inhibit-quit nil)
          orig-buffer-undo-list
          new-undo-lst)
    
      ;; Remember current state of the `buffer-undo-list'.
      ;; to detect a chunk which redoes current undo operation later,
      (setq orig-buffer-undo-list buffer-undo-list)
    
      ;; Execute the undo operation by `primitive-undo'.
      (setq new-undo-lst
            (primitive-undo
             (if old-fashioned-undo/allow-remove-boundary-p count 1)
             (cdr (symbol-value undo-lst-name))))

      ;; Due to the side effect of the operation `primitive-undo',
      ;; a chunk, which cancells current undo operation, will be
      ;; prepended to the `buffer-undo-list'.
      ;;
      ;; We pick that chunk from the `buffer-undo-list' and prepend to
      ;; `chunk-lst-to-redo'.
      (setq chunk-lst-to-redo
            (nconc (ldiff! buffer-undo-list orig-buffer-undo-list)
                   chunk-lst-to-redo))
      (setq buffer-undo-list orig-buffer-undo-list)
        
      ;; Push a boundary to `chunk-lst-to-redo' if necessary.
      (when (1st-is-not-boundary chunk-lst-to-redo)
        (push nil chunk-lst-to-redo))

      ;; Update value of `undo-lst'.
      (set (symbol-value 'undo-lst-name)
           new-undo-lst)
        
      ;; Push a bundary to `undo-lst' if necessary.
      (when (1st-is-not-boundary (symbol-value undo-lst-name))
        (push nil (symbol-value undo-lst-name)))

      ;; Continue running premitive-undo by recursive call.
      (when (and (not old-fashioned-undo/allow-remove-boundary-p)
                 (< 1 count)
                 (symbol-value undo-lst-name)) ;`undo-lst' is not empty.
        (setq chunk-lst-to-redo
              (nconc (old-fashioned-undo/run-primitive-undo-aux :count (1- count)
                                                         :undo-lst-name undo-lst-name
                                                         :by-chunk-p by-chunk-p)
                     chunk-lst-to-redo)))
        
      chunk-lst-to-redo)))

 
;;; ===========================================================================
;;;
;;;  old-fashioned-undo/lst: Manipulate Buffer Undo List.
;;;
;;; ===========================================================================

 
;; -----------------------------------------------------------------------------
;;  (old-fashioned-undo/lst/split lst &key limit) => num-of-inserted-bounds
;; -----------------------------------------------------------------------------
(defun* old-fashioned-undo/lst/split (lst &key limit)
  "Split LST by inserting boundary between each elements
from 1st to (1+ LIMIT)th element.

Returns number of boundary inserted to the LST."
    (let ((next-chunk (old-fashioned-undo/lst/get-next lst :chunk-p nil))
          (num-inserted 0))
      
      (when (< 0 limit)
        (when next-chunk ;;The lst is not empty
          (when (car next-chunk)  ;;The next-element is not boundary
            ;; Push boundary to the next-chunk like:
            ;;   (xth-elm ...) => (nil xth-elm ...)
            (setcdr next-chunk
                    (cons (car next-chunk) (cdr next-chunk)))
            (setcar next-chunk nil)
            (incf num-inserted))
          (setq num-inserted (+ num-inserted
                                (old-fashioned-undo/lst/split next-chunk
                                                       :limit (1- limit))))))
      num-inserted))

 
;; -----------------------------------------------------------------------------
;;  (old-fashioned-undo/lst/get-count LST &KEY CHUNK-P) => NUM
;; -----------------------------------------------------------------------------
(defun old-fashioned-undo/lst/get-count (lst &key chunk-p)
  "Count undo/redo elements in LST.

When non-nil value is set to the argument CHUNK-P,
count chunks in the LST instead of elements."
  (let ((ret-val 0)
        (cur-node lst)
        (done nil))
    (or (eq cur-node nil)
        (while (not done)
          (setq ret-val (1+ ret-val))
          (setq cur-node (old-fashioned-undo/lst/get-next cur-node :chunk-p chunk-p))
          (if (eq cur-node nil) ;; All of chunks are processed.
              (setq done t))))
    ret-val))

 
;; -----------------------------------------------------------------------------
;;  (old-fashioned-undo/lst/get-next LST &KEY CHUNK-P) => LST
;; -----------------------------------------------------------------------------
(defun* old-fashioned-undo/lst/get-next (lst &key chunk-p)
  "Find a sub list which starts with a next undo/redo element from the LST.

When non-nil value is set to the argument CHUNK-P,
find sub list which starts a next undo/redo chunk instead of a element.

Returns nil when no next element or chunk exists."
  (let ((cur-node lst)
        (done nil))
    (while (not done)
      (cond
       ;; Stop scanning at the end of buffer-undo/redo-list.
       ((eq cur-node nil)
        (setq done t))
      
       ;; Stop scanning at the end of buffer-undo/redo-list.
       ((eq (cdr cur-node) nil)
        (setq cur-node nil)
        (setq done t))
       
       ;; Skip an element which INDICATES THAT AN UNMODIFIED
       ;; BUFFER BECAME MODIFIED.
       ((eq (car-safe (car (cdr cur-node))) t)
        (setq cur-node (cdr cur-node)))
       
       ;; Skip the first element when it is a BOUNDARY.
       ((eq (car cur-node) nil)
        (setq cur-node (cdr cur-node)))
       
       ;; Skip an element when it is a PREVIOUS VALUE OF POINT.
       ((integerp (car (cdr cur-node)))
        (setq cur-node (cdr cur-node)))
       
       ;; Skip an element when it is a MARKER and ADJUSTMENT pair.
       ((and (markerp  (car (car (cdr cur-node))))
             (integerp (cdr (car (cdr cur-node)))))
        (setq cur-node (cdr cur-node)))
       
       ;; Stop scanning when the NEXT ELEMENT IS A BOUNDARY.
       ((eq (car (cdr cur-node)) nil)
        (setq cur-node (cdr cur-node))
        (setq done t))

       ;; Other than above.
       (t (setq cur-node (cdr cur-node))
          ;; When the argument `chunk-p' is
          ;; set, skip just one element.
          (when (not chunk-p)
            (setq done t)))))

    cur-node))


 
;;; ===========================================================================
;;;
;;;  Menus
;;;
;;; ===========================================================================

(if (string-match "XEmacs" emacs-version)
    ;; XEmacs
    (progn
      (delete-menu-item '("Edit" "Undo"))
      (delete-menu-item '("Edit" "Redo"))
      (add-menu-button '("Edit")
               ["Undo" undo
            :active buffer-undo-list]
               "Cut")
      (add-menu-button '("Edit")
               ["Redo" redo
            :active (and old-fashioned-undo/buffer-redo-list
                     (eq buffer-undo-list
                     old-fashioned-undo/.last-buffer-undo-list))]
               "Cut"))
  ;; GNU Emacs
  (define-key menu-bar-edit-menu [undo]
  '(menu-item "Undo" undo
          :enable (and (not buffer-read-only)
               buffer-undo-list)
          :help "Undo last operation"))
  
  (define-key-after menu-bar-edit-menu [redo]
    '(menu-item "Redo" redo
        :enable (and (not buffer-read-only)
                 (and old-fashioned-undo/buffer-redo-list
                  (eq buffer-undo-list
                      old-fashioned-undo/.last-buffer-undo-list)))
        :help "Redo last operation") 'undo))

 
;;; ===========================================================================
;;;
;;;  Define Minor Mode.
;;;
;;; ===========================================================================

(easy-mmode-define-minor-mode
 old-fashioned-undo-mode "Minor mode for old fashioned undo/redo."
 :global t
 :init-value nil
 :lighter " OFU"
 (if (or (not (boundp 'old-fashioned-undo-mode))
         old-fashioned-undo-mode)
     (progn
       ;; Override embedded functions. (Evil hack)
       (setf (symbol-function 'undo)
             (symbol-function 'old-fashioned-undo/undo))
       (setf (symbol-function 'redo)
             (symbol-function 'old-fashioned-undo/redo))

       ;; Initialize variables
       (setq old-fashioned-undo/buffer-redo-list nil))
   (progn
     ;; Restore original undo functions.
     (setf (symbol-function 'undo)
           (symbol-function 'old-fashioned-undo/orig-undo))
     (if (fboundp 'old-fashioned-undo/orig-redo)
         (setf (symbol-function 'redo)
               (symbol-function 'old-fashioned-undo/orig-redo))
       (fmakunbound 'redo)))))

(when (not (featurep 'old-fashioned-undo))
  ;; Save original functions.
  (when (not (fboundp 'old-fashioned-undo/orig-undo))
    (setf (symbol-function 'old-fashioned-undo/orig-undo)
            (symbol-function 'undo)))
  (when (and (fboundp 'redo)
             (not (fboundp 'old-fashioned-undo/orig-redo)))
    (setf (symbol-function 'old-fashioned-undo/orig-redo)
            (symbol-function 'redo))))

(provide 'old-fashioned-undo)


;;; old-fashioned-undo.el ends here
