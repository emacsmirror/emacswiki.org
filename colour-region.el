;;; colour-region.el --- Toggle regions of the buffer with different text snippets

;; Filename: colour-region.el
;; Description: Toggle regions of the buffer with different text snippets
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: Sometime in 2008 (can't remember when exactly)
;; Version: 0.4
;; Last-Updated: 2013-05-04 21:37:20
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/colour-region
;; Keywords: 
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; * Commentary
;; Bitcoin donations gratefully accepted: 1Aa6Hh39RCBvr6BrxDWvThYynuKcwv6NL4
;; 
;; This library is based on [[http://www.emacswiki.org/emacs/hide-region.el][hide-region]].
;; You can define areas of the buffer as "colour-regions" which can be toggled between various states,
;; e.g. for hiding or highlighting the text. Each colour-region can also contain several different texts which can be 
;; toggled between. 
;; I'm not sure how useful this is in practice, but it was fun to build.
;; 
;; * Creating colour-regions of different types
;; You can create a new colour-region by selecting an area of text in the buffer, and executing any of the following 
;; commands:
;; 
;; - colour-region-new
;; - colour-region-store-text
;; - colour-region-toggle-overlay
;; - colour-region-toggle-text
;; 
;; You can choose what type the new colour-region should have by using an appropriate prefix key,
;; e.g. using a prefix of C-3 will create a type 3 colour-region (which is green by default).
;; The colour and formatting for a particular type of colour-region can be customized using the `colour-region-formats' option. 
;; * Prefix keys for commands
;; For many of the commands you can choose which regions to apply them to by using the appropriate prefix key:
;; 
;;  - No prefix key means apply the command to the colour-region nearest point.
;;  - A prefix of 0 (i.e. pressing C-0 before entering the command) means apply the command to all colour-regions
;;    in the current buffer.
;;  - A positive non-zero prefix of say N means apply the command to all colour-regions of the Nth type
;;    (that were created using the same prefix).
;; * Navigation
;; You can quickly move between colour regions using the following commands:
;; 
;; - colour-region-next 
;; - colour-region-previous 
;; 
;; Both of these commands take prefix arg to specify which colour-regions to consider when moving.
;; See the commands documentation for details.
;; * Copying/pasting/removing colour-reginos
;; Killed/copied colour-regions are stored in `colour-region-kill-ring'.
;; The following commands can be used for copying and pasting colour-regions.
;; 
;; - colour-region-remove
;; - colour-region-copy
;; - colour-region-kill
;; - colour-region-kill-ring-rotate
;; - colour-region-yank
;; - colour-region-yank-pop
;; 
;; The remove/copy/kill commands accept the previously mentioned prefix keys to specify with regions they apply to.
;; See the commands documentation for details.
;; * Altering colour-regions
;; The following commands can be used to alter the content and display format of colour-regions:
;; 
;; - colour-region-toggle-overlay
;; - colour-region-toggle-text
;; - colour-region-change-comment
;; - colour-region-change-type
;; - colour-region-store-text
;; - colour-region-func
;; 
;; All of these commands accept the previously mentioned prefix keys for specifying which regions they apply to.
;; See the commands documentation for details.
;; * Saving colour-regions
;; These commands can be used for loading and saving colour-regions.
;; 
;; - colour-region-load 
;; - colour-region-save
;;  
;; * Customizable options
;; The following options can be customized:
;; 
;;  - colour-region-formats : List of text-properties to apply to the different states of each region type.
;;  - colour-region-save-on-kill : Whether or not to save colour-regions when buffer is killed.
;;  - colour-region-load-on-find-file : If set to t then always load colour-regions when a new file is opened.
;;  - colour-region-kill-ring-max : The maximum number of elements allowed on the colour-region-kill-ring before old ones are removed.
;; 
;; See the doc strings for more details.
;; * Hooks
;; The following hook functions are used to load/save colour regions automatically. 
;; They are added to the relevant hooks when colour-region-initialize is run.
;; 
;;  - colour-region-find-file-hook 
;;  - colour-region-kill-emacs-hook 
;;  - colour-region-kill-buffer-hook
;; 
;; * Installation
;; 
;;  - If you have [[http://www.marmalade-repo.org/][marmalade-repo.org]], this colour-region is part of the emacs packages you can install.  
;;    Just type M-x package-install colour-region marmalade 
;;  - Otherwise download the library from here Lisp:colour-region.el, and put it in a directory in the emacs load path, 
;;    like ~/.emacs.d
;;  - Add the following lines to your ~/.emacs file:
;; 
;;      (require 'colour-region)
;;      (colour-region-initialize)
;; 
;;;;


;;; Installation:
;;
;; Put colour-region.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'colour-region)
;; (colour-region-initialize)
;;



;;; Customize:
;;
;; `colour-region-formats' : List of text-properties to apply to the different states of each region type.
;; `colour-region-save-on-kill' : If set to t then save colour-regions when buffer is killed.
;; `colour-region-load-on-find-file' : If set to t then always load colour-regions when a new file is opened.
;; `colour-region-kill-ring-max' : The maximum number of elements allowed on the colour-region-kill-ring before old ones are 
;;                                 removed.


;;
;; All of the above can customized by:
;;      M-x customize-group RET colour-region RET
;;

;;; Change log:
;; 11-May-2013      
;;    Last-Updated: 2013-05-04 21:37:20 (Joe Bloggs)
;;    Add bitcoin address for donations
;; 7-May-2013      
;;    Last-Updated: 2013-05-04 21:37:20 (Joe Bloggs)
;;    Try org-readme-sync
;;	
;; 2013/05/04
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; This code is heavily based on hide-region by Mathias Dahl, available here: http://www.emacswiki.org/emacs/hide-region.el
;;

;;; TODO
;;
;; 

;;; Require


;;; Code:


(defgroup colour-region nil
  "Functions to define associated regions within buffer, 
and apply different overlays to them."
  :prefix "colour-region-"
  :group 'convenience)

(defcustom colour-region-formats
  '((("" nil
      (:background "blue" :foreground "black") nil ""
      (:background "blue" :foreground "black")
      (face . (:foreground "blue")))
     ("@[1:" t
      (:background "blue" :foreground "black") nil "]@"
      (:background "blue" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[1:" nil
      (:background "blue" :foreground "black") nil "]@"
      (:background "blue" :foreground "black")
      (face . (:background "blue"))))
    (("" nil
      (:background "red" :foreground "black") nil ""
      (:background "red" :foreground "black")
      (face . (:foreground "red")))
     ("@[2:" t
      (:background "red" :foreground "black") nil "]@"
      (:background "red" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[2:" nil
      (:background "red" :foreground "black") nil "]@"
      (:background "red" :foreground "black")
      (face . (:background "red"))))
    (("" nil
      (:background "green" :foreground "black") nil ""
      (:background "green" :foreground "black")
      (face . (:foreground "green")))
     ("@[3:" t
      (:background "green" :foreground "black") nil "]@"
      (:background "green" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[3:" nil
      (:background "green" :foreground "black") nil "]@"
      (:background "green" :foreground "black")
      (face . (:background "green"))))
    (("" nil
      (:background "yellow" :foreground "black") nil ""
      (:background "yellow" :foreground "black")
      (face . (:foreground "yellow")))
     ("@[4:" t
      (:background "yellow" :foreground "black") nil "]@"
      (:background "yellow" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[4:" nil
      (:background "yellow" :foreground "black") nil "]@"
      (:background "yellow" :foreground "black")
      (face . (:background "yellow"))))
    (("" nil
      (:background "cyan" :foreground "black") nil ""
      (:background "cyan" :foreground "black")
      (face . (:foreground "cyan")))
     ("@[5:" t
      (:background "cyan" :foreground "black") nil "]@"
      (:background "cyan" :foreground "black")
      (invisible . t)
      (intangible . t))	 
     ("@[5:" nil
      (:background "cyan" :foreground "black") nil "]@"
      (:background "cyan" :foreground "black")
      (face . (:background "cyan"))))
    (("" nil
      (:background "pink" :foreground "black") nil ""
      (:background "pink" :foreground "black")
      (face . (:foreground "pink")))
     ("@[6:" t
      (:background "pink" :foreground "black") nil "]@"
      (:background "pink" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[6:" nil
      (:background "pink" :foreground "black") nil "]@"
      (:background "pink" :foreground "black")
      (face . (:background "pink")))))
  "List of text-properties to apply to the different states of each region type.
Each element is a list whose elements correspond to the different states of the associated region type.
The elements corresponding to different states are lists containing the following elements:

The 'before-string property for the overlay
A value indicating whether the region text should be added to the before-string
A list of face properties for the before-string
A value indicating whether the region text should be added to the after-string
The 'after-string property for the overlay
A list of face properties for the after-string
All further elements in the list are cons cells of (property . value) pairs to be applied to the overlay.
"
  :type '(repeat sexp)
  :group 'colour-region)

(defcustom colour-region-save-on-kill t
  "If set to t then save colour-regions when buffer is killed.
If set to prompt then prompt to save. If nil then don't save.
This is a buffer local variable."
  :group 'colour-region
  :type '(choice (const :tag "Always save" t)
		 (const :tag "Prompt" prompt)
		 (const :tag "Never save" nil)))

(make-variable-buffer-local 'colour-region-save-on-kill)

(defcustom colour-region-load-on-find-file t
  "If set to t then always load colour-regions when a new file is opened.
If set to prompt then prompt to load. If nil then don't load."
  :group 'colour-region
  :type '(choice (const :tag "Always load" t)
		 (const :tag "Prompt" prompt)
		 (const :tag "Never load" nil)))

(make-variable-buffer-local 'colour-region-load-on-find-file)

(defun colour-region-create-new-type nil
  "Create new colour-region format list and append to colour-region-formats."
  (let* ((newtypenum (1+ (length colour-region-formats)))
	 (newtypestring (number-to-string newtypenum))
	 (newhiddenformat
          (list (concat "@[" newtypestring ":")
                t '(:background ? :foreground "black")
                nil "]@"
                '(:background ? :foreground "black")
                '(invisible . t)
                '(intangible . t)))
	 (newunhiddenformat (list (concat "@[" newtypestring ":")
                                  t '(:background ? :foreground "black")
                                  nil "]@"
                                  '(:background ? :foreground "black")
                                  '(face . (:background "cyan"))))
	 (newformatlist (list newhiddenformat newunhiddenformat)))
    (setcar (nthcdr (1- newtypenum) colour-region-formats) (cons newformatlist nil))))

(defvar colour-regions nil
  "List of colour-regions.
Each element is a list containing the following elements (in order):
buffer name
start position
end position 
comment 
current type
current state
index of current content in following list
list of content lists, each of which contains the following elements:
     state associated with content
     comment for content
     text string to display
an overlay corresponding to the colour-region")

(make-variable-buffer-local 'colour-regions)

(defcustom colour-region-kill-ring-max 30
  "The maximum number of elements allowed on the colour-region-kill-ring before old ones are removed."
  :type 'integer
  :group 'colour-region)

(defvar colour-region-kill-ring (make-ring colour-region-kill-ring-max)
  "Variable to store killed/copied regions (that may be yanked with `colour-region-yank').")

(defvar colour-region-kill-ring-index 0
  "The index of the current element on the `colour-region-kill-ring'.
This points to the most recent element unless the ring has been rotated.")

(defun colour-region-apply-according-to-prefix (func1)
  "Apply function FUNC1 to colour region(s) according to current prefix arg.

With no prefix argument apply to nearest colour-region.
With non-zero prefix argument apply to all colour-regions of type corresponding to argument.
With prefix argument of zero apply to all colour-regions in current buffer."
  (if current-prefix-arg
      ;; if prefix argument is 0, change all colour-regions in current buffer
      (if (equal current-prefix-arg 0)
          (dolist (cregion colour-regions)
            (if (equal (buffer-name) (car cregion))
                (apply func1 (list cregion)))) 
        ;; else if prefix argument is other number, change corresponding colour-regions
        (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
            (dolist (cregion colour-regions)
              (if (and (equal (buffer-name) (car cregion)) 
                       (equal (nth 4 cregion) (1- current-prefix-arg)))
                  (apply func1 (list cregion))))))
    ;; otherwise (e.g. no prefix argument) just change the one closest to point
    (let ((bestindex (colour-region-find-nearest (lambda (hideregion) t))))
      (if bestindex (let ((cregion (nth bestindex colour-regions)))
                      (apply func1 (list cregion)))
        ;; else give message to user
        (message "No colour-regions found in current buffer!")))))

;;;###autoload
(defun colour-region-new (comment)
  "Create a new colour-region for selected region (if no region is selected inform user):
1) Prompt user for COMMENT for colour-region. 
2) If a positive prefix argument is given set colour-region type to that corresponding with prefix argument.
   Otherwise use type 1 colour-region.
3) Set state of colour-region to 1.
4) Add colour-region to colour-regions variable.
5) Apply overlay with format in colour-region-formats corresponding to state and type of colour-region.

Actually internal type and state values start from 0 not 1, 
but since I use prefix argument of 0 to mean all buffers, I use 1 to indicate initial type."
  (interactive 
   ;; prompt for comment string for colour region
   (list 
    (if mark-active 
	(read-string "Comment string (default is first line of region): " nil nil 
		     ;; set default comment string to first line of region
		     (buffer-substring-no-properties (region-beginning) 
						     (let ((oldpoint (point)) lineend) 
						       (goto-char (region-beginning)) 
						       (setq lineend (line-end-position)) 
						       (goto-char oldpoint) 
						       (min lineend (region-end))))))))
  (if mark-active
      ;; make new colour-region
      (let ((colourregion (list (buffer-name) (region-beginning) (region-end) comment
                                ;; use prefix argument-1 as colour-region type, or 0 if none given
                                (if (and current-prefix-arg 
                                         (<= current-prefix-arg (length colour-region-formats)) 
                                         (> current-prefix-arg 0))
                                    (1- current-prefix-arg) 0) 0 0
                                (list 
                                 (list 0 comment 
                                       (buffer-substring-no-properties 
                                        (region-beginning) (region-end))))
                                (make-overlay (region-beginning) (region-end)))))
	;; apply colour-region
	(colour-region-apply-overlay colourregion)
	;; append new colour-region to colour-regions
	(setq colour-regions (if (equal colour-regions (list nil))
				 (list colourregion)
			       (append colour-regions (list colourregion))))
	;; deactivate the mark
	(deactivate-mark))
    (message "No region selected!")))

;;;###autoload
(defun colour-region-store-text nil
  "If region is selected then run colour-region-new function.
Otherwise save currently displayed text, comment and state of colour-region(s).
Insert the new text-part into the next position in the text-parts list of this colour-region(s). 

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if (and mark-active (not (equal (point) (mark))))
      ;; if a region has been selected then just call colour-region-new
      (call-interactively 'colour-region-new)
    (colour-region-apply-according-to-prefix
     (lambda (cregion)
       (colour-region-apply-save-text-part cregion 1)
       (message "New text region saved")))))

;;;###autoload
(defun colour-region-toggle-overlay nil
  "If region is selected then run colour-region-new function.
Otherwise toggle overlay state of colour-region(s): 
       if colour-region(s) is in final state, set it to state 0, 
       otherwise set it to next state.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if (and mark-active (not (equal (point) (mark))))
      ;; if a region has been selected then just call colour-region-new
      (call-interactively 'colour-region-new)
    (colour-region-apply-according-to-prefix
     (lambda (cregion)
       (colour-region-apply-toggle-overlay cregion)))))

;;;###autoload
(defun colour-region-toggle-text nil
  "If region is selected then run colour-region-new function.
Otherwise save current text in current text-region of colour-region(s),
and toggle to next text-state: 
       if colour-region(s) is in final state, set it to state 0, 
       otherwise set it to next state.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if (and mark-active (not (equal (point) (mark))))
      ;; if a region has been selected then just call colour-region-new
      (call-interactively 'colour-region-new)
    (colour-region-apply-according-to-prefix
     (lambda (cregion)
       (colour-region-apply-toggle-text-part cregion)))))

;;;###autoload
(defun colour-region-remove nil
  "Remove colour-region(s), and delete from colour-regions.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (colour-region-apply-remove cregion))))

;; Make sure func2 arg has different name to func1 arg in colour-region-apply-according-to-prefix function
;;;###autoload
(defun colour-region-func (func2)
  "Apply a user-supplied elisp function to colour-region(s).
The function (func) should take two arguments: the start and end positions of a region.
If applied to several colour-regions (i.e. when a prefix argument is used) func is applied 
to colour-regions one at a time in the order in which they appear in the current buffer.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive (list (read-minibuffer "Function to apply: ")))
  ;; remove all overlays from current buffer
  ;; (they will be reapplied later
  (dolist (cregion colour-regions)
    (if (equal (nth 0 cregion) (buffer-name))
	(remove-overlays (nth 1 cregion) (nth 2 cregion))))
  ;; sort colour-regions by end position
  ;; to make adjusting overlays easier later
  (setq colour-regions
	(sort colour-regions
	      (lambda (overlayA overlayB) 
		(< (nth 2 overlayA) (nth 2 overlayB)))))
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (let* ((oldpoint (point))
            (start (nth 1 cregion))
            (end (nth 2 cregion))
            (comment (nth 3 cregion))
            (index (position cregion colour-regions))
            newpoint change)
       (goto-char end)
       ;; call function on current overlay 
       (condition-case err
           (funcall func2 start end)
         (error (message "Error applying function to colour-region with comment: %s" comment)))
       ;; work out how much buffer positions have changed
       (setq newpoint (point))
       (setq change (- newpoint end))
       (goto-char oldpoint)
       ;; adjust overlays positions in colour-regions appropriately
       (dotimes (j (- (length colour-regions) index))
         (setq cregionB (nth (+ index j) colour-regions))
         (if (equal (nth 0 cregionB) (buffer-name))
             (setcar (nthcdr (+ index j) colour-regions)
                     (list (nth 0 cregionB)
                           (if (equal j 0)
                               (nth 1 cregionB)
                             (+ (nth 1 cregionB) change))
                           (+ (nth 2 cregionB) change)
                           (nth 3 cregionB)
                           (nth 4 cregionB)
                           (nth 5 cregionB))))))))
  ;; re-apply overlays to all colour-regions
  (dolist (cregion colour-regions)
    (if (equal (buffer-name) (car cregion))
	(colour-region-apply-overlay cregion))))

;;;###autoload
(defun colour-region-next nil
  "Move point to next colour-region in current buffer.

If no prefix argument is given, move to next colour-region in current buffer.
If a prefix argument of 0 is given, move to first colour-region in current buffer.
If a positive non-zero prefix argument is given, move to next colour-region with type 
corresponding to that prefix argument."
  (interactive)
  (let (predicate1 predicate2 (best nil))
    (if current-prefix-arg
	(if (equal current-prefix-arg 0)
	    (setq predicate1 
		  (lambda (cregion) t)
		  predicate2 
		  (lambda (cregion) (> (nth 1 cregion) best)))
 	  (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	      (setq predicate1 
		    (lambda (cregion) (equal (nth 4 cregion) (1- current-prefix-arg)))
		    predicate2
		    (lambda (cregion) (<= (nth 1 current) best)))))
      (setq predicate1 
	    (lambda (cregion) t)
	    predicate2 
	    (lambda (cregion) (<= (nth 1 current) best))))
    (dolist (current colour-regions)
      (if (and (equal (nth 0 current) (buffer-name)) 
               (> (nth 1 current) (point)) 
               (funcall predicate1 current))
          (if (not best)
              (setq best (nth 1 current))
            (if (funcall predicate2 current)
                (setq best (nth 1 current))))))
    (if best (goto-char best)
      (message "No further colour-regions found in current buffer!"))))

;;;###autoload
(defun colour-region-previous nil
  "Move point to previous colour-region in current buffer.

If no prefix argument is given, move to previous colour-region in current buffer.
If a prefix argument of 0 is given, move to last colour-region in current buffer.
If a positive non-zero prefix argument is given, move to previous colour-region with type 
corresponding to that prefix argument."
  (interactive)
  (let (predicate1 predicate2 (best nil))
    (if current-prefix-arg
	(if (equal current-prefix-arg 0)
	    (setq predicate1 
		  (lambda (cregion) t)
		  predicate2 
		  (lambda (cregion) (< (nth 1 cregion) best)))
 	  (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	      (setq predicate1 
		    (lambda (cregion) (equal (nth 4 cregion) (1- current-prefix-arg)))
		    predicate2
		    (lambda (cregion) (>= (nth 1 current) best)))))
      (setq predicate1 
	    (lambda (cregion) t)
	    predicate2 
	    (lambda (cregion) (>= (nth 1 current) best))))
    (dolist (current colour-regions)
      (if (and (equal (nth 0 current) (buffer-name)) 
               (< (nth 1 current) (point)) 
               (funcall predicate1 current))
          (if (not best)
              (setq best (nth 1 current))
            (if (funcall predicate2 current)
                (setq best (nth 1 current))))))
    (if best (goto-char best)
      (message "No further colour-regions found in current buffer!"))))

;;;###autoload
(defun colour-region-copy nil
  "Copy colour-region to `colour-region-kill-ring'.
With no prefix argument copy nearest colour-region.
With non-zero prefix argument copy all colour-regions of type corresponding to argument.
With prefix argument of zero copy all colour-regions in current buffer."
  (interactive)
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (ring-insert colour-region-kill-ring
                  (colour-region-apply-copy cregion)))))

;;;###autoload
(defun colour-region-kill nil
  "Kill colour-region and hidden text to colour-region-kill-ring.
With no prefix argument kill nearest colour-region.
With non-zero prefix argument kill all colour-regions of type corresponding to argument.
With prefix argument of zero kill all colour-regions in current buffer."
  (interactive)
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (colour-region-apply-kill cregion))))

;;;###autoload
(defun colour-region-change-comment (comment)
  "Change the COMMENT of a colour-region(s).

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive
   (list (read-string "Comment string (default is first line of region): " nil nil 
                      ;; set default comment string to first line of region
                      (buffer-substring-no-properties (region-beginning) 
                                                      (let ((oldpoint (point)) lineend) 
                                                        (goto-char (region-beginning)) 
                                                        (setq lineend (line-end-position)) 
                                                        (goto-char oldpoint) 
                                                        (min lineend (region-end)))))))
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (setcar (nthcdr 3 current) comment)
     (colour-region-apply-overlay current))))

;;;###autoload
(defun colour-region-change-type (type)
  "Change the type of a colour-region(s). 
Prompts for a new TYPE number, if the number entered is larger than the number of 
currently available types then a new type is created (with value one higher than the 
previous highest type number) and the colour-region is set to that type.
If the number entered is invalid or less than 1, then the type is not changed.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive (list (read-number (format  "New type (an integer > 1) : "))))
  ;; if number entered is higher than highest type available, create new type and use it
  (if (> type (length colour-region-formats))
      (progn
	(setq type (length colour-region-formats))
	(colour-region-create-new-type))
    (setq type (1- type)))
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (if (> type -1)
         (progn
           (setcar (nthcdr 4 cregion) type)
           (colour-region-apply-overlay cregion))))))

(defun colour-region-find-nearest (predicate)
  "Find the index in colour-regions of the colour-region that is nearest to point
in the current buffer, and that returns non-nil when passed to 'predicate' function.
Returns nil if no colour-region satisfying 'predicate' is found in current buffer.

'predicate' must be a function that takes a colour-region as it's only argument."
  ;; if no prefix argument was given, then toggle nearest colour-region
  (let (current nearestoverlaypos (bestindex nil))
    ;; first loop through colour-regions to find nearest overlay
    (dotimes (i (length colour-regions))
      (setq current (nth i colour-regions))
      ;; only consider overlays on current buffer that satisfy predicate
      (if (and (equal (buffer-name) (car current)) (funcall predicate current))
	  ;; if this is the first time we find an overlay matching this buffer
	  ;; then it is the nearest overlay found so far, so set variables appropriately
	  (if (not bestindex)
	      (setq nearestoverlaypos (nth 1 current) bestindex i) 
	    ;; else if start position of overlay is closer to point 
	    ;; than nearestoverlaypos is...
	    (if (< (abs (- (nth 1 current) (point)))
                   (abs (- nearestoverlaypos (point))))
		(setq nearestoverlaypos (nth 1 current) bestindex i)
	      ;; otherwise, if end position of overlay is closer to point
	      ;; than nearestoverlaypos..
	      (if (< (abs (- (nth 2 current) (point))) (abs (- nearestoverlaypos (point))))
		  (setq nearestoverlaypos (nth 2 current) bestindex i))))))
    bestindex))

(defun* colour-region-in-colour-region-p (&optional (pos (point)))
  "Return non-nil if buffer position POS lies within a colour-region.
By default POS is set to the current cursor position."
  (colour-region-find-nearest
   (lambda (cregion)
     (and (> pos (nth 1 cregion))
          (< pos (nth 2 cregion))))))

(defun colour-region-apply-overlay (cregion)
  "Apply appropriate overlay properties (according to colour-region-formats) to CREGION"
  ;; get colourregion properties
  (let ((storedoverlay (nth (1- (length cregion)) cregion)))
    (unless (not (overlayp storedoverlay))
      (let* ((start (overlay-start storedoverlay))
             (end (overlay-end storedoverlay))
             (comment (nth 3 cregion))
             (regiontype (nth 4 cregion))
             (formattype (nth 5 cregion))
             (formatlist (nth formattype (nth regiontype colour-region-formats)))
             (beforestring (concat (nth 0 formatlist)
                                   (if (nth 1 formatlist)
                                       comment "")))
             (beforestringformat (nth 2 formatlist))
             (afterstring (concat (if (nth 3 formatlist)
                                      comment "")
                                  (nth 4 formatlist)))
             (afterstringformat (nth 5 formatlist)))
        ;; make new overlay and apply appropriate properties
        ;; (but only if old overlay is valid)
        (unless (or (not start) (not end))
          (remove-overlays start end)
          (let ((newoverlay (make-overlay start end)))
            (overlay-put newoverlay 'before-string
                         (propertize beforestring 'face beforestringformat))
            (overlay-put newoverlay 'after-string
                         (propertize afterstring 'face afterstringformat))
            (dotimes (i (- (length formatlist) 6))
              (let ((currentproperty (nth (+ i 6) formatlist)))
                (overlay-put newoverlay (car currentproperty) (cdr currentproperty))))
            ;; update overlay stored in colourregion
            (setcar (nthcdr (1- (length cregion)) cregion) newoverlay)))))))

(defun colour-region-apply-toggle-overlay (cregion)
  "Toggle overlay state of CREGION"
  (let ((currentstate (nth 5 cregion))
	(textpart (nth (nth 6 cregion) (nth 7 cregion))))
    ;; if current state of cregion is last state, change to first state,
    ;; otherwise change to next state
    (if (equal currentstate (1- (length (nth (nth 4 cregion) colour-region-formats))))
	(progn
	  (setcar (nthcdr 5 cregion) 0)
	  (setcar textpart 0))
      (progn
	(setcar (nthcdr 5 cregion) (1+ currentstate))
	(setcar textpart (1+ currentstate))))
    ;; apply overlay for new state
    (colour-region-apply-overlay cregion)))

(defun colour-region-apply-toggle-text-part (cregion)
  "Toggle text-part of CREGION"
  ;; only toggle if there is more than one stored text region
  (if (> (length (nth 7 cregion)) 1)
      (let* ((oldtextnum (nth 6 cregion))
	     ;; if current text is last one, change to first one
	     ;; otherwise change to next one
	     (newtextnum (if (equal oldtextnum (1- (length (nth 7 cregion))))
			     (setq newtextnum 0)
			   (setq newtextnum (1+ oldtextnum))))
	     (newtextpart (nth newtextnum (nth 7 cregion)))
	     (newtext (nth 2 newtextpart))
	     (storedoverlay (nth (1- (length cregion)) cregion))
	     (start (overlay-start storedoverlay))
	     (end (overlay-end storedoverlay))
	     (newend (+ start (length newtext)))
	     (diff (- newend end))
	     (currentpoint (point))
	     (newpoint (if (< currentpoint end) currentpoint (+ currentpoint diff))))
	;; store current text-part
	(colour-region-apply-save-text-part cregion 0)
	;; set state, comment and text-part number to correspond to new text-part
	(setcar (nthcdr 5 cregion) (nth 0 newtextpart))
	(setcar (nthcdr 3 cregion) (nth 1 newtextpart))
	(setcar (nthcdr 6 cregion) newtextnum)
	;; delete old text and copy new text into buffer
	(delete-region start end)
	(goto-char start)
	(insert newtext)
	;; move overlay to fit new text
	(move-overlay storedoverlay start newend)
	;; apply overlay and move point back to correct position
	(colour-region-apply-overlay cregion)
	(goto-char newpoint))))

(defun colour-region-apply-save-text-part (cregion pos)
  "Save currently displayed text, comment and state of CREGION, as new text-part in
text-parts list of cregion.
If POS is 0, copy over text-part corresponding to current display, 
else place it pos positions further/behind current text-part in list according
to whether pos is positive/negative.
New position is calculated modulo the length of the new list so that large POS values
don't cause problems."
  (let* ((comment (nth 3 cregion))
	 (state (nth 5 cregion))
	 (storedoverlay (nth (1- (length cregion)) cregion))
	 (start (overlay-start storedoverlay))
	 (end (overlay-end storedoverlay))
	 (text (buffer-substring-no-properties start end))
	 (newtextpart (list state comment text))
	 (textparts (nth 7 cregion))
	 (oldpos (nth 6 cregion))
	 ;; will insert newtextpart just before newpos, so set accordingly:
	 (newpos (if (> pos 0)
		     (mod (+ oldpos pos) (1+ (length textparts)))
		   (mod (+ oldpos pos 1) (1+ (length textparts))))))
    ;; if pos is 0, just replace current textpart (in position oldpos)
    (if (equal pos 0)
	(setcar (nthcdr oldpos textparts) newtextpart)
      ;; else insert newtextpart into textparts, just before newpos
      ;; i.e. change the cdr of the position just before newpos to a list with
      ;; newtextpart at the beginning followed by the rest of textparts 
      ;; (which is the newpos'th cdr or nil if we are at the end of the list)
      (if (> newpos 0)
	  (setcdr (nthcdr (1- newpos) textparts) (cons newtextpart (nthcdr newpos textparts)))
	(setcdr (nthcdr (1- (length textparts)) textparts) (cons newtextpart nil))))
    ;; set index of current text-part to correspond with newtextpart
    (setcar (nthcdr 6 cregion) newpos)))

(defun colour-region-apply-remove (cregion)
  "Remove CREGION from buffer and colour-regions."
  (let ((storedoverlay (nth (1- (length cregion)) cregion)))
    (remove-overlays (overlay-start storedoverlay)
		     (overlay-end storedoverlay))
    (setq colour-regions (delq cregion colour-regions))))

(defun colour-region-apply-kill (cregion)
  "Kill CREGION (including hidden text) from buffer and colour-regions,
and place on colour-region-kill-ring."
  (colour-region-apply-remove cregion)
  (ring-insert colour-region-kill-ring cregion)
  (delete-region (nth 1 cregion) (nth 2 cregion)))

(defun colour-region-apply-copy (cregion)
  "Returns a copy of colour-region CREGION (with a new overlay)."
  (let* ((newcregion (copy-tree cregion))
         (oldoverlay (car (last cregion))))
    (setf (car (last newcregion))
          (copy-overlay oldoverlay))
    newcregion))

;;;###autoload
(defun colour-region-kill-ring-rotate nil
  "Rotate the `colour-region-kill-ring-index' so that it points to the next item in the ring."
  (interactive)
  (let ((len (cadr colour-region-kill-ring)))
    (setq colour-region-kill-ring-index
          (mod (+ 1 colour-region-kill-ring-index) len))))

(defun colour-region-insert (cregion)
  "Insert the colour-region CREGION into the buffer at point.
You should make sure to check that you are not already in a colour-region
using the `colour-region-in-colour-region-p' function."
  (let* ((index (nth 6 cregion))
         (texts (nth 7 cregion))
         (text (nth 2 (nth index texts)))
         (overlay (car (last cregion)))
         (pos (point)))
    (insert text)
    ;; Change the start/end information
    (setf (nth 1 cregion) pos
          (nth 2 cregion) (point))
    ;; Move the overlay
    (move-overlay overlay pos (point))
    ;; Apply overlay and move point back to correct position
    (colour-region-apply-overlay cregion)
    (goto-char pos)))

;;;###autoload
(defun colour-region-yank nil
  "Yank the most recent kill in the `colour-region-kill-ring' into the buffer at point."
  (interactive)
  (if (colour-region-in-colour-region-p)
      (error "You are already in a colour-region, wont yank into it")
    (let ((newcregion (colour-region-apply-copy
                       (ring-ref colour-region-kill-ring colour-region-kill-ring-index))))
      (colour-region-insert newcregion)
      (setq colour-regions (append colour-regions (list newcregion))))))

;;;###autoload
(defun colour-region-yank-pop nil
  "Rotate the `colour-region-kill-ring' and yank the next kill into the buffer at point."
  (interactive)
  (if (colour-region-in-colour-region-p)
      (error "You are already in a colour-region, wont yank into it")
    (colour-region-kill-ring-rotate)
    (colour-region-yank)))

(defun colour-region-apply-update-start-end (cregion)
  "Update the start and end information in CREGION to correspond with the overlay."
  (let ((storedoverlay (nth (1- (length cregion)) cregion)))
    (if (overlayp storedoverlay)
	(progn
	  (setcar (nthcdr 1 cregion)
                  (overlay-start storedoverlay))
	  (setcar (nthcdr 2 cregion)
                  (overlay-end storedoverlay))))))

(defun colour-region-apply-update-overlay (cregion)
  "Create and apply a new overlay for CREGION based on 
start, end, type and status values."
  (let* ((start (nth 1 cregion))
	 (end (nth 2 cregion))
	 (newoverlay (make-overlay start end)))
    (setcar (nthcdr (1- (length cregion)) cregion) newoverlay)
    (colour-region-apply-overlay cregion)))

(defun colour-region-update-start-end nil
  "Update the start and end information in all colour regions"
  (dolist (current colour-regions)
    (colour-region-apply-update-start-end current)))

(defun colour-region-update-overlays nil
  "Create and update overlays in colour-regions according to
start, end, type and status values"
  (dolist (current colour-regions)
    (colour-region-apply-update-overlay current)))

(defun colour-region-kill-emacs-hook nil
  "Hook for saving colour-regions when emacs is killed.
Note: colour-regions will not be usable after running this function until it
is restored with `colour-region-update-overlays'.

This is added to `kill-emacs-hook' when `colour-region-initialize' is run."
  ;; need to adjust colour-regions for each buffer, since it is a buffer local variable
  (dolist (thisbuffer (buffer-list)) 
    (with-current-buffer thisbuffer
      (if (and (buffer-file-name) 
	       colour-region-save-on-kill 
	       (> (length colour-regions) 0))
	  (if (eq colour-region-save-on-kill 'prompt)
	      (if (y-or-n-p (concat "Save colour-regions for " (buffer-name) " ? "))
		  (colour-region-save))
	    (colour-region-save))))))

(defun colour-region-kill-buffer-hook nil
  "Save colour-regions if buffer is killed, and `colour-region-save-on-kill' is t.
Prompt for save if `colour-region-save-on-kill' equals 'prompt.

This is added to `kill-buffer-hook' when `colour-region-initialize' is run."
  (if (and colour-region-save-on-kill (> (length colour-regions) 0))
      (if (eq colour-region-save-on-kill 'prompt)
	  (if (y-or-n-p 
	       (concat "Save colour-regions for " 
		       (buffer-name) " ? "))
	      (colour-region-save))
	(colour-region-save))))

(defun colour-region-find-file-hook nil
  "If `colour-region-load-on-find-file' is t, load colour-regions from filename returned by
`colour-region-default-save-file' function.If `colour-region-load-on-find-file' is equal to 'prompt, then prompt the user first.
If `colour-region-load-on-find-file' is nil, or the filename returned by `colour-region-default-save-file' doesn't exist,
then don't load.

This is added to `find-file-hook' when `colour-region-initialize' is run."
  (let ((filename (colour-region-default-save-file)))
    (if (and colour-region-load-on-find-file
	     (file-exists-p filename))
	(if (eq colour-region-load-on-find-file 'prompt)
	    (if (y-or-n-p 
		 (concat "Load colour-regions from "
			 filename " ? "))
		(colour-region-load))
	  (colour-region-load)))))

;;; Initialization function
(defun colour-region-initialize nil
  "Initialize colour-region; setup hooks."
  (add-hook 'find-file-hook 'colour-region-find-file-hook t)
  (add-hook 'kill-buffer-hook 'colour-region-kill-buffer-hook t)
  (add-hook 'kill-emacs-hook 'colour-region-kill-emacs-hook))

;;; function to save all the colour-regions in current buffer
;; (there are probably better ways of doing this, but it'll do for now)
;;;###autoload
(defun colour-region-save (&optional filename)
  "Save colour-regions for the current buffer (if there are any) from FILENAME.
If FILENAME is not provided then the colour-regions are read from the filename returned by 
the colour-region-default-save-file function."
  (interactive)
  (let ((filename2 (or filename
		       (colour-region-default-save-file))))
    (progn
      ;; update start and end information for all colour-regions 
      (colour-region-update-start-end)
      ;; remove overlays from colour-regions so that we can save it with session
      (dolist (current colour-regions)
	(let ((storedoverlay (nth (1- (length current)) current)))
	  (if (overlayp storedoverlay) 
	      (setcar (nthcdr (1- (length current)) current) t))))
      ;; write colour-regions to file (with message explaining contents of file)
      (write-region 
       (concat ";; This file contains the contents of the colour-regions variable associated with the buffer " (buffer-name) ".\n;; It is used by the emacs library colour-region.el in the functions colour-region-save and colour-region-load\n\n(setq colour-regions '" (prin1-to-string colour-regions) ")")
       nil
       filename2)
      ;; restore overlays to colour-regions
      (colour-region-update-overlays))))

;;;###autoload
(defun colour-region-load (&optional filename)
  "Load colour-regions for the current buffer from FILENAME.
If FILENAME is not provided then the colour-regions are stored in the filename returned by 
the colour-region-default-save-file function."
  (interactive)
  (let ((string)
	(savedfile (if filename
		       (if (file-name-directory filename) filename
			 (concat default-directory filename))
		     (colour-region-default-save-file))))
    (if (not (file-regular-p savedfile))
	(message (concat "Can't read" savedfile " !"))
      (with-temp-buffer
	(goto-char (point-min))
	(insert-file-contents savedfile)
	(setq string (buffer-string)))
      (eval (read string))
      (colour-region-update-overlays))))

(defun colour-region-default-save-file nil
  "Returns the default filename for the current buffer for saving colour-regions"
  (concat default-directory ".colour-regions_for_" (buffer-name)))

(provide 'colour-region)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "colour-region.el" (buffer-name) (buffer-string) "update")

;;; colour-region.el ends here




