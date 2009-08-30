;;; fsdired.el --- sort files to go to different places in dired

;; Copyright (C) 2006 Joakim Verona

;; Filename: fsdired.el
;; Author: Joakim Verona joakim@verona.se
;; Version: 0.11
;; Keywords: dired, environment, files, renaming

;; fsdired.el is free software

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

; Mark files and move them to different places with the help of dired.
;
; Use it, for instance, if you have a lot random files in a directory,
; and would like to move them to different places, on some criteria
; that you know, but not your computer.  For instance, mark all comic
; book archive files with "1" and all text-files about motherboards
; with "2" and all text-files about dinosaurs with "3".  Noodle about
; with this for a while, reviewing your marks, discovering new types
; of files in your disorganized random dir, and finally press "c-c" to
; pop-up a buffer where you decide where the files go.  When decided,
; press c-c and the files go there!
;
;More precise insructions:
;
; - use "fsdired-change-to-fsdired-mode" to start, in a dired buffer
; - use numeric keys 0-9 to tag files in different groups
; - use C-c [0-9] to review your marks
;   - use g to restore the view
; - use c-c c-c to end.  this will:
;   - switch back the marks buffer to  dired mode
;   - popup a command buffer where you can decide what to do with each group
;     there is a line for each group looking like:
;     "1M /tmp", where 1 corresponds to a mark, M is a command, and /tmp an argument for the command
;     commands(yeah only one for now):
;     M - move
;   - c-i inserts a file name at point, which is convenient with ido mode, for instance

;;;;;;;;;;;;;;;;;;;;;;;;
;Issues: and ideas

; - must the cmd buffer end with newline?


; - if there are no marks for a certain char in  the cmd buffer, default will be the file the cursor is on...
;(dired-get-marked-files)

; - 1st line could indicate which dired buffer the command buffer is associated with by default
; - processing dired commands is kind of the same as "x" for expunge...

;- currently nearly no error checking is done!
;
;  - ought to handle invalid format of the command buffer
;  - ought to handle missing command character definitions
;  - ought to report no matching command line in the command buffer for a mark
;  - some convenience functions to build the command buffer:
;    - command to fill with sane defaults(consider though that we might want to reuse command buffers)
;- hide-lines.el is used when reviewing marks.  would be
;  better to find some std (cvs) Emacs facility
;- dont crash when destination exists
;- prompt for directory creation, if desired destionation doesnt exist
;- dont crash when "." is somewhere, mark or destinarion
;- before commiting command buffer, see to it that lines arent hidden
;- possibly hide/show lines automatically in dired buffer when moving in command buffer
;- the keymap set/restore mechanism seem to discard user customizations, why?

;DONE
; - the rename M is odd when the dir to move to doesnt exist(should ask to mk dir)
;    - facility to enter a directory path conveniently
; - add possibility to add kbd bindings directly to dired kbd map
; - c-1 etc dosn no longer work?

;fsdired has only been tested on CVS Emacs 22.

; thanks to:
;  Andrew M.  Scott

;;; History:
; 0.1  - initial release for review
; 0.11 - Some minor changes on advice from  Andrew M.  Scott
;; 


;;; Code:

;how to hide lines:
(setq fsdired-has-hide-lines (require 'hide-lines nil t) )
;http://www.emacswiki.org/cgi-bin/wiki/HideLines
; - I try to fail gracefully if no hide-lines



;I tried another method which didnt require an external package:
;  dired-do-kill-lines, set dired-marker-regexp
;  revert-bufer to restore
;this didnt work because revert-buffer doesnt restore marks on hidden lines


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the routines to change to/from fsdired mode basically just
;;add/remove a bunch of keybindings that might be global instead if
;;prefered

(defun fsdired-change-to-fsdired-mode ()
"Start organizing files enter from dired."

  (interactive)
  (use-local-map fsdired-mode-map)
)

(defun fsdired-change-to-dired-mode ()
"Quit organizing files."
  (interactive)
  "Change the mode back to dired."
  (use-local-map dired-mode-map)
)

(defun fsdired-add-keys-to-dired()
  "add the fsdired keys to the dired-mode-map.
use either this or the mode swicthing commands"
  (fsdired-add-keys dired-mode-map)
  (define-key dired-mode-map (kbd "C-c C-p") 'fsdired-open-process-marks-commandlist-buffer)
  (define-key dired-mode-map (kbd "C-c C-r") 'fsdired-revert-buffer)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fsdired-mark-char (arg char)
  "Set the mark character to use.
Argument ARG .
Argument CHAR"
  (let ((dired-marker-char char))
    (dired-mark arg)))


(defun fsdired-mark (arg)
  "Read the kbd sequence that triggered this function, and set the mark.
Argument ARG unused, but I think I need P for this-command-keys"
  (interactive "P")
  (let
      ((new-char (string-to-char (this-command-keys))))
    (fsdired-mark-char arg new-char)
    )
  )

 (defun fsdired-view-marked (arg)
   "Show only the files in dired corresponding to a mark.
Argument ARG unused, but I think I need P for this-command-keys"
   (interactive "P")
   (let*
       ((filter-char  (substring (this-command-keys) -1))
        (regexp (concat "^[^" (regexp-quote filter-char) "]")))
     (fsdired-revert-buffer)
     (if  fsdired-has-hide-lines    (hide-matching-lines regexp)   ) ;from hide-lines
     )
   )



(defun fsdired-revert-buffer ()
  "Show all files again."
  (interactive)
  (if  fsdired-has-hide-lines    (show-all-invisible)  ) ;from hide-lines
)

;(defvar fsdired-mode-map nil)
;(unless fsdired-mode-map

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;setup the keys used to mark files

(defun fsdired-add-keys (keymap)
  (define-key keymap "1" 'fsdired-mark)
  (define-key keymap (kbd "C-c 1") 'fsdired-view-marked)
  (define-key keymap "2" 'fsdired-mark)
  (define-key keymap (kbd "C-c 2") 'fsdired-view-marked)
  (define-key keymap "3" 'fsdired-mark)
  (define-key keymap (kbd "C-c 3") 'fsdired-view-marked)
  (define-key keymap "4" 'fsdired-mark)
  (define-key keymap (kbd "C-c 4") 'fsdired-view-marked)
  (define-key keymap "5" 'fsdired-mark)
  (define-key keymap (kbd "C-c 5") 'fsdired-view-marked)
  (define-key keymap "6" 'fsdired-mark)
  (define-key keymap (kbd "C-c 6") 'fsdired-view-marked)
  (define-key keymap "7" 'fsdired-mark)
  (define-key keymap (kbd "C-c 7") 'fsdired-view-marked)
  (define-key keymap "8" 'fsdired-mark)
  (define-key keymap (kbd "C-c 8") 'fsdired-view-marked)
  (define-key keymap "9" 'fsdired-mark)
  (define-key keymap (kbd "C-c 9") 'fsdired-view-marked)
  (define-key keymap "0" 'fsdired-mark)
  (define-key keymap (kbd "C-c 0") 'fsdired-view-marked)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;create the special keymap
(setq fsdired-mode-map (copy-keymap dired-mode-map))
(fsdired-add-keys fsdired-mode-map)
(define-key fsdired-mode-map (kbd "C-c C-c") 'fsdired-open-process-marks-commandlist-buffer)
(define-key fsdired-mode-map "g" 'fsdired-revert-buffer)
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;setup the kbd map used in the speccial command buffer
(setq fsdired-command-buffer-mode-map (make-sparse-keymap))
(define-key fsdired-command-buffer-mode-map (kbd "C-c C-c") 'fsdired-process-marks)
(define-key fsdired-command-buffer-mode-map (kbd "C-c i") 'fsdired-insert-file-name)

(defvar fsdired-last-dired-buffer)

(defun fsdired-open-process-marks-commandlist-buffer ()
  "Start processing marks, from given command argument buffer."
  (interactive)
  (fsdired-change-to-dired-mode) ; switch back to dired mode
  (setq fsdired-last-dired-buffer (current-buffer))
  (switch-to-buffer "*FSDIRED COMMAND BUFFER*")
  (use-local-map fsdired-command-buffer-mode-map)
)

(defun fsdired-process-marks ()
  "Start processing marks, from given command argument buffer."
  (interactive)
  ; parse the command buffer
  ; figure out which dired buffer the marks are in
  ;  maybe fromb buffer local var, or just selecting the buffer
  ; call fsdired-process-marks-commandlist
  
  ; do the parse
  ; currently no error checking
    (save-excursion
      (beginning-of-buffer)
      (setq fsdired-commandlist '())
      
      (let
            ((exit-while nil))
        ; loop the command buffer and build a list from it.
        ; it will look like ((1 "M" "/tmp") ... )
        (while (not exit-while) 
          (let ((beg (point)))
            (forward-line 1)
            (setq exit-while (eobp))
            (let* ((line (buffer-substring-no-properties beg (point)))
                   (mark-char (substring line 0 1))
                   (command-char (substring line 1 2))
                   (argument (substring line 3 -1))
                   )
              (setq  fsdired-commandlist (append fsdired-commandlist (list (list mark-char command-char argument ))))
               )
            )
          )
        )
      (message "commandlist:%s" fsdired-commandlist)
      )
    ;now select the buffer with the marks, and process them
    (save-excursion
      (let ((bf (read-buffer "Buffer where marks are:" fsdired-last-dired-buffer t)))
        (set-buffer bf)
        (fsdired-process-marks-commandlist fsdired-commandlist)
        )
      )
    )

(defun fsdired-markers-exists ()
"does lines matching the current dired marker char exist at all in the dired buffer?
"
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (dired-marker-regexp)))
      (re-search-forward regexp nil t)
      )))

(defun fsdired-process-marks-commandlist (command-list)
  "This does the actual processing of marks.
Argument COMMAND-LIST list of commands."
  (mapcar (lambda (char-command-pair)
            (let* ((dired-marker-char (string-to-char (first char-command-pair)))
                   (files (dired-get-marked-files));now "files" are all the files marked with the current marker chair in the command-list
                   (command-char (second char-command-pair));this is the command char we are going to apply to the files
                   (argument (third char-command-pair))
                   )
              (fset 'mapcommand2 `(lambda (file) ( ,(intern (concat "fsdired-command-" command-char)) file argument) ))
              (message "commandchar:%s command:%s" command-char (symbol-function 'mapcommand2))
              (if (fsdired-markers-exists);his is needed because if no markers exist, default will be current line even withouth marker
                  (mapcar 'mapcommand2 files)
                  )
              )
            )
          command-list)
)

(defun fsdired-insert-file-name ()
  "Ask for a filename and insert at point."
  (interactive)
  (insert (read-file-name "directory-name:"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; defuns matching command chars in the command buffer
; currently only "M" does anything useful
; FILE is the file to do something with
; WHERETO is the argument from the command buffer 
;
; "M" - move
; more commands that might get implemented
; depending on demand and usefullnes
; "C" - copy
; "A" - archive
; "U" - unpack
; "!" - shell command (this would need more advanced argument parsing)

(defun fsdired-command-M (file whereto)
  "Move a FILE.
Corresponds to a M in the command buffer.
Argument WHERETO where to move the file."
(message "mv '%s' '%s'" file whereto)
  (dired-rename-file file whereto 1)
)

(defun fsdired-command-C (file whereto)
  "Dummy copy command.
FILE and WHERETO same as for all command letters."
  (message "dummy cp '%s' '%s'" file whereto)
)

(defun fsdired-command-A (file whereto)
  "Dummy archive command.
FILE and WHERETO same as for all command letters."
  (message "dummy arch '%s' '%s'" file whereto)
)

(defun fsdired-command-U (file whereto)
  "Dummy unpack command.
FILE and WHERETO same as for all command letters."
  (message "dummy unpack '%s' '%s'" file whereto)
)

(defun fsdired-command-! (file whereto)
  "Dummy ! command.
FILE and WHERETO same as for all command letters."
  (message "dummy ! '%s' '%s'" file whereto)
)


(provide 'fsdired)

;;; fsdired.el ends here

