;;; cdb-gud.el --- Grand Unified Debugger mode for running CDB

;; Author: Stephan Doll <stephan_doll at dantz.com>
;; Maintainer: Aaron Brady <abrady0 at yahoo dot com> :
;; Version: 1.0 (January 30, 2002)
;; Version: 1.4 (January 10, 2007) : updated to handle latest cdb
;; Version: 1.5 (January 26, 2011) : helper functions for debugging running processes
;; frames a little better. also added helper functions. see
;; 'cdbDebugChoice' at bottom for example.
;; Version: 1.5 (October 19, 2008) : parse fixes for latest debug tools.

;; This file is NOT part of GNU Emacs but the same permissions apply.
;; This is free software (needed for emacswiki upload.pl)
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides Emacs (GUD) integration for Microsoft's CDB
;; debugger.  (CDB is the text-mode version of WinDbg).  For more
;; details about the Emacs' debugger integration, read "Running
;; Debuggers Under Emacs" in the Emacs manual.
;;
;; To install this package:
;;
;;    - Download and install the latest version of the `Debugging Tools
;;      for Windows' from http://www.microsoft.com/ddk/debugging/.  Add it
;;      to your PATH environment.
;;
;;    - Put the following into your .emacs file:
;;
;;      (load "cdb-gud")
;;
;;    - You can customize `gud-cdb-directories' to help GUD find your source
;;      files.
;;
;;    - If you want key bindings similar to MS' GUI debuggers, add the
;;      following:
;;
;;      (global-set-key [f5]    'gud-cont)
;;      (global-set-key [f7]    'gud-tbreak)
;;      (global-set-key [f8]    'gud-step)
;;      (global-set-key [f9]    'gud-break)
;;      (global-set-key [f10]   'gud-next)
;;      (global-set-key [f11]   'gud-finish)
;;

;;; Here is a simple tutorial:

;; In Emacs, run

;;    	M-x cdb
;;     "Run cdb (like this):" cdb <name of your exe>

;; This will open a new Emacs buffer "*gud-xxx*".  In it you will get a
;; CDB command prompt '0:000> '.  (CDB commands are documented in the
;; 'Debugging tools for Windows' online help).  To get to the begin of
;; your code, type:

;;      'g main' <Enter> (or 'g WinMain' if you have a GUI application).

;; CDB will load the application and break at your main() function.
;; Emacs should open another window with your main() source file and show
;; a little '>' were the debugger stopped.  You now can set more
;; breakpoints in your sources, single-step, etc.  To use the common VC++

;; You can also issue additional commands from the CDB command prompt --
;; e.g.:

;;     - 'dv'  Displays local variables

;;     - 'dt' or '??' shows the content of a single variable.

;; To get the current stack trace, either use the 'k' command or execute
;; "M-x speedbar".  The later will display the calling stack in a
;; additional Emacs frame and you can use the mouse to switch between
;; stack frames.

;; If the little GUD source line marker '>' is hard to follow, add the
;; following to your .emacs:

;; ;;-------------------------------------------------------------
;; ;; Add color to the current GUD line
;; ;;
 (defvar gud-overlay
   (let* ((ov (make-overlay (point-min) (point-min))))
     (overlay-put ov 'face 'secondary-selection)
     ov)
   "Overlay variable for GUD highlighting.")

 (defadvice gud-display-line (after my-gud-highlight act)
   "Highlight current line."
   (let* ((ov gud-overlay)
          (bf (gud-find-file true-file)))
	 (if bf
		 (save-excursion
		   (set-buffer bf)
		   (move-overlay ov (line-beginning-position) (line-end-position) (current-buffer))))))

 (defun gud-kill-buffer ()
   (if (eq major-mode 'gud-mode)
        (delete-overlay gud-overlay)))

 (add-hook 'kill-buffer-hook 'gud-kill-buffer)
;; ;;-------------------------------------------------------------

;; Have fun,
;; -Stephan
    
;;; Code:

(require 'gud)

;;; History of argument lists passed to cdb.
(defvar gud-cdb-history nil)

(defcustom gud-cdb-directories nil
  "*A list of directories that cdb should search for source code.
If nil, only source files in the program directory
will be known to cdb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
                 (repeat :value ("")
                         directory))
  :group 'gud)

(defvar gud-cdb-options-hook nil
  "the default options to use when starting a coh cdb instance")

(defun gud-cdb-massage-args (file args)
;; ab: my hack for remote  (append args (cons "-c" (cons "l+*;l-s" (cons "-lines" nil)))))
  (setq tmp (append (loop for i in gud-cdb-options-hook append (funcall i)) (cons "-c" (cons "l+*;l-s" (cons "-lines" args)))))
;;  (debug)
 tmp)

(defmacro make-gud-cdb-massage-args-remote (remote_addr)
  (append '(lambda (file args) (cons file args))
		  (list (list 'cons "-remote" remote_addr) 
				(cons '(cons "-c" (cons "l+*;l-s" (cons "-lines" args))) nil))))
  ;;(list 'cons "-remote" remote_addr))
;;(funcall (make-gud-cdb-massage-args-remote "123.456.789") "a" "b")

;;(defun gud-cdb-massage-args-remote (file args)
;; ab: my hack for remote  (append args (cons "-c" (cons "l+*;l-s" (cons "-lines" nil)))))
;;  (cons "-remote (cons "-c" (cons "l+*;l-s" (cons "-lines" args)))

(defun gud-cdb-file-name (f)
  "Transform a relative file name to an absolute file name, for cdb."
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((directories gud-cdb-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
                (setq result (expand-file-name path)
                      directories nil)))
          (setq directories (cdr directories)))))
    result))

(defvar gud-marker-acc "")
(make-variable-buffer-local 'gud-marker-acc)

(defvar gud-output-acc "" 
  "accumulates all input between input markers 0:000>")
(make-variable-buffer-local 'gud-output-acc)

;; paths are hard-coded, it may be useful to remap
;; a path to another, e.g. the visual studio std library
;; is apparently compiled in f:/RTM/..., but I have it 
;; installed under program files.
(defvar gud-cdb-remap-fname-hooks nil
  "Hook to be run by gud-cdb-remap-fname when a source file is looked up")

(defun gud-cdb-remap-fname (fname)
  (cond
   ((not fname) nil)
   ;;   ((file-exists-p fname) fname)
   (t ;; try to look the file up
    (let (;;(file-name-directory fname)
          ;;(file-name-nondirectory fname)
          (full-filename (expand-file-name fname))
          )
	  (or 
	   (loop for i in gud-cdb-remap-fname-hooks
			 if (and (setq full-filename (funcall i fname)) (file-exists-p full-filename)) return full-filename)
	   fname) 
	  )
	)
   )
  )

;; ab: where output is handled, parsed, and turned into source file
;; gud-display-line : the 'find-file' of gud, calls gud-display-line which calls display-buffer
(defun gud-cdb-marker-filter (string)
  (setq gud-marker-acc (concat gud-marker-acc string))
  (let* ((output "")
		(input-cursor-found)
		(lines)
		(find-stack-marker
			  (lambda (stack-lines)
				(if (string-match 
					 (concat 
					  "^[0-9a-zA-Z]*" ;; optional frame number
					  " *[0-9a-zA-Z]+" ;; instruction pointer
					  " [0-9a-zA-Z]+ " ;; return address
					  ".+!.+\\"	;; module!function
					  ;;"+0x[0-9a-zA-Z]+" ;; offset (optional, ab: removing)
					  " \\[\\(.*\\) @ \\([0-9]+\\)\\]" ;; file @ line
					  ) stack-lines)
					(let
						((fname)
						 (linenum))
					  (setq fname (substring stack-lines (match-beginning 1) (match-end 1)))
					  (setq linenum (string-to-number (substring stack-lines (match-beginning 2) (match-end 2))))
					  (setq fname (gud-cdb-remap-fname fname))
					  (if (and fname (file-exists-p fname))
						  (cons fname linenum)
						(funcall find-stack-marker (substring stack-lines (match-end 0))))))))
		(set-from-stack-marker (lambda (stack-lines)
								 (setq gud-last-frame (or 
													   (funcall find-stack-marker stack-lines)
													   gud-last-frame))))
        (set-gud-last-frame (lambda (fname line)
                              "change new mark for buffer if file found"
                              (let ((fn (gud-cdb-remap-fname fname)))
                                (if fn
                                    (setq gud-last-frame (cons fn line)))))))

;;    (message "in")

	;; *********************************************************************************
	;; output capture  
	;; *********************************************************************************

	;; accum latest
	(setq lines (split-string string "\n"))
	(loop for line in lines
		  until (setq input-cursor-found (string-match "^[0-9]:[0-9][0-9][0-9]\\(:.*\\)?>" line))
		  do
		  (setq gud-output-acc (concat gud-output-acc line "\n")))
	;; ab: match frame markers.
;; example:
;;"0b 0012f8ac 00516f22 Auctionserver!XactLogLine_FromStr+0xd9 [c:\\src\\xactlog.c @ 372]
;	(debug)
	(if input-cursor-found 
		(cond
         ;;; info about a particular line, e.g. from a step
         ((string-match "^\\([a-zA-Z]:.*\\)(\\([0-9]+\\))" gud-output-acc)
          (funcall set-gud-last-frame (match-string 1 gud-output-acc) (string-to-number (match-string 2 gud-output-acc))))
         ;;; default, scrape for a stack trace
         (t (funcall set-from-stack-marker gud-output-acc))))
		  
;; 		 (string-match 
;; 		 (concat 
;; 		  "^[0-9a-zA-Z]*" ;; optional frame number
;; 		  " *[0-9a-zA-Z]+";; instruction pointer
;; 		  " [0-9a-zA-Z]+ ";; return address
;; 		  ".+!.+\\"		  ;; module!function
;; 		  ;;"+0x[0-9a-zA-Z]+" ;; offset (optional, ab: removing)
;; 		  " \\[\\(.*\\) @ \\([0-9]+\\)\\]" ;; file @ line
;; 		  ) gud-output-acc))
;; 		(let
;; 			((fname (match-string 1 gud-output-acc))
;; 			 (linenum (string-to-number (match-string 2 gud-output-acc))))
;; 		  (if (file-exists-p fname)
;; 			  (setq gud-last-frame (cons fname linenum)))))

	;; clear the accumulator
	(if input-cursor-found
		(setq gud-output-acc ""))

;; Process all the complete markers in this chunk.  This regex might catch
;;     ;; too much, but that is the debugger's fault ...

;; 	;; ab: .frame n parsing. only 1 line of output.
;; 	(if (string-match "^[0-9a-f]+ .*\n.*>" string) 
;; 		(funcall set-from-stack-marker string))
	
;; 	;; ab: add 'k' stack parsing
;; 	(if (string-match "^ChildEBP.*RetAddr[ ]*\n" string)
;; 		(progn
;; 		  (funcall set-from-stack-marker (substring string (match-end 0)))))
		  
;;     (while (string-match "^\\([-A-Za-z_\.:\\]*\\)(\\([0-9]*\\))\n" gud-marker-acc)

;;    (message "before-munch")
    ;; too munch, but that is the debugger's fault ...
    (while (string-match 
			"^\\([-A-Za-z0-9_\.:\\]*\\)(\\([0-9]*\\))\n" 
			gud-marker-acc)
      (setq

       ;; Extract the frame position from the marker.
;;        gud-last-frame
;;        (cons (substring gud-marker-acc (match-beginning 1) (match-end 1))
;;              (string-to-number (substring gud-marker-acc
;;                                        (match-beginning 2)
;;                                        (match-end 2))))

       ;; Append any text before the marker to the output we're going
       ;; to return - we don't include the marker in this text.
       output (concat output
                      (substring gud-marker-acc 0 (match-beginning 0)))

       ;; Set the accumulator to the remaining text.
       gud-marker-acc (substring gud-marker-acc (match-end 0))))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-marker-acc)
        (progn
        ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring gud-marker-acc
                                                 0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (setq gud-marker-acc
                (substring gud-marker-acc (match-beginning 0))))
      (setq output (concat output gud-marker-acc)
            gud-marker-acc ""))
;;     (message "last frame: %s" gud-last-frame)
;;     (message "marker-acc: %s" gud-marker-acc)
;;     (message "output: %i" (length output))
;;     (message "%s" output)
;;     (setq output "Breakpoint 0 hit
;; GameClient!SwitchToGameplayState:
;; 015a74e0 55              push    ebp
;; 0:000> 
;; ")
;;     (setq gud-last-frame '("c:\\src\\crossroads\\gameclientlib\\gclloading.c" . 58))
    ;;(sleep-for 5)
    output))

(defun gud-cdb-find-file (f)
  (save-excursion
    (let ((realf (gud-cdb-file-name f)))
	  (if (file-exists-p (or realf f))
		  (if realf
			  (find-file-noselect realf t)
			(find-file-noselect f 'nowarn)
			)
        ))))

(defun cdb-simple-send (proc string)
  (comint-send-string proc (concat string "\n")) ;; this first: error writing to buf otherwise
  (if (string-match "^[ \t]*[Qq][ \t]*" string)
      (kill-buffer gud-comint-buffer)
	))

(defun cdb (command-line)
  "Run cdb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'cdb)))

  (gud-common-init command-line 'gud-cdb-massage-args
                   'gud-cdb-marker-filter 'gud-cdb-find-file)

  (set (make-local-variable 'gud-minor-mode) 'cdb)

  (gud-def gud-break  "bu `%d%f:%l` " "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "g `%d%f:%l` "  "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-step   "t "            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "p "            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "g "            "\C-r" "Continue with display.")
  (gud-def gud-finish "g @$ra "       "\C-f" "Finish executing current function.")
  (gud-def gud-print  "?? %e "        "\C-p" "Evaluate C expression at point.")

  (setq comint-prompt-regexp "^[0-9a-f]:[0-9a-f][0-9a-f][0-9a-f]> ")
  (setq comint-input-sender 'cdb-simple-send)
  (setq paragraph-start comint-prompt-regexp)

  (run-hooks 'cdb-mode-hook))

;; cdb speedbar functions

(defun gud-cdb-goto-stackframe (text token indent)
  "Goto the stackframe described by TEXT, TOKEN, and INDENT."
  (speedbar-with-attached-buffer
   (gud-display-line (nth 2 token) (string-to-number (nth 3 token)))
   (gud-basic-call (concat ".frame " (nth 1 token)))))

(defvar gud-cdb-complete-in-progress)

(defvar gud-cdb-fetched-stack-frame nil
  "Stack frames we are fetching from CDB.")

(defvar gud-cdb-fetched-stack-frame-list nil
  "List of stack frames we are fetching from CDB.")

(defun gud-cdb-get-stackframe (buffer)
  "Extract the current stack frame out of the GUD CDB BUFFER."
  (let ((newlst nil)
        (gud-cdb-fetched-stack-frame-list nil))
    (gud-cdb-run-command-fetch-lines "kn " buffer)
    (if (and (car gud-cdb-fetched-stack-frame-list)
             (string-match "No stack" (car gud-cdb-fetched-stack-frame-list)))
        ;; Go into some other mode???
        nil
      (while gud-cdb-fetched-stack-frame-list
        (let ((e (car gud-cdb-fetched-stack-frame-list))
              (name nil) (num nil))
          (if (not (string-match "^\\([0-9a-f]+\\) [0-9a-f]* [0-9a-f]* \\([[a-zA-Z_0-9:$~!+]*\\).*$" e))
              nil
            (setq num (match-string 1 e)
                  name (match-string 2 e))
            (setq newlst
                  (cons
                   (if (string-match
                        "\\([-0-9a-zA-Z\\_.:]+\\) @ \\([0-9]+\\)" e)
                       (list name num (match-string 1 e)
                             (match-string 2 e))
                     (list name num))
                   newlst))))
        (setq gud-cdb-fetched-stack-frame-list
              (cdr gud-cdb-fetched-stack-frame-list)))
      (nreverse newlst))))

(defun gud-cdb-run-command-fetch-lines (command buffer)
  "Run COMMAND, and return when `gud-cdb-fetched-stack-frame-list' is full.
BUFFER is the GUD buffer in which to run the command."
  (save-excursion
    (set-buffer buffer)
    (if (save-excursion
          (goto-char (point-max))
          (forward-line 0)
          (not (looking-at comint-prompt-regexp)))
        nil
  ;; Much of this copied from CDB complete, but I'm grabbing the stack
      ;; frame instead.
      (let ((gud-marker-filter 'gud-cdb-speedbar-stack-filter))
        ;; Issue the command to CDB.
        (gud-basic-call command)
        (setq gud-cdb-complete-in-progress t)
        ;; Slurp the output.
        (while gud-cdb-complete-in-progress
          (accept-process-output (get-buffer-process gud-comint-buffer) 15))
        (setq gud-cdb-fetched-stack-frame nil
              gud-cdb-fetched-stack-frame-list
              (nreverse gud-cdb-fetched-stack-frame-list))))))

(defun gud-cdb-speedbar-stack-filter (string)
  ;; checkdoc-params: (string)
  "Filter used to read in the current CDB stack."
  (setq string (concat gud-cdb-fetched-stack-frame string))
  (while (string-match "\n" string)
    (setq gud-cdb-fetched-stack-frame-list
          (cons (substring string 0 (match-beginning 0))
                gud-cdb-fetched-stack-frame-list))
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
        (setq gud-cdb-complete-in-progress nil)
        string)))

(defun gud-speedbar-buttons (buffer)
  "Create a speedbar display based on the current state of GUD.
If the GUD BUFFER is not running a supported debugger, then turn
off the specialized speedbar mode."
  (if (and (save-excursion (goto-char (point-min))
                           (looking-at "XXX")) ; *SD* Always update ...
           (equal gud-last-last-frame gud-last-speedbar-stackframe))
      nil
    (setq gud-last-speedbar-buffer buffer)
    (let* ((ff (save-excursion (set-buffer buffer) gud-find-file))
       ;;(lf (save-excursion (set-buffer buffer) gud-last-last-frame))
           (frames
            (cond ((eq ff 'gud-gdb-find-file)
                   (gud-gdb-get-stackframe buffer)
                   )
                  ;; *SD* ++
                  ((eq ff 'gud-cdb-find-file)
                   (gud-cdb-get-stackframe buffer)
                   )
                  ;; *SD* --
                  ;; Add more debuggers here!
                  (t
                   (speedbar-remove-localized-speedbar-support buffer)
                   nil))))
      (erase-buffer)
      (if (not frames)
          (insert "No Stack frames\n")
        (insert "Current Stack:\n"))
      (while frames
        (insert (nth 1 (car frames)) ":\n")
        (if (= (length (car frames)) 2)
            (progn
              (speedbar-insert-button (car (car frames))
                                      'speedbar-directory-face
                                      nil nil nil t))
          (speedbar-insert-button (car (car frames))
                                  'speedbar-file-face
                                  'speedbar-highlight-face
                                  (cond ((eq ff 'gud-gdb-find-file)
                                         'gud-gdb-goto-stackframe)
                                        ((eq ff 'gud-cdb-find-file)
                                         'gud-cdb-goto-stackframe)
                                        (t (error "Should never be here")))
                                  (car frames) t))
        (setq frames (cdr frames)))
      )
    (setq gud-last-speedbar-stackframe gud-last-last-frame)))

;; *********************************************************************************
;; cdb helpers  
;; *********************************************************************************

(defun cdb-pidFromExe (&optional exe-name-regexp predicate)
  (interactive)
  (cdr (cdb-promptPidAndExe exe-name-regexp predicate)))

(defun cdb-promptPidAndExe (&optional exe-name-regexp predicate)
  (interactive)
  (let
	  ((tlist)
	   (exe)
	   (exes))
	(setq tlist (shell-command-to-string "tlist"))
	(setq exes
		  (loop for n from 0 for i in (reverse (split-string tlist "\n"))
				collect 
				(progn
				  (if (string-match "\\(^[ 0-9]+ \\)\\(.*\\)" i)
					  (cons (match-string 2 i) (match-string 1 i))))))
	(if exe-name-regexp 
		(setq exe (loop for i in exes if (string-match exe-name-regexp (car i)) return (car i)))
		(setq exe (completing-read "in: " ;; prompt 
							   exes ;; table
							   predicate ;; predicat (setq predicate (lambda (c) (string-match "mapserver.exe" (car c))))
							   nil ;; require match
							   nil ;; initial input
							   nil ;; hist
							   nil ;; def
							   nil ;; inherit input method
							   )))
	(cons exe (string-to-number (cdr (assoc exe exes))))))

(defun cdbAttach (exe-pid)
  (interactive (funcall (lambda () (list (cdb-pidFromExe)))))
	(if exe-pid
		(progn
		  (gud-call (format ".attach 0n%i; " exe-pid)))))

(defun cdbDebugChoice (&optional predicate)
  "if you want to debug a program running on windows, call this function and it will give you the list of running processes and
   allow you to attach to one of them."
  (interactive)
  (let* ((exepidpair)
		 (pid))
	(setq exepidpair (cdb-promptPidAndExe nil predicate))
	(setq pid (cdr exepidpair))
	(cdb (format "cdb -p %i" pid))
	(rename-buffer (format "*gud-%s*" (car (string-split "\\s-+" (car exepidpair) 1))) t)))

(defun cdbSetIP ()
  "set instruction pointer to current point. if you are in source code and want to change the current line to mark"
  (interactive)
  (gud-call (format "r eip = %s" (cdbLineNoKill))))
(defalias 'eip 'cdbSetIP)

(defun cdbLineNoKill ()
  "get current line in cdb format"
  (concat "`" (buffer-file-name) ":" (number-to-string (count-lines (point-min) (1+ (point)))) "`"))

(defun cdbLine ()
  "kill current line in cdb format"
  (interactive)
  (message (kill-new (cdbLineNoKill))))
(defalias 'xl 'cdbLine)

;; ----------------------------------------
;; auto-complete support

(defvar cdb-ac-match-limit t
  "limit for the number of matches to be collected in the cdb buffer or the src buffer for the current frame")

(defun cdb-ac-candidate-words-in-buffer (prefix)
  "cdb wants the words from the buffer for the current frame. the `ac-candidate-words-in-buffer' doesn't support this, but this version takes explicit params to do this"
  (let ((i 0)
        candidate
        candidates
        (regexp (concat "\\_<" (regexp-quote prefix) "\\(\\sw\\|\\s_\\)+\\_>")))
    (save-excursion
	  (goto-char 0)
      ;; Search forward
      (while (and (or (eq cdb-ac-match-limit t)
                      (< i limit))
                  (re-search-forward regexp nil t))
        (setq candidate (match-string-no-properties 0))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      (nreverse candidates))))

(defun cdb-ac-candidates ()
  "list of potentially matching auto-complete words"
;;  (debug)
  (cond
   ((looking-back "\\(0x\\)[0-9]+") nil)
   (t
	(append
	 (setq foo (ac-candidate-words-in-buffer))
	 (if (and gud-last-last-frame (car gud-last-last-frame) (find-buffer-visiting (car gud-last-last-frame)))
		 (with-current-buffer (find-buffer-visiting (car gud-last-last-frame))
		   (cdb-ac-candidate-words-in-buffer ac-prefix)
		   )
	   )
	 ))
   )
  )

(defvar cdb-ac-sources 
  '((candidates . cdb-ac-candidates) 
	(requires . 3)))

(defun cdb-ac-mode-init ()
  (interactive)
  "set up the auto complete variables for cdb"
  (auto-complete-mode t)
;;  (ac-define-dictionary-source
;;   ac-source-cdb-keywords
;;   ("g" "k" "kn" "p")) ;; not really necessary with the 3 character requirement
  (setq ac-sources '(cdb-ac-sources
					 ;; ac-source-cdb-keywords
					 )))

(if (require 'auto-complete nil t)
	(add-hook 'cdb-mode-hook 'cdb-ac-mode-init))

;;; cdb-gud.el ends here
