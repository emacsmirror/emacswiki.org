;;; ascope.el --- Another cscope interface for emacs
;; Wrtitten by Staton Sun
;; my email address is sunnycamel@gmail.com
;;
;;
;; comment:
;; Because xcscope.el open a new process of "cscope -L" for each a
;; definition, it's wasting time for reloading cscope and reopening
;; database.
;;Another cscope interface called bscope can get the search result quick but it is weak in the presentation of search result. 
;;This script merge the xcscope.el and bscope.el together make it fast and versatile

;; Usage:
;;load this script using (require 'ascope.el) in you .emacs
;; M-x ascope-init load the cscope database. This command must be issue prior to issue any other command below, the directory feed to this command must be the directory include the cscope.out file
;; M-x ascope-find-global-definition
;; M-x ascope-find-this-symbol
;; M-x ascope-find-this-text-string
;; M-x ascope-find-functions-calling-this-function
;; M-x ascope-find-called-functions
;; M-x ascope-find-files-including-file
;; M-x ascope-all-symbol-assignments
;; M-x ascope-clear-overlay-arrow
;; M-x ascope-pop-mark


;;run next commands in the search result buffer (*Result)
;;ascope-next-symbol this command is bind to key "n"
;;ascope-prev-symbol this command is bind to key "p"
;;ascope-select-entry-other-window-delete-window this command is bind to key "enter"


(defgroup ascope nil
"Cscope interface for (X)Emacs.
Using cscope, you can easily search for where symbols are used and defined.
It is designed to answer questions like:

Where is this variable used?
What is the value of this preprocessor symbol?
Where is this function in the source files?
What functions call this function?
What functions are called by this function?
Where does the message \"out of space\" come from?
Where is this source file in the directory structure?
What files include this header file?
"
:prefix "ascope-"
:group 'tools)

(defcustom ascope-allow-arrow-overlays t
"*If non-nil, use an arrow overlay to show target lines.
Arrow overlays are only used when the following functions are used:

ascope-show-entry-other-window
ascope-show-next-entry-other-window
ascope-show-prev-entry-other-window

The arrow overlay is removed when other cscope functions are used.
Note that the arrow overlay is not an actual part of the text, and can
be removed by quitting the cscope buffer."
:type 'boolean
:group 'ascope)

(defcustom ascope-overlay-arrow-string "=>"
"*The overlay string to use when displaying arrow overlays."
:type 'string
:group 'ascope)

(defcustom ascope-name-line-width -30
"*The width of the combined \"function name:line number\" field in the
cscope results buffer. If negative, the field is left-justified."
:type 'integer
:group 'ascope)

(defcustom ascope-use-face t
"*Whether to use text highlighting (? la font-lock) or not."
:group 'ascope
:type '(boolean))

(defface ascope-file-face
'((((class color) (background dark))
(:foreground "yellow"))
(((class color) (background light))
(:foreground "blue"))
(t (:bold t)))
"Face used to highlight file name in the *ascope* buffer."
:group 'cscope)

(defface ascope-function-face
'((((class color) (background dark))
(:foreground "cyan"))
(((class color) (background light))
(:foreground "magenta"))
(t (:bold t)))
"Face used to highlight function name in the *ascope* buffer."
:group 'ascope)


(defface ascope-line-number-face
'((((class color) (background dark))
(:foreground "red"))
(((class color) (background light))
(:foreground "red"))
(t (:bold t)))
"Face used to highlight line number in the *ascope* buffer."
:group 'ascope)


(defface ascope-line-face
'((((class color) (background dark))
(:foreground "green"))
(((class color) (background light))
(:foreground "black"))
(t (:bold nil)))
"Face used to highlight the rest of line in the *ascope* buffer."
:group 'ascope)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst ascope-separator-line
"-------------------------------------------------------------------------------\n"
"Line of text to use as a visual separator.
Must end with a newline.")
(defvar ascope-first-match nil
"The first match result output by cscope.")
(make-variable-frame-local 'ascope-first-match-point)

(defvar ascope-first-match-point nil
"Buffer location of the first match.")
(make-variable-frame-local 'ascope-first-match-point)

(defvar ascope-action-message nil "The message about what action is taken")
(make-variable-frame-local 'ascope-action-message)

(defvar ascope-last-file nil
"The file referenced by the last line of cscope process output.")
(make-variable-frame-local 'ascope-last-file)

(defvar ascope-list-entry-keymap nil
"The keymap used in the *Result* buffer which lists search results.")
(if ascope-list-entry-keymap
nil
(setq ascope-list-entry-keymap (make-keymap))
(suppress-keymap ascope-list-entry-keymap)
(define-key ascope-list-entry-keymap "n" 'ascope-next-symbol)
(define-key ascope-list-entry-keymap "p" 'ascope-prev-symbol)
(define-key ascope-list-entry-keymap " " 'ascope-show-entry-other-window)
(define-key ascope-list-entry-keymap (kbd "RET") 'ascope-select-entry-other-window-delete-window)
)


(defvar ascope-list-entry-hook nil
"*Hook run after ascope-list-entry-mode entered.")

(defvar ascope-marker nil
"The location from which cscope was invoked.")

(defvar ascope-output-buffer-name "*Result*"
"The name of the cscope output buffer.")

(defvar ascope-marker-ring-length 30 )

(defvar ascope-marker-ring (make-ring ascope-marker-ring-length))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ascope-init (dir)
  (interactive "DCscope Initial Directory: ")
  (if (get-process "ascope") (kill-process (get-process "ascope")))
  (if (get-buffer "*ascope*") (kill-buffer (get-buffer "*ascope*")))
  (setq default-directory dir)
  (start-process "ascope" "*ascope*" "cscope" "-ld" "-f" "cscope.out")
  (set-process-filter (get-process "ascope") 'ascope-filter)
  (with-current-buffer "*ascope*"
    (accept-process-output (get-process "ascope") 3)
    (if (looking-at ".*cannot open.*cscope\.out.*")
	(progn
	  (setq buf (get-buffer "*ascope*"))
	  (if buf
	      (kill-buffer buf))
	  (message "ascope: no cscope.out file here"))
      (progn
	(ascope-wait-for-output)
	(message "ascope: load ok"))
      ))
  )

(defun ascope-find-this-symbol (symbol)
"Locate a symbol in source code."
(interactive (ascope-interactive "Find this symbol: "))
(setq query-command (concat "0" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Find this symbol: %s" symbol))
(ascope-query query-command)
)

(defun ascope-find-global-definition (symbol)
"Find a symbol's global definition."
(interactive (ascope-interactive "Find this global definition: "))
(setq query-command (concat "1" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Finding global definition: %s" symbol))
(ascope-query query-command)
)

(defun ascope-find-called-functions (symbol)
"Display functions called by a function."
(interactive (ascope-interactive "Find functions called by this function: "))
(setq query-command (concat "2" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Find functions called by this function: %s" symbol))
(ascope-query query-command)
)

(defun ascope-find-functions-calling-this-function (symbol)
"Display functions calling a function."
(interactive (ascope-interactive "Find functions calling this function: "))
(setq query-command (concat "3" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Find functions calling this function: %s" symbol))
(ascope-query query-command)
)

(defun ascope-find-this-text-string (symbol)
"Locate where a text string occurs."
(interactive (ascope-interactive "Find this text string: "))
(setq query-command (concat "4" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Find this text string: %s" symbol))
(ascope-query query-command)
)

(defun ascope-find-files-including-file (symbol)
"Locate all files #including a file."
(interactive (ascope-interactive "Find files #including this file: "))
(setq query-command (concat "8" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Find files #including this file: %s" symbol))
(ascope-query query-command)
)

(defun ascope-all-symbol-assignments (symbol)
"Find all the assignments of the symbol"
(interactive (ascope-interactive "this don't work due to the bug of cscope, Find all assignments of symbol: "))
(setq query-command (concat "10" symbol "\n") )
(ring-insert ascope-marker-ring (point-marker))
(setq ascope-action-message (format "Find all assignments of symbol %s" symbol))
(ascope-query query-command)
)

(defun ascope-pop-mark()
"Pop back to where cscope was last invoked."
(interactive)
(if (ring-empty-p ascope-marker-ring)
(error "There are no marked buffers in the ascope-marker-ring yet"))
(let* ( (marker (ring-remove ascope-marker-ring 0))
(old-buffer (current-buffer))
(marker-buffer (marker-buffer marker))
marker-window
(marker-point (marker-position marker))
(ascope-buffer (get-buffer ascope-output-buffer-name)) )


(setq ascope-marker marker)

(if marker-buffer
(if (eq old-buffer ascope-buffer)
(progn ;; In the *cscope* buffer.
(set-buffer marker-buffer)
(setq marker-window (display-buffer marker-buffer))
(set-window-point marker-window marker-point)
(select-window marker-window))
(switch-to-buffer marker-buffer))
(error "The marked buffer has been deleted"))
(goto-char marker-point)
(set-buffer old-buffer)))


(defun ascope-clear-overlay-arrow ()
"Clean up teh overlay arrow."
(interactive)
(let ()
(if overlay-arrow-position
(set-marker overlay-arrow-position nil))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ascope-show-entry-other-window ()
"Display the entry at point in other window.
Point is not saved on mark ring."
(interactive)
(let ((file (get-text-property (point) 'ascope-file))
(line-number (get-text-property (point) 'ascope-line-number)))
(ascope-show-entry-internal file line-number nil nil t))
)

(defun ascope-select-entry-other-window-delete-window ()
"Display the entry at point in other window.
Point is not saved on mark ring, at late kill the result window"
(interactive)
(ascope-show-entry-other-window)
(if overlay-arrow-position
(set-marker overlay-arrow-position nil))
(setq overlay-arrow-position nil
overlay-arrow-string nil)
(setq buf (get-buffer ascope-output-buffer-name))
(delete-window (get-buffer-window buf))
)

(defun ascope-next-symbol ()
"Move to the next symbol in the *ascope* buffer."
(interactive)
(ascope-buffer-search t t))

(defun ascope-prev-symbol ()
"Move to the previous symbol in the *ascope* buffer."
(interactive)
(ascope-buffer-search t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ascope-show-entry-internal (file line-number
&optional save-mark-p window arrow-p)
(let (buffer old-pos old-point new-point forward-point backward-point
line-end line-length)
(if (and (stringp file)
(integerp line-number))
(progn
(unless (file-readable-p file)
(error "%s is not readable or exists" file))
(setq buffer (find-file-noselect file))
(if (windowp window)
(set-window-buffer window buffer)
(setq window (display-buffer buffer)))
(set-buffer buffer)
(setq default-directory (file-name-directory file))
(if (> line-number 0)
(progn
(setq old-pos (point))
(goto-line line-number)
(setq old-point (point))
(setq new-point old-point)
(set-window-point window new-point)
(if (and ascope-allow-arrow-overlays arrow-p)
(set-marker overlay-arrow-position (point))
(set-marker overlay-arrow-position nil))
(if ascope-marker
(progn ;; The search was successful. Save the marker so it
;; can be returned to by ascope-pop-mark.
(ring-insert ascope-marker-ring ascope-marker)
(setq ascope-marker nil)))
)

(message "No entry found at point."))
))))


(defun ascope-buffer-search (do-symbol do-next)
"The body of the following four functions."
(let* (line-number old-point point
(search-file (not do-symbol))
(search-prev (not do-next))
(direction (if do-next 1 -1))
(old-buffer (current-buffer))
(old-buffer-window (get-buffer-window old-buffer))
(buffer (get-buffer ascope-output-buffer-name))
(buffer-window (get-buffer-window (or buffer (error "The *ascope* buffer does not exist yet"))))
)
(set-buffer buffer)
(setq old-point (point))
(forward-line direction)
(setq point (point))
(setq line-number (get-text-property point 'ascope-line-number))
(while (or (not line-number) (and do-symbol (= line-number -1)))
(forward-line direction)
(setq point (point))
(if (or (and do-next (>= point (point-max)))
(and search-prev (<= point (point-min))))
(progn
(goto-char old-point)
(error "The %s of the *cscope* buffer has been reached"
(if do-next "end" "beginning"))))

(setq line-number (get-text-property point 'ascope-line-number)))

(if (eq old-buffer buffer) ;; In the *bscope* buffer.
(ascope-show-entry-other-window))

(if (windowp buffer-window)
(set-window-point buffer-window point))
(set-buffer old-buffer)
))

(defun ascope-list-entry-mode ()
(use-local-map ascope-list-entry-keymap)
(setq buffer-read-only t
mode-name "ascope"
major-mode 'ascope-list-entry-mode
overlay-arrow-string ascope-overlay-arrow-string
)
(or overlay-arrow-position
(setq overlay-arrow-position (make-marker)))
(run-hooks 'ascope-list-entry-hook)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ascope-canonicalize-directory (dir)
(or dir
(setq dir default-directory))
(setq dir (file-name-as-directory
(expand-file-name (substitute-in-file-name dir))))
dir
)


(defun ascope-filter (process string)
;; Write the output into the Tramp Process
(with-current-buffer (process-buffer process)
(save-excursion
(goto-char (point-max))
(insert string)
))
)

(defun ascope-query (command)
(let ((proc (get-process "ascope")) outbuf
)
(with-current-buffer (process-buffer proc)

(goto-char (point-max))
(insert command)

(process-send-string "ascope" command)

(ascope-wait-for-output )

(ascope-process-output)
)

(setq outbuf (get-buffer-create ascope-output-buffer-name))
(with-current-buffer outbuf
(progn
(pop-to-buffer outbuf)
(shrink-window 5)
(insert ascope-separator-line "\n")
(insert "Search complete.")
(if ascope-first-match
(set-window-point (get-buffer-window outbuf) ascope-first-match-point)
(insert "\nNothing found!"))
(ascope-list-entry-mode)
)
))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ascope-interactive (prompt)
(list
(let (sym)
(setq sym (current-word))
(read-string
(if sym
(format "%s (default %s): "
(substring prompt 0 (string-match "[ :]+\\'" prompt))
sym)
prompt)
nil nil sym)
))
)

(defun ascope-make-entry-line (func-name line-number line)
;; The format of entry line:
;; func-name[line-number]______line
;; <- cscope-name-line-width ->
;; `format' of Emacs doesn't have "*s" spec.
(let* ((fmt (format "%%%ds %%s" ascope-name-line-width))
(str (format fmt (format "%s[%s]" func-name line-number) line))
beg end)
(if ascope-use-face
(progn
(setq end (length func-name))
(put-text-property 0 end 'face 'ascope-function-face str)
(setq beg (1+ end)
end (+ beg (length line-number)))
(put-text-property beg end 'face 'ascope-line-number-face str)
(setq end (length str)
beg (- end (length line)))
(put-text-property beg end 'face 'ascope-line-face str)
))
str))

(defun ascope-insert-text-with-properites (text filename &optional line-number)
(let (plist beg end
(outbuf (get-buffer-create ascope-output-buffer-name)))
(progn
(set-buffer outbuf)
(if (not ascope-first-match)
(progn
(insert ascope-action-message "\n\n")
(insert ascope-separator-line)
))

;;insert file name here
;; If the current file is not the same as the previous
;; one ...
(if (not (and ascope-last-file
(string= filename ascope-last-file)))
(progn
;; The current file is different.

;; Insert a separating blank line if
;; necessary.
(if ascope-last-file (insert "\n"))
;; Insert the file name
(setq str (concat "*** " filename ":"))
(if ascope-use-face
(put-text-property 0 (length str)
'face 'ascope-file-face
str))
(insert str)
(insert "\n")
(setq ascope-last-file filename)
))

(if (not ascope-first-match)
(progn
(setq ascope-first-match-point (point))
(setq ascope-first-match '(t))
))
(setq beg (point))
(insert text)

(setq end (point)
plist (plist-put plist 'ascope-file filename))
(if line-number
(progn
(if (stringp line-number)
(setq line-number (string-to-number line-number)))
(setq plist (plist-put plist 'ascope-line-number line-number))
))
(add-text-properties beg end plist)
(insert "\n")
)))

(defun ascope-process_one_chunk (text-start text-end)
(with-current-buffer "*ascope*"
(setq stuff (buffer-substring-no-properties text-start text-end))
(while (and stuff
(string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)" stuff))
(setq line (substring stuff
(match-beginning 1) (match-end 1)))

(setq stuff (substring stuff
(match-beginning 2)
(match-end 2)))
(if (= (length stuff) 0)
(setq stuff nil))

(if (string-match
"\\([^[:blank:]]*\\)[[:blank:]]+\\([^[:blank:]]*\\)[[:blank:]]+\\([[:digit:]]*\\)[[:blank:]]+\\(.*\\)"
line)
(progn
(let (str)
(setq file (substring line (match-beginning 1)
(match-end 1))
function-name (substring line (match-beginning 2)
(match-end 2))
line-number (substring line
(match-beginning 3)
(match-end 3))
line (substring line (match-beginning 4)
(match-end 4))
)

(ascope-insert-text-with-properites
(ascope-make-entry-line function-name
line-number
line)
(expand-file-name file)
line-number)
))))
)
)


(defun ascope-process-output ()
(setq ascope-first-match nil
ascope-last-file nil)
(if (get-buffer ascope-output-buffer-name)
(kill-buffer ascope-output-buffer-name)
)
(let (text-start text-end text-max)
(with-current-buffer "*ascope*"
(setq text-start (point))
(setq text-max (point-max))
(if (>= (- text-max text-start) 5000)
(setq text-end (+ text-start 5000))
(setq text-end text-max))
)
(while (and (> (- text-end text-start) 0) (<= text-end text-max))

(ascope-process_one_chunk text-start text-end)

(setq text-start (+ text-end 1))
(if (>= (- text-max text-start) 5000)
(setq text-end (+ text-start 5000))
(setq text-end text-max))))
)

(defun ascope-wait-for-output (&optional timeout)

(let ((proc (get-buffer-process (current-buffer)))
(found nil)
(start-time (current-time))
(start-point (point)))

(save-excursion
(while (not found)
(accept-process-output proc 1)
(goto-char (point-max)) ;move the last line
(beginning-of-line) ;move the beginning of last line
(setq found (looking-at "^>>"))) ;looking for cscope prompt "^>>"
)
)
)

(provide 'ascope)
;;; ascope.el ends here
