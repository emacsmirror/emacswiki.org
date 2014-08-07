;;; lineno.el
;;;
;;; Alternate mode to display line numbers. While there are other
;;; functions that also do this, lineno is better suited for large
;;; files since it does not put line numbers throughout the buffer,
;;; but only in the visible window. This way the size of the file does
;;; not matter, it is the window size which determines the update
;;; speed, and this is fast enough.
;;;
;;; It only works in file buffers so far, and in them only for regular
;;; editing. Anything else which changes the buffer position from
;;; outside the buffer (ediff, etc.) will not update the line numbers.
;;; You can either live without them, do any command in the buffer, or
;;; add lineno-post-command to the appropriate hook.
;;;
;;; There appears to be an emacs bug which causes an infinite loop in
;;; fill-paragraph. See comment and kludge below.
;;;
;;; As with other line numbering functions, tabs work funny, so it is
;;; best to use spaces instead of tabs if the indenting is important
;;; to you. Or, I think if you use "\t" as lineno-padding all will
;;; display correctly.
;;; 
;;; To install: put in path, (require 'lineno) To use: M-x
;;; lineno-minor-mode <RET> to toggle it.  The face used for the line
;;; numbers can be set in a custom group: "M-x customize-group <RET>
;;; lineno <RET>" If the face for the line numbers is larger than the
;;; buffer face there will be some minor problems, like odd scrolling
;;; behavior or line numbers occasionally repeated. To fix this
;;; behavior make sure your line number face is no larger than the
;;; buffer face, and/or turn on lineno-bigface-fix.
;;;
;;; c 2007 Russell Young
;;; emacs@young-0.com
;;; Open source: anyone can use, distribute, or modify, but please
;;; leave in the attribution

(require 'cl)
(load "cl-macs")

(defface lineno-default-face
  '((((type x w32 mac) (class color))
	 (:foreground "black" :background "grey75" :underline t)))

  "Default face used for line numbers in lineno-minor-mode 
Note: Making this face larger than the buffer face (for instance,
making it bold) messes up some movement functions, like 'M->' or
'M-v'. If you make sure the line number font is smaller than the
buffer font this won't be a problem. Or, you can turn on
lineno-bigface-fix, which improves it at the cost of a little extra
processing."
  :group 'lineno)

(defcustom lineno-face 'lineno-default-face
  "Face to use to display line numbers
Note: Making this face larger than the buffer face (for instance,
making it bold) messes up some movement functions, like 'M->' or
'M-v'. If you make sure the line number font is smaller than the
buffer font this won't be a problem. Or, you can turn on
lineno-bigface-fix, which improves it at the cost of a little extra
processing."
  :group 'lineno
  :type 'face)

(defcustom lineno-padding " "
  "Character to put after the line number.
Most commonly would be a blank or empty."
  :group 'lineno
  :type 'string)
(defcustom lineno-padding-use-face t
  "If non-nil write lineno-padding in lineno-face"
  :group 'lineno
  :type 'boolean)

(defcustom lineno-bigface-fix t
  "Call screen painter recursively after it has finished

This is a workaround for the big-face problem: if the line number face
is larger than the buffer face, it is possible for point to be off
screen after painting the window. This can also happen if point is
near the bottom and inserting line numbers causes lines to wrap. In
any case it is not a disaster, it will repaint properly after the next
command. Setting this option to t causes the lineno function to be
called recursively, which will recenter the screen around the actual
position of POINT.

There can still be a little funny behavior with this kludge in, so the
best choice is to keep your lineno face reasonably small. While I
don't see how, it is possible that some funny configuration could
cause this to loop indefinitely."
  :group 'lineno
  :type 'boolean)

;;; internal use variables
(defvar lineno-overlays nil
  "Holds current overlays so they can be reused or removed")
(make-variable-buffer-local 'lineno-overlays)
(defvar lineno-change-overlay nil
  "Holds overlay which contains all visible screen")
(make-variable-buffer-local 'lineno-change-overlay)
(defvar lineno-update-from nil
  "Used to pass the earliest change location to the repainter")
(make-variable-buffer-local 'lineno-update)
(defvar lineno-window-limits nil
  "cons of window start and end position, used to force repaint on change")
(make-variable-buffer-local 'lineno-window-limits)
(defvar lineno-tag-width 0
  "Used to make the width of all line number fields identical")
(make-variable-buffer-local 'lineno-tag-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Painting and updating the screen
;;
;; The line numbers need to be repainted whenever the window moves, or
;; when an insertion or deletion changes the number of line in a file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun different-lines (p1 p2)
  "Returns t if the 2 positions given are on different lines"
  (save-match-data
    (string-match "\n" (buffer-substring p1 p2))))

(defun lineno-get-from ()
  "Computes the initial position for the renumbering
returns nil if no renumbering needed"
  (let ((from (cond ((/= (window-start) (car lineno-window-limits)) (window-start))
                    (lineno-update-from lineno-update-from)
                    ((> (window-end) (cdr lineno-window-limits)) 
                     (cdr lineno-window-limits)))))
    (and from (save-excursion
                (goto-char from)
                (line-beginning-position)))
    ))

;;; see if we already have the first line number before calculating it
;;; There can be multiple overlays if one or more lines are deleted,
;;; use the smallest
(defun lineno-get-first-line (from)
  (or (reduce (lambda (x y) (if x (if y (min x y) x) y))
			  (mapcar (lambda (z) (let ((lineno (overlay-get z 'lineno)))
									(and (numberp lineno) lineno)))
					  (overlays-in from from))
			  :initial-value nil)
	  (1+ (count-lines 1 from))))

;;; The post-command-hook function to repaint the line numbers, if needed. 
(defun lineno-post-command (&optional from force-tag-width)
  "Repaints the visible line numbers when the view or contents change"
  (and (sit-for 0)        ; forces screen update if no commands waiting
       (or from (setq from (lineno-get-from)))
       (let* ((point (point))
              (line (lineno-get-first-line from))
              (same (count-lines (window-start) from))
              (reuse (nthcdr same lineno-overlays))
              (tag-width (or force-tag-width (lineno-tag-width from line))))
         (if (and (not force-tag-width) (/= tag-width lineno-tag-width))
             (lineno-post-command (window-start) tag-width)
           (setf (nthcdr (min same (length lineno-overlays)) lineno-overlays) nil)
           (setq lineno-overlays (append lineno-overlays
                                         (lineno-update-linenos from line reuse tag-width))
                 lineno-window-limits (cons (window-start) (window-end))
                 lineno-update-from nil
                 lineno-tag-width tag-width)
		   (goto-char point)
		   (and lineno-bigface-fix (lineno-post-command))
           ))))

;;; An attempt to get it working for shell mode: doesn't work properly
;(defadvice comint-send-input (after lineno-shell-function disable)
;  (debug)
;  (lineno-post-command))

;;; Records buffer changes so the necessary line numbers can be updated.
;;; Called when changes are made to the visible portion of the buffer
;;;
;;; KLUDGE: There appears to be a bug in emacs 21 (at least) that
;;; causes this to go into an infinite loop for the FILL-PARAGRAPH
;;; command (at least). The problem is that the regexp in
;;; DIFFERENT-LINE causes a SUBST-CHAR-IN-REGION in
;;; FILL-REGION-AS-PARAGRAPH in fill.el to fail. If I copy the line
;;; twice in the source code it works fine, if it only appears once it
;;; fails to make a substitution, and causes an infinite loop
;;; inserting empty comment lines into the buffer.
;;;
;;; I don't have time now to check the source code, so this is handled
;;; in a really crude way, by disabling the check for the specific
;;; command FILL-PARAGRAPH. (I wonder if there are any other commands
;;; where this problem appears.) Since fill-paragraph will almost
;;; certainly cause a screen repaint anyway, there is no cost to this.
(defun lineno-modification (overlay after begin end &optional length)
  (and (or (eq this-command 'fill-paragraph)    ; BAD KLUDGE
           (different-lines begin end))
       (<= begin (or lineno-update-from (buffer-size)))
       (setq lineno-update-from begin)))

;;; Internal function, updates the visible line numbers. Does not preserve point.
(defun lineno-update-linenos (from line reuse width)
  "Inserts line number overlays starting from position FROM and line number LINE.
REUSE is a list of overlays which can be reused rather than always getting new
lisp objects."
  (let* ((to (window-end))
         (overlays ())
         overlay)
    (goto-char from)
    (beginning-of-line)
    (while (< (point) to)
      (if (setq overlay (pop reuse))
          (move-overlay overlay (point) (point))
        (setq overlay (make-overlay (point) (point))))
	  (overlay-put overlay 'lineno line)
      (overlay-put overlay 'before-string (lineno-tag line width))
      (setq overlays (cons overlay overlays)
            line (1+ line))
      (forward-line 1))
    (mapcar 'delete-overlay reuse)
    (reverse overlays)))

;;; Manufactures the text which is used for line numbers
(defun lineno-tag (num width)
  (let ((text (format "        %s%s" num  lineno-padding)))
    (setq text (substring text (- (length text) (+ width (length lineno-padding)))))
    (put-text-property 0 (if lineno-padding-use-face (length text) width)
                       'face lineno-face text)
    text))

;;; computes the number of chars to put in the tags
(defun lineno-tag-width (from line) 
  (setq line (+ line (count-lines from (window-end))))
  (let ((i 0))
	(while (< 0 (setq i (1+ i)
					  line (/ line 10))))
	i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Command functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lineno-minor-mode (&optional force)
  "Switches on and off lineno-minor-mode. 
With FORCE >= 0 it turns it on, < 0 turns it off, otherwise it toggles
the mode.

lineno-minor-mode displays line numbers on the left side of the screen.
Other line number modes number the entire file, but for large files
they run too slowly to be useful. lineno-minor-mode only numbers the
lines currently visible in the selected window, and only updates them
when needed. This makes it efficient enough to work for any size file.
"
  (interactive)
  (if (or (and (not force) lineno-change-overlay) (and force (<= force 0))) 
	  (lineno-off)
    (setq lineno-change-overlay (make-overlay 0 (+ 2 (buffer-size)) nil nil t)
          lineno-window-limits '(-1 . 0)
          lineno-update-from nil)
    (overlay-put lineno-change-overlay 'modification-hooks '(lineno-modification))
    (overlay-put lineno-change-overlay 'insert-in-front-hooks '(lineno-modification))
    (overlay-put lineno-change-overlay 'insert-behind-hooks '(lineno-modification))
    (overlay-put lineno-change-overlay 'lineno t)
    (add-hook 'post-command-hook 'lineno-post-command nil t)))

(defun lineno-off ()
  "Turns off lineno-minor-mode"
  (interactive)
  (remove-hook 'post-command-hook 'lineno-post-command)
  (mapcar 'delete-overlay lineno-overlays)
  (if lineno-change-overlay (delete-overlay lineno-change-overlay))
  (setq lineno-overlays nil
		lineno-change-overlay nil
		lineno-tag-width 0
		lineno-window-limits nil))

(defun lineno-cleanup ()
  "Turns off lineno-minor-mode, including cleanup if there is a problem
Mainly used during debugging phase, this cleans up extraneous overlays which
could be left up after a problem."
  (interactive)
  (lineno-off)
  (save-restriction
    (widen)
    (mapcar (lambda (overlay) (if (overlay-get overlay 'lineno) 
                                  (delete-overlay overlay)))
            (overlays-in 0 (point-max)))))

(provide 'lineno)
