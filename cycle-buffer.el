;;; cycle-buffer.el --- select buffer by cycling through

;; Author:     Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; Maintainer: same
;; Idea by:    Kurt Partridge <kepart@cs.washington.edu>
;; Created: 05 June 1996
;; Keywords: switch-to-buffer, cycle buffer list

;; LCD Archive Entry:
;; cycle-buffer|Vladimir Alexiev|vladimir@cs.ualberta.ca|
;; Select buffers by cycling|
;; ftp://ftp.cs.ualberta.ca/pub/oolog/emacs/cycle-buffer.el|
;; 21-May-97|Version 2.16|

;;; Commentary:

;; Description:
;; ------------
;; cycle-buffer is yet another way of selecting buffers. Instead of prompting
;; you for a buffer name, cycle-buffer switches to the most recently used
;; buffer, and repeated invocations of cycle-buffer-forward switch to less
;; recently visited buffers. If you accidentally overshoot, calling
;; cycle-buffer-backward goes back. You should issue consecutive cycle command
;; pretty quickly: if there is some intervening command between two cycling
;; commands, or if a settable timeout expires, the cycling is reset and the
;; next cycle-buffer will get you to the last buffer.
;;
;; I find this to be the fastest buffer-switching mechanism; it's like C-x
;; b <return> w/out the return, but it's not limited to the most recently
;; accessed buffer.  Plus you never have to remember buffer names; you
;; just keep cycling until you recognize the buffer you're searching for. The
;; buffer ring is shown in the echo area centered around the current buffer;
;; if you see the name of the buffer you are looking for a few positions away
;; from the center, you can give an argument to cycle-buffer to get directly
;; to it. Positive arguments move to the right, negative arguments to the
;; left.
;;
;; In addition to cycling forward and backward, there are two versions of the
;; command provided: normal and "permissive". The permissive version allows
;; (as per factory settings) buffers of the form *bufname*, while the normal
;; version does not.

;; Installation:
;; -------------
;; Add these lines in your .emacs:
;;   (autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
;;   (autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
;;   (autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
;;   (autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
;;   (autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer will be considered." t)
;;   (global-set-key [(f9)]        'cycle-buffer-backward)
;;   (global-set-key [(f10)]       'cycle-buffer)
;;   (global-set-key [(shift f9)]  'cycle-buffer-backward-permissive)
;;   (global-set-key [(shift f10)] 'cycle-buffer-permissive)
;; You may want to adjust the keyboard bindings to suit your taste. See below
;; for other customisable variables.

;; Todo
;; ----
;; - try not to shift the list in the echo area, move the [ ] only.
;; - in order not to switch to the intermediate buffers, implement an electric
;;   minibuffer mode where cycle-buffer and cycle-buffer-backward only scroll
;;   the buffer list in the minibuffer, and the buffer is switched only upon
;;   exit from that mode (partly suggested by terry@santafe.edu)
;; SUGGESTIONS ARE WELCOME.

;; ChangeLog
;; ---------
;; Fri Jun  7 11:00:32 1996 [VA]
;; floatp-safe is not defined in emacs, pointed by tim@ipac.caltech.edu
;;
;; Sat Jun  8 17:27:57 1996 [VA]
;; alex.lefaive@Eng.Sun.COM suggested an "inverse" of cycle-buffer-ignore, a
;; variable that would only *allow* certain buffers. I decided to generalise
;; the two to cycle-buffer-filter, a general and-or form.
;;
;; Sat Jun  8 20:11:23 1996 [VA]
;; Doc fix, pointed by terry@santafe.edu
;;
;; Mon Jun 10 11:55:25 1996 [VA]
;; Added cycle-buffer-interesting and cycle-buffer-toggle-interesting,
;; suggestion by Josh MacDonald <jmacd@CS.Berkeley.EDU>
;;
;; Mon Mar 24 14:54:37 1997 [Martin Stjernholm <mast@lysator.liu.se>]
;; Added cycle-buffer-next-command to pre-command-hook to discard changes
;; to the buffer list caused by buffers we just fly through.
;;
;; Wed May 21 02:29:18 1997 [VA]
;; cycle-buffer-message: don't log the buffer list onto the message log
;;
;; Mon Dec  1 18:57:31 1997 [VA]
;; cycle-buffer-shorten-name: turn ` ' in buf name to `_', also save a couple
;; of spaces around each name.

(eval-when-compile (require 'cl))

(defvar cycle-buffer-filter
  '((not (eq (aref (buffer-name) 0) ? )) ; " buffer"
    (not (member (buffer-name)          ; uninteresting buffers
                 '("lispdir.dat" "*reportmail*" ".newsrc-dribble" "info dir"
                   ".infonotes")))
    (not (string-match "^\\(TAGS\\|\\*?sent\\)" (buffer-name)))
    (or (eq cycle-buffer-allow-visible t) ; visible buffers
        (eq (current-buffer) cycle-buffer-current)
        (not (get-buffer-window (current-buffer) 
                                (if cycle-buffer-allow-visible nil 'visible)))))
  "*A list of forms that determine if a buffer is considered for switching to.
All forms should return non-nil for a buffer to be eligible. The forms are
evaluated in the buffer in question, so they can check its buffer-local
variables (eg major-mode).

You can add more restrictions by consing to the variable from inside
cycle-buffer-load-hook or after (require 'cycle-buffer). For example to
restrict switching from a C++ file only to relevant files, do this:
  (setq cycle-buffer-filter (cons 
     '(or (not (eq major-mode 'c++-mode))
          (string-match \"\\\\.\\\\(cc\\\\|hh\\\\)$\\\\|I?[mM]akefile\" (buffer-name)))
     cycle-buffer-filter))
Unfortunately, in order to *relax* the restrictions, you will probably need to
copy the variable to your .emacs and make changes in the text.")

(defvar cycle-buffer-filter-extra
  '((not (string-match "^\\*.*\\*\\(<[0-9]+>\\)?$" (buffer-name)))
    cycle-buffer-interesting)
  "*List of forms that are evaluated in addition to cycle-buffer-filter for
the non-permissive versions of the cycle-buffer commands.")

(defvar cycle-buffer-allow-visible 'other
  "*Whether to consider visible buffers. nil: ignore them; t: allow them; any
other value: allow buffers visible on other frames, but not on the selected
frame.") 

(defvar cycle-buffer-show t
  "*Whether to show the buffer names in the echo area when you invoke one of
the cycle-buffer commands. Possible values:
  nil      never
  t        always (same as 0)
  number   after that many successive invocations of a cycle-buffer command
  'prefix  only when the command was given a DISTANCE prefix argument.")

(defvar cycle-buffer-show-permissive 'maybe
  "*Whether to show buffers in the echo area using the permissive version of
the test. nil: use the restricted version; t: use the permissive version; any
other value: use the setting of the invoked cycle command. If you don't intend
to mix cycle-buffer commands of different permissiveness, set this to 'maybe.")

(defvar cycle-buffer-show-length 20
  "*Maximum number of chars to show of a buffer name.")

(defvar cycle-buffer-show-format '(" [%s]" . " %s")
  "*A cons of two format strings, the car of which is used for the current
buffer, and the cdr for non-current buffers.")

(defvar cycle-buffer-reset-after 4.5
  "*Reset the cycle after this many seconds of inactivity. The next
cycle-buffer will return you to the last buffer instead of going forward.
 (Any intervening command other than the cycling commands does this reset.)
This option is useful if you sometimes switch between buffers and do not
execute any commands, but stay for some time (eg to read something), and after
that want to return to the previous buffer. Set this to nil if you don't like
this behaviour.")

(defvar cycle-buffer-load-hook nil
  "Hook that is run right after cycle-buffer is loaded.")

;; end of user variables

(defconst cycle-buffer-commands
  '(cycle-buffer cycle-buffer-backward
                 cycle-buffer-permissive cycle-buffer-backward-permissive)
  "List of all cycle-buffer commands.")

(defvar cycle-buffer-invocations 0
  "How many cycle commands were invoked successively.")

(defvar cycle-buffer-last-time nil
  "Last time a cycle command was called.")

(defvar cycle-buffer-list nil
  "Buffer list as set by the last cycle-buffer command.")

(defvar cycle-buffer-current nil
  "The value of (current-buffer) before the command was invoked.")

(defvar cycle-buffer-interesting t
  "Whether the current buffer should be considered. Use
cycle-buffer-toggle-interesting to set it interactively.")

(make-variable-buffer-local 'cycle-buffer-interesting)

;;;###autoload
(defun cycle-buffer (&optional distance permissive)
  "Switch to the next buffer on the buffer list without prompting.
Successive invocations select buffers further down on the buffer list.
A prefix argument specifies the DISTANCE to skip, negative moves back."
  (interactive "p")
  (let ((time (current-time)) list last buf)
    (if cycle-buffer-reset-after        ; avoid float computation if not needed
        (setq time (+ (* (nth 0 time) 65536.0)
                      (nth 1 time)
                      (/ (nth 2 time) 1e6))))
    (if (or (not cycle-buffer-list)     ; initialization
            (not (memq last-command cycle-buffer-commands)) ; intervening cmd
            (and cycle-buffer-reset-after
                 (or (not (funcall
                           (if (fboundp 'floatp-safe) 'floatp-safe 'floatp)
                           cycle-buffer-last-time))
                     (> (- time cycle-buffer-last-time)
                        cycle-buffer-reset-after)))) ; elapsed time
	(progn
	  ;; Put current buffer on top of Emacs' buffer list
	  (switch-to-buffer (current-buffer))
	  ;; regenerate the buffer list
	  (setq cycle-buffer-invocations 0
		cycle-buffer-list (cycle-buffer-filter (buffer-list)
						       cycle-buffer-filter))))
                                        ; we use the permissive filter here
                                        ; because cycle-buffer-list is shared 
                                        ; between all cycle functions      
    (setq cycle-buffer-last-time time)
    (setq cycle-buffer-invocations (1+ cycle-buffer-invocations))
    (setq list
          (if permissive cycle-buffer-list
            (cycle-buffer-filter cycle-buffer-list cycle-buffer-filter-extra)))
    (if (null list) (error "There is no appropriate buffer to switch to."))
    (if (< distance 0)
        (setq distance (- distance)
              list (reverse list))) ; not nreverse: preserve cycle-buffer-list
    (setq distance (% distance (length list)))
    (if (and (not (eq (car list) (current-buffer)))
             (> distance 0))
        (setq distance (1- distance)))
    (while (not (zerop distance))
      (setq distance (1- distance)
            list (cdr list)))
    (setq buf (car list))
    (if (eq buf (current-buffer))
        (error "There's no point in switching to the current buffer."))
    ;; find the cons before the one containing buf
    (setq list cycle-buffer-list)
    (while (and list (not (eq (car (cdr list)) buf)))
      (setq list (cdr list)))
    (or list (setq list cycle-buffer-list))
    ;; find the last cons
    (setq last cycle-buffer-list)
    (while (cdr last) (setq last (cdr last)))
    ;; restructure the list so that buf is in the beginning
    (setcdr last cycle-buffer-list)
    (setq cycle-buffer-list (cdr list))
    (setcdr list nil)
    ;; and go
    ;; Switch buffer but do not change Emacs' buffer list. Install a
    ;; hook that makes that change if the next command isn't a
    ;; cycle-buffer command.
    (switch-to-buffer buf t)
    (add-hook 'pre-command-hook 'cycle-buffer-next-command)
    (if (cond ((eq nil cycle-buffer-show) nil)
              ((eq t cycle-buffer-show))
              ((eq 'prefix cycle-buffer-show)
               current-prefix-arg)
              ((numberp cycle-buffer-show)
               (< cycle-buffer-show cycle-buffer-invocations))
              ((error "Invalid value: cycle-buffer-show, %s" cycle-buffer-show)))
        (cycle-buffer-show permissive))))

;;;###autoload
(defun cycle-buffer-backward (&optional distance)
  "Switch to the previous buffer in the buffer list."
  (interactive "p")
  (cycle-buffer (- distance)))

;;;###autoload
(defun cycle-buffer-permissive (&optional distance)
  "Switch to the next buffer, allowing more buffers (*bufname* by default)."
  (interactive "p")
  (cycle-buffer distance t))

;;;###autoload
(defun cycle-buffer-backward-permissive (&optional distance)
  "Switch to the previous buffer, allowing more buffers (*bufname* by default)."
  (interactive "p")
  (cycle-buffer (- distance) t))

(defun cycle-buffer-show (permissive)
  ;; Show cycle-buffer-list in the echo area like this:
  ;;   n/2 n/2+1 .. n [0] 1 2 .. n/2
  ;; WARNING: this waits for cycle-buffer-reset-after seconds if there is no
  ;; interactive command issued sooner.
  (let (wid list str s mid current next p n)
    (setq wid (window-width (minibuffer-window)))
    (setq list
          (cond ((eq cycle-buffer-show-permissive t) cycle-buffer-list)
                ((eq cycle-buffer-show-permissive nil)
                 (cycle-buffer-filter
                  cycle-buffer-list cycle-buffer-filter-extra))
                (permissive cycle-buffer-list)
                ((cycle-buffer-filter
                  cycle-buffer-list cycle-buffer-filter-extra))))
    (setq str (if (and list (eq (car list) (current-buffer))) ""
                ;; selected buffer not in show list 
                (format (car cycle-buffer-show-format) "")))
    (setq mid (/ (length str) 2))
    (setq next t)
    (while (and list (not (and p n (eq (car p) (car n))))
                (< (length str) (* wid 2)))
      (cond ((null n) (setq n list) (setq s (car n)))
            ((null p) (setq p (reverse list)) (setq s (car p)))
            (next (setq n (cdr n)) (setq s (car n)))
            (t (setq p (cdr p)) (setq s (car p))))
      (setq current (eq s (current-buffer)))
      (setq s (format
               (funcall (if current 'car 'cdr) cycle-buffer-show-format)
               (cycle-buffer-shorten-name (buffer-name s))))
      (cond (next (if current
                      (setq mid (/ (length (setq str s)) 2))
                    (setq str (concat str s))))
            (t (setq mid (+ mid (length s)))
               (setq str (concat s str))))
      (setq next (not next)))
    (setq wid (/ wid 2))
    (if (> mid wid) (setq str (substring str (- mid wid))))
    (if (< mid wid) (setq str (concat (make-string (- wid mid) ? ) str)))
    (cycle-buffer-message str)
    (if (and cycle-buffer-reset-after
             (sit-for cycle-buffer-reset-after))
        (cycle-buffer-message ""))))

(defun cycle-buffer-message (str)
  "Show STR but don't log it on the message log."
  (if (fboundp 'display-message)
      ;; XEmacs way of preventing log messages.
      (display-message 'no-log str)
    (let ((message-log-max nil))
        (message "%s" str))))

(defun cycle-buffer-shorten-name (s)
  "Shorten S to cycle-buffer-show-length. Feel free to customize."
  (let (len prefix suffix)
    (if (string-match "\\(ftp \\|/\\)\\(anonymous\\|ftp\\)@" s) ; cut off
        (setq s (concat (substring s 0 (match-beginning 0))
                        (substring s (match-end 0))))
      (setq s (copy-sequence s)))       ; else the loop below is destructive
    (loop for i below (length s)
          do (if (eq (aref s i) ? ) (aset s i ?_)))
    (setq len (length s))
    (if (> len cycle-buffer-show-length)
        (progn ; shorten s but preserve a numeric suffix
          (set-match-data (list len len))
          (string-match "<?[0-9]+>?$" s)
          (setq suffix (match-beginning 0))
          (setq prefix (- cycle-buffer-show-length 2 (- len suffix)))
          (setq s (concat (substring s 0 prefix) ".." (substring s suffix))))))
  s)

(defun cycle-buffer-filter (list filter)
  ;; Filter LIST through the variable cycle-buffer-filter
  (let (result)
    (setq cycle-buffer-current (current-buffer))
    (while list
      (set-buffer (car list))
      (if (eval (cons 'and filter))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (set-buffer cycle-buffer-current)
    (nreverse result)))

(defun cycle-buffer-next-command ()
  ;; Take this buffer to the top if a non-cycle-buffer command was
  ;; issued.
  (if (not (member this-command cycle-buffer-commands))
      (switch-to-buffer (current-buffer)))
  (remove-hook 'pre-command-hook 'cycle-buffer-next-command))

;;;###autoload
(defun cycle-buffer-toggle-interesting (&optional arg)
  "Toggle the value of cycle-buffer-interesting for the current buffer.
With positive arg set it, with non-positive arg reset it. A buffer is only
considered by cycle-buffer when cycle-buffer-interesting is t."
  (interactive "P")
  (setq cycle-buffer-interesting (if arg (> (prefix-numeric-value arg) 0)
                                   (not cycle-buffer-interesting)))
  (message "This buffer will%s be considered by cycle-buffer."
           (if cycle-buffer-interesting "" " not")))

(run-hooks 'cycle-buffer-load-hook)
(provide 'cycle-buffer)

;;; end of cycle-buffer.el

