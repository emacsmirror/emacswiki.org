;;; erc-track-xmas.el

;; Xemacs port of erc-track module. Colors and mouse on erc track 
;; modeline.

;; Synched with 1.62 cvs version.

;; Copyright (C) 2002  Mario Lang <mlang@delysid.org>

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm, faces
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcChannelTracking

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Highlights keywords and pals (friends), and hides or highlights fools
;; (using a dark color).  Add to your ~/.emacs:

;; (require 'erc-track)
;; (erc-track-mode 1)

;; Todo:
;; * Add extensibility so that custom functions can track
;;   custom modification types.

(require 'erc)
(require 'erc-compat)
(require 'erc-match)

;;; Code:

(defconst erc-track-version "$Revision: 1.62 $"
  "ERC track mode revision")

(defgroup erc-track nil
  "Track active buffers and show activity in the modeline."
  :group 'erc)

(defcustom erc-track-visibility t
  "Where do we look for buffers to determine their visibility?
The value of this variable determines, when a buffer is considered
visible or invisible.  New messages in invisible buffers are tracked,
while switching to visible buffers when they are tracked removes them
from the list.  See also `erc-track-when-inactive-mode'.

Possible values are:

t                - all frames
visible          - all visible frames
nil              - only the selected frame
selected-visible - only the selected frame if it is visible

Activity means that there was no user input in the last 10 seconds."
  :group 'erc-track
  :type  '(choice (const :tag "All frames" t)
		  (const :tag "All visible frames" visible)
		  (const :tag "Only the selected frame" nil)
		  (const :tag "Only the selected frame if it was active" active)))

(defcustom erc-track-exclude nil
  "A list targets (channel names or query targets) which should not be tracked."
  :group 'erc-track
  :type '(repeat string))

(defcustom erc-track-exclude-types '("NICK")
  "*List of message types to be ignored.
This list could look like '(\"JOIN\" \"PART\")."
  :group 'erc-track
  :type 'erc-message-type)

(defcustom erc-track-shorten-start 1
  "This number specifies the minimum number of characters a channel name in
the mode-line should be reduced to."
  :group 'erc-track
  :type 'number)

(defcustom erc-track-shorten-cutoff 4
  "All channel names longer than this value will be shortened."
  :group 'erc-track
  :type 'number)

(defcustom erc-track-shorten-aggressively nil
  "*If non-nil, channel names will be shortened more aggressively.
Usually, names are not shortened if this will save only one character.
Example: If there are two channels, #linux-de and #linux-fr, then
normally these will not be shortened.  When shortening aggressively,
however, these will be shortened to #linux-d and #linux-f.

If this variable is set to `max', then channel names will be shortened
to the max.  Usually, shortened channel names will remain unique for a
given set of existing channels.  When shortening to the max, the shortened
channel names will be unique for the set of active channels only.
Example: If there are tow active channels #emacs and #vi, and two inactive
channels #electronica and #folk, then usually the active channels are
shortened to #em and #v.  When shortening to the max, however, #emacs is
not compared to #electronica -- only to #vi, therefore it can be shortened
even more and the result is #e and #v.

This setting is used by `erc-track-shorten-names'."
  :group 'erc-track
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (const :tag "Max" max)))

(defcustom erc-track-shorten-function 'erc-track-shorten-names
  "*This function will be used to reduce the channel names before display.
It takes one argument, CHANNEL-NAMES which is a list of strings.
It should return a list of strings of the same number of elements.
If nil instead of a function, shortening is disabled."
  :group 'erc-track
  :type '(choice (const :tag "Disabled")
		 function))

(defcustom erc-track-use-faces t
  "*If non-nil, use faces to indicate current nick/pal/fool/keyword/dangerous
host activities in the mode-line.
The faces used are the same as used for text in the buffers.
\(e.g. `erc-pal-face' is used if a pal sent a message to that channel.)"
  :group 'erc-track
  :type 'boolean)

(defcustom erc-track-faces-priority-list
  '(erc-error-face erc-current-nick-face erc-keyword-face erc-pal-face
    erc-nick-msg-face erc-direct-msg-face erc-dangerous-host-face
    erc-default-face erc-action-face erc-nick-default-face erc-fool-face
    erc-notice-face erc-input-face erc-prompt-face)
  "A list of faces used to highlight active buffer names in the modeline.
If a message contains one of the faces in this list, the buffer name will
be highlighted using that face.  The first matching face is used."
  :group 'erc-track
  :type '(repeat face))

(defcustom erc-track-priority-faces-only nil
  "If you would like to ignore changes in certain channels where there
are no faces corresponding to your erc-track-faces-priority-list, set
this variable.  You can set a list of channel name strings, so those
will be ignored while all other channels will be tracked as normal.
Other options are 'all, to apply this to all channels or nil, to disable
this feature.
Note: If you have a lot of faces listed in erc-track-faces-priority-list,
setting this variable might not be very useful."
  :group 'erc-track
  :type '(choice (const nil)
		 (repeat string)
		 (const all)))

(defvar erc-modified-channels-string ""
  "Internal string used for displaying modified channels in the mode line.")

(put 'erc-modified-channels-string 'risky-local-variable t); allow properties

(if (featurep 'xemacs)
    (defconst def-global-mode-string global-mode-string))

(defvar erc-modified-channels-alist nil
  "An ALIST used for tracking channel modification activity.
Each element looks like (BUFFER COUNT FACE) where BUFFER is a buffer
object of the channel the entry corresponds to, COUNT is a number
indicating how often activity was noticed, and FACE is the face to use
when displaying the buffer's name.  See `erc-track-faces-priority-list',
and `erc-track-showcount'.

Entries in this list should only happen for buffers where activity occurred
while the buffer was not visible.")

;;; Shortening of names

(defun erc-track-shorten-names (channel-names)
  "Call `erc-unique-channel-names' with the correct parameters.
This function is a good value for `erc-track-shorten-function'.
The list of all channels is returned by `erc-all-buffer-names'.
CHANNEL-NAMES is the list of active channel names.
Only channel names longer than `erc-track-shorten-cutoff' are
actually shortened, and they are only shortened to a minimum
of `erc-track-shorten-start' characters."
  (erc-unique-channel-names
   (erc-all-buffer-names)
   channel-names
   (lambda (s)
     (> (length s) erc-track-shorten-cutoff))
   erc-track-shorten-start))

(defvar erc-default-recipients)

(defun erc-all-buffer-names ()
  "Return all channel or query buffer names.
Note that we cannot use `erc-channel-list' with a nil argument,
because that does not return query buffers."
  (save-excursion
    (let (result)
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (or (eq major-mode 'erc-mode) (eq major-mode 'erc-dcc-chat-mode))
	  (setq result (cons (buffer-name) result))))
      result)))

(defun erc-unique-channel-names (all active &optional predicate start)
  "Return a list of unique channel names.
ALL is the list of all channel and query buffer names.
ACTIVE is the list of active buffer names.
PREDICATE is a predicate that should return non-nil if a name needs
  no shortening.
START is the minimum length of the name used."
  (if (eq 'max erc-track-shorten-aggressively)
      ;; Return the unique substrings of all active channels.
      (erc-unique-substrings active predicate start)
    ;; Otherwise, determine the unique substrings of all channels, and
    ;; for every active channel, return the corresponding substring.
    ;; Given the names of the active channels, we now need to find the
    ;; corresponding short name from the list of all substrings.  To
    ;; avoid problems when there are two channels and one is a
    ;; substring of the other (notorious examples are #hurd and
    ;; #hurd-bunny), every candidate gets the longest possible
    ;; substring.
    (let ((all-substrings (sort
			   (erc-unique-substrings all predicate start)
			   (lambda (a b) (> (length a) (length b)))))
	  result)
      (dolist (channel active)
	(let ((substrings all-substrings)
	      candidate
	      winner)
	  (while (and substrings (not winner))
	    (setq candidate (car substrings)
		  substrings (cdr substrings))
	    (when (and (string= candidate
				(substring channel
					   0
					   (min (length candidate)
						(length channel))))
		       (not (member candidate result)))
	      (setq winner candidate)))
	  (setq result (cons winner result))))
      (nreverse result))))

(defun erc-unique-substrings (strings &optional predicate start)
  "Return a list of unique substrings of STRINGS."
  (if (or (not (numberp start))
	  (< start 0))
      (setq start 2))
  (mapcar
   (lambda (str)
     (let* ((others (delete str (copy-sequence strings)))
	    (maxlen (length str))
	    (i (min start
                    (length str)))
	    candidate
	    done)
       (if (and (functionp predicate) (not (funcall predicate str)))
	   ;; do not shorten if a predicate exists and it returns nil
	   str
	 ;; Start with smallest substring candidate, ie. length 1.
	 ;; Then check all the others and see whether any of them starts
	 ;; with the same substring.  While there is such another
	 ;; element in the list, increase the length of the candidate.
	 (while (not done)
	   (if (> i maxlen)
	       (setq done t)
	     (setq candidate (substring str 0 i)
		   done (not (erc-unique-substring-1 candidate others))))
	   (setq i (1+ i)))
	 (if (and (= (length candidate) (1- maxlen))
		  (not erc-track-shorten-aggressively))
	     str
	   candidate))))
   strings))

(defun erc-unique-substring-1 (candidate others)
  "Return non-nil when any string in OTHERS starts with CANDIDATE."
  (let (result other (maxlen (length candidate)))
    (while (and others
		(not result))
      (setq other (car others)
	    others (cdr others))
      (when (and (>= (length other) maxlen)
		 (string= candidate (substring other 0 maxlen)))
	(setq result other)))
    result))

;;; Test:

(assert
 (and
  ;; verify examples from the doc strings
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("#emacs" "#vi" "#electronica" "#folk")
	    '("#emacs" "#vi")))
	 '("#em" "#vi")); emacs is different from electronica
  (equal (let ((erc-track-shorten-aggressively t))
	   (erc-unique-channel-names
	    '("#emacs" "#vi" "#electronica" "#folk")
	    '("#emacs" "#vi")))
	 '("#em" "#v")); vi is shortened by one letter
  (equal (let ((erc-track-shorten-aggressively 'max))
	   (erc-unique-channel-names
	    '("#emacs" "#vi" "#electronica" "#folk")
	    '("#emacs" "#vi")))
	 '("#e" "#v")); emacs need not be different from electronica
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("#linux-de" "#linux-fr")
	    '("#linux-de" "#linux-fr")))
	 '("#linux-de" "#linux-fr")); shortening by one letter is too aggressive
  (equal (let ((erc-track-shorten-aggressively t))
	   (erc-unique-channel-names
	    '("#linux-de" "#linux-fr")
	    '("#linux-de" "#linux-fr")))
	 '("#linux-d" "#linux-f")); now we want to be aggressive
  ;; specific problems
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("#dunnet" "#lisp" "#sawfish" "#fsf" "#guile"
	      "#testgnome" "#gnu" "#fsbot" "#hurd" "#hurd-bunny"
	      "#emacs")
	    '("#hurd-bunny" "#hurd" "#sawfish" "#lisp")))
	   '("#hurd-" "#hurd" "#s" "#l"))
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-substrings
	    '("#emacs" "#vi" "#electronica" "#folk")))
	 '("#em" "#vi" "#el" "#f"))
  (equal (let ((erc-track-shorten-aggressively t))
	   (erc-unique-substrings
	    '("#emacs" "#vi" "#electronica" "#folk")))
	 '("#em" "#v" "#el" "#f"))
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("#emacs" "#burse" "+linux.de" "#starwars"
	      "#bitlbee" "+burse" "#ratpoison")
	    '("+linux.de" "#starwars" "#burse")))
	 '("+l" "#s" "#bu"))
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("fsbot" "#emacs" "deego")
	    '("fsbot")))
	 '("fs"))
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("fsbot" "#emacs" "deego")
	    '("fsbot")
	    (lambda (s)
	      (> (length s) 4))
	    1))
	 '("f"))
  (equal (let ((erc-track-shorten-aggressively nil))
	   (erc-unique-channel-names
	    '("fsbot" "#emacs" "deego")
	    '("fsbot")
	    (lambda (s)
	      (> (length s) 4))
	    2))
	 '("fs"))
  (let ((erc-track-shorten-aggressively nil))
    (equal (erc-unique-channel-names '("deego" "#hurd" "#hurd-bunny" "#emacs")
				     '("#hurd" "#hurd-bunny"))
	   '("#hurd" "#hurd-")))
  ;; general examples
  (let ((erc-track-shorten-aggressively t))
    (and (equal (erc-unique-substring-1 "abc" '("ab" "abcd")) "abcd")
	 (not (erc-unique-substring-1 "a" '("xyz" "xab")))
	 (equal (erc-unique-substrings '("abc" "xyz" "xab")) 
		'("ab" "xy" "xa"))
	 (equal (erc-unique-substrings '("abc" "abcdefg"))
		'("abc" "abcd"))))
  (let ((erc-track-shorten-aggressively nil))
    (and (equal (erc-unique-substring-1 "abc" '("ab" "abcd")) "abcd")
	 (not (erc-unique-substring-1 "a" '("xyz" "xab")))
	 (equal (erc-unique-substrings '("abc" "xyz" "xab")) 
		'("abc" "xyz" "xab"))
	 (equal (erc-unique-substrings '("abc" "abcdefg")) 
		'("abc" "abcd"))))))

;;; Module

;;;###autoload (autoload 'erc-track-mode "erc-track" nil t)
(define-erc-module track track-modified-channels
  "This mode tracks ERC channel buffers with activity."
  ((or global-mode-string
       (setq global-mode-string '("")))
   (if (featurep 'xemacs)
       (progn
	 (setq global-mode-string def-global-mode-string)
	 (setq global-mode-string
	     (append global-mode-string
		     '(erc-modified-channels-string))))
     (or (memq 'erc-modified-channels-string global-mode-string)
	 (setq global-mode-string
	       (append global-mode-string
		       '(erc-modified-channels-string)))))
   (setq erc-modified-channels-string "")
   (erc-update-mode-line)
   (if (featurep 'xemacs)
       (defadvice switch-to-buffer (after erc-update (&rest args) activate)
         (erc-modified-channels-update))
     (add-hook 'window-configuration-change-hook 'erc-modified-channels-update))
   (add-hook 'erc-insert-post-hook 'erc-track-modified-channels))
  ((if (featurep 'xemacs)
       (setq global-mode-string def-global-mode-string)
     (setq global-mode-string
	   (delq 'erc-modified-channels-string global-mode-string)))
   (if (featurep 'xemacs)
       (ad-disable-advice 'switch-to-buffer 'after 'erc-update)
     (remove-hook 'window-configuration-change-hook 'erc-modified-channels-update))
   (remove-hook 'erc-insert-post-hook 'erc-track-modified-channels)))

;;;###autoload (autoload 'erc-track-when-inactive-mode "erc-track" nil t)
(define-erc-module track-when-inactive nil
  "This mode enables channel tracking even for visible buffers,
if you are inactivity."
  ((if (featurep 'xemacs)
       (defadvice switch-to-buffer (after erc-update-when-inactive (&rest args) activate)
         (erc-user-is-active))
     (add-hook 'window-configuration-change-hook 'erc-user-is-active))
   (add-hook 'erc-send-completed-hook 'erc-user-is-active)
   (add-hook 'erc-server-001-hook 'erc-user-is-active))
  ((setq global-mode-string
	 (delq 'erc-modified-channels-string global-mode-string))
   (if (featurep 'xemacs)
       (ad-disable-advice 'switch-to-buffer 'after 'erc-update-when-inactive)
     (remove-hook 'window-configuration-change-hook 'erc-user-is-active))
   (remove-hook 'erc-send-completed-hook 'erc-user-is-active)
   (remove-hook 'erc-server-001-hook 'erc-user-is-active)
   (remove-hook 'erc-timer-hook 'erc-user-is-active)))

;;; Visibility

(defvar erc-buffer-activity nil
  "Last time the user sent something.")

(defvar erc-buffer-activity-timeout 10
  "How many seconds of inactivity by the user
to consider when `erc-track-visibility' is set to
only consider active buffers visible.")

(defun erc-user-is-active (&rest ignore)
  "Set `erc-buffer-activity'."
  (setq erc-buffer-activity (erc-current-time))
  (erc-track-modified-channels))

(defun erc-buffer-visible (buffer)
  "Return non-nil when the buffer is visible."
  (if erc-track-when-inactive-mode
      (when erc-buffer-activity; could be nil
	(and (get-buffer-window buffer erc-track-visibility)
	     (<= (erc-time-diff erc-buffer-activity (erc-current-time))
		 erc-buffer-activity-timeout)))
    (get-buffer-window buffer erc-track-visibility)))

;;; Tracking the channel modifications

(defvar erc-modified-channels-update-inside nil
  "Variable to prevent running `erc-modified-channels-update' multiple
times.  Without it, you cannot debug `erc-modified-channels-display',
because the debugger also cases changes to the window-configuration.")

(defun erc-modified-channels-update ()
  "This function updates the information in `erc-modified-channels-alist'
according to buffer visibility.  It calls
`erc-modified-channels-display' at the end. This should usually be
called via `window-configuration-change-hook'."
  (interactive)
  (unless erc-modified-channels-update-inside
    (let ((erc-modified-channels-update-inside t))
      (mapcar (lambda (elt)
		(let ((buffer (car elt)))
		  (when (or (not (bufferp buffer))
			    (not (buffer-live-p buffer))
			    (erc-buffer-visible buffer))
		    (erc-modified-channels-remove-buffer buffer))))
	      erc-modified-channels-alist)
      (erc-modified-channels-display)
      (force-mode-line-update t))))


(if (featurep 'xemacs)
    (defun erc-make-mode-line-buffer-name (string buffer &optional faces count)
      (let ((map (make-sparse-keymap))
	    (name (if erc-track-showcount
		      (concat string
			      erc-track-showcount-string
			      (int-to-string (car count)))
		    (copy-sequence string)))
	    (extent (make-extent nil nil))
	    (format nil))
	(define-key map 'button2
	  `(lambda (e)
	     (interactive "e")
	     (erc-modified-channels-remove-buffer ,buffer)
	     (erc-modified-channels-display)
	     (save-selected-window
	       (select-window
		(event-window e))
	       (switch-to-buffer ,buffer))))
	(define-key map 'button3
	  `(lambda (e)
	     (interactive "e")
	     (erc-modified-channels-remove-buffer ,buffer)
	     (erc-modified-channels-display)))
	(when (and faces erc-track-use-faces)
	  (set-extent-face extent (car faces)))
	(set-extent-keymap extent map)
	(set-extent-property extent 'help-echo 
			     (concat "button2: Goto mod channel/button3: remove mod channel: " (buffer-name buffer)))
	(setq format (cons extent (concat "[" name "]")))
	format))

  (defun erc-make-mode-line-buffer-name (string buffer &optional faces count)
    "Return STRING as a button that switches to BUFFER when clicked.
If FACES are provided, color STRING with them."
    ;; We define a new sparse keymap every time, because 1. this data
    ;; structure is very small, the alternative would require us to
    ;; defvar a keymap, 2. the user is not interested in customizing it
    ;; (really?), 3. the defun needs to switch to BUFFER, so we would
    ;; need to save that value somewhere.
    (let ((map (make-sparse-keymap))
	  (name (if erc-track-showcount
		    (concat string
			    erc-track-showcount-string
			    (int-to-string count))
		  (copy-sequence string))))
      (define-key map (vector 'mode-line 'mouse-2)
	`(lambda (e)
	   (interactive "e")
	   (save-selected-window
	     (select-window
	      (posn-window (event-start e)))
	     (switch-to-buffer ,buffer))))
      (define-key map (vector 'mode-line 'mouse-3)
	`(lambda (e)
	   (interactive "e")
	   (save-selected-window
	     (select-window
	      (posn-window (event-start e)))
	     (switch-to-buffer-other-window ,buffer))))
      (put-text-property 0 (length name) 'local-map map name)
      (when (and faces erc-track-use-faces)
	(put-text-property 0 (length name) 'face faces name))
      name)))

(defun erc-modified-channels-display ()
  "Set `erc-modified-channels-string'
according to `erc-modified-channels-alist'.
Use `erc-make-mode-line-buffer-name' to create buttons."
  (if (or
	(eq 'mostactive erc-track-switch-direction)
	(eq 'leastactive erc-track-switch-direction))
      (erc-track-sort-by-activest))
  (if (null erc-modified-channels-alist)
      (setq erc-modified-channels-string "")
    ;; erc-modified-channels-alist contains all the data we need.  To
    ;; better understand what is going on, we split things up into
    ;; four lists: BUFFERS, COUNTS, SHORT-NAMES, and FACES.  These
    ;; four lists we use to create a new
    ;; `erc-modified-channels-string' using
    ;; `erc-make-mode-line-buffer-name'.
    (let* ((buffers (mapcar 'car erc-modified-channels-alist))
	   (counts (mapcar 'cadr erc-modified-channels-alist))
	   (faces (mapcar 'cddr erc-modified-channels-alist))
	   (long-names (mapcar 'buffer-name buffers))
	   (short-names (if (functionp erc-track-shorten-function)
			    (funcall erc-track-shorten-function
				     long-names)
			  long-names))
	   strings)
      (if (featurep 'xemacs)
	  (progn
	    (while buffers
	      (setq strings (cons (list
				   (car short-names)
				   (car buffers)
				   (car faces)
				   (car counts))
				  strings)
		    short-names (cdr short-names)
		    buffers (cdr buffers)
		    counts (cdr counts)
		    faces (cdr faces)))
	    (setq erc-modified-channels-string
		  (mapcar (lambda (elt)
			    (erc-make-mode-line-buffer-name (car elt) (nth 1 elt) (nthcdr 2 elt) (nthcdr 3 elt)))
			  (nreverse strings))))
	(while buffers
	  (setq strings (cons (erc-make-mode-line-buffer-name
			       (car short-names)
			       (car buffers)
			       (car faces)
			       (car counts))
			      strings)
		short-names (cdr short-names)
		buffers (cdr buffers)
		counts (cdr counts)
		faces (cdr faces)))
	(setq erc-modified-channels-string
	      (concat " [" (mapconcat 'identity (nreverse strings) ",") "] "))))))

(defun erc-modified-channels-remove-buffer (buffer)
  "Remove BUFFER from `erc-modified-channels-alist'."
  (interactive "bBuffer: ")
  (setq erc-modified-channels-alist
	(delete (assq buffer erc-modified-channels-alist)
		erc-modified-channels-alist))
  (when (interactive-p)
    (erc-modified-channels-display)))

(defun erc-track-find-face (faces)
  "Return the face to use in the modeline from the faces in FACES.
If `erc-track-faces-priority-list' is set, the one from FACES who is
first in that list will be used."
  (let ((candidates erc-track-faces-priority-list)
	candidate face)
    (while (and candidates (not face))
      (setq candidate (car candidates)
	    candidates (cdr candidates))
      (when (memq candidate faces)
	(setq face candidate)))
    face))

(defun erc-track-modified-channels ()
  "Hook function for `erc-insert-post-hook' to check if the current
buffer should be added to the modeline as a hidden, modified
channel.  Assumes it will only be called when current-buffer
is in `erc-mode'."
  (let ((this-channel (erc-default-target)))
    (if (and (not (erc-buffer-visible (current-buffer)))
	     (not (member this-channel erc-track-exclude))
	     (not (erc-message-type-member
		   (or (erc-find-parsed-property)
                       (point-min))
		   erc-track-exclude-types)))
	;; If the active buffer is not visible (not shown in a
	;; window), and not to be excluded, determine the kinds of
	;; faces used in the current message, and unless the user
	;; wants to ignore changes in certain channels where there
	;; are no faces corresponding to the erc-track-faces-priority-list,
	;; and the faces in the current message are found in said
	;; priority list, add the buffer to the erc-modified-channels-alist,
	;; if it is not already there.  If the buffer is already on the list
	;; (in the car), change its face attribute (in the cddr) if
	;; necessary.  See `erc-modified-channels-alist' for the
	;; exact data structure used.
	(let ((faces (erc-faces-in (buffer-string))))
	  (unless (and
		   (or (eq erc-track-priority-faces-only 'all)
		       (member this-channel erc-track-priority-faces-only))
		   (not (dolist (f faces)
			  (when (member f erc-track-faces-priority-list)
			    (return t)))))
	  (if (not (assq (current-buffer) erc-modified-channels-alist))
	      ;; Add buffer, faces and counts
	      (setq erc-modified-channels-alist
		    (cons (cons (current-buffer)
				(cons 1 (erc-track-find-face faces)))
			  erc-modified-channels-alist))
	    ;; Else modify the face for the buffer, if necessary.
	    (when faces
 	      (let* ((cell (assq (current-buffer)
				erc-modified-channels-alist))
		     (old-face (cddr cell))
		     (new-face (erc-track-find-face
				(if old-face
				    (cons old-face faces)
				  faces))))
		(setcdr cell (cons (1+ (second cell)) new-face)))))
	  ;; And display it
	  (erc-modified-channels-display)))
      ;; Else if the active buffer is the current buffer, remove it
      ;; from our list.
      (when (or (erc-buffer-visible (current-buffer))
		(and this-channel
		     (assq (current-buffer) erc-modified-channels-alist)
		     (member this-channel erc-track-exclude)))
	;; Remove it from mode-line if buffer is visible or
	;; channel was added to erc-track-exclude recently.
	(erc-modified-channels-remove-buffer (current-buffer))
	(erc-modified-channels-display)))))

(defun erc-faces-in (str)
  "Return a list of all faces used in STR."
  (let ((i 0)
	(m (length str))
	(faces (erc-list (get-text-property 0 'face str))))
    (while (and (setq i (next-single-property-change i 'face str m))
		(not (= i m)))
      (dolist (face (erc-list (get-text-property i 'face str)))
	(add-to-list 'faces face)))
    faces))

(assert
 (let ((str "is bold"))
   (put-text-property 3 (length str)
		      'face '(bold erc-current-nick-face)
		      str)
   (erc-faces-in str)))

(defun erc-find-parsed-property ()
  "Find the next occurrence of the `erc-parsed' text property."
  (let ((pos (point-min)))
    (while (and pos (not (get-text-property pos 'erc-parsed)))
      (setq pos (next-single-property-change pos 'erc-parsed)))
    pos))

;;; Buffer switching

(defcustom erc-track-switch-from-erc t
  "If non-nil, `erc-track-switch-buffer' will return to the last non-erc buffer
when there are no more active channels."
  :type 'boolean
  :group 'erc-track)

(defcustom erc-track-showcount nil
  "If non-nil, count of unseen messages will be shown for each channel."
  :type 'boolean
  :group 'erc-track)

(defcustom erc-track-showcount-string ":"
  "The string to display between buffer name and the count in the mode line.
The default is a colon, resulting in \"#emacs:9\"."
  :type 'string
  :group 'erc-track)

(defcustom erc-track-switch-direction 'oldest
  "Direction `erc-track-switch-buffer' should switch.
'oldest will find the oldest active buffer.
'newest finds the latest, 'leastactive finds buffer with least unseen messages,
'mostactive - with most unseen messages."
  :group 'erc-track
  :type '(choice (const oldest) (const newest) (const leastactive) (const mostactive)))

(defvar erc-track-last-non-erc-buffer nil
  "Stores the name of the last buffer you were in before activating
`erc-track-switch-buffers'")

(defun erc-track-sort-by-activest ()
  "Sorts erc-modified-channels-alist by 'activeness'(count of not seen messages)
+of channel"
  (setq erc-modified-channels-alist
	(sort erc-modified-channels-alist
	      (lambda (a b) (> (nth 1 a) (nth 1 b))))))

(defun erc-track-get-active-buffer (arg)
  "Return the buffer name of ARG in `erc-modified-channels-alist'.
Negative arguments index in the opposite direction.  This direction is
relative to `erc-track-switch-direction'"
  (let ((dir erc-track-switch-direction)
        offset)
    (if (< arg 0)
        (progn
          (cond
           ((eq 'oldest dir) (setq dir 'newest))
           ((eq 'newest dir) (setq dir 'oldest))
    	   ((eq 'mostactive dir) (setq dir 'leastactive))
    	   ((eq 'leastactive dir) (setq dir 'mostacive)))
          (setq arg (* -1 arg))))
    (setq arg (- arg 1))
    (setq offset (cond
                  ((or (eq 'oldest dir) (eq 'leastactive dir))
                   (- (- (length erc-modified-channels-alist) 1) arg))
                  (t 0)))
    ;; normalise out of range user input
    (if (>= offset (length erc-modified-channels-alist))
        (setq offset (- (length erc-modified-channels-alist) 1))
    (if (< offset 0)
        (setq offset 0))
    (car (nth offset erc-modified-channels-alist)))))

(defun erc-track-switch-buffer (arg)
  "Switch to the next active ERC buffer, or if there are no active buffers,
switch back to the last non-ERC buffer visited.  Next is defined by
`erc-track-switch-direction', a negative argument will reverse this."
  (interactive "p")
  (when erc-track-mode
    (let ((dir erc-track-switch-direction))
      (if erc-modified-channels-alist
	  (progn
	    ;; if we're not in erc-mode, set this buffer to return to
	    (unless (eq major-mode 'erc-mode)
	      (setq erc-track-last-non-erc-buffer (current-buffer)))
	    ;; and jump to the next active channel
	    (switch-to-buffer (erc-track-get-active-buffer arg)))
	;; if no active channels, switch back to what we were doing before
	(when (and erc-track-last-non-erc-buffer
		   erc-track-switch-from-erc
		   (buffer-live-p erc-track-last-non-erc-buffer))
	  (switch-to-buffer erc-track-last-non-erc-buffer))))))

;; These bindings are global, because they pop us from any other
;; buffer to an active ERC buffer!

(global-set-key (kbd "C-c C-@") 'erc-track-switch-buffer)
(global-set-key (kbd "C-c C-SPC") 'erc-track-switch-buffer)

(provide 'erc-track-xmas)

;;; erc-track-xmas.el ends here
