;;; mpris-track-list.el --- MPRIS track list

;; Copyright (C) 2009, Changyuan Yu

;; Author: Changyuan Yu <reivzy@gmail.com>
;; Created: 2009-08-12
;; Version: 0.2
;; Keywords: dbus, mpris
;; Compatibility: Emacs 23

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

;; List playlist via MPRIS, now only support play/stop/pause.

;; use M-x mpris-track-list to open MPRIS track list.

;;; Change Log:
;; 2010-06-05 v0.2
;;     replace 'length' field with 'mtime', and now works with qmmp.
;; 2009-08-12 v0.1
;;     initial version.

;;; Code:

(require 'mpris-dbus)
(require 'cl)

(defgroup mpris-track-list nil
  "MPRIS track list maintain."
  :group 'mpris)

(defcustom mpris-track-list-format
  '((position  3  bold)
    (artist   18  mpris-face0)
    (title    -1  mpris-face1)
    (mtime   10  bold))
  "track list format"
  :type '(repeat (list (choice :tag "keyword"
                               (const position) ;; track position
                               (const codec)
                               (const title)
                               (const genre)
                               (const location)
                               (const mtime)
                               (const album)
                               (const tracknumber)
                               (const artist)
                               (const quality)
                               (const lossy))
                       (integer :tag "width")
                       (face :tag "style"))))

(defcustom mpris-track-list-mtime-format
  'min-sec
  "format of track length(mtime)."
  :type '(choice :tag "length format"
                 (const :tag "minute - second" min-sec)
                 (const :tag "minute"          min)
                 (const :tag "second"          sec)
                 (const :tag "msec"            msec)))


(defcustom mpris-track-list-buffer-name
  "*MPRIS track list*"
  "MPRIS track list buffer name."
  :type 'string)

(defface mpris-face0
  '((default (:foreground "yellow")))
    "mpris face1"
    :group 'mpris-track-list)

(defface mpris-face1
  '((default (:foreground "green")))
  "mpris face2"
  :group 'mpris-track-list)

(defface mpris-face2
  '((default (:foreground "blue")))
  "mpris face3"
  :group 'mpris-track-list)

(defface mpris-track-list-stop-face
  '((default (:background "darkred")))
  "mpris stopped track face"
  :group 'mpris-track-list)

(defface mpris-track-list-pause-face
  '((default (:background "darkblue")))
  "mpris paused track face"
  :group 'mpris-track-list)

(defface mpris-track-list-play-face
  '((default (:background "grey16")))
  "mpris playing track face"
  :group 'mpris-track-list)


;; some utils function

(defun make-incr-list1 (n ret)
  (if (eq n 0) (cons 0 ret)
    (make-incr-list1 (1- n) (cons n ret))))
(defun make-incr-list (n)
  (make-incr-list1 (1- n) ()))

(defun string-fit-width (str w)
  (if (stringp str)
      (let* ((w0  (string-width str))
             (w1  (abs w))
             (del (- w1 w0)))
        (if (<= w0 w1) (if (> w 0) (concat str (make-string del ?\s))
                         (concat (make-string del ?\s) str))
          (concat (truncate-string-to-width str (- w1 3)) "...")))
    (make-string w ?\s)))


;; (string-fit-width "a12345" -9)
;; (string-fit-width "a12345" 8)
;; (string-fit-width "a12345" 5)

(defvar mpris-track-list-current-list
  ()
  "Current track list.")


(defun mpris-track-list-update1 ()
  "Update whole track list from remote player."
  (let (len)
    (setq len (mpris-get-length))
    (setq mpris-track-list-current-list
          (mapcar (lambda (i)
                    (cons `(position . ,i)
                          (mpris-simplify-metadata (mpris-get-metadata i))))
                  (make-incr-list len)))))

(defun mpris-track-list-update ()
  "Update track list and refresh view."
  (interactive)
  (mpris-track-list-update1)
  (mpris-track-list-revert))

(defun mpris-track-list-buffer-exist ()
  "Test MPRIS track list buffer existence"
  (member mpris-track-list-buffer-name
              (mapcar (function buffer-name) (buffer-list))))

;; a list functions convert values to string
(defun mpris-track-list-show-string (val)
  "Function to show string."
  val)

(defun mpris-track-list-show-integer (val)
  "Function to show integer."
  (format "%d" val))

(defun mpris-track-list-show-uri (val)
  "Function to show uri, TODO"
  val)

(defun mpris-track-list-show-mtime (val)
  "Function to show track length, input unit is ms.
Output related to `mpris-track-list-mtime-format'."
  (let* ((f mpris-track-list-mtime-format)
         (sec0 (/ val 1000.0)) ; for 'sec
         (min0 (/ sec0 60.0))  ; for 'min
         (min1 (/ val 60000))  ; for 'min-sec
         (sec1 (/ (- val (* min1 60000)) 1000.0)) ; for 'min-sec
         (str  (cond ((eq f 'min)  (format "%.02f" min0))
                     ((eq f 'sec)  (format "%.02f" sec0))
                     ((eq f 'msec) (format "%d"    val))
                     (t           (format "%d:%02d" min1 (floor sec1))))))
    str))

(defvar mpris-track-list-show-func-alist
  '((position    . mpris-track-list-show-integer)
    (codec       . mpris-track-list-show-string)
    (title       . mpris-track-list-show-string)
    (genre       . mpris-track-list-show-string)
    (location    . mpris-track-list-show-uri)
    (mtime       . mpris-track-list-show-mtime)
    (album       . mpris-track-list-show-string)
    (tracknumber . mpris-track-list-show-integer)
    (artist      . mpris-track-list-show-string)
    (quality     . mpris-track-list-show-string))
  "AList to show value of metadata.")

(defun mpris-track-list-revert ()
  "Refresh track list view."
  (interactive)
  (when (mpris-track-list-buffer-exist)
    (let ((old-buf (buffer-name)))
      (with-current-buffer mpris-track-list-buffer-name
        (let ((l0 (line-number-at-pos (point)))
              (lm (line-number-at-pos (window-start)))
              (n0 (mpris-get-current-track)))
          (setq buffer-read-only nil)
          (goto-char (point-min))
          (erase-buffer)
          ;; insert track list
          (mapc (lambda (i)
                  (let (p1)
                    (insert "  ")
                    (setq p1 (point))
                    (mpris-track-list-insert-line (cdr i))
                    ;; highlight current play track
                    (when (= (car i) n0)
                      (mpris-track-list-highlight p1 (point)))
                    (insert "\n")))
                (mpris-track-list-filter-current))
          ;; remove last empty line
          (move-end-of-line 0)
          (delete-region (point) (point-max))
          (setq buffer-read-only t)
          ;; restore window scroll pos
          (when (equal old-buf (buffer-name))
          ;; only restore window related pos when current buffer
          ;; is track list buffer
            (goto-char (point-min)) (forward-line (1- lm))
            (set-window-start nil (line-beginning-position)))
          ;; resotre cursor pos
          (goto-char (point-min)) (forward-line 9))
        ))))


(defun mpris-track-list-fix-format ()
  "Calc all uncertained width in `mpris-track-list-format'."
  (let* ((f mpris-track-list-format)
         (total (- (- (frame-width) (length mpris-track-list-format))
                   2))
         (ll (mapcar (lambda (i) (nth 1 i)) f))
         (pos (apply '+ (remove-if (lambda (x) (< x 0)) ll)))
         (neg (apply '+ (remove-if (lambda (x) (> x 0)) ll)))
         (left (- total pos)))
    (mapcar (lambda (i)
              (if (>= (nth 1 i) 0) i
                (let* ((w  (nth 1 i))
                       ;; w / neg * left
                       (w1 (floor (/ (* (float w) left) neg))))
                  (cons (car i) (cons w1 (cddr i)))))) f)))

;; filter track list metadata according to current format
;; return (( Id . (metadata) ))
(defun mpris-track-list-filter (data)
  "Filter metadata to left only necessary data for display,
according to `mpris-track-list-format'."
  (let ((id (cdr (assoc 'position data))))
    (cons id (mapcar (lambda (i)
                       (let* ((key (car i))
                              (val (cdr (assoc key data))))
                         (cons key val)))
                     mpris-track-list-format))))

(defun mpris-track-list-filter-current ()
  "Filter `mpris-track-list-current-list'."
  (mapcar 'mpris-track-list-filter mpris-track-list-current-list))


(defun mpris-track-list-show-field (kv)
  "Convert (keyword . value) pair to string."
  (let* ((key (car kv))
         (val (cdr kv))
         (fun (cdr (assoc key mpris-track-list-show-func-alist))))
    (funcall fun val)))

(defun mpris-track-list-insert-line (data)
  "Insert a track line."
  (let ((format (mpris-track-list-fix-format)))
    (mapc (lambda (i)
            (let (k v wid fmt fac p0)
              (setq k (car i))
              (setq v (cdr i))
              (setq fmt (cdr (assoc k format))) ;; format
              (setq wid (nth 0 fmt))
              (setq fac (nth 1 fmt))
              (if (numberp v) (setq wid (- wid))) ;; righ align for number
              (setq p0 (point))
              (insert (string-fit-width
                       (mpris-track-list-show-field i) wid))
              (add-text-properties p0 (point)
                                   `(font-lock-face ,fac))
              (insert " ")))
            data)))

(defun mpris-track-list-face-attr-list (face)
  "remove face unspecified attrib and make a property list"
  (mapcar (lambda (x) (list (car x) (cdr x)))
          (remove-if (lambda (x) (eq 'unspecified (cdr x)))
                     (face-all-attributes face (selected-frame))))
  )

(defun mpris-track-list-highlight (p0 p1)
  "Highlight region according to current media player status."
  (let ((s (car (mpris-get-status)))
        f0 a0)
    (cond ((= s 1) (setq f0 'mpris-track-list-pause-face))
          ((= s 2) (setq f0 'mpris-track-list-stop-face))
          (t       (setq f0 'mpris-track-list-play-face)))
    (setq a0 (mpris-track-list-face-attr-list f0))
    (let ((p p0)
          attr face1)
      (while (< p p1)
        (setq face1 (get-text-property p 'face))
        (if (listp face1) (setq attr (append a0 face1))
          (setq attr (append a0 (mpris-track-list-face-attr-list face1))))
        (add-text-properties p (1+ p) `(font-lock-face ,attr))
        (setq p (1+ p))))))



(defun mpris-track-list-play (&optional pos)
  "Play track at current line."
  (interactive)
  (if (string= (buffer-name) mpris-track-list-buffer-name)
      (let* ((n1  (or pos (1- (line-number-at-pos))))
             (n0  (mpris-get-current-track))
             (del (abs (- n1 n0)))
             (act (if (> n1 n0) 'mpris-next 'mpris-prev)))
        (unless (= del 0)
          (mpris-stop)
          (dotimes (i del)
            (funcall act)))
          (mpris-play)
          )))


;;; major mode
(defvar mpris-track-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "g"         'mpris-track-list-revert)
    (define-key map "G"         'mpris-track-list-update)
    (define-key map "n"         'next-line)
    (define-key map "p"         'previous-line)
    (define-key map " "         'mpris-toggle-play)
    (define-key map "s"         'mpris-stop)
    (define-key map "j"         'next-line)
    (define-key map "k"         'previous-line)
    (define-key map "\r"        'mpris-track-list-play)
    (define-key map "q"         'quit-window)
    map)
  "Local keymap for `mpris-track-list-mode' buffer.")


(defun mpris-track-list-mode ()
  "Major mode for maintain media player track list via MPRIS.
\\<mpris-track-list-mode-map>
\\[mpris-track-list-revert] -- revert track list.
\\[mpris-track-list-update] -- update track list from remote media player.
\\[mpris-stop] -- stop play.
\\[mpris-track-list-play] -- play current track.
\\[mpris-toggle-play] -- toggle play/pause status.
"
  (kill-all-local-variables)
  (use-local-map mpris-track-list-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'mpris-track-list-mode)
  (setq mode-name "MPRIS Track List")
  (run-mode-hooks 'mpris-track-list-mode-hook)
  )

;; call back

(defun mpris-track-list-revert-cb (a)
  (mpris-track-list-revert))

(defun mpris-track-list-update-cb (a)
  (mpris-track-list-update))

(defun mpris-track-list-init-dbus-callback ()
  "Init dbus callback, major status change and tracklist change.
TODO"
  (dbus-register-signal :session mpris-service
                        "/Player" "org.freedesktop.MediaPlayer"
                        "TrackChange" 'mpris-track-list-revert-cb)
  (dbus-register-signal :session mpris-service
                        "/Player" "org.freedesktop.MediaPlayer"
                        "StatusChange" 'mpris-track-list-revert-cb)
  ;; current TrackListChange signal is not implement in audacious
  (dbus-register-signal :session mpris-service
                        "/TrackList" "org.freedesktop.MediaPlayer"
                        "TrackListChange" 'mpris-track-list-update-cb)
  )

;;;###autoload
(defun mpris-track-list ()
  "Open MPRIS Track List."
  (interactive)
  (switch-to-buffer mpris-track-list-buffer-name)
  (unless (eq major-mode 'mpris-track-list-mode)
    ;; init buffer
    (mpris-track-list-update)
    (mpris-track-list-mode)
    (mpris-track-list-init-dbus-callback)
    (font-lock-mode t)))

(put 'mpris-track-list-mode 'mode-class 'special)

(provide 'mpris-track-list)

;;; mpris-track-list.el ends here
