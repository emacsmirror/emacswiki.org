;;; ecurses.el --- Curses-like API for Emacs

;; Copyright (c) 2022 Christopher Leyon

;; Version:     0.1
;; Time-stamp:  <2022-03-13 16:04:11 cleyon>
;; Author:      Christopher Leyon <cleyon@gmail.com>
;; Created:     <2022-03-13 15:54:30 cleyon>
;; Keywords:    window

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Installation:

;;; To do:

;;; Change Log:

;;; Code:


(defvar stdscr "Standard screen")

;;; XXX These should be window specific!
(defvar ecurses--echo nil)
(defvar ecurses--timeout -1)

(defun background (width height c)
  "Make a background grid."
  (erase-buffer)
  (dotimes (i (1- height))
    (insert-char c width) ; or equivalent: (dotimes (j width) (insert c))
    (newline))
  (insert-char c width) ; see above
)

(defun dbgpoint ()
  (interactive)
  (let ((yx (wgetyx 'stdscr)))
    (message "point:Y=%d,X=%d" (car yx) (cadr yx))
    yx))



 
(defun addch (c &optional verbose)
  (waddch 'stdscr c verbose))

(defun addstr (s &optional verbose)
  (waddstr 'stdscr s verbose))

(defun clrtobot ()
  (wclrtobot 'stdscr))

(defun clrtoeol ()
  (wclrtoeol 'stdscr))

(defun curs_set (n)
  (cond ((= n 0) (setq cursor-type nil))
        ((= n 1) (setq cursor-type ecurses--cursor))
        ((= n 2) (setq cursor-type ecurses--cursor))
        (t (user-error "Ecurses: parameter to curs_set must be 0, 1, or 2"))))

(defun echo ()
  (setq ecurses--echo t))

(defun endwin ()
  ;; (erase-buffer)
)

(defun erase ()
  (werase 'stdscr))

(defun flushinp ()
  (discard-input))

(defun getch ()
  (wgetch 'stdscr))

(defun getmaxx ()
  (wgetmaxx 'stdscr))

(defun getmaxy ()
  (wgetmaxy 'stdscr))

(defun getmaxyx (win)
  (list (get win 'width) (get win 'height)))

(defmacro getnstr (var n)
  (list 'setq var (list 'xwgetstr (list 'quote 'stdscr) n)))

(defmacro getstr (var)
  (list 'setq var (list 'xwgetstr (list 'quote 'stdscr) nil)))

(defun getx ()
  (wgetx 'stdscr))

(defun gety ()
  (wgety 'stdscr))

(defun getyx ()
  (wgetyx 'stdscr))

(defun initscr (&optional width height strictp background-char)
  (when (and strictp
             (or (> width  (window-body-width))
                 (> height (window-text-height))))
    (user-error "Ecurses: %dx%d screen too large for %dx%d window"
                width height
                (window-body-width) (window-text-height)))
  (let ((my-width  (or width  (window-body-width)))
        (my-height (or height (window-text-height)))
        (background (or background-char ?\s))) ; ?\.  for more visible background
    (put 'stdscr 'width my-width)
    (put 'stdscr 'height my-height)
    (put 'stdscr 'buffer (current-buffer))
    (put 'stdscr 'window (selected-window))
    (put 'stdscr 'bgchar background)

    (setq ecurses--cursor cursor-type)
    (font-lock-mode 0)

    (werase 'stdscr)
    ;; (background my-width my-height background)
    ;; (goto-char (point-min))
))

(defun move (y x)
  (wmove 'stdscr y x))

(defun mvaddstr (y x s &optional verbose)
  (wmove 'stdscr y x)
  (waddstr 'stdscr s verbose))

(defun mvwaddstr (win y x s &optional verbose)
  (wmove win y x)
  (waddstr win s verbose))

(defun napms (ms)
  (sleep-for 0 ms)
  (refresh))

(defun noecho ()
  (setq ecurses--echo nil))

(defun printw (s &rest args)
  (let ((text (eval `(format s ,@args))))
    (addstr text)))

(defun refresh ()
  (wrefresh 'stdscr))

(defun waddch (win c &optional verbose)
  (when verbose
    (message "waddch('%c')" c))
  (with-current-buffer (get win 'buffer)
    (cond ((zerop c) nil)
          ((= c ?\n)
           ;; Should this scroll??
           (if (>= (wgetx win) (1- (wgetmaxx win)))
               ;; We're already on the bottom line, so adding a newline
               ;; is meaningless.  Just overwrite the bottom line again.
               (wmove win 0 (wgetx win))
             (wmove win 0 (1+ (wgetx win)))))

          (t (delete-char 1)
             (insert c)
             (when (>= (wgety win) (wgetmaxy win))
               (backward-char 1))
             )
          )))

(defun waddstr (win s &optional verbose)
  (mapc (lambda (c)
          (waddch win c verbose))
        (vconcat s)))

(defun wclrtobot (win)
  (with-current-buffer (get win 'buffer)
    (wclrtoeol win)
    (save-excursion
      ;; height==24   0..23
      (while (< (wgetx win) (1- (wgetmaxx win)))
        (mvwdown win)
        (mvwbol win)
        (wclrtoeol win)))))

(defun wclrtoeol (win)
  (with-current-buffer (get win 'buffer)
    (when (not (eolp))
      (save-excursion
        (simple-delete-eol)
        ;(kill-line)
        (insert-char (get win 'bgchar) (- (wgetmaxy win) (wgety win)))))))

(defun werase (win)
  (with-current-buffer (get win 'buffer)
   (background (wgetmaxy win) (wgetmaxx win)
               (get win 'bgchar))
    (goto-char (point-min))))

;;; Don't like either `read-char' or `read-char-exclusive' because they
;;; display junk in echo area.
(defun wgetch (win)
  (let (ch good-char)
    (while (not good-char)
      (setq ch (if (< ecurses--timeout 0)
                   (read-key)
                 (read-key nil nil (/ ecurses--timeout 1000.0))))
      (when (eq (type-of ch) 'integer)
        (setq good-char t)))
    ch))

(defun wgetmaxx (win)
  (cadr (getmaxyx win)))

(defun wgetmaxy (win)
  (car (getmaxyx win)))

(defun wgetx (win)
  (with-current-buffer (get win 'buffer)
    (1- (line-number-at-pos))))

(defun wgety (win)
  (with-current-buffer (get win 'buffer)
    (current-column)))

(defun wgetyx (win)
  (list (wgety win) (wgetx win)))

(defun wmove (win y x)
  (with-current-buffer (get win 'buffer)
    (goto-char (point-min))
    (forward-line x)
    (forward-char y)))

(defun wrefresh (win)
  (sit-for 0))



 
;;;; Non-standard

(defun mvbol ()
  (mvwbol 'stdscr))

(defun mvdown ()
  (mvwdown 'stdscr))

(defun mvleft ()
  (mvwleft 'stdscr))

(defun mvright ()
  (mvwright 'stdscr))

(defun mvtab (n)
  (mvwtab 'stdscr n))

(defun mvup ()
  (mvwup 'stdscr))

(defun mvwbol (win)
  (wmove win 0 (wgetx win)))

(defun mvwdown (win)
  (when (< (wgetx win) (1- (wgetmaxx win)))
    (wmove win (wgety win) (1+ (wgetx win)))))

(defun mvwleft (win)
  (when (> (wgety win) 0)
    (wmove win (1- (wgety win)) (wgetx win))))

(defun mvwright (win)
  (when (< (wgety win) (1- (wgetmaxy win)))
    (wmove win (1+ (wgety win)) (wgetx win))))

(defun mvwtab (win n)
  (when (and (>= n 0)
             (< n (1- (wgetmaxy win)))
             (> n (wgety win)))
    ;; (message "At %d, target %d => %d" (wgety win) n (- n (wgety win)))
    (dotimes (x (- n (wgety win)))
      (mvwright win))))

(defun mvwup (win)
  (when (> (wgetx win) 0)
    (wmove win (wgety win) (1- (wgetx win)))))

(defun simple-delete-eol ()
  (let ((pt (point)))
    (while (not (eolp))
      (forward-char))
    (delete-region pt (point))))

(defun xwgetstr (win max)
  (interactive)
  (let ((s "")
        (len 0)
        done x)
    (while (not done)
      (setq x (wgetch win))
      ;; (message "s='%s', x=%c" s x)
      (cond ((eq x 7)   ; C-g
             ;; Check [no]qiflush()
             ;;; XXX Should C-g (interrupt) delete its input?  If so, emulate C-u
             ;; (while (> len 0)
             ;;   (when ecurses--echo
             ;;     (backward-delete-char 1)
             ;;     (insert (get 'stdscr 'bgchar)) ; should really should be previous char
             ;;     (mvwleft win))
             ;;   (setq len (1- len)
             ;;         s   (substring s 0 len))
             ;;   )
             (setq s "")
             (setq done t))

            ((eq x 13)  ; ENTER
             (setq done t))

            ((eq x 21)  ; C-u
             (while (> len 0)
               (when ecurses--echo
                 (backward-delete-char 1)
                 (insert (get 'stdscr 'bgchar)) ; should really should be previous char
                 (mvwleft win))
               (setq len (1- len)
                     s   (substring s 0 len))
               ))

            ((eq x 127) ; DEL
             (when (> len 0)
               (when ecurses--echo
                 (backward-delete-char 1)
                 (insert (get 'stdscr 'bgchar)) ; should really should be previous char
                 (mvwleft win))
               (setq len (1- len)
                     s   (substring s 0 len))
               ))

            (t          ; any other character
             (if (and max (= len max))
                 (ding)
               (setq len (1+ len)
                     s   (concat s (char-to-string x)))
               (when ecurses--echo
                 (if (eq ecurses--echo t)
                     (waddch win x)
                   (waddch win ecurses--echo)))))))
    s)
)


(provide 'ecurses)

;;; ecurses.el ends here
