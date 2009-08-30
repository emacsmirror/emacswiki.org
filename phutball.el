;;; phutball.el

;; See http://leechuck.de/phutball for further information and documentation

;; phutball.el is free software

(defvar phutball-mode-map nil
  "Local keymap to use in Phutball mode.")

(if phutball-mode-map nil
  (setq phutball-mode-map (make-sparse-keymap))

  ;; Key bindings for cursor motion.
  (substitute-key-definition 'previous-line 'phutball-move-up
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'next-line 'phutball-move-down
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'beginning-of-line 'phutball-beginning-of-line
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'end-of-line 'phutball-end-of-line
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'forward-char 'phutball-move-right
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'newline 'phutball-set-player-i
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'newline-and-indent 'phutball-do-jump-i
                             phutball-mode-map (current-global-map))
  (substitute-key-definition 'backward-char 'phutball-move-left
                             phutball-mode-map (current-global-map))
  (define-key phutball-mode-map [kp-2] 'phutball-move-down)
  (define-key phutball-mode-map [RET] 'phutball-set-player-i)
  (define-key phutball-mode-map [?J] 'phutball-jump-i)
  (define-key phutball-mode-map [C-j] 'phutball-do-jump-i)
  (define-key phutball-mode-map "\M-n" 'phutball-new-game)
  (define-key phutball-mode-map [kp-8] 'phutball-move-up))

(defvar phutball-font-lock-keywords
  (list '(("@" . font-lock-warning-face)
        ("X" . font-lock-type-face)
        ("." . font-lock-string-face))))

(defvar phutball-mode-hook nil
  "*List of functions to call when entering Phutball mode.")

(defconst phutball-square-width 3
  "*Horizontal spacing between squares on the Phutball board.")

(defconst phutball-square-height 2
  "*Vertical spacing between squares on the Phutball board.")

(defconst phutball-x-offset 3
  "*Number of columns between the Phutball board and the side of the window.")

(defconst phutball-y-offset 1
  "*Number of lines between the Phutball board and the top of the window.")

(defvar phutball-field-width 15
  "*Number of rows in the field")

(defvar phutball-field-height 19
  "*Number of columns in the field")

(defvar phutball-default-server "theopc"
  "*Default Phutball Servername.")

(defvar phutball-default-port 9551
  "*Default Phutball Serverport.")

(defvar phutball-server
  "*The server of the actual phutball game.")

(defvar phutball-port
  "*The port of the actual phutball game.")

(defvar phutball-player-name "Emacs-Test"
  "*The default player-name")

(defvar phutball-opponent-name ""
  "The name of the opponent")

(defvar phutball-ball-position "H10")

(defvar phutball-is-playing -1
  "The player that is playing now. 0 for opponent, 1 for you")

(defvar phutball-jump-list nil)

(defvar phutball-count-moves 0)

(defvar phutball-time-left -1
  "The remaining time for the current move.")

(defun phutball-xy-to-index (x y)
  "Translate X, Y cartesian coords into the corresponding field index."
  (+ (* y phutball-field-width) x y))

(defun phutball-display-field (x y)
  "Displays the Phutball field"
  (goto-char (point-min))
  (let ((count-fields-y (+ 2 y)))
    (while (> count-fields-y 0)
      (let ((count-space phutball-square-height))
        (while (> count-space 0)
          (insert "\n")
          (setq count-space (- count-space 1))))
      (setq count-fields-y (- count-fields-y 1))
      (let ((count-fields-x (+ 2 x)))
        (while (> count-fields-x 0)
          (let ((count-space (- phutball-square-width 1)))
            (while (> count-space 0)
              (insert " ")
              (setq count-space (- count-space 1)))
            (insert "."))
          (setq count-fields-x (- count-fields-x 1)))
        )
      ))
  (goto-char (point-min))
  (insert-char 32 (- phutball-square-width 1))
  (insert "@")
  (let ((count-f x))
    (while (> count-f 0)
      (let ((count-space (- phutball-square-width 1)))
        (while (> count-space 0)
          (insert " ")
          (setq count-space (- count-space 1)))
        (insert (+ ?A (- x count-f))))
      (setq count-f (- count-f 1))
        ))
  (insert-char 32 (- phutball-square-width 1))
  (insert "@")
  (goto-char (point-min))
  (let ((count-fields-y (+ 1 y)))
    (while (> count-fields-y -1)
      (let ((count-space phutball-square-height))
        (while (> count-space 0)
          (next-line 1)
          (setq count-space (- count-space 1))))
      (beginning-of-line)
      (insert (number-to-string count-fields-y))
      (if (> count-fields-y 9 )
          (delete-char 2)
        (delete-char 1))
      (setq count-fields-y (- count-fields-y 1))
      ))
  (phutball-set-movecount 0)
  )

(defun phutball-max-width ()
  "Largest possible board width for the current window."
  (1+ (/ (- (window-width (selected-window))
            phutball-x-offset phutball-x-offset 1)
         phutball-square-width)))

(defun phutball-max-height ()
  "Largest possible board height for the current window."
  (1+ (/ (- (window-height (selected-window))
            phutball-y-offset phutball-y-offset 2)
         ;; 2 instead of 1 because WINDOW-HEIGHT includes the mode line !
         phutball-square-height)))

(defun phutball-goto-square (index)
  "Move point to square number INDEX."
  (phutball-goto-xy (phutball-index-to-x index) (phutball-index-to-y index)))

(defun phutball-goto-xy (x y)
  "Move point to square at X, Y coords."
  (let ((inhibit-point-motion-hooks t))
    (goto-line (+ phutball-y-offset 
                  (* (- phutball-field-height(- y 2)) 
                     phutball-square-height))))
  (move-to-column (- (* (+ 1 x) phutball-square-width) 
                     1)))

(defun phutball-move-down ()
  "Move point down one row on the Phutball board."
  (interactive)
  (if (> (phutball-point-y) 0)
      (next-line phutball-square-height)))

(defun phutball-move-up ()
  "Move point up one row on the Phutball board."
  (interactive)
  (if (< (phutball-point-y) (+ 1 phutball-field-height))
      (previous-line phutball-square-height)))

(defun phutball-move-right ()
  "Move point right one column on the Phutball field."
  (interactive)
  (if (< (phutball-point-x) (+ 1 phutball-field-width))
      (phutball-goto-xy (+ 1 (phutball-point-x)) (phutball-point-y))))

(defun phutball-move-left ()
  "Move point left one column on the Phutball field."
  (interactive)
  (if (> (phutball-point-x) 0)
      (phutball-goto-xy (- (phutball-point-x) 1) (phutball-point-y))))

(defun phutball-point-x ()
  "Returns the fields column where point is."
  (with-current-buffer (get-buffer "*Phutball*")
    (let ((p (point)))
      (beginning-of-line)
      (let ((p2 (point)))
        (goto-char p)
        (+ 0 (/ (- p p2) phutball-square-width))))))

(defun phutball-point-y ()
  "Return the board row where point is."
  (with-current-buffer (get-buffer "*Phutball*")
    (let ((inhibit-point-motion-hooks t))
      (+ 2 (- phutball-field-height (/ (- (count-lines 1 (point)) 1) phutball-square-height))))))

(defun phutball-beginning-of-line ()
  "Move point to first square on the Phutball board row."
  (interactive)
  (move-to-column phutball-x-offset))

(defun phutball-end-of-line ()
  "Move point to last square on the Phutball board row."
  (interactive)
  (move-to-column (- (* phutball-field-width phutball-square-width) 1)))

(defun phutball-convert-coords (l)
  "Converts phutball-string-coordinates to phutball-number-coordinates."
  (let ((temp (string-to-list l)))
    (setq a (- (car temp) 64))
    (if (> (length temp) 2)
        (setq b (+ (* 10 (- (car (cdr temp)) (string-to-char "0")))
                   (- (car (cdr (cdr temp))) (string-to-char "0"))))
      (setq b (- (car (cdr temp)) (string-to-char "0")))))
  (list a b))

(defun phutball-convert-coords-backward (x y)
  "Converts the decimal presentation of the coordinates to phutball-rep
Like (10,12) to H12"
  (let ((as (string (- (+ x ?@) 0))))
    (concat as (number-to-string y))))


;;(phutball-convert-coords-backward 8 10)

;;(phutball-convert-coords "H10")
;;(car (cdr (cdr (string-to-list "H10"))))

;; l is the string representation of the coordinates
(defun phutball-set-ball (l)
  (phutball-goto-xy (car (phutball-convert-coords l))
                    (car (cdr (phutball-convert-coords l))))
  (with-current-buffer (get-buffer "*Phutball*")
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@")
      (backward-char))))

(defun phutball-set-player (l)
  (with-current-buffer (get-buffer "*Phutball*")
    (let ((tpoint (point)))
      (phutball-goto-xy (car (phutball-convert-coords l)) 
                        (car (cdr (phutball-convert-coords l))))
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert "X")
        (backward-char))
    (goto-char tpoint))))

(defun phutball-jumpstring-to-list (j)
  (let ((tlist (split-string (substring j 1 (- (length j) 1)) ",")))
    tlist))

;; FIXME: Pruefen, ob Sprung moeglich ist
(defun phutball-jump (j)
  "Jumps over the list defined in j (as String)"
  (let ((moves (mapcar 'phutball-convert-coords 
                       (phutball-jumpstring-to-list j))))
    (phutball-do-jump moves)))


(defun phutball-do-jump (m)
  "Jumps over the list (as list of numbers) m"
  (if (> (length m) 1)
      (progn
        (phutball-set-movecount phutball-count-moves)
        (let ((p1 (car m))
              (p2 (car (cdr m))))
          (with-current-buffer (get-buffer "*Phutball*")
            (phutball-goto-xy (car p1) (car (cdr p1)))
            (let ((inhibit-read-only t))
              (delete-char 1)
              (insert ".")
              (backward-char))
            (while (or (not (= (car p2) (phutball-point-x)))
                       (not (= (car (cdr p2)) (phutball-point-y))))
              (if (> (- (car p1) (car p2)) 0) ;; move left
                  (phutball-move-left)
                (if (< (- (car p1) (car p2)) 0) ;; move right
                    (phutball-move-right)))
              (if (> (- (car (cdr p1)) (car (cdr p2))) 0) ;; move up
                  (phutball-move-down)
                (if (< (- (car (cdr p1)) (car (cdr p2))) 0) ;; move down
                    (phutball-move-up)))
              (let ((inhibit-read-only t))
                (delete-char 1)
                (insert ".")
                (backward-char)))
            (let ((inhibit-read-only t))
              (delete-char 1)
              (insert "@")
              (backward-char))))
        (phutball-do-jump (cdr m)))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (phutball-point-x) (phutball-point-y)))))

(defun phutball-jump-i ()
  "Appends to jumplist"
  (interactive)
  (if (= phutball-is-playing 1)
      (progn
        (with-current-buffer (get-buffer "*Phutball*")
          (save-excursion
            (if (interactive-p)
                (setq phutball-jump-list nil))
            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; nach oben
            (phutball-move-up)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-up))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "1"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; rechts oben
            (phutball-move-up)
            (phutball-move-right)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-up)
                    (phutball-move-right))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "2"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; nach rechts
            (phutball-move-right)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-right))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "3"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; rechts unten
            (phutball-move-down)
            (phutball-move-right)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-down)
                    (phutball-move-right))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "4"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; nach unten
            (phutball-move-down)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-down))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "5"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; links unten
            (phutball-move-down)
            (phutball-move-left)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-down)
                    (phutball-move-left))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "6"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; nach links
            (phutball-move-left)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-left))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "7"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; links oben
            (phutball-move-up)
            (phutball-move-left)
            (if (= (char-after) ?X)
                (progn
                  (while (= (char-after) ?X)
                    (phutball-move-up)
                    (phutball-move-left))
                  (let ((inhibit-read-only t))
                    (delete-char 1)
                    (insert "8"))))

            (phutball-goto-xy (car (phutball-convert-coords
                                    phutball-ball-position))
                              (car (cdr (phutball-convert-coords
                                         phutball-ball-position))))
            ;; der Ball
            (let ((inhibit-read-only t))
              (delete-char 1)
              (insert "0"))

            (setq phutball-jump-list
                  (append phutball-jump-list
                          (list (list (- (phutball-point-x) 1)
                                      (- (phutball-point-y) 0)))))


            (let ((tempstring (read-from-minibuffer "Where to jump?")))
              (cond ((= (string-to-number tempstring) 1)
                     (phutball-clear-field)
                     (phutball-jump-up)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 2)
                     (phutball-clear-field)
                     (phutball-jump-right-up)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 3)
                     (phutball-clear-field)
                     (phutball-jump-right)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 4)
                     (phutball-clear-field)
                     (phutball-jump-right-down)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 5)
                     (phutball-clear-field)
                     (phutball-jump-down)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 6)
                     (phutball-clear-field)
                     (phutball-jump-left-down)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 7)
                     (phutball-clear-field)
                     (phutball-jump-left)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 8)
                     (phutball-clear-field)
                     (phutball-jump-left-up)
                     (phutball-jump-i))
                    ((= (string-to-number tempstring) 0)
                     (phutball-clear-field)
                     (phutball-do-jump-i)))))))))


(defun phutball-clear-field ()
  "Stupid function because undo does not work.
Removes all occurences of numbers from the field."
  (let ((x (+ 1 phutball-field-width)))
    (while (> x 0)
      (let ((y (+ 1 phutball-field-height)))
        (while (> y 0)
          (phutball-goto-xy x y)
          (if (or (= (char-after) ?1)
                  (= (char-after) ?2)
                  (= (char-after) ?3)
                  (= (char-after) ?4)
                  (= (char-after) ?5)
                  (= (char-after) ?6)
                  (= (char-after) ?7)
                  (= (char-after) ?8)
                  (= (char-after) ?0))
              (progn
                (let ((inhibit-read-only t))
                  (delete-char 1)
                  (insert "."))))
          (setq y (- y 1))))
      (setq x (- x 1))))
      (phutball-goto-xy (car (phutball-convert-coords
                              phutball-ball-position))
                        (car (cdr (phutball-convert-coords
                                   phutball-ball-position))))
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert "@")))


(defun phutball-jump-up ()
  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-up)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-up)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@")
      (backward-char 1))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 0) (phutball-point-y)))
    ))))
  
(defun phutball-jump-right-up ()
  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-up)
  (phutball-move-right)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-up)
        (phutball-move-right)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))
  
(defun phutball-jump-right ()

  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-right)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-right)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))
  
(defun phutball-jump-right-down ()

  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-right)
  (phutball-move-down)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-right)
        (phutball-move-down)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))
  
(defun phutball-jump-down ()

  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-down)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-down)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))
  
(defun phutball-jump-left-down ()

  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-down)
  (phutball-move-left)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-left)
        (phutball-move-down)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))
  
(defun phutball-jump-left ()

  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-left)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-left)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))
  
(defun phutball-jump-left-up ()

  (phutball-goto-xy (car (phutball-convert-coords
                          phutball-ball-position))
                    (car (cdr (phutball-convert-coords
                               phutball-ball-position))))
  ;; Ball loeschen
  (let ((inhibit-read-only t))
    (delete-char 1)
    (insert ".")
    (backward-char 1)
  ;; den Rest loeschen
  (phutball-move-up)
  (phutball-move-left)
  (if (= (char-after) ?X)
  (progn
    (while (= (char-after) ?X)
      (let ((inhibit-read-only t))
        (delete-char 1)
        (insert ".")
        (backward-char 1)
        (phutball-move-left)
        (phutball-move-up)))
    ;; Ball setzen
    (let ((inhibit-read-only t))
      (delete-char 1)
      (insert "@"))
    (setq phutball-ball-position (phutball-convert-coords-backward
                                  (- (phutball-point-x) 1) (phutball-point-y)))
    ))))


(defun phutball-do-jump-i ()
  "Executes the jump"
  (interactive)
  (setq phutball-count-moves (+ 1 phutball-count-moves))
  (if (not phutball-jump-list)
      (message "Nowhere to jump")
    (progn
      (message "Jumping")
      (phutball-do-jump phutball-jump-list)
      (setq phutball-is-playing 0)
      (let ((tempstring "[")
            (templist phutball-jump-list))
        (let ((a (car templist)))
          (setq tempstring (concat tempstring 
                                   (phutball-convert-coords-backward
                                    (car a) (car (cdr a))))))
        (setq templist (cdr templist))
        (while templist
          (let ((a (car templist)))
            (setq tempstring (concat tempstring ","
                                     (phutball-convert-coords-backward
                                      (car a) (car (cdr a))))))
          (setq templist (cdr templist)))
        (setq tempstring (concat tempstring "]"))
        (with-current-buffer (get-buffer "*phutball-socket-temp-buf*")
          (insert (concat "Ich_ziehe (Jump " tempstring ")\n")))
        (process-send-string (get-process "phutball-socket")
                             (concat "Ich_ziehe (Jump " tempstring ")\n")))
      (setq phutball-jump-list nil))))

;;(phutball-jump "[H10,H12,H13]")

;; FIXME: Pruefen, ob schon X steht
(defun phutball-set-player-i ()
  "Sets the ball interactivly"
  (interactive)
  (let ((tpoint (point)))
  (setq phutball-count-moves (+ 1 phutball-count-moves))
  (with-current-buffer (get-buffer "*Phutball*")
    (if (> phutball-is-playing 0)
        (if (not (= (char-after) ?X))
            (progn
              (let ((inhibit-read-only t))
                (delete-char 1)
                (insert "X")
                (backward-char))
              (with-current-buffer (get-buffer "*phutball-socket-temp-buf*")
                (insert (concat "Ich_ziehe (Place " (phutball-convert-coords-backward
                                                     (phutball-point-x) (phutball-point-y))
                                ")\n")))
              (process-send-string (get-process "phutball-socket")
                                   (concat "Ich_ziehe (Place "
                                           (phutball-convert-coords-backward 
                                            (phutball-point-x) (phutball-point-y)) ")\n"))
              (phutball-set-movecount phutball-count-moves)
              (setq phutball-is-playing 0))
          (message "Not possible"))
      (message "Not your turn")))
  (goto-char tpoint)))


(defun phutball-open-connection ()
  (open-network-stream "phutball-socket" "phutball-socket-buffer"
                       phutball-server phutball-port)
  (set-process-filter (get-process "phutball-socket") 'phutball-filter))
;;  (process-send-string (get-process "phutball-socket")
;;                     phutball-player-name))

;; FIXME: Totally buggy. Check if buffer exists. Check for empty lines.
;;        Fix newlines.
(defun phutball-filter (proc string)
  (with-current-buffer (get-buffer-create "*phutball-socket-temp-buf*")
    (insert string))
  (if (string-match "\n" string)
      (with-current-buffer (get-buffer "*phutball-socket-temp-buf*")
        (backward-char)
        (delete-char 1)
        (goto-char (- (point-max) 2))
        (goto-char (line-beginning-position))
        (replace-regexp "\\ *=\\ *" "=")
        (goto-char (line-beginning-position))
        (replace-regexp "\\ *(\\ *" "(")
        (goto-char (line-beginning-position))
        (replace-regexp "\\ *)\\ *" ")")
        (goto-char (line-beginning-position))
        (replace-regexp "\\ *{\\ *" "{")
        (goto-char (line-beginning-position))
        (replace-regexp "\\ *}\\ *" "}")
        (goto-char (line-beginning-position))
        (replace-regexp "\\ *,\\ *" ",")
        (goto-char (line-beginning-position))
        (cond 
         ((search-forward "Wer_bist_du" nil t)
          (progn
            (insert (concat "\nIch_bin \"" phutball-player-name "\"\n"))
            (process-send-string (get-process "phutball-socket") 
                      (concat "Ich_bin \"" phutball-player-name "\"\n"))))
         ((re-search-forward 
           "Das_Spielfeld_ist.*breit=[0-9]+" nil t)
          (progn 
            (copy-to-register 'a (+ 35 (match-beginning 0))
                              (match-end 0))
            (setq phutball-field-width (string-to-number (get-register 'a)))
            (re-search-forward "hoch=[0-9]+")
            (copy-to-register 'a (+ 5 (match-beginning 0))
                              (match-end 0))
            (setq phutball-field-height (string-to-number (get-register 'a)))
            (re-search-forward "ball=[A-O][0-9]+")
            (copy-to-register 'a (+ 5 (match-beginning 0))
                              (match-end 0))
            (setq phutball-ball-position (get-register 'a))
            (with-current-buffer (get-buffer "*Phutball*")
              (let ((inhibit-read-only t))
                (phutball-display-field phutball-field-width phutball-field-height))
              (phutball-set-ball phutball-ball-position))
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (goto-char (line-end-position))
            (insert "\nOK\n")))
         ((re-search-forward "Die_Spielzeit_ist.*" nil t)
          (progn
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (goto-char (line-end-position))
            (insert "\nOK\n")))
         ((re-search-forward "Dein_Gegner_ist[^\n]*" nil t)
          (progn
            (copy-to-register 'a (+ 17 (match-beginning 0))
                              (- (match-end 0) 1))
            (with-current-buffer (get-buffer "*Phutball*")
              (setq phutball-opponent-name (get-register 'a)))
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (goto-char (line-end-position))
            (insert "\nOK\n")))
         ((re-search-forward "Ende .*#[^\n]*" nil t)
          (progn
            (setq phutball-is-playing -1)
            (copy-to-register 'a (+ 7 (match-beginning 0))
                              (match-end 0))
            (with-current-buffer (get-buffer "*Phutball*")
              (message (concat "Ende: " (get-register 'a))))))
         ((re-search-forward "Du_beginnst" nil t)
          (progn
            (message "Du beginnst")
            (phutball-set-playername phutball-player-name
                                     phutball-opponent-name)
;;          (setq phutball-is-playing 1)
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (goto-char (line-end-position))
            (insert "\nOK\n")))
         ((re-search-forward "Der_andere_beginnt" nil t)
          (progn
            (setq phutball-is-playing 0)
            (message "Dein Gegner beginnt")
            (phutball-set-playername phutball-opponent-name
                                     phutball-player-name)
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (goto-char (line-end-position))
            (insert "\nOK\n")))
         ((re-search-forward "Wo_ziehst_du" nil t)
          (progn
            (setq phutball-is-playing 1)
            (goto-char (line-end-position))
            (insert "\n")))
         ((re-search-forward "Die_Restzeit_ist.*(Zeit{main=[0-9]+" nil t)
          (progn
            (copy-to-register 'a (+ 27 (match-beginning 0))
                              (match-end 0))
            (let ((maintime (get-register 'a)))
              (re-search-forward "byoyomi=[0-9]+" nil t)
              (copy-to-register 'a (+ 8 (match-beginning 0))
                                (match-end 0))
              (setq phutball-time-left (+ (string-to-number maintime)
                                          (string-to-number 
                                           (get-register 'a)))))
            (goto-char (line-end-position))
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (insert "\nOK\n")))
         ((re-search-forward "Der_andere_zieht.*(Place ...?.?)" nil t)
          (progn
            (setq phutball-count-moves (+ 1 phutball-count-moves))
            (phutball-set-movecount phutball-count-moves)
            (copy-to-register 'a (+ 23 (match-beginning 0))
                              (- (match-end 0) 1))
            (phutball-set-player (get-register 'a))
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (goto-char (line-end-position))
            (insert "\nOK\n")))
         ((re-search-forward "Der_andere_zieht.*(Jump [^\\ s]*)" nil t)
          (progn
            (setq phutball-count-moves (+ 1 phutball-count-moves))
            (phutball-set-movecount phutball-count-moves)
            (copy-to-register 'a (+ 22 (match-beginning 0))
                              (- (match-end 0) 1))
            (phutball-jump (get-register 'a))
            (process-send-string (get-process "phutball-socket")
                                 "OK\n")
            (setq phutball-is-playing 1)
            (goto-char (line-end-position))
            (insert "\nOK\n")))
))))

(defun phutball-new-game ()
  (interactive)
  (with-current-buffer "*Phutball*"
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (with-current-buffer "*phutball-socket-temp-buf*"
    (erase-buffer))
  (phutball-open-connection))


(defun phutball-set-playername (a b)
  "Inserts the names of the 2 players in the status-line below the field."
  (with-current-buffer "*Phutball*"
    (let ((inhibit-read-only t)
          (tpoint (point)))
      (phutball-goto-xy 0 0)
      (end-of-line)
      (newline 2)
      (insert (concat "Spieler: " a " vs. " b))
      (goto-char tpoint))))

(defun phutball-set-movecount (a)
  "Displays the number of moves on the right side of the field."
  (with-current-buffer "*Phutball*"
    (let ((inhibit-read-only t)
          (tpoint (point)))
      (phutball-goto-xy (+ 2 phutball-field-width) phutball-field-height)
      (insert "a")
      (backward-char 1)
      (kill-line)
      (insert (concat "     Zug: " (number-to-string a)))
      (goto-char tpoint))))

(defun phutball-display-time ()
  "Displays the remaining time for the current move."
  (if (= phutball-is-playing 1)
      (progn
        (with-current-buffer "*Phutball*"
          (if (not (= phutball-time-left -1))
              (progn
                (let ((inhibit-read-only t)
                      (tpoint (point)))
                  (phutball-goto-xy (+ 2 phutball-field-width)
                                    (- phutball-field-height 1))
                  (insert "a")
                  (backward-char 1)
                  (kill-line)
                  (insert (concat "     Zeit: " (number-to-string
                                                 phutball-time-left)))
                  (goto-char tpoint))
                (setq phutball-time-left (- phutball-time-left 1))))))))

(defun phutball-mode ()
  "Major mode for playing phutball games over a network server."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Phutball*") t)
  (erase-buffer)
  (kill-all-local-variables)
  (setq major-mode 'phutball-mode)
  (setq mode-name "Phutball")
  (run-hooks 'phutball-mode-hook)
;;  (phutball-display-field phutball-field-width phutball-field-height)
  (phutball-goto-xy 1 1)
  (use-local-map phutball-mode-map)
  (setq buffer-read-only t)
  (setq phutball-server (read-from-minibuffer "Server: "
                                       phutball-default-server))
  ;; FIXME: Port has to be a number. Use read and number-p in future
  (setq phutball-port (string-to-number (read-from-minibuffer "Port: "
                                  (number-to-string phutball-default-port))))
  (setq phutball-player-name (read-from-minibuffer "Your Name: "
                                       phutball-player-name))
  (message "Stand by while connecting")
  (turn-on-font-lock)
  (phutball-open-connection)
  (run-at-time 0 1 'phutball-display-time)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-default '(phutball-font-lock-keywords))
)


(provide 'phutball)

;;; phutball.el ends here
