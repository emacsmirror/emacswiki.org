;;; powerline.el
;; Vim style powerline for Emacs, courtesy of http://www.reddit.com/r/emacs/comments/qlbsr/vim_powerline_ported_to_emacs/c3zoxgy
(defvar powerline-color1)
(defvar powerline-color2)

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(set-face-attribute 'mode-line nil
                    :background "Turquoise"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defvar powerline-minor-modes nil)

(defun powerline-make-face
  (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "powerline-" bg))))
        (make-face cface)
        (set-face-attribute cface nil
                            :foreground (if fg fg "white")
                            :background bg
                            :box nil)
        cface)
    nil))
(defun powerline-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (powerline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (propertize " " 'face plface)
     (if localmap
         (propertize string 'face plface 'mouse-face plface1 'local-map localmap)
       (propertize string 'face plface))
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display (arrow-right-xpm color1 color2))
       ""))))
(defun powerline-make-right
  (string color2 &optional color1 localmap)
  (let ((plface (powerline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
       (propertize " " 'display (arrow-left-xpm color1 color2))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if localmap
         (propertize string 'face plface 'mouse-face plface1 'local-map localmap)
       (propertize string 'face plface))
     (propertize " " 'face plface))))
(defun powerline-make-fill
  (color)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (powerline-make-face color)))
    (propertize " " 'display '((space :align-to (- right-fringe 20)))
                'face plface)))
(defun powerline-make-text
  (string color &optional localmap)
  (let ((plface (powerline-make-face color)))
    (if localmap
        (propertize string 'face plface 'mouse-face plface 'local-map localmap)
      (propertize string 'face plface))))
(defun powerline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (powerline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (powerline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (powerline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (powerline-make-right  string color1 color1 localmap))
        (t                             (powerline-make-text   string color1 localmap))))
(defun powerline-buffer-name
  (side color1 &optional color2)
  (powerline-make side
                  "%b"
                  color1 color2))
(defun powerline-buffer-size
  (side color1 &optional color2)
  (powerline-make side
                  "%I"
                  color1 color2))
(defun powerline-rmw
  (side color1 &optional color2)
  (powerline-make side
                  "%*"
                  color1 color2))
(defun powerline-major-mode
  (side color1 &optional color2)
  (powerline-make side
                  "%[%m%]"
                  color1 color2))
(defun powerline-minor-modes
  (side color1 &optional color2)
  (powerline-make side
                  (substring (format-mode-line minor-mode-alist) 1)
                  color1 color2))
(defun powerline-percent
  (side color1 &optional color2)
  (powerline-make side
                  "%6p"
                  color1 color2))
(defun powerline-row
  (side color1 &optional color2)
  (powerline-make side
                  "%4l"
                  color1 color2))
(defun powerline-column
  (side color1 &optional color2)
  (powerline-make side
                  "%3c"
                  color1 color2))
(defun powerline-narrow
  (side color1 &optional color2)
  (powerline-make side
                  "%n"
                  color1 color2))
(defun powerline-emacsclient
  (side color1 &optional color2)
  (powerline-make side
                  mode-line-client
                  color1 color2))
(defun powerline-vc
  (side color1 &optional color2)
  (if (buffer-file-name (current-buffer))
      (powerline-make side
                      (vc-mode-line (buffer-file-name (current-buffer)))
                      color1 color2)
    ""))
(setq-default mode-line-format
              (list '(:eval (powerline-rmw            'left   nil  ))
                    '(:eval (powerline-buffer-size    'left   nil  ))
                    '(:eval (powerline-buffer-name    'left   nil  powerline-color1  ))
                    '(:eval (powerline-major-mode     'left        powerline-color1  ))
                    '(:eval (powerline-minor-modes    'left        powerline-color1  powerline-color2  ))
                    '(:eval (powerline-narrow         'center                        powerline-color2  ))
                    '(:eval (powerline-vc             'center                        powerline-color2  ))
                    '(:eval (powerline-make-fill                                     powerline-color2  ))
                    '(:eval (powerline-percent        'right       powerline-color1  powerline-color2  ))
                    '(:eval (powerline-row            'right  nil  powerline-color1))
                    '(:eval (powerline-text ":" nil))
                    '(:eval (powerline-column         'text   nil  ))))

(provide 'powerline)
