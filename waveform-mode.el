;;; 
;-*- coding: utf-8 -*-
; http://www.emacswiki.org/emacs/WaveformMode
; waveform-mode by Zhu, Shenli
; 
; (l)ow, m(i)ddle, (h)igh, repea(t), (s)pace, s(p)lit

; auto style (for synchronized digital circuit)
; Text according to marker, marker($m) will put text center align
; (m)arker, l(e)ft, (r)ight

;; TODO list
; make current buffer back to .wf buffer instead of *Waveform*
; make "Waveform" buffer read only

(defconst wf_display_buffer "*Waveform*"
  "Buffer to display generated waveform.")

(defvar wf_input_buffer nil)
(defvar wf_clk_name nil)
(defvar wf_marker_name "M")
(defvar wf_marker_pos nil)
(defvar wf_marker_nr nil)
(defvar wf_clk_width nil)

(defvar wf-mode-map
       (let ((map (make-sparse-keymap))
             (menu-map (make-sparse-keymap)))
         (define-key map (kbd "C-M-u") 'wf-update)
         (define-key map [menu-bar waveform-mode] (cons "Waveform" menu-map))
         (define-key menu-map [wu]
           '(menu-item "Generate waveform" wf-update
                       :help "update and generate waveform"))
         map)
       "Keymap for `waveform-mode'")

; (describe-face)
(setq wf-font-lock-defaults
      '(("^[a-zA-Z][0-9a-zA-Z_-]*\\(\\[[0-9]+:?[0-9]*\\]\\)?"
         . font-lock-function-name-face)
        ("\\$[0-9]*[lhtsp]" . font-lock-type-face)
        ("\\$[0-9]*i" . font-lock-variable-name-face)
        ("@[ ]*([ ]*posedge[ ]*\\(.*?\\))[ ]*$" . font-lock-keyword-face)
        ("\\$[0-9]+[mer]" . font-lock-keyword-face)
        (" [_⎺\\\/]+" . font-lock-type-face)
        ("⎼" . font-lock-variable-name-face)
        ("<\\(.*?>\\)" . (1 highlight))))

(define-derived-mode waveform-mode fundamental-mode 
  "A major mode for drawing digital circuit waveform"
  (use-local-map wf-mode-map)
  (set (make-local-variable 'comment-start) "# ")
  (make-local-variable 'wf_input_buffer)
  (setq font-lock-defaults '(wf-font-lock-defaults))
  (setq mode-name "Waveform")
  ;(flyspell-mode -1)
  (auto-fill-mode nil)
  ;; perl style comment: “# …” 
  (modify-syntax-entry ?# "< b" waveform-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" waveform-mode-syntax-table)
   )

;;;###autoload
(defalias 'wfu 'wf-update)

;;;###autoload
(defun wf-update ()
  "update waveform to reflect outside changes to waveform source"
  (interactive)
  
  (assert (eq major-mode 'waveform-mode))
  (setq wf_input_buffer (current-buffer)) ; don't work?
  (let ((wf_input_buffer1 (current-buffer)))
    (when (one-window-p) (split-window))
    (kill-buffer wf_display_buffer)
    (set-buffer (get-buffer-create wf_display_buffer))
    (waveform-mode)
    (barf-if-buffer-read-only)
    (erase-buffer)
    (insert-buffer-substring wf_input_buffer1)
    (when (wf-auto-mode-p wf_display_buffer)
      (progn
        (wf-translate-clk-line wf_display_buffer)
        (wf-gen-mark-line wf_display_buffer)
        (wf-translate-mark wf_display_buffer)
    ;;     (wf-translate-other-auto wf_display_buffer))
    ))
    ;(wf-translate-buf wf_display_buffer)
    ;(setq buffer-read-only t) 
    ;(next-window) ; don't work
    ))  

(defun wf-auto-mode-p (buf)
  "check whether it has @posedge clk, if no, return nil, if yes return
clk"
  (progn
    (beginning-of-buffer)
    (if (re-search-forward "^\\(.+?\\)@[ ]*([ ]*posedge[ ]*\\(.*?\\))[ ]*$")
        (progn
          (setq wf_marker_name (substring-no-properties (match-string 1)))
          (setq wf_clk_name (substring-no-properties (match-string 2)))
          t)
      nil)))

(defun wf-translate-clk-line (buf)
  (set-buffer buf)
  (goto-char (point-min))
  (re-search-forward (concat "^" wf_clk_name))
  (let* ((clk_begin (progn (beginning-of-line) (point)))
         (clk_end (progn (end-of-line) (point)))
         (clk_line (delete-and-extract-region clk_begin clk_end))
         (clk_expand_line 
          (with-temp-buffer
            (insert clk_line)
            (wf-translate-buf (current-buffer))
            ; generate column number of clock edge
            (beginning-of-line)
            (setq wf_marker_pos nil)
            (while (re-search-forward "\/" nil t)
              (if wf_marker_pos
                  ; non-nil
                  (setq wf_marker_pos (append wf_marker_pos (list (-
                                                                   (point) 1))))
                ;nil
                (setq wf_marker_pos (list (- (point) 1)))))
            (buffer-string))))
    (insert clk_expand_line)))  

(defun wf-gen-mark-line (buf)
  (set-buffer buf)
  (goto-char (point-min))
  (re-search-forward (concat "^" wf_marker_name))
  (delete-region (point) (progn (end-of-line) (point)))  
  (let* ((marker_begin (progn (beginning-of-line) (point)))
        (marker_end (progn (end-of-line) (point)))
        (marker_line (delete-and-extract-region marker_begin marker_end))
        (marker_expand_line 
         (with-temp-buffer
           (insert marker_line)
           (setq wf_marker_nr (length wf_marker_pos))
           (let ((n 0))
             (while (< n wf_marker_nr)
               (progn
                 (wf-translate-char "s" (- (elt wf_marker_pos n) (point)))
                 (insert (number-to-string n))
                 (setq n (+ n 1)))))
           (buffer-string))))
    (insert marker_expand_line)))


; case 1: $i$0m
(defconst wf_marker_1 " \\$\\(?1:[hil]\\)\\$\\(?2:[0-9]+\\)\\(?3:m\\)")
; case 2: $0m<pc>$1m
(defconst wf_marker_2 "\\(?:7\\$\\(?1:[0-9]+\\)\\(?2:m\\)\\(?:\\(?:$\\(?3:[hil]\\)\\)\\|\\(?:<\\(?4:.+?\\)>\\)\\)\\$\\(?5:[0-9]+\\)\\(?6:m\\)\\)")
; case 3: $4m$i 
(defconst wf_marker_3 "\\$\\(?1:[0-9]+\\)\\(?2:m\\)\\$\\(?3:[hil]?\\) *?$")

(defconst wf_marker_all (concat "\\(?:" wf_marker_1 "\\)" "\\|"
                                "\\(?:" wf_marker_2 "\\)" "\\|"
                                "\\(?:" wf_marker_3 "\\)"))
(defun wf-translate-mark (buf)
  (set-buffer buf)
  (setq wf_clk_width (- (elt wf_marker_pos 1) (elt wf_marker_pos 0)))
  ; case 1
  (goto-char (point-min))
  (while (re-search-forward wf_marker_1 nil t)
    (let* ((char (match-string 1))
           (marker_nr (match-string 2))
           (str_len (+ (length marker_nr) 4))
           (marker_len (+ (length marker_nr) 2))
           (char_begin (- (point) str_len))
           (char_end (+ char_begin 2)))
      (progn
          ; remove $i
          (delete-region char_begin char_end)
          ; insert char
          (backward-char marker_len)
          (wf-translate-char char (- (- (elt wf_marker_pos
                                (string-to-number marker_nr))
                                (current-column)) 1)))))
  ; case 2
  (goto-char (point-min))
  (while (re-search-forward wf_marker_2 nil t)
    (let* ((left_marker_nr (match-string 1))
           (right_marker_nr (match-string 5))
           (left_marker_len (+ (length left_marker_nr) 2))
           (right_marker_len (+ (length right_marker_nr) 2))
           (marker_distance (- (string-to-number right_marker_nr)
                                (string-to-number left_marker_nr)))
           (char (match-string 3))
           (content (match-string 4))
           (content_len (length content))
           (str_len (if char (+ left_marker_len right_marker_len 2)
                      (+ left_marker_len right_marker_len content_len
                                2))))
      (progn
        ; remove $m
        (backward-char str_len)
        (delete-region (point) (+ (point) left_marker_len))
        ; case <content>
        (when content 
          (let* ((all_spaces (- (* wf_clk_width marker_distance) content_len 2))
                 (left_spaces (/ all_spaces 2))
                 (right_spaces (- all_spaces left_spaces)))
            (progn
              (forward-char 1)
              (wf-translate-char "s" left_spaces)
              (forward-char content_len)
              (wf-translate-char "s" right_spaces))))
        ; case $[hil]
        (when char 
          (progn
            ; remove $[hil]
            (delete-region (point) (+ (point) 2))
            (cond ((string= char "h") (insert 47)) ;/
                  ((string= char "l") (insert 92)) ;\
                  ((string= char "i") (wf-translate-char "i" 1)))
            (wf-translate-char char (- (* wf_clk_width marker_distance) 1))
            )
        ))))
  ; case 3
  (goto-char (point-min))
  (while (re-search-forward wf_marker_3 nil t)
    (let* ((left_marker_nr (string-to-number (match-string 1)))
           (char (match-string 3))
           (end_marker_nr (- (length wf_marker_pos) 1))
           (marker_distance (- end_marker_nr left_marker_nr)))
      (progn
        (backward-char 5)
        (delete-region (point) (+ (point) 5)) ; remove $5m$l
        (cond ((string= char "h") (insert 47)) ;/
              ((string= char "l") (insert 92)) ;\
              ((string= char "i") (wf-translate-char "i" 1)))
        (wf-translate-char char (- (* wf_clk_width marker_distance) 1)))))
  )

       

(defun wf-translate-buf (buf)
  "translate waveform string, normally one per line"
  (progn
    (pop-to-buffer buf)
    (beginning-of-buffer)
    (while
        (re-search-forward "\$\\([0-9]+\\)\\([hmltsp]\\)" nil t)
      (setq times (string-to-number (match-string 1)))
      (setq char (match-string 2))
      (if (string= char "t") ; repeat
          (let ((r_begin (match-beginning 0))
                (r_end (match-end 0)))
            (progn
             (re-search-backward " ")
             (goto-char r_end)
             (dotimes (i (- times 1))
               (insert-buffer-substring (current-buffer)
                                        (+ (match-beginning 0) 1) r_begin))
             (delete-region r_begin r_end)
             ))
        (progn
          (wf-translate-char char times) ; insert replace
          (delete-region (match-beginning 0) (match-end 0)))))))

(defun wf-translate-char (char times)
  "char is h,i,l,p"
  (if (string= char "l") ; low
      (dotimes (i times) (insert "_"))
    (if (string= char "s") ; space
        (dotimes (i times) (insert " "))
      (progn  
        (setq ucode (cond ((string= char "h") "23BA") ; high
                          ((string= char "i") "23BC") ; middle
                          ((string= char "p") "222C"))) ; split
        (ucs-insert ucode times)))))

    
(provide 'waveform-mode)
