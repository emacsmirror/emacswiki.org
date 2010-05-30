;;;  This package can highlight variable and function call and others in c/emacs, make life easy
;; to enable this package, add this two lines into your .emacs:
;; (require 'zjl-hl)
;; (zjl-hl-enable-global-all-modes);(zjl-hl-disable-global-all-modes)

;;;###autoload
;;; begin lisp code
(require 'highlight)
(require 'region-list-edit)

;;;###autoload
(defcustom zjl-hl-make-fun-call-notable  t
  "enlarge font of called function, so that become notable"
  :type 'boolean :group 'zjl-hl)
(defcustom zjl-hl-fun-call-notable-degree  1.2
  "Control the font size of function call"
  :type 'boolean :group 'zjl-hl)
;;;###autoload
(defcustom zjl-hl-c-mode-enable-flag t
  "Enable c mode highlight when zjl-hl-enable-global-all is called"
  :type 'boolean :group 'zjl-hl)
;;;###autoload
(defcustom zjl-hl-c++-mode-enable-flag nil
  "Enable c++ mode highlight when zjl-hl-enable-global-all is called.
Currently only c style file but named as *.cpp is supported"
  :type 'boolean :group 'zjl-hl)
(defcustom zjl-hl-elisp-mode-enable-flag t
  "Enable emacs lisp mode highlight when zjl-hl-enable-global-all is called."
  :type 'boolean :group 'zjl-hl)

;;;###autoload
(defcustom zjl-hl-normal-size 40000
  "The size of erea that zjl-hl can highlight without any delay.
You can improve this if your computer has enough performance."
  :type 'integer :group 'zjl-hl)
;;;###autoload
(defcustom zjl-hl-toobig-size 10000000
  "The threshold size of function that zjl-hl will stop to highlight since it is too big. The size corresponds to the largest function found in current screen and
+-zjl-hl-numberofscreen-to-hl-each-time screens"
  :type 'integer :group 'zjl-hl)
;;;###autoload
(defcustom zjl-hl-toobig-not-update-size 1000000
  "The size of function that zjl-chl will stop to  highlight when the function is modified.
the function means those that inculded in current screen and +-zjl-hl-numberofscreen-to-hl-each-time
screens"
  :type 'integer :group 'zjl-hl)
;;;###autoload
(defcustom zjl-hl-numberofscreen-to-hl-each-time 2
  "The number of screens around current screen to highlight every time.
This variable is define for:
I use idle timer delay to begin highlight current screen when user stop to scroll screen
(so as to have no delay when scroll),but this cause the highlight happen delay 0.5sec
after we stop scroll screen, and this not feels so good. The way to void this(in some degree)
is highlighting [-zjl-hl-numberofscreen-to-hl-each-time +zjl-hl-numberofscreen-to-hl-each-time]
screens for each time zjl-hl work"
    :type 'integer :group 'zjl-hl)

;;;###autoload
(defcustom zjl-hl-firstscreen-hl-toggle nil
  "When not nil and when you open a new buffer, hl buffer before it shown on window.
this will cause delay that feel uncomfortable.Don't enable this unless your computer has
enough performance."
  :type 'boolean :group 'zjl-hl)

;;variable that use to add/remove timer
(setq zjl-hl-timer-obj nil)

(defface zjl-hl-font-lock-bracket-face
  '((((class color)
      (background dark))
     (:foreground "firebrick3" :weight bold))
    (((class color)
      (background light))
     (:foreground "firebrick3"))
    (t
     ()))
  "*Font lock mode face for brackets, e.g. '(', ']', etc."
  :group 'zjl-hl-faces)
(defvar zjl-hl-font-lock-bracket-face 'zjl-hl-font-lock-bracket-face
  "Font lock mode face for backets.  Changing this directly
  affects only new buffers.")

(defface zjl-hl-operators-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "DarkGoldenrod4"))
    (t
     ()))
  "*Font lock mode face for operater, e.g. '+', '-', etc."
  :group 'zjl-hl-faces)
(defvar zjl-hl-operators-face 'zjl-hl-operators-face)

(defface zjl-hl-member-reference-face
  '((((class color)
      (background dark))
     (:foreground "#f4a957"))
    (((class color)
      (background light))
     (:foreground "#008000"))
    (t
     ()))
  "*Font lock mode face for the struct member reference, e.g. b in \"a->b\"."
  :group 'zjl-hl-faces)
(defvar zjl-hl-member-reference-face 'zjl-hl-member-reference-face)

(defface zjl-hl-function-call-face
  '((((class color)
      (background dark))
     (:foreground "#e566d7" :weight normal))
    (((class color)
      (background light))
     (:foreground "#008000" :bold t))
    (t
     ()))
  "*Font lock mode face for functioin calling."
  :group 'zjl-hl-faces)
(defvar zjl-hl-function-call-face 'zjl-hl-function-call-face)
;; (defvar zjl-hl-function-call-face 'font-lock-function-name-face)

(defface zjl-hl-local-variable-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod"))
    (((class color)
      (background light))
     (:foreground "#008080"))
    (t
     ()))
  "*Font lock mode face for variable in function body."
  :group 'zjl-hl-faces)
(defvar zjl-hl-local-variable-reference-face 'zjl-hl-local-variable-reference-face)
;; (defvar zjl-hl-local-variable-reference-face 'font-lock-variable-name-face)

(defface zjl-hl-parameters-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod" :weight bold))
    (((class color)
      (background light))
     (:foreground "#008080" :bold t))
    (t
     ()))
  "*Font lock mode face for parameters in function body"
  :group 'zjl-hl-faces)
(defvar zjl-hl-parameters-reference-face 'zjl-hl-parameters-reference-face)

(defface zjl-hl-number-face
  '((((class color)
      (background dark))
     (:foreground "#eeeeec"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t
     ()))
  "*Font lock mode face for number, e.g. \"0xf5\" \"0.5\" \"234\", etc."
  :group 'zjl-hl-faces)
(defvar zjl-hl-number-face 'zjl-hl-number-face)


(defface zjl-hl-member-point-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "SpringGreen4"))
    (t
     ()))
  "*Font lock mode face for \"->\"."
  :group 'zjl-hl-faces)
(defvar zjl-hl-member-point-face 'zjl-hl-member-point-face)

;;comment should be #ff00000 for good look.

(setq zjl-hl-operators-regexp
      (regexp-opt '("+" "-" "*" "/" "%" "!"
                    "&" "^" "~" "|"
                    "=" "<" ">"
                    "." "," ";" ":")))

(setq zjl-hl-brackets-regexp
      (regexp-opt '("(" ")" "[" "]" "{" "}")))

(setq zjl-hl-types-regexp
      (concat
       "\\_<[_a-zA-Z][_a-zA-Z0-9]*_t\\_>" "\\|"       
      (concat "\\_<" (regexp-opt '("unsigned" "int" "char" "float" "void" "UINT8" "UINT16" "UINT32") 'words) "\\_>")))

(setq zjl-hl-warning-words-regexp
      (concat "\\_<" (regexp-opt '("FIXME" "TODO" "BUG" "XXX" "DEBUG")) "\\_>"))

(setq zjl-hl-c-mode-keywords (list
       '("->"
          0  zjl-hl-member-point-face keep) ;;should put before the c-operators
       (cons zjl-hl-operators-regexp (cons 0  '(zjl-hl-operators-face keep)))       
       (cons zjl-hl-brackets-regexp 'zjl-hl-font-lock-bracket-face)
       (cons zjl-hl-types-regexp 'font-lock-type-face)
       (cons zjl-hl-warning-words-regexp 'font-lock-warning-face)
       '("\\(\\_<\\(?:\\(?:0x[0-9a-fA-F]*\\)\\|\\(?:[0-9]+\\(\\.[0-9]+\\)?\\)\\|\\(?:0[0-7]*\\)\\|\\(?:[01]+b\\)\\)\\_>\\)"
        0  zjl-hl-number-face keep)
       '("\\(?:\\.\\|->\\)\\(\\_<\\(?:\\w\\|\\s_\\)+\\_>\\)"
         1  zjl-hl-member-reference-face keep)
       '("\\(\\_<\\(\\w\\|\\s_\\)+\\_>\\)[ 	]*("
         1  zjl-hl-function-call-face keep)
       ))

;;; begin lisp code
;;DarkGoldenrod4
;;SaddleBrown
;;RosyBrown
(defface zjl-hl-elisp-function-call-face
  '((((class color)
      (background dark))
     (:foreground "darkgreen"))
    (((class color)
      (background light))
     (:foreground "firebrick"))
    (t
     ()))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-hl-faces)
(defvar zjl-hl-elisp-function-call-face 'zjl-hl-elisp-function-call-face)
;; (defvar zjl-hl-elisp-function-call-face 'font-lock-function-name-face)
;;(setq zjl-hl-elisp-function-call-face 'zjl-hl-elisp-function-call-face9)


(defface zjl-hl-elisp-setq-face
  '((((class color)
      (background dark))
     (:foreground "dark slate grey"))
    (((class color)
      (background light))
     (:foreground "RosyBrown"))
    (t
     ()))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-hl-faces)
(defvar zjl-hl-elisp-setq-face 'zjl-hl-elisp-setq-face)
;(setq zjl-hl-elisp-setq-face 'zjl-hl-elisp-setq-face6)

(defface zjl-hl-elisp-number-face
  '((((class color)
      (background dark))
     (:foreground "red"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t
     ()))
  "*Face used for link privilege indicator (l) in dired buffers."
  :group 'zjl-hl-faces)
(defvar zjl-hl-elisp-number-face 'zjl-hl-elisp-number-face)

(setq zjl-hl-emacs-lisp-mode-keywords '(("\\(\\_<\\(?:\\(?:0x[0-9a-fA-F]*\\)\\|\\(?:[0-9]+\\(\\.[0-9]+\\)?\\)\\|\\(?:0[0-7]*\\)\\|\\(?:[01]+b\\)\\|nil\\|t\\)\\_>\\)"
          0  zjl-hl-elisp-number-face keep)
                             ("\\(\\_<setq\\_>\\)" 0  zjl-hl-elisp-setq-face keep)                             
                             ("([ 	]*\\(\\_<\\(?:\\w\\|\\s_\\)+\\_>\\)" 1  zjl-hl-elisp-function-call-face keep)
                             ))


;;;###autoload
(defun zjl-hl-local-variable-and-parameter-in-func-region (start end )
    (save-excursion
      (let* ((local-variable-list '())
             (parameter-list '())                
             (func-body-begin (funcall zjl-hl-get-body-begin start end))
             (case-fold-search nil)
             items
             item
             (successful t))
        (condition-case nil         
          (when func-body-begin                               
            (goto-char func-body-begin)
            (setq items (semantic-get-local-variables))
            (unless items
              (semantic-parse-region start end);;force reparse, idea from ecb-method
              (setq items (semantic-get-local-variables)))
            (when items
              (dolist (item items)
                (setq local-variable-list (cons (car item) local-variable-list))))
            (setq items (semantic-get-local-arguments))
            (unless items
              (semantic-parse-region start end);;force reparse, idea from ecb-method
              (setq items (semantic-get-local-arguments)))
            (when items
              (dolist (item items)
                (setq parameter-list (cons (car item) parameter-list))))
            (save-excursion
              (when parameter-list ;; should before local-variale-list since some parameter will override by local-variable
                (case major-mode
                  ('c-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-hl-parameters-reference-face t t t))
                  ('c++-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-hl-parameters-reference-face t t t))
                  ('emacs-lisp-mode (zjl-hl-symbol-region start end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-hl-parameters-reference-face t t t))))
              (when local-variable-list
                (case major-mode
                  ('c-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-hl-local-variable-reference-face t t t))
                  ('c++-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-hl-local-variable-reference-face t t t))
                  ('emacs-lisp-mode
                   ;;(zjl-hl-symbol-region start end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  'default t t t)
                   (zjl-hl-symbol-region start end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-hl-local-variable-reference-face t t t))))              
              ))
        (error (setq successful nil)))
        successful)))

;;;###autoload
(defun zjl-hl-symbol-region (start end symbol-exp symbol-face override-c-mode override-my override-nil)
  (let (target-this-end target-next-start (case-fold-search nil) pos-pair)
    (save-excursion
      (goto-char start)
      (while   (< (point) end)
        (setq pos-pair (funcall zjl-hl-next-comment-pos (point) end))
        (if pos-pair
            (progn
              (setq target-this-end (car pos-pair))
              (setq target-next-start (cdr pos-pair)))
          (setq target-this-end end)
          (setq target-next-start end))
        (if override-c-mode
            (progn
              (hlt-highlight-regexp-region (point) target-this-end symbol-exp symbol-face)
              (goto-char target-next-start))
          (while (and (re-search-forward symbol-exp target-this-end t)
                      (save-excursion
                        (re-search-backward symbol-exp)
                        (not (looking-back "\\.\\|->"))))
            (backward-char)
            (when (or 
                   (and override-my
                        (or (equal (get-char-property (point) 'face) 'zjl-hl-parameters-reference-face)
                            (equal (get-char-property (point) 'face) 'zjl-hl-local-variable-reference-face)
                            (not (zjl-hl-what-face))))
                   (and override-nil
                        (not (zjl-hl-what-face))))
              (let ((symbol-boundaries (bounds-of-thing-at-point 'symbol)))
                (hlt-highlight-regexp-region (car symbol-boundaries) (cdr symbol-boundaries) ".*" symbol-face)))
            (forward-char))
          (goto-char target-next-start))))))

;;;###autoload
(defun zjl-hl-find-regexp-goto-end (start end regexp);;if find, return point after the re-search, else return nil
  (let (target-this-end target-next-start (case-fold-search nil) find-p pos-pair)
    (save-excursion
      (goto-char start)
      (while   (< (point) end)
        (setq pos-pair (funcall zjl-hl-next-comment-pos (point) end))
        (if pos-pair
            (progn
              (setq target-this-end (car pos-pair))
              (setq target-next-start (cdr pos-pair)))
          (setq target-this-end end)
          (setq target-next-start end))
        (setq find-p (re-search-forward regexp target-this-end t))
        (if find-p
            (progn  (setq find-p (point))
                    (goto-char end))
          (goto-char target-next-start))))
        find-p))


;;;###autoload
(defun zjl-hl-local-variable-and-parameter-region (start end)
  (save-excursion
    (hlt-unhighlight-region-for-face zjl-hl-parameters-reference-face start end)
    (hlt-unhighlight-region-for-face zjl-hl-local-variable-reference-face start end)
    
    (let ((regions (funcall zjl-hl-find-hl-var-arg-regions start end))
          (successful t))
      (dolist (each-region regions)
        (setq  successful (and (zjl-hl-local-variable-and-parameter-in-func-region (car each-region) (cdr each-region))
                               successful)))
      successful)
    ))

;;borrow from http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
;;;###autoload
(defun zjl-hl-what-face ()
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    face))


;;;###autoload
(defun zjl-hl-func-too-big-stop-reset(thisbuffer)
  (save-excursion
    (when (buffer-live-p thisbuffer)
      (set-buffer thisbuffer)
      (setq zjl-hl-func-too-big-stop 1)
      (setq zjl-hl-func-too-big-interval 1)
      (setq zjl-hl-func-too-big-timer-registed nil))))

;;;###autoload
(defun zjl-hl-timer-do-every-time (thisbuffer)
  (condition-case nil
      (save-excursion
        (when (buffer-live-p thisbuffer)
          (with-current-buffer thisbuffer
            (when (get-buffer-window)
              (let(start end temp-regions temptime (all-regions-successful t) win-s win-e)
                ;;current-screen not include  window-end this point, but it is ok to include it from hl.
                (if (and (setq win-s (window-start))
                         (setq win-e (window-end)))
                    (progn
                      (setq start (copy-marker (max (point-min);; hl eary around screen window
                                                    (- win-s
                                                       (* zjl-hl-numberofscreen-to-hl-each-time (- win-e win-s))))))
                      (setq end (copy-marker (min (point-max)
                                                  (+ win-e
                                                     (* zjl-hl-numberofscreen-to-hl-each-time (- win-e win-s)))))))
                  (progn
                    (setq end (point-max-marker))
                    (setq start (point-min-marker))))

                (setq start (save-excursion
                              (goto-char start)
                              (if (beginning-of-defun) 
                                  (point-marker)
                                start)))
                (setq end (save-excursion
                            (goto-char end)
                            (if (let ((orginal (point)));;for end-of-defun's bug,  it return nil whenever it is called
                                  (end-of-defun)
                                  (/= (point) orginal))
                                (point-marker)
                              end)))
                (when (and (< (- end start) zjl-hl-toobig-size)
                           (or (< (- end start) zjl-hl-normal-size)
                               (< zjl-hl-func-too-big-stop 2)))
                  (setq temp-regions (list (cons start end)))
                  (dolist (hl-region zjl-hl-regions-already-hl)
                    (setq temp-regions (region-list-edit-delete temp-regions hl-region)))
                  (when temp-regions
                    (dolist (each-region temp-regions)
                      (if (zjl-hl-local-variable-and-parameter-region (car each-region) (cdr each-region))
                          (setq zjl-hl-regions-already-hl (region-list-edit-add zjl-hl-regions-already-hl each-region t)) ;; t means merge hl-regions
                        (setq all-regions-successful nil)))

                    (when all-regions-successful
                      (when (and (> (- end start) zjl-hl-normal-size)
                                 (< zjl-hl-func-too-big-stop 2))          
                        (setq zjl-hl-func-too-big-stop (1+ zjl-hl-func-too-big-stop)))

                      (when (and (> (- end start) zjl-hl-normal-size)
                                 (<= zjl-hl-func-too-big-stop 2))
                        (setq temptime  (/ (- end start) (/ zjl-hl-normal-size 8)))
                        (setq zjl-hl-func-too-big-interval (max temptime zjl-hl-func-too-big-interval)))
                      (when (and (equal zjl-hl-func-too-big-stop 2)
                                 (not zjl-hl-func-too-big-timer-registed))
                        (run-with-idle-timer zjl-hl-func-too-big-interval nil 'zjl-hl-func-too-big-stop-reset (current-buffer))
                        (setq zjl-hl-func-too-big-timer-registed t)))))))
            ))
        )        (error nil))
  )

;;;###autoload
(defun zjl-hl-window-scroll-hook(par1 par2)
  (save-excursion
    (when (or (equal major-mode 'c-mode) ;;if major-mode changed,this hook may still running???
              (equal major-mode 'c++-mode)
              (equal major-mode 'emacs-lisp-mode))
      (when zjl-hl-firsttime
        (add-hook 'semantic-after-partial-cache-change-hook 'zjl-hl-semantic-after-partial-cache-change-hook t t)
        (setq zjl-hl-firsttime nil))

      ;;this should before if, to canel possbile hl timer, before zjl-hl-timer-do-every-time realy begin
      (when zjl-hl-timer-obj
        (cancel-timer zjl-hl-timer-obj))
      
      (if zjl-hl-firsttime-need-hl
          (progn (zjl-hl-timer-do-every-time (current-buffer))
                 (setq zjl-hl-firsttime-need-hl nil))
        
        (setq zjl-hl-timer-obj (run-with-idle-timer 0.5 nil 'zjl-hl-timer-do-every-time (current-buffer)))
        ))))

;;;###autoload
(defun zjl-hl-init ()
  (save-excursion
      (let (start end)
        (make-local-variable 'zjl-hl-func-too-big-stop)
        (make-local-variable 'zjl-hl-func-too-big-interval)
        (make-local-variable 'zjl-hl-func-too-big-timer-registed)
        (make-local-variable 'zjl-hl-firsttime)
        (make-local-variable 'zjl-hl-firsttime-need-hl)
        (make-local-variable 'zjl-hl-regions-already-hl)
        
        (setq zjl-hl-regions-already-hl nil)
        (setq zjl-hl-func-too-big-timer-registed nil)
        (setq zjl-hl-func-too-big-stop 0)
        (setq zjl-hl-func-too-big-interval 1)
        (setq zjl-hl-firsttime t)
        
        (make-local-variable 'zjl-hl-find-hl-var-arg-regions)
        (setq zjl-hl-find-hl-var-arg-regions
              (case major-mode
                ('c-mode 'zjl-hl-find-hl-var-arg-regions-c)
                ('c++-mode 'zjl-hl-find-hl-var-arg-regions-c)
                ('emacs-lisp-mode 'zjl-hl-find-hl-var-arg-regions-elisp)))

        (make-local-variable 'zjl-hl-next-comment-pos)
        (setq zjl-hl-next-comment-pos
              (case major-mode
                ('c-mode 'zjl-hl-next-comment-pos-c)
                ('c++-mode 'zjl-hl-next-comment-pos-c)
                ('emacs-lisp-mode 'zjl-hl-next-comment-pos-elisp)))

        (make-local-variable 'zjl-hl-get-body-begin)
        (setq zjl-hl-get-body-begin
              (case major-mode
                ('c-mode 'zjl-hl-get-body-begin-c)
                ('c++-mode 'zjl-hl-get-body-begin-c)
                ('emacs-lisp-mode 'zjl-hl-get-body-begin-elisp)))

        
        (setq zjl-hl-firsttime-need-hl nil)
        
        (when zjl-hl-firstscreen-hl-toggle
          (setq start (copy-marker (max (point-min);; hl early around screen window
                         (- (window-start)
                                         (* zjl-hl-numberofscreen-to-hl-each-time (- (window-end) (window-start)))))))
        (setq end (copy-marker (min (point-max)
                       (+ (window-end)
                                       (* zjl-hl-numberofscreen-to-hl-each-time (- (window-end) (window-start)))))))
        (if (< (- end start) zjl-hl-normal-size)
            (setq zjl-hl-firsttime-need-hl t)
          (setq zjl-hl-firsttime-need-hl nil)))
        
        (add-hook 'window-scroll-functions 'zjl-hl-window-scroll-hook t t))))


;idea from how ecb update method buffer
;;(add-hook (ecb--semantic-after-partial-cache-change-hook) 'ecb-update-after-partial-reparse t)
;;(add-hook (ecb--semantic-after-toplevel-cache-change-hook) 'ecb-rebuild-methods-buffer-with-tagcache t)
;;;###autoload
(defun zjl-hl-semantic-after-partial-cache-change-hook (updated-tags)
  (when zjl-hl-regions-already-hl  ;;maybe zjl-hl-init has not hl any where
    (let (start end overlay todo)
      (dolist (tags updated-tags)
        (setq overlay (semantic-tag-overlay tags))
        (setq todo (cons (overlay-start overlay) (overlay-end overlay)))
        (setq zjl-hl-regions-already-hl (region-list-edit-delete zjl-hl-regions-already-hl todo))
        )
      (when (< (- (overlay-end overlay) (overlay-start overlay)) zjl-hl-toobig-not-update-size)
        (zjl-hl-window-scroll-hook 1 1)))))

(setq zjl-hl-fun-call-notable-degree-old-value nil)
;;;###autoload
(defun zjl-hl-enable-global-all-modes ()
  (interactive)
  (when zjl-hl-make-fun-call-notable
    (setq zjl-hl-fun-call-notable-degree-old-value (face-attribute 'font-lock-function-name-face :height))
    (set-face-attribute 'font-lock-function-name-face nil
                    :height zjl-hl-fun-call-notable-degree))
    (when zjl-hl-c-mode-enable-flag
    (zjl-hl-enable-global 'c-mode))
  (when zjl-hl-c++-mode-enable-flag
    (zjl-hl-enable-global 'c++-mode))
  (when zjl-hl-elisp-mode-enable-flag
    (zjl-hl-enable-global 'emacs-lisp-mode))
)

;;;###autoload
(defun zjl-hl-disable-global-all-modes ()
  (interactive)
  (when (and zjl-hl-make-fun-call-notable
             zjl-hl-fun-call-notable-degree-old-value)
    (set-face-attribute 'font-lock-function-name-face nil
                    :height zjl-hl-fun-call-notable-degree-old-value))
  (when zjl-hl-c-mode-enable-flag
    (zjl-hl-disable-global 'c-mode))
  (when zjl-hl-c++-mode-enable-flag
     (zjl-hl-disable-global 'c++-mode))
  (when zjl-hl-elisp-mode-enable-flag
    (zjl-hl-disable-global 'emacs-lisp-mode)))

;;;###autoload
(defun zjl-hl-enable-global (mode)
  (let ((mode-name (symbol-name mode)) hook keywords)
    (setq keywords (intern-soft (concat "zjl-hl-" mode-name "-keywords")))
    (font-lock-add-keywords mode (symbol-value keywords) 1)
    (setq hook (intern-soft (concat mode-name "-hook")))
    (add-hook hook 'zjl-hl-init)))

;;;###autoload
(defun zjl-hl-disable-global (mode)
  (let ((mode-name (symbol-name mode)) hook keywords)
    (setq keywords (intern-soft (concat "zjl-hl-" mode-name "-keywords")))
    (font-lock-remove-keywords mode (symbol-value keywords))
    (setq hook (intern-soft (concat mode-name "-hook")))
    (remove-hook hook 'zjl-hl-init)))


;;;###autoload
(defun zjl-hl-get-body-begin-c (start end);;if has no begin, return nil, (zjl-hl-find-regexp-goto-end) may return nil
(save-excursion
  (goto-char start)
  (zjl-hl-find-regexp-goto-end start end "{")))

;;;###autoload
(defun zjl-hl-get-body-begin-elisp (start end);;if has no begin, return nil
  (save-excursion
    (let (pos1 pos2 letfinded)
      (goto-char start)
      (if (looking-at-p "(let\\(\\*\\)?\\_>")
          (progn (setq letfinded (re-search-forward "let\\(\\*\\)?\\_>" end t))
                 (if (setq pos1 (and letfinded
                                     (zjl-hl-find-regexp-goto-end letfinded end "(")));; (zjl-hl-find-regexp-goto-end) may return nil
                     (progn
                       (setq pos1 (1- pos1))
                       (goto-char pos1)
                       (forward-sexp)
                       (setq pos2 (point))
                       (if (/= pos1 pos2)
                           pos2
                         nil))
                   nil))
        (when (looking-at-p "(defun")
          (forward-char)
          (setq pos2 (point)))))))

;;;###autoload
(defun zjl-hl-find-hl-var-arg-regions-c (start end)
  (save-excursion
    (goto-char start)
    (let (defun-start defun-end regions)
      (while (and (setq defun-end 
                        (if (let ((orginal (point)));;for end-of-defun's bug,  it return nil whenever it is called
                              (end-of-defun)
                              (/= (point) orginal))
                            (point)
                          nil))
                  (setq defun-start
                        (if (beginning-of-defun)
                            (point)
                          nil))
                  (< defun-start end))
        (setq regions  (cons (cons defun-start defun-end) regions))
        (goto-char defun-end))
      regions)))

;; (progn (re-search-forward  "([ 	]*let" nil t)
;;        (goto-char (match-beginning 0)))

;;;###autoload
(defun zjl-hl-find-hl-var-arg-regions-elisp (start end)
  (save-excursion
    (goto-char start)
    (let (defun-start defun-end regions)
      (while (and (setq defun-end 
                        (if (let ((orginal (point)));;for end-of-defun's bug,  it return nil whenever it is called
                              (end-of-defun)
                              (/= (point) orginal))
                            (point)
                          nil))
                  (setq defun-start
                        (if (beginning-of-defun)
                            (point)
                          nil))
                  (< defun-start end))
        (setq regions  (cons (cons defun-start defun-end) regions))
        (goto-char defun-start)
        (let (let-start let-end)
          (while (and (setq let-start (zjl-hl-find-regexp-goto-end (point) defun-end "(let\\(\\*\\)?\\_>"))
                      (progn (goto-char let-start)
                             (re-search-backward "(let" nil t)
                             (setq let-start (point))
                             )
                        (<= let-start defun-end)
                      )
            (forward-sexp);;this function do not return t even if it forward successfully
            (setq let-end (point))
            (when (/= let-end let-start)
              (setq regions  (cons (cons let-start let-end) regions))
              (goto-char (+ 2 let-start)))))
        (goto-char defun-end))
      (setq regions (nreverse regions));;before reverse, the list is  let-in-b fun-b let-in-a fun-a,  so the global argument will override the local variable, which we dont wish.
      regions)))


;;;###autoload
(defun zjl-hl-next-comment-pos-c (start end)
  (save-excursion
    (let (next-comment-start next-comment-exist comment-pos-1 comment-pos-2 min-pos pos-pair)
      (goto-char start)
      (save-excursion
        (if (re-search-forward "/\\*" end t)
            (progn 
              (setq comment-pos-1 (point))
              (setq next-comment-exist t))
          (setq comment-pos-1 end)))
      (save-excursion 
        (if (re-search-forward "//" end t)
            (progn
              (setq comment-pos-2 (point))
              (setq next-comment-exist t))
          (setq comment-pos-2 end)))
      (when next-comment-exist
        (setq min-pos (min comment-pos-1 comment-pos-2))
        (setq next-comment-start (- min-pos 2))
        (cond
         ((equal comment-pos-2 min-pos) 
          (goto-char min-pos)
          (end-of-line))
         ((equal comment-pos-1 min-pos)
          (unless (re-search-forward "\\*/" end t)
            (goto-char end))))
        (setq pos-pair (cons next-comment-start (point)))
        )
      pos-pair)))

;;;###autoload
(defun zjl-hl-next-comment-pos-elisp (start end)
  (save-excursion
    (let (next-comment-start next-comment-exist comment-pos-1 comment-pos-2 min-pos pos-pair)
      (goto-char start)
      (save-excursion
        (if (re-search-forward "\\([^\\\\]\"\\)\\|\\(^\"\\)" end t);;?\"  or "weatherinfo\":{", in some package 
            (progn 
              (setq comment-pos-1 (point))
              (setq next-comment-exist t))
          (setq comment-pos-1 end)))
      (save-excursion 
        (if (re-search-forward ";" end t)
            (progn
              (setq comment-pos-2 (point))
              (setq next-comment-exist t))
          (setq comment-pos-2 end)))
      (when next-comment-exist
        (setq min-pos (min comment-pos-1 comment-pos-2))
        (setq next-comment-start (- min-pos 1))
        (cond
         ((equal comment-pos-2 min-pos) 
          (goto-char min-pos)
          (end-of-line))
         ((equal comment-pos-1 min-pos)
          (goto-char next-comment-start)
          (forward-sexp)))
        (setq pos-pair (cons next-comment-start (point)))
        )
      pos-pair)))

(provide 'zjl-hl)
;;; end lisp code 
