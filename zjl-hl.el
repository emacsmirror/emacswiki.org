;;;Updated in 2016.5.27
;;;This package can highlight argument/variable and keywords in c/c++/java
;; Issue now: all disable function can not remove keyword I add, don't know how
(require 'cl)
(require 'highlight)
(require 'region-list-edit)

(defcustom zjl-hl-make-fun-call-noticeable  t
  "enlarge font of called function, so that become noticeable"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-fun-call-noticeable-degree  1.2
  "Control the font size of function call"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-c-mode-enable-flag t
  "Enable c mode highlight when zjl-hl-enable-global-all is called"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-c++-mode-enable-flag nil
  "Enable c++ mode highlight when zjl-hl-enable-global-all is called."
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-java-mode-enable-flag nil
  "Enable Java highlight when zjl-hl-enable-global-all is called."
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-no-delay-max-size 10000
  "The size of erea that zjl-hl can highlight without any delay.
You can improve this if your computer has enough performance."
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-too-big-size 40000
  "The threshold size of function that zjl-hl will stop to highlight since it is too big. The size corresponds to the largest function found in current screen and
+-zjl-hl-numberofscreen-to-hl-each-time screens"
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-too-big-to-update-size 40000
  "If the edited function's size is too large, zjl-hl will stop to do partial highlight"
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-pre-hl-max 10
  "xxx"
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-numberofscreen-to-hl-each-time 1
  "The number of screens around current screen to highlight every time, need because:
   I use idle timer delay to begin highlight current screen when user stop to scroll screen (so as to have no delay when scroll),but this cause the highlight happen delay 0.5sec after we stop scroll screen, and this not feels so good. The way somehow to void this is highlighting [-zjl-hl-numberofscreen-to-hl-each-time +zjl-hl-numberofscreen-to-hl-each-time] screens for each time zjl-hl work"
    :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-semantic-parse-depth 20
  "the deepest subscope zjl-hl try to get semantic parse to get local variable, it is fine to set as high as 20"
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-global-variable-hl nil
  "xxx"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-error-message-print-p t
  "xxx"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-dbg-print-p 'critical
  "whether to print debug info or not"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-firstscreen-hl-toggle nil
  "When not nil and when you open a new buffer, hl buffer before it shown on window.
this will cause delay that feel uncomfortable.Don't enable this unless your computer has
enough performance."
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-delete-else-do-hl nil
  "When not nil and when you open a new buffer, hl buffer before it shown on window.
this will cause delay that feel uncomfortable.Don't enable this unless your computer has
enough performance."
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-generic-macro-enable 10
  "When t, assume symbol as Macro if it is purly composed by capital/number/underline.
When number, also consider symbol as Macro as long as first N chars are capital/number/underline.
When nil, do not apply above two assumptions, most Macro won't be highlighted"
  :type 'integer :group 'zjl-hl)

(setq zjl-hl-timer-obj nil)
(setq zjl-hl-update-screen-timer nil)

(defface zjl-hl-font-lock-bracket-face
  '((((class color)
      (background dark))
     (:foreground "firebrick3" :bold nil :italic nil))
    (((class color)
      (background light))
     (:foreground "firebrick3" :bold nil :italic nil))
    (t()))
  "*Font lock mode face for brackets, e.g. '(', ']', etc."
  :group 'zjl-hl-faces)

(defvar zjl-hl-font-lock-bracket-face 'zjl-hl-font-lock-bracket-face
  "Font lock mode face for backets.  Changing this directly
  affects only new buffers.")

(defface zjl-hl-font-lock-bracket-more-visible-face
  '((((class color)
      (background dark))
     (:foreground "brown1" :bold t :italic nil))
    (((class color)
      (background light))
     (:foreground "brown1" :bold t :italic nil))
    (t()))
  "*Font lock mode face for brackets, e.g. '(', ']', etc."
  :group 'zjl-hl-faces)

(defvar zjl-hl-font-lock-bracket-more-visible-face 'zjl-hl-font-lock-bracket-more-visible-face
  "Font lock mode face for backets.  Changing this directly
  affects only new buffers.")

(defface zjl-hl-operators-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen" :bold nil :italic nil))
    (((class color)
      (background light))
     (:foreground "DarkGoldenrod4" :bold nil :italic nil))
    (t()))
  "*Font lock mode face for operater, e.g. '+', '-', etc."
  :group 'zjl-hl-faces)

(defvar zjl-hl-operators-face 'zjl-hl-operators-face)

(defface zjl-hl-member-reference-face
  '((((class color)
      (background dark))
     (:foreground "#f4a957" :bold nil :italic nil))
    (((class color)
      (background light))
     (:foreground "#008024" :bold nil :italic nil))
    (t()))
  "*Font lock mode face for the struct member reference, e.g. b in \"a->b\"."
  :group 'zjl-hl-faces)

(defvar zjl-hl-member-reference-face 'zjl-hl-member-reference-face)

(defface zjl-hl-function-call-face
  '((((class color)
      (background dark))
     (:foreground "#e566d7" :bold t))
    (((class color)
      (background light))
     (:foreground "#008024" :bold t :italic nil))
    (t()))
  "*Font lock mode face for functioin calling."
  :group 'zjl-hl-faces)
(defvar zjl-hl-function-call-face 'zjl-hl-function-call-face)

(copy-face 'font-lock-type-face 'zjl-hl-template-face)
(defvar zjl-hl-template-face 'zjl-hl-template-face)

(defface zjl-hl-local-variable-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod"))
    (((class color)
      (background light))
     (:foreground "#127974"))
    (t()))
  "*Font lock mode face for variable in function body."
  :group 'zjl-hl-faces)

(defvar zjl-hl-local-variable-reference-face 'zjl-hl-local-variable-reference-face)

(defface zjl-hl-global-variable-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod"))
    (((class color)
      (background light))
     (:foreground "maroon" :bold nil :italic t))
    (t()))
  "*Font lock mode face for variable in function body."
  :group 'zjl-hl-faces)

(defvar zjl-hl-global-variable-reference-face 'zjl-hl-global-variable-reference-face)

(defface zjl-hl-parameters-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod" :bold t))
    (((class color)
      (background light))
     (:foreground "#127974" :bold t :italic nil))
    (t()))
  "*Font lock mode face for parameters in function body"
  :group 'zjl-hl-faces)

(defvar zjl-hl-parameters-reference-face 'zjl-hl-parameters-reference-face)

(defface zjl-hl-number-face
  '((((class color)
      (background dark))
     (:foreground "#eeeeec"))
    (((class color)
      (background light))
     (:foreground "red" :bold nil :italic nil))
    (t()))
  "*Font lock mode face for number, e.g. \"0xf5\" \"0.5\" \"234\", etc."
  :group 'zjl-hl-faces)

(defvar zjl-hl-number-face 'zjl-hl-number-face)

(defface zjl-hl-member-point-face
  '((((class color)
      (background dark))
     (:foreground "PaleGreen"))
    (((class color)
      (background light))
     (:foreground "SpringGreen4" :bold nil :italic nil))
    (t()))
  "*Font lock mode face for \"->\"."
  :group 'zjl-hl-faces)

(defvar zjl-hl-member-point-face 'zjl-hl-member-point-face)

(set-face-attribute 'font-lock-constant-face nil :foreground "red")

(setq zjl-hl-operators-regexp
      (regexp-opt '("+" "-" "*" "/" "%" "!"
                    "&" "^" "~" "|"
                    "=" "<" ">"
                    "." "," ";" ":")))

(setq zjl-hl-brackets-regexp (regexp-opt '("(" ")" "[" "]")))
(setq zjl-hl-brackets-more-visible-regexp (regexp-opt '("{" "}")))

(setq zjl-hl-types-name '("int" "INT" "int8" "int16" "int32" "int64" "INT8" "INT16" "INT32" "INT64" "uint8" "uint16" "uint32" "uint64" "UINT8" "UINT16" "UINT32" "UINT64" "u8" "u16" "u32" "u64" "U8" "U16" "U32" "U64" "long" "short" "char" "CHAR" "float" "FLOAT" "unsigned" "UNSIGNED" "void" "VOID"))

(setq zjl-hl-types-regexp
      (concat
       "\\_<[_a-zA-Z][_a-zA-Z0-9]*_t\\_>" "\\|"       
       (concat "\\_<" (regexp-opt zjl-hl-types-name 'words) "\\_>")))

(setq zjl-hl-warning-words-regexp
      (concat "\\_<" (regexp-opt '("FIXME" "TODO" "HACK" "fixme" "todo" "hack" "BUG" "XXX" "DEBUG")) "\\_>"))

(setq zjl-hl-macro-regexp
      (concat "\\_<\\(" "BIT[0-9]\\|\\(?:0x\\)?[0-9]+\\(?:ULL?\\|ull?\\|llu\\|LLU\\|[uU]\\)\\|SUCCESS\\|ERROR\\|FALSE\\|TRUE\\|IN\\|OUT\\|__\\(func\\|file\\|line\\)__" "\\)\\_>"))

;;;http://emacs.stackexchange.com/questions/7396/how-can-i-determine-which-function-was-called-interactively-in-the-stack
(defun zjl-hl-get-caller-func-name ()
  "Get the current function' caller function name"
  (let* ((index 5)
         (frame (backtrace-frame index))
         (found 0))
    (while (not (equal found 2))
      (setq frame (backtrace-frame (incf index)))
      (when (equal t (first frame)) (incf found)))
    (second frame)))
(defun zjl-hl-call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (incf index))
    (remove-if-not 'car frames)))
(defun zjl-hl-function-stack ()
  "Like call-stack but is a list of only the function names; And this function might cause emacs crash"s
  (butlast (mapcar 'cl-second (zjl-hl-call-stack))))

(defun zjl-hl-dbg-print (str &optional level &rest value)
  (when (or (and (equal zjl-hl-dbg-print-p 'critical)
		 (equal level 'critical))
	    (and (equal zjl-hl-dbg-print-p 'debug)
		 (member level '(critical debug)))
	    (equal zjl-hl-dbg-print-p 'verbose))
    (let (str-text)
      ;; do not use zjl-hl-function-stack with 'debug in mostly, emacs crash because of it
      (setq str-text (format "\n\n=== TIME=%s buffer=%s Elisp-Func=%s Code-Func=%s  %s\n" (substring (current-time-string) 11 19) (buffer-name) (zjl-hl-get-caller-func-name) (which-function) (apply #'format str value)))
      (get-buffer-create "*zjl-hl-dbg-print*")
      (with-current-buffer "*zjl-hl-dbg-print*"
        (goto-char (point-max))
        (insert str-text)
	(let ((beyond (- (point-max) 700000)))
	  (when (> beyond 0)
	    (goto-char (point-min))
	    (delete-region (point-min) (+ (point-min) beyond))
	    (kill-line)))))))
(defun zjl-hl-dbg-print-message (str &rest value)
  (apply #'zjl-hl-dbg-print str 'critical value)
  (apply #'message str value))
(defun zjl-hl-search-member-function-call (limit)
  (let ((pos (re-search-forward "\\(?:\\.\\|->\\)\\(\\_<\\(?:\\w\\|\\s_\\)+\\_>\\)[ 	\n]*[^\\(]" limit t)))
    (when pos
      (goto-char (- pos 1))
      (save-match-data (re-search-backward "\\.\\|->")))))
(setq zjl-hl-c-mode-keywords (list
                              '("->"
                                0  zjl-hl-member-point-face keep) ;;should put before the c-operators
                              (cons zjl-hl-operators-regexp (cons 0  '(zjl-hl-operators-face keep)))       
                              (cons zjl-hl-brackets-regexp 'zjl-hl-font-lock-bracket-face)
                              (cons zjl-hl-brackets-more-visible-regexp 'zjl-hl-font-lock-bracket-more-visible-face)       
                              (cons zjl-hl-types-regexp 'font-lock-type-face)
                              (cons zjl-hl-warning-words-regexp 'font-lock-warning-face)
                              (cons zjl-hl-macro-regexp 'zjl-hl-number-face)
                              '("\\(\\_<\\(?:\\(?:0x[0-9a-fA-F]*\\)\\|\\(?:[0-9]+\\(\\.[0-9]+\\)?\\)\\|\\(?:0[0-7]*\\)\\|\\(?:[01]+b\\)\\)\\_>\\)"
                                0  zjl-hl-number-face keep)
                              '(zjl-hl-search-member-function-call 1  zjl-hl-member-reference-face keep);; this is for case of a.b.c, need this function to let b and c both cover
                              '("\\(?:\\.\\|->\\)\\(\\_<\\(?:\\w\\|\\s_\\)+\\_>\\)[ 	\n]*("
                                1  zjl-hl-function-call-face keep)       
                              '("\\(\\_<\\(\\w\\|\\s_\\)+\\_>\\)[ 	\n]*("
                                1  zjl-hl-function-call-face keep)))

;; Do not forbid the first captial to be number, because there are \\_<[0-9]+ULL?\\_> in kernel code
(cond
 ((equal t zjl-hl-generic-macro-enable)
  (add-to-list 'zjl-hl-c-mode-keywords '("\\_<\\([_A-Z0-9]+\\)\\_>" 
                                         0  zjl-hl-number-face keep) t))
 ((numberp zjl-hl-generic-macro-enable)
  (add-to-list 'zjl-hl-c-mode-keywords (list (concat "\\(" "\\_<\\([_A-Z0-9]+\\)\\_>" "\\|" "\\_<\\([_A-Z0-9]\\{"  (number-to-string zjl-hl-generic-macro-enable) ",\\}[_A-Z0-9a-z]+\\)\\_>" "\\)") 0  'zjl-hl-number-face 'keep) t))
 (t nil)
 )

(setq zjl-hl-c++-mode-keywords zjl-hl-c-mode-keywords)
(add-to-list 'zjl-hl-c++-mode-keywords '("<[_a-zA-Z][_a-zA-Z0-9]*\\(::[_a-zA-Z][_a-zA-Z0-9]*\\)?[ 	]*\\*?>"
                                         0  zjl-hl-template-face keep) t)
(defvaralias 'zjl-hl-java-mode-keywords 'zjl-hl-c-mode-keywords) ;java default deal with template <>, though with different face, I guess its fine

;;Need to ensure the result is all strings!
(defun zjl-hl-get-var-in-semantic-parcel (input &optional argument)
  (interactive)
  (let (result type-str)
    (dolist (item input)
      (when (listp item)
        (if (or (eq (cadr item) 'variable) (and argument (eq (cadr item) 'function)))
            (cond ((listp (car item))
                   (dolist (var-desc (car item))
                     (when (and (listp var-desc) 
                                (stringp (car var-desc))
                                (not (member (car var-desc) zjl-hl-types-name)))
                       (add-to-list 'result (car var-desc)))))
                  ((stringp (car item))
                   (if (and (not (equal "" (car item)))
                            (not (member (car item) zjl-hl-types-name)))
                       (add-to-list 'result (car item))
                     (when (listp (caddr item))
                       (setq type-str (plist-get (caddr item) ':type))
                       (when (and (stringp type-str)
                                  (not (member type-str zjl-hl-types-name)))
                         (add-to-list 'result type-str)))))))))
    result))

;; Default replace-regexp print info in message buffer, so need a custom function
(defun zjl-hl-replace-regexp (regexp to-string &optional delimited start end)
  (save-excursion
      (when start (goto-char start))
      (zjl-hl-dbg-print "while start" 'debug)
      (while (re-search-forward regexp end t)
        (replace-match to-string))
      (zjl-hl-dbg-print "while end" 'debug)))

(defun zjl-hl-get-arg-use-regexp (arg-start arg-end)
  "Two marker as args and return args between two marks as list"
  (let (items item)
    (when (and arg-start arg-end)
      (goto-char arg-start)
      (zjl-hl-replace-regexp  "\\(=.*?\\)\\(,\\|)\\)" "\\2" nil arg-start arg-end)
      (goto-char arg-start)

      (zjl-hl-dbg-print "while start search function pointer" 'debug)
      ;; argument like void * (*func-pointer)(int a, int b),
      (while (re-search-forward "([ 	\n]*\\*[ 	\n]*\\_<\\([_A-Za-z0-9]+\\)\\_>[ 	\n]*)[ 	\n]*(.*?)[ 	\n]*\\(,\\|)\\)" arg-end t)
        (setq item (match-string-no-properties 1))
        (when (not (member item zjl-hl-types-name))
          (add-to-list 'items item)))
      (zjl-hl-dbg-print "while end search function pointer" 'debug)
      
      (zjl-hl-replace-regexp  "\\*\\|\n\\|(.*?)" "" nil arg-start arg-end) ;; remove just incase there is another () inside arg (), and remove * \n.
      (zjl-hl-replace-regexp  "[ 	]\\{2,\\}" " " nil arg-start arg-end)
      (goto-char arg-start)
      (zjl-hl-dbg-print "while start normal argument" 'debug)
      (while (re-search-forward "\\_<\\([_A-Za-z0-9]+\\)\\_>[ 	\n]*\\(,\\|)\\)" arg-end t)
        (setq item (match-string-no-properties 1))
        (when (not (member item zjl-hl-types-name))
          (add-to-list 'items item)))
      (zjl-hl-dbg-print "while end normal argument" 'debug))
    items))

(setq zjl-hl-dbg-profile-func-start-time nil)
(setq zjl-hl-dbg-profile-prev-time nil)
(defun zjl-hl-dbg-profile-start ()
  (when (equal zjl-hl-dbg-print-p 'verbose)
    (setq zjl-hl-dbg-profile-prev-time (float-time))
    (setq zjl-hl-dbg-profile-func-start-time (float-time))))
(defun zjl-hl-dbg-profile-print (str)
  (when (equal zjl-hl-dbg-print-p 'verbose)
    (when zjl-hl-dbg-profile-prev-time
      (let ((temp (- (float-time) zjl-hl-dbg-profile-prev-time)))
        (zjl-hl-dbg-print "zjl-hl-dbg-profile-print: %s %s" 'verbose str temp)))
    (setq zjl-hl-dbg-profile-prev-time (float-time))))
(defun zjl-hl-dbg-profile-end (str)
  (when (equal zjl-hl-dbg-print-p 'verbose)
    (when zjl-hl-dbg-profile-func-start-time
      (let ((temp (- (float-time) zjl-hl-dbg-profile-func-start-time)))
        (zjl-hl-dbg-print "zjl-hl-dbg-profile-end: %s %s" 'verbose str temp)))))

(defun zjl-hl-c-beginning-of-defun ()
  (condition-case err
      (c-beginning-of-defun)
    (error (zjl-hl-dbg-print "%s() call c-beginning-of-defun and got error: %s" 'critical (zjl-hl-get-caller-func-name) (error-message-string err))
           nil)))
(defun zjl-hl-c-end-of-defun ()
  (condition-case err
      (c-end-of-defun)
    (error (zjl-hl-dbg-print "%s() call c-end-of-defun and got error: %s" 'critical (zjl-hl-get-caller-func-name) (error-message-string err))
           nil)))

(defun zjl-hl-do-func-region (start end AB)
  "the two parmeters are marker"
  (condition-case err
      (save-excursion
        (let* ((local-variable-list '())
               (parameter-list '())                
               (func-body-begin (funcall zjl-hl-get-body-begin start end)) ;point, not marker
               (case-fold-search nil)
               items
               items-deepest
               depth
               (successful t)
               (nonterminal (case major-mode
                              ('c-mode 'bovine-inner-scope)
                              ('c++-mode 'bovine-inner-scope)
                              ('java-mode 'field_declaration)))
               (original-mode major-mode)
               (original-buffer (current-buffer))
               )
          (setq zjl-hl-local-variable-list nil)
          (setq zjl-hl-global-variable-list nil)
          (condition-case err
              (progn 
                (case AB ('A (condition-case nil;;use this to limit err impaction
                                 (let (func-body-begin-temp-buffer)
                                   (with-temp-buffer
                                     ;; with-current-buffer (find-file-noselect "/home/xxx/work/test/zjl-hl-test.c")
                                     ;; (erase-buffer)
                                     ;; (setq original-mode 'c++-mode)

                                     (insert-buffer-substring-no-properties original-buffer start end)
                                     (setq func-body-begin-temp-buffer (copy-marker (+ (- func-body-begin (marker-position start)) 1)))
                                     ;; remove all comment
                                     (zjl-hl-replace-regexp  "\\(//.*\\)\\|\\(/\\*\\(?:.\\|\n\\)*?\\*/\\)" "" nil (point-min) (point-max))
				     ;; replace all string as ""
                                     (zjl-hl-replace-regexp  "\"\"\\|\\(\"\\([^\"]\\|\"\\)*?[^\\]\"\\)" "\"\"" nil (point-min) (point-max))
				     (let ((zjl-hl-disable-init-for-temp-buffer t))
				       (funcall (intern-soft (symbol-name original-mode))))

                                     (goto-char (point-min))

				     ;; this let is to simplify all "a = xxxx;" as "a=0"
                                     (let (start-point start-marker stop-marker search-stop-point search-exp-sucess)
				       (zjl-hl-dbg-print "while start replace a = xxxx" 'debug)
                                       (while (setq start-point (zjl-hl-find-regexp-return-end-pos (point) (point-max) "[^!>=<]=[^=]" ))
                                         (goto-char (1- (car start-point)))
                                         (setq start-marker (point-marker))

                                         ;;To deal with "for (i = 0; i !=0 ; i = 0)", because of there is no ; after =
                                         (backward-up-list)
                                         (forward-sexp)
                                         (setq search-stop-point (point))
                                         (goto-char start-marker)

                                         ;;must able to do with "unsigned int best_score =0,score = 0;"
                                         (setq search-exp-sucess nil)
                                         (condition-case nil
                                             (let ((continue t))
					       (zjl-hl-dbg-print "while start searching *,; in variables definition" 'debug)
                                               (while (and continue
                                                           (<= (point) search-stop-point))
                                                 (forward-sexp)
                                                 (when (looking-at "[ 	\n]*[,;]")
                                                   (setq search-exp-sucess t)
                                                   (setq continue nil)))
					       (zjl-hl-dbg-print "while end searching *,; in variables definition" 'debug))
                                           (error (setq search-exp-sucess nil)))
                                         (when search-exp-sucess
                                           (progn (setq stop-marker (point-marker))
                                                  (delete-region start-marker stop-marker)
                                                  (insert "0"))))
				       (zjl-hl-dbg-print "while end replace a = xxxx" 'debug))

				     ;; Remove <...> which cause semantic failed to variable detect
                                     (when (or (equal 'c++-mode original-mode) (equal 'java-mode original-mode))
                                       (goto-char (point-min))
                                       (let (start-point)
					 (zjl-hl-dbg-print "while start remove <>" 'debug)
                                         (while (setq start-point (zjl-hl-find-regexp-return-end-pos (point) (point-max) "<"))
                                           (goto-char (1- (car start-point)))
					   (forward-sexp)
					   (if (looking-back ">")
					       (delete-region (1- (car start-point)) (point))))
					 (zjl-hl-dbg-print "while end remove <>" 'debug)))

				     (goto-char func-body-begin-temp-buffer)
				     (let (start-point)
				       (zjl-hl-dbg-print "while start remove all function call" 'debug)
				       (while (setq start-point (zjl-hl-find-regexp-return-end-pos (point) (point-max) "("))
					 (if (looking-back "\\_<if\\|for\\|while\\|switch\\_>[ 	\n]*(")
				     	     (progn (goto-char (car start-point)))
				     	   (goto-char (1- (car start-point)))
				     	   (forward-sexp)
				     	   (if (looking-back ")")
				     	       (delete-region (1- (car start-point)) (point)))))
				       (zjl-hl-dbg-print "while end remove all function call" 'debug))

                                     (semantic-new-buffer-fcn)

                                     (condition-case err ;; to isolate err impact
                                         (let ((start-time (float-time)))
					   (when func-body-begin-temp-buffer
					     (goto-char func-body-begin-temp-buffer)
					     (setq items-deepest (semantic-parse-region func-body-begin-temp-buffer (- (point-max) 2) nonterminal zjl-hl-semantic-parse-depth nil))
					     (zjl-hl-dbg-print "in temp buffer var(with large depth)=%s" 'verbose items-deepest)
					     (dolist (item (zjl-hl-get-var-in-semantic-parcel items-deepest))
					       (add-to-list 'local-variable-list item))
					     (setq depth 0)
					     (zjl-hl-dbg-print "while start in temp buffer get var" 'debug)
					     (while (and (< depth zjl-hl-semantic-parse-depth)
							 (< (- (float-time) start-time) 2))
					       (setq items (semantic-parse-region func-body-begin-temp-buffer (- (point-max) 2) nonterminal depth nil))
					       ;;since the semantic-parse-region cost 95% of zjl-hl, so must reduce its calling as much as possible
					       (if (equal items items-deepest)
						   (setq depth zjl-hl-semantic-parse-depth)
						 (zjl-hl-dbg-print "in temp buffer var(with less depth)=%s" 'verbose items)
						 (dolist (item (zjl-hl-get-var-in-semantic-parcel items))
						   (add-to-list 'local-variable-list item)))
					       (setq depth (1+ depth)))

					     (zjl-hl-dbg-print "while end in temp buffer get var" 'debug)))
                                       (error (zjl-hl-dbg-print "error in zjl-hl-do-func-region when find local variable in temp buffer %s" 'critical (error-message-string err))))

                                     ;; Use REGEXP to get arg, because below two formal methods do not work in temp buffer (but work most time in real file buffer), don't know why
                                     ;;1. (semantic-parse-region (point-min) (point-max) nonterminal nil nil)  -- this alawys return nil in temp buffer or a temp file buffer
                                     ;;2. (semantic-get-local-auguments) -- this always return nil in temp file buffer when file is first time opened, and have data returnned if close and then reopen file.
                                     (condition-case err ;; to isolate err impact
                                         (when func-body-begin-temp-buffer
                                           (goto-char func-body-begin-temp-buffer)
                                           (let (arg-start arg-end);;start is after(, end is after )
                                             (when (and (equal major-mode 'c++-mode) (re-search-backward "[^:]:[^:]" nil t))
                                               (forward-char 1))
                                             (when (search-backward ")" nil t)
                                               (forward-char 1)
                                               (setq arg-end (point-marker)) ;; after point  )
                                               (backward-sexp)
                                               (forward-char 1)
                                               (setq arg-start (point-marker)))
                                             (dolist (item (zjl-hl-get-arg-use-regexp arg-start arg-end))
                                               (add-to-list 'parameter-list item))

                                             ;; This is to double check the () is the one hold args, by search the () after the func name
                                             (goto-char (point-min))
                                             (when (and (search-forward (concat (replace-regexp-in-string "[(*=)]" "" (which-function)) "[ 	\n]*(") func-body-begin-temp-buffer t)
                                                        (not (equal (point-marker) arg-start)))
                                               (setq arg-start (point-marker))
                                               (backward-char 1)
                                               (forward-sexp)
                                               (setq arg-end (point-marker)))
                                             (dolist (item (zjl-hl-get-arg-use-regexp arg-start arg-end))
                                               (add-to-list 'parameter-list item)))
                                           (zjl-hl-dbg-print "in temp buffer arg=%s" 'verbose parameter-list))
                                       (error (zjl-hl-dbg-print "error in zjl-hl-do-func-region when find arg in temp buffer:%s" 'critical (error-message-string err))))))
                               (error (set-buffer original-buffer)
                                      (setq successful nil))))
                      ('B (condition-case err ;; to isolate err impact
                              (when func-body-begin
                                (goto-char func-body-begin)
                                (setq items (semantic-get-local-arguments))
                                (zjl-hl-dbg-print "in current buffer arg=%s" 'verbose items)
                                (dolist (item (zjl-hl-get-var-in-semantic-parcel items t))
                                  ;; (when (not (member item parameter-list)) (zjl-hl-dbg-print "find more arg" item));;purelly for debug
                                  (add-to-list 'parameter-list item)))
                            (error (zjl-hl-dbg-print "error in zjl-hl-do-func-region when find arg:%s" 'critical (error-message-string err))))
                          (condition-case err ;; To isolate err impact
                              (let ((start-time (float-time)))
				(when func-body-begin
				  (goto-char func-body-begin)
				  (setq items-deepest (semantic-parse-region func-body-begin (- end 2) nonterminal zjl-hl-semantic-parse-depth nil))
				  (zjl-hl-dbg-print "in current buffer var(with large depth)=%s" 'verbose items-deepest)
				  (dolist (item (zjl-hl-get-var-in-semantic-parcel items-deepest))
				    (add-to-list 'local-variable-list item))
				  (setq depth 0)
				  (zjl-hl-dbg-print "while start in current buffer get var" 'debug)
				  (while (and (< depth zjl-hl-semantic-parse-depth)
					      (< (- (float-time) start-time) 2))
				    (setq items (semantic-parse-region func-body-begin (- end 2) nonterminal depth nil))
				    ;; Since the semantic-parse-region cost 95% of zjl-hl, so must reduce its calling as much as possible
				    (if (equal items items-deepest)
					(setq depth zjl-hl-semantic-parse-depth)
				      (zjl-hl-dbg-print "in current buffer var(with less depth)=%s" 'verbose items)
				      (dolist (item (zjl-hl-get-var-in-semantic-parcel items))
					;; (when (not (member item local-variable-list)) (zjl-hl-dbg-print "find more var" item));;purelly for debug
					(add-to-list 'local-variable-list item)))
				    (setq depth (1+ depth)))

				  (zjl-hl-dbg-print "while end in current buffer get var" 'debug)))
                            (error (zjl-hl-dbg-print "error in zjl-hl-do-func-region when find local variable=%s" 'critical (error-message-string err)))))))
            (error (zjl-hl-dbg-print "error in zjl-hl-do-func-region:%s" 'critical (error-message-string err))
                   (setq successful nil)))
          (save-excursion
            (when parameter-list ;; Should place before local-variable-list since some parameter will override by local-variable
              (zjl-hl-symbol-region start end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-hl-parameters-reference-face))
            (when local-variable-list
              (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-hl-local-variable-reference-face)))
          successful))
    (error (zjl-hl-dbg-print "error in zjl-hl-do-func-region:%s" 'critical (error-message-string err))
           nil)))

(defun zjl-hl-symbol-region (start end symbol-exp symbol-face)
  (let (target-this-end target-next-start (case-fold-search nil) pos-pair)
    (save-excursion
      (goto-char start)
      (zjl-hl-dbg-print "while start" 'debug)
      (while (< (point) end)
        (setq pos-pair (zjl-hl-next-region-to-ignore-hl (point) end))
        (if pos-pair
            (progn
              (setq target-this-end (car pos-pair))
              (setq target-next-start (cdr pos-pair)))
          (setq target-this-end end)
          (setq target-next-start end))
        (progn
          (hlt-highlight-regexp-region (point) target-this-end symbol-exp symbol-face)
          (goto-char target-next-start)))
      (zjl-hl-dbg-print "while end" 'debug))))

(defun zjl-hl-next-region-to-ignore-hl (start end)
  (save-excursion
    (let (pos-pair find-p)
      (goto-char start)
      (save-excursion
        ;;possible of pr_debug("xxx \"chosen\"xxx\n"); a("", "xxx")
        (when (re-search-forward "\\(//.*\\)\\|\\(/\\*\\(?:.\\|\n\\)*?\\*/\\)\\|\\(\"\"\\|\\(\"\\([^\"]\\|\\\"\\)*?[^\\]\"\\)\\)\\|\\(\\(\\.\\|->\\)\\_<[_a-zA-Z]\\(\\w\\|\\s_\\)*\\)" end t) 
          (setq pos-pair (cons (nth 0 (match-data)) (nth 1 (match-data))))))
      pos-pair)))

;;; If find, return point after the re-search, else return nil; point didn't move
;;; But be careful, the car of return alist is the last point not the nth n+1th point
(defun zjl-hl-find-regexp-return-end-pos (start end regexp &optional index)
  (let (target-this-end target-next-start (case-fold-search nil) find-p3 find-p pos-pair tempstart)
    (save-excursion
      (goto-char start)
      (zjl-hl-dbg-print "while start" 'debug)
      (while (< (point) end)
        (setq tempstart (point))
        ;;possible of pr_debug("xxx \"chosen\"xxx\n");
        (setq find-p3 (re-search-forward (concat "\\(//.*\\)\\|\\(/\\*\\(?:.\\|\n\\)*?\\*/\\)\\|\\(\"\"\\|\\(?:\"\\(?:[^\"]\\|\\\"\\)*?[^\\]\"\\)\\)\\|\\(" regexp "\\)") end t))
        (if (not find-p3)
            (goto-char end)
          (when (nth 8 (match-data)) ;;Don't use (match-data t), it has a buffer obj in end of list
            (if (and index (nth index (match-data))) 
                (setq find-p (cons (point) (nth index (match-data t))))
              (setq find-p (cons (point) nil)))
            (goto-char end))))
      (zjl-hl-dbg-print "while end" 'debug))
    find-p))

(defun zjl-hl-splitting-to-func (start end AB)
  (save-excursion
    (let ((regions (funcall zjl-hl-find-hl-var-arg-regions start end))
          temp-regions
          (successful t)
          (successful-all t))
      (zjl-hl-dbg-print "regions=%s" 'debug regions)
      (dolist (each-region regions)
        (setq  successful-all (and (setq successful (zjl-hl-do-func-region (car each-region) (cdr each-region) AB))
                                   successful-all))
        (zjl-hl-dbg-print "function success or not? %s" 'verbose successful)
        (case AB ('A (if (not zjl-hl-semantic-parse-done)
                         (setq zjl-hl-regions-1A-DONE (region-list-edit-add zjl-hl-regions-1A-DONE each-region))
                       (when successful
                         (setq zjl-hl-regions-2A-Y (region-list-edit-add zjl-hl-regions-2A-Y each-region)))
                       (when (not successful)
                         (setq temp-regions (list each-region))
                         (dolist (retry-region zjl-hl-regions-2A-X-try-again)
                           (setq temp-regions (region-list-edit-delete temp-regions retry-region)))
                         (if temp-regions
                             ;;The region isn't in retry list yet, so add to retry list for 2nd chance
                             (setq zjl-hl-regions-2A-X-try-again (region-list-edit-add zjl-hl-regions-2A-X-try-again each-region))
                           ;;Else it has been retried, so add to retry done list to prevent further retry
                           (setq zjl-hl-regions-2A-X-X (region-list-edit-add zjl-hl-regions-2A-X-X each-region))))))
              ('B (if (not zjl-hl-semantic-parse-done)
                         (setq zjl-hl-regions-1B-DONE (region-list-edit-add zjl-hl-regions-1B-DONE each-region))
                       (when successful
                         (setq zjl-hl-regions-2B-Y (region-list-edit-add zjl-hl-regions-2B-Y each-region)))
                       (when (not successful)
                         (setq temp-regions (list each-region))
                         (dolist (retry-region zjl-hl-regions-2B-X-try-again)
                           (setq temp-regions (region-list-edit-delete temp-regions retry-region)))
                         (if temp-regions
                             ;;The region isn't in retry list yet, so add to retry list for 2nd chance
                             (setq zjl-hl-regions-2B-X-try-again (region-list-edit-add zjl-hl-regions-2B-X-try-again each-region))
                           ;;Else it has benn retried, so add to retry done list to prevent further retry
                           (setq zjl-hl-regions-2B-X-X (region-list-edit-add zjl-hl-regions-2B-X-X each-region)))))))
        )
      successful-all)))

(setq zjl-hl-timer-B-obj nil)
(setq zjl-hl-timer-pre-hl-obj nil)
(defun zjl-hl-stop-work (&optional seconds)
  (if (not zjl-hl-freeze-already)
      (progn (setq zjl-hl-freeze-already t)
	     (setq zjl-hl-too-big-size (/ zjl-hl-too-big-size 4))
	     (setq zjl-hl-too-big-to-update-size (/ zjl-hl-too-big-to-update-size 4))
	     (zjl-hl-dbg-print-message "zjl-hl shrink zjl-hl-too-big-size after detect freeze for seconds %s" (if seconds seconds "") )))
  (zjl-hl-disable-current-buffer)
  (zjl-hl-dbg-print-message "zjl-hl stop work on this buffer after detect freeze again"))
(defun zjl-hl-timer-do-whole (thisbuffer &optional start end AB)
  (condition-case err
      (save-excursion
        (when (buffer-live-p thisbuffer)
          (with-current-buffer thisbuffer
            (when (get-buffer-window)
              (let(temp-regions (win-s (window-start)) (win-e (window-end)) next-step A-time (start-time (float-time)))
                (unless (and start end)
                  (if (and win-s win-e)
                      (progn
                        (setq start (copy-marker (max (point-min);;hl larger of region that cover current screen
                                                      (- win-s
                                                         (* zjl-hl-numberofscreen-to-hl-each-time (- win-e win-s))))))
                        (setq end (copy-marker (min (point-max)
                                                    (+ win-e
                                                       (* zjl-hl-numberofscreen-to-hl-each-time (- win-e win-s)))))))
                    (progn
                      (setq end (point-max-marker))
                      (setq start (point-min-marker)))))
                (setq start (save-excursion
                              (goto-char start)
                              (if (zjl-hl-c-beginning-of-defun) 
                                  (point-marker)
                                (copy-marker (max start (point-min-marker))))))
                (setq end (save-excursion
                            (goto-char end)
                            (if (zjl-hl-c-end-of-defun)
                                (point-marker)
                              (copy-marker (min end (point-max-marker))))))
                (zjl-hl-dbg-print "start end=%s" 'verbose (cons start end))

                (setq next-step start)
                (when (> (- end start) (* 3 zjl-hl-no-delay-max-size))
                  (save-excursion
                    (let (find-better-end)
		      (zjl-hl-dbg-print "while start find-better-end" 'debug)
                      (while (and (<= (- next-step start) zjl-hl-too-big-size)
                                  (< next-step (point-max))
                                  (< next-step end)
                                  (not find-better-end))
                        (goto-char (setq next-step (min (+ next-step (/ zjl-hl-no-delay-max-size 4))
                                                        (point-max))))
                        (when (and (zjl-hl-c-end-of-defun)
                                   (< (point-marker) end))
                          (setq find-better-end (point-marker))))
		      (zjl-hl-dbg-print "while end find-better-end" 'debug)
                      (when find-better-end (setq end find-better-end)))))
                
                (zjl-hl-dbg-print "after shrink, start end=%s" 'debug (cons start end))

                (when (< zjl-hl-pre-hl-max-buffer zjl-hl-pre-hl-max)
                  (setq zjl-hl-pre-hl-max-buffer (1+ zjl-hl-pre-hl-max-buffer))
                  (let ((pre-hl-start  (copy-marker (min (point-max) (1+ end))))
                        (pre-hl-end    (copy-marker (min (point-max) (+ end (* zjl-hl-numberofscreen-to-hl-each-time (- win-e win-s)))))))
                    (unless (> pre-hl-end pre-hl-start)
                      (setq pre-hl-start  (point-min-marker)
                            pre-hl-end    (copy-marker (min  (point-max) (+ (point-min) (* zjl-hl-numberofscreen-to-hl-each-time (- win-e win-s)))))))
                    (when zjl-hl-timer-pre-hl-obj
                      (cancel-timer zjl-hl-timer-pre-hl-obj)
                      (setq zjl-hl-timer-pre-hl-obj nil))
                    (setq zjl-hl-timer-pre-hl-obj (run-with-idle-timer 5 nil 'zjl-hl-timer-do-whole thisbuffer pre-hl-start pre-hl-end))))


                (when (< (- end start) zjl-hl-too-big-size)
                  (setq A-time (float-time))
                  (zjl-hl-dbg-print "zjl-hl-semantic-parse-done %s" 'debug zjl-hl-semantic-parse-done)
                  (zjl-hl-dbg-print "zjl-hl-regions-1A-DONE %s" 'debug zjl-hl-regions-1A-DONE)
		  (zjl-hl-dbg-print "zjl-hl-regions-2A-Y %s" 'debug zjl-hl-regions-2A-Y)
		  (zjl-hl-dbg-print "zjl-hl-regions-2A-X-X %s" 'debug zjl-hl-regions-2A-X-X)
                  (progn
                    (setq temp-regions (list (cons start end)))
                    (if (not zjl-hl-semantic-parse-done)
                        (progn (dolist (hl-region zjl-hl-regions-1A-DONE)
                                 (setq temp-regions (region-list-edit-delete temp-regions hl-region))))
                      (dolist (hl-region zjl-hl-regions-2A-Y)
                        (setq temp-regions (region-list-edit-delete temp-regions hl-region)))
                      (dolist (hl-region zjl-hl-regions-2A-X-X)
                        (setq temp-regions (region-list-edit-delete temp-regions hl-region))))
		    (zjl-hl-dbg-print "before splitting to func - A" 'debug)
                    (when temp-regions
                      (zjl-hl-dbg-print "temp-regions=%s" 'debug temp-regions)
                      (dolist (each-region temp-regions)
                        (when (zjl-hl-splitting-to-func (car each-region) (cdr each-region) 'A)
                          ;; Though in zjl-hl-splitting-to-func has add highlighted each function region into list already, here add the whole region again, so reduce list fragment.
                          (if (not zjl-hl-semantic-parse-done) 
                              (setq zjl-hl-regions-1A-DONE (region-list-edit-add zjl-hl-regions-1A-DONE each-region))
                            (setq zjl-hl-regions-2A-Y (region-list-edit-add zjl-hl-regions-2A-Y each-region))))))
		    (zjl-hl-dbg-print "after splitting to func - A" 'debug))
                  (setq A-time (- (float-time) A-time))
                  (if (or AB (< A-time 0.75))
                      (progn
                        (zjl-hl-dbg-print "continue after A-time %s" 'debug A-time)
                        (zjl-hl-dbg-print "zjl-hl-regions-1B-DONE %s" 'debug zjl-hl-regions-1B-DONE)
			(zjl-hl-dbg-print "zjl-hl-regions-2B-Y %s" 'debug zjl-hl-regions-2B-Y)
			(zjl-hl-dbg-print "zjl-hl-regions-2B-X-X %s" 'debug zjl-hl-regions-2B-X-X)
                        (setq temp-regions (list (cons start end)))
                        (if (not zjl-hl-semantic-parse-done)
                            (progn (dolist (hl-region zjl-hl-regions-1B-DONE)
                                     (setq temp-regions (region-list-edit-delete temp-regions hl-region))))
                          (dolist (hl-region zjl-hl-regions-2B-Y)
                            (setq temp-regions (region-list-edit-delete temp-regions hl-region)))
                          (dolist (hl-region zjl-hl-regions-2B-X-X)
                            (setq temp-regions (region-list-edit-delete temp-regions hl-region))))
			(zjl-hl-dbg-print "before splitting to func - B" 'debug)
                        (when temp-regions
                          (zjl-hl-dbg-print "temp-regions= %s" 'debug temp-regions)
                          (dolist (each-region temp-regions)
                            (when (zjl-hl-splitting-to-func (car each-region) (cdr each-region) 'B)
                              ;; Though in zjl-hl-splitting-to-func has add highlighted each function region into list already, here add the whole region again, so reduce list fragment.
                              (if (not zjl-hl-semantic-parse-done)
                                  (setq zjl-hl-regions-1B-DONE (region-list-edit-add zjl-hl-regions-1B-DONE each-region))
                                (setq zjl-hl-regions-2B-Y (region-list-edit-add zjl-hl-regions-2B-Y each-region))))))
			(zjl-hl-dbg-print "after splitting to func - B" 'debug))
                    (when zjl-hl-timer-B-obj
                      (cancel-timer zjl-hl-timer-B-obj)
                      (setq zjl-hl-timer-B-obj nil))
                    (setq zjl-hl-timer-B-obj (run-with-idle-timer 2 nil 'zjl-hl-timer-do-whole thisbuffer))))
		(if (> (- (float-time) start-time) 8)
		    (zjl-hl-stop-work (- (float-time) start-time))
		  (when (> (- (float-time) start-time) 2)
		    (setq zjl-hl-freeze-max-times (1+ zjl-hl-freeze-max-times))
		    (when (= zjl-hl-freeze-max-times 10)
		      (setq zjl-hl-freeze-max-times 0)
		      (zjl-hl-dbg-print "seems frequence freeze detected" 'debug)
		      (zjl-hl-stop-work)
		      ))))))))
    (error (zjl-hl-dbg-print "error in zjl-hl-timer-do-whole: %s" 'critical (error-message-string err)))))

(defun zjl-hl-window-scroll-hook(par1 par2)
  (condition-case err
      (save-excursion
        (when (or (equal major-mode 'c-mode) ;;If major-mode changed,this hook may still running???
                  (equal major-mode 'c++-mode)
                  (equal major-mode 'java-mode))
          (when zjl-hl-firsttime
            (add-hook 'semantic-after-partial-cache-change-hook 'zjl-hl-semantic-after-partial-cache-change-hook t)
	    (zjl-hl-dbg-print "add semantic partial change hook" 'debug)
            (setq zjl-hl-firsttime nil))
          ;;This should before if, to canel possbile hl timer, before zjl-hl-timer-do-whole realy begin
          (when zjl-hl-timer-obj
            (cancel-timer zjl-hl-timer-obj)
            (setq zjl-hl-timer-obj nil))
          (if zjl-hl-first-time-hl-no-delay-p
              (progn (zjl-hl-timer-do-whole (current-buffer))
                     (setq zjl-hl-first-time-hl-no-delay-p nil))
            (setq zjl-hl-timer-obj (run-with-idle-timer 0.2 nil 'zjl-hl-timer-do-whole (current-buffer))))))
    (error (zjl-hl-dbg-print "error in zjl-hl-window-scroll-hook: %s" 'critical (error-message-string err)))))


(setq zjl-hl-firsttime t)
(setq zjl-hl-first-time-hl-no-delay-p nil)
(setq zjl-hl-regions-2A-Y nil)
(setq zjl-hl-regions-1A-DONE nil)
(setq zjl-hl-regions-2A-X-try-again nil)
(setq zjl-hl-regions-2A-X-X nil)
(setq zjl-hl-regions-2B-Y nil)
(setq zjl-hl-regions-1B-DONE nil)
(setq zjl-hl-regions-2B-X-try-again nil)
(setq zjl-hl-regions-2B-X-X nil)
(setq zjl-hl-pre-hl-max-buffer 0)
(setq zjl-hl-freeze-max-times 0)
(setq zjl-hl-semantic-parse-done nil)
(setq zjl-hl-semantic-parse-done-count 1)
(setq zjl-hl-local-variable-list nil)
(setq zjl-hl-global-variable-list nil)
(setq zjl-hl-has-if-else-macro-with-unbalanced-brace nil)
(setq zjl-hl-after-semantic-parse-done-obj nil)
(setq zjl-hl-partial-change-region nil)
(setq zjl-hl-partial-change-region-activate nil)
(setq zjl-hl-partial-change-region-activate-timer nil)
(setq zjl-hl-disable-init-for-temp-buffer nil)
(setq zjl-hl-init-called-already nil)
(setq zjl-hl-freeze-already nil)
(defun zjl-hl-init ()
  (save-excursion
    (unless (or (and zjl-hl-disable-init-for-temp-buffer (not buffer-file-name)) ;;temp buffer so disable zjl-hl-init
		zjl-hl-init-called-already) ;;(string-match-p "\\.h$" buffer-file-name)
      (let (start end)
	(make-local-variable 'zjl-hl-init-called-already)
	(setq zjl-hl-init-called-already t)
	(make-local-variable 'zjl-hl-firsttime)
	(make-local-variable 'zjl-hl-first-time-hl-no-delay-p)
	(make-local-variable 'zjl-hl-regions-2A-Y)
	(make-local-variable 'zjl-hl-regions-1A-DONE)
	(make-local-variable 'zjl-hl-regions-2A-X-try-again)
	(make-local-variable 'zjl-hl-regions-2A-X-X)
	(make-local-variable 'zjl-hl-regions-2B-Y)
	(make-local-variable 'zjl-hl-regions-1B-DONE)
	(make-local-variable 'zjl-hl-regions-2B-X-try-again)
	(make-local-variable 'zjl-hl-regions-2B-X-X)
	(make-local-variable 'zjl-hl-semantic-parse-done)
	(make-local-variable 'zjl-hl-semantic-parse-done-count)
	(make-local-variable 'zjl-hl-local-variable-list)
	(make-local-variable 'zjl-hl-global-variable-list)
	(make-local-variable 'zjl-hl-find-hl-var-arg-regions)
	(make-local-variable 'zjl-hl-after-semantic-parse-done-obj)
	(make-local-variable 'zjl-hl-after-semantic-parse-done-obj)
	(make-local-variable 'zjl-hl-pre-hl-max-buffer)
	(make-local-variable 'zjl-hl-freeze-max-times)
	(make-local-variable 'zjl-hl-partial-change-region)
	(make-local-variable 'zjl-hl-partial-change-region-activate)
	(make-local-variable 'zjl-hl-partial-change-region-activate-timer)
	(make-local-variable 'zjl-hl-freeze-already)

	(setq zjl-hl-find-hl-var-arg-regions
	      (case major-mode
		('c-mode 'zjl-hl-find-hl-var-arg-regions-c)
		('c++-mode 'zjl-hl-find-hl-var-arg-regions-c++)
		('java-mode 'zjl-hl-find-hl-var-arg-regions-c++)))

	(make-local-variable 'zjl-hl-get-body-begin)
	(setq zjl-hl-get-body-begin
	      (case major-mode
		('c-mode 'zjl-hl-get-body-begin-c)
		('c++-mode 'zjl-hl-get-body-begin-c)
		('java-mode 'zjl-hl-get-body-begin-c)))

	(make-local-variable 'zjl-hl-has-if-else-macro-with-unbalanced-brace)

	(goto-char (point-min))
	(condition-case  err
	    (let (temp-point)
	      (while (setq temp-point (zjl-hl-find-regexp-return-end-pos (point) (point-max) "^[ 	]*#else" ))
		(goto-char (car temp-point))
		(end-of-defun)))
	  (error (zjl-hl-dbg-print-message "!! zjl-hl: see unbalanced brace in #else part and might has problem to hl, error string= %s" (error-message-string err))
		 (zjl-hl-dbg-print "!! zjl-hl: see unbalanced brace in #else part and might has problem to hl, error string= %s" 'verbose (error-message-string err))
		 (when zjl-hl-delete-else-do-hl
		   (setq zjl-hl-has-if-else-macro-with-unbalanced-brace t))))

	;; This is needed, the buffer might be parsed recently and result is cached before file is open, so no parse semantic-after-idle-scheduler-reparse-hook called to set the parse-done flag.
	(when (not (equal (buffer-name) "zjl-hl-test.c"));;sometime I use this buffer for test hl of simplified code, and do not want be bothered.
	  (run-with-idle-timer 7 nil 'zjl-hl-after-buffer-load-a-while-force-set-parse-done (current-buffer))
	  (run-with-timer 20 nil 'zjl-hl-after-buffer-load-a-while-force-set-parse-done (current-buffer))
	  
	  ;;hl larger of region that cover current screen
	  (when zjl-hl-firstscreen-hl-toggle
	    (setq start (copy-marker (max (point-min)
					  (- (window-start)
					     (* zjl-hl-numberofscreen-to-hl-each-time (- (window-end) (window-start)))))))
	    (setq end (copy-marker (min (point-max)
					(+ (window-end)
					   (* zjl-hl-numberofscreen-to-hl-each-time (- (window-end) (window-start)))))))
	    (if (< (- end start) zjl-hl-no-delay-max-size)
		(setq zjl-hl-first-time-hl-no-delay-p t)
	      (setq zjl-hl-first-time-hl-no-delay-p nil)))
	  (add-hook 'after-change-functions 'zjl-hl-after-change-functions--hook t t)
	  (add-hook 'semantic-after-idle-scheduler-reparse-hook 'zjl-hl-after-semantic-idle t t)
	  (add-hook 'window-scroll-functions 'zjl-hl-window-scroll-hook t t))))))

(defun zjl-hl-update-screen (thisbuffer)
  (condition-case err
      (when (buffer-live-p thisbuffer)
        (with-current-buffer thisbuffer
          (when (get-buffer-window thisbuffer 'visible)
            (zjl-hl-window-scroll-hook 1 1))))
    (error (zjl-hl-dbg-print "error in zjl-hl-update-screen: %s" 'critical (error-message-string err)))))

;;Idea from how ecb update method buffer
;;(add-hook (ecb--semantic-after-partial-cache-change-hook) 'ecb-update-after-partial-reparse t)
;;(add-hook (ecb--semantic-after-toplevel-cache-change-hook) 'ecb-rebuild-methods-buffer-with-tagcache t)
;; can refer to ecb\ecb-method-browser.el later for ecb-update-after-partial-reparse and ecb--semantic-after-partial-cache-change-hook
(defun zjl-hl-semantic-after-partial-cache-change-hook (updated-tags)
  (condition-case err
      (when (or zjl-hl-regions-2A-Y zjl-hl-regions-2B-Y)  ;;maybe zjl-hl-init has not hl any where
	(let (overlay todo todo-p)
	  (zjl-hl-dbg-print "zjl-hl-semantic-after-partial-cache-change-hook start" 'debug)
	  (dolist (tags updated-tags)
	    (setq overlay (semantic-tag-overlay tags))
	    (when (overlayp overlay);add blank line will has no overlay
	      (when (and (eq (current-buffer) (overlay-buffer overlay))
			 (< (- (overlay-end overlay) (overlay-start overlay)) zjl-hl-too-big-to-update-size))
		(setq todo (cons (copy-marker (overlay-start overlay)) (copy-marker (overlay-end overlay))))
		(when todo
		  (zjl-hl-dbg-print "zjl-hl-semantic-after-partial-cache-change-hook todo=%s" 'debug todo)
		  (setq zjl-hl-partial-change-region (region-list-edit-add zjl-hl-partial-change-region todo))
		  (setq todo-p t)))))
	  ;; Minimize call of hl, test shows that modificaiton of a word may call this function 3 times
	  (when todo-p
	    (when zjl-hl-partial-change-region-activate-timer
	      (cancel-timer zjl-hl-partial-change-region-activate-timer)
	      (setq zjl-hl-partial-change-region-activate-timer nil))
	    (setq zjl-hl-partial-change-region-activate-timer (run-with-idle-timer 5 nil 'zjl-hl-partial-change-region-activate (current-buffer))))))
    (error (zjl-hl-dbg-print "error in zjl-hl-semantic-after-partial-cache-change-hook:%s" 'critical (error-message-string err)))))


(defun zjl-hl-after-change-functions--hook (beg end len)
  (condition-case err
      (when (or zjl-hl-regions-2A-Y zjl-hl-regions-2B-Y)  ;;maybe zjl-hl-init has not hl any where
	(let (overlay todo todo-p)
	  (zjl-hl-dbg-print "zjl-hl-after-change-functions--hook  partial change start" 'debug)
	  (when (< (- end beg) zjl-hl-too-big-to-update-size)
	    (setq todo (cons (copy-marker beg) (copy-marker end)))
	    (when todo
	      (zjl-hl-dbg-print "zjl-hl-after-change-functions--hook partial change todo=%s" 'debug todo)
	      (setq zjl-hl-partial-change-region (region-list-edit-add zjl-hl-partial-change-region todo))))
	  ;; Minimize call of hl, test shows that modificaiton of a word may call this function 3 times
	  (when todo
	    (when zjl-hl-partial-change-region-activate-timer
	      (cancel-timer zjl-hl-partial-change-region-activate-timer)
	      (setq zjl-hl-partial-change-region-activate-timer nil))
	    (setq zjl-hl-partial-change-region-activate-timer (run-with-idle-timer 5 nil 'zjl-hl-partial-change-region-activate (current-buffer))))))
    (error (zjl-hl-dbg-print "error in zjl-hl-after-change-functions--hook" 'critical (error-message-string err))))
  )

(defun zjl-hl-partial-change-region-activate (change-buffer)
  (when (buffer-live-p change-buffer)
    (with-current-buffer change-buffer
      (when (and zjl-hl-partial-change-region (listp zjl-hl-partial-change-region))
	(zjl-hl-dbg-print "partial before" 'debug)
	(zjl-hl-dbg-print "zjl-hl-partial-change-region= %s" 'debug zjl-hl-partial-change-region)
	(zjl-hl-dbg-print "zjl-hl-regions-2A-Y= %s" 'debug zjl-hl-regions-2A-Y)
	(zjl-hl-dbg-print "zjl-hl-regions-2A-X-X= %s" 'debug zjl-hl-regions-2A-X-X)
	(zjl-hl-dbg-print "zjl-hl-regions-2B-Y= %s" 'debug zjl-hl-regions-2B-Y)
	(zjl-hl-dbg-print "zjl-hl-regions-2B-X-X= %s" 'debug zjl-hl-regions-2B-X-X)
	(let (func-start func-end func-region)
	  (dolist (change-region zjl-hl-partial-change-region)
	    ;; I observe that when a { is add in cpp, the semantic change hook will give the new larger "func"
	    ;; area in its parameter in most time but could also be the smaller old func area in sometime.
	    ;; And when the added { is removed then the smaller old func area is updated in parameter
	    ;; so need to manually find out the exact larger area we expected
	    (setq func-start (save-excursion
			       (goto-char (car change-region))
			       (if (zjl-hl-c-beginning-of-defun)
				   (point-marker)
				 (car change-region))))
	    (setq func-end (save-excursion
			       (goto-char (cdr change-region))
			       (if (zjl-hl-c-end-of-defun)
				   (point-marker)
				 (cdr change-region))))
	    (setq func-region (cons func-start func-end))
	    (setq zjl-hl-regions-2A-Y (region-list-edit-delete-greed zjl-hl-regions-2A-Y func-region))
	    (setq zjl-hl-regions-2A-X-X (region-list-edit-delete-greed zjl-hl-regions-2A-X-X func-region))
	    (setq zjl-hl-regions-2A-X-try-again (region-list-edit-delete-greed zjl-hl-regions-2A-X-try-again func-region))
	    (setq zjl-hl-regions-2B-Y (region-list-edit-delete-greed zjl-hl-regions-2B-Y func-region))
	    (setq zjl-hl-regions-2B-X-X (region-list-edit-delete-greed zjl-hl-regions-2B-X-X func-region))
	    (setq zjl-hl-regions-2B-X-try-again (region-list-edit-delete-greed zjl-hl-regions-2B-X-try-again func-region))))

	(zjl-hl-dbg-print "partial after" 'debug)
	(zjl-hl-dbg-print "zjl-hl-regions-2A-Y= %s" 'debug zjl-hl-regions-2A-Y)
	(zjl-hl-dbg-print "zjl-hl-regions-2A-X-X= %s" 'debug zjl-hl-regions-2A-X-X)
	(zjl-hl-dbg-print "zjl-hl-regions-2B-Y= %s" 'debug zjl-hl-regions-2B-Y)
	(zjl-hl-dbg-print "zjl-hl-regions-2B-X-X= %s" 'debug zjl-hl-regions-2B-X-X)
	;; (zjl-hl-dbg-print "in region partial-5" 'debug)
	(when zjl-hl-update-screen-timer
	  (cancel-timer zjl-hl-update-screen-timer)
	  (setq zjl-hl-update-screen-timer nil))
	(setq zjl-hl-update-screen-timer (run-with-idle-timer 7 nil 'zjl-hl-update-screen (current-buffer))))
      (setq zjl-hl-partial-change-region nil))))

(setq zjl-hl-fun-call-noticeable-degree-old-value nil)

(defun zjl-hl-enable-global-all-modes ()
  (interactive)
  (when zjl-hl-make-fun-call-noticeable
    (setq zjl-hl-fun-call-noticeable-degree-old-value (face-attribute 'font-lock-function-name-face :underline))
    (set-face-attribute 'font-lock-function-name-face nil :underline t))
  (when zjl-hl-c-mode-enable-flag
    (zjl-hl-enable-global 'c-mode))
  (when zjl-hl-c++-mode-enable-flag
    (zjl-hl-enable-global 'c++-mode))
  (when zjl-hl-java-mode-enable-flag
    (zjl-hl-enable-global 'java-mode)))


(defun zjl-hl-disable-global-all-modes ()
  (interactive)
  (when (and zjl-hl-make-fun-call-noticeable
             zjl-hl-fun-call-noticeable-degree-old-value)
    (set-face-attribute 'font-lock-function-name-face nil :underline zjl-hl-fun-call-noticeable-degree-old-value))
  (when zjl-hl-c-mode-enable-flag
    (zjl-hl-disable-global 'c-mode))
  (when zjl-hl-c++-mode-enable-flag
    (zjl-hl-disable-global 'c++-mode))
  (when zjl-hl-java-mode-enable-flag
    (zjl-hl-disable-global 'java-mode)))

(defun zjl-hl-enable-current-buffer ()
  (interactive)
  (when (or (equal major-mode 'c-mode)
            (equal major-mode 'c++-mode)
            (equal major-mode 'java-mode))
    (let ((mode-name (symbol-name major-mode)) hook keywords)
      (setq keywords (intern-soft (concat "zjl-hl-" mode-name "-keywords")))
      (font-lock-add-keywords major-mode (symbol-value keywords) 1)
      (funcall major-mode)
      (zjl-hl-init)
      (zjl-hl-window-scroll-hook 1 1))))


(defun zjl-hl-disable-current-buffer ()
  (interactive)
  (when (or (equal major-mode 'c-mode)
            (equal major-mode 'c++-mode)
            (equal major-mode 'java-mode))
    (remove-hook 'window-scroll-functions 'zjl-hl-window-scroll-hook t)
    (remove-hook 'after-change-functions 'zjl-hl-after-change-functions--hook t)
    (zjl-hl-dbg-print "remove 'zjl-hl-semantic-after-partial-cache-change-hook" 'debug)
    (remove-hook 'semantic-after-partial-cache-change-hook 'zjl-hl-semantic-after-partial-cache-change-hook t)
    (when zjl-hl-timer-obj
      (cancel-timer zjl-hl-timer-obj)
      (setq zjl-hl-timer-obj nil))
    (when zjl-hl-timer-B-obj
      (cancel-timer zjl-hl-timer-B-obj)
      (setq zjl-hl-timer-B-obj nil))
    (when zjl-hl-timer-pre-hl-obj
      (cancel-timer zjl-hl-timer-pre-hl-obj)
      (setq zjl-hl-timer-pre-hl-obj nil))
    (when zjl-hl-update-screen-timer
      (cancel-timer zjl-hl-update-screen-timer)
      (setq zjl-hl-update-screen-timer nil))
    (when zjl-hl-after-semantic-parse-done-obj
      (cancel-timer zjl-hl-after-semantic-parse-done-obj)
      (setq zjl-hl-after-semantic-parse-done-obj nil))
    (when zjl-hl-partial-change-region-activate-timer
      (cancel-timer zjl-hl-partial-change-region-activate-timer)
      (setq zjl-hl-partial-change-region-activate-timer nil))))

(defun zjl-hl-enable-global (mode)
  (let ((mode-name (symbol-name mode)) hook keywords)
    (setq keywords (intern-soft (concat "zjl-hl-" mode-name "-keywords")))
    (font-lock-add-keywords mode (symbol-value keywords) 1)
    (setq hook (intern-soft (concat mode-name "-hook")))
    (add-hook hook 'zjl-hl-init)))

(defun zjl-hl-disable-global (mode)
  (let ((mode-name (symbol-name mode)) hook keywords)
    (setq keywords (intern-soft (concat "zjl-hl-" mode-name "-keywords")))
    (font-lock-remove-keywords mode (symbol-value keywords))
    (setq hook (intern-soft (concat mode-name "-hook")))
    (remove-hook hook 'zjl-hl-init)
    (remove-hook 'window-scroll-functions 'zjl-hl-window-scroll-hook)
    (zjl-hl-dbg-print "remove 'zjl-hl-semantic-after-partial-cache-change-hook" 'debug)
    (remove-hook 'semantic-after-partial-cache-change-hook 'zjl-hl-semantic-after-partial-cache-change-hook)))

(defun zjl-hl-get-body-begin-c (start end)
  (save-excursion
    (goto-char start)
    (car-safe (zjl-hl-find-regexp-return-end-pos start end "{"))))

(defun zjl-hl-find-hl-var-arg-regions-c (start end)
  (save-excursion
    (goto-char start)
    ;; Add last begin/end and iteration limitation is to stop some wrong parenthesis pair ("#ifdef.else .endif" inside fun call paramter might cause this) to freezen emacs 
    (let (fun-begin fun-end regions (iteration-num 0) last-fun-begin last-fun-end)
      (zjl-hl-dbg-print "while start" 'debug)
      (while (and (setq fun-end 
                        (if (and (zjl-hl-c-end-of-defun)
                                 (not (equal (point-marker) last-fun-end)))
                            (point-marker)
                          nil))
                  (setq fun-begin
                        (if (and (zjl-hl-c-beginning-of-defun)
                                 (not (equal (point-marker) last-fun-begin)))
                            (point-marker)
                          nil))
                  (< fun-begin end)
                  (< iteration-num 100))
        (setq regions  (cons (cons fun-begin fun-end) regions))
        (goto-char fun-end)

        (setq last-fun-begin fun-begin)
        (setq last-fun-end fun-end)
        (setq iteration-num (1+ iteration-num)))
      (zjl-hl-dbg-print "while end" 'debug)
      regions)))

;;Function need to work for class/namespace in c++/java, and like special below case: in 1,2 point, both get same function start, 1 end in large scope, 2 end in scope inside 1.
;;@SuppressWarnings({1"EmptyCatchBlock", "PointlessBooleanExpression"})
;;public final class ViewRootImpl implements ViewParent,
;;        View.AttachInfo.Callbacks, HardwareRenderer.HardwareDrawCallbacks {2
;;...
;;}
(defun zjl-hl-find-hl-var-arg-regions-c++ (start end)
  (save-excursion
    (goto-char start)
    (let (tempvalue fun-begin fun-end regions (iteration-num 0) last-fun-begin last-fun-end index region)
      (zjl-hl-dbg-print "while start" 'debug)
      (while (and  (setq tempvalue (car (zjl-hl-find-regexp-return-end-pos (point) end "{" nil)))
                   (goto-char tempvalue)
                   (setq fun-end 
                         (if (zjl-hl-c-end-of-defun)
                             (point-marker)
                           nil))
                   (setq fun-begin
                         (if (zjl-hl-c-beginning-of-defun)
                             (point-marker)
                           nil))
                   (< fun-begin end)
                   (< iteration-num 200))
        
        ;; (setq index 0)
        ;; (while (setq region (nth index regions))
        ;;   (if (and (<= (car region) fun-begin) (>= (cdr region) fun-end))
        ;;     (setq regions (delete region regions))  
        ;;     (setq index (1+ index))))
        
        ;;In case of class or name space, need skip the end of class/namespace
        (goto-char fun-end)
        (re-search-backward "}")
        
        (if (and (zjl-hl-c-beginning-of-defun)
                 (not (equal (point-marker) fun-begin)))
            (goto-char tempvalue)
          (setq regions (cons (cons fun-begin fun-end) regions))
          (goto-char fun-end))

        (when (or (equal fun-begin last-fun-begin) (equal fun-end last-fun-end))
          (goto-char tempvalue))
      
        (setq last-fun-begin fun-begin)
        (setq last-fun-end fun-end)
        (setq iteration-num (1+ iteration-num)))
      (zjl-hl-dbg-print "while end" 'debug)
      (zjl-hl-dbg-print "regions=%s" 'verbose regions)
      regions)
))

(defun zjl-hl-after-semantic-idle ()
  (when zjl-hl-after-semantic-parse-done-obj
    (cancel-timer zjl-hl-after-semantic-parse-done-obj)
    (setq zjl-hl-after-semantic-parse-done-obj nil))
  (setq zjl-hl-after-semantic-parse-done-obj (run-with-idle-timer 2.5 nil 'zjl-hl-after-semantic-parse-done (current-buffer))))

(defun zjl-hl-revert-clone-buffer-then-kill (thisbuffer)
  (when (buffer-live-p thisbuffer)
    (revert-buffer thisbuffer t t)
    (kill-buffer thisbuffer)))

(defun zjl-hl-hl-whole-buffer (&optional thisbuffer)
  (interactive)
  (unless thisbuffer (setq thisbuffer (current-buffer)))
  (condition-case err
      (when (buffer-live-p thisbuffer)
        (with-current-buffer thisbuffer
	  (let (else-deleted)
	    (when (and zjl-hl-delete-else-do-hl zjl-hl-has-if-else-macro-with-unbalanced-brace)
	      (with-current-buffer (clone-indirect-buffer nil nil)
		(zjl-hl-dbg-print-message "zjl-hl delete #else part to hl this buffer")
		(sit-for 3)
		(setq else-deleted t)
		(zjl-hl-replace-regexp  "\\(^[ 	]*#else.*\\(?:.\\|\n\\)*?\\)\\(#endif\\)" "\n// TODO : zjl-hl delete the #else clause here, need fix\n/*\n#\\1*/\n\\2" nil (point-min) (point-max))))
	    (setq zjl-hl-regions-2A-Y nil)
	    (setq zjl-hl-regions-2A-X-try-again nil)
	    (setq zjl-hl-regions-2A-X-X nil)
	    (setq zjl-hl-regions-2B-Y nil)
	    (setq zjl-hl-regions-2B-X-try-again nil)
	    (setq zjl-hl-regions-2B-X-X nil)
	    (zjl-hl-timer-do-whole thisbuffer (point-min-marker) (point-max-marker) 'AB)
	    (when else-deleted
	      (run-with-idle-timer 6 nil 'zjl-hl-revert-clone-buffer-then-kill (current-buffer))))))
    (error (zjl-hl-dbg-print-message "error in zjl-hl-hl-whole-buffer: %s" (error-message-string err))
           (zjl-hl-dbg-print "error in zjl-hl-hl-whole-buffer: %s" 'verbose (error-message-string err)))))

(defun zjl-hl-after-semantic-parse-done (thisbuffer)
  (when (buffer-live-p thisbuffer)
    (with-current-buffer thisbuffer
      (unless zjl-hl-semantic-parse-done
        (when (< zjl-hl-semantic-parse-done-count 20)
          ;; I just saw the semantic-after-idle-scheduler-reparse-hook is run 3 times for a file, with interval of 1 and 3 seconds.
          ;; So for now I clean the whole hl regions list for a short solution, but this is conflict with the partial update of above
          ;;(zjl-hl-dbg-print-message "zjl:%s clean the hl regions list !!! :%s" (current-time) (buffer-name))
          (setq zjl-hl-semantic-parse-done-count (1+ zjl-hl-semantic-parse-done-count))
          (setq zjl-hl-regions-2A-Y nil)
          (setq zjl-hl-regions-2A-X-try-again nil)
          (setq zjl-hl-regions-2A-X-X nil)
          (setq zjl-hl-regions-2B-Y nil)
          (setq zjl-hl-regions-2B-X-try-again nil)
          (setq zjl-hl-regions-2B-X-X nil)

          (when zjl-hl-update-screen-timer
            (cancel-timer zjl-hl-update-screen-timer)
            (setq zjl-hl-update-screen-timer nil))
	  (if (not (and zjl-hl-delete-else-do-hl zjl-hl-has-if-else-macro-with-unbalanced-brace))
	      (setq zjl-hl-update-screen-timer (run-with-idle-timer 1 nil 'zjl-hl-update-screen (current-buffer)))
	    (if (y-or-n-p-with-timeout "!! zjl-hl: see unbalance brace, can I temporarily delete #else part to do whole buffer highlight?" 5 nil)
		(progn (zjl-hl-dbg-print-message "zjl-hl delete #else part to hl this buffer")
		       (zjl-hl-hl-whole-buffer (current-buffer)))
	      (zjl-hl-dbg-print-message "zjl-hl didn't delete #else part")
	      (setq zjl-hl-update-screen-timer (run-with-idle-timer 1 nil 'zjl-hl-update-screen (current-buffer)))))
          (setq zjl-hl-semantic-parse-done t))))))
(defun zjl-hl-after-buffer-load-a-while-force-set-parse-done (thisbuffer)  
  (zjl-hl-after-semantic-parse-done thisbuffer))

(provide 'zjl-hl)
