;;;This package can highlight variables and function calling and other keywords in c
;; to enable this package, add this two lines into your .emacs:
;; (require 'zjl-hl)
;; (zjl-hl-enable-global-all-modes);to disable, call (zjl-hl-disable-global-all-modes)

;;; begin lisp code
(require 'highlight)
(require 'region-list-edit)

(defcustom zjl-hl-make-fun-call-notable  t
  "enlarge font of called function, so that become notable"
  :type 'boolean :group 'zjl-hl)
(defcustom zjl-hl-fun-call-notable-degree  1.2
  "Control the font size of function call"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-c-mode-enable-flag t
  "Enable c mode highlight when zjl-hl-enable-global-all is called"
  :type 'boolean :group 'zjl-hl)

(defcustom zjl-hl-c++-mode-enable-flag nil
  "Enable c++ mode highlight when zjl-hl-enable-global-all is called.
Currently only c style file but named as *.cpp is supported"
  :type 'boolean :group 'zjl-hl)



(defcustom zjl-hl-normal-size 40000
  "The size of erea that zjl-hl can highlight without any delay.
You can improve this if your computer has enough performance."
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-toobig-size 10000000
  "The threshold size of function that zjl-hl will stop to highlight since it is too big. The size corresponds to the largest function found in current screen and
+-zjl-hl-numberofscreen-to-hl-each-time screens"
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-toobig-not-update-size 1000000
  "The size of function that zjl-chl will stop to  highlight when the function is modified.
the function means those that inculded in current screen and +-zjl-hl-numberofscreen-to-hl-each-time
screens"
  :type 'integer :group 'zjl-hl)

(defcustom zjl-hl-numberofscreen-to-hl-each-time 1
  "The number of screens around current screen to highlight every time.
This variable is define for:
I use idle timer delay to begin highlight current screen when user stop to scroll screen
(so as to have no delay when scroll),but this cause the highlight happen delay 0.5sec
after we stop scroll screen, and this not feels so good. The way to void this(in some degree)
is highlighting [-zjl-hl-numberofscreen-to-hl-each-time +zjl-hl-numberofscreen-to-hl-each-time]
screens for each time zjl-hl work"
    :type 'integer :group 'zjl-hl)


(defcustom zjl-hl-global-variable-hl nil
"xxx"
    :type 'boolean :group 'zjl-hl)



(defcustom zjl-hl-firstscreen-hl-toggle nil
  "When not nil and when you open a new buffer, hl buffer before it shown on window.
this will cause delay that feel uncomfortable.Don't enable this unless your computer has
enough performance."
  :type 'boolean :group 'zjl-hl)

;;variable that use to add/remove timer
(setq zjl-hl-timer-obj nil)
(setq zjl-hl-timer-semantic-update-obj nil)


(defface zjl-hl-font-lock-bracket-face
  '((((class color)
      (background dark))
     (:foreground "firebrick3" :bold nil :italic nil))
    (((class color)
      (background light))
     (:foreground "firebrick3" :bold nil :italic nil))
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
     (:foreground "PaleGreen" :bold nil :italic nil))
    (((class color)
      (background light))
     (:foreground "DarkGoldenrod4" :bold nil :italic nil))
    (t
     ()))
  "*Font lock mode face for operater, e.g. '+', '-', etc."
  :group 'zjl-hl-faces)
(defvar zjl-hl-operators-face 'zjl-hl-operators-face)

(defface zjl-hl-member-reference-face
  '((((class color)
      (background dark))
     (:foreground "#f4a957" :bold nil :italic nil))
    (((class color)
      (background light))
     ;(:foreground "#008000" :bold nil :italic nil); theme 0
     ;(:foreground "#009c00" :bold nil :italic nil);between color-28/34, prefer 34
     ;(:foreground "#28a028" :bold nil :italic nil)
     (:foreground "#008024" :bold nil :italic nil)     
     ;;(:foreground "DarkKhaki")
     ;; (:foreground "DarkSalmon"); theme 1
     ;(:foreground "PaleVioletRed")
     )
    (t
     ()))
  "*Font lock mode face for the struct member reference, e.g. b in \"a->b\"."
  :group 'zjl-hl-faces)
(defvar zjl-hl-member-reference-face 'zjl-hl-member-reference-face)

(defface zjl-hl-function-call-face
  '((((class color)
      (background dark))
     (:foreground "#e566d7" :bold t))
    (((class color)
      (background light))
     ;(:foreground "#008000" :bold t :italic nil);theme 0
     ;(:foreground "#009c00" :bold t :italic nil);between color-28/34, prefer 34
     ;(:foreground "#28a028" :bold t :italic nil)
     (:foreground "#008024" :bold t :italic nil)     
     ;; (:foreground "#008080" :bold t);; theme 1
     )
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
    ;(:foreground "LimeGreen")
     ;SpringGreen4
     ;(:foreground "red")
     ;(:foreground "#008080")
     ;(:foreground "color-36")
     ;(:foreground "#00afaf");(:foreground "color-37")
     (:foreground "#127974");(:foreground "color-37")
     ;(:foreground "#008000" :bold nil :italic nil);;theme 0
     ;(:foreground "#28a028");;lighter than the ForestGreen but darker than the LimeGreen;;; theme 1
     )
    (t
     ()))
  "*Font lock mode face for variable in function body."
  :group 'zjl-hl-faces)

(defvar zjl-hl-local-variable-reference-face 'zjl-hl-local-variable-reference-face)
;; (defvar zjl-hl-local-variable-reference-face 'font-lock-variable-name-face)


(defface zjl-hl-global-variable-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod"))
    (((class color)
      (background light))
     (:foreground "maroon" :bold nil :italic t)
     )
    (t
     ()))
  "*Font lock mode face for variable in function body."
  :group 'zjl-hl-faces)
(defvar zjl-hl-global-variable-reference-face 'zjl-hl-global-variable-reference-face)

(defface zjl-hl-parameters-reference-face
  '((((class color)
      (background dark))
     (:foreground "LightGoldenrod" :bold t))
    (((class color)
      (background light))
     ;(:foreground "#008080" :bold t :italic nil));theme 0
     ;(:foreground "color-36" :bold t :italic nil));theme 0
     ;(:foreground "#00afaf" :bold t :italic nil));(:foreground "color-37" :bold t :italic nil));theme 0
    (:foreground "#127974" :bold t :italic nil));(:foreground "color-37" :bold t :italic nil));theme 0
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
     (:foreground "red" :bold nil :italic nil))
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
     (:foreground "SpringGreen4" :bold nil :italic nil))
    (t
     ()))
  "*Font lock mode face for \"->\"."
  :group 'zjl-hl-faces)
(defvar zjl-hl-member-point-face 'zjl-hl-member-point-face)

(  )
(set-face-attribute 'font-lock-constant-face nil :foreground "red")

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
      (concat "\\_<" (regexp-opt '("unsigned" "int" "char" "float" "void" "VOID" "UNSIGNED" "INT" "CHAR" "FLOAT" "UINT8" "UINT16" "UINT32" "UINT64") 'words) "\\_>")))

(setq zjl-hl-warning-words-regexp
      (concat "\\_<" (regexp-opt '("FIXME" "TODO" "HACK" "fixme" "todo" "hack" "BUG" "XXX" "DEBUG")) "\\_>"))

(setq zjl-hl-macro-regexp
      (concat "\\_<\\(" "BIT[0-9]\\|SUCCESS\\|ERROR\\|FALSE\\|TRUE\\|IN\\|OUT" "\\)\\_>"))

(setq zjl-hl-c-mode-keywords (list
       '("->"
          0  zjl-hl-member-point-face keep) ;;should put before the c-operators
       (cons zjl-hl-operators-regexp (cons 0  '(zjl-hl-operators-face keep)))       
       (cons zjl-hl-brackets-regexp 'zjl-hl-font-lock-bracket-face)
       (cons zjl-hl-types-regexp 'font-lock-type-face)
       (cons zjl-hl-warning-words-regexp 'font-lock-warning-face)
       (cons zjl-hl-macro-regexp 'zjl-hl-number-face)
       '("\\(\\_<\\(?:\\(?:0x[0-9a-fA-F]*\\)\\|\\(?:[0-9]+\\(\\.[0-9]+\\)?\\)\\|\\(?:0[0-7]*\\)\\|\\(?:[01]+b\\)\\)\\_>\\)"
        0  zjl-hl-number-face keep)
       '("\\(?:\\.\\|->\\)\\(\\_<\\(?:\\w\\|\\s_\\)+\\_>\\)"
         1  zjl-hl-member-reference-face keep)
       '("\\(\\_<\\(\\w\\|\\s_\\)+\\_>\\)[ 	
]*("
         1  zjl-hl-function-call-face keep)
       ))

;;; begin lisp code
;;DarkGoldenrod4
;;SaddleBrown
;;RosyBrown



(defun zjl-hl-local-variable-and-parameter-in-func-region (start end )
  (condition-case nil         
      (save-excursion
        (let* ((local-variable-list '()) (local-constant-list '())
               (parameter-list '())                
               (func-body-begin (funcall zjl-hl-get-body-begin start end))
               (case-fold-search nil)
               items
               item
               allitems   ;;name-nned-to-clean, also some zjl-hl-variable need to clean
               (successful t)
               subdomain-start
               subdomain-end
               temppoint
               temppoint2
               temp-value
               meet-a-for
               meet-a-for-end
               (allitemsstrstart 0) ;;name need to clean
               allitemstemp ;;name need to clean
               (domainstartlist '()))
          (setq zjl-hl-local-variable-list nil)
          (setq zjl-hl-local-parameter-list nil)
          (setq zjl-hl-global-variable-list nil)
          (condition-case nil         
              (when func-body-begin
                
                (goto-char func-body-begin)
                
                (setq items (semantic-get-local-arguments))

                ;; (unless items
                ;;   (semantic-parse-region start end);;force reparse, idea from ecb-method
                ;;   (setq items (semantic-get-local-arguments)))
                
                (when items
                  (dolist (item items)
                    (when (not (member (car item) parameter-list))
                      (setq parameter-list (cons (car item) parameter-list)))
                    ))
                
                (save-excursion
                  (if parameter-list ;; should before local-variable-list since some parameter will override by local-variable
                      (progn (setq zjl-hl-local-parameter-list parameter-list)
                             (case major-mode
                               ('c-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-hl-parameters-reference-face t nil nil))
                               ('c++-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt parameter-list) "\\_>") zjl-hl-parameters-reference-face t nil nil))
                               ))
                    (when (zjl-hl-find-regexp-return-end-pos2 start func-body-begin "," nil)
                      (setq successful nil));; since if there is "," foudn in parameter area, so should have a argument, but the cedit may return nil, so.. 
                    )
                  ))
            (error (setq successful nil)))

          ;;current code if there is for (int i;;;;) then the i outside of the for()  will also been higlight.. not so good... ,the up-list will go to for();
          (condition-case nil
              (when func-body-begin                               
                (goto-char func-body-begin)
                
                
                (setq local-variable-list '())
                (setq global-variable-list '())
                
                (while (progn (setq temp-value (zjl-hl-find-regexp-return-end-pos2 (point) end "\\(\\_<return\\_>\\)\\|\\(\\(?:volatile +\\)?\\_<\\(?:\\w\\|\\s_\\)+\\_>\\(?:[ ]+\\**\\|\\**[ ]+\\)\\(?:\\_<\\(?:\\w\\|\\s_\\)+\\_>\\)[ 
]*\\(?:[=;,[][^=]\\)\\)" 12));
                              (setq temppoint (car-safe temp-value)));
                                        ;[^=] is to prevent the #if xxx == ss
                  (goto-char temppoint)
                  (when (cdr-safe temp-value);; thus skip the "..." and the return.. skp the ".." is because the (blackward-up-list) will crash if the point is in the "..".    NUMBER 12 here is because the zjl-xxxx-pos2 is has a \\(in it)
                    (save-excursion
                      (backward-up-list)    ; for(int i;...) will stop at (..  but in some compiler this i is available in the upper {}, so need the below two lines
                      (setq subdomain-start (point)) ;;this can be a begining of {, or can be beginning of ( of a "for"
                      (while (not (looking-at-p "{"))   ;; for the "for" expression -start
                        (unless meet-a-for
                          (setq meet-a-for t)
                          (forward-sexp)
                          (setq meet-a-for-end (1- (point)))
                          (backward-sexp)
                          )
                        (backward-up-list)
                        )                              ;; for the "for" expression -end

                      (forward-sexp)
                      (setq subdomain-end (point))
                      )
                    
                    (when (not (member subdomain-start domainstartlist))
                      (setq domainstartlist (cons subdomain-start domainstartlist))

                      (condition-case nil
                          (if meet-a-for
                              (progn
                                (setq meet-a-for nil)
                                (setq items (semantic-parse-region (1+ subdomain-start) meet-a-for-end 'bovine-inner-scope nil t)));; search in the for () experssion
                            (progn
                              ;;(setq items (semantic-get-local-variables))
                              (setq items (semantic-parse-region (1+ subdomain-start) (1- subdomain-end) 'bovine-inner-scope nil t));;if use the parse-region then need this line
                              ;; Without this 'bovine-inner-scope, the semantic will return func calling and variables that is global defined, or is  yyy in the xxx.ddds.yyy
                              ;; semantic-get-local-variables sometime will lose a local defination
                              ))
                        (error (progn (setq successful nil) (message "zjl-hl-local-variable-and-parameter-in-func-region error"))))         

                      ;;difference of semantic-get-local-variables and semantic-parse-region here is just for:constant and that cahce in definfination of ..-get-local-...  the variable the two get should be same. (based on my experience now 2012/03/10)
                      (when items
                        (dolist (item items)
                          (when (and (not (member (car item) local-variable-list))
                                     (not (member (car item) local-constant-list));;if use the parse-region then need this line
                                     )

                            ;;(setq local-variable-list (cons (car item) local-variable-list)) ;; if use the semantic-get-local-variables, use this line
                            (if (equal '(:constant-flag t) (nth 2 item))                        ;;if use the parse-region then need this if, other wise the above line
                                (setq local-constant-list (cons (car item) local-constant-list))
                              (setq local-variable-list (cons (car item) local-variable-list))
                              )
                            )
                          ))

                      (save-excursion
                        (when local-variable-list
                          (case major-mode
                            ('c-mode (zjl-hl-symbol-region subdomain-start subdomain-end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-hl-local-variable-reference-face t nil nil))
                            ('c++-mode (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt local-variable-list) "\\_>")  zjl-hl-local-variable-reference-face t nil nil))
                            )
                          )

                        ;;if use the parse-region then need these "when" clasue
                        (when local-constant-list
                          (case major-mode
                            ('c-mode (zjl-hl-symbol-region subdomain-start subdomain-end (concat "\\_<" (regexp-opt local-constant-list) "\\_>")  zjl-hl-number-face t nil nil))
                            )
                          )
                        )

                      )
                    (if (setq temppoint2 (car-safe (zjl-hl-find-regexp-return-end-pos2 (point) end "{\\|\\_<for\\_>")) );;for the "for" expression;; and skip the return clause. thus it won't be checked by the while xxx re-search
                        (goto-char temppoint2)
                      (goto-char end)))
                  )


                (when zjl-hl-global-variable-hl
                  (setq allitems (semantic-parse-region func-body-begin end))
                  (setq zjl-hl-local-variable-list local-variable-list)
                  (when allitems

                    (zjl-hl-test allitems nil nil nil nil)

                    ;;(setq allitemsstr (prin1-to-string allitems));;
                                        ;(prin1 allitems)
                    ;; (while (setq allitemsstrstart (string-match "\\(\".*?\"\\) \\(type\\|variable\\)"  allitemsstr  allitemsstrstart))
                    ;;   (when (not (equal (substring allitemsstr (nth 4 (match-data)) (nth 5 (match-data))) "function"))
                    ;;     (setq item (substring allitemsstr (nth 2 (match-data)) (nth 3 (match-data))))
                    ;;     (when (and    
                    ;;            (not (member item local-variable-list))
                    ;;            (not (member item local-constant-list));;if use the parse-region then need this line
                    ;;            (not (member item global-variable-list))
                    ;;            )
                    ;;       ;(setq global-variable-list (cons item global-variable-list))
                    ;;       nil
                    ;;       ))
                    ;;   )
                    (when zjl-hl-global-variable-list
                      (case major-mode
                        ('c-mode
                         (zjl-hl-symbol-region func-body-begin end (concat "\\_<" (regexp-opt zjl-hl-global-variable-list) "\\_>")  zjl-hl-global-variable-reference-face t nil nil)
                         )
                        )
                      )
                    ));;when allitems
                )
            (error (progn (setq successful nil)
                          (message "zjl-hl-local-variable-and-parameter-in-func-region error"))))
          successful))
    (error (progn (message "zjl-hl-local-variable-and-parameter-in-func-region error")
                  nil)))
  )


(defun zjl-hl-test (allitems empty nowfunction nowflag nownothing)
  (let (item )
    (dolist (item allitems)
      (when (listp item)
        (when (and (or (equal 'variable (nth 1 item))
                       (equal 'type (nth 1 item)))
                   (not (member (car item) zjl-hl-local-variable-list))
                   (not (member (car item) zjl-hl-local-parameter-list))
                   (not (member (car item) zjl-hl-global-variable-list))
                   )
          (save-excursion
            (if (and (arrayp (nth 4 item))
                     (aref (nth 4 item) 0))
                (progn
                  (goto-char (aref (nth 4 item) 0))
                  (when (not (looking-back "-> *\\|\\. *"))
                    (setq zjl-hl-global-variable-list (cons (car item) zjl-hl-global-variable-list))      
                    ))
              (setq zjl-hl-global-variable-list (cons (car item) zjl-hl-global-variable-list))      
              )
            )
          )
        (when (or (and (equal 'function (nth 1 item))
                       (setq nowfunction t))
                  (and nowfunction
                       (equal ':prototype-flag (nth 0 item))
                       (not (setq nowfunction nil))
                       (setq nowflag t)
                       )
                  (and nowflag
                       (not (setq nowflag nil))
                       (setq nownothing t)
                       )
                  (and nownothing
                       (equal "" (nth 0 item))
                       (not (setq nownothing nil))
                       (setq empty t))
                  (and empty
                       (equal ':type (nth 0 item))
                       (not (setq empty nil)))
                  )
          (zjl-hl-test item empty nowfunction nowflag nownothing))
          )
        ))
    )

(defun zjl-hl-symbol-region (start end symbol-exp symbol-face override-c-mode override-my override-nil)
  (let (target-this-end target-next-start (case-fold-search nil) pos-pair)
    (save-excursion
      (goto-char start)
      (while   (< (point) end)
        (setq pos-pair (zjl-hl-next-valid-code-pos-c (point) end))
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

(defun zjl-hl-next-valid-code-pos-c (start end) ;;; 
  (save-excursion
    (let (pos-pair find-p)
      (goto-char start)
      (save-excursion
        (when (re-search-forward "\\(\"[^\"]*\"\\)\\|\\(//.*\\)\\|\\(/\\*\\(?:.\\|
\\)*?\\*/\\)" end t) ;; in test, this regexp can handle the /* ... // ^M *// correctly. since it search after the match of the /* it the emacs c-gode just start to search the */ ----this is my guess.
          (setq pos-pair (cons (nth 0 (match-data)) (nth 1 (match-data))))
          )
        )
      pos-pair)))


(defun zjl-hl-find-regexp-return-end-pos (start end regexp);;if find, return point after the re-search, else return nil; point didn't move
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


(defun zjl-hl-find-regexp-return-end-pos2 (start end regexp &optional index);;if find, return point after the re-search, else return nil; point didn't move
  (let (target-this-end target-next-start (case-fold-search nil) find-p3 find-p pos-pair tempstart)
    (save-excursion
      (goto-char start)
      (while   (< (point) end)
        (setq tempstart (point))
        (setq find-p3 (re-search-forward (concat "\\(\"[^\"]*\"\\)\\|\\(//.*\\)\\|\\(/\\*\\(?:.\\|
\\)*?\\*/\\)\\|\\(" regexp "\\)") end t))
;;;; in test, this regexp can handle the "/* ... // ^M *//"  or "/*   define */" or "// define" correctly. since it search after the match of the /* it the emacs c-gode just start to search the */ ----this is my guess.
        (if (not find-p3)
            (goto-char end)
          (when (nth 8 (match-data))
            ;;!!!!!!!!!!!!!!!!!!!!! if use (match-data t)  meet emacs error !!!!!!!!!!!!!!!!!!!!!
            ;;don't know why there is 8th elemant(29170 29171 nil nil nil nil nil nil 29170 29171 #<buffer DxeMain.c>), and this is not seen when in debug mode
            (if index
                (setq find-p (cons (point) (nth index (match-data t))));(list nil nil)m
              (setq find-p (cons (point) nil))
              )
            (goto-char end))
          )
        ))
    find-p))

 


(defun zjl-hl-local-variable-and-parameter-region (start end)
  (save-excursion
    (hlt-unhighlight-region-for-face zjl-hl-parameters-reference-face start end)
    (hlt-unhighlight-region-for-face zjl-hl-local-variable-reference-face start end)
        
    (let ((regions (funcall zjl-hl-find-hl-var-arg-regions start end))
          regionbak
          temp-regions
          (successful t)
          (successful-all t))
      (dolist (each-region regions)
        (setq  successful-all (and (setq successful (zjl-hl-local-variable-and-parameter-in-func-region (car each-region) (cdr each-region)))
                               successful-all))
        (when successful
          (setq zjl-hl-regions-already-hl (region-list-edit-add zjl-hl-regions-already-hl each-region t))
          )
        (when (not successful)
          (message "zjlb")
          (setq regionbak each-region)
          (setq temp-regions (list each-region))
          (dolist (retry1-region zjl-hl-regions-already-retry1)
            (setq temp-regions (region-list-edit-delete temp-regions retry1-region)))
          (if temp-regions
              ;;this means the region each-region has not been in retry1, the last failure is seen first time, so so need to add to it retry1
              (setq zjl-hl-regions-already-retry1 (region-list-edit-add zjl-hl-regions-already-retry1 each-region t))
              ;;else it has already been retry once, so need to add to retry1done
            (setq zjl-hl-regions-already-retry1-done (region-list-edit-add zjl-hl-regions-already-retry1-done each-region t))
              )
          
          )
        )
      successful-all)
    ))

;;borrow from http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs

(defun zjl-hl-what-face ()
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    face))



(defun zjl-hl-func-too-big-stop-reset(thisbuffer)
  (condition-case nil
      (save-excursion
        (when (buffer-live-p thisbuffer)
          (set-buffer thisbuffer)
          (setq zjl-hl-func-too-big-stop 1)
          (setq zjl-hl-func-too-big-interval 1)
          (setq zjl-hl-func-too-big-timer-registed nil)))
    (error (message "zjl-hl-func-too-big-stop-reset error")))
  )


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
                  (dolist (hl-region zjl-hl-regions-already-retry1-done)
                    (setq temp-regions (region-list-edit-delete temp-regions hl-region)))                  
                  (when temp-regions
                    (dolist (each-region temp-regions)
                      (unless (zjl-hl-local-variable-and-parameter-region (car each-region) (cdr each-region))
                        (setq all-regions-successful nil))
                      )

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
        )        (error (message "zjl-hl-timer-do-every-time error")))
  )

(defun zjl-hl-window-scroll-hook(par1 par2)
  (condition-case nil
      (save-excursion
        (when (or (equal major-mode 'c-mode) ;;if major-mode changed,this hook may still running???
                  (equal major-mode 'c++-mode)
                  )
          (when zjl-hl-firsttime
            (add-hook 'semantic-after-partial-cache-change-hook 'zjl-hl-semantic-after-partial-cache-change-hook t)
            (setq zjl-hl-firsttime nil))

          ;;this should before if, to canel possbile hl timer, before zjl-hl-timer-do-every-time realy begin
          (when zjl-hl-timer-obj
            (cancel-timer zjl-hl-timer-obj))
          
          (if zjl-hl-firsttime-need-hl
              (progn (zjl-hl-timer-do-every-time (current-buffer))
                     (setq zjl-hl-firsttime-need-hl nil))
            
            (setq zjl-hl-timer-obj (run-with-idle-timer 0.5 nil 'zjl-hl-timer-do-every-time (current-buffer)))
            )))
(error (message "zjl-hl-window-scroll-hook error"))))

(defun zjl-hl-init ()
  (save-excursion
      (let (start end)
        (make-local-variable 'zjl-hl-func-too-big-stop)
        (make-local-variable 'zjl-hl-func-too-big-interval)
        (make-local-variable 'zjl-hl-func-too-big-timer-registed)
        (make-local-variable 'zjl-hl-firsttime)
        (make-local-variable 'zjl-hl-firsttime-need-hl)
        (make-local-variable 'zjl-hl-regions-already-hl)
        (make-local-variable 'zjl-hl-regions-already-retry1)        
        (make-local-variable 'zjl-hl-regions-already-retry1-done)        
        (make-local-variable 'zjl-hl-temp-list)
        
        (make-local-variable 'zjl-hl-local-variable-list)
        (make-local-variable 'zjl-hl-global-variable-list)
        (make-local-variable 'zjl-hl-local-parameter-list)
        (setq zjl-hl-global-variable-list nil)
        
        (setq zjl-hl-regions-already-hl nil)
        (setq zjl-hl-regions-already-retry1 nil)
        (setq zjl-hl-regions-already-retry1-done nil)        
        (setq zjl-hl-func-too-big-timer-registed nil)
        (setq zjl-hl-func-too-big-stop 0)
        (setq zjl-hl-func-too-big-interval 1)
        (setq zjl-hl-firsttime t)
        
        (make-local-variable 'zjl-hl-find-hl-var-arg-regions)
        (setq zjl-hl-find-hl-var-arg-regions
              (case major-mode
                ('c-mode 'zjl-hl-find-hl-var-arg-regions-c)
                ('c++-mode 'zjl-hl-find-hl-var-arg-regions-c)
                ))

        (make-local-variable 'zjl-hl-next-comment-pos)
        (setq zjl-hl-next-comment-pos
              (case major-mode
                ('c-mode 'zjl-hl-next-comment-pos-c)
                ('c++-mode 'zjl-hl-next-comment-pos-c)
                ))

        (make-local-variable 'zjl-hl-get-body-begin)
        (setq zjl-hl-get-body-begin
              (case major-mode
                ('c-mode 'zjl-hl-get-body-begin-c)
                ('c++-mode 'zjl-hl-get-body-begin-c)
                ))

        
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


(defun zjl-hl-semantic-update-screen-update (thisbuffer)
  (condition-case nil
      (when (buffer-live-p thisbuffer)
        (with-current-buffer thisbuffer
          (when (get-buffer-window thisbuffer 'visible)
            (zjl-hl-window-scroll-hook 1 1)  
            )
          ))
    (error (message "zjl-hl-semantic-update-screen-update error")))
  )

;idea from how ecb update method buffer
;;(add-hook (ecb--semantic-after-partial-cache-change-hook) 'ecb-update-after-partial-reparse t)
;;(add-hook (ecb--semantic-after-toplevel-cache-change-hook) 'ecb-rebuild-methods-buffer-with-tagcache t)
;; can refer to site-lisp\ecb\ecb-method-browser.el later for ecb-update-after-partial-reparse and ecb--semantic-after-partial-cache-change-hook
(defun zjl-hl-semantic-after-partial-cache-change-hook (updated-tags)
  (condition-case nil
      (when zjl-hl-regions-already-hl  ;;maybe zjl-hl-init has not hl any where
        (let (start end overlay todo)
          (dolist (tags updated-tags)
            (setq overlay (semantic-tag-overlay tags))
            (setq todo (cons (overlay-start overlay) (overlay-end overlay)))
            (setq zjl-hl-regions-already-hl (region-list-edit-delete zjl-hl-regions-already-hl todo))
            )
          
          ;;I suspect too frequent calling is the reason of dead of emacs(20-30% cpu occupied). but if just comment out this line, the hl is not do unless  movescreen.  and the dead is still seen. so the dead is not related to this..
          ;; (when (< (- (overlay-end overlay) (overlay-start overlay)) zjl-hl-toobig-not-update-size) 
          ;;   (zjl-hl-window-scroll-hook 1 1))
          
          ;; use below code to reduce the calling of zjl-hl-window-scroll-hook (test result show that that modify a word may call this function 3 times if it is called directly, like above)
          (when zjl-hl-timer-semantic-update-obj
            (cancel-timer zjl-hl-timer-semantic-update-obj))
          (when (< (- (overlay-end overlay) (overlay-start overlay)) zjl-hl-toobig-not-update-size) 
            (setq zjl-hl-timer-semantic-update-obj (run-with-idle-timer 3 nil 'zjl-hl-semantic-update-screen-update (current-buffer))))
          )
        )
    (error (message "zjl-hl-semantic-after-partial-cache-change-hook error"))))
                        

(setq zjl-hl-fun-call-notable-degree-old-value nil)

(defun zjl-hl-enable-global-all-modes ()
  (interactive)
  (when zjl-hl-make-fun-call-notable
    ;; (setq zjl-hl-fun-call-notable-degree-old-value (face-attribute 'font-lock-function-name-face :height))
    ;; (set-face-attribute 'font-lock-function-name-face nil
    ;;                 :height zjl-hl-fun-call-notable-degree)
    (setq zjl-hl-fun-call-notable-degree-old-value (face-attribute 'font-lock-function-name-face :underline))    
    (set-face-attribute 'font-lock-function-name-face nil :underline t)
    )
    (when zjl-hl-c-mode-enable-flag
    (zjl-hl-enable-global 'c-mode))
  (when zjl-hl-c++-mode-enable-flag
    (zjl-hl-enable-global 'c++-mode))

)


(defun zjl-hl-disable-global-all-modes ()
  (interactive)
  (when (and zjl-hl-make-fun-call-notable
             zjl-hl-fun-call-notable-degree-old-value)
    ;; (set-face-attribute 'font-lock-function-name-face nil :height zjl-hl-fun-call-notable-degree-old-value)
     (set-face-attribute 'font-lock-function-name-face nil :underline zjl-hl-fun-call-notable-degree-old-value)
    )
  (when zjl-hl-c-mode-enable-flag
    (zjl-hl-disable-global 'c-mode))
  (when zjl-hl-c++-mode-enable-flag
     (zjl-hl-disable-global 'c++-mode))
  )


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
    (remove-hook 'window-scroll-functions 'zjl-hl-window-scroll-hook t)
    (remove-hook 'semantic-after-partial-cache-change-hook 'zjl-hl-semantic-after-partial-cache-change-hook t)
    ))


(defun zjl-hl-get-body-begin-c (start end);;if has no begin, return nil, (zjl-hl-find-regexp-return-end-pos) may return nil
  (save-excursion
    (goto-char start)
    (car-safe (zjl-hl-find-regexp-return-end-pos2 start end "{"))))



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



(defun zjl-hl-next-comment-pos-c (start end) ;;; 
  (save-excursion
    (let (pos-pair find-p)
      (goto-char start)
      (save-excursion
        (when (re-search-forward "\\(//.*\\)\\|\\(/\\*\\(?:.\\|
\\)*?\\*/\\)" end t) ;; in test, this regexp can handle the /* ... // ^M *// correctly. since it search after the match of the /* it the emacs c-gode just start to search the */ ----this is my guess.
          (setq pos-pair (cons (nth 0 (match-data)) (nth 1 (match-data))))
          )
        )
      pos-pair)))


(provide 'zjl-hl)
;;; end lisp code 
