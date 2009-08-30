;;; parser babylon dictionary result, and insert into my personal dictionary.

(defun do-vocabulary-insert-word (new-word)
  "do-vocabulary-insert-word:"
  (do-vocabulary-insert-all new-word "" "" "" "")
  )

(defun do-vocabulary-insert-all (new-word new-phonetic new-term new-english new-chinese)
  "do-vocabulary-insert-all:"
  (let ((vocabulary-file-name "~/vocabulary/vocabulary.bib")
        (comment-str "@comment{"))
    (find-file vocabulary-file-name)
    (goto-char (point-min))
    (if (search-forward (concat "VOCABULARY{" new-word ",") nil t)
        (progn
          (message "%s is already in vocabulary" new-word))
      (progn
        (goto-char (point-min))
        (when (search-forward comment-str nil t)
          (previous-line)
          (beginning-of-line)
          (insert (concat "\n@VOCABULARY{" new-word ",\n"))
          (insert (concat "  key = {" new-word "},\n"))
          (insert (concat "  word = {" new-word "},\n"))
          (insert (concat "  phonetic = {\\char\"005B" new-phonetic "\\char\"005D},\n"))
          (insert (concat "  term = {" new-term "},\n"))
          (insert (concat "  english = {" new-english "},\n"))
          (insert (concat "  chinese = {" new-chinese "},\n"))
          (insert (concat "  owner = {吴晓春},\n"))
          (insert (concat "  timestamp = {" (format-time-string "%Y.%m.%d") "},\n"))
          (insert (concat "}\n"))
          (save-buffer)
          )
        )
      )


    )
  )

(defun vocabulary-insert (new-word)
  "lookup-dict-input:"
  (interactive "sWord:")
  (do-vocabulary-insert-word: new-word)
  )



(defun babylon-translation-parse (buf)
  "babylon-translation-parser:"
  (let ((word nil)(phonetic nil)(term nil)(english nil)(chinese nil))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (setq word (thing-at-point 'word))
    ;; (message   "word is    :%s" word)
    (when (re-search-forward "\\[\\(.*|| \\)*\\(.*\\)\\]" nil t)
      (setq phonetic (babylon-phonetic-adjust (match-string 2)))
      ;; (message "phonetic is:%s" phonetic)
      )
    (when (re-search-forward "(.*) \\(.*\\)[ ]*" nil t)
      (setq chinese (match-string 1))
      ;; (message "chinese is :%s" chinese)
      )
    (when (re-search-forward "^\\(n\\|adj\\|adv\\|v\\|prep\\|vi\\|vt\\|conj\\)\\.[ ]")
      (setq term (match-string 1))
      ;; (message "term is    :%s" term)

      (setq english (buffer-substring (point) (line-end-position)))
      ;; (message "english is :%s" english)
      )
    (if (and word phonetic term english chinese)
        (progn
          (message "parse from babylon translation OK!" )
          (list word phonetic term english chinese)
          )
      (progn
        (message "parse from babylon translation error!" )
        nil
        )
      )
    )
  )

(global-set-key [(control c)(b)]    'babylon-translation-to-my-dict-and-cite)

(defun babylon-translation-utility-add-cite (text-word dict-word)
  "babylon-translation-function:"
  (insert (concat "\\cite{" dict-word "}"))
  (fill-paragraph t)
  (save-buffer)
  (message "cite \"%s\" with \"%s\" OK!" text-word dict-word)
  )

(defun babylon-utility-word-suggestion (word)
  "babylon-utility-word-suggestion:"
  (let ((suggestion)
        (len (length word))
        (number-of-last-letter-to-be-ignore 1)
        )
    (setq suggest (concat (substring word 0 (- len number-of-last-letter-to-be-ignore)) "[a-z]*" ))
    )
  )

(defun babylon-translation-to-my-dict-and-cite ()
  "babylon-transation-to-dict-and-cite:"
  (interactive)
  (let* ((dict-word)
         (text-word)
         (matched-text-word)
         (original-buff (current-buffer)))
    (setq dict-word (babylon-translation-to-my-dict))
    (when dict-word
      (setq text-word dict-word)
      (switch-to-buffer original-buff)
      (if (re-search-forward (concat "\\b" dict-word "\\b") nil t)
          (progn
            (babylon-translation-utility-add-cite text-word dict-word)
            )
        (progn
          (setq text-word
                (read-regexp
                 (concat "can not find [" dict-word "], re-search? "
                         "default:[" (babylon-utility-word-suggestion dict-word) "]")
                 (babylon-utility-word-suggestion dict-word)))

          (if (re-search-forward (concat "\\b" text-word "\\b") nil t)
              (progn
                (setq matched-text-word (match-string 0))
                (babylon-translation-utility-add-cite matched-text-word dict-word)
                )
            (progn
              ;; try again from top
              (goto-char (point-min))
              (if (re-search-forward (concat "\\b" text-word "\\b") nil t)
                  (progn
                    (setq matched-text-word (match-string 0))
                    (babylon-translation-utility-add-cite matched-text-word dict-word)
                    )
                (progn
                  (message "search failed again, abort!" )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

(defun babylon-translation-to-my-dict ()
  "parse-babylon-dict:"
  (interactive)
  (let ((buf "*babylon-temp-buffer*")
        (parse-result nil)
        (word))
    ;; clear buffer first
    (if (get-buffer buf)
        (kill-buffer buf)
      (progn
        (switch-to-buffer (get-buffer-create buf))
        (clipboard-yank)))

    (setq parse-result (babylon-translation-parse buf))
    (if parse-result
        (progn
          (setq word (nth 0 parse-result))
          (do-vocabulary-insert-all word
                                    (nth 1 parse-result)
                                    (nth 2 parse-result)
                                    (nth 3 parse-result)
                                    (nth 4 parse-result))
          (kill-new word)
          (kill-buffer buf)
          (message "put [%s] into vocabulary OK, and leave it to clipboard" word)
          word
          )
      (progn
        (message "parse from babylon translation error!" )
        nil
        )
      )
    )
  )

(defvar babylon-phonetic-adjust-list nil)
(setq babylon-phonetic-adjust-list '(
                                     ("ɪ" . "i")
                                     ("ʊ" . "u")
                                     ("‚" . "\\char\"0345")
                                     ("ː" . ":")
                                     ("ɒ" . "ɔ")
                                     ))

(defun babylon-phonetic-adjust (original-str)
  "babylon-phonetic-adjust:"
  (let ((len (length original-str))
        (i 0)(ch nil)(adjust-pair)
        (out-string nil))
    (while (< i len)
      (setq ch (substring original-str i (+ i 1)))
      (setq adjust-pair (assoc ch babylon-phonetic-adjust-list))
      (if adjust-pair
          (setq out-string (concat out-string (cdr adjust-pair)))
        (setq out-string (concat out-string ch))
        )
      (setq i (+ i 1))
      )
    out-string
    )
  )

;; (defun lookup-dict (arg)
;;   "lookup-dict:"
;;   (let ((browser-program "c:/\"Program Files\"/Safari/Safari.exe")
;;         (dict-web-site "http://services.aonaware.com/DictService/Default.aspx?action=define&dict=*&query=")
;;         (command))
;;     ;; 注意：需要给参数添加引号
;;     (setq command (concat browser-program " \"" dict-web-site arg "\""))
;;     (wuxch-shell-command-background command)
;;     )
;;   )

;; (defun lookup-dict-word (&optional arg)
;;   "Copy words at point"
;;   (interactive "P")
;;   (let ((current-pos (point))
;;         (beg (progn (if (looking-back "[a-zA-Z]" 1) (backward-word 1)) (point)))
;;       	(end (progn (forward-word arg) (point))))
;;     (lookup-dict (buffer-substring beg end))
;;     (goto-char current-pos)
;;     )
;;   )

;; (global-set-key [(control c)(d)]    'lookup-dict-word)

;; (defun lookup-dict-input (qurey-word)
;;   "lookup-dict-input:"
;;   (interactive "sDict:")
;;   (lookup-dict qurey-word)
;;   )

;; (global-set-key [(control f1)]    'lookup-dict-input)

(provide 'wuxch-dict)
