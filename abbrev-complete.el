;;; abbrev-complete.el
;;;$Id: abbrev-complete.el,v 1.35.2.1 2003/10/25 10:15:54 akihisa Exp $

;; Author: Matsushita Akihisa <akihisa@mail.ne.jp>
;; Keywords: abbrev expand completion convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; abbrev mode completion function

;; If you type M-x expand-abbrev in line head, abbreviations are listed to other window.

;; And you can operate like following expression. 

;; a M-x expand-abbrev
;; your addres M-x expand-abbrev (your addres = expansion of addres)
;; as far as I can tell M-x expand-abbrev (expansion of afaict)
;; abbrev  M-x expand-abbrev (expansion by dabbrev)
;; after  M-x expand-abbrev (expansion by dabbrev)

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'abbrev-complete)

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/abbrev-complete.el

;; Customize

;; If you use with hippie-expand, remove try-expand-all-abbrevs and
;; try-expand-dabbrev, try-expand-dabbrev-all-buffers from
;; hippie-expand-try-functions-list.

;; example

;; (setq abbrev-complete-use-hippie t)
;; (setq hippie-expand-try-functions-list
;;   '(try-complete-file-name-partially
;;     try-complete-file-name
;;     try-expand-list
;;     try-expand-line
;;     try-complete-lisp-symbol-partially
;;     try-complete-lisp-symbol))

;;; History:

;; 2003/06/25: Add abbrev-complete-use-hippie
;; 2002/12/18: Add abbrev-complete-not-abbrev-completion-seg
;;             Add abbrev-complete-immediately-dabbrev-completion-seg
;; 2002/12/12: Bug fix. abbrev-complete-dabbrev-expand ()
;; 2002/12/11: Add abbrev-complete-word-seg.
;;             Add abbrev-complete-completion
;;             Add abbrev-complete-show-completion
;;             Highlight *Completions*.
;; 2002/12/07:Ver.1.00

;; TODO
;; Rewrite :)
;; Document

;;; Code:

(require 'dabbrev)

;; User variables
(defvar abbrev-complete-no-entry-dabbrev t
  "*non-nil means this program completes by dabbrev")

(defvar abbrev-complete-show-completion 'always
  "*nil means don't show *Completions* buffer.
t:Show *Completions* buffer
'always:Show *Completions* buffer even one word
'view:Show *Completions* buffer after abbrev-complete-word-seg")

(defvar abbrev-complete-word-seg "[- \"#%&'()\=~]"
  "*Segmentation of word")

(defvar abbrev-complete-completion t
  "*non-nil means, try completion for abbrev")

(defvar abbrev-complete-not-dabbrev-seg "[\\\\]"
  "*non-nil means, try completion for abbrev")

(defvar abbrev-complete-not-abbrev-completion-seg "[-\"#\%&'()\=~]"
  "*After abbrev-complete-not-abbrev-completion-seg, not complete by abbrev")

(defvar abbrev-complete-immediately-dabbrev-completion-seg "[-]"
  "*If you expand \"a-\", this program complete by dabbrev immediately")

(defvar abbrev-complete-use-hippie nil
  "*non-nil means to use hippie-expand")

(defvar abbrev-complete-add-region-to-abbrev t
  "*non-nil means to add the region text to abbrev")


;; Internal variables
(defvar abbrev-complete-word "")
(defvar abbrev-complete-start-position nil)
(defvar abbrev-complete-before-abbrev nil)
(defvar abbrev-complete-before-dabbrev nil)
(defvar abbrev-complete-table nil)
(defvar abbrev-complete-table-prev nil)
(defvar abbrev-complete-maybes nil)
(defvar abbrev-complete-maybes-all nil)
(defvar abbrev-complete-highlight-face 'highlight)
(defvar abbrev-complete-highlight-overlay nil)
(defvar abbrev-complete-do-dabbrev t)
(defvar abbrev-complete-do-abbrev t)
(defvar abbrev-complete-not-dabbrev-seg-internal "")
(defvar abbrev-completion-dabbrev-word "")
(defvar abbrev-completion-point nil)
(defvar abbrev-complete-use-hippie-internal nil)
(defvar abbrev-complete-mode-buffer "*abbrev*")
(defvar abbrev-complete-mode-parent-buffer nil)
(defvar abbrev-complete-mode-map nil)
(defvar abbrev-complete-windows-conf nil)

(or abbrev-complete-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-c\C-c"
        (function abbrev-complete-mode-add-abbrev))
      (define-key map "\C-c\C-q"
        (function abbrev-complete-mode-quit))

      (setq abbrev-complete-mode-map map)
      ))


(defadvice expand-abbrev
  (around check-abbrev-list activate)
  (if (not (eq last-command this-command))
      (setq abbrev-complete-windows-conf (current-window-configuration)))
  (cond
   ((and abbrev-complete-add-region-to-abbrev
         mark-active)
    (abbrev-complete-add-abbrev)
    )
   (t
    (abbrev-complete)
    (if abbrev-complete-before-abbrev
        (progn
          ;;(delete-region abbrev-complete-start-position abbrev-completion-point)
          ;;(insert (car abbrev-complete-maybes))
          ;;(insert abbrev-complete-word)
          (let (str tp empty)
            (if (= (point-max) (point))
                (if (= (point-min) (point))
                    (setq empty t)
                  (put-text-property (- (point) 1) (point) 'abbcomp t))
              (put-text-property (point) (+ (point) 1) 'abbcomp t))

            ad-do-it

            ;;(save-excursion (goto-char (point-max)) (insert "  ->normal \n"))
            (save-excursion
              (if empty
                  (setq abbrev-completion-point (point-max))
                (setq tp (get-text-property (point) 'abbcomp))
                (while (not tp)
                  (if (= (point) (point-max))
                      (setq tp t)
                    (forward-char 1)
                    (setq tp (get-text-property (point) 'abbcomp))))
                (setq abbrev-completion-point (point))
                (remove-text-properties (point-min) (point-max) '(abbcomp nil))))))))))

(defun abbrev-complete-add-abbrev ()
  (let ((pbuf (current-buffer))
        (mode major-mode)
        (exp
         (buffer-substring-no-properties
          (region-beginning) (region-end)))
        name table)
    (with-output-to-temp-buffer abbrev-complete-mode-buffer
      (set-buffer abbrev-complete-mode-buffer)
      (kill-all-local-variables)

      (if (functionp mode)
          (call-interactively mode)
        (error "Can't be determined major-mode!"))

      (make-local-variable 'abbrev-complete-mode-parent-buffer)
      (setq abbrev-complete-mode-parent-buffer pbuf)
      
      (use-local-map abbrev-complete-mode-map)
      (princ exp)))
  (pop-to-buffer abbrev-complete-mode-buffer))

(defun abbrev-complete-mode-add-abbrev ()
  (interactive)
  (let ((exp
         (buffer-substring-no-properties
          (point-min)
          (save-excursion
            (goto-char (point-max))
            (while (string-match "[\n ]" (buffer-substring (- (point) 1) (point)))
              (forward-char -1))
            (point))))
        name table)
    (if (or only-global-abbrevs
            (y-or-n-p "Global abbrev? "))
        (setq table global-abbrev-table)
      (if local-abbrev-table
          (setq table local-abbrev-table)
        (error "No per-mode abbrev table")))
    (setq name
          (read-string (format (if exp "abbrev for \"%s\": "
                                 "Undefine %s abbrev: ")
                               exp)))
    (set-text-properties 0 (length name) nil name)
    (if (or (null exp)
            (not (abbrev-expansion name table))
            (y-or-n-p (format "%s expands to \"%s\"; redefine? "
                              name (abbrev-expansion name table))))
        (define-abbrev table (downcase name) exp)))
  (abbrev-complete-mode-quit))

(defun abbrev-complete-mode-quit ()
  (interactive)
  ;;(switch-to-buffer abbrev-complete-mode-parent-buffer)
  (kill-buffer abbrev-complete-mode-buffer)
  (set-window-configuration abbrev-complete-windows-conf)
  )

(defun abbrev-comple-delete-window ()
  (if (and (get-buffer "*Completions*")
           (get-buffer-window (get-buffer "*Completions*")))
      (progn
        (setq abbrev-complete-table-prev nil)
        (delete-window (get-buffer-window (get-buffer "*Completions*")))
        (kill-buffer (get-buffer "*Completions*"))
        (set-window-configuration abbrev-complete-windows-conf)
        )))

(defun abbrev-complete-dabbrev-expand ()
  (abbrev-comple-delete-window)
  (cond
   ((or (not (= he-num -1)) abbrev-complete-use-hippie-internal)
    (setq abbrev-complete-use-hippie-internal nil)
    (call-interactively 'hippie-expand)
    (if he-expand-list
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list he-expand-list)))
    )
   (t
    (condition-case err
        (dabbrev-expand nil) ;; No further expansion -> error
      (error
       (delete-region abbrev-complete-start-position (point))
       (insert abbrev-complete-word)
       (if (not (= 0 (length abbrev-complete-not-dabbrev-seg-internal)))
           (insert abbrev-complete-not-dabbrev-seg-internal))
       (dabbrev--reset-global-variables)
       (setq he-num -1)
       (setq abbrev-complete-use-hippie-internal abbrev-complete-use-hippie)
       (setq abbrev-complete-before-abbrev nil)
       (setq abbrev-complete-before-dabbrev nil)
       (setq abbrev-complete-maybes
             (sort
              (append
               (all-completions abbrev-complete-word global-abbrev-table)
               (all-completions abbrev-complete-word local-abbrev-table))
              'string<))
       (setq abbrev-complete-maybes-all abbrev-complete-maybes)
       (setq abbrev-complete-table (abbrev-complete-make-list abbrev-complete-maybes-all))
       (setq abbrev-completion-point (point))
       (message "No further dynamic expansion for `%s' found" abbrev-completion-dabbrev-word)
       )))))

(defun abbrev-complete-expand-abbrev ()
  (setq abbrev-complete-before-abbrev nil)
  (setq abbrev-complete-before-dabbrev t)
  (abbrev-complete-dabbrev-expand)
  (if  (string= "*Completions*"
                (buffer-name (window-buffer (next-window))))
      (abbrev-comple-delete-window)
    ))

(defun abbrev-complete-highlight-completions ()
  (let (cbuf start end)
    (setq cbuf (current-buffer))
    (set-buffer (get-buffer "*Completions*"))
    (goto-char (point-min))
    (if (re-search-forward (concat "\\("
                                   (car abbrev-complete-maybes)
                                   "\\)[ ]+->") nil t)
        (progn
          (setq start (match-beginning 1))
          (setq end (match-end 1))))
    (if abbrev-complete-highlight-overlay
        (move-overlay abbrev-complete-highlight-overlay start end)
      (setq abbrev-complete-highlight-overlay (make-overlay start end)))
    (overlay-put abbrev-complete-highlight-overlay
                 'face abbrev-complete-highlight-face)
    (add-hook 'pre-command-hook
              (lambda ()
                (if abbrev-complete-highlight-overlay
                    (delete-overlay abbrev-complete-highlight-overlay))))
    (set-buffer cbuf)
    ))

(defun abbrev-complete-car-expand ()
  (let (str tp empty)
    (if (= (point-max) (point))
        (if (= (point-min) (point))
            (setq empty t)
          (put-text-property (- (point) 1) (point) 'abbcomp t))
      (put-text-property (point) (+ (point) 1) 'abbcomp t))
    (insert (car abbrev-complete-maybes))
    
    (ad-deactivate 'expand-abbrev)
    (expand-abbrev)
    (ad-activate 'expand-abbrev)
    
    (save-excursion
      (if empty
          (setq abbrev-completion-point (point-max))
        (setq tp (get-text-property (point) 'abbcomp))
        (while (not tp)
          (if (= (point) (point-max))
              (setq tp t)
            (forward-char 1)
            (setq tp (get-text-property (point) 'abbcomp))))
        (setq abbrev-completion-point (point))
        (remove-text-properties (point-min) (point-max) '(abbcomp nil))))

    (message "Expansion for '%s'" (car abbrev-complete-maybes))
    (if (string= "*Completions*"
                 (buffer-name (window-buffer (next-window))))
        (abbrev-complete-highlight-completions))
    ;;(setq abbrev-complete-table (cdr abbrev-complete-table))
    (setq abbrev-complete-maybes
          (cdr abbrev-complete-maybes))))

(unless (functionp 'abbrev-not-at-stop)
  (defun abbrev-not-at-stop ()
    ()))

(defun abbrev-complete-get-info (name)
  (let ((table local-abbrev-table))
    (with-temp-buffer
      (make-local-variable 'local-abbrev-table)
      (setq local-abbrev-table table)
      (erase-buffer)
      (insert name)
      ;;(end-of-line)
      (ad-deactivate 'expand-abbrev)
      (expand-abbrev)
      (ad-activate 'expand-abbrev)
      (let ((str (split-string (buffer-string) "\n")) (w (- (window-width) 15)))
        (setq str (truncate-string
                   (mapconcat (lambda (y) y) str
                              "^J") w))
        str
        ))))

(defun abbrev-complete-make-list (completion-list)
  (let ((lst completion-list) (str-list nil) (w (- (window-width) 15))
        abbrev-str)
    (while lst
      (setq abbrev-str
            (format (concat "%-" (int-to-string (/ (window-width) 3)) "s"
                            " -> %s")
                    (car lst)
                    (truncate-string
                     (abbrev-complete-get-info (car lst))
                     (/ (window-width) 3))))
      (setq str-list
            (cons
             (truncate-string
              abbrev-str
              w)
             str-list))
      (setq lst (cdr lst)))
    (reverse str-list)))

(defun abbrev-complete-display-completion ()
  (if (and
       (eq abbrev-complete-table abbrev-complete-table-prev)
       (get-buffer "*Completions*")
       (get-buffer-window (get-buffer "*Completions*")))
      (let* ((other-window-scroll-buffer (get-buffer "*Completions*"))
            (pt (save-current-buffer (set-buffer other-window-scroll-buffer)
                                     (point))))
        (if (not (= 0 (length abbrev-complete-word)))
            (save-current-buffer (set-buffer other-window-scroll-buffer)
                                 (re-search-forward (concat (car abbrev-complete-maybes)) nil t))
          (scroll-other-window)
          (if (= pt (save-current-buffer (set-buffer other-window-scroll-buffer)
                                         (point)))
              (with-output-to-temp-buffer "*Completions*"
                (display-completion-list abbrev-complete-table)
                ))))
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list abbrev-complete-table)
      )
    )
  (setq abbrev-complete-table-prev abbrev-complete-table)
  )


(defun abbrev-complete ()
  (interactive)
  (let* (pos word)
    (if abbrev-complete-use-hippie
        (require 'hippie-exp))
    ;; abbrev-complete-completion : reset
    (if (featurep 'lcomp)
        (setq lcomp-before-completion-winconf nil))
    (setq abbrev-complete-before-abbrev nil)
    (if (and abbrev-complete-completion
             (eq last-command this-command))
        (cond
         ((and (not abbrev-complete-before-dabbrev)
               (and abbrev-complete-do-dabbrev
                    abbrev-complete-no-entry-dabbrev))
          (progn
            (if (> abbrev-completion-point (point-max))
                (delete-region abbrev-complete-start-position (point))
              (delete-region abbrev-complete-start-position abbrev-completion-point))
            (insert abbrev-complete-word)
            (setq abbrev-completion-point (point))
            ))))

    (if (eq last-command this-command)
        ()
      (progn
        ;; Reset all variables
        (setq abbrev-complete-use-hippie-internal abbrev-complete-use-hippie)
        (setq he-num -1)
        (dabbrev--reset-global-variables)
        (setq abbrev-complete-do-dabbrev abbrev-complete-no-entry-dabbrev)
        (setq abbrev-complete-before-abbrev nil)
        (setq abbrev-complete-before-dabbrev nil)
        (setq abbrev-complete-not-dabbrev-seg-internal nil)
        (setq abbrev-complete-do-abbrev t)
        (setq abbrev-completion-point (point))
    
        ;; Read and delete suffix
        (cond
         ((string-match
           abbrev-complete-not-dabbrev-seg
           (char-to-string (preceding-char)))
          (progn
            (setq abbrev-complete-not-dabbrev-seg-internal (char-to-string (preceding-char)))
            (delete-region (- (point) 1) (point))
            (setq abbrev-complete-do-dabbrev nil)
            (setq abbrev-completion-point (point))
            ))
         ((string-match
           abbrev-complete-immediately-dabbrev-completion-seg
           (char-to-string (preceding-char)))
          (progn
            (setq abbrev-complete-not-dabbrev-seg-internal (char-to-string (preceding-char)))
            (setq abbrev-complete-do-abbrev nil)
            (delete-region (- (point) 1) (point))
            (setq abbrev-completion-point (point))
            ))
         )
        
        (if (or (= (line-beginning-position) (point))
                (string-match
                 abbrev-complete-word-seg
                 (buffer-substring (- (point) 1) (point))))
            ()
          (setq abbrev-completion-dabbrev-word (dabbrev--abbrev-at-point)))
        
        ))

    ;; Read word
    (if (= (line-beginning-position) (point))
        (setq pos (point))
      (if (string-match
           abbrev-complete-word-seg
           (buffer-substring (- (point) 1) (point)))
          (setq pos (point))
        (setq pos (save-excursion
                    (forward-word -1)
                    (point)))))
    
    (setq word (buffer-substring-no-properties pos (point)))
    
    ;; no complete by abbrev : abbrev-compl(point)
    (if (and (not abbrev-complete-before-dabbrev)
             (char-before pos)
             (string-match abbrev-complete-not-abbrev-completion-seg
                           (char-to-string (char-before pos)))
             (not (= pos (line-beginning-position))))
        (setq abbrev-complete-before-abbrev t))

    ;; Resete completions list, word, position
    (if (eq last-command this-command)
        ()
      (progn
        ;; Reset completion
        (setq abbrev-complete-start-position pos)
        (setq abbrev-complete-word word)
        (setq abbrev-complete-maybes
              (sort
               (append
                (all-completions word global-abbrev-table)
                (all-completions word local-abbrev-table))
               'string<))
        (setq abbrev-complete-maybes-all abbrev-complete-maybes)
        (setq abbrev-complete-table (abbrev-complete-make-list abbrev-complete-maybes-all))
        ))

    ;;body
    (if (and
         (or abbrev-complete-before-dabbrev
             (not abbrev-complete-do-abbrev) ;; complete abbrev (session)? -> no
             abbrev-complete-before-abbrev)
         abbrev-complete-no-entry-dabbrev ;; complete dabbrev (user)? -> Yes
         abbrev-complete-do-dabbrev) ;; complete dabbrev (session)? -> Yes
        (progn
          (if abbrev-complete-before-dabbrev
              (abbrev-complete-dabbrev-expand)
            (abbrev-complete-expand-abbrev)))
      
;; abbrev-complete-before-dabbrev, abbrev-complete-before-abbrev : nil
      (cond
       ;; No further abbrev-expansion
       ((null abbrev-complete-maybes)
        ;;(save-excursion (goto-char (point-max)) (insert "0 "))
        (delete-region abbrev-complete-start-position abbrev-completion-point) ;;(point))
        (insert abbrev-complete-word)
        (setq abbrev-completion-point (point))

        (if (and abbrev-complete-no-entry-dabbrev
                 abbrev-complete-do-dabbrev)
            (abbrev-complete-expand-abbrev)
          (progn
            (setq word abbrev-complete-word)
            (delete-region abbrev-complete-start-position abbrev-completion-point)
            (insert (concat abbrev-complete-word
                            abbrev-complete-not-dabbrev-seg-internal))
            (setq abbrev-completion-point (point))
            (setq abbrev-complete-maybes
                  (sort
                   (append
                    (all-completions abbrev-complete-word global-abbrev-table)
                    (all-completions abbrev-complete-word local-abbrev-table))
                   'string<))
            (setq abbrev-complete-table (abbrev-complete-make-list abbrev-complete-maybes-all))

            (setq abbrev-complete-maybes-all abbrev-complete-maybes)
            (setq abbrev-complete-before-abbrev nil)
            (message "No further expansion for `%s' found" word)
            )))

       ;; *NOT* after abbrev-complete-word-seg and only one abbrev
       ((and (not (= 0 (length word)))
             (= (length abbrev-complete-maybes) 1))
        ;;(save-excursion (goto-char (point-max)) (insert "1 "))
        (delete-region abbrev-complete-start-position abbrev-completion-point) ;;(point))
        (if (eq abbrev-complete-show-completion 'always)
            (abbrev-complete-display-completion))
        (insert (car abbrev-complete-maybes))
        (setq abbrev-completion-point (point))
        (setq abbrev-complete-before-abbrev t)
        (if  (string= "*Completions*"
                      (buffer-name (window-buffer (next-window))))
            (abbrev-complete-highlight-completions))
        (message "Expansion for '%s'" (car abbrev-complete-maybes))
        (setq abbrev-complete-maybes nil)
        (setq abbrev-complete-table nil)
        ;;(sit-for 0.5) ;; wait 0.5 sec
        )

       ;; After abbrev-complete-word-seg and only one abbrev
       ((and (= 0 (length word))
             (= (length abbrev-complete-maybes) 1))
        ;;(save-excursion (goto-char (point-max)) (insert "2 "))
        (delete-region abbrev-complete-start-position abbrev-completion-point)
        (if (eq abbrev-complete-show-completion 'always)
            (abbrev-complete-display-completion))
        (insert (car abbrev-complete-maybes))
        (setq abbrev-completion-point (point))
        (setq abbrev-complete-before-abbrev t)
        (setq abbrev-complete-maybes nil)
        (setq abbrev-complete-table nil)
        )

       ;; Default
       (t
        ;;(save-excursion (goto-char (point-max)) (insert "3 "))
        (delete-region abbrev-complete-start-position abbrev-completion-point)
        (if (or (eq abbrev-complete-show-completion 'always)
                (eq abbrev-complete-show-completion t)
                (and (eq abbrev-complete-show-completion 'view)
                     (= 0 (length word))))
            (abbrev-complete-display-completion))
        (if (and abbrev-complete-completion
                 (not (= 0 (length abbrev-complete-word))))
            (abbrev-complete-car-expand)
          (progn
            (insert (try-completion word  (mapcar 'list abbrev-complete-maybes-all)))
            (setq abbrev-completion-point (point))))
        (setq abbrev-complete-before-abbrev nil))
       ))
    ;;(save-excursion (goto-char (point-max)) (insert "\n"))

    ))


;; Delete *Completions* window
(add-hook 'pre-command-hook
          (lambda ()
            (if (and
                 (not (eq last-command this-command))
                 (string= "*Completions*"
                          (buffer-name (window-buffer (next-window)))))
                (abbrev-comple-delete-window))))


(provide 'abbrev-complete)
;;; abbrev-complete.el ends here
