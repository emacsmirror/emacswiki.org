;;; tv-utils.el --- Some useful functions for emacs 
;; 
;; Author: ThierryVolpiatto
;; Maintainer: ThierryVolpiatto
;; 
;; Created: mar jan 20 21:49:07 2009 (+0100)
;; Version: 
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.;;; tv-utils.el --- 

;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (tv-ee-index-create)

;;;;«INDEX»
;;; «.mount-and-umount-sshfs»                      (to "mount-and-umount-sshfs")
;;; «.find-file-as-root»                           (to "find-file-as-root")
;;; «.get-ip»                                      (to "get-ip")
;;; «.Crontab»                                     (to "Crontab")
;;; «.Madagascar»                                  (to "Madagascar")
;;; «.tv-yank-from-screen»                         (to "tv-yank-from-screen")
;;; «.Chrono-func»                                 (to "Chrono-func")
;;; «.cat-like-cat»                                (to "cat-like-cat")
;;; «.Show-current-face»                           (to "Show-current-face")
;;; «.mcp»                                         (to "mcp")
;;; «.Multi-read-name»                             (to "Multi-read-name")
;;; «.Delete-processes»                            (to "Delete-processes")
;;; «.move-to-window-line»                         (to "move-to-window-line")
;;; «.switch-other-window»                         (to "switch-other-window")
;;; «.scroll-both-windows»                         (to "scroll-both-windows")
;;; «.scratch-shortcut»                            (to "scratch-shortcut")
;;; «.get-elisp-index-at-point»                    (to "get-elisp-index-at-point")
;;; «.bookmarks»                                   (to "bookmarks")
;;; «.registers-config»                            (to "registers-config")
;;; «.tv-browse-url-at-point»                      (to "tv-browse-url-at-point")
;;; «.open-file-in-browser»                        (to "open-file-in-browser")
;;; «.open-file-in-firefox-from-dired»             (to "open-file-in-firefox-from-dired")
;;; «.reduce-file-name»                            (to "reduce-file-name")
;;; «.save-scratch-buffer»                         (to "save-scratch-buffer")
;;; «.Scratch-auto-save»                           (to "Scratch-auto-save")
;;; «.function-to-call-when-kill-emacs»            (to "function-to-call-when-kill-emacs")
;;; «.Iterators»                                   (to "Iterators")
;;; «.Break-long-lines»                            (to "Break-long-lines")
;;; «.Stardict»                                    (to "Stardict")
;;; «.Get-mime-type-of-file»                       (to "Get-mime-type-of-file")
;;; «.walk-in-large-files»                         (to "walk-in-large-files")
;;; «.Eshell-pager»                                (to "Eshell-pager")
;;; «.Eval-region»                                 (to "Eval-region")
;;; «.String-processing»                           (to "String-processing")
;;; «.Time-functions»                              (to "Time-functions")
;;; «.mapc-with-progress-reporter»                 (to "mapc-with-progress-reporter")
;;; «.mapcar-with-progress-reporter»               (to "mapcar-with-progress-reporter")
;;; «.Send-current-buffer-to-firefox»              (to "Send-current-buffer-to-firefox")
;;; «.Insert-date-at-point»                        (to "Insert-date-at-point")
;;; «.key-for-calendar»                            (to "key-for-calendar")
;;; «.Cvs-update-current-directory-and-compile-it» (to "Cvs-update-current-directory-and-compile-it")
;;; «.csv2org»                                     (to "csv2org")
;;; «.push-line-forward»                           (to "push-line-forward")
;;; «.get-pid-from-process-name»                   (to "get-pid-from-process-name")
;;; «.copy-files-async-with-slime»                 (to "copy-files-async-with-slime")
;;; «.Delete-files-async-with-slime»               (to "Delete-files-async-with-slime")
;;; «.Mime-types»                                  (to "Mime-types")
;;; «.getenv-improved»                             (to "getenv-improved")
;;; «.Underline»                                   (to "Underline")
;;; «.Insert-pairs»                                (to "Insert-pairs")
;;; «.Copy-boxquote»                               (to "Copy-boxquote")
;;; «.Provide»                                     (to "Provide")
;;; «.END»                                         (to "END")

;;;==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(eval-when-compile (require 'cl))

;; «mount-and-umount-sshfs» (to ".mount-and-umount-sshfs")

;;;###autoload
(defun tv-mount-sshfs (fs mp)
  (interactive (list (read-string "FileSystem: "
                                  "thievol:/home/thierry")
                     (read-directory-name "MountPoint: "
                                          "/home/thierry/"
                                          "/home/thierry/sshfs-thievol/"
                                          t
                                          "sshfs-thievol")))
    (set-buffer (get-buffer-create "*sshfs*"))
    (text-mode) (erase-buffer) (insert "=====*sshfs*=====\n\n")
    (if (> (length (cddr (directory-files mp t))) 0)
        (insert (format "Directory %s is busy, mountsshfs aborted" mp))
      (call-process-shell-command "sshfs" nil t nil (format "%s %s" fs mp))
      (if (= (length (cddr (directory-files mp t))) 0)
          (insert (format "Failed to mount remote filesystem %s on %s" fs mp))
        (insert (format "%s Mounted successfully on %s" fs mp))))
    (find-file mp)
    (display-buffer "*sshfs*"))

;;;###autoload
(defun tv-umount-sshfs (mp)
  (interactive (list (read-directory-name "MountPoint: "
                                          "/home/thierry/"
                                          "/home/thierry/sshfs-thievol/"
                                          t
                                          "sshfs-thievol")))
  (if (equal (pwd) (format "Directory %s" mp))
      (message "Filesystem is busy can't umount!")
    (progn
      (if (>= (length (cddr (directory-files mp))) 0)
          (progn
            (set-buffer (get-buffer-create "*sshfs*"))
            (erase-buffer) (insert "=====*sshfs*=====\n\n")
            (text-mode) (goto-char (point-min))
            (forward-line 2) (delete-region (point) (point-max))
            (and (call-process-shell-command "fusermount" nil t nil (format "-u %s" mp))
                 (insert (format "%s Successfully unmounted" mp)))
            (display-buffer "*sshfs*"))
        (message "No existing remote filesystem to unmount!")))))

;; «find-file-as-root» (to ".find-file-as-root")

;;;###autoload
(defun find-file-as-root (file)
  (interactive "fFindFileAsRoot: ")
  (find-file (concat "/su::" (expand-file-name file))))
(global-set-key (kbd "<f5> r") 'find-file-as-root)

;; «get-ip» (to ".get-ip")
;; get my external ip (need my python script)
;;;###autoload
(defun tv-get-ip ()
  "get my ip"
  (interactive)
  (let ((my-ip (with-temp-buffer
                 (call-process "get_IP.py" nil t nil)
                 (buffer-string))))
    (message "%s" (replace-regexp-in-string "\n" "" my-ip))))

;; «Crontab» (to ".Crontab")
;;;###autoload
(defun tv-insert-new-crontab (min hr month-day month week-day progr)
  (interactive "sMin (0-59 or *): \
\nsHour (0 23 or *) \
\nsDayOfMonth (1 31 or *) : \
\nsMonth (1 12 or *): \
\nsDayOfWeek (0 7 or *) : \nsCommand: ")
  (let ((abs-prog (with-temp-buffer
                    (call-process
                     "which"
                     nil
                     t
                     nil
                     (format "%s" progr))
                    (buffer-string))))
    (insert (concat min
                    " "
                    hr
                    " "
                    month-day
                    " "
                    month
                    " "
                    week-day
                    " "
                    abs-prog))))

;; «Madagascar» (to ".Madagascar")

;;;###autoload
(defun* tv-convert-euro-to-mga (eur-amount &key (eur-mga-value 3.77))
  (interactive "nEuroAmount: ")
  (if current-prefix-arg
      (setq eur-mga-value (read-number "NewValueFor10000MGA: ")))
  (let* ((1euro-value (/ 10000 eur-mga-value))
         (result (/ (round (* 100 (* eur-amount 1euro-value)))
                    100.00)))
    (message "%s Euros = %s Ariary (Based on 10000 MGA = %s Euros)"
             eur-amount
             (int-to-string result)
             eur-mga-value)))

;;;###autoload
(defun* tv-convert-mga-to-euro (mga-amount &key (eur-mga-value 3.77))
  (interactive "nMgaAmount: ")
  (if current-prefix-arg
      (setq eur-mga-value (read-number "NewValueFor10000MGA: ")))
  (let* ((1mga-value (/ eur-mga-value 10000))
         (result (/ (round (* 100
                              (* mga-amount 1mga-value)))
                    100.00)))
    (message "%s Ariary = %s Euro (Based on 10000 MGA = %s Euros)"
             mga-amount
             (int-to-string result)
             eur-mga-value)))


;; «tv-yank-from-screen» (to ".tv-yank-from-screen")
;;;###autoload
(defun tv-yank-from-screen ()
  "With prefix arg don't remove new lines"
  (interactive)
  (let* ((content (cat "~/.screen_exchange"))
         (len (length content)))
    (if current-prefix-arg
        (insert content)
        (insert
         (replace-regexp-in-string "\n" "" content)))
    (forward-char len)))

(global-set-key (kbd "C-c Y") 'tv-yank-from-screen)

;;;###autoload
(defun tv-copy-for-screen (beg end)
  (interactive "r")
  (let ((k-region (buffer-substring-no-properties beg end))
        (require-final-newline nil))
    (save-excursion
      (find-file "~/.screen_exchange")
      (goto-char (point-min))
      (erase-buffer)
      (insert (replace-regexp-in-string "\n" "" k-region))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(global-set-key (kbd "C-c C") 'tv-copy-for-screen)

;; «Chrono-func» (to ".Chrono-func")
;;;###autoload
(defmacro chrono-func (output fn &rest args)
  `(let ((init-time (cadr (current-time)))
         (final-time)
         (final-result))
     (setq final-result (funcall ,fn ,@args))
     (setq final-time (- (cadr (current-time)) init-time))
     (message "Time:%2s s" final-time)
     (when ,output
       final-result)))

;; «cat-like-cat» (to ".cat-like-cat")
;;;###autoload
(defmacro cat (file)
  `(let ((file-contents (with-temp-buffer
                          (insert-file-contents ,file)
                          (buffer-string))))
     file-contents))

;; «Show-current-face» (to ".Show-current-face")
;;;###autoload
(defun whatis-face ()
  (interactive)
  (message "CurrentFace: %s"
           (get-text-property (point) 'face)))

;; «mcp» (to ".mcp")
;;;###autoload
(defun lmcp (file &optional list-of-dir)
  "Copy `file' in multi directory.
At each prompt of directory add + to input
to be prompt for next directory.
When you do not add a + to directory name
input is finish and function executed"
  (interactive "fFile: ")
  (let* ((dest-list nil)
         (final-list
          (if list-of-dir
              list-of-dir
              (multi-read-name 'read-directory-name))))
    (loop for i in final-list
       do
         (copy-file file i t))))

;; «Multi-read-name» (to ".Multi-read-name")
;;;###autoload
(defun* multi-read-name (&optional (fn 'read-string))
  "Prompt as many time you add + to end of prompt.
Return a list of all inputs in `var'.
You can specify input function to use."
  (labels ((multiread ()
             (let ((stock)
                   (str (funcall fn (cond ((eq fn 'read-string)
                                           "String(add + to repeat): ")
                                          ((eq fn 'read-directory-name)
                                           "Directory(add + to repeat): ")
                                          (t
                                           "File(add + to repeat): ")))))
               (push (replace-regexp-in-string "\+" "" str) stock)
               (cond ((string-match "\+" str)
                      (push (car stock) var)
                      (multiread))
                     (t
                      (push (car stock) var)
                      (nreverse var))))))
    (let (var)
      (multiread))))

;; «Delete-processes» (to ".Delete-processes")
;;;###autoload
(defun tv-kill-process (process)
  (interactive (list (completing-read "Process: "
                                      (mapcar #'process-name
                                              (process-list))
                                      nil
                                      t)))
  (delete-process (get-process process)))

;; «move-to-window-line» (to ".move-to-window-line")
;;;###autoload
(defun screen-top (&optional n)
  "Move the point to the top of the screen."
  (interactive "p")
  (move-to-window-line (or n 0)))

;;;###autoload
(defun screen-bottom (&optional n)
  "Move the point to the bottom of the screen."
  (interactive "P")
  (move-to-window-line (- (prefix-numeric-value n))))

(global-set-key [C-left] 'screen-top)

(global-set-key [C-right] 'screen-bottom)

;; «switch-other-window» (to ".switch-other-window")
;; C-x o inversé de n windows(optional)
;;;###autoload
(defun other-window-backward (&optional n)
  "retourne sur n window(s) precedent(s)"
  (interactive "p")
  (other-window (- (or n 1))))
(global-set-key (kbd "C-<") 'other-window-backward)

;; «scroll-both-windows» (to ".scroll-both-windows")
(defun scroll-both-windows (&optional n)
  (interactive "P")
  (let ((num (prefix-numeric-value n)))
    (scroll-down num)
    (scroll-other-window (- num))))
(global-set-key (kbd "C-;") #'(lambda ()
                                (interactive)
                                (scroll-both-windows)))

(global-set-key (kbd "C-:") #'(lambda ()
                                (interactive)
                                (scroll-both-windows -1)))

;; «scratch-shortcut» (to ".scratch-shortcut")
;;;###autoload
(defun go-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "<f11> s c") 'go-to-scratch)

;; «get-elisp-index-at-point» (to ".get elisp-index at point")
;;;###autoload
(defun tv-get-index-at-point (expr)
  (interactive
   (list (read-from-minibuffer "Search: "
                               nil
                               nil
                               nil
                               nil
                               (thing-at-point 'sexp))))
  (elisp-index-search expr))


;; «bookmarks» (to ".bookmarks")

(setq bookmark-bmenu-toggle-filenames nil)
(add-hook 'bookmark-bmenu-mode-hook 'hl-line-mode)

(defvar tv-bookmark-active-p nil)
;;;###autoload
(defun tv-toggle-window-bookmark ()
  (interactive)
  (if tv-bookmark-active-p
      (progn
        (setq tv-bookmark-active-p nil)
        (quit-window t)
        (delete-window))
      (setq tv-bookmark-active-p t)
      (split-window-horizontally 30)
      (bookmark-bmenu-list)
      (switch-to-buffer "*Bookmark List*")))

(global-set-key (kbd "<f5> b") 'tv-toggle-window-bookmark)

;; «registers-config» (to ".registers-config")

;; Redefine append-to-register with a "\n"
;;;###autoload
(defun tv-append-to-register (register start end &optional delete-flag)
  "Append region to text in register REGISTER.
With prefix arg, delete as well.
Called from program, takes four args: REGISTER, START, END and DELETE-FLAG.
START and END are buffer positions indicating what to append."
  (interactive "cAppend to register: \nr\nP")
  (let ((reg (get-register register))
        (text (filter-buffer-substring start end)))
    (set-register
     register (cond ((not reg) text)
                    ((stringp reg) (concat reg "\n" text))
                    (t (error "Register does not contain text")))))
  (if delete-flag (delete-region start end)))

(global-set-key (kbd "C-x r a") 'tv-append-to-register)

(global-set-key (kbd "C-x r L") 'list-registers)

;; «tv-browse-url-at-point» (to ".tv-browse-url-at-point")
;;;###autoload
(defun tv-browse-url-at-point (&optional FF)
  (interactive "P")
  (let ((url (thing-at-point 'sexp)))
    (if current-prefix-arg
        (when (y-or-n-p (format "FF-Browse <%s>" url))
          (browse-url-firefox url))
        (when (y-or-n-p (format "W3m-Browse <%s>" url))
          (w3m-browse-url url)))))
(global-set-key (kbd "C-c u") 'tv-browse-url-at-point)

;; «open-file-in-browser» (to ".open-file-in-browser")
;;;###autoload
(defun tv-find-file-as-url ()
  (interactive)
  (if current-prefix-arg
      (dired-open-file-firefox)
      (w3m-find-file (expand-file-name (thing-at-point 'filename)))))

;; «open-file-in-firefox-from-dired» (to ".open-file-in-firefox-from-dired")
;;;###autoload
(defun dired-open-file-firefox (&optional fname)
  (interactive)
  (let ((file (or fname (dired-filename-at-point))))
    (browse-url-firefox (concat "file://" file))))

;; «reduce-file-name» (to ".reduce-file-name")

(defun* tv-reduce-file-name (fname level &key unix-close expand)
    "Reduce file-name by `level' from end or beginning depending `level' value.
if `level' is positive reduce by end else by beginning.
`level' is an integer.
`unix-close' boolean, if true close filename with /.
`expand' boolean, if true expand-file-name."
  (let* ((exp-fname (expand-file-name fname))
         (fname-list (split-string (if expand exp-fname fname) "/" t))
         (len (length fname-list))
         (pop-list (if (< level 0)
                       (subseq fname-list (* level -1))
                       (subseq fname-list 0 (- len level))))
         (result (mapconcat #'(lambda (x) x) pop-list "/")))
     (if unix-close
         (if expand
             (if (< level 0)
                 (concat "../" result "/")
                 (concat "/" result "/"))
             (if (string-match "~/" result)
                 (concat result "/")
                 (if (< level 0)
                     (concat "../" result "/")
                     (concat "/" result "/"))))
         (if expand
             (if (< level 0)
                 (concat "../" result "/")
                 (concat "/" result "/"))
             (if (string-match "~/" result)
                 (concat result "/")
                 (if (< level 0)
                     (concat "../" result "/")
                     (concat "/" result "/")))))))

;; «save-scratch-buffer» (to ".save-scratch-buffer")
;(defvar save-scratch-file "~/.emacs.d/save-scratch.el")
(defvar scratch-auto-save-file "~/.emacs.d/save-scratch.el")
;;;###autoload
(defun tv-save-scratch (&optional append)
  (interactive "P")
  (with-current-buffer "*scratch*"
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let* ((beg (point))
             (end (point-max))
             (buffer-contents (buffer-substring beg end)))
        (find-file scratch-auto-save-file)
        (if (or current-prefix-arg
                append)
            (progn
              (goto-char (point-max))
              (insert buffer-contents)
              (save-buffer)
              (kill-buffer (current-buffer)))
            (erase-buffer)
            (goto-char (point-min))
            (insert buffer-contents)
            (save-buffer)
            (kill-buffer (current-buffer)))))))

(defun tv-restore-scratch-buffer ()
  (with-current-buffer "*scratch*"
    (goto-char (point-min))
    ;(forward-line)
    (erase-buffer)
    (when (file-exists-p scratch-auto-save-file)
      (insert-file-contents scratch-auto-save-file))
    (if (eq (point-min) (point-max))
        (insert (format ";; SCRATCH BUFFER\n;; %s\n\n" (make-string 14 ?=))))))

(define-key lisp-interaction-mode-map (kbd "C-x C-s") 'tv-save-scratch)

;; «Scratch-auto-save» (to ".Scratch-auto-save")
;; (find-fline "~/.scratch-autosave/scratch.el")
(defvar tv-use-autosave-in-scratch nil)
(defun run-autosave-in-scratch ()
  (with-current-buffer "*scratch*"
    (make-auto-save-file-name)
    (auto-save-mode 1)
    (setq buffer-auto-save-file-name scratch-auto-save-file)
    (message "AutoSave in Scratch buffer enabled")))

;; «function-to-call-when-kill-emacs» (to ".function-to-call-when-kill-emacs")
;; (find-fline "~/bin/stop-emacs-daemon" "emacsclient")
;; (find-fline "~/.emacs.d/emacs-config-laptop/.emacs.el" "'emacs-startup-hook")
(defun save-before-kill ()
  (save-some-buffers)
  (tv-save-scratch)
  (desktop-save "~/"))

;; «Iterators» (to ".Iterators")
(require 'iterator)

;; «Break-long-lines» (to ".Break-long-lines")
(defun tv-break-long-lines (beg end)
  (interactive "r")
  (with-current-buffer (current-buffer)
    (let ((start-pos beg)
          (end-pos end))
      (deactivate-mark)
      (goto-char start-pos)
      (forward-char 80)
      (catch 'break-line
        (while t
          (when (> (point) end-pos)
            (throw 'break-line nil))
          (catch 'continue
            (newline)
            (dotimes (i 80)
              (when (eq (point) end-pos)
                (throw 'continue nil))
              (forward-char)))
          (forward-word))))))

;; «Stardict» (to ".Stardict")

(require 'showtip)

(defun translate-at-point ()
  (interactive)
  (let* ((word (thing-at-point 'word))
         (result
          (eshell-command-result (format "sdcv %s" word))))
    (showtip result)))

(global-set-key (kbd "C-c t r") 'translate-at-point)

;; «Get-mime-type-of-file» (to ".Get-mime-type-of-file")
(defun file-mime-type (fname)
  "Get the mime-type of fname"
  (interactive "fFileName: ")
  (if (interactive-p)
      (message "%s" (mailcap-extension-to-mime (file-name-extension fname t)))
      (mailcap-extension-to-mime (file-name-extension fname t))))

;; «walk-in-large-files» (to ".walk-in-large-files")
;; Iterator required. Experimental, need work.
(defun walk-file-by-blocks (file start bsize &optional mode)
  (interactive "fFile: \nnByte-offset: \nnBlockSize: ")
  (let* ((action)
         (pos)
         (size)
         (count) ; blocks counter
         (line-num (line-number-at-pos start))
         (cur-iter (flines-iterator file start bsize line-num)))
    (unless mode
      (if current-prefix-arg
          (setq mode (intern (read-string "Mode: ")))))
    (unwind-protect
         (switch-to-buffer "*walk-file*")
      (if mode
          (funcall mode))
      (iter-map #'(lambda (x)
                    (setq line-num (string-to-number (first x)))
                    (insert (concat (first x) ": " (second x) "\n")))
                cur-iter)
      (setq count 0)
      (setq pos (point))
      (setq size bsize)
      (setq cur-iter (flines-iterator file pos bsize line-num))
      (catch 'break
        (while t
          (setq action (read-event "(n)ext (e)dit (q)uit"))
          (case action
            ('?n
             (incf count)
             (delete-region (point-min) (point-max))
             (goto-char (point-min))
             (iter-map #'(lambda (x)
                           (setq line-num (string-to-number (first x)))
                           (insert (concat (first x) ": " (second x) "\n")))
                       cur-iter)
             (setq pos (+ pos (point)))
             (setq cur-iter (flines-iterator file pos bsize line-num)))
            ('?e
             (let ((str-pos (number-to-string pos))
                   (str-size (number-to-string size))
                   (region (buffer-substring (point-min) (point-max))))
               (save-excursion
                 (find-file (concat (file-name-sans-extension file)
                                    str-pos
                                    "-"
                                    str-size
                                    (file-name-extension file t)))
                 (insert region)
                 (save-buffer)
                 (kill-buffer (current-buffer)))))
            ('?q
             (throw 'break nil)))))
      (kill-buffer))))

;; «Eshell-pager» (to ".Eshell-pager")
;; Iterator required. Experimental, need work.
(defun tv-less (com)
  "A pager for eshell."
  (let ((A (eshell-iterator com))
        (beg)
        (init-line)
        action last-elm
        (end)
        (flag-move nil))
    (unwind-protect
         (with-current-buffer (current-buffer)
           (forward-line)
           (setq beg (point))
           (setq init-line (line-number-at-pos))
           (dotimes (n 20)
             (insert (concat (iter-next A) "\n")))
           (catch 'break
             (while t
               (setq action (read-event "(n)ext (b)ack (q)uit"))
               (case action
                 ('?n
                  (block restart
                    (if flag-move
                        (progn
                          (forward-line 20)
                          (when (eq (point) end)
                            (setq flag-move nil)
                            (return-from restart)))
                        (dotimes (n 20)
                          (setq last-elm (iter-next A))
                          (insert (concat last-elm "\n"))))
                    (setq end (point))))
                 ('?b
                  (setq flag-move t)
                  (unless (<= (line-number-at-pos) init-line)
                    (forward-line -20)))
                 ('?q (throw 'break nil))))))
      (delete-region beg end))))

;; «Eval-region» (to ".Eval-region")
(defun tv-eval-region (beg end)
  (interactive "r")
  (eval-region beg end t))

;; «String-processing» (to ".String-processing")

(defmacro *string (str num)
  `(let ((str-lis))
    (dotimes (n ,num)
      (push ,str str-lis))
    (mapconcat #'(lambda (i) i) str-lis "")))
    
(defmacro +string (str-0 str-1)
  `(let ((str-lis (list ,str-0 ,str-1)))
     (mapconcat #'(lambda (i) i) str-lis "")))

;;; «Time-functions» (to ".Time-functions")
;;;###autoload
(defun* tv-time-date-in-n-days (days &optional (separator "-"))
  "Return the date in string form in n +/-days
like ==>2008.03.16"
  (let* ((days-in-sec (* 3600 (* (+ days) 24)))
         (interval-days-sec (if `(< ,days 0)
                                (+ (float-time (current-time)) days-in-sec)
                              (- (float-time (current-time)) days-in-sec)))
         (sec-to-time (seconds-to-time interval-days-sec))
         (time-dec (decode-time sec-to-time))
         (new-date ""))
    (setq new-date (concat
                    (int-to-string (nth 5 time-dec))
                    separator
                    (substring (int-to-string (/ (float (nth 4 time-dec)) 100)) 2)
                    separator
                    (if (< (length (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2)) 2)
                        (concat (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2) "0")
                      (substring (int-to-string (/ (float (nth 3 time-dec)) 100)) 2))))
    new-date))

;;;###autoload
(defun* tv-cur-date-string (&optional (separator "-"))
  "Return current date under string form ==>2008.03.16"
  (interactive)
  (let ((year (nth 5 (decode-time (current-time))))
        (month (nth 4 (decode-time (current-time))))
        (day (nth 3 (decode-time (current-time))))
        (str-day-date ""))
    (setq str-day-date
          (concat (int-to-string year)
                  separator
                  (if (< (length (substring (int-to-string (/ (float month) 100)) 2)) 2)
                      (concat (substring (int-to-string (/ (float month) 100)) 2) "0")
                      (substring (int-to-string (/ (float month) 100)) 2))
                  separator
                  (if (< (length (substring (int-to-string (/ (float day) 100)) 2)) 2)
                      (concat (substring (int-to-string (/ (float day) 100)) 2) "0")
                      (substring (int-to-string (/ (float day) 100)) 2))))
    str-day-date))

;; «mapc-with-progress-reporter» (to ".mapc-with-progress-reporter")
(defmacro mapc-with-progress-reporter (message func seq)
  `(let* ((max (length ,seq))
          (progress-reporter
           (make-progress-reporter
            (message ,message)
            0 max))
          (count 0))
     (mapc #'(lambda (x)
               (progress-reporter-update progress-reporter count)
               (funcall ,func x)
               (incf count))
           ,seq)
     (progress-reporter-done progress-reporter)))

;; «mapcar-with-progress-reporter» (to ".mapcar-with-progress-reporter")
(defmacro mapcar-with-progress-reporter (message func seq)
  `(let* ((max (length ,seq))
          (progress-reporter
           (make-progress-reporter
            (message ,message)
            0 max))
          (count 0)
          (new-seq))
     (setq new-seq (mapcar #'(lambda (x)
                               (progress-reporter-update progress-reporter count)
                               (incf count)
                               (funcall ,func x))
                           ,seq))
     (progress-reporter-done progress-reporter)
     new-seq))

;; «Send-current-buffer-to-firefox» (to ".Send-current-buffer-to-firefox")
(defun tv-htmlize-buffer-to-firefox ()
  (interactive)
  (let* ((fname (concat "/tmp/" (symbol-name (gensym "emacs2firefox"))))
         (html-fname (concat fname ".html"))
         (buffer-contents (buffer-substring (point-min) (point-max))))
    (save-excursion
      (find-file fname)
      (insert buffer-contents)
      (write-file fname)
      (save-buffer)
      (kill-buffer))
    (htmlize-file fname html-fname)
    (browse-url-firefox (format "file://%s" html-fname))))

(defun tv-htmlfontify-buffer-to-firefox ()
  (interactive)
  (let ((fname (concat "/tmp/" (symbol-name (gensym "emacs2firefox")) ".html")))
    (htmlfontify-buffer)
    (with-current-buffer (current-buffer)
      (write-file fname))
    (browse-url-firefox (format "file://%s" fname))))

;; <2009-04-09 Jeu.>
(defun tv-htmlfontify-region-to-firefox (beg end)
  (interactive "r")
  (let ((fname (concat "/tmp/" (symbol-name (gensym "emacs2firefox")) ".html"))
        (buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (htmlfontify-buffer)
      (write-file fname))
    (browse-url-firefox (format "file://%s" fname))))

(global-set-key (kbd "<f5> h f b") 'tv-htmlfontify-buffer-to-firefox)
(global-set-key (kbd "<f5> h f r") 'tv-htmlfontify-region-to-firefox)

;; «Insert-date-at-point» (to ".Insert-date-at-point")
(setq calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"])
(global-set-key (kbd "C-c i") 'tv-org-cycle-date-at-point)
;; Modified: 2009/04/15 08:43
(defun tv-org-cycle-date-at-point ()
  "Insert current date at point and cycle.
Work in all buffers and  all modes."
  (interactive)
  (let ((beg (point))
        action
        (n 0))
    (flet ((day (str-date)
             (let ((cur-date (mapcar #'string-to-number
                                     (split-string str-date "-"))))
               (setq cur-date (append (cdr cur-date) (list (car cur-date))))
               (calendar-day-name cur-date t)))
           (ins-cur-day ()
             (let* ((day-date (tv-cur-date-string))
                    (day-name (day day-date)))
               (insert (concat "<" day-date " " day-name ".>"))))
           (ins-next-day (i)
             (let* ((fn (if (< i 0) '1- '1+))
                    (new-date (tv-time-date-in-n-days (funcall fn n)))
                    (day-name (day new-date)))
               (delete-region beg (point))
               (insert (concat "<" new-date " " day-name ".>"))))
           (insert-time ()
             (let* ((all (decode-time (current-time)))
                    (h (number-to-string (nth 2 all)))
                    (m (number-to-string (nth 1 all)))
                    (end (point))
                    time)
               (cond ((eq (length m) 1)
                      (setq m (concat "0" m))
                      (setq time (concat " " h ":" m)))
                     ((eq (length h) 1)
                      (setq h (concat "0" h))
                      (setq time (concat " " h ":" m)))
                     ((and (eq (length m) 1)
                           (eq (length h) 1))
                      (setq m (concat "0" m))
                      (setq h (concat "0" h))
                      (setq time (concat " " h ":" m)))
                     (t
                      (setq time (concat " " h ":" m))))
               (cond ((looking-back "\\.>")
                      (forward-char -1)
                      (insert time))
                     ((and (looking-back ":[0-9]\\{2\\}")
                           (looking-at ">"))
                      (re-search-backward "\\.")
                      (delete-region (1+ (point)) end)
                      (forward-char 2))
                     ((looking-back "/[0-9]\\{2\\}")
                      (insert time))
                     ((and (looking-at "\\>")
                           (looking-back ":[0-9]\\{2\\}"))
                      (re-search-backward "/[0-9]\\{2\\}")
                      (delete-region (+ (point) 3) end)
                      (forward-char 3)))))
           (remove-org-stuff ()
             (let (beg
                   date
                   (end (point)))
               (when (re-search-backward "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
                 (setq date (replace-regexp-in-string "-" "/" (match-string 0)))
                 (setq beg (1- (point)))
                 (delete-region beg end)
                 (if (looking-at ">")
                     (progn
                       (delete-char 1)
                       (insert date))
                     (insert date))
                 (sit-for 1)))))
      (ins-cur-day)
      (catch 'break
        (while t
          (setq action (read-event "(>)next (<)prec (d)ate/only (t)oggle-time (q)uit"))
          (case action
            ('?>
             (ins-next-day 1)
             (setq n (incf n)))
            ('?<
             (ins-next-day -1)
             (setq n (decf n)))
            ('?t
             (insert-time))
            ('?d
             (remove-org-stuff))
            ('?q
             (throw 'break nil))))))))

;; «key-for-calendar» (to ".key-for-calendar")
(defvar tv-calendar-alive nil)
(defun tv-toggle-calendar ()
  (interactive)
  (if (not tv-calendar-alive)
      (progn
        (calendar)
        (setq tv-calendar-alive t))
      (calendar-exit)
      (setq tv-calendar-alive nil)))
(global-set-key (kbd "<f5> c") 'tv-toggle-calendar)

;; «Cvs-update-current-directory-and-compile-it» (to ".Cvs-update-current-directory-and-compile-it")
;; <2009-04-17 Ven. 16:15>
(require 'pcvs)
(defun update-cvs-dir-and-compile ()
  "Cvs update current dir and compile it."
  (interactive)
  (let ((dir default-directory))
    (cvs-update dir nil)
    (while (not (equal cvs-mode-line-process "exit"))
      (sit-for 1))
    (message "Wait compiling %s..." dir)
    (shell-command "make")))

;; «csv2org» (to ".csv2org")
;; Convert a (french) csv file to org table.
(require 'csv2org)

;; «push-line-forward» (to ".push-line-forward")
(defun tv-push-line-forward-or-backward (&optional up)
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (or current-prefix-arg
            up)
        (delete-char -1)
        (newline))))
(global-set-key (kbd "C-c n") 'tv-push-line-forward-or-backward)
(global-set-key (kbd "C-c p") #'(lambda ()
                                  (interactive)
                                  (tv-push-line-forward-or-backward t)))


;; «get-pid-from-process-name» (to ".get-pid-from-process-name")
(defun tv-get-pid-from-process-name (process-name)
  (let ((process-list (list-system-processes)))
    (catch 'break
      (dolist (i process-list)
        (let ((process-attr (process-attributes i)))
          (when process-attr
            (when (string-match process-name
                                (cdr (assq 'comm
                                           process-attr)))
              (throw 'break
                i))))))))

;; «copy-files-async-with-slime» (to ".copy-files-async-with-slime")

(setq slime-enable-evaluate-in-emacs t)
(defvar tv-slime-copy-files-list nil)
(defvar tv-slime-copy-dest-dir nil)
(defun tv-slime-dired-copy-files-or-dir-async (&optional file-list dir)
  "Copy a list of marked files-or-dirs async to a given directory using slime.
give `file-list' as a list of files.
`dir' is a regular directory name.
`slime-enable-evaluate-in-emacs' have to be non--nil."
  (interactive)
  (slime-check-connected)
  (cond (file-list ;; non interactive call
         (setq tv-slime-copy-files-list file-list))
        ((eq major-mode 'dired-mode)
         (setq tv-slime-copy-files-list (mapcar #'(lambda (x)
                                                    (if (file-directory-p x)
                                                        (file-name-as-directory x)
                                                        x))
                                                (dired-get-marked-files)))))
  (if dir ;; non interactive call
      (setq tv-slime-copy-dest-dir dir)
      (setq tv-slime-copy-dest-dir
            (expand-file-name
             (read-directory-name (format "Copy %s files to directory: " (length tv-slime-copy-files-list))
                                  nil nil nil
                                  (when dired-dwim-target
                                    (dired-dwim-target-directory))))))
  (slime-eval-async '(cl:loop
                      with l = (swank::eval-in-emacs 'tv-slime-copy-files-list)
                      with d = (swank::eval-in-emacs 'tv-slime-copy-dest-dir) 
                      for i in l
                      do
                      (tv-fad-extensions:copy-file-or-dir i d)) 
                    (lambda (result)
                      ;(message "%S" result)
                      (message "%s files copied to %s"
                               (length tv-slime-copy-files-list)
                               tv-slime-copy-dest-dir))))

;; «Delete-files-async-with-slime» (to ".Delete-files-async-with-slime")

(defvar tv-slime-delete-files-list nil)
(defun tv-slime-dired-delete-files-async (&optional file-list)
  (interactive)
  (slime-check-connected)
  (cond (file-list
         (setq tv-slime-delete-files-list file-list))
        ((eq major-mode 'dired-mode)
         (setq tv-slime-delete-files-list (dired-get-marked-files)))
        (t
         (setq tv-slime-delete-files-list (read-file-name "File: "))))
  (if (y-or-n-p (format "Really delete %s files?" (length tv-slime-delete-files-list))) 
      (slime-eval-async '(cl:loop
                          with l = (swank::eval-in-emacs 'tv-slime-delete-files-list)
                          for i in l
                          do
                          (cl:delete-file i))
                        (lambda (result)
                          (message "%s files deleted" (length tv-slime-delete-files-list))))))

;; «Mime-types» (to ".Mime-types")
(defun tv-dired-mime-type ()
  (interactive)
  (let ((fname (dired-filename-at-point)))
    (message "%s" (file-mime-type fname))))

;; «getenv-improved» (to ".getenv-improved")
(defun tv-get-env (&optional elm)
  "Why? because `getenv' is incomplete."
  (interactive)
  (let* ((env-info (with-temp-buffer
                     (call-process "env" nil t nil)
                     (split-string (buffer-string) "\n")))
         (env-alist (loop for i in env-info
                       for l = (split-string i "=")
                       collect (cons (car l) (cdr l))))
         (env-keys (loop for i in env-info
                      for l = (split-string i "=")
                      collect (car l)))
         (env (or elm
                  (completing-read "Env: "
                                   env-keys))))
    (if (interactive-p)
        (message "%s" (cadr (assoc env env-alist)))
        (cadr (assoc env env-alist)))))

;; «Underline» (to ".Underline")
(defun tv-underline (beg end &optional underline-str)
  (interactive "r")
  (let ((str (buffer-substring beg end))
        (ustr (cond (underline-str)
                    (current-prefix-arg
                     (read-string "String: "))
                    (t
                     "=")))
         (len-str (- end beg))
         (len-bol-pt (- beg (point-at-bol))))
    (forward-line 1)
    (insert (*string " " len-bol-pt))
    (insert (concat (*string ustr len-str) "\n"))))

;; «Insert-pairs» (to ".Insert-pairs")
;; (find-fline "/usr/share/emacs/23.1.50/lisp/emacs-lisp/lisp.el" "defun insert-pair")
(setq parens-require-spaces t)

(defun tv-insert-double-quote (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))
(global-set-key (kbd "M-\"") 'tv-insert-double-quote)

(defun tv-insert-double-backquote (&optional arg)
  (interactive "P")
  (insert-pair arg ?\` ?\'))
(global-set-key (kbd "C-M-\`") 'tv-insert-double-backquote)

(defun tv-move-pair-forward ()
  (interactive)
  (catch 'break
    (while t
      (setq action (read-event "`('insert (q)uit"))
      (case action
        ('?\(
         (skip-chars-forward " ")
         (insert "(")
         (forward-symbol 1)
         (insert ")"))
        ('?q
         (throw 'break nil))))))
(global-set-key (kbd "C-M-(") 'tv-move-pair-forward)

(defun tv-insert-double-quote-and-close-forward ()
  (interactive)
  (catch 'break
    (while t
      (setq action (read-event "`\"' insert (q)uit"))
      (case action
        ('?\"
         (skip-chars-forward " \n")
         (insert "\"")
         (forward-symbol 1)
         (insert "\""))
        ('?q
         (throw 'break nil))))))
(global-set-key (kbd "C-M-\"") 'tv-insert-double-quote-and-close-forward)

(defun tv-insert-pair-and-close-forward ()
  (interactive)
  (insert "(")
  (catch 'break
    (while t
      (setq action (read-event "`)'insert (q)uit"))
      (case action
        ('?\)
         (unless (looking-back "(")
           (delete-char -1))
         (skip-chars-forward " ")
         (forward-symbol 1)
         (insert ")"))
        ('?q
         (forward-char -1)
         (throw 'break nil))))))
(global-set-key (kbd "C-M-)") 'tv-insert-pair-and-close-forward)

;; «Copy-boxquote» (to ".Copy-boxquote")
(defun boxquote-copy-box-without-box (beg end)
  (interactive "r")
  (let (new-beg
        new-end
        next-end
        title)
    (deactivate-mark)
    (save-excursion
      (goto-char beg)
      (setq title (boxquote-get-title)))
    (boxquote-unbox-region beg end)
    (setq new-end (point))
    (goto-char beg)
    (while (not (looking-at ".*[^ \n]"))
      (forward-char 1))
    (setq new-beg (point))
    (goto-char new-end)
    (while (not (looking-back ".*[^ \n]"))
      (forward-char -1))
    (setq next-end (point))
    (copy-region-as-kill new-beg next-end)
    (boxquote-region beg new-end)
    (when title
      (boxquote-title title))))

;; Binded to <f7> q c
;; (find-fline "~/.emacs.d/emacs-config-laptop/.emacs.el" "require 'boxquote")

;; «Chromium-google-navigator» (to ".Chromium-google-navigator")

(defun browse-url-chromium (url)
  (interactive (list (read-from-minibuffer "URL: "
                                           nil nil nil 'minibuffer-history
                                           (if current-prefix-arg
                                               (thing-at-point 'url)
                                               "http://www.google.fr"))))
  (let ((args "--enable-plugins")
        (proc (concat "chromium " url)))
    (message "Starting Chromium...") (sit-for 0.2)
    (apply 'start-process proc nil "chromium-bin"
           (list args url))
    (set-process-sentinel (get-process proc)
                          #'(lambda (process event)
                              (message
                               "%s is %s"
                               process
                               event)))))

(global-set-key (kbd "<f7> c") 'browse-url-chromium)

(defun w3m-view-this-page-in-chrome ()
  (interactive)
  (let ((url (w3m-print-current-url)))
    (browse-url-chromium url)))


;; «Provide» (to ".Provide")
(provide 'tv-utils)

;; «END» (to ".END")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tv-utils.el ends here
;; Local Variables:
;; eev-comment-prefix: ";"
;; ee-anchor-format: "«%s»"
;; End:
