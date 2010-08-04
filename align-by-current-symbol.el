;;; align-by-current-symbol.el --- Align lines containing a symbol according to that symbol.

;; Copyright (c) 2010 Chris Done. All rights reserved.

;; Author:   Chris Done <chrisdone@gmail.com>
;; Created:  14-May-2010
;; Version:  0.1
;; Keywords: convenience
;; X-URL:    http://emacswiki.org/align-by-current-symbol.el

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
;; Chris Done BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;; OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Example usage:
;;
;; (load "align-by-current-symbol.el")
;; (global-set-key (kbd "C-c C-.") 'align-by-current-symbol)
;;
;; By default it requires spaces to be around the symbol.
;;
;; Use the following to turn this off:
;;
;; (global-set-key (kbd "C-c C-.") 
;;    (lambda () 
;;       (interactive) (align-by-current-symbol t)))
;;
;; Example demonstration:
;;
;; mumumu = zotzot
;; chi = far
;; popo = k
;; zarlo => mu
;;
;; place point at `=' after `chi', hit C-c C-.:
;;
;; mumumu = zotzot
;; chi    = far
;; popo   = k
;; zarlo => mu

 (defun align-by-current-symbol (&optional add-space)
  "Indent all the lines above and below the current
   by the current non-whitespace symbol."
  (interactive "P")
  (let ((symbol (current-symbol)))
    (if symbol
        (let* ((symbol. (if add-space
                            symbol
                          (concat " " symbol " ")))
               (start (or (first/last-occurance symbol. 'search-backward 'previous-line 'line-beginning-position)
                          (line-beginning-position)))
               (end (or (first/last-occurance symbol. 'search-forward-regexp 'next-line 'line-end-position)
                        (line-end-position))))
          (align-string start end (regexp-opt (list symbol.)) (point-min))))))

(defun first/last-occurance (string search move-line line-position)
  "Find the first/last line with an occurance of a string
   in a sequence of lines containing the string."
  (setq pos nil)
  (setq first nil)
  (setq try t)
  (save-excursion
    (goto-char (funcall line-position))
    (while (or try (not (equal pos nil)))
      (setq try nil)
      (setq first pos)
      (setq pos (funcall search
                         string
                         (save-excursion (funcall move-line) (funcall line-position))
                         t)))
    (if first (line-beginning-position))))

(defun current-symbol ()
  "Get the (non-whitespace) symbol at the cursor."
  (save-excursion
    (skip-chars-forward " \t")
    (let ((start (search-backward-regexp " " nil t)))
      (if start
          (let ((start. (+ 1 start)))
            (forward-char)
            (let ((end (search-forward-regexp " " nil t)))
              (if end
                  (progn
                    (backward-char)
                    (let ((str (buffer-substring-no-properties start. (point))))
                      (if (not (string-match "[\\r\\n]" str))
                          str))))))))))

(provide 'align-by-current-symbol)
;;; align-by-current-symbol.el ends here
