;;; bpath.el --- small bookmark tool

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Commentary:
;;
;; You can bookmark a buffer path, and can call it with regex match.
;; You can open file without dired buffer open
;;
;; Installation
;; 
;; Add bpath.el to your load path 
;; add your .emacs
;;
;; (require 'bpath)
;; (define-key global-map "\C-c\C-b" 'bpath::book)
;; (define-key global-map "\C-c\C-z" 'bpath::read)
;;
;; Usage
;; 
;; 1.To save current path
;; 
;;     C-c C-b
;; 
;; 2.To call saved path
;; 
;;     C-c C-z
;;

;;; Developer: 
;;
;; kobapan <at> gmail <dot> com

;;; Keywords:
;;
;; $Id: bpath.el 5 2009-04-14 06:17:59Z kobapan $

;;; Change Log:
;;
;; 2009-04-14 not to change major mode 

;;; Code:

(defconst bpathconf "~/.bpath")

;; interactive 
(defun bpath::book (path)
  "make bookmark with current `PATH`"
  (interactive
   (list (bpath::input-path)))
  (bpath::save-path path))

(defun bpath::input-path ()
  "read current path"
  (let (path)
    (if (and (boundp 'running-xemacs) running-xemacs)
        (setq path (read-directory-name "book this path: " default-directory default-directory nil))
      (setq path (read-file-name "book this path: " default-directory default-directory nil)))))

(defun bpath::save-path (path)
  "save `PATH` in .bpath (bookmarks file)"
  (save-excursion
    (set-buffer (find-file-noselect bpathconf))
    (let ((information (concat path "\n")))
      (if (re-search-forward (concat path "\n") nil 0 nil) ; goto-char point-max if not hit
          (replace-match information) ; replace if hit
        (insert information))) ; insert in point-max if not hit
    (basic-save-buffer)
    (kill-buffer nil)))


;; interactive 
(defun bpath::read (path)
  "complement user's input `PATH` in mini buffer using .bpath (bookmarks file)"
  (interactive
   (list (bpath::read-file (bpath::select-path)))))

(defun bpath::minibuffer-mode ()
  "add key map for bpath completion to minibuffer-local-map"
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map " " 'exit-minibuffer) ; [SPACE]
    (define-key map "\t" 'exit-minibuffer) ; [TAB]
    map))

(defun bpath::select-path (&optional apropos)
  "show bookmarked path in mini buffer, which is matched with user's input\n 
with no match, recursive call\n
with single match, call `C-c C-f` with matched path\n 
with maltiple match, open selection buffer"
  (let (input lst)
    (setq input (read-from-minibuffer "apropos?: " apropos (bpath::minibuffer-mode)))
    (setq lst (bpath::study-book input))
    (cond 
     ((null lst) (bpath::select-path))
     ((null (cdr lst)) (car lst))
     (t
      (bpath::select-complete-with-buffer lst)
      (bpath::select-path input)))))

(defun bpath::study-book (needle)
  "regex search `NEEDLE` from .bpath and make complementary list"
  (save-excursion
    (set-buffer (find-file-noselect bpathconf))
    (widen)
    (goto-char (point-min))
    (let* (lst (case-fold-search t))
      (while (re-search-forward (bpath::regex-make needle) nil t)
        (setq lst (cons (buffer-substring (match-beginning 1) (match-end 1)) lst)))
      (kill-buffer nil)
      lst)))

(defun bpath::regex-make (needle)
  "make regex string\n
^$ are identifier for prefix search and/or suffix search"
  (cond ((string= "" needle) "\\(.*\\)\r?\n")
        ((= 1 (length needle)) (concat "\\(.*" needle ".*\\)\r?\n"))
        (t
         (let ((prefix (substring needle 0 1))
               (suffix (substring needle -1))
               (body (if (= 2 (length needle))
                         ""
                       (substring needle 1 -1))))
           (if (equal "^" prefix)
               (setq prefix "^\\(")
             (setq prefix (concat "\\(.*" prefix)))
           (if (equal "$" suffix)
               (setq suffix "\\)\r?\n")
             (setq suffix (concat suffix ".*\\)\r?\n")))
           (concat prefix body suffix)))))

(defun bpath::read-file (path)
  "call `C-c C-f` with `PATH`"
  (let ((enable-recursive-minibuffers t))
    (if (and (boundp 'running-xemacs) running-xemacs)
        (find-file (read-directory-name "Find file: " path path nil))
      (find-file (read-file-name "Find file: " path path nil)))))

;; buffer for select complete
(defvar bpath::completion-buffer "*<bpath-list>*")

(defun bpath::completion-buffer-mode ()
  "major mode for bpath completion buffer"
  (let ((map (make-sparse-keymap)))
    (setq major-mode 'bpath::completion-buffer-mode)
    (setq mode-name "bpath::completion-buffer-mode")
    (define-key map [double-mouse-1] 'bpath::select-into-minibuffer)
    (define-key map " " 'bpath::select-into-minibuffer)
    (define-key map "\r" 'bpath::select-into-minibuffer)    
    (define-key map "\n" 'bpath::select-into-minibuffer)    
    (setq bpath::completion-buffer-mode map)
    (use-local-map bpath::completion-buffer-mode)))

(defun bpath::select-complete-with-buffer (lst)
  "complete mini buffer from completion buffer"
  (save-excursion
    (let ((bpath::buffer (get-buffer-create bpath::completion-buffer)))
      (set-buffer bpath::buffer)
      (bpath::completion-buffer-mode)
      (setq buffer-read-only nil) ; unlock
      (erase-buffer)
      (mapcar (lambda (x) (when (file-directory-p x) (insert x "\n")))
              lst)
      (goto-char (point-min))
      (setq buffer-read-only t)   ; lock
      (pop-to-buffer bpath::buffer)
      (switch-to-completions))))

(defun bpath::select-buffer-p-kill ()
  "kill BUFFER if it is exist."
  (when (get-buffer bpath::completion-buffer)
      (kill-buffer bpath::completion-buffer)))

;; interactive
(defun bpath::select-into-minibuffer ()
  "select path and insert into minibuffer"
  (interactive)
  (let ((window-min-height 2) ans)
    (beginning-of-line)
    (setq ans (buffer-substring (point) (progn (end-of-line) (point))))
    (shrink-window 1000)
    (bpath::select-buffer-p-kill)
    (bpath::read-file ans)))

(provide 'bpath)
;;; bpath.el ends here
