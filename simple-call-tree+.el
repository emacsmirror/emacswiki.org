;;; simple-call-tree+.el --- Extensions to simple-call-tree

;; Filename: simple-call-tree+.el
;; Description: extensions to simple-call-tree
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Created: 2012-11-01 21:28:07
;; Version: 0.1
;; Last-Updated: 2012-11-01 21:28:07
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/simple-call-tree+.el
;;      https://github.com/vapniks/simple-call-tree-ext
;; Keywords: programming
;; Compatibility: GNU Emacs 24.2.1
;;
;; Features that might be required by this library:
;;
;; `thingatpt' `outline-magic' `fm'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;; 
;; This library is based on simple-call-tree.el by Alex Schroeder.
;; When the command `simple-call-tree-display-buffer' is executed
;; a call tree for the functions in the current buffer will be created.
;; The user is also prompted for other files containing functions to be
;; included in the call tree.
;; The call tree is displayed in a buffer called *Simple Call Tree*,
;; which has a dedicated menu in the menu-bar showing various commands
;; and their keybindings. Most of these commands are self explanatory
;; so try them out.
;; You can navigate the call tree either by moving through consecutive
;; headers (n/p or N/P keys) or by jumping to main branches (j for branch
;; corresponding to function at point, and J to prompt for a function).
;; When you jump to a branch, it is added to `simple-call-tree-jump-ring',
;; and you can navigate your jump history using the </> keys.
;; You can also add the function under point to the jump-ring with the . key.
;; If you use a negative prefix (e.g. C--) before pressing j then the branch
;; jumped to will not be added to the jump-ring.
;; If you have fm.el (available here: http://www.damtp.cam.ac.uk/user/sje30/emacs/fm.el)
;; you can press f to toggle follow mode on/off.
;; When follow mode is on, or when you press the v or C-o key the code line
;; corresponding to the branch under point will be displayed. If it is a
;; toplevel branch then that function will be displayed, if it is a lower-level
;; branch then the corresponding function call will be displayed.
;; You can invert the tree by pressing i, and change the depth by pressing d.
;; You can also toggle narrowing to the current branch by pressing /.
;; 

;;; Installation:
;;
;; Put simple-call-tree+.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'simple-call-tree+)



;;; Customize:
;;
;; `simple-call-tree-default-recenter' : How to recenter the window after moving to another function in the "*Simple Call Tree*" 
;;                                       buffer.
;; `simple-call-tree-jump-ring-max' : Maximum number of elements in `simple-call-tree-jump-ring', before old elements are removed.


;;
;; All of the above can customized by:
;;      M-x customize-group RET simple-call-tree+ RET
;;

;;; Change log:
;;	
;; 2012/11/01
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; Alex Schroeder - the creator of the original simple-call-tree.el
;;                  (available here: http://www.emacswiki.org/emacs/simple-call-tree.el)
;;

;;; TODO
;;
;; I am going to work on a plugin for one-key.el which provides similar functionality.
;; If anyone wants to implement the following ideas, please do:
;; More reliable code for building tree (handle duplicate function names properly).
;; Commands to do query-replace-regexp on functions, and maybe other refactorings.
;; Code for marking functions (like with files in dired mode) and then applying operations to the marked functions.
;; Code for rearranging functions.

;;; Require
(require 'thingatpt)
(require 'outline-magic nil t)
(require 'fm nil t)
;;; Code:

(defgroup simple-call-tree nil
  "Simple call tree - display a simple call tree for functions in a buffer."
  :group 'tools
  :link '(url-link "http://www.emacswiki.org/SimpleCallTree"))

(defcustom simple-call-tree-default-recenter 'middle
  "How to recenter the window after moving to another function in the \"*Simple Call Tree*\" buffer.
Can be one of the following symbols: 'top 'middle 'bottom.
This variable is used by the `simple-call-tree-jump-to-function' function when no prefix arg is given."
  :group 'simple-call-tree
  :type '(choice (const :tag "Top" top)
                 (const :tag "Middle" middle)
                 (const :tag "Bottom" bottom)))

(defcustom simple-call-tree-default-valid-fonts '(font-lock-function-name-face
                                                  font-lock-variable-name-face)
  "List of fonts to use for finding objects to include in the call tree."
  :group 'simple-call-tree
  :type '(repeat face))

(defcustom simple-call-tree-default-invalid-fonts '(font-lock-comment-face
                                                    font-lock-string-face
                                                    font-lock-doc-face)
  "List of fonts that should not be in the text property of any valid token."
  :group 'simple-call-tree
  :type '(repeat face))

(defcustom simple-call-tree-major-mode-alist
  '((cperl-mode nil nil (lambda (pos)
                          (goto-char pos)
                          (beginning-of-line)
                          (looking-at "sub")) nil)
    (perl-mode nil nil (lambda (pos)
                         (goto-char pos)
                         (beginning-of-line)
                         (looking-at "sub")) nil))
  "Alist of major modes, and information to use for identifying objects for the simple call tree.
Each element is a list in the form '(MAJOR-MODE VALID-FONTS INVALID-FONTS START-TEST END-TEST) where:

MAJOR-MODE is the symbol for the major-mode that this items applies to.

VALID-FONTS is either nil or a list of fonts for finding objects for the call tree (functions/variables/etc).
If nil then `simple-call-tree-default-valid-fonts' will be used.

INVALID-FONTS is either nil or a list of fonts that should not be present in the text properties of
any objects to be added to the call tree. If nil then `simple-call-tree-default-invalid-fonts' will be used. 

START-TEST indicates how to determing the start of the next object.
If START-TEST is nil then objects are found by examining changes in font only. If START-TEST is a
function then in addition to the font check, this function will be called at the beginning of a name
and should return non-nil if that symbol is a valid object.

END-TEST indicates how to find the end of the current object when parsing a buffer for the call tree.
If END-TEST is nil then font changes will be used to determine the end of an object (by searching for the
next part of text whose font is in FONTS). If END-TEST it is t then `end-of-defun' will be used, and if
it is a function then that function will be used in the same way as `end-of-defun' (but needs no argument)."
  :group 'simple-call-tree
  :type '(repeat (list (symbol :tag "major-mode symbol")
                       (repeat :tag "Faces"
                               (face :help-echo "List of faces corresponding to items to include in the call tree."))
                       (choice :tag "Start test"
                               (const :tag "Font only" nil)
                               (function
                                :tag "Function:"
                                :help-echo "Function that evaluates to true when point is at beginning of function name."))
                       (choice :tag "End test"
                               (const :tag "Font only" nil)
                               (const :tag "end-of-defun function" t)
                               (function :tag "Other function" :help-echo "Function for finding end of object"))))
  :link '(variable-link simple-call-tree-default-valid-fonts))

;; Saves a little typing
(defmacro whilelast (&rest forms)
  `(while (progn ,@forms)))

(defmacro whilenotlast (&rest forms)
  `(while (not (progn ,@forms))))

(defun simple-call-tree-symbol-as-regexp (symbolname)
  (concat "\\_<" (regexp-opt (list symbolname)) "\\_>"))

;; Major-mode for simple call tree
(define-derived-mode simple-call-tree-mode outline-mode "Simple Call Tree"
  "The major-mode for the one-key menu buffer."
  :group 'simple-call-tree
  (setq simple-call-tree-mode-map (make-keymap)
        buffer-read-only nil)
  (outline-minor-mode 1)
  (setq outline-regexp "|\\([-<>]*\\) "
        outline-level 'simple-call-tree-outline-level)
  ;; Set keymap
  (define-key simple-call-tree-mode-map (kbd "q") 'simple-call-tree-quit)
  (if (featurep 'outline-magic)
      (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-cycle)
    (define-key simple-call-tree-mode-map (kbd "<tab>") 'outline-toggle-children))
  (define-key simple-call-tree-mode-map (kbd "a") 'show-all)
  (define-key simple-call-tree-mode-map (kbd "1") 'delete-other-windows)
  (define-key simple-call-tree-mode-map (kbd "h") 'hide-sublevels)
  (define-key simple-call-tree-mode-map (kbd "SPC") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "C-o") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "v") 'simple-call-tree-view-function)
  (define-key simple-call-tree-mode-map (kbd "<return>") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "o") 'simple-call-tree-visit-function)
  (define-key simple-call-tree-mode-map (kbd "j") 'simple-call-tree-jump-to-function)
  (define-key simple-call-tree-mode-map (kbd "J") '(lambda nil (interactive)
                                                     (setq current-prefix-arg 1)
                                                     (call-interactively 'simple-call-tree-jump-to-function)))
  (define-key simple-call-tree-mode-map (kbd "C-j") '(lambda nil (interactive)
                                                       (setq current-prefix-arg 1)
                                                       (call-interactively 'simple-call-tree-jump-to-function)))
  (define-key simple-call-tree-mode-map (kbd ".") 'simple-call-tree-jump-ring-add)
  (define-key simple-call-tree-mode-map (kbd "u") 'simple-call-tree-move-up)
  (define-key simple-call-tree-mode-map (kbd "^") 'simple-call-tree-move-up)
  (define-key simple-call-tree-mode-map (kbd "n") 'simple-call-tree-move-next)
  (define-key simple-call-tree-mode-map (kbd "p") 'simple-call-tree-move-prev)
  (define-key simple-call-tree-mode-map (kbd "C-f") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "C-b") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "<S-down>") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "<S-up>") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "N") 'simple-call-tree-move-next-samelevel)
  (define-key simple-call-tree-mode-map (kbd "P") 'simple-call-tree-move-prev-samelevel)
  (define-key simple-call-tree-mode-map (kbd "i") 'simple-call-tree-invert-buffer)
  (define-key simple-call-tree-mode-map (kbd "d") 'simple-call-tree-change-maxdepth)
  (define-key simple-call-tree-mode-map (kbd "/") 'simple-call-tree-toggle-narrowing)
  (define-key simple-call-tree-mode-map (kbd "<") 'simple-call-tree-jump-prev)
  (define-key simple-call-tree-mode-map (kbd ">") 'simple-call-tree-jump-next)
  (define-key simple-call-tree-mode-map (kbd "M-p") 'simple-call-tree-jump-prev)
  (define-key simple-call-tree-mode-map (kbd "M-n") 'simple-call-tree-jump-next)
  (define-key simple-call-tree-mode-map (kbd "w") 'widen)
  (use-local-map simple-call-tree-mode-map)
  (easy-menu-define nil simple-call-tree-mode-map "test"
  `("Simple Call Tree"
    ["Quit" simple-call-tree-quit
     :help "Quit and bury this buffer"]
    ["View Function At Point" simple-call-tree-view-function
     :help "View the function at point"
     :key "v"]
    ["Visit Function At Point" simple-call-tree-visit-function
     :help "Visit the function at point"]
    ["Jump To Branch At Point" simple-call-tree-jump-to-function
     :help "Goto the toplevel branch for the function at point"]
    ["Jump To Branch" ,(lambda nil (interactive) (setq current-prefix-arg 1)
                         (call-interactively 'simple-call-tree-jump-to-function))
     :help "Prompt for a toplevel branch to jump to"
     :keys "J"]
    ["Add To Jump Ring" simple-call-tree-jump-ring-add
     :help "Add the function at point to the jump ring"]
    ["Previous Jump" simple-call-tree-jump-prev
     :help "Goto previous function in jump ring"]
    ["Next Jump" simple-call-tree-jump-next
     :help "Goto next function in jump ring"]
    ["Parent Branch" simple-call-tree-move-up
     :help "Goto the parent branch of this branch"]
    ["Next Branch" simple-call-tree-move-next
     :help "Goto the next branch"]
    ["Previous Branch" simple-call-tree-move-prev
     :help "Goto the previous branch"]
    ["Next Branch Same Level" simple-call-tree-move-next-samelevel
     :help "Goto the next branch at the same level as this one"
     :key "N"]
    ["Previous Branch Same Level" simple-call-tree-move-prev-samelevel
     :help "Goto the previous branch at the same level as this one"
     :key "P"]
    ["Cycle Tree Visibility" outline-cycle
     :visible (featurep 'outline-magic)
     :keys "<tab>"]
    ["Toggle Children Visibility" outline-toggle-children
     :visible (not (featurep 'outline-magic))
     :keys "<tab>"]
    ["Show All" show-all
     :help "Show All Branches"
     :key-sequence "a"]
    ["Hide Sublevels" hide-sublevels
     :help "Hide Lower Level Branches"
     :key-sequence "h"]
    ["Toggle Follow mode" fm-toggle
     :help "Toggle Follow Mode - auto display of function at point"
     :visible (featurep 'fm)
     :style toggle
     :selected fm-working]
    ["Delete Other Windows" delete-other-windows
     :help "Make this window fill the whole frame"
     :key "1"]
    ["Invert Tree" simple-call-tree-invert-buffer
     :help "Invert the tree"
     :style toggle
     :selected simple-call-tree-inverted-bufferp]
    ["Change Depth" simple-call-tree-change-maxdepth
     :help "Change the depth of the tree"]
    ["Toggle Narrowing" simple-call-tree-toggle-narrowing
     :help "Toggle between narrowed/wide buffer"
     :style toggle
     :selected (simple-call-tree-buffer-narrowed-p)]))
  (setq mode-line-format
        (append
         (subseq mode-line-format 0
                 (1+ (position 'mode-line-buffer-identification
                               mode-line-format)))
         (list '(:eval (format (if simple-call-tree-inverted-bufferp
                                   " Inverted, maxdepth=%d "
                                 " maxdepth=%d ")
                               simple-call-tree-current-maxdepth)))
         (subseq mode-line-format
                 (+ 2 (position 'mode-line-buffer-identification
                                mode-line-format))))))

(defvar simple-call-tree-alist nil
  "Alist of functions and the functions they call.")

(defvar simple-call-tree-locations-alist nil
  "Alist of functions and their locations within their respective buffers.
The car of each element is a function name, and the cdr is a marker indicating the position
of the functions definition.")

(defvar simple-call-tree-inverted-bufferp nil
  "Indicates if the *Simple Call Tree* buffer is currently inverted or not.
If non-nil then children correspond to callers of parents in the outline tree.
Otherwise it's the other way around.")

(defvar simple-call-tree-current-maxdepth nil
  "The current maximum depth of the tree in the *Simple Call Tree* buffer.
The minimum value is 0 which means show top level functions only.")

(defcustom simple-call-tree-jump-ring-max 20
  "Maximum number of elements in `simple-call-tree-jump-ring', before old elements are removed."
  :group 'simple-call-tree
  :type 'integer)

(defvar simple-call-tree-jump-ring (make-ring simple-call-tree-jump-ring-max)
  "Ring to hold history of functions jumped to in *Simple Call Tree* buffer.")

(defvar simple-call-tree-jump-ring-index 0
  "The current position in the jump ring.")

;;; Functions from simple-call-tree.el (some are rewritten)

(defun simple-call-tree-add (start end alist)
  "Add tokens between START and END to ALIST.
ALIST is a list with a string identifying the function in its car,
and the list of functions it calls in the cdr."
  (dolist (entry simple-call-tree-alist)
    (goto-char start)
    (catch 'done
      (while (re-search-forward (simple-call-tree-symbol-as-regexp (car entry))
                                end t)
        (unless (not (simple-call-tree-valid-face-p))
          (setcdr alist (cons (car entry) (cdr alist)))
          (throw 'done t))))))

(defun* simple-call-tree-analyze (&optional (buffers (list (current-buffer))))
  "Analyze the current buffer, or the buffers in list BUFFERS.
The result is stored in `simple-call-tree-alist'.
Optional arg BUFFERS is a list of buffers to analyze together.
By default it is set to a list containing the current buffer."
  (interactive)
  (setq simple-call-tree-alist nil
        simple-call-tree-locations-alist nil)
  ;; First add all the functions defined in the buffers to simple-call-tree-alist.
  (let (pos oldpos count nextfunc max item endtest oldpos)
    (dolist (buf buffers)
      (with-current-buffer buf
        (font-lock-default-fontify-buffer)
        (setq pos (point-min)
              count 0)
        (save-excursion
          (while (setq nextfunc (simple-call-tree-next-func 'pos))
            (add-to-list 'simple-call-tree-alist (list nextfunc))
            (goto-char pos)
            (add-to-list 'simple-call-tree-locations-alist (cons nextfunc (point-marker)))
            (setq count (1+ count))
            (message "Identifying functions...%d:%s" count nextfunc)))))
    ;; Set variables in preparation for next part.
    (dolist (buf buffers)
      (with-current-buffer buf
        (setq pos (point-min)
              max count
              count 0
              endtest (fifth (assoc major-mode simple-call-tree-major-mode-alist)))
        (save-excursion
          ;; Loop through functions, adding called functions to associated items in simple-call-tree-alist.
          (while (setq nextfunc (simple-call-tree-next-func 'pos))
            (setq item (assoc nextfunc simple-call-tree-alist)
                  count (1+ count)
                  oldpos pos)
            (message "Identifying functions called...%d/%d" count max)
            (cond ((functionp endtest) (funcall endtest))
                  (endtest (end-of-defun))
                  ((simple-call-tree-next-func 'pos)
                   (progn (goto-char pos)
                          (previous-line)
                          (end-of-line)))
                  (t (goto-char (point-max))))
            (simple-call-tree-add oldpos (point) item))))))
  (message "simple-call-tree done"))

(defun simple-call-tree-invert (alist)
  "Invert ALIST and return the result."
  (let (result)
    (dolist (item simple-call-tree-alist)
      (let ((caller (car item))
            (callees (cdr item)))
        (dolist (callee callees)
          (let ((elem (assoc callee result)))
            (if elem
                (setcdr elem (cons caller (cdr elem)))
              (setq result (cons (list callee caller) result)))))))
    result))

;;; New functions (not in simple-call-tree.el)

(defun simple-call-tree-valid-face-p nil
  "Return t if face at point is a valid function name face, and nil otherwise."
  (let ((faces (get-text-property (point) 'face)))
    (unless (listp faces) (setq faces (list faces)))
    (not (intersection faces
                       simple-call-tree-default-invalid-fonts))))

(defun* simple-call-tree-get-function-at-point (&optional (buf "*Simple Call Tree*"))
  "Return the name of the function nearest point in the *Simple Call Tree* buffer.
If optional arg BUF is supplied then use BUF instead of the *Simple Call Tree* buffer."
  (with-current-buffer buf
    (if (equal buf "*Simple Call Tree*")
        (let* ((start (next-single-property-change (line-beginning-position) 'face))
               (end (next-single-property-change start 'face)))
          (buffer-substring-no-properties start end))
      (symbol-name (if (functionp 'symbol-nearest-point)
                       (symbol-nearest-point)
                     (symbol-at-point))))))

(defun simple-call-tree-next-func (posvar)
  "Find the next function in the current buffer after position POSVAR, and return its name.
POSVAR should be a symbol which evaluates to a position in the current buffer. If a function is found
its value will be changed to the position in the current buffer just after the function name."
  (let* ((start (eval posvar))
         (modevals (assoc major-mode simple-call-tree-major-mode-alist))
         (validfonts (or (second modevals)
                         simple-call-tree-default-valid-fonts))
         (invalidfonts (or (third modevals)
                           simple-call-tree-default-invalid-fonts))
         (test (fourth modevals))
         end)
    (while (and (not (and (memq (get-text-property start 'face) validfonts)
                          (not (memq (get-text-property start 'face) invalidfonts))
                          (or (not (functionp test))
                              (funcall test start))))
                (setq start (next-single-property-change start 'face))))
    (unless (not start)
      (setq end (next-single-property-change start 'face))
      (set posvar end)
      (unless (not end)
        (buffer-substring-no-properties start end)))))

(defun* simple-call-tree-display-buffer (&optional files)
  "Display call tree for current buffer.
If optional arg FILES is supplied it specifies a list of files to search for functions
to display in the tree.
When called interactively files will be prompted for and only functions in the current buffer will be used."
  (interactive "P")
  (let (buffers dir regexp)
    (or files
        (if (y-or-n-p "Include other files?")
            (whilelast
             (setq dir (ido-read-directory-name "Dir containing files: "))
             (list-directory dir)
             (setq regexp (read-regexp "Regexp matching filenames (RET to finish)"))
             (unless (string= regexp "")
               (mapc (lambda (name) (if (string-match regexp name)
                                        (add-to-list 'files (concat dir name))))
                     (directory-files dir))))))
    (save-excursion
      (setq buffers (loop for file in files
                          collect (find-file file))))
    (if (or (not files) (called-interactively-p))
        (add-to-list 'buffers (current-buffer)))
    (simple-call-tree-analyze buffers)
    (simple-call-tree-list-callers-and-functions)
    (setq simple-call-tree-jump-ring (make-ring simple-call-tree-jump-ring-max)
          simple-call-tree-jump-ring-index 0)))

(defun* simple-call-tree-current-function (func &optional wide)
  "Display call tree for function FUNC.
If called interactively FUNC will be set to the symbol nearest point,
unless a prefix arg is used in which case the function returned by `which-function'
will be used.
Note: `which-function' may give incorrect results if `imenu' has not been used in
the current buffer.
If a call tree containing FUNC has not already been created then the user is prompted
for which files to build the tree from.

If optional arg WIDE is non-nil then the *Simple Call Tree* buffer will be widened,
otherwise it will be narrowed around FUNC."
  (interactive (list (if current-prefix-arg
                         (which-function)
                       (simple-call-tree-get-function-at-point (current-buffer)))))
  (if (assoc func simple-call-tree-alist)
      (if (get-buffer "*Simple Call Tree*")
          (switch-to-buffer "*Simple Call Tree*")
        (simple-call-tree-list-callers-and-functions))
    (simple-call-tree-display-buffer))
  (simple-call-tree-jump-to-function func)
  (if wide (simple-call-tree-toggle-narrowing 1)
    (simple-call-tree-toggle-narrowing -1)))

(defun* simple-call-tree-list-callers-and-functions (&optional (maxdepth 2)
                                                               (funclist simple-call-tree-alist))
  "List callers and functions in FUNCLIST to depth MAXDEPTH.
By default FUNCLIST is set to `simple-call-tree-alist'."
  (switch-to-buffer (get-buffer-create "*Simple Call Tree*"))
  (if (not (equal major-mode 'simple-call-tree-mode)) (simple-call-tree-mode))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((inverted (not (equal funclist simple-call-tree-alist)))
        (maxdepth (max maxdepth 1)))
    (dolist (item funclist)
      (simple-call-tree-list-callees-recursively (car item) maxdepth 1 funclist inverted))
    (setq simple-call-tree-current-maxdepth (max maxdepth 1)
          simple-call-tree-inverted-bufferp inverted
          buffer-read-only t)))

(defun* simple-call-tree-list-callees-recursively (fname &optional (maxdepth 2)
                                                         (curdepth 1)
                                                         (funclist simple-call-tree-alist)
                                                         (inverted (not (equal funclist simple-call-tree-alist))))
  "Insert a call tree for the function named FNAME, to depth MAXDEPTH.
FNAME must be the car of one of the elements of FUNCLIST which is set to `simple-call-tree-alist' by default.
The optional arguments MAXDEPTH and CURDEPTH specify the maximum and current depth of the tree respectively.
This is a recursive function, and you should not need to set CURDEPTH."
  (let* ((callees (cdr (assoc fname funclist)))
         (arrowtail (make-string (1- curdepth) 45))
         (arrow (if inverted (concat (if (> curdepth 1) "<") arrowtail " ")
                  (concat arrowtail (if (> curdepth 1) "> " " "))))
         (face (intern-soft (format "outline-%d" (mod curdepth 8)))))
    (insert "|" arrow (propertize fname
                                  'font-lock-face (list :inherit face :underline t)
                                  'mouse-face 'highlight) "\n")
    (if (< curdepth maxdepth)
        (dolist (callee callees)
          (simple-call-tree-list-callees-recursively callee maxdepth (1+ curdepth) funclist inverted)))))

(defun simple-call-tree-outline-level nil
  "Return the outline level of the function at point.

Note: this may not give correct results if the current headline was reached by moving
the cursor manually since it depends on the match string from a previous outline movement
command. To ensure correct results do a search for `outline-regexp' from the beginning of
the line first in order to capture the match-string.
We can't do that in this function as it causes other problems with outline mode commands."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (let ((len (length (match-string 1))))
        (if (= len 0) 1 len)))))

(defun simple-call-tree-get-parent nil
  "Return the name of the parent of the function at point according to the current tree.
If there is no parent, return nil."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (if (condition-case nil
              (simple-call-tree-move-up)
            (error nil))
          (simple-call-tree-get-function-at-point)))))

(defun simple-call-tree-get-toplevel nil
  "Return the name of the toplevel parent of the subtree at point."
  (with-current-buffer "*Simple Call Tree*"
    (save-excursion
      (move-beginning-of-line nil)
      (re-search-forward outline-regexp)
      (let ((level (simple-call-tree-outline-level)))
        (if (condition-case nil
                (outline-up-heading (1- level))
              (error nil))
            (simple-call-tree-get-function-at-point))))))

;;; Major-mode commands bound to keys

(defun simple-call-tree-quit nil
  "Quit the *Simple Call Tree* buffer."
  (interactive)
  (let ((win (get-buffer-window "*Simple Call Tree*")))
    (if win (with-selected-window win
              (if fm-working (fm-toggle))
              (if (> (length (window-list)) 1)
                  (delete-window)
                (bury-buffer))))))

(defun simple-call-tree-invert-buffer (&optional maxdepth)
  "Invert the tree in *Simple Call Tree* buffer."
  (interactive "P")
  (move-beginning-of-line nil)
  (re-search-forward outline-regexp)
  (let ((depth (if current-prefix-arg (prefix-numeric-value maxdepth)
                 simple-call-tree-current-maxdepth))
        (funclist (if simple-call-tree-inverted-bufferp
                      simple-call-tree-alist
                    (simple-call-tree-invert simple-call-tree-alist)))
        (narrowedp (simple-call-tree-buffer-narrowed-p))
        (thisfunc (simple-call-tree-get-function-at-point)))
    (simple-call-tree-list-callers-and-functions depth funclist)
    (simple-call-tree-jump-to-function thisfunc)
    (if narrowedp (simple-call-tree-toggle-narrowing -1))
    (setq simple-call-tree-current-maxdepth (max depth 1))))

(defun simple-call-tree-change-maxdepth (maxdepth)
  "Alter the maximum tree depth in the *Simple Call Tree* buffer."
  (interactive "P")
  (move-beginning-of-line nil)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (depth (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
                  (floor (abs (read-number "Maximum depth to display: " 2)))))
         (funclist (if simple-call-tree-inverted-bufferp
                       (simple-call-tree-invert simple-call-tree-alist)
                     simple-call-tree-alist))
         (narrowedp (simple-call-tree-buffer-narrowed-p))
         (thisfunc (simple-call-tree-get-function-at-point))
         (thistree (simple-call-tree-get-toplevel)))
    (simple-call-tree-list-callers-and-functions depth funclist)
    (setq simple-call-tree-current-maxdepth (max depth 1))
    (simple-call-tree-jump-to-function (or thistree thisfunc))
    (if narrowedp (simple-call-tree-toggle-narrowing -1))
    (if (> level 1) (search-forward
                     thisfunc
                     (save-excursion (outline-end-of-subtree) (point)) t))))

(defun simple-call-tree-view-function nil
  "Display the source code corresponding to current header.
If the current header is a calling or toplevel function then display that function.
If it is a called function then display the position in the calling function where it is called."
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (thisfunc (simple-call-tree-get-function-at-point))
         (parent (simple-call-tree-get-parent))
         (funmark (cdr (assoc (if simple-call-tree-inverted-bufferp
                                  thisfunc
                                (or parent thisfunc))
                              simple-call-tree-locations-alist)))
         (buf (marker-buffer funmark))
         (pos (marker-position funmark)))
    (display-buffer buf)
    (with-selected-window (get-buffer-window buf)
      (goto-char pos)
      (if (> level 1)
          (whilenotlast
           (re-search-forward
            (simple-call-tree-symbol-as-regexp
             (if simple-call-tree-inverted-bufferp
                 parent
               thisfunc)))
           (simple-call-tree-valid-face-p)))
      (recenter 1))))

(defun* simple-call-tree-visit-function nil
  "Visit the source code corresponding to the current header.
If the current header is a calling or toplevel function then visit that function.
If it is a called function then visit the position in the calling function where it is called."
  (interactive)
  (move-beginning-of-line nil)
  (re-search-forward outline-regexp)
  (let* ((level (simple-call-tree-outline-level))
         (thisfunc (simple-call-tree-get-function-at-point))
         (parent (simple-call-tree-get-parent))
         (funmark (cdr (assoc (if simple-call-tree-inverted-bufferp
                                  thisfunc
                                (or parent thisfunc))
                              simple-call-tree-locations-alist)))
         (buf (marker-buffer funmark))
         (pos (marker-position funmark)))
    (pop-to-buffer buf)
    (goto-char pos)
    (if (> level 1)
        (whilenotlast
         (re-search-forward
          (simple-call-tree-symbol-as-regexp
           (if simple-call-tree-inverted-bufferp
               parent
             thisfunc)))
         (simple-call-tree-valid-face-p)))
    (recenter 1)))

(defun* simple-call-tree-jump-to-function (fnstr &optional skipring)
  "Move cursor to the line corresponding to the function with name FNSTR.
When called interactively FNSTR will be set to the function name under point,
or if called with a prefix arg it will be prompted for.
Unless optional arg SKIPRING is non-nil (which will be true if called with a negative
prefix arg) then the function name will be added to `simple-call-tree-jump-ring'"
  (interactive (list (if current-prefix-arg
                         (ido-completing-read "Jump to function: "
                                              (mapcar 'car simple-call-tree-alist))
                       (simple-call-tree-get-function-at-point))
                     (< (prefix-numeric-value current-prefix-arg) 0)))
  (let* ((narrowedp (simple-call-tree-buffer-narrowed-p)))
    (widen)
    (with-current-buffer "*Simple Call Tree*"
      (goto-char (point-min))
      (re-search-forward (concat "^" (regexp-opt (list (concat "| " fnstr))) "$"))
      (unless skipring (simple-call-tree-jump-ring-add fnstr))
      (if narrowedp (simple-call-tree-toggle-narrowing)
        (case simple-call-tree-default-recenter
          (top (recenter 0))
          (middle (recenter))
          (bottom (recenter -1))
          (t (recenter arg)))))))

(defun simple-call-tree-jump-prev nil
  "Jump to the previous function in the `simple-call-tree-jump-ring'.
The current index into the ring is `simple-call-tree-jump-ring-index'."
  (interactive)
  (unless (ring-empty-p simple-call-tree-jump-ring)
    (let ((len (cadr simple-call-tree-jump-ring)))
      (setq simple-call-tree-jump-ring-index
            (mod (1+ simple-call-tree-jump-ring-index) len))
      (simple-call-tree-jump-to-function
       (ring-ref simple-call-tree-jump-ring
                 simple-call-tree-jump-ring-index)
       t)
      (message "Position %d in jump ring history"
               simple-call-tree-jump-ring-index))))

(defun simple-call-tree-jump-next nil
  "Jump to the next function in the `simple-call-tree-jump-ring'.
The current index into the ring is `simple-call-tree-jump-ring-index'."
  (interactive)
  (unless (ring-empty-p simple-call-tree-jump-ring)
    (let ((len (cadr simple-call-tree-jump-ring)))
      (setq simple-call-tree-jump-ring-index
            (mod (1- simple-call-tree-jump-ring-index) len))
      (simple-call-tree-jump-to-function
       (ring-ref simple-call-tree-jump-ring
                 simple-call-tree-jump-ring-index)
       t)
      (message "Position %d in jump ring history"
               simple-call-tree-jump-ring-index))))

(defun simple-call-tree-jump-ring-add (fnstr)
  "Add the function at point to the jump-ring.
Adds the string FNSTR to the `simple-call-tree-jump-ring' at the position indicated by
`simple-call-tree-jump-ring-index', and reset `simple-call-tree-jump-ring-index' to 0.
When called interactively the name of the function at point is used for FNSTR."
  (interactive (list (simple-call-tree-get-function-at-point)))
  (setf (cadr simple-call-tree-jump-ring) (- (ring-length simple-call-tree-jump-ring)
                                             simple-call-tree-jump-ring-index))
  (setq simple-call-tree-jump-ring-index 0)
  (ring-insert simple-call-tree-jump-ring fnstr)
  (message "Added %s to `simple-call-tree-jump-ring'" fnstr))

(defun simple-call-tree-move-up nil
  "Move cursor to the parent of this function."
  (interactive)
  (outline-up-heading 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-next nil
  "Move cursor to the next function."
  (interactive)
  (outline-next-visible-heading 1)
  (goto-char (next-single-property-change (point) 'face)))  

(defun simple-call-tree-move-prev nil
  "Move cursor to the previous function."
  (interactive)
  (outline-previous-visible-heading 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-next-samelevel nil
  "Move cursor to the next function at the same level as the current one."
  (interactive)
  (outline-forward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-move-prev-samelevel nil
  "Move cursor to the previous function at the same level as the current one."  
  (interactive)
  (outline-backward-same-level 1)
  (goto-char (next-single-property-change (point) 'face)))

(defun simple-call-tree-buffer-narrowed-p nil
  "Return non-nil if *Simple Call Tree* buffer is narrowed."
  (with-current-buffer "*Simple Call Tree*"
    (or (/= (point-min) 1)
        (/= (point-max) (1+ (buffer-size))))))

(defun simple-call-tree-toggle-narrowing (&optional state)
  "Toggle narrowing of *Simple Call Tree* buffer.
If optional arg STATE is > 0, or if called interactively with a positive prefix arg,
then widen the buffer. If STATE is < 0 or if called interactively with a negative
prefix arg then narrow the buffer.
Otherwise toggle the buffer between narrow and wide state.
When narrowed, the buffer will be narrowed to the subtree at point."
  (interactive "P")
  (with-current-buffer "*Simple Call Tree*"
    (let ((pos (point)))
      (if (or (and state (> (prefix-numeric-value state) 0))
              (and (not state) (simple-call-tree-buffer-narrowed-p)))
          (widen)
        (if (looking-at "^| ")
            (re-search-forward "^| ")
          (re-search-backward "^| "))
        (outline-mark-subtree)
        (narrow-to-region (region-beginning) (region-end))
        (let (select-active-regions) (deactivate-mark)))
      (goto-char pos))))

(unless (not (featurep 'fm))
  (add-to-list 'fm-modes '(simple-call-tree-mode simple-call-tree-visit-function))
  (add-hook 'simple-call-tree-mode-hook 'fm-start))

(provide 'simple-call-tree+)

;;; simple-call-tree+.el ends here
