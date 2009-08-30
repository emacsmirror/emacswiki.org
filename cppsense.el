;;; cppsense.el --- Simple, heuristic, tag-based C++ coding assistant

;; Copyright (C) 2007  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Needs a TAG table containing all files of the project.
;;
;; F1 is the only key to use.
;;
;; F1 on a symbol goes to the declaration of the symbol. (Returning to
;; the place before the jump can be done with standard etags key M-*)
;;
;; C-F1 on a symbol shows the definition of the symbol in a tooltip.
;;
;; F1 on an include line goes to the include file.
;;
;; F1 shows completions if it can offer any for the position before
;; point.
;;
;; F1 anywhere else goes to the source file from a header or vica
;; versa.


;; Tested on Emacs 22.


;; TODO
;;  
;;  - it thinks delete var; is a variable declaration
;;
;;  - info about current class in tooltip
;;
;;  - inherited members
;;
;;  - private/public
;;
;;  - inline functions
;;
;;  - overloaded functions
;;
;;  - highlight match in tooltip
;;
;;  - offer locally declared variables and function arguments for
;;    completion
;;
;;  - this->
;;
;;  - a()->
;;
;;  - automatic refresh of index
;;
;;
;;  - class matching regexp should be a constant or in a function
;;


;; Assumptions:
;;
;; - there is a TAG file
;;
;; - the TAG file contains all files of the project
;;
;; - in the TAG file include files are indexed first and then source
;;   files
;;
;; - include files are in the tag file in include order
;;
;; - classes are defined in a file with the same name (fallback to
;; - TAGS if not true)
;;



(require 'cl)
(require 'etags)


(defvar cppsense-filename-hash (make-hash-table :test 'equal))

(defvar cppsense-debug nil "*Debug")

(defvar cppsense-last-cmd-time (current-time))

(setq cppsense-popup-threshold 0.3)

(defconst cppsense-symbol-regexp "\\(\\<\\(\\sw\\|\\s_\\)+\\>\\)")

(defvar cppsense-markers nil)

(defvar cppsense-completion-frame nil)

(defvar cppsense-selection-overlay nil)

(defvar cppsense-completions-buffer "*cpssense-completions*")

(defvar cppsense-enable-automatic-completion nil)

(defvar cppsense-completion-symbol-beginning-position nil)

(defvar cppsense-completion-editing-commands
  '(self-insert-command
    c-electric-backspace))


(defvar cppsense-header-extensions '("h" "hh"))

(defvar cppsense-source-extensions '("cc" "cpp" "icc"))

(defvar cppsense-include-regexp "#include \\(<\\|\"\\)\\(.*\\)\\(>\\|\"\\)")

(defvar cppsense-include-regexp-paren 2)

(defvar cppsense-symbol-cache (make-hash-table :test 'equal))


(defun cppsense-initialize ()
  (interactive)

  (if tags-file-name
      (find-file-noselect tags-file-name)
    (call-interactively 'visit-tags-table))

  (message "Collecting file names from TAGS file...")

  (clrhash cppsense-filename-hash)

  (with-current-buffer (get-file-buffer tags-file-name)
    (goto-char (point-min))
    (while (search-forward "\f\n" nil t)
      (looking-at "\\(.*\\),[0-9]+")
      (let* ((file (match-string 1))
             (name (file-name-nondirectory file))
             (dir (file-name-directory file))
             (directories (gethash name cppsense-filename-hash)))
        ;; remove / from the end od dir
        (setq dir (substring dir 0 -1))
        (puthash name
                 (if directories
                     (append directories (list dir))
                   (list dir))
                 cppsense-filename-hash))))

    (message "Done."))


(add-hook 'c++-mode-hook 'cppsense-setup)
(add-hook 'c-mode-hook 'cppsense-setup)

(defun cppsense-setup ()
  (if (eq (hash-table-count cppsense-filename-hash) 0)
      (cppsense-initialize))
  (if cppsense-enable-automatic-completion
      (add-hook 'post-command-hook 'cppsense-try-automatic-completion nil t))
  (local-set-key (kbd "<f1>") 'cppsense-do-something-clever)
  (local-set-key (kbd "C-<f1>") 'cppsense-show-popup-help-for-symbol))


(defun cppsense-do-something-clever ()  
  (interactive)
  (if (eq last-command 'cppsense-do-something-clever)
      (pop-tag-mark)

    (let ((origin (cppsense-create-point-marker)))
      (unwind-protect
          (progn
            (setq cppsense-markers nil)
            (some (lambda (func)
                    (funcall func))
                  '(cppsense-include-handler
                    cppsense-symbol-handler
                    cppsense-completion-handler
                    cppsense-nothing-handler)))

        (cppsense-destroy-markers))
      (require 'etags)
      (ring-insert find-tag-marker-ring origin))))


(defun cppsense-show-popup-help-for-symbol ()
  (interactive)
  (cppsense-do-something-clever))


(defun cppsense-include-handler ()
  (if (save-excursion
        (beginning-of-line)
        (looking-at cppsense-include-regexp))

      (let* ((file (match-string cppsense-include-regexp-paren))
             (path (cppsense-get-include-file-path file t)))
        (if (not path)
            (error "File %s not found." file)

          (find-file path)
          (message "File %s found." file)))))


(defun cppsense-get-include-file-path (file &optional check-current-dir)
  (let* ((dirname (file-name-directory file))
         (name (file-name-nondirectory file)))
    (if (and check-current-dir
             (file-readable-p name))
        (expand-file-name file)

      (let ((dirs (gethash name cppsense-filename-hash))
            dir)
        (when dirs
          (setq dir
                (if dirname
                    (progn
                      ;; remove /
                      (setq dirname (substring dirname 0 -1))
                      (some (lambda (d)
                              (if (equal dirname
                                         (file-name-nondirectory d))
                                  d))
                            dirs))

                  (if (not (cdr dirs))
                      (car dirs)

                    ;; the directories are assumed to
                    ;; be in the order of search, so
                    ;; choosing the first one automatically
                    (car dirs))))
          ;; (cppsense-completing-read 
          ;; (concat "There are more than one candidates. "
          ;; "Select which one to visit: ")
          ;; dirs))))

          (if dir
              (concat dir "/" name)))))))


(defun cppsense-symbol-handler ()
  (let* ((symbol-info (cppsense-get-symbol-info-at-point))
         (marker (cdr (assoc 'declaration symbol-info))))
    (when symbol-info
      (if (not marker)
          (error "Can't find declaration of symbol: %s" 
                 (cdr (assoc 'symbol symbol-info)))
          
        (if (eq this-command 'cppsense-show-popup-help-for-symbol)
            (cppsense-show-popup-help (cppsense-get-code-context-from-marker marker))

          (switch-to-buffer (marker-buffer marker))
          (goto-char marker)))


      (move-marker marker nil)

      t)))


(defun cppsense-get-symbol-info-at-point ()
  (when (and (not (eobp))
             (let ((syntax (char-syntax (char-after))))
               (or (eq syntax ?w)
                   (eq syntax ?_))))
    (let* ((symbol-begin (cppsense-get-symbol-begin-at-point))
           (symbol-end (cppsense-get-symbol-end-at-point))
           (symbol (buffer-substring-no-properties symbol-begin symbol-end))
           (marker (some (lambda (func)
                           (funcall func symbol symbol-begin symbol-end))
                         '(cppsense-find-symbol-with-heuristics
                           cppsense-lookup-symbol-in-tag-file))))
      (cppsense-debug (concat "cppsense-get-symbol-info-at-point: "
                              "symbol:%s   begin: %s   end: %s   marker: %s")
                      symbol symbol-begin symbol-end marker)
      (list (cons 'symbol symbol)
            (cons 'begin symbol-begin)
            (cons 'end symbol-end)
            (cons 'declaration marker)))))


(defun cppsense-get-declaration-marker-of-symbol-at-point ()
  (assoc-default 'declaration (cppsense-get-symbol-info-at-point)))


(defun cppsense-find-symbol-with-heuristics (symbol symbol-begin symbol-end)
  (if (cppsense-within-function)
      (cppsense-find-symbol-within-function symbol symbol-begin symbol-end)
    
    (if (cppsense-within-class)
        (save-excursion
          (c-beginning-of-defun)
          (when (looking-at (concat "class\\s-*" cppsense-symbol-regexp))
            (goto-char (match-beginning 1))
            (let* ((members (cppsense-get-members-of-class
                             (cppsense-create-point-marker)))
                   (marker (if members
                               (some (lambda (member)
                                       (if (equal (plist-get member 'name) symbol)
                                           (plist-get member 'declaration)))
                                     members))))
              (if marker
                  (cppsense-get-member-function-definition symbol marker)))))

    ;; outside of a function
    (cppsense-try-class symbol))))


(defun cppsense-find-symbol-within-function (symbol symbol-begin symbol-end)
  (save-excursion
    (if (save-excursion
          (goto-char symbol-end)
          (looking-at "("))

        ;; function call
        (if (cppsense-member-p symbol-begin)
            (cppsense-get-qualified-member-function symbol)

          ;; standalone function call. assume it's a member
          ;; function in the same file
          (goto-char (point-min))
          (when (re-search-forward (concat "::\\<\\(" symbol "\\)\\>(") nil t)
            (goto-char (match-beginning 1))
            (cppsense-debug (concat "cppsense-find-symbol-with-heuristics: "
                                    "member function in same file found: %s")
                            symbol)
            (cppsense-create-point-marker)))
          
      ;; we don't know it what it is yet

      (if (cppsense-member-p symbol-begin)
          ;; qualified member variable
          (cppsense-get-qualified-member-info-at-point symbol)

        (c-beginning-of-defun)
        (if (or (re-search-forward (concat "\\s-\\(" symbol "\\)[,)]")
                                   (save-excursion
                                     (re-search-forward "^{"))
                                   t)
                (re-search-forward (concat "^\\s-*\\S-.*\\s-\\<\\(" symbol "\\)\\>\\s-*\\(;\\|[,=\[].*;\\)")
                                   (save-excursion
                                     (c-end-of-defun)
                                     (point))
                                   t))
            ;; it's a function argument or local variable
            (progn (goto-char (or (match-beginning 1)
                                  (match-beginning 0)))
                   (cppsense-debug (concat "cppsense-find-symbol-with-heuristics: "
                                           "function arg or local variable found: %s")
                                   symbol)
                   (cppsense-create-point-marker))

          (or (cppsense-try-class symbol)
              (progn
                (cppsense-debug (concat "cppsense-find-symbol-with-heuristics: "
                                        "assume symbol is an unqualified member variable: %s")
                                symbol)
                ;; extract object type from function definition
                (when (re-search-forward (concat cppsense-symbol-regexp "::")
                                         (save-excursion
                                           (search-forward "{" nil t)                       
                                           (forward-char -1)
                                           (forward-sexp)
                                           (point))
                                         t)
                  (goto-char (match-beginning 0))
                  (let ((decl (cppsense-get-declaration-marker-of-symbol-at-point)))
                    (if decl
                        (cppsense-get-member-of-class decl symbol)))))))))))


(defun cppsense-member-p (symbol-begin)
  (or (and (eq (char-before symbol-begin) ?>)
           (eq (char-before (1- symbol-begin)) ?-))
      (and (eq (char-before symbol-begin) ?:)
           (eq (char-before (1- symbol-begin)) ?:))
      (eq (char-before symbol-begin) ?.)))
  

(defun cppsense-get-qualified-member-function (function)
  (let ((marker (cppsense-get-qualified-member-info-at-point function)))
    (if marker
        (cppsense-get-member-function-definition function marker))))


(defun cppsense-get-member-function-definition (function marker)
  (cppsense-debug "cppsense-get-member-function-definition: %s %s"
                  function marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (c-beginning-of-defun)
      (let ((class (cppsense-get-class-on-current-line)))
        (if (not class)
            (progn (message "Can't find class containing function %s" function)
                   marker)

          (let* ((regexp (concat "\\<" class "::\\(" function "\\)\\s-*\\(\n\\s-*\\)?("))
                 (inline (save-excursion
                          (goto-char (point-min))
                          (if (re-search-forward regexp nil t)
                              (match-beginning 1)))))
            (if inline
                (progn
                  (goto-char inline)
                  (cppsense-create-point-marker))
                           
              (let ((source-file (cppsense-get-other-file)))
                (if (not source-file)
                    (progn (message "Can't find source file for class %s" class)
                           marker)
                                        
                  (with-current-buffer (find-file-noselect
                                        source-file)
                    (save-excursion
                      (goto-char (point-min))
                      (if (not (let (found)
                                 (while (and (not found)
                                             (re-search-forward regexp nil t))
                                   (unless (save-match-data
                                             (cppsense-within-function))
                                     (setq found t)))
                                 found))
                          (progn (cppsense-debug "Can't find function %s in source file of class %s"
                                                 function class)
                                 marker)

                        (goto-char (match-beginning 1))
                        (cppsense-debug "Definition of function %s is found."
                                        function)
                        (cppsense-create-point-marker)))))))))))))


(defun cppsense-get-qualified-member-info-at-point (membername)
  (let ((parent-decl (cppsense-get-parent-class)))
    (if parent-decl
        (cppsense-get-member-of-class parent-decl membername))))


(defun cppsense-get-parent-class ()
  (save-excursion
    ;; go to preceding symbol
    (cppsense-go-to-beginning-of-symbol)
    (skip-syntax-backward ".")
    (forward-char -1)

    (let ((parent-decl (cppsense-get-declaration-marker-of-symbol-at-point)))
      (when parent-decl
        (with-current-buffer (marker-buffer parent-decl)
          (save-excursion
            (goto-char parent-decl)

            (unless (cppsense-get-class-on-current-line)
              ;; if not a class already then find out what it is
              (backward-word)
              (setq parent-decl (cppsense-get-declaration-marker-of-symbol-at-point)))

            parent-decl))))))


(defun cppsense-get-member-of-class (class-marker membername)
  (let ((members (cppsense-get-members-of-class class-marker)))
    (if members
        (or (some (lambda (member)
                    (if (equal (plist-get member 'name) membername)
                        (plist-get member 'declaration)))
                  members)
            (progn
              (cppsense-debug (concat "cppsense-get-member-of-class: "
                                      "member not found: %s")
                              membername)
              nil)))))


(defun cppsense-get-members-of-class (marker &optional dont-garbage-collect-markers)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (let ((class (cppsense-get-class-on-current-line)))
        (if (not class)
            (progn (message "Not a class or a struct: %s"
                            (cppsense-get-symbol-name marker))
                   nil)

          (let* ((end (save-excursion
                        (search-forward "{" nil t)                       
                        (forward-char -1)
                        (forward-sexp)
                        (point)))
                 (member-regexp (concat cppsense-symbol-regexp
                                        "\\(;" ; data member
                                        "\\|"
                                        ;; dont put ; at the
                                        ;; end, the function declaration
                                        ;; can be broken into multiple
                                        ;; lines
                                        "\\s-*(" ; function
                                        "\\)"))
                 members)

            (while (re-search-forward member-regexp end t)
              (unless (or
                       ;; it's a comment
                       (eq (get-text-property (point) 'face)
                           'font-lock-comment-face)
                       ;; skip constructors/destructors
                       (equal (match-string 1) class)
                       ;; skip friend classes
                       (save-match-data
                         (save-excursion
                           (beginning-of-line)                      
                           (looking-at "\\s-*friend class"))))
                (let* ((name (match-string-no-properties 1))
                       (declaration (save-excursion
                                      (save-match-data
                                        (goto-char (match-beginning 1))
                                        (if dont-garbage-collect-markers
                                            (point-marker)
                                          (cppsense-create-point-marker)))))
                       (info (list 'name name 'declaration declaration)))

                  (when (save-excursion
                        (goto-char (match-end 1))
                        (looking-at "\\s-*("))
                    (let ((definition (cppsense-get-member-function-definition
                                       name declaration)))
                      (if dont-garbage-collect-markers
                          (setq definition (copy-marker definition)))
                      (setq info (append info (list 'definition definition)))))

                  (push info members)))
              ;; avoid matching anything else on the same line
              (forward-line 1))

            (cppsense-debug (concat "cppsense-get-members-of-class: "
                                    "members found: %s") members)

            members))))))


(defun cppsense-get-class-on-current-line ()
  (save-excursion (beginning-of-line)
                  (if (looking-at (concat "\\s-*\\(class\\|struct\\)\\s-+"
                                          cppsense-symbol-regexp))
                      (match-string 2))))                              


(defun cppsense-try-class (symbol)
  (let* ((file (concat symbol ".hh"))
         (dirs  (gethash file cppsense-filename-hash)))
    (if dirs
        (with-current-buffer (find-file-noselect (concat (car dirs) "/" file))
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward (concat "^\\(class\\|struct\\)\\s-+\\<\\("
                                             symbol
                                             "\\)\\>") nil t)
              (goto-char (match-beginning 2))
              (cppsense-debug (concat "cppsense-try-class: "
                                      "class found: %s") symbol)
              (cppsense-create-point-marker)))))))


(defun cppsense-lookup-symbol-in-tag-file (symbol symbol-begin symbol-end)
  (let (file text line pos)
    (with-current-buffer (get-file-buffer tags-file-name)
      (save-excursion
        (goto-char (point-min))
        (when (let ((case-fold-search nil))
                (search-forward (concat "\177" symbol "\001") nil t))
          (setq text (buffer-substring-no-properties 
                      (line-beginning-position) (match-beginning 0)))
          (goto-char (match-end 0))
          (looking-at "\\([0-9]+\\),\\([0-9]+\\)")
          (setq line (string-to-int (match-string 1)))
          (setq pos (string-to-int (match-string 2)))
          (re-search-backward "\f\n\\([^\n]+\\),[0-9]*\n")
          (setq file (match-string 1)))))

    (when file
      (save-excursion
        (with-current-buffer (find-file-noselect file)
          (etags-goto-tag-location (cons text (cons line pos)))
          (cppsense-debug "Found symbol %s in tag file." symbol)
          (cppsense-create-point-marker))))))


(defun cppsense-completion-handler ()
  (if (not (bobp))
      (let ((syntax (char-syntax (char-before))))
        (when (or (eq syntax ?w)
                  (eq syntax ?_))
          (save-excursion
            (cppsense-go-to-beginning-of-symbol)
            (setq cppsense-completion-symbol-beginning-position (point)))

          (setq cppsense-completion-candidates nil)

          ;; class member
          (if (cppsense-member-p
               cppsense-completion-symbol-beginning-position)
            
              (let* ((parent-decl (cppsense-get-parent-class)))
                (if parent-decl
                    (setq cppsense-completion-candidates 
                          (list
                           (list 'header "Class Members"
                                 'candidates
                                 (cppsense-get-members-of-class parent-decl t))))))

            ;; unqualified member or global symbol
            (let ((class (cppsense-get-class-marker-for-containing-function)))
                (setq cppsense-completion-candidates 
                      (append (if class
                                  (list (list 'header "Class Members"
                                              'candidates
                                              (cppsense-get-members-of-class
                                               (assoc-default 'declaration class) t))))
                              (list (list 'header "Global Symbols"
                                          'candidates (cppsense-get-include-data)))))))

          (if (not cppsense-completion-candidates)
              (message "No completions for symbol before point.")

            (setq cppsense-completion-candidates
                  (mapcar (lambda (candidates-info)
                            (let ((sorted-candidates
                                   ;; sort is destructive
                                   (sort (copy-list (plist-get candidates-info 'candidates))
                                         (lambda (first second)
                                           (string< (downcase (plist-get first 'name))
                                                    (downcase (plist-get second 'name)))))))
                              (plist-put candidates-info 'candidates sorted-candidates)))
                          cppsense-completion-candidates))

            (cppsense-show-completion-for-point))
          
          t))))


(defun cppsense-show-completion-for-point ()
  (let* ((maxlinelen 
          (apply 
           'max
           (mapcar (lambda (candidates-info)
                     (apply 'max (mapcar (lambda (candidate)
                                           (length (plist-get candidate 'name)))
                                         (plist-get candidates-info 'candidates))))
                   cppsense-completion-candidates)))
         (numofcandidates 
          (apply '+ (mapcar (lambda (candidates-info)
                              (length (plist-get candidates-info 'candidates)))
                            cppsense-completion-candidates)))
         (width (max (+ maxlinelen 2) ; padding
                     15))
         (height (max (min numofcandidates 20) 5))
         (pixel-width (* width (frame-char-width)))
         (pixel-height (* 
                        ;; let's say the titlebar has the same
                        ;; height as a text line
                        (1+ height)
                        (frame-char-height)))
         (position (cppsense-calculate-popup-position pixel-height
                                                      pixel-width))
         (frame-params (list (cons 'top             (cdr position))
                             (cons 'left            (car position))
                             (cons 'width           width)
                             (cons 'height          height)
                             (cons 'minibuffer      nil)
                             (cons 'menu-bar-lines  0)
                             (cons 'tool-bar-lines  0)
                             (cons 'title           "Completions")))
         (orig-frame (selected-frame)))

    (if (not cppsense-completion-frame)
        (setq cppsense-completion-frame (make-frame  frame-params))

      (modify-frame-parameters cppsense-completion-frame frame-params)
      (make-frame-visible cppsense-completion-frame))

    (select-frame cppsense-completion-frame)
    (switch-to-buffer cppsense-completions-buffer)
    (setq cursor-type nil)
    (setq mode-line-format nil)
    (set-window-fringes nil 0 0)
    (if cppsense-selection-overlay
        ;; make sure the overlay belongs to the completion buffer if
        ;; it's newly created
        (move-overlay cppsense-selection-overlay (point-min) (point-min))

      (setq cppsense-selection-overlay 
            (make-overlay (point-min) (point-min)))
      (overlay-put cppsense-selection-overlay 'face 'highlight))
 
    (redirect-frame-focus cppsense-completion-frame orig-frame)
    (select-frame orig-frame)

    (add-hook 'pre-command-hook 'cppsense-pre-command)
    (add-hook 'post-command-hook 'cppsense-post-command))

    (setq cppsense-saved-keys nil)
    (dolist (binding `((,(kbd "<down>") . cppsense-next-line)
                       (,(kbd "<up>") . cppsense-previous-line)
                       (,(kbd "<next>") . cppsense-next-page)
                       (,(kbd "<prior>") . cppsense-previous-page)
                       (,(kbd "<ESC>") . cppsense-completion-cancel)
                       (,(kbd "<RET>") . cppsense-completion-insert-selection)))
      (let ((key (car binding))
            (command (cdr binding)))
        (push (cons key (lookup-key (current-local-map) key))
              cppsense-saved-keys)
        (define-key (current-local-map) key command)))

    (cppsense-update-completion-list)

    (setq cppsense-completion-just-started t))


(defun cppsense-pre-command ()
  (unless (or (eq this-command 'handle-switch-frame)
              (memq this-command cppsense-completion-editing-commands)
              (get this-command 'cppsense-allowed-during-completion))
   (cppsense-completion-cleanup)))


(defun cppsense-post-command ()
  (if cppsense-completion-just-started
      (setq cppsense-completion-just-started nil)

    (if (or (< (point) cppsense-completion-symbol-beginning-position)
            (and (eq this-command 'self-insert-command)
                 (let ((syntax (char-syntax (char-before))))
                   (not (or (eq syntax ?w)
                            (eq syntax ?_))))))
        (cppsense-completion-cleanup)

      (if (memq this-command cppsense-completion-editing-commands)
          (cppsense-update-completion-list)))))


(defun cppsense-update-completion-list ()
  (let* ((orig-frame (selected-frame))
         (filter (if (eq (selected-frame) cppsense-completion-frame)
                     ""
                   (downcase (buffer-substring 
                              cppsense-completion-symbol-beginning-position
                              (point)))))
         (len (length filter)))
    (unwind-protect
        (progn
          (select-frame cppsense-completion-frame)
          (switch-to-buffer cppsense-completions-buffer)
          (erase-buffer)

          (dolist (candidates-info cppsense-completion-candidates)
            (let ((header-inserted nil))
              (dolist (candidate (plist-get candidates-info 'candidates))
                (when (or (equal "" filter)
                          (string-match filter (plist-get candidate 'name)))
                  (unless header-inserted
                    (let ((start (point)))
                      (insert (plist-get candidates-info 'header) "\n")
                      (put-text-property start (point)
                                         'face 'header-line))
                    (setq header-inserted t))
                  (let ((start (point)))                      
                    (insert (plist-get candidate 'name) "\n")
                    (put-text-property start (1+ start)
                                       'cppsense-completion-candidate candidate))))))

          (goto-char (point-min))
          (cppsense-previous-line))

      (select-frame orig-frame))))


(defun cppsense-completion-cleanup ()
  (remove-hook 'pre-command-hook 'cppsense-pre-command)
  (remove-hook 'post-command-hook 'cppsense-post-command)
  (make-frame-invisible cppsense-completion-frame)

  (dolist (binding cppsense-saved-keys)
    (define-key (current-local-map) (car binding) (cdr binding)))

  (dolist (candidates-info cppsense-completion-candidates)
    (dolist (candidate (plist-get candidates-info 'candidates))
      (dolist (elem candidate)
        (if (markerp elem)
            (move-marker elem nil)))))

  (tooltip-hide)

  (with-current-buffer cppsense-completions-buffer
    (setq cursor-type t)
    (kill-local-variable 'mode-line-format)))


(defun cppsense-mark-current-line ()
  (let ((orig-frame (selected-frame)))
    (unwind-protect
        (progn          
          (select-frame cppsense-completion-frame)
          (move-overlay cppsense-selection-overlay
                        (line-beginning-position)
                        (1+ (line-end-position))))
      (select-frame orig-frame))))


(defun cppsense-next-line ()
  (interactive)
  (cppsense-move-selection (lambda () (forward-line 1))))

(put 'cppsense-next-line 'cppsense-allowed-during-completion t)


(defun cppsense-previous-line ()
  (interactive)
  (cppsense-move-selection (lambda () (forward-line -1))))

(put 'cppsense-previous-line 'cppsense-allowed-during-completion t)


(defun cppsense-next-page ()
  (interactive)
  (cppsense-move-selection (lambda ()
                               (condition-case nil
                                   (scroll-up)
                                 (end-of-buffer (goto-char (point-max)))))))

(put 'cppsense-next-page 'cppsense-allowed-during-completion t)


(defun cppsense-previous-page ()
  (interactive)
  (cppsense-move-selection (lambda ()
                               (condition-case nil
                                   (scroll-down)
                                 (beginning-of-buffer (goto-char (point-min)))))))

(put 'cppsense-previous-page 'cppsense-allowed-during-completion t)


(defun cppsense-move-selection (func)
  (interactive)

  (tooltip-hide)

  (let ((orig-frame (selected-frame)))
    (unwind-protect
        (progn
          (select-frame cppsense-completion-frame)
          (let ((start (point)))
            (funcall func)
            (if (eobp)
                (forward-line -1)
              (if (bobp)
                  (forward-line 1)
                (unless (get-text-property (line-beginning-position)
                                           'cppsense-completion-candidate)
                  (if (> start (point))
                      (forward-line -1)
                    (forward-line 1))))))
          (cppsense-mark-current-line))
      (select-frame orig-frame)))

  (with-current-buffer cppsense-completions-buffer    
    (unless (eq (overlay-start cppsense-selection-overlay)
                (overlay-end cppsense-selection-overlay))
      (let* ((candidate (get-text-property (line-beginning-position)
                                           'cppsense-completion-candidate))
             (marker (or (plist-get candidate 'definition)
                         (plist-get candidate 'declaration))))
        (if (and marker
                 (sit-for 0.5))
            (let ((orig-frame (selected-frame)))
              (unwind-protect
                  (progn
                    (unless (markerp marker)
                      (setq marker
                            (with-current-buffer (find-file-noselect
                                                  (plist-get marker 'file))
                              (save-excursion
                                (goto-char (plist-get marker 'pos))
                                (search-forward (plist-get candidate 'name) nil t)
                                (cppsense-create-point-marker)))))
                    (select-frame cppsense-completion-frame)
                    (save-excursion
                      (end-of-line)
                      (cppsense-show-popup-help
                       (cppsense-get-code-context-from-marker marker))))

                (select-frame orig-frame))))))))


(defun cppsense-completion-insert-selection ()
  (interactive)
  (let ((candidate (with-current-buffer cppsense-completions-buffer
                     (get-text-property (line-beginning-position)
                                        'cppsense-completion-candidate))))
    (when candidate
      (let ((p (point)))
        (cppsense-go-to-beginning-of-symbol)
        (delete-region (point) p))
      (insert (plist-get candidate 'name)))))


(defun cppsense-completion-cancel ()
  ;; post command hook will take care of it
  (interactive))


(defun cppsense-try-automatic-completion ()
  (if (and (or (eq this-command 'self-insert-command)
               (eq this-command 'c-electric-backspace))
           cppsense-completion-frame
           (not (frame-visible-p cppsense-completion-frame))
           (sit-for 0.5))
      (cppsense-completion-handler)))


(defun cppsense-nothing-handler ()
  (let ((other-file (cppsense-get-other-file)))
    (if other-file
        (find-file other-file)
      (error "Can't find other file."))))


(defun cppsense-get-other-file ()
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (name (file-name-sans-extension file))
         (ext (file-name-extension file))
         (other-extensions (cond ((member ext cppsense-header-extensions)
                                  cppsense-source-extensions)
                                 ((member ext cppsense-source-extensions)
                                  cppsense-header-extensions)
                                 (t (error "Unknown extension %s." ext)))))
    (some (lambda (ext)
            (let ((other-file (concat name "." ext)))
              (if (file-readable-p other-file)
                  other-file

                (let ((directories (gethash other-file 
                                            cppsense-filename-hash)))
                  (when directories
                    (if (second directories)
                        (message (concat "There are more than one possible candidates. "
                                         "Choosing the first one.")))
                    (concat (car directories) "/" other-file))))))
          other-extensions)))


(defun cppsense-completing-read (&rest args)
  "Same as completing-read, but completes list of candidates immediately."
  (let ((unread-command-events (cons ?\t unread-command-events)))
    (apply 'completing-read args)))


(defun cppsense-debug (string &rest args)
  (if cppsense-debug
      (apply 'message string args)))



(defun cppsense-color-string-background (oldstr color)
  (interactive)
  (let ((prevpos 0)
        pos 
        (str (copy-sequence oldstr))
        (continue t))
    (while continue
      (setq pos (next-single-property-change prevpos 'face str))
      (unless pos
        (setq pos (length str))
        (setq continue nil))
              
      (let ((face (get-text-property prevpos 'face str)))
        (put-text-property prevpos pos 'face
                           (list (cons 'background-color color)
                                 (cons 'foreground-color (if face
                                                             (face-foreground face))))
                           str))
      (setq prevpos pos))
    str))


(defun cppsense-show-popup-help (message)
  (let* ((old-propertize (symbol-function 'propertize))
         (x-max-tooltip-size '(120 . 40))
         (lines (split-string message "\n"))
         (tooltip-width (* (frame-char-width)
                           (apply 'max (mapcar 'length lines))))
         (tooltip-height (* (frame-char-height) (min (length lines)
                                                     (cdr x-max-tooltip-size))))
         (xy (cppsense-calculate-popup-position tooltip-height tooltip-width))
         (tooltip-frame-parameters (append `((left . ,(car xy))
                                             (top . ,(cdr xy)))
                                           tooltip-frame-parameters)))

    ;; move the mouse cursor from the way
    (set-mouse-position (selected-frame) 0 100)

    ;; the definition of `propertize' is substituted with a dummy
    ;; function temporarily, so that tooltip-show doesn't override the
    ;; properties of msg
    (fset 'propertize (lambda (string &rest properties)
                        string))
    (unwind-protect
        (tooltip-show message)
      (fset 'propertize old-propertize))))


(defun cppsense-calculate-popup-position (height width)
  (let* ((point-pos (posn-at-point))
         (point-xy (posn-x-y point-pos))
         (x (let ((x (+ (car point-xy) (frame-parameter nil 'left))))
              (if (> (+ x width) (x-display-pixel-width))
                  (- (x-display-pixel-width) width 10)
                x)))
         (y (let* ((point-y (+ (cdr point-xy) (frame-parameter nil 'top)))
                   (y (- point-y height)))
              (if (< y 0)
                  (+ point-y (* 4 (frame-char-height)))
                y))))
    (cons x y)))


(defun cppsense-truncate-path (path &optional length)
  "If PATH is too long truncate some components from the beginning."
  (let ((maxlength (if length
                       length
                     70)))
    (if (<= (length path) maxlength)
        path

      (let* ((components (reverse (split-string path "/")))
             (tmppath (car components)))
        (setq components (cdr components))

        (while (and components
                    (< (length tmppath) maxlength))
          (setq path tmppath)
          (setq tmppath (concat (car components)
                                "/"
                                tmppath))
          (setq components (cdr components)))

        (concat ".../" path)))))


(defun cppsense-within-function-or-class ()  
  (save-excursion
    (while (and (not (bobp))
                (not (eq (char-after (line-beginning-position)) ?{))
                (not (eq (char-after (line-beginning-position)) ?})))
      (forward-line -1))
    (eq (char-after (line-beginning-position)) ?{)))


(defun cppsense-within-function ()  
  (and (cppsense-within-function-or-class)
       (save-excursion
         (c-beginning-of-defun)
         (not (looking-at "class")))))


(defun cppsense-within-class ()  
  (and (cppsense-within-function-or-class)
       (save-excursion
         (c-beginning-of-defun)
         (looking-at "class"))))


(defun cppsense-get-symbol-name (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (if (looking-at cppsense-symbol-regexp)
          (match-string 1)
        (cppsense-debug "cppsense-get-symbol-name: no match: %s"
                        marker)))))


(defun cppsense-get-symbol-begin (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (cppsense-get-symbol-begin-at-point))))


(defun cppsense-get-symbol-end (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (cppsense-get-symbol-end-at-point))))


(defun cppsense-get-symbol-begin-at-point ()
  (save-excursion
    (cppsense-go-to-beginning-of-symbol)
    (point)))


(defun cppsense-go-to-beginning-of-symbol ()
  (skip-syntax-backward "w_"))


(defun cppsense-get-symbol-end-at-point ()
  (save-excursion
    (cppsense-go-to-end-of-symbol)
    (point)))


(defun cppsense-go-to-end-of-symbol ()
  (skip-syntax-forward "w_"))


(defun cppsense-create-point-marker ()
  (let ((marker (point-marker)))
    (push marker cppsense-markers)
    marker))

(defun cppsense-destroy-markers ()
  (dolist (marker cppsense-markers)
    (move-marker marker nil)))           


(defun cppsense-get-code-context-from-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (concat (cppsense-color-string-background
               (concat
                (cppsense-truncate-path (buffer-file-name))
                ":\n")
               "moccasin")
              "\n"
              (buffer-substring (save-excursion
                                  (forward-line -5)
                                  (point))
                                (line-beginning-position))
              (cppsense-color-string-background
               (buffer-substring (line-beginning-position)
                                 (1+ (line-end-position)))
               "honeydew2")
              (buffer-substring (1+ (line-end-position))
                                (save-excursion
                                  (forward-line +5)
                                  (point)))))))



(defun cppsense-get-class-marker-for-containing-function ()
  (and (cppsense-within-function)
       (save-excursion
         (c-beginning-of-defun)
         (when (re-search-forward (concat cppsense-symbol-regexp "::")
                                  ;; stop search at the opening brace
                                  (save-excursion
                                    (search-forward "{" nil t)                       
                                    (forward-char -1)
                                    (forward-sexp)
                                    (point))
                                  t)
           (goto-char (match-beginning 0))
           (cppsense-get-symbol-info-at-point)))))


(defun cppsense-get-symbols-for-file (file)
  (let ((local-scopes (cppsense-get-local-scope-areas)))
    (with-current-buffer (get-file-buffer tags-file-name)
      (goto-char (point-min))
      (let ((file-regexp (concat "\f\n\\(.*" file "\\),[0-9]*\n"))
            symbols)
        (if (re-search-forward file-regexp nil t)
            (while (not (looking-at "\f"))
              (if (looking-at ".*\177\\(.*\\)\001\\([0-9]+\\),\\([0-9]+\\)")
                  (let ((name (match-string-no-properties 1))
                        (pos (string-to-int (match-string 3))))
                    (unless (or (member-if (lambda (symbol)
                                             (equal name (plist-get symbol 'name)))
                                           symbols)
                                ;; skip symbols in local scopes
                                (some (lambda (scope)
                                        (and (>= pos (car scope))
                                             (<= pos (cdr scope))))
                                      local-scopes))
                      (push (list 'name name
                                  'declaration (list 'pos pos 'file file))
                            symbols))))
              (forward-line 1))

          (message "File not found in TAG file: %s" file))

        symbols))))


(defun cppsense-get-local-scope-areas ()
  (let (areas)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^{" nil t)
        (push (cons (save-excursion
                      (let ((start (point)))
                        (c-beginning-of-defun)
                        (if (cppsense-get-class-on-current-line)
                            ;; classes are listed in global symbols,
                            ;; while functions are not
                            start
                          (point))))
                    (save-excursion
                      (c-end-of-defun)
                      (point)))
              areas)))
    areas))


(defun cppsense-get-include-data-internal ()
  (let* ((expanded-path (expand-file-name (buffer-file-name)))
         (symbols (gethash expanded-path cppsense-symbol-cache)))
    (unless symbols
      (setq symbols (cppsense-get-symbols-for-file expanded-path))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward cppsense-include-regexp nil t)
          (let ((include (match-string-no-properties cppsense-include-regexp-paren)))
            (unless (member include skip-includes)
              (push include skip-includes)
              (let ((path (cppsense-get-include-file-path include)))
                (if path
                    (if (file-readable-p path)
                        (with-current-buffer (find-file-noselect path)
                          (setq symbols
                                (append symbols 
                                        (cppsense-get-include-data-internal))))

                      (message "Include file %s not readable." path))

                  (message "Include file %s not found." include)))))))
    
      (puthash expanded-path symbols cppsense-symbol-cache))

    symbols))


(defun cppsense-get-include-data ()
  (let (skip-includes)
    (cppsense-get-include-data-internal)))


(provide 'cppsense)
