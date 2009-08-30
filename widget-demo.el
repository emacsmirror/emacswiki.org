;;; widget-demo.el --- Some recipes for creating widgets

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: widget-demo.el,v 0.0 2007/11/13 17:50:14 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; The widget document is not easy to understand. I think the
;; quick way to use widget is learning from examples. So I wrote
;; this library to help others understand and use widget quickly.
;;
;; I'm not expert at widget, so there may be something wrong in the
;; code. If you found it, please let me know.
;;

;;; Add New Page:
;; You can test you widget definition by add it as a new page to
;; widget demo. You can benefit from without write the code to
;; creating buffer and the key bindings of `widget-demo-mode'. Here is
;; a simple example to add new widget demo page:
;; 
;; (defun widget-demo-tree-test ()
;;   (widget-insert "Tree:\n")
;;   (widget-create
;;    '(tree-widget
;;      :node (push-button :format "%[%t%]\n" :tag "hello")
;;      :open t
;;      (push-button :format "%[%t%]\n" :tag "node1")
;;      (push-button :format "%[%t%]\n" :tag "node2"))))
;; (widget-demo-add-page "Tree Test" 'widget-demo-tree-test)

;;; TODO:
;; 1. Add tree-widget
;; 2. Add complex widget create by using several types of widget and
;;    using group.
;; 3. Add tutorial about defining new widget
;; 4. Add a macro for simplified page definition. For example:
;;    (widget-demo-insert
;;     "Some text here"
;;     (button :notify callback "label")
;;     "More text"
;;     (editable-field :form-id identifier))

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (autoload 'widget-demo "widget-demo" "Show demo of widget" t)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'info)
(require 'tree-widget)

(defvar widget-demo-buffer-name "*Widget Demo*"
  "*Name of the widget demo buffer")

(defvar widget-demo-list
  '(("Contents" widget-demo-contents :header "Widget Demo")
    ("Example" widget-demo-example :header "Example in info")
    ("WidgetTree" widget-tree :header "Widget Tree")
    ("Button" widget-demo-button)
    ("Text" widget-demo-text)
    ("Choice" widget-demo-choice)
    )
  "A list of pages to show.")

(defvar widget-demo-current nil
  "Current page in `widget-demo-list'.")

(defvar widget-demo-form nil
  "A table for lookup widget created in current buffer.")

(defvar widget-demo-anchors nil
  "A table for lookup markers created in current buffer.")

(defvar widget-demo-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'widget-demo-next)
    (define-key map "p" 'widget-demo-previous)
    (define-key map "u" 'widget-demo)
    (define-key map "t" 'widget-demo)
    (dolist (num (number-sequence ?1 ?9))
      (define-key map (char-to-string num) 'widget-demo-goto-page))
    ;; this command may be helpful for debug
    (define-key map "r" 'widget-demo-reflesh)
    (define-key map "\C-c\C-s" 'widget-demo-show-source)
    (define-key map "\C-c\C-n" 'widget-demo-next)
    (define-key map "\C-c\C-p" 'widget-demo-previous)
    (define-key map "\C-c\C-u" 'widget-demo)
    (define-key map "\C-c\C-t" 'widget-demo)
    map)
  "Keymap to use in *Widget Demo* buffer.")

(defvar widget-demo-menu nil)
(unless widget-demo-menu
  (easy-menu-define
   widget-demo-menu widget-demo-mode-map "Widget Demo"
   `("Widget"
     ["Next Page" widget-demo-next t]
     ["Previous Page" widget-demo-previous t]
     ["Top" widget-demo t]
     ["Refresh" widget-demo-reflesh t]
     "--"
     ,@(mapcar (lambda (p)
                 (vector (car p) 'widget-demo-menu-goto t))
               widget-demo-list))))

;;{{{  Helper functions
(defun widget-demo-menu-goto ()
  (interactive)
  (widget-demo-goto (symbol-name last-command-event)))

(defun widget-demo-form-create (id widget)
  (if (assoc id widget-demo-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) widget-demo-form)))

(defun widget-demo-form-add (id widget)
  (let ((old (assoc id widget-demo-form)))
    (if old
        (setcdr old widget)
      (push (cons id widget) widget-demo-form))))

(defun widget-demo-form-get (id)
  (cdr (assoc id widget-demo-form)))

(defun widget-demo-create-anchor (anchor)
  (push (cons anchor (point-marker)) widget-demo-anchors))

(defun widget-demo-resolve-link (name)
  "Return the Next, Top, Next page."
  (let* ((page (assoc name widget-demo-list))
         (rest (member page widget-demo-list))
         (len (length rest))
         next previous)
    ;; the contents only list next 
    (if (eq page (car widget-demo-list))
        (list (cadr widget-demo-list))
      (if (= len 1)
          (setq next nil)
        (setq next (cadr rest)))
      (setq previous (nth (- (length widget-demo-list) len 1)
                          widget-demo-list))
      (list next (car widget-demo-list) previous))))

(defun widget-demo-add-page (&rest page)
  "add new PAGE to widget-demo.
The page is constituted by

 (PAGE-NAME PAGE-DEFINITION [KEYWORD VALUE]).

Current only keyword :header is supported, which value is a string
to display in menu and the header of buffer instead of the page-name."
  (setq widget-demo-list
        (append widget-demo-list (list page))))

(defun widget-demo-remove-page (name)
  (setq widget-demo-list
        (delq (assoc name widget-demo-list)
              widget-demo-list)))
;;}}}

;;{{{  Commands
;;;###autoload 
(defun widget-demo ()
  "Show widget demo."
  (interactive)
  (switch-to-buffer widget-demo-buffer-name)
  (widget-demo-goto "Contents"))

(define-derived-mode widget-demo-mode nil "WDemo"
  "Widget demo.
\\{widget-demo-mode-map}"
  (make-local-variable 'widget-demo-form)
  (make-local-variable 'widget-demo-anchors))

(defun widget-demo-goto (link)
  (interactive
   (list (completing-read "Goto: " widget-demo-list nil t)))
  (switch-to-buffer widget-demo-buffer-name)
  (widget-demo-mode)
  (setq link (split-string link "#"))
  (let* ((inhibit-read-only t)
         (name (car link))
         (anchor (cadr link))
         (page (assoc name widget-demo-list)))
    (erase-buffer)
    (remove-overlays)
    (setq widget-demo-current name)
    ;; insert buttons
    (let ((links (widget-demo-resolve-link name))
          (label '("Next" "Contents" "Prev")))
      (dolist (link links)
        (when link
          (widget-create 'push-button
                         :format
                         (if (string= (car label) "Contents")
                             "%[Contents%]"
                           (format "%%t: %%[%s%%]" (car link)))
                         :button-face 'info-xref
                         :tag (car label)
                         :notify (lambda (wid &rest ignore)
                                   (widget-demo-goto (widget-value wid)))
                         (car link))
          (widget-insert "  "))
        (setq label (cdr label))))
    ;; insert title 
    (widget-insert "\n\n ")
    (widget-insert
     (propertize
      (or (plist-get page :header) (car page))
      'face 'info-title-1))
    (widget-insert "\n\n")
    (funcall (cadr page))
    ;; if there is an anchor, jump to the anchor
    (if (and anchor
             (setq anchor (assoc-default anchor widget-demo-anchors)))
        (goto-char anchor)
      (goto-char (point-min)))
    (widget-setup)
    (use-local-map widget-demo-mode-map)))

(defun widget-demo-next ()
  (interactive)
  (let ((links (widget-demo-resolve-link widget-demo-current)))
    (if (car links)
        (widget-demo-goto (caar links))
      (message "No next pages!"))))

(defun widget-demo-previous ()
  (interactive)
  (let ((links (widget-demo-resolve-link widget-demo-current)))
    (if (nth 2 links)
        (widget-demo-goto (car (nth 2 links)))
      (message "No previous pages!"))))

(defun widget-demo-goto-page ()
  (interactive)
  (let ((num (- last-command-event ?0)))
    (if (< num (length widget-demo-list))
        (widget-demo-goto (car (nth num widget-demo-list)))
      (message "Only %d pages!" (length widget-demo-list)))))

(defun widget-demo-reflesh ()
  (interactive)
  (widget-demo-goto widget-demo-current))

(defun widget-demo-show-source ()
  (interactive)
  (let ((page (assoc widget-demo-current widget-demo-list)))
    (with-selected-window
        (display-buffer
         (find-file-noselect (find-library-name "widget-demo")))
      (imenu (symbol-name (cadr page)))
      (recenter 1))))
;;}}}

;;{{{  Pages
(defun widget-demo-contents ()
  (let ((idx 1))
    (dolist (page (cdr widget-demo-list))
      (widget-insert (format "%3d. " idx))
      (widget-create 'link
                     :format "%[%t%]"
                     :tag (or (plist-get page :header) (car page))
                     :button-prefix ""
                     :button-suffix ""
                     :notify (lambda (widget &rest ignore)
                               (widget-demo-goto (widget-value widget)))
                     (car page))
      (widget-insert "\n")
      (setq idx (1+ idx)))))

(defun widget-demo-example ()
  (widget-insert "Here is some documentation.\n\n")
  (widget-create 'editable-field
                 :size 13
                 :format "Name: %v "    ; Text after the field!
                 "My Name")
  (widget-create 'menu-choice
                 :tag "Choose"
                 :value "This"
                 :help-echo "Choose me, please!"
                 :notify (lambda (widget &rest ignore)
                           (message "%s is a good choice!"
                                    (widget-value widget)))
                 '(item :tag "This option" :value "This")
                 '(choice-item "That option")
                 '(editable-field :menu-tag "No option" "Thus option"))
  (widget-create 'editable-field
                 :format "Address: %v"
                 "Some Place\nIn some City\nSome country.")
  (widget-insert "\nSee also ")
  (widget-create 'link
                 :notify (lambda (&rest ignore)
                           (widget-value-set
                            (widget-demo-form-get 'repeat)
                            '("En" "To" "Tre"))
                           (widget-setup))
                 "other work")
  (widget-insert
   " for more information.\n\nNumbers: count to three below\n")
  (widget-demo-form-create
   'repeat
   (widget-create 'editable-list
                  :entry-format "%i %d %v"
                  :notify (lambda (widget &rest ignore)
                            (let ((old (widget-get widget
                                                   ':example-length))
                                  (new (length (widget-value widget))))
                              (unless (eq old new)
                                (widget-put widget ':example-length new)
                                (message "You can count to %d." new))))
                  :value '("One" "Eh, two?" "Five!")
                  '(editable-field :value "three")))
  (widget-insert "\n\nSelect multiple:\n\n")
  (widget-create 'checkbox t)
  (widget-insert " This\n")
  (widget-create 'checkbox nil)
  (widget-insert " That\n")
  (widget-create 'checkbox
                 :notify (lambda (&rest ignore) (message "Tickle"))
                 t)
  (widget-insert " Thus\n\nSelect one:\n\n")
  (widget-create 'radio-button-choice
                 :value "One"
                 :notify (lambda (widget &rest ignore)
                           (message "You selected %s"
                                    (widget-value widget)))
                 '(item "One") '(item "Another One.") '(item "A Final One."))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (if (= (length (widget-value (widget-demo-form-get 'repeat)))
                                  3)
                               (message "Congratulation!")
                             (error "Three was the count!")))
                 "Apply Form")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (widget-example))
                 "Reset Form")
  (widget-insert "\n"))

(defun widget-demo-button ()
  ;; simplest button
  (widget-insert "The action to perform when click button is set to :notify ")
  (widget-create 'push-button 
                 :notify (lambda (wid cwid &optional event)
                           (if event
                               (message "You click by mouse!")
                             (message "Got it!")))
                 "Push me")
  ;; better looking button
  (widget-insert "\n\nYou can change the looking of the button by set :button-face and :format.\n")
  (widget-create 'push-button
                 :format "This is %[ %t %] without prefix and suffix, and have different face.\n"
                 :button-face 'custom-button
                 :tag "button")
  ;; deactivate button
  (widget-insert "\nWidget can be activated or deactivated.\n")
  (widget-create 'toggle
                 :format "Turn %[%v%] the button: "
                 :button-face 'custom-button
                 :notify (lambda (wid &rest ignore)
                           (let ((but (widget-demo-form-get 'button)))
                             (if (widget-apply but :active)
                                 (widget-apply but :deactivate)
                               (widget-apply but :activate))))
                 t)
  (widget-demo-form-create
   'button
   (widget-create 'push-button
                  :format "%[%t%]\n"
                  :button-face 'custom-button
                  :tag "Button"
                  :notify (lambda (wid &rest ignore)
                            (message "Got it!"))))
  (widget-apply (widget-demo-form-get 'button) :deactivate)
  ;; image button
  (widget-insert "\nHere is a button which label is a image: ")
  (widget-create 'push-button
                 :format "%[%t%]"
                 :tag-glyph (find-image `((:type xpm :file "refresh.xpm")))
                 :tag "image"
                 :notify (lambda (wid &rest ingore)
                           (message "Create new file!")))
  ;; link
  (widget-insert "\nThe link to ")
  (widget-create 'link
                 :button-prefix ""
                 :button-suffix ""
                 :button-face 'info-xref
                 :action (lambda (wid &rest ignore)
                           (widget-demo-goto "Choice#checklist"))
                 "checklist")
  ;; url-link
  (widget-insert " in Choice page.\n")
  (widget-insert "\nThe url-link to ")
  (widget-create 'url-link
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[emacswiki%]"
                 :button-face 'info-xref
                 "http://www.emacswiki.org")
  (widget-insert "\n")
  ;; info-link
  (widget-insert "\nThe link to info node ")
  (widget-create 'info-link
                 :button-prefix ""
                 :button-suffix ""
                 :button-face 'info-xref
                 "widget"))

(defun widget-demo-text ()
  ;; simpliest
  (widget-insert "A text field with label\n")
  (widget-create 'editable-field
                 :format "Label: %v")
  ;; textfield size
  (widget-insert "\nThe size of text field can limit by setting :size\n")
  (widget-create 'editable-field
                 :format "Size 13: %v\n"
                 :size 13)
  ;; :notify
  (widget-insert "\nYou can add callback when the text is changed.\n")
  (widget-create 'editable-field
                 :notify (lambda (wid changed &rest ingore)
                           (message "Now the text is: %s" (widget-value changed))))
  ;; how to use :action
  (widget-insert "\nIf you want do something when press enter, bind it to :action\n"
                 "Press Enter to change each other: ")
  (widget-demo-form-create
   'left
   (widget-create 'editable-field
                  :action (lambda (wid &rest ignore)
                            (widget-demo-change-text wid 'right))
                  :size 13
                  "abc"))
  (widget-insert " <=> ")
  (widget-demo-form-create
   'right
   (widget-create 'editable-field
                  :action (lambda (wid &rest ignore)
                            (widget-demo-change-text wid 'left))
                  :size 13
                  "123"))
  (widget-insert "\n")
  ;; password
  (widget-insert "\nThe textfield can used for input password\n")
  (widget-create 'editable-field
                 :format "Password: %v\n"
                 :secret ?*)
  ;; validate
  (widget-insert "\nA regexp can set to validate the text\n")
  (widget-create 'editable-field
                 :format "Tel: %v"
                 :valid-regexp "\\`\\s-*[0-9]+\\([0-9]+-\\)*[0-9]+\\s-*\\'"
                 :action (lambda (wid &rest ignore)
                           (if (widget-apply wid :validate)
                               (error "The telephone number is not valid: %s!" (widget-get wid :error))
                             (message "%s is ok!" (widget-value wid)))))
  ;; completion
  (widget-insert "\nCompletion can set to :complete-function.\n"
                 "Try complete some symbols: ")
  (widget-create 'editable-field
                 :complete-function 'lisp-complete-symbol)
  (widget-insert (propertize (concat
                              "The default key to invoke completion is M-TAB, which is occupied in many window\n"
                              "manager. Note that widget-field-keymap is replaced by custom-field-keymap in \n"
                              "cus-edit.el(Line 4525). So if you want remap command widget-complete, define it\n"
                              "in custom-field-keymap.\n")
                             'face 'widget-inactive))
  ;; deactivate text
  (widget-insert "\nClick the checkbox to activate the textfield.\n")
  (widget-create 'checkbox
                 :notify (lambda (wid &rest ignore)
                           (let ((text (widget-demo-form-get 'text))
                                 (but (widget-demo-form-get 'button)))
                             (if (widget-apply text :active)
                                 (progn
                                   (widget-apply text :deactivate)
                                   (widget-apply but :deactivate))
                               (progn
                                 (widget-apply text :activate)
                                 (widget-apply but :activate))))))
  (widget-demo-form-create
   'text
   (widget-create 'editable-field
                  :format "Text: %v "
                  :size 20))
  (widget-apply (widget-demo-form-get 'text) :deactivate)
  (widget-demo-form-create
   'button
   (widget-create 'push-button
                  :notify (lambda (wid &rest ignore)
                            (let ((text (widget-demo-form-get 'text))
                                  start)
                              (save-excursion
                                (goto-char (widget-field-start text))
                                (forward-char (length (widget-value text)))
                                (insert "a"))))
                  "Append"))
  (widget-apply (widget-demo-form-get 'button) :deactivate)
  (widget-insert "\n")
  ;; text
  (widget-insert "\nText is intended for multiline text.\n")
  (widget-create 'text :format "Input text: %v")
  ;; editable list
  (widget-insert "\nEditable list is use for edit a list. ")
  (widget-create 'push-button
                 :format "%[%v%]\n"
                 :notify (lambda (wid &rest ignore)
                           (message "%S" (widget-value (widget-demo-form-get 'editable-list))))
                 "See Value")
  (widget-demo-form-create
   'editable-list
   (widget-create 'editable-list
                  :foramt "%v %i %d\n"
                  :value '("1" "2" "3")
                  '(editable-field
                    :current-index 3
                    :value-to-external
                    (lambda (wid value)
                      (if (widget-get wid :parent)
                          value
                        (let ((idx (1+ (widget-get wid :current-index))))
                          (widget-put wid :current-index idx)
                          (number-to-string idx))))))))

;; maybe it is not work, because marker of :to is delete also
(defun widget-demo-change-text (wid id)
  (let ((other (widget-demo-form-get id)))
    (save-excursion
      (goto-char (widget-field-start other))
      (delete-region (point) (widget-field-end other))
      (insert (widget-value wid)))))

(defun widget-demo-choice ()
  ;; menu choice
  (widget-insert "Here is a menu choice. The label is set by :tag, and the default item is\n"
                 "the child item that :value match the :value of the menu choice.\n")
  (widget-demo-form-create
   'menu-choice
   (widget-create 'menu-choice
                  :tag "Menu choices"
                  :button-face 'custom-button
                  :notify (lambda (wid &rest ignore)
                            (message "Current value: %S" (widget-value wid)))
                  :value 'const-variable
                  '(push-button :tag "button"
                                :format "%[%t%]\n"
                                :notify (lambda (wid &rest ignore)
                                          (message "button activate"))
                                "This is button")
                  '(item :tag "item" :value "This is item")
                  '(choice-item :tag "choice" "This is choice item")
                  '(const :tag "const" const-variable)
                  '(editable-field :menu-tag "editable field" "text")))
  ;; toggle
  (widget-insert "\nToggle is used for switch a flag: ")
  (widget-create 'toggle
                 :on "turn on"
                 :off "turn off"
                 :notify (lambda (wid &rest ignore)
                           (message (concat "turn the option "
                                            (if (widget-value wid) "on" "off"))))
                 t)
  ;; check box
  (widget-insert "\nCheck box is like toggle: ")
  (widget-create 'checkbox
                 :format "%[%v%] Option\n"
                 :notify (lambda (wid &rest ignore)
                           (message (concat "Option "
                                            (if (widget-value wid)
                                            "selected" "unselected"))))
                 t)
  ;; radio button
  (widget-insert "\nRadio button is used for select one from a list.\n")
  (widget-create 'radio-button-choice
                 :value "One"
                 :notify (lambda (wid &rest ignore)
                           (message "You select %S %s"
                                    (widget-type wid)
                                    (widget-value wid)))
                 '(item "One")
                 '(item "Two")
                 '(item "Three"))
  (widget-insert "\n")
  ;; checklist
  (widget-demo-create-anchor "checklist")
  (widget-insert "Checklist is used for multiple choices from a list.\n")
  (widget-create 'checklist
                 :notify (lambda (wid &rest ignore)
                           (message "The value is %S" (widget-value wid)))
                 '(item "one")
                 '(item "two")
                 '(item "three")))
;;}}}

;;{{{  Widget tree
(defun widget-tree-ancestor (widget)
  (let ((parent (car (get widget 'widget-type))))
    (if parent
        (cons widget (widget-tree-ancestor parent)))))

(defvar widget-tree-list nil
  "Inherit tree of all widget")

(defun widget-tree-build ()
  (let (list seen ancestor tree-list)
    (mapatoms (lambda (a)
                (and (get a 'widget-type)
                     (push a list)))
              obarray)
    (setq list (remq 'default list))
    (while list
      (setq ancestor (nreverse (widget-tree-ancestor (car list))))
      (setq parent 'default)
      (dolist (type ancestor)
        (unless (member type seen)
          (setq alist (assoc parent tree-list)
                tree-list (delq alist tree-list)
                alist (cons parent (cons type (cdr alist)))
                tree-list (cons alist tree-list)
                list (delq type list))
          (push type seen))
        (setq parent type)))
    (setq widget-tree-list tree-list)))

(defun widget-tree-browse (but &rest ignore)
  (if (= (length (window-list)) 1)
      (split-window))
  (save-selected-window
    (other-window 1)
    (widget-browse (intern (widget-get but :tag)))))

(defun widget-tree-widget (type)
  (let ((list (assoc type widget-tree-list)))
    (if list
        `(tree-widget
          :node (push-button
                 :tag ,(symbol-name type)
                 :format "%[%t%]\n"
                 :notify widget-tree-browse)
          :dynargs widget-tree-expand)
      `(push-button
        :format "%[%t%]\n"
        :tag ,(symbol-name type)
        :notify widget-tree-browse))))

(defun widget-tree-expand (tree)
  (or (widget-get tree :args)
      (let ((type (intern (widget-get (tree-widget-node tree) :tag))))
        (mapcar 'widget-tree-widget
                (cdr (assoc type widget-tree-list))))))

(defun widget-tree ()
  (widget-insert
   "This is a list of all defined widget. The tree-widget show the
inherit relationship of the widget.\n\n")

  (widget-insert "  You can click the button to browse the widget.\n\n")
  (unless widget-tree-list
    (widget-tree-build))
  (widget-apply-action
   (widget-create (widget-tree-widget 'default)))
  (if (require 'tree-mode nil t)
      (tree-minor-mode t)
    (widget-insert "\n\n")
    (widget-insert "   I recommend you use ")
    (widget-create 'url-link
                   :button-prefix ""
                   :button-suffix ""
                   :format "%[tree-mode%]"
                   :button-face 'info-xref
                   "http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el")
    (widget-insert " to browse tree-widget.\n\n")))
;;}}}

(provide 'widget-demo)
;;; widget-demo.el ends here
