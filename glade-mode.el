;;; glade-mode.el --- A mode to view glade interface using tree-widget

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@163.com
;; Version: $Id: glade-mode.el,v 0.0 2007/06/16 12:08:19 ywb Exp $
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

;; 1. Why I write it?
;;    Sometimes, I use glade draw a interface, I forget what signal I
;;    used for a widget, I have to swap Emacs from coding to Glade to
;;    look up for the signal. The other way is open the glade file to
;;    search. But I hate the markup label, and I want a easy way for
;;    me to browse the whole file. So I think use tree-widget to
;;    represent the glade interface is necessary.
;;
;; 2. Can it modify glade file?
;;    Currently not. But I think it is not hard to edit current
;;    content in the xml. It really hard to add something to the xml.
;;    Because I don't know what is valid to add to file.
;;
;; 3. How to invocate glade-mode automaticly when open .glade file?
;;    Because magic-mode-alist is used before auto-mode-alist, So you
;;    have to add a regexp to magic-mode-alist:
;;    (add-to-list 'magic-mode-alist
;;      '("<\\?xml[ \t\r\n]+[^>]*>[ \t\r\n]*<!DOCTYPE glade-interface" . glade-mode))
;;
;; 4. Where to get and put the icons?
;;    The icons used can be download from http://glade.gnome.org/download.html.
;;    For current release, they are in directory pixmaps/16x16. I
;;    extract the map between the icon and GtkWidget type from
;;    widgets/gtk+.xml. tree-widget library find icon in directories
;;    list `tree-widget-themes-load-path'. You can copy pixmaps/16x16
;;    to one of the directories with name "glade". For example, If you
;;    add "~/.emacs.d/" to the `tree-widget-themes-load-path', the
;;    icons should in "~/.emacs.d/tree-widget/glade".
;;
;; At last, Note this lib use `tree-mode', which can be download from:
;; http://www.emacswiki.org/cgi-bin/emacs/tree-mode.el

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'glade-mode)

;;; Code:

(provide 'glade-mode)
(eval-when-compile
  (require 'cl))

(require 'tree-mode)
(require 'xml)

;;{{{  icon list
(defvar glade-icon-alist
  '(("GtkWindow" . "window")
    ("GtkMenuItem" . "menuitem")
    ("GtkCheckMenuItem" . "checkmenuitem")
    ("GtkRadioMenuItem" . "radiomenuitem")
    ("GtkImageMenuItem" . "imagemenuitem")
    ("GtkSeparatorMenuItem" . "separatormenuitem")
    ("GtkMenuBar" . "menubar")
    ("GtkToolbar" . "toolbar")
    ("GtkToolItem" . "toolitem")
    ("GtkSeparatorToolItem" . "separatortoolitem")
    ("GtkToolButton" . "toolbutton")
    ("GtkToggleToolButton" . "toggletoolbutton")
    ("GtkRadioToolButton" . "radiotoolbutton")
    ("GtkMenuToolButton" . "menutoolbutton")
    ("GtkHandleBox" . "handlebox")
    ("GtkLabel" . "label")
    ("GtkEntry" . "entry")
    ("GtkTextView" . "textview")
    ("GtkButton" . "button")
    ("GtkToggleButton" . "togglebutton")
    ("GtkCheckButton" . "checkbutton")
    ("GtkSpinButton" . "spinbutton")
    ("GtkRadioButton" . "radiobutton")
    ("GtkFileChooserButton" . "filechooserbutton")
    ("GtkColorButton" . "colorbutton")
    ("GtkFontButton" . "fontbutton")
    ("GtkComboBox" . "combobox")
    ("GtkComboBoxEntry" . "comboboxentry")
    ("GtkTreeView" . "treeview")
    ("GtkIconView" . "iconview")
    ("GtkProgressBar" . "progressbar")
    ("GtkImage" . "image")
    ("GtkDialog" . "dialog")
    ("GtkHBox" . "hbox")
    ("GtkVBox" . "vbox")
    ("GtkTable" . "table")
    ("GtkHPaned" . "hpaned")
    ("GtkVPaned" . "vpaned")
    ("GtkNotebook" . "notebook")
    ("GtkAlignment" . "alignment")
    ("GtkFrame" . "frame")
    ("GtkAspectFrame" . "aspectframe")
    ("GtkHScale" . "hscale")
    ("GtkVScale" . "vscale")
    ("GtkCalendar" . "calendar")
    ("GtkMenu" . "menu")
    ("GtkHScrollbar" . "hscrollbar")
    ("GtkVScrollbar" . "vscrollbar")
    ("GtkHButtonBox" . "hbuttonbox")
    ("GtkVButtonBox" . "vbuttonbox")
    ("GtkHSeparator" . "hseparator")
    ("GtkVSeparator" . "vseparator")
    ("GtkStatusbar" . "statusbar")
    ("GtkAccelLabel" . "accellabel")
    ("GtkArrow" . "arrow")
    ("GtkLayout" . "layout")
    ("GtkFixed" . "fixed")
    ("GtkDrawingArea" . "drawingarea")
    ("GtkEventBox" . "eventbox")
    ("GtkExpander" . "expander")
    ("GtkViewport" . "viewport")
    ("GtkScrolledWindow" . "scrolledwindow")
    ("GtkAboutDialog" . "aboutdialog")
    ("GtkColorSelectionDialog" . "colorselectiondialog")
    ("GtkFileChooserDialog" . "filechooserdialog")
    ("GtkFontSelectionDialog" . "fontselectiondialog")
    ("GtkInputDialog" . "inputdialog")
    ("GtkMessageDialog" . "messagedialog")
    ("GtkRuler" . "ruler")
    ("GtkHRuler" . "hruler")
    ("GtkVRuler" . "vruler")
    ("GtkCombo" . "combo")
    ("GtkOptionMenu" . "optionmenu")
    ("GtkList" . "list")
    ("GtkListItem" . "listitem")
    ("GtkCList" . "clist")
    ("GtkColorSelection" . "colorselection")
    ("GtkFontSelection" . "fontselection")
    ("GtkCurve" . "curve")
    ("GtkGammaCurve" . "gammacurve")
    ("GtkFileSelection" . "fileselection")
    ("Custom" . "custom"))
  "Map gtk widget type to icon")
;;}}}

(defvar glade-conf-widget nil
  "Widget to edit in *glade* buffer")
(defvar glade-buffer-file-name nil
  "The buffer assoc file name")
  
(define-derived-mode glade-mode tree-mode "Glade"
  "Mode to view glade interface"
  (let (xml)
    (tree-widget-set-theme "glade")
    (widen)
    (setq xml (xml-parse-region (point-min) (point-max)))
    (set (make-local-variable 'glade-buffer-file-name)
         buffer-file-name)
    (setq buffer-file-name nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (toplevel (xml-get-children (car xml) 'widget))
        (tree-mode-insert (glade-tree-widget toplevel))))
    (setq buffer-read-only t)
    (auto-save-mode 0)
    (goto-char (point-min))))

(defun glade-mode-quit (arg)
  (interactive "P")
  (setq buffer-read-only nil)
  (setq buffer-file-name glade-buffer-file-name)
  (if buffer-file-name
      (let ((auto-mode-alist nil)
            (magic-mode-alist nil))
        (revert-buffer nil t)
        (xml-mode))))

;;{{{  Build widget
(defun glade-tree-widget (widget)
  (let ((children (xml-get-children widget 'child)))
    (if (null children)
        (glade-push-button widget)
      `(tree-widget
        :node ,(glade-push-button widget)
        ,@(mapcar (lambda (c)
                    (glade-tree-widget (glade-child-to-widget c)))
                  (xml-get-children widget 'child))))))

(defun glade-push-button (widget)
  (if (eq (car widget) 'placeholder)
      `(push-button
        :tag "placeholder"
        :button-icon ,(or (assoc-default "placeholder" glade-icon-alist)
                          "custom")
        :format "%[%t%]\n")
    (list 'push-button
          :tag (xml-get-attribute widget 'id)
          :button-icon (or (assoc-default (xml-get-attribute widget 'class)
                                          glade-icon-alist)
                           "custom")
          :format "%[%t%]\n"
          :notify 'glade-show-node
          :glade-property (xml-get-children widget 'property)
          :glade-accessibility (xml-get-children widget 'accessibility)
          :glade-signal (xml-get-children widget 'signal)
          :glade-attributes (xml-node-attributes widget)
          :glade-accelerator (xml-get-children widget 'accelerator)
          :glade-packing (car (xml-get-children widget 'packing)))))

;; every child may have a attribute "internal-child", and may have
;; packing node. But the tree widget only show the widget node, so I
;; add internal-child to widget's attributes and packing node to
;; widget's child
(defun glade-child-to-widget (child)
  (let ((widget (car (or (xml-get-children child 'widget)
                         (xml-get-children child 'placeholder)))))
    (cons (car widget)
          (cons (append (xml-node-attributes widget)
                        (xml-get-attribute-or-nil child 'internal-child))
                (append (xml-node-children widget)
                        (xml-get-children child 'packing))))))
;;}}}

(defun glade-tree-flat-map (func tree)
  (if (tree-widget-p tree)
      (apply 'append (mapcar (lambda (child)
                               (glade-tree-flat-map func child))
                             (widget-get tree :children)))
    (list (funcall func tree))))

(define-derived-mode glade-conf-mode conf-unix-mode "Glade-Conf"
  "Configure mode for glade"
  (make-local-variable 'glade-conf-widget))

(defun glade-list-signal ()
  (interactive)
  (let ((tree (tree-mode-tree-ap)))
    (if (null tree)
        (message "No widget at point!")
      (with-current-buffer (get-buffer-create "*glade*")
        (unless (eq major-mode 'glade-conf-mode)
          (glade-conf-mode))
        (erase-buffer)
        (setq glade-conf-widget tree)
        (dolist (widget (delq nil (glade-tree-flat-map
                                   (lambda (but)
                                     (and (widget-get but :glade-signal)
                                          (cons (widget-get but :tag)
                                                (widget-get but :glade-signal))))
                                   tree)))
          (insert "[" (car widget) "]\n")
          (dolist (sig (cdr widget))
            (insert (format "%-20s = %s\n"
                            (xml-get-attribute sig 'name)
                            (xml-get-attribute sig 'handler))))
          (insert "\n"))
        (if (= (buffer-size) 0)
            (message "No signal found!")
          (display-buffer (current-buffer)))))))

(defun glade-show-node (widget &rest ignore)
  (with-current-buffer (get-buffer-create "*glade*")
    (unless (eq major-mode 'glade-conf-mode)
      (glade-conf-mode))
    (erase-buffer)
    (setq glade-conf-widget widget)
    (dolist (prop (widget-get widget :glade-property))
      (insert "[property]\n")
      (dolist (attr (xml-node-attributes prop))
        (insert (format "%-20S = %s\n"
                        (car attr)
                        (cdr attr))))
      (insert (format "%-20s = %s\n"
                      "DATA" (car (xml-node-children prop))))
      (insert "\n"))
    (dolist (feat '(:glade-signal :glade-accelerator))
      (dolist (sig (widget-get widget feat))
        (insert (format "[%s]\n"
                        (substring (symbol-name feat) 7)))
        (dolist (attr (xml-node-attributes sig))
          (insert (format "%-20S = %s\n"
                          (car attr)
                          (cdr attr))))
        (insert "\n")))
    (when (widget-get widget :glade-packing)
      (insert "[packing]\n")
      (dolist (prop (xml-get-children (widget-get widget :glade-packing) 'property))
        (insert (format "%-20s = %s\n"
                        (xml-get-attribute prop 'name)
                        (car (xml-node-children prop))))))
    (display-buffer (current-buffer))))

(define-key glade-mode-map "\C-c\C-c" 'glade-mode-quit)

;;; glade-mode.el ends here
