;;; gob.el --- A code generator for GObject

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: gob.el,v 0.0 2007/11/13 14:45:03 ywb Exp $
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
;; This library means to help create gobject quickly.
;; It provide a widget user interface to configure object
;; as well as abbrev interface to quick insert code.
;; To invoke widget user interface, use M-x gob-new-object.
;; To use it as abbrev, you need some elisp to configure tempo.
;; This an example of configuration:
;;   (require 'tempo)
;;   (defvar tempo-c-tags nil)
;;   (setq tempo-interactive t)
;;   (defun tempo-space ()
;;     "if has match something in tempo-tags, expand, otherwise insert
;;   space. "
;;     (interactive)
;;     (if (tempo-expand-if-complete)
;;         nil
;;       (call-interactively 'self-insert-command)))
;;   (defun tempo-install (&optional finder tags)
;;     "install tempo for certain major mode"
;;     (if finder (setq tempo-match-finder finder))
;;     (if tags (tempo-use-tag-list tags))
;;     (local-set-key " " 'tempo-space))
;;    (require 'gob)
;;    (add-hook 'c-mode-hook
;;              (lambda ()
;;                (tempo-install nil 'tempo-c-tags)
;;                (gob-install-tempo)))
;; Now, in c-mode, you can use abbrev "headerx", "sourcex", "macrox",
;; and so on to quick insert code snips.
;; Another way for insert template, is using command M-x gob-header
;; and M-x gob-source.
;; Note that the only way to create code for install object data
;; member, is using the widget user interface.
;;
;;; Quick start:
;; To create a simple object, do as following:
;; 1. M-x gob-new-object RET
;; 2. fill the form, typically you should set class, parent class,
;;    and data members. For example, set as following:
;;     class        => demo_person
;;     parent class => g_object
;;     members      => name, gchar *
;;                     age, gint
;; 3. press button "Generate Code", and save the header and source file
;; 4. create a test file demoperson-t.c:
;;      /* demo-person-t.c --- test file for class person */
;;      #include "demoperson.h"
;;      #include <stdio.h>
;;      int main (int argc, char * argv[]) 
;;      {
;;          DemoPerson* person;
;;          char* name;
;;          int age;
;;          g_type_init();
;;          person = demo_person_new();
;;          demo_person_set_name(person, "John");
;;          demo_person_set_age(person, 24);
;;          g_object_get(G_OBJECT(person), "name", &name,
;;                       "age", &age, NULL);
;;          printf("Hello, my name is %s, I'm %d years old.\n",
;;                  name, age);
;;          g_free(name);
;;          g_object_unref(G_OBJECT(person));
;;          return 0;
;;      }
;; 5. write a makefile:
;;     CC = gcc
;;     CFLAGS = $(shell pkg-config --cflags gobject-2.0) -g
;;     LDFLAGS = $(shell pkg-config --libs gobject-2.0)
;;     
;;     all: demoperson-t
;;     
;;     demoperson-t: demoperson-t.o demoperson.o
;;     	$(CC) -o $@ $^ $(LDFLAGS)
;; 6. make && ./demoperson-t
;; 
;; Enjoy!

;;; See Also
;; http://www.gustavobarbieri.com.br/gobject-class.el

;;; TODO
;; 1. support for methods
;; 2. create interface

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'gob)

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar gob-form nil
  "A table to store registered widget in the buffer")

(defvar gob-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "\C-c\C-r" 'gob-reset)
    (define-key map "\C-c\C-c" 'gob-submit)
    map)
  "Keymap in `gob-mode'")

(defvar gob-class-seperator "[_-]"
  "sperator for class name")

(defvar gob-template-brace '("<" . ">")
  "Open brace and close brace for variables in template.")

(defvar gob-names nil
  "Various names for class.")

(defvar gob-tempo-tags 'tempo-c-tags
  "Tempo tags used in `c-mode'.")

(defvar gob-tempo-suffix "x"
  "Suffix for expand tempo.")

(defvar gob-public-methods nil
  "Methods to declare.")

(defun gob-parse-pairs (str)
  (let ((start 0)
        pairs)
    (while (string-match "\\([[:word:]_]+\\)\\s-*=\\s-*\\(\"[^\"]+\"\\|'[^']+'\\)" str start)
      (push (cons (match-string 1 str) (substring (match-string 2 str) 1 -1)) pairs)
      (setq start (match-end 0)))
    pairs))

(defun gob-define-template (str)
  "Convert a string to tempo template.

The special thing in the string is marker using `gob-template-brace'.
The special thing has syntax as:
<[directive|name] attribute='value' ...>

directive including:
 1. include: need a atrribute 'template'. It will insert the included
    template.

name can have attributes:
 1. prompt: a string to prompt user to input the value
 2. init: a init value for the name
 3. marker: a marker will insert before insert the name. You can
    use `tempo-forward-mark' and `tempo-backward-mark' to navigate
    from marker to marker.
 4. nofix: name is case-insensitive. If the name is upcased or capitalized,
    the value of the name will be upcased or capitalized. If you don't
    want the case of value changed, set this flag to 1
 5. noinput: when insert tempo-template in interactive mode(the value of
    tempo-interactive is non-nil), all name will prompt for a value.
    set this flag to 1 to inhibit prompt.

If tempo-interactive is nil, all default value of name is set in
`gob-names'.
"
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((start (point))
          (re (concat (regexp-quote (car gob-template-brace))
                      "\\([[:word:]_]+\\)"
                      "\\(\\(\\s-+\\sw+\\s-*=\\s-*\\(\"[^\"]+\"\\|'[^']+'\\)\\)*\\)"
                      (regexp-quote (cdr gob-template-brace))))
          name attrs template)
      (while (re-search-forward re nil t)
        (push (buffer-substring start (match-beginning 0)) template)
        (setq name (match-string 1)
              attrs (gob-parse-pairs (match-string 2)))
        (if (string= name "include")
            (push `(l (cons 'l (assoc-default ,(assoc-default "template" attrs) gob-templates)))
                  template)
          (push (list 'l (list 'gob-lookup-name name (or (assoc-default "prompt" attrs)
                                                         (concat (capitalize name) ": "))
                               (assoc-default "init" attrs)
                               (gob-string-to-boolean (assoc-default "marker" attrs))
                               (gob-string-to-boolean (assoc-default "nofix" attrs))
                               (gob-string-to-boolean (assoc-default "noinput" attrs))))
                template))
        (setq start (point)))
      (push (buffer-substring (point) (point-max)) template)
      (nreverse template))))

(defun gob-string-to-boolean (str)
  (and str
       (> (string-to-number str) 0)))

(defun gob-capitalize (name)
  (if (string-match "_" name)
      (mapconcat 'capitalize (split-string name "_") "")
    ;; if is camel word, return directly
    (if (let ((case-fold-search nil))
          (and (string-match "^[A-Z]" name)
               (> (length (replace-regexp-in-string "[A-Z]" "" name)) 0)))
        name
      (capitalize name))))

(defun gob-lookup-name (name prompt init-value insert-marker
                             &optional nofixcase force)
  (let* ((lcname (downcase name))
         (var (intern lcname))
         value)
    (or init-value
        (setq init-value (or (assoc-default lcname gob-names) "")))
    (or (setq value (tempo-lookup-named var))
        (progn
          (setq value (if (and (null force) tempo-interactive)
                          (read-from-minibuffer prompt init-value)
                        init-value))
          (tempo-save-named var value)
          (if insert-marker
              (tempo-insert-mark (point-marker)))))
    (if nofixcase
        value
      (funcall
       (or (assoc-default name
                          (mapcar (lambda (f) (cons (funcall f name) f))
                                  '(downcase upcase gob-capitalize)))
           'downcase)
       value))))

;;{{{  Templates
(defvar gob-templates
  (list (cons "macro"
              (gob-define-template
               "#define <NAMESPACE>_TYPE_<NAME>                 (<namespace>_<name>_get_type ())
#define <NAMESPACE>_<NAME>(obj)                 (G_TYPE_CHECK_INSTANCE_CAST ((obj), <NAMESPACE>_TYPE_<NAME>, <NameSpace><Name>))
#define <NAMESPACE>_<NAME>_CLASS(klass)         (G_TYPE_CHECK_CLASS_CAST ((klass), <NAMESPACE>_TYPE_<NAME>, <NameSpace><Name>Class))
#define <NAMESPACE>_IS_<NAME>(obj)              (G_TYPE_CHECK_INSTANCE_TYPE ((obj), <NAMESPACE>_TYPE_<NAME>))
#define <NAMESPACE>_IS_<NAME>_CLASS(klass)      (G_TYPE_CHECK_CLASS_TYPE ((klass), <NAMESPACE>_TYPE_<NAME>))
#define <NAMESPACE>_<NAME>_GET_CLASS(obj)       (G_TYPE_INSTANCE_GET_CLASS ((obj), <NAMESPACE>_TYPE_<NAME>, <NameSpace><Name>Class))
"))
        (cons "definition"
              (gob-define-template
               "typedef struct _<NameSpace><Name> <NameSpace><Name>;
typedef struct _<NameSpace><Name>Class <NameSpace><Name>Class;

struct _<NameSpace><Name>
{
    <ParentNameSpace><Parentname> parent;

    <data_member marker='1' noinput='1' nofix='1'>
};

struct _<NameSpace><Name>Class
{
    <ParentNameSpace prompt='Parent namespace: '><Parentname prompt='Parent class name: '>Class parent_class;
<object_methods marker='1' noinput='1' nofix='1'>
};
"))
        (cons "method"
              (gob-define-template
               "GType <namespace>_<name>_get_type (void) G_GNUC_CONST;
<NameSpace><Name>* <namespace>_<name>_new(void);
<public_methods marker='1' noinput='1' nofix='1'>
"))
        (cons "header"
              (gob-define-template
               "<include template='macro'>
<include template='definition'>
<include template='method'>
"))
        (cons "source"
              (gob-define-template
               "<data_id noinput='1' nofix='1'>
static void <prefix prompt='Using prefix: '>_dispose (GObject *object);
static void <prefix>_finalize (GObject *object);
static void <prefix>_set_property ( GObject *object,
                                       guint property_id,
                                       const GValue *value,
                                       GParamSpec *pspec );
static void <prefix>_get_property( GObject *object,
                                      guint property_id,
                                      GValue *value,
                                      GParamSpec *pspec );

G_DEFINE_TYPE( <NameSpace><Name>, <prefix>, <PARENTNAMESPACE prompt='Parent namespace: '>_TYPE_<PARENTNAME prompt='Parent class name: '>);

static void <prefix>_class_init( <NameSpace><Name>Class *klass)
{
    GObjectClass *gobject_class = (GObjectClass *)klass;

    gobject_class->dispose = <prefix>_dispose;
    gobject_class->finalize = <prefix>_finalize;
    gobject_class->set_property = <prefix>_set_property;
    gobject_class->get_property = <prefix>_get_property;
<install_property noinput='1' nofix='1'>
}

static void <prefix>_init (<NameSpace><Name> *self)
{
}

static void <prefix>_dispose (GObject *object)
{
    <NameSpace><Name> *self = (<NameSpace><Name>*)object;

    G_OBJECT_CLASS (<prefix>_parent_class)->dispose(object);
}

static void <prefix>_finalize (GObject *object)
{
    <NameSpace><Name> *self = (<NameSpace><Name>*)object;

    g_signal_handlers_destroy(object);
    G_OBJECT_CLASS (<prefix>_parent_class)->finalize(object);
}
<set_property noinput='1' nofix='1'>
<get_property noinput='1' nofix='1'>
<NameSpace><Name>* <namespace>_<name>_new(void)
{
    return (<Namespace><Name>*) g_object_new(<NAMESPACE>_TYPE_<NAME>, 0);
}
"))
        )
  "Format for macro")
;;}}}

;;{{{  Form definition
(defun gob-form-insert (id widget)
  (if (assoc id gob-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) gob-form)))

(defun gob-form-add (id widget)
  (let ((old (assoc id gob-form)))
    (if old
        (setcdr old widget)
      (push (cons id widget) gob-form))))

(defun gob-form-get (id)
  (cdr (assoc id gob-form)))

(define-derived-mode gob-mode nil "GOB"
  "Create a new gobject.

\\{gob-mode-map}"
  (make-local-variable 'gob-form))
  
;; A new object need these informations:
;;  - name
;;  - package
;;  - parent
;;  - members
;;  - private methods
;;  - public methods
;;  - prefix
;;  - filename
(defun gob-new-object (&rest ignore)
  (interactive)
  (switch-to-buffer "*gob*")
  (gob-mode)
  (let ((inhibit-read-only t)
        (offset 15)
        (size 30))
    (erase-buffer)
    (remove-overlays)
    (widget-insert (propertize "Create a new object" 'face 'info-title-1))
    (widget-insert "\n\n")
    (widget-insert (propertize "General:" 'face 'info-title-2) "\n")
    ;; class name
    (widget-insert (propertize
                    (format (format "%%-%ds" offset) "Class:")
                    'help-echo "eg gtk_button"))
    (gob-form-insert 'class (widget-create 'editable-field
                                           :help-echo "eg gtk_button"
                                           :size size
                                           :notify 'gob-change))
    ;; parent class name
    (widget-insert "\n")
    (widget-insert (propertize
                    (format (format "%%-%ds" offset) "Parent Class:")
                    'help-echo "eg gtk_bin"))
    (gob-form-insert 'parent (widget-create 'editable-field
                                            :help-echo "eg gtk_bin"
                                            :size size))
    ;; data members
    (widget-insert "\n")
    (widget-insert (format (format "%%-%ds" offset) "Members:") "\n")
    (gob-form-insert 'members
                     (widget-create 'editable-list
                                    :entry-format "%i %d %v"
                                    :indent (- offset 12)
                                    :value '(("" "gchar *"))
                                    `(group
                                      (editable-field :format "%v "
                                                      :size ,size)
                                      (menu-choice
                                       :tag "Type"
                                       ,@(mapcar (lambda (type)
                                                   (list 'item type))
                                       '("gchar *" "gint" "gchar" "guchar"
                                        "gboolean" "guint"))))))
    (widget-insert "\n\n")
    (widget-insert (propertize "Options:" 'face 'info-title-2) "\n")
    ;; Prefix
    (widget-insert (format (format "%%-%ds" (- offset 3)) "Prefix:"))
    (widget-create 'checkbox
                   :notify (lambda (wid &rest ignore)
                             (gob-toggle-active 'prefix)))
    (gob-form-insert 'prefix
                     (widget-create 'editable-field
                                    :format " %v"
                                    :size 20))
    (widget-apply (gob-form-get 'prefix) :deactivate)
    ;; file basename
    (widget-insert "\n")
    (widget-insert (format (format "%%-%ds" (- offset 3)) "File name:"))
    (widget-create 'checkbox
                   :notify (lambda (wid &rest ignore)
                             (gob-toggle-active 'file-name)))
    (gob-form-insert 'file-name
                     (widget-create 'editable-field
                                    :format " %v"
                                    :size 20))
    (widget-apply (gob-form-get 'file-name) :deactivate)
    ;; save directory
    (widget-insert "\n")
    (widget-insert (format (format "%%-%ds" offset) "Save to:"))
    (gob-form-insert 'directory
                     (widget-create 'directory
                                    :format "%v"
                                    :size 20
                                    (expand-file-name default-directory)))
    ;; Buttons
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify 'gob-submit
                   "Generate Code")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify 'gob-reset
                   "Reset")
    (widget-setup)
    (use-local-map gob-mode-map)
    (widget-forward 1)))

(defun gob-toggle-active (id)
  (let ((text (gob-form-get id)))
    (if (widget-apply text :active)
        (widget-apply text :deactivate)
      (widget-apply text :activate))))

(defalias 'gob-reset 'gob-new-object)

(defun gob-change (wid &optional ignore)
  (let* ((class (gob-form-get 'class))
         (prefix (gob-form-get 'prefix))
         (file-name (gob-form-get 'file-name))
         (class-name (gob-field-value 'class))
         (names (gob-parse-name class-name "")))
    (dolist (pair (list (cons prefix (assoc-default "prefix" names))
                        (cons file-name (format "%s%s"
                                                (assoc-default "namespace" names)
                                                (assoc-default "name" names)))))
      (let ((inhibit-read-only t)
            (wid (car pair))
            (text (cdr pair))
            (buffer-undo-list t)
            len)
        (when (not (widget-apply wid :active))
          (save-excursion
            (setq len (length (widget-value wid)))
            (widget-apply wid :activate)
            (goto-char (widget-field-start wid))
            (delete-region (point) (+ (point) len))
            (insert text)
            (widget-apply wid :deactivate)))))))

(defun gob-trim (str)
  (replace-regexp-in-string
   "\\s-*\\'" "" 
   (replace-regexp-in-string "\\`\\s-*" "" str)))

(defun gob-field-value (id)
  (gob-trim (widget-value (gob-form-get id))))

(defun gob-submit (&rest ignore)
  (interactive)
  (let* ((class-name (gob-field-value 'class))
         (class-parent (gob-field-value 'parent))
         (gob-names (gob-parse-name class-name class-parent))
         (prefix (gob-field-value 'prefix))
         (default-directory (gob-field-value 'directory))
         (file-name (gob-field-value 'file-name))
         (file-header (concat file-name ".h"))
         (file-code   (concat file-name ".c"))
         (DEFINE_NAME (concat (upcase prefix) "_H"))
         (parent_include (if (string-match "^gtk_" class-parent)
                             "gtk/gtk.h"
                           "glib-object.h"))
         gob-public-methods data)
    (dolist (var '(class-name class-parent file-name))
      (if (string= "" (symbol-value var))
          (error "%S is empty" var)))
    (setcdr (assoc "prefix" gob-names) prefix)
    ;; handle object data members
    (mapc (lambda (m)
            (let ((name (gob-trim (car m))))
              (unless (string= "" name)
                (push (cons name (cdr m)) data))))
          (widget-value (gob-form-get 'members)))
    (setq data (nreverse data))
    (setq gob-names
          (append
           (list (cons "data_member"
                       (gob-trim
                        (mapconcat (lambda (m)
                                     (format "    %s %s;\n" (cadr m) (car m)))
                                   data "")))
                 (cons "data_id"
                       (concat
                        "enum {\n"
                        "   PROP_0,\n\n"
                        (apply 'concat
                               (mapcar (lambda (p)
                                         (concat "   PROP_" (upcase (car p)) ",\n"))
                                       data))
                        "};\n"))
                 (cons "install_property"
                       (gob-create-property data))
                 (cons "set_property"
                       (gob-function-set-property data))
                 (cons "get_property"
                       (gob-function-get-property data))
                 (cons "public_methods"
                       (mapconcat 'identity gob-public-methods "\n")))
           gob-names))
    (delete-other-windows)
    (split-window-horizontally)
    (find-file file-header)
    (if (and (file-exists-p file-header)
             (y-or-n-p (format "File %s is exists. Empty the file now? "
                               file-header)))
        (erase-buffer))
    (insert
     (concat
      "#ifndef __" DEFINE_NAME "__\n"
      "#define __" DEFINE_NAME "__\n"
      "\n"
      "#include <" parent_include ">\n"
      "\n"))
    (gob-insert-template "header")
    (goto-char (point-max))
    (insert "#endif /* __" DEFINE_NAME "__ */\n")

    (other-window 1)

    (find-file file-code)
    (if (and (file-exists-p file-code)
             (y-or-n-p (format "File %s is exists. Empty the file now? "
                               file-code)))
        (erase-buffer))
    (insert
     (concat
      "#include \"" file-header "\"\n"
      "\n"))
    (gob-insert-template "source")))

(defvar gob-type-to-function
  ;; function    type    value_type parameters
  '(
    ("guint"     "flags"   nil "UNKOWN_FLAGS" "default_value")
    ("gboolean"  "boolean" nil "TRUE")
    ("gchar"     "char"    nil "G_MININT8" "G_MAXINT8" "0")
    ("guchar"    "uchar"   nil "G_MINUINT8" "G_MAXUINT8" "0")
    ("gint"      "int"     nil "G_MININT" "G_MAXINT" "0")
    ("gint"      "enum"    nil "UNKOWN_TYPE" "default_value")
    ("guint"     "uint"    nil "G_MINUINT" "G_MAXUINT" "0")
    ("glong"     "long"    nil "G_MINLONG" "G_MAXLONG" "0")
    ("gulong"    "ulong"   nil "G_MINULONG" "G_MAXULONG" "0")
    ("gint64"    "int64"   nil "G_MININT64" "G_MAXINT64" "0")
    ("gfloat"    "float"   nil "G_MINFLOAT" "G_MAXFLOAT" "0")
    ("gdouble"   "double"  nil "G_MINDOUBLE" "G_MAXDOUBLE" "0")
    ("gchar *"   "string" "const gchar *" "\"default_value\"")
    ("param" "UNKOWN_TYPE")
    ("boxed" "UNKOWN_TYPE")
    ("pointer")
    ("object" "UNKOWN_TYPE")
    ("gtype" "UNKOWN_TYPE"))
  "")

(defun gob-create-property (data)
  (let ((str "") func)
    (dolist (prop data)
      (setq func (assoc (cadr prop) gob-type-to-function))
      (setq str
            (concat
             str
             (format
              "    g_object_class_install_property(
        gobject_class,
        PROP_%s,
        g_param_spec_%s(\"%s\",
                        \"%s\",
                        \"%s\",
                        %s,
                        %s));\n"
              (upcase (car prop))       ; NAME
              (cadr func)               ; function
              (car prop)
              (capitalize (car prop))
              (or (nth 2 prop) "blurb here")
              (mapconcat 'identity (nthcdr 3 func) ", ")
              (or (nth 3 prop) "G_PARAM_READWRITE")))))
    str))

(defun gob-function-set-property (data)
  (let (str prototype)
    (setq str
          (format 
           "static void %s_set_property ( GObject *object,
                                         guint property_id,
                                         const GValue *value,
                                         GParamSpec *pspec ) 
{
   %s%s* self = %s_%s(object);
    switch (property_id) {
"
           (assoc-default "prefix" gob-names)
           (gob-capitalize (assoc-default "namespace" gob-names))
           (gob-capitalize (assoc-default "name" gob-names))
           (upcase (assoc-default "namespace" gob-names))
           (upcase (assoc-default "name" gob-names))))
    (dolist (prop data)
      (setq str
            (concat str
                    (format
                     (concat
                      "    case PROP_%s:\n"
                      "       %s_set_%s(self, g_value_get_%s(value));\n"
                      "       break;\n")
                     (upcase (car prop))
                     (assoc-default "prefix" gob-names)
                     (car prop)
                     (cadr (assoc (cadr prop) gob-type-to-function))))))
    (setq str
          (concat str
                  "    default:\n"
                  "        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);\n"
                  "        break;\n"
                  "    }\n"
                  "}\n\n"))
    (dolist (prop data)
      (setq prototype
           (format
            "void %s_set_%s(%s%s *self, %s value)"
            (assoc-default "prefix" gob-names)
            (car prop)
            (gob-capitalize (assoc-default "namespace" gob-names))
            (gob-capitalize (assoc-default "name" gob-names))
            (or (nth 2 (assoc (cadr prop) gob-type-to-function))
                (cadr prop))))
      (add-to-list 'gob-public-methods (concat prototype ";"))
      (setq str
            (concat str
                    (format
                     (concat
                      "%s"
                      "{\n"
                      "    g_return_if_fail(%s_IS_%s(self));\n"
                      "%s"
                      "}\n\n")
                     prototype
                     (upcase (assoc-default "namespace" gob-names))
                     (upcase (assoc-default "name" gob-names))
                     (if (string= (cadr prop) "gchar *")
                         (format
                          (concat
                           "    g_free(self->%s);\n"
                           "    self->%s = g_strdup(value);\n")
                          (car prop) (car prop))
                     (format "    self->%s = value;\n"
                             (car prop)))))))
    str))

(defun gob-function-get-property (data)
  (let (str)
    (setq str
          (format 
           "static void %s_get_property ( GObject *object,
                                         guint property_id,
                                         GValue *value,
                                         GParamSpec *pspec ) 
{
   %s%s* self = %s_%s(object);
    switch (property_id) {
"
           (assoc-default "prefix" gob-names)
           (gob-capitalize (assoc-default "namespace" gob-names))
           (gob-capitalize (assoc-default "name" gob-names))
           (upcase (assoc-default "namespace" gob-names))
           (upcase (assoc-default "name" gob-names))))
    (dolist (prop data)
      (setq str
            (concat str
                    (format
                     (concat
                      "    case PROP_%s:\n"
                      "       g_value_set_%s(value, self->%s);\n"
                      "       break;\n")
                     (upcase (car prop))
                     (cadr (assoc (cadr prop) gob-type-to-function))
                     (car prop)))))
    (setq str
          (concat str
                  "    default:\n"
                  "        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);\n"
                  "        break;\n"
                  "    }\n"
                  "}\n"))))
;;}}}

(defun gob-parse-name (class-name class-parent)
  (setq class-name (downcase class-name)
        class-parent (if (string= "" class-parent)
                         "g_object"
                       (downcase class-parent)))
  (let ((name-parts (split-string class-name gob-class-seperator))
        (parent-parts (split-string class-parent gob-class-seperator))
        name namespace)
    (list
     (cons "namespace" (setq namespace (car name-parts)))
     (cons "name" (setq name (mapconcat 'identity (cdr name-parts) "_")))
     (cons "prefix" (if (and (= (length namespace) 0) (= (length name) 0))
                            ""
                          (format "%s_%s" namespace name)))
     (cons "parentnamespace" (downcase (car parent-parts)))
     (cons "parentname" (mapconcat 'downcase (cdr parent-parts) "_")))))

(defun gob-header (class-name class-parent)
  (interactive "sClass name(eg gtk_button): \nsParent class(eg gtk_bin): ")
  (let ((gob-names (gob-parse-name class-name class-parent)))
    (gob-insert-template "header")))

(defun gob-source (class-name class-parent)
  (interactive "sClass name(eg gtk_button): \nsParent class(eg gtk_bin): ")
  (let ((gob-names (gob-parse-name class-name class-parent)))
    (gob-insert-template "source")))

(defun gob-insert-template (template)
  (let ((tempo-interactive nil)
        (tmpl (assoc-default template gob-templates)))
    (tempo-insert-template 'tmpl nil)))

(defun gob-install-tempo ()
  (when gob-tempo-tags
    (mapc (lambda (pair)
            (tempo-define-template (car pair)
                                   (cdr pair)
                                   (concat (car pair) gob-tempo-suffix)
                                   (format "Insert gobject %s" (car pair))
                                   gob-tempo-tags))
          gob-templates)
    (setq gob-tempo-tags nil)))

(provide 'gob)
;;; gob.el ends here
