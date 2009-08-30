;;; gobject-class.el --- functions to easy GObject-based class developers
;; Author: Gustavo Sverzut Barbieri <barbieri@gmail.com>
;; Copyright: public domain
;; URL: http://blog.gustavobarbieri.com.br/old-website/gobject-class.el
;; EmacsWiki: GObjectClassHelpers
;; Keywords: gobject, glib, gtk, helper, utilities
;;; Commentary:
;;; Code:


(defun string-join (list separator)
  "Takes a list of string and joins them using delimiter."
  (mapconcat (lambda (x) x) list separator))

(defun string-concat (list)
  "Takes a list of strings and joins them."
  (mapconcat (lambda (x) x) list ""))

(defun ask-value-non-empty (prompt)
  "Ask a question in minibuffer and ensure it's not empty string."
  (let ((x (read-string prompt)))
    (if (string= "" x)
	(ask-value-non-empty prompt)
      x)))

(defun gobject-class-header (class_name parent_class_name)
  "Generate the header definition for a GObject derived class.

It takes 2 parameters:
   CLASS_NAME: class name, like 'gtk_button' or 'gtk_tree_view'. First
      element before the underscore character (_) will be used as name
      space. Example: 'gtk_button' is the 'button' class in 'gtk' name
      space.
   PARENT_CLASS_NAME: parent class name, like 'g_object'. First element
      before the underscore character (_) will be used as name space.
      Example: 'g_object' is the 'object' class in 'g' namespace.
"
  (interactive "sClass name (ie: gtk_tree_view): \nsParent class name (default: g_object): ")

  (defun right-fill (len string)
    "Takes a string and fill it to take at least len characters"
    (setq missing (- len (length string)))
    (if	(> missing 0)
	(concat string (make-string missing ?\  ))
      string))
  (defun macro-line-cnt (&rest sequences)
    (concat (right-fill 72 (string-concat sequences)) "\\\n"))
  (defun macro-line-end (&rest sequences)
    (concat (string-concat sequences) "\n"))


  (let* ((parent_class_name (if (string= "" parent_class_name)
				"g_object"
			      (downcase parent_class_name)))
	 (class_name        (downcase (if (string= "" class_name)
					  (ask-value-non-empty
					   "You must provide class name (ie: gtk_tree_view): ")
					class_name)))
	 (pieces-class_name  (split-string (downcase class_name) "_"))
	 (pieces-parent_class_name (split-string parent_class_name "_"))
	 (namespace       (car-safe pieces-class_name))
	 (name            (string-join (cdr-safe pieces-class_name) "_"))
	 (ParentClassName (mapconcat 'capitalize pieces-parent_class_name ""))
	 (NAMESPACE       (upcase namespace))
	 (NAME            (upcase name))
	 (NameSpace       (capitalize namespace))
	 (Name            (mapconcat 'capitalize (cdr-safe pieces-class_name) ""))
	 (ClassName       (concat NameSpace Name))
	 )

    (insert
     (concat
      "\n"
      "G_BEGIN_DECLS\n"
      "\n"
      (macro-line-cnt "#define " NAMESPACE "_TYPE_" NAME)
      (macro-line-end "   (" namespace "_" name "_get_type())")
      (macro-line-cnt "#define " NAMESPACE "_" NAME "(obj)")
      (macro-line-cnt "   (G_TYPE_CHECK_INSTANCE_CAST ((obj),")
      (macro-line-cnt "                                " NAMESPACE "_TYPE_" NAME ",")
      (macro-line-end "                                " ClassName "))")
      (macro-line-cnt "#define " NAMESPACE "_" NAME "_CLASS(klass)")
      (macro-line-cnt "   (G_TYPE_CHECK_CLASS_CAST ((klass),")
      (macro-line-cnt "                             " NAMESPACE "_TYPE_" NAME ",")
      (macro-line-end "                             " ClassName "Class))")
      (macro-line-cnt "#define IS_" NAMESPACE "_" NAME "(obj)")
      (macro-line-cnt "   (G_TYPE_CHECK_INSTANCE_TYPE ((obj),")
      (macro-line-end "                                " NAMESPACE "_TYPE_" NAME "))")
      (macro-line-cnt "#define IS_" NAMESPACE "_" NAME "_CLASS(klass)")
      (macro-line-cnt "   (G_TYPE_CHECK_CLASS_TYPE ((klass),")
      (macro-line-end "                             " NAMESPACE "_TYPE_" NAME "))")
      (macro-line-cnt "#define " NAMESPACE "_" NAME "_GET_CLASS(obj)")
      (macro-line-cnt "   (G_TYPE_INSTANCE_GET_CLASS ((obj),")
      (macro-line-cnt "                               " NAMESPACE "_TYPE_" NAME ",")
      (macro-line-end "                               " ClassName "Class))")
      "\n"
      "typedef struct _" ClassName "      " ClassName ";\n"
      "typedef struct _" ClassName "Class " ClassName "Class;\n"
      "\n"
      "struct _" ClassName "Class\n"
      "{\n"
      "    " ParentClassName "Class parent_class;\n"
      "};\n"
      "\n"
      "struct _" ClassName "\n"
      "{\n"
      "    " ParentClassName " parent;\n"
      "};\n"
      "\n"
      "GType " namespace "_" name "_get_type (void) G_GNUC_CONST;\n"
      "\n"
      "G_END_DECLS\n"
      "\n"
      )
     )
    )
  )

(defun gobject-class-code (class_name parent_class_name)
  "Generate the code implementation for a GObject derived class.

It takes 2 parameters:
   CLASS_NAME: class name, like 'gtk_button' or 'gtk_tree_view'. First
      element before the underscore character (_) will be used as name
      space. Example: 'gtk_button' is the 'button' class in 'gtk' name
      space.
   PARENT_CLASS_NAME: parent class name, like 'g_object'. First element
      before the underscore character (_) will be used as name space.
      Example: 'g_object' is the 'object' class in 'g' namespace.
"
  (interactive "sClass name (ie: gtk_tree_view): \nsParent class name (default: g_object): ")

  (let* ((parent_class_name (if (string= "" parent_class_name)
				"g_object"
			      (downcase parent_class_name)))
	 (class_name        (downcase (if (string= "" class_name)
					  (ask-value-non-empty
					   "You must provide class name (ie: gtk_tree_view): ")
					class_name)))
	 (pieces-class_name  (split-string (downcase class_name) "_"))
	 (pieces-parent_class_name (split-string parent_class_name "_"))
	 (namespace       (car-safe pieces-class_name))
	 (name            (string-join (cdr-safe pieces-class_name) "_"))
	 (ParentClassName (mapconcat 'capitalize pieces-parent_class_name ""))
	 (NameSpace       (capitalize namespace))
	 (Name            (mapconcat 'capitalize (cdr-safe pieces-class_name) ""))
	 (ClassName       (concat NameSpace Name))
	 (class_name      (concat namespace "_" name))
         (PARENT_NAMESPACE (upcase (car-safe pieces-parent_class_name)))
         (PARENT_NAME      (mapconcat 'upcase (cdr-safe pieces-parent_class_name) "_"))
	 )

    (insert
     (concat
      "\n"
      "static void " class_name "_dispose (GObject *object);\n"
      "static void " class_name "_finalize (GObject *object);\n"
      "\n"
      "G_DEFINE_TYPE (" ClassName ", " class_name ", " PARENT_NAMESPACE "_TYPE_" PARENT_NAME ");\n"
      "\n"
      "static void\n"
      class_name "_class_init (" ClassName "Class *klass)\n"
      "{\n"
      "    GObjectClass *gobject_class = (GObjectClass *)klass;\n"
      "\n"
      "    gobject_class->dispose = " class_name "_dispose;\n"
      "    gobject_class->finalize = " class_name "_finalize;\n"
      "}\n"
      "\n"
      "static void\n"
      class_name "_init (" ClassName " *self)\n"
      "{\n"
      "}\n"
      "\n"
      "static void\n"
      class_name "_dispose (GObject *object)\n"
      "{\n"
      "    " ClassName " *self = (" ClassName " *)object;\n"
      "\n"
      "    G_OBJECT_CLASS (" class_name "_parent_class)->dispose (object);\n"
      "}\n"
      "\n"
      "static void\n"
      class_name "_finalize (GObject *object)\n"
      "{\n"
      "    " ClassName " *self = (" ClassName " *)object;\n"
      "\n"
      "    g_signal_handlers_destroy (object);\n"
      "    G_OBJECT_CLASS (" class_name "_parent_class)->finalize (object);\n"
      "}\n"
      "\n"
      )
     )
    )
  )

(defun gobject-class-generate (class_name parent_class_name)
  "Generate header (.h) and code (.c) files for a GObject derived class.

It takes 2 parameters:
   CLASS_NAME: class name, like 'gtk_button' or 'gtk_tree_view'. First
      element before the underscore character (_) will be used as name
      space. Example: 'gtk_button' is the 'button' class in 'gtk' name
      space.
   PARENT_CLASS_NAME: parent class name, like 'g_object'. First element
      before the underscore character (_) will be used as name space.
      Example: 'g_object' is the 'object' class in 'g' namespace.
"
  (interactive "sClass name (ie: gtk_tree_view): \nsParent class name (default: g_object): ")

  (let* ((parent_class_name (if (string= "" parent_class_name)
				"g_object"
			      (downcase parent_class_name)))
	 (class_name        (downcase (if (string= "" class_name)
					  (ask-value-non-empty
					   "You must provide class name (ie: gtk_tree_view): ")
					class_name)))
	 (pieces-class_name  (split-string (downcase class_name) "_"))
	 (pieces-parent_class_name (split-string parent_class_name "_"))
	 (base_file_name (string-join pieces-class_name "-"))
	 (file_header (concat base_file_name ".h"))
	 (file_code   (concat base_file_name ".c"))
	 (DEFINE_NAME (concat (upcase class_name) "_H"))
	 (parent_include (if (string= "g_object" parent_class_name)
			     "glib-object.h"
			   (if (string-match "^gtk_" parent_class_name)
			       "gtk/gtk.h"
			     (concat (string-join pieces-parent_class_name "-") ".h")
			     )
			   ))
	 )

    (delete-other-windows)
    (split-window-vertically)

    (find-file file_header)
    (insert
     (concat
      "#ifndef __" DEFINE_NAME "__\n"
      "#define __" DEFINE_NAME "__\n"
      "\n"
      "#include <" parent_include ">\n"
      "\n"))
    (gobject-class-header class_name parent_class_name)
    (insert "#endif /* __" DEFINE_NAME "__ */\n")

    (other-window 1)

    (find-file file_code)
    (insert
     (concat
      "#include \"" file_header "\"\n"
      "\n"))
    (gobject-class-code class_name parent_class_name)
    )
  )

(provide 'gobject-class)

;;; gobject-class.el ends here
