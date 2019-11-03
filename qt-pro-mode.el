;;; qt-pro-mode.el --- Qt Pro/Pri major mode

;; Copyright (C) 2007, 2018-2019  Free Software Foundation, Inc.

;; Author: Todd Neal <tolchz@gmail.com>
;; Keywords: extensions

;; Url: https://github.com/EricCrosson/qt-pro-mode

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:
;;
;; Version 1.1 (15 March 2018)
;; . deal with Qt 5.10 (modified by Vinicius José Latorre)
;;
;; Version 1.0 (7 January 2007)
;;
;; Based off simple.el
;;
;; Add the following to your .emacs to install
;;
;; (require 'qt-pro-mode)
;; (add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))
;;
;; or:
;;
;; (use-package qt-pro-mode
;;   :ensure t
;;   :mode ("\\.pro\\'" "\\.pri\\'"))

;;; Code:

;;;; TODO: Qt pro keymap
;;;; (defvar qt-pro-mode-map
;;;;   (let ((map (make-sparse-keymap)))
;;;;     (define-key map [foo] 'qt-pro-do-foo)
;;;;     map)
;;;;   "Keymap for `qt-pro-mode'.")

(defvar qt-pro-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?_ "w" st)
    st)
  "Syntax table for `qt-pro-mode'.")

(defvar qt-pro-font-lock-keywords
  `(;; \ at end of line for line continuation
    ("\\\\" (0 font-lock-preprocessor-face t))
    ;; ! (negation), | (or), : (nested scope), else
    ("\\([!|:]\\|else\\)" (0 font-lock-preprocessor-face t))
    ;; path specification
    ("[^][ \t/=(){}]*/" (0 font-lock-constant-face t))
    ;; path specification
    ("[^][ \t/=(){}]*/[-_A-Za-z0-9]+ " (0 font-lock-constant-face t))
    ;; path specification
    ("[^][ \t/=(){}]*/[-_A-Za-z0-9]+$" (0 font-lock-constant-face t))
    ;; $$(NAME), $(NAME) - environment variable expansion
    ("$$?(\\s *\\w+\\s *)" (0 font-lock-variable-name-face t))
    ;; $${NAME}, ${NAME} - environment variable expansion
    ("$$?{\\s *\\w+\\s *}" (0 font-lock-variable-name-face t))
    ;; $$NAME - qmake variable expansion
    ("$$\\w+" (0 font-lock-variable-name-face t))
    ;; $$basename(dir) - replace function
    (,(concat
       (regexp-opt '("absolute_path" "basename" "cat" "clean_path" "dirname"
		     "enumerate_vars" "escape_expand" "find" "files" "first"
		     "format_number" "fromfile" "getenv" "join" "last" "list"
		     "lower" "member" "num_add" "prompt" "quote" "re_escape"
		     "relative_path" "replace" "sprintf" "resolve_depends"
		     "reverse" "section" "shadowed" "shell_path" "shell_quote"
		     "size" "sort_depends" "sorted" "split" "str_member"
		     "str_size" "system" "system_path" "system_quote"
		     "take_first" "take_last" "unique" "upper"
		     "val_escape") "$$\\(")
       "(")
     (0 font-lock-function-name-face t))
    ;; $$[QMAKE_SPEC] - qmake property expansion
    (,(concat
       (regexp-opt '("QMAKE_SPEC" "QMAKE_VERSION" "QMAKE_XSPEC" "QT_HOST_BINS"
		     "QT_HOST_DATA" "QT_HOST_PREFIX" "QT_INSTALL_ARCHDATA"
		     "QT_INSTALL_BINS" "QT_INSTALL_CONFIGURATION"
		     "QT_INSTALL_DATA" "QT_INSTALL_DOCS" "QT_INSTALL_EXAMPLES"
		     "QT_INSTALL_HEADERS" "QT_INSTALL_IMPORTS"
		     "QT_INSTALL_LIBEXECS" "QT_INSTALL_LIBS"
		     "QT_INSTALL_PLUGINS" "QT_INSTALL_PREFIX" "QT_INSTALL_QML"
		     "QT_INSTALL_TESTS" "QT_INSTALL_TRANSLATIONS" "QT_SYSROOT"
		     "QT_VERSION" "QMAKE_EXT_MOC" "QMAKE_EXT_UI"
		     "QMAKE_EXT_PRL" "QMAKE_EXT_LEX" "QMAKE_EXT_YACC"
		     "QMAKE_EXT_OBJ" "QMAKE_EXT_CPP" "QMAKE_EXT_H") "$$\\[\\s *\\(")
       "\\s *\\]")
     (0 font-lock-variable-name-face t))
    ;; myvar.depends - qmake member
    (,(concat
       (regexp-opt '("CONFIG" "CONFIG.combine" "CONFIG.target_predeps"
		     "CONFIG.explicit_dependencies" "CONFIG.no_link"
		     "commands" "depend_command" "dependency_type"
		     "depends" "input" "name" "output" "output_function"
		     "recurse" "recurse_target" "subdir" "target"
		     "variable_out" "variables"
		     "files" "path") "\\.\\(")
       "\\>")
     (0 font-lock-builtin-face t))
    ;; exists(file) - test function
    (,(concat
       (regexp-opt '("cache" "CONFIG" "contains" "count" "debug" "defined"
		     "dirname" "equals" "error" "eval" "exists" "export" "find"
		     "for" "greaterThan" "if" "include" "infile"
		     "isActiveConfig" "isEmpty" "isEqual" "join" "lessThan"
		     "load" "log" "member" "message" "mkpath" "prompt" "quote"
		     "replace" "requires" "sprintf" "system" "touch" "unique"
		     "unset" "versionAtLeast" "versionAtMost" "warning"
		     "write_file" "packagesExist" "prepareRecursiveTarget"
		     "qtCompileTest" "qtHaveModule") `words)
       "(")
     (0 font-lock-function-name-face))
    ;; macro name
    (,(regexp-opt '("QT_DEPRECATED_WARNINGS"
		    "QT_NO_CAST_FROM_ASCII"
		    "QT_NO_CAST_TO_ASCII"
		    "QT_NO_NARROWING_CONVERSIONS_IN_CONNECT"
		    "QT_RESTRICTED_CAST_FROM_ASCII"
		    "QT_USE_QSTRINGBUILDER") `words)
     (0 font-lock-constant-face))
    ;; qmake variable
    (,(regexp-opt '("CONFIG" "DEFINES" "DEF_FILE" "DEPENDPATH" "DESTDIR"
		    "DISTFILES" "DLLDESTDIR" "FORMS" "GUID" "HEADERS" "ICON"
		    "IDLSOURCES" "INCLUDEPATH" "INSTALLS" "LEXIMPLS"
		    "LEXOBJECTS" "LEXSOURCES" "LIBS" "LITERAL_HASH" "MAKEFILE"
		    "MAKEFILE_GENERATOR" "MSVCPROJ_*" "MOC_DIR"
		    "OBJECTIVE_HEADERS" "OBJECTIVE_SOURCES" "OBJECTS"
		    "OBJECTS_DIR" "POST_TARGETDEPS" "PRE_TARGETDEPS"
		    "PRECOMPILED_HEADER" "PWD" "OUT_PWD" "QMAKE" "QMAKESPEC"
		    "QMAKE_AR_CMD" "QMAKE_BUNDLE_DATA" "QMAKE_BUNDLE_EXTENSION"
		    "QMAKE_CC" "QMAKE_CFLAGS" "QMAKE_CFLAGS_DEBUG"
		    "QMAKE_CFLAGS_RELEASE" "QMAKE_CFLAGS_SHLIB"
		    "QMAKE_CFLAGS_THREAD" "QMAKE_CFLAGS_WARN_OFF"
		    "QMAKE_CFLAGS_WARN_ON" "QMAKE_CLEAN" "QMAKE_CXX"
		    "QMAKE_CXXFLAGS" "QMAKE_CXXFLAGS_DEBUG"
		    "QMAKE_CXXFLAGS_RELEASE" "QMAKE_CXXFLAGS_SHLIB"
		    "QMAKE_CXXFLAGS_THREAD" "QMAKE_CXXFLAGS_WARN_OFF"
		    "QMAKE_CXXFLAGS_WARN_ON" "QMAKE_DEVELOPMENT_TEAM"
		    "QMAKE_DISTCLEAN" "QMAKE_EXTENSION_SHLIB"
		    "QMAKE_EXTENSION_STATICLIB" "QMAKE_EXT_MOC" "QMAKE_EXT_UI"
		    "QMAKE_EXT_PRL" "QMAKE_EXT_LEX" "QMAKE_EXT_YACC"
		    "QMAKE_EXT_OBJ" "QMAKE_EXT_CPP" "QMAKE_EXT_H"
		    "QMAKE_EXTRA_COMPILERS" "QMAKE_EXTRA_TARGETS"
		    "QMAKE_FAILED_REQUIREMENTS" "QMAKE_FRAMEWORK_BUNDLE_NAME"
		    "QMAKE_FRAMEWORK_VERSION" "QMAKE_HOST" "QMAKE_INCDIR"
		    "QMAKE_INCDIR_EGL" "QMAKE_INCDIR_OPENGL"
		    "QMAKE_INCDIR_OPENGL_ES2" "QMAKE_INCDIR_OPENVG"
		    "QMAKE_INCDIR_X11" "QMAKE_INFO_PLIST"
		    "QMAKE_IOS_DEPLOYMENT_TARGET" "QMAKE_LFLAGS"
		    "QMAKE_LFLAGS_CONSOLE" "QMAKE_LFLAGS_DEBUG"
		    "QMAKE_LFLAGS_PLUGIN" "QMAKE_LFLAGS_RPATH"
		    "QMAKE_LFLAGS_REL_RPATH" "QMAKE_REL_RPATH_BASE"
		    "QMAKE_LFLAGS_RPATHLINK" "QMAKE_LFLAGS_RELEASE"
		    "QMAKE_LFLAGS_APP" "QMAKE_LFLAGS_SHLIB"
		    "QMAKE_LFLAGS_SONAME" "QMAKE_LFLAGS_THREAD"
		    "QMAKE_LFLAGS_WINDOWS" "QMAKE_LIBDIR" "QMAKE_LIBDIR_FLAGS"
		    "QMAKE_LIBDIR_EGL" "QMAKE_LIBDIR_OPENGL"
		    "QMAKE_LIBDIR_OPENVG" "QMAKE_LIBDIR_X11" "QMAKE_LIBS"
		    "QMAKE_LIBS_EGL" "QMAKE_LIBS_OPENGL"
		    "QMAKE_LIBS_OPENGL_ES1, QMAKE_LIBS_OPENGL_ES2"
		    "QMAKE_LIBS_OPENVG" "QMAKE_LIBS_THREAD" "QMAKE_LIBS_X11"
		    "QMAKE_LIB_FLAG" "QMAKE_LINK" "QMAKE_LINK_SHLIB_CMD"
		    "QMAKE_LN_SHLIB" "QMAKE_OBJECTIVE_CFLAGS" "QMAKE_POST_LINK"
		    "QMAKE_PRE_LINK" "QMAKE_PROJECT_NAME"
		    "QMAKE_PROVISIONING_PROFILE" "QMAKE_MAC_SDK"
		    "QMAKE_MACOSX_DEPLOYMENT_TARGET" "QMAKE_MAKEFILE"
		    "QMAKE_QMAKE" "QMAKE_RESOURCE_FLAGS" "QMAKE_RPATHDIR"
		    "QMAKE_RPATHLINKDIR" "QMAKE_RUN_CC" "QMAKE_RUN_CC_IMP"
		    "QMAKE_RUN_CXX" "QMAKE_RUN_CXX_IMP" "QMAKE_SONAME_PREFIX"
		    "QMAKE_TARGET" "QMAKE_TARGET_COMPANY"
		    "QMAKE_TARGET_DESCRIPTION" "QMAKE_TARGET_COPYRIGHT"
		    "QMAKE_TARGET_PRODUCT" "QMAKE_TVOS_DEPLOYMENT_TARGET"
		    "QMAKE_UIC_FLAGS" "QMAKE_WATCHOS_DEPLOYMENT_TARGET" "QT"
		    "QTPLUGIN" "QT_VERSION" "QT_MAJOR_VERSION"
		    "QT_MINOR_VERSION" "QT_PATCH_VERSION" "RC_FILE"
		    "RC_CODEPAGE" "RC_DEFINES" "RC_ICONS" "RC_LANG"
		    "RC_INCLUDEPATH" "RCC_DIR" "REQUIRES" "RESOURCES"
		    "RES_FILE" "SOURCES" "SUBDIRS" "TARGET" "TARGET_EXT"
		    "TARGET_x" "TARGET_x.y.z" "TEMPLATE" "TRANSLATIONS"
		    "UI_DIR" "VERSION" "VERSION_PE_HEADER" "VER_MAJ" "VER_MIN"
		    "VER_PAT" "VPATH" "WINRT_MANIFEST" "YACCSOURCES"
		    "_PRO_FILE_" "_PRO_FILE_PWD_") `words)
     (0 font-lock-keyword-face)))
  "Keyword highlighting specification for `qt-pro-mode'.")

;;;###autoload
(define-derived-mode qt-pro-mode fundamental-mode "Qt-pro"
  "A major mode for editing Qt-pro files."
  :syntax-table qt-pro-mode-syntax-table
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults) '(qt-pro-font-lock-keywords)))


(provide 'qt-pro-mode)

;;; qt-pro-mode.el ends here
