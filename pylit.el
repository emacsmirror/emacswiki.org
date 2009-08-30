;;; pylit.el --- support for the PyLit literate programming tool
;;;
;;; Author: Riccardo Murri <riccardo.murri@gmail.com>
;;; Version: 0.2.2
;;;
;;; Copyright (c) 2007 Riccardo Murri <riccardo.murri@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA


;;; Commentary:

;; pylit.el
;; ========
;;
;; This package implements the command ``pylit-toggle`` to switch
;; between alternate views (code or documentation) of the same
;; PyLit literate source.
;;
;; When invoked on a buffer, ``pylit-toggle`` will determine (based on
;; the buffer file name extension) whether it's a code or documentation
;; buffer, then it will invoke the ``pylit`` shell command to translate
;; the buffer contents to the other form, and display the new buffer.
;;
;;
;; Invocation of PyLit_
;; --------------------
;;
;; .. _PyLit: http://pylit.berlios.de/
;;
;; *Note:* ``pylit.el`` assumes that the PyLit_ tool is available as
;; shell command ``pylit`` on the current execution path.  To alter
;; this, customize the ``pylit-command`` variable.
;;
;; As of 2007-02-09, the PyLit_ distribution does not include a script
;; to invoke PyLit's functionality from the command line.  On any
;; UNIX-like system, this can be easily worked around by creating a
;; file ``pylit`` somewhere in your executable search path (the
;; ``PATH``) with the following contents:
;;
;; | #!/bin/sh
;; | exec env PYTHONPATH=/path/to/pylit/repository/src \
;; |   python /path/to/pylit/repository/src.pylit.py "$@"
;;
;;
;; Issues with specific language modes
;; ===================================
;;
;; Python
;; ------
;; 
;; By default, ``python-mode`` will take comments into account when
;; computing indentation; so, any PyLit literate comment will reset
;; the indentation.  If the Emacs customization variable
;; ``py-honor-comment-indentation`` is set to ``nil``, then
;; ``python-mode`` will ignore any comments for code indentation
;; purposes.
;;
;;
;; TO-DO
;; =====
;;
;; - SHOULD support different doc files extensions, not just ".txt"
;;   (at least, ".rst" ".rest" ".text")
;; - Provide a way to specify the direction of conversion:
;;   code2txt or txt2code.
;;   - add a prefix argument to ``pylit-toggle``?
;;   - or provide explicit conversion functions ``pylit-code2txt``
;;     and ``pylit-txt2code``?
;; - Provide a function to strip documentation comments (``pylit -s -c``)
;;   - either provide function ``pylit-extract-code``,
;;   - or make this behavior selectable by a prefix argument 
;;     to ``pylit-txt2code`` (see above)
;; - add unit tests using regress.el
;;


;;; History:

;; ChangeLog
;; =========
;;
;; 0.1 (2007-01-20)
;;   * First working draft.
;; 0.2
;;   * BUGFIX: display the converted buffer only in ``pylit-toggle``
;;   * record the paired buffer in a buffer-local variable;
;;     this fixes a possible bug when a buffer of the same name of
;;     the would-be destination buffer is already in use.
;; 0.2.1 (2007-02-21)
;;   * Reformatted documentation to be compatible with PyLit's proposed
;;     Emacs Lisp filters.
;; o.2.2 (2007-02-25)
;;   * Added note on the interaction with ``python-mode``.


;;; Code:

;; Implementation
;; ==============
;;
;; .. note:: Functions that are not intended for use outside of this
;;           module feature a double '-' in their names, 
;;           e.g. ``pylit--find-paired``.
;;

(defgroup pylit nil
  "Support for the PyLit literate programming tool.")

(defcustom pylit-command "pylit"
  "Path name of the `pylit' executable."
  :group 'pylit
  :type 'string)

;; XXX: is this really needed?
(defcustom pylit-default-literate-file-extension "txt"
  "The default extension for literate document files.

Must NOT include a leading dot `.'")

(make-variable-buffer-local 'pylit--paired-buffer)
(defvar pylit--paired-buffer nil
  "Buffer to store PyLit conversion output.")

(make-variable-buffer-local 'pylit--paired-conv)
(defvar pylit--paired-conv nil 
  "Direction of the conversion to fill `pylit--paired-buffer'.")


;; XXX: should replace TEXT-EXT with a regexp match on the extension?
(defun pylit--find-paired (filename &optional text-ext)
  "Guess the name and kind of the buffer where PyLit output should be stored.

Returns a cons cell (NAME . CONV) where NAME (a string) is the
name of the buffer where PyLit output should be stored, and CONV
is one of the two symbols 'txt2code or 'code2txt, specifying
which direction the PyLit conversion should be performed to fill
buffer NAME.

The name of the output buffer is obtained by adding or
removing TEXT-EXT from the passed filename.  For instance::

  (pylit--find-paired-buffer-append \"file.py\" \"txt\")
  \"file.py.txt\"

  (pylit--find-paired-buffer-append \"file.py.txt\" \"txt\")
  \"file.py\"
"
  (unless text-ext
    (setq text-ext pylit-default-literate-file-extension))
  (let*
      ((ext (file-name-extension filename))
       (sans (file-name-sans-extension filename)))
    ;; XXX: should replace this with a regexp match on the extension?
    (if (string-equal ext text-ext)
        ;; return filename with .txt extension removed
        (cons sans 'txt2code)
      ;; else, append .txt extension
      (cons (concat filename "." text-ext) 'code2txt))))

(defun pylit--find-paired-file-name (filename &optional text-ext)
  "Return the name of the paired PyLit buffer."
  (car (pylit--find-paired filename text-ext)))

(defun pylit--find-paired-conv (filename &optional text-ext)
  "Return the conversion to get the paired PyLit buffer."
  (cdr (pylit--find-paired filename text-ext)))


(defun pylit--opposite-conversion (conv)
  "Return the conversion direction opposite to CONV."
  (cond 
   ((eq conv 'txt2code) 'code2txt)
   ((eq conv 'code2txt) 'txt2code)))


(defun pylit--make-conversion-command (conversion &optional strip)
  "Return the correct PyLit incantation to convert reST to code or code to text.

First argument CONVERSION must be one of the symbols 'txt2code or 'code2txt,
to specify which conversion should be attempted.

If optional second argument STRIP is non-nil, then PyLit will
also strip documentation comments from the output."
  (cond
   ((eq conversion 'code2txt)
    (format "%s %s -c -" pylit-command (if strip "-s" "")))
   ((eq conversion 'txt2code)
    (format "%s -t -" pylit-command))
   (t 
    ;; XXX: is this the right error condition?
    (signal 'wrong-type-argument
     (list conversion "(should be symbol 'txt2code or 'code2txt)")))))


(defun pylit--convert-buffer (conversion src-buffer dest-buffer &optional strip)
  "Convert contents of SRC-BUFFER, putting the result into DEST-BUFFER.

First argument CONVERSION is either one of symbols 'txt2code or
'code2txt. 

Second argument SRC-BUFFER is a buffer (or a buffer name) whose
contents are fed to PyLit as input; PyLit's output is stored into
the buffer specified by third argument DEST-BUFFER (which is
created if not existing, and whose former contents are lost)

If optional fourth argument STRIP (prefix argument when called
interactively) is non-nil, then strip all documentation comments
from the result (thus preventing further conversion the other way
round, from commented code to reST)."
  ;; if DEST-BUFFER has unsaved changes, 
  ;; offer user a chance to abort operation;
  (if
      (and 
       (buffer-modified-p dest-buffer)
       (yes-or-no-p 
        (format 
         "Buffer `%s' has unsaved changes, that will be overwritten by this operation. Abort? " 
         (buffer-name dest-buffer))))
      (signal 'quit "PyLit text to code conversion aborted"))
  ;; erase contents of destination buffer
  (save-excursion
    (set-buffer dest-buffer)
    (erase-buffer))
  (save-excursion
    (set-buffer src-buffer)
    ;; if SRC-BUFFER has been modified, offer user a chance to save
    (if (and 
         (buffer-modified-p src-buffer)
         (yes-or-no-p 
          (format "Buffer `%s' has unsaved changes. Save it before conversion? " 
                  (buffer-name src-buffer))))
        (save-buffer))
    ;; invoke PyLit on buffer contents
    (shell-command-on-region (point-min) (point-max) 
                             (pylit--make-conversion-command conversion strip)
                             dest-buffer nil " *PyLit errors*")))


(defun pylit-toggle ()
  "Run PyLit's txt2code or code2txt conversion and switch to converted buffer."
  (interactive)
  (let*
      ((name-and-conv 
        (pylit--find-paired (buffer-file-name (current-buffer))))
       (name (car name-and-conv))
       (conv (cdr name-and-conv))
       (src-buffer (current-buffer)))
    (unless (and pylit--paired-buffer (buffer-live-p pylit--paired-buffer))
        (setq pylit--paired-buffer (get-buffer-create name))
        (setq pylit--paired-conv conv))
    (pylit--convert-buffer pylit--paired-conv 
                           src-buffer
                           pylit--paired-buffer)
    (if (get-buffer-window pylit--paired-buffer)
        (select-window (get-buffer-window pylit--paired-buffer))
      (switch-to-buffer pylit--paired-buffer))
    (setq pylit--paired-buffer src-buffer)
    (setq pylit--paired-conv (pylit--opposite-conversion conv))
    (unless buffer-file-name 
      ;; newly-created buffer, set up file name to save to and major
      ;; mode.
      (setq buffer-file-name name)
      (set-auto-mode))))


;; that's all folks!
(provide 'pylit)

;;; pylit.el ends here
