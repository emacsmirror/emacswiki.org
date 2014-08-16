;;; dummy-h-mode.el --- switch major mode to c-/c++-/objc-mode on .h file

;;; Copyright (C) 2012-2014 yascentur

;; Author:   yascentur <screenname at gmail dot com>
;; Keywords: c-mode c++-mode objc-mode
;; Version:  1.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The `dummy-h-mode' is a major mode for header files (.h)
;; of C, C++ or Objective-C.  It detects the suitable major mode
;; which is one of `c-mode', `c++-mode' and `objc-mode',
;; then switches the buffer major mode to the detected major mode.
;; The detecting process is as follows:
;;
;; 1. Check the existance of the corresponding source file (.c, .cc or ...)
;; 2. Search keywords in the file.
;; 3. Check the filename extensions of all files in the directory.

;;; Usage:

;; Add following 2 lines into your init file.
;;
;;     (add-to-list 'auto-mode-alist '("\\.h$" . dummy-h-mode))
;;     (autoload 'dummy-h-mode "dummy-h-mode" "Dummy H mode" t)

;;; Customization:

;; Set default major mode (`c-mode', `c++-mode' or `objc-mode').
;; Default: `c-mode'.
;;
;;     (add-hook 'dummy-h-mode-hook
;;               (lambda ()
;;                 (setq dummy-h-mode-default-major-mode 'c++-mode)))
;;
;; Set additional C keywords and their minimum counts of appearance.
;;
;;     (add-hook 'dummy-h-mode-hook
;;               (lambda ()
;;                 (add-to-list 'dummy-h-mode-c-keywords
;;                              '("\*[ \t]*restrict" . 3))))
;;
;; In this customization, `c-mode' would be detected if `\*[ \t]*restrict'
;; appears more than 3 times in the file.  The `dummy-h-mode-cc-keywords'
;; for C++ and the `dummy-h-mode-objc-keywords' for Objective-C
;; are also available.
;;
;; Set search limit in keywords searching detection for large size files.
;; Default: 30000 chars (equal to ca. 1000 lines).
;;
;;     (add-hook 'dummy-h-mode-hook
;;               (lambda ()
;;                 (setq dummy-h-mode-search-limit 60000)))

;;; Code:

(eval-when-compile (require 'cl))

(defgroup dummy-h-mode nil
  "Dummy H mode"
  :group  'convenience
  :prefix "dummy-h-mode-")

(defcustom dummy-h-mode-hook nil
  "Hook run when entering dummy H mode"
  :type  'hook
  :group 'dummy-h-mode)

(defcustom dummy-h-mode-default-major-mode 'c-mode
  "Default major mode"
  :type  'symbol
  :group 'dummy-h-mode)

(defcustom dummy-h-mode-c-keywords
  '(("_Bool[\* \t\n\r]" . 1)
    ("_Complex[\* \t\n\r]" . 1)
    ("_Imaginary[\* \t\n\r]" . 1))
  "C keywords and their minimum counts of appearance"
  :type  '(repeat (cons regexp number))
  :group 'dummy-h-mode)

(defcustom dummy-h-mode-cc-keywords
  '(("[\n\r][ \t]*private:" . 1)
    ("[\n\r][ \t]*public:" . 1)
    ("[\n\r][ \t]*protected:" . 1)
    ("::" . 1)
    ("[\n\r][ \t]*using[ \t\n\r]+namespace[ \t\n\r]" . 1))
  "C++ keywords and their minimum counts of appearance"
  :type  '(repeat (cons regexp number))
  :group 'dummy-h-mode)

(defcustom dummy-h-mode-objc-keywords
  '(("[\n\r][ \t]*@class[ \t\n\r]" . 1)
    ("[\n\r][ \t]*@end[ \t\n\r]" . 1)
    ("[\n\r][ \t]*@implementation[ \t\n\r]" . 1)
    ("[\n\r][ \t]*@interface[ \t\n\r]" . 1)
    ("[\n\r][ \t]*@protocol[ \t\n\r]" . 1))
  "Objective-C keywords and their minimum counts of appearance"
  :type  '(repeat (cons regexp number))
  :group 'dummy-h-mode)

(defcustom dummy-h-mode-search-limit 30000
  "Search limit for large size files"
  :type  'number
  :group 'dummy-h-mode)

(defun dummy-h-mode-get-major-mode-by-source-file ()
  "Get major mode by checking corresponding source file"
  (if (buffer-file-name)
      (let ((file-name-wo-h (file-name-sans-extension (buffer-file-name))))
        (cond
         ((file-exists-p (concat file-name-wo-h ".c"))
          'c-mode)
         ((or (file-exists-p (concat file-name-wo-h ".cc"))
              (file-exists-p (concat file-name-wo-h ".cxx"))
              (file-exists-p (concat file-name-wo-h ".cpp"))
              (file-exists-p (concat file-name-wo-h ".cp")))
          'c++-mode)
         ((or (file-exists-p (concat file-name-wo-h ".m"))
              (file-exists-p (concat file-name-wo-h ".mm")))
          'objc-mode)))
    nil))

(defun dummy-h-mode-if-containing-keywords (keywords)
  "Get if containing keywords"
  (if keywords
      (or (save-excursion
            (goto-char (point-min))
            (re-search-forward
             (caar keywords) dummy-h-mode-search-limit t (cdar keywords)))
          (dummy-h-mode-if-containing-keywords (cdr keywords)))
    nil))

(defun dummy-h-mode-get-major-mode-by-keywords ()
  "Get major mode by searching keywords"
  (cond
   ((dummy-h-mode-if-containing-keywords dummy-h-mode-objc-keywords)
    'objc-mode)
   ((dummy-h-mode-if-containing-keywords dummy-h-mode-cc-keywords)
    'c++-mode)
   ((dummy-h-mode-if-containing-keywords dummy-h-mode-c-keywords)
    'c-mode)))

(defun dummy-h-mode-count-file-extension (files extension)
  "Counts file if its extension is matching"
  (if files
      (+ (if (string= (file-name-extension (car files)) extension) 1 0)
         (dummy-h-mode-count-file-extension (cdr files) extension))
    0))

(defun dummy-h-mode-get-major-mode-by-files-directory ()
  "Get major mode by checking all files in directory"
  (if (buffer-file-name)
      (let* ((dir-files
              (directory-files (file-name-directory (buffer-file-name))))
             (count-c-files
              (dummy-h-mode-count-file-extension dir-files "c"))
             (count-cc-files
              (+ (dummy-h-mode-count-file-extension dir-files "cc")
                 (dummy-h-mode-count-file-extension dir-files "cxx")
                 (dummy-h-mode-count-file-extension dir-files "cpp")
                 (dummy-h-mode-count-file-extension dir-files "cp")))
             (count-objc-files
              (+ (dummy-h-mode-count-file-extension dir-files "m")
                 (dummy-h-mode-count-file-extension dir-files "mm"))))
        (cond
         ((and (> count-c-files count-cc-files)
               (> count-c-files count-objc-files))
          'c-mode)
         ((> count-cc-files count-objc-files)
          'c++-mode)
         ((> count-objc-files 0)
          'objc-mode)))
    nil))

(defun dummy-h-mode-switch ()
  "Switch major mode"
  (funcall (or (dummy-h-mode-get-major-mode-by-source-file)
               (dummy-h-mode-get-major-mode-by-keywords)
               (dummy-h-mode-get-major-mode-by-files-directory)
               dummy-h-mode-default-major-mode)))

(defun dummy-h-mode ()
  "Dummy H mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dummy-h-mode)
  (setq mode-name "DummyH")
  (run-hooks 'dummy-h-mode-hook)
  (dummy-h-mode-switch))

(provide 'dummy-h-mode)

;;; dummy-h-mode.el ends here
