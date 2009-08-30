;;; auto-complete-extension.el --- Some extension for auto-complete-mode

;; Filename: auto-complete-extension.el
;; Description: Some extension for auto-complete-mode
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-02 11:12:53
;; Version: 0.1
;; Last-Updated: 2008-12-02 11:12:56
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/auto-complete-extension.el
;; Keywords: auto-complete
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `auto-complete'
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some extension for auto-complete-mode.
;;
;; Below are commands you can use:
;;
;; `ac-source-gtags'
;;      Provide a completion for C or C++.
;;      You need install `gtags' first.
;;
;; `ac-source-c++'
;;      Provide a completion keyword for C++.
;;
;; `ac-source-haskell'
;;      Provide a completion for Haskell.
;;      You need install `GHC' and `hoogle' first.
;;
;; About how to use this package, please see:
;;
;; http://www.emacswiki.org/emacs/download/init-auto-complete.el
;;

;;; Installation:
;;
;; Put auto-complete-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'auto-complete-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2008/12/02
;;      First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'auto-complete)

;;; Code:

;; Haskell
(defun ac-haskell-hoogle (prefix)
  (let (expansion all-expansions end-of-period)
    (with-temp-buffer
      ;; Remove period from end of line.
      (when (string-match "\\(.*\\)\\.$" prefix)
        (setq prefix (match-string 1 prefix))
        (setq end-of-period t))
      ;; Search candidate use `hoogle'.
      (call-process  "hoogle" nil t nil "" prefix)
      ;; Get match candidate.
      (goto-char (point-min))
      (while (re-search-forward
              "^\\(\\(module\\|keyword\\|Prelude\\|Data\\|Language\\.Haskell[^ ]*\\)\\( type\\| class\\)?\\)[ \\.]\\([^ \n]+\\)"
              nil t)
        (setq expansion (match-string 4))
        (setq all-expansions (cons expansion all-expansions)))
      ;; Search class.
      (goto-char (point-min))
      (while (re-search-forward
              "^Prelude class .*=> \\([^ \n]+\\)"
              nil t)
        (setq expansion (match-string 1))
        (setq all-expansions (cons expansion all-expansions)))
      ;; Search calss level.
      (goto-char (point-min))
      ;; Add period when option `end-of-period'
      ;; is `non-nil'.
      (when end-of-period
        (setq prefix (format "%s." prefix)))
      (while (re-search-forward
              (format "^%s[^ \n]+" prefix)
              nil t)
        (setq expansion (match-string 0))
        (setq all-expansions (cons expansion all-expansions))))
    all-expansions))

;; http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html
(defconst ac-haskell-ghc-pragmas
  (sort
   (list "LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "DEPRECATED" "INLINE" "NOINLINE"
         "LINE" "RULES" "SPECIALIZE" "UNPACK" "SOURCE")
   #'(lambda (a b) (> (length a) (length b))))
  "GHC pragmas.")

(defconst ac-haskell-defined-punctunation
  (sort
   (list "==" "/=" "<=" ">=" ">>=" ">>" "**" "^^")
   #'(lambda (a b) (> (length a) (length b))))
  "Defined punctunation in Haskell.")

(defconst ac-haskell-misc
  (sort
   (list "-fglasgow-exts")
   #'(lambda (a b) (> (length a) (length b))))
  "GHC pragmas.")

(defvar ac-source-haskell
  '((candidates . (lambda ()
                    (all-completions ac-target
                                     (append nil
                                             ac-haskell-defined-punctunation
                                             ac-haskell-ghc-pragmas
                                             (ac-haskell-hoogle ac-target)
                                             ac-haskell-misc)))))
  "Sources for Haskell keywords.")

(provide 'auto-complete-extension)

;;; auto-complete-extension.el ends here
