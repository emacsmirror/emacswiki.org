;;; init-lsp.el --- Configuration for lsp-mode

;; Filename: init-lsp.el
;; Description: Configuration for lsp-mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-10 22:46:41
;; Version: 0.2
;; Last-Updated: 2018-09-14 09:48:52
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-lsp.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
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
;; Configuration for lsp-mode
;;

;;; Installation:
;;
;; Put init-lsp.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-lsp)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-lsp RET
;;

;;; Change log:
;;
;; 2018-09-14
;;      * Switch from eglot to lsp-mode since lsp-ruby looks fix disconnect problem.
;;
;; 2018/09/10
;;      * First released.
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
(require 'lsp-mode)
(require 'lsp-ruby)
(require 'lsp-python)
(require 'lsp-javascript-typescript)
(require 'company-lsp)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Enable LSP backend.
(push 'company-lsp company-backends)

;; Configuration to fix LSP errors.
(setq lsp-enable-eldoc nil) ;we will got error "Wrong type argument: sequencep" from `eldoc-message' if `lsp-enable-eldoc' is non-nil
(setq lsp-message-project-root-warning t) ;avoid popup warning buffer if lsp can't found root directory (such as edit simple *.py file)
(setq create-lockfiles nil) ;we will got error "Error from the Language Server: FileNotFoundError" if `create-lockfiles' is non-nil

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
;;
;; When type os. in python file, pyls will crash.
;; So you need clone python-language-server and path https://github.com/palantir/python-language-server/issues/373
;; Then install patched version with command:
;;
;; sudo pip install .
;;
(add-hook 'python-mode-hook #'lsp-python-enable)

;; Ruby support for lsp-mode using the solargraph gem.
;; Install: gem install solargraph
;; NOTE: and you need put below line in your Gemfile, otherwise lsp-ruby will report tcp error.
;;
;; gem "solargraph"
;;
(add-hook 'ruby-mode-hook #'lsp-ruby-enable)

;; Javascript, Typescript and Flow support for lsp-mode
;; Install: npm i -g javascript-typescript-langserver
(add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
(add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp-javascript-typescript-enable) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp-javascript-typescript-enable) ;; for rjsx-mode support

(defun lsp-company-transformer (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun lsp-js-hook nil
  (make-local-variable 'company-transformers)
  (push 'lsp-company-transformer company-transformers))

(add-hook 'js-mode-hook 'lsp-js-hook)

(provide 'init-lsp)

;;; init-lsp.el ends here
