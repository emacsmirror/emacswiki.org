;;; init-ruby.el --- Init for ruby

;; Filename: init-ruby.el
;; Description: Init for ruby
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-03-22 20:52:31
;; Version: 0.1
;; Last-Updated: 2014-03-22 20:52:31
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-ruby.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
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
;; Init for ruby
;;

;;; Installation:
;;
;; Put init-ruby.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-ruby)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-ruby RET
;;

;;; Change log:
;;
;; 2014/03/22
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

(require 'enh-ruby-mode)
(require 'ruby-electric)
(require 'flymake-ruby)
(require 'inf-ruby-extension)
(require 'robe)
(require 'yasnippet)

;;; Code:

(setq robe-highlight-capf-candidates nil) ;avoid robe set color of auto-complete candidate
(setq enh-ruby-check-syntax nil)
(dolist (hook (list
               'enh-ruby-mode-hook
               ))
  (add-hook hook (lambda ()
                   (yas-minor-mode-on)
                   (auto-complete-mode 1)
                   (ruby-electric-mode)
                   (flymake-ruby-load)
                   (robe-mode)
                   (robe-ac-setup)
                   (save-excursion
                     (window-configuration-to-register 'a)
                     (inf-ruby)
                     (robe-start)
                     (jump-to-register 'a)
                     )
                   )))

(add-hook 'inf-ruby-mode-hook
          (lambda ()
            (auto-complete-mode)
            (require 'ac-inf-ruby)
            (ac-inf-ruby-enable)))

(provide 'init-ruby)

;;; init-ruby.el ends here
