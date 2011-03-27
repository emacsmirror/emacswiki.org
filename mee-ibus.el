;;; myibus.el --- 
;; 
;; Filename: myibus.el
;; Description: 
;; Author: Sylvester Y. Shi
;; Maintainer: Sylvester
;; Created: Tue Mar 22 20:38:56 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Tue Mar 22 20:39:40 2011 (+0800)
;;           By: Sylvester Y. Shi
;;     Update #: 1
;; URL: http://blog.meecoder.com/
;; Keywords: emacs, sylvester, ibus
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; Sylvester's configure file about ibus.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 0.1 created.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; *This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(require 'ibus)
;; Turn on ibus-mode automatically after loading .emacs
(add-hook 'after-init-hook 'ibus-mode-on)
;; Use C-SPC for Set Mark command
(ibus-define-common-key ?\C-\s nil)
;; Use C-/ for Undo command
(ibus-define-common-key ?\C-/ nil)
;; Use S-SPC toggle ibus
(ibus-define-common-key ?\S-\s nil)
(global-set-key (kbd "S-SPC") 'ibus-toggle)
;; Use C-x M-i toggle ibus mode.
(global-set-key (kbd "C-x M-i") 'ibus-mode)
;;change cursor color depending on IBus status
(setq ibus-cursor-color '("red" "white" "limegreen"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; myibus.el ends here
