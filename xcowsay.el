;;; xcowsay.el ---
;;
;; Filename: xcowsay.el
;; Description:
;; Author: Christian Giménez
;; Maintainer:
;; Created: lun jul 11 19:18:23 2011 (-0300)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 2
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
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

;;;###autoload
(defun xcowsay (mensaje)
  "Le indica al demonio de xcowsay que diga algo."
  (interactive "MMensaje?")
  (start-process "*xcowsay-decir*" "*xcowsay-decir*"
                 "dbus-send"
                 "--dest=uk.me.doof.Cowsay"
                 "/uk/me/doof/Cowsay"
                 "uk.me.doof.Cowsay.ShowCow"
                 (format "string:'%s'" mensaje)
                 )
  )


(provide 'xcowsay)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xcowsay.el ends here
