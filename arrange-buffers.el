;;; arrange-buffers.el -- display all buffers which name and mode match a regexp 
;; 
;; Last update: 11-02-22
;; Compatibility: Tested with GNU Emacs 24.0.50.1
;; Written by Zarza <zarza.el at googlemail.com>
;; Feel free to modify
;; 
;; -------------------------------------------------------------------
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;; -------------------------------------------------------------------
;;
;;; Commentary:
;;
;; (arrange-buffers MATCH-NAME MATCH-MODE)
;; Displays all buffers whose name regexp-match MATCH-NAME
;;                *and* whose mode regexp-match MATCH-MODE.
;;
;;
;;; Installation:
;;
;; Put this file in your load path and
;; (require 'arrange-buffers)
;;
;;
;;; Examples:
;;
;; Show all open buffers in (any) lisp mode
;; (arrange-buffers ".*" "lisp")
;;
;; Show all open org-buffers
;; (arrange-buffers ".*" "org-mode")
;;
;; Show all open rcirc channels:
;; (arrange-buffers "^#" "rcirc-mode")
;; Show all open rcirc queries:
;; (arrange-buffers "^[^#&\\*]" "rcirc-mode")
;;

;;; Code:

(defun match-buffers ( MATCH-NAME MATCH-MODE )
  "Returns a list of buffers regexp-matching MATCH-NAME and MATCH-MODE."
  (let ((buffers (buffer-list))
        (matched-buffers (list)))
    ; get matching buffers
    (dolist (buffer buffers matched-buffers)
      (with-current-buffer buffer
        (if (and (string-match MATCH-MODE (symbol-name major-mode))
                 (string-match MATCH-NAME (buffer-name buffer))
                 )
            (setq matched-buffers (cons buffer matched-buffers))
          )))
  ))

(defun buffer-sorter ( A B )
  (string< (buffer-name A) (buffer-name B))
)

(defun arrange-buffers ( MATCH-NAME MATCH-MODE )
  "Displays all buffers whose name regexp-match MATCH-NAME *and* whose mode regexp-match MATCH-MODE."
  (interactive "sMatch buffer names: \nsMatch buffer modes: ")

  (let* ((buffers (sort (match-buffers MATCH-NAME MATCH-MODE) 'buffer-sorter))
         (rows 1)
         (cols 1)
         )

    (if (= 0 (length buffers))
        (message "No matching buffers.")

      ; calculate number of rows/columns
      (setq rows (floor (sqrt (length buffers))))
      (setq cols (ceiling (/ (length buffers) (float rows))))
      (message "%d rows, %d cols for %d buffers." rows cols (length buffers))

      ; split windows...
      (delete-other-windows)

      ; create rows
      (if (> rows 1)
          (dotimes (i (- rows 1))
            (split-window-vertically)
            (balance-windows)
            ))
      ; create cols
      (if (> cols 1)
          (dotimes (j rows)
            (other-window -1)
            (dotimes (i (- cols 1))
              (split-window-horizontally)
              (balance-windows)
              )))
      
      ; display buffers...
      (dolist (buffer buffers)
        (with-current-buffer buffer
          (set-window-buffer nil buffer)
          (other-window 1)
          ))

      ; remove empty windows (if cols•rows > length-of-buffers)
      (if (> (* cols rows) (length buffers) )
          (dotimes (i (- (* cols rows) (length buffers)))
            (delete-window)
            ))
      (balance-windows)

      )
  ))

(provide 'arrange-buffers)

;; arrange-buffers.el ends here
