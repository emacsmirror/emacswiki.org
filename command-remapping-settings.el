;;; command-remapping-settings.el - Remap commands in all modes from a custom variable

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This code is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Emacs. If not, see http://www.gnu.org/licenses.

;; To Add more remappings, M-x customize-variable command-remapping-alist.

(defun command-remappings-process-mode-map (keymap remap-alist enable)
  (flet ((maybe-cdr (list) (when enable (cdr list))))
    (when (boundp keymap)
      (mapc #'(lambda (pair)
                (define-key (eval keymap)
                  (vector 'remap (car pair))
                  (maybe-cdr pair)))
            remap-alist))))

(defun command-remappings-setup (alist &optional mode-map enable)
  "Set up command remappings as defined in command-remapping-alist.
With optional arg mode-map, only process specific mode map.
With optional arg enable, enable remappings if positive, disable otherwise."
  (setq enable (or (null enable) (> enable 0)))
  (mapc #'(lambda (mode-remappings)
            (command-remappings-process-mode-map
             (car mode-remappings)
             (cdr mode-remappings)
             enable))
        (if mode-map
            (assoc mode-map alist)
          alist)))

(defun command-remappings-remove (alist &optional mode-map)
  "Unmap all remappings defined in command-remapping-alist.
With optional arg mode-map, only process specific mode map."
  (command-remappings-setup mode-map 0))

(defcustom command-remapping-alist
  nil
  "Alist of mode maps and the command-remappings to do in them."
  :type '(alist :key-type (symbol :tag "Mode-map") :value-type (alist :tag "Remappings" :key-type (symbol :tag "From") :value-type (symbol :tag "To")))
  :set #'(lambda (sym val)
           (when (boundp sym)
             (command-remappings-remove (eval sym)))
           (set-default sym val)
           (command-remappings-setup val)
           )
  )

(command-remappings-setup command-remapping-alist)

;; Don't forget to add command-remappings-setup to mode-specific hooks if needed

(defun add-remapping-setup-to-hook (hook mode-map &optional append local)
  "Add #'(lambda () (command-remappings-setup mode-map)) to hook."
  (add-hook
   hook
   #'(lambda () (command-remappings-setup mode-map))
   append local))
;; Example: (add-remapping-setup-to-hook 'cperl-mode-hook 'cperl-mode-map)

(defun add-remapping-setup-to-mode-hook (mode)
  "Shortcut for add-remapping-setup-to-hook on X-mode if hook is X-mode-hook and map is X-mode-map."
  (add-remapping-setup-to-mode-hook (intern (concat (symbol-name mode) "-hook"))
                                    (intern (concat (symbol-name mode) "-map"))))
;; Example: (add-remapping-setup-to-mode-hook 'cperl-mode)
