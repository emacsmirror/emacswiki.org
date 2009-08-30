;;; vhdl-goto-def.el --- Jumps to definition of signal, constant or function

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author:  wandad guscheh <wandad.guscheh@fh-hagenberg.at>
;; Keywords: vhdl

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

;;; Commentary:

;; Usage: Open any vhdl file and invoke vhdl-goto-type-def with key sequence (\C-c\C-s by default).
;; Cursor will jump to corresponding definition if there is one. Functions searches packages
;; too. If no buffer with package is open, functions asks for location of package.

;; To get back to the start of the search, press \C-x\C-x if corresponding definition has been found
;; in the same file, \C-x b RET if the search has jumped to another buffer.

;; Functions works for signals, constants, types, subtypes, components and subprograms.
;; Works only well for vhdl files with more or less correct syntax. Finds also signals in entity definition.

;; Also have a look at customization possibilities with \M-x customize-group whdl. Change option
;; use-ido-find-file to nil if ido-find-file is not installed on your system.

;; If you have any suggestions or found any bugs please mail me at <wandad.guscheh@fh-hagenberg.at>.

;;; Code:

(defgroup whdl nil "Some customizations of whdl packages" :group 'local)

(defcustom use-ido-find-file t
  "*If t, ido-find-file functions are used for asking for buffers. If nil, standard emacs functions are used."
  :type 'boolean :group 'whdl)

(defcustom allowed-chars-in-signal "a-z0-9A-Z_"
  "*This regexp determins what characters of a signal or constant or function name are allowed.
Needed to determine end of name."
  :type 'string :group 'whdl)

(defcustom allowed-chars-in-signal-plus "a-z0-9A-Z_-"
  "*This regexp determins what characters of a signal or constant or function name are allowed.
Needed to determine end of name."
  :type 'string :group 'whdl)

(defun whdl-get-name (&optional dont-downcase)
"This function extracts word at current position. To determine end of word, allowed-chars-in-signal is used."
  (save-excursion
    (re-search-forward (concat " *[" allowed-chars-in-signal "]*"))
    (backward-char)
    (if (not dont-downcase)
        (downcase (buffer-substring-no-properties (1+ (point)) (+ (re-search-backward (concat "[^"allowed-chars-in-signal "]")) 1)))
      (buffer-substring-no-properties (1+ (point)) (+ (re-search-backward (concat "[^"allowed-chars-in-signal "]")) 1)))))

(defun whdl-get-name-plus (&optional dont-downcase)
"This function extracts word at current position. To determine end of word, allowed-chars-in-signal is used."
  (save-excursion
    (re-search-forward (concat " *[" allowed-chars-in-signal-plus "]*"))
    (backward-char)
    (if (not dont-downcase)
        (downcase (buffer-substring-no-properties (1+ (point)) (+ (re-search-backward (concat "[^"allowed-chars-in-signal "]")) 1)))
      (buffer-substring-no-properties (1+ (point)) (+ (re-search-backward (concat "[^"allowed-chars-in-signal "]")) 1)))))

(defun whdl-package-names ()
"Gets all used packages of a vhdl file. Only use work.NAME.blabla is valid. Returns all NAME"
  (save-excursion
    (let ((packages '()))
      (goto-char (point-min))
      (while (re-search-forward "^ *use  *work\." nil t nil)
        (forward-char)
        (push (whdl-get-name) packages))
      (if (whdl-set-entity-of-arch)
          (while (re-search-forward "^ *use  *work\." nil t nil)
            (forward-char)
            (push (whdl-get-name) packages)))
      packages)))

(defun whdl-set-entity-of-arch ()
  (let ((package-buffer))
    (if (equal (whdl-get-entity-or-package-name) "")
        (if (setq package-buffer (whdl-get-buffer (whdl-get-entity-name-of-architecture)))
            (progn
              (set-buffer package-buffer)
              (goto-char (point-min)))
          (if (setq package-buffer (whdl-ask-for-package (concat (whdl-get-entity-name-of-architecture) " entity file")))
              (progn
                (set-buffer package-buffer)
                (goto-char (point-min))))))
    (if package-buffer t nil)))


(defun whdl-get-buffer (entity-or-package-name)
  "Returns buffer where entity-or-package-name is found. Buffer must exist"
  (save-excursion
    (let ((current-buffer-list (buffer-list)) (counter 0) found)
      (while (and (nth counter current-buffer-list) (not found))
        (set-buffer (nth counter current-buffer-list))
        (if (equal entity-or-package-name (whdl-get-entity-or-package-name))
              (setq found t)
          (setq counter (1+ counter))))
      (if found
          (nth counter current-buffer-list)
        nil))))

(defun whdl-get-entity-or-package-name ()
"Extracts name of a entity or of a package"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^ *\\(entity\\|package\\) +" nil t nil)
        (whdl-get-name-plus)
      "")))

(defun whdl-get-entity-name-of-architecture()
  "Extracts name of architecture if present, returns empty string if nothing found"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\(^\\)\\s-*architecture\\s-+[a-zA-Z0-9_]+\\s-+of\\s-+" nil t nil)
        (whdl-get-name)
      "")))


(defun whdl-ask-for-package (package-name)
  (if use-ido-find-file
      (let ((ido-current-directory (expand-file-name (file-name-directory (buffer-file-name))))
            filename)
        (let (ido-saved-vc-mt
              (vc-master-templates (and (boundp 'vc-master-templates) vc-master-templates))
              (ido-work-directory-index -1)
              (ido-work-file-index -1)
              (ido-find-literal nil))
          (unless filename
            (setq ido-saved-vc-mt vc-master-templates)
            (setq filename (ido-read-internal 'file
                                              (concat "Where is '" package-name "? ")
                                              'ido-file-history nil nil nil))))
        (setq filename (concat ido-current-directory filename))
        (find-file-noselect filename))
    (read-file-name (concat "Where is '" package-name "? "))))


(defun whdl-process-file (name)
"searches a package or a vhdl file for name and tests if it is a type definition or not"
  (let ((found nil) should-be-in-entity beginning-of-entity-port end-of-entity end-of-entity-port apoint (current-pos (point)))
    (save-excursion
      (goto-char (point-min))
      (setq beginning-of-entity-port (re-search-forward (concat "^[ \t]*entity[ \n\t]+[" allowed-chars-in-signal "]+[ \n\t]+is") nil t nil))
      (if beginning-of-entity-port
          (progn
            (setq end-of-entity (save-excursion (re-search-forward "^[ \t]*end")))
            (re-search-forward "port[ \n\t]*(" nil t nil)
            (setq end-of-entity-port (progn (up-list) (point)))
            (goto-char (point-min))
            (setq should-be-in-entity (re-search-forward (concat " +" name "[ \n\t]+") nil t nil))
            (if (and should-be-in-entity (< beginning-of-entity-port should-be-in-entity) (> end-of-entity-port should-be-in-entity)
                     (< (save-excursion (re-search-forward ":" nil t nil)) (save-excursion (re-search-forward "\n" nil t nil)))
                     (< (point) (save-excursion (re-search-forward ":" nil t nil)))
                     (< end-of-entity-port end-of-entity))
                (setq found (point)))))
      (goto-char (point-min))
      (while (and (not found) (re-search-forward "^ *\\(component\\|function\\|procedure\\|constant\\|file\\|type\\|subtype\\)[ \n\t]+" nil t nil))
        (if (equal name (whdl-get-name))
            (setq found (point))))
      (goto-char (point-min))
      (while (and (not found) (re-search-forward "^[ \t]*signal[ \n\t]+" nil t nil))
        (if (equal name (whdl-get-name))
            (setq found (point))
          (while (> (save-excursion (search-forward ":" nil t nil)) (if (setq apoint (save-excursion (search-forward "," nil t nil))) apoint 0))
            (search-forward "," nil t nil)
            (if (equal name (whdl-get-name))
                (setq found (point)))))))
    (if found found nil)))

(defun vhdl-goto-type-def ()
"Main fuction. Reads word at cursor and tries to find a corresponding signal or type definition.
This function first tries to find a signal or type definition in the buffer from where the function have
been called. It can only jump to signal, constant, type and subtype definitions. Works also for signals in
an entity (in and out ports, function will then jump to the entity). To go back to the point where the function
has been called press `\C-x\C-x'.
If there was nothing found, it reads the packages used, and works through all opened buffers to find packages used
in the vhdl file. If a definition has been found in a package, package will be displayed. To go back to original
vhdl file press `\C-x b RET'."
  (interactive)
  (setq current-pos (point))
  (if (not (setq found (whdl-process-file (whdl-get-name))))  ;no definition in calling file found
      (let ((to-search-for (whdl-get-name)) (package-list (whdl-package-names))
            (counter 0) found package-buffer (to-open-packages '()))
        (while (and (not found) (nth counter package-list))
          (setq package-buffer (whdl-get-buffer (nth counter package-list)))
          (if (not package-buffer)
              (setq to-open-packages (append (list (nth counter package-list)) to-open-packages))
            (save-excursion
               (set-buffer package-buffer)
               (setq found (whdl-process-file to-search-for))))
            (setq counter (1+ counter)))
        (setq counter 0)
        (if (not found)
            (save-excursion
              (if (whdl-set-entity-of-arch)
                  (progn
                    (setq found (whdl-process-file to-search-for))
                    (setq package-buffer (current-buffer))))))
        (while (and (not found) (nth counter to-open-packages))
          (if (setq package-buffer (whdl-ask-for-package (nth counter to-open-packages)))
              (save-excursion
                 (set-buffer package-buffer)
                 (setq found (whdl-process-file to-search-for))))
            (setq counter (1+ counter)))
        (if found
            (progn
              (switch-to-buffer package-buffer)
              (goto-char found)
              (recenter))
          (message "sorry, no corresponding definition found")))
    (progn
      (push-mark current-pos t nil)
      (goto-char found)
      (beginning-of-line)
      (recenter))))

(provide 'vhdl-goto-def)
;;; vhdl-goto-def.el ends here
