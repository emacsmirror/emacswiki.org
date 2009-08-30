;;; -*- Emacs-Lisp -*-
;;; profiles.el --- profile management system
;;; Version 0.3 - 2009-04-05
;;;
;;; Author: Sylvain Bougerel <sylvain.bougerel.devel@gmail.com>
;;;
;;; This file is NOT part of GNU Emacs.  You may however redistribute it and/or
;;; modify it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option) any
;;; later version.
;;;
;;; profiles.el is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public License along with
;;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;;; Place - Suite 330, Boston, MA 02111-1307, USA.
;;;
;;; Copyright (C) 2009 Sylvain Bougerel

;;; ----------------------------------------------------------------------------
;;; Installation:
;;;
;;; Copy this file under one of the directories that is in your `load-path'.

;;; ----------------------------------------------------------------------------
;;; Change log:
;;;
;;; version 0.3 - 2009-04-05 Added the customisation variables, and defined
;;; `profile-define' to replace `profile-create' and `profile-replace'.
;;;
;;; version 0.2 - 2009-04-04 `Profiles' is set immediately after the creation of
;;;    the buffer. Previously it was set only after a few step and that caused
;;;    certain packages not to work with `Profiles'.
;;;
;;; version 0.1 - 2009-03-31 - First version

;;; ----------------------------------------------------------------------------
;;; Todo:
;;;
;;;
;;; Create function profile-define with both profile-create and profile-replace
;;; functionalities.

;;; ----------------------------------------------------------------------------
;;; Description:
;;;
;;; Profiles manages user-defined sets of generic properties such as user name
;;; or mailing address across various buffers. Profiles are aimed towards
;;; developers that work on different projects, sometimes under different names,
;;; different mailing address, and that wish to apply similar settings across
;;; different files.
;;;
;;; `Profiles' provides a higher level functionality that `dirvars.el', albeit
;;; less invasive. Where `dirvars.el' requires you to create a file in each of
;;; your project files, profiles try to take adventage of the fact that each
;;; particular project's setting are probably similar in some way or require
;;; more than just setting user variables. In any case, `profiles' can be used
;;; together with `dirvars.el'.
;;;
;;; For exemple, user Joe is working on project ProjA and ProjB. Both projects
;;; imposes that coding style follows "gnu" standards, but Joe generally prefer
;;; to work with "elemtel" and this is what he is using for all his other
;;; projects. He is also using a different mailing address for both projects,
;;; and wants it to be set automatically for his ChangeLog.Fortunatly, Joe uses
;;; `profiles' and he has configured his "~/.emacs" in this way:
;;;
;;;     (require 'profiles)
;;;     (profile-define "usual" "Joe" "joeuser@usual.com"
;;;                     'coding-style "elemtel"
;;;                     ; any property value can be attached!
;;;                     'prefered-license "GPLv2")
;;;     (profile-define "proj-a-and-b" "Joe" "joeuser@proj-a-and-b.com"
;;;                     'coding-style "gnu"
;;;                     'prefered-license nil)
;;;     (setq profile-path-alist
;;;           '(("/path/to/proj/a" . "proj-a-and-b")
;;;             ("/path/to/proj/b" . "proj-a-and-b")))
;;;     (profile-set-default "usual")
;;;
;;;     ; ...and later in his config file...
;;;     (add-hook 'c-mode-common-hook
;;;               '(lambda ()
;;;                   (c-set-style (profile-current-get 'coding-style))))
;;;
;;;     ; ...and a little later too...
;;;     (add-hook 'change-log-mode-hook
;;;	          '(lambda ()
;;;                   (setq add-log-full-name
;;;                         (profile-current-name))
;;;                   (setq add-log-mailing-address
;;;                         (profile-current-mailing-address))))
;;;
;;; And voila! His name, mailing address and coding style are now
;;; automatically set for ProjA and ProjB to the desired values. This did not
;;; require Joe to make any amendement to the project themselves. Adding new
;;; profiles will be easy too.
;;;
;;; Saving the profile path in your "~/.emacs" is not such a good idea however.
;;; Instead you can use the interactive function `profile-add-regexp-and-save'
;;; that will populate `profile-path-alist' with a new entry. This setting will
;;; be saved in the file `profile-path-alist-file'.
;;;
;;; You can automatically load the settings of this file by adding:
;;;
;;;     (profile-load-path-alist)
;;;
;;; ...in your "~/.emacs". This file is printed in human readable lisp and can
;;; be edited manually, as necessary.

;;; ----------------------------------------------------------------------------
;;; Enjoy!

(require 'custom)

;;; ----------------------------------------------------------------------------
;;; Custom variables

(defgroup profiles nil
  "*Profiles."
  :group 'profiles)

(defcustom profile-path-alist-file "~/.emacs.profiles"
  "*The file that will contain the `profile-path-alist'."
  :group 'profiles
  :type 'file)

(defcustom profile-path-alist nil
  "*An alist of pairs formed by (REGEXP . PROFILE-NAME). Both are
strings. If the path of the file in the current buffer matches
REGEXP, then the corresponding profile is used. If there are no
matches, the default value of `profile-current' is used
instead."
  :group 'profiles
  :type 'alist)

;;; ----------------------------------------------------------------------------
;;; Variables

(defvar profile-obarray
  (let ((intern-obarray (make-vector 7 0)))
    (intern "default" intern-obarray)
    intern-obarray)
  "The alist of available profiles. The first member of the cons
is the profile name and the second member is the profile
parameters.")

(defvar profile-current (intern-soft "default" profile-obarray)
  "Buffer local variable that contains the current profile. Add
more profiles using `profile-create' in your \"~/.emacs\" or
interactively.")

;;; ----------------------------------------------------------------------------
;;; Functions

;;;###autoload
(defun profilep (arg)
  "Return t if arg is a profile, nil otherwise."
  (if (intern-soft arg profile-obarray) t nil))

;;;###autoload
(defun profile-set-current (profile-name)
  "Make PROFILE-NAME the current profile. The current profile is
a buffer local variable"
  (unless (intern-soft profile-name profile-obarray)
    (error "Profile " profile-name " does not exists"))
  (setq profile-current (intern-soft profile-name profile-obarray)))

;;;###autoload
(defun profile-set-default (profile-name)
  "Make PROFILE-NAME the default profile. The profile must exists."
  (interactive "i")
  (if (interactive-p)
      (setq profile-name
            (completing-read "Set default profile: "
                             profile-obarray nil t
                             (symbol-name profile-current)))
    (unless (intern-soft profile-name profile-obarray)
      (error "Profile " profile-name " does not exists")))
  (set-default 'profile-current (intern-soft profile-name profile-obarray)))

;;;###autoload
(defun profile-define (profile &optional name mail &rest plist)
  "Create or replace PROFILE with NAME and MAIL. PROFILE, NAME
and MAIL are all required to be string values. Optional argument
PLIST is a property list."
  (interactive "i")
  (if (interactive-p)
      (progn
        (setq profile (read-string "Name of the profile: "))
        (setq name (read-string "Your name: "))
        (setq mail (read-string "Your mailing address: ")))
    ; check that params given in program are ok
    (unless (stringp name) (error "Parameter profile is not a string"))
    (unless (string-or-null-p name) (error "Parameter name is not a string"))
    (unless (string-or-null-p mail) (error "Parameter mail is not a string")))
  (setplist (intern profile profile-obarray)
            (append (list 'name name 'mailing-address mail) plist)))

;;;###autoload
(defun profile-put (profile property value)
  "Put PROPERTY's VALUE into PROFILE or replace any existing
value."
  (unless (stringp profile) (error "Parameter profile is not a string"))
  (unless (intern-soft profile profile-obarray)
    (error "Profile " profile " does not exists"))
  (put (intern-soft profile profile-obarray) property value))

;;;###autoload
(defun profile-get (profile property)
  "Get PROPERTY's VALUE from `profile-current'."
  (unless (stringp profile) (error "Parameter profile is not a string"))
  (unless (intern-soft profile profile-obarray)
    (error "Profile " profile " does not exists"))
  (get (intern-soft profile profile-obarray) property))

;;;###autoload
(defun profile-current-put (property value)
  "Put PROPERTY's VALUE into the `profile-current' or replace any
  existing value."
  (put profile-current property value))

;;;###autoload
(defun profile-current-name nil
  "Return the name for `profile-current'."
  (get profile-current 'name))

;;;###autoload
(defun profile-current-mailing-address nil
  "Return the mailing address for `profile-current'."
  (get profile-current 'mailing-address))

;;;###autoload
(defun profile-current-get (property)
  "Return the value of PROPERTY for the current profile
`profile-current'. The returned property is not evaluated."
  (get profile-current property))

;;;###autoload
(defun profile-current-eval (property)
  "Return the evaluation of PROPERTY's value for the current
profile `profile-current'."
  (eval (get profile-current property)))

;;;###autoload
(defun profile-load-path-alist nil
  "Load the path alist from `profile-path-alist-file'."
  (interactive)
  (when (file-readable-p profile-path-alist-file)
    (load-file profile-path-alist-file)))

;;;###autoload
(defun profile-save-path-alist nil
  "Save the path alist to `profile-path-alist-file'."
  (interactive)
  (when (file-writable-p profile-path-alist-file)
    (with-temp-file profile-path-alist-file
      (insert "(setq profile-path-alist (quote")
      (print profile-path-alist (current-buffer))
      (insert "))"))))

(defun profile-find-path-alist (&optional filename)
  "Scans `profile-path-alist' to find the matching profile for
the FILENAME or return the default value for `profile-current'.
If FILENAME is nil, then match against the buffer's current file
name, or the buffer's name."
  (or
   (assoc-default (or filename (buffer-file-name) (buffer-name))
                  profile-path-alist 'string-match)
   (symbol-name (default-value 'profile-current))))

;;;###autoload
(defun profile-add-regexp-and-save (regexp profile-name)
  "Add an element to the `profile-path-alist'. Request a REGEXP
and a specific PROFILE-NAME from the user if called
interactively."
  (interactive "i\ni") ; ignore both arguments, we'll set them later
  (if (interactive-p)
      (progn
        (setq regexp (read-file-name "Path regexp: "
                                     (buffer-file-name)))
        (setq profile-name
              (completing-read "Apply profile: "
                               profile-obarray nil t
                               (symbol-name profile-current))))
    (unless (intern-soft profile-name profile-obarray)
      (error "Profile " profile-name " does not exists")))
  (setq profile-path-alist
        (cons (cons regexp profile-name) profile-path-alist))
  (profile-save-path-alist))

;;;###autoload
(defun profile-find-file (filename profile-name)
  "Modify `profile-path-alist', call `profile-save-path-alist' to
remember the modification and open a file using `find-file'. This
function allows a user the open the file FILENAME with a
different profile PROFILE-NAME than the default profile and
remember this setting."
  (interactive "GFind file: \ni")
  (if (interactive-p)
      (setq profile-name
            (completing-read "Apply profile: "
                             profile-obarray nil t
                             (symbol-name profile-current)))
    (unless (intern-soft profile-name profile-obarray)
      (error "Profile " profile-name " does not exists")))
  (setq profile-path-alist
        (cons (cons filename profile-name) profile-path-alist))
  (profile-save-path-alist)
  (find-file filename))

;;; ---------------------------------------------------------------------
;;; Initialization

(defadvice find-file-noselect-1
  (before before-find-file-noselect-1 activate)
  "Set the buffer local variable `profile-current' right after
the creation of the buffer."
  (with-current-buffer buf
    (make-local-variable 'profile-current)
    (put 'profile-current 'permanent-local t)
    (setq profile-current
          (intern-soft (profile-find-path-alist filename) profile-obarray))))

;;; ---------------------------------------------------------------------
;;; Exposition

(provide 'profiles)