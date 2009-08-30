;;; apt-utils.el --- Emacs interface to APT (Debian package management)

;;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;;	$Id: apt-utils.el,v 1.212 2007-10-06 07:55:08 mphodges-guest Exp $

;; apt-utils.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; apt-utils.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; Package to interface Emacs with APT.  Start things off using e.g.:
;; M-x apt-utils-show-package RET emacs21 RET
;;
;; Other packages (dependencies, conflicts etc.) can be navigated
;; using apt-utils-{next,previous}-package,
;; apt-utils-choose-package-link or apt-utils-follow-link.  Return to
;; the previous package with apt-utils-view-previous-package.
;; ChangeLog and README files for the current package can easily be
;; accessed with, for example, apt-utils-view-changelog.
;;
;; For normal (i.e., not virtual) packages, the information can be
;; toggled between `package' and `showpkg' displays using
;; apt-utils-toggle-package-info; the latter is useful for the
;; "Reverse Depends".
;;
;; View the key bindings with describe-mode (bound to ? by default).

;;; Code:

(defconst apt-utils-version "2.10.0"
  "Version number of this package.")

(require 'browse-url)
(require 'jka-compr)

(cond
 ((fboundp 'puthash)
  (defalias 'apt-utils-puthash 'puthash))
 ((and (require 'cl)
       (fboundp 'cl-puthash))
  (defalias 'apt-utils-puthash 'cl-puthash))
 (t
  (error "No puthash function known")))

;; Customizable variables

(defgroup apt-utils nil
  "Emacs interface to APT (Debian package management)."
  :group 'tools
  :link '(url-link "http://mph-emacs-pkgs.alioth.debian.org/AptUtilsEl.html"))

(defcustom apt-utils-fill-packages t
  "*Fill APT package names if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-show-link-info t
  "*Show APT package descriptions when cycling through links if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-show-all-versions nil
  "*Show APT descriptions for multiple package versions if t."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-automatic-update 'ask
  "*Controls automatic rebuilding of APT package lists.

If t always rebuilt when `apt-utils-timestamped-file' is newer
than the timestamp stored in `apt-utils-package-list-built'.  If
equal to the symbol ask, ask the user about the update.  If nil,
never update automatically."
  :group 'apt-utils
  :type '(choice (const :tag "Always update automatically" t)
                 (const :tag "Ask user about update" ask)
                 (const :tag "Never update automatically" nil)))

(defcustom apt-utils-grep-dctrl-args '("-e")
  "*List of arguments to pass to `apt-utils-grep-dctrl-program'."
  :group 'apt-utils
  :type '(repeat string))

(defcustom apt-utils-kill-buffer-confirmation-function 'yes-or-no-p
  "Function called before killing any buffers.
The function is called with one argument, which is a prompt.
Suitable non-nil values include `yes-or-no-p', `y-or-n-p' and
`ignore'."
  :group 'apt-utils
  :type '(choice (const :tag "Kill buffers only after yes or no query" yes-or-no-p)
                 (const :tag "Kill buffers only after y or n query" y-or-n-p)
                 (const :tag "Never kill buffers" ignore)
                 (const :tag "Kill buffers without confirmation" nil)))

(defcustom apt-utils-search-split-regexp "\\s-*&&\\s-*"
  "Regular expression used to split multiple search terms.
See `apt-utils-search' and `apt-utils-search-names-only'."
  :group 'apt-utils
  :type 'regexp)

(defcustom apt-utils-web-browse-debian-changelog-url
  "http://packages.debian.org/changelogs/pool/main/%d/%s/%s_%v/changelog"
  "Template URL for Debian ChangeLog files.
See `apt-utils-web-format-url'."
  :group 'apt-utils
  :type 'string)

(defcustom apt-utils-web-browse-bug-reports-url
  "http://bugs.debian.org/%p"
  "Template URL for Debian bug reports.
See `apt-utils-web-format-url'."
  :group 'apt-utils
  :type 'string)

(defcustom apt-utils-web-browse-copyright-url
  "http://packages.debian.org/changelogs/pool/main/%d/%s/%s_%v/%p.copyright"
  "Template URL for Debian copyright files.
See `apt-utils-web-format-url'."
  :group 'apt-utils
  :type 'string)

(defcustom apt-utils-web-browse-versions-url
  "http://packages.debian.org/%p"
  "Template URL for Debian version information.
See `apt-utils-web-format-url'."
  :group 'apt-utils
  :type 'string)

(defcustom apt-utils-show-package-hooks nil
  "Hooks to be run after presenting package information."
  :group 'apt-utils
  :type 'hook)

(defcustom apt-utils-use-current-window nil
  "If non-nil always display APT utils buffers in the current window.
In this case `switch-to-buffer' is used to select the APT utils
buffer.  If nil, `display-buffer' is used, and the precise
behaviour depends on the value of `pop-up-windows'."
  :group 'apt-utils
  :type 'boolean)

(defcustom apt-utils-dpkg-program "/usr/bin/dpkg"
  "Location of the dpkg program.
This can be set to dlocate, which has the advantage of better
performance, but uses cached data that may be out of date."
  :group 'apt-utils
  :type '(choice (const :tag "dpkg" "/usr/bin/dpkg")
                 (const : tag "dlocate" "/usr/bin/dlocate")
                 (file :must-match t)))

(defcustom apt-utils-display-installed-status t
  "If non-nil display the installed status of the current package."
  :group 'apt-utils
  :type 'boolean)

;; Faces

(defface apt-utils-normal-package-face
  '((((class color) (background light))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "yellow")))
  "Face used for APT normal package hyperlinks."
  :group 'apt-utils)

(defface apt-utils-virtual-package-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green")))
  "Face used for APT virtual package hyperlinks."
  :group 'apt-utils)

(defface apt-utils-field-keyword-face
  '((((class color) (background light))
     (:foreground "purple" :bold t))
    (((class color) (background dark))
     (:foreground "purple" :bold t)))
  "Face used for APT field keywords."
  :group 'apt-utils)

(defface apt-utils-field-contents-face
  '((((class color) (background light))
     (:foreground "orchid"))
    (((class color) (background dark))
     (:foreground "orange")))
  "Face used for APT field contents."
  :group 'apt-utils)

(defface apt-utils-description-face
  '((((class color))
     (:foreground "cadet blue")))
  "Face used for APT package description."
  :group 'apt-utils)

(defface apt-utils-version-face
  '((((class color))
     (:italic t)))
  "Face used for APT package versions."
  :group 'apt-utils)

(defface apt-utils-broken-face
  '((((class color))
     (:foreground "red")))
  "Face used for unknown APT package."
  :group 'apt-utils)

(defface apt-utils-file-face
  '((((class color))
     (:foreground "brown")))
  "Face used for files."
  :group 'apt-utils)

(defface apt-utils-installed-status-face
  '((((class color))
     (:italic t)))
  "Face used for installed status."
  :group 'apt-utils)

;; Other variables

(defvar apt-utils-apt-cache-program "/usr/bin/apt-cache"
  "Location of the apt-cache program.")

(defvar apt-utils-grep-dctrl-program "/usr/bin/grep-dctrl"
  "Location of the grep-dctrl program.")

(defvar apt-utils-grep-dctrl-file-directory "/var/lib/apt/lists"
  "Directory used by `apt-utils-search-grep-dctrl'.
See also `apt-utils-grep-dctrl-file-list'.")

(defvar apt-utils-grep-dctrl-file-list nil
  "List of files searched by `apt-utils-search-grep-dctrl'.
If no list is specified, this is computed on demand from files in
`apt-utils-grep-dctrl-file-directory'.")

(defvar apt-utils-package-list nil
  "Hash table containing APT packages types.")

(defvar apt-utils-package-list-built nil
  "If non-nil, a timestamp for the APT package list data.")

(defvar apt-utils-package-history nil
  "History of packages for each `apt-utils-mode' buffer.")
(make-variable-buffer-local 'apt-utils-package-history)

(defvar apt-utils-current-links nil
  "Package links associated with the `apt-utils-mode' buffer.")
(make-variable-buffer-local 'apt-utils-current-links)

(defvar apt-utils-buffer-positions nil
  "Cache of positions associated with package history.
These are stored in a hash table.  See also
`apt-utils-package-history'")
(make-variable-buffer-local 'apt-utils-buffer-positions)

(defvar apt-utils-dired-buffer nil
  "Keep track of dired buffer.")

(defvar apt-utils-automatic-update-asked nil
  "Non-nil if user already asked about updating package lists.")

(defvar apt-utils-timestamped-file "/var/cache/apt/pkgcache.bin"
  "File to check timestamp of (see `apt-utils-automatic-update').")

;; XEmacs support

(defconst apt-utils-xemacs-p
  (or (featurep 'xemacs)
      (string-match "XEmacs\\|Lucid" (emacs-version)))
  "True if we are using apt-utils under XEmacs.")

;; Other version-dependent configuration

(defalias 'apt-utils-line-end-position
  (cond
   ((fboundp 'line-end-position) 'line-end-position)
   ((fboundp 'point-at-eol) 'point-at-eol)))

(defalias 'apt-utils-line-beginning-position
  (cond
   ((fboundp 'line-beginning-position) 'line-beginning-position)
   ((fboundp 'point-at-bol) 'point-at-bol)))

(defconst apt-utils-completing-read-hashtable-p
  ;; I think this is a valid way to check this feature...
  (condition-case nil
      (or (all-completions "" (make-hash-table)) t)
    (error nil))
  "Non-nil if `completing-read' supports hash table as input.")

(defconst apt-utils-face-property
  (if (with-temp-buffer
        ;; We have to rename to something without a leading space,
        ;; otherwise font-lock-mode won't get activated.
        (rename-buffer "*test-font-lock*")
        (font-lock-mode 1)
        (and (boundp 'char-property-alias-alist)
             (member 'font-lock-face
                     (assoc 'face char-property-alias-alist))))
      'font-lock-face
    'face)
  "Use font-lock-face if `add-text-properties' supports it.
Otherwise, just use face.")

(cond
 ;; Emacs 21
 ((fboundp 'replace-regexp-in-string)
  (defalias 'apt-utils-replace-regexp-in-string 'replace-regexp-in-string))
 ;; Emacs 20
 ((and (require 'dired)
       (fboundp 'dired-replace-in-string))
  (defalias 'apt-utils-replace-regexp-in-string 'dired-replace-in-string))
 ;; XEmacs
 ((fboundp 'replace-in-string)
  (defun apt-utils-replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))
 ;; Bail out
 (t
  (error "No replace in string function found")))

;; Commands and functions

;;;###autoload
(defun apt-utils-show-package (&optional new-session)
  "Show information for a Debian package.
A selection of known packages is presented.  See `apt-utils-mode'
for more detailed help.  If NEW-SESSION is non-nil, generate a
new `apt-utils-mode' buffer."
  (interactive "P")
  (let ((package (apt-utils-choose-package)))
    (when (> (length package) 0)
      (apt-utils-show-package-1 package t new-session))))

(defun apt-utils-show-package-1 (package-spec &optional interactive new-session)
  "Present Debian package information in a dedicated buffer.

PACKAGE-SPEC can be either a string (the name of the package) or
a list, where the car of the list is the name of the package, and
the cdr is the package type.

If INTERACTIVE is non-nil, then we have been called
interactively (or from a keyboard macro) via
`apt-utils-show-package'.  Hence, reset the history of visited
packages.

If NEW-SESSION is non-nil, generate a new `apt-utils-mode'
buffer."
  (apt-utils-check-package-lists)
  (let (package type)
    (cond ((and package-spec (listp package-spec))
           (setq package (car package-spec))
           (setq type (cdr package-spec)))
          ((stringp package-spec)
           (setq package package-spec
                 type (apt-utils-package-type package))))
    ;; Set up the buffer
    (cond
     (new-session
      (set-buffer (generate-new-buffer "*APT package info*"))
      (apt-utils-mode)
      (apt-utils-update-mode-name))
     ((eq major-mode 'apt-utils-mode)
      ;; do nothing
      )
     (t
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)))
    ;; If called interactively, initialize apt-utils-package-history
    (when (or interactive new-session)
      (setq apt-utils-package-history (cons (cons package type) nil))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal))))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (cond
       ((equal type 'normal)
        (call-process apt-utils-apt-cache-program nil '(t nil) nil "show" package)
        ;; Remove old versions if not wanted
        (unless apt-utils-show-all-versions
          (goto-char (point-min))
          (re-search-forward "^$")
          (unless (eobp)
            (delete-region (point) (point-max))))
        (apt-utils-add-package-links))
       ;; Virtual package or normal package w/ showpkg
       ((memq type '(virtual normal-showpkg))
        (call-process apt-utils-apt-cache-program nil '(t nil) nil "showpkg" package)
        (apt-utils-add-showpkg-links package))
       ;; Normal search
       ((equal type 'search)
        (insert (format "Debian package search for %s\n\n" package))
        (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
               "search" "--"
               (split-string package apt-utils-search-split-regexp))
        (apt-utils-add-search-links 'search))
       ;; Search for names only
       ((equal type 'search-names-only)
        (insert (format "Debian package search (names only) for %s\n\n" package))
        (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
               "search" "--names-only" "--"
               (split-string package apt-utils-search-split-regexp))
        (apt-utils-add-search-links 'search-names-only))
       ;; Search for file names
       ((equal type 'search-file-names)
        (insert (format "Debian package search (file names) for %s\n\n" package))
        (apply 'call-process apt-utils-dpkg-program nil t nil
               "-S" (list package))
        (apt-utils-add-search-links 'search-file-names))
       ;; grep-dctrl search
       ((equal type 'search-grep-dctrl)
        (insert (format "grep-dctrl search for %s\n\n"
                        (concat (format "\"%s\" " (car package))
                                (mapconcat 'identity (cdr package) " "))))
        (apply 'call-process apt-utils-grep-dctrl-program nil t nil package)
        (apt-utils-add-package-links)))
      (if apt-utils-use-current-window
          (switch-to-buffer (current-buffer))
	(select-window (display-buffer (current-buffer))))
      ;; Point only needs setting for new sessions or when choosing
      ;; new packages with apt-utils-follow-link or
      ;; apt-utils-choose-package-link.
      (goto-char (point-min))
      (run-hooks 'apt-utils-show-package-hooks)))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun apt-utils-list-package-files ()
  "List the files associated with the current package.
The list appears in a `dired-mode' buffer.  Only works for
installed packages; uses `apt-utils-dpkg-program'."
  (interactive)
  (let ((package (caar apt-utils-package-history))
        (type (cdar apt-utils-package-history))
        files)
    (setq files (apt-utils-get-package-files package))
    ;; Some meta packages contain only directories, so
    ;; apt-utils-get-package-files returns '("/."); however, we don't
    ;; want to list /.
    (when (equal files '("/."))
      (setq files nil))
    (cond
     ((memq type '(normal normal-showpkg))
      (if files
          (progn
            ;; Some versions of Emacs won't update dired for the same
            ;; directory name if it already exists
            (if (buffer-live-p apt-utils-dired-buffer)
                (kill-buffer apt-utils-dired-buffer))
            (setq apt-utils-dired-buffer (dired-noselect files))
            (display-buffer apt-utils-dired-buffer))
        (message "Package does not contain any files/is not installed.")))
     (t
      (message "No files associated for type: %s." type)))))

(defalias 'apt-utils-view-package-files 'apt-utils-list-package-files)

(defun apt-utils-get-package-files (package &optional filter installed)
  "Return a list of files belonging to package PACKAGE.
With optional argument FILTER, return files matching this regular
expression.

With non-nil INSTALLED, return t if package is installed,
otherwise nil."
  (let (files)
    (catch 'installed
      (with-temp-buffer
        (call-process apt-utils-dpkg-program nil t nil "-L" package)
        ;; Check for files
        (cond
         ((or (search-backward "does not contain any files" nil t)
              (search-backward "not installed" nil t)
              ;; dlocate returns nothing for uninstalled packages
              (or (zerop (buffer-size))))
          (when installed
            (throw 'installed nil)))
         (installed
          (throw 'installed t))
         (t
          (setq files (split-string (buffer-string) "\n"))
          ;; Keep regular files or top directory (for dired)
          (setq files
                (delq nil
                      (mapcar (lambda (elt)
                                (if (and (or (file-regular-p elt)
                                             (string-equal "/." elt))
                                         (string-match (or filter ".") elt))
                                    elt
                                  nil))
                              files))))))
    files)))

(defun apt-utils-current-package-installed-p ()
  "Return non-nil if the current-package is installed."
  (apt-utils-get-package-files (caar apt-utils-package-history) nil t))

;;;###autoload
(defun apt-utils-search ()
  "Search Debian packages for regular expression.
To search for multiple patterns use a string like \"foo && bar\".
The regular expression used to split the
terms (`apt-utils-search-split-regexp') is customisable."
  (interactive)
  (apt-utils-search-internal 'search
                             "Search packages for regexp: "))

(defun apt-utils-search-names-only ()
  "Search Debian package names for regular expression.
To search for multiple patterns use a string like \"foo && bar\".
The regular expression used to split the
terms (`apt-utils-search-split-regexp') is customisable."
  (interactive)
  (apt-utils-search-internal 'search-names-only
                             "Search package names for regexp: "))

(defun apt-utils-search-file-names ()
  "Search Debian file names for string."
  (interactive)
  (apt-utils-search-internal 'search-file-names
                             "Search file names for string: "))

(defun apt-utils-search-internal (type prompt)
  "Search Debian packages for regular expression or string.
The type of search is specified by TYPE, the prompt for the
search is specified by PROMPT."
  (apt-utils-check-package-lists)
  (let ((regexp (read-from-minibuffer prompt)))
    ;; Set up the buffer
    (cond
     ((eq major-mode 'apt-utils-mode)
      ;; do nothing
      )
     (t
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)))
    (let ((inhibit-read-only t)
          result)
      (erase-buffer)
      ;; Can't search for string starting with "-" because the "--"
      ;; option isn't understood by dpkg or dlocate
      (when (and (eq type 'search-file-names)
                 (string-match "^-" regexp))
        (setq regexp (apt-utils-replace-regexp-in-string "^-+" "" regexp)))
      (insert (format "Debian package search%s for %s\n\n"
                      (cond ((eq type 'search-names-only) " (names only)")
                            ((eq type 'search-file-names) " (file names)")
                            (t ""))
                      regexp))
      (setq result
            (cond
             ((eq type 'search)
              (setq apt-utils-package-history (cons (cons regexp 'search) nil))
              (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
                     "search" "--"
                     (split-string regexp apt-utils-search-split-regexp)))
             ((eq type 'search-names-only)
              (setq apt-utils-package-history (cons (cons regexp 'search-names-only) nil))
              (apply 'call-process apt-utils-apt-cache-program nil '(t nil) nil
                     "search" "--names-only" "--"
                     (split-string regexp apt-utils-search-split-regexp)))

             ((eq type 'search-file-names)
              (setq apt-utils-package-history (cons (cons regexp 'search-file-names) nil))
              (apply 'call-process apt-utils-dpkg-program nil t nil
                     "-S" (list regexp)))))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal)))
      (if (eq result 0)
          (apt-utils-add-search-links type)
        (if (hash-table-p apt-utils-current-links)
            (clrhash apt-utils-current-links)))
      (goto-char (point-min))
      ;; Sort results
      (save-excursion
        (forward-line 2)
        (sort-lines nil (point) (point-max)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer (current-buffer)))))

(defun apt-utils-search-grep-dctrl ()
  "Search Debian packages for regular expression using grep-dctrl."
  (interactive)
  (apt-utils-check-package-lists)
  (let (args
        (fields (apt-utils-read-fields "Search package fields: "))
        (show (apt-utils-read-fields "Show package fields: "))
        (regexp (read-from-minibuffer "Search regexp: ")))
    ;; Check args
    (cond
     ((equal (length fields) 0)
      (error "No fields selected for search"))
     ((equal (length show) 0)
      (error "No fields selected for show"))
     ((equal (length regexp) 0)
      (error "No regexp selected")))
    (setq fields (concat "-F" fields))
    (setq show (concat "-s" show))
    (cond
     ((eq major-mode 'apt-utils-mode)
      ;; do nothing
      )
     (t
      (set-buffer (get-buffer-create "*APT package info*"))
      (apt-utils-mode)))
    (let ((inhibit-read-only t)
          result)
      (erase-buffer)
      ;; Construct argument list (need to keep this)
      (setq args (append (list regexp fields show) apt-utils-grep-dctrl-args
                         (or apt-utils-grep-dctrl-file-list
                             (directory-files apt-utils-grep-dctrl-file-directory
                                              t "_Packages"))))
      (insert (format "grep-dctrl search for %s\n\n"
                      (mapconcat
                       (lambda (elt)
                         (if (string-equal regexp elt)
                             (format "\"%s\"" regexp)
                           elt))
                       args " ")))
      (setq result
            (apply 'call-process
                   apt-utils-grep-dctrl-program nil t nil args))
      (setq apt-utils-package-history (cons (cons args 'search-grep-dctrl) nil))
      (if (hash-table-p apt-utils-buffer-positions)
          (clrhash apt-utils-buffer-positions)
        (setq apt-utils-buffer-positions (make-hash-table :test 'equal)))
      (if (eq result 0)
          (apt-utils-add-package-links)
        (if (hash-table-p apt-utils-current-links)
            (clrhash apt-utils-current-links)))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer (current-buffer)))))

(defun apt-utils-read-fields (prompt)
  "Read fields for `apt-utils-search-grep-dctrl'.
Use PROMPT for `completing-read'."
  (let ((chosen "foo")
        (completion-ignore-case t)
        ;; Why can't I use '(...) for the list?
        (keywords (list "Architecture" "Bugs" "Conffiles" "Conflicts"
                        "Depends" "Description" "Enhances" "Essential"
                        "Filename" "Installed-Size" "MD5sum" "Maintainer"
                        "Origin" "Package" "Pre-Depends" "Priority"
                        "Provides" "Recommends" "Replaces" "Section"
                        "Size" "Source" "Suggests" "Tag" "Task" "Version"
                        "url"))
        fields)
    (while (> (length chosen) 0)
        (setq chosen
              (completing-read prompt
                               (mapcar (lambda (elt)
                                         (list elt elt))
                                       keywords)
                               nil
                               t))
      (setq keywords (delete chosen keywords))
      (if (stringp fields)
          (progn
            (when (> (length chosen) 0)
              (setq fields (concat fields "," chosen))))
        (setq fields chosen)))
    fields))

(defun apt-utils-toggle-package-info ()
  "Toggle between package and showpkg info for normal packages."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package (caar apt-utils-package-history))
        (type (cdar apt-utils-package-history))
        posns)
    (cond
     ((equal type 'normal)
      (setq posns (apt-utils-update-buffer-positions 'toggle))
      (setq apt-utils-package-history
            (cons (cons package 'normal-showpkg)
                  (cdr apt-utils-package-history)))
      (apt-utils-show-package-1 (car apt-utils-package-history) nil)
      (goto-char (car posns))
      (set-window-start (selected-window) (cadr posns)))
     ((equal type 'normal-showpkg)
      (setq posns (apt-utils-update-buffer-positions 'toggle))
      (setq apt-utils-package-history
            (cons (cons package 'normal)
                  (cdr apt-utils-package-history)))
      (apt-utils-show-package-1 (car apt-utils-package-history) nil)
      (goto-char (car posns))
      (set-window-start (selected-window) (cadr posns)))
     ((equal type 'virtual)
      (message "Cannot toggle info for virtual packages."))
     ((memq type '(search search-names-only
                          search-file-names
                          search-grep-dctrl))
      (message "Cannot toggle info for searches.")))))

(defun apt-utils-normal-package-p ()
  "Return non-nil if the current package is a normal package.
That is, not a normal-showpkg, search or a virtual package."
  (eq (cdar apt-utils-package-history) 'normal))

(defun apt-utils-toggle-package-p ()
  "Return non-nil if we can toggle between package and showpkg.
See also `apt-utils-toggle-package-info'."
  (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))

(defun apt-utils-check-package-lists ()
  "Determine whether package lists need rebuilding."
  (apt-utils-update-mode-name)
  (cond
   ((null apt-utils-package-list-built)
    (apt-utils-build-package-list))
   ((and (apt-utils-packages-needs-update)
         ;; Only act for non-nil apt-utils-automatic-update
         apt-utils-automatic-update
         (cond
          ((eq apt-utils-automatic-update t))
          ((eq apt-utils-automatic-update 'ask)
           (unless apt-utils-automatic-update-asked
             (setq apt-utils-automatic-update-asked t)
             (yes-or-no-p
              "APT package lists may be out of date. Update them? ")))))
    (apt-utils-build-package-list t))))

;; Find ChangeLog files

(defun apt-utils-view-changelog ()
  "Find ChangeLog for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-changelog-file package)))
      (if file
        (apt-utils-view-file file)
      (message "No ChangeLog file found for %s." package))))))

(defun apt-utils-changelog-file (&optional package)
  "Find ChangeLog file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("CHANGELOG" "ChangeLog" "Changelog" "changelog")
          '("" ".gz"))))
        file))

;; Find Debian ChangeLog files

(defun apt-utils-view-debian-changelog ()
  "Find Debian ChangeLog for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-debian-changelog-file package)))
      (if file
          (apt-utils-view-file file)
        (message "No Debian ChangeLog file found for %s." package))))))

(defun apt-utils-debian-changelog-file (&optional package)
  "Find Debian ChangeLog file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("changelog.Debian")
          '(".gz"))))
        file))

;; Find NEWS files

(defun apt-utils-view-news ()
  "Find NEWS for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-news-file package)))
      (if file
          (apt-utils-view-file file)
        (message "No NEWS file found for %s." package))))))

(defun apt-utils-news-file (&optional package)
  "Find NEWS file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("NEWS")
          '("" ".gz"))))
    file))

;; Find Debian NEWS files

(defun apt-utils-view-debian-news ()
  "Find Debian NEWS for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-debian-news-file package)))
      (if file
          (apt-utils-view-file file)
        (message "No Debian NEWS file found for %s." package))))))

(defun apt-utils-debian-news-file (&optional package)
  "Find Debian NEWS file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("NEWS.Debian")
          '(".gz"))))
    file))

;; Find README files

(defun apt-utils-view-readme ()
  "Find README for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-readme-file package)))
      (if file
          (apt-utils-view-file file)
        (message "No README file found for %s." package))))))

(defun apt-utils-readme-file (&optional package)
  "Find README file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("README" "readme")
          '("" ".gz"))))
    file))

;; Find Debian README files

(defun apt-utils-view-debian-readme ()
  "Find Debian README for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-debian-readme-file package)))
      (if file
          (apt-utils-view-file file)
        (message "No Debian README file found for %s." package))))))

(defun apt-utils-debian-readme-file (&optional package)
  "Find Debian README file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/" package)
          '("README.Debian" "README.debian")
          '("" ".gz"))))
    file))

;; Find copyright files

(defun apt-utils-view-copyright ()
  "Find copyright file for the current package."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let* ((package (caar apt-utils-package-history))
           (file (apt-utils-copyright-file package)))
      (if file
          (apt-utils-view-file file)
        (message "No copyright file found for %s." package))))))

(defun apt-utils-copyright-file (&optional package)
  "Find copyright file for PACKAGE or the current package."
  (unless package (setq package (caar apt-utils-package-history)))
  (let ((file
         (apt-utils-find-readable-file
          (format "/usr/share/doc/%s/copyright" package)
          '("")
          '(""))))
    file))

(defun apt-utils-view-man-page ()
  "View man page for the current package.
If there is more than one man page associated with the package,
offer a choice."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history))
          (regexp
           "^.*/man/\\([a-zA-Z_/.]+\\)?man[0-9]/\\(.*\\)\\.\\([0-9a-z]+\\)\\.gz")
          choice chosen files table)
      (setq files (apt-utils-get-package-files package
                                               "/man/.*\\.gz$"))
      (cond
       ((null files)
        (message "No man pages found for %s." package))
       ((not (cdr files))
        (setq chosen (car files)))
       (t
        (setq table (mapcar
                     (lambda (file)
                       (setq choice
                             (with-temp-buffer
                               (insert file)
                               (when (re-search-backward regexp nil t)
                                 (replace-match "\\2 (\\1\\3)" nil nil))
                               (buffer-string)))
                       (cons choice file))
                     files))
        (setq chosen
              (cdr (assoc
                    (let ((completion-ignore-case t))
                      (completing-read "Choose man page: " table nil t))
                    table)))))
      (when chosen
        (if (fboundp 'woman-find-file)
            (woman-find-file chosen)
          (manual-entry chosen)))))))

(defun apt-utils-view-emacs-startup-file ()
  "View Emacs startup file for the current package.
If there is more than one file associated with the package, offer
a choice."
  (interactive)
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (let ((package (caar apt-utils-package-history))
          chosen files table)
      (setq files
            (or (apt-utils-get-package-files package
                                             "^/etc/emacs/site-start.d/.*")
                (and (boundp 'debian-emacs-flavor)
                     (apt-utils-get-package-files
                      package
                      (format "^/etc/%s/site-start.d/.*"
                              (symbol-name debian-emacs-flavor))))))
      (cond
       ((null files)
        (message "No Emacs startup files found for %s." package))
       ((not (cdr files))
        (setq chosen (car files)))
       (t
        (setq table (mapcar
                     (lambda (file)
                       (cons file file))
                     files))
        (setq chosen
              (cdr (assoc
                    (let ((completion-ignore-case t))
                      (completing-read "Choose Emacs startup file: " table nil t))
                    table)))))
      (when chosen
        (apt-utils-view-file chosen))))))

(defun apt-utils-view-version ()
  "View installed version information for current package."
  (interactive)
  (let ((package (caar apt-utils-package-history))
        (type (cdar apt-utils-package-history)))
    (if (memq type '(normal normal-showpkg))
        (let ((info (apt-utils-get-installed-info package)))
          (if info
              (message (apply #'format
                              "%s: version %s (Desired = %s; Status = %s; Error = %s)"
                              package info))
            (message "Not installed; not known to dkpg")))
      (message "Can show version info only for normal packages"))))

(defun apt-utils-get-installed-info (package)
  "Return list of installation information for package PACKAGE."
  (let ((desired-list '((?u "Unknown")
                        (?i "Install")
                        (?r "Remove")
                        (?p "Purge")
                        (?h "Hold")))
        (status-list '((?n "Not installed")
                       (?i "Installed")
                       (?c "Config files")
                       (?u "Unpackage")
                       (?f "Failed config")
                       (?h "Half installed")))
        (err-list '((?  "None")
                    (?h "Hold")
                    (?r "Reinstall required")
                    (?x "Hold + reinstall required")))
        desired status err status-bad err-bad)
    (unless (eq package 'broken)
      (with-temp-buffer
        (let ((process-environment (append '("COLUMNS=200") (copy-alist process-environment))))
          (call-process apt-utils-dpkg-program nil t nil "-l" package))
        (when (re-search-backward
               (format "^\\([a-z ][a-z ][a-z ]\\)\\s-+%s\\s-+\\(\\S-+\\)"
                       (regexp-quote package)) nil t)
          (progn
            (setq desired (aref (match-string 1) 0)
                  status (aref (match-string 1) 1)
                  err (aref (match-string 1) 2)
                  status-bad (not (eq status (downcase status)))
                  err-bad (not (eq err (downcase err))))
            ;; Return list of information
            (list (match-string 2)      ; version
                  (cadr (assoc desired desired-list))
                  (concat (cadr (assoc (downcase status) status-list))
                          (and status-bad " [bad]"))
                  (concat (cadr (assoc (downcase err) err-list))
                          (and err-bad " [bad]")))))))))

(defun apt-utils-insert-installed-info (package)
  "Insert installed information for package PACKAGE at point."
  (let ((posn (point)))
    (insert (format " (%s)" (or (nth 2 (apt-utils-get-installed-info package))
                                "Not installed; not known to dpkg")))
    (add-text-properties (1+ posn)
                         (point)
                         '(face apt-utils-installed-status-face))))

;; File-related utility functions

(defun apt-utils-find-readable-file (dir prefixes suffixes)
  "Find a readable file composed of directory prefix and suffix.
Directory is DIR, prefix is one of PREFIXES and suffix is one of
SUFFIXES."
  (catch 'found
    (mapcar (lambda (prefix)
              (mapcar (lambda (suffix)
                        (when (file-readable-p (concat dir prefix suffix))
                          (throw 'found (concat dir prefix suffix))))
                      suffixes))
            prefixes)
    nil))                               ; Return nil, if no file found

(defun apt-utils-view-file (file)
  "View file FILE in function `view-mode'."
  (cond ((string-match "\\.gz$" file)
         (if (fboundp 'with-auto-compression-mode)
             (with-auto-compression-mode
               (view-file file))
           (auto-compression-mode 1)
           (view-file file)))
        (t
         (view-file file))))

;; Follow hyperlinks

(defun apt-utils-follow-link (new-session)
  "Follow hyperlink at point.
With non-nil NEW-SESSION, follow link in a new buffer."
  (interactive "P")
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((package
         (cadr
          (member 'apt-package (text-properties-at (point))))))
    (apt-utils-follow-link-internal package new-session)))

(defun apt-utils-mouse-follow-link (event)
  "Follow hyperlink at mouse click.
Argument EVENT is a mouse event."
  (interactive "e")
  (let (package)
    (save-selected-window
      (mouse-set-point event)
      (setq package (apt-utils-package-at-point))
      (apt-utils-follow-link-internal package nil))))

(defun apt-utils-package-at-point ()
  "Return name of package at point, if any."
  (cadr
   (member 'apt-package (text-properties-at
                         (point)))))

(defun apt-utils-follow-link-internal (package new-session)
  "Follow hyperlink for PACKAGE.
With non-nil NEW-SESSION, follow link in a new buffer."
  (cond
   ((equal package 'broken)
    (message "Package name is broken somehow."))
   (package
    (unless new-session
      (apt-utils-update-buffer-positions 'forward))
    (apt-utils-show-package-1 package nil new-session)
    (unless new-session
      (setq apt-utils-package-history
            (cons (cons package (apt-utils-package-type package))
                  apt-utils-package-history))))
   (t
    (message "No known package at point."))))

;; Go to previous package in list

(defun apt-utils-view-previous-package ()
  "Go back to previous package displayed."
  (interactive)
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (if (cdr apt-utils-package-history)
      (progn
        (let ((posns (apt-utils-update-buffer-positions 'backward)))
          (apt-utils-show-package-1 (cadr apt-utils-package-history) nil)
          (goto-char (car posns))
          (set-window-start (selected-window) (cadr posns)))
        (setq apt-utils-package-history (cdr apt-utils-package-history)))
    (message "No further package history.")))

(defun apt-utils-previous-package-p ()
  "Return non-nil if there is a previous entry in the package history.
See also `apt-utils-package-history'."
  (cdr apt-utils-package-history))

;; Adapted from widget-move

(defun apt-utils-next-package (&optional arg)
  "Move point to the ARG next package.
ARG may be negative to move backward."
  (interactive "p")
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (cond
   ;; No links
   ((or (null apt-utils-current-links)
        (= (hash-table-count apt-utils-current-links) 0))
    (message "No package links."))
   ;; One link
   ((and (= (hash-table-count apt-utils-current-links) 1)
         (not (eq (cdar apt-utils-package-history) 'search-file-names)))
    (goto-char (point-min))
    (goto-char (next-single-property-change (point)
                                                 'apt-package)))
   (t
    (let ((old (apt-utils-package-at)))
      ;; Forward.
      (while (> arg 0)
        (cond ((eobp)
               (goto-char (point-min)))
              (t
               (goto-char (or (next-single-property-change
                               (point) 'apt-package)
                              (point-max)))))
        (let ((new (apt-utils-package-at)))
          (when new
            (unless (eq new old)
              (setq arg (1- arg))
              (setq old new)))))
      ;; Backward.
      (while (< arg 0)
        (cond ((bobp)
               (goto-char (point-max)))
              (t
               (goto-char (or (previous-single-property-change
                               (point) 'apt-package)
                              (point-min)))))
        (let ((new (apt-utils-package-at)))
          (when new
            (unless (eq new old)
              (setq arg (1+ arg))))))
      ;; Go to beginning of field.
      (let ((new (apt-utils-package-at)))
        (while (eq (apt-utils-package-at) new)
          (backward-char)))
      (forward-char))))
  ;; Echo some info
  (when apt-utils-show-link-info
    (apt-utils-package-at-message)))

(defun apt-utils-previous-package (&optional arg)
  "Move point to the ARG previous package.
ARG may be negative to move forward."
  (interactive "p")
  (apt-utils-next-package (- arg)))

;; Choose a package from the known links

(defun apt-utils-choose-package-link (new-session)
  "Choose a Debian package from a list of links.
With non-nil NEW-SESSION, follow link in a new buffer."
  (interactive "P")
  (apt-utils-choose-package-link-internal new-session))

(defun apt-utils-choose-package-link-internal (new-session)
  "Choose a Debian package from a list of links.
With non-nil NEW-SESSION, follow link in a new buffer."
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (error "Not in APT utils buffer"))
   ((= (hash-table-count apt-utils-current-links) 0)
    (message "No package links."))
   (t
    (let* ((PC-word-delimiters "-")
           (package
            (completing-read "Choose related Debian package: "
                             (cond
                              (apt-utils-completing-read-hashtable-p
                               apt-utils-current-links)
                              (t
                               (apt-utils-build-completion-table
                                apt-utils-current-links)))
                             nil t)))
      (when (> (length package) 0)
        (unless new-session
          (apt-utils-update-buffer-positions 'forward))
        (apt-utils-show-package-1 package nil new-session)
        (unless new-session
          (setq apt-utils-package-history
                (cons (cons package (apt-utils-package-type package))
                      apt-utils-package-history))))))))

(defun apt-utils-build-package-list (&optional force)
  "Build list of Debian packages known to APT.
With optional argument FORCE, rebuild the packages lists even if
they are defined.  When package lists are not up-to-date, this is
indicated in `mode-name'."
  (when (or force (null apt-utils-package-list-built))
    (unwind-protect
        (progn
          (setq apt-utils-package-list-built nil
                apt-utils-automatic-update-asked nil)
          (message "Building Debian package lists...")
          ;; Hash table listing package types
          (if (hash-table-p apt-utils-package-list)
              (clrhash apt-utils-package-list)
            (setq apt-utils-package-list (make-hash-table :test 'equal)))
          ;; All packages except virtual ones
          (with-temp-buffer
            ;; Virtual and normal packages
            (call-process apt-utils-apt-cache-program nil '(t nil) nil "pkgnames")
            (goto-char (point-min))
            (while (not (eobp))
              (apt-utils-puthash (buffer-substring (apt-utils-line-beginning-position)
                                                   (apt-utils-line-end-position))
                                 'virtual apt-utils-package-list)
              (forward-line 1))
            ;; Normal packages
            (erase-buffer)
            (call-process apt-utils-apt-cache-program nil '(t nil) nil "pkgnames"
                          "-o" "APT::Cache::AllNames=0")
            (goto-char (point-min))
            (while (not (eobp))
              (apt-utils-puthash (buffer-substring (apt-utils-line-beginning-position)
                                                   (apt-utils-line-end-position))
                                 'normal apt-utils-package-list)
              (forward-line 1)))
          (message "Building Debian package lists...done.")
          (setq apt-utils-package-list-built (current-time))
          (apt-utils-update-mode-name))
      (unless apt-utils-package-list-built
        (message "Building Debian package lists...interrupted.")
        (apt-utils-update-mode-name)
        (if (hash-table-p apt-utils-package-list)
            (clrhash apt-utils-package-list))))))

(defun apt-utils-rebuild-package-lists ()
  "Rebuild the APT package lists."
  (interactive)
  (apt-utils-build-package-list t))

(defun apt-utils-choose-package ()
  "Choose a Debian package name."
  (let ((package
         (and (eq major-mode 'apt-utils-mode)
              (cadr (member 'apt-package
                            (text-properties-at (point))))))
        (PC-word-delimiters "-"))
    (when (not (stringp package))
      (setq package nil))
    (completing-read (if package
                         (format "Choose Debian package (%s): " package)
                       "Choose Debian package: ")
                     'apt-utils-choose-package-completion
                     nil t package)))

;; emacs 22 has `dynamic-completion-table' to help construct a
;; function like this, but emacs 21 and xemacs 21) don't
(defun apt-utils-choose-package-completion (str pred all)
  "Apt package name completion handler, for `completing-read'."
  (let ((enable-recursive-minibuffers t))
    (apt-utils-check-package-lists))
  (cond ((null all)
         (try-completion str (if apt-utils-completing-read-hashtable-p
                                 apt-utils-package-list
                               (apt-utils-build-completion-table
                                apt-utils-package-list))
                         pred))
        ((eq all t)
         (all-completions str (if apt-utils-completing-read-hashtable-p
                                  apt-utils-package-list
                                (apt-utils-build-completion-table
                                 apt-utils-package-list))
                          pred))
        ((eq all 'lambda)
         (if (fboundp 'test-completion)
	     ;; `test-completion' is new in emacs22, and it takes
	     ;; hashtables, so don't really need to test
	     ;; apt-utils-completing-read-hashtable-p
             (test-completion str (if apt-utils-completing-read-hashtable-p
                                      apt-utils-package-list
                                    (apt-utils-build-completion-table
                                     apt-utils-package-list))
                              pred)
           (and (gethash str apt-utils-package-list)
		t)))))

(defun apt-utils-build-completion-table (hash)
  "Build completion table for packages using keys of hashtable HASH."
  (let (ret)
    (maphash (lambda (key value)
               (setq ret (cons (list key) ret)))
             hash)
    ret))

;; Add hyperlinks

(defun apt-utils-add-package-links ()
  "Add hyperlinks to related Debian packages."
  (let ((keywords '("Conflicts" "Depends" "Enhances" "Package"
                    "Pre-Depends" "Provides" "Recommends" "Replaces"
                    "Suggests"))
        match)
    (if (hash-table-p apt-utils-current-links)
        (clrhash apt-utils-current-links)
      (setq apt-utils-current-links (make-hash-table :test 'equal)))
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ \n:]+\\):\\( \\|$\\)"
                              (point-max) t)
      (setq match (match-string 1))
      (add-text-properties (if (looking-at "$")
                               (point) ;; Conffiles (also see below)
                             (1- (point)))
                           (save-excursion
                             (beginning-of-line)
                             (point))
                           `(,apt-utils-face-property apt-utils-field-keyword-face))
      (cond
       ((member match keywords)
        ;; Remove newline characters in field
        (let ((end (apt-field-end-position)))
          (subst-char-in-region (point) end ?\n ?\  )
          (canonically-space-region (point) end))
        ;; Find packages
        (let ((packages (apt-utils-current-field-packages))
              (inhibit-read-only t)
              face
              length length-no-version
              package)
          (while packages
            (setq package (car packages))
            (setq length (length package))
            ;; Remove version info (in parenthesis), and whitespace
            (setq package (apt-utils-replace-regexp-in-string
                           "\\((.*)\\|\\s-+\\)" "" package))
            (setq length-no-version (length package))
            ;; Package type
            (cond
             ((equal (apt-utils-package-type package t) 'normal)
              (setq face 'apt-utils-normal-package-face))
             ((equal (apt-utils-package-type package t) 'virtual)
              (setq face 'apt-utils-virtual-package-face))
             (t
              (setq face 'apt-utils-broken-face)
              (setq package 'broken)))
            ;; Store package links
            (apt-utils-current-links-add-package package)
            ;; Add text properties
            (add-text-properties (point) (+ (point) length-no-version)
                                 `(,apt-utils-face-property ,face
                                        mouse-face highlight
                                        apt-package ,package))
            ;; Version?
            (when (> length length-no-version)
              (add-text-properties (+ (point) length-no-version 1)
                                   (+ (point) length)
                                   `(,apt-utils-face-property apt-utils-version-face)))
            ;; Fill package names
            (when (and apt-utils-fill-packages
                       (> (current-column) (+ 2 (length match)))
                       (> (+ (current-column) length) fill-column))
              (when (equal (char-before) ?\ )
                (delete-char -1))          ; trailing whitespace
              (insert "\n" (make-string (+ 2 (length match)) ? )))
            (forward-char length)
            (when (and (equal match "Package")
                       apt-utils-display-installed-status)
              (apt-utils-insert-installed-info package))
            (skip-chars-forward ", |\n")
            (setq packages (cdr packages)))))
       ((equal match "Description")
        (add-text-properties (point)
                             (save-excursion
                               (or
                                (re-search-forward "^[^ ]" (point-max) t)
                                (point-max)))
                             `(,apt-utils-face-property apt-utils-description-face)))
       ;; Conffiles doesn't have trailing space
       ((looking-at "$")
        nil)
       (t
        (add-text-properties (1- (point))
                             (save-excursion
                               (end-of-line)
                               (point))
                             `(,apt-utils-face-property apt-utils-field-contents-face)))))))

(defun apt-utils-add-showpkg-links (package)
  "Add hyperlinks to related Debian packages for PACKAGE."
  (let ((keywords '("Reverse Depends" "Reverse Provides"))
        (inhibit-read-only t)
        start end regexp face link)
    (if (hash-table-p apt-utils-current-links)
        (clrhash apt-utils-current-links)
      (setq apt-utils-current-links (make-hash-table :test 'equal)))
    (while keywords
      (setq regexp (concat "^" (car keywords) ": "))
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) t)
        (add-text-properties (match-beginning 0) (1- (match-end 0))
                             `(,apt-utils-face-property
                               apt-utils-field-keyword-face))
        ;; Limits of search
        (setq start (1+ (point)))
        (setq end (or (re-search-forward "[a-z]:" (point-max) t)
                      (point-max)))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (while (not (eobp))
            (when (or (looking-at "^\\s-+\\(.*\\),")
                      (looking-at "^\\(.*\\) "))
              (setq link (match-string 1))
              (cond
               ((equal (apt-utils-package-type link t) 'normal)
                (setq face 'apt-utils-normal-package-face))
               ((equal (apt-utils-package-type link t) 'virtual)
                (setq face 'apt-utils-virtual-package-face))
               (t
                (setq face 'apt-utils-broken-face)
                (setq link 'broken)))
              ;; Store package links
              (apt-utils-current-links-add-package link)
              (add-text-properties (match-beginning 1) (match-end 1)
                                   `(,apt-utils-face-property ,face
                                          mouse-face highlight
                                          apt-package ,link)))
          (forward-line))))
      (setq keywords (cdr keywords))))
  (when (and apt-utils-display-installed-status
             (eq (apt-utils-package-type package t) 'normal))
    (goto-char (point-min))
    (re-search-forward "Package: .*$")
    (apt-utils-insert-installed-info package)))

(defun apt-utils-add-search-links (type)
  "Add hyperlinks to related Debian packages.
The type of search is specified by TYPE."
  (let ((inhibit-read-only t)
        local-keymap
        face link regexp)
    (when (eq type 'search-file-names)
      (setq local-keymap (make-sparse-keymap))
      (define-key local-keymap (kbd "RET")
        (lambda ()
          (interactive)
          (view-file (or (get-text-property (point) 'apt-package-file)
                         (get-text-property (1- (point)) 'apt-package-file))))))
    (if (hash-table-p apt-utils-current-links)
        (clrhash apt-utils-current-links)
      (setq apt-utils-current-links (make-hash-table :test 'equal)))
    (goto-char (point-min))
    (forward-line 2)                    ; Move past header
    (cond
     ((eq type 'search-file-names)
      ;; Reformat diversion information
      (save-excursion
        (while (re-search-forward "diversion by \\(.*\\) \\(from\\|to\\): \\(.*\\)" nil t)
          (replace-match "\\1: \\3 (diversion \\2)" nil nil)))
      (setq regexp "\\([^:,]+\\)[,:]"))
     (t
      (setq regexp"^\\([^ ]+\\) - ")))
    (while (re-search-forward regexp (point-max) t)
      (setq link (match-string 1))
      (cond
       ((equal (apt-utils-package-type link t) 'normal)
        (setq face 'apt-utils-normal-package-face))
       ((equal (apt-utils-package-type link t) 'virtual)
        (setq face 'apt-utils-virtual-package-face))
       (t
        (setq face 'apt-utils-broken-face)
        (setq link 'broken)))
      ;; Store package links
      (apt-utils-current-links-add-package link)
      (add-text-properties (match-beginning 1) (match-end 1)
                           `(,apt-utils-face-property ,face
                                  mouse-face highlight
                                  apt-package ,link))
      ;; Multiple fields separated by commas
      (when (eq type 'search-file-names)
        (if (eq (char-before) ?\:)
            (progn
              (when local-keymap
		(let ((start (1+ (point)))
		      (end (save-excursion
			     (goto-char (apt-utils-line-end-position))
			     (re-search-backward " (diversion \\(from\\|to\\))"
						 (apt-utils-line-beginning-position)
						 t)
			     (point))))
		  (add-text-properties start end
				       `(face apt-utils-file-face
					      keymap ,local-keymap
					      ;; Pretend we're a package
					      ;; so that we can move
					      ;; here with
					      ;; apt-utils-next-package
					      apt-package dummy
					      apt-package-file
					      ,(buffer-substring-no-properties start end)
					      ))))
	      (goto-char (1+ (apt-utils-line-end-position))))
	  (skip-chars-forward ", "))))))

(defun apt-utils-package-type (package &optional no-error)
  "Return what type of package PACKAGE is.
With optional argument NO-ERROR, don't flag an error for unknown
packages."
  (or (gethash package apt-utils-package-list)
      (cond
       (no-error
        nil)
       (t
        (error
         (substitute-command-keys
          "Package name is broken: rebuild package lists using \\[apt-utils-rebuild-package-lists] may help")
         package)))))

(defun apt-utils-package-at ()
  "Get package at point."
  (get-text-property (point) 'apt-package))

(defun apt-utils-package-at-message ()
  "Emit message describing package at point."
  (let ((package (apt-utils-package-at)))
    (cond
     ((eq package 'dummy)
      ;; Do nothing as this isn't really a package
      )
     ((equal package 'broken)
      (message "Package name is broken somehow."))
     (package
      (with-temp-buffer
        (call-process apt-utils-apt-cache-program nil t nil "show" package)
        (if (re-search-backward "^Description: \\(.*\\)$" (point-min) t)
            (message "%s: %s." package (match-string 1))
          (message "%s: virtual package (no description)."
                   package)))))))

(defun apt-utils-quit (&optional kill-buffer)
  "Quit this `apt-utils-mode' buffer.
With prefix argument KILL-BUFFER, kill the `apt-utils-mode'
buffer."
  (interactive "P")
  (unless (equal major-mode 'apt-utils-mode)
    (error "Not in APT utils buffer"))
  (let ((buffer (current-buffer)))
    (if (fboundp 'quit-window)
        (quit-window)
      (bury-buffer))
    (when kill-buffer
      (kill-buffer buffer)))
  (run-hooks 'apt-utils-quit-hooks))

(defun apt-utils-cleanup ()
  "Clean up lists used by `apt-utils-mode'.
Specifically, nullify `apt-utils-package-list'.  Only do this if
there are no buffers left in `apt-utils-mode'."
  (unless (memq 'apt-utils-mode
                (mapcar (lambda (b)
                          (with-current-buffer b
                            major-mode))
                        (delete (current-buffer) (buffer-list))))
    (clrhash apt-utils-package-list)
    (setq apt-utils-package-list-built nil)))

(defun apt-utils-describe-package ()
  "Describe package at point."
  (interactive)
  (apt-utils-package-at-message))

(defun apt-utils-kill-other-window-buffers ()
  "Kill buffers in other windows and the windows themselves.
See `apt-utils-kill-buffer-confirmation-function' for
customisation options."
  (interactive)
  (cond
   ((not (eq major-mode 'apt-utils-mode))
    (error "Not in APT utils buffer"))
   ((not (cdr (window-list)))
    (message "No other windows to kill"))
   (t
    (when (or (null apt-utils-kill-buffer-confirmation-function)
              (funcall apt-utils-kill-buffer-confirmation-function
                       "Kill buffers in other windows? "))
      (let ((buffer-list
             (delq (current-buffer)
                   (mapcar #'window-buffer (window-list)))))
        (mapc (lambda (b)
                (when (buffer-live-p b)
                  (kill-buffer b)))
              buffer-list))
      (delete-other-windows))
    (message nil))))

;; Track positions

(defun apt-utils-update-buffer-positions (type)
  "Update `apt-utils-buffer-positions'.
TYPE can be forward, backward, or toggle."
  (let (posns)
    (cond
     ((eq type 'forward)
      ;; Make the key unique; we could visit the same package more
      ;; than once
      (apt-utils-puthash (format "%s/%s/%d"
                       (caar apt-utils-package-history)
                       (cdar apt-utils-package-history)
                       (length apt-utils-package-history))
               (list (point) (window-start (selected-window)))
               apt-utils-buffer-positions))
     ((eq type 'backward)
      ;; Remove old values
      (remhash (format "%s/normal/%d"
                       (caar apt-utils-package-history)
                       (length apt-utils-package-history))
               apt-utils-buffer-positions)
      (remhash (format "%s/normal-showpkg/%d"
                       (caar apt-utils-package-history)
                       (length apt-utils-package-history))
               apt-utils-buffer-positions)
      (remhash (format "%s/virtual/%d"
                       (caar apt-utils-package-history)
                       (length apt-utils-package-history))
               apt-utils-buffer-positions)
      ;; Get position for previous package
      (setq posns
            (gethash (format "%s/%s/%d"
                             (car (cadr apt-utils-package-history))
                             (cdr (cadr apt-utils-package-history))
                             (1- (length apt-utils-package-history)))
                     apt-utils-buffer-positions)))
     ((eq type 'toggle)
      ;; new/old package types
      (let ((package (caar apt-utils-package-history))
            (type (cdar apt-utils-package-history))
            new old)
        (if (equal type 'normal)
            (setq old 'normal
                  new 'normal-showpkg)
          (setq old 'normal-showpkg
                new 'normal))
        ;; Set position for old entry
        (apt-utils-puthash (format "%s/%s/%d"
                         package
                         old
                         (length apt-utils-package-history))
                 (list (point) (window-start (selected-window)))
                 apt-utils-buffer-positions)
        ;; Get position for new entry
        (setq posns
              (gethash (format "%s/%s/%d"
                               package
                               new
                               (length apt-utils-package-history))
                       apt-utils-buffer-positions
                       (list 1 1)))     ; default value
        )))
    posns))

(defun apt-utils-current-field-packages ()
  "Return a list of the packages on the current line."
  (let ((keywords '("Conflicts" "Depends" "Enhances" "Package"
                    "Pre-Depends" "Provides" "Recommends" "Replaces"
                    "Suggests"))
        eol match packages posn string)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (cond
       ((eobp)
        (message "Not on package field line.")
        nil)
       ((and (re-search-forward "^\\([^ \n:]+\\): " eol t)
             (setq match (match-string 1))
             (member match keywords))
        (setq posn (point))
        (goto-char (apt-field-end-position))
        (setq string (buffer-substring-no-properties posn (point)))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (while (re-search-forward "\n *" nil t)
            (replace-match " "))
          (setq packages
                ;; Packages split by commas, or alternatives by vertical
                ;; bars; for Enhances, multiple lines my be spanned
                (split-string (buffer-substring (point-min) (point-max))
                              " ?[,|] ?")))
        packages)
       (t
        (message "Not on package field line.")
        nil)))))

(defun apt-field-end-position ()
  "Move to end of current field."
  (save-excursion
    (re-search-forward "\\(^[^: ]+:\\|^$\\)")
    (beginning-of-line)
    (backward-char)
    (point)))

;; Borrowed from gnus/lisp/time-date.el

(defun apt-utils-time-less-p (t1 t2)
  "Say whether time value T1 is less than time value T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
           (< (nth 1 t1) (nth 1 t2)))))

(defun apt-utils-web-browse-debian-changelog ()
  "Browse web version of Debian ChangeLog file for the current package."
  (interactive)
  (apt-utils-web-browse-url
   apt-utils-web-browse-debian-changelog-url))

(defun apt-utils-web-browse-bug-reports ()
  "Browse Debian bug reports for the current package."
  (interactive)
  (apt-utils-web-browse-url
   apt-utils-web-browse-bug-reports-url))

(defun apt-utils-web-browse-copyright ()
  "Browse web version of Debian copyright file for the current package."
  (interactive)
  (apt-utils-web-browse-url
   apt-utils-web-browse-copyright-url))

(defun apt-utils-web-browse-versions ()
  "Browse web version information for the current package."
  (interactive)
  (apt-utils-web-browse-url
   apt-utils-web-browse-versions-url))

(defun apt-utils-web-browse-url (url)
  "Browse Debian-related URL.
The URL can contain tokens that need formatting (see
`apt-utils-web-format-url')."
  (cond
   ((not (equal major-mode 'apt-utils-mode))
    (message "Not in APT utils buffer."))
   ((not (memq (cdar apt-utils-package-history) '(normal normal-showpkg)))
    (message "Not a normal package."))
   (t
    (browse-url (apt-utils-web-format-url url)))))

(defun apt-utils-web-format-url (url)
  "Format and return Debian URL.
The tokens that can be replaced are:
    %d: pool directory
    %s: source package name
    %p: package name
    %v: package version."
  (let ((buffer (current-buffer))
        (package (caar apt-utils-package-history))
        (type (cdar apt-utils-package-history))
        char source-package version)
    (save-excursion                     ; for normal package type
      (with-temp-buffer
        (cond
         ((eq type 'normal)
          (set-buffer buffer))
         ((eq type 'normal-showpkg)
          (call-process apt-utils-apt-cache-program nil '(t nil) nil "show" package)))
        (goto-char (point-min))
        (if (re-search-forward "^Source: \\(.*\\)$" (point-max) t)
            (setq source-package (match-string 1))
          (setq source-package package))
        (goto-char (point-min))
        (re-search-forward "^Version: \\([0-9]:\\)?\\(.*\\)$" (point-max))
        (setq version (match-string 2))))
    ;; Format the URL
    (while (string-match "%\\(.\\)" url)
      (setq char (string-to-char (match-string 1 url)))
      (setq url (apt-utils-replace-regexp-in-string
                 (match-string 0 url)
                 (cond
                  ((eq char ?d)
                   (substring source-package 0
                              (if (string-match "^lib[a-z]"
                                                source-package)
                                  4 1)))
                  ((eq char ?s) source-package)
                  ((eq char ?p) package)
                  ((eq char ?v) version)
                  (t
                   (error "Unrecognized token (%%%c) in URL: %s" char url)))
                 url))))
  url)

(defun apt-utils-packages-needs-update ()
  "Return t if `apt-utils' package lists needs updating."
  (or (not apt-utils-package-list-built)
      (apt-utils-time-less-p apt-utils-package-list-built
                             (nth 5 (file-attributes apt-utils-timestamped-file)))))

(defun apt-utils-update-mode-name ()
  "Update `mode-name' for all buffers in `apt-utils-mode'."
  (let* ((need-update (apt-utils-packages-needs-update))
         (update-string
          (and need-update
               (substitute-command-keys
                ": update using \\<apt-utils-mode-map>\\[apt-utils-rebuild-package-lists]")))
         (name (concat "APT utils" update-string)))
    (mapc (lambda (b)
            (with-current-buffer b
              (when (eq major-mode 'apt-utils-mode)
                (setq mode-name name))))
          (buffer-list))))

(defun apt-utils-current-links-add-package (package)
  "Add PACKAGE to `apt-utils-current-links' hashtable."
  (unless (eq package 'broken)
    (apt-utils-puthash package nil apt-utils-current-links)))

;; Mode settings

(defvar apt-utils-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "#")             'apt-utils-rebuild-package-lists)
    (define-key map (kbd "1")             'delete-other-windows)
    (define-key map (kbd "<")             'apt-utils-view-previous-package)
    (define-key map (kbd ">")             'apt-utils-choose-package-link)
    (define-key map (kbd "?")             'describe-mode)
    (define-key map (kbd "DEL")           'scroll-down)
    (define-key map (kbd "M-TAB")         'apt-utils-previous-package)
    (define-key map (kbd "RET")           'apt-utils-follow-link)
    (define-key map (kbd "S s")           'apt-utils-search)
    (define-key map (kbd "S f")           'apt-utils-search-file-names)
    (define-key map (kbd "S g")           'apt-utils-search-grep-dctrl)
    (define-key map (kbd "S n")           'apt-utils-search-names-only)
    (define-key map (kbd "SPC")           'scroll-up)
    (define-key map (kbd "TAB")           'apt-utils-next-package)
    (define-key map (kbd "b C")           'apt-utils-web-browse-debian-changelog)
    (define-key map (kbd "b b")           'apt-utils-web-browse-bug-reports)
    (define-key map (kbd "b l")           'apt-utils-web-browse-copyright)
    (define-key map (kbd "b v")           'apt-utils-web-browse-versions)
    (define-key map (kbd "d")             'apt-utils-describe-package)
    (when (fboundp 'window-list)
      (define-key map (kbd "k")           'apt-utils-kill-other-window-buffers))
    (define-key map (kbd "l")             'apt-utils-list-package-files)
    (define-key map (kbd "o")             'other-window)
    (define-key map (kbd "q")             'apt-utils-quit)
    (define-key map (kbd "s")             'apt-utils-show-package)
    (define-key map (kbd "t")             'apt-utils-toggle-package-info)
    (define-key map (kbd "v C")           'apt-utils-view-debian-changelog)
    (define-key map (kbd "v R")           'apt-utils-view-debian-readme)
    (define-key map (kbd "v N")           'apt-utils-view-debian-news)
    (define-key map (kbd "v c")           'apt-utils-view-changelog)
    (define-key map (kbd "v e")           'apt-utils-view-emacs-startup-file)
    (define-key map (kbd "v f")           'apt-utils-view-package-files)
    (define-key map (kbd "v l")           'apt-utils-view-copyright)
    (define-key map (kbd "v m")           'apt-utils-view-man-page)
    (define-key map (kbd "v n")           'apt-utils-view-news)
    (define-key map (kbd "v r")           'apt-utils-view-readme)
    (define-key map (kbd "v v")           'apt-utils-view-version)
    (define-key map [(shift iso-lefttab)] 'apt-utils-previous-package)
    (define-key map [(shift tab)]         'apt-utils-previous-package)
    (define-key map
      (if apt-utils-xemacs-p '(button2) (kbd "<mouse-2>"))
      'apt-utils-mouse-follow-link)
    map)
  "Keymap for apt-utils mode.")

;; Menus

(defvar apt-utils-menu nil
  "Menu to use for `apt-utils-mode'.")

(when (fboundp 'easy-menu-define)

  (easy-menu-define apt-utils-menu apt-utils-mode-map "Apt Utils Menu"
    `("Apt Utils"
      ["Show Package"               apt-utils-show-package t]
      ["Toggle Package Info"        apt-utils-toggle-package-info
       (apt-utils-toggle-package-p)]
      ["View Previous Package"      apt-utils-view-previous-package
       (apt-utils-previous-package-p)]
      ["Choose Package Link"        apt-utils-choose-package-link
       (> (hash-table-count apt-utils-current-links) 0)]
      ["Next Package Link"          apt-utils-next-package
       (> (hash-table-count apt-utils-current-links) 0)]
      ["Previous Package Link"      apt-utils-previous-package
       (> (hash-table-count apt-utils-current-links) 0)]
      ["Follow Link at Point"       apt-utils-follow-link
       (apt-utils-package-at-point)]
      ["Rebuild Package Lists"      apt-utils-rebuild-package-lists t]
      "---"
      ("Search"
       ["Package Descriptions"      apt-utils-search t]
       ["Package Names"             apt-utils-search-names-only t]
       ["Installed Files"           apt-utils-search-file-names t]
       ["Grep-Dctrl"                apt-utils-search-grep-dctrl t])
      ("View Files"
       ,@(list (if apt-utils-xemacs-p
                   :included
                 :active)
               '(apt-utils-current-package-installed-p))
       ["ChangeLog"                 apt-utils-view-changelog
        (apt-utils-changelog-file)]
       ["Debian ChangeLog"          apt-utils-view-debian-changelog
        (apt-utils-debian-changelog-file)]
       ["README"                    apt-utils-view-readme
        (apt-utils-readme-file)]
       ["Debian README"             apt-utils-view-debian-readme
        (apt-utils-debian-readme-file)]
       ["NEWS"                      apt-utils-view-news
        (apt-utils-news-file)]
       ["Debian NEWS"               apt-utils-view-debian-news
        (apt-utils-debian-news-file)]
       ["Copyright"                 apt-utils-view-copyright
        (apt-utils-copyright-file)]
       "---"
       ["Man Page"                  apt-utils-view-man-page
        (apt-utils-current-package-installed-p)]
       "---"
       ["All Package Files (dired)" apt-utils-view-package-files
        (apt-utils-current-package-installed-p)])
      ("Browse URL"
       ,@(list (if apt-utils-xemacs-p
                   :included
                 :active)
               '(apt-utils-toggle-package-p))
       ["Debian ChangeLog"          apt-utils-web-browse-debian-changelog t]
       ["Bug Reports"               apt-utils-web-browse-bug-reports t]
       ["Copyright"                 apt-utils-web-browse-copyright t]
       ["Package Versions"          apt-utils-web-browse-versions t])
      "---"
      ["Help"                       describe-mode t]
      ["Quit"                       apt-utils-quit t])))

(defun apt-utils-mode ()
  "Major mode to interface Emacs with APT (Debian package management).

Start things off with, for example:

    M-x apt-utils-show-package RET emacs21 RET

Other packages (dependencies, conflicts etc.) can be navigated
using:

    \\[apt-utils-toggle-package-info] toggle package and showpkg information
    \\[apt-utils-view-previous-package] show the previous package from history
    \\[apt-utils-choose-package-link] choose next package from current links
    \\[apt-utils-next-package] move to next package link
    \\[apt-utils-previous-package] move to previous package link
    \\[apt-utils-follow-link] show package for the link at point
    \\[apt-utils-list-package-files] list package files (in a `dired' buffer)

Confirmation will be requested before updating the list of known
packages.  The update can be started at any time with
\\[apt-utils-rebuild-package-lists].

Package searches can be performed using:

    \\[apt-utils-search] search for regular expression in package names and descriptions
    \\[apt-utils-search-names-only] search for regular expression in package names
    \\[apt-utils-search-file-names] search for string in filenames
    \\[apt-utils-search-grep-dctrl] search for regular expression in selected package fields
    (using the grep-dctrl program)

Files associated with installed packages can be accessed using:

    \\[apt-utils-view-changelog] view ChangeLog file
    \\[apt-utils-view-debian-changelog] view Debian ChangeLog file
    \\[apt-utils-view-readme] view README file
    \\[apt-utils-view-debian-readme] view Debian ChangeLog file
    \\[apt-utils-view-news] view NEWS file
    \\[apt-utils-view-debian-news] view Debian NEWS file
    \\[apt-utils-view-copyright] view copyright (licence) file
    \\[apt-utils-view-man-page] view man page

Web locations can be visited using:

    \\[apt-utils-web-browse-debian-changelog] browse Debian ChangeLog URL
    \\[apt-utils-web-browse-bug-reports] browse bug report URL
    \\[apt-utils-web-browse-copyright] browse copyright (licence) URL
    \\[apt-utils-web-browse-versions] browse package versions URL

A history of navigated packages is maintained when package links
are followed using `apt-utils-choose-package-link' or
`apt-utils-follow-link'.  This history is reset when
`apt-utils-show-package' or any of the search commands is used.

Key definitions:
\\{apt-utils-mode-map}"
  (kill-all-local-variables)
  (use-local-map apt-utils-mode-map)
  (setq major-mode 'apt-utils-mode)
  (setq mode-name "APT utils")
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  ;; XEmacs
  (when (and (fboundp 'easy-menu-add)
             apt-utils-menu)
    (easy-menu-add apt-utils-menu))
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'apt-utils-cleanup nil t)
  (run-hooks 'apt-utils-mode-hook))

;; Debugging

(defun apt-utils-trace-all ()
  "Trace all `apt-utils' functions.  For debugging."
  (require 'trace)
  (let ((buffer (get-buffer-create "*APT Utils Trace*")))
    (buffer-disable-undo buffer)
    (all-completions "apt-utils" obarray
                     (lambda (sym)
                       (and (fboundp sym)
                            (not (memq (car-safe (symbol-function sym))
                                       '(autoload macro)))
                            (trace-function-background sym buffer))))))

(provide 'apt-utils)

;;; apt-utils.el ends here
