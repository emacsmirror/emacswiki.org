;;; sunrise-x-mirror.el --- Extension to the Sunrise Commander File Manager that
;; allows to access compressed archives in read/write mode.

;; Copyright (C) 2008 José Alfredo Romero L.

;; Author: José Alfredo Romero L. <joseito@poczta.onet.pl>
;; Keywords: Sunrise Commander Emacs File Manager Extension Archives Read/Write

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation,  either  version  3 of the License, or (at your option) any later
;; version.
;; 
;; This  program  is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See the GNU General Public License for more de-
;; tails.

;; You  should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This  is  an  *experimental* extension for the Sunrise Commander file manager
;; (see http://joseito.republika.pl/sunrise-commander.el.gz for  details),  that
;; allows  browsing  compressed  archives  in full read-write mode. Sunrise does
;; provide means for transparent browsing of archives (through AVFS),  but  this
;; supports exclusively read-only navigation - if you want to edit a file inside
;; the virtual filesystem, copy, remove, or rename anything, you still  have  to
;; uncompress the archive, do the stuff and compress it back yourself.

;; It  uses  funionfs  to  create  a  writeable  overlay on top of the read-only
;; filesystem provided by AVFS. You can freely add, remove  or  modify  anything
;; inside  the  resulting  union filesystem (a.k.a. the "mirror area"), and then
;; commit all modifications (or not) to  the  original  archive  with  a  single
;; keystroke.  There  is no preliminary uncompressing of the archive and nothing
;; happens if you don't make any modifications to it (or  if  you  don't  commit
;; them).  When  you commit your modifications, the contents of the union fs are
;; compressed to create an updated archive that is  then  used  to  replace  the
;; original one (optionally, you can keep a backup copy of the original, just in
;; case).

;; Be  warned,  though,  that  this  method  may  be  impractical for very large
;; archives with strong compression (like tar.gz or tar.bz2), since  the  uncom-
;; pressing  happens  in the final stage and requires multiple access operations
;; through AVFS. What this means is that probably you'll have to wait a looooong
;; time  if you try to commit changes to a tar.bz2 file with several hundreds of
;; megabytes in size.

;; For this extension to work you ABSOLUTELY must have:
;; 1) FUSE + AVFS support working in your Sunrise Commander installment.
;; 2) FUSE + funionfs installed and working.
;; 3) All programs required for repacking archives: at least zip, jar and tar.
;; 4)  Your  AVFS  filesystem  root  mounted  inside  a directory where you have
;; writing access.

;; This  means  that  most  probably  this extension will work out-of-the-box on
;; Linux (or other unices), but you'll have a hard  time  to  make  it  work  on
;; Windows.   It was written on GNU Emacs 23 on Linux and tested on GNU Emacs 22
;; and 23 for Linux.

;; This is version 1 $Rev: 138 $ of the Sunrise Commander Mirror Extension.

;;; Installation and Usage:

;; 1) Put this file somewhere in your emacs load-path.

;; 2) Add a (require 'sunrise-x-mirror) to your .emacs file, preferably right
;; after (require 'sunrise-commander).

;; 3) Evaluate the new expression, or reload your .emacs file, or restart emacs.

;; 4)  Run  the Sunrise Commander (M-x sunrise), select any compressed directory
;; in the active pane and press C-c C-b. This will automatically take you to the
;; mirror area for the selected archive. You can make any modifications you want
;; to the contents of the archive in here. When you're done,  just  press  again
;; C-c C-b  anywhere  inside the mirror area. If there are any changes to commit
;; (and if you confirm) the original archive will be replaced by a new one  with
;; the  contents  of the mirror area you've just been working on.   If you don't
;; change the defaults, the original will be renamed to  its  own  name  with  a
;; ".bak" extension.

;; 5)  You  can add support for new archive formats by adding new entries to the
;; sr-mirror-pack-commands-alist  custom  variable,  which  contains  a  regular
;; expression  to  match against the name of the archive and a string containing
;; the shell command to  execute  for  packing  back  the  mirror  area  into  a
;; compressed archive.

;; 6)  Once you've gained enough confidence using this extension (if you do) you
;; can reset the sr-mirror-keep-backups flag to get rid of all the backup copies
;; produced by it.

;;; Code:

(eval-when-compile (require 'sunrise-commander))

(defcustom sr-mirror-keep-backups t
  "Flag that indicates whether backup files are to be kept whenever the mirror
  of a read-only archive is modified and committed."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-mirror-pack-commands-alist
  '(
    ("\\.zip$" .                    "zip -r   %f *")
    ("\\.[jwesh]ar$" .              "jar cvf  %f *")
    ("\\.\\(?:tar\\.gz\\|tgz\\)$" . "tar cvzf %f *")
    ("\\.tar\\.bz2$" .              "tar cvjf %f *")
   )
  "List of shell commands to repack the contents of the current mirror area into
  a compressed archive of the appropriate type. Use %f as a placeholder for  the
  name  of  the  resulting  archive. If no repacking command has been registered
  here for a file (usu. a file extension),  Sunrise  will  refuse  to  create  a
  mirror area for it even if it is normally browseable through AVFS."
  :group 'sunrise
  :type 'alist)

(defvar sr-mirror-home nil
  "Root directory of all mirror areas. This is set automatically by the function
  sr-mirror enable and reset by sr-mirror-disable to keep the mirror home  path,
  as  well  as  to  indicate  mirroring  support  is on/off. Do not mess with it
  directly - if you need to change the name of your mirror home dir then  modify
  sr-mirror-enable.")

(define-key sr-mode-map "\C-c\C-b" 'sr-mirror-toggle)

(defun sr-mirror-enable ()
  "Enables  sunrise  mirror  support by setting the sr-mirror-home variable to a
  non-nil value and activating all advice necessary for mirror operations.  This
  method is called every time a new mirror area is created."
  (if sr-mirror-home
      (ignore)
    (progn
      (setq sr-mirror-home (concat sr-avfs-root "#mirror#/"))
      (ad-activate 'sr-copy-files)
      (ad-activate 'make-directory)
      (ad-activate 'save-buffer))))

(defun sr-mirror-disable ()
  "Disables   sunrise   mirror  support  by  resetting  the  sr-mirror-home  and
  deactivating all advice used in mirror operations. This method is called after
  the last mirror area in the current mirror home is closed."
  (if sr-mirror-home
      (progn
        (setq sr-mirror-home nil)
        (ad-deactivate 'sr-copy-files)
        (ad-deactivate 'make-directory)
        (ad-deactivate 'save-buffer))))

(defun sr-mirror-open ()
  "Uses  funionfs to create a writeable filesystem overlay over the AVFS virtual
  fs of the selected compressed archive and displays it in the current pane. The
  result  is  a  mirror  of  the  contents of the original archive that is fully
  writeable."
  (interactive)
  (let* ((path (dired-get-filename))
         (virtual (sr-avfs-dir path))
         (fname (file-name-nondirectory path))
         (base) (mirror) (overlay) (command))

    (if (null (assoc-default fname sr-mirror-pack-commands-alist 'string-match))
        (error (concat "Sunrise: sorry, no packer was registered for " fname)))
    (if (null virtual)
        (error (concat "Sunrise: sorry, don't know how to mirror " path)))

    (sr-mirror-enable)
    (if (not (file-exists-p sr-mirror-home))
        (make-directory sr-mirror-home))

    (setq base (sr-mirror-mangle path))
    (setq mirror (concat sr-mirror-home base))
    (setq overlay (concat sr-mirror-home "." base))
    (if (not (file-directory-p mirror))
        (progn
          (make-directory mirror)
          (make-directory overlay)
          (setq command (concat "cd ~; funionfs " overlay " " mirror " -o dirs=" virtual "=ro"))
          (shell-command-to-string command)))
    (sr-goto-dir mirror)
    t ))

(defun sr-mirror-close ()
  "Destroys  the  current mirror area by unmounting and deleting the directories
  it was built upon. Tries to automatically repack the mirror and substitute the
  original  archive  with  a  new  one  containing the modifications made to the
  mirror."
  (interactive)
  (if (null sr-mirror-home)
      (error (concat "Sunrise: sorry, can't mirror " (dired-get-filename)))

    (let ((here (dired-current-directory))
          (pos) (mirror) (overlay))
      (if (sr-overlapping-paths-p sr-mirror-home here)
          (progn
            (setq pos (string-match "\\(?:/\\|$\\)" here (length sr-mirror-home)))
            (setq mirror (substring here (length sr-mirror-home) pos))
            (setq overlay (concat "." mirror ))
            (sr-follow-file (sr-mirror-demangle mirror))
            (sr-mirror-commit mirror overlay)
            (sr-mirror-unmount mirror overlay))
        (error (concat "Sunrise: sorry, that's not a mirror area: " here)))

      (if (null (directory-files sr-mirror-home nil "^[^.]"))
          (sr-mirror-disable))

      t)))

(defun sr-mirror-commit (mirror overlay)
  "Commits  all  modifications  made  to  the  given mirror in the given overlay
  directory by replacing the mirrored archive with a  new  one  built  with  the
  current  contents of the mirror. Keeps a backup of the original archive if the
  sr-mirror-backup variable is not nil (as set by default)."
  (if (and (sr-mirror-files (concat sr-mirror-home overlay))
           (y-or-n-p "Sunrise: commit changes in mirror? "))
      (condition-case err
          (let ((repacked (sr-mirror-repack mirror))
                (target (dired-get-filename)))
            (if sr-mirror-keep-backups
                (rename-file target (concat target ".bak") 1))
            (rename-file repacked (dired-current-directory) t))
        (error
         (progn
           (setq err (second err))
           (if (not (yes-or-no-p (concat err ". OK to continue? ")))
               (error err)))))))

(defun sr-mirror-unmount (mirror overlay)
  "Unmounts  and  deletes  all directories used for mirroring a given compressed
  archive."
  (let* ((command (concat "fusermount -u " sr-mirror-home mirror))
         (err (shell-command-to-string command)))
    (if (or (null err) (string= err ""))
        (progn
          (dired-delete-file (concat sr-mirror-home mirror) 'always)
          (dired-delete-file (concat sr-mirror-home overlay) 'always)
          (sr-revert-buffer))
      (error (concat "Sunrise: Error unmounting mirror: " err)))))

(defun sr-mirror-toggle ()
  "Opens  a new mirror area or destroys the current one, depending on the actual
  context."
  (interactive)
  (let ((open-ok) (close-ok) (err-msg))
    (condition-case err1
        (setq open-ok (sr-mirror-open))
      (error (condition-case err2
                 (progn
                   (setq close-ok (sr-mirror-close))
                   (setq err-msg (second err1)))
               (error
                  (setq err-msg (second err2))) )) )
    (if (and (not open-ok) (not close-ok))
        (error err-msg))))

(defun sr-mirror-repack (mirror)
  "Tries  to repack the given mirror. On success returns a string containing the
  full path to the newly packed archive, on failure throws an error."
  (let* ((target-home (concat sr-mirror-home ".repacked/"))
         (target (replace-regexp-in-string
                  "/?$" ""
                  (car (last (split-string mirror "+")))))
         (files (directory-files (concat sr-mirror-home mirror)))
         (command (assoc-default mirror sr-mirror-pack-commands-alist 'string-match)))

    (if (null command)
        (error (concat "Sunrise: sorry, don't know how to repack " mirror)))

    (if (not (file-exists-p target-home))
        (make-directory target-home))
    (setq target (concat target-home target))
    (setq command (replace-regexp-in-string "%f" target command))
    (setq command (concat "cd " sr-mirror-home mirror "; " command))
    (shell-command-to-string command)
    target))

(defun sr-mirror-mangle (path)
  "Transforms  the  given  filesystem  path  into  a  string  that  can  be used
  internally as the name of a new mirror area."
  (if (equal ?/ (string-to-char path))
      (setq path (substring path 1)))
  (replace-regexp-in-string
   "/" "+"
   (replace-regexp-in-string "\\+" "{+}" path)))

(defun sr-mirror-demangle (path)
  "Does  the  opposite of sr-mirror-mangle, ie. transforms the given mirror area
  name into a regular filesystem path."
  (concat "/"
          (replace-regexp-in-string
           "{\\+}" "+" (replace-regexp-in-string
                        "\\+\\([^}]\\)" "/\\1" path))))

(defun sr-mirror-files (directory)
  "Returns a list with the names of files and directories that can be considered
  as mirror modifications inside an overlay directory."
  (if (not (file-directory-p directory))
      (ignore)
    (let ((files (directory-files directory)))
      (mapc (lambda (x) (setq files (delete x files)))
              '("." ".." "._funionfs_control~"))
      files)))

(defun sr-mirror-overlay-redir (dirname &optional force-root)
  "Analyses  the  given  directory  path  and rewrites it (if necessary) to play
  nicely with the mirror fs the given path  belongs  to.  If  the  path  is  not
  inside any mirror fs then it is returned unmodified."
  (if (null sr-avfs-root)
      dirname
    (let ((xpdir (expand-file-name dirname))
          (mirror) (pos) (target))
      (if (sr-overlapping-paths-p sr-mirror-home xpdir)
          (progn
            (setq mirror (substring xpdir (length sr-mirror-home)))
            (setq pos (string-match "/\\|$" mirror))
            (if pos
                (progn
                  (setq target (replace-regexp-in-string "^/" "" (substring mirror pos)))
                  (setq mirror (substring mirror 0 pos))))
            (if (and target
                     (or (> (length target) 0) force-root)
                     (not (equal ?. (string-to-char mirror)))) 
                (concat sr-mirror-home "." mirror "/" target)
              dirname))
        dirname))))

;; This redirects all sr-copy operations to the right path under the overlay
;; directory:
(defadvice sr-copy-files
  (around sr-mirror-advice-sr-copy-files
          (file-path-list target-dir &optional do-overwrite))
  (let ((orig target-dir))
    (setq target-dir (sr-mirror-overlay-redir target-dir t))
    (if (> (length target-dir) (length orig))
        (make-directory target-dir))
    ad-do-it))

;; This redirects directory creation operations to the right path under the
;; overlay directory:
(defadvice make-directory
  (around sr-mirror-advice-make-directory (dirname &optional parents))
  (setq dirname (sr-mirror-overlay-redir dirname))
  (setq parents t)
  ad-do-it)

;; This creates all the subdirectories needed (and sets their permissions) in
;; order to make possible the redirection of buffer saving operations to the
;; right path under the overlay directory:
(defadvice save-buffer
  (around sr-mirror-advice-save-buffer (&optional args))
  (let* ((orig (buffer-file-name))
         (target (sr-mirror-overlay-redir orig)))
    (if (> (length target) (length orig))
        (let ((default-directory "~/")
              (target-dir (file-name-directory target)))
          (make-directory target-dir)
          (shell-command-to-string (concat dired-chmod-program " a+x " target-dir))
          (write-file target nil))
      ad-do-it)))

;; This toggles the read-only flag in all buffers opened inside a mirror area,
;; so they are always writeable by default:
(defun sr-mirror-toggle-read-only ()
  (if sr-mirror-home
      (let* ((orig (buffer-file-name))
             (target (sr-mirror-overlay-redir orig)))
        (if (> (length target) (length orig))
            (toggle-read-only -1)))))
(add-hook 'find-file-hook 'sr-mirror-toggle-read-only)

(provide 'sunrise-x-mirror)
