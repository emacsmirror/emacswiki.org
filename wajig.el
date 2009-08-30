;;; wajig.el --- an interface for wajig

;; Copyright (C) 2005, 2006, 2007 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.53
;; Url: http://williamxu.net9.org/ref/wajig.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; "Wajig is an interface to many Debian administrative tasks. Written
;; in Python, wajig uses traditional Debian administration and user
;; tools including apt-get, dpkg, apt-cache, wget, and others. It is
;; intended to unify and simplify common administrative tasks." See
;; http://www.togaware.com/wajig/ for more infomation.

;; I write this package as a wrapper of wajig for Emacs. I love living
;; within Emacs. :-)

;; Put this file into your load-path and the following into your ~/.emacs:

;;    (autoload 'wajig "wajig"
;;             "Create a *wajig* buffer." t)

;; Then, simply run `M-x wajig'.

;; Features
;; --------

;; - Ease of Typing

;;   Most of wajig commands can be invoked by typing very few
;;   keys. e.g., in *wajig* buffer, `o' for `wajig show'.

;; - `ido-completing-read' Support

;;   With the power of `ido-completing-read', you don't have to remember
;;   the full name of a package. It is also an ease of typing.

;; - Output Highlighting

;;   Highlight command outputs. Make them look nicer.

;; The Aim
;; -------

;; At first, i thought i would write an interface for wajig in
;; Emacs. Then i found wajig also supports some other system operations
;; besides deb control, such as start/stop system services. Hence, i
;; plan to add more system administrations, in the hope of reducing
;; typing in a shell as much as possible.

;; Important Notes
;; ---------------

;; 1. It's currently expected to run all the commands in a
;;    non-interactive style. So do have the following settings:
;;
;;     - `dpkg-reconfigure debconf', and choose non-interactive
;;     - add the following into /etc/apt/apt.conf:
;;
;;         APT {
;;              Get {
;;                   Assume-Yes "true";
;;              };
;;         };

;; 2. I only tested wajig commands that i tend to use often. Far from
;;    all of the wajig commands! Hence, possibly some functions won't
;;    work properly. Be careful and let me know if you find any!

;;; Change Log:

;; v 0.1 [2005/09/15 17:19:36] Initial version

;;; Todo:

;; - Make output with percentage number(e.g., `wajig update') display
;;   nicer.

;;; Code:

;;; User Customization

(defgroup wajig nil
  "An interface for wajig in debian."
  :group 'wajig)

(defcustom wajig-mode-hook nil
  "Normal hook run after entering wajig mode."
  :type 'hook
  :group 'wajig)

(defcustom wajig-source-download-dir "~/download"
  "Directory for saving source downloads."
  :type 'string
  :group 'wajig)

(defcustom wajig-bin-path '("~/bin" "/usr/local/bin" "/usr/bin" "/bin"
                            "/usr/games" "/usr/bin/X11" "/usr/games")
  "User's bin path."
  :type 'list
  :group 'wajig)

(defcustom wajig-sbin-path '("/usr/local/sbin" "/usr/sbin" "/sbin")
  "Sbin path."
  :type 'list
  :group 'wajig)

(defcustom wajig-frequent-commands '("ps -ef")
  "Frequently running commands. See `wajig-run-frequent-commands'."
  :type 'list
  :group 'wajig)

(defcustom wajig-cache-filename "~/.wajig-cache.el"
  "Wajig cache file."
  :type 'string
  :group 'wajig)

 
;;; Wajig Commands

(defvar wajig-commands-string ""
  "Output from `wajig commands'.")

(defvar wajig-process nil
  "Wajig command process.")

(defvar wajig-running nil
  "Is the wajig process running?")

(defvar wajig-installed-pkgs nil
  "Installed packages on the system.
You can run `wajig-update-cache' to keep update.")

(defvar wajig-daemons nil
  "Existing daemons' list.
You can run `wajig-update-cache' to keep update.")

(defvar wajig-daemons-command-list
  '(wajig-start wajig-stop wajig-restart wajig-reload)
  "Wajig command list for controling system daemons.")

(defvar wajig-command-path-alist nil
  "An assoc list of command and its full path.
You could run `wajig-update-cache' to keep update.
e.g., '((\"ls\" . \"/bin/ls\")). ")

(defun wajig-update-cache ()
  "Update wajig cache saved in `wajig-cache-filename'."
  (interactive)
  (message "Updating wajig cache...")
  (setq wajig-commands-string
        (shell-command-to-string "sudo wajig list-commands"))
  (wajig-update-installed-pkgs)
  (wajig-update-daemons)
  (wajig-update-command-path-alist)
  (with-temp-buffer
    (insert ";; automatically generated by wajig.el\n")
    (insert (format "
\(setq wajig-installed-pkgs '%S
      wajig-command-path-alist '%S
      wajig-daemons '%S
      wajig-commands-string '%S)
"
                    wajig-installed-pkgs
                    wajig-command-path-alist
                    wajig-daemons
                    wajig-commands-string))
    (write-region (point-min) (point-max) wajig-cache-filename))
  (message "Updating wajig cache...done"))

(defun wajig-update-installed-pkgs ()
  "Update `wajig-installed-pkgs'."
  (setq wajig-installed-pkgs
	(split-string
	 (shell-command-to-string "sudo wajig list-installed"))))

(defun wajig-update-daemons ()
  "Update daemons' list."
  (setq wajig-daemons
	(split-string
	 (replace-regexp-in-string
	  "^Found.*\n" ""
	  (shell-command-to-string "sudo wajig list-daemons")))))

(defun wajig-update-command-path-alist ()
  "Update `wajig-command-path-alist' immediately."
  (setq wajig-command-path-alist '())
  ;; bin path
  (mapc
   (lambda (path)
     (mapc
      (lambda (cmd)
        (unless (assoc cmd wajig-command-path-alist)
          (setq wajig-command-path-alist
                (append wajig-command-path-alist
                        `((,(file-name-nondirectory cmd) . ,cmd))))))
      (split-string
       (shell-command-to-string
        (concat "find " path " -maxdepth 1 -type f -perm -u+x")))))
   wajig-bin-path)
  ;; sbin path
  (mapc
   (lambda (path)
     (mapc
      (lambda (cmd)
        (unless (assoc cmd wajig-command-path-alist)
          (setq wajig-command-path-alist
                (append
                 wajig-command-path-alist
                 `((,(file-name-nondirectory cmd)
                    .
                    ,(concat "sudo " cmd)))))))
      (split-string
       (shell-command-to-string
        (concat "find " path " -maxdepth 1 -type f -perm -u+x")))))
   wajig-sbin-path)
  wajig-command-path-alist)

;; initial variables
(if (file-readable-p wajig-cache-filename)
    (load-file wajig-cache-filename)
  (wajig-update-cache))

(defun wajig-process-sentinel (process event)
  "Set buffer read-only after a wajig command finishes."
  (setq wajig-running nil)
  (save-excursion
    (with-current-buffer (get-buffer "*wajig*")
      (let ((inhibit-read-only t))
	(cond
	 ((eq (process-status process) 'exit)
	  (goto-char (point-max))
	  (insert "------------- done --------------\n"))
	 ((eq (process-status process) 'signal)
	  (message "wajig process killed")))))))

(defun wajig-process-filter (process output)
  "Filter wajig command outputs."
  (with-current-buffer (process-buffer process)
    (let ((moving (= (point) (process-mark process)))
	  (inhibit-read-only t)
	  (percentage-match "[0-9]\\{1,3\\}%"))
      (save-excursion
	(goto-char (process-mark process))
	(setq output
              (replace-regexp-in-string "\r" "\n" output))
	;; make percentage output nicer
;;         (cond ((string-match percentage-match output)
;;                (message "wajig: %s" output))
;;               ((string-match "^\\ +$\\|^\n$" output)
;;                nil)
;;               (t
;;                (forward-line 0)
;; ;;                (insert output)))
        (insert output)
	(set-marker (process-mark process) (point)))
      (and moving (goto-char (process-mark process))))))

(defun wajig-kill ()
  "Kill wajig process."
  (interactive)
  (when wajig-process
    (unless (eq (process-status wajig-process) 'exit)
      (delete-process wajig-process))
    (setq wajig-running nil)))

(defmacro define-wajig-command (command &optional arglist)
  "Define a new wajig command. COMMAND is one of wajig commands,
such as help, update. Optional ARGLIST is (pkg). e.g.,

    (define-wajig-command help)
    (define-wajig-command show (pkg))

pkg is the package name to operate on."
  (when (symbolp command)
    (setq command (symbol-name command)))
  (let* ((wajig-command (intern (format "wajig-%s" command)))
	 (docstring
	  (progn		       ; show help from `wajig commands'
	    (string-match (format "^ %s.*" command)
			  wajig-commands-string)
	    (match-string 0 wajig-commands-string)))
	 (interactive
	  (if arglist
	      (setq interactive
		    `(interactive
		      (list
		       (wajig-completing-read
			,(format "$ sudo wajig %s " command)
			,(if (memq wajig-command
				  wajig-daemons-command-list)
			    'wajig-daemons
			  'wajig-installed-pkgs)))))
	    '(interactive))))
    `(defun ,wajig-command ,arglist
       ,docstring
       ,interactive
       (let ((inhibit-read-only t))
	 (wajig)
	 (if wajig-running
	     (error "Wajig process already exists")
	   (erase-buffer)
	   (setq wajig-running t)
	   (setq wajig-process
		 ,(if arglist
		      `(start-process "wajig" "*wajig*"
				      "sudo" "wajig" ,command ,(car arglist))
		    `(start-process "wajig" "*wajig*"
				    "sudo" "wajig" ,command)))
	   (set-process-filter wajig-process 'wajig-process-filter)
	   (set-process-sentinel wajig-process 'wajig-process-sentinel))))))

(defun wajig-do (command-string)
  "Run COMMAND-STRING, e.g., '(\"cmd\" \"arg1\" ...) in *wajig*
buffer."
  (let ((inhibit-read-only t))
    (wajig)
    (erase-buffer)
    (if wajig-running
	(error "Wajig process already exists")
      (setq wajig-running t)
      (setq wajig-process
	    (apply 'start-process "wajig" "*wajig*" command-string))
      (set-process-filter wajig-process 'wajig-process-filter)
      (set-process-sentinel wajig-process 'wajig-process-sentinel))))

;; Compatibility
;; -------------

(defalias 'wajig-completing-read
  (if (and (fboundp 'ido-completing-read)
	   ido-mode)
      'ido-completing-read		; added in Emacs 22
    'completing-read))

;; Wajig Commands Without Arguments
;; --------------------------------

(define-wajig-command auto-clean)
(define-wajig-command auto-download)
(define-wajig-command clean)
(define-wajig-command commands)
(define-wajig-command daily-upgrade)
(define-wajig-command describe-new)
(define-wajig-command detail-new)
(define-wajig-command dist-upgrade)
(define-wajig-command docs)
(define-wajig-command file-download)
(define-wajig-command file-remove)
(define-wajig-command fix-configure)
(define-wajig-command fix-install)
(define-wajig-command fix-missing)
(define-wajig-command init)
(define-wajig-command integrity)
(define-wajig-command help)
(define-wajig-command large)
(define-wajig-command last-update)
(define-wajig-command list)
(define-wajig-command list-all)
(define-wajig-command list-cache)
(define-wajig-command list-commands)
(define-wajig-command list-daemons)
(define-wajig-command list-hold)
(define-wajig-command list-installed)
(define-wajig-command list-log)
(define-wajig-command list-orphans)
(define-wajig-command local-dist-upgrade)
(define-wajig-command local-upgrade)
(define-wajig-command non-free)
(define-wajig-command orphans)
(define-wajig-command reset)
(define-wajig-command search-apt)
(define-wajig-command setup)
(define-wajig-command showdistupgrade)
(define-wajig-command showinstall)
(define-wajig-command showremove)
(define-wajig-command showupgrade)
(define-wajig-command size)
(define-wajig-command sizes)
(define-wajig-command snapshot)
(define-wajig-command tasksel)
(define-wajig-command toupgrade)
(define-wajig-command update)

;; Wajig Commands With One Argument
;; --------------------------------

(define-wajig-command auto-install (pkg))
(define-wajig-command available (pkg))
(define-wajig-command build (pkg))
(define-wajig-command build-depend (pkg))
(define-wajig-command changelog (pkg))
(define-wajig-command dependents (pkg))
(define-wajig-command describe (pkg))
(define-wajig-command detail (pkg))
(define-wajig-command download (pkg))
(define-wajig-command file-install (pkg))
(define-wajig-command find-file (pkg))
(define-wajig-command find-pkg (pkg))
(define-wajig-command force (pkg))
(define-wajig-command hold (pkg))
(define-wajig-command install (pkg))
(define-wajig-command install/dist (pkg))
(define-wajig-command list-files (pkg))
(define-wajig-command list-names (pkg))
(define-wajig-command list-scripts (pkg))
(define-wajig-command list-section (pkg))
(define-wajig-command list-status (pkg))
(define-wajig-command list-wide (pkg))
(define-wajig-command move (pkg))
(define-wajig-command news (pkg))
(define-wajig-command package (pkg))
(define-wajig-command policy (pkg))
(define-wajig-command purge (pkg))
(define-wajig-command purge-depend (pkg))
(define-wajig-command purge-orphans (pkg))
(define-wajig-command readme (pkg))
(define-wajig-command recommended (pkg))
(define-wajig-command reconfigure (pkg))
(define-wajig-command recursive (pkg))
(define-wajig-command reinstall (pkg))
(define-wajig-command reload (pkg))
(define-wajig-command remove (pkg))
(define-wajig-command remove-depend (pkg))
(define-wajig-command remove-orphans (pkg))
(define-wajig-command repackage (pkg))
(define-wajig-command restart (pkg))
(define-wajig-command rpm2deb (pkg))
(define-wajig-command rpminstall (pkg))
(define-wajig-command rpmtodeb (pkg))
(define-wajig-command search (pkg))
(define-wajig-command show (pkg))
(define-wajig-command source (pkg))
(define-wajig-command start (pkg))
(define-wajig-command status (pkg))
(define-wajig-command status-match (pkg))
(define-wajig-command status-search (pkg))
(define-wajig-command stop (pkg))
(define-wajig-command suggested (pkg))
(define-wajig-command unhold (pkg))
(define-wajig-command unofficial (pkg))
(define-wajig-command update-alts (pkg))
(define-wajig-command update-pci-ids (pkg))
(define-wajig-command update-usb-ids (pkg))
(define-wajig-command upgrade (pkg))
(define-wajig-command versions (pkg))
(define-wajig-command whatis (pkg))
(define-wajig-command whichpkg (pkg))

;; Network Admin
;; -------------

(defun wajig-network-ifconfig ()
  "$ sudo ifconfig    Display status of active interfaces"
  (interactive)
  (wajig-do '("sudo" "ifconfig")))

(defun wajig-network-netstat (option)
  "$ netstat OPTION

OPTION could be:
    -r        Print routing tables
    -na, -a   Print network connections"
  (interactive
   (list
    (ido-completing-read
     "$ netstat "
     '("-r" "-na" "-a"))))
  (wajig-do `("netstat" ,option)))

(defun wajig-network-ping (host)
  "$ ping HOST"
  (interactive "s$ ping ")
  (wajig-do `("ping" ,host)))

(defun wajig-network-traceroute (host)
  "$ traceroute HOST"
  (interactive "s$ traceroute ")
  (wajig-do `("traceroute" ,host)))

(defun wajig-network-nmap (host-or-network)
  "$ nmap -A -T4 HOST-OR-NETWORK"
  (interactive "s$ nmap -A -T4 ")
  (wajig-do `("nmap" "-A" "-T4" ,host-or-network)))

;; Misc
;; ----

(defun wajig-mode-help ()
  "Help page for wajig-mode."
  (interactive)
  (message "For a list of available commands, press h or c."))

(defun wajig-edit-sources ()
  "Edit /etc/apt/sources.list using `tramp'."
  (interactive)
  (find-file "/sudo::/etc/apt/sources.list"))

(defun wajig-search-by-name (pkg)
  "Run `wajig search -n pkg' or `apt-cache search -n pkg'."
  (interactive
   (list
    (wajig-completing-read
     "$ apt-cache search -n "
     wajig-installed-pkgs)))
  (wajig-do `("apt-cache" "search" ,pkg "-n")))

(defun wajig-show-at-point ()
  "Run `wajig show' on current word(pkg name)."
  (interactive)
  (wajig-show (current-word)))

(defun wajig-manually (command-str)
  "Run special commands manually."
  (interactive
   (list
    (ido-completing-read
     "$ "
     (mapcar
      (lambda (command-path)
        (car command-path))
      wajig-command-path-alist))))
  (when (assoc command-str wajig-command-path-alist)
    (setq command-str
          (cdr (assoc command-str wajig-command-path-alist))))
  (wajig-do (split-string command-str)))

(defun wajig-source (pkg)
  "Run \"cd `wajig-source-download-dir' && wajig source pkg\""
  (interactive
   (list
    (ido-completing-read
     (format "$ cd %s && wajig source " wajig-source-download-dir)
     wajig-installed-pkgs)))
  (let ((old-dir default-directory))
    (cd wajig-source-download-dir)
    (wajig-do `("wajig" "source" ,pkg))
    (cd old-dir)))

(defun wajig-source-download (pkg)
  "Run \"cd `wajig-source-download-dir' && wajig source -d pkg\""
  (interactive
   (list
    (ido-completing-read
     (format "$ cd %s && wajig source -d " wajig-source-download-dir)
     wajig-installed-pkgs)))
  (let ((old-dir default-directory))
    (cd wajig-source-download-dir)
    (wajig-do `("wajig" "source" ,pkg "-d"))
    (cd old-dir)))

(defun wajig-bug (pkg)
  "Run \"wajig bug pkg\". (Should run as non-root?)"
  (interactive
   (list
    (ido-completing-read
     "$ wajig bug "
     wajig-installed-pkgs)))
  (wajig-do `("wajig" "bug" ,pkg)))

(defun wajig-query-installed ()
  "$ dpkg -l	list packages concisely"
  (interactive)
  (wajig-do '("dpkg" "-l")))

(defun wajig-upgrade-all ()
  "$ apt-get upgrade	upgrade all packages"
  (interactive)
  (wajig-do '("sudo" "apt-get" "upgrade")))

(defun wajig-run-frequent-commands (command-str)
  "Run frequent commands, such \"ps -ef\". See
`wajig-frequent-commands'."
  (interactive
   (list
    (ido-completing-read
     "$ "
     wajig-frequent-commands)))
  (wajig-do (split-string command-str)))

(defun wajig-install-at-point ()
  "Run `wajig install' on current word(pkg name)."
  (interactive)
  (wajig-install (current-word)))

 
;;; Wajig Mode

(defvar wajig-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" 'wajig-auto-clean)
    (define-key map "c" 'wajig-commands)
    (define-key map "d" 'wajig-source)
    (define-key map "D" 'wajig-source-download)
    (define-key map "E" 'wajig-edit-sources)
    (define-key map "h" 'wajig-help)
    (define-key map "i" 'wajig-install)
    (define-key map "I" 'wajig-install-at-point)
    (define-key map "K" 'wajig-kill)
    (define-key map "l" 'wajig-list-files)
    (define-key map "m" 'wajig-manually)
    (define-key map "o" 'wajig-show)
    (define-key map "R" 'wajig-remove)
    (define-key map "S" 'wajig-search)
    (define-key map "r" 'wajig-run-frequent-commands)
    (define-key map "s" 'wajig-search-by-name)
    (define-key map "t" 'wajig-toupgrade)
    (define-key map "u" 'wajig-update)
    (define-key map "U" 'wajig-upgrade)
    (define-key map "
" 'wajig-show-at-point)
    (define-key map "w" 'wajig-whichpkg)
    ;; query status
    (define-key map (kbd "Q a") 'wajig-available)
    (define-key map (kbd "Q b") 'wajig-bug)
    (define-key map (kbd "Q c") 'wajig-changelog)
    (define-key map (kbd "Q i") 'wajig-query-installed)
    (define-key map (kbd "Q l") 'wajig-list-log)
    (define-key map (kbd "Q N") 'wajig-non-free)
    (define-key map (kbd "Q n") 'wajig-news)
    (define-key map (kbd "Q p") 'wajig-policy)
    (define-key map (kbd "Q r") 'wajig-readme)
    (define-key map (kbd "Q S") 'wajig-list-scripts)
    (define-key map (kbd "Q s") 'wajig-status)
    ;; file operation
    (define-key map (kbd "F i") 'wajig-file-install)
    ;; services control
    (define-key map (kbd "V x") 'wajig-start)
    (define-key map (kbd "V v") 'wajig-stop)
    (define-key map (kbd "V r") 'wajig-restart)
    (define-key map (kbd "V l") 'wajig-list-daemons)
    ;; cursor movement
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    ;; network admin
    (define-key map (kbd "N i") 'wajig-network-ifconfig)
    (define-key map (kbd "N n") 'wajig-network-netstat)
    (define-key map (kbd "N p") 'wajig-network-ping)
    (define-key map (kbd "N t") 'wajig-network-traceroute)
    (define-key map (kbd "N m") 'wajig-network-nmap)
    map)
    "Keymap for `wajig-mode'.")

(defvar wajig-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st)
  "Syntax table used while in `wajig-mode'.")

(defvar wajig-font-lock-keywords
  `(("^Package:\\(.*\\)"
     (1 font-lock-function-name-face nil t))
    ("^Conflicts:"
     (0 font-lock-warning-face nil t))
    ("^Description:\\(.*\n\\)"
     (1 font-lock-function-name-face nil t))
    (,(concat
       "^\\("
       (regexp-opt
	'("Package" "Priority" "Section" "Installed-Size" "Maintainer"
	  "Architecture" "Version" "Depends" "Suggests" "Filename"
	  "Size" "MD5sum" "Description" "Tag" "Status" "Replaces"
	  "Conffiles" "Source" "Provides" "Pre-Depends" "Recommends"
          "SHA1" "SHA256" "Enhances" "Config-Version" "Task"))
       "\\):")
     (0 font-lock-keyword-face t t)))
  "Keywords to highlight in wajig mode.")

(define-derived-mode wajig-mode nil "Wajig"
  "Major mode for wajig in debian.
\\{wajig-mode-map}"
  (wajig-mode-help)
  (set-syntax-table wajig-mode-syntax-table)
  (setq font-lock-defaults '(wajig-font-lock-keywords))
  ;; (wajig-update-cache)
  (run-hooks 'wajig-mode-hook))

;;;###autoload
(defun wajig ()
  "Create a *wajig* buffer."
  (interactive)
  (let ((wajig-exist-p (get-buffer "*wajig*")))
    (switch-to-buffer "*wajig*")
    (unless wajig-exist-p
      (wajig-mode))))

(provide 'wajig)

;;; wajig.el ends here
