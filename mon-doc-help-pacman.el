;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -*- mode: EMACS-LISP; -*-
;;; this is mon-doc-help-pacman.el
;;; ================================================================
;;; DESCRIPTION:
;;; Provides help-mode sytle access to documentation of Arch distribution for
;;; GNU/Linux. This functionality was previously in mon-doc-help-utils but as it
;;; isn't particularly relevant to emacs it is better off segregated in its own
;;; package.
;;;
;;; FUNCTIONS:►►►
;;; `mon-help-pacman-Q', `mon-help-pacman-S' `mon-help-pacman-commands'
;;; FUNCTIONS:◄◄◄
;;;
;;; MACROS:
;;;
;;; METHODS:
;;;
;;; CLASSES:
;;;
;;; CONSTANTS:
;;;
;;; VARIABLES:
;;; `*regexp-clean-pacman-Q*', `*regexp-clean-pacman-S*'
;;;
;;; ALIASED/ADVISED/SUBST'D:
;;;
;;; DEPRECATED:
;;;
;;; RENAMED:
;;;
;;; MOVED:
;;;
;;; TODO:
;;;
;;; NOTES:
;;;
;;; SNIPPETS:
;;;
;;; REQUIRES:
;;; mon-doc-help-utils.el
;;;
;;; THIRD-PARTY-CODE:
;;;
;;; Strip or reformat with regexps on these commonly employed "TAGS":
;;;
;;; TAGS-APPEARING-IN-COMMENTS:
;;; :CLEANUP :CLOSE :COURTESY :CREATED :DATE :EMACS-WIKI :EVAL-BELOW-TO-TEST
;;; :FIXES :FIXME :HIS :IF-NOT-FEATURE-P :KEYWORD-REGEXPS-IN
;;; :LOAD-SPECIFIC-PROCEDURES :MODIFICATIONS :RENAMED :SEE-BELOW :SUBJECT :TODO
;;; :TEST-ME :UNCOMMENT-BELOW-TO-TEST :VERSION :WAS
;;;
;;; TAGS-APPEARING-IN-DOCSTRINGS:
;;; :ALIASED-BY :CALLED-BY :EXAMPLE :FACE-DEFINED-IN :FACE-DOCUMENTED-IN
;;; :FILE :IDIOM :NOTE :SEE :SEE-ALSO :SOURCE :USED-BY
;;;
;;; AUTHOR: MON KEY
;;; MAINTAINER: MON KEY
;;;
;;; PUBLIC-LINK: (URL `http://www.emacswiki.org/emacs/mon-doc-help-pacman.el')
;;; FIRST-PUBLISHED: <Timestamp: #{2009-12-08T13:58:20-05:00Z}#{09502} - by MON>
;;; 
;;; FILE-CREATED:
;;; <Timestamp: #{2009-12-08T13:16:32-05:00Z}#{09502} - by MON KEY>
;;; ================================================================
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;;; Floor, Boston, MA 02110-1301, USA.
;;; ================================================================
;;; Permission is granted to copy, distribute and/or modify this
;;; document under the terms of the GNU Free Documentation License,
;;; Version 1.3 or any later version published by the Free Software
;;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;;; and no Back-Cover Texts. A copy of the license is included in
;;; the section entitled "GNU Free Documentation License".
;;; A copy of the license is also available from the Free Software
;;; Foundation Web site at:
;;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ================================================================
;;; Copyright © 2009 MON KEY 
;;; ==============================
;;; CODE:

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-02T20:58:01-05:00Z}#{09493} - by MON>
(defvar *regexp-clean-pacman-Q*
  '(("-c" "--changelog" "View the changelog of a package.")
    ("-g" "--groups" "View all members of a package group.")
    ("-i" "--info" "View package information \(-ii for backup files\).")
    ("-k" "--check" "Check that the files owned by the package\(s\) are present.")
    ("-l" "--list" "List the contents of the queried package.")
    ("-q" "--quiet" "Show less information for query and search.")
    ("-v" "--verbose" "Be verbose.")
    ("--noconfirm" nil "Do not ask for any confirmation.")
    ("--noprogressbar" nil "Do not show a progress bar when downloading files.")
    ("--noscriptlet" nil "Do not execute the install scriptlet if one exists.")
    ("--debug" nil "Display debug messages.")
    ;; :HAS-FILTER
    ("-m" "--foreign" "List installed packages not found in sync db\(s\) [filter].")
    ("-t" "--unrequired" "List packages not required by any package [filter].")
    ("-u" "--upgrades" "List outdated packages [filter].")
    ("-d" "--deps" "List packages installed as dependencies [filter].")
    ("-e" "--explicit" "List packages explicitly installed [filter].")
    ;; :HAS-ARG
    ("-o" "--owns" "<FILE> Query the package that owns <file>.")
    ("-p" "--file" "<PACKAGE> Query a package file instead of the database.")
    ("-s" "--search" "<REGEX> Search locally-installed packages for matching strings.")
    ("-r" "--root" "<PATH> Set an alternate installation root.")
    ("-b" "--dbpath" "<PATH> Set an alternate database location.")
    ("--config" nil "<PATH> Set an alternate configuration file.")
    ("--logfile" nil "<PATH> Set an alternate log file.")
    ("--cachedir" nil "<DIR> set an alternate package cache location."))
  "*A list of short and long flags Flags Arch's `pacman -Q' command.
:SEE-ALSO `mon-help-pacman-Q', `*regexp-clean-pacman-S*', `mon-help-pacman-commands'.\n►►►")
;;
;;; :TEST-ME *regexp-clean-pacman-Q*
;;
;;; (progn (makunbound '*regexp-clean-pacman-Q*)(unintern '*regexp-clean-pacman-Q*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-02T20:55:45-05:00Z}#{09493} - by MON>
(defun mon-help-pacman-Q (&optional insertp intrp)
  ""
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-pacman-Q :insertp t)
      (message "Pass non-nil for optional arg INTRP")))
;;
;; Now, tack the var list onto the docstring.
(eval-when-compile
  (let (put-flags)
    (setq put-flags "")
    (mapc #'(lambda (x) 
              (setq put-flags 
                    (concat 
                     (format "`%s' %s%s\n" 
                             (car x) 
                             (if (cadr x) (concat "`" (cadr x) "'") "")
                             (concat " ;" (caddr x)))
                     put-flags)))
          (reverse (symbol-value '*regexp-clean-pacman-Q*)))
    (setq put-flags
          (with-temp-buffer 
            (insert put-flags)
            (while (search-backward-regexp "' +;" nil t)
              (skip-chars-forward "^;")
              (indent-to 20))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (mon-help-put-var-doc-val->func 
     'put-flags  
     'mon-help-pacman-Q
     (concat 
      "Flags for use with Arch's `pacman -Q'.\n\n"
      ":USAGE  pacman {-Q --query} [options] [package(s)]\n\n"
      ";; :PACMAN-Q-OPTIONS:\n")
     nil 
     (concat 
      "\n\n;; :QUERY-PACKAGE-DATABASE\n\n"
      "This operation allows you to view installed packages and their files, as\n"
      "well as meta-information about individual packages. Queries can interrogate\n"
      "dependencies, conflicts, install date, build date, size etc. and can be run\n"
      "against the local package database or can be used on individual .tar.gz\n"
      "packages. In the first case, if no package names are provided in the command\n"
      "line, all installed packages will be queried. Additionally, various filters can\n"
      "be applied on the package list.\n\n"
      ";; :QUERY-OPTIONS\n\n"
      "-c, --changelog\n\n"
      "    View the ChangeLog of a package. Not every package will provide one but it\n"
      "    will be shown if available.\n\n"
      "-d, --deps\n\n"
      "    Restrict or filter output to packages installed as dependencies.  This\n"
      "    option can be combined with -t for listing real orphans- packages that were\n"
      "    installed as dependencies but are no longer required by any installed\n"
      "    package. \n"
      "    :NOTE \(-Qdt is equivalent to the pacman 3.0.X -Qe option.\)\n\n"
      "-e, --explicit\n\n"
      "    Restrict or filter output to packages explicitly installed. This option can\n"
      "    be combined with -t to list top-level packages- those packages that were\n"
      "    explicitly installed but are not required by any other package. \n"
      "    :NOTE \(-Qet is equivalent to the pacman 2.9.X -Qe option.\)\n\n"
      "-g, --groups\n\n"
      "    Display all packages that are members of a named group. If a name is not\n"
      "    specified, list all grouped packages.\n\n"
      "-i, --info\n\n"
      "    Display information on a given package. The -p option can be used if\n"
      "    querying a package file instead of the local database. Passing two --info or\n"
      "    -i flags will also display the list of backup files and their modification\n"
      "    states.\n\n"
      "-k --check\n\n"
      "    Check that all files owned by the given package\(s\) are present on the\n"
      "    system. If packages are not specified or filter flags are not provided,\n"
      "    check all installed packages.\n\n"
      "-l, --list\n\n"
      "    List all files owned by a given package. Multiple packages can be specified\n"
      "    on the command line.\n\n"
      "-m, --foreign\n\n"
      "    Restrict or filter output to packages that were not found in the sync\n"
      "    database\(s\). Typically these are packages that were downloaded manually\n"
      "    and installed with --upgrade.\n\n"
      "-o, --owns <'file'>\n\n"
      "    Search for the package that owns file. The path can be relative or absolute.\n\n"
      "-p, --file\n\n"
      "    Signifies that the package supplied on the command line is a file and not an\n"
      "    entry in the database. The file will be decompressed and queried. This is\n"
      "    useful in combination with --info and --list.\n\n"
      "-q, --quiet\n\n"
      "    Show less information for certain query operations. This is useful when\n"
      "    pacman's output is processed in a script. Search will only show package\n"
      "    names and not version, group, and description information; owns will only\n"
      "    show package names instead of \"file is owned by pkg\" messages; group will\n"
      "    only show package names and omit group names; list will only show files and\n"
      "    omit package names; check will only show pairs of package names and missing\n"
      "    files; a bare query will only show package names rather than names and\n"
      "    versions.\n\n"
      "-s, --search <'regexp'>\n\n"
      "    This will search each locally-installed package for names or descriptions\n"
      "    that match regexp. When you include multiple search terms, only packages\n"
      "    with descriptions matching ALL of those terms will be returned.\n\n"
      "-t, --unrequired\n\n"
      "    Restrict or filter output to packages not required by any currently\n"
      "    installed package.\n\n"
      "-u, --upgrades\n\n"
      "    Restrict or filter output to packages that are out of date on the local\n"
      "    system. Only package versions are used to find outdated packages,\n"
      "    replacements are not checked here. This option works best if the sync\n"
      "    database is refreshed using -Sy.\n\n"
      ":SEE-ALSO `*regexp-clean-pacman-Q*', `mon-help-pacman-commands',\n"
      "`*regexp-clean-pacman-S*', `mon-help-pacman-S',.\n►►►"))))
;;
;;; :TEST-ME (mon-help-pacman-Q )
;;; :TEST-ME (mon-help-pacman-Q t)
;;; :TEST-ME (call-interactively 'mon-help-pacman-Q)
;;; :TEST-ME (describe-function 'mon-help-pacman-Q)
;;
;;; (progn (fmakunbound 'mon-help-pacman-Q) (unintern 'mon-help-pacman-Q))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-07T16:41:35-05:00Z}#{09501} - by MON KEY>
(defvar *regexp-clean-pacman-S*
  '(;; :SHROT-AND-LONG
    ("-c" "--clean"          "Remove old packages from cache directory \(-cc for all\).")
    ("-d" "--nodeps"         "Skip dependency checks.")
    ("-f" "--force"          "Force install, overwrite conflicting files.")
    ("-g" "--groups"         "View all members of a package group.")
    ("-i" "--info"           "View package information.")
    ("-p" "--print-uris"     "Print out URIs for given packages and their dependencies.")
    ("-u" "--sysupgrade"     "Upgrade installed packages \(-uu allows downgrade\).")
    ("-w" "--downloadonly"   "Download packages but do not install/upgrade anything.")
    ("-y" "--refresh"        "Download fresh package databases from the server.")
    ("-q" "--quiet"          "Show less information for query and search.")
    ("-v" "--verbose"        "Be verbose.")
    ;; :NO-SHORT
    ("--needed"        nil "Don't reinstall up to date packages.")
    ("--noconfirm"     nil "Do not ask for any confirmation.")
    ("--noprogressbar" nil "Do not show a progress bar when downloading files.")
    ("--noscriptlet"   nil "Do not execute the install scriptlet if one exists.")
    ("--debug"         nil "Display debug messages.")
    ("--asdeps"        nil "Install packages as non-explicitly installed.")
    ("--asexplicit"    nil "Install packages as explicitly installed.")
    ;; :HAS-ARG
    ("-l" "--list"     "<REPO> view a list of packages in a repo.")
    ("-s" "--search"   "<REGEX> Search remote repositories for matching strings.")
    ("-r" "--root" 	  "<PATH> Set an alternate installation root.")
    ("-b" "--dbpath"   "<PATH> Set an alternate database location.")
    ;; :HAS-ARG-NO-SHORT
    ("--cachedir"    nil   "<DIR> Set an alternate package cache location.")
    ("--config"      nil   "<PATH> Set an alternate configuration file.")
    ("--logfile"     nil   "<PATH> Set an alternate log file.")
    ("--ignore"      nil   "<PKG> Ignore a package upgrade \(can be used more than once\).")
    ("--ignoregroup" nil   "<GRP> Ignore a group upgrade \(can be used more than once\)."))
  "*A list of short and long flags Flags Arch's `pacman -S' command.\n
:SEE-ALSO `mon-help-pacman-S', `*regexp-clean-pacman-Q*', `mon-help-pacman-commands'.\n►►►")
;;
;;; :TEST-ME *regexp-clean-pacman-S*
;;
;;; (progn (makunbound '*regexp-clean-pacman-S*)(unintern '*regexp-clean-pacman-S*))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-07T16:41:33-05:00Z}#{09501} - by MON KEY>
(defun mon-help-pacman-S (&optional insertp intrp)
  ""
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-pacman-Q :insertp t)
      (message "Pass non-nil for optional arg INTRP")))
;;
;; Now, tack the var list onto mon-help-pacman-S's docstring.
(eval-when-compile
  (let (put-flags)
    (setq put-flags "")
    (mapc #'(lambda (x) 
              (setq put-flags 
                    (concat 
                     (format "`%s' %s%s\n" 
                             (car x) 
                             (if (cadr x) (concat "`" (cadr x) "'") "")
                             (concat " ;" (caddr x))) ;
                     put-flags)))
          (reverse (symbol-value '*regexp-clean-pacman-S*)))
    (setq put-flags
          (with-temp-buffer 
            (insert put-flags)
            (while (search-backward-regexp "' +;" nil t)
              (skip-chars-forward "^;")
              (indent-to 22))
            (buffer-substring-no-properties (buffer-end 0) (buffer-end 1))))
    (mon-help-put-var-doc-val->func 
     'put-flags  
     'mon-help-pacman-S
     (concat 
      "Flags for use with Arch's `pacman -S'.\n\n"
      ":USAGE  pacman {-S --sync} [options] [package\(s\)]\n\n"
      ";; :PACMAN-S-OPTIONS:\n")
     nil 
(concat 
      "\n;; :PACMAN-SYNCHRONIZE\n\n"
      "Packages are installed directly from the ftp servers, including all dependencies\n"
      "required to run the packages.  For example:\n\n"
      " shell> pacman -S qt\n\n"
      "will download and install qt and all the packages it depends on.\n\n"
      "If a package name exists in more than one repo, the repo can be explicitly\n"
      "specified to clarify the package to install:\n\n"
      " shell> pacman -S testing/qt\n\n"
      "You can also specify version requirements:\n\n"
      "pacman -S \"bash>=3.2\"\n\n"
      ":NOTE \(Quotes are needed, otherwise your shell interprets \">\" as redirection to file.\)\n\n"
      "In addition to packages, groups can be specified as well. For example, if gnome\n"
      "is a defined package group, then:\n\n"
      " shell> pacman -S gnome\n\n"
      "will install every package in the gnome group, as well as the dependencies of\n"
      "those packages.\n\n"
      "Packages which provide other packages are also handled. For example, pacman -S\n"
      "foo will first look for a foo package. If foo is not found, packages which\n"
      "provide the same functionality as foo will be searched for. If any package is\n"
      "found, it will be installed.\n\n"
      "To upgrade all packages that are out of date use:\n\n" 
      " shell> pacman -Su\n\n"
      "When upgrading, pacman performs version comparison to determine which packages\n"
      "need upgrading. This behavior operates as follows:\n\n"
      "Alphanumeric:\n"
      " 1.0a < 1.0alpha < 1.0b < 1.0beta < 1.0p < 1.0pre < 1.0rc < 1.0\n\n"
      "Numeric:\n"
      " 1 < 1.0 < 1.1 < 1.1.1 < 1.2 < 2.0 < 3.0.0\n"
      "\n:SYNC-OPTIONS\n\n"
      "-c, --clean\n\n"
      "  Remove packages that are no longer installed from the cache as well as\n"
      "  currently unused sync databases to free up disk space. When pacman downloads\n"
      "  packages, it saves them in a cache directory. In addition, databases are saved\n"
      "  for every sync DB you download from, and are not deleted even if they are\n"
      "  removed from the configuration file pacman.conf(5). Use one --clean switch to\n"
      "  only remove packages that are no longer installed; use two to remove all\n"
      "  packages from the cache. In both cases, you will have a yes or no option to\n"
      "  remove packages and/or unused downloaded databases.\n\n"
      "  If you use a network shared cache, see the CleanMethod option in\n"
      "  pacman.conf(5).\n\n"
      "-g, --groups\n\n"
      "  Display all the members for each package group specified. If no group names\n"
      "  are provided, all groups will be listed; pass the flag twice to view all\n"
      "  groups and their members.\n\n"
      "-i, --info\n\n"
      "  Display dependency and other information for a given package. This will search\n"
      "  through all repositories for a matching package.\n\n"
      "-l, --list\n\n"
      "  List all packages in the specified repositories. Multiple repositories can be\n"
      "  specified on the command line.\n\n"
      "-p, --print-uris\n\n"
      "  Print out URIs for each package that will be installed, including any\n"
      "  dependencies yet to be installed. These can be piped to a file and downloaded\n"
      "  at a later time, using a program like wget.\n\n"
      "-q, --quiet\n\n"
      "  Show less information for certain sync operations. (This is useful when\n"
      "  pacman's output is processed in a script.) Search will only show package names\n"
      "  and not repo, version, group, and description information; list will only show\n"
      "  package names and omit databases and versions; group will only show package\n"
      "  names and omit group names.\n\n"
      "-s, --search <'regexp'>\n\n"
      "  This will search each package in the sync databases for names or descriptions\n"
      "  that match regexp. When you include multiple search terms, only packages with\n"
      "  descriptions matching ALL of those terms will be returned.\n\n"
      "-u, --sysupgrade\n\n"
      "  Upgrades all packages that are out of date. Each currently-installed package\n"
      "  will be examined and upgraded if a newer package exists. A report of all\n"
      "  packages to upgrade will be presented and the operation will not proceed\n"
      "  without user confirmation. Dependencies are automatically resolved at this\n"
      "  level and will be installed/upgraded if necessary. Pass this option twice to\n"
      "  enable package downgrade; in this case pacman will select sync packages whose\n"
      "  version does not match with the local version. This can be useful when the\n"
      "  user switches from a testing repo to a stable one.\n\n"
      "-w, --downloadonly\n\n"
      "  Retrieve all packages from the server, but do not install/upgrade anything.\n\n"
      "-y, --refresh\n\n"
      "  Download a fresh copy of the master package list from the server(s) defined in\n"
      "  pacman.conf(5). This should typically be used each time you use --sysupgrade\n"
      "  or -u. Passing two --refresh or -y flags will force a refresh of all package\n"
      "  lists even if they are thought to be up to date.\n\n"
      "--needed\n\n"
      "  Don't reinstall the targets that are already up-to-date.\n\n"
      "--ignore <'package'>\n\n"
      "  Directs pacman to ignore upgrades of package even if there is one\n"
      "  available. Multiple packages can be specified by separating them with a comma.\n\n"
      "--ignoregroup <'group'>\n\n"
      "  Directs pacman to ignore upgrades of all packages in group even if there is\n"
      "  one available. Multiple groups can be specified by separating them with a\n"
      "  comma.\n\n"
      ":SEE-ALSO `*regexp-clean-pacman-S*', `mon-help-pacman-Q'.\n►►►"))))
;;
;;; :TEST-ME (mon-help-pacman-S )
;;; :TEST-ME (mon-help-pacman-S t)
;;; :TEST-ME (call-interactively 'mon-help-pacman-S)
;;; :TEST-ME (describe-function 'mon-help-pacman-S)
;;
;;; (progn (fmakunbound 'mon-help-pacman-S) (unintern 'mon-help-pacman-S))

;;; ==============================
;;; :CREATED <Timestamp: #{2009-12-07T19:50:21-05:00Z}#{09501} - by MON>
(defun mon-help-pacman-commands (&optional insertp intrp)
  "Common commands for using Arch's pacman package manager.\n
Install a single package or list of packages.
 shell> pacman -S package_name1 package_name2\n
Specify which rep to install from.
 shell> pacman -S extra/package_name
 shell> pacman -S testing/package_name\n
Remove a single package, leaving dependencies installed.
 shell> pacman -R package_name\n
Remove a package's dependencies not required by any other installed package.
 shell> pacman -Rs package_name\n
Delete .pacsave backup files.
 shell> pacman -Rn package_name
 shell> pacman -Rns package_name\n
Synchronize the repository databases and update the system in one go.
 shell> pacman -Syu\n
Query the local package database with the -Q flag.
 shell> pacman -Q --help\n
Query the sync databases with the -S flag.
 shell> pacman -S --help\n
Search for packages.
 shell> pacman -Ss package\n
Search for already installed packages:
 shell> pacman -Qs package\n
Display extensive information about a given package.
 shell> pacman -Si package\n
Information about locally installed packages.
 shell> pacman -Qi package\n
Retrieve a list of the files installed by a package.
 shell> pacman -Ql package\n
Query the database for which package a file on the file system belongs to.
 shell> pacman -Qo /path/to/a/file\n
List all packages no longer required as dependencies \(orphans\).
 shell> pacman -Qdt\n
Download a package without installing it.
 shell> pacman -Sw package\n
Install a 'local' package not from a repository.
 shell> pacman -U /path/to/package/package_name-version.pkg.tar.gz\n
Install a 'remote' package \(not from a repository\).
 shell> pacman -U http://www.examplepackage/repo/examplepkg.tar.gz\n
Clean the package cache of packages not currently installed.
\(/var/cache/pacman/pkg\)
 shell> pacman -Sc\n
:SEE-ALSO `mon-help-pacman-Q', `*regexp-clean-pacman-Q*',
`mon-help-pacman-S', `*regexp-clean-pacman-S*'.\n
:SOURCE (URL `http://wiki.archlinux.org/index.php/Pacman')\n►►►"
  (interactive "i\nP")
  (if (or insertp intrp)
      (mon-help-function-spit-doc 'mon-help-pacman-commands :insertp t)
      (message "Pass non-nil for optional arg INTRP")))
;;
;;; :TEST-ME (mon-help-pacman-commands)
;;; :TEST-ME (mon-help-pacman-commands t)
;;; :TEST-ME (describe-function 'mon-help-pacman-commands)
;;; :TEST-ME (call-interactively 'mon-help-pacman-commands)

;;; ==============================
(provide 'mon-doc-help-pacman)
;;; ==============================

;;; ================================================================
;;; mon-doc-help-pacman.el ends here
;;; EOF
