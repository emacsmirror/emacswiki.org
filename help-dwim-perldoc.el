;;; help-dwim-perldoc.el --- 

;; Copyright 2007 Ye Wenbin
;;
;; Author: wenbinye@gmail.com
;; Version: $Id: help-dwim-perldoc.el,v 0.0 2007/08/22 19:49:58 ywb Exp $
;; Keywords: 
;; X-URL: not distributed yet

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
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'help-dwim-perldoc)
;;   (add-hook 'cperl-mode-hook
;;             (lambda () (help-dwim-active-type 'perldoc)))

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'help-dwim)
(require 'woman)

(defvar help-dwim-perldoc-obarray nil
  "Items for `perldoc'")
(defvar help-dwim-perldoc-cache "~/.emacs.d/perldoc-cache.el"
  "Cache file for build `help-dwim-perldoc-obarray'.")
(defvar help-dwim-perldoc-cache-pl "~/.emacs.d/perldoc-cache.pl"
  "Perl script to generate `help-dwim-perldoc-cache'")
(defvar help-dwim-perldoc-perl "perl"
  "Command name of perl")
(defvar help-dwim-perldoc-perldoc "perldoc"
  "Command name of perldoc")
(defvar help-dwim-perldoc-pod2man "pod2man"
  "Command name of pod2man")
(defvar help-dwim-perldoc-buffer-format "*WoMan Perldoc %S*"
  "*Buffer name for perldoc buffer.")

(defun help-dwim-build-perldoc-obarray (&optional re-cache)
  (interactive "P")
  (if (and (null re-cache)
           (file-exists-p help-dwim-perldoc-cache))
      (load help-dwim-perldoc-cache)
    (message "This may take some time, please wait...")
    (setq help-dwim-perldoc-cache-pl
          (expand-file-name help-dwim-perldoc-cache-pl)
          help-dwim-perldoc-cache
          (expand-file-name help-dwim-perldoc-cache))
    (unless (file-exists-p help-dwim-perldoc-cache-pl)
      (help-dwim-perldoc-create-pl))
    (set-process-sentinel
     (start-process-shell-command
      "hdwim" nil
      (concat help-dwim-perldoc-perl " '" help-dwim-perldoc-cache-pl "' >" help-dwim-perldoc-cache))
     (lambda (proc event)
       (if (zerop (process-exit-status proc))
           (progn
             (message "Create perldoc cache successfully!")
             (load help-dwim-perldoc-cache))
         (message "%s" event))))))

(defun help-dwim-perldoc (symbol)
  (interactive
   (list
    (intern
     (completing-read "Perldoc: " help-dwim-perldoc-obarray nil t)
     help-dwim-perldoc-obarray)))
  (let ((buf (format help-dwim-perldoc-buffer-format symbol))
        (name (symbol-name symbol)))
    (if (buffer-live-p (get-buffer buf))
        (display-buffer buf)
      (when symbol
        (with-current-buffer (get-buffer-create buf)
          (if (boundp symbol)           ; function
              (progn
                (call-process help-dwim-perldoc-perldoc  nil t nil "-u" "-f" name)
                (goto-char (point-min))
                (insert (format "=head1 %s\n\n=over 4\n\n" name))
                (goto-char (point-max))
                (insert (format "\n=back\n\n=cut")))
            (if (string-match "\\.pod$" name)
                (setq name (replace-match "" nil nil name)))
            (call-process help-dwim-perldoc-perldoc  nil t nil "-u" name))
          (call-process-region (point-min) (point-max)
                               help-dwim-perldoc-pod2man t t nil
                               "-n" name)
          (condition-case nil
              (woman-process-buffer)
            (error
             (erase-buffer)
             (apply 'call-process
                    `(,help-dwim-perldoc-perldoc
                      nil t nil ,@(if (boundp symbol)
                                      "-f") ,name))))
          (display-buffer (current-buffer)))))))

(help-dwim-register
 '(perldoc . [ "a-zA-Z_:." help-dwim-perldoc-obarray nil help-dwim-perldoc ])
 nil
 '((help-dwim-build-perldoc-obarray)))
;; In case activate perldoc in custom-file
(help-dwim-load-extra)

(defun help-dwim-perldoc-create-pl ()
  (with-temp-buffer
    (insert
     "#! /usr/bin/perl -w\n"
     "use File::Find;\n"
     "use Data::Dumper qw(Dumper);\n"
     "use Text::Wrap qw(wrap);\n"
     "my $fn = build_function();\n"
     "print <<'EL';\n"
     "(setq help-dwim-perldoc-obarray (make-vector 1519 nil))\n"
     ";; Functions\n"
     "(mapc (lambda (func)\n"
     "         (set (intern func help-dwim-perldoc-obarray) t))\n"
     "'(\n"
     "EL\n"
     "my $i = 1;\n"
     "print wrap('', '', join(' ', map {qq(\"$_\")} sort keys %$fn )), \"))\\n\\n\";\n"
     "\n"
     "print <<'EL';\n"
     ";; Modules\n"
     "(mapc (lambda (mod)\n"
     "         (intern mod help-dwim-perldoc-obarray))\n"
     "'(\n"
     "EL\n"
     "my $mod = build_modules();\n"
     "print wrap('', '', join(' ', map {qq(\"$_) . (exists $fn->{$_} ? \".pod\" : \"\") . '\"'} sort keys %$mod )), \"))\\n\";\n"
     "\n"
     "sub build_modules {\n"
     "    my %mod;\n"
     "    for my $dir ( @INC ) {\n"
     "        next if $dir eq '.';\n"
     "        next unless -d $dir;\n"
     "        my $len = length($dir)+1;\n"
     "        find( { wanted => sub {\n"
     "                    if ( -f $_ && /\\.(pm|pod)$/i ) {\n"
     "                        my $mod = substr($File::Find::name, $len);\n"
     "                        $mod =~ s#^pod/##;\n"
     "                        $mod =~ s/.(pm|pod)$//;\n"
     "                        $mod =~ s#/#::#g;\n"
     "                        $mod{$mod}++;\n"
     "                    }\n"
     "                },\n"
     "                follow => 1\n"
     "            }, $dir);\n"
     "    }\n"
     "    return \\%mod;\n"
     "}\n"
     "\n"
     "sub build_function {\n"
     "    chomp(my $file = `perldoc -l perlfunc`);\n"
     "    my %fn;\n"
     "    open(FH, $file) or die \"Can't open file $file: $!\";\n"
     "    while ( <FH> ) {\n"
     "        last if /^=head2 Alphabetical/;\n"
     "    }\n"
     "    while ( <FH> ) {\n"
     "        last if /^=over/;\n"
     "    }\n"
     "    my $stat = 1;\n"
     "    while ( <FH> ) {\n"
     "        if ( /^=item/ ) {\n"
     "            if ( $stat ) {\n"
     "                my $fn = (split /\\s+/, $_)[1];\n"
     "                $fn =~ s#/.*$##;  #  y///, m// and so on\n"
     "                $fn =~ s/\\(.*$//; # chomp(, chop(\n"
     "                $fn{$fn}++;\n"
     "            }\n"
     "        } elsif ( /^=over/ ) {\n"
     "            $stat = 0;\n"
     "        } elsif ( /^=back/ ) {\n"
     "            $stat = 1;\n"
     "        }\n"
     "    }\n"
     "    map { $fn{'-'.$_}++ } qw/A B C M O R S T W X b c d e f g k l o p r s t u w x z/;\n"
     "    return \\%fn;\n"
     "}\n")
    (write-region (point-min) (point-max) help-dwim-perldoc-cache-pl)))

;;{{{  perldoc-tree
(defvar help-dwim-perldoc-tree-buffer "*Perldoc*"
  "Buffer name for `perldoc-tree'")
(defvar help-dwim-perldoc-pragmas
  '("attributes" "attrs" "autouse" "base" "bigint" "bignum" "bigrat"
    "blib" "bytes" "charnames" "constant" "diagnostics" "encoding"
    "fields" "filetest" "if" "integer" "less" "lib" "locale" "open"
    "ops" "overload" "perlpod" "perlpodspec" "re" "sigtrap" "sort"
    "strict" "subs" "threads" "threads::shared" "utf8" "vars" "vmsish"
    "warnings" "warnings::register"))

(defun help-dwim-perldoc-tree ()
  "Create pod tree."
  (interactive)
  (require 'tree-widget)
  (require 'tree-mode nil t)
  (help-dwim-build-perldoc-obarray)
  (if (get-buffer help-dwim-perldoc-tree-buffer)
      (display-buffer help-dwim-perldoc-tree-buffer)
    (with-current-buffer (get-buffer-create help-dwim-perldoc-tree-buffer)
      (widget-create (help-dwim-perldoc-tree-widget))
      (if (fboundp 'tree-minor-mode)
          (tree-minor-mode 1)
        (use-local-map widget-keymap))
      (widget-setup)
      (display-buffer (current-buffer)))))
(defalias 'perldoc-tree 'help-dwim-perldoc-tree)

(defun help-dwim-perldoc-tree-widget ()
  `(tree-widget
    :node (push-button
           :tag "Perldoc"
           :format "%[%t%]\n")
    :open t
    ,@(mapcar
       (lambda (cat)
         `(tree-widget
           :node (push-button
                  :tag ,(car cat)
                  :format "%[%t%]\n")
           :dynargs ,(cdr cat)))
       '(("Function" . help-dwim-perldoc-function-expand)
         ("Core document" . help-dwim-perldoc-coredoc-expand)
         ("Pragram" . help-dwim-perldoc-pragram-expand)
         ("Modules" . help-dwim-perldoc-modules-expand)))))

(defun help-dwim-perldoc-function-expand (tree)
  (or (widget-get tree :args)
      (mapcar (lambda (f)
                (help-dwim-perldoc-item f t))
              (sort (all-completions "" help-dwim-perldoc-obarray
                                     (lambda (sym) (and sym (boundp sym))))
                    'string<))))

(defun help-dwim-perldoc-coredoc-expand (tree)
  (or (widget-get tree :args)
      (mapcar 'help-dwim-perldoc-item 
              (sort
               (let (completion-ignore-case)
                 (all-completions "perl" help-dwim-perldoc-obarray))
               'string<))))

(defun help-dwim-perldoc-pragram-expand (tree)
  (or (widget-get tree :args)
      (mapcar 'help-dwim-perldoc-item
              help-dwim-perldoc-pragmas)))

(defun help-dwim-perldoc-modules-expand (tree)
  (or (widget-get tree :args)
      (let ((hash (make-hash-table :test 'equal))
            case-fold-search
            module)
        (mapatoms
         (lambda (sym)
           (and (not (boundp sym))
                (puthash (car (split-string (symbol-name sym) "::")) t hash)))
         help-dwim-perldoc-obarray)
        (maphash
         (lambda (key val)
           (unless (or (string-match "^perl" key)
                       (string-match "\\.pod$" key)
                       (member key help-dwim-perldoc-pragmas))
             (push key module)))
         hash)
        (mapcar 'help-dwim-perldoc-module-item
                (sort module 'string<)))))

(defun help-dwim-perldoc-has-submodp (mod)
  (not (null (try-completion (concat mod "::") help-dwim-perldoc-obarray))))

(defun help-dwim-perldoc-sub-module-expand (tree)
  (or (widget-get tree :args)
      (let ((name (widget-get (tree-widget-node tree) :tag))
            module)
        (mapc (lambda (mod)
                (if (string-match (concat (regexp-quote name) "::[^:]+")
                                  mod)
                    (add-to-list 'module (match-string 0 mod))))
              (all-completions name help-dwim-perldoc-obarray))
        (mapcar
         'help-dwim-perldoc-module-item
         (sort module
               (lambda (m1 m2)
                 (let ((p1 (help-dwim-perldoc-has-submodp m1))
                       (p2 (help-dwim-perldoc-has-submodp m2)))
                   (if (eq p1 p2)
                       (string< m1 m2)
                     p1))))))))

(defun help-dwim-perldoc-module-item (mod)
  (if (help-dwim-perldoc-has-submodp mod)
      `(tree-widget
        :node (push-button
               :tag ,mod
               :format "%[%t%]\n"
               :notify help-dwim-perldoc-select)
        :dynargs help-dwim-perldoc-sub-module-expand)
    `(push-button
      :tag ,mod
      :format "%[%t%]\n"
      :notify help-dwim-perldoc-select)))

(defun help-dwim-perldoc-item (name &optional funcp)
  `(push-button
    :tag ,name
    :format "%[%t%]\n"
    :funtion ,funcp
    :notify help-dwim-perldoc-select))

(defun help-dwim-perldoc-select (node &rest ignore)
  (let ((name (widget-get node :tag))
        (funcp (widget-get node :funtion))
        sym)
    (setq sym (intern-soft name help-dwim-perldoc-obarray))
    (if (not sym)
        (message "No perldoc found for %s" name)
      (and (not funcp) (boundp sym)
           (setq sym (intern-soft (concat name ".pod") help-dwim-perldoc-obarray)))
      (help-dwim-perldoc sym))))
;;}}}
 
(provide 'help-dwim-perldoc)
;;; help-dwim-perldoc.el ends here

