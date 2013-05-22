;;; ess-smart-underscore.el --- Ess Smart Underscore
;;
;; Filename: ess-smart-underscore.el
;; Description: ess-smart-underscore
;; Author: Matthew L. Fidler
;; Maintainer: Matthew Fidler
;; Created: Thu Jul 14 11:04:42 2011 (-0500)
;; Version: 0.79
;; Last-Updated: Mon Apr  9 15:27:09 2012 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 137
;; URL: http://github.com/mlf176f2/ess-smart-underscore.el
;; Keywords: ESS, underscore
;; Compatibility:
;; 
;; Features that might be required by this library:
;;
;;   `custom', `easymenu', `ess', `ess-compat', `ess-custom',
;;   `font-lock', `syntax', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; 
;; * Installation
;; 
;; To use without using a package manager:
;; 
;;  - Put the library in a directory in the emacs load path, like ~/.emacs.d
;;  - Add (require 'ess-smart-underscore) in your ~/.emacs file
;; 
;; This is in emacswiki, so this package can also be installed using el-get.
;; 
;; After installing el-get, Type M-x el-get-install ess-smart-underscore.
;; * Ess-Smart Underscore Package Information
;; Smart "_" key: insert `ess-S-assign', unless:
;; 
;;   1. in string/comment
;;   2. after a $ (like d$one_two) (toggle with `ess-S-underscore-after-$')
;;   3. when the underscore is part of a variable definition previously defined.
;;      (toggle with `ess-S-underscore-after-defined')
;;   4. when the underscore is after a "=" or "<-" on the same line.
;;   5. inside a parenthetical statement () or [].
;;      (toggle with `ess-S-underscore-when-inside-paren')
;;   6. At the beginning of a line.
;;   7. In a variable that contains underscores already (for example foo_a)
;;      (toggle with `ess-S-underscore-when-variable-contains-underscores')
;;   8. The preceding character is not a tab/space (toggle with
;;      `ess-S-underscore-when-last-character-is-a-space'.  Not enabled
;;      by default.)
;; 
;; An exception to 
;; 
;; 
;; a <- b |
;; 
;; 
;; Pressing an underscore here would produce
;; 
;; 
;; 
;; a <- b <-
;; 
;; 
;; However when in the following situation
;; 
;; 
;; a <- b|
;; 
;; 
;; Pressing an underscore would produce
;; 
;; 
;; a <- b_
;; 
;; 
;; This behavior can be toggled by `ess-S-space-underscore-is-assignment'
;; 
;; If the underscore key is pressed a second time, the assignment
;; operator is removed and replaced by the underscore.  `ess-S-assign',
;; typically " <- ", can be customized.  In ESS modes other than R/S,
;; an underscore is always inserted.
;; 
;; In addition the ess-smart-underscore attempts to work with noweb-mode
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;; 22-May-2013    Matthew L. Fidler  
;;    Last-Updated: Mon Apr  9 15:27:09 2012 (-0500) #137 (Matthew L. Fidler)
;;    Added more ggplot operators. 
;; 21-May-2013    Matthew L. Fidler  
;;    Last-Updated: Mon Apr  9 15:27:09 2012 (-0500) #137 (Matthew L. Fidler)
;;    Added math to ggplot's functions...
;; 21-May-2013    Matthew L. Fidler  
;;    Last-Updated: Mon Apr  9 15:27:09 2012 (-0500) #137 (Matthew L. Fidler)
;;    Added hook to R-mode to make it behave a little better.
;; 15-May-2013    Matthew L. Fidler  
;;    Last-Updated: Mon Apr  9 15:27:09 2012 (-0500) #137 (Matthew L. Fidler)
;;    Fixed ess-smart-underscore to work in an inferior R process.
;; 15-May-2013    Matthew L. Fidler  
;;    Last-Updated: Mon Apr  9 15:27:09 2012 (-0500) #137 (Matthew L. Fidler)
;;    Added ggplot function prefixes.  
;; 05-Nov-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Apr  9 15:27:09 2012 (-0500) #137 (Matthew L. Fidler)
;;    Better handling of noweb.  I think it Came from Denis Haine and
;;    Martin Maechler.
;; 22-Feb-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Feb 22 20:27:04 2012 (-0600) #120 (Matthew L. Fidler)
;;    Support unbalanced sexps.
;; 02-Feb-2012    Matthew L. Fidler  
;;    Last-Updated: Thu Feb  2 21:06:52 2012 (-0600) #117 (Matthew L. Fidler)
;;    Took out auto-installing.  Most package managers don't want you
;;    to do this.
;; 03-Aug-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Aug  3 15:05:15 2011 (-0500) #112 (Matthew L. Fidler)
;;    Bug fix for parenthetical statement
;; 20-Jul-2011    Matthew L. Fidler  
;;    Last-Updated: Wed Jul 20 15:20:10 2011 (-0500) #101 (Matthew L. Fidler)

;;    Changed to allow underscore instead of assign when inside a
;;    parenthetical statement.

;; 15-Jul-2011    Matthew L. Fidler
;;    Last-Updated: Fri Jul 15 11:34:52 2011 (-0500) #90 (Matthew L. Fidler)
;;    Bug fix for d[d$CMT == 2,"DV"] _ to produce d[d$CMT == 2,"DV"] <-
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
(require 'ess)

(defcustom ess-S-underscore-after-$ t
  "Should underscore produce an underscore if it is an element of a list/data structure?

 Used by \\[ess-smart-underscore]."
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-underscore-after-defined t
  "Should underscore produce an underscore if it is after a variable has been defined?

 Used by \\[ess-smart-underscore]."
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-underscore-after-<-or-= t
  "Should underscore produce an underscore if it is after a \"<-\" or \"=\"?

 Used by \\[ess-smart-underscore]."
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-space-underscore-is-assignment t
  "Should underscore produce `ess-S-assign' when a space is right before the cursor.

Used by \\[ess-smart-underscore]."
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-underscore-when-inside-paren t
  "Should an underscore be produced instead of `ess-S-assign' when inside a parenthetical expression?"
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-underscore-when-inside-unbalanced-parenthesis t
  "Should an underscore be produced instead of `ess-S-assign' when inside an unbalanced parenthetical expression such as:

  foo(bar_

This requires `ess-S-underscore-when-inside-paren' to be true.
"
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-underscore-when-preceeding-words
  '(
    "add"
    "aes"
    "annotation"
    "calc"
    "continuous"
    "coord"
    "cut"
    "discrete"
    "element"
    "expand"
    "facet"
    "geom"
    "gg"
    "guide"
    "label"
    "last"
    "math"
    "mean"
    "position"
    "scale"
    "scale_color"
    "scale_colour"
    "scale_x"
    "scale_y"
    "stat"
    "theme"
    "trans"
    "translate"
    "translate_qplot"
    "update"
    "update_"
    "update_geom"
    )
  "Things that should have underscores after them. "
  :group 'ess-S
  :type '(repeat
          (string :tag "Word")))


(defcustom ess-S-underscore-when-variable-contains-underscores t
  "Should an underscore be produced instead of `ess-S-assign' when variable already contains an underscore?"
  :group 'ess-S
  :type 'boolean)

(defcustom ess-S-underscore-when-last-character-is-a-space nil
  "ESS produces an underscore only when the last character is not a space or a tab."
  :group 'ess-S
  :type 'boolean)

;;;###autoload
(defun ess-smarter-underscore ()
  "Smart \"_\" key: insert `ess-S-assign', unless:
  1. in string/comment
  2. after a $ (like d$one_two) (toggle with `ess-S-underscore-after-$')
  3. when the underscore is part of a variable definition previously defined.
     (toggle with `ess-S-underscore-after-defined')
  4. when the underscore is after a \"=\" or \"<-\" on the same line.
     (toggle with `ess-S-underscore-after-<-or-=')
  5. inside a parenthetical statement () or [].
     (toggle with `ess-S-underscore-when-inside-paren')
  6. At the beginning of a line.
  7. In a variable that contains underscores already (for example foo_a)
     (toggle with `ess-S-underscore-when-variable-contains-underscores')
  8. The preceding character is not a tab/space
     (toggle with `ess-S-underscore-when-last-character-is-a-space'.  Not enabled by default.)
  9. The preceding words/characters are in `ess-S-underscore-when-preceeding-words'


An exception to #4 is in the following situation:

a <- b |

pressing an underscore here would produce

a <- b <-

However when in the following situation

a <- b|

pressing an underscore would produce

a <- b_

This behavior can be toggled by `ess-S-space-underscore-is-assignment'

If the underscore key is pressed a second time, the assignment
operator is removed and replaced by the underscore.  `ess-S-assign',
typically \" <- \", can be customized.  In ESS modes other than R/S,
an underscore is always inserted. "
  (interactive)
  ;;(insert (if (ess-inside-string-or-comment-p (point)) "_"
  ;;ess-S-assign))
  ;;(message "%s" (looking-back "_[^ \t\n]*?\\="))
  (save-restriction
    (ignore-errors
      (when (and (eq major-mode 'inferior-ess-mode)
                 (> (point) (process-mark (get-buffer-process
                                           (current-buffer)))))
        (narrow-to-region (process-mark (get-ess-process)) (point-max)))
      (and ess-noweb-mode
           (noweb-in-code-chunk)
           (noweb-narrow-to-chunk)))
    (if (or
         (not (equal ess-language "S"))
         (looking-back "^[ \t\n]*\\=")
         (looking-back (regexp-opt ess-S-underscore-when-preceeding-words t))
         (and ess-S-underscore-when-variable-contains-underscores
              (looking-back "_[^ \t\n]*?\\="))
         (and ess-S-underscore-when-last-character-is-a-space
              (looking-back "[^ \t]\\="))
         (ess-inside-string-or-comment-p (point))
         ;; Data
         (and ess-S-underscore-after-$ (save-match-data (save-excursion (re-search-backward "\\([$]\\)[A-Za-z0-9.]+\\=" nil t))))
         (and ess-S-underscore-after-<-or-=
              (let ((ret (save-match-data (and (not (looking-back ess-S-assign))
                                               (looking-back "\\(<-\\|\\<=\\>\\).*")))))
                (if (and ret ess-S-space-underscore-is-assignment
                         (looking-back "[ \t]"))
                    (setq ret nil))
                (symbol-value 'ret)))
         ;; Look for variable
         (and ess-S-underscore-after-defined
              (not (looking-back ess-S-assign)) ; Hack to fix bug
              (save-match-data
                (save-excursion
                  (let (word)
                    (when (looking-back "\\<[A-Za-z0-9.]+[ \t]*")
                      (setq word (match-string 0))
                      (setq ret
                            (or (re-search-backward (format "^[ \t]*%s_[A-Za-z0-9.]*[ \t]*\\(<-\\|=\\)" word) nil t)
                                (re-search-backward (format "->[ \t]*%s_[A-Za-z0-9.]*[ \t]*$" word) nil t)))
                      (symbol-value 'ret))))))
         (and ess-S-underscore-when-inside-paren
              (save-match-data
                (save-excursion
                  (let ((pt (point))
                        ret)
                    (when (re-search-backward "\\((\\|\\[\\).*\\=" nil t)
                      (condition-case err
                          (progn
                            (forward-sexp)
                            (when (> (point) pt)
                              (setq ret t)))
                        (error
                         (when ess-S-underscore-when-inside-unbalanced-parenthesis
                           (setq ret 't)))))
                    (symbol-value 'ret))))))
        (insert "_")
      ;; Else one keypress produces ess-S-assign; a second keypress will delete
      ;; ess-S-assign and instead insert _
      ;; Rather than trying to count a second _ keypress, just check whether
      ;; the current point is preceded by ess-S-assign.
      (let ((assign-len (length ess-S-assign)))
        (if (and
             (>= (point) (+ assign-len (point-min))) ;check that we can move back
             (looking-back ess-S-assign))
            ;; If we are currently looking at ess-S-assign, replace it with _
            (progn
              (replace-match "")
              (insert "_"))
          (delete-horizontal-space)
          (insert ess-S-assign))))))
(define-key ess-mode-map (kbd "_") 'ess-smarter-underscore)

(defun ess-smart-underscore-add-inf ()
  "Add to inferior mode."
  (local-set-key (kbd "_") 'ess-smarter-underscore))

(add-hook 'ess-R-post-run-hook 'ess-smart-underscore-add-inf)
(add-hook 'R-mode-hook 'ess-smart-underscore-add-inf)
(provide 'ess-smart-underscore)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ess-smart-underscore.el ends here
