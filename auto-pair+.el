;;; auto-pair+.el --- Autopair handler to extend Autopair behavior.
;; 
;; Filename: auto-pair+.el
;; Description: Autopair handler to extend Autopair behavior.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sun Nov  7 18:40:10 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Tue Nov 30 14:05:00 2010 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 550
;; URL:  http://www.emacswiki.org/emacs/auto-pair+.el 
;; Keywords: Autopair, selection, whitespace
;; Compatibility: 
;; 
;; Features that might be required by this library:
;; 
;;   `autopair', `cl'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Added TextMate wrapping of selection:
;; 
;;       It is also possible to wrap a selection in an open/close
;;       character by selecting text and typing the opening
;;       character. For example if you type foo, select it and type (
;;       then TextMate will make it (foo) placing the caret after the
;;       ending parentheses.
;;
;; This requires `autopair-autowrap' to be true.
;;
;; The default TextMate method of putting the cursor at the end
;; parenthesis is useful in many modes (though not really lisp).  To
;; change where the cursor ends up you can customize
;; `autopair-goto-char-after-region-wrap'.  It may be placed in the following locations:
;;
;; 1(2foo3)4
;;
;; 1 -- Before the open parenthesis
;; 2 -- After the open parenthesis (useful for lisp)
;; 3 -- Before the close parenthesis
;; 4 -- After the close parenthesis (TextMate default)
;;
;; Anything else puts the point where you had it .  To change the
;; default after-location for a certain mode, like emacs-lisp-mode,
;; you may wish to add the following hook to emacs-lisp-mode:
;;
;; (add-hook 'emacs-lisp-mode-hook (lambda() (setq autopair-goto-char-after-region-wrap 2)))
;;
;; This only works outside of comments and strings.  Otherwise the
;; value of `autopair-goto-char-after-region-wrap-comments'.
;;
;; In a similar vein, though I'm unsure it is supported in TextMate, I
;; added wrapping of a quote.  When
;; `autopair-escape-region-when-quoting' (enabled by default) is true,
;; then it will appropriately quote the string.  For example selecting
;; the following string:
;;
;; This is a test of the quoting system, "this is only a test"
;;
;; And pressing quote, gives:
;;
;; "This is a test of the quoting system, \"this is only a test\""
;;
;;  Quoting the whole phrase again gives:
;;
;; "\"This is a test of the quoting system, \"this is only a test\"\""
;;
;; You may also select a region within the string and quote it.  When
;; this happens, the appropriate escape characters are used.  For
;; example by selecting the word quoting and pressing " in
;; emacs-lisp-mode gives:
;;
;; "\"This is a test of the \"quoting\" system, \"this is only a test\"\""
;;
;; Note:  For this to work font-locking must be enabled.
;;
;; Therefore already quoted strings will not be quoted again.
;;
;; This behavior is appropriate for most modes.  By default, the
;; escape character is determined by the syntax class (looks at first
;; 256 characters)
;;
;; However if this is visual basic mode, you may prefer the following:
;;
;; "This is a test of the quoting system, ""this is only a test"""
;;
;; Also upon re-quoting the above region, one gets:
;;
;; """This is a test of the quoting system, ""this is only a test""""" 
;;
;; This may be achieved by changing the `autopair-quote-string' to
;; "%s" by the following hook:
;;
;; (add-hook 'visual-basic-mode-hook (lambda() (setq autopair-quote-string "%s")))
;;
;; Currently quoting only occurs outside of comments.
;;
;; Also when typing inside a string, quotes are generated automatically.  When
;; typing twice, the quoted quotes are change to regular quotes.
;;
;; In addition to having  a backspace change:
;;  " \"|\" "
;; to
;; "  "
;; It also changes:
;;
;;   " \"\"| "
;;
;; To
;;  " \" "
;;
;; unless `autopair-delete-orphan-quoted-string' is nil 
;;
;; Added `autopair-skip-whitespace-but-dont-delete'.  This will skip
;; whitespace, but not delete it.  This implies the following:
;;
;; In R mode, lets say you type:
;;
;;    f <- function(x,...){
;;
;; If | represents the carat, autopairs will give you the following:
;;
;;   f <- function(x,...){|}
;;
;; By typing return the following is produced
;;
;;  f <- function(x){
;;      |
;;  }
;;
;;
;; Currently in autopair mode the effect of typing } gives:
;;
;;  f <- function(x){
;;      }|
;;  }
;;
;; By enabling `autopair-skip-whitespace-but-dont-delete', typing a }
;; gives:
;;
;; f <- function(x){
;;
;; }|
;;
;; Features that I call bugs that are taken care of with this
;; modification: Deleting when autopairs shouldn't and special returns
;; when autopairs shouldn't.  This can be customized by
;; `autopair-backspace-fix'.  I'm not sure which method TextMate
;; actually does.
;;
;;
;; Consider the following in lisp:
;;
;; ()|()
;;
;; If you backspace, then the following occurs
;;
;; ()
;;
;; I believe that this should occur instead:
;;
;; (()
;;
;; With the first expression:
;; ()|()
;;
;; Pressing return yields
;;  ()
;;  |
;;  ()
;; I believe it should yield:
;;
;; ()
;; |()
;;
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 30-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 30 13:52:41 2010 (-0600) #547 (Matthew L. Fidler)
;;    When pressing quote inside of a string first assume \"\".  After another quote,
;;    assume ""
;; 19-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Nov 19 08:36:24 2010 (-0600) #523 (Matthew L. Fidler)
;;    String Quote bug-fix.
;; 09-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  9 12:28:02 2010 (-0600) #518 (Matthew L. Fidler)
;;    Bugfix for cases where there is no extra syntax pair.
;; 08-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Nov  8 12:46:40 2010 (-0600) #398 (Matthew L. Fidler)
;;    Initial release.
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
;; 
;;
(setq autopair-autowrap 't)

(defcustom autopair-delete-orphan-quoted-string 't
  "When enabled, delete orphan strings.  For example when enabled with \\\"\\\"| pressing a backspace will yield \\\"| "
  :group 'autopair
  :type 'booelan
  )
(defcustom autopair-backspace-fix 't
  "When enabled, this fixes the condition when autopairs shouldn't be deleted or have an extra return added.  
  
 Consider the following in lisp:

 ()|()

 If you backspace, then the following occurs

 ()

 I believe that this should occur instead:

 (()

 With the first expression:
 ()|()

 Pressing return yields
  ()
  |
  ()
 I believe it should yield:

 ()
 |()"
  :type 'boolean
  :group 'autopair
  )
(defcustom autopair-goto-char-after-region-wrap 4
  "* When foo is selected and an open parenthesis is added,
wrapping of the region in () is done, as in (foo).  When this
occurs, the cursor can be dealt within 6 possible ways.  Put the cursor at:
1(2foo3)4, put the cursor just outside the newly added parenthesis (5), or leave the cursor alone (everything else).

When the cursor is just outside the parenthesis it is either at
|(foo) or (foo)|, whichever was closer to the initial position.

"
  :type '(choice
          (const :tag "Leave the cursor alone" 0)
          (const :tag "Put cursor before the beginning parenthesis" 1)
          (const :tag "Put cursor just inside the beginning parenthesis" 2)
          (const :tag "Put cursor before the ending parenthesis" 3)
          (const :tag "Put cursor just after the ending parenthesis" 4)
          (const :tag "Put cursor just outside the newly added parenthesis" 5)
          )
  :group 'autopair)

(defcustom autopair-goto-char-after-region-wrap-comments 4
  "* When foo is selected and an open parenthesis is added,
wrapping of the region in () is done, as in (foo).  When this
occurs, the cursor can be dealt within 6 possible ways.  Put the cursor at:
1(2foo3)4, put the cursor just outside the newly added parenthesis (5), or leave the cursor alone (everything else).

When the cursor is just outside the parenthesis it is either at
|(foo) or (foo)|, whichever was closer to the initial position.

"
  :type '(choice
          (const :tag "Leave the cursor alone" 0)
          (const :tag "Put cursor before the beginning parenthesis" 1)
          (const :tag "Put cursor just inside the beginning parenthesis" 2)
          (const :tag "Put cursor before the ending parenthesis" 3)
          (const :tag "Put cursor just after the ending parenthesis" 4)
          (const :tag "Put cursor just outside the newly added parenthesis" 5)
          )
  :group 'autopair)
(make-variable-buffer-local 'autopair-goto-char-after-region-wrap)
(defcustom autopair-escape-region-when-quoting 't
  "* When regions are selected, quote that region"
  :type 'boolean
  :group 'autopair)

(defcustom autopair-quote-string nil
  "* The string used for quoting string characters within a
string.  If you use double quotes, as in Visual Basic, set this
to \"%s\" as this will double the quotes.  This is a buffer local
variable with a default of escaping quote characters with a
backslash.  To make this behave well in visual-basic-mode, you may add the hook:

  (add-hook 'visual-basic-mode-hook (lambda() (setq autopair-quote-string \"%s\")))
 "
  :type 'string
  :group 'autopair)
(make-variable-buffer-local 'autopair-quote-string)
(add-hook 'visual-basic-mode-hook (lambda() (setq autopair-quote-string "%s")))
(defcustom autopair-skip-whitespace-but-dont-delete 't
  "* Skips whitespace but doesn't delete it.
In R mode, lets say you type:

    f <- function(x,...){

If | represents the carat, autopairs will give you the following:

  f <- function(x,...){|}

By typing return the following is produced

 f <- function(x){
     |
 }


Currently in autopair mode the effect of typing } gives:

 f <- function(x){
     }|
 }

By enabling `autopair-skip-whitespace-but-dont-delete', typing a }
gives:

f <- function(x){

}|

"
  :type 'boolean
  :group 'autopair) 
(defun autopair-get-quote-string ()
  "* Gets quote string from current syntax table."
  (unless autopair-quote-string
    (dotimes (char 256)
      (unless autopair-quote-string
        (if (= ?\\ (char-syntax char))
            (setq autopair-quote-string (string char)))))))
(defun autopair-default-quote-region (quote-char point-begin point-end)
  "*This function quotes a region to be in a string.  Returns the final length of the quoted region"
  (autopair-get-quote-string)
  (save-excursion
    (save-restriction
      (narrow-to-region point-begin point-end)
      (goto-char (point-min))
      (while (re-search-forward (regexp-quote quote-char) nil 't)
        (cond
         ( (string= "%s" autopair-quote-string)
           ;; Try to quote correctly.  Everything should be in pairs.
           ;; Those that are not in pairs should be quoted.
           (unless (re-search-forward (format "\\=%s" (regexp-quote quote-char)) nil t)
             (insert quote-char)))
         ( 't
           (save-excursion 
             (goto-char (match-beginning 0))
             (unless (looking-back (regexp-quote (format autopair-quote-string quote-char)))
               (insert (format autopair-quote-string quote-char)))))))
      (- (point-max) (point-min)))))
(defun autopair-texmate-style-newline()
  "* TextMate style newline"
  (save-excursion
    (newline-and-indent))
  (indent-according-to-mode)
  (when (or (and (boundp 'global-hl-line-mode)
                 global-hl-line-mode)
            (and (boundp 'hl-line-mode)
                 hl-line-mode))
    (hl-line-unhighlight) (hl-line-highlight))
  )
(defun autopair-default-handle-wrap-action+ (wrap-action pair pos-before region-before)
  "Called on  wrap"
  ;; Dummy function to call autopair-default-handle-action+
  (condition-case err 
      (let ((action (first autopair-action))
            num
            (p1 (car region-before))
            (p2 (cdr region-before)))
        (when (eq 'wrap wrap-action)
          (cond
           (;; automatically insert closing delimiter at the right point when there is a region selected
            (and (eq 'opening action)
                 (not (eq pair (char-before)))
                 )
            (let (comment-or-string
                  (last (char-before)))
              (unless comment-or-string
                (setq comment-or-string
                      (memq
                       (get-text-property (min p1 p2 pos-before) 'face)
                       '(font-lock-comment-face font-lock-string-face font-lock-doc-face))))
              (unless comment-or-string
                (setq comment-or-string
                      (memq
                       (get-text-property (max p1 p2 pos-before) 'face)
                       '(font-lock-comment-face font-lock-string-face font-lock-doc-face))))
              (save-excursion 
                (goto-char (+ 1 (max pos-before p1 p2)))
                (insert pair)
                (goto-char (min pos-before p1 p2))
                (insert last)
                )
              (delete-backward-char 1)
              (setq num autopair-goto-char-after-region-wrap)
              (when comment-or-string
                (setq num autopair-goto-char-after-region-wrap-comments))
              (cond
               ((= num 1)
                (goto-char (min pos-before p1 p2))) 
               ((= num 2)
                (goto-char (+ 1 (min pos-before p1 p2))))
               ( (= num 3)
                 (goto-char (+ 1 (max pos-before p1 p2))))
               ( (= num 4)
                 (goto-char (+ 2 (max pos-before p1 p2))))
               ((= num 5)
                (if (> (- (max pos-before p1 p2) (point)) (- (point) (min pos-before p1 p2)))
                    (goto-char (min pos-before p1 p2))
                  (goto-char (+ 2 (max pos-before p1 p2))))))
              (autopair-blink))
            (setq autopair-action nil))
           (;; automatically insert quote delimiter on region.
            ;; Use a function to quote the region.
            (eq 'insert-quote action)
            (let (comment
                  f
                  len 
                  (last (char-before))
                  strp
                  strp-pos-before
                  )
              (unless comment
                (setq comment
                      (memq
                       (get-text-property (min pos-before p1 p2) 'face)
                       '(font-lock-comment-face))))
              (unless comment
                (setq comment
                      (memq
                       (get-text-property (max pos-before p1 p2) 'face)
                       '(font-lock-comment-face))))
              (unless strp
                (setq strp
                      (memq
                       (get-text-property (min pos-before p1 p2) 'face)
                       '(font-lock-string-face font-lock-doc-face))))
              (unless strp
                (setq strp
                      (memq
                       (get-text-property (max pos-before p1 p2) 'face)
                       '(font-lock-string-face font-lock-doc-face))))     
              (save-excursion 
                (goto-char (+ 1 (max pos-before p1 p2)))
                (insert pair)
                (goto-char (min pos-before p1 p2))
                (insert last))
              
              ;; Delete the already inserted quote pair.
              (delete-backward-char 1)
              (if comment
                  (setq len (- (max p1 p2 pos-before ) (min p1 p2 pos-before)))
                (setq len (autopair-default-quote-region (string pair)
                                                         (+ 1 (min pos-before p1 p2))
                                                         (max p1 p2 pos-before))))
              (goto-char (min p1 pos-before p2))
              ;; Quote is escaped when in a string.
              (when strp
                (if (looking-at (regexp-quote (string pair)))
                    (insert (format autopair-quote-string (string pair)))
                  (backward-char 1)
                  (insert (format autopair-quote-string (string pair)))
                  (forward-char 1)))
              ;; Now go to the end of the string.
              "This is a test of the quoting system."
              (forward-char (+ 2 len))
              (when strp 
                (save-excursion (insert (format autopair-quote-string (string pair)))))
              (autopair-blink))
            (setq autopair-action nil)
            ))))
    (error
     (message "[auto-pair+] Ignored error in `autopair-default-handle-wrap-action+',%s " (error-message-string err)))))

(defun autopair-default-handle-action+ (action pair pos-before &optional reg-start reg-end)
  ;;(message "action is %s, pair is: %s or %s" action pair (string pair))
  (autopair-get-quote-string)
  (condition-case err
      (cond (;; automatically insert closing `delimiter'
             (and (eq 'opening action)
                  (not (eq pair (char-before))))
             (insert pair)
             (autopair-blink)
             (backward-char 1))
            (;; automatically insert closing quote delimiter
             (eq 'insert-quote action)
             ;; Quote the pair when inside the region.
             (let ((strp (memq (get-text-property (- pos-before 1) 'face)
                               '(font-lock-string-face font-lock-doc-face))))
               (if (and strp
                        (looking-at (regexp-quote (format "%s%s" (format autopair-quote-string (string pair)) (string pair))))
                        )
                   (progn
                     ;; Actually skip-quote action.
                     (replace-match "")
                     (looking-at ".*")
                     (if (looking-back (regexp-quote (format "%s%s%s" autopair-quote-string (string pair) (string pair))))
                         (progn ;; Quote pressed twice change \"\" to ""
                           (replace-match "")
                           (insert (format "%s%s" (string pair) (string pair)))
                           (backward-char 1)
                           )
                       (save-excursion
                         (backward-char 1)
                         (insert (format autopair-quote-string (string pair))))))
                 (if (and strp (looking-at "[ \t]*$")) nil ;; If looking at the end of line, just insert the quote.
                   (when strp          
                     (save-excursion (backward-char 1) (insert (format autopair-quote-string (string pair))))
                     (insert (format autopair-quote-string (string pair)))
                     )
                   (insert pair)
                   (when strp
                     (backward-char 1)))
                 (unless (and strp (looking-at "[ \t]*$")) ;; Don't move backwards if just inserted a single quote.
                   (autopair-blink)
                   (backward-char 1)))))
            (;; automatically skip open closer quote delimiter
             (and (eq 'skip-quote action)
                  (eq pair (char-after (point))))
             (delete-char 1)
             (if (and 
                  (string= autopair-quote-string "%s")))
             (autopair-blink-matching-open))
            (;; skip over newly-inserted-but-existing closing delimiter
             ;; (normal case)
             (eq 'closing action)
             (let ((skipped 0))
               (when (and (not (eq (get-text-property (point) 'face) 'font-lock-comment-face))
                          (or autopair-skip-whitespace-but-dont-delete autopair-skip-whitespace))
                 (setq skipped (save-excursion (skip-chars-forward "\s\n\t"))))
               (when (eq autopair-inserted (char-after (+ (point) skipped)))
                 (unless (zerop skipped) (autopair-blink (+ (point) skipped)))
                 (if (zerop skipped)
                     (progn (backward-char 1) (delete-char 1) (forward-char))
                   (if autopair-skip-whitespace
                       (delete-char (1+ skipped))
                     (delete-backward-char 1)
                     (forward-char (1+ skipped))))
                 (autopair-blink-matching-open))))
            (;; autodelete quote closing delimiter in strings.
             (if (not (and (eq 'backspace action)
                           (or
                            (string= "%s" autopair-quote-string)
                            (and (not (string= "%s" autopair-quote-string))
                                 (memq (get-text-property (point) 'face) '(font-lock-string-face font-lock-doc-face)))
                            )
                           (or
                            (eq ?\" (char-syntax pair))
                            (string= "%s" autopair-quote-string)
                            )
                           ))
                 nil
               (if (and (looking-at (regexp-quote (format "%s%s" (format autopair-quote-string (string pair))
                                                          (string pair))))
                        (save-match-data
                          (if (not
                               (looking-back (regexp-quote (format "%s" (format autopair-quote-string (string pair))))))
                              nil
                            (replace-match "")
                            't))) 't
                 ;; Auto-delete unmatched quote closing delimiter
                 (if (and autopair-delete-orphan-quoted-string
                          (looking-back (regexp-quote (format autopair-quote-string (string pair)))))
                     't
                   nil)))
             (replace-match ""))
            (;; autodelete closing delimiter
             (and (eq 'backspace action)
                  (eq pair (char-after (point))))
             (if (not autopair-backspace-fix)
                 (delete-char 1)
               ;; Now check the type of point that this is...
               (cond
                (;; ()|() type
                 ;; Do nothing.
                 (= ?\( (char-syntax pair)))
                (;; Check to see if it in the extra autopair-extra-pairs
                 (let ((pair-opening-regexp
                        (regexp-opt
                         (mapcar (lambda(x) (string (nth 0 x)))
                                 (append
                                  (getf autopair-extra-pairs :string)
                                  (getf autopair-extra-pairs :comment)
                                  (getf autopair-extra-pairs :code)
                                  (getf autopair-extra-pairs :everywhere))) 't)))
                   ;; If this is a starting pair, do nothing.   `'|`' type.
                   (if (string= "" pair-opening-regexp)
                       nil
                     (string-match pair-opening-regexp (string pair)))))
                (;; Delete character
                 't
                 (delete-char 1)))))
            (;; opens an extra line after point, then indents
             (and (eq 'newline action)
                  (eq pair (char-after (point))))
             (if (not autopair-backspace-fix)
                 (autopair-texmate-style-newline)
               (cond
                (;; ()|() type
                 ;; Do nothing.
                 (= ?\( (char-syntax pair)))
                (;; Check to see if it in the extra autopair-extra-pairs
                 (let ((pair-opening-regexp
                        (regexp-opt
                         (mapcar (lambda(x) (string (nth 0 x)))
                                 (append
                                  (getf autopair-extra-pairs :string)
                                  (getf autopair-extra-pairs :comment)
                                  (getf autopair-extra-pairs :code)
                                  (getf autopair-extra-pairs :everywhere))) 't)))
                   ;; If this is a starting pair, do nothing.   `'|`' type.
                   (if (string= "" pair-opening-regexp) nil
                     (string-match pair-opening-regexp (string pair)))))
                (;; Type in extra return
                 (autopair-texmate-style-newline))))))
    (error
     (message "[auto-pair+] Ignored error in `autopair-default-handle-action+',%s " (error-message-string err)))))
(defun autopair-default-handle-action (action pair pos-before)
  (autopair-default-handle-action+ action pair pos-before))
(defun autopair-default-handle-wrap-action (wrap-action pair pos-before region-before)
  (autopair-default-handle-wrap-action+ wrap-action pair pos-before region-before)
  )
(provide 'auto-pair+)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-pair+.el ends here
