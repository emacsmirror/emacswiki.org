;;; redshift-indent.el --- minor mode to control the size of (indentation) space
;;
;; Author: Phil S.
;; URL: https://www.emacswiki.org/emacs/redshift-indent.el
;; Keywords: convenience
;; Created: Sep 2015
;; Version: 0.4.3

;; This file is not part of GNU Emacs.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `redshift-indent-mode' controls the expansion and contraction of space,
;; based on the `redshift-cosmological-constant' multiplier.  Only the
;; indentation whitespace at the beginning of a line is affected.
;;
;; This simplest way to invoke `redshift-indent-mode' is to call the
;; `redshift-indent' command, optionally with a prefix argument to set
;; the size of the indentation.
;;
;; Local variables can also be utilised to enable this mode. As the purpose
;; is to circumvent the standard appearance of a file, it is unlikely that
;; a file-local variable would be appropriate; but directory-local variables
;; (e.g. using a custom .dir-locals.el file) may prove a convenient way to
;; trigger this behaviour for a given project.
;;
;; There is no differentiation between whitespace indicating nesting structure,
;; and whitespace used beyond that point for code alignment purposes. (i.e.
;; visual alignments are prone to break with this mode enabled, which makes
;; it a relatively poor fit for languages like lisp where much of the
;; indentation is based on alignment.) You can customize the `redshift-regexp'
;; variable, and/or write a custom function for the `redshift-search-forward'
;; variable, in order to implement such a differentiations, if desired.
;;
;; You may (or may not) find that (setq x-stretch-cursor t) is a useful
;; setting to use with this library, in order to visualise the width of
;; the specified spaces which are replacing the indentation.

;;; Known issues:
;;
;; The current implementation doesn't correctly handle tabs, so I recommend
;; only using this for indentation which is 100% spaces.

;;; Naming scheme:
;;
;; This library is named for astronomical concepts regarding the expansion
;; and/or contraction of Space (i.e. the Universe).
;;
;; Edwin Hubble's observation of redshift in distant galaxies provided the
;; first evidence that the Universe was expanding. Einstein's general theory
;; of relativity had previously predicted that the size of the Universe was
;; variable; however, believing this to be incorrect, he had introduced a
;; "cosmological constant" into his equations to counteract gravity, fixing
;; the size of Space to a static value.

;;; Implementation notes:
;;
;; There are two options for changing the apparent size of spaces using the
;; `display' text-property. The first is to use the (space-width FACTOR)
;; specification to adjust the size of each individual space. This is not
;; sufficient for our purposes: Firstly it does not support indentation with
;; tabs; and secondly it works only for actual space characters, which makes
;; the approach incompatible with the likes of `whitespace-mode' (which can
;; use `buffer-display-table' to display spaces as an alternative character
;; which is unaffected by the space-width specification.
;;
;; The second option is to use the (space . PROPS) specification to replace
;; any sequence of chars with a single space wide enough to cover all the
;; original characters). This is the approach used here. Note that in order
;; to preserve the visibility and editability of each indentation character,
;; the text property is applied to each character individually (we could
;; instead generate a single space with a width spanning the entirety of
;; the indentation for that line, but this is less agreeable when editing).

;;; Change Log:
;; 0.4.3 - Work-in-progress release to the Emacs Wiki.

;;; Code:

(defcustom redshift-cosmological-constant 0.5
  "The cosmological constant fixes the size of Space.

This value is a multiplier, applied to the width of indentation
spaces (and/or tabs) when `redshift-indent-mode' is enabled."
  :group 'indent
  :type 'number)

(defvar redshift-regexp "[[:blank:]]+"
  "A regular expression matching a sequence of indentation characters.

The default value matches any sequence of spaces and/or tabs.

If you indent with tabs (exclusively) and align with spaces, you
can set this pattern to \"\\t+\" to prevent redshift from changing
the widths of any spaces following the indentation tabs. This is
most likely to be useful as a file- or directory-local setting.

n.b. Do not anchor this pattern with ^")

(defun redshift--add-text-properties (beginning end width)
  "Apply space-warping text property to the specified region.

Buffer positions BEGINNING and END delineate a region of (only) indentation
spaces and/or tabs.

WIDTH is the `current-column' which Emacs has calculated at END, which means
it does not matter whether the indentation for any given line consists of
spaces, tabs, or a mixture of both."
  (save-excursion
    (with-silent-modifications
      (let ((end-char end)
            (end-column (current-column))
            (this-char nil)
            (this-char-width nil))
        ;; We need to work backwards to prevent (current-column) from taking
        ;; our modified widths into account.
        (while (> end-char beginning)
          (setq this-char (goto-char (- end-char 1)))
          (let ((this-column (current-column)))
            (setq this-char-width (- end-column this-column))
            (add-text-properties
             this-char end-char
             ;; FIXME: We seem to have two options here, but neither of them
             ;; are exactly right. Upstream changes to the text properties are
             ;; needed, I believe.
             ;;
             ;; With :width, the `current-column' result comes from the the
             ;; (rounded) number of real columns over which the specified
             ;; space(s) stretch; whereas with :relative-width the
             ;; `current-column' is the same as it would have been without
             ;; the text property (which is what we need, to prevent buffer
             ;; changes when re-indenting).
             ;;
             ;; Unfortunately :relative-width also breaks for tabs which are
             ;; visually narrower than tab-width (whereas :width gives us the
             ;; correct appearance in all situations).
             ;;
             ;; At present there does not appear to be a way to get the best
             ;; of both worlds. :relative-width is the safer of the two, so
             ;; I am using this for now.
             ;;
             ;; TODO: Follow up in bug 21533.
             `(display (space :relative-width
                              ,redshift-cosmological-constant)))
            ;; `(display (space :width ,(* this-char-width
            ;;                             redshift-cosmological-constant))))
            (setq end-char this-char
                  end-column this-column)))))))

(defun redshift--remove-text-properties (beginning end)
  "Remove our text property from all spaces in the specified region.

Unlike `redshift--add-text-properties', buffer positions BEGINNING and END
can span any region of the buffer. We only ever add properties to indentation
spaces, but those spaces may subsequently be moved or copied to other parts
of the buffer, so we may need to remove the properties from any arbitrary
spaces."
  ;;
  ;; n.b. We cannot assume that all contiguous spaces share the same
  ;; text properties (for example, it is easy to yank modified spaces
  ;; into the middle of a sequence of unmodified spaces); however, that
  ;; is unimportant when it comes to removing properties, as we do not
  ;; actually need all of the characters in the affected region to have
  ;; the properties in question.
  (save-excursion
    (goto-char beginning)
    (with-silent-modifications
      (while (re-search-forward redshift-regexp end :noerror)
        (remove-text-properties
         (match-beginning 0) (match-end 0) '(display))))))

(defvar redshift-search-forward 're-search-forward
  "A function which finds the next sequence of indentation, and which
leaves point at the first character after that indentation.

Must be argument-compatible with `re-search-forward' (which is the
default value). The REGEXP argument will be `redshift-regexp' with
the addition that it be anchored to the beginning of a line.

Custom search functions could be used to implement custom indentation
detection, in instances where the point at which indentation stops and
alignment begins can be intelligently determined. This can prevent
visual misalignments in the code, when the indentation is shifted.")

(defun redshift--indentation (action &optional beginning end)
  "Manipulate the size of space for all indentation in the buffer.

If ACTION is :shift, we modify the size of space.
If ACTION is :rest, we return space to its usual size.

Optionally, process only the indentation between buffer positions
BEGINNING and END."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or beginning (point-min)))
      (with-silent-modifications
        (let ((anchored-regexp (concat "^" redshift-regexp)))
          (while (funcall redshift-search-forward anchored-regexp end :noerror)
            (let ((indent-start (match-beginning 0))
                  (indent-end (match-end 0)))
              (cond ((eq action :shift)
                     (redshift--add-text-properties indent-start indent-end
                                                    (current-column)))
                    ((eq action :rest)
                     (remove-text-properties indent-start indent-end
                                             '(display)))))))))))

(defalias 'redshift--indentation-shift
  (apply-partially 'redshift--indentation :shift)
  "Equivalent to `redshift--indentation' with ACTION set to :shift")

(defalias 'redshift--indentation-rest
  (apply-partially 'redshift--indentation :rest)
  "Equivalent to `redshift--indentation' with ACTION set to :rest")

(defun redshift--after-change (beginning end old-len)
  "Reset the size of any redshifted space which is not indentation.

This function is called via `after-change-functions'.

START and END are the start and end of the changed text. OLD-LEN is the
pre-change length. (For an insertion, the pre-change length is zero;
for a deletion, that length is the number of chars deleted, and the
post-change beginning and end are at the same place.)

Changes may result in redshifted space no longer being indentation,
but also not included in the \"change\". We need to extend the range
to include any adjacent space, to take account of such occurances."
  ;;
  ;; This can occur anywhere -- joining lines can shift indentation to
  ;; the end of another line, and any redshifted space in the kill
  ;; ring can be yanked into any arbitrary part of the buffer. Any
  ;; such space is no longer matched by the indentation regexp.
  (save-excursion
    ;; Extend the beginning of the region (if necessary).
    (goto-char beginning)
    (when (looking-back redshift-regexp nil :greedy)
      (setq beginning (match-beginning 0)))
    ;; Extend the end of the region (if necessary).
    (goto-char end)
    (when (looking-at redshift-regexp)
      (setq end (match-end 0))))
  ;; Now clean up the (modified) region.
  (redshift--remove-text-properties beginning end)
  ;; Finally, we may have clobbered the text properties of indented
  ;; text, so we need to restore them. (It should be more efficient
  ;; to detect these cases and avoid removing the properties in the
  ;; first place, but this is simpler for now.)
  (redshift--indentation :shift beginning end))

;;;###autoload
(define-minor-mode redshift-indent-mode
  "Adjust the size of space to your liking.

Multiplies the widths of indentation by `redshift-cosmological-constant'."
  :init-value nil
  :lighter " <<"
  (if redshift-indent-mode
      (progn
        (add-hook 'after-change-functions 'redshift--after-change nil :local)
        (add-hook 'change-major-mode-hook 'redshift--indentation-rest nil :local)
        (redshift--indentation :rest)
        (redshift--indentation :shift))
    (remove-hook 'after-change-functions 'redshift--after-change :local)
    (remove-hook 'change-major-mode-hook 'redshift--indentation-rest :local)
    (redshift--indentation :rest)))

;;;###autoload
(defun redshift-indent (arg)
  "Invoke `redshift-indent-mode' with a buffer-local cosmological constant.

With no prefix ARG, `redshift-indent-mode' is toggled.
If ARG is positive, `redshift-cosmological-constant' is set to ARG.
If ARG is negative, `redshift-cosmological-constant' is set to 1.0 / (abs ARG).
If ARG is C-u, `redshift-cosmological-constant' is prompted for interactively.

e.g.:
With prefix arg 2, indentation will appear 2 times its normal width.
With prefix arg -2, indentation will appear 0.5 times its normal width."
  (interactive "P")
  (if (null arg)
      (redshift-indent-mode 'toggle)
    ;; Otherwise set the local cosmological constant, according to ARG.
    (setq arg (if (consp arg)
                  (read-number "Multiply indentation by: "
                               redshift-cosmological-constant)
                (prefix-numeric-value arg)))
    (setq-local redshift-cosmological-constant (if (>= arg 0)
                                                   arg
                                                 (/ 1.0 (abs arg))))
    (redshift-indent-mode 1)))

;;; redshift-indent.el ends here
