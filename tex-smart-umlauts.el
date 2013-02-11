;;; tex-smart-umlauts.el --- Smart umlaut conversion for TeX.

;;; Author: Frank Fischer <frank.fischer at mathematik.tu-chemnitz.de>
;;; Keywords: tex, wp
;;; Version: 1.0.0
;;; URL: http://hub.darcs.net/lyro/tex-smart-umlauts

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Transform LaTeX encoded non-ASCII characters to and from their
;; visible (utf-8) representations when visiting a file and preserve
;; their original encoding when saving the buffer.
;;
;; TeX/LaTeX documents often contain special characters that are not
;; available in ASCII code in a special command form. For example, the
;; German umlaut ä can be written in LaTeX as {\"a}, \"{a}, \"a or
;; even "a. Of course, nowadays one should an ecoding like utf-8 that
;; contain a natural representation of these characters.
;; Unfortunately, old documents may still use the old encodings or
;; non-ASCII encodings are not allowed for other reasons. Emacs can
;; already automatically encode and decode such characters from and to
;; their LaTeX representation using the `iso-cvt' package: during
;; editing the characters are utf-8 encoded but when saved to disk the
;; characters are transformed to a LaTeX representation.
;;
;; Unfortunately, this an all-or-nothing approach: either all
;; characters are transformed or none. But when working on a document
;; with several author this may be problematic. This may particularly
;; true if the document is stored in a revision control system. In
;; these cases each author may use its own editor, each editor may
;; have its own setting and encodes umlauts in a special preferred
;; way. Each time one author does a small change, *all* umlauts in the
;; whole document may get transformed. This may also lead to huge
;; diffs that contain of many hunks that only change the encoding of
;; some characters.
;;
;; The purpose of `tex-smart-umlauts' is to automatically encode and
;; decode umlauts from and to their tex representation, while
;; *preserving* the original encoding. In other words, when a LaTeX
;; file is visited, the original encoding of each character is saved
;; and the character is transformed to its visible (utf-8)
;; representation. When the document is saved again, each character
;; that has been present when the document has been loaded is saved in
;; its *original* encoding. Only newly inserted non-ASCII characters
;; get a new encoding that depends on the user-options of
;; `tex-smart-umlauts'. This way, a small change in a document will
;; not reencode all non-ASCII charactes as `iso-cvt' would do and only
;; the modified parts of the document will really be modified on disk.
;;
;;; Usage:
;;
;; Simply add
;;
;; (add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-decode)
;;
;; to your .emacs file. This will convert all LaTeX-encoded characters
;; to their resp. visible encoding and store their original encodings.
;; This will also automatically register the encoding so that the
;; original encodings are restored when the buffer is saved.
;;
;;; Code:

(require 'iso-cvt)

(defgroup tex-smart-umlauts nil
  "Intelligent automatic conversion of TeX umlaut encodings."
  :group 'tex
  :prefix 'tex-smart-umlauts-)

(defcustom tex-smart-umlauts-encode 'auto
  "Encoding of new umlauts.
This option has three possible values. \"Never\" means that newly
inserted characters are never encoded, i.e. they are written
unmodified to the file. \"Always\" means each new character is
encoded according to `tex-smart-umlauts-encodings'. \"Automatic\"
means conversion happens if and only if there is at least one
decoded character in the buffer (i.e. some character that has
been decoded in the file when it has been loaded)."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "Automatic." :value auto)
                (const :tag "Always." :value t))
  :group 'tex-smart-umlauts)

(defcustom tex-smart-umlauts-encodings
  '(
    "^\\\\[^[:word:][:space:]][[:word:]]"
    "^\\\\[^[:word:][:space:]]{[[:word:]]}"
    "^{\\\\.*}$"
    )
  "A list of regular expressions that match your prefered encodings in order.
When the encoding for some character is determined, these regular
expressions are matched against all possible encodings of that
character. The first matching encoding (that matches the first
regular expression) will be used. If no regular expression
matches, some arbitrary encoding will be used."
  :type '(repeat regexp)
  :group 'tex-smart-umlauts)

(defcustom tex-smart-umlauts-german-shortcuts nil
  "If non-nil, also translate German shortcuts like \"a, \"o, \"s.
Note that this is often viable for tex-files, but inappropriate
for other files like BibTeX files, because the latter may contain
strings enclosed in double-quotes."
  :type 'boolean
  :group 'tex-smart-umlauts)

(defface tex-smart-umlauts-marks '((t :foreground "white" :background "red"))
  "Face for displaying umlaut conversions."
  :group 'tex-smart-umlauts)

(defvar tex-smart-umlauts--decode-table nil
  "Conversion table for tex decoding.
Each element of the list is two-element list of strings (tex
iso). The first element is the tex-encoding of a character, the
second element is the encoded character itself.")

(defvar tex-smart-umlauts--encode-table nil
  "Conversion table for tex encodings.
Same structure as `tex-smart-umlauts--decode-table', but used for
newly inserted characters.")

(defvar tex-smart-umlauts--mark-overlays nil
  "List of active overlays for showing stored conversions.")
(make-variable-buffer-local 'tex-smart-umlauts--mark-overlays)

(add-to-list 'format-alist
             '(tex-smart-umlauts "Persistent TeX (encoding)" nil
                                 tex-smart-umlauts-decode
                                 tex-smart-umlauts-encode-format t nil))

(defun tex-smart-umlauts--update-tables ()
  "Update internal encoding and decoding tables.
This function updates the internal encoding and decoding tables
from the tables defined in `iso-cvt'."
  (tex-smart-umlauts--update-decode-table)
  (tex-smart-umlauts--update-encode-table))

(defun tex-smart-umlauts--update-decode-table ()
  "Update internal decoding table.
This function updates the internal decoding table from the table
defined in `iso-cvt'. If `tex-smart-umlauts-german-shortcuts' is
non-nil, additional encodings like \"a are included."
  (let ((tab iso-tex2iso-trans-tab))
    (when tex-smart-umlauts-german-shortcuts
      (setq tab (append tab '(("\"a" "ä")
                              ("\"o" "ö")
                              ("\"u" "ü")
                              ("\"A" "Ä")
                              ("\"O" "Ö")
                              ("\"U" "Ü")
                              ("\"s" "ß")))))
    (setq tab (append tab '(("\\\\ss " "ß"))))
    (setq tex-smart-umlauts--decode-table
          (append
           (mapcar
            #'(lambda (pair)
                (let ((tex (car pair))
                      (iso (cadr pair)))
                  (list tex (propertize iso 'tex-smart-umlauts
                                        (tex-smart-umlauts--regexp-unquote tex)))))
            tab)
           (mapcar
            #'(lambda (char)
                (list (regexp-quote char)
                      (propertize char 'tex-smart-umlauts t)))
            (delete-dups (mapcar #'cadr tab)))))))

(defun tex-smart-umlauts--regexp-unquote (regexp)
  "Remove quoted characters from regular expression REGEXP.
This is the inverse of `regexp-quote', but note that it only
works with simple regular expression. The effect is to replace
double backslashes by a single backslash."
  (replace-regexp-in-string "\\\\\\\\" "\\" regexp t t))

(defun tex-smart-umlauts--update-encode-table ()
  "Update internal encoding table.
The internal encoding table is created by switching key and
values of the decoding table `tex-smart-umlauts--decode-table'. If
a character as several possible encodings, one is selected
according to `tex-smart-umlauts-encodings'."
  (let ((tbl (make-hash-table :test 'equal))
        encode-alist)
    ;; collect all encodings for a character in a list
    (dolist (pair tex-smart-umlauts--decode-table)
      (let ((key (concat (cadr pair)))
            (encoding (car pair)))
        (unless (string= key (car pair))
          (set-text-properties 0 (length key) nil key)
          (setq encoding (tex-smart-umlauts--regexp-unquote encoding))
          (puthash key (cons encoding (gethash key tbl)) tbl))))
    (setq tex-smart-umlauts--encode-table nil)
    ;; select the first matching encoding for each character
    (maphash
     #'(lambda (char encodings)
         (push (cons char
                     (catch 'done
                       (dolist (re-encoding tex-smart-umlauts-encodings)
                         (dolist (encoding encodings)
                           (when (string-match re-encoding encoding)
                             (throw 'done encoding))))
                       (car encodings)))
               encode-alist))
     tbl)
    (setq tex-smart-umlauts--encode-table
          (mapcar
           #'(lambda (pair)
               (let ((tex (car pair))
                     (iso (cadr pair)))
                 (list tex
                       (propertize iso
                                   'tex-smart-umlauts
                                   (cdr-safe (assoc iso encode-alist))))))
           tex-smart-umlauts--decode-table))))

;;;###autoload
(defun tex-smart-umlauts-decode (&optional from to)
  "Mark and decode all tex encoded characters in region.
Only characters between the buffer positions FROM and TO are
decoded. If FROM and TO are nil the whole buffer is decoded. This
function registers 'tex-smart-umlauts' has buffer file format, so
decoded characters are encoded again when the buffer is saved."
  (interactive "*")
  (if (region-active-p)
      (setq from (region-beginning)
            to (region-end))
    (setq from (or from (point-min))
          to (or to (point-max))))
  (let ((modified (buffer-modified-p)))
    (tex-smart-umlauts--translate from to tex-smart-umlauts--decode-table)
    (set-buffer-modified-p modified)
    (add-to-list 'buffer-file-format 'tex-smart-umlauts)
    (add-hook 'after-revert-hook #'tex-smart-umlauts-decode nil t)))

;;;###autoload
(defun tex-smart-umlauts-encode (&optional from to)
  "Mark and encode all characters in region.
Only characters between the buffer positions FROM and TO are
decoded. If FROM and TO are nil the whole buffer is encoded.
Characters, that have existed when the buffer has been loaded
will be encoded in their original encoding. Newly inserted
characters will be encoded according the rules of
`tex-smart-umlauts-encodings' and `tex-smart-umlauts-encode'.
This function should never be called directly but is called just
before the buffer is saved if the buffer has been decoded with
`tex-smart-umlauts-decode'."
  (interactive "*")
  (if (region-active-p)
      (setq from (region-beginning)
            to (region-end))
    (setq from (or from (point-min))
          to (or to (point-max))))
  (when (tex-smart-umlauts--should-encode from to)
    (let ((modified (buffer-modified-p))
          (beg (move-marker (make-marker) from))
          (end (move-marker (make-marker) to)))
      (tex-smart-umlauts--translate from to tex-smart-umlauts--encode-table)
      (set-buffer-modified-p modified)
      (setq from (marker-position beg)
            to (marker-position end))
      (move-marker beg nil)
      (move-marker end nil)))
  (tex-smart-umlauts--each-char
   from to
   #'(lambda (pos)
       (let ((encoded (get-text-property pos 'tex-smart-umlauts)))
         (when (eq encoded t)
           (setq encoded (buffer-substring-no-properties pos (1+ pos))))
         (when (eq encoded nil) (debug))
         (insert encoded)
         (delete-char 1)))))

(defun tex-smart-umlauts-encode-all (&optional from to)
  "Reencode all charactes in region.
Only characters between the buffer positions FROM and TO are
decoded. If FROM and TO are nil the whole buffer is encoded. The
original encodings of all characters in the region is dropped and
replaced by a new encoding according to the rules of
`tex-smart-umlauts-encodings' and `tex-smart-umlauts-encode'."
  (interactive "*")
  (let ((tex-smart-umlauts-encode t))
    (tex-smart-umlauts-unmark from to)
    (tex-smart-umlauts-encode from to)))

(defun tex-smart-umlauts--should-encode (from to)
  "Return if characters should be encoded.
This function returns non-nil if either
`tex-smart-umlauts-encode' is t or if it is 'auto' and at least
one character in the region FROM to TO has been decoded when the
buffer has been loaded."
  (or (eq tex-smart-umlauts-encode t)
      (and (eq tex-smart-umlauts-encode 'auto)
           (catch 'found
             (tex-smart-umlauts--each-char
              from to
              #'(lambda (pos)
                  (unless (eq (get-text-property pos 'tex-smart-umlauts) t)
                    (throw 'found t))))
             nil))))

(defun tex-smart-umlauts-encode-format (from to buffer)
  "Format function to be used in `format-alist'.
The region between FROM and TO should be encoded. The argument
BUFFER is ignored."
  (tex-smart-umlauts-encode from to))

;;;###autoload
(defun tex-smart-umlauts-unmark (&optional from to)
  "Remove encoding marks from all converted characters in region.
This makes tex-smart-umlauts consider all characters as if they
were modified, i.e. their original encoding will not be restored
when the buffer is stored. FROM and TO are the region in which
should be scanned. If FROM and TO are nil the whole buffer is
used."
  (interactive "*")
  (if (region-active-p)
      (setq from (region-beginning)
            to (region-end))
    (setq from (or from (point-min))
          to (or to (point-max))))
  (remove-text-properties from to '(tex-smart-umlauts)))

;;;###autoload
(defun tex-smart-umlauts-show-encodings ()
  "Show encodings of all umlauts in buffer."
  (interactive)
  (tex-smart-umlauts-hide-encodings)
  (tex-smart-umlauts--each-char
   (point-min) (point-max)
   #'(lambda (pos)
       (let ((ov (make-overlay pos (1+ pos)))
             (orig-txt (get-text-property pos 'tex-smart-umlauts)))
         (push ov tex-smart-umlauts--mark-overlays)
         (overlay-put ov
                      'after-string
                      (propertize (if (eq orig-txt t)
                                      (buffer-substring-no-properties pos (1+ pos))
                                    orig-txt)
                                  'face 'tex-smart-umlauts-marks))))))

;;;###autoload
(defun tex-smart-umlauts-hide-encodings ()
  "Hide encodings of all umlauts in buffer."
  (interactive)
  (dolist (ov tex-smart-umlauts--mark-overlays)
    (delete-overlay ov))
  (setq tex-smart-umlauts--mark-overlays nil))

(defun tex-smart-umlauts--each-char (from to function)
  "Iterates over all encoded characters in region.
The region between FROM and TO is scanned for characters that
have an associated encoding. The function FUNCTION is called once
for each character with one parameter, the buffer position of
this character."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (let ((marker (make-marker))
            (beg (if (get-text-property (point-min) 'tex-smart-umlauts)
                     (point-min)
                   (next-single-property-change (point-min) 'tex-smart-umlauts))))
        (while beg
          (let* ((end (next-single-property-change beg 'tex-smart-umlauts)))
            (goto-char beg)
            (move-marker marker beg)
            (dotimes (i (- end beg))
              (let ((pos (marker-position marker)))
                (move-marker marker (1+ pos))
                (funcall function pos)))
            (setq beg (marker-position marker))
            (unless (get-text-property beg 'tex-smart-umlauts)
              (setq beg (next-single-property-change beg 'tex-smart-umlauts)))))
        (move-marker marker nil)))))

(defun tex-smart-umlauts--translate (from to trans-tab)
  "Translate between FROM and TO using the translation table TRANS-TAB."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (let ((buffer-read-only nil)
            (case-fold-search nil))
        (dolist (trans-this trans-tab)
          (save-excursion
            (while (re-search-forward (car trans-this) nil t)
              (unless (get-text-property (match-beginning 0) 'tex-smart-umlauts)
                (replace-match (cadr trans-this) t t))))))
      (point-max))))

(tex-smart-umlauts--update-tables)

(provide 'tex-smart-umlauts)

;;; tex-smart-umlauts.el ends here
