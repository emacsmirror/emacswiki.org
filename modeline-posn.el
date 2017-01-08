(defcustom modelinepos-style '((if modelinepos-rect-p " %d rows, %d cols" " %d ch, %d l")
                               (if modelinepos-rect-p
                                   (count-lines (region-beginning) (region-end))
                                 (abs (- (mark t) (point))))
                               (if modelinepos-rect-p
                                   (if (fboundp 'rectangle--pos-cols) ; Emacs 24.4+
                                       (let ((rpc  (rectangle--pos-cols (region-beginning) (region-end))))
                                         (abs (- (car rpc) (cdr rpc))))
                                     (let ((start  (region-beginning))
                                           (end    (region-end))
                                           startcol endcol)
                                       (save-excursion
                                         (goto-char start)
                                         (setq startcol   (current-column))
                                         (beginning-of-line)
                                         (goto-char end)
                                         (setq endcol  (current-column))
                                         (when (< endcol startcol) ; Ensure start column is the left one.
                                           (let ((col  startcol))
                                             (setq startcol  endcol
                                                   endcol    col)))
                                         (abs (- startcol endcol)))))
                                 (count-lines (mark t) (point))))
  "*What info to include about the region size, in mode-line.
Value `chars+lines' means print the number of characters and lines or,
if a rectangle command is invoked, the number of rows and columns.

In general, the value is a format string followed by however many
sexps the strings expects as arguments."
  :type '(choice
          (const :tag "Characters: \"_ chars\""
           (" %d chars" (abs (- (mark t) (point)))))
          (const :tag "Bytes: \"_ bytes\""
           (" %d bytes" (let* ((use-empty-active-region  modelinepos-empty-region-flag)
                               (strg                     (if (use-region-p)
                                                             (buffer-substring-no-properties
                                                              (region-beginning) (region-end))
                                                           "")))
                          (string-bytes strg))))
          (const :tag "Chars & Lines: \"_ ch, _ l\" or Rows & Columns: \"_ rows, _ cols\""
           ((if modelinepos-rect-p " %d rows, %d cols" " %d ch, %d l")
            (if modelinepos-rect-p
                (count-lines (region-beginning) (region-end))
              (abs (- (mark t) (point))))
            (if modelinepos-rect-p
                (if (fboundp 'rectangle--pos-cols) ; Emacs 24.4+
                    (let ((rpc  (rectangle--pos-cols (region-beginning) (region-end))))
                      (abs (- (car rpc) (cdr rpc))))
                  (let ((start  (region-beginning))
                        (end    (region-end))
                        startcol endcol)
                    (save-excursion
                      (goto-char start)
                      (setq startcol   (current-column))
                      (beginning-of-line)
                      (goto-char end)
                      (setq endcol  (current-column))
                      (when (< endcol startcol) ; Ensure start column is the left one.
                        (let ((col  startcol))
                          (setq startcol  endcol
                                endcol    col)))
                      (abs (- startcol endcol)))))
              (count-lines (mark t) (point)))))
          (list :tag "Customized format"
           (string :tag "Format string")
           (repeat :inline t (sexp :tag "Sexp argument for format string"))))
  :group 'Modeline :group 'Convenience :group 'Help)
