;;; sentence-highlight.el --- highlight the current sentence

;; Copyright (C) 2011 Simon Belak <sb@hekovnik.si>
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation  and/or other materials provided with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; Version: 0.1
;; Author: Simon Belak <sb@hekovnik.si>
;; Keywords: plain text, writing, highlight, editing, focus

;; This file is not part of GNU Emacs.

;; Getting Started
;; ---------------

;; To start using Sentence Highlight Mode, place it somewhere in your Emacs
;; load-path and add the line

;;    (require 'sentence-highlight)

;; in your `.emacs` file.

;; Enable it on the fly with `M-x sentence-highlight-mode RET' or always enable
;; it in a major mode `M' (e.g., `text') with:

;;    (add-hook M-mode-hook 'sentence-highlight-mode)

;; Sentence Highlight Mode relies on Emacs’s sentence detection, which by
;; default expects sentences to end with terminal punctuation followed by two
;; spaces or a new line. The former can be changed by setting
;; `sentence-end-double-space' to nil. See:

;;    http://www.gnu.org/software/libtool/manual/emacs/Sentences.html

;; for more details.

;; History
;; -------

;; Based on a hack from http://www.emacswiki.org/emacs/SentenceHighlight
;; The initial version, 0.1, was released on Octorber 21, 2011.

;;; Code:

(defgroup sentence-highlight nil
  "Sentence highlight mode."
  :group 'local)

;; Faces

(defgroup sentence-highlight-faces nil
  "Faces used in sentence highlight mode"
  :group 'sentence-highlight
  :group 'faces)

(defface sentence-highlight-face
  '((t :inherit font-lock-keyword-face))
  "Face for highlight."
  :group 'sentence-highlight-faces)
(setq sentence-highlight-face 'sentence-highlight-face)

(defvar sentence-highlight-mode nil)
(make-variable-buffer-local 'sentence-highlight-mode)

(setq sentence-extent (make-overlay 0 0))
(overlay-put sentence-extent 'face sentence-highlight-face)
(setq sentence-end "[^.].[.?!]+\\([]\"')}]*\\|<[^>]+>\\)\\($\\| $\\|\t\\| \\)[ \t\n]*")

(defun sentence-begin-pos ()
  (save-excursion
    (unless (= (point) (point-max))
      (forward-char))
    (backward-sentence)
    (point)))

(defun sentence-end-pos ()
  (save-excursion
    (unless (= (point) (point-max))
      (forward-char))
    (backward-sentence)
    (forward-sentence)
    (point)))

(defun sentence-highlight-current (&rest ignore)
  "Highlight the current sentence."
  (and sentence-highlight-mode
       (> (buffer-size) 0)
       (point-marker)
       (move-overlay sentence-extent
		     (sentence-begin-pos)
		     (sentence-end-pos)
		     (current-buffer))))

(defun sentence-highlight-mode (&optional arg)
  "Minor mode for highlighting current sentence."
  (interactive "p")
  (setq sentence-highlight-mode (if arg
				    (> (prefix-numeric-value arg) 0)
				  (not sentence-highlight-mode)))
  (if sentence-highlight-mode
      (progn
	(add-hook 'post-command-hook 'sentence-highlight-current nil t))
    (remove-hook 'post-command-hook 'sentence-highlight-current t)))

(unless (assq 'sentence-highlight-mode minor-mode-alist)
  (setq minor-mode-alist (cons '(sentence-highlight-mode " Sentence")
			       minor-mode-alist)))

(provide 'sentence-highlight)

;;; sentence-highlight.el ends here
