;;; auto-preamble.el -- Auto-type the preamble of source code file.
;; Time-stamp: <2012-02-02 14:20:04 hqwrong>
;; ;Copyright (C) 2012 by 王泓钦(hqwrong) <hq.wrong@gmail.com>
;; Author: 王泓钦(hqwrong) <hq.wrong@gmail.com>
;; Version: 0.1

;;; Comment:
;; Put it in your load directory,and require it.
;; Use command `auto-preamble' to auto type preamble.
;; So far, it only supports Lisp source file.
;; Enjoy!

;;; License: GPL v3 or later
;;    
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or (at
;;    your option) any later version.
;;    
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;    
;;          

;;; Code:


(defvar ap-sections
      '(Name Time-Stamp Copyright Author Version Comment License Code File-End)
      "Auto-Preamble will prepare preamble based on this list. ")

(defvar ap-default-mail-addr 
   "hq.wrong@gmail.com")
(defvar ap-default-author
   "王泓钦(hqwrong)")
(defvar ap-license-alist
   '(("GPL v3 or later" . license-gplv3)
     ("WTFPL" . license-WTFPL)
     ("Apache v2.0" . license-apachev2)
     ("BSD 2 clause" . license-bsd-2)
     ("MIT" . license-mit)))
(defvar ap-default-license
   "GPL v3 or later")
(defvar license-manifesto-offset
      3
      "how many spaces to shift for license manifesto to look pretty.")
(defvar license-apachev2
   "
   Licensed under the Apache License, Version 2.0 (the \"License\");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

          http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an \"AS IS\" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
")
(defvar license-gplv3
   "
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
")
(defvar license-wtfpl
   "
Everyone is permitted to copy and distribute verbatim or modified
copies of this license document, and changing it is allowed as long
as the name is changed.

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. You just DO WHAT THE FUCK YOU WANT TO.
")
(defvar license-bsd-2
   "
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met: 

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials
      provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
   CONTRIBUTORS \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES,
   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
   BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING NEGLIGENCE OR
   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
   ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.

   The views and conclusions contained in the software and
   documentation are those of the authors and should not be
   interpreted as representing official policies, either expressed
   or implied, of the FreeBSD Project.

")

(defvar license-mit
   "
   Permission is hereby granted, free of charge, to any person obtaining a copy of
   this software and associated documentation files (the \"Software\"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is furnished to do
   so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
")

(defun ap-section-prefix ()
   "prefix for header"
   ";;; ")
(defun ap-content-prefix ()
   "prefix for content"
   ";; ")

(define-skeleton ap-name-skeleton
   "skeleton for name section"
   "One Line Description: "
   (ap-section-prefix)
   (file-name-nondirectory (buffer-file-name (current-buffer)))
   " -- "
   str
   "\n")
(define-skeleton ap-time-stamp-skeleton
   "skeleton for time-stamp"
   nil
   (ap-content-prefix)
   "Time-stamp: <>"
   '(time-stamp)
   "\n")
(define-skeleton ap-version-skeleton
   ""
   "Version Number: "
   (ap-content-prefix)
   "Version: "
   str
   "\n")
(define-skeleton ap-copyright-skeleton
   "skeleton for copyright"
   nil
   (ap-content-prefix)
   '(copyright)
   " <"
   ap-default-mail-addr
   ">"
   "\n"
   )
(define-skeleton ap-author-skeleton
   "skeleton for author"
   nil
   (ap-content-prefix)
   "Author: "
   (read-from-minibuffer "Author: "
                         ap-default-author)
    " <"
    (read-from-minibuffer "email adrress: " ap-default-mail-addr)
    ">;\n"
   ";;         "
   ("Author(RET for done): "
    str 
    " <"
    (read-from-minibuffer "email adrress: ")
    ">;\n"
    ";;         ")
   -13                                  ;delete last ';'
   "\n"
)

(define-skeleton ap-maintainer-skeleton
   "skeleton for author"
   nil
   (ap-content-prefix)
   "Maintainer: "
   (read-from-minibuffer "Maintainer: "
                         ap-default-author)
    " <"
    (read-from-minibuffer "email adrress: " ap-default-mail-addr)
    ">;\n"
   ";;         "
   ("Maintainer(RET for done): "
    str 
    " <"
    (read-from-minibuffer "email adrress: ")
    ">;\n"
    ";;         ")
   -13                                  ;delete last ';'
   "\n"
)
(define-skeleton ap-comment-skeleton
   ""
   nil
   "\n"
   (ap-section-prefix)
   "Comment:"
   "\n")

(define-skeleton ap-license-skeleton
   "skeleton for license"
   nil
   "\n"
   (ap-section-prefix)
   "License: "
   (setq v1 (completing-read (format "License(RET for %s): "
                                     ap-default-license)
                             (mapcar 'car ap-license-alist)
                             nil
                             t
                             nil nil
                             ap-default-license))
   "\n"
   '(setq v2 (point))
   '(setq lp (length (ap-content-prefix)))
   (let (
         (x (assoc v1 ap-license-alist)))
      (if (null x)
            nil
         (cdr x)))
   "\n"
   (make-string (+ lp license-manifesto-offset) ?\s)

   '(progn
       (push-mark (line-beginning-position))
       (open-rectangle v2 (point) t)
       (string-rectangle (point)
                         (+ (mark) lp)   
                         (ap-content-prefix))
       (goto-char (mark))
       (end-of-line)
       (pop-mark))
   "\n"
   )
      
(define-skeleton ap-code-skeleton
   ""
   nil
   "\n"
   (ap-section-prefix)
   "Code:\n\n"
   )
(defun ap-file-end-skeleton (arg)
   (when (consp arg)
      (end-of-buffer))
   (skeleton-insert
    '(nil "\n"
          (ap-section-prefix)
          (file-name-nondirectory (buffer-file-name (current-buffer)))
          " ends here.")))
(defun auto-preamble (arg)
   (interactive "P")
   (dolist (x ap-sections)
      (let* (
             (name (downcase (symbol-name x)))
             (fun (intern (concat "ap-" name "-skeleton"))))
         (if (equal name "file-end")
               (funcall fun arg)
            (funcall fun)))))
(provide 'auto-preamble)

;;; auto-preamble.el ends here.
