;;; trantab.el --- translation table bundles

;; Copyright (C) 2008  MATSUYAMA Tomohiro

;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defvar translation-table-subtype 'translation-table)

(defun remap-translation-table (char-table mapper-char-table)
  (let ((mapped (make-char-table translation-table-subtype)))
    (set-char-table-parent mapped char-table)
    (map-char-table (lambda (key value)
                      (let ((translation (aref char-table key)))
                        (when translation
                          (aset mapped key nil)
                          (aset mapped value translation))))
                    mapper-char-table)
    mapped))

(defmacro define-translation-table (name &rest definition)
  (declare (indent defun))
  `(progn
     (defvar ,name (make-char-table translation-table-subtype))
     ,@(mapcar (lambda (pair)
                 (list 'aset name (car pair) (cdr pair)))
               definition)))

(define-translation-table translation-table-dvorak-us
  (?- . ?\[)
  (?_ . ?\{)
  (?= . ?\])
  (?+ . ?\})
  (?q . ?')
  (?Q . ?\")
  (?w . ?,)
  (?W . ?<)
  (?e . ?.)
  (?E . ?>)
  (?r . ?p)
  (?R . ?P)
  (?t . ?y)
  (?T . ?Y)
  (?y . ?f)
  (?Y . ?F)
  (?u . ?g)
  (?U . ?G)
  (?i . ?c)
  (?I . ?C)
  (?o . ?r)
  (?O . ?R)
  (?p . ?l)
  (?P . ?L)
  (?\[ . ?/)
  (?\{ . ??)
  (?\] . ?=)
  (?\} . ?+)
  (?a . ?a)
  (?A . ?A)
  (?s . ?o)
  (?S . ?O)
  (?d . ?e)
  (?D . ?E)
  (?f . ?u)
  (?F . ?U)
  (?g . ?i)
  (?G . ?I)
  (?h . ?d)
  (?H . ?D)
  (?j . ?h)
  (?J . ?H)
  (?k . ?t)
  (?K . ?T)
  (?l . ?n)
  (?L . ?N)
  (?\; . ?s)
  (?\: . ?S)
  (?\' . ?-)
  (?\" . ?_)
  (?z . ?\;)
  (?Z . ?\:)
  (?x . ?q)
  (?X . ?Q)
  (?c . ?j)
  (?C . ?J)
  (?v . ?k)
  (?V . ?K)
  (?b . ?x)
  (?B . ?X)
  (?n . ?b)
  (?N . ?B)
  (?m . ?m)
  (?M . ?M)
  (?, . ?w)
  (?< . ?W)
  (?. . ?v)
  (?> . ?V)
  (?/ . ?z)
  (?? . ?Z))

(provide 'trantab)
;;; trantab.el ends here
