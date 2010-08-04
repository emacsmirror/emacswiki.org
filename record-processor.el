;;; record-processor.el

;; Copyright (C) 2009  Mike Mattie

;; Author: Mike Mattie
;; License: LGPL-v3
;; Version: 0.0.1a

;;; Code:

(defconst record-processor-version "0.0.1a")

;;
;; extractor
;;

(defun record-processor-make-extractor ( field extractor-fn &rest sub-extractors )
  "record-processor-make-extractor FIELD FN EXTRACTORS

   Create an extractor. Field is a string identifying the data extracted.

   BOUNDS-FN takes two parameters: START END bounding a region to search.
   It returns a three element list consisting of START END, and any extracted
   data as the final value.

   EXTRACTORS is a list of sub-extractors. They will be executed within the
   bounds established by BOUNDS-FN allowing extractors to be nested.
  "
  (list field extractor-fn sub-extractors))

(defmacro record-processor-lookup ( pointer-list )
  (let
    ((keys   (split-string pointer-list "->" t))
     (lookup nil))

    ;; create the first lookup which references the table
    ;; parameter directly.
    (setq lookup `(cdr table))

    (when keys
      (mapc
        (lambda ( lookup-key )
          (setq lookup
            `(cdr-safe (assoc ,lookup-key ,lookup))))
        keys))

    `(lambda ( table )
       ,lookup) ))

(defun record-processor-output ( start end &optional data )
  (list start end data))

(defun recp-extractor-field ( extractor )
  (car extractor))

(defun recp-extractor-fn ( extractor )
  (cadr extractor))

(defun recp-nested-extractors ( extractor )
  (caddr extractor))

(defun recp-ex-output-table ( ex-output )
  "recp-ex-output-table

  "
  (let
    ((table nil)
     (values ex-output))

    (mapc (lambda ( key )
            (when (car values)
              (push (cons key (car values)) table))
            (setq values (cdr values)) ) '("start" "end" "data"))
    table))

(defun recp-run-extractor ( start end extractor )
  (goto-char start)
  (catch 'exit
    (let
      ;; output is a (start end data) list.
      ((ex-output (funcall (recp-extractor-fn extractor) start end)))

      ;; if anything was found construct a table of the extractor's results.
      (when ex-output
        (unless (listp ex-output)
          (throw 'exit end))

        (let
          ((table (recp-ex-output-table ex-output)))

          ;; execute any sub-extractors. Add non-nil output as keys
          ;; in the table.
          (when (recp-nested-extractors extractor)
            (mapc (lambda ( nested )
                    (let
                      ((ex-output (recp-run-extractor
                                    (cdr (assoc "start" table))
                                    (cdr (assoc "end"   table))
                                    nested)))
                      (unless (listp ex-output)
                        (throw 'exit (cdr (assoc "end" table))))
                      (push ex-output table)))
              (recp-nested-extractors extractor)))

          ;; return the extractor output as an assoc table entry.
          (cons (recp-extractor-field extractor) table) )) )))

(defun record-processor ( begin end &rest record-extractors )
  "record-processor

  "
  (let
    ((extracted nil)
     (records   nil)
     (pos       begin))

    (catch 'done
      (while pos
        ;; search through the extractors for the first to return non-nil.
        (catch 'record-found
          (mapc
            (lambda ( extractor )
              (when (setq extracted (recp-run-extractor pos end extractor)))
              (throw 'record-found t))
            record-extractors))

        ;; if none returned non-nil terminate, there is nothing left to do.
        (unless extracted (throw 'done records))

        ;; results are either a data list, or a integer indicating where to
        ;; position the cursor after a skipped record.
        (setq pos
          (if (listp extracted)
            (progn
              (push extracted records)
              (cdr (assoc "end" (cdr extracted))))
            extracted))

        (unless (< pos (buffer-end 1)) (throw 'done records)) )) ))

(provide 'record-processor)
