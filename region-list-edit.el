;;;Updated in 2016.5.27
;;;This package can add/delete a region into/from a region list, such as ‘((4 . 7) (11 . 15) (17 . 17) (20 . 25))
;;The member of regioin-list always include the node of the start and end. for example: (3 . 5), then 3 and 5 is both belong to this region.

;;;###autoload
(defun region-list-edit-add (regions-list todo &optional merge)
  "Add a region todo to regions-list, the start and end node of todo, is included"
  (interactive)
  (let (beg-node-to-insert beg-node-included-p end-node-to-insert end-node-included-p beg-have-find-p end-have-find-p temp-node temp-list temp-pre temp-next)
    (dotimes (Num (length regions-list))
      (cond ((and (not beg-have-find-p);;in the inner of Num'th node,include the start and end of Num'th node
                  (>= (car todo) (car (nth Num regions-list)))
                  (<= (car todo) (cdr (nth Num regions-list))))
             (setq beg-have-find-p t)
             (setq beg-node-to-insert Num)
             (setq beg-node-included-p t))

            ((and (not beg-have-find-p);;before the Num'th node
                  (< (car todo) (cdr (nth Num regions-list))))
             (setq beg-have-find-p t)
             (setq beg-node-to-insert Num)
             (setq beg-node-included-p nil))
            (t nil)))
    (dotimes (Num (length regions-list))
      (cond ((and (not end-have-find-p);;in the inner of Num'th node
                  (>= (cdr todo) (car (nth Num regions-list)))
                  (<= (cdr todo) (cdr (nth Num regions-list))))
             (setq end-have-find-p t)
             (setq end-node-to-insert Num)
             (setq end-node-included-p t))

            ((and (not end-have-find-p);;before the Num'th node
                  (< (cdr todo) (car (nth Num regions-list))))
             (setq end-have-find-p t)
             (setq end-node-to-insert Num)
             (setq end-node-included-p nil))
            
            (t nil)))

    (cond ((not beg-have-find-p);;start and end both bigger than the last region in list
           (setq temp-list (list todo))
           (if regions-list 
               (setcdr (nthcdr (1- (length regions-list)) regions-list) temp-list)
             (setq regions-list temp-list)))

          ((and beg-have-find-p;;end is bigger than the last region, start is in one node
                beg-node-included-p
                (not end-have-find-p))
           (setq temp-node (cons (car (nth beg-node-to-insert regions-list))(cdr todo)))
           (setq temp-list (nthcdr beg-node-to-insert regions-list))
           (setcar temp-list temp-node)
           (setcdr temp-list nil))
          
          ((and beg-have-find-p;;end is bigger than the last region, start is between nodes
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                (not end-have-find-p))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setcdr temp-pre (list todo)))

          ((and beg-have-find-p;;end is bigger than the last region, start is before all node
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                (not end-have-find-p))
           (setq regions-list (list todo)))

          ((and beg-have-find-p ;;start and end both in nodes, even end is in the first node
                beg-node-included-p
                end-have-find-p
                end-node-included-p)
           (setq temp-node (cons (car (nth beg-node-to-insert regions-list)) (cdr (nth end-node-to-insert  regions-list))))
           (setq temp-list (nthcdr beg-node-to-insert regions-list))
           (setcar temp-list temp-node)
           (setcdr temp-list (nthcdr (1+ end-node-to-insert) regions-list)))

          ((and beg-have-find-p;;start is between nodes, end is in node, even end is in the last node
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                end-have-find-p
                end-node-included-p)
           (setq temp-node (cons (car todo) (cdr (nth end-node-to-insert regions-list))))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setq temp-next (nthcdr (1+ end-node-to-insert) regions-list))
           (setcdr temp-pre (cons temp-node temp-next)))

          ((and beg-have-find-p;;start is before all nodes, end is in node,even end is in the last node
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                end-have-find-p
                end-node-included-p)
           (setq temp-node (cons (car todo) (cdr (nth end-node-to-insert regions-list))))
           (setq temp-next (nthcdr (1+ end-node-to-insert) regions-list))
           (setq regions-list (cons temp-node temp-next)))

          ((and beg-have-find-p ;;both before the first node
                end-have-find-p
                (not end-node-included-p)
                (= end-node-to-insert 0))
           (setq regions-list (cons todo regions-list)))

          ((and beg-have-find-p;;start and end both are between nodes
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))           
           (setq temp-next (nthcdr end-node-to-insert regions-list))
           (setcdr temp-pre (cons todo temp-next)))

          ((and beg-have-find-p;;start is before all nodes, end is between nodes
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq temp-list (nthcdr end-node-to-insert regions-list))
           (setq regions-list (cons todo temp-list))
           )

          ((and beg-have-find-p;;start is in node,end is between nodes
                beg-node-included-p
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq temp-node (cons (car (nth beg-node-to-insert regions-list)) (cdr todo)))
           (setq temp-list (nthcdr beg-node-to-insert regions-list))
           (setcar temp-list temp-node)
           (setcdr temp-list (nthcdr end-node-to-insert regions-list)))
          (t nil))
  (when merge
    (setq temp-node regions-list)
    (while temp-node
      (setq temp-next (cdr temp-node))
      (if (and temp-next
               (= (1+ (cdar temp-node)) (caar temp-next)))
        (progn (setcar temp-node (cons (caar temp-node) (cdar temp-next)))
               (setcdr temp-node (cdr temp-next)))
        (setq temp-node temp-next)
        )))
 regions-list
))

;;(setq region-list-edit-example '((4 . 7) (11 . 15) (17 . 17) (20 . 25)))
;;start and end both bigger than the last region in list
;;(region-list-edit-add region-list-edit-example '(26 . 27))
;;end is bigger than the last region, start is in one node
;;(region-list-edit-add region-list-edit-example '(25 . 27))
;;(region-list-edit-add region-list-edit-example '(23 . 27))
;;(region-list-edit-add region-list-edit-example '(4 . 27))
;;end is bigger than the last region, start is between nodes
;;(region-list-edit-add region-list-edit-example '(5 . 27))
;;end is bigger than the last region, start is before all node
;;(region-list-edit-add region-list-edit-example '(3 . 27))

;;start and end both in nodes, even end is in the first node
;;(region-list-edit-add region-list-edit-example '(4 . 17))
;;(region-list-edit-add region-list-edit-example '(4 . 4))
;;(region-list-edit-add region-list-edit-example '(4 . 25))
;;start is between nodes, end is in node, even end is in the last node
;;(region-list-edit-add region-list-edit-example '(16 . 22))
;;start is before all nodes, end is in node,even end is in the last node
;;(region-list-edit-add region-list-edit-example '(1 . 22))
;;both before the first node
;;(region-list-edit-add region-list-edit-example '(1 . 1))
;;(region-list-edit-add region-list-edit-example '(2 . 3))

;;start and end both are between nodes----end is between nodes
;;(region-list-edit-add region-list-edit-example '(10 . 19))
;;start is before all nodes, end is between nodes
;;(region-list-edit-add region-list-edit-example '(1 . 19))
;;start is in node,end is between nodes
;;(region-list-edit-add region-list-edit-example '(5 . 19))

;;(setq region-list-edit-example nil)
;;(setq region-list-edit-example (region-list-edit-add region-list-edit-example '(5 . 19)))
;;(region-list-edit-delete region-list-edit-example '(5 . 19))

;;(setq region-list-edit-example '((5 . 19)))
;;(setq region-list-edit-example (region-list-edit-add region-list-edit-example '(5 . 19)))
;;(region-list-edit-delete region-list-edit-example '(6 . 18))
;;(region-list-edit-delete region-list-edit-example '(6 . 19))
;;(region-list-edit-delete region-list-edit-example '(5 . 18))
;;(setq region-list-edit-example nil)


;;;###autoload
(defun region-list-edit-delete (regions-list todo)
  "Delete a region todo from regions-list, the start and end node of todo, is included"
  (interactive)
  (let (beg-node-to-insert beg-node-included-p end-node-to-insert end-node-included-p beg-have-find-p end-have-find-p temp-node temp-list temp-pre temp-next cartodo cdrtodo)
    ;; because (copy-marker (1- (car todo))) minimum=1, even (car todo) ==1, so need cartodo just for boundary check.
    ;; same as cdrtodo
    (setq cartodo (if (markerp (car todo));;if is marker, then keep as marker
		      (1- (marker-position (car todo)))
		    (1- (car todo))))
    (setq cdrtodo (if (markerp (cdr todo))
		      (1+ (marker-position (cdr todo)))
		    (1+ (cdr todo))))
    
    (setq todo (cons (if (markerp (car todo));;if is marker, then keep as marker
			 (copy-marker (1- (car todo)))
		       (1- (car todo)))
		     (if (markerp (cdr todo))
			 (copy-marker (1+ (cdr todo)))
		       (1+ (cdr todo)))))
    (dotimes (Num (length regions-list))
      (cond ((and (not beg-have-find-p);;in the inner of Num'th node,include the start and end of Num'th node
                  (>= cartodo (car (nth Num regions-list)))
                  (<= cartodo (cdr (nth Num regions-list))))
             (setq beg-have-find-p t)
             (setq beg-node-to-insert Num)
             (setq beg-node-included-p t))

            ((and (not beg-have-find-p);;before the Num'th node
                  (< cartodo (cdr (nth Num regions-list))))
             (setq beg-have-find-p t)
             (setq beg-node-to-insert Num)
             (setq beg-node-included-p nil))
            (t nil)))
    
    (dotimes (Num (length regions-list))
      (cond ((and (not end-have-find-p);;in the inner of Num'th node
                  (>= cdrtodo (car (nth Num regions-list)))
                  (<= cdrtodo (cdr (nth Num regions-list))))
             (setq end-have-find-p t)
             (setq end-node-to-insert Num)
             (setq end-node-included-p t))

            ((and (not end-have-find-p);;before the Num'th node
                  (< cdrtodo (car (nth Num regions-list))))
             (setq end-have-find-p t)
             (setq end-node-to-insert Num)
             (setq end-node-included-p nil))
            
            (t nil)))

    (cond ((not beg-have-find-p);;start and end both bigger than the last region in list
           (setq temp-list (list todo))
           nil)

          ((and beg-have-find-p;;end is bigger than the last region, start is in one node
                beg-node-included-p
                (not end-have-find-p))
           (setq temp-node (cons (car (nth beg-node-to-insert regions-list))(car todo)))
           (setq temp-list (nthcdr beg-node-to-insert regions-list))
           (setcar temp-list temp-node)
           (setcdr temp-list nil))
          
          ((and beg-have-find-p;;end is bigger than the last region, start is between nodes
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                (not end-have-find-p))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setcdr temp-pre nil))

          ((and beg-have-find-p;;end is bigger than the last region, start is before all node
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                (not end-have-find-p))
           (setq regions-list nil))

          ((and beg-have-find-p ;;start and end both in nodes, even end is in the first node  (4 . 6) - (4 . 6)
                beg-node-included-p
                end-have-find-p
                end-node-included-p)
           (setq temp-pre (cons (car (nth beg-node-to-insert regions-list)) (car todo)))
           (setq temp-list (nthcdr beg-node-to-insert regions-list))
           (setq temp-node (cons (cdr todo) (cdr (nth end-node-to-insert regions-list))))
           (setq temp-next (nthcdr (1+ end-node-to-insert) regions-list))
           (setq temp-next (cons temp-node temp-next))
           (setcar temp-list temp-pre)
           (setcdr temp-list temp-next))

          ((and beg-have-find-p;;start is between nodes, end is in node, even end is in the last node
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                end-have-find-p
                end-node-included-p)
           (setq temp-node (cons (cdr todo) (cdr (nth end-node-to-insert regions-list))))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setq temp-next (nthcdr (1+ end-node-to-insert) regions-list))
           (setcdr temp-pre (cons temp-node temp-next)))

          ((and beg-have-find-p;;start is before all nodes, end is in node,even end is in the last node
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                end-have-find-p
                end-node-included-p)
           (setq temp-node (cons (cdr todo) (cdr (nth end-node-to-insert regions-list))))
           (setq temp-next (nthcdr end-node-to-insert regions-list))
           (setcar temp-next temp-node)
           (setq regions-list temp-next))

          ((and beg-have-find-p ;;both before the first node
                end-have-find-p
                (not end-node-included-p)
                (= end-node-to-insert 0))
           nil)

          ((and beg-have-find-p;;start and end both are between nodes
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))           
           (setq temp-next (nthcdr end-node-to-insert regions-list))
           (setcdr temp-pre temp-next))

          ((and beg-have-find-p;;start is before all nodes, end is between nodes
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq regions-list (nthcdr end-node-to-insert regions-list))
           )

          ((and beg-have-find-p;;start is in node,end is between nodes
                beg-node-included-p
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq temp-node (cons (car (nth beg-node-to-insert regions-list)) (car todo)))
           (setq temp-next (nthcdr end-node-to-insert regions-list))
           (setq temp-pre (nthcdr beg-node-to-insert regions-list))
           (setcar temp-pre temp-node)
           (setcdr temp-pre temp-next))
          (t nil))
    regions-list))

;;(setq region-list-edit-example '((4 . 7) (11 . 15) (17 . 17) (20 . 25)))
;;start and end both bigger than the last region in list
;;(region-list-edit-delete region-list-edit-example '(27 . 28))
;;end is bigger than the last region, start is in one node
;;(region-list-edit-delete region-list-edit-example '(25 . 27))
;;(region-list-edit-delete region-list-edit-example '(18 . 27))
;;(region-list-edit-delete region-list-edit-example '(4 . 27))
;;(region-list-edit-delete region-list-edit-example '(5 . 25))
;;end is bigger than the last region, start is between nodes
;;(region-list-edit-delete region-list-edit-example '(16 . 27))
;;end is bigger than the last region, start is before all node
;;(region-list-edit-delete region-list-edit-example '(3 . 27))

;;start and end both in nodes, even end is in the first node
;;(region-list-edit-delete region-list-edit-example '(6 . 17))
;;(region-list-edit-delete region-list-edit-example '(4 . 4))
;;(region-list-edit-delete region-list-edit-example '(4 . 25))

;;start is between nodes, end is in node, even end is in the last node
;;(region-list-edit-delete region-list-edit-example '(9 . 22))
;;start is before all nodes, end is in node,even end is in the last node
;;(region-list-edit-delete region-list-edit-example '(1 . 22))
;;both before the first node
;;(region-list-edit-delete region-list-edit-example '(1 . 4))
;;(region-list-edit-delete region-list-edit-example '(2 . 3))

;;start and end both are between nodes----end is between nodes
;;(region-list-edit-delete region-list-edit-example '(10 . 18))
;;(region-list-edit-delete region-list-edit-example '(10 . 11))
;;start is before all nodes, end is between nodes
;;(region-list-edit-delete region-list-edit-example '(1 . 14))
;;start is in node,end is between nodes
;;(region-list-edit-delete region-list-edit-example '(5 . 19))

;;((1 . 1))
;;()=nil


(defun region-list-edit-delete-greed (regions-list todo)
  "Delete a region todo from regions-list, the start and end node of todo, is included, and delete any region of whole that cross with the todo"
  (interactive)
  (let (beg-node-to-insert beg-node-included-p end-node-to-insert end-node-included-p beg-have-find-p end-have-find-p temp-node temp-list temp-pre temp-next cartodo cdrtodo)
    ;; because (copy-marker (1- (car todo))) minimum=1, even (car todo) ==1, so need cartodo just for boundary check.
    ;; same as cdrtodo
    (setq cartodo (if (markerp (car todo));;if is marker, then keep as marker
		      (1- (marker-position (car todo)))
		    (1- (car todo))))
    (setq cdrtodo (if (markerp (cdr todo))
		      (1+ (marker-position (cdr todo)))
		    (1+ (cdr todo))))

    (setq todo (cons (if (markerp (car todo));;if is marker, then keep as marker
			 (copy-marker (1- (car todo)))
		       (1- (car todo)))
		     (if (markerp (cdr todo))
			 (copy-marker (1+ (cdr todo)))
		       (1+ (cdr todo)))))
    (dotimes (Num (length regions-list))
      (cond ((and (not beg-have-find-p);;in the inner of Num'th node,include the start and end of Num'th node
                  (>= cartodo (car (nth Num regions-list)))
                  (<= cartodo (cdr (nth Num regions-list))))
             (setq beg-have-find-p t)
             (setq beg-node-to-insert Num)
             (setq beg-node-included-p t))

            ((and (not beg-have-find-p);;before the Num'th node
                  (< cartodo (cdr (nth Num regions-list))))
             (setq beg-have-find-p t)
             (setq beg-node-to-insert Num)
             (setq beg-node-included-p nil))
            (t nil)))

    (dotimes (Num (length regions-list))
      (cond ((and (not end-have-find-p);;in the inner of Num'th node
                  (>= cdrtodo (car (nth Num regions-list)))
                  (<= cdrtodo (cdr (nth Num regions-list))))
             (setq end-have-find-p t)
             (setq end-node-to-insert Num)
             (setq end-node-included-p t))

            ((and (not end-have-find-p);;before the Num'th node
                  (< cdrtodo (car (nth Num regions-list))))
             (setq end-have-find-p t)
             (setq end-node-to-insert Num)
             (setq end-node-included-p nil))

            (t nil)))

    (cond ((not beg-have-find-p);;start and end both bigger than the last region in list
           nil)

          ((and beg-have-find-p;;end is bigger than the last region, start is in one node
                beg-node-included-p
                (not end-have-find-p))
	   (if (= 0 beg-node-to-insert)
	       (setq regions-list nil)
	     (setq temp-list (nthcdr (1- beg-node-to-insert) regions-list))
	     (setcdr temp-list nil)))

          ((and beg-have-find-p;;end is bigger than the last region, start is between nodes
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                (not end-have-find-p))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setcdr temp-pre nil))

          ((and beg-have-find-p;;end is bigger than the last region, start is before all node
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                (not end-have-find-p))
           (setq regions-list nil))

          ((and beg-have-find-p ;;start and end both in nodes, even end is in the first node  (4 . 6) - (4 . 6)
                beg-node-included-p
                end-have-find-p
                end-node-included-p)
	   (if (= 0 beg-node-to-insert)
	       (setq regions-list (nthcdr (1+ end-node-to-insert) regions-list))
	     (setq temp-list (nthcdr (1- beg-node-to-insert) regions-list))
	     (setcdr temp-list (nthcdr (1+ end-node-to-insert) regions-list))))

          ((and beg-have-find-p;;start is between nodes, end is in node, even end is in the last node
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                end-have-find-p
                end-node-included-p)
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setcdr temp-pre (nthcdr (1+ end-node-to-insert) regions-list)))

          ((and beg-have-find-p;;start is before all nodes, end is in node,even end is in the last node
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                end-have-find-p
                end-node-included-p)
           (setq regions-list (nthcdr (1+ end-node-to-insert) regions-list)))

          ((and beg-have-find-p ;;both before the first node
                end-have-find-p
                (not end-node-included-p)
                (= end-node-to-insert 0))
           nil)

          ((and beg-have-find-p;;start and end both are between nodes
                (not beg-node-included-p)
                (> beg-node-to-insert 0)
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
           (setcdr temp-pre (nthcdr end-node-to-insert regions-list)))

          ((and beg-have-find-p;;start is before all nodes, end is between nodes
                (not beg-node-included-p)
                (= beg-node-to-insert 0)
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
           (setq regions-list (nthcdr end-node-to-insert regions-list)))

          ((and beg-have-find-p;;start is in node,end is between nodes
                beg-node-included-p
                end-have-find-p
                (not end-node-included-p)
                (> end-node-to-insert 0))
	   (if (= 0 beg-node-to-insert)
	       (setq regions-list (nthcdr end-node-to-insert regions-list))
	     (setq temp-pre (nthcdr (1- beg-node-to-insert) regions-list))
	     (setcdr temp-pre (nthcdr end-node-to-insert regions-list))))
          (t nil))
    regions-list))

;;(setq region-list-edit-example '((4 . 7) (11 . 15) (17 . 17) (20 . 25)))
;;start and end both bigger than the last region in list
;;(region-list-edit-delete-greed region-list-edit-example '(27 . 28))
;;end is bigger than the last region, start is in one node
;;(region-list-edit-delete-greed region-list-edit-example '(25 . 27))
;;(region-list-edit-delete-greed region-list-edit-example '(18 . 27))
;;(region-list-edit-delete-greed region-list-edit-example '(4 . 27))
;;(region-list-edit-delete-greed region-list-edit-example '(5 . 25))
;;end is bigger than the last region, start is between nodes
;;(region-list-edit-delete-greed region-list-edit-example '(16 . 27))
;;end is bigger than the last region, start is before all node
;;(region-list-edit-delete-greed region-list-edit-example '(3 . 27))

;;start and end both in nodes, even end is in the first node
;;(region-list-edit-delete-greed region-list-edit-example '(6 . 17))
;;(region-list-edit-delete-greed region-list-edit-example '(4 . 4))
;;(region-list-edit-delete-greed region-list-edit-example '(4 . 25))

;;start is between nodes, end is in node, even end is in the last node
;;(region-list-edit-delete-greed region-list-edit-example '(9 . 22))
;;start is before all nodes, end is in node,even end is in the last node
;;(region-list-edit-delete-greed region-list-edit-example '(1 . 22))
;;both before the first node
;;(region-list-edit-delete-greed region-list-edit-example '(1 . 4))
;;(region-list-edit-delete-greed region-list-edit-example '(2 . 3))

;;start and end both are between nodes----end is between nodes
;;(region-list-edit-delete-greed region-list-edit-example '(10 . 18))
;;(region-list-edit-delete-greed region-list-edit-example '(10 . 11))
;;start is before all nodes, end is between nodes
;;(region-list-edit-delete-greed region-list-edit-example '(1 . 14))
;;start is in node,end is between nodes
;;(region-list-edit-delete-greed region-list-edit-example '(5 . 19))


(provide 'region-list-edit)
