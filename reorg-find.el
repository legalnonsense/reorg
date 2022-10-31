;; -*- lexical-binding: t; -*-

;; Functions to find by text properties in the buffer 

;;; Text property finding functions

(defun reorg--get-view-prop (&optional property point)
  "Get PROPERTY from the current heading.  If PROPERTY
is omitted or nil, get the 'reorg-data' prop.  If it is
supplied, get that property from 'reorg-data'."
  (let ((props (get-text-property (or point (point)) 'reorg-data)))
    (if property
	(alist-get property props)
      props)))

(defun reorg--get-view-props (&optional point &rest props)
  "Get text property PROPS at point. If there are multiple PROPS,
get nested properties."
  (cl-labels ((get-props (props &optional payload)
			 (if props 
			     (let ((props (if (listp props) props (list props))))
			       (if (not payload)
				   (->> (get-text-property (or point (point)) (car props))
					(get-props (cdr props)))
				 (->> (alist-get (car props) payload)
				      (get-props (cdr props)))))
			   payload)))
    (if props 
	(get-props props)
      (let ((inhibit-field-text-motion t))
	(reorg--get-view-prop nil (or point (point)))))))

(defun reorg--goto-next-prop (property &optional value limit
				       predicate)
  "Initially supposed to act like 'text-property-search-forward'
but inclde a LIMIT parameter. Now, assume we are getting 'reorg-data
and PROPERTY refers to the key of that alist.  The code is sloppy.
DOES NOT RUN 'reorg--navigation-hooks'." 
  (cond
   ((eobp)
    nil)
   ((if limit
	(> (point) limit)
      (= (point) (point-max)))
    nil)
   (t    
    (let ((origin (point))
          (ended nil)
	  (limit (or limit (point-max)))
          pos)
      ;; skip the current property if NOT-CURRENT is non-nil 
      ;; (when (text-property--match-p value (get-text-property (point) property)
      ;; 				    predicate)
      ;; 	(text-property--find-end-forward (point) property value predicate))
      ;; Find the next candidate.
      (cl-loop with found = nil
	       while (not ended)
	       do (setq pos (next-single-property-change (point) 'reorg-data nil limit))
	       if (or (not pos)		
		      (> pos limit))
	       return
	       (progn (reorg--goto-char origin)
		      (run-hooks 'reorg--navigation-hook)
		      (setq ended t)
		      nil)
	       else do
	       (progn (goto-char pos)
		      (if (and (< (point) limit)
			       (funcall (or predicate #'equal)
					value
					(if property 
					    (alist-get property 
						       (get-text-property (point) 'reorg-data))
					  (get-text-property (point) 'reorg-data))))
			  (progn 
			    (setq ended t)
			    (setq found t))
			;; (setq pos (next-single-property-change (point) 'reorg-data nil limit))
			(when (or (not pos)
				  (>= pos limit))
			  (goto-char origin)
			  (setq ended t))))
	       finally return (if (not found)
				  nil
				(point)))))))

(defun reorg--goto-previous-prop (property &optional value limit
					   predicate)
  "See 'reorg--get-next-prop'"
  (cond
   ((bobp)
    nil)
   ((< (point) (or limit (point-min)))
    nil)
   (t    
    (let ((origin (point))
          (ended nil)
	  (limit (or limit (point-min)))
          pos)
      (cl-loop with found = nil
	       with pos = nil 
	       while (not ended)
	       do (setq pos (previous-single-property-change (point) 'reorg-data nil limit))
	       if (or (not pos)		
		      (< pos limit))
	       return
	       (progn (reorg--goto-char origin)
		      (setq ended t)
		      nil)
	       else do
	       (progn (goto-char pos)
		      (if (and (>= (point) limit)
			       (funcall (or predicate #'equal)
					value
					(if property 
					    (alist-get property
						       (get-text-property (point) 'reorg-data))
					  (get-text-property (point) 'reorg-data))))

			  (progn 
			    (setq ended t)
			    (setq found t))
			(when (or (not pos)
				  (bobp)
				  (<= pos limit))
			  (goto-char origin)
			  (setq ended t))))
	       finally return (if (not found)
				  nil
				(point)))))))


;; (defun reorg--find-prop (prop &optional val from to test transform first-only)
;;   "TEST is a function that accepts two arguments: VAL and
;; the text property at the beginning of the region.  If TEST is nil, use 'equal'.
;; PROP is a property that is contained within the reorg-data
;; text property data.  VAL is a target value.  TRANSFORM is a function
;; that accepts to arguments (PROP and the current BEG) and returns
;; a value to be compared against VAL used TEST.  FIRST-ONLY means to return
;; the first match location; otherwise return all matching locations."
;;   (save-excursion 
;;     (cl-loop
;;      with test = (if val
;; 		     (or test #'equal)
;; 		   (lambda (&rest args) t))
;;      with from = (or from (point-min))
;;      with to = (or to (point-max))
;;      for (beg . end) being the intervals
;;      property 'reorg-data
;;      from from
;;      to to
;;      when (funcall
;; 	   test
;; 	   (cond (transform
;; 		  (funcall transform (reorg--get-view-prop)))
;; 		 (t
;; 		  (alist-get prop
;; 			     (get-text-property beg 'reorg-data))))
;; 	   ;;		  (reorg--get-view-prop prop beg)))
;; 	   val)
;;      if (and first-only
;; 	     (or
;; 	      (> beg (point))
;; 	      (< end (point))))
;;      return (cons beg end)
;;      else if first-only
;;      do (+ 1 1) ;; isn't there a PASS command? 
;;      else
;;      collect (cons beg end))))

;; (defun reorg--get-next-prop (prop &optional val test transform limit)
;;   "Find the next text prop PROP that matches VAL.
;; Returns (beg . end) points of the matching property."
;;   (reorg--find-prop prop val (point) limit test transform t))

;; (defun reorg--get-previous-prop (prop &optional val test transform limit)
;;   "Find the previous text prop PROP that matches VAL.
;; Returns (beg . end) points of the matching property."
;;   (when-let ((previous (reorg--find-prop prop val limit (point) test transform)))
;;     (cl-loop for (beg . end) in (reverse previous)
;; 	     unless (and (>= (point) beg)
;; 			 (<= (point) end))
;; 	     return (cons beg end))))

(defun reorg--goto-char (point)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (run-hooks 'reorg--navigation-hook))

;;; Navigation commands 
;;;; headings

(defun reorg--goto-next-heading ()
  "goto next heading"
  (interactive)
  (reorg--goto-next-prop nil nil nil (lambda (a b) t))
  (run-hooks 'reorg--navigation-hook))

(defun reorg--goto-previous-heading ()
  "goto next heading"
  (interactive)
  (reorg--goto-previous-prop nil nil nil (lambda (a b) t))
  (run-hooks 'reorg--navigation-hook))

;;;; siblings

(defun reorg--goto-next-sibling ()
  "goto next sibling in same subtree"
  (interactive)
  (reorg--goto-next-prop
   'reorg-level
   (reorg--get-view-prop 'reorg-level)
   (save-excursion
     (reorg--goto-next-prop 'reorg-level
			    (reorg--get-view-prop 'reorg-level)
			    nil
			    (lambda (a b)
			      (< b a)))))
  (run-hooks 'reorg--navigation-hook))

(defun reorg--goto-previous-sibling ()
  "goto previous sibling"
  (interactive)
  (reorg--goto-previous-prop
   'reorg-level
   (reorg--get-view-prop 'reorg-level)
   (save-excursion
     (reorg--goto-previous-prop
      'reorg-level
      (reorg--get-view-prop 'reorg-level)
      nil
      (lambda (a b) (< b a)))))
  (run-hooks 'reorg--navigation-hook))

;;;; clones

(defun reorg--goto-next-clone ()
  "goto next clone"
  (interactive)
  (when 
      (reorg--goto-next-prop 'id
			     (reorg--get-view-prop 'id))
    (run-hooks 'reorg--navigation-hook)))

(defun reorg--goto-previous-clone ()
  "goto next clone"
  (interactive)
  (when 
      (reorg--goto-previous-prop 'id
				 (reorg--get-view-prop 'id))
    (run-hooks 'reorg--navigation-hook)))


;;;; parents

(defun reorg--goto-next-parent ()
  "Goto the next parent."
  (interactive)
  (when 
      (reorg--goto-next-prop 'reorg-level
			     (reorg--get-view-prop 'reorg-level)
			     nil
			     (lambda (a b) (> a b)))
    (run-hooks 'reorg--navigation-hook)))

(defun reorg--goto-parent ()
  "Goto the next parent."
  (interactive)
  (when 
      (reorg--goto-previous-prop 'reorg-level
				 (1- (reorg--get-view-prop 'reorg-level)))
    (run-hooks 'reorg--navigation-hook)))

(defun reorg--goto-root ()
  "goto root"
  (when (not (= 1 (reorg--get-view-prop 'reorg-level)))
    (reorg--goto-previous-prop 'reorg-level
			       1)
    (run-hooks 'reorg--navigation-hook)))

;;;; getting data

(defun reorg--get-outline-level ()
  "Get the outline level of the heading at point."
  (reorg--get-view-prop 'reorg-level))

;;;; predicates

(defun reorg--last-branch-p ()
  "Does the current branch have any present children?"
  (reorg--goto-next-prop 'reorg-branch
			 t
			 (save-excursion
			   (prog1 
			       (reorg--goto-next-sibling)
			     (run-hooks 'reorg--navigation-hook)))
			 (lambda (a b) t)))

(provide 'reorg-find)
