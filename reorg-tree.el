
;;; -*- lexical-binding: t; -*-

(defun reorg--goto-next-relative-level (&optional relative-level previous start-level)
  "Goto the next branch that is at RELATIVE-LEVEL up to any branch that is a
lower level than the current branch."
  (when (> (abs (+ (reorg-outline-level) relative-level)) 0)
    (let ((start-level (or start-level (reorg-outline-level)))
	  (point (point))
	  (relative-level (or relative-level 0)))
      (cl-loop while (and (reorg-tree--goto-next-property-field 'reorg-field-type 'branch previous)
			  (not (bobp)))
	       if (or (< (reorg-outline-level) start-level)
		      (and (> relative-level 0)
			   (= (reorg-outline-level) start-level)))
	       return (progn
			(setf (point) point)
			nil)
	       else if (= (reorg-outline-level)
			  (abs (+ start-level relative-level)))
	       return t
	       finally (progn
			 (setf (point) point)
			 nil)))))

(defun reorg-into--get-list-of-sibling-branches-at-point ()
  "Get a list of cons cells in the form (FUNCTION . RESULTS)."
  (save-excursion
    (let ((level (reorg-outline-level))
	  (disable-point-adjustment t))
      (while (reorg--goto-next-relative-level 0 t))
      (cl-loop with alist = nil
	       with current-func = nil
	       do (setq current-func (reorg--get-view-props nil 'reorg-data :grouper-list))
	       and do (setf (alist-get current-func
				       alist nil nil #'equal)			      
			    (append
			     (alist-get current-func
					alist nil nil #'equal)
			     (-list 
			      (reorg--get-view-props nil 'reorg-data :grouper-list-results))))
	       while (reorg--goto-next-relative-level 0)
	       finally return alist))))



(defun reorg-into--get-list-of-child-branches-at-point ()
  "Get a list of cons cells in the form (FUNCTION . RESULTS)."
  (save-excursion
    (let ((level (reorg-outline-level))
	  (disable-point-adjustment t))
      (when (reorg--goto-next-relative-level 1)
	(cl-loop with alist = nil
		 with current-func = nil
		 do (setq current-func (reorg--get-view-props nil 'reorg-data :grouper-list))
		 and do (setf (alist-get current-func
					 alist nil nil #'equal)			      
			      (append
			       (alist-get current-func
					  alist nil nil #'equal)
			       (-list 
				(reorg--get-view-props nil 'reorg-data :grouper-list-results))))
		 while (reorg--goto-next-relative-level 0)
		 finally return alist)))))

(defun reorg-into--need-to-make-new-branch? (data &optional point)
  "asdf"
  (let* ((children (reorg-into--get-list-of-child-branches-at-point)))
    (cl-loop with new-result = nil
	     with return = nil
	     for (func . results) in children
	     do (setq new-result (funcall func data))
	     if (and new-result
		     (member new-result results))
	     do (push (cons func new-result) return)
	     else do (push (cons func nil) return)
	     finally return (reverse return))))

(defun reorg-tree--map-siblings (func &optional pred pred-val test-fn)
  (cl-loop initially (push (funcall func) results)
	   initially (unless pred-val 
		       (setq pred-val
			     (funcall pred)))
	   with results = nil

	   for backward in '(t nil)
	   do (cl-loop with point = (point)
		       while (and (reorg--goto-next-relative-level 0 backward)
				  (or (not pred)
				      (funcall (or test-fn #'equal )
					       (funcall pred)
					       pred-val)))
		       do (push (funcall func) results)
		       finally (progn (setq results (reverse results))
				      (goto-char point)))
	   finally return results))

(defun reorg-tree--goto-next-property-field (prop val &optional backward pred transformer)
  "Move to the beginning of the next field of text property PROP that
matches VAL.

If PRED is specified, compare values using PRED instead of `eq'.

If BACKWARD is non-nil, move backward instead of forward. 

TRANSFORMER is an optional function that accepts one argument
(the value of the text property at point) and transforms it before
comparing it to VAL.

For example, if (get-text-property (point) PROP) returns a plist, but you
only want to see if one value is present, the TRANSFORMER:

(lambda (plist) (plist-get plist :interesting-property))

will extract the single value prior to comparing to VAL."
  (let ((func (if backward
		  #'previous-single-property-change
		#'next-single-property-change))
	(limit (if backward
		   (point-min)
		 (point-max)))
	(pred (or pred #'eq))
	(search-invisible t))

    (cl-loop with point = (point)
	     with origin = (point)
	     while point

	     do (setq point (funcall func point (car (-list prop))))
	     
	     if (and (null point)
		     (funcall pred
			      (funcall (or transformer #'identity)
				       (apply #'reorg--get-view-props limit (-list prop)))
			      val))
	     return (goto-char limit)

	     else if (null point)
	     return (progn (goto-char origin) nil)
	     
	     else if (funcall pred
			      val
			      (funcall (or transformer #'identity)
				       (apply #'reorg--get-view-props point (-list prop))))
	     return (goto-char point)

	     else do (forward-char (- point (point))))))

(defun reorg-tree--get-by-branch-predicate (prop val func)
  "Execute FUNC at each branch that has PROP equal to VAL and
make a list of the results."
  (let (results)
    (save-excursion
      (cl-loop with level = (reorg-outline-level)
	       while (and (reorg-tree--goto-next-property-field nil
								'reorg-data val nil #'equal
								(lambda (x) (plist-get x prop)))
			  (> (reorg-outline-level) level))
	       do (cl-pushnew (funcall func) results :test #'equal)))
    (reverse results)))

(cl-defun reorg--drop-into-template (data
				     template
				     &optional (n 0 np)
				     result-sorters
				     grouper-list
				     grouper-list-results
				     format-string
				     (level 1))
  (let* ((grouper (plist-get template :group))
	 (children (plist-get template :children))
	 (heading-sorter (plist-get template :sort))
	 (heading-sort-getter (or (plist-get template :sort-getter)
				  #'car))
	 (format-string (or (plist-get template :format-string)
			    format-string
			    reorg-headline-format))
	 (result-sort (plist-get template :sort-results))
	 (result-sorters (append result-sorters					  
				 (cl-loop for (form . pred) in result-sort
					  collect (cons `(lambda (x)
							   (reorg--let-plist x
									     ,form))
							pred)))))
    (reorg-tree--with-wide-buffer
     (goto-char (point-min))
     (if (reorg-into--need-to-make-new-branch? data)
	 (if children
	     (progn 
	       (cl-loop while (reorg--goto-next-relative-level 1)
			do (cl-loop for child in children
				    do (reorg--drop-into-template data
								  (plist-get template :children)
								  result-sorters
								  grouper-list
								  grouper-list-results nil
								  format-string
								  (1+ level))))

	       (plist-put data 
			  (cl-loop for x below (length children)
				   do (cl-loop
				       for y below (length (nth n (cdr data)))
				       do (doloop
					   (nth y (nth n (cdr data)))
					   (nth x children)
					   x
					   result-sorters
					   grouper-list
					   (append grouper-list-results
						   (list (plist-get (car (nth y (nth n (cdr data))))
								    :branch-value)))
					   format-string
					   (1+ level)))))
	       ))))))

(provide 'reorg-tree)
