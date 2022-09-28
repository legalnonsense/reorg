;;; -*- lexical-binding: t; -*-

;;; TODO
;;;; deal with disappearing headings 
(defun reorg-outline-level ()
  (get-text-property (point) 'reorg-level))

(defun reorg--goto-next-relative-level (&optional relative-level backward start-level no-error)
  "Goto the next branch that is at RELATIVE-LEVEL up to any branch that is a
lower level than the current branch."
  ;; Outline levels start at 1, so make sure the destination is not out of bounds.  
  (when-let* ((start-level (or start-level (reorg-outline-level)))
	      (point (point)))
    (cond  ((if backward (bobp) (eobp)) nil)
	   ((>= 0 (abs (+ (reorg-outline-level) (or relative-level 0))))
	    (if no-error nil
	      (error "Cannot move to relative-level %d from current level %d"
		     relative-level
		     start-level)))
	   (t
	    (cl-flet ((looking-for (thing)
				   (eq thing 
				       (pcase (or relative-level 0)
					 ((pred (< 0)) 'descendant)
					 ((pred (> 0)) 'ancestor)
					 ((pred (= 0)) 'sibling))))
		      (current-level () (reorg-outline-level))
		      (exit () (progn (if (if backward (bobp) (eobp))
					  (if backward (point-min) (point-max))
					(setf (point) point) nil)))
		      (goto-next () (reorg-tree--goto-next-property-field
				     'reorg-node-type
				     'branch
				     backward)))
	      
	      (cl-loop while (and (goto-next)
				  (if backward (not (bobp)) (not (eobp))))
		       if (if backward
			      (or (and (looking-for 'descendant)
				       (<= (current-level) start-level))
				  (and (looking-for 'sibling)
				       (< (current-level) start-level)))			    
			    (or (and (looking-for 'descendant)
				     (= (current-level) start-level))
				(and (looking-for 'sibling)
				     (< (current-level) start-level))))
		       return (exit)
		       else if (= (current-level)
				  (abs (+ start-level (or relative-level 0))))
		       return point
		       finally return (exit)))))))

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

(defun reorg-tree--goto-first-sibling-in-current-group ()
  (cl-loop with current = (reorg--get-view-props nil 'reorg-branch-form)
	   with point = (point)
	   while (and (reorg--goto-next-relative-level 0 'previous)
		      (equal (reorg--get-view-props nil 'reorg-branch-form)
			     current))
	   do (setq point (point))
	   finally (goto-char point)))

(defun reorg-tree--goto-next-sibling-group (&optional previous)
  "adsf"
  (cl-loop with point = (point)
	   with current = (reorg--get-view-props nil 'reorg-branch-form)
	   while (reorg--goto-next-relative-level 0 previous)
	   when (not (equal (reorg--get-view-props nil 'reorg-branch-form)
			    current))
	   return (point)
	   finally return (progn (goto-char point) nil)))

(defun reorg-tree--map-siblings (func &optional pred pred-val test-fn)
  "Map all siblings at point, but restrict it using PRED, PRED-VAL,
and TEST-FN."
  (when (eq 'branch (reorg--get-view-props nil 'reorg-node-type))
    (cl-loop initially (push (funcall func) results)
	     initially (when pred
			 (unless pred-val 
			   (setq pred-val
				 (funcall pred))))
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
	     finally return results)))

(defun reorg-tree--map-current-sibling-group (func)
  (reorg-tree--map-siblings func))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reorg-new--get-view-props (&optional point &rest props)
  "Get text property PROPS at point. If there are multiple PROPS,
get nested properties."
  (cl-labels ((get-props (props &optional payload)
			 (if props 
			     (let ((props (if (listp props) props (list props))))
			       (if (not payload)
				   (->> (get-text-property (or point (point)) (car props))
					(get-props (cdr props)))
				 (->> (plist-get payload (car props))
				      (get-props (cdr props)))))
			   payload)))
    (if props 
	(get-props props)
      (let ((inhibit-field-text-motion t))
	(get-text-property (or point (point)) reorg--data-property-name)))))

(defun reorg-new--get-current-group-members (&optional children)
  (save-excursion
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (when children (reorg--goto-next-relative-level 1))
    (reorg-tree--map-current-sibling-group
     (lambda ()
       (reorg--get-view-props nil 'reorg-branch-name)))))

(defun reorg-tree--map-siblings-by-group (func)
  (reorg-tree--with-wide-buffer
   (cl-loop with results = nil
	    do (push (reorg-tree--map-siblings
		      func
		      (lambda ()
			(reorg--get-view-props nil
					       'reorg-data
					       :branch-predicate)))
		     results)
	    while (reorg-tree--goto-next-sibling-group)
	    finally return results)))

(defun reorg-tree--get-sibling-group-markers ()
  "Get the markers for the starting point of each
sibling group."
  (reorg-tree--with-wide-buffer
   (cl-loop with results = nil
	    do (push (point-marker)
		     results)
	    while (reorg-tree--goto-next-sibling-group)
	    finally return (reverse results))))

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
	(pred (or pred #'equal))
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


(defun reorg-tree--branch-insert--find-location (data)
  "insert into the branch at point."
  (save-excursion
    (reorg-tree--goto-first-sibling-in-current-group)
    (let* ((branch-predicate (reorg--get-view-props nil 'reorg-branch-form))
	   (name (funcall branch-predicate data))
	   (level (reorg-outline-level))
	   (format-string (reorg--get-view-props nil 'reorg-data :format-string))
	   (branch-sorter (reorg--get-view-props nil 'reorg-data :branch-sorter))
	   (branch-sort-getter (reorg--get-view-props nil 'reorg-data :branch-sort-getter))
	   (existing-data (copy-tree (reorg--get-view-props)))
	   (new-data (plist-put existing-data :branch-name name)))

      (if (and branch-sort-getter branch-sorter)
	  (cl-loop when (funcall branch-sorter
				 (funcall branch-sort-getter name)
				 (funcall branch-sort-getter (reorg--get-view-props nil 'reorg-data :branch-name)))
		   return (reorg-tree--branch-insert--insert-heading new-data)
		   while (reorg--goto-next-relative-level 0)
		   finally return (reorg-tree--branch-insert--insert-heading new-data 'after))
	(reorg-tree--branch-insert--insert-heading new-data)))))

;; ;;
(defun reorg-tree--branch-insert--insert-heading (data &optional after)
  "Insert a new branch using DATA at POINT or (point)."
  (let ((disable-point-adjustment t))
    (if after
	(progn (end-of-line) (insert "\n"))
      (beginning-of-line))
    (save-excursion 
      (insert (reorg--create-headline-string data
					     (plist-get data :format-string)
					     (plist-get data :level))
	      (if after "" "\n")))
    (reorg-dynamic-bullets--fontify-heading)
    (beginning-of-line)
    (point)))


(defun reorg-new--get-current-group-members ()
  (if (org-before-first-heading-p)
      (reorg-tree--goto-next-property-field 'reorg-node-type 'branch nil #'equal)
    (reorg-tree--map-current-sibling-group
     (lambda ()
       (reorg--get-view-props nil 'reorg-branch-name)))))

(defun reorg-new--group-exists-p (group-form)
  "If GROUP-FUNC is in the buffer, go to it and
return the point.  If it is not, return nil."
  (let ((point (point)))
    (setf (point) (point-min))
    (if (reorg-tree--goto-next-property-field 'reorg-branch-form
					      group-form nil #'equal)
	(point)
      (progn (goto-char point) nil))))

(defun reorg-new--find-heading-location (new-header)
  "Find the location of NEW-HEADER, assuming it does not exist, and
go to the next line."
  (let ((name (plist-get new-header 'reorg-branch-name)))
    (cl-flet ((exit () (reorg-new--goto-end-of-subtree)))
      (let* ((branch-sort (plist-get new-header 'reorg-branch-sort))
	     (branch-sort-pred (car branch-sort))
	     (branch-level (plist-get new-header 'reorg-level))
	     (branch-sort-getter (cdr branch-sort))
	     (branch-history (plist-get new-header 'reorg-branch-history)))
	(if branch-sort-pred 
	    (cl-loop initially do (goto-char (point-min))
		     and initially do (reorg-tree--goto-next-property-field 'reorg-branch-history branch-history)
		     when (funcall branch-sort-pred
				   (funcall branch-sort-getter name)
				   (funcall branch-sort-getter (reorg--get-view-props nil 'reorg-branch-name)))
		     return (exit)
		     while (reorg--goto-next-relative-level 0)
		     finally return (exit))
	  (exit))))))


(defun reorg--insert-new-line ()
  "Insert a new line at point."
  (insert "\n")
  (forward-line -1))

(defun reorg-new--insert-heading (data &optional after)
  "Insert a new branch using DATA at POINT or (point)."
  (save-excursion 
    (let ((disable-point-adjustment t))
      (beginning-of-line)
      (insert (reorg--create-headline-string data
					     (plist-get data 'reorg-format)
					     (plist-get data 'reorg-level)))

      (reorg-dynamic-bullets--fontify-heading)
      (point))))

(cl-defun reorg--drop-into-outline (data template)
  "Drop DATA into outline based on TEMPLATE."
  (cl-labels
      ((doloop
	(data
	 template
	 &optional (n 0 np)
	 result-sort
	 branch-sort
	 branch-sort-getter
	 (format-string reorg-headline-format)
	 (level 1)
	 branch-history)

	(when-let* ((branch-func `(lambda (x)
				    (reorg--let-plist x
						      ,(plist-get template :group))))
		    (name (funcall branch-func data)))
	  (let* ((branch-sort (cons (plist-get template :branch-sort)
				    (or (plist-get template :branch-sort-getter)
					#'identity)))
		 (result-sort (append result-sort
				      (cl-loop for (form . pred) in (plist-get template :result-sort)
					       collect (cons `(lambda (x)
								(reorg--let-plist x
										  ,form))
							     pred))))
		 (children (plist-get template :children))
		 (members (or (and (= level 1)
				   (progn (while (reorg--goto-next-relative-level -1 t nil t))
					  (reorg-new--get-current-group-members)))
			      (and (reorg--goto-next-relative-level 1)
				   (reorg-new--get-current-group-members))))
		 (branch-hash (sxhash-equal (cons (plist-get template :group) name)))
		 (branch-history (push branch-hash branch-history))
		 (format-string (plist-get template :format-string))
		 (disable-point-adjustment t)
		 (inhibit-field-text-motion t))

	    (unless (let ((point (point)))
		      (goto-char (point-min))
		      (if (reorg-tree--goto-next-property-field 'reorg-branch-history branch-history nil #'equal)
			  t
			(progn (goto-char point) nil)))
	      (let ((new-header `( reorg-branch-name ,name			       
				   reorg-branch-sort ,branch-sort
				   reorg-branch-form ,(plist-get template :group)
				   reorg-format ,format-string
				   reorg-branch-history ,branch-history
				   reorg-branch-hash ,branch-hash
				   reorg-result-sort ,result-sort
				   reorg-level ,level
				   reorg-node-type branch)))
		(reorg-new--find-heading-location new-header)		
		(end-of-line)
		(unless (org--line-empty-p 1)
		  (insert "\n"))
		(reorg-new--insert-heading new-header)))
	    (if children
		(cl-loop for x below (length children)
			 do (doloop
			     data
			     (nth x children)
			     x
			     result-sort
			     nil
			     nil
			     format-string
			     (1+ level)
			     branch-history))
	      (save-excursion 
		(reorg-new--insert-into-leaves data result-sort (1+ level) format-string)))))))
    (save-excursion 
      (goto-char (point-min))
      (insert "\n")
      (doloop data template)
      (goto-char (point-min))
      (delete-char 1))))

(defun reorg-new--insert-into-leaves (data result-sort level format-string)
  "asdf"
  (let ((disable-point-adjustment t))
    (cl-loop while (and (reorg--get-view-props nil 'reorg-node-type)
			(not (eq (reorg--get-view-props nil 'reorg-node-type) 'branch)))
	     if (cl-loop for (func . pred) in result-sort
			 if (funcall pred
				     (funcall func data)
				     (funcall func (reorg--get-view-props)))
			 return t
			 finally return nil)     	     
	     return (progn (end-of-line) (insert "\n" (reorg--create-headline-string data format-string level)))
	     finally (progn (end-of-line) (insert "\n" (reorg--create-headline-string data format-string level))))
    (reorg-dynamic-bullets--fontify-heading)))

(defun reorg-new--does-the-group-exist? (branch-func)
  (reorg-tree--goto-next-property-field 'reorg-branch-form branch-func nil #'equal))


(defun reorg-new--goto-end-of-leaves ()
  (if (reorg-tree--goto-next-property-field 'reorg-node-type )
      (reorg-tree--goto-next-property-field 'reorg-node-type 'data t)
    (setf (point) (point-max))))




(defun reorg-new--goto-end-of-subtree ()
  (while (reorg--goto-next-relative-level 1))
  (reorg-new--goto-end-of-leaves))

