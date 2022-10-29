;; -*- lexical-binding: t; -*-

;; Functions to find by text properties in the buffer 

(defun reorg--get-view-prop (&optional property point)
  "Get PROPERTY from the current heading.  If PROPERTY
is omitted or nil, get the 'reorg-data' prop.  If it is
supplied, get that property from 'reorg-data'."
  (let ((props (get-text-property (or point (point)) reorg--data-property-name)))
    (if property
	(alist-get property props)
      ;;(plist-get props property)
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
	(get-text-property (or point (point)) reorg--data-property-name)))))

(defun reorg--find-prop (prop &optional val from to test transform first-only)
  "TEST is a function that accepts two arguments: VAL and
the text property at the beginning of the region.  
PROP is a property that is contained within the reorg-data
text property data.  VAL is a target value."
  (save-excursion 
    (cl-flet ((truth (&rest args) t))
      (cl-loop with test = (if val
			       (or test #'equal )
			     #'truth)
	       with from = (or from (point-min))
	       with to = (or to (point-max))
	       for (beg . end) being the intervals
	       property 'reorg-data
	       from from
	       to to
	       when (funcall
		     test
		     (if transform
			 (funcall transform prop beg)
		       (reorg--get-view-prop prop beg))
		     val)
	       if (and first-only
		       (or
			(> beg (point))
			(< end (point))))
	       return (cons beg end)
	       else if first-only
	       do (+ 1 1)
	       else
	       collect (cons beg end)))))

(defun reorg--get-next-prop (prop &optional val test transform)
  "Find the next text prop PROP that matches VAL.
Returns (beg . end) points of the matching property."
  (reorg--find-prop prop val (point) nil test transform t))

(defun reorg--get-previous-prop (prop &optional val test transform)
  "Find the previous text prop PROP that matches VAL.
Returns (beg . end) points of the matching property."
  (when-let ((previous (reorg--find-prop prop val nil (point) test transform)))
    (cl-loop for (beg . end) in (reverse previous)
	     unless (and (>= (point) beg)
			 (<= (point) end))
	     return (cons beg end))))

(defun reorg--goto-char (point)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--goto-next-prop (prop &optional val test end transform)
  "Go to next PROP that matches VAL."
  (when-let ((target (reorg--get-next-prop prop val test transform)))
    (reorg--goto-char (if end
			  (cdr target)
			(car target)))))

(defun reorg--goto-previous-prop (prop &optional val test end transform)
  "Go to next PROP that matches VAL."
  (when-let ((target (reorg--get-previous-prop prop val test transform)))
    (reorg--goto-char (if end (cdr target)
			(car target)))))


(defun reorg--get-all-clone-start-points ()
  "get all clone start points for the clone at point"
  (cl-loop for (b . e) in (reorg--find-prop
			   'id
			   (reorg--get-view-prop 'id))
	   collect b))

(defun reorg--goto-next-sibling ()
  "goto next sibling in same subtree"
  (reorg--goto-next-prop nil
			 (list (reorg--get-view-prop 'reorg-level)
			       (reorg--get-view-prop 'group-id))
			 #'equal
			 nil
			 (lambda (_prop point)
			   (list 
			    (reorg--get-view-prop 'reorg-level point)
			    (reorg--get-view-prop 'group-id point)))))

(defun reorg--goto-next-clone ()
  "goto next clone"
  (interactive)
  (reorg--goto-next-prop 'id
			 (reorg--get-view-prop 'id)))

(defun reorg--goto-previous-clone ()
  "goto next clone"
  (interactive)
  (reorg--goto-previous-prop 'id
			     (reorg--get-view-prop 'id)))


