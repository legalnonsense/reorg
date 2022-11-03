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
				       predicate visible-only)
  "Initially based on 'text-property-search-forward'
but inclde a LIMIT parameter.  Now, assume we are getting 'reorg-data
and PROPERTY is the key of that alist.
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
			       (if visible-only
				   (not (org-invisible-p (point) t))
				 t)
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
					   predicate visible-only)
  "See 'reorg--goto-next-prop'"
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
	       do (setq pos
			(previous-single-property-change
			 (point)
			 'reorg-data
			 nil
			 limit))
	       if (or (not pos)		
		      (< pos limit))
	       return
	       (progn (reorg--goto-char origin)
		      (setq ended t)
		      nil)
	       else do
	       (progn (goto-char pos)
		      (if (and (>= (point) limit)
			       (funcall
				(or predicate #'equal)
				value
				(if property 
				    (alist-get
				     property
				     (get-text-property
				      (point)
				      'reorg-data))
				  (get-text-property
				   (point)
				   'reorg-data)))
			       (if visible-only
				   (not (org-invisible-p (point) t))
				 t))
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

(defun reorg--get-previous-prop (property &optional
					  value
					  limit
					  predicate
					  visible-only)
  "Return the point instead of moving it."
  (save-excursion (funcall #'reorg--goto-previous-prop
			   property
			   value
			   limit
			   predicate
			   visible-only)))

(defun reorg--get-next-prop (property &optional
				      value
				      limit
				      predicate
				      visible-only)
  "get next instead of moving it."
  (save-excursion (funcall #'reorg--goto-next-prop
			   property
			   value
			   limit
			   predicate
			   visible-only)))

(defun reorg--goto-char (point)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (run-hooks 'reorg--navigation-hook)
  (point))

;;; Navigation commands 

(defmacro reorg--create-navigation-commands (alist)
  "Create navigation commands. ALIST is a list in the form of (NAME . FORM)
where NAME is the name of what you are moving to, e.g., \"next-heading\"
and FORM is evaluated to see if that target exists.  NAME should be hyphenated.

FORM must return the point of the target if it exists, or nil if it doesn't.

If the target exists, the function will move to that point and run
`reorg--navigation-hook'.

If the target does not exist, the function will return nil.

Also create functions to get the point of the target, but not move to it."
  `(progn 
     ,@(cl-loop for (name . form) in alist
		append (list `(defun ,(reorg--create-symbol 'reorg--goto- name) nil
				,(concat "Move point to "
					 (s-replace "-" " " (symbol-name name))
					 " and run navigation hook.")
				(interactive)
				(when-let ((point ,form))
				  (reorg--goto-char point)))
			     `(defun ,(reorg--create-symbol 'reorg--get- name) nil
				,(concat "Get the point of "
					 (s-replace "-" " " (symbol-name name))
					 ".")
				(prog1 
				    (save-excursion
				      (when (funcall
					     ',(reorg--create-symbol 'reorg--goto- name))
					(point)))
				  (run-hooks 'reorg--navigation-hook)))))))

(reorg--create-navigation-commands
 ((next-heading . (reorg--get-next-prop nil nil nil (lambda (a b) t)))
  (next-visible-heading . (reorg--get-next-prop nil nil nil (lambda (a b) t) t))
  (previous-heading . (reorg--get-previous-prop nil nil nil (lambda (a b) t)))
  (next-branch . (reorg--get-next-prop 'reorg-branch t nil nil nil))
  (next-visible-branch . (reorg--get-next-prop 'reorg-branch t nil nil t))
  (previous-visible-heading . (reorg--get-previous-prop nil nil nil (lambda (a b) t) t))
  (next-sibling . (reorg--get-next-prop
		   'reorg-level
		   (reorg--get-view-prop 'reorg-level)
		   (reorg--get-next-prop 'reorg-level
					 (reorg--get-view-prop 'reorg-level)
					 nil
					 (lambda (a b)
					   (< b a)))))
  (previous-sibling . (reorg--get-previous-prop
		       'reorg-level
		       (reorg--get-view-prop 'reorg-level)
		       (reorg--get-previous-prop
			'reorg-level
			(reorg--get-view-prop 'reorg-level)
			nil
			(lambda (a b) (< b a)))))
  (next-clone . (reorg--get-next-prop 'id
				      (reorg--get-view-prop 'id)))
  (previous-clone . (reorg--get-previous-prop 'id
					      (reorg--get-view-prop 'id)))
  (next-parent . (reorg--get-next-prop 'reorg-level
				       (reorg--get-view-prop 'reorg-level)
				       nil
				       (lambda (a b) (> a b))))
  (parent . (reorg--get-previous-prop 'reorg-level
				      (1- (reorg--get-view-prop 'reorg-level))))
  (root . (and (/= 1 (reorg--get-view-prop 'reorg-level))
	       (reorg--get-previous-prop 'reorg-level 1)))
  (next-child . (and
		 (reorg--get-view-prop 'reorg-branch)
		 (reorg--get-next-prop 'reorg-level
				       (1+ (reorg--get-view-prop 'reorg-level))
				       (reorg--get-next-prop 'reorg-level
							     (reorg--get-view-prop 'reorg-level)
							     nil
							     (lambda (a b)
							       (>= a b))))))
  (next-visible-child
   .
   (and
    (reorg--get-view-prop 'reorg-branch)
    (reorg--get-next-prop 'reorg-level
			  (1+ (reorg--get-view-prop 'reorg-level))
			  (reorg--get-next-prop 'reorg-level
						(reorg--get-view-prop 'reorg-level)
						nil
						(lambda (a b)
						  (>= a b)))
			  nil
			  t)))))




;;;; getting data

(defun reorg--get-outline-level ()
  "Get the outline level of the heading at point."
  (reorg--get-view-prop 'reorg-level))

;;;; predicates
(defun reorg--last-branch-p ()
  "is this the last branch in the subtree?"
  (and (reorg--get-view-prop 'reorg-branch)
       (not (reorg--get-view-prop
	     'reorg-branch
	     (reorg--get-next-child)))))


(defun reorg-edits--get-field-bounds ()
  "Get the bounds of the field at point."
  (let ((match (save-excursion (text-property--find-end-forward
				(point)
				'reorg-data
				(reorg--get-view-prop)
				#'equal))))
    (cons
     (prop-match-beginning match)
     (prop-match-end match))))


(provide 'reorg-find)
