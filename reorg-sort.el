;; -*- lexical-binding: t; -*-

(defun reorg--multi-sort (functions-and-predicates sequence)
  "FUNCTIONS-AND-PREDICATES is an alist of functions and predicates.
It uses the FUNCTION and PREDICATE arguments useable by `seq-sort-by'.
SEQUENCE is a sequence to sort."
  (seq-sort 
   (lambda (a b)
     (cl-loop for (func . pred) in functions-and-predicates	      
	      unless (equal (funcall func a)
			    (funcall func b))
	      return (funcall pred
			      (funcall func a)
			      (funcall func b))))
   sequence))

(cl-defgeneric reorg--seq-group-by (form sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall `(lambda (e)
			     (reorg--let-plist e ,form))
			  elt))
	    (cell (assoc key acc)))
       (if cell
	   (setcdr cell (push elt (cdr cell)))
	 (push (list key elt) acc))
       acc))
   (seq-reverse sequence)
   nil))

(cl-defun reorg--group-and-sort (list template &optional (n 0 np))
  "Group RESULTS according to TEMPLATE."
  (let ((copy (copy-tree list)))
    (cl-labels ((doloop (data
			 template
			 &optional (n 0 np)
			 result-sorters
			 grouper-list
			 grouper-list-results
			 format-string
			 (level 1))
			(let ((grouper (plist-get template :group))
			      (children (plist-get template :children))
			      (class (plist-get template :class))
			      (heading-sorter (plist-get template :sort))
			      (heading-sort-getter (or (plist-get template :sort-getter)
						       #'car))
			      (format-string (or (plist-get template :format-string)
						 format-string
						 reorg-headline-format))
			      (result-sort (plist-get template :sort-results)))			  
			  (when result-sort
			    (setq result-sorters
				  (append result-sorters					  
					  (cl-loop for (form . pred) in result-sort
						   collect (cons `(lambda (x)
								    (reorg--let-plist x
								      ,form))
								 pred)))))
			  (unless np
			    (let ((old (cl-copy-list data)))
			      (setcar data '_)
			      (setcdr data (list old))))
			  (setf
			   (nth n (cdr data))
			   (--> (nth n (cdr data))
				(cond ((functionp grouper)
				       (->> it
					    (seq-group-by grouper)
					    (seq-map (lambda (x) (list (car x) (cdr x))))))
				      ((stringp grouper)
				       (list (list grouper it)))
				      (t (->> it
					      (reorg--seq-group-by grouper)
					      (seq-map (lambda (x) (list (car x) (cdr x)))))))
				(seq-filter (lambda (x) (and (not (null (car x)))
							     (not (null (cdr x)))
							     (not (null x))))
					    it)
				(cl-loop
				 for x in it
				 do (setf
				     (car x)
				     (list
				      :branch-name (car x) 
				      :headline (car x)
				      :reorg-branch t
				      :result-sorters result-sorters 
				      :grouper-list `(lambda (x)
						       (reorg--let-plist x
							 ,grouper))
				      :branch-predicate `(lambda (x)
							   (reorg--let-plist x
							     ,grouper))
				      :branch-result (car x)
				      :grouper-list-results (car x)
				      :format-string format-string
				      :result-sorters result-sorters
				      :template template 
				      :children children
				      :branch-sorter heading-sorter
				      :branch-sort-getter heading-sort-getter
				      :reorg-level level))
				 finally return it)
				(seq-filter (lambda (x) (and (not (null (car x)))
							     (not (null (cdr x)))
							     (not (null x))))
					    it)
				(cl-loop for x in it
					 do (setf (car x)
						  (reorg--create-headline-string (car x)
										 (copy-tree format-string)
										 level))
					 finally return it)
				(if heading-sorter
				    (seq-sort-by (lambda (y)
						   (funcall heading-sort-getter (car y)))
						 heading-sorter
						 it)
				  it)))
			  (if children
			      (progn ;; if 
				(cl-loop for x below (length (nth n (cdr data)))
					 do (setcdr
					     (nth x (nth n (cdr data)))
					     (cl-loop
					      for z below (length children)
					      collect (seq-copy
						       (cadr (nth x (nth n (cdr data))))))))
				(cl-loop for x below (length children)
					 do (cl-loop
					     for y below (length (nth n (cdr data)))
					     do (doloop
						 (nth y (nth n (cdr data)))
						 (nth x children)
						 x
						 result-sorters
						 grouper-list
						 (append
						  grouper-list-results
						  (list (plist-get (car (nth y (nth n (cdr data))))
								   :branch-value)))
						 format-string
						 (1+ level)))))
			    (progn ;; else 
			      (when result-sorters
				(cl-loop for x below (length (nth n (cdr data)))
					 do (setf
					     (cadr (nth x (nth n (cdr data))))
					     (reorg--multi-sort result-sorters
								(cadr (nth x (nth n (cdr data))))))))
			      (cl-loop for x below (length (nth n (cdr data)))
				       do (setf
					   (cadr (nth x (nth n (cdr data))))
					   (cl-loop for each in (cadr (nth x (nth n (cdr data))))
						    collect
						    (progn (plist-put each :reorg-stars (1+ level))
							   (reorg--create-headline-string each format-string (1+ level)))))))))))
      (doloop copy template)
      (cadr copy))))

(provide 'reorg-sort)
