;; ;; -*- lexical-binding: t; -*-


(defun xxx (seq)
  (cl-loop for each in seq
	   with results = nil
	   with num = (length seq)
	   do (cl-loop with done = 0
		       for x from 0		       
		       if (< x (length (alist-get 'parent-dirs each)))
		       do (push
			   each
			   (alist-get (intern (nth x (alist-get 'parent-dirs each))) results))
		       else do (cl-incf done)
		       when (= done num)
		       return results)
	   finally return results))

(defun xxx (seq n)
  (let ((groups (reorg--seq-group-by (lambda (x) (nth n (alist-get 'parent-dirs x))) seq)))
    (if groups
	(cl-loop for each in groups
		 append (cons (car each) 
			      (xxx (cdr each) (1+ n))))
      seq)))


(xxx xxx-seq 0) ;;;test 
(alist-get 'parent-dirs (car xxx-seq))
(setq xxx-seq `(((parent-dirs . ("home" "jeff" ".emacs.d" "lisp" "reorg" "TEST"))
		 (name . "1"))
		((parent-dirs . ("home" "jeff" ".emacs.d" "lisp" "reorg" "TESTS"))
		 (name . "2"))
		((parent-dirs . ("home" "jeff" "legal" "whatever"))
		 (name . "3"))
		((parent-dirs . ("home" "jeff" "legal" "xxx"))
		 (name . "4"))
		((parent-dirs . ("etc" "something" "legal" "xxx"))
		 (name . "5"))))


(defun reorg--get-group-and-sort* (data
				   template
				   level
				   ignore-sources
				   &optional
				   inherited-props)
  "Apply TEMPLATE to DATA and apply the :action-function 
specified in the template or `reorg--grouper-action-function'
to the results."
  (when-let ((invalid-keys
	      (seq-difference 
	       (cl-loop for key in template by #'cddr
			collect key)
	       reorg--valid-template-keys)))
    (error "Invalid keys in entry in tempate: %s" invalid-keys))
  (cl-flet ((get-header-metadata
	     (header groups sorts bullet)
	     (let ((id (org-id-new)))
	       (list
		(cons 'branch-name header)
		(cons 'reorg-branch t)
		(cons 'branch-type 'branch)
		(cons 'result-sorters sorts)
		(cons 'bullet bullet)
		(cons 'reorg-level level)
		(cons 'parent-id (plist-get inherited-props :parent-id))
		(cons 'group-id
		      (md5
		       (concat 
			(pp-to-string (plist-get
				       inherited-props
				       :parent-template))
			(pp-to-string (plist-get inherited-props :header)))))
		(cons 'id id)))))
    (setq xxx inherited-props)
    (let ((format-results (or (plist-get template :format-results)
			      (plist-get inherited-props :format-results)
			      reorg-headline-format))
	  (result-sorters (or (append (plist-get inherited-props :sort-results)
				      (plist-get template :sort-results))
			      reorg-default-result-sort))
	  (sources (plist-get template :sources))
	  (action-function (or (plist-get inherited-props :action-function)
			       reorg--grouper-action-function))
	  (bullet (or (plist-get template :bullet)
		      (plist-get inherited-props :bullet)))
	  (face (or (plist-get template :face)
		    (plist-get inherited-props :face)
		    reorg-default-face))
	  (group (plist-get template :group))
	  (header-sort (plist-get template :sort-groups))
	  (level (or level 0))
	  results metadata)
      (setq inherited-props (car inherited-props))
      (when (and sources (not ignore-sources))
	(cl-loop for each in sources
		 do (push each reorg--current-sources))
	(setq data (append data (reorg--getter sources))))
      (setq results
	    (pcase group 
	      ((pred functionp)
	       (reorg--seq-group-by group data))
	      ((pred stringp)
	       (list (cons group data)))
	      ((pred (not null))
	       (when-let ((at-dots (seq-uniq 
				    (reorg--at-dot-search
				     group))))
		 (setq data (cl-loop
			     for d in data 
			     append
			     (cl-loop
			      for at-dot in at-dots
			      if (listp (alist-get at-dot d))
			      return
			      (cl-loop for x in (alist-get at-dot d)
				       collect
				       (let ((ppp (copy-alist d)))
					 (setf (alist-get at-dot ppp) x)
					 ppp))
			      finally return data))))
	       ;; NEW CODE GOES HERE.
	       ;; if group contains .!
	       ;; recurse.
	       (reorg--seq-group-by (reorg--walk-tree
				     group
				     #'reorg--turn-at-dot-to-dot
				     data)
				    data))))
      (if (null results)
	  (cl-loop for child in (plist-get template :children)
		   collect (reorg--get-group-and-sort
			    data
			    child
			    level
			    ignore-sources
			    (list :header nil
				  :format-results format-results
				  :parent-id nil
				  :sort-results result-sorters
				  :parent-template template
				  :bullet bullet
				  :face face)))
	(when header-sort
	  (setq results 
		(cond ((functionp header-sort)
		       (seq-sort-by #'car
				    header-sort
				    results))
		      (t (seq-sort-by #'car
				      `(lambda (x)
					 (let-alist x
					   ,header-sort))
				      results)))))

	;; If there are children, recurse 
	(cond ((and (plist-get template :children)
		    results)
	       (cl-loop
		for (header . children) in results
		append
		(cons
		 (funcall action-function
			  (setq metadata
				(get-header-metadata header
						     group
						     result-sorters
						     bullet))
			  nil
			  level
			  (list 
			   (cons 'header header)
			   (cons 'bullet bullet)
			   (cons 'reorg-face face)))
		 (cl-loop for child in (plist-get template :children)
			  collect 
			  (reorg--get-group-and-sort			  
			   children
			   child
			   (1+ level)
			   ignore-sources
			   (list 
			    :header header
			    :parent-template template
			    :format-results format-results
			    :sort-results result-sorters
			    :parent-id (alist-get 'id metadata)
			    :bullet bullet
			    :face face))))))
	      ((plist-get template :children)
	       (cl-loop for child in (plist-get template :children)
			collect
			(reorg--get-group-and-sort
			 data
			 child
			 (1+ level)
			 ignore-sources
			 (progn
			   (setq metadata (get-header-metadata nil
							       group
							       result-sorters
							       bullet))
			   (cl-loop for (key . val) in metadata
				    append (list (reorg--add-remove-colon key)
						 val))))))
	      (t 
	       (cl-loop for (header . children) in results
			append
			(cons				
			 (funcall
			  action-function
			  (setq metadata
				(get-header-metadata header
						     group
						     result-sorters
						     bullet))
			  nil
			  level
			  (plist-get template :overrides)
			  (plist-get template :post-overrides))
			 (list 
			  (cl-loop
			   with
			   children = 
			   (if result-sorters
			       (reorg--multi-sort result-sorters
						  children)
			     children)
			   for result in children
			   collect
			   (funcall
			    action-function
			    (append result
				    (list 
				     (cons 'group-id
					   (alist-get 'id metadata))
				     (cons 'parent-id
					   (alist-get 'id metadata))))
			    format-results
			    (1+ level)
			    (plist-get template :overrides)
			    (plist-get template :post-overrides))))))))))))

