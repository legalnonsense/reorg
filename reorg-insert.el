;; -*- lexical-binding: t; -*-


;; does group exist? if so, go to it. If not, create it.
;; loop until there are no more groups
;; does the leaf exist? if so, go to it. if not, loop
;; over existing leaves until result-sort says to insert 

(cl-defun reorg--insert (list template &optional (n 0 np))
  "Group RESULTS according to TEMPLATE."
  (cl-labels
      ((doloop (data
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
		     (result-sort (plist-get template :sort-results))
		     (overrides (plist-get template :format-string-overrides)))
		 (when result-sort
		   (setq result-sorters
			 (append result-sorters					  
				 (cl-loop for (form . pred) in result-sort
					  collect (cons `(lambda (x)
							   (let-alist x
							     ,form))
							pred)))))
		 (unless np
		   (let ((old (cl-copy-list data)))
		     (setcar data '_)
		     (setcdr data (list old))))
		 (setf
		  (nth n (cdr data))
		  (--> (nth n (cdr data))
		       (cond
			((functionp grouper)
			 (->> it
			      (seq-group-by grouper)
			      (seq-map (lambda (x) (list (car x) (cdr x))))))
			((stringp grouper)
			 (list (list grouper it)))
			(t
			 (when-let ((at-dots (cl-delete-duplicates
					      (reorg--dot-at-search grouper)
					      :test #'equal)))
			   (setq
			    it 				 
			    (cl-loop
			     for data in it
			     append (cl-loop
				     for (_ . at-dot) in at-dots
				     if (listp (alist-get at-dot data))
				     return (cl-loop for x in (alist-get at-dot data)
						     collect (let ((ppp (copy-alist data)))
							       (setf (alist-get at-dot ppp) x)
							       ppp))
				     finally return data))))
			 (setq xxx grouper)
			 (->> it			      
			      (reorg--seq-group-by
			       (reorg--walk-tree grouper
						 #'reorg--turn-at-dot-to-dot))
			      (seq-map (lambda (x) (list (car x) (cdr x)))))))
		       (seq-filter (lambda (x) (and (not (null (car x)))
						    (not (null (cdr x)))
						    (not (null x))))
				   it)
		       (cl-loop
			for x in it
			do (setf
			    (car x)
			    (let ((ddd (list
					(cons 'branch-name (car x))
					(cons 'headline (car x))
					(cons 'reorg-branch t)
					(cons 'result-sorters result-sorters)
					(cons 'grouper-list `(lambda (x)
							       (let-alist x
								 ,grouper)))
					(cons 'branch-predicate `(lambda (x)
								   (let-alist x
								     ,grouper)))
					(cons 'branch-result (car x))
					(cons 'grouper-list-results (car x))
					(cons 'format-string format-string)
					(cons 'result-sorters result-sorters)
					(cons 'template template)
					(cons 'children children)
					(cons 'branch-sorter heading-sorter)
					(cons 'branch-sort-getter heading-sort-getter)
					(cons 'reorg-level level))))
			      (append ddd
				      `((group-id . ,(md5 (with-temp-buffer
							    (insert (pp grouper))
							    (buffer-string))))
					(id . ,(md5 (with-temp-buffer
						      (insert (pp ddd))
						      (buffer-string))))))))
			
			finally return it)
		       (seq-filter (lambda (x) (and (not (null (car x)))
						    (not (null (cdr x)))
						    (not (null x))))
				   it)
		       (cl-loop for x in it
				do (setf (car x)
					 (reorg--create-headline-string (car x)
									(copy-tree format-string)
									level
									overrides))
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
					 (list (alist-get 'branch-value
							  (nth y (nth n (cdr data))))))
					format-string
					(1+ level)))))

		   (progn ;; else 
		     (when result-sorters
		       (cl-loop for x below (length (nth n (cdr data)))
				do (setf
				    (cadr (nth x (nth n (cdr data))))
				    (reorg--multi-sort result-sorters
						       (cadr (nth x (nth n (cdr data))))))))
		     (cl-loop
		      for x below (length (nth n (cdr data)))
		      do
		      (setf
		       (cadr (nth x (nth n (cdr data))))
		       (cl-loop
			for each in (cadr (nth x (nth n (cdr data))))
			collect
			(progn (setf (alist-get 'reorg-stars each) (1+ level))
			       (push (cons 'reorg-level (1+ level)) each)
			       (push (cons 'group-id (md5 (with-temp-buffer
							    (insert (pp grouper))
							    (buffer-string))))
				     each)
			       (reorg--create-headline-string each
							      format-string
							      (1+ level)
							      overrides))))))))))
    (doloop list template)
    (cadr list)))
