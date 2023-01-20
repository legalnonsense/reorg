;; ;; -*- lexical-binding: t; -*-


;; (defun xxx (seq)
;;   (cl-loop for each in seq
;; 	   with results = nil
;; 	   with num = (length seq)
;; 	   do (cl-loop with done = 0
;; 		       for x from 0		       
;; 		       if (< x (length (alist-get 'parent-dirs each)))
;; 		       do (push
;; 			   each
;; 			   (alist-get (intern (nth x (alist-get 'parent-dirs each))) results))
;; 		       else do (cl-incf done)
;; 		       when (= done num)
;; 		       return results)
;; 	   finally return results))

(setq xxx-seq `(((parent-dirs . ("home" "jeff" ".emacs.d" "lisp" "reorg" "TEST"))
		 (name . "1"))
		((parent-dirs . ("home" "jeff" ".emacs.d" "lisp" "reorg" "TEST"))
		 (name . "2"))
		((parent-dirs . ("home" "jeff" "legal" "whatever"))
		 (name . "3"))
		((parent-dirs . ("home" "jeff" "legal" "xxx"))
		 (name . "4"))
		((parent-dirs . ("home" "jeff" "something"))
		 (name . "6"))
		((parent-dirs . ("home" "jarff" "something"))
		 (name . "6"))
		((parent-dirs . ("etc" "something" "legal" "xxx"))
		 (name . "5"))))

(reorg--drill-group xxx-seq 'parent-dirs nil #'string>) ;;;test 


(defun reorg--drill-group (seq prop &optional n sorter level)
  (setq level (or level 1))
  (let ((groups (reorg--seq-group-by
		 (lambda (x)
		   (nth (or n 0) (alist-get prop x)))
		 seq)))
    (if groups
	(progn 
	  (when sorter
  	    (setq groups (seq-sort-by #'car
				      sorter
				      groups)))
	  (cl-loop for each in groups
		   collect (cons (reorg--create-headline-string
				  (list (cons 'branch-name (car each))
					(cons 'reorg-branch t)
					(cons 'branch-type 'branch))
				  nil
				  level)
				 (reorg--drill-group (cdr each)
						     prop
						     (1+ (or n 0))
						     sorter
						     (1+ level)))))
      (if (plist-get template :children)
	  (cl-loop for child in (plist-get template :children)
		   collect (reorg--get-group-and-sort
			    seq
			    child
			    (1+ level)
			    ignore-sources))
	(when sort-results
	  (setq seq 
		(reorg--multi-sort sort-results
				   seq)))
	(cl-loop for each in seq
		 collect 
		 (funcall
		  action-function
		  (append each
			  (list
			   ;; I am not proud of any of this.
			   (cons 'group-id
				 (alist-get 'id metadata))
			   (cons 'parent-id
				 (alist-get 'id metadata))))
		  format-results
		  (1+ level)
		  (plist-get template :overrides)
		  (plist-get template :post-overrides)))))))

(defun reorg--check-template-keys (template)
  "check keys"
  (when-let ((invalid-keys
	      (seq-difference 
	       (cl-loop for key in template by #'cddr
			collect key)
	       reorg--valid-template-keys)))
    (error "Invalid keys in entry in template: %s" invalid-keys)))

(defun reorg--get-group-and-sort (data
				  template
				  level
				  ignore-sources
				  &optional
				  inherited-props)
  "Apply TEMPLATE to DATA and apply the :action-function 
specified in the template or `reorg--grouper-action-function'
to the results."
  (reorg--check-template-keys template)    
  (let ((format-results (or (plist-get template :format-results)
			    (plist-get inherited-props :format-results)))
	(sort-results (or (append (plist-get inherited-props :sort-results)
				  (plist-get template :sort-results))))
	(sources (plist-get template :sources))
	(action-function (or (plist-get inherited-props :action-function)
			     reorg--grouper-action-function))
	(bullet (or (plist-get template :bullet)
		    (plist-get inherited-props :bullet)))
	(face (or (plist-get template :face)
		  (plist-get inherited-props :face)
		  reorg-default-face))
	(group (plist-get template :group))
	(sort-groups (plist-get template :sort-groups))
	results metadata)
    (cl-labels ((get-header-metadata
		 (header)
		 (list
		  (cons 'branch-name header)
		  (cons 'reorg-branch t)
		  (cons 'branch-type 'branch)
		  (cons 'sort-results sort-results)
		  (cons 'bullet bullet)
		  (cons 'reorg-level level)
		  (cons 'id (org-id-new))
		  (cons 'parent-id (plist-get inherited-props
					      :parent-id))
		  (cons 'group-id (md5
				   (concat 
				    (pp-to-string
				     (plist-get inherited-props
						:parent-template))
				    (pp-to-string
				     (plist-get inherited-props
						:header)))))))
		(drill!
		 (seq prop &optional n level)
		 (setq level (or level 1))
		 (let ((groups (reorg--seq-group-by
				(lambda (x)
				  (nth (or n 0) (alist-get prop x)))
				seq)))
		   (if groups
		       (progn 
			 (when sort-groups
  			   (setq groups (seq-sort-by #'car
						     sort-groups
						     groups)))
			 (cl-loop for each in groups
				  collect (cons (reorg--create-headline-string
						 (setq metadata
						       (get-header-metadata (car each)))
						 nil
						 level)
						(drill! (cdr each)
							prop
							(1+ (or n 0))
							(1+ level)))))
		     (if (plist-get template :children)
			 (cl-loop for child in (plist-get template :children)
				  collect (reorg--get-group-and-sort
					   seq
					   child
					   (1+ level)
					   ignore-sources))
		       (when sort-results
			 (setq seq 
			       (reorg--multi-sort sort-results
						  seq)))
		       (cl-loop for each in seq
				collect 
				(funcall
				 action-function
				 (append each
					 (list
					  ;; I am not proud of any of this.
					  (cons 'group-id
						(alist-get 'id metadata))
					  (cons 'parent-id
						(alist-get 'id metadata))))
				 format-results
				 (1+ level)
				 (plist-get template :overrides)
				 (plist-get template :post-overrides))))))))
      
      ;; (setq inherited-props (car inherited-props))
      ;; I believe this was needed because I used &rest in the parameters 
      (when (and sources (not ignore-sources))
	(cl-loop for each in sources
		 do (push each reorg--current-sources))
	;; append new data to old (inherited) data 
	(setq data (append data (reorg--getter sources))))
      
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
		     finally return data)))
	(setq group (reorg--walk-tree group
				      #'reorg--turn-at-dot-to-dot
				      data)))
      ;; dealing with .!

      (if-let ((bit
		(and (symbolp group)
		     (s-starts-with-p ".!" (symbol-name group))
		     (intern (substring (symbol-name group) 2)))))
	  ;; off-load everthing to the drill! function 
	  (drill! data bit nil level)
	;; all that follows is for when there is no drill bit 
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
				    :sort-results sort-results
				    :parent-template template
				    :bullet bullet
				    :face face)))
	  (when sort-groups
	    (setq results 
		  (cond ((functionp sort-groups)
			 (seq-sort-by #'car
				      sort-groups
				      results))
			(t (seq-sort-by #'car
					`(lambda (x)
					   (let-alist x
					     ,sort-groups))
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
				  (get-header-metadata header))
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
			      :sort-results sort-results
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
			     (setq metadata (get-header-metadata nil))
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
				  (get-header-metadata header))
			    nil
			    level
			    (plist-get template :overrides)
			    (plist-get template :post-overrides))
			   (list 
			    (cl-loop
			     with
			     children = 
			     (if sort-results
				 (reorg--multi-sort sort-results
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
			      (plist-get template :post-overrides)))))))))))))


;; (defun reorg--get-group-and-sort (data
;; 				  template
;; 				  level
;; 				  ignore-sources
;; 				  &optional
;; 				  inherited-props)
;;   "Apply TEMPLATE to DATA and apply the :action-function 
;; specified in the template or `reorg--grouper-action-function'
;; to the results."
;;   (reorg--check-template-keys template)    
;;   (let ((format-results (or (plist-get template :format-results)
;; 			    (plist-get inherited-props :format-results)))
;; 	(sort-results (or (append (plist-get inherited-props :sort-results)
;; 				  (plist-get template :sort-results))))
;; 	(sources (plist-get template :sources))
;; 	(action-function (or (plist-get inherited-props :action-function)
;; 			     reorg--grouper-action-function))
;; 	(bullet (or (plist-get template :bullet)
;; 		    (plist-get inherited-props :bullet)))
;; 	(face (or (plist-get template :face)
;; 		  (plist-get inherited-props :face)
;; 		  reorg-default-face))
;; 	(group (plist-get template :group))
;; 	(sort-groups (plist-get template :sort-groups))
;; 	results metadata)
;;     (cl-labels ((get-header-metadata
;; 		 (header)
;; 		 (list
;; 		  (cons 'branch-name header)
;; 		  (cons 'reorg-branch t)
;; 		  (cons 'branch-type 'branch)
;; 		  (cons 'sort-results sort-results)
;; 		  (cons 'bullet bullet)
;; 		  (cons 'reorg-level level)
;; 		  (cons 'id (org-id-new))
;; 		  (cons 'parent-id (plist-get inherited-props
;; 					      :parent-id))
;; 		  (cons 'group-id (md5
;; 				   (concat 
;; 				    (pp-to-string
;; 				     (plist-get inherited-props
;; 						:parent-template))
;; 				    (pp-to-string
;; 				     (plist-get inherited-props
;; 						:header)))))))
;; 		(drill!
;; 		 (seq prop &optional n l)
;; 		 (let ((groups (reorg--seq-group-by
;; 				(lambda (x)
;; 				  (nth (or n 0) (alist-get prop x)))
;; 				seq)))
;; 		   (if (not groups)
;; 		       (reorg--get-group-and-sort
;; 			seq
;; 			(plist-get template :children)
;; 			(1+ l)
;; 			ignore-sources
;; 			(list 
;; 			 :header nil
;; 			 :parent-template template
;; 			 :format-results format-results
;; 			 :sort-results sort-results
;; 			 :parent-id (alist-get 'id metadata)
;; 			 :bullet bullet
;; 			 :face face))  
;; 		     (when sort-groups
;;   		       (setq groups (seq-sort-by #'car
;; 						 sort-groups
;; 						 groups)))
;; 		     (cl-loop for each in groups
;; 			      for x from l
;; 			      collect (cons (funcall action-function
;; 						     (setq metadata
;; 							   (get-header-metadata
;; 							    (car each)))
;; 						     format-results
;; 						     x
;; 						     (plist-get template :overrides)
;; 						     (plist-get template :post-overrides))
;; 					    (drill!
;; 					     (cdr each)
;; 					     prop
;; 					     (1+ (or n 0))
;; 					     (1+ l)
;; 					     )))))))

;;       ;; (setq inherited-props (car inherited-props))
;;       ;; I believe this was needed because I used &rest in the parameters 
;;       (when (and sources (not ignore-sources))
;; 	(cl-loop for each in sources
;; 		 do (push each reorg--current-sources))
;; 	;; append new data to old (inherited) data 
;; 	(setq data (append data (reorg--getter sources))))

;;       ;; dealing with .@
;;       (when (and (not (functionp group))
;; 		 (not (stringp group)))
;; 	;; process .@
;; 	(when-let ((at-dots (seq-uniq 
;; 			     (reorg--at-dot-search
;; 			      group))))
;; 	  (setq data (cl-loop
;; 		      for d in data 
;; 		      append
;; 		      (cl-loop
;; 		       for at-dot in at-dots
;; 		       if (listp (alist-get at-dot d))
;; 		       return
;; 		       (cl-loop for x in (alist-get at-dot d)
;; 				collect
;; 				(let ((ppp (copy-alist d)))
;; 				  (setf (alist-get at-dot ppp) x)
;; 				  ppp))
;; 		       finally return data)))
;; 	  (setq group (reorg--walk-tree group
;; 					#'reorg--turn-at-dot-to-dot
;; 					data))))
;;       ;; dealing with .!
;;       (if-let ((bit
;; 		(and (symbolp group)
;; 		     (s-starts-with-p ".!" (symbol-name group))
;; 		     (intern (substring (symbol-name group) 2)))))
;; 	  (drill! data bit nil level)
;; 	;; no .! encountered 
;; 	(setq results (reorg--seq-group-by group
;; 					   data))
;; 	(when sort-groups
;; 	  (setq results 
;; 		(cond ((functionp sort-groups)
;; 		       (seq-sort-by #'car
;; 				    sort-groups
;; 				    results))
;; 		      (t (seq-sort-by #'car
;; 				      `(lambda (x)
;; 					 (let-alist x
;; 					   ,sort-groups))
;; 				      results)))))
;; 	;; If there are template children, recurse 
;; 	(cond ((and (plist-get template :children)
;; 		    results)
;; 	       (cl-loop
;; 		for (header . children) in results
;; 		append
;; 		(cons
;; 		 (funcall action-function
;; 			  (setq metadata
;; 				(get-header-metadata header))
;; 			  nil
;; 			  level
;; 			  (list 
;; 			   (cons 'header header)
;; 			   (cons 'bullet bullet)
;; 			   (cons 'reorg-face face)))
;; 		 (cl-loop for child in (plist-get template :children)
;; 			  collect 
;; 			  (reorg--get-group-and-sort			  
;; 			   children
;; 			   child
;; 			   (1+ level)
;; 			   ignore-sources
;; 			   (list 
;; 			    :header header
;; 			    :parent-template template
;; 			    :format-results format-results
;; 			    :sort-results sort-results
;; 			    :parent-id (alist-get 'id metadata)
;; 			    :bullet bullet
;; 			    :face face))))))
;; 	      ;; if results are nil, but there are children.
;; 	      ;; Q. WHY WOULD WE KEEP GOING?
;; 	      ;; A. Because the template could declare
;; 	      ;;    additional sources. 
;; 	      ((plist-get template :children)
;; 	       (cl-loop for child in (plist-get template :children)
;; 			collect
;; 			(reorg--get-group-and-sort
;; 			 data
;; 			 child
;; 			 (1+ level)
;; 			 ignore-sources
;; 			 (progn
;; 			   (setq metadata (get-header-metadata nil))
;; 			   (cl-loop for (key . val) in metadata
;; 				    append (list (reorg--add-remove-colon key)
;; 						 val))))))
;; 	      ;; If there are no children, finish the recursion 
;; 	      (t
;; 	       (setq results
;; 		     (if sort-results
;; 			 (reorg--multi-sort sort-results
;; 					    data)
;; 		       data))
;; 	       (cl-loop for each in data
;; 			collect 
;; 			(funcall
;; 			 action-function
;; 			 (append each
;; 				 (list
;; 				  ;; I am not proud of any of this.
;; 				  (cons 'group-id
;; 					(alist-get 'id metadata))
;; 				  (cons 'parent-id
;; 					(alist-get 'id metadata))))
;; 			 format-results
;; 			 (1+ level)
;; 			 (plist-get template :overrides)
;; 			 (plist-get template :post-overrides)))))))))

