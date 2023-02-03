

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
  ;; inheritence 
  (let ((group-id (md5 (pp-to-string template)))
	(format-results (or (plist-get template :format-results)
			    (plist-get template :format-result)
			    (plist-get inherited-props :format-results)))
	(sort-results (or (append (plist-get inherited-props :sort-results)
				  (plist-get template :sort-results))))
	(sources (or (plist-get template :sources)
		     (plist-get template :source)))
	(action-function (or (plist-get inherited-props :action-function)
			     reorg--grouper-action-function))
	(bullet (when-let ((bullet (or (plist-get template :bullet)
				       (plist-get template :bullets)
				       (plist-get inherited-props :bullet))))
		  (if (equal bullet "")
		      (char-to-string 8203) ;; use a zero width space
		    bullet)))
	(folded-bullet (or (plist-get template :folded-bullet)
			   (plist-get template :folded-bullets)
			   (plist-get inherited-props :folded-bullet)))
	(face (or (plist-get template :face)
		  (plist-get inherited-props :face)
		  reorg-default-face))
	(group (or (plist-get template :group)
		   (plist-get template :groups)))
	(sort-groups (or (plist-get template :sort-groups)
			 (plist-get template :sort-groups)))
	results metadata)
    (cl-labels ((get-header-metadata
		 (header parent-id)
		 (list
		  (cons 'branch-name header)
		  (cons 'reorg-branch t)
		  (cons 'branch-type 'branch)
		  (cons 'sort-results sort-results)
		  (cons 'bullet bullet)
		  (cons 'folded-bullet folded-bullet)
		  (cons 'reorg-level level)
		  (cons 'id (org-id-new))
		  (cons 'parent-id parent-id)
		  (cons 'group-id group-id)))
		(drill!
		 (seq prop &optional n level inherited)
		 (setq level (or level 1))
		 (if-let ((groups (reorg--seq-group-by
				   `(lambda (x)
				      (eval ;; I know. 
				       (reorg--walk-tree
					(reorg--walk-tree
					 ',prop
					 (lambda (xx)
					   (reorg--turn-dot-bang-to-val xx ,(or n 0) x)))
					(lambda (xx)
					  (reorg--turn-dot-to-val xx x)))))
				   seq)))
		     (progn 
		       (when sort-groups
  			 (setq groups (seq-sort-by #'car
						   sort-groups
						   groups)))
		       (cl-loop
			for each in groups
			do (setq metadata
				 (get-header-metadata
				  (car each)
				  (plist-get inherited :id)))
			collect 
			(cons (reorg--create-headline-string
			       metadata
			       nil
			       level)
			      (drill! (cdr each)
				      prop						
				      (1+ (or n 0))
				      (1+ level)
				      (plist-put 
				       inherited-props
				       :id 
				       (alist-get 'id metadata))))))
		   (if (plist-get template :children)
		       (cl-loop for child in (plist-get template :children)
				collect (reorg--get-group-and-sort
					 seq
					 child
					 ;; same test here re: did we increase the level
					 (1+ level)
					 ignore-sources
					 (list :header nil
					       :format-results format-results
					       :id (plist-get inherited :id)
					       :sort-results sort-results
					       :parent-template template
					       :bullet bullet
					       :folded-bullet folded-bullet
					       :face face)))
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
					(cons 'group-id
					      (alist-get 'id metadata))
					(cons 'parent-id
					      (alist-get 'id metadata))))
			       format-results
			       level
			       (plist-get template :overrides)
			       (plist-get template :post-overrides)))))))
      ;; end drill 
      
      (when (and sources (not ignore-sources))
	(cl-loop for each in sources
		 do (push each reorg--current-sources))
	(setq data (append data (reorg--getter sources))))

      ;; begin .@
      (when-let ((at-dots (seq-uniq 
			   (reorg--dot-at-search
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
				      #'reorg--turn-dot-at-to-dot
				      data)))
      ;; end .@
      
      ;; begin .! 
      (if (reorg--dot-bang-search group)
	  (drill! data group nil level inherited-props)
	;;end .!
	(setq results
	      (pcase group 
		((pred functionp)
		 (reorg--seq-group-by group data))
		((pred stringp)
		 (list (cons group data)))
		((pred (not null))
		 (reorg--seq-group-by group
				      data))
		(_ nil)))
	;; no results
	(if (null results)
	    (cl-loop for child in (plist-get template :children)
		     collect (reorg--get-group-and-sort
			      data
			      child
			      level
			      ignore-sources
			      (list :header nil
				    :format-results format-results
				    :id nil
				    :sort-results sort-results
				    :parent-template template
				    :bullet bullet
				    :folded-bullet folded-bullet
				    :face face)))
	  ;; if results 
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

	  ;; If there are results and children
	  (cond ((and (plist-get template :children)
		      results)
		 (cl-loop
		  for (header . children) in results
		  append
		  (cons
		   ;; create the header 
		   (funcall action-function
			    (setq metadata
				  (get-header-metadata
				   header (plist-get inherited-props :id)))

			    nil
			    level
			    (list 
			     (cons 'header header)
			     (cons 'bullet bullet)
			     (cons 'folded-bullet folded-bullet)
			     (cons 'reorg-face face)))
		   ;; there is metadata 
		   (cl-loop for child in (plist-get template :children)
			    collect 
			    (reorg--get-group-and-sort			  
			     children
			     child
			     (if (equal "​"
					(alist-get
					 'branch-name
					 metadata))
				 level
			       (1+ level))
			     ignore-sources
			     (list 
			      :header header
			      :parent-template template
			      :format-results format-results
			      :sort-results sort-results
			      :id (alist-get 'id metadata)
			      :bullet bullet
			      :folded-bullet folded-bullet
			      :face face))))))
		;; if there are only children 
		((plist-get template :children)
		 (debug nil "I don't think this happens")
		 (cl-loop for child in (plist-get template :children)
			  collect
			  (reorg--get-group-and-sort
			   data
			   child
			   level
			   ignore-sources
			   (progn
			     (setq metadata (get-header-metadata nil inherited-props))
			     (cl-loop for (key . val) in metadata
				      append (list (reorg--add-remove-colon key)
						   val))))))
		;; if there are normal results 
		(t 
		 (cl-loop for (header . children) in results
			  append
			  (cons				
			   (funcall
			    action-function
			    (setq metadata
				  (get-header-metadata
				   header
				   (plist-get inherited-props :id)))
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
			      (if (equal ""
					 (alist-get
					  'branch-name
					  metadata))
				  level
				(1+ level))
			      (plist-get template :overrides)
			      (plist-get template :post-overrides)))))))))))))
