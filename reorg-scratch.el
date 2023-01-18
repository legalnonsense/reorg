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
		((parent-dirs . ("etc" "something" "legal" "xxx"))
		 (name . "5"))))

(reorg--drill-group xxx-seq 'parent-dirs nil #'string<) ;;;test 


(defun reorg--drill-group (seq prop &optional n sorter)
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
		   collect (cons (car each) 
				 (reorg--drill-group (cdr each) prop (1+ (or n 0)) sorter))))
      ;; children need to be called here
      seq)))

(defun reorg--get-group-and-sort (data
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
    (error "Invalid keys in entry in template: %s" invalid-keys))
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
    (cl-flet ((get-header-metadata
	       (header)
	       (list
		(cons 'branch-name header)
		(cons 'reorg-branch t)
		(cons 'branch-type 'branch)
		(cons 'result-sorters result-sorters)
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
					      :header))))))))
      ;; (setq inherited-props (car inherited-props))
      (when (and sources (not ignore-sources))
	(cl-loop for each in sources
		 do (push each reorg--current-sources))
	(setq data (append data (reorg--getter sources))))
      (pcase group
	((and (pred symbolp)
	      g
	      (guard (s-starts-with-p ".!" (symbol-name g))))
	 (error "Drill code"))
	(_ 
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
		    ;; copy entries 
		    (setq data (cl-loop
				for d in data 
				append
				(cl-loop
				 for at-dot in at-dots
				 if (listp (alist-get at-dot d))
				 return
				 (cl-loop for x in (alist-get at-dot d)
					  collect
					  ;; not sure about this copy-alist
					  ;; not sure what ppp means 
					  (let ((ppp (copy-alist d)))
					    (setf (alist-get at-dot ppp) x)
					    ppp))
				 finally return data))
			  ;; remove at-dots from the template
			  ;; TODO stop calling them "at-dots"
			  group (reorg--walk-tree group
						  #'reorg--turn-at-dot-to-dot
						  data)))))
	       results (reorg--seq-group-by group
					    data))
	 
	 (if (null results)
	     (progn
	       ;; this happens a lot
	       ;; but why do we want it do?
	       ;; if there are no results, who cares? 
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
				       :face face))))
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
			       :sort-results result-sorters
			       :parent-id (alist-get 'id metadata)
			       :bullet bullet
			       :face face))))))
		 ;; if results are nil, but there are children.
		 ;; WHY WOULD WE KEEP GOING? 
		 ((plist-get template :children)
		  (debug nil "results are nil again!?")
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
		 ;; If there are no children, finish the recursion 
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
					;; I am not proud of any of this.
					(cons 'group-id
					      (alist-get 'id metadata))
					(cons 'parent-id
					      (alist-get 'id metadata))))
			       format-results
			       (1+ level)
			       (plist-get template :overrides)
			       (plist-get template :post-overrides))))))))))))))

