;; ;; -*- lexical-binding: t; -*-



(setq xxx-seq '(((reorg-level . 8) (dirp) (id . "2473fecf-ed14-474d-86f6-129396268fc1") (parent . "/home/jeff/.emacs.d/lisp/reorg/TEST") (fullname . "/home/jeff/.emacs.d/lisp/reorg/TEST/second-example-screenshot.png") (filename . "second-example-screenshot.png") (extension . "png") (parent-dirs "home" "jeff" ".emacs.d" "lisp" "reorg" "TEST") (path . "/home/jeff/.emacs.d/lisp/reorg/TEST/second-example-screenshot.png") (depth . 7) (class . files) (group-id . "7e0e9cba-1314-459f-b923-48447dbc74d6") (parent-id . "7e0e9cba-1314-459f-b923-48447dbc74d6") (reorg-headline . "second-example-screenshot.png;; => ") (reorg-class . files) (parent-id . "7e0e9cba-1314-459f-b923-48447dbc74d6") (reorg-field-type . leaf))
		((reorg-level . 8) (dirp) (id . "b0a84306-7e51-4e6d-9c1f-e5e92e5a14d0") (parent . "/home/jeff/.emacs.d/lisp/reorg/TESTS") (fullname . "/home/jeff/.emacs.d/lisp/reorg/TESTS/elgantt-test.org") (filename . "elgantt-test.org") (extension . "org") (parent-dirs "home" "jeff" ".emacs.d" "lisp" "reorg" "TESTS") (path . "/home/jeff/.emacs.d/lisp/reorg/TESTS/elgantt-test.org") (depth . 7) (class . files) (group-id . "a345aa14-a3d8-42c3-a923-191da66c4af8") (parent-id . "a345aa14-a3d8-42c3-a923-191da66c4af8") (reorg-headline . "elgantt-test.org;; => ") (reorg-class . files) (parent-id . "a345aa14-a3d8-42c3-a923-191da66c4af8") (reorg-field-type . leaf))))


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

