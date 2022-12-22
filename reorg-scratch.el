;; -*- lexical-binding: t; -*-

;;     ﯍    

(defvar reorg-default-result-sort nil "")
(defvar reorg--field-property-name 'reorg-field-name "")
(defvar reorg--extra-prop-list nil "")
(defvar reorg--grouper-action-function #'reorg--create-headline-string*
  "")
;; (setq reorg--grouper-action-function (lambda (data
;; 					      format-string
;; 					      &optional
;; 					      level
;; 					      overrides)
;; 				       (let ((h (reorg--create-headline-string*
;; 						 data
;; 						 format-string
;; 						 level
;; 						 overrides)))
;; 					 (with-current-buffer (get-buffer-create "*REORG*")
;; 					   (insert h))
;; 					 h)))

(defun reorg--map-all-branches (func)
  "map all"
  (save-excursion 
    (goto-char (point-min))
    (while (reorg--goto-next-branch)
      (funcall func))))

(defun reorg--delete-headers-maybe* ()
  "delete headers at point if it has no children.
assume the point is at a branch." 
  (cl-loop with p = nil
	   if (reorg--get-next-child)
	   return t
	   else
	   do (setq p (reorg--get-parent))
	   do (reorg--delete-header-at-point)
	   if (null p)
	   return t
	   else do (goto-char p)))

(defun reorg--multi-sort* (functions-and-predicates sequence)
  "FUNCTIONS-AND-PREDICATES is an alist of functions and predicates.
It uses the FUNCTION and PREDICATE arguments useable by `seq-sort-by'.
SEQUENCE is a sequence to sort. USES LET-ALIST"
  (seq-sort 
   (lambda (a b)
     (cl-loop for (form . pred) in functions-and-predicates	      
	      unless (equal (funcall `(lambda (a) (let-alist a ,form)) a)
			    (funcall `(lambda (b) (let-alist b ,form)) b))
	      return (funcall pred
			      (funcall `(lambda (a) (let-alist a ,form)) a)
			      (funcall `(lambda (b) (let-alist b ,form)) b))))
   sequence))

;; (defun reorg--group-and-sort* (data
;; 			       template
;; 			       &optional
;; 			       action
;; 			       sorters
;; 			       format-string
;; 			       level
;; 			       parent-id
;; 			       parent-header
;; 			       bullet
;; 			       face)
;;   "group and sort and run action on the results"
;;   (cl-flet ((get-header-props
;; 	     (header groups sss bullet)
;; 	     (reorg--thread-as*
;; 	       (props (list 
;; 		       (cons 'branch-name header)
;; 		       (cons 'headline header)
;; 		       (cons 'reorg-branch t)
;; 		       (cons 'branch-type 'branch)
;; 		       (cons 'grouper-list
;; 			     `(lambda (x)
;; 				(let-alist x
;; 				  ,(plist-get groups :group))))
;; 		       (cons 'branch-predicate
;; 			     `(lambda (x)
;; 				(let-alist x
;; 				  ,(plist-get groups :group))))
;; 		       (cons 'branch-result header)
;; 		       (cons 'grouper-list-results header)
;; 		       (cons 'format-string format-string)
;; 		       (cons 'result-sorters sss)
;; 		       (cons 'template template)
;; 		       (cons 'sort-groups (plist-get groups :sort-groups))
;; 		       (cons 'children (plist-get groups :children))
;; 		       (cons 'branch-sorter ;; heading-sorter
;; 			     nil)
;; 		       (cons 'branch-sort-getter ;; heading-sort-getter
;; 			     nil)
;; 		       (cons 'bullet bullet)
;; 		       (cons 'reorg-level level)))
;; 	       (append props
;; 		       `((group-id . ,(md5 (with-temp-buffer
;; 					     (insert (pp parent-id))
;; 					     (insert (pp parent-header))
;; 					     (insert (pp groups))
;; 					     (buffer-string))))
;; 			 (id . ,(org-id-new)))))))
;;     (let (pppz)
;;       ;; inheritance
;;       (setq format-string
;; 	    (or format-string
;; 		(plist-get template :format-results)
;; 		reorg-headline-format))
;;       ;; sorters
;;       ;; (or sorters
;;       ;; 	(plist-get template :sort-results)
;;       ;; 	reorg-default-result-sort))

;;       ;; for each child...
;;       (cl-loop
;;        with sorterz = sorters
;;        for groups in (plist-get template :children)
;;        ;; inheritence for
;;        do (setq format-string (or (plist-get groups :format-results)
;; 				  format-string
;; 				  reorg-headline-format)
;; 		sorterz (append sorters (plist-get groups :sort-results))
;; 		bullet (or bullet (plist-get groups :bullet))
;; 		level (or level 1))
;;        append (reorg--thread-as* data
;; 		(pcase (plist-get groups :group)
;; 		  ((pred functionp)
;; 		   ;; easy.
;; 		   (reorg--seq-group-by* (plist-get groups :group)
;; 					 data))
;; 		  ((pred stringp)
;; 		   ;; easy. 
;; 		   (list (cons (plist-get groups :group)
;; 			       data)))
;; 		  (_
;; 		   ;; assume dot and at-dot notation.
;; 		   ;; not so easy.
;; 		   (when-let ((at-dots (seq-uniq 
;; 					(reorg--at-dot-search* (plist-get groups :group)))))
;; 		     ;; if it has an at-dot, then make a copy of each entry in DATA
;; 		     ;; if its value is a list.
;; 		     ;; For example, if the grouping form was:
;; 		     ;;
;; 		     ;; (if (evenp .@b)
;; 		     ;;     "B is even"
;; 		     ;;   "B is odd")
;; 		     ;;
;; 		     ;; and an entry in DATA had the value:
;; 		     ;; 
;; 		     ;; '((a . 1) (b . 2 3 4))
;; 		     ;;
;; 		     ;; Then, being being processed by the parser,
;; 		     ;; the entry is replaced by three separate entries:
;; 		     ;; 
;; 		     ;; '((a . 1) (b . 2))
;; 		     ;; '((a . 1) (b . 3))
;; 		     ;; '((a . 1) (b . 4))
;; 		     (setq
;; 		      data
;; 		      (cl-loop
;; 		       for d in data 
;; 		       append
;; 		       (cl-loop
;; 			for at-dot in at-dots
;; 			if (progn
;; 			     (listp (alist-get at-dot d)))
;; 			return (cl-loop for x in (alist-get at-dot d)
;; 					collect (let ((ppp (copy-alist d)))
;; 						  (setf (alist-get at-dot ppp) x)
;; 						  ppp))
;; 			finally return data))))
;; 		   (reorg--seq-group-by*
;; 		    ;; convert any at-dots to dots (yes, this could be part
;; 		    ;; of the conditional above, but I wanted to avoid it for
;; 		    ;; some reason. 
;; 		    (reorg--walk-tree* (plist-get groups :group)
;; 				       #'reorg--turn-at-dot-to-dot
;; 				       data)
;; 		    data)))
;; 		;; If there is a group sorter, sort the headers
;; 		;;TODO add the header meta data before the sorter 
;; 		(if-let ((sort (plist-get groups :sort-groups)))
;; 		    (cond ((functionp sort)
;; 			   (seq-sort-by #'car
;; 					sort
;; 					data))
;; 			  (t (seq-sort-by #'car
;; 					  `(lambda (x)
;; 					     (let-alist x
;; 					       ,sort))
;; 					  data)))
;; 		  data)
;; 		;; If there are children, recurse 
;; 		(if (plist-get groups :children)
;; 		    (cl-loop
;; 		     for (header . results) in data
;; 		     collect
;; 		     (cons
;; 		      (funcall
;; 		       (or action
;; 			   reorg--grouper-action-function)
;; 		       (setq pppz
;; 			     (get-header-props header groups sorterz bullet))
;; 		       nil
;; 		       level)
;; 		      (reorg--group-and-sort*			  
;; 		       results
;; 		       groups
;; 		       action
;; 		       sorterz
;; 		       format-string
;; 		       (1+ level)
;; 		       header
;; 		       bullet
;; 		       face)))
;; 		  ;; if there aren't children,
;; 		  ;; sort the results if necessary
;; 		  ;; then convert each batch of results
;; 		  ;; into to headline strings
;; 		  (cl-loop for (header . results) in data
;; 			   collect
;; 			   (cons				
;; 			    (funcall
;; 			     (or action
;; 				 reorg--grouper-action-function)
;; 			     (setq pppz
;; 				   (get-header-props header groups sorterz bullet))
;; 			     nil
;; 			     level)
;; 			    (list 
;; 			     (cl-loop
;; 			      with
;; 			      results = 
;; 			      (if sorterz
;; 				  (reorg--multi-sort* sorterz
;; 						      results)
;; 				results)
;; 			      for result in results
;; 			      collect
;; 			      (funcall
;; 			       (or action
;; 				   reorg--grouper-action-function)
;; 			       (append result
;; 				       (list 
;; 					(cons 'group-id
;; 					      (alist-get 'id pppz))))
;; 			       format-string
;; 			       (1+ level))))))))))))
(defvar reorg-default-bullet  "->" "")
(defvar reorg-default-face 'default "")

(defun reorg--group-and-sort* (data
			       template
			       level
			       &rest
			       inherited-props)
  (cl-flet ((get-header-metadata
	     (header groups sorts bullet)
	     (list
	      (cons 'branch-name header)
	      (cons 'reorg-branch t)
	      (cons 'branch-type 'branch)
	      (cons 'result-sorters sorts)
	      (cons 'bullet bullet)
	      (cons 'reorg-level level)
	      (cons 'group-id
		    (md5 (with-temp-buffer
			   (insert (pp (plist-get inherited-props :parent-id))
				   (pp (plist-get inherited-props :parent-header))
				   (pp groups))
			   (buffer-string))))
	      (cons 'id (org-id-new)))))

    "group and sort and run action on the results"
    (let ((format-results (or (plist-get template :format-results)
			      (plist-get inherited-props :format-results)
			      reorg-headline-format))
	  (result-sorters (or (append (plist-get inherited-props :sort-results)
				      (plist-get template :sort-results))
			      reorg-default-result-sort))
	  (action-function (or (plist-get inherited-props :action-function)
			       reorg--grouper-action-function))
	  (bullet (or (plist-get template :bullet)
		      (plist-get inherited-props :bullet)
		      reorg-default-bullet))
	  (face (or (plist-get template :face)
		    (plist-get inherited-props :face)
		    reorg-default-face))
	  (group (plist-get template :group))
	  (header-sort (plist-get template :sort-groups))
	  (level (or level 0))
	  results metadata)
      (setq results
	    (pcase group 
	      ((pred functionp)
	       (reorg--seq-group-by* group data))
	      ((pred stringp)
	       (list (cons group data)))
	      ((pred (not null))
	       (when-let ((at-dots (seq-uniq 
				    (reorg--at-dot-search*
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
	       (reorg--seq-group-by* (reorg--walk-tree*
                                      group
				      #'reorg--turn-at-dot-to-dot
				      data)
		                     data))))
      (if (null results)
	  (cl-loop for child in (plist-get template :children)
		   collect (reorg--group-and-sort* data child level
		                                   (list :header nil
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
	        collect
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
			  (reorg--group-and-sort*			  
			   children
			   child
			   (1+ level)
			   (list :header header
			         :bullet bullet
			         :face face))))))
	      ((plist-get template :children)
	       (cl-loop for child in (plist-get template :children)
		        collect
		        (reorg--group-and-sort*
		         data
		         child
		         (1+ level)
		         (setq metadata (get-header-metadata nil
							     group
							     result-sorters
							     bullet)))))
	      (t 
	       (cl-loop for (header . children) in results
		        collect
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
			  (plist-get template :post-overrides)
			  )
		         (list 
			  (cl-loop
			   with
			   children = 
			   (if result-sorters
			       (reorg--multi-sort* result-sorters
						   children)
			     children)
			   for result in children
			   collect
			   (funcall
			    action-function
			    (append result
				    (list 
				     (cons 'group-id
					   (alist-get 'id metadata))))
			    format-results
			    (1+ level)
			    (plist-get template :overrides)
			    (plist-get template :post-overrides))))))))))))

(defun reorg--create-headline-string* (data
				       format-string
				       &optional
				       level
				       overrides
				       post-overrides)
  "Create a headline string from DATA using FORMAT-STRING as the
template.  Use LEVEL number of leading stars.  Add text properties
`reorg--field-property-name' and  `reorg--data-property-name'."
  (cl-flet ((create-stars (num &optional data)
			  (make-string (if (functionp num)
					   (funcall num data)
					 num)
				       ?*)))
    ;; update the DATA that will be stored in
    ;; `reorg-data'    
    (push (cons 'reorg-level level) data)
    (cl-loop for (prop . val) in overrides
	     do (setf (alist-get prop data)
		      (if (let-alist--deep-dot-search val)
			  (funcall `(lambda ()
		                      (let-alist ',data 
					,val)))
			val)))
    (let (headline-text)
      (apply
       #'propertize
       ;; get the headline text 
       (setq headline-text
	     (if (alist-get 'reorg-branch data)
		 (concat (create-stars level)
			 " "
			 (alist-get 'branch-name data)
			 "\n")
	       (let* ((new (reorg--walk-tree*
			    format-string
			    #'reorg--turn-dot-to-display-string*
			    data))
		      (result (funcall `(lambda (data)
					  (concat ,@new "\n"))
				       data)))
		 result)))
       'reorg-data ;; property1
       (progn (setq data (append data
				 (list
				  (cons 'reorg-headline
					headline-text)
				  (cons 'reorg-class
					(alist-get 'class data))
				  (cons 'reorg-field-type
					(if (alist-get
					     'reorg-branch data)
					    'branch 'leaf)))))
	      (cl-loop for (prop . val) in post-overrides
		       do (setf (alist-get prop data)
				(alist-get prop post-overrides)))
	      data)
       ;; (funcall `(lambda ()
       ;; 		   (let-alist ',new-data 
       ;; 		     ,val)))))

       reorg--field-property-name ;; property2
       (if (alist-get 'reorg-branch data)
	   'branch 'leaf)
       (alist-get (alist-get 'class data) ;; extra props 
		  reorg--extra-prop-list)))))

;; (defmacro reorg--thread-as* (name &rest form)
;;   "I didn't know `-as->' existed..."
;;   (declare (indent defun))  
;;   (if (listp name)
;;       (append 
;;        `(let* ,(append `((,(car name) ,(cadr name)))
;; 		       (cl-loop for each in form
;; 				collect `(,(car name) ,each))))
;;        (list (car name)))
;;     (append `(let*
;; 		 ,(append `((,name ,(car form)))
;; 			  (cl-loop for each in (cdr form)
;; 				   collect `(,name ,each))))
;; 	    (list name))))

(defun reorg--seq-group-by* (func sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'.  Do not group results
that return nil."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (if (functionp func)
		     (funcall func elt)
		   (funcall `(lambda (e)
			       (let-alist e ,func))
			    elt)))
	    (cell (assoc key acc)))
       (if cell
	   (setcdr cell (push elt (cdr cell)))
	 (when key
	   (push (list key elt) acc)))
       acc))
   (seq-reverse sequence)
   nil))


(defun reorg--walk-tree* (form func &optional data)
  "Why the hell doesn't dash or seq do this?
Am I crazy?" 
  (cl-labels
      ((walk
	(form d)
	(cl-loop for each in form
		 if (listp each)
		 collect (walk each d)
		 else
		 collect (if d
			     (funcall func each d)
			   (funcall func each)))))
    (if (listp form)
	(walk form data)
      (if data 
	  (funcall func form data)
	(funcall func form)))))

(defun reorg--at-dot-search* (data)
  "Return alist of symbols inside DATA that start with a `.@'.
Perform a deep search and return a alist of any symbol
same symbol without the `@'.

See `let-alist--deep-dot-search'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\.@" name)
	;; Return the cons cell inside a list, so it can be appended
	;; with other results in the clause below.
	(list (intern (replace-match "" nil nil name))))))
   ;; (list (cons data (intern (replace-match "" nil nil name)))))))
   ((vectorp data)
    (apply #'nconc (mapcar #'reorg--at-dot-search* data)))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (reorg--at-dot-search* (cadr data)))
   (t (append (reorg--at-dot-search* (car data))
	      (reorg--at-dot-search* (cdr data))))))

(defun reorg--turn-dot-to-display-string* (elem data)
  "turn .symbol to a string using a display function."
  (if (and (symbolp elem)
	   (string-match "\\`\\." (symbol-name elem)))
      (let* ((sym (intern (substring (symbol-name elem) 1)))
	     (fu (reorg--get-display-func-name
		  (alist-get 'class data)
		  (substring (symbol-name elem) 1))))
	(cond ((eq sym 'stars)
	       (make-string (alist-get 'reorg-level data) ?*))
	      ((fboundp fu) (funcall fu data))
	      (t
	       (funcall `(lambda ()
			   (let-alist ',data
			     ,elem))))))
    elem))

(defun reorg--goto-next-sibling-same-group* (&optional data)
  "goot next sibing same group"
  (let ((id (or
	     (and data (alist-get 'group-id data))
	     (reorg--get-view-prop 'group-id))))
    (reorg--goto-next-prop 'group-id id)))

(defun reorg--goto-next-leaf-sibling* ()
  "goto next sibling"
  (reorg--goto-next-prop 'reorg-field-type
			 'leaf
			 (reorg--get-next-parent)))

;;TODO move these into `reorg--create-navigation-commands'
(defun reorg--goto-first-leaf* ()
  "goto the first leaf of the current group"
  (reorg--goto-next-prop 'reorg-field-type
			 'leaf
			 (let ((sib (reorg--get-next-sibling))
			       (par (reorg--get-next-parent)))
			   (if (and sib par)
			       (if (< sib par) sib par)
			     (if sib sib par)))))

(defun reorg--goto-id (header &optional group)
  "goto ID that matches the header string"
  (let ((point (point)))
    (goto-char (point-min))
    (if (reorg--goto-next-prop
	 (if group 'group-id 'id)
	 (alist-get (if group 'group-id 'id) header))
	(point)
      (reorg--goto-char point)
      nil)))

(defun reorg--delete-header-at-point ()
  "delete the header at point"
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

(defun reorg--insert-header-at-point (header-string &optional next-line)
  "insert header at point"
  (when next-line
    (forward-line))
  ;; (when (eobp)
  ;;   (insert (apply #'propertize "\n" (text-properties-at (1- (point))))))
  (save-excursion 
    (insert header-string))
  (reorg-dynamic-bullets--fontify-heading)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--find-header-location-within-groups* (header-string)
  "assume the point is on the first header in the group"
  (let-alist (get-text-property 0 'reorg-data header-string)    
    (if .sort-groups
	(cl-loop with point = (point)
		 if (equal .branch-name
			   (reorg--get-view-prop 'branch-name))
		 return (point)
		 else if (funcall .sort-groups
				  .branch-name
				  (reorg--get-view-prop 'branch-name))
		 return nil
		 while (reorg--goto-next-sibling-same-group*
			(get-text-property 0 'reorg-data header-string))
		 finally return (progn (goto-char point)
				       nil))
      (cl-loop with point = (point)
	       when (equal .branch-name
			   (reorg--get-view-prop 'branch-name))
	       return t
	       while (reorg--goto-next-sibling-same-group*
		      (get-text-property 0 'reorg-data header-string))
	       finally return (progn (goto-char point)
				     nil)))))

(defun reorg--find-first-header-group-member* (header-data)
  "goto the first header that matches the group-id of header-data"
  (let ((point (point)))
    (if (equal (reorg--get-view-prop 'group-id)
	       (alist-get 'group-id header-data))
	(point)
      (if (reorg--goto-next-prop 'group-id
				 (alist-get 'group-id header-data)
				 (reorg--get-next-parent))
	  (point)
	(goto-char point)
	nil))))

(defun reorg--find-leaf-location* (leaf-string &optional result-sorters)
  "find the location for LEAF-DATA among the current leaves. put the
point where the leaf should be inserted (ie, insert before)"
  ;; goto the first leaf if at a branch 
  (unless (eq 'leaf (reorg--get-view-prop 'reorg-field-type))
    (if (reorg--goto-first-leaf*)
	(when-let ((result-sorters
		    (or result-sorters
			(save-excursion 
			  (reorg--goto-parent)
			  (reorg--get-view-prop 'result-sorters))))) 
	  (let ((leaf-data (get-text-property 0 'reorg-data leaf-string)))
	    (cl-loop with point = (point)
		     when (cl-loop for (func . pred) in result-sorters
				   unless (equal (funcall `(lambda (x) (let-alist x ,func))
							  leaf-data)
						 (funcall `(lambda (x) (let-alist x ,func))
							  (reorg--get-view-prop)))
				   return (funcall pred
						   (funcall `(lambda (x) (let-alist x ,func))
							    leaf-data)
						   (funcall `(lambda (x) (let-alist x ,func))
							    (reorg--get-view-prop))))
		     return (point)
		     while (reorg--goto-next-leaf-sibling*)
		     finally (goto-char (line-beginning-position 2)))))
      (reorg--goto-next-heading))))

(defun reorg--get-next-group-id-change ()
  "get next group id change"
  (reorg--get-next-prop 'group-id
			(reorg--get-view-prop)
			nil
			(lambda (a b)
			  (not (equal a b)))))

(defun reorg--insert-new-heading* (data template)
  "insert an individual heading"
  (save-excursion 
    (goto-char (point-min))
    (reorg--map-id (alist-get 'id data)
		   (reorg-views--delete-leaf)
		   (when (reorg--goto-parent)
		     (reorg--delete-headers-maybe*)))
    (cl-loop with header-groups = (reorg--get-all-tree-paths
				   (reorg--group-and-sort*
				    (list data)
				    template
				    #'reorg--create-headline-string*)
				   (lambda (x)
				     (eq 'leaf
					 (get-text-property 0 'reorg-field-type x))))
	     for headers in header-groups
	     do (goto-char (point-min))
	     collect 
	     (cl-loop
	      with leaf = (car (last headers))
	      with leaf-props = (get-text-property 0 'reorg-data leaf)	    
	      for header in (butlast headers)
	      when (eq 'leaf (alist-get 'reorg-field-type leaf-props))
	      do (let* ((header-props (get-text-property 0 'reorg-data header))
			(group-id (alist-get 'group-id header-props))
			(id (alist-get 'id header-props)))
		   (unless (or (reorg--goto-id header-props)
			       (equal id (reorg--get-view-prop 'id)))		   
		     (if (reorg--find-first-header-group-member* header-props)
			 (unless (reorg--find-header-location-within-groups* header)
			   (reorg--insert-header-at-point header))
		       (reorg--insert-header-at-point header t))))
	      finally (progn (setq point (point))
			     (when (eq 'leaf (alist-get 'reorg-field-type leaf-props))
			       (reorg--find-leaf-location* leaf)
			       (reorg--insert-header-at-point leaf))
			     (goto-char point))))
    (org-indent-refresh-maybe (point-min) (point-max) nil)
    (run-hooks 'reorg--navigation-hook)))

;; (defun reorg--insert-new-heading** (data template)
;;   "insert an individual heading"
;;   (goto-char (point-min))
;;   (reorg--map-id (alist-get 'id data)
;; 		 (reorg-views--delete-leaf)
;; 		 (when (reorg--goto-parent)
;; 		   (reorg--delete-headers-maybe*)))
;;   (let ((groups (reorg--group-and-sort*
;; 		 (list data)
;; 		 template
;; 		 #'reorg--create-headline-string*)))
;;     (cl-labels ((leafp (entry)
;; 		       (eq 'leaf
;; 			   (alist-get 'reorg-field-type entry)))
;; 		(find (entry (depth 1))
;; 		      (let* ((props (get-text-property 0 'reorg-data entry))
;; 			     (group-id (alist-get 'group-id props))
;; 			     (id (alist-get 'id props)))

;; 		      (if (leafp entry)
;; 			  (progn (reorg--find-leaf-location* entry)
;; 				 (reorg--insert-header-at-point entry))




;; 			for headers in header-groups
;; do (goto-char (point-min))
;; collect 
;; (cl-loop
;;  with headers = (-flatten headers)    
;;  with leaf = (car (last headers))
;;  with leaf-props = (get-text-property 0 'reorg-data leaf)	    
;;  for header in (butlast headers)
;;  when (eq 'leaf (alist-get 'reorg-field-type leaf-props))
;;  do (let* ((header-props (get-text-property 0 'reorg-data header))
;; 	       (group-id (alist-get 'group-id header-props))
;; 	       (id (alist-get 'id header-props)))
;; 	  (unless (or (reorg--goto-id header-props)
;; 		      (equal id (reorg--get-view-prop 'id)))		   
;; 	    (if (reorg--find-first-header-group-member* header-props)
;; 		(unless (reorg--find-header-location-within-groups* header)
;; 		  (reorg--insert-header-at-point header))
;; 	      (reorg--insert-header-at-point header t))))

;; (org-indent-refresh-maybe (point-min) (point-max) nil))


(defun reorg--get-all-tree-paths (data leaf-func)
  "get a list of all the paths in a tree.
e.g.:
(reorg--get-all-tree-paths '((1 (2 (- 3 4 5))
				(6 (7 (- 8 9))
				   (10 (- 11)))))
			   (lambda (x) (eq x '-)))
produces:

'((1 2 - 3 4 5)
 (1 6 7 - 8 9)
 (1 6 10 - 11))"
  
  (let (aaa aaaa n)
    (cl-labels ((n-manager (new nn)
			   (if new
			       (push nn n)
			     (let ((val (+ (car n) nn)))
			       (if (= val 0)
				   (progn 
				     (pop n)
				     (when n
				       (setq n 
					     (n-manager nil -1))))
				 (setcar n val))))
			   n)
		(nnn (data)
		     (while data
		       (pcase (pop data)
			 ((and (pred listp)
			       x
			       (guard (funcall leaf-func (car x))))
			  ;; (guard (eq '- (car x))))
			  (mapc (lambda (y) (push y aaa)) x)
			  (push (reverse aaa) aaaa)
			  (setq n (n-manager nil -1))
			  (setq aaa (subseq aaa (- (length aaa) (length n)))))
			 ((and (pred listp)
			       x
			       (guard (null (cdr x))))
			  nil)
			 ((and x (pred listp))
			  (push (car x) aaa)
			  (push (length (cdr x)) n)
			  (nnn (cdr x)))
			 (x (error "someting went wrong"))))))
      (nnn data)
      (reverse aaaa))))

		   ;; (defun tree-path (tree)
		   ;;   (let (path)
		   ;;     (if (and (listp tree) (cdr tree))
		   ;; 	(cl-loop for child in (cdr tree)
		   ;; 		 do (append (list (car tree))
		   ;; 			    (tree-path child)))
		   ;;       (if (listp tree) (car tree)) tree)))


		   ;; (tree-path '(a (b c)))

		   (provide 'reorg-scratch)
