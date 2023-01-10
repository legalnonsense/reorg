;; -*- lexical-binding: t; -*-


;;     ﯍    

(defun reorg--get-longest-line-length ()
  "get longest line length"
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
	     collect (reorg--line-length) into lengths
	     do (forward-line)
	     finally return (apply #'max lengths))))

(defun reorg--line-length ()
  "get the line length including align-to"
  (interactive)
  (save-excursion 
    (goto-char (line-beginning-position))
    (let ((point (point))
	  (start (point))
	  (length 0)
	  found)
      (cl-loop while (and (setq point (next-single-property-change
				       (point)
				       'display
				       nil
				       (line-end-position)))
			  (/= (line-end-position) point))
	       do (progn 
		    (setq found t)
		    (let ((l (- point start)))
		      (if-let* ((display (get-char-property point 'display))
				(align-to (plist-get (cdr display) :align-to)))
			  (if (< l align-to)
			      (progn 
				(cl-incf length align-to)
				(setq start point)
				(setf (point) point))
			    (cl-incf length l)
			    (setf start point)
			    (setf (point) point))
			(setf start point)
			(setf (point) point)
			(cl-incf length l))))
	       finally (cl-incf length (- (line-end-position) (point))))
      (unless found 
	(cl-incf length (- (line-end-position)
			   (line-beginning-position))))
      length)))

(defun reorg--sort-by-list (a b seq &optional predicate list-predicate)
  "Provide a sequence SEQ and return the earlier of A or B."
  (let ((a-loc (seq-position seq a (or list-predicate #'equal)))
	(b-loc (seq-position seq b (or list-predicate #'equal))))
    (cond
     ((and (null a-loc) (null b-loc)) nil)
     ((null a-loc) nil)
     ((null b-loc) t)
     (t (funcall (or predicate #'<) a-loc b-loc)))))

(defun reorg--turn-at-dot-to-dot (elem &rest _ignore)
  "turn .@symbol into .symbol."
  (if (and (symbolp elem)
	   (string-match "\\`\\.@" (symbol-name elem)))
      (intern (concat "." (substring (symbol-name elem) 2)))
    elem))

(defmacro reorg--create-string-comparison-funcs ()
  "string<, etc., while ignoring case."
  `(progn 
     ,@(cl-loop for each in '("<" ">" "=" )
		collect `(defun ,(intern (concat "reorg-string" each)) (a b)
			   ,(concat "like string" each " but ignore case")
			   (,(intern (concat "string" each))
			    (if a (downcase a) "")
			    (if b (downcase b) ""))))))

(reorg--create-string-comparison-funcs)

(defun reorg-views--delete-leaf ()
  "delete the heading at point"
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

(defmacro reorg--map-id (id &rest body)
  "Execute BODY at each entry that matches ID."
  `(org-with-wide-buffer 
    (goto-char (point-min))
    (let ((id ,id))
      (while (reorg--goto-next-prop 'id id)
	,@body))))

(defun reorg--map-all-branches (func)
  "map all"
  (save-excursion 
    (goto-char (point-min))
    (while (reorg--goto-next-branch)
      (funcall func))))

(defun reorg--delete-headers-maybe ()
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

(defun reorg--multi-sort (functions-and-predicates sequence)
  "FUNCTIONS-AND-PREDICATES is an alist of functions and predicates.

FUNCTIONS should not be functions.  Use a form that can contain dotted symbols
as used by `let-alist'."
  (seq-sort 
   (lambda (a b)
     (cl-loop for (form . pred) in functions-and-predicates	      
	      unless (equal (funcall `(lambda (a) (let-alist a ,form)) a)
			    (funcall `(lambda (b) (let-alist b ,form)) b))
	      return (funcall pred
			      (funcall `(lambda (a) (let-alist a ,form)) a)
			      (funcall `(lambda (b) (let-alist b ,form)) b))))
   sequence))



(defun reorg--get-group-and-sort (data
				  template
				  level
				  ignore-sources
				  &rest
				  inherited-props)
  "Fetching, grouping, and sorting function to prepare
data to be inserted into buffer."
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
			(pp-to-string (plist-get inherited-props :parent-template))
			(pp-to-string (plist-get inherited-props :header)))))
		(cons 'id id)))))
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
				  :parent-id nil
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
			   (list :header header
				 :parent-template template
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

(defun reorg--create-headline-string (data
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
	       (let* ((new (reorg--walk-tree
			    format-string
			    #'reorg--turn-dot-to-display-string
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
				  (cons 'parent-id
					(alist-get 'parent-id data))
				  (cons 'reorg-field-type
					(if (alist-get
					     'reorg-branch data)
					    'branch 'leaf)))))
	      (cl-loop for (prop . val) in post-overrides
		       do (setf (alist-get prop data)
				(alist-get prop post-overrides)))
	      data)
       reorg--field-property-name ;; property2
       (if (alist-get 'reorg-branch data)
	   'branch 'leaf)
       (alist-get (alist-get 'class data) ;; extra props 
		  reorg--extra-prop-list)))))

(defun reorg--seq-group-by (func sequence)
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

(defun reorg--walk-tree (tree func &optional data)
  "apply func to each element of tree and return the results" 
  (cl-labels
      ((walk
	(tree d)
	(cl-loop for each in tree
		 if (listp each)
		 collect (walk each d)
		 else
		 collect (if d
			     (funcall func each d)
			   (funcall func each)))))
    (if (listp tree)
	(walk tree data)
      (if data 
	  (funcall func tree data)
	(funcall func tree)))))

(defun reorg--code-search (func code)
  "Return alist of symbols inside CODE that match REGEXP.
See `let-alist--deep-dot-search'."
  (let (acc)
    (cl-labels ((walk (code)
		      (cond ((symbolp code)
			     (when (funcall func code)
			       (push code acc)))
			    ((listp code)
			     (walk (car code))
			     (walk (cdr code) )))))
      (walk code)
      (cl-delete-duplicates acc))))

(defun reorg--at-dot-search (data)
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
    (apply #'nconc (mapcar #'reorg--at-dot-search data)))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (reorg--at-dot-search (cadr data)))
   (t (append (reorg--at-dot-search (car data))
	      (reorg--at-dot-search (cdr data))))))

(defun reorg--turn-dot-to-display-string (elem data)
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

(defun reorg--goto-next-sibling-same-group (&optional data)
  "goot next sibing same group"
  (let ((id (or
	     (and data (alist-get 'group-id data))
	     (reorg--get-view-prop 'group-id))))
    (reorg--goto-next-prop 'group-id id)))

(defun reorg--goto-next-leaf-sibling ()
  "goto next sibling"
  (reorg--goto-next-prop 'reorg-field-type
			 'leaf
			 (reorg--get-next-parent)))

;;TODO move these into `reorg--create-navigation-commands'
(defun reorg--goto-first-leaf ()
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
  (save-excursion 
    (insert header-string))
  (reorg-dynamic-bullets--fontify-heading)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--find-header-location-within-groups (header-string)
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
		 while (reorg--goto-next-sibling-same-group
			(get-text-property 0 'reorg-data header-string))
		 finally return (progn (goto-char point)
				       nil))
      (cl-loop with point = (point)
	       when (equal .branch-name
			   (reorg--get-view-prop 'branch-name))
	       return t
	       while (reorg--goto-next-sibling-same-group
		      (get-text-property 0 'reorg-data header-string))
	       finally return (progn (goto-char point)
				     nil)))))

(defun reorg--find-first-header-group-member (header-data)
  "goto the first header that matches the group-id of header-data"
  (let ((point (point)))
    (if (equal (reorg--get-view-prop 'group-id)
	       (alist-get 'group-id header-data))
	(point)
      (if (reorg--goto-next-prop 'group-id
				 (alist-get 'group-id header-data)
				 (or (reorg--get-next-parent)
				     (point-max)))
	  (point)
	(goto-char point)
	nil))))

(defun reorg--find-leaf-location (leaf-string &optional result-sorters)
  "find the location for LEAF-DATA among the current leaves. put the
point where the leaf should be inserted (ie, insert before)"
  ;; goto the first leaf if at a branch 
  (unless (eq 'leaf (reorg--get-view-prop 'reorg-field-type))
    (if (reorg--goto-first-leaf)
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
		     while (reorg--goto-next-leaf-sibling)
		     finally (goto-char (line-beginning-position 2)))))
      (reorg--goto-next-heading))))

(defun reorg--get-next-group-id-change ()
  "get next group id change"
  (reorg--get-next-prop 'group-id
			(reorg--get-view-prop)
			nil
			(lambda (a b)
			  (not (equal a b)))))

(defun reorg--update-heading-at-point ()
  "update the current heading"
  (interactive)
  (reorg--insert-new-heading
   (reorg--with-point-at-orig-entry nil
				    nil
				    (reorg--parser
				     nil
				     (reorg--get-view-prop 'class)))
   reorg--current-template))

(defun reorg--insert-new-heading (data template)
  "insert an individual heading"
  (save-excursion 
    (goto-char (point-min))
    (reorg--map-id (alist-get 'id data)
		   (let ((parent (reorg--get-parent)))
		     (reorg-views--delete-leaf)
		     (when parent
		       (goto-char parent)
		       (reorg--delete-headers-maybe))))
    (cl-loop with header-groups = (reorg--get-all-tree-paths
				   (reorg--get-group-and-sort
				    (list data) template 1 t)
				   (lambda (x)
				     (and (listp x)
					  (stringp (car x))
					  (eq
					   'leaf
					   (get-text-property
					    0
					    'reorg-field-type
					    (car x))))))
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
		     (if (reorg--find-first-header-group-member header-props)
			 (unless (reorg--find-header-location-within-groups header)
			   (reorg--insert-header-at-point header))
		       (reorg--insert-header-at-point header t))))
	      finally (progn (setq point (point))
			     (when (eq 'leaf (alist-get 'reorg-field-type leaf-props))
			       (reorg--find-leaf-location leaf)
			       (reorg--insert-header-at-point leaf))
			     (goto-char point))))
    (org-indent-refresh-maybe (point-min) (point-max) nil))
  (run-hooks 'reorg--navigation-hook))

(defun reorg--get-all-tree-paths (tree leaf-func)
  "Get a list of all paths in tree.
LEAF-FUNC is a function run on each member to determine
whether it terminates the branch.

For example:

(reorg--get-all-tree-paths '((1 (2 (- 3 4 5))
				(6 (7 (- 8 9))
				   (10 (- 11)
                                        (- 12)))))
			   (lambda (x) (eq '- (car x))))
returns:

((1 2 - 3 4 5)
 (1 6 7 - 8 9)
 (1 6 10 - 11)
 (1 6 10 - 12))
"  
  (let (paths)
    (cl-labels ((doloop (tree &optional path)
			(cond ((funcall leaf-func tree)
			       (push (append (reverse path) tree) paths))
			      ((or (stringp (car tree))
				   (numberp (car tree))
				   (symbolp (car tree)))
			       (push (car tree) path)
			       (cl-loop for child in (cdr tree)
					do (doloop child path)))
			      (tree (cl-loop for child in tree
					     do (doloop child path))))))
      (doloop tree)
      (reverse paths))))

(cl-defmacro reorg-create-data-type (&optional ;
				     &key
				     class
				     name
				     parse
				     disable
				     display
				     append)
  ;; TODO add disabled key to remove data type
  ;; from the parser list 
  ""
  (let* ((parsing-func (reorg--create-symbol 'reorg--
					     class
					     '--parse-
					     name))
	 (display-func (reorg--create-symbol 'reorg--
					     class
					     '--display-
					     name)))
    `(progn
       (cond ((not ,disable)
	      (defun ,parsing-func (&optional data DATA)
		(let-alist DATA 
		  ,parse))
	      (setf (alist-get ',class reorg--parser-list)
		    (assoc-delete-all ',name
				      (alist-get
				       ',class
				       reorg--parser-list)))
	      (if ',append		     
		  (setf (alist-get ',class reorg--parser-list)
			(append (alist-get ',class reorg--parser-list)
				(list 
				 (cons ',name #',parsing-func))))
		(cl-pushnew (cons ',name #',parsing-func)
			    (alist-get ',class reorg--parser-list)))
	      (if ',display 
		  (defun ,display-func (alist)
		    ,display)
		(fmakunbound ',display-func)))
	     (t ;;if disabled 
	      (setf (alist-get ',class reorg--parser-list)
		    (assoc-delete-all ',name
				      (alist-get ',class reorg--parser-list)))
	      (fmakunbound ',display-func)
	      (fmakunbound ',parsing-func))))))




(provide 'reorg-scratch)

