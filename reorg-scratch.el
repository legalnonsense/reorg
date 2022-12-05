;; -*- lexical-binding: t; -*-

(defvar reorg-default-result-sort nil "")
(defvar reorg--field-property-name 'reorg-field-name "")
(defvar reorg--extra-prop-list nil "")
(defvar reorg--grouper-action-function #'reorg--create-headline-string*
  "")
(setq reorg--grouper-action-function (lambda (data
					      format-string
					      &optional
					      level
					      overrides)
				       (let ((h (reorg--create-headline-string*
						 data
						 format-string
						 level
						 overrides)))
					 (with-current-buffer (get-buffer-create "*REORG*")
					   (insert h))
					 h)))

(defun reorg--multi-sort* (functions-and-predicates sequence)
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

(defun reorg--group-and-sort* (data
			       template
			       &optional
			       action
			       sorters
			       format-string
			       level
			       parent-id
			       parent-header)
  "group and sort and run action on the results"
  (cl-flet ((get-header-props
	     (header groups sss)
	     (reorg--thread-as*
	       (props (list 
		       (cons 'branch-name header)
		       (cons 'headline header)
		       (cons 'reorg-branch t)
		       (cons 'branch-type 'branch)
		       (cons 'grouper-list
			     `(lambda (x)
				(let-alist x
				  ,(plist-get groups :group))))
		       (cons 'branch-predicate
			     `(lambda (x)
				(let-alist x
				  ,(plist-get groups :group))))
		       (cons 'branch-result header)
		       (cons 'grouper-list-results header)
		       (cons 'format-string format-string)
		       (cons 'result-sorters sss)
		       (cons 'template template)
		       (cons 'sort-groups (plist-get groups :sort-groups))
		       (cons 'children (plist-get groups :children))
		       (cons 'branch-sorter ;; heading-sorter
			     nil)
		       (cons 'branch-sort-getter ;; heading-sort-getter
			     nil)
		       (cons 'reorg-level level)))
	       (append props
		       `((group-id . ,(md5 (with-temp-buffer
					     (insert (pp parent-id))
					     (insert (pp parent-header))
					     (insert (pp groups))
					     (buffer-string))))
			 (id . ,(org-id-new)))))))
    (let (pppz)
      ;; inheritance
      (setq format-string
	    (or format-string
		(plist-get template :format-results)
		reorg-headline-format))
      
      ;; sorters
      ;; (or sorters
      ;; 	(plist-get template :sort-results)
      ;; 	reorg-default-result-sort))

      ;; for each child...
      (cl-loop
       with sorterz = sorters
       for groups in (plist-get template :children)
       ;; inheritence for
       do (setq format-string (or (plist-get groups :format-results)
				  format-string
				  reorg-headline-format)
		sorterz (append sorters (plist-get groups :sort-results))
		level (or level 1))
       append (reorg--thread-as* data
		(pcase (plist-get groups :group)
		  ((pred functionp)
		   ;; easy.
		   (reorg--seq-group-by* (plist-get groups :group)
					 data))
		  ((pred stringp)
		   ;; easy. 
		   (list (cons (plist-get groups :group)
			       data)))
		  (_
		   ;; assume dot and at-dot notation.
		   ;; not so easy.
		   (when-let ((at-dots (seq-uniq 
					(reorg--at-dot-search* (plist-get groups :group)))))
		     
		     ;; if it has an at-dot, then make a copy of each entry in DATA
		     ;; if its value is a list.
		     ;; For example, if the grouping form was:
		     ;;
		     ;; (if (evenp .@b)
		     ;;     "B is even"
		     ;;   "B is odd")
		     ;;
		     ;; and an entry in DATA had the value:
		     ;; 
		     ;; '((a . 1) (b . 2 3 4))
		     ;;
		     ;; Then, being being processed by the parser,
		     ;; the entry is replaced by three separate entries:
		     ;; 
		     ;; '((a . 1) (b . 2))
		     ;; '((a . 1) (b . 3))
		     ;; '((a . 1) (b . 4))

		     (setq
		      data
		      (cl-loop
		       for d in data 
		       append
		       (cl-loop
			for at-dot in at-dots
			if (listp (alist-get at-dot d))
			return (cl-loop for x in (alist-get at-dot d)
					collect (let ((ppp (copy-alist d)))
						  (setf (alist-get at-dot ppp) x)
						  ppp))
			finally return data))))
		   (reorg--seq-group-by*
		    ;; convert any at-dots to dots (yes, this could be part
		    ;; of the conditional above, but I wanted to avoid it for
		    ;; some reason. 
		    (reorg--walk-tree* (plist-get groups :group)
				       #'reorg--turn-at-dot-to-dot
				       data)
		    data)))
		;; If there is a group sorter, sort the headers
		(if (plist-get groups :sort-groups)
		    (seq-sort-by #'car 
				 (plist-get groups :sort-groups)
				 data)
		  data)
		;; If there are children, recurse 
		(if (plist-get groups :children)
		    (cl-loop
		     for (header . results) in data
		     collect
		     (cons
		      (funcall
		       (or action
			   reorg--grouper-action-function)
		       (setq pppz 
			     (get-header-props header groups sorterz))
		       nil
		       level)
		      (reorg--group-and-sort*			  
		       results
		       groups
		       action
		       sorterz
		       format-string
		       (1+ level)
		       header)))
		  ;; if there aren't children,
		  ;; sort the results if necessary
		  ;; then convert each batch of results
		  ;; into to headline strings
		  (cl-loop for (header . results) in data
			   collect
			   (cons				
			    (funcall
			     (or action
				 reorg--grouper-action-function)
			     (setq pppz 
				   (get-header-props header groups sorterz))
			     nil
			     level)
			    (cl-loop
			     with
			     results = 
			     (if sorterz
				 (reorg--multi-sort* sorterz
						     results)
			       results)
			     for result in results
			     collect
			     (funcall
			      (or action
				  reorg--grouper-action-function)
			      (append result
				      (list 
				       (cons 'group-id
					     (alist-get 'id pppz))))
			      format-string
			      (1+ level)))))))))))

(defun reorg--create-headline-string* (data
				       format-string
				       &optional
				       level
				       overrides)
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
		      (funcall `(lambda ()
				  (let-alist data 
				    ,val)))))
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
       (append data
	       (list
		(cons 'reorg-headline
		      headline-text)
		(cons 'reorg-class
		      (alist-get 'class data))
		(cons 'reorg-field-type
		      (if (alist-get
			   'reorg-branch data)
			  'branch 'leaf))))       
       reorg--field-property-name ;; property2
       (if (alist-get 'reorg-branch data)
	   'branch 'leaf)
       (alist-get (alist-get 'class data) ;; extra props 
		  reorg--extra-prop-list)))))

(defmacro reorg--thread-as* (name &rest form)
  "I didn't know `-as->' existed..."
  (declare (indent defun))  
  (if (listp name)
      (append 
       `(let* ,(append `((,(car name) ,(cadr name)))
		       (cl-loop for each in form
				collect `(,(car name) ,each))))
       (list (car name)))
    (append `(let*
		 ,(append `((,name ,(car form)))
			  (cl-loop for each in (cdr form)
				   collect `(,name ,each))))
	    (list name))))

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
	(form)
	(cl-loop for each in form
		 if (listp each)
		 collect (walk each)
		 else
		 collect (if data
			     (funcall func each data)
			   (funcall func each)))))
    (if (listp form)
	(walk form)
      (if data 
	  (funcall func form data)
	(funcall func form)))))

(defun reorg--at-dot-search* (data)
  "Return alist of symbols inside DATA that start with a `.@'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'.

See `let-alist--deep-dot-search'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\.@" name)
	;; Return the cons cell inside a list, so it can be appended
	;; with other results in the clause below.
	(list (cons data (intern (replace-match "" nil nil name)))))))
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
				 unless (equal (funcall func leaf-data)
					       (funcall func (reorg--get-view-prop)))
				 return (funcall pred
						 (funcall func leaf-data)
						 (funcall func (reorg--get-view-prop))))
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
  (goto-char (point-min))
  (reorg--map-id (alist-get 'id data)
		 (reorg-views--delete-leaf))
  (cl-loop with header-groups = (reorg--group-and-sort*
				 (list data)
				 template
				 #'reorg--create-headline-string*)   
	   for headers in header-groups
	   do (goto-char (point-min))
 	   collect 
	   (cl-loop
	    with headers = (-flatten headers)    
	    with leaf = (car (last headers))
	    with leaf-props = (get-text-property 0 'reorg-data leaf)
	    for header in (butlast headers)
	    ;; headers is alist of the parent headers in order 
	    do (let* ((header-props (get-text-property 0 'reorg-data header))
		      (group-id (alist-get 'group-id header-props))
		      (id (alist-get 'id header-props)))
		 (unless (or (reorg--goto-id header-props)
			     (equal id (reorg--get-view-prop 'id)))		   
		   (if (reorg--find-first-header-group-member* header-props)
		       (unless (reorg--find-header-location-within-groups* header)
			 (reorg--insert-header-at-point header))
		     (reorg--insert-header-at-point header t))))
	    finally (progn (reorg--find-leaf-location* leaf)
			   (reorg--insert-header-at-point leaf)))))

(defun xxx-create-test-data ()
  (interactive)
  (cl-loop
   for x below 100
   collect
   (cl-loop
    for b in ;; '(a b c d e f g h i j k l m n o p)
    '(a b c d)
    collect (cons b (random 10)) into x
    finally return (append x (list (cons 'id (org-id-new))
				   (cons 'class 'org))))))

(setq xxx-data (xxx-create-test-data))

(setq xxx-template
      '(	
	:children
	(( :group (lambda (x) (when (oddp (alist-get 'a x))
				(concat "A: "
					(number-to-string 
					 (alist-get 'a x)))))
	   :format-results (.stars (format " (%d %d %d %d)" .a .b .c .d))
	   :sort-groups (lambda (a b)
			  (string<
			   a
			   b))
	   :sort-results (((lambda (x) (alist-get 'd x)) . >))
	   :children (( :sort-results (((lambda (x) (alist-get 'c x)) . >))
			:group (lambda (x) (if (= 0 (% (alist-get 'b x) 2))
					       "B is even"
					     "B is odd")))))
	 ( :group (lambda (x) (when (= (alist-get 'b x) 5)
				"B IS FIVE"))
	   :sort-results (((lambda (x) (alist-get 'a x)) . <))
	   :format-results (.stars " " (format "a is %d, b is %d"
					       .a
					       .b))))))

(defun reorg--run-new-test ()
  "test"
  (interactive)
  (with-current-buffer (get-buffer-create "*REORG*")
    (erase-buffer)
    (reorg--group-and-sort* xxx-data xxx-template)
    (goto-char (point-min))
    (reorg-view-mode)
    (olivetti-mode)    
    (setq cursor-type 'box)
    (reorg-dynamic-bullets-mode)
    (org-visual-indent-mode))
  (tab-bar-new-tab)
  (unless (tab-bar-switch-to-tab "*REORG*")
    (tab-bar-switch-to-next-tab)
    (set-window-buffer nil "*REORG*")))

(defun reorg--insertion-test (data)
  (interactive)
  (reorg--insert-new-heading* data xxx-template))

(setq xxx (reorg--group-and-sort* (list '((a . 7) (b . 5) (c . 3) (d . 4) (id . "1234")))
				  xxx-template)
      xxx (-flatten (cl-loop for headers in xxx
			     collect (cl-loop for header in (-flatten headers)
					      collect headers)))
      xxx (car (last xxx)))


