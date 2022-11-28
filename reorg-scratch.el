:PROPERTIES:
:ID:       7d86d5de-0e44-4ff9-9b2f-b2b76c7d31df
:END:
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

(defun xxx-create-test-data ()
  (interactive)
  (cl-loop for x below 20
	   collect (cl-loop for b in ;; '(a b c d e f g h i j k l m n o p)
			    '(a b c d)
			    collect (cons b (random 10)) into x
			    finally return (append x (list (cons 'id (org-id-get-create t)))))))

(setq xxx-data (xxx-create-test-data))

(setq xxx-template
      '(
	:format-results (.stars " " (pp (list .a .b .c .d)))
	:children
	(( :group (lambda (x) (when (oddp (alist-get 'a x))
				(concat "A: "
					(number-to-string 
					 (alist-get 'a x)))))
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
				"B IS FIVE"))))))

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
	     (header groups)
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
		       (cons 'result-sorters sorters)
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
			 (id . ,(md5 (with-temp-buffer
				       (insert 
					       (pp (plist-get groups :group)))
				       (buffer-string)))))))))
	       ;; inheritance
    (setq format-string
	  (or format-string
	      (plist-get template :format-results)
	      reorg-headline-format)
	  sorters 
	  (or sorters
	      (plist-get template :sort-results)
	      reorg-default-result-sort))

    ;; for each child...
    (cl-loop 
     for groups in (plist-get template :children)
     ;; inheritence for 
     do (setq format-string (or format-string
				(plist-get template :format-results)
				reorg-headline-format)
	      sorters (append sorters (plist-get groups :sort-results))
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
		    ;; format the header since it won't be in the recursed
		    ;; data 
		    (funcall
		     (or action
			 reorg--grouper-action-function)
		     (get-header-props header groups)
		     nil
		     level
		     ;; overrides
		     )
		    (reorg--group-and-sort*			  
		     results
		     groups
		     action
		     sorters
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
			   (get-header-props header groups)
			   nil
			   level
			   nil)
			  (cl-loop with results = (if sorters
						      (reorg--multi-sort* sorters
									  results)
						    results)
				   for result in results
				   collect
				   (funcall
				    (or action
					reorg--grouper-action-function)
				    result
				    format-string
				    (1+ level)
				    ;;OVERRIDES
				    )))))))))
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
    (push (cons 'reorg-level level) data)
    (let (headline-text)
      (apply
       #'propertize 
       (concat
	(if (alist-get 'reorg-branch data)
	    (propertize 
	     (concat (create-stars level) " " (alist-get 'branch-name data) "\n")
	     reorg--field-property-name
	     'branch)
	  ;; TODO:get rid of this copy-tree
	  (cl-loop for (prop . val) in overrides
		   do (setf (alist-get prop data)
			    (funcall `(lambda ()
					(let-alist data 
					  ,val)))))
	  (concat 
	   (let ((xxx (reorg--walk-tree* format-string
					 #'reorg--turn-dot-to-display-string*
					 data)))
	     (setq headline-text 
		   (funcall `(lambda (data)
			       (concat ,@xxx))
			    data)))))) 
       'reorg-data     
       (append data
	       (list
		(cons 'reorg-headline headline-text)
		(cons 'reorg-class (alist-get 'class data))
		(cons 'reorg-field-type (if (alist-get 'reorg-branch data)
					    'branch 'leaf))))
       reorg--field-property-name
       (if (alist-get 'reorg-branch data)
	   'branch 'leaf)
       (alist-get (alist-get 'class data)
		  reorg--extra-prop-list)))))

(defmacro reorg--thread-as* (name &rest form)
  "like `-as->' but better!?"
  (declare (indent defun)
	   (debug t))
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
			 (reorg--get-next-sibling)))

(defun reorg--goto-id (header-string)
  "goto ID that matches the header string"
  (reorg--goto-next-prop
   'id
   (alist-get 'id
	      (get-text-property 0
				 'reorg-data
				 header-string))))

(defun reorg--delete-header-at-point ()
  "delete the header at point"
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

(defun reorg--insert-header-at-point (header-string)
  "insert header at point"
  (goto-char (point-at-bol))
  (let ((point (point)))
    (insert header-string)
    (goto-char point)
    (run-hooks 'reorg--navigation-hook)))

(defun reorg--find-header-location-within-groups* (header-string)
  "assume the point is on the first header in the group"
  (let-alist (get-text-property 0 'reorg-data header-string)
    (when .sort-groups
      (cl-loop with point = (point)
	       when (or (equal .branch-name
			       (reorg--get-view-prop 'branch-name))
			(funcall .sort-groups
				 .branch-name
				 (reorg--get-view-prop 'branch-name)))
	       return (point)
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

(defun reorg--find-leaf-location* (leaf-data &optional result-sorters)
  "assume the point is on the first leaf in the group"
  (unless (eq 'leaf (reorg--get-view-prop 'branch-type ))
    (reorg--goto-first-leaf*))
  (when-let ((result-sorters (or result-sorters
				 (save-excursion 
				   (reorg--goto-parent)
				   (reorg--get-view-prop 'result-sorters)))))
    (let ((leaf-data (get-text-property 0 'reorg-data leaf-data)))
      (cl-loop with point = (point)
	       when (cl-loop for (func . pred) in result-sorters
			     unless (equal (funcall func leaf-data)
					   (funcall func (reorg--get-view-prop)))
			     return (funcall pred
					     (funcall func leaf-data)
					     (funcall func (reorg--get-view-prop))))
	       return (point)
	       while (reorg--goto-next-sibling-same-group*)
	       finally return (progn (goto-char point)
				     nil)))))

(defun reorg--get-next-group-id-change ()
  "get next group id change"
  (reorg--get-next-prop 'group-id
			(reorg--get-view-prop)
			nil
			(lambda (a b)
			  (not (equal a b)))))


(defun reorg--insert-heading* (data template)
  "insert an individual heading"
  (reorg--map-id (alist-get 'id data)
		 (reorg-views--delete-leaf))
  (cl-loop with header-groups = (reorg--group-and-sort*
				 (list data)
				 template
				 #'reorg--create-headline-string*)   
	   for headers in header-groups
 	   collect 
	   (cl-loop with headers = (-flatten headers)    
		    with leaf = (car (last headers))
		    with leaf-props = (get-text-property 0 'reorg-data leaf)
		    for header in (butlast headers)
		    ;; headers is alist of the parent headers in order 
		    do (let* ((header-props (get-text-property 0 'reorg-data header))
			      (group-id (alist-get 'group-id header-props))
			      (id (alist-get 'id header-props)))
			 ;; (debug nil (reorg--get-view-prop 'id)
			 ;; 	id
			 ;; 	"group id:"
			 ;; 	(reorg--get-view-prop 'group-id)
			 ;; 	group-id)
			 (unless (or (equal id (reorg--get-view-prop 'id))
				     (reorg--goto-id id))
			   (reorg--find-first-header-group-member* header-props)
			   (reorg--find-header-location-within-groups* header)))
		    finally (progn (reorg--find-leaf-location* leaf)
				   (reorg--insert-header-at-point leaf)))))

(defun reorg--run-new-test ()
  "test"
  (interactive)
  (with-current-buffer (get-buffer-create "*REORG*")
    (erase-buffer)
    (reorg--group-and-sort* xxx-data xxx-template)))

(defun reorg--insertion-test ()
  (interactive)
  (reorg--insert-heading* '((a . 7)(b . 5) (c . 3) (d . 4) (id . "1234")) xxx-template))

(setq xxx (reorg--group-and-sort* (list '((a . 7) (b . 5) (c . 3) (d . 4) (id . "1234"))) xxx-template))
(cl-loop for headers in xxx
	 collect (cl-loop for header in (-flatten headers)
			  collect headers))
xxx
