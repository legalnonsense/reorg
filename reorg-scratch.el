;; -*- lexical-binding: t; -*-


(defvar reorg--grouper-action-function #'reorg--create-headline-string*
  "")

(defun reorg--insert-heading* (data template)
  "insert an individual heading"
  (reorg--thread-as data
		    (reorg--group-and-sort* (list data) template)
		    (reorg--flatten* data)
		    (reverse data)
		    (cdr data)))

(reorg--insert-heading* '((a . 1) (b . 2)) xxx-template)

(defun reorg--flatten* (data)
  (cl-labels ((walk (tree)
		    (cl-loop for each in tree
			     if (listp each)
			     collect (car each)
			     and collect (walk (cdr each))
			     else
			     collect each)))
    (-flatten (walk data))))

(defun xxx-create-test-data ()
  (interactive)
  (cl-loop for x below 1000
	   collect (cl-loop for b in ;; '(a b c d e f g h i j k l m n o p)
			    '(a b c d)
			    collect (cons b (random 10)))))

(setq xxx-data (xxx-create-test-data))

(setq xxx-template '( :format-string (format "xxx %d" .b)
		      :sort-results (((lambda (x) (alist-get 'd x)) . <))
		      :children
		      (( :group (number-to-string .a)
			 :children (( :group (if (evenp .b)
						 "B is even"
					       "B is odd")))))))



(setq xxx-template
      '( :format-string (format "a is %d but b is %d" .a .b)
	 :children
	 (( :group (lambda (x) (when (oddp (alist-get 'a x))
				 (concat "A: "
					 (number-to-string 
					  (alist-get 'a x)))))
	    :sort-group (lambda (a b)
			  (string>
			   (car a)
			   (car b)))
	    :sort-results (((lambda (x) (alist-get 'd x)) . >))
	    :children (( :sort-results (((lambda (x) (alist-get 'c x)) . >))
			 :group (lambda (x) (if (= 0 (% (alist-get 'b x) 2))
						"B is even"
					      "B is odd"))))))))


(defun reorg--group-and-sort* (data
			       template
			       &optional
			       sorters
			       format-string
			       level)
  (cl-flet ((get-header-props
	     (header groups)
	     (reorg--thread-as (props (list 
				       (cons 'branch-name header)
				       (cons 'headline header)
				       (cons 'reorg-branch t)
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
				       (cons 'children (plist-get groups :children))
				       (cons 'branch-sorter ;; heading-sorter
					     nil)
				       (cons 'branch-sort-getter ;; heading-sort-getter
					     nil)
				       (cons 'reorg-level level)))
	       (append props
		       `((group-id . ,(md5 (with-temp-buffer
					     (insert (pp template))
					     (buffer-string))))
			 (id . ,(md5 (with-temp-buffer
				       (insert (pp props))
				       (buffer-string)))))))))
    ;; inheritance
    (setq format-string
	  (or format-string
	      (plist-get template :format-string)
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
				(plist-get template :format-string)
				reorg-headline-format)
	      sorters (append sorters (plist-get groups :sort-results))
	      level (or level 1))
     append (reorg--thread-as data
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
		   ;;		 
		   (setq data
			 (cl-loop
			  for d in data 
			  append (cl-loop
				  for at-dot in at-dots
				  if (listp (alist-get at-dot d))
				  return (cl-loop for x in (alist-get at-dot d)
						  collect (let ((ppp (copy-alist d)))
							    (setf (alist-get at-dot ppp) x)
							    ppp))
				  finally return data))))
		 (reorg--seq-group-by*
		  ;; convert any at-dots to dots 
		  (reorg--walk-tree* (plist-get groups :group)
				     #'reorg--turn-at-dot-to-dot
				     data)
		  data)))
	      ;; If there is a group sorter, sort the headers
	      (if (plist-get groups :sort-group)
		  (seq-sort (plist-get groups :sort-group)
			    data)
		data)
	      ;; If there are children, recurse 
	      (if (plist-get groups :children)
		  (cl-loop
		   for (header . results) in data
		   collect
		   (cons (funcall
			  reorg--grouper-action-function
			  (get-header-props header groups)
			  nil
			  level
			  ;; overrides
			  )
			 (reorg--group-and-sort*
			  results
			  groups
			  sorters
			  format-string
			  (1+ level))))
		;; if there aren't children,
		;; sort the results if necessary
		;; then convert each batch of results
		;; into to headline strings
		(cl-loop for (header . results) in data
			 collect
			 (cons				
			  (funcall
			   reorg--grouper-action-function
			   (get-header-props header groups)
			   nil
			   level
			   nil)
			  (cl-loop with results = (if sorters
						      (reorg--multi-sort sorters
									 results)
						    results)
				   for result in results
				   collect
				   (funcall
				    reorg--grouper-action-function
				    result
				    format-string
				    level
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
	   (funcall `(lambda (data)
		       (concat ,xxx))
		    data))
	 "\n")))         
     'reorg-data     
     (append data
	     (list 			;
	      (cons 'reorg-class (alist-get 'class data))))
     reorg--field-property-name
     (if (alist-get 'reorg-branch data)
	 'branch 'leaf)
     (alist-get (alist-get 'class data)
		reorg--extra-prop-list))))

(defmacro reorg--thread-as (name &rest form)
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
  (cl-labels ((walk (form)
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

;; TESTS
(let ((reorg--grouper-action-function (lambda (x &rest y) (identity x))))
  (reorg--group-and-sort* xxx-data xxx-template))

(reorg--group-and-sort* xxx-data xxx-template) ;;;test 
