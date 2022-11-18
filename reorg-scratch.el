;; -*- lexical-binding: t; -*-

(defun xxx-create-test-data ()
  (interactive)
  (cl-loop for x below 1000
	   collect (cl-loop for b in ;; '(a b c d e f g h i j k l m n o p)
			    '(a b c d)
			    collect (cons b (random 10)))))

(setq xxx-data (xxx-create-test-data))

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

;; (reorg--multi-sort '(((lambda (x) (alist-get 'c x)) . >)) yyy)
;; ( :group (lambda (x) (when (= (alist-get 'a x) 6)
;; 			 "A = 6"))
;;   :sort-results (
;; 		   ((lambda (x) (alist-get 'b x)) . >))))))

(defun reorg--group-and-sort* (data
			       template
			       &optional
			       sorters
			       format-string
			       level)
  (setq format-string
	(or format-string
	    (plist-get template :format-string)
	    reorg-headline-format)
	sorters
	(or sorters
	    (plist-get template :sort-results)
	    reorg-default-result-sort))
  
  (cl-loop 
   for groups in (plist-get template :children)
   do (setq format-string (or format-string
			      (plist-get template :format-string)
			      reorg-headline-format)
	    sorters (append sorters (plist-get groups :sort-results))
	    level (or level 1))
   append (reorg--thread-as data
	    (reorg--seq-group-by* (plist-get groups :group)
				  data)
	    (if (plist-get groups :sort-group)
		(seq-sort (plist-get groups :sort-group)
			  data)
	      data)  
	    (cond ((plist-get groups :children)
		   (cl-loop for e in data
			    collect
			    (cons (car e)
				  (reorg--group-and-sort*
				   (cdr e)
				   groups
				   sorters
				   format-string
				   (1+ level)))))
		  (sorters ;; no more headers
		   (cl-loop for each in data
			    collect
			    (cons (car each)
				  (cl-loop for each in (reorg--multi-sort sorters (cdr each))
					   collect (reorg--create-headline-string*
						    each 
						    format-string
						    level)))))
		  ;; format-string
		  ;; level))))
		  (t ;; no more headers; no sort
		   (cl-loop for (h . r) in data
			    collect (cons h
					  (cl-loop for each in r
						   collect
						   (reorg--create-headline-string*
						    each
						    format-string
						    level
						    nil)))))))))

(defun reorg--create-headline-string* (data format-string &optional level overrides)
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
	   (concat (create-stars level) " " (alist-get 'branch-name data))
	   reorg--field-property-name
	   'branch)
	;; TODO:get rid of this copy-tree
	(cl-loop for (prop . val) in overrides
		 do (setf (alist-get prop data)
			  (funcall `(lambda ()
				      (let-alist data 
					,val)))))
	(let ((xxx (reorg--walk-tree format-string
				     #'reorg--turn-dot-to-display-string
				     data)))
	  (funcall `(lambda (data)
		      (concat ,xxx))
		   data)))
      ;; (apply (car xxx)
      ;; 	 (cdr xxx)))
      
      ;; `(lambda (data)
      ;;    (let-alist data 
      ;;      ,format-string))
      ;; data))))
      "\n")
     'reorg-data     
     (append data
	     (list 			;
	      (cons 'reorg-class (alist-get 'class data))
	      (cons 'reorg-level level)))
     reorg--field-property-name
     'leaf
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
     (let* ((key (funcall func elt))
	    (cell (assoc key acc)))
       (if cell
	   (setcdr cell (push elt (cdr cell)))
	 (when key
	   (push (list key elt) acc)))
       acc))
   (seq-reverse sequence)
   nil))

(reorg--group-and-sort* xxx-data xxx-template) ;;;test 
