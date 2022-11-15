;; -*- lexical-binding: t; -*-

(defun xxx-create-test-data ()
  (interactive)
  (cl-loop for x below 1000
	   collect (cl-loop for b in ;; '(a b c d e f g h i j k l m n o p)
			    '(a b c d)
			    collect (cons b (random 10)))))

(setq xxx-data (xxx-create-test-data))

(setq xxx-template
      '( :children (( :group (lambda (x) (when (oddp (alist-get 'a x))
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
							"B is odd")))))
		    ( :group (lambda (x) (when (= (alist-get 'a x) 6)
					   "A = 6"))
		      :sort-results (
				     ((lambda (x) (alist-get 'b x)) . >))))))

(defcustom reorg-default-result-sort nil "")

(defun xxx-thread-as-rewrite (data
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
				  (xxx-thread-as-rewrite
				   (cdr e)
				   groups
				   sorters
				   format-string
				   (1+ level)))))
		  (sorters ;; no more headers
		   (cl-loop for each in data
			    collect
			    (cons (car each)
				  (reorg--multi-sort sorters (cdr each)))))
		  (t ;; no more headers; no sort
		   (cl-loop for (header . rest) in data
			    collect (cons header rest)))))))

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

(xxx-thread-as-rewrite xxx-data xxx-template) ;;;test 
