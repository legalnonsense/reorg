;; -*- lexical-binding: t; -*-





(defun xxx-create-test-data ()
  (interactive)
  (cl-loop for x below 1000
	   collect (cl-loop for b in ;; '(a b c d e f g h i j k l m n o p)
			    '(a b c d)
			    collect (cons b (random 100)))))

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
		      :children (( :group (lambda (x) (if (= 0 (% (alist-get 'b x) 2))
							  "B is even"
							"B is odd"))))))))

;; ( :group (lambda (x) (when (evenp (alist-get 'b x))
;; 			     (concat "B: "
;; 				     (number-to-string
;; 				      (alist-get 'b x)))))))))

(defun xxx (data template &optional sorters)
  (cl-loop with results = nil
	   for groups in (plist-get template :children)
	   append (let* ((result (reorg--seq-group-by* (plist-get groups :group)
						       data))
			 (result (if (plist-get groups :sort-group)
				     (seq-sort (plist-get groups :sort-group)
					       result)
				   result))
			 (result (cond ((plist-get groups :children)
					(cl-loop for e in result
						 collect (cons (car e) (xxx (cdr e) groups))))
				       (sorters
					(reorg--multi-sort sorters result))
				       (t result))))
		    result)))

(defun xxx-thread-as-rewrite (data template &optional sorters)
  (cl-loop with results = nil
	   for groups in (plist-get template :children)
	   append (reorg--thread-as
		   data
		   (reorg--seq-group-by* (plist-get groups :group)
					 data)
		   (if (plist-get groups :sort-group)
		       (seq-sort (plist-get groups :sort-group)
				 data)
		     data)
		   (cond ((plist-get groups :children)
			  (cl-loop for e in data
				   collect (cons (car e) (xxx (cdr e) groups))))
			 (sorters
			  (reorg--multi-sort sorters data))
			 (t data)))))


;; (defmacro reorg--thread-as (name &rest form)
;;   `(let*
;;        ,(append `((,name ,(car form))
;; 		  ,@(cl-loop for each in (cdr form)
;; 			     collect `(,name ,each))))
;;      ,name))

;; (let ((x 5))
;;   (reorg--thread-as x (+ 1 3 x) (+ 2   x)))

(defmacro reorg--thread-as (name &rest form)
  "kind of like `-->' but better!?"
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

(reorg--thread-as (s "asdf")
		  (substring s 1 2)
		  ((lambda (x) x) s)
		  (concat s "xxx"))

(let ((sx "xxx"))
  (reorg--thread-as (s sx)
		    (substring s 1 2)
		    (concat s "xxx")))




(cl-defgeneric reorg--seq-group-by* (func sequence)
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

(equal (xxx xxx-data xxx-template) ;;;test
       (xxx-thread-as-rewrite xxx-data xxx-template))

