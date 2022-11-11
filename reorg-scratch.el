;; -*- lexical-binding: t; -*-





(defun xxx-create-test-data ()
  (interactive)
  (cl-loop for x below 10
	   collect (cl-loop for b in '(a b c d e f g h i j k l m n o p)
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
		      :children (( :group (lambda (x) (/= 66 (alist-get 'b x))
					    "EQUALS 66"))))
		    
		    ( :group (lambda (x) (when (evenp (alist-get 'b x))
					   (concat "B: "
						   (number-to-string
						    (alist-get 'b x)))))))))

(defun xxx (data template)
  (cl-loop with results = nil
	   for groups in (plist-get template :children)
	   collect (let* ((result (reorg--seq-group-by* (plist-get groups :group)
							data))
			  (result (if (plist-get groups :sort-group)
				      (seq-sort (plist-get groups :sort-group)
						result)
				    result))
			  (result (if (plist-get groups :children)
				      (cl-loop for e in result
					       collect (xxx (cadr e) groups))
				    result)))
		     result)))



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

(xxx xxx-data xxx-template) ;;;test  

