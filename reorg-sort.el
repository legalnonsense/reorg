;; -*- lexical-binding: t; -*-

(defun reorg--multi-sort (functions-and-predicates sequence)
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

(cl-defgeneric reorg--seq-group-by (form sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall `(lambda (e)
			     (let-alist e ,form))
			  elt))
	    (cell (assoc key acc)))
       (if cell
	   (setcdr cell (push elt (cdr cell)))
	 (push (list key elt) acc))
       acc))
   (seq-reverse sequence)
   nil))

(defun reorg--dot-at-search (data)
  "Return alist of symbols inside DATA that start with a `.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\.@" name)
        ;; Return the cons cell inside a list, so it can be appended
        ;; with other results in the clause below.
        (list (cons data (intern (replace-match "" nil nil name)))))))
   ((vectorp data)
    (apply #'nconc (mapcar #'reorg--dot-at-search data)))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (reorg--dot-at-search (cadr data)))
   (t (append (reorg--dot-at-search (car data))
              (reorg--dot-at-search (cdr data))))))

(defun reorg--turn-at-dot-to-dot (elem)
  "turn a .@symbol into .symbol
the value from the parsed data or calling the display
function created by the type creation macro."
  (if (and (symbolp elem)
	   (string-match "\\`\\.@" (symbol-name elem)))
      (intern (concat "." (substring (symbol-name elem) 2)))
    elem))

(cl-defun reorg--group-and-sort (list template &optional (n 0 np))
  "Group RESULTS according to TEMPLATE."
  (let ((copy (copy-tree list)))
    (cl-labels
	((doloop
	  (data
	   template
	   &optional (n 0 np)
	   result-sorters
	   grouper-list
	   grouper-list-results
	   format-string
	   (level 1))
	  (let ((grouper
		 (plist-get template :group))
		(children
		 (plist-get template :children))
		(class
		 (plist-get template :class))
		(heading-sorter
		 (plist-get template :sort))
		(heading-sort-getter
		 (or (plist-get template :sort-getter)
		     #'car))
		(format-string
		 (or (plist-get template :format-string)
		     format-string
		     reorg-headline-format))
		(result-sort
		 (plist-get template :sort-results)))
	    (when result-sort
	      (setq result-sorters
		    (append result-sorters					  
			    (cl-loop for (form . pred) in result-sort
				     collect (cons `(lambda (x)
						      (let-alist x
							,form))
						   pred)))))
	    (unless np
	      (let ((old (cl-copy-list data)))
		(setcar data '_)
		(setcdr data (list old))))
	    (setf
	     (nth n (cdr data))
	     (--> (nth n (cdr data))
		  (cond ((functionp grouper)
			 (->> it
			      (seq-group-by grouper)
			      (seq-map (lambda (x) (list (car x) (cdr x))))))
			((stringp grouper)
			 (list (list grouper it)))
			(t
			 (when-let ((at-dots (cl-delete-duplicates
					      (reorg--dot-at-search grouper)
					      :test #'equal)))
			   (setq it 				 
				 (cl-loop for data in it
					  append (cl-loop for (_ . at-dot) in at-dots
							  if (listp (alist-get at-dot data))
							  return (cl-loop for x in (alist-get at-dot data)
									  collect (let ((ppp (copy-alist data)))
										    (setf (alist-get at-dot ppp) x)
										    ppp))
							  finally return data))))
			 (->> it			      
			      (reorg--seq-group-by
			       (reorg--depth-first-apply grouper
							 #'reorg--turn-at-dot-to-dot))
			      (seq-map (lambda (x) (list (car x) (cdr x)))))))
		  (seq-filter (lambda (x) (and (not (null (car x)))
					       (not (null (cdr x)))
					       (not (null x))))
			      it)
		  (cl-loop
		   for x in it
		   do (setf
		       (car x)
		       (list
			(cons 'branch-name (car x))
			(cons 'headline (car x))
			(cons 'reorg-branch t)
			(cons 'result-sorters result-sorters)
			(cons 'grouper-list `(lambda (x)
					       (let-alist x
						 ,grouper)))
			(cons 'branch-predicate `(lambda (x)
						   (let-alist x
						     ,grouper)))
			(cons 'branch-result (car x))
			(cons 'grouper-list-results (car x))
			(cons 'format-string format-string)
			(cons 'result-sorters result-sorters)
			(cons 'template template)
			(cons 'children children)
			(cons 'branch-sorter heading-sorter)
			(cons 'branch-sort-getter heading-sort-getter)
			(cons 'reorg-level level)))
		   finally return it)
		  (seq-filter (lambda (x) (and (not (null (car x)))
					       (not (null (cdr x)))
					       (not (null x))))
			      it)
		  (cl-loop for x in it
			   do (setf (car x)
				    (reorg--create-headline-string (car x)
								   (copy-tree format-string)
								   level))
			   finally return it)
		  (if heading-sorter
		      (seq-sort-by (lambda (y)
				     (funcall heading-sort-getter (car y)))
				   heading-sorter
				   it)
		    it)))
	    (if children
		(progn ;; if		  
		  (cl-loop for x below (length (nth n (cdr data)))
			   do (setcdr
			       (nth x (nth n (cdr data)))
			       (cl-loop
				for z below (length children)
				collect (seq-copy
					 (cadr (nth x (nth n (cdr data))))))))
		  (cl-loop for x below (length children)
			   do (cl-loop
			       for y below (length (nth n (cdr data)))
			       do (doloop
				   (nth y (nth n (cdr data)))
				   (nth x children)
				   x
				   result-sorters
				   grouper-list
				   (append
				    grouper-list-results
				    (list (alist-get 'branch-value
						     (nth y (nth n (cdr data))))))
				   format-string
				   (1+ level)))))

	      (progn ;; else 
		(when result-sorters
		  (cl-loop for x below (length (nth n (cdr data)))
			   do (setf
			       (cadr (nth x (nth n (cdr data))))
			       (reorg--multi-sort result-sorters
						  (cadr (nth x (nth n (cdr data))))))))
		(cl-loop for x below (length (nth n (cdr data)))
			 do (setf
			     (cadr (nth x (nth n (cdr data))))
			     (cl-loop for each in (cadr (nth x (nth n (cdr data))))
				      collect
				      (progn (setf (alist-get 'reorg-stars each) (1+ level))
					     (push (cons 'reorg-level (1+ level)) each)
					     (reorg--create-headline-string each format-string (1+ level)))))))))))
      (doloop copy template)
      (cadr copy))))

(provide 'reorg-sort)
