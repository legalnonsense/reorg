;;; -*- lexical-binding: t; -*-

(defun xxx-reorg-test-1 ()
  (interactive)
  (reorg-open-sidebar '( :group "Reorg test"
			 :children (( :group (when-let ((legs .property.legs)
							(legs (string-to-number legs)))
					       (when (> legs 4)
						 (format "Num legs: %d" legs)))
				      ;; :sort-getter (lambda (x) (string-to-number (s-trim (cadr (s-split ":" (car x))))))
				      ;; :sort (lambda (a b)
				      ;; 	      (< a b))
				      :sort-results ((.headline . string>)))

				    ( :group (when-let ((predator .property.predator))
					       (if (string= "1" predator)
						   "Predator"
						 "Non-predator"))
				      :children (( :group (when-let ((eggs .property.eggs))
							    (if (string= "1" eggs)
								"Non-mammal"
							      "Mammal"))
						   :sort-results ((.property.legs . string<)))))))


		      (setq reorg-headline-format '((stars) (" ") (headline) (" ") (align-to 20) (deadline) (" ")
						    (align-to 50)
						    (property legs)))))


(defun xxx-reorg-test-3 ()
  (interactive)
  (reorg-open-sidebar '( :group "Reorg test"
			 :sort-results ((.headline . string>))
			 :children (( :group (when-let ((legs .property.legs)
							(legs (string-to-number legs)))
					       (format "Num legs: %d" legs))
				      :format-string ((stars) (" ") (headline) (" ") (align-to 20) (property fins) (align-to 30) (deadline) (" ")
						      (align-to 50) (property legs))
				      :children (( :group (when (not (null .property.tail))
							    (if (string= .property.tail "1")
								"Tail" "Lacking a tail"))
						   :children (( :group (when (not (null .property.fins))
									 (if (string= .property.fins "1")
									     "Fins" "No fins")))))))
				    ( :group (when-let ((predator .property.predator))
					       (if (string= "1" predator)
						   "Predator"
						 "Non-predator"))
				      :format-string ((stars) (" ") (todo) (" ") (headline) (align-to 30) (property predator) (" ") (scheduled))
				      :children (( :group (when-let ((eggs .property.eggs))
							    (if (string= "1" eggs)
								"Non-mammal"
							      "Mammal")))))))))

(cl-defun reorg--drop-into-template (data template
					  &optional (n 0 np)
					  result-sorters
					  grouper-list
					  grouper-list-results
					  format-string
					  (level 1))

  (let ((grouper (plist-get template :group))
	(children (plist-get template :children))
	(heading-sorter (plist-get template :sort))
	(heading-sort-getter (or (plist-get template :sort-getter)
				 #'car))
	(format-string (or (plist-get template :format-string)
			   format-string
			   reorg-headline-format))
	(result-sort (plist-get template :sort-results)))
    (when result-sort
      (setq result-sorters
	    (append result-sorters					  
		    (cl-loop for (form . pred) in result-sort
			     collect (cons `(lambda (x)
					      (reorg--let-plist x
								,form))
					   pred)))))
    (reorg-tree--with-wide-buffer
     (goto-char (point-min))
     (if (reorg-into--need-to-make-new-branch? data)
	 (if children
	     (progn 
	       (cl-loop while (reorg--goto-next-relative-level 1)
			do (cl-loop for child in children
				    do (reorg--drop-into-template data
								  (plist-get template :children)
								  result-sorters
								  grouper-list
								  grouper-list-results nil
								  format-string
								  (1+ level))))

		(plist-put data 
	       (cl-loop for x below (length children)
			do (cl-loop
			    for y below (length (nth n (cdr data)))
			    do (doloop
				(nth y (nth n (cdr data)))
				(nth x children)
				x
				result-sorters
				grouper-list
				(append grouper-list-results
					(list (plist-get (car (nth y (nth n (cdr data))))
							 :branch-value)))
				format-string
				(1+ level)))))
