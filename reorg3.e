(let ((tree '(( :group "Reorg test"
		;;		:sort-results ((.headline . string>))
		:children
		(( :group (when-let ((legs .property.legs)
				     (legs (string-to-number legs)))
			    (format "Num legs: %d" legs))
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
		   :children (( :group (when-let ((eggs .property.eggs))
					 (if (string= "1" eggs)
					     "Non-mammal"
					   "Mammal")))))
		 ( :group (substring .headline 0 1)
		   :children (( :group "child 1")
			      ( :group "child 2")))))
	      ( :group "TEST"))))
  (reorg--get-all-trees-paths tree))
;;;test

(defun reorg--get-all-trees-paths (data)
  (let (aaa aaaa n)
    (cl-labels ((n-manager (new nn)
			   (if new
			       (push nn n)
			     (let ((val (+ (or (car n) 1) nn)))
			       (if (= val 0)
				   (progn 
                		     (pop n)
				     (when n
				       (setq n 
					     (n-manager nil -1))))
				 (setcar n val))))
			   n)
		(nnn (data)
		     (while data
		       (let ((this (pop data)))
			 (push (plist-get this :group) aaa)
			 (cond ((not (plist-get this :children))
				(push (reverse aaa) aaaa)
				(setq n (n-manager nil -1))
				(setq aaa (subseq aaa (- (length aaa) (length n)))))
			       (t 
				(push (length (plist-get this :children)) n)
				(nnn (plist-get this :children))))))))  
      (nnn data)
      (reverse aaaa))))