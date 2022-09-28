;;; -*- lexical-binding: t; -*-

(setq xxx-template '(( :group "Reorg test"
		       :sort-results ((.headline . string>))
		       :children
		       (( :group (when-let ((legs .property.legs)
					    (legs (string-to-number legs)))
				   (format "Num legs: %d" legs))
			  :sort ((lambda (a b) (cond ((and a b) (< a b))
						     (a a)
						     (b b)
						     (t a)))
				 .
				 (lambda (x)
				   (when-let* ((str (split-string x ":" t " "))
					       (num (cadr str))
					       (num (string-to-number num)))
				     num)))

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
						  "Mammal")))))))
		     ( :group (substring .headline 0 1)
		       :sort (identity . string<))))
		     

(let ((tree '(( :group "Reorg test"
		;;		:sort-results ((.headline . string>))
		:format-string "asdf"
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
  ;; (reorg3--process-data xxx-data xxx-template))
  (reorg3--process-template tree))

(defun reorg3--outline-insert (branch-group)
  "Insert BRANCH into the outline."
  ;;   ;; First, Look to see if the first branch-hash is there
  ;;   ;; if it is, go to it. pop that branch from the branch stack
  ;;   ;;
  ;;   ;; Second, loop. Check the next. If so, go to it and continue.
  ;;   ;;
  ;;   ;; If the hash does not exist, do nothing else. Insert
  ;;   ;; each of your headings and your branch in sequential order
  ;;   ;; accoding to the fist location 

  ;;   ;; If you reach the end, insert it into the leaves
  ;;   ;; If you don't reach the end:
  ;;   ;; move to the next child of the current branch
  ;;   ;; check it against the branch sorting function
  ;;   ;; When you find your home, insert a blank line there
  ;;   ;; Insert yourself
  ;;   ;; update the data in the branch hierarchy if necessary
  ;;   ;;
  ;;   ;; Necessary functions:
  ;;   ;; - next-child: returns the next child; nil if there are none
  ;;   ;; - insert-leaf-here: insert a blank line and insert the leaf
  ;;   ;; - next-leaf: error: there are no leaves
  ;;   ;;              point: the point of the next leaf
  ;;   ;; - get-data: get the information about the current leaf or heading
  ;;   ;; - find-sorted-location: runs tests in the form of ((FUNC . PRED))
  ;;   ;;                  and puts the point in the correct location
  ;;   ;; - goto-funcs: goto the first point on the line of:
  ;;   ;;               - last child
  ;;   ;;               - first child
  ;;   ;;               - end of subtree

  (let* ((leaf (car (last branch-group)))
	 (branches-to-insert (cl-loop with branches = (reverse (butlast branch-group))
				      for n from 0
				      for branch in branches
				      when (reorg-tree--goto-next-property-field :branch-hash
										 (plist-get branch :branch-hash))
				      return (seq-subseq branches 0 n)
				      finally return nil)))
    (if branches-to-insert 
	(cl-loop for branch in branches-to-insert
		 do (reorg3--move-to-branch-position new-branch)
		 do (reorg3--make-blank-line 'after)
		 and do (insert (reorg--create-headline-string branch)))
      

      
      (reorg3--make-blank-line 'after)
      (insert (reorg--create-headline-string leaf)))

    ))


(reorg3--move-to-branch-position `( :branch-sort ((lambda (a b) (cond ((and a b) (< a b))
								      (a a)
								      (b b)
								      (t a)))
						  .
						  (lambda (x)
						    (when-let* ((str (split-string x ":" t " "))
								(num (cadr str))
								(num (string-to-number num)))
						      num)))
				    :branch-name "Num legs: 1000"
				    :level 2))

(defun reorg3--move-to-branch-position (new-header)
  "Find the location of NEW-HEADER, assuming it does not exist, and
go to the next line."
  (when (reorg--goto-next-relative-level 1)
    (let* ((name (plist-get new-header :branch-name))
	   (branch-sort (plist-get new-header :branch-sort))
	   (branch-sort-pred (car branch-sort))
	   (branch-level (plist-get new-header :level))
	   (branch-sort-getter (cdr branch-sort)))
      (if branch-sort-pred 
	  (cl-loop when (funcall branch-sort-pred
				 (funcall branch-sort-getter name)
				 (funcall branch-sort-getter (reorg--get-view-props nil :branch-name)))
		   return t
		   while (reorg--goto-next-relative-level 0)
		   finally return t)
	(forward-line 1)))))



   (defun reorg--nth (n list)
     "Like `nth', but allow negative numbers."
     (when (and (< n 0)
		(> (+ (length list) n) -1))
       (setq n (+ (length list) n)))
     (nth n list))

   (defun reorg3--process-data (data-list template)
     "Process DATA, which is a list of plists.  For each member
of DATA that is to be inserted into the outline, call `reorg3--outline-insert'."
     (cl-loop
      with template = (reorg3--process-template template)
      for d in data-list
      append
      (cl-loop
       with template-level = 0
       for each in template
       collect
       (cl-loop with nilp = nil
		with results = nil
		for n below (length each)

		if (funcall `(lambda (x) (reorg--let-plist x ,(plist-get (nth n each) :group))) d)
		collect `( :node-type branch
			   :template-level ,template-level
			   :level ,(1+ n)
			   
			   :branch-name ,(funcall `(lambda (x) (reorg--let-plist x ,(plist-get (nth n each) :group))) d)
			   
			   :branch-form ,(plist-get (nth n each) :group)
			   :branch-form-hash ,(sxhash-equal (plist-get (nth n each) :group))
			   :branch-hash ,(sxhash-equal
					  (list
					   (funcall `(lambda (x) (reorg--let-plist x ,(plist-get (nth n each) :group))) d)
					   (plist-get (nth n each) :group)
					   (1+ n)))
			   :branch-sort ,(plist-get (nth n each) :branch-sort)
			   :result-sort ,(plist-get (nth n each) :result-sort)
			   :format-string ,(or (plist-get (nth n each) :format-string)
					       reorg-headline-format))
		into results
		else return nil

		if (= 1 (- (length each) n))
		collect `( :reorg-data ,d
			   :node-type data
			   :level ,(+ 2 n)
			   :format-string ,(or (plist-get (nth n each) :format-string)
					       reorg-headline-format))
		into results

		;; finally do (reorg3--outline-insert results)
		finally return results))))


   (defun reorg3--process-template (template)
     "Process TEMPATE to get a list of all the paths from root to leaf point.
Return a processed template for use with `reorg3--process-data'."
     (let (group results log)
       (cl-labels
	   ((logger (newp delta)
		    (if newp
			(push delta log)
		      (let ((val (+ (or (car log) 1) delta)))
			(if (= val 0)
			    (progn 
                	      (pop log)
			      (when log (setq log (logger nil -1))))
			  (setcar log val))))
		    log)
	    (traverse (template
		       &optional
		       result-sort
		       format-string
		       (level 0)
		       (group-levels (list 0))
		       (counter 0)
		       (template-level 0)

		      (while template
			(let* ((this (pop template))
			       (result-sort
				(append
				 result-sort
				 (cl-loop for (form . pred)
					  in (plist-get template :result-sort)
					  collect (cons `(lambda (x)
							   (reorg--let-plist x
									     ,form))
							pred)))))
			  (push `( :group ,(plist-get this :group)
				   :result-sort ,result-sort
				   :branch-sort ,(plist-get this :sort)
				   
				   :format-string ,(or (plist-get this :format-string)
						       reorg-headline-format))
				group)
			  (cond ((not (plist-get this :children))
				 (push (reverse group) results)
				 (setq log (logger nil -1))
				 (setq group (subseq group (- (length group) (length log)))))
				(t 
				 (push (length (plist-get this :children)) log)
				 (traverse (plist-get this :children)
					   result-sort
					   format-string
					   level
					   (append (list (1+ (car group-levels)))
						   (cdr group-levels))
					   (1+ counter))))))))
	 (traverse template)
	 (reverse results))))

   (defun reorg3--make-blank-line (&optional after)
     (let ((inhibit-field-text-motion nil)
	   (disable-point-adjustment t))
       (if after
	   (end-of-line)
	 (beginning-of-line))
       (insert "\n")
       (unless after (forward-line -1))))

   (defun reorg3--find-outline-location-1 (data prop next)
     (cl-flet ((exit (x)
		     (reorg3--make-blank-line 'after)
		     (cl-loop for d in (seq-subseq data x)
			      do (insert (reorg--create-headline-string d (plist-get d :format-string) (plist-get d :level))
					 "\n"))))
       (cl-loop for entry in data 
		for x from 0
		if (when-let ((hash (plist-get entry prop)))
		     (reorg-tree--goto-next-property-field prop hash))
		return (exit x)
		finally return (if next
				   (reorg3--find-outline-location-1 data next nil)
				 (exit x)))))
 
(defun reorg3--insert-outline-entry (data)

(defun reorg3--traverse-children (func)
  
  (cl-loop while (reorg--goto-next-relative-level 0 nil nil t)
	   if (funcall func)
	   return (point)
	   finally return 
	   ))


(defun reorg3--find-outline-location-2 (data template)
  nil)

;; (defun reorg3--process-template-now (template entry)
;;   "Process TEMPATE to get a list of all the paths from root to leaf point.
;; Return a processed template for use with `reorg3--process-date'."
;;   (let (results results-results)
;;     (cl-labels ((traverse
;; 		 (template &optional
;; 			   result-sort
;; 			   format-string
;; 			   (level 0)
;; 			   branch-history)
;; 		 (while template
;; 		   (when-let* ((this (pop template))
;; 			       (name (funcall `(lambda (x)
;; 						 (reorg--let-plist x
;; 								   ,(plist-get this :group)))
;;   					      entry)))
;; 		     (let ((result-sort
;; 			    (append
;; 			     result-sort
;; 			     (cl-loop for (form . pred)
;; 				      in (plist-get template :result-sort)
;; 				      collect (cons `(lambda (x)
;; 						       (reorg--let-plist x
;; 									 ,form))
;; 						    pred))))
;; 			   (branch-hash (sxhash-equal (list name (plist-get this :group) level))))
;; 		       (push branch-hash branch-history)
;; 		       (push `( :node-type branch ;
;; 				:level ,level

;; 				:branch-name ,name              

;; 				:branch-form ,(plist-get this :group)
;; 				:branch-form-hash ,(sxhash-equal (plist-get this :group))

;; 				:branch-hash ,branch-hash
;; 				:branch-hash-history ,branch-history				

;; 				:branch-sort ,(plist-get this :branch-sort)
;; 				:result-sort ,result-sort     
;; 				:format-string ,(or (plist-get this :format-string)
;; 						    reorg-headline-format))
;; 			     results)
;; 		       (if (plist-get this :children)
;; 			   (traverse (plist-get this :children)
;; 				     result-sort
;; 				     format-string
;; 				     (1+ level)
;; 				     branch-history)
;; 			 (push `( :leaf ,entry
;; 				  :level level)
;; 			       results)
;; 			 (push (reverse results) results-results)
;; 			 (setq results nil)
;; 			 ))))))
;;       (traverse template)
;;       (reverse results-results))))

;; (reorg3--process-template-now xxx-template xxx) 



