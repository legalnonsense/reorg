;; -*- lexical-binding: t; -*-

(defun reorg-user--main-view ()
  (interactive)
  (reorg-open-main-window
   :sources '((org . "~/org/taskmaster.org"))
   :template
   '( :children
      (( :group (when (and (equal .priority "A")
			   .todo
			   (not (equal .todo "DONE")))
		  (propertize (concat "Top Priority"
				      (propertize " "
						  'display 
						  (cons 'space
							'(:align-to center))))
			      'font-lock-face
			      '((t (:family "ET Bembo" :underline t :height 1.4)))))
	 :format-results (.priority
			  " "
			  (s-pad-right 20 " " .category-inherited)
			  .headline
			  (propertize " " 'display (cons 'space '(:align-to 20)))
			  ))              
       ( :group (when (and .ts-ts
			   (or .deadline
			       .scheduled
			       .timestamp)
			   (ts> .ts-ts (ts-now))
			   (ts< .ts-ts (ts-adjust 'day +7 (ts-now)))
			   (and .todo 
				(not (string= "DONE" .todo))))
		  (propertize (concat "This week"
				      (propertize " "
						  'display 
						  (cons 'space
							'(:align-to center))))
			      'font-lock-face '((t (:family "ET Bembo" :underline t :height 1.4)))))
	 :format-results (.ts-type
			  " "
			  (s-pad-right 30 " " .ts)
			  (s-pad-right 20
				       " "
				       .category-inherited)
			  " " .headline)
	 :sort-results ((.ts . string<)))       
       ( :group (when .category-inherited
		  (propertize (concat "By Client"
				      (propertize " "
						  'display 
						  (cons 'space
							'(:align-to center))))
			      'font-lock-face
			      '((t (:family "ET Bembo" :underline t :height 1.4)))))	 
	 :children
	 (( :group
	    (propertize (concat .category-inherited
				(propertize " "
					    'display 
					    (cons 'space
						  '(:align-to center))))
			'font-lock-face
			'((t (:height 1.4 :family "ET Bembo"
				      :background "grey"))))
	    :sort-groups
	    (lambda (a b)
	      (string< (downcase a)
		       (downcase b)))			
	    :children
	    (( :group
	       (when
		   (and
		    .todo
		    (or 
		     (string= .todo "TASK")
		     (string= .todo "WAITING")))
		 
		 (propertize
		  (concat "TASKS"
			  (propertize " "
				      'display
				      (cons 'space
					    '(:align-to center))))
		  'font-lock-face
		  '((t ( :underline t
			 :background "light grey")))))

	       :sort-group
	       string<
	       :format-results
	       (.priority
		" "
		(s-pad-right 15 " " .todo)
		" " .headline)
	       :sort-results
	       ((.priority . string<)
		(.todo . string<)
		(.headline . string<)))
	     ( :group (when (and .ts-ts
				 (or .deadline
				     .scheduled
				     .timestamp)
				 (ts> .ts-ts (ts-now))
				 (and .todo 
				      (not (string= "DONE" .todo))))
			(propertize (concat "CALENDAR"
					    (propertize " "
							'display
							(cons 'space
							      '(:align-to center))))
				    'font-lock-face
				    '((t ( :underline t
					   :background "light grey")))))
	       :format-results
	       (.ts-type
		" "
		(s-pad-right 30 " " .ts)
		" " .headline)
	       :sort-results
	       (( .ts . string<)))))))
       ( :group (propertize (concat "Calendar"
				    (propertize " "
						'display 
						(cons 'space
						      '(:align-to center))))
			    'font-lock-face
			    '((t (:family "ET Bembo" :underline t :height 1.4))))
         :children (( :group
		      (when (and .ts-year
				 (ts>= .ts-ts (ts-now)))
			.ts-year)
		      :sort-groups
                      (lambda (a b) (string< a b))
                      :children
                      (( :group
                         .ts-month
                         :sort-groups
                         (lambda (a b)
                           (let ((seq '("January"
					"February"
					"March"
					"April"
					"May"
					"June"
					"July"
					"August"
					"September"
					"October"
					"November"
					"December")))
                             (< (seq-position seq a 'string=)
				(seq-position seq b 'string=))))
                         :sort-results
                         ((.ts-day . <))
                         :format-results
                         (.stars
                          " "
                          (s-pad-left 2 " "
                                      (number-to-string
                                       .ts-day))
                          " "
                          (s-pad-right 12 " "
                                       .ts-day-name)
                          (s-pad-right
                           20
                           " "
                           .category-inherited)
                          .headline))))))
       ))))
