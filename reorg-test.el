;; -*- lexical-binding: t; -*-

(defun reorg-user--test-main-view ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/org/taskmaster.org"))
   :template
   '( :children
      (( :group "By client"
	 :children
	 (( :group
	    .category-inherited
	    :sort-groups
	    (lambda (a b)
	      (string< (downcase a)
		       (downcase b)))			
	    :children
	    (( :group
	       (when
		   (and
		    .todo
		    (not (string= "DONE" .todo))
		    (not (string= "EVENT" .todo))
		    (not (string= "DEADLINE" .todo)))
		 "Tasks")
	       :sort-group
	       string<
	       :format-results
	       (.priority
		" "
		(s-pad-right 15 " " .todo)
		" " .headline)
	       :sort-results
	       ((.priority . string<)
		(.headline . string<)))
	     ( :group (when (and .ts-ts
				 (ts> .ts-ts (ts-now)))
			"Calendar")
	       :format-results
	       (.ts-type
		" "
		(s-pad-right 30 " " .ts)
		" " .headline)
	       :sort-results
	       (( .ts . string<)))))))
       ( :group "By delegatee"
	 :children (( :group
		      .delegatee
		      :sort-group
		      (lambda (a b)
			(string< a b)))))		 
       ( :group "Calendar"
	 :children (( :group
		      .ts-year
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
			  .headline))))))))))

(defun reorg-user--test-email ()
  (interactive)
  (reorg-open-sidebar
   :sources '((email . "subject:allums"))
   :template '( :children (( :group "Allums emails"
			     :children (( :group (when (eq .class 'email)
						   "Emails")
					  :format-results (.stars
							   " "
							   .date
							   "\t\t"
							   .subject))))))))

(defun reorg-user--test-allums-view ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/org/Allums.org")
	      (org . "~/tmp/tmp.org")
	      (email . "subject:allums")
	      (files . "find ~/legal/Dropbox/Allums\,\\ Matthew/docket -type f"))
   :template '( :children (( :group "Allums"
			     :children (( :group (when (and .todo
							    (equal .category-inherited
								   "Allums")
							    (equal .todo "TASK"))
						   "TASKS")
					  :format-results (.stars " " .task " " .headline))
					( :group (when (and (equal .category-inherited
								   "Allums")
							    .ts-ts)
						   "CALENDAR")
					  :format-results (.stars " " .headline))
					( :group (when (eq .class 'files)
						   "Docket")
					  :format-results (.stars " " .filename))
					( :group (when (eq .class 'email)
						   "Emails")
					  :format-results (.stars
							   " "
							   .date
							   "\t\t"
							   .subject)
					  :sort-results ((.date . string>)))))))))


(defun reorg-user--test-main-view ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/tmp/tmp.org"))
   :template
   '( :children
      (( :group "By client"
	 :children
	 (( :group
	    .category-inherited
	    :sort-groups
	    (lambda (a b)
	      (string< (downcase a)
		       (downcase b)))			
	    :children
	    (( :group
	       (when
		   (and
		    .todo
		    (not (string= "DONE" .todo))
		    (not (string= "EVENT" .todo))
		    (not (string= "DEADLINE" .todo)))
		 "Tasks")
	       :sort-group
	       string<
	       :format-results
	       (.priority
		" "
		(s-pad-right 15 " " .todo)
		" " .headline)
	       :sort-results
	       ((.priority . string<)
		(.headline . string<)))
	     ( :group (when (and .ts-ts
				 (ts> .ts-ts (ts-now)))
			"Calendar")
	       :format-results
	       (.ts-type
		" "
		(s-pad-right 30 " " .ts)
		" " .headline)
	       :sort-results
	       (( .ts . string<)))))))
       ( :group "By delegatee"
	 :children (( :group
		      .delegatee
		      :sort-group
		      (lambda (a b)
			(string< a b)))))		 
       ( :group "Calendar"
	 :children (( :group
		      .ts-year
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
			  .headline))))))))))

   (defun reorg-user--leo-2 ()
     (interactive)
     (reorg-open-sidebar
      :sources '((leo . "~/.leo/workbook.leo"))
      :template '( :group "workbook.leo"
		   :format-string ((make-string (1+ (or .leo-level 1)) ?*) " " .headline)
		   :format-string-overrides ((reorg-branch . t)
					     (reorg-level . .leo-level)))))

   (defun reorg-user--leo ()
     (interactive)
     (reorg-open-sidebar
      :sources '((leo . "~/.leo/workbook.leo"))
      :template '( :group "workbook.leo"
		   :format-string (.stars " " .headline))))

   (defun reorg-user--clone-file-2 ()
     (interactive)
     (reorg-open-sidebar
      :sources '((org . "~/tmp/tmp.org"))
      :template '( :group "tmp.org"
		   :format-string ((make-string (1+ (or .org-level 1)) ?*) " " .headline)
		   :format-string-overrides ((reorg-branch . t)
					     (reorg-level . .org-level)))))
   ;; :sort <
   ;; :sort-getter .order)))

   (defun reorg-user--clone-file ()
     (interactive)
     (reorg-open-sidebar
      :sources '((org . "~/tmp/tmp.org"))
      :template '( :group "tmp.org"
		   :format-string (.stars " " .headline)
		   :sort <
		   :sort-getter .order)))


(defun xxx-reorg-test-15 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org"))
   :template '( 
	       :children (( :group .@at-names
			    :format-results
			    (.stars " " .headline))))))

(defun xxx-reorg-test-16 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org"))
   :template
   '( 
     :format-string (.stars " " .headline)
     :children (( :group .@tag-list
		  :sort-groups (lambda (a b)
				 (string< (downcase a)
					  (downcase b)))
		  :children (( :group .@at-names )))))))

(defun reorg-test--test-file-view ()
  (interactive)
  (reorg-open-sidebar
   :sources '((files . "find ~/Desktop -type f"))
   :template
   '( 
     :children (( :group "By extension"
		  :children (( :group .extension
			       :sort-groups (lambda (a b) (string< (downcase a)
								   (downcase b)))
			       :sort-results (((downcase .filename) . string<))
			       :format-results (.filename))))
		( :group "by parent"
		  :children (( :group (when .depth (number-to-string .depth ))
			       :sort-groups string<
			       :format-results (.stars " " .fullname))))))))

(provide 'reorg-test)

