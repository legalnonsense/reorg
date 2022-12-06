;; -*- lexical-binding: t; -*-

;; (alist-get 'org reorg--parser-list)
;; (alist-get 'org reorg--extra-prop-list)
;; (alist-get 'org reorg--render-func-list)

(defun reorg-user--test-main-view ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/org/taskmaster.org"))
   :template '( :children
		(( :group "By client"
		   :children (( :group .category-inherited
				:sort-groups (lambda (a b)
					       (string< (downcase a)
							(downcase b)))			
				:children (( :group (when
							(and
							 .todo
							 (not (string= "DONE" .todo))
							 (not (string= "EVENT" .todo))
							 (not (string= "DEADLINE" .todo)))
						      "Tasks")
					     :format-results (.priority " " .todo " " .headline)
					     :sort-results ((.priority . string<)))
					   ( :group (when .ts "Calendar")
					     :format-results (.ts-type
							      " "
							      (s-pad-right 25 " " .ts)
							      " " .headline)
					     ;; :sort-results
					     )))))))))



(defun reorg-user--test-new-grouper ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/tmp/tmp.org"))
   :template '( :children
		(( :group "SPLIT TAGS"
		   :children (( :group .@tag-list))
		   :sort-groups (lambda (a b)
				  (string< (downcase a)
					   (downcase b))))
		 ( :group "TAGS"
		   :children (( :group (when (not (string= "" .tags))
					 .tags))))

		 ( :group "TASKS"
		   :children
		   (( :group .todo
		      :format-results (.stars " " .headline)
		      :sort-groups string<
		      :sort-results (((lambda (x) (alist-get 'headline x)) . string<)))))
		 ( :group "Calendar deadlines"
		   :children (( :group .deadline
				:sort-groups string<
				:format-results (.stars " " .headline " " .deadline))))))))




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




(defun reorg-user--test-files ()
  (interactive)
  (reorg-open-sidebar
   :sources '((files . "find ~/legal/Dropbox/hannah -type f"))
   :template '( :group "Files"
		:format-string (.stars " " .extension "\t\t" .filename)
		:children (( :group .extension)))))

(defun reorg-user--create-meeting-view ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/tmp/tmp.org"))
   :template
   '( :group "MEETING VIEW"
      :format-string (.stars " " .headline)
      :children
      (
       ( :group "CASE LIST"
	 :children
	 (
	  ( :group .category-inherited
	    :sort string< 
	    :sort-getter (lambda (x) (downcase x))
	    :children
	    (
	     ( :group (when (and .todo
				 (not (string= .todo "DONE"))
				 (not (string= .todo "EVENT"))
				 (not (string= .todo "OPP_DUE"))
				 (not (string= .todo "DEADLINE")))
			"TASKS" )
	       :format-string (.stars " " (s-pad-right 10 " " .todo) " " .headline)
	       :sort-results ((.todo . string<)
			      ((downcase .headline) . string<)))
	     ( :group (when (and
			     (or (string= .todo "DEADLINE")
				 (string= .todo "EVENT")
				 (string= .todo "OPP_DUE"))
			     (or .timestamp
				 .deadline
				 .scheduled))
			"CALENDAR")
	       :format-string (.ts-type
			       " "
			       (s-pad-right 50
					    "."
					    (s-truncate 40 .headline "..."))
			       .ts)
	       :sort-results ((.ts . string<)))
	     ( :group (when (string= .headline "_NOTES_")
			"Progress Notes")
	       :format-string (.stars " " .headline))))))
       ( :group "Date tree"
	 :children
	 (( :group (when-let ((time (or .timestamp
					.deadline
					.scheduled
					.timestamp-ia)))
		     (number-to-string
		      (ts-year
		       (ts-parse time))))
	    :sort string< 
	    :sort-getter identity
	    :children
	    (( :group
	       (when-let ((time (or .timestamp
				    .deadline
				    .scheduled
				    .timestamp-ia)))
		 (concat
		  " "
		  (ts-month-name (ts-parse time))
		  " "
		  (number-to-string (ts-year (ts-parse time)))))
	       :sort string<
	       :sort-getter identity
	       :children
	       (( :group
		  (when-let ((time (or .timestamp
				       .deadline
				       .scheduled
				       .timestamp-ia)))
		    (concat 
		     (ts-day-name (ts-parse time))
		     " "
		     (s-pad-left 2
				 "0"
				 (number-to-string
				  (ts-day (ts-parse time))))
		     " "
		     (ts-month-name (ts-parse time))
		     ", "
		     (number-to-string 
		      (ts-year (ts-parse time)))))
		  :sort string<
		  :sort-getter identity)))))))))))

(defun reorg-user--create-date-tree (&optional file)
  (interactive)
  (reorg-open-sidebar
   :sources `((org . ,(or file "~/Desktop/tmp.org")))
   :template 
   '( :group "Date tree"
      :children
      (( :group (when-let ((time (or .timestamp
				     .deadline
				     .scheduled
				     .timestamp-ia)))
		  (number-to-string
		   (ts-year
		    (ts-parse time))))
	 :sort string< 
	 :sort-getter identity
	 :children
	 (( :group
	    (when-let ((time (or .timestamp
				 .deadline
				 .scheduled
				 .timestamp-ia)))
	      (concat
	       " "
	       (ts-month-name (ts-parse time))
	       " "
	       (number-to-string (ts-year (ts-parse time)))))
	    :sort string<
	    :sort-getter identity
	    :children
	    (( :group
	       (when-let ((time (or .timestamp
				    .deadline
				    .scheduled
				    .timestamp-ia)))
		 (concat 
		  (ts-day-name (ts-parse time))
		  " "
		  (s-pad-left 2
			      "0"
			      (number-to-string
			       (ts-day (ts-parse time))))
		  " "
		  (ts-month-name (ts-parse time))
		  ", "
		  (number-to-string 
		   (ts-year (ts-parse time)))))
	       :sort string<
	       :sort-getter identity)))))))))

(defun xxx-reorg-test-17 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/Desktop/tmp.org"))
   :template
   '( :group "MEETING VIEW"
      :children (( :group (when .timestamp-all
			    .root)
		   :sort string<
		   :sort-getter (lambda (x) (downcase x))
		   :sort-results ((.timestamp-all . string<))
		   :format-string (concat " "
					  (or .deadline
					      .timestamp
					      (when .timestamp-range
						(car .timestamp-range))
					      .scheduled)
					  "\t"
					  .todo
					  "\t"
					  .headline))))))

(defun xxx-reorg-test-16 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org"))
   :template
   '( :group .@tag-list
      :format-string (.stars " " .headline)
      :children (( :group .@at-name )))))

;; group by cited file
(defun xxx-reorg-test-15 ()
  (interactive)
  (reorg-open-sidebar
   :sources '(;;(files . "find ~/legal/Dropbox/hannah -type f")
	      (org . "~/org/Hannah.org"))
   :template
   '( :group "Olivia Hannah"
      :children (( :group 
		   :format-string (" " .headline))))))

;; sort by inactive timestamp in any ancestor node 
(defun xxx-reorg-test-15 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((files . "find ~/legal/Dropbox/hannah -type f")
	      (org . "~/org/Hannah.org"))
   :template
   '( :group "Olivia Hannah"
      :children (( :group (or .root-ts-inactive .timestamp-ia)
		   :format-string (.headline))))))

(defun xxx-reorg-test-14 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((files . "find ~/Desktop -type f")
	      (org . "~/legal/Dropbox/DropsyncFiles/taskmaster.org"))
   :template
   '( :group "TEST"
      :children (( :group .todo
		   :sort string<
		   :sort-getter (lambda (x) (downcase x))
		   :format-string (concat " "
					  (if (eq .class 'org)
					      .headline
					    .filename)
					  .todo)
		   :sort-results ((.headline . string<))
		   :children (( :group (substring .headline 0 1)
				:sort string<
				:sort-getter (lambda (x) (downcase x)))
			      ( :group (substring .headline -1)
				:sort string<
				:sort-getter (lambda (x) (downcase x))
				:format-string (concat " "
						       .ts
						       .headline))))
		 ( :group (when (eq .class 'files)
			    .extension)
		   :format-string (concat " " .filename))))))

(defun xxx-reorg-test-13 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((files . "find ~/Desktop -type f")
	      (org . "~/legal/Dropbox/DropsyncFiles/taskmaster.org"))
   :template
   '( :group "TEST"
      :children (( :group (progn
			    (cond  ((eq .class 'org)
				    "Orgmode")
				   ((eq .class 'files)
				    "Files")))
		   ;; :sort string<
		   :format-string (concat " "
					  (if (eq .class 'org)
					      .headline
					    .filename)))))))
;; (cond ((eq .class 'org)
;; 	 (concat " " .headline))
;; 	((eq .class 'files)
;; 	 (concat " " .filename)))
;; :sort-getter (lambda (x) (downcase x)))))))

(defun xxx-reorg-test-12 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((files . "find ~/Desktop -type f"))
   :template
   '( :group "MEETING VIEW"
      :children (( :group .extension
		   :sort string<
		   :format-string (concat " " .filename)
		   :sort-getter (lambda (x) (downcase x)))))))

(defun xxx-reorg-test-11 ()
  (interactive)
  (reorg-open-sidebar
   :sources '((org . "~/legal/Dropbox/DropsyncFiles/taskmaster.org"))
   :template
   '( :group "MEETING VIEW"
      :children (( :group .root
		   :sort string<
		   :format-string (concat " " .headline)
		   :sort-getter (lambda (x) (downcase x)))))))



(defun xxx-reorg-test-control-panel-9 ()
  "test new headline creator"
  (interactive)
  (reorg-open-sidebar
   :file "~/.emacs.d/lisp/reorg/taskmaster.org"
   :template '( :group "MEETING VIEW"
		:children (( :group (when (and
					   (or (string= .todo "DEADLINE")
					       (string= .todo "EVENT")
					       (string= .todo "OPP_DUE"))
					   (or .timestamp
					       .deadline
					       .scheduled))
				      .category)
			     :sort string< 
			     :sort-getter identity
			     :format-string (concat " " .headline))))))

(defun xxx-reorg-test-control-panel-8 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/.emacs.d/lisp/reorg/taskmaster.org"
   :template '( :group "MEETING VIEW"
		:children (( :group (when (and
					   (or (string= .todo "DEADLINE")
					       (string= .todo "EVENT")
					       (string= .todo "OPP_DUE"))
					   (or .timestamp
					       .deadline
					       .scheduled))
				      .category)
			     :sort string< 
			     :sort-getter identity
			     :format-string ((concat (make-string .level ?*) " " .headline)))))))

(defun xxx-reorg-test-control-panel-7 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/.emacs.d/lisp/reorg/taskmaster.org"
   :template '( :group "MEETING VIEW"
		:children (( :group .category 
			     :sort string< 
			     :sort-getter identity
			     :children (( :group (when (and
							(or (string= .todo "DEADLINE")
							    (string= .todo "EVENT")
							    (string= .todo "OPP_DUE"))
							(or .timestamp
							    .deadline
							    .scheduled))
						   "CALENDAR")
					  :format-string ((stars) (" ") (ts-type) (" ") (ts) ("\n") (headline))
					  :sort-results ((.ts . string<)))
					( :group (when (and .todo
							    (not (string= .todo "DONE"))
							    (not (string= .todo "EVENT"))
							    (not (string= .todo "OPP_DUE"))
							    (not (string= .todo "DEADLINE")))
						   "TASKS" )
					  :format-string ((stars) (" ") (todo) (" ") (headline))
					  :sort-results ((.todo . string<)
							 ((downcase .headline) . string<)))))))))

(defun xxx-reorg-test-control-panel-7 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/.emacs.d/lisp/reorg/taskmaster.org"
   :template '( :group "MEETING VIEW"
		:children (( :group .category 
			     :sort string< 
			     :sort-getter identity
			     :children (( :group (when (and
							(or (string= .todo "DEADLINE")
							    (string= .todo "EVENT")
							    (string= .todo "OPP_DUE"))
							(or .timestamp
							    .deadline
							    .scheduled))
						   "CALENDAR")
					  :format-string ((stars) (" ") (ts-type) (" ") (ts) ("\n") (headline))
					  :sort-results ((.ts . string<)))
					( :group (when (and .todo
							    (not (string= .todo "DONE"))
							    (not (string= .todo "EVENT"))
							    (not (string= .todo "OPP_DUE"))
							    (not (string= .todo "DEADLINE")))
						   "TASKS" )
					  :format-string ((stars) (" ") (todo) (" ") (headline))
					  :sort-results ((.todo . string<)
							 ((downcase .headline) . string<)))))))))

(defun xxx-reorg-test-agenda--all-todo-8 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/legal/Dropbox/DropsyncFiles/taskmaster.org"
   :template '( :group "TASKS"
		:children (( :group (when .todo .category )
			     :sort string< 
			     :sort-getter identity
			     :format-string ((stars) (" ") (todo) (" ") (headline) (align-to 100) (ts))
			     :sort-results ((.ts . string<)
					    (.todo . string<)
					    ((downcase .headline) . string<)))))))

(defun xxx-reorg-test-6 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/legal/Dropbox/DropsyncFiles/taskmaster.org"
   :template '( :group "DATE TREE"
		:children (( :group (when-let ((time (or .timestamp
							 .deadline
							 .scheduled
							 .timestamp-ia)))
				      (number-to-string
				       (ts-year
					(ts-parse time))))
			     :sort string< 
			     :sort-getter identity
			     :children (( :group
					  (when-let ((time (or .timestamp
							       .deadline
							       .scheduled
							       .timestamp-ia)))
					    (concat
					     (s-pad-left 2 "0" (number-to-string
								(ts-month (ts-parse time))))
					     " "
					     (ts-month-name (ts-parse time))))
					  :sort string<
					  :sort-getter identity
					  :children (( :group

						       (when-let ((time (or .timestamp
									    .deadline
									    .scheduled
									    .timestamp-ia)))
							 (concat 
							  (s-pad-left 2
								      "0"
								      (number-to-string
								       (ts-day (ts-parse time))))
							  " "
							  (ts-day-name (ts-parse time))))
						       :sort string<
						       :sort-getter identity)))))
			   ( :group (when (and (s-contains-p "brian" .headline t)
					       (or .timestamp .timestamp-ia .deadline .scheduled))
				      "Brian stuff")
			     :sort-results ((.headline . string<)))))))

(defun xxx-reorg-test-4 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/.emacs.d/lisp/reorg/short-zoo.org"
   :template '( :group "Reorg test"
		:sort-results ((.headline . string>))
		:children (( :group (when-let ((legs .property.legs)
					       (legs (string-to-number legs)))
				      (format "Num legs: %d" legs))
			     :sort (lambda (a b) (cond ((and a b) (< a b))
						       (a a)
						       (b b)
						       (t a)))
			     :sort-getter (lambda (x)
					    (when-let* ((str (split-string x ":" t " "))
							(num (cadr str))
							(num (string-to-number num)))
					      num))
			     :format-string
			     ((stars)
			      (" ")
			      (headline)
			      (" ")
			      (align-to 20)
			      (property fins)
			      (align-to 30)
			      (deadline)
			      (" ")
			      (align-to 50)
			      (property legs))
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
			     :format-string ((stars)
					     (" ")
					     (todo)
					     (" ")
					     (headline)
					     (align-to 30)
					     (property predator)
					     (" ")
					     (scheduled))
			     :children (( :group (when-let ((eggs .property.eggs))
						   (if (string= "0" eggs)
						       "Non-mammal"
						     "Mammal")))))
			   ( :group (substring .headline 0 1)
			     :sort-getter identity
			     :sort string<)))))

(defun xxx-reorg-test-5 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/legal/Dropbox/DropsyncFiles/taskclone.org"
   :template '( :group (when (or (and .todo
				      (or (string= .todo "task")
					  (string= .todo "waiting")
					  (string= .todo "opp_due")))
				 (or (and .deadline
					  (ts<= (ts-now)
						(ts-parse-org .deadline)))
				     (and .timestamp
					  (ts<= (ts-now)
						(ts-parse-org .timestamp)))))
			 .category-inherited)
		:sort string<
		:format-string ((stars) (" ") (todo) (" ") (headline) (align-to 30) (deadline))
		:sort-getter identity
		:children (( :group (when (and .todo
					       (or (string= .todo "task")
						   (string= .todo "waiting")
						   (string= .todo "opp_due")))
				      "TASKS")
			     :sort string<
			     :format-string ((stars) (align-to 10) (priority) (align-to 15) (todo) (align-to 25) (headline)))
			   ( :group (when (or (and .deadline
						   (ts<= (ts-now)
							 (ts-parse-org .deadline)))
					      (and .timestamp
						   (ts<= (ts-now)
							 (ts-parse-org .timestamp))))
				      "CALENDAR")
			     :format-string ((stars) (align-to 10) (deadline) (align-to 35) (timestamp) (align-to 70) (headline))
			     :sort-results ((.deadline . string<)
					    (.timestamp . string<)))))))

(defun xxx-reorg-test-5 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/legal/Dropbox/DropsyncFiles/taskclone.org"
   :template '( :group (when (or (and .todo
				      (or (string= .todo "task")
					  (string= .todo "waiting")
					  (string= .todo "opp_due")))
				 (or (and .deadline
					  (ts<= (ts-now)
						(ts-parse-org .deadline)))
				     (and .timestamp
					  (ts<= (ts-now)
						(ts-parse-org .timestamp)))))
			 .category-inherited)
		:sort string<
		:format-string ((stars) (" ") (todo) (" ") (headline) (align-to 30) (deadline))
		:sort-getter identity
		:children (( :group (when (and .todo
					       (or (string= .todo "task")
						   (string= .todo "waiting")
						   (string= .todo "opp_due")))
				      "TASKS")
			     :sort string<
			     :format-string ((stars) (align-to 10) (priority) (align-to 15) (todo) (align-to 25) (headline)))
			   ( :group (when (or (and .deadline
						   (ts<= (ts-now)
							 (ts-parse-org .deadline)))
					      (and .timestamp
						   (ts<= (ts-now)
							 (ts-parse-org .timestamp))))
				      "CALENDAR")
			     :format-string ((stars) (align-to 10) (deadline) (align-to 35) (timestamp) (align-to 70) (headline))
			     :sort-results ((.deadline . string<)
					    (.timestamp . string<)))))))




(provide 'reorg-test)


