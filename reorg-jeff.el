;; -*- lexical-binding: t;-*-
(setq reorg-test-org-file-list (org-agenda-files))

(defun jrf/reorg-client ()
  (interactive)
  (let ((now (ts-format "<%Y-%m-%d" (ts-dec 'day 1 (ts-now)))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :group (when (or 
			    (member .todo '("TASK"
					    "DELEGATED"
					    "EVENT"
					    "OPP_DUE"
					    "WAITING" ;
					    "DEADLINE"))
			    (and .ts
				 (string> .ts ,now))
			    (string= .headline "_NOTES_"))
		       (propertize .root
				   'face
				   `(( t ( :foreground ,(face-foreground 'default)
					   :height 1.2
					   :family "ETBembo"
					   :weight bold
					   :underline t)))))
	      :format-results (.stars "   " (s-pad-right 5 " " .priority)
				      (s-pad-right 15 " " .todo)
				      " "
				      (if .ts
					  (s-pad-right 50 "." .headline)
					.headline)
				      .ts)
	      :sort-groups reorg-string<
	      :children (( :group (when (member .todo '("TASK" "DELEGATED" "WAITING"))
				    "TASKS")
			   :sort-groups (lambda (a b)
					  (reorg--sort-by-list a b
							       '("TASK"
								 "DELEGATED"
								 "WAITING")))
			   :sort-results ((.priority . string<)))
			 ( :group (when (and .ts
					     (not (member .todo '("TASK"
								  "DONE"
								  "DELEGATED"
								  "WAITING"))))
				    "CALENDAR")
			   :sort-results ((.ts . string<)))
			 ( :group (when (equal "_NOTES_" .headline) "")
			   :format-results (.stars "  NOTES"))))))))

(defun jrf/reorg-calendar-journal ()
  (interactive)
  (reorg-open-sidebar 
   `( :sources ((org . ,(org-agenda-files)))
      :group "Calendar"
      :children (( :group
		   (when (and .ts-year
			      (= (string-to-number .ts-year)
				 (ts-year (ts-now))))
		     .ts-year)
		   :sort-groups
		   reorg-string>
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
			  (reorg--sort-by-list a b seq)))
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
		       .headline))))))))
  
(provide 'reorg-jeff)
