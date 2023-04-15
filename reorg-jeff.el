;; -*- lexical-binding: t;-*-
(setq reorg-test-org-file-list (org-agenda-files))
;; (setq reorg-test-org-file-list "~/tmp/tmp.org")y

(defun jrf/reorg-client ()
  (interactive)
  (let ((now (ts-format "%Y-%m-%d" (ts-now))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :format-results ( (s-pad-right 5 " " .priority)
				(s-pad-right 15 " " .todo)
				" "
				(if .ts-single
				    (s-pad-right 50 "." .headline)
				  .headline)
				(when .ts-single
				  (reorg-org--format-time-string
				   .ts-single
				   "%a, %b %d, %Y"
				   "%a, %b %d, %Y at %-l:%M%p")))
	      :children
	      (( :group "Cases"
		 :children
		 (( :group (when (or 
				  (member .todo '("TASK"
						  "DELEGATED"
						  "EVENT"
						  "OPP_DUE"
						  "WAITING"
						  "DEADLINE"))
				  (and .ts-single
				       (org-string>= (reorg-org--format-time-string
						      .ts-single
						      "%Y-%m-%d")
						     ,now))
				  (string= .headline "_NOTES_"))
			     .root)
		    :sort-groups reorg-string<
		    :children (( :group (when (member .todo '("TASK"
							      "DELEGATED"
							      "WAITING"))
					  "TASKS")
				 :sort-groups (lambda (a b)
						(reorg--sort-by-list a b
								     '("TASK"
								       "DELEGATED"
								       "WAITING")))
				 :sort-results ((.priority . string<)
						(.todo . string<)
						(.headline . reorg-string<)))
			       ( :group (when (and
					       .ts-single
					       (org-string>= (reorg-org--format-time-string
							      .ts-single
							      "%Y-%m-%d")
							     ,now)
					       (not (member .todo '("TASK"
								    "DONE"
								    "DELEGATED"
								    "WAITING"))))
					  "CALENDAR")
				 :sort-results ((.ts-single . string<)))
			       ( :group (when (equal "_NOTES_" .headline) "")
				 :format-results (.stars "  NOTES"))))))
	       ( :group (when (and (member .todo '("TASK"
						   "DELEGATED"
						   "WAITING"))
				   .property.delegated)
			  "Delegations")
		 :format-results ((s-pad-right 5 " " .priority)
				  (s-pad-right 10 " " .todo)
				  (s-pad-right 20 " " .category-inherited)
				  " "
				  .headline)
		 :children (( :group .delegated
			      :sort-results ((.category-inherited . reorg-string<)
					     (.priority . reorg-string<)
					     (.headline . reorg-string<))
			      :sort-groups reorg-string<)))
	       ( :group (when (and (member .todo '("TASK"
						   "DELEGATED"
						   "WAITING"))
				   (if .ts-single
				       (org-string>= (reorg-org--format-time-string
						      .ts-single
						      "%Y-%m-%d")
						     ,now)
				     t))
			  "Priorities")
		 :sort-results ((.category-inherited . reorg-string<)
				(.todo . (lambda (a b)
					   (reorg--sort-by-list a
								b
								'("TASK"
								  "DELEGATED"
								  "WAITING")))))
		 :format-results ((s-pad-right 10 " " .todo)
				  (s-pad-right 15 " " .category-inherited)
				  (if .ts-single
				      (s-pad-right 50 "." .headline)
				    .headline)
				  (when .ts-single
				    (reorg-org--format-time-string
				     .ts-single
				     "%a, %b %d, %Y"
				     "%a, %b %d, %Y at %-l:%M%p")))
		 :children (( :group (when (equal .priority "A") "High"))
			    ( :group (when (equal .priority "B") "Medium"))
			    ( :group (when (equal .priority "C") "Low"))))

	       ))))))

(defun jrf/reorg-calendar-journal-log ()
  ""
  (interactive)
  (reorg-open-sidebar 
   `( :sources ((org . ,reorg-test-org-file-list))
      :group "Calendar"
      :format-results (.stars
		       " "
		       (s-pad-left 2 " "
				   (number-to-string
				    (ts-day .ts-all)))
		       " "
		       (s-pad-right 12 " "
				    (ts-day-name .ts-all))
		       (s-pad-right
			20
			" "
			.category-inherited)
		       .headline)
      :children (( :groups
		   (when .@ts-all
		     (ts-format "%Y" .ts-all))
		   :sort-groups reorg-string>
		   :children (( :group (ts-month-name .ts-all)
				:sort-groups (lambda (a b)
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
				:sort-results ((.ts-all . ts<)))))))))

(provide 'reorg-jeff)
