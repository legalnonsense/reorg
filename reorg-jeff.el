;; -*- lexical-binding: t;-*-
(setq reorg-test-org-file-list (org-agenda-files))
;; (setq reorg-test-org-file-list "~/tmp/tmp.org")y

(defun jrf/reorg-main ()
  (interactive)
  (reorg-org-capture-enable)
  (let ((now (format-time-string "%Y-%m-%d"))
	(week-ago (ts-format "%Y-%m-%d" (ts-dec 'day 7 (ts-now)))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :format-results ( (s-pad-right 5 " " .priority)
				(s-pad-right 15 " " .todo)
				" "
				(if .ts-single
				    (s-pad-right 50 "." .headline)
				  .headline)
				(s-pad-right 35
					     " "
					     (if .ts-single
						 (reorg-org--format-time-string
						  .ts-single
						  "%a, %b %d, %Y"
						  "%a, %b %d, %Y at %-l:%M%p")
					       ""))
				.clocked-time)
	      :children
	      (
	       ( :group (when (and (member .todo '("TASK"
						   "DELEGATED"
						   "EVENT"
						   "DONE"
						   "OPP_DUE"
						   "WAITING"
						   "DEADLINE"))
				   .ts-agenda-today)
			  "Today")
		 :format-results
		 ((if (reorg-org--ts-hhmm-p .ts-agenda-today)
		      (s-pad-right 20 "."
				   (downcase
				    (reorg-org--format-time-string 
				     .ts-agenda-today
				     "" "%-l:%M%p")))
		    (make-string 20 ? ))
		  (s-pad-right 10 " " .todo)
		  (s-pad-right 15 " " .category-inherited)
		  (s-pad-right 50 " " .headline)
		  (s-pad-left 30 " " .clocked-time))
		 :sort-results
		 ((.ts-agenda-today . string<)))
	       
	       ( :group "Cases"
		 :children
		 (( :group (when (and (or 
				       (member .todo '("TASK"
						       "DELEGATED"
						       "EVENT"
						       "OPP_DUE"
						       "WAITING"
						       "DEADLINE"))
				       ;; .ts-single)

				       ;; (and .ts-single
				       ;;      (org-string>= (reorg-org--format-time-string
				       ;; 		      .ts-single
				       ;; 		      "%Y-%m-%d")
				       ;; 		     ,now))
				       (string= .headline "_NOTES_"))
				      (not (member "prospective" .tag-list))
				      (not (member "closed" .tag-list)))
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
			       ( :group (when (and (or .ts-deadline
						       .ts-active-first
						       .ts-scheduled)

						   ;; (org-string>= (reorg-org--format-time-string
						   ;; 		      .ts-single
						   ;; 		      "%Y-%m-%d")
						   ;; 		     ,now)
						   (not (member .todo '("TASK"
									"DONE"
									"DELEGATED"
									"WAITING"))))
					  "CALENDAR")
				 :sort-results ((.ts-single . string<)))
			       ( :group (when (equal "_NOTES_" .headline) "")
				 :format-results (.stars "  NOTES"))))))
	       
	       ( :group "Prospective cases"
		 :children (( :group (when (and (or 
						 (member .todo '("TASK"
								 "DELEGATED"
								 "EVENT"
								 "OPP_DUE"
								 "WAITING"
								 "DEADLINE"))
						 ;; .ts-single)

						 ;; (and .ts-single
						 ;;      (org-string>= (reorg-org--format-time-string
						 ;; 		      .ts-single
						 ;; 		      "%Y-%m-%d")
						 ;; 		     ,now))
						 (string= .headline "_NOTES_"))
						(member "prospective" .tag-list))
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
					 ( :group (when (and (or .ts-deadline
								 .ts-active-first
								 .ts-scheduled)

							     ;; (org-string>= (reorg-org--format-time-string
							     ;; 		      .ts-single
							     ;; 		      "%Y-%m-%d")
							     ;; 		     ,now)
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
						   ))
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
			    ( :group (when (equal .priority "C") "Low"))))))))))

(defun jrf/reorg-billing-log ()
  ""
  (interactive)
  (reorg-open-sidebar
   `( :sources ((org . ,reorg-test-org-file-list))
      :group (when (and .@ts-all-flat
			(stringp .ts-all-flat)
			(s-starts-with-p ,(format-time-string "%Y-%m")
					 .ts-all-flat))
	       "This month")
      :children (( :group (reorg-org--format-time-string .ts-all-flat "%B")
		   :children (( :group (reorg-org--format-time-string .ts-all-flat "%d %A")
				:sort-groups reorg-string>))))      
      :sort-results ((.ts-all-flat . (lambda (a b)
				       (reorg-string< 
					(reorg-org--format-time-string a "%H:%M")
					(reorg-org--format-time-string b "%H:%M")))))
      :format-results ((s-pad-right 20 " " .category-inherited)
		       " "
		       (s-pad-right 10 " " .todo)
		       " "
		       (s-pad-right 50 " "
				    (s-truncate 40 .headline "..."))
		       .clocked-time))))



(provide 'reorg-jeff)
