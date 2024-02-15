;; -*- lexical-binding: t;-*-
(setq reorg-test-org-file-list (org-agenda-files))
;; (setq reorg-test-org-file-list "~/tmp/tmp.org")y

(defun reorg--map-buffer (func)
  "apply funcs at each entry of the current buffer"
  (save-excursion 
  (ov-clear)
  (goto-char (point-min))
  (cl-loop do (funcall func)
	   while (reorg--goto-next-heading))))

(defun reorg--hide-heading ()
  "make the current heading invisible"
  (ov (1- (point-at-bol))
      (point-at-eol)
      'invisible t
      'reorg t))

(defun reorg---hide-test ()
  "hide anything starting with a \"[\""
  (reorg--map-buffer (lambda () (when (and (reorg--get-prop 'headline)
					   (s-starts-with-p "["
							    (reorg--get-prop 'headline)))
				  (reorg--hide-heading)))))

(defun reorg--truncate-and-pad (string trunc pad &optional ellipsis padding)
  (->> (s-truncate trunc string (or ellipsis " "))
       (s-pad-right pad (or padding " "))))

(defun jrf/reorg-diary ()
  (interactive)
  (reorg-org-capture-enable)
  (reorg-open-sidebar
   `( :sources ((org . ,reorg-test-org-file-list))
      :format-results (
		       (propertize
			(reorg--truncate-and-pad 
			 (car (s-split "," .root))
			 12 13)
			'face '((t ( :foreground "black"
				     :background "light gray"
				     :box t))))
		       "     "
		       (propertize 
			(reorg--truncate-and-pad .headline 50 55 "...")
			'face
			'((t ( :foreground "black"
				     :background "light gray"
				     :box t))))
		       .clocked-time)
      :group (when .ts-all-flat
	       (substring .@ts-all-flat 0 4))
      :sort-groups string>
      :children (( :group (reorg-org--format-time-string .ts-all-flat "%B")
		   :bullets "-"
		   :folded-bullets ">"
		   :sort-groups (lambda (a b)
		     (reorg--sort-by-list a b '("January"
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
		   :children (( :group (reorg-org--format-time-string .ts-all-flat "%e %A")
				:sort-groups string<)))))))

(defun jrf/reorg-agenda-cases ()
  (interactive)
  (reorg-org-capture-enable)
  (reorg-open-sidebar
   `( :sources ((org . ,reorg-test-org-file-list))
      :group (when (member "active" .tag-list) .root)
      :sort-groups reorg-string<
      :format-results ((s-pad-right 5 " " .priority)
		       (s-pad-right 15 " " .todo)
		       (if (or .ts-single-active .ts-active-range-first)
			   (s-pad-right 50 "." .headline)
			 .headline)
		       (s-pad-right 35
				    " "
				    (cond (.ts-single-active
					   (reorg-org--format-time-string
					    .ts-single-active
					    "%a, %b %d, %Y"
					    "%a, %b %d, %Y at %-l:%M%p"))
					  (.ts-active-range-first
					    (reorg-org--format-time-string
					     .ts-active-range-first
					     "%a, %b %d, %Y"
					     "%a, %b %d, %Y at %-l:%M%p"))
					  (t ""))))
      :children (( :group (when (and (member .todo '("TASK" "DELEGATED" "WAITING"))
				     (not .archivedp))
			    "--TASKS--")
		   :sort-results ((.priority . reorg-string<)
				  (.todo . (lambda (a b)
						 (reorg--sort-by-list a b '("TASK"
									    "DELEGATED"
									    "WAITING"))))))
		 ( :group (when (and (member .todo '("EVENT" "DEADLINE" "OPP_DUE"))
				     (not .archivedp))
			    "--CALENDAR--")
		   :sort-results ((.ts-single-active . reorg-string<)))
		 ( :group (when (and .archivedp
				     .ts-all-flat)
			    "--ARCHIVE--")
		   :format-results ((s-pad-right 20 " "
						 (if-let ((ts (or .ts-closed
								  .ts-active-first
								  .ts-inactive-first)))
						     (reorg-org--format-time-string
						      ts "%b %e, %Y")
						   " "))
				    (->> .headline
					 (s-truncate 47)
					 (s-pad-right 50 " "))
				    .clocked-time)
		   :sort-results (((or .ts-closed
				       .ts-active-first
				       (car .ts-inactive-all)
				       "") . reorg-string<)))))))

(defun jrf/reorg-today-agenda (&optional date)
    (interactive)
    (reorg-org-capture-enable)
    (let* ((now (or date (format-time-string "%Y-%m-%d")))
	   (week-ago (ts-format "%Y-%m-%d" (ts-dec 'day 7 (ts-parse now)))))
      (reorg-open-sidebar
       `( :sources ((org . ,reorg-test-org-file-list))
	  :children
	  (( :group (when (and (member .todo '("TASK"
					       "DELEGATED"
					       "EVENT"
					       "OPP_DUE"
					       "WAITING"
					       "DEADLINE"))
			       (not .archivedp))
		      (propertize (ts-format "%A, %b. %e, %Y" (ts-parse ,now))
				  'face '((t (:height 1.5)))
				  'keymap (let ((map (make-sparse-keymap)))
					    (define-key map (kbd "B")
					      (lambda ()
						(interactive)
						(funcall
						 #'jrf/reorg-today-agenda
						 (ts-format "%Y-%m-%d"
						 (ts-dec 'day 1 (ts-parse ,now))))))
					    (define-key map (kbd "F")
					      (lambda ()
						(interactive)
						(funcall
						 #'jrf/reorg-today-agenda
						 (ts-format "%Y-%m-%d"
						 (ts-inc 'day 1 (ts-parse ,now))))))
					    map)))
	     :children (
			( :group (when (cl-loop for each in .ts-active-flat
						when (and (string-prefix-p ,now each)
							  (reorg-org--ts-hhmm-p each))
						return each)
				   "--SCHEDULE--")
			  :format-results
			  (
			   (s-pad-right 23  "."
					(downcase
					 (reorg-org--format-time-string 
					  (cl-loop for each in '.ts-active-flat
						when (and (string-prefix-p ,now each)
							  (reorg-org--ts-hhmm-p each))
						return each)
					  "" "%-l:%M%p")))
			   .headline
			   " "
			   (propertize (s-pad-right 18 " "
						    (car (s-split "," .root t)))
				       'face
				       'org-tag))
			  :sort-results
			  ((.ts-agenda-today . reorg-string<))
			  )
			( :group (when (and (or (cl-loop for each in .ts-active-flat
							 when (and (string-prefix-p ,now each)
							  (reorg-org--ts-hhmm-p each))
							 return each)
						(equal .priority "A"))
					    (not .donep)
					    (not .archivedp))
				   "--TASKS TODAY--")
			  :format-results 
			  ( .priority
			    " "
			    (s-pad-right
			     20
			     " "
			     (car (s-split ","
					   .root)))
			    (s-pad-right 50 " " .headline))
			  :sort-results
			  ((.priority . reorg-string<)))			
			( :group (when (and .ts-single-active
					    (string< .ts-single-active (format-time-string "%Y-%m-%d"))
					    .todo
					    (not .archivedp)
					    (not (member .todo '("EVENT"
								 "DEADLINE"
								 "OPP_DUE"
								 "WAITING"
								 "DONE"))))
				   "--CARRIED OVER--")
			  :format-results ( .priority
					    " "
					    (s-pad-right
					     20
					     " "
					     (car (s-split ","
							   .root)))
					    (s-pad-right 50 " " .headline))
			  :sort-results ((.root . reorg-string<)
					 (.priority  . reorg-string<))))))))))

;; (defun jrf/reorg-main ()
;;   (interactive)
;;   (reorg-org-capture-enable)
;;   (let ((now (format-time-string "%Y-%m-%d"))
;; 	(week-ago (ts-format "%Y-%m-%d" (ts-dec 'day 7 (ts-now)))))
;;     (reorg-open-sidebar
;;      `( :sources ((org . ,reorg-test-org-file-list))
;; 	:format-results ( (s-pad-right 5 " " .priority)
;; 			  (s-pad-right 15 " " .todo)
;; 			  " "
;; 			  (if .ts-single
;; 			      (s-pad-right 50 "." .headline)
;; 			    .headline)
;; 			  (s-pad-right 35
;; 				       " "
;; 				       (if .ts-single
;; 					   (reorg-org--format-time-string
;; 					    .ts-single
;; 					    "%a, %b %d, %Y"
;; 					    "%a, %b %d, %Y at %-l:%M%p")
;; 					 ""))
;; 			  .clocked-time)
;; 	:children
;; 	(
;; 	 ( :group (when (and (member .todo '("TASK"
;; 					     "DELEGATED"
;; 					     "EVENT"
;; 					     "DONE"
;; 					     "OPP_DUE"
;; 					     "WAITING"
;; 					     "DEADLINE"))
;; 			     .ts-agenda-today)
;; 		    "Today")
;; 	   :format-results
;; 	   ((if (reorg-org--ts-hhmm-p .ts-agenda-today)
;; 		(s-pad-right 20 "."
;; 			     (downcase
;; 			      (reorg-org--format-time-string 
;; 			       .ts-agenda-today
;; 			       "" "%-l:%M%p")))
;; 	      (make-string 20 ? ))
;; 	    (s-pad-right 10 " " .todo)
;; 	    (s-pad-right 15 " " .category-inherited)
;; 	    (s-pad-right 50 " " .headline)
;; 	    (s-pad-left 30 " " .clocked-time))
;; 	   :sort-results
;; 	   ((.ts-agenda-today . string<)))	 
;; 	 ( :group "Cases"
;; 	   :children
;; 	   (( :group (when (and (or 
;; 				 (member .todo '("TASK"
;; 						 "DELEGATED"
;; 						 "EVENT"
;; 						 "OPP_DUE"
;; 						 "WAITING"
;; 						 "DEADLINE"))
;; 				 (string= .headline "_NOTES_"))
;; 				(not (member "prospective" .tag-list))
;; 				(not (member "closed" .tag-list))
;; 				(if .ts-single
;; 				    (org-string>= (reorg-org--format-time-string
;; 						   .ts-single
;; 						   "%Y-%m-%d")
;; 						  ,now)
;; 				  t))
;; 		       .root)
;; 	      :sort-groups reorg-string<
;; 	      :children (( :group (when (member .todo '("TASK"
;; 							"DELEGATED"
;; 							"WAITING"))
;; 				    "TASKS")
;; 			   :sort-groups (lambda (a b)
;; 					  (reorg--sort-by-list a b
;; 							       '("TASK"
;; 								 "DELEGATED"
;; 								 "WAITING")))
;; 			   :sort-results ((.priority . string<)
;; 					  (.todo . string<)
;; 					  (.headline . reorg-string<)))
;; 			 ( :group (when (and (or .ts-deadline
;; 						 .ts-active-first
;; 						 .ts-scheduled)
;; 					     ;; (org-string>= (reorg-org--format-time-string
;; 					     ;; 		      .ts-single
;; 					     ;; 		      "%Y-%m-%d")
;; 					     ;; 		     ,now)
;; 					     (not (member .todo '("TASK"
;; 								  "DONE"
;; 								  "DELEGATED"
;; 								  "WAITING"))))
;; 				    "CALENDAR")
;; 			   :sort-results ((.ts-single . string<)))
;; 			 ( :group (when (equal "_NOTES_" .headline) "")
;; 			   :format-results (.stars "  NOTES"))))))
	 
;; 	 ( :group "Prospective cases"
;; 	   :children (( :group (when (and (or 
;; 					   (member .todo '("TASK"
;; 							   "DELEGATED"
;; 							   "EVENT"
;; 							   "OPP_DUE"
;; 							   "WAITING"
;; 							   "DEADLINE"))
;; 					   ;; .ts-single)

;; 					   ;; (and .ts-single
;; 					   ;;      (org-string>= (reorg-org--format-time-string
;; 					   ;; 		      .ts-single
;; 					   ;; 		      "%Y-%m-%d")
;; 					   ;; 		     ,now))
;; 					   (string= .headline "_NOTES_"))
;; 					  (member "prospective" .tag-list))
;; 				 .root)
;; 			:sort-groups reorg-string<
;; 			:children (( :group (when (member .todo '("TASK"
;; 								  "DELEGATED"
;; 								  "WAITING"))
;; 					      "TASKS")
;; 				     :sort-groups (lambda (a b)
;; 						    (reorg--sort-by-list a b
;; 									 '("TASK"
;; 									   "DELEGATED"
;; 									   "WAITING")))
;; 				     :sort-results ((.priority . string<)
;; 						    (.todo . string<)
;; 						    (.headline . reorg-string<)))
;; 				   ( :group (when (and (or .ts-deadline
;; 							   .ts-active-first
;; 							   .ts-scheduled)

;; 						       ;; (org-string>= (reorg-org--format-time-string
;; 						       ;; 		      .ts-single
;; 						       ;; 		      "%Y-%m-%d")
;; 						       ;; 		     ,now)
;; 						       (not (member .todo '("TASK"
;; 									    "DONE"
;; 									    "DELEGATED"
;; 									    "WAITING"))))
;; 					      "CALENDAR")
;; 				     :sort-results ((.ts-single . string<)))
;; 				   ( :group (when (equal "_NOTES_" .headline) "")
;; 				     :format-results (.stars "  NOTES"))))))
;; 	 ( :group (when (and (member .todo '("TASK"
;; 					     "DELEGATED"
;; 					     "WAITING"))
;; 			     .property.delegated)
;; 		    "Delegations")
;; 	   :format-results ((s-pad-right 5 " " .priority)
;; 			    (s-pad-right 10 " " .todo)
;; 			    (s-pad-right 20 " " .category-inherited)
;; 			    " "
;; 			    .headline)
;; 	   :children (( :group .delegated
;; 			:sort-results ((.category-inherited . reorg-string<)
;; 				       (.priority . reorg-string<)
;; 				       (.headline . reorg-string<))
;; 			:sort-groups reorg-string<)))
;; 	 ( :group (when (and (member .todo '("TASK"
;; 					     "DELEGATED"
;; 					     ))
;; 			     (if .ts-single
;; 				 (org-string>= (reorg-org--format-time-string
;; 						.ts-single
;; 						"%Y-%m-%d")
;; 					       ,now)
;; 			       t))
;; 		    "Priorities")
;; 	   :sort-results ((.category-inherited . reorg-string<)
;; 			  (.todo . (lambda (a b)
;; 				     (reorg--sort-by-list a
;; 							  b
;; 							  '("TASK"
;; 							    "DELEGATED"
;; 							    "WAITING")))))
;; 	   :format-results ((s-pad-right 10 " " .todo)
;; 			    (s-pad-right 15 " " .category-inherited)
;; 			    (if .ts-single
;; 				(s-pad-right 50 "." .headline)
;; 			      .headline)
;; 			    (when .ts-single
;; 			      (reorg-org--format-time-string
;; 			       .ts-single
;; 			       "%a, %b %d, %Y"
;; 			       "%a, %b %d, %Y at %-l:%M%p")))
;; 	   :children (( :group (when (equal .priority "A") "High"))
;; 		      ( :group (when (equal .priority "B") "Medium"))
;; 		      ( :group (when (equal .priority "C") "Low")))))))))

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
