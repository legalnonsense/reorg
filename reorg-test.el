;;; -*- lexical-binding: t; -*-

(defun xxx-reorg-test-control-panel-10 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/org/taskmaster.org"
   :template '( :group "MEETING VIEW"
		:children (( :group .category 
			     :sort string< 
			     :sort-getter (lambda (x) (downcase x))
			     :children (( :group (when (and .todo
							    (not (string= .todo "DONE"))
							    (not (string= .todo "EVENT"))
							    (not (string= .todo "OPP_DUE"))
							    (not (string= .todo "DEADLINE")))
						   "TASKS" )
					  :format-string (concat " " (s-pad-right 10 " " .todo) .headline)
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
					  :format-string (concat
							  " "
							  .ts-type
							  " "
							  (s-pad-right 50
								       "."
								       (s-truncate 40 .headline "..."))
							  .ts)
					  :sort-results ((.ts . string<)))
					( :group (when (string= .headline "_NOTES_")
						   "Progress Notes"))))))))


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







