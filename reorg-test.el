;;; -*- lexical-binding: t; -*-

(defun xxx-reorg-test-4 ()
  (interactive)
  (reorg-open-sidebar
   :file "~/.emacs.d/lisp/reorg/TESTS/zoo-short.org"
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

(ts>= (ts-now) (ts-parse-org "<2021-10-15 Fri 14:00>"))


