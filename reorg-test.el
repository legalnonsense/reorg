;;; -*- lexical-binding: t; -*-

(defun xxx-reorg-test-3 ()
  (interactive)
  (reorg-open-sidebar (setq xxx-reorg-test-3-template '( :group "Reorg test"
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
								      :sort string<))))))

(defun xxx-reorg-test-4 ()
  (interactive)
  (reorg-open-sidebar
   :files "~/.emacs.d/lisp/reorg/short-zoo.org"

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
   :template '( :group .deadline)))
;; :children (( :group (when .todo "Tasks")
;; 	     :format-string ((stars) (" ") (todo) (align-to 20) (headline)))
;; 	   ( :group (when (or .deadline .timestamp)
;; 		      "Caldendar")
;; 	     :fortmat-string ((stars) (" ") (todo) (align-to 20) (deadline) (align-to 40) (timestamp)))))))


(with-current-buffer (find-file-noselect "~/legal/Dropbox/DropsyncFiles/taskclone.org")
  (--> (reorg--map-entries)
       (setq xxx it)
       (reorg--group-and-sort it template)))

(reorg--group-and-sort xxx '( :group (if .todo .todo "no todo")))
