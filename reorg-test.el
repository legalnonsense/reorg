;;; -*- lexical-binding: t; -*-

(defun xxx-reorg-test-1 ()
  (interactive)
  (reorg-open-sidebar '( :group "Reorg test"
			 :children (( :group (when-let ((legs .property.legs)
							(legs (string-to-number legs)))
					       (when (> legs 4)
						 (format "Num legs: %d" legs)))
				      ;; :sort-getter (lambda (x) (string-to-number (s-trim (cadr (s-split ":" (car x))))))
				      ;; :sort (lambda (a b)
				      ;; 	      (< a b))
				      :sort-results ((.headline . string>)))

				    ( :group (when-let ((predator .property.predator))
					       (if (string= "1" predator)
						   "Predator"
						 "Non-predator"))
				      :children (( :group (when-let ((eggs .property.eggs))
							    (if (string= "1" eggs)
								"Non-mammal"
							      "Mammal"))
						   :sort-results ((.property.legs . string<)))))))


		      (setq reorg-headline-format '((stars) (" ") (headline) (" ") (align-to 20) (deadline) (" ")
						    (align-to 50)
						    (property legs)))))


(defun xxx-reorg-test-3 ()
  (interactive)
  (reorg-open-sidebar '( :group "Reorg test"
			 :children (( :group (when-let ((legs .property.legs)
							(legs (string-to-number legs)))
					       (format "Num legs: %d" legs))
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
							      "Mammal")))))))))

