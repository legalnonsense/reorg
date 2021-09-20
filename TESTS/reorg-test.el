;;; -*- lexical-binding: t; -*-


(setq reorg-test-template
      '( :group "Clients"
	 :children (( :group (lambda (x) (plist-get x :category))))))

(reorg-open-sidebar '( :group (lambda (x) (if (string= "task" (plist-get x :todo)) "TASKS" "NO TODO"))
		       :children (( :group (lambda (x) (plist-get x :category))))))

(defun xxx ()
  (interactive)
  (let ((reorg-headline-format '(:stars :todo " " :headline-only " " :timestamp)))
    (reorg-open-sidebar '( :group "Parent"
			   :children (( :group "TODO states"
					:children (( :group (lambda (x) (plist-get x :todo))
						     :pre-transformer (lambda (x) (plist-put x :used t)))))
				      ( :group "BY CATEGORY"
					:children (( :group (lambda (x) (plist-get x :category))
						     :post-transformer (lambda (x) (when (plist-get x :used)
										     (plist-put x :clone t)))))))))))

(defun reorg-elgantt-test-file ()
  (interactive)
  (let ((reorg-headline-format '(:stars :deadline (:todo 200) (:headline-only 100))))
    (reorg-open-sidebar '( :group "Elgantt test"
			   :children (( :group "TODO states"
					:children (( :group (lambda (x) (plist-get x :todo)))))
				      ( :group "By hashtag"
					:children (( :group (lambda (x) (-first (lambda (x) (s-starts-with-p "#" x))
										(plist-get x :tags-all)))
						     :sort-results (lambda (a b)
								     (string>
								      (plist-get a :deadline)
								      (plist-get b :deadline)))))))))))





(insert (reorg--create-headline-string xxx '(:stars :todo :deadline :headline-only :tags) 1))
(insert (reorg--create-headline-string xxx '(:stars :todoz :deadline :headline-only :tags) 1))
(insert (reorg--create-headline-string xxx '(:stars (:todo 10) "Deadline: " (:deadline 50) :headline-only :tags) 1))
*  Deadline: <2020-03-13 Fri> Design space suit 




(setq yyy (make-overlay 1647 1657))
(overlay-put yyy 'face '(:box t))
(setq reorg-edits--current-field-overlay nil)
(setq reorg-edits--current-field-overlay (make-overlay 1 2 ))
 (overlay-put reorg-edits--current-field-overlay 'face '(:box t))
