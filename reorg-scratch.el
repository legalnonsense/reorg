;; -*- lexical-binding: t; -*-

;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

'((( :headline "* By client\n"
     :id "b92c491098343a3ee048af2ae7d5eebaBy client"
     :group-id "b92c491098343a3ee048af2ae7d5eeba"
     :parent-id nil)
   ( :headline "** Abraha, Seifu\n"
     :id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu"
     :group-id "8c7bd4ee270a7b45899af20b5cb0f702"
     :parent-id "b92c491098343a3ee048af2ae7d5eebaBy client")
   ( :headline "*** Tasks\n"
     :id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu4297c1f66ff67d8df8962821c7d8d81eTasks"
     :group-id "4297c1f66ff67d8df8962821c7d8d81e"
     :parent-id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu")
   ( :headline "⚡ TASK            this is a test!\n"
     :id "d51e0c82-2005-4e7a-90d9-eb3024c4276a"
     :group-id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu4297c1f66ff67d8df8962821c7d8d81eTasks"
     :parent-id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu4297c1f66ff67d8df8962821c7d8d81eTasks"))
  (( :headline "* By client\n"
     :id "b92c491098343a3ee048af2ae7d5eebaBy client"
     :group-id "b92c491098343a3ee048af2ae7d5eeba"
     :parent-id nil)
   ( :headline "** Abraha, Seifu\n"
     :id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu"
     :group-id "8c7bd4ee270a7b45899af20b5cb0f702"
     :parent-id "b92c491098343a3ee048af2ae7d5eebaBy client")
   ( :headline "*** Calendar\n"
     :id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu235fe0000369a4c2d171b16de8008479Calendar"
     :group-id "235fe0000369a4c2d171b16de8008479"
     :parent-id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu")
   ( :headline " Thu, Feb 23                    this is a test!\n"
     :id "d51e0c82-2005-4e7a-90d9-eb3024c4276a"
     :group-id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu235fe0000369a4c2d171b16de8008479Calendar"
     :parent-id "b92c491098343a3ee048af2ae7d5eebaBy client8c7bd4ee270a7b45899af20b5cb0f702Abraha, Seifu235fe0000369a4c2d171b16de8008479Calendar"))
  (( :headline "* Calendar\n"
     :id "4345f7664a902987ddd4372b5789b9bcCalendar"
     :group-id "4345f7664a902987ddd4372b5789b9bc"
     :parent-id nil)
   ( :headline "** 2023\n"
     :id "4345f7664a902987ddd4372b5789b9bcCalendar70fa3bddc1a414cff7cf6a31c3b481a32023"
     :group-id "70fa3bddc1a414cff7cf6a31c3b481a3"
     :parent-id "4345f7664a902987ddd4372b5789b9bcCalendar")
   ( :headline "*** February\n"
     :id "4345f7664a902987ddd4372b5789b9bcCalendar70fa3bddc1a414cff7cf6a31c3b481a32023ad56c1a9eb4e272501b3d49a683ea2a9February"
     :group-id "ad56c1a9eb4e272501b3d49a683ea2a9"
     :parent-id "4345f7664a902987ddd4372b5789b9bcCalendar70fa3bddc1a414cff7cf6a31c3b481a32023")
   ( :headline "**** 23 Thursday    Abraha, Seifu       this is a test!\n"
     :id "d51e0c82-2005-4e7a-90d9-eb3024c4276a"
     :group-id "4345f7664a902987ddd4372b5789b9bcCalendar70fa3bddc1a414cff7cf6a31c3b481a32023ad56c1a9eb4e272501b3d49a683ea2a9February"
     :parent-id "4345f7664a902987ddd4372b5789b9bcCalendar70fa3bddc1a414cff7cf6a31c3b481a32023ad56c1a9eb4e272501b3d49a683ea2a9February")))


(setq yyy (-->
	   (reorg--get-group-and-sort (list xxx) reorg--current-template 1 t)
	   (reorg--get-all-tree-paths it (lambda (x)
					   (and (listp x)
						(stringp (car x))
						(eq
						 'leaf
						 (get-text-property
						  0
						  'reorg-field-type
						  (car x))))))
	   ;; (reorg--walk-tree it (lambda (x)
	   ;; 			  (let ((props (get-text-property 1 'reorg-data x)))
	   ;; 			    (list :headline (org-no-properties (seq-copy x))
	   ;; 				  :id (alist-get 'id props)
	   ;; 				  :group-id (alist-get 'group-id props)
	   ;; 				  :parent-id (alist-get 'parent-id props)))))
	   ))
(cadr (caddr yyy))

;; insert leaf
;;; find leaf location 
;; insert header(s)
;;; find header location
(defun reorg--find-header-location-within-groups (header-string)
  "assume the point is on the first header in the group"
  (let-alist (get-text-property 0 'reorg-data header-string)
    (if .sort-groups
	(cl-loop with point = (point)
		 if (equal .branch-name
			   (reorg--get-prop 'branch-name))
		 return (point)
		 else if (funcall .sort-groups
				  .branch-name
				  (reorg--get-prop 'branch-name))
		 return nil
		 while (reorg--goto-next-sibling-same-group
			(get-text-property 0 'reorg-data header-string))
		 finally return (progn (goto-char point)
				       nil))
      (cl-loop with point = (point)
	       when (equal .branch-name
			   (reorg--get-prop 'branch-name))
	       return t
	       while (reorg--goto-next-sibling-same-group
		      (get-text-property 0 'reorg-data header-string))
	       finally return (progn (goto-char point)
				     nil)))))

(defun xxx (&optional first)
  (goto-char (point-min))
  (cl-loop for group in yyy
	   do (cl-loop for heading in group
		       unless (reorg--goto-next-prop
			       'id
			       (alist-get
				'id 
				(get-text-property
				 1
				 'reorg-data
				 heading)))
		       do (unless first
			    (reorg--goto-next-child
			    (reorg--find-header-location-within-groups heading)
		       and do 
		       
