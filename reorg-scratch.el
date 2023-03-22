;; -*- lexical-binding: t; -*-

(defun reorg--get-parent-id (&optional data)
  "go to next sibing same group"
  (let* ((id-path (reverse (if data (alist-get 'id-path data)
			     (reorg--get-prop 'id-path))))
	 (parent (cadr id-path)))
    parent))

(defun reorg--goto-group-and-id (id group-id)
  ""
  (let ((point (point)))
    (goto-char (point-min))
    (cl-loop when (and (equal id
			      (reorg--get-prop 'id))
		       (equal group-id
			      (reorg--get-prop 'group-id)))
	     return t
	     while (reorg--goto-next-heading)
	     finally (progn (goto-char point) nil))))

(defun reorg--goto-first-group-member (data)
  ""
  (let ((parent-id (reorg--get-parent-id data))
	(group-id (alist-get 'group-id data)))
    (goto-char (point-min))
    (cl-loop when (and (equal parent-id
			      (reorg--get-parent-id))
		       (equal group-id
			      (reorg--get-prop 'group-id)))
	     return t
	     while (reorg--goto-next-branch))))

(defun reorg--goto-next-sibling-same-group (&optional data)
  ""
  (interactive)
  (reorg--goto-next-prop 'parent-id (or data
					(reorg--get-prop))
			 nil
			 (lambda (a b)
			   (equal (reorg--get-parent-id a)
				  (reorg--get-parent-id b)))))

(defun reorg--find-header-location-within-groups (header-string)
  "Find the location of HEADER-STRING in the current outline."
  (reorg--goto-first-group-member
   (get-text-property 0 'reorg-data header-string))
  (let-alist (get-text-property 0 'reorg-data header-string)
    (if .sort-group
	(cl-loop with point = (point)
		 if (equal .branch-name
			   (reorg--get-prop 'branch-name))
		 return (point)
		 else if (funcall .sort-group
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

(defun reorg--find-leaf-location (leaf-string &optional result-sorters)
  "find the location for LEAF-DATA among the current leaves. put the
point where the leaf should be inserted (ie, insert before)"
  ;; goto the first leaf if at a branch 
  (unless (eq 'leaf (reorg--get-prop 'reorg-field-type))
    (if (reorg--goto-first-leaf)
	(when-let ((result-sorters
		    (or result-sorters
			(save-excursion 
			  (reorg--goto-parent)
			  (reorg--get-prop 'sort-results))))) 
	  (let ((leaf-data (get-text-property 0 'reorg-data leaf-string)))
	    (cl-loop
	     with point = (point)
	     when (cl-loop for (func . pred) in result-sorters
			   unless (equal (funcall
					  `(lambda (x) (let-alist x ,func))
					  leaf-data)
					 (funcall
					  `(lambda (x) (let-alist x ,func))
					  (reorg--get-prop)))
			   return (funcall pred
					   (funcall
					    `(lambda (x) (let-alist x ,func))
					    leaf-data)
					   (funcall
					    `(lambda (x) (let-alist x ,func))
					    (reorg--get-prop))))
	     return (point)
	     while (reorg--goto-next-leaf-sibling)
	     finally (goto-char (line-beginning-position 2)))))
      (reorg--goto-next-heading))))

(defun reorg--new-insert-new (data)
  "asdf"
  (setq cursor-type 'box)
  (setq 
   data (reorg--get-group-and-sort
	 (list data)
	 reorg--current-template
	 1
	 t)
   yyy data
   data (reorg--get-all-tree-paths
	 data
	 (lambda (x)
	   (and (listp x)
		(stringp (car x))
		(eq
		 'leaf
		 (get-text-property
		  0
		  'reorg-field-type
		  (car x))))))
   zzz data)
  data)

(defun reorg--insert-new-heading (data)
  ""
  (setq data (reorg--new-insert-new data))
  (cl-loop for group in data
	   do (goto-char (point-min))
	   do (cl-loop with leaf = (car (last group))
		       with headings = (butlast group)
		       with stop = nil
		       while (not stop)
		       for heading in headings
		       for n from 0
		       when (and heading
				 (not (string= "" heading)))
		       do (let* ((props (get-text-property 0 'reorg-data heading))
				 (id (alist-get 'id props)))
			    ;; if we don't find thefirst header, then none
			    ;; of them exist 
			    (unless (reorg--goto-next-prop 'id id nil nil nil t)
			      ;; check to see if there is an existing group
			      (let ((afterp 
				     (reorg--find-header-location-within-groups heading)))
				(cl-loop for x from n to (1- (length headings))
					 do
					 (reorg--insert-header-at-point (nth x headings) afterp)
					 finally
					 (progn
					   (setq stop t)
					   (let ((afterp (reorg--at-last-leaf-p)))
					     (reorg--insert-header-at-point leaf afterp)))))))
		       finally (progn (unless stop
					(let ((afterp (reorg--find-leaf-location leaf)))
					  (reorg--insert-header-at-point leaf)))))))

(defun reorg--at-last-leaf-p ()
  "at the last leaf?"
  (save-excursion 
    (reorg--goto-next-leaf-sibling)))

(defun reorg--refresh-org-visual-outline ()
  ""
  (when-let ((beg (and org-visual-indent-mode
		       (reorg--get-parent)))
	     (end (or (reorg--get-next-parent)
		      (point-max))))
    (org-visual-indent--org-indent-add-properties beg end)))
