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
	(group-id (alist-get 'group-id data))
	(point (point)))
    (goto-char (point-min))
    (cl-loop when (and (equal parent-id
			      (reorg--get-parent-id))
		       (equal group-id
			      (reorg--get-prop 'group-id)))
	     return t
	     while (reorg--goto-next-branch)
	     finally return (progn (goto-char point)
				   nil))))

(defun reorg--goto-next-sibling-same-group (&optional data)
  ""
  (interactive)
  (let ((parent-id (if data 
		       (reorg--get-parent-id data)
		     (reorg--get-parent-id (reorg--get-prop))))
	(group-id (if data
		      (alist-get 'group-id data)
		    (reorg--get-prop 'group-id)))
	(point (point)))
    (when (reorg--goto-next-sibling)
      (if (and (equal parent-id
		      (reorg--get-parent-id))
	       (equal group-id
		      (reorg--get-prop 'group-id)))
	  (point)
	(goto-char point)
	(run-hooks 'reorg--navigation-hook)
	nil))))

(defun reorg--goto-last-leaf-depth-first ()
  "goto last leaf of current tree"
  ;; this function doesn't seem to work the
  ;; way it was intended 
  (while (reorg--goto-next-leaf-sibling)))

(defun reorg--find-header-location (header-string)
  "Find the location of HEADER-STRING in the current outline."
  (setq header-string (if (stringp header-string)
			  (get-text-property 0 'reorg-data header-string)
			header-string))
  (when (reorg--goto-first-group-member header-string)
    (let-alist header-string
      (if .sort-group
	  (cl-loop with point = (point)
		   when (funcall .sort-group
				 .branch-name
				 (reorg--get-prop 'branch-name))
		   return (point)
		   while (reorg--goto-next-sibling-same-group
			  header-string)
		   finally return (point))

	(cl-loop with point = (point)
		 when (equal .branch-name
			     (reorg--get-prop 'branch-name))
		 return t
		 while (reorg--goto-next-sibling-same-group
			header-string)
		 finally return (reorg--goto-last-leaf)
		 
		 ;; (progn (goto-char point)
		 ;;        (forward-line)
		 ;;        nil)
		 ))))))

(defun reorg--find-leaf-location (leaf-string &optional result-sorters)
  "find the location for LEAF-DATA among the current leaves. put the
point where the leaf should be inserted (ie, insert before)"
  ;; goto the first leaf if at a branch
  ;; (push leaf-string xxx)
  (unless (eq 'leaf (reorg--get-prop 'reorg-field-type))
    (when-let ((result-sorters
		(or result-sorters
		    (reorg--get-prop 'sort-results))))
      ;; (reorg--goto-first-leaf)
      (reorg--goto-next-child)
      (let ((leaf-data (if (stringp leaf-string)
			   (get-text-property 0 'reorg-data leaf-string)
			 leaf-string)))
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
	 while (reorg--goto-next-sibling-same-group)
	 finally return (point))))))

(defun reorg--insert-new-heading (data)
  ""
  (setq
   xxx nil
   data (reorg--get-group-and-sort
	 (list data)
	 reorg--current-template
	 1
	 t)
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
  (setq cursor-type 'box)
  (cl-loop for group in data
	   do (goto-char (point-min))
	   and do (cl-loop
		   with leaf = (car (last group))
		   with headings = (butlast group)
		   with stop = nil
		   while (not stop)
		   for heading in headings
		   for n from 0
		   when (and heading
			     (not (string= "" heading)))
		   do (let* ((props (get-text-property 0 'reorg-data heading))
			     (id (alist-get 'id props)))
			(unless (reorg--goto-next-prop 'id id nil nil nil t)
			  ;; THE ERROR IS HERE 
			  (unless (reorg--find-header-location heading)
			    (forward-line))
			  (cl-loop for x from n to (1- (length headings))
				   do
				   (reorg--insert-header-at-point
				    (nth x headings))
				   and do (forward-line)
				   finally
				   (progn
				     (setq stop t)
				     (reorg--insert-header-at-point
				      leaf)))))
		   finally (unless stop
			     (reorg--find-leaf-location leaf)
			     (reorg--insert-header-at-point leaf t)))))




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

;; check to see if the last header exists
;; if so, find the leaf location (reorg--traverse-leaf-group)
;; find the first header
;; if it doesn't exist, find it's location in the header groups (reorg--traverse-header-group)
;; insert all remaining headers and leaf
;; if it exists, loop for next header

zzz
