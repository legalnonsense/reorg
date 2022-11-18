;; -*- lexical-binding: t; -*-

;;; BRANCH 1

'((reorg-level . 1)
  (branch-name . "MEETING VIEW")
  (headline . "MEETING VIEW")
  (reorg-branch . t)
  (result-sorters)
  (grouper-list lambda
		(x)
		(let-alist x "MEETING VIEW"))
  (branch-predicate lambda
		    (x)
		    (let-alist x "MEETING VIEW"))
  (branch-result . "MEETING VIEW")
  (grouper-list-results . "MEETING VIEW")
  (format-string \.stars " " \.headline)
  (result-sorters)
  (template :group "MEETING VIEW" :format-string
	    (\.stars " " \.headline)
	    :children
	    ((:group "CASE LIST" :children
		     ((:group \.category-inherited :sort string< :sort-getter
			      (lambda
				(x)
				(downcase x))
			      :children
			      ((:group
				(when
				    (and \.todo
					 (not
					  (string= \.todo "DONE"))
					 (not
					  (string= \.todo "EVENT"))
					 (not
					  (string= \.todo "OPP_DUE"))
					 (not
					  (string= \.todo "DEADLINE")))
				  "TASKS")
				:format-string
				(\.stars " "
				 (s-pad-right 10 " " \.todo)
				 " " \.headline)
				:sort-results
				((\.todo . string<)
				 ((downcase \.headline)
				  . string<)))
			       (:group
				(when
				    (and
				     (or
				      (string= \.todo "DEADLINE")
				      (string= \.todo "EVENT")
				      (string= \.todo "OPP_DUE"))
				     (or \.timestamp \.deadline \.scheduled))
				  "CALENDAR")
				:format-string
				(\.ts-type " "
				 (s-pad-right 50 "."
					      (s-truncate 40 \.headline "..."))
				 \.ts)
				:sort-results
				((\.ts . string<)))
			       (:group
				(when
				    (string= \.headline "_NOTES_")
				  "Progress Notes")
				:format-string
				(\.stars " Notes"))))))
	     (:group "Date tree" :children
		     ((:group
		       (when-let
			   ((time
			     (or \.timestamp \.deadline \.scheduled \.timestamp-ia)))
			 (number-to-string
			  (ts-year
			   (ts-parse time))))
		       :sort string< :sort-getter identity :children
		       ((:group
			 (when-let
			     ((time
			       (or \.timestamp \.deadline \.scheduled \.timestamp-ia)))
			   (concat " "
				   (ts-month-name
				    (ts-parse time))
				   " "
				   (number-to-string
				    (ts-year
				     (ts-parse time)))))
			 :sort string< :sort-getter identity :children
			 ((:group
			   (when-let
			       ((time
				 (or \.timestamp \.deadline \.scheduled \.timestamp-ia)))
			     (concat
			      (ts-day-name
			       (ts-parse time))
			      " "
			      (s-pad-left 2 "0"
					  (number-to-string
					   (ts-day
					    (ts-parse time))))
			      " "
			      (ts-month-name
			       (ts-parse time))
			      ", "
			      (number-to-string
			       (ts-year
				(ts-parse time)))))
			   :sort string< :sort-getter identity)))))))))
  (children
   (:group "CASE LIST" :children
	   ((:group \.category-inherited :sort string< :sort-getter
		    (lambda
		      (x)
		      (downcase x))
		    :children
		    ((:group
		      (when
			  (and \.todo
			       (not
				(string= \.todo "DONE"))
			       (not
				(string= \.todo "EVENT"))
			       (not
				(string= \.todo "OPP_DUE"))
			       (not
				(string= \.todo "DEADLINE")))
			"TASKS")
		      :format-string
		      (\.stars " "
		       (s-pad-right 10 " " \.todo)
		       " " \.headline)
		      :sort-results
		      ((\.todo . string<)
		       ((downcase \.headline)
			. string<)))
		     (:group
		      (when
			  (and
			   (or
			    (string= \.todo "DEADLINE")
			    (string= \.todo "EVENT")
			    (string= \.todo "OPP_DUE"))
			   (or \.timestamp \.deadline \.scheduled))
			"CALENDAR")
		      :format-string
		      (\.ts-type " "
		       (s-pad-right 50 "."
				    (s-truncate 40 \.headline "..."))
		       \.ts)
		      :sort-results
		      ((\.ts . string<)))
		     (:group
		      (when
			  (string= \.headline "_NOTES_")
			"Progress Notes")
		      :format-string
		      (\.stars " Notes"))))))
   (:group "Date tree" :children
	   ((:group
	     (when-let
		 ((time
		   (or \.timestamp \.deadline \.scheduled \.timestamp-ia)))
	       (number-to-string
		(ts-year
		 (ts-parse time))))
	     :sort string< :sort-getter identity :children
	     ((:group
	       (when-let
		   ((time
		     (or \.timestamp \.deadline \.scheduled \.timestamp-ia)))
		 (concat " "
			 (ts-month-name
			  (ts-parse time))
			 " "
			 (number-to-string
			  (ts-year
			   (ts-parse time)))))
	       :sort string< :sort-getter identity :children
	       ((:group
		 (when-let
		     ((time
		       (or \.timestamp \.deadline \.scheduled \.timestamp-ia)))
		   (concat
		    (ts-day-name
		     (ts-parse time))
		    " "
		    (s-pad-left 2 "0"
				(number-to-string
				 (ts-day
				  (ts-parse time))))
		    " "
		    (ts-month-name
		     (ts-parse time))
		    ", "
		    (number-to-string
		     (ts-year
		      (ts-parse time)))))
		 :sort string< :sort-getter identity))))))))
  (branch-sorter)
  (branch-sort-getter . car)
  (reorg-level . 1)
  (group-id . "0088112e9cc5103dcc0e6aa8f69a5c34")
  (id . "5d857dde9542562e419ac51c1a747d9c")
  (reorg-class)
  (reorg-level . 1))


;;; BRANCH 2

'((reorg-level . 2)
  (branch-name . "CASE LIST")
  (headline . "CASE LIST")
  (reorg-branch . t)
  (result-sorters)
  (grouper-list lambda
		(x)
		(let-alist x "CASE LIST"))
  (branch-predicate lambda
		    (x)
		    (let-alist x "CASE LIST"))
  (branch-result . "CASE LIST")
  (grouper-list-results . "CASE LIST")
  (format-string \.stars " " \.headline)
  (result-sorters)
  (template :group "CASE LIST" :children
	    ((:group \.category-inherited :sort string< :sort-getter
		     (lambda
		       (x)
		       (downcase x))
		     :children
		     ((:group
		       (when
			   (and \.todo
				(not
				 (string= \.todo "DONE"))
				(not
				 (string= \.todo "EVENT"))
				(not
				 (string= \.todo "OPP_DUE"))
				(not
				 (string= \.todo "DEADLINE")))
			 "TASKS")
		       :format-string
		       (\.stars " "
			(s-pad-right 10 " " \.todo)
			" " \.headline)
		       :sort-results
		       ((\.todo . string<)
			((downcase \.headline)
			 . string<)))
		      (:group
		       (when
			   (and
			    (or
			     (string= \.todo "DEADLINE")
			     (string= \.todo "EVENT")
			     (string= \.todo "OPP_DUE"))
			    (or \.timestamp \.deadline \.scheduled))
			 "CALENDAR")
		       :format-string
		       (\.ts-type " "
			(s-pad-right 50 "."
				     (s-truncate 40 \.headline "..."))
			\.ts)
		       :sort-results
		       ((\.ts . string<)))
		      (:group
		       (when
			   (string= \.headline "_NOTES_")
			 "Progress Notes")
		       :format-string
		       (\.stars " Notes"))))))
  (children
   (:group \.category-inherited :sort string< :sort-getter
	   (lambda
	     (x)
	     (downcase x))
	   :children
	   ((:group
	     (when
		 (and \.todo
		      (not
		       (string= \.todo "DONE"))
		      (not
		       (string= \.todo "EVENT"))
		      (not
		       (string= \.todo "OPP_DUE"))
		      (not
		       (string= \.todo "DEADLINE")))
	       "TASKS")
	     :format-string
	     (\.stars " "
	      (s-pad-right 10 " " \.todo)
	      " " \.headline)
	     :sort-results
	     ((\.todo . string<)
	      ((downcase \.headline)
	       . string<)))
	    (:group
	     (when
		 (and
		  (or
		   (string= \.todo "DEADLINE")
		   (string= \.todo "EVENT")
		   (string= \.todo "OPP_DUE"))
		  (or \.timestamp \.deadline \.scheduled))
	       "CALENDAR")
	     :format-string
	     (\.ts-type " "
	      (s-pad-right 50 "."
			   (s-truncate 40 \.headline "..."))
	      \.ts)
	     :sort-results
	     ((\.ts . string<)))
	    (:group
	     (when
		 (string= \.headline "_NOTES_")
	       "Progress Notes")
	     :format-string
	     (\.stars " Notes")))))
  (branch-sorter)
  (branch-sort-getter . car)
  (reorg-level . 2)
  (group-id . "e985750d86e6e0e7069430544d364bbd")
  (id . "94e1866b0810743cddbba34de2bb9795")
  (reorg-class)
  (reorg-level . 2))

;;; BRANCH 3

'((reorg-level . 3)
  (branch-name . "ABC")
  (headline . "ABC")
  (reorg-branch . t)
  (result-sorters)
  (grouper-list lambda
		(x)
		(let-alist x \.category-inherited))
  (branch-predicate lambda
		    (x)
		    (let-alist x \.category-inherited))
  (branch-result . "ABC")
  (grouper-list-results . "ABC")
  (format-string \.stars " " \.headline)
  (result-sorters)
  (template :group \.category-inherited :sort string< :sort-getter
	    (lambda
	      (x)
	      (downcase x))
	    :children
	    ((:group
	      (when
		  (and \.todo
		       (not
			(string= \.todo "DONE"))
		       (not
			(string= \.todo "EVENT"))
		       (not
			(string= \.todo "OPP_DUE"))
		       (not
			(string= \.todo "DEADLINE")))
		"TASKS")
	      :format-string
	      (\.stars " "
	       (s-pad-right 10 " " \.todo)
	       " " \.headline)
	      :sort-results
	      ((\.todo . string<)
	       ((downcase \.headline)
		. string<)))
	     (:group
	      (when
		  (and
		   (or
		    (string= \.todo "DEADLINE")
		    (string= \.todo "EVENT")
		    (string= \.todo "OPP_DUE"))
		   (or \.timestamp \.deadline \.scheduled))
		"CALENDAR")
	      :format-string
	      (\.ts-type " "
	       (s-pad-right 50 "."
			    (s-truncate 40 \.headline "..."))
	       \.ts)
	      :sort-results
	      ((\.ts . string<)))
	     (:group
	      (when
		  (string= \.headline "_NOTES_")
		"Progress Notes")
	      :format-string
	      (\.stars " Notes"))))
  (children
   (:group
    (when
	(and \.todo
	     (not
	      (string= \.todo "DONE"))
	     (not
	      (string= \.todo "EVENT"))
	     (not
	      (string= \.todo "OPP_DUE"))
	     (not
	      (string= \.todo "DEADLINE")))
      "TASKS")
    :format-string
    (\.stars " "
     (s-pad-right 10 " " \.todo)
     " " \.headline)
    :sort-results
    ((\.todo . string<)
     ((downcase \.headline)
      . string<)))
   (:group
    (when
	(and
	 (or
	  (string= \.todo "DEADLINE")
	  (string= \.todo "EVENT")
	  (string= \.todo "OPP_DUE"))
	 (or \.timestamp \.deadline \.scheduled))
      "CALENDAR")
    :format-string
    (\.ts-type " "
     (s-pad-right 50 "."
		  (s-truncate 40 \.headline "..."))
     \.ts)
    :sort-results
    ((\.ts . string<)))
   (:group
    (when
	(string= \.headline "_NOTES_")
      "Progress Notes")
    :format-string
    (\.stars " Notes")))
  (branch-sorter . string<)
  (branch-sort-getter lambda
		      (x)
		      (downcase x))
  (reorg-level . 3)
  (group-id . "329c59a51dc1197824bc4d176efc7093")
  (id . "4edc4514bb21233b1f7fd5e2b2817540")
  (reorg-class)
  (reorg-level . 3))

;;; BRANCH 4

'((reorg-level . 4)
  (branch-name . "Progress Notes")
  (headline . "Progress Notes")
  (reorg-branch . t)
  (result-sorters)
  (grouper-list lambda
		(x)
		(let-alist x
		  (when
		      (string= \.headline "_NOTES_")
		    "Progress Notes")))
  (branch-predicate lambda
		    (x)
		    (let-alist x
		      (when
			  (string= \.headline "_NOTES_")
			"Progress Notes")))
  (branch-result . "Progress Notes")
  (grouper-list-results . "Progress Notes")
  (format-string \.stars " Notes")
  (result-sorters)
  (template :group
	    (when
		(string= \.headline "_NOTES_")
	      "Progress Notes")
	    :format-string
	    (\.stars " Notes"))
  (children)
  (branch-sorter)
  (branch-sort-getter . car)
  (reorg-level . 4)
  (group-id . "758619f7c4895a955e2b2128a7200e87")
  (id . "b817754d7ac805f713f49f2a41adf3fd")
  (reorg-class)
  (reorg-level . 4))

;;; LEAF

'((reorg-level . 5)
  (group-id . "758619f7c4895a955e2b2128a7200e87")
  (reorg-level . 5)
  (reorg-stars . 5)
  (org-level . 2)
  (childrenp . t)
  (at-name)
  (root-ts-inactive)
  (root . "All Business (Ishmael)")
  (org-level . 2)
  (order . 53830)
  (buffer . #<buffer tmp.org>)
  (buffer-name . "tmp.org")
  (filename . "/home/jeff/tmp/tmp.org")
  (category . "ABC")
  (category-inherited . "ABC")
  (id . "936c6d13-0a34-4618-b882-1e23ad72b89c")
  (timestamp-range)
  (timestamp-ia-range)
  (timestamp-ia . "[2021-12-13 Mon 14:51]")
  (link-file-path)
  (link-file-name)
  (link)
  (links)
  (timestamp)
  (todo)
  (tags . "")
  (headline . "_NOTES_")
  (scheduled)
  (deadline)
  (body . "" )
  (priority . "B")
  (timestamp-all)
  (ts-type)
  (ts)
  (headline . "_NOTES_")
  (tag-list)
  (class . org)
  (reorg-class . org)
  (reorg-level . 5))

;; does group exist? if so, go to it. If not, create it.

;;; FUNCS

(defun reorg--find-group-by-id (id)
  "Goto the first appearance of ID."
  (reorg--get-next-prop 'group-id id))

(defun reorg--insert (entry)
  (cond ((reorg--goto-next-prop 'group-id (alist-get 'group-id entry))
	 ;; then check branch sorters
	 (if (alist-get 'branch-sorter entry)
	     (cl-loop when
		      (funcall (alist-get 'branch-sorter entry)
			       (funcall
				(alist-get 'branch-sort-getter
					   (alist-get 'branch-name entry)))
			       (funcall
				(alist-get 'branch-sort-getter
					   (buffer-substring
					    (line-beginning-position)
					    (line-beginning-position 2)))))
		      return t
		      while (reorg--goto-next-prop 'group-id (alist-get 'group-id entry))
		      finally return nil)))))

(setq xxx-data (list 
		(list (cons 'a 1)
		      (cons 'b 2)
		      (cons 'c 3))
		(list (cons 'a 3)
		      (cons 'b 2)
		      (cons 'c 3))
		(list (cons 'a 2)
		      (cons 'b 2)
		      (cons 'c 3))
		(list (cons 'a 4)
		      (cons 'b 2)
		      (cons 'c 3))
		(list (cons 'a 5)
		      (cons 'b 66)
		      (cons 'c 3))))
(setq xxx-template '( :group (oddp .a)
		      :children (( :group (evenp .b)))))
(cl-labels
    ((recurse
      (data template &optional result-sorters format-string (level 1))
      ;; process template 
      (let ((group (plist-get template :group))
	    (children (plist-get template :children))
	    (class (plist-get template :class))
	    (header-sort (plist-get template :sort))
	    (header-sort-getter (plist-get template :sort-getter))
	    (format-string (or (plist-get template :format-string)
			       format-string
			       reorg-headline-format))
	    (result-sorters
	     (when-let ((sorters (plist-get template :sort-results)))
	       (setq result-sorters
		     (append result-sorters 
			     (cl-loop for (form . pred) in sorters
				      collect (cons `(lambda (x)
						       (let-alist x
							 ,form))
						    pred))))))
	    (overrides
	     (plist-get template :format-string-overrides)))
	(cond
	 ((functionp grouper)
	  (seq-group-by grouper data))
	 ((stringp grouper)
	  (list (list grouper data)))
	 (t
	  (when-let ((at-dots (cl-delete-duplicates
			       (reorg--dot-at-search grouper)
			       :test #'equal)))
	    (setq
	     it 				 
	     (cl-loop
	      for data in it
	      append (cl-loop
		      for (_ . at-dot) in at-dots
		      if (listp (alist-get at-dot data))
		      return (cl-loop for x in (alist-get at-dot data)
				      collect (let ((ppp (copy-alist data)))
						(setf (alist-get at-dot ppp) x)
						ppp))
		      finally return data))))
	  (setq xxx grouper)
	  (->> it			      
	       (reorg--seq-group-by
		(reorg--walk-tree grouper
				  #'reorg--turn-at-dot-to-dot))
	       (seq-map (lambda (x) (list (car x) (cdr x)))))))
	(seq-filter (lambda (x) (and (not (null (car x)))
				     (not (null (cdr x)))
				     (not (null x))))
		    it)
	(cl-loop
	 for x in it
	 do (setf
	     (car x)
	     (let ((ddd (list
			 (cons 'branch-name (car x))
			 (cons 'headline (car x))
			 (cons 'reorg-branch t)
			 (cons 'result-sorters result-sorters)
			 (cons 'grouper-list `(lambda (x)
						(let-alist x
						  ,grouper)))
			 (cons 'branch-predicate `(lambda (x)
						    (let-alist x
						      ,grouper)))
			 (cons 'branch-result (car x))
			 (cons 'grouper-list-results (car x))
			 (cons 'format-string format-string)
			 (cons 'result-sorters result-sorters)
			 (cons 'template template)
			 (cons 'children children)
			 (cons 'branch-sorter heading-sorter)
			 (cons 'branch-sort-getter heading-sort-getter)
			 (cons 'reorg-level level))))
	       (append ddd
		       `((group-id . ,(md5 (with-temp-buffer
					     (insert (pp template))
					     (buffer-string))))
			 (id . ,(md5 (with-temp-buffer
				       (insert (pp ddd))
				       (buffer-string))))))))
	 
	 finally return it)
	(seq-filter (lambda (x) (and (not (null (car x)))
				     (not (null (cdr x)))
				     (not (null x))))
		    it)
	(cl-loop for x in it
		 do (setf (car x)
			  (propertize 
			   (reorg--create-headline-string (car x)
							  (copy-tree format-string)
							  level
							  overrides)
			   'reorg-headline-string
			   (reorg--create-headline-string (car x)
							  (copy-tree format-string)
							  level
							  overrides)))
		 
		 finally return it)
	(if heading-sorter
	    (seq-sort-by (lambda (y)
			   (funcall heading-sort-getter
				    (reorg--get-string-match
				     "^*+[[:space:]]+\\(.+\\)"
				     (car y)
				     1)))
			 heading-sorter
			 it)
	  it)))
     (if children
	 (progn ;; if		  
	   (cl-loop for x below (length (nth n (cdr data)))
		    do (setcdr
			(nth x (nth n (cdr data)))
			(cl-loop
			 for z below (length children)
			 collect (seq-copy
				  (cadr (nth x (nth n (cdr data))))))))
	   (cl-loop for x below (length children)
		    do (cl-loop
			for y below (length (nth n (cdr data)))
			do (doloop
			    (nth y (nth n (cdr data)))
			    (nth x children)
			    x
			    result-sorters
			    grouper-list
			    (append
			     grouper-list-results
			     (list (alist-get 'branch-value
					      (nth y (nth n (cdr data))))))
			    format-string
			    (1+ level)))))

       (progn ;; else 
	 (when result-sorters
	   (cl-loop for x below (length (nth n (cdr data)))
		    do (setf
			(cadr (nth x (nth n (cdr data))))
			(reorg--multi-sort result-sorters
					   (cadr (nth x (nth n (cdr data))))))))
	 (cl-loop
	  for x below (length (nth n (cdr data)))
	  do
	  (setf
	   (cadr (nth x (nth n (cdr data))))
	   (cl-loop
	    for each in (cadr (nth x (nth n (cdr data))))
	    collect
	    (progn (setf (alist-get 'reorg-stars each) (1+ level))
		   (push (cons 'reorg-level (1+ level)) each)
		   (push (cons 'group-id (md5 (with-temp-buffer
						(insert (pp grouper))
						(buffer-string))))
			 each)
		   (reorg--create-headline-string each
						  format-string
						  (1+ level)
						  overrides))))))))))
(doloop copy template)
(cadr copy))))



