;; -*- lexical-binding: t; -*-

(setq reorg-test-org-file-list (org-agenda-files))
;; (setq reorg-test-org-file-list "~/tmp/tmp.org")
;; (defun reorg-test-blank-header ()
;;   (interactive)
;;   (reorg-open-sidebar '( :sources ((org . "~/tmp/tmp.org"))
;; 			 :bullet "adsf"
;; 			 :group "test"
;; 			 :children (( :group (when .todo "")
;; 				      :format-results (.headline))))))

(defun reorg*-mix-org-and-files ()
  (interactive)
  (reorg-open-sidebar
   `( :sources ((files . "find ~/Downloads -type f | grep pdf")
		(org . ,reorg-test-org-file-list))
      :group "Everything"
      :sort-results ((.filename . reorg-string<))
      :format-results (.stars " "
			      (or .filename
				  .headline)
			      " "
			      (if .headline
				  "org"
				"files")

			      ))))

  (defun reorg*-group-then-drill (&optional dir)
    (interactive "D")
    (reorg-open-sidebar
     `( :sources ((files . ,(concat "find "
				    dir
				    " -type f")))
	:format-results (.filename)
	:sort-results ((.filename . string<))
	:sort-groups string<
	:group (if (= (mod (length .filename) 2) 0)
		   "EVEN"
		 "ODD")
	:children  (( :group .!parent-dirs)))))

(defun reorg*-drill-then-group (&optional dir)
  (interactive "D")
  (reorg-open-sidebar
   `( :sources ((files . ,(concat "find "
				  dir
				  " -type f")))
      :format-results (.filename)
      :group .!parent-dirs
      :sort-results ((.filename . string<))
      :sort-groups string<
      :children (( :group (if (= (mod (length .filename) 2) 0)
			      "EVEN"
			    "ODD"))))))

(defun reorg-files (&optional dir)
  (interactive "D")
  (reorg-open-sidebar
   `( :sources ((files . ,(concat "find "
				  dir
				  " -type f")))
      :format-results (.filename)
      :group .!parent-dirs
      :sort-results ((.filename . string<))
      :sort-groups string<)))

(defun reorg-client-todo-1 ()
  (interactive)
  (let ((now (ts-format "<%Y-%m-%d" (ts-dec 'day 1 (ts-now)))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :group (when (and 
			    (member .todo '("TASK"
					    "DELEGATED"
					    "EVENT"
					    "OPP_DUE"
					    "DEADLINE"))
			    (if .ts
				(string> .ts ,now)
			      t))
		       .root)
	      ;; (propertize .root
	      ;; 		  'face
	      ;; 		  `(( t ( :foreground ,(face-foreground 'default)
	      ;; 			  :height 1.5
	      ;; 			  :family "ETBembo"
	      ;; 			  :weight bold
	      ;; 			  :underline t)))))
	      :format-results ("   " (s-pad-right 5 " " .priority)
			       (s-pad-right 15 " " .todo)
			       " "
			       (if .ts
				   (s-pad-right 50 "." .headline)
				 .headline)
			       .ts)
	      :sort-results ((.ts . string<)
			     ;; (.todo . (lambda (a b)
			     ;; 		(reorg--sort-by-list a b '("TODO"
			     ;; 					   "WAITING"
			     ;; 					   "DELEGATED"))

			     (.priority . string<))
	      :sort-groups reorg-string<)))))

(defun reorg-client-todo ()
  (interactive)
  (let ((now (ts-format "<%Y-%m-%d" (ts-dec 'day 1 (ts-now)))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :group (when (and 
			    (member .todo '("TASK"
					    "DELEGATED"
					    "EVENT"
					    "OPP_DUE"
					    "DEADLINE"))
			    (if .ts
				(string> .ts ,now)
			      t))

		       (propertize .root
				   'face
				   `(( t ( :foreground ,(face-foreground 'default)
					   :height 1.5
					   :family "ETBembo"
					   :weight bold
					   :underline t)))))
	      :format-results ("   " (s-pad-right 5 " " .priority)
			       (s-pad-right 15 " " .todo)
			       " "
			       (if .ts
				   (s-pad-right 50 "." .headline)
				 .headline)
			       .ts)
	      :sort-results ((.ts . string<)
			     ;; (.todo . (lambda (a b)
			     ;; 		(reorg--sort-by-list a b '("TODO"
			     ;; 					   "WAITING"
			     ;; 					   "DELEGATED"))
			     (.priority . string<)
			     (.headline . string<))
	      :sort-groups reorg-string<)))))

(defun reorg-client-1 ()
  (interactive)
  (let ((now (ts-format "<%Y-%m-%d" (ts-dec 'day 1 (ts-now)))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :group (when (or 
			    (member .todo '("TASK"
					    "DELEGATED"
					    "EVENT"
					    "OPP_DUE"
					    "DEADLINE"))
			    (and .ts
				 (string> .ts ,now))
			    (string= .headline "_NOTES_"))
		       (propertize .root
				   'face
				   `(( t ( :foreground ,(face-foreground 'default)
					   :height 1.5
					   :family "ETBembo"
					   :weight bold
					   :underline t)))))
	      :format-results (.stars "   " (s-pad-right 5 " " .priority)
				      (s-pad-right 15 " " .todo)
				      " "
				      (if .ts
					  (s-pad-right 50 "." .headline)
					.headline)
				      .ts)
	      :sort-groups reorg-string<
	      :children (( :group (when (member .todo '("TASK" "DELEGATED" "WAITING"))
				    "TASKS")
			   :sort-groups (lambda (a b)
					  (reorg--sort-by-list a b
							       '("TASK"
								 "DELEGATED"
								 "WAITING")))
			   :sort-results ((.priority . string<)))
			 ( :group (when (and .ts
					     (not (member .todo '("TASK"
								  "DELEGATED"
								  "WAITING"))))
				    "CALENDAR")
			   :sort-results ((.ts . string<)))
			 ( :group (when (equal "_NOTES_" .headline) "")
			   :format-results (.stars "  NOTES"))))))))

(defun reorg-client ()
  (interactive)
  (let ((now (ts-format "<%Y-%m-%d" (ts-dec 'day 1 (ts-now)))))
    (reorg-open-sidebar
     (setq reorg-client-template
	   `( :sources ((org . ,reorg-test-org-file-list))
	      :group (when (or 
			    (member .todo '("TASK"
					    "DELEGATED"
					    "EVENT"
					    "OPP_DUE"
					    "WAITING" ;
					    "DEADLINE"))
			    (and .ts
				 (string> .ts ,now))
			    (string= .headline "_NOTES_"))
		       (propertize .root
				   'face
				   `(( t ( :foreground ,(face-foreground 'default)
					   :height 1.5
					   :family "ETBembo"
					   :weight bold
					   :underline t)))))
	      :format-results (.stars "   " (s-pad-right 5 " " .priority)
				      (s-pad-right 15 " " .todo)
				      " "
				      (if .ts
					  (s-pad-right 50 "." .headline)
					.headline)
				      .ts)
	      :sort-groups reorg-string<
	      :children (( :group (when (member .todo '("TASK" "DELEGATED" "WAITING"))
				    "TASKS")
			   :sort-groups (lambda (a b)
					  (reorg--sort-by-list a b
							       '("TASK"
								 "DELEGATED"
								 "WAITING")))
			   :sort-results ((.priority . string<)))
			 ( :group (when (and .ts
					     (not (member .todo '("TASK"
								  "DONE"
								  "DELEGATED"
								  "WAITING"))))
				    "CALENDAR")
			   :sort-results ((.ts . string<)))
			 ( :group (when (equal "_NOTES_" .headline) "")
			   :format-results (.stars "  NOTES"))))))))

(defun reorg-todo ()
  (interactive)
  (reorg-open-sidebar
   `( :sources ((org . "~/tmp/tmp.org"))
      :group "Tasks"
      :format-results ((s-pad-right 20 " " .category-inherited)
		       (s-pad-right 10 " " .todo)
		       .headline)
      :children (( :group (when (and .priority
				     (string= .priority "A")
				     (member .todo '("TASK")))
			    "TOP PRIORITY")
		   :sort-results ((.category-inherited . reorg-string<)))
		 ( :group (when (member .todo '("TASK"
						"WAITING"
						"DELEGATED"))
			    .todo)
		   :sort-groups (lambda (a b)
				  (reorg--sort-by-list a b
						       '("TASK"
							 "WAITING"
							 "DELEGATED"
							 "OPP_DUE"))))))))

(defun reorg-calendar ()
  (interactive)
  (reorg-open-sidebar 
   '( :sources ((org . "~/tmp/tmp.org"))
      :group "Calendar"
      :children (( :group
		   .ts-year
		   :sort-groups
		   reorg-string>
		   :children
		   (( :group
		      .ts-month
		      :sort-groups		      
		      (lambda (a b)
			(let ((seq '("January"
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
			  (reorg--sort-by-list a b seq)))
		      :sort-results
		      ((.ts-day . <))
		      :format-results
		      (.stars
		       " "
		       (s-pad-left 2 " "
				   (number-to-string
				    .ts-day))
		       " "
		       (s-pad-right 12 " "
				    .ts-day-name)
		       (s-pad-right
			20
			" "
			.category-inherited)
		       .headline))))))))


;;; make it pretty
(defun reorg--beautification-test ()
  (interactive)
  (reorg-open-sidebar
   `( :sources ((org . "~/tmp/tmp.org"))
      :bullet ""
      :group (propertize "test" 'face '((t ( :height 1.5))))
      :format-results (.stars .headline))))

;;; drill code

(defun reorg--test-drill-code ()
  (interactive)
  (reorg-open-sidebar
   `( :sources ((files . "find ~/Downloads/emacs -type f"))
      ;; :bullet ,(svg-tag-make "OPEN")
      ;; :folded-bullet ,(svg-tag-make "CLOSED" 'default)
      ;; :group .!parent-dirs
      :group .!parent-dirs
      :format-results ((make-string .depth ?\t)
		       .filename))))

(defun reorg-test-file-tree ()
  (interactive)
  (reorg-open-sidebar   
   `( :sources ((files . "find ~/legal/Dropbox/Wilson-Anthony -type f | grep .pdf")
		(org . "~/tmp/tmp.org"))
      :group (when (eq .class 'files) "")
      :children (( :group  .!parent-dirs
		   :sort-results ((.filename . reorg-string<))
		   :format-results (.stars " " .filename))))))

;; (defun reorg-test-file-tree-BAD ()
;;   (interactive)
;;   (reorg-open-sidebar
;;    `( :sources ((files . "~/.emacs.d/lisp/reorg"))
;;       :group "test"
;;       :format-results (.filename)
;;       :sort-results ((.filename . reorg-string<))
;;       :children
;;       (( :group (nth 0 .parent-dirs)
;; 	 :children
;; 	 (( :group (nth 1 .parent-dirs)
;; 	    :children
;; 	    (( :group (nth 2 .parent-dirs)
;; 	       :children
;; 	       (( :group (nth 3 .parent-dirs))))))))))))



;;; JSON testing

(defun reorg--test-json-foodtruck ()
  (interactive)
  (reorg-open-sidebar '( :sources ((json . "~/.emacs.d/lisp/reorg/TEST/bbb8-hzi6.json"))
			 :group .applicant
			 :format-results (.permit " " .dayofweekstr)
			 :sort-results ((.dayofweekstr . string<)))))


(defun reorg--test-json ()
  (interactive)
  (reorg-open-sidebar '( :sources ((json . "~/.emacs.d/lisp/reorg/TEST/y77d-th95.json"))
			 :group (if .year (substring .year 0 4) "Unknown year")
			 :sort-groups string>
			 :format-results (.mass "\t" .name " " .geolocation.type)
			 :sort-results (((if .mass (string-to-number .mass) "") . <))
			 :children (( :group (if .mass
						 (if (> (string-to-number .mass) 1000)
						     "Mass > 1000"
						   "Mass <= 1000")
					       "Mass Unknown")
				      :sort-groups string<)))))

;;; org-mode testing

(defun reorg--set-up-capture-test ()
  "set up capture testing"
  (interactive)
  (setq org-capture-templates
	'(("t" "Task" entry (function org-legal-default-capture-find-func) "* TASK %?\n")
	  ("c" "Calendar" entry (function org-legal-calendar-capture-find-func) "* EVENT %?\n")
	  ("d" "Calendar" entry (function org-legal-calendar-capture-find-func) "* DEADLINE %?\n")
	  ("n" "Progress note" plain (function org-legal-notes-capture-find-func) "%U\n%?\n")
	  ("w" "Case review agenda" checkitem (id "ebd5bf86-9234-443a-bc90-9a11571d1424"))))

  (defun org-legal-default-capture-find-func (&optional file)
    "Prompt the user to select from the root headings in an agenda file."
    (interactive)
    (let* ((headings (org-ql-select "~/tmp/tmp.org"
		       `(level 1)
		       :action
		       (lambda ()
			 `(:file ,(buffer-file-name)
				 :point ,(org-element-property
					  :begin
					  (org-element-at-point))
				 :headline ,(org-no-properties (org-get-heading t t t t))))))
	   (candidate (completing-read "Task target: " 
				       (mapcar (lambda (heading) (plist-get heading :headline))
					       headings)))
	   (target (cl-loop for heading in headings
			    when (string= (plist-get heading :headline) candidate)
			    return heading)))
      (find-file (plist-get target :file))
      (widen)
      (goto-char (plist-get target :point))
      (if (re-search-forward "^\\*+[[:space:]]+_TASKS_"
			     (save-excursion (org-end-of-subtree)) t)
          (progn (goto-char (match-end 0)))
	(org-insert-heading-respect-content)
	(org-metaright)
	(insert "_TASKS_"))))


  (defun org-legal-calendar-capture-find-func (&optional file)
    "Prompt the user to select from the root headings in an agenda file, 
  then move to the first sub-heading called \"_CALENDAR_\"."
    (interactive)
    (let* ((headings (org-ql-select "~/tmp/tmp.org"
		       `(level 1)
		       :action
		       (lambda ()
			 `(:file ,(buffer-file-name)
				 :point ,(org-element-property
					  :begin
					  (org-element-at-point))
				 :headline ,(org-no-properties (org-get-heading t t t t))))))
	   (candidate (completing-read "Calendar target: " 
				       (mapcar (lambda (heading) (plist-get heading :headline))
					       headings)))
	   (target (cl-loop for heading in headings
			    when (string= (plist-get heading :headline) candidate)
			    return heading)))
      (find-file (plist-get target :file))
      (widen)
      (goto-char (plist-get target :point))
      (if (re-search-forward "^\\*+[[:space:]]+_CALENDAR_"
			     (save-excursion (org-end-of-subtree)) t)
	  (progn (goto-char (match-end 0)))
	(org-insert-heading-respect-content)
	(org-metaright)
	(insert "_CALENDAR_"))))

  (defun org-legal-notes-capture-find-func (&optional file)
    "Prompt the user to select from the root headings in an agenda file, 
  then move to the first sub-heading called \"_NOTES_\"."
    (interactive)
    (let* ((headings (org-ql-select "~/tmp/tmp.org"
		       `(level 1)
		       :action
		       (lambda ()
			 `(:file ,(buffer-file-name)
				 :point ,(org-element-property
                                          :begin
                                          (org-element-at-point))
				 :headline ,(org-no-properties (org-get-heading t t t t))))))
           (candidate (completing-read "Note: " 
				       (mapcar (lambda (heading) (plist-get heading :headline))
					       headings)))
           (target (cl-loop for heading in headings
			    when (string= (plist-get heading :headline) candidate)
			    return heading)))
      (find-file (plist-get target :file))
      (widen)
      (goto-char (plist-get target :point))
      (if (re-search-forward "^\\*+[[:space:]]+_NOTES_"
			     (save-excursion (org-end-of-subtree)) t)
          (progn (org-end-of-meta-data t))
	(org-insert-heading-respect-content)
	(org-metaright)
	(insert "_NOTES_"))))
  (reorg-org-capture-enable))

(defun reorg-elisp-test ()
  (interactive)
  (reorg-open-sidebar
   `( :sources ((elisp . ,(buffer-file-name)))
      :children (( :group (pcase .form-type
			    ((or "defun" "cl-defun") "Functions")
			    ((or "defmacro" "cl-defmacro") "Macros")
			    (_ nil))
		   :sort-groups reorg-string<
		   :sort-results (((f-filename .file) . reorg-string<)
				  (.form-name . reorg-string<))
		   :children
		   (( :group (if (s-contains-p "--" .form-name)
				 "Private"
			       "Public")
		      :sort-groups reorg-string<
		      :format-results ((replace-regexp-in-string (rx "reorg-"
								     (zero-or-one "-"))
								 ""
								 .form-name)
				       (propertize " " 'display
						   `(space . (:align-to 70)))
				       (f-filename .file)
				       ))))
		 ( :group (when (member .form-type '("defcustom"
						     "defvar"
						     "defconst"))
			    "Variables")
		   :children (( :group (when (member .form-type '("defcustom"
								  "defvar"
								  "defconst"))
					 .form-type)
				:format-results ((replace-regexp-in-string (rx "reorg-"
									       (zero-or-one "-"))
									   ""
									   .form-name)
						 (propertize " " 'display
							     `(space . (:align-to 70)))
						 (f-filename .file)
						 ))))))))


(defun reorg-test-1 ()
  (interactive)
  (reorg-open reorg-template--test-org)
  (reorg-open-sidebar))

(setq reorg-template--test-org '( :sources ((org . "~/tmp/tmp.org"))
				  :children
				  (( :group "By client"
				     ;; :bullet "It's a tarp!"
				     :children
				     (( :group
					.category-inherited
					:sort-groups
					reorg-string<
					:children
					(( :group
					   (when
					       (and
						.todo
						(not (string= "DONE" .todo))
						(not (string= "EVENT" .todo))
						(not (string= "DEADLINE" .todo))
						(not (string= "OPP_DUE" .todo)))
					     "Tasks")
					   :sort-groups
					   reorg-string<
					   :format-results
					   (.priority
					    " "
					    (s-pad-right 15 " " .todo)
					    " " .headline)
					   :sort-results
					   ((.priority . reorg-string<)
					    (.headline . reorg-string<)))
					 ( :group (when (and .ts-ts
							     (ts> .ts-ts (ts-now))
							     (not (string= "DONE" .todo)))
						    "Calendar")
					   :format-results
					   (.ts-type
					    " "
					    (s-pad-right 30 " " .ts)
					    " " .headline)
					   :sort-results
					   (( .ts . reorg-string<)))))))
				   ( :group "By delegatee"
				     ;; :bullet "This should be different."
				     :children (( :group
						  .delegatee
						  :sort-groups
						  reorg-string<)))		 
				   ( :group "Calendar"
				     :children (( :group
						  .ts-year
						  ;; :bullet "Year: "
						  :sort-groups
						  string<
						  :children
						  (( :group
						     .ts-month
						     ;; :bullet "       "
						     :sort-groups
						     (lambda (a b)
						       (let ((seq '("January"
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
							 (< (seq-position seq a 'string=)
							    (seq-position seq b 'string=))))
						     :sort-results
						     ((.ts-day . <))
						     :format-results
						     ("                             "
						      " "
						      (s-pad-left 2 " "
								  (number-to-string
								   .ts-day))
						      " "
						      (s-pad-right 12 " "
								   .ts-day-name)
						      (s-pad-right
						       20
						       " "
						       .category-inherited)
						      .headline)))))))))
(defun reorg-test-all ()
  (interactive)
  (reorg-open reorg-template--test-all))

(setq reorg-template--test-all
      '( :sources ((org . "~/tmp/tmp.org"))
	 :format-results (.stars " " .headline)
	 :children
	 (( :group "By client"
	    :children
	    (( :group
	       .category-inherited
	       :sort-groups
	       reorg-string<			
	       :children
	       (( :group
		  (when
		      (and
		       .todo
		       (not (string= "DONE" .todo))
		       (not (string= "EVENT" .todo))
		       (not (string= "DEADLINE" .todo)))
		    "Tasks")
		  :sort-groups
		  reorg-string<
		  :format-results
		  (.priority
		   " "
		   (s-pad-right 15 " " .todo)
		   " " .headline)
		  :sort-results
		  ((.priority . string<)
		   (.headline . string<)))
		( :group (when (and .ts-ts
				    (ts> .ts-ts (ts-now)))
			   "Calendar")
		  :format-results
		  (.ts-type
		   " "
		   (s-pad-right 30 " " .ts)
		   " " .headline)
		  :sort-results
		  (( .ts . string<)))))))
	  ( :group "By delegatee"
	    :children (( :group
			 .delegatee
			 :sort-groups
			 (lambda (a b)
			   (string< a b)))))		 
	  ( :group "Calendar"
	    :children (( :group
			 .ts-year
			 :sort-groups
			 (lambda (a b) (string< a b))
			 :children
			 (( :group
			    .ts-month
			    :sort-groups
			    (lambda (a b)
			      (let ((seq '("January"
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
				(< (seq-position seq a 'string=)
				   (seq-position seq b 'string=))))
			    :sort-results
			    ((.ts-day . <))
			    :format-results
			    (.stars
			     " "
			     (s-pad-left 2 " "
					 (number-to-string
					  .ts-day))
			     " "
			     (s-pad-right 12 " "
					  .ts-day-name)
			     (s-pad-right
			      20
			      " "
			      .category-inherited)
			     .headline))))))
	  ( :sources ((email . "subject:allums"))
	    :group (when (equal .class "email")
		     "Emails")
	    :format-results (.stars
			     " "
			     .date
			     "\t\t"
			     .subject))
	  ( :sources ((files . "find ~/legal/Dropbox/Allums\\,\\ Matthew/docket -type f"))
	    :group (when (eq .class 'files) "")
	    :children (( :group "File tree"
			 :children (( :group .!parent-dirs
				      :format-results (.stars " " .filename)
				      :sort-results ((.filename . reorg-string<)))))
		       ( :group "By extension"
			 :children (( :group .extension
				      :sort-groups (lambda (a b) (string< (downcase a)
									  (downcase b)))
				      :sort-results (((downcase .filename) . string<))
				      :format-results (.filename)))))))))

(defun reorg-user--test-all ()
  (interactive)
  (reorg-open-main-window
   '( :sources ((org . "~/tmp/tmp.org"))
      :children
      (( :group "By client"
	 :children
	 (( :group
	    .category-inherited
	    :sort-groups
	    (lambda (a b)
	      (string< (downcase a)
		       (downcase b)))			
	    :children
	    (( :group
	       (when
		   (and
		    .todo
		    (not (string= "DONE" .todo))
		    (not (string= "EVENT" .todo))
		    (not (string= "DEADLINE" .todo)))
		 "Tasks")
	       :sort-group
	       string<
	       :format-results
	       (.priority
		" "
		(s-pad-right 15 " " .todo)
		" " .headline)
	       :sort-results
	       ((.priority . string<)
		(.headline . string<)))
	     ( :group (when (and .ts-ts
				 (ts> .ts-ts (ts-now)))
			"Calendar")
	       :format-results
	       (.ts-type
		" "
		(s-pad-right 30 " " .ts)
		" " .headline)
	       :sort-results
	       (( .ts . string<)))))))
       ( :group "By delegatee"
	 :children (( :group
		      .delegatee
		      :sort-group
		      (lambda (a b)
			(string< a b)))))		 
       ( :group "Calendar"
	 :children (( :group
		      .ts-year
		      :sort-groups
		      (lambda (a b) (string< a b))
		      :children
		      (( :group
			 .ts-month
			 :sort-groups
			 (lambda (a b)
			   (let ((seq '("January"
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
			     (< (seq-position seq a 'string=)
				(seq-position seq b 'string=))))
			 :sort-results
			 ((.ts-day . <))
			 :format-results
			 (.stars
			  " "
			  (s-pad-left 2 " "
				      (number-to-string
				       .ts-day))
			  " "
			  (s-pad-right 12 " "
				       .ts-day-name)
			  (s-pad-right
			   20
			   " "
			   .category-inherited)
			  .headline))))))
       ( :sources ((email . "subject:allums"))
	 :group (when (eq .class 'email)
		  "Emails")
	 :format-results (.stars
			  " "
			  .date
			  "\t\t"
			  .subject))
       ( :sources ((files . "find ~/legal/Dropbox/Allums\\,\\ Matthew/docket -type f"))
	 :group "Files"
	 :children (( :group "By extension"
		      :children (( :group .extension
				   :sort-groups (lambda (a b) (string< (downcase a)
								       (downcase b)))
				   :sort-results (((downcase .filename) . string<))
				   :format-results (.filename))))
		    ( :group "by parent"
		      :children (( :group (when .depth (number-to-string .depth ))
				   :sort-groups string<
				   :format-results (.stars " " .fullname))))))))))

(defun reorg-user--test-main-view ()
  (interactive)
  (reorg-open-main-window
   '( :sources ((org . "~/tmp/tmp.org"))
      :children
      (( :group "By client"
	 :children
	 (( :group
	    .category-inherited
	    :sort-groups
	    (lambda (a b)
	      (string< (downcase a)
		       (downcase b)))			
	    :children
	    (( :group
	       (when
		   (and
		    .todo
		    (not (string= "DONE" .todo))
		    (not (string= "EVENT" .todo))
		    (not (string= "DEADLINE" .todo)))
		 "Tasks")
	       :sort-group
	       string<
	       :format-results
	       (.priority
		" "
		(s-pad-right 15 " " .todo)
		" " .headline)
	       :sort-results
	       ((.priority . string<)
		(.headline . string<)))
	     ( :group (when (and .ts-ts
				 (ts> .ts-ts (ts-now)))
			"Calendar")
	       :format-results
	       (.ts-type
		" "
		(s-pad-right 30 " " .ts)
		" " .headline)
	       :sort-results
	       (( .ts . string<)))))))
       ( :group "By delegatee"
	 :children (( :group
		      .delegatee
		      :sort-group
		      (lambda (a b)
			(string< a b)))))		 
       ( :group "Calendar"
	 :children (( :group
		      .ts-year
		      :sort-groups
		      (lambda (a b) (string< a b))
		      :children
		      (( :group
			 .ts-month
			 :sort-groups
			 (lambda (a b)
			   (let ((seq '("January"
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
			     (< (seq-position seq a 'string=)
				(seq-position seq b 'string=))))
			 :sort-results
			 ((.ts-day . <))
			 :format-results
			 (.stars
			  " "
			  (s-pad-left 2 " "
				      (number-to-string
				       .ts-day))
			  " "
			  (s-pad-right 12 " "
				       .ts-day-name)
			  (s-pad-right
			   20
			   " "
			   .category-inherited)
			  .headline))))))))))

(defun reorg-test-email ()
  (interactive)
  (reorg-open
   '( :sources ((email . "subject:allums"))
      :children (( :group "Allums emails"
		   :children (( :group (when (eq .class 'email)
					 "Emails")
				:format-results (.stars
						 " "
						 .date
						 "\t"
						 .from
						 "\t\t"
						 .subject))))))))



(defun reorg-user--leo ()
  (interactive)
  (reorg-open-sidebar
   '( :sources ((leo . "~/.leo/workbook.leo"))
      :group "workbook.leo"
      :format-results (.stars " " .headline)
      :overrides (
		  (reorg-level . (1+ (or .leo-level 0))))
      :post-overrides ((reorg-branch . t)))))

(defun reorg-user--clone-file ()
  (interactive)
  (reorg-open-in-current-window
   '( :group "tmp.org"
      :sources ((org . "~/tmp/tmp.org"))
      :overrides ((reorg-level . (or .org-level 1)))
      :format-results (.stars " " .headline)
      :post-overrides ((reorg-branch . t)))))

(defun reorg-test--at-names ()
  (interactive)
  (reorg-open-in-current-window
   '( :sources ((org . "~/.emacs.d/lisp/reorg/TESTS/new.org"))
      :children (( :group .@at-names
		   :format-results
		   (.stars " " .headline))))))

(defun reorg-test--tag-list ()
  (interactive)
  (reorg-open-in-current-window
   '(
     :sources ((org . "~/.emacs.d/lisp/reorg/TESTS/new.org"))
     :format-results (.stars " " .headline)
     :children (( :group .@tag-list
		  :sort-groups (lambda (a b)
				 (string< (downcase a)
					  (downcase b)))
		  :children (( :group .@at-names )))))))

(defun reorg-test--file-view ()
  (interactive)
  (reorg-open-sidebar
   '(
     :sources ((files . "find ~/Desktop -type f"))
     :children (( :group "By extension"
		  :children (( :group .extension
			       :sort-groups (lambda (a b) (string< (downcase a)
								   (downcase b)))
			       :sort-results (((downcase .filename) . string<))
			       :format-results (.filename))))
		( :group "by parent"
		  :children (( :group (when .depth (number-to-string .depth ))
			       :sort-groups string<
			       :format-results (.stars " " .fullname))))))))



  
(provide 'reorg-test)

(defun reorg-file-killer ()
  (interactive)
  (reorg-open-sidebar
   '( :sources ((files . "find ~/tmp/elpa/corfu-0.26 -type f")
		(files . "find ~/Desktop/wallpapers/ -type f"))
      :group "test"
      :children (( :group (when (quote .class) .!parent-dirs)
		   :format-results (.filename)
		   :sort-results ((.filename . string>))
		   :children (( :group (if (string= (substring .filename 0 1) "c")
					   "C"
					 "Not C")
				:children (( :group .!parent-dirs)))))))))
