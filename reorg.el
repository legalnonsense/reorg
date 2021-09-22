;;; -*- lexical-binding: t; -*-

(require 'reorg-views)
(require 'reorg-edits)

(defcustom reorg-parser-use-id-p t "use id or markers?")
(defcustom reorg-buffer-name "*REORG*"
  "Default buffer name for tree view window.")
(defcustom reorg-buffer-side 'left
  "Which side for the tree buffer?")
(defcustom reorg-face-text-prop 'font-lock-face
  "When setting a face, use this text property.")
(defconst reorg--data-property-name 'reorg-data)
(defconst reorg--field-property-name 'reorg-field-type)

(defvar reorg-headline-format '(:stars :headline)
  "Headline format.")


;;; Utilities

(defun reorg--plist-p (data)
  "Is DATA a plist based on:
1. Is it a list?
2. Is the first element a keyword?
3. Are there an even number of elements?"
  (and (listp data)
       (keywordp (car data))
       (evenp (length data))))

(defun reorg--goto-headline-start ()
  (save-match-data 
    (goto-char (org-entry-beginning-position))
    (re-search-forward "^\\*+[[:space:]]" nil t)
    (backward-char 1)
    (point)))

(defun reorg--get-headline-start ()
  (save-excursion (reorg--goto-headline-start)))

(defun reorg--goto-headline-start ()
  (save-match-data 
    (goto-char (org-entry-beginning-position))
    (re-search-forward "^\\*+[[:space:]]" nil t)
    (backward-char 1)
    (point)))

;;; Parsing org files

(defun reorg-parser--get-body-elements ()
  "Use org-element to get all elements after the property drawers."
  (org-element--parse-elements (save-excursion (org-back-to-heading)
					       (org-end-of-meta-data t)
					       (point))
			       (or (save-excursion (outline-next-heading))
				   (point-max))
			       'first-section nil nil nil nil))

(defun reorg-parser--get-body-string (&optional no-props)
  "Parse all elements from the start of the body to the next node.
and return the tree beginning with the section element.
If NO-PROPS is non-nil, remove text properties."
  (--> (reorg-parser--get-body-elements)
       (org-element-interpret-data it)
       (if no-props
	   (org-no-properties it)
	 it)))

(defun reorg-parser--add-remove-prop-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix.
Returns PROP with a colon prefix. If REMOVE is t,
then return PROP with no colon prefix."
  (pcase `(,remove ,(keywordp prop))
    (`(t t) (intern (substring (symbol-name prop) 1)))
    (`(nil nil) (intern (concat ":" (symbol-name prop))))
    (_ prop)))

(defun reorg-parser--to-string (arg)
  "Convert ARG to a string."
  (pcase arg
    ((pred null) nil)
    ((pred numberp) (number-to-string arg))
    ((pred stringp) arg)
    ((pred symbolp) (symbol-name arg))
    (_ arg)))

(defun reorg-parser--get-children-ids ()
  "Get the list of IDs of all children."
  (org-with-wide-buffer
   (let (ids)
     (when (org-goto-first-child)
       (push (org-id-get (point) t) ids)
       (while (outline-get-next-sibling)
	 (push (org-id-get (point) t) ids)))
     ids)))

(defcustom reorg-parser--additional-parsers
  '((location . #'reorg-parser--location-parser))
  "alist where keyword will be stored as the value
of the data and value is a function to call at point." )

;; (defun reorg-parser--run-org-ql (&optional buffer name query)
;;   "run org-ql"
;;   (setq org-ql-cache (make-hash-table :weakness 'key))
;;   (org-ql-select buffer query
;;     :action #'reorg-parser--headline-parser))

(defun reorg-parser--map-entries (&optional match scope &rest skip)
  "Run the parser at each heading in the current buffer."
  (unless scope
    (org-with-wide-buffer 
     (org-map-entries
      #'reorg-parser--headline-parser match scope skip))))
     

(defun reorg-parser--org-entry-properties ()
  "Convert keys from strings to symbols."
  (let ((exclusions '(alltags tags)))
    (cl-loop for (key . value) in (org-entry-properties)
	     unless (memq (intern (downcase key)) exclusions)
	     append (list (reorg-parser--add-remove-prop-colon (intern (downcase key)))
			  value))))

(defun reorg-parser--get-tags (inherited)
  "Get tags for heading at point.  INHERITED can be:
local: only get local tags
all: get both local and inherited tags
inherited: get only inherited tags."
  (let* ((all-tags (mapcar #'org-no-properties (org-get-tags (point))))
	 (local-tags (mapcar #'org-no-properties (org-get-local-tags)))
	 (inherited-tags (-difference all-tags local-tags)))
    (pcase inherited
      (`local local-tags)
      (`all all-tags)
      (`inherited inherited-tags))))

(defun reorg-parser--get-children-ids ()
  "Get the list of IDs of all children."
  (org-with-wide-buffer
   (let (ids)
     (when (org-goto-first-child)
       (push (org-id-get (point) t) ids)
       (while (outline-get-next-sibling)
	 (push (org-id-get (point) t) ids)))
     ids)))

(defun reorg-parser--timestamp-parser (&optional inactive range)
  "Find the fist timestamp in the current heading and return it. 
if INACTIVE is non-nil, get the first inactive timestamp.  If 
RANGE is non-nil, only look for timestamp ranges."
  (save-excursion
    (when (re-search-forward
	   (pcase `(,inactive ,range)
	     (`(nil t)
	      org-tr-regexp)
	     (`(nil nil)
	      org-ts-regexp)
	     (`(t nil)
	      org-ts-regexp-inactive)
	     (`(t t)
	      (concat 
	       org-ts-regexp-inactive
	       "--?-?"
	       org-ts-regexp-inactive)))
	   (org-entry-end-position)
	   t)
      (propertize 
       (match-string 0)
       reorg-face-text-prop
       'org-date))))

(defun reorg-parser--org-entry-properties ()
  "Convert keys from strings to symbols."
  (let ((exclusions '(alltags tags timestamp timestamp-ia)))
    (cl-loop for (key . value) in (org-entry-properties)
	     unless (memq (intern (downcase key)) exclusions)
	     append (list (reorg-parser--add-remove-prop-colon
			   (intern (downcase (string-replace "_" "-" key))))
			  value))))

(defun reorg-parser--headline-parser ()
  "Runs at each org heading and returns a plist of 
relevant properties to be inserted into the calendar buffer."
  (cl-loop with data = (reorg-parser--org-entry-properties)
	   for (prop . val) in `((id . ,(if reorg-parser-use-id-p
					    (org-id-get-create)
					  "nil"))
				 (category . ,(reorg-get-set-props "CATEGORY"))
				 (category-inherit . ,(reorg-get-set-props "CATEGORY" :inherit t))				 
				 (tag-local . ,(reorg-parser--get-tags 'local))
				 (tag-string . ,(propertize (org-get-tags-string) reorg-face-text-prop 'org-tag))
				 (tag-inherit . ,(reorg-parser--get-tags 'inherited))				 
				 (headline . ,(org-get-heading))
				 (deadline . ,(reorg-get-set-props 'deadline))
				 (headline-only . ,(org-no-properties (org-get-heading t t t t)))
				 (level . ,(org-current-level))
				 (todo . ,(reorg-get-set-props 'todo))
				 (tags-all . ,(reorg-parser--get-tags 'all))				 
				 (marker . ,(point-marker))
				 (timestamp . ,(reorg-parser--timestamp-parser))
				 (timestamp-ia . ,(reorg-parser--timestamp-parser t))
				 (timestamp-range . ,(reorg-parser--timestamp-parser nil t))
				 (timestamp-ia-range . ,(reorg-parser--timestamp-parser t t))
				 (body . ,(reorg-parser--get-body-string t))
				 (children-id . ,(when reorg-parser-use-id-p
						   (reorg-parser--get-children-ids)))
				 (siblings-id . ,(when reorg-parser-use-id-p
						   (save-excursion
						     (let ((current-id (org-id-get-create)))
						       (when (org-up-heading-safe)
							 (cl-loop initially (org-goto-first-child)
								  collect (org-id-get-create) into ids
								  while (org-goto-sibling)
								  finally return (remove current-id ids)))))))
				 (ancestors-id . ,(when reorg-parser-use-id-p
						    (save-excursion 
						      (cl-loop while (org-up-heading-safe)
							       collect (org-id-get-create)))))
				 (parent-id . ,(when reorg-parser-use-id-p
						 (save-excursion (when (org-up-heading-safe)
								   (org-id-get)))))
				 (buffer-name . ,(buffer-name))
				 (buffer . ,(current-buffer)))
	   do (plist-put data (reorg-parser--add-remove-prop-colon prop) (reorg-parser--to-string val))
	   finally return data))

(cl-defun reorg-get-set-props (prop &key
				    (val nil valp)
				    keep
				    multi-value
				    inherit
				    literal-nil
				    no-duplicates
				    no-text-properties
				    include-date-prefix
				    &allow-other-keys)
  "Change the org heading at point by set PROP to VAL.

It accepts the following properties, as well as any others that are set 
in the headings property drawer. Any such properties can be accessed as string
 or a symbol, e.g., \"CATEGORY\" or 'category.  See the return value of 
`reorg-parser--headline-parser' for more information.

There are flags for dealing with multivalued properties, inheritence, 
etc.:

If VAL is a list, assume a multi-valued property.
If KEEP is non-nil and VAL is a list or MULTI is non-nil, keep the old value.
If NO-DUPLICATES is non-nil and dealing with multi-valued, delete duplicates.
If MULTI is non-nil, use a multivalued property even if VAL is not a list.

When setting a value, return a cons cell with the old value as the `car' 
and new value as the `cdr'."
  (cl-macrolet ((get-or-set (&key get set)
			    `(if (not valp)
				 ,get
			       (let ((old-val ,get))
				 (save-excursion 
				   (org-back-to-heading)		 
				   ,set
				   (cons old-val ,get))))))
    (pcase prop
      ;;(org-insert-time-stamp (org-read-date t t "2021-01-01"))
      ((or `deadline
	   `scheduled)
       (get-or-set :get (when-let ((date (org-entry-get (point) (if (eq prop 'deadline)
								    "DEADLINE" "SCHEDULED")
							inherit literal-nil)))
			  (concat
			   (when include-date-prefix
			     (propertize 
			      (if (eq prop 'deadline) "DEADLINE: " "SCHEDULED: ")
			      reorg-face-text-prop 'org-special-keyword))
			   (propertize date reorg-face-text-prop 'org-date)))
		   :set (let ((func (if (eq prop 'deadline)
					#'org-deadline #'org-scheduled)))
			  (if (null val)
			      (funcall func '(4))
			    (funcall func nil val)))))

      (`comment
       (get-or-set :get (org-in-commented-heading-p)
		   :set (when (not (xor (not val)
					(org-in-commented-heading-p)))
			  (org-toggle-comment))))
      (`tags
       (get-or-set :get (org-get-tags (point) (not inherit))
		   :set (if keep
			    (org-set-tags (if no-duplicates
					      (delete-duplicates (append old-val
									 (-list val))
								 :test #'string=)
					    (append old-val (-list val))))
			  (org-set-tags val))))
      (`headline
       (get-or-set :get (org-entry-get (point) "ITEM")
		   ;; keep the comment if it is there
		   :set (let ((commentedp (org-in-commented-heading-p)))
			  (org-edit-headline val)
			  (when commentedp
			    (reorg-get-set-props 'comment :val t)))))
      (`todo
       (get-or-set :get (when-let ((todo (org-entry-get (point) "TODO")))
			  (propertize
			   todo
			   reorg-face-text-prop
			   (org-get-todo-face todo)))
		   :set (org-todo val)))
      ((or `timestamp
	   `timestamp-ia)
       (get-or-set :get (when-let ((timestamp (org-entry-get (point)
							     (if (eq 'timestamp prop)
								 "TIMESTAMP"
							       "TIMESTAMP_IA"))))
			  (propertize timestamp reorg-face-text-prop 'org-date))
		   :set (if (and old-val
				 (search-forward old-val (org-entry-end-position) t))
			    (progn (replace-match (concat val))
				   (delete-blank-lines))
			  (org-end-of-meta-data t)
			  (delete-blank-lines)
			  (when val 
			    (insert (concat val "\n"))))))
      (`body
       (get-or-set :get (reorg-edits--get-body-string no-text-properties)
		   :set (error "You can't set body text (yet).")))
      ((or (pred stringp)
	   (pred symbolp))
       (when (symbolp prop) (setq prop (thread-last
					   (reorg-parser--add-remove-prop-colon prop 'remove)
					 (symbol-name)
					 (upcase))))
       (get-or-set :get (if (or multi-value (and val (listp val)) keep)
			    (org-entry-get-multivalued-property (point) prop)
			  (org-entry-get (point) prop inherit literal-nil))
		   :set (cond ((or multi-value (listp val) keep)
			       (apply #'org-entry-put-multivalued-property (point) prop
				      (if no-duplicates
					  (delete-duplicates (append old-val (-list val)) :test #'string=)
					(append old-val (-list val)))))
			      (t (org-entry-put (point) prop val))))))))

(cl-defun reorg--update-hash (id prop val &rest keys)
  "Set PROP and VAL of entry ID, and then replace the 
current view entry."
  (let (new)
    (reorg--with-point-at-orig-entry id
      (apply reorg-get-set-props prop :val val keys)
      (setq new (reorg-parser--headline-parser)))
    (puthash id new reorg-hash-table)
    (reorg--create-headline-string
     new
     reorg-headline-format
     (outline-level))))

;;; Sorting and grouping

(defun reorg--multi-sort (functions-and-predicates sequence)
  "FUNCTIONS-AND-PREDICATES is an alist of functions and predicates.
It uses the FUNCTION and PREDICATE arguments useable by `seq-sort-by'.
SEQUENCE is a sequence to sort."
  ;; This is a naive solution, but works for now. 
  (seq-sort 
   (lambda (a b)
     (cl-loop for (func . pred) in functions-and-predicates
	      unless (equal (funcall func a)
			    (funcall func b))
	      return (funcall pred
			      (funcall func a)
			      (funcall func b))))
   sequence))

(cl-defun reorg--group-by (cell template &optional (n 0 np))
  "Apply TEMPLATE to CELL, and return the resulting hierarchy.  
CELL should be a list of data.  TEMPLATE is a nested list of functions
that each accept one argument.  N is for internal use only.  
CELL is destructively modified."
  (let ((copy (seq-copy cell)))
    (cl-labels ((doloop (data template &optional (n 0 np) result-sorters)
			(let ((grouper (plist-get template :group))
			      (children (plist-get template :children))
			      (sorter (plist-get template :sort))
			      (sort-getter (or (plist-get template :sort-getter)
					       #'car))
			      (pre-filter (plist-get template :pre-filter))
			      (post-filter (plist-get template :post-filter))
			      (format-string (plist-get template :format-string))
			      (pre-transformer (plist-get template :pre-transformer))
			      (post-transformer (plist-get template :post-transformer))
			      (result-sort-func (or (plist-get template :sort-results-getter)
						    #'identity))
			      (result-sort-pred (plist-get template :sort-results)))
			  (when result-sort-pred
			    (setq result-sorters (reverse result-sorters))
			    (push (cons result-sort-func result-sort-pred) 
				  result-sorters)
			    (setq result-sorters (reverse result-sorters)))
			  
			  (unless np
			    (let ((old (cl-copy-list data)))
			      (setcar data '_)
			      (setcdr data (list old))))

			  (setf (nth n (cdr data))
				(--> (nth n (cdr data))
				     (if pre-transformer
					 (seq-map pre-transformer it)
				       it)
				     (if pre-filter (seq-remove pre-filter it) it)
				     (cond ((functionp grouper)
					    (->> it
						 (seq-group-by grouper)
						 (seq-map (lambda (x) (list (car x) (cdr x))))))
					   ((or (stringp grouper)
						(symbolp grouper))
					    (list (list grouper it)))
					   (t (error "something is wrong with your :group")))
				     (seq-filter (lambda (x) (and (not (null (car x)))
								  (not (null (cdr x)))
								  (not (null x))))
						 it)
				     (if sorter
					 (seq-sort-by (or sort-getter #'car) sorter it)
				       it)
				     (if post-filter
					 (cl-loop for each in it
						  collect (list (car each) (seq-remove post-filter (cadr each))))
				       it)
				     (if post-transformer
					 (cl-loop for each in it
						  collect (list (car each) (seq-map post-transformer (cadr each))))
				       it)))
			  (if children
			      (progn 
				(cl-loop for x below (length (nth n (cdr data)))
					 do (setcdr (nth x (nth n (cdr data)))
						    (cl-loop for z below (length children)
							     collect (seq-copy (cadr (nth x (nth n (cdr data))))))))
				(cl-loop for x below (length children)
					 do (cl-loop for y below (length (nth n (cdr data)))
						     do (doloop (nth y (nth n (cdr data)))
								(nth x children)
								x
								result-sorters))))
			    (when result-sorters
			      (cl-loop for x below (length (nth n (cdr data)))
				       do (setf (cadr (nth x (nth n (cdr data))))
						(reorg--multi-sort result-sorters
								   (cadr (nth x (nth n (cdr data))))))))))))

      (doloop copy template))
    (cadr copy)))

;;; Convert grouped list to org headings

(defun reorg--create-headline-stars (num &optional data)
  "Create heading stars. NUM can be an integer or a function
that is called with DATA as an argument."
  (concat 

   " "))

(defun reorg--process-results (data &optional format-string)
  "Process the results of `org-group' and turn them into orgmode headings."
  (setq format-string  (or format-string reorg-headline-format))
  (let (results)
    (cl-labels ((recurse (data level)
			 (cl-loop for entry in data
				  do (push (reorg--create-headline-string (car entry)
									  format-string
									  level)
					   results)
				  if (reorg--plist-p (caadr entry))
				  do (cl-loop for x in (cadr entry)
					      do (push (reorg--create-headline-string x
										      format-string
										      (1+ level))
						       results))
				  else do (cl-loop for e in (cdr entry)
						   do (recurse e (1+ level))))))
      (recurse data 1))
    (reverse results)))

(defun reorg--create-headline-string (data format-string &optional num)
  "this is a mess."
  (cl-flet* ((get-field-name (keyword) (intern (substring (symbol-name keyword) 1)))
	     (make-field-string (string field)
				(concat 
				 (propertize string
					     reorg--field-property-name
					     (get-field-name field))
				 " "))
	     (create-stars (num &optional data)
			   (make-string (if (functionp num)
					    (funcall num data)
					  num)
					?*)))

    (if (stringp data)
	(concat 
	 (propertize 
	  (concat (create-stars num nil) " " data)
	  reorg--field-property-name :no-data)
	 "\n")
      (cl-loop for field in format-string  
	       concat (cond ((symbolp field)
			     (if (eq field :stars)
				 (make-field-string (create-stars num data) field)
			       (when-let ((field-val (reorg-parser--to-string (plist-get data field))))
				 (make-field-string field-val field))))
			    ((stringp field) field))
	       ;; ((listp field)
	       ;;  (concat
	       ;;   (when-let ((field-val (reorg-parser--to-string (plist-get data (car field)))))
	       ;; 	(make-field-string  (car field)
	       ;; 			    (get-field-name (car field))))
	       ;;   (propertize " "
	       ;; 		  'display
	       ;; 		  `(space . (:align-to ,(cadr field)))))))
	       into results
	       finally return
	       (propertize
		(concat
		 results
		 "\n")
		reorg--data-property-name data)))))

;;; Creating the tree buffer

(defun reorg--insert-org-headlines (data)
  "it's just a loop"
  (cl-loop for x in data do (insert x)))

;;; Window control 

(defun reorg--open-side-window ()
  "Open a side window to display the tree."
  (display-buffer-in-side-window (get-buffer-create reorg-buffer-name)
				 `((side . ,reorg-buffer-side)
				   (slot . nil))))

(defun reorg--select-main-window (&optional buffer)
  "Select the source window. If BUFFER is non-nil,
switch to that buffer in the window." 
  (select-window (window-main-window))
  (when buffer
    (switch-to-buffer buffer)))

(defun reorg--select-tree-window ()
  "Select the tree window." 
  (select-window
   (car 
    (window-at-side-list nil reorg-buffer-side))))


;;; Tree buffer 

(defun reorg--get-view-prop (&optional property)
  "Get PROPERTY from the current heading."
  (save-excursion 
    (beginning-of-line)
    (let ((props (get-text-property (point-at-bol) reorg--data-property-name)))
      (if property 
	  (plist-get props property)
	props))))


;;; Main

(defun reorg-open-sidebar (template &optional file)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (--> (reorg-parser--map-entries file)
		      (reorg--group-by it template)
		      (reorg--process-results it))))
    (when (get-buffer reorg-buffer-name)
      (kill-buffer reorg-buffer-name))
    (reorg--open-side-window)
    (reorg--select-tree-window)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (reorg-view-mode)
    (org-show-all)
    (goto-char (point-min))))

(provide 'reorg)

(cl-flet (
	  (create-stars (num &optional data)
			(make-string (if (functionp num)
					 (funcall num data)
				       num)
				     ?*)))
  (create-stars 5 nil))
