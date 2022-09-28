;;; -*- lexical-binding: t; -*-

;;; TODOs
;;;; write branch inserter

;;; requires

(require 'let-alist)
(require 'reorg-dynamic-bullets)
(require 'org-visual-indent)

;;; constants

(defconst reorg--data-property-name 'reorg-data)
;;(defconst reorg--id-property-name 'reorg-id)
(defconst reorg--field-property-name 'reorg-field-type)

;;; customs

(defcustom reorg-parser-use-id-p t
  "use id or markers?")
(defcustom reorg-buffer-name "*REORG*"
  "Default buffer name for tree view window.")
(defcustom reorg-buffer-side 'left
  "Which side for the tree buffer?")
(defcustom reorg-face-text-prop 'font-lock-face
  "When setting a face, use this text property.")
(defcustom reorg-headline-format '((stars) (" ") (headline))
  "Headline format.")

;;; variables 

(defvar-local reorg-current-template nil
  "the current template in this buffer")

(defvar reorg-parser-list nil
  "parser list")

(defvar reorg-words nil
  "A list of `reorg-words'.")

(defvar reorg--cache nil
  "The results of the parsed buffer.
For now, only for debugging purposes.")

(defvar reorg--grouped-results nil
  "The results of applying reorg--group-and-sort
to parsed data.  For now, only for debugging.")

(defvar reorg-setter-alist nil
  "setter alist")
(defvar reorg--last-point nil
  "last point (edit mode)")

;;; macros

(defmacro reorg--with-restore-state (&rest body)
  "do BODY while saving, excursion, restriction, etc."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction       
       (widen)
       (let ((inhibit-field-text-motion t))
	 ,@body))))

(defmacro reorg-set-when (var &rest body)
  "Example:
(let ((x 5))
  (reorg-set-when x
   		  ((< x 10) (+ 10 x))
		  ((> x 20) 'a)
		  ((< x 100) 'b)
		  ((eq 'b x) \"fuck you\"))
  x)
;; => \"fuck you\"
"
  `(progn
     ,@(cl-loop for each in body
		collect `(if ,(car each)
			     (setf ,var ,@(cdr each))
			   ,var))))

;;; utilites

(defun reorg--add-remove-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix.
Returns PROP with a colon prefix. If REMOVE is t,
then return PROP with no colon prefix."
  (pcase `(,remove ,(keywordp prop))
    (`(t t) (intern (substring (symbol-name prop) 1)))
    (`(nil nil) (intern (concat ":" (symbol-name prop))))
    (_ prop)))

(defun reorg--to-string (arg)
  "Convert ARG to a string."
  (pcase arg
    ((pred null) nil)
    ((pred numberp) (number-to-string arg))
    ((pred stringp) arg)
    ((pred symbolp) (symbol-name arg))
    (_ arg)))

(defun reorg--plist-p (data)
  "Is DATA a plist based on:
1. Is it a list?
2. Is the first element a keyword?
3. Are there an even number of elements?"
  (and (listp data)
       (keywordp (car data))
       (evenp (length data))))

(defun reorg--id-p (data)
  "Is DATA an org-id?"
  (and (stringp data)
       (string-match "[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+" data)))

;;; parsing functions

;;;; headlines

(defun reorg--goto-headline-start ()
  (save-match-data 
    (goto-char (org-entry-beginning-position))
    (re-search-forward "^\\*+[[:space:]]" nil t)
    (backward-char 1)
    (point)))

(defun reorg--get-headline-start ()
  (save-excursion (reorg--goto-headline-start)))

;;;; property drawer

(defun reorg-parser--get-property-drawer ()
  "asdf"
  (save-excursion
    (org-back-to-heading)
    (let (seen-base props)
      (while (re-search-forward org-property-re (org-entry-end-position) t)
	(let* ((key (upcase (match-string-no-properties 2)))
	       (extendp (string-match-p "\\+\\'" key))
	       (key-base (if extendp (substring key 0 -1) key))
	       (value (match-string-no-properties 3)))
	  (cond
	   ((member-ignore-case key-base org-special-properties))
	   (extendp
	    (setq props
		  (org--update-property-plist key value props)))
	   ((member key seen-base))
	   (t (push key seen-base)
	      (let ((p (assoc-string key props t)))
		(if p (setcdr p (concat value " " (cdr p)))
		  (unless (or (null key)
			      (equal "" key)
			      (equal "PROPERTIES" key)
			      (equal "END" key))
		    (setq props (append (list
					 (reorg--add-remove-colon (intern (downcase key)))
					 value)
					props)))))))))
      props)))

;;;; timestamps

(defun reorg--timestamp-parser (&optional inactive range)
  "Find the fist timestamp in the current heading and return it. 
if INACTIVE is non-nil, get the first inactive timestamp.  If 
RANGE is non-nil, only look for timestamp ranges."
  (save-excursion
    (cl-loop while (re-search-forward (pcase `(,inactive ,range)
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
	     when (save-match-data (not (eq (car (org-element-at-point))
					    'planning)))
	     return (org-no-properties (match-string 0)))))
;;;; body

(defun reorg--get-body ()
  "get headings body text"
  (org-element-interpret-data
   (org-element--parse-elements (save-excursion (org-back-to-heading)
						(org-end-of-meta-data t)
						(point))
				(or (save-excursion (outline-next-heading))
				    (point-max))
				'first-section nil nil nil nil)))
;;; data macro

(cl-defmacro reorg-create-data-type (&optional &key
					       name
					       get
					       getter
					       set
					       parse
					       validate
					       disabled
					       display-prefix
					       display-suffix
					       display
					       face
					       editable
					       display
					       orig-entry-func 
					       field-keymap
					       heading-keymap
					       post-edit-hook
					       pre-edit-hook
					       &allow-other-keys)
  `(progn
     (let ((data (list 
		  :parse (lambda () ,parse)
		  :get (lambda (id &rest args)			 
			 (reorg--with-point-at-orig-entry id buffer
							  ,get))
		  :getter (lambda (plist arg)
			    ,getter)		  
		  :get-view-string (lambda ()
				     (pcase-let ((`(,start . ,end)
						  (reorg--get-field-bounds)))
				       (buffer-substring start end)))
		  :validate (lambda (val &rest args)
			      ,validate)
		  :orig-entry-func (cl-defun ,(intern (concat "reorg-display-orig--"  (symbol-name name))) (plist &rest args)
				     ,@orig-entry-func)
		  :display (cl-defun ,(intern (concat "reorg-display--"  (symbol-name name))) (plist &rest args)
			     (let ((val (plist-get plist (or (when ',getter
							       ,getter)
							     ,(reorg--add-remove-colon name)))))
			       (when ',display (setq val (apply (lambda (&rest args) ,display) args)))
			       (when ',face (setq val (propertize
						       val
						       'font-lock-face
						       (cond ((internal-lisp-face-p ',face)
							      ',face)
							     ((functionp ',face)
							      (funcall ',face plist))
							     (t (error "invalid face specification"))))))
			       (setq val (concat ,display-prefix val ,display-suffix))
			       (setq val (propertize val 'reorg-field-type ',name))
			       (setq val (propertize val 'field (list 'reorg ',name)))
			       (setq val (propertize val 'front-sticky t))
			       (when ',field-keymap 
				 (setq val (propertize
					    val
					    'keymap
					    (let ((map (make-sparse-keymap)))
					      ,@(cl-loop
						 for key in field-keymap
						 collect `(define-key map
							    (kbd ,(car key))
							    ',(cdr key)))
					      map))))
			       val
			       )))))
       ;; header keymaps come last

       (if ',disabled
	   (progn 
	     (cl-loop for (key . func) in ',field-keymap
		      do (advice-remove func #'reorg--refresh-advice))
	     (setq reorg-parser-list (remove ',name reorg-parser-list)))
	 ;; (cl-loop for (key . func) in ',field-keymap
	 ;; 	  do (advice-add func :around #'reorg--refresh-advice))

	 (if (alist-get ',name reorg-setter-alist)
	     (setf (alist-get ',name reorg-setter-alist) ,set)
	   (push (cons ',name ,set) reorg-setter-alist))
	 
	 (if (alist-get ',name reorg-parser-list)
	     (setf (alist-get ',name reorg-parser-list) (plist-get data :parse))
	   (push (cons ',name (plist-get data :parse)) reorg-parser-list))))))


;;; data macro application 

(reorg-create-data-type :name body
			:parse (reorg--get-body)
			:disabled t)

(reorg-create-data-type :name deadline
			:parse (org-entry-get (point) "DEADLINE")
			:set (lambda ()
			       (reorg--with-source-and-sync
				 (if val (org-deadline nil val)
				   (org-deadline '(4)))))
			;; :display (if (plist-get plist :deadline)
			;; 	     (concat 
			;; 	      (propertize "DEADLINE: "

			;; 			  'font-lock-face 'org-special-keyword)
			;; 	      (propertize (plist-get plist :deadline)
			;; 			  'font-lock-face 'org-date))
			;; 	   "__________")
			:display (when (plist-get plist :deadline)
				   (string-pad 
				    (ts-format "%B %e, %Y" ;
					       (ts-parse-org (plist-get plist :deadline)))
				    18
				    nil t)))

			
(reorg-create-data-type :name scheduled
			:parse (org-entry-get (point) "SCHEDULED")
			:set (lambda ()
			       (reorg--with-source-and-sync
				 (if val (org-scheduled nil val)
				   (org-scheduled '(4)))))
			:display (if (plist-get plist :scheduled)
				     (concat 
				      (propertize "SCHEDULED: "

						  'font-lock-face 'org-special-keyword)
				      (propertize (plist-get plist :scheduled)
						  'font-lock-face 'org-date))
				   "__________"))

(reorg-create-data-type :name headline
			:set (lambda ()
			       (let ((val (field-string-no-properties)))
				 (reorg--with-source-and-sync val
				   (org-edit-headline val))))
			:face org-level-3
			:parse (org-no-properties
				(org-get-heading t t t t)))

(reorg-create-data-type :name property
			:parse (reorg-parser--get-property-drawer)
			:set (lambda ()
			       (reorg--with-source-and-sync
				 (let* ((pair (split-string val ":" t " "))
					(key (upcase (car pair)))
					(val (cadr pair)))
				   (org-set-property key val))))
			:display (let* ((key (reorg--add-remove-colon (car args) t))
					(val (plist-get (plist-get plist :property)
							(reorg--add-remove-colon key))))
				   (concat
				    (propertize (format "%s:" key) 'font-lock-face 'org-special-keyword)
				    " "
				    (propertize (format "%s" val) 'font-lock-face 'org-property-value)))
			:field-keymap (("C-c C-x p" . org-set-property)))

(reorg-create-data-type :name tags
			:parse (org-get-tags-string)
			:get (org-get-tags-string)
			;; :set (org-set-tags val)
			:face org-tag-group
			:heading-keymap (("C-c C-c" . org-set-tags-command)))

(reorg-create-data-type :name todo
			:parse (org-entry-get (point) "TODO")
			:get (org-entry-get (point) "TODO")			
			;; :set (org-todo val)
			:display (when-let ((s (plist-get plist :todo)))
				   (propertize
				    s
				    'font-lock-face
				    (org-get-todo-face s)))
			:heading-keymap (("C-c C-t" . org-todo)
					 ("S-<right>" . org-shiftright)
					 ("S-<left>" . org-shiftleft)))



(reorg-create-data-type :name timestamp
			:parse (when (reorg--timestamp-parser)
				 (org-no-properties (reorg--timestamp-parser)))
			:get (reorg--timestamp-parser)
			;; :set (if-let* ((old-val (reorg--timestamp-parser)))
			;; 	 (when (search-forward old-val (org-entry-end-position) t)
			;; 	   (replace-match (concat val)))
			;;        (when val
			;; 	 (org-end-of-meta-data t)
			;; 	 (insert (concat val "\n"))
			;; 	 (delete-blank-lines)))
			:display
			(if (plist-get plist :timestamp)
			    (concat 
			     (propertize (plist-get plist :timestamp)
					 'font-lock-face 'org-date))
			  "____")
			:field-keymap (("S-<up>" . org-timestamp-up)
				       ("S-<down>" . org-timestamp-down))
			:header-keymap (("C-c ." . org-time-stamp))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string)))

(reorg-create-data-type :name timestamp-ia
			:parse (when (reorg--timestamp-parser t)
				 (org-no-properties (reorg--timestamp-parser t)))
			:get (reorg--timestamp-parser t)
			;; :set (if-let* ((old-val (reorg--timestamp-parser t)))
			;; 	 (when (search-forward old-val (org-entry-end-position) t)
			;; 	   (replace-match (concat val)))
			;;        (when val
			;; 	 (org-end-of-meta-data t)
			;; 	 (insert (concat val "\n"))
			;; 	 (delete-blank-lines)))
			:face org-date
			:field-keymap (("S-<up>" . org-timestamp-up)
				       ("S-<down>" . org-timestamp-down))
			:header-keymap (("C-c ." . org-time-stamp))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string)))

(reorg-create-data-type :name timestamp-ia-range
			:parse (when (reorg--timestamp-parser t t)
				 (org-no-properties (reorg--timestamp-parser t t)))
			:get (reorg--timestamp-parser t)
			;; :set (if-let* ((old-val (reorg--timestamp-parser t)))
			;; 	 (when (search-forward old-val (org-entry-end-position) t)
			;; 	   (replace-match (concat val)))
			;;        (when val
			;; 	 (org-end-of-meta-data t)
			;; 	 (insert (concat val "\n"))
			;; 	 (delete-blank-lines)))
			:face org-date
			:keymap (("S-<up>" . org-timestamp-up)
				 ("S-<down>" . org-timestamp-down))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string)))

(reorg-create-data-type :name timestamp-range
			:parse (when (reorg--timestamp-parser nil t)
				 (org-no-properties (reorg--timestamp-parser nil t)))
			:get (reorg--timestamp-parser t)
			;; :set (if-let* ((old-val (reorg--timestamp-parser t)))
			;; 	 (when (search-forward old-val (org-entry-end-position) t)
			;; 	   (replace-match (concat val)))
			;;        (when val
			;; 	 (org-end-of-meta-data t)
			;; 	 (insert (concat val "\n"))
			;; 	 (delete-blank-lines)))
			:face org-date
			:keymap (("S-<up>" . org-timestamp-up)
				 ("S-<down>" . org-timestamp-down))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string))
			:disabled t)

(reorg-create-data-type :name id
			:parse (org-id-get-create))

(reorg-create-data-type :name category-inherited
			:parse (org-entry-get-with-inheritance "CATEGORY"))

(reorg-create-data-type :name category
			:parse (org-get-category))
			;; :set (org-set-property "CATEGORY" val))

(reorg-create-data-type :name file
			:parse (buffer-file-name)
			:disabled t
			:disable t)

(reorg-create-data-type :name buffer-name
			:parse (buffer-name))

(reorg-create-data-type :name buffer
			:parse (current-buffer))

(reorg-create-data-type :name level
			:parse (org-current-level))

(reorg-create-data-type :name priority
			:parse (org-entry-get (point) "PRIORITY")
			:display (pcase (plist-get plist :priority)
				   ("A" "⚡")
				   ("B" "⚐")
				   ("C" "")
				   (_ " ")))


;;; parsing org file

(defun reorg--parser ()
  "Create a plist using `reorg-parser-list' for each org heading."
  (cl-loop with result = nil
	   for (name . func) in reorg-parser-list
	   append (list (reorg--add-remove-colon name) (funcall func)) into result
	   finally return result)) 

(defun reorg--map-entries (&optional match scope &rest skip)
  "Run the parser at each heading in the current buffer.
See `org-map-entries' for explanation of the parameters."
  (reorg--with-restore-state   
   (org-map-entries
    #'reorg--parser match scope skip)))

(defun reorg--org-ql ()
  (interactive)
  (org-ql-select nil nil
    :action #'reorg--parser))

;;; grouping and sorting parsed results
;;;; let-plist 

(cl-defmacro reorg--let-plist (plist &rest body)
  `(cl-labels ((plist-p
                (lst)
                (and (listp lst) (keywordp (car lst))))
	       (decolon
                (lst)
                (if (plist-p lst)
                    (cl-loop for (key value) on lst by #'cddr
			     for tail = (intern (cl-subseq (symbol-name key) 1))
			     collect (cons tail (decolon value)))
                  lst)))
     (let-alist (decolon ,plist) ,@body)))

;;;; grouping and sorting 

(defun reorg--multi-sort (functions-and-predicates sequence)
  "FUNCTIONS-AND-PREDICATES is an alist of functions and predicates.
It uses the FUNCTION and PREDICATE arguments useable by `seq-sort-by'.
SEQUENCE is a sequence to sort."
  (seq-sort 
   (lambda (a b)
     (cl-loop for (func . pred) in functions-and-predicates	      
	      unless (equal (funcall func a)
			    (funcall func b))
	      return (funcall pred
			      (funcall func a)
			      (funcall func b))))
   sequence))

(cl-defgeneric reorg--seq-group-by (form sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall `(lambda (e)
			     (reorg--let-plist e ,form))
			  elt))
	    (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
	 (push (list key elt) acc))
       acc))
   (seq-reverse sequence)
   nil))

(cl-defun reorg--group-and-sort (list template &optional (n 0 np))
  "Group RESULTS according to TEMPLATE."
  (let ((copy (copy-tree list)))
    (cl-labels ((doloop (data
			 template
			 &optional (n 0 np)
			 result-sorters
			 grouper-list
			 grouper-list-results
			 format-string
			 (level 1))
			(let ((grouper (plist-get template :group))
			      (children (plist-get template :children))
			      (heading-sorter (plist-get template :sort))
			      (heading-sort-getter (or (plist-get template :sort-getter)
						       #'car))
			      (format-string (or (plist-get template :format-string)
						 format-string
						 reorg-headline-format))
			      (result-sort (plist-get template :sort-results)))			  
			  (when result-sort
			    (setq result-sorters
				  (append result-sorters					  
					  (cl-loop for (form . pred) in result-sort
						   collect (cons `(lambda (x)
								    (reorg--let-plist x
										      ,form))
								 pred)))))
			  (unless np
			    (let ((old (cl-copy-list data)))
			      (setcar data '_)
			      (setcdr data (list old))))
			  (setf (nth n (cdr data))
				(--> (nth n (cdr data))
				     (cond ((functionp grouper)
					    (->> it
						 (seq-group-by grouper)
						 (seq-map (lambda (x) (list (car x) (cdr x))))))
					   ((stringp grouper)
					    (list (list grouper it)))
					   (t (->> it
						   (reorg--seq-group-by grouper)
						   (seq-map (lambda (x) (list (car x) (cdr x)))))))
				     (seq-filter (lambda (x) (and (not (null (car x)))
								  (not (null (cdr x)))
								  (not (null x))))
						 it)
				     (cl-loop for x in it					      
					      do (setf (car x)
						       (list :branch-name (car x)
							     :headline (car x)
							     :reorg-branch t
							     :result-sorters result-sorters 
							     :grouper-list `(lambda (x)
									      (reorg--let-plist x
												,grouper))
							     :branch-predicate `(lambda (x)
										  (reorg--let-plist x
												    ,grouper))
							     :branch-result (car x)
							     :grouper-list-results (car x)
							     :format-string format-string
							     :result-sorters result-sorters
							     :template template 
							     :children children
							     :branch-sorter heading-sorter
							     :branch-sort-getter heading-sort-getter
							     :reorg-level level))
					      finally return it)
				     (seq-filter (lambda (x) (and (not (null (car x)))
								  (not (null (cdr x)))
								  (not (null x))))
						 it)
				     (cl-loop for x in it
					      do (setf (car x) (reorg--create-headline-string (car x)
											      format-string
											      level))
					      finally return it)
				     (if heading-sorter
					 (seq-sort-by (lambda (y) (funcall heading-sort-getter (car y)))
						      heading-sorter
						      it)
				       it)))
			  (if children
			      (progn 
				(cl-loop for x below (length (nth n (cdr data)))
					 do (setcdr (nth x (nth n (cdr data)))
						    (cl-loop for z below (length children)
							     collect (seq-copy (cadr (nth x (nth n (cdr data))))))))
				(cl-loop for x below (length children)
					 do (cl-loop
					     for y below (length (nth n (cdr data)))
					     do (doloop
						 (nth y (nth n (cdr data)))
						 (nth x children)
						 x
						 result-sorters
						 grouper-list
						 (append grouper-list-results
							 (list (plist-get (car (nth y (nth n (cdr data))))
									  :branch-value)))
						 format-string
						 (1+ level)))))
			    (when result-sorters
			      (cl-loop for x below (length (nth n (cdr data)))
				       do (setf (cadr (nth x (nth n (cdr data))))
						(reorg--multi-sort result-sorters
								   (cadr (nth x (nth n (cdr data))))))))
			    (cl-loop for x below (length (nth n (cdr data)))
				     do (setf (cadr (nth x (nth n (cdr data))))
					      (cl-loop for each in (cadr (nth x (nth n (cdr data))))
						       collect (reorg--create-headline-string each format-string (1+ level)))))))))
      (doloop copy template)
      (setq yyy copy)
      (cadr copy))))

;;; Generating the outline

;;;; cloning outline

(defun reorg--clone-outline-results (data &optional format-string)
  "asdf"
  (cl-loop for d in data
	   collect (reorg--create-headline-string d format-string (plist-get d :level))))

;;;; process results

(defun reorg--process-results (data &optional format-string)
  "Process the results of `reorg--group-and-sort' and turn them into orgmode headings."
  (setq format-string (or format-string reorg-headline-format))
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

;;;; Creating headlines from headline template 

(defun reorg--create-headline-string (data format-string &optional level)
  (cl-flet ((create-stars (num &optional data)
			  (make-string (if (functionp num)
					   (funcall num data)
					 num)
				       ?*)))
    (propertize 
     (if (plist-get data :reorg-branch)
	 (propertize 
	  (concat (create-stars level) " " (plist-get data :branch-name))
	  reorg--field-property-name
	  'branch)
       (cl-loop for each in format-string
		if (stringp (car each))
		concat (car each)
		else if (eq 'stars (car each))
		concat (propertize (create-stars level) reorg--field-property-name 'stars) 
		else if (eq 'property (car each))
		concat (apply (intern (concat "reorg-display--" (symbol-name (car each))))
			      data
			      (cdr each))
		else if (eq 'align-to (car each))
        	concat (propertize  " " 'display `(space . (:align-to ,(cadr each))))
		else if (eq 'pad (car each))
		concat (make-string (cadr each) ? )
		else
		concat (apply (intern (concat "reorg-display--" (symbol-name (car each))))
			      data
			      (cdr each))))
     reorg--data-property-name
     data)))

;;; Insert headlines into buffer

(defun reorg--insert-org-headlines (data)
  "Insert grouped and sorted data into outline."
  (let (results)
    (cl-labels ((recurse (data)
			 (cond ((stringp data)
				(insert data "\n"))
			       (data (cl-loop for entry in data
					      do (recurse entry))))))
      (recurse data))))

;;; window control

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

;;; view buffer

(defun reorg--get-view-props (&optional point &rest props)
  "Get text property PROPS at point. If there are multiple PROPS,
get nested properties."
  (cl-labels ((get-props (props &optional payload)
			 (if props 
			     (let ((props (if (listp props) props (list props))))
			       (if (not payload)
				   (->> (get-text-property (or point (point)) (car props))
					(get-props (cdr props)))
				 (->> (plist-get payload (car props))
				      (get-props (cdr props)))))
			   payload)))
    (if props 
	(get-props props)
      (let ((inhibit-field-text-motion t))
	(get-text-property (or point (point)) reorg--data-property-name)))))

(defun reorg--get-view-prop (&optional property)
  "Get PROPERTY from the current heading."
  (save-excursion 
    (beginning-of-line)
    (let ((props (get-text-property (point-at-bol) reorg--data-property-name)))
      (if property 
	  (plist-get props property)
	props))))

(defun reorg-outline-level ()
  "Get the outline level of the heading at point."
  (save-excursion
    (let ((search-invisible t))
      (outline-back-to-heading t)
      (re-search-forward "^*+ " (point-at-eol))
      (1- (length (match-string 0))))))

(defun reorg--get-field-at-point (&optional point)
  "Get the reorg-field-type at point."
  (get-text-property (or point (point)) reorg--field-property-name))

(defun reorg--get-field-bounds ()
  "Get the bounds of the field at point."
  (when-let ((field (reorg--get-field-at-point)))
    (cons
     (save-excursion 
       (cl-loop while (and (equal (reorg--get-field-at-point)
				  field)
			   (not (bobp)))
		do (forward-char -1)
		finally return (1+ (point))))
     (save-excursion 
       (cl-loop while (and (equal (reorg--get-field-at-point)
				  field)
			   (not (eobp)))
		
		do (forward-char 1)
		finally return (point))))))
;;; main

(defun reorg-open-sidebar-clone (&optional file)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (--> (reorg--map-entries file)
		      (reorg--clone-outline-results it '((stars) (" ") (headline))))))
    (when (get-buffer reorg-buffer-name)
      (kill-buffer reorg-buffer-name))
    (reorg--open-side-window)
    (reorg--select-tree-window)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (reorg-view-mode)
    (reorg-dynamic-bullets-mode)
    (org-visual-indent-mode)    
    (goto-char (point-min))))

(cl-defun reorg-open-sidebar (&key file template)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (with-current-buffer (find-file-noselect file)
		   (--> (reorg--map-entries)
			(setq xxx it)
			(reorg--group-and-sort it template)))))
    (when (get-buffer reorg-buffer-name)
      (kill-buffer reorg-buffer-name))
    (reorg--open-side-window)
    (reorg--select-tree-window)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (reorg-view-mode)
    (reorg-dynamic-bullets-mode)
    (org-visual-indent-mode)
    (toggle-truncate-lines 1)
    (setq reorg-current-template template)
    (goto-char (point-min))))

  (defun reorg-open-sidebar-fundamental (template &optional format-string file)
    "Open this shit in the sidebar."
    (interactive)
    (let ((results (--> (reorg--map-entries file)
			(reorg--group-and-sort it template)
			(reorg--process-results it format-string))))
      (when (get-buffer reorg-buffer-name)
	(kill-buffer reorg-buffer-name))
      (reorg--open-side-window)
      (reorg--select-tree-window)
      (let ((inhibit-read-only t))
	(erase-buffer))
      (reorg--insert-org-headlines results)
      (fundamental-mode)))

;;; reorg-views
;;;; clone functions

(defun reorg--jump-to-next-clone (&optional id previous)
  "Move to the next clone of the current node."
  (interactive)
  (let ((func (if previous
		  #'text-property-search-backward
		#'text-property-search-forward))
	(id (or id (reorg--get-view-prop :id))))
    (if (funcall func reorg--data-property-name
		 id
		 (lambda (val plist)
		   (string= 
		    (plist-get plist :id)
		    val))
		 'not-current)
	(let ((point (point)))
	  (when previous (backward-char))
	  (outline-back-to-heading)
	  (save-excursion 
	    (reorg--unfold-at-point point)
	    (reorg-edits--update-box-overlay)
	    point))
      (if previous
	  (goto-char (point-max))
	(goto-char (point-min)))
      (reorg--jump-to-next-clone id previous)))
  (reorg-edits--post-field-navigation-hook))

(defun reorg--jump-to-previous-clone (&optional id)
  "Jump to previous clone"
  (interactive)
  (reorg--jump-to-next-clone id 'previous))

;;;; view buffer functions

(defun reorg-view--update-highlight-overlay (&optional &rest _args)
  "update transclusion overlay."
  nil)
;; (delete-overlay reorg-current-heading-overlay)
;; (move-overlay reorg-current-heading-overlay (reorg--get-headline-start) (point-at-eol)))

(defun reorg--initialize-overlay ()
  "initialize the transclusion overlay."
  nil)
;; (setq reorg-current-heading-overlay
;; 	(make-overlay 1 2))
;; (overlay-put reorg-current-heading-overlay
;; 	       'face
;; 	       'reorg-current-heading-face)
;; (overlay-put reorg-current-heading-overlay 'insert-behind-hooks '(reorg--transclusion-logger
;; 								    reorg-view--update-highlight-overlay
;; 								    reorg--modification-hook-func))
;; (overlay-put reorg-current-heading-overlay 'insert-in-front-hooks '(reorg--transclusion-logger reorg--modification-hook-func))
;; (overlay-put reorg-current-heading-overlay 'modification-hooks '(reorg--transclusion-logger reorg--modification-hook-func))
;; (delete-overlay reorg-current-heading-overlay))

(defun reorg-view--update-view-headline ()
  "Goto source buffer, re-parse, update."
  (let ((props (reorg--with-point-at-orig-entry nil nil
						(reorg-parser--headline-parser)))
	(inhibit-modification-hooks t))
    (reorg-props 'headline :val (propertize (plist-get props :headline)
					    reorg--data-property-name props))))

(defun reorg-view--tree-to-source--goto-heading (&optional id buffer no-narrow no-select)
  "Goto ID in the source buffer. If NARROW is non-nil, narrow to the heading."
  (interactive)
  (when  (and (or buffer (reorg--get-view-prop :buffer))
	      (or id (reorg--get-view-prop :id)))
    (if reorg-parser-use-id-p 
	(reorg-view--goto-source-id
	 (or buffer (reorg--get-view-prop :buffer))
	 (or id (reorg--get-view-prop :id))
	 (not no-narrow))
      (reorg-view--goto-source-marker 
       (or buffer (reorg--get-view-prop :buffer))
       (or id (reorg--get-view-prop :marker))
       (not no-narrow)))))

(defun reorg-view--source--goto-end-of-meta-data ()
  "Go to the end of the meta data and insert a blank line
if there is not one."
  (let ((next-heading (reorg--with-restore-state
		       (outline-next-heading)
		       (point)))
	(end-of-meta-data (save-excursion
			    (org-end-of-meta-data t)
			    (re-search-backward (rx (not whitespace)))
			    (match-end 0))))

    (if (= (- next-heading end-of-meta-data) 1)
	(progn (goto-char end-of-meta-data)
	       (insert "\n"))
      (goto-char (1+ end-of-meta-data)))))

(defun reorg-view--source--narrow-to-heading ()
  "Narrow to the current heading only, i.e., no subtree."
  (org-back-to-heading)
  (org-narrow-to-element))

(defun reorg-view--goto-source-id (buffer id &optional narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (with-current-buffer buffer 
    (let ((old-point (point))
	  (search-invisible t))
      (widen)
      (goto-char (point-min))
      (if (re-search-forward id nil t)
	  (when narrow
	    (reorg-view--source--narrow-to-heading))
	t)
      (goto-char old-point)))
  (reorg--select-main-window)
  (set-window-buffer (selected-window) buffer))


(defun reorg-view--goto-source-marker (buffer marker &optional narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (reorg--select-main-window)
  (set-window-buffer (selected-window) buffer)
  (widen)
  (goto-char (marker-position marker))
  (when narrow
    (reorg-view--source--narrow-to-heading)
    t)
  nil)

(defun reorg--move-to-next-entry-follow ()
  (interactive)
  (outline-next-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-previous-entry-follow ()
  (interactive)
  (outline-previous-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-next-entry-no-follow ()
  (interactive)
  (outline-next-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (org-back-to-heading)
  (reorg--select-tree-window))

(defun reorg--move-to-previous-entry-no-follow ()
  (interactive)
  (outline-previous-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (org-back-to-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--goto-next-parent ()
  "Goto the next parent."
  (interactive)
  (when (re-search-forward (concat "^*\\{" (number-to-string (1- (outline-level))) "\\} ") nil t)
    (beginning-of-line)
    (reorg-edits--post-field-navigation-hook)
    (reorg-view--update-highlight-overlay)
    (reorg-edits--post-field-navigation-hook)))

(defun reorg--goto-parent ()
  "Goto the next parent."
  (interactive)
  (org-up-heading-safe)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook))

;;;; updating the tree

;; (defun reorg--update-this-heading ()
;;   "Update heading at point and all clones."
;;   (let ((data 
;; 	 (reorg--with-point-at-orig-entry (reorg--get-view-prop :id)
;; 					  (reorg--get-view-prop :buffer)
;; 					  (reorg--parser)))
;; 	(id (reorg--get-view-prop :id)))
;;     (reorg--select-tree-window)
;;     (save-restriction
;;       (save-excursion    
;; 	(reorg--map-id id
;; 		       (reorg-views--replace-heading data)
;; 		       (reorg-dynamic-bullets--fontify-heading))))))

(defun reorg--update-this-heading (data template)
  "Delete the heading and all clones, re-insert them into the outline,
move to the first new entry."
  (let ((disable-point-adjustment t)
	(search-invisible t)
	(id (reorg--get-view-prop :id)))
    (reorg--select-tree-window)
    (reorg--map-id (plist-get data :id)
		   (reorg-views--delete-leaf)
		   (reorg-views--delete-headers-maybe))
    (reorg--branch-insert--drop-into-outline data
					     template)))
		       




;;;; view mode

;; (defun reorg--shift-up (arg)
;;   "Shift priority or timestamp."
;;   (interactive "P")
;;   (pcase (reorg-edits--get-field-type)
;;     ((or 'deadline
;; 	 'scheduled
;; 	 'timestamp
;; 	 'timestamp-ia)
;;      (org-timestamp-up arg))
;;     (`priority
;;      (org-priority-up))))

;; (defun reorg--shift-down (arg)
;;   "Shift priority or timestamp."
;;   (interactive "P")
;;   (pcase (reorg-edits--get-field-type)
;;     ((or 'deadline
;; 	 'scheduled
;; 	 'timestamp
;; 	 'timestamp-ia)
;;      (org-timestamp-down arg))
;;     (`priority
;;      (org-priority-down))))

(defvar reorg-view-mode-map 
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") #'reorg-view--tree-to-source--goto-heading)
    (define-key map (kbd "e") #'reorg-edits--start-edit)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "f") #'reorg-edits-move-to-next-field)
    ;; (define-key map (kbd "S-<up>") #'reorg--shift-up)
    ;; (define-key map (kbd "S-<down>") #'reorg--shift-down)
    (define-key map (kbd "b") #'reorg-edits-move-to-previous-field)
    (define-key map (kbd "c") #'reorg--jump-to-next-clone)
    ;;(define-key map (kbd "C") #'reorg--jump-to-previous-clone)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map (kbd "n") #'reorg--move-to-next-entry-no-follow)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry-no-follow)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle)
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "keymap")

(defmacro reorg--with-source-buffer (&rest body)
  "Execute BODY in the source buffer and
update the heading at point."
  (declare (indent defun))
  `(progn
     (let ((val (field-string-no-properties)))
       (reorg-view--tree-to-source--goto-heading)
       (save-restriction
	 (save-excursion 
	   ,@body)))))

(defmacro reorg--with-source-and-sync (&rest body)
  "Execute BODY in the source buffer and
update the heading at point."
  (declare (indent defun))
  `(progn
     (let ((val (field-string-no-properties))
	   (inhibit-field-text-motion t)
	   (search-invisible t)
	   data)
       (reorg-view--tree-to-source--goto-heading)
       (org-with-wide-buffer
	(org-back-to-heading)
	,@body
	(setq data (reorg--parser)))
       (reorg--select-tree-window)
       (reorg--map-id (plist-get data :id)
		      (reorg-views--delete-leaf)
		      (reorg-views--delete-headers-maybe))
       (reorg--branch-insert--drop-into-outline data
						reorg-current-template))))


(define-derived-mode reorg-view-mode
  fundamental-mode
  "Org tree view"
  "Tree view of an Orgmode file. \{keymap}"
  (reorg--initialize-overlay)
  (setq cursor-type 'box)
  ;;(setq-local disable-point-adjustment t)
  (use-local-map reorg-view-mode-map)
  (add-hook 'post-command-hook #'reorg-edits--update-box-overlay nil t))

;;; reorg-edit-mode
;;;; transclusion hook

(defun reorg--modification-hook-func (overlay postp beg end &optional length)
  "overlay post change hook."
  (when postp
    (save-match-data
      (let* ((overlay-beg (overlay-start overlay))
	     (headline-beg (reorg--get-headline-start))
	     (relative-beg (if (<= (- beg headline-beg) 0)
			       0
			     (- beg headline-beg)))
	     (adjustment (if (< beg overlay-beg)
			     (- beg overlay-beg)
			   0)))
	(cond
	 ((and (= beg end) (> length 0))
	  (reorg--with-point-at-orig-entry nil nil
					   (reorg--goto-headline-start)
					   (forward-char relative-beg)
					   (delete-region (point) (+ (point) length))))	 
	 ((and (/= beg end) (> length 0))
	  (let ((s (buffer-substring-no-properties (+ overlay-beg
						      relative-beg) end)))
	    (message s)
	    (reorg--with-point-at-orig-entry nil nil
					     (reorg--goto-headline-start)
					     (forward-char relative-beg)
					     (delete-region (point) (+ (point)
								       (+ length adjustment)))
					     (insert s))))	 
	 ((or (= length 0) (/= beg end))
	  (let ((s (buffer-substring-no-properties beg end)))
	    (reorg--with-point-at-orig-entry nil nil
					     (reorg--goto-headline-start)
					     (forward-char relative-beg)
					     (insert s)))))))))


;;;; customs

(defcustom reorg-edits-commit-edit-shortcut "C-c C-c"
  "Shortcut to commit edits when in `reorg-edits-mode'
Accepts any string acceptable to `kbd'."
  :type 'string)

(defcustom reorg-edits-abort-edit-shortcut "C-c C-k"
  "Shortcut to abort edits when in `reorg-edits-mode'
Accepts any string acceptable to `kbd'."
  :type 'string)

(defcustom reorg-edits-start-edit-shortcut "C-c C-c"
  "Shortcut to initiate `reorg-edits-mode'
Accepts any string acceptable to `kbd'."
  :type 'string)

;;;; variables

(defvar reorg-edits--restore-state nil
  "When editing a clone, save the current headline and body
to restore if the edit is abandoned.")

(defvar reorg-edits--previous-header-line nil
  "previous header line")

(defvar reorg-edits--header-line
  '(:eval
    (format 
     "Editing headline. '%s' to finish and update. '%s' to abandon."
     reorg-edits-commit-edit-shortcut
     reorg-edits-abort-edit-shortcut))
  "The value of header-line-format when `reorg-edits-mode' is 
invoked.")

(defvar reorg-edits--current-field-overlay
  (let ((overlay (make-overlay 1 2)))
    (overlay-put overlay 'face '( :box (:line-width -1)
				  :foreground "cornsilk"))    
    (overlay-put overlay 'priority 1000)
    overlay)
  "Overlay for field at point.")

(defvar reorg-edits-field-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd reorg-edits-commit-edit-shortcut)
      #'reorg-edits--commit-edit)
    (define-key map (kbd reorg-edits-abort-edit-shortcut)
      #'reorg-edits--discard-edit)
    (define-key map (kbd "TAB") #'reorg-edits-move-to-next-field)
    (define-key map (kbd "BACKTAB") #'reorg-edits-move-to-previous-field)
    (define-key map [remap kill-line] #'reorg--kill-field)
    map)
  "keymap.")

;;;; macros

(defmacro reorg--with-point-at-orig-entry (id buffer &rest body)
  "Execute BODY with point at the heading with ID at point."
  `(when-let ((id ,(or id (reorg--get-view-prop :id))))
     (with-current-buffer ,(or buffer (reorg--get-view-prop :buffer))
       (reorg--with-restore-state
	(goto-char (point-min))
	;; NOTE: Can't use `org-id-goto' here or it will keep the
	;;       buffer open after the edit.  Getting the buffer
	;;       and searching for the ID should ensure the buffer
	;;       stays hidden.  It also avoids using `org-id'
	;;       for anything other than ID generation. 
	(save-match-data
	  (if (re-search-forward id)
	      (progn 
		,@body)
	    (error "Heading with ID %s not found." id)))))))

;;;; field navigation 

(defun reorg-edits--post-field-navigation-hook ()
  "Tell the user what field they are on."
  (reorg-edits--update-box-overlay)
  (setf (point) (car 
		 (reorg-edits--get-field-bounds))))

(defun reorg--unfold-at-point (&optional point)
  "Unfold so the heading at point is visible."
  (save-excursion 
    (reorg--goto-parent)
    (outline-show-subtree)
    (goto-char point)
    (outline-show-subtree)
    (goto-char point)))

(let ((point nil))
  (defun reorg-edits--update-box-overlay ()
    "Tell the user what field they are on."
    (unless (= (point) (or point 0))
      (when-let ((field (get-text-property (point) reorg--field-property-name)))
	(delete-overlay reorg-edits--current-field-overlay)
	(move-overlay reorg-edits--current-field-overlay
		      (car (reorg-edits--get-field-bounds))
		      (if (eq field 'stars)
			  (let ((inhibit-field-text-motion t))
			    (point-at-eol))
			(cdr (reorg-edits--get-field-bounds))))
	;; (message "You are on the field for the heading's %s"
	;; 	 (reorg-edits--get-field-type)))
	(setq point (point))))))

(defun reorg-edits--move-selection-overlay ()
  (if-let ((bounds (reorg-edits--get-field-bounds)))
      (move-overlay reorg-edits--current-field-overlay
		    (car bounds)
		    (cdr bounds))
    (delete-overlay reorg-edits--current-field-overlay)))

(defun reorg-edits-move-to-next-field (&optional previous)
  "Move to the next field at the current heading."
  (interactive)
  (let ((current-field (reorg-edits--get-field-at-point)))
    (unless (if previous (= (point) (org-entry-beginning-position))
	      (= (point) (org-entry-end-position)))
      (cl-loop with point = (point)
	       do (if previous (cl-decf point) (cl-incf point))
	       when (and (reorg-edits--get-field-at-point point)
			 (not (equal (reorg-edits--get-field-at-point point)
				     current-field)))
	       return (prog1 (setf (point) point)
			(reorg-edits--post-field-navigation-hook))
	       when (if previous (= point (org-entry-beginning-position))
		      (= point (org-entry-end-position)))
	       return nil)
      (reorg-edits--post-field-navigation-hook))))

(defun reorg-edits-move-to-previous-field ()
  "Move to the next field at the current heading."
  (interactive)  
  (reorg-edits-move-to-next-field 'previous))

;;;; field editing

(defun reorg--kill-field ()
  "Temporary replacement for kill-line when editing a field."
  (interactive)
  (goto-char (field-beginning))
  (funcall-interactively #'self-insert-command 1 ? )
  (delete-region (point) (field-end))
  (backward-char 1))

(defun reorg-edits--get-field-type ()
  "Get the field type at point, if any."
  (get-text-property (point) reorg--field-property-name))

(defun reorg-edit-field--replace-field-at-point (val field)
  "Replace FIELD with VAL."
  (let ((beg (field-beginning))
	(end (field-end)))
    (delete-region beg end)
    (goto-char beg)
    (insert (propertize val `(reorg--field-property-name
			      ,field
			      field
			      ,field)))))

(defun reorg-edits--get-field-value ()
  "Get the value of the field at point.  Return 
nil if there is no value."
  (field-string-no-properties))
;; (pcase-let ((`(,start . ,end) (reorg-edits--get-field-bounds)))
;;   (when (and start end)
;;     (buffer-substring-no-properties start end))))

(defun reorg-edits--commit-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (interactive)
  (let ((type (reorg-edits--get-field-type))
	(val (reorg-edits--get-field-value)))
    (funcall (alist-get type reorg-setter-alist) val)
    (reorg--update-this-heading)
    (reorg-edits-mode -1)))

;; (cl-defun reorg-get-set-props (prop &key
;; 				    (val nil valp)
;; 				    keep
;; 				    multi-value
;; 				    inherit
;; 				    literal-nil
;; 				    no-duplicates
;; 				    no-text-properties
;; 				    &allow-other-keys)
;;   "Change the org heading at point by set PROP to VAL.
;; It accepts the following properties, as well as any others that are set 
;; in the headings property drawer. Any such properties can be accessed as string
;;  or a symbol, e.g., \"CATEGORY\" or 'category.  See the return value of 
;; `reorg-parser--headline-parser' for more information.
;; There are flags for dealing with multivalued properties, inheritence, 
;; etc.:
;; If VAL is a list, assume a multi-valued property.
;; If KEEP is non-nil and VAL is a list or MULTI is non-nil, keep the old value.
;; If NO-DUPLICATES is non-nil and dealing with multi-valued, delete duplicates.
;; If MULTI is non-nil, use a multivalued property even if VAL is not a list.
;; Return a cons cell with the old value as the `car' and new value as the `cdr'."
;;   (cl-macrolet ((get-or-set (&key get set)
;; 			    `(if (not valp)
;; 				 ,get
;; 			       (let ((old-val ,get))
;; 				 (save-excursion 
;; 				   (org-back-to-heading)		 
;; 				   ,set
;; 				   (cons old-val ,get))))))
;;     (pcase prop
;;       ;;(org-insert-time-stamp (org-read-date t t "2021-01-01"))
;;       (`deadline
;;        (get-or-set :get (org-entry-get (point) "DEADLINE" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-deadline '(4))
;; 			  (org-deadline nil val))))
;;       (`scheduled
;;        (get-or-set :get (org-entry-get (point) "SCHEDULED" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-schedule '(4))
;; 			  (org-schedule nil val))))
;;       (`comment
;;        (get-or-set :get (org-in-commented-heading-p)
;; 		   :set (when (not (xor (not val)
;; 					(org-in-commented-heading-p)))
;; 			  (org-toggle-comment))))
;;       (`tags
;;        (get-or-set :get (org-get-tags (point) (not inherit))
;; 		   :set (if keep
;; 			    (org-set-tags (if no-duplicates
;; 					      (delete-duplicates (append old-val
;; 									 (-list val))
;; 								 :test #'string=)
;; 					    (append old-val (-list val))))
;; 			  (org-set-tags val))))
;;       (`headline
;;        (get-or-set :get (org-entry-get (point) "ITEM")
;; 		   ;; keep the comment if it is there
;; 		   :set (let ((commentedp (org-in-commented-heading-p)))
;; 			  (org-edit-headline val)
;; 			  (when commentedp
;; 			    (reorg-get-set-props 'comment :val t)))))
;;       (`todo
;;        (get-or-set :get (org-entry-get (point) "TODO")
;; 		   :set (org-todo val)))
;;       ((or `timestamp
;; 	   `timestamp-ia)
;;        (get-or-set :get (org-entry-get (point) (if (eq 'timestamp prop)
;; 						   "TIMESTAMP"
;; 						 "TIMESTAMP_IA"))
;; 		   :set (if (and old-val
;; 				 (search-forward old-val (org-entry-end-position) t))
;; 			    (progn (replace-match (concat val))
;; 				   (delete-blank-lines))
;; 			  (org-end-of-meta-data t)
;; 			  (delete-blank-lines)
;; 			  (when val 
;; 			    (insert (concat val "\n"))))))
;;       (`body
;;        (get-or-set :get (reorg-edits--get-body-string no-text-properties)
;; 		   :set (error "You can't set body text (yet).")))
;;       ((or (pred stringp)
;; 	   (pred symbolp))
;;        (when (symbolp prop) (setq prop (symbol-name prop)))
;;        (get-or-set :get (if (or multi-value (and val (listp val)) keep)
;; 			    (org-entry-get-multivalued-property (point) prop)
;; 			  (org-entry-get (point) prop inherit literal-nil))
;; 		   :set (cond ((or multi-value (listp val) keep)
;; 			       (apply #'org-entry-put-multivalued-property (point) prop
;; 				      (if no-duplicates
;; 					  (delete-duplicates (append old-val (-list val)) :test #'string=)
;; 					(append old-val (-list val)))))
;; 			      (t (org-entry-put (point) prop val))))))))

;; (defun reorg-edits--sync-field-with-source ()
;;   "Set SOURCE to the value of the current field."
;;   (let ((field (reorg-edits--get-field-at-point))
;; 	(val (reorg-edits--get-field-value)))
;;     (reorg--with-point-at-orig-entry nil
;; 				     (reorg-get-set-props field
;; 							  :val val ))))

(defun reorg-edits--discard-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (interactive)
  (reorg-edits-mode -1)
  (when reorg-edits--restore-state
    (reorg-views--replace-heading
     reorg-edits--restore-state))
  (setq reorg-edits--restore-state nil)
  (message "Discarded edit."))

(defun reorg-edits--start-edit ()
  "Start editing the headline at point."
  (interactive)
  (reorg-edits-mode 1))

(defun reorg-edits--move-into-field ()
  "If the point is at the border of a field, then 
move it into the field.  This ensures that `reorg-edits--get-field-bounds'
returns the correct positions."
  (cond ((and (get-text-property (point) reorg--field-property-name)
	      (not (get-text-property (1- (point)) reorg--field-property-name))
	      (get-text-property (+ 1 (point)) reorg--field-property-name))
	 (forward-char 1))
	((and (get-text-property (point) reorg--field-property-name)
	      (not (get-text-property (1+ (point)) reorg--field-property-name)))
	 (forward-char -1))))

(defun reorg-edits--get-field-at-point (&optional point)
  "Get the `reorg--field-property-name' at point."
  (get-text-property (or point (point)) reorg--field-property-name))

(defun reorg-edits--kill-line ()
  "Kill up to the end of the end point."
  (interactive)
  (pcase-let ((`(,start . ,end) (reorg-edits--get-field-bounds)))
    (delete-region start end)))

(defun reorg-edits--get-field-bounds ()
  "Get the bounds of the field at point."
  (when-let ((field (reorg-edits--get-field-at-point)))
    (cons
     (save-excursion 
       (cl-loop while (and (equal (reorg-edits--get-field-at-point)
				  field)
			   (not (bobp)))
		do (forward-char -1)
		finally return (1+ (point))))
     (save-excursion 
       (cl-loop while (and (equal (reorg-edits--get-field-at-point)
				  field)
			   (not (eobp)))
		
		do (forward-char 1)
		finally return (point))))))

(defmacro reorg-view--make-change-in-org-buffer-and-sync-clones (&rest body)
  "asdf"
  `(let (data)
     (reorg--with-point-at-orig-entry nil nil
				      ,@body
				      (setq data (reorg--parser)))
     (reorg--map-id (plist-get data :id)
		    (reorg-views--replace-heading data))))


(defun reorg-views--insert-before-point (data &optional level format-string)
  "insert a hearing before the heading at point."
  (reorg--with-restore-state
   (beginning-of-line)
   (insert "\n")
   (previous-line 1)
   (let ((string (reorg--create-headline-string data
						(or format-string reorg-headline-format)
						(or level (reorg-outline-level)))))
     (insert string)
     (reorg-dynamic-bullets--fontify-heading)
     (1+ (length string)))))



(defun reorg-views--insert-after-point (data &optional level format-string)
  "insert a heading after the current point."
  (reorg--with-restore-state
   (end-of-line)
   (insert "\n")
   (previous-line 1)
   (let ((string (reorg--create-headline-string data
						(or format-string reorg-headline-format)
						(or level (reorg-outline-level)))))
     (insert string)
     (reorg-dynamic-bullets--fontify-heading)
     (1+ (length string)))))

(defun reorg-views--delete-leaf ()
  "delete the heading at point"
  (let ((inhibit-field-text-motion t))
    (delete-region (point-at-bol)
		   (line-beginning-position 2))))

(defun reorg-views--delete-headers-maybe ()
  (cl-loop while (and (reorg--goto-next-property-field 'reorg-field-type 'branch t)
		      (not (reorg--get-next-level-branches))
		      (not (reorg-tree--branch-has-leaves-p)))
	   do (reorg-views--delete-heading)))
    


(defun reorg-views--replace-heading (data)
  "Replace the heading at point with NEW."
  (let ((level (reorg-outline-level))
	(inhibiit-field-text-motion t)
	(search-invisible t))
    (save-excursion
      (reorg-views--delete-headers-maybe)
      (reorg-views--insert-before-point data level))))

;;;; edit mode
;;;;; minor mode

(define-minor-mode reorg-edits-mode
  "Minor mode to edit headlines."
  nil
  " EDITING HEADLINE"
  reorg-edits-field-mode-map
  (when-let* ((prop (get-text-property (point) reorg--field-property-name))
	      (start (previous-single-property-change (point) reorg--field-property-name))
	      (end (next-single-property-change (point) reorg--field-property-name))
	      (val (buffer-substring start end)))
    ;; on
    (if reorg-edits-mode
	(progn		
	  ;; (reorg--add-command-hooks)
	  ;;(reorg--lock-buffer)
	  (setq cursor-type 'bar
		reorg--last-point nil
		reorg-edits--previous-header-line header-line-format
		header-line-format reorg-edits--header-line
		reorg-edits--restore-state (get-text-property (point-at-bol) 'reorg-data)
		reorg-edit-field--start-marker (set-marker (make-marker) start)
		reorg-edit-field--end-marker (set-marker (make-marker) end)
		overriding-local-map reorg-edits-field-mode-map))
      ;; off
      ;;(reorg--unlock-buffer)
      ;; (reorg--add-command-hooks 'remove)
      (delete-overlay reorg-edits--current-field-overlay)
      (setq header-line-format reorg-edits--previous-header-line
	    reorg-edits--previous-header-line nil
	    reorg-edit-field--start-marker nil
	    reorg-edit-field--end-marker nil
	    reorg-edits--restore-state nil
	    overriding-local-map nil
	    cursor-type nil))))

;;;;; keep point in field

(defun reorg--pre-command-hook ()
  "asdf"
  (interactive)
  (if (eq 'reorg (car (field-at-pos (point))))
      (setq reorg--last-point (point))))

(defun reorg--post-command-hook ()
  "zxcv"
  (when reorg--last-point
    (constrain-to-field nil reorg--last-point)
    (setq reorg--last-point nil)))

(defun reorg--add-command-hooks (&optional remove)
  "asdf"
  (setq reorg--last-point nil)
  (if remove
      (progn 
	(remove-hook 'pre-command-hook #'reorg--pre-command-hook t)
	(remove-hook 'post-command-hook #'reorg--post-command-hook t))
    (add-hook 'pre-command-hook #'reorg--pre-command-hook nil t)
    (add-hook 'post-command-hook #'reorg--post-command-hook nil t)))

;;;; get or set props 

;; (cl-defun reorg-get-set-props (prop &key
;; 				    (val nil valp)
;; 				    keep
;; 				    multi-value
;; 				    inherit
;; 				    literal-nil
;; 				    no-duplicates
;; 				    no-text-properties
;; 				    &allow-other-keys)
;;   "Change the org heading at point by set PROP to VAL.

;; It accepts the following properties, as well as any others that are set 
;; in the headings property drawer. Any such properties can be accessed as string
;;  or a symbol, e.g., \"CATEGORY\" or 'category.  See the return value of 
;; `reorg-parser--headline-parser' for more information.

;; There are flags for dealing with multivalued properties, inheritence, 
;; etc.:

;; If VAL is a list, assume a multi-valued property.
;; If KEEP is non-nil and VAL is a list or MULTI is non-nil, keep the old value.
;; If NO-DUPLICATES is non-nil and dealing with multi-valued, delete duplicates.
;; If MULTI is non-nil, use a multivalued property even if VAL is not a list.

;; Return a cons cell with the old value as the `car' and new value as the `cdr'."
;;   (cl-macrolet ((get-or-set (&key get set)
;; 			    `(if (not valp)
;; 				 ,get
;; 			       (let ((old-val ,get))
;; 				 (save-excursion 
;; 				   (org-back-to-heading)		 
;; 				   ,set
;; 				   (cons old-val ,get))))))
;;     (pcase prop
;;       ;;(org-insert-time-stamp (org-read-date t t "2021-01-01"))
;;       (`deadline
;;        (get-or-set :get (org-entry-get (point) "DEADLINE" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-deadline '(4))
;; 			  (org-deadline nil val))))
;;       (`scheduled
;;        (get-or-set :get (org-entry-get (point) "SCHEDULED" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-schedule '(4))
;; 			  (org-schedule nil val))))
;;       (`comment
;;        (get-or-set :get (org-in-commented-heading-p)
;; 		   :set (when (not (xor (not val)
;; 					(org-in-commented-heading-p)))
;; 			  (org-toggle-comment))))
;;       (`tags
;;        (get-or-set :get (org-get-tags (point) (not inherit))
;; 		   :set (if keep
;; 			    (org-set-tags (if no-duplicates
;; 					      (delete-duplicates (append old-val
;; 									 (-list val))
;; 								 :test #'string=)
;; 					    (append old-val (-list val))))
;; 			  (org-set-tags val))))
;;       (`headline
;;        (get-or-set :get (org-entry-get (point) "ITEM")
;; 		   ;; keep the comment if it is there
;; 		   :set (let ((commentedp (org-in-commented-heading-p)))
;; 			  (org-edit-headline val)
;; 			  (when commentedp
;; 			    (reorg-get-set-props 'comment :val t)))))
;;       (`todo
;;        (get-or-set :get (org-entry-get (point) "TODO")
;; 		   :set (org-todo val)))
;;       ((or `timestamp
;; 	   `timestamp-ia)
;;        (get-or-set :get (org-entry-get (point) (if (eq 'timestamp prop)
;; 						   "TIMESTAMP"
;; 						 "TIMESTAMP_IA"))
;; 		   :set (if (and old-val
;; 				 (search-forward old-val (org-entry-end-position) t))
;; 			    (progn (replace-match (concat val))
;; 				   (delete-blank-lines))
;; 			  (org-end-of-meta-data t)
;; 			  (delete-blank-lines)
;; 			  (when val 
;; 			    (insert (concat val "\n"))))))
;;       (`body
;;        (get-or-set :get (reorg-edits--get-body-string no-text-properties)
;; 		   :set (error "You can't set body text (yet).")))
;;       ((or (pred stringp)
;; 	   (pred symbolp))
;;        (when (symbolp prop) (setq prop (symbol-name prop)))
;;        (get-or-set :get (if (or multi-value (and val (listp val)) keep)
;; 			    (org-entry-get-multivalued-property (point) prop)
;; 			  (org-entry-get (point) prop inherit literal-nil))
;; 		   :set (cond ((or multi-value (listp val) keep)
;; 			       (apply #'org-entry-put-multivalued-property (point) prop
;; 				      (if no-duplicates
;; 					  (delete-duplicates (append old-val (-list val)) :test #'string=)
;; 					(append old-val (-list val)))))
;; 			      (t (org-entry-put (point) prop val))))))))

;;;; org shortcuts

(defmacro reorg--create-org-shortcut (name func shortcut)
  (progn
    (let ((func-name (intern (concat "reorg--org-shortcut-"
				     (symbol-name name)))))
      `(progn
	 (defun ,func-name (arg)
	   ,(format "Execute %s in the source buffer and update the heading at point."
		    func)
	   (interactive "P")
	   (reorg--with-source-and-sync
	     (if (eq 'org-set-property ',func)
		 (funcall-interactively #',func arg nil)
	       (funcall-interactively #',func arg))))
	 (define-key reorg-view-mode-map (kbd ,shortcut) #',func-name)))))

(reorg--create-org-shortcut tag org-set-tags-command "C-c C-c")
(reorg--create-org-shortcut todo org-todo "C-c C-t")
(reorg--create-org-shortcut deadline org-deadline "C-c C-d")
(reorg--create-org-shortcut schedule org-schedule "C-c C-s")
(reorg--create-org-shortcut property org-set-property "C-c C-x p")
(reorg--create-org-shortcut priority org-priority "C-c C-p")

;;;; inserting headers into the appropriate location

;;;;; reorg-map-branches

(defmacro reorg--map-all-branches (&rest body)
  "Move to the next clone of the current node."
  `(save-restriction 
     (save-excursion
       (goto-char (point-min))
       (while
	   (text-property-search-backward 'reorg-field-type
					  'branch
					  nil
					  'not-current)
	 ,@body))))

(defun reorg--last-branch-p ()
  "Does the current branch have any children?"
  (save-excursion 
    (forward-line)
    (not (eq 'branch (get-text-property (point) 'reorg-field-type)))))

;;;;; insert-into-branch 

(defun reorg-tree--branch-has-leaves-p ()
  (save-excursion 
    (let ((disable-point-adjustment t)
	  (inhibit-field-text-motion t))
      (forward-line)
      (not (eq 'branch
	       (reorg--get-view-props nil 'reorg-field-type))))))

(defun reorg--insert-into-leaves (data sorters &optional level format-string)
  (let ((disable-point-adjustment t)
	(format-string (or format-string (reorg--get-view-props nil 'reorg-data :format-string))))
    (when (and (null (reorg--children-p))
	       (eq (reorg--get-view-props nil 'reorg-field-type) 'branch))
      (if (reorg-tree--branch-has-leaves-p)
	  (progn 
	    (forward-line 1) ;; this should be a text-property forward, not a line forward
	    (cl-loop while (not (eq (reorg--get-view-props nil 'reorg-field-type)
				    'branch))
		     with level = (or level (reorg-outline-level))
		     if (cl-loop for (func . pred) in sorters
				 if (funcall pred
					     (funcall func data)
					     (funcall func (reorg--get-view-props)))
				 return t
				 finally return nil)
		     return (reorg-views--insert-before-point data level format-string)
		     else do (forward-line)
		     finally return (reorg-views--insert-before-point data level format-string)))
	(reorg-views--insert-after-point data (1+ level) format-string ))))
  (reorg-dynamic-bullets--fontify-heading))

(defun reorg--set-text-property (plist val &rest props)
  "Set the text property at point to a new value.
Set the text property (car PROPS) to the value of
the PROPS drilling nested plist whatever.

If PROPS is only one element then start with 'reorg-data
otherwise start with the car of PROPS.

If PLIST is nil, then get the text properties at point."
  (let* ((inhibit-field-text-motion t)
	 (results (or plist (text-properties-at (point)))))
    (cl-loop for prop in (or (butlast props) `(,reorg--data-property-name))
	     with new-results = nil
	     do (setf new-results (if prop
				      (plist-get results prop)
				    results))
	     finally return (progn (plist-put new-results (car (last props)) val)
				   (put-text-property (point-at-bol)
						      (point-at-eol)
						      reorg--data-property-name
						      new-results)
				   results))))

(defun reorg--insert-heading (&optional data level format)
  (let ((inhibit-field-text-motion t))
    (beginning-of-line)
    (insert 
     (reorg--create-headline-string (or data (reorg--get-view-props nil))
				    (or format reorg-headline-format)
				    (or level (reorg-outline-level)))
     "\n")
    (forward-line -1)
    (reorg-dynamic-bullets--fontify-heading)))




;;;; Footer

(provide 'reorg)

;;; reorg.el ends here

;;; reorg-view starts here

;;;; text property navigation 



(defun reorg--goto-previous-property-field (prop val &optional pred transformer)
  "Move to the beginning of the buffer position that
text property PROP that matches VAL.  Check for matching VAL
using `eq', unless PRED is suppied."
  (reorg-tree--goto-next-property-field nil prop val 'backward pred transformer))

(defun reorg--get-next-level-branches ()
  "Get the headlines of the next level sub-branches."
  (save-excursion 
    (let (results)
      (reorg--map-next-level (push (reorg--get-view-props nil 'reorg-data :headline)
				   results))
      results)))

;; (defun reorg--goto-previous-branch (&optional relative-level)
;;   "Wrapper for `reorg--goto-next-branch' which moves to the
;; previous branch."
;;   (reorg--goto-next-branch relative-level 'previous))

;;;; tree navigation re-write

(defmacro reorg--map-id (id &rest body)
  "Execute BODY at each entry that matches ID."
  `(progn 
     (goto-char (point-min))
     (while (text-property-search-forward reorg--data-property-name
					  ,id
					  (lambda (val plist)
					    (string= 
					     (plist-get plist :id)
					     val))
					  'not-current)
       (save-excursion 
	 (outline-back-to-heading)
	 ,@body)
       (forward-char 1))))

(defmacro reorg--map-next-level (&rest body)
  "Go to the next branch. With integer RELATIVE-LEVEL, go to the next
level relative to the current one.  For example, if the current outline
level is 2, and RELATIVE-LEVEL is -1, the function will move to the next
branch at level 1.  If RELATIVE-LEVEL is 1, the function will move to the
next branch at level 3.

If PREVIOUS is non-nil, move to the previous branch instead of the next.

Return nil if there is no such branch."
  `(let ((start-level (reorg-outline-level))
	 (point (point)))
     (cl-loop while (reorg--goto-next-relative-level 1 nil start-level)
	      if (= (1+ start-level) (reorg-outline-level))
	      do ,@body
	      else if (/= start-level (reorg-outline-level))
	      return nil)
     (goto-char point)))

;;;; reorg new tree

(defun reorg--children-p ()
  "Does the current node have sub-branches?"
  (and (eq (reorg--get-view-props nil 'reorg-field-type) 'branch)
       (reorg--get-view-props nil 'reorg-data :children)))




;; goto next header 
;; goto the next child
;; goto to last child 
;; goto next sibling
;; goto next level lower 
;; goto next last level lower


;;; inserting into branch





(defun reorg--insert-into-branch-or-make-new-branch (data &optional point)
  (let* ((children (reorg-into--get-list-of-child-branches-at-point)))
    (cl-loop with x = nil
	     with point = (or point (point))
	     do (setf (point) point)
	     for (func . results) in children
	     do (setq x (funcall func data)
		      point (point))
	     when x
	     do (if (member x results)
		    (progn 
		      (reorg-tree--goto-next-property-field nil 'reorg-data
							    x
							    nil
							    #'string=
							    (lambda (y) (plist-get y :headline)))
		      (if (reorg--children-p)
			  (reorg--insert-into-branch-or-make-new-branch data)
			(reorg--insert-into-leaves data
						   (reorg--get-view-props nil 'reorg-data :result-sorters))))
		  (reorg--insert-new-branch `( :branch-name x
					       :headline x
					       :reorg-branch t
					       :result-sorters ,result-sorters
					       :grouper-list ,func
					       :grouper-list-results x
					       :format-string 'xxx
					       :result-sorters 'xxx
					       :children 'xxx
					       :branch-value 'xxx
					       :reorg-level (reorg-current-level)))
		  (reorg--insert-into-branch-or-make-new-branch data)))))





(defun reorg-into--at-branch-p ()
  (eq (reorg--get-view-props nil 'reorg-field-type)
      'branch))

(defun reorg-into--descend-into-branch-at-point (data)
  "IDK what to do."
  (when (reorg-into--at-branch-p)   
    (pcase `(,(reorg--children-p) ,(reorg-into--member-of-this-header? data))
      (`(nil t) (reorg--insert-into-leaves data (reorg--get-view-props nil 'reorg-data :results-sorters)))
      (`(t t) (reorg--goto-next-relative-level 1) (reorg--into--descend-into-branch-at-point))
      (`(t nil) (reorg--goto-next-relative-level 0) (reorg--into--descend-into-branch-at-point))
      (`(nil nil) (reorg--goto-next-relative-level 
		   )))))

(defun reorg-into--member-of-this-header? (data)
  "Is DATA a member of the header?" 
  (equal
   (funcall
    (car (last (reorg--get-view-props nil 'reorg-data :grouper-list))) data)
   (car (last (reorg--get-view-props nil 'reorg-data :grouper-list-results)))))

;;; actions 

(defmacro reorg-tree--with-wide-buffer (&rest body)
  "Execute BODY and restore the buffer to its previous state."
  `(save-restriction
     (save-excursion
       (let ((disable-point-adjustment t))
	 ,@body))))

(defun reorg-tree--contract-node ()
  (outline-hide-subtree))

(defun reorg-tree--expand-node ()
  (outline-show-subtree))


(defun reorg-tree--is-cloned-p ()
  (when-let ((id (reorg--get-view-prop :id)))
    (setf (point) (point-min))
    (text-property-search-forward reorg--data-property-name
				  id
				  (lambda (val plist)
				    (string= 
				     (plist-get plist :id)
				     val))
				  'not-current)))


(defun reorg-tree--is-folded-p ()
  "Is the current heading folded?"
  (let ((inhibit-field-text-motion t))
    (invisible-p (point-at-eol))))

(defun reorg-tree--is-root-p ()
  (= (reorg-outline-level 1)))

(defun reorg--org-shortcut-deadline
    (arg)
  "Execute org-deadline in the source buffer and update the heading at point."
  (interactive "P")
  (reorg--with-source-and-sync
    (if
	(eq 'org-set-property 'org-deadline)
	(funcall-interactively #'org-deadline arg nil)
      (funcall-interactively #'org-deadline arg))))
