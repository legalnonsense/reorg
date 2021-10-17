;;; -*- lexical-binding: t; -*-

;;; requires

(require 'let-alist)

;;; constants

(defconst reorg--data-property-name 'reorg-data)
(defconst reorg--id-property-name 'reorg-id)
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
(defcustom reorg-headline-format '(:stars :headline)
  "Headline format.")

;;; variables 

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

;;; macros

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

;;;; body 

(defun reorg-parser--get-body-string ()
  "Parse all elements from the start of the body to the next node.
and return the tree beginning with the section element.
If NO-PROPS is non-nil, remove text properties."
  (org-no-properties 
   (org-element-interpret-data
    (org-element--parse-elements (save-excursion (org-back-to-heading)
						 (org-end-of-meta-data t)
						 (point))
				 (or (save-excursion (outline-next-heading))
				     (point-max))
				 'section nil nil nil nil))))

(defun reorg-parser--set-body-string (string)
  "Parse all elements from the start of the body to the next node.
and return the tree beginning with the section element.
If NO-PROPS is non-nil, remove text properties."
  (save-excursion 
    (if-let* ((props (cadar 
		      (org-element--parse-elements (save-excursion (org-back-to-heading)
								   (org-end-of-meta-data t)
								   (point))
						   (or (save-excursion (outline-next-heading))
						       (point-max))
						   'section nil nil nil nil)))
	      (begin (plist-get props :begin))
	      (end (plist-get props :end)))
	(progn 
	  (delete-region begin end)
	  (goto-char begin)
	  (insert string))
      (org-end-of-meta-data)
      (insert string))
    (unless (string= "\n" (substring string (1- (length string))))
      (insert "\n"))
    (delete-blank-lines)))

;;;; timestamps

(defun reorg--timestamp-parser (&optional inactive range)
  "Get the first timestamp at the current heading.  If INACTIVE
is non-nil, get the first inactive timestamp.  If RANGE is non-nil,
get the first active/inactive (depending on the value of INACTIVE) 
timestamp range.  Do not include scheduled or deadline timestamps. "
  (org-element-map
      (org-element--parse-elements
       (point)
       (org-entry-end-position)
       'section nil nil nil nil)
      'timestamp
    (lambda (timestamp)
      (when (eq (plist-get (cadr timestamp) :type)
		(pcase `(,inactive ,range)
		  (`(t t) 'inactive-range)
		  (`(t nil) 'inactive)
		  (`(nil t) 'active-range)
		  (`(nil nil) 'active)))
	(plist-get (cadr timestamp) :raw-value)))
    nil t))

;;;; relations

(defun reorg-parser--get-children-ids ()
  "Get the list of IDs of all children."
  (org-with-wide-buffer
   (let (ids)
     (when (org-goto-first-child)
       (push (org-id-get (point) t) ids)
       (while (outline-get-next-sibling)
	 (push (org-id-get (point) t) ids)))
     ids)))

;;;; tags

(defun reorg-parser--get-tags (which)
  "Get tags for heading at point.  WHICH can be:
'local      only get local tags
'all        get both local and inherited tags
'inherited  get only inherited tags.
'string     get the tag string for the heading"
  (pcase which
    (`string (org-get-tags-string))
    (`local (mapcar #'org-no-properties (org-get-local-tags)))
    (`all (mapcar #'org-no-properties (org-get-tags (point))))
    (`inherited
     (seq-difference
      (mapcar #'org-no-properties (org-get-tags (point)))
      (mapcar #'org-no-properties (org-get-local-tags))))
    (_ (error "%s is not a valid tag option" which))))

;;;; properties

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

;; (defun reorg-parser--get-property-drawer ()
;;   "adsf"
;;   ;; (seq-remove (lambda (prop)
;;   ;; 		(let ((key (car prop)))
;;   ;; 		  (or (null key)
;;   ;; 		      (equal "PROPERTIES" key)
;;   ;; 		      (equal "END" key))))
;;   (org-element-map
;; 	  (org-element--parse-elements
;; 	   (point)
;; 	   (org-entry-end-position)
;; 	   'node-property 'greater-element nil nil nil)
;; 	  'node-property
;; 	(lambda (prop)
;; 	  (when-let ((key (plist-get (cadr prop) :key))
;; 		     (val (plist-get (cadr prop) :value)))
;; 	    (unless (or (null key)
;; 			(equal "" key)
;; 			(equal "PROPERTIES" key)
;; 			(equal "END" key))
;; 	      (list key val))))))

;;; data macro

(defun reorg--refresh-advice (old-fun &rest args)
  (reorg--with-point-at-orig-entry
   (apply old-fun args)  
   t))

(defun reorg--modification-hook ()
  (pcase-let* ((`(,start . ,end) (reorg--get-field-bounds))
	       (new-val (buffer-substring-no-properties start end))
	       (field (reorg--get-field-at-point)))
    (delete-region start end)
    (goto-char start)
    (insert (reorg-action field 'display new-val))))

(defun reorg-action (type action &rest args)
  "Call the ACTION associated with TYPE.  ACTION
can be get, set, display, or validate.  TYPE is any
data type defined by the `reorg-create-data-type' macro.
ARGS are supplied to the function defined by ACTION in each 
`reorg-create-data-type' declaration."
  (apply
   (plist-get 
    (alist-get type reorg-parser-list)
    (reorg--add-remove-colon action))
   args))

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
		  :set  (lambda (id val &rest args)
			  (reorg--with-point-at-orig-entry id buffer
							   ,set))
		  :get-view-string (lambda ()
				     (pcase-let ((`(,start . ,end)
						  (reorg--get-field-bounds)))
				       (buffer-substring start end)))
		  :validate (lambda (val &rest args)
			      ,validate)
		  :display (cl-defun ,(intern (concat "reorg-display--"  (symbol-name name))) (plist &rest args)
			     (let ((val (plist-get plist (or
							  (when ',getter
							    ,getter)
							  ,(reorg--add-remove-colon name)))))
			       (when ',display (setq val (apply (lambda (&rest args) ,display) args)))
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
			       (when ',face (setq val (propertize
						       val
						       'font-lock-face
						       (cond ((internal-lisp-face-p ',face)
							      ',face)
							     ((functionp ',face)
							      (funcall ',face val))
							     (t (error "invalid face specification"))))))
			       (setq val (concat ,display-prefix val ,display-suffix))
			       (setq val (propertize val 'reorg-field-type ',name))))
		  ;; header keymaps come last


		  ))) ;; end let 
       (if ',disabled
	   (progn 
	     (cl-loop for (key . func) in ',field-keymap
		      do (advice-remove func #'reorg--refresh-advice))
	     (setq reorg-parser-list (remq ',name reorg-parser-list)))
	 (cl-loop for (key . func) in ',field-keymap
		  do (advice-add func :around #'reorg--refresh-advice))
	 (if (alist-get ',name reorg-parser-list)
	     (setf (alist-get ',name reorg-parser-list) (plist-get data :parse))
	   (push (cons ',name (plist-get data :parse)) reorg-parser-list))))))

;;; data macro application 

(reorg-create-data-type :name deadline
			:parse (org-entry-get (point) "DEADLINE")
			:set (if val (org-deadline nil val)
			       (org-deadline '(4)))
			:display-prefix (apply #'propertize "DEADLINE: "
					       '(font-lock-face org-special-keyword))
			:face org-date
			:field-keymap (("S-<up>" . org-timestamp-up)
				       ("S-<down>" . org-timestamp-down))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string))) 

(reorg-create-data-type :name property
			:parse (reorg-parser--get-property-drawer)
			:getter (plist-get (plist-get plist :property-drawer)
					   (reorg--add-remove-colon (car args)))
			:display (let* ((key  (reorg--add-remove-colon (car args) t))
					(val (plist-get (plist-get plist :property-drawer)
							(reorg--add-remove-colon key))))
				   (concat
				    (propertize (format ":%s:" key) 'font-lock-face 'org-special-keyword)
				    " "
				    (propertize (format "%s" val) 'font-lock-face 'org-property-value))
				   (format ":%s: %s" key val))
			:field-keymap (("C-c C-x p" . org-set-property)))

(reorg-create-data-type :name tags
			:parse (org-get-tags-string)
			:get (org-get-tags-string)
			:set (org-set-tags val)
			:face org-tag-group
			:heading-keymap (("C-c C-c" . org-set-tags-command)))

(reorg-create-data-type :name todo
			:parse (org-entry-get (point) "TODO")
			:get (org-entry-get (point) "TODO")			
			:set (org-todo)
			:display (when-let ((s (plist-get plist :todo)))
				   (propertize
				    s
				    'font-lock-face
				    (org-get-todo-face s)))
			:heading-keymap (("C-c C-t" . org-todo)
					 ("S-<right>" . org-shiftright)
					 ("S-<left>" . org-shiftleft)))

(reorg-create-data-type :name scheduled
			:parse (org-entry-get (point) "SCHEDULED")
			:get (org-entry-get (point) "SCHEDULED")
			:set (if val (org-deadline nil val)
			       (org-deadline '(4)))
			:display (concat (apply #'propertize "SCHEDULED: "
						'(font-lock-face org-special-keyword))
					 val)
			:face org-date
			:keymap (("S-<up>" . org-timestamp-up)
				 ("S-<down>" . org-timestamp-down)
				 ("C-c C-s" . org-schedule))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string)))

(reorg-create-data-type :name timestamp
			:parse (when (reorg--timestamp-parser)
				 (org-no-properties (reorg--timestamp-parser)))
			:get (reorg--timestamp-parser)
			:set (if-let* ((old-val (reorg--timestamp-parser)))
				 (when (search-forward old-val (org-entry-end-position) t)
				   (replace-match (concat val)))
			       (when val
				 (org-end-of-meta-data t)
				 (insert (concat val "\n"))
				 (delete-blank-lines)))
			:face org-date
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
			:set (if-let* ((old-val (reorg--timestamp-parser t)))
				 (when (search-forward old-val (org-entry-end-position) t)
				   (replace-match (concat val)))
			       (when val
				 (org-end-of-meta-data t)
				 (insert (concat val "\n"))
				 (delete-blank-lines)))
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
			:set (if-let* ((old-val (reorg--timestamp-parser t)))
				 (when (search-forward old-val (org-entry-end-position) t)
				   (replace-match (concat val)))
			       (when val
				 (org-end-of-meta-data t)
				 (insert (concat val "\n"))
				 (delete-blank-lines)))
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
			:set (if-let* ((old-val (reorg--timestamp-parser t)))
				 (when (search-forward old-val (org-entry-end-position) t)
				   (replace-match (concat val)))
			       (when val
				 (org-end-of-meta-data t)
				 (insert (concat val "\n"))
				 (delete-blank-lines)))
			:face org-date
			:keymap (("S-<up>" . org-timestamp-up)
				 ("S-<down>" . org-timestamp-down))
			:validate (with-temp-buffer
				    (insert val)
				    (beginning-of-buffer)
				    (org-timestamp-change 0 'day)
				    (buffer-string)))

(reorg-create-data-type :name id
			:parse (org-id-get-create))

(reorg-create-data-type :name headline
			:parse (org-no-properties (org-get-heading t t t t)))

(reorg-create-data-type :name category-inherited
			:parse (org-entry-get-with-inheritance "CATEGORY"))

(reorg-create-data-type :name category
			:parse (org-get-category)
			:set (org-set-property "CATEGORY" val))



(reorg-create-data-type :name file
			:parse (buffer-file-name))

(reorg-create-data-type :name buffer-name
			:parse (buffer-name))

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
  (org-with-wide-buffer 
   (org-map-entries
    #'reorg--parser match scope skip)))

;;; grouping and sorting parsed results
;;;; let-plist 

(cl-defmacro reorg--let-plist (plist &rest body)
  (cl-assert (fboundp 'let-alist))
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

;;;; multi-sort

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

;;;; group and sort 

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
				     (if pre-transformer
					 (seq-map pre-transformer it)
				       it)
				     (if pre-filter (seq-remove pre-filter it) it)
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
				     (if sorter
					 (progn 
					   (setq xxx it)
					   (seq-sort-by (or sort-getter #'car) sorter it))
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
      (doloop copy template)
      (cadr copy))))


;;; Generating the outline
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
    (if (stringp data)
	(concat (create-stars level) " " data)
      (cl-loop for each in format-string
	       if (stringp (car each))
	       concat (car each)
	       else if (eq 'stars (car each))
	       concat (create-stars level)
	       else if (eq 'align-to (car each))
	       concat (propertize " " 'display `(space . (:align-to ,(cadr each))))
	       else if (eq 'pad (car each))
	       concat (make-string (cadr each) ? )
	       else
	       concat (apply (intern (concat "reorg-display--" (symbol-name (car each))))
			     data
			     (cdr each))))))
;; (defun reorg--generate-org-headings (data format-string &optional num)
;;   "Turn the output of `reorg--group-and-sort' into 
;;   orgmode headings."
;;   (cl-flet* ((get-field-name (keyword) (intern (substring (symbol-name keyword) 1)))
;; 	     (make-field-string (string field)
;; 				(concat 
;; 				 (propertize string
;; 					     reorg--field-property-name
;; 					     (get-field-name field))
;; 				 " "))
;; 	     (create-stars (num &optional data)
;; 			   (make-string (if (functionp num)
;; 					    (funcall num data)
;; 					  num)
;; 					?*)))
;;     (when (reorg--id-p data)
;;       (setq data (gethash data reorg-hash-table)))
;;     (if (stringp data)
;; 	(concat 
;; 	 (propertize 
;; 	  (concat (create-stars num nil) " " data)
;; 	  reorg--field-property-name :no-data)
;; 	 "\n")
;;       (cl-loop for field in format-string  
;; 	       concat (cond ((symbolp field)
;; 			     (if (eq field :stars)
;; 				 (make-field-string (create-stars num data) field)
;; 			       (when-let ((field-val (reorg--to-string (plist-get data field))))
;; 				 (make-field-string field-val field))))
;; 			    ((stringp field) field)
;; 			    ((listp field)
;; 			     (concat
;; 			      (when-let ((field-val (reorg--to-string (plist-get data (car field)))))
;; 				(make-field-string  (car field)
;; 						    (get-field-name (car field))))
;; 			      (propertize " "
;; 					  'display
;; 					  `(space . (:align-to ,(cadr field)))))))
;; 	       into results
;; 	       finally return
;; 	       (propertize
;; 		(concat
;; 		 results
;; 		 "\n")
;; 		reorg--data-property-name data
;; 		reorg--id-property-name (plist-get data :id))))))

;;; Insert headlines into buffer

(defun reorg--insert-org-headlines (data)
  "it's just a loop"
  (cl-loop for x in data do (insert x)))
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
  (outline-back-to-heading)
  (re-search-forward "^*+ " (point-at-eol))
  (1- (length (match-string 0))))

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
  (let ((results (reorg--map-entries file)))
    (setq results
	  (cl-loop
	   for each in results
	   collect
	   (reorg--create-headline-string
	    each
	    reorg-headline-format
	    (string-to-number
	     (plist-get (gethash each reorg-hash-table)
			:level)))))
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

(defun reorg-open-sidebar (template &optional file)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (--> (reorg--map-entries file)
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

;;; reorg-views

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
  (let ((next-heading (org-with-wide-buffer
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
  (widen)
  (org-show-all)
  (reorg-view--source--goto-end-of-meta-data)
  (narrow-to-region (save-excursion
		      (org-back-to-heading))
		    (save-excursion 
		      (outline-next-heading)
		      (point))))

(defun reorg-view--goto-source-id (buffer id &optional narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (reorg--select-main-window)
  (set-window-buffer (selected-window) buffer)
  (widen)
  (let ((old-point (point)))
    (goto-char (point-min))
    (if (re-search-forward id nil t)
	(progn (org-back-to-heading)
	       (when narrow
		 (reorg-view--source--narrow-to-heading))
	       t)
      (goto-char old-point)
      nil)))

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
  (org-next-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-previous-entry-follow ()
  (interactive)
  (org-previous-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-next-entry-no-follow ()
  (interactive)
  (org-next-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window))

(defun reorg--move-to-previous-entry-no-follow ()
  (interactive)
  (org-previous-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--goto-next-parent ()
  "Goto the next parent."
  (interactive)
  (when (re-search-forward (concat "^*\\{" (number-to-string (1- (org-current-level))) "\\} ") nil t)
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

(defmacro reorg--map-id (id &rest body)
  "Execute BODY at each entry that matches
ID."
  `(save-excursion
     (goto-char (point-min))
     (while (text-property-search-forward
	     reorg--id-property-name
	     ,id
	     nil)
       ,@body)))

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
     (reorg-outline-level))))

(defun reorg--shift-up (arg)
  "Shift priority or timestamp."
  (interactive "P")
  (pcase (reorg-edits--get-field-type)
    ((or 'deadline
	 'scheduled
	 'timestamp
	 'timestamp-ia)
     (org-timestamp-up arg))
    (`priority
     (org-priority-up))))

(defun reorg--shift-down (arg)
  "Shift priority or timestamp."
  (interactive "P")
  (pcase (reorg-edits--get-field-type)
    ((or 'deadline
	 'scheduled
	 'timestamp
	 'timestamp-ia)
     (org-timestamp-down arg))
    (`priority
     (org-priority-down))))

;;;; view mode

(defvar reorg-view-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'reorg-view--tree-to-source--goto-heading)
    (define-key map (kbd "e") #'reorg-edits--start-edit)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "f") #'reorg-edits-move-to-next-field)
    (define-key map (kbd "S-<up>") #'reorg--shift-up)
    (define-key map (kbd "S-<down>") #'reorg--shift-down)
    (define-key map (kbd "b") #'reorg-edits-move-to-previous-field)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map (kbd "n") #'reorg--move-to-next-entry-no-follow)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry-no-follow)
    (define-key map (kbd "TAB") #'outline-cycle)
    map)
  "keymap")

(define-derived-mode reorg-view-mode outline-mode
  "Org tree view"
  "Tree view of an Orgmode file. \{keymap}"
  (reorg--initialize-overlay)
  (setq cursor-type nil)
  (reorg-dynamic-bullets-mode)
  (org-visual-indent-mode)
  (use-local-map reorg-view-mode-map))

(defmacro reorg-view--make-change-in-org-buffer-and-sync-clones (&rest body)
  "asdf"
  `(let (data)
     (reorg--with-point-at-orig-entry nil nil
				      ,@body
				      (setq data (reorg-parser--headline-parser)))
     (reorg--map-id (plist-get data :id)
		    (reorg-views--replace-heading data))))

(defun reorg-views--replace-heading (data)
  "Replace the heading at point with NEW."
  (let ((level (reorg-outline-level)))
    (outline-back-to-heading)
    (delete-region (point)
		   (save-excursion (outline-next-heading) (point)))
    (insert (reorg--create-headline-string data
					   reorg-headline-format
					   level))))

;;; reorg-edit-mode
;;;; keep point in field

(defun reorg--kill-field ()
  "asdf"
  (interactive)
  (save-excursion
    (let* ((end (field-end))
           (beg (field-beginning)))
      (delete-region (point) end))))

(defun reorg--pre-command-hook ()
  "asdf"
  (interactive)
  (if (eq 'reorg (field-at-pos (point)))
      (setq reorg--current-location
	    (list (point)
		  (field-beginning)
		  (field-end)))
    (setq reorg--current-location nil)))

(defun reorg--post-command-hook ()
  "zxcv"
  (interactive)
  (unless (eq 'reorg (field-at-pos (point)))
    (when reorg--current-location
      (cond ((< (point)
		(cadr reorg--current-location))
	     (goto-char (cadr reorg--current-location)))
	    ((> (point)
		(caddr reorg--current-location))
	     (goto-char (caddr reorg--current-location)))))))

(defun reorg--add-command-hooks (&optional remove)
  "asdf"
  (setq reorg--current-location nil)
  (if remove
      (progn 
	(remove-hook 'pre-command-hook #'reorg--pre-command-hook t)
	(remove-hook 'post-command-hook #'reorg--post-command-hook t))
    (add-hook 'pre-command-hook #'reorg--pre-command-hook nil t)
    (add-hook 'post-command-hook #'reorg--post-command-hook nil t)))

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
    map)
  "keymap.")

;;;; macros

(defmacro reorg--with-point-at-orig-entry (id buffer &rest body)
  "Execute BODY with point at the heading with ID at point."
  `(when-let ((id ,(or id (reorg--get-view-prop :id))))
     (with-current-buffer (or ,buffer (reorg--get-view-prop :buffer))
       (org-with-wide-buffer 
	(goto-char (point-min))
	;; NOTE: Can't use `org-id-goto' here or it will keep the
	;;       buffer open after the edit.  Getting the buffer
	;;       and searching for the ID should ensure the buffer
	;;       stays hidden.
	(save-match-data
	  (if (re-search-forward id)
	      (progn 
		,@body)
	    (error "Heading with ID %s not found." id)))))))

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

;;;; functions

(defun reorg-edits--post-field-navigation-hook ()
  "Tell the user what field they are on."
  (reorg-edits--update-box-overlay)
  (setf (point) (car 
		 (reorg-edits--get-field-bounds))))

(defun reorg-edits--update-box-overlay ()
  "Tell the user what field they are on."
  (when-let ((field (get-text-property (point) reorg--field-property-name)))
    (delete-overlay reorg-edits--current-field-overlay)
    (move-overlay reorg-edits--current-field-overlay
		  (car (reorg-edits--get-field-bounds))
		  (if (eq field 'stars)
		      (point-at-eol)
		    (cdr (reorg-edits--get-field-bounds))))
    (message "You are on the field for the heading's %s"
	     (reorg-edits--get-field-type))))

(defun reorg-edits--get-field-type ()
  "Get the field type at point, if any."
  (get-text-property (point) reorg--field-property-name))

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
    (reorg-view--make-change-in-org-buffer-and-sync-clones
     (reorg-get-set-props type :val val))
    (reorg-edits-mode -1)))

(defun reorg-edits--sync-field-with-source ()
  "Set SOURCE to the value of the current field."
  (let ((field (reorg-edits--get-field-at-point))
	(val (reorg-edits--get-field-value)))
    (reorg--with-point-at-orig-entry nil
				     (reorg-get-set-props field
							  :val val ))))

(defun reorg-edits--discard-edit ()
  "Discard the current edit and restore the node
to its previous state, and turn off the minor mode."
  (interactive)
  (reorg-edit-field--replace-field
   reorg-edits--restore-state)
  (setq reorg-edits--restore-state nil)
  (reorg-edits-mode -1)
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

(defun reorg-edit-heading (id prop val)
  "Update the hashtable at ID by setting PROP to VAL.
Update the applicable heading in the orgmode document. 
Update the headings in the view buffer."
  (plist-put (gethash id reorg-hash-table) prop
	     (reorg--with-point-at-orig-entry id
					      (plist-get (gethash id reorg-hash-table) :buffer)
					      (reorg-get-set-props prop :val val)
					      (reorg-parser--headline-parser)))
  (reorg--select-tree-window)
  (reorg--map-id id
		 (reorg-views--replace-heading
		  (gethash id reorg-hash-table))))

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

(defun reorg-edits--move-selection-overlay ()
  (if-let ((bounds (reorg-edits--get-field-bounds)))
      (move-overlay reorg-edits--current-field-overlay
		    (car bounds)
		    (cdr bounds))
    (delete-overlay reorg-edits--current-field-overlay)))

;;;; minor mode

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
	  ;;(add-hook 'post-command-hook #'reorg-edits--keep-point-in-range nil t)
	  ;;(reorg--lock-buffer)
	  (setq cursor-type 'bar
		reorg-edits--previous-header-line header-line-format
		header-line-format reorg-edits--header-line
		reorg-edits--restore-state (reorg-edits--get-field-value)
		reorg-edit-field--start-marker (set-marker (make-marker) start)
		reorg-edit-field--end-marker (set-marker (make-marker) end)
		overriding-local-map reorg-edits-field-mode-map))
      ;; off
      ;;(reorg--unlock-buffer)
      ;;(remove-hook 'post-command-hook #'reorg-edits--keep-point-in-range t)
      (delete-overlay reorg-edits--current-field-overlay)
      (setq header-line-format reorg-edits--previous-header-line
	    reorg-edits--previous-header-line nil
	    reorg-edit-field--start-marker nil
	    reorg-edit-field--end-marker nil
	    reorg-edits--restore-state nil
	    overriding-local-map nil
	    cursor-type nil))))

(provide 'reorg)


;;;; testing 

(cl-loop for x in (reorg--process-results (reorg--group-and-sort xxx '( :group (concat "Legs: " .property.legs)
									:children (( :group (concat "Aquatic: "
												    (if (string= "1" .property.aquatic)
													"yes"
												      "no"))))))

					  '((stars) (pad 10) (headline)))
	 do (insert x "\n"))

