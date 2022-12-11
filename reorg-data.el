
;; -*- lexical-binding: t; -*-

;;; variables

(defvar reorg--extra-prop-list nil "")
;;(defvar reorg--parser-func-list nil "")
(defvar reorg--getter-list nil "")
(defvar reorg--parser-list nil "")
(defvar reorg--render-func-list nil "")


;;; name constructors

(defun reorg--get-display-buffer-func-name (class)
  "Create `reorg--CLASS--display-buffer' symbol"
  (reorg--create-symbol 'reorg--
			class
			'--display-buffer))

(defun reorg--getter-func-name (class)
  "Create `reorg--CLASS--get-from-source' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--get-from-source))

(defun reorg--get-parser-func-name (class name)
  "Create `reorg--CLASS--parse-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--parse-
			name))

(defun reorg--get-global-parser-func-name (class)
  "Create `reorg--CLASS--parse-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--parser))

(defun reorg--get-display-func-name (class name)
  "Create `reorg--CLASS--display-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--display-
			name))

(defun reorg--get-parser-list-name (class)
  "Create `reorg--CLASS--parser-list' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--parser-list))

;;; class macro

(cl-defmacro reorg-create-class-type (&key name
					   getter
					   follow
					   keymap
					   extra-props
					   render-func
					   display-buffer)
  "Create a new class type. NAME is the name of the class.
GETTER is a form that does two things:
1. Retrieves data;
2. Calls each function in the class's parser-list (created by `reorg-create-data-type')
3. Returns the parsed data as an alist.

To retrieve data and call the parser, GETTER is a function
that is called with one argument: a list of sources.
A source can be an orgmode file, output from the command line,
a database, or anything.

Because data can take any form, GETTER is responsible for
gathering the data, and deciding how and when to run the parser
functions on that data.

It is easy to write a proper getter: simply accept a list of SOURCES
and then return a list of plists which represents your complete data set.

For example, if you are parsing an org-mode file, you will
likely want to run the parser functions at each heading
as you navigate the document.  So you will want to call `org-map-entries'
(or use `org-ql', or, if you are a crazy person, `org-element').
and, at each heading, call THE_PARSER. THE_PARSER is a let-bound
function that is available inside the GETTER function.  It will
automatically call each of the parsers in the class's parser list.

But if you want to parse a list of files, you'll likely want to
grab input from the command line (e.g., ls or find) and /then/
process the data.  This means you get the data first, turn it
into a list, and then loop over the list with `cl-loop' or `mapcar'
and return the results.

Note that the types of data that will be available are defined by the
`reorg-create-data-type' macro.  This macro requires you to define a name,
a parser, a display function, and more.  The macro will automatically add
text properties that are necessary to display the data in the display tree.

A source should be defined as whatever you want the user to be allowed to
call from the template macro.
"  
  `(progn
     (defun ,(reorg--create-symbol 'reorg--
				   name
				   '--get-from-source)
	 (&rest sources)
       (cl-flet ((PARSER (&optional d)
			 (reorg--parser d ',name)))
	 (cl-loop
	  for SOURCE in sources
	  append ,getter)))
     (if (boundp 'reorg--getter-list)
	 (setf (alist-get ',name reorg--getter-list) nil)
       (defvar reorg--getter-list nil "Getter list for all classes"))
     (cl-pushnew  #',(reorg--create-symbol 'reorg--
					   name
					   '--get-from-source)
		  (alist-get ',name reorg--getter-list))
     (if (boundp 'reorg--parser-list)
	 (setf (alist-get ',name reorg--parser-list) nil)
       (defvar reorg--parser-list nil "Parser list for all classes."))     
     (cl-pushnew (cons 'class (lambda (&optional _) ',name))
		 (alist-get ',name reorg--parser-list))
     ;; (setf (alist-get ',name reorg--parser-list)
     ;; 	   (cons 'class (lambda () ',(name)))
     (setf (alist-get ',name reorg--extra-prop-list)
	   ',extra-props)
     (when ',keymap
       (setf (alist-get ',name reorg--extra-prop-list)
	     (append (alist-get ',name reorg--extra-prop-list)
		     (list 
	     	      'keymap
		      ',(let ((map (make-sparse-keymap)))
			  (cl-loop for (key . func) in keymap
				   collect (define-key map (kbd key) func))
			  map)))))
     (when ',render-func
       (setf (alist-get ',name reorg--render-func-list)
	     ',render-func))))
;; (when ',render-func
;;   (setf (alist-get ',name reorg--extra-prop-list)
;; 	     (append (alist-get ',name reorg--extra-prop-list)
;; 		     (list 
;; 	     	      'render-func
;; 		      ',render-func))))))

;; all that is needed for a type is:
;; class
;; name
;; parse
;; set
;; &rest extra-props
;; and only do: 1. create a named function for the parser-list
;;
;; the getter is the one that tags everything with its class, so that
;; an object's class is part of the original data
;;
;; after that, the parser functions only run if they are of the same class
;; the the parser list is a list of parser lists 
;; instead of separate lists
;;




;;; data macro

(cl-defmacro reorg-create-data-type (&optional
				     &key
				     class
				     name
				     parse
				     set
				     disable
				     display
				     append)
  ;; TODO add disabled key to remove data type
  ;; from the parser list 

  "Create the data types that will be used to represent and
interact with the data as key-value pairs.

NAME is the name of the thing.  It can be string or a symbol.

PARSE is a form that is called for each piece of data gathered by
GETTER.  It must extract the relevant information from whatever data GETTER
returns.

DISPLAY is a function that accepts a plist and returns a string for display in the
display tree.

EXTRA-PROPS is a plist of text properties that are added to the
text properties of any field displaying the data type.
"
  (let* ((parsing-func (reorg--get-parser-func-name class name))
	 (display-func (reorg--get-display-func-name class name)))
    `(progn
       (if (not ,disable)
	   (progn 
	     (defun ,parsing-func (&optional data)
	       ,parse)
	     (if ',append
		 (progn 
		   (setf (alist-get ',class reorg--parser-list)
			 (remove (cons ',name #',parsing-func)
				 (alist-get ',class reorg--parser-list)))
		   (setf (alist-get ',class reorg--parser-list)
			 (append (alist-get ',class reorg--parser-list)
				 (list 
				  (cons ',name #',parsing-func)))))
	       (cl-pushnew (cons ',name #',parsing-func)
			   (alist-get ',class reorg--parser-list)))
	     (if ',display 
		 (defun ,display-func (alist)
		   ,display)
	       (fmakunbound ',display-func)))
	 (progn
	   (fmakunbound ',display-func)
	   (fmakunbound ',parsing-func)
	   (setf (alist-get ',name (alist-get ',class reorg--parser-list)) nil))))))

(defun reorg--render-source ()
  "Render the heading at point."
  (when-let ((func (alist-get (reorg--get-view-prop 'class) reorg--render-func-list)))
    (funcall func))
  (reorg--select-tree-window))

(defun reorg--goto-source ()
  "Goto rendered source buffer."
  (interactive)
  (reorg--render-source)
  (reorg--select-main-window))

(defun reorg--parser (data class &optional type)
  "Call each parser in CLASS on DATA and return
the result.  If TYPE is provided, only run the
parser for that type."
  (if type
      (cons type 
	    (funcall (alist-get
		      type
		      (alist-get class
				 reorg--parser-list))
		     data))
    (cl-loop for (type . func) in (alist-get class reorg--parser-list)
	     collect (cons type (funcall func data)))))

(defun reorg--parser (data class &optional type)
  "Call each parser in CLASS on DATA and return
the result.  If TYPE is provided, only run the
parser for that type."
  (if type
      (cons type 
	    (funcall (alist-get
		      type
		      (alist-get class
				 reorg--parser-list))
		     data))
    (cl-loop for (type . func) in (alist-get class reorg--parser-list)
	     collect (cons type (funcall func (append data results)))
	     into results
	     finally return results)))

(cl-loop for (a . b) in '((a . 2) ((lambda (data)
				     (alist-get 'a data)) . 3) (c . 5))
	 if (symbolp a)
	 collect (cons a b) into xxx
	 else if (functionp a)
	 collect (funcall a xxx) into xxx
	 finally return xxx)

;;; creating headline strings from parsed data 

(defun reorg--walk-tree (form func &optional data)
  (cl-labels ((walk (form)
		    (cl-loop for each in form
			     if (listp each)
			     collect (walk each)
			     else
			     collect (if data
					 (funcall func each data)
				       (funcall func each)))))
    (if (listp form)
	(walk form)
      (if data 
	  (funcall func form data)
	(funcall func form)))))

;; (defun reorg--walk-tree (form func &optional data)
;;   "Run FUNC at each node of TREE using a depth-first traversal
;; and destructively modify TREE. 
;; FUNC is a function that accepts one argument, which is the
;; current element of TREE."
;;   (let ((tree (copy-tree form)))
;;     (cl-labels ((doloop (tree func)
;; 			(setf (car tree) (funcall func (car tree) data))
;; 			(cl-loop for n below (length (cdr tree))
;; 				 if (listp (nth n (cdr tree))) do
;; 				 (doloop (nth n (cdr tree)) func)
;; 				 else do
;; 				 (setf (nth n (cdr tree))
;; 				       (funcall func (nth n (cdr tree)) data)))))
;;       (if (listp tree)
;; 	  (progn 
;; 	    (doloop tree func)
;; 	    tree)
;; 	(funcall func tree)))))

;; (reorg--walk-tree
;;  '(.todo " " .headline)
;;  #'reorg--turn-dot-to-display-string
;;  '((todo . "xxx")
;;    (headline . "yyy")))

;; (reorg--walk-tree
;;  '((s-pad-right 10 " " .todo) " " .headline)
;;  #'reorg--turn-dot-to-display-string
;;  '((todo . "xxx")
;;    (headline . "yyy")))

;; TODO ensure a call to display function instead of relying on the data alist 
(defun reorg--create-headline-string (data format-string &optional level overrides)
  "Create a headline string from DATA using FORMAT-STRING as the
template.  Use LEVEL number of leading stars.  Add text properties
`reorg--field-property-name' and  `reorg--data-property-name'."
  (cl-flet ((create-stars (num &optional data)
			  (make-string (if (functionp num)
					   (funcall num data)
					 num)
				       ?*)))
    (push (cons 'reorg-level level) data)
    (apply
     #'propertize 
     (concat
      (if (alist-get 'reorg-branch data)
	  (propertize 
	   (concat (create-stars level) " " (alist-get 'branch-name data))
	   reorg--field-property-name
	   'branch)
	;; TODO:get rid of this copy-tree
	(let ((format-copy (copy-tree format-string)))
	  (cl-loop for (prop . val) in overrides
		   do (setf (alist-get prop data)
			    (funcall `(lambda ()
					(let-alist data 
					  ,val)))))
	  (concat
	   ;;	   (when level (propertize (create-stars level) reorg--field-property-name 'stars))
	   (let ((xxx (reorg--walk-tree format-string
					#'reorg--turn-dot-to-display-string
					data)))
	     (funcall `(lambda (data)
			 (concat ,@xxx))
		      data)))))
      ;; (apply (car xxx)
      ;; 	 (cdr xxx)))
      
      ;; `(lambda (data)
      ;;    (let-alist data 
      ;;      ,format-string))
      ;; data))))
      "\n")
     'reorg-data     
     (append data
	     (list 			;
	      (cons 'reorg-class (alist-get 'class data))
	      (cons 'reorg-level level)))
     reorg--field-property-name
     'leaf
     (alist-get (alist-get 'class data)
		reorg--extra-prop-list)
     )))

(defun reorg--getter (sources)
  "Get entries from SOURCES, whih is an alist
in the form of (CLASS . SOURCE)."
  (cl-loop for (class . source) in sources
	   append (funcall (car (alist-get class reorg--getter-list))
			   source)))


(provide 'reorg-data)
