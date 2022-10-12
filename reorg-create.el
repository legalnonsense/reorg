;; -*- lexical-binding: t; -*-

;;; name constructors

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
					   getter)
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
     (defun ,(reorg--get-getter-func-name name)
	 (&rest sources)
       (cl-flet ((PARSER (&optional arg)
			 (funcall 
			  ',(reorg--get-global-parser-func-name name)
			  arg)))
	 (cl-loop for SOURCE in sources
		  append
		  ,getter)))
     (defun ,(reorg--get-global-parser-func-name name) (&optional arg)
       (cl-loop for (type . func) in ,(reorg--get-parser-list-name name)
		collect (cons type (funcall func arg))))
     (defvar ,(reorg--get-parser-list-name name)
       nil
       ,(concat "Parser list for class " (symbol-name name)))))


;;; data macro

(cl-defmacro reorg-create-data-type (&optional &key class 
					       name
					       parser
					       disabled
					       display)
  "Create the data types that will be used to represent and
interact with the data as key-value pairs.

NAME is the name of the thing.  It can be string or a symbol.

PARSER is a form that is called for each piece of data gathered by
GETTER.  It must extract the relevant information from whatever data GETTER
returns.

If DISABLE is non-nil, remove this type of data from the parser list.

DISPLAY is a function that accepts a plist and returns a string for display in the
display tree. 
"
  (let ((parsing-func (reorg--get-parser-func-name class name))
	(display-func (reorg--get-display-func-name class name))
	(parser-list (reorg--get-parser-list-name class))
	(global-parser (reorg--get-global-parser-func-name class)))
    `(progn 
       (defun ,parsing-func (&optional data)
	 ,parser)
       (cl-pushnew (cons ',name ',parsing-func) ,parser-list)      
       (defun ,display-func (alist)
	 (if ',display
	     ,display
	   (alist-get ',name alist))))))


;;; creating headline strings from parsed data 

(defun reorg--depth-first-apply (treee func &optional data)
  "Run FUNC at each node of TREE using a depth-first traversal
and destructively modify TREE. 
FUNC is a function that accepts one argument, which is the
current element of TREE."
  (let ((tree (copy-tree treee)))
    (cl-labels ((doloop (tree func)
			(setf (car tree) (funcall func (car tree) data))
			(cl-loop for n below (length (cdr tree))
				 if (listp (nth n (cdr tree))) do
				 (doloop (nth n (cdr tree)) func)
				 else do
				 (setf (nth n (cdr tree))
				       (funcall func (nth n (cdr tree)) data)))))
      (doloop tree func))
    tree))

(defun reorg--turn-dot-to-field (elem data)
  (if (and (symbolp elem)
	   (string-match "\\`\\." (symbol-name elem)))
      (pcase-let ((`(,class ,name) (mapcar
				    #'intern
				    (s-split "\\." (symbol-name elem) t))))
	(funcall (reorg--get-display-func-name class name) data))
    elem))

(defun reorg--create-headline-string (data format-string &optional level)
  "Create a headline string from DATA using FORMAT-STRING as the
template.  Use LEVEL number of leading stars.  Add text properties
`reorg--field-property-name' and  `reorg--data-property-name'."
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
       ;;TODO fix the text properties
       ;;TODO fix to use LEVEL (if provided) instead of .level

       ;; This copy is here for reasons.  So are a few others.
       ;; They should be tested and eliminated. 
       (let ((format-copy (copy-tree format-string)))
	 (concat
	  (when level (propertize (create-stars level) reorg--field-property-name 'stars))
	  (funcall `(lambda ()
		      ,(reorg--depth-first-apply format-copy
						 #'reorg--turn-dot-to-field
						 data))))))
     'reorg-class
     (plist-get data :class)
     reorg--data-property-name
     data)))
