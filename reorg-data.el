
;; -*- lexical-binding: t; -*-

;;; variables



(defun reorg--create-symbol (&rest args)
  "Create a symbol from ARGS which can be
numbers, strings, symbols."
  ;; (intern
  ;;  (apply #'concat 
  ;; 	  (mapcar (lambda (x)
  ;; 		    (pcase x
  ;; 		      ((pred stringp)
  ;; 		       x)
  ;; 		      ((pred numberp x)
  ;; 		       (number-to-string x))
  ;; 		      ((pred symbolp x)
  ;; 		       (symbol-name x))
  ;; 		      (_ (error "Unknown argument type: %s of type %s."
  ;; 				x 
  ;; 				(type-of x)))))
  ;; 		  args)))
  (cl-loop for arg in args
	   if (stringp arg)
	   concat arg into ret
	   else if (numberp arg)
	   concat (number-to-string arg) into ret
	   else if (symbolp arg)
	   concat (symbol-name arg) into ret
	   finally return (intern ret)))

;;; name constructors

;; (defun reorg--get-display-buffer-func-name (class)
;;   "Create `reorg--CLASS--display-buffer' symbol"
;;   (reorg--create-symbol 'reorg--
;; 			class
;; 			'--display-buffer))

;; (defun reorg--getter-func-name (class)
;;   "Create `reorg--CLASS--get-from-source' symbol."
;;   (reorg--create-symbol 'reorg--
;; 			class
;; 			'--get-from-source))

(defun reorg--get-parser-func-name (class name)
  "Create `reorg--CLASS--parse-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--parse-
			name))

;; (defun reorg--get-global-parser-func-name (class)
;;   "Create `reorg--CLASS--parse-NAME' symbol."
;;   (reorg--create-symbol 'reorg--
;; 			class
;; 			'--parser))

(defun reorg--get-display-func-name (class name)
  "Create `reorg--CLASS--display-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--display-
			name))

;; (defun reorg--get-parser-list-name (class)
;;   "Create `reorg--CLASS--parser-list' symbol."
;;   (reorg--create-symbol 'reorg--
;; 			class
;; 			'--parser-list))

;;; class macro

(cl-defmacro reorg-create-class-type (&key name
					   getter
					   follow
					   keymap
					   extra-props
					   render-func
					   display-buffer)
  ""
  (let ((func-name (reorg--create-symbol 'reorg--
					 name
					 '--get-from-source)))
    `(progn
       (defun ,func-name
	   (&rest sources)
	 (cl-flet ((PARSER (&optional d)
			   (reorg--parser d ',name)))
	   (cl-loop
	    for SOURCE in sources
	    append ,getter)))
       (if (boundp 'reorg--getter-list)
	   (setf (alist-get ',name reorg--getter-list) nil)
	 (defvar reorg--getter-list nil "Getter list for all classes"))
       (cl-pushnew  #',func-name
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
	       ',render-func)))))

;;; data macro

;; (cl-defmacro reorg-create-data-type (&optional
;; 				     &key
;; 				     class
;; 				     name
;; 				     parse
;; 				     disable
;; 				     display
;; 				     append)
;;   ;; TODO add disabled key to remove data type
;;   ;; from the parser list 

;;   "Create the data types that will be used to represent and
;; interact with the data as key-value pairs.

;; NAME is the name of the thing.  It can be string or a symbol.

;; PARSE is a form that is called for each piece of data gathered by
;; GETTER.  It must extract the relevant information from whatever data GETTER
;; returns.

;; DISPLAY is a function that accepts a plist and returns a string for display in the
;; display tree.

;; EXTRA-PROPS is a plist of text properties that are added to the
;; text properties of any field displaying the data type.
;; "
;;   (let* ((parsing-func (reorg--create-symbol 'reorg--
;; 					     class
;; 					     '--parse-
;; 					     name))
;; 	 (display-func (reorg--create-symbol 'reorg--
;; 					     class
;; 					     '--display-
;; 					     name)))
;;     `(progn
;;        (if (not ,disable)
;; 	   (progn 
;; 	     (defun ,parsing-func (&optional data)
;; 	       ,parse)
;; 	     (if ',append
;; 		 (progn 
;; 		   (setf (alist-get ',class reorg--parser-list)
;; 			 (remove (cons ',name #',parsing-func)
;; 				 (alist-get ',class reorg--parser-list)))
;; 		   (setf (alist-get ',class reorg--parser-list)
;; 			 (append (alist-get ',class reorg--parser-list)
;; 				 (list 
;; 				  (cons ',name #',parsing-func)))))
;; 	       (cl-pushnew (cons ',name #',parsing-func)
;; 			   (alist-get ',class reorg--parser-list)))
;; 	     (if ',display 
;; 		 (defun ,display-func (alist)
;; 		   ,display)
;; 	       (fmakunbound ',display-func)))
;; 	 (progn
;; 	   (fmakunbound ',display-func)
;; 	   (fmakunbound ',parsing-func)
;; 	   (setf (alist-get ',name (alist-get ',class reorg--parser-list)) nil))))))

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
    (if data
	(cl-loop for (type . func) in (alist-get class reorg--parser-list)
		 collect (cons type (funcall func data)))
      (cl-loop for (type . func) in (alist-get class reorg--parser-list)
	       collect (cons type (funcall func data)) into data
	       finally return data))))

(defun reorg--getter (sources)
  "Get entries from SOURCES, whih is an alist
in the form of (CLASS . SOURCE)."
  (cl-loop for (class . source) in sources
	   append (funcall (car (alist-get class reorg--getter-list))
			   source)))


(provide 'reorg-data)
