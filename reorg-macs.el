;; -*- lexical-binding: t; -*-

(cl-defmacro reorg-create-class-type (&key name
					   getter
					   follow
					   keymap
					   extra-props
					   render-func
					   display-buffer)
  "Create a new class type"
  (let ((func-name (reorg--create-symbol 'reorg--
					 name
					 '--get-from-source)))
    `(progn
       (defun ,func-name
	   (&rest sources)
	 (cl-flet ((PARSER (&optional d)
			   (reorg--parser d ',name					  
					  reorg--temp-parser-list)))
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
       (defun ,(reorg--get-parser-func-name name 'class) (&rest _)
	 "" (symbol-name ',name))
       (cl-pushnew (cons 'class (reorg--get-parser-func-name ',name 'class))
		   (alist-get ',name reorg--parser-list))
       (defun ,(reorg--get-parser-func-name name 'id) (&rest _)
	 "" (org-id-new))
       (cl-pushnew (cons 'id (reorg--get-parser-func-name ',name 'id))
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

(cl-defmacro reorg-create-data-type (&optional ;
				     &key
				     class
				     name
				     parse
				     disable
				     display
				     append)
  "Create a new type within a class"
  (let* ((parsing-func (reorg--create-symbol 'reorg--
					     class
					     '--parse-
					     name))
	 (display-func (reorg--create-symbol 'reorg--
					     class
					     '--display-
					     name)))
    `(progn
       (cond ((not ,disable)
	      (defun ,parsing-func (&optional data DATA)
		(let-alist DATA 
		  ,parse))
	      (setf (alist-get ',class reorg--parser-list)
		    (assoc-delete-all ',name
				      (alist-get
				       ',class
				       reorg--parser-list)))
	      (if ',append		     
		  (setf (alist-get ',class reorg--parser-list)
			(append (alist-get ',class reorg--parser-list)
				(list 
				 (cons ',name #',parsing-func))))
		(cl-pushnew (cons ',name #',parsing-func)
			    (alist-get ',class reorg--parser-list)))
	      (if ',display 
		  (defun ,display-func (data)
		    (let-alist data 
		      ,display))
		(fmakunbound ',display-func)))
	     (t ;;if disabled 
	      (setf (alist-get ',class reorg--parser-list)
		    (assoc-delete-all ',name
				      (alist-get ',class reorg--parser-list)))
	      (fmakunbound ',display-func)
	      (fmakunbound ',parsing-func))))))

(provide 'reorg-macs)
