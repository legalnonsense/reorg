;; -*- lexical-binding: t; -*-

(require 's)

(defun reorg-company--get-candidates (arg)
  "Get company candidates."
  (let* ((all-dots (sort
		    (delete-dups 
		     (cl-loop for (class . rest) in reorg--parser-list
			      append 
			      (cl-loop for (type . func)
				       in rest
				       collect
				       (concat "." (symbol-name type)))))
		    #'string<)))
    (if (equal arg "..")
	all-dots
      (let ((target (and (string-match "\\(^\\.\\.?\\)\\(.+\\)" arg)
			 (match-string 2 arg))))
	(if-let ((class (alist-get (intern target) reorg--parser-list)))
	    (sort (cl-loop for (type . func) in class
			   collect (concat "." (symbol-name type))) #'string<) 
	  (let ((root (concat "." (match-string 2 arg))))
	    (cl-remove-if-not (lambda (x) (s-starts-with-p root x))
			      all-dots)))))))

(defun reorg-company--annotation (candidate)
  "Get annotation"
  (setq candidate (intern (substring candidate 1)))
  (when-let ((results (cl-loop for (key . rest) in reorg--parser-list
			       append (cl-loop for (k . v) in rest
					       when (equal k candidate)
					       collect key))))
    (concat "["
	    (substring 
	     (cl-loop for result in results
		      concat (concat (symbol-name result) " "))
	     0 -1)
	    "]")))

(defun reorg-company (command &optional arg &rest _)
  "company backend"
  (cl-case command
    (interactive (company-begin-backend 'reorg-company))
    (prefix (and (eq major-mode 'emacs-lisp-mode)
		 (when-let ((sym (thing-at-point 'symbol)))
		   (when (equal "."
				(substring sym 0 1))
		     sym))))
    (candidates (reorg-company--get-candidates arg))
    (annotation (reorg-company--annotation arg))
    (sorted t)
    (no-cache t)))

;;;###autoload 
(defun reorg-company-enable ()
  "Add reorg company backend to the current buffer"
  (interactive)
  (add-to-list 'company-backends #'reorg-company))

(defun reorg-company-disable ()
  "Remove reorg company backends from current buffer"
  (interactive)
  (setq company-backends 
	(remove #'reorg-company
		company-backends)))

(provide 'reorg-company)


(defun reorg-capf--annotation (candidate)
  "Get annotation"
  (setq candidate (intern (substring candidate 1)))
  (when-let ((results (cl-loop for (key . rest) in reorg--parser-list
			       append (cl-loop for (k . v) in rest
					       when (equal k candidate)
					       collect key))))
    (concat "["
	    (substring 
	     (cl-loop for result in results
		      concat (concat (symbol-name result) " "))
	     0 -1)
	    "]")))

(defun reorg-capf ()
  "capf for reorg template"
  (let (start end collection props)
    (when (looking-back "\\.[.|[:word:]]+")
      (let* ((two-dots-p (s-starts-with-p ".." (match-string 0)))
	     (start (if two-dots-p
			(1- (match-beginning 0))
		      (match-beginning 0)))
	     (end (match-end 0)))
	(list start
	      end 
	      (sort
	       (delete-dups
		(cl-loop for (class . rest) in reorg--parser-list
			 append (cl-loop for (type . func) in rest
					 collect
					 (concat "." (symbol-name type)))))

	       #'string<)
	      :annotation-function
	      #'reorg-capf--annotation)))))

(add-to-list 'completion-at-point-functions #'reorg-capf)

