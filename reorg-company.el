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
(rassoc 5 '((a . 4) (b . 5)))
 (defun reorg-company--annotation (canditate)
   "Get annotation"
   "test")

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





