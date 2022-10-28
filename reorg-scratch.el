;; -*- lexical-binding: t; -*-

;; 1.


(let* ((template '( :group .@tag-list
		    :format-string (concat " " .headline)
		    :children (( :group .@at-name
				 :format-string (concat " " .headline)))))
       (results (--> (reorg--getter '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org")))
		     (reorg--group-and-sort it template))))
  results)

(defun reorg-insert--find-by-prop (prop val &optional test backward)
  "find the first branch that matches LEVEL."
  (when-let* ((func (if backward
			#'text-property-search-backward
		      #'text-property-search-forward))
	      (match (funcall func 
			      'reorg-data
			      val
			      (lambda (a b) (funcall
					     (or test #'string=)
					     (alist-get prop b)
					     a))
			      t)))
    (goto-char (prop-match-beginning match))))

(defun reorg--find-prop (prop &optional val from to test)
  (cl-flet ((truth (&rest args) t))
    (cl-loop with test = (if val
			     (or test #'equal )
			   #'truth)
	     for (beg . end) being the intervals
	     property 'reorg-data
	     from (or from (point-min))
	     to (or to (point-max))
	     when (funcall test
			   (alist-get prop
				      (get-text-property beg 'reorg-data))
			   val)
	     collect (cons beg end))))

(defun reorg--truth (&rest _args)
  "Ignore everything and return t."
  t)

(defun reorg--and (a b)
  "Ignore everything and return t."
  (and a b))

(reorg--find-prop 'reorg-data t
		  'reorg--and
		  nil nil
		  (lambda (x)
		    (get-text-property x 'reorg-data)
		    ))








(insert xxx)
* ccc

;; is the current data a branch or leaf?
;; if it is a heading, is the heading present?
;; Go to it. loop to next data
;; if it is a leaf, insert it at the appropriate point


;; heading present?
;; subheading present?
;; find header location 

(md5 (with-temp-buffer
       (insert
	(pp '(a b)))
       (buffer-string)))




(cl-loop for (a . b) being the intervals
	 collect a)
