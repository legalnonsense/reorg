
;;; -*- lexical-binding: t; -*-

(defun reorg-tree--goto-next-property-field (prop val &optional backward pred transformer level-limit)
  "Move to the beginning of the next field of text property PROP that
matches VAL.

If PRED is specified, compare values using PRED instead of `eq'.

If BACKWARD is non-nil, move backward instead of forward. 

TRANSFORMER is an optional function that accepts one argument
(the value of the text property at point) and transforms it before
comparing it to VAL.

For example, if (get-text-property (point) PROP) returns a plist, but you
only want to see if one value is present, the TRANSFORMER:

(lambda (plist) (plist-get plist :interesting-property))

will extract the single value prior to comparing to VAL.

LEVEL-LIMIT is a cons cell with a predicate and level number.  The search
will stop if it's not true."
  (let ((func (if backward
		  #'previous-single-property-change
		#'next-single-property-change))
	(limit (if backward
		   (point-min)
		 (point-max)))
	(pred (or pred #'eq))
	(search-invisible t)
	(inhibit-field-text-motion t)
	(disable-point-adjustment t))
    
    (cl-loop with point = (point)
	     with origin = (point)
	     with start-level = (or (cdr level-limit) (reorg-outline-level))
	     while point	     

	     do (setq point (funcall func point prop))

	     when (and level-limit
		       (not 
			(funcall (car level-limit)
				 (reorg-outline-level)
				 start-level)))
	     return (progn (goto-char origin) nil)
	     
	     if (and (null point)
		     (funcall pred
			      (funcall (or transformer #'identity)
				       (get-text-property limit prop))
			      val))
	     return (goto-char limit)

	     else if (null point)
	     return (progn (goto-char origin) nil)
	     
	     else if (funcall pred
			      val
			      (funcall (or transformer #'identity)
				       (get-text-property point prop)))
	     return (goto-char point)

	     else do (forward-char (- point (point))))))

(defun reorg-tree--get-by-branch-predicate (prop val func)
  "Execute FUNC at each branch that has PROP equal to VAL and
make a list of the results."
  (let (results)
    (save-excursion
      (cl-loop with level = (reorg-outline-level)
	       while (and (reorg-tree--goto-next-property-field
			   'reorg-data val nil #'equal
			   (lambda (x) (plist-get x prop)))
			  (> (reorg-outline-level) level))
	       do (cl-pushnew (funcall func) results :test #'equal)))
    (reverse results)))


(defun yyy ()
  (reorg-tree--get-by-branch-predicate :branch-predicate
				       xxx
				       (lambda ()
					 (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

