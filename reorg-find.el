;; -*- lexical-binding: t; -*-

;; Functions to find by text properties in the buffer 

(defun reorg--find-prop (prop &optional val from to test)
  "TEST is a function that accepts two arguments: VAL and
the text property at the beginning of the region.  
PROP is a property that is contained within the reorg-data
text property data.  VAL is a target value."
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

(defun reorg--get-next-prop (prop &optional val test)
  "Find the next text prop PROP that matches VAL."
  (when-let ((next (reorg--find-prop prop val (point) nil test)))
    (cl-loop for (beg . end) in next
	     unless (and (>= (point) beg)
			 (<= (point) end))
	     return (cons beg end))))


(defun reorg--get-previous-prop (prop &optional val test)
  "Find the previous text prop PROP that matches VAL."
  (when-let ((previous (reorg--find-prop prop val nil (point) test)))
    (cl-loop for (beg . end) in (reverse previous)
	     unless (and (>= (point) beg)
			 (<= (point) end))
	     return (cons beg end))))

(defun reorg--goto-next-prop (prop &optional val test)
  "Go to next PROP that matches VAL."
  (when-let ((target (reorg--get-next-prop prop val test)))
    (reorg--goto-char (car target))))

(defun reorg--goto-previous-prop (prop &optional val test)
  "Go to next PROP that matches VAL."
  (when-let ((target (reorg--get-previous-prop prop val test)))
    (reorg--goto-char (car target))))

(defun reorg--goto-char (point)
  "Goto POINT and run hook funcs."
  (goto-char (point))
  (run-hooks 'reorg--navigation-hook))

