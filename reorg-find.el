;; -*- lexical-binding: t; -*-

;; Functions to find by text properties in the buffer 

(defun reorg--get-view-prop (&optional property)
  "Get PROPERTY from the current heading.  If PROPERTY
is omitted or nil, get the 'reorg-data' prop.  If it is
supplied, get that property from 'reorg-data'."
  (save-excursion 
    (beginning-of-line)
    (let ((props (get-text-property (point-at-bol) reorg--data-property-name)))
      (if property
	  (alist-get property props)
	;;(plist-get props property)
	props))))

(defun reorg--get-view-props (&optional point &rest props)
  "Get text property PROPS at point. If there are multiple PROPS,
get nested properties."
  (cl-labels ((get-props (props &optional payload)
			 (if props 
			     (let ((props (if (listp props) props (list props))))
			       (if (not payload)
				   (->> (get-text-property (or point (point)) (car props))
					(get-props (cdr props)))
				 (->> (alist-get (car props) payload)
				      (get-props (cdr props)))))
			   payload)))
    (if props 
	(get-props props)
      (let ((inhibit-field-text-motion t))
	(get-text-property (or point (point)) reorg--data-property-name)))))

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
	     when (funcall
		   test
		   (alist-get
		    prop
		    (reorg--get-view-prop prop))
		   val)
	     collect (cons beg end))))

(defun reorg--get-next-prop (prop &optional val test)
  "Find the next text prop PROP that matches VAL.
Returns (beg . end) points of the matching property."
  (when-let ((next (reorg--find-prop prop val (point) nil test)))
    (cl-loop for (beg . end) in next
	     unless (and (>= (point) beg)
			 (<= (point) end))
	     return (cons beg end))))

(defun reorg--get-previous-prop (prop &optional val test)
  "Find the previous text prop PROP that matches VAL.
Returns (beg . end) points of the matching property."
  (when-let ((previous (reorg--find-prop prop val nil (point) test)))
    (cl-loop for (beg . end) in (reverse previous)
	     unless (and (>= (point) beg)
			 (<= (point) end))
	     return (cons beg end))))

(defun reorg--goto-char (point)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--goto-next-prop (prop &optional val test)
  "Go to next PROP that matches VAL."
  (when-let ((target (reorg--get-next-prop prop val test)))
    (reorg--goto-char (car target))))

(defun reorg--goto-previous-prop (prop &optional val test)
  "Go to next PROP that matches VAL."
  (when-let ((target (reorg--get-previous-prop prop val test)))
    (reorg--goto-char (car target))))


