;; -*- lexical-binding: t; -*-

;; 1.


(let* ((template '( :group .@tag-list
		    :format-string (concat " " .headline)
		    :children (( :group .@at-name
				 :format-string (concat " " .headline)))))
       (results (--> (reorg--getter '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org")))
		     (reorg--group-and-sort it template))))
  results)



(defun reorg--turn-at-dot-to-dot (elem)
  "turn a .@symbol into .symbol
the value from the parsed data or calling the display
function created by the type creation macro."
  (if (and (symbolp elem)
	   (string-match "\\`\\.@" (symbol-name elem)))
      (intern (concat "." (substring (symbol-name elem) 2)))
    elem))

(defun reorg--depth-first-apply (form func &optional data)
  "Run FUNC at each node of TREE using a depth-first traversal
and destructively modify TREE. 
FUNC is a function that accepts one argument, which is the
current element of TREE."
  (let ((tree (copy-tree form)))
    (cl-labels ((doloop (tree func)
			(setf (car tree) (funcall func (car tree)))
			(cl-loop for n below (length (cdr tree))
				 if (listp (nth n (cdr tree))) do
				 (doloop (nth n (cdr tree)) func)
				 else do
				 (setf (nth n (cdr tree))
				       (funcall func (nth n (cdr tree)))))))
      (if (listp tree)
	  (progn 
	    (doloop tree func)
	    tree)
	(funcall func tree)))))



(cl-flet ((xxx (a) (+ 1 a)))
  (reorg--depth-first-apply '(1 2 (3 4)) #'xxx))

(let ((yyy '(concat " " .@a)))
  (reorg--depth-first-apply yyy #'reorg--turn-at-dot-to-dot))



xxx
