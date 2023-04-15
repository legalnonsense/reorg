;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/reorg
;; Version: 0.0.1

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; TODO re-write so that all data is stored in a hashtable
;; and the only thing stored in a text property is the key
;; to the hash table 

;;; requires

(eval-when-compile
  (require 'cl-lib))
(require 'outline)
(require 'org)
;; (require 'org-agenda)
(require 'seq)
(eval-when-compile
  (require 'org-macs))
(require 'let-alist)
(require 'dash)
(require 'f)
(require 's)
(require 'org-visual-indent nil t)

;;; constants

(defconst reorg--data-property-name 'reorg-data)

(defconst reorg--field-property-name 'reorg-field-type)

(defconst reorg--valid-template-keys '(:source
				       :sources
				       :group
				       :groups 
				       :children
				       :override
				       :overrides
				       :post-override
				       :post-overrides
				       :sort-result
				       :sort-results
				       :bullet
				       :bullets
				       :folded-bullet
				       :folded-bullets
				       :format-result
				       :format-results
				       :sort-group
				       :sort-groups)
  "Allowable template keys.")

;;; customs

(defcustom reorg-toggle-shortcut "C-; r"
  "shortcut to open tree side window")

(defcustom reorg-parser-use-id-p t
  "use id or markers?")

(defcustom reorg-buffer-name "*REORG*"
  "Default buffer name for tree view window.")

(defcustom reorg-buffer-side 'left
  "Which side for the tree buffer?")

(defcustom reorg-face-text-prop 'font-lock-face
  "When setting a face, use this text property.")

(defcustom reorg-headline-format '(.stars " " .headline)
  "Default headline format.")

(defcustom reorg-default-bullet  "->" "")

(defcustom reorg-default-face 'default "")

(defcustom reorg-default-result-sort nil "")

;;; variables 

;; TODO move into the 'reorg-data text property
(defvar reorg--field-property-name 'reorg-field-name "")

(defvar reorg--temp-parser-list nil "")

(defvar reorg--grouper-action-function
  #'reorg--create-headline-string
  "")

(defvar reorg--current-template nil
  "the current template in this buffer")

(defvar reorg--current-sources nil
  "the current template in this buffer")

(defvar reorg--navigation-hook nil
  "Post-navigation hook.")

(defvar reorg--extra-prop-list nil "")

(defvar reorg--getter-list nil "")

(defvar reorg--parser-list nil "")

(defvar reorg--render-func-list nil "")

;;; reorg requires
(defun reorg--create-symbol (&rest args)
  "Create a symbol from ARGS which can be
numbers, strings, symbols."
  (cl-loop for arg in args
	   if (stringp arg)
	   concat arg into ret
	   else if (numberp arg)
	   concat (number-to-string arg) into ret
	   else if (symbolp arg)
	   concat (symbol-name arg) into ret
	   finally return (intern ret)))

(defun reorg--get-prop (&optional property point)
  "Get PROPERTY from the current heading.  If PROPERTY
is omitted or nil, get the 'reorg-data' prop.  If it is
supplied, get that property from 'reorg-data'."
  (let ((props (get-text-property (or point (point)) 'reorg-data)))
    (if property
	(alist-get property props)
      props)))

(defun reorg--get-display-func-name (class name)
  "Create `reorg--CLASS--display-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--display-
			name))

(defun reorg--get-parser-func-name (class name)
  "Create `reorg--CLASS--parse-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--parse-
			name))

;;;; Macros 

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

       (defun ,(reorg--get-parser-func-name name 'class-name) (&rest _)
	 "" (symbol-name ',name))
       (cl-pushnew (cons 'class (reorg--get-parser-func-name ',name 'class-name))
		   (alist-get ',name reorg--parser-list))
       
       (defun ,(reorg--get-parser-func-name name 'class) (&rest _)
	 "" ',name)
       (cl-pushnew (cons 'class (reorg--get-parser-func-name ',name 'class))
		   (alist-get ',name reorg--parser-list))

       ;; (defun ,(reorg--get-parser-func-name name 'buffer-file-name) (&rest _)
       ;;   "" (buffer-file-name))
       ;; (cl-pushnew (cons 'class (reorg--get-parser-func-name ',name 'buffer-file-name))
       ;; 		 (alist-get ',name reorg--parser-list))

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
				     doc
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
		,doc
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


;;; code 

;;;; convenience functions

(defmacro reorg--create-string-comparison-funcs ()
  "Create string comparison functions that ignore case:
reorg-string<, reorg-string=, reorg-string>.  These functions
are convenience functions for writing templates.  Also
transform any nil value into an empty string." 
  `(progn 
     ,@(cl-loop for each in '("<" ">" "=" )
		collect `(defun ,(intern (concat "reorg-string" each)) (a b)
			   ,(concat "like string" each " but ignore case"
				    "and allow nils")
			   (,(intern (concat "string" each))
			    (if a (downcase a) "")
			    (if b (downcase b) ""))))))

(reorg--create-string-comparison-funcs)

(defun reorg--add-number-suffix (num)
  "create the suffix for a number"
  (pcase (if (numberp num) 
	     (number-to-string num)
	   num)
    ((pred (s-ends-with-p "11")) "th")
    ((pred (s-ends-with-p "12")) "th")
    ((pred (s-ends-with-p "13")) "th")
    ((pred (s-ends-with-p "1")) "st")
    ((pred (s-ends-with-p "2")) "nd")
    ((pred (s-ends-with-p "3")) "rd")
    (_ "th")))

(defun reorg--sort-by-list (a b seq &optional predicate list-predicate)
  "Apply PREDICATE (default `<') to the respective indices in sequence
SEQ for the value of A and value of B.  A and B are members of SEQ if
`equal' to a value in the list.  Specificy a different test using
LIST-PREDICATE."
  (let ((a-loc (seq-position seq a (or list-predicate #'equal)))
	(b-loc (seq-position seq b (or list-predicate #'equal))))
    (cond
     ((and (null a-loc) (null b-loc)) nil)
     ((null a-loc) nil)
     ((null b-loc) t)
     (t (funcall (or predicate #'<) a-loc b-loc)))))

;;;; programmer utilities

(defun reorg--add-remove-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix.
Returns PROP with a colon prefix. If REMOVE is t,
then return PROP with no colon prefix."
  (pcase `(,remove ,(keywordp prop))
    (`(t t) (intern (substring (symbol-name prop) 1)))
    (`(nil nil) (intern (concat ":" (symbol-name prop))))
    (_ prop)))

(defun reorg--walk-tree (tree func &optional data)
  "Apply func to each element of tree and return the results.
If DATA is provided, call FUNC with DATA as an argument.
Otherwise call FUNC with no arguments."
  ;; I am not sure why I put the data option here.
  (cl-labels
      ((walk
	(tree &optional d)
	(cl-loop for each in tree
		 if (listp each)
		 collect (walk each d)
		 else
		 collect (if data
			     (funcall func each d)
			   (funcall func each)))))
    (if (listp tree)
	(walk tree data)
      (if data 
	  (funcall func tree data)
	(funcall func tree)))))

(defun reorg--get-all-tree-paths (tree leaf-func)
  "Get a list of all paths in tree.
LEAF-FUNC is a function run on each member to determine
whether the current tree is a leaf. 

For example:

(reorg--get-all-tree-paths '((1 (2 (- 3 4 5))
				(6 (7 (- 8 9))
				   (10 (- 11)
                                        (- 12)))))
			   (lambda (x) (eq '- (car x))))
returns:

'((1 2 - 3 4 5)
  (1 6 7 - 8 9)
  (1 6 10 - 11)
  (1 6 10 - 12))

Must supply LEAF-FUNC to return non-nil when a leaf is encountered
because the data in the leaf could be a list or tree itself."
  (let (paths)
    (cl-labels ((doloop (tree &optional path)
			(cond ((funcall leaf-func tree)
			       (push (append (reverse path) tree) paths))
			      ((or (stringp (car tree))
				   (numberp (car tree))
				   (symbolp (car tree)))
			       (push (car tree) path)
			       (cl-loop for child in (cdr tree)
					do (doloop child path)))
			      (tree (cl-loop for child in tree
					     do (doloop child path))))))
      (doloop tree)
      (reverse paths))))

;;;; window control

(defun reorg--buffer-p ()
  "Are you in the reorg buffer?"
  (string= (buffer-name)
	   reorg-buffer-name))

(defun reorg--open-side-window ()
  "Open a side window to display the tree."
  (display-buffer-in-side-window (get-buffer-create reorg-buffer-name)
				 `((side . ,reorg-buffer-side)
				   (dedicated . t)
				   (slot . nil)
				   (window-parameters . ((reorg . t)))))
  (reorg--select-tree-window)
  ;; TODO figure out a dwim method of setting sidebar size
  ;; or make it a defcustom. See `reorg--get-longest-line-length'
  ;; It's suprisingly tricky to calculate the length of a line that
  ;; includes :align-to display text props and includes fonts of
  ;; a different height.
  (balance-windows))

(defun reorg--select-main-window (&optional buffer)
  "Select the source window. If BUFFER is non-nil,
switch to that buffer in the window." 
  (select-window (window-main-window))
  (when buffer
    (switch-to-buffer buffer)))

(defun reorg--select-tree-window ()
  "Select the tree window."
  (when-let ((window (--first 
		      (window-parameter it 'reorg)
		      (window-at-side-list nil reorg-buffer-side))))
    (select-window window)))

(defun reorg--render-source ()
  "Render the heading at point."
  (when (reorg--get-prop 'class)
    (when-let ((func (alist-get
		      (reorg--get-prop 'class)
		      reorg--render-func-list
		      nil nil #'equal)))
      (funcall func)
      ;;FIXME this is redundant. see `reorg--goto-source'
      (when (reorg--buffer-in-side-window-p)
	(reorg--select-tree-window)))))

(defun reorg--goto-source ()
  "Goto rendered source buffer."
  (interactive)
  (reorg--render-source)
  (when (reorg--buffer-in-side-window-p)
    (reorg--select-main-window)))

;;;; Box cursor line overlay 

;; name needs updating 
(defvar reorg-edits--current-field-overlay nil
  "Overlay for field at point.")

;; old stuff that has stuck around
(defun reorg-edits--get-field-bounds ()
  "Get the bounds of the field at point."
  (let ((match (save-excursion (text-property--find-end-forward
				(point)
				'reorg-data
				(reorg--get-prop)
				#'equal))))
    (cons
     (prop-match-beginning match)
     (prop-match-end match))))


(let ((point nil))
  ;; I don't know why I used a closure.
  (defun reorg-edits--update-box-overlay ()
    "Tell the user what field they are on."
    (unless (= (point) (or point 0))
      (when-let ((field (get-text-property (point) reorg--field-property-name)))
	(delete-overlay reorg-edits--current-field-overlay)
	(move-overlay reorg-edits--current-field-overlay
		      (car (reorg-edits--get-field-bounds))
		      (let ((inhibit-field-text-motion t))
			(if (= (point) (point-at-bol))
			    (point-at-eol)
			  (cdr (reorg-edits--get-field-bounds))))))
      (setq point (point)))))

;;;; programatically interacting with tree buffer 


(defun reorg--goto-next-prop (property &optional
				       value
				       limit
				       predicate
				       visible-only
				       current)
  "Assume we are getting 'reorg-data and PROPERTY is the key of that alist.
Does not run 'reorg--navigation-hooks'.  CURRENT means to consider the
text at point to see if it matches."
  ;; I'd be shocked if there are no bugs here. 
  (cond
   ((eobp)
    nil)
   ((if limit
	(> (point) limit)
      (= (point) (point-max)))
    nil)
   ((and current
	 (funcall (or predicate #'equal)
		  (alist-get property (reorg--get-prop))
		  value))
    (point))
   (t    
    (let ((origin (point))
	  (done nil)
	  (limit (or limit (point-max)))
	  pos)
      (cl-loop with found = nil
	       while (not done)
	       do (setq pos (next-single-property-change
			     (point)
			     'reorg-data nil limit))
	       if (or (not pos)		
		      (> pos limit))
	       return
	       (progn (reorg--goto-char origin)
		      (setq done t)
		      nil)
	       else do
	       (progn (goto-char pos)
		      (if (and (< (point) limit)
			       (if visible-only
				   (not (org-invisible-p (point) t))
				 t)
			       (funcall (or predicate #'equal)
					value
					(if property 
					    (alist-get property 
						       (get-text-property
							(point)
							'reorg-data))
					  (get-text-property (point)
							     'reorg-data))))
			  (progn 
			    (setq done t)
			    (setq found t))
			(when (or (not pos)
				  (>= pos limit))
			  (goto-char origin)
			  (setq done t))))
	       finally return (if (not found)
				  nil
				(point)))))))

(defun reorg--goto-previous-prop (property &optional value limit
					   predicate visible-only current)
  "See `reorg--goto-next-prop'"
  (cond
   ((bobp)
    nil)
   ((< (point) (or limit (point-min)))
    nil)
   (t    
    (let ((origin (point))
	  (done nil)
	  (limit (or limit (point-min)))
	  pos)
      (cl-loop with found = nil
	       with pos = nil 
	       while (not done)
	       do (setq pos
			(previous-single-property-change
			 (point)
			 'reorg-data
			 nil
			 limit))
	       if (or (not pos)		
		      (< pos limit))
	       return
	       (progn (reorg--goto-char origin)
		      (setq done t)
		      nil)
	       else do
	       (progn (goto-char pos)
		      (if (and (>= (point) limit)
			       (funcall
				(or predicate #'equal)
				value
				(if property 
				    (alist-get
				     property
				     (get-text-property
				      (point)
				      'reorg-data))
				  (get-text-property
				   (point)
				   'reorg-data)))
			       (if visible-only
				   (not (org-invisible-p (point) t))
				 t))
			  (progn 
			    (setq done t)
			    (setq found t))
			(when (or (not pos)
				  (bobp)
				  (<= pos limit))
			  (goto-char origin)
			  (setq done t))))
	       finally return (if (not found)
				  nil
				(point)))))))

(defun reorg--get-previous-prop (property &optional
					  value
					  limit
					  predicate
					  visible-only
					  current)
  "Save-excursion wrapper for `reorg--goto-previous-prop'."
  (save-excursion (reorg--goto-previous-prop
		   property
		   value
		   limit
		   predicate
		   visible-only
		   current)))

(defun reorg--get-next-prop (property &optional
				      value
				      limit
				      predicate
				      visible-only
				      current)
  "Save-excursion wrapper for `reorg--goto-previous-prop'."
  (save-excursion (reorg--goto-next-prop
		   property
		   value
		   limit
		   predicate
		   visible-only
		   current)))

(defmacro reorg--create-navigation-commands (alist)
  "Create navigation commands. ALIST is a list in the form of (NAME . FORM)
where NAME is the name of what you are moving to, e.g., \"next-heading\"
and FORM is evaluated to see if that target exists.

This creates two interactive functions:
  reorg--get-NAME
  reorg--goto-NAME."
  `(progn 
     ,@(cl-loop
	for (name . form) in alist
	append (list `(defun ,(reorg--create-symbol 'reorg--goto- name)
			  (&optional no-update)
			,(concat "Move point to "
				 (s-replace "-" " " (symbol-name name))
				 " and run navigation hook.")
			(interactive)
			(when-let ((point ,form))
			  (if no-update
			      (goto-char point)
			    (reorg--goto-char point))))
		     `(defun ,(reorg--create-symbol 'reorg--get- name) nil
			,(concat "Get the point of "
				 (s-replace "-" " " (symbol-name name))
				 ".")
			,form)))))

(reorg--create-navigation-commands
 ;; If you want to debug, just pp macro expand and instrument
 ;; the defun in a scratch buffer.  
 ((next-leaf-sibling . (reorg--get-next-prop 'reorg-field-type
					     'leaf
					     (reorg--get-next-parent)))
  (next-heading . (reorg--get-next-prop
		   nil
		   nil
		   nil
		   (lambda (a b) t)))
  (next-visible-heading . (reorg--get-next-prop
			   nil
			   nil
			   nil
			   (lambda (a b) t) t))
  (previous-heading . (reorg--get-previous-prop
		       nil
		       nil
		       nil
		       (lambda (a b) t)))
  (next-branch . (reorg--get-next-prop
		  'reorg-branch
		  t
		  nil
		  nil
		  nil))
  (next-visible-branch . (reorg--get-next-prop
			  'reorg-branch
			  t
			  nil
			  nil
			  t))
  (previous-visible-heading . (reorg--get-previous-prop
			       'reorg-field-type
			       t
			       nil
			       (lambda (a b) t)
			       t))
  (next-sibling . (reorg--get-next-prop
		   'reorg-level
		   (reorg--get-prop 'reorg-level)
		   (reorg--get-next-prop 'reorg-level
					 (reorg--get-prop
					  'reorg-level)
					 nil
					 (lambda (a b)
					   (< b a)))))
  (previous-sibling . (reorg--get-previous-prop
		       'reorg-level
		       (reorg--get-prop 'reorg-level)
		       (reorg--get-previous-prop
			'reorg-level
			(reorg--get-prop 'reorg-level)
			nil
			(lambda (a b) (< b a)))))
  (next-clone . (reorg--get-next-prop
		 'id
		 (reorg--get-prop 'id)))
  (previous-clone . (reorg--get-previous-prop
		     'id
		     (reorg--get-prop 'id)))
  (next-parent . (reorg--get-next-prop
		  'reorg-level
		  (reorg--get-prop 'reorg-level)
		  nil
		  (lambda (a b) (> a b))))
  (parent . (reorg--get-previous-prop
	     'reorg-level
	     (1- (reorg--get-prop 'reorg-level))))
  (root . (and (/= 1 (reorg--get-prop 'reorg-level))
	       (reorg--get-previous-prop 'reorg-level 1)))
  (next-child . (reorg--get-next-prop
		 'reorg-level
		 (1+ (reorg--get-prop 'reorg-level))
		 (reorg--get-next-prop
		  'reorg-level
		  (reorg--get-prop 'reorg-level)
		  nil
		  (lambda (a b)
		    (>= a b)))))
  (next-visible-child . (and
			 (reorg--get-prop 'reorg-branch)
			 (reorg--get-next-prop
			  'reorg-level
			  (1+ (reorg--get-prop 'reorg-level))
			  (reorg--get-next-prop
			   'reorg-level
			   (reorg--get-prop 'reorg-level)
			   nil
			   (lambda (a b)
			     (>= a b)))
			  nil
			  t)))))

(defun reorg--goto-char (point &optional no-hook)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (unless no-hook 
    (run-hooks 'reorg--navigation-hook))
  (point))

;;;; Tree buffer movement and commands

(defun reorg--leaves-visible-p ()
  "are the leaves of the heading visible?"
  (when-let ((p (reorg--goto-last-leaf-depth-first)))
    (not (invisible-p p))))

(defun reorg--move-to-next-entry-follow ()
  "move to next entry"
  (interactive)
  (reorg--goto-next-visible-heading)
  (reorg--render-source)
  (reorg--select-tree-window)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--move-to-previous-entry-follow ()
  "move to previous entry"
  (interactive)
  (reorg--goto-previous-visible-heading)
  (reorg--render-source)
  (reorg--select-tree-window)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--move-to-next-entry-no-follow ()
  "next entry"
  (interactive)
  (reorg--goto-next-visible-heading)
  (reorg--render-source)
  (reorg--select-tree-window)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--move-to-previous-entry ()
  "move to previous entry"
  (interactive)
  (reorg--goto-previous-visible-heading)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--move-to-next-entry ()
  "next entry"
  (interactive)
  (reorg--goto-next-visible-heading)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--move-to-previous-entry-no-follow ()
  "previous entry"
  (interactive)
  (reorg--goto-previous-visible-heading)
  (reorg--render-source)
  (reorg--select-tree-window)
  (run-hooks 'reorg--navigation-hook))

;;;; mapping functions

(defmacro reorg--map-id (id &rest body)
  "Execute BODY at each entry that matches ID."
  `(org-with-wide-buffer 
    (goto-char (point-min))
    (let ((id ,id))
      (while (reorg--goto-next-prop 'id id)
	,@body))))

(defun reorg--map-all-branches (func)
  "map all headings"
  (save-excursion 
    (goto-char (point-min))
    (while (reorg--goto-next-branch)
      (funcall func))))

;;;; insertion navigation code

(defun reorg--goto-id (header &optional group)
  "goto ID that matches the header string"
  (let ((point (point)))
    (goto-char (point-min))
    (if (reorg--goto-next-prop
	 (if group 'group-id 'id)
	 (alist-get (if group 'group-id 'id) header))
	(point)
      (reorg--goto-char point)
      nil)))

(defun reorg--get-parent-id (&optional data)
  "get the parent ID of DATA or of the header at point"
  (let* ((id-path (reverse (if data (alist-get 'id-path data)
			     (reorg--get-prop 'id-path))))
	 (parent (cadr id-path)))
    parent))

(defun reorg--goto-group-and-id (id group-id)
  "goto the next heading that has ID and GROUP-ID"
  (let ((point (point)))
    (goto-char (point-min))
    (cl-loop when (and (equal id
			      (reorg--get-prop 'id))
		       (equal group-id
			      (reorg--get-prop 'group-id)))
	     return t
	     ;; this is a hack.  It should use
	     ;; `reorg--goto-next-prop'
	     while (reorg--goto-next-heading)
	     finally (progn (goto-char point) nil))))

(defun reorg--goto-first-group-member (data)
  "Same hacky bullshit."
  (let ((parent-id (reorg--get-parent-id data))
	(group-id (alist-get 'group-id data))
	(point (point)))
    (goto-char (point-min))
    (cl-loop when (and (equal parent-id
			      (reorg--get-parent-id))
		       (equal group-id
			      (reorg--get-prop 'group-id)))
	     return t
	     while (reorg--goto-next-branch)
	     finally return (progn (goto-char point)
				   nil))))

(defun reorg--goto-next-sibling-same-group (&optional data)
  "Just what the function name says."
  ;; I won't pretend to enjoy writing code to nativate
  ;; an outline.  Whatever hacky shit seems to work
  ;; is what I use. 
  (interactive)
  (let ((parent-id (if data 
		       (reorg--get-parent-id data)
		     (reorg--get-parent-id (reorg--get-prop))))
	(group-id (if data
		      (alist-get 'group-id data)
		    (reorg--get-prop 'group-id)))
	(point (point)))
    (when (reorg--goto-next-sibling)
      (if (and (equal parent-id
		      (reorg--get-parent-id))
	       (equal group-id
		      (reorg--get-prop 'group-id)))
	  (point)
	(goto-char point)
	(run-hooks 'reorg--navigation-hook)
	nil))))

(defun reorg--goto-last-leaf-depth-first ()
  "goto last leaf of current tree"
  ;; this function doesn't seem to work the
  ;; way it was intended, but it might still
  ;; be in use somewhere. 
  (while (reorg--goto-next-leaf-sibling)))

(defun reorg--find-header-location (header-string)
  "Find the location of HEADER-STRING in the current outline.
Assume that the point is on the first header of the group."
  (setq header-string (if (stringp header-string)
			  (get-text-property 0 'reorg-data header-string)
			header-string))
  (let-alist header-string
    (when (reorg--goto-first-group-member header-string)
      (if (equal .branch-name (reorg--get-prop 'branch-name))
	  (point)
	(if .sort-group
	    (cl-loop with point = (point)
		     when (funcall .sort-group
				   .branch-name
				   (reorg--get-prop 'branch-name))
		     return (point)
		     while (reorg--goto-next-sibling-same-group
			    header-string)
		     finally return (progn (when (reorg--has-leaves-p)		     
					     (reorg--goto-last-leaf-depth-first)
					     (forward-line))
					   (point)))
	  (cl-loop with point = (point)
		   when (equal .branch-name
			       (reorg--get-prop 'branch-name))
		   return t
		   while (reorg--goto-next-sibling-same-group
			  header-string)
		   finally return
		   (if (reorg--has-leaves-p)		     
		       (reorg--goto-last-leaf-depth-first)
		     (forward-line))))))))

(defun reorg--has-leaves-p ()
  "does the header have leaves?  Maybe.  You won't know because
this function actually checks to see if the current header
has any children."
  (save-excursion 
    (reorg--goto-next-child)))

(defun reorg--at-leaf-p ()
  "at a leaf?"
  (eq 'leaf (reorg--get-prop 'reorg-field-type)))

(defun reorg--find-leaf-location (leaf-string &optional result-sorters)
  "find the location for LEAF-DATA among the current leaves. put the
point where the leaf should be inserted (ie, insert before). Assume
that the point is on the parent heading, and the location is within
the following leaves."
  ;; goto the first leaf if at a branch
  ;; (push leaf-string xxx)
  (if (eq 'leaf (reorg--get-prop 'reorg-field-type))
      (error "You cannot run this function on a leaf.")
    (if-let ((result-sorters
	      (or result-sorters
		  (reorg--get-prop 'sort-results))))
	(progn
	  (reorg--goto-next-leaf-sibling)
	  (let ((leaf-data (if (stringp leaf-string)
			       (get-text-property 0 'reorg-data leaf-string)
			     leaf-string)))
	    (cl-loop
	     with point = (point)
	     when (cl-loop for (func . pred) in result-sorters
			   with a = nil
			   with b = nil
			   do (setq a (funcall
				       `(lambda (x) (let-alist x ,func))
				       leaf-data)
				    b (funcall
				       `(lambda (x) (let-alist x ,func))
				       (reorg--get-prop)))
			   unless (equal a b)
			   return (and a b (funcall pred a b)))
	     return (point)
	     while (reorg--goto-next-sibling-same-group)
	     finally return nil)))
      (reorg--goto-last-leaf-depth-first)
      (forward-line))))

(defun reorg--goto-same-id-after-insert (data)
  "goto the same thing that was just changed, and if it's disappeared
find the nearest clone."
  ;; THIS IS PROBABLY NOT THE DESIRED BEHAVIOR AFTER A HEADING IS CHANGED
  ;; USER MIGHT WANT TO KEEP THE POINT AT POINT
  (let* ((id (if (stringp data)
		 (alist-get 'id 
			    (get-text-property 0 'reorg-data data))
	       (alist-get 'id data)))
	 (next (reorg--get-next-prop 'id id nil nil nil t))
	 (previous (reorg--get-previous-prop 'id id nil nil nil t))
	 (next-distance (and next
			     (- next (point))))
	 (previous-distance (and previous
				 (- (point) previous))))
    (cond ((and next previous)
	   (if (< next previous)
	       (goto-char next)
	     (goto-char previous)))
	  (next (goto-char next))
	  (previous (goto-char previous)))
    (run-hooks 'reorg--navigation-hook)))

(defun reorg--insert-new-heading (data)
  "take DATA (a suitable alist) run it through the parser,
and insert as many copies of it as necessary in the appropriate
locations."
  (let ((final-leaf nil)
	(point (point)))
    (save-excursion 
      (goto-char (point-min))
      ;; This code is so fragile that I dare not
      ;; remove this obviously unnecessary hook.
      (run-hooks 'reorg--navigation-hook)
      (setq
       data (reorg--get-group-and-sort
	     (list data)
	     reorg--current-template
	     1
	     t)
       data (reorg--get-all-tree-paths
	     data
	     (lambda (x)
	       (and (listp x)
		    (stringp (car x))
		    (eq
		     'leaf
		     (get-text-property
		      0
		      'reorg-field-type
		      (car x)))))))
      ;; (setq cursor-type 'box)
      (cl-loop for group in data
	       do (goto-char (point-min))
	       and do (cl-loop
		       with leaf = (car (last group))
		       with headings = (butlast group)
		       with stop = nil
		       while (not stop)
		       for heading in headings
		       for n from 0
		       when (and heading
				 ;; This is sure to be a problem in
				 ;; some rare cases as I have not
				 ;; solved how to deal with blank headings
				 (not (string= "" heading)))
		       do (let* ((props (get-text-property 0 'reorg-data heading))
				 (id (alist-get 'id props)))
			    (unless (reorg--goto-next-prop 'id id nil nil nil t)
			      (unless (reorg--find-header-location heading)
				(forward-line))
			      (cl-loop for x from n to (1- (length headings))
				       do
				       (reorg--insert-header-at-point
					(nth x headings))
				       and do (forward-line)
				       finally
				       (progn
					 (setq stop t)
					 (setq final-leaf leaf)
					 (reorg--insert-header-at-point
					  leaf)))))
		       finally (unless stop
				 (setq final-leaf leaf)
				 (let ((afterp (not (reorg--find-leaf-location leaf))))
				   (reorg--insert-header-at-point
				    leaf afterp))))))
    (goto-char point)
    (reorg--goto-same-id-after-insert final-leaf)))

(defun reorg--at-last-leaf-p ()
  "at the last leaf?"
  (not 
   (save-excursion 
     (reorg--goto-next-leaf-sibling))))

(defun reorg--get-next-group-id-change ()
  "get next group id change"
  ;; this could be defined in the macro
  (reorg--get-next-prop 'group-id
			(reorg--get-prop)
			nil
			(lambda (a b)
			  (not (equal a b)))))

;;;; window selector

(defun reorg--buffer-in-side-window-p ()
  "Is the reorg buffer in a side window?"
  (cl-loop for window in (window-at-side-list nil reorg-buffer-side)
	   when (window-parameter window 'reorg)
	   return window))

;; Be wary of this function along with `reorg--render-maybe'.
(defun reorg--select-window-run-func-maybe (window &optional func switch-back)
  "WINDOW is either 'main or 'tree. FUNC is a function with no args."
  (when-let ((win
	      (seq-find
	       (lambda (x)
		 (window-parameter x 'reorg))
	       (window-at-side-list nil reorg-buffer-side))))
    (pcase window
      ('tree (reorg--select-tree-window)
	     (funcall func)
	     (when switch-back
	       (reorg--select-main-window)))
      ('main (funcall func)
	     (when switch-back
	       (reorg--select-tree-window))))))

(defun reorg--render-maybe ()
  "maybe render if we are in a tree window."
  (reorg--select-window-run-func-maybe 'main #'reorg--render-source t))

;;;; outline management 

(defun reorg--refresh-org-visual-outline ()
  "refresh the outline indentation for the current subtree."
  (when-let ((beg (and org-visual-indent-mode
		       (reorg--get-parent)))
	     (end (or (reorg--get-next-parent)
		      (point-max))))
    (org-visual-indent--org-indent-add-properties beg end)))

(defun reorg-views--delete-leaf ()
  "delete the heading at point"
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

(defun reorg--delete-headers-maybe ()
  "Delete header, and its parents, at point if there
are no children.  Error if the point is at a branch."
  (if (reorg--at-leaf-p)
      (error "not at a branch.")
    (cl-loop with p = nil
	     if (reorg--get-next-child)
	     return t
	     else
	     do (setq p (reorg--get-parent))
	     do (reorg--delete-header-at-point)
	     if (null p)
	     return t
	     else do (goto-char p))))

(defun reorg--delete-dangling-headers ()
  "delete any unnecessary headers"
  (reorg--map-all-branches
   #'reorg--delete-headers-maybe))

(defun reorg--insert-all (data)
  ;; it might be better to change `reorg--grouper-action-function'
  ;; i.e., `reorg--create-headline-string', so that it inserts
  ;; directly into the buffer and then bypass this method of printing
  ;; the outline.
  ;;
  ;; update: it's only slightly better.
  "Insert grouped and sorted data into outline."
  (let (results)
    (cl-labels ((recurse (data)
			 (cond ((stringp data)
				(insert data))
			       (data (cl-loop for entry in data
					      do (recurse entry))))))
      (recurse data))))

(defun reorg--delete-header-at-point ()
  "delete the header at point"
  ;; TODO update this to detect the span of a heading
  ;; using text properties instead of assuming a heading
  ;; is only one line 
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

(defun reorg--insert-header-at-point (header-string &optional next-line)
  "Insert HEADER-STRING at point.  If NEXT-LINE is non-nil,
insert it on the next line.  Run navigation hook after insertion."
  (when next-line
    (forward-line))
  (save-excursion 
    (insert header-string))
  (reorg-bullets--fontify-heading)
  (run-hooks 'reorg--navigation-hook)
  (reorg--refresh-org-visual-outline))

(defun reorg--delete-entries (id)
  "Delete all heads that are associated with ID."
  (reorg--map-id id
		 (let ((parent (reorg--get-parent)))
		   (reorg-views--delete-leaf)
		   (when parent
		     (goto-char parent)
		     (reorg--delete-headers-maybe)
		     (reorg--refresh-org-visual-outline)))))

;;; core functions (fetching, parsing, grouping, sorting, formatting)

(defun reorg--getter (sources)
  "Get entries from SOURCES, whih is an alist
in the form of (CLASS . SOURCE)."
  (cl-loop for (class . source) in sources
	   append (funcall (car (alist-get class reorg--getter-list))
			   source)))

(defun reorg--get-all-dotted-symbols (form)
  "Get all dotted symbols in FORM including any
dotted symbols necesasry from CLASS.

Also get any nested dots by returning only the first dot:
.property.category will only return property. 

Don't include the .class or .stars values.
Return unique values.
Don't return nils."
  (seq-filter (lambda (x) (when (and
				 x
				 (not (eq x 'class))
				 (not (eq x 'stars)))
			    x))
	      (seq-uniq
	       (cl-loop for (x . y) in (let-alist--deep-dot-search form)
			collect (--> y
				     (symbol-name it)
				     (if (string-match "^[@!]" it)
					 (substring it 1)
				       it)
				     (if (string-match "\\." it)
					 (car (s-split "\\." it))
				       it)
				     (intern it))))))



(defun reorg--get-all-dotted-symbols-in-fun (fun)
  "return a list of all of the dotted symbols in a
function's code."
  (pcase-let ((`(,buf . ,mark) (find-function-noselect fun t)))
    (with-current-buffer buf
      (save-excursion 
	(save-restriction
	  (goto-char mark)
	  (narrow-to-defun)
	  (cl-loop
	   while (re-search-forward "\\_<.*?\\_>" nil t)
	   collect (match-string-no-properties 0) into results
	   finally return
	   (seq-filter
	    (lambda (x) x) ;; wtf?
	    (seq-map (lambda (x)
		       (intern 
			(cond ((or (s-starts-with-p ".!" x)
				   (s-starts-with-p ".@" x))
			       (substring x 2))
			      ((s-starts-with-p "." x)
			       (substring x 1)))))
		     (seq-uniq
		      (seq-filter
		       (lambda (s)
			 (string-match
			  "\\`\\.[@!]*\\(.+\\)" s))
		       results))))))))))

(defun reorg--pre-parser-sort (parsers)
  "sort. not sure what it does."
  (cl-loop for (class . rest) in (seq-uniq parsers)
	   collect
	   (cons class
		 (sort
		  (seq-uniq rest)
		  (lambda (a b)
		    (reorg--sort-by-list
		     a
		     b
		     (alist-get class reorg--parser-list)
		     #'<))))))

(defun reorg--pre-parser (template &optional recursed)
  "Call each parser in CLASS on DATA and return
the result.  If TYPE is provided, only run the
parser for that data type."
  ;; the purpose of the pre-parser is so that
  ;; the parser only fetches the data needed to generate
  ;; the outline.  This speeds parsing considerably
  ;; when dealing with something like an
  ;; org-mode file.  
  (cl-loop for class in
	   (seq-uniq
	    (-flatten 
	     (mapcar #'car (reorg--get-all-x-from-template
			    template
			    :sources))))
	   if recursed
	   append
	   (cl-loop
	    for dsym in (reorg--get-all-dotted-symbols template)
	    when (alist-get dsym (alist-get class reorg--parser-list))
	    collect (cons dsym
			  (reorg--get-parser-func-name class dsym))
	    when (and (alist-get dsym (alist-get class reorg--parser-list))
		      (reorg--get-all-dotted-symbols-in-fun 
		       (reorg--get-parser-func-name class dsym)))
	    append
	    (cl-loop for each in (reorg--get-all-dotted-symbols-in-fun 
				  (reorg--get-parser-func-name class dsym))
		     append
		     (reorg--pre-parser
		      `( :sources ((,class . _))
			 :groups ,(intern
				   (concat "."
					   (symbol-name 
					    each))))
		      'recursed))
	    )
	   else 
	   collect (cons class  
			 (cl-loop
			  for dsym in (append (reorg--get-all-dotted-symbols template)
					      ;; These are defaults that need
					      ;; to be included in the parsed data
					      ;; even if the user does not define them.
					      ;; This is hack to deal with that problem.
					      (list 'marker
						    'class
						    'buffer-file-name
						    'mu4e-data
						    'id
						    'fullname
						    'path))
			  
			  when (alist-get dsym (alist-get class reorg--parser-list))
			  collect (cons dsym
					(reorg--get-parser-func-name class dsym))
			  when (and (alist-get dsym (alist-get class reorg--parser-list))
				    (reorg--get-all-dotted-symbols-in-fun 
				     (reorg--get-parser-func-name class dsym)))
			  append
			  (cl-loop for each in (reorg--get-all-dotted-symbols-in-fun 
						(reorg--get-parser-func-name class dsym))
				   append
				   (reorg--pre-parser
				    `( :sources ((,class . _))
				       :groups ,(intern
						 (concat "."
							 (symbol-name 
							  each))))
				    'recursed))))))

(defun reorg--parser (data class parser-list)
  "Call each parser in CLASS on DATA and return
the result.  If TYPE is provided, only run the
parser for that data type."
  ;; to call this on an org heading at point
  ;; using all parsers that have been defined for
  ;; that type of file, run:
  ;; (reorg--parser nil 'org reorg--parser-list)
  ;; if you want to run it on an alist of data
  ;; about a file, run:
  ;; (reorg--parser ALIST 'files reorg--parser-list)
  ;;
  ;; Remember that `reorg-parser-list' is automatically
  ;; created and mantained by the `reorg-create-data-type'
  ;; and `reorg-create-class-type' macros.
  ;;
  ;; `reorg-temp-parser' contains the parser list
  ;; used to generate the current outline. 
  (if (= 1 (length (alist-get class parser-list)))
      data
    (cl-loop with DATA = nil
	     for (type . func) in (alist-get class parser-list)
	     collect (cons type (funcall func data DATA)) into DATA
	     finally return DATA)))

(defun reorg--seq-group-by (form sequence &optional return-nils)
  "Apply FORM to each element of SEQUENCE and group
the results.  See `seq-group-by'. Do not group
nil results unless RETURN-NiLS is non-nil. If FORM is
a function, then call the function with a single argument.

If FORM is not a function, then create an anonymous function
that wraps FORM in a `let-alist' and makes all of the
data in each element of SEQENCE available using
dotted notation."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (if (functionp form)
		     (funcall form elt)
		   ;; To avoid a backquoted lambda,
		   ;; this could be done using
		   ;; `reorg--turn-dot-to-val'
		   ;; but that requires using `eval'
		   (funcall `(lambda (e)
			       (let-alist e ,form))
			    elt)))
	    (cell (assoc key acc)))
       (if cell
	   (setcdr cell (push elt (cdr cell)))
	 (when (or key return-nils)
	   (push (list key elt) acc)))
       acc))
   (seq-reverse sequence)
   nil))

(defun reorg--multi-sort (functions-and-predicates sequence)
  "FUNCTIONS-AND-PREDICATES is an alist '(FORM . PREDICATE)
where predicate. FORM is is executed inside a let-alist which
means form can use dotted symbols access the members of SEQUENCE.

PREDICATE is a function that accepts two arguments and returns non-nil
if the pair is properly sorted."
  (seq-sort 
   (lambda (a b)
     (cl-loop for (form . pred) in functions-and-predicates
	      with ax = nil
	      with bx = nil
	      do (setq ax (funcall `(lambda (a) (let-alist a ,form)) a)
		       bx (funcall `(lambda (b) (let-alist b ,form)) b))	      
	      unless (equal ax bx)
	      return (funcall pred
			      ax
			      bx)))
   sequence))

(defun reorg--check-template-keys (template)
  "Ensure that the keywords in TEMPLATE are valid."
  (when-let ((invalid-keys
	      (seq-difference 
	       (cl-loop for key in template by #'cddr
			collect key)
	       reorg--valid-template-keys)))
    (error "Invalid keys in entry in template: %s" invalid-keys)))

(defun reorg--get-group-and-sort (data
				  template
				  &optional
				  level
				  ignore-sources
				  inherited-props
				  recursed)
  "Apply TEMPLATE to DATA and apply the :action-function 
specified in the template or `reorg--grouper-action-function'
to the results."
  ;; this is the core sorting function.
  ;; it's quite a ride. 
  (unless recursed

    (reorg--check-template-keys template)  
    (setq reorg--current-template template)
    (setq reorg--temp-parser-list (->> (reorg--pre-parser template)
				       (reorg--pre-parser-sort))))
  (setq level (or level 1))
  ;; inheritence 
  (let ((group-id (md5 (pp-to-string template)))
	(format-results (or (plist-get template :format-results)
			    (plist-get template :format-result)
			    (plist-get inherited-props :format-results)))
	(sort-results (or (append (plist-get inherited-props :sort-results)
				  (plist-get template :sort-results))))
	(sources (or (plist-get template :sources)
		     (plist-get template :source)))
	(action-function (or (plist-get inherited-props :action-function)
			     reorg--grouper-action-function))
	(bullet (when-let ((bullet (or (plist-get template :bullet)
				       (plist-get template :bullets)
				       (plist-get inherited-props :bullet))))
		  (if (equal bullet "")
		      (char-to-string 8203) ;; use a zero width space
		    bullet)))
	(folded-bullet (or (plist-get template :folded-bullet)
			   (plist-get template :folded-bullets)
			   (plist-get inherited-props :folded-bullet)))
	(face (or (plist-get template :face)
		  (plist-get inherited-props :face)
		  reorg-default-face))
	(group (or (plist-get template :group)
		   (plist-get template :groups)))
	(sort-groups (or (plist-get template :sort-groups)
			 (plist-get template :sort-groups)))
	results metadata)
    (cl-labels ((get-header-metadata
		 (header parent-id)
		 (let* ((id ""))
		   (list
		    (cons 'branch-name header)
		    (cons 'reorg-branch t)
		    (cons 'branch-type 'branch)
		    (cons 'sort-results sort-results)
		    (cons 'sort-group sort-groups)
		    (cons 'bullet bullet)
		    (cons 'folded-bullet folded-bullet)
		    (cons 'reorg-level level)
		    (cons 'group-id group-id)
		    (cons
		     'id
		     (let
			 ((idx (md5 (concat (pp-to-string template)
					    header
					    (pp-to-string 
					     parent-id)))))
		       (setq id idx)))
		    (cons 'id-path (append (-list parent-id)
					   (-list id))))))
		(drill!
		 ;; DANGER.  
		 (seq prop &optional n level inherited)
		 (setq level (or level 1))
		 (if-let ((groups (reorg--seq-group-by
				   `(lambda (x)				      
				      (eval
				       (reorg--walk-tree
					(reorg--walk-tree 
					 ',prop
					 (lambda (xx)
					   (reorg--turn-dot-bang-to-val
					    xx
					    ,(or n 0)
					    x)))
					(lambda (xx)					  
					  (reorg--turn-dot-to-val
					   xx
					   x)))))
				   seq
				   'return-nils)))
		     (progn 
		       (when sort-groups
  			 (setq groups (seq-sort-by #'car
						   sort-groups
						   groups)))
		       (append 
			(cl-loop
			 for each in groups                         
			 when (car each)
			 do (progn 
			      (setq metadata
				    (get-header-metadata
				     (car each)
				     (plist-get inherited-props :id-path))))
			 and collect
			 (cons (funcall action-function
					metadata
					nil
					level)
			       (list 
				(drill! (cdr each)
					prop						
					(1+ (or n 0))
					(1+ level)
					(plist-put 
					 inherited-props
					 :id-path
					 (alist-get 'id-path metadata))))))
			(when (alist-get nil groups)
			  (if-let ((children (plist-get template :children)))
			      (cl-loop for child in children
				       collect (reorg--get-group-and-sort
						(alist-get nil groups)
						child
						level
						t
						(list
						 :format-results format-results
						 :id-path
						 (plist-get inherited :id-path)
						 :sort-results sort-results
						 :parent-template template
						 :bullet bullet
						 :folded-bullet folded-bullet
						 :face face) 
						t))
			    (cl-loop
			     for each in (alist-get nil groups)
			     collect
			     (funcall
			      action-function
			      (let ((idx (or (alist-get 'id each)
					     (org-id-uuid))))
				(setf (alist-get 'group-id each) group-id)
				(setf (alist-get 'id each) idx)
				(setf (alist-get 'id-path each)
				      (append
				       (-list 
					(alist-get 'id-path metadata))
				       (-list idx)))
				each)
			      format-results
			      level
			      (plist-get template :overrides)
			      (plist-get template :post-overrides)))))))
		   (if (plist-get template :children)
		       (cl-loop for child in (plist-get template :children)
				collect (reorg--get-group-and-sort
					 seq
					 child
					 level
					 ignore-sources
					 (list :header nil
					       :format-results format-results
					       :id-path
					       (plist-get inherited :id-path)
					       :sort-results sort-results
					       :parent-template template
					       :bullet bullet
					       :folded-bullet folded-bullet
					       :face face)
					 'recursed))
		     (when sort-results
		       (setq seq 
			     (reorg--multi-sort sort-results
						seq)))
		     (cl-loop for each in seq
			      collect 
			      (funcall
			       action-function
			       (let ((idx (org-id-uuid)))
				 (setf (alist-get 'group-id each) group-id)
				 (setf (alist-get 'id each) idx)
				 (setf (alist-get 'id-path each)
				       (append (-list 
						(alist-get 'id-path metadata))
					       (-list idx)))
				 each)
			       format-results
			       level
			       (plist-get template :overrides)
			       (plist-get template :post-overrides)))))))
      ;; end drill
      
      (when (and sources (not ignore-sources))
	;; 	(cl-loop for each in sources
	;; 		 do (push each
	;; 			  reorg--current-sources)) 
	(setq data (append data (reorg--getter sources))))

      (if (null group) ;; if there's no group, skip to kids
	  ;; this allows the user to have multiple top-level
	  ;; groups instead of only one group. 
	  (cl-loop for child in (plist-get template :children)
		   collect (reorg--get-group-and-sort
			    data
			    child
			    level
			    ignore-sources
			    (list :header nil
				  :format-results format-results
				  :id nil
				  :id-path nil ;; NOT SURE
				  :sort-results sort-results
				  :parent-template template
				  :bullet bullet
				  :folded-bullet folded-bullet
				  :face face)
			    'recursed))
	
	;; begin .@
	;; If the value referenced in a .@variable
	;; is a list with multiple values, create a copy
	;; of the data for each value of in the list. so:
	;;
	;; '(((a . 1) (b . (2 3 4))))
	;;
	;; becomes:
	;;
	;;   '(((a . 1) (b . 2))
	;;     ((a . 1) (b . 3))
	;;     ((a . 1) (b . 4)))
	(when-let ((at-dots (seq-uniq 
			     (reorg--dot-at-search
			      group))))
	  (setq data (cl-loop
		      for d in data 
		      append
		      (cl-loop
		       for at-dot in at-dots
		       if (listp (alist-get at-dot d))
		       return
		       (cl-loop for x in (alist-get at-dot d)
				collect
				(let ((ppp (copy-alist d)))
				  (setf (alist-get at-dot ppp) x)
				  (setf (alist-get 'at-clone ppp) t)
				  ppp))
		       finally return data)))
	  (setq group (reorg--walk-tree group
					#'reorg--turn-dot-at-to-dot
					data)))
	;; end .@
	
	;; begin .! 
	(if (reorg--dot-bang-search group)
	    (drill! data group nil level inherited-props)
	  ;;end .!

	  ;; else we are ready to group
	  (setq results
		(pcase group 
		  ((pred functionp)
		   (reorg--seq-group-by group data))
		  ((pred stringp)
		   (list (cons group data)))
		  (_ 
		   (reorg--seq-group-by group
					data))))

	  (when results
	    ;; sort groups
	    
	    (when (and sort-groups
		       (not (and 
			     (= 1 (length results))
			     (equal "" (caar results)))))
	      (setq results
		    ;; make this a pcase and allow
		    ;; syntax same as sort results?
		    (cond ((functionp sort-groups)
			   (seq-sort-by #'car
					sort-groups
					results))
			  (t (seq-sort-by #'car
					  `(lambda (x)
					     (let-alist x
					       ,sort-groups))
					  results)))))


	    (cond ((and (plist-get template :children)
			(= 1 (length results))
			(equal "" (caar results)))
		   (setq metadata (get-header-metadata
				   "" (plist-get inherited-props :id-path)))
		   (cl-loop for (header . children) in results  
			    append
			    (cl-loop for child in (plist-get template :children)
				     collect 
				     (reorg--get-group-and-sort			  
				      children
				      child
				      level
				      ignore-sources
				      (list 
				       :header header
				       :parent-template template
				       :format-results format-results
				       :sort-results sort-results
				       :id (alist-get 'id metadata)
				       :id-path (alist-get 'id-path metadata)
				       :bullet bullet
				       :folded-bullet folded-bullet
				       :face face)
				      'recursed))))
		  ((plist-get template :children) ;; if there are children
		   (cl-loop
		    for (header . children) in results
		    append
		    (cons
		     (funcall action-function
			      (setq metadata
				    (get-header-metadata
				     header (plist-get inherited-props :id-path)))
			      nil
			      level
			      (list 
			       (cons 'header header)
			       (cons 'bullet bullet)
			       (cons 'folded-bullet folded-bullet)
			       (cons 'reorg-face face)))
		     (cl-loop for child in (plist-get template :children) 
			      collect 
			      (reorg--get-group-and-sort			  
			       children
			       child
			       (if (equal "​"
					  (alist-get
					   'branch-name
					   metadata))
				   level
				 (1+ level))
			       ignore-sources
			       (list 
				:header header
				:parent-template template
				:format-results format-results
				:sort-results sort-results
				:id (alist-get 'id metadata)
				:id-path (alist-get 'id-path metadata)
				:bullet bullet
				:folded-bullet folded-bullet
				:face face)
			       'recursed)))))
		  (t ;; no children 
		   (cl-loop for (header . children) in results
			    append
			    (cons				
			     (funcall
			      action-function
			      (setq metadata
				    (get-header-metadata
				     header
				     (plist-get inherited-props :id-path)))
			      nil
			      level
			      (plist-get template :overrides)
			      (plist-get template :post-overrides))
			     (list 
			      (cl-loop
			       with
			       children = 
			       (if sort-results
				   (reorg--multi-sort sort-results
						      children)
				 children)
			       for result in children
			       collect
			       (funcall
				action-function
				(let ((idx (or (alist-get 'id result)
					       (org-id-uuid))))
				  (setf (alist-get 'group-id result) group-id)
				  (setf (alist-get 'id result) idx)
				  (setf (alist-get 'id-path result)
					(append
					 (-list 
					  (alist-get 'id-path metadata))
					 (-list 
					  idx)))
				  result)
				format-results
				(if (equal ""
					   (alist-get
					    'branch-name
					    metadata))
				    level
				  (1+ level))
				(plist-get template :overrides)
				(plist-get template :post-overrides))))))))))))))

;;; template code search and replace

(defun reorg--dot-bang-search (data)
  "Return alist of symbols inside DATA that start with a `.!'.
Perform a deep search and return a alist of any symbol
same symbol without the `@'.

See `let-alist--deep-dot-search'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\.!" name)
	;; Return the cons cell inside a list, so it can be appended
	;; with other results in the clause below.
	(list (intern (replace-match "" nil nil name))))))
   ;; (list (cons data (intern (replace-match "" nil nil name)))))))
   ((vectorp data)
    (apply #'nconc (mapcar #'reorg--dot-bang-search data)))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (reorg--dot-bang-search (cadr data)))
   (t (append (reorg--dot-bang-search (car data))
	      (reorg--dot-bang-search (cdr data))))))

;; This function and `reorg--dot-bang-search' should be
;; made into a single function 
(defun reorg--dot-at-search (data)
  "Return alist of symbols inside DATA that start with a `.@'.
Perform a deep search and return a alist of any symbol
same symbol without the `@'.

See `let-alist--deep-dot-search'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\.@" name)
	;; Return the cons cell inside a list, so it can be appended
	;; with other results in the clause below.
	(list (intern (replace-match "" nil nil name))))))
   ;; (list (cons data (intern (replace-match "" nil nil name)))))))
   ((vectorp data)
    (apply #'nconc (mapcar #'reorg--dot-at-search data)))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (reorg--dot-at-search (cadr data)))
   (t (append (reorg--dot-at-search (car data))
	      (reorg--dot-at-search (cdr data))))))

(defun reorg--turn-dot-at-to-dot (elem &rest _ignore)
  "turn .@symbol into .symbol."
  (if (and (symbolp elem)
	   (string-match "\\`\\.@" (symbol-name elem)))
      (intern (concat "." (substring (symbol-name elem) 2)))
    elem))

(defun reorg--turn-dot-bang-to-dot (elem n d)
  "turn .!symbol into .symbol."
  (if-let ((sym (and (symbolp elem)
		     (string-match "\\`\\.!" (symbol-name elem))
		     (intern (substring (symbol-name elem) 2)))))
      (nth n (alist-get sym d))
    elem))

(defun reorg--turn-dot-bang-to-val (elem n d)
  "turn .!symbol into .symbol."
  (if-let ((sym (and (symbolp elem)
		     (string-match "\\`\\.!" (symbol-name elem))
		     (intern (substring (symbol-name elem) 2)))))
      (nth n (alist-get sym d))
    elem))

(defun reorg--turn-dot-to-val (elem d)
  "turn .symbol into its value."
  (if-let ((sym (and (symbolp elem)
		     (string-match "\\`\\." (symbol-name elem))
		     (intern (substring (symbol-name elem) 1)))))
      (alist-get sym d)
    elem))

;; TODO maybe consolidate this into the previous two
(defun reorg--turn-dot-to-display-string (elem data)
  "turn .symbol to a string using the display
function created by the `reorg-create-data-type' macro."
  (pcase elem
    ((and (pred symbolp)
	  (guard (string-match "\\`\\." (symbol-name elem))))
     (let* ((sym (intern (substring (symbol-name elem) 1)))
	    (fu (reorg--get-display-func-name
		 (alist-get 'class data)
		 (substring (symbol-name elem) 1))))
       (cond ((eq sym 'stars)
	      (make-string (alist-get 'reorg-level data) ?*))
	     ((fboundp fu) (funcall fu data))
	     (t
	      (alist-get sym data)))))
    ;; (funcall `(lambda ()
    ;; 		(let-alist ',data
    ;; 		  ,elem)))))))
    ((and `(.align ,col)
	  (guard  (numberp col)))
     (propertize " " 'display '(space . (:align-to ,col))))
    ((and `(.pad ,num ,char)
	  (guard (numberp num))
	  (guard (characterp char)))
     (make-string num char))
    (_ elem)))

(defvar reorg--cache nil "")

(defun reorg--create-headline-string (data
				      format-string

				      &optional
				      level
				      overrides
				      post-overrides
				      text-props)
  "Create a headline string from DATA using FORMAT-STRING as the
template.  Use LEVEL number of leading stars.  OVERRIDES
overrides text properties to DATA, POST-OVERRIDES overrides
text properties stored in the text property reorg-data.

FORMAT-STRING is an list of forms included dotted symbols.
This function will substitute the dotted symbols and then apply `concat'
to the resulting list. Everything in :format-string must evaluate to a
string or to nil."
  ;; This function treats branches and leaves of the outline differently.
  ;; TODO stop treating them differently, i.e., allow leaves to have leaves.
  (when data (push data reorg--cache))
  (unless (equal "" (alist-get 'branch-name data))
    (cl-flet ((create-stars (num &optional data)
			    (make-string (if (functionp num)
					     (funcall num data)
					   num)
					 ?*)))
      ;; update the DATA that will be stored in
      ;; `reorg-data'    
      (push (cons 'reorg-level level) data)
      ;; the overrides are for hacking and testing
      (cl-loop for (prop . val) in overrides
	       do (setf (alist-get prop data)
			(if (let-alist--deep-dot-search val)
			    (funcall `(lambda ()
					(let-alist ',data 
					  ,val)))
			  val)))
      (let (headline-text)
	(apply
	 #'propertize
	 ;; get the headline text 
	 (setq headline-text
	       (if (alist-get 'reorg-branch data)
		   (concat (create-stars level)
			   " "
			   (alist-get 'branch-name data)
			   "\n")
		 (let* ((new (reorg--walk-tree
			      format-string
			      #'reorg--turn-dot-to-display-string
			      data))
			(result (funcall `(lambda (data)
					    (concat ,@new "\n"))
					 data)))
		   result)))
	 ;; main data store
	 'reorg-data 
	 (progn (setq data (append data
				   (list
				    (cons 'reorg-headline
					  headline-text)
				    (cons 'reorg-class
					  (alist-get 'class data))
				    ;; (cons 'new-id
				    ;; 	  (org-id-uuid))
				    (cons 'parent-id
					  (alist-get 'parent-id data))
				    (cons 'reorg-field-type
					  (if (alist-get
					       'reorg-branch data)
					      'branch 'leaf)))))
		;; reorg-data post-overrides 
		(cl-loop for (prop . val) in post-overrides
			 do (setf (alist-get prop data)
				  (alist-get prop post-overrides)))
		data)
	 ;; legacy code 
	 reorg--field-property-name
	 (if (alist-get 'reorg-branch data)
	     'branch 'leaf)
	 ;; props are created by the class macro
	 (alist-get (alist-get 'class data)
		    reorg--extra-prop-list))))))

;;; outline metadata functions

(defun reorg--get-all-x-from-template (template x)
  "Walk the template tree and make a list of all unique template
sources.  This is used for updating the reorg tree, e.g., as part
of an org-capture hook to make sure the captured entry belongs to
one of the sources."
  (cl-labels ((get-x
	       (template)
	       (append (cl-loop for each in template
				when (plist-get each x)
				append (plist-get each x)
				append (get-x (plist-get template
							 :children)))
		       (plist-get template x))))
    (seq-uniq (get-x template))))

(defun reorg--get-all-sources-from-template (template)
  "Walk the template tree and make a list of all unique template
sources.  This is used for updating the reorg tree, e.g., as part
of an org-capture hook to make sure the captured entry belongs to
one of the sources."
  (cl-loop for each in (reorg--get-all-x-from-template template :sources)
	   append (cl-loop for x in (cdr each)
			   collect (cons (car each)
					 x))))

;;; user interface/help 

(defun reorg-help-list-modules ()
  "Let the modules available."
  (interactive)
  (pop-to-buffer (get-buffer-create "*REORG HELP*"))
  (help-mode)
  (let ((inhibit-read-only t))
    (insert "\nAvailable modules: \n")
    (cl-loop for (module . parsers) in reorg--parser-list
	     collect module into message
	     finally (insert (pp-to-string message)))))

;; (defun reorg-help-data-types ()
;;   "List data types for a given module"
;;   (interactive)
;;   (pop-to-buffer (get-buffer-create "*REORG HELP*"))
;;   (help-mode)
;;   (let ((module
;; 	 (completing-read "Select module: " (reorg--list-modules))))
;;     (cl-loop for (name . func) in (alist-get (intern module)
;; 					     reorg--parser-list)
;; 	     collect name into message 
;; 	     finally (message (pp-to-string message)))))

;;;; template completion functions 

(defun reorg-capf--annotation (candidate)
  "Get annotation for reorg-capf candidates"
  (setq candidate (intern (substring candidate 1)))
  (when-let ((results
	      (cl-loop for (key . rest) in reorg--parser-list
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
  ;; This is horribly inefficient but it does not need to be fast. 
  (let (start end collection props)
    
    (when (looking-back (rx "."
			    (or "."
				"@"
				"!"
				(one-or-more word)))
			(save-excursion (backward-sexp)
					(point)))
      (let* ((two-dots-p (s-starts-with-p ".." (match-string 0)))
	     (start (match-beginning 0))
	     (end (if two-dots-p
		      (1- (match-end 0))
		    (match-end 0))))
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

(defun reorg-enable-completion (&optional disable)
  "Enable completion using dot prefixes for data types."
  (interactive)
  (if disable
      (delete #'reorg-capf completion-at-point-functions)
    (add-to-list 'completion-at-point-functions #'reorg-capf)))

(defun reorg-disable-completion ()
  "Disable completion at point."
  (interactive)
  (reorg-enable-completion t))

;;;; opening and closing reorg 

;;;###autoload
(defun reorg-open-in-current-window (&optional template point)
  "open the reorg buffer here"
  (interactive)
  (reorg-open (or template reorg--current-template) point)
  (set-window-buffer nil reorg-buffer-name))

;;;###autoload
(defun reorg-open-sidebar (&optional template point)
  "open reorg in sidebar"
  (interactive)
  (reorg-open (or template reorg--current-template) point)
  (reorg--open-side-window)
  (reorg--select-tree-window))

(defun reorg-open-raw-data (&optional template point data)
  "I used this for debugging at some point."
  (interactive)
  (when (get-buffer reorg-buffer-name)
    (kill-buffer reorg-buffer-name))
  (with-current-buffer (get-buffer-create reorg-buffer-name)
    (erase-buffer)
    (reorg--insert-all
     (reorg--get-group-and-sort data template 1 t))
    (setq reorg--current-sources
	  (reorg--get-all-sources-from-template template)
	  reorg--current-template
	  template)
    (reorg-mode)
    (reorg--goto-char (or point (point-min)))))

;;;###autoload 
(defun reorg-open (template &optional point data)
  "Open TEMPLATE in Reorg, but do not switch to
the buffer."
  (interactive)
  (when (get-buffer reorg-buffer-name)
    (kill-buffer reorg-buffer-name))
  (with-current-buffer (get-buffer-create reorg-buffer-name)
    (erase-buffer)
    (reorg--insert-all
     (reorg--get-group-and-sort data template))
    (setq reorg--current-sources
	  (reorg--get-all-sources-from-template template)
	  reorg--current-template
	  template)
    (reorg-mode)
    (reorg--goto-char (or point (point-min)))))

(defun reorg-reload ()
  "reload the current template"
  (interactive)
  (if (reorg--buffer-in-side-window-p)
      (reorg-open-sidebar nil (point))
    (reorg-open-in-current-window nil (point))))

(defun reorg--close-tree-buffer ()
  "Close the tree buffer."
  (interactive)
  (let* ((window (seq-find
		  (lambda (x)
		    (window-parameter x 'reorg))
		  (window-at-side-list nil reorg-buffer-side)))
	 (buffer (window-buffer window)))
    (mapc #'delete-window
	  (seq-filter (lambda (x) (window-parameter x 'reorg))
		      (window-at-side-list nil reorg-buffer-side)))))

(defun reorg--toggle-tree-buffer ()
  "toggle tree buffer"
  (interactive)
  (if (seq-find
       (lambda (x)
	 (window-parameter x 'reorg))
       (window-at-side-list nil reorg-buffer-side))
      (reorg--close-tree-buffer)
    (reorg--open-side-window)
    (reorg--select-tree-window)))

;;;; major mode

(defvar reorg-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'reorg--goto-source)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "<left>") #'reorg--goto-parent)
    (define-key map (kbd "c") #'reorg--goto-next-clone)
    (define-key map (kbd "R") #'reorg-reload)
    (define-key map (kbd "f") #'reorg--goto-next-sibling)
    (define-key map (kbd "b") #'reorg--goto-previous-sibling)
    (define-key map (kbd "C") #'reorg--goto-previous-clone)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map [remap undo] #'org-agenda-undo)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "n") #'reorg--move-to-next-entry)
    (define-key map (kbd "<down>") #'reorg--move-to-next-entry)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry)
    (define-key map (kbd "<up>") #'reorg--move-to-previous-entry)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") (lambda () (interactive)
					(outline-cycle-buffer)
					(reorg-bullets--fontify-buffer)))
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "keymap")

(define-derived-mode reorg-mode
  fundamental-mode
  "Reorg"
  "Reorganize your life."
  :group 'reorg
  (setq cursor-type nil)
  (use-local-map reorg-mode-map)
  ;; (if (fboundp #'org-visual-indent-mode)
  ;;     (org-visual-indent-mode)
  ;; (org-indent-mode)
  (toggle-truncate-lines 1)
  (setq-local cursor-type nil)
  (setq reorg-edits--current-field-overlay
	(let ((overlay (make-overlay 1 2)))
	  (overlay-put overlay 'face `( :box (:line-width -1)
					:foreground ,(face-foreground 'default)))
	  (overlay-put overlay 'priority 1000)
	  overlay))
  ;; (reorg--map-all-branches #'reorg--delete-headers-maybe)  
  (add-hook 'reorg--navigation-hook #'org-show-context nil t)  
  (add-hook 'reorg--navigation-hook #'reorg-edits--update-box-overlay nil t)
  (add-hook 'reorg--navigation-hook #'reorg--render-maybe nil t)
  (reorg-bullets-mode 1)
  (when (fboundp 'org-visual-indent-mode)
    (org-visual-indent-mode 1))
  (global-set-key (kbd reorg-toggle-shortcut) #'reorg--toggle-tree-buffer)
  (reorg--goto-char 1))

(provide 'reorg)

