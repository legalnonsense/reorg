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
(require 'org-agenda) ;; needed for `org-with-remote-undo'
(require 'seq)
(require 'let-alist)
(require 'dash)
(require 's)
(require 'org-visual-indent nil t)

;;; constants

(defconst reorg--data-property-name 'reorg-data)

(defconst reorg--field-property-name 'reorg-field-type)

(defconst reorg--valid-template-keys '(:sources
				       :group
				       :children
				       :overrides
				       :post-overrides
				       :sort-results
				       :bullet
				       :folded-bullet
				       :format-results
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

(defvar reorg--extra-prop-list nil "")

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

;; TODO rename to reorg-bullets
(require 'reorg-bullets)

;;;; macro helper functions 

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

;; For whatever reason this isn't used. 
;; (defun reorg--get-parser-func-name (class name)
;;   "Create `reorg--CLASS--parse-NAME' symbol."
;;   (reorg--create-symbol 'reorg--
;; 			class
;; 			'--parse-
;; 			name))

(defun reorg--get-display-func-name (class name)
  "Create `reorg--CLASS--display-NAME' symbol."
  (reorg--create-symbol 'reorg--
			class
			'--display-
			name))

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
       (cl-pushnew (cons 'class (lambda (&optional _ __) ',name))
		   (alist-get ',name reorg--parser-list))
       (cl-pushnew (cons 'id (lambda (&optional _ __) #'org-id-new))
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

;;;;  requires

;; need to call the above macros before requiring
;; these files
;; TODO figure out if there is a way to avoid this
;; maybe `with-eval-when-compile'? 
(require 'reorg-org)
(require 'reorg-json)
(require 'reorg-files)
(require 'reorg-leo)
(require 'reorg-email)
(require 'reorg-elisp)
(require 'reorg-test)

;;; code 

;;;; user convenience functions

(defmacro reorg--create-string-comparison-funcs ()
  "Create string comparison functions that ignore case:
reorg-string<, reorg-string=, reorg-string>.  These functions
are convenience functions for writing templates." 
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
  ;; I am not sure why I put the data option here. I don't think
  ;; I used it. 
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

;; (defun reorg--walk-tree (tree func &optional data) ;
;;   "Apply func to each element of tree and return the results.
;; If DATA is provided, call FUNC with DATA as an argument.
;; Otherwise call FUNC with no arguments."
;;   ;; I am not sure why I put the data option here.
;;   (cl-labels
;;       ((walk
;; 	(tree)
;; 	(cl-loop for each in tree
;; 		 if (listp each)
;; 		 collect (walk each)
;; 		 else
;; 		 collect (funcall func each data))))
;;     (if (listp tree)
;; 	(walk tree)
;;       (funcall func tree data))))


;; (defun reorg--code-search (func code)
;;   "Return alist of symbols inside CODE that match REGEXP.
;; See `let-alist--deep-dot-search'."
;;   ;; It sure seems like this could be done with `reorg--walk-tree'
;;   ;; I was tired and it was easier to start over. 
;;   (let (acc)
;;     (cl-labels ((walk (code)
;; 		      (cond ((symbolp code)
;; 			     (when (funcall func code)
;; 			       (push code acc)))
;; 			    ((listp code)
;; 			     (walk (car code))
;; 			     (walk (cdr code) )))))
;;       (walk code)
;;       (cl-delete-duplicates acc))))

(defun reorg--get-all-tree-paths (tree leaf-func)
  "Get a list of all paths in tree.
LEAF-FUNC is a function run on each member to determine
whether it terminates the branch.

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
  ;; This approach has some limitations (I don't remember what)
  ;; but it works for now.
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
  ;; It's apparently tricky to calculate the length of a line that
  ;; includes :align-to display text props and includes fonts of a different
  ;; height.  There must be an easier way.
  ;; For now, balance the windows
  ;; (setf (window-width) 150))
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
  (when-let ((func (alist-get
		    (reorg--get-prop 'class)
		    reorg--render-func-list)))
    (funcall func)
    ;;FIXME this is redundant. see `reorg--goto-source'
    (when (reorg--buffer-in-side-window-p)
      (reorg--select-tree-window))))

(defun reorg--goto-source ()
  "Goto rendered source buffer."
  (interactive)
  (reorg--render-source)
  (when (reorg--buffer-in-side-window-p)
    (reorg--select-main-window)))

;;;; Box cursor line overlay 

;; TODO fix this remenant of a bad idea
;; so that the current header is highlighted
;; in a responsible manner 
(defvar reorg-edits--current-field-overlay
  (let ((overlay (make-overlay 1 2)))
    (overlay-put overlay 'face `( :box (:line-width -1)
				  :foreground ,(face-foreground 'default)))
    (overlay-put overlay 'priority 1000)
    overlay)
  "Overlay for field at point.")

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


(let ((point nil)) ;; I don't know why I did this.
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

;; (defvar reorg-edits--current-field-overlay
;;   (let ((overlay (make-overlay 1 2)))
;;     (overlay-put overlay 'face `( :box (:line-width -1)
;; 				  :foreground ,(face-foreground 'default)))
;;     (overlay-put overlay 'priority 1000)
;;     overlay)
;;   "Overlay for field at point.")

;;;; programatically interacting with tree buffer 

(defun reorg--get-prop (&optional property point)
  "Get PROPERTY from the current heading.  If PROPERTY
is omitted or nil, get the 'reorg-data' prop.  If it is
supplied, get that property from 'reorg-data'."
  (let ((props (get-text-property (or point (point)) 'reorg-data)))
    (if property
	(alist-get property props)
      props)))

(defun reorg--goto-next-prop (property &optional
				       value
				       limit
				       predicate
				       visible-only)
  "Assume we are getting 'reorg-data and PROPERTY is the key of that alist.
Does not run 'reorg--navigation-hooks'."
  ;; There must be a better way.
  (cond
   ((eobp)
    nil)
   ((if limit
	(> (point) limit)
      (= (point) (point-max)))
    nil)
   (t    
    (let ((origin (point))
	  (ended nil)
	  (limit (or limit (point-max)))
	  pos)
      (cl-loop with found = nil
	       while (not ended)
	       do (setq pos (next-single-property-change
			     (point)
			     'reorg-data nil limit))
	       if (or (not pos)		
		      (> pos limit))
	       return
	       (progn (reorg--goto-char origin)
		      (run-hooks 'reorg--navigation-hook)
		      (setq ended t)
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
			    (setq ended t)
			    (setq found t))
			(when (or (not pos)
				  (>= pos limit))
			  (goto-char origin)
			  (setq ended t))))
	       finally return (if (not found)
				  nil
				(point)))))))

(defun reorg--goto-previous-prop (property &optional value limit
					   predicate visible-only)
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
					  visible-only)
  "Save-excursion wrapper for `reorg--goto-previous-prop'."
  (save-excursion (reorg--goto-previous-prop
		   property
		   value
		   limit
		   predicate
		   visible-only)))

(defun reorg--get-next-prop (property &optional
				      value
				      limit
				      predicate
				      visible-only)
  "Save-excursion wrapper for `reorg--goto-previous-prop'."
  (save-excursion (reorg--goto-next-prop
		   property
		   value
		   limit
		   predicate
		   visible-only)))

(defun reorg--goto-char (point &optional no-hook)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (unless no-hook 
    (run-hooks 'reorg--navigation-hook))
  (point))

;;;; Tree buffer movement and commands

(defmacro reorg--create-navigation-commands (alist)
  "Create navigation commands. ALIST is a list in the form of (NAME . FORM)
where NAME is the name of what you are moving to, e.g., \"next-heading\"
and FORM is evaluated to see if that target exists.

This creates two functions: reorg--get-NAME and reorg--goto-NAME."
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
 ;; If you want to debug, just pp macro expand, and instrument
 ;; the defun in a scratch buffer. 
 ((first-leaf . (reorg--get-next-prop 'reorg-field-type
				      'leaf
				      (let ((sib (reorg--get-next-sibling))
					    (par (reorg--get-next-parent)))
					(if (and sib par)
					    (if (< sib par) sib par)
					  (if sib sib par)))))
  (next-leaf-sibling . (reorg--get-next-prop 'reorg-field-type
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
			       (lambda (a b) t)))
  
  ;; nil
  ;; nil
  ;; nil
  ;; (lambda (a b) t) t))
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
  (next-child . ;; (and
	      ;; (reorg--get-prop
	      ;;  'reorg-branch)
	      (reorg--get-next-prop
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

(defun reorg--leaves-visible-p ()
  "are the leaves of the heading visible?"
  (when-let ((p (reorg--get-first-leaf)))
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

;;;; insertion code

(defun reorg--goto-next-sibling-same-group (&optional data)
  "goot next sibing same group"
  (let ((id (or
	     (and data (alist-get 'group-id data))
	     (reorg--get-prop 'group-id))))
    (reorg--goto-next-prop 'group-id id)))

;; (defun reorg--goto-next-leaf-sibling ()
;;   "goto next sibling"
;;   (reorg--goto-next-prop 'reorg-field-type
;; 			   'leaf
;; 			   (reorg--get-next-parent)))

;; (defun reorg--goto-first-leaf ()
;;   "goto the first leaf of the current group"
;;   (reorg--goto-next-prop 'reorg-field-type
;; 			 'leaf
;; (let ((sib (reorg--get-next-sibling))
;; 	(par (reorg--get-next-parent)))
;;   (if (and sib par)
;; 	(if (< sib par) sib par)
;;     (if sib sib par)))))		

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

(defun reorg--find-header-location-within-groups (header-string)
  "assume the point is on the first header in the group"
  (let-alist (get-text-property 0 'reorg-data header-string)
    (if .sort-groups
	(cl-loop with point = (point)
		 if (equal .branch-name
			   (reorg--get-prop 'branch-name))
		 return (point)
		 else if (funcall .sort-groups
				  .branch-name
				  (reorg--get-prop 'branch-name))
		 return nil
		 while (reorg--goto-next-sibling-same-group
			(get-text-property 0 'reorg-data header-string))
		 finally return (progn (goto-char point)
				       nil))
      (cl-loop with point = (point)
	       when (equal .branch-name
			   (reorg--get-prop 'branch-name))
	       return t
	       while (reorg--goto-next-sibling-same-group
		      (get-text-property 0 'reorg-data header-string))
	       finally return (progn (goto-char point)
				     nil)))))

(defun reorg--find-first-header-group-member (header-data)
  "goto the first header that matches the group-id of header-data"
  (let ((point (point)))
    (if (equal (reorg--get-prop 'group-id)
	       (alist-get 'group-id header-data))
	(point)
      (if (reorg--goto-next-prop 'group-id
				 (alist-get 'group-id header-data)
				 (or (reorg--get-next-parent)
				     (point-max)))
	  (point)
	(goto-char point)
	nil))))

(defun reorg--find-leaf-location (leaf-string &optional result-sorters)
  "find the location for LEAF-DATA among the current leaves. put the
point where the leaf should be inserted (ie, insert before)"
  ;; goto the first leaf if at a branch 
  (unless (eq 'leaf (reorg--get-prop 'reorg-field-type))
    (if (reorg--goto-first-leaf)
	(when-let ((result-sorters
		    (or result-sorters
			(save-excursion 
			  (reorg--goto-parent)
			  (reorg--get-prop 'result-sorters))))) 
	  (let ((leaf-data (get-text-property 0 'reorg-data leaf-string)))
	    (cl-loop
	     with point = (point)
	     when (cl-loop for (func . pred) in result-sorters
			   unless (equal (funcall
					  `(lambda (x) (let-alist x ,func))
					  leaf-data)
					 (funcall
					  `(lambda (x) (let-alist x ,func))
					  (reorg--get-prop)))
			   return (funcall pred
					   (funcall
					    `(lambda (x) (let-alist x ,func))
					    leaf-data)
					   (funcall
					    `(lambda (x) (let-alist x ,func))
					    (reorg--get-prop))))
	     return (point)
	     while (reorg--goto-next-leaf-sibling)
	     finally (goto-char (line-beginning-position 2)))))
      (reorg--goto-next-heading))))

(defun reorg--get-next-group-id-change ()
  "get next group id change"
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


;; I am very wary of this function along with `reorg--render-maybe'.
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

(defun reorg-views--delete-leaf ()
  "delete the heading at point"
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

(defun reorg--delete-headers-maybe ()
  "Delete header, and its parents, at point if there
are no children.  Assume the point is at a branch." 
  (cl-loop with p = nil
	   if (reorg--get-next-child)
	   return t
	   else
	   do (setq p (reorg--get-parent))
	   do (reorg--delete-header-at-point)
	   if (null p)
	   return t
	   else do (goto-char p)))

(defun reorg--delete-dangling-headers ()
  "delete any unnecessary headers"
  (reorg--map-all-branches
   #'reorg--delete-headers-maybe))

(defun reorg--insert-all (data)
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
  (run-hooks 'reorg--navigation-hook))

(defun reorg--delete-entries (id)
  "Delete all heads that are associated with ID."
  (reorg--map-id id
		 (let ((parent (reorg--get-parent)))
		   (reorg-views--delete-leaf)
		   (when parent
		     (goto-char parent)
		     (reorg--delete-headers-maybe)))))

(defun reorg--insert-new-heading (data template)
  "Parse DATA according to TEMPLATE, delete old headers
that contained DATA, and insert the new header(s)
into the appropriate place(s) in the outline."
  ;; FIXME this is buggy. 
  (save-excursion 
    (goto-char (point-min))
    (cl-loop with header-groups = (reorg--get-all-tree-paths
				   (reorg--get-group-and-sort
				    (list data) template 1 t)
				   (lambda (x)
				     (and (listp x)
					  (stringp (car x))
					  (eq
					   'leaf
					   (get-text-property
					    0
					    'reorg-field-type
					    (car x))))))
	     for headers in header-groups
	     do (goto-char (point-min))
	     collect
	     (cl-loop
	      with leaf = (car (last headers))
	      with leaf-props = (get-text-property 0 'reorg-data leaf)
	      for header in (butlast headers)
	      when (eq 'leaf (alist-get 'reorg-field-type leaf-props))
	      do (let* ((header-props (get-text-property 0 'reorg-data header))
			(group-id (alist-get 'group-id header-props))
			(id (alist-get 'id header-props)))
		   (unless (or (reorg--goto-id header-props)
			       (equal id (reorg--get-prop 'id)))
		     (if (reorg--find-first-header-group-member header-props)
			 (unless (reorg--find-header-location-within-groups
				  header)
			   (reorg--insert-header-at-point header))
		       (reorg--insert-header-at-point header t))))
	      finally (progn (setq point (point))
			     (when (eq 'leaf (alist-get
					      'reorg-field-type
					      leaf-props))
			       (reorg--find-leaf-location leaf)
			       (reorg--insert-header-at-point leaf))
			     (goto-char point))))
    (org-indent-refresh-maybe (point-min) (point-max) nil))
  (run-hooks 'reorg--navigation-hook))

;;; core functions (fetching, parsing, grouping, sorting, formatting)

(defun reorg--getter (sources)
  "Get entries from SOURCES, whih is an alist
in the form of (CLASS . SOURCE)."
  (cl-loop for (class . source) in sources
	   append (funcall (car (alist-get class reorg--getter-list))
			   source)))

(defun reorg--parser (data class &optional type)
  "Call each parser in CLASS on DATA and return
the result.  If TYPE is provided, only run the
parser for that data type."
  (if type
      (cons type 
	    (funcall (alist-get
		      type
		      (alist-get class
				 reorg--parser-list))
		     data))
    (if (= 1 (length (alist-get class reorg--parser-list)))
	data
      (cl-loop with DATA = nil
	       for (type . func) in (alist-get class reorg--parser-list)
	       collect (cons type (funcall func data DATA)) into DATA
	       finally return DATA))))

(defun reorg--seq-group-by (form sequence)
  "Apply FORM to each element of SEQUENCE and group
the results.  See `seq-group-by'. Do not group
nil results. If FORM is a function, then call
the function with a single argument.  If FORM
is not a function, then create an anonymous function
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
		   ;; but requires using `eval'
		   (funcall `(lambda (e)
			       (let-alist e ,form))
			    elt)))
	    (cell (assoc key acc)))
       (if cell
	   (setcdr cell (push elt (cdr cell)))
	 (when key
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
	      unless (equal (funcall `(lambda (a) (let-alist a ,form)) a)
			    (funcall `(lambda (b) (let-alist b ,form)) b))
	      return (funcall pred
			      (funcall `(lambda (a) (let-alist a ,form)) a)
			      (funcall `(lambda (b) (let-alist b ,form)) b))))
   sequence))

(defun reorg--check-template-keys (template)
  "check keys"
  (when-let ((invalid-keys
	      (seq-difference 
	       (cl-loop for key in template by #'cddr
			collect key)
	       reorg--valid-template-keys)))
    (error "Invalid keys in entry in template: %s" invalid-keys)))

(defun reorg--get-group-and-sort (data
				  template
				  level
				  ignore-sources
				  &optional
				  inherited-props)
  "Apply TEMPLATE to DATA and apply the :action-function 
specified in the template or `reorg--grouper-action-function'
to the results."
  (reorg--check-template-keys template)
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
    (cl-labels ((get-header-metadata ;; sloppy
		 (header parent-id)
		 (list
		  (cons 'branch-name header)
		  (cons 'reorg-branch t)
		  (cons 'branch-type 'branch)
		  (cons 'sort-results sort-results)
		  (cons 'bullet bullet)
		  (cons 'folded-bullet folded-bullet)
		  (cons 'reorg-level level)
		  (cons 'id (org-id-new))
		  (cons 'parent-id parent-id)
		  (cons 'group-id group-id)))
		(drill!
		 (seq prop &optional n level inherited)
		 (setq level (or level 1))
		 (if-let ((groups (reorg--seq-group-by
				   `(lambda (x)
				      (eval ;; I know. 
				       (reorg--walk-tree
					(reorg--walk-tree
					 ',prop
					 (lambda (xx)
					   (reorg--turn-dot-bang-to-val xx ,(or n 0) x)))
					(lambda (xx)
					  (reorg--turn-dot-to-val xx x)))))
				   seq)))
		     (progn 
		       (when sort-groups
  			 (setq groups (seq-sort-by #'car
						   sort-groups
						   groups)))
		       (cl-loop
			for each in groups
			do (setq metadata
				 (get-header-metadata
				  (car each)
				  (plist-get inherited :id)))
			collect 
			(cons (reorg--create-headline-string
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
					:id 
					(alist-get 'id metadata)))))))
		   (if (plist-get template :children)
		       (cl-loop for child in (plist-get template :children)
				collect (reorg--get-group-and-sort
					 seq
					 child
					 ;; same test here re: did we increase the level
					 (1+ level)
					 ignore-sources
					 (list :header nil
					       :format-results format-results
					       :id (plist-get inherited :id)
					       :sort-results sort-results
					       :parent-template template
					       :bullet bullet
					       :folded-bullet folded-bullet
					       :face face)))
		     (when sort-results
		       (setq seq 
			     (reorg--multi-sort sort-results
						seq)))
		     (cl-loop for each in seq
			      collect 
			      (funcall
			       action-function
			       (append each
				       (list
					(cons 'group-id
					      (alist-get 'id metadata))
					(cons 'parent-id
					      (alist-get 'id metadata))))
			       format-results
			       level
			       (plist-get template :overrides)
			       (plist-get template :post-overrides)))))))
      ;; end drill 
      
      (when (and sources (not ignore-sources))
	(cl-loop for each in sources
		 do (push each reorg--current-sources))
	(setq data (append data (reorg--getter sources))))

      ;; begin .@
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
	(setq results
	      (pcase group 
		((pred functionp)
		 (reorg--seq-group-by group data))
		((pred stringp)
		 (list (cons group data)))
		((pred (not null))
		 (reorg--seq-group-by group
				      data))
		(_ nil)))
	;; no results
	(if (null results)
	    (cl-loop for child in (plist-get template :children)
		     collect (reorg--get-group-and-sort
			      data
			      child
			      level
			      ignore-sources
			      (list :header nil
				    :format-results format-results
				    :id nil
				    :sort-results sort-results
				    :parent-template template
				    :bullet bullet
				    :folded-bullet folded-bullet
				    :face face)))
	  ;; if results 
	  (when sort-groups
	    (setq results 
		  (cond ((functionp sort-groups)
			 (seq-sort-by #'car
				      sort-groups
				      results))
			(t (seq-sort-by #'car
					`(lambda (x)
					   (let-alist x
					     ,sort-groups))
					results)))))

	  ;; If there are results and children
	  (cond ((and (plist-get template :children)
		      results)
		 (cl-loop
		  for (header . children) in results
		  append
		  (cons
		   ;; create the header 
		   (funcall action-function
			    (setq metadata
				  (get-header-metadata
				   header (plist-get inherited-props :id)))

			    nil
			    level
			    (list 
			     (cons 'header header)
			     (cons 'bullet bullet)
			     (cons 'folded-bullet folded-bullet)
			     (cons 'reorg-face face)))
		   ;; there is metadata 
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
			      :bullet bullet
			      :folded-bullet folded-bullet
			      :face face))))))
		;; if there are only children 
		((plist-get template :children)
		 (debug nil "I don't think this is ever reached.")
		 (cl-loop for child in (plist-get template :children)
			  collect
			  (reorg--get-group-and-sort
			   data
			   child
			   level
			   ignore-sources
			   (progn
			     (setq metadata (get-header-metadata nil inherited-props))
			     (cl-loop for (key . val) in metadata
				      append (list (reorg--add-remove-colon key)
						   val))))))
		;; if there are normal results 
		(t 
		 (cl-loop for (header . children) in results
			  append
			  (cons				
			   (funcall
			    action-function
			    (setq metadata
				  (get-header-metadata
				   header
				   (plist-get inherited-props :id)))
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
			      (append result
				      (list 
				       (cons 'group-id
					     (alist-get 'id metadata))
				       (cons 'parent-id
					     (alist-get 'id metadata))))
			      format-results
			      (if (equal ""
					 (alist-get
					  'branch-name
					  metadata))
				  level
				(1+ level))
			      (plist-get template :overrides)
			      (plist-get template :post-overrides)))))))))))))

;; (defun reorg--get-group-and-sort (data
;; 				  template
;; 				  level
;; 				  ignore-sources
;; 				  &optional
;; 				  inherited-props)
;;   "Apply TEMPLATE to DATA and apply the :action-function 
;; specified in the template or `reorg--grouper-action-function'
;; to the results."
;;   (reorg--check-template-keys template)
;;   (let ((group-id (md5 (pp-to-string template)))
;; 	(format-results (or (plist-get template :format-results)
;; 			    (plist-get template :format-result)
;; 			    (plist-get inherited-props :format-results)))
;; 	(sort-results (or (append (plist-get inherited-props :sort-results)
;; 				  (plist-get template :sort-results))))
;; 	(sources (or (plist-get template :sources)
;; 		     (plist-get template :source)))
;; 	(action-function (or (plist-get inherited-props :action-function)
;; 			     reorg--grouper-action-function))
;; 	(bullet (when-let ((bullet (or (plist-get template :bullet)
;; 				       (plist-get template :bullets)
;; 				       (plist-get inherited-props :bullet))))
;; 		  (if (equal bullet "")
;; 		      (char-to-string 8203) ;; use a zero width space
;; 		    bullet)))
;; 	(folded-bullet (or (plist-get template :folded-bullet)
;; 			   (plist-get template :folded-bullets)
;; 			   (plist-get inherited-props :folded-bullet)))
;; 	(face (or (plist-get template :face)
;; 		  (plist-get inherited-props :face)
;; 		  reorg-default-face))
;; 	(group (or (plist-get template :group)
;; 		   (plist-get template :groups)))
;; 	(sort-groups (or (plist-get template :sort-groups)
;; 			 (plist-get template :sort-groups)))
;; 	results metadata)
;;     (cl-labels ((get-header-metadata
;; 		 (header inherited-props)
;; 		 (list
;; 		  (cons 'branch-name header)
;; 		  (cons 'reorg-branch t)
;; 		  (cons 'branch-type 'branch)
;; 		  (cons 'sort-results sort-results)
;; 		  (cons 'bullet bullet)
;; 		  (cons 'folded-bullet folded-bullet)
;; 		  (cons 'reorg-level level)
;; 		  (cons 'id (org-id-new))
;; 		  (cons 'parent-id (plist-get inherited-props :parent-id))
;; 		  (cons 'group-id group-id)))
;; 		;; (md5
;; 		;;  (concat 
;; 		;;   (pp-to-string
;; 		;;    (plist-get inherited-props
;; 		;; 		:parent-template))
;; 		;;   (pp-to-string
;; 		;;    (plist-get inherited-props
;; 		;; 		:header)))))))
;; 		(drill!
;; 		 (seq prop &optional n level inherited-props)
;; 		 (setq level (or level 1))
;; 		 (if-let ((groups (reorg--seq-group-by
;; 				   `(lambda (x)
;; 				      (eval ;; I know.
;; 				       (reorg--walk-tree
;; 					(reorg--walk-tree
;; 					 ',prop
;; 					 (lambda (xx)
;; 					   (reorg--turn-dot-bang-to-val xx ,(or n 0) x)))
;; 					(lambda (xx)
;; 					  (reorg--turn-dot-to-val xx x)))))
;; 				   seq)))
;; 		     (progn 
;; 		       (when sort-groups
;;   			 (setq groups (seq-sort-by #'car
;; 						   sort-groups
;; 						   groups)))
;; 		       (cl-loop
;; 			for each in groups
;; 			do (setq metadata
;; 				 (get-header-metadata
;; 				  (car each)
;; 				  (setq inherited-props
;; 					(list :header nil
;; 					      :format-results format-results
;; 					      :parent-id (alist-get 'id metadata)
;; 					      :sort-results sort-results
;; 					      :parent-template template
;; 					      :bullet bullet
;; 					      :folded-bullet folded-bullet
;; 					      :face face))))
;; 			collect 
;; 			;; save this string and check if it is
;; 			;; "" when deciding whether to increase
;; 			;; the level 
;; 			(cons (reorg--create-headline-string
;; 			       metadata
;; 			       nil
;; 			       level)
;; 			      (drill! (cdr each)
;; 				      prop						
;; 				      (1+ (or n 0))
;; 				      (1+ level)
;; 				      inherited-props))))
;; 		   (if (plist-get template :children)
;; 		       (cl-loop for child in (plist-get template :children)
;; 				collect (reorg--get-group-and-sort
;; 					 seq
;; 					 child
;; 					 ;; same test here re: did we increase the level
;; 					 (1+ level)
;; 					 ignore-sources
;; 					 (list :header nil
;; 					       :format-results format-results
;; 					       :parent-id (alist-get 'id inherited-props)
;; 					       :sort-results sort-results
;; 					       :parent-template template
;; 					       :bullet bullet
;; 					       :folded-bullet folded-bullet
;; 					       :face face)))
;; 		     (when sort-results
;; 		       (setq seq 
;; 			     (reorg--multi-sort sort-results
;; 						seq)))
;; 		     (cl-loop for each in seq
;; 			      collect 
;; 			      (funcall
;; 			       action-function
;; 			       (append each
;; 				       (list
;; 					(cons 'group-id
;; 					      (alist-get 'id metadata))
;; 					(cons 'parent-id
;; 					      (alist-get 'id metadata))))
;; 			       format-results
;; 			       level
;; 			       (plist-get template :overrides)
;; 			       (plist-get template :post-overrides)))))))
;;       ;; end drill 

;;       (when (and sources (not ignore-sources))
;; 	(cl-loop for each in sources
;; 		 do (push each reorg--current-sources))
;; 	(setq data (append data (reorg--getter sources))))

;;       ;; begin .@
;;       (when-let ((at-dots (seq-uniq 
;; 			   (reorg--dot-at-search
;; 			    group))))
;; 	(setq data (cl-loop
;; 		    for d in data 
;; 		    append
;; 		    (cl-loop
;; 		     for at-dot in at-dots
;; 		     if (listp (alist-get at-dot d))
;; 		     return
;; 		     (cl-loop for x in (alist-get at-dot d)
;; 			      collect
;; 			      (let ((ppp (copy-alist d)))
;; 				(setf (alist-get at-dot ppp) x)
;; 				ppp))
;; 		     finally return data)))
;; 	(setq group (reorg--walk-tree group
;; 				      #'reorg--turn-dot-at-to-dot
;; 				      data)))
;;       ;; end .@

;;       ;; begin .! 
;;       (if (reorg--dot-bang-search group)
;; 	  (drill! data group nil level inherited-props)
;; 	;;end .!
;; 	(setq results
;; 	      (pcase group 
;; 		((pred functionp)
;; 		 (reorg--seq-group-by group data))
;; 		((pred stringp)
;; 		 (list (cons group data)))
;; 		((pred (not null))
;; 		 ;; (when-let ((at-dots (seq-uniq 
;; 		 ;; 		      (reorg--dot-at-search
;; 		 ;; 		       group))))
;; 		 ;;   (setq data (cl-loop
;; 		 ;; 	       for d in data 
;; 		 ;; 	       append
;; 		 ;; 	       (cl-loop
;; 		 ;; 		for at-dot in at-dots
;; 		 ;; 		if (listp (alist-get at-dot d))
;; 		 ;; 		return
;; 		 ;; 		(cl-loop for x in (alist-get at-dot d)
;; 		 ;; 			 collect
;; 		 ;; 			 (let ((ppp (copy-alist d)))
;; 		 ;; 			   (setf (alist-get at-dot ppp) x)
;; 		 ;; 			   ppp))
;; 		 ;; 		finally return data))))
;; 		 (reorg--seq-group-by group
;; 				      data))))
;; 	;; no results
;; 	(if (null results)
;; 	    (cl-loop for child in (plist-get template :children)
;; 		     collect (reorg--get-group-and-sort
;; 			      data
;; 			      child
;; 			      level
;; 			      ignore-sources
;; 			      (list :header nil
;; 				    :format-results format-results
;; 				    :parent-id nil
;; 				    :sort-results sort-results
;; 				    :parent-template template
;; 				    :bullet bullet
;; 				    :folded-bullet folded-bullet
;; 				    :face face)))
;; 	  ;; if results 
;; 	  (when sort-groups
;; 	    (setq results 
;; 		  (cond ((functionp sort-groups)
;; 			 (seq-sort-by #'car
;; 				      sort-groups
;; 				      results))
;; 			(t (seq-sort-by #'car
;; 					`(lambda (x)
;; 					   (let-alist x
;; 					     ,sort-groups))
;; 					results)))))

;; 	  ;; If there are results and children
;; 	  (cond ((and (plist-get template :children)
;; 		      results)
;; 		 (cl-loop
;; 		  for (header . children) in results
;; 		  append
;; 		  (cons
;; 		   ;; create the header 
;; 		   (funcall action-function
;; 			    (setq metadata
;; 				  (get-header-metadata
;; 				   header
;; 				   (setq inherited-props
;; 					 (list 
;; 					  :header header
;; 					  :parent-template template
;; 					  :format-results format-results
;; 					  :sort-results sort-results
;; 					  :parent-id (alist-get 'id metadata)
;; 					  :bullet bullet
;; 					  :folded-bullet folded-bullet
;; 					  :face face))))
;; 			    nil
;; 			    level
;; 			    (list 
;; 			     (cons 'header header)
;; 			     (cons 'bullet bullet)
;; 			     (cons 'folded-bullet folded-bullet)
;; 			     (cons 'reorg-face face)))
;; 		   ;; there is metadata 
;; 		   (cl-loop for child in (plist-get template :children)
;; 			    collect 
;; 			    (reorg--get-group-and-sort			  
;; 			     children
;; 			     child
;; 			     (if (equal "​"
;; 					(alist-get
;; 					 'branch-name
;; 					 metadata))
;; 				 level
;; 			       (1+ level))
;; 			     ;;			     (1+ level)
;; 			     ignore-sources
;; 			     (list 
;; 			      :header header
;; 			      :parent-template template
;; 			      :format-results format-results
;; 			      :sort-results sort-results
;; 			      :parent-id (alist-get 'id metadata)
;; 			      :bullet bullet
;; 			      :folded-bullet folded-bullet
;; 			      :face face))))))
;; 		;; if there are only children 
;; 		((plist-get template :children)
;; 		 (cl-loop for child in (plist-get template :children)
;; 			  collect
;; 			  (reorg--get-group-and-sort
;; 			   data
;; 			   child
;; 			   level
;; 			   ignore-sources
;; 			   (progn
;; 			     (setq metadata (get-header-metadata nil inherited-props))
;; 			     (cl-loop for (key . val) in metadata
;; 				      append (list (reorg--add-remove-colon key)
;; 						   val))))))
;; 		;; if there are normal results 
;; 		(t 
;; 		 (cl-loop for (header . children) in results
;; 			  append
;; 			  (cons				
;; 			   (funcall
;; 			    action-function
;; 			    (setq metadata
;; 				  (get-header-metadata header inherited-props))
;; 			    nil
;; 			    level
;; 			    (plist-get template :overrides)
;; 			    (plist-get template :post-overrides))
;; 			   (list 
;; 			    (cl-loop
;; 			     with
;; 			     children = 
;; 			     (if sort-results
;; 				 (reorg--multi-sort sort-results
;; 						    children)
;; 			       children)
;; 			     for result in children
;; 			     collect
;; 			     (funcall
;; 			      action-function
;; 			      (append result
;; 				      (list 
;; 				       (cons 'group-id
;; 					     (alist-get 'id metadata))
;; 				       (cons 'parent-id
;; 					     (alist-get 'id metadata))))
;; 			      format-results
;; 			      (if (equal ""
;; 					 (alist-get
;; 					  'branch-name
;; 					  metadata))
;; 				  level
;; 				(1+ level))
;; 			      (plist-get template :overrides)
;; 			      (plist-get template :post-overrides)))))))))))))

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
;; TODO consolidate these two functions
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

(defun reorg--turn-dot-bang-to-val (elem n d)
  "turn .@symbol into .symbol."
  (if-let ((sym (and (symbolp elem)
		     (string-match "\\`\\.!" (symbol-name elem))
		     (intern (substring (symbol-name elem) 2)))))
      (nth n (alist-get sym d))
    elem))

(defun reorg--turn-dot-to-val (elem d)
  "turn .@symbol into .symbol."
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

(defun reorg--get-all-sources-from-template (template)
  "Walk the template tree and make a list of all unique template
sources.  This is used for updating the reorg tree, e.g., as part
of an org-capture hook to make sure the captured entry belongs to
one of the sources."
  (cl-labels ((get-sources
	       (template)
	       (append (cl-loop for each in template
				when (plist-get each :sources)
				append (plist-get each :sources)
				append (get-sources (plist-get template
							       :children)))
		       (plist-get template :sources))))
    (seq-uniq (get-sources template))))

(defun reorg--list-modules ()
  "Let the modules available."
  (cl-loop for (module . parsers) in reorg--parser-list
	   collect module))

(defun reorg-list-data-types ()
  "List data types for a given module"
  (let ((module
	 (completing-read "Select module: " (reorg--list-modules))))
    (cl-loop for (name . func) in (alist-get (intern module)
					     reorg--parser-list)
	     collect name into results
	     return results)))

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
  ;; This is probably a terribly inefficient capf
  ;; but it does not need to be fast. 
  (let (start end collection props)
    (when (looking-back "\\.[.|[:word:]]+")
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
     (reorg--get-group-and-sort data template 1 nil))
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

(defvar reorg-main-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'reorg--goto-source)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "<left>") #'reorg--goto-parent)
    (define-key map (kbd "g") #'reorg-org--update-heading-at-point)
    (define-key map (kbd "G") (lambda () (interactive)
				(reorg--close-tree-buffer)
				(kill-buffer reorg-buffer-name)
				(save-excursion (reorg-open-main-window
						 reorg--current-template))))
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
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer)
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "keymap")

(define-derived-mode reorg-mode
  fundamental-mode
  "Reorg"
  "Reorganize your life."
  :group 'reorg
  (setq cursor-type nil)
  (use-local-map reorg-main-mode-map)
  ;; (if (fboundp #'org-visual-indent-mode)
  ;;     (org-visual-indent-mode)
  ;; (org-indent-mode)
  (toggle-truncate-lines 1)
  (setq-local cursor-type nil)
  ;; (reorg--map-all-branches #'reorg--delete-headers-maybe)  
  (add-hook 'reorg--navigation-hook #'org-show-context nil t)  
  (add-hook 'reorg--navigation-hook #'reorg-edits--update-box-overlay nil t)
  (add-hook 'reorg--navigation-hook #'reorg--render-maybe nil t)
  (global-set-key (kbd reorg-toggle-shortcut) #'reorg--toggle-tree-buffer)
  (reorg--goto-char 1))

(add-hook 'reorg-mode-hook #'reorg-bullets-mode)
(add-hook 'reorg-mode-hook #'org-visual-indent-mode)


(provide 'reorg)


