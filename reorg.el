;; -*- lexical-binding: t; -*-

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
;;(defconst reorg--id-property-name 'reorg-id)

(defconst reorg--field-property-name 'reorg-field-type)

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

;;; constants

(defconst reorg--valid-template-keys '(:sources
				       :group
				       :children
				       :overrides
				       :post-overrides
				       :sort-results
				       :bullet
				       :format-results
				       :sort-groups)
  "Allowable template keys.")

;;; reorg requires 
(require 'reorg-data)
(require 'reorg-dynamic-bullets)
(require 'reorg-scratch)

;;; reorg data types 
(require 'reorg-org)
(require 'reorg-files)
(require 'reorg-leo)
(require 'reorg-email)

;;; testing requires
(require 'reorg-test)


;;; window control

(defun reorg--open-side-window ()
  "Open a side window to display the tree."
  (display-buffer-in-side-window (get-buffer-create reorg-buffer-name)
				 `((side . ,reorg-buffer-side)
				   (dedicated . t)
				   (slot . nil)
				   (window-parameters . ((reorg . t)))))
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
		    (reorg--get-view-prop 'class)
		    reorg--render-func-list)))
    (funcall func)
    (when (reorg--buffer-in-side-window-p)
      (reorg--select-tree-window))))

(defun reorg--goto-source ()
  "Goto rendered source buffer."
  (interactive)
  (reorg--render-source)
  (when (reorg--buffer-in-side-window-p)
    (reorg--select-main-window)))

;;; main

(defun reorg--insert-all (data)
  "Insert grouped and sorted data into outline."
  (let (results)
    (cl-labels ((recurse (data)
			 (cond ((stringp data)
				(insert data))
			       (data (cl-loop for entry in data
					      do (recurse entry))))))
      (recurse data))))

(defun reorg--get-all-sources-from-template (template)
  "Walk the template tree and make a list of all unique template
sources.  This is used for updating the reorg tree, e.g., as part
of an org-capture hook to make sure the captured entry belongs to
one of the sources."
  (cl-labels ((get-sources (template)
			   (append (cl-loop for each in template
					    when (plist-get each :sources)
					    append (plist-get each :sources)
					    append (get-sources (plist-get template :children)))
				   (plist-get template :sources))))
    (-uniq (get-sources template))))


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
  ;; (reorg--select-tree-window)
  ;; (set-window-buffer nil reorg-buffer-name)
  (reorg--open-side-window)
  (reorg--select-tree-window))

(defun reorg-open (template &optional point)
  "Open this shit in the sidebar."
  (interactive)
  (when (get-buffer reorg-buffer-name)
    (kill-buffer reorg-buffer-name))
  (with-current-buffer (get-buffer-create reorg-buffer-name)
    (erase-buffer)
    (reorg--insert-all
     (reorg--get-group-and-sort nil template 1 nil))
    (setq reorg--current-sources
	  (reorg--get-all-sources-from-template template)
	  reorg--current-template
	  template)
    (reorg-mode)
    (goto-char (or point (point-min)))
    (run-hooks 'reorg--navigation-hook)))




;;;; view buffer functions

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



;;;; NEW WINDOW SELECTOR

(defun reorg--buffer-in-side-window-p ()
  "Is the reorg buffer in a side window?"
  (--first 
   (window-parameter it 'reorg)
   (window-at-side-list nil reorg-buffer-side)))

(defun reorg--select-window-run-func-maybe (window &optional func switch-back)
  "WINDOW is either 'main or 'tree. FUNC is a function with no args."
  (when-let ((win
	      (seq-find
	       (lambda (x)
		 (window-parameter x 'reorg))
	       (window-at-side-list nil reorg-buffer-side))))
    (pcase window
      ('tree (progn (reorg--select-tree-window)
		    (funcall func)
		    (when switch-back
		      (reorg--select-main-window))))
      ('main (progn (funcall func)
		    (when switch-back
		      (reorg--select-tree-window)))))))

(defun reorg--render-maybe ()
  "maybe render if we are in a tree window."
  (reorg--select-window-run-func-maybe 'main #'reorg--render-source t))

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

;;;; updating the tree

(defun reorg-reload ()
  "reload the current template"
  (interactive)
  (if (reorg--buffer-in-side-window-p)
      (reorg-open-sidebar)
    (reorg-open-in-current-window)))

(defun reorg--buffer-p ()
  "Are you in the reorg buffer?"
  (string= (buffer-name)
	   reorg-buffer-name))

(defvar reorg-main-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'reorg--goto-source)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "<left>") #'reorg--goto-parent)
    (define-key map (kbd "g") #'reorg--update-heading-at-point)
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

(defun reorg--close-tree-buffer ()
  "Close the tree buffer."
  (interactive)
  (let* ((window (seq-find
		  (lambda (x)
		    (window-parameter x 'reorg))
		  (window-at-side-list nil reorg-buffer-side)))
	 (buffer (window-buffer window)))
    (mapc #'delete-window (seq-filter (lambda (x) (window-parameter x 'reorg))
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

(define-derived-mode reorg-mode
  fundamental-mode
  "Reorg"
  "Reorganize your life. \{keymap}"
  (setq cursor-type nil)
  (use-local-map reorg-main-mode-map)
  (reorg-dynamic-bullets-mode)
  (if (fboundp #'org-visual-indent-mode)
      (org-visual-indent-mode)
    (org-indent-mode))
  (toggle-truncate-lines 1)
  (setq-local cursor-type nil)
  ;; (reorg--map-all-branches #'reorg--delete-headers-maybe)  
  (add-hook 'reorg--navigation-hook #'org-show-context nil t)  
  (add-hook 'reorg--navigation-hook #'reorg-edits--update-box-overlay nil t)
  (add-hook 'reorg--navigation-hook #'reorg--render-maybe nil t)
  (global-set-key (kbd reorg-toggle-shortcut) #'reorg--toggle-tree-buffer)
  (goto-char (point-min))
  (run-hooks 'reorg--navigation-hook))

(defvar reorg-edits--current-field-overlay
  (let ((overlay (make-overlay 1 2)))
    (overlay-put overlay 'face `( :box (:line-width -1)
				  :foreground ,(face-foreground 'default)))
    (overlay-put overlay 'priority 1000)
    overlay)
  "Overlay for field at point.")

;;;; field navigation 

(defun reorg--unfold-at-point (&optional point)
  "Unfold so the heading at point is visible."
  (save-excursion 
    (reorg--goto-parent)
    (outline-show-subtree)
    (goto-char point)
    (outline-show-subtree)
    (goto-char point)))

(let ((point nil))
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

;;;; utilities

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



(defun reorg--add-remove-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix.
Returns PROP with a colon prefix. If REMOVE is t,
then return PROP with no colon prefix."
  (pcase `(,remove ,(keywordp prop))
    (`(t t) (intern (substring (symbol-name prop) 1)))
    (`(nil nil) (intern (concat ":" (symbol-name prop))))
    (_ prop)))

(defun reorg-edits--get-field-bounds ()
  "Get the bounds of the field at point."
  (let ((match (save-excursion (text-property--find-end-forward
				(point)
				'reorg-data
				(reorg--get-view-prop)
				#'equal))))
    (cons
     (prop-match-beginning match)
     (prop-match-end match))))

;;;; finding functions

(defun reorg--get-view-prop (&optional property point)
  "Get PROPERTY from the current heading.  If PROPERTY
is omitted or nil, get the 'reorg-data' prop.  If it is
supplied, get that property from 'reorg-data'."
  (let ((props (get-text-property (or point (point)) 'reorg-data)))
    (if property
	(alist-get property props)
      props)))

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
	(reorg--get-view-prop nil (or point (point)))))))

(defun reorg--goto-next-prop (property &optional value limit
				       predicate visible-only)
  "Initially based on 'text-property-search-forward'
but inclde a LIMIT parameter.  Now, assume we are getting 'reorg-data
and PROPERTY is the key of that alist.
DOES NOT RUN 'reorg--navigation-hooks'." 
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
	       do (setq pos (next-single-property-change (point) 'reorg-data nil limit))
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
						       (get-text-property (point) 'reorg-data))
					  (get-text-property (point) 'reorg-data))))
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
  "See 'reorg--goto-next-prop'"
  (cond
   ((bobp)
    nil)
   ((< (point) (or limit (point-min)))
    nil)
   (t    
    (let ((origin (point))
          (ended nil)
	  (limit (or limit (point-min)))
          pos)
      (cl-loop with found = nil
	       with pos = nil 
	       while (not ended)
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
		      (setq ended t)
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
			    (setq ended t)
			    (setq found t))
			(when (or (not pos)
				  (bobp)
				  (<= pos limit))
			  (goto-char origin)
			  (setq ended t))))
	       finally return (if (not found)
				  nil
				(point)))))))

(defun reorg--get-previous-prop (property &optional
					  value
					  limit
					  predicate
					  visible-only)
  "Return the point instead of moving it."
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
  "get next instead of moving it."
  (save-excursion (reorg--goto-next-prop
		   property
		   value
		   limit
		   predicate
		   visible-only)))

(defun reorg--goto-char (point)
  "Goto POINT and run hook funcs."
  (goto-char point)
  (run-hooks 'reorg--navigation-hook)
  (point))

;;; Navigation commands 

(defmacro reorg--create-navigation-commands (alist)
  "Create navigation commands. ALIST is a list in the form of (NAME . FORM)
where NAME is the name of what you are moving to, e.g., \"next-heading\"
and FORM is evaluated to see if that target exists.

This creates two functions: reorg--get-NAME and reorg--goto-NAME."
  `(progn 
     ,@(cl-loop for (name . form) in alist
		append (list `(defun ,(reorg--create-symbol 'reorg--goto- name) (&optional no-update)
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
 ((next-heading . (reorg--get-next-prop nil nil nil (lambda (a b) t)))
  (next-visible-heading . (reorg--get-next-prop nil nil nil (lambda (a b) t) t))
  (previous-heading . (reorg--get-previous-prop nil nil nil (lambda (a b) t)))
  (next-branch . (reorg--get-next-prop 'reorg-branch t nil nil nil))
  (next-visible-branch . (reorg--get-next-prop 'reorg-branch t nil nil t))
  (previous-visible-heading . (reorg--get-previous-prop nil nil nil (lambda (a b) t) t))
  (next-sibling . (reorg--get-next-prop
		   'reorg-level
		   (reorg--get-view-prop 'reorg-level)
		   (reorg--get-next-prop 'reorg-level
					 (reorg--get-view-prop 'reorg-level)
					 nil
					 (lambda (a b)
					   (< b a)))))
  (previous-sibling . (reorg--get-previous-prop
		       'reorg-level
		       (reorg--get-view-prop 'reorg-level)
		       (reorg--get-previous-prop
			'reorg-level
			(reorg--get-view-prop 'reorg-level)
			nil
			(lambda (a b) (< b a)))))
  (next-clone . (reorg--get-next-prop 'id
				      (reorg--get-view-prop 'id)))
  (previous-clone . (reorg--get-previous-prop 'id
					      (reorg--get-view-prop 'id)))
  (next-parent . (reorg--get-next-prop 'reorg-level
				       (reorg--get-view-prop 'reorg-level)
				       nil
				       (lambda (a b) (> a b))))
  (parent . (reorg--get-previous-prop 'reorg-level
				      (1- (reorg--get-view-prop 'reorg-level))))
  (root . (and (/= 1 (reorg--get-view-prop 'reorg-level))
	       (reorg--get-previous-prop 'reorg-level 1)))
  (next-child . (and
		 (reorg--get-view-prop 'reorg-branch)
		 (reorg--get-next-prop 'reorg-level
				       (1+ (reorg--get-view-prop 'reorg-level))
				       (reorg--get-next-prop 'reorg-level
							     (reorg--get-view-prop 'reorg-level)
							     nil
							     (lambda (a b)
							       (>= a b))))))
  (next-visible-child
   .
   (and
    (reorg--get-view-prop 'reorg-branch)
    (reorg--get-next-prop 'reorg-level
			  (1+ (reorg--get-view-prop 'reorg-level))
			  (reorg--get-next-prop 'reorg-level
						(reorg--get-view-prop 'reorg-level)
						nil
						(lambda (a b)
						  (>= a b)))
			  nil
			  t)))))



(provide 'reorg)
