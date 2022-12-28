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

;;; reorg requires 
(require 'reorg-data)
(require 'reorg-utils)
(require 'reorg-find)
(require 'reorg-sort)
(require 'reorg-dynamic-bullets)
(require 'reorg-scratch)

;;; reorg data types 
(require 'reorg-org)
(require 'reorg-files)
(require 'reorg-leo)
(require 'reorg-email)

;;; testing requires
(require 'reorg-test)

;;; constants

(defconst reorg--data-property-name 'reorg-data)
;;(defconst reorg--id-property-name 'reorg-id)

(defconst reorg--field-property-name 'reorg-field-type)

;;; customs

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
  #'reorg--create-headline-string*
  "")

(defvar-local reorg--current-template nil
  "the current template in this buffer")

(defvar-local reorg--current-sources nil
  "the current template in this buffer")

(defvar reorg--navigation-hook nil
  "Post-navigation hook.")

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
  (select-window
   (--first 
    (window-parameter it 'reorg)
    (window-at-side-list nil reorg-buffer-side))))

;;; main

(defun reorg--insert-org-headlines (data)
  "Insert grouped and sorted data into outline."
  (let (results)
    (cl-labels ((recurse (data)
			 (cond ((stringp data)
				(insert data))
			       (data (cl-loop for entry in data
					      do (recurse entry))))))
      (recurse data))))

(cl-defun reorg-open-main-window (template)
  "Open this shit in the sidebar."
  (interactive)
  (with-current-buffer (get-buffer-create reorg-buffer-name)
    (erase-buffer)
    (reorg--insert-org-headlines
     (reorg--get-group-and-sort* nil template 1))
    (reorg-main-mode)
    (reorg-dynamic-bullets-mode)
    (org-visual-indent-mode)
    (toggle-truncate-lines 1)
    (setq reorg--current-template template)
    (setq-local cursor-type nil)
    ;; (reorg--map-all-branches #'reorg--delete-headers-maybe*)
    (goto-char (point-min))
    (run-hooks 'reorg--navigation-hook)
    (set-window-buffer nil reorg-buffer-name)))

(cl-defun reorg-open-sidebar (template)
  "Open this shit in the sidebar."
  (interactive)
  (when (get-buffer reorg-buffer-name)
    (kill-buffer reorg-buffer-name))
  (reorg--open-side-window)
  (reorg--select-tree-window)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (reorg--insert-org-headlines
   (reorg--get-group-and-sort* nil template 1))
  (reorg-view-mode)
  (reorg-dynamic-bullets-mode)
  (org-visual-indent-mode)
  (toggle-truncate-lines 1)
  (setq reorg--current-template template)
  (setq-local cursor-type nil)
  (goto-char (point-min))
  (run-hooks 'reorg--navigation-hook))
	     

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

(defun reorg--move-to-previous-entry-no-render ()
  "move to previous entry"
  (interactive)
  (reorg--goto-previous-visible-heading)
  (run-hooks 'reorg--navigation-hook))

(defun reorg--move-to-next-entry-no-render ()
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

(defun reorg--update-this-heading-all ()
  "Update heading at point and all clones."
  (let ((data 
	 (reorg--with-point-at-orig-entry (reorg--get-view-prop 'id)
					  (reorg--get-view-prop 'buffer)
					  (reorg--parser nil 'org)))
	(id (reorg--get-view-prop 'id)))
    (reorg--select-tree-window)
    (save-restriction
      (save-excursion    
	(reorg--map-id id
		       (reorg-view--update-view-headline)
		       (reorg-dynamic-bullets--fontify-heading))))))

(defvar reorg-main-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [remap undo] #'org-agenda-undo)
    (define-key map (kbd "RET") #'reorg--goto-source)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "<left>") #'reorg--goto-parent)
    (define-key map (kbd "g") #'reorg--update-this-heading)
    (define-key map (kbd "G") (lambda () (interactive)
				(save-excursion (reorg-open-main-window
						 reorg--current-template))))
    (define-key map (kbd "c") #'reorg--goto-next-clone)
    (define-key map (kbd "f") #'reorg--goto-next-sibling)
    (define-key map (kbd "b") #'reorg--goto-previous-sibling)
    (define-key map (kbd "C") #'reorg--goto-previous-clone)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map [remap undo] #'org-agenda-undo)
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "n") #'reorg--move-to-next-entry-no-render)
    (define-key map (kbd "<down>") #'reorg--move-to-next-entry-no-render)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry-no-render)
    (define-key map (kbd "<up>") #'reorg--move-to-previous-entry-no-render)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle)
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "keymap")

(defvar reorg-view-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [remap undo] #'org-agenda-undo)
    (define-key map (kbd "RET") #'reorg--goto-source)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "g") #'reorg--update-this-heading)
    (define-key map (kbd "G") (lambda () (interactive)
				(save-excursion (reorg-open-main-window
						 reorg--current-template))))
    (define-key map (kbd "c") #'reorg--goto-next-clone)
    (define-key map (kbd "f") #'reorg--goto-next-sibling)
    (define-key map (kbd "b") #'reorg--goto-previous-sibling)
    (define-key map (kbd "C") #'reorg--goto-previous-clone)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map (kbd "q") #'reorg--close-tree-buffer)
    (define-key map (kbd "n") #'reorg--move-to-next-entry-no-follow)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry-no-follow)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle)
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "keymap")

(defun reorg--close-tree-buffer ()
  "Close the tree buffer."
  (interactive)
  (let* ((window (--first 
		  (window-parameter it 'reorg)
		  (window-at-side-list nil reorg-buffer-side)))
	 (buffer (window-buffer window)))
    (mapc #'delete-window (--select (window-parameter it 'reorg)
                                    (window-at-side-list nil reorg-buffer-side)))))

(defun reorg--toggle-tree-buffer ()
  "toggle tree buffer"
  (interactive)
  (if (--first 
       (window-parameter it 'reorg)
       (window-at-side-list nil reorg-buffer-side))
      (reorg--close-tree-buffer)
    (reorg--open-side-window)))

  (define-derived-mode reorg-main-mode
    fundamental-mode
    "Agenda style"
    "Tree view of an Orgmode file. \{keymap}"
    (setq cursor-type nil)
    (use-local-map reorg-main-mode-map)
    (add-hook 'reorg--navigation-hook #'org-show-context nil t)  
    (add-hook 'reorg--navigation-hook #'reorg-edits--update-box-overlay nil t))

(define-derived-mode reorg-view-mode
  fundamental-mode
  "Org tree view"
  "Tree view of an Orgmode file. \{keymap}"
  (setq cursor-type nil)
  (use-local-map reorg-view-mode-map)
  (add-hook 'reorg--navigation-hook #'org-show-context nil t)
  (add-hook 'reorg--navigation-hook #'reorg-edits--update-box-overlay nil t))

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

(defun reorg-edits--get-field-at-point (&optional point)
  "Get the `reorg--field-property-name' at point."
  (get-text-property (or point (point)) 'reorg-data))

(defun reorg-views--insert-before-point (data &optional level format-string)
  "insert a hearing before the heading at point."
  (reorg--with-restore-state
   (beginning-of-line)
   (let ((string (reorg--create-headline-string data
						(or format-string reorg-headline-format)
						(or level (reorg--get-view-prop 'reorg-level)))))
     (insert string)
     (goto-char (point-at-bol))
     (reorg-dynamic-bullets--fontify-heading))))

(defun reorg-views--insert-after-point (data &optional level format-string)
  "insert a heading after the current point."
  (reorg--with-restore-state
   (end-of-line)
   (insert "\n")
   (let ((string (reorg--create-headline-string data
						(or format-string reorg-headline-format)
						(or level (reorg--get-view-prop 'reorg-level)))))
     (insert string)
     (reorg-dynamic-bullets--fontify-heading)
     (1+ (length string)))))

(defun reorg-views--delete-leaf ()
  "delete the heading at point"
  (delete-region (point-at-bol)
		 (line-beginning-position 2)))

;; (defun reorg-views--delete-headers-maybe () ;; SUSPECT
;;   "VERY SUSPECT"
;;   (save-excursion 
;;     (cl-loop while (and (reorg-tree--goto-next-property-field 'reorg-field-type 'branch t)
;; 			(not (reorg--get-next-level-branches))
;; 			(not (reorg-tree--branch-has-leaves-p)))
;; 	     do (reorg-views--delete-leaf))))

(defun reorg-view--update-view-headline ()
  "Goto source buffer, re-parse, update. WORKS"
  (let ((inhibit-modification-hooks t)
	(props (reorg--with-point-at-orig-entry nil nil
						(reorg--parser nil 'org)))
	(level (reorg--get-view-prop 'reorg-level))
	(format (save-excursion
		  (cl-loop while (org-up-heading-safe)
			   when (reorg--get-view-prop 'format-string)
			   return (reorg--get-view-prop 'format-string)
			   finally return reorg-headline-format))))
    (reorg-views--delete-leaf)
    (reorg--insert-heading props level format)))

;;;; inserting headers into the appropriate location

;;;;; reorg-map-branches

;; (defmacro reorg--map-all-branches (&rest body)
;;   "Move to the next clone of the current node."
;;   `(save-restriction 
;;      (save-excursion
;;        (goto-char (point-min))
;;        (while
;; 	   (text-property-search-backward 'reorg-field-type
;; 					  'branch
;; 					  nil
;; 					  'not-current)
;; 	 ,@body))))

(defmacro reorg--map-id (id &rest body)
  "Execute BODY at each entry that matches ID."
  `(org-with-wide-buffer 
    (goto-char (point-min))
    (let ((id ,id))
      (while (reorg--goto-next-prop 'id id)
	,@body))))

(defmacro reorg--map-next-level (&rest body)
  "Go to the next branch. With integer RELATIVE-LEVEL, go to the next
level relative to the current one.  For example, if the current outline
level is 2, and RELATIVE-LEVEL is -1, the function will move to the next
branch at level 1.  If RELATIVE-LEVEL is 1, the function will move to the
next branch at level 3.

If PREVIOUS is non-nil, move to the previous branch instead of the next.

Return nil if there is no such branch."
  `(let ((start-level (reorg--get-view-prop 'reorg-level))
	 (point (point)))
     (cl-loop while (reorg--goto-next-relative-level 1 nil start-level)
	      if (= (1+ start-level) (reorg--get-view-prop 'reorg-level))
	      do ,@body
	      else if (/= start-level (reorg--get-view-prop 'reorg-level))
	      return nil)
     (goto-char point)))

;;; actions 

(defmacro reorg-tree--with-wide-buffer (&rest body)
  "Execute BODY and restore the buffer to its previous state."
  `(save-restriction
     (save-excursion
       (let ((disable-point-adjustment t))
	 ,@body))))

(defun reorg-tree--contract-node ()
  (outline-hide-subtree))

(defun reorg-tree--expand-node ()
  (outline-show-subtree))

(defun reorg--org-shortcut-deadline (arg)
  "Execute org-deadline in the source buffer and update the heading at point."
  (interactive "P")
  (reorg--with-source-and-sync
    (if
	(eq 'org-set-property 'org-deadline)
	(funcall-interactively #'org-deadline arg nil)
      (funcall-interactively #'org-deadline arg))))


;;;; reorg-tree

(defun reorg--goto-next-relative-level (&optional relative-level backward start-level no-error)
  "Goto the next branch that is at RELATIVE-LEVEL up to any branch that is a
lower level than the current branch."
  ;; Outline levels start at 1, so make sure the destination is not out of bounds. 
  (let* ((start-level (or start-level (reorg--get-view-prop 'reorg-level)))
	 (point (point)))
    (cond  ((>= 0 (abs (+ (reorg--get-view-prop 'reorg-level) (or relative-level 0))))
	    (if no-error nil
	      (error "Cannot move to relative-level %d from current level %d"
		     relative-level
		     start-level)))
	   (t
	    (cl-flet ((looking-for (thing)
				   (eq thing 
				       (pcase (or relative-level 0)
					 ((pred (< 0)) 'descendant)
					 ((pred (> 0)) 'ancestor)
					 ((pred (= 0)) 'sibling))))
		      (current-level () (reorg--get-view-prop 'reorg-level))
		      (exit () (progn (setf (point) point) nil))
		      (goto-next () (reorg-tree--goto-next-property-field
				     'reorg-field-type
				     'branch
				     backward)))
	      
	      (cl-loop while (and (goto-next)
				  (if backward (not (bobp)) (not (eobp))))
		       if (if backward
			      (or (and (looking-for 'descendant)
				       (<= (current-level) start-level))
				  (and (looking-for 'sibling)
				       (< (current-level) start-level)))			    
			    (or (and (looking-for 'descendant)
				     (= (current-level) start-level))
				(and (looking-for 'sibling)
				     (< (current-level) start-level))))
		       return (exit)
		       else if (= (current-level)
				  (abs (+ start-level (or relative-level 0))))
		       return point
		       finally (exit)))))))

(defun reorg-into--need-to-make-new-branch? (data &optional point)
  "asdf"
  (let* ((children (reorg-into--get-list-of-child-branches-at-point)))
    (cl-loop with new-result = nil
	     with return = nil
	     for (func . results) in children
	     do (setq new-result (funcall func data))
	     if (and new-result
		     (member new-result results))
	     do (push (cons func new-result) return)
	     else do (push (cons func nil) return)
	     finally return (reverse return))))

(defun reorg-into--get-list-of-sibling-branches-at-point ()
  "Get a list of cons cells in the form (FUNCTION . RESULTS)."
  (save-excursion
    (let ((level (reorg--get-view-prop 'reorg-level))
	  (disable-point-adjustment t))
      (while (reorg--goto-next-relative-level 0 t))
      (cl-loop with alist = nil
	       with current-func = nil
	       do (setq current-func (reorg--get-view-props nil 'reorg-data 'grouper-list))
	       and do (setf (alist-get current-func
				       alist nil nil #'equal)			      
			    (append
			     (alist-get current-func
					alist nil nil #'equal)
			     (-list 
			      (reorg--get-view-props nil 'reorg-data 'grouper-list-results))))
	       while (reorg--goto-next-relative-level 0)
	       finally return alist))))

(defun reorg-into--get-list-of-child-branches-at-point ()
  "Get a list of cons cells in the form (FUNCTION . RESULTS)."
  (save-excursion
    (let ((level (reorg--get-view-prop 'reorg-level))
	  (disable-point-adjustment t))
      (when (reorg--goto-next-relative-level 1)
	(cl-loop with alist = nil
		 with current-func = nil
		 do (setq current-func (reorg--get-view-props nil 'reorg-data 'grouper-list))
		 and do (setf (alist-get current-func
					 alist nil nil #'equal)			      
			      (append
			       (alist-get current-func
					  alist nil nil #'equal)
			       (-list 
				(reorg--get-view-props nil 'reorg-data 'grouper-list-results))))
		 while (reorg--goto-next-relative-level 0)
		 finally return alist)))))

(defun reorg-tree--goto-first-sibling-in-current-group ()
  (cl-loop with current = (reorg--get-view-props nil 'reorg-data 'branch-predicate)
	   with point = (point)
	   while (and (reorg--goto-next-relative-level 0 'previous)
		      (equal (reorg--get-view-props nil 'reorg-data 'branch-predicate)
			     current))
	   do (setq point (point))
	   finally (goto-char point)))

(defun reorg-tree--goto-next-sibling-group (&optional previous)
  "adsf"
  (cl-loop with point = (point)
	   with current = (reorg--get-view-props nil 'reorg-data 'branch-predicate)
	   while (reorg--goto-next-relative-level 0 previous)
	   when (not (equal (reorg--get-view-props nil 'reorg-data 'branch-predicate)
			    current))
	   return (point)
	   finally return (progn (goto-char point) nil)))

(defun reorg-tree--map-siblings (func &optional pred pred-val test-fn)
  "Map all siblings at point, but restrict it using PRED, PRED-VAL,
and TEST-FN."
  (cl-loop initially (push (funcall func) results)
	   initially (when pred
		       (unless pred-val 
			 (setq pred-val
			       (funcall pred))))
	   with results = nil

	   for backward in '(t nil)
	   do (cl-loop with point = (point)
		       while (and (reorg--goto-next-relative-level 0 backward)
				  (or (not pred)
				      (funcall (or test-fn #'equal )
					       (funcall pred)
					       pred-val)))
		       do (push (funcall func) results)
		       finally (progn (setq results (reverse results))
				      (goto-char point)))
	   finally return results))

(defun reorg-tree--map-current-sibling-group (func)
  (reorg-tree--map-siblings func (lambda ()
				   (reorg--get-view-props nil
							  'reorg-data
							  'branch-predicate))))

(defun reorg-tree--get-current-group-members ()
  (reorg-tree--map-current-sibling-group
   (lambda ()
     (reorg--get-view-props nil 'reorg-data 'branch-name))))

(defun reorg-tree--map-siblings-by-group (func)
  (reorg-tree--with-wide-buffer
   (cl-loop with results = nil
	    do (push (reorg-tree--map-siblings
		      func
		      (lambda ()
			(reorg--get-view-props nil
					       'reorg-data
					       'branch-predicate)))
		     results)
	    while (reorg-tree--goto-next-sibling-group)
	    finally return results)))

(defun reorg-tree--get-sibling-group-markers ()
  "Get the markers for the starting point of each
sibling group."
  (reorg-tree--with-wide-buffer
   (cl-loop with results = nil
	    do (push (point-marker)
		     results)
	    while (reorg-tree--goto-next-sibling-group)
	    finally return (reverse results))))

(defun reorg-tree--goto-next-property-field (prop val &optional backward pred transformer)
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

will extract the single value prior to comparing to VAL."
  (let ((func (if backward
		  #'previous-single-property-change
		#'next-single-property-change))
	(limit (if backward
		   (point-min)
		 (point-max)))
	(pred (or pred #'eq))
	(search-invisible t))

    (cl-loop with point = (point)
	     with origin = (point)
	     while point

	     do (setq point (funcall func point (car (-list prop))))
	     
	     if (and (null point)
		     (funcall pred
			      (funcall (or transformer #'identity)
				       (apply #'reorg--get-view-props limit (-list prop)))
			      val))
	     return (goto-char limit)

	     else if (null point)
	     return (progn (goto-char origin) nil)
	     
	     else if (funcall pred
			      val
			      (funcall (or transformer #'identity)
				       (apply #'reorg--get-view-props point (-list prop))))
	     return (goto-char point)

	     else do (forward-char (- point (point))))))

(defun reorg-tree--branch-insert--insert-heading (data &optional after)
  "Insert a new branch using DATA at POINT or (point)."
  (let ((disable-point-adjustment t))
    (if after
	(progn (end-of-line) (insert "\n"))
      (beginning-of-line))
    (save-excursion 
      (insert (reorg--create-headline-string data
					     (alist-get 'format-string data)
					     (alist-get 'level data))
	      (if after "" "\n")))
    (reorg-dynamic-bullets--fontify-heading)
    (point)))

;; (cl-defun reorg--branch-insert--drop-into-outline (data template)
;;   (cl-labels
;;       ((doloop
;; 	(data
;; 	 template
;; 	 &optional (n 0 np)
;; 	 result-sorters
;; 	 grouper-list
;; 	 grouper-list-results
;; 	 format-string
;; 	 (level 1)
;; 	 (before t))
;; 	(let ((grouper `(lambda (x)
;; 			  (let-alist x
;; 			    ,(plist-get template :group))))
;; 	      (children (plist-get template :children))
;; 	      (heading-sorter (plist-get template :sort))
;; 	      (heading-sort-getter (or (plist-get template :sort-getter)
;; 				       #'car))
;; 	      (format-string (or (plist-get template :format-string)
;; 				 format-string
;; 				 reorg-headline-format))
;; 	      (result-sort (plist-get template :sort-results)))
;; 	  (when result-sort
;; 	    (setq result-sorters
;; 		  (append result-sorters					  
;; 			  (cl-loop for (form . pred) in result-sort
;; 				   collect (cons `(lambda (x)
;; 						    (let-alist x
;; 						      ,form))
;; 						 pred)))))
;; 	  (let ((name (funcall grouper data))
;; 		(members (reorg-tree--get-current-group-members)))
;; 	    (when name
;; 	      (if (member name members)
;; 		  (unless (equal name (reorg--get-view-props nil
;; 							     'reorg-data
;; 							     'branch-name))
;; 		    (reorg-tree--goto-next-property-field
;; 		     'reorg-data
;; 		     name
;; 		     nil
;; 		     #'equal
;; 		     (lambda (x) (alist-get 'branch-name x))))
;; 		(if (and heading-sort-getter heading-sorter members)
;; 		    (cl-loop
;; 		     with new-data = (list (cons 'name name)
;; 					   (cons 'branch-name name)
;; 					   (cons 'heading-sorter heading-sorter)
;; 					   (cons 'heading-sort-getter heading-sort-getter)
;; 					   (cons 'format-string format-string)
;; 					   (cons 'level level)
;; 					   (cons 'reorg-level level)
;; 					   (cons 'reorg-branch t)
;; 					   (cons 'branch-predicate grouper))
;; 		     when (funcall heading-sorter
;; 				   (funcall heading-sort-getter name)
;; 				   (funcall heading-sort-getter
;; 					    (reorg--get-view-props
;; 					     nil
;; 					     'reorg-data
;; 					     'branch-name)))
;; 		     return (reorg-tree--branch-insert--insert-heading new-data)
;; 		     while (reorg--goto-next-relative-level 0)
;; 		     finally return (reorg-tree--branch-insert--insert-heading new-data))
;; 		  (reorg-tree--branch-insert--insert-heading
;; 		   (list (cons 'name name)
;; 			 (cons 'branch-name name)
;; 			 (cons 'heading-sorter heading-sorter)
;; 			 (cons 'heading-sort-getter heading-sort-getter)
;; 			 (cons 'format-string format-string)
;; 			 (cons 'level level)
;; 			 (cons 'reorg-branch t)
;; 			 (cons 'reorg-level level)
;; 			 (cons 'branch-predicate grouper))
;; 		   (not before))))	  
;; 	      (if children 
;; 		  (cl-loop 
;; 		   with before = nil
;; 		   for x below (length children)
;; 		   for marker in (save-excursion
;; 				   (setq before (reorg--goto-next-relative-level 1))
;; 				   (reorg-tree--get-sibling-group-markers))
;; 		   do (goto-char marker)
;; 		   and do (doloop
;; 			   data
;; 			   (nth x children)
;; 			   x
;; 			   result-sorters
;; 			   nil
;; 			   nil
;; 			   format-string
;; 			   (1+ level)
;; 			   before))
;; 		(push (cons 'reorg-level (if (reorg-tree--branch-has-leaves-p)
;; 					     (1+ level)
;; 					   level))
;; 		      data)
;; 		(reorg--insert-into-leaves data
;; 					   result-sorters
;; 					   (if (reorg-tree--branch-has-leaves-p)
;; 					       (1+ level)
;; 					     level)
;; 					   format-string)
;; 		(redraw-display)))))))
;;     (goto-char (point-min))
;;     (doloop data template)))

(defun reorg--goto-headline-start ()
  "goto start of org headline"
  (save-match-data 
    (goto-char (org-entry-beginning-position))
    (re-search-forward "^\\*+[[:space:]]" nil t)
    (backward-char 1)
    (point)))

(defun reorg--get-headline-start ()
  "get the start of the headline"
  (save-excursion (reorg--goto-headline-start)))

(provide 'reorg)
