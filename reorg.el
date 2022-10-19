;; -*- lexical-binding: t; -*-

;; (require 'reorg-tree)
(require 'reorg-sort)
(require 'reorg-utils)
(require 'reorg-create)
(require 'reorg-org)
(require 'reorg-files)

;;; requires

(require 'let-alist)
(require 'reorg-dynamic-bullets)
(require 'org-visual-indent)

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
(defcustom reorg-headline-format '(concat .stars " " .headline)
  "Headline format.")

;;; variables 

(defvar-local reorg-current-template nil
  "the current template in this buffer")

(defvar reorg-parser-list nil
  "parser list")

;; (defvar reorg-words nil
;;   "A list of `reorg-words'.")

;; (defvar reorg--cache nil
;;   "The results of the parsed buffer.
;; For now, only for debugging purposes.")

;; (defvar reorg--grouped-results nil
;;   "The results of applying reorg--group-and-sort
;; to parsed data.  For now, only for debugging.")

;; (defvar reorg-setter-alist nil
;;   "setter alist")
;; (defvar reorg--last-point nil
;;   "last point (edit mode)")

;; (defun reorg--clone-outline-results (data &optional format-string)
;;   "asdf"
;;   (cl-loop for d in data
;; 	   collect (reorg--create-headline-string d format-string (plist-get d :level))))

(defun reorg--insert-org-headlines (data)
  "Insert grouped and sorted data into outline."
  (let (results)
    (cl-labels ((recurse (data)
			 (cond ((stringp data)
				(insert data "\n"))
			       (data (cl-loop for entry in data
					      do (recurse entry))))))
      (recurse data))))

;;; window control

(defun reorg--open-side-window ()
  "Open a side window to display the tree."
  (display-buffer-in-side-window (get-buffer-create reorg-buffer-name)
				 `((side . ,reorg-buffer-side)
				   (slot . nil)))
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
   (car 
    (window-at-side-list nil reorg-buffer-side))))

;;; view buffer

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

(defun reorg--get-view-prop (&optional property)
  "Get PROPERTY from the current heading."
  (save-excursion 
    (beginning-of-line)
    (let ((props (get-text-property (point-at-bol) reorg--data-property-name)))
      (if property
	  (alist-get property props)
	;;(plist-get props property)
	props))))

(defun reorg-outline-level ()
  "Get the outline level of the heading at point."
  (save-excursion
    (let ((search-invisible t))
      (outline-back-to-heading t)
      (re-search-forward "^*+ " (point-at-eol))
      (1- (length (match-string 0))))))

;; (defun reorg--get-field-at-point (&optional point)
;;   "Get the reorg-field-type at point."
;;   (get-text-property (or point (point)) reorg--field-property-name))

;; (defun reorg--get-field-bounds ()
;;   "Get the bounds of the field at point."
;;   (when-let ((field (reorg--get-field-at-point)))
;;     (cons
;;      (save-excursion 
;;        (cl-loop while (and (equal (reorg--get-field-at-point)
;; 				  field)
;; 			   (not (bobp)))
;; 		do (forward-char -1)
;; 		finally return (1+ (point))))
;;      (save-excursion 
;;        (cl-loop while (and (equal (reorg--get-field-at-point)
;; 				  field)
;; 			   (not (eobp)))

;; 		do (forward-char 1)
;; 		finally return (point))))))
;;; main

(defun reorg-open-sidebar-clone (&optional file)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (--> (reorg--map-entries file)
		      (reorg--clone-outline-results it '((stars) (" ") (headline))))))
    (when (get-buffer reorg-buffer-name)
      (kill-buffer reorg-buffer-name))
    (reorg--open-side-window)
    (reorg--select-tree-window)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (reorg-view-mode)
    (reorg-dynamic-bullets-mode)
    (org-visual-indent-mode)    
    (goto-char (point-min))))

(cl-defun reorg-open-sidebar (&key sources template)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (--> (reorg--getter sources)
		      (reorg--group-and-sort it template))))
    (when (get-buffer reorg-buffer-name)
      (kill-buffer reorg-buffer-name))
    (reorg--open-side-window)
    (reorg--select-tree-window)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (reorg-view-mode)
    (reorg-dynamic-bullets-mode)
    (org-visual-indent-mode)
    (toggle-truncate-lines 1)
    (setq reorg-current-template template)
    (setq-local cursor-type nil)
    (goto-char (point-min))))

(defun reorg-open-sidebar-fundamental (template &optional format-string file)
  "Open this shit in the sidebar."
  (interactive)
  (let ((results (--> (reorg--map-entries file)
		      (reorg--group-and-sort it template)
		      (reorg--process-results it format-string))))
    (when (get-buffer reorg-buffer-name)
      (kill-buffer reorg-buffer-name))
    (reorg--open-side-window)
    (reorg--select-tree-window)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (fundamental-mode)))

(defun reorg-open-sidebar (template &optional file)
  "Open this shit in the sidebar."
  (let ((results (--> (org-meta--run-org-ql file)
		      (reorg-group it template)
		      (reorg-process-results it))))
    (when (get-buffer org-meta-view-buffer-name)
      (kill-buffer org-meta-view-buffer-name))
    (org-meta--open-in-side-window)
    (org-meta--select-view-window)
    (fundamental-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (reorg--insert-org-headlines results)
    (org-meta-view-mode)
    (org-dynamic-bullets-mode -1)
    (reorg-dynamic-bullets-mode 1)
    (org-show-all)
    (goto-char (point-min))))

;;; reorg-views
;;;; clone functions

(defun reorg--jump-to-next-clone (&optional id previous)
  "Move to the next clone of the current node."
  (interactive)
  (let ((func (if previous
		  #'text-property-search-backward
		#'text-property-search-forward))
	(id (or id (reorg--get-view-prop 'id))))
    (if (funcall func reorg--data-property-name
		 id
		 (lambda (val alist)
		   (string=
		    (alist-get 'id alist)
		    val))
		 'not-current)
	(let ((point (point)))
	  (when previous (backward-char))
	  (outline-back-to-heading)
	  (save-excursion 
	    (reorg--unfold-at-point point)
	    (reorg-edits--update-box-overlay)
	    point))
      (if previous
	  (goto-char (point-max))
	(goto-char (point-min)))
      (reorg--jump-to-next-clone id previous)))
  (reorg-edits--post-field-navigation-hook))

(defun reorg--jump-to-previous-clone (&optional id)
  "Jump to previous clone"
  (interactive)
  (reorg--jump-to-next-clone id 'previous))

;;;; view buffer functions

(defun reorg-view--update-highlight-overlay (&optional &rest _args)
  "update transclusion overlay."
  nil)
;; (delete-overlay reorg-current-heading-overlay)
;; (move-overlay reorg-current-heading-overlay (reorg--get-headline-start) (point-at-eol)))

;; (defun reorg--initialize-overlay ()
;;   "initialize the transclusion overlay."
;;   nil)
;; (setq reorg-current-heading-overlay
;; 	(make-overlay 1 2))
;; (overlay-put reorg-current-heading-overlay
;; 	       'face
;; 	       'reorg-current-heading-face)
;; (overlay-put reorg-current-heading-overlay 'insert-behind-hooks '(reorg--transclusion-logger
;; 								    reorg-view--update-highlight-overlay
;; 								    reorg--modification-hook-func))
;; (overlay-put reorg-current-heading-overlay 'insert-in-front-hooks '(reorg--transclusion-logger reorg--modification-hook-func))
;; (overlay-put reorg-current-heading-overlay 'modification-hooks '(reorg--transclusion-logger reorg--modification-hook-func))
;; (delete-overlay reorg-current-heading-overlay))





;; (reorg-props 'headline :val (propertize (plist-get props :headline)
;; 					    reorg--data-property-name props))
(defun reorg-view--tree-to-source--goto-heading (&optional id buffer no-narrow no-select)
  "Goto ID in the source buffer. If NARROW is non-nil, narrow to the heading."
  (interactive)
  (when  (and (or buffer (reorg--get-view-prop 'buffer))
	      (or id (reorg--get-view-prop 'id)))
    (if reorg-parser-use-id-p 
	(reorg-view--goto-source-id
	 (or buffer (reorg--get-view-prop 'buffer))
	 (or id (reorg--get-view-prop 'id))
	 (not no-narrow))
      (reorg-view--goto-source-marker 
       (or buffer (reorg--get-view-prop 'buffer))
       (or id (reorg--get-view-prop 'marker))
       (not no-narrow)))))

(defun reorg-view--source--goto-end-of-meta-data ()
  "Go to the end of the meta data and insert a blank line
if there is not one."
  (let ((next-heading (reorg--with-restore-state
		       (outline-next-heading)
		       (point)))
	(end-of-meta-data (save-excursion
			    (org-end-of-meta-data t)
			    (re-search-backward (rx (not whitespace)))
			    (match-end 0))))

    (if (= (- next-heading end-of-meta-data) 1)
	(progn (goto-char end-of-meta-data)
	       (insert "\n"))
      (goto-char (1+ end-of-meta-data)))))

(defun reorg-view--source--narrow-to-heading ()
  "Narrow to the current heading only, i.e., no subtree."
  (org-back-to-heading)
  (org-narrow-to-element))

(defun reorg-view--goto-source-id (buffer id &optional narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (with-current-buffer buffer 
    (let ((old-point (point))
	  (search-invisible t))
      (widen)
      (goto-char (point-min))
      (if (re-search-forward id nil t)
	  (when narrow
	    (reorg-view--source--narrow-to-heading))
	t)
      (goto-char old-point)))
  (reorg--select-main-window)
  (set-window-buffer (selected-window) buffer))


(defun reorg-view--goto-source-marker (buffer marker &optional narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (reorg--select-main-window)
  (set-window-buffer (selected-window) buffer)
  (widen)
  (goto-char (marker-position marker))
  (when narrow
    (reorg-view--source--narrow-to-heading)
    t)
  nil)

(defun reorg--move-to-next-entry-follow ()
  (interactive)
  (outline-next-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-previous-entry-follow ()
  (interactive)
  (outline-previous-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-next-entry-no-follow ()
  (interactive)
  (outline-next-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (org-back-to-heading)
  (reorg--select-tree-window))

(defun reorg--move-to-previous-entry-no-follow ()
  (interactive)
  (outline-previous-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (org-back-to-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--goto-next-parent ()
  "Goto the next parent."
  (interactive)
  (when (re-search-forward
	 (concat "^*\\{" (number-to-string (1- (outline-level))) "\\} ") nil t)
    (beginning-of-line)
    (reorg-edits--post-field-navigation-hook)
    (reorg-view--update-highlight-overlay)
    (reorg-edits--post-field-navigation-hook)))

(defun reorg--goto-parent ()
  "Goto the next parent."
  (interactive)
  (org-up-heading-safe)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook))

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

;; (defun reorg--update-this-heading (data template)
;;   "Delete the heading and all clones, re-insert them into the outline,
;; move to the first new entry."
;;   (let ((disable-point-adjustment t)
;; 	(search-invisible t)
;; 	(id (reorg--get-view-prop 'id)))
;;     (reorg--select-tree-window)
;;     (reorg--map-id (plist-get data :id)
;; 		   (reorg-views--delete-leaf)
;; 		   (reorg-views--delete-headers-maybe))
;;     (reorg--branch-insert--drop-into-outline data
;; 					     template)))

;;;; view mode

;; (defun reorg--shift-up (arg)
;;   "Shift priority or timestamp."
;;   (interactive "P")
;;   (pcase (reorg-edits--get-field-type)
;;     ((or 'deadline
;; 	 'scheduled
;; 	 'timestamp
;; 	 'timestamp-ia)
;;      (org-timestamp-up arg))
;;     (`priority
;;      (org-priority-up))))

;; (defun reorg--shift-down (arg)
;;   "Shift priority or timestamp."
;;   (interactive "P")
;;   (pcase (reorg-edits--get-field-type)
;;     ((or 'deadline
;; 	 'scheduled
;; 	 'timestamp
;; 	 'timestamp-ia)
;;      (org-timestamp-down arg))
;;     (`priority
;;      (org-priority-down))))

(defvar reorg-view-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'reorg-view--tree-to-source--goto-heading)
    ;; (define-key map (kbd "e") #'reorg-edits--start-edit)
    (define-key map (kbd "u") #'reorg--goto-parent)
    ;; (define-key map (kbd "f") #'reorg-edits-move-to-next-field)
    ;; (define-key map (kbd "S-<up>") #'reorg--shift-up)
    ;; (define-key map (kbd "S-<down>") #'reorg--shift-down)
    ;; (define-key map (kbd "b") #'reorg-edits-move-to-previous-field)
    (define-key map (kbd "c") #'reorg--jump-to-next-clone)
    (define-key map (kbd "C") #'reorg--jump-to-previous-clone)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map (kbd "n") #'reorg--move-to-next-entry-no-follow)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry-no-follow)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'outline-cycle)
    (define-key map (kbd "l") #'recenter-top-bottom)
    map)
  "keymap")

;; (defmacro reorg--with-source-buffer (&rest body)
;;   "Execute BODY in the source buffer and
;; update the heading at point."
;;   (declare (indent defun))
;;   `(progn
;;      (let ((val (field-string-no-properties)))
;;        (reorg-view--tree-to-source--goto-heading)
;;        (save-restriction
;; 	 (save-excursion 
;; 	   ,@body)))))



(define-derived-mode reorg-view-mode
  fundamental-mode
  "Org tree view"
  "Tree view of an Orgmode file. \{keymap}"
  ;; (reorg--initialize-overlay)
  (setq cursor-type nil)
  ;;(setq-local disable-point-adjustment t)
  (use-local-map reorg-view-mode-map)
  (add-hook 'post-command-hook #'reorg-edits--update-box-overlay nil t))

;;; reorg-edit-mode
;;;; transclusion hook

;; (defun reorg--modification-hook-func (overlay postp beg end &optional length)
;;   "overlay post change hook."
;;   (when postp
;;     (save-match-data
;;       (let* ((overlay-beg (overlay-start overlay))
;; 	     (headline-beg (reorg--get-headline-start))
;; 	     (relative-beg (if (<= (- beg headline-beg) 0)
;; 			       0
;; 			     (- beg headline-beg)))
;; 	     (adjustment (if (< beg overlay-beg)
;; 			     (- beg overlay-beg)
;; 			   0)))
;; 	(cond
;; 	 ((and (= beg end) (> length 0))
;; 	  (reorg--with-point-at-orig-entry nil nil
;; 					   (reorg--goto-headline-start)
;; 					   (forward-char relative-beg)
;; 					   (delete-region (point) (+ (point) length))))	 
;; 	 ((and (/= beg end) (> length 0))
;; 	  (let ((s (buffer-substring-no-properties (+ overlay-beg
;; 						      relative-beg) end)))
;; 	    (message s)
;; 	    (reorg--with-point-at-orig-entry nil nil
;; 					     (reorg--goto-headline-start)
;; 					     (forward-char relative-beg)
;; 					     (delete-region (point) (+ (point)
;; 								       (+ length adjustment)))
;; 					     (insert s))))	 
;; 	 ((or (= length 0) (/= beg end))
;; 	  (let ((s (buffer-substring-no-properties beg end)))
;; 	    (reorg--with-point-at-orig-entry nil nil
;; 					     (reorg--goto-headline-start)
;; 					     (forward-char relative-beg)
;; 					     (insert s)))))))))


;;;; customs

;; (defcustom reorg-edits-commit-edit-shortcut "C-c C-c"
;;   "Shortcut to commit edits when in `reorg-edits-mode'
;; Accepts any string acceptable to `kbd'."
;;   :type 'string)

;; (defcustom reorg-edits-abort-edit-shortcut "C-c C-k"
;;   "Shortcut to abort edits when in `reorg-edits-mode'
;; Accepts any string acceptable to `kbd'."
;;   :type 'string)

;; (defcustom reorg-edits-start-edit-shortcut "C-c C-c"
;;   "Shortcut to initiate `reorg-edits-mode'
;; Accepts any string acceptable to `kbd'."
;;   :type 'string)

;;;; variables

;; (defvar reorg-edits--restore-state nil
;;   "When editing a clone, save the current headline and body
;; to restore if the edit is abandoned.")

;; (defvar reorg-edits--previous-header-line nil
;;   "previous header line")

;; (defvar reorg-edits--header-line
;;   '(:eval
;;     (format 
;;      "Editing headline. '%s' to finish and update. '%s' to abandon."
;;      reorg-edits-commit-edit-shortcut
;;      reorg-edits-abort-edit-shortcut))
;;   "The value of header-line-format when `reorg-edits-mode' is 
;; invoked.")

;; (defvar reorg-edits--current-field-overlay
;;   (let ((overlay (make-overlay 1 2)))
;;     (overlay-put overlay 'face '( :box (:line-width -1)
;; 				  :foreground "cornsilk"))    
;;     (overlay-put overlay 'priority 1000)
;;     overlay)
;;   "Overlay for field at point.")

;; (defvar reorg-edits-field-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd reorg-edits-commit-edit-shortcut)
;;       #'reorg-edits--commit-edit)
;;     (define-key map (kbd reorg-edits-abort-edit-shortcut)
;;       #'reorg-edits--discard-edit)
;;     (define-key map (kbd "TAB") #'reorg-edits-move-to-next-field)
;;     (define-key map (kbd "BACKTAB") #'reorg-edits-move-to-previous-field)
;;     (define-key map [remap kill-line] #'reorg--kill-field)
;;     map)
;;   "keymap.")

;;;; macros



;;;; field navigation 

(defun reorg-edits--post-field-navigation-hook ()
  "Tell the user what field they are on."
  (reorg-edits--update-box-overlay)
  (setf (point) (car 
		 (reorg-edits--get-field-bounds))))

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
      ;; (message "You are on the field for the heading's %s"
      ;; 	 (reorg-edits--get-field-type)))
      (setq point (point)))))

;; (defun reorg-edits--move-selection-overlay ()
;;   (if-let ((bounds (reorg-edits--get-field-bounds)))
;;       (move-overlay reorg-edits--current-field-overlay
;; 		    (car bounds)
;; 		    (cdr bounds))
;;     (delete-overlay reorg-edits--current-field-overlay)))

;; (defun reorg-edits-move-to-next-field (&optional previous)
;;   "Move to the next field at the current heading."
;;   (interactive)
;;   (let ((current-field (reorg-edits--get-field-at-point)))
;;     (unless (if previous (= (point) (org-entry-beginning-position))
;; 	      (= (point) (org-entry-end-position)))
;;       (cl-loop with point = (point)
;; 	       do (if previous (cl-decf point) (cl-incf point))
;; 	       when (and (reorg-edits--get-field-at-point point)
;; 			 (not (equal (reorg-edits--get-field-at-point point)
;; 				     current-field)))
;; 	       return (prog1 (setf (point) point)
;; 			(reorg-edits--post-field-navigation-hook))
;; 	       when (if previous (= point (org-entry-beginning-position))
;; 		      (= point (org-entry-end-position)))
;; 	       return nil)
;;       (reorg-edits--post-field-navigation-hook))))

;; (defun reorg-edits-move-to-previous-field ()
;;   "Move to the next field at the current heading."
;;   (interactive)  
;;   (reorg-edits-move-to-next-field 'previous))

;;;; field editing

;; (defun reorg--kill-field ()
;;   "Temporary replacement for kill-line when editing a field."
;;   (interactive)
;;   (delete-region (point) (field-end)))

;; (defun reorg--kill-field ()
;;   "Temporary replacement for kill-line when editing a field."
;;   (interactive)
;;   (goto-char (field-beginning))
;;   (funcall-interactively #'self-insert-command 1 ? )
;;   (delete-region (point) (field-end))
;;   (backward-char 1))

;; (defun reorg-edits--get-field-type ()
;;   "Get the field type at point, if any."
;;   (get-text-property (point) reorg--field-property-name))

;; (defun reorg-edit-field--replace-field-at-point (val field)
;;   "Replace FIELD with VAL."
;;   (let ((beg (field-beginning))
;; 	(end (field-end)))
;;     (delete-region beg end)
;;     (goto-char beg)
;;     (insert (propertize val `(reorg--field-property-name
;; 			      ,field
;; 			      field
;; 			      ,field)))))

;; (defun reorg-edits--get-field-value ()
;;   "Get the value of the field at point.  Return 
;; nil if there is no value."
;;   (field-string-no-properties))
;; (pcase-let ((`(,start . ,end) (reorg-edits--get-field-bounds)))
;;   (when (and start end)
;;     (buffer-substring-no-properties start end))))

;; (defun reorg-edits--commit-edit ()
;;   "Discard the current edit and restore the node
;; to its previous state, and turn off the minor mode."
;;   (interactive)
;;   (let ((type (reorg-edits--get-field-type))
;; 	(val (reorg-edits--get-field-value)))
;;     (funcall (alist-get type reorg-setter-alist) val)
;;     (reorg--update-this-heading)
;;     (reorg-edits-mode -1)))

;; (cl-defun reorg-get-set-props (prop &key
;; 				    (val nil valp)
;; 				    keep
;; 				    multi-value
;; 				    inherit
;; 				    literal-nil
;; 				    no-duplicates
;; 				    no-text-properties
;; 				    &allow-other-keys)
;;   "Change the org heading at point by set PROP to VAL.
;; It accepts the following properties, as well as any others that are set 
;; in the headings property drawer. Any such properties can be accessed as string
;;  or a symbol, e.g., \"CATEGORY\" or 'category.  See the return value of 
;; `reorg-parser--headline-parser' for more information.
;; There are flags for dealing with multivalued properties, inheritence, 
;; etc.:
;; If VAL is a list, assume a multi-valued property.
;; If KEEP is non-nil and VAL is a list or MULTI is non-nil, keep the old value.
;; If NO-DUPLICATES is non-nil and dealing with multi-valued, delete duplicates.
;; If MULTI is non-nil, use a multivalued property even if VAL is not a list.
;; Return a cons cell with the old value as the `car' and new value as the `cdr'."
;;   (cl-macrolet ((get-or-set (&key get set)
;; 			    `(if (not valp)
;; 				 ,get
;; 			       (let ((old-val ,get))
;; 				 (save-excursion 
;; 				   (org-back-to-heading)		 
;; 				   ,set
;; 				   (cons old-val ,get))))))
;;     (pcase prop
;;       ;;(org-insert-time-stamp (org-read-date t t "2021-01-01"))
;;       (`deadline
;;        (get-or-set :get (org-entry-get (point) "DEADLINE" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-deadline '(4))
;; 			  (org-deadline nil val))))
;;       (`scheduled
;;        (get-or-set :get (org-entry-get (point) "SCHEDULED" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-schedule '(4))
;; 			  (org-schedule nil val))))
;;       (`comment
;;        (get-or-set :get (org-in-commented-heading-p)
;; 		   :set (when (not (xor (not val)
;; 					(org-in-commented-heading-p)))
;; 			  (org-toggle-comment))))
;;       (`tags
;;        (get-or-set :get (org-get-tags (point) (not inherit))
;; 		   :set (if keep
;; 			    (org-set-tags (if no-duplicates
;; 					      (delete-duplicates (append old-val
;; 									 (-list val))
;; 								 :test #'string=)
;; 					    (append old-val (-list val))))
;; 			  (org-set-tags val))))
;;       (`headline
;;        (get-or-set :get (org-entry-get (point) "ITEM")
;; 		   ;; keep the comment if it is there
;; 		   :set (let ((commentedp (org-in-commented-heading-p)))
;; 			  (org-edit-headline val)
;; 			  (when commentedp
;; 			    (reorg-get-set-props 'comment :val t)))))
;;       (`todo
;;        (get-or-set :get (org-entry-get (point) "TODO")
;; 		   :set (org-todo val)))
;;       ((or `timestamp
;; 	   `timestamp-ia)
;;        (get-or-set :get (org-entry-get (point) (if (eq 'timestamp prop)
;; 						   "TIMESTAMP"
;; 						 "TIMESTAMP_IA"))
;; 		   :set (if (and old-val
;; 				 (search-forward old-val (org-entry-end-position) t))
;; 			    (progn (replace-match (concat val))
;; 				   (delete-blank-lines))
;; 			  (org-end-of-meta-data t)
;; 			  (delete-blank-lines)
;; 			  (when val 
;; 			    (insert (concat val "\n"))))))
;;       (`body
;;        (get-or-set :get (reorg-edits--get-body-string no-text-properties)
;; 		   :set (error "You can't set body text (yet).")))
;;       ((or (pred stringp)
;; 	   (pred symbolp))
;;        (when (symbolp prop) (setq prop (symbol-name prop)))
;;        (get-or-set :get (if (or multi-value (and val (listp val)) keep)
;; 			    (org-entry-get-multivalued-property (point) prop)
;; 			  (org-entry-get (point) prop inherit literal-nil))
;; 		   :set (cond ((or multi-value (listp val) keep)
;; 			       (apply #'org-entry-put-multivalued-property (point) prop
;; 				      (if no-duplicates
;; 					  (delete-duplicates (append old-val (-list val)) :test #'string=)
;; 					(append old-val (-list val)))))
;; 			      (t (org-entry-put (point) prop val))))))))

;; (defun reorg-edits--sync-field-with-source ()
;;   "Set SOURCE to the value of the current field."
;;   (let ((field (reorg-edits--get-field-at-point))
;; 	(val (reorg-edits--get-field-value)))
;;     (reorg--with-point-at-orig-entry nil
;; 				     (reorg-get-set-props field
;; 							  :val val ))))

;; (defun reorg-edits--discard-edit ()
;;   "Discard the current edit and restore the node
;; to its previous state, and turn off the minor mode."
;;   (interactive)
;;   (reorg-edits-mode -1)
;;   (when reorg-edits--restore-state
;;     (reorg-views--replace-heading
;;      reorg-edits--restore-state))
;;   (setq reorg-edits--restore-state nil)
;;   (message "Discarded edit."))

;; (defun reorg-edits--start-edit ()
;;   "Start editing the headline at point."
;;   (interactive)
;;   (reorg-edits-mode 1))

;; (defun reorg-edits--move-into-field ()
;;   "If the point is at the border of a field, then 
;; move it into the field.  This ensures that `reorg-edits--get-field-bounds'
;; returns the correct positions."
;;   (cond ((and (get-text-property (point) reorg--field-property-name)
;; 	      (not (get-text-property (1- (point)) reorg--field-property-name))
;; 	      (get-text-property (+ 1 (point)) reorg--field-property-name))
;; 	 (forward-char 1))
;; 	((and (get-text-property (point) reorg--field-property-name)
;; 	      (not (get-text-property (1+ (point)) reorg--field-property-name)))
;; 	 (forward-char -1))))

;; (defun reorg-edits--get-field-at-point (&optional point)
;;   "Get the `reorg--field-property-name' at point."
;;   (get-text-property (or point (point)) reorg--field-property-name))

;; (defun reorg-edits--kill-line ()
;;   "Kill up to the end of the end point."
;;   (interactive)
;;   (pcase-let ((`(,start . ,end) (reorg-edits--get-field-bounds)))
;;     (delete-region start end)))

;; (defun reorg-edits--get-field-bounds ()
;;   "Get the bounds of the field at point."
;;   (when-let ((field (reorg-edits--get-field-at-point)))
;;     (cons
;;      (save-excursion 
;;        (cl-loop while (and (equal (reorg-edits--get-field-at-point)
;; 				  field)
;; 			   (not (bobp)))
;; 		do (forward-char -1)
;; 		finally return (1+ (point))))
;;      (save-excursion 
;;        (cl-loop while (and (equal (reorg-edits--get-field-at-point)
;; 				  field)
;; 			   (not (eobp)))

;; 		do (forward-char 1)
;; 		finally return (point))))))

(defun reorg-views--insert-before-point (data &optional level format-string)
  "insert a hearing before the heading at point."
  (reorg--with-restore-state
   (beginning-of-line)
   (insert "\n")
   (previous-line 1)
   (beginning-of-line)
   (let ((string (reorg--create-headline-string data
						(or format-string reorg-headline-format)
						(or level (reorg-outline-level)))))
     (insert string)
     (reorg-dynamic-bullets--fontify-heading)
     (1+ (length string)))))

(defun reorg-views--insert-after-point (data &optional level format-string)
  "insert a heading after the current point."
  (reorg--with-restore-state
   (end-of-line)
   (insert "\n")
   ;; (previous-line 1)
   ;; (end-of-line)
   (let ((string (reorg--create-headline-string data
						(or format-string reorg-headline-format)
						(or level (reorg-outline-level)))))
     (insert string)
     (reorg-dynamic-bullets--fontify-heading)
     (1+ (length string)))))

(defun reorg-views--delete-leaf ()
  "delete the heading at point"
  (let ((inhibit-field-text-motion t))
    (delete-region (point-at-bol)
		   (line-beginning-position 2))))

(defun reorg-views--delete-headers-maybe () ;; SUSPECT
  "VERY SUSPECT"
  (save-excursion 
    (cl-loop while (and (reorg-tree--goto-next-property-field 'reorg-field-type 'branch t)
			(not (reorg--get-next-level-branches))
			(not (reorg-tree--branch-has-leaves-p)))
	     do (reorg-views--delete-leaf))))
;; do (reorg-views--delete-heading))))

(defun reorg-views--replace-heading (data) 
  "Replace the heading at point with DATA. SUSPECT"
  (let ((level (reorg-outline-level))
	(inhibiit-field-text-motion t)
	(search-invisible t))
    (save-excursion
      ;; (reorg-views--delete-leaf)
      (reorg-views--delete-headers-maybe)
      (reorg-views--insert-before-point data level))))

(defun reorg-view--update-view-headline ()
  "Goto source buffer, re-parse, update. WORKS"
  (let ((inhibit-modification-hooks t)
	(props (reorg--with-point-at-orig-entry nil nil
						(reorg--parser nil 'org)))
	(level (reorg-outline-level))
	(format (save-excursion
		  (cl-loop while (org-up-heading-safe)
			   when (reorg--get-view-prop 'format-string)
			   return (reorg--get-view-prop 'format-string)
			   finally return reorg-headline-format))))
    (reorg-views--delete-leaf)
    (reorg--insert-heading props level format)))

;;;; edit mode
;;;;; minor mode

;; (define-minor-mode reorg-edits-mode
;;   "Minor mode to edit headlines."
;;   nil
;;   " EDITING HEADLINE"
;;   reorg-edits-field-mode-map
;;   (when-let* ((prop (get-text-property (point) reorg--field-property-name))
;; 	      (start (previous-single-property-change (point) reorg--field-property-name))
;; 	      (end (next-single-property-change (point) reorg--field-property-name))
;; 	      (val (buffer-substring start end)))
;;     ;; on
;;     (if reorg-edits-mode
;; 	(progn		
;; 	  ;; (reorg--add-command-hooks)
;; 	  ;;(reorg--lock-buffer)
;; 	  (setq cursor-type 'bar
;; 		reorg--last-point nil
;; 		reorg-edits--previous-header-line header-line-format
;; 		header-line-format reorg-edits--header-line
;; 		reorg-edits--restore-state (get-text-property (point-at-bol) 'reorg-data)
;; 		reorg-edit-field--start-marker (set-marker (make-marker) start)
;; 		reorg-edit-field--end-marker (set-marker (make-marker) end)
;; 		overriding-local-map reorg-edits-field-mode-map))
;;       ;; off
;;       ;;(reorg--unlock-buffer)
;;       ;; (reorg--add-command-hooks 'remove)
;;       (delete-overlay reorg-edits--current-field-overlay)
;;       (setq header-line-format reorg-edits--previous-header-line
;; 	    reorg-edits--previous-header-line nil
;; 	    reorg-edit-field--start-marker nil
;; 	    reorg-edit-field--end-marker nil
;; 	    reorg-edits--restore-state nil
;; 	    overriding-local-map nil
;; 	    cursor-type nil))))

;;;;; keep point in field

;; (defun reorg--pre-command-hook ()
;;   "asdf"
;;   (interactive)
;;   (if (eq 'reorg (car (field-at-pos (point))))
;;       (setq reorg--last-point (point))))

;; (defun reorg--post-command-hook ()
;;   "zxcv"
;;   (when reorg--last-point
;;     (constrain-to-field nil reorg--last-point)
;;     (setq reorg--last-point nil)))

;; (defun reorg--add-command-hooks (&optional remove)
;;   "asdf"
;;   (setq reorg--last-point nil)
;;   (if remove
;;       (progn 
;; 	(remove-hook 'pre-command-hook #'reorg--pre-command-hook t)
;; 	(remove-hook 'post-command-hook #'reorg--post-command-hook t))
;;     (add-hook 'pre-command-hook #'reorg--pre-command-hook nil t)
;;     (add-hook 'post-command-hook #'reorg--post-command-hook nil t)))

;;;; get or set props 

;; (cl-defun reorg-get-set-props (prop &key
;; 				    (val nil valp)
;; 				    keep
;; 				    multi-value
;; 				    inherit
;; 				    literal-nil
;; 				    no-duplicates
;; 				    no-text-properties
;; 				    &allow-other-keys)
;;   "Change the org heading at point by set PROP to VAL.

;; It accepts the following properties, as well as any others that are set 
;; in the headings property drawer. Any such properties can be accessed as string
;;  or a symbol, e.g., \"CATEGORY\" or 'category.  See the return value of 
;; `reorg-parser--headline-parser' for more information.

;; There are flags for dealing with multivalued properties, inheritence, 
;; etc.:

;; If VAL is a list, assume a multi-valued property.
;; If KEEP is non-nil and VAL is a list or MULTI is non-nil, keep the old value.
;; If NO-DUPLICATES is non-nil and dealing with multi-valued, delete duplicates.
;; If MULTI is non-nil, use a multivalued property even if VAL is not a list.

;; Return a cons cell with the old value as the `car' and new value as the `cdr'."
;;   (cl-macrolet ((get-or-set (&key get set)
;; 			    `(if (not valp)
;; 				 ,get
;; 			       (let ((old-val ,get))
;; 				 (save-excursion 
;; 				   (org-back-to-heading)		 
;; 				   ,set
;; 				   (cons old-val ,get))))))
;;     (pcase prop
;;       ;;(org-insert-time-stamp (org-read-date t t "2021-01-01"))
;;       (`deadline
;;        (get-or-set :get (org-entry-get (point) "DEADLINE" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-deadline '(4))
;; 			  (org-deadline nil val))))
;;       (`scheduled
;;        (get-or-set :get (org-entry-get (point) "SCHEDULED" inherit literal-nil)
;; 		   :set (if (null val)
;; 			    (org-schedule '(4))
;; 			  (org-schedule nil val))))
;;       (`comment
;;        (get-or-set :get (org-in-commented-heading-p)
;; 		   :set (when (not (xor (not val)
;; 					(org-in-commented-heading-p)))
;; 			  (org-toggle-comment))))
;;       (`tags
;;        (get-or-set :get (org-get-tags (point) (not inherit))
;; 		   :set (if keep
;; 			    (org-set-tags (if no-duplicates
;; 					      (delete-duplicates (append old-val
;; 									 (-list val))
;; 								 :test #'string=)
;; 					    (append old-val (-list val))))
;; 			  (org-set-tags val))))
;;       (`headline
;;        (get-or-set :get (org-entry-get (point) "ITEM")
;; 		   ;; keep the comment if it is there
;; 		   :set (let ((commentedp (org-in-commented-heading-p)))
;; 			  (org-edit-headline val)
;; 			  (when commentedp
;; 			    (reorg-get-set-props 'comment :val t)))))
;;       (`todo
;;        (get-or-set :get (org-entry-get (point) "TODO")
;; 		   :set (org-todo val)))
;;       ((or `timestamp
;; 	   `timestamp-ia)
;;        (get-or-set :get (org-entry-get (point) (if (eq 'timestamp prop)
;; 						   "TIMESTAMP"
;; 						 "TIMESTAMP_IA"))
;; 		   :set (if (and old-val
;; 				 (search-forward old-val (org-entry-end-position) t))
;; 			    (progn (replace-match (concat val))
;; 				   (delete-blank-lines))
;; 			  (org-end-of-meta-data t)
;; 			  (delete-blank-lines)
;; 			  (when val 
;; 			    (insert (concat val "\n"))))))
;;       (`body
;;        (get-or-set :get (reorg-edits--get-body-string no-text-properties)
;; 		   :set (error "You can't set body text (yet).")))
;;       ((or (pred stringp)
;; 	   (pred symbolp))
;;        (when (symbolp prop) (setq prop (symbol-name prop)))
;;        (get-or-set :get (if (or multi-value (and val (listp val)) keep)
;; 			    (org-entry-get-multivalued-property (point) prop)
;; 			  (org-entry-get (point) prop inherit literal-nil))
;; 		   :set (cond ((or multi-value (listp val) keep)
;; 			       (apply #'org-entry-put-multivalued-property (point) prop
;; 				      (if no-duplicates
;; 					  (delete-duplicates (append old-val (-list val)) :test #'string=)
;; 					(append old-val (-list val)))))
;; 			      (t (org-entry-put (point) prop val))))))))




;;;; inserting headers into the appropriate location

;;;;; reorg-map-branches

(defmacro reorg--map-all-branches (&rest body)
  "Move to the next clone of the current node."
  `(save-restriction 
     (save-excursion
       (goto-char (point-min))
       (while
	   (text-property-search-backward 'reorg-field-type
					  'branch
					  nil
					  'not-current)
	 ,@body))))

(defun reorg--last-branch-p ()
  "Does the current branch have any children?"
  (save-excursion 
    (forward-line)
    (not (eq 'branch (get-text-property (point) 'reorg-field-type)))))

;;;;; insert-into-branch 

(defun reorg-tree--branch-has-leaves-p ()
  (save-excursion 
    (let ((disable-point-adjustment t)
	  (inhibit-field-text-motion t))
      (forward-line)
      (not (eq 'branch
	       (reorg--get-view-props nil 'reorg-field-type))))))

(defun reorg--insert-into-leaves (data sorters &optional level format-string)
  (let ((disable-point-adjustment t)
	(format-string (or format-string (reorg--get-view-props nil 'reorg-data 'format-string))))
    (when (and (null (reorg--children-p))
	       (eq (reorg--get-view-props nil 'reorg-field-type) 'branch))
      (if (reorg-tree--branch-has-leaves-p)
	  (progn 
	    (forward-line 1) ;; this should be a text-property forward, not a line forward
	    (cl-loop while (not (eq (reorg--get-view-props nil 'reorg-field-type)
				    'branch))
		     with level = (or level (reorg-outline-level))
		     if (cl-loop for (func . pred) in sorters
				 if (funcall pred
					     (funcall func data)
					     (funcall func (reorg--get-view-props)))
				 return t
				 finally return nil)
		     return (reorg-views--insert-before-point data level format-string)
		     else do (forward-line)
		     finally return (reorg-views--insert-before-point data level format-string)))
	(reorg-views--insert-after-point data (1+ level) format-string ))))
  (reorg-dynamic-bullets--fontify-heading))

;; (defun reorg--set-text-property (plist val &rest props)
;;   "Set the text property at point to a new value.
;; Set the text property (car PROPS) to the value of
;; the PROPS drilling nested plist whatever.

;; If PROPS is only one element then start with 'reorg-data
;; otherwise start with the car of PROPS.

;; If PLIST is nil, then get the text properties at point."
;;   (let* ((inhibit-field-text-motion t)
;; 	 (results (or plist (text-properties-at (point)))))
;;     (cl-loop for prop in (or (butlast props) `(,reorg--data-property-name))
;; 	     with new-results = nil
;; 	     do (setf new-results (if prop
;; 				      (plist-get results prop)
;; 				    results))
;; 	     finally return (progn (plist-put new-results (car (last props)) val)
;; 				   (put-text-property (point-at-bol)
;; 						      (point-at-eol)
;; 						      reorg--data-property-name
;; 						      new-results)
;; 				   results))))

(defun reorg--insert-heading (&optional data level format)
  (let ((inhibit-field-text-motion t))
    (beginning-of-line)
    (insert 
     (reorg--create-headline-string (or data (reorg--get-view-props nil))
				    (or format reorg-headline-format)
				    (or level (reorg-outline-level)))
     "\n")
    (forward-line -1)
    (reorg-dynamic-bullets--fontify-heading)))




;;;; Footer





;;; reorg-view starts here

;;;; text property navigation 



(defun reorg--goto-previous-property-field (prop val &optional pred transformer)
  "Move to the beginning of the buffer position that
text property PROP that matches VAL.  Check for matching VAL
using `eq', unless PRED is suppied."
  (reorg-tree--goto-next-property-field nil prop val 'backward pred transformer))

(defun reorg--get-next-level-branches ()
  "Get the headlines of the next level sub-branches."
  (save-excursion 
    (let (results)
      (reorg--map-next-level (push (reorg--get-view-props nil 'reorg-data 'headline)
				   results))
      results)))

;; (defun reorg--goto-previous-branch (&optional relative-level)
;;   "Wrapper for `reorg--goto-next-branch' which moves to the
;; previous branch."
;;   (reorg--goto-next-branch relative-level 'previous))

;;;; tree navigation re-write

(defmacro reorg--map-id (id &rest body)
  "Execute BODY at each entry that matches ID."
  `(save-excursion  
     (goto-char (point-min))
     (while (text-property-search-forward reorg--data-property-name
					  ,id
					  (lambda (val alist)
					    (string= 
					     (alist-get 'id alist)
					     val))
					  'not-current)
       (save-excursion 
	 (outline-back-to-heading)
	 ,@body)
       (forward-char 1))))

(defmacro reorg--map-next-level (&rest body)
  "Go to the next branch. With integer RELATIVE-LEVEL, go to the next
level relative to the current one.  For example, if the current outline
level is 2, and RELATIVE-LEVEL is -1, the function will move to the next
branch at level 1.  If RELATIVE-LEVEL is 1, the function will move to the
next branch at level 3.

If PREVIOUS is non-nil, move to the previous branch instead of the next.

Return nil if there is no such branch."
  `(let ((start-level (reorg-outline-level))
	 (point (point)))
     (cl-loop while (reorg--goto-next-relative-level 1 nil start-level)
	      if (= (1+ start-level) (reorg-outline-level))
	      do ,@body
	      else if (/= start-level (reorg-outline-level))
	      return nil)
     (goto-char point)))

;;;; reorg new tree

(defun reorg--children-p ()
  "Does the current node have sub-branches?"
  (and (eq (reorg--get-view-props nil 'reorg-field-type) 'branch)
       (reorg--get-view-props nil 'reorg-data 'children)))




;; goto next header 
;; goto the next child
;; goto to last child 
;; goto next sibling
;; goto next level lower 
;; goto next last level lower


;;; inserting into branch





(defun reorg--insert-into-branch-or-make-new-branch (data &optional point)
  (let* ((children (reorg-into--get-list-of-child-branches-at-point)))
    (cl-loop with x = nil
	     with point = (or point (point))
	     do (setf (point) point)
	     for (func . results) in children
	     do (setq x (funcall func data)
		      point (point))
	     when x
	     do (if (member x results)
		    (progn 
		      (reorg-tree--goto-next-property-field nil 'reorg-data
							    x
							    nil
							    #'string=
							    (lambda (y) (alist-get 'headline y)))
		      (if (reorg--children-p)
			  (reorg--insert-into-branch-or-make-new-branch data)
			(reorg--insert-into-leaves data
						   (reorg--get-view-props nil 'reorg-data 'result-sorters))))
		  (reorg--insert-new-branch (list (cons 'branch-name x)
						  (cons 'headline x)
						  (cons 'reorg-branch t)
						  (cons 'result-sorters ,result-sorters)
						  (cons 'grouper-list ,func)
						  (cons 'grouper-list-results x)
						  (cons 'format-string 'xxx)
						  (cons 'result-sorters 'xxx)
						  (cons 'children 'xxx)
						  (cons 'branch-value 'xxx)
						  (cons 'reorg-level (reorg-current-level)))))
	     (reorg--insert-into-branch-or-make-new-branch data))))





(defun reorg-into--at-branch-p ()
  (eq (reorg--get-view-props nil 'reorg-field-type)
      'branch))

(defun reorg-into--descend-into-branch-at-point (data)
"IDK what to do."
(when (reorg-into--at-branch-p)   
  (pcase `(,(reorg--children-p) ,(reorg-into--member-of-this-header? data))
    (`(nil t) (reorg--insert-into-leaves data (reorg--get-view-props nil 'reorg-data 'results-sorters)))
    (`(t t) (reorg--goto-next-relative-level 1) (reorg--into--descend-into-branch-at-point))
    (`(t nil) (reorg--goto-next-relative-level 0) (reorg--into--descend-into-branch-at-point))
    (`(nil nil) (reorg--goto-next-relative-level 
		 )))))

(defun reorg-into--member-of-this-header? (data)
  "Is DATA a member of the header?" 
  (equal
   (funcall
    (car (last (reorg--get-view-props nil 'reorg-data 'grouper-list))) data)
   (car (last (reorg--get-view-props nil 'reorg-data 'grouper-list-results)))))

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


(defun reorg-tree--is-cloned-p ()
  (when-let ((id (reorg--get-view-prop 'id)))
    (setf (point) (point-min))
    (text-property-search-forward reorg--data-property-name
				  id
				  (lambda (val alist)
				    (string= 
				     (alist-get 'id alist)
				     val))
				  'not-current)))


(defun reorg-tree--is-folded-p ()
  "Is the current heading folded?"
  (let ((inhibit-field-text-motion t))
    (invisible-p (point-at-eol))))

(defun reorg-tree--is-root-p ()
  (= (reorg-outline-level 1)))

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
  (let* ((start-level (or start-level (reorg-outline-level)))
	 (point (point)))
    (cond  ((>= 0 (abs (+ (reorg-outline-level) (or relative-level 0))))
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
		      (current-level () (reorg-outline-level))
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
    (let ((level (reorg-outline-level))
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
    (let ((level (reorg-outline-level))
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

;; (defun reorg-tree--get-by-branch-predicate (prop val func)
;;   "Execute FUNC at each branch that has PROP equal to VAL and
;; make a list of the results."
;;   (let (results)
;;     (save-excursion
;;       (cl-loop with level = (reorg-outline-level)
;; 	       while (and (reorg-tree--goto-next-property-field nil
;; 								'reorg-data val nil #'equal
;; 								(lambda (x) (alist-get prop x)))
;; 			  (> (reorg-outline-level) level))
;; 	       do (cl-pushnew (funcall func) results :test #'equal)))
;;     (reverse results)))


;; (defun reorg-tree--branch-insert--find-location (data)
;; "insert into the branch at point."
;; (save-excursion
;;   (reorg-tree--goto-first-sibling-in-current-group)
;;   (let* ((branch-predicate (reorg--get-view-props nil 'reorg-data 'branch-predicate))
;; 	 (name (funcall branch-predicate data))
;; 	 (level (reorg-outline-level))
;; 	 (format-string (reorg--get-view-props nil 'reorg-data 'format-string))
;; 	 (branch-sorter (reorg--get-view-props nil 'reorg-data 'branch-sorter))
;; 	 (branch-sort-getter (reorg--get-view-props nil 'reorg-data 'branch-sort-getter))
;; 	 (existing-data (copy-tree (reorg--get-view-props)))
;; 	 (new-data (plist-put existing-data 'branch-name name)))

;;     (if (and branch-sort-getter branch-sorter)
;; 	(cl-loop when (funcall branch-sorter
;; 			       (funcall branch-sort-getter name)
;; 			       (funcall branch-sort-getter (reorg--get-view-props nil 'reorg-data 'branch-name)))
;; 		 return (reorg-tree--branch-insert--insert-heading new-data)
;; 		 while (reorg--goto-next-relative-level 0)
;; 		 finally return (reorg-tree--branch-insert--insert-heading new-data 'after))
;;       (reorg-tree--branch-insert--insert-heading new-data)))))

;; ;;
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

(cl-defun reorg--branch-insert--drop-into-outline (data template)
  (cl-labels
      ((doloop
	(data
	 template
	 &optional (n 0 np)
	 result-sorters
	 grouper-list
	 grouper-list-results
	 format-string
	 (level 1)
	 (before t))
	(let ((grouper `(lambda (x)
			  (let-alist x
			    ,(plist-get template :group))))
	      (children (plist-get template :children))
	      (heading-sorter (plist-get template :sort))
	      (heading-sort-getter (or (plist-get template :sort-getter)
				       #'car))
	      (format-string (or (plist-get template :format-string)
				 format-string
				 reorg-headline-format))
	      (result-sort (plist-get template :sort-results)))
	  (when result-sort
	    (setq result-sorters
		  (append result-sorters					  
			  (cl-loop for (form . pred) in result-sort
				   collect (cons `(lambda (x)
						    (let-alist x
						      ,form))
						 pred)))))
	  (let ((name (funcall grouper data))
		(members (reorg-tree--get-current-group-members)))
	    (when name
	      (if (member name members)
		  (unless (equal name (reorg--get-view-props nil 'reorg-data 'branch-name))
		    (reorg-tree--goto-next-property-field 'reorg-data name
							  nil #'equal (lambda (x) (alist-get 'branch-name x))))
		(if (and heading-sort-getter heading-sorter members)
		    (cl-loop with new-data = (list (cons 'name name)
						   (cons 'branch-name name)
						   (cons 'heading-sorter heading-sorter)
						   (cons 'heading-sort-getter heading-sort-getter)
						   (cons 'format-string format-string)
						   (cons 'level level)
						   (cons 'reorg-branch t)
						   (cons 'branch-predicate grouper))
			     when (funcall heading-sorter
					   (funcall heading-sort-getter name)
					   (funcall heading-sort-getter (reorg--get-view-props nil 'reorg-data 'branch-name)))
			     return (reorg-tree--branch-insert--insert-heading new-data)
			     while (reorg--goto-next-relative-level 0)
			     finally return (reorg-tree--branch-insert--insert-heading new-data))
		  (reorg-tree--branch-insert--insert-heading (list (cons 'name name)
								   (cons 'branch-name name)
								   (cons 'heading-sorter heading-sorter)
								   (cons 'heading-sort-getter heading-sort-getter)
								   (cons 'format-string format-string)
								   (cons 'level level)
								   (cons 'reorg-branch t)
								   (cons 'branch-predicate grouper))
							     (not before))))	  
	      (if children 
		  (cl-loop 
		   with before = nil
		   for x below (length children)
		   for marker in (save-excursion
				   (setq before (reorg--goto-next-relative-level 1))
				   (reorg-tree--get-sibling-group-markers))
		   do (goto-char marker)
		   and do (doloop
			   data
			   (nth x children)
			   x
			   result-sorters
			   nil
			   nil
			   format-string
			   (1+ level)
			   before))
		(reorg--insert-into-leaves data
					   result-sorters
					   (if (reorg-tree--branch-has-leaves-p)
					       (1+ level)
					     level)
					   format-string)
		(redraw-display)
		

		))))))


    (goto-char (point-min))
    (doloop data template)))

(defun reorg--goto-headline-start ()
  (save-match-data 
    (goto-char (org-entry-beginning-position))
    (re-search-forward "^\\*+[[:space:]]" nil t)
    (backward-char 1)
    (point)))

(defun reorg--get-headline-start ()
  (save-excursion (reorg--goto-headline-start)))

(provide 'reorg)
