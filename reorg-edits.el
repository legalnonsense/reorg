;;; -*- lexical-binding: t; -*-

;;;;;

(defmacro reorg--with-point-at-orig-entry (&rest body)
  "Execute BODY with point at the heading with ID at point."
  (declare (indent defun))
  `(let ((id (reorg--get-view-prop :id))
	 (marker (reorg--get-view-prop :marker))
	 (buffer (reorg--get-view-prop :buffer)))
     (with-current-buffer buffer
       (org-with-wide-buffer 
	(goto-char (point-min))
	;; NOTE: Can't use `org-id-goto' here or it will keep the
	;;       buffer open after the edit.  Getting the buffer
	;;       and searching for the ID should ensure the buffer
	;;       stays hidden.
	(if reorg-parser-use-id-p 
	    (if (save-match-data (re-search-forward id nil t))
		(progn 
		  ,@body)
	      (error "Heading with ID %s not found." id))
	  (goto-char (marker-position marker))
	  ,@body)))))

(defun reorg--modification-hook-func (overlay postp beg end &optional length)
  "overlay post change hook."
  (when postp
    (save-match-data
      (let* ((overlay-beg (overlay-start overlay))
	     (headline-beg (reorg--get-headline-start))
	     (relative-beg (if (<= (- beg headline-beg) 0)
			       0
			     (- beg headline-beg)))
	     (adjustment (if (< beg overlay-beg)
			     (- beg overlay-beg)
			   0)))
	(cond
	 ((and (= beg end) (> length 0))
	  (reorg--with-point-at-orig-entry
	    (reorg--goto-headline-start)
	    (forward-char relative-beg)
	    (delete-region (point) (+ (point) length))))

	 
	 ((and (/= beg end) (> length 0))
	  (let ((s (buffer-substring-no-properties (+ overlay-beg
						      relative-beg) end)))
	    (message s)
	    (reorg--with-point-at-orig-entry
	      (reorg--goto-headline-start)
	      (forward-char relative-beg)
	      (delete-region (point) (+ (point)
					(+ length adjustment)))
	      (insert s))))

	 
	 ((or (= length 0) (/= beg end))
	  (let ((s (buffer-substring-no-properties beg end)))
	    (reorg--with-point-at-orig-entry
	      (reorg--goto-headline-start)
	      (forward-char relative-beg)
	      (insert s)))))))))

;;;;;

(defvar-local reorg-edits--restore-state nil
  "When editing a clone, save the current headline and body
  to restore if the edit is abandoned.")

(defvar-local reorg-edits--previous-header-line nil
  "previous header line")

(defcustom reorg-edits-commit-edit-shortcut "C-c C-c"
  "Shortcut to commit edits when in `reorg-edits-mode'
Accepts any string acceptable to `kbd'."
  :type 'string)

(defcustom reorg-edits-abort-edit-shortcut "C-c C-k"
  "Shortcut to abort edits when in `reorg-edits-mode'
Accepts any string acceptable to `kbd'."
  :type 'string)

(defcustom reorg-edits-start-edit-shortcut "C-c C-c"
  "Shortcut to initiate `reorg-edits-mode'
Accepts any string acceptable to `kbd'."
  :type 'string)

(defvar reorg-edits--header-line
  '(:eval
    (format 
     "Editing headline. '%s' to finish and update. '%s' to abandon."
     reorg-edits-commit-edit-shortcut
     reorg-edits-abort-edit-shortcut))
  "The value of header-line-format when `reorg-edits-mode' is 
invoked.")

(defvar reorg-edits--current-field-overlay
  (let ((overlay (make-overlay 1 2)))
    (overlay-put overlay 'face '(:box t))
    (overlay-put overlay 'priority 1000)
    overlay)
  "Overlay for field at point.")

;;;;;;;;;;;;;;;;;;;

(defun reorg-edits--post-field-navigation-function ()
  "Tell the user what field they are on."
  (when-let ((field (get-text-property (point) reorg--field-property-name)))
    (move-overlay reorg-edits--current-field-overlay
		  (car (reorg-edits--get-field-bounds))
		  (cdr (reorg-edits--get-field-bounds)))
    (message "You are on the field for the heading's %s"
	     (get-text-property (point) reorg--field-property-name))))

(defun reorg-edits-move-to-next-field (&optional previous)
  "Move to the next field at the current heading."
  (interactive)
  (cl-loop with point = (point)
	   while (setq point (funcall (if previous
					  #'previous-single-property-change
					#'next-single-property-change)
				      point
				      reorg--field-property-name
				      nil
				      (if previous 
					  (org-entry-beginning-position)
					(org-entry-end-position))))
	   until (get-text-property point reorg--field-property-name)
	   finally (goto-char point))
  (reorg-edits--post-field-navigation-function))

(defun reorg-edits-move-to-previous-field ()
  "Move to the next field at the current heading."
  (interactive)  
  (reorg-edits-move-to-next-field 'previous))

(defun reorg-edit-field--replace-field (field val)
  "Replace FIELD with VAL."
  ;;; TODO
  )

(defun reorg-edits--keep-point-in-range (&optional start end)
  "Temporary post-command-hook to keep the point between
two markers." 
  (when (and (or reorg-edit-field--start-marker start)
	     (or reorg-edit-field--end-marker end))
    (cond ((< (point) reorg-edit-field--start-marker)
	   (goto-char reorg-edit-field--start-marker))
	  ((> (point) reorg-edit-field--end-marker)
	   (goto-char reorg-edit-field--end-marker)))))

(defun reorg-edits--get-field-value ()
  "Get the value of the field at point.  Return 
nil if there is no value." 
  (pcase-let ((`(,start . ,end) (reorg-edits--get-field-bounds)))
    (when (and start end)
      (buffer-substring-no-properties start end))))

;; (defun reorg-edits--commit-edit ()
;;   "Discard the current edit and restore the node
;; to its previous state, and turn off the minor mode."
;;   (interactive)
;;   (reorg-edit-field--replace-field field val)
;;   (reorg-edits-mode -1))

;; (defun reorg-edits--discard-edit ()
;;   "Discard the current edit and restore the node
;; to its previous state, and turn off the minor mode."
;;   (interactive)
;;   (reorg-edit-field--replace-field
;;    reorg-edits--restore-state)
;;   (setq reorg-edits--restore-state nil)
;;   (reorg-edits-mode -1)
;;   (message "Discarded edit."))

(defun reorg-edits--start-edit ()
  "Start editing the headline at point."
  (interactive)
  (reorg-edits-mode 1))

(defun reorg-edits--at-editable-field-p ()
  "Is the point at an editable field?"
  (get-text-property (point) reorg--field-property-name))

(defun reorg-edits--move-into-field ()
  "If the point is at the border of a field, then 
move it into the field.  This ensures that `reorg-edits--get-field-bounds'
returns the correct positions."
  (cond ((and (get-text-property (point) reorg--field-property-name)
	      (not (get-text-property (1- (point)) reorg--field-property-name)))
	 (forward-char 1))
	((and (get-text-property (point) reorg--field-property-name)
	      (not (get-text-property (1+ (point)) reorg--field-property-name)))
	 (forward-char -1))))

(defun reorg-edits--get-field-bounds () 
  "Return the bounds of the field at point as a cons cell
(start . end). If the point is not at a field, return nil."
  (when (reorg-edits--at-editable-field-p)
    (save-excursion 
      (reorg-edits--move-into-field)
      (cons (previous-single-property-change (point)
					     reorg--field-property-name
					     nil
					     (org-entry-beginning-position))
	    (next-single-property-change (point)
					 reorg--field-property-name
					 nil
					 (org-entry-end-position))))))

(defvar reorg-edits-field-mode-map
  (let ((map (copy-keymap org-mode-map)))
    (define-key map (kbd reorg-edits-commit-edit-shortcut)
      #'reorg-edits--commit-edit)
    (define-key map (kbd reorg-edits-abort-edit-shortcut)
      #'reorg-edits--discard-edit)
    (define-key map (kbd "TAB") #'reorg-edits-move-to-next-field)
    (define-key map (kbd "BACKTAB") #'reorg-edits-move-to-previous-field)
    map)
  "keymap.")

(define-minor-mode reorg-edits-mode
  "Minor mode to edit headlines."
  :init-value nil
  :lighter " EDITING HEADLINE"
  :keymap reorg-edits-field-mode-map
  :body (if reorg-edits-mode
	    ;; on
	    (when-let* ((prop (get-text-property (point) reorg--field-property-name))
			(start (previous-single-property-change (point) reorg--field-property-name))
			(end (next-single-property-change (point) reorg--field-property-name))
			(val (buffer-substring start end)))
	      (goto-char (org-meta--goto-start))
	      (add-hook 'post-command-hook #'org-meta-view--keep-point-in-range nil t)		 
	      (setq cursor-type 'bar
		    reorg-edits--previous-header-line header-line-format
		    header-line-format reorg-edits--header-line
		    reorg-edits--restore-state
		    reorg-edit-field--start-marker (set-marker (make-marker) start)
		    reorg-edit-field--end-marker (set-marker (make-marker) end)
		    overriding-local-map reorg-edits-field-mode-map))
	  (when-let* ((prop (get-text-property (point) reorg--field-property-name))
		      (start (previous-single-property-change (point) reorg--field-property-name))
		      (end (next-single-property-change (point) reorg--field-property-name))
		      (val (buffer-substring start end)))
	    (reorg-edit-field--set-field prop val)
	    (remove-hook 'post-command-hook #'org-meta-view--keep-point-in-range t)
	    (delete-overlay reorg-edits--current-field-overlay)
	    (setq header-line-format org-meta--previous-header-line
		  reorg-edits--previous-header-line nil
		  reorg-edit-field--start-marker nil
		  reorg-edit-field--end-marker nil
		  reorg-edits--restore-state nil
		  overriding-local-map nil
		  cursor-type nil))))

(provide 'reorg-edits)
