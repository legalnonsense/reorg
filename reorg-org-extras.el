;; -*- lexical-binding: t; -*-

;;; syncing

(defmacro reorg-view--make-change-in-org-buffer-and-sync-clones (&rest body)
  "asdf"
  `(let (data)
     (reorg--with-point-at-orig-entry nil nil
				      ,@body
				      (setq data (reorg--parser)))
     (reorg--map-id (plist-get data :id)
		    (reorg-views--replace-heading data))))

(defmacro reorg--with-point-at-orig-entry (id buffer &rest body)
  "Execute BODY with point at the heading with ID at point."
  `(when-let ((id (or ,id (reorg--get-view-prop :id))))
     (with-current-buffer (or ,buffer (reorg--get-view-prop :buffer))
       (reorg--with-restore-state
	(goto-char (point-min))
	;; NOTE: Can't use `org-id-goto' here or it will keep the
	;;       buffer open after the edit.  Getting the buffer
	;;       and searching for the ID should ensure the buffer
	;;       stays hidden.  It also avoids using `org-id'
	;;       for anything other than ID generation. 
	(save-match-data
	  (if (re-search-forward id)
	      (progn 
		,@body)
	    (error "Heading with ID %s not found." id)))))))

;;; shortcuts

(defmacro reorg--create-org-shortcut (name func shortcut)
  (progn
    (let ((func-name (intern (concat "reorg--org-shortcut-"
				     (symbol-name name)))))
      `(progn
	 (defun ,func-name (arg)
	   ,(format "Execute %s in the source buffer and update the heading at point."
		    func)
	   (interactive "P")
	   (reorg--with-source-and-sync
	     (if (eq 'org-set-property ',func)
		 (funcall-interactively #',func arg nil)
	       (funcall-interactively #',func arg))))
	 (define-key reorg-view-mode-map (kbd ,shortcut) #',func-name)))))

(reorg--create-org-shortcut tag org-set-tags-command "C-c C-c")
(reorg--create-org-shortcut todo org-todo "C-c C-t")
(reorg--create-org-shortcut
 headline
 (lambda (_arg) (org-edit-headline (read-string "New headline: "
						(org-get-heading t t t t))))
 "C-c C-e")
(reorg--create-org-shortcut deadline org-deadline "C-c C-d")
(reorg--create-org-shortcut schedule org-schedule "C-c C-s")
(reorg--create-org-shortcut property org-set-property "C-c C-x p")
(reorg--create-org-shortcut priority org-priority "C-c C-p")


;;; helpers 

(defun reorg--timestamp-parser (&optional inactive range)
  "Find the fist timestamp in the current heading and return it. 
if INACTIVE is non-nil, get the first inactive timestamp.  If 
RANGE is non-nil, only look for timestamp ranges."
  (save-excursion
    (cl-loop while (re-search-forward (pcase `(,inactive ,range)
					(`(nil t)
					 org-tr-regexp)
					(`(nil nil)
					 org-ts-regexp)
					(`(t nil)
					 org-ts-regexp-inactive)
					(`(t t)
					 (concat 
					  org-ts-regexp-inactive
					  "--?-?"
					  org-ts-regexp-inactive)))
				      (org-entry-end-position)
				      t)
	     when (save-match-data (not (eq (car (org-element-at-point))
					    'planning)))
	     return (org-no-properties (match-string 0)))))

(defun reorg--get-body ()
  "get headings body text"
  (org-element-interpret-data
   (org-element--parse-elements (save-excursion (org-back-to-heading)
						(org-end-of-meta-data t)
						(point))
				(or (save-excursion (outline-next-heading))
				    (point-max))
				'first-section nil nil nil nil)))

;; (defun reorg--parser ()
;;   "Create a plist using `reorg-parser-list' for each org heading."
;;   (cl-loop with result = nil
;; 	   for (name . func) in reorg-parser-list
;; 	   append (list (reorg--add-remove-colon name) (funcall func)) into result
;; 	   finally return result))


(defcustom reorg-mapping-function #'reorg--map-entries "mapping function")
;; (setq reorg-mapping-function #'reorg--org-ql)

(defun reorg--map-entries (&optional match scope &rest skip)
  "Run the parser at each heading in the current buffer.
See `org-map-entries' for explanation of the parameters."
  (reorg--with-restore-state   
   (org-map-entries
    #'reorg--parser match scope skip)))

(defun reorg--org-ql ()
  (interactive)
  (org-ql-select nil nil
    :action #'reorg--parser))


(defun reorg--ts-hhmm-p (ts)
  (string-match (rx (or (seq (** 1 2 digit)
			     ":"
			     (= 2 digit))
			(seq (** 1 2 digit)
			     (or "am"
				 "pm"
				 "AM"
				 "PM"))))
		ts))

(defun reorg--ts-hhmm-p (ts)
  (string-match (rx (or (seq (** 1 2 digit)
			     ":"
			     (= 2 digit))
			(seq (** 1 2 digit)
			     (or "am"
				 "pm"
				 "AM"
				 "PM"))))
		ts))

(defun reorg--format-time-string (ts no-time-format &optional time-format)
  (format-time-string
   (if (reorg--ts-hhmm-p ts)
       (or time-format no-time-format)
     no-time-format)
   (org-read-date nil t ts )))

(defun reorg-parser--get-property-drawer ()
  "asdf"
  (save-excursion
    (org-back-to-heading)
    (let (seen-base props)
      (while (re-search-forward org-property-re (org-entry-end-position) t)
	(let* ((key (upcase (match-string-no-properties 2)))
	       (extendp (string-match-p "\\+\\'" key))
	       (key-base (if extendp (substring key 0 -1) key))
	       (value (match-string-no-properties 3)))
	  (cond
	   ((member-ignore-case key-base org-special-properties))
	   (extendp
	    (setq props
		  (org--update-property-plist key value props)))
	   ((member key seen-base))
	   (t (push key seen-base)
	      (let ((p (assoc-string key props t)))
		(if p (setcdr p (concat value " " (cdr p)))
		  (unless (or (null key)
			      (equal "" key)
			      (equal "PROPERTIES" key)
			      (equal "END" key))
		    (setq props (append (list
					 (reorg--add-remove-colon (intern (downcase key)))
					 value)
					props)))))))))
      props)))


;;;; headlines

(defun reorg--goto-headline-start ()
  (save-match-data 
    (goto-char (org-entry-beginning-position))
    (re-search-forward "^\\*+[[:space:]]" nil t)
    (backward-char 1)
    (point)))

(defun reorg--get-headline-start ()
  (save-excursion (reorg--goto-headline-start)))


(defmacro reorg--with-restore-state (&rest body)
  "do BODY while saving, excursion, restriction, etc."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction       
       (widen)
       (let ((inhibit-field-text-motion t))
	 ,@body))))
(provide 'reorg-org-extras)
