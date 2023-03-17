;; -*- lexical-binding: t; -*-

;;; syncing function

(defun reorg-org--update-heading-at-point ()
  "update the current heading"
  (interactive)
  (reorg--insert-new-heading
   (reorg--with-point-at-orig-entry nil
				    nil
				    (reorg--parser
				     nil
				     (reorg--get-prop 'class)))))

(defmacro reorg-org--with-source-and-sync (&rest body)
  "Execute BODY in the source buffer and
update the heading at point."
  (declare (indent defun))
  `(progn
     (let* ((data nil)
	    (marker (reorg--get-prop 'marker))
	    (buffer (marker-buffer marker))
	    (id (reorg--get-prop 'id)))
       (org-with-remote-undo buffer
	 (with-current-buffer buffer 
	   (let ((old-point (point))
		 (search-invisible t))
	     (widen)
	     (ov-clear)
	     (goto-char marker)
	     ,@body
	     (setq data (reorg--parser nil 'org reorg--temp-parser-list)))
	   (with-current-buffer reorg-buffer-name
	     (save-excursion
	       (save-restriction
		 (reorg--delete-entries id)
		 (reorg--insert-new-heading data)))))))))

;;; org-capture integration 

(defun reorg-org-capture-hook ()
  "Org-capture hook function to that
runs and determines whether to insert the most
recently captured heading belongs in the outline."
  (let (data)
    (with-current-buffer 
	(reorg--org-capture-goto-last-stored)
      (setq data (reorg--parser nil 'org)))
    (with-current-buffer reorg-buffer-name
      ;; When the captured item belongs to a source
      ;; used to generate the outline
      (when (member (cons
		     (alist-get 'class data)
		     (abbreviate-file-name
		      (alist-get 'filename data)))
		    reorg--current-sources)
	;; try to insert it into the outline 
	(reorg--insert-new-heading data)))))

(defun reorg-org-capture (&optional goto keys)
  "Wrapper for org-capture to handle window
and buffer switching.  This should be called instead of
`org-capture'."
  ;;TODO re-write this as advice 
  (interactive "P")
  (let ((in-side-p (reorg--buffer-in-side-window-p))
	(in-reorg-buffer-p (string=
			    reorg-buffer-name
			    (buffer-name (current-buffer)))))
    (when in-side-p
      (reorg--toggle-tree-buffer))
    (org-capture goto keys)
    (when in-reorg-buffer-p
      (if in-side-p
	  (reorg--toggle-tree-buffer)
	(set-window-buffer reorg-buffer-name)))))

(defun reorg-org-capture-enable (&optional disable)
  "Set up reorg to process anything captured by
org-capture and insert it into the outline at the
appropriat point(s)."
  (interactive "P")
  (if disable
      (remove-hook 'org-capture-after-finalize-hook
		   #'reorg-org-capture-hook)  
    (add-hook 'org-capture-after-finalize-hook
	      #'reorg-org-capture-hook)))

(defun reorg-org-capture-disable ()
  "Disable org capture"
  (interactive)
  (reorg-org-capture-enable 'disable))

(defun reorg--org-capture-goto-last-stored ()
  "Same as org-capture-goto-last-stored, except it call
`reorg--org-goto-marker-or-bmk' instead of `org-goto-marker-or-bmk'."
  (interactive)
  (reorg--org-goto-marker-or-bmk
   org-capture-last-stored-marker
   (plist-get org-bookmark-names-plist
	      :last-capture)))

(defun reorg--org-goto-marker-or-bmk (marker &optional bookmark)
  "Like `org-goto-marker-or-bmk' except that it handle buffer
switching differently.  If a bookmark is found, return the
buffer in which the bookmark was found."
  (if (and marker (marker-buffer marker)
	   (buffer-live-p (marker-buffer marker)))
      (progn
	(with-current-buffer (marker-buffer marker)
	  (when (or (> marker (point-max)) (< marker (point-min)))
	    (widen))
	  (goto-char marker)
	  (org-show-context 'org-goto)
	  (current-buffer)))
    (if bookmark
	(progn 
	  (bookmark-jump bookmark)
	  (current-buffer))      
      (error "Cannot find location"))))

;;; navigation commands 

(defun reorg-org--open-agenda-day (&optional arg)
  "Open org-agenda if the heading at point
contains a timestamp."
  (interactive "P")
  (when (reorg--get-prop 'ts)
    (let ((date (list (reorg--get-prop 'ts-month-num)
		      (reorg--get-prop 'ts-day)
		      (string-to-number
		       (reorg--get-prop 'ts-year)))))
      (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))))

;;; Convenience functions / specialized parsing functions

(defvar reorg-org--org-link-regexp
  (rx
   "[["
   (group (+? not-newline))
   "]["
   (group (+? not-newline))
   "]]")
  "Org link regexp.")

(defun reorg-org--link-parser ()
  "the first link in the current heading and return an alist."
  (save-excursion 
    (let ((limit (or (save-excursion (when (re-search-forward
					    org-heading-regexp
					    nil t)
				       (point)))
		     (point-max))))
      (when (re-search-forward
	     ;; reorg-org--org-link-regexp
	     org-link-any-re
	     limit
	     t)
	(list 
	 (cons 'link (match-string-no-properties 1))
	 (cons 'text (match-string-no-properties 2)))))))

(defun reorg-org--all-link-parser ()
  "the first link in the current heading and return an alist."
  (save-excursion 
    (let ((limit (or (save-excursion (when (re-search-forward
					    org-heading-regexp
					    nil t)
				       (point)))
		     (point-max))))
      (cl-loop while (re-search-forward
		      org-any-link-re
		      limit
		      t)
	       collect (list 
			(cons 'link (match-string-no-properties 1))
			(cons 'text (match-string-no-properties 2)))))))

(defun reorg-org--ts-hhmm-p (ts)
  ""
  (string-match (rx (or (seq (** 1 2 digit)
			     ":"
			     (= 2 digit))
			(seq (** 1 2 digit)
			     (or "am"
				 "pm"
				 "AM"
				 "PM"))))
		ts))


(defun reorg-org--format-time-string (ts no-time-format &optional time-format)
  "Format a ts object.  I am honestly not sure what this is doing."
  (format-time-string
   (if (reorg-org--ts-hhmm-p ts)
       (or time-format no-time-format)
     no-time-format)
   (org-read-date nil t ts )))

(defun reorg-org--get-property-drawer ()
  "Get the property drawer of the heading at point as a plist."
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

(defun reorg-org--timestamp-parser (&optional inactive range)
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

(defun reorg-org--get-body ()
  "get headings body text"  
  ;; FIXME this adds way too much time to parsing the
  ;; org file.  A regexp would probably be faster
  ;; (org-no-properties
  ;;  (org-element-interpret-data
  ;;   (org-element--parse-elements (save-excursion (org-back-to-heading)
  ;; 						 (org-end-of-meta-data t)
  ;; 						 (point))
  ;; 				 (or (save-excursion (outline-next-heading))
  ;; 				     (point-max))
  ;; 				 'first-section nil nil nil nil)))
  (save-excursion
    (when (reorg-org--goto-end-of-meta-data)
      (string-trim 
       (buffer-substring-no-properties
	(point)
	(if (re-search-forward outline-regexp nil t)
	    (point-at-bol)
	  (point-max)))))))

;;; macros 

(defmacro reorg-org--with-restore-state (&rest body)
  "do BODY while saving, excursion, restriction, etc."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction       
       (widen)
       (let ((inhibit-field-text-motion t))
	 ,@body))))

(defmacro reorg-org--with-point-at-orig-entry (id buffer &rest body)
  "Execute BODY with point at the heading with ID at point."
  `(when-let ((id (or ,id (reorg--get-prop 'id))))
     (with-current-buffer (or ,buffer (reorg--get-prop 'buffer))
       (reorg-org--with-restore-state
	(goto-char (point-min))
	;; NOTE: Can't use `org-id-goto' here or it will keep the
	;;       buffer open after the edit.  Getting the buffer
	;;       and searching for the ID should ensure the buffer
	;;       stays hidden.  This avoids using `org-id'
	;;       for anything other than ID generation. 
	(save-match-data
	  (if (re-search-forward id)
	      (progn 
		,@body)
	    (error "Heading with ID %s not found." id)))))))

;;; render source func

(defun reorg-org--render-source (&optional buffer id no-narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (let ((marker (reorg--get-prop 'marker)))
    (reorg--select-main-window (marker-buffer marker))
    (widen)
    (org-show-all)
    (goto-char marker)
    (reorg-org--source--narrow-to-heading)
    (reorg--select-tree-window)))

;; (defun reorg--org--goto-source (&optional buffer id no-narrow)
;;   "Move to buffer and find heading with ID.  If NARROW is non-nil,
;; then narrow to that heading and return t.  If no heading is found, don't move
;; the point and return nil."
;;   (let ((id (or id (reorg--get-prop 'id))))
;;     (with-current-buffer (or buffer (reorg--get-prop 'buffer))
;;       (let ((old-point (point))
;; 	    (search-invisible t))
;; 	(widen)
;; 	(ov-clear)
;; 	(goto-char (point-min))
;; 	(if (re-search-forward id nil t)
;; 	    (progn (goto-char (match-beginning 0))
;; 		   (org-back-to-heading)
;; 		   (when (not no-narrow)
;; 		     (reorg-org--source--narrow-to-heading)))
;; 	  (goto-char old-point))))))

(defun reorg-org--goto-end-of-meta-data ()
  "Go to the end of the meta data and insert a blank line
if there is not one."
  (let ((next-heading (reorg-org--with-restore-state
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

(defun reorg-org--at-inline-task-p ()
  "at an inline task?"
  (and (org-at-heading-p)
       (not (org-with-limited-levels 
	     (org-at-heading-p)))))

(defun reorg-org--source--narrow-to-heading ()
  "Narrow to the current heading only, i.e., no subtree."
  (org-back-to-heading)     
  (unless (reorg-org--at-inline-task-p)
    (org-narrow-to-element))
  (reorg-org--goto-end-of-meta-data))

;;; edit commands 

(defun reorg-org--org-edit-headline (&optional arg)
  "Edit the headline at point"
  (interactive "P")
  (reorg-org--with-source-and-sync 
    (org-edit-headline (read-string "New headline: "
				    (org-get-heading t t t t)))))

(defun reorg-org--org-todo (&optional arg)
  "Edit todo state at point"
  (interactive "P")
  (reorg-org--with-source-and-sync
    (reorg--select-main-window)
    (funcall-interactively #'org-todo arg)))

(defun reorg-org--org-set-tags-command (&optional arg)
  "set tags at point"
  (interactive "P")
  (reorg-org--with-source-and-sync
    (funcall-interactively #'org-set-tags-command arg)))

(defun reorg-org--org-deadline (&optional arg)
  "set deadline at point"
  (interactive "P")
  (reorg-org--with-source-and-sync
    (funcall-interactively #'org-deadline arg)))

(defun reorg-org--org-schedule (&optional arg)
  "set scheduled at point"
  (interactive "P")
  (reorg-org--with-source-and-sync
    (funcall-interactively #'org-schedule arg)))

(defun reorg-org--org-set-property (&optional arg)
  "set property at point"
  (interactive )
  (reorg-org--with-source-and-sync
    (funcall-interactively #'org-set-property nil nil)))

(defun reorg-org--org-priority (&optional arg)
  "set priority at point"
  (interactive "P")
  (reorg-org--with-source-and-sync
    (funcall-interactively #'org-priority arg)))

(defun reorg-org--reload-heading (&optional arg)
  "reload heading at point"
  (interactive)
  (reorg-org--with-source-and-sync))

;;; org class 

(reorg-create-class-type
 :name org
 :render-func reorg-org--render-source
 :keymap (("SPC" . reorg-org--open-agenda-day)
	  ("h" . reorg-org--org-edit-headline)
	  ("t" . reorg-org--org-todo)
	  ("a" . reorg-org--org-set-tags-command)
	  ("d" . reorg-org--org-deadline)
	  ("s" . reorg-org--org-schedule)
	  ("r" . reorg-org--org-set-property)
	  ("i" . reorg-org--org-priority)
	  ("g" . reorg-org--reload-heading))
 ;; :getter (with-current-buffer (find-file SOURCE)
 ;; 	   (widen)
 ;; 	   (org-map-entries #'PARSER)))
 :getter
 (progn
   (when (functionp SOURCE)
     (setq SOURCE (funcall SOURCE)))
   (setq org-ql-cache (make-hash-table :weakness 'key))
   (org-ql-select SOURCE nil :action #'PARSER)))

;;; org data 

(reorg-create-data-type
 :name delegatee
 :class org
 :parse (org-entry-get (point) "DELEGATEE"))

(reorg-create-data-type
 :name tag-list
 :class org
 :parse (org-get-tags))

(reorg-create-data-type
 :name ts-pretty
 :class org
 :parse (when-let ((ts (or
			(org-entry-get (point) "DEADLINE")
			(reorg-org--timestamp-parser)
			(org-no-properties
			 (reorg-org--timestamp-parser nil t))
			(org-entry-get (point) "SCHEDULED"))))
	  (if (=
	       (string-to-number
		(format-time-string "%Y"))
	       (ts-year (ts-parse-org ts)))
	      (s-pad-right 22 " "
			   (reorg-org--format-time-string
			    ts
			    "%a, %b %d"
			    "%a, %b %d at %-l:%M%p"))
	    (s-pad-right 22 " "
			 (reorg-org--format-time-string
			  ts
			  "%a, %b %d, %Y"
			  "%a, %b %d, %Y at %-l:%M%p")))))


(reorg-create-data-type
 :name ts
 :class org
 :parse (or
	 (org-entry-get (point) "DEADLINE")
	 (when (reorg-org--timestamp-parser)
	   (org-no-properties (reorg-org--timestamp-parser)))
	 (when (reorg-org--timestamp-parser nil t)
	   (org-no-properties (reorg-org--timestamp-parser nil t)))
	 (org-entry-get (point) "SCHEDULED")))

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

;; (reorg-create-data-type
;;  :name ts
;;  :class org
;;  :parse (or
;; 	 (org-entry-get (point) "DEADLINE")
;; 	 (when (reorg-org--timestamp-parser)
;; 	   (org-no-properties (reorg-org--timestamp-parser)))
;; 	 (when (reorg-org--timestamp-parser nil t)
;; 	   (org-no-properties (reorg-org--timestamp-parser nil t)))
;; 	 (org-entry-get (point) "SCHEDULED"))
;;  :display (if-let ((ts (alist-get 'ts data)))
;; 	      (if (=
;; 		   (string-to-number
;; 		    (format-time-string "%Y"))
;; 		   (ts-year (ts-parse-org ts)))
;; 		  (s-pad-right 22 " "
;; 			       (reorg-org--format-time-string
;; 				ts

;; 				"%a, %b %d"
;; 				"%a, %b %d at %-l:%M%p"))
;; 		(s-pad-right 22 " "
;; 			     (reorg-org--format-time-string
;; 			      ts
;; 			      "%a, %b %d, %Y"
;; 			      "%a, %b %d, %Y at %-l:%M%p")))
;; 	    nil))

(reorg-create-data-type
 :name timestamp-all
 :class org
 :parse (or
	 (org-entry-get (point) "DEADLINE")
	 (reorg-org--timestamp-parser)
	 (org-no-properties (reorg-org--timestamp-parser
			     nil t))
	 (org-entry-get (point) "SCHEDULED")))

(reorg-create-data-type
 :name priority
 :class org
 :parse (org-entry-get (point) "PRIORITY")
 :display (pcase (alist-get 'priority data)
	    ("A" "⚡")
	    ("B" "⇥")
	    ("C" "⬊")
	    (_ " ")))

(reorg-create-data-type :name body
			:class org
			:parse (reorg-org--get-body))

(reorg-create-data-type
 :name deadline
 :class org
 :parse (org-entry-get (point) "DEADLINE")
 :display (when (alist-get 'deadline data)
	    (string-pad 
	     (ts-format "%B %e, %Y" ;
			(ts-parse-org
			 (alist-get 'deadline data)))
	     18
	     nil t)))

(reorg-create-data-type
 :name scheduled
 :class org 
 :parse (org-entry-get (point) "SCHEDULED")
 :display (if (alist-get 'scheduled data)
	      (concat 
	       (propertize "SCHEDULED: "
			   'font-lock-face
			   'org-special-keyword)
	       (propertize (alist-get 'scheduled data)
			   'font-lock-face 'org-date))
	    "__________"))

(reorg-create-data-type
 :name headline
 :class org
 :parse (org-no-properties
	 (org-get-heading t t t t)))

(reorg-create-data-type
 :name tags
 :class org
 :parse (org-get-tags-string))

(reorg-create-data-type
 :name todo
 :class org
 :parse (org-entry-get (point) "TODO")
 :display (when-let ((s (alist-get 'todo data)))
	    (propertize
	     s
	     'font-lock-face
	     (org-get-todo-face s))))

(reorg-create-data-type
 :name timestamp
 :class org
 :parse (when (reorg-org--timestamp-parser)
	  (org-no-properties (reorg-org--timestamp-parser)))
 :display
 (if (alist-get 'timestamp data)
     (concat 
      (propertize (alist-get 'timestamp data)
		  'font-lock-face 'org-date))
   "____"))

(reorg-create-data-type
 :name links
 :class org
 :parse (reorg-org--all-link-parser))

(reorg-create-data-type
 :name link
 :class org
 :parse (reorg-org--link-parser))

(reorg-create-data-type
 :name link-file-name
 :class org
 :parse (when-let* ((data (reorg-org--link-parser))
		    (path (alist-get 'link data))
		    (name (f-filename path)))
	  (car (s-split "::" name))))

(reorg-create-data-type
 :name link-file-path
 :class org
 :parse (when-let* ((data (reorg-org--link-parser))
		    (data (alist-get 'link data))
		    (data (cadr (s-split (rx (one-or-more alnum)
					     ":/")
					 data)))
		    (data (car (s-split "::" data))))
	  (concat "/" data)))

(reorg-create-data-type
 :name marker
 :class org
 :parse (point-marker))

(reorg-create-data-type
 :name timestamp-ia
 :class org
 :parse (when (reorg-org--timestamp-parser t)
	  (org-no-properties (reorg-org--timestamp-parser t))))

(reorg-create-data-type
 :name timestamp-ia-range
 :class org
 :parse (when (reorg-org--timestamp-parser t t)
	  (org-no-properties (reorg-org--timestamp-parser t t))))

(reorg-create-data-type
 :name timestamp-range
 :class org
 :parse (when (reorg-org--timestamp-parser nil t)
	  (org-no-properties (reorg-org--timestamp-parser nil t))))

(reorg-create-data-type
 :name id
 :class org
 ;; :parse (org-id-new))
 :parse (org-id-get-create))

(reorg-create-data-type
 :name category-inherited
 :class org
 :parse (org-entry-get-with-inheritance "CATEGORY"))

(reorg-create-data-type
 :name category
 :class org
 :parse (org-get-category))

(reorg-create-data-type
 :name filename
 :class org
 :parse (f-filename (buffer-file-name)))

(reorg-create-data-type
 :name buffer-name
 :class org
 :parse (buffer-name))

(reorg-create-data-type
 :name buffer
 :class org
 :parse (current-buffer))

(reorg-create-data-type
 :name order
 :class org
 :parse (point))

(reorg-create-data-type
 :name org-level
 :class org
 :parse (org-current-level))

(reorg-create-data-type
 :name root
 :class org
 :parse (save-excursion (while (org-up-heading-safe))
			(org-no-properties
			 (org-get-heading t t t t))))
(reorg-create-data-type
 :name root-ts-inactive
 :class org
 :parse (save-excursion
	  (cl-loop while (org-up-heading-safe)
		   when (reorg-org--timestamp-parser t nil)
		   return (reorg-org--timestamp-parser t nil))))

(reorg-create-data-type
 :name at-names
 :class org
 :parse (let ((headline (org-get-heading t t t t)))
	  (cl-loop
	   with start = 0
	   while (setq start
		       (and
			(string-match "@\\([[:word:]]+\\)" headline start)
			(match-end 1)))
	   collect (match-string-no-properties 1 headline))))

(reorg-create-data-type
 :name ts-year
 :class org
 :parse (when .ts-ts
	  (number-to-string (ts-year .ts-ts))))

(reorg-create-data-type
 :name ts-month
 :class org
 :parse (when .ts-ts
	  (ts-month-name .ts-ts)))

(reorg-create-data-type
 :name ts-month-num
 :class org
 :parse (when-let ((ts (alist-get 'ts-ts DATA)))
	  (ts-month ts)))

(reorg-create-data-type
 :name ts-day
 :class org
 :parse (when .ts-ts
	  (ts-day .ts-ts)))

(reorg-create-data-type
 :name ts-day-name
 :class org
 :parse (when .ts-ts
	  (ts-day-name .ts-ts)))

(reorg-create-data-type
 :name timestamp-type 
 :class org
 :parse (cond 
	 ((org-entry-get (point) "DEADLINE") "deadline")
	 ((reorg-org--timestamp-parser) "active")
	 ((org-no-properties (reorg-org--timestamp-parser nil t)) "range")
	 ((org-entry-get (point) "SCHEDULED") "scheduled"))
 :display (pcase (alist-get 'ts-type data)
	    ("deadline" "≫")
	    ("active" "⊡")
	    ("range" "➥")
	    ("scheduled" "⬎")
	    (_ " ")))

(reorg-create-data-type
 :name ts-ts
 :class org
 :parse (when .ts-any
	  (ts-parse-org .ts-any)))

;; (reorg-create-data-type
;;  :name all-active-timestamps
;;  :class org
;;  :parse (save-excursion
;; 	  (cl-loop
;; 	   while (re-search-forward org-ql-regexp-ts-active
;; 				    (save-excursion (outline-next-heading))
;; 				    t)
;; 	   collect (match-string-no-properties 0))))

(reorg-create-data-type
 :name ts-any
 :class org
 :parse (or 
	 (org-entry-get (point) "DEADLINE")
	 (reorg-org--timestamp-parser) ;; active timestamp
	 (org-no-properties (reorg-org--timestamp-parser nil t)) ;; active range
	 (org-entry-get (point) "SCHEDULED")
	 (org-no-properties (reorg-org--timestamp-parser t nil))
	 (org-no-properties (reorg-org--timestamp-parser t t))))

(provide 'reorg-org)
