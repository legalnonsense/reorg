;; -*- lexical-binding: t; -*-

(require 'reorg)			

;;; syncing function

;; (defun reorg-org--update-heading-at-point ()
;;   "update the current heading"
;;   (interactive)
;;   (reorg--insert-new-heading
;;    (reorg--with-point-at-orig-entry nil
;; 				    nil
;; 				    (reorg--parser
;; 				     nil
;; 				     (reorg--get-prop 'class)))))

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
	     (reorg--delete-entries id)
	     (reorg--insert-new-heading data))))
       (run-hooks 'reorg--navigation-hook))))

;;; org-capture integration 

(defun reorg-org-capture-hook ()
  "Org-capture hook function to that
runs and determines whether to insert the most
recently captured heading belongs in the outline."
  (let (data)
    (with-current-buffer 
	(reorg--org-capture-goto-last-stored)
      (setq data (reorg--parser nil 'org reorg--temp-parser-list)))
    (with-current-buffer reorg-buffer-name
      ;; When the captured item belongs to a source
      ;; used to generate the outline
      (when (member (cons
		     (alist-get 'class data)
		     (alist-get 'buffer-file-name data))
		    reorg--current-sources)
	;; try to insert it into the outline 
	(reorg--insert-new-heading data)))))

(defun reorg-org-capture-enable (&optional disable)
  "Set up reorg to process anything captured by
org-capture and insert it into the outline at the
appropriat point(s)."
  (interactive "P")
  (remove-hook 'org-capture-after-finalize-hook
	       #'reorg-org-capture-hook)
  (unless disable 
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
  "Like `org-goto-marker-or-bmk' except that it handles buffer
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

;; (defun reorg-org--open-agenda-day (&optional arg)
;;   "Open org-agenda if the heading at point
;; contains a timestamp."
;;   (interactive "P")
;;   (when (reorg--get-prop 'ts)
;;     (let ((date (list (reorg--get-prop 'ts-month-num)
;; 		      (reorg--get-prop 'ts-day)
;; 		      (string-to-number
;; 		       (reorg--get-prop 'ts-year)))))
;;       (org-agenda-list nil (calendar-absolute-from-gregorian date) 'day))))

;;; Convenience functions / specialized parsing functions

(defvar reorg-org--org-link-regexp
  (rx
   "[["
   (group (+? not-newline))
   "]["
   (group (+? not-newline))
   "]]")

  "Org link regexp.")

(defun reorg-org--link-parser (&optional heading-only)
  "the first link in the current heading and return an alist."
  (save-excursion
    (org-back-to-heading)
    (cl-loop while (re-search-forward
		    org-link-any-re
		    (if heading-only (point-at-eol)
		      (org-entry-end-position))
		    t)
	     collect 
	     (list
	      (cons 'active-dir default-directory)
	      (cons 'element (org-element-context))
	      (cons 'dest (match-string-no-properties 2))
	      (cons 'text (match-string-no-properties 3))))))

(defun reorg-org--ts-hhmm-p (ts)
  "does TS have a time specification?"
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
  "Format a timestamp string.  NO-TIME-FORMAT is the format to use if there is
no HHMM specification.  TIME-FORMAT is used if there is an HHMM specification."
  (format-time-string
   (if (reorg-org--ts-hhmm-p ts)
       (or time-format no-time-format)
     no-time-format)
   (org-read-date nil t ts)))

(defun reorg-org--get-property-drawer ()
  "Get the property drawer of the heading at point as an alist."
  ;; I must have stolen this from somehwere but I don't know where.
  ;; ISN'T THERE A BUILT IN WAY TO DO THIS? 
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
		    (setq props (append (list (cons
					       (intern (downcase key))
					       value))
					props)))))))))
      props)))

(defun reorg-org--timestamp-parser (type &optional all)
  "TYPE can be deadline, scheduled, closed, active,
active-range, inactive, inactive-range.

If active or inactive timestamps, return only the first result.
If ALL is non-nil, then return a list of all results.

Planning timestamps (i.e., deadline, scheduled, and closed) are not
included in active or inactive timestamps.

If TYPE is 'all, then return a list of all timestamps in the headling.

If ALL is non-nil and TYPE is active or inactive, then also return
the opening and closing dates for any time ranges."
  (if (eq type 'all)
      (seq-filter #'identity
		  (cl-loop for each
			   in '((deadline)
				(scheduled)
				(closed)
				(active t)
				(inactive t))
			   append
			   (ensure-list
			    (apply #'reorg-org--timestamp-parser
				   each))))
    (save-excursion
      (cl-loop
       while (re-search-forward
	      (pcase type
		('deadline
		 org-deadline-time-regexp)
		('scheduled
		 org-scheduled-time-regexp)
		('closed
		 org-closed-time-regexp)
		('active-range
		 org-tr-regexp)
		('active
		 org-ts-regexp)
		('inactive
		 org-ts-regexp-inactive)
		('inactive-range
		 (concat 
		  org-ts-regexp-inactive
		  "--?-?"
		  org-ts-regexp-inactive))
		(_ (error "Invalid type %s" type)))
	      (org-entry-end-position)
	      t)
       when (or (and (not (member type
				  '(deadline scheduled closed)))
		     (save-match-data
		       (not
			(eq
			 (car (org-element-at-point))
			 'planning))))
		(member type '(deadline scheduled closed)))
       if all
       collect (org-no-properties (--> (match-string
					(if (member type
						    '(scheduled
						      deadline
						      closed))
					    1 0))
				       (cond ((member type
						      '(scheduled
							deadline))				     
					      (concat "<" it ">"))
					     ((eq type 'closed)
					      (concat "[" it "]"))
					     (t it))))
       
       into results
       and do (goto-char (match-end 0))
       else 
       return (org-no-properties (--> (match-string
				       (if (member type
						   '(scheduled
						     deadline
						     closed))
					   1 0))
				      (cond ((member type
						     '(scheduled
						       deadline))				     
					     (concat "<" it ">"))
					    ((eq type 'closed)
					     (concat "[" it "]"))
					    (t it))))
       finally return results))))


;; (defun reorg-org--timestamp-parser (&optional inactive range)
;;   "Find the fist timestamp in the current heading and return it. 
;; if INACTIVE is non-nil, get the first inactive timestamp.  If 
;; RANGE is non-nil, only look for timestamp ranges.

;; Do not return deadline, scheduled, or closed timestamps.

;; If ALL is non-nil, return a list of all timestamps found.  Not that
;; if ALL is non-nil, RANGE is ignored."
;;   ;; also don't understand why there isn't a built in way to do this
;;   (save-excursion
;;     (cl-loop while (re-search-forward
;; 		    (pcase `(,inactive ,range)
;; 		      (`(nil t)
;; 		       org-tr-regexp)
;; 		      (`(nil nil)
;; 		       org-ts-regexp)
;; 		      (`(t nil)
;; 		       org-ts-regexp-inactive)
;; 		      (`(t t)
;; 		       (concat 
;; 			org-ts-regexp-inactive
;; 			"--?-?"
;; 			org-ts-regexp-inactive)))
;; 		    (org-entry-end-position)
;; 		    t)
;; 	     when (save-match-data (not (eq (car (org-element-at-point))
;; 					    'planning)))
;; 	     return (org-no-properties (match-string 0)))))

(defun reorg-org--get-body ()
  "get headings body text"  
  ;; FIXME this adds way too much time to parsing the
  ;; org file.
  ;;
  ;; First attempt:
  ;; (org-no-properties
  ;;  (org-element-interpret-data
  ;;   (org-element--parse-elements (save-excursion (org-back-to-heading)
  ;; 						 (org-end-of-meta-data t)
  ;; 						 (point))
  ;; 				 (or (save-excursion (outline-next-heading))
  ;; 				     (point-max))
  ;; 				 'first-section nil nil nil nil)))
  ;; 
  ;; Second attempt; not sure if any better 
  (save-excursion
    (when (reorg-org--goto-end-of-meta-data)
      (string-trim 
       (buffer-substring-no-properties
	(point)
	(org-entry-end-position))))))

(defun reorg-org--map-entries (files func)
  "regular expression is faster than `org-map-entries'
even if it doesn't make archives available"
  (cl-loop for file in files
	   append (with-current-buffer (find-file-noselect file)
		    (org-with-wide-buffer
		     (goto-char (point-min))
		     (cl-loop while (re-search-forward
				     org-heading-regexp
				     nil
				     t)
			      do (goto-char (match-beginning 0))
			      collect (funcall func)
			      do (goto-char (point-at-eol)))))))

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
  "Execute BODY in the source buffer."
  (declare (indent defun))
  `(when-let ((marker (reorg--get-prop 'marker))
	      (buffer (marker-buffer marker))
	      (id (reorg--get-prop 'id)))
     (with-current-buffer buffer
       (save-excursion
	 (save-restriction
	   (let ((old-point (point))
		 (search-invisible t))
	     (widen)
	     (ov-clear)
	     (goto-char marker)
	     ,@body))))))

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

(defun reorg-org--edit-timestamp (&optional inactive)
  (completing-read "Select timestamp to edit: "
		   (reorg-org--with-point-at-orig-entry
		    nil
		    nil
		    (reorg-org--timestamp-parser 'all))
		   ))


;;; org class 

(reorg-create-class-type
 :name org
 :render-func reorg-org--render-source
 :keymap (;; ("SPC" . reorg-org--open-agenda-day)
	  ("h" . reorg-org--org-edit-headline)
	  ("t" . reorg-org--org-todo)
	  ("a" . reorg-org--org-set-tags-command)
	  ("d" . reorg-org--org-deadline)
	  ("s" . reorg-org--org-schedule)
	  ("r" . reorg-org--org-set-property)
	  ("i" . reorg-org--org-priority)
	  ("g" . reorg-org--reload-heading))
 :getter
 ;; Example 1
 ;; (with-current-buffer (find-file SOURCE)
 ;; 	   (widen)
 ;; 	   (org-map-entries #'PARSER)))

 ;; Example 2
 ;; (progn
 ;;   (setq org-ql-cache (make-hash-table :weakness 'key))
 ;;   (org-ql-select SOURCE nil :action #'PARSER))

 ;; Example 3
 (reorg-org--map-entries SOURCE #'PARSER)
 )

;;; org data 

(reorg-create-data-type
 :name buffer-file-name
 :doc "Path to buffer file."
 :class org
 :parse (buffer-file-name))

(reorg-create-data-type
 :name delegated
 :doc "Get the DELEGATED property."
 :class org
 :parse (org-entry-get (point) "DELEGATED"))

(reorg-create-data-type
 :name property
 :doc "Get property drawer as an alist."
 :parse (reorg-org--get-property-drawer)
 :class org)

(reorg-create-data-type
 :name tag-list
 :class org
 :parse (org-get-tags))

(reorg-create-data-type
 :name ts-single
 :class org
 :parse (or (reorg-org--timestamp-parser 'deadline)
	    (reorg-org--timestamp-parser 'active)
	    (reorg-org--timestamp-parser 'scheduled)))

;; (reorg-create-data-type
;;  :name ts-pretty
;;  :class org
;;  :parse (when-let ((ts (or
;; 			(org-entry-get (point) "DEADLINE")
;; 			(reorg-org--timestamp-parser)
;; 			(org-no-properties
;; 			 (reorg-org--timestamp-parser nil t))
;; 			(org-entry-get (point) "SCHEDULED"))))
;; 	  (if (=
;; 	       (string-to-number
;; 		(format-time-string "%Y"))
;; 	       (ts-year (ts-parse-org ts)))
;; 	      (s-pad-right 22 " "
;; 			   (reorg-org--format-time-string
;; 			    ts
;; 			    "%a, %b %d"
;; 			    "%a, %b %d at %-l:%M%p"))
;; 	    (s-pad-right 22 " "
;; 			 (reorg-org--format-time-string
;; 			  ts
;; 			  "%a, %b %d, %Y"
;; 			  "%a, %b %d, %Y at %-l:%M%p")))))


;; (reorg-create-data-type
;;  :name deadline 
;;  :class org
;;  :parse (or
;; 	 (org-entry-get (point) "DEADLINE")
;; 	 (when (reorg-org--timestamp-parser)
;; 	   (org-no-properties (reorg-org--timestamp-parser)))
;; 	 (when (reorg-org--timestamp-parser nil t)
;; 	   (org-no-properties (reorg-org--timestamp-parser nil t)))
;; 	 (org-entry-get (point) "SCHEDULED")))

(reorg-create-data-type
 :name timestamp-active
 :class org
 :parse (reorg-org--timestamp-parser 'active))

(reorg-create-data-type
 :name timestamp-range
 :class org
 :parse (reorg-org--timestamp-parser 'active-range))

(reorg-create-data-type
 :name timestamp-active-all 
 :class org
 :parse (reorg-org--timestamp-parser 'active t))

(reorg-create-data-type
 :name timestamp-inactive
 :class org
 :parse (reorg-org--timestamp-parser 'inactive))

(reorg-create-data-type
 :name timestamp-inactive-all
 :class org
 :parse (reorg-org--timestamp-parser 'inactive t))

(reorg-create-data-type
 :name priority
 :class org
 :parse (org-entry-get (point) "PRIORITY")
 :display (pcase (alist-get 'priority data)
	    ("A" "↗")
	    ("B" "→")
	    ("C" "↘")
	    (_ " ")))

(reorg-create-data-type
 :name body
 :class org
 :parse (reorg-org--get-body))

(reorg-create-data-type
 :name deadline
 :class org
 :parse (reorg-org--timestamp-parser 'deadline))

(reorg-create-data-type
 :name scheduled
 :class org 
 :parse (reorg-org--timestamp-parser 'scheduled))

(reorg-create-data-type
 :name closed
 :class org 
 :parse (reorg-org--timestamp-parser 'closed))

(reorg-create-data-type
 :name headline
 :class org
 :parse (org-no-properties
	 (org-get-heading t t t t)))

(reorg-create-data-type
 :name todo
 :class org
 :parse (org-entry-get (point) "TODO"))

;; (reorg-create-data-type
;;  :name links
;;  :class org
;;  :parse (reorg-org--all-link-parser))

;; (reorg-create-data-type
;;  :name link
;;  :class org
;;  :parse (reorg-org--link-parser))

;; (reorg-create-data-type
;;  :name link-file-name
;;  :class org
;;  :parse (when-let* ((data (reorg-org--link-parser))
;; 		    (path (alist-get 'link data))
;; 		    (name (f-filename path)))
;; 	  (car (s-split "::" name))))

;; (reorg-create-data-type
;;  :name link-file-path
;;  :class org
;;  :parse (when-let* ((data (reorg-org--link-parser))
;; 		    (data (alist-get 'link data))
;; 		    (data (cadr (s-split (rx (one-or-more alnum)
;; 					     ":/")
;; 					 data)))
;; 		    (data (car (s-split "::" data))))
;; 	  (concat "/" data)))

(reorg-create-data-type
 :name active-ts
 :class org
 :parse (-non-nil (append (list (reorg-org--timestamp-parser 'deadline))
			  (list (reorg-org--timestamp-parser 'scheduled))
			  (reorg-org--timestamp-parser 'active t))))


(reorg-create-data-type
 :name marker
 :class org
 :parse (point-marker))

(reorg-create-data-type
 :name id
 :class org
 :parse (org-id-get-create))

(reorg-create-data-type
 :name category-inherited
 :class org
 :parse (org-entry-get-with-inheritance "CATEGORY"))

;; (reorg-create-data-type
;;  :name category
;;  :class org
;;  :parse (org-get-category))

(reorg-create-data-type
 :name filename
 :class org
 :parse (f-filename (buffer-file-name)))

(reorg-create-data-type
 :name buffer-name
 :class org
 :parse (buffer-name))

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
 :doc (concat "Go to each parent heading and return the first"
	      " inactive timestamp found.")
 :class org
 :parse (save-excursion
	  (cl-loop while (org-up-heading-safe)
		   when (reorg-org--timestamp-parser 'inactive)
		   return (reorg-org--timestamp-parser 'inactive))))

(reorg-create-data-type
 :name at-names
 :class org
 :parse (let ((headline (org-get-heading t t t t)))
	  (cl-loop
	   with start = 0
	   while
	   (setq start
		 (and
		  (string-match "@\\([[:word:]]+\\)"
				headline
				start)
		  (match-end 1)))
	   collect (match-string-no-properties 1 headline))))

(reorg-create-data-type
 :name timestamp-type
 :disable t
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
 :name ts-all
 :class org
 :parse (reorg-org--timestamp-parser 'all))

;; (reorg-create-data-type
;;  :name ts-ts
;;  :class org
;;  :parse (when .ts-any
;; 	  (ts-parse-org .ts-any)))

;; (reorg-create-data-type
;;  :name all-active-timestamps
;;  :class org
;;  :parse (save-excursion
;; 	  (cl-loop
;; 	   while (re-search-forward org-ql-regexp-ts-active
;; 				    (save-excursion (outline-next-heading))
;; 				    t)
;; 	   collect (match-string-no-properties 0))))

;; (reorg-create-data-type
;;  :name ts-any
;;  :class org
;;  :parse (or 
;; 	 (org-entry-get (point) "DEADLINE")
;; 	 (reorg-org--timestamp-parser) ;; active timestamp
;; 	 (org-no-properties (reorg-org--timestamp-parser nil t)) ;; active range
;; 	 (org-entry-get (point) "SCHEDULED")
;; 	 (org-no-properties (reorg-org--timestamp-parser t nil))
;; 	 (org-no-properties (reorg-org--timestamp-parser t t))))

(provide 'reorg-org)
