;; -*- lexical-binding: t; -*-



;;; syncing macro

(defun reorg-org-capture-disable ()
  "disable org capture"
  (interactive)
  (reorg-org-capture-enable t))

(defun reorg-org-capture-enable (&optional disable)
  "wrapper for org-capture"
  (interactive "P")  
  (if disable
      (remove-hook 'org-capture-after-finalize-hook
		   #'reorg-org-capture-hook)  
    (add-hook 'org-capture-after-finalize-hook
	      #'reorg-org-capture-hook)))

(defun reorg-org-capture-hook ()
  "org capture hook to put captured header
into current reorg outline."
  (let (data)
    (reorg--select-main-window)
    (org-with-wide-buffer 
     (org-capture-goto-last-stored)
     (setq data (reorg--parser nil 'org)))
    (reorg--select-tree-window)
    (when (member (cons
		   (alist-get 'class data)
		   (abbreviate-file-name
		    (alist-get 'filename data)))
		  reorg--current-sources) 	  
      (reorg--insert-new-heading* data reorg--current-template))))

(defmacro reorg--with-source-and-sync (&rest body)
  "Execute BODY in the source buffer and
update the heading at point."
  (declare (indent defun))
  `(progn
     (let (data)
       (org-with-remote-undo (reorg--get-view-prop 'buffer)
	 (reorg--goto-source)
	 (org-with-wide-buffer
	  (org-back-to-heading)
	  ,@body
	  (setq data (reorg--parser nil 'org)))
	 (reorg--select-tree-window)
	 (save-excursion
	   (reorg--insert-new-heading* data reorg--current-template))))))

(defun reorg--get-format-string ()
  "get format string at point"
  (save-excursion 
    (cl-loop until (or (reorg--get-view-prop 'format-string)
		       (not (reorg--goto-parent t)))
	     finally return (or (reorg--get-view-prop 'format-string)
				reorg-headline-format))))

;;; parsing functions 

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
  (org-no-properties
   (org-element-interpret-data
    (org-element--parse-elements (save-excursion (org-back-to-heading)
						 (org-end-of-meta-data t)
						 (point))
				 (or (save-excursion (outline-next-heading))
				     (point-max))
				 'first-section nil nil nil nil))))

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

(defmacro reorg--with-restore-state (&rest body)
  "do BODY while saving, excursion, restriction, etc."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction       
       (widen)
       (let ((inhibit-field-text-motion t))
	 ,@body))))

;;; moving from outline to orgmode 
;; TODO fix tree to source orgmode functions; figure out how to generalize
;; TODO delete unused files

(defun reorg--org--render-source (&optional buffer id no-narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (let ((id (or id (reorg--get-view-prop 'id))))
    (reorg--select-main-window (or buffer (reorg--get-view-prop 'buffer)))
    (let ((old-point (point))
	  (search-invisible t))
      (widen)
      (goto-char (point-min))
      (if (re-search-forward id nil t)
	  (progn (goto-char (match-beginning 0))
		 (org-back-to-heading)
		 (when (not no-narrow)
		   (reorg-view--source--narrow-to-heading)))
	(goto-char old-point)))
    (reorg--select-tree-window)))

;; TODO figure out where to use this after navigation
;; ie, what hook should call this? 
(defun reorg-org--goto-end-of-meta-data ()
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
  (org-narrow-to-element)
  (reorg-org--goto-end-of-meta-data))


;; (reorg--select-main-window)
;; (set-window-buffer (selected-window) buffer)))

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

(reorg-create-class-type
 :name org
 :render-func reorg--org--render-source
 :keymap (("h" . (lambda (&optional arg)					   
		   (interactive)
		   (reorg--with-source-and-sync 
		     (org-edit-headline (read-string "New headline: "
						     (org-get-heading t t t t))))))
	  ("t" . (lambda (&optional arg) (interactive "P")
		   (reorg--with-source-and-sync
		     (funcall-interactively #'org-todo arg))))
	  ("a" . (lambda (&optional arg) (interactive "P")
		   (reorg--with-source-and-sync
		     (funcall-interactively #'org-set-tags-command arg))))
	  ("d" . (lambda (&optional arg) (interactive "P")
		   (reorg--with-source-and-sync
		     (funcall-interactively #'org-deadline arg))))
	  ("s" . (lambda (&optional arg) (interactive "P")
		   (reorg--with-source-and-sync
		     (funcall-interactively #'org-schedule arg))))
	  ("r" . (lambda (&optional arg) (interactive )
		   (reorg--with-source-and-sync
		     (funcall-interactively #'org-set-property))))
	  ("i" . (lambda (&optional arg) (interactive "P")
		   (reorg--with-source-and-sync
		     (funcall-interactively #'org-priority arg))))
	  ("g" . (lambda (&optional arg) (interactive)
		   (reorg--with-source-and-sync))))
 :getter (with-current-buffer (find-file-noselect SOURCE)
	   (widen)
	   (org-show-all)
	   (org-map-entries
	    #'PARSER)))

(reorg-create-data-type
 :name ts-ts
 :class org
 :append t
 :parse (when (alist-get 'ts-any data)
	  (ts-parse-org (alist-get 'ts-any data))))

(reorg-create-data-type
 :name delegatee
 :class org
 :parse (org-entry-get (point) "DELEGATED"))

(reorg-create-data-type
 ;; this uses the already parsed ts-any 
 :name ts-year
 :class org
 :append t
 :parse (when-let ((ts (alist-get 'ts-ts data)))
	  (number-to-string (ts-year ts))))

(reorg-create-data-type
 ;; this uses the already parsed ts-any 
 :name ts-month
 :class org
 :append t
 :parse (when-let ((ts (alist-get 'ts-ts data)))
	  (ts-month-name ts)))

(reorg-create-data-type
 ;; this uses the already parsed ts-any 
 :name ts-day
 :class org
 :append t
 :parse (when-let ((ts (alist-get 'ts-ts data)))
	  (ts-day ts)))

(reorg-create-data-type
 ;; this uses the already parsed ts-any 
 :name ts-day-name
 :class org
 :append t
 :parse (when-let ((ts (alist-get 'ts-ts data)))
	  (ts-day-name ts)))


(reorg-create-data-type
 :name tag-list
 :class org
 :parse (org-get-tags))

(reorg-create-data-type
 :name headline
 :class org
 ;; :set (lambda ()
 ;;        (let ((val (field-string-no-properties)))
 ;; 	 (reorg--with-source-and-sync val
 ;; 	   (org-edit-headline val))))
 ;; :face org-level-3
 :parse (->> (org-no-properties
	      (org-get-heading t t t t))
	     (replace-regexp-in-string reorg-org--org-link-regexp "")
	     (s-trim)
	     (s-replace " \\." "")))


(reorg-create-data-type
 :name ts-pretty
 :class org
 :parse (when-let ((ts (or
			(org-entry-get (point) "DEADLINE")
			(reorg--timestamp-parser)
			(org-no-properties (reorg--timestamp-parser nil t))
			(org-entry-get (point) "SCHEDULED"))))
	  (if (=
	       (string-to-number
		(format-time-string "%Y"))
	       (ts-year (ts-parse-org ts)))
	      (s-pad-right 22 " "
			   (reorg--format-time-string ts

						      "%a, %b %d"
						      "%a, %b %d at %-l:%M%p"))
	    (s-pad-right 22 " "
			 (reorg--format-time-string ts
						    "%a, %b %d, %Y"
						    "%a, %b %d, %Y at %-l:%M%p")))))


(reorg-create-data-type
 ;;TODO add :desc[ription] keyword 
 :name ts
 :class org
 :parse (or
	 (org-entry-get (point) "DEADLINE")
	 (when (reorg--timestamp-parser)
	   (org-no-properties (reorg--timestamp-parser)))
	 (when (reorg--timestamp-parser nil t)
	   (org-no-properties (reorg--timestamp-parser nil t))))
 :display (if-let ((ts (alist-get 'ts alist)))
	      (if (=
		   (string-to-number
		    (format-time-string "%Y"))
		   (ts-year (ts-parse-org ts)))
		  (s-pad-right 22 " "
			       (reorg--format-time-string ts

							  "%a, %b %d"
							  "%a, %b %d at %-l:%M%p"))
		(s-pad-right 22 " "
			     (reorg--format-time-string ts
							"%a, %b %d, %Y"
							"%a, %b %d, %Y at %-l:%M%p")))
	    ""))

(reorg-create-data-type
 :name ts-any
 :class org
 :parse (or 
	 (org-entry-get (point) "DEADLINE")
	 (reorg--timestamp-parser) ;; active timestamp
	 (org-no-properties (reorg--timestamp-parser nil t)) ;; active range
	 (org-entry-get (point) "SCHEDULED")
	 (org-no-properties (reorg--timestamp-parser t nil))
	 (org-no-properties (reorg--timestamp-parser t t))))

(reorg-create-data-type
 :name ts-type
 :class org
 :parse (cond 
	 ((org-entry-get (point) "DEADLINE") "deadline")
	 ((reorg--timestamp-parser) "active")
	 ((org-no-properties (reorg--timestamp-parser nil t)) "range")
	 ((org-entry-get (point) "SCHEDULED") "scheduled"))
 :display (pcase (alist-get 'ts-type alist)
	    ("deadline" "≫")
	    ("active" "⊡")
	    ("range" "➥")
	    ("scheduled" "⬎")
	    (_ " ")))

(reorg-create-data-type
 :name timestamp-all
 :class org
 :parse (or
	 (org-entry-get (point) "DEADLINE")
	 (reorg--timestamp-parser)
	 (org-no-properties (reorg--timestamp-parser nil t))
	 (org-entry-get (point) "SCHEDULED")))

(reorg-create-data-type
 :name priority
 :class org
 :parse (org-entry-get (point) "PRIORITY")
 :display (pcase (alist-get 'priority alist)
	    ("A" "⚡")
	    ("B" "➙")
	    ("C" "﹍")
	    (_ " ")))

(reorg-create-data-type :name body
			:class org
			:parse (reorg--get-body))

(reorg-create-data-type
 :name deadline
 :class org
 :parse (org-entry-get (point) "DEADLINE")
 ;; :set (lambda ()
 ;;        (reorg--with-source-and-sync
 ;; 	 (if val (org-deadline nil val)
 ;; 	   (org-deadline '(4)))))
 ;; :display (if (plist-get plist :deadline)
 ;; 	     (concat 
 ;; 	      (propertize "DEADLINE: "

 ;; 			  'font-lock-face 'org-special-keyword)
 ;; 	      (propertize (plist-get plist :deadline)
 ;; 			  'font-lock-face 'org-date))
 ;; 	   "__________")
 :display (when (alist-get 'deadline alist)
	    (string-pad 
	     (ts-format "%B %e, %Y" ;
			(ts-parse-org (alist-get 'deadline alist)))
	     18
	     nil t)))

(reorg-create-data-type
 :name scheduled
 :class org 
 :parse (org-entry-get (point) "SCHEDULED")
 ;; :set (lambda ()
 ;;        (reorg--with-source-and-sync
 ;; 	 (if val (org-scheduled nil val)
 ;; 	   (org-scheduled '(4)))))
 :display (if (alist-get 'scheduled alist)
	      (concat 
	       (propertize "SCHEDULED: "

			   'font-lock-face 'org-special-keyword)
	       (propertize (alist-get 'scheduled alist)
			   'font-lock-face 'org-date))
	    "__________"))

(reorg-create-data-type
 :name headline
 :class org
 ;; :set (lambda ()
 ;;        (let ((val (field-string-no-properties)))
 ;; 	 (reorg--with-source-and-sync val
 ;; 	   (org-edit-headline val))))
 ;; :face org-level-3
 :display (alist-get 'headline alist)
 :parse (org-no-properties
	 (org-get-heading t t t t)))

;; (reorg-create-data-type
;;  :name property
;;  :class org
;;  :parse (reorg-parser--get-property-drawer)
;;  ;; :set (lambda ()
;;  ;;        (reorg--with-source-and-sync
;;  ;; 	 (let* ((pair (split-string val ":" t " "))
;;  ;; 		(key (upcase (car pair)))
;;  ;; 		(val (cadr pair)))
;;  ;; 	   (org-set-property key val))))
;;  :display (let* ((key (reorg--add-remove-colon (car args) t))
;; 		 (val (plist-get (plist-get plist :property)
;; 				 (reorg--add-remove-colon key))))
;; 	    (concat
;; 	     (propertize (format "%s:" key) 'font-lock-face 'org-special-keyword)
;; 	     " "
;; 	     (propertize (format "%s" val) 'font-lock-face 'org-property-value))))
;; ;; :field-keymap (("C-c C-x p" . org-set-property)))

(reorg-create-data-type
 :name tags
 :class org
 :parse (org-get-tags-string))
;; :get (org-get-tags-string)
;; :set (org-set-tags val)
;; :face org-tag-group
;; :heading-keymap (("C-c C-c" . org-set-tags-command)))

(reorg-create-data-type
 :name todo
 :class org
 :parse (org-entry-get (point) "TODO")
 ;; :get (org-entry-get (point) "TODO")			
 ;; :set (org-todo val)
 :display (when-let ((s (alist-get 'todo alist)))
	    (propertize
	     s
	     'font-lock-face
	     (org-get-todo-face s))))
;; :heading-keymap (("C-c C-t" . org-todo)
;; 		  ("S-<right>" . org-shiftright)
;; 		  ("S-<left>" . org-shiftleft)))

(reorg-create-data-type
 :name timestamp
 :class org
 :parse (when (reorg--timestamp-parser)
	  (org-no-properties (reorg--timestamp-parser)))
 ;; :get (reorg--timestamp-parser)
 ;; :set (if-let* ((old-val (reorg--timestamp-parser)))
 ;; 	 (when (search-forward old-val (org-entry-end-position) t)
 ;; 	   (replace-match (concat val)))
 ;;        (when val
 ;; 	 (org-end-of-meta-data t)
 ;; 	 (insert (concat val "\n"))
 ;; 	 (delete-blank-lines)))
 :display
 (if (alist-get 'timestamp alist)
     (concat 
      (propertize (alist-get 'timestamp alist)
		  'font-lock-face 'org-date))
   "____"))
;; :field-keymap (("S-<up>" . org-timestamp-up)
;; 		("S-<down>" . org-timestamp-down))
;; :header-keymap (("C-c ." . org-time-stamp))
;; :validate (with-temp-buffer
;; 	     (insert val)
;; 	     (beginning-of-buffer)
;; 	     (org-timestamp-change 0 'day)
;; 	     (buffer-string)))

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
	     reorg-org--org-link-regexp
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
		      reorg-org--org-link-regexp
		      limit
		      t)
	       collect (list 
			(cons 'link (match-string-no-properties 1))
			(cons 'text (match-string-no-properties 2)))))))


(reorg-create-data-type :name links
			:class org
			:parse (reorg-org--all-link-parser))

(reorg-create-data-type :name link
			:class org
			:parse (reorg-org--link-parser))

(reorg-create-data-type :name link-file-name
			:class org
			:parse (when-let* ((data (reorg-org--link-parser))
					   (path (alist-get 'link data))
					   (name (f-filename path)))
				 (car (s-split "::" name))))

(reorg-create-data-type :name link-file-path
			:class org
			:parse (when-let* ((data (reorg-org--link-parser))
					   (data (alist-get 'link data))
					   (data (cadr (s-split (rx (one-or-more alnum)
								    ":/")
								data)))
					   (data (car (s-split "::" data))))
				 (concat "/" data)))

(reorg-create-data-type :name timestamp-ia
			:class org
			:parse (when (reorg--timestamp-parser t)
				 (org-no-properties (reorg--timestamp-parser t))))
;; :get (reorg--timestamp-parser t)
;; :set (if-let* ((old-val (reorg--timestamp-parser t)))
;; 	 (when (search-forward old-val (org-entry-end-position) t)
;; 	   (replace-match (concat val)))
;;        (when val
;; 	 (org-end-of-meta-data t)
;; 	 (insert (concat val "\n"))
;; 	 (delete-blank-lines)))
;; :face org-date
;; :field-keymap (("S-<up>" . org-timestamp-up)
;; 		("S-<down>" . org-timestamp-down))
;; :header-keymap (("C-c ." . org-time-stamp))
;; :validate (with-temp-buffer
;; 	     (insert val)
;; 	     (beginning-of-buffer)
;; 	     (org-timestamp-change 0 'day)
;; 	     (buffer-string)))

(reorg-create-data-type :name timestamp-ia-range
			:class org
			:parse (when (reorg--timestamp-parser t t)
				 (org-no-properties (reorg--timestamp-parser t t))))
;; :get (reorg--timestamp-parser t)
;; :set (if-let* ((old-val (reorg--timestamp-parser t)))
;; 	 (when (search-forward old-val (org-entry-end-position) t)
;; 	   (replace-match (concat val)))
;;        (when val
;; 	 (org-end-of-meta-data t)
;; 	 (insert (concat val "\n"))
;; 	 (delete-blank-lines)))
;; :face org-date
;; :keymap (("S-<up>" . org-timestamp-up)
;; 	 ("S-<down>" . org-timestamp-down))
;; :validate (with-temp-buffer
;; 	    (insert val)
;; 	    (beginning-of-buffer)
;; 	    (org-timestamp-change 0 'day)
;; 	    (buffer-string)))

(reorg-create-data-type :name timestamp-range
			:class org
			:parse (when (reorg--timestamp-parser nil t)
				 (org-no-properties (reorg--timestamp-parser nil t))))
;; :get (reorg--timestamp-parser t)
;; ;; :set (if-let* ((old-val (reorg--timestamp-parser t)))
;; ;; 	 (when (search-forward old-val (org-entry-end-position) t)
;; ;; 	   (replace-match (concat val)))
;; ;;        (when val
;; ;; 	 (org-end-of-meta-data t)
;; ;; 	 (insert (concat val "\n"))
;; ;; 	 (delete-blank-lines)))
;; :face org-date
;; :keymap (("S-<up>" . org-timestamp-up)
;; 	 ("S-<down>" . org-timestamp-down))
;; :validate (with-temp-buffer
;; 	    (insert val)
;; 	    (beginning-of-buffer)
;; 	    (org-timestamp-change 0 'day)
;; 	    (buffer-string))
;; :disabled nil)

(reorg-create-data-type :name id
			:class org
			:parse (org-id-get-create))

(reorg-create-data-type :name category-inherited
			:class org
			:parse (org-entry-get-with-inheritance "CATEGORY"))

(reorg-create-data-type :name category
			:class org
			:parse (org-get-category))
;; :set (org-set-property "CATEGORY" val))

(reorg-create-data-type :name filename
			:class org
			:parse (abbreviate-file-name (buffer-file-name)))

(reorg-create-data-type :name buffer-name
			:class org
			:parse (buffer-name))

(reorg-create-data-type :name buffer
			:class org
			:parse (current-buffer))

(reorg-create-data-type :name order
			:class org
			:parse (point))

(reorg-create-data-type :name org-level
			:class org
			:parse (org-current-level))

(reorg-create-data-type :name root
			:class org
			:parse (save-excursion (while (org-up-heading-safe))
					       (org-no-properties
						(org-get-heading t t t t))))
(reorg-create-data-type
 ;; inactive timestamp between the current heading and the root
 :name root-ts-inactive
 :class org
 :parse (save-excursion (cl-loop while (org-up-heading-safe)
				 when (reorg--timestamp-parser t nil)
				 return (reorg--timestamp-parser t nil))))

(reorg-create-data-type
 :name at-names
 :class org
 :parse (let ((headline (org-get-heading t t t t)))
	  (cl-loop with start = 0
		   while (setq start (and (string-match "@\\([[:word:]]+\\)" headline start)
					  (match-end 1)))
		   collect (match-string-no-properties 1 headline))))

(reorg-create-data-type
 :name childrenp
 :class org
 :parse (org-sidebar--children-p))

(provide 'reorg-org)



