;; -*- lexical-binding: t; -*-
;;; data 

(reorg-create-class-type :name org
			 :getter 
			 (with-current-buffer (find-file-noselect SOURCE)
			   (org-show-all)
			   (org-map-entries
			    #'PARSER)))

(reorg-create-data-type :name headline
			:class org
			;; :set (lambda ()
			;;        (let ((val (field-string-no-properties)))
			;; 	 (reorg--with-source-and-sync val
			;; 	   (org-edit-headline val))))
			;; :face org-level-3
			:parse (org-no-properties
				(org-get-heading t t t t)))

(reorg-create-data-type :name ts
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
					 (reorg--format-time-string ts
								    "%a, %b %d"
								    "%a, %b %d at %-l:%M%p")
				       (reorg--format-time-string ts
								  "%a, %b %d, %Y"
								  "%a, %b %d, %Y at %-l:%M%p"))
				   "nots"))

(reorg-create-data-type :name ts-type
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

(reorg-create-data-type :name priority
			:class org
			:parse (org-entry-get (point) "PRIORITY")
			:display (pcase (alist-get 'priority alist)
				   ("A" "⚡")
				   ("B" "➙")
				   ("C" "﹍")
				   (_ " ")))

;; (reorg-create-data-type :name body
;; 			:class org
;; 			:parse (reorg--get-body))

(reorg-create-data-type :name deadline
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

(reorg-create-data-type :name scheduled
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

(reorg-create-data-type :name headline
			:class org
			;; :set (lambda ()
			;;        (let ((val (field-string-no-properties)))
			;; 	 (reorg--with-source-and-sync val
			;; 	   (org-edit-headline val))))
			;; :face org-level-3
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

(reorg-create-data-type :name file
			:class org
			:parse (buffer-file-name))

(reorg-create-data-type :name buffer-name
			:class org
			:parse (buffer-name))

(reorg-create-data-type :name buffer
			:class org
			:parse (current-buffer))

(reorg-create-data-type :name level
			:class org
			:parse (org-current-level)
			:display (number-to-string (alist-get 'level alist)))

(reorg-create-data-type :name root
			:class org
			:parse (save-excursion (while (org-up-heading-safe))
					       (org-no-properties
						(org-get-heading t t t t))))



(provide 'reorg-org)
