;;; reorg-dynamic-bullets.el --- Orgmode dynamic bullets -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'outline)

;;;; Faces

(defface reorg-dynamic-bullets-face '((t (:foreground "gray")))
  "Bullet face"
  :group 'reorg-dynamic-bullets)

(defface reorg-dynamic-bullets-clone-face '((t (:foreground "red")))
  "Clone face"
  :group 'reorg-dynamic-bullets)

;;;; Customization

(defcustom reorg-dynamic-bullets-update-triggers
  '((outline-cycle . reorg-dynamic-bullets--org-cycle-hook-func))
  "Alist in the form '((hook-or-function . refresh-function)))
where refresh-function is added as a hook or advice to hook-or-function.
Refresh function should be one of the `reorg-dynamic-bullets--fontify' functions
below."
  :type 'alist
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-folded-body-text-bullet "▶"
  "Bullet for a folded node with text in its body."
  :type 'string
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-folded-no-body-text-bullet "▷"
  "Bullet for folded node with no text in its body."
  :type 'string
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-unfolded-body-text-bullet "▼"
  "Bullet for an unfolded node with text in its body."
  :type 'string
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-unfolded-no-body-text-bullet "▽"
  "Bullet for an unfolded node with no text in its body."
  :type 'string
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-leaf-body-text-bullet "▬"
  "Bullet for a leaf node with body text."
  :type 'string
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-leaf-no-body-text-bullet "▭"
  "Bullet for a leaf node with no body text."
  :type 'string
  :group 'reorg-dynamic-bullets)

(defcustom reorg-dynamic-bullets-refresh-func
  #'reorg-dynamic-bullets--refresh-with-compose-region
  "Function to refresh bullets.  Must take
two arguments: BEG and END representing the region
to be refreshed. Two options are:
`reorg-dynamic-bullets--refresh-with-compose-region',
`reorg-dynamic-bullets--refresh-with-text-props', or
`reorg-dynamic-bullets--refresh-with-font-lock'."
  :type 'function
  :group 'reorg-dynamic-bullets)

;;;; Variables

(defvar reorg-dynamic-bullets--idle-timer nil
  "Idle timer to update heading bullets.")

;;;; Constants

(defconst reorg-dynamic-bullets--heading-re "^\\(?1:\\*+\\) "
  "Outline heading regexp.")

;; (defconst reorg-dynamic-bullets--font-lock-keyword
;;   `((,reorg-dynamic-bullets--heading-re
;;      (1 (list 'face 'reorg-dynamic-bullets-face
;;  	      'display (reorg-dynamic-bullets--create-heading-bullet)))))
;;   "Font-lock keyword to fontify heading stars.")

(defconst reorg-dynamic-bullets--font-lock-keyword
  `((,reorg-dynamic-bullets--heading-re
     (1 (progn (compose-region (match-beginning 1) (match-end 1)
			       (reorg-dynamic-bullets--create-heading-bullet))
	       nil))))
  "Font-lock keyword to fontify heading stars.")

;;;; Minor mode

(define-minor-mode reorg-dynamic-bullets-mode
  "Display orgmode trees."
  nil
  " dbullets"
  nil
  (if reorg-dynamic-bullets-mode
      (progn 
	(reorg-dynamic-bullets--add-all-hooks-and-advice)
	;;(cl-pushnew 'display font-lock-extra-managed-props)
	;;(font-lock-add-keywords nil reorg-dynamic-bullets--font-lock-keyword)
	(reorg-dynamic-bullets--fontify-buffer)
	(setq reorg-dynamic-bullets--idle-timer
	      (run-with-idle-timer 1 'repeat #'reorg-dynamic-bullets--fontify-heading)))
    ;; (font-lock-remove-keywords nil reorg-dynamic-bullets--font-lock-keyword)
    (reorg-dynamic-bullets--add-all-hooks-and-advice 'remove)
    (reorg-dynamic-bullets--fontify (point-min) (point-max) 'remove)
    (cancel-timer reorg-dynamic-bullets--idle-timer)
    ;; (font-lock-flush (point-min) (point-max))
    ;; (font-lock-ensure (point-min) (point-max))
    ))

;;;; Functions

;;;;; Outline position predicates

(defun reorg-dynamic-bullets--has-children-p (&optional invisible)
  "Does the current node have any children?"
  (save-excursion
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (outline-on-heading-p t)
	   (> (funcall outline-level) level)
	   (or (not invisible)
	       (outline-invisible-p))))))

(defun reorg-dynamic-bullets--heading-folded-p ()
  "Is the current heading folded?"
  (and (save-excursion
	 (org-goto-first-child)
	 (outline-invisible-p))
       (save-excursion 
	 (org-end-of-meta-data t)
	 (outline-invisible-p))))

(defun reorg-dynamic-bullets--body-p ()
  "Use org-element to get all elements after the property drawers."
  (not (string= "" (reorg--get-view-prop :body))))
;; (when (reorg--get-view-prop)
;;   (reorg--with-source-buffer
;;     (org-element--parse-elements (save-excursion (org-back-to-heading)
;; 						   (org-end-of-meta-data t)
;; 						   (point))
;; 				   (or (save-excursion (outline-next-heading))
;; 				       (point-max))
;; 				   'first-section nil nil nil nil))))

;; (defun reorg-dynamic-bullets--body-p ()
;;   "Does the current heading have text in its body? \"Body\" is
;; defined as any text, including property drawers, following
;; the heading and before the next heading."
;;   (cddr (car (org-element--parse-elements
;; 	      (point)
;; 	      (save-excursion (or (outline-next-heading)
;; 				  (point-max)))
;; 	      nil nil nil nil nil))))

;;;;; Creating prefix strings

(defun reorg-dynamic-bullets--create-heading-bullet ()
  "Create a string to be displayed in lieu of the headings' leading stars."
  (save-excursion 
    (outline-back-to-heading)
    (let ((children (reorg-dynamic-bullets--has-children-p))
	  (folded (reorg-dynamic-bullets--heading-folded-p))
	  (body (reorg-dynamic-bullets--body-p))
	  (clone (reorg--get-view-prop :clone)))
      (propertize 
       (cond ((and children folded body)
	      reorg-dynamic-bullets-folded-body-text-bullet)
	     ((and children folded)
	      reorg-dynamic-bullets-folded-no-body-text-bullet)
	     ((and children body)
	      reorg-dynamic-bullets-unfolded-body-text-bullet)
	     (children
	      reorg-dynamic-bullets-unfolded-no-body-text-bullet)
	     (body
	      reorg-dynamic-bullets-leaf-body-text-bullet)
	     (t
	      reorg-dynamic-bullets-leaf-no-body-text-bullet))
       'face
       (if clone 'reorg-dynamic-bullets-clone-face
	 'reorg-dynamic-bullets-face)))))





;;;;; Refreshing display

(defun reorg-dynamic-bullets--refresh-with-compose-region (beg end &optional remove)
  (if remove
      (decompose-region beg end)
    (compose-region beg end (reorg-dynamic-bullets--create-heading-bullet))))

(defun reorg-dynamic-bullets--refresh-with-text-props (beg end &optional remove)
  "Refresh all bullets from BEG to END."
  ;; This appears to a bit faster than using 
  ;; (font-lock-fontify-region beg end).
  (remove-text-properties beg end '(display nil))
  (unless remove
    (put-text-property beg
		       end
		       'display
		       (reorg-dynamic-bullets--create-heading-bullet))))

(defun reorg-dynamic-bullets--refresh-with-font-lock (beg end &optional remove)
  ;; NOTE: REMOVE is not used right now.  But it is used for
  ;; `reorg-dynamic-bullets--refresh-with-text-props', above. 
  "Refresh all bullets from BEG to END using 
`font-lock-fontify-region'."
  (font-lock-fontify-region beg end))

(defun reorg-dynamic-bullets--fontify (beg end &optional remove)
  "Fontify only the leading stars from BEG to END.
All fontifying functions use this function as their base.  
This function searches the region for the headline regexp and calls 
`reorg-dynamic-bullets-refresh-func' to act on the matches."
  (when reorg-dynamic-bullets-mode 
    (save-excursion
      (save-match-data 
	(goto-char beg)
	(while
	    (re-search-forward reorg-dynamic-bullets--heading-re end t)
	  (save-excursion
	    (funcall reorg-dynamic-bullets-refresh-func
		     (match-beginning 1)
		     (match-end 1))))))))

(defun reorg-dynamic-bullets--fontify-buffer (&rest _)
  "Fontify the entire buffer."
  (reorg-dynamic-bullets--fontify (point-min) (point-max)))

(defun reorg-dynamic-bullets--fontify-tree (&rest _)
  "Fontify the entire tree from root to last leaf."
  (when-let* ((level (outline-level))
	      (beg (save-excursion (if (= 1 level)
				       (progn (beginning-of-line)
					      (point))
				     (while (outline-up-heading nil))
				     (point))))
	      (end (save-excursion (outline-end-of-subtree)
				   (point))))
    (reorg-dynamic-bullets--fontify beg end)))

(defun reorg-dynamic-bullets--fontify-heading (&rest _)
  "Fontify the current heading only."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (reorg-dynamic-bullets--fontify (point-at-bol)
				      (point-at-eol)))))

(defun reorg-dynamic-bullets--fontify-heading-and-previous-sibling (&rest _)
  "Fontify the current heading and previous sibling."
  (let ((beg (save-excursion (or (outline-get-last-sibling)
				 (outline-previous-heading))
			     (point)))
	(end (line-beginning-position 2)))
    (reorg-dynamic-bullets--fontify beg end)))

(defun reorg-dynamic-bullets--fontify-heading-and-parent (&rest _)
  "Fontify the current heading only."
  (let ((beg (save-excursion (outline-up-heading nil)
			     (point)))
	(end (save-excursion (outline-next-visible-heading 1)
			     (point))))
    (reorg-dynamic-bullets--fontify beg end)))

(defun reorg-dynamic-bullets--fontify-children (&rest _)
  "Fontify current heading to last child."
  (save-excursion
    (when (outline-back-to-heading)
      (let ((beg (point-at-bol))
	    (end (save-excursion
		   (outline-end-of-subtree)
		   (point))))
	(reorg-dynamic-bullets--fontify beg end)))))

(defun reorg-dynamic-bullets--org-cycle-hook-func ()
  "Called after `org-cyle'."
  (reorg-dynamic-bullets--fontify-buffer))

;;;; Hooks and advice

(defun reorg-dynamic-bullets--add-hook-or-advice (hook-or-func
						  func
						  &optional remove)
  "If HOOK-OR-FUNC is a hook, add FUNC as a local hook.
If HOOK-OF-FUNC is a function, add FUNC as advice after HOOK-OR-FUNC.
if REMOVE is non-nil, remove the hook or advice."
  (pcase hook-or-func
    ((pred (functionp))
     (if remove
	 (advice-remove hook-or-func func)
       (advice-add hook-or-func :after func)))
    ((pred (lambda (sym)
	     (let ((name (symbol-name sym)))
	       (and (>= (length name) 6)
		    (string=
		     "-hook"
		     (substring name -5 (length name)))))))
     (if remove
	 (remove-hook hook-or-func func t)
       (add-hook hook-or-func func nil t)))))

(defun reorg-dynamic-bullets--add-all-hooks-and-advice (&optional remove)
  "Add hooks and advice to all members of
`reorg-dynamic-bullets-update-triggers.'"
  (cl-loop for (hook-or-func . func) in reorg-dynamic-bullets-update-triggers
	   when (and hook-or-func func)
	   do (reorg-dynamic-bullets--add-hook-or-advice
	       hook-or-func
	       func
	       remove)))

;;;; Footer

(provide 'reorg-dynamic-bullets)

;;; reorg-dynamic-bullets.el ends here
