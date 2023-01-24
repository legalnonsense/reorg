;;; reorg-bullets.el --- Orgmode dynamic bullets -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)
(require 'outline)

;;;; Faces

(defface reorg-bullets-face '((t (:foreground "gray")))
  "Bullet face"
  :group 'reorg-bullets)

;; (defface reorg-bullets-clone-face '((t (:foreground "red")))
;;   "Clone face"
;;   :group 'reorg-bullets)

;;;; Customization

(defcustom reorg-bullets-update-triggers
  '((outline-cycle . reorg-bullets--org-cycle-hook-func))
  "Alist in the form '((hook-or-function . refresh-function)))
where refresh-function is added as a hook or advice to hook-or-function.
Refresh function should be one of the `reorg-bullets--fontify' functions
below."
  :type 'alist
  :group 'reorg-bullets)

(defcustom reorg-bullets-folded-body-text-bullet "▶"
  "Bullet for a folded node with text in its body."
  :type 'string
  :group 'reorg-bullets)

(defcustom reorg-bullets-folded-no-body-text-bullet "▷"
  "Bullet for folded node with no text in its body."
  :type 'string
  :group 'reorg-bullets)

(defcustom reorg-bullets-unfolded-body-text-bullet "▼"
  "Bullet for an unfolded node with text in its body."
  :type 'string
  :group 'reorg-bullets)

(defcustom reorg-bullets-unfolded-no-body-text-bullet "▽"
  "Bullet for an unfolded node with no text in its body."
  :type 'string
  :group 'reorg-bullets)

(defcustom reorg-bullets-leaf-body-text-bullet "▬"
  "Bullet for a leaf node with body text."
  :type 'string
  :group 'reorg-bullets)

(defcustom reorg-bullets-leaf-no-body-text-bullet "▭"
  "Bullet for a leaf node with no body text."
  :type 'string
  :group 'reorg-bullets)

(defcustom reorg-bullets-refresh-func
  #'reorg-bullets--refresh-with-text-props
  "Function to refresh bullets.  Must take
two arguments: BEG and END representing the region
to be refreshed. Two options are:
`reorg-bullets--refresh-with-compose-region',
`reorg-bullets--refresh-with-text-props', or
`reorg-bullets--refresh-with-font-lock'."
  :type 'function
  :group 'reorg-bullets)

;;;; Variables

(defvar reorg-bullets--idle-timer nil
  "Idle timer to update heading bullets.")

;;;; Constants

(defconst reorg-bullets--heading-re "^\\(?1:\\*+\\) "
  "Outline heading regexp.")

;; (defconst reorg-bullets--font-lock-keyword
;;   `((,reorg-bullets--heading-re
;;      (1 (list 'face 'reorg-bullets-face
;;  	      'display (reorg-bullets--create-heading-bullet)))))
;;   "Font-lock keyword to fontify heading stars.")

(defconst reorg-bullets--font-lock-keyword
  `((,reorg-bullets--heading-re
     (1 (progn (compose-region (match-beginning 1) (match-end 1)
			       (reorg-bullets--create-heading-bullet))
	       nil))))
  "Font-lock keyword to fontify heading stars.")

;;;; Minor mode

(define-minor-mode reorg-bullets-mode
  "Display orgmode trees."
  :global nil
  :lighter ""
  (if reorg-bullets-mode
      (progn 
	(reorg-bullets--add-all-hooks-and-advice)
	;;(cl-pushnew 'display font-lock-extra-managed-props)
	;;(font-lock-add-keywords nil reorg-bullets--font-lock-keyword)
	(reorg-bullets--fontify-buffer))
    ;; (setq reorg-bullets--idle-timer
    ;;       (run-with-idle-timer 1 'repeat #'reorg-bullets--fontify-heading)))
    ;; (font-lock-remove-keywords nil reorg-bullets--font-lock-keyword)
    (reorg-bullets--add-all-hooks-and-advice 'remove)
    (reorg-bullets--fontify (point-min) (point-max) 'remove)
    ;; (cancel-timer reorg-bullets--idle-timer)
    ;; (font-lock-flush (point-min) (point-max))
    ;; (font-lock-ensure (point-min) (point-max))
    ))

;;;; Functions

;;;;; Outline position predicates

(defun reorg-bullets--heading-folded-p ()
  "Is the current heading folded?"
  (and (reorg--get-prop 'reorg-branch)
       (not
	(reorg--get-next-visible-child))))

;;;;; Creating prefix strings

(defun reorg-bullets--children-p ()
  "Does the current heading have children?"
  (reorg--get-next-child))

;; (defun reorg-bullets--create-heading-bullet ()
;;   "Create a string to be displayed in lieu of the headings' leading stars."
;;   (let ((folded (reorg-bullets--heading-folded-p))
;; 	(children (reorg--get-next-child))
;; 	(body (reorg--get-prop 'body)))
;;     (propertize 
;;      (cond ((and children folded body)
;; 	    reorg-bullets-folded-body-text-bullet)
;; 	   ((and children folded)
;; 	    reorg-bullets-folded-no-body-text-bullet)
;; 	   ((and children body)
;; 	    reorg-bullets-unfolded-body-text-bullet)
;; 	   (children
;; 	    reorg-bullets-unfolded-no-body-text-bullet)
;; 	   (body
;; 	    reorg-bullets-leaf-body-text-bullet)
;; 	   (t
;; 	    reorg-bullets-leaf-no-body-text-bullet))
;;      'face
;;      'reorg-bullets-face)))

(defun reorg-bullets--create-heading-bullet ()
  "Create a string to be displayed in lieu of the headings' leading stars."
  (let (;;(branch (reorg--get-prop 'reorg-branch))
	(branch (and (reorg--get-prop 'reorg-branch)
		     (reorg--get-next-child)))
	(folded (reorg-bullets--heading-folded-p))
	(body nil))
    ;; (body (reorg--get-prop 'body)))
    (propertize 
     (cond ((and branch folded body)
	    (or (reorg--get-prop 'folded-bullet)
		(reorg--get-prop 'bullet)
		reorg-bullets-folded-body-text-bullet))
	   ((and branch folded)
	    (or (reorg--get-prop 'folded-bullet)
		(reorg--get-prop 'bullet)
		reorg-bullets-folded-no-body-text-bullet))
	   ((and branch body)
	    (or (reorg--get-prop 'unfolded-bullet)
		(reorg--get-prop 'bullet)
		reorg-bullets-unfolded-no-body-text-bullet))
	   (branch
	    (or (reorg--get-prop 'unfolded-bullet)
		(reorg--get-prop 'bullet)
		reorg-bullets-unfolded-no-body-text-bullet))
	   (body
	    (or (reorg--get-prop 'leaf-bullet)
		(reorg--get-prop 'bullet)
		reorg-bullets-leaf-body-text-bullet))
	   (t
	    (or (reorg--get-prop 'leaf-bullet)
		(reorg--get-prop 'bullet)
		reorg-bullets-leaf-no-body-text-bullet))))))
     ;; 'face
     ;; 'reorg-bullets-face)))

;;;;; Refreshing display

(defun reorg-bullets--refresh-with-compose-region (beg end &optional remove)
  "refresh with compose region"
  (decompose-region beg end)
  (compose-region beg end (reorg-bullets--create-heading-bullet)))

(defun reorg-bullets--refresh-with-text-props (beg end &optional remove)
  "Refresh all bullets from BEG to END."
  ;; This appears to a bit faster than using 
  ;; (font-lock-fontify-region beg end).
  (remove-text-properties beg end '(display nil))
  (unless remove
    (put-text-property beg
		       end
		       'display
		       (reorg-bullets--create-heading-bullet))))

(defun reorg-bullets--refresh-with-font-lock (beg end &optional remove)
  ;; NOTE: REMOVE is not used right now.  But it is used for
  ;; `reorg-bullets--refresh-with-text-props', above. 
  "Refresh all bullets from BEG to END using 
`font-lock-fontify-region'."
  (font-lock-fontify-region beg end))

(defun reorg-bullets--fontify (beg end &optional remove)
  "Fontify only the leading stars from BEG to END.
All fontifying functions use this function as their base.  
This function searches the region for the headline regexp and calls 
`reorg-bullets-refresh-func' to act on the matches."
  (let ((search-invisible nil))
    (save-match-data
      (save-excursion
	(goto-char beg)
	(while (and (let ((b (point))
			  (e (when (re-search-forward
				    reorg-bullets--heading-re
				    (point-at-eol)
				    t)
			       (match-end 1))))
		      (when (and b e)
			(funcall reorg-bullets-refresh-func
				 b e))
		      (reorg--goto-next-visible-heading))
		    (< (point) end)))))
    (run-hooks 'reorg--navigation-hook)))


(defun reorg-bullets--fontify-buffer (&rest _)
  "Fontify the entire buffer."
  (reorg-bullets--fontify (point-min) (point-max)))

(defun reorg-bullets--fontify-tree (&rest _)
  "Fontify the entire tree from root to last leaf."
  (when-let* ((beg (point))
	      ;; (if (= 1 level)
	      ;;     (progn (beginning-of-line)
	      ;; 	      (point))
	      ;;   (reorg--get-root))
	      (end (or (reorg--get-next-sibling)
		       (reorg--get-next-parent)
		       (point-max))))
    (reorg-bullets--fontify beg end)))

(defun reorg-bullets--fontify-heading (&rest _)
"Fontify the current heading only."
(save-excursion
  (reorg-bullets--fontify (point-at-bol)
				  (point-at-eol))))


;; (defun reorg-bullets--fontify-heading-and-previous-sibling (&rest _)
;;   "Fontify the current heading and previous sibling."
;;   (let ((beg (save-excursion (or (outline-get-last-sibling)
;; 				   (outline-previous-heading))
;; 			       (point)))
;; 	  (end (line-beginning-position 2)))
;;     (reorg-bullets--fontify beg end)))

(defun reorg-bullets--fontify-heading-and-parent (&rest _)
  "Fontify the current heading only."
  (let ((beg (reorg--get-parent))
	(end (reorg--get-next-visibile-heading)))
    (reorg-bullets--fontify beg end)))

;; (defun reorg-bullets--fontify-children (&rest _)
;;   "Fontify current heading to last child."
;;   (save-excursion
;;     (when (outline-back-to-heading)
;; 	(let ((beg (point-at-bol))
;; 	      (end (save-excursion
;; 		     (outline-end-of-subtree)
;; 		     (point))))
;; 	  (reorg-bullets--fontify beg end)))))

(defun reorg-bullets--org-cycle-hook-func ()
  "Called after `org-cyle'."
  (reorg-bullets--fontify-tree))

;;;; Hooks and advice

(defun reorg-bullets--add-hook-or-advice (hook-or-func
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

(defun reorg-bullets--add-all-hooks-and-advice (&optional remove)
  "Add hooks and advice to all members of
`reorg-bullets-update-triggers.'"
  (cl-loop for (hook-or-func . func) in reorg-bullets-update-triggers
	   when (and hook-or-func func)
	   do (reorg-bullets--add-hook-or-advice
	       hook-or-func
	       func
	       remove)))

;;;; Footer

(provide 'reorg-bullets)
;;; reorg-bullets.el ends here
