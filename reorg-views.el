;;; -*- lexical-binding: t; -*-

;; (defun reorg-open-view (name &optional source) "NAME is the name of
;;   the view.  SOURCE is the source buffer."
;;   (interactive)
;;   (let ((data (reorg--iterate-over-source-buffer name)))
;;     (reorg--insert-all-headings-into-view data)
;;     (reorg--open-in-side-window)))

;;; view buffer functions

(defun reorg-view--update-highlight-overlay (&optional &rest _args)
  "update transclusion overlay."
  nil)
  ;; (delete-overlay reorg-current-heading-overlay)
  ;; (move-overlay reorg-current-heading-overlay (reorg--get-headline-start) (point-at-eol)))

(defun reorg--initialize-overlay ()
  "initialize the transclusion overlay."
  nil)
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

(defun reorg-view--update-view-headline ()
  "Goto source buffer, re-parse, update."
  (let ((props (reorg--with-point-at-orig-entry
		 (reorg-parser--headline-parser)))
	(inhibit-modification-hooks t))
    (reorg-props 'headline :val (propertize (plist-get props :headline)
					    reorg--data-property-name props))))

(defun reorg-view--tree-to-source--goto-heading (&optional id buffer no-narrow no-select)
  "Goto ID in the source buffer. If NARROW is non-nil, narrow to the heading."
  (interactive)
  (when  (and (or buffer (reorg--get-view-prop :buffer))
	      (or id (reorg--get-view-prop :id)))
    (if reorg-parser-use-id-p 
	(reorg-view--goto-source-id
	 (or buffer (reorg--get-view-prop :buffer))
	 (or id (reorg--get-view-prop :id))
	 (not no-narrow))
      (reorg-view--goto-source-marker 
       (or buffer (reorg--get-view-prop :buffer))
       (or id (reorg--get-view-prop :marker))
       (not no-narrow)))))

(defun reorg-view--source--goto-end-of-meta-data ()
  "Go to the end of the meta data and insert a blank line
if there is not one."
  (let ((next-heading (org-with-wide-buffer
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
  (widen)
  (org-show-all)
  (reorg-view--source--goto-end-of-meta-data)
  (narrow-to-region (save-excursion
		      (org-back-to-heading))
		    (save-excursion 
		      (outline-next-heading)
		      (point))))

(defun reorg-view--goto-source-id (buffer id &optional narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (reorg--select-main-window)
  (set-window-buffer (selected-window) buffer)
  (widen)
  (let ((old-point (point)))
    (goto-char (point-min))
    (if (re-search-forward id nil t)
	(progn (org-back-to-heading)
	       (when narrow
		 (reorg-view--source--narrow-to-heading))
	       t)
      (goto-char old-point)
      nil)))

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
  (org-next-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-previous-entry-follow ()
  (interactive)
  (org-previous-visible-heading 1)
  (reorg-view--update-highlight-overlay)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--move-to-next-entry-no-follow ()
  (interactive)
  (org-next-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  )

(defun reorg--move-to-previous-entry-no-follow ()
  (interactive)
  (org-previous-visible-heading 1)
  (reorg-edits--post-field-navigation-hook)
  (reorg-view--update-highlight-overlay)
  (reorg-view--tree-to-source--goto-heading)
  (reorg--select-tree-window)
  (reorg-edits--post-field-navigation-hook))

(defun reorg--goto-next-parent ()
  "Goto the next parent."
  (interactive)
  (when (re-search-forward (concat "^*\\{" (number-to-string (1- (org-current-level))) "\\} ") nil t)
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



;;;;; Major mode

(defvar reorg-view-mode-map 
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'reorg-view--tree-to-source--goto-heading)
    (define-key map (kbd "e") #'reorg-edits--start-edit)
    (define-key map (kbd "u") #'reorg--goto-parent)
    (define-key map (kbd "f") #'reorg-edits-move-to-next-field)
    (define-key map (kbd "b") #'reorg-edits-move-to-previous-field)
    (define-key map (kbd "U") #'reorg--goto-next-parent)
    (define-key map (kbd "n") #'reorg--move-to-next-entry-no-follow)
    (define-key map (kbd "p") #'reorg--move-to-previous-entry-no-follow)
    (define-key map (kbd "TAB") #'outline-cycle)
    map)
  "keymap")

(define-derived-mode reorg-view-mode outline-mode
  "Org tree view"
  "Tree view of an Orgmode file. \{keymap}"
  (reorg--initialize-overlay)
  (setq cursor-type nil)
  (reorg-dynamic-bullets-mode)
  (org-visual-indent-mode)
  (use-local-map reorg-view-mode-map))




(provide 'reorg-views)
