;; -*- lexical-binding: t; -*-

;;; syncing

;; (defmacro reorg-view--make-change-in-org-buffer-and-sync-clones (&rest body)
;;   "asdf"
;;   `(let (data)
;;      (reorg--with-point-at-orig-entry nil nil
;; 				      ,@body
;; 				      (setq data (reorg--parser)))
;;      (reorg--map-id (plist-get data :id)
;; 		    (reorg-views--replace-heading data))))



;;; shortcuts

;; (defmacro reorg--create-org-shortcut (name func shortcut)
;;   (progn
;;     (let ((func-name (intern (concat "reorg--org-shortcut-"
;; 				     (symbol-name name)))))
;;       `(progn
;; 	 (defun ,func-name (arg)
;; 	   ,(format "Execute %s in the source buffer and update the heading at point."
;; 		    func)
;; 	   (interactive "P")
;; 	   (reorg--with-source-and-sync
;; 	     (if (eq 'org-set-property ',func)
;; 		 (funcall-interactively #',func arg nil)
;; 	       (funcall-interactively #',func arg))))
;; 	 (define-key reorg-view-mode-map (kbd ,shortcut) #',func-name)))))

;; (reorg--create-org-shortcut tag org-set-tags-command "C-c C-c")
;; (reorg--create-org-shortcut todo org-todo "C-c C-t")
;; (reorg--create-org-shortcut
;;  headline
;;  (lambda (_arg) (org-edit-headline (read-string "New headline: "
;; 						(org-get-heading t t t t))))
;;  "C-c C-e")
;; (reorg--create-org-shortcut deadline org-deadline "C-c C-d")
;; (reorg--create-org-shortcut schedule org-schedule "C-c C-s")
;; (reorg--create-org-shortcut property org-set-property "C-c C-x p")
;; (reorg--create-org-shortcut priority org-priority "C-c C-p")


;;; helpers 





;; (defun reorg--parser ()
;;   "Create a plist using `reorg-parser-list' for each org heading."
;;   (cl-loop with result = nil
;; 	   for (name . func) in reorg-parser-list
;; 	   append (list (reorg--add-remove-colon name) (funcall func)) into result
;; 	   finally return result))


;; (setq reorg-mapping-function #'reorg--org-ql)








;;;; headlines



(provide 'reorg-org-extras)
