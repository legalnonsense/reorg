;; -*- lexical-binding: t; -*-

(reorg-create-class-type
 :name files
 :getter (cl-loop for each in (s-split "\n" (shell-command-to-string
					     SOURCE)
				       t)
		  collect (PARSER each))
 :keymap (("x" . (lambda () (interactive) (message "adf")))
	  ("e" . (lambda ()
		   (interactive)
		   (let ((file (reorg--get-view-prop 'fullname)))
		     (reorg--select-main-window)
		     (find-file file))))

	  ("y" . (lambda () (interactive) (message "yyyy")))
	  ("d" . (lambda () (interactive) (dired (reorg--get-view-prop 'parent))))
	  ("o" . (lambda () (interactive)
		   (xdg-open (reorg--get-view-prop 'path))))))

;; (defun reorg--files--get-from-source
;;     (&rest sources) 
;;   (cl-flet
;;       ((PARSER
;; 	(&optional d)
;; 	(reorg--parser d 'files)))
;;     ))

;; (defun xxx-error (&rest sources)
;;   (cl-loop for SOURCE in sources append
;; 	   (cl-loop for each in
;; 		    (s-split "\n"
;; 			     (shell-command-to-string SOURCE)
;; 			     t)
;; 		    collect
;; 		    (reorg--parser each 'files))))

(reorg-create-data-type
 :name depth 
 :class files
 :parse (f-depth data))

(reorg-create-data-type
 :name path
 :class files
 :parse data)

(reorg-create-data-type
 :name extension
 :class files
 :parse (f-ext data))

(reorg-create-data-type
 :name filename
 :class files
 :parse (f-filename data))

(reorg-create-data-type
 :name fullname
 :class files
 :parse data)

(reorg-create-data-type
 :name parent
 :class files
 :parse (f-parent data))

(reorg-create-data-type
 :name id
 :class files
 :parse (org-id-uuid))

(provide 'reorg-files)
