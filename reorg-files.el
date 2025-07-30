;; -*- lexical-binding: t; -*-
(require 'reorg)



(reorg-create-class-type
 :name files
 :getter (cl-loop for each in (ensure-list SOURCE)
		  append (cl-loop for file in (s-split "\n" (shell-command-to-string
							     (concat 
							      "find "
							      (eshell-quote-argument each)
							      " -type f"
							      ))
						       t)
				  collect (PARSER file)))
 :keymap (("e" . (lambda ()
		   (interactive)
		   (let ((file (reorg--get-prop 'fullname)))
		     (reorg--select-main-window)
		     (find-file file))))
	  ("d" . (lambda () (interactive) (dired (reorg--get-prop 'parent))))
	  ("o" . (lambda () (interactive)
		   (xdg-open (reorg--get-prop 'path))))))

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
 :name class
 :class files
 :parse 'files)

(reorg-create-data-type
 :name dirp
 :class files
 :parse (f-dir-p data))

(reorg-create-data-type
 :name depth 
 :class files
 :parse (f-depth data))

(reorg-create-data-type
 :name path
 :class files
 :parse data)

(reorg-create-data-type
 :name path-list
 :class files
 :parse (seq-subseq (s-split "/" data t) 6))

(reorg-create-data-type
 :class files
 :name client-parent-dirs
 :parse (car (butlast (nthcdr 6 (s-split "/" data t)))))

(reorg-create-data-type
 :name parent-dirs
 :class files 
 :parse (butlast (s-split "/" data t)))

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
 :disable t
 :class files
 :parse (org-id-uuid))



(provide 'reorg-files)
