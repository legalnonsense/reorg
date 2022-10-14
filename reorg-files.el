;; -*- lexical-binding: t; -*-

(reorg-create-class-type
 :name files
 :getter (cl-loop for each in (s-split "\n" (shell-command-to-string
					     SOURCE))
		  collect (PARSER each))
 :keymap (("x" . (lambda () (interactive) (message "adf")))
	  ("y" . (lambda () (interactive) (message "yyyy")))
	  ("o" . (lambda () (interactive)
		   (xdg-open (reorg--get-view-prop 'path) t))))
 :extra-props ( test asfasfd
		face '(:underline t)))

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
 :name parent
 :class files
 :parse (f-parent data))


(provide 'reorg-files)
