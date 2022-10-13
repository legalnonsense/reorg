;; -*- lexical-binding: t; -*-

(reorg-create-class-type
 :name files
 :getter (cl-loop for each in (s-split "\n" (shell-command-to-string
					     SOURCE))
		  collect (PARSER each)))

(reorg-create-data-type
 :name depth 
 :class files
 :display "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
 :parse (f-depth data))

(reorg-create-data-type
 :name extension
 :class files
 :parse (f-ext data))

(reorg-create-data-type
 :name filename
 :class files
 :display (concat "_" (alist-get 'filename alist) "_")
 :parse (f-filename data))

(reorg-create-data-type
 :name parent
 :class files
 :parse (f-parent data))


(provide 'reorg-files)
