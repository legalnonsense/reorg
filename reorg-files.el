;; -*- lexical-binding: t; -*-


(reorg-create-class-type
 :name files
 :getter (cl-loop for each in (s-split "\n" (shell-command-to-string
					     SOURCE))
		  collect (PARSER each)))

(reorg-create-data-type
 :name depth 
 :class files
 :display (number-to-string (alist-get 'depth alist))
 :parser (f-depth data))

(reorg-create-data-type
 :name extension
 :class files
 :parser (f-ext data))

(reorg-create-data-type
 :name filename
 :class files
 :display (concat "_" (alist-get 'filename alist) "_")
 :parser (f-filename data))

(reorg-create-data-type
 :name parent
 :class files
 :parser (f-parent data))


(provide 'reorg-files)
