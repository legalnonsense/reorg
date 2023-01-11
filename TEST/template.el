


(setq reorg-test-template
      '( :sources ((org . "test.org"))
	 :group "Reorg example"
	 :children
	 (( :group (when .timestamp-ia
		     "Headings with timestamps, sorted by timestamp")
	    :format-results (.stars " " .headline)
	    :sort-results ((.timestamp-ia . string<)))
	  ( :group (concat "Headings grouped by each @name in the heading, "
			   "then grouped by tag, but don't display timestamps")
	    :children (( :group .@at-names
			 :children (( :group .tags
				      :sort-groups string<				   
				      :format-results (.stars
						       " "
						       (replace-regexp-in-string
							org-ts-regexp-inactive
							""
							.headline)))))))
	  ( :group "Headings with TODO keywords"
	    :children (( :group .todo
			 :format-results (.priority "\t" (upcase .headline))
			 :sort-results ((.tags . string<)))))
	  ( :group "All headings, but only IDs displayed"
	    :format-results (.id)))))
	  ;; ( :sources ((elisp . "../reorg.el"))
	  ;;   :group (when (member .form-type '("defvar" "defcustom" "defconst"))
	  ;; 	     (concat 
	  ;; 	      "All functions in the reorg source code, "
	  ;; 	      "with the reorg- prefix removed, sorted alpabetically, "
	  ;; 	      "and grouped by type, and displayed with a large indent"))
	  ;;   :children (( :group .form-type 
	  ;; 		 :format-results ((propertize " " 'display '(space . (:align-to 30)))
	  ;; 				  (replace-regexp-in-string "reorg-?-"
	  ;; 							    ""
	  ;; 							    .form-name))
	  ;; 		 :sort-results ((.form-name . string<)))))
	  ;; ( :sources ((files . "find ~/.emacs.d/lisp/reorg/ -type f | grep .el$"))
	  ;;   :group (when (string= .extension "el")
	  ;; 	     (concat 
	  ;; 	      "All reorg elisp files, grouped by length of file name, "
	  ;; 	      "with smiles next to filenames containing an A"))
	  ;;   :children (( :group (number-to-string (length .filename))
	  ;; 		 :sort-results string>
	  ;; 		 :sort-groups (lambda (a b) (< (string-to-number a)
	  ;; 					       (string-to-number b)))
	  ;; 		 :format-results ((concat
	  ;; 				   (when (s-contains-p "a" .filename)
	  ;; 				     ":-) ")
	  ;; 				   .filename))))))))


(reorg-open-in-current-window reorg-test-template)


