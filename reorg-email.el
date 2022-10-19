;; -*- lexical-binding: t; -*-

(reorg-create-class-type
 :name email
 :getter (cl-loop for each in (s-split "\n" (shell-command-to-string
					     SOURCE)
				       'omit-nil)
		  collect (PARSER (read each)))
 :keymap (("o" . (lambda () (interactive) (mu4e-view (reorg--get-view-prop 'old-data)))))
 :extra-props ( face '(:underline t)
		))

(reorg-create-data-type
 :class email 
 :name sender
 :parse (let ((xxx (plist-get data :from)))
	  (or (caar xxx)
	      (cadr xxx)
	      "")))

(reorg-create-data-type
 :class email 
 :name to 
 :parse (cl-loop for each in (plist-get data :to)
		 concat (or (car each)
			    (cdr each)
			    "")))

(reorg-create-data-type
 :class email
 :name date
 :parse (format-time-string "%Y-%m-%d %H:%M" (plist-get data :date)))

(reorg-create-data-type
 :class email
 :name subject
 :parse (plist-get data :subject))

(reorg-create-data-type
 :class email
 :name old-data
 :parse data)




(provide 'reorg-email)
