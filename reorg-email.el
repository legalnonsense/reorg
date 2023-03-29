;; -*- lexical-binding: t; -*-
(require 'reorg)

(reorg-create-class-type
 :name email
 :render-func (lambda ()
		(let ((data (reorg--get-prop 'mu4e-data)))
		  (reorg--select-main-window)
		  (mu4e-view data)
		  (reorg--select-tree-window)))
 :getter (cl-loop for each in (s-split "\n" (shell-command-to-string
					     (concat "mu find "
						     SOURCE
						     " --format=sexp"))
				       'omit-nil)
		  collect (PARSER (read each))))

(reorg-create-data-type
 :class email 
 :name from
 :parse (when-let ((from (car-safe (plist-get data :from))))
	  (plist-get from :name)))

(reorg-create-data-type
 :class email 
 :name to 
 :parse (cl-loop for each in (plist-get data :to)
		 concat (plist-get each :email)))

(reorg-create-data-type
 :class email
 :name path
 :parse (plist-get data :path))

(reorg-create-data-type
 :class email
 :name date
 :parse (format-time-string
	 "%Y-%m-%d %H:%M"
	 (plist-get data :date)))

(reorg-create-data-type
 :class email
 :name subject
 :parse (plist-get data :subject))

(reorg-create-data-type
 :class email
 :name mu4e-data
 :parse data)

(provide 'reorg-email)
