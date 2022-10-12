;; -*- lexical-binding: t; -*-


(defun reorg--process-results (data &optional format-string)
  "Process the results of `reorg--group-and-sort' and turn them into orgmode headings."
  (setq format-string (or format-string reorg-headline-format))
  (let (results)
    (cl-labels ((recurse (data level)
			 (cl-loop for entry in data
				  do (push (reorg--create-headline-string (car entry)
									  format-string
									  level)
					   results)
				  if (reorg--plist-p (caadr entry))
				  do (cl-loop
				      for x in (cadr entry)
				      do (push (reorg--create-headline-string x
									      format-string
									      (1+ level))
					       results))
				  else do (cl-loop for e in (cdr entry)
						   do (recurse e (1+ level))))))
      (recurse data 1))
    (reverse results)))



;; (funcall `(lambda () ,(reorg--depth-first-apply '(concat "*** " .headline) #'reorg--turn-dot-to-field xxx))) ;
;;;XXX 



;; (cl-flet ((create-stars (num &optional data)
;; 			  (make-string (if (functionp num)
;; 					   (funcall num data)
;; 					 num)
;; 				       ?*)))
;;   (propertize (create-stars (plist-get data :reorg-stars))
;; 		reorg--field-property-name 'stars)))




;; (reorg--create-headline-string xxx '(concat (propertize .headline 'xxx 'yyy) " " .ts) 5) 


;; (cl-loop for each in format-string
;; 		if (stringp (car each))
;; 		concat (car each)
;; 		else if (eq 'stars (car each))
;; 		concat (propertize (create-stars level) reorg--field-property-name 'stars) 
;; 		else if (eq 'property (car each))
;; 		concat (apply (intern (concat "reorg-display--" (symbol-name (car each))))
;; 			      data
;; 			      (cdr each))
;; 		else if (eq 'align-to (car each))
;;  	concat (propertize  " " 'display `(space . (:align-to ,(cadr each))))
;; 		else if (eq 'pad (car each))
;; 		concat (make-string (cadr each) ? )
;; 		else
;; 		concat (apply (intern (concat "reorg-display--" (symbol-name (car each))))
;; 			      data
;; 			      (cdr each)))

(provide 'reorg-display)
