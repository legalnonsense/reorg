;; -*- lexical-binding: t; -*-

(defun leo-parse-file (file &optional level)
  (cl-flet ((get-t (x) (cdar (nth 1 x)))
	    (get-vh (x) (nth 2 (nth 2 x))))
    (let* ((level (or level 1))
	   (buffer (find-file-noselect file))
	   (data (with-current-buffer buffer
		   (libxml-parse-xml-region (point-min)
					    (point-max))))	   
	   (vnodes (cddar (cdddr (cdddar (cdddr data))))))
      (cl-loop for each in vnodes
	       collect
	       (list 
		(propertize (concat
			     (make-string level ?*) " "
			     (get-vh each) "\n")
			    'level level
			    'body (car
				   (last
				    (car
				     (dom-elements
				      data 'tx (get-t each)))))
			    'id (get-t each)
			    'headline (get-vh each))
		(when (subseq each 3)
		  (zzz (subseq each 3) (1+ level))))))))

(leo-parse-file "~/.leo/workbook.leo")


