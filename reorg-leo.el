;; -*- lexical-binding: t; -*-

(defun reorg-leo--render-func (&optional id buffer)
  "render func"
  (let ((id (or id (reorg--get-prop 'id)))
	(buffer (or buffer (reorg--get-prop 'buffer))))
    (reorg--select-main-window buffer)
    ;; (switch-to-buffer buffer) 
    (widen)
    (goto-char (point-min))
    (re-search-forward (concat "<t tx=\""
			       id
			       "\">"))
    (setq b (match-end 0))
    (goto-char (match-beginning 0))
    (re-search-forward "</t>")
    (setq e (match-beginning 0))
    (goto-char b)
    (narrow-to-region b e)))
    ;;(reorg--select-tree-window)))

(reorg-create-class-type
 :name leo
 :getter
 (cl-loop for each in
	  (cl-flet ((get-t (x) (cdar (nth 1 x)))
		    (get-vh (x) (nth 2 (nth 2 x))))
	    (let* ((level 0)
		   (buffer (find-file-noselect SOURCE))
		   (data (with-current-buffer buffer
			   (widen)
			   (libxml-parse-xml-region (point-min)
						    (point-max))))	   
		   (vnodes (cddar (cdddr (cdddar (cdddr data))))))
	      (cl-labels ((zzz (d l n)
			       (cl-loop for each in d
					collect
					(list
					 (cons 'leo-level l)
					 (cons 'body (car
						      (last
						       (car
							(dom-elements
							 data 'tx (get-t each))))))
					 (cons 'id (get-t each))
					 (cons 'headline (get-vh each))
					 (cons 'buffer buffer)
					 (cons 'order (cl-incf n)))
					append (when (subseq each 3)
						 (zzz (subseq each 3) (1+ l) n)))))
		(zzz vnodes level 1))))
	  collect (PARSER each))
 :render-func reorg-leo--render-func)

(reorg-create-data-type
 :class leo
 :name at-node
 :parse (and (s-starts-with-p "@" (alist-get 'headline data))
	     (string-match "^@\\([^[:space:]]+\\)" (alist-get 'headline data))
	     (match-string 1 (alist-get 'headline data))))

(reorg-create-data-type
 :class leo
 :name leo-level
 :parse (alist-get 'leo-level data))

(reorg-create-data-type
 :class leo
 :name body
 :parse (alist-get 'body data))

(reorg-create-data-type
 :class leo
 :name headline
 :parse (alist-get 'headline data))

(reorg-create-data-type
 :class leo
 :name buffer
 :parse (alist-get 'buffer data))

(reorg-create-data-type
 :class leo
 :name order
 :parse (alist-get 'order data))

(reorg-create-data-type
 :class leo
 :name id
 :parse (alist-get 'id data))

(provide 'reorg-leo)
