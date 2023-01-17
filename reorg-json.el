;; -*- lexical-binding: t; -*-

(defun reorg-json--render-func ()
  ""
  (when-let ((marker (reorg--get-prop 'marker)))
    (reorg--select-main-window (marker-buffer marker))
    (widen)
    (goto-char marker)
    (narrow-to-region
     (point)
     (save-excursion 
       (forward-sexp)
       (point))))
  (reorg--select-tree-window))

(reorg-create-class-type
 :name json
 :render-func reorg-json--render-func
 :getter (with-current-buffer (find-file-noselect SOURCE)
	   (widen)
	   (goto-char (point-min))
	   (cl-loop with buffer = (current-buffer)
		    with file = (buffer-file-name)
		    with json-array-type = 'list
		    with json-key-type = 'symbol
		    with json-object-type = 'alist
		    with json-null = nil
		    with json-false = nil
		    while (re-search-forward (rx line-start
						 (one-or-more space)
						 "{")
					     nil t)
		    do (goto-char (match-beginning 0))
		    collect (append (list (cons 'marker (point-marker))
					  (cons 'file file)
					  (cons 'class 'json)
					  (cons 'buffer buffer))
				    (json-read))
		    do (goto-char (point-at-eol)))))

;; (reorg-create-class-type
;;  :name json
;;  :render-func reorg-json--render-func
;;  :getter (with-current-buffer (find-file-noselect SOURCE)
;; 	   (let ((json-array-type 'list)
;; 		 (json-key-type 'symbol)
;; 		 (json-object-type 'alist)
;; 		 (json-null nil)
;; 		 (json-false nil)
;; 		 (file (buffer-file-name))
;; 		 (buffer (current-buffer)))
;; 	     (save-excursion
;; 	       (widen)
;; 	       (goto-char (point-min))
;; 	       (cl-loop for each in (json-read)
;; 			collect (append (list (cons 'file file)
;; 					      (cons 'buffer buffer)
;; 					      (cons 'class 'json))
;; 					each))))))

(provide 'reorg-json)
