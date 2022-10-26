;; -*- lexical-binding: t; -*-

;; 1.


(let* ((template '( :group .@tag-list
		    :format-string (concat " " .headline)
		    :children (( :group .@at-name
				 :format-string (concat " " .headline)))))
       (results (--> (reorg--getter '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org")))
		     (reorg--group-and-sort it template))))
  results)

(defun reorg-insert--find-by-prop (&optional backward &rest )
  "find the first branch that matches LEVEL."
  (when-let* ((func (if backward
			#'text-property-search-backward
		      #'text-property-search-forward))
	      (match (funcall func 
			      'reorg-data
			      id
			      (lambda (a b) (string= (alist-get 'id b)
						     a)))))
    (goto-char (prop-match-beginning match))))

(setq yyy (reorg-insert--find-first-branch "743034db81a0b72264db2b225e3ae9e9"))



(insert xxx)* ccc



;; is the current data a branch or leaf?
;; if it is a heading, is the heading present?
;; Go to it. loop to next data
;; if it is a leaf, insert it at the appropriate point


;; heading present?
;; subheading present?
;; find header location 

(md5 (with-temp-buffer
       (insert
	(pp '(a b)))
       (buffer-string)))





