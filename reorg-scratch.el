;; -*- lexical-binding: t; -*-

;; 1.


(let* ((template '( :group .@tag-list
		    :format-string (concat " " .headline)
		    :children (( :group .@at-name
				 :format-string (concat " " .headline)))))
       (results (--> (reorg--getter '((org . "~/.emacs.d/lisp/reorg/TESTS/new.org")))
		     (reorg--group-and-sort it template))))
  results)

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

       "sadfasdaa")
     (buffer-substring-no-properties (point-min) (point-max))))




