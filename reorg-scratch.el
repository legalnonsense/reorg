;; ;; ;; -*- lexical-binding: t; -*-
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.






(funcall (let ((xxx `(concat "a" (concat "c" "a" "b"))))
	   `(lambda ()
	      ,xxx)))


(setq xxx-seq '(((a . ("1" "3" "4")) (b . "3"))
		((a . ("4" "5" "5")) (b . "3"))
		((a . ("4" "5" "5")) (b . "7"))
		((a . ("2" "3" "4")) (b . "6")))
      xxx-sym '(concat .!a .!a .b)
      xxx-n 1)

(xxx xxx-seq xxx-sym xxx-n)

(defun xxx (seq sym n)
  ;; (reorg--seq-group-by
  `(lambda (x)
     (eval ;; I know.
      (let-alist x
	(reorg--walk-tree
	 ',sym
	 (lambda (xx)
	   (reorg--turn-dot-bang-to-val xx ,(or n 0) x)))))))
;; seq))
