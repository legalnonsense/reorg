;; -*- lexical-binding: t; -*-


(defun reorg--dot-at-search (data)
  "Return alist of symbols inside DATA that start with a `.'.
Perform a deep search and return an alist where each car is the
symbol, and each cdr is the same symbol without the `.'."
  (cond
   ((symbolp data)
    (let ((name (symbol-name data)))
      (when (string-match "\\`\\.@" name)
        ;; Return the cons cell inside a list, so it can be appended
        ;; with other results in the clause below.
        (list (cons data (intern (replace-match "" nil nil name)))))))
   ((vectorp data)
    (apply #'nconc (mapcar #'reorg--dot-at-search data)))
   ((not (consp data)) nil)
   ((eq (car data) 'let-alist)
    ;; For nested ‘let-alist’ forms, ignore symbols appearing in the
    ;; inner body because they don’t refer to the alist currently
    ;; being processed.  See Bug#24641.
    (reorg--dot-at-search (cadr data)))
   (t (append (reorg--dot-at-search (car data))
              (reorg--dot-at-search (cdr data))))))

(setq
 yyy
 (let ((grouper (lambda (x) (let-alist x (when .at-name .@tag-list)))))
   (cl-loop for each in xxx
	    if (listp (funcall grouper each))
	    append (cl-loop
		    with dots = (cl-delete-duplicates
				 (let-alist--deep-dot-search grouper)
				 :test #'equal)
		    for (_ . dot) in dots
		    append (cl-loop for x in (alist-get dot each)
				    collect (let ((ppp (copy-alist each)))
					      (setf (alist-get dot ppp) x)
					      ppp)))
	    else append each)))


