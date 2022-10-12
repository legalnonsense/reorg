;; -*- lexical-binding: t; -*-


(cl-defmacro reorg--let-plist (plist &rest body)
  (declare (indent defun))
  `(cl-labels ((plist-p
		(lst)
		(and (listp lst) (keywordp (car lst))))
	       (decolon
		(lst)
		(if (plist-p lst)
		    (cl-loop for (key value) on lst by #'cddr
			     for tail = (intern (cl-subseq (symbol-name key) 1))
			     collect (cons tail (decolon value)))
		  lst)))
     (let-alist (decolon ,plist) ,@body)))

(defun reorg--add-number-suffix (num)
  "create the suffix for a number"
  (pcase (if (numberp num) 
	     (number-to-string num)
	   num)
    ((pred (s-ends-with-p "11")) "th")
    ((pred (s-ends-with-p "12")) "th")
    ((pred (s-ends-with-p "13")) "th")
    ((pred (s-ends-with-p "1")) "st")
    ((pred (s-ends-with-p "2")) "nd")
    ((pred (s-ends-with-p "3")) "rd")
    (_ "th")))

(defun reorg--create-symbol (&rest args)
  "Create a symbol from ARGS which can be
strings or symbols.  Used in macros."
  (cl-loop for arg in args
           if (stringp arg)
           concat arg into ret
	   else if (numberp arg)
	   concat (number-to-string arg) into ret
           else if (symbolp arg)
           concat (symbol-name arg) into ret
           finally return (intern ret)))

(defun reorg--add-remove-colon (prop &optional remove)
  "PROP is a symbol with or without a colon prefix.
Returns PROP with a colon prefix. If REMOVE is t,
then return PROP with no colon prefix."
  (pcase `(,remove ,(keywordp prop))
    (`(t t) (intern (substring (symbol-name prop) 1)))
    (`(nil nil) (intern (concat ":" (symbol-name prop))))
    (_ prop)))

(defun reorg--to-string (arg)
  "Convert ARG to a string."
  (pcase arg
    ((pred null) nil)
    ((pred numberp) (number-to-string arg))
    ((pred stringp) arg)
    ((pred symbolp) (symbol-name arg))
    (_ arg)))

(defun reorg--plist-p (data)
  "Is DATA a plist based on:
1. Is it a list?
2. Is the first element a keyword?
3. Are there an even number of elements?"
  (and (listp data)
       (keywordp (car data))
       (evenp (length data))))

(defun reorg--id-p (data)
  "Is DATA an org-id?"
  (and (stringp data)
       (string-match "[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+" data)))

(provide 'reorg-utils)
