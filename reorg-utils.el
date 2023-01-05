;; -*- lexical-binding: t; -*-

;; (defun reorg--get-string-match (regexp string &optional group start)
;;   "Get match from string."
;;   (save-match-data
;;     (when (string-match regexp string (or start 0))
;;       (match-string (or group 0) string))))

;; (cl-defmacro reorg--let-plist (plist &rest body)
;;   (declare (indent defun))
;;   `(cl-labels ((plist-p
;; 		(lst)
;; 		(and (listp lst) (keywordp (car lst))))
;; 	       (decolon
;; 		(lst)
;; 		(if (plist-p lst)
;; 		    (cl-loop for (key value) on lst by #'cddr
;; 			     for tail = (intern (cl-subseq (symbol-name key) 1))
;; 			     collect (cons tail (decolon value)))
;; 		  lst)))
;;      (let-alist (decolon ,plist) ,@body)))



;; (defun reorg--to-string (arg)
;;   "Convert ARG to a string."
;;   (pcase arg
;;     ((pred null) nil)
;;     ((pred numberp) (number-to-string arg))
;;     ((pred stringp) arg)
;;     ((pred symbolp) (symbol-name arg))
;;     (_ arg)))

;; (defun reorg--plist-p (data)
;;   "Is DATA a plist based on:
;; 1. Is it a list?
;; 2. Is the first element a keyword?
;; 3. Are there an even number of elements?"
;;   (and (listp data)
;;        (keywordp (car data))
;;        (evenp (length data))))

;; (defun reorg--id-p (data)
;;   "Is DATA an org-id?"
;;   (and (stringp data)
;;        (string-match "[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+-[[:alnum:]]+" data)))

;; (provide 'reorg-utils)
