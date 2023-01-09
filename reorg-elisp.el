;; -*- lexical-binding: t; -*-

(defun reorg-elisp--render-source (&optional buffer id no-narrow)
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (let ((marker (or id (reorg--get-view-prop 'marker))))
    (reorg--select-main-window (or buffer (reorg--get-view-prop 'buffer)))
    (widen)
    (goto-char marker)
    (narrow-to-defun)
    (reorg--select-tree-window)))

(reorg-create-class-type
 :name elisp
 :render-func reorg-elisp--render-source
 :keymap (("w" . (lambda () (interactive)
		   (kill-new (reorg--get-view-prop 'form-name)))))
 :getter (with-current-buffer (find-file-noselect SOURCE)
	   (save-restriction
	     (save-excursion
	       (widen)
	       (goto-char (point-min))
	       (cl-loop while
			(re-search-forward
			 (rx (seq
			      line-start
			      "("
			      (group 
			       (or "defun" "defvar" "defcustom"
				   "defconst" "defmacro" "cl-defun"
				   "cl-defmacro"))))
			 nil t)
			collect (PARSER
				 (when-let 
				     ((contents (thing-at-point 'defun)))
				   (s-trim (org-no-properties contents)))))))))

(reorg-create-data-type
 :class elisp
 :name form-name 
 :parse (s-trim (org-no-properties (nth 1 (s-split " " data)))))

(reorg-create-data-type
 :class elisp
 :name buffer
 :parse (current-buffer))

(reorg-create-data-type
 :class elisp
 :name id
 :parse (org-id-new))

(reorg-create-data-type
 :class elisp
 :name form-type 
 :parse (substring (org-no-properties (nth 0 (s-split " " data))) 1))

(reorg-create-data-type
 :class elisp
 :name file
 :parse (buffer-file-name))

(reorg-create-data-type
 :class elisp
 :name marker
 :parse (point-marker))

(reorg-create-data-type
 :class elisp
 :name callees
 :parse (reorg--code-search
	 (lambda (x)
	   (and (functionp x)
		(s-starts-with-p "reorg" (symbol-name x))))
	 (read data)))

(reorg-create-data-type
 :class elisp
 :name order
 :parse (point))

(provide 'reorg-elisp)

