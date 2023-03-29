;; -*- lexical-binding: t; -*-
(require 'reorg)

(defun reorg-elisp--render-source () 
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (when-let ((marker (reorg--get-prop 'marker))
	     (buffer (reorg--get-prop 'buffer)))
    (reorg--select-main-window buffer)
    (widen)
    (goto-char marker)
    (recenter)
    (reorg--select-tree-window)))

(reorg-create-class-type
 :name elisp
 :render-func reorg-elisp--render-source
 :keymap (("w" . (lambda () (interactive)
		   (kill-new (reorg--get-prop 'form-name))
		   (message (concat "Copied " (reorg--get-prop 'form-name))))))
 :getter (progn (when (f-directory-p SOURCE)
		  (setq SOURCE
			(directory-files SOURCE t (rx
						   "reorg-"
						   (one-or-more any)
						   ".el"
						   line-end))))
		(cl-loop for SOURCE in (-list SOURCE)
			 append
			 (with-current-buffer (find-file SOURCE)
			   (save-restriction
			     (save-excursion
			       (widen)
			       (goto-char (point-min))
			       (cl-loop while
					(re-search-forward
					 (rx 
					  line-start
					  "("
					  (or "defun" "defvar" "defcustom"
					      "defconst" "defmacro" "cl-defun"
					      "cl-defmacro"))
					 nil t)
					collect
					(let (results)
					  (beginning-of-defun)
					  (when-let* ((contents (thing-at-point 'defun)))
					    (setq results (PARSER (s-trim
								   (org-no-properties
								    contents)))))
					  (end-of-defun)
					  results))))))))

(reorg-create-data-type
 :class elisp
 :name form-name 
 :parse (s-trim (nth 1 (s-split " " data))))

(reorg-create-data-type
 :class elisp
 :name buffer
 :parse (current-buffer))

(reorg-create-data-type
 :class elisp
 :disable t
 :name id
 :parse (org-id-new))

(reorg-create-data-type
 :class elisp
 :name form-type 
 :parse (substring (nth 0 (s-split " " data)) 1))

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
 :name order
 :parse (point))

(provide 'reorg-elisp)

