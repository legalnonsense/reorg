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
 :keymap (("SPC" . reorg-org--open-agenda-day))
 :getter (with-current-buffer (find-file-noselect SOURCE)
	   (cl-loop 


 (org-ql-select SOURCE nil :action #'PARSER))
