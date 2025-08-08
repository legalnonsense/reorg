;; -*- lexical-binding: t; -*-
(require 'reorg)

(reorg-create-class-type
 :name email
 :render-func reorg-email--render-source 
 :getter (cl-loop for s in (ensure-list SOURCE)
		  append (cl-loop for each in (s-split "\n" (shell-command-to-string
							     (concat "mu find "
								     s
								     " --format=sexp"))
						       'omit-nil)
				  collect (PARSER (read each)))))

(defun reorg-email--render-source ()
  "Move to buffer and find heading with ID.  If NARROW is non-nil,
then narrow to that heading and return t.  If no heading is found, don't move
the point and return nil."
  (let ((msg (reorg--get-prop 'mu4e-data)))   
    (reorg--select-main-window (get-buffer-create "*REORG mu4e*"))
    (let ((inhibit-read-only t))
      (remove-overlays (point-min)(point-max) 'mu4e-overlay t)
      (erase-buffer)
      (insert-file-contents-literally
       (mu4e-message-readable-path msg) nil nil nil t)
      (setq-local mu4e--view-message msg)
      (mu4e--view-render-buffer msg))    
    (reorg--select-tree-window)))


(reorg-create-data-type
 :class email
 :name class-name
 :parse "email")

(reorg-create-data-type
 :class email 
 :name from
 :parse (when-let ((from (car-safe (plist-get data :from))))
	  (plist-get from :name)))

(reorg-create-data-type
 :class email 
 :name to 
 :parse (cl-loop for each in (plist-get data :to)
		 concat (plist-get each :email)))

(reorg-create-data-type
 :class email
 :name path
 :parse (plist-get data :path))

(reorg-create-data-type
 :class email
 :name date
 :parse (format-time-string
	 "%Y-%m-%d %H:%M"
	 (plist-get data :date)))

(reorg-create-data-type
 :class email
 :name subject
 :parse (plist-get data :subject))

(reorg-create-data-type
 :class email
 :name mu4e-data
 :parse data)

(provide 'reorg-email)
