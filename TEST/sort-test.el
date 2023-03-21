;; -*- lexical-binding: t; -*-


(defun reorg--sort-test ()
  ""
  (interactive)
  (reorg-open-sidebar
   '( :sources ((org . "~/.emacs.d/lisp/reorg/TEST/sort-test.org"))
      :group "NAME OF GROUP"
      :sort-results ((.headline . reorg-string<)
		     (.deadline . reorg-string<))
      :format-results (.stars
		       " "
		       (s-pad-right 30 " " .headline)
		       .deadline))))




(defun reorg--sort-test ()
  ""
  (interactive)
  (reorg-open-sidebar
   '( :sources ((org . "~/.emacs.d/lisp/reorg/TEST/sort-test.org"))
      :group "group"
      :children (( :group .deadline
		   :sort-groups reorg-string<)
		 ( :group .headline
		   :sort-groups reorg-string<)
		 ( :group (when (not (zerop (string-to-number .headline)))
			    "numbers")))
      :format-results (.headline))))

