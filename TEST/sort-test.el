;; -*- lexical-binding: t; -*-


(defun reorg--sort-test ()
  ""
  (interactive)
  (reorg-open-sidebar
   '( :sources ((org . "~/.emacs.d/lisp/reorg/TEST/sort-test.org"))
      :group "NAME OF GROUP"
      :sort-results ((.headline . reorg-string<))
      :format-results (.stars " " .headline))))




