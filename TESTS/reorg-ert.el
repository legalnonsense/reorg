# -*- lexical-binding: t; -*-
(defvar reorg-ert-buffer-name "*REORG-ERT*" "test buffer name")
(defvar reorg-ert-test-file "~/tmp/reorg-ert-test.org" "")

(ert-deftest reorg-multiple-timestamp ()
  "test multiple timestamps"
  (with-current-buffer (get-buffer-create reorg-ert-buffer-name)
    (erase-buffer)
    (insert 
     "* TASK entry with three active timestamps\n"
     "<2023-01-05 Thu>\n"
     "<2023-01-06 Fri>\n"
     "<2023-01-07 Sat>")
    (write-file reorg-ert-test-file))
  (let ((test-template `( :sources ((org . ,reorg-ert-test-file))
			  :group .@all-active-timestamps
			  :format-results (.headline))))
    (reorg-open test-template))
  (should (string=
	   (concat 
	    "* <2023-01-05 Thu>\n"
	    "entry with three active timestamps\n"
	    "* <2023-01-06 Fri>\n"
	    "entry with three active timestamps\n"
	    "* <2023-01-07 Sat>\n"
	    "entry with three active timestamps\n")
	   (with-current-buffer reorg-buffer-name
	     (org-no-properties
	      (buffer-string))))))

(ert-deftest reorg-multiple-timestamp-with-tags ()
  "test multiple timestamps"
  (progn (with-current-buffer (get-buffer-create reorg-ert-buffer-name)
	   (erase-buffer)
	   (insert 
	    "* TASK entry with three active timestamps :TAG1:\n"
	    "<2023-01-05 Thu>\n"
	    "<2023-01-06 Fri>\n"
	    "<2023-01-07 Sat>")
	   (write-file reorg-ert-test-file))
	 (let ((test-template `( :sources ((org . ,reorg-ert-test-file))
				 :group .tags
				 :children (( :group .@all-active-timestamps
					      :format-results (.headline))))))
	   (reorg-open test-template)))
  (should (string=
	   (concat 
	    "* :TAG1:\n"
	    "** <2023-01-05 Thu>\n"
	    "entry with three active timestamps\n"
	    "** <2023-01-06 Fri>\n"
	    "entry with three active timestamps\n"
	    "** <2023-01-07 Sat>\n"
	    "entry with three active timestamps\n")
	   (with-current-buffer reorg-buffer-name
	     (org-no-properties
	      (buffer-string))))))

