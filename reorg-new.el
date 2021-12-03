;;; reorg-new.el ---  -*- lexical-binding: t; -*-



(defvar reorg-get-candidates-func nil)
(defvar reorg-parse-candidates-func nil)
(defvar reorg-group-and-sort-func nil)
(defvar reorg-group-and-sort-macro nil)
(defvar reorg-update-hook nil)
(defvar reorg-set-property-macro nil)
(defvar reorg-create-header-string nil)
(defvar reorg-insert-headers nil)

(defvar reorg-thing
  '( :display-func
     :parse-source-func    ;; how to parse this data from the candidate
     :get-view-func
     :update-func ;; 
     :set-func
     :field-keymap ;; includes update hook
     :header-keymap ;; includes an update hook
     ))


(funcall yyy "asdf")

(setq yyy #'xxx)

(defmacro xxx (arg)
  `(insert ,arg))


