;;;; gexprp.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :gexprp
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :cl-ppcre
               :series)
  :components ((:file "package")
               (:file "readtable")
               (:file "gexprp")))

(defmethod perform ((o test-op) (c (eql (find-system :gexprp))))
  (load-system :gexprp)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :gexprp.internal :gexprp))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

