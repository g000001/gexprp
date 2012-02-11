;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :gexprp
  (:use)
  (:export))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun external-symbols (pkg &aux ans)
    (do-external-symbols (s pkg ans)
      (push s ans))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((defpkg (name)
                 `(progn
                    (when (find-package ,name)
                      (delete-package ,name) )
                    (make-package ,name)
                    (use-package '(:gexprp :cl :named-readtables :fiveam :series)
                                 ,name )
                    (import (set-difference (external-symbols :ppcre)
                                            (external-symbols :series)
                                            :test #'string-equal)
                            ,name ))))
    (defpkg :gexprp.internal) ))
