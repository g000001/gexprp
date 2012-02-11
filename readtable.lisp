;;;; readtable.lisp

(cl:in-package :gexprp.internal)
(in-readtable :common-lisp)

#|(defreadtable :gexprp  (:merge :standard)
  (:macro-char char fctn opt...)
  (:syntax-from readtable to-char from-char)
  (:case :upcase))|#
