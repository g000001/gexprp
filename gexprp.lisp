;;;; gexprp.lisp

(cl:in-package :gexprp.internal)

(def-suite gexprp)

(in-suite gexprp)

(defun remove-font-deco (str)
  (regex-replace-all "[0-9]" str ""))

(defmacro space-keeper (char pool)
  `(if (char= #\Newline ,char)
       (setq ,pool () )
       (when (white-char-p ,char)
	 (push ,char ,pool))))

(defun white-char-p (char)
  (case char
    ((#\Space #\Tab #\Newline #\Linefeed #\Nul) t)
    (otherwise nil)))

(defun basename (path)
  (let ((p (namestring path)))
    (subseq p (1+ (position #\/ p :from-end 'T)))))

(defun grep-expr* (expr infile out-gatherer hr &optional (type 1))
  (let* ((key (nreverse (cons #\( (coerce (string expr) 'list))))
	 (key-len (length key))
	 (acc (make-list key-len :initial-element #\Nul))
	 (g out-gatherer)
	 (pcnt type)
	 (no 1)
	 (total-lines 0)
	 (max 0)
	 inflg space acc-space in-comment)
    (iterate ((c (scan-file infile #'read-char)))
      (space-keeper c space)
      (cond ((not inflg) (setq acc (subseq (push c acc) 0 key-len)))
	    ('T (case c
		  (#\( (or in-comment (incf pcnt)))
		  (#\) (or in-comment (decf pcnt)))
                  (#\; (setq in-comment 'T))
                  (#\Newline (setq in-comment nil)))
		(push c acc)
		(when (zerop pcnt)
		  (let* ((lines (1+ (count #\Newline acc))))
		    (setq max (max lines max))
		    (next-out g (format nil "~&~A~%;;; ~A/~D (~D line~:P).~%~A"
					hr (basename infile) no lines hr))
		    (next-out g (format nil "~A~A~%"
					(coerce (nreverse acc-space) 'string)
					(remove-font-deco
                                         (coerce (reverse acc) 'string))))
		    (setq inflg nil pcnt 1)
		    (incf no)
		    (incf total-lines lines)))))
      (when (equalp key acc)
	(setq inflg t
	      acc-space (delete #\Space space :count (count #\Space key)))))
    (list (1- no)
	  total-lines
	  max)))

(defun grep-expr (expr infiles outfile)
  (when (probe-file outfile) (delete-file outfile))
  (loop :with g := (gatherer (lambda (x) (collect-file outfile x #'write-line)))
        :with ge := (loop :for x :in infiles
		          :collect (grep-expr* expr
                                        x
                                        g
                                        (format nil
                                                ";;; ~V@{~A~:*~}~*"
                                                64
                                                #\- )
                                        1 ) )
        :for (e l m) :in ge
        :sum e :into exprs
        :sum l :into lines
        :maximize m :into max
        :finally (let ((founds (if (zerop (* lines exprs))
                                     0
                                     (truncate lines exprs))))
                     (next-out g
                               (format nil
                                       "[~A]~%~
                                        Total: ~D expr~:P and ~D line~:P.~%~
                                        ~D line~:P per expr.~%~
                                        max = ~D line~:P~%"
                                       expr
                                       exprs
                                       lines
                                       founds
                                       max ))
                     (result-of g)
                     (return founds))))

(defun expr-count* (expr infile &optional (type 1))
  (let* ((key (nreverse (cons #\( (coerce (string expr) 'list))))
	 (key-len (length key))
	 (acc (make-list key-len :initial-element #\Nul))
	 (pcnt type)
	 (no 1)
	 (total-lines 0)
	 (max 0)
	 inflg)
    (iterate ((c (scan-file infile #'read-char)))
      (cond ((not inflg) (setq acc (subseq (push c acc) 0 key-len)))
	    ('T (case c
		  (#\( (incf pcnt))
		  (#\) (decf pcnt)))
		(push c acc)
		(when (zerop pcnt)
		  (let* ((lines (1+ (count #\Newline acc))))
		    (setq max (max lines max))
		    (setq inflg nil pcnt 1)
		    (incf no)
		    (incf total-lines lines)))))
      (when (equalp key acc)
	(setq inflg t)))
    (list (1- no)
	  total-lines
	  max)))

(defun expr-count (exprs infiles outfile)
  (when (probe-file outfile) (delete-file outfile))
  (let ((g (gatherer (lambda (x) (collect-file outfile x #'write-line)))))
    (let ((foo (loop for xx in exprs
		     collect (loop :for (e l m) :in
				   (mapcar (lambda (x) (expr-count* xx x 1))
			                   infiles)
				   :sum e :into exprs
				   :sum l :into lines
				   :maximize m :into max
				   :finally (return (list
                                                     xx
						    exprs
						    lines
						    (if (or (zerop exprs)
                                                            (zerop lines))
                                                        0
                                                        (truncate lines exprs))
						    max))))))
      (next-out g
                (format nil
                        "~{~{~20@A:~8D expr~:P~1,2@T~4D line~:P.~
                         ~1,2@T~4D line~:P/expr.~1,2@Tmax = ~4D line~:P~%~}~}"
			  (sort foo #'> :key #'cadr)))
      (result-of g))))

;;; eof
