(cl:in-package :gexprp.internal)

(defun iter-check (dir outfile)
  (expr-count '(:loop\
		:do\
		:map\
		:mapcar
		:mapc\
		:mapl\
		:maplist
		:mapcon
		:mapcan
		:dolist
		:do-named
		:do*\
		:do*-named
		:do-forever
		:dotimes
		:prog\
		:labels
		)
	      dir
	      outfile))

(handler-bind ((sb-int:stream-decoding-error
                (lambda (condition)
                  (declare (ignore condition))
                  ;; 不正なバイト列はスキップする
                  ;;(invoke-restart (find-restart 'sb-int:attempt-resync))

                  ;; 不正なバイト列を別の文字(文字列)で置き換えたい場合
                  (invoke-restart (find-restart 'sb-impl::input-replacement) #\?)
                  )))
  (GREP-EXPR 'defmacro
           (DIRECTORY
            "/l/Franz_Lisp,_Opus_38.60/**/*.l"
            )
           "/tmp/foo.txt"))
