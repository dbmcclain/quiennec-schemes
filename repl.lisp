
(in-package :sdlisp-repl)

;; -----------------------------------------------
;; Toplevel REPL

;; for use by Lisp to execute Scheme expressions

(defmacro with-sdlisp (expr)
  `(eval-sdlisp ',expr))

(editor:setup-indent "with-sdlisp" 1)

(defvar *debuggable* nil)

(defun dbg (t/f)
  (setf *debuggable* t/f))

(defun continue-in-sdlisp (c)
  (declare (ignore c))
  (um:when-let (restart (find-restart 'continue-in-sdlisp))
    (invoke-restart restart)))

(defun handle-repl-error (err)
  (if *debuggable*
      (error err)
    (progn
      (format t "~&ERROR: ~A"
              (um:format-error err))
      (continue-in-sdlisp err))))

(defun repl ()
  #F
  (labels ((repl1 ()
             (let ((x (reader-next)))
               (cond ((eq #'reader-next x))
                     
                     ((member x '(:bye bye))
                      (return-from repl "See ya!"))
                     
                     (t 
                      (let ((val (eval-sdlisp x)))
                        (format t "~&")
                        (print-result val t)
                        (eval-sdlisp `(begin
                                       (set! ittt itt)
                                       (set! itt  it)
                                       (set! it   ',val)))
                        ))
                     ))))
    (let ((sav-parms g.params))
      (unwind-protect
          (loop
           (handler-bind ((error #'handle-repl-error))
             (restart-case
                 (progn
                   (flush-reader-buf)
                   (loop (repl1)))
               (continue-in-sdlisp ()
                 (setf g.params sav-parms)
                 nil))))
        (setf g.params sav-parms))
      )))

(defun file-loader (fname)
  #F
  (with-open-file (f fname :direction :input)
    (tagbody
     again
     (let ((x (reader-next f)))
       (cond ((eq x f))
             
             (t
              (eval-sdlisp x)
              (go again))
             ))
     )))

