;; fex.lisp -- from Tim Bradshaw 06/21

;;;; This is not FEXPRs
;;;

(in-package :cl-user)

(defstruct $fexarg
  ;; A secret structure which holds the argument to a FEX
  (form nil)                            ;the form
  (thunk (constantly nil)))             ;a thunk you can call for the value

(defgeneric fexarg-form (fa)
  ;; return either the form of a fexarg and T if it is a $FEXARG, or
  ;; NIL and NIL if it is not
  (:method ((fa $fexarg))
   (values ($fexarg-form fa) t))
  (:method ((fa t))
   (values nil nil)))

(defgeneric fexarg-value (fa)
  ;; returns either the value of a fexarg (from calling its thunk) and
  ;; T if it is a $FEXARG, or the argument and NIL if it is not
  (:method ((fa $fexarg))
   (values (funcall ($fexarg-thunk fa)) t))
  (:method ((fa t))
   (values fa nil)))

(defmacro fex (s)
  ;; like FUNCTION for FEXs
  `(get ',s 'fex))

(defun body->decls/forms (body)
  ;; Utility: parse a function body into zero or more DECLARE forms
  ;; and zero or more other forms
  (loop for bt on body
        for decl-maybe = (first bt)
        while (and (consp decl-maybe)
                   (eql (first decl-maybe) 'declare))
        collect decl-maybe into decls
        finally (return (values decls bt))))


(defmacro deffex (fx args &body body)
  ;; define a FEX
  `(progn
     ,(multiple-value-bind (decls forms) (body->decls/forms body)
        `(setf (fex ,fx)  (lambda ,args
                            ,@decls
                            (block ,fx
                              ,@forms))))
     (defmacro ,fx ,args
       `(funcall (fex ,',fx)
                 ,@(mapcar (lambda (a)
                            `(make-$fexarg :form ',a
                                           :thunk (lambda ()
                                                    ,a)))
                          (list ,@args))))))
#||
(deffex f (a)
  (list (multiple-value-list (fexarg-form a))
        (multiple-value-list (fexarg-value a))))

(f (+ 1 2))
(let ((x 1))
  (f (+ x 1)))
(funcall (fex f) (+ 1 2))
(let ((x 1))
  (funcall (fex f) (+ x 1)))

;; ---------------------------------------------------
(deffex maybe (test then else)
 (if (fexarg-value test)
     (progn
       (format *debug-io* "~&Will evaluate ~A, not ~A~%"
               (fexarg-form then) (fexarg-form else))
       (fexarg-value then))
   (progn
     (format *debug-io* "~&Will evaluate ~A, not ~A~%"
               (fexarg-form else) (fexarg-form then))
     (fexarg-value else))))

And now

(maybe 1 (print 2) (print 3))
Will evaluate (print 2), not (print 3)

2 
2
t

This is all done behind the scenes by defining the fex as a macro
which wraps its arguments in suitable structures which contain the
source of the argument and an anonymous function which, when called,
will evaluate the argument:

(deffex tiny (it) (fexarg-value it))
-> (progn
     (setf (fex tiny) (lambda (it) (block tiny (fexarg-value it))))
     (defmacro tiny (it)
       `(funcall (fex tiny)
                 ,@(mapcar (lambda (a)
                             `(make-$fexarg :form
                                            ',a
                                            :thunk
                                            (lambda () ,a)))
                           (list it)))))


||#