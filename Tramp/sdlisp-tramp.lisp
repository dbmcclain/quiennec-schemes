;; sdlisp-tramp.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; Copyright (C) 2008,2-17 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:sdlisp-tramp)
;; -------------------------------------------

(defconstant +UIVE+  "Uninitialized variable: ~A")
(defconstant +NAFE+  "Not a function")
(defconstant +IAE+   "Incorrect arity")

;; -------------------------------------------

(defcse SHALLOW-ARGUMENT-REF (j)
  (declare (fixnum j))
  #f
  (does
    (answer (svref (car (the list %env)) j))))

(defcse DEEP-ARGUMENT-REF (i j)
  (declare (fixnum i j))
  #f
  (does
    (answer (svref (nth i (the list %env)) j))))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (declare (fixnum j))
  #f
  (does
    (inject
     m
     (does
       (setf (svref (car (the list %env)) j) %ans))) ))

(defun DEEP-ARGUMENT-SET! (i j m)
  (declare (fixnum i j))  
  #f
  (does
    (inject
     m
     (does
       (setf (svref (nth i (the list %env)) j) %ans)) )))

(defun get-effective-sym (sym)
  (declare (sym sym))
  (or (and (sym-param sym)
           (maps:find g.params (sym-name sym)))
      sym))

(defcse CHECKED-GLOBAL-REF (sym)
  #f
  (declare (sym sym))
  (does
    (let* ((v (sym-val (get-effective-sym sym))))
      (when (eql v undefined-value)
        (tramp-error +UIVE+ (sym-name sym)))
      (answer v))))

(defcse PREDEFINED (sym)
  #f
  (declare (sym sym))
  (does
    (answer (sym-val sym))))

(defun GLOBAL-SET! (sym m)
  #f
  (declare (sym sym))
  (does
    (inject
     m
     (does
       (setf (sym-val (get-effective-sym sym)) %ans))
     )))

(defun CONSTANT (value)
  #f
  (if (and (integerp value)
           (<= -10 value 10))
      (cse (CONSTANT value)
           (does
             (answer value)))
    (does
      (answer value))))

(defun ALTERNATIVE (m1 m2 m3)
  #f
  (does
    (inject
     m1
     (does
       (if %ans
           (inject m2)
          (inject m3))) )))

(defun CLAUSE-SEQUENCE (m m+)
  #f
  (does
    (inject m m+)))

(defun TR-FIX-LET (m* m+)
  #f
  (does
    (let ((sav-env %env))
      (inject
       m*
       (does
         (set-env (cons %ans sav-env))
         (inject m+)) ))))

(defun FIX-LET (m* m+)
  #f
  (let ((fwd (tr-fix-let m* m+)))
    (does
      (let ((sav-env %env))
        (inject
         fwd
         (does
           (set-env sav-env)))))))

;; ----------------------------------------------

(defun CALL0 (address)
  #f
  (does
    (answer (funcall address))))

(defun CALL1 (address m1)
  #f
  (does
    (inject
     m1
     (does
       (answer (funcall address %ans))))))

(defun CALL2 (address m1 m2)
  #f
  (does
    (inject
     m1
     (does
       (let ((v1 %ans))
         (inject
          m2
          (does
            (answer (funcall address v1 %ans)))))))))

(defun CALL3 (address m1 m2 m3)
  #f
  (does
    (inject
     m1
     (does
       (let ((v1 %ans))
         (inject
          m2
          (does
            (let ((v2 %ans))
              (inject
               m3
               (does
                 (answer (funcall address v1 v2 %ans))))))))))))

(defun CALLN (address ms)
  #f
  (if ms
      (does
        (labels ((iter (ms vs)
                   (let ((new-vs #|(cons %ans vs)|# (addq vs %ans)))
                     (if ms
                         (inject
                          (car (the cons ms))
                          (does
                            (iter (cdr (the cons ms)) new-vs)))
                       ;; else
                       (answer (apply address #|(nreverse (the list new-vs))|#
                                      (the list (car (the cons new-vs)))))
                       ))))
          (inject
           (car (the cons ms))
           (does
             (iter (cdr (the cons ms)) nil)))))
    ;; else
    (does
      (answer (funcall address)))))

;; -------------------------------------------------------------

(defun THUNK-CLOSURE (m+)
  #f
  (does
    (let ((sav-env %env))
      (answer (make-closure
                 :code (does
                         (if (= 1 (the fixnum (length %ans)))
                             (progn
                               (set-env sav-env)
                               (inject m+))
                           (tramp-error +IAE+)))
                 )))))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (does
      (let ((sav-env %env))
        (answer (make-closure
                   :code (does
                           (let ((v* %ans))
                             (let ((nel (length v*)))
                               (declare (fixnum nel))
                               (if (= nel arity+1)
                                   (progn
                                     (set-env (cons v* sav-env))
                                     (inject m+))
                                 (tramp-error +IAE+))
                               )))
                   ))))))
  
;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (does
      (let ((sav-env %env))
        (answer (make-closure
                   :code (does
                           (let* ((v*  %ans)
                                  (nel (length v*)))
                             (declare (fixnum nel))
                             (cond ((>= nel arity+1)
                                    ;;; Gather into a list all arguments from arity+1
                                    ;;; to the end of the activation frame and store
                                    ;;; this list into the arity+1th slot.
                                    (setf (svref v* arity)
                                          (coerce (subseq v* arity (the fixnum (1- nel)))
                                                  'list))
                                    (set-env (cons v* sav-env))
                                    (inject m+))
                                   
                                   (t (tramp-error +IAE+))
                                   )))
                   ))))))

;; -------------------------------------------------------------

(defun invoke (f)
  #f
  (with-lispm *lispm*
    (cond ((or (and (symbolp f)
                    (fboundp f))
               (functionp f))
           (let ((v* %ans))
             (declare (simple-vector v*))
             (answer (apply f (coerce (subseq v* 0 (1- (length v*))) 'list)))))
          ((closure-p f)
           (funcall (closure-code f)))
          (t (tramp-error +NAFE+))
          )))
  
(defun TR-REGULAR-CALL (m m*) 
  #f
  (does
    (inject
     m
      (does
        (let ((f %ans))
          (inject
           m*
           (does
             (invoke f)) )) )) ))

(defun REGULAR-CALL (m m*)
  #f
  (let ((fwd (tr-regular-call m m*)))
    (does
      (let ((sav-env %env))
        (inject
         fwd
         (does
           (set-env sav-env)))))))

(defun STORE-ARGUMENT (m m* rank)
  (declare (fixnum rank))
  #f
  (does
    (inject
     m
     (does
       (let ((v %ans))
         (inject
          m*
          (does
            (setf (svref %ans rank) v))))))))

(defun CONS-ARGUMENT (m m* arity)
  (declare (fixnum arity))
  #f
  (does
    (inject
     m
     (does
       (let ((v %ans))
         (inject
          m*
          (does
            (push v (svref %ans arity)))))))))

(defcse ALLOCATE-FRAME (size)
  (declare (fixnum size))
  (let ((size+1 (1+ size)))
    (declare (fixnum size+1))
    #f
    (does
      (answer (make-array size+1)))))

;; ---------------------------------------------------------------------
;; CALL/CC

(defun call/cc (fn)
  #f
  (with-lispm *lispm*
    (let* ((env-sav %env)       ;; restore bindings
           (parm-sav g.params)  ;; restore parameter bindings
           (ktl-sav %ktl))      ;; restore trampoline list
      (answer (vector (make-closure
                       :code (lambda ()
                               (with-lispm *lispm*
                                 (let* ((v*  %ans)
                                        (nel (length v*)))
                                   (declare (fixnum nel))
                                   (if (= nel 2)
                                       (setf %ktl ktl-sav
                                             %env env-sav
                                             g.params parm-sav
                                             %ans (svref v* 0))
                                      (tramp-error +IAE+))
                                   ))))
                      NIL))
      (invoke fn)
      )))

(defun dyn-wrap (fn)
  #F
  (with-lispm *lispm*
    (let ((parm-sav g.params))
      (answer (vector nil))
      (inject
       (does
         (setf g.params parm-sav)))
      (invoke fn))))

;; ---------------------------------------------------------------------
;; APPLY

(defun argvec (args-list)
  (declare (list args-list))
  (coerce (nconc args-list '(nil)) 'vector))

(defun sdlisp-apply (fn &rest args)
  #f
  (let ((ap-args (apply #'list* args)))
    (cond ((or (and (symbolp fn)
                    (fboundp fn))
               (functionp fn))
           (apply fn ap-args))
          ((closure-p fn)
           (with-lispm *lispm*
             (answer  (argvec ap-args))
             (funcall (closure-code fn))))
          (t (tramp-error +NAFE+))
          )))

;; ---------------------------------------------------------------------
;; LISP-CALLABLE = make a closure into a Lisp function value.

(defun make-lisp-callable (clos)
  #f
  (cond ((closure-p clos)
         (lambda (&rest args)
           (let ((v* (argvec args)))
             (trampoline nil v* (closure-code clos)))))
        ((or (and (symbolp clos)
                  (fboundp clos))
             (functionp clos))
         clos)
        (t (tramp-error +NAFE+))
        ))

;; ----------------------------------------------------------------------
;; EXTEND-PARAMS -- used by PARAMS macro to create dynamic bindings

(defun get-params ()
  g.params)

(defun set-params (val)
  (setf g.params val))

(defun extend-params (sym val)
  (let ((sym  (if (symbolp sym)
                  (g.current-extend! sym)
                sym)))
    (declare (sym sym))
    (let ((sym* (g.param-extend! (sym-name sym))))
      (setf (sym-val sym*) val
            (sym-param sym) t)
      )))

;; -----------------------------------------------
;; Toplevel REPL

(defun eval-sdlisp (expr)
  ;; for use by Lisp to execute Scheme expressions
  (trampoline nil nil (meaning expr nil t)))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-tramp)))
    (repl)))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp-tramp)))
    (file-loader fname)))



#|
 ;; test show that for lists shorter than about 200 elements
 ;; searching a list is faster than using a tree
(defun tst (&optional (ngrp 1) (nel 1000))
  (let* ((tree (sets:empty))
         (lst  (scramble (um:range 0 nel))))
    (loop for ix from 0 below nel do
          (setf tree (sets:add ix tree)))
    (print "Base timing:")
    (time (loop repeat ngrp do
                (loop repeat nel do (random nel))))
    (print "List timing:")
    (time (loop repeat ngrp do
                (loop repeat nel do
                      (find (random nel) lst))))
    (print "Tree timing:")
    (time (loop repeat ngrp do
                (loop repeat nel do
                      (sets:mem (random nel) tree))))
    ))
|#
