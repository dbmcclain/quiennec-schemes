;; sdlisp-dtc.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:sdlisp-dtc)
;; -------------------------------------------

(defcse SHALLOW-ARGUMENT-REF (j)
  (declare (fixnum j))
  #f
  (lambda (env)
    (declare (cons env))
    (svref (the simple-vector (car env)) j)))

(defcse DEEP-ARGUMENT-REF (i j)
  (declare (fixnum i j))
  #f
  (lambda (env)
    (declare (cons env))
    (svref (the simple-vector (nth i env)) j)))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (declare (fixnum j))
  #f
  (lambda (env)
    (declare (cons env))
    (setf (svref (the simple-vector (car env)) j) (funcall m env))) )

(defun DEEP-ARGUMENT-SET! (i j m)
  (declare (fixnum i j))  
  #f
  (lambda (env)
    (declare (cons env))
    (setf (svref (the simple-vector (nth i env)) j) (funcall m env))) )

;;; Note that we lost the name of the variable, it must be retrieved
;;; from elsewhere.   TOBEDONE

(defcse CHECKED-GLOBAL-REF (sym)
  #f
  (declare (sym sym))
  (lambda (env)
    (declare (ignore env))
    (let ((v (sym-val sym)))
      (if (eql v undefined-value)
          (error "Uninitialized variable")
        v ) ) ) )

(defcse PREDEFINED (sym)
  #f
  (declare (sym sym))
  (lambda (env)
    (declare (ignore env))
    (sym-val sym)))

(defun GLOBAL-SET! (sym m)
  #f
  (declare (sym sym))
  (lambda (env)
    (setf (sym-val sym) (funcall m env))) )

(defun CONSTANT (value)
  #f
  (if (and (integerp value)
           (<= -10 value 10))
      (cse (CONSTANT value)
           (lambda (env)
             (declare (ignore env))
             value))
    (lambda (env)
      (declare (ignore env))
      value) ))

(defun ALTERNATIVE (m1 m2 m3)
  #f
  (lambda (env)
    (if (funcall m1 env)
        (funcall m2 env)
      (funcall m3 env))) )

(defun CLAUSE-SEQUENCE (m m+)
  #f
  (lambda (env)
    (funcall m env)
    (funcall m+ env)) )

(defun TR-FIX-LET (m* m+)
  #f
  (lambda (env)
    (declare (cons env))
    (funcall m+ (cons (funcall m* env) env)) ) )

(defun FIX-LET (m* m+)
  (tr-fix-let m* m+))

;; ----------------------------------------------

(defun CALL0 (address)
  #f
  (lambda (env)
    (declare (ignore env))
    (funcall address)))

(defun CALL1 (address m1)
  #f
  (lambda (env)
    (funcall address (funcall m1 env))) )

(defun CALL2 (address m1 m2)
  #f
  (lambda (env)
    (let ((v1 (funcall m1 env))) 
      (funcall address v1 (funcall m2 env)) )) )

(defun CALL3 (address m1 m2 m3)
  #f
  (lambda (env)
    (let* ((v1 (funcall m1 env))
           (v2 (funcall m2 env)) )
      (funcall address v1 v2 (funcall m3 env)) )) )

(defun CALLN (address ms)
  #f
  (declare (cons ms))
  (lambda (env)
    (let ((vs (mapcar (lambda (m)
                        (funcall m env))
                      ms)))
      (apply address vs))))

;; -------------------------------------------------------------

(defun THUNK-CLOSURE (m+)
  #f
  (lambda (env)
    (make-closure
     :code (lambda (v*)
             (declare (simple-vector v*))
             (if (= 1 (the fixnum (length v*)))
                 (funcall m+ env)
               (error "Incorrect arity"))) )))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (lambda (env)
      (make-closure
       :code (lambda (v*)
               (declare (simple-vector v*))
               (let ((nel (length v*)))
                 (declare (fixnum nel))
                 (if (= nel arity+1)
                     (funcall m+ (cons v* env))
                   (error "Incorrect arity")) ))
       ))))

;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (lambda (env)
      (make-closure
       :code (lambda (v*)
               (declare ((array t (*)) v*))
               (let ((nel (length v*)))
                 (declare (fixnum nel))
                 (cond ((>= nel arity+1)
                        ;;; Gather into a list all arguments from arity+1
                        ;;; to the end of the activation frame and store
                        ;;; this list into the arity+1th slot.
                        (setf (svref v* arity)
                              (coerce (subseq v* arity (the fixnum (1- nel)))
                                      'list))
                        (funcall m+ (cons v* env)))
                       
                       (t (error "Incorrect arity"))
                       ))) ))))

;; -------------------------------------------------------------

(defun invoke (f v*)
  #f
  (declare (simple-vector v*))
  (cond ((or (symbolp f)
             (functionp f))
         (apply f (coerce (subseq v* 0 (1- (length v*))) 'list)))
        ((closure-p f)
         (funcall (closure-code f) v*))
        (t (error "Not a function"))
        ))

(defun TR-REGULAR-CALL (m m*) 
  #f
  (lambda (env)
    (let ((f (funcall m env)))
      (invoke f (funcall m* env)) ) ) )

(defun REGULAR-CALL (m m*)
  (tr-regular-call m m*))

(defun STORE-ARGUMENT (m m* rank)
  #f
  (declare (fixnum rank))
  (lambda (env)
    (let* ((v  (funcall m env))
           (v* (funcall m* env)) )
      (declare ((array t (*)) v*))
      (setf (svref v* rank) v)
      v* ) ) )

(defun CONS-ARGUMENT (m m* arity)
  #f
  (declare (fixnum arity))
  (lambda (env)
    (let* ((v   (funcall m env))
           (v*  (funcall m* env)))
      (declare ((array t (*)) v*))
      (push v (svref v* arity))
      v* ) ) )

(defcse ALLOCATE-FRAME (size)
  #f
  (declare (fixnum size))
  (let ((size+1 (1+ size)))
    (declare (fixnum size+1))
    (lambda (env)
      (declare (ignore env))
      (make-array size+1) ) ) )

;; ---------------------------------------------------------------------
;; APPLY

(defun sdlisp-apply (fn &rest args)
  (let ((ap-args (apply #'list* args)))
    (cond ((functionp fn)
           (apply fn ap-args))
          (t
           (invoke fn (coerce (append ap-args '(NIL)) 'vector)))
          )))

;; ---------------------------------------------------------------------
;; LISP-CALLABLE = make a closure into a Lisp function value.

(defun make-lisp-callable (clos)
  (cond ((closure-p clos)
         (lambda (&rest args)
           (let ((v* (coerce (append args '(nil)) 'vector)))
             (invoke clos v*))) )
        ((functionp clos) clos)
        (t (error "Not a function"))
        ))

;; -----------------------------------------------
;; Toplevel REPL

;; for use by Lisp to execute Scheme expressions

(defun eval-sdlisp (expr)
  (funcall (meaning expr nil t) nil))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-dtc)))
    (repl)))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp)))
    (file-loader fname)))
