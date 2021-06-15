;; sdlisp-cps.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:sdlisp-cps)
;; -------------------------------------------

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(defcse SHALLOW-ARGUMENT-REF (j)
  (declare (fixnum j))
  #f
  (lambda (env k)
    (declare (cons env))
    (funcall k
             (svref (the simple-vector (car env)) j))))

(defcse DEEP-ARGUMENT-REF (i j)
  (declare (fixnum i j))
  #f
  (lambda (env k)
    (declare (cons env))
    (funcall k
             (svref (the simple-vector (nth i env)) j))))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (declare (fixnum j))
  #f
  (lambda (env k)
    (declare (cons env))
    (funcall m env
             (lambda (ans)
               (funcall k
                        (setf (svref (the simple-vector (car env)) j) ans))) )))

(defun DEEP-ARGUMENT-SET! (i j m)
  (declare (fixnum i j))  
  #f
  (lambda (env k)
    (declare (cons env))
    (funcall m env
             (lambda (ans)
               (funcall k
                        (setf (svref (the simple-vector (nth i env)) j) ans))) )))

;;; Note that we lost the name of the variable, it must be retrieved
;;; from elsewhere.   TOBEDONE

(defcse CHECKED-GLOBAL-REF (sym)
  #f
  (declare (sym sym))
  (lambda (env k)
    (declare (ignore env))
    (let ((v (sym-val sym)))
      (if (eql v undefined-value)
          (error "Uninitialized variable")
        (funcall k v) ) ) ) )

(defcse PREDEFINED (sym)
  #f
  (declare (sym sym))
  (lambda (env k)
    (declare (ignore env))
    (funcall k (sym-val sym))))

(defun GLOBAL-SET! (sym m)
  #f
  (declare (sym sym))
  (lambda (env k)
    (funcall m env
             (lambda (ans)
               (funcall k (setf (sym-val sym) ans))) )))

(defun CONSTANT (value)
  #f
  (if (and (integerp value)
           (<= -10 value 10))
      (cse (CONSTANT value)
           (lambda (env k)
             (declare (ignore env))
             (funcall k value)))
    (lambda (env k)
      (declare (ignore env))
      (funcall k value)) ))

(defun ALTERNATIVE (m1 m2 m3)
  #f
  (lambda (env k)
    (funcall m1 env
             (lambda (ans)
               (if ans
                   (funcall m2 env k)
                 (funcall m3 env k))) )))

(defun CLAUSE-SEQUENCE (m m+)
  #f
  (lambda (env k)
    (funcall m env
             (lambda (ans)
               (declare (ignore ans))
               (funcall m+ env k)) )))

(defun TR-FIX-LET (m* m+)
  #f
  (lambda (env k)
    (declare (cons env))
    (funcall m* env
             (lambda (v*)
               (funcall m+ (cons v* env) k)) )))

(defun FIX-LET (m* m+)
  (tr-fix-let m* m+))

;; ----------------------------------------------

(defun CALL0 (address)
  #f
  (lambda (env k)
    (declare (ignore env))
    (funcall k (funcall address))))

(defvar *kcurrent*)

(defun CALL1 (address m1)
  #f
  (lambda (env k)
    (funcall m1 env
             (lambda (ans)
               (funcall k
                        (let ((*kcurrent* k))
                          (funcall address ans))) ))))

(defun CALL2 (address m1 m2)
  #f
  (lambda (env k)
    (funcall m1 env
             (lambda (v1)
               (funcall m2 env
                        (lambda (v2)
                          (funcall k (funcall address v1 v2))) ))) ))

(defun CALL3 (address m1 m2 m3)
  #f
  (lambda (env k)
    (funcall m1 env
             (lambda (v1)
               (funcall m2 env
                        (lambda (v2)
                          (funcall m3 env
                                   (lambda (v3)
                                     (funcall k (funcall address v1 v2 v3))) )) )) )))

(defun CALLN (address ms)
  #f
  (declare (cons ms))
  (lambda (env k)
    (let ((vs nil))
      (labels ((iter (ms ans)
                 (push ans vs)
                 (if ms
                     (funcall (car ms) env
                              (lambda (ans)
                                (iter (cdr ms) ans)))
                   ;; else
                   (funcall k
                            (funcall address (nreverse vs)))) ))
        (funcall (car ms) env
                 (lambda (ans)
                   (iter (cdr ms) ans)))) )))

;; -------------------------------------------------------------

(defun THUNK-CLOSURE (m+)
  #f
  (lambda (env k)
    (funcall k
             (make-closure
              :code (lambda (k v*)
                      (declare (simple-vector v*))
                      (if (= 1 (the fixnum (length v*)))
                          (funcall m+ env k)
                        (error "Incorrect arity"))) ))))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (lambda (env k)
      (funcall k
               (make-closure
                :code (lambda (k v*)
                        (declare (simple-vector v*))
                        (let ((nel (length v*)))
                          (declare (fixnum nel))
                          (if (= nel arity+1)
                              (funcall m+ (cons v* env) k)
                            (error "Incorrect arity")) ))
                )))))

;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (lambda (env k)
      (funcall k
               (make-closure
                :code (lambda (k v*)
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
                                 (funcall m+ (cons v* env) k))
                                
                                (t (error "Incorrect arity"))
                                ))) )))))

;; -------------------------------------------------------------

(defun invoke (f k v*)
  #f
  (declare (simple-vector v*))
  (cond ((or (symbolp f)
             (functionp f))
         (funcall k (apply f (coerce (subseq v* 0 (1- (length v*))) 'list))))
        ((closure-p f)
         (funcall (closure-code f) k v*))
        (t (error "Not a function"))
        ))

(defun TR-REGULAR-CALL (m m*) 
  #f
  (lambda (env k)
    (funcall m env
             (lambda (f)
               (funcall m* env
                        (lambda (v*)
                          (invoke f k v*)) ))) ))

(defun REGULAR-CALL (m m*)
  (tr-regular-call m m*))

(defun STORE-ARGUMENT (m m* rank)
  #f
  (declare (fixnum rank))
  (lambda (env k)
    (funcall m env
             (lambda (v)
               (funcall m* env
                        (lambda (v*)
                          (declare ((array t (*)) v*))
                          (setf (svref v* rank) v)
                          (funcall k v*)) ))) ))

(defun CONS-ARGUMENT (m m* arity)
  #f
  (declare (fixnum arity))
  (lambda (env k)
    (funcall m env
             (lambda (v)
               (funcall m* env
                        (lambda (v*)
                          (declare ((array t (*)) v*))
                          (push v (svref v* arity))
                          (funcall k v*)) ))) ))

(defcse ALLOCATE-FRAME (size)
  #f
  (declare (fixnum size))
  (let ((size+1 (1+ size)))
    (declare (fixnum size+1))
    (lambda (env k)
      (declare (ignore env))
      (funcall k (make-array size+1) ) ) ))

;; ---------------------------------------------------------------------
;; CALL/CC

(defun call/cc (fn)
  #f
  (let* ((k  *kcurrent*)
         (v* (vector (make-closure
                      :code (lambda (kignore v*)
                              (declare (ignore kignore))
                              (declare (simple-vector v*))
                              (let ((nel (length v*)))
                                (declare (fixnum nel))
                                (if (= nel 2)
                                    (funcall k (svref v* 0))
                                  (error "Incorrect arity")) )))
                     NIL)))
    (invoke fn k v*)))

;; ---------------------------------------------------------------------
;; APPLY

(defun sdlisp-apply (fn &rest args)
  #f
  (let ((ap-args (apply #'list* args)))
    (cond ((or (and (symbolp fn)
                    (fboundp fn))
               (functionp fn))
           (apply fn ap-args))
          ((closure-p fn)
           (let ((v* (coerce (append ap-args '(NIL)) 'vector)))
             (invoke fn #'identity v*)))
          (t (error "Not a function"))
          )))

;; ---------------------------------------------------------------------
;; LISP-CALLABLE = make a closure into a Lisp function value.

(defun make-lisp-callable (clos)
  #f
  (cond ((closure-p clos)
         (lambda (&rest args)
           (let ((v* (coerce (append args '(nil)) 'vector)))
             (invoke clos #'identity v*))))
        ((functionp clos) clos)
        (t (error "Not a function"))
        ))

;; -----------------------------------------------
;; Toplevel REPL

;; for use by Lisp to execute Scheme expressions

(defun eval-sdlisp (expr)
  (funcall (meaning expr nil t) nil #'identity))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-cps)))
    (repl)))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp-cps)))
    (file-loader fname)))
