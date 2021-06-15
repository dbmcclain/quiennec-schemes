;; sdlisp-bc.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:sdlisp-bc)
;; -------------------------------------------

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(defcse SHALLOW-ARGUMENT-REF (j)
  (does (lm-SHALLOW-ARGUMENT-REF j)))

(defcse DEEP-ARGUMENT-REF (i j)
  (does (lm-DEEP-ARGUMENT-REF i j)))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (does (lm-SHALLOW-ARGUMENT-SET! j m)))

(defun DEEP-ARGUMENT-SET! (i j m)
  (does (lm-DEEP-ARGUMENT-SET! i j m)))

(defcse CHECKED-GLOBAL-REF (sym)
  (does (lm-CHECKED-GLOBAL-REF sym)))

(defcse PREDEFINED (sym)
  (does (lm-PREDEFINED sym)))

(defun GLOBAL-SET! (sym m)
  (does (lm-GLOBAL-SET! sym m)))

(defun CONSTANT (value)
  #f
  (if (and (integerp value)
           (<= -10 value 10))
      (cse `(CONSTANT ,value)
           (does (lm-CONSTANT value)))
    (does (lm-CONSTANT value))))

(defun ALTERNATIVE (m1 m2 m3)
  (does (lm-ALTERNATIVE m1 m2 m3)))

(defun CLAUSE-SEQUENCE (m m+)
  (does (lm-CLAUSE-SEQUENCE m m+)))

(defun TR-FIX-LET (m* m+)
  (does (lm-TR-FIX-LET m* m+)))

(defun FIX-LET (m* m+)
  (does (lm-FIX-LET m* m+)))

;; ----------------------------------------------

(defun CALL0 (address)
  (does (lm-CALL0 address)))

(defun CALL1 (address m1)
  (does (lm-CALL1 address m1)))

(defun CALL2 (address m1 m2)
  (does (lm-CALL2 address m1 m2)))

(defun CALL3 (address m1 m2 m3)
  (does (lm-CALL3 address m1 m2 m3)))

(defun CALLN (address ms)
  (does (lm-CALLN address ms)))

;; -------------------------------------------------------------

(defun THUNK-CLOSURE (m+)
  #f
  (does (lm-CLOSE-OVER
         (lambda (envc)
           (declare (ignore envc))
           (make-closure
            :code (lambda ()
                    (with-lispm *lispm*
                      (if (= 1 (the fixnum (length %ans)))
                          (perform m+)
                        (error "Incorrect arity")))) )))))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (does (lm-CLOSE-OVER
           (lambda (envc)
             (make-closure
              :code (lambda ()
                      (with-lispm *lispm*
                        (let* ((v*  %ans)
                               (nel (length v*)))
                          (declare (fixnum nel))
                          (if (= nel arity+1)
                              (progn
                                (set-env (cons v* envc))
                                (perform m+))
                            (error "Incorrect arity")) )))
              ))))))

;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    (does (lm-CLOSE-OVER
           (lambda (envc)
             (make-closure
              :code (lambda ()
                      (with-lispm *lispm*
                        (let* ((v*  %ans)
                               (nel (length v*)))
                          (declare (fixnum nel))
                          (cond ((>= nel arity+1)
                                 ;;; Gather into a list all arguments from arity+1
                                 ;;; top the end of the activation frame and store
                                 ;;; this list into the arity+1th slot.
                                 (setf (svref v* arity)
                                       (coerce (subseq v* arity (the fixnum (1- nel)))
                                               'list))
                                 (set-env (cons v* envc))
                                 (perform m+))
                                
                                (t (error "Incorrect arity"))
                                )))) ))))))

;; -------------------------------------------------------------

(defun TR-REGULAR-CALL (m m*)
  (does (lm-TR-REGULAR-CALL m m*)))

(defun REGULAR-CALL (m m*)
  (does (lm-REGULAR-CALL m m*)))

(defun STORE-ARGUMENT (m m* rank)
  (does (lm-STORE-ARGUMENT m m* rank)))

(defun CONS-ARGUMENT (m m* arity)
  (does (lm-CONS-ARGUMENT m m* arity)))

(defcse ALLOCATE-FRAME (size)
  (does (lm-ALLOCATE-FRAME size)))

;; ---------------------------------------------------------------------
;; CALL/CC

(defun call/cc (fn)
  #f
  (with-lispm *lispm*
    (let* ((env-sav %env)
           (ktl-sav %ktl)
           (v* (vector (make-closure
                        :code (lambda ()
                                (with-lispm *lispm*
                                  (let* ((v*  %ans)
                                         (nel (length v*)))
                                    (declare (fixnum nel))
                                    (if (= nel 2)
                                        (setf %ktl ktl-sav
                                              %env env-sav
                                              %ans (svref v* 0))
                                      (error "Incorrect arity")) ))))
                       NIL)))
      (lm-INVOKE fn v*)) ))

;; ---------------------------------------------------------------------
;; APPLY

(defun sdlisp-apply (fn &rest args)
  #f
  (let ((ap-args (apply #'list* args)))
    (cond ((functionp fn)
           (apply fn ap-args))
          ((closure-p fn)
           (let ((v* (coerce (append ap-args '(NIL)) 'vector)))
             (lm-INVOKE fn v*)))
          (t (error "Not a function"))
          )))

;; ---------------------------------------------------------------------
;; LISP-CALLABLE = make a closure into a Lisp function value.

(defun make-lisp-callable (clos)
  #f
  (cond ((closure-p clos)
         (lambda (&rest args)
           (let ((v* (coerce (append args '(nil)) 'vector)))
             (lm-INVOKE clos v*))))
        ((functionp clos) clos)
        (t (error "Not a function"))
        ))

;; -----------------------------------------------
;; Toplevel REPL

;; for use by Lisp to execute Scheme expressions

(defun eval-sdlisp (expr)
  (operate nil nil (meaning expr nil t)))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-bc)))
    (repl)))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp-bc)))
    (file-loader fname)))
