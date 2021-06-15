;; sdlisp-forth.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:sdlisp-forth)
;; -------------------------------------------

(defvar *ostream*)

(defun emit (fmt &rest args)
  (terpri *ostream*)
  (apply #'format *ostream* fmt args))

(defun SHALLOW-ARGUMENT-REF (j)
  (declare (fixnum j))
  (lambda ()
    (emit "~A @shallow" j)))

(defun DEEP-ARGUMENT-REF (i j)
  (declare (fixnum i j))
  (lambda ()
    (emit "~A ~A @deep" i j)))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (declare (fixnum j))
  (lambda ()
    (funcall m)
    (emit "~A !shallow" j)))

(defun DEEP-ARGUMENT-SET! (i j m)
  (declare (fixnum i j))
  (lambda ()
    (funcall m)
    (emit "~A ~A !deep" i j)))

(defun CHECKED-GLOBAL-REF (sym)
  (declare (sym sym))
  (lambda ()
    (emit "~A @chk?" (sym-name sym))))

(defun PREDEFINED (sym)
  (lambda ()
    (emit "~A @" (sym-name sym))))

(defun GLOBAL-SET! (sym m)
  (lambda ()
    (funcall m)
    (emit "~A !" (sym-name sym))))

(defun CONSTANT (value)
  (lambda ()
    (emit "~A" value)))

(defun ALTERNATIVE (m1 m2 m3)
  (lambda ()
    (funcall m1)
    (emit "if")
    (funcall m2)
    (when m3
      (emit "else")
      (funcall m3))
    (emit "then")))

(defun CLAUSE-SEQUENCE (m m+)
  (lambda ()
    (funcall m)
    (emit "drop")
    (funcall m+)))

(defun TR-FIX-LET (m* m+)
  (lambda ()
    (funcall m*)
    (funcall m+)))

(defun FIX-LET (m* m+)
  (lambda ()
    (emit "@env")
    (funcall m*)
    (funcall m+)
    (emit "restore-env")))

;; ----------------------------------------------

(defun CALL0 (address)
  (lambda ()
    (emit "~A" address)))

(defun CALL1 (address m1)
  (lambda ()
    (funcall m1)
    (emit "~A" address)))

(defun CALL2 (address m1 m2)
  (lambda ()
    (funcall m1)
    (funcall m2)
    (emit "~A" address)))

(defun binary-op? (op)
  (member op '(+ - * / mod div & \| << >>)))

(defun CALL3 (address m1 m2 m3)
  (lambda ()
    (funcall m1)
    (funcall m2)
    (if (binary-op? address)
        (emit "~A" address))
    (funcall m3)
    (emit "~A" address)))

(defun CALLN (address ms)
  (lambda ()
    (let ((binop (binary-op? address)))
      (loop for ix from 1 to 2
            for m in ms
            do
            (funcall m))
      (loop for m in (cddr ms) do
            (if binop
                (emit "~A" address))
            (funcall m))
      (if binop
          (emit "~A" address)
        ;; else
        (emit "~A ' ~A apply" (length ms) address))) ))

;; -------------------------------------------------------------

(defun THUNK-CLOSURE (m+)
  (lambda ()
    (emit "0 tmp ! close-over ]")
    (funcall m+)
    (emit "[ end-closure ]")))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  (lambda ()
    (emit "~A tmp ! close-over ]" arity)
    (funcall m+)
    (emit "[ end-closure ]")))

;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  (lambda ()
    (emit "~A tmp ! close-over ]" (- arity))
    (funcall m+)
    (emit "[ end-closure ]")))

;; -------------------------------------------------------------

(defstruct frame
  nargs)

(defun TR-REGULAR-CALL (m m*)
  (lambda ()
    (let ((ans (funcall m*)))
      (funcall m)
      (emit "~A jmp invoke" (frame-nargs ans)))))

(defun REGULAR-CALL (m m*)
  (lambda ()
    (emit "@env")
    (let ((ans (funcall m*)))
      (funcall m)
      (emit "~A invoke" (frame-nargs ans))
      (emit "restore-env"))))

(defun STORE-ARGUMENT (m m* rank)
  (lambda ()
    (let ((ans (funcall m*)))
      (funcall m)
      (if (plusp rank)
          (emit "over ~A cells+!" rank)
        (emit "over!"))
      ans)))

(defun CONS-ARGUMENT (m m* arity)
  (lambda ()
    (let ((ans (funcall m*)))
      (funcall m)
      (if (plusp arity)
          (emit "over ~A cells+@ cons over ~A cells+!" arity arity)
        (emit "over@ cons over!"))
      ans)))

(defun ALLOCATE-FRAME (size)
  (lambda ()
    (emit "~A cells-alloc" (1+ size))
    (make-frame
     :nargs size)))

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

(defun ->forth (expr)
  (with-output-to-string (s)
    (let ((*ostream* s))
      (funcall (meaning expr nil t)))))

(defun eval-sdlisp (expr)
  (funcall (meaning expr nil t) nil))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-dtc)))
    (repl)))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp)))
    (file-loader fname)))
