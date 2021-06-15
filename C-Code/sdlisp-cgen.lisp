;; sdlisp-dtc.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

;; -------------------------------------------
(in-package #:sdlisp-cgen)
;; -------------------------------------------

(defvar *hdr-stream*)
(defvar *body-stream*)
(defvar *indent* 0)

(defun open-stream-pair ()
  (let ((hdr-stream  (make-string-output-stream))
        (body-stream (make-string-output-stream)))
    (values hdr-stream body-stream)))

(defun close-stream-pair (h b dst)
  (let ((hstr (get-output-stream-string h)))
    (when (plusp (length hstr))
      (princ hstr dst)
      (terpri dst)))
  (princ (get-output-stream-string b) dst)
  (close h)
  (close b))

(defun emit (stream str &rest args)
  (format stream "~%")
  (dotimes (ix *indent*)
    (princ #\space stream))
  (apply #'format stream str args))

(defun emit-hdr (str &rest args)
  (apply #'emit *hdr-stream* str args))

(defun emit-body (str &rest args)
  (apply #'emit *body-stream* str args))

;; ------------------------------------------------

(defvar *cse-env* nil)

(defun lcl-cse? (key)
  (cdr (assoc key *cse-env* :test #'equal)))

(defun memo-cse! (key var)
  (push (cons key var) *cse-env*)
  var)

(defun do-with-cse (key fn)
  (let ((var (lcl-cse? key)))
    (unless var
      (setf var (gensym))
      (funcall fn var)
      (memo-cse! key var))
    var))

(defmacro with-cse ((var key) &body body)
  `(do-with-cse ,key (lambda (,var) ,@body)))

(editor:setup-indent "with-cse" 1)

;; ----------------------------------------------

(defun current-env ()
  (or (lcl-cse? 'env)
      "__env"))

(defun SHALLOW-ARGUMENT-REF (j)
  (declare (fixnum j))
  (lambda ()
    (with-cse (dst `(shallow-arg ,j))
      (emit-hdr "obj ~A;" dst);
      (emit-body "~A = ((obj*)Car(~A))[~A];"
                 dst (current-env) j)) ))

(defun DEEP-ARGUMENT-REF (i j)
  (declare (fixnum i j))
  (lambda ()
    (with-cse (dst `(deep-arg ,i ,j))
      (emit-hdr "obj ~A;" dst)
      (emit-body "~A = ((obj*)Car(NthCdr(~A,~A)))[~A];"
                 dst (current-env) i j)) ))

(defun SHALLOW-ARGUMENT-SET! (j m)
  (declare (fixnum j))
  (lambda ()
    (let ((ans (funcall m)))
      (emit-body "((obj*)Car(~A))[~A] = ~A;"
                 (current-env) j ans)
      (memo-cse! `(shallow-arg ,j) ans)
      ans)))

(defun DEEP-ARGUMENT-SET! (i j m)
  (declare (fixnum i j))
  (lambda ()
    (let ((ans (funcall m)))
      (emit-body "((obj*)Car(nthCdr(~A,~A)))[~A] = ~A;"
                 (current-env) i j ans)
      (memo-cse! `(deep-arg ,i ,j) ans)
      ans)))

(defun CHECKED-GLOBAL-REF (sym)
  (declare (sym sym))
  (lambda ()
    (with-cse (dst `(global ,sym))
      (emit-hdr "obj ~A;" dst)
      (emit-body "~A = SymVal(Symbol(~A));" dst (sym-name sym))
      (emit-body "assert(~A != Undefined);" dst))))

(defun PREDEFINED (sym)
  (declare (sym sym))
  (lambda ()
    (with-cse (dst `(global ,sym))
      (emit-hdr "obj ~A;" dst)
      (emit-body "~A = SymVal(Symbol(~A));" dst (sym-name sym)) )))

(defun GLOBAL-SET! (sym m)
  (declare (sym sym))
  (lambda ()
    (let ((ans (funcall m)))
      (emit-body "SymVal(Symbol(~A)) = ~A;" (sym-name sym) ans)
      (memo-cse! `(global ,sym) ans)
      ans)))

(defun CONSTANT (value)
  (lambda ()
    value))

;; ------------------------------------------------------------

(defun do-with-new-block (fn)
  (emit-body "{")
  (multiple-value-bind (hdr-stream body-stream)
      (open-stream-pair)
    (let ((*hdr-stream*  hdr-stream)
          (*body-stream* body-stream)
          (*indent*      (+ *indent* 2))
          (*cse-env*     *cse-env*))
      (funcall fn))
    (close-stream-pair hdr-stream body-stream *body-stream*))
  (emit-body "}"))

(defmacro with-new-block (&body body)
  `(do-with-new-block (lambda () ,@body)))

;; ------------------------------------------------------------

(defun ALTERNATIVE (m1 m2 m3)
  (lambda ()
    (let ((dst nil)
          (tst (funcall m1)))
      (emit-body "if(~A)" tst)
      (with-new-block
        (let ((ans (funcall m2)))
          (when ans
            (setf dst (gensym (symbol-name :ans)))
            (emit-body "~A = ~A;" dst ans))))
      (emit-body "else")
      (with-new-block
        (let ((ans (funcall m3)))
          (when ans
            (unless dst
              (setf dst (gensym (symbol-name :ans))))
            (emit-body "~A = ~A;" dst ans))))
      (when dst
        (emit-hdr "obj ~A;" dst))
      dst) ))

(defun CLAUSE-SEQUENCE (m m+)
  (lambda ()
    (funcall m)
    (funcall m+)) )

(defun TR-FIX-LET (m* m+)
  (lambda ()
    (with-new-block
      (let ((ans (funcall m*))
            (new-env (gensym "ENV")))
        (emit-hdr "cons *~A;" new-env)
        (emit-body "~A = Cons((obj)~A,~A);"
                   new-env (frame-name ans) (current-env))
        (memo-cse! 'env new-env)
        (let ((ans (funcall m+)))
          (when ans
            (emit-body "return ~A;" ans)))))))

(defun FIX-LET (m* m+)
  (lambda ()
    (let ((dst nil))
      (with-new-block
        (let ((frame (funcall m*))
              (new-env (gensym "ENV")))
          (emit-hdr "cons *~A;" new-env)
          (emit-body "~A = Cons(~A,~A);"
                     new-env (frame-name frame) (current-env))
          (memo-cse! 'env new-env)
          (let ((ans (funcall m+)))
            (when ans
              (setf dst (gensym (symbol-name :ans)))
              (emit-body "~A = ~A;" dst ans)))))
      (when dst
        (emit-hdr "obj ~A;" dst))
      dst)))

;; ----------------------------------------------

(defun CALL0 (address)
  (lambda ()
    (let ((dst (gensym (symbol-name :ans))))
      (emit-hdr "obj ~A;" dst)
      (emit-body "~A = ~A();" dst address)
      dst)))

(defun CALL1 (address m1)
  (lambda ()
    (let ((dst (gensym (symbol-name :ans)))
          (ans1 (funcall m1)))
      (emit-hdr "obj ~A;" dst);
      (emit-body "~A = ~A(~A);" dst address ans1)
      dst)))

(defun binop? (oper)
  (member oper '(+ - / * % ^ < <= == >= > << >>)))

(defun CALL2 (address m1 m2)
  (lambda ()
    (let ((dst (gensym (symbol-name :ans)))
          (ans1 (funcall m1))
          (ans2 (funcall m2)))
      (emit-hdr "obj ~A;" dst);
      (if (binop? address)
          (emit-body "~A = (~A ~A ~A);" dst ans1 address ans2)
        (emit-body "~A = ~A(~A, ~A);" dst address ans1 ans2))
      dst)))

(defun CALL3 (address m1 m2 m3)
  (lambda ()
    (let ((dst (gensym (symbol-name :ans)))
          (ans1 (funcall m1))
          (ans2 (funcall m2))
          (ans3 (funcall m3)))
      (emit-hdr "obj ~A;" dst)
      (if (binop? address)
          (emit-body "~A = (~A ~A ~A) ~A ~A;" dst ans1 address ans2 address ans3)
        (emit-body "~A = ~A(~A, ~A, ~A);" dst address ans1 ans2 ans3))
      dst)))

(defun CALLN (address ms)
  (lambda ()
    (let ((dst (gensym (symbol-name :ans)))
          (anss (mapcar (lambda (m)
                          (funcall m))
                        ms)))
      (emit-hdr "obj ~A;" dst)
      (if (binop? address)
          (emit-body "~A = ~A;" dst
                (reduce (lambda (acc opnd)
                          (format nil "(~A ~A ~A)" acc address opnd))
                        anss))
        (emit-body "~A = ~A(~{~A~^, ~});" dst address anss))
      dst)))

;; -------------------------------------------------------------

(defvar *pending-gen* nil)

(defun THUNK-CLOSURE (m+)
  (lambda ()
    (let ((dst (gensym (symbol-name :clos)))
          (fn  (gensym (symbol-name :fun))))
      (emit-hdr "closure *~A;" dst)
      (emit-body "~A = Closure(~A, ~A);" dst fn (current-env))
      (memo-cse! `(closure-env ,dst) (current-env))
      (memo-cse! `(closure-fn ,dst) fn)
      (push (list 'thunk-closure fn m+) *pending-gen*)
      dst)))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  (lambda ()
    (let ((dst (gensym (symbol-name :clos)))
          (fn  (gensym (symbol-name :fun))))
      (emit-hdr "closure *~A;" dst)
      (emit-body "~A = Closure(~A, ~A);" dst fn (current-env))
      (memo-cse! `(closure-env ,dst) (current-env))
      (memo-cse! `(closure-fn ,dst) fn)
      (push (list 'fix-closure fn m+ arity) *pending-gen*)
      dst)))

;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  (lambda ()
    (let ((dst (gensym (symbol-name :clos)))
          (fn  (gensym (symbol-name :fun))))
      (emit-hdr "closure *~A;" dst)
      (emit-body "~A = Closure(~A, ~A);" dst fn (current-env))
      (memo-cse! `(closure-env ,dst) (current-env))
      (memo-cse! `(closure-fn ,dst) fn)
      (push (list 'nary-closure fn m+ arity) *pending-gen*)
      dst)))

;; -------------------------------------------------------------

(defstruct frame
  name nargs)

(defun closure-env (arg)
  (or (lcl-cse? `(closure-env ,arg))
      (format nil "ClosureEnv(~A)" arg)))

(defun closure-fn (arg)
  (or (lcl-cse? `(closure-fn ,arg))
      (format nil "(ClosureFn(~A))" arg)))

(defun TR-REGULAR-CALL (m m*)
  (lambda ()
    (with-new-block
      (let ((fn      (funcall m))
            (frame   (funcall m*))
            (new-env (gensym (symbol-name :env))))
        (if (plusp (frame-nargs frame))
            (progn
              (emit-hdr "cons *~A;" new-env)
              (emit-body "~A = Cons((obj)~A, ~A);" new-env (frame-name frame) (closure-env fn))
              (emit-body "return (*~A)(~A, ~A);" (closure-fn fn) (frame-nargs frame) new-env))
          ;; else
          (emit-body "return (*~A)(0, ~A);" (closure-fn fn) (closure-env fn)))))
    nil))

#|
(defun TR-REGULAR-CALL (m m*)
  (REGULAR-CALL m m*))
|#

(defun REGULAR-CALL (m m*)
  (lambda ()
    (let ((dst     (gensym (symbol-name :ans))))
      (emit-hdr "obj ~A;" dst)
      (with-new-block
        (let ((fn      (funcall m))
              (frame   (funcall m*))
              (new-env (gensym (symbol-name :env))))
          (if (plusp (frame-nargs frame))
              (progn
                (emit-hdr "cons *~A;" new-env)
                (emit-body "~A = Cons((obj)~A, ~A);" new-env (frame-name frame) (closure-env fn))
                (emit-body "~A = (*~A)(~A, ~A);" dst (closure-fn fn) (frame-nargs frame) new-env))
            (emit-body "~A = (*~A)(0, ~A);" dst (closure-fn fn) (closure-env fn)))))
      dst)))

(defun STORE-ARGUMENT (m m* rank)
  (lambda ()
    (let ((v (funcall m))
          (frame (funcall m*)))
      (emit-body "~A[~A] = ~A;" (frame-name frame) rank v)
      frame)))

(defun CONS-ARGUMENT (m m* arity)
  (lambda ()
    (let ((v (funcall m))
          (frame (funcall m*)))
      (emit-body "(cons*)~A[~A] = Cons(~A, (cons*)~A[~A]);"
            (frame-name frame) arity
            v
            (frame-name frame) arity)
      frame)))

(defun ALLOCATE-FRAME (size)
  (declare (fixnum size))
  (lambda ()
    (let ((frame (gensym (symbol-name :frame))))
      (when (plusp size)
        (emit-hdr "obj *~A;" frame)
        (emit-body "~A = NewFrame(~A);" frame size))
      (make-frame
       :name frame
       :nargs size))))

;; ---------------------------------------------------------------------
;; APPLY

(defun sdlisp-apply (fn &rest args)
  (declare (ignore fn args))
  (NYI))

(defun NYI ()
  (error "Not yet implemented!"))

;; ---------------------------------------------------------------------
;; LISP-CALLABLE = make a closure into a Lisp function value.

(defun make-lisp-callable (clos)
  (declare (ignore clos))
  (NYI))

;; -----------------------------------------------
;; Toplevel REPL

;; for use by Lisp to execute Scheme expressions

(defun gen-thunk (pend)
  (declare (ignore dot-args))
  (emit-body "obj ~A(int __nargs, cons* __env)" (second pend))
  (with-new-block
    (emit-body "assert(__nargs == 0);")
    (let ((ans (funcall (third pend))))
      (when ans
        (emit-body "return ~A;" ans)))))
    

(defun gen-fix-closure (pend nargs)
  (declare (ignore dot-args))
  (emit-body "obj ~A(int __nargs, cons* __env)" (second pend))
  (with-new-block
    (emit-body "assert(__nargs == ~A);" nargs)
    (let ((ans (funcall (third pend))))
      (when ans
        (emit-body "return ~A;" ans)))))
    

(defun gen-nary-closure (pend nargs)
  (declare (ignore dot-args))
  (emit-body "obj ~A(int __nargs, cons* __env)" (second pend))
  (with-new-block
    (emit-body "assert(__nargs >= ~A);" nargs)
    (emit-body "if(__nargs > ~A)" nargs)
    (emit-body "  ((obj*)Car(__env))[~A] = vecToList((obj*)Car(__env), ~A, __nargs);" nargs nargs)
    (let ((ans (funcall (third pend))))
      (when ans
        (emit-body "return ~A;" ans)))))
    

(defun generate (pend)
  (case (car pend)
    (thunk-closure (gen-thunk pend))
    (fix-closure   (gen-fix-closure pend (fourth pend)))
    (nary-closure  (gen-function pend (fourth pend) t))
    ))

(defun do-with-new-body (fn)
  (with-output-to-string (s)
    (multiple-value-bind (hdr-stream body-stream)
        (open-stream-pair)
      (let ((*hdr-stream*  hdr-stream)
            (*body-stream* body-stream))
        (funcall fn))
      (close-stream-pair hdr-stream body-stream s))))

(defmacro with-new-body (&body body)
  `(do-with-new-body (lambda () ,@body)))

(defun genc (expr)
  (let ((m (meaning expr nil t)))
  (setf *pending-gen* nil)
  (let ((outstrs (list
                  (with-new-body
                   (emit-body "obj init()")
                   (with-new-block
                     (emit-hdr "cons *__env = NIL;")
                     (let ((ans (funcall m)))
                       (when ans
                         (emit-body "return ~A;" ans))))))))
    (loop for pend = (pop *pending-gen*)
          while pend
          do
          (push
           (with-new-body
            (generate pend))
           outstrs))
    (loop for str in outstrs do
          (princ str)
          (terpri))
    )))
  
(defun eval-sdlisp (expr)
  (genc expr))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-dtc)))
    (repl)))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp)))
    (file-loader fname)))
