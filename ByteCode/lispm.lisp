
;; -------------------------------------------
(in-package #:sdlisp-bc)
;; -------------------------------------------

;; ------------------------------------------
;; Lisp Machine...

(defstruct lispm
  env ans ktl)

(defvar *lispm*)

(defmacro with-lispm (lispm &body body)
  `(with-accessors ((%env  lispm-env)
                    (%ans  lispm-ans)
                    (%ktl  lispm-ktl)) ,lispm
     (declare (ignorable %env %ans %ktl))
     ,@body))

(editor:setup-indent "with-lispm" 1)

;; intentional var capture of %ktl, %ans, and %env
(defmacro inject (&rest ops)
  `(setf %ktl (list* ,@ops (the list %ktl))))

(defmacro answer (arg)
  `(setf %ans ,arg))

(defmacro set-env (arg)
  `(setf %env ,arg))

(defmacro does ((sym &rest args))
  `(list ',sym ,@args))

#|
(let ((j 15))
  (does (lm-SHALLOW-STORE j)))

(let ((j 15))
  `(lm-SHALLOW-STORE ,j))
|#

(um:defmacro! perform (arg)
  `(let ((,g!arg ,arg))
     (declare (cons ,g!arg))
     (apply (car ,g!arg) (cdr ,g!arg))))

(defmacro next ()
  `(perform (pop (the cons %ktl))))

(defmacro xnext ()
  )

(defmacro nextls ()
  )

(defmacro deflispm ((name . args) &body body)
  `(defun ,name ,args
     #f
     (with-lispm *lispm*
       ,@body)))

(editor:setup-indent "deflispm" 1)

;; ----------------------------------------------

(defun operate (env ans k)
  #f
  (let* ((lispm (make-lispm
                 :env env
                 :ans ans
                 :ktl (list k (does (lm-EXIT)))))
         (*lispm* lispm))
    (declare (lispm lispm)
             (dynamic-extent lispm))
    (with-lispm lispm
      (do ()
          ((null (the list %ktl)) %ans)
        (next))) ))

;; ----------------------------------------------

(deflispm (lm-EXIT)
  %ans) ;; in case of stack unwind from call/cc et al

(deflispm (lm-SHALLOW-ARGUMENT-REF j)
  (answer (svref (car %env) j))
  (nextls))

(deflispm (lm-DEEP-ARGUMENT-REF i j)
  (answer (svref (nth i %env) j))
  (nextls))

(deflispm (lm-SHALLOW-STORE j)
  (setf (svref (car %env) j) %ans)
  (nextls))

(deflispm (lm-SHALLOW-ARGUMENT-SET! j m)
  (inject
    (does (lm-SHALLOW-STORE j)))
  (perform m))

(deflispm (lm-DEEP-STORE i j)
  (setf (svref (nth i %env) j) %ans)
  (nextls))
           
(deflispm (lm-DEEP-ARGUMENT-SET i j m)
  (inject
    (does (lm-DEEP-STORE i j)))
  (perform m))

(deflispm (lm-CHECKED-GLOBAL-REF sym)
  (let ((val (sym-val sym)))
    (when (eq val undefined-value)
      (error "unbound: ~A" (sym-name sym)))
    (answer val)
    (nextls)))

(deflispm (lm-PREDEFINED sym)
  (answer (sym-val sym))
  (nextls))

(deflispm (lm-GLOBAL-STORE sym)
  (setf (sym-val sym) %ans)
  (nextls))

(deflispm (lm-GLOBAL-SET! sym m)
  (inject
    (does (lm-GLOBAL-STORE sym)))
  (perform m))

(deflispm (lm-CONSTANT v)
  (answer v)
  (nextls))

(deflispm (lm-ALT-BRANCH m2 m3)
  (perform
   (if %ans
       m2
     m3)))

(deflispm (lm-ALTERNATIVE m1 m2 m3)
  (inject
    (does (lm-ALT-BRANCH m2 m3)))
  (perform m1))

(deflispm (lm-CLAUSE-SEQUENCE m m+)
  (inject m+)
  (perform m))

(deflispm (lm-RESTORE-ENV envx)
  (set-env envx)
  %ans ;; in case of stack unwind from call/cc or similar
  (nextls))

(deflispm (lm-FIX-LET-A m+ envx)
  (set-env (cons %ans envx))
  (perform m+))

(deflispm (lm-TR-FIX-LET m* m+)
  (inject
    (does (lm-FIX-LET-A m+ %env)))
  (perform m*))

;; utility routine to inject the env restore
(deflispm (lm-SAVE-ENV)
  (inject
      (does (lm-RESTORE-ENV %env))))

(deflispm (lm-FIX-LET m* m+)
  (lm-SAVE-ENV)
  (lm-TR-FIX-LET m* m+))

(deflispm (lm-CALL0 address)
  (answer (funcall address))
  (xnext))

(deflispm (lm-CALL1-A address)
  (answer (funcall address %ans))
  (xnext))

(deflispm (lm-CALL1 address m1)
  (inject
    (does (lm-CALL1-A address)))
  (perform m1))

(deflispm (lm-CALL2-B address ans1)
  (answer (funcall address ans1 %ans))
  (xnext))
        
(deflispm (lm-CALL2-A address m2)
  (inject
    (does (lm-CALL2-B address %ans)))
  (perform m2))

(deflispm (lm-CALL2 address m1 m2)
  (inject
    (does (lm-CALL2-A address m2)))
  (perform m1))

(deflispm (lm-CALL3-C address ans1 ans2)
  (answer (funcall address ans1 ans2 %ans))
  (xnext))

(deflispm (lm-CALL3-B address ans1 m3)
  (inject
    (does (lm-CALL3-C address ans1 %ans)))
  (perform m3))

(deflispm (lm-CALL3-A address m2 m3)
  (inject
    (does (lm-CALL3-B address %ans m3)))
  (perform m2))

(deflispm (lm-CALL3 address m1 m2 m3)
  (inject
    (does (lm-CALL3-A address m2 m3)))
  (perform m1))

(deflispm (lm-CALLN-A address m* lst)
  (if m*
      (progn
        (inject
          (does (lm-CALLN-A address (cdr m*) (cons %ans lst))))
        (perform (car m*)))
    (progn
      (answer (apply address (nreverse (cons %ans lst))))
      (xnext)) ))
           
(deflispm (lm-CALLN address m*)
  (inject
    (does (lm-CALLN-A address (cdr m*) nil)))
  (perform (car m*)))

(deflispm (lm-CLOSE-OVER fn)
  (answer (funcall fn %env))
  (nextls))

(deflispm (lm-REGULAR-CALL-B clos)
  (cond ((or (symbolp clos)
             (functionp clos))
         (let ((v* %ans))
           (answer (apply clos (coerce (subseq v* 0 (1- (length v*))) 'list)))
           (xnext)))
        ((closure-p clos) (funcall (closure-code clos)))
        (t (error "Not a function"))
        ))

;; utility routine for use by exended primitives
(deflispm (lm-INVOKE f args)
  (answer args)
  (lm-REGULAR-CALL-B f))
  
(deflispm (lm-REGULAR-CALL-A m*)
  (inject
    (does (lm-REGULAR-CALL-B %ans)))
  (perform m*))

(deflispm (lm-TR-REGULAR-CALL m m*)
  (inject
    (does (lm-REGULAR-CALL-A m*)))
  (perform m))

(deflispm (lm-REGULAR-CALL m m*)
  (lm-SAVE-ENV)
  (lm-TR-REGULAR-CALL m m*))

(deflispm (lm-ALLOCATE-FRAME size)
  (answer (make-array (1+ size)))
  (next))

(deflispm (lm-STORE-ARGUMENT-B v rank)
  (setf (svref %ans rank) v)
  (next))

(deflispm (lm-STORE-ARGUMENT-A m* rank)
  (inject
    (does (lm-STORE-ARGUMENT-B %ans rank)))
  (perform m*))

(deflispm (lm-STORE-ARGUMENT m m* rank)
  (inject
    (does (lm-STORE-ARGUMENT-A m* rank)))
  (perform m))

(deflispm (lm-CONS-ARGUMENT-B v arity)
  (push v (svref %ans arity))
  (next))

(deflispm (lm-CONS-ARGUMENT-A m* arity)
  (inject
    (does (lm-CONS-ARGUMENT-B %ans arity)))
  (perform m*))

(deflispm (lm-CONS-ARGUMENT m m* arity)
  (inject
    (does (lm-CONS-ARGUMENT-A m* arity)))
  (perform m))

