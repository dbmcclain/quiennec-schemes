;; sdlisp-2.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces" (part 2)
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package :sdlisp-bc)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.

(defun init-sdlisp ()
  ;; define immutable values
  (setf g.current  (make-hash-table)
        g.init     (make-hash-table))
        
  (dolist (var `((t    ,t)
                 (nil  ,nil)))
    (g.init-initialize! (car var) (cadr var)))

  ;; define initial mutable values
  (dolist (var '(it itt ittt))
    (g.current-extend! var))

  (with-sdlisp
      (begin
       (set! it nil)
       (set! itt nil)
       (set! ittt nil)))
  
  ;; define primitives
  (dolist (prim '((cons cons 2)
                  (car  car  1)
                  (cdr  cdr  1)
                  (list list *)
                  (list* list* *)
                  (pair? consp 1)
                  (symbol? symbolp 1)
                  (eq? eql 2)
                  (set-car! rplaca 2)
                  (set-cdr! rplacd 2)
                  (not not 1)
                  (+ + *)
                  (- - *)
                  (= = *)
                  (< < *)
                  (> > *)
                  (* * *)
                  (/ / *)
                  (<= <= *)
                  (>= >= *)
                  (remainder rem 2)
                  (truncate truncate 1)
                  (truncate-by truncate 2)
                  (round round 1)
                  (round-by round 2)
                  (ceiling ceiling 1)
                  (ceiling-by ceiling 2)
                  (floor floor 1)
                  (floor-by floor 2)
                  (display print 1)
                  (sin sin 1)
                  (cos cos 1)
                  (tan tan 1)
                  (asin asin 1)
                  (acos acos 1)
                  (atan atan 1)
                  (atan2 atan 2)
                  (log log 1)
                  (logb log 2)
                  (exp exp 1)
                  (expt expt 2)
                  (complex complex 2)
                  (phase phase 1)
                  (abs abs 1)
                  (cis cis 1)
                  (1- 1- 1)
                  (1+ 1+ 1)
                  (internal-make-delay internal-make-delay 1)
                  (force force 1)
                  (apply sdlisp-apply *)
                  (mlc   make-lisp-callable 1)
                  (load  load-file 1)))
    (destructuring-bind (name fn arity) prim
      (g.init-initialize! name (symbol-function fn)
                          (case arity
                            (0  nil)
                            (1  '(a))
                            (2  '(a b))
                            (3  '(a b c))
                            (*  '*))) ))

  ;; define addition, but more complex, primitives
  #|(dolist (prim '((apply         make-apply-primitive)
                  ;; (list          make-list-primitive)
                  (call/eb       make-call/eb-primitive)
                  ;; (lisp          make-the-lisp-primitive)
                  (lisp-callable make-lisp-callable-primitive)))
    (g.init-initialize! (car prim) (funcall (cadr prim))))|#
  )
                
;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Global environment initializers.

;;; Definitial allows to redefine immutable global variables. Useful
;;; when debugging interactively.

#|
(defmacro definitial (name value)
  `(g.init-initialize! ',name ,value))

#+:LISPWORKS
(editor:setup-indent "definitial" 2 2)

(defmacro defprimitive (name value arity)
  (case arity
    (0 `(defprimitive0 ,name ,value))
    (1 `(defprimitive1 ,name ,value))
    (2 `(defprimitive2 ,name ,value))
    (3 `(defprimitive3 ,name ,value))
    ))
|#

(defun incorrect-arity (name)
  (error "Incorrect arity: ~A" name))
#|
(defun make-primitive (sd-name lisp-name nargs)
  (funcall
   (case nargs
     (0 #'make-primitive0)
     (1 #'make-primitive1)
     (2 #'make-primitive2)
     (3 #'make-primitive3)
     (t #'make-primitive*))
   sd-name lisp-name))

(defun make-primitive0 (name value)
  (let ((arity+1 (1+ 0)))
    (make-closure
     :code (lambda (v* sr)
               (declare (ignore sr)
                        ((array t (*)) v*))
               #f
               (if (= (length v*) arity+1)
                   (funcall value)
                 (incorrect-arity name)))
     :closed-environment nil)))

(defun make-primitive1 (name value)
  (let ((arity+1 (1+ 1)))
    (declare (fixnum arity+1))
    (make-closure
     :code (lambda (v* sr)
             (declare (ignore sr)
                      ((array t (*)) v*))
             #f
             (let ((nel (length v*)))
               (declare (fixnum nel))
               (cond ((= nel arity+1 )
                      (funcall value (aref v* 0)))
                     (t (incorrect-arity name))
                     )))
     :closed-environment nil)))

(defun make-primitive2 (name value)
  (let ((arity+1 (1+ 2)))
    (declare (fixnum arity+1))
    (make-closure
     :code (lambda (v* sr)
             (declare (ignore sr)
                      ((array t (*)) v*))
             #f
             (let ((nel (length v*)))
               (declare (fixnum nel))
               (cond ((= nel arity+1)
                      (funcall value
                               (aref v* 0)
                               (aref v* 1) ))
                     (t (incorrect-arity name))
                     )))
     :closed-environment nil)))

(defun make-primitive3 (name value)
  (let ((arity+1 (1+ 3)))
    (declare (fixnum arity+1))
    (make-closure
     :code (lambda (v* sr)
             (declare (ignore sr)
                      ((array t (*)) v*))
             #f
             (let ((nel (length v*)))
               (declare (fixnum nel))
               (cond ((= nel arity+1)
                      (funcall value
                               (aref v* 0)
                               (aref v* 1)
                               (aref v* 2)))
                     (t (incorrect-arity name))
                     )))
     :closed-environment nil)))

(defun make-primitive* (name value)
  (declare (ignore name))
  (make-closure
   :code (lambda (v* sr)
             (declare (ignore sr)
                      ((array t (*)) v*))
             #f
             (apply value (coerce (subseq v* 0 (1- (length v*))) 'list)))
   :closed-environment nil))
|#
#|
(defmacro defprimitive0 (name value)
  `(definitial ,name ,(make-primitive0 name value)))

(defmacro defprimitive1 (name value)
  `(definitial ,name ,(make-primitive1 name value)))

(defmacro defprimitive2 (name value)
  `(definitial ,name ,(make-primitive2 name value)))

(defmacro defprimitive3 (name value)
  `(definitial ,name ,(make-primitive3 name value)))

;;; Define a location in the user global environment.

(defmacro defvariable (name)
  `(g.current-extend! ',name))
|#

;; ---------------------------------------------------------------------
;; CALL/CC

;;; We do not need to save the register *env* since call/cc is not a
;;; primitive (it is defined by definitial and not by defprimitive)
;;; and non-primitive invokations are regularly handled.

#|
(definitial call/cc
  (let* ((arity 1)
         (arity+1 (1+ arity)) )
    (make-closure
     :code
     (lambda (v* sr)
         (declare (ignore sr))
         (if (= arity+1 (length (activation-frame-argument v*)))
             (call/cc
              (lambda (k)
                (invoke 
                 (aref (activation-frame-argument v*) 0)
                 (let ((frame (allocate-frame (1+ 1))))
                   (setf (aref (activation-frame-argument frame) 0)
                         (make-closure
                          :code (lambda (value r)
                                    (if (= (length (activation-frame-argument value))
                                           arity+1 )
                                        (funcall k (aref (activation-frame-argument value) 0))
                                      (error "Incorrect arity" 'continuation) ) )
                          :closed-environment nil ) )
                   frame ) ) ) )
           (error "Incorrect arity" 'call/cc) ) )
     :closed-environment nil ) ) )
|#

#|
(defun make-call/eb-primitive ()
  (let* ((arity 1)
         (arity+1 (1+ arity))
         (kcl (make-closure
               :code (lambda (v* sr)
                         (declare (ignore sr)
                                  ((array t (*)) v*))
                         #f
                         (if (= arity+1 (length v*))
                             (throw :call/eb (aref v* 0))
                           (incorrect-arity 'call/eb-continuation)))
               :closed-environment nil)))
    (lambda (v* sr)
      (declare (ignore sr)
               ((array t (*)) v*))
      #f
      (if (= arity+1 (length v*))
          (catch :call/eb
            (let ((fn (aref v* 0)))
              (setf (aref v* 0) kcl)
              (funcall fn v*)))
        (incorrect-arity 'call/eb)))
    ))
|#

;; ---------------------------------------------------------------------
;; APPLY

(defun sdlisp-apply (fn &rest args)
  (let ((ap-args (apply #'list* args)))
    (cond ((functionp fn)
           (apply fn ap-args))
          (t
           (let ((v* (coerce (append ap-args '(NIL)) 'vector)))
             (with-accessors ((ans lispm-ans)
                              (ktl lispm-ktl)) *lispm*
               (setf ans v*)
               (push `(,(symbol-value 'REGULAR-CALL-B) ,fn) ktl) )))
          )))

;; ---------------------------------------------------------------------
;; LISP = make a Lisp value available to Scheme. Atoms are provided directly.
;; Function values are translated into closures.
#|
(defun make-the-lisp-primitive ()
  (let* ((arity 1)
         (arity+1 (1+ arity)))
    (make-closure
     :code (lambda (v* sr)
               (declare (ignore sr))
               (let ((afa (activation-frame-argument v*)))
                 (if (= (length afa) arity+1)
                     (let ((val (aref afa 0)))
                       (cond ((functionp val)
                              (make-closure
                               :code (lambda (v* sr)
                                         (declare (ignore sr))
                                         (let ((afa (activation-frame-argument v*)))
                                           (apply val (coerce (subseq afa 0 (1- (length afa))) 'list))
                                           ))
                               :closed-environment nil))
                             ((keywordp val) val)
                             ((symbolp val)  (symbol-value val))
                             (t              val)
                             ))
                   (incorrect-arity 'lisp))))
     :closed-environment nil)))
|#

;; ---------------------------------------------------------------------
;; LISP-CALLABLE = make a closure into a Lisp function value.

(defun make-lisp-callable (clos)
  (cond ((closure-p clos)
         (lambda (&rest args)
           (let ((v* (coerce (append args '(nil)) 'vector)))
             (operate nil v* `((REGULAR-CALL-B ,clos))) )))
        ((functionp clos) clos)
        (t (error "Not a function"))
        ))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Testing

;;; Preserve the current modifiable global environment (containing a,
;;; b, foo, fact, fib etc.) All tests will be compiled in that environment.

#|
(let ((g g.current))
(defun original.g.current ()
    g))

;;; This variant produces a table of symbols.

(defparameter sg.current.names (list 'foo))

(defun stand-alone-producer (e)
  (setf g.current (original.g.current))
  (let* ((m (meaning e nil t))
         (size (length g.current))
         (global-names (mapcar #'car (reverse g.current))) )
    (lambda ()
      (setf sg.current (make-array size :initial-element undefined-value))
      (setf sg.current.names global-names)
      (let ((*env* nil))
        (funcall m) ) ) ))

(defun CHECKED-GLOBAL-REF+ (i)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eql v undefined-value)
          (error "Uninitialized variable" 
                 (nth i sg.current.names) )
          v ) ) ) )

;;; this one requires to close the name of the variables that must be
;;; checked. To use it you must also change meaning-reference that calls it.

(defun CHECKED-GLOBAL-REF- (i n)
  (lambda () 
    (let ((v (global-fetch i)))
      (if (eql v undefined-value)
          (error "Uninitialized variable" n)
          v ) ) ) )

;;; retrofit for tests.
(setf (symbol-value CHECKED-GLOBAL-REF) CHECKED-GLOBAL-REF+)

(define (scheme6d)
  (interpreter 
   "Scheme? "  
   "Scheme= " 
   t
   (lambda (read print error)
     (setf wrong error)
     (setf static-wrong error)
     (lambda ()
       (let ((*env* nil))
         (print ((stand-alone-producer (read)))) ) ) ) ))

(define (test-scheme6d file)
  (suite-test 
   file 
   "Scheme? " 
   "Scheme= "
   #t
   (lambda (read check error)
     (set! wrong error)
     (set! static-wrong error)
     (lambda ()
       (check ((stand-alone-producer (read)))) ) )
   equal? ) )

;;; Pay attention to tail-rec in Scheme->C.

(define (bench6d factor e)
  (let ((start (get-internal-run-time))
        (m (meaning e nil #t)) )
    (let loop ((factor factor))
      (set! *env* nil)
      (let ((v (m)))
        (let ((duration (- (get-internal-run-time) start)))
          (when (<= factor 1)
            (display (list duration v))
            (newline) ) ) )
      (if (> factor 1)
          (loop (- factor 1)) ) ) ) )
|#
;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The following code use 
;;; pp to pretty-print expressions,
;;; and eval for a local hack (should be made of macros instead).

(defvar combinator-names
  '( SHALLOW-ARGUMENT-REF
     PREDEFINED
     DEEP-ARGUMENT-REF
     SHALLOW-ARGUMENT-SET!
     DEEP-ARGUMENT-SET!
     ;; GLOBAL-REF
     CHECKED-GLOBAL-REF
     GLOBAL-SET!
     CONSTANT
     ALTERNATIVE
     CLAUSE-SEQUENCE
     TR-FIX-LET
     ;; FIX-LET
     CALL0
     CALL1
     CALL2
     CALL3
     CALLN
     THUNK-CLOSURE
     FIX-CLOSURE
     NARY-CLOSURE
     TR-REGULAR-CALL
     ;; REGULAR-CALL
     STORE-ARGUMENT
     CONS-ARGUMENT
     ALLOCATE-FRAME
     ;; ALLOCATE-DOTTED-FRAME
     ;; LISP-CALLABLE-CLOSURE
     ;; LISP-SYMBOL-VALUE
     ;; LISP-FUNCTION-CLOSURE
     ))

(defparameter combinator-originals nil)

(defun install-regular-combinators ()
  (when combinator-originals
    (loop for old-value in combinator-originals
          for name in combinator-names
          do
          (setf (symbol-function name) old-value)
          )))

(defun install-disassembling-combinators ()
  (unless combinator-originals
    (setf combinator-originals (mapcar #'symbol-function combinator-names)))
  (dolist (name combinator-names)
    (setf (symbol-function name)
          (lambda (&rest args) (cons name args))
          )))

(defun qdis (e)
  (install-disassembling-combinators)
  (unwind-protect
      (pprint (meaning e nil t))
    (install-regular-combinators)
    (terpri) ))

#|
(qdis '(lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(qdis '(lambda (n) (if (= n 0) 1 (* (fact (- n 1)) n))))
|#

;;; end of chap6d.scm

;; ---------------------------------------------------------------------
;; ---------------------------------------------------------------------
;; Scheme macros
;; ---------------------------------------------------------------------
;; ---------------------------------------------------------------------

(defun scheme-macro (symbol)
  (and (symbolp symbol)
       (get symbol 'scheme-macro)))

;; --------------------------------------------

(defmacro def-scheme-macro (name parmlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme-macro)
         (lambda ,parmlist . ,body)))

;; --------------------------------------------

(defun scheme-macro-expand (x)
  "Macro-expand this Scheme expression."
  (if (and (consp x)
           (scheme-macro (first x)))
      (progn ;; scheme-macro-expand
       (displace x (apply (scheme-macro (first x)) (rest x))))
    x))

(defun displace (old new)
  "Destructively change old cons-cell to new value"
  (if (consp new)
      (progn
        (setf (car old) (car new)
              (cdr old) (cdr new))
        old)
    (displace old `(begin ,new))))

(defun starts-with (list symbol)
  (eql (first list) symbol))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;; ----------------------------------
;; LET

(def-scheme-macro let (vars &rest body)
  (if (symbolp vars)
      ;; named let
      (let ((f    vars)
            (vars (first body))
            (body (rest body)))
        `(letrec ((,f (lambda ,(mapcar #'first vars) . ,body)))
                 (,f . ,(mapcar #'second vars))))
    ;; 'regular' let
    `((lambda ,(mapcar #'first vars) . ,body)
      . ,(mapcar #'second vars))))
  
;; --------------------------------------------
;; LET*

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin . ,body)
    `(let (,(first bindings))
       (let* ,(rest bindings) . ,body))))

;; --------------------------------------------
;; DO

(def-scheme-macro do (bindings test &rest body)
  ;; translate to named let
  (let ((g (gensym))
        (vars  (mapcar #'first bindings))
        (inits (mapcar #'second bindings))
        (steps (mapcar #'third bindings)))
    `(let ,g ,(mapcar #'list vars inits)
       (if ,(car test)
           ,(if (cdr test)
                (if (length=1 (cdr test))
                    (cadr test)
                  `(begin ,@(cdr test))))
         (begin
          ,@body
          (,g ,@(mapcar (lambda (var step)
                            (or step var))
                        vars steps)))
         ))
    ))

;; --------------------------------------------
;; AND

(def-scheme-macro and (&rest args)
  (cond ((null args) 't)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

;; --------------------------------------------
;; OR

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

;; --------------------------------------------
;; COND

(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond . ,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        ((eq (second (first clauses)) '=>)
         (assert (= (length (first clauses)) 3))
         (let ((var (gensym)))
           `(let ((,var ,(first (first clauses))))
              (if ,var (,(third (first clauses)) ,var)
                (cond . ,(rest clauses))))
           ))
        (t `(if ,(first (first clauses))
                (begin . ,(rest (first clauses)))
              (cond .,(rest clauses))))))

;; --------------------------------------------
;; CASE

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                (lambda (clause)
                    (if (starts-with clause 'else)
                        clause
                      `((member ,key-val ',(um:mklist (first clause)))
                        . ,(rest clause))))
                clauses)))))

;; --------------------------------------------
;; DEFINE

(def-scheme-macro define (name &rest body)
  (cond ((symbolp name)
         (g.current-extend! name)
         `(begin (set! ,name . ,body)
                 ',name))
        
        ((consp name)
         `(define ,(first name)
              (lambda ,(rest name) . ,body)))

        (t (error "Invalid DEFINE syntax: ~S" name))
        ))

#+:LISPWORKS
(editor:setup-indent "define" 2 2)

(defun translate-inner-defines (body &optional accum)
  (cond ((endp body) (nreverse accum))
        
        ((and (consp (first body))
              (starts-with (first body) 'define))
         (push
          (if (symbolp (second (first body)))
              `(let ((,(second (first body)) .,(cddr (first body))))
                 .,(rest body))
            (destructuring-bind (name &rest args) (second (first body))
              `(letrec ((,name (lambda ,args .,(cddr (first body)))))
                       .,(rest body))))
          accum)
         (nreverse accum))

        (t (translate-inner-defines (rest body) (cons (first body) accum)))
        ))

;; --------------------------------------------
;; INCF & DECF

(def-scheme-macro incf (place &optional (delta 1))
  (let ((pls (gensym)))
    `(let ((,pls ,place))
       (set! ,place (+ ,pls ,delta)))
    ))

(def-scheme-macro decf (place &optional (delta 1))
  `(incf ,place (- 0 ,delta)))

;; --------------------------------------------
;; DELAY & FORCE

(def-scheme-macro delay (computation)
  `(internal-make-delay (lambda () ,computation)))

(defstruct delayed
  fn val)

(defun internal-make-delay (fn)
  (make-delayed
   :fn  fn
   :val undefined-value))

(defun force (x)
  (if (delayed-p x)
      (if (eq (delayed-val x) undefined-value)
          (setf (delayed-val x)
                (funcall (delayed-fn x) (allocate-frame 1)))
        (delayed-val x))
    x))

;; --------------------------------------------
;; LETREC

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar (lambda (v)
                     (list (first v) nil))
                 bindings)
     ,@(mapcar (lambda (v)
                   `(set! . ,v))
               bindings)
     . ,body))

;; --------------------------------------------
;; PUSH & POP

(def-scheme-macro push (val place)
  `(set! ,place (cons ,val ,place)))

(def-scheme-macro pop (place)
  (let ((plc (gensym))
        (top (gensym)))
    `(let* ((,plc ,place)
            (,top (car ,plc)))
       (set! ,place (cdr ,plc))
       ,top)
    ))
    
;; --------------------------------------------
;; AMB & Backtracking

(def-scheme-macro amb (x y)
  `(random-choice (lambda () ,x) (lambda () ,y)))

#|
(with-sdlisp
 (begin

  (define backtrack-points nil)

  (define (fail)
      ((pop backtrack-points)))

  (define (choose-first f g)
      (call/cc (lambda (k)
                 (push (lambda () (k (g))) backtrack-points)
                 (f))))

  (define (random-choice f g)
      (if (= 1 (random 2))
          (choose-first f g)
        (choose-first g f)))

  ))
|#
        
;; -------------------------------------------

(defun set-vector (vec pos val)
  (setf (aref vec pos) val))

(defun set-string (str pos ch)
  (setf (char str pos) ch))

(defmacro time-sdlisp (expr)
  `(time (with-sdlisp ,expr)))

#|
(with-sdlisp
 (begin

  (define (set-vector! vec pos val)
      ((lisp #'set-vector) vec pos val))

  (define (set-string! str pos ch)
      ((lisp #'set-string) str pos ch))
  
  (define (map fn lst)
      (letrec ((iter (lambda (lst ans)
                       (if ((lisp #'null) lst)
                           ((lisp #'nreverse) ans)
                         (iter (cdr lst) (cons (fn (car lst)) ans))))))
              (iter lst nil)))

  (define (some fn lst)
      (letrec ((iter (lambda (lst)
                       (if ((lisp #'null) lst)
                           nil
                         (or (fn (car lst))
                             (iter (cdr lst)))))
                     ))
              (iter lst)))

  (define (every fn lst)
      (letrec ((iter (lambda (lst)
                       (if ((lisp #'null) lst)
                           t
                         (and (fn (car lst))
                              (iter (cdr lst)))
                         ))
                     ))
              (iter lst)))

  (define (remove-if fn lst)
      (letrec ((iter (lambda (lst ans)
                       (if ((lisp #'null) lst)
                           ((lisp #'nreverse) ans)
                         (iter (cdr lst) (if (fn (car lst))
                                             ans
                                           (cons (car lst) ans)))
                         ))))
              (iter lst nil)))
  ))

(with-sdlisp
 (define (tst n) (if (> n 0) (begin (display n) (tst (- n 1))))))
(with-sdlisp
 (define (tst n) (letrec ((iter (lambda (x) (if (> x 0) (begin (display x) (iter (- x 1)))))))
                         (iter n))))

|#

;; ---------------------------------------------------

#|
(with-sdlisp
 (begin
  (define (cadr x) (car (cdr x)))
  
  (display
   (let ((k 'wait)
        (f '()))
     (set! f (let ((g ((lambda (a) (lambda () a))
                       (call/cc (lambda (nk) (set! k nk) (nk 1))) )))
               (cons g f) ))
     (if ((lisp #'null) (cdr f)) (k 2))
     (list ((car f)) ((cadr f))) ))
  ))
|#

#|
(with-sdlisp
 (begin
  
  (define (fib n)
      (if (< n 2)
          1
        (+ (fib (- n 1))
           (fib (- n 2)))))

  (define (fast-fib n)
      (if (< n 2)
          1
        (let ((iter nil))
          (set! iter (lambda (ix f1 f2)
                       ;; (display (cons n ix))
                       (if (< ix n)
                           (iter (+ ix 1) f2 (+ f1 f2))
                         f2)))
          ;; (display n)
          (iter 2 1 1)) ))
    
  (define (tak x y z)
      (if (not (< y x))
          z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
           (tak (- z 1) x y))))
  ))

(qdis
 '(define (fast-fib n)
      (if (< n 2)
          1
        (let f ((ix 2)
                (f1 1)
                (f2 1))
          (if (< ix n)
              (f (+ ix 1) f2 (+ f1 f2))
            f2))
        )))

(qdis
 '(define (fib n)
      (if (< n 2)
          1
        (+ (fib (- n 1))
           (fib (- n 2))))))

(qdis
 '(define (fast-fib n)
      (if (< n 2)
          1
        (let ((iter nil))
          (set! iter (lambda (ix f1 f2)
                       (display (cons n ix))
                       (if (< ix n)
                           (iter (+ ix 1) f2 (+ f1 f2))
                         f2)))
          (display n)
          (iter 2 1 1)) )))
|#

;; ---------------------------------------------------------------------
(init-sdlisp)
;; ---------------------------------------------------------------------

