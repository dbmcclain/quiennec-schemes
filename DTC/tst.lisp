;; sdlisp-2.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces" (part 2)
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package :sdlisp-dtc)

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

#|
(defun incorrect-arity (name)
  (error "Incorrect arity: ~A" name))

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

;;; end of chap6d.scm

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
        (let iter ((ix 2)
                   (f1 1)
                   (f2 1))
          (if (<= ix n)
              (iter (+ ix 1) f2 (+ f1 f2))
            f2))))
  
  (define (tak x y z)
      (if (not (< y x))
          z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
           (tak (- z 1) x y))))
  ))

(with-sdlisp
    (define (fast-fib n)
        (if (< n 2)
            1
          (let iter ((ix 2)
                     (f1 1)
                     (f2 1))
            (if (<= ix n)
                (iter (+ ix 1) f2 (+ f1 f2))
              f2)))) )
  
(dis
 '(define (fast-fib n)
      (if (< n 2)
          1
        (let f ((ix 2)
                (f1 1)
                (f2 1))
          (if (<= ix n)
              (f (+ ix 1) f2 (+ f1 f2))
            f2))
        )))

(dis
 '(define (fib n)
      (if (< n 2)
          1
        (+ (fib (- n 1))
           (fib (- n 2))))))

(dis
 '(define (fast-fib n)
      (if (< n 2)
          1
        (let ((iter nil))
          (set! iter (lambda (ix f1 f2)
                       (display (cons n ix))
                       (if (<= ix n)
                           (iter (+ ix 1) f2 (+ f1 f2))
                         f2)))
          (display n)
          (iter 2 1 1)) )))
|#

