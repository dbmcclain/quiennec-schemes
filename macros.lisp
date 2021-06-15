
(in-package :sdlisp-macros)

;; ---------------------------------------------------------------------
;; ---------------------------------------------------------------------
;; Scheme macros
;; ---------------------------------------------------------------------
;; ---------------------------------------------------------------------

(defun scheme-macro? (symbol)
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
  (um:if-let (mfn (and (consp x)
                       (scheme-macro? (car x))))
      (displace x (apply mfn (cdr x)))
    x))

(defun displace (old new)
  "Destructively change old cons-cell to new value"
  (if (consp new)
      (progn
        (setf (car old) (car new)
              (cdr old) (cdr new))
        old)
    (displace old `(begin ,new))))

(defun starts-with (symbol list)
  (eql symbol (first list)))

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;; --------------------------------------------
;; LETREC

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar (lambda (v)
                     `(,(first v) nil))
                 bindings)
     ,@(mapcar (lambda (v)
                   `(set! . ,v))
               bindings)
     . ,body))

(editor:setup-indent "letrec" 2)

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
  (cond ((null clauses)
         nil)

        (t (let ((clause (car clauses)))
             (cond ((length=1 clause)
                    `(or ,(first clause)
                         (cond ,@(rest clauses))))

                   ((starts-with 'else clause)
                    `(begin
                      ,@(rest clause)))

                   ((eql '=> (second clause))
                    (assert (= 3 (length clause)))
                    (let ((var (gensym)))
                      `(let ((,var ,(first clause)))
                         (if ,var
                             (,(third clause) ,var)
                           (cond ,@(rest clauses))))
                      ))

                   (t
                    `(if ,(first clause)
                         (begin ,@(rest clause))
                       (cond ,@(rest clauses))))
                   )))
        ))

;; --------------------------------------------
;; CASE

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                (lambda (clause)
                    (if (starts-with 'else clause)
                        clause
                      `((member ,key-val ',(um:mklist (first clause)))
                        . ,(rest clause))))
                clauses)))))

;; --------------------------------------------
;; DEFINE

(def-scheme-macro define (name &rest body)
  (cond ((symbolp name)
         ;; (g.current-extend! name)
         (if *toplevel*
             `(begin (set! ,name . ,body)
                     ',name)
           ;; else
           (error "Don't use DEFINE inside of lambda body")))
        
        ((consp name)
         `(define ,(first name)
              (lambda ,(rest name) . ,body)))

        (t (error "Invalid DEFINE syntax: ~S" name))
        ))

#+:LISPWORKS
(editor:setup-indent "define" 1)

;; --------------------------------------------
#|
 ;; usused
(defun translate-inner-defines (body &optional accum)
  (cond ((endp body) #|(nreverse accum)|# (car accum))
        
        ((and (consp (first body))
              (starts-with 'define (first body)))
         (#|push|# addq accum
          (if (symbolp (second (first body)))
              `(let ((,(second (first body)) .,(cddr (first body))))
                 .,(rest body))
            (destructuring-bind (name &rest args) (second (first body))
              `(letrec ((,name (lambda ,args .,(cddr (first body)))))
                       .,(rest body))))
          #|accum|#)
         #|(nreverse accum)|# (car accum))

        (t (translate-inner-defines (rest body)
                                    #|(cons (first body) accum)|#
                                    (addq accum (first body)) ))
        ))
|#

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
#|
(def-scheme-macro amb (&rest alts)
  `(let ((+prev-amb-fail amb-fail))
     (call/cc
      (lambda (+sk)
        ,@(mapcar (lambda (alt)
                    `(call/cc
                      (lambda (+fk)
                        (set! amb-fail
                              (lambda ()
                                (set! amb-fail +prev-amb-fail)
                                (+fk 'fail)))
                        (+sk ,alt))))
                  alts)
        (+prev-amb-fail)))))
|#
#|
(def-scheme-macro amb (&rest args)
  `(random-choice (lambda () ,x) (lambda () ,y)))
|#

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

