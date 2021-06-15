;; sdlisp-1.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces"
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(defpackage :sdlisp-bc
  (:use #:common-lisp)
  (:nicknames #:sdlbc)
  (:export
   #:sdlisp
   #:init-sdlisp
   #:with-sdlisp
   ))

;; -------------------------------------------
(in-package #:sdlisp-bc)
;; -------------------------------------------

;; -------------------------------------------

;;;                      Threaded interpreter.
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in R)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . index)
;;; or predefined (and immutable) then return
;;;                 (PREDEFINED . index)

(defvar g.current (make-hash-table))
(defvar g.init    (make-hash-table))

;;; This tag is used in the value cell of uninitialized variables.
(defconstant undefined-value (cons 'undefined 'value))

(defstruct sym
  (val undefined-value)
  name
  plist)

(defstruct (prim-sym
            (:include sym)) )

;; -------------------------------------------------------------------------

(defun inherit-lisp-value (n is-fn)
  (if is-fn
      (unless (macro-function n)
        (when (symbol-function n)
          (let ((args (lw:function-lambda-list n nil)))
            (g.init-initialize! n (symbol-function n)
                                (if (or (not (listp args))
                                        (find-if (um:rcurry #'member lambda-list-keywords) args))
                                    '*
                                  args)) )))
    ;; else
    (when (boundp n)
      (g.init-initialize! n (symbol-value n))
      (primitive? n))
    ))
              
;; -------------------------------------------------------------------------

(defstruct local-sym
  name
  level
  offs)

(defun compute-kind (r n)
  (or (local-variable? r 0 n)
      (primitive? n)
      (global-variable? n)))

(defun local-variable? (r i n)
  (declare (fixnum i n))
  (when (consp r)
    (let ((pos (position n (car r))))
      (if pos
          (make-local-sym
           :name  n
           :level i
           :offs  pos)
        (local-variable? (cdr r) (1+ i) n)))
    ))

(defun global-variable? (n)
  (gethash n g.current))

(defun primitive? (n)
  (gethash n g.init))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Representation of local environments, they contain the values of
;;; the local variables (but global and predefined variables).
;;; Runtime environment or, activation frames, are represented by 
;;; vectors (named v*). They have the following structure:
;;;           +------------+
;;;           | next       |  ---> next V*
;;;           | argument 0 |  value of the first argument
;;;           | argument 1 |  value of the second argument
;;;           .            .
;;;           | free slot  |  Free slot for nary variable
;;;           +------------+
;;; The number of arguments can be extracted from the size of the
;;; activation frame.

;;; A direct implementation with inlined vectors is approximatively 
;;; 7 times faster under sci.

;;; R is the static representation of the runtime local environment.
;;; It is represented by a list of list of variables (the classical
;;; rib cage). 

(defun r-extend* (r n*)
  (cons n* r) )

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.
;;; It actually tolerates only 100 new global variables.

;;; G.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the SG.CURRENT vector.

(defun g.current-extend! (n)
  (unless (symbolp n)
    (error "Not a symbol: ~S" n))
  (cond ((primitive? n) (error "Can't redefine: ~S" n))
        ((global-variable? n))
        (t (setf (gethash n g.current) (make-sym
                                        :name n)))
        ))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. G.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the SG.INIT vector.

;;; Add that value is associated to name in the predefined global environment.

(defun g.init-initialize! (name value &optional (lambda-list nil lambda-list-p))
  (setf (gethash name g.init) (make-prim-sym
                               :name  name
                               :val   value
                               :plist (when lambda-list-p
                                        `(lambda-list ,lambda-list)))))

(defun get-description (prim-sym)
  (getf (sym-plist prim-sym) 'lambda-list :none))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(defparameter *common-subexpressions* (make-hash-table :test 'equal))

(defun get-cse (key)
  (gethash key *common-subexpressions*))

(defun memo-cse (key fn)
  (setf (gethash key *common-subexpressions*) fn))

;; ------------------------------------------
;; Common subexpression elimination

(defmacro cse ((name &rest args) fn)
  (let ((gkey (gensym)))
    `(let ((,gkey (list ',name ,@args)))
       (or (get-cse ,gkey)
           (memo-cse ,gkey ,fn)))
    ))

(defmacro defcse (name args &body body)
  `(defun ,name ,args
     ,@(butlast body)
     (cse (,name ,@args) ,@(last body))))

#+:LISPWORKS
(editor:setup-indent "defcse" 2 2)

;; ------------------------------------------

(defstruct lispm
  env ans ktl)

(defvar *lispm*)

(defun operate (env ans ktl)
  #f
  (let* ((lispm (make-lispm
                 :env env
                 :ans ans
                 :ktl ktl))
         (*lispm* lispm))
    (declare (dynamic-extent lispm))
    (with-accessors ((ans lispm-ans)
                     (ktl lispm-ktl)) lispm
      (do ()
          ((null ktl) ans)
        (let ((oper (pop ktl)))
          (apply (car oper) (cdr oper)))) )))
  
;; ----------------------------------------------

(defun compiled-clause (clause)
  (destructuring-bind ((op . args) . body) clause
    (declare (ignore op))
    (compile nil
             `(lambda ,args
                #f
                (with-accessors ((env  lispm-env)
                                 (ans  lispm-ans)
                                 (ktl  lispm-ktl)) *lispm*
                  (declare (ignorable env ans ktl))
                  ,@body)))))

(defun install-byte-code (clause)
  (setf (symbol-value (caar clause))
        (compiled-clause clause)))

(dolist (clause
         '(((SHALLOW-ARGUMENT-REF j)
            (setf ans (svref (car env) j)))

           ((DEEP-ARGUMENT-REF i j)
            (setf ans (svref (nth i env) j)))

           ((SHALLOW-STORE j)
            (setf (svref (car env) j) ans))

           ((SHALLOW-ARGUMENT-SET! j m)
            (setf ktl (list* m
                             `(,(symbol-value 'SHALLOW-STORE) ,j)
                             ktl)))
           ((DEEP-STORE i j)
            (setf (svref (nth i env) j) ans))
           
           ((DEEP-ARGUMENT-SET i j m)
            (setf ktl (list* m
                             `(,(symbol-value 'DEEP-STORE) ,i ,j)
                             ktl)))
           ((CHECKED-GLOBAL-REF sym)
            (let ((val (sym-val sym)))
              (when (eq val undefined-value)
                (error "unbound: ~A" (sym-name sym)))
              (setf ans val)))

           ((PREDEFINED sym)
            (setf ans (sym-val sym)))

           ((GLOBAL-STORE sym)
            (setf (sym-val sym) ans))

           ((GLOBAL-SET! sym m)
            (setf ktl (list* m
                             `(,(symbol-value 'GLOBAL-STORE) ,sym)
                             ktl)))
           ((CONSTANT v)
            (setf ans v))

           ((ALT-BRANCH m2 m3)
            (setf ktl (list*
                       (if ans
                           m2
                         m3)
                       ktl)))

           ((ALTERNATIVE m1 m2 m3)
            (setf ktl (list* m1
                             `(,(symbol-value 'ALT-BRANCH) ,m2 ,m3)
                             ktl)))

           ((CLAUSE-SEQUENCE m m+)
            (setf ktl (list* m m+ ktl)))

           ((RESTORE-ENV envx)
            (setf env envx))

           ((FIX-LET-A m+)
            (setf env (cons ans env)
                  ktl (list* m+ ktl)))

           ((TR-FIX-LET m* m+)
            (setf ktl (list* m*
                             `(,(symbol-value 'FIX-LET-A) ,m+)
                             ktl)))
           ((FIX-LET m* m+)
            (setf ktl (list* m*
                             `(,(symbol-value 'FIX-LET-A) ,m+)
                             `(,(symbol-value 'RESTORE-ENV) ,env)
                             ktl)))
           ((CALL0 address)
            (setf ans (funcall address)))

           ((CALL1-A address)
            (setf ans (funcall address ans)))

           ((CALL1 address m1)
            (setf ktl (list* m1
                             `(,(symbol-value 'CALL1-A) ,address)
                             ktl)))
           ((CALL2-B address ans1)
            (setf ans (funcall address ans1 ans)))
        
           ((CALL2-A address m2)
            (setf ktl (list* m2
                             `(,(symbol-value 'CALL2-B) ,address ,ans)
                             ktl)))
           ((CALL2 address m1 m2)
            (setf ktl (list* m1
                             `(,(symbol-value 'CALL2-A) ,address ,m2)
                             ktl)))
           ((CALL3-C address ans1 ans2)
            (setf ans (funcall address ans1 ans2 ans)))

           ((CALL3-B address ans1 m3)
            (setf ktl (list* m3
                             `(,(symbol-value 'CALL3-C) ,address ,ans1 ,ans)
                             ktl)))
           ((CALL3-A address m2 m3)
            (setf ktl (list* m2
                             `(,(symbol-value 'CALL3-B) ,address ,ans ,m3)
                             ktl)))
           ((CALL3 address m1 m2 m3)
            (setf ktl (list* m1
                             `(,(symbol-value 'CALL3-A) ,address ,m2 ,m3)
                             ktl)))
           ((CALLN-A address m* lst)
            (if m*
                (setf ktl (list* (car m*)
                                 `(,(symbol-value 'CALLN-A) ,address ,(cdr m*) ,(cons ans lst))
                                 ktl))
              (setf ans (apply address (nreverse (cons ans lst))))))
           
           ((CALLN address m*)
            (setf ktl (list* (car m*)
                             `(,(symbol-value 'CALLN-A) ,address ,(cdr m*) nil)
                             ktl)))
           ((CLOSE-OVER fn)
            (setf ans (funcall fn env)))

           ((REGULAR-CALL-B clos)
            (cond ((functionp clos)
                   (setf ans (apply clos (coerce (subseq ans 0 (1- (length ans))) 'list))))
                  (t (funcall (closure-code clos)))
                  ))
                  
           ((REGULAR-CALL-A m*)
            (setf ktl (list* m*
                             `(,(symbol-value 'REGULAR-CALL-B) ,ans)
                             ktl)))
           ((REGULAR-CALL m m*)
            (setf ktl (list*  m
                              `(,(symbol-value 'REGULAR-CALL-A) ,m*)
                              `(,(symbol-value 'RESTORE-ENV) ,env)
                              ktl)))
           ((TR-REGULAR-CALL m m*)
            (setf ktl (list* m
                             `(,(symbol-value 'REGULAR-CALL-A) ,m*)
                             ktl)))

           ((ALLOCATE-FRAME size)
            (setf ans (make-array (1+ size))))

           ((STORE-ARGUMENT-B v* rank)
            (setf (svref v* rank) ans
                  ans             v*))

           ((STORE-ARGUMENT-A m rank)
            (setf ktl (list* m
                             `(,(symbol-value 'STORE-ARGUMENT-B) ,ans ,rank)
                             ktl)))
           ((STORE-ARGUMENT m m* rank)
            (setf ktl (list* m*
                             `(,(symbol-value 'STORE-ARGUMENT-A) ,m ,rank)
                             ktl)))
           ((CONS-ARGUMENT-B v* arity)
            (push ans (svref v* arity))
            (setf ans v*))

           ((CONS-ARGUMENT-A m arity)
            (setf ktl (list m
                            `(,(symbol-value 'CONS-ARGUMENT-B) ,ans ,arity)
                            ktl)))
           ((CONS-ARGUMENT m m* arity)
            (setf ktl (list* m*
                             `(,(symbol-value 'CONS-ARGUMENT-A) ,m ,arity)
                             ktl)))
           ))
  (install-byte-code clause))

;; -------------------------------------------------------------

(defcse SHALLOW-ARGUMENT-REF (j)
  `(,(symbol-value 'SHALLOW-ARGUMENT-REF) ,j))

(defcse DEEP-ARGUMENT-REF (i j)
  `(,(symbol-value 'DEEP-ARGUMENT-REF) ,i ,j))

(defun SHALLOW-ARGUMENT-SET! (j m)
  `(,(symbol-value 'SHALLOW-ARGUMENT-SET!) ,j ,m))

(defun DEEP-ARGUMENT-SET! (i j m)
  `(,(symbol-value 'DEEP-ARGUMENT-SET!) ,i ,j ,m))

(defcse CHECKED-GLOBAL-REF (sym)
  `(,(symbol-value 'CHECKED-GLOBAL-REF) ,sym))

(defcse PREDEFINED (sym)
  `(,(symbol-value 'PREDEFINED) ,sym))

(defun GLOBAL-SET! (sym m)
  `(,(symbol-value 'GLOBAL-SET!) ,sym ,m))

(defun CONSTANT (value)
  #f
  (if (and (integerp value)
           (<= -10 value 10))
      (cse `(CONSTANT ,value)
           `(,(symbol-value 'CONSTANT) ,value))
    `(,(symbol-value 'CONSTANT) ,value)))

(defun ALTERNATIVE (m1 m2 m3)
  `(,(symbol-value 'ALTERNATIVE) ,m1 ,m2 ,m3))

(defun CLAUSE-SEQUENCE (m m+)
  `(,(symbol-value 'CLAUSE-SEQUENCE) ,m ,m+))

(defun TR-FIX-LET (m* m+)
  `(,(symbol-value 'TR-FIX-LET) ,m* ,m+))

(defun FIX-LET (m* m+)
  `(,(symbol-value 'FIX-LET) ,m* ,m+))

;; ----------------------------------------------

(defun CALL0 (address)
  `(,(symbol-value 'CALL0) ,address))

(defun CALL1 (address m1)
  `(,(symbol-value 'CALL1) ,address ,m1))

(defun CALL2 (address m1 m2)
  `(,(symbol-value 'CALL2) ,address ,m1 ,m2))

(defun CALL3 (address m1 m2 m3)
  `(,(symbol-value 'CALL3) ,address ,m1 ,m2 ,m3))

(defun CALLN (address ms)
  `(,(symbol-value 'CALLN) ,address ,ms))

;; -------------------------------------------------------------

(defstruct closure
  code)

(defun THUNK-CLOSURE (m+)
  #f
  `(,(symbol-value 'CLOSE-OVER)
    ,(lambda (envc)
       (declare (ignore envc))
       (make-closure
        :code (lambda ()
                (with-accessors ((env  lispm-env)
                                 (v*   lispm-ans)
                                 (ktl  lispm-ktl)) *lispm*
                  (declare (ignorable env v* ktl))
                  (if (= 1 (the fixnum (length v*)))
                      (push m+ ktl)
                    (error "Incorrect arity")))) ))))

;; -------------------------------------------------------------

(defun FIX-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    `(,(symbol-value 'CLOSE-OVER)
      ,(lambda (envc)
         (make-closure
          :code (lambda ()
                  (with-accessors ((env  lispm-env)
                                   (v*   lispm-ans)
                                   (ktl  lispm-ktl)) *lispm*
                    (declare (ignorable env v* ktl))
                    (let* ((v*  v*)
                           (nel (length v*)))
                      (declare (fixnum nel))
                      (if (= nel arity+1)
                          (progn
                            (setf env (cons v* envc))
                            (push m+ ktl))
                        (error "Incorrect arity")) )))
          )))))

;; -------------------------------------------------------------

(defun NARY-CLOSURE (m+ arity)
  (declare (fixnum arity))
  #f
  (let ((arity+1 (1+ arity)))
    (declare (fixnum arity+1))
    `(,(symbol-value 'CLOSE-OVER)
      ,(lambda (envc)
         (make-closure
          :code (lambda ()
                  (with-accessors ((env  lispm-env)
                                   (v*   lispm-ans)
                                   (ktl  lispm-ktl)) *lispm*
                    (declare (ignorable env v* ktl))
                    (let* ((v*  v*)
                           (nel (length v*)))
                      (declare (fixnum nel))
                      (cond ((>= nel arity+1)
                             ;;; Gather into a list all arguments from arity+1
                             ;;; top the end of the activation frame and store
                             ;;; this list into the arity+1th slot.
                             (setf (svref v* arity)
                                   (coerce (subseq v* arity (the fixnum (1- nel)))
                                           'list))
                             (setf env (cons v* envc))
                             (push m+ ktl))
                            
                            (t (error "Incorrect arity"))
                            )))) )))))

;; -------------------------------------------------------------

(defun TR-REGULAR-CALL (m m*)
  `(,(symbol-value 'TR-REGULAR-CALL) ,m ,m*))

(defun REGULAR-CALL (m m*)
  `(,(symbol-value 'REGULAR-CALL) ,m ,m*))

(defun STORE-ARGUMENT (m m* rank)
  `(,(symbol-value 'STORE-ARGUMENT) ,m ,m* ,rank))

(defun CONS-ARGUMENT (m m* arity)
  `(,(symbol-value 'CONS-ARGUMENT) ,m ,m* ,arity))

(defcse ALLOCATE-FRAME (size)
  `(,(symbol-value 'ALLOCATE-FRAME) ,size))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The threaded interpreter.
;;; E is the expression to evaluate
;;; SR is the representation of the local lexical environment
;;; TAIL? is a boolean that indicates if E is a terminal call (also
;;; means whether the *env* register should be restored or not).

(defun meaning (e r tail?)
  (cond ((keywordp e) (meaning-quotation e r tail?))
        ((symbolp e)  (meaning-reference e r tail?))
        ((atom e)     (meaning-quotation e r tail?))
        ;;
        ((scheme-macro (car e))
         (meaning (scheme-macro-expand e) r tail?))
        ;;
        (t (case (car e)
             (quote  (meaning-quotation (cadr e) r tail?))
             (lambda (meaning-abstraction (cadr e) (cddr e) r tail?))
             (if     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
             (begin  (meaning-sequence (cdr e) r tail?))
             (set!   (meaning-assignment (cadr e) (caddr e) r tail?))
             (lisp   (meaning-lisp-value (cadr e) r tail?))
             (t      (meaning-application (car e) (cdr e) r tail?)) ;; symbols
             ))))

(defun meaning-lisp-value (e r tail?)
  (declare (ignore r tail?))
  (let ((val (eval e)))
    (cond ((functionp val)
           (let ((f (if (compiled-function-p val)
                        val
                      (compile nil val))))
             #f
             (lambda (env)
               (declare (ignore env))
               (make-closure
                :code
                (lambda (v* sr)
                  (declare (ignore sr)
                           ((array t (*)) v*))
                  (apply f
                         (coerce (subseq v* 0
                                         (the fixnum (1-
                                                      (the fixnum (length v*)))))
                                 'list))) )) ))
          (t (CONSTANT val))
          )))

(defun meaning-reference (n r tail?)
  (declare (ignore tail?))
  (let ((kind (or (compute-kind r n)
                  (inherit-lisp-value n nil)
                  (g.current-extend! n))))
    (typecase kind
      (local-sym
       (let ((i (local-sym-level kind))
             (j (local-sym-offs  kind)))
         (declare (fixnum i j))
         (if (= i 0)
             (SHALLOW-ARGUMENT-REF j )
           (DEEP-ARGUMENT-REF i j ))))

      (prim-sym
       (PREDEFINED kind))

      (sym
       (CHECKED-GLOBAL-REF kind))
      )))

(defun meaning-quotation (v r tail?)
  (declare (ignore r tail?))
  (cond ((keywordp v) (CONSTANT v))
        ((symbolp v)  (CONSTANT (or (primitive? v)
                                    (global-variable? v)
                                    (g.current-extend! v))))
        (t (CONSTANT V))
        ))

(defun meaning-alternative (e1 e2 e3 r tail?)
  (let ((m1 (meaning e1 r nil))
        (m2 (meaning e2 r tail?))
        (m3 (meaning e3 r tail?)) )
    (ALTERNATIVE m1 m2 m3) ) )

(defun meaning-assignment (n e r tail?)
  (declare (ignore tail?))
  (let* ((kind (or (compute-kind r n)
                   (g.current-extend! n)))
         (m (meaning e r nil)))
    (typecase kind
      (local-sym
       (let ((i (local-sym-level kind))
             (j (local-sym-offs  kind)))
         (declare (fixnum i j))
         (if (= i 0)
             (SHALLOW-ARGUMENT-SET! j m)
           (DEEP-ARGUMENT-SET! i j m) ) ) )
      (prim-sym
       (error "Immutable predefined variable: ~S" n))
      (sym
       (GLOBAL-SET! kind m))
      (t (error "Not a place: ~S" n))
      )))

(defun meaning-sequence (e+ r tail?)
  (if (consp e+)
      (if (consp (cdr e+))
          (meaning*-multiple-sequence (car (the cons e+))
                                      (cdr (the cons e+))
                                      r tail?)
        (meaning*-single-sequence (car (the cons e+)) r tail?) )
    (error "Illegal syntax: (begin)") ) )

(defun meaning*-single-sequence (e r tail?) 
  (meaning e r tail?) )

(defun meaning*-multiple-sequence (e e+ r tail?)
  (let ((m1 (meaning e r nil))
        (m+ (meaning-sequence e+ r tail?)) )
    (CLAUSE-SEQUENCE m1 m+) ) )

(defun meaning-abstraction (nn* e+ r tail?)
  (um:nlet-tail parse ((n* nn*)
                       (regular nil) )
    ;; traverse the lambda list looking for a dotted pair tail
    (cond
     ((consp n*) (parse (cdr (the cons n*))
                        (cons (car (the cons n*)) regular)))
     ((null n*)  (meaning-fix-abstraction nn* e+ r tail?))
     (t          (meaning-dotted-abstraction 
                  (nreverse regular) n* e+ r tail? )) ) ) )

(defun meaning-fix-abstraction (n* e+ r tail?)
  (declare (ignore tail?))
  (let* ((arity (length n*)))
    (declare (fixnum arity))
    (if (zerop arity)
        (let ((m+ (meaning-sequence e+ r t)))
          (THUNK-CLOSURE m+))
      (let* ((r2 (r-extend* r n*))
             (m+ (meaning-sequence e+ r2 t)) )
        (FIX-CLOSURE m+ arity) ) ) ))

(defun meaning-dotted-abstraction (n* n e+ r tail?)
  (declare (ignore tail?))
  (let* ((arity (length n*))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence e+ r2 t)) )
    (NARY-CLOSURE m+ arity) ) )

;;; Application meaning.

(defun meaning-application (e e* r tail?)
  (cond ((and (symbolp e)
              (let ((kind (or (compute-kind r e)
                              (inherit-lisp-value e t))))
                (and (not (local-sym-p kind))
                     (functionp (sym-val kind))
                     (let ((desc (get-description kind)))
                       (unless (eq :none desc)
                         (when (listp desc) ;; incl NIL
                           (unless (= (length desc) (length e*))
                             (error "Incorrect arity for primitive: ~S" e)) ))
                       (meaning-primitive-application kind e* r tail?))) )))
        ((and (consp e)
              (eql 'lambda (car (the cons e))) )
         (meaning-closed-application e e* r tail?) )
        (t (meaning-regular-application e e* r tail?)) ;; regular symbol functions
        ))

;;; Parse the variable list to check the arity and detect wether the
;;; abstraction is dotted or not.

(defun meaning-closed-application (e ee* r tail?)
  (declare (cons e))
  (let ((nn* (cadr (the cons e))))
    (um:nlet-tail parse ((n* nn*)
                         (e* ee*)
                         (regular nil) )
      (cond
       ((consp n*) 
        (if (consp e*)
            (parse (cdr (the cons n*))
                   (cdr (the cons e*))
                   (cons (car (the cons n*))
                         regular))
          (meaning `((lambda ,(nreverse regular)
                       (lambda ,n* ,@(cddr (the cons e))))
                     ,@ee*)
                   r tail?) ))
       ((null n*)
        (if (null e*)
            (meaning-fix-closed-application 
             nn* (cddr (the cons e)) ee* r tail? )
          (error "Too many arguments: ~S ~S" e ee*) ) )
       (t   (meaning-dotted-closed-application 
             (nreverse regular) n* (cddr (the cons e)) ee* r tail? )) ) ) ) )

(defun meaning-fix-closed-application (n* body e* r tail?)
  (let* ((m* (meaning* e* r (length e*) nil))
         (r2 (r-extend* r n*))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail?
        (TR-FIX-LET m* m+) 
      (FIX-LET m* m+) ) ) )

(defun meaning-dotted-closed-application (n* n body e* r tail?)
  (let* ((m* (meaning-dotted* e* r (length e*) (length n*) nil))
         (r2 (r-extend* r (append n* (list n))))
         (m+ (meaning-sequence body r2 tail?)) )
    (if tail? (TR-FIX-LET m* m+)
        (FIX-LET m* m+) ) ) )

;;; Handles a call to a predefined primitive. The arity is already checked.
;;; The optimization is to avoid the allocation of the activation frame.
;;; These primitives never change the *env* register nor have control effect.

(defun meaning-primitive-application (e e* r tail?)
  (declare (cons e*)
           (ignore tail?))
  (let* ((address (sym-val e))
         (size    (length e*)) )
    (declare (fixnum size))
    (case size
      (0 (CALL0 address))
      (1 
       (let ((m1 (meaning (car e*) r nil)))
         (CALL1 address m1) ) )
      (2 
       (let ((m1 (meaning (car e*) r nil))
             (m2 (meaning (cadr e*) r nil)) )
         (CALL2 address m1 m2) ) )
      (3 
       (let ((m1 (meaning (car e*) r nil))
             (m2 (meaning (cadr e*) r nil))
             (m3 (meaning (caddr e*) r nil)) )
         (CALL3 address m1 m2 m3) ) )
      (t (let ((ms (mapcar (lambda (e)
                             (meaning e r nil))
                           e*)))
           (CALLN address ms)))
      )))

;;; In a regular application, the invocation protocol is to call the
;;; function with an activation frame and a continuation: (f v* k).

(defun meaning-regular-application (e e* r tail?)
  (let* ((m (meaning e r nil))
         (m* (meaning* e* r (length e*) nil)) )
    (if tail?
        (TR-REGULAR-CALL m m*)
      (REGULAR-CALL m m*)) ) )

(defun meaning* (e* r size tail?)
  (if (consp e*)
      (meaning-some-arguments (car (the cons e*))
                              (cdr (the cons e*))
                              r size tail?)
      (meaning-no-argument r size tail?) ) )

(defun meaning-some-arguments (e e* r size tail?)
  (declare (fixnum size))
  (let ((m (meaning e r nil))
        (m* (meaning* e* r size tail?))
        (rank (- size (the fixnum (+ (the fixnum (length e*)) 1))) ))
    (STORE-ARGUMENT m m* rank) ) )

(defun meaning-no-argument (r size tail?)
  (declare (ignore r tail?))
  (ALLOCATE-FRAME size) )


(defun meaning-dotted* (e* r size arity tail?)
  (if (consp e*)
      (meaning-some-dotted-arguments (car (the cons e*))
                                     (cdr (the cons e*)) 
                                     r size arity tail? )
      (meaning-no-dotted-argument r size arity tail?) ) )

(defun meaning-some-dotted-arguments (e e* r size arity tail?)
  (declare (fixnum size arity))
  (let ((m (meaning e r nil))
        (m* (meaning-dotted* e* r size arity tail?))
        (rank (- size (the fixnum (+ (the fixnum (length e*)) 1))) ))
    (if (< rank arity)
        (STORE-ARGUMENT m m* rank)
      (CONS-ARGUMENT m m* arity) ) ) )

(defun meaning-no-dotted-argument (r size arity tail?)
  (declare (ignore r size tail?))
  (ALLOCATE-FRAME arity) )

;; -----------------------------------------------
;; Toplevel REPL

;; for use by Lisp to execute Scheme expressions

(defun eval-sdlisp (expr)
  (operate nil nil (list (meaning expr nil t)) ))

(defmacro with-sdlisp (expr)
  `(eval-sdlisp ',expr))

(editor:setup-indent "with-sdlisp" 1)

(defmethod print-result ((sym sym) stream)
  (format stream "~A" (sym-name sym)))

(defmethod print-result (val stream)
  (format stream "~S" val))

(defmethod print-result ((clos closure) stream)
  (format stream "#<Closure ~x>" (sys:object-address clos)))

(defun sdlisp ()
  (let ((*package* (find-package :sdlisp-bc)))
    (flush-reader-buf)
    (progn ;; handler-case
        (um:nlet-tail toplevel ()
          (let ((x (reader-next)))
            (if (null x)
                (toplevel)
              (unless (member x '(:bye bye))
                (let ((val (eval-sdlisp x)))
                  (format t "~&")
                  (print-result val t)
                  (eval-sdlisp `(begin
                                 (set! ittt itt)
                                 (set! itt  it)
                                 (set! it   ',val)))
                  (toplevel) )))
            ))
        #|
      (error (err)
        (format t "~&ERROR: ~A"
                (um:format-error err))
        (sdlisp))
      |#
      )))

(defun load-file (fname)
  (let ((*package* (find-package :sdlisp-bc)))
    (with-open-file (f fname :direction :input)
      (um:nlet-tail toplevel ()
        (let ((x (reader-next f)))
          (if (null x)
              (toplevel)
            (unless (member x '(:bye bye))
              (eval-sdlisp x)
              (toplevel) ))
          )))))

;; -------------------------------------------
;; Stream reader

(defstruct reader
  buf pos)

(defparameter *reader* (make-reader))

(defun flush-reader-buf ()
  (setf (reader-buf *reader*) nil
        (reader-pos *reader*) 0))

(defun reader-next (&optional (stream *standard-input*))
  (if (interactive-stream-p stream)
      (progn
        (if (or (null (reader-buf *reader*))
                (>= (reader-pos *reader*) (length (reader-buf *reader*))))
            (progn
              (format t "~&>> ")
              (setf (reader-buf *reader*) (read-line stream nil)
                    (reader-pos *reader*) 0)))
        (tagbody
         :again
         (handler-case
             (multiple-value-bind (ans end)
                 (read-from-string (reader-buf *reader*) nil nil
                                   :start (reader-pos *reader*))
               (setf (reader-pos *reader*) end)
               (return-from reader-next ans))
           (error (err)
             (declare (ignore err))
             (format t "~&>>> ")
             (setf (reader-buf *reader*)
                   (concatenate 'string (reader-buf *reader*) " " (read-line stream nil)))
             (go :again))
           )))
    ;; else
    (let ((ans (read stream nil stream)))
      (if (eq ans stream)
          (progn
            (close stream)
            :bye)
        ans))
    ))
        