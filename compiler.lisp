
(in-package :sdlisp-compiler)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Combinators

(defparameter *common-subexpressions* (make-hash-table :test 'equal
                                                       :weak-kind :value))

(defun get-cse (key)
  (gethash key *common-subexpressions*))

(defun memo-cse (key fn)
  (setf (gethash key *common-subexpressions*) fn))

;; ------------------------------------------
;; Common subexpression elimination

(defun do-cse (key fn)
  (or (get-cse key)
      (memo-cse key (funcall fn))))

(defmacro cse ((name &rest args) fn)
  `(do-cse (list* ',name ,@args) (lambda () ,fn)))

(defmacro defcse (name args &body body)
  `(defun ,name ,args
     ,@(butlast body)
     (cse (,name ,@args) ,@(last body))))

#+:LISPWORKS
(editor:setup-indent "defcse" 1)

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
        ((scheme-macro? (car e))
         (meaning (scheme-macro-expand e) r tail?))
        ;;
        (t (case (car e)
             (quote   (meaning-quotation (cadr e) r tail?))
             (lambda  (meaning-abstraction (cadr e) (cddr e) r tail?))
             (if      (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
             (begin   (meaning-sequence (cdr e) r tail?))
             (set!    (meaning-assignment (cadr e) (caddr e) r tail?))
             (lisp    (meaning-lisp-value (cadr e) r tail?))
             (t       (meaning-application (car e) (cdr e) r tail?)) ;; symbols
             ))))

(defun meaning-lisp-value (e r tail?)
  (let ((val (funcall (compile nil `(lambda () ,e)))))
    (cond ((functionp val)
           (let* ((f   (if (compiled-function-p val)
                           val
                         (compile nil val)))
                  (sym (inherit-lisp-function f)))
             (meaning-reference sym r tail?)
             ))
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
  (cond  ((or (eq v nil)  ;; so that '() eq () eq NIL eq 'NIL
              (eq v t))   ;; so that 'T eq T
          (meaning-reference v r tail?))
         
         ((keywordp v) (CONSTANT v))
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
  (um:nlet parse ((n* nn*)
                  (regular nil) )
    ;; traverse the lambda list looking for a dotted pair tail
    (cond
     ((consp n*) (parse (cdr (the cons n*))
                        ;; (cons (car (the cons n*)) regular)
                        (addq regular (car (the cons n*))) ))
     ((null n*)  (meaning-fix-abstraction nn* e+ r tail?))
     (t          (meaning-dotted-abstraction 
                  #|(nreverse regular)|# (car regular)
                                         n* e+ r tail? ))
     )))

(defvar *toplevel* t)

(defun meaning-fix-abstraction (n* e+ r tail?)
  (declare (ignore tail?))
  (let* ((*toplevel*  nil)
         (arity (length n*)))
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
                              (inherit-lisp-value e t)
                              (g.current-extend! e))))
                (and (not (local-sym-p kind))
                     (let ((sv (sym-val kind)))
                       (or (functionp sv)
                           (and (symbolp sv)
                                (fboundp sv))))
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
  (let ((nn* (cadr (the cons e)))) ;; skip leading 'lambda to get args
    (um:nlet parse ((n* nn*)
                    (e* ee*)
                    (regular nil) )
      (cond
       ((consp n*) 
        (if (consp e*)
            (parse (cdr (the cons n*))
                   (cdr (the cons e*))
                   (addq regular (car (the cons n*)))
                   #|
                   (cons (car (the cons n*))
                         regular)
                   |#
                   )
          (meaning `((lambda #|,(nreverse regular)|# ,(car regular)
                       (lambda ,n* ,@(cddr (the cons e))))
                     ,@ee*)
                   r tail?) ))
       ((null n*)
        (if (null e*)
            (meaning-fix-closed-application 
             nn* (cddr (the cons e)) ee* r tail? )
          (error "Too many arguments: ~S ~S" e ee*) ) )
       (t   (meaning-dotted-closed-application 
             #|(nreverse regular)|# (car regular)
                                    n* (cddr (the cons e)) ee* r tail? )) ) ) ) )

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
  (let* ((m  (meaning e r nil))
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

