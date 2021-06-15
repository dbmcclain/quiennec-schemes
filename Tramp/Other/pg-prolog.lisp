
(defpackage #:pg-prolog
  (:use #:common-lisp)
  (:export
   ))

(in-package #:pg-prolog)

;; --------------------------------------------------

(defmacro =lambda (parms &body body)
  `#'(lambda (%sk ,@parms) #F ,@body))

(defmacro =defun (name parms &body body)
  (let ((f (um:symb '= name)))
    `(progn
       (defmacro ,name ,parms
         `(locally #F
            (,',f %sk ,,@parms)))
       (defun ,f (%sk ,@parms) #F ,@body))))

(defmacro =bind (parms expr &body body)
  `(locally #F
     (let ((%sk #'(lambda ,parms ,@body)))
       ,expr)))

(defmacro =values (&rest retvals)
  `(locally #F
     (funcall %sk ,@retvals)))

(defmacro =funcall (fn &rest args)
  `(locally #F
     (funcall ,fn %sk ,@args)))

(defmacro =apply (fn &rest args)
  `(locally #F
     (apply ,fn %sk ,@args)))

(defmacro with-cont (&body body)
  `(let ((%sk #'values))
     (locally #F
       ,@body)))

;; --------------------------------------------------
;; Depth-First Choose

(defparameter *paths*  nil)
(defconstant failsym '@)

(defmacro choose (&rest choices)
  `(locally #F
     ,@(when choices
         `((setf *paths* (nconc (list ,@(mapcar #`(lambda () ,a1) choices)) *paths*))))
     (fail)))

(defmacro choose-bind (var choices &body body)
  `(locally #F
     (%choose-bind #'(lambda (,var) ,@body) ,choices)))

(defun %choose-bind (fn choices)
  #F
  (declare (list choices))
  (setf *paths* (nconc (mapcar #'(lambda (choice)
                                   #'(lambda ()
                                       (funcall fn choice)))
                               choices)
                       *paths*))
  (fail))

(defmacro alt-choice (&body body)
  ;; enqueue an alternate choice for later (fail)
  `(locally #F
     (push #'(lambda () ,@body) *paths*)))

(defun fail ()
  #F
  (if (null *paths*)
      failsym
    (funcall (pop *paths*))))

(defun reset-choices ()
  (setf *paths* nil))

(defun mark ()
  (push #'fail *paths*))

(defun cut ()
  #F
  (cond ((null *paths*))
        ((eq #'fail (car *paths*))
         (pop *paths*))
        (t
         (pop *paths*)
         (cut))
        ))
    
;; --------------------------------------------------

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro acond2 (&rest clauses)
  (when clauses
    (let ((cl1 (car clauses))
          (val (gensym))
          (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
         (if (or ,val ,win)
             (let ((it ,val))
               (declare (ignorable it ,val))
               ,@(cdr cl1))
           (acond2 ,@(cdr clauses))))
      )))

;; --------------------------------------------------
;; Start with PG Interpreter...

(defmacro with-inference (query &body body)
  (let ((binds (gensym)))
    `(with-cont
       (setf *paths* nil)
       (=bind (,binds) (prove-query ',(rep_ query) nil)
         (let ,(mapcar #'(lambda (v)
                           `(,v (fullbind ',v ,binds)))
                       (vars-in query #'atom))
           ,@body
           (fail))))
    ))

(defun rep_ (x)
  (cond ((atom x)
         (if (eq x '_)
             (gensym "?")
           x))
        (t
         (cons (rep_ (car x)) (rep_ (cdr x))))
        ))

(defun fullbind (x b)
  (cond ((varsym? x)
         (aif2 (binding x b)
               (fullbind it b)
             (gensym)))

        ((atom x)  x)

        (t  (cons (fullbind (car x) b)
                  (fullbind (cdr x) b)))
        ))

(defun varsym? (x)
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

(defun gensym? (s)
  (and (symbolp s)
       (not (symbol-package s))))

(defun vars-in (expr &optional (atom? #'atom))
  (cond ((funcall atom? expr)
         (when (varsym? expr)
           (list expr)))
        (t 
         (union (vars-in (car expr) atom?)
                (vars-in (cdr expr) atom?)))
        ))

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y)
        (eql x '_)
        (eql y '_))
    (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x)  (values (cons (cons x y) binds) t))
   ((varsym? y)  (values (cons (cons y x) binds) t))
   ((and (consp x)
         (consp y)
         (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))
   ))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (um:aif (assoc x binds)
                 (or (recbind (cdr it) binds)
                     it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))
    ))

(=defun prove-query (expr binds)
  (case (car expr)
    (and  (prove-and (cdr expr) binds))
    (or   (prove-or  (cdr expr) binds))
    (not  (prove-not (cadr expr) binds))
    (t    (prove-simple expr binds))
    ))

(=defun prove-and (clauses binds)
  (if (null clauses)
      (=values binds)
    (=bind (binds) (prove-query (car clauses) binds)
      (prove-and (cdr clauses) binds))
    ))

(=defun prove-or (clauses binds)
  (choose-bind c clauses
               (prove-query c binds)))

(=defun prove-not (expr binds)
  (let ((save-paths *paths*))
    (setf *paths* nil)
    (choose (=bind (b) (prove-query expr binds)
              (declare (ignore b))
              (setf *paths* save-paths)
              (fail))
            (progn
              (setf *paths* save-paths)
              (=values binds))
            )))

(defvar *rlist*  nil)

(=defun implies (r query binds)
  (let ((r2 (change-vars r)))
    (multiple-value-bind (it win)
        (match query (cdr r2) binds)
      (if (or it win)
          (prove-query (car r2) it)
        (fail))
      )))

(=defun prove-simple (query binds)
  (choose-bind r *rlist*
               (implies r query binds)))

(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
               `(and ,@ant))))
    `(length (um:conc1f *rlist* (rep_ (cons ',ant ',con))))
    ))

(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
                      (cons v (um:symb '? (gensym))))
                  (vars-in r #'atom))
          r))

;; ----------------------------------------------------
(progn
  (<- (gaunt raoul))
  (<- (smells-of raoul turpentine))
  (<- (painter rubens))
  
  (<- (painter ?x)
      (hungry ?x)
      (smells-of ?x turpentine))
  (<- (hungry ?x)
      (or (gaunt ?x)
          (eats-ravenously ?x)))

  (<- (append nil ?xs ?xs))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
      (append ?xs ?ys ?zs))
  )

#|
PG-PROLOG 679 > (with-inference (painter ?x) (print ?x))

RUBENS 
RAOUL 
@

PG-PROLOG 680 > (with-inference (append ?x (c d) (a b c d)) (format t "Left: ~A~%" ?x))
Left: (A B)
@

PG-PROLOG 681 > (with-inference (append (a b) ?x (a b c d)) (format t "Right: ~A~%" ?x))
Right: (C D)
@

PG-PROLOG 682 > (with-inference (append (a b) (c d) ?x) (format t "Whole: ~A~%" ?x))
Whole: (A B C D)
@

PG-PROLOG 683 > (with-inference (append ?x ?y (a b c)) (format t "Left: ~A  Right: ~A~%" ?x ?y))
Left: NIL  Right: (A B C)
Left: (A)  Right: (B C)
Left: (A B)  Right: (C)
Left: (A B C)  Right: NIL
@

 |#

;; -----------------------------------------------------------------
;; Now upgrade the interpreter to a compiler for faster running inferences

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(editor:setup-indent "with-gensyms" 1)

(defun simple? (x)
  (or (atom x)
      (eq (car x) 'quote)))

(defun user-varsym? (x)
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

(defun user-vars-in (expr &optional (atom? #'atom))
  (cond ((funcall atom? expr)
         (when (user-varsym? expr)
           (list expr)))
        (t 
         (union (user-vars-in (car expr) atom?)
                (user-vars-in (cdr expr) atom?)))
        ))

(defmacro with-inference (query &rest body)
  (let ((vars (user-vars-in query #'simple?))
        (gb   (gensym)))
    `(with-gensyms ,vars
       (setf *paths* nil)
       (=bind (,gb) ,(gen-query (rep_ query) nil '*paths*)
         (block nil
           (let ,(mapcar #'(lambda (v)
                             `(,v (fullbind ,v ,gb)))
                         vars)
             ,@body)
           (when (eql 'q (read-from-string (read-line) nil nil))
             (return))
           (fail))))
    ))

(defun varsym? (x)
  (and (symbolp x)
       (not (symbol-package x))))

(defun gen-query (expr binds paths)
  (case (car expr)
    (and  (gen-and (cdr expr) binds paths))
    (or   (gen-or  (cdr expr) binds paths))
    (not  (gen-not (cadr expr) binds paths))
    (lisp (gen-lisp (cadr expr) binds))
    (is   (gen-is  (cadr expr) (third expr) binds))
    (cut  `(progn
             (setf *paths* ,paths)
             (=values ,binds)))
    (t    `(prove (list ',(car expr)
                        ,@(mapcar #'form (cdr expr)))
                  ,binds *paths*))
    ))

(defun gen-and (clauses binds paths)
  (cond ((null clauses)
         `(=values ,binds))
        (t
         (let ((gb (gensym)))
           `(=bind (,gb) ,(gen-query (car clauses) binds paths)
              ,(gen-and (cdr clauses) gb paths))
           ))
        ))

(defun gen-or (clauses binds paths)
  `(choose
    ,@(mapcar #'(lambda (c)
                  (gen-query c binds paths))
              clauses)))

(defun gen-not (expr binds paths)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setf *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds paths)
                 (declare (ignore b))
                 (setf *paths* ,gpaths)
                 (fail))
               (progn
                 (setf *paths* ,gpaths)
                 (=values ,binds))
               ))))

(defmacro with-binds (binds expr)
  `(let ,(mapcar #'(lambda (v)
                     `(,v (fullbind ,v ,binds)))
                 (user-vars-in expr))
     ,expr))

(defun gen-lisp (expr binds)
  `(if (with-binds ,binds ,expr)
       (=values ,binds)
     (fail)))

(defun gen-is (expr1 expr2 binds)
  `(aif2 (match ,expr1 (with-binds ,binds ,expr2) ,binds)
         (=values it)
         (fail)))

(defun reset-rules (names)
  (dolist (name names)
    (remprop name 'prolog-rules)))

(=defun prove (query binds paths)
  (let* ((rule-name (car query))
         (rules     (get rule-name 'prolog-rules)))
    (choose-bind r rules
      (=funcall r query binds paths))))

(defun form (pat)
  (if (simple? pat)
      pat
    `(cons ,(form (car pat)) ,(form (cdr pat)))
    ))

#|
(with-inference (and (big ?x) (red ?x))
                (print ?x))
=>
(WITH-GENSYMS (?X)
  (SETF *PATHS* NIL)
  (=BIND (#:G20835)
      (=BIND (#:G20836) (PROVE (LIST 'BIG ?X) NIL)
        (=BIND (#:G20837) (PROVE (LIST 'RED ?X) #:G20836)
          (=VALUES #:G20837)))
    (LET ((?X (FULLBIND ?X #:G20835)))
      (PRINT ?X))
    (FAIL)))
|#

(defmacro <- (con &rest ant)
  (let* ((ant (if (= (length ant) 1)
                  (car ant)
                `(and ,@ant)))
         (rule-name (car con)))
    `(length (um:conc1f (get ',rule-name 'prolog-rules)
                        ,(rule-fn (rep_ ant) (rep_ con))))
    ))

(defun rule-fn (ant con)
  (with-gensyms (val win fact binds paths)
    `(=lambda (,fact ,binds ,paths)
       (with-gensyms ,(user-vars-in (list ant con) #'simple?)
         (multiple-value-bind (,val ,win)
             (match ,fact
                    (list ',(car con)
                          ,@(mapcar #'form (cdr con)))
                    ,binds)
           (if ,win
               ,(gen-query ant val paths)
             (fail))
           )))
    ))

#|
(<- (member ?x (_ . ?rest)) (member ?x ?rest))
 |#

  ;; --------------------------------------------
  ;;
  ;; 		   +---------------+
  ;;		   |	    +---+  |
  ;;		   |	+-->| d |--+
  ;;               |   /    +---+
  ;;		   v  /	      |	 				  
  ;;		 +---+ 	      |        +---+
  ;;	      +->| b |--------|------->| e |
  ;;	     /	 +---+	      |        +---+
  ;;	    /  	      \	      v	      	 ^   
  ;;   +---+	       \    +---+     	 |   
  ;;   | a |	 	+-->| f |     	 |   
  ;;   +---+   	       	    +---+      	 |   
  ;;     ^  \  	    +--------------------+   
  ;;	 |   \ 	    |  	       	       	     
  ;;   	 |    \	  +---+	    +---+      	     
  ;;	 |     +->| c |---->| g |   ( _ .._ Emacs )
  ;;	 |	  +---+	    +---+     
  ;;	 |		      |	      
  ;;	 +--------------------+
  ;;

(progn
  (reset-rules '(connections
                 connection
                 path
                 direct-path
                 member))
  
  (<- (connections 'a '(b c)))
  (<- (connections 'b '(d e f)))
  (<- (connections 'c '(e g)))
  (<- (connections 'd '(b f)))
  (<- (connections 'g '(a)))

  (<- (connection ?x ?x))
  (<- (connection ?x ?y)
      (connections ?x ?ns)
      (member ?y ?ns))
  
  (<- (direct-path ?x ?x ?v (?x)))
  (<- (direct-path ?x ?y ?v (?x . ?p))
      (connection ?x ?n)
      (not (member ?n ?v))
      (direct-path ?n ?y (?n . ?v) ?p))

  (<- (member ?x (?x . _)))
  (<- (member ?x (_ . ?rest))
      (member ?x ?rest))
  
  (<- (path ?x ?y ?p)
      (direct-path ?x ?y (?x) ?p))

  ;; ------------------------------------
  
  (reset-rules '(append
                 quicksort
                 partition))

  (<- (append nil ?ys ?ys))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
      (append ?xs ?ys ?zs))

  (<- (quicksort (?x . ?xs) ?ys)
      (partition ?xs ?x ?littles ?bigs)
      (quicksort ?littles ?ls)
      (quicksort ?bigs ?bs)
      (append ?ls (?x . ?bs) ?ys))
  (<- (quicksort nil nil))

  (<- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
      (lisp (<= ?x ?y))
      (partition ?xs ?y ?ls ?bs))
  (<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
      (lisp (> ?x ?y))
      (partition ?xs ?y ?ls ?bs))
  (<- (partition nil ?y nil nil))
  )

#|
(with-inference (path 'a 'e ?p)
                (print ?p))
|#

