
(defpackage #:pg-prolog-compiler
  (:use #:common-lisp)
  (:export
   #:with-inference
   #:reset-rules
   #:<-
   #:cut
   #:fail
   #:is
   #:lisp
   #:_
   ))

(in-package #:pg-prolog-compiler)

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun use-feature (feat tf)
    (if tf
        (pushnew feat *features*)
      (setf *features* (delete feat *features*))))
  
  (defun use-maps (tf)
    (use-feature :using-maps tf))
  
  (defun use-birthrecords (tf)
    (use-feature :using-birthrecords tf))

  ;; (use-maps t)
  ;; (use-birthrecords t)
  )
|#


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

(defun fail ()
  #F
  (if (null *paths*)
      failsym
    (funcall (pop *paths*))))

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

(defun sym-names-equal? (s1 s2)
  (and (symbolp s1)
       (symbolp s2)
       (string-equal (symbol-name s1) (symbol-name s2))))

(defun is-wild? (x)
  (sym-names-equal? x '_))

(defun varsym? (x)
  (and (symbolp x)
       (char= (char (symbol-name x) 0) #\?)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #`(,a1 (gensym ,(symbol-name a1))) syms)
     ,@body))

(editor:setup-indent "with-gensyms" 1)

(defun simple? (x)
  (or (atom x)
      (eq (car x) 'quote)
      (eq (car x) 'function)))

(defun rep_ (x)
  (cond ((atom x)
         (if (is-wild? x)
             (gensym "?")
           x))
        (t
         (cons (rep_ (car x)) (rep_ (cdr x))))
        ))

(defun vars-in (expr &optional (atom? #'atom))
  (cond ((funcall atom? expr)
         (when (varsym? expr)
           (list expr)))
        (t 
         (union (vars-in (car expr) atom?)
                (vars-in (cdr expr) atom?)))
        ))

#|
(defparameter *stat-miss*  0)
(defparameter *statv*
  (make-array 16
              :adjustable t
              :initial-element 0))

(defun grab-stats (x binds)
  #-:using-maps
  (cond ((varsym? x)
         (let* ((off-end nil)
                (pos (or
                      (position x binds
                                :key #'car)
                      (setf off-end (length binds)))))
           (when (>= pos (length *statv*))
             (setf *statv* (adjust-array *statv* (+ pos 16)
                                         :initial-element 0)))
           (incf (aref *statv* pos))
           (when off-end
             (incf *stat-miss* pos))))

        (t
         (incf (aref *statv* 0)))
        ))

(defun init-stats ()
  (setf *statv* (make-array 16
                            :adjustable t
                            :initial-element 0)
        *stat-miss* 0))
|#

(defun find-binding (sym binds)
  (maps:find binds sym))

(defun add-binding (var val binds)
  (maps:add binds var (list val)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (when (varsym? x)
               (um:aif (find-binding x binds)
                   (or (recbind (car it) binds)
                       it)))))
    (declare (dynamic-extent #'recbind))
    (let ((b (recbind x binds)))
      (values (car b) b))))

(defun match (x y &optional (binds (maps:empty)))
  (acond2
   ((or (eql x y)
        (is-wild? x)
        (is-wild? y))
    (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x)  (values (add-binding x y binds) t))
   ((varsym? y)  (values (add-binding y x binds) t))
   ((and (consp x)
         (consp y)
         (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))
   ))

;; ----------------------------------------------

(defun fullbind (x b)
  (cond ((varsym? x)
         (aif2 (binding x b)
               (fullbind it b)
             (gensym)))

        ((atom x)  x)

        (t  (cons (fullbind (car x) b)
                  (fullbind (cdr x) b)))
        ))

(defun var-bindings (vars binds)
  (mapcar #`(,a1  (fullbind ,a1 ,binds)) vars))

(defun form (pat)
  (if (simple? pat)
      pat
    `(cons ,(form (car pat)) ,(form (cdr pat)))
    ))

(defun normalize-query (expr)
  `(list ',(car expr)
         ,@(mapcar #'form (cdr expr))))

;; -----------------------------------------------------------------
;; Now upgrade the interpreter to a compiler for faster running inferences

(defun read-quit (binds)
  (let ((inp (read-from-string (read-line) nil nil)))
    (cond ((sym-names-equal? inp '?)
           (inspect binds)
           (read-quit binds))

          ((sym-names-equal? inp 'v)
           (sets:view-set binds :key #'maps:map-cell-key)
           (print (sets:cardinal binds))
           (read-quit binds))

          (t 
           (sym-names-equal? inp 'q))
          )))

(defmacro with-inference (query &rest body)
  (let ((vars (vars-in query #'simple?))
        (gb   (gensym)))
    `(with-gensyms ,vars
       (setf *paths* nil)
       (=bind (,gb) ,(gen-query (rep_ query) (maps:empty) '*paths*)
         (block nil
           (let ,(var-bindings vars gb)
             ,@body)
           (when (read-quit ,gb)
             (return))
           (fail))))
    ))

#|
(defvar *S1* (copy-seq *statv*)) ;; without birth records
(defvar *S2* (copy-seq *statv*)) ;; with birth records
(labels ((wt-avg (v)
           (float (/ (vm:total (vops:vmul v (um:range 0 (length v))))
                     (vm:total v)))))
  (let ((s1-mfp (wt-avg *s1*))
        (s2-mfp (wt-avg *s2*)))
    (plt:plot 'plt1 *s1*
              :title "Without Birth Records"
              :xtitle "Length of Search"
              :ytitle "Count"
              :clear t
              :line-type :stepped
              :ylog t
              :yrange '(0.9 1e5)
              :xrange '(0 4000))
    (plt:draw-text 'plt1 (format nil "Mean = ~,1F" s1-mfp) '(:frac 0.75 0.95))
    (plt:plot 'plt2 *s2*
              :title "With Birth Records"
              :xtitle "Length of Search"
              :ytitle "Count"
              :clear t :line-type :stepped :ylog t :yrange '(0.9 1e5) :xrange '(0 4000))
    (plt:draw-text 'plt2 (format nil "Mean = ~,1F" s2-mfp) '(:frac 0.75 0.95))
    (plt:plot 'plt3 *s1*
              :title "Without Birth Records"
              :xtitle "Length of Search"
              :ytitle "Count"
              :clear t
              :line-type :stepped
              :ylog nil
              :yrange '(0.9 100)
              :xrange '(0 1000))
    (plt:draw-text 'plt3 (format nil "Mean = ~,1F" s1-mfp) '(:frac 0.75 0.95))
    (plt:plot 'plt4 *s2*
              :title "With Birth Records"
              :xtitle "Length of Search"
              :ytitle "Count"
              :clear t
              :line-type :stepped
              :ylog nil
              :yrange '(0.9 100)
              :xrange '(0 1000))
    (plt:draw-text 'plt4 (format nil "Mean = ~,1F" s2-mfp) '(:frac 0.75 0.95))
    ))

(labels ((get-cums (v)
           (let* ((ixs (um:range 0 (length v)))
                  (pairs (delete-if #'zerop (um:zip ixs (coerce v 'list)) :key #'second))
                  (cum 0))
             (loop for (len ntimes) in pairs collect
                   (list len (incf cum (* ntimes len)))))))
  (let* ((c1 (get-cums *s1*)) ;; without birth records
         (c2 (get-cums *s2*)) ;; with birth records
         (xlim (reduce #'max (map 'vector #'first c2)))
         (ylim (reduce #'max (map 'vector #'second c1))))
    (plt:plot 'cplot
              (map 'vector #'first c1)
              (map 'vector #'second c1)
              :title "Cumulative Searches"
              :xtitle "Length of Search"
              :ytitle "Cumulative Count"
              :clear t
              :ylog  nil
              :xrange `(0 ,(* 1.075 xlim))
              :yrange `(0.9 ,(* 1.075 ylim))
              ;; :xrange '(0 1000)
              ;; :yrange '(0 2e6)
              :legend "Without BirthRecords")
    (plt:plot 'cplot
              (map 'vector #'first c2)
              (map 'vector #'second c2)
              :color :red
              :legend "With BirthRecords")
    ))
                   
  
|#

#|
(time
 (loop repeat 10 do
       (let ((ct 0))
         (block test
           (with-inference (bfs-path 'a 'e ?p)
                           (incf ct)
                           ;; (print ?p)
                           (if (< ct 15)
                               (fail)
                             (return-from test :ok)))))))
 |#

(defun gen-query (expr binds paths)
  (case (car expr)
    (and  (gen-and  (cdr expr)  binds paths))
    (or   (gen-or   (cdr expr)  binds paths))
    (not  (gen-not  (cadr expr) binds paths))
    (lisp (gen-lisp (cadr expr) binds))
    (is   (gen-is   (cadr expr) (third expr) binds))
    (cut  `(progn
             (setf *paths* ,paths)
             (=values ,binds)))
    (t    `(prove ,(normalize-query expr) ,binds *paths*))
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
  `(let ,(var-bindings (vars-in expr) binds)
     ,expr))

(defun gen-lisp (expr binds)
  `(if (with-binds ,binds ,expr)
       (=values ,binds)
     (fail)))

(defun gen-is (expr1 expr2 binds)
  `(aif2 (match ,expr1 (with-binds ,binds ,expr2) ,binds)
         (=values it)
         (fail)))

(=defun prove (query binds paths)
  (let* ((rule-name (car query))
         (rules     (get rule-name 'prolog-rules)))
    (choose-bind r rules
      (=funcall r query binds paths))))

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

(defun rule-fn (ant con)
  (with-gensyms (new-binds win fact binds paths)
    `(=lambda (,fact ,binds ,paths)
       (with-gensyms ,(vars-in (list ant con) #'simple?)
         (multiple-value-bind (,new-binds ,win)
             (match ,fact ,(normalize-query con) ,binds)
           (if ,win
               ,(gen-query ant new-binds paths)
             (fail))
           )))
    ))

(defun reset-rules (names)
  (dolist (name names)
    (remprop name 'prolog-rules)))

(defmacro <- (con &rest ant)
  (let* ((ant (if (= (length ant) 1)
                  (car ant)
                `(and ,@ant)))
         (rule-name (car con)))
    `(length (um:conc1f (get ',rule-name 'prolog-rules)
                        ,(rule-fn (rep_ ant) (rep_ con))))
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
  
  (<- (direct-path ?x ?x _ (?x)))
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
  ;; Connection paths using Breadth-First Search
  ;; with an overtly depth-first engine
  
  (reset-rules '(bfs-path
                 bfs/q
                 enq-alts
                 reverse
                 rev/3
                 car
                 all-cxs
                 enq-cxs))

  (<- (bfs-path ?x ?y ?p)
      ;; prevent infinite loop looking for unobtainium in cyclic
      ;; graphs
      (all-cxs (?x) () ?a)
      (member ?y ?a)
      ;; at this point, it must be reachable from here...
      (bfs/q ((?x)) ?y ?p))

  (<- (bfs/q (?xhd . _) ?x ?p)
      (car ?xhd ?x)
      (reverse ?xhd ?p))
  (<- (bfs/q (?xhd . ?xs) ?y ?p)
      (car ?xhd ?hd)
      (connections ?hd ?hds)
      (enq-alts ?xhd ?hds ?xs ?q)
      (bfs/q ?q ?y ?p))
  (<- (bfs/q (_ . ?xs) ?y ?p)
      (bfs/q ?xs ?y ?p))

  (<- (enq-alts ?xhd () ?p ?p))
  (<- (enq-alts ?xhd (?xh . ?xt) ?p ?q)
      (append ?p ((?xh . ?xhd)) ?pp)
      (enq-alts ?xhd ?xt ?pp ?q))

  (<- (reverse ?x ?y)
      (rev/3 ?x () ?y))
  
  (<- (rev/3 () ?x ?x))
  (<- (rev/3 (?hd . ?tl) ?x ?y)
      (rev/3 ?tl (?hd . ?x) ?y))
               
  (<- (car (?a . _) ?a))

  (<- (all-cxs () ?a ?a))
  (<- (all-cxs (?h . ?t) ?y ?a)
      (not (member ?h ?y))
      (cut)
      (enq-cxs ?h ?t ?q)
      (all-cxs ?q (?h . ?y) ?a))
  (<- (all-cxs (_ . ?t) ?y ?a)
      (all-cxs ?t ?y ?a))

  (<- (enq-cxs ?h ?t ?q)
      (connections ?h ?hs)
      (cut)
      (append ?t ?hs ?q))
  (<- (enq-cxs _ ?q ?q))
  
  ;; ------------------------------------
  
  (reset-rules '(append
                 quicksort
                 partition))

  (<- (append () ?ys ?ys))
  (<- (append (?x . ?xs) ?ys (?x . ?zs))
      (append ?xs ?ys ?zs))

  (<- (quicksort () ()))
  (<- (quicksort (?x . ?xs) ?ys)
      (partition ?xs ?x ?littles ?bigs)
      (quicksort ?littles ?ls)
      (quicksort ?bigs ?bs)
      (append ?ls (?x . ?bs) ?ys))

  (<- (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
      (lisp (<= ?x ?y))
      (partition ?xs ?y ?ls ?bs))
  (<- (partition (?x . ?xs) ?y ?ls (?x . ?bs))
      (lisp (> ?x ?y))
      (partition ?xs ?y ?ls ?bs))
  (<- (partition nil ?y nil nil))

  ;; -----------------------------

  (reset-rules '(integrate))

  (<- (integrate ?f ?a ?b ?c)
      (midpt ?a ?b ?m)
      (is ?v (funcall ?f ?m))
      (is ?c (* ?v (- ?b ?a))))
  (<- (integrate ?f ?a ?b ?c)
      (midpt ?a ?b ?m)
      (integrate ?f ?a ?m ?ca)
      (integrate ?f ?m ?b ?cb)
      (is ?c (+ ?ca ?cb)))

  (<- (midpt ?a ?b ?m)
      (is ?m (/ (+ ?a ?b) 2)))
  )

#|
(with-inference (path 'a 'e ?p) (princ ?p))
=>
(A B E)
(A C E)
@

(with-inference (bfs-path 'a 'e ?p)
                (princ ?p))
=>
(A B E)
(A C E)
(A B D B E)
(A C G A B E)
(A C G A C E)
(A B D B D B E)
(A C G A B D B E)
(A B D B D B D B E)
(A C G A C G A B E)
(A C G A C G A C E)
(A C G A B D B D B E)
(A B D B D B D B D B ...)q
|#

