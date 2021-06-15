
(in-package :user)

(define-condition amb-failure (error)
  ())

(defun amb-fail ()
  (error (make-condition 'amb-failure)))

(defun scramble (lst)
  #F
  (let* ((v  (coerce lst 'vector)))
    (loop for ix from (1- (length v)) downto 1 do
          (let ((jx (random (1+ ix))))
            (unless (= ix jx)
              (rotatef (aref v jx) (aref v ix)))))
    (coerce v 'list)))

(defun do-amb (&rest fns)
  #F
  (dolist (fn (scramble fns))
    (handler-case
        (return-from do-amb (funcall fn))
      (amb-failure (exn)
        (declare (ignore exn)))
      ))
  (amb-fail))

(defmacro amb (&rest alt-clauses)
  `(locally #F
     (do-amb ,@(mapcar #`(lambda ()
                           ,a1)
                       alt-clauses))))

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

(defun qempty ()
  (list nil))

(defun qempty? (q)
  #F
  (declare (cons q))
  (null (car q)))

(defun qreset (q)
  #F
  (declare (cons q))
  (setf (car q) nil))

(defun qsend (x q)
  #F
  (declare (cons q))
  (let* ((cell (list x))
         (tl   (or (car q)  cell))
         (hd   (or (cdr tl) cell)))
    (declare (cons cell hd tl))
    (setf (cdr tl)   cell
          (cdr cell) hd
          (car q)    cell)
    ))

(defun qread (q)
  #F
  (declare (cons q))
  (um:when-let (tl (car q))
    (locally
      (declare (cons tl))
      (let* ((hd  (cdr tl))
             (ans (car hd)))
        (declare (cons hd))
        (if (eq hd tl)
            (setf (car q) nil)
          (setf (cdr tl) (cdr hd)))
        ans))
    ))

(defun qadd (items q)
  #F
  (declare (cons q)
           (list items))
  (when items
    (let* ((ilast (last (the cons items)))
           (tl    (or (car q)  ilast))
           (hd    (or (cdr tl) items)))
      (declare (cons ilast hd tl))
      (setf (cdr tl)    items
            (cdr ilast) hd
            (car q)     ilast)
      )))

(defun qcontents (q)
  #F
  (declare (cons q))
  (um:when-let (tl (shiftf (car q) nil))
    (shiftf (cdr (the cons tl)) nil)))

(defun qinsert (items q)
  #F
  (declare (cons q)
           (list items))
  (when items
    (let* ((ilast (last (the cons items)))
           (tl    (or (car q)  ilast))
           (hd    (or (cdr tl) items)))
      (declare (cons ilast hd tl))
      (setf (cdr tl)    items
            (cdr ilast) hd
            (car q)     tl)
      )))

#|
(let ((q (qempty)))
  (qadd '(1 2 3) q)
  (qread q)
  (qadd '(a b c) q)
  (qread q)
  (qread q)
  (assert (equalp (qcontents q)
                  '(a b c))))
 |#
;; --------------------------------------------------
;; Breadth-First Choose

(defvar *paths* (qempty))
(defconstant failsym '@)

(defmacro choose (&rest choices)
  `(locally #F
     ,@(when choices
         `((qadd (list ,@(mapcar #`(lambda () ,a1) choices)) *paths*)))
     (fail)))

(defmacro choose-bind (var choices &body body)
  `(locally #F
     (%choose-bind #'(lambda (,var) ,@body) ,choices)))

(defun %choose-bind (fn choices)
  #F
  (declare (list choices))
  (qadd (mapcar #'(lambda (choice)
                    #'(lambda ()
                        (funcall fn choice)))
                choices)
        *paths*)
  (fail))

(defmacro alt-choice (&body body)
  ;; enqueue an alternate choice for later (fail)
  `(locally #F
     (qsend #'(lambda () ,@body) *paths*)))

(defun fail ()
  #F
  (if (qempty? *paths*)
      failsym
    (funcall (qread *paths*))))

(defun reset-choices ()
  (qreset *paths*))

(defun mark ()
  (qsend #'fail *paths*))

(defun cut ()
  (let* ((qc  (qcontents *paths*))
         (pos (position #'fail qc
                        :from-end t)))
    (qinsert (if pos
                 (subseq qc 0 pos)
               qc)
             *paths*)))
    
;; --------------------------------------------------------

#|
(progn
  (=defun two-numbers ()
    (choose-bind n1 '(0 1 2 3 4 5)
      (choose-bind n2 '(0 1 2 3 4 5)
        (=values n1 n2))))

  (=defun parlor-trick (sum)
    (=bind (n1 n2) (two-numbers)
      (if (= (+ n1 n2) sum)
          `(the sum of ,n1 ,n2)
        (fail)))))

(progn
  (=defun descent (n1 n2)
    (cond ((eq n1 n2) (=values (list n2)))
          ((kids n1)  (choose-bind n (kids n1)
                        (=bind (p) (descent n n2)
                          (=values (cons n1 p)))))
          (t (fail))))

  (defun kids (n)
    (case n
      (a  '(b c))
      (b  '(d e))
      (c  '(d f))
      (f  '(g)))))

;; -----------------------------------------------------
;; General Network Connectivity
;; using breadth-first nondeterministic search
;; breadth-first handles paths with cycles

(progn

  (=defun connection-path (n1 n2)
    ;;
    ;; Note: a path must exist from n1 to n2, or else we possibly go
    ;; into an infinite loop if there are any cycles emanating from n1
    ;; Should check with CONNECTIVITY first.
    ;;
    (labels ((sub-cx-for (n)
               (choose-bind nn (connections n)
                 (=bind (p) (connection-path nn n2)
                   (=values (cons n1 p))))))
      
      (cond ((eq n1 n2)
             ;; we connect when we are the same, or through any follow
             ;; path which leads back to n2
             (alt-choice
               (sub-cx-for n2))
             (=values (list n2)))

            (t
             ;; connect through any follow path which leads to n2
             (sub-cx-for n1))
            )))

  (defun connectivity (n1 n2)
    ;; returns nil if no path from n1 to n2
    (let ((trace-list nil))
      (labels ((do-trace (n)
                 (unless (member n trace-list)
                   (push n trace-list)
                   (dolist (nn (connections n))
                     (do-trace nn)))))
        (do-trace n1)
        (member n2 trace-list))))

  (defun connection (n1 n2)
    ;; return a connection path from n1 to n2
    ;; alternate paths shown with subsequent (fail)
    ;; shortest paths shown first
    (cond ((connectivity n1 n2)
           (reset-choices)
           (with-cont
             (loop for ans = (connection-path n1 n2) then (fail) do
                   (princ ans)
                   (when (or (eql ans failsym)
                             (eql 'q (read-from-string (read-line) nil nil)))
                     (loop-finish))
                   )))
  
          (t 'no-connection)
          ))

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

  (defparameter *connections-list*
    '((a . (b c))
      (b . (d e f))
      (c . (e g))
      (d . (b f))
      (e . ())
      (f . ())
      (g . (a))
      ))
  
  (defun connections (n)
    (cdr (assoc n *connections-list*))))

 |#
#|
 CL-USER 666 > (connection 'a 'd)
(A B D)
(A B D B D)
(A C G A B D)
(A B D B D B D)
(A C G A B D B D)q
NIL
|#


