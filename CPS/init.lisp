;; intit.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces" (part 2)
;;
;; copyright (C) 2008 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package :sdlisp-cps)

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
                  (load  load-file 1)
                  (call/cc call/cc 1)))
    (destructuring-bind (name fn arity) prim
      (g.init-initialize! name fn
                          (case arity
                            (0  nil)
                            (1  '(a))
                            (2  '(a b))
                            (3  '(a b c))
                            (*  '*))) ))
  )

(init-sdlisp)
