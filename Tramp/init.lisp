;; init.lisp
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Adopted from Christian Quiennec's book "Lisp in Small Pieces" (part 2)
;;
;; Copyright (C) 2008,2-17 by Refined Audiometrics Laboratory, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(in-package :sdlisp-tramp)

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Initialization of the predefined global environment.

(defun init-sdlisp ()
  ;; define immutable values
  (setf g.current  (make-hash-table)
        g.init     (make-hash-table)
        g.lispfns  (make-hash-table)
        g.params   (maps:empty))
        
  (dolist (var `((t    ,t)
                 (nil  ,nil)))
    (g.init-initialize! (car var) (cadr var)))

  ;; define initial mutable values
  (dolist (var '(it itt ittt))
    (g.current-extend! var))

  (with-sdlisp
      (begin
       (set! it   nil)
       (set! itt  nil)
       (set! ittt nil)))
  
  ;; define primitives
  (dolist (prim '((arity arity 1)
                  (cons cons 2)
                  (car  car  1)
                  (cdr  cdr  1)
                  (list list *)
                  (list* list* *)
                  (null?  null 1)
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
                  (+call/cc call/cc 1)
                  (dyn-wrap dyn-wrap 1)
                  (wait sleep 1)
                  ))
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

;; ---------------------------------------------
;; support code

(def-scheme-macro prog1 (clause &rest clauses)
  (let ((ans  (gensym)))
    `(let ((,ans ,clause))
       ,@clauses
       ,ans)))

(def-scheme-macro when (pred &rest body)
  `(if ,pred
       (begin
        ,@body)))

(def-scheme-macro unless (pred &rest body)
  `(when (not ,pred) ,@body))

(def-scheme-macro loop (&rest body)
  `(+loop (lambda () ,@body)))

;; --------------------------------------------
;; PARAMS & Dynamic Bindings

(def-scheme-macro params (bindings &rest body)
  `(+params ',(mapcar (um:compose #'first #'um:mklist) bindings)
            (list ,@(mapcar (um:compose #'second #'um:mklist) bindings))
            (lambda ()
              ,@body)))

(def-scheme-macro params* (bindings &rest body)
  (if (null bindings)
      `(begin
        ,@body)
    ;; else
    `(params (,(car bindings))
       (params* ,(cdr bindings) ,@body))
    ))


(editor:setup-indent "params"  1)
(editor:setup-indent "params*" 1)

;; ---------------------------------------------
;; UNWIND-PROTECT

(def-scheme-macro unwind-protect (clause &rest clauses)
  `(+unwind-protect (lambda ()
                      ,clause)
                    (lambda ()
                      ,@clauses)))

;; -------------------------------------------------
;; CATCH / THROW

(def-scheme-macro catch (tag &rest body)
  `(+catch ',tag (lambda () ,@body)))

(def-scheme-macro throw (tag retval)
  `(+throw ',tag ,retval))

;; --------------------------------------------------
;; TRY

(def-scheme-macro try (clause err-fn)
  `(+try (lambda ()
           ,clause)
         ,err-fn))

;; --------------------------------------------
;; AMB & Backtracking

(defun scramble (lst)
  (let* ((v  (coerce lst 'vector)))
    (loop for ix from (1- (length v)) downto 1 do
          (let ((jx (random (1+ ix))))
            (unless (= ix jx)
              (rotatef (aref v jx) (aref v ix)))))
    (coerce v 'list)))
        
(def-scheme-macro amb (&rest alts)
  `(+amb ,@(mapcar #`(lambda ()
                       ,a1)
                   alts)))

;; -----------------------------------------------
;;; handling &key args

(def-scheme-macro with-options (args &rest body)
  `(+with-options (lambda () ,@body) ,@args))

(editor:setup-indent "with-options" 1)

;; --------------------------------------------
;; Support code for unwinds and errors

(with-sdlisp
    (begin

     ;; --- user level code ------------

     (define first   car)
     (define second  (lisp #'cadr))
     (define third   (lisp #'caddr))
     (define fourth  (lisp #'cadddr))
     
     (define (identity x)
       x)
     
     (define (constantly val)
       (lambda args
         val))

     (define true         (constantly t))
     (define false        (constantly nil))
     (define do-nothing   false)

     (define (+loop fn)
       (catch loop-finish
         (let iter ()
           (fn)
           (iter))))

     (define (loop-finish retval)
       (throw loop-finish retval))
     
     (define (dynamic-wind thunk1 thunk2 thunk3)
       (thunk1)
       (push (list (get-global-state) thunk1 thunk3) *winds*)
       (prog1
           (thunk2)
         (pop *winds*)
         (thunk3)))

     (define (error msg)
       (throw error msg))

     (define (call/cc fn)
       (let ((winds   *winds*)
             (state   (get-global-state)))
         (+call/cc
          (lambda (+sk)
            (fn (lambda (retval)
                  (+do-winds *winds* winds)
                  (set-global-state state)
                  (+sk retval))
                )))
         ))

     ;; -----------------------------------------

     (define *options*  (list :key  identity
                              :test eq?))

     (define getf   (Lisp #'getf))
     
     (define (+with-options fn . args)
       (params ((*options* (append args *options*)))
         (fn)))

     (define (getopt key)
       (getf *options* key))
       
     (define (curry fn . pre-args)
       (lambda post-args
         (apply fn (append pre-args post-args))))

     (define (rcurry fn . post-args)
       (lambda pre-args
         (apply fn (append pre-args post-args))))

     (define (compose . fns)
       (lambda (arg)
         (foldr (lambda (init fn)
                  (fn init))
                arg fns)))

     (define (invoke fn)
       (fn))

     ;; -------------------------------------

     (define (iter fn lst)
       (when lst
         (fn (car lst))
         (iter fn (cdr lst))))
     
     (define (foldl fn lst init)
       (if lst
           (foldl fn (cdr lst) (fn (car lst) init))
         init))

     (define (reverse lst)
       (foldl cons lst nil))

     (define (foldr fn init lst)
       (foldl fn (reverse lst) init))

     (define (revmap fn lst)
       (foldl (lambda (item init)
                (cons (fn item) init))
              lst nil))

     (define (map fn lst)
       (reverse (revmap fn lst)))

     (define (every fn lst)
       (if lst
           (and (fn (car lst))
                (every fn (cdr lst)))
         t))

     (define (some fn lst)
       (when lst
         (or (fn (car lst))
             (some fn (cdr lst)))))
     
      (define (mapcars fn . lsts)
       (letrec ((iter (lambda (lsts init)
                        (if (every identity lsts)
                            (iter (map cdr lsts) (cons (apply fn (map car lsts)) init))
                          init))
                      ))
           (reverse (iter lsts nil))))

     (define (remove-if pred lst)
       (let ((keyfn (getopt :key)))
         (reverse (foldl (lambda (it acc)
                           (if (pred (keyfn it))
                               acc
                             (cons it acc)))
                         lst nil))))

     (define (eq-item item)
       (curry (getopt :test) item))
     
     (define (remove item lst)
       (remove-if (eq-item item) lst))

     (define (member-if pred lst)
       (letrec ((keyfn (getopt :key))
                (iter  (lambda (lst)
                         (when lst
                           (if (pred (keyfn (car lst)))
                               lst
                             (iter (cdr lst)))))
                       ))
           (iter lst)))

     (define (member item lst)
       (member-if (eq-item item) lst))
     
     (define (assoc item alst)
       (car (with-options (:key car)
              (member item alst))))

     (define (acons a b alst)
       (cons (cons a b) alst))

     (define (find-if pred lst)
       (car (member-if pred lst)))

     (define (find item lst)
       (find-if (eq-item item) lst))

     (define (position-if pred lst)
       (letrec ((keyfn (getopt :key))
                (iter  (lambda (lst pos)
                         (when lst
                           (if (pred (keyfn (car lst)))
                               pos
                             (iter (cdr lst) (1+ pos)))))
                       ))
           (iter lst 0)))

     (define (position item lst)
       (position-if (eq-item item) lst))
     
     (define zip
       (curry mapcars list))

     ;; --- for internal use -------------
     (define +error     (lisp #'error))

     (define *winds*    nil) ;; a list of (global-state entry-thunk exit-thunk)
     (define *catches*  nil) ;; a list of (tag . cont)

     (define (get-global-state)
       ;; a complete global environment has *catches*, params, and *winds*
       ;; local binding env and exec stack are handled by continuations
       (list *catches*
             (get-params)
             *winds*))

     (define (set-global-state state)
       (set! *catches* (first state))
       (set-params (second state))
       (set! *winds*   (third state)))

     (define (+do-winds from to)
       (letrec ((state       first)
                (entry-thunk second)
                (exit-thunk  third)
                (restore-env (lambda (rec sel)
                               (set-global-state (state rec))
                               ((sel rec)) ))
                (lctprefs    (lambda (from to)
                               ;; return reversed prefixes of from and to
                               ;; that share the longest common tail
                               (letrec ((iter (lambda (rfrom rto)
                                                (if (and rfrom rto
                                                         (eq? rfrom rto))
                                                    (iter (cdr rfrom) (cdr rto))
                                                  (list rfrom rto)))
                                              ))
                                   (iter (reverse from) (reverse to)))))
                (restore     (lambda (rfrom rto)
                               (iter (lambda (rec)
                                       (restore-env rec exit-thunk))
                                     (reverse rfrom))
                               (iter (lambda (rec)
                                       (restore-env rec entry-thunk))
                                     rto))))
           (apply restore (lctprefs from to))
         ))

     (define (+params vars vals fn)
       (dyn-wrap (lambda ()
                   (do ((vars  vars  (cdr vars))
                        (vals  vals  (cdr vals)))
                       ((null? vars)
                        (fn))
                     (extend-params (car vars) (car vals))))
                 ))

     (define (+throw tag retval)
       (letrec ((find-cont (lambda ()
                             (cdr (assoc tag *catches*))))
                (errout (lambda (msg . args)
                          (+do-winds *winds* nil)
                          (set-global-state nil)
                          (apply +error msg args)))
                )
           
           (cond ((find-cont) =>
                  (lambda (sk)
                    (sk retval)))
                 
                 ((eq? 'error tag)
                  (errout retval))
                 
                 (else
                  (errout "Invalid throw tag: ~A" tag))
                 )))

     (define (+catch tag fn)
       (call/cc (lambda (+sk)
                  (push (cons tag +sk) *catches*)
                  ;; must exit through continuation
                  ;; to restore global state
                  (+sk (fn)))
                ))

     (define (+unwind-protect fn fnunw)
       (dynamic-wind
        do-nothing
        fn
        fnunw))

     (define (+try fn errfn)
       (catch ans
         (errfn (catch error
                  (throw ans (fn))
                  ))
         ))

     (define (amb-fail)
         (error "amb tree exhausted"))

     (define (+amb . fns)
       (let ((fns (scramble fns))
             (thrower (lambda ()
                        (throw amb-fail 'fail))))
         (loop
          (if fns
              (catch amb-fail
                (params ((amb-fail thrower))
                  (loop-finish ((pop fns)))
                  ))
            ;; else
            (amb-fail)))
         ))

     ))

