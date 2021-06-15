
(in-package :sdlisp-cps)

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
     (if (eq? 'nil (cdr f)) (k 2))
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


