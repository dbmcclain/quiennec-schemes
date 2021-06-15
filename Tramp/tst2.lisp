
(define (identity x)
    x)

(define (constantly x)
    (lambda args
      x))

(define (true . args)
    (lambda args
      t))

(define (false . args)
    (lambda args
      nil))

(define y
    (lambda (h)
      ((lambda (x) (x x))
       (lambda (g)
         (h (lambda args
              (apply (g g) args)))))))

(define fac
    (Y
     (lambda (f)
       (lambda (x)
         (if (< x 2)
             1
           (* x (f (- x 1))))))))

(define (curry f . pre-args)
    (lambda post-args
      (apply f (append pre-args post-args))))

(define (rcurry f . post-args)
    (lambda pre-args
      (apply f (append pre-args post-args))))

