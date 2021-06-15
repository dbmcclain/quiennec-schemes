
(in-package :sdlisp-disassembler)

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; The following code use 
;;; pp to pretty-print expressions,
;;; and eval for a local hack (should be made of macros instead).

(defvar combinator-names
  '( SHALLOW-ARGUMENT-REF
     PREDEFINED
     DEEP-ARGUMENT-REF
     SHALLOW-ARGUMENT-SET!
     DEEP-ARGUMENT-SET!
     CHECKED-GLOBAL-REF
     GLOBAL-SET!
     CONSTANT
     ALTERNATIVE
     CLAUSE-SEQUENCE
     TR-FIX-LET
     FIX-LET
     CALL0
     CALL1
     CALL2
     CALL3
     CALLN
     THUNK-CLOSURE
     FIX-CLOSURE
     NARY-CLOSURE
     TR-REGULAR-CALL
     REGULAR-CALL
     STORE-ARGUMENT
     CONS-ARGUMENT
     ALLOCATE-FRAME
     ))

(defparameter combinator-originals nil)

(defun install-regular-combinators ()
  (when combinator-originals
    (loop for old-value in combinator-originals
          for name in combinator-names
          do
          (setf (symbol-function name) old-value)
          )))

(defun install-disassembling-combinators ()
  (unless combinator-originals
    (setf combinator-originals (mapcar #'symbol-function combinator-names)))
  (dolist (name combinator-names)
    (setf (symbol-function name)
          (lambda (&rest args) (cons name args))
          )))

(defun dis (e)
  (install-disassembling-combinators)
  (unwind-protect
      (pprint (meaning e nil t))
    (install-regular-combinators)
    (terpri) ))

#|
(dis '(lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(dis '(lambda (n) (if (= n 0) 1 (* (fact (- n 1)) n))))
|#
