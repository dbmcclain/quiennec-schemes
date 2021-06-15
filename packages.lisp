
(in-package :cl-user)

(defpackage :sdl.stream-reader
  (:use #:common-lisp)
  (:export
   #:*reader*
   #:make-reader
   #:reader-buf
   #:reader-pos
   #:flush-reader-buf
   #:reader-next
   ))

(defpackage :sdlisp
  (:use #:common-lisp)
  (:export
   #:inherit-lisp-value
   #:inherit-lisp-function
   #:compute-kind
   #:local-sym
   #:local-sym-name
   #:local-sym-level
   #:local-sym-offs
   #:local-sym-p
   #:make-prim-sym
   #:make-sym
   #:copy-sym
   #:arity
   
   #:local-variable?
   #:global-variable?
   #:primitive?
   #:parameter?
   
   #:g.current
   #:g.init
   #:g.lispfns
   #:g.params
   
   #:g.init-initialize!
   #:g.current-extend!
   #:get-description
   #:g.param-extend!
   #:extend-params
   
   #:sym
   #:sym-p
   #:sym-val
   #:sym-param
   #:sym-name
   #:sym-plist
   #:prim-sym
   #:undefined-value

   #:closure
   #:make-closure
   #:closure-code
   #:closure-p

   #:print-result
   
   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:else
   #:=>

   #:*debuggable*
   #:addq
   ))

(defpackage :sdlisp-compiler
  (:use #:common-lisp #:sdlisp)
  (:export
   #:meaning
   #:cse
   #:defcse
   #:*toplevel*
   
   #:scheme-macro?
   #:scheme-macro-expand

   #:SHALLOW-ARGUMENT-REF
   #:DEEP-ARGUMENT-REF
   #:SHALLOW-ARGUMENT-SET!
   #:DEEP-ARGUMENT-SET!
   #:CHECKED-GLOBAL-REF
   #:PARAM-REF
   #:PREDEFINED
   #:GLOBAL-SET!
   #:PARAM-SET!
   #:CONSTANT
   #:ALTERNATIVE
   #:CLAUSE-SEQUENCE
   #:TR-FIX-LET
   #:FIX-LET
   #:CALL0
   #:CALL1
   #:CALL2
   #:CALL3
   #:CALLN
   #:THUNK-CLOSURE
   #:FIX-CLOSURE
   #:NARY-CLOSURE
   #:TR-REGULAR-CALL
   #:REGULAR-CALL
   #:STORE-ARGUMENT
   #:CONS-ARGUMENT
   #:ALLOCATE-FRAME
   ))

(defpackage :sdlisp-disassembler
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-compiler)
  (:export
   #:dis))

(defpackage :sdlisp-repl
  (:use #:common-lisp #:sdlisp #:sdlisp-compiler #:sdl.stream-reader)
  (:export
   #:with-sdlisp
   #:eval-sdlisp
   #:dbg
   #:repl
   #:file-loader
   ))

(defpackage :sdlisp-macros
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-compiler
   #:sdlisp-repl)
  (:export
   #:def-scheme-macro
   #:scheme-macro?
   
   #:define
   #:params
   #:delay
   #:internal-make-delay
   #:force
   #:letrec
   #:amb

   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

(defpackage :sdlisp-dtc
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-repl
   #:sdlisp-compiler
   #:sdl.stream-reader
   #:sdlisp-disassembler
   #:sdlisp-macros)
  (:export
   #:sdlisp
   #:load-file
   #:init-sdlisp
   #:with-sdlisp
   #:dis

   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:delay
   #:force
   #:letrec
   #:amb
   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

(defpackage :sdlisp-forth
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-repl
   #:sdlisp-compiler
   #:sdl.stream-reader
   #:sdlisp-disassembler
   #:sdlisp-macros)
  (:export
   #:sdlisp
   #:load-file
   #:init-sdlisp
   #:with-sdlisp
   #:dis

   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:delay
   #:force
   #:letrec
   #:amb
   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

(defpackage :sdlisp-cgen
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-repl
   #:sdlisp-compiler
   #:sdl.stream-reader
   #:sdlisp-disassembler
   #:sdlisp-macros)
  (:export
   #:sdlisp
   #:load-file
   #:init-sdlisp
   #:with-sdlisp
   #:dis

   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:delay
   #:force
   #:letrec
   #:amb
   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

(defpackage :sdlisp-cps
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-repl
   #:sdlisp-compiler
   #:sdl.stream-reader
   #:sdlisp-disassembler
   #:sdlisp-macros)
  (:export
   #:sdlisp
   #:load-file
   #:init-sdlisp
   #:with-sdlisp
   #:dis

   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:delay
   #:force
   #:letrec
   #:amb
   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

(defpackage :sdlisp-bc
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-repl
   #:sdlisp-compiler
   #:sdl.stream-reader
   #:sdlisp-disassembler
   #:sdlisp-macros)
  (:export
   #:sdlisp
   #:load-file
   #:init-sdlisp
   #:with-sdlisp
   #:dis

   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:delay
   #:force
   #:letrec
   #:amb
   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

(defpackage :sdlisp-tramp
  (:use #:common-lisp
   #:sdlisp
   #:sdlisp-repl
   #:sdlisp-compiler
   #:sdl.stream-reader
   #:sdlisp-disassembler
   #:sdlisp-macros)
  (:export
   #:sdlisp
   #:load-file
   #:init-sdlisp
   #:with-sdlisp
   #:dis

   #:BEGIN
   #:SET!
   #:LISP
   #:bye
   #:it
   #:itt
   #:ittt
   #:define
   #:delay
   #:force
   #:letrec
   #:amb
   #:set-vector
   #:set-string
   #:time-sdlisp
   ))

