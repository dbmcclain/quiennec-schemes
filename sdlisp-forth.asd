;; sdlisp-forth.asd
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Our own variant of Lisp
;;
;; Copyright (C) 2008-2010 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(asdf:defsystem "sdlisp-forth"
  :description "SD-Lisp: our own variant of Lisp"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components
  ((:FILE "packages")
   (:FILE "sdlisp")
   (:FILE "compiler")
   (:FILE "dis")
   (:FILE "macros")
   (:FILE "reader")
   (:FILE "repl")
   (:MODULE "Forth"
	    :COMPONENTS ((:file "sdlisp-forth")
                         (:file "init"))
	    
	    :SERIAL T))
  :serial t
  :depends-on   ("useful-macros"))

