;; sdlisp-bc.asd
;; --------------------------------------------------------------------------------------
;; SD-Lisp -- Our own variant of Lisp
;;
;; Copyright (C) 2008-2010 by SpectroDynamics, LLC. All rights reserved.
;;
;; DM/SD  08/08
;; --------------------------------------------------------------------------------------

(asdf:defsystem "sdlisp-bc"
  :description "SD-Lisp: our own variant of Lisp (ByteCode version)"
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
   (:MODULE "ByteCode"
	    :COMPONENTS ((:file "lispm")
                         (:file "sdlisp-bc")
                         (:file "init"))
	    
	    :SERIAL T))
  :serial t
  :depends-on   ("useful-macros"))

