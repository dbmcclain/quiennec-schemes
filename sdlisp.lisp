
(in-package :sdlisp)

;; -------------------------------------------

;;;                      Threaded interpreter.
;;; Continuation are now implicit and call/cc is a magical operator.
;;; Also try to introduce combinators as much as possible.
;;; Closures are explicitely represented.

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Determine the nature of a variable.
;;; Three different answers. Or the variable is local (ie appears in R)
;;; then return     (LOCAL index . depth)
;;; global (ie created by the user) then return
;;;                 (GLOBAL . index)
;;; or predefined (and immutable) then return
;;;                 (PREDEFINED . index)

(defvar g.current (make-hash-table))
(defvar g.init    (make-hash-table))
(defvar g.lispfns (make-hash-table))
(defvar g.params  (maps:empty))

#|
(sets:view-set g.params :key #'maps:map-cell-key)
 |#

;;; This tag is used in the value cell of uninitialized variables.
(defconstant undefined-value (cons 'undefined 'value))

(defstruct sym
  (val undefined-value)
  name
  param ;; true if has dynamic bindings
  plist)

(defstruct (prim-sym
            (:include sym)) )

(defstruct closure
  code)

;;; --------------------------------------------------------

(defun arity (clos)
  (labels ((llen (fn)
             (let ((llst (lw:function-lambda-list fn nil)))
               (cond ((eq llst :none)  (values 0 nil))
                     ((consp llst)
                      (if (find-if (um:rcurry #'member lambda-list-keywords) llst)
                          (values '* llst)
                        (values (length llst) llst)))
                     ))))
    (cond ((closure-p clos)
           (llen (closure-code clos)))
          ((or (and (symbolp clos)
                    (fboundp clos))
               (functionp clos))
           (llen clos))
          (t (error "Not a function"))
          )))

;; -------------------------------------------------------------------------

(defun inherit-lisp-value (n is-fn)
  (if is-fn
      (unless (macro-function n)
        (when (fboundp n)
          (multiple-value-bind (arity args) (arity n)
            (g.init-initialize! n (symbol-function n)
                                (if (symbolp arity)
                                    arity
                                  args))
            )))
    ;; else
    (when (boundp n)
      (g.init-initialize! n (symbol-value n))
      (primitive? n))
    ))

(defun inherit-lisp-function (f)
  (or (gethash f g.lispfns)
      (let ((sym (gensym)))
        (setf (symbol-function sym) f
              (gethash f g.lispfns) sym)
        (inherit-lisp-value sym t)
        sym)
      ))

;; -------------------------------------------------------------------------

(defstruct local-sym
  name
  level
  offs)

(defun compute-kind (r n)
  (or (local-variable? r 0 n)
      (primitive? n)
      (global-variable? n)))

(defun local-variable? (r i n)
  (declare (fixnum i n))
  (when (consp r)
    (let ((pos (position n (car r))))
      (if pos
          (make-local-sym
           :name  n
           :level i
           :offs  pos)
        (local-variable? (cdr r) (1+ i) n)))
    ))

(defun global-variable? (n)
  (gethash n g.current))

(defun primitive? (n)
  (gethash n g.init))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo

(defun need-symbol (n)
  (unless (symbolp n)
    (error "Not a symbol: ~S" n)))

(defun cant-redefine-primitive (n)
  (when (primitive? n)
    (error "Can't redefine: ~S" n)))

;; ------------------------------------------------------------------

(defun g.param-extend! (n)
  (need-symbol n)
  (cant-redefine-primitive n)
  (let ((sym (make-sym
              :name n)))
    (setf g.params (maps:add g.params n sym))
    sym))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; User-defined global environment definition. This environment is
;;; initially completely empty and can be extended by the user.

;;; G.CURRENT represents the `static' user-defined global environment. 
;;; It is represented by the list of the symbols naming these global
;;; variables. Their values are held in the SG.CURRENT vector.

(defun g.current-extend! (n)
  (need-symbol n)
  (cant-redefine-primitive n)
  (cond ((global-variable? n))
        (t (setf (gethash n g.current) (make-sym
                                        :name n)))
        ))

;;;oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Predefined global environment definition. This global environment
;;; is immutable. G.INIT represents the static predefined global
;;; environment and is represented by the list of the symbols naming
;;; these global variables. Their values are held in the SG.INIT vector.

;;; Add that value is associated to name in the predefined global environment.

(defun g.init-initialize! (name value &optional (lambda-list nil lambda-list-p))
  (setf (gethash name g.init) (make-prim-sym
                               :name  name
                               :val   value
                               :plist (when lambda-list-p
                                        `(lambda-list ,lambda-list)))))

(defun get-description (prim-sym)
  (getf (sym-plist prim-sym) 'lambda-list :none))

;;; --------------------------------------------------------

;;; --------------------------------------------------------

(defmethod print-result ((sym sym) stream)
  (format stream "~A" (sym-name sym)))

(defmethod print-result (val stream)
  (format stream "~S" val))

(defmethod print-result ((clos closure) stream)
  (format stream "#<Closure ~x>" (sys:object-address clos)))

;; ---------------------------------------------------------
;; Allow us to accumulate lists without needing to later reverse them.
;; NULL q = empty. Contents = (CAR q)

(defun addq (q item)
  #F
  (let ((cell (list item)))
    (cond (q
           (setf (cdr (the cons q))
                 (setf (cdr (the cons (cdr (the cons q)))) cell))
           q)
          (t
           (cons cell cell))
          )))

;; ---------------------------------------------------------

