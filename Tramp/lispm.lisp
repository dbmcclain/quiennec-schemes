;; -------------------------------------------
(in-package #:sdlisp-tramp)
;; -------------------------------------------

;; ------------------------------------------

(defstruct lispm
  env ans ktl)

(defvar *lispm* (make-lispm))

(defmacro with-lispm (lispm &body body)
  `(with-accessors ((%env lispm-env)
                    (%ans lispm-ans)
                    (%ktl lispm-ktl)) ,lispm
     (declare (ignorable %env %ans %ktl))
     ,@body))

(editor:setup-indent "with-lispm" 1)

(defmacro answer (arg)
  `(setf %ans ,arg))

(defmacro set-env (arg)
  `(setf %env ,arg))

(defmacro inject (&rest args)
  `(setf %ktl (list* ,@args (the list %ktl))))

(defmacro does (&body body)
  `(lambda ()
     (with-lispm *lispm*
       ,@body)))


(defmacro next ()
  `(funcall (pop (the cons %ktl))))

;; ------------------------------------------------------------

(define-condition tramp-error (error)
  ((err-msg  :reader err-msg  :initarg :msg))
  (:report report-tramp-error))

(defun report-tramp-error (err stream)
  (princ (err-msg err) stream))

(defun tramp-error (&rest args)
  (error (make-instance 'tramp-error
                        :msg (apply #'format nil args))))

(defun inject-error (cx)
  (with-lispm *lispm*
    (inject (meaning `(error ,(um:format-error cx)) nil t))))

(defun restart-trampoline (cx)
  (um:when-let (restart (find-restart 'restart-trampoline))
    (when (or (global-variable? 'error)
              (primitive? 'error))
      (inject-error cx)
      (invoke-restart restart))
    ))

(defun handle-trampoline-error (err)
  (cond (*debuggable*
         (error err))
        
        (t
         (restart-trampoline err) ;; doesn't return if we can handle it
         (error err))
        ))

;; -----------------------------------------------------------

(defun trampoline (env ans k)
  #f
  (let* ((lispm (make-lispm
                 :env env
                 :ans ans
                 :ktl (list k)))
         (*lispm* lispm))
    (declare (dynamic-extent lispm)
             (lispm lispm))
    (with-lispm lispm
      (handler-bind ((tramp-error #'handle-trampoline-error))
        (loop
         (restart-case
             (do ()
                 ((null (the list %ktl))
                  (return-from trampoline %ans))
               (next))
           (restart-trampoline ()
             nil)))
        ))))

;; ------------------------------------------

