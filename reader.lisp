
(in-package :sdl.stream-reader)

;; -------------------------------------------
;; Stream reader

(defstruct reader
  buf pos)

(defparameter *reader* (make-reader))

(defun flush-reader-buf ()
  (setf (reader-buf *reader*) nil
        (reader-pos *reader*) 0))

#|
(defun reader-next (&optional (stream *standard-input*))
  (if (interactive-stream-p stream)
      (with-accessors ((reader-buf reader-buf)
                       (reader-pos reader-pos)) *reader*
        (tagbody
         top
         (when (or (null reader-buf)
                   (>= reader-pos (length reader-buf)))
           (format t "~&>> ")
           (setf reader-buf (read-line stream nil)
                 reader-pos 0))

         again
         (handler-case
             (multiple-value-bind (ans end)
                 (read-from-string reader-buf nil #'reader-next
                                   :start reader-pos)
               (setf reader-pos end)
               (return-from reader-next ans))
           
           (error (exn)
             (typecase exn
               ((or end-of-file)
                (format t "~&>>> ~A" reader-buf)
                (setf reader-buf (concatenate 'string
                                              reader-buf 
                                            (read-line stream nil)))
                (go again))
               (t
                (format t "~&ERROR: ~A"
                        (um:format-error exn))
                (format t "~&>>> ~A" (subseq reader-buf 0 reader-pos))
                (setf reader-buf (concatenate 'string
                                              (subseq reader-buf 0 reader-pos)
                                              (read-line stream nil)))
                (go again))
               ))
           )))
    ;; else
    (read stream nil stream)
    ))
|#
(defun reader-next (&optional (stream *standard-input*))
  (when (interactive-stream-p stream)
    (format t "~&>> "))
  (read stream nil stream))
