;; From K. Pittman - Input from interactive streams with editing

(defvar *input-seen-stack* '())
(defvar *input-waiting-queue* '())
(defvar *rubout-handling* nil)

(defun invoke-rubout-handler (parser-function)
    (let ((*input-seen-stack* '())
	  (*input-waiting-queue* '())
	  (*rubout-handling* t))
      (loop (catch 'edit-character-typed
		     (return-from invoke-rubout-handler
		(funcall parser-function)))
	    (handle-editing)
	    (setq *input-waiting-queue* (reverse *input-seen-stack*)))))

(defvar *delete-character-char* #\%)
(defvar *delete-line-char* #\@)

(defun undisplay-char (char) (format t "[~C]" char))

(defun handle-editing ()
    (loop (let ((char (read-char)))
	    (cond ((eql char *delete-character-char*)
		   (when *input-seen-stack*
		     (undisplay-char (pop *input-seen-stack*))))
		  ((eql char *delete-line-char*)
		   (mapc #'undisplay-char *input-seen-stack*)
		   (setq *input-seen-stack* nil))
		  (t ;; Must be done editing.
		   (unread-char char)
		   (return))))))

  ;;; Normally, this would be a different method on READ-CHAR, but we're just
  ;;; indirecting through a different function name for presentational simplicity.
(defun read-a-char ()
    (if *rubout-handling*
	(if *input-waiting-queue*
	   (pop *input-waiting-queue*)
	   (let ((c (peek-char nil)))
	     (cond ((or (eql c *delete-character-char*)
			(eql c *delete-line-char*))
		    (throw 'edit-character-typed nil))
		   (t
		    (read-char) ;for effect
		    (push c *input-seen-stack*)
		    c))))
	(read-char)))

(defun unread-a-char (char)
    (if *rubout-handling*
	(push char *input-waiting-queue*)
	(unread-char char))
    char)

;; --------------------------------------------------------------------

(defun ambitious-read-eval-print ()
  (loop (write-char #\newline)
        (print (invoke-rubout-handler #'eval-while-reading))))

(defvar *impatient* t)

(defvar *paren* (gensym "CLOSE-PAREN"))

(defun peek-char-gobbling-whitespace ()
  (do ((char (read-a-char) (read-a-char)))
      ((not (member char '(#\Space #\Tab #\Return)))
       (unread-a-char char) char)))

(defun read-form-or-close-paren ()
  (if (eql (peek-char-gobbling-whitespace) #\)) 
      (progn (read-a-char) *paren*)
    (read-literal)))

(defun read-evaled-form-or-close-paren ()
  (if (eql (peek-char-gobbling-whitespace) #\))
      (progn (read-a-char) *paren*)
    (eval-while-reading)))

(defun eval-while-reading ()
  (case (peek-char-gobbling-whitespace)
    (#\) (xparse-error "Close paren seen where not expected."))
    (#\; (read-line) (eval-while-reading))
    (#\' (read-a-char) (read-literal))
    (#\( (read-a-char)
         (if (eql (peek-char-gobbling-whitespace) #\)) '()
           (let ((fn (read-literal)))
             (case fn
               ((cond)    (eval-cond-while-reading))
               ((quote)   (eval-quote-while-reading))
               ((progn)   (eval-progn-while-reading))
               ((setq)    (eval-setq-while-reading))
               ((defun)   (eval-defun-while-reading))
               (otherwise (eval-function-while-reading fn))))))
    (otherwise (eval (read-literal)))))

(defun eval-setq-while-reading ()
  (do ((rv nil) (var (read-form-or-close-paren) (read-form-or-close-paren)))
      ((eql var *paren*) rv)
    (let ((value (read-evaled-form-or-close-paren)))
      (cond ((eql value *paren*) (xparse-error "Wrong number of args to SETQ"))
            (t (setq rv (set var value)))))))

(defun eval-function-while-reading (fn)
  (do ((form (read-evaled-form-or-close-paren) (read-evaled-form-or-close-paren))
       (l nil (cons form l)))
      ((eql form *paren*)
       (apply fn (nreverse l)))))

(defun eval-quote-while-reading ()
  (prog1 (read-literal)
    (cond ((eql (peek-char-gobbling-whitespace) #\)) (read-a-char))
          (t (xparse-error "Wrong number of args to QUOTE")))))

(defun eval-cond-while-reading ()
  (prog1 (try-cond-clauses-while-reading)
    (ignore-rest-of-list "You already passed the succeeding COND branch.
  Save yourself some work and just close off the COND, huh?")))

(defun try-cond-clauses-while-reading ()
  (do ((c (peek-char-gobbling-whitespace) (peek-char-gobbling-whitespace)))
      ((eql c #\)))
    (cond ((eql c #\()
           (read-a-char)
           (multiple-value-bind (win? value)
               (try-cond-clause-while-reading)
             (if win? (return value))))
          (t (xparse-error "Ill-formed COND clause.")))))

(defun ignore-rest-of-list (&optional (msg "Just close off the list.
  I'm ignoring stuff from here on at this level anyway."))
  (cond (*impatient*
         (let ((c (peek-char-gobbling-whitespace)))
           (cond ((eql c #\)) (read-a-char) nil)
                 (t (xparse-error msg)))))
        (t
         (do ((form (read-form-or-close-paren) (read-form-or-close-paren)))
             ((eql form *paren*))))))

(defun eval-progn-while-reading (&optional (seed nil))
  (do ((val seed form)
       (form (read-evaled-form-or-close-paren)
             (read-evaled-form-or-close-paren)))
      ((eql form *paren*) val)))

(defun try-cond-clause-while-reading ()
  (let ((test (eval-while-reading)))
    (cond ((not test) (beep :low)
           (ignore-rest-of-list "This COND branch failed.
  Don't bother typing the body of the failing clause, huh? It's boring.")
           (values nil nil))
          (t (beep :high) (values test (eval-progn-while-reading test))))))

(defun eval-defun-while-reading ()
  (do ((form (read-form-or-close-paren) (read-form-or-close-paren))
       (l '() (cons form l)))
      ((eql form *paren*) ;DEFUN's body can't run until args available in a call
       (eval `(defun ,@(nreverse l))))))

(defun beep (kind) kind)  ;Replace with something useful if you like.

(defun xparse-error (format-string &rest format-arguments)
  (apply #'error format-string format-arguments))

(defun read-literal ()  ;Doesn't do whole job, but enough for this sample.
  (values (read-from-string
           (with-output-to-string (s)
             (do ((c (read-a-char) (read-a-char)))
                 ((and (not (alphanumericp c)) (not (find c ".+-*/=><")))
                  (unread-a-char c))
               (write-char c s))))))
