(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :element-type
                 (array-element-type array)
                 :displaced-to array)
     dims)))

(defun find-queen (arr p d)
  (destructuring-bind ((px py) (dx dy)) (list p d)
    (do ((x px (+ x dx))
         (y py (+ y dy)))
        ((not (array-in-bounds-p arr x y)))
      (when (= (aref arr x y) 1)
        (return t)))))

(defun queen-in-row (board row)
  (find-queen board (list row 0) '(0 1)))

(defun queen-in-col (board col)
  (find-queen board (list 0 col) '(1 0) ))

(defun queen-in-diags (board x y)
  (member t (mapcar (lambda (p)
                      (find-queen board (list x y) p))
                  '((1 1) (-1 -1) (1 -1) (-1 1)))))
  
(defun queen-in-range (board x y)
  (or (queen-in-row board x)
      (queen-in-col board y)
      (queen-in-diags board x y)))

(defun backtracking (pred explore node)
  (if (funcall pred node)
      (list node)
    (mapcan (lambda (n)
              (backtracking pred explore n))
            (funcall explore node))))

(defun count-queens (board)
  (loop for i below (array-total-size board)
        for box = (row-major-aref board i)
        count (> box 0)))

(defun solutionp (board)
  (and board (= 8 (count-queens board))))

(defun put-queens (board)
  (loop for i below 8
        when (not (queen-in-row board i))  
        return
        (loop for j below 8
              for b = (copy-array board)
              when (not (queen-in-range board i j))
               do (setf (aref b i j) 1)
               and collect b)))

(defvar board (make-array '(8 8) :initial-element 0))

(defun 8-queens ()
  (backtracking #'solutionp #'put-queens board))

(defun print-q (q)
  (let ((v (make-array 64
                       :displaced-to q)))
    (loop for rix from 0 below 64 by 8
          for eix = (+ rix 8)
          do
          (pprint (subseq v rix eix)))))

