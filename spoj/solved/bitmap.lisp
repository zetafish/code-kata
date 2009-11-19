;;; https://www.spoj.pl/problems/BITMAP/


(defun solve (bitmap)
  (destructuring-bind (rows cols) (array-dimensions bitmap)
    (let ((dist (make-array (list rows cols)
			    :initial-element nil))
	  (roots nil))
      

      ;; Initialize the roots
      (dotimes (a rows)
	(dotimes (b cols)
	  (when (= 1 (aref bitmap a b))
	    (push (cons a b) roots)
	    (setf (aref dist a b) 0))))

      ;; Flood fill
      (do ((lst roots)
	   (d 0 (1+ d)))
	  ((null lst))
	(let ((next nil))
	  (flet ((examine (a b)
		   (when (and (<= 0 a) (< a rows)
			      (<= 0 b) (< b cols)
			      (null (aref dist a b)))
		     (setf (aref dist a b) (1+ d))
		     (push (cons a b) next))))
	    (dolist (elem lst)
	      (destructuring-bind (a . b) elem
		(examine (1- a) b)
		(examine a (1- b))
		(examine (1+ a) b)
		(examine a (1+ b)))))
	  (setf lst next)))
      dist)))

(defun read-grid (n m)
  (let ((grid (make-array (list n m)
			  :element-type 'bit)))
    (dotimes (i n)
      (let ((s (read-line)))
	(let ((v (map 'vector 
		      #'(lambda (c) (- (char-code c) (char-code #\0)))
		      s)))
	  (dotimes (j m)
	    (setf (aref grid i j) (svref v j))))))
    grid))

(defun digits (n)
  (do ((n n)
       (s nil))
      ((= 0 n) (mapcar #'digit-char (if (null s) '(0) s)))
    (multiple-value-bind (q r) (floor n 10)
      (setf n q s (push r s)))))

(defun format-grid (g)
  (destructuring-bind (rows cols) (array-dimensions g)
    (let ((s nil))
      (dotimes (row rows)
	(dotimes (col cols)
	  (when (> col 0) (setf s (push #\Space s)))
	  (setf s (nconc (nreverse (digits (aref g row col))) s)))
	(setf s (push #\Newline s)))
      (map 'string #'identity (nreverse s)))))

(defun main ()
  (dotimes (i (read))
    (let ((rows (read))
	  (cols (read)))
      (let ((grid (read-grid rows cols)))
	(let ((s (solve grid)))
	  (princ (format-grid s)))))))
		    

(defun test ()
  (with-input-from-string (s "
2
3 4
0001
0011
0110
3 4
0001
0011
0110
")
    (let ((*standard-input* s))
      (main))))


(main)