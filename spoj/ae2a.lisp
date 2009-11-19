;;; https://www.spoj.pl/problems/AE2A/

(defparameter *hash* (make-hash-table :test #'equal))

(defun branches (n k)
  (mapcar #'(lambda (p) (list (- n 1) (- k p)))
	  '(1 2 3 4 5 6)))

(defun prob (n k)
  (if (= 1 n)
      (cond ((< k 1) (list 0 6))
	    ((> k 6) (list 0 6))
	    (t (list 1 5)))
      (multiple-value-bind (val found) 
	  (gethash (list n k) *hash*)
	(if found
	    val
	    (setf (gethash (list n k) *hash*)
		  (reduce #'(lambda (x y) (mapcar #'+ x y))
			  (mapcar #'(lambda (p)
				      (destructuring-bind (n k) p
					(prob n k)))
				  (branches n k))))))))

  
