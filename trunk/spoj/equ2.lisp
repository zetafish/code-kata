;; https://www.spoj.pl/problems/EQU2/
;;

(defun main ()
  (dotimes (i (read))
    (destructuring-bind (x y)
	(solve (read))
      (format t "~a ~a~%" x y))))

(defun make-f (n)
  #'(lambda (x y)
      (- (* x x) (* n y y))))

(defun search (f m x0 y0 x1 y1)
  "pre: (f x0 y0) <= m < (f x1 y1)"
  (cond ((= m (funcall f x0 y0)) (list x0 y0))
	(t (let* ((x (/ (- x1 x0) 2))
		  (y (/ (- y1 y0) 2))
		  (val (funcall f x y)))
	     (cond ((= m val) (list x y))
		   ((< m val) 
	     
  (if (and (= x0 x1) (= y0 y1))(list x0 y0)
      (let ((x (
	((
	
  
  


(defun solve (n)
  (do ((x 1)
       (y 1))
      ((= 1 (- (* x x) (* n y y))) 
       (list x y))
    (let ((v (- (* x x) (* n y y))))
      (format t "~a ~a~%" x y)
      (cond ((= v 1) (return (list x y)))
	    ((< v 1) (incf x)) ;; or dec y??
	    ((> v 1) ; 
	     (setf x 1) 
	     (incf y))))))
	     
	     
(solve 9)  
       
       
