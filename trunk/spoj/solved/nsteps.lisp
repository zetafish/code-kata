;; https://www.spoj.pl/problems/NSTEPS

(defun get-number (x y)
  (cond ((= x y)
	 (if (evenp x) 
	     (* 2 x)
	     (1- (* 2 x))))
	((= x (+ y 2))
	 (if (evenp x)
	     (+ 2 (* 2 y))
	     (+ 1 (* 2 y))))
	(t "No Number")))

(defun main ()
  (dotimes (i (read))
    (format t "~a~%" (get-number (read) (read)))))


(main)
	 
	 
