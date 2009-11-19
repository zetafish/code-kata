;; https://www.spoj.pl/problems/FASHION/

(defun solve (n a b)
  (let ((s 0)
	(a (sort a #'<))
	(b (sort b #'<)))
    (dotimes (i n)
      (incf s (* (aref a i)
		 (aref b i))))
    s))

(defun main ()
  (dotimes (i (read))
    (let* ((n (read))
	   (a (make-array n))
	   (b (make-array n)))
      (dotimes (j n)
	(setf (aref a j) (read)))
      (dotimes (j n)
	(setf (aref b j) (read)))
      (format t "~a~%" (solve n a b)))))

(main)
