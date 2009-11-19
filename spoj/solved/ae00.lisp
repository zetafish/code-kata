;;; https://www.spoj.pl/problems/AE00/

(defun count-rectangles (lim)
  (let ((c 0))
    (do ((y 1 (1+ y)))
	((> y lim))
      (do ((p y (+ p y))
	   (x 1 (1+ x)))
	  ((or (> p lim) (> x y)))
	(incf c)))
    c))

(defun main ()
  (format t "~a~%" (count-rectangles (read))))

(main)




		 
	
		