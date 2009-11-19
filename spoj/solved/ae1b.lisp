;;; https://www.spoj.pl/problems/AE1B/

(defun main ()
  (let ((n (read)) ; number of boxes
	(k (read)) ; number of screws for a table
	(s (read)) ; number of tables to make
	(lst nil))
    (dotimes (i n)
      (push (read) lst))
    (do ((need (* k s))
	 (count 0)
	 (lst (sort lst #'>) (rest lst)))
	((<= need 0) (format t "~a~%" count))
      (decf need (first lst))
      (incf count))))

(main)
    
