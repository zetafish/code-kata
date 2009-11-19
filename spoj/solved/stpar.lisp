;; https://www.spoj.pl/problems/STPAR/


(defun solve (trucks)
  (let ((passed (list 0))
	(trucks (reverse trucks))
	(side nil))
    (loop 
;       (format t "~a ~a ~a~%" trucks side passed)
       (cond ((and trucks (= (first passed) (1- (first trucks))))
	      (push (pop trucks) passed))
	     ((and side (= (first passed) (1- (first side))))
	      (push (pop side) passed))
	     (trucks
	      (push (pop trucks) side))
	     (t (return))))
    (and (null trucks)
	 (null side))))
		 

(defun main ()
  (do ((n (read) (read)))
      ((= n 0))
    (let ((trucks nil))
      (dotimes (i n)
	(push (read) trucks))
      (format t "~a~%" (if (solve trucks) "yes" "no")))))

(main)