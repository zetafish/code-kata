;;; https://www.spoj.pl/problems/PERMUT2/


;; ambi:     0 3 2 1
;; non-ambi: 1 2 3 4 0
(defun ambigu (perm)
  (let ((n (length perm)))
    (do ((i 0 (1+ i)))
	((or (= i n)
	     (/= i (aref perm (aref perm i))))
	 (= i n)))))
     

(defun main ()
  (loop
       (let ((n (read)))
	 (when (= n 0) (return))
	 (let ((p (make-array (list n))))
	   (dotimes (i n)
	     (setf (aref p i) (1- (read))))
	   (when (not (ambigu p))
	     (princ "not "))
	   (princ "ambiguous")
	   
	   (terpri)))))


(main)