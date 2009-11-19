;; modular power
(defun mod-pow (base exponent modulo)
  (do ((e exponent)
       (result 1))
      ((= e 0) result)
    (when (= 1 (logand e 1))
      (setf result (mod (* result base) modulo)))
    (setf e (ash e -1)
	  base (mod (* base base) modulo))))
  

(defun miller-rabin-test (n k)
  "Test for compositeness"
  ;; get factors 2 from a 
  (let ((d (1- n)) (s 0))
    (do ()
	((= 1 (logand d 1)))
      (incf s)
      (setf d (ash d -1)))
    (let ((composite nil))
      (dotimes (i k)
	(let* ((a (+ 2 (random (- n 3))))
	       (x (mod-pow a d n)))
	  (dotimes (r s)
	    (format t "a=~d x=~d i=~d ~a~%" a x i composite)
	    (cond ((= x 1) 
		   (setf composite t)
		   (return))
		  ((= x (1- n))
		   (return)))
	    (setf x (mod (* x x) n)))
	(when composite (return))))
      (not composite))))
	  
(print (miller-rabin-test 5 10))
	 
      