;; https://www.spoj.pl/problems/PRIME1/

(eval-when (:compile-toplevel :execute)
  (defun sieve (n)
    "Find primes <= n with sieve"
    (do ((primes (list 2))
	 (marks (make-array n :initial-element nil)) 
	 (root (ceiling (sqrt n))))
	((> (first primes) root)
	 (do ((i (1+ (first primes)) (1+ i)))
	     ((>= i n))
	   (when (not (aref marks i))
	     (push i primes)))
	 (nreverse primes))
      (let ((p (first primes)))
	;; cross out multiples of last prime, start at p^2
	(do ((i (* p p) (+ i p)))
	    ((>= i n))
	  (setf (aref marks i) t))
	;; add next prime
	(do ((i (1+ p) (1+ i)))
	    ((not (aref marks i))
	     (push i primes)))))))
  
(defmacro defprimes ()
  `'(,@(sieve (ceiling (sqrt 1000000000)))))

(let ((primes (defprimes)))
  (defun prime-marks (a b)
    (let* ((n (+ b (- a) 1))
	   (marks (make-array n
			      :element-type 'bit
			      :initial-element 1))
	   (sqrt-b (floor (sqrt b))))
      (do ((primes primes (rest primes)))
	  ((or (null primes)
	       (> (first primes) sqrt-b)))
	(let ((p (first primes)))
	  (do ((i (let ((j (* p (ceiling a p))))
		    (+ (- a) j (if (= j p) p 0)))
		  (+ i p)))
	      ((>= i n))
	    (setf (bit marks i) 0))))
      marks)))

(defun primes-a-b (a b)
  (if (= 1 a) (primes-a-b 2 b)
      (let ((m (prime-marks a b))
	    (n (+ b (- a) 1)))
	(do ((i 0 (1+ i))
	     (result nil))
	    ((= i n) (nreverse result))
	  (when (= 1 (bit m i))
	    (push (+ a i) result))))))

(defun test (a b)
  (let ((x (remove-if #'(lambda (x) (< x a))
		      (sieve b)))
	(y (primes-a-b a b)))
    (print (equal x y))))

(defun main ()
  (dotimes (i (read))
    (let* ((a (max 2 (read)))
	   (b (read))
	   (n (+ b (- a) 1))
	   (m (prime-marks a b)))
      (do ((i 0 (1+ i)))
	  ((= i n))
	(when (= 1 (bit m i))
	  (princ (+ a i))
	  (princ #\Newline))))
    (princ #\Newline)))

      
(main)

(defun measure (f &rest args)
  (time
   (progn 
     (apply f args)
     nil)))