;;; ==================================================
;;; https://www.spoj.pl/problems/TWOSQRS/
;;;
;;; Tags: sieve eratosthenes primes
;;;
;;; N is sum of 2 squares
;;; IFF
;;; All prime factors of for 4m+3 have even exponent
;;; in prime factorization of N
;;;
;;; for input: 0<=n<=10^12
;;; ==================================================

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun sieve (n)
    "Find primes < n with sieve"
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
	     (push i primes))))))

  (defun make-bits (lst n)
    (let ((b (make-array n
			 :element-type 'bit
			 :initial-element 0)))
      (dolist (x lst)
	(setf (bit b x) 1))
      b)))

(defmacro defprimes (n)
  `'(,@(sieve n)))

(defmacro 4m+3 (n)
  `(= #b11 (logand ,n #b11)))


(let* ((primes (defprimes 1000000)))
  (defun two-squares-p (n)
    (let ((root (floor (sqrt n))))
      (do ((n n)
	   (failed nil)
	   (primes primes))
	  ((or failed (= n 1))
	   (if failed "No" "Yes"))
	(let ((d (first primes)))
	  (cond ((> d root)
		 (setf failed (4m+3 n)
		       n 1))
		((= 0 (mod n d))
		 (setf n (/ n d))
		 (when (4m+3 d)
		   (setf failed (> (mod n d) 0))
		   (setf n (/ n d))))
		(t (setf primes (rest primes)))))))))

(defun test1 ()
  (dolist (i '(1 2 7 14 49 9 17 76 2888 27))
    (format t "~a~%" (two-squares-p i))))
	   
(defun main ()
  (dotimes (i (read))
    (format t "~d~%" (two-squares-p (read)))))

(main)
