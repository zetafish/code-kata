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

(defun divides (a d)
  "Does d divides a?"
  (multiple-value-bind (q r) 
      (floor a d)
    (= 0 r)))


(let ((primes (defprimes)))
  (defun factorize (n)
    "Factorize number n into prime factors"
    (let ((f nil))
      (do ((n n)
	   (primes primes))
	  ((= 1 n) f)
	(let ((p (first primes)))
	  (cond ((divides n p) 
		 (push p f)
		 (setf n (/ n p)))
		(t
		 (setf primes (rest primes))))))
      f)))

(defun run-length-encode (lst)
  "Convert to run-length encoded list. Consequetive duplicates are encoded as (N E)"
  (do ((encoded nil)
       (lst lst (rest lst)))
      ((null lst) (reverse encoded))
    (let ((x (first lst)))
      (if (and encoded (= x (second (first encoded))))
	  (incf (first (first encoded)))
	(push (list 1 x) encoded)))))

(defun divisors (n)
  (let ((p (run-length-encode (factorize n))))
    (do ((dd nil
    (print p)))


