;;; =============================================
;;; https://www.spoj.pl/problems/BRTREE/
;;; 3762
;;; =============================================

;;; (f k n) is a generalized fibonacci number
;;; k -> is the number of previous terms to sum
;;; n -> the nth number in the series
;;;
;;; (f 0 n) = n
;;; (f 1 n) = n + (f 1 (n-1)) = 1/2 * n * (n+1)
;;; (f 2 n) = n + (f 2 (n-1)) + (f 2 (n-2)), i.e. the "normal" fibonacci
;;;
;;; Note:
;;; (f k n) = 0 for n<=0

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *limit* 1234567890))

(defun bin-search (f x a b)
  "Find (max m: a<=m<b and f(m)<=x : m)"
  (cond ((= a b) nil)
	((= 1 (- b a)) 
	 (if (<= (funcall f a) x) a nil))
	(t (let ((m (floor (/ (+ a b) 2))))
	     ;(format t "~d..~d..~d~%" a m b )
	     (if (<= (funcall f m) x)
		 (bin-search f x m b)
		 (bin-search f x a m))))))
	     
(defun make-limited-func (f)
  (let ((lim (bin-search f *limit* 0 (1+ *limit*))))
    #'(lambda (n) 
	(funcall f (min n (1+ lim))))))

;; TODO: optimize
(defun make-mapped-func (series)
  (let ((n (length series))
	(z (first (last series))))
    #'(lambda (x)
	(if (< x n) (nth x series) z))))

;; TODO: optimize
(eval-when (:compile-toplevel)
  (defun fib-series (k)
    (let ((series ())
	  (sum 0)
	  (front ())
	  (back (make-list k :initial-element 0)))
      (do*((n 0 (1+ n))
	   (y 0 (+ n sum)))
	  ((> y *limit*) (push y series))
	(push y series)
	(when (null back) 
	  (setf back (nreverse front))
	  (setf front nil))
	(decf sum (pop back))
	(incf sum y)
	(push y front))
      (nreverse series))))

(defmacro make-matrix ()
  (let ((lst ()))
    (do ((k 2 (1+ k)))
	((> k 30))
      (let ((f (fib-series k)))
	(push (make-array (length f) :initial-contents f) lst)))
    `,(make-array (length lst) :initial-contents (nreverse lst))))


	 
(defun test-fib-series ()
  (dotimes (i 30)
    (let* ((s (fib-series (+ 2 i)))
	   (n (length s)))
      (format t "fibs(~d)=~a~%" (+ 2 i) 
	      (and (> (nth (- n 1) s) *limit*)
		   (<= (nth (- n 2) s) *limit*))))))
	 
(defun make-fk (k)
  (cond ((= 0 k) (make-limited-func #'(lambda (n) n)))
	((= 1 k) (make-limited-func #'(lambda (n) (* 1/2 n (1+ n)))))
	(t (make-mapped-func (fib-series k)))))


(defun matrix-ref (matrix k n)
  (let* ((k (min k (1- (array-total-size matrix))))
	 (a (aref matrix k))
	 (n (min n (1- (array-total-size a)))))
    (aref a n)))

	    
(defun make-f ()
  (let ((f0 (make-limited-func #' (lambda (n) n)))
	(f1 (make-limited-func #' (lambda (n) (* 1/2 n (1+ n)))))
	(m (make-matrix)))
    #'(lambda (k n)
	(cond ((= k 0) (funcall f0 n))
	      ((= k 1) (funcall f1 n))
	      (t (matrix-ref m (- k 2) n))))))


(defun main ()
  (let ((f (make-f)))
    (do ((n (read) (read))
	 (k (read) (read)))
	((= n 0))
      (format t "~d~%" (funcall f k (1- n))))))

(main)