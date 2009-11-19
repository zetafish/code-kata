;; https://www.spoj.pl/problems/DQUERY/
;;
;; HINTS:
;;   http://en.wikipedia.org/wiki/Segment_tree

(defun store-segments (lst)
  (let ((end-points (sort (mapcar #'(lambda (x) (car x)) lst) #'<)))
    (print end-points)))


(defun test ()
  (store-segments '((1 . 2) (3 . 4) (7 . 100) (4 . 6)))
)


(defun main ()
  (let* ((n (read))
	 (a (make-array n)))
    (dotimes (i n)
      (setf (aref a i) (read)))
    (dotimes (i (read))
      (princ (dquery a (read) (read)))
      (princ #\Newline))))

(defparameter *limit* 1000000)

(defun dquery (a p q)
  "#unique in a[p:q]"
  (let* ((b (make-array *limit*
			:element-type 'bit
			:initial-element 0)))
    (do ((c 0)
	 (i (1- p) (1+ i)))
	((= i q) c)
      (let ((v (aref a i)))
	(when (= 0 (bit b v))
	  (incf c)
	  (setf (bit b v) 1))))))

(defun test1 ()
  (let ((a #(1 1 2 1 3)))
    (dolist (x '((1 5)
		 (2 4)
		 (3 5)))
      (princ (apply #'dquery a x))
      (princ #\Newline))))

(main)