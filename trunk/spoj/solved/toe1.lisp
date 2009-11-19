;;; https://www.spoj.pl/problems/TOE1/
;;;

(defmacro lref (lst x y)
  `(nth ,x (nth ,y ,lst)))

(defun line-points (lst x y inc-x inc-y)
  (let ((points nil))
    (do ((n 0 (1+ n))
	 (x x (funcall inc-x x))
	 (y y (funcall inc-y y)))
	((= 3 n))
      (push (cons x y) points))
    (mapcar #'(lambda (p) (lref lst (first p) (rest p)))
	    points)))

(defun row (grid n)
  (line-points grid 0 n #'1+ #'identity))

(defun col (grid n)
  (line-points grid n 0 #'identity #'1+))

(defun d1 (grid)
  (line-points grid 0 0 #'1+ #'1+))

(defun d2 (grid)
  (line-points grid 2 0 #'1- #'1+))


(defun winner-p (lst)
  (and (every #'identity lst)
       (let ((s (apply #'+ lst)))
	 (or (= 0 s)
	     (= 3 s)))))

(defun finished-p (grid)
  (or (winner-p (row grid 0))
      (winner-p (row grid 1))
      (winner-p (row grid 2))
      (winner-p (col grid 0))
      (winner-p (col grid 1))
      (winner-p (col grid 2))
      (winner-p (d1 grid))
      (winner-p (d2 grid))))


(defun make-grid ()
  (copy-tree '((nil nil nil)
	       (nil nil nil)
	       (nil nil nil))))


(defun generate ()
  (let ((stack nil)
	(visit (make-hash-table :test #'equal))
	(root (make-grid)))
    (push (cons 0 root) stack)
    (setf (gethash root visit) t)
    (do ()
	((null stack))
      (destructuring-bind (n . grid) (pop stack)
	(unless (finished-p grid)
	  (dotimes (y 3)
	    (dotimes (x 3)
	      (when (null (lref grid x y))
		(let ((next-grid (copy-tree grid))
		      (next-n (- 1 n)))
		  (setf (lref next-grid x y) next-n)
		  (unless (gethash next-grid visit)
		    (setf (gethash next-grid visit) t)
		    (push (cons next-n next-grid) stack)))))))))
    visit))


(defun read-grid ()
  (let ((g (make-grid)))
    (dotimes (y 3)
      (let ((s (read-line)))
	(dotimes (x 3)
	  (setf (lref g x y)
		(case (aref s x)
		  (#\O 0)
		  (#\X 1)
		  (#. nil))))))
    g))

    

(defun print-grid (grid)
  (terpri)
  (dotimes (y 3)
    (dotimes (x 3)
      (format t "~5a"(lref  grid x y)))
    (terpri))
  (terpri))
    
(defun main ()
  (let ((valid (generate)))
    (dotimes (i (read))
      (when (> i 0) (read-line))
      (let ((g (read-grid)))
	(princ (if (gethash g valid)
		   "yes"
		   "no")))
	(terpri))))

(defun test ()
  (maphash #'(lambda (k v) (format t "~a ~a ~a~%" (sxhash k) k v))
	   (generate)))

(main)