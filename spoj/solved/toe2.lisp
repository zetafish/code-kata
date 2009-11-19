;;; https://www.spoj.pl/problems/TOE1/
;;;

;;(defmacro lref (lst x y)
;;  `(nth ,x (nth ,y ,lst)))

(defmacro lref (s x y)
  `(aref ,s (+ ,x (* 3 ,y))))

(defun line-points (lst x y inc-x inc-y)
  (let ((points nil))
    (do ((n 0 (1+ n))
	 (x x (funcall inc-x x))
	 (y y (funcall inc-y y)))
	((= 3 n))
      (push (cons x y) points))
    (map 'string 
	 #'(lambda (p) (lref lst (first p) (rest p)))
	 points)))

(defun row (grid n)
  (line-points grid 0 n #'1+ #'identity))

(defun col (grid n)
  (line-points grid n 0 #'identity #'1+))

(defun d1 (grid)
  (line-points grid 0 0 #'1+ #'1+))

(defun d2 (grid)
  (line-points grid 2 0 #'1- #'1+))


(defun winner-p (s)
  (or (every #'(lambda (x) (equal x #\O)) s)
      (every #'(lambda (x) (equal x #\X)) s)))
	     

(defun finished-p (s)
  (or (notany #'(lambda (x) (equal x #\.)) s)
      (winner-p (row s 0))
      (winner-p (row s 1))
      (winner-p (row s 2))
      (winner-p (col s 0))
      (winner-p (col s 1))
      (winner-p (col s 2))
      (winner-p (d1 s))
      (winner-p (d2 s))))


(defun make-grid ()
  (copy-tree "........."))


(defun generate ()
  (let ((stack nil)
	(final (make-hash-table :test #'equal))
	(visit (make-hash-table :test #'equal))
	(root (make-grid)))
    (push (cons #\X root) stack)
    (setf (gethash root visit) t)
    (do ()
	((null stack))
      (destructuring-bind (ch . grid) (pop stack)
	(if (finished-p grid)
	    (setf (gethash grid final) t)
	    (dotimes (y 3)
	      (dotimes (x 3)
		(when (eql #\. (lref grid x y))
		  (let ((next-grid (copy-seq grid))
			(next-ch (if (eql ch #\X) #\O #\X)))
		    (setf (lref next-grid x y) ch)
		    (unless (gethash next-grid visit)
		      (setf (gethash next-grid visit) t)
		      (push (cons next-ch next-grid) stack)))))))))
    final))


(defun read-grid ()
  (let ((g (make-grid)))
    (let ((s (read-line)))
      (dotimes (y 3)
	(dotimes (x 3)
	  (let ((n (+ x (* 3 y))))
	    (setf (lref g x y)
		  (case (aref s n)
		    (#\O 0)
		    (#\X 1)
		    (#. nil)))))))
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
    (do ((g (read-line) (read-line)))
	((string= g "end"))
      (princ (if (gethash g valid)
		 "valid"
		 "invalid"))
      (terpri))))

(defun test ()
  (maphash #'(lambda (k v) (format t "~a ~a~%" k v))
	   (generate)))

(main)