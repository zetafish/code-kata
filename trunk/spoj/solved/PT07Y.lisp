; https://www.spoj.pl/problems/PT07Y/
;
; NOTE: 
;   DFS algorithm uses a vector to implement the stack/queue
;   Using a list would be too slow
					

;; (defun read-graph (n m)
;;   (let ((graph (make-array n 
;; 			   :element-type 'fixnum 
;; 			   :initial-element nil)))
;;     (dotimes (i m)
;;       (let ((x (1- (read)))
;; 	    (y (1- (read))))
;; 	(push x (aref graph y))
;; 	(push y (aref graph x))))
;;     graph))

(defun read-graph (n m)
  (if (/= n (1+ m)) 
      nil
      (let ((graph (make-array n
			       :element-type 'fixnum
			       :initial-element nil))
	    (covered (make-array n
				 :element-type 'bit
				 :initial-element 0)))
	(dotimes (i m)
	  (let ((x (1- (read)))
		(y (1- (read))))
	    (when (= x y) (return nil))
	    (push x (aref graph y))
	    (push y (aref graph x))
	    (setf (aref covered x) 1)
	    (setf (aref covered y) 1)))
	(when (every #'(lambda (x) (= 1 x)) covered)
	  graph))))


(defun dfs (adj)
  (let ((n (array-total-size adj)))
    (if (= 0 n) t
	(do ((todo (make-array n 
			       :element-type 'fixnum
			       :initial-element 0
			       :fill-pointer 1))
	     (visit (make-array n
				:element-type 'bit
				:initial-element 0)))
	    ((= 0 (fill-pointer todo))
	     (every #'(lambda (x) (= x 1)) visit))
	  (let ((x (vector-pop todo)))
	    (setf (aref visit x) 1)
	    (dolist (y (aref adj x))
	      (when (= 0 (aref visit y))
		(vector-push y todo))))))))


(defun main ()
  (let* ((n (read))
	 (m (read))
	 (g (read-graph n m)))
     (format t "~a~%"
 	    (if (and g (dfs g))
 		"YES"
 		"NO"))))

  
(main)
