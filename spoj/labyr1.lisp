;; https://www.spoj.pl/problems/LABYR1/
;;
;; Tags: graph floyd-warshall tree

(defstruct (graph (:constructor size))
  (size)
  (edges))

(defun floyd-warshall (n edge-cost)
  "Find all pairs shortes path in weighted, directed graph."
  (let ((path (make-array (list n n)
			  :initial-element nil)))

    ;; Initialize the path matrix
    (dotimes (i n)
	(dotimes (j n)
	    (setf (aref path i j)
		  (if (= i j) 0
		      (aref edge-cost i j)))))

    ;; Perform Floyd-Warshall
    (dotimes (k n)
	(dotimes (i n)
	    (dotimes (j n)
		(setf (aref path i j)
		      (let ((ij (aref path i j))
			    (ik (aref path i k))
			    (kj (aref path k j)))
			;; (min ij (+ ik kj))
			(cond ((null ij)
			       (cond ((null ik) nil)
				     ((null kj) nil)
				     (t (+ ik kj))))
			      ((null ik) ij)
			      ((null kj) ij)
			      (t (min ij (+ ik kj)))))))))
    path))

(defun test-fw ()
  (print (floyd-warshall 4 #2A((nil 1 nil nil)
			       (nil nil 1 nil)
			       (nil nil 1 2)
			       (4 6 6 7) ))))

				
