;;; Graph Algorithms

(defun +/nil (&rest args)
  (if (some #'null args) nil
      (reduce #'+ args)))

(defun -/nil (&rest args)
  (if (some #'null args) nil
      (reduce #'- args)))

(defun max/nil (&rest args)
  (let ((lst (remove-if #'null args)))
    (if (null lst) nil
	(reduce #'max lst))))

(defun min/nil (&rest args)
  (let ((lst (remove-if #'null args)))
    (if (null lst) nil
	(reduce #'min lst))))





;; Floyd-Warshall algorithm
;;
;; Purpose: 
;;   Find all pairs shortest path in a weighted directed graph
;; 
;; Complexity:
;;   O(V^3) with V the number of vertices
;;   O(V^2) comparisons needed
;;
;; Tags:? 
;;   graph dynamic-programming shortest-path all-pair-shortest-path
(defun floyd-warshall (vertex-count edge-cost)
  "Compute all pair shortest path."
  (let ((path (make-array (list vertex-count vertex-count)
			  :initial-element nil)))
    ;; Initialize path matrix
    (dolist (e edge-cost)
      (destructuring-bind (u v w) e
	(setf (aref path u v) w)))
    (dotimes (i vertex-count)
      (setf (aref path i i) 0))

    ;; Perform the algorithm
    (dotimes (k vertex-count)
      (dotimes (i vertex-count)
	(dotimes (j vertex-count)
	  (setf (aref path i j)
		(min/nil (aref path i j)
			 (+/nil (aref path i k)
				(aref path k j)))))))

    path))

(defun kruskal (vertex-count 
  "Find minimum spanning tree usign Kruskal's algorithm."
  
	  

(defun test-floyd-warshall ()
  (print (floyd-warshall 4 '((0 1 1)
			     (0 2 1)
			     (0 3 10)
			     (1 3 20)
			     (2 3 1)))))

			  

  