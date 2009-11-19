;;; https://www.spoj.pl/problems/PT07Z/
;;;
;;; TAGS: graph tree lowest-common-ancestor

(defun euler-path (vertext-count edges)
  "Construct Eulerian path in a connected graph.

   vertex-count: number of vertices
   edges: list of undirected edges"

  (let ((degree (make-array vertex-count
			    :element-type 'fixnum
			    :initial-element 0)))

    ;; Determine the degree of all nodes
    (dolist (e edges)
      (destructuring-bind (u v) e
	(incf (aref degree u))
	(incf (aref degree v))))

    ;; Choose vertex with odd degree, or any if no such exists.
    (let ((x (dotimes (i vertex-count)
		(when (oddp (aref degree i))
		  (return i)))))
      (when (null x) (setf x 0))
      
      ;; 
	   
      

    
    
(defun read-graph (n)
  (let ((adj (make-array n 
			 :element-type 'fixnum
			 :initial-element nil)))
    (dotimes (i (1- n))
      (let ((x (1- (read)))
	    (y (1- (read))))
	(push x (aref adj y))
	(push y (aref adj x))))))

(defmacro reduce/nil (f &rest args)
  (let ((gf (gensym))
	(ga (gensym)))
    `(let ((,gf ,f)
	   (,ga (list ,@args)))
       (if (some #'null ,ga) nil
	   (reduce ,gf ,ga)))))

(defmacro +/nil (&rest args)
  (reduce/nil #'+ args))

(defmacro max/nil (&rest args)
  (reduce/nil #'max args))

(defmacro min/nil (&rest args)
  (reduce/nil #'min args))
    
(defun bellman-ford-yen (nodes edges source)
  "Single source shortest path, for a graph without negative cycles."

  ;; Impose linear order on the vertices based on their number.

  ;; Partition the set of all edges into one of two subsets. The
  ;; edges where.

  ;; Initialize the graph
  (let ((dist (make-array nodes 
			  :element-type 'fixnum :initial-value nil))
	(pred (make-array nodes
			  :element-type :initial-value nil)))
    (setf (aref dist source) 0)

    ;; Relax edges repeatedly
    (dotimes (i nodes)
      (dolist (e edges)
	(destructuring-bind (u v) e
	  (when (> (aref dist v) (1+ (aref dist u)))
	    (setf (aref dist v) (1+ (aref dist u))
		  (aref pred v) u)))))

    ;; TODO: should check for negative cycles
	

    

		       


;; (defun seek (adj x y)
;;   (let* ((n (array-total-size adj))
;; 	 (visit (make-array n
;; 			    :element-type 'bit
;; 			    :initial-element 1))
	 
;;     (do ((todo (make-array n 
;; 			   :element-type 'fixnum
;; 			   :initial-element x
;; 			   :fill-pointer 1))
;; 	     (visit (make-array n
;; 				:element-type 'bit
;; 				:initial-element 0)))
;; 	    ((= 0 (fill-pointer todo))
;; 	     (every #'(lambda (x) (= x 1)) visit))
;; 	  (let ((x (vector-pop todo)))
;; 	    (setf (aref visit x) 1)
;; 	    (dolist (y (aref adj x))
;; 	      (when (= 0 (aref visit y))
;; 		(vector-push y todo))))))))


(defun main ()
  (let ((g (read-graph (read))))))

(defun blah ()
  (let ((n 10000))
    (let ((path (make-array (list n n)
			    :element-type 'fixnum)))
      (dotimes (k n)
	(dotimes (i n)
	  (dotimes (j n)
	    (setf (aref path i j) -1)))))
    (print (array-total-size path))))

(blah)
    


  
;(main)
