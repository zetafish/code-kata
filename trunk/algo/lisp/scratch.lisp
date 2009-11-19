;;; =============================================================
;;; Dijkstra's Algorithm for Shortest Path
;;;
;;; Dijkstra's algorithm solves the single-source shortest path
;;; for a graph with nonnegative edge path costs. It produces a
;;; shortest path tree.
;;;
;;; http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
;;; =============================================================

;; Setup infinity (may be too small)
(defconstant *INF* most-positive-fixnum)

(defclass vertex ()
  ((id :reader id
       :initarg :id)))

(defclass edge ()
  ((weight :reader weight
	   :initarg :weight
	   :initform 1)
   (u :initarg :u
      :reader u)
   (v :initarg :v
      :reader v)))

(defclass graph () 
  ((edges 
    :initform nil
    :accessor edges)
   (vertices
    :initform nil
    :accessor vertices)))


  
    

(defun dijkstra (graph source)
  "Computes the shortest path tree from given root."
  (let ((dist (make-array (vertex-count graph) 
			  :initial-element *INF*))
	(prev (make-array (vertex-count graph)
			  :initial-element nil))
	(queue (make-instance 'min-pairing-heap)))
    (setf (aref dist source) 0)
    (dotimes (i (vertex-count graph))
      (setf queue (heap-insert queue (aref dist i) i)))
    (do ((queue queue (heap-remove-minimum queue)))
	((null (heap-minimum queue)) prev)
      (let ((u (heap-minimum queue)))
	(dolist (v (aref (adj graph) u))
	  (let ((alt (+ (aref dist u) 
			(aref (weight graph) u v))))
	    (when (< alt (aref dist v))
	      (setf (aref dist v) alt)
	      (setf (aref prev v) u))))))))
	      
	
  