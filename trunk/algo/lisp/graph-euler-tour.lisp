;;; ======================================================================
;;; Euler Tour algorithm
;;;
;;; In graph theory, an Eulerian path is a path in a graph which visits
;;; each edge exactly once. Similarlym, an Eulerian circuit is an Eulerian
;;; path which starts and ends on the same vertex.
;;;
;;; See: http://en.wikipedia.org/wiki/Euler_tour
;;; ======================================================================



;; -----------------------------------------------------------------------
;; eulerian-path
;; 
;; count: number of vertices
;; adj  : array of adjecency lists
;; -----------------------------------------------------------------------
(defun eulerian-path (count adj)
  "Computes Eulerian path of a connected undirected graph."

  ;; Select vertex with odd degree if one exists
  (let ((n (dotimes (x count)
	     (when (oddp (length adj))
	       (return x)))))
    (when (null n) (setf n 0))
