;;; A* algoritm

;;; Pairing heap
(defclass pheap ()
  ((sort-fn :initform #'>
	    :initarg :sort-fn)
   (key-fn  :initform #'identity
	    :initarg :key-fn)
   (nodes   :initform nil)
   (root    :initform nil :initarg :root)
   (count   :initform 0 :initarg :count)))

(defgeneric heap-meld (heap1 heap2))
(defgeneric heap-add (heap item))
(defgeneric heap-pop (heap)

;; (defmethod heap-meld ((h1 pheap) (h2 pheap))
;;   (with-slots ((sort sort-fn)
;; 	       (key key-fn)
;; 	       (h1-root root)
;; 	       (h1-nodes nodes)
;; 	       (h1-count count)) h1
;;     (with-slots ((h2-root root)
;; 		 (h2-nodes nodes)
;; 		 (h2-count count)) h2
;;       (let ((h (make-instance 'pheap 
;; 			      :sort-fn sort
;; 			      :key-fn key)))
;; 	(with-slots ((h-root root)
;; 		     (h-nodes nodes)
;; 		     (h-count count))
;; 	    (cond ((= 0 h1-count))))))))

(defmethod heap-add ((h pheap) item)
  (with-slots (sort-fn key-fn) h
    (heap-meld h (make-instance 'pheap
				:sort-fn sort-fn
				:key-fn key-fn
				:root item
				:count 1))))

(defmethod heap-pop ((h heap))
  (with-slots (root count nodes) h
    (cond ((= 0 count) (values nil nil))
	  (t (let ((item root))
	       (setf root nil
		     count (1- count))
	       (do ()
		   ((null (rest nodes)))
		 (do ((merged nil)
		      (p nodes (rest p)))
		     ((null p) (setf nodes merged))
		   (if (null (rest p))
		       (push (pop p) merged)
		       (push (heap-meld (pop p) (pop p)) merged))))
	       (values item t))))))

	    


(defun a-star (start goal key-fn heuristic-fn)
  "Determine the shortest path from start to goal."
  (let ((g-score (make-hash-table :test #'equal))
	(h-score (make-hash-table :test #'equal))
	(f-score (make-hash-table :test #'equal)))
    (let ((key (funcall key-fn start)))
      (setf (gethash key g) 0
	    (gethash key h) (funcall heuristic-fn start goal)
	    (gethash key f) (gethash key h))
    