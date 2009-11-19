;;; A* algorithm

(defclass prio-queue ()
  ((lst :initform nil)
   (key-fn :initform #'identity
	   :initarg :key-fn)))

(defmethod queue-delmin ((q prio-queue))
  (with-slots (lst) q
    (let ((x (reduce #'min q)))
      (setf lst (delete x lst))
      x)))

(defmethod queue-add ((q prio-queue) item)
  (with-slots (lst) q
    (push item lst)))

(defmethod queue-emptyp ((q prio-queue))
  (with-slots (lst) q
    (null lst)))
    

(defun a-start (start goal)
  (let ((g (make-hash-table :test #'equal))
	(h (make-hash-table :test #'equal))
	(f (make-hash-table :test #'equal)))
    (let ((open (make-instance 'prio-queue
				 :key-fn #'(lambda (x) (gethash f x)))))
      (queue-add closed start)
      (setf (gethash start g) 0
	    (gethash start h (funcall heuristic start goal))
	    (gethash start f (gethash start h)))
      (do ()
	  ((queue-emptyp open))
	(let ((x (queue-delmin open)))
	  (when (equal x goal)
	    (return-from a-start (