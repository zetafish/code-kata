;;; Fibonacci heap
;;;
;;; See: http://en.wikipedia.org/wiki/Fibonacci_heap

(defclass heap ()
  ((min
    :accessor minimum
    :initform nil
    :type node)
   (nodes
    :accessor nodes
    :type node-list
    :initform (make-instance 'node-list))))

(defclass node () 
  ((key
    :reader key
    :initarg :key)
   (value
    :reader value
    :initarg :value
    :initform nil)
   (next
    :accessor next
    :initform nil)
   (prev
    :accessor prev
    :initform nil)
   (nodes
    :accessor nodes
    :initform (make-instance 'node-list))))

(defclass node-list ()
  ((head :accessor head :initform nil :type node)
   (tail :accessor tail :initform nil :type node)))

(defmethod concat ((a node-list) (b node-list))
  (cond ((null (head a)) b)
	((null (head b)) a)
	(t (setf (next (tail a)) (head b)
		 (prev (head b)) (tail a))
	   a)))

(defmethod insert (key value (heap node-list))
  "Insert key/value to heap."
  (let ((h2 (make-instance 'heap))
	(n (make-instance 'node :key key :value value)))
;;    (setf (head (nodes h2)) nq
	;;  (tail (nodes h2)) n)
    (fib-merge h2 heap)))


(defmethod fib-merge ((h1 heap) (h2 heap))
  "Destructively merge h1 and h2"
  (setf (nodes h1) (concat (nodes h1) (nodes h2)))
  (cond ((null (minimum h1)) (setf (minimum h1) (minimum h2)))
	((null (minimum h2)) nil)
	((< (key (minimum h2)) (key (minimum h1)))
	 (setf (minimum h1) (minimum h2))))
  h1)

(defmethod heap-remove-min ((h heap))
  (let ((orphans (nodes h))
	(degree (make-array 


(defmethod pp ((x node-list))
  (do ((p (head x) (next p)))
      ((null p))
    (princ (key p))
    (princ " "))
  (princ #\Newline))

(defun test ()
  (let ((lst (make-instance 'node-list)))
    (dotimes (i 10)
      (setf lst (insert (make-instance 'node :key i)
			lst))
      (pp lst))))
      
	
      

   


    
    
  

;; (defstruct (heap (:constructor make-heap (x)))
;;    (min (make-tree x))
;;    (size 1)
;;    (trees (list min)))

;; (defstruct (tree (:constructor make-tree (key)))
;;   (key)	
;;   (marked nil)
;;   (degree 0)
;;   (sub nil))

;; (defun heap-find-minimum (heap)
;;   (heap-node-key (heap-heap-min heap)))

;; (defun heap-insert (heap a)
;;   (heap-merge heap (make-heap a)))

;; (defun heap-remove-min (heap)
;;   (let ((orphans (tree-sub (heap-min heap)))
;; 	(found (make-array (ceiling (log (heap-size heap) 2))
;; 			   :initial-element nil)))
;;     (dolist (x orphans)
;;       (let ((d (tree-degree x)))
;; 	(if (aref found d)
;; 	    (
      
    
  
  
  
    
    
    
    
    
    
  

