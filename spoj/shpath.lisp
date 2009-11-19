;;; https://www.spoj.pl/problems/SHPATH/

;;(load "../algo/pairing-heap.lisp")

(defclass heap ()
  ((sort-fun :initform #'>
	     :initarg :sort-fun
	     :reader heap-sort-fun)
   (key-fun :initform #'identity
	    :initarg :key-fun
	    :reader heap-key-fun)))
	     
(defclass pairing-heap (heap)
  ((root :initform nil)
   (nodes :initform nil)
   (count :initform 0)))

(defgeneric heap-meld (heap1 heap2))
(defgeneric heap-add (heap item))
(defgeneric heap-remove-root (heap))

(defmethod heap-meld ((h1 pairing-heap)
		      (h2 pairing-heap))
  (with-slots ((sort sort-fun)
	       (key key-fun)
	       (h1-root root)
	       (h1-nodes nodes)
	       (h1-count count)) h1
    (with-slots ((h2-root root)
		 (h2-nodes nodes)
		 (h2-count count)) h2
      (cond ((= 0 h1-count) h2)
	    ((= 0 h2-count) h1)
	    ((funcall sort
		      (funcall key h1-root)
		      (funcall key h2-root))
	     (push h2 h1-nodes)
	     (incf h1-count h2-count)
	     h1)
	    (t
	     (push h1 h2-nodes)
	     (incf h2-count h1-count)
	     h2)))))

(defmethod heap-add ((h pairing-heap) item)
  (with-slots (sort-fun key-fun) h
    (let ((s (make-instance 'pairing-heap
			    :sort-fun sort-fun
			    :key-fun key-fun)))
      (with-slots (root count) s
	(setf root item
	      count 1))
      (values (heap-meld h s) s))))


(defmethod heap-remove-root ((h pairing-heap))
  (with-slots (nodes count root) h
    (cond ((= 0 count) (error "cannot remove from empty heap"))
	  ((= 1 count)
	   (setf nodes nil root nil count 0)
	   h)
	  (t 
	   (dotimes (i (1- (length nodes)))
	     (do ((todo (copy-list nodes))
		  (tmp nil))
		 ((null todo)(setf nodes tmp))
	       (if (null (rest todo))
		   (push (pop todo) tmp)
		   (push (heap-meld (pop todo) (pop todo)) tmp))))
	   (first nodes)))))
  
(defclass heapq ()
  ((heap :initarg :heap)))

(defmethod heapq-push ((hq heapq) item)
  (with-slots (heap) hq
    (setf heap (heap-add heap item))))

(defmethod heapq-pop ((hq heapq))
  (with-slots (heap) hq
    (with-slots (root) heap
      (let ((item root))
	(setf heap (heap-remove-root heap))
	item))))

(defmethod heapq-empty-p ((hq heapq))
  (with-slots (heap) hq
    (= 0 (heap-size heap))))


(defun shortest-path (cost a b)
  (dijkstra cost a b))

(defun dijkstra (cost source destination)
  (let ((dist (make-array (length cost) 
			  :initial-element most-positive-fixnum))
	(visited (make-array (length cost)
			     :element-type 'bit
			     :initial-element 0))
	(hq (make-instance 'heapq
			   :heap (make-instance 
				  'pairing-heap 
				  :sort-fun #'<
				  :key-fun #'(lambda (x) (car x))))))
    (setf (aref dist source) 0)
    (heapq-push hq (cons source 0))
    (do ()
	((= 1 (aref visited destination)))
      (destructuring-bind (node . distance) (heapq-pop hq)
	(when (= 0  (aref visited node))
	  (setf (aref visited node) 1)
	  (dolist (edge (aref cost node))
	    (destructuring-bind (end . weight) edge
	      (let ((alt (+ (aref dist node) weight)))
		(when (< alt (aref dist end))
		  (setf	(aref dist end) alt)
		  (heapq-push hq (cons end alt)))))))))
      (aref dist destination)))

(defun main ()
  (dotimes (i (read))
    (let ((n (read)))
      (let ((names (make-array n))
	    (index (make-hash-table))
	    (cost (make-array n :initial-element nil)))
	(dotimes (j n)
	  (let ((name (read)))
	    (setf (aref names j) name
		  (gethash name index) j)
	    (dotimes (k (read))
	      (setf (aref cost j)
		    (push (cons (1- (read)) (read)) (aref cost j))))))
	(dotimes (j (read))
	  (let ((a (gethash (read) index))
		(b (gethash (read) index)))
	    (princ (shortest-path cost a b))
	    (terpri)))))))
	    
(defun test ()	    
  (with-input-from-string 
      (s "1 4 gdansk 2 2 1 3 3
              bygdgoszcz 3 1 1 3 1 4 4
              torun 3 1 3 2 1 4 1 
              warszawa 2 2 4 3 1 
              2
              gdansk warszawa
              bygdgoszcz warszawa")
    (let ((*standard-input* s))
      (main))))
			 

  
(main)