;;; Pairing Heap
;;;
;;; See:
;;;   http://en.wikipedia.org/wiki/Pairing_heap
;;;-------------------------------------------------------------

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
  


(defun test-pairing-heap (&optional (sort #'>))
  (labels ((new-heap ()
	     (make-instance 'pairing-heap :sort-fun sort))
	   (build-heap (&rest items)
	     (let ((h (new-heap)))
	       (dolist (i items)
		 (setf h (heap-add h i)))
	       h)))
    (let ((h (new-heap)))
      (setf h (heap-add h 1))
      (setf h (heap-remove-root h))
      (setf h (heap-add h 1))
      (setf h (heap-meld h (build-heap 9)))
      (setf h (heap-meld h (build-heap 9 3)))
      (setf h (heap-meld h (build-heap 4 2)))
      (setf h (heap-meld h (build-heap 6 3 1)))
      (setf h (heap-meld h (build-heap 5 0)))
      (setf h (heap-meld h (build-heap 7 1)))
      (print h)
      (setf h (heap-meld h (build-heap 8 2)))
      (print h)
      (setf h (heap-remove-root h))
      (print h)
      (setf h (heap-remove-root h))
      (print h)
      (do ()
	  ((= 0 (heap-size h)))
	(setf h (heap-remove-root h))
	(print h))
      nil
      )))


(defmethod print-object ((h pairing-heap) stream)
 (labels ((aux (h prefix)
	       (with-slots (root nodes key-fun count) h
		 (when (> count 0)
		   (princ (funcall key-fun root) stream)
		   (when nodes (terpri stream))
		   (do ((sub nodes (rest sub)))
		       ((null sub))
		     (princ prefix stream)
		     (princ (if (rest sub) "+--" "\\--") stream)
		     (aux (first sub)
			  (concatenate 'string
				       prefix
				       (if (rest sub) "|" " ")
				       "  "))
		     (terpri stream)
		     (when (null (rest sub))
		       (princ prefix stream)))))))
   (print-unreadable-object (h stream :type t)
     (format stream "count=~a~%" (heap-size h))
     (aux h ""))))

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
