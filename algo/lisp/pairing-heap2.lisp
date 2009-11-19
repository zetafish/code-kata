;;; Pairing Heap
;;;
;;; See:
;;;   http://en.wikipedia.org/wiki/Pairing_heap

(defclass heap ()
  ((key :initform nil :initarg :key :accessor key)
   (val :initform nil :initarg :val :accessor val)
   (sub :initform nil :accessor sub)))

(defclass min-heap (heap) ())

(defclass max-heap (heap) ())

(defmethod heap-merge ((h1 max-heap) (h2 max-heap))
  (if (> (key h1) (key h2))
      (push h1 (sub h2))
      (push h2 (sub h1))))

(defmethod heap-merge ((h1 min-heap) (h2 min-heap))
  (if (< (key h1) (key h2))
      (push h1 (sub h2))
      (push h2 (sub h1))))

(defmethod heap-insert ((h heap) key &optional value)
  (heap-merge h (make-instance (class-of h)
			       :key key 
			       :val value)))

(defmethod heap-remove-top ((h heap))
  (do ((front (copy-list (sub h)))
       (back nil))
      ((and (null back) (null (rest front)))
       (first front))
    (if (null front)
	(setf front (nreverse back)
	      back nil)
	(push (heap-merge (pop front) (pop front)) back))))

(defun build-heap (&rest lst)
  (let ((h (make-instance 'max-heap)))
    (dolist (x lst)
      (setf h (heap-ins h x)))
    h))


(defun test ()
  (let ((h nil))
    (setf h (heap-insert h 9))
    (setf h (heap-merge h (build-heap 9 3)))
    (setf h (heap-merge h (build-heap 4 2)))
    (setf h (heap-merge h (build-heap 6 3 1)))
    (setf h (heap-merge h (build-heap 5 0)))
    (setf h (heap-merge h (build-heap 7 1)))
    (setf h (heap-merge h (build-heap 8 2)))
    (setf h (heap-remove-top h))
    (princ #\Newline)
    (heap-print h)))
    

(defun heap-print (heap &optional (prefix ""))
  (when heap
    (princ (key heap))
    (when (sub heap)
      (princ #\Newline))
    (do ((sub (sub heap) (rest sub)))
	((null sub))
      (princ prefix)
      (princ (if (rest sub) "+--" "\\--"))
      (heap-print 
       (first sub) 
       (string-concat prefix 
		      (if (rest sub) "|" " ")
		      "  "))
      (princ #\Newline) 
      (when (null (rest sub)) 
	(princ prefix)))))
	
(test)    
