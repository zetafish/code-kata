;;; ======================================================================
;;; Cartesian Tree
;;;
;;; See: http://en.wikipedia.org/wiki/Cartesian_tree
;;; ======================================================================

(defstruct (node (:constructor make-node (key)))
  (key)
  (left)
  (right)
  (parent))

(defun make-cartesian-tree (lst)
  (let ((heap nil))
    (do ((prev nil)
	 (lst lst (rest lst)))
	((null lst) heap)
      (let ((x (make-node (first lst)))
	    (y (do ((y prev (node-parent y)))
		   ((or (null y) (< (node-key y) (first lst)))
		    y))))
	(cond ((null y)
	       (when heap (setf (node-parent heap) x))
	       (setf (node-left x) heap
		     heap x))
	      (t
	       (when (node-right y) (setf (node-parent (node-right y)) x))
	       (setf (node-parent x) y
		     (node-left x) (node-right y)
		     (node-right y) x)))
	(setf prev x)))
    heap))
      
		
(defun in-order (node)
  (when node
    (in-order (node-left node))
    (princ (node-key node))
    (princ " ")
    (in-order (node-right node))))

(defun pre-order (node)
  (when node
    (format t "~a " (node-key node))
    (pre-order (node-left node))
    (pre-order (node-right node))))

(defun heap-print (node pivot)
  (when node
    (let* ((half (expt 2 (1- (heap-depth node))))
	   (spaces (make-string pivot :initial-element #\Space))
	   (dashes (make-string half :initial-element #\-)))
      (princ spaces)
      (princ (node-key node))
      (princ #\Newline)
      (when (node-left node)
	(princ (make-string (- pivot half) :initial-element #\Space))
	(princ dashes))
      (when (or (node-left node) (node-right node))
	(princ "+"))
      (when (node-right node)
	(princ dashes))
      (princ #\Newline)
      (heap-print (node-left node) (- pivot half))
      (heap-print (node-right node) (+ pivot half)))))
		  
      
  

(defun test ()
  (let ((lst '(9 3 7 1 8 12 10 20 15 18 5)))
    (let ((heap (make-cartesian-tree lst)))
      (in-order heap)
      (princ #\Newline)
      (pre-order heap)
      (princ #\Newline))))
      ;(heap-print heap (expt 2 (heap-depth heap))))))
    
     

(test)