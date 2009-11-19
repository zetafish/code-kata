;; Segment Tree
;;
;; A segment tree is a tree data structure for storing intervals or segments. It allows
;; querying which of the stored segments contain a given point. It is, in principle, a
;; static structure; that is, its content cannot be modified once the structure is built.
;; 
;; A similar data structure is the interval tree.

;; See: http://en.wikipedia.org/wiki/Segment_tree


(defstruct (aa (:constructor nil))
  (key)
  (value)
  (level 1)
  (left nil)
  (right nil))

(defun skew (node)
  (cond ((null node) node)
        ((null (aa-left node)) node)
	((= (aa-level (aa-left node)) (aa-level node))
	 (let ((l (aa-left node)))
	   (setf (aa-left node) (aa-right l)
		 (aa-right l) node)
	   l))
	(t node)))

(defun split (node)
  (cond ((null node) node)
	((null (aa-right node)) node)
	((null (aa-right (aa-right node))) node)
	((= (aa-level node)
	    (aa-level (aa-right (aa-right node))))
	 (let ((r (aa-right node)))
	   (setf (aa-right node) (aa-left r)
		 (aa-left r) node)
	   (incf (aa-level r))
	   r))
	(t node)))

(defun aa-insert (node key value)
  (split
   (skew
    (cond ((null node)
	   (make-aa key value))
	  ((< key (aa-key node))
	   (setf (aa-left node) 
		 (aa-insert (aa-left node) key value))
	   node)
	  ((> key (aa-key node))
	   (setf (aa-right node) 
		 (aa-insert (aa-right node) key value))
	   node)
	  (t 
	   (setf (aa-value node) value)
	   node)))))
	    

(defun build-tree (intervals)
  (let ((tree nil))
    (dolist (x (sort (mapcar #'(lambda (x) (first x)) intervals) #'<))
      (setf tree (aa-insert tree x nil)))
    
	
    