;; Implementation of AVL Tree.
;;
;; An AVL tree is a self-balancing binary search tree. In an AVL
;; tree, the heights of the two child subtrees of any node differ
;; by at most one; therefore, it is also said to be height-balanced.
;;
;; Lookup, insertion and deletion all take O(log n) time in both
;; average and worst cases, where n is the number of nodes in the
;; tree prior to the operation. Insertions and deletions may require
;; the tree to be rebalanced by one or more tree rotations.
;;
;; See: http://en.wikipedia.org/wiki/AVL_tree



(defstruct (avl (:constructor 
		 make-avl (left key value right)))
  (left)
  (key)
  (value)
  (right)
  (height 0))

(defun avl-balance-factor (node)
  (cond ((null node) 0)
	(t (- (avl-height (avl-right node))
	      (avl-height (avl-left node))))))
  

(defun avl-insert (node key value)
  (if (null node)
      (make-avl nil key value nil)
      (let ((k (avl-key node)))
	(cond ((< key k) 
	       (setf (avl-left node) 
		     (avl-insert node key value)))
	      ((> key k)
	       (setf (avl-right node)
		     (avl-insert node key value)))
	      (t (setf (avl-value node) value)))
	node)))
	       
	     
	   