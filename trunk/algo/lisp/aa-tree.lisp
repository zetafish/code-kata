;; Implementation of AA Tree.
;;
;; An AA tree is a variation of the red-black tree. Unlike red-black
;; tree, red nodes on an AA tree can only be added as a right subchild.
;; In other words, no red node can be a left sub-chile. This results in
;; the simulation of a 2-3 tree instead of a 2-3-4-tree, which greatly
;; simplifies the maintenance operations.
;;
;; An AA tree only has to consider the two following shapes:
;;
;; [x]     [x]
;;   \       \
;;   [y]     [y]
;;             \
;;             [z]
;;
;; See: http://en.wikipedia.org/wiki/AA_tree

(defstruct (aa (:constructor make-aa (key value)))
  (key)
  (value)
  (level 1)
  (left nil)
  (right nil))

(defun skew (node)
  "Skew is a right rotation when an insertion or deletion creates
   a left horizontal link:
       
        [L]<--[T]           [L]-->[T]
       /   \     \   ==>   /     /   \
      +     +     +       +     +     +
     [A]    [B]   [R]   [A]    [B]   [R]

  "
  (cond ((null node) node)
        ((null (aa-left node)) node)
	((= (aa-level (aa-left node)) (aa-level node))
	 ;; swap pointers of left links
	 (let ((l (aa-left node)))
	   (setf (aa-left node) (aa-right l)
		 (aa-right l) node)
	   l))
	(t node)))

(defun split (node)
  "Split is a conditional left rotation when an insertion or deletion
   creates two horizontal right links:

     [T]-->[R]-->[X]   ==>   [R]
     /     /                /   \
    +     +                +     +
   [A]  [B]               [T]   [X]
                         /   \  
                        +     +
                      [A]     [B]
  "
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
    ;; Do the normal binary tree insert. Set the result of the
    ;; recursive call to the correct child in case a new node
    ;; was created or the root of the subtree changes.
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
	    
(defun aa-inorder (node)
  (if (null node) nil
      (append (aa-inorder (aa-left node))
	      (list (cons (aa-key node)
			  (aa-value node)))
	      (aa-inorder (aa-right node)))))
      