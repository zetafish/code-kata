;;; http://en.wikipedia.org/wiki/Depth-first_search

(defun dfs (adj preorder-f postorder-f)
  (let ((n (array-total-size adj)))
    (when (> n 0)
      (let ((visit (make-array n 
			       :element-type 'bit
			       :initial-element 0)))
	(funcall preorder-f 0)
	(setf (aref visit 0) 1)
	(do ((todo (make-array n
			       :element-type 'fixnum
			       :fill-pointer 1
			       :initial-element 0)))
	    ((= 0 (fill-pointer todo)))
	  (let ((x (vector-pop todo)))
	    (funcall postorder-f x)
	    (dolist (y (aref adj x))
	      (when (= 0 (aref visit y))
		(funcall preorder-f y)
		(setf (aref visit y) 1)
		(vector-push y todo)))))))))

(defun test-dfs ()
  (let ((pre nil)
	(post nil)
	(adj #((1 2 3 4 5 6) (5) (6) (5) (2) (4) (1))))
    (dfs adj 
	 #'(lambda (x) (setf pre (push x pre)))
	 #'(lambda (x) (setf post (push x post))))
    (print (reverse pre))
    (print (reverse post))))
	 
	  
	    

