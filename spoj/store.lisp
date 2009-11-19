;;; https://www.spoj.pl/problems/STORE/

(defconstant +CASE+ #\s)
(defconstant +KEEPER+ #\m) 
(defconstant +PARCEL+ #\p)
(defconstant +GOAL+ #\k)
(defconstant +EMPTY+ #\w)

(defun opposite (d)
  (cond ((eq d :up)	:dow)
	((eq d :down)	:up)
	((eq d :left)	:right)
	((eq d :right)	:left)))

(defstruct (point (:constructor make-point (x y)))  (x) (y))
(defmethod up ((pt point))    (make-point (point-x pt) (1- (point-y pt))))
(defmethod down ((pt point))  (make-point (point-x pt) (1+ (point-y pt))))
(defmethod left ((pt point))  (make-point (1- (point-x pt)) (point-y pt)))
(defmethod right ((pt point)) (make-point (1+ (point-x pt)) (point-y pt)))

(defmethod around ((pt point))
  (dolist (d '(:up :down :left :right))
    (princ d)))

(defmethod pp (x y)
  #'(lambda (op)
      (cond ((eq op :up)    (decf y))
	    ((eq op :down)  (incf y))
	    ((eq op :left)  (decf x))
	    ((eq op :right) (incf x))
	    ((eq op :x)     x)
	    ((eq op :y)     y)
	    ((eq op :clone) (pp x y)))))

(defmacro with-maze ((r c) m &body body)
  `(destructuring-bind (,r ,c) (array-dimensions ,m)
     (progn
       ,@body)))

(defun grid-validp (maze &rest pts)
  (with-maze (rows cols) maze
    (every #'(lambda (p)
	       (and (<= 0 (car p)) (< (car p) rows)
		    (<= 0 (cdr p)) (< (cdr p) cols)))
	   pts)))

(defun grid-get (grid p)
  (aref grid (car p) (cdr p)))

(defun grid-zerop (grid p)
  (zerop (grid-get grid p)))

(defun grid-emptyp (grid &rest pts)
  (every #'(lambda (p)
	     (char= +EMPTY+ (grid-get grid p)))
	 pts))

(defun grid-setf (grid p val)
  (when p
    (setf (aref grid (car p) (cdr p)) val)))

(defun grid-find (maze ch)
  (with-maze (rows cols) maze
    (dotimes (r rows)
      (dotimes (c cols)
	(when (equal ch (aref maze r c))
	  (return-from grid-find (cons r c)))))))

(defun print-maze (maze)
  (with-maze (rows cols) maze
    (dotimes (r rows)
      (terpri)
      (dotimes (c cols)
	(princ (let ((ch (grid-get maze (cons r c))))
		 (cond ((char= +EMPTY+ ch) #\Space)
		       ((char= +CASE+ ch) #\x)
		       (t ch))))))
    (terpri)))

(defun reachablep (maze start goal)
  (let ((visit (make-array (array-dimensions maze)
			   :element-type 'bit
			   :initial-element 0))
	(stack (list start)))
    (flet ((branch (p)
	     (when (and (grid-validp maze p)
			(grid-zerop visit p)
			(grid-emptyp maze p))
	       (grid-setf visit p 1)
	       (push p stack))))

      (grid-setf visit start 1)
      (do ()
	  ((null stack))
	(let ((p (pop stack)))
	  (when (pt= p goal)
	    (return t))
	  (branch (north p))
	  (branch (south p))
	  (branch (east p))
	  (branch (west p)))))))

(defun solve (maze)
  (let ((keeper (grid-find maze +KEEPER+))
	(goal (grid-find maze +GOAL+))
	(parcel (grid-find maze +PARCEL+))
	(dist (make-hash-table :test #'equal))
	(queue nil)
	(next nil))
    (setf (gethash (list keeper parcel) dist) 0)
    (push (list keeper parcel) queue)
    (grid-setf maze keeper +EMPTY+)
    (grid-setf maze parcel +EMPTY+)
    (grid-setf maze goal +EMPTY+)
    
    (flet ((branch (d k1 k2 p1 p2)
	     (when (and (grid-validp maze k2 p2)
			(grid-emptyp maze k2 p2)
			(null (gethash (list p1 p2) dist))
			(reachablep maze k1 k2))
	       (setf (gethash (list p1 p2) dist) d)
	       (push (list p1 p2) next))))
      (do ((d 1 (1+ d)))
	  ((null queue) 'NO)
	(dolist (elem queue)
	  (destructuring-bind (k p) elem
	    (when (pt= goal p)
	      (return-from solve (gethash (list k p) dist)))
	    (grid-setf maze parcel +EMPTY+)
	    (grid-setf maze p +PARCEL+)
	    (setf parcel p)
	    (branch d k (south p) p (north p))
	    (branch d k (north p) p (south p))
	    (branch d k (east p) p (west p))
	    (branch d k (west p) p (east p))))
	(setf queue next
	      next nil)))))


(defun read-maze ()
  (let ((rows (read))
	(cols (read)))
    (let ((maze (make-array (list rows cols))))
      (dotimes (r rows)
	(let ((s (map 'vector #'identity (read-line))))
	  (dotimes (c cols)
	    (setf (aref maze r c) (svref s c)))))
      maze)))

(defun test-read-maze ()
  (with-input-from-string (buf "10 12
SSSSSSSSSSSS
SwwwwwwwSSSS
SwSSSSwwSSSS
SwSSSSwwSKSS
SwSSSSwwSwSS
SwwwwwPwwwww
SSSSSSSwSwSw
SSSSSSMwSwww
SSSSSSSSSSSS
SSSSSSSSSSSS
")
    (let ((*standard-input* buf))
      (read-maze))))



(defun xmain ()
  (dotimes (i (read))
    (format t "~a~%" (solve (read-maze)))))


(xmain)