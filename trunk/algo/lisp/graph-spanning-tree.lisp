;;; Spanning Tree Algorithm's
;;;
;;; See:
;;;   http://en.wikipedia.org/wiki/Kruskal%27s_algorithm
;;;   http://en.wikipedia.org/wiki/Prim%27s_algorithm
;;;   http://en.wikipedia.org/wiki/Reverse-Delete_algorithm
;;;   http://en.wikipedia.org/wiki/Bor%C5%AFvka%27s_algorithm

(defclass graph ()
  ((vertices :initform nil :accessor vertices)))
    
(defclass edge ()
  ((weight :initform 1 :initarg :weight :accessor weight)
   (u :initform (error "Missing u") :initarg :u :accessor u)
   (v :initform (error "Missing v") :initarg :v :accessor v)))


(defun kruskal (graph)
  (let ((queue nil)
	(tree nil))
    (dolist (e (edges graph))
      (setf queue (heap-insert queue (weight e) e)))
    (dotimes (i (length (vertices graph)))
      (multiple-value-bind (u v) (val (minimum queue))
	...
    
      
    