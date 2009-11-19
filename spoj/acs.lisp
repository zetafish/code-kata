;;; https://www.spoj.pl/problems/ACS/

(defconstant +nrow+ 5678)
(defconstant +ncol+ 12)


(defclass cell ()
  ((value :initarg :value
	  :reader value)
   (row :initarg :row
	:reader row)
   (col :initarg :col
	:reader col)))

(defclass row ()
  ((index :accessor index
	  :initarg :index)
   (cells :initform (make-array +ncol+)
	  :accessor cells)))

(defclass col ()
  ((index :accessor index
	  :initarg :index)
   (cells :initform (make-array +nrow+)
	  :accessor cells)))

(defparameter *cells* (make-array (* +nrow+ +ncol+)))
(defparameter *rows* (make-array +nrow+))
(defparameter *cols* (make-array +ncol+))

(defun init-data ()
  (dotimes (r +nrow+)
    (setf (aref *rows* r)
	  (make-instance 'row :index r)))
  (dotimes (c +ncol+)
    (setf (aref *cols* c)
	  (make-instance 'col :index c)))
  (dotimes (r +nrow+)
    (dotimes (c +ncol+)
      (let ((cell (make-instance 'cell
				 :value (+ (* c +nrow+) r)
				 :row (aref *rows* r)
				 :col (aref *cols* c))))
	(setf (aref *cells* (+ (* c +nrow+) r)) 
	      cell)
	(setf (aref (cells (aref *rows* r)) c)
	      cell)
	(setf (aref (cells (aref *cols* c)) r)
	      cell)))))

(defun r (a b)
  (rotatef (index (aref *rows* a))
	   (index (aref *rows* b)))
  (rotatef (aref *rows* a)
	   (aref *rows* b))
  (dotimes (c +ncol+)
    (let ((col (cells (aref *cols* c))))
      (rotatef (aref col a)
	       (aref col b)))))

(defun c (a b)
  (rotatef (index (aref *cols* a))
	   (index (aref *cols* b)))
  (rotatef (aref *cols* a)
	   (aref *cols* b))
  (dotimes (r +nrow+)
    (let ((row (cells (aref *rows* r))))
      (rotatef (aref row a)
	       (aref row b)))))

(defun w (z)
  (let ((cell (aref *cells* z)))
    (cons (1+ (index (col cell)))
	  (1+ (index (row cell))))))

(defun q (a b)
  (value (aref (cells (aref *rows* a)) b)))
  
(defun main ()
  (loop
       (let ((cmd (read)))
	 (cond ((eq 'R cmd)
		(r (1- (read)) (1- (read))))
	       ((eq 'C cmd)
		(c (1- (read)) (1- (read))))
	       ((eq 'Q cmd)
		(format t "~a~%" (q (1- (read)) (1- (read)))))
	       ((eq 'W cmd)
		(destructuring-bind (x . y) (w (1- (read)))
		  (format t "~a ~a~%" x y)))))))

(init-data)
(main)

