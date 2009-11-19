;;; https://www.spoj.pl/problems/ASSIGN/
;;;
;;; Assign n topics to n students so that everybody gets
;;; exactly one topic he likes

(let ((count 0))

  (defun reset ()
    (setf count 0))

  (defun inc-count()
    (incf count))
  
  (defun get-count ()
    count))

(defclass student ()
  (pref (make-array 21 
		    :element-type 'fixnum
		    :initial-element nil)))

(defmethod likes ((s student) (t fixnum))
  (with-slots (pref) s
    (svref pref t)))
  
  

(defun seek (assign y x)
  (cond ((= *count* y) (get-count))
	((= *count* x) (assign (1+ y) x))
	(t (let ((sub 0))
	     (do ((
  