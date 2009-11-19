;;; =============================================
;;; https://www.spoj.pl/problems/ORDERSET/
;;;
;;; Solved using van Emde Boas trees
;;; =============================================


(defstruct (veb (:constructor make-veb (bits)))
  (bits)
  (min)
  (max)
  (summary)
  (sub))

(defmacro min-key (veb)
  `(first (veb-min ,veb)))

(defmacro min-val (veb)
  `(rest (veb-min ,veb)))

(defmacro max-key (veb)
  `(first (veb-max ,veb)))

(defmacro max-val (veb)
  `(rest (veb-max ,veb)))

(defun bits/2 (veb)
  (ash (veb-bits veb) -1))

(defun sqrt-range (veb)
  (ash 1 (ash (veb-bits veb) -1)))

(defun split (veb x)
  (values 
   (ash x (- (bits/2 veb)))
   (logand x (- (ash 1 (bits/2 veb)) 1))))
		
(defun emptyp (veb)
  (null (min-key veb)))

(defun singlep (veb)
  (and (not (emptyp veb))
       (= (min-key veb) (max-key veb))))

(defun sub (veb index)
  (when (null (veb-sub veb))
    (setf (veb-sub veb) 
	  (make-array (ash 1 (bits/2 veb))
		      :initial-element nil)))
  (when (null (aref (veb-sub veb) index))
    (setf (aref (veb-sub veb) index)
	  (make-veb (bits/2 veb))))
  (aref (veb-sub veb) index))

(defun summary (veb)
  (when (null (veb-summary veb))
    (setf (veb-summary veb) 
	  (make-veb (bits/2 veb))))
  (veb-summary veb))

(defun ins (veb k &optional v)
  (cond ((emptyp veb) 
	 (setf (veb-min veb) (cons k v)
	       (veb-max veb) (cons k v)))

	((= (min-key veb) k)
	 (setf (min-val veb) v))

	((= (max-key veb) k)
	 (setf (max-val veb) v))

	((> (min-key veb) k)
	 (rotatef (min-key veb) k)
	 (rotatef (min-val veb) v)
	 (ins veb k v))

	((< (max-key veb) k)
	 (rotatef (max-key veb) k)
	 (rotatef (max-val veb) v)
	 (ins veb k v))

	(t (multiple-value-bind (hi lo) 
	       (split veb k)
	     (when (emptyp (sub veb hi))
	       (ins (summary veb) hi))
	     (ins (sub veb hi) lo v)))))

(defun pred-max-key (veb)
  (let* ((i (max-key (veb-summary veb)))
	 (j (max-key (aref (veb-sub veb) i))))
    (+ j (* i (sqrt-range veb)))))

(defun succ-min-key (veb)
  (let* ((i (min-key (veb-summary veb)))
	 (j (min-key (aref (veb-sub veb) i))))
    (+ j (* i (sqrt-range veb)))))


(defun succ (veb k)
  (cond ((emptyp veb) nil)
	((< k (min-key veb)) (veb-min veb))
	((>= k (max-key veb)) nil)
	((null (veb-summary veb)) (veb-max veb))
	((>= k (pred-max-key veb)) (veb-max veb))
	(t
	 (multiple-value-bind (hi lo) (split veb k)
	   (if (or (emptyp (sub veb hi)) 
		   (<= (max-key (sub veb hi)) lo))
	       (let* ((i (first (succ (summary veb) hi)))
		      (j (min-key (sub veb i))))
		 (cons
		  (+ j (* i (sqrt-range veb)))
		  (min-val (sub veb i))))
	       (destructuring-bind (j . v) (succ (sub veb hi) lo)
		 (cons
		  (+ j (* hi (sqrt-range veb)))
		  v)))))))

(defun pred (veb k)
  (cond ((emptyp veb) nil)
	((> k (max-key veb)) (veb-max veb))
	((<= k (min-key veb)) nil)
	((null (veb-summary veb)) (veb-min veb))
	((<= k (succ-min-key veb)) (veb-min veb))
	(t
	 (multiple-value-bind (hi lo) (split veb k)
	   (if (or (emptyp (sub veb hi))
		   (<= lo (min-key (sub veb hi))))
	       (let* ((i (first (pred (summary veb) hi)))
		      (j (max-key (sub veb i))))
		 (cons
		  (+ j (* i (sqrt-range veb)))
		  (max-val (sub veb i))))
	       (destructuring-bind (j . v) (pred (sub veb hi) lo)
		 (cons
		  (+ j (* hi (sqrt-range veb)))
		  v)))))))
