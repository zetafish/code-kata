;;; https://www.spoj.pl/problems/MCARDS

(defun split (lst)
  (let ((segments ()))
    (dolist (x lst)
      (if (or (null segments)
	      (< x (first (first segments))))
	  (push (list x) segments)
	  (push x (first segments))))
    segments))

(defun solve (lst)
  (let ((sizes (mapcar #'length
		       (split lst))))
;    (print sizes)
 ;   (print (apply #'+ sizes))
  ;  (print (apply #'max sizes))
    (- (apply #'+ sizes)
       (apply #'max sizes))))

    