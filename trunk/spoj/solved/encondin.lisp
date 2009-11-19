;;; https://www.spoj.pl/problems/ENCONDIN/
;;;
;;; 2-9 identical characters are encoded by
;;; two characters: n(=length) v(=value)
;;; More than 9 -> first 9 9 9 then n
;;; Non-repeating block is enclosed by '1'
;;; 1 in a non-repeating block is escaped to 11
;;;
;;; Examples:
;;;   AAAAAABCCCC -> 6A1B14C
;;;   12344 -> 11123124


(defun run-length-encode (lst)
  (do ((lst lst (rest lst))
       (result nil))
      ((null lst) (nreverse result))
    (let ((cur (first lst))
	  (prev (second (first result))))
      (cond ((or (null prev) 
		 (not (eq cur prev))
		 (= 9 (first (first result))))
	     (push (list 1 cur) result))
	    (t (incf (first (first result))))))))
	    
(defun group-by (similar-p lst)
  (do ((lst lst (rest lst))
       (result nil))
      ((null lst) (nreverse (mapcar #'reverse result)))
    (if (or (null result)
	    (not (funcall similar-p (first lst) (first (first result)))))
	(push (list (first lst)) result)
	(push (first lst) (first result)))))


(defun encode (s)
  (mapcar #'(lambda (lst)
	      (list (first (first lst)) 
		    (mapcar #'(lambda (y) (second y)) lst)))
	  (group-by #'(lambda (x y) (= 1 (first x) (first y)))
		    (run-length-encode (map 'list #'identity s)))))

(defun print-group (group)
  (let ((n (first group))
	(g (second group)))
    (if (= n 1)
	(progn
	  (princ 1)
	  (dolist (x g)
	    (princ x)
	    (when (eq #\1 x) (princ x)))
	  (princ 1))
	(progn
	  (princ n)
	  (princ (first g))))))

(defun print-encoding (lst)
  (mapcar #'(lambda (elem) (print-group elem))
	  lst))

(defun main ()
  (do ((s (read-line) (read-line)))
      (nil)
    (print-encoding (encode s))
    (terpri)))
    
       

(handler-case  
    (main)
  (condition ()))
	  
