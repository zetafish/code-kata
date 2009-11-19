;; ================================================
;; https://www.spoj.pl/problems/SETSTACK/
;;
;; ================================================



(defun make-machine()
  (let ((ss nil)
	(hist nil))
    #'(lambda (cmd)
	(cond ((eq cmd 'push)
	       (push nil ss))

	      ((eq cmd 'dup)
	       (push (first ss) ss))

	      ((eq cmd 'union)
	       (cond ((eq (first hist) 'dup) 
		      (pop ss))
		     ((eq (first hist) 'push) 
		      (pop ss))
		     (t 
		      (push (union (pop ss) (pop ss) :test #'equal) ss))))

	      ((eq cmd 'intersect)
	       (cond ((eq (first hist) 'push) 
		      (pop ss) (pop ss) (push nil ss))
		     ((eq (first hist) 'dup) 
		      (pop ss))
		     (t 
		      (push (intersection (pop ss) (pop ss) :test #'equal) ss))))

	      ((eq cmd 'add)
	       (push (adjoin (pop ss) (pop ss) :test #'equal) ss)))
	(push cmd hist)
	(length (first ss)))))

(defun test1 ()
  (let ((m (make-machine)))
    (dolist (x '(push dup add push add dup add dup union))
      (format t "~d~%" (funcall m x)))))

(defun main ()
  (dotimes (i (read))
    (let ((m (make-machine)))
      (dotimes (j (read))
	(format t "~d~%" (funcall m (read)))))
    (format t "***~%")))

(main)