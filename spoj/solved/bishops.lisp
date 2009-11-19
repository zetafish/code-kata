;; https://www.spoj.pl/problems/BISHOPS

(defun bishops (n)
  (cond ((= 0 n) 0)
	((= 1 n) 1)
	((= 2 n) 2)
	(t (* 2 (1- n)))))

(defun main ()
  (handler-case
      (loop (format t "~a~%" (bishops (read))))
    (end-of-file (e))))
    

(main)