;; https://www.spoj.pl/problems/HAREFOX/

(defun main ()
  (dotimes (i (read))
    (let ((a (read))
	  (b (read))
	  (c (read))
	  (d (read))
	  (h1998 (read))
	  (f1998 (read)))
      (format t "~a~%" (solve a b c d h1998 f1998)))))
	    