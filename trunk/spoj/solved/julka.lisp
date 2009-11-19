;;; https://www.spoj.pl/problems/JULKA/

(defun main ()
  (dotimes (i 10)
    (let* ((total (read))
	   (delta (read))
	   (n (/ (- total delta) 2))
	   (k (+ delta n)))
      (princ k)
      (princ #\Newline)
      (princ n)
      (princ #\Newline))))

(main)
