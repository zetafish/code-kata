;;; https://www.spoj.pl/problems/MUL

(defun mul (s)
  (with-open-stream (in (make-string-input-stream s))
;    (read in)
;    (read in)
    0))


(defun main()
  (dotimes (i (read))
    (let ((s (read-line)))
      (princ (mul s))
      (princ #\Newline))))

(main)

