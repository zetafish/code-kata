(defvar *data* (make-array 10000 :initial-element nil))
;(values (read-sequence *data* (make-string-input-stream "test string")) *data*)
;(read-sequence *data* (make-string-input-stream "test string"))
;(print *data*)

;(read-sequence *data* *standard-input*)
;(print *data*)


(defun bulk-read-1 ()
  (let ((buf (make-string 10)))
    (print (read-sequence buf *standard-input*))
    (print buf)))

(bulk-read-1)