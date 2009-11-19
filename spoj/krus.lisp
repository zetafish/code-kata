;; ================================================
;; https://www.spoj.pl/problems/KRUS/
;;
;; 2009-02-14: Initial file
;; 2009-02-15: Submitted => WA
;; 2009-02-15: Submitted => 
;; ================================================

(defstruct cell
  (wizard)
  (visits))


(defun make-grid (n)
  (let ((a (make-array (list n n))))
    (dotimes (r n)
      (dotimes (c n)
	(setf (aref a r c) (make-cell))))
    a))

(defun setf-wizard (grid row col wizard)
  (setf (cell-wizard (aref grid row col)) wizard))

(defun get-wizard-cmd (grid row col days)
  (let ((wiz (cell-wizard (aref grid row col))))
    (when wiz
      (aref wiz (mod days 7)))))


(define-modify-macro make-turn (mapping)
  (lambda (place mapping)
      (second (assoc place mapping))))
        

(defun make-walker (grid n k)
  (let ((turns 0)
	(days 0)
	(row 0)
	(col 0)
	(dir 'right))

    #'(lambda ()
	;; 	(format t "turns=~d days=~d row=~d col=~d dir=~a cell=~a~%"
	;; 		turns days row col dir (aref grid row col))
	;; listen to wizard if present and awake
	(let ((cmd (get-wizard-cmd grid row col days)))
	  (cond ((eq cmd #\L) 
		 (make-turn dir '((up left) (left down) (down right) (right up)))
		 (incf turns))
		((eq cmd #\R) 
		 (make-turn dir '((up right) (right down) (down left) (left up)))
		 (incf turns))))
    
	;; bounce from edge if needed
	(when (or (and (eq dir 'right) (= col (1- n)))
		  (and (eq dir 'left) (= col 0))
		  (and (eq dir 'down) (= row (1- n)))
		  (and (eq dir 'up) (= row 0)))
	  (make-turn dir '((left right) (right left) (up down) (down up)))
	  (incf turns))

	;; do a step if k is not reached
	(when (< turns k)
	  (cond ((eq dir 'up) (decf row))
		((eq dir 'down) (incf row))
		((eq dir 'left) (decf col))
		((eq dir 'right) (incf col)))
	  (incf days))

	(when (>= turns k)
	  days))))
	      	      

(defun test1 ()
  (let ((a (make-grid 5)))
    (setf-wizard a 0 2 "RRRRRRR")
    (setf-wizard a 0 4 "RRRRLRR")
    (let ((w (make-walker a 5 2)))
      (do ((result nil (funcall w)))
	  ((not (null result))
	   (format t "~d~%" result))))))
	
	   
(defun main ()
  (let ((n (read))	    ;  size of the grid  : 2<=n<=200
	(k (read))	    ;  number of turns   : 1<=k<=1,000,000,000
	(m (read)))	    ; number of wizards : 0<=m<=10,000
    (let ((a (make-grid n)))
      (dotimes (i m)
	(let ((r (1- (read)))
	      (c (1- (read)))
	      (s (read-line)))
	  (setf-wizard a r c s)))
      (let ((w (make-walker a n k)))
	(do ((result nil (funcall w)))
	    ((not (null result))
	     (format t "~d~%" result)))))))

(main)