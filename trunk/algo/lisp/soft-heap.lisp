;;; See http://en.wikipedia.org/wiki/Soft_heap
;;;
;;; Soft heap invented in 2000, is able to achieve amortized constant-time
;;; bounds for all five of its operations:
;;;
;;; create(S): create a new soft heap
;;; insert(S x): insert an element into a soft heap
;;; meld(S S'): combine the contents of two soft heaps into one, destroying both
;;; delete(S x): delete an element from a soft heap
;;; findmin(S): get the element with minimum key in the soft heap

;; ;;; Base class
;; (defclass heap ()
;;   ((sort-fn :initform #'< :initarg :sort-fn)
;;    (key-fn :initform #'identity :initarg :key-fn)))

;; (defgeneric heap-insert (h x))
;; (defgeneric heap-meld (h1 h2))
;; (defgeneric heap-delete (h x))
;; (defgeneric heap-findmin (h))

;; ;;; Soft heap
;; ;;;
;; ;;; Applications of soft heaps: 
;; ;;; - minimum spanning tree
;; ;;; - percentiles
;; ;;; - finding medians
;; ;;; - near sorting
;; ;;; - generally for approximate ranking
;; ;;;

;; ;;; INV1 (rank invariant):
;; ;;;   #children of the root >= (floor (rank root) 
;; (defclass soft-heap (heap)
;;   ((rank     :initform 0
;; 	     :initarg :rank
;; 	     :documentaion "rank")
;;    (ckey     :initform nil
;; 	     :initarg :ckey
;; 	     :documentation "common key")
;;    (children :initform nil
;; 	     :documentation "child nodes")
;;    (items    :initform nil
;; 	     :initarg :items
;; 	     :documentation "item list")
   
(defclass node ()
  ((ckey :initform nil :initarg :ckey)
   (rank :initform 0 :initarg :rank)
   (child :initform nil)
   (il :initform nil)
   (il-tail :initform nil)))

(defclass head ()
  ((queue :initform nil)
   (next :initform nil)
   (prev :initform nil)
   (suffix-min :initform nil)
   (rank :initform 0)))

(defclass soft-heap ()
  ((header :initform nil)
   (tail :initform nil)))

(defmethod heap-insert ((h soft-heap) newkey)
  (let ((l (make-insstance 'ilnode
			   :key newkey)))
    (let ((q (make-instance 'node 
			    :ckey newkey
			    :il l
			    :il-tail l)
	  