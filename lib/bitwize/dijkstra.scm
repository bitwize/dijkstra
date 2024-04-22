(define-record-type heap
  (really-make-heap c v l)
  heap?
  (c heap-comparator)
  (v heap-vector heap-vector-set!)
  (l heap-len heap-len-set!))

(define (make-heap c)
  (really-make-heap c (make-vector 64) 0))

(define (heap-left-index heap x)
  (let* ((y (+ (* 2 x) 1)))
    (if (>= y (heap-len heap)) #f y)))

(define (heap-right-index heap x)
  (let* ((y (+ (* 2 x) 2)))
    (if (>= y (heap-len heap)) #f y)))

(define (heap-parent-index heap x)
  (if (<= x 0) #f
      (/ (- x (if (odd? x) 1 2)) 2)))

(define (heap-top heap)
  (vector-ref (heap-vector heap) 0))

(define (heap-remove! heap)
  (let* ((hl (heap-len heap)))
    (if (< hl 1)
	#f
	(let* ((top (heap-top heap)))
	  (heap-swap! heap 0 (- hl 1))
	  (heap-len-set! heap (- hl 1))
	  (let loop ((pos 0))
	    (cond
	     ((children-satisfy? heap pos) top)
	     (else
	      (let* ((left (heap-left-index heap pos))
		     (right (heap-right-index heap pos))
		     (chl
		      (cond
		       ((not left) right)
		       ((not right) left)
		       (((heap-comparator heap) right left)
			right)
		       
			(else  left))))
		(heap-swap! heap pos chl)
		(loop chl)))))))))

(define (heap-swap! heap x y)
  (let* ((hv (heap-vector heap))
	 (temp (vector-ref hv x)))
    (vector-set! hv x (vector-ref hv y))
    (vector-set! hv y temp)))

(define (children-satisfy? heap x)
  (let* ((c (heap-comparator heap))
	 (v (heap-vector heap)))
    (and (let* ((y (heap-left-index heap x)))
	   (or (not y)
	       (c
		(vector-ref v x)
		(vector-ref v y))))
	 (let* ((y (heap-right-index heap x)))
	   (or (not y)
	       (c (vector-ref v x)
		  (vector-ref v y)))))))

(define (heap-reallocate-vector! heap)
  (let* ((v1 (heap-vector heap))
	 (v1l (vector-length v1))
	 (v2 (make-vector (+ v1l (/ v1l 2))))
	 (v2l (vector-length v2)))
    (do ((i 0 (+ i 1)))
	((>= i v1l) (heap-vector-set! heap v2))
      (vector-set! v2 i (vector-ref v1 i)))))

(define (heap-add! heap item)
  (let* ((hl (heap-len heap)))
    (if (>= hl (vector-length (heap-vector heap)))
	(heap-reallocate-vector! heap))
    (let* ((hv (heap-vector heap)))
      (vector-set! hv hl item)
      (heap-len-set! heap (+ hl 1))
      (let loop ((pos hl))
	(let* ((par (heap-parent-index heap pos)))
	  (cond
	   ((or (zero? pos) (children-satisfy? heap par)) pos)
	   (else
	    (heap-swap! heap pos par)
	    (loop par))))))))

(define-record-type prio-node
  (make-prio-node v p)
  prio-node?
  (v prio-node-value)
  (p prio-node-priority prio-node-priority-set!))

(define (prio-<= a b)
  (<= (prio-node-priority a)
      (prio-node-priority b)))

(define (make-prio-queue)
  (make-heap prio-<=))
(define-record-type graph-node
  (really-make-graph-node name neighbors)
  graph-node?
  (name graph-node-name)
  (neighbors graph-node-neighbors graph-node-neighbors-set!))

(define (prio-queue-insert! q val prio)
  (heap-add! q (make-prio-node val prio)))

(define (make-graph-node name)
  (really-make-graph-node name '()))

(define (connect! graph-node-a graph-node-b distance)
  (let* ((connect-one-way!
	  (lambda (source destination)
	    (let ((a (assq destination (graph-node-neighbors source))))
	      (if a
		  (set-cdr! a distance)
		  (graph-node-neighbors-set! source
					     (cons
					      (cons destination distance)
					      (graph-node-neighbors source))))))))
    (connect-one-way! graph-node-a graph-node-b)
    (connect-one-way! graph-node-b graph-node-a)))

(define (find-shortests start-node)
  (let* ((visited (make-hash-table eq?))
	 (visit! (lambda (node) (hash-table-set! visited node #t)))
	 (pq (make-prio-queue))
	 (dists (make-hash-table eq?))
	 (prevs (make-hash-table eq?))
	 (visited? (lambda (x) (hash-table-ref/default visited x #f)))
	 (dist (lambda (x) (hash-table-ref/default dists x +inf.0)))
	 (prev (lambda (x) (hash-table-ref/default prevs x '()))))
    (prio-queue-insert! pq start-node 0)
    (hash-table-set! dists start-node 0)
    (let loop ()
      (if
       (zero? (heap-len pq)) (values dists prevs)
       (let* ((u (prio-node-value (heap-remove! pq))))
	 (if (not (visited? u))
	     (for-each
	      (lambda (x)
		(receive (v edge)
		    (values (car x) (cdr x))
		  (if (< (+ (dist u) edge) (dist v))
		      (begin
			(hash-table-set! dists v (+ (dist u) edge))
			(hash-table-set! prevs v u)
			(prio-queue-insert! pq v (dist v))))))
	      (graph-node-neighbors u)))
	 (visit! u)
	 (loop))))))
