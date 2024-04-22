(import (scheme base))
(import (srfi 8))
(import (srfi 64))
(import (srfi 69))
(import (bitwize dijkstra))

(define node-1 (make-graph-node 1))
(define node-2 (make-graph-node 2))
(define node-3 (make-graph-node 3))
(define node-4 (make-graph-node 4))

(connect! node-1 node-2 24)
(connect! node-1 node-4 20)
(connect! node-3 node-1 3)
(connect! node-4 node-3 12)

(test-begin "shortest paths")

(receive (dists prevs) (find-shortests node-1)
  (test-eqv 4 (hash-table-size dists))
  (test-eqv 0 (hash-table-ref dists node-1))
  (test-eqv 24 (hash-table-ref dists node-2))
  (test-eqv 3 (hash-table-ref dists node-3))
  (test-eqv 15 (hash-table-ref dists node-4)))
