(define-library (bitwize dijkstra)
  (import (scheme base))
  (import (srfi 8))
  (import (srfi 9))
  (import (srfi 69))
  (export make-graph-node
	  graph-node-name
	  graph-node-neighbors
	  connect!
	  find-shortests)
  (include "dijkstra.scm"))
