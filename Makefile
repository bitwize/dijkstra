GSI ?= gsi

.PHONY: test

test:
	cd tests && $(GSI) ../lib/ ./dijkstra-test.scm
