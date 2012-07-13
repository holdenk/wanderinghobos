CSCFLAGS = -O2
CSC = csc $(CSCFLAGS)

lifter: src/*.scm src/*.ss
	$(CSC) -o $@ $^

test: main
	cat maps/contest1.map | ./lifter