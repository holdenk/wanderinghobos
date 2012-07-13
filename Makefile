main:
	csc *.scm -o main
test: main
	cat maps/contest1.amp | ./main