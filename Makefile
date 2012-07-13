CSCFLAGS = -O2
CSC = csc $(CSCFLAGS)

SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)

all: lifter test

lifter: $(SOURCES)
	$(CSC) -o $@ $^

test: lifter
	cat maps/contest1.map | ./lifter

clean:
	rm -f lifter src/*.o

.PHONY: all clean