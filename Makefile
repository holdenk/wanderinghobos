CSCFLAGS = -O2
CSC = csc $(CSCFLAGS)

SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)

PRODUCT = lifter

TEST_CASES = contest1

all: $(PRODUCT) test

$(PRODUCT): $(addsuffix .o, $(basename $(SOURCES)))
	$(CSC) -o $@ $^

test: $(PRODUCT) $(addsuffix .result, $(TEST_CASES))

clean:
	rm -f lifter src/*.o

.PHONY: all clean test
#.SILENT:

%.o: %.scm
	$(CSC) -c $<

%.o: %.ss
	$(CSC) -c $<

#### TEST CASES ####

%.result: tests/%.map $(PRODUCT)
	cat $< | ./$(PRODUCT) > $@
