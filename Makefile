CSCFLAGS = -O2 -uses parse-input -uses pairing-heap
CSC = csc $(CSCFLAGS)

SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)

PRODUCT = lifter

STATIC = install PACKAGES-TESTING README Makefile

TEST_CASES = contest1

TEAM_NUM = 0

all: $(PRODUCT) test

$(PRODUCT): $(addsuffix .o, $(basename $(SOURCES)))
	$(CSC) -o $@ $^

package:
	vagrant up
	vagrant ssh -c "cd src; make clean lifter"
	tar czf icfp-$(TEAM_NUM).tgz $(PRODUCT) $(SOURCES) $(STATIC)

test: $(PRODUCT) $(addprefix test-results/, $(addsuffix .result, $(TEST_CASES)))

clean:
	rm -f lifter src/*.o icfp-*.tgz
	rm -rf test-results

.PHONY: all clean test package
.SILENT:

%.o: %.scm
	$(CSC) -c $<

%.o: %.ss
	$(CSC) -c $<

#### TEST CASES ####

test-results/%.result: tests/%.map $(PRODUCT)
	mkdir -p test-results
	cat $< | ./$(PRODUCT) > $@
