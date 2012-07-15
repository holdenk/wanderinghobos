CSCFLAGS = -O2 
# -O4 -optimize-leaf-routines -u -unboxing  -no-argc-checks -no-bound-checks -no-procedure-checks -local
CSC = csc $(CSCFLAGS)

IGNORES = src/main.scm src/test-runner.scm src/loadme.scm src/play-map.scm
SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)
NONEXEC_SOURCES = $(filter-out $(IGNORES), $(SOURCES))
TEST_FILES = $(shell find tests/ -iname \*.map)

PRODUCT = lifter

STATIC = install PACKAGES-TESTING README Makefile CHICKEN-EGGS

TEST_CASES = contest1

TEAM_NUM = 96693622

all: $(PRODUCT) play-map test

$(PRODUCT): $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/main.o
	$(CSC) -o $@ $^

play-map: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/play-map.o
	$(CSC) -o $@ $^

package:
	vagrant up
	vagrant ssh -c "cd src; make clean lifter"
	tar czf icfp-$(TEAM_NUM).tgz $(PRODUCT) $(SOURCES) $(STATIC)

test: $(PRODUCT) $(addprefix test-results/, $(addsuffix .result, $(TEST_CASES))) test-results/simulate-tests

test-runner: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/test-runner.o
	$(CSC) -o $@ $^

clean:
	rm -f lifter test-runner play-map src/*.o icfp-*.tgz
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
	cat $< | ./$(PRODUCT) -iamohsopretty -localmode> $@

test-results/simulate-tests: $(TEST_FILES) test-runner
	mkdir -p test-results
	./test-runner $(basename $(TEST_FILES)) | tee test-results/simulate-tests
