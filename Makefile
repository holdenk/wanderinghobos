CSCFLAGS = -O2
CSC = csc $(CSCFLAGS)

SOURCES = $(shell find src/ -iname \*.scm -or -iname \*.ss)
NONEXEC_SOURCES = $(filter-out src/main.scm src/test-runner.scm, $(SOURCES))
TEST_FILES = $(shell find tests/ -iname \*.map)

PRODUCT = lifter

STATIC = install PACKAGES-TESTING README Makefile

TEST_CASES = contest1

TEAM_NUM = 0

all: $(PRODUCT) test

$(PRODUCT): $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/main.o
	$(CSC) -o $@ $^

package:
	vagrant up
	vagrant ssh -c "cd src; make clean lifter"
	tar czf icfp-$(TEAM_NUM).tgz $(PRODUCT) $(SOURCES) $(STATIC)

test: $(PRODUCT) $(addprefix test-results/, $(addsuffix .result, $(TEST_CASES))) test-results/simulate-tests

test-runner: $(addsuffix .o, $(basename $(NONEXEC_SOURCES))) src/test-runner.o
	$(CSC) -o $@ $^

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

test-results/simulate-tests: $(TEST_FILES) test-runner
	mkdir -p test-results
	./test-runner $(basename $(TEST_FILES)) | tee test-results/simulate-tests
