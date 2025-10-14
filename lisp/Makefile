.PHONY: test test-process test-project test-utils test-create test-show clean

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L .

# Run all tests
test:
	@echo "Running all beads.el tests..."
	$(BATCH) -l beads.el \
		-l beads-create.el \
		-l beads-show.el \
		-l test/beads-process-test.el \
		-l test/beads-project-test.el \
		-l test/beads-utils-test.el \
		-l test/beads-transient-create-test.el \
		-l test/beads-show-test.el \
		-f ert-run-tests-batch-and-exit

# Run individual test suites
test-process:
	@echo "Running process tests..."
	$(BATCH) -l beads.el -l test/beads-process-test.el -f ert-run-tests-batch-and-exit

test-project:
	@echo "Running project tests..."
	$(BATCH) -l beads.el -l test/beads-project-test.el -f ert-run-tests-batch-and-exit

test-utils:
	@echo "Running utils tests..."
	$(BATCH) -l beads.el -l test/beads-utils-test.el -f ert-run-tests-batch-and-exit

test-create:
	@echo "Running create transient tests..."
	$(BATCH) -l beads.el -l beads-create.el -l test/beads-transient-create-test.el -f ert-run-tests-batch-and-exit

test-show:
	@echo "Running show mode tests..."
	$(BATCH) -l beads.el -l beads-show.el -l test/beads-show-test.el -f ert-run-tests-batch-and-exit

# Interactive test mode
test-interactive:
	$(EMACS) -Q -L . -l beads.el \
		-l beads-create.el \
		-l beads-show.el \
		-l test/beads-process-test.el \
		-l test/beads-project-test.el \
		-l test/beads-utils-test.el \
		-l test/beads-transient-create-test.el \
		-l test/beads-show-test.el \
		--eval "(ert t)"

# Check for byte-compile warnings
compile:
	@echo "Byte-compiling beads.el..."
	$(BATCH) -f batch-byte-compile beads.el beads-create.el beads-show.el

# Clean compiled files
clean:
	rm -f beads.elc beads-create.elc beads-show.elc test/*.elc

# Show test statistics
test-stats:
	@echo "=== Test Statistics ==="
	@echo "Process tests: $$(grep -c 'ert-deftest' test/beads-process-test.el)"
	@echo "Project tests: $$(grep -c 'ert-deftest' test/beads-project-test.el)"
	@echo "Utils tests: $$(grep -c 'ert-deftest' test/beads-utils-test.el)"
	@echo "Create tests: $$(grep -c 'ert-deftest' test/beads-transient-create-test.el)"
	@echo "Show tests: $$(grep -c 'ert-deftest' test/beads-show-test.el)"
	@echo "Total tests: $$(grep -c 'ert-deftest' test/*.el)"
	@echo ""
	@echo "Total lines: $$(wc -l beads.el beads-create.el beads-show.el test/*.el | tail -1)"
