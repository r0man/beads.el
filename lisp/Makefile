.PHONY: test test-process test-project test-utils test-list test-create test-show test-update test-main test-misc clean

EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L .

# Run all tests
test:
	@echo "Running all beads.el tests..."
	$(BATCH) -l beads.el \
		-l beads-list.el \
		-l beads-create.el \
		-l beads-update.el \
		-l beads-show.el \
		-l beads-main.el \
		-l beads-misc.el \
		-l test/beads-process-test.el \
		-l test/beads-project-test.el \
		-l test/beads-utils-test.el \
		-l test/beads-list-test.el \
		-l test/beads-transient-create-test.el \
		-l test/beads-transient-update-test.el \
		-l test/beads-show-test.el \
		-l test/beads-main-test.el \
		-l test/beads-misc-test.el \
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

test-list:
	@echo "Running list mode tests..."
	$(BATCH) -l beads.el -l beads-list.el -l test/beads-list-test.el -f ert-run-tests-batch-and-exit

test-create:
	@echo "Running create transient tests..."
	$(BATCH) -l beads.el -l beads-create.el -l test/beads-transient-create-test.el -f ert-run-tests-batch-and-exit

test-show:
	@echo "Running show mode tests..."
	$(BATCH) -l beads.el -l beads-show.el -l test/beads-show-test.el -f ert-run-tests-batch-and-exit

test-update:
	@echo "Running update transient tests..."
	$(BATCH) -l beads.el -l beads-list.el -l beads-show.el -l beads-update.el -l test/beads-transient-update-test.el -f ert-run-tests-batch-and-exit

test-main:
	@echo "Running main transient menu tests..."
	$(BATCH) -l beads.el -l beads-list.el -l beads-show.el -l beads-create.el -l beads-main.el -l test/beads-main-test.el -f ert-run-tests-batch-and-exit

test-misc:
	@echo "Running misc commands tests..."
	$(BATCH) -l beads.el -l beads-misc.el -l test/beads-misc-test.el -f ert-run-tests-batch-and-exit

# Interactive test mode
test-interactive:
	$(EMACS) -Q -L . -l beads.el \
		-l beads-list.el \
		-l beads-create.el \
		-l beads-update.el \
		-l beads-show.el \
		-l beads-main.el \
		-l beads-misc.el \
		-l test/beads-process-test.el \
		-l test/beads-project-test.el \
		-l test/beads-utils-test.el \
		-l test/beads-list-test.el \
		-l test/beads-transient-create-test.el \
		-l test/beads-transient-update-test.el \
		-l test/beads-show-test.el \
		-l test/beads-main-test.el \
		-l test/beads-misc-test.el \
		--eval "(ert t)"

# Check for byte-compile warnings
compile:
	@echo "Byte-compiling beads.el..."
	$(BATCH) -f batch-byte-compile beads.el beads-list.el beads-create.el beads-update.el beads-show.el beads-main.el beads-misc.el

# Clean compiled files
clean:
	rm -f beads.elc beads-list.elc beads-create.elc beads-update.elc beads-show.elc beads-main.elc beads-misc.elc test/*.elc

# Show test statistics
test-stats:
	@echo "=== Test Statistics ==="
	@echo "Process tests: $$(grep -c 'ert-deftest' test/beads-process-test.el)"
	@echo "Project tests: $$(grep -c 'ert-deftest' test/beads-project-test.el)"
	@echo "Utils tests: $$(grep -c 'ert-deftest' test/beads-utils-test.el)"
	@echo "List tests: $$(grep -c 'ert-deftest' test/beads-list-test.el)"
	@echo "Create tests: $$(grep -c 'ert-deftest' test/beads-transient-create-test.el)"
	@echo "Update tests: $$(grep -c 'ert-deftest' test/beads-transient-update-test.el)"
	@echo "Show tests: $$(grep -c 'ert-deftest' test/beads-show-test.el)"
	@echo "Main tests: $$(grep -c 'ert-deftest' test/beads-main-test.el)"
	@echo "Misc tests: $$(grep -c 'ert-deftest' test/beads-misc-test.el)"
	@echo "Total tests: $$(grep -c 'ert-deftest' test/*.el)"
	@echo ""
	@echo "Total lines: $$(wc -l beads.el beads-list.el beads-create.el beads-update.el beads-show.el beads-main.el beads-misc.el test/*.el | tail -1)"
