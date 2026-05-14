EMACS ?= emacs
REFBOX_DIR ?= ../refbox
ORG_SLIPBOX_DIR ?= ../org-slipbox
PACKAGE = refbox-org-slipbox.el
TEST_FILE = tests/test-refbox-org-slipbox.el

.PHONY: compile
compile:
	$(EMACS) --batch -Q \
		-L . \
		-L $(REFBOX_DIR) \
		-L $(ORG_SLIPBOX_DIR) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile \
		$(PACKAGE)

.PHONY: test
test:
	REFBOX_DIR="$(REFBOX_DIR)" \
	ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)" \
	$(EMACS) --batch -Q \
		-L . \
		-L $(REFBOX_DIR) \
		-L $(ORG_SLIPBOX_DIR) \
		--eval '(setq load-prefer-newer t)' \
		-l $(TEST_FILE) \
		-f ert-run-tests-batch-and-exit

.PHONY: checkdoc
checkdoc:
	$(EMACS) --batch -Q \
		-L . \
		-L $(REFBOX_DIR) \
		-L $(ORG_SLIPBOX_DIR) \
		--eval "(progn (require 'checkdoc) (checkdoc-file \"$(PACKAGE)\"))"

.PHONY: check
check:
	$(MAKE) compile REFBOX_DIR="$(REFBOX_DIR)" ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)"
	$(MAKE) test REFBOX_DIR="$(REFBOX_DIR)" ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)"
	$(MAKE) checkdoc REFBOX_DIR="$(REFBOX_DIR)" ORG_SLIPBOX_DIR="$(ORG_SLIPBOX_DIR)"
