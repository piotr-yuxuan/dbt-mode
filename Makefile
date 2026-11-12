# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later
#
# The checks of dbt-mode.  Run them from the repository's root in
# `nix develop', which provides every tool, or with DEPS naming the
# directories of lsp-mode and its dependencies.  Every test and lint run
# happens in a scratch home.  DBT_MODE_TEST_DBT, when it names a dbt 2
# executable, runs the tests against the real dbt lsp too.

EMACS ?= emacs
DEPS ?=
SCRATCH := scripts/with-scratch-home
PACKAGE_EL := dbt-project.el dbt-lineage.el dbt-cli.el dbt-mode.el dbt-lsp.el
TOOL_EL := $(wildcard scripts/*.el tests/helpers/*.el) tests/run-ert.el
LOAD := -L . $(addprefix -L ,$(DEPS))
# Without a dbt 2, the live tests skip for this reason only.
SKIP := $(if $(DBT_MODE_TEST_DBT),,no dbt 2 executable)

.PHONY: check lint compile checkdoc package-lint relint complexity test

check: lint test

lint: compile checkdoc package-lint relint complexity

compile:
	@for file in $(PACKAGE_EL) $(TOOL_EL); do \
	  $(SCRATCH) $(EMACS) -Q --batch $(LOAD) -L tests/helpers -l scripts/lint.el \
	    -f repo-lint-compile "$$file" || exit 1; \
	done

checkdoc:
	@for file in $(PACKAGE_EL); do \
	  $(SCRATCH) $(EMACS) -Q --batch -l scripts/lint.el -f repo-lint-checkdoc "$$file" || exit 1; \
	done

package-lint:
	@$(SCRATCH) $(EMACS) -Q --batch $(LOAD) -l package-lint \
	  --eval "(setq package-lint-main-file \"dbt-mode.el\")" \
	  --eval "(setq package-lint-batch-fail-on-warnings t)" \
	  -f package-lint-batch-and-exit $(PACKAGE_EL)

relint:
	@$(SCRATCH) $(EMACS) -Q --batch $(LOAD) -l relint -f relint-batch $(PACKAGE_EL)

complexity:
	@$(EMACS) -Q --batch -l scripts/complexity.el -f repo-complexity-batch $(PACKAGE_EL) $(TOOL_EL) \
	  $(wildcard tests/*/*-test.el)

test:
	@DBT_MODE_TEST_ALLOW_SKIP="$(SKIP)" $(SCRATCH) $(EMACS) -Q --batch $(LOAD) -L tests/helpers \
	  -l tests/run-ert.el tests
