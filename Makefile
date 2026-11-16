# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later
#
# The checks of dbt-mode.  Run them from the repository's root in
# `nix develop', which provides every tool, or with DEPS naming the
# directories of lsp-mode and its dependencies.  Every test and lint run
# happens in a scratch home.  DBT_MODE_TEST_DBT, when it names a dbt 2
# executable, runs the tests against the real dbt too, and against its
# dbt lsp when it is dbt 2's proprietary build, which dbt-oss is not.
# DBT_MODE_TEST_DBT_CACHE, a cache of dbt's database drivers such as
# ~/.cache/com.getdbt, is copied into the scratch home of the tests, so
# that dbt downloads none.

EMACS ?= emacs
DEPS ?=
SCRATCH := scripts/with-scratch-home
PACKAGE_EL := dbt-project.el dbt-edit.el dbt-lineage.el dbt-rows.el dbt-cli.el dbt-mode.el dbt-compiled.el dbt-new.el dbt-babel.el dbt-dashboard.el dbt-lsp.el dbt-consult.el dbt-embark.el
TOOL_EL := $(wildcard scripts/*.el tests/helpers/*.el) tests/run-ert.el
LOAD := -L . $(addprefix -L ,$(DEPS))
# The value $(1), quoted for the shell.
quote = '$(subst ','\'',$(1))'
# Without a dbt 2, the live tests skip for this reason only; with
# dbt-oss, the tests of its server do, for theirs, which dbt is asked
# in a scratch home.  Only the tests expand it, as asking takes time.
# The question gives up after 10 seconds, as dbt-project--probe-seconds
# does in Emacs, where timeout exists; without it, as on macOS outside
# nix, it has no deadline, rather than let a missing timeout skip the
# server's tests.
SKIP = $(if $(DBT_MODE_TEST_DBT),$(shell $(SCRATCH) sh -c 't=; command -v timeout >/dev/null 2>&1 && t="timeout -k 1 10"; $$t "$$0" lsp --help >/dev/null 2>&1 || echo "no dbt lsp in this dbt"' $(call quote,$(DBT_MODE_TEST_DBT))),no dbt 2 executable)

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
	@DBT_MODE_TEST_ALLOW_SKIP="$(SKIP)" \
	  $(SCRATCH) $(if $(DBT_MODE_TEST_DBT_CACHE),--dbt-cache $(call quote,$(DBT_MODE_TEST_DBT_CACHE))) \
	  $(EMACS) -Q --batch $(LOAD) -L tests/helpers -l tests/run-ert.el tests
