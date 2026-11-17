# Working in this repository

This repository holds the Emacs package `dbt-mode`, a major mode for the
SQL and Jinja of dbt projects with a client of `dbt lsp` for lsp-mode.
`README.md` and `docs/` describe it for users.

## Workflow

Every change follows the skill `workflow:plans`; load it before the first
edit. In this repository: plans are `docs/work/plans/NNN-slug.md`, not
tracked; the check is `make check`, run in `nix develop`, and, before a
merge, `make test` with `DBT_MODE_TEST_DBT` naming each build of dbt 2 at
hand in turn, dbt-oss and the proprietary one, so that the live tests run
against both, and against the real server with the proprietary one.

## Layout

```
dbt-project.el             the project model: root, configuration, index, ref graph; no dbt needed
dbt-mode.el                the major mode: Jinja syntax and font lock, xref, completion, imenu, commands
dbt-edit.el                typing and moving: Jinja pairs and blocks, indentation, CTEs as defuns, expreg's regions
dbt-new.el                 new models, macros, snapshots, YAML entries and unit tests
dbt-lineage.el             the tree of a model's upstream and downstream
dbt-rows.el                the grid of a query's rows, whoever ran the query
dbt-compiled.el            the compiled SQL beside its model, and its diff with another state
dbt-babel.el               Org Babel blocks of dbt
dbt-dashboard.el           the state of a project, from the artifacts of its runs
dbt-node.el                what dbt knows of a node, in a buffer of its own
dbt-consult.el             the resources of a project, read with consult
dbt-embark.el              embark's actions on a resource
dbt-cli.el                 dbt-dispatch, dbt's commands in a compilation buffer
dbt-lsp.el                 the lsp-mode client of dbt lsp and its custom commands
tests/run-ert.el           strict ERT runner: a skip must have an allowed reason
tests/helpers/             what the tests share
tests/dbt-mode/            the tests; dbt-lsp-test.el runs the lsp client offline, *-live-test.el against the real dbt lsp
tests/fixtures/shop/       the fixture dbt project, on DuckDB
tests/bin/dbt              a stand-in for dbt; for `dbt lsp' it runs fake_dbt_lsp.py, a scripted server
tests/bin/fake-gh          a stand-in for `gh api', for the tests of scripts/release.py
tests/scripts/             tests of scripts/
scripts/with-scratch-home  runs a command in a scratch home
scripts/lint.el            byte-compile and checkdoc, one file per Emacs
scripts/complexity.el      cyclomatic complexity of each definition, at most 10; each binding of a *-let* form counts
scripts/check-public       what must not be published, or must be there, in the tracked files
scripts/check-commits      the titles and messages of the commits since a base
scripts/public-patterns.sh the private text that both checks look for
scripts/release.py         the release, run by CD
.github/workflows/         CI, the checks on every push, and CD, a release after each green push to main
.version                   the last version released, written by the release alone
docs/tutorials/            lessons: first-project.md, the fixture from a clone
docs/how-to/               install, format and fix, run the tests, troubleshoot
docs/reference/            keys, commands, options, limitations, the release pipeline
docs/explanation/          how dbt-mode reads a project without dbt, and what the server does
flake.nix                  the development shell
```

`docs/social-media-preview.png`, 1280 by 640, the repository's picture for
social media, is made from an HTML source kept in the untracked
`docs/work/social-preview/`, rasterised by headless Chromium; GitHub takes
it under Settings, Social preview. It shows a stylised Emacs frame drawn
for it, and no mark of dbt Labs, of Emacs or of anyone else.

## What the server does

The protocol of `dbt lsp` is not documented. What dbt-lsp.el relies on was
observed on dbt 2.0.9, and the live tests check it: the custom commands
`dbt.compileFile`, `dbt.show`, `dbt.listNodes` and the code lens
`dbt.previewCte`, the notifications `dbt/*`, file renames through
`workspace/willRenameFiles`, and that formatting, the linter's fix-all,
`dbt.compileFile` and `dbt.show` with a uri all read the file on disk.
`dbt.show` answers no columns for a macro. lsp-mode reads JSON false as
nil.
