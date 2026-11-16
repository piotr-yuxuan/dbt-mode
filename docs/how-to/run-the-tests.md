# Run the tests

`make check` in `nix develop` runs the lints and the tests; the lsp client
is tested offline through a scripted server, `tests/bin/fake_dbt_lsp.py`. With
`DBT_MODE_TEST_DBT` naming a dbt 2 executable, the tests also run the real
dbt on the fixture project, `tests/fixtures/shop`, and, when it is the
proprietary build, its `dbt lsp` through lsp-mode. `DBT_MODE_TEST_DBT_CACHE`,
naming a cache of dbt's database drivers such as `~/.cache/com.getdbt`, is
copied into the scratch home, so that dbt downloads no driver. The tests
of `dbt-consult-resource` and of embark's actions use the consult and
embark of the development shell.

`make check` also runs the tests of the scripts, `make test-python`, the
check of public text, `make public`, and actionlint on the workflows,
`make workflows`; `make commits BASE=main` checks the commit messages
since `main`. CI runs `make check` without a dbt, so without the live
tests, and `make commits` on a branch other than `main`: see [the
release pipeline](../reference/release-pipeline.md).
