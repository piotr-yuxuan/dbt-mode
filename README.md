# dbt-mode

[![CI](https://github.com/piotr-yuxuan/dbt-mode/actions/workflows/ci.yml/badge.svg)](https://github.com/piotr-yuxuan/dbt-mode/actions/workflows/ci.yml)
[![CD](https://github.com/piotr-yuxuan/dbt-mode/actions/workflows/cd.yml/badge.svg)](https://github.com/piotr-yuxuan/dbt-mode/actions/workflows/cd.yml)
[![Release](https://img.shields.io/github/v/release/piotr-yuxuan/dbt-mode?sort=semver)](https://github.com/piotr-yuxuan/dbt-mode/releases)
[![Licence](https://img.shields.io/github/license/piotr-yuxuan/dbt-mode)](https://github.com/piotr-yuxuan/dbt-mode#licence)
[![Issues](https://img.shields.io/github/issues/piotr-yuxuan/dbt-mode)](https://github.com/piotr-yuxuan/dbt-mode/issues)
[![Emacs 31.1+](https://img.shields.io/badge/Emacs-31.1%2B-7f5ab6)](https://www.gnu.org/software/emacs/)

Edit [dbt](https://www.getdbt.com/) projects in Emacs: SQL with Jinja,
navigation across models, sources, macros and docs, previews of rows,
lineage, and, with dbt 2's proprietary build, its language server through
[lsp-mode](https://emacs-lsp.github.io/lsp-mode/). It is for those who
write dbt models in Emacs, whichever dbt they have: much of it reads the
project's files and needs no dbt at all. It is pre-release, tested with
`dbt-oss` 2.0.11 and the proprietary 2.0.9.

`dbt-mode` is `sql-mode` that also reads Jinja. A `.sql` file below a
`dbt_project.yml` opens in it, unless it lies under the project's
`target-path` (`target/` by default) or `logs/`, where dbt writes plain SQL.
It keeps the keys of `sql-mode`, but for `C-M-a` and `C-M-e`, which move by
CTE rather than by statement.

## Install

dbt-mode needs Emacs 31.1 or later and lsp-mode 9, and works with the one
dbt you have. With [straight.el](https://github.com/radian-software/straight.el) and
`use-package`:

```elisp
(use-package dbt-mode
  :straight (dbt-mode :type git :host github :repo "piotr-yuxuan/dbt-mode")
  :hook ((dbt-mode . dbt-lsp-deferred)
         ((yaml-mode yaml-ts-mode) . dbt-properties-mode-maybe)))
```

[Your first project](docs/tutorials/first-project.md) walks through it
from a clone, with nothing installed, and [Install](docs/how-to/install.md)
gives the rest: expreg, embark and consult, the server, dbt-lsp-plus.

## Documentation

**Tutorial**

- [Your first project](docs/tutorials/first-project.md): from a clone to the rows of a model.

**How-to guides**

- [Install](docs/how-to/install.md): which dbt does what, and dbt-mode with expreg, embark, consult and dbt-lsp-plus.
- [Format and fix](docs/how-to/format-and-fix.md): with the server or `dbt format`, on save or by hand.
- [Run the tests](docs/how-to/run-the-tests.md): offline, or against a real dbt.

**Reference**

- [Keys](docs/reference/keys.md): every key, `dbt-dispatch`, the mode line, the lineage and the rows.
- [Commands](docs/reference/commands.md): writing and moving, rows and compiled SQL, a node, the dashboard.
- [Options](docs/reference/options.md): every option, with its default.
- [Limitations](docs/reference/limitations.md): what dbt-mode does not do.
- [The release pipeline](docs/reference/release-pipeline.md): the two workflows, what a release writes, how to raise a version by hand.

**Explanation**

- [How dbt-mode reads a project without dbt](docs/explanation/reading-a-project.md): navigation, completion and the index, as dbt resolves them.
- [What the server does](docs/explanation/the-server.md): `dbt lsp`, dbt-lsp-plus, and what dbt-mode relies on.

## Licence

European Union Public Licence 1.2 (`LICENSE`) or GNU General Public
License 3.0 or later (`COPYING`).
