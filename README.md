# dbt-mode

Edit [dbt](https://www.getdbt.com/) projects in Emacs: SQL with Jinja,
navigation across models, sources, macros and docs, previews of rows,
lineage, and the full language server of dbt 2 through
[lsp-mode](https://emacs-lsp.github.io/lsp-mode/).

`dbt-mode` is `sql-mode` that also reads Jinja. A `.sql` file below a
`dbt_project.yml` opens in it, unless it lies in `target/` or `logs/`, where
dbt writes plain SQL. It keeps every key of `sql-mode`.

## What it does

Without any dbt executable, dbt-mode reads the project's files:

- Jinja's `{# #}` comments are comments, apostrophes included, and the tags
  `{{ }}` and `{% %}` show their delimiters, keywords, dbt's functions,
  packages and macro calls, over several lines too;
- `M-.` on `ref('…')`, `source('…', '…')`, `doc('…')` or a macro call,
  `dbt_utils.star(…)` included, goes to its definition, and `M-?` lists where
  it is used, YAML entries included;
- completion of the arguments of `ref`, `source` and `doc`, and of macros in
  a tag;
- `imenu` lists the CTEs, and the macros, tests, snapshots and docs blocks;
- the lineage of a model, from its `ref` and `source` calls.

With dbt 2 (`dbt lsp`), `dbt-lsp-deferred` starts the server through
lsp-mode, which then gives:

- definitions and references of models, sources and macros, packages' and
  dbt's own included;
- hover with a model's description, parents, children and columns;
- completion of models, macros and SQL functions;
- diagnostics as you type, such as a `ref` to a model that does not exist;
- semantic tokens, and a "Preview CTE" code lens above each CTE;
- renaming a model renames every `ref` and YAML entry that names it, whether
  by `dbt-lsp-rename-model`, Dired or `rename-visited-file`;
- the compiled SQL, the rows of a model, a region or a CTE, and the lineage
  as the server computes it.

Column-level features (column lineage, column definitions and renames)
need a dbt account: without one, the server says nothing about columns.

## Keys

| Key           | Command                | What it does                                                     |
|---------------|------------------------|------------------------------------------------------------------|
| `C-c C-d`     | `dbt-dispatch`         | Run a dbt command, by default on the current model               |
| `C-c C-v`     | `dbt-visit-compiled`   | The compiled SQL of the model                                    |
| `C-c C-p`     | `dbt-preview`          | The rows of the model, or of the region; with `C-u`, of the CTE  |
| `C-c C-g`     | `dbt-lineage`          | The upstream and downstream of a model                           |
| `C-c C-y`     | `dbt-visit-properties` | The YAML entry of the model                                      |
| `C-c C-f`     | `dbt-find-resource`    | Go to any model, seed, snapshot, macro, docs block or source     |
| `C-c C-e`     | `dbt-insert-ref`       | Insert a `ref`, read with completion                             |
| `M-.`, `M-?`  | `xref`                 | Definition and uses of what point is on                          |

`dbt-dispatch` runs `build`, `run`, `test`, `seed`, `snapshot`, `compile`,
`show`, `list`, `lint`, `format`, `parse`, `deps`, `debug`, `retry` and
`docs generate` in a compilation buffer whose errors link to their files.
Its selection defaults to the current model, and two switches add its
upstream (`+model`) or its downstream (`model+`).

In the lineage and the rows, `RET` visits a node's file and `g` asks again.

## Installation

dbt-mode needs Emacs 29.1 or later and lsp-mode 9. The language server
needs dbt 2.0 or later, the `dbt` distribution (dbt Core 1.x and `dbt-oss`
have no `dbt lsp`); it was tested with dbt 2.0.9.

With [straight.el](https://github.com/radian-software/straight.el) and
`use-package`:

```elisp
(use-package dbt-mode
  :straight (dbt-mode :type git :host github :repo "piotr-yuxuan/dbt-mode")
  :hook ((dbt-mode . dbt-lsp-deferred)
         ((yaml-mode yaml-ts-mode) . dbt-properties-mode-maybe)))
```

`dbt-lsp-deferred` makes the root of the dbt project a folder of the lsp
session before it starts the server, so that the server runs on the
project, not on an enclosing repository. `dbt-properties-mode-maybe` turns
on `M-.`, `M-?` and completion in the YAML files of a dbt project.

## Options

| Option                         | Default | Meaning                                                         |
|--------------------------------|---------|-----------------------------------------------------------------|
| `dbt-executable`               | `"dbt"` | The dbt executable                                              |
| `dbt-lsp-arguments`            | `nil`   | More arguments of `dbt lsp`, such as `("--profiles-dir" "dir")` |
| `dbt-lsp-enable-linter`        | `nil`   | Whether the server lints the SQL with dbt's linter              |
| `dbt-preview-limit`            | `50`    | The rows that a preview shows                                   |
| `dbt-project-index-lifetime`   | `30`    | Seconds before the project's files are read again               |
| `dbt-mode-jinja-max-tag-length`| `4000`  | The longest Jinja tag that font lock looks across               |

lsp-mode's own options apply: `lsp-lens-enable` shows the "Preview CTE"
lenses, `lsp-semantic-tokens-enable` the server's semantic tokens.

## Formatting

`dbt lsp` 2.0.9 formats the file on disk rather than the buffer, so its edits
would corrupt unsaved changes. dbt-mode hides formatting from lsp-mode;
`dbt-lsp-format-buffer` saves the buffer first, then formats it, and
`dbt-dispatch` runs `dbt format`.

## Limitations

- Indentation is that of `sql-mode`, which has none of its own; the
  [sql-indent](https://elpa.gnu.org/packages/sql-indent.html) package works
  with dbt-mode.
- The server gives the YAML files nothing in dbt 2.0.9; dbt-mode's own
  navigation and completion work there.
- Without a server, previewing a CTE runs its Jinja as an inline query, so
  a CTE that uses a model's context, such as `is_incremental()`, needs the
  server, which previews the compiled SQL.
- Only lsp-mode is supported, not Eglot.

## Tests

`make check` in `nix develop` runs the lints and the tests. With
`DBT_MODE_TEST_DBT` naming a dbt 2 executable, the tests also run the real
`dbt lsp` on the fixture project, `tests/fixtures/shop`, through lsp-mode.

## Licence

European Union Public Licence 1.2 (`LICENSE`) or GNU General Public
License 3.0 or later (`COPYING`).
