# dbt-mode

Edit [dbt](https://www.getdbt.com/) projects in Emacs: SQL with Jinja,
navigation across models, sources, macros and docs, previews of rows,
lineage, and the full language server of dbt 2 through
[lsp-mode](https://emacs-lsp.github.io/lsp-mode/).

`dbt-mode` is `sql-mode` that also reads Jinja. A `.sql` file below a
`dbt_project.yml` opens in it, unless it lies under the project's
`target-path` (`target/` by default) or `logs/`, where dbt writes plain SQL.
It keeps the keys of `sql-mode`, but for `C-M-a` and `C-M-e`, which move by
CTE rather than by statement.

## What it does

Without any dbt executable, dbt-mode reads the project's files:

- Jinja's `{# #}` comments are comments, apostrophes included, and the tags
  `{{ }}` and `{% %}` show their delimiters, keywords, dbt's functions,
  packages and macro calls, over several lines too; a string inside a tag
  is a string, so the apostrophe of `"doesn't"` opens nothing;
- `M-.` on `ref('…')`, `source('…', '…')`, `doc('…')` or a macro call,
  `dbt_utils.star(…)` included, goes to its definition, and on
  `ref('m', v=2)` to that version; `M-?` lists the calls that resolve to the
  same definition, so a package's model and the project's namesake stay
  apart, and the YAML entries of a model;
- completion of the arguments of `ref`, `source` and `doc`, and of macros in
  a tag;
- `imenu` lists the CTEs, and the macros, tests, snapshots and docs blocks;
- the lineage of a model, from its `ref` and `source` calls.

Writing and moving:

- typing `{` then `{`, `%` or `#` gives `{{ | }}`, `{% | %}` or `{# | #}`,
  point in the middle, when only blanks, a closing bracket or a comma
  follow, with or without `electric-pair-mode`. A `-` or `+` typed next
  sticks to the opener, `{%- | %}`; a tag typed in full out of habit, its
  spaces and its closer included, comes out as typed, but for a space too
  many before the closer with `electric-pair-mode`; `DEL` deletes an empty
  pair.
  `dbt-edit-pair-jinja` turns all of it off;
- `C-c /` closes the innermost open Jinja block, `{% endif %}` and the like,
  and `C-c %` moves between a block's opening and closing tags;
- CTEs are the defuns of a model: `C-M-a`, `C-M-e`, `C-M-h` and `C-x n d`
  work on them; with [expreg](https://elpa.gnu.org/packages/expreg.html),
  `expreg-expand` also selects the Jinja tag and its contents, the block and
  the CTE around point, once `dbt-edit-expreg-regions` is in
  `expreg-functions` (see Installation);
- `C-c C-a` writes a new staging model from a source table, as dbt Labs'
  style guide lays it out with its YAML entry, a model, an incremental
  model, a macro, a snapshot defined in YAML, the YAML entry of the current
  model, seed, snapshot or analysis, or a unit test of the model, named
  `test_MODEL` by default, with an input for each model and source it
  reads. An entry joins its list as the file writes its items, and a name
  that the list has already is refused.

Rows and compiled SQL:

- `C-c C-p` shows the rows of the model, of the region, or, with `C-u`, of
  the CTE around point, in one grid, whether dbt lsp or `dbt show` runs the
  query: each row on one line, `NULL` in its own face, a column of numbers
  sorted as numbers. `/` keeps the rows with a cell that matches a regexp
  (with `C-u`, in one column), `w` copies the rows shown as CSV, Markdown
  or Org, `+` asks again with twice the limit, or a numeric prefix's, as
  dbt has no offset, `g` asks again, and `RET` shows a cell's whole value,
  in a buffer of its own when it is long or holds a newline. The grid of a
  CTE or a region is named after its file, as `*dbt rows: orders/final*`.
  Two columns of one name, as a join gives, show apart, the second renamed
  `id_1` as dbt lsp does. Without a server, a number shows as JSON reads
  it: `12.30` as `12.3`. Asked again, a CTE is taken from the model as it
  is now;
- `C-c C-v` shows the compiled SQL beside the model and keeps it there:
  point's line in it faces point's line in the model, CTE by CTE, and
  saving compiles it again with dbt lsp; without a server, it is read again
  when dbt writes it. `dbt-compiled-diff` shows how it differs from the
  compiled SQL of another state, such as that of `dbt compile --target-path
  STATE` on main, or production's artifacts;
- in Org, `C-c C-c` on a `#+begin_src dbt` block gives its rows as a table,
  or its compiled SQL with `:compile yes`; `:limit`, `:target`, `:var`,
  passed as dbt's `--vars`, and Babel's `:dir` apply; each cell is on one
  line of the table. A block on a target of `dbt-protected-targets`, `prod`
  by default, asks first: the target is `:target`, else `DBT_TARGET`, else
  the profile's default, read from a call of `env_var` as dbt reads it. A
  default that dbt-mode cannot read, such as other Jinja, asks too while a
  target is protected.

The index knows versioned models, snapshots defined in YAML, flow-style
YAML, packages under `packages-install-path` and local packages, and leaves
`{# #}` comments out. A project nested in another, such as a package's
`integration_tests`, is its own project; a package that a project installs
or declares belongs to that project. Saving re-reads only the files that
changed.

With dbt 2 (`dbt lsp`), `dbt-lsp-deferred` starts the server through
lsp-mode, which then gives:

- definitions of what dbt-mode does not resolve, such as dbt's own macros:
  on a `ref`, a source, a doc or a macro of the project or its packages,
  `M-.` and `M-?` stay dbt-mode's, as dbt lsp 2.0.9 lists the uses in the
  current file only, ignores a `ref`'s version and resolves no `doc`;
- hover with a model's description, parents, children and columns;
- completion of models, macros and SQL functions;
- diagnostics as you type, such as a `ref` to a model that does not exist;
- semantic tokens, and a "Preview CTE" code lens above each CTE;
- renaming a model, whether by `dbt-lsp-rename-model`, Dired or
  `rename-visited-file`, renames its `ref` calls in models and its YAML
  entry; dbt lsp 2.0.9 leaves those of analyses and YAML tests, which
  `dbt-lsp-rename-model` then lists. A version of a model is not renamed;
- the compiled SQL, the rows of a model, a region or a CTE, and the lineage
  as the server computes it. The server compiles and previews the file on
  disk, so these commands save the buffer first.

Column-level features (column lineage, column definitions and renames)
need a dbt account: without one, the server says nothing about columns.

## Keys

| Key           | Command                | What it does                                                     |
|---------------|------------------------|------------------------------------------------------------------|
| `C-c C-d`     | `dbt-dispatch`         | Run a dbt command, by default on the current model               |
| `C-c C-v`     | `dbt-visit-compiled`   | The compiled SQL beside the model, facing point; with `C-u`, the SQL of the last run (`dbt-visit-run`) |
| `C-c C-p`     | `dbt-preview`          | The rows of the model, or of the region; with `C-u`, of the CTE  |
| `C-c C-g`     | `dbt-lineage`          | The upstream and downstream of a model                           |
| `C-c C-y`     | `dbt-visit-properties` | The YAML entry of the model                                      |
| `C-c C-f`     | `dbt-find-resource`    | Go to any model, seed, snapshot, analysis, macro, docs block or source |
| `C-c C-e`     | `dbt-insert-ref`       | Insert a `ref`, read with completion                             |
| `C-c C-t`     | `dbt-visit-tests`      | The tests of the model: its YAML entries, the data tests that name it, its singular tests; not its unit tests |
| `C-c C-w`     | `dbt-copy-relation`    | Copy the relation of the model, or of the `ref` or `source` at point, from `target/manifest.json` |
| `C-c M-w`     | `dbt-copy-build-command` | Copy `dbt build -s` of what point is on, a `ref` or a `source`, else the model; with `C-u`, `+…+` |
| `C-c C-a`     | `dbt-new`              | Write a new model, macro, snapshot, YAML entry or unit test      |
| `C-c /`       | `dbt-close-block`      | Close the innermost open Jinja block                             |
| `C-c %`       | `dbt-jump-to-matching-tag` | Move between a block's opening and closing tags              |
| `M-.`, `M-?`  | `xref`                 | Definition and uses of what point is on                          |

`dbt-dispatch` runs `build`, `run`, `test`, `seed`, `snapshot`, `compile`,
`show`, `list`, `lint`, `format`, `parse`, `deps`, `debug`, `retry` and
`docs generate` in a compilation buffer whose errors link to their files.
Its selection defaults to what point is on, a `ref` or a `source` as dbt
selects it (`orders.v2`, `source:raw.orders`), else the current model, else
the file by path (`path:tests/assert_positive_amount.sql`) for a file that
holds no model, such as a singular test or a properties file; two switches
add its upstream (`+model`) or its downstream (`model+`), and "Changed on the
branch" selects instead, by path, each SQL, Python, YAML or CSV file that git
sees changed since the merge base with the default branch (`origin/HEAD`,
else `main`), uncommitted, unsaved and new ones included: dbt selects what a
file defines, and what a properties file describes and tests; a macro, by
path, selects nothing, which `state:modified` with `--state` covers. Each command
gets only the options dbt takes for it: `--full-refresh` goes to `build`,
`run`, `compile` and `seed`, the limit to `build`, `run`, `test`, `compile`,
`show` and `list`, `--defer`, `--state` and `--vars` to all but `docs
generate`. `.` runs the project's last dbt command again, which the menu
shows, and transient's
`C-M-p` and `C-M-n` recall earlier options. dbt runs without colours or
progress bars, whatever `TERM` Emacs passes on, and the project's modified
buffers are offered for saving first, as `compile` does.

The mode line shows the target that dbt runs on and the model's
materialization, from its `config()` call or else the manifest of your
runs, and `compiling` while dbt lsp compiles.  It is computed when the
buffer opens and at each save, and reads the manifest only for a model
without `config()`.

The lineage is a tree, upstream above and downstream below: each node shows
once, under the node through which it is nearest. `TAB` folds a node, `+`
and `-` show one level more or less, point staying on its node, `e` writes
the graph within that depth as Graphviz dot, each node named by its unique
id, and an SVG beside it when `dot` is installed, rendered while you go on,
and `RET` visits a node's file. In the lineage and the rows, `g` asks again, and
without a server the lineage reads the project again.
The rows show nested values as JSON and newlines as `\n`; lsp-mode reads
JSON `false` as null, so it shows as `NULL` with dbt lsp.

## Installation

dbt-mode needs Emacs 31.1 or later and lsp-mode 9. The language server
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

With expreg, the regions of dbt-mode join its own:

```elisp
(use-package expreg
  :config
  ;; expreg-functions is local to each buffer once set: change its default.
  (setq-default expreg-functions
                (cons #'dbt-edit-expreg-regions
                      (remq #'dbt-edit-expreg-regions (default-value 'expreg-functions)))))
```

`dbt-lsp-deferred` makes the root of the dbt project a folder of the lsp
session before it starts the server, so that the server runs on the
project, not on an enclosing repository, and each dbt project of a
repository gets its own server, whatever `lsp-auto-guess-root` says. It
adds the folder only once `dbt-executable` is known to run `dbt lsp`, which
it asks once per executable, within ten seconds. `dbt-properties-mode-maybe` turns
on `M-.`, `M-?` and completion in the YAML files of a dbt project.

## Options

| Option                         | Default | Meaning                                                         |
|--------------------------------|---------|-----------------------------------------------------------------|
| `dbt-executable`               | `"dbt"` | The dbt executable                                              |
| `dbt-lsp-arguments`            | `nil`   | More arguments of `dbt lsp`, such as `("--profiles-dir" "dir")` |
| `dbt-lsp-enable-linter`        | `nil`   | Whether the server lints the SQL with dbt's linter              |
| `dbt-lsp-proxy`                | `nil`   | Whether dbt lsp runs through dbt-lsp-plus, when it is installed; a request on a source that it lists goes to it |
| `dbt-preview-limit`            | `50`    | The rows that a preview shows                                   |
| `dbt-protected-targets`        | `("prod")` | The targets on which an Org block asks before it runs; an unread default asks too |
| `dbt-project-index-lifetime`   | `30`    | Seconds before the project's files are read again               |
| `dbt-mode-jinja-max-tag-length`| `4000`  | The longest Jinja tag that font lock looks across               |

lsp-mode's own options apply: `lsp-lens-enable` shows the "Preview CTE"
lenses, `lsp-semantic-tokens-enable` the server's semantic tokens.

## Formatting and fixes

`dbt lsp` 2.0.9 formats the file on disk rather than the buffer, and so does
its linter's fix-all code action: their edits would corrupt unsaved changes.
dbt-mode hides both from lsp-mode. `dbt-lsp-format-buffer` saves the buffer
first, then formats it; `dbt-lsp-fix-all` saves, then fixes and formats,
whether or not `dbt-lsp-enable-linter` is on; `dbt-dispatch` runs `dbt
format` and `dbt lint`. lsp-mode's `lsp-format-buffer-on-save` and
`lsp-fix-all-on-save` are turned off in the buffers of dbt lsp, where they
could only fail.

`dbt-lsp-rename-model` refuses a name that another model, seed or snapshot
of the package already has.

## Limitations

- Indentation is that of `sql-mode`, which has none of its own; the
  [sql-indent](https://elpa.gnu.org/packages/sql-indent.html) package works
  with dbt-mode.
- The server gives the YAML files nothing in dbt 2.0.9; dbt-mode's own
  navigation and completion work there.
- Without a server, previewing a CTE runs its Jinja as an inline query, with
  all that comes before its WITH, so a CTE that uses a model's context, such
  as `is_incremental()`, needs the server, which previews the compiled SQL.
- `dbt-compiled-diff` compares with a state's manifest, not with another
  git revision: dbt compiles the project on disk.
- Only lsp-mode is supported, not Eglot.

## Tests

`make check` in `nix develop` runs the lints and the tests; the lsp client
is tested offline through a scripted server, `tests/bin/fake_dbt_lsp.py`. With
`DBT_MODE_TEST_DBT` naming a dbt 2 executable, the tests also run the real
`dbt lsp` on the fixture project, `tests/fixtures/shop`, through lsp-mode.

## Licence

European Union Public Licence 1.2 (`LICENSE`) or GNU General Public
License 3.0 or later (`COPYING`).
