# Commands

## Writing and moving

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
- `TAB` and `C-M-\` indent by what holds a line: a parenthesis or a Jinja
  block puts its contents one step, `dbt-indent-offset`, in from its line,
  and `)`, `{% endif %}` or `{% else %}` back at that line, as soon as they
  are typed; clauses sit at the level, the lines of a clause one step in,
  as in dbt Labs' style guide; a Jinja tag sits at the level of what it
  leads to, a clause or a column; a comment line sits as the code after
  it; a line that starts in a string or a comment stays put, and dbt-mode
  indents with spaces;
- CTEs are the defuns of a model: `C-M-a`, `C-M-e`, `C-M-h` and `C-x n d`
  work on them; with [expreg](https://elpa.gnu.org/packages/expreg.html),
  `expreg-expand` also selects the Jinja tag and its contents, the block and
  the CTE around point, once `dbt-edit-expreg-regions` is in
  `expreg-functions` (see [Install](../how-to/install.md));
- `dbt-breadcrumb-mode`, off by default, says in the header line where
  point is, once Emacs is idle: the directories from the project's root,
  the file, then the CTEs and the Jinja blocks that hold point, as
  `models > marts > orders.sql > final > if is_incremental()`. A macro,
  test, snapshot, materialization, docs or `set` block shows the name it
  defines, an `if`, `for` or `call` block its statement, as dbt-lsp-plus
  names them (the `if`, `for` and `call` blocks once its outline has
  them). When lsp-mode's breadcrumbs show the document symbols that the
  server announces, as the proxy does, they replace the mode's line; with
  dbt lsp alone, which announces none, lsp-mode's breadcrumbs name the
  file and the mode adds the CTEs and blocks after it.
  `(add-hook 'dbt-mode-hook #'dbt-breadcrumb-mode)` turns it on in every
  model;
- `C-c C-a` writes a new staging model from a source table, as dbt Labs'
  style guide lays it out with its YAML entry, a model, an incremental
  model, a macro, a snapshot defined in YAML, the YAML entry of the current
  model, seed, snapshot or analysis, or a unit test of the model, named
  `test_MODEL` by default, with an input for each model and source it
  reads. An entry joins its list as the file writes its items, and a name
  that the list has already is refused.

## Rows and compiled SQL

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
- `dbt-data-diff` compares the rows of the model, as last built on the
  default target, with those of its relation in another state, such as
  production's artifacts: one query on the default target gives the rows
  that each side lacks, `{{ dbt.except() }}` as dbt writes it for the
  adapter, in a first column `dbt_diff` naming the side, in the grid of the
  rows. The columns are the model's, by name, in any order on the other
  side, which must have them all and be readable from the same connection;
- in Org, `C-c C-c` on a `#+begin_src dbt` block gives its rows as a table,
  or its compiled SQL with `:compile yes`; `:limit`, `:target`, `:var`,
  passed as dbt's `--vars`, and Babel's `:dir` apply; each cell is on one
  line of the table. A block on a target of `dbt-protected-targets`, `prod`
  by default, asks first: the target is `:target`, else `DBT_TARGET`, else
  the profile's default, read from a call of `env_var` as dbt reads it. A
  default that dbt-mode cannot read, such as other Jinja, asks too while a
  target is protected. The data diff and the dashboard's freshness ask as
  a block does.

## What dbt knows of a node

`C-c C-o`, `dbt-describe`, shows what dbt knows of the model, seed,
snapshot or source table that the `ref` or `source` at point names, or
the YAML entry, else of the current resource: its description, its file
and its YAML entry, its relation, owner, materialization and its
settings, tags, group, access, contract, version, whether it is
disabled, its columns, its tests, what it depends on and what uses it.
With dbt-lsp-plus, it comes from the proxy's own `dbt parse`, with the
summary that its hover shows; otherwise from `target/manifest.json`,
saying when dbt wrote it, its tests left to the proxy and to `C-c C-t`.
What uses the node comes from dbt-mode's index, as for the lineage.
`RET` visits a file or a node, `TAB` folds a section, `g` shows the node
again. Without a server, eldoc on a `ref` or a `source` says the
materialization and the relation of what it names, in a model as in
YAML, once dbt-mode holds the project's manifest as dbt last wrote it:
after `C-c C-o`, `C-c C-w` or the dashboard in this session, or once the
mode line of a model that sets no materialization of its own read it.
eldoc never reads a manifest.

## The dashboard

The dashboard, `dbt-dashboard` or `h` in `dbt-dispatch`, shows the state of
the project from the artifacts of your own runs, which it never writes, and
which dbt-mode's previews, diffs and formats leave as they are: the nodes
that errored, failed, warned or were skipped in `target/run_results.json`,
the most severe first, with dbt's message to its cause and a test's
failing rows, the ten slowest nodes, and the sources that
`target/sources.json` says are stale or, with dbt Core 1.x, could not be
checked, each section saying which command wrote its file, how many
results it holds and how long ago. dbt 2 leaves out of
`target/sources.json` a check that could not run, such as one of a
missing table, and writes `"results": []` when none could: the last line
of the section of stale sources says to look at dbt's output, after `f`
its buffer, or at its log, `logs/dbt.log` unless `DBT_LOG_PATH` or
`--log-path` moves it. `RET` visits a node's file, `TAB` folds a section,
`n` and `p` move by line, `g` reads the files again, and `f` runs `dbt
source freshness`, then reads them again once it ends.
