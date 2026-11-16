# What the server does

With dbt 2's proprietary build (`dbt lsp`), `dbt-lsp-deferred` starts the
server through lsp-mode, which then gives the following; with `dbt-oss`
it starts dbt-lsp-plus alone when `dbt-lsp-proxy` is on and dbt-lsp-plus
is installed ([Install](../how-to/install.md#with-dbt-lsp-plus)), and otherwise nothing, and `M-x dbt-lsp` says why:

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

With dbt-lsp-plus (`dbt-lsp-proxy`), the results of the last run, whoever
ran it, are diagnostics too: once `b` in `dbt-dispatch` has built, each
model that errored and each test that failed or warned shows where it is
written, a generic test at its YAML entry, through flymake or flycheck,
as `lsp-diagnostics-provider` says.

dbt writes `logs` and `target` in the project at each run, and dbt lsp at
each compilation, while dbt lsp asks lsp-mode to watch every file:
without more, each run sends the server about a hundred events, and has a
dbt-lsp-plus older than its plan 025 read the project again at each.
Once dbt-lsp.el is loaded, dbt-mode keeps lsp-mode's file watchers out
of `logs`, `target` and the `target-path` of `dbt_project.yml`, for any
server that lsp-mode then starts on a dbt project, by a regexp of their
true names that it adds to the global value of
`lsp-file-watch-ignored-directories` for this Emacs session. Customize
then shows the option as changed outside it, and saving it there would
keep the regexps. A watch that another server made on the project's
folder before dbt-lsp.el was loaded keeps watching them, until every
server on that folder stops; a `.dir-locals.el` at the workspace's root
that sets the option for every mode replaces the whole list, these
regexps included; a `DBT_TARGET_PATH` or `DBT_LOG_PATH` elsewhere in
the project is still watched.

With `dbt-oss` and `dbt-lsp-proxy` on, dbt-lsp-plus runs alone, without
`dbt lsp`, from the files and from its own `dbt parse`, run offline. It
answers `M-.` and `M-?` on sources, the lineage of `C-c C-g` from its
call hierarchy, `C-c C-o`, the document symbols of the breadcrumbs, and,
once it has parsed, hover, inlay hints, the parse's problems and the
last run's results as diagnostics. The rest stays dbt-mode's, as
without a server: `C-c C-p` and the data diff ask `dbt show`, `C-c C-v`
reads what `dbt compile` wrote, `M-.` and `M-?` answer the rest, `C-c
C-o` reads the manifest while the proxy has not parsed, and eldoc reads
it always; `dbt-oss` cannot format, and renaming a model or fixing lint
issues needs `dbt lsp`. Alone, the proxy keeps of `dbt-lsp-arguments`
only the profiles directory, profile, target, vars and packages' path,
`dbt-lsp-enable-linter` does nothing, and the mode line says nothing
while it parses. dbt Core 1.x, which refuses `dbt lsp` otherwise than
dbt 2 does, gets no server.

## What dbt-mode relies on

The protocol of `dbt lsp` is not documented. What dbt-lsp.el relies on was
observed on dbt 2.0.9, and the live tests check it: the custom commands
`dbt.compileFile`, `dbt.show`, `dbt.listNodes` and the code lens
`dbt.previewCte`, the notifications `dbt/*`, file renames through
`workspace/willRenameFiles`, and that formatting, the linter's fix-all,
`dbt.compileFile` and `dbt.show` with a uri all read the file on disk.
`dbt.show` answers no columns for a macro. lsp-mode reads JSON false as
nil.
