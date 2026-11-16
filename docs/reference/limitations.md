# Limitations

- Indentation knows parentheses, Jinja blocks and the clauses of SQL, not
  the whole grammar: a `case`, its `when`s and its `end` sit at one column,
  one step in from their clause.  A Jinja tag typed at the end of the
  buffer, before what it leads to, sits at the clauses' level until
  `C-M-\` places it.  A new line inside a block comment starts at the
  first column.
- The server gives the YAML files nothing in dbt 2.0.9; dbt-mode's own
  navigation and completion work there.
- Without a server, previewing a CTE runs its Jinja as an inline query, with
  all that comes before its WITH, so a CTE that uses a model's context, such
  as `is_incremental()`, needs the server, which previews the compiled SQL.
- `dbt-compiled-diff` compares with a state's manifest, not with another
  git revision: dbt compiles the project on disk.
- Only lsp-mode is supported, not Eglot.
