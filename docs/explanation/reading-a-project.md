# How dbt-mode reads a project without dbt

Without any dbt executable, dbt-mode reads the project's files:

- Jinja's `{# #}` comments are comments, apostrophes included, and the tags
  `{{ }}` and `{% %}` show their delimiters, keywords, dbt's functions,
  packages and macro calls, over several lines too; a string inside a tag
  is a string, so the apostrophe of `"doesn't"` opens nothing;
- `M-.` on `ref('…')`, `source('…', '…')`, `doc('…')` or a macro call,
  `dbt_utils.star(…)` included, goes to its definition, and on
  `ref('m', v=2)` to that version, and on a name that its package lacks
  says which packages define it; it resolves each as dbt 2.0.11 does:
  - a `ref` or a `source` in the calling file's package first (in a
    macro, the macro's, as dbt-mode cannot know the node that calls it),
    or in the project first when its `dbt_project.yml` sets the flag
    `require_ref_searches_node_package_before_root: false`, and a YAML
    entry in its own package;
  - a `ref` without `v=` to the latest version of each package's model,
    its `latest_version` or else its highest;
  - a `doc` in the project, else in the packages by the order of their
    names, whatever package calls it;

  where the project and a package define the same name, `M-.` goes
  straight to the project's definition when dbt takes that one; when
  dbt takes a package's, it lists it first, then its namesakes, as dbt
  skips a package's model that the project disables to override it,
  which dbt-mode does not read, and a call in a macro resolves from the
  node that calls the macro; a macro call, and `C-c C-f`, list every
  definition; a macro of the project that a package's node calls, as
  through dispatch, which dbt resolves from that package, still goes to
  the project's; a `ref` or a `source` that neither the calling
  package nor the project defines and two packages or more do, which
  dbt rejects as ambiguous, lists them, each saying so, and `C-c C-e`
  asks whose such a name to call;
- `M-?` lists the calls that resolve to the same definition, so a
  package's model and the project's namesake stay apart, and the YAML
  entries of a model;
- with [consult](https://github.com/minad/consult), `M-x
  dbt-consult-resource` reads a resource of the project or of its
  packages, as `C-c C-f` names them, grouped by kind, each with its
  package and file, and previews the current one at its definition;
  with [embark](https://github.com/oantolin/embark), `embark-act` acts
  on the call or the YAML entry at point, and on the candidates of that
  command and of `C-c C-f`: `RET` goes where `M-.` would, `d` lists every
  definition, `y` visits the YAML entry of a model, seed or snapshot, `b`
  builds it, `r` runs a model, `t` tests it, `l` shows its lineage and
  `w` copies its relation, each where `M-.` would go, and refusing a
  resource it does not fit, such as a macro to build; embark's general
  actions stay, but for `w`, its copy of the target (see [Install](../how-to/install.md));
- completion of the arguments of `ref`, `source` and `doc`, and of macros in
  a tag;
- `imenu` lists the CTEs, and the macros, tests, snapshots and docs blocks;
- the lineage of a model, from its `ref` and `source` calls.

The index knows versioned models, snapshots defined in YAML, flow-style
YAML, packages under `packages-install-path` and local packages, and leaves
`{# #}` comments out. A project nested in another, such as a package's
`integration_tests`, is its own project; a package that a project installs
or declares belongs to that project. Saving re-reads only the files that
changed.
