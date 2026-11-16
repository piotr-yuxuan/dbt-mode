# Options

| Option                         | Default | Meaning                                                         |
|--------------------------------|---------|-----------------------------------------------------------------|
| `dbt-executable`               | `"dbt"` | The dbt executable                                              |
| `dbt-lsp-arguments`            | `nil`   | More arguments of `dbt lsp`, such as `("--profiles-dir" "dir")` |
| `dbt-lsp-enable-linter`        | `nil`   | Whether the server lints the SQL with dbt's linter              |
| `dbt-lsp-proxy`                | `nil`   | Whether dbt lsp runs through dbt-lsp-plus, when it is installed; `M-.` and `M-?` on what it says it answers go to it |
| `dbt-preview-limit`            | `50`    | The rows that a preview shows                                   |
| `dbt-lineage-level-limit`      | `500`   | The most nodes of a level of the lineage asked of dbt-lsp-plus  |
| `dbt-indent-offset`            | `4`     | The columns by which a parenthesis or a Jinja block indents     |
| `dbt-protected-targets`        | `("prod")` | The targets on which an Org block, the data diff and the dashboard's freshness ask first; an unread default asks too |
| `dbt-project-index-lifetime`   | `30`    | Seconds before the project's files are read again               |
| `dbt-mode-jinja-max-tag-length`| `4000`  | The longest Jinja tag that font lock looks across               |

lsp-mode's own options apply: `lsp-lens-enable` shows the "Preview CTE"
lenses, `lsp-semantic-tokens-enable` the server's semantic tokens,
`lsp-headerline-breadcrumb-enable` its breadcrumbs, whose segments
`lsp-headerline-breadcrumb-segments` chooses.
