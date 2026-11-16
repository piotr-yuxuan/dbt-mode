# Keys

| Key           | Command                | What it does                                                     |
|---------------|------------------------|------------------------------------------------------------------|
| `C-c C-d`     | `dbt-dispatch`         | Run a dbt command, by default on the current model               |
| `C-c C-v`     | `dbt-visit-compiled`   | The compiled SQL beside the model, facing point; with `C-u`, the SQL of the last run (`dbt-visit-run`) |
| `C-c C-p`     | `dbt-preview`          | The rows of the model, or of the region; with `C-u`, of the CTE  |
| `C-c C-g`     | `dbt-lineage`          | The upstream and downstream of a model                           |
| `C-c C-y`     | `dbt-visit-properties` | The YAML entry of the model                                      |
| `C-c C-f`     | `dbt-find-resource`    | Go to any model, seed, snapshot, analysis, macro, docs block or source |
| `C-c C-e`     | `dbt-insert-ref`       | Insert a `ref`, read with completion, as dbt resolves it here    |
| `C-c C-t`     | `dbt-visit-tests`      | The tests of the model: its YAML entries, the data tests that name it, its singular tests; not its unit tests |
| `C-c C-w`     | `dbt-copy-relation`    | Copy the relation of the model, or of the `ref` or `source` at point, from `target/manifest.json` |
| `C-c C-o`     | `dbt-describe`         | What dbt knows of the node at point, a `ref` or a `source`, else of the model |
| `C-c M-w`     | `dbt-copy-build-command` | Copy `dbt build -s` of what point is on, a `ref` or a `source`, else the model; with `C-u`, `+…+` |
| `C-c C-a`     | `dbt-new`              | Write a new model, macro, snapshot, YAML entry or unit test      |
| `C-c /`       | `dbt-close-block`      | Close the innermost open Jinja block                             |
| `C-c %`       | `dbt-jump-to-matching-tag` | Move between a block's opening and closing tags              |
| `C-c C-q`     | `dbt-format-buffer`    | Save, then format: with dbt lsp by the server, else by `dbt format` |
| `M-.`, `M-?`  | `xref`                 | Definition and uses of what point is on                          |

`dbt-dispatch` runs `build`, `run`, `test`, `seed`, `snapshot`, `compile`,
`show`, `list`, `lint`, `format`, `parse`, `deps`, `debug`, `retry` and
`docs generate` in a compilation buffer whose errors link to their files;
with a dbt that cannot format, such as `dbt-oss`, it greys out `lint` and
`format`, which only dbt 2's proprietary build runs. Its selection
defaults to what point is on, a `ref` or a `source` as dbt
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
and `-` show one level more or less, point staying on its node, `m` shows
or hides the macros, `e` writes the graph within that depth as Graphviz
dot, each node named by its unique id, and an SVG beside it when `dot` is
installed, rendered while you go on, and `RET` visits a node's file. With
dbt-lsp-plus, the lineage comes from its call hierarchy, one level at a
time: it opens two levels deep, `+` asks the next level, and a level of
more nodes than `dbt-lineage-level-limit` is the last asked in its
direction, as the header line says; the macros, which only it gives, are
hidden at first; a node of which it has no item, such as a snapshot defined
in YAML, has its lineage from dbt lsp's list of nodes; a call that fails is
said in a message, the rest of its level shown, and `g` asks again. With
dbt lsp alone, the lineage comes whole from the server's list of nodes, and
without a server from the project's files. lsp-mode's own view of the call
hierarchy, `lsp-treemacs-call-hierarchy` when lsp-treemacs is installed,
needs nothing of dbt-mode. In the lineage and the rows, `g` asks again:
with dbt-lsp-plus to the depth shown, two levels at least, and without a
server the lineage reads the project again.
The rows show nested values as JSON and newlines as `\n`; lsp-mode reads
JSON `false` as null, so it shows as `NULL` with dbt lsp.
