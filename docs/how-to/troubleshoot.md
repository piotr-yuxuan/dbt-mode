# Troubleshoot

## A request times out

```
Timeout while waiting for response.  Method: textDocument/references
```

lsp-mode waited `lsp-response-timeout` seconds, 10 by default, and no
answer came.  From dbt-mode's own `M-.` and `M-?`, the message starts
with `dbt-lsp-plus: textDocument/references failed:`.  To find out why:

1. **Who was asked.**  With `dbt-lsp-proxy` on, dbt-lsp-plus answers
   references and renames on a source (a `source()` call, a source's or a
   table's `name:`) and definitions of the kinds it lists; `dbt lsp`
   answers everything else, the references of a model or a macro
   included, and the proxy passes its answer on unchanged.
   `M-x lsp-describe-session` shows, under the capabilities, the kinds
   that `experimental.dbtLspPlus` claims.
2. **Whether the server is still compiling.**  `dbt lsp` answers once it
   has compiled the project, which a large project may take more than ten
   seconds to do after the server starts or after a change of the
   project.  Ask again once the progress in the mode line has ended, or
   wait longer:

   ```elisp
   (setq lsp-response-timeout 60)
   ```

3. **What went over the wire.**  Log every message, restart, and ask
   again:

   ```elisp
   (setq lsp-log-io t)
   ```

   then `M-x lsp-workspace-restart`, the request, and `M-x
   lsp-workspace-show-log`.  The buffer `*lsp-log: dbt-lsp:PID*` shows
   the request, then its answer if one came, each with its time: no
   answer at all and a late one are different problems.  Turn it off
   afterwards, as it slows lsp-mode down.
4. **Without the proxy.**  `(setq dbt-lsp-proxy nil)`, then `M-x
   lsp-workspace-restart`, and the same request.  If it still times out,
   `dbt lsp` is slow or stuck; if it answers now, the proxy is at fault.
   [dbt-lsp-plus's troubleshooting](https://github.com/piotr-yuxuan/dbt-lsp-plus/blob/main/docs/how-to/troubleshoot.md)
   goes on from there.

## Where Emacs shows what the servers say

- `*lsp-log*`: the servers' log messages, among them dbt-lsp-plus's,
  each prefixed `dbt-lsp-plus:`, once per reason: why its `dbt parse`
  failed or was refused, that it answers alone, that it restarts `dbt
  lsp`.
- `*dbt-lsp::stderr*`: what the server process writes on its standard
  error, the proxy's and `dbt lsp`'s.
- `*lsp-log: dbt-lsp:PID*`: every message, with `lsp-log-io` on.
- `M-x lsp-describe-session`: the session's folders, its server and the
  capabilities it announced.

## Other symptoms

- **`M-x dbt-lsp` says that this dbt runs no dbt lsp.**  Only dbt 2's
  proprietary build has `dbt lsp`: point `dbt-executable` at it, or turn
  `dbt-lsp-proxy` on with dbt-lsp-plus on `exec-path`, which serves
  dbt-oss alone.
- **The dispatch menu says that dbt did not answer.**  dbt took more than
  ten seconds to say whether it runs a command, as a launcher such as
  `uvx dbt` may with a cold cache.  `C-c C-q` and `M-x dbt-lsp` ask again.
- **No hover summary, inlay hints or node information with the proxy.**
  Its `dbt parse` failed, as `*lsp-log*` says; most often dbt finds no
  `profiles.yml`, which it looks for in the server's working directory
  and in `~/.dbt`, not in the project:

  ```elisp
  (setq dbt-lsp-arguments '("--profiles-dir" "/path/to/project"))
  ```

## Report a problem

Open an issue on [dbt-mode](https://github.com/piotr-yuxuan/dbt-mode/issues)
for what Emacs does, or on
[dbt-lsp-plus](https://github.com/piotr-yuxuan/dbt-lsp-plus/issues) for
what the proxy answers, with the versions of Emacs, lsp-mode, dbt-mode,
dbt (`dbt --version`) and dbt-lsp-plus (`dbt-lsp-plus --version`), the
lines of `*lsp-log*` and `*dbt-lsp::stderr*`, and the part of the
message log around the request.
