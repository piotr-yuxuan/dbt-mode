# Format and fix

`dbt lsp` 2.0.9 formats the file on disk rather than the buffer, and so does
its linter's fix-all code action: their edits would corrupt unsaved changes.
dbt-mode hides both from lsp-mode. `dbt-lsp-format-buffer` saves the buffer
first, then formats it; `dbt-lsp-fix-all` saves, then fixes and formats,
whether or not `dbt-lsp-enable-linter` is on; `dbt-dispatch` runs `dbt
format` and `dbt lint`. lsp-mode's `lsp-format-buffer-on-save` and
`lsp-fix-all-on-save` are turned off in the buffers of dbt lsp, where they
could only fail.

`C-c C-q`, `dbt-format-buffer`, saves the buffer, then formats it while you
go on: with dbt lsp by the server, whose edits leave the buffer modified;
without one by `dbt format FILE`, which formats the saved file, the buffer
then showing the result. Text you type meanwhile wins, unformatted, which
a message says, and the next save asks nothing. A save waits for a
format that runs, so that an older one never writes over it, and stops
it after `dbt-cli-format-deadline`, 30 seconds. dbt may format the file
and still fail on another file of the project: the buffer shows the
file, and dbt's error, which names the file it is about, is the message. `dbt-format-on-save-mode`, a minor mode to turn on from
`dbt-mode-hook`, formats after each save, and with dbt lsp saves again
when that changed the buffer; a failure to format is a message, and the
save stands. A dbt with neither a server nor `dbt format`, such as
`dbt-oss`, cannot format: `C-c C-q` saves, then says so, and
`dbt-format-on-save-mode` leaves saves as they are, which it says once
for each dbt.

`dbt-lsp-rename-model` refuses a name that another model, seed or snapshot
of the package already has.
