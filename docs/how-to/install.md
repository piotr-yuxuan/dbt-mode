# Install

dbt-mode needs Emacs 31.1 or later and lsp-mode 9, and works with the one
dbt you have. `dbt-oss`, the open-source build of dbt 2, runs all but the
language server, the formatter and the linter: `dbt lsp` and `dbt format`
come with dbt 2's proprietary build only, and `dbt lint` runs only there,
which `dbt system upgrade-distribution` turns `dbt-oss` into; dbt Core
1.x has none of the three. dbt-mode asks dbt once whether it has the
server and the formatter, asks again once dbt is replaced, and does
without. A dbt that does not answer within ten seconds whether it runs
a command is taken as lacking that one, as a message says, without
being asked at each save again; the dispatch menu says that it did not
answer, and `C-c C-q` and `M-x dbt-lsp`, which you run and may wait for,
ask it again. It was tested with `dbt-oss` 2.0.11 and the proprietary
2.0.9.

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

With embark, its actions on a dbt resource join its own; `M-x
dbt-consult-resource` needs consult alone, and can take the place of
`C-c C-f`:

```elisp
(with-eval-after-load 'embark
  (require 'dbt-embark)
  (add-to-list 'embark-target-finders #'dbt-embark-target)
  (add-to-list 'embark-keymap-alist
               '(dbt-identifier dbt-embark-identifier-map embark-general-map)))
(keymap-set dbt-mode-map "<remap> <dbt-find-resource>" #'dbt-consult-resource)
```

`dbt-lsp-deferred` makes the root of the dbt project a folder of the lsp
session before it starts the server, so that the server runs on the
project, not on an enclosing repository, and each dbt project of a
repository gets its own server, whatever `lsp-auto-guess-root` says. It
adds the folder only once `dbt-executable` is known to run `dbt lsp`, which
it asks once, within ten seconds, and again once dbt is replaced;
`M-x dbt-lsp` asks again a dbt that did not answer in time.
`dbt-properties-mode-maybe` turns on `M-.`, `M-?` and completion in the
YAML files of a dbt project.

## With dbt-lsp-plus

dbt-lsp-plus is a language server in front of `dbt lsp`, built from its
own repository with `cargo install --path . --bin dbt-lsp-plus`. Once
its executable is on `exec-path`, `(setq dbt-lsp-proxy t)` runs
`dbt lsp` through it, and, with `dbt-oss`, runs it alone: see [what the
server does](../explanation/the-server.md).
