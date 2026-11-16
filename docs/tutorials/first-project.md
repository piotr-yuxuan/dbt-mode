# Your first project

In this lesson you open the small dbt project that the tests use, from a
clone of this repository, and go from a model to its definitions, its uses
and its lineage, then build it and see its rows. It takes about ten
minutes on a machine with Nix. Nothing is installed: your own Emacs
configuration stays out of it. Steps 1 to 7 need no dbt; step 8 needs a
dbt 2 on your `PATH`, and `dbt-oss` is enough.

## 1. Get the clone and the tools

```sh
git clone https://github.com/piotr-yuxuan/dbt-mode
cd dbt-mode
nix develop
```

**Check:** `emacs --version` starts with `GNU Emacs 31.1`.

## 2. Start a separate Emacs on a model

```sh
emacs -Q -L . --eval "(require 'dbt-mode)" tests/fixtures/shop/models/marts/orders.sql
```

The project is `tests/fixtures/shop`, a shop on DuckDB: two seeds, two
staging models, two marts, a macro and a package.

**Check:** the mode line shows `dbt dev table`: the mode, the target that
dbt would run on, and the materialization that the model's `config()`
sets. The `{# #}` block at the top is a comment, apostrophe included.

## 3. Move by CTE

Put point in `customers as (` and press `C-M-e`, then `C-M-a`.

**Check:** point goes to the end of the CTE `customers`, then back to its
first line. Press `M-g i`: imenu offers `orders`, `customers` and
`final`, the CTEs of the model.

## 4. Go to a definition

Put point on `stg_orders` in `{{ ref('stg_orders') }}` and press `M-.`.

**Check:** `models/staging/stg_orders.sql` opens. Press `M-,` to come
back to `orders.sql`.

## 5. Find its uses

On the same `stg_orders`, press `M-?`, then `RET` for the default it
offers.

**Check:** a list of five uses: the analysis `order_counts.sql`,
`orders.sql`, the snapshot `orders_snapshot.sql`, the singular test
`assert_positive_amount.sql`, and the YAML entry of the model in
`_staging.yml`. Quit it with `q`.

## 6. Find any resource

Press `C-c C-f`, type `cents`, and choose `macro:cents_to_dollars`.

**Check:** `macros/cents_to_dollars.sql` opens. Go back to `orders.sql`
with `C-x b orders.sql RET`.

## 7. See its lineage

Press `C-c C-g`, then `RET` for the default, `orders`.

**Check:** a buffer `*dbt lineage: orders*` shows a tree: upstream,
`stg_orders` with the seed `raw_orders` under it, and `stg_customers`
with `raw_customers`; downstream, `customers`. Without a server, it comes
from the `ref` and `source` calls of the project's files. Put point on
`customers` and press `RET`: `models/marts/customers.sql` opens. Go back
to `orders.sql`.

## 8. Build it and see its rows

This step runs dbt, which writes `target/` and `logs/` in the project;
git ignores both. The first run may fetch dbt's DuckDB driver.

Press `C-c C-d`, then `-u` for "With its upstream", then `b`.

**Check:** a buffer `*dbt shop: build*` runs `dbt build --select +orders`
and ends with `Finished 'build' successfully for target 'dev'`: two
seeds, three models and three tests.

Now press `C-c C-p`.

**Check:** a buffer `*dbt rows: orders*` shows the columns `order_id`,
`customer_id`, `first_name`, `amount` and `is_paid`, and three rows, two
of them Ada's.

## 9. End it

Quit Emacs with `C-x C-c`.

**Check:** `git status` says that nothing changed. To remove what dbt
wrote, `rm -r tests/fixtures/shop/target tests/fixtures/shop/logs`.

## Where next

You have moved through a dbt project without dbt, then built it. To keep
dbt-mode in your own Emacs, [install it](../how-to/install.md). [The
keys](../reference/keys.md) and [the commands](../reference/commands.md)
list the rest, and [how dbt-mode reads a
project](../explanation/reading-a-project.md) explains what it resolves
without dbt.
