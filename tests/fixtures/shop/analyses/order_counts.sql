select status, count(*) from {{ ref('stg_orders') }} group by 1
