select
    customer_id,
    count(*) as orders_count
from {{ ref('orders') }}
left join {{ ref('stg_customers') }} using (customer_id)
group by 1
