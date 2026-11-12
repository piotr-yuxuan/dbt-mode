{% macro is_positive(expression) %}
    ({{ expression }} > 0)
{% endmacro %}
