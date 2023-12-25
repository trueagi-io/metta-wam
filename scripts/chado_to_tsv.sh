#!/bin/bash

# Replace these variables with your own values
fb_remote_host="chado.flybase.org"
fb_user="flybase"
fb_database="flybase"
fb_scheme="public"

# Create a directory to store the TSV files
mkdir -p "./data/tsv_exports_new/$fb_scheme/"

(

cd "./data/tsv_exports_new/$fb_scheme/"

# List of table names in the information_schema
is_tables=(
    "tables"
    "columns"
    "table_constraints"
    "key_column_usage"
    "referential_constraints"
    "schemata"
    "views"
    "column_privileges"
    "table_privileges"
    "routines"
    "triggers"
    "domains"
    "domain_constraints"
    "sequences"
    "character_sets"
    "collations"
    "check_constraints"
    "view_column_usage"
    "view_table_usage"
    "view_routine_usage"
    "parameters"
)


# Loop for information_schema tables
for is_table in "${is_tables[@]}"; do
    tablename="information_schema.$is_table"
    echo "Processing $tablename"
    psql -h $fb_remote_host -U $fb_user -d $fb_database -c "\COPY (SELECT * FROM $tablename) TO './${tablename}.tsv' WITH (FORMAT CSV, HEADER, DELIMITER E'\t')"
done


# Retrieve and export individual schema tables
tables=$(psql -h $fb_remote_host -U $fb_user -d $fb_database -t -c "SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_catalog = '$fb_database' AND table_schema = '$fb_scheme';")

for table in $tables; do
    filepath="./${table}.tsv"
    echo "Exporting table: $fb_scheme.$table"

    if [ ! -f "$filepath" ]; then
        echo "Exporting to $filepath"
        psql -h $fb_remote_host -U $fb_user -d $fb_database -c "\COPY $fb_scheme.$table TO STDOUT WITH (FORMAT CSV, DELIMITER E'\t', HEADER true)" > "$filepath"
    else
        echo "$filepath already exists."
    fi
done

sql_query="
SELECT
    fk.table_name AS foreignkey_table,
    fk.column_name AS foreignkey_column_name,
    pk.table_name AS primarykey_table,
    pk.column_name AS primarykey_column_name
FROM
    information_schema.key_column_usage AS fk
JOIN
    information_schema.referential_constraints AS rc
    ON fk.constraint_catalog = rc.constraint_catalog
    AND fk.constraint_schema = rc.constraint_schema
    AND fk.constraint_name = rc.constraint_name
JOIN
    information_schema.key_column_usage AS pk
    ON rc.unique_constraint_catalog = pk.constraint_catalog
    AND rc.unique_constraint_schema = pk.constraint_schema
    AND rc.unique_constraint_name = pk.constraint_name
    AND fk.ordinal_position = pk.ordinal_position
WHERE
    fk.table_schema = 'public' AND pk.table_schema = 'public'
ORDER BY
    pk.table_name, fk.ordinal_position
"

# Export query result to TSV file
psql -h $fb_remote_host -U $fb_user -d $fb_database -c "\COPY ($sql_query) TO './fk_to_pk_links.tsv' WITH (FORMAT CSV, DELIMITER E'\t')"



output_file="columns.txt"

for table in $tables; do
    columns=$(psql -h $fb_remote_host -U $fb_user -d $fb_database -t -A -c "SELECT string_agg(column_name, ', ' ORDER BY ordinal_position) FROM information_schema.columns WHERE table_schema = '$fb_scheme' AND table_name = '$table';")
    echo "$table: $columns" >> "$output_file"
done

echo "Column names written to $output_file"

)
