# Replace these variables with your own values
fb_remote_host="chado.flybase.org"
fb_user="flybase"
fb_database="flybase"
fb_scheme="public"

# Connect to PostgreSQL server and retrieve table names within the specified schema
tables=$(psql -h $fb_remote_host -U $fb_user -d $fb_database -t -c "SELECT table_name FROM information_schema.tables WHERE table_type = 'BASE TABLE' AND table_catalog = '$fb_database' AND table_schema = '$fb_scheme';")

# Create a directory to store the TSV files
mkdir -p "./data/tsv_exports/$fb_scheme/"

# Iterate over each table
for table in $tables; do
    # Print the table name
    echo "Exporting table: $fb_scheme.$table"
    FILE="./data/tsv_exports/$fb_scheme/$fb_scheme.${table}"

    if [ -f "${FILE}.tsv" ]; then
        echo "${FILE}.tsv exists."
    else
            if [ -f "${FILE}.tsv" ]; then
                echo "${FILE}.tsv exists."
            else
                    if [ -f "${FILE}.pl" ]; then
                        echo "${FILE}.pl exists."
                    else
                        # Export the table data to a TSV file
                        echo "${FILE}.tsv does not exist. "
                        psql -h $fb_remote_host -U $fb_user -d $fb_database -c "COPY $fb_scheme.$table TO STDOUT WITH (FORMAT CSV, DELIMITER E'\t', HEADER true)" > "${FILE}.tsv"
                   fi
            fi
    fi


done

