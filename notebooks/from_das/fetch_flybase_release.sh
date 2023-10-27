#! /bin/bash
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <release tag> <target dir>"
    echo "   <release tag> something like 2022_06"
    echo "   <target dir> a directory where output will be put (all its contents WILL BE DELETED before the script execution)"
    exit 1
fi

function validate_url(){
  if [[ `wget -S --spider $1  2>&1 | grep 'HTTP/1.1 200 OK'` ]]; then
    return 0
  else
    return 1
  fi
}

declare -a StringArray=(\
"fbgn_fbtr_fbpp_expanded_*.tsv.gz" \
"physical_interactions_mitab_fb_*.tsv.gz" \
"dmel_gene_sequence_ontology_annotations_fb_*.tsv.gz" \
"gene_map_table_*.tsv.gz" \
"ncRNA_genes_fb_*.json.gz" \
"gene_association.fb.gz" \
"gene_genetic_interactions_*.tsv.gz" \
"allele_genetic_interactions_*.tsv.gz" \
"allele_phenotypic_data_*.tsv.gz" \
"disease_model_annotations_fb_*.tsv.gz" \
"dmel_human_orthologs_disease_fb_*.tsv.gz" \
"fbrf_pmid_pmcid_doi_fb_*.tsv.gz")

rm -rf $2
mkdir -p $2/$1/precomputed
wget --no-parent -A .gz -r http://ftp.flybase.org/releases/FB$1/precomputed_files/

for arg in ${StringArray[@]}; do
    find ftp.flybase.org -name $arg -print -exec cp {} $2/$1/precomputed \;
done

rm -rf ftp.flybase.org

URL="http://ftp.flybase.net/releases/FB$1/psql/FB$1.sql.gz"
if validate_url $URL; then
    wget $URL
    mv FB$1.sql.gz $2/$1
  else
    echo "Couldn't find release file"
fi
