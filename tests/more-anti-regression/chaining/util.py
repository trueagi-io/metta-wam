import numpy as np

NUM_GENES = 8
NUM_TRANSCRIPT_PER_GENE = 2
NUM_SNPS_PER_GENE = 5
NUM_TADS = 2
NUM_GENES_PER_TAD = 4
NUM_EQTL_PER_TAD = 1

NUM_INTERACTIONS_PER_GENE = 10
NUM_GOS = 50
NUM_GENES_PER_GO = 5

small_letters = "abcdefghijklmnopqrstuvwxyz"
capital_letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

out = []

np.random.seed(42)

for i in range(NUM_GENES):
    gene_id = small_letters[i]
    out.append(f"(: gene-{gene_id} (gene {gene_id}))")
    for j in range(NUM_TRANSCRIPT_PER_GENE):
        k = i*NUM_TRANSCRIPT_PER_GENE
        out.append(f"(: transcript-t{k+j+1} (transcript t{k+j+1}))")
        out.append(f"(: protein-p{k+j+1} (protein p{k+j+1}))")
        out.append(f"(: transcribed_to-{gene_id}-t{k+j+1} (transcribed_to (gene {gene_id}) (transcript t{k+j+1})))")
        out.append(f"(: translates_to-t{k+j+1}-p{k+j+1} (translates_to (transcript t{k+j+1}) (protein p{k+j+1})))")

    for j in range(NUM_SNPS_PER_GENE):
        k = i*NUM_SNPS_PER_GENE
        out.append(f"(: sequence_variant-rs{k+j+1} (sequence_variant rs{k+j+1}))")
        out.append(f"(: closest-gene-rs{k+j+1}-{gene_id} (closest-gene (sequence_variant rs{k+j+1})) (gene {gene_id}))")
        
    for j in list(np.random.choice(len(capital_letters), NUM_INTERACTIONS_PER_GENE)):
        gene_id_2 = capital_letters[j]
        out.append(f"(: gene-{gene_id_2} (gene {gene_id_2}))")
        out.append(f"(: coexpressed_with-{gene_id}-{gene_id_2} (coexpressed_with (gene {gene_id}) (gene {gene_id_2})))")

for i in range(NUM_TADS):
    tad_region_id = f"chr16-{1000*(i+1)}-{1000*(i+2)}"
    out.append(f"(: tad-{tad_region_id} (tad {tad_region_id}))")
    k = i*NUM_GENES_PER_TAD
    for j in range(NUM_GENES_PER_TAD):
        gene_id = small_letters[k+j]
        out.append(f"(: in-tad-region-{tad_region_id}-{gene_id} (in-tad-region (tad {tad_region_id}) (gene {gene_id})))")
        
    eqtl_gene_ids = list(np.random.choice(NUM_GENES_PER_TAD, NUM_EQTL_PER_TAD))
    for s in eqtl_gene_ids:
        gene_id = small_letters[k+s]
        snp_id = (k+s)*NUM_SNPS_PER_GENE
        out.append(f"(: eqtl-rs{snp_id}-{gene_id} (eqtl (sequence_variant rs{snp_id}) (gene {gene_id})))")
        
        
for i in range(NUM_GOS):
    go_id = f"{i+1:07d}"
    out.append(f"(: ontology_term-GO:{go_id} (ontology_term GO:{go_id}))")
    g = np.random.choice(NUM_GENES, 1)[0]
    p = np.random.choice(NUM_TRANSCRIPT_PER_GENE, 1)[0]
    protein_id = f"{g*NUM_TRANSCRIPT_PER_GENE + 1}"
    
    out.append(f"(: go_gene_product-GO:{go_id}-p{protein_id} (go_gene_product (ontology_term GO:{go_id}) (protein p{protein_id})))")
    
    idxs = list(np.random.choice(len(capital_letters), NUM_GENES_PER_GO - 1))
    for j in idxs:
        gene_id_2 = capital_letters[j]
        out.append(f"(: transcript-t{gene_id_2} (transcript t{gene_id_2}))")
        out.append(f"(: protein-p{gene_id_2} (protein p{gene_id_2}))")
        out.append(f"(: transcribed_to-{gene_id_2}-t{gene_id_2} (transcribed_to (gene {gene_id_2}) (transcript t{gene_id_2})))")
        out.append(f"(: translates_to-t{gene_id_2}-p{gene_id_2} (translates_to (transcript t{gene_id_2} (protein p{gene_id_2}))))")
        out.append(f"(: gene-{gene_id_2} (gene {gene_id_2}))")
        out.append(f"(: go_gene_product-GO:{go_id}-p{gene_id_2} (go_gene_product (ontology_term GO:{go_id}) (protein p{gene_id_2})))")



if __name__ == "__main__":
    with open(f"./sample_kb.metta", "w") as fp:
        for line in out:
            fp.write(f"{line}\n")
    