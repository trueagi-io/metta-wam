/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%*********************************************************************************************
% PROGRAM FUNCTION:  Performs type induction, schema inference, and logical reasoning to dynamically 
% query, integrate, and analyze biological datasets, often listing equivalent SQL versions for reference.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











































/*
Finding Publications That Link Multiple Mutations
SQL Version:

SELECT P1.title AS Publication1, M1.name AS Mutation1, P2.title AS Publication2, M2.name AS Mutation2
FROM Mutation AS M1
INNER JOIN MutationPublication AS MP1 ON M1.id = MP1.mutation_id
INNER JOIN Publication AS P1 ON MP1.publication_id = P1.id
INNER JOIN MutationPublication AS MP2 ON M1.id != MP2.mutation_id
INNER JOIN Mutation AS M2 ON MP2.mutation_id = M2.id
INNER JOIN Publication AS P2 ON MP2.publication_id = P2.id AND P1.id = P2.id
WHERE M1.name != M2.name
*/



%!  linked_mutations(+Publication1, +Mutation1, +Publication2, +Mutation2) is nondet.
%
%   Identifies linked mutations that are associated with the same publication.
%
%   This predicate establishes a relationship between two mutations (Mutation1 
%   and Mutation2) that are linked through the same publication. It ensures that 
%   the mutations are distinct, both in terms of their IDs and their content, while 
%   also checking that they appear in the same publication.
%
%   @arg Publication1 The first publication associated with Mutation1.
%   @arg Mutation1    The first mutation to be linked.
%   @arg Publication2 The second publication associated with Mutation2.
%   @arg Mutation2    The second mutation to be linked.
%
%   @examples
%     % Find linked mutations:
%     ?- linked_mutations('Pub1', 'MutationA', 'Pub1', 'MutationB').
%     Publication1 = 'Pub1',
%     Mutation1 = 'MutationA',
%     Publication2 = 'Pub1',
%     Mutation2 = 'MutationB'.
%
linked_mutations(Publication1, Mutation1, Publication2, Mutation2) :-
    % Retrieve the mutation ID associated with Mutation1.
    mutation(M1_id, Mutation1),
    % Get the publication ID for Mutation1.
    mutation_publication(M1_id, MP1_id),
    % Map the publication ID to its corresponding publication.
    publication(MP1_id, Publication1),
    % Retrieve the mutation ID associated with Mutation2.
    mutation_publication(M2_id, MP2_id),
    mutation(M2_id, Mutation2),
    % Map the publication ID to its corresponding publication.
    publication(MP2_id, Publication2),
    % Ensure that the two mutations are distinct by ID and content.
    M1_id \= M2_id,
    Mutation1 \= Mutation2,
    % Ensure the two mutations share the same publication ID.
    MP1_id = MP2_id.

/*
 Publications Discussing Mutations in Related Genes
SQL Version:

SELECT G1.name AS Gene1, M1.name AS Mutation1, G2.name AS Gene2, M2.name AS Mutation2, P.title
FROM Gene AS G1
INNER JOIN Mutation AS M1 ON G1.id = M1.gene_id
INNER JOIN MutationPublication AS MP1 ON M1.id = MP1.mutation_id
INNER JOIN Publication AS P ON MP1.publication_id = P.id
INNER JOIN Mutation AS M2 ON P.id IN (SELECT publication_id FROM MutationPublication WHERE mutation_id = M2.id)
INNER JOIN Gene AS G2 ON M2.gene_id = G2.id AND G1.id != G2.id
WHERE G1.function = G2.function OR G1.pathway = G2.pathway
*/



%!  related_genes_publication(+Gene1, +Mutation1, +Gene2, +Mutation2, +Title) is nondet.
%
%   Finds related genes and their associated mutations based on shared publications.
%
%   This predicate identifies pairs of genes (Gene1 and Gene2) and their associated 
%   mutations (Mutation1 and Mutation2) that are linked by the same publication (Title).
%   It ensures the two genes are distinct and considers them related if they share 
%   the same function or pathway.
%
%   @arg Gene1       The name of the first gene.
%   @arg Mutation1   The name of the mutation associated with Gene1.
%   @arg Gene2       The name of the second gene.
%   @arg Mutation2   The name of the mutation associated with Gene2.
%   @arg Title       The title of the publication linking the genes and mutations.
%
%   @examples
%     % Example query to find related genes and mutations:
%     ?- related_genes_publication('GeneA', 'MutationX', 'GeneB', 'MutationY', 'Some Publication').
%     Gene1 = 'GeneA',
%     Mutation1 = 'MutationX',
%     Gene2 = 'GeneB',
%     Mutation2 = 'MutationY',
%     Title = 'Some Publication'.
%
related_genes_publication(Gene1, Mutation1, Gene2, Mutation2, Title) :-
    % Retrieve details of the first gene, including its ID, name, function, and pathway.
    gene(G1_id, Gene1, Function1, Pathway1),
    % Retrieve details of the mutation associated with the first gene.
    mutation(M1_id, Mutation1),
    % Link the mutation to the first gene.
    mutation_gene(M1_id, G1_id),
    % Retrieve the publication ID and title for the mutation.
    mutation_publication(M1_id, MP1_id),
    publication(MP1_id, Title),
    % Retrieve details of the second gene, including its ID, name, function, and pathway.
    gene(G2_id, Gene2, Function2, Pathway2),
    % Retrieve details of the mutation associated with the second gene.
    mutation(M2_id, Mutation2),
    % Link the mutation to the second gene.
    mutation_gene(M2_id, G2_id),
    % Retrieve the publication ID and title for the mutation.
    mutation_publication(M2_id, MP2_id),
    publication(MP2_id, Title),
    % Ensure the two genes are distinct.
    G1_id \= G2_id,
    % Consider the genes related if they share the same function or pathway.
    (Function1 = Function2; Pathway1 = Pathway2).

/*

 Publications Linking Phenotypic Effects of Different Mutations
SQL Version:

SELECT M1.name AS Mutation1, P1.description AS Phenotype1, M2.name AS Mutation2, P2.description AS Phenotype2, Pub.title
FROM Mutation AS M1
INNER JOIN Phenotype AS P1 ON M1.phenotype_id = P1.id
INNER JOIN MutationPublication AS MP1 ON M1.id = MP1.mutation_id
INNER JOIN Publication AS Pub ON MP1.publication_id = Pub.id
INNER JOIN MutationPublication AS MP2 ON Pub.id = MP2.publication_id AND MP1.mutation_id != MP2.mutation_id
INNER JOIN Mutation AS M2 ON MP2.mutation_id = M2.id
INNER JOIN Phenotype AS P2 ON M2.phenotype_id = P2.id
*/


%!  linked_phenotypes(+Publication, +Mutation1, +Phenotype1, +Mutation2, +Phenotype2) is nondet.
%
%   Finds mutations and their associated phenotypes linked through a common publication.
%
%   This predicate identifies pairs of mutations (Mutation1 and Mutation2) and their 
%   associated phenotypes (Phenotype1 and Phenotype2) that are discussed in the same 
%   publication. It ensures that the two mutations are distinct.
%
%   @arg Publication   The title of the publication linking the mutations and phenotypes.
%   @arg Mutation1     The name of the first mutation.
%   @arg Phenotype1    The phenotype associated with the first mutation.
%   @arg Mutation2     The name of the second mutation.
%   @arg Phenotype2    The phenotype associated with the second mutation.
%
%   @examples
%     % Example query to find linked phenotypes:
%     ?- linked_phenotypes('Shared Publication', 'MutationA', 'PhenotypeX', 'MutationB', 'PhenotypeY').
%     Publication = 'Shared Publication',
%     Mutation1 = 'MutationA',
%     Phenotype1 = 'PhenotypeX',
%     Mutation2 = 'MutationB',
%     Phenotype2 = 'PhenotypeY'.
%
linked_phenotypes(Publication, Mutation1, Phenotype1, Mutation2, Phenotype2) :-
    % Retrieve the ID of the first mutation.
    mutation(M1_id, Mutation1),
    % Link the first mutation to its associated phenotype.
    mutation_phenotype(M1_id, P1_id),
    phenotype(P1_id, Phenotype1),
    % Retrieve the publication ID and title for the first mutation.
    mutation_publication(M1_id, MP1_id),
    publication(MP1_id, Publication),
    % Retrieve the publication ID and details for the second mutation.
    mutation_publication(M2_id, MP2_id),
    mutation(M2_id, Mutation2),
    % Link the second mutation to its associated phenotype.
    mutation_phenotype(M2_id, P2_id),
    phenotype(P2_id, Phenotype2),
    % Ensure the second mutation is linked to the same publication.
    publication(MP2_id, Publication),
    % Ensure the two mutations are distinct.
    M1_id \= M2_id.


/*

 Gene Expression and Phenotypes
SQL Version:


SELECT Gene.name, Expression.tissue, Phenotype.description
FROM Gene
INNER JOIN Expression ON Gene.id = Expression.gene_id
INNER JOIN Phenotype ON Gene.id = Phenotype.gene_id
WHERE Expression.tissue = 'wing disc' AND Phenotype.description LIKE '%wing defects%'
*/

%!  gene_expression_phenotype(+GeneName, +Tissue, +PhenotypeDescription) is nondet.
%
%   Identifies genes expressed in a specific tissue and their associated phenotypes.
%
%   This predicate finds genes (GeneName) that are expressed in the tissue 'wing disc' 
%   and retrieves their associated phenotypes (PhenotypeDescription) that include 
%   the term 'wing defects' in their description.
%
%   @arg GeneName            The name of the gene being queried.
%   @arg Tissue              The tissue where the gene is expressed (must be 'wing disc').
%   @arg PhenotypeDescription The description of the phenotype associated with the gene, 
%                             which must contain the substring 'wing defects'.
%
%   @examples
%     % Example query to find genes with 'wing defects' phenotypes in the 'wing disc':
%     ?- gene_expression_phenotype('GeneX', Tissue, Description).
%     GeneName = 'GeneX',
%     Tissue = 'wing disc',
%     PhenotypeDescription = 'wing defects observed in mutants'.
%
gene_expression_phenotype(GeneName, Tissue, PhenotypeDescription) :-
    % Retrieve the ID of the gene and match its name.
    gene(GeneId, GeneName),
    % Check the tissue where the gene is expressed.
    expression(GeneId, Tissue),
    % Ensure the tissue is specifically 'wing disc'.
    Tissue = 'wing disc',
    % Retrieve the phenotype description associated with the gene.
    phenotype(GeneId, PhenotypeDescription),
    % Check that the phenotype description contains the substring 'wing defects'.
    sub_string(PhenotypeDescription, _, _, _, 'wing defects').

/*
 Protein-Protein Interactions
SQL Version:

SELECT Protein1.name, Protein2.name
FROM Protein AS Protein1
INNER JOIN Interaction ON Protein1.id = Interaction.protein1_id
INNER JOIN Protein AS Protein2 ON Interaction.protein2_id = Protein2.id
WHERE Protein1.name = 'protein_of_interest'
*/


%!  protein_interaction(+ProteinName1, +ProteinName2) is nondet.
%
%   Identifies interactions involving a specific protein of interest.
%
%   This predicate finds pairs of proteins (ProteinName1 and ProteinName2) where 
%   ProteinName1 is a predefined 'protein_of_interest' and ProteinName2 interacts 
%   with it. The interaction is defined by the relationship between their protein IDs.
%
%   @arg ProteinName1 The name of the first protein (must be 'protein_of_interest').
%   @arg ProteinName2 The name of the second protein interacting with ProteinName1.
%
%   @examples
%     % Example query to find proteins interacting with 'protein_of_interest':
%     ?- protein_interaction('protein_of_interest', InteractingProtein).
%     ProteinName1 = 'protein_of_interest',
%     ProteinName2 = 'InteractingProteinX'.
%
protein_interaction(ProteinName1, ProteinName2) :-
    % Retrieve the ID of the first protein and match its name.
    protein(Protein1Id, ProteinName1),
    % Ensure that the first protein is the 'protein_of_interest'.
    ProteinName1 = 'protein_of_interest',
    % Find interactions involving the first protein's ID.
    interaction(Protein1Id, Protein2Id),
    % Retrieve the name of the interacting protein.
    protein(Protein2Id, ProteinName2).


/*
 Genetic Modifiers of a Mutation
SQL Version:

SELECT ModifierGene.name, Mutation.name, ModifiedPhenotype.description
FROM Gene AS ModifierGene
INNER JOIN GeneticInteraction ON ModifierGene.id = GeneticInteraction.modifier_gene_id
INNER JOIN Mutation ON GeneticInteraction.mutation_id = Mutation.id
INNER JOIN Phenotype AS ModifiedPhenotype ON Mutation.phenotype_id = ModifiedPhenotype.id
WHERE Mutation.name = 'specific_mutation'
*/


%!  genetic_modifier(+ModifierGeneName, +MutationName, +PhenotypeDescription) is nondet.
%
%   Identifies genetic modifiers of a specific mutation and their impact on phenotype.
%
%   This predicate finds genes (ModifierGeneName) that act as genetic modifiers 
%   for a specific mutation (MutationName). It also retrieves the associated 
%   phenotype (PhenotypeDescription) modified by the mutation. The mutation is 
%   restricted to the one named 'specific_mutation'.
%
%   The logic corresponds to an SQL query that joins gene, genetic interaction, 
%   mutation, and phenotype tables to identify relevant genetic modifiers.
%
%   @arg ModifierGeneName     The name of the gene acting as a genetic modifier.
%   @arg MutationName         The name of the mutation being modified (must be 'specific_mutation').
%   @arg PhenotypeDescription The description of the phenotype modified by the mutation.
%
%   @examples
%     % Example query to find genetic modifiers of 'specific_mutation':
%     ?- genetic_modifier(ModifierGene, 'specific_mutation', Phenotype).
%     ModifierGeneName = 'ModifierGeneX',
%     MutationName = 'specific_mutation',
%     PhenotypeDescription = 'PhenotypeY'.
%
genetic_modifier(ModifierGeneName, MutationName, PhenotypeDescription) :-
    % Retrieve the ID and name of the genetic modifier gene.
    gene(ModifierGeneId, ModifierGeneName),
    % Find the genetic interaction linking the modifier gene to a mutation.
    genetic_interaction(ModifierGeneId, MutationId),
    % Retrieve the ID and name of the mutation.
    mutation(MutationId, MutationName),
    % Restrict to the specific mutation of interest.
    MutationName = 'specific_mutation',
    % Retrieve the phenotype associated with the mutation.
    phenotype(MutationId, PhenotypeDescription).

/*
4. Gene Ontology (GO) Annotations
SQL Version:

SELECT Gene.name, GOAnnotation.GO_term, GOAnnotation.description
FROM Gene
INNER JOIN GOAnnotation ON Gene.id = GOAnnotation.gene_id
WHERE Gene.name IN ('gene1', 'gene2', 'gene3')
*/

%!  gene_go_annotation(+GeneName, +GOTerm, +GODescription) is nondet.
%
%   Retrieves Gene Ontology (GO) annotations for a specific set of genes.
%
%   This predicate identifies GO annotations (GOTerm and GODescription) associated 
%   with a given set of genes. The genes are restricted to a predefined list 
%   ('gene1', 'gene2', 'gene3'). The logic corresponds to an SQL query that 
%   joins gene and GO annotation tables with a filter on gene names.
%
%   @arg GeneName       The name of the gene for which GO annotations are retrieved.
%   @arg GOTerm         The Gene Ontology term associated with the gene.
%   @arg GODescription  A description of the Gene Ontology term.
%
%   @examples
%     % Example query to find GO annotations for specific genes:
%     ?- gene_go_annotation(Gene, GOTerm, Description).
%     GeneName = 'gene1',
%     GOTerm = 'GO:0001234',
%     GODescription = 'Some biological process'.
%
gene_go_annotation(GeneName, GOTerm, GODescription) :-
    % Retrieve the ID and name of the gene.
    gene(GeneId, GeneName),
    % Ensure the gene name is one of the specified set.
    fb-member(GeneName, ['gene1', 'gene2', 'gene3']),
    % Retrieve the associated GO term and its description.
    go_annotation(GeneId, GOTerm, GODescription).


/*

SELECT Strain.name, StockCenter.name, StockCenter.location
FROM Strain
INNER JOIN StockCenter ON Strain.stock_center_id = StockCenter.id
WHERE Strain.name LIKE '%desired_trait%'
ChatGPT
Translating these SQL queries to Prolog predicates involves creating a set of Prolog facts and rules that mimic the structure and logic of the SQL database and queries. Here's how each of these queries can be represented in Prolog:

 Gene Alleles and Associated Diseases

SELECT Gene.name, Allele.name, Disease.human_disease
FROM Gene
INNER JOIN Allele ON Gene.id = Allele.gene_id
LEFT JOIN Disease ON Allele.disease_id = Disease.id
WHERE Gene.name = 'target_gene_name'
*/


%!  gene_allele_disease(+GeneName, +AlleleName, -HumanDisease) is nondet.
%
%   Identifies alleles of a specific gene and their associated diseases.
%
%   This predicate finds alleles (AlleleName) linked to a specific gene (GeneName)
%   and retrieves the associated human disease (HumanDisease). If no disease is 
%   associated with an allele, the HumanDisease is set to 'No associated disease'. 
%   The logic corresponds to an SQL query that performs a left join between the 
%   gene, allele, and disease tables, filtered by a specific gene name.
%
%   @arg GeneName       The name of the gene being queried (must be 'target_gene_name').
%   @arg AlleleName     The name of the allele associated with the gene.
%   @arg HumanDisease   The human disease associated with the allele, or 'No associated disease'.
%
%   @examples
%     % Example query to find alleles and diseases for the target gene:
%     ?- gene_allele_disease('target_gene_name', AlleleName, HumanDisease).
%     GeneName = 'target_gene_name',
%     AlleleName = 'AlleleX',
%     HumanDisease = 'DiseaseY'.
%
%     % Example result for an allele with no associated disease:
%     GeneName = 'target_gene_name',
%     AlleleName = 'AlleleZ',
%     HumanDisease = 'No associated disease'.
%
gene_allele_disease(GeneName, AlleleName, HumanDisease) :-
    % Retrieve the ID and name of the target gene.
    gene(GeneId, GeneName),
    % Ensure the gene is 'target_gene_name'.
    GeneName = 'target_gene_name',
    % Retrieve the ID and name of the allele associated with the gene.
    allele(GeneId, AlleleId, AlleleName),
    % Attempt to find a disease associated with the allele.
    (   disease(AlleleId, HumanDisease)
    % If no disease is associated, set to 'No associated disease'.
    ;   HumanDisease = 'No associated disease').


/*

 Transcription Factors Regulating a Gene
SQL Version:
SELECT TranscriptionFactor.name, TargetGene.name, Regulation.type
FROM Gene AS TranscriptionFactor
INNER JOIN GeneRegulation AS Regulation ON TranscriptionFactor.id = Regulation.transcription_factor_id
INNER JOIN Gene AS TargetGene ON Regulation.target_gene_id = TargetGene.id
WHERE TargetGene.name = 'specific_target_gene'
*/



%!  transcription_factor_regulation(+TranscriptionFactorName, +TargetGeneName, +RegulationType) is nondet.
%
%   Identifies transcription factors that regulate a specific target gene.
%
%   This predicate finds transcription factors (TranscriptionFactorName) that regulate 
%   a specific target gene (TargetGeneName) and retrieves the type of regulation 
%   (RegulationType). The target gene is restricted to the one named 'specific_target_gene'.
%
%   The logic corresponds to an SQL query that joins the gene and gene regulation tables 
%   to identify transcription factors and their regulatory relationships.
%
%   @arg TranscriptionFactorName The name of the transcription factor.
%   @arg TargetGeneName          The name of the target gene being regulated (must be 'specific_target_gene').
%   @arg RegulationType          The type of regulation (e.g., activation, repression).
%
%   @examples
%     % Example query to find transcription factors regulating 'specific_target_gene':
%     ?- transcription_factor_regulation(TranscriptionFactor, 'specific_target_gene', RegulationType).
%     TranscriptionFactorName = 'TF1',
%     TargetGeneName = 'specific_target_gene',
%     RegulationType = 'activation'.
%
transcription_factor_regulation(TranscriptionFactorName, TargetGeneName, RegulationType) :-
    % Retrieve the ID and name of the transcription factor.
    gene(TranscriptionFactorId, TranscriptionFactorName),
    % Find the regulation linking the transcription factor to the target gene.
    gene_regulation(TranscriptionFactorId, TargetGeneId, RegulationType),
    % Retrieve the ID and name of the target gene.
    gene(TargetGeneId, TargetGeneName),
    % Restrict the target gene to 'specific_target_gene'.
    TargetGeneName = 'specific_target_gene'.

/*

 Publications Related to a Gene Mutation
SQL Version:

SELECT Mutation.name, Publication.title, Publication.authors
FROM Mutation
INNER JOIN MutationPublication ON Mutation.id = MutationPublication.mutation_id
INNER JOIN Publication ON MutationPublication.publication_id = Publication.id
WHERE Mutation.name = 'specific_mutation_name'
*/


%!  mutation_publication_info(+MutationName, +Title, +Authors) is nondet.
%
%   Retrieves publications associated with a specific gene mutation.
%
%   This predicate finds publications (Title and Authors) that are related to a 
%   specific mutation (MutationName). The mutation is restricted to the one named 
%   'specific_mutation_name'.
%
%   The logic corresponds to an SQL query that joins the mutation, mutation publication, 
%   and publication tables to identify relevant publications for the specified mutation.
%
%   @arg MutationName The name of the mutation being queried (must be 'specific_mutation_name').
%   @arg Title        The title of the publication associated with the mutation.
%   @arg Authors      The authors of the publication associated with the mutation.
%
%   @examples
%     % Example query to find publications related to 'specific_mutation_name':
%     ?- mutation_publication_info('specific_mutation_name', Title, Authors).
%     MutationName = 'specific_mutation_name',
%     Title = 'Significant Research on Mutation',
%     Authors = 'Author1, Author2, Author3'.
%
mutation_publication_info(MutationName, Title, Authors) :-
    % Retrieve the ID and name of the mutation.
    mutation(MutationId, MutationName),
    % Restrict the mutation to 'specific_mutation_name'.
    MutationName = 'specific_mutation_name',
    % Link the mutation to its associated publication ID.
    mutation_publication(MutationId, PublicationId),
    % Retrieve the title and authors of the publication.
    publication(PublicationId, Title, Authors).

/*

4. RNAi Knockdown Effects on Phenotype
SQL Version:

SELECT Gene.name, RNAiExperiment.description, Phenotype.description
FROM Gene
INNER JOIN RNAiExperiment ON Gene.id = RNAiExperiment.gene_id
INNER JOIN Phenotype ON RNAiExperiment.phenotype_id = Phenotype.id
WHERE Gene.name = 'gene_of_interest'
*/



%!  rnai_knockdown_phenotype(+GeneName, +RNAiDescription, +PhenotypeDescription) is nondet.
%
%   Identifies the effects of RNAi knockdown on phenotypes for a specific gene.
%
%   This predicate finds RNAi experiments (RNAiDescription) and their associated 
%   phenotypes (PhenotypeDescription) for a specific gene (GeneName). The query 
%   is restricted to the gene named 'gene_of_interest'.
%
%   The logic corresponds to an SQL query that joins the gene, RNAi experiment, 
%   and phenotype tables to identify the RNAi knockdown effects for the specified gene.
%
%   @arg GeneName             The name of the gene being queried (must be 'gene_of_interest').
%   @arg RNAiDescription      The description of the RNAi experiment associated with the gene.
%   @arg PhenotypeDescription The description of the phenotype resulting from the RNAi experiment.
%
%   @examples
%     % Example query to find RNAi knockdown effects for 'gene_of_interest':
%     ?- rnai_knockdown_phenotype('gene_of_interest', RNAiDesc, PhenotypeDesc).
%     GeneName = 'gene_of_interest',
%     RNAiDescription = 'Knockdown of gene in tissue X',
%     PhenotypeDescription = 'Phenotype Y observed'.
%
rnai_knockdown_phenotype(GeneName, RNAiDescription, PhenotypeDescription) :-
    % Retrieve the ID and name of the gene.
    gene(GeneId, GeneName),
    % Restrict the query to the specific gene of interest.
    GeneName = 'gene_of_interest',
    % Retrieve the RNAi experiment details linked to the gene.
    rnai_experiment(GeneId, RNAiId, RNAiDescription),
    % Retrieve the phenotype associated with the RNAi experiment.
    phenotype(RNAiId, PhenotypeDescription).



/*
5. Stock Center Information for Specific Strains

SELECT Strain.name, StockCenter.name, StockCenter.location
FROM Strain
INNER JOIN StockCenter ON Strain.stock_center_id = StockCenter.id
WHERE Strain.name LIKE '%desired_trait%'
*/

%!  stock_center_info(+StrainName, +StockCenterName, +Location) is nondet.
%
%   Retrieves stock center information for specific strains.
%
%   This predicate finds stock centers (StockCenterName and Location) associated 
%   with strains (StrainName) that match a specific trait. The query filters strains 
%   whose names contain the substring 'desired_trait'.
%
%   The logic corresponds to an SQL query that joins the strain and stock center 
%   tables and applies a filtering condition on strain names.
%
%   @arg StrainName       The name of the strain being queried (must contain 'desired_trait').
%   @arg StockCenterName  The name of the stock center associated with the strain.
%   @arg Location         The location of the stock center.
%
%   @examples
%     % Example query to find stock centers for strains with 'desired_trait':
%     ?- stock_center_info(StrainName, StockCenterName, Location).
%     StrainName = 'StrainX_desired_trait',
%     StockCenterName = 'CenterY',
%     Location = 'LocationZ'.
%
stock_center_info(StrainName, StockCenterName, Location) :-
    % Retrieve the ID and name of the strain.
    strain(StrainId, StrainName),
    % Ensure the strain name contains the substring 'desired_trait'.
    sub_string(StrainName, _, _, _, 'desired_trait'),
    % Retrieve the stock center information linked to the strain.
    stock_center(StrainId, StockCenterId, StockCenterName, Location).






%!  flybase_tables(-Tables) is det.
%
%   Provides a list of tables available in the FlyBase database schema.
%
%   This predicate defines a comprehensive list of tables that are part of the FlyBase 
%   database. FlyBase is a database for Drosophila genetics and molecular biology.
%
%   @arg Tables A list of table names representing the FlyBase database schema.
%
%   @examples
%     % Example query to retrieve all FlyBase tables:
%     ?- flybase_tables(Tables).
%     Tables = [
%         analysis, analysisfeature, analysisgrp, analysisgrpmember, analysisprop,
%         audit_chado, cell_line, cell_line_loaderm, cell_line_loadermprop, ...
%     ].
%
flybase_tables([
analysis, analysisfeature, analysisgrp, analysisgrpmember, analysisprop, audit_chado, cell_line, cell_line_loaderm, cell_line_loadermprop,
cell_line_dbxref, cell_line_feature, cell_line_library, cell_line_libraryprop, cell_line_pub, cell_line_relationship, cell_line_strain,
cell_line_strainprop, cell_line_synonym, cell_lineprop, cell_lineprop_pub, contact, cv, loaderm, loaderm_dbxref, loaderm_relationship,
loadermpath, loadermprop, loadermsynonym, db, dbxref, dbxrefprop, eimage, environment, environment_loaderm, expression, expression_loaderm,
expression_loadermprop, expression_image, expression_pub, expressionprop, feature, feature_loaderm, feature_loaderm_dbxref,
feature_loadermprop, feature_dbxref, feature_expression, feature_expressionprop, feature_genotype, feature_grpmember,
feature_grpmember_pub, feature_humanhealth_dbxref, feature_interaction, feature_interaction_pub, feature_interactionprop,
feature_phenotype, feature_pub, feature_pubprop, feature_relationship, feature_relationship_pub, feature_relationshipprop,
feature_relationshipprop_pub, feature_synonym, featureloc, featureloc_pub, featuremap, featuremap_pub, featurepos, featureprop,
featureprop_pub, featurerange, genotype, genotype_loaderm, genotype_loadermprop, genotype_dbxref, genotype_pub, genotype_synonym,
genotypeprop, genotypeprop_pub, grp, grp_loaderm, grp_dbxref, grp_pub, grp_pubprop, grp_relationship, grp_relationship_pub,
grp_relationshipprop, grp_synonym, grpmember, grpmember_loaderm, grpmember_pub, grpmemberprop, grpmemberprop_pub, grpprop,
grpprop_pub, humanhealth, humanhealth_loaderm, humanhealth_loadermprop, humanhealth_dbxref, humanhealth_dbxrefprop,
humanhealth_dbxrefprop_pub, humanhealth_feature, humanhealth_featureprop, humanhealth_phenotype, humanhealth_phenotypeprop,
humanhealth_pub, humanhealth_pubprop, humanhealth_relationship, humanhealth_relationship_pub, humanhealth_synonym, humanhealthprop,
humanhealthprop_pub, interaction, interaction_cell_line, interaction_loaderm, interaction_loadermprop, interaction_expression,
interaction_expressionprop, interaction_group, interaction_group_feature_interaction, interaction_pub, interactionprop,
interactionprop_pub, library, library_loaderm, library_loadermprop, library_dbxref, library_dbxrefprop, library_expression,
library_expressionprop, library_feature, library_featureprop, library_grpmember, library_humanhealth, library_humanhealthprop,
library_interaction, library_pub, library_relationship, library_relationship_pub, library_strain, library_strainprop, library_synonym,
 libraryprop, libraryprop_pub, lock, organism, organism_loaderm, organism_loadermprop, organism_dbxref, organism_grpmember,
 organism_library, organism_pub, organismprop, organismprop_pub, phendesc, phenotype, phenotype_comparison, phenotype_comparison_loaderm,
  phenotype_loaderm, phenstatement, project, pub, pub_dbxref, pub_relationship, pubauthor, pubprop, sql_features, sql_implementation_info,
  sql_parts, sql_sizing, stock, stock_loaderm, stock_dbxref, stock_genotype, stock_pub, stock_relationship, stock_relationship_pub,
  stockcollection, stockcollection_stock, stockcollectionprop, stockprop, stockprop_pub, strain, strain_loaderm, strain_loadermprop,
  strain_dbxref, strain_feature, strain_featureprop, strain_phenotype, strain_phenotypeprop,
strain_pub, strain_relationship, strain_relationship_pub, strain_synonym, strainprop, strainprop_pub, synonym, tableinfo, update_track]).

/*
table_n_type(fbgn_NAseq_Uniprot, 1, gene_symbol, _).
table_n_type(fbgn_NAseq_Uniprot, 2, organism_abbreviation, _).
table_n_type(fbgn_NAseq_Uniprot, 3, primary_FBgn, 'FBgn').
table_n_type(fbgn_NAseq_Uniprot, 4, nucleotide_accession, _).
table_n_type(fbgn_NAseq_Uniprot, 5, na_based_protein_accession, _).
table_n_type(fbgn_NAseq_Uniprot, 6, 'UniprotKB/Swiss-Prot/TrEMBL_accession', _).
table_n_type(fbgn_NAseq_Uniprot, 7, 'EntrezGene', _).
table_n_type(fbgn_NAseq_Uniprot, 8, 'RefSeq_transcripts', _).
table_n_type(fbgn_NAseq_Uniprot, 9, 'RefSeq_proteins', _).
*//*
table_n_type(fbgn_uniprot, 1, primary_FBgn, 'FBgn').
table_n_type(fbgn_uniprot, 2, gene_symbol, _).
table_n_type(fbgn_uniprot, 3, 'CG', _).
table_n_type(fbgn_uniprot, 4, 'UniprotKB/Swiss-Prot/TrEMBL_accession', _).
table_n_type(pmid_fbgn_uniprot, 1, primary_FBgn, 'FBgn').
table_n_type(pmid_fbgn_uniprot, 2, gene_symbol, _).
table_n_type(pmid_fbgn_uniprot, 3, 'CG', _).
table_n_type(pmid_fbgn_uniprot, 4, 'UniprotKB/Swiss-Prot/TrEMBL_accession', _).
*/

%!  ncRNA_genes_fb_scheme(-JSONSchema) is det.
%
%   Provides the JSON schema for RNAcentral ncRNA objects.
%
%   This predicate contains the JSON schema definition for representing ncRNA 
%   (non-coding RNA) objects in RNAcentral. The schema ensures that the entries 
%   adhere to specific requirements, such as including primary identifiers, taxon 
%   IDs, sequence ontology (SO) terms, and DNA sequences. It also includes various 
%   optional properties like names, descriptions, and cross-references.
%
%   @arg JSONSchema A string representing the JSON schema for ncRNA objects.
%
%   @examples
%     % Example query to retrieve the ncRNA schema:
%     ?- ncRNA_genes_fb_scheme(Schema).
%     Schema = '{ "$schema": "http://json-schema.org/draft-04/schema#", ... }'.
%
ncRNA_genes_fb_scheme(
'
{
        "$schema": "http://json-schema.org/draft-04/schema#",
        "title": "RNAcentral ncRNA object",
        "description": "A sequence to import to RNAcentral. These entries must not be pseudogenes or coding RNA transcripts.",
        "id": "ncrna.json#",
        "type": "object",
        "required": ["primaryId", "taxonId", "soTermId", "sequence", "url"],
        "additionalProperties": false,
        "properties": {
                "primaryId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description": "The prefixed primary (MOD) ID for an entity. For internal use, e.g. FLYBASE:FBgn0003301, MGI:87917."
                },
                "taxonId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description" : "The NCBI taxonId for the species of the gene entity."
                },
                "soTermId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description": "The SO Term that represents as best we can, the bioType, or featureType of the object in the file."
                },
                "ecoTermId": {
                        "$ref" : "global-id.json#/properties/globalId",
                        "description": "The ECO Term that represents the evidence for this sequence having the given SO term."
                },
                "sequence": {
                        "$ref": "sequence.json#properties/sequence",
                        "description": "The DNA sequence of this entry."
                },
                "name": {
                        "type": "string",
                        "description": "The name of this sequence."
                },
                "description": {
                        "type": "string",
                        "description": "An informative human readable description. If not provided then it will be generated using the name and species information."
                },
                "symbol": {
                        "type": "string",
                        "description": "The symbol for this ncRNA"
                },
                "symbolSynonyms": {
                        "type": "array",
                        "items": {
                                "type": "string"
                        },
                        "uniqueItems": true,
                        "description": "A list of other symbols for the ncRNA"
                },
                "version": {
                        "description": "The version of this sequence, if any"
                },
                "gene": {
                        "$ref": "gene.json#"
                },
                "crossReferenceIds": {
                        "description":"Collection holding a limited set (for now) of database cross references for each gene.  That set is defined in geneCrossReferences.json dataSoruce enumeration.  NCBI GENE means just the NCBI Gene reference.  UniProtKB is swissprot and trembl.  Ensembl is just the GENE id for now (not transcript nor protein).",
                        "type": "array",
                        "items": {
                                "$ref" : "global-id.json#/properties/globalId"
                        },
                        "uniqueItems": true
                },
                "genomeLocations": {
                        "description":"Collection holding the set of locations for this sequence. This should include the exon/intron structure of the sequence.",
                        "type": "array",
                        "items": {
                                "$ref" : "location.json#"
                        },
                        "uniqueItems": true
                },
                "url": {
                        "type": "string",
                        "format": "uri",
                        "description": "URL to the page for this entry in the database"
                },
                "externalUrl": {
                        "type": "string",
                        "format": "uri",
                        "description": "URL to a synopsis outside the database, like wikipedia."
                },
                "secondaryStructure": {
                        "description": "The secondary structure in dot-bracket notation for this sequence",
                        "$ref": "secondary-structure.json#/properties/dot_bracket"
                },
                "publications": {
                        "description": "List of references to associate with this sequence.",
                        "type": "array",
                        "items": {
                                "anyOf": [
                                        { "$ref": "publications.json#/properties/pubMedId" },
                                        { "$ref": "publications.json#/properties/doi" }
                                ]
                        },
                        "uniqueItems": true
                },
                "localization": {
                        "description": "Name of the organelle or plasmid this sequence is found in",
                        "enum": [
                                "mitochondria",
                                "plastid",
                                "chloroplast",
                                "plasmid"
                        ]
                },
                "sequenceFeatures": {
                        "anticodon": {
                                "description": "Information about the anticodon",
                                "$ref": "anticodon.json#"
                        },
                        "modifications": {
                                "description": "List of the modifications in this sequence",
                                "type": "array",
                                "items": {
                                        "$ref": "modification.json#"
                                },
                                "uniqueItems": true
                        }
                },
                "sourceModel": {
                        "description": "Information about the model used to find this sequence",
                        "$ref" : "global-id.json#/properties/globalId"
                },
                "product": {
                        "description": "The name of the product, this should be more specific than the name of the SO term that was provided",
                        "type": "string"
                },
                "additionalAnnotations": {
                        "isoType": {
                                "description": "The isoType of this tRNA",
                                "enum": [
                                        "Ala",
                                        "Cys",
                                        "Asp",
                                        "Glu",
                                        "Phe",
                                        "Gly",
                                        "His",
                                        "Ile",
                                        "Lys",
                                        "Leu",
                                        "iMet",
                                        "Met",
                                        "Asn",
                                        "Pro",
                                        "Gln",
                                        "Arg",
                                        "Ser",
                                        "Thr",
                                        "Val",
                                        "Trp",
                                        "Tyr"
                                ]
                        }
                },
                "relatedSequences": {
                        "description": "Information about related sequences, like precursors or targets",
                        "type": "array",
                        "items": {
                                "$ref": "related-sequence.json#"
                        },
                        "uniqueItems": true
                },
                "inferredPhylogeny": {
                        "description": "A phylogeny that has been inferred for this sequence",
                        "$ref": "inferred-phylogeny.json#"
                }
        }
}').

%                                        #genotype_symbols           	genotype_FBids	phenotype_name	phenotype_id	qualifier_names	qualifier_ids	reference

% 'dynamic' allows runtime changes to the predicate while 'module_transparent' means that
% each goal associated with a transparent-declared predicate will inherit the context module from its parent goal.
:- dynamic maybe_corisponds/2.
:- dynamic numeric_value_p_n/3.
:- module_transparent maybe_corisponds/2.
:- module_transparent numeric_value_p_n/3.

%!  column_names(+TableName, -Columns) is det.
%
%   Provides the column names for a specific table in the FlyBase schema.
%
%   This predicate defines the structure of various tables by listing their column names. 
%   The `column_names/2` predicate maps the table name (`TableName`) to a list of 
%   column names (`Columns`) that describe the attributes of each table. Some column 
%   definitions include compound structures such as `listOf/1` to represent arrays or 
%   specific formatting details.
%
%   @arg TableName The name of the table being queried.
%   @arg Columns   A list of column names defining the table structure.
%
%   @examples
%     % Example query to retrieve column names for the 'cyto-genetic-seq' table:
%     ?- column_names('cyto-genetic-seq', Columns).
%     Columns = ['Cytogenetic_map_position', 'Genetic_map_position', 
%                'Sequence_coordinates_(release_6)', 'R6_conversion_notes'].
%
%     % Example query to retrieve column names for 'Dmel_enzyme':
%     ?- column_names('Dmel_enzyme', Columns).
%     Columns = [gene_group_id, gene_group_name, listOf(gene_group_GO_id), 
%                listOf(gene_group_GO_name), listOf(gene_group_EC_number), 
%                listOf(gene_group_EC_name), gene_id, gene_symbol, gene_name, 
%                listOf(gene_EC_number), listOf(gene_EC_name)].
%
column_names('cyto-genetic-seq', ['Cytogenetic_map_position', 'Genetic_map_position', 'Sequence_coordinates_(release_6)', 'R6_conversion_notes']).
column_names('Dmel_enzyme', [gene_group_id, gene_group_name, listOf(gene_group_GO_id), listOf(gene_group_GO_name), listOf(gene_group_EC_number), listOf(gene_group_EC_name), gene_id, gene_symbol, gene_name, listOf(gene_EC_number), listOf(gene_EC_name)]).
column_names('scRNA-Seq_gene_expression', ['Pub_ID', 'Pub_miniref', 'Clustering_Analysis_ID', 'Clustering_Analysis_Name', 'Source_Tissue_Sex', 'Source_Tissue_Stage', 'Source_Tissue_Anatomy', 'Cluster_ID', 'Cluster_Name', 'Cluster_Cell_Type_ID', 'Cluster_Cell_Type_Name', 'Gene_ID', 'Gene_Symbol', 'Mean_Expression', 'Spread']).
column_names(allele_genetic_interactions, [allele_symbol, allele_FBal, interaction, 'FBrf']).
column_names(allele_phenotypic,           [allele_symbol, allele_FBal, phenotype, 'FBrf']).
column_names(automated_gene_summaries, [primary_FBgn, summary_text]).
column_names(best_gene_summary, ['FBgn', 'Gene_Symbol', 'Summary_Source', 'Summary']).
column_names(cDNA_clone, ['FBcl', organism_abbreviation, clone_name, dataset_metadata_name, listOf(cDNA_accession), listOf('EST_accession')]).
column_names(dataset_metadata, ['Dataset_Metadata_ID', 'Dataset_Metadata_Name', 'Item_ID', 'Item_Name']).
column_names(disease_model_annotations, ['FBgn', 'Gene_symbol', 'HGNC', 'DO_qualifier', 'DO', 'DO_term', 'Allele_used_in_model_(FBal)', 'Allele_used_in_model_(symbol)', 'Based_on_orthology_with_(HGNC_ID)', 'Based_on_orthology_with_(symbol)', 'Evidence/interacting_alleles', 'Reference_(FBrf)']).
column_names(dmel_gene_sequence_ontology_annotations, [gene_primary_id, gene_symbol, so_term_name, so_term_id]).
column_names(dmel_human_orthologs_disease, ['Dmel_gene', 'Dmel_gene_symbol', 'Human_gene_HGNC', 'Human_gene_OMIM', 'Human_gene_symbol', 'DIOPT_score', 'OMIM_Phenotype_IDs', 'OMIM_Phenotype_IDs[name]']).
column_names(dmel_paralogs, ['FBgn', 'GeneSymbol', 'Arm/Scaffold', 'Location', 'Strand', 'Paralog_FBgn', 'Paralog_GeneSymbol', 'Paralog_Arm/Scaffold', 'Paralog_Location', 'Paralog_Strand', 'DIOPT_score']).
column_names(dmel_unique_protein_isoforms, ['FBgn', 'FB_gene_symbol', representative_protein, listOf(identical_protein)]).
column_names(entity_publication, [entity_id, entity_name, 'FlyBase_publication', 'PubMed']).
column_names(fb_synonym, [primary_FBid, organism_abbreviation, current_symbol, current_fullname, listOf(fullname_synonym, ['|']), listOf(symbol_synonym, ['|'])]).
column_names(fbal_to_fbgn,             ['AlleleID', 'AlleleSymbol', 'GeneID', 'GeneSymbol']).
column_names(fbgn_annotation_ID, [gene_symbol, organism_abbreviation, primary_FBgn, listOf(secondary_FBgn), annotation_ID, listOf(secondary_annotation_ID)]).
column_names(fbgn_exons2affy1_overlaps, ['FBgn', listOf(affy)]).
column_names(fbgn_exons2affy2_overlaps, ['FBgn', listOf(affy)]).
column_names(fbgn_fbtr_fbpp, ['FBgn', 'FBtr', 'FBpp']).
column_names(fbgn_fbtr_fbpp_expanded, [organism, gene_type, gene_ID, gene_symbol, gene_fullname, annotation_ID, transcript_type, transcript_ID, transcript_symbol, polypeptide_ID, polypeptide_symbol]).
column_names(fbgn_gleanr, [organism_abbreviation, gene_symbol, primary_FBgn, 'GLEANR']).
column_names(fbgn_NAseq_Uniprot, [gene_symbol, organism_abbreviation, primary_FBgn, nucleotide_accession, na_based_protein_accession, 'UniprotKB/Swiss-Prot/TrEMBL_accession', 'EntrezGene', 'RefSeq_transcripts', 'RefSeq_proteins']).
column_names(fbgn_uniprot, [primary_FBgn, gene_symbol, 'CG', 'UniprotKB/Swiss-Prot/TrEMBL_accession']).
column_names(fbrf_pmid_pmcid_doi, ['FBrf', 'PMID', 'PMCID', 'DOI', pub_type, miniref, pmid_added]).
column_names(gene_functional_complementation, ['Dmel_gene_symbol', 'Dmel_gene_FBgn', ortholog_gene_symbol, ortholog_gene_FBgn_ID, reference_FBrf]).
column_names(gene_genetic_interactions, [listOf('Starting_gene_symbol'), listOf('Starting_gene_FBgn'), listOf('Interacting_gene_symbol'), listOf('Interacting_gene_FBgn'), 'Interaction_type', 'Publication_FBrf']).
column_names(gene_group, ['FB_group', 'FB_group_symbol', 'FB_group_name', 'Parent_FB_group', 'Parent_FB_group_symbol', 'Group_member_FB_gene', 'Group_member_FB_gene_symbol']).
column_names(gene_groups_HGNC, ['FB_group', 'FB_group_symbol', 'FB_group_name', 'HGNC_family']).
column_names(gene_map_table, [organism_abbreviation, current_symbol, primary_FBid, recombination_loc, cytogenetic_loc, sequence_loc]).
column_names(gene_rpkm_matrix, [gene_primary_id, gene_symbol, gene_fullname, gene_type, 'DATASAMPLE_NAME_(DATASET_ID)']).
column_names(gene_rpkm_report, ['Release_ID', 'FBgn', 'GeneSymbol', 'Parent_library_FBlc', 'Parent_library_name', 'RNASource_FBlc', 'RNASource_name', 'RPKM_value', 'Bin_value', 'Unique_exon_base_count', 'Total_exon_base_count', 'Count_used']).
column_names(gene_snapshots, ['FBgn', 'GeneSymbol', 'GeneName', datestamp, gene_snapshot_text]).
column_names(genomic_clone, ['FBcl', organism_abbreviation, clone_name, accession]).
column_names(genotype_phenotype_data, [listOf(genotype_symbols, [/, ' ']), listOf(genotype_FBids, [/, ' ']), phenotype_name, phenotype_id, listOf(qualifier_names, ['|']), listOf(qualifier_ids, ['|']), reference]).
column_names(insertion_mapping, [insertion_symbol, 'FBti', genomic_location, range, orientation, estimated_cytogenetic_location, observed_cytogenetic_location]).
column_names(organism_list, [genus, species, abbreviation, common_name, 'NCBI_taxon', 'drosophilid?']).
column_names(pathway_group, ['FB_group', 'FB_group_symbol', 'FB_group_name', 'Parent_FB_group', 'Parent_FB_group_symbol', 'Group_member_FB_gene', 'Group_member_FB_gene_symbol']).
column_names(physical_interactions_mitab, [listOf('ID_Interactor_A'), listOf('ID_Interactor_B'), listOf('Alt_ID_Interactor_A'), listOf('Alt_ID_Interactor_B'), listOf('Alias_Interactor_A'), listOf('Alias_Interactor_B'), listOf('Interaction_Detection_Method'), listOf('Publication_1st_Author'), listOf('Publication'), 'Taxid_Interactor_A', 'Taxid_Interactor_B', listOf('Interaction_Type'), listOf('Source_Database'), listOf('Interaction_Identifier'), listOf('Confidence_Value'), listOf('Expansion_Method'), listOf('Biological_Role_Interactor_A'), listOf('Biological_Role_Interactor_B'), listOf('Experimental_Role_Interactor_A'), listOf('Experimental_Role_Interactor_B'), listOf('Type_Interactor_A'), listOf('Type_Interactor_B'), listOf('Xref_Interactor_A'), listOf('Xref_Interactor_B'), listOf('Interaction_Xref'), listOf('Annotation_Interactor_A'), listOf('Annotation_Interactor_B'), listOf('Interaction_Annotation'), listOf('Host_Organism'), 'Interaction_Parameters', 'Creation_Date', 'Update_Date', 'Checksum_Interactor_A', 'Checksum_Interactor_B', 'Interaction_Checksum', 'Negative', listOf('Feature_Interactor_A'), listOf('Feature_Interactor_B'), 'Stoichiometry_Interactor_A', 'Stoichiometry_Interactor_B', listOf('Identification_Method_Participant_A'), listOf('Identification_Method_Participant_B')]).
column_names(pmid_fbgn_uniprot, ['FBrf', 'PMID', 'FBgn', 'UniProt_database', 'UniProt_id']).
column_names(synonym, [primary_FBid, organism_abbreviation, current_symbol, current_fullname, listOf(fullname_synonym), listOf(symbol_synonym)]).

%!  column_names_ext(+TableName, -Columns) is det.
%
%   Provides the extended column names for specific tables in the FlyBase schema.
%
%   This predicate defines the structure of specific tables by listing their extended 
%   column names, including additional formatting or structures such as `listOf/2`.
%   It expands on the basic `column_names/2` predicate by accommodating more complex 
%   tables and datasets.
%
%   @arg TableName The name of the table being queried.
%   @arg Columns   A list of extended column names defining the table structure.
%
%   @examples
%     % Example query to retrieve extended column names for 'gene_genetic_interactions':
%     ?- column_names_ext(gene_genetic_interactions, Columns).
%     Columns = [listOf('Starting_gene_symbol', ['|']), 
%                listOf('Starting_gene_FBgn', ['|']), 
%                listOf('Interacting_gene_symbol', ['|']), 
%                listOf('Interacting_gene_FBgn', ['|']), 
%                'Interaction_type', 'Publication_FBrf'].
%
%     % Example query to retrieve extended column names for 'gene_rpkm_matrix':
%     ?- column_names_ext(gene_rpkm_matrix, Columns).
%     Columns = [gene_primary_id, gene_symbol, gene_fullname, gene_type, 
%                'BCM_1_E2-4hr_(FBlc0000061)', 'BCM_1_E14-16hr_(FBlc0000062)', ...].
%
column_names_ext(gene_genetic_interactions, [listOf('Starting_gene_symbol', ['|']), listOf('Starting_gene_FBgn', ['|']), listOf('Interacting_gene_symbol', ['|']), listOf('Interacting_gene_FBgn', ['|']), 'Interaction_type', 'Publication_FBrf']).
column_names_ext(gene_rpkm_matrix, [gene_primary_id, gene_symbol, gene_fullname, gene_type, 'BCM_1_E2-4hr_(FBlc0000061)', 'BCM_1_E14-16hr_(FBlc0000062)', 'BCM_1_E2-16hr_(FBlc0000063)', 'BCM_1_E2-16hr100_(FBlc0000064)', 'BCM_1_L3i_(FBlc0000065)', 'BCM_1_L3i100_(FBlc0000066)', 'BCM_1_P3d_(FBlc0000067)', 'BCM_1_FA3d_(FBlc0000068)', 'BCM_1_MA3d_(FBlc0000069)', 'BCM_1_P_(FBlc0000070)', 'BCM_1_L_(FBlc0000071)', 'BCM_1_A17d_(FBlc0000072)', 'mE_mRNA_em0-2hr_(FBlc0000086)', 'mE_mRNA_em2-4hr_(FBlc0000087)', 'mE_mRNA_em4-6hr_(FBlc0000088)', 'mE_mRNA_em6-8hr_(FBlc0000089)', 'mE_mRNA_em8-10hr_(FBlc0000090)', 'mE_mRNA_em10-12hr_(FBlc0000091)', 'mE_mRNA_em12-14hr_(FBlc0000092)', 'mE_mRNA_em14-16hr_(FBlc0000093)', 'mE_mRNA_em16-18hr_(FBlc0000094)', 'mE_mRNA_em18-20hr_(FBlc0000095)', 'mE_mRNA_em20-22hr_(FBlc0000096)', 'mE_mRNA_em22-24hr_(FBlc0000097)', 'mE_mRNA_L1_(FBlc0000098)', 'mE_mRNA_L2_(FBlc0000099)', 'mE_mRNA_L3_12hr_(FBlc0000100)', 'mE_mRNA_L3_PS1-2_(FBlc0000101)', 'mE_mRNA_L3_PS3-6_(FBlc0000102)', 'mE_mRNA_L3_PS7-9_(FBlc0000103)', 'mE_mRNA_WPP_(FBlc0000104)', 'mE_mRNA_P5_(FBlc0000105)', 'mE_mRNA_P6_(FBlc0000106)', 'mE_mRNA_P8_(FBlc0000107)', 'mE_mRNA_P9-10_(FBlc0000108)', 'mE_mRNA_P15_(FBlc0000109)', 'mE_mRNA_AdF_Ecl_1days_(FBlc0000110)', 'mE_mRNA_AdF_Ecl_5days_(FBlc0000111)', 'mE_mRNA_AdF_Ecl_30days_(FBlc0000112)', 'mE_mRNA_AdM_Ecl_1days_(FBlc0000113)', 'mE_mRNA_AdM_Ecl_5days_(FBlc0000114)', 'mE_mRNA_AdM_Ecl_30days_(FBlc0000115)', 'mE_mRNA_A_MateF_1d_head_(FBlc0000207)', 'mE_mRNA_A_MateF_4d_ovary_(FBlc0000208)', 'mE_mRNA_A_MateM_1d_head_(FBlc0000209)', 'mE_mRNA_A_VirF_1d_head_(FBlc0000210)', 'mE_mRNA_A_VirF_4d_head_(FBlc0000211)', 'mE_mRNA_A_MateF_20d_head_(FBlc0000212)', 'mE_mRNA_A_MateF_4d_head_(FBlc0000213)', 'mE_mRNA_A_MateM_20d_head_(FBlc0000214)', 'mE_mRNA_A_MateM_4d_acc_gland_(FBlc0000215)', 'mE_mRNA_A_MateM_4d_head_(FBlc0000216)', 'mE_mRNA_A_MateM_4d_testis_(FBlc0000217)', 'mE_mRNA_A_1d_carcass_(FBlc0000218)', 'mE_mRNA_A_1d_dig_sys_(FBlc0000219)', 'mE_mRNA_A_20d_carcass_(FBlc0000220)', 'mE_mRNA_A_20d_dig_sys_(FBlc0000221)', 'mE_mRNA_A_4d_carcass_(FBlc0000222)', 'mE_mRNA_A_4d_dig_sys_(FBlc0000223)', 'mE_mRNA_P8_CNS_(FBlc0000224)', 'mE_mRNA_L3_CNS_(FBlc0000225)', 'mE_mRNA_L3_Wand_carcass_(FBlc0000226)', 'mE_mRNA_L3_Wand_dig_sys_(FBlc0000227)', 'mE_mRNA_L3_Wand_fat_(FBlc0000228)', 'mE_mRNA_L3_Wand_imag_disc_(FBlc0000229)', 'mE_mRNA_L3_Wand_saliv_(FBlc0000230)', 'mE_mRNA_A_VirF_20d_head_(FBlc0000231)', 'mE_mRNA_A_VirF_4d_ovary_(FBlc0000232)', 'mE_mRNA_WPP_fat_(FBlc0000233)', 'mE_mRNA_WPP_saliv_(FBlc0000234)', 'mE_mRNA_P8_fat_(FBlc0000235)', 'mE_mRNA_A_4d_Cold1_(FBlc0000237)', 'mE_mRNA_A_4d_Cold2_(FBlc0000238)', 'mE_mRNA_L3_Cu_0.5mM_(FBlc0000239)', 'mE_mRNA_L3_late_Zn_5mM_(FBlc0000240)', 'mE_mRNA_A_4d_Cu_15mM_(FBlc0000241)', 'mE_mRNA_A_4d_Zn_4.5mM_(FBlc0000242)', 'mE_mRNA_A_4d_Caffeine_25mg/ml_(FBlc0000243)', 'mE_mRNA_A_4d_Caffeine_2.5mg/ml_(FBlc0000244)', 'mE_mRNA_L3_Caffeine_1.5mg/ml_(FBlc0000245)', 'mE_mRNA_A_4d_Cd_0.1M_(FBlc0000246)', 'mE_mRNA_A_4d_Cd_0.05M_(FBlc0000247)', 'mE_mRNA_L3_Cd_12h_(FBlc0000248)', 'mE_mRNA_L3_Cd_6hr_(FBlc0000249)', 'mE_mRNA_A_4d_Paraquat_5mM_(FBlc0000250)', 'mE_mRNA_A_4d_Paraquat_10mM_(FBlc0000251)', 'mE_mRNA_L3_Rotenone_8ug_(FBlc0000252)', 'mE_mRNA_L3_Rotenone_2ug_(FBlc0000253)', 'mE_mRNA_L3_EtOH_10_(FBlc0000254)', 'mE_mRNA_L3_EtOH_5_(FBlc0000255)', 'mE_mRNA_L3_EtOH_2.5_(FBlc0000256)', 'mE_mRNA_A_4d_Heatshock_(FBlc0000257)', 'mE_mRNA_A_10d_Resveratrol_100uM_(FBlc0000672)', 'mE_mRNA_A_10d_Rotenone_Starved_(FBlc0000673)', 'mE_mRNA_F_Sindbis_virus_(FBlc0000674)', 'mE_mRNA_L_Sindbis_virus_(FBlc0000675)', 'mE_mRNA_M_Sindbis_virus_(FBlc0000676)', 'mE_mRNA_P_Sindbis_virus_(FBlc0000677)', 'mE_mRNA_CME-W2_cells_(FBlc0000261)', 'mE_mRNA_GM2_cells_(FBlc0000262)', 'mE_mRNA_mbn2_cells_(FBlc0000263)', 'mE_mRNA_BG2-c2_cells_(FBlc0000264)', 'mE_mRNA_D20-c5_cells_(FBlc0000265)', 'mE_mRNA_S3_cells_(FBlc0000266)', 'mE_mRNA_1182-4H_cells_(FBlc0000267)', 'mE_mRNA_CME_L1_cells_(FBlc0000268)', 'mE_mRNA_Kc167_cells_(FBlc0000269)', 'mE_mRNA_BG1-c1_cells_(FBlc0000270)', 'mE_mRNA_D11_cells_(FBlc0000271)', 'mE_mRNA_D16-c3_cells_(FBlc0000272)', 'mE_mRNA_D17-c3_cells_(FBlc0000273)', 'mE_mRNA_D21_cells_(FBlc0000274)', 'mE_mRNA_D32_cells_(FBlc0000275)', 'mE_mRNA_D4-c1_cells_(FBlc0000276)', 'mE_mRNA_D8_cells_(FBlc0000277)', 'mE_mRNA_D9_cells_(FBlc0000278)', 'mE_mRNA_S1_cells_(FBlc0000279)', 'mE_mRNA_S2R+_cells_(FBlc0000280)', 'mE_mRNA_Sg4_cells_(FBlc0000281)', 'mE_mRNA_OSS_cells_(FBlc0000282)', 'mE_mRNA_OSC_cells_(FBlc0000283)', 'mE_mRNA_fGS_cells_(FBlc0000284)', 'Knoblich_mRNA_L3_CNS_neuroblast_(FBlc0000505)', 'Knoblich_mRNA_L3_CNS_neuron_(FBlc0000506)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Brain_(FBlc0003619)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Crop_(FBlc0003620)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Carcass_(FBlc0003621)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Eye_(FBlc0003622)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_FatBody_(FBlc0003623)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Head_(FBlc0003624)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Hindgut_(FBlc0003625)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Midgut_(FBlc0003626)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Ovary_(FBlc0003627)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_RectalPad_(FBlc0003628)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_SalivaryGland_(FBlc0003629)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_ThoracicoAbdominalGanglion_(FBlc0003630)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_MalpighianTubule_(FBlc0003631)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Mated_Spermathecum_(FBlc0003632)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Virgin_Spermathecum_(FBlc0003633)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Whole_(FBlc0003634)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Brain_(FBlc0003635)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Crop_(FBlc0003636)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Carcass_(FBlc0003637)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Eye_(FBlc0003638)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_FatBody_(FBlc0003639)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Head_(FBlc0003640)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Hindgut_(FBlc0003641)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Midgut_(FBlc0003642)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_RectalPad_(FBlc0003643)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_SalivaryGland_(FBlc0003644)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_ThoracicoAbdominalGanglion_(FBlc0003645)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_MalpighianTubule_(FBlc0003646)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Testis_(FBlc0003647)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_AccessoryGland_(FBlc0003648)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Whole_(FBlc0003649)', 'RNA-Seq_Profile_FlyAtlas2_L3_CNS_(FBlc0003650)', 'RNA-Seq_Profile_FlyAtlas2_L3_FatBody_(FBlc0003651)', 'RNA-Seq_Profile_FlyAtlas2_L3_Hindgut_(FBlc0003652)', 'RNA-Seq_Profile_FlyAtlas2_L3_MalpighianTubule_(FBlc0003653)', 'RNA-Seq_Profile_FlyAtlas2_L3_Midgut_(FBlc0003654)', 'RNA-Seq_Profile_FlyAtlas2_L3_SalivaryGland_(FBlc0003655)', 'RNA-Seq_Profile_FlyAtlas2_L3_Trachea_(FBlc0003656)', 'RNA-Seq_Profile_FlyAtlas2_L3_Carcass_(FBlc0003657)', 'RNA-Seq_Profile_FlyAtlas2_L3_Whole_(FBlc0003658)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Heart_(FBlc0003724)', 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Heart_(FBlc0003725)']).
column_names_ext(pmid_fbgn_uniprot, ['FBrf_id', 'PMID', 'FBgn_id', 'UniProt_database', 'UniProt_id']).

%!  guess_rest(+Predicate, +ArgIndex, +Type, -Guess) is nondet.
%
%   Makes a guess for a missing value in a specific argument position of a predicate.
%
%   This predicate attempts to determine a value (`Guess`) for a given argument position 
%   (`ArgIndex`) in a predicate (`Predicate`). The guess is constrained by the argument's 
%   expected type (`Type`) and is generated by analyzing the table structure and calling 
%   the predicate dynamically.
%
%   The process involves:
%   1. Using `table_n_type/4` to infer the possible types of arguments in the predicate.
%   2. Ensuring that `Guess` is initially unbound (`var(Guess)`).
%   3. Determining the arity of the predicate using `fb_pred_nr/2`.
%   4. Constructing a callable term using `functor/3` and setting the target argument 
%      position using `arg/3`.
%   5. Dynamically calling the predicate with `call/1` and extracting the value of `Guess`.
%
%   @arg Predicate The name of the predicate being queried.
%   @arg ArgIndex  The index of the argument to guess (1-based).
%   @arg Type      The expected type of the argument.
%   @arg Guess     The guessed value for the argument.
%
%   @examples
%     % Example query to guess a value for the second argument of a predicate:
%     ?- guess_rest('some_predicate', 2, 'integer', Guess).
%     Guess = 42.
%
guess_rest(P, N, T, Guess) :-
    % Use the table structure to infer the argument type and prepare a guess.
    table_n_type(P, N, T, Guess),
    % Ensure that the guess is unbound.
    var(Guess),
    % Retrieve the arity of the predicate.
    fb_pred_nr(P, A),
    % Construct a callable term for the predicate.
    functor(C, P, A),
    % Set the target argument position to guess.
    arg(N, C, Guess),
    % Dynamically call the predicate and extract the value.
    once(call(C)).

%!  maybe_corisponds(+ConceptA, +ConceptB) is det.
%
%   Defines potential correspondences between two conceptual mappings.
%
%   The predicate `maybe_corisponds/2` identifies potential relationships between 
%   two conceptual mappings. These mappings are defined using the `ConceptMapFn/3` 
%   functor, which describes how data fields or properties in different data sources 
%   might correspond to one another.
%
%   The mappings include field names, field indices, and the data source (with 
%   arity) from which the fields originate. These correspondences are particularly 
%   useful for establishing links between disparate data sources, such as databases 
%   or file formats.
%
%   @arg ConceptA The first conceptual mapping, defined as `ConceptMapFn(Field, Index, Source/Arity)`.
%   @arg ConceptB The second conceptual mapping, also defined as `ConceptMapFn(Field, Index, Source/Arity)`.
%
%   @examples
%     % Example: Checking if two concepts correspond:
%     ?- maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12),
%                         'ConceptMapFn'('FBgn', 1, dmel_paralogs/11)).
%     true.
%
%     % Example: Iterating over all correspondences:
%     ?- maybe_corisponds(ConceptA, ConceptB).
%     ConceptA = 'ConceptMapFn'('FBgn', 1, disease_model_annotations/12),
%     ConceptB = 'ConceptMapFn'('FBgn', 1, dmel_paralogs/11).
%
maybe_corisponds('ConceptMapFn'('Allele_used_in_model_(symbol)', 8, disease_model_annotations/12), 'ConceptMapFn'(current_symbol, 3, synonym/6)).
maybe_corisponds('ConceptMapFn'('Allele_used_in_model_(symbol)', 8, disease_model_annotations/12), 'ConceptMapFn'(description, 6, stocks/7)).
maybe_corisponds('ConceptMapFn'('Allele_used_in_model_(symbol)', 8, disease_model_annotations/12), 'ConceptMapFn'(uniquename, 5, stocks/7)).
maybe_corisponds('ConceptMapFn'('AlleleSymbol', 2, fbal_to_fbgn/4), 'ConceptMapFn'('Allele_used_in_model_(symbol)', 8, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('Based_on_orthology_with_(symbol)', 10, disease_model_annotations/12), 'ConceptMapFn'('Human_gene_symbol', 5, dmel_human_orthologs_disease/8)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('GeneName', 3, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FB_group_name', 3, gene_group/7), 'ConceptMapFn'('FB_group_name', 3, gene_groups_HGNC/4)).
maybe_corisponds('ConceptMapFn'('FB_group_name', 3, gene_group/7), 'ConceptMapFn'(gene_group_name, 2, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FB_group_name', 3, gene_groups_HGNC/4), 'ConceptMapFn'('FB_group_name', 3, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FB_group_name', 3, gene_groups_HGNC/4), 'ConceptMapFn'(gene_group_name, 2, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FB_group_symbol', 2, gene_group/7), 'ConceptMapFn'('FB_group_symbol', 2, gene_groups_HGNC/4)).
maybe_corisponds('ConceptMapFn'('FB_group_symbol', 2, gene_groups_HGNC/4), 'ConceptMapFn'('FB_group_symbol', 2, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FB_group_symbol', 2, gene_groups_HGNC/4), 'ConceptMapFn'('Parent_FB_group_symbol', 5, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FB_group_symbol', 2, gene_groups_HGNC/4), 'ConceptMapFn'('Parent_FB_group_symbol', 5, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBal', 1, fbal_to_fbgn/4), 'ConceptMapFn'('FBal', 7, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBal', 7, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, synonym/6)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgg', 1, gene_group/7), 'ConceptMapFn'('FBgg', 1, gene_groups_HGNC/4)).
maybe_corisponds('ConceptMapFn'('FBgg', 1, gene_group/7), 'ConceptMapFn'(gene_group_id, 1, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgg', 1, gene_groups_HGNC/4), 'ConceptMapFn'('FBgg', 1, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgg', 1, gene_groups_HGNC/4), 'ConceptMapFn'('FBgg', 4, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgg', 1, gene_groups_HGNC/4), 'ConceptMapFn'('FBgg', 4, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgg', 1, gene_groups_HGNC/4), 'ConceptMapFn'(gene_group_id, 1, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, automated_gene_summaries/2), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, automated_gene_summaries/2), 'ConceptMapFn'('FBgn', 4, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, best_gene_summary/4), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, disease_model_annotations/12), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_paralogs/11), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 1, gene_snapshots/5), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_functional_complementation/5), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 2, gene_rpkm_report/12), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 4, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, gene_map_table/6), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, gene_map_table/6), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, gene_map_table/6), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, gene_map_table/6), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, gene_map_table/6), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 3, gene_map_table/6), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, dmel_paralogs/11), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, dmel_paralogs/11), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, gene_group/7), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, gene_group/7), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, gene_group/7), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, pathway_group/7), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('FBgn', 6, pathway_group/7), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('FBpp', 3, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBpp', 10, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('FBrf', 1, fbrf_pmid_pmcid_doi/7), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBrf', 1, fbrf_pmid_pmcid_doi/7), 'ConceptMapFn'('FBrf', 5, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBrf', 1, fbrf_pmid_pmcid_doi/7), 'ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBrf', 3, entity_publication/4), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBrf', 3, entity_publication/4), 'ConceptMapFn'('FBrf', 4, allele_genetic_interactions/4)).
maybe_corisponds('ConceptMapFn'('FBrf', 3, entity_publication/4), 'ConceptMapFn'('FBrf', 5, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBrf', 3, entity_publication/4), 'ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBrf', 4, allele_genetic_interactions/4), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBrf', 4, allele_genetic_interactions/4), 'ConceptMapFn'('FBrf', 5, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('FBrf', 4, allele_genetic_interactions/4), 'ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBrf', 5, gene_functional_complementation/5), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBrf', 5, gene_functional_complementation/5), 'ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('FBtr', 2, fbgn_fbtr_fbpp/3), 'ConceptMapFn'('FBtr', 8, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'(gene_fullname, 5, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'(gene_name, 9, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('GeneName', 3, gene_snapshots/5), 'ConceptMapFn'(species, 2, organism_list/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'('GeneSymbol', 3, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 3, gene_rpkm_report/12), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 3, gene_rpkm_report/12), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 3, gene_rpkm_report/12), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 3, gene_rpkm_report/12), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4), 'ConceptMapFn'(ortholog_gene_symbol, 3, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'('Genetic_map_position', 2, 'cyto-genetic-seq'/4), 'ConceptMapFn'(recombination_loc, 4, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(gene_name, 9, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_name, 9, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('HGNC', 3, dmel_human_orthologs_disease/8), 'ConceptMapFn'('HGNC', 9, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'('Summary', 4, best_gene_summary/4), 'ConceptMapFn'(gene_snapshot_text, 5, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('GeneSymbol', 3, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 1, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 2, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 6, gene_group/7)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 6, pathway_group/7)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9)).
maybe_corisponds('ConceptMapFn'(_, 1, fbgn_uniprot/4), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(_, 1, genotype_phenotype/7), 'ConceptMapFn'('AlleleSymbol', 2, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'(_, 1, genotype_phenotype/7), 'ConceptMapFn'(allele_symbol, 1, allele_genetic_interactions/4)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBrf', 1, fbrf_pmid_pmcid_doi/7)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBrf', 3, entity_publication/4)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBrf', 4, allele_genetic_interactions/4)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBrf', 5, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 1, pmid_fbgn_uniprot/5), 'ConceptMapFn'(_, 7, genotype_phenotype/7)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Dmel_gene_symbol', 1, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Dmel_gene_symbol', 2, dmel_human_orthologs_disease/8)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Gene_Symbol', 2, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Gene_symbol', 2, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('GeneSymbol', 2, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('GeneSymbol', 2, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('GeneSymbol', 4, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, gene_group/7)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Interacting_gene_symbol', 3, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Paralog_GeneSymbol', 7, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'('Starting_gene_symbol', 1, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(_, 1, fbgn_NAseq_Uniprot/9)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 2, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(_, 2, genotype_phenotype/7), 'ConceptMapFn'('FBal', 1, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'(_, 2, genotype_phenotype/7), 'ConceptMapFn'('FBal', 2, allele_genetic_interactions/4)).
maybe_corisponds('ConceptMapFn'(_, 2, pmid_fbgn_uniprot/5), 'ConceptMapFn'('PMID', 2, fbrf_pmid_pmcid_doi/7)).
maybe_corisponds('ConceptMapFn'(_, 2, pmid_fbgn_uniprot/5), 'ConceptMapFn'('PubMed', 4, entity_publication/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('Dmel_gene', 1, dmel_human_orthologs_disease/8)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, best_gene_summary/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 1, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 2, gene_rpkm_report/12)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 3, fbal_to_fbgn/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 4, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 6, dmel_paralogs/11)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'('FB_gene_symbol', 2, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'('FBan', 5, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'('Group_member_FB_gene_symbol', 7, pathway_group/7)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(_, 3, fbgn_uniprot/4), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 3, genotype_phenotype/7), 'ConceptMapFn'('GeneName', 3, gene_snapshots/5)).
maybe_corisponds('ConceptMapFn'(_, 3, genotype_phenotype/7), 'ConceptMapFn'(current_fullname, 4, synonym/6)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 1, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 1, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy1_overlaps/2)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 1, fbgn_exons2affy2_overlaps/2)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 1, fbgn_fbtr_fbpp/3)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 2, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 3, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 3, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 3, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'('FBgn', 7, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'(_, 1, fbgn_uniprot/4)).
maybe_corisponds('ConceptMapFn'(_, 3, pmid_fbgn_uniprot/5), 'ConceptMapFn'(gene_primary_id, 1, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(_, 4, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'('EST_accession', 6, cDNA_clone/6)).
maybe_corisponds('ConceptMapFn'(_, 4, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(accession, 4, genomic_clone/4)).
maybe_corisponds('ConceptMapFn'(_, 4, fbgn_NAseq_Uniprot/9), 'ConceptMapFn'(clone_name, 3, cDNA_clone/6)).
maybe_corisponds('ConceptMapFn'(_, 4, fbgn_uniprot/4), 'ConceptMapFn'(_, 6, fbgn_NAseq_Uniprot/9)).
maybe_corisponds('ConceptMapFn'(_, 5, pmid_fbgn_uniprot/5), 'ConceptMapFn'(_, 4, fbgn_uniprot/4)).
maybe_corisponds('ConceptMapFn'(_, 7, genotype_phenotype/7), 'ConceptMapFn'('FBrf', 12, disease_model_annotations/12)).
maybe_corisponds('ConceptMapFn'(_, 7, genotype_phenotype/7), 'ConceptMapFn'('FBrf', 3, entity_publication/4)).
maybe_corisponds('ConceptMapFn'(_, 7, genotype_phenotype/7), 'ConceptMapFn'('FBrf', 4, allele_genetic_interactions/4)).
maybe_corisponds('ConceptMapFn'(_, 7, genotype_phenotype/7), 'ConceptMapFn'('FBrf', 5, gene_functional_complementation/5)).
maybe_corisponds('ConceptMapFn'(_, 7, genotype_phenotype/7), 'ConceptMapFn'('FBrf', 6, gene_genetic_interactions/6)).
maybe_corisponds('ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(current_symbol, 2, gene_map_table/6)).
maybe_corisponds('ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(annotation_ID, 6, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(current_symbol, 2, gene_map_table/6), 'ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6)).
maybe_corisponds('ConceptMapFn'(current_symbol, 2, gene_map_table/6), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(current_symbol, 2, gene_map_table/6), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(current_symbol, 2, gene_map_table/6), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(current_symbol, 2, gene_map_table/6), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(current_symbol, 3, synonym/6), 'ConceptMapFn'(description, 6, stocks/7)).
maybe_corisponds('ConceptMapFn'(current_symbol, 3, synonym/6), 'ConceptMapFn'(uniquename, 5, stocks/7)).
maybe_corisponds('ConceptMapFn'(cytogenetic_loc, 5, gene_map_table/6), 'ConceptMapFn'(estimated_cytogenetic_location, 6, insertion_mapping/7)).
maybe_corisponds('ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170), 'ConceptMapFn'(gene_fullname, 5, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(gene_fullname, 3, gene_rpkm_matrix/170), 'ConceptMapFn'(gene_name, 9, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(gene_fullname, 5, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(gene_name, 9, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 1, fbgn_annotation_ID/6), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4), 'ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 2, dmel_gene_sequence_ontology_annotations/4), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170), 'ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 2, gene_rpkm_matrix/170), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(gene_symbol, 4, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(gene_symbol, 8, 'Dmel_enzyme'/11)).
maybe_corisponds('ConceptMapFn'(gene_type, 2, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(gene_type, 4, gene_rpkm_matrix/170)).
maybe_corisponds('ConceptMapFn'(gene_type, 2, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(so_term_name, 3, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(gene_type, 4, gene_rpkm_matrix/170), 'ConceptMapFn'(so_term_name, 3, dmel_gene_sequence_ontology_annotations/4)).
maybe_corisponds('ConceptMapFn'(gene_type, 4, gene_rpkm_matrix/170), 'ConceptMapFn'(transcript_type, 7, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(identical_protein, 4, dmel_unique_protein_isoforms/4), 'ConceptMapFn'(polypeptide_symbol, 11, fbgn_fbtr_fbpp_expanded/11)).
maybe_corisponds('ConceptMapFn'(polypeptide_symbol, 11, fbgn_fbtr_fbpp_expanded/11), 'ConceptMapFn'(representative_protein, 3, dmel_unique_protein_isoforms/4)).
maybe_corisponds('ConceptMapFn'(so_term_name, 3, dmel_gene_sequence_ontology_annotations/4), 'ConceptMapFn'(transcript_type, 7, fbgn_fbtr_fbpp_expanded/11)).

%!  numeric_value_p_n(+Table, +Column, +Name) is det.
%
%   Maps numeric or identifier columns in a table to their respective names.
%
%   This predicate associates numeric column indexes (Column) in a specific
%   table (Table) to their descriptive names (Name). The `numeric_value_p_n/3`
%   predicate is dynamic and serves as a metadata reference for identifying
%   the meaning of numeric columns within structured data tables.
%
%   @arg Table The name of the table where the column exists.
%   @arg Column The numeric index of the column within the table.
%   @arg Name The descriptive name associated with the column index.
%
%   @examples
%     % Example: Retrieving the column name of a specific column in a table:
%     ?- numeric_value_p_n(dmel_paralogs, 11, Name).
%     Name = 'DIOPT_score'.
%
%     % Example: Listing all column names for a table:
%     ?- numeric_value_p_n(dmel_paralogs, Column, Name).
%     Column = 5,
%     Name = 'Strand' ;
%     Column = 10,
%     Name = 'Paralog_Strand' ;
%     Column = 11,
%     Name = 'DIOPT_score'.
%
numeric_value_p_n(dmel_human_orthologs_disease, 6, 'DIOPT_score').
numeric_value_p_n(dmel_human_orthologs_disease, 7, 'OMIM_Phenotype_IDs').
numeric_value_p_n(dmel_paralogs, 10, 'Paralog_Strand').
numeric_value_p_n(dmel_paralogs, 11, 'DIOPT_score').
numeric_value_p_n(dmel_paralogs, 5, 'Strand').
numeric_value_p_n(entity_publication, 4, 'PubMed').
numeric_value_p_n(fbgn_NAseq_Uniprot, 7, 'EntrezGene').
numeric_value_p_n(fbrf_pmid_pmcid_doi, 2, 'PMID').
numeric_value_p_n(gene_groups_HGNC, 4, 'Parent_FB_group').
numeric_value_p_n(gene_rpkm_matrix, 10, 'BCM_1_L3i100_(FBlc0000066)').
numeric_value_p_n(gene_rpkm_matrix, 100, 'mE_mRNA_L_Sindbis_virus_(FBlc0000675)').
numeric_value_p_n(gene_rpkm_matrix, 101, 'mE_mRNA_M_Sindbis_virus_(FBlc0000676)').
numeric_value_p_n(gene_rpkm_matrix, 102, 'mE_mRNA_P_Sindbis_virus_(FBlc0000677)').
numeric_value_p_n(gene_rpkm_matrix, 103, 'mE_mRNA_CME-W2_cells_(FBlc0000261)').
numeric_value_p_n(gene_rpkm_matrix, 104, 'mE_mRNA_GM2_cells_(FBlc0000262)').
numeric_value_p_n(gene_rpkm_matrix, 105, 'mE_mRNA_mbn2_cells_(FBlc0000263)').
numeric_value_p_n(gene_rpkm_matrix, 106, 'mE_mRNA_BG2-c2_cells_(FBlc0000264)').
numeric_value_p_n(gene_rpkm_matrix, 107, 'mE_mRNA_D20-c5_cells_(FBlc0000265)').
numeric_value_p_n(gene_rpkm_matrix, 108, 'mE_mRNA_S3_cells_(FBlc0000266)').
numeric_value_p_n(gene_rpkm_matrix, 109, 'mE_mRNA_1182-4H_cells_(FBlc0000267)').
numeric_value_p_n(gene_rpkm_matrix, 11, 'BCM_1_P3d_(FBlc0000067)').
numeric_value_p_n(gene_rpkm_matrix, 110, 'mE_mRNA_CME_L1_cells_(FBlc0000268)').
numeric_value_p_n(gene_rpkm_matrix, 111, 'mE_mRNA_Kc167_cells_(FBlc0000269)').
numeric_value_p_n(gene_rpkm_matrix, 112, 'mE_mRNA_BG1-c1_cells_(FBlc0000270)').
numeric_value_p_n(gene_rpkm_matrix, 113, 'mE_mRNA_D11_cells_(FBlc0000271)').
numeric_value_p_n(gene_rpkm_matrix, 114, 'mE_mRNA_D16-c3_cells_(FBlc0000272)').
numeric_value_p_n(gene_rpkm_matrix, 115, 'mE_mRNA_D17-c3_cells_(FBlc0000273)').
numeric_value_p_n(gene_rpkm_matrix, 116, 'mE_mRNA_D21_cells_(FBlc0000274)').
numeric_value_p_n(gene_rpkm_matrix, 117, 'mE_mRNA_D32_cells_(FBlc0000275)').
numeric_value_p_n(gene_rpkm_matrix, 118, 'mE_mRNA_D4-c1_cells_(FBlc0000276)').
numeric_value_p_n(gene_rpkm_matrix, 119, 'mE_mRNA_D8_cells_(FBlc0000277)').
numeric_value_p_n(gene_rpkm_matrix, 12, 'BCM_1_FA3d_(FBlc0000068)').
numeric_value_p_n(gene_rpkm_matrix, 120, 'mE_mRNA_D9_cells_(FBlc0000278)').
numeric_value_p_n(gene_rpkm_matrix, 121, 'mE_mRNA_S1_cells_(FBlc0000279)').
numeric_value_p_n(gene_rpkm_matrix, 122, 'mE_mRNA_S2R+_cells_(FBlc0000280)').
numeric_value_p_n(gene_rpkm_matrix, 123, 'mE_mRNA_Sg4_cells_(FBlc0000281)').
numeric_value_p_n(gene_rpkm_matrix, 124, 'mE_mRNA_OSS_cells_(FBlc0000282)').
numeric_value_p_n(gene_rpkm_matrix, 125, 'mE_mRNA_OSC_cells_(FBlc0000283)').
numeric_value_p_n(gene_rpkm_matrix, 126, 'mE_mRNA_fGS_cells_(FBlc0000284)').
numeric_value_p_n(gene_rpkm_matrix, 127, 'Knoblich_mRNA_L3_CNS_neuroblast_(FBlc0000505)').
numeric_value_p_n(gene_rpkm_matrix, 128, 'Knoblich_mRNA_L3_CNS_neuron_(FBlc0000506)').
numeric_value_p_n(gene_rpkm_matrix, 129, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Brain_(FBlc0003619)').
numeric_value_p_n(gene_rpkm_matrix, 13, 'BCM_1_MA3d_(FBlc0000069)').
numeric_value_p_n(gene_rpkm_matrix, 130, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Crop_(FBlc0003620)').
numeric_value_p_n(gene_rpkm_matrix, 131, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Carcass_(FBlc0003621)').
numeric_value_p_n(gene_rpkm_matrix, 132, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Eye_(FBlc0003622)').
numeric_value_p_n(gene_rpkm_matrix, 133, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_FatBody_(FBlc0003623)').
numeric_value_p_n(gene_rpkm_matrix, 134, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Head_(FBlc0003624)').
numeric_value_p_n(gene_rpkm_matrix, 135, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Hindgut_(FBlc0003625)').
numeric_value_p_n(gene_rpkm_matrix, 136, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Midgut_(FBlc0003626)').
numeric_value_p_n(gene_rpkm_matrix, 137, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Ovary_(FBlc0003627)').
numeric_value_p_n(gene_rpkm_matrix, 138, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_RectalPad_(FBlc0003628)').
numeric_value_p_n(gene_rpkm_matrix, 139, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_SalivaryGland_(FBlc0003629)').
numeric_value_p_n(gene_rpkm_matrix, 14, 'BCM_1_P_(FBlc0000070)').
numeric_value_p_n(gene_rpkm_matrix, 140, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_ThoracicoAbdominalGanglion_(FBlc0003630)').
numeric_value_p_n(gene_rpkm_matrix, 141, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_MalpighianTubule_(FBlc0003631)').
numeric_value_p_n(gene_rpkm_matrix, 142, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Mated_Spermathecum_(FBlc0003632)').
numeric_value_p_n(gene_rpkm_matrix, 143, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Virgin_Spermathecum_(FBlc0003633)').
numeric_value_p_n(gene_rpkm_matrix, 144, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Whole_(FBlc0003634)').
numeric_value_p_n(gene_rpkm_matrix, 145, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Brain_(FBlc0003635)').
numeric_value_p_n(gene_rpkm_matrix, 146, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Crop_(FBlc0003636)').
numeric_value_p_n(gene_rpkm_matrix, 147, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Carcass_(FBlc0003637)').
numeric_value_p_n(gene_rpkm_matrix, 148, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Eye_(FBlc0003638)').
numeric_value_p_n(gene_rpkm_matrix, 149, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_FatBody_(FBlc0003639)').
numeric_value_p_n(gene_rpkm_matrix, 15, 'BCM_1_L_(FBlc0000071)').
numeric_value_p_n(gene_rpkm_matrix, 150, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Head_(FBlc0003640)').
numeric_value_p_n(gene_rpkm_matrix, 151, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Hindgut_(FBlc0003641)').
numeric_value_p_n(gene_rpkm_matrix, 152, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Midgut_(FBlc0003642)').
numeric_value_p_n(gene_rpkm_matrix, 153, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_RectalPad_(FBlc0003643)').
numeric_value_p_n(gene_rpkm_matrix, 154, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_SalivaryGland_(FBlc0003644)').
numeric_value_p_n(gene_rpkm_matrix, 155, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_ThoracicoAbdominalGanglion_(FBlc0003645)').
numeric_value_p_n(gene_rpkm_matrix, 156, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_MalpighianTubule_(FBlc0003646)').
numeric_value_p_n(gene_rpkm_matrix, 157, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Testis_(FBlc0003647)').
numeric_value_p_n(gene_rpkm_matrix, 158, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_AccessoryGland_(FBlc0003648)').
numeric_value_p_n(gene_rpkm_matrix, 159, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Whole_(FBlc0003649)').
numeric_value_p_n(gene_rpkm_matrix, 16, 'BCM_1_A17d_(FBlc0000072)').
numeric_value_p_n(gene_rpkm_matrix, 160, 'RNA-Seq_Profile_FlyAtlas2_L3_CNS_(FBlc0003650)').
numeric_value_p_n(gene_rpkm_matrix, 161, 'RNA-Seq_Profile_FlyAtlas2_L3_FatBody_(FBlc0003651)').
numeric_value_p_n(gene_rpkm_matrix, 162, 'RNA-Seq_Profile_FlyAtlas2_L3_Hindgut_(FBlc0003652)').
numeric_value_p_n(gene_rpkm_matrix, 163, 'RNA-Seq_Profile_FlyAtlas2_L3_MalpighianTubule_(FBlc0003653)').
numeric_value_p_n(gene_rpkm_matrix, 164, 'RNA-Seq_Profile_FlyAtlas2_L3_Midgut_(FBlc0003654)').
numeric_value_p_n(gene_rpkm_matrix, 165, 'RNA-Seq_Profile_FlyAtlas2_L3_SalivaryGland_(FBlc0003655)').
numeric_value_p_n(gene_rpkm_matrix, 166, 'RNA-Seq_Profile_FlyAtlas2_L3_Trachea_(FBlc0003656)').
numeric_value_p_n(gene_rpkm_matrix, 167, 'RNA-Seq_Profile_FlyAtlas2_L3_Carcass_(FBlc0003657)').
numeric_value_p_n(gene_rpkm_matrix, 168, 'RNA-Seq_Profile_FlyAtlas2_L3_Whole_(FBlc0003658)').
numeric_value_p_n(gene_rpkm_matrix, 169, 'RNA-Seq_Profile_FlyAtlas2_Adult_Female_Heart_(FBlc0003724)').
numeric_value_p_n(gene_rpkm_matrix, 17, 'mE_mRNA_em0-2hr_(FBlc0000086)').
numeric_value_p_n(gene_rpkm_matrix, 170, 'RNA-Seq_Profile_FlyAtlas2_Adult_Male_Heart_(FBlc0003725)').
numeric_value_p_n(gene_rpkm_matrix, 18, 'mE_mRNA_em2-4hr_(FBlc0000087)').
numeric_value_p_n(gene_rpkm_matrix, 19, 'mE_mRNA_em4-6hr_(FBlc0000088)').
numeric_value_p_n(gene_rpkm_matrix, 20, 'mE_mRNA_em6-8hr_(FBlc0000089)').
numeric_value_p_n(gene_rpkm_matrix, 21, 'mE_mRNA_em8-10hr_(FBlc0000090)').
numeric_value_p_n(gene_rpkm_matrix, 22, 'mE_mRNA_em10-12hr_(FBlc0000091)').
numeric_value_p_n(gene_rpkm_matrix, 23, 'mE_mRNA_em12-14hr_(FBlc0000092)').
numeric_value_p_n(gene_rpkm_matrix, 24, 'mE_mRNA_em14-16hr_(FBlc0000093)').
numeric_value_p_n(gene_rpkm_matrix, 25, 'mE_mRNA_em16-18hr_(FBlc0000094)').
numeric_value_p_n(gene_rpkm_matrix, 26, 'mE_mRNA_em18-20hr_(FBlc0000095)').
numeric_value_p_n(gene_rpkm_matrix, 27, 'mE_mRNA_em20-22hr_(FBlc0000096)').
numeric_value_p_n(gene_rpkm_matrix, 28, 'mE_mRNA_em22-24hr_(FBlc0000097)').
numeric_value_p_n(gene_rpkm_matrix, 29, 'mE_mRNA_L1_(FBlc0000098)').
numeric_value_p_n(gene_rpkm_matrix, 30, 'mE_mRNA_L2_(FBlc0000099)').
numeric_value_p_n(gene_rpkm_matrix, 31, 'mE_mRNA_L3_12hr_(FBlc0000100)').
numeric_value_p_n(gene_rpkm_matrix, 32, 'mE_mRNA_L3_PS1-2_(FBlc0000101)').
numeric_value_p_n(gene_rpkm_matrix, 33, 'mE_mRNA_L3_PS3-6_(FBlc0000102)').
numeric_value_p_n(gene_rpkm_matrix, 34, 'mE_mRNA_L3_PS7-9_(FBlc0000103)').
numeric_value_p_n(gene_rpkm_matrix, 35, 'mE_mRNA_WPP_(FBlc0000104)').
numeric_value_p_n(gene_rpkm_matrix, 36, 'mE_mRNA_P5_(FBlc0000105)').
numeric_value_p_n(gene_rpkm_matrix, 37, 'mE_mRNA_P6_(FBlc0000106)').
numeric_value_p_n(gene_rpkm_matrix, 38, 'mE_mRNA_P8_(FBlc0000107)').
numeric_value_p_n(gene_rpkm_matrix, 39, 'mE_mRNA_P9-10_(FBlc0000108)').
numeric_value_p_n(gene_rpkm_matrix, 40, 'mE_mRNA_P15_(FBlc0000109)').
numeric_value_p_n(gene_rpkm_matrix, 41, 'mE_mRNA_AdF_Ecl_1days_(FBlc0000110)').
numeric_value_p_n(gene_rpkm_matrix, 42, 'mE_mRNA_AdF_Ecl_5days_(FBlc0000111)').
numeric_value_p_n(gene_rpkm_matrix, 43, 'mE_mRNA_AdF_Ecl_30days_(FBlc0000112)').
numeric_value_p_n(gene_rpkm_matrix, 44, 'mE_mRNA_AdM_Ecl_1days_(FBlc0000113)').
numeric_value_p_n(gene_rpkm_matrix, 45, 'mE_mRNA_AdM_Ecl_5days_(FBlc0000114)').
numeric_value_p_n(gene_rpkm_matrix, 46, 'mE_mRNA_AdM_Ecl_30days_(FBlc0000115)').
numeric_value_p_n(gene_rpkm_matrix, 47, 'mE_mRNA_A_MateF_1d_head_(FBlc0000207)').
numeric_value_p_n(gene_rpkm_matrix, 48, 'mE_mRNA_A_MateF_4d_ovary_(FBlc0000208)').
numeric_value_p_n(gene_rpkm_matrix, 49, 'mE_mRNA_A_MateM_1d_head_(FBlc0000209)').
numeric_value_p_n(gene_rpkm_matrix, 5, 'BCM_1_E2-4hr_(FBlc0000061)').
numeric_value_p_n(gene_rpkm_matrix, 50, 'mE_mRNA_A_VirF_1d_head_(FBlc0000210)').
numeric_value_p_n(gene_rpkm_matrix, 51, 'mE_mRNA_A_VirF_4d_head_(FBlc0000211)').
numeric_value_p_n(gene_rpkm_matrix, 52, 'mE_mRNA_A_MateF_20d_head_(FBlc0000212)').
numeric_value_p_n(gene_rpkm_matrix, 53, 'mE_mRNA_A_MateF_4d_head_(FBlc0000213)').
numeric_value_p_n(gene_rpkm_matrix, 54, 'mE_mRNA_A_MateM_20d_head_(FBlc0000214)').
numeric_value_p_n(gene_rpkm_matrix, 55, 'mE_mRNA_A_MateM_4d_acc_gland_(FBlc0000215)').
numeric_value_p_n(gene_rpkm_matrix, 56, 'mE_mRNA_A_MateM_4d_head_(FBlc0000216)').
numeric_value_p_n(gene_rpkm_matrix, 57, 'mE_mRNA_A_MateM_4d_testis_(FBlc0000217)').
numeric_value_p_n(gene_rpkm_matrix, 58, 'mE_mRNA_A_1d_carcass_(FBlc0000218)').
numeric_value_p_n(gene_rpkm_matrix, 59, 'mE_mRNA_A_1d_dig_sys_(FBlc0000219)').
numeric_value_p_n(gene_rpkm_matrix, 6, 'BCM_1_E14-16hr_(FBlc0000062)').
numeric_value_p_n(gene_rpkm_matrix, 60, 'mE_mRNA_A_20d_carcass_(FBlc0000220)').
numeric_value_p_n(gene_rpkm_matrix, 61, 'mE_mRNA_A_20d_dig_sys_(FBlc0000221)').
numeric_value_p_n(gene_rpkm_matrix, 62, 'mE_mRNA_A_4d_carcass_(FBlc0000222)').
numeric_value_p_n(gene_rpkm_matrix, 63, 'mE_mRNA_A_4d_dig_sys_(FBlc0000223)').
numeric_value_p_n(gene_rpkm_matrix, 64, 'mE_mRNA_P8_CNS_(FBlc0000224)').
numeric_value_p_n(gene_rpkm_matrix, 65, 'mE_mRNA_L3_CNS_(FBlc0000225)').
numeric_value_p_n(gene_rpkm_matrix, 66, 'mE_mRNA_L3_Wand_carcass_(FBlc0000226)').
numeric_value_p_n(gene_rpkm_matrix, 67, 'mE_mRNA_L3_Wand_dig_sys_(FBlc0000227)').
numeric_value_p_n(gene_rpkm_matrix, 68, 'mE_mRNA_L3_Wand_fat_(FBlc0000228)').
numeric_value_p_n(gene_rpkm_matrix, 69, 'mE_mRNA_L3_Wand_imag_disc_(FBlc0000229)').
numeric_value_p_n(gene_rpkm_matrix, 7, 'BCM_1_E2-16hr_(FBlc0000063)').
numeric_value_p_n(gene_rpkm_matrix, 70, 'mE_mRNA_L3_Wand_saliv_(FBlc0000230)').
numeric_value_p_n(gene_rpkm_matrix, 71, 'mE_mRNA_A_VirF_20d_head_(FBlc0000231)').
numeric_value_p_n(gene_rpkm_matrix, 72, 'mE_mRNA_A_VirF_4d_ovary_(FBlc0000232)').
numeric_value_p_n(gene_rpkm_matrix, 73, 'mE_mRNA_WPP_fat_(FBlc0000233)').
numeric_value_p_n(gene_rpkm_matrix, 74, 'mE_mRNA_WPP_saliv_(FBlc0000234)').
numeric_value_p_n(gene_rpkm_matrix, 75, 'mE_mRNA_P8_fat_(FBlc0000235)').
numeric_value_p_n(gene_rpkm_matrix, 76, 'mE_mRNA_A_4d_Cold1_(FBlc0000237)').
numeric_value_p_n(gene_rpkm_matrix, 77, 'mE_mRNA_A_4d_Cold2_(FBlc0000238)').
numeric_value_p_n(gene_rpkm_matrix, 78, 'mE_mRNA_L3_Cu_0.5mM_(FBlc0000239)').
numeric_value_p_n(gene_rpkm_matrix, 79, 'mE_mRNA_L3_late_Zn_5mM_(FBlc0000240)').
numeric_value_p_n(gene_rpkm_matrix, 8, 'BCM_1_E2-16hr100_(FBlc0000064)').
numeric_value_p_n(gene_rpkm_matrix, 80, 'mE_mRNA_A_4d_Cu_15mM_(FBlc0000241)').
numeric_value_p_n(gene_rpkm_matrix, 81, 'mE_mRNA_A_4d_Zn_4.5mM_(FBlc0000242)').
numeric_value_p_n(gene_rpkm_matrix, 82, 'mE_mRNA_A_4d_Caffeine_25mg/ml_(FBlc0000243)').
numeric_value_p_n(gene_rpkm_matrix, 83, 'mE_mRNA_A_4d_Caffeine_2.5mg/ml_(FBlc0000244)').
numeric_value_p_n(gene_rpkm_matrix, 84, 'mE_mRNA_L3_Caffeine_1.5mg/ml_(FBlc0000245)').
numeric_value_p_n(gene_rpkm_matrix, 85, 'mE_mRNA_A_4d_Cd_0.1M_(FBlc0000246)').
numeric_value_p_n(gene_rpkm_matrix, 86, 'mE_mRNA_A_4d_Cd_0.05M_(FBlc0000247)').
numeric_value_p_n(gene_rpkm_matrix, 87, 'mE_mRNA_L3_Cd_12h_(FBlc0000248)').
numeric_value_p_n(gene_rpkm_matrix, 88, 'mE_mRNA_L3_Cd_6hr_(FBlc0000249)').
numeric_value_p_n(gene_rpkm_matrix, 89, 'mE_mRNA_A_4d_Paraquat_5mM_(FBlc0000250)').
numeric_value_p_n(gene_rpkm_matrix, 9, 'BCM_1_L3i_(FBlc0000065)').
numeric_value_p_n(gene_rpkm_matrix, 90, 'mE_mRNA_A_4d_Paraquat_10mM_(FBlc0000251)').
numeric_value_p_n(gene_rpkm_matrix, 91, 'mE_mRNA_L3_Rotenone_8ug_(FBlc0000252)').
numeric_value_p_n(gene_rpkm_matrix, 92, 'mE_mRNA_L3_Rotenone_2ug_(FBlc0000253)').
numeric_value_p_n(gene_rpkm_matrix, 93, 'mE_mRNA_L3_EtOH_10_(FBlc0000254)').
numeric_value_p_n(gene_rpkm_matrix, 94, 'mE_mRNA_L3_EtOH_5_(FBlc0000255)').
numeric_value_p_n(gene_rpkm_matrix, 95, 'mE_mRNA_L3_EtOH_2.5_(FBlc0000256)').
numeric_value_p_n(gene_rpkm_matrix, 96, 'mE_mRNA_A_4d_Heatshock_(FBlc0000257)').
numeric_value_p_n(gene_rpkm_matrix, 97, 'mE_mRNA_A_10d_Resveratrol_100uM_(FBlc0000672)').
numeric_value_p_n(gene_rpkm_matrix, 98, 'mE_mRNA_A_10d_Rotenone_Starved_(FBlc0000673)').
numeric_value_p_n(gene_rpkm_matrix, 99, 'mE_mRNA_F_Sindbis_virus_(FBlc0000674)').
numeric_value_p_n(gene_rpkm_report, 10, 'Unique_exon_base_count').
numeric_value_p_n(gene_rpkm_report, 11, 'Total_exon_base_count').
numeric_value_p_n(gene_rpkm_report, 8, 'RPKM_value').
numeric_value_p_n(gene_rpkm_report, 9, 'Bin_value').
numeric_value_p_n(gene_snapshots, 4, datestamp).
numeric_value_p_n(insertion_mapping, 5, orientation).
numeric_value_p_n(organism_list, 5, 'NCBI_taxon').
numeric_value_p_n(pmid_fbgn_uniprot, 2, gene_symbol).

%!  table_n_type(+Table, +Column, +Name, +Type) is det.
%
%   Defines the metadata for tables, associating columns with their names and types.
%
%   This predicate describes the structure of various tables by mapping column 
%   indexes (Column) to their descriptive names (Name) and optionally to their 
%   types (Type). The `table_n_type/4` predicate is dynamic and serves as a 
%   metadata reference for interpreting data from different tables.
%
%   @arg Table The name of the table.
%   @arg Column The numeric index of the column within the table.
%   @arg Name The descriptive name of the column.
%   @arg Type The optional type of the column, such as an identifier type or other metadata.
%
%   @examples
%     % Example: Finding the name of a specific column in a table:
%     ?- table_n_type(dmel_paralogs, 11, Name, Type).
%     Name = 'DIOPT_score',
%     Type = _.
%
%     % Example: Listing all columns for a table:
%     ?- table_n_type(dmel_paralogs, Column, Name, Type).
%     Column = 1,
%     Name = 'FBgn',
%     Type = 'FBgn' ;
%     Column = 2,
%     Name = 'GeneSymbol',
%     Type = _ ;
%     ...
%
table_n_type('cyto-genetic-seq', 1, 'Cytogenetic_map_position', _).
table_n_type('cyto-genetic-seq', 2, 'Genetic_map_position', _).
table_n_type('cyto-genetic-seq', 3, 'Sequence_coordinates_(release_6)', _).
table_n_type('cyto-genetic-seq', 4, 'R6_conversion_notes', _).
table_n_type('Dmel_enzyme', 1, gene_group_id, _).
table_n_type('Dmel_enzyme', 10, gene_EC_number, _).
table_n_type('Dmel_enzyme', 11, gene_EC_name, _).
table_n_type('Dmel_enzyme', 2, gene_group_name, _).
table_n_type('Dmel_enzyme', 3, gene_group_GO_id, _).
table_n_type('Dmel_enzyme', 4, gene_group_GO_name, _).
table_n_type('Dmel_enzyme', 5, gene_group_EC_number, _).
table_n_type('Dmel_enzyme', 6, gene_group_EC_name, _).
table_n_type('Dmel_enzyme', 7, gene_id, 'FBgn').
table_n_type('Dmel_enzyme', 8, gene_symbol, _).
table_n_type('Dmel_enzyme', 9, gene_name, _).
table_n_type(allele_genetic_interactions, 1, allele_symbol, _).
table_n_type(allele_genetic_interactions, 2, allele_FBal, 'FBal').
table_n_type(allele_genetic_interactions, 3, interaction, _).
table_n_type(allele_genetic_interactions, 4, 'FBrf', 'FBrf').
table_n_type(automated_gene_summaries, 1, primary_FBgn, 'FBgn').
table_n_type(automated_gene_summaries, 2, summary_text, _).
table_n_type(best_gene_summary, 1, 'FBgn', 'FBgn').
table_n_type(best_gene_summary, 2, 'Gene_Symbol', _).
table_n_type(best_gene_summary, 3, 'Summary_Source', _).
table_n_type(best_gene_summary, 4, 'Summary', _).
table_n_type(cDNA_clone, 1, 'FBcl', 'FBcl').
table_n_type(cDNA_clone, 2, organism_abbreviation, _).
table_n_type(cDNA_clone, 3, clone_name, _).
table_n_type(cDNA_clone, 4, dataset_metadata_name, _).
table_n_type(cDNA_clone, 5, cDNA_accession, _).
table_n_type(cDNA_clone, 6, 'EST_accession', _).
table_n_type(dataset_metadata, 1, 'Dataset_Metadata_ID', _).
table_n_type(dataset_metadata, 2, 'Dataset_Metadata_Name', _).
table_n_type(dataset_metadata, 3, 'Item_ID', _).
table_n_type(dataset_metadata, 4, 'Item_Name', _).
table_n_type(disease_model_annotations, 1, 'FBgn', 'FBgn').
table_n_type(disease_model_annotations, 10, 'Based_on_orthology_with_(symbol)', _).
table_n_type(disease_model_annotations, 11, 'Evidence/interacting_alleles', _).
table_n_type(disease_model_annotations, 12, 'Reference_(FBrf)', 'FBrf').
table_n_type(disease_model_annotations, 2, 'Gene_symbol', _).
table_n_type(disease_model_annotations, 3, 'HGNC', 'HGNC').
table_n_type(disease_model_annotations, 4, 'DO_qualifier', _).
table_n_type(disease_model_annotations, 5, 'DO', _).
table_n_type(disease_model_annotations, 6, 'DO_term', _).
table_n_type(disease_model_annotations, 7, 'Allele_used_in_model_(FBal)', 'FBal').
table_n_type(disease_model_annotations, 8, 'Allele_used_in_model_(symbol)', _).
table_n_type(disease_model_annotations, 9, 'Based_on_orthology_with_(HGNC_ID)', 'HGNC').
table_n_type(dmel_gene_sequence_ontology_annotations, 1, gene_primary_id, _).
table_n_type(dmel_gene_sequence_ontology_annotations, 2, gene_symbol, _).
table_n_type(dmel_gene_sequence_ontology_annotations, 3, so_term_name, _).
table_n_type(dmel_gene_sequence_ontology_annotations, 4, so_term_id, _).
table_n_type(dmel_human_orthologs_disease, 1, 'Dmel_gene', _).
table_n_type(dmel_human_orthologs_disease, 2, 'Dmel_gene_symbol', _).
table_n_type(dmel_human_orthologs_disease, 3, 'Human_gene_HGNC', 'HGNC').
table_n_type(dmel_human_orthologs_disease, 4, 'Human_gene_OMIM', 'OMIM').
table_n_type(dmel_human_orthologs_disease, 5, 'Human_gene_symbol', _).
table_n_type(dmel_human_orthologs_disease, 6, 'DIOPT_score', _).
table_n_type(dmel_human_orthologs_disease, 7, listOf('OMIM_Phenotype_IDs',[',']), 'OMIM').
table_n_type(dmel_human_orthologs_disease, 8, 'OMIM_Phenotype_IDs[name]', 'OMIM').
table_n_type(dmel_paralogs, 1, 'FBgn', 'FBgn').
table_n_type(dmel_paralogs, 10, 'Paralog_Strand', _).
table_n_type(dmel_paralogs, 11, 'DIOPT_score', _).
table_n_type(dmel_paralogs, 2, 'GeneSymbol', _).
table_n_type(dmel_paralogs, 3, 'Arm/Scaffold', _).
table_n_type(dmel_paralogs, 4, 'Location', _).
table_n_type(dmel_paralogs, 5, 'Strand', _).
table_n_type(dmel_paralogs, 6, 'Paralog_FBgn', 'FBgn').
table_n_type(dmel_paralogs, 7, 'Paralog_GeneSymbol', _).
table_n_type(dmel_paralogs, 8, 'Paralog_Arm/Scaffold', _).
table_n_type(dmel_paralogs, 9, 'Paralog_Location', _).
table_n_type(dmel_unique_protein_isoforms, 1, 'FBgn', 'FBgn').
table_n_type(dmel_unique_protein_isoforms, 2, 'FB_gene_symbol', _).
table_n_type(dmel_unique_protein_isoforms, 3, representative_protein, _).
table_n_type(dmel_unique_protein_isoforms, 4, identical_protein, _).
table_n_type(entity_publication, 1, entity_id, _).
table_n_type(entity_publication, 2, entity_name, _).
table_n_type(entity_publication, 3, 'FlyBase_publication', 'FBrf').
table_n_type(entity_publication, 4, 'PubMed', _).
table_n_type(fbal_to_fbgn, 1, 'AlleleID', 'FBal').
table_n_type(fbal_to_fbgn, 2, 'AlleleSymbol', _).
table_n_type(fbal_to_fbgn, 3, 'GeneID', 'FBgn').
table_n_type(fbal_to_fbgn, 4, 'GeneSymbol', _).
table_n_type(fbgn_annotation_ID, 1, gene_symbol, _).
table_n_type(fbgn_annotation_ID, 2, organism_abbreviation, _).
table_n_type(fbgn_annotation_ID, 3, primary_FBgn, 'FBgn').
table_n_type(fbgn_annotation_ID, 4, secondary_FBgn, 'FBgn').
table_n_type(fbgn_annotation_ID, 5, annotation_ID, 'FBan').
table_n_type(fbgn_annotation_ID, 6, secondary_annotation_ID, 'FBan').
table_n_type(fbgn_exons2affy1_overlaps, 1, 'FBgn', 'FBgn').
table_n_type(fbgn_exons2affy1_overlaps, 2, affy, _).
table_n_type(fbgn_exons2affy2_overlaps, 1, 'FBgn', 'FBgn').
table_n_type(fbgn_exons2affy2_overlaps, 2, affy, _).
table_n_type(fbgn_fbtr_fbpp, 1, 'FBgn', 'FBgn').
table_n_type(fbgn_fbtr_fbpp, 2, 'FBtr', 'FBtr').
table_n_type(fbgn_fbtr_fbpp, 3, 'FBpp', 'FBpp').
table_n_type(fbgn_fbtr_fbpp_expanded, 1, organism, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 10, polypeptide_ID, 'FBpp').
table_n_type(fbgn_fbtr_fbpp_expanded, 11, polypeptide_symbol, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 2, gene_type, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 3, gene_ID, 'FBgn').
table_n_type(fbgn_fbtr_fbpp_expanded, 4, gene_symbol, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 5, gene_fullname, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 6, annotation_ID, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 7, transcript_type, _).
table_n_type(fbgn_fbtr_fbpp_expanded, 8, transcript_ID, 'FBtr').
table_n_type(fbgn_fbtr_fbpp_expanded, 9, transcript_symbol, _).
table_n_type(fbgn_gleanr, 1, organism_abbreviation, _).
table_n_type(fbgn_gleanr, 2, gene_symbol, _).
table_n_type(fbgn_gleanr, 3, primary_FBgn, 'FBgn').
table_n_type(fbgn_gleanr, 4, 'GLEANR', _).
table_n_type(fbrf_pmid_pmcid_doi, 1, 'FBrf', 'FBrf').
table_n_type(fbrf_pmid_pmcid_doi, 2, 'PMID', 'PMID').
table_n_type(fbrf_pmid_pmcid_doi, 3, 'PMCID', 'PMCID').
table_n_type(fbrf_pmid_pmcid_doi, 4, 'DOI', 'DOI').
table_n_type(fbrf_pmid_pmcid_doi, 5, pub_type, _).
table_n_type(fbrf_pmid_pmcid_doi, 6, miniref, _).
table_n_type(fbrf_pmid_pmcid_doi, 7, pmid_added, _).
table_n_type(gene_functional_complementation, 1, 'Dmel_gene_symbol', _).
table_n_type(gene_functional_complementation, 2, 'Dmel_gene_FBgn', 'FBgn').
table_n_type(gene_functional_complementation, 3, ortholog_gene_symbol, _).
table_n_type(gene_functional_complementation, 4, ortholog_gene_FBgn_ID, 'FBgn').
table_n_type(gene_functional_complementation, 5, reference_FBrf, 'FBrf').
table_n_type(gene_genetic_interactions, 1, 'Starting_gene_symbol', _).
table_n_type(gene_genetic_interactions, 2, 'Starting_gene_FBgn', 'FBgn').
table_n_type(gene_genetic_interactions, 3, 'Interacting_gene_symbol', _).
table_n_type(gene_genetic_interactions, 4, 'Interacting_gene_FBgn', 'FBgn').
table_n_type(gene_genetic_interactions, 5, 'Interaction_type', _).
table_n_type(gene_genetic_interactions, 6, 'Publication_FBrf', 'FBrf').
table_n_type(gene_group, 1, 'FB_group', 'FBgg').
table_n_type(gene_group, 2, 'FB_group_symbol', _).
table_n_type(gene_group, 3, 'FB_group_name', _).
table_n_type(gene_group, 4, 'Parent_FB_group', 'FBgg').
table_n_type(gene_group, 5, 'Parent_FB_group_symbol', _).
table_n_type(gene_group, 6, 'Group_member_FB_gene', 'FBgn').
table_n_type(gene_group, 7, 'Group_member_FB_gene_symbol', _).
table_n_type(gene_groups_HGNC, 1, 'FB_group', 'FBgg').
table_n_type(gene_groups_HGNC, 2, 'FB_group_symbol', _).
table_n_type(gene_groups_HGNC, 3, 'FB_group_name', _).
table_n_type(gene_groups_HGNC, 4, 'Parent_FB_group', 'FBgg').
table_n_type(gene_groups_HGNC, 5, 'Parent_FB_group_symbol', _).
table_n_type(gene_groups_HGNC, 6, 'Group_member_FB_gene', 'FBgn').
table_n_type(gene_groups_HGNC, 7, 'Group_member_FB_gene_symbol', _).
table_n_type(gene_map_table, 1, organism_abbreviation, _).
table_n_type(gene_map_table, 2, current_symbol, _).
table_n_type(gene_map_table, 3, primary_FBid, 'FBgn').
table_n_type(gene_map_table, 4, recombination_loc, _).
table_n_type(gene_map_table, 5, cytogenetic_loc, _).
table_n_type(gene_map_table, 6, sequence_loc, _).
table_n_type(gene_rpkm_matrix, 1, gene_primary_id, 'FBgn').
table_n_type(gene_rpkm_matrix, 10, 'BCM_1_L3i100_(FBlc0000066)', 'FBlc').
table_n_type(gene_rpkm_matrix, 11, 'BCM_1_P3d_(FBlc0000067)', 'FBlc').
table_n_type(gene_rpkm_matrix, 12, 'BCM_1_FA3d_(FBlc0000068)', 'FBlc').
table_n_type(gene_rpkm_matrix, 13, 'BCM_1_MA3d_(FBlc0000069)', 'FBlc').
table_n_type(gene_rpkm_matrix, 14, 'BCM_1_P_(FBlc0000070)', 'FBlc').
table_n_type(gene_rpkm_matrix, 15, 'BCM_1_L_(FBlc0000071)', 'FBlc').
table_n_type(gene_rpkm_matrix, 16, 'BCM_1_A17d_(FBlc0000072)', 'FBlc').
table_n_type(gene_rpkm_matrix, 2, gene_symbol, _).
table_n_type(gene_rpkm_matrix, 3, gene_fullname, _).
table_n_type(gene_rpkm_matrix, 4, gene_type, _).
table_n_type(gene_rpkm_matrix, 5, 'BCM_1_E2-4hr_(FBlc0000061)', 'FBlc').
table_n_type(gene_rpkm_matrix, 6, 'BCM_1_E14-16hr_(FBlc0000062)', 'FBlc').
table_n_type(gene_rpkm_matrix, 7, 'BCM_1_E2-16hr_(FBlc0000063)', 'FBlc').
table_n_type(gene_rpkm_matrix, 8, 'BCM_1_E2-16hr100_(FBlc0000064)', 'FBlc').
table_n_type(gene_rpkm_matrix, 9, 'BCM_1_L3i_(FBlc0000065)', 'FBlc').
table_n_type(gene_rpkm_report, 1, 'Release_ID', _).
table_n_type(gene_rpkm_report, 10, 'Unique_exon_base_count', _).
table_n_type(gene_rpkm_report, 11, 'Total_exon_base_count', _).
table_n_type(gene_rpkm_report, 12, 'Count_used', _).
table_n_type(gene_rpkm_report, 2, 'FBgn', 'FBgn').
table_n_type(gene_rpkm_report, 3, 'GeneSymbol', _).
table_n_type(gene_rpkm_report, 4, 'Parent_library_FBlc', 'FBlc').
table_n_type(gene_rpkm_report, 5, 'Parent_library_name', _).
table_n_type(gene_rpkm_report, 6, 'RNASource_FBlc', 'FBlc').
table_n_type(gene_rpkm_report, 7, 'RNASource_name', _).
table_n_type(gene_rpkm_report, 8, 'RPKM_value', _).
table_n_type(gene_rpkm_report, 9, 'Bin_value', _).
table_n_type(gene_snapshots, 1, 'FBgn', 'FBgn').
table_n_type(gene_snapshots, 2, 'GeneSymbol', _).
table_n_type(gene_snapshots, 3, 'GeneName', _).
table_n_type(gene_snapshots, 4, datestamp, _).
table_n_type(gene_snapshots, 5, gene_snapshot_text, _).
table_n_type(genomic_clone, 1, 'FBcl', 'FBcl').
table_n_type(genomic_clone, 2, organism_abbreviation, _).
table_n_type(genomic_clone, 3, clone_name, _).
table_n_type(genomic_clone, 4, accession, _).
table_n_type(insertion_mapping, 1, insertion_symbol, _).
table_n_type(insertion_mapping, 2, 'FBti', 'FBti').
table_n_type(insertion_mapping, 3, genomic_location, _).
table_n_type(insertion_mapping, 4, range, _).
table_n_type(insertion_mapping, 5, orientation, _).
table_n_type(insertion_mapping, 6, estimated_cytogenetic_location, _).
table_n_type(insertion_mapping, 7, observed_cytogenetic_location, _).
table_n_type(organism_list, 1, genus, _).
table_n_type(organism_list, 2, species, _).
table_n_type(organism_list, 3, abbreviation, _).
table_n_type(organism_list, 4, common_name, _).
table_n_type(organism_list, 5, 'NCBI_taxon', _).
table_n_type(organism_list, 6, 'drosophilid?', _).
table_n_type(pathway_group, 1, 'FB_group', 'FBgg').
table_n_type(pathway_group, 2, 'FB_group_symbol', _).
table_n_type(pathway_group, 3, 'FB_group_name', _).
table_n_type(pathway_group, 4, 'Parent_FB_group', 'FBgg').
table_n_type(pathway_group, 5, 'Parent_FB_group_symbol', _).
table_n_type(pathway_group, 6, 'Group_member_FB_gene', 'FBgn').
table_n_type(pathway_group, 7, 'Group_member_FB_gene_symbol', _).
table_n_type(physical_interactions_mitab, 1, 'ID_Interactor_A', _).
table_n_type(physical_interactions_mitab, 10, 'Taxid_Interactor_A', _).
table_n_type(physical_interactions_mitab, 11, 'Taxid_Interactor_B', _).
table_n_type(physical_interactions_mitab, 12, 'Interaction_Type', _).
table_n_type(physical_interactions_mitab, 13, 'Source_Database', _).
table_n_type(physical_interactions_mitab, 14, 'Interaction_Identifier', _).
table_n_type(physical_interactions_mitab, 15, 'Confidence_Value', _).
table_n_type(physical_interactions_mitab, 16, 'Expansion_Method', _).
table_n_type(physical_interactions_mitab, 17, 'Biological_Role_Interactor_A', _).
table_n_type(physical_interactions_mitab, 18, 'Biological_Role_Interactor_B', _).
table_n_type(physical_interactions_mitab, 19, 'Experimental_Role_Interactor_A', _).
table_n_type(physical_interactions_mitab, 2, 'ID_Interactor_B', _).
table_n_type(physical_interactions_mitab, 20, 'Experimental_Role_Interactor_B', _).
table_n_type(physical_interactions_mitab, 21, 'Type_Interactor_A', _).
table_n_type(physical_interactions_mitab, 22, 'Type_Interactor_B', _).
table_n_type(physical_interactions_mitab, 23, 'Xref_Interactor_A', _).
table_n_type(physical_interactions_mitab, 24, 'Xref_Interactor_B', _).
table_n_type(physical_interactions_mitab, 25, 'Interaction_Xref', _).
table_n_type(physical_interactions_mitab, 26, 'Annotation_Interactor_A', _).
table_n_type(physical_interactions_mitab, 27, 'Annotation_Interactor_B', _).
table_n_type(physical_interactions_mitab, 28, 'Interaction_Annotation', _).
table_n_type(physical_interactions_mitab, 29, 'Host_Organism', _).
table_n_type(physical_interactions_mitab, 3, 'Alt_ID_Interactor_A', _).
table_n_type(physical_interactions_mitab, 30, 'Interaction_Parameters', _).
table_n_type(physical_interactions_mitab, 31, 'Creation_Date', _).
table_n_type(physical_interactions_mitab, 32, 'Update_Date', _).
table_n_type(physical_interactions_mitab, 33, 'Checksum_Interactor_A', _).
table_n_type(physical_interactions_mitab, 34, 'Checksum_Interactor_B', _).
table_n_type(physical_interactions_mitab, 35, 'Interaction_Checksum', _).
table_n_type(physical_interactions_mitab, 36, 'Negative', _).
table_n_type(physical_interactions_mitab, 37, 'Feature_Interactor_A', _).
table_n_type(physical_interactions_mitab, 38, 'Feature_Interactor_B', _).
table_n_type(physical_interactions_mitab, 39, 'Stoichiometry_Interactor_A', _).
table_n_type(physical_interactions_mitab, 4, 'Alt_ID_Interactor_B', _).
table_n_type(physical_interactions_mitab, 40, 'Stoichiometry_Interactor_B', _).
table_n_type(physical_interactions_mitab, 41, 'Identification_Method_Participant_A', _).
table_n_type(physical_interactions_mitab, 42, 'Identification_Method_Participant_B', _).
table_n_type(physical_interactions_mitab, 5, 'Alias_Interactor_A', _).
table_n_type(physical_interactions_mitab, 6, 'Alias_Interactor_B', _).
table_n_type(physical_interactions_mitab, 7, 'Interaction_Detection_Method', _).
table_n_type(physical_interactions_mitab, 8, 'Publication_1st_Author', _).
table_n_type(physical_interactions_mitab, 9, 'Publication', _).
table_n_type(stocks, 1, stock_id, 'FBst').
table_n_type(stocks, 2, dbxref_id, _).
table_n_type(stocks, 3, organism_id, _).
table_n_type(stocks, 4, name, _).
table_n_type(stocks, 5, uniquename, _).
table_n_type(stocks, 6, description, _).
table_n_type(stocks, 7, type_id, _).
table_n_type(stocks, 8, is_obsolete, _).
table_n_type(synonym, 1, primary_FBid, 'FBgn').
table_n_type(synonym, 2, organism_abbreviation, _).
table_n_type(synonym, 3, current_symbol, _).
table_n_type(synonym, 4, current_fullname, _).
table_n_type(synonym, 5, fullname_synonym, _).
table_n_type(synonym, 6, symbol_synonym, _).

