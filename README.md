# :rocket: Version Space Candidate Elimination inside of MeTTa

The Candidate Elimination algorithm is a conceptual learning algorithm that incrementally refines the version space boundary. This implementation focuses on bringing this algorithm into the MeTTa relational programming environment.

## :pushpin: Overview
- The algorithm refines the version space boundary using observed training examples.
- It maintains two sets of hypotheses:
  - **G**: The most general hypotheses.
  - **S**: The most specific hypotheses.
- Each training example is processed to refine the `G` and `S` sets, eliminating inconsistent hypotheses.

## :package: Getting Started
### :gear: Prerequisites
- Build [Hyperon Experimental](https://github.com/trueagi-io/hyperon-experimental).

### :toolbox: Installation
```
# Clone the repository
git clone https://github.com/logicmoo/vspace-metta

# Change to the cloned directory
cd vspace-metta

chmod +x INSTALL.md  #incase Git lost +x n the file
# Answer the questions and follow the directions
./INSTALL.md
```

This script automates the setup of SWI-Prolog and its associated Python packages on your system. Here's a brief overview of the main components it installs and updates:

#### SWI-Prolog
- **Checks** if SWI-Prolog is already installed.
- **Installs or Updates** SWI-Prolog to ensure version 9.1 or higher is present.

#### Python and pip
- **Verifies** if Python's package manager `pip` is installed.
- **Installs pip** if it's not found, allowing Python packages to be managed.

#### Python Packages for SWI-Prolog
- **Installs Janus**: A Python package that interfaces with SWI-Prolog.
- **Installs PySWIP**: Another Python package that provides integration with SWI-Prolog.

#### SWI-Prolog Packages
- **Updates or Installs** specific SWI-Prolog packages (`predicate_streams`, `logicmoo_utils`, and `dictoo`) as needed.

#### System Requirements
- **Requires sudo access** for certain operations, such as installing SWI-Prolog and pip.
- **Modifies system package sources** to include the SWI-Prolog development repository.
- **Installs and updates packages** using the system's package manager and Python's pip.

---

**Note**: Running this script will modify your system's software configuration, including adding repositories and installing new packages. Ensure you understand these changes and have the necessary permissions before proceeding.

---




#### For loading Flybase into Rust Metta (Optional)

For testing/viewing to see how we see the flybase as MeTTa.
Or to load it into a Rust Space instead of VSpace.

```

# takes around 8 minutes
./scripts/convert_to_metta.sh ./data/ftp.flybase.org/releases/FB2023_04/precomputed_files/

# to get an atoms count (should be at least 56 million)
find ./data/ftp.flybase.org/releases/FB2023_04/precomputed_files/ -type f -name "*.metta" -exec wc -l {} +

```



## :computer: Various Usages and Demos

- Run in MeTTaLog without Rust installed:
```
./MeTTa
metta &self +> !(ensure-loaded! whole_flybase)
metta &self +> !(match &flybase (gene_map_table $Dmel $abo FBgn0000018 $C $D $E) (gene_map_table $Dmel $abo FBgn0000018 $C $D $E))


!(match &flybase (gene_map_table $Dmel $Abo FBgn0000018 $C $D $E) (gene_map_table $Dmel $Abo FBgn0000018 $C $D $E))

NDet Result(1):
(gene_map_table Dmel abo FBgn0000018 2-44 32C1-32C1 2L:10973443..10975293(-1))

$E = 2L:10973443..10975293(-1)
$D = 32C1-32C1
$C = 2-44
$Abo = abo
$Dmel = Dmel


; Execution took 0.000279 secs. (279.20 microseconds)
More Solutions? key=(;)
NDet Result(2):
(gene_map_table Dmel abo FBgn0000018 2-44 32C1-32C1 2L:10973443..10975293(-1))

$E = 2L:10973443..10975293(-1)
$D = 32C1-32C1
$C = 2-44
$Abo = abo
$Dmel = Dmel


; Execution took 0.002 secs. (1.53 milliseconds)
More Solutions? key=(;)
Last Result(3):
(gene_map_table $_311278 $_311500 FBgn0000018 $_311724 $_311928 $_312132)

metta &self +> !(, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32) (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33) (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42) (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc) (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))


- Run in Rust MeTTA:
  ```
  export PYTHONPATH=metta_vspace:$PYTHONPATH
  metta 1-VSpaceTest.metta
  @h
  metta@&self +> !(ensure-loaded! whole_flybase)
  metta@&self +> !(, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32) (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33) (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42) (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc) (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))
  ```
- Launch vspace-metta Jupyter notebook:
  ```
  ./scripts/start_jupyter.sh
  ```
- Run long huge tests of vspace-metta
  ```
  ./scripts/test_in_metta.sh
  ```

```
metta +> !(test_custom_v_space)

; (add-atom &vspace_8 a)
; (add-atom &vspace_8 b)
; (atom-count &vspace_8)
Pass Test:(Values same: 2 == 2)
Pass Test:(Values same: Test Space Payload Attrib == Test Space Payload Attrib)
; (get-atoms &vspace_8)
Pass Test:( [a, b] == [a, b] )
; (add-atom &vspace_9 a)
; (add-atom &vspace_9 b)
; (add-atom &vspace_9 c)
; (remove-atom &vspace_9 b)
Pass Test:(remove_atom on a present atom should return true)
; (remove-atom &vspace_9 bogus)
Pass Test:(remove_atom on a missing atom should return false)
; (get-atoms &vspace_9)
Pass Test:( [a, c] == [a, c] )
; (add-atom &vspace_10 a)
; (add-atom &vspace_10 b)
; (add-atom &vspace_10 c)
; (atom-replace &vspace_10 b d)
; (add-atom &vspace_10 d)
Pass Test:(Expression is true: True)
; (get-atoms &vspace_10)
Pass Test:( [a, c, d] == [a, d, c] )
; (add-atom &vspace_11 (A B))
; (add-atom &vspace_11 (C D))
; (add-atom &vspace_11 (A E))
; (match &vspace_11 ($_105354) (A $_105354))
; RES: (metta-iter-bind  &vspace_11 (A B) (B))
; RES: (metta-iter-bind  &vspace_11 (A E) (E))
Pass Test:( [ { $xx <- B },
 { $xx <- E } ] == [{xx: B}, {xx: E}] )
; (add-atom &vspace_12 (A B))
Pass Test:(Values same: CSpace == CSpace)
; (match &vspace_12 ($_117600) (A $_117600))
; RES: (metta-iter-bind  &vspace_12 (A B) (B))
Pass Test:( [ { $v <- B } ] == [{v: B}] )
; (add-atom &vspace_12 (big-space None))
; (add-atom &vspace_13 (A B))
; (match &vspace_13 ($_129826) (A $_129826))
; RES: (metta-iter-bind  &vspace_13 (A B) (B))
; (match &vspace_13 ($_135548) (: B $_135548))
Pass Test:(Values same: [[B]] == [[B]])
```

## :bulb: Expected Features
- **Relational Programming**: Utilize MeTTa's declarative paradigm to represent knowledge as relations and query this knowledge base.
- **Flybase Integration**: Test learning while interacting with Flybase data using MeTTa.

## :dart: Advantages of Version Space
- **Incremental Learning**: Suitable for handling one example at a time, refining the G-set and S-set without revisiting old data.
- **Consistency Maintenance**: Maintains all hypotheses consistent with observed data.
- **Transparent Learning**: The G-set and S-set provide an interpretable view of the learned content.

## :raised_hands: Acknowledgments
- Thanks to the developers and contributors of [Hyperon Experimental MeTTa](https://github.com/trueagi-io/hyperon-experimental) and [PySWIP](https://github.com/yuce/pyswip).
- Appreciation to [Flybase](https://flybase.org) for their genetic data that aids in learning and testing.

## :phone: Support
- For support, queries, or feature suggestions, kindly open an issue on our [GitHub repository](https://github.com/logicmoo/vspace-metta/issues).

## :scroll: License
- vspace-metta is distributed under the LGPL License.

## metta_interp.PL
[MeTTaLog (In Prolog)](https://github.com/logicmoo/vspace-metta/blob/main/MeTTaLog.md)
