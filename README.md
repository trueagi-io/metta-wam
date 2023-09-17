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
- Install PySWIP using:
  ```
  pip install pyswip
  ```

### :toolbox: Installation
```
# Clone the repository
git clone https://github.com/logicmoo/vspace-metta


# Change to the cloned directory
cd vspace-metta

# Install the package
export PYTHONPATH=$PWD/metta_vspace:$PYTHONPATH
# LAter on we might use pip install .

# Download necessary files
wget --no-parent -A .gz -r -P ./data http://ftp.flybase.org/releases/FB2023_04/precomputed_files


# Unzip them
find ./data -name "*.gz" -execdir gunzip {} \;
find ./data -name "*.zip" -execdir unzip -d ./ {} \;
# should be arround 2.0G
du -h ./data

wget https://raw.githubusercontent.com/The-Sequence-Ontology/SO-Ontologies/master/Ontology_Files/so.obo -o data/ontologies/so.obo

# For all files instead of using FBte:0000666  we will use FBte0000666  (since tha tis how it is used in the TSV files)
# For all files instead of using flybase:FBte0000666  we will use FBte0000666 (mostly it only happens in strings)
find ./data/ftp.flybase.net/releases/current/precomputed_files/ -type f -exec sed -i -e 's/\(FB[a-z]\{2\}\):\([0-9]\)/\1\2/g' -e 's/flybase:\([A-Za-z]\)/\1/g' {} +

find ./data/ontologies/ -type f -exec sed -i -e 's/\(FB[a-z]\{2\}\):\([0-9]\)/\1\2/g' -e 's/flybase:\([A-Za-z]\)/\1/g' {} +

rm -rf data/SO-Ontologies/
git clone https://github.com/The-Sequence-Ontology/SO-Ontologies data/SO-Ontologies

```

## :computer: Usage
- Run in MeTTA:
  ```
  export PYTHONPATH=metta_vspace:$PYTHONPATH
  metta 1-VSpaceTest.metta
  @h
  ```
- Launch the vspace-metta Jupyter notebook:
  ```
  ./start_jupyter.sh
  ```
- Run long huge tests of vspace-metta
  ```
  ./test_in_metta.sh
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
