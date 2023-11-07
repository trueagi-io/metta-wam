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

# Add our python scripts PYTHONPATH  ( Later on we might use pip install . )
export PYTHONPATH=$PWD/metta_vspace:$PYTHONPATH

# set our precomputed url
export PRECOMPUTED_URL=ftp.flybase.org/releases/FB2023_04/precomputed_files/

# Download necessary files
wget --no-parent -A .gz -r -P ./data/ http://$PRECOMPUTED_URL

# set our precomputed loc
export PRECOMPUTED_LOC=./data/$PRECOMPUTED_URL

# see if the server gave us duplicated files (usually two files!)
find $PRECOMPUTED_LOC -type f -name '*_fb_*' -exec bash -c 'if [[ -f ${1/_fb_????_??/} ]]; then ls -l ${1}; ls -l ${1/_fb_????_??/};  echo "Will delete:" ${1/_fb_????_??/}; fi' _ {} \;

# delete the more ambiguous duplicated files
find $PRECOMPUTED_LOC -type f -name '*_fb_*' -exec bash -c 'if [[ -f ${1/_fb_????_??/} ]]; then rm -f ${1/_fb_????_??/}; fi' _ {} \;

# Should be arround 587M
du -hs $PRECOMPUTED_LOC

# Decompress the downloaded files
find $PRECOMPUTED_LOC -type f -name "*.gz" -execdir gunzip {} \;

# Check that it should be arround 7.2G now
du -hs $PRECOMPUTED_LOC

# For all files instead of using FBte:0000666  we will use FBte0000666  (since that is how it is used in the TSV files)
# Also flybase:FBte0000666  will become FBte0000666 (mostly it only happens in strings)
find $PRECOMPUTED_LOC -type f -exec sed -i -e 's/\(FB[a-z]\{2\}\):\([0-9]\)/\1\2/g' -e 's/flybase:\([A-Za-z]\)/\1/g' {} \;
find $PRECOMPUTED_LOC -name "*.fb" -exec sed -i -e 's/FB:FB/FB/g' {} \;
find $PRECOMPUTED_LOC -name "*.json" -exec sed -i -e 's/FLYBASE:FB/FB/g' {} \;


```

#### Completely Optional

For testing/viewing to see how we see the flybase as MeTTa.
Or to load it into a Rust Space instead of VSpace.

```

# takes around 8 minutes
./scripts/convert_to_metta.sh ./data/ftp.flybase.org/releases/FB2023_04/precomputed_files/

# to get an atoms count (should be at least 56 million)
find ./data/ftp.flybase.org/releases/FB2023_04/precomputed_files/ -type f -name "*.metta" -exec wc -l {} +

```



## :computer: Various Usages and Demos
- Run in MeTTA:
  ```
  export PYTHONPATH=metta_vspace:$PYTHONPATH
  metta 1-VSpaceTest.metta
  @h
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
