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
git clone https://github.com/logicmoo/metta-vspace

# Change to the cloned directory
cd metta-vspace

# Install the package
pip install .

# Download necessary files
wget --no-parent -A .gz -r http://ftp.flybase.org/releases/FB2023_04/precomputed_files/


# Unzip them
find ftp.flybase.org -name "*.gz" -execdir gunzip {} \;
find ftp.flybase.org -name "*.zip" -execdir unzip -d ./ {} \;
du -h ftp.flybase.org  # should be arround 2.0G
```

## :computer: Usage
- Launch the metta-vspace Jupyter notebook:
  ```
  ./start_jupyter.sh
  ```
- Ensure metta-vspace operates correctly within MeTTa:
  ```
  ./test_in_metta.sh
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
- For support, queries, or feature suggestions, kindly open an issue on our [GitHub repository](https://github.com/logicmoo/metta-vspace/issues).

## :scroll: License
- metta-vspace is distributed under the LGPL License.

