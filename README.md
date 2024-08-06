
# :rocket: An Implementation of MeTTa designed to run on the Warren Abstract Machine (WAM)

Info at [./docs/OVERVIEW.md](docs/OVERVIEW.md) in this repository.

See [Tests](tests/) for MeTTa as well as [Results](reports/TEST_LINKS.md)

## :package: Getting Started

### :toolbox: Installation
Clone and set up MeTTaLog with the following commands:
```
git clone https://github.com/trueagi-io/metta-wam

cd metta-wam
. scripts/ensure_venv  # ensures we are runing in a python venv
pip install ansi2html   # needed for running tests
pip install hyperon  # needed for running tests
chmod +x INSTALL.sh  # Make sure the script is executable
. ./INSTALL.sh # Follow the default prompts 

```
The INSTALL.sh script handles the installation of essential components and updates:
#### Python Packages
- Ensures Python's `pip` is installed or installs it.
- **Installs mettalog**: Allows Rust MeTTa use extra functionality found in mettalog
- **Installs mettalog-jupyter-kernal**: Work with metta files in Jupyter Notebooks
- **Installs metakernal**: (No relation!) but allows our Jypter Kernel to work
- **Checks** if SWI-Prolog is already installed.
- **Installs or Updates** to ensure version 9.1 or higher is present.
- **Installs janus**: A Python package that interfaces with SWI-Prolog.
- **Installs pyswip**: Another Python package that provides further integration
**Note**: Running this script modifies software configurations and installs packages. Ensure you're prepared for these changes.

## :whale: Docker

To build a docker image containing MeTTaLog readily available run the
following command

```bash
docker build -t mettalog .
```

You may then enter a corresponding containter with the following
command

```bash
docker run -it --entrypoint 'bash -i' mettalog
```

Once inside the container you may enter the MeTTaLog REPL with the
following command

```bash
mettalog --repl
```

or run a metta script as follows

```bash
mettalog myprg.metta
```

or run/load a metta script and debug in the repl

```bash
mettalog myprg.metta --repl
```


Docker has a rich functionality set.  In particular it allows you to
[copy](https://docs.docker.com/engine/reference/commandline/container_cp/)
files back and forth between the host and the container.  For more
information about Docker you may refer to its
[manuals](https://docs.docker.com/manuals/) and its [reference
documentation](https://docs.docker.com/reference/).


## :computer: Usage and Demos

Interact directly with MeTTaLog through the REPL:
```bash
mettalog --repl

metta+> !(+ 1 1)
!(+ 1 1)

Deterministic: 2

; Execution took 0.000105 secs. (105.29 microseconds)
metta+>
```
Exit the REPL with `ctrl-D`.

**To run a script:**
```bash
mettalog tests/baseline_compat/hyperon-experimental_scripts/b0_chaining_prelim.metta
```

**Note:** Remember, the `MeTTa` script's name is case-sensitive. Do not confuse it with `metta`, which refers to the MeTTa Interpreter written in Rust.




** Launch Jupyter notebook: (in progress) **
 - Contains a Jupyter Kernel for MeTTa (allows runing of MeTTa scripts remotely)
```
./scripts/start_jupyter.sh
```

### Running Tests
Execute a unit test:
```bash
mettalog --test --clean tests/baseline_compat/hyperon-experimental_scripts/00_lang_case.metta
```
The output is saved as an HTML file in the same directory.

- Execute baseline sanity tests:
```
mettalog --test --clean ./tests/baseline-compat
```

## :raised_hands: Acknowledgments
Thanks to the Hyperon Experimental MeTTa, PySWIP teams, and Flybase for their contributions to this project.

## :phone: Support
For queries or suggestions, please open an issue on our [GitHub Issues Page](https://github.com/trueagi-io/metta-wam/issues).

## :scroll: License
MeTTaLog is distributed under the LGPL License, facilitating open collaboration and use.


## :gear: Prerequisites for using MeTTaLog in Rust 

- A build of [Hyperon Experimental](https://github.com/trueagi-io/hyperon-experimental) is required.
```bash
  /home/user$ metta
  metta> !(import-py! mettalog)
  metta> !(mettalog:repl)
  metta@&self +> !(ensure-loaded! whole_flybase)

  metta@&self +> !(let $query 
                     (, (fbgn_fbtr_fbpp_expanded $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $_ $_ $_ $_ $_) 
					    (dmel_unique_protein_isoforms $ProteinID $ProteinSymbol $TranscriptSymbol $_) 
						(dmel_paralogs $ParalogGeneID $ProteinSymbol $_ $_ $_ $_ $_ $_ $_ $_ $_) 
						(gene_map_table $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc) 
						(synonym $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $_ $_))
					(match &self $query $query))
                    
```


```shell
metta> !(test_custom_v_space)

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



## Python interaction

Module loading
; using the python default module resolver $PYTHONPATH
`!(import! &self motto.llm_gate)` 
; using the python path
`!(import! &self ../path/to/motto/llm_gate.py)`
; Rust way (was the only way to load llm_gate functions from Rust)
`!(import! &self motto)` 

; Script running
`!(pymain! &self ../path/to/motto/test_llm_gate.py ( arg1 arg2 ))`
; Single methods is python files
`!(pyr! &self ../path/to/motto/test_llm_gate.py "run_tests" ((= verbose True)))`

```
; Can define a shortcut
(: run-llm-tests (-> Bool Ratio))
(= 
  (run-llm-tests $verbose)
  (pyr! &self ../path/to/motto/test_llm_gate.py "run_tests" ((= verbose $verbose))))
```

## MeTTaLog Extras

```
; For the compiler to know that the member function will be a predicate 
(: member/2 Compiled)

; Declare member/2
(: member/2 Nondeterministic)


; Allow rewrites of member to invert using superpose
(: member/2 (Inverted 1 superpose))


```

```
; MeTTa file loading
!(include! &self ../path/to/motto/test_llm_gate.metta)

; Http Files
!(include! &self https://somewhere/test_llm_gate.metta)
```



```
; interfacing to Prolog
(:> OptionsList (List (^ Expresson (Arity 2))))
(:> ThreadOptions OptionsList)
(:> ThreadId Number)
(: make-thread (-> Expression ThreadOptions ThreadId))
(: thread_create/3 Deterministic)
;; (add-atom &self (Imported thread_create/3 2 make-thread))
(= 
  (make-thread $goal $options)
  (let True 
    (as-tf (thread_create! $goal $result $options))
	$result))

; returns a number and keeps going
!(make-thread (shell! "xeyes") ((detached False)))

```

To get comparable Interp vs Compiler statistics in one Main Output
```
clear ; mettalog --test --v=./src/main --log --html tests/*baseline*/ \
  --output=4-06-main-both --clean
clear ; mettalog --test --v=./src/canary-lng --log --html tests/*baseline*/ \
  --output=4-06-canary-lng-both --clean
clear ; mettalog --test --v=./src/canary --log --html tests/*baseline*/ \
  --output=4-06-canary-wd-both --clean
```



Vs for diffing
```

clear ; mettalog --test --v=./src/canary --log --html --compile=full tests/baseline_compat/ \
  --output=4-06-compile_full --clean

clear ; mettalog --test --v=./src/canary --log --html --compile=false tests/baseline_compat/ \
  --output=4-06-compile_false --clean

```

