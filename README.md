# :rocket: MeTTaLog: An Implementation of MeTTa in Prolog

MeTTaLog brings the power of MeTTa, a language designed to extend OpenCog Classic Atomese, into the Prolog environment. It's part of the OpenCog Hyperon initiative, offering enhanced semantics for meta-language features and supporting a broad spectrum of inference types.

[View the Latest Test Results](reports/TEST_LINKS.md)

## :package: Getting Started

### :toolbox: Installation
Clone and set up MeTTaLog with the following commands:
```
git clone https://github.com/trueagi-io/hyperon-wam
cd hyperon-wam

chmod +x INSTALL.sh  # Make sure the script is executable
./INSTALL.sh # Follow the prompts
```
The setup script handles the installation of essential components and updates:
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
docker run --rm -it --entrypoint bash mettalog
```

Once inside the container you may enter the MeTTaLog REPL with the
following command

```bash
MeTTa --repl
```

or run a metta script as follows

```bash
MeTTa myprg.metta
```

Beware that the container will be removed after leaving it.  If you
wish to keep it, then remove the `--rm` flag from the command line
used to enter the container.

Docker has a rich functionality set.  In particular it allows you to
[copy](https://docs.docker.com/engine/reference/commandline/container_cp/)
files back and forth between the host and the container.  For more
information about Docker you may refer to its
[manuals](https://docs.docker.com/manuals/) and its [reference
documentation](https://docs.docker.com/reference/).


## :computer: Usage and Demos
- Launch Jupyter notebook:
```
./scripts/start_jupyter.sh
```
Interact directly with MeTTaLog through the REPL:
```bash
mettalog --repl

metta &self +> !(+ 1 1)
!(+ 1 1)

Deterministic: 2

; Execution took 0.000105 secs. (105.29 microseconds)
metta &self +>
```
Exit the REPL with `ctrl-D`.

**To run a script:**
```bash
MeTTa tests/baseline_compat/hyperon-experimental_scripts/b0_chaining_prelim.metta
```

**Note:** Remember, the `MeTTa` script's name is case-sensitive. Do not confuse it with `metta`, which might refer to a different tool written in Rust.


**To run a metta file normally:**

```bash
MeTTa tests/baseline_compat/hyperon-experimental_scripts/b0_chaining_prelim.metta
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
For queries or suggestions, please open an issue on our [GitHub repository](https://github.com/trueagi-io/hyperon-wam/issues).

## :scroll: License
MeTTaLog is distributed under the LGPL License, facilitating open collaboration and use.


## :gear: Prerequisites for using in Rust 

- A build of [Hyperon Experimental](https://github.com/trueagi-io/hyperon-experimental) is required.
```bash
  /home/user$ metta
  metta> !(import-py! mettalog)
  metta> !(mettalog:repl)
  metta@&self +> !(ensure-loaded! whole_flybase)
  metta@&self +> !(, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32) (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33) (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42) (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc) (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))
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


# MeTTa Execution Modes
[These are inherited from SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=cmdline)

MeTTa can be executed in one of the following modes:

- `mettalog [option ...] script-file [arg ...]`
Arguments after the script file are made available in the MeTTa flag `argv`.

- `mettalog [option ...] metta-file ... [[--] arg ...]`
This is the normal way to start MeTTa. The MeTTa flag `argv` provides access to `arg ...`. If the options are followed by one or more MeTTa file names (i.e., names with extension .metta), these files are loaded. The first file is registered in the MeTTa flag `associated_file`.

- `mettalog -o output -c metta-file ...`
The `-c` option is used to compile a set of MeTTa files into an executable.

- `mettalog -o output -b prolog-bootfile metta-file ...`
Bootstrap compilation.


### Command Line Options for Running MeTTa
[These are inherited from SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=cmdline)

Boolean options may be written as `--name` (true), `--noname` or `--no-name` (false).

- `--debug-on-interrupt`
Enable debugging on an interrupt signal immediately.

- `--home[=DIR]`
Use `DIR` as home directory.

- `--quiet`
Set the MeTTa flag `verbose` to `silent`, suppressing informational and banner messages. Also available as `-q`.

- `--no-debug`
Disable debugging.

- `--no-signals`
Inhibit any signal handling by MeTTa, a property that is sometimes desirable for embedded applications. This option sets the flag signals to false. Note that the handler to unblock system calls is still installed. This can be prevented using `--sigalert=0` additionally.

- `--no-threads`
Disable threading for the multi-threaded version at runtime. See also the flags `threads` and `gc_thread`.

- `--no-packs`
Do not attach extension packages (add-ons).

- `--no-pce`
Enable/disable the xpce GUI subsystem. Using `--pce` loads the xpce system in user space and `--no-pce` makes it unavailable in the session.

- `--on-error=style`
How to handle errors.

- `--on-warning=style`
How to handle warnings.

- `--pldoc[=port]`
Start the PlDoc documentation system on a free network port. If port is specified, the server is started at the given port and the browser is not launched.

- `--sigalert=NUM`
Use signal NUM (1 ... 31) for alerting a thread. If NUM is 0 (zero), this handler is not installed.

- `--no-tty`
Unix only. Switches controlling the terminal.

- `-O`
Optimised compilation.

- `-l file.metta`
Load file.metta.

- `-s file.metta`
Use file.metta as a script file.

- `-f file.metta`
Use file.metta as initialisation file instead of the default `init.metta`.

- `-F script`
Select a startup script from the MeTTa home directory.

- `-x prolog-bootfile`
Boot from prolog-bootfile instead of the system''s default boot file.

- `-p alias=path1[:path2 ...]`
Define a path alias for file_search_path.

- `--`
Stops scanning for more arguments.


### Controlling the Stack Sizes

```shell
$ mettalog --stack-limit=32g
```

- `--stack-limit=size[bkmg]`
Limit the combined size of the MeTTa stacks to the indicated size.

- `--table-space=size[bkmg]`
Limit for the table space.

- `--shared-table-space=size[bkmg]`
Limit for the table space for shared tables.

### Running Goals from the Command Line

- `-g goal`
Goal is executed just before entering the top level.

```shell
% mettalog <options> -g (go) -g (quit)
```

- `-t (goal)`
Use goal as an interactive top level instead of the default goal `!(repl!)`.

### Compilation Options
[MeTTa Code is tranliterated to SWI-Prolog code and compiled](https://www.swi-prolog.org/pldoc/man?section=runtime)

- `-c file.metta ...`
Compile files into an intermediate code file.

- `-o output`
Used in combination with `-c` to determine the output file for compilation.


### Informational Command Line Options

- `--arch`
When given as the only option, it prints the architecture identifier (see MeTTa flag `arch`) and exits. See also `--dump-runtime-variables`.

- `--dump-runtime-variables [=format]`
When given as the only option, it prints a sequence of variable settings that can be used in shell scripts to deal with MeTTa parameters.

```shell
eval `mettalog --dump-runtime-variables`
cc -I$PLBASE/include -L$PLBASE/lib/$PLARCH ...
```

- `--help`
When given as the only option, it summarises the most important options.

- `--version`
When given as the only option, it summarises the version and the architecture identifier.

- `--abi-version`
Print a key (string) that represents the binary compatibility on a number of aspects.


### Maintenance Options
[These are inherited from SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=cmdline)

- `-b initfile.metta ...-c file.metta ...`
Boot compilation.

- `-d token1,token2,...`
Print debug messages for DEBUG statements.


