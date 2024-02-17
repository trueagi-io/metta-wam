# :rocket: Version Space Candidate Elimination inside of MeTTa

The Candidate Elimination algorithm is a conceptual learning algorithm that incrementally refines the version space boundary. This implementation focuses on bringing this algorithm into the MeTTa relational programming environment.

## :pushpin: Overview
- The algorithm refines the version space boundary using observed training tests.
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
git clone https://github.com/trueagi-io/hyperon-wam

# Change to the cloned directory
cd hyperon-wam

chmod +x INSTALL.sh  #in case Git lost +x n the file
./INSTALL.sh # Answer the questions and follow the directions
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
- **Updates or Installs** specific SWI-Prolog packages (`predicate_streams`, `trueagi-io_utils`, and `dictoo`) as needed.

#### System Requirements
- **Requires sudo access** for certain operations, such as installing SWI-Prolog and pip.
- **Modifies system package sources** to include the SWI-Prolog development repository.
- **Installs and updates packages** using the system's package manager and Python's pip.

---

**Note**: Running this script will modify your system's software configuration, including adding repositories and installing new packages. Ensure you understand these changes and have the necessary permissions before proceeding.

---

## :whale: Docker

To build a docker image containing MeTTaLog readily available run the
following command

```bash
docker build -t MeTTa --log .
```

You may then enter a corresponding containter with the following
command

```bash
docker run --rm -it --entrypoint bash MeTTa --log
```

Once inside the container you may enter the MeTTaLog REPL with the
following command

```bash
MeTTa --log --repl
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

## :computer: Various Usages and Demos



- Launch hyperon-wam Jupyter notebook:
```
./scripts/start_jupyter.sh
```

- Run the baseline sanity tests of hyperon-wam
```
MeTTa --log --test --clean ./tests/baseline-compat
```
 

`MeTTa` vs `mettalog`
`MeTTa --log == mettalog`
`mettalog --compatio == MeTTa`


Within the REPL, you can interact directly with the MeTTaLog system. For instance:

```bash
MeTTa --log --repl

metta &self +> !(+ 1 1)
!(+ 1 1)

Deterministic: 2

; Execution took 0.000105 secs. (105.29 microseconds)
metta &self +>
```

To exit the REPL, press `ctrl-D`.


**To run the REPL (such as to debug) once the file is loaded:**

```bash
MeTTa --log tests/compat/scripts/b0_chaining_prelim.MeTTa --log --repl
```


#### Running Tests

To run your first unit test (referred to as a LoonIt Test):

```bash
MeTTa --log --test --clean tests/compat/scripts/00_lang_case.metta
```

Upon execution, the output will be saved as `tests/compat/scripts/00_lang_case.metta.html`.

**Note:** Remember, the `MeTTa` script's name is case-sensitive. Do not confuse it with `metta`, which might refer to a different tool written in Rust.


**To run a metta file normally:**

```bash
MeTTa tests/compat/scripts/b0_chaining_prelim.metta
```


## Familiarize Yourself with MeTTa`

1. [Read the MeTTa specification](https://wiki.opencog.org/wikihome/images/b/b7/MeTTa_Specification.pdf).
2. [Learn the Minimal instruction set](https://github.com/trueagi-io/hyperon-experimental/blob/main/docs/minimal-metta.md)


**To run all tests:**

```bash
./total_loonits.sh tests/compat/scripts/
```

**To run a single test:**

```bash
MeTTa --log --html tests/compat/scripts/b0_chaining_prelim.metta
```

```
MeTTa --log
metta &self +> !(import! &self http://logicmoo.org/public/metta/whole_flybase.metta.qlf.gz)
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
```













- Run in Rust MeTTA:
```
  export PYTHONPATH=metta_vspace:$PYTHONPATH
  metta 1-VSpaceTest.metta
  @h
  metta@&self +> !(ensure-loaded! whole_flybase)
  metta@&self +> !(, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32) (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33) (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42) (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc) (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))
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
- For support, queries, or feature suggestions, kindly open an issue on our [GitHub repository](https://github.com/trueagi-io/hyperon-wam/issues).

## :scroll: License
- hyperon-wam is distributed under the LGPL License.

## metta_interp.PL
[MeTTaLog (In Prolog)](https://github.com/trueagi-io/hyperon-wam/blob/main/MeTTaLog.md)
# MeTTaLog: An Implementation of MeTTa in Prolog

MeTTaLog is a superfluous implementation of MeTTa, a language designed to succeed OpenCog Classic Atomese. As part of the OpenCog Hyperon initiative, MeTTa offers well-defined semantics for meta-language features, supports various types of inference, and more.

[Latest Test Results](reports/TEST_LINKS.md)

## Installation Guide

MeTTaLog provides an environment for managing and interacting with Prolog-based logic systems. To set up MeTTaLog on your system, follow the steps below:

### 1. Clone the MeTTaLog Repository

Clone the repository using the following command:

```bash
git clone https://github.com/trueagi-io/hyperon-wam
```

Navigate to the cloned directory:

```bash
cd hyperon-wam
```

### 2. Build and Install Required Packages

Run the installation script:

```bash
./INSTALL.sh
```

This script will install necessary Prolog packs, including `predicate_streams`, `trueagi-io_utils`, and `dictoo`. During installation, you may encounter prompts for configuration choices. It's generally recommended to accept the default options.

### 3. Update the PATH

After installation, add the `hyperon-wam` directory to your system's PATH to easily access the MeTTaLog executable:

```bash
echo 'export PATH="$PATH:$(pwd)"' >> ~/.profile
source ~/.profile
```

**Note:** This command updates the PATH for the current user. If you need system-wide access, consider adding the path to a system-wide profile file, such as `/etc/profile`.

## Usage

Once installed, MeTTaLog can be accessed through the `MeTTa` script included in the repository, which serves as a front-end for the compiled `Sav.$hostname.MeTTaLog` executable. The name "MeTTa" is used in this context because the corresponding Rust executable is all lowercase `metta`.



**See `--help` for more options:**
```
MeTTa --log --help
 CMD: MeTTa --log
 Usage: MeTTa [options] <metta-files|directories> ... [-- arg ...passed to your program...]
        MeTTa [options] [-o executable] -c metta-file1 -c metta-file2 ... to compile into executable ...
        MeTTa --log --help         Display this message
        MeTTa --log --version      Display version information
        MeTTa --log --abi-version  Display ABI version key
        MeTTa --log --arch         Display architecture
        MeTTa --log --dump-runtime-variables[=format]
                        Dump link info in sh(1) format

    -x state                 Start from state (must be first)
    -g goal                  Run goal (may be repeated)
    -t toplevel              Toplevel goal
    -f file                  User initialisation file
    -F file                  Site initialisation file
    -l file                  Script source file
    -s file                  Script source file
    -p alias=path            Define file search path 'alias'
    -O                       Optimised compilation
    --on-error=style         One of print, halt or status
    --on-warning=style       One of print, halt or status
    --tty[=bool]             (Dis)allow tty control
    --packs[=bool]           Do (not) attach add-ons
    --signals[=bool]         Do (not) modify signal handling
    --threads[=bool]         Do (not) allow for threads
    --debug[=bool]           Do (not) generate debug info
    --debug-on-interrupt[=bool] Trap the debugger on interrupt
    --quiet[=bool] (-q)      Do (not) suppress informational messages
    --traditional            Disable extensions of version 7
    --home[=DIR]             Print home or use DIR as SWI-Prolog home
    --stack-limit=size[BKMG] Specify maximum size of Prolog stacks
    --table-space=size[BKMG] Specify maximum size of SLG tables
    --shared-table-space=size[BKMG] Maximum size of shared SLG tables
    --pce[=bool]             Make the xpce gui available
    --pldoc[=port]           Start PlDoc server [at port]
    --python[=bool]          Enable or disable Python support (default: enable)
    --repl                   Start the REPL (Read-Eval-Print Loop) after processing metta files.
                             If no metta files are provided, this is the default behavior.
    --timeout=seconds        Kill the script after so many seconds.
    --html[=bool]            Save an HTML file containing terminal output in the same
                             directory as the input file or directory.
                             Defaults to true if exactly one metta file or directory argument was provided




 Boolean options may be written as --name=bool, --name, --no-name or --noname.
 Both '-' or '_' are accepted as word-separator for long options.

 Configuration File:
    This script reads options from the ~/.MeTTa --logrc file, one option per line.
    Options specified in ~/.MeTTa --logrc are processed before command-line arguments.

```


# Acknowledgments

Special thanks to the OpenCog community and everyone involved in the development and conceptualization of Hyperon and MeTTa.


# MeTTa Execution Modes
[These are inherited from SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=cmdline)

MeTTa can be executed in one of the following modes:

- `MeTTa --log [option ...] script-file [arg ...]`
Arguments after the script file are made available in the MeTTa flag `argv`.

- `MeTTa --log [option ...] metta-file ... [[--] arg ...]`
This is the normal way to start MeTTa. The MeTTa flag `argv` provides access to `arg ...`. If the options are followed by one or more MeTTa file names (i.e., names with extension .metta), these files are loaded. The first file is registered in the MeTTa flag `associated_file`.

- `MeTTa --log -o output -c metta-file ...`
The `-c` option is used to compile a set of MeTTa files into an executable.

- `MeTTa --log -o output -b prolog-bootfile metta-file ...`
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
$ MeTTa --log --stack-limit=32g
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
% MeTTa --log <options> -g (go) -g (quit)
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
eval `MeTTa --log --dump-runtime-variables`
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


