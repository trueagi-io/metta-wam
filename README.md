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
- Ensures Python's `pip` is installed or installs it.
- Installs necessary Python packages: `mettalog`, `mettalog-jupyter-kernel`, `metakernel`, `janus`, and `pyswip`.
- Updates the PATH for the current user session.
- Checks for SWI-Prolog installation, updating or installing as needed.

**Note**: Running this script modifies software configurations and installs packages. Ensure you're prepared for these changes.

## :whale: Docker
Build and run MeTTaLog in Docker:
```bash
docker build -t mettalog .
```
Enter the container:
```bash
docker run --rm -it --entrypoint bash mettalog
```
Within the container, access the MeTTaLog REPL or execute a script:
```bash
MeTTa --repl
# or
MeTTa myprg.metta
```
**Note**: The container is removed upon exit unless `--rm` is omitted. Docker facilitates file transfers between host and container.

## :computer: Usage and Demos
- Launch Jupyter notebook:
```
./scripts/start_jupyter.sh
```
- Execute baseline sanity tests:
```
mettalog --test --clean ./tests/baseline-compat
```
Interact directly with MeTTaLog through the REPL:
```bash
mettalog --repl

metta &self +> !(+ 1 1)
# Output: Deterministic: 2
```
Exit the REPL with `ctrl-D`.

### Running Tests
Execute a unit test:
```bash
mettalog --test --clean tests/baseline_compat/hyperon-experimental_scripts/00_lang_case.metta
```
The output is saved as an HTML file in the same directory.

**To run a script:**
```bash
MeTTa tests/baseline_compat/hyperon-experimental_scripts/b0_chaining_prelim.metta
```

## :bulb: Key Features
- **Relational Programming**: Harness MeTTa's paradigm for complex knowledge representation and querying.
- **Flybase Integration**: Utilize genetic data for enriched learning experiences.

## :dart: Version Space Advantages
- **Incremental Learning**: Adapt to new examples without revisiting previous data.
- **Consistency**: Keep hypotheses in line with observed data.
- **Interpretability**: Understand learning outcomes through G and S sets.

## :raised_hands: Acknowledgments
Thanks to the Hyperon Experimental MeTTa, PySWIP teams, and Flybase for their contributions to this project.

## :phone: Support
For queries or suggestions, please open an issue on our [GitHub repository](https://github.com/trueagi-io/hyperon-wam/issues).

## :scroll: License
MeTTaLog is distributed under the LGPL License, facilitating open collaboration and use.


## :gear: Prerequisites for using in Rust 
- A build of [Hyperon Experimental](https://github.com/trueagi-io/hyperon-experimental) is required.
``` bash
  /home/user$ metta
  metta> !(import-py! mettalog)
  metta> !(mettalog:repl)
  metta@&self +> !(ensure-loaded! whole_flybase)
  metta@&self +> !(, (fbgn_fbtr_fbpp_expanded! $GeneID $TranscriptType $TranscriptID $GeneSymbol $GeneFullName $AnnotationID $28 $29 $30 $31 $32) (dmel_unique_protein_isoforms! $ProteinID $ProteinSymbol $TranscriptSymbol $33) (dmel_paralogs! $ParalogGeneID $ProteinSymbol $34 $35 $36 $37 $38 $39 $40 $41 $42) (gene_map_table! $MapTableID $OrganismAbbreviation $ParalogGeneID $RecombinationLoc $CytogeneticLoc $SequenceLoc) (synonym! $SynonymID $MapTableID $CurrentSymbol $CurrentFullName $43 $44))
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


