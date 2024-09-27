# MeTTaLog Language server

NOTE: very much a work in progress.

An LSP server for the programming language MeTTa. The server itself is written in Prolog, and requires MeTTaLog (found at https://github.com/trueagi-io/metta-wam) for some of the tools it has there. It will soon be packaged as part of the MeTTaLog project.

It is based off the `lsp_server` for SWI-Prolog (found at https://github.com/jamesnvc/lsp_server). The README there may still be useful is it describes how to connect to an LSP server from different editors - we hav currently only tested VSCode.

Still a work-in-progress - we currently supports showing documentation on hover. We are spending time getting a well designed core, which will make the other functionality easy to implement.

Only tested with SWI-Prolog and version requirements are whatever is required for the MeTTalog project (9.3.9 or higher). SWI-Prolog as installed as part of MeTTalog installation.

There are two components that need to be installed: the prolog package needs to be installed in SWI-Prolog, and there are specific installation requirements for each editor. Instructions for VSCode are given below.

## Installing the SWI-Prolog package

NOTE: this will at some point be added to the MeTTalog installation and become unncessary.

This is a slightly modified version of the directions to be found at https://www.swi-prolog.org/howto/Pack.html

* Make sure that you are in the `metta-wam/src/packs` directory. From the `metta-wam` directory,

```
cd src/packs
```

* Create a `.tgz` (tarred and gzipped) file. Note that the suffix should be `.tgz`, not the more usual `.tar.gz`.

```
tar -zcvf lsp_server_metta-0.0.3.tgz --exclude=vscode lsp_server_metta
```

* From SWI-Prolog, install file directly as a package:

```
?- pack_install('lsp_server_metta-0.0.3.tgz').
```

## Installing for VSCode

VSCode require a `.vsix` file. There are two ways to get that:

UNFINISHED

* VSCode

  - download the latest ~.vsix~ file from the [[https://github.com/jamesnvc/lsp_server/releases][releases page]]
  - clone this repo and copy/symlink the ~vscode/~ directory to ~~/.vscode/extensions/~
  - clone and build the ~.vsix~ file yourself by the follwing steps:
    1. install ~vsce~ (~npm install -g vsce~)
    2. run ~vsce publish~ from the ~vscode/~ directory
    3. add the resulting ~.vsix~ to VSCode.
