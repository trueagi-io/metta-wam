# MeTTaLog Language server

NOTE: very much a work in progress.

An LSP server for the programming language MeTTa. The server itself is written in Prolog, and requires MeTTaLog (found at https://github.com/trueagi-io/metta-wam) for some of the tools it has there. It will soon be packaged as part of the MeTTaLog project.

It is based off the `lsp_server` for SWI-Prolog (found at https://github.com/jamesnvc/lsp_server). The README there may still be useful is it describes how to connect to an LSP server from different editors - we hav currently only tested VSCode.

Still a work-in-progress - we currently supports showing documentation on hover. We are spending time getting a well designed core, which will make the other functionality easy to implement.

Only tested with SWI-Prolog and version requirements are whatever is required for the MeTTalog project (9.3.9 or higher). SWI-Prolog as installed as part of MeTTalog installation.

There are two components that need to be installed: the prolog package needs to be installed in SWI-Prolog (and `logicmoo_utils` if you don't have that already, and there are specific installation requirements for each editor. Instructions for VSCode are given below.

## Installing the SWI-Prolog package(s)

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

* You may need to install the `logicmoo_utils` package. If you already have this (you can check with `pack_list('logicmoo_utils').`, `i` as the first character of the return line means installed, `p` means not installed) it won't hurt to install it again:

```
pack_install('logicmoo_utils').
```

## Installing for VSCode

VSCode require a `.vsix` file. There are two ways to get that:

### From the marketplace

![screenshot](images/VSCode_lsp_install.png)

From VSCode, go to the extensions button on the left (four squares with one of them offset, circled in red), at the top of the EXTENSIONS pane, go to the place where it says `Search Extensions in Marketplace`, and type `metta`. Several extensions will appear, you want the `metta-lsp` server written by Roy Ward (the one with a green oval around it). Click the `Install` button and you are done.

### Build the `.vsix` file yourself

* install `vsce` (`npm install -g vsce`, although the `-g` didn't work for me)

* `cd vscode`

* `vsce package` (the might be some Javascript dependencies required to be installed before this works). If you are using the instructions from metta-lsp-0.0.2.vsix, DO NOT `vsce publish` as is suggested there unless you want to fork this and add your own version to the marketplace.

* Take the resulting `.vsix` file (called `metta-lsp-0.0.2.vsix` or something similar), and install into VSCode (find the `...` at the top right of the EXTENSIONS panel, click on it and select `Install from VSIX...`.
