# MeTTaLog Language Server

NOTE: very much a work in progress.

An LSP server for the programming language MeTTa. The server itself is written in Prolog and requires MeTTaLog (found at https://github.com/trueagi-io/metta-wam) for some of the tools it has there. It will soon be packaged as part of the MeTTaLog project.

It is based on the `lsp_server` for SWI-Prolog (found at https://github.com/jamesnvc/lsp_server). The README there may still be useful as it describes how to connect to an LSP server from different editors - we have currently only tested VSCode.

Still a work-in-progress - we currently support showing documentation on hover. We are spending time getting a well-designed core, which will make other functionality easier to implement.

Only tested with SWI-Prolog and version requirements are whatever is required for the MeTTalog project (9.3.9 or higher). SWI-Prolog is installed as part of the MeTTalog installation.

There are two components that need to be installed: the Prolog package needs to be installed in SWI-Prolog (and `logicmoo_utils` if you don't have that already), and there are specific installation requirements for each editor.

See below instructions for:
 - [VScode](#installing-for-vscode)
 - [Emacs](#installing-with-emacs)
 - [NeoVim](#installing-with-neovim)


## Installing the SWI-Prolog package(s) (Optional, see "Configuration" below)

NOTE: this will at some point be added to the MeTTalog installation and become unnecessary.

This is a slightly modified version of the directions found at https://www.swi-prolog.org/howto/Pack.html

* Make sure that you are in the `metta-wam/src/packs` directory. From the `metta-wam` directory:

```bash
cd src/packs
```

* Create a `.tgz` (tarred and gzipped) file. Note that the suffix should be `.tgz`, not the more usual `.tar.gz`.

```bash
tar -zcvf lsp_server_metta-0.0.3.tgz --exclude=vscode lsp_server_metta
```

* From SWI-Prolog, install the file directly as a package:

```prolog
?- pack_install('lsp_server_metta-0.0.3.tgz').
```

* You may need to install the `logicmoo_utils` package. If you already have this (you can check with `pack_list('logicmoo_utils').`, where `i` as the first character of the return line means installed, `p` means not installed), it won't hurt to install it again:

```prolog
pack_install('logicmoo_utils').
```

## Installing for VSCode

VSCode requires a `.vsix` file. Currently, you'll have to build it yourself.

### Build the `.vsix` file yourself

* Ensure you have a recent version of node installed. If you don't, try installing [nvm.sh](https://nvm.sh) and install the latest stable node.

* Make sure you're in the `vscode` directory:

  ```bash
  cd lsp_server_metta/vscode
  ```

* Install the required dependencies for packaging:

  ```bash
  npm install
  ```

* Package the extension:

  ```bash
  npx vsce package
  ```

  You may be prompted about a missing license file. Confirm by typing `y` to continue.

* The resulting `.vsix` file will be located in the `vscode` directory (e.g., `metta-lsp-0.2.0.vsix` or something similar).

* Install the resulting `.vsix` file into VSCode (find the `...` at the top right of the EXTENSIONS panel, click on it and select `Install from VSIX...`).

### Configuring

By default, the LSP server will assume you have already installed the LSP server as a pack.
Instead of doing this, you can also configure the LSP to run by loading the server from the source repo.

To do this, go to "extensions", click on the "manage" cog on the "metta-lsp" extension, then settings.

From the settings page, check `metta-lsp > server: Debug Lsp` to true and set `Metta-lsp > Server: Mettalog Path` to the path to the metta-wam directory (e.g. `/home/myusername/src/metta-wam`).


Alternatively, you can start the LSP server yourself, by running the following command in the metta-wam directory:

```bash
swipl -l libraries/lsp_server_metta/prolog/lsp_server_metta.pl -g lsp_server_metta:main -t 'halt' -- port 40222
```

Then, go to the extension settings, uncheck `Metta-lsp › Server: Spawn Process` and make sure that `Metta-lsp › Server: Port` matches the argument used to start the server (e.g. `40222` in the example above).
This will make VSCode connect to the running server instead of trying to start the process itself.

## Installing with Emacs

### Eglot

If you've installed the `lsp_server_metta` pack:

```emacs-lisp
(add-to-list 'eglot-server-programs
              (cons 'metta-mode
                    (list
                     "swipl"
                     "-g" "use_module(library(lsp_server_metta))."
                     "-g" "lsp_server_metta:main"
                     "-t" "halt"
                     "--"
                     "port" :autoport)))
```

If you want to load directly from the source:

```emacs-lisp
;; Replace this with the path to your metta-wam directory
(let ((mettalog-dir "/path/to/metta-wam"))
   (add-to-list 'eglot-server-programs
                (cons 'metta-mode
                      (list
                       "env"
                       (concat "METTALOG_DIR=" mettalog-dir)
                       (concat "SWIPL_PACK_PATH=" mettalog-dir "/libraries")
                       "swipl"
                       "-l" (concat mettalog-dir "/libraries/lsp_server_metta/prolog/lsp_server_metta.pl")
                       "-g" "lsp_server_metta:main"
                       "-t" "halt"
                       "--"
                       "port" :autoport))))
```

Note that the server can take a while to start up, so if you have issues with eglot timing out trying to connect, it may be preferable to use the below method.

To connect to a running server, started like this:

```bash
swipl -l libraries/lsp_server_metta/prolog/lsp_server_metta.pl -g lsp_server_metta:main -t 'halt' -- port 40222
```

Run `C-u M-x eglot` and enter `localhost:40222` (or whatever port you started the server on).

### lsp-mode

If you've installed the `lsp_server_metta` pack:

```emacs-lisp
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection (lambda (port)
                                        (list
                                         "swipl"
                                         "-g" "use_module(library(lsp_server_metta))."
                                         "-g" "lsp_server_metta:main"
                                         "-t" "halt"
                                         "--"
                                         "port" port)))
  :major-modes '(metta-mode)
  :activation-fn (lsp-activate-on "metta")
  :server-id 'metta-lsp))
```

If you want to load directly from the source:

```emacs-lisp
;; Replace this with the path to your metta-wam directory
(let ((mettalog-dir "/path/to/metta-wam"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tcp-connection (lambda (port)
                                          (list
                                           "swipl"
                                           "-l" (concat mettalog-dir "/libraries/lsp_server_metta/prolog/lsp_server_metta.pl")
                                           "-g" "lsp_server_metta:main"
                                           "-t" "halt"
                                           "--"
                                           "port" port)))
     :environment-fn (lambda ()
                       (list ("METTALOG_DIR" . mettalog-dir)
                             ("SWIPL_PACK_PATH". (concat mettalog-dir "/libraries"))))
    :major-modes '(metta-mode)
    :activation-fn (lsp-activate-on "metta")
    :server-id 'metta-lsp)))

```

## Installing with Neovim

It currently isn't straightforward to have an automatically started server over a TCP port with neovim.

Note that the LSP doesn't configure a metta mode itself. If you don't already have one set up, at a minimum you'll want to figure neovim to recognize the type of `.metta` files. To do this, create the following file at `~/.config/nvim/ftdetect/metta.vim`:

```vim
au BufRead,BufNewFile *.metta    set filetype=metta
```

### Neovim >= 0.11

As of version 0.11, Neovim now has easier support for configuring an LSP server without additional plugins.


#### Connecting to a Running Server

Since neovim can't start its own LSP server that will connect over TCP, you'll have to start your own server, by running the following command in the Mettalog directory:

```bash
swipl -l libraries/lsp_server_metta/prolog/lsp_server_metta.pl -g lsp_server_metta:main -t 'halt' -- port 40222
```

Then create the following file at `~/.config/nvim/lsp/metta.lua`:

```lua
return {
   cmd = vim.lsp.rpc.connect('127.0.0.1', 40222),
   --                  this port argument ^
   -- should match the above argument used when starting the server
   root_markers = { '.git', },
   filetypes = { 'metta' },
}
```

Then need to enable the LSP by adding the following line to your `init.lua`:

```lua
vim.lsp.enable({'metta'})
```
#### Automatically Started Server Over stdio


To run from the `metta_wam` directory, put the following file at `~/.config/nvim/lsp/metta.lua`:

```lua
local mettaDir = '/Users/james/Work/metta/metta-wam'
return {
   cmd = { 'swipl',
           '-l', mettaDir .. '/libraries/lsp_server_metta/prolog/lsp_server_metta.pl',
           '-g', 'lsp_server_metta:main',
           '-t', 'halt',
           '--', 'stdio' },
   cmd_cwd = mettaDir,
   cmd_env = { METTALOG_DIR = mettaDir;
               SWIPL_PACK_PATH = mettaDir .. '/libraries'; },
   root_markers = { '.git', },
   filetypes = { 'metta' },
}
```

If the server has been installed as a pack:

```lua
return {
   cmd = { 'swipl',
           '-g', 'use_module(library(lsp_server_metta))',
           '-g', 'lsp_server_metta:main',
           '-t', 'halt',
           '--', 'stdio' },
   root_markers = { '.git', },
   filetypes = { 'metta' },
}
```

In either case, you'll then need to enable the LSP by adding the following line to your `init.lua`:

```lua
vim.lsp.enable({'metta'})
```

### Neovim < 0.11

To have an automatically started server over stdio, add the following to your `~/.config/nvim/init.lua`.

If you've installed the `lsp_server_metta` pack:

```lua
vim.api.nvim_create_autocmd(
   'FileType', {
      pattern = 'metta',
      callback = function(ev)
         vim.lsp.start({
               name = 'metta_lsp',
               cmd = {'swipl',
                     '-g', 'use_module(library(lsp_server_metta)).',
                     '-g', 'lsp_server_metta:main',
                     '-t', 'halt',
                     '--', 'stdio'},
               root_dir = vim.fs.root(ev.buf, {'README.md'}),
         })
      end,
})
```

To run from the `metta_wam` directory

```lua
vim.api.nvim_create_autocmd(
   'FileType', {
      pattern = 'metta',
      callback = function(ev)
         local mettalogDir = '/path/to/metta-wam'
         vim.lsp.start({
               name = 'metta_lsp',
               cmd = { 'swipl',
                       '-l', mettalogDir .. '/libraries/lsp_server_metta/prolog/lsp_server_metta.pl',
                       '-g', 'lsp_server_metta:main',
                       '-t', 'halt',
                       '--', 'stdio' },
               cmd_cwd = mettalogDir,
               cmd_env = { METTALOG_DIR = mettalogDir;
                           SWIPL_PACK_PATH = mettalogDir .. '/libraries'; },
               root_dir = vim.fs.root(ev.buf, {'README.md'}),
         })
      end,
})
```

Alternatively, you can start the LSP server manually by running the following command in the metta-wam directory:

```bash
swipl -l libraries/lsp_server_metta/prolog/lsp_server_metta.pl -g lsp_server_metta:main -t 'halt' -- port 40222
```

And then configure neovim to connect to it with the below configuration, where the port argument to `vim.lsp.rpc.connect` matches that used to start the server above (40222 in this case):

```lua
vim.api.nvim_create_autocmd(
   'FileType', {
      pattern = 'metta',
      callback = function(ev)
         vim.lsp.start({
               name = 'metta_lsp',
               cmd = vim.lsp.rpc.connect('127.0.0.1', 40222),
               root_dir = vim.fs.root(ev.buf, {'README.md'}),
         })
      end,
})
```

## Configuring LLM Integrations

To use the LLM-based LSP code actions, some configuration is required.

### Using OpenAI's API

Set the environment variable `OPENAI_API_KEY` and optionally `METTA_LLM_MODEL` (defaults to `gpt-3.5-turbo`).

If you are starting the server manually and connecting to it, set this environment variable there, e.g.

```bash
OPENAI_API_KEY="abc" METTA_LLM_MODEL="gpt-4o" swipl -l ... # as above
```

If you're configuring your editor to start the server automatically, see the appropriate section below under [Configuring Your Editor For LLM Usage](#configuring-your-editor-for-llm-usage).


### Running Your Own LLM

To run a local LLM for your editor to communicate to, install & run [ollama](https://ollama.com).

For improved "knowledge" of Metta, you can use the included Modelfile to create an LLM variant with a system prompt that includes more context about the Metta language. Do this by running the following command from the `metta-wam` directory:

```bash
ollama create llama3-metta -f libraries/lsp_server_metta/Modelfile
```

Then configure start your server or configure your editor as indicated under [Configuring Your Editor For LLM Usage](#configuring-your-editor-for-llm-usage), except instead of setting `OPENAI_API_KEY` to whatever value, set `METTA_LLM_URL` to `http://localhost:11434/api/generate` and set `METTA_LLM_MODEL` to `llama3-metta`.

### Configuring Your Editor For LLM Usage

#### VSCode

Go to "extensions", click on the "manage" cog on the "metta-lsp" extension, then settings.

From here, you can set "enable integration with chatgpt" to true and set the API key or LLM URL and model.

#### Emacs

##### Eglot

If you've installed the `lsp_server_metta` pack:

```emacs-lisp
(add-to-list 'eglot-server-programs
              (cons 'metta-mode
                    (list
                     "env"                    ; added
                     "OPENAI_API_KEY=abc"     ; added
                     "METTA_LLM_MODEL=gpt-4o" ; added
                     "swipl"
                     "-g" "use_module(library(lsp_server_metta))."
                     "-g" "lsp_server_metta:main"
                     "-t" "halt"
                     "--"
                     "port" :autoport)))
```

If you want to load directly from the source:

```emacs-lisp
;; Replace this with the path to your metta-wam directory
(let ((mettalog-dir "/path/to/metta-wam"))
   (add-to-list 'eglot-server-programs
                (cons 'metta-mode
                      (list
                       "env"
                       "OPENAI_API_KEY=abc"      ; added
                       "METTA_LLM_MODEL=gpt-4o"  ; added
                       (concat "METTALOG_DIR=" mettalog-dir)
                       (concat "SWIPL_PACK_PATH=" mettalog-dir "/libraries")
                       "swipl"
                       "-l" (concat mettalog-dir "/libraries/lsp_server_metta/prolog/lsp_server_metta.pl")
                       "-g" "lsp_server_metta:main"
                       "-t" "halt"
                       "--"
                       "port" :autoport))))
```

##### lsp-mode

If you've installed the `lsp_server_metta` pack:

```emacs-lisp
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-tcp-connection (lambda (port)
                                        (list
                                         "env"                    ; added
                                         "OPENAI_API_KEY=abc"     ; added
                                         "METTA_LLM_MODEL=gpt-4o" ; added
                                         "swipl"
                                         "-g" "use_module(library(lsp_server_metta))."
                                         "-g" "lsp_server_metta:main"
                                         "-t" "halt"
                                         "--"
                                         "port" port)))
  :major-modes '(metta-mode)
  :activation-fn (lsp-activate-on "metta")
  :server-id 'metta-lsp))
```

If you want to load directly from the source:

```emacs-lisp
;; Replace this with the path to your metta-wam directory
(let ((mettalog-dir "/path/to/metta-wam"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tcp-connection (lambda (port)
                                          (list
                                           "swipl"
                                           "-l" (concat mettalog-dir "/libraries/lsp_server_metta/prolog/lsp_server_metta.pl")
                                           "-g" "lsp_server_metta:main"
                                           "-t" "halt"
                                           "--"
                                           "port" port)))
     :environment-fn (lambda ()
                       (list ("METTALOG_DIR" . mettalog-dir)
                             ("OPENAI_API_KEY" . "abc") ; added
                             ("METTA_LLM_MODEL" . "gpt-4o") ; added
                             ("SWIPL_PACK_PATH". (concat mettalog-dir "/libraries"))))
    :major-modes '(metta-mode)
    :activation-fn (lsp-activate-on "metta")
    :server-id 'metta-lsp)))

```

#### Neovim

If you've installed the `lsp_server_metta` pack:

```lua
vim.api.nvim_create_autocmd(
   'FileType', {
      pattern = 'metta',
      callback = function(ev)
         vim.lsp.start({
               name = 'metta_lsp',
               cmd = {'swipl',
                     '-g', 'use_module(library(lsp_server_metta)).',
                     '-g', 'lsp_server_metta:main',
                     '-t', 'halt',
                     '--', 'stdio'},
               root_dir = vim.fs.root(ev.buf, {'README.md'}),
               -- begin added
               cmd_env = { OPENAI_API_KEY = 'abc';
                           METTA_LLM_MODEL = 'gpt-4o'; },
               -- end added
         })
      end,
})
```

To run from the `metta_wam` directory

```lua
vim.api.nvim_create_autocmd(
   'FileType', {
      pattern = 'metta',
      callback = function(ev)
         local mettalogDir = '/path/to/metta-wam'
         vim.lsp.start({
               name = 'metta_lsp',
               cmd = { 'swipl',
                       '-l', mettalogDir .. '/libraries/lsp_server_metta/prolog/lsp_server_metta.pl',
                       '-g', 'lsp_server_metta:main',
                       '-t', 'halt',
                       '--', 'stdio' },
               cmd_cwd = mettalogDir,
               cmd_env = { METTALOG_DIR = mettalogDir;
                           OPENAI_API_KEY = 'abc'; -- added
                           METTA_LLM_MODEL = 'gpt-4o'; -- added
                           SWIPL_PACK_PATH = mettalogDir .. '/libraries'; },
               root_dir = vim.fs.root(ev.buf, {'README.md'}),
         })
      end,
})
```
