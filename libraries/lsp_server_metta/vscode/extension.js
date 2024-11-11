const vscode = require('vscode');
const lsp = require('vscode-languageclient');

function activate(context) {

  let serverOptions = {
    run: {command: "swipl",
          args: ["-g", "use_module(library(lsp_server_metta)).",
                 "-g", "lsp_server_metta:main",
                 "-t", "halt",
                 "--", "stdio"]},
    debug: {command: "swipl",
            args: ["-g", "use_module(library(syslog)).",
                   "-g", "openlog(metta_lsp, [], user).",
                   "-g", "use_module(library(debug)).",
                   "-g", "debug(server(high)).",
                   "-g", "use_module(library(lsp_server_metta)).",
                   "-g", "lsp_server_metta:main",
                   "-t", "halt",
                   "--", "stdio"]}
  };

  let clientOptions = {
    documentSelector: [{scheme: "file", language: "metta"},{scheme: "file", language: "prolog"}],
  };

  let client = new lsp.LanguageClient(
    'metta-lsp',
    'MeTTa Language Client',
    serverOptions,
    clientOptions
  );

  context.subscriptions.push(client.start());
}
exports.activate = activate;

function deactivate(context) {
  if (!context.client) { return; }
  context.client.stop();
}
exports.deactivate = deactivate;
