// extension.js
const vscode = require('vscode');
const net = require('net');

// IMPORTANT: Import from 'vscode-languageclient/node' instead of 'vscode-languageclient'.
const {
  LanguageClient,
  RevealOutputChannelOn,
  Trace,
  StreamMessageReader,
  StreamMessageWriter
} = require('vscode-languageclient/node');

let client;

function activate(context) {
  // 1) Main channel for all client logs
  const outputChannel = vscode.window.createOutputChannel("MeTTa Language Client");
  outputChannel.show(true);

  // 2) Separate channel for server-sent messages
  const serverMessageChannel = vscode.window.createOutputChannel("MeTTa LSP Messages");

  // Show the current user settings in the output channel
  showMettaLSPSettings(outputChannel);

  // Read user settings
  const config = vscode.workspace.getConfiguration("metta-lsp");
  const mode = config.get("server.mode", "stdio");
  const spawnProcess = config.get("server.spawnProcess", true);
  const port = config.get("server.port", 40222);
  const address = config.get("server.address", "127.0.0.1");
  const swiplPath = config.get("server.swiplPath", "swipl");

  // Define server options for stdio
  const serverOptions_stdio = {
    run: {
      command: swiplPath,
      args: [
        "-g", "use_module(library(metta_lsp)).",
        "-g", "lsp_server_metta:main",
        "-t", "halt",
        "--", "stdio"
      ]
    },
    debug: {
      command: swiplPath,
      args: [
        "-g", "use_module(library(syslog)).",
        "-g", "openlog(metta_lsp, [], user).",
        "-g", "use_module(library(debug)).",
        "-g", "debug(server(high)).",
        "-g", "use_module(library(metta_lsp)).",
        "-g", "lsp_server_metta:main",
        "-t", "halt",
        "--", "stdio"
      ]
    }
  };

  // Define server options for port-based with spawning
  const serverOptions_portSpawn = {
    run: {
      command: swiplPath,
      args: [
        "-g", "use_module(library(metta_lsp)).",
        "-g", "lsp_server_metta:main",
        "-t", "halt",
        "--", "port", port.toString()
      ]
    },
    debug: {
      command: swiplPath,
      args: [
        "-g", "use_module(library(syslog)).",
        "-g", "openlog(metta_lsp, [], user).",
        "-g", "use_module(library(debug)).",
        "-g", "debug(server(high)).",
        "-g", "use_module(library(metta_lsp)).",
        "-g", "lsp_server_metta:main",
        "-t", "halt",
        "--", "port", port.toString()
      ]
    }
  };

  // Decide serverOptions + clientOptions based on mode
  let serverOptions;
  let clientOptions;

  if (mode === "stdio") {
    outputChannel.appendLine("Using 'stdio' mode.");
    serverOptions = serverOptions_stdio;
    clientOptions = standardClientOptions(outputChannel);
  } else if (mode === "port" && spawnProcess) {
    outputChannel.appendLine(`Using 'port' mode (spawnProcess=true). Port ${port}.`);
    // We'll spawn the SWI-Prolog process that listens on the port,
    // then we also need a custom streamProvider to connect to it.
    serverOptions = serverOptions_portSpawn;
    clientOptions = withSocketStreamProvider(address, port, outputChannel);
  } else if (mode === "port" && !spawnProcess) {
    outputChannel.appendLine(
      `Using 'port' mode (spawnProcess=false). Connecting to ${address}:${port} with retry logic.`
    );
    // We do not spawn the process, so we rely on an externally running server.
    // We'll create serverOptions that returns a StreamInfo from a net socket.
    serverOptions = () => createServerOptions_ExternalPort(address, port, outputChannel);
    clientOptions = standardClientOptions(outputChannel);
  } else {
    outputChannel.appendLine(
      `Unrecognized config (mode='${mode}', spawnProcess='${spawnProcess}'). Falling back to stdio.`
    );
    serverOptions = serverOptions_stdio;
    clientOptions = standardClientOptions(outputChannel);
  }

  outputChannel.appendLine("Starting MeTTa Language Client...");
  client = new LanguageClient(
    "metta-lsp",
    "MeTTa Language Client",
    serverOptions,
    clientOptions
  );

  const disposable = client.start();
  context.subscriptions.push(disposable);

  // Once the client is ready, set up a custom notification listener
  client.onReady().then(() => {
    outputChannel.appendLine("MeTTa Language Client is ready!");
    outputChannel.show(false);

    // Example: The server can send a custom notification "metta-lsp/showMessage"
    client.onNotification("metta-lsp/showMessage", (params) => {
      if (params && typeof params.text === "string") {
        serverMessageChannel.appendLine(params.text);
        // Decide whether to pop up automatically or not
        // serverMessageChannel.show(true);
      }
    });

  }).catch(err => {
    outputChannel.appendLine(`MeTTa Language Client failed to start: ${err}`);
    outputChannel.show(true);
  });
}

function deactivate() {
  if (client) {
    return client.stop();
  }
}

// -----------------------------------------------------------------------------
// Standard client options (stdio or external server mode).
function standardClientOptions(outputChannel) {
  return {
    documentSelector: [
      { scheme: "file", language: "metta" },
      { scheme: "file", language: "prolog" }
    ],
    outputChannel,
    traceOutputChannel: outputChannel,
    trace: Trace.Verbose,
    revealOutputChannelOn: RevealOutputChannelOn.Info
  };
}

// -----------------------------------------------------------------------------
// If we spawn a local server that ONLY communicates on a port,
// define clientOptions that attach a custom streamProvider
function withSocketStreamProvider(address, port, outputChannel) {
  return {
    documentSelector: [
      { scheme: "file", language: "metta" },
      { scheme: "file", language: "prolog" }
    ],
    outputChannel,
    traceOutputChannel: outputChannel,
    trace: Trace.Verbose,
    revealOutputChannelOn: RevealOutputChannelOn.Info,
    streamProvider: () => connectSocketWithRetry(address, port, outputChannel)
  };
}

// -----------------------------------------------------------------------------
// If connecting to an EXTERNAL server (spawned separately),
// define a serverOptions function returning a Promise<StreamInfo>
function createServerOptions_ExternalPort(address, port, outputChannel) {
  return connectSocketWithRetry(address, port, outputChannel);
}

// -----------------------------------------------------------------------------
// Creates a net.Socket with retry logic. Returns StreamInfo suitable for LSP.
//
// - If the connection fails or closes, it waits 10s, then tries again
// - Once resolved the first time, the LSP client is considered "started".
//   If it closes later, we schedule a reconnect attempt, though the LSP client
//   may or may not re-initialize fully. (Behavior can vary.)
function connectSocketWithRetry(address, port, outputChannel) {
  return new Promise((resolve, reject) => {
    let socket;
    let resolved = false;

    function tryConnect() {
      outputChannel.appendLine(`Trying to connect to ${address}:${port}...`);

      socket = net.connect({ host: address, port }, () => {
        outputChannel.appendLine(`Connected to server at ${address}:${port}.`);

        // Create LSP-compatible StreamReaders/Writers
        const reader = new StreamMessageReader(socket);
        const writer = new StreamMessageWriter(socket);

        if (!resolved) {
          resolved = true;
          resolve({ reader, writer });
        } else {
          outputChannel.appendLine(`Reconnected to server at ${address}:${port}.`);
        }
      });

      // If socket errors out before connecting
      socket.on("error", (err) => {
        outputChannel.appendLine(`Socket error: ${err.message}`);
        socket.destroy();
        if (!resolved) {
          scheduleRetry();
        }
      });

      // If the server closes or drops the connection
      socket.on("close", () => {
        outputChannel.appendLine(`Socket closed. Attempting reconnect to ${address}:${port}...`);
        socket.destroy();
        // Even if we previously resolved, we can attempt to reconnect.
        // The LSP client may or may not handle re-initialization gracefully, but we try.
        scheduleRetry();
      });
    }

    function scheduleRetry() {
      outputChannel.appendLine("Will retry in 10 seconds...");
      setTimeout(tryConnect, 10000);
    }

    tryConnect();
  });
}

// -----------------------------------------------------------------------------
// Logs out the current MeTTa LSP config properties at startup
function showMettaLSPSettings(outputChannel) {
  const config = vscode.workspace.getConfiguration("metta-lsp");
  const allKeys = [
    "maxNumberOfProblems",
    "trace.server",
    "features",
    "debug.showIncompleteFeatures",
    "options",
    "xtras.chatgpt.enabled",
    "xtras.chatgpt.apiKey",
    "xtras.chatgpt.model",
    "xtras.chatgpt.maxTokens",
    "xtras.chatgpt.temperature",
    "server.mode",
    "server.spawnProcess",
    "server.port",
    "server.address"
  ];

  outputChannel.appendLine("-------------------------------------------");
  outputChannel.appendLine("Current MeTTa LSP Configuration:");
  allKeys.forEach(key => {
    const value = config.get(key);
    outputChannel.appendLine(`  metta-lsp.${key} = ${JSON.stringify(value)}`);
  });
  outputChannel.appendLine("-------------------------------------------");
}

module.exports = {
  activate,
  deactivate
};

