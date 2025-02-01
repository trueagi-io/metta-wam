const vscode = require('vscode');
const net = require('net');
const {
  LanguageClient,
  RevealOutputChannelOn,
  Trace
} = require('vscode-languageclient');

let client;

function activate(context) {
  // 1) Main channel for all client logs (unified with trace)
  const outputChannel = vscode.window.createOutputChannel("MeTTa Language Client");
  outputChannel.show(true);

  // 2) Separate channel for server-sent messages
  const serverMessageChannel = vscode.window.createOutputChannel("MeTTa LSP Messages");
  // serverMessageChannel.show(false);

  // Log current configuration in the main channel
  showMettaLSPSettings(outputChannel);

  // Read user settings
  const config = vscode.workspace.getConfiguration("metta-lsp");
  const mode = config.get("server.mode", "stdio");
  const spawnProcess = config.get("server.spawnProcess", true);
  const port = config.get("server.port", 40222);
  const address = config.get("server.address", "127.0.0.1");

  // Define server options for stdio
  const serverOptions_stdio = {
    run: {
      command: "swipl",
      args: [
        "-g", "use_module(library(metta_lsp)).",
        "-g", "lsp_server_metta:main",
        "-t", "halt",
        "--", "stdio"
      ]
    },
    debug: {
      command: "swipl",
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

  // Define server options for port-based, spawned process
  const serverOptions_portSpawn = {
    run: {
      command: "swipl",
      args: [
        "-g", "use_module(library(metta_lsp)).",
        "-g", "lsp_server_metta:main",
        "-t", "halt",
        "--", "port", port.toString()
      ]
    },
    debug: {
      command: "swipl",
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

  // Decide which approach to use
  let serverOptions;
  let clientOptions;

  if (mode === "stdio") {
    outputChannel.appendLine("Using 'stdio' mode.");
    serverOptions = serverOptions_stdio;
    clientOptions = standardClientOptions(outputChannel);
  } else if (mode === "port" && spawnProcess) {
    outputChannel.appendLine(`Using 'port' mode (spawnProcess=true). Port ${port}.`);
    serverOptions = serverOptions_portSpawn;
    clientOptions = withSocketStreamProvider(address, port, outputChannel);
  } else if (mode === "port" && !spawnProcess) {
    outputChannel.appendLine(
      `Using 'port' mode (spawnProcess=false). Connecting to ${address}:${port} with retry logic if needed.`
    );
    serverOptions = createServerOptions_ExternalPort(address, port, outputChannel);
    clientOptions = standardClientOptions(outputChannel);
  } else {
    outputChannel.appendLine(
      `Unrecognized config (mode='${mode}' spawnProcess='${spawnProcess}'). Falling back to stdio.`
    );
    serverOptions = serverOptions_stdio;
    clientOptions = standardClientOptions(outputChannel);
  }

  // Create and start the Language Client
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

    // Example of a custom notification: "metta-lsp/showMessage"
    // The server can send { text: "...some message..." }
    client.onNotification("metta-lsp/showMessage", (params) => {
      if (params && typeof params.text === 'string') {
        serverMessageChannel.appendLine(params.text);
        serverMessageChannel.show(false);
      }
    });

  }).catch(err => {
    outputChannel.appendLine(`MeTTa Language Client failed to start: ${err}`);
    outputChannel.show(false);
  });
}

function deactivate() {
  if (client) {
    return client.stop();
  }
}

// Reusable client options that unify logs in "MeTTa Language Client"
function standardClientOptions(outputChannel) {
  return {
    documentSelector: [
      { scheme: "file", language: "metta" },
      { scheme: "file", language: "prolog" }
    ],
    outputChannel: outputChannel,
    traceOutputChannel: outputChannel,
    trace: Trace.Verbose,
    revealOutputChannelOn: RevealOutputChannelOn.Info
  };
}

// If we spawn a local server that ONLY communicates on a port, define a streamProvider
function withSocketStreamProvider(address, port, outputChannel) {
  return {
    documentSelector: [
      { scheme: "file", language: "metta" },
      { scheme: "file", language: "prolog" }
    ],
    outputChannel: outputChannel,
    traceOutputChannel: outputChannel,
    trace: Trace.Verbose,
    revealOutputChannelOn: RevealOutputChannelOn.Info,
    streamProvider: () => connectSocketWithRetry(address, port, outputChannel)
  };
}

// If connecting to an external server, define serverOptions as a function returning a Promise<StreamInfo>
function createServerOptions_ExternalPort(address, port, outputChannel) {
  const serverOptionsFunction = () => connectSocketWithRetry(address, port, outputChannel);
  return {
    run: serverOptionsFunction,
    debug: serverOptionsFunction
  };
}

// Attempts to connect, retries every 10s if the server is not up or closes
function connectSocketWithRetry(address, port, outputChannel) {
  return new Promise((resolve, reject) => {
    let socket;
    let resolved = false;

    function tryConnect() {
      outputChannel.appendLine(`Trying to connect to ${address}:${port}...`);
      socket = net.connect({ host: address, port }, () => {
        outputChannel.appendLine(`Connected to server at ${address}:${port}.`);
        if (!resolved) {
          resolved = true;
          resolve({ reader: socket, writer: socket });
        } else {
          outputChannel.appendLine(`Reconnected to server at ${address}:${port}.`);
        }
      });

      socket.on("data", (chunk) => {
        outputChannel.appendLine("Server -> Client:\n" + chunk.toString());
      });

      socket.on("error", (err) => {
        outputChannel.appendLine(`Socket error: ${err.message}`);
        socket.destroy();
        if (!resolved) {
          scheduleRetry();
        }
      });

      socket.on("close", () => {
        outputChannel.appendLine(`Socket closed. Will attempt reconnect to ${address}:${port}.`);
        socket.destroy();
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

