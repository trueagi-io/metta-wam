package io.trueagi.mettalog;

import org.jpl7.Query;
import org.jline.reader.*;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class MeTTaLogConsole {
    private static final Logger log = LoggerFactory.getLogger(MeTTaLogConsole.class);
    private static String prompt = "prolog> ";
    private static String currentMode = "prolog";
    private static final LinkedList<String> tempHelpfulCommands = new LinkedList<>();
    private static final LinkedList<String> permHelpfulCommands = new LinkedList<>();
    private static boolean historyModified = false;

    public static void main(String[] args) {
        try {
            initialize();
            startListeningForCommands();
        } catch (IOException e) {
            log.error("An error occurred during execution: ", e);
            System.out.println("An unexpected error occurred. Please check the logs for details.");
        }
    }

    private static void initialize() {
        System.out.println("Enter MeTTaLog queries (type 'exit' to quit):");
    }

    private static boolean loadHelpfulCommands(String filePath) {
        if (!Files.exists(Paths.get(filePath))) {
            log.warn("Helpful commands file not found: {}", filePath);
            return false;
        }

        try {
            List<String> commands = Files.readAllLines(Paths.get(filePath));
            for (String command : commands) {
                command = command.trim();
                if (!command.isEmpty() && !command.startsWith("#")) {
                    tempHelpfulCommands.add(command);
                    permHelpfulCommands.add(command);
                }
            }
            log.info("Loaded helpful commands from: {}", filePath);
            return true;
        } catch (IOException e) {
            log.error("Error reading helpful commands file: {}", filePath, e);
            return false;
        }
    }

    private static void saveFilteredHistory(LineReader reader) {
        if (!historyModified) return;

        try {
            DefaultHistory newHistory = new DefaultHistory();
            List<String> userCommands = new ArrayList<>();

            for (History.Entry entry : reader.getHistory()) {
                String line = entry.line();
                if (!permHelpfulCommands.contains(line)) { 
                    userCommands.add(line);
                }
            }

            for (String line : userCommands) {
                newHistory.add(line);
            }

            newHistory.save();
            log.info("Filtered history saved successfully.");
            historyModified = false;
        } catch (Exception e) {
            log.error("Error while saving filtered history: ", e);
        }
    }

    private static void saveHelpfulCommandsToHistory(LineReader reader, String historyFilePath) {
        try {
            List<String> historyEntries = new ArrayList<>(Files.readAllLines(Paths.get(historyFilePath)));

            // Add helpful commands to history
            for (String command : tempHelpfulCommands) {
                if (!historyEntries.contains(command)) {
                    historyEntries.add(command);
                }
            }

            // Save the updated history to disk
            Files.write(Paths.get(historyFilePath), historyEntries);

        } catch (IOException e) {
            log.error("Error saving helpful commands to history: ", e);
        }
    }

    private static void processCommand(String command) {
        if (command.startsWith("@")) {
            switch (command.toLowerCase()) {
                case "@h":
                    showHelp();
                    break;
                case "@m":
                    setMode("mettalog", "mettalog> ");
                    break;
                case "@p":
                    setMode("prolog", "prolog> ");
                    break;
                default:
                    System.out.println("Unknown command: " + command);
            }
        } else {
            evaluateString(command);
        }
    }

    private static void setMode(String mode, String newPrompt) {
        currentMode = mode;
        prompt = newPrompt;
    }

    private static void showHelp() {
        System.out.println("Available commands:");
        System.out.println("@h - Show this help message");
        System.out.println("@m - Switch to MeTTaLog mode");
        System.out.println("@p - Switch to Prolog mode");
        System.out.println("exit - Exit the console");
    }

    private static void evaluateString(String query) {
        switch (currentMode) {
            case "prolog":
                jplQuery(query);
                break;
            case "mettalog":
                mettaLogQuery(query);
                break;
            default:
                System.out.println("Unknown mode. Please select a mode using @m or @p.");
        }
    }

    private static void jplQuery(String queryStr) {
        log.info("Executing Prolog query: {}", queryStr);
        try {
            Query query = new Query(queryStr);
            if (query.hasSolution()) {
                log.info("Query successful: {}", queryStr);
            } else {
                log.info("Query failed: {}", queryStr);
            }
        } catch (Exception e) {
            log.error("Error executing Prolog query: {}", queryStr, e);
            System.out.println("An error occurred while processing the Prolog query. Check logs for details.");
        }
    }

    private static void mettaLogQuery(String queryStr) {
        System.out.println("Executing MeTTaLog query: " + queryStr);
    }

















	private static void startListeningForCommands() throws IOException {
    Terminal terminal = TerminalBuilder.builder().system(true).build();
    String historyFilePath = System.getProperty("user.home") + "/.mettalog_history";
    String commandsFile = System.getProperty("user.home") + "/helpful_commands.txt";

    // If history file exists but is corrupted, delete it
    if (Files.exists(Paths.get(historyFilePath))) {
        try {
            List<String> lines = Files.readAllLines(Paths.get(historyFilePath));
            for (String line : lines) {
                if (line.contains("\0") || line.contains("\u0000")) { // Detect invalid characters
                    log.warn("Corrupt history file detected! Deleting and recreating...");
                    Files.delete(Paths.get(historyFilePath));
                    break;
                }
            }
        } catch (IOException e) {
            log.error("Failed to check history file integrity: ", e);
        }
    }

    DefaultHistory history = new DefaultHistory();
    LineReader reader = LineReaderBuilder.builder()
        .terminal(terminal)
        .history(history)
        .variable(LineReader.HISTORY_FILE, historyFilePath)
        .build();

    try {
        history.load();
    } catch (Exception e) {
        log.error("History file is corrupted. Resetting...");
        Files.deleteIfExists(Paths.get(historyFilePath));
        history = new DefaultHistory(); // Use fresh history
    }

    // Load helpful commands into memory
    if (!loadHelpfulCommands(commandsFile)) {
        loadHelpfulCommands("helpful_commands.txt");
    }

    // **Inject helpful commands into JLine history buffer**
    int helpfulIndex = 0;
    List<String> injectedHelpfulCommands = new ArrayList<>(tempHelpfulCommands);

    historyModified = true;

    while (true) {
        String input;
        try {
            // **Inject helpful commands first, then user history**
            if (helpfulIndex < injectedHelpfulCommands.size()) {
                input = injectedHelpfulCommands.get(helpfulIndex++);
                history.add(input); // Store in JLine history buffer
                System.out.println(prompt + input); // Show it
            } else {
                input = reader.readLine(prompt);
            }

        } catch (UserInterruptException | EndOfFileException e) {
            System.out.println("\nExiting...");
            break;
        }

        if (input == null || "exit".equalsIgnoreCase(input.trim())) {
            System.out.println("\nExiting...");
            break;
        }

        if (!input.isEmpty()) {
            processCommand(input);
            history.add(input); // Store user input in history buffer
            historyModified = true;
        }
    }
}

}
