package io.trueagi.mettalog.minecraft;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
import org.geysermc.mcprotocollib.auth.GameProfile;
import org.geysermc.mcprotocollib.auth.SessionService;
import org.geysermc.mcprotocollib.network.ClientSession;
import org.geysermc.mcprotocollib.network.ProxyInfo;
import org.geysermc.mcprotocollib.network.Session;
import org.geysermc.mcprotocollib.network.event.session.DisconnectedEvent;
import org.geysermc.mcprotocollib.network.event.session.SessionAdapter;
import org.geysermc.mcprotocollib.network.factory.ClientNetworkSessionFactory;
import org.geysermc.mcprotocollib.network.packet.Packet;
import org.geysermc.mcprotocollib.protocol.MinecraftConstants;
import org.geysermc.mcprotocollib.protocol.MinecraftProtocol;
import org.geysermc.mcprotocollib.protocol.packet.ingame.clientbound.ClientboundLoginPacket;
import org.geysermc.mcprotocollib.protocol.packet.ingame.clientbound.ClientboundSystemChatPacket;
import org.geysermc.mcprotocollib.protocol.packet.ingame.serverbound.ServerboundChatPacket;
import org.geysermc.mcprotocollib.protocol.packet.ingame.serverbound.player.ServerboundMovePlayerPosPacket;
import org.geysermc.mcprotocollib.protocol.packet.ingame.serverbound.player.ServerboundMovePlayerPosRotPacket;

import org.jpl7.Atom;
import org.jpl7.Compound;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.JRef;


import java.io.File;
import java.nio.charset.StandardCharsets;
import java.net.InetSocketAddress;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.BitSet;
import java.util.UUID;
import java.util.Scanner;

public class BotController {

    private static final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(BotController.class);

    static String DEFAULT_USERNAME = "MeTTaNPC1";
    static String DEFAULT_PASSWORD = ""; // Empty for offline mode
    static String DEFAULT_SERVER = "127.0.0.1";
    static int DEFAULT_PORT = 25565;
	static InetSocketAddress ADDRESS = new InetSocketAddress(DEFAULT_SERVER, DEFAULT_PORT);

	public boolean needsLogin = true;

    public static ProxyInfo PROXY = null;
    public static ProxyInfo AUTH_PROXY = null;

    public ClientSession client;
    private String username;
    private String password;
    private InetSocketAddress serverAddress;
    private double x = 0, y = 64, z = 0; // Bot's position
    private float yaw = 0f, pitch = 0f; // Bot's rotation

    public volatile boolean shouldProcessQueue = false; // Flag to control queue processing
    public int queueSleepTimeMs = 100; // Default sleep time: 100ms (1/10th of a second)

    public BotController() {
        log.info("BotController initialized. Waiting for login command...");

        // Load Prolog files dynamically
        loadPrologFiles();

        
        // Assert a Prolog predicate with the BotController object
        invokeProlog("assert", makeTerm("bot_controller", this));

        if (new File(prologPathRunner).exists()) {
            new Query("consult('" + prologPathRunner.replace("\\", "\\\\") + "')").hasSolution();
        } else {
            log.error("Prolog file missing: {}", prologPathRunner);
        }

    }

    public BotController(String username, String password, String server, int port) {
        this();
        login(username, password, server, port);
    }

    private String prologPathDriver;
    private String prologPathRunner;
    
    /**
     * Dynamically finds the Prolog files based on the class location and loads them.
     */
    private void loadPrologFiles() {
        try {
            // Get the directory containing this class
            Path basePath = Paths.get(new File(BotController.class.getProtectionDomain().getCodeSource().getLocation().toURI()).getParentFile().getParent());

            // Construct paths to the Prolog files
            prologPathDriver = basePath.resolve("prolog/minecraft_bot_driver.pl").toString();
            prologPathRunner = basePath.resolve("prolog/minecraft_bot_hello.pl").toString();

            log.info("Loading Prolog files: \n  - {}\n  - {}", prologPathDriver, prologPathRunner);

            // Load Prolog files
            if (new File(prologPathDriver).exists()) {
                new Query("consult('" + prologPathDriver.replace("\\", "\\\\") + "')").hasSolution();
            } else {
                log.error("Prolog file missing: {}", prologPathDriver);
            }

            log.info("Prolog files successfully loaded.");

        } catch (URISyntaxException e) {
            log.error("Error locating Prolog files", e);
        }
    }
    
       public void startQueueProcessing() {
        if (!shouldProcessQueue) {
            shouldProcessQueue = true;
            new Thread(this::executeQueuedCommands).start();
            log.info("Queue processing started.");
        }
    }

     public void stopQueueProcessing() {
        shouldProcessQueue = false;
        log.info("Queue processing stopped.");
    }


    public void login(String username, String password, String server, int port) { 
		if (!needsLogin)  return;
		needsLogin = false;
		this.username = username != null ? username : DEFAULT_USERNAME;
        this.password = password != null ? password : DEFAULT_PASSWORD;
        this.serverAddress = new InetSocketAddress(server, port);
        
        // Set up client with login details
        setupClient();
        
        // Connect the client
        connectClient();
    }

    public void login0() {       
		if (!needsLogin)  return; 
		needsLogin = false;
        this.username = username != null ? username : DEFAULT_USERNAME;
        this.password = password != null ? password : DEFAULT_PASSWORD;
        this.serverAddress = new InetSocketAddress(DEFAULT_SERVER, DEFAULT_PORT);
        
        // Set up client with login details
        setupClient();
        
        // Connect the client
        connectClient();
    }

    /**
     * Initializes the client with login details and assigns event listeners.
     */
    private void setupClient() {
        MinecraftProtocol protocol = (password == null || password.isEmpty())
                ? new MinecraftProtocol(new GameProfile(UUID.randomUUID(), username), null)
                : new MinecraftProtocol(new GameProfile(UUID.randomUUID(), username), password);
    
        SessionService sessionService = new SessionService();
        sessionService.setProxy(AUTH_PROXY);

        client = ClientNetworkSessionFactory.factory()
                .setRemoteSocketAddress(serverAddress)
                .setProtocol(protocol)
                .setProxy(PROXY)
                .create();

        client.setFlag(MinecraftConstants.SESSION_SERVICE_KEY, sessionService);
        client.addListener(new SessionAdapter() {
            @Override
            public void packetReceived(Session session, Packet packet) {
                if (packet instanceof ClientboundLoginPacket) {
                    log.info("Bot successfully logged in as {}", username);
                    invokeProlog("on_bot_connected");
                } else if (packet instanceof ClientboundSystemChatPacket systemChatPacket) {
					if(false) {
						String plainTextContent = convertComponentToString(systemChatPacket.getContent());
						log.info("Received Chat: {}", plainTextContent); 
						invokeProlog("on_chat_message", plainTextContent);
					}
                }
            }

            @Override
            public void disconnected(DisconnectedEvent event) {
				String plainTextReason = convertComponentToString(event.getReason());
				Throwable cause = event.getCause();
                log.info("Disconnected: {} - Cause: {}", plainTextReason, cause);
                invokeProlog("on_bot_disconnected", plainTextReason);
                if (!plainTextReason.contains("logged out")) {
                    log.warn("Unexpected disconnect! Attempting to respawn...");
                    invokeProlog("on_bot_killed", "disconnection");
                }
            }
        });
    }

    /**
     * Connects the client after setup.
     */
    private void connectClient() {
        log.info("Attempting to log in as {}", this.username);
        client.connect(true);
    }

    /** Converts Adventure Component to a String */
    static String convertComponentToString(Component component) {
		if (true) {
			return ""+component;
		}
		return PlainTextComponentSerializer.plainText().serialize(component);
    }
    
    public void executeQueuedCommands() {
        while (shouldProcessQueue) {
            Query query = new Query("dequeue_command(Command).");
    
            if (query.hasSolution()) {
                // Safely retrieve the solution
                java.util.Map<String, Term> solution = query.nextSolution();
                if (solution != null && solution.containsKey("Command")) {
                    Term command = solution.get("Command");
                    executeCommand(command);
                } else {
                    log.warn("Prolog query returned null or missing 'Command'. Skipping execution.");
                }
            }
    
            try {
                Thread.sleep(queueSleepTimeMs);
            } catch (InterruptedException e) {
                log.error("Polling interrupted", e);
                Thread.currentThread().interrupt();
            }
        }
    }


    public void executeCommand(Term command) {
        try {
        if (command.hasFunctor("move", 3)) {
            int dx = Integer.parseInt(command.arg(1).toString());
            int dy = Integer.parseInt(command.arg(2).toString());
            int dz = Integer.parseInt(command.arg(3).toString());
            move(dx, dy, dz);
        } else if (command.hasFunctor("rotate", 2)) {
            float newYaw = Float.parseFloat(command.arg(1).toString());
            float newPitch = Float.parseFloat(command.arg(2).toString());
            rotate(newYaw, newPitch);
        } else if (command.hasFunctor("chat", 1)) {
            String message = command.arg(1).toString();
            sendMessage(message);
        } else if (command.hasFunctor("login", 4)) {
            String user = command.arg(1).toString();
            String pass = command.arg(2).toString();
            String server = command.arg(3).toString();
            int port = Integer.parseInt(command.arg(4).toString());
            login(user, pass, server, port);
        } else {
            log.warn("Unknown command received: {}", command);
        }
        } catch (Exception e) {
            log.error("Error executing command: {}", command, e);
        }
    }

    public void move(int dx, int dy, int dz) {
        x += dx;
        y += dy;
        z += dz;
        log.info("Moving to: x={} y={} z={}", x, y, z);

        if (client != null && client.isConnected()) {
            client.send(new ServerboundMovePlayerPosPacket(true, true, x, y, z));
        } else {
            log.warn("Bot is not connected. Cannot move.");
        }
    }

    public void rotate(float newYaw, float newPitch) {
        yaw = newYaw;
        pitch = newPitch;
        log.info("Rotating to yaw={} pitch={}", yaw, pitch);

        if (client != null && client.isConnected()) {
            client.send(new ServerboundMovePlayerPosRotPacket(true, true, x, y, z, yaw, pitch));
        } else {
            log.warn("Bot is not connected. Cannot rotate.");
        }
    }

    public void sendMessage(String message) {
        if (client != null && client.isConnected()) {
            client.send(new ServerboundChatPacket(message, Instant.now().toEpochMilli(), 0L, null, 0, new BitSet()));
            log.info("Sent message: {}", message);
        } else {
            log.warn("Bot is not connected. Cannot send message.");
        }
    }


    public void invokeProlog(String predicate, Object... args) {
        Query query;
    
        if (args.length == 0) {
            // No arguments: Use an Atom query
            query = new Query(new Atom(predicate));
        } else {
            // Arguments exist: Use Compound query
            Term[] terms = new Term[args.length];
            for (int i = 0; i < args.length; i++) {
                terms[i] = makeTerm(args[i]);
            }
            query = new Query(new Compound(predicate, terms));
        }
    
        log.info("Prolog execute: {}", query.toString());
    
        if (query.hasSolution()) {
            log.info("Prolog executed successfully: {}", query.toString());
        } else {
            log.error("Prolog execution failed: {}", query.toString());
        }
    }
        
    public Term makeTerm(Object obj) {
        if (obj instanceof Term) {
            return (Term) obj; // Already a JPL term, use as-is
        } else if (obj instanceof String) {
            return new Atom((String) obj); // Convert to Prolog Atom
        } else if (obj instanceof Number) {
            return Term.textToTerm(obj.toString()); // Convert to Prolog number
        } else {
            return new JRef(obj); // Pass as Java object reference
        }
    }

    public Term makeTerm(String predicate, Object... args) {
        if (args.length == 0) {
            return new Atom(predicate); // No arguments ? return Atom
        } else {
            Term[] terms = new Term[args.length];
            for (int i = 0; i < args.length; i++) {
                terms[i] = makeTerm(args[i]); // Convert each argument
            }
            return new Compound(predicate, terms); // Arguments exist ? return Compound
        }
    }


    public static void main(String[] args) {
        log.info("Starting Metta-Minecraft bot. Waiting for login command...");
        
        BotController bc = new BotController();
        // bc.invokeProlog("on_main", args); make this work
		bc.login0();
        bc.startQueueProcessing(); // Starts queue processing thread
    
        // Start reading input and querying Prolog
        bc.readInputAndQueryProlog();
    }
    
    public void readInputAndQueryProlog() {
        Scanner scanner = new Scanner(System.in);
        log.info("Enter Prolog queries (type 'exit' to quit):");
    
        jplQuery("listing(bot_controller/1).");
        while (true) {
            System.out.print("> "); // User prompt
            String input = scanner.nextLine().trim();
    
            if (input.equalsIgnoreCase("exit")) {
                log.info("Exiting input mode...");
                break;
            }
    
            if (!input.isEmpty()) {
                jplQuery(input);
            }
        }
    
        scanner.close();
    }
    
    /**
     * Executes a JPL Prolog query and logs the result.
     */
    private void jplQuery(String queryStr) {
        log.info("Executing Prolog query: {}", queryStr);
    
        Query query = new Query(queryStr);
        if (query.hasSolution()) {
            log.info("Query successful: {}", queryStr);
        } else {
            log.info("Query failed: {}", queryStr);
        }
    }


  public static void status() {
        SessionService sessionService = new SessionService();
        sessionService.setProxy(AUTH_PROXY);

        MinecraftProtocol protocol = new MinecraftProtocol();
        ClientSession client = ClientNetworkSessionFactory.factory()
                .setRemoteSocketAddress(ADDRESS)
                .setProtocol(protocol)
                .setProxy(PROXY)
                .create();
        client.setFlag(MinecraftConstants.SESSION_SERVICE_KEY, sessionService);
        client.setFlag(MinecraftConstants.SERVER_INFO_HANDLER_KEY, (session, info) -> {
            log.info("Version: {}, {}", info.getVersionInfo().getVersionName(), info.getVersionInfo().getProtocolVersion());
            log.info("Player Count: {} / {}", info.getPlayerInfo().getOnlinePlayers(), info.getPlayerInfo().getMaxPlayers());
            log.info("Players: {}", Arrays.toString(info.getPlayerInfo().getPlayers().toArray()));
            log.info("Description: {}", info.getDescription());
            log.info("Icon: {}", new String(Base64.getEncoder().encode(info.getIconPng()), StandardCharsets.UTF_8));
        });

        client.setFlag(MinecraftConstants.SERVER_PING_TIME_HANDLER_KEY, (session, pingTime) ->
                log.info("Server ping took {}ms", pingTime));

        client.connect(true);
        while (client.isConnected()) {
            try {
                Thread.sleep(5);
            } catch (InterruptedException e) {
                log.error("Interrupted while waiting for server to disconnect.", e);
            }
        }
    }


  public static void login_test() {
        MinecraftProtocol protocol = new MinecraftProtocol("tttt");

        SessionService sessionService = new SessionService();
        sessionService.setProxy(AUTH_PROXY);


        ClientSession client = ClientNetworkSessionFactory.factory()
                .setRemoteSocketAddress(ADDRESS)
                .setProtocol(protocol)
                .setProxy(PROXY)
                .create();
        client.setFlag(MinecraftConstants.SESSION_SERVICE_KEY, sessionService);
        client.addListener(new SessionAdapter() {
            @Override
            public void packetReceived(Session session, Packet packet) {
                if (packet instanceof ClientboundLoginPacket) {
                    session.send(new ServerboundChatPacket("Hello, this is a test of MCProtocolLib.", Instant.now().toEpochMilli(), 0L, null, 0, new BitSet()));
                } else if (packet instanceof ClientboundSystemChatPacket systemChatPacket) {
                    Component message = systemChatPacket.getContent();
                    log.info("Received Message: {}", message);
                    session.disconnect(Component.text("Finished"));
                }
            }

            @Override
            public void disconnected(DisconnectedEvent event) {
                log.info("Disconnected: {}", event.getReason(), event.getCause());
            }
        });

        client.connect(true);
    }

}



