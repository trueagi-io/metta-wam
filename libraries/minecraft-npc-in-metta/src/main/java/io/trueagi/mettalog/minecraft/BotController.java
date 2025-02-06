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
import org.geysermc.mcprotocollib.protocol.packet.ingame.serverbound.ServerboundMovePlayerPacket;
import org.jpl7.Query;
import org.jpl7.Term;

import java.net.InetSocketAddress;
import java.time.Instant;
import java.util.BitSet;
import java.util.UUID;

public class BotController {

    private static final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(BotController.class);

    static String DEFAULT_USERNAME = "MeTTaNPC-1";
    static String DEFAULT_PASSWORD = ""; // Empty for offline mode
    static String DEFAULT_SERVER = "127.0.0.1";
    static int DEFAULT_PORT = 25565;

    public static ProxyInfo PROXY = null;
    public static ProxyInfo AUTH_PROXY = null;

    private ClientSession client;
    private String username;
    private String password;
    private InetSocketAddress serverAddress;
    private double x = 0, y = 64, z = 0; // Bot's initial position

    public BotController() {
        log.info("BotController initialized. Waiting for login command...");
        new Thread(this::executeQueuedCommands).start();
    }

    public BotController(String username, String password, String server, int port) {
        this();
        login(username, password, server, port);
    }

    public void login(String username, String password, String server, int port) {        
        this.username = username != null ? username : DEFAULT_USERNAME;
        this.password = password != null ? password : DEFAULT_PASSWORD;
        this.serverAddress = new InetSocketAddress(server, port);
        log.info("Attempting to log in as {}", this.username);
        MinecraftProtocol protocol;
        if (password == null || password.isEmpty()) {
            protocol = new MinecraftProtocol(new GameProfile(UUID.randomUUID(), username), null);
        } else {
            protocol = new MinecraftProtocol(new GameProfile(UUID.randomUUID(), username), password);
        }

        SessionService sessionService = new SessionService();
        sessionService.setProxy(AUTH_PROXY);

        client = ClientNetworkSessionFactory.factory()
                .setRemoteSocketAddress(new InetSocketAddress(server, port))
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
					String plainTextContent = convertComponentToString(systemChatPacket.getContent());
					log.info("Received Chat: {}", plainTextContent); 
                    invokeProlog("on_chat_message", plainTextContent);
                }
            }

            @Override
            public void disconnected(DisconnectedEvent event) {
				String plainTextReason = convertComponentToString(event.getReason());
				Throwable cause = event.getCause();
                log.info("Disconnected: {}", plainTextReason, cause);
                invokeProlog("on_bot_disconnected", plainTextReason);
                if (!plainTextReason.contains("logged out")) {
                    log.warn("Unexpected disconnect! Attempting to respawn...");
                    invokeProlog("on_bot_killed", "disconnection");
                }
            }
        });

        client.connect(true);
    }

    /** Converts Adventure Component to a String */
    static String convertComponentToString(Component component) {
        return PlainTextComponentSerializer.plainText().serialize(component);
    }
    
    public void executeQueuedCommands() {
        while (true) {
            Query query = new Query("dequeue_command(Command).");
            if (query.hasSolution()) {
                Term command = query.oneSolution().get("Command");
                executeCommand(command);
            }

            try {
                Thread.sleep(1000);
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
        log.info("Moving to: {} {} {}", x, y, z);

        if (client != null && client.isConnected()) {
            client.send(new ServerboundMovePlayerPacket.PosRot(true, x, y, z, 0f, 0f));
        } else {
            log.warn("Bot is not connected. Cannot move.");
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

    private void invokeProlog(String predicate, String... args) {
        StringBuilder queryStr = new StringBuilder(predicate + "(");
        for (int i = 0; i < args.length; i++) {
            queryStr.append("'").append(args[i].replace("'", "\\'")).append("'");
            if (i < args.length - 1) queryStr.append(", ");
        }
        queryStr.append(").");

        Query q = new Query(queryStr.toString());
        if (q.hasSolution()) {
            log.info("Prolog executed: {}", queryStr);
        } else {
            log.error("Prolog execution failed: {}", queryStr);
        }
    }


    public static void main(String[] args) {
        log.info("Starting Metta-Minecraft bot. Waiting for login command...");
        new BotController();
    }
}

