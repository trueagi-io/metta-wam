package io.trueagi.mettalog.minecraft;

import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.serializer.plain.PlainTextComponentSerializer;
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
import org.jpl7.Query;

import java.net.InetSocketAddress;
import java.time.Instant;
import java.util.BitSet;

public class BotController {

    private static final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(BotController.class);

    private static final ProxyInfo PROXY = null;
    private static final ProxyInfo AUTH_PROXY = null;
    private static final InetSocketAddress ADDRESS = new InetSocketAddress("127.0.0.1", 25565);

    private ClientSession client;

    public BotController(String username, String server, int port) {
        MinecraftProtocol protocol = new MinecraftProtocol(username);
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
					log.info("Bot logged in as {}", username);
				} else if (packet instanceof ClientboundSystemChatPacket systemChatPacket) {
					String plainTextContent = convertToString(systemChatPacket.getContent());
					log.info("Received Chat: {}", plainTextContent); 
					invokeProlog("on_chat_message", new String[]{plainTextContent}); // FIXED
				}
			}

            @Override
            public void disconnected(DisconnectedEvent event) {
				String plainTextReason = convertToString(event.getReason());
				Throwable cause = event.getCause();
                log.info("Disconnected: {}", plainTextReason, cause);
                invokeProlog("on_bot_disconnected", plainTextReason);
            }
        });

        client.connect(true);
    }

    /** Converts Adventure Component to a String */
    static String convertToString(Component component) {
        return PlainTextComponentSerializer.plainText().serialize(component);
    }
    
    static String convertToString(String str) {
        return str;
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
        log.info("Starting PrologBot...");
        BotController bot = new BotController("PrologBot", "localhost", 25565);

        Query query = new Query("consult('src/main/prolog/prolog_bot.pl')");
        log.info("Prolog loaded: {}", query.hasSolution());

        // Shutdown hook to disconnect gracefully
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            log.info("Shutting down...");
        }));

        // Keep bot running
        try {
            while (true) {
                Thread.sleep(1000);
            }
        } catch (InterruptedException e) {
            log.error("Bot interrupted", e);
            Thread.currentThread().interrupt();
        }
    }
}

