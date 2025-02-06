package io.trueagi.mettalog.minecraft;

import net.kyori.adventure.text.Component;
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

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.BitSet;

public class BotExample {

    private static final org.slf4j.Logger log = org.slf4j.LoggerFactory.getLogger(BotExample.class);


    private static final ProxyInfo PROXY = null;
    private static final ProxyInfo AUTH_PROXY = null;

    private static final InetSocketAddress ADDRESS = new InetSocketAddress("127.0.0.1", 25565);

    public static void main(String[] args) {
        status();
        login();
    }

    private static void status() {
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

    private static void login() {
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


