# 🏰️ Metta-MCBot

A **Minecraft AI bot** powered by **MCProtocolLib, Prolog (JPL), and MeTTa**.

## 🚀 Features

✔ **Connects to a Minecraft server**\
✔ **Receives chat messages and processes them**\
✔ **Uses MeTTa scripts to control bot behavior**\
✔ **Voxel-based world interaction for automation**

⚠ **You will need a Minecraft client** to **connect and interact with the bot**.\
The bot does **not provide a graphical interface**, so you must use a **Minecraft game client** to chat with and observe the bot in action.

---

## 🛠 Prerequisites

Before running the bot, ensure you have:

- **A running Minecraft server**: The bot requires a Minecraft server to connect to. You can download and set up a **Minecraft Java Edition server** from [Mojang's official site](https://www.minecraft.net/en-us/download/server) or use a custom server like **PaperMC** or **Spigot**.

  - Ensure your server is running and accessible at `localhost:25565`.
  - Configure your server properties (`server.properties`) to allow non-premium accounts if needed.

- **A Minecraft client**: Since the bot has no graphical interface, you need **Minecraft Java Edition** to connect and interact with it.

  - Open **Multiplayer** in Minecraft.
  - Add a server with the address `localhost:25565`.
  - Connect and use the in-game chat to interact with the bot.

- **Java 21+** installed:

  ```sh
  java -version
  ```

- **Apache Maven** installed: using Instructions from:
  [Upgrade Maven Installation](https://lightspeed001.medium.com/upgrade-existing-maven-installation-to-latest-0c17074a866a)

  ```sh
  mvn -version
  ```

  Expected output:

  ```
  Apache Maven 3.9.6 (bc0240f3c744dd6b6ec2920b3cd08dcc295161ae)
  Maven home: /opt/apache-maven-3.9.6
  Java version: 21.0.5, vendor: Oracle Corporation, runtime: /usr/lib/jvm/jdk-21.0.5-oracle-x64
  Default locale: en_US, platform encoding: UTF-8
  OS name: "linux", version: "5.15.167.4-microsoft-standard-wsl2", arch: "amd64", family: "unix"
  ```

### **🔎 Hiding Java 17 from JPL**

JPL does not support Java 21+ when Java 17 is visible in the system. To prevent conflicts, ensure Java 17 is hidden:

```sh
sudo update-alternatives --config java
```

Select Java 21 as the default version.

Ensure that `LD_LIBRARY_PATH` does not point to Java 17:

```sh
echo $LD_LIBRARY_PATH
```

If necessary, update `.bashrc` or `/etc/environment`:

```sh
export JAVA_HOME=/usr/lib/jvm/jdk-21.0.5-oracle-x64
export PATH="$JAVA_HOME/bin:$PATH"
export LD_LIBRARY_PATH="$JAVA_HOME/lib/server/"
```

### **⚠️ Cannot use with Java 17**

If you encounter the following error:

```sh
ERROR: Java exception: java.lang.UnsupportedClassVersionError: io/trueagi/mettalog/minecraft/BotController 
  has been compiled by a more recent version of the Java Runtime (class file version 65.0), 
  this version of the Java Runtime only recognizes class file versions up to 61.0
```

This likely means that **SWI-Prolog is still using Java 17** instead of Java 21. To resolve this:

1. Verify which Java version SWI-Prolog is using:
   ```sh
   swipl
   ?- jpl_get_default_jvm_version(V).
   ```
2. If it is using Java 17, update `LD_LIBRARY_PATH` and `JAVA_HOME` to point to Java 21.
3. Restart your terminal or source your environment settings:
   ```sh
   source ~/.bashrc
   ```

---

## 🚀 Running the Minecraft Bot

You have **four methods** to run the bot:

1. **Using Java Only** (to ensure basic connectivity)
   ```sh
   source hello_java_only.sh
   ```
2. **Using Java**
   ```sh
   source java_run.sh
   ```
3. **Using MeTTa**
   ```sh
   source metta_run.sh
   ```
4. **Using Prolog**
   ```sh
   source prolog_run.sh
   ```

---

## 🏗️ Developing the Bot

### **📝 Writing MeTTa AI Logic**

MeTTa scripts define bot behavior. Example (`minecraft_bot_hello.metta`):

```metta
!(import! &self minecraft_bot_driver.metta )

(event-trigger (on_chat "hello bot") (say "Hello, human!"))
(event-trigger (on_chat "who are you?") (say "I am a MeTTa-powered bot!"))
(event-trigger (on_chat "what can you do?") (say "I can move, chat, and explore using voxel data!"))
(event-trigger (on_chat "move forward") (move 1 0 0))
(event-trigger (on_chat "move up") (move 0 1 0))
(event-trigger (on_chat "scan area") (enumerate_voxel_space))
(event-trigger (on_chat "goodbye") (say "Goodbye, see you soon!"))
```

### **🔹 Prolog Integration**

Prolog scripts (`minecraft_bot_hello.pl`) handle game logic:

```prolog
on_chat_message("hello bot") :-
    writeln('Bot: Hello, player!').
```

---

## 📞 Contact

For support, open an issue on GitHub. 🚀

