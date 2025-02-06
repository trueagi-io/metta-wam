# 🏗️ Metta-MCBot  
A **Minecraft AI bot** powered by **MCProtocolLib, Prolog (JPL), and MeTTa**.  

This bot can:
- Connect to a **Minecraft server**.
- Process **chat messages** and send them to **MeTTa**.
- Work with **MeTTa scripts** for decision-making.
- Interact with the world using **Voxel space recognition**.

---

## 🚀 Features  
✔ **Connects to a Minecraft server**  
✔ **Receives chat messages and processes them**  
✔ **Uses MeTTa scripts to control bot behavior**  
✔ **Voxel-based world interaction for automation**  

⚠ **You will need a Minecraft client** to **connect and interact with the bot**.  
The bot does **not provide a graphical interface**, so you must use a **Minecraft game client** to chat with and observe the bot in action.

---

## 📌 Project Structure  
```
src/
│── main/
│   ├── metta/                     # MeTTa AI scripts
│   │   ├── minecraft_bot_hello.metta
│   │   ├── minecraft_bot_driver.metta
│   ├── java/io/trueagi/mettalog/minecraft/
│   │   ├── BotController.java      # Main bot logic
│   │   ├── BotExample.java         # Example bot usage
│   │   ├── VoxelGetter.java        # Handles voxel-based world interaction
│   │   ├── SWIPrologBuilder.java   # Prolog integration utilities
│   │   ├── WorldBuilder.java       # Builds world-related data
│   ├── prolog/                     # Prolog scripts
│   │   ├── minecraft_bot_hello.pl
│   │   ├── minecraft_bot_driver.pl
│── pom.xml                          # Maven project configuration
│── README.md                        # Project documentation
│── libs/                             # External JARs
│   ├── jpl8.jar                      # JPL (Java-Prolog) integration library
```

---

## 🛠 Prerequisites  
Before running the bot, ensure you have:
- **Java 17+** installed:
  ```sh
  java -version
  ```
- **Apache Maven** installed:
  ```sh
  mvn -version
  ```
- **SWI-Prolog** installed:
  ```sh
  swipl --version
  ```
- **A running Minecraft server** (e.g., `localhost:25565`).  
- **A Minecraft game client** (e.g., **Minecraft Java Edition**) to connect and interact with the bot.

---

## 🚀 Running the Minecraft Bot  
### **1️⃣ Build the Project**  
```sh
mvn clean package
```
### **2️⃣ Generate Classpath for Dependencies**  
Since the JAR requires dependencies, first generate the classpath:  
```sh
mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt
```
### **3️⃣ Start the Minecraft Server**  
Ensure you have a Minecraft **1.20+ server** running on `localhost:25565`.  
You can use **PaperMC, Spigot, or a vanilla server**.

### **4️⃣ Run the Bot**  
```sh
java -cp "$(cat classpath.txt):target/mettalog.minecraft-1.0-SNAPSHOT.jar" io.trueagi.mettalog.minecraft.BotController
```
### **5️⃣ Connect with a Graphical Minecraft Client**  
- **Launch your Minecraft Java Edition client**.  
- Go to **Multiplayer** → **Direct Connect**.  
- Connect to `localhost:25565`.  
- **Interact with the bot in chat!** (e.g., type `hello bot`).

### **6️⃣ Test Prolog Integration**  
Open SWI-Prolog and consult the Prolog scripts:
```prolog
?- consult('src/main/prolog/minecraft_bot_driver.pl').
?- consult('src/main/prolog/minecraft_bot_hello.pl').
```

---

## 🏗️ Developing the Bot  
### **📝 Writing MeTTa AI Logic**  
MeTTa scripts define bot behavior. Example (`minecraft_bot_hello.metta`):  
```metta
(: on_chat "hello bot" (say "Hello, human!"))
(: on_chat "where are you?" (say "I'm exploring the world."))
```

### **🔹 Prolog Integration**  
Prolog scripts (`minecraft_bot_hello.pl`) handle game logic:
```prolog
on_chat_message("hello bot") :-
    writeln('Bot: Hello, player!').
```

---

## 📌 Useful Commands  
### **➡️ Move the bot**  
```prolog
?- move(1, 0, 0).  % Move forward
?- move(0, 1, 0).  % Jump
```

### **💬 Chat from Prolog**  
```prolog
?- chat("I am a MeTTa-powered bot!").
```

### **🧊 Voxel Space Querying**  
```prolog
?- get_voxel_data(X, Y, Z, BlockID).
```

---

## 🔧 Troubleshooting  
### **"Bot not connecting to server"**  
- Ensure **Minecraft server is running** (`localhost:25565`).  
- Check **firewall settings** allowing connections.  

### **"Prolog script not loading"**  
- Ensure SWI-Prolog is installed and working:
  ```sh
  swipl --version
  ```
- Use **full file paths** when consulting:
  ```prolog
  ?- consult('/absolute/path/to/minecraft_bot_hello.pl').
  ```

### **"Maven build fails"**  
- Try **forcing a dependency update**:
  ```sh
  mvn clean package -U
  ```
- Ensure **JPL is in `libs/`**:
  ```sh
  ls libs/jpl8.jar
  ```

### **"Bot does not respond to chat"**  
- Ensure **you are using a Minecraft Java Edition client** to chat with the bot.  
- Make sure **the bot is properly connected** to the Minecraft server (`localhost:25565`).  
- Check the **Prolog script logic** to confirm responses are correctly defined.

---

## 🤝 Contributing  
We welcome contributions!  
1. Fork the repo  
2. Create a new branch: `feature-name`  
3. Submit a PR 🎉  

---

## 📝 License  
This project is licensed under **MIT License**.

---

## 📞 Contact  
For support, open an issue on GitHub. 🚀

