# 🏗️ Metta-MCBot
A Minecraft bot powered by **MCProtocolLib, Prolog (JPL), and Metta**. This bot can:
- Connect to a **Minecraft server**.
- Process **chat messages** and send them to **MeTTa**.
- Work with **Metta scripts** for decision-making.
- Interact with the world using **Voxel space recognition**.

## 🚀 Features
✔ **Connects to a Minecraft server**  
✔ **Receives chat messages and processes them**  
✔ **Uses Metta scripts to control bot behavior**  
✔ **Voxel-based world interaction for automation**  

---

## 📌 Project Structure
```
src/
│── main/
│   ├── metta/                     # Metta AI scripts
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
- **A running Minecraft server** (e.g., localhost:25565).

---

## 📥 Installation
### **1️⃣ Clone the Repository**
```sh
git clone https://github.com/trueagi/metta-mcbot.git
cd metta-mcbot
```

### **2️⃣ Build the Project**
```sh
mvn clean package
```

---

## 🚀 Running the Minecraft Bot
### **1️⃣ Start the Minecraft Server**
Ensure you have a Minecraft **1.20+ server** running on `localhost:25565`.

### **2️⃣ Run the Bot**
```sh
java -cp target/metta-mcbot-1.0-SNAPSHOT.jar io.trueagi.mettalog.minecraft.BotController
```

### **3️⃣ Test Prolog Integration**
Open SWI-Prolog and consult the Prolog scripts:
```prolog
?- consult('src/main/prolog/minecraft_bot_hello.pl').
?- consult('src/main/prolog/minecraft_bot_driver.pl').
```

---

## 🏗️ Developing the Bot
### **📝 Writing Metta AI Logic**
Metta scripts define bot behavior. Example (`minecraft_bot_hello.metta`):
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
?- chat("I am a Metta-powered bot!").
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
For support,  open an issue on GitHub. 🚀


