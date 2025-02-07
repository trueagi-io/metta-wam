#!/bin/bash

DEBUG=false

# Detect if script is being sourced
SOURCED=false
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
  SOURCED=true
fi

# Parse arguments for debug mode
while getopts "d" opt; do
  case $opt in
    d)
      DEBUG=true
      echo "🛠 Debug mode enabled."
      ;;
    \?)
      echo "❌ Invalid option: -$OPTARG" >&2
      [[ $SOURCED == false ]] && exit 1 || return 1
      ;;
  esac
done

echo "🚀 Starting Metta-Minecraft Bot Setup..."

# Step 1: Build the Java Project
echo "🔧 Compiling Java project..."
mvn clean package || { echo "❌ Build failed! Check for errors."; [[ $SOURCED == false ]] && exit 1 || return 1; }
echo "✅ Completed Java project."

# Step 2: Generate the classpath file for dependencies
echo "📦 Resolving dependencies..."
mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt || { echo "❌ Failed to build classpath."; [[ $SOURCED == false ]] && exit 1 || return 1; }
echo "✅ Resolved project dependencies."

# Step 3: Set up environment variables
export LD_LIBRARY_PATH=/usr/local/lib/swipl/lib/x86_64-linux/
export CLASSPATH="$(pwd)/target/mettalog.minecraft-1.0-SNAPSHOT.jar:$(cat classpath.txt)"

# Debug Information
if $DEBUG; then
  echo "🔎 Debug Info:"
  echo "📁 Current Directory: $(pwd)"
  echo "📜 CLASSPATH: $CLASSPATH"
  echo "📜 Classpath File Contents:"
  cat classpath.txt
  echo "📜 Java Version:"
  java -version
  echo "📜 SWI-Prolog Version:"
  swipl --version
fi

# Step 4: Inform the user about the environment setup
cat <<EOF
✅ CLASSPATH/Environment setup complete!
🔹 If you encounter issues, check logs and ensure the Minecraft server is running.
📦 To verify the CLASSPATH in Metta, type: !(println! (call-fn getenv CLASSPATH))
🔧 To start the bot in Metta, type: !(start-bot)
EOF

# Step 5: Launch Metta if not sourced
echo "🧠 Launching Metta..."
mettalog minecraft_bot_hello.metta --repl

