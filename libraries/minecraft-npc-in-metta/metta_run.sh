#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

(
    cd "$SCRIPT_DIR" || { echo "❌ Failed to change directory to script location."; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1; }

    # Ensure environment variables are correctly loaded
    source environment.sh || { echo "❌ Failed to source environment.sh"; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1; }

    # Build the Java Project
    echo "🔧 Compiling Java project..."
    mvn clean package || { echo "❌ Build failed!"; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1; }
    echo "✅ Java project compiled successfully."

    # Generate the classpath file for dependencies
    echo "📦 Resolving dependencies..."
    mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt || { echo "❌ Failed to build classpath."; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1; }
    echo "✅ Dependencies resolved."

    # Ensure classpath.txt exists before reading it
    if [[ ! -f classpath.txt ]]; then
        echo "❌ classpath.txt not found! Build process may have failed."
        [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1
    fi

    # Set CLASSPATH
    export CLASSPATH="$(pwd)/target/mettalog.minecraft-1.0-SNAPSHOT.jar:$(cat classpath.txt)"
    echo "🔹 CLASSPATH set."

    # Display environment setup instructions
    cat <<EOF
📦 To verify the CLASSPATH in Metta, type: !(println! (call-fn getenv CLASSPATH))
🔧 To start the bot in Metta, type: !(start-bot)
EOF

    # Launch Metta
    echo "🧠 Launching Metta..."
    mettalog minecraft_bot_hello.metta --repl
)

