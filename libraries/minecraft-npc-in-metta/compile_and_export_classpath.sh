#!/bin/bash

DEBUG=false

# Detect if script is being sourced
SOURCED=false
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
  SOURCED=true
fi

# Get the absolute path of the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Enclose execution in a subshell to ensure directory is restored

  # Change to script directory
  cd "$SCRIPT_DIR" || { echo "❌ Failed to change directory to script location."; [[ $SOURCED == false ]] && exit 1 || return 1; }

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


