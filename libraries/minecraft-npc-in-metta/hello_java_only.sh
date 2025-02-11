#!/bin/bash

# Get the absolute path of the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
(
  # Change to script directory
  cd "$SCRIPT_DIR" || { echo "Failed to change directory to script location."; [[ $SOURCED == false ]] && exit 1 || return 1; }

    source environment.sh
    mvn clean package
    mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt
    
    export CLASSPATH="$(cat classpath.txt):$(pwd)/target/mettalog.minecraft-1.0-SNAPSHOT.jar"
    java -cp "$CLASSPATH" io.trueagi.mettalog.minecraft.BotExample

)
