#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)" # Get the absolute path of the script directory
( # Change to script directory
  cd "$SCRIPT_DIR" || { echo "Failed to change directory to script location."; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1 }

    source envionment.sh

    mvn clean package
    mvn dependency:build-classpath -Dmdep.outputFile=classpath.txt
    
    export CLASSPATH="$(pwd)/target/mettalog.minecraft-1.0-SNAPSHOT.jar:$(cat classpath.txt)"

    java -cp "$CLASSPATH" io.trueagi.mettalog.minecraft.BotController
)
