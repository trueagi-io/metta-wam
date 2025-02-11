#!/bin/bash

# Function to check for JDK 21 availability
check_jdk21_available() {
    if apt-cache search --names-only 'jdk.*21' | grep -Ei 'jdk.*21|java.*21' > /dev/null; then
        echo "JDK 21 package found in the repositories."
        return 0
    else
        echo "JDK 21 package not found in the repositories."
        return 1
    fi
}

# Check if the script is being sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    echo "This script must be sourced to affect the current environment."
    exit 1
fi



# Scan for libjvm.so to determine JAVA_HOME
echo "Scanning for libjvm.so to determine JAVA_HOME..."
java_homes=($(sudo find /usr/lib/jvm -name "libjvm.so" | grep "lib/server/libjvm.so" | sed 's|/lib/server/libjvm.so||' | sort))

if [[ ${#java_homes[@]} -eq 0 ]]; then
    echo "Error: No valid JDK installation found with libjvm.so."
    return 1
fi

# Prioritize JDK versions matching "jdk*21" or "msopenjdk-21-amd64"
for home in "${java_homes[@]}"; do
    if [[ "$(basename "$home")" == jdk*21* || "$(basename "$home")" == "msopenjdk-21-amd64" ]]; then
        export JAVA_HOME="$home"
        break
    fi
done

if [[ -z "$JAVA_HOME" ]]; then

	echo "Checking for JDK 21 availability before adding PPA..."
	if ! check_jdk21_available; then
	    echo "Adding PPA for Oracle JDK and updating package lists..."
	    sudo apt update
	    sudo apt install --reinstall software-properties-common -y
	    sudo add-apt-repository ppa:openjdk-r/ppa -y
	    sudo add-apt-repository ppa:linuxuprising/java -y
	    sudo apt-get update
	    check_jdk21_available || { echo "JDK 21 still not available."; return 1; }
	fi

	# Find the correct package name dynamically
	jdk_package=$(apt-cache search --names-only 'jdk.*21' | grep -Ei 'jdk.*21|java.*21' | awk '{print $1}' | head -n1)

	if [[ -z "$jdk_package" ]]; then
	    echo "No JDK 21 package found for installation."
	    return 1
	fi

	echo "Installing $jdk_package..."
	sudo apt-get install -y "$jdk_package" || { echo "JDK installation failed."; return 1; }



	# Scan for libjvm.so to determine JAVA_HOME
	echo "Scanning for libjvm.so to determine JAVA_HOME..."
	java_homes=($(sudo find /usr/lib/jvm -name "libjvm.so" | grep "lib/server/libjvm.so" | sed 's|/lib/server/libjvm.so||' | sort))

	if [[ ${#java_homes[@]} -eq 0 ]]; then
	    echo "Error: No valid JDK installation found with libjvm.so."
	    return 1
	fi

	# Prioritize JDK versions matching "jdk*21" or "msopenjdk-21-amd64"
	for home in "${java_homes[@]}"; do
	    if [[ "$(basename "$home")" == jdk*21* || "$(basename "$home")" == "msopenjdk-21-amd64" ]]; then
		export JAVA_HOME="$home"
		break
	    fi
	done

fi

echo "JAVA_HOME=$JAVA_HOME"

# Ensure Java is properly set
sudo update-alternatives --install /usr/bin/java java "$JAVA_HOME/bin/java" 2000 || { echo "Failed to set Java alternative."; return 1; }
sudo update-alternatives --install /usr/bin/javac javac "$JAVA_HOME/bin/javac" 2000 || { echo "Failed to set Javac alternative."; return 1; }
sudo update-alternatives --set java "$JAVA_HOME/bin/java"
sudo update-alternatives --set javac "$JAVA_HOME/bin/javac"

# Validate Java Installation
java_version=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}')
if [[ -z "$java_version" || "$java_version" != "21"* ]]; then
    echo "Error: Java version is not set correctly. Detected version: $java_version"
    return 1
else
    echo "Java version $java_version is correctly installed and active."
fi

# Set M2_HOME and install Maven if missing
export M2_HOME="/opt/apache-maven-3.9.6"
if [ ! -d "$M2_HOME" ]; then
    echo "Maven directory not found. Downloading and installing Maven..."
    sudo mkdir -p "$M2_HOME"
    sudo chown -R $(whoami) "$M2_HOME"
    sudo curl -fsSL https://dlcdn.apache.org/maven/maven-3/3.9.6/binaries/apache-maven-3.9.6-bin.tar.gz | sudo tar -xz --strip-components=1 -C "$M2_HOME"
fi

# Define the correct order for PATH
PREPEND_PATH="$JAVA_HOME/bin:$M2_HOME/bin"

# Remove existing instances of JAVA_HOME/bin and M2_HOME/bin in PATH to avoid duplicates
CLEANED_PATH=$(echo "$PATH" | sed -E "s#:?$JAVA_HOME/bin:##g; s#:?$M2_HOME/bin:##g; s#:$JAVA_HOME/bin##g; s#:$M2_HOME/bin##g")


# Ensure PREPEND_PATH is always at the front
export PATH="$PREPEND_PATH:$CLEANED_PATH"

# Ensure JAVA_HOME/lib/server is the first entry in LD_LIBRARY_PATH
if [[ -z "$LD_LIBRARY_PATH" ]]; then
    export LD_LIBRARY_PATH="$JAVA_HOME/lib/server"
elif [[ "$LD_LIBRARY_PATH" != "$JAVA_HOME/lib/server:"* && "$LD_LIBRARY_PATH" != "$JAVA_HOME/lib/server" ]]; then
    export LD_LIBRARY_PATH="$JAVA_HOME/lib/server:$LD_LIBRARY_PATH"
fi

# Echo environment variables at the end
echo ""
echo "========================================="
echo "M2_HOME=$M2_HOME"
echo "JAVA_HOME=$JAVA_HOME"
echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
echo ""
echo "PATH=$PATH"
echo "========================================="
echo ""

return 0  # Ensure script exits cleanly when sourced

