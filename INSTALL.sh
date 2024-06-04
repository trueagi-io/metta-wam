#!/bin/bash

export RPWD=$PWD
IS_SOURCED=$( [[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo 1 || echo 0)
# Function to exit the script properly
if [ "$IS_SOURCED" -eq "0" ]; then SCRIPT=$(readlink -f "$0"); else SCRIPT=$(readlink -f "${BASH_SOURCE[0]}"); fi
export MeTTa=$(realpath "$SCRIPT")
export METTALOG_DIR=$(dirname "$MeTTa")
export PIP_BREAK_SYSTEM_PACKAGES=1
# cd "$METTALOG_DIR" || { echo "Failed to navigate to $METTALOG_DIR"; [[ "$IS_SOURCED" == "1" ]] && return 1 || exit 1; }

(cd $METTALOG_DIR ; git update-index --assume-unchanged .bash_history) || true

# Run this file with ./INSTALL.md
# ```

# Function to prompt for user confirmation with 'N' as the default
confirm_with_default() {
    echo -e -n "$2"
    while true; do
        if [ "$1" == "N" ]; then
            read -s -p " (y/N): " -n 1 yn
        else
            read -s -p " (${1}/n): " -n 1 yn
        fi

        if [ -z "$yn" ]; then
            yn="$1"  # Corrected assignment without spaces
        fi

        case $yn in
            [Yy]* ) echo "Y" && return 0;;
            [Nn]* ) echo "N" && return 1;;
            * ) echo -e "${YELLOW}Please answer yes or no.${NC}";;
        esac
    done
}


# Function to prompt for input with a default value
prompt_for_input() {
    read -e -i "$2" -p "$1" value
    echo -e "${value:-$2}"
}

# ANSI escape codes
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
GREEN='\033[0;32m'
BOLD='\033[1m'
# ANSI escape code to reset color
NC='\033[0m' # No Color

# Initialize easy_install to a default value of '?'
easy_install="?"

# Check command line arguments for --steps or --easy
for arg in "$@"
do
    case $arg in
        --steps)
            easy_install="N"
            shift # Remove --steps from the list of arguments
            ;;
        --easy)
            easy_install="Y"
            shift # Remove --easy from the list of arguments
            ;;
        *)
            # Ignore unknown options
            ;;
    esac
done

# Ask the user if easy_install is still '?'
if [ "$easy_install" == "?" ]; then
    if confirm_with_default "Y" "Would you like to use easy installation mode?"; then
        easy_install="Y"
    else
        easy_install="N"
    fi
fi

if [ -f /.dockerenv ]; then
    inside_docker="-y"
else
    inside_docker=""
fi


echo -e "${BLUE}Starting the installation process..${NC}."

# Function to compare versions
version_ge() {
    # Compare $1 with $2; if $1 >= $2, return 0 (true), else return 1 (false)
    printf '%s\n%s' "$2" "$1" | sort -VC
    return $?
}

# SWI-Prolog from source
build_swi_prolog_from_src() {

    sudo apt-get update
    # Install build dependencies
    echo -e "${BLUE}Installing build dependencies...${NC}"
    local build_deps="build-essential autoconf git cmake libpython3-dev libgmp-dev libssl-dev unixodbc-dev \
        libreadline-dev zlib1g-dev libarchive-dev libossp-uuid-dev libxext-dev \
        libice-dev libjpeg-dev libxinerama-dev libxft-dev libxpm-dev libxt-dev \
        pkg-config libdb-dev libpcre3-dev libyaml-dev"
    sudo apt-get install -y $build_deps && {
        echo -e "${GREEN}Build dependencies installed successfully.${NC}"
    } || {
        echo -e "${RED}Failed to install build dependencies. Exiting.${NC}"
        exit 1
    }
    
    rm -rf swipl-devel

    # Check if the SWI-Prolog source code directory exists
    if [ -d "swipl-devel" ]; then
        echo -e "${BLUE}SWI-Prolog source code directory exists. Pulling updates...${NC}"
        cd swipl-devel && git pull && cd ..
    else
        echo -e "${BLUE}Cloning SWI-Prolog source code...${NC}"
        git clone https://github.com/SWI-Prolog/swipl-devel.git
        
    fi

    #(cd swipl-devel && git checkout V9.3.0)


    # Update submodules
    echo -e "${BLUE}Updating submodules...${NC}"
    #git -C . submodule update --init packages/ltx2htm packages/pldoc packages/nlp packages/archive packages/clib packages/http packages/sgml packages/ssl packages/zlib
    cd swipl-devel && git submodule update --init  && {
        echo -e "${GREEN}Submodules updated successfully.${NC}"
    } || {
        echo -e "${RED}Failed to update submodules. Exiting.${NC}"
        exit 1
    }

    # Configure and build
    echo -e "${BLUE}Configuring and building SWI-Prolog...${NC}"
    unset LD_PRELOAD
    mkdir -p build && cd build
    cmake .. && make && {
        echo -e "${GREEN}SWI-Prolog configured and built successfully.${NC}"
    } || {
        echo -e "${RED}Failed during SWI-Prolog build process. Exiting.${NC}"
        exit 1
    }
    sudo make install && {
        echo -e "${GREEN}SWI-Prolog installed successfully.${NC}"
    } || {
        echo -e "${RED}Failed to install SWI-Prolog. Exiting.${NC}"
        exit 1
    }
}

function swipl_version () {
    if ! command -v swipl &> /dev/null; then
       echo "0.0"
    else
       echo $(swipl --version | awk '{print $3}')
    fi
}

# Function to install or update SWI-Prolog
install_or_update_swipl() {
    echo -e "${BLUE}Starting SWI-Prolog installation or update.${NC}"

    #commented this out since the maintainer may not be doing this for every version we want to run
    #echo " " > /etc/apt/sources.list
    #sudo apt install -y apt-utils software-properties-common
    #sudo apt-add-repository -y ppa:swi-prolog/devel
    #sudo apt-get update
    # Remove existing installation if any before reinstalling/upgrading
    #sudo apt-get remove -y swi-prolog??* 
    #sudo apt-get install -y swi-prolog
    swi_prolog_version=$(swipl_version)
    required_version="9.1"
    if version_ge $swi_prolog_version $required_version; then
        echo -e "${GREEN}SWI-Prolog version $swi_prolog_version is installed and meets the required version $required_version or higher.${NC}"
    else
            echo -e "${YELLOW}Attempting to update SWI-Prolog...${NC}"
            build_swi_prolog_from_src
            swi_prolog_version=$(swipl_version)
            if version_ge $swi_prolog_version $required_version; then
                echo -e "${GREEN}SWI-Prolog upgraded to $swi_prolog_version, which meets the required version $required_version or higher.${NC}"
            else
                echo -e "${YELLOW}Failed to upgrade SWI-Prolog to version $required_version or higher. Janus may not work without this version.${NC}"
            fi
    fi

}

# Check if SWI-Prolog is installed
if ! command -v swipl &> /dev/null; then
    if confirm_with_default "Y" "SWI-Prolog is not installed. Would you like to install it?"; then
        install_or_update_swipl
    else
        echo -e "${RED}SWI-Prolog installation aborted. Exiting script${NC}."
        exit 1
    fi
else
    swi_prolog_version=$(swipl_version)
    required_version="9.1"
    if version_ge $swi_prolog_version $required_version; then
        echo -e "${GREEN}SWI-Prolog version $swi_prolog_version is installed and meets the required version $required_version or higher.${NC}"
    else
        if confirm_with_default "Y" "SWI-Prolog is not version $required_version or higher. Would you like to update it?"; then
            echo -e "${YELLOW}Attempting to update SWI-Prolog...${NC}"
            install_or_update_swipl
            swi_prolog_version=$(swipl_version)
            if version_ge $swi_prolog_version $required_version; then
                echo -e "${GREEN}SWI-Prolog upgraded to $swi_prolog_version, which meets the required version $required_version or higher.${NC}"
            else
                echo -e "${YELLOW}Failed to upgrade SWI-Prolog to version $required_version or higher. Janus may not work without this version.${NC}"
            fi
        fi
    fi
fi

function ensure_pip() {
    # Check if pip is installed
    if ! command -v pip &> /dev/null; then
	echo "pip is not installed. Installing pip..."
	sudo apt-get update
	sudo apt-get install -y python3-pip
	if [ $? -ne 0 ]; then
	    echo -e "${RED}Failed to install pip. Exiting script${NC}."
	    exit 1
	fi
    else
	echo "pip is already installed."
    fi
}

# Assuming SWI-Prolog 9.1 is installed successfully
# Install Janus for SWI-Prolog
echo -e "${BLUE}Checking if Janus Python support is already installed${NC}..."
if ! swipl -g "use_module(library(janus)), halt(0)." -t "halt(1)" 2>/dev/null; then
    # janus not installed, prompt the user
    if [ "${easy_install}" == "Y" ] || confirm_with_default "Y" "Would you like to install Python (Janus) support"; then
	    echo "Installing Janus for SWI-Prolog..."
	    ensure_pip
	    sudo pip install git+https://github.com/SWI-Prolog/packages-swipy.git
	    sudo apt install libpython3-dev
	    if [ $? -ne 0 ]; then
		echo -e "${RED}Failed to install Janus. Exiting script${NC}."
		exit 1
	    else
		echo "Janus installed successfully."
	    fi
    else
        echo -e "${YELLOW}Skipping Janus Python support installation${NC}."
    fi
else
    echo -e "${GREEN}Janus Python support is already installed${NC}."
fi


# Install PySWIP for SWI-Prolog
echo -e "${BLUE}Checking if Pyswip is already installed${NC}..."
if ! python3 -c "import pyswip" &> /dev/null; then
    # Pyswip not installed, prompt the user
    if [ "${easy_install}" == "Y" ] || confirm_with_default "Y" "Would you like to install Pyswip"; then
        echo -e "${BLUE}Installing Pyswip..${NC}."
	ensure_pip
        sudo pip install git+https://github.com/logicmoo/pyswip.git
        echo -e "${GREEN}Pyswip installation complete${NC}."
    else
        echo -e "${YELLOW}Skipping Pyswip installation${NC}."
    fi
else
    echo -e "${GREEN}Pyswip is already installed${NC}."
fi

if false && ! swipl -g "use_module(library(predicate_streams)), halt(0)." -t "halt(1)" 2>/dev/null; then
    echo "Installing predicate_streams..."
    echo -e "${YELLOW}${BOLD}If asked, say yes to everything and/or accept the defaults...${NC}"
    (
    if [ -d "reqs/predicate_streams" ]; then
        echo -e "${BLUE}predicate_streams directory exists. Pulling updates...${NC}"
        (cd reqs/predicate_streams && git pull)
     else
        mkdir -p reqs/ && cd reqs
        git clone https://github.com/logicmoo/predicate_streams
        cd ..
     fi
    ) || swipl -g "pack_install(predicate_streams,[interactive(false)])" -t halt
else
    echo -e "${GREEN}Pack predicate_streams is already installed${NC}."
fi



if false && ! swipl -g  "use_module(library(logicmoo_utils)), halt(0)." -t "halt(1)" 2>/dev/null; then
    echo "Installing logicmoo_utils..."
    echo -e "${YELLOW}${BOLD}If asked, say yes to everything and/or accept the defaults...${NC}"
    (
    if [ -d "reqs/logicmoo_utils" ]; then
        echo -e "${BLUE}logicmoo_utils directory exists. Pulling updates...${NC}"
        (cd reqs/logicmoo_utils && git pull)
     else
        mkdir -p reqs/ && cd reqs
        git clone https://github.com/TeamSPoon/logicmoo_utils
        cd ..
     fi
    ) || swipl -g "pack_install(logicmoo_utils,[interactive(false)])" -t halt
 else
    echo -e "${GREEN}Pack logicmoo_utils is already installed${NC}."
fi

env_file="${METTALOG_DIR}/scripts/envvars_mettalog.sh"

# Function to check if metalog is in the user's PATH and update environment variables
check_metalog_in_path() {
    # Add sourcing line to .bashrc if it's not already there
    if ! grep -q "$env_file" "${HOME}/.bashrc"; then
        echo ""
        echo -e "${BLUE}MeTTaLog is not in your .bashrc${NC}."
        echo "" >> "${HOME}/.bashrc"
        echo "# Source MeTTaLog environment" >> "${HOME}/.bashrc"
        echo "source \"$env_file\"" >> "${HOME}/.bashrc"
        echo -e "${GREEN}MeTTaLog is NOW in your .bashrc!${NC}."
    else 
        echo -e "${GREEN}MeTTaLog was already in your .bashrc!${NC}."
    fi

    source "$env_file"

    echo "METTALOG_DIR=$METTALOG_DIR"
    echo "PYTHONPATH=$PYTHONPATH"
    echo "PATH=$PATH"
}

# Call the function to perform the check and update
check_metalog_in_path

which swipl

echo -e "${GREEN}Installation and setup complete!${NC}."


if false && confirm_with_default "N" "Show README.md"; then
    echo -en "${GREEN}"
    cat README.md
    echo -en "${NC}"
fi

# End of the script

# ```


