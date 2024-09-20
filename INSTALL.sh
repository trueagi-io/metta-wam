#!/bin/bash -x -e -v
# ```
# Run this file with source ./INSTALL.md
# ANSI escape codes for colors
YELLOW='\033[1;33m'
BLUE='\033[1;36m' # Lighter shade of blue
RED='\033[0;31m'
GREEN='\033[1;32m' # Lighter shade of green
BOLD='\033[1m'
NC='\033[0m' # No Color

# Ensure the script is being sourced, not executed
IS_SOURCED=$( [[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo 1 || echo 0)
if [ "$IS_SOURCED" -eq "0" ]; then
    echo -e "${RED}This script must be sourced, not executed. Use 'source $0'${NC}."
    return 1
fi

# Check if "--help" is in the arguments and display help if it is
if [[ "$@" =~ "--help" ]]; then
    echo -e "${BOLD}Usage:${NC} source $0 [OPTIONS]"
    echo ""
    echo -e "${BOLD}Options:${NC}"
    echo -e "  ${YELLOW}--help${NC}                          Show this help message and exit."
    echo -e "  ${YELLOW}--force${NC}                         Force rebuilding of SWI-Prolog."
    echo -e "  ${YELLOW}--swi=src|ppa|skip${NC}              Choose the installation method for SWI-Prolog:"
    echo -e "                                  ${BLUE}'src'${NC}  - Install from source."
    echo -e "                                  ${BLUE}'ppa'${NC}  - Install from PPA (default)."
    echo -e "                                  ${BLUE}'skip'${NC} - Skip SWI-Prolog installation."
    echo -e "  ${YELLOW}--easy[=y|n]${NC}                    Enable or disable easy installation mode."
    echo -e "                                  '--easy' implies '--easy=y'."
    echo -e "  ${YELLOW}--steps${NC}                         Enable step-by-step installation."
    echo -e "  ${YELLOW}--allow-system-modifications${NC}    Allow modifying system files and settings."
    echo -e "  ${YELLOW}--break-system-packages${NC}         Use PIP (Python) Flag: --break-system-packages"
    return 0
fi

# Export environment variables
export RPWD=$PWD
export MeTTa=$(realpath "${BASH_SOURCE[0]}")
export METTALOG_DIR=$(dirname "$MeTTa")

cd "$METTALOG_DIR" || { echo "Failed to navigate to $METTALOG_DIR"; [[ "$IS_SOURCED" == "1" ]] && return 1 || exit 1; }


if [ -f .bash_history ]; then 
    (cd $METTALOG_DIR ; git update-index --assume-unchanged .bash_history) || true
fi

# Default values for variables
FORCE_REINSTALL_SWI=0
SWI_INSTALL="src"
EASY_INSTALL="?"
INSTALL_TYPE="non_docker"

# Detect environment unless user has already specified
if [ -n "$GITHUB_ACTIONS" ]; then
    echo -e "${BLUE}GitHub Actions environment detected${NC}."
    INSTALL_TYPE="github_vm"
    export PIP_BREAK_SYSTEM_PACKAGES=1
    export ALLOW_MODIFY_SYSTEM=1    
    SWI_INSTALL="ppa"
    EASY_INSTALL="Y"
elif [ ! -z "$JENKINS_URL" ]; then
    echo -e "${BLUE}Jenkins environment detected${NC}."
    INSTALL_TYPE="jenkins_ci"
    #export PIP_BREAK_SYSTEM_PACKAGES=1
    #export ALLOW_MODIFY_SYSTEM=1
    SWI_INSTALL="ppa"
    EASY_INSTALL="Y"
elif [ -f /.dockerenv ] || grep -qa docker /proc/1/cgroup; then
    echo -e "${BLUE}Docker environment detected${NC}."
    INSTALL_TYPE="docker_vm"
    export PIP_BREAK_SYSTEM_PACKAGES=1
    export ALLOW_MODIFY_SYSTEM=1        
    SWI_INSTALL="src"
    EASY_INSTALL="Y"
else
    echo -e "${BLUE}User's Devel Machine${NC}."
    INSTALL_TYPE="non_docker"
    SWI_INSTALL="src"
    EASY_INSTALL="Y"
fi

# Parse command-line arguments and allow user overrides
for arg in "$@"; do
  case $arg in
    --swi=*) SWI_INSTALL="${arg#*=}" ;;
    --force) FORCE_REINSTALL_SWI=1 ; UPDATE_SYSTEM=1 ;;
    --steps) EASY_INSTALL="N" ;;
    --easy)  EASY_INSTALL="Y" ;;
    --easy=*)
      easy_value=$(echo "${arg#*=}" | tr '[:upper:]' '[:lower:]')
      [[ "$easy_value" =~ ^(y|n)$ ]] && EASY_INSTALL="${easy_value^^}" || {
        echo -e "${RED}Invalid value for --easy. Use y|n.${NC}"; return 1
      }
    ;;
    --allow-system-modifications)
      export ALLOW_MODIFY_SYSTEM=1
      echo -e "${GREEN}System modifications allowed${NC}"
    ;;
    --break-system-packages)
      export PIP_BREAK_SYSTEM_PACKAGES=1
      echo -e "${GREEN}Breaking system packages allowed${NC}"
    ;;
    *) echo -e "${RED}Unknown argument:${NC} $arg" ;;
  esac
done

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
            yn="$1"
        fi

        case $yn in
            [Yy]* ) echo "Y" && return 0;;
            [Nn]* ) echo "N" && return 1;;
            * ) echo -e "${YELLOW}Please answer yes or no${NC}.";;
        esac
    done
}

prompt_for_input() {
    # Function to prompt for input with a default value to be used later
    read -e -i "$2" -p "$1" value
    echo -e "${value:-$2}"
}



# Function to compare versions
version_ge() {
    # Compare $1 with $2; if $1 >= $2, return 0 (true), else return 1 (false)
    printf '%s\n%s' "$2" "$1" | sort -VC
    return $?
}


# List of required system dependencies for SWI-Prolog development, including various Java development tools and build systems
SWI_DEV_DEPS="build-essential autoconf git cmake libpython3-dev libgmp-dev libssl-dev unixodbc-dev libffi-dev \
    libreadline-dev zlib1g-dev libarchive-dev libossp-uuid-dev libxext-dev libice-dev libjpeg-dev \
    libxinerama-dev libxft-dev libxpm-dev libunwind-dev libxt-dev pkg-config libdb-dev libpcre3 libpcre3-dev \
    libpcre2-dev libpcrecpp0v5 libpcre16-3 libpcre32-3 libpcrecpp0v5 libqt5widgets5 qtbase5-dev \
    libgoogle-perftools-dev"
    
# Required for JPL    
SWI_DEV_DEPS+=" junit junit4 libhamcrest-java default-jdk default-jdk-headless"

# Function to check if a package is installed
is_package_installed() {
    dpkg-query -W -f='${Status}' "$1" 2>/dev/null | grep -q "install ok installed"
}

# Only install system dependencies if needed
install_swi_devel_deps() {
    local missing_packages=()

    echo -e "${BLUE}Checking required system dependencies...${NC}"

    # Loop through each package in SWI_DEV_DEPS to check if it's installed
    for package in $SWI_DEV_DEPS; do
        if ! is_package_installed "$package"; then            
            missing_packages+=("$package")
        fi
    done

    if [ ${#missing_packages[@]} -eq 0 ]; then
        echo -e "${GREEN}All development dependencies are already installed.${NC}"
        return 0
    else
        echo -e "${YELLOW}The following packages are missing:${NC}"
        echo "${missing_packages[@]}"
    fi

    if [ "$UPDATE_SYSTEM" -eq 1 ]; then
        echo -e "${BLUE}Installing missing dependencies...${NC}"
        sudo apt-get update
        sudo apt-get install -y "${missing_packages[@]}" && {
            echo -e "${GREEN}All missing build dependencies installed successfully${NC}."
            return 0
        } || {
            echo -e "${RED}Failed to install one or more dependencies.${NC}"
            return 0
        }
    else
        echo -e "${RED}System modifications are not allowed. The following missing dependencies were found but cannot be installed:${NC}"
        for pkg in "${missing_packages[@]}"; do
            echo -e "${RED}$pkg${NC}"
        done
        return 0
    fi
}



# Function to install SWI-Prolog based on the value of SWI_INSTALL
install_swi() {
  if [ "$SWI_INSTALL" = "src" ]; then
    install_swi_from_src
  elif [ "$SWI_INSTALL" = "ppa" ]; then
    install_swi_from_ppa
  elif [ "$SWI_INSTALL" = "upgrade" ]; then
    install_swi_from_ppa_UP
  else
    echo "Skipping SWI-Prolog installation. SWI_INSTALL is set to $SWI_INSTALL."
  fi
}

install_swi_from_ppa_UP() {
    echo -e "${BLUE}Installing SWI-Prolog Devel PPA UPGRADING..${NC}."
    if false && confirm_with_default "N" "Blank out APT SOURCES"; then
        echo -e "${BLUE}Blanking out APT SOURCES.${NC}."
        echo " " > /etc/apt/sources.list
    fi
    sudo apt install -y apt-utils software-properties-common
    sudo apt-add-repository -y ppa:swi-prolog/devel
    sudo apt-get update
    echo -e "${BLUE}Remove existing installation if any before reinstalling/upgrading..${NC}."
    sudo apt-get remove -y swi-prolog??*
    sudo apt-get install -y swi-prolog
    }
    
install_swi_from_ppa() {
    install_swi_devel_deps
    echo -e "${BLUE}Installing SWI-Prolog Devel PPA..${NC}."
    sudo add-apt-repository ppa:swi-prolog/devel -y
    sudo apt update
    echo -e "${BLUE}Installing SWI-Prolog Itself..${NC}."
    sudo apt install -y swi-prolog
}

# SWI-Prolog from source
install_swi_from_src() {
    echo -e "${BLUE}Installing SWI-Prolog from SRC..${NC}."
    
    if [ -n "$SWI_INSTALL_VERSION" ]; then
        rm -rf swipl-devel
    fi
    

    install_swi_devel_deps

    # Check if the SWI-Prolog source code directory exists
    if [ -d "swipl-devel" ]; then
        echo -e "${BLUE}SWI-Prolog source code directory exists. Pulling updates..${NC}."
        cd swipl-devel && git pull && cd ..
    else
        echo -e "${BLUE}Cloning SWI-Prolog source code..${NC}."
        git clone https://github.com/SWI-Prolog/swipl-devel.git        
    fi

    if [ -n "$SWI_INSTALL_VERSION" ]; then
        echo -e "${BLUE}Using ${SWI_INSTALL_VERSION} version of SWI-Prolog..${NC}."
        cd swipl-devel && git checkout "$SWI_INSTALL_VERSION"
    fi

    # Update submodules
    echo -e "${BLUE}Updating submodules..${NC}."
    if [ -n "$SWI_INSTALL_VERSION" ]; then
        echo -e "${BLUE}Special submodule update for ${SWI_INSTALL_VERSION} version of SWI-Prolog..${NC}."
        git -C . submodule update --init packages/ltx2htm packages/pldoc packages/nlp packages/archive packages/clib packages/http packages/sgml packages/ssl packages/zlib
    fi

    cd swipl-devel && git submodule update --init  && {
        echo -e "${GREEN}Submodules updated successfully${NC}."
    } || {
        echo -e "${RED}Failed to update submodules. Exiting${NC}."
        return 1
    }

    # Configure and build
    echo -e "${BLUE}Configuring and building SWI-Prolog..${NC}."
    
    if [ -n "$SWI_INSTALL_VERSION" ]; then
        echo -e "${BLUE}Unsetting LD_PRELOAD to avoid interference during the build..${NC}."
        unset LD_PRELOAD
    fi
    
    mkdir -p build && cd build
    cmake .. && make && {
        echo -e "${GREEN}SWI-Prolog configured and built successfully${NC}."
    } || {
        echo -e "${RED}Failed during SWI-Prolog build process. Exiting${NC}."
        return 1
    }
    sudo make install && {
        echo -e "${GREEN}SWI-Prolog installed successfully${NC}."
    } || {
        echo -e "${RED}Failed to install SWI-Prolog. Exiting${NC}."
        return 1
    }
}

function swipl_version () {
    if ! command -v swipl &> /dev/null; then
       echo "0.0"
    else
       echo $(swipl --version | awk '{print $3}'
    fi
}

# Function to install or update SWI-Prolog
install_or_update_swipl() {
    echo -e "${BLUE}Starting SWI-Prolog installation or update${NC}."

    swi_prolog_version=$(swipl_version)
    required_version="9.3.9"

    if [ "$FORCE_REINSTALL_SWI" -eq 1 ]; then
        echo -e "${YELLOW}Forcing rebuild of SWI-Prolog as --force was passed.${NC}"
        install_swi
    elif version_ge "$swi_prolog_version" "$required_version"; then
        echo -e "${GREEN}SWI-Prolog version $swi_prolog_version is installed and meets the required version $required_version or higher${NC}."
    else
        echo -e "${YELLOW}Attempting to update SWI-Prolog..${NC}"
        install_swi
        swi_prolog_version=$(swipl_version)
        if version_ge "$swi_prolog_version" "$required_version"; then
            echo -e "${GREEN}SWI-Prolog upgraded to $swi_prolog_version, which meets the required version $required_version or higher${NC}."
        else
            echo -e "${YELLOW}Failed to upgrade SWI-Prolog to version $required_version or higher. Janus may not work without this version.${NC}"
        fi
    fi
}


# Ask the user if EASY_INSTALL is still '?'
if [ "$EASY_INSTALL" == "?" ]; then
    if confirm_with_default "Y" "Would you like to use easy installation mode?"; then
        EASY_INSTALL="Y"
else
        EASY_INSTALL="N"
fi
fi


if [[ ! -n "$UPDATE_SYSTEM" ]]; then
    if [ -n "$ALLOW_MODIFY_SYSTEM" ] && [ "$ALLOW_MODIFY_SYSTEM" -eq 1 ]; then
       UPDATE_SYSTEM=1
    else
       UPDATE_SYSTEM=0
    fi
fi


# Define a function to print variables in bold and colorful format
print_var() {
    local var_name=$1
    local var_value=${!var_name:-"${RED} <Not Set> ${NC}"}
    echo -e "${YELLOW}${var_name}${NC}=${BOLD}${BLUE}${var_value}${NC}"
}

# List of variables to print
vars_to_print=("INSTALL_TYPE" "FORCE_REINSTALL_SWI" "SWI_INSTALL" "EASY_INSTALL" 
  "PIP_BREAK_SYSTEM_PACKAGES"  
  "ALLOW_MODIFY_SYSTEM"
  "UPDATE_SYSTEM"
   "RPWD" "MeTTa" "METTALOG_DIR" "SWI_INSTALL_VERSION")

# Iterate over the list and print each variable
for var in "${vars_to_print[@]}"; do
    print_var $var
done

echo -e "${BLUE}Starting the installation process.${NC}."

upgrade_pip() {
    if [ "$UPDATE_SYSTEM" -eq 1 ]; then
        if [ "${EASY_INSTALL}" == "N" ] && confirm_with_default "N" "Upgrade pip to the latest version?"; then

            # Upgrade pip to the latest version
            echo -e "${BLUE}Upgrading pip to the latest version..${NC}."

            # If PIP_BREAK_SYSTEM_PACKAGES is set to 1, allow pip to break system packages
            if [ -n "$PIP_BREAK_SYSTEM_PACKAGES" ] && [ "$PIP_BREAK_SYSTEM_PACKAGES" -eq 1 ]; then
                PIP_CMD="pip install --upgrade pip --break-system-packages"
            else
                PIP_CMD="pip install --upgrade pip"
            fi

            $PIP_CMD
            if [ $? -ne 0 ]; then
                echo -e "${RED}Failed to upgrade pip. Exiting${NC}."
                return 1
            else
                echo -e "${GREEN}pip upgraded successfully${NC}."
            fi
        fi
    fi
}

ensure_venv_and_pip() {
    # Check if Python is installed
    if ! command -v python3 &> /dev/null; then
        echo -e "${RED}Python3 is not installed. Please install Python3 to continue${NC}."
        return 1
    fi

    # Check if venv is installed
    if ! python3 -m venv --help &> /dev/null; then
        echo -e "${YELLOW}Python venv module is not installed. Installing venv..${NC}."
        
        # Only allow system modifications if UPDATE_SYSTEM is set to 1
        if [ "$UPDATE_SYSTEM" -eq 1 ]; then
            sudo apt-get update
            sudo apt-get install -y python3-venv
            if [ $? -ne 0 ]; then
                echo -e "${RED}Failed to install Python venv. Exiting${NC}."
                return 1
            else
                echo -e "${GREEN}Python venv module installed successfully${NC}."
            fi
        else
            echo -e "${RED}System modifications are not allowed. Cannot install Python venv. Exiting${NC}."
            return 1
        fi
    else
        echo -e "${GREEN}Python venv module is already installed${NC}."
fi

    # Check if pip is installed
    if ! command -v pip3 &> /dev/null; then
        echo -e "${YELLOW}pip is not installed. Installing pip..${NC}."

        # Only allow system modifications if UPDATE_SYSTEM is set to 1
        if [ "$UPDATE_SYSTEM" -eq 1 ]; then
            sudo apt-get update
            sudo apt-get install -y python3-pip
            if [ $? -ne 0 ]; then
                echo -e "${RED}Failed to install pip. Exiting${NC}."
                return 1
    else
                echo -e "${GREEN}pip installed successfully${NC}."
    fi
        else
            echo -e "${RED}System modifications are not allowed. Cannot install pip. Exiting${NC}."
            return 1
        fi
    else
        echo -e "${GREEN}pip is already installed${NC}."
    fi

    source ./scripts/ensure_venv
}

ensure_venv_and_pip

# Ensure SWI-Prolog is installed with Janus support
if ! command -v swipl &> /dev/null || ! swipl -g "use_module(library(janus)), halt(0)." -t "halt(1)" 2>/dev/null; then
    if confirm_with_default "Y" "SWI-Prolog is not installed with Janus support. Would you like to install it?"; then
        install_or_update_swipl
    else
        echo -e "${RED}SWI-Prolog installation aborted. Exiting script${NC}."
        return 1
    fi
else
    swi_prolog_version=$(swipl_version)
    required_version="9.3.9"
    if version_ge $swi_prolog_version $required_version; then
        echo -e "${GREEN}SWI-Prolog version $swi_prolog_version is installed and meets the required version $required_version or higher${NC}."
        if [ "$FORCE_REINSTALL_SWI" -eq 1 ]; then
            echo -e "${YELLOW}Forcing update SWI-Prolog..${NC}."
            install_or_update_swipl
            swi_prolog_version=$(swipl_version)
            if version_ge $swi_prolog_version $required_version; then
                echo -e "${GREEN}SWI-Prolog upgraded to $swi_prolog_version, which meets the required version $required_version or higher${NC}."
            else
                echo -e "${YELLOW}Failed to upgrade SWI-Prolog to version $required_version or higher. Janus may not work without this version${NC}."
            fi
        fi
    else
        if confirm_with_default "Y" "SWI-Prolog is not version $required_version or higher. Would you like to update it?"; then
            echo -e "${YELLOW}Attempting to update SWI-Prolog..${NC}."
            install_or_update_swipl
            swi_prolog_version=$(swipl_version)
            if version_ge $swi_prolog_version $required_version; then
                echo -e "${GREEN}SWI-Prolog upgraded to $swi_prolog_version, which meets the required version $required_version or higher${NC}."
            else
                echo -e "${YELLOW}Failed to upgrade SWI-Prolog to version $required_version or higher. Janus may not work without this version${NC}."
            fi
        fi
    fi
fi

# Assuming SWI-Prolog 9.3.9 is installed successfully
# Install Janus Python support for SWI-Prolog
echo -e "${BLUE}Checking if Janus Python support is already installed..${NC}."
if ! swipl -g "use_module(library(janus)), halt(0)." -t "halt(1)" 2>/dev/null; then
    # janus not installed, prompt the user
    if [ "${EASY_INSTALL}" == "Y" ] || confirm_with_default "Y" "Would you like to install Python (Janus) support?"; then
        echo -e "${BLUE}Installing Janus for SWI-Prolog..${NC}."
	    pip install git+https://github.com/SWI-Prolog/packages-swipy.git	    
	    if [ $? -ne 0 ]; then
		echo -e "${RED}Failed to install Janus. Exiting script${NC}."
		return 1
	    else
            echo -e "${GREEN}Janus installed successfully${NC}."
	    fi
    else
        echo -e "${YELLOW}Skipping Janus Python support installation${NC}."
    fi
else
    echo -e "${GREEN}Janus Python support is already installed${NC}."
fi

# Install PySWIP for Python-SWI-Prolog interface
echo -e "${BLUE}Checking if PySWIP is already installed..${NC}."
if ! python3 -c "import pyswip" &> /dev/null; then
    if [ "${EASY_INSTALL}" == "Y" ] || confirm_with_default "Y" "Would you like to install PySWIP?"; then
        echo -e "${BLUE}Installing PySWIP..${NC}."
        ensure_venv_and_pip
        pip install git+https://github.com/logicmoo/pyswip.git
        echo -e "${GREEN}PySWIP installation complete${NC}."
    else
        echo -e "${YELLOW}Skipping PySWIP installation${NC}."
    fi
else
    echo -e "${GREEN}PySWIP is already installed${NC}."
fi

# Optional installation of predicate_streams and logicmoo_utils
if false && ! swipl -g "use_module(library(predicate_streams)), halt(0)." -t "halt(1)" 2>/dev/null; then
    echo -e "${BLUE}Installing predicate_streams..${NC}."
    echo -e "${YELLOW}${BOLD}If asked, say yes to everything and/or accept the defaults..${NC}."
    (
    if [ -d "reqs/predicate_streams" ]; then
        echo -e "${BLUE}predicate_streams directory exists. Pulling updates..${NC}."
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

# Optional installation of logicmoo_utils
if false && ! swipl -g  "use_module(library(logicmoo_utils)), halt(0)." -t "halt(1)" 2>/dev/null; then
    echo -e "${BLUE}Installing logicmoo_utils..${NC}."
    echo -e "${YELLOW}${BOLD}If asked, say yes to everything and/or accept the defaults..${NC}."
    (
    if [ -d "reqs/logicmoo_utils" ]; then
        echo -e "${BLUE}logicmoo_utils directory exists. Pulling updates..${NC}."
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

# Environment file setup for MeTTaLog
env_file="${METTALOG_DIR}/scripts/envvars_mettalog.sh"

# Function to check if metalog is in the user's PATH and update environment variables
# Ensure MeTTaLog is sourced in .bashrc
check_metalog_in_path() {
    # Add sourcing line to .bashrc if it's not already there
    if ! grep -q "$env_file" "${HOME}/.bashrc"; then
        echo ""
        echo -e "${BLUE}MeTTaLog is not in your .bashrc${NC}."
        echo "" >> "${HOME}/.bashrc"
        echo "# Source MeTTaLog environment" >> "${HOME}/.bashrc"
        echo "source \"$env_file\"" >> "${HOME}/.bashrc"
        echo -e "${GREEN}MeTTaLog added to .bashrc${NC}."
    else 
        echo -e "${GREEN}MeTTaLog was already in your .bashrc${NC}."
    fi

    source "$env_file"

    print_var "METTALOG_DIR"
    print_var "PYTHONPATH"
    print_var "PATH"
}

# Call the function to check and update PATH
check_metalog_in_path

echo -e "${GREEN}SWIPL executable is: ${NC}$(which swipl)"

echo -e "${GREEN}Installation and setup complete${NC}!"

# Optionally display README.md
if false && confirm_with_default "N" "Show README.md"; then
    echo -en "${GREEN}"
    cat README.md
    echo -en "${NC}"
fi


cd $RPWD

# End of the script

# ```

