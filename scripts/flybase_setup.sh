#!/bin/bash

export RPWD=$PWD
IS_SOURCED=$( [[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo 1 || echo 0)
# Function to exit the script properly
if [ "$IS_SOURCED" -eq "0" ]; then SCRIPT=$(readlink -f "$0"); else SCRIPT=$(readlink -f "${BASH_SOURCE[0]}"); fi
export MeTTa=$(realpath "$SCRIPT")
export METTALOG_DIR=$(dirname "$SCRIPT")
echo "METTALOG_DIR=$METTALOG_DIR"
# cd "$METTALOG_DIR" || { echo "Failed to navigate to $METTALOG_DIR"; [[ "$IS_SOURCED" == "1" ]] && return 1 || exit 1; }

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


echo -e "${BLUE}Starting the installation process..${NC}."

# Function to compare versions
version_ge() {
    # Compare $1 with $2; if $1 >= $2, return 0 (true), else return 1 (false)
    printf '%s\n%s' "$2" "$1" | sort -VC
    return $?
}

# SWI-Prolog from source
build_swi_prolog_from_src() {

    # Install build dependencies
    echo -e "${BLUE}Installing build dependencies...${NC}"
    local build_deps="build-essential autoconf git libgmp-dev libssl-dev unixodbc-dev \
        libreadline-dev zlib1g-dev libarchive-dev libossp-uuid-dev libxext-dev \
        libice-dev libjpeg-dev libxinerama-dev libxft-dev libxpm-dev libxt-dev \
        pkg-config libdb-dev libpcre3-dev libyaml-dev"
    sudo apt-get update && sudo apt-get install -y $build_deps && {
        echo -e "${GREEN}Build dependencies installed successfully.${NC}"
    } || {
        echo -e "${RED}Failed to install build dependencies. Exiting.${NC}"
        exit 1
    }

    # Clone the SWI-Prolog repository
    echo -e "${BLUE}Cloning SWI-Prolog source code...${NC}"
    rm -rf swipl-devel/
    git clone https://github.com/SWI-Prolog/swipl-devel.git && cd swipl-devel && {
        echo -e "${GREEN}SWI-Prolog source code cloned successfully.${NC}"
    } || {
        echo -e "${RED}Failed to clone SWI-Prolog repository. Exiting.${NC}"
        exit 1
    }

    # Update submodules
    echo -e "${BLUE}Updating submodules...${NC}"
    #git -C . submodule update --init packages/ltx2htm packages/pldoc packages/nlp packages/archive packages/clib packages/http packages/sgml packages/ssl packages/zlib
    git submodule update --init  && {
        echo -e "${GREEN}Submodules updated successfully.${NC}"
    } || {
        echo -e "${RED}Failed to update submodules. Exiting.${NC}"
        exit 1
    }

    # Configure and build
    echo -e "${BLUE}Configuring and building SWI-Prolog...${NC}"
    mkdir build && cd build
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

# Call the build function
# build_swi_prolog_from_src


# Function to install or update SWI-Prolog
install_or_update_swipl() {

    echo -e "${BLUE}Starting SWI-Prolog installation or update.${NC}"
    sudo apt-add-repository -y ppa:swi-prolog/devel
    sudo apt-get update
    # Remove existing installation if any before reinstalling/upgrading
    sudo apt-get remove -y swi-prolog??* 
    sudo apt-get install -y swi-prolog
    swi_prolog_version=$(swipl --version | awk '{print $3}')
    required_version="9.1"
    if version_ge $swi_prolog_version $required_version; then
        echo -e "${GREEN}SWI-Prolog version $swi_prolog_version is installed and meets the required version $required_version or higher.${NC}"
    else
            echo -e "${YELLOW}Attempting to update SWI-Prolog...${NC}"
            build_swi_prolog_from_src
            swi_prolog_version=$(swipl --version | awk '{print $3}')
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
    swi_prolog_version=$(swipl --version | awk '{print $3}')
    required_version="9.1"
    if version_ge $swi_prolog_version $required_version; then
        echo -e "${GREEN}SWI-Prolog version $swi_prolog_version is installed and meets the required version $required_version or higher.${NC}"
    else
        if confirm_with_default "Y" "SWI-Prolog is not version $required_version or higher. Would you like to update it?"; then
            echo -e "${YELLOW}Attempting to update SWI-Prolog...${NC}"
            install_or_update_swipl
            swi_prolog_version=$(swipl --version | awk '{print $3}')
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


# Setting PYTHONPATH environment variable
echo -e "${BLUE}Setting PYTHONPATH environment variable..${NC}."
export PYTHONPATH=$PWD/src:$PYTHONPATH


if confirm_with_default "N" "Setup Flybase Developnent files"; then

# Confirming download of Quick Loadable Flybase files
if [ "${easy_install}" == "Y" ] || confirm_with_default "Y" "Download Quick Loadable Flybase files"; then
    if [ -f whole_flybase.qlf ]; then
        echo -e "${YELLOW}whole_flybase.qlf already exists. Skipping download and extraction.${NC}"
    else
        echo -e "${BLUE}Downloading whole_flybase.qlf...${NC}"
        wget --show-progress https://logicmoo.org/public/metta/data/whole_flybase.qlf.gz && echo "Unzipping..." && gunzip whole_flybase.qlf.gz || { echo -e "${RED}Error in download or unzipping.${NC}" && exit 1; }
        echo -e "${BLUE}Download and unzipping complete.${NC}"
    fi

    # Checking for whole_flybase in README.md
    echo -e "${BLUE}Checking for whole_flybase in README.md...${NC}"
    grep -B 3 -A 4 whole_flybase README.md || { echo -e "${RED}Error or no matches in README.md${NC}" && exit 1; }

    echo -e "${Green}Process completed successfully.${NC}"
    if [ "${easy_install}" == "Y" ]; then
         exit 0
    fi
else
    echo -e "${YELLOW}Download of Quick Loadable Flybase files skipped.${NC}"
fi




echo -e "${BLUE}Allowing user to override FBPC_VERSION..${NC}."
export FBPC_VERSION=$(prompt_for_input "Enter the Flybase version slug (or press <enter> to use this default): " "2023_05")
echo -e "${GREEN}"
set -x
export FBPC_URL="ftp.flybase.org/releases/FB$FBPC_VERSION/precomputed_files/"
export FBPC_LOC="./data/$FBPC_URL"
set +x
echo -e "${NC}"

need_fb_files="Y"
if find "$FBPC_LOC" -mindepth 2 -type f -print -quit | grep -q '.'; then
    need_fb_files="N"
    echo -e -n "${GREEN}Looks like we already have the Flybase data.. \n${BLUE}Really RE-"
else
   echo -e "${YELLOW}Looks like we *need* have the Flybase data..${NC}."
fi

if confirm_with_default "${need_fb_files}" "Download Flybase Release?"; then
    echo -e "${BLUE}You may override the Flybase URL${NC}..."
    export FBPC_URL=$(prompt_for_input "Enter the FBPC URL http://" "${FBPC_URL}")

    echo -e "${BLUE}Downloading necessary files..${NC}."
    wget -c --no-parent -r -P ./data/  --reject="index.htm*"  http://$FBPC_URL

    echo -e "${BLUE}Setting the precomputed location..${NC}."
    export FBPC_LOC=./data/$FBPC_URL

    echo -e "${BLUE}Checking and deleting duplicated files..${NC}."
    find $FBPC_LOC -type f -name '*_fb_*' -exec bash -c 'if [[ -f ${1/_fb_????_??/} ]]; then rm -f ${1/_fb_????_??/}; fi' _ {} \;

    echo -e "${BLUE}Calculating disk usage (should be around 587M)..${NC}."
    du -hs $FBPC_LOC
    echo ""

    echo -e "${BLUE}Decompressing the downloaded files..${NC}."
    find $FBPC_LOC -type f -name "*.gz" -execdir bash -c 'if [ ! -f "${1%.gz}" ]; then gunzip -k "$1"; else echo "File exists, skipping: $1"; fi' bash {} \;
    #find $FBPC_LOC -type f -name "*.zip" -print -execdir unzip -o -d . {} \;
    echo ""

    echo -e "${BLUE}Normalizing identifiers in files..${NC}."
    set -x
    find $FBPC_LOC -name "*.fb" -exec sed -i -e 's/FB:FB/FB/g' {} \;
    find $FBPC_LOC -name "*.json" -exec sed -i -e 's/FLYBASE:FB/FB/g' {} \;
    time find $FBPC_LOC -type f ! -name "*.gz" -exec sed -i -e 's/\(FB[a-z]\{2\}\):\([0-9]\)/\1\2/g' -e 's/[Ff][Ll][Yy][Bb][Aa][Ss][Ee]:\([A-Za-z]\)/\1/g' {} \;
    set +x

fi

echo -e "${BLUE}Checking disk usage for flybase (should be around 7.9G)..${NC}."
du -hs $FBPC_LOC
echo ""


if confirm_with_default "N" "Building the Loadable Files might take around 30 minutes. Do you want to continue"; then
    echo -e "${BLUE}Building the Loadable File..${NC}."
    set -x
    ./MeTTa -G "'!(create-flybase-pl! ${FBPC_LOC})'" -G "'!(halt! 777)'"
    set +x
    echo -e "${BLUE}Loadable File build complete${NC}."


    echo -e "${BLUE}No metta files for:${NC}."
    find ftp.flybase.org -type f ! -name "*.png" ! -name "*.gz" ! -name "*.metta" ! -name "*.datalog" -exec bash -c 'for file; do base="${file%.*}"; [[ -z $(find "$(dirname "$file")" -type f -wholename "$base*etta") ]] && echo "$file"; done' bash {} +

    echo -e "${BLUE}No datalog files for:${NC}."
    find ftp.flybase.org -type f ! -name "*.png" ! -name "*.gz" ! -name "*.zip" ! -name "*.datalog" -exec bash -c 'for file; do base="${file%.*}"; [[ -z $(find "$(dirname "$file")" -type f -wholename "$base*atalog") ]] && echo "$file"; done' bash {} +


    echo -e "${BLUE}Removing ':' in datalog files..${NC}."
    find ./data -type f -name "*.datalog" -print -exec sed -i -e 's/\(FB[a-z]\{2\}\):\([0-9]\)/\1\2/g' -e 's/[Ff][Ll][Yy][Bb][Aa][Ss][Ee]:\([A-Za-z]\)/\1/g' {} \;

    echo -e "${BLUE}Removing duplicate lines in datalog files..${NC}."
    time find ./data -type f -name "*.datalog" -exec sh -c 'awk '\''!seen[$0]++'\'' "$1" > tmpfile && mv -f tmpfile "$1"' sh {} \;

    echo -e "${BLUE}Removing duplicate lines in metta files..${NC}."
    time find ./data -type f -name "*.metta" -exec sh -c 'awk '\''!seen[$0]++'\'' "$1" > tmpfile && mv -f tmpfile "$1"' sh {} \;

    echo -e "${BLUE}Alligning OBO preds.${NC}."
    time find ./data/supplimental/12_ontologies/ -type f -name "*.datalog?*" -print -exec sed -i -e 's/obo-is-a/obo-is-type/g'  {} \;

    echo -e "${BLUE}Alligning OBO preds.${NC}."
    time find ./data -type f -name "*.datalog?*" -print -exec sed -i -e 's/obo-has-definition/obo-def/g' -e 's/obo-Inheritance/obo-is-a/g' -e 's/obo-Member/Member/g' -e 's/obo-has-name/obo-name/g'  {} \;
    #  -e 's/obo-[A-Z][A-z]+-/obo-/g'

    echo -e "${BLUE}Combining all of the datalog files into one big one..${NC}."
    find ./data -mindepth 2 -type f -name "*.datalog" -exec cat ./data/whole_header.datalog {} + > ./data/whole_flybase.metta.datalog

    echo -e "${BLUE}Combining all of the MeTTa files into one big one..${NC}."
    find ./data -mindepth 2 -type f -name "*.metta" -exec cat {} + > ./data/whole_flybase.metta

    echo -e "${BLUE}Removing any duplicates from one big one..${NC}."
    time awk '!seen[$0]++' ./data/whole_flybase.metta.datalog > tmpfile && mv -f tmpfile ./data/whole_flybase.metta.datalog

    echo -e "${BLUE}Removing any duplicates from one big one..${NC}."
    time awk '!seen[$0]++' ./data/whole_flybase.metta > tmpfile && mv -f tmpfile ./data/whole_flybase.metta


else
    echo -e "${BLUE}Skipping the building of the Loadable File${NC}."
fi

ls -lh whole_flybase.datalog2
wc - l whole_flybase.datalog2

if confirm_with_default "Y" "Building the  Quick Load File might take around 30 minutes. Do you want to continue"; then
    echo -e "${BLUE}Building the Quick Load File..${NC}."
    set -x
    time swipl -g "qcompile('data/whole_flybase.datalog2')" -t halt
    set +x
    echo -e "${BLUE}Quick Load File build complete${NC}."
else
    echo -e "${BLUE}Skipping the building of the Quick Load File${NC}."
fi

# Optional Rust Metta loading
if confirm_with_default_no "Would you like to be able to load these Flybase Metta files into Rust Metta"; then
    echo -e "${BLUE}Converting data for Rust Metta..${NC}."
    set -x
    ./scripts/convert_to_metta.sh $FBPC_LOC
    set +x
    echo -e "${BLUE}Counting atoms (should be at least 56 million)..${NC}."
    find $FBPC_LOC -type f -name "*.metta" -exec wc -l {} +
fi

fi

