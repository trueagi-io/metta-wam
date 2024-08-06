#!/bin/bash

# Check if the script was sourced or executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # ANSI escape code for yellow foreground
    yellow='\033[1;33m'
    # Reset color
    NC='\033[0m'

    echo -e "${yellow}This script should be sourced to work correctly."
    echo -e "Please use '. ${BASH_SOURCE[0]}' \n   or 'source ${BASH_SOURCE[0]}' instead.${NC}"
    exit 1
fi


# Resolve the actual directory of the current script, following symlinks
SCRIPT=$(readlink -f "${BASH_SOURCE[0]}")
SCRIPT_DIR=$(dirname "$SCRIPT")
# Set METTALOG_DIR to one level above the script's directory
METTALOG_DIR=$(dirname "$SCRIPT_DIR")

export METTALOG_DIR

# Extend the PATH and PYTHONPATH environment variables

# Check if METTALOG_DIR is already in PATH, if not, add it
if [[ ":$PATH:" != *":$METTALOG_DIR:"* ]]; then
    export PATH=$PATH:${METTALOG_DIR}
fi

add_py_dirs_to_pythonpath() {
    local dir="$1"  # The initial directory to search
    if [[ -z "$dir" ]]; then
        echo "Usage: add_py_dirs_to_pythonpath <directory>"
        return 1
    fi

    # Convert existing PYTHONPATH into an array, removing duplicates
    IFS=':' read -r -a existing_dirs <<< "$PYTHONPATH"
    declare -A unique_dirs
    for d in "${existing_dirs[@]}"; do
        if [[ $d != .* ]] && [[ $d != *~* ]] && [[ -d $d ]]; then  # Ensure directories are not hidden or backup
            unique_dirs["$d"]=1
        fi
    done

    # Find directories containing .py files, excluding directories that are hidden or contain a tilde
    while IFS= read -r -d '' d; do
        # Skip directories that are hidden, contain a tilde, or are subdirectories of already-added directories
        if [[ $d =~ /.* ]] || [[ $d == .* ]] || [[ $d == *~* ]]; then
            continue
        fi
        local skip_dir=false
        for added_dir in "${!unique_dirs[@]}"; do
            if [[ $d == $added_dir* ]]; then
                skip_dir=true
                break
            fi
        done
        if $skip_dir; then
            continue
        fi
        # Check if the directory contains Python files
        local py_file_count=$(find "$d" -maxdepth 1 -type f -name "*.py" | wc -l)
        if [[ $py_file_count -gt 0 ]]; then
            unique_dirs["$d"]=1
        fi
    done < <(find "$dir" -type d -print0 | sort -uz)

    # Rebuild PYTHONPATH from unique directories
    PYTHONPATH=""
    for d in "${!unique_dirs[@]}"; do
        PYTHONPATH="${PYTHONPATH:+$PYTHONPATH:}$d"
    done    
}

# Resolve the absolute path and pass it to the function
#add_py_dirs_to_pythonpath "$(realpath "$METTALOG_DIR/../hyperon-experimental/python/sandbox")"
#add_py_dirs_to_pythonpath "$(realpath "$METTALOG_DIR/tests/")"


# Prepend METTALOG_DIR/src to PYTHONPATH if it's not already included
if [[ ":$PYTHONPATH:" != *":${METTALOG_DIR}/src:"* ]]; then
    PYTHONPATH="${METTALOG_DIR}/src${PYTHONPATH:+:$PYTHONPATH}"
fi

# Prepend METTALOG_DIR/tests/python_compat/metta-motto to PYTHONPATH if it's not already included
if [[ ":$PYTHONPATH:" != *":${METTALOG_DIR}/tests/python_compat/metta-motto:"* ]]; then
    PYTHONPATH="${METTALOG_DIR}/tests/python_compat/metta-motto${PYTHONPATH:+:$PYTHONPATH}"
fi

#echo "Updated PYTHONPATH: $PYTHONPATH"
# Optionally, print the values to verify they are set (you can remove these lines in production)
# echo "METTALOG_DIR=$METTALOG_DIR"
# echo "PATH=$PATH"
echo "PYTHONPATH=$PYTHONPATH"


_mettalog_autocomplete() {
    local cur prev opts files_and_dirs    
    COMPREPLY=() # Array variable storing the possible completions.
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"
    # echo "COMP_WORDS: ${COMP_WORDS[@]} COMP_CWORD: ${COMP_CWORD} cur: ${cur} prev: ${prev}" > /dev/null
#            -g -t -G -T
#        --abi-version --help --version --arch --dump-runtime-variables
#        -c -O --code=
#        -o    --home=    --quiet --prolog   --breakable   --debug 
    opts="
        --log --html
        --repl --stack-limit=30G --table-space=20G 
	--on-fail=repl
	--e=trace
        --shared-table-space=2000M --pce=false --packs --pldoc=8040 --python=false --tty=false 
        --test --fresh --clean 
	--failures --regressions --continue 
        --timeout=90
	--docker=false
        --exec=skip --eval=debug --case=debug --signals=false --threads=false 
	--output=./
	--v=src/
        ---eval=nodebug
        --debug-on-interrupt=false
        --on-error= 
        --on-warning="

    compopt -o nospace
    

    if [[ ${cur} == --v=* ]]; then
	local was_out_path="$METTALOG_DIR/src/"
	local subdirs=$(find "${was_path}" -mindepth 1 -maxdepth 1 -type d)
	# Remove the path prefix from each subdir for the completion
	COMPREPLY=( $(compgen -W "${subdirs//"$was_out_path"/}" -- "${cur#--v=}") )
	return 0
    fi

    if [[ ${cur} == --output=* ]]; then
	local was_out_path="."
	local subdirs=$(find "${was_path}" -mindepth 1 -maxdepth 1 -type d)
	# Remove the path prefix from each subdir for the completion
	COMPREPLY=( $(compgen -W "${subdirs//"$was_out_path"/}" -- "${cur#--output=}") )
	return 0
    fi

    # Handle comppleion for -g -t 
    if [[ ${prev} == "-g" || ${prev} == "-t" ]]; then
        COMPREPLY=( '"repl"' )
        return 0
    fi    
    
    if [[ ${prev} == "-G" || ${prev} == "-T" ]]; then
        COMPREPLY=( "'\"(REPL!)\"'" )
        return 0
    fi

    # Example adjustment for directory completion
    if [[ ${prev} == "-d" || ${prev} == "--directory" ]]; then
        local IFS=$'\n' # Change IFS to handle spaces in directory names
        # Generate directory completions and append a slash to each
        local dirs=$(compgen -d -- "${cur}" | sed 's/$/\//')
        COMPREPLY=( $dirs )
        return 0
    fi
    
    # Handle file completion after --test 
    if [[ ${prev} == "--test" || ${prev} == "--fresh" ]]; then
        COMPREPLY=( $(compgen -W "--clean --continue --failures --regressions" -- ${cur}) )
        return 0
    fi
    # Handle file completion (no directory) for -x -f -F -l -s --rc
    if [[ ${prev} == "-x" || ${prev} == "-l" || ${prev} == "-s" || ${prev} == "--rc" ]]; then
        _filedir -f # Bash's internal function for file completion.
        return 0
    fi     
    if [[ ${prev} == "-f" || ${prev} == "-F" ]]; then
        _filedir -f # Bash's internal function for file and directory completion.
        return 0
    fi

    if [[ ${prev} == "-L" ]]; then 
        # only allow .metta files and directories possibly containing .metta files
        _filedir -f "./*.metta"
        return 0
    fi

    if [[ "$cur" == --home=* ]]; then
        # Extract the part after '=' to complete it as a directory
        local cur_dir="${cur#*=}"
        local dir_completions=$(compgen -d -- "$cur_dir" | sed 's/$/\//')

        # Append each directory completion with --home= prefix, avoiding space
        COMPREPLY=( $(printf "%s\n" "${dir_completions[@]}" | sed -e "s|^|--home=|") )
        
        # Ensure COMPREPLY items are treated as single items
        compopt -o nospace
        return 0
    fi

    if [[ ${cur} == -* ]]; then
        compopt -o nospace
        COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )        
        return 0
    fi

    if [[ ${cur} == =* ]]; then
        COMPREPLY=( "true" "false" "debug" "silent" "show" "verbose")
        return 0
    fi

    # Handle general case where options are not being completed,
    # allowing completion of both files and directories as well as all other options.
    # first get a list of files in the current directory
    
    # then filter out the files that match the file mask "*.metta" 
    COMPREPLY1=( $(compgen -G "${cur}*.metta" -- ${cur}) )
    # use $files_and_dirs to inumerate the directories make sure they end with a slash
    # and will be used like ( $(compgen -W "${files_and_dirs}" -- ${cur}) )
    files_and_dirs=$(ls -d */ 2>/dev/null)
    # then filter out the directories
    COMPREPLY2=( $(compgen -G "${files_and_dirs}" -- ${cur}) )
    COMPREPLY2=( $(compgen -d -- "$cur" | sed 's/$/\//') )
    # then filter out the options
    COMPREPLY3=( $(compgen -W "${opts}" -- ${cur}) )
    # merge the completions
    COMPREPLY=( "${COMPREPLY1[@]}" "${COMPREPLY2[@]}" "${COMPREPLY3[@]}" )
    return 0
}

# Register the autocomplete function for mettalog command.
complete -F _mettalog_autocomplete mettalog
complete -F _mettalog_autocomplete MeTTaLog
complete -F _mettalog_autocomplete MeTTa
