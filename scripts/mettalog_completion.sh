#!/bin/bash


[ -f /etc/bash_completion ] && source /etc/bash_completion

_mettalog() {

    ########################################################################
    # 1) ARRAYS

    # 1a) Short options -> placeholders we want to show in the listing.
    declare -A short_opt_placeholder=(
      ["-i"]="<include.incl>"
      ["-s"]="<script.pl>"
      ["-f"]="<file.pl>"
      ["-S"]="<script.metta>"
      ["-F"]="<init.pl>"
      ["-x"]="<local,wam>"
      ["-l"]="<load.pro>"
      ["-g"]="\"<goal>\""
      ["-t"]="'halt'"
      ["-G"]="\"(metta goal)\""
      ["-L"]=""
      ["-I"]=""
    )

    # 1b) Short options -> file/directory completions once a space is typed.
    declare -A short_opt_file_extension_or_dir=(
      ["-i"]="incl"
      ["-s"]="pl"
      ["-f"]="pl"
      ["-S"]="metta"
      ["-F"]="pl"
      ["-x"]="wam"
      ["-l"]="pro"
      ["-I"]="/"     # directories only
      ["-L"]="/"     # directories only
    )

    # 1c) Some short flags with no arguments at all (optional).
    local short_flags_no_args=(
      "-O"
      "-p"
    )

    ########################################################################
    # 2) LONG OPTIONS

    # 2a) Long no-argument options
    local long_noarg_opts=(
        "--help" "--version" "--traditional" "--abi-version" "--arch"
        "--repl" "--test" "--continue" "--failures" "--regress" "--fresh"
        "--no-regen" "--clean" "--skip-tests"
        "--prolog"

        "--v=prolog/metta_lang"
    )

    # 2b) Enumerated
    declare -A enum_opts=(
        ["--vn"]="true false auto"
        ["--exec"]="true skip debug"
        ["--eval"]="nodebug debug"
        ["--case"]="nodebug debug"
        ["--markdown"]="false true"
        ["--docker"]="false true"
        ["--e"]="notrace trace"

        ["--compat"]="false true"
        ["--compatio"]="false true"
        ["--repeats"]="true false"
        ["--time"]="false true"
        ["--top-self"]="true false auto"
        ["--answer-format"]="show rust silent detailed"
        ["--transpiler"]="silent verbose"
        ["--log"]="unset false true"
        ["--devel"]="false true"
        ["--synth-unit-tests"]="false true"
        ["--optimize"]="true false"
        ["--compile"]="false full true"
        ["--tabling"]="auto true false"
        ["--trace-on-eval"]="false true"
        ["--trace-on-load"]="silent verbose"
        ["--trace-on-exec"]="false silent verbose"
        ["--trace-on-error"]="non-type false true"
        ["--trace-on-fail"]="false true"
        ["--trace-on-test"]="true silent false verbose"
        ["--repl-on-error"]="false true"
        ["--repl-on-fail"]="false true"
        ["--on-fail"]="continue repl halt"
        ["--exit-on-fail"]="false true"
        ["--rrtrace"]="false true"

        ["--table-space"]="2G 20G 30M 1000k"
        ["--shared-table-space"]="2G 20G 30M 1000k"
        ["--stack-limit"]="2G 30G 30M 1000k"

        ["--pldoc"]="none 8040"
        ["--service"]="none http://metta.singularitynet.io"
        ["--port"]="none 3023"
        ["--telnet"]="44440 none"

        ["--timeout"]="inf 60"
        ["--rust-timeout"]="60 inf"
        ["--initial-result-count"]="10 inf 1"
        ["--max-result-count"]="inf 10 1"
        ["--stack-max"]="500 inf 1000 10000"
        ["--trace-length"]="500 inf"
        ["--trace-on-overtime"]="4.0 inf"
        ["--trace-on-overflow"]="1000 inf"

        # Newer
        ["--threads"]="true false"
        ["--signals"]="true false"
        ["--debug-on-interrupt"]="true false"
        ["--pce"]="true false"
        ["--packs"]="true false"
        ["--python"]="true false"
        ["--tty"]="true false"
        ["--quiet"]="false true"
        ["--report"]="N Y"
        ["--html"]="false true"
        ["--repl"]="false true"

        # Multi-value enumerations
        ["--on-error"]="print halt status"
        ["--on-warning"]="print halt status"
    )

    # 2c) Directory-only & File-only are now **associative** with defaults
    declare -A dir_only_opts=(
      ["--output"]="./reports"
      ["--cachedir"]="~/.cache"
      ["--home"]="/usr/local/lib/swipl"
    )

    declare -A file_only_opts=(
      ["--rc"]="~/.mettarc"
    )

    ########################################################################
    # 3) INIT
    ########################################################################
    local old_cwb="$COMP_WORDBREAKS"
    COMP_WORDBREAKS="${COMP_WORDBREAKS//=/}"

    local cur prev words cword
    _init_completion || return

    ########################################################################
    # 4) HELPER FUNCTIONS

    # 4a) Directory completion, keep `..`, exclude hidden & ~
    _complete_dir_only() {
        local partial="$1"
        local IFS=$'\n'

        # 1) Gather directories from `compgen -d`
        local dirs=( $(compgen -d -- "$partial") )
        local slashDirs=()
    
        # 2) Append a slash at the end, so directories appear as `dir/`
        for d in "${dirs[@]}"; do
            [[ "$d" != */ ]] && d+="/"
            slashDirs+=( "$d" )
        done

        # 3) Filter out “hidden” directories except `.` or `..`
        #    i.e., exclude `.git`, `.config`, etc., but keep `./` and `../`.
        slashDirs=( $(
            printf '%s\n' "${slashDirs[@]}" \
            | grep -vE '(^|/)\.[^./]|~'
        ) )

        COMPREPLY=( "${slashDirs[@]}" )
    }

    # 4b) File-only, exclude hidden & ~
    _complete_file_only() {
        local partial="$1"
        local IFS=$'\n'
        local files=( $(compgen -f -- "$partial") )
        local visible=()
        for f in "${files[@]}"; do
            [[ "$f" =~ (^|/)\.[^.]|~ ]] && continue
            visible+=( "$f" )
        done
        COMPREPLY=( "${visible[@]}" )
    }

    # 4c) Extension-based (for short options)
    _complete_extensions_or_dir() {
        local partial="$1"
        local exts="$2"

        if [[ "$exts" == "/" ]]; then
            _complete_dir_only "$partial"
            return
        fi

        local IFS=$'\n'
        local patterns=( $exts )
        local all=( $(compgen -f -- "$partial") )
        local visible=()

        for f in "${all[@]}"; do
            [[ "$f" =~ (^|/)\.[^.]|~ ]] && continue
            visible+=( "$f" )
        done

        local matched=()
        for vf in "${visible[@]}"; do
            for ext in "${patterns[@]}"; do
                if [[ "$vf" =~ \.$ext$ ]]; then
                    matched+=( "$vf" )
                    break
                fi
            done
        done
        COMPREPLY=( "${matched[@]}" )
    }


    # Debug info
    #echo -e "\nDEBUG: cur='$cur' prev='$prev' words=(${words[*]}) cword=$cword COMP_LINE='$COMP_LINE' COMP_POINT=$COMP_POINT\n"

    ########################################################################
    # 5) CHECK PREVIOUS TOKEN

    # If PREV is in short_opt_file_extension_or_dir => file/dir completions
    if [[ -n "${short_opt_file_extension_or_dir[$prev]+_exists}" ]]; then
        _complete_extensions_or_dir "$cur" "${short_opt_file_extension_or_dir[$prev]}"
        return 0
    fi
    
    
    # (Other checks if PREV is in short_opt_placeholder, etc.)
    if [[ -n "${short_opt_placeholder[$prev]+_exists}" ]] \
       && [[ -z "${short_opt_file_extension_or_dir[$prev]+_exists}" ]]; then
        compopt -o nospace
        COMPREPLY=( "${short_opt_placeholder[$prev]}" )
        complete_extensions_or_dir "$cur" "${short_opt_file_extension_or_dir[$prev]}"
        return 0
    fi


    # 5a) If PREV is in short_opt_file_extension_or_dir => file/dir completions
    if [[ -n "${short_opt_file_extension_or_dir[$prev]+_exists}" ]]; then
        _complete_extensions_or_dir "$cur" "${short_opt_file_extension_or_dir[$prev]}"
        COMP_WORDBREAKS="$old_cwb"
        return 0
    fi

    # 5b) If PREV is in short_opt_placeholder but not file extension => textual placeholder
    if [[ -n "${short_opt_placeholder[$prev]+_exists}" ]] \
       && [[ -z "${short_opt_file_extension_or_dir[$prev]+_exists}" ]]; then
        compopt -o nospace
        COMPREPLY=( "${short_opt_placeholder[$prev]}" )
        COMP_WORDBREAKS="$old_cwb"
        return 0
    fi

    # 5c) If user typed `--something =` but it split as `prev=--something` and `cur==`
    #     => treat it like `--something=`, do directory completion if `--something` is in dir_only_opts.
    if [[ -n "${dir_only_opts[$prev]+_exists}" && "$cur" == "=" ]]; then
        # e.g. user typed: `mettalog --output =`
        _complete_dir_only ""
    
        # 2) Don't add a trailing space (we want --some-dir-opt=dir/)
        compopt -o nospace
    
        # 3) Prepend --some-dir-opt= to each directory match
        COMPREPLY=( "${COMPREPLY[@]/#/${prev}=}" )
        COMP_WORDBREAKS="$old_cwb"
        return 0
    fi

    ########################################################################
    # 6) MAIN CASE
    ########################################################################
    case "$cur" in

        ####################################################################
        # A) --option=value
        ####################################################################
        --*=*)
            local optionName="${cur%%=*}"   # e.g. --output
            local partialVal="${cur#*=}"    # e.g. (whatever after =)

            # 1) If `optionName` is in dir_only_opts => do directory completion
            if [[ -n "${dir_only_opts[$optionName]+_exists}" ]]; then
                local defaultVal="${dir_only_opts[$optionName]}"

                if [[ -z "$partialVal" ]]; then
                    # user typed `--output=` with NO partial => insert default
                    COMPREPLY=( "${optionName}=${defaultVal}" )
                    compopt -o nospace
                    return 0
                else
                    # user typed something => normal directory completion
                    _complete_dir_only "$partialVal"

                    if [[ ${#COMPREPLY[@]} -eq 0 ]]; then
                        : # compopt -o nofilenames
                    else
                        compopt -o nospace
                    fi
                    # Prepend `--output=` etc. to each match
                    COMPREPLY=( "${COMPREPLY[@]/#/${optionName}=}" )
                    return 0
                fi
            fi

            # 2) If `optionName` is in file_only_opts => do file completion
            if [[ -n "${file_only_opts[$optionName]+_exists}" ]]; then
                local defaultVal="${file_only_opts[$optionName]}"

                if [[ -z "$partialVal" ]]; then
                    # user typed `--input=` with NO partial => insert default
                    COMPREPLY=( "${optionName}=${defaultVal}" )
                    compopt -o nospace
                    return 0
                else
                    # user typed something => normal file completion
                    _complete_file_only "$partialVal"

                    if [[ ${#COMPREPLY[@]} -eq 0 ]]; then
                        : # compopt -o nofilenames
                    else
                        compopt -o nospace
                    fi
                    COMPREPLY=( "${COMPREPLY[@]/#/${optionName}=}" )
                    return 0
                fi
            fi

            # 3) enumerated?
            if [[ -n "${enum_opts[$optionName]+_exists}" ]]; then
                local suggestions=( $(compgen -W "${enum_opts[$optionName]}" -- "$partialVal") )
                local newReply=()
                for s in "${suggestions[@]}"; do
                    newReply+=( "${optionName}=${s}" )
                done
                COMPREPLY=( "${newReply[@]}" )
                return 0
            fi

            # 4) Otherwise fallback => let's do file completion by default
            compopt -o nospace
            _complete_file_only "$partialVal"
            COMPREPLY=( "${COMPREPLY[@]/#/${optionName}=}" )
            return 0
            ;;

        ####################################################################
        # B) --option (no '=')
        ####################################################################
        --*)
            local enumerated_expansions=()
            for key in "${!enum_opts[@]}"; do
                if [[ "$key" == "$cur"* ]]; then
                    local values=( ${enum_opts[$key]} )
                    if [[ ${#values[@]} -ge 2 ]]; then
                        enumerated_expansions+=( "${key}=${values[1]}" )
                    else
                        enumerated_expansions+=( "${key}=${values[0]}" )
                    fi
                fi
            done

            local noarg_expansions=()
            for opt in "${long_noarg_opts[@]}"; do
                [[ "$opt" == "$cur"* ]] && noarg_expansions+=( "$opt" )
            done

            # If user typed something that starts with a dir_only_opt, suggest `--opt=default`
            for dop in "${!dir_only_opts[@]}"; do
                if [[ "$dop" == "$cur"* ]]; then
                    noarg_expansions+=( "${dop}=${dir_only_opts[$dop]}" )
                fi
            done

            # If user typed something that starts with a file_only_opt, suggest `--opt=default`
            for fop in "${!file_only_opts[@]}"; do
                if [[ "$fop" == "$cur"* ]]; then
                    noarg_expansions+=( "${fop}=${file_only_opts[$fop]}" )
                fi
            done

            local combined=( "${enumerated_expansions[@]}" "${noarg_expansions[@]}" )

            # If there's exactly 1 match => insert directly
            if [[ ${#combined[@]} -eq 1 ]]; then
                # if that single match ends with '=', skip trailing space
                if [[ "${combined[0]}" == *= ]]; then
                    compopt -o nospace
                fi
                COMPREPLY=( "${combined[0]}" )
            else
                # Multiple expansions: optionally skip trailing space for all that end with '='
                compopt -o nospace
                COMPREPLY=( $(compgen -W "${combined[*]}" -- "$cur") )
            fi

            COMP_WORDBREAKS="$old_cwb"
            return 0
            ;;

        ####################################################################
        # C) SHORT OPTIONS or '-'
        ####################################################################
        -[a-zA-Z]*|-)
            local combined_list=()

            # 1) From short_opt_placeholder
            for so in "${!short_opt_placeholder[@]}"; do
                combined_list+=( "$so ${short_opt_placeholder[$so]}" )
            done

            # 2) From short_flags_no_args
            for flag in "${short_flags_no_args[@]}"; do
                if [[ -z "${short_opt_placeholder[$flag]+_exists}" ]]; then
                    combined_list+=( "$flag" )
                fi
            done

            # 3) From short_opt_file_extension_or_dir
            for sod in "${!short_opt_file_extension_or_dir[@]}"; do
                if [[ -z "${short_opt_placeholder[$sod]+_exists}" ]]; then
                    local exts="${short_opt_file_extension_or_dir[$sod]}"
                    if [[ "$exts" == "/" ]]; then
                        combined_list+=( "$sod \"dir/\"" )
                    else
                        local ext1="${exts%% *}"
                        combined_list+=( "$sod \"file.$ext1\"" )
                    fi
                fi
            done

            # Deduplicate
            declare -A seen
            local final_list=()
            for item in "${combined_list[@]}"; do
                if [[ -z "${seen[$item]+_exists}" ]]; then
                    seen["$item"]=1
                    final_list+=( "$item" )
                fi
            done

            # Partial match
            local matches=()
            for entry in "${final_list[@]}"; do
                local shortName="${entry%% *}"
                if [[ "$shortName" == "$cur"* ]]; then
                    matches+=( "$entry" )
                fi
            done

            if [[ ${#matches[@]} -eq 0 ]]; then
                matches=( "${final_list[@]}" )
            fi

            COMPREPLY=( "${matches[@]}" )
            COMP_WORDBREAKS="$old_cwb"
            return 0
            ;;
    esac

    ########################################################################
    # 7) FALLBACK: directories + *.metta + short & long options
    ########################################################################
    COMP_WORDBREAKS="$old_cwb"

    # 7a) Gather directories
    local IFS=$'\n'
    local dirs=( $(compgen -d -- "$cur") )
    local slashDirs=()
    for d in "${dirs[@]}"; do
        [[ "$d" != */ ]] && d+="/"
        slashDirs+=( "$d" )
    done

    # By default, let's not filter out hidden dirs here.
    # If you want to exclude hidden, re-enable something like:
    #   | grep -vE '(^|/)\.([^.]|$)|~'
    slashDirs=( $(
        printf '%s\n' "${slashDirs[@]}"  # | grep -vE '(^|/)\.([^.]|$)|~'
    ) )

    # 7b) Gather *.metta files
    local allf=( $(compgen -f -- "$cur") )
    local vis=()
    for f in "${allf[@]}"; do
        [[ "$f" =~ (^|/)\.[^.]|~ ]] && continue
        vis+=( "$f" )
    done

    local metta_files=( $(
        printf '%s\n' "${vis[@]}" \
        | grep -E '\.metta$'
    ) )

    # 7c) Gather short options
    local short_matches=()

    # 1) from short_opt_placeholder
    for so in "${!short_opt_placeholder[@]}"; do
        short_matches+=( "$so ${short_opt_placeholder[$so]}" )
    done

    # 2) from short_flags_no_args
    for fl in "${short_flags_no_args[@]}"; do
        if [[ -z "${short_opt_placeholder[$fl]+_exists}" ]]; then
            short_matches+=( "$fl" )
        fi
    done

    # 3) from short_opt_file_extension_or_dir
    for sfd in "${!short_opt_file_extension_or_dir[@]}"; do
        if [[ -z "${short_opt_placeholder[$sfd]+_exists}" ]]; then
            local exts="${short_opt_file_extension_or_dir[$sfd]}"
            if [[ "$exts" == "/" ]]; then
                short_matches+=( "$sfd \"dir/\"" )
            else
                local first_ext="${exts%% *}"
                short_matches+=( "$sfd \"file.$first_ext\"" )
            fi
        fi
    done

    # Deduplicate short
    declare -A seenShort
    local final_short=()
    for item in "${short_matches[@]}"; do
        if [[ -z "${seenShort[$item]+_exists}" ]]; then
            seenShort["$item"]=1
            final_short+=( "$item" )
        fi
    done

    final_short=()

    # 7d) Gather long options
    local final_long=()
    # enumerated expansions
    for key in "${!enum_opts[@]}"; do
        final_long+=( "$key" )
    done
    # no-arg expansions
    for opt in "${long_noarg_opts[@]}"; do
        final_long+=( "$opt" )
    done

    # Deduplicate
    declare -A seenLong
    local final_long_clean=()
    for item in "${final_long[@]}"; do
        if [[ -z "${seenLong[$item]+_exists}" ]]; then
            seenLong["$item"]=1
            final_long_clean+=( "$item" )
        fi
    done
    final_long_clean=()

    # 7e) Combine them all
    COMPREPLY=( "${slashDirs[@]}" "${metta_files[@]}" "${final_short[@]}" "${final_long_clean[@]}" )
    return 0
}

##############################################################################
# 9) REGISTER
##############################################################################
complete -r mettalog 2>/dev/null
complete -o nospace -F _mettalog mettalog
