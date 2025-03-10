#!/bin/bash

###############################################################################
# 1) Function to capture header lines until we see <rdf:Description>
#    Writes them to a temp file, returns "temp_file_path line_count"
###############################################################################
capture_rdf_header() {
    local input_file="$1"
    local tmpfile
    tmpfile="$(mktemp)"
    local line_count=0

    while IFS='' read -r line; do
        # If we see <rdf:Description>, stop capturing
        if [[ "$line" =~ \<rdf:Description.*\> ]]; then
            break
        fi
        echo "$line" >> "$tmpfile"
        ((line_count++))
    done < "$input_file"

    # Print the temp file path and the count of lines
    echo "$tmpfile $line_count"
}

###############################################################################
# 2) Verbose Split with Fancy Progress + Prepending the Header
###############################################################################
split_rdf_by_size() {
    local input_file="$1"
    local chunk_size="$2"

    [[ ! -f "$input_file" ]] && { echo "❌ File '$input_file' not found!"; exit 1; }
    [[ -z "$chunk_size" ]] && { echo "❌ Please specify chunk size (e.g., 1G or 500M)"; exit 1; }

    # Convert chunk size to bytes
    case "$chunk_size" in
        *G) chunk_size_bytes=$(( ${chunk_size%G} * 1073741824 )) ;;  # Convert GB to bytes
        *M) chunk_size_bytes=$(( ${chunk_size%M} * 1048576 )) ;;     # Convert MB to bytes
        *) echo "❌ Invalid chunk size format. Use M for megabytes or G for gigabytes."; exit 1 ;;
    esac

    # Basic file info
    local total_size
    total_size=$(stat -c%s "$input_file")
    local filename
    filename=$(basename -- "$input_file")
    local base_name="${filename%.*}"
    local ext="${filename##*.}"
    local split_dir="${base_name}_split_${ext}"
    mkdir -p "$split_dir"

    # 1) Capture the header lines (before <rdf:Description>)
    #    This returns something like: "/tmp/tmp.XXXX 5"
    local result
    result=($(capture_rdf_header "$input_file"))
    local header_file="${result[0]}"
    local header_lines="${result[1]}"

    echo "🚀 Splitting '$input_file' (~$((total_size/1024/1024/1024)) GB) into ~$chunk_size chunks in '$split_dir/'..."

    # 2) Skip the captured header lines, feed the remainder to awk
    tail -n +$((header_lines + 1)) "$input_file" | awk \
        -v prefix="${split_dir}/${base_name}_part" \
        -v ext="$ext" \
        -v total_size="$total_size" \
        -v chunk_size="$chunk_size_bytes" \
        -v header_file="$header_file" '
      BEGIN {
        quarter = chunk_size / 4;
        emoji[0]="🥚"; emoji[1]="🐣"; emoji[2]="🐥"; emoji[3]="🦅";
        cmd="date +%s"; cmd | getline start_time; close(cmd);

        file_num=1; size=0; dots_printed=0; bytes_processed=0;
        outfile=sprintf("%s_%06d.%s", prefix, file_num, ext);

        # Read the captured header lines into an array
        linecount=0;
        while ((getline line < header_file) > 0) {
          linecount++;
          header_lines[linecount] = line;
        }
        close(header_file);

        # Print the header into the first chunk
        for(i=1; i<=linecount; i++){
          print header_lines[i] > outfile;
        }

        time_str="estimating..."; 
        elapsed_str="0m 0s";
        printf "\r%-110s", emoji[0]" Creating "outfile" "emoji[0]" ("time_str", elapsed: "elapsed_str")"
      }

      {
        print >> outfile;
        line_length=length($0)+1; 
        size+=line_length; 
        bytes_processed+=line_length;

        # Update elapsed/remaining time every ~10MB
        if (bytes_processed % (10*1024*1024) < line_length) {
          cmd="date +%s"; cmd | getline now; close(cmd);
          elapsed=now-start_time; 
          elapsed_str=sprintf("%dm %ds", int(elapsed/60), elapsed%60);
          if (elapsed>0) {
            rate=bytes_processed/elapsed;
            remaining_seconds=int((total_size-bytes_processed)/rate);
            minutes=int(remaining_seconds/60); 
            seconds=remaining_seconds%60;
            time_str=sprintf("%dm %ds remaining", minutes, seconds);
          }
        }

        # Fancy quarter-based progress updates
        if (dots_printed < 1 && size >= quarter) {
          dots_printed=1; 
          printf "\r%-110s", emoji[1]" Creating "outfile" "emoji[1]" ("time_str", elapsed: "elapsed_str")"
        }
        else if (dots_printed < 2 && size >= 2*quarter) {
          dots_printed=2; 
          printf "\r%-110s", emoji[2]" Creating "outfile" "emoji[2]" ("time_str", elapsed: "elapsed_str")"
        }
        else if (dots_printed < 3 && size >= 3*quarter) {
          dots_printed=3; 
          printf "\r%-110s", emoji[3]" Creating "outfile" "emoji[3]" ("time_str", elapsed: "elapsed_str")"
        }

        # Split condition: if we see </rdf:Description> and chunk_size is reached
        if ($0=="</rdf:Description>" && size>=chunk_size) {
          close(outfile);
          printf "\r%-110s ✅\r", emoji[3]" Finished "outfile" "emoji[3]" (elapsed: "elapsed_str")";

          file_num++;
          outfile=sprintf("%s_%06d.%s", prefix, file_num, ext);
          size=0;
          dots_printed=0;

          # Prepend the header into the new file
          for(i=1; i<=linecount; i++){
            print header_lines[i] > outfile;
          }

          printf "\r%-110s", emoji[0]" Creating "outfile" "emoji[0]" ("time_str", elapsed: "elapsed_str")"
        }
      }

      END {
        cmd="date +%s"; cmd | getline now; close(cmd);
        total_time=int(now-start_time);
        elapsed_str=sprintf("%dm %ds", int(total_time/60), total_time%60);

        printf "\r%-110s ✅\r", emoji[dots_printed]" Finished "outfile" "emoji[dots_printed]" (elapsed: "elapsed_str")";
        printf "🎉 Total files created: %d in %s.\n", file_num, elapsed_str
      }
    '

    # Remove the temp file if you like, or keep it for debugging
    rm -f "$header_file"
}

###############################################################################
# 3) Quiet version (no fancy progress), also includes the same header logic
###############################################################################
split_rdf_by_size_quiet() {
    local input_file="$1"
    local chunk_size="$2"

    [[ ! -f "$input_file" ]] && { echo "File '$input_file' not found!"; exit 1; }
    [[ -z "$chunk_size" ]] && { echo "❌ Please specify chunk size (e.g., 1G or 500M)"; exit 1; }

    case "$chunk_size" in
        *G) chunk_size_bytes=$(( ${chunk_size%G} * 1073741824 )) ;;
        *M) chunk_size_bytes=$(( ${chunk_size%M} * 1048576 )) ;;
        *) echo "❌ Invalid chunk size format. Use M for megabytes or G for gigabytes."; exit 1 ;;
    esac

    local total_size
    total_size=$(stat -c%s "$input_file")
    local filename
    filename=$(basename -- "$input_file")
    local base_name="${filename%.*}"
    local ext="${filename##*.}"
    local split_dir="${base_name}_split_${ext}"
    mkdir -p "$split_dir"

    # Capture the header lines
    local result
    result=($(capture_rdf_header "$input_file"))
    local header_file="${result[0]}"
    local header_lines="${result[1]}"

    # Skip those lines in the main stream
    tail -n +$((header_lines + 1)) "$input_file" | awk \
        -v prefix="${split_dir}/${base_name}_part" \
        -v ext="$ext" \
        -v chunk_size="$chunk_size_bytes" \
        -v header_file="$header_file" '
      BEGIN {
        file_num = 1;
        size = 0;
        outfile = sprintf("%s_%06d.%s", prefix, file_num, ext);

        # Read the header lines
        linecount = 0;
        while ((getline line < header_file) > 0) {
          linecount++;
          header_lines[linecount] = line;
        }
        close(header_file);

        # Prepend them into the first chunk
        for(i=1; i<=linecount; i++){
          print header_lines[i] > outfile;
        }
      }
      {
        print >> outfile;
        size += length($0) + 1;

        if ($0=="</rdf:Description>" && size >= chunk_size) {
          close(outfile);
          file_num++;
          outfile = sprintf("%s_%06d.%s", prefix, file_num, ext);
          size = 0;

          # Prepend the header
          for(i=1; i<=linecount; i++){
            print header_lines[i] > outfile;
          }
        }
      }
      END {
        printf "Total files created: %d\n", file_num
      }
    '

    rm -f "$header_file"
}

###############################################################################
# 4) Usage / Argument Parsing
###############################################################################
usage() {
    echo "Usage: $0 [-q] <filename.rdf> <chunk_size (e.g., 1G or 500M)>"
    exit 1
}

quiet_mode=""
while getopts ":q" opt; do
  case ${opt} in
    q ) quiet_mode="true" ;;
    \? ) usage ;;
  esac
done
shift $((OPTIND -1))

[[ -z "$1" || -z "$2" ]] && usage

if [[ "$quiet_mode" == "true" ]]; then
    split_rdf_by_size_quiet "$1" "$2"
else
    split_rdf_by_size "$1" "$2"
fi

