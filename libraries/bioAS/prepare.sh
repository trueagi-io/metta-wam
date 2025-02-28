#!/bin/bash

split_rdf_by_size() {
    local input_file="$1"
    local chunk_size="$2"
    [[ ! -f "$input_file" ]] && { echo "❌ File '$input_file' not found!"; exit 1; }
    [[ -z "$chunk_size" ]] && { echo "❌ Please specify chunk size (e.g., 1G or 500M)"; exit 1; }

    # Convert chunk size to bytes
    case "$chunk_size" in
        *G) chunk_size_bytes=$(( ${chunk_size%G} * 1073741824 )) ;;  # Convert GB to bytes
        *M) chunk_size_bytes=$(( ${chunk_size%M} * 1048576 )) ;;  # Convert MB to bytes
        *) echo "❌ Invalid chunk size format. Use M for megabytes or G for gigabytes."; exit 1 ;;
    esac

    local total_size=$(stat -c%s "$input_file")
    local filename=$(basename -- "$input_file")
    local base_name="${filename%.*}"
    local ext="${filename##*.}"
    local split_dir="${base_name}_split_${ext}"
    mkdir -p "$split_dir"

    echo "🚀 Splitting '$input_file' (~$((total_size/1024/1024/1024)) GB) into ~$chunk_size chunks in '$split_dir/'..."

    awk -v prefix="${split_dir}/${base_name}_part" -v ext="$ext" -v total_size="$total_size" -v chunk_size="$chunk_size_bytes" '
      BEGIN { 
        quarter = chunk_size / 4;
        emoji[0]="🥚"; emoji[1]="🐣"; emoji[2]="🐥"; emoji[3]="🦅";
        cmd="date +%s"; cmd | getline start_time; close(cmd);
        file_num=1; size=0; dots_printed=0; bytes_processed=0;
        outfile=sprintf("%s_%02d.%s", prefix, file_num, ext);
        time_str="estimating..."; elapsed_str="0m 0s"
        printf "\r%-110s", emoji[0]" Creating "outfile" "emoji[0]" ("time_str", elapsed: "elapsed_str")"
      }
      {
        print >> outfile;
        line_length=length($0)+1; size+=line_length; bytes_processed+=line_length;

        if(bytes_processed % (10*1024*1024) < line_length){
          cmd="date +%s"; cmd | getline now; close(cmd);
          elapsed=now-start_time; 
          elapsed_str=sprintf("%dm %ds", int(elapsed/60), elapsed%60);
          if(elapsed>0){
            rate=bytes_processed/elapsed;
            remaining_seconds=int((total_size-bytes_processed)/rate);
            minutes=int(remaining_seconds/60); seconds=remaining_seconds%60;
            time_str=sprintf("%dm %ds remaining", minutes, seconds);
          }
        }

        if (dots_printed < 1 && size >= quarter) {
          dots_printed=1; printf "\r%-110s", emoji[1]" Creating "outfile" "emoji[1]" ("time_str", elapsed: "elapsed_str")"
        }
        else if (dots_printed < 2 && size >= 2*quarter) {
          dots_printed=2; printf "\r%-110s", emoji[2]" Creating "outfile" "emoji[2]" ("time_str", elapsed: "elapsed_str")"
        }
        else if (dots_printed < 3 && size >= 3*quarter) {
          dots_printed=3; printf "\r%-110s", emoji[3]" Creating "outfile" "emoji[3]" ("time_str", elapsed: "elapsed_str")"
        }

        if ($0=="</rdf:Description>" && size>=chunk_size) {
          close(outfile);
          printf "\r%-110s ✅\n", emoji[3]" Finished "outfile" "emoji[3]" (elapsed: "elapsed_str")";
          file_num++; outfile=sprintf("%s_%02d.%s", prefix, file_num, ext);
          size=dots_printed=0; 
          printf "\r%-110s", emoji[0]" Creating "outfile" "emoji[0]" ("time_str", elapsed: "elapsed_str")"
        }
      }
      END {
        cmd="date +%s"; cmd | getline now; close(cmd);
        total_time=int(now-start_time);
        elapsed_str=sprintf("%dm %ds", int(total_time/60), total_time%60);
        printf "\r%-110s ✅\n", emoji[dots_printed]" Finished "outfile" "emoji[dots_printed]" (elapsed: "elapsed_str")";
        printf "🎉 Total files created: %d in %s.\n", file_num, elapsed_str
      }' "$input_file"
}

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

    local total_size=$(stat -c%s "$input_file")
    local filename=$(basename -- "$input_file")
    local base_name="${filename%.*}"
    local ext="${filename##*.}"
    local split_dir="${base_name}_split_${ext}"
    mkdir -p "$split_dir"

    awk -v prefix="${split_dir}/${base_name}_part" -v ext="$ext" -v chunk_size="$chunk_size_bytes" '
      BEGIN { file_num = 1; size = 0; outfile = sprintf("%s_%02d.%s", prefix, file_num, ext); }
      {
        print >> outfile;
        size += length($0) + 1;
        if ($0=="</rdf:Description>" && size >= chunk_size) {
          close(outfile);
          file_num++;
          outfile = sprintf("%s_%02d.%s", prefix, file_num, ext);
          size = 0;
        }
      }
      END { printf "Total files created: %d\n", file_num }
    ' "$input_file"
}

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

