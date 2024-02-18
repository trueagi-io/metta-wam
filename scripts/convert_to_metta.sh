#!/bin/bash

get_line_count() {
  wc -l "$1" | cut -d' ' -f1
}

process_file() {
  local INPUT_FILE=$1
  local FORCE=$2
  local OUTPUT_FILE="${INPUT_FILE}.metta"
  local HEAD=$(basename -- "$INPUT_FILE")
  HEAD=${HEAD/.tsv/}
  HEAD=${HEAD/.fb/}
  # HEAD=${HEAD%.*}  # Remove the file extension
  HEAD=${HEAD/_fb_2[0-9][0-9][0-9]_[0-9][0-9]/}
  HEAD=${HEAD/2[0-9][0-9][0-9]_[0-9][0-9]/}
  HEAD=${HEAD/\./_}

  # Skip if the input file is already a .metta file
  if [[ "$INPUT_FILE" == *.metta ]]; then
    #echo "Skipping .metta file: $INPUT_FILE"
    return
  fi

  local input_lines=$(get_line_count "$INPUT_FILE")
  echo -e  "Into-MeTTa: $input_lines lines.  \t $INPUT_FILE "
  echo -en "            "

  # Check if the output file exists and is newer than the input file
  if [[ "$FORCE" != "--force" ]] && [ -e "$OUTPUT_FILE" ] && [ "$OUTPUT_FILE" -nt "$INPUT_FILE" ]; then
    local line_count=$(get_line_count "$OUTPUT_FILE")
    # Calculate 80% of the input line count
    local threshold=$(( input_lines * 80 / 100 ))
    if [ "$line_count" -ge "$threshold" ] && [ "$line_count" -ge "400" ]; then
      echo -e "$line_count atoms.  \t Skipping: $HEAD already exists and is newer."
      return
    fi
  fi

  local start_time=$(date +%s.%N) # Get the start time with nanosecond precision

  dos2unix -f "$INPUT_FILE" > /dev/null 2>/dev/null
  # Process based on the file extension
  case "$INPUT_FILE" in
    *.obo | *.json | *.fa)
      rm -f "$OUTPUT_FILE"
      echo -ne "."
      ###########swipl -l src/main/flybase_convert.pl -- --convert "$INPUT_FILE" --halt | tee -p "$OUTPUT_FILE"
      #echo "" ; echo ""
      #echo "swipl -l src/main/flybase_convert.pl -- --context=$HEAD --convert \"$INPUT_FILE\" --halt > \"$OUTPUT_FILE\""
      #echo "" ; echo ""
      swipl -l src/main/flybase_convert.pl -- --context=$HEAD --convert "$INPUT_FILE" --halt  #> "$OUTPUT_FILE" 2>/dev/null

      echo -ne "."
      # Create a temporary file
      temp_file=$(mktemp)
      echo -ne "."
      # Make sure temporary file will be deleted on script exit
      trap 'rm -f "$temp_file"' EXIT
      # Remove duplicates with awk and save to temporary file
      awk '!seen[$0]++' "$OUTPUT_FILE" > "$temp_file"
      # Overwrite the original file with the temporary file
      mv "$temp_file" "$OUTPUT_FILE"
      echo -ne "."
      ;;
    *)
      echo -ne "."
      #python3 src/mettalog.py "$INPUT_FILE" --analyze > "$OUTPUT_FILE"
      python3 src/panda_util.py "$INPUT_FILE" --analyze > "$OUTPUT_FILE"
      skip_lines=$?
      echo -ne "."
      awk -v head="$HEAD" -v skip="$skip_lines" '
      function should_quote_metta(field) {
       if (field ~ /[ ()\n\t\r]/) {
           return 1  # Quote if field contains space, (, ), newline, tab, or carriage return
       }
       if (field ~ /[^[:print:]]/) {
          return 1  # Quote if field contains any non-printing character
       }
        if (field ~ /[" ]|'\''|\/|,|\|/) {
          # return 1  # Quote if field contains ", space, , /, , or |
        }
        if (field ~ /^[0-9]/ && field !~ /^[0-9]+$/) {
          # return 1  # Quote if field starts with a number and contains non-numeric characters
        }
        return 0  # No need to quote
      }

      BEGIN {FS="\t"; OFS=" "}
      {
        # Skip lines that start with whitespace (if any) followed by # or ;
        if (/^[[:space:]]*[#;]/) {
          next
        }

        # Skip lines that does not contain a tab character (thus having less than 2 fields)
        if (NF < 2) {
          next
        }

        # Construct the line to check for uniqueness
        line_output = "(" head
        non_blank_found = 0
        for (i = 1; i <= NF; i++) {
          # Remove all control characters except newline
          gsub(/[[:cntrl:]]/, "", $i)
          # Escape double quotes to avoid breaking the output syntax
          gsub(/"/, "\\\"", $i)

          if ($i == "" || $i ~ /^[[:space:]]*$/) {
            # Represent fields that are only whitespace as ()
            line_output = line_output " ()"
          } else {
            # Determine if we should quote this field
            if (should_quote_metta($i)) {
              line_output = line_output " \"" $i "\""
            } else {
              line_output = line_output " " $i
            }
            non_blank_found = 1
          }
        }

        # Skip this line if it is a duplicate
        if (non_blank_found && !(line_output in printed_lines)) {
          if (skip > 0) {
            skip--
          } else {
            print line_output ")"
            printed_lines[line_output] = 1
          }
        }
      }
      ' "$INPUT_FILE" >> "$OUTPUT_FILE"

      ;;
  esac

  local end_time=$(date +%s.%N)  # Get the end time with nanosecond precision
  local elapsed_time=$(echo "$end_time - $start_time" | bc)  # Calculate elapsed time using bc
  elapsed_time=$(printf "%.2f" "$elapsed_time")  # Format the elapsed time

  local output_lines=$(get_line_count "$OUTPUT_FILE")
  echo -e "$output_lines atoms.   \t Converted $HEAD in $elapsed_time seconds."
}



# Check if any arguments are provided
if [ "$#" -eq 0 ]; then
  echo "Usage: $0 [--force] <list of directories and files>"
  exit 1
fi

FORCE=""
ARGS=()

# Separate the --force flag from the other arguments
for arg in "$@"; do
  if [[ "$arg" == "--force" ]]; then
    FORCE="--force"
  else
    ARGS+=("$arg")
  fi
done

# Process each argument
for INPUT_PATH in "${ARGS[@]}"; do
  if [ -d "$INPUT_PATH" ]; then
    # Start timing for directory processing
    dir_start_time=$(date +%s.%N)
    echo "Processing TSV files: $INPUT_PATH"
    find "$INPUT_PATH" -type f -name "*.*" -not -name "*.metta" -not -name "*.obo" -not -name "*.json" -not -name "*.fa" -print0 | while IFS= read -r -d $'\0' file; do
       process_file "$file" "$FORCE"
    done
    echo "Processing Special files: $INPUT_PATH"
    find "$INPUT_PATH" -type f \( -name "*.obo" -or -name "*.json" -or -name "*.fa" \) -print0 | while IFS= read -r -d $'\0' file; do
      echo  process_file "$file" "$FORCE"
    done
    # End timing for directory processing
    dir_end_time=$(date +%s.%N)
    dir_elapsed_time=$(echo "$dir_end_time - $dir_start_time" | bc)
    dir_elapsed_time=$(printf "%.2f" "$dir_elapsed_time")
    echo "Completed processing of directory '$INPUT_PATH' in $dir_elapsed_time seconds."
  elif [ -f "$INPUT_PATH" ]; then
    # Show elapsed time for individual files
    process_file "$INPUT_PATH" "$FORCE"
    head -100 "$INPUT_PATH".metta
    tail -10 "$INPUT_PATH".metta
  else
    echo "Error: '$INPUT_PATH' is not a valid file or directory."
  fi
done

