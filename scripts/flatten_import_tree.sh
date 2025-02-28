copy_metta_files() {
    local top_dir="${1%/}"   # Remove trailing slash from the top directory
    local slug="${2}"        # Custom slug provided by the user
    local flat_dir="${3%/}"  # Remove trailing slash from the flat directory name
    local tree_dir="./${slug}_tree_${top_dir}"  # Temporary directory
    local filename_map_file="filename_map.txt"
    local verbose=0  # Verbose mode off by default

    # Enable verbose mode if `-v` is the fourth argument
    [[ "$4" == "-v" ]] && verbose=1

    if [[ -z "$top_dir" || -z "$slug" || -z "$flat_dir" ]]; then
        echo "❌ Error: Please provide a top directory, a slug, and a flat directory."
        echo "Usage: copy_metta_files <top_directory> <slug> <flat_directory> [-v]"
        return 1
    fi

    if [[ ! -d "$top_dir" ]]; then
        echo "❌ Error: Top directory '$top_dir' does not exist."
        return 1
    fi

    echo "🛠️ Preparing '$tree_dir/' and merging into '$flat_dir/'..."
    rm -rf "$tree_dir"  # Still erase the tree directory
    mkdir -p "$tree_dir" "$flat_dir"  # Ensure the flat directory is kept
    echo "✅ '$tree_dir/' is cleaned. '$flat_dir/' will be merged."

    # Preserve the filename_map.txt across runs
    touch "$filename_map_file"

    declare -A filename_map

    # Load existing filename map from file (only if it's non-empty)
    echo "🔄 Loading existing filename mappings..."
    while read -r key value; do
        if [[ -n "$key" && -n "$value" ]]; then
            filename_map["$key"]="$value"
        fi
    done < "$filename_map_file"

    # First pass: Copy directory structure and files into _tree
    echo "📂 Copying directory structure and files into '$tree_dir/'..."
    cp -a "$top_dir/." "$tree_dir/"

    # Map filenames to new paths in _tree
    find "$tree_dir" -name "*.metta" -print0 | while IFS= read -r -d '' file; do
        if [[ ! -f "$file" ]]; then continue; fi  # Skip if not a file
        base_filename="$(basename "$file")"
        base_filename_no_ext="${base_filename%.metta}"  # Remove .metta extension
        relative_path="${file#$tree_dir/}"
        flattened_name="${relative_path//\//__}"  # Convert slashes `/` into safer `__`
        flattened_name="${slug}__${flattened_name}"  # Prepend slug ONCE
        mapped_key="__${base_filename_no_ext}"  # Ensure uniqueness with `__` prefix

        # Add only new entries to the map
        if [[ -n "$base_filename_no_ext" && -z "${filename_map[$base_filename_no_ext]}" ]]; then
            filename_map["$base_filename_no_ext"]="$flattened_name"
            echo "$base_filename_no_ext $flattened_name" >> "$filename_map_file"  # Append to file
            echo "🔍 Copied: $file → $flattened_name"
        fi
    done

    echo "✅ All files copied. Now updating imports inside '$tree_dir/'..."

    # Second pass: Adjust imports inside _tree files
    for dest in "${filename_map[@]}"; do
        if [[ ! -f "$dest" ]]; then continue; fi  # Ensure file exists
        temp_file="$(mktemp)"

        echo "🛠️ Adjusting imports inside: $dest"

        awk -v tree_dir="$tree_dir" -v slug="$slug" -v filename_map_file="$filename_map_file" '
        BEGIN {
            # Load filename map into an AWK associative array
            while ((getline < filename_map_file) > 0) {
                filename_map[$1] = $2;
            }
        }
        {
            # Match import lines like:
            # !(import! &self chaining:dtl:forward:no-curry)
            # !(import! &self chaining:dtl:utils)
            if ($0 ~ /\(import! &[^ ]+ [^)]+\)/) {
                match($0, /\(import! &[^ ]+ ([^)]+)\)/, arr);
                original_import = arr[1];

                # Step 1: Convert `:` to `/` for normal path processing
                gsub(":", "/", original_import);

                # Step 2: Extract filename without extension
                split(original_import, parts, "/");
                filename_only = parts[length(parts)];  # Extract last element
                filename_no_ext = filename_only;  # Default if no .metta present
                sub(/\.metta$/, "", filename_no_ext);  # Remove .metta if present

                # Step 3: Ensure filename matches stored key
                final_lookup = "__" filename_no_ext;

                # If file exists in map, replace with the flattened name
                if (filename_no_ext in filename_map) {
                    new_import = filename_map[filename_no_ext];

                    # Avoid adding slug twice
                    if (index(new_import, slug "__") == 0) {
                        new_import = slug "__" new_import;
                    }

                    # Remove `.metta` from imports
                    new_import = gensub(/\.metta$/, "", "g", new_import);

                    print "🔄 Replacing inside file: " original_import " → " new_import > "/dev/stderr";
                    gsub(original_import, new_import);
                }
            }
            print;
        }
        ' "$dest" > "$temp_file"

        mv "$temp_file" "$dest"
    done

    echo "✅ All imports adjusted inside '$tree_dir/'. Now merging files into '$flat_dir/' and fixing imports..."

    # Third pass: Flatten files into _flat (MERGING instead of erasing)
    find "$tree_dir" -name "*.metta" -print0 | while IFS= read -r -d '' file; do
        if [[ ! -f "$file" ]]; then continue; fi  # Ensure file exists

        # Generate a flattened filename using __ separator
        relative_path="${file#$tree_dir/}"
        relative_path="${relative_path//:/\/}"  # Convert colons `:` into slashes `/`
        flat_filename="${relative_path//\//__}"
        flat_filename="${slug}__${flat_filename}"  # Prepend slug ONCE
        dest="$flat_dir/$flat_filename"

        # Copy the file to the flattened structure (DO NOT REMOVE EXISTING FILES)
        echo "📄 Merging: $relative_path → $dest"
        cp "$file" "$dest"

        # Adjust imports inside flattened files
        temp_file="$(mktemp)"
        awk -v slug="$slug" -v filename_map_file="$filename_map_file" '
        BEGIN {
            # Load filename map into an AWK associative array
            while ((getline < filename_map_file) > 0) {
                filename_map[$1] = $2;
            }
        }
        {
            # Match import statements and replace with their flattened version
            if ($0 ~ /\(import! &[^ ]+ [^)]+\)/) {
                match($0, /\(import! &[^ ]+ ([^)]+)\)/, arr);
                original_import = arr[1];

                # Step 1: Convert `:` to `/`
                gsub(":", "/", original_import);

                # Step 2: Extract filename without extension
                split(original_import, parts, "/");
                filename_only = parts[length(parts)];
                filename_no_ext = filename_only;
                sub(/\.metta$/, "", filename_no_ext);

                # Step 3: Ensure filename matches stored key
                final_lookup = "__" filename_no_ext;

                # If the imported file exists in the map, update the import path
                if (filename_no_ext in filename_map) {
                    flattened_import = filename_map[filename_no_ext];

                    # Avoid adding slug twice
                    if (index(flattened_import, slug "__") == 0) {
                        flattened_import = slug "__" flattened_import;
                    }

                    # Remove `.metta` from imports
                    flattened_import = gensub(/\.metta$/, "", "g", flattened_import);

                    gsub(original_import, flattened_import);
                    print "🔄 Fixing import in flattened file: " original_import " → " flattened_import > "/dev/stderr";
                }
            }
            print;
        }
        ' "$dest" > "$temp_file"

        mv "$temp_file" "$dest"
    done

    echo "✅ All .metta files processed, imports fixed, and merged into '$flat_dir/'."
}

copy_metta_files chaining chaining my_flat_dir
copy_metta_files hyperon-pln hyperon-pln my_flat_dir
copy_metta_files hyperon-miner hyperon-miner my_flat_dir
#!/bin/bash

# Directory containing the .metta files
TARGET_DIR="my_flat_dir"

# Step 1: Find all .metta files and store them in an array
metta_files=($(find "$TARGET_DIR" -type f -name "*.metta"))

# Step 2: Create mappings for resolving imports
declare -A file_to_module
declare -A module_to_file

for file in "${metta_files[@]}"; do
    # Extract filename without extension
    filename=$(basename "$file" .metta)
    
    # Convert filename to import format (replace __ with :)
    module_name="${filename//__/__}"  # Ensure format consistency

    # Store mappings
    file_to_module["$file"]="$module_name"
    module_to_file["$module_name"]="$filename"
done

# Step 3: Process each .metta file to correct its imports
for file in "${metta_files[@]}"; do
    temp_file="${file}.tmp"

    # Read file line by line
    while IFS= read -r line; do
        if echo "$line" | grep -Eq '^\!\(import\!\s*&[a-zA-Z0-9_-]+\s+[a-zA-Z0-9_:|-]+\)'; then
            # Extract &prefix and module name
            import_prefix=$(echo "$line" | sed -E 's/^\!\(import\!\s*(&[a-zA-Z0-9_-]+)\s+([a-zA-Z0-9_:|-]+)\)/\1/')
            import_module=$(echo "$line" | sed -E 's/^\!\(import\!\s*(&[a-zA-Z0-9_-]+)\s+([a-zA-Z0-9_:|-]+)\)/\2/')

            # Convert import notation (replace : with __)
            fixed_import="${import_module//:/__}"

            # Lookup the corrected import path
            if [[ -n "${module_to_file[$fixed_import]}" ]]; then
                corrected_module="$fixed_import"
                correct_import="!(import! ${import_prefix} ${corrected_module})"
                echo "$correct_import" >> "$temp_file"
                echo "Fixed import in: $file -> $correct_import"
            else
                echo "$line" >> "$temp_file"
            fi
        else
            echo "$line" >> "$temp_file"
        fi
    done < "$file"

    # Replace the original file with the updated one
    mv "$temp_file" "$file"
    echo "Updated imports in: $file"
done

echo "Import fixes completed!"
