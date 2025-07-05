#!/bin/bash

# Usage: ./py2metta.sh arc_math_utils.py

set -e

INPUT_FILE="$1"

if [ -z "$INPUT_FILE" ]; then
  echo "❌ Error: Please provide a Python filename."
  echo "Usage: $0 <filename.py>"
  exit 1
fi

BASENAME=$(basename "$INPUT_FILE")
NAME="${BASENAME%.*}"

echo "📂 Using input: $INPUT_FILE"
echo "📝 Base name: $NAME"

# Helper: run command if target doesn't exist
run_if_missing() {
  OUT="$3"

  if [ ! -f "$OUT" ]; then
    echo "🛠️ Generating: $OUT"
    eval "python $@"
  else
    echo "⏭️  Skipping: $OUT already exists."
  fi
  sed '/^```/d' -i "$OUT"
}

# Round 1
echo "⚙️ Round 1: Expanding input Python"
run_if_missing expand_with_chatgpt.py "$INPUT_FILE" "${NAME}_gpt_1.py"
run_if_missing expand_with_ast.py "$INPUT_FILE" "${NAME}_ast_1.py"

# Round 2
echo "⚙️ Round 2: Further expansions"
run_if_missing expand_with_chatgpt.py "${NAME}_ast_1.py" "${NAME}_gpt_2.py"
run_if_missing expand_with_ast.py "${NAME}_gpt_1.py" "${NAME}_ast_2.py"

# Prolog generation
echo "⚙️ Generating Prolog"
run_if_missing python_to_prolog.py "${NAME}_gpt_1.py" "${NAME}_gpt_1.pl"
run_if_missing python_to_prolog.py "${NAME}_ast_1.py" "${NAME}_ast_1.pl"
run_if_missing python_to_prolog.py "${NAME}_gpt_2.py" "${NAME}_gpt_2.pl"
run_if_missing python_to_prolog.py "${NAME}_ast_2.py" "${NAME}_ast_2.pl"

# Prolog → MeTTa
echo "⚙️ Translating Prolog to MeTTa"
run_if_missing prolog_to_metta.py "${NAME}_gpt_1.pl" "${NAME}_gpt_1_prolog.metta"
run_if_missing prolog_to_metta.py "${NAME}_ast_1.pl" "${NAME}_ast_1_prolog.metta"
run_if_missing prolog_to_metta.py "${NAME}_gpt_2.pl" "${NAME}_gpt_2_prolog.metta"
run_if_missing prolog_to_metta.py "${NAME}_ast_2.pl" "${NAME}_ast_2_prolog.metta"

# Python → MeTTa (direct)
echo "⚙️ Direct Python-to-MeTTa"
run_if_missing converter.py "${NAME}_gpt_1.py" "${NAME}_gpt_1.metta"
run_if_missing converter.py "${NAME}_ast_1.py" "${NAME}_ast_1.metta"
run_if_missing converter.py "${NAME}_gpt_2.py" "${NAME}_gpt_2.metta"
run_if_missing converter.py "${NAME}_ast_2.py" "${NAME}_ast_2.metta"

echo "✅ All passes completed!"

