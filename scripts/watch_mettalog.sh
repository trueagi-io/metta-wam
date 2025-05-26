#!/bin/bash

# Create a temporary helper script in /tmp
PROCESS_SCRIPT="/tmp/_process_lines_$$.sh"

cat > "$PROCESS_SCRIPT" <<'EOF'
#!/bin/bash

last_dot_time=0
YELLOW='\033[1;33m'
RED='\033[0;31m'
GREEN='\033[0;32m'


strip_ansi() {
  echo "$1" | sed -E 's/\x1B\[[0-9;]*[mGKHF]//g'
}

while IFS= read -r line; do
  now=$(date +%s)
  clean_line=$(strip_ansi "$line")

  if [[ "$clean_line" =~ === && ! "$clean_line" =~ =========== ]]; then
    echo -e "${YELLOW}🟢 $clean_line\033[0m"
    continue
  fi

  if [[ "$clean_line" =~ ailures\:\ 0 ]]; then
    echo -e "${GREEN}🟢 $clean_line\033[0m"
    continue
  fi

  if [[ "$clean_line" =~ [Ff]ailures: ]]; then
    res_count=$(echo "$clean_line" | sed -nE 's/.*[Ff]ailures:[[:space:]]*([0-9]+).*[[:space:]]*$/\1/p')
    if [[ "$res_count" != "0" && -n "$res_count" ]]; then
      echo -e "${RED}🔴 $clean_line\033[0m"
      echo -en "\a" ; echo -en "\a"  # Beep sound twice
      
    fi
    continue
  fi

  if [[ "$clean_line" =~ [Ss]uccesses: ]]; then
    res_count=$(echo "$clean_line" | sed -nE 's/.*[Ss]uccesses:[[:space:]]*([0-9]+).*[[:space:]]*$/\1/p')
    if [[ -n "$res_count" ]]; then
      echo -e "${GREEN}🟢 $clean_line\033[0m"
      continue
    fi
  fi

  # Default case: print dot every 4 seconds
  if (( now - last_dot_time >= 4 )); then
    echo -n "."; last_dot_time=$now
  fi
done
EOF

chmod +x "$PROCESS_SCRIPT"

# Cleanup handler
cleanup() {
  echo -e "\n🛑 Caught Ctrl+C. Cleaning up..."
  [[ -n "$WATCH_PID" ]] && kill -- -"$WATCH_PID" 2>/dev/null
  rm -f "$PROCESS_SCRIPT"
  exit 0
}

trap cleanup SIGINT

last_seen_file=""

while true; do
  if [[ -n "$1" ]]; then
    TEE_FILE="$1"
  else
    TEE_FILE=$(ls -t TEE*.ansi  | head -n 1)
  fi

  if [[ -z "$TEE_FILE" ]]; then
    echo "❌ No matching file found (TEE*.ansi) — retrying in 2 seconds..."
    sleep 2
    continue
  fi

  if [[ "$TEE_FILE" == "$last_seen_file" ]]; then
    sleep 2
    continue
  fi

  echo -e "📄 Now watching: $TEE_FILE"
  last_seen_file="$TEE_FILE"

  # Run tail | process in a subshell group, and capture the group PID
  (
    stdbuf -oL tail -n0 -F "$TEE_FILE" 2>/dev/null | "$PROCESS_SCRIPT"
  ) &
  WATCH_PID=$!

  # Wait until file changes or Ctrl+C
  while [[ "$TEE_FILE" == "$last_seen_file" ]]; do
    sleep 2
    TEE_FILE=$(ls -t TEE*.ansi 2>/dev/null | head -n 1)
  done

  echo -e "🔁 Detected newer file: $TEE_FILE"
  kill -- -"$WATCH_PID" 2>/dev/null
done

