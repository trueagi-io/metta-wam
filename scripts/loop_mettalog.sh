#!/bin/bash

last_interrupt_time=0
export TEE_FILE="TEE_loop_mettalog.ansi"

# Function to handle Ctrl+C (SIGINT)
cleanup() {
  now=$(date +%s.%N)

  diff=$(echo "$now - $last_interrupt_time" | bc)
  if (( $(echo "$diff < 0.25" | bc -l) )); then
    echo -e "\nDouble Ctrl+C detected — exiting." | tee -a "$TEE_FILE"
    kill -9 "$METTALOG_PID" 2>/dev/null
    exit 0
  else
    echo -e "\nSingle Ctrl+C — killing mettalog (PID $METTALOG_PID) and restarting..." | tee -a "$TEE_FILE"
    kill -9 "$METTALOG_PID" 2>/dev/null
    last_interrupt_time=$now
  fi
}


echo "In other terminal Remember:  tail -f $TEE_FILE | grep -i -E 'Successes:|===|warn|Failures:'"
sleep 2

trap cleanup SIGINT

while true; do
  echo "=== $(date) ===" | tee -a "$TEE_FILE"
  echo "In other terminal:  tail -f $TEE_FILE | grep -i -E 'Successes:|===|warn|Failures:'"
  echo "or:                 watch_mettalog.sh $TEE_FILE"
  sleep 1

  # Run mettalog and log everything to file
  mettalog "$@"  < /dev/null 2>&1 | stdbuf -oL tee -a "$TEE_FILE" &
  METTALOG_PID=$!
  wait $METTALOG_PID
done

