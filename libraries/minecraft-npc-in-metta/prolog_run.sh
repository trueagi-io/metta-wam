#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

(
    cd "$SCRIPT_DIR" || { echo "❌ Failed to change directory to script location."; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1; }

    # Ensure environment variables are correctly loaded
    source environment.sh || { echo "❌ Failed to source environment.sh"; [[ "${BASH_SOURCE[0]}" != "${0}" ]] && return 1 || exit 1; }

  cat <<EOF
✅ CLASSPATH/Environment setup complete!
🔹 If you encounter issues, check logs and ensure the Minecraft server is running.

📦 To verify the CLASSPATH in Prolog:  ?- genv('CLASSPATH',CP).
🔧 To start the bot in Prolog,  type:  ?- login0.
EOF

  # Step 5: Launch Metta if not sourced
  echo "🧠 swipl minecraft_bot_prolog.pl"
  swipl prolog/minecraft_bot_prolog.pl
) # End of subshell (directory is restored automatically)

