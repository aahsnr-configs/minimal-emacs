#!/usr/bin/env bash

# Best Practice: Exit immediately if a command fails (-e), if an uninitialized
# variable is used (-u), and ensure pipeline failures are properly propagated (-o pipefail).
set -euo pipefail

# Define variables for paths and logging
EMACS_DIR="${HOME}/.config/emacs"
LOG_FILE="doom_install_log.txt"

echo "=== Doom Emacs Interactive Installer Script ==="

# 1. Ensure the parent directory (~/.config) exists
mkdir -p "$(dirname "$EMACS_DIR")"

# 2. Find and remove any existing emacs config folder or symlink
if [ -d "$EMACS_DIR" ] || [ -L "$EMACS_DIR" ]; then
  echo "[*] Found an existing Emacs configuration directory at $EMACS_DIR."
  echo "[*] Removing it safely..."
  rm -rf "$EMACS_DIR"
  echo "[✓] Old configuration removed."
else
  echo "[*] No existing Emacs configuration folder found. Proceeding cleanly..."
fi

# 3. Clone the Doom Emacs core repository
echo "[*] Cloning Doom Emacs core repository..."
git clone --depth 1 https://github.com/doomemacs/core "$EMACS_DIR"

# 4. Execute the doom install command interactively with real-time screen output
echo "[*] Launching Doom Emacs installer..."
echo "[*] Note: The installation is fully interactive. Prompts will appear in real-time below."
echo "----------------------------------------------------------------------"

# Technical Solution:
# Instead of using a standard pipe which causes Emacs to buffer and hide text, we use
# 'script'. This creates a pseudo-terminal wrapper that forces the installer to stream
# every character to your screen instantly while logging a perfect copy to your file.
if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS / BSD 'script' syntax
  script -q "$LOG_FILE" "${EMACS_DIR}/bin/doom" install
else
  # Linux (util-linux) 'script' syntax
  script -q -c "${EMACS_DIR}/bin/doom install" "$LOG_FILE"
fi

echo "----------------------------------------------------------------------"
echo "[✓] Doom Emacs installation script has finished execution."
echo "[✓] Complete output log has been successfully saved to: $LOG_FILE"
