#!/usr/bin/env bash
set -e

# Determine Emacs config directory
if [ -d "$HOME/.config/emacs" ]; then
  EMACS_DIR="$HOME/.config/emacs"
elif [ -d "$HOME/.emacs.d" ]; then
  EMACS_DIR="$HOME/.emacs.d"
else
  echo "Error: Could not find Emacs config directory."
  exit 1
fi

STRAIGHT_DIR="${EMACS_DIR}/straight"
LOCKFILE="${STRAIGHT_DIR}/versions/packages-lock.el"
LOGFILE="straight-bootstrap-$(date +%s).log"

echo "==> Emacs config directory: ${EMACS_DIR}"
echo "==> Log file: ${LOGFILE}"

echo "==> Cleaning straight.el build and repo caches..."
# Backup lockfile
if [ -f "$LOCKFILE" ]; then
  cp "$LOCKFILE" /tmp/packages-lock.el.bak
  echo "    Backed up lockfile."
fi

# Remove repos and build directories to force fresh clones
rm -rf "${STRAIGHT_DIR}/repos"
rm -rf "${STRAIGHT_DIR}/build"
rm -rf "${STRAIGHT_DIR}/cache"

# Restore lockfile
mkdir -p "${STRAIGHT_DIR}/versions"
if [ -f /tmp/packages-lock.el.bak ]; then
  mv /tmp/packages-lock.el.bak "$LOCKFILE"
  echo "    Restored lockfile."
fi

echo "==> Running Emacs in batch mode to capture synchronous straight.el cloning..."
echo "    (This will take a few minutes as it clones all repositories from scratch)"

# Run Emacs in batch mode.
# 1. Force full git clones so historical commits from the lockfile are present.
# 2. Load early-init.el and init.el to trigger all straight-use-package calls.
# 3. Capture both stdout and stderr.
emacs --batch \
  --eval "(setq straight-vc-git-default-clone-depth 'full)" \
  --eval "(setq debug-on-error t)" \
  --load "${EMACS_DIR}/early-init.el" \
  --load "${EMACS_DIR}/init.el" \
  --eval "(message \"=== straight.el bootstrap sequence completed ===\")" \
  2>&1 | tee "$LOGFILE"

echo ""
echo "==> Analysis of log file:"
if grep -q "Could not reset to commit" "$LOGFILE"; then
  echo "    [!] FOUND 'Could not reset to commit' warnings."
  echo "    Extracting failed packages:"
  grep "Could not reset to commit" "$LOGFILE" | awk -F'"' '{print "        - " $4 " (commit: " $2 ")"}'
else
  echo "    [OK] No 'Could not reset' warnings found."
fi

echo ""
echo "==> Full log saved to: $(pwd)/$LOGFILE"
echo "    Please share the extracted failed packages list or the full log file."
