#!/usr/bin/env bash
# ==============================================================================
# serve.sh
# Launches a local HTTP server to preview the generated documentation.
# ==============================================================================

set -euo pipefail

PORT="${1:-8000}"

if ! command -v python3 &>/dev/null; then
  echo "❌ Python 3 is required but not installed." >&2
  exit 1
fi

echo "🚀 Starting local server at http://localhost:$PORT" >&2
echo "📂 Serving directory: $(pwd)" >&2
echo "Press [CTRL+C] to stop." >&2

# Bind to localhost only for security
python3 -m http.server "$PORT" --bind 127.0.0.1
