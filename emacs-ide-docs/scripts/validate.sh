#!/usr/bin/env bash
# ==============================================================================
# validate.sh
# Validates that all generated HTML files meet the structural requirements
# for the 20-batch external CSS/JS architecture.
# ==============================================================================

set -euo pipefail

FEATURES_DIR="features"
ERRORS=0

if [ ! -d "$FEATURES_DIR" ]; then
  echo "❌ Directory '$FEATURES_DIR' not found!" >&2
  exit 1
fi

echo "🔍 Validating files in $FEATURES_DIR..." >&2

# Safely handle empty directories without triggering set -u
shopt -s nullglob
html_files=("$FEATURES_DIR"/*.html)
css_files=("$FEATURES_DIR"/*.css)
js_files=("$FEATURES_DIR"/*.js)
shopt -u nullglob

# Check for external assets (required for all batches after Batch 1)
has_shared_css=false
has_shared_js=false

if [ ${#css_files[@]} -gt 0 ]; then
  for css_file in "${css_files[@]}"; do
    if [[ "$(basename "$css_file")" == "shared-styles.css" ]]; then
      has_shared_css=true
      break
    fi
  done
fi

if [ ${#js_files[@]} -gt 0 ]; then
  for js_file in "${js_files[@]}"; do
    if [[ "$(basename "$js_file")" == "shared-scripts.js" ]]; then
      has_shared_js=true
      break
    fi
  done
fi

# Validate HTML files
if [ ${#html_files[@]} -eq 0 ]; then
  echo "⚠️ No HTML files found in $FEATURES_DIR" >&2
  exit 0
fi

for file in "${html_files[@]}"; do
  filename=$(basename "$file")
  file_errors=0

  # 1. Check for Title
  if ! grep -q "<title>" "$file"; then
    echo "  ⚠️  $filename: Missing <title>" >&2
    ((file_errors++)) || true
  fi

  # 2. Check for Sidebar
  if ! grep -q 'class="sidebar"' "$file"; then
    echo "  ⚠️  $filename: Missing sidebar" >&2
    ((file_errors++)) || true
  fi

  # 3. Check for Topbar
  if ! grep -q 'class="topbar"' "$file"; then
    echo "  ⚠️  $filename: Missing topbar" >&2
    ((file_errors++)) || true
  fi

  # 4. Check for 6 Accordions (Safe integer parsing)
  acc_count=$(grep -E -c 'class="acc[ "]' "$file" || echo "0")
  if ! [[ "$acc_count" =~ ^[0-9]+$ ]]; then
    acc_count=0
  fi
  if [ "$acc_count" -lt 6 ]; then
    echo "  ⚠️  $filename: Expected at least 6 accordions, found $acc_count" >&2
    ((file_errors++)) || true
  fi

  # 5. Check for Prism.js
  if ! grep -q "prism.min.js" "$file"; then
    echo "  ⚠️  $filename: Missing Prism.js" >&2
    ((file_errors++)) || true
  fi

  # 6. NEW: Check for external CSS reference
  if ! grep -q 'href="shared-styles.css"' "$file"; then
    echo "  ⚠️  $filename: Missing external CSS reference (shared-styles.css)" >&2
    ((file_errors++)) || true
  fi

  # 7. NEW: Check for external JS reference
  if ! grep -q 'src="shared-scripts.js"' "$file"; then
    echo "  ⚠️  $filename: Missing external JS reference (shared-scripts.js)" >&2
    ((file_errors++)) || true
  fi

  # 8. NEW: Check file size (detect truncation)
  file_size=$(wc -c <"$file")
  if [ "$file_size" -lt 25000 ]; then
    echo "  ⚠️  $filename: Suspiciously small ($file_size bytes) - possibly truncated" >&2
    ((file_errors++)) || true
  fi

  # 9. PATCHED: Check for stale generic Lisp class instead of the dedicated Elisp tokenizer
  if grep -q 'class="language-lisp"' "$file"; then
    echo "  ⚠️  $filename: Uses class=\"language-lisp\" — should be class=\"language-elisp\"" >&2
    ((file_errors++)) || true
  fi

  if [ "$file_errors" -eq 0 ]; then
    echo "  ✅ $filename: Valid" >&2
  else
    ERRORS=$((ERRORS + file_errors))
  fi
done

# Summary of external assets
echo "" >&2
if [ "$has_shared_css" = true ]; then
  echo "✅ shared-styles.css found" >&2
else
  echo "⚠️  shared-styles.css NOT found (required after Batch 1)" >&2
fi

if [ "$has_shared_js" = true ]; then
  echo "✅ shared-scripts.js found" >&2
  # PATCHED: verify Clipboard API is tried before the deprecated execCommand fallback
  if [ -f "$FEATURES_DIR/shared-scripts.js" ]; then
    if ! grep -q "navigator.clipboard" "$FEATURES_DIR/shared-scripts.js"; then
      echo "⚠️  shared-scripts.js: No navigator.clipboard call found — copyCode() is likely still execCommand-only (deprecated)" >&2
      ERRORS=$((ERRORS + 1))
    fi
  fi
else
  echo "⚠️  shared-scripts.js NOT found (required after Batch 1)" >&2
fi

if [ "$ERRORS" -gt 0 ]; then
  echo "" >&2
  echo "❌ Validation failed with $ERRORS errors." >&2
  exit 1
else
  echo "" >&2
  echo "🎉 All files passed validation!" >&2
  exit 0
fi
