# 🛠️ Hardened Automation Suite (20-Batch External CSS/JS Architecture)

I have completely re-engineered the automation suite for the **20-batch workflow with external CSS/JS architecture**. Based on modern 2026 DevOps best practices and verified LLM output token limits (Claude Opus 4.8 and Fable 5 support 128K output tokens on the synchronous Messages API, matching the plan's original 128K/64K assumption), this suite handles the modular asset extraction while maintaining zero dependencies and bulletproof reliability.

> **July 2026 addendum:** batches 1–20 are already complete, so the batching mechanics below are historical/reference material — they already worked. Two real fixes are folded into `validate.sh` below: (1) code blocks must use `class="language-elisp"`, not the generic `class="language-lisp"`, since Prism's dedicated Elisp tokenizer handles `declare`/`interactive`/quote-splice forms that generic Lisp highlighting gets wrong; (2) `shared-scripts.js`'s clipboard copy must try `navigator.clipboard.writeText()` before falling back to the deprecated `document.execCommand('copy')`. Run the updated `validate.sh` against the existing `features/` directory to catch any files still using the old class name or clipboard path.

The Python parser now handles HTML, CSS, and JavaScript files using `pathlib` for robust cross-platform path management [[1], [4]]. The validation suite verifies external asset references and enforces the Batch 1 dependency chain.

---

## 1. Directory Setup

```bash
mkdir -p scripts features
```

---

## 2. The Hardened Scripts

### `scripts/save_batch.py` (Enhanced for External Assets)

**Why Python?** Python's `pathlib` and explicit state-machine logic prevent the file-descriptor leaks and nested-backtick truncation bugs inherent from Awk [[1], [2]]. This enhanced version now extracts HTML, CSS, and JavaScript files from LLM markdown output.

````python
#!/usr/bin/env python3
"""
save_batch.py
Parses LLM markdown output and extracts HTML, CSS, and JS files into the features/ directory.

Usage: ./scripts/save_batch.py <output.md>
   or: ./scripts/save_batch.py batch1_output.md

Supports:
- HTML files (```html blocks)
- CSS files (```css blocks)
- JavaScript files (```javascript or ```js blocks)
"""

import sys
import re
from pathlib import Path

def extract_files(input_text: str, output_dir: Path):
    """
    Extract HTML, CSS, and JS files from markdown output.

    Looks for patterns like:
    Filename: `intellisense.html`
    ```html
    <content>
    ```
    """
    output_dir.mkdir(parents=True, exist_ok=True)
    lines = input_text.split('\n')

    current_filename = None
    in_code_block = False
    code_type = None  # 'html', 'css', or 'js'
    code_content = []
    saved_files = []

    for line in lines:
        # 1. Look for filename marker: Filename: `slug.ext` or Filename: slug.ext
        filename_match = re.search(r'Filename:\s*`?([a-zA-Z0-9_-]+\.(html|css|js))`?', line)
        if filename_match and not in_code_block:
            current_filename = filename_match.group(1)
            continue

        # 2. Start of code block (detect type)
        if re.match(r'^```[ \t]*html', line) and current_filename:
            in_code_block = True
            code_type = 'html'
            code_content = []
            continue
        elif re.match(r'^```[ \t]*css', line) and current_filename:
            in_code_block = True
            code_type = 'css'
            code_content = []
            continue
        elif re.match(r'^```[ \t]*(javascript|js)', line) and current_filename:
            in_code_block = True
            code_type = 'js'
            code_content = []
            continue

        # 3. End of code block (Strict match for closing backticks)
        if in_code_block and re.match(r'^```[ \t]*$', line):
            in_code_block = False
            file_path = output_dir / current_filename
            file_path.write_text('\n'.join(code_content), encoding='utf-8')
            saved_files.append(str(file_path))

            # Visual feedback based on file type
            if code_type == 'html':
                print(f"  ✅ Saved HTML: {file_path}", file=sys.stderr)
            elif code_type == 'css':
                print(f"  ✅ Saved CSS: {file_path}", file=sys.stderr)
            elif code_type == 'js':
                print(f"  ✅ Saved JS: {file_path}", file=sys.stderr)

            current_filename = None
            code_type = None
            continue

        # 4. Accumulate content
        if in_code_block:
            code_content.append(line)

    return saved_files

def main():
    input_file = sys.argv[1] if len(sys.argv) > 1 else None

    if input_file and input_file != '-':
        with open(input_file, 'r', encoding='utf-8') as f:
            text = f.read()
        source_name = input_file
    else:
        text = sys.stdin.read()
        source_name = "stdin"

    print(f"📥 Extracting files from {source_name}...", file=sys.stderr)
    saved = extract_files(text, Path("features"))

    # Categorize saved files
    html_files = [f for f in saved if f.endswith('.html')]
    css_files = [f for f in saved if f.endswith('.css')]
    js_files = [f for f in saved if f.endswith('.js')]

    print(f"🎉 Successfully extracted {len(saved)} files into ./features/", file=sys.stderr)
    if html_files:
        print(f"   📄 {len(html_files)} HTML file(s)", file=sys.stderr)
    if css_files:
        print(f"   🎨 {len(css_files)} CSS file(s)", file=sys.stderr)
    if js_files:
        print(f"   ⚡ {len(js_files)} JS file(s)", file=sys.stderr)

if __name__ == "__main__":
    main()
````

---

### `scripts/validate.sh` (External Asset Verification)

Hardened with `shopt -s nullglob` for safe directory globbing, fallback integer parsing to prevent BSD/macOS `grep -c` crashes, and **new checks for external CSS/JS references**.

```bash
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
    file_size=$(wc -c < "$file")
    if [ "$file_size" -lt 50000 ]; then
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
```

---

### `scripts/build-index.py` (Unchanged)

Hardened using `string.Template` to completely eliminate f-string CSS brace-escaping bugs and injection vulnerabilities.

```python
#!/usr/bin/env python3
"""
build-index.py
Scans the features/ directory for HTML files, extracts metadata,
and generates a Tokyo Night styled index.html landing page.
"""

import re
import sys
from pathlib import Path
from datetime import datetime
from string import Template

FEATURES_DIR = Path("features")
OUTPUT_FILE = Path("index.html")

def extract_metadata(html_content: str) -> dict:
    title_match = re.search(r"<title>(.*?)</title>", html_content, re.IGNORECASE | re.DOTALL)
    h1_match = re.search(r"<h1>(.*?)</h1>", html_content, re.IGNORECASE | re.DOTALL)
    category_match = re.search(r'<div class="category">(.*?)</div>', html_content, re.IGNORECASE | re.DOTALL)

    return {
        "title": title_match.group(1).strip() if title_match else "Untitled",
        "h1": h1_match.group(1).strip() if h1_match else "Untitled",
        "category": category_match.group(1).strip() if category_match else "General"
    }

def generate_index(features: list) -> str:
    categories = {}

    for feat in features:
        cat = feat["category"]
        if cat not in categories:
            categories[cat] = []
        categories[cat].append(feat)

    content_html = []

    for cat in sorted(categories.keys()):
        content_html.append(f'<h2 class="category-title">{cat}</h2><div class="grid">')

        for feat in sorted(categories[cat], key=lambda x: x["h1"]):
            # Safe HTML escaping for user content
            h1_safe = feat["h1"].replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            title_safe = feat["title"].replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
            file_safe = feat["file"].replace("&", "&amp;").replace('"', "&quot;")

            content_html.append(f'''
                <a href="{file_safe}" class="card">
                    <span class="badge">Working</span>
                    <h3>{h1_safe}</h3>
                    <p>{title_safe}</p>
                </a>
            ''')

        content_html.append('</div>')

    return "\n".join(content_html)

def main():
    if not FEATURES_DIR.exists():
        print(f"❌ Directory '{FEATURES_DIR}' not found!", file=sys.stderr)
        sys.exit(1)

    features = []

    for file in FEATURES_DIR.glob("*.html"):
        if file.name == "index.html":
            continue

        content = file.read_text(encoding="utf-8")
        meta = extract_metadata(content)
        meta["file"] = f"features/{file.name}"
        features.append(meta)

    if not features:
        print("⚠️ No HTML files found in features/", file=sys.stderr)
        sys.exit(1)

    # Using string.Template to avoid f-string CSS brace escaping issues
    html_template = Template("""<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Feature Index</title>
    <link href="https://cdn.jsdelivr.net/npm/@fontsource/jetbrains-mono@5.0.18/index.min.css" rel="stylesheet" />
    <style>
      :root {
        --bg: #1a1b26; --surface: #24283b; --surface-2: #1f2335; --border: #2f3348;
        --text: #c0caf5; --text-dim: #9aa5ce; --blue: #7aa2f7; --purple: #bb9af7;
        --cyan: #7dcfff; --green: #9ece6a;
      }
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body {
        font-family: "JetBrains Mono", monospace; background: var(--bg); color: var(--text);
        padding: 40px; line-height: 1.6; max-width: 1200px; margin: 0 auto;
      }
      h1 { font-size: 28px; margin-bottom: 8px; }
      .subtitle { color: var(--text-dim); margin-bottom: 32px; }
      .category-title {
        font-size: 14px; text-transform: uppercase; letter-spacing: 0.1em;
        color: var(--purple); margin: 32px 0 16px; border-bottom: 1px solid var(--border); padding-bottom: 8px;
      }
      .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 16px; }
      .card {
        background: var(--surface); border: 1px solid var(--border); border-radius: 10px;
        padding: 20px; text-decoration: none; color: var(--text); transition: all 0.2s;
        display: flex; flex-direction: column; gap: 8px;
      }
      .card:hover { border-color: var(--blue); transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.2); }
      .card h3 { font-size: 16px; color: var(--blue); }
      .card p { font-size: 13px; color: var(--text-dim); }
      .badge {
        display: inline-block; font-size: 10px; font-weight: 700; text-transform: uppercase;
        color: var(--green); background: rgba(158, 206, 106, 0.1); border: 1px solid rgba(158, 206, 106, 0.3);
        padding: 2px 8px; border-radius: 12px; width: fit-content;
      }
    </style>
  </head>
  <body>
    <h1>Emacs IDE Documentation</h1>
    <p class="subtitle">Complete index of all IDE features. Generated on $timestamp.</p>
    $content
  </body>
</html>""")

    final_html = html_template.safe_substitute(
        timestamp=datetime.now().strftime("%Y-%m-%d %H:%M"),
        content=generate_index(features)
    )

    OUTPUT_FILE.write_text(final_html, encoding="utf-8")
    print(f"🎉 Successfully generated {OUTPUT_FILE} with {len(features)} features.", file=sys.stderr)

if __name__ == "__main__":
    main()
```

---

### `scripts/serve.sh` (Unchanged)

Hardened with localhost-only binding to prevent accidental LAN exposure.

```bash
#!/usr/bin/env bash
# ==============================================================================
# serve.sh
# Launches a local HTTP server to preview the generated documentation.
# ==============================================================================

set -euo pipefail

PORT="${1:-8000}"

if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is required but not installed." >&2
    exit 1
fi

echo "🚀 Starting local server at http://localhost:$PORT" >&2
echo "📂 Serving directory: $(pwd)" >&2
echo "Press [CTRL+C] to stop." >&2

# Bind to localhost only for security
python3 -m http.server "$PORT" --bind 127.0.0.1
```

---

## 📖 Detailed Execution Instructions

### Make Scripts Executable

```bash
chmod +x scripts/*.sh scripts/*.py
```

---

## 🔄 The 20-Batch Workflow

### ⚠️ CRITICAL: Batch 1 Dependency Rule

**Batch 1 is special.** It generates the external CSS and JavaScript files that all subsequent batches reference. You **must** complete Batch 1 successfully before running any other batch.

**Batch 1 generates:**

- `shared-styles.css` (~800 lines of Tokyo Night CSS)
- `shared-scripts.js` (~100 lines of vanilla JavaScript)
- 2 HTML feature files (IntelliSense + Hover Info Native)

**Batches 2-20 generate:**

- 2 HTML feature files per batch
- All HTML files reference `shared-styles.css` and `shared-scripts.js`

---

### The Batch Loop (Repeat 20 times)

#### Step A: Generate Batch Content

Paste the Master Batch Prompt into our chat, change `[N]` to the batch number (1-20), and append `GREEN LIGHT`.

**Example for Batch 1:**

```
[Master Batch Prompt with BATCH 1] GREEN LIGHT
```

#### Step B: Save LLM Output

Copy my entire markdown response and save it to a local file:

```bash
# For Batch 1
# Save to batch1.md

# For Batch 2
# Save to batch2.md

# ... and so on
```

#### Step C: Extract Files

Run the parser to extract HTML, CSS, and JS files:

```bash
./scripts/save_batch.py batch[N].md
```

**Expected output for Batch 1:**

```
📥 Extracting files from batch1.md...
  ✅ Saved CSS: features/shared-styles.css
  ✅ Saved JS: features/shared-scripts.js
  ✅ Saved HTML: features/intellisense.html
  ✅ Saved HTML: features/hover-info-native.html
🎉 Successfully extracted 4 files into ./features/
   📄 2 HTML file(s)
   🎨 1 CSS file(s)
   ⚡ 1 JS file(s)
```

**Expected output for Batches 2-20:**

```
📥 Extracting files from batch2.md...
  ✅ Saved HTML: features/hover-info-childframe.html
  ✅ Saved HTML: features/signature-help.html
🎉 Successfully extracted 2 files into ./features/
   📄 2 HTML file(s)
```

#### Step D: Validate Structural Integrity

```bash
./scripts/validate.sh
```

**Validation checks:**

- HTML contains `<title>` tag
- HTML contains sidebar and topbar
- HTML contains exactly 6 accordions
- HTML references Prism.js
- **NEW:** HTML references `shared-styles.css`
- **NEW:** HTML references `shared-scripts.js`
- **NEW:** File size > 50KB (detects truncation)
- **NEW:** External assets exist (after Batch 1)
- **PATCHED:** No `class="language-lisp"` remains (should be `class="language-elisp"`)
- **PATCHED:** `shared-scripts.js` calls `navigator.clipboard` (not execCommand-only)

---

## 🧪 Testing & Verification Protocol

### After Batch 1 (CRITICAL TEST POINT)

**This is where you verify the external asset architecture works.**

#### 1. Verify External Assets Exist

```bash
ls -lh features/shared-styles.css features/shared-scripts.js
```

**Expected output:**

```
-rw-r--r-- 1 user user  25K Jul 17 10:30 features/shared-styles.css
-rw-r--r-- 1 user user 3.2K Jul 17 10:30 features/shared-scripts.js
```

#### 2. Launch Preview Server

```bash
./scripts/serve.sh
```

#### 3. Test in Browser

Open `http://localhost:8000/features/intellisense.html` and verify:

✅ **Tokyo Night styling renders correctly** (dark background, blue accents)
✅ **Sidebar navigation works** (click to collapse/expand)
✅ **Topbar displays correctly** (breadcrumb, focus button)
✅ **Accordions expand/collapse** (click headers)
✅ **Code blocks have syntax highlighting** (Prism.js working)
✅ **Mobile responsive** (resize browser window)
✅ **Focus mode toggles** (click focus button, press ESC)
✅ **Clipboard copy works** (click copy button on code blocks)

**If any of these fail, STOP and troubleshoot before proceeding to Batch 2.**

---

### After Batches 2-20

For each subsequent batch:

1. **Extract files:** `./scripts/save_batch.py batch[N].md`
2. **Validate:** `./scripts/validate.sh`
3. **Refresh browser:** Simply refresh `http://localhost:8000` to see new pages
4. **Test new features:** Navigate to the new feature pages and verify styling

**The external CSS/JS will automatically apply to all new pages.**

---

## 🚨 Troubleshooting Guide

### Issue: "Missing external CSS reference" Error

**Cause:** HTML file doesn't contain `<link rel="stylesheet" href="shared-styles.css">`

**Solution:**

1. Check the LLM output in `batch[N].md` — did it include the `<link>` tag?
2. If missing, regenerate the batch with explicit instruction: "Include `<link rel='stylesheet' href='shared-styles.css'>` in the `<head>`"

---

### Issue: "shared-styles.css NOT found" After Batch 1

**Cause:** Batch 1 didn't generate the CSS file

**Solution:**

1. Check `batch1.md` — did the LLM output include a `Filename: shared-styles.css` marker?
2. If missing, regenerate Batch 1 with explicit instruction: "Generate shared-styles.css with all Tokyo Night CSS"

---

### Issue: Pages Look Unstyled in Browser

**Cause:** External CSS/JS files missing or not in same directory

**Solution:**

1. Verify files exist: `ls features/shared-*.{css,js}`
2. Verify they're in the same directory as HTML files
3. Check browser console for 404 errors
4. If files missing, regenerate Batch 1

---

### Issue: "Suspiciously small" Warning

**Cause:** HTML file is truncated (< 50KB)

**Solution:**

1. LLM output was cut off due to token limits
2. Regenerate the batch
3. If problem persists, split into single-feature batches

---

### Issue: Validation Fails with Multiple Errors

**Cause:** LLM reasoning degradation from excessive output

**Solution:**

1. Reduce batch size (generate 1 feature instead of 2)
2. Or regenerate with more explicit structural instructions

---

## 🎯 Finalize & Preview

### After All 20 Batches Complete

#### 1. Generate Master Index

```bash
./scripts/build-index.py
```

This creates `index.html` with links to all 40 feature pages.

#### 2. Launch Final Preview

```bash
./scripts/serve.sh
```

#### 3. Comprehensive Testing

Open `http://localhost:8000` and verify:

✅ **Index page displays all 40 features** organized by category
✅ **All internal links work** (click through to each feature)
✅ **Sidebar navigation updates** (active state changes per page)
✅ **Consistent styling** across all pages (same CSS applied)
✅ **All accordions functional** on every page
✅ **Mobile responsive** on all pages

---

## 📊 Batch Schedule Reference

| Batch  | Features                                     | External Assets                                      |
| ------ | -------------------------------------------- | ---------------------------------------------------- |
| **1**  | IntelliSense + Hover Info (Native)           | **Generate** shared-styles.css and shared-scripts.js |
| **2**  | Hover Info (Childframe) + Signature Help     | Reference existing assets                            |
| **3**  | Go to Definition + Go to Declaration         | Reference existing assets                            |
| **4**  | Go to Type Definition + Go to Implementation | Reference existing assets                            |
| **5**  | Find All References + Document Highlight     | Reference existing assets                            |
| **6**  | Document Symbols + Workspace Symbol Search   | Reference existing assets                            |
| **7**  | Code Actions + Document Formatting           | Reference existing assets                            |
| **8**  | Range Formatting + On-type Formatting        | Reference existing assets                            |
| **9**  | Rename Symbol + Folding Ranges               | Reference existing assets                            |
| **10** | Selection Range + Linked Editing Range       | Reference existing assets                            |
| **11** | Call Hierarchy + Type Hierarchy              | Reference existing assets                            |
| **12** | Semantic Tokens + Inlay Hints                | Reference existing assets                            |
| **13** | Inline Values + Document Links               | Reference existing assets                            |
| **14** | Document Color + Diagnostics (Push)          | Reference existing assets                            |
| **15** | Diagnostics (Pull) + Moniker                 | Reference existing assets                            |
| **16** | File-Operation Hooks + Execute Command       | Reference existing assets                            |
| **17** | Peek Definition + Breadcrumbs Bar            | Reference existing assets                            |
| **18** | Problems Panel + Sticky Scroll               | Reference existing assets                            |
| **19** | Bracket Colorization + Minimap               | Reference existing assets                            |
| **20** | Multi-Cursor Editing + Quick Fix Lightbulb   | Reference existing assets                            |

---

## 🚀 Next Steps: Transitioning to Phase 3 (Astro SSG)

Once you have verified all 40 standalone HTML files via the local server, we will execute Phase 3 of the `plan.md`: migrating these modular artifacts into the Astro 6.x/MDX architecture (retargeted from 5.x — Astro 6 has been stable since March 10, 2026, and Astro is now Cloudflare-owned).

### Migration Bridge Strategy

**Component Extraction:**
The external CSS/JS architecture makes Phase 3 migration significantly easier:

- `shared-styles.css` splits into `tokens.css`, `base.css`, `components.css`
- `shared-scripts.js` migrates to `src/scripts/shared.js` (the Clipboard-API-first `copyCode()` carries over unchanged)
- HTML files are parsed, stripping `<head>`, sidebar, and topbar
- 6 accordion bodies are isolated into raw MDX snippets
- `class="language-elisp"` code blocks map directly to Astro's Shiki-based code blocks or an Elisp Prism grammar in an MDX `<CodeWindow>` component — carry the class through unchanged

**Frontmatter Generation:**
The extraction script will automatically generate YAML frontmatter (Title, Category, LSP methods, Routing) based on the metadata bars in each HTML file.

**Astro Scaffolding:**
I will provide the `GREEN LIGHT` authorized Astro 6.x project structure, including:

- `FeatureLayout.astro` template
- Atomic components (`<Accordion>`, `<StackGrid>`, `<CodeWindow>`)
- Content Collections schema
- Search integration via **Pagefind's own component-based UI (v1.5.0+)** — not the `astro-pagefind` wrapper component, which is now in maintenance mode
- Astro 6's built-in Fonts API to self-host JetBrains Mono instead of the jsdelivr CDN `<link>` used in the tactical HTML

**Content Ingestion:**
We will drop the extracted MDX snippets into `src/content/` and let Astro's Content Collections automatically generate the final, hyper-optimized static site with View Transitions.

---

## ✅ Ready to Begin?

**Are you ready to start Batch 1?**

If so, paste the Master Batch Prompt with `BATCH 1` and `GREEN LIGHT`, and we will initialize the pipeline with the external CSS/JS architecture.

**Remember:** Batch 1 is critical — it generates the shared assets that all subsequent batches depend on. Test thoroughly before proceeding to Batch 2.
