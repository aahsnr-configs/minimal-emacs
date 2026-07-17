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
    lines = input_text.split("\n")

    current_filename = None
    in_code_block = False
    code_type = None  # 'html', 'css', or 'js'
    code_content = []
    saved_files = []

    for line in lines:
        # 1. Look for filename marker: Filename: `slug.ext` or Filename: slug.ext
        filename_match = re.search(
            r"Filename:\s*`?([a-zA-Z0-9_-]+\.(html|css|js))`?", line
        )
        if filename_match and not in_code_block:
            current_filename = filename_match.group(1)
            continue

        # 2. Start of code block (detect type)
        if re.match(r"^```[ \t]*html", line) and current_filename:
            in_code_block = True
            code_type = "html"
            code_content = []
            continue
        elif re.match(r"^```[ \t]*css", line) and current_filename:
            in_code_block = True
            code_type = "css"
            code_content = []
            continue
        elif re.match(r"^```[ \t]*(javascript|js)", line) and current_filename:
            in_code_block = True
            code_type = "js"
            code_content = []
            continue

        # 3. End of code block (Strict match for closing backticks)
        if in_code_block and re.match(r"^```[ \t]*$", line):
            in_code_block = False
            file_path = output_dir / current_filename
            file_path.write_text("\n".join(code_content), encoding="utf-8")
            saved_files.append(str(file_path))

            # Visual feedback based on file type
            if code_type == "html":
                print(f"  ✅ Saved HTML: {file_path}", file=sys.stderr)
            elif code_type == "css":
                print(f"  ✅ Saved CSS: {file_path}", file=sys.stderr)
            elif code_type == "js":
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

    if input_file and input_file != "-":
        with open(input_file, "r", encoding="utf-8") as f:
            text = f.read()
        source_name = input_file
    else:
        text = sys.stdin.read()
        source_name = "stdin"

    print(f"📥 Extracting files from {source_name}...", file=sys.stderr)
    saved = extract_files(text, Path("features"))

    # Categorize saved files
    html_files = [f for f in saved if f.endswith(".html")]
    css_files = [f for f in saved if f.endswith(".css")]
    js_files = [f for f in saved if f.endswith(".js")]

    print(
        f"🎉 Successfully extracted {len(saved)} files into ./features/",
        file=sys.stderr,
    )
    if html_files:
        print(f"   📄 {len(html_files)} HTML file(s)", file=sys.stderr)
    if css_files:
        print(f"   🎨 {len(css_files)} CSS file(s)", file=sys.stderr)
    if js_files:
        print(f"   ⚡ {len(js_files)} JS file(s)", file=sys.stderr)


if __name__ == "__main__":
    main()
