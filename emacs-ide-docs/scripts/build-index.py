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
    title_match = re.search(
        r"<title>(.*?)</title>", html_content, re.IGNORECASE | re.DOTALL
    )
    h1_match = re.search(r"<h1>(.*?)</h1>", html_content, re.IGNORECASE | re.DOTALL)
    category_match = re.search(
        r'<div class="category">(.*?)</div>', html_content, re.IGNORECASE | re.DOTALL
    )

    return {
        "title": title_match.group(1).strip() if title_match else "Untitled",
        "h1": h1_match.group(1).strip() if h1_match else "Untitled",
        "category": category_match.group(1).strip() if category_match else "General",
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
            h1_safe = (
                feat["h1"]
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
            )
            title_safe = (
                feat["title"]
                .replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
            )
            file_safe = feat["file"].replace("&", "&amp;").replace('"', "&quot;")

            content_html.append(f'''
                <a href="{file_safe}" class="card">
                    <span class="badge">Working</span>
                    <h3>{h1_safe}</h3>
                    <p>{title_safe}</p>
                </a>
            ''')

        content_html.append("</div>")

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
        content=generate_index(features),
    )

    OUTPUT_FILE.write_text(final_html, encoding="utf-8")
    print(
        f"🎉 Successfully generated {OUTPUT_FILE} with {len(features)} features.",
        file=sys.stderr,
    )


if __name__ == "__main__":
    main()
