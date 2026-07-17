#!/usr/bin/env python3
"""
migrate_assets.py
Splits the tactical shared-styles.css into modular Astro CSS files
and migrates shared-scripts.js to the public directory.
"""

import re
from pathlib import Path


def main():
    css_file = Path("features/shared-styles.css")
    js_file = Path("features/shared-scripts.js")

    out_styles = Path("src/styles")
    out_styles.mkdir(parents=True, exist_ok=True)
    Path("public").mkdir(exist_ok=True)

    if css_file.exists():
        css = css_file.read_text()

        # 1. Extract :root (Tokens)
        root_match = re.search(r":root\s*\{[^}]+\}", css, re.DOTALL)
        tokens = root_match.group(0) if root_match else ""

        # 2. Extract @media (Responsive)
        media_matches = re.findall(
            r"@media[^{]+\{(?:[^{}]+|\{[^{}]*\})*\}", css, re.DOTALL
        )
        responsive = "\n\n".join(media_matches)

        # 3. Remaining CSS (Components & Base)
        remaining = css
        if root_match:
            remaining = remaining.replace(root_match.group(0), "")
        for m in media_matches:
            remaining = remaining.replace(m, "")

        (out_styles / "tokens.css").write_text(tokens)
        (out_styles / "responsive.css").write_text(responsive)
        (out_styles / "components.css").write_text(remaining)
        print("✅ CSS split into tokens.css, responsive.css, and components.css")
    else:
        print("⚠️ features/shared-styles.css not found. Run Batch 1 first.")

    if js_file.exists():
        js = js_file.read_text()
        Path("public/shared.js").write_text(js)
        print("✅ JS migrated to public/shared.js")
    else:
        print("⚠️ features/shared-scripts.js not found. Run Batch 1 first.")


if __name__ == "__main__":
    main()
