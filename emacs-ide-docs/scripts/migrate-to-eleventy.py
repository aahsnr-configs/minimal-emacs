#!/usr/bin/env python3
"""
Phase 3: Strangler Pattern Migration Script (Eleventy 11ty)
Converts the 40 tactical HTML files into Eleventy Markdown files with Nunjucks layouts.

FIX: Removed {% raw %} wrapper injection. Since markdownTemplateEngine is now false,
we no longer need to protect curly braces from Nunjucks.
"""

import os
import re
from pathlib import Path
from bs4 import BeautifulSoup

INPUT_DIR = Path("features")
OUTPUT_DIR = Path("src/content/docs")
ASSETS_DIR = Path("src/assets")


def clean_text(text):
    if not text:
        return ""
    return re.sub(r"\s+", " ", text).strip().replace('"', '\\"')


def migrate():
    print(f"🚀 Starting Eleventy Migration: {INPUT_DIR} -> {OUTPUT_DIR}")

    if not INPUT_DIR.exists():
        print(f"❌ Error: Input directory '{INPUT_DIR}' not found.")
        return

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    ASSETS_DIR.mkdir(parents=True, exist_ok=True)

    # 1. Move Shared Assets
    for asset in ["shared-styles.css", "shared-scripts.js"]:
        src_file = INPUT_DIR / asset
        if src_file.exists():
            dest_file = ASSETS_DIR / asset
            dest_file.write_text(src_file.read_text(encoding="utf-8"), encoding="utf-8")
            print(f"✅ Moved asset: {asset} -> {ASSETS_DIR}")

    # 2. Process HTML Files
    html_files = list(INPUT_DIR.glob("*.html"))
    if not html_files:
        print(f"⚠️ Warning: No HTML files found in '{INPUT_DIR}'.")
        return

    for html_file in html_files:
        if html_file.name in ["index.html", "shared-styles.css", "shared-scripts.js"]:
            continue

        print(f"📄 Processing {html_file.name}...")
        soup = BeautifulSoup(html_file.read_text(encoding="utf-8"), "html.parser")

        # Extract Frontmatter
        h1 = soup.find("h1")
        title = clean_text(h1.text) if h1 else "Untitled"

        category_div = soup.find("div", class_="category")
        category = clean_text(category_div.text) if category_div else "General"

        status_span = soup.find("span", class_="status")
        status = clean_text(status_span.text) if status_span else "Working"

        parity_div = soup.find("div", class_="parity")
        parity = ""
        if parity_div:
            parity_span = parity_div.find("span")
            if parity_span:
                parity = clean_text(parity_span.text)

        # Extract Main Content
        main_content = soup.find("main", class_="main")
        if not main_content:
            print(f"  ⚠️ Skipping {html_file.name}: No <main> found.")
            continue

        page_head = main_content.find("header", class_="page-head")
        articles = main_content.find_all("article", class_="acc")

        content_html = ""
        if page_head:
            content_html += str(page_head) + "\n\n"
        for article in articles:
            content_html += str(article) + "\n\n"

        # REMOVED: {% raw %} wrapper is no longer needed because markdownTemplateEngine is false.
        # The Markdown content is now treated as pure HTML/Markdown, not a Nunjucks template.

        # Construct Markdown with YAML Frontmatter
        slug = html_file.stem
        md_content = f"""---
title: "{title}"
category: "{category}"
status: "{status}"
parity: "{parity}"
layout: base.njk
---

{content_html}"""

        output_path = OUTPUT_DIR / f"{slug}.md"
        output_path.write_text(md_content, encoding="utf-8")

    print(
        f"\n✅ Migration Complete! {len(html_files)} files converted to Eleventy Markdown."
    )
    print("👉 Next steps: Run 'npm run dev' to preview the 11ty site.")


if __name__ == "__main__":
    migrate()
