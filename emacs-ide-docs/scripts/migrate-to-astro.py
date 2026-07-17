#!/usr/bin/env python3
"""
Phase 3: Strangler Pattern Migration Script (Final Verified Version)
Converts the 40 raw HTML feature files from Phase 2 into Astro 5.x Standard Markdown
Content Collections.

CRITICAL FIX: Outputs `.md` instead of `.mdx`.
Elisp code contains angle brackets like `<escape>` and `<mouse-1>`.
MDX's JSX parser interprets these as unclosed components, causing fatal build errors.
Standard Markdown (`.md`) uses `remark` and treats fenced code blocks as literal raw text,
completely bypassing the JSX parser while retaining full Astro Content Collection features.
"""

import os
import glob
import re
from bs4 import BeautifulSoup

INPUT_DIR = "features"
OUTPUT_DIR = "src/content/docs"


def clean_text(text):
    """Sanitize text for YAML frontmatter."""
    if not text:
        return ""
    text = re.sub(r"\s+", " ", text).strip()
    return text.replace('"', '\\"')


def migrate():
    print(f"🚀 Starting Phase 3 Migration: {INPUT_DIR} -> {OUTPUT_DIR}")

    if not os.path.exists(INPUT_DIR):
        print(f"❌ Error: Input directory '{INPUT_DIR}' not found.")
        return

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Clear previous MD/MDX files to prevent stale data
    for old_file in glob.glob(os.path.join(OUTPUT_DIR, "*.md*")):
        os.remove(old_file)

    html_files = glob.glob(os.path.join(INPUT_DIR, "*.html"))
    if not html_files:
        print(f"⚠️ Warning: No HTML files found in '{INPUT_DIR}'.")
        return

    for html_file in html_files:
        filename = os.path.basename(html_file)
        print(f"📄 Processing {filename}...")

        with open(html_file, "r", encoding="utf-8") as f:
            soup = BeautifulSoup(f, "html.parser")

        # Extract Frontmatter Data
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

        # Extract Page Header and Main Content (The 6 Accordions)
        main_content = soup.find("main", class_="main")
        page_head = (
            main_content.find("header", class_="page-head") if main_content else None
        )
        articles = (
            main_content.find_all("article", class_="acc") if main_content else []
        )

        # Construct Standard Markdown
        md_content = f"""---
title: "{title}"
category: "{category}"
status: "{status}"
parity: "{parity}"
---

"""
        # Use decode_contents(formatter="html") to preserve &lt; and &gt; entities
        if page_head:
            md_content += page_head.decode_contents(formatter="html") + "\n\n"

        for article in articles:
            md_content += article.decode_contents(formatter="html") + "\n\n"

        # Write to Content Collection as standard Markdown (.md)
        slug = filename.replace(".html", ".md")
        output_path = os.path.join(OUTPUT_DIR, slug)
        with open(output_path, "w", encoding="utf-8") as f:
            f.write(md_content)

    print(
        f"\n✅ Migration Complete! {len(html_files)} files converted to Standard Markdown (.md)."
    )
    print("👉 Next steps: Run 'npm run dev' to preview the Astro 5.x site.")


if __name__ == "__main__":
    migrate()
