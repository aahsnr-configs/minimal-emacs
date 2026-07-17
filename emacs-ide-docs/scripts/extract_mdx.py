#!/usr/bin/env python3
"""
extract_mdx.py
Parses the 40 tactical HTML files and converts them into Astro MDX Content Collections
with YAML frontmatter and <Accordion> component tags.
"""

import re
import os
from pathlib import Path


def extract_frontmatter(html):
    title = re.search(r"<h1>(.*?)</h1>", html)
    category = re.search(r'<div class="category">(.*?)</div>', html)
    status = re.search(r'<span class="status"[^>]*>(.*?)</span>', html)
    parity = re.search(r'<div class="parity">.*?<span>(.*?)</span>', html, re.DOTALL)

    return {
        "title": title.group(1).strip() if title else "Untitled",
        "category": category.group(1).strip() if category else "General",
        "status": status.group(1).strip() if status else "Working",
        "vscode_parity": parity.group(1).strip().replace("\n", " ") if parity else "",
    }


def extract_accordions(html):
    accs = re.findall(r'<article class="acc">(.*?)</article>', html, re.DOTALL)
    sections = []
    for acc in accs:
        title_match = re.search(
            r'<span class="t">\s*(?:<svg.*?</svg>)?\s*(.*?)\s*</span>', acc, re.DOTALL
        )
        title = title_match.group(1).strip() if title_match else "Section"

        inner_match = re.search(
            r'<div class="acc-inner">(.*?)</div>\s*</div>\s*</div>', acc, re.DOTALL
        )
        inner = inner_match.group(1).strip() if inner_match else ""

        sections.append({"title": title, "content": inner})
    return sections


def main():
    features_dir = Path("features")
    out_dir = Path("src/content/features")
    out_dir.mkdir(parents=True, exist_ok=True)

    count = 0
    for html_file in features_dir.glob("*.html"):
        if html_file.name == "index.html":
            continue

        html = html_file.read_text()
        meta = extract_frontmatter(html)
        sections = extract_accordions(html)

        slug = html_file.stem
        cat_slug = re.sub(r"[^a-z0-9]+", "-", meta["category"].lower()).strip("-")

        cat_dir = out_dir / cat_slug
        cat_dir.mkdir(exist_ok=True)

        fm = f"""---
title: "{meta["title"]}"
category: "{meta["category"]}"
status: "{meta["status"]}"
vscode_parity: "{meta["vscode_parity"]}"
---
"""
        mdx_content = fm
        for sec in sections:
            # Escape quotes in title for MDX attributes
            safe_title = sec["title"].replace('"', '\\"')
            mdx_content += f'\n<Accordion title="{safe_title}">\n\n{sec["content"]}\n\n</Accordion>\n'

        (cat_dir / f"{slug}.mdx").write_text(mdx_content)
        count += 1

    print(f"✅ Successfully extracted {count} features into src/content/features/")


if __name__ == "__main__":
    main()
