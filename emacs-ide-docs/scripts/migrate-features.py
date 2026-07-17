#!/usr/bin/env python3
"""
migrate-features.py
Migrates tactical HTML files from the 20-batch workflow into Astro 6.x MDX files
with proper frontmatter and component calls.

Usage:
    python scripts/migrate-features.py --input-dir features --output-dir src/content/features
    python scripts/migrate-features.py --input-dir features --output-dir src/content/features --dry-run
"""

import argparse
import re
import sys
from pathlib import Path
from html import unescape

try:
    from bs4 import BeautifulSoup, Tag
except ImportError:
    print(
        "❌ BeautifulSoup4 is required. Install with: pip install beautifulsoup4 lxml",
        file=sys.stderr,
    )
    sys.exit(1)


def extract_text(el: Tag) -> str:
    """Extract text content from a BeautifulSoup element, decoding HTML entities."""
    if el is None:
        return ""
    return unescape(el.get_text(strip=True))


def extract_inner_html(el: Tag) -> str:
    """Extract inner HTML, converting inline code and kbd to MDX-safe format."""
    if el is None:
        return ""
    html = el.decode_contents()
    # Decode HTML entities
    html = unescape(html)
    return html.strip()


def slugify(text: str) -> str:
    """Convert a title to a URL-safe slug."""
    text = text.lower().strip()
    text = re.sub(r"[^\w\s-]", "", text)
    text = re.sub(r"[\s_]+", "-", text)
    text = re.sub(r"-+", "-", text)
    return text.strip("-")


def parse_html_file(html_path: Path) -> dict:
    """Parse a tactical HTML file and extract all metadata and content."""
    content = html_path.read_text(encoding="utf-8")
    soup = BeautifulSoup(content, "lxml")

    data = {}

    # ---- Extract page header metadata ----
    title_el = soup.find("h1")
    data["title"] = (
        extract_text(title_el) if title_el else html_path.stem.replace("-", " ").title()
    )

    category_el = soup.find("div", class_="category")
    data["category"] = extract_text(category_el) if category_el else "General"

    data["slug"] = html_path.stem

    # Status
    status_el = soup.find("span", class_="status")
    data["status"] = "working"

    # VS Code Parity
    parity_el = soup.find("div", class_="parity")
    if parity_el:
        parity_span = parity_el.find("span")
        if parity_span:
            parity_text = extract_text(parity_span)
            data["vscode_parity"] = [
                p.strip() for p in parity_text.split("·") if p.strip()
            ]
        else:
            data["vscode_parity"] = []
    else:
        data["vscode_parity"] = []

    # Meta bar — LSP methods and routing
    meta_bar = soup.find("div", class_="meta-bar")
    data["lsp_methods"] = []
    data["routing"] = ""

    if meta_bar:
        meta_items = meta_bar.find_all("div", class_="meta-item")
        for item in meta_items:
            k_el = item.find("span", class_="k")
            if not k_el:
                continue
            k = extract_text(k_el).upper()
            if k == "LSP":
                # Extract all <code> elements as LSP methods
                codes = item.find_all("code")
                data["lsp_methods"] = [
                    extract_text(c) for c in codes if extract_text(c)
                ]
            elif k == "ROUTING":
                code_el = item.find("code")
                if code_el:
                    # Replace route arrows with →
                    routing_html = code_el.decode_contents()
                    routing_text = unescape(routing_html)
                    routing_text = routing_text.replace(
                        '<span class="route-arrow">→</span>', "→"
                    )
                    routing_text = re.sub(r"<[^>]+>", "", routing_text)
                    data["routing"] = routing_text.strip()

    # ---- Extract accordion content ----
    accordions = soup.find_all("article", class_="acc")
    data["accordions"] = []

    for acc in accordions:
        head = acc.find("button", class_="acc-head")
        body = acc.find("div", class_="acc-body")

        if not head or not body:
            continue

        # Extract title
        title_span = head.find("span", class_="t")
        acc_title = extract_text(title_span) if title_span else "Untitled"

        # Determine icon type from title
        icon = "grid"
        if "ecosystem" in acc_title.lower():
            icon = "globe"
        elif "stack" in acc_title.lower() or "implementation" in acc_title.lower():
            icon = "stack"
        elif "command" in acc_title.lower() or "keybinding" in acc_title.lower():
            icon = "keyboard"
        elif "config" in acc_title.lower():
            icon = "code"
        elif "architect" in acc_title.lower() or "enhancement" in acc_title.lower():
            icon = "book"

        # Check if default open
        is_open = "open" in head.get("class", [])

        # Extract inner content
        inner = body.find("div", class_="acc-inner")
        if not inner:
            continue

        acc_data = {
            "title": acc_title,
            "icon": icon,
            "id": slugify(acc_title),
            "defaultOpen": is_open,
            "content": [],  # Will be populated with MDX component calls
        }

        # Parse the inner content into component calls
        acc_data["content"] = parse_acc_inner(inner, data)
        data["accordions"].append(acc_data)

    # ---- Extract config filename from code window ----
    code_win = soup.find("div", class_="code-win")
    if code_win:
        fname_el = code_win.find("span", class_="fname")
        data["config_filename"] = extract_text(fname_el) if fname_el else "init.el"
    else:
        data["config_filename"] = "init.el"

    return data


def parse_acc_inner(inner: Tag, page_data: dict) -> list:
    """Parse the inner content of an accordion into MDX component calls."""
    components = []

    for child in inner.children:
        if not isinstance(child, Tag):
            continue

        # Parity Matrix table
        if child.find("table", class_="tbl"):
            table = child.find("table", class_="tbl")
            rows = []
            tbody = table.find("tbody")
            if tbody:
                for tr in tbody.find_all("tr"):
                    tds = tr.find_all("td")
                    if len(tds) >= 2:
                        # Check if this is a command table (4 columns) or parity matrix (2 columns)
                        pass
                    # We'll handle this below

            # Determine table type by checking headers
            thead = table.find("thead")
            if thead:
                headers = [extract_text(th) for th in thead.find_all("th")]
                if "Action" in headers and "Command" in headers:
                    # Command table
                    rows = []
                    if tbody:
                        for tr in tbody.find_all("tr"):
                            tds = tr.find_all("td")
                            if len(tds) >= 3:
                                row = {
                                    "action": extract_text(tds[0]),
                                    "cmd": extract_text(tds[1]),
                                    "key": extract_inner_html(tds[2]),
                                    "notes": extract_text(tds[3])
                                    if len(tds) > 3
                                    else "",
                                }
                                rows.append(row)
                    components.append(
                        {
                            "type": "CommandTable",
                            "items": rows,
                        }
                    )
                elif "VS Code Behavior" in headers:
                    # Parity matrix
                    rows = []
                    if tbody:
                        for tr in tbody.find_all("tr"):
                            tds = tr.find_all("td")
                            if len(tds) >= 2:
                                row = {
                                    "vscode": extract_inner_html(tds[0]),
                                    "emacs": extract_inner_html(tds[1]),
                                }
                                rows.append(row)
                    components.append(
                        {
                            "type": "ParityMatrix",
                            "items": rows,
                        }
                    )

        # Grid of cards
        grid = child.find("div", class_="grid-2")
        if grid:
            # Check card type
            stack_cards = grid.find_all("div", class_="stack-card")
            eco_cards = grid.find_all("div", class_="eco-card")
            vs_cards = grid.find_all("div", class_="vs-card")
            enh_cards = grid.find_all("div", class_="enh-card")

            if stack_cards:
                items = []
                for card in stack_cards:
                    name_el = card.find("div", class_="stack-name")
                    role_el = card.find("div", class_="stack-role")
                    desc_el = card.find("div", class_="stack-desc")
                    ic_el = card.find("div", class_="stack-ic")

                    # Determine color from inline style
                    color = "blue"
                    if ic_el and ic_el.get("style"):
                        style = ic_el["style"]
                        if "purple" in style or "#bb9af7" in style:
                            color = "purple"
                        elif "cyan" in style or "#7dcfff" in style:
                            color = "cyan"
                        elif "green" in style or "#9ece6a" in style:
                            color = "green"
                        elif "yellow" in style or "#e0af68" in style:
                            color = "yellow"
                        elif "orange" in style or "#ff9e64" in style:
                            color = "orange"

                    items.append(
                        {
                            "name": extract_text(name_el),
                            "role": extract_text(role_el),
                            "desc": extract_inner_html(desc_el),
                            "color": color,
                        }
                    )
                components.append(
                    {
                        "type": "StackGrid",
                        "items": items,
                    }
                )

            elif eco_cards:
                items = []
                for card in eco_cards:
                    name_el = card.find("div", class_="eco-name")
                    sub_el = card.find("div", class_="eco-sub")
                    desc_el = card.find("p", class_="eco-desc")
                    ic_el = card.find("div", class_="eco-ic")

                    color = "blue"
                    if ic_el and ic_el.get("style"):
                        style = ic_el["style"]
                        if "purple" in style:
                            color = "purple"
                        elif "cyan" in style:
                            color = "cyan"
                        elif "green" in style:
                            color = "green"
                        elif "yellow" in style:
                            color = "yellow"

                    items.append(
                        {
                            "name": extract_text(name_el),
                            "sub": extract_text(sub_el),
                            "desc": extract_inner_html(desc_el),
                            "color": color,
                        }
                    )
                components.append(
                    {
                        "type": "EcoGrid",
                        "items": items,
                    }
                )

            elif vs_cards:
                for card in vs_cards:
                    variant = "ok" if "ok" in card.get("class", []) else "no"
                    h4 = card.find("h4")
                    title = extract_text(h4) if h4 else ""
                    # Remove the ✓ or ✕ prefix
                    title = re.sub(r"^[✓✕]\s*", "", title)

                    rows = []
                    vs_rows = card.find_all("div", class_="vs-row")
                    for row in vs_rows:
                        lab = row.find("span", class_="lab")
                        val = row.find("span", class_="val")
                        rows.append(
                            {
                                "label": extract_text(lab),
                                "value": extract_inner_html(val),
                            }
                        )

                    components.append(
                        {
                            "type": "VsCard",
                            "variant": variant,
                            "title": title,
                            "rows": rows,
                        }
                    )

            elif enh_cards:
                for card in enh_cards:
                    classes = card.get("class", [])
                    color = "g"
                    if "p" in classes:
                        color = "p"
                    elif "y" in classes:
                        color = "y"

                    title_el = card.find("div", class_="enh-title")
                    desc_el = card.find("p", class_="enh-desc")

                    components.append(
                        {
                            "type": "EnhCard",
                            "title": extract_text(title_el),
                            "desc": extract_inner_html(desc_el),
                            "color": color,
                        }
                    )

        # Code window
        code_win = child.find("div", class_="code-win")
        if code_win:
            fname_el = code_win.find("span", class_="fname")
            filename = extract_text(fname_el) if fname_el else "init.el"

            code_el = code_win.find("code")
            if code_el:
                code_text = unescape(code_el.get_text())
                components.append(
                    {
                        "type": "CodeWindow",
                        "filename": filename,
                        "code": code_text,
                        "lang": "emacs-lisp",
                    }
                )

        # Section title (text heading within accordion)
        sec_title = child.find("div", class_="sec-title")
        if (
            sec_title
            and not child.find("div", class_="grid-2")
            and not child.find("table")
        ):
            # This is a standalone section title — we'll include it as a comment
            pass

    return components


def generate_frontmatter(data: dict) -> str:
    """Generate YAML frontmatter from parsed data."""
    lines = ["---"]
    lines.append(f'title: "{data["title"]}"')
    lines.append(f'category: "{data["category"]}"')
    lines.append(f'slug: "{data["slug"]}"')
    lines.append(f'status: "{data["status"]}"')

    # VS Code Parity
    if data["vscode_parity"]:
        lines.append("vscode_parity:")
        for item in data["vscode_parity"]:
            lines.append(f'  - "{escape_yaml(item)}"')

    # LSP Methods
    if data["lsp_methods"]:
        lines.append("lsp_methods:")
        for method in data["lsp_methods"]:
            lines.append(f'  - "{escape_yaml(method)}"')

    # Routing
    if data["routing"]:
        lines.append(f'routing: "{escape_yaml(data["routing"])}"')

    # Stack
    stack_items = []
    for acc in data["accordions"]:
        for comp in acc["content"]:
            if comp["type"] == "StackGrid":
                stack_items = comp["items"]
                break

    if stack_items:
        lines.append("stack:")
        for item in stack_items:
            lines.append(f'  - name: "{escape_yaml(item["name"])}"')
            lines.append(f'    role: "{escape_yaml(item["role"])}"')
            lines.append(f'    desc: "{escape_yaml(item["desc"])}"')
            lines.append(f'    color: "{item["color"]}"')

    # Ecosystem
    eco_items = []
    for acc in data["accordions"]:
        for comp in acc["content"]:
            if comp["type"] == "EcoGrid":
                eco_items = comp["items"]
                break

    if eco_items:
        lines.append("ecosystem:")
        for item in eco_items:
            lines.append(f'  - name: "{escape_yaml(item["name"])}"')
            lines.append(f'    sub: "{escape_yaml(item["sub"])}"')
            lines.append(f'    desc: "{escape_yaml(item["desc"])}"')
            lines.append(f'    color: "{item["color"]}"')

    # Commands
    cmd_items = []
    for acc in data["accordions"]:
        for comp in acc["content"]:
            if comp["type"] == "CommandTable":
                cmd_items = comp["items"]
                break

    if cmd_items:
        lines.append("commands:")
        for item in cmd_items:
            lines.append(f'  - action: "{escape_yaml(item["action"])}"')
            lines.append(f'    cmd: "{escape_yaml(item["cmd"])}"')
            lines.append(f'    key: "{escape_yaml(item["key"])}"')
            if item.get("notes"):
                lines.append(f'    notes: "{escape_yaml(item["notes"])}"')

    # Enhancements
    enh_items = []
    for acc in data["accordions"]:
        for comp in acc["content"]:
            if comp["type"] == "EnhCard":
                enh_items.append(comp)

    if enh_items:
        lines.append("enhancements:")
        for item in enh_items:
            lines.append(f'  - title: "{escape_yaml(item["title"])}"')
            lines.append(f'    desc: "{escape_yaml(item["desc"])}"')
            lines.append(f'    color: "{item["color"]}"')

    # Parity Matrix
    parity_items = []
    for acc in data["accordions"]:
        for comp in acc["content"]:
            if comp["type"] == "ParityMatrix":
                parity_items = comp["items"]
                break

    if parity_items:
        lines.append("parity_matrix:")
        for item in parity_items:
            lines.append(f'  - vscode: "{escape_yaml(item["vscode"])}"')
            lines.append(f'    emacs: "{escape_yaml(item["emacs"])}"')

    # Config filename
    lines.append(f'config_filename: "{data["config_filename"]}"')

    # Rejected alternative
    vs_cards = []
    for acc in data["accordions"]:
        for comp in acc["content"]:
            if comp["type"] == "VsCard":
                vs_cards.append(comp)

    rejected = [c for c in vs_cards if c["variant"] == "no"]
    if rejected:
        rej = rejected[0]
        lines.append("rejected_alternative:")
        lines.append(f'  name: "{escape_yaml(rej["title"])}"')
        lines.append("  reasons:")
        for row in rej["rows"]:
            lines.append(f'    - label: "{escape_yaml(row["label"])}"')
            lines.append(f'      value: "{escape_yaml(row["value"])}"')

    lines.append("---")
    return "\n".join(lines)


def escape_yaml(text: str) -> str:
    """Escape a string for safe inclusion in YAML double-quoted strings."""
    if not text:
        return ""
    text = text.replace("\\", "\\\\")
    text = text.replace('"', '\\"')
    text = text.replace("\n", "\\n")
    text = text.replace("\r", "")
    return text


def generate_mdx_body(data: dict) -> str:
    """Generate the MDX body content with component calls."""
    lines = []
    lines.append("")
    lines.append(
        "import { Accordion, ParityMatrix, StackGrid, EcoGrid, VsCard, EnhCard, CodeWindow, CommandTable } from '../../components';"
    )
    lines.append("")

    for acc in data["accordions"]:
        lines.append(
            f'<Accordion title="{escape_attr(acc["title"])}" icon="{acc["icon"]}" id="{acc["id"]}"{" defaultOpen" if acc["defaultOpen"] else ""}>'
        )

        for comp in acc["content"]:
            if comp["type"] == "ParityMatrix":
                lines.append("  <ParityMatrix items={[")
                for item in comp["items"]:
                    lines.append(
                        f"    {{ vscode: `{escape_jsx(item['vscode'])}`, emacs: `{escape_jsx(item['emacs'])}` }},"
                    )
                lines.append("  ]} />")

            elif comp["type"] == "StackGrid":
                lines.append("  <StackGrid items={[")
                for item in comp["items"]:
                    lines.append(
                        f'    {{ name: "{escape_jsx(item["name"])}", role: "{escape_jsx(item["role"])}", desc: `{escape_jsx(item["desc"])}`, color: "{item["color"]}" }},'
                    )
                lines.append("  ]} />")

            elif comp["type"] == "EcoGrid":
                lines.append("  <EcoGrid items={[")
                for item in comp["items"]:
                    lines.append(
                        f'    {{ name: "{escape_jsx(item["name"])}", sub: "{escape_jsx(item["sub"])}", desc: `{escape_jsx(item["desc"])}`, color: "{item["color"]}" }},'
                    )
                lines.append("  ]} />")

            elif comp["type"] == "CommandTable":
                lines.append("  <CommandTable items={[")
                for item in comp["items"]:
                    notes = (
                        f', notes: "{escape_jsx(item["notes"])}"'
                        if item.get("notes")
                        else ""
                    )
                    lines.append(
                        f'    {{ action: "{escape_jsx(item["action"])}", cmd: "{escape_jsx(item["cmd"])}", key: `{escape_jsx(item["key"])}`{notes} }},'
                    )
                lines.append("  ]} />")

            elif comp["type"] == "VsCard":
                lines.append(
                    f'  <VsCard variant="{comp["variant"]}" title="{escape_attr(comp["title"])}" rows={{['
                )
                for row in comp["rows"]:
                    lines.append(
                        f'    {{ label: "{escape_jsx(row["label"])}", value: `{escape_jsx(row["value"])}` }},'
                    )
                lines.append("  ]}} />")

            elif comp["type"] == "EnhCard":
                lines.append(
                    f'  <EnhCard title="{escape_attr(comp["title"])}" desc=`{escape_jsx(comp["desc"])}` color="{comp["color"]}" />'
                )

            elif comp["type"] == "CodeWindow":
                # Use template literal for code to preserve formatting
                lines.append(
                    f'  <CodeWindow filename="{escape_attr(comp["filename"])}" lang="{comp["lang"]}" code={{`{escape_template_literal(comp["code"])}`}} />'
                )

        lines.append("</Accordion>")
        lines.append("")

    return "\n".join(lines)


def escape_attr(text: str) -> str:
    """Escape a string for use in an HTML/JSX attribute."""
    if not text:
        return ""
    text = text.replace("&", "&amp;")
    text = text.replace('"', "&quot;")
    text = text.replace("<", "&lt;")
    text = text.replace(">", "&gt;")
    return text


def escape_jsx(text: str) -> str:
    """Escape a string for use inside JSX template literals or attributes."""
    if not text:
        return ""
    # For template literals, we need to escape backticks and ${
    text = text.replace("\\", "\\\\")
    text = text.replace("`", "\\`")
    text = text.replace("${", "\\${")
    return text


def escape_template_literal(text: str) -> str:
    """Escape a string for use inside a JavaScript template literal."""
    if not text:
        return ""
    text = text.replace("\\", "\\\\")
    text = text.replace("`", "\\`")
    text = text.replace("${", "\\${")
    return text


def determine_category_dir(category: str) -> str:
    """Map a category string to a directory name."""
    mapping = {
        "completion & intelligence": "completion",
        "navigation & code jumping": "navigation",
        "diagnostics & symbols": "diagnostics",
        "code actions & formatting": "code-actions",
        "formatting & editing": "formatting",
        "debugging & visual": "visual",
        "visual & diagnostics": "visual",
        "visual enhancements": "visual",
        "workspace & code actions": "code-actions",
        "navigation & visual": "navigation",
        "diagnostics & visual": "diagnostics",
        "editing & code actions": "editing",
    }
    return mapping.get(category.lower(), "general")


def main():
    parser = argparse.ArgumentParser(description="Migrate tactical HTML to Astro MDX")
    parser.add_argument(
        "--input-dir", required=True, help="Directory containing tactical HTML files"
    )
    parser.add_argument(
        "--output-dir", required=True, help="Directory to write MDX files"
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Parse and show summary without writing files",
    )

    args = parser.parse_args()

    input_dir = Path(args.input_dir)
    output_dir = Path(args.output_dir)

    if not input_dir.exists():
        print(f"❌ Input directory '{input_dir}' not found!", file=sys.stderr)
        sys.exit(1)

    html_files = sorted(input_dir.glob("*.html"))
    # Exclude index.html
    html_files = [f for f in html_files if f.name != "index.html"]

    if not html_files:
        print(f"⚠️ No HTML files found in {input_dir}", file=sys.stderr)
        sys.exit(0)

    print(f"📥 Found {len(html_files)} HTML files to migrate...", file=sys.stderr)

    migrated = 0
    errors = 0

    for html_path in html_files:
        try:
            data = parse_html_file(html_path)

            if args.dry_run:
                print(
                    f"  ✅ {html_path.name}: {data['title']} ({data['category']}) — {len(data['accordions'])} accordions",
                    file=sys.stderr,
                )
                migrated += 1
                continue

            # Generate MDX content
            frontmatter = generate_frontmatter(data)
            body = generate_mdx_body(data)
            mdx_content = f"{frontmatter}\n{body}\n"

            # Determine output path
            category_dir = determine_category_dir(data["category"])
            out_path = output_dir / category_dir / f"{data['slug']}.mdx"
            out_path.parent.mkdir(parents=True, exist_ok=True)

            # Write the file
            out_path.write_text(mdx_content, encoding="utf-8")
            print(
                f"  ✅ Migrated: {html_path.name} → {out_path.relative_to(output_dir.parent.parent)}",
                file=sys.stderr,
            )
            migrated += 1

        except Exception as e:
            print(f"  ❌ Error migrating {html_path.name}: {e}", file=sys.stderr)
            errors += 1

    print(
        f"\n🎉 Migration complete: {migrated} files migrated, {errors} errors",
        file=sys.stderr,
    )

    if errors > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
