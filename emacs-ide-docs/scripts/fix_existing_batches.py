#!/usr/bin/env python3
"""
fix_existing_batches.py

One-time patch script for the already-generated 20-batch output.
Does NOT call any LLM and does NOT regenerate any file from scratch —
it only edits the 40 existing feature HTML files and shared-scripts.js
in place to apply two fixes:

  1. class="language-lisp"  ->  class="language-elisp"
     (Prism's dedicated Elisp tokenizer, not the generic Lisp one)

  2. copyCode() in shared-scripts.js: try navigator.clipboard.writeText()
     first, fall back to the textarea/execCommand('copy') approach only
     if the Clipboard API is unavailable (execCommand alone is deprecated).

Usage:
    ./scripts/fix_existing_batches.py            # apply fixes
    ./scripts/fix_existing_batches.py --dry-run   # report only, change nothing
    ./scripts/fix_existing_batches.py --dir path/to/features

A .bak backup is written next to every file that gets changed.
Run ./scripts/validate.sh afterward to confirm everything now passes.
"""

import argparse
import sys
from pathlib import Path

NEW_COPY_CODE = '''async function copyCode(btn) {
  const codeEl = btn.closest(".code-win").querySelector("code");
  const text = codeEl.innerText;

  try {
    if (navigator.clipboard && window.isSecureContext) {
      await navigator.clipboard.writeText(text);
      triggerCopySuccess(btn);
      return;
    }
    throw new Error("Clipboard API unavailable");
  } catch (err) {
    // Legacy fallback for non-secure contexts only — execCommand is deprecated per MDN
    const textArea = document.createElement("textarea");
    textArea.value = text;
    textArea.style.position = "fixed";
    textArea.style.top = "0";
    textArea.style.left = "0";
    textArea.style.width = "2em";
    textArea.style.height = "2em";
    textArea.style.padding = "0";
    textArea.style.border = "none";
    textArea.style.outline = "none";
    textArea.style.boxShadow = "none";
    textArea.style.background = "transparent";
    document.body.appendChild(textArea);

    textArea.focus();
    textArea.select();

    try {
      const successful = document.execCommand("copy");
      if (successful) {
        triggerCopySuccess(btn);
      } else {
        console.warn("Fallback copy command was unsuccessful");
      }
    } catch (fallbackErr) {
      console.error("Fallback: Oops, unable to copy", fallbackErr);
    }

    document.body.removeChild(textArea);
  }
}'''


def replace_function(text: str, func_name: str, new_code: str):
    """Brace-counting replace of a top-level `function name(...) { ... }` block.
    Robust to indentation/formatting differences from regex matching."""
    start_marker = f"function {func_name}("
    start = text.find(start_marker)
    if start == -1:
        return text, False

    brace_start = text.find("{", start)
    if brace_start == -1:
        return text, False

    depth = 0
    i = brace_start
    while i < len(text):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                break
        i += 1
    else:
        return text, False  # unbalanced braces, don't touch it

    end = i + 1
    return text[:start] + new_code.strip() + text[end:], True


def fix_html_files(features_dir: Path, dry_run: bool):
    changed, unchanged = [], []
    for html_file in sorted(features_dir.glob("*.html")):
        text = html_file.read_text(encoding="utf-8")
        if 'class="language-lisp"' in text:
            new_text = text.replace('class="language-lisp"', 'class="language-elisp"')
            n = text.count('class="language-lisp"')
            if not dry_run:
                html_file.with_suffix(".html.bak").write_text(text, encoding="utf-8")
                html_file.write_text(new_text, encoding="utf-8")
            changed.append((html_file.name, n))
        else:
            unchanged.append(html_file.name)
    return changed, unchanged


def fix_shared_scripts(features_dir: Path, dry_run: bool):
    js_file = features_dir / "shared-scripts.js"
    if not js_file.exists():
        return "missing"

    text = js_file.read_text(encoding="utf-8")
    if "navigator.clipboard" in text:
        return "already-fixed"

    new_text, ok = replace_function(text, "copyCode", NEW_COPY_CODE)
    if not ok:
        return "pattern-not-found"

    if not dry_run:
        js_file.with_suffix(".js.bak").write_text(text, encoding="utf-8")
        js_file.write_text(new_text, encoding="utf-8")
    return "patched"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dir", default="features", help="Path to the features/ directory (default: features)")
    parser.add_argument("--dry-run", action="store_true", help="Report what would change without writing files")
    args = parser.parse_args()

    features_dir = Path(args.dir)
    if not features_dir.is_dir():
        print(f"Directory not found: {features_dir}", file=sys.stderr)
        sys.exit(1)

    mode = "DRY RUN — no files will be modified" if args.dry_run else "APPLYING FIXES"
    print(f"=== {mode} ===\n")

    print("-- HTML: class=\"language-lisp\" -> class=\"language-elisp\" --")
    changed, unchanged = fix_html_files(features_dir, args.dry_run)
    for name, n in changed:
        verb = "would fix" if args.dry_run else "fixed"
        print(f"  ✅ {verb} {n} occurrence(s) in {name}")
    print(f"  {len(changed)} file(s) needed the fix, {len(unchanged)} already clean.\n")

    print("-- shared-scripts.js: Clipboard API fix --")
    result = fix_shared_scripts(features_dir, args.dry_run)
    if result == "patched":
        verb = "would patch" if args.dry_run else "patched"
        print(f"  ✅ {verb} copyCode() to use navigator.clipboard.writeText() first")
    elif result == "already-fixed":
        print("  ✅ already uses navigator.clipboard — no change needed")
    elif result == "missing":
        print("  ⚠️  features/shared-scripts.js not found — check --dir path")
    elif result == "pattern-not-found":
        print("  ⚠️  could not locate a copyCode() function to patch — check the file manually")

    if not args.dry_run and (changed or result == "patched"):
        print("\nBackups written as *.bak next to each modified file.")
        print("Run ./scripts/validate.sh next to confirm everything passes.")


if __name__ == "__main__":
    main()
