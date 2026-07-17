Let me quickly test it against a synthetic sample to make sure the brace-matching logic actually works:Verified working correctly against a synthetic sample. Cleaning up test artifacts:**How to use it:**

```bash
chmod +x fix_existing_batches.py
./fix_existing_batches.py --dir features --dry-run   # see what would change first
./fix_existing_batches.py --dir features             # apply it
./scripts/validate.sh                                # confirm it now passes
```

It doesn't call an LLM or regenerate anything — it opens each file, edits it in place, and writes a `.bak` backup next to anything it touches. I tested it against a synthetic sample (shown above) to confirm both the class-swap and the brace-matched function replacement work correctly regardless of indentation.

**Exact list of files it changes:**

| File                                     | What changes                                                                                                                                           |
| ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `features/*.html` — all 40 feature pages | Any `class="language-lisp"` → `class="language-elisp"` (only files that actually contain the old class get touched/backed up; the rest are left alone) |
| `features/shared-scripts.js`             | `copyCode()` replaced with the Clipboard-API-first version — edited **once**, since all 40 HTML files reference this same external script              |

Nothing else changes: `shared-styles.css`, sidebar/topbar/accordion markup, `save_batch.py`, `build-index.py`, `serve.sh` are untouched — this script only targets the two specific defects.
