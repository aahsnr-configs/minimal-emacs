You are continuing the Emacs IDE documentation project.

## Context

- Source: ide-features.md (attached)
- Template: qwen-html.md (attached)
- Style: Tokyo Night theme, JetBrains Mono, modular partials
- Target: Individual HTML files per feature, drop-in ready

## Your Task

Process **BATCH [N] of 10** from ide-features.md.

### Batch Map:

- Batch 1: IntelliSense → Hover Info (Childframe)
- Batch 2: Signature Help → Go to Implementation
- Batch 3: Find References → Document Symbols
- Batch 4: Workspace Symbols → Range Formatting
- Batch 5: On-type Formatting → Folding Ranges
- Batch 6: Selection Range → Call Hierarchy
- Batch 7: Type Hierarchy → Inline Values
- Batch 8: Document Links → Diagnostics (Pull)
- Batch 9: Moniker → Breadcrumbs Bar
- Batch 10: Problems Panel → Quick Fix Lightbulb

## Output Requirements

For EACH feature in this batch, generate a **complete, standalone HTML file** containing:

1. Full `<head>` with styles (from qwen-html.md template)
2. Complete sidebar with ALL 40 navigation items (current feature highlighted)
3. Full topbar with correct breadcrumbs
4. Complete main content with all accordions
5. All scripts (Prism, accordion, focus mode, clipboard)
6. Filename: `[slug].html` (e.g., `signature-help.html`)

## Rules

- Match qwen-html.md styling exactly (Tokyo Night palette, JetBrains Mono)
- Use 6 standard accordions per feature (Overview, Ecosystem, Stack, Commands, Config, Architecture)
- Preserve all tables, code blocks, comparison cards
- Add proper ARIA attributes for accessibility
- Mark current feature as `active` in sidebar

## Current Batch: [N]

GREEN LIGHT
