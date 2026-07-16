# Comprehensive Plan: Modular HTML Documentation for Emacs 31 IDE Features

## 📋 Executive Summary

Based on my analysis of the four reference files, I'll design a **modular, SSG-based documentation architecture** that transforms `ide-features.md` into a scalable, maintainable HTML documentation system using `qwen-html.md` as the visual foundation. The plan strictly adheres to the modular partials approach outlined in `project_state.yaml`, abandoning monolithic HTML in favor of component-based architecture.

---

## 🔍 Research Insights (Web Search Synthesis)

After investigating modern documentation architectures, here are the key findings that inform this plan:

### Industry Best Practices Identified:

1. **Astro + MDX** dominates 2025-2026 technical documentation (used by Starlight, Vite, Tailwind)
2. **Atomic Design + Content Collections** is the gold standard for modular docs
3. **View Transitions API** is now standard for SPA-like navigation in SSGs
4. **Island Architecture** allows interactive components without full hydration
5. **Feature-flagged sections** enable progressive documentation rollout

---

## 🏗️ Phase 1: Architecture Design

### 1.1 Technology Stack Recommendation

```
┌─────────────────────────────────────────────────────┐
│  SSG Framework: Astro 5.x (with Content Collections)│
│  Styling: Vanilla CSS (Tokyo Night tokens)          │
│  Markdown: MDX for interactive components           │
│  Search: Pagefind (static search index)             │
│  Deployment: Cloudflare Pages / Vercel             │
│  Icons: Lucide (matches current SVG style)         │
└─────────────────────────────────────────────────────┘
```

**Why Astro?**

- Zero-JS by default (matches your performance-first Emacs philosophy)
- Native content collections = perfect for 30+ feature pages
- Component islands allow interactive accordions without bloat
- Astro's `<Code>` component handles Prism.js syntax highlighting natively

### 1.2 Directory Structure (Modular Partials)

```
emacs-ide-docs/
├── src/
│   ├── content/                    # MDX source files (one per feature)
│   │   ├── completion/
│   │   │   ├── intellisense.mdx
│   │   │   ├── hover-info.mdx
│   │   │   ├── signature-help.mdx
│   │   │   └── semantic-tokens.mdx
│   │   ├── navigation/
│   │   │   ├── go-to-definition.mdx
│   │   │   ├── find-references.mdx
│   │   │   └── breadcrumbs.mdx
│   │   ├── diagnostics/
│   │   │   ├── flymake-push.mdx
│   │   │   └── flymake-pull.mdx
│   │   ├── formatting/
│   │   │   ├── apheleia.mdx
│   │   │   └── range-formatting.mdx
│   │   └── visual/
│   │       ├── minimap.mdx
│   │       ├── sticky-scroll.mdx
│   │       └── bracket-colorization.mdx
│   │
│   ├── components/                 # Reusable UI atoms
│   │   ├── layout/
│   │   │   ├── Sidebar.astro
│   │   │   ├── Topbar.astro
│   │   │   └── PageShell.astro
│   │   ├── feature/
│   │   │   ├── Accordion.astro     # Matches your .acc pattern
│   │   │   ├── ParityMatrix.astro  # Behavioral matrix tables
│   │   │   ├── StackGrid.astro     # Implementation stack cards
│   │   │   ├── EcoGrid.astro       # Ecosystem integration
│   │   │   ├── VsCard.astro        # Chosen vs Rejected comparisons
│   │   │   ├── CodeWindow.astro    # Prism-powered code blocks
│   │   │   ├── CommandTable.astro  # Keybinding tables
│   │   │   └── MetaBar.astro       # LSP/routing metadata
│   │   └── ui/
│   │       ├── Kbd.astro           # Keyboard shortcut badges
│   │       ├── StatusBadge.astro   # "Working" indicators
│   │       └── RouteArrow.astro    # eglot → capf arrows
│   │
│   ├── layouts/
│   │   └── FeatureLayout.astro     # Unified page template
│   │
│   ├── styles/
│   │   ├── tokens.css              # CSS custom properties (from qwen-html)
│   │   ├── base.css                # Reset + typography
│   │   ├── components.css          # Extracted from qwen-html
│   │   └── responsive.css          # Mobile breakpoints
│   │
│   ├── data/
│   │   └── features.yaml           # Central metadata registry
│   │
│   └── pages/
│       ├── index.astro             # Landing / feature index
│       └── [category]/[slug].astro # Dynamic feature routes
│
├── public/
│   └── fonts/                      # JetBrains Mono self-hosted
│
└── astro.config.mjs
```

### 1.3 Content Schema (YAML Frontmatter Standard)

Every `.mdx` file in `src/content/` will use this unified schema:

```yaml
---
title: "IntelliSense / Code Completion"
category: "Completion & Intelligence"
status: "working" # working | partial | planned
vscode_parity: "Autocomplete popup · ghost text · auto-imports"
lsp_methods:
  - "textDocument/completion"
  - "completionItem/resolve"
routing: "eglot → capf → cape → corfu"
stack:
  - { name: "eglot", role: "LSP Client" }
  - { name: "corfu", role: "UI Engine" }
commands:
  - { action: "Trigger", cmd: "completion-at-point", key: "TAB", notes: "..." }
emacs31_enhancements:
  - "TTY Child-Frame Support"
  - "PGTK Wayland Fixes"
---
```

This schema lets Astro auto-generate:

- Sidebar navigation (grouped by `category`)
- MetaBar component (LSP + routing)
- StackGrid component
- CommandTable component

---

## 🎨 Phase 2: Design System Extraction

### 2.1 Token Extraction from `qwen-html.md`

I'll extract these design tokens into `tokens.css`:

| Token Category | Variables to Extract                                                                                                  |
| -------------- | --------------------------------------------------------------------------------------------------------------------- |
| **Colors**     | `--bg`, `--surface`, `--border`, `--text`, `--blue`, `--purple`, `--cyan`, `--green`, `--red`, `--yellow`, `--orange` |
| **Spacing**    | `--sidebar-w: 240px`, `--topbar-h: 56px`, component paddings                                                          |
| **Typography** | JetBrains Mono import, size scale (11px → 28px)                                                                       |
| **Motion**     | `--transition-speed: 0.3s`, cubic-bezier curves                                                                       |
| **Radius**     | Card radius (10px), button radius (6px), pill radius (12px)                                                           |

### 2.2 Component Library (Atomic Design)

**Atoms (smallest):**

- `<Kbd>` — keyboard shortcut badges
- `<CodeInline>` — inline `code` styling
- `<StatusBadge>` — "Working" pill with green dot
- `<RouteArrow>` — the `→` connector with optical centering

**Molecules:**

- `<MetaBar>` — LSP + routing metadata strip
- `<ParityMatrix>` — VS Code ↔ Emacs comparison table
- `<CommandTable>` — Action/Command/Key/Notes grid
- `<VsCard>` — "Chosen" (green) vs "Rejected" (red) comparison

**Organisms:**

- `<Accordion>` — collapsible sections with chevron rotation
- `<StackGrid>` — 2-column implementation stack cards
- `<EcoGrid>` — ecosystem integration cards
- `<CodeWindow>` — macOS-style code block with copy button

**Templates:**

- `<FeatureLayout>` — full page with header + accordions
- `<PageShell>` — sidebar + topbar + main wrapper

### 2.3 Accordion Standardization

From `qwen-html.md`, every feature page uses **6 standard accordion sections**. I'll formalize this:

| Accordion                       | Purpose                         | Required? |
| ------------------------------- | ------------------------------- | --------- |
| **Feature Overview**            | Parity matrix + status          | ✅ Always |
| **Ecosystem Integration**       | Related packages                | ✅ Always |
| **Implementation Stack**        | Component cards                 | ✅ Always |
| **Commands & Keybindings**      | Action table                    | ✅ Always |
| **Configuration**               | Elisp code window               | ✅ Always |
| **Architecture & Enhancements** | Why chosen + Emacs 31 specifics | ✅ Always |

Optional accordions (for complex features):

- **Known Issues & Workarounds** (e.g., Hover Info)
- **Language-Specific Behavior** (e.g., Type Definition)
- **Troubleshooting** (e.g., Semantic Tokens)

---

## 📝 Phase 3: Content Migration Strategy

### 3.1 Feature Inventory (34 features to migrate)

Based on `ide-features.md`, here's the full migration matrix:

| #   | Feature                 | Category     | Status                 | Priority |
| --- | ----------------------- | ------------ | ---------------------- | -------- |
| 1   | IntelliSense            | Completion   | ✅ Done (in qwen-html) | —        |
| 2   | Hover Info (Native)     | Completion   | ✅ Done (in qwen-html) | —        |
| 3   | Hover Info (Childframe) | Completion   | 🟡 Needs migration     | P1       |
| 4   | Signature Help          | Completion   | 🔴 New                 | P1       |
| 5   | Go to Definition        | Navigation   | 🔴 New                 | P1       |
| 6   | Go to Declaration       | Navigation   | 🔴 New                 | P1       |
| 7   | Go to Type Definition   | Navigation   | 🔴 New                 | P1       |
| 8   | Go to Implementation    | Navigation   | 🔴 New                 | P1       |
| 9   | Find All References     | Navigation   | 🔴 New                 | P1       |
| 10  | Document Highlight      | Navigation   | 🔴 New                 | P2       |
| 11  | Document Symbols        | Diagnostics  | 🔴 New                 | P1       |
| 12  | Workspace Symbol Search | Diagnostics  | 🔴 New                 | P2       |
| 13  | Code Actions            | Code Actions | 🔴 New                 | P1       |
| 14  | Document Formatting     | Formatting   | 🔴 New                 | P1       |
| 15  | Range Formatting        | Formatting   | 🔴 New                 | P2       |
| 16  | On-type Formatting      | Formatting   | 🔴 New                 | P2       |
| 17  | Rename Symbol           | Code Actions | 🔴 New                 | P1       |
| 18  | Folding Ranges          | Formatting   | 🔴 New                 | P2       |
| 19  | Selection Range         | Formatting   | 🔴 New                 | P2       |
| 20  | Linked Editing Range    | Visual       | 🔴 New                 | P2       |
| 21  | Call Hierarchy          | Navigation   | 🔴 New                 | P2       |
| 22  | Type Hierarchy          | Navigation   | 🔴 New                 | P2       |
| 23  | Semantic Tokens         | Completion   | 🔴 New                 | P1       |
| 24  | Inlay Hints             | Completion   | 🔴 New                 | P1       |
| 25  | Inline Values           | Debugging    | 🔴 New                 | P2       |
| 26  | Document Links          | Visual       | 🔴 New                 | P2       |
| 27  | Document Color          | Visual       | 🔴 New                 | P3       |
| 28  | Diagnostics (Push)      | Diagnostics  | 🔴 New                 | P1       |
| 29  | Diagnostics (Pull)      | Diagnostics  | 🔴 New                 | P2       |
| 30  | Moniker                 | Navigation   | 🔴 New                 | P3       |
| 31  | File-Operation Hooks    | Workspace    | 🔴 New                 | P2       |
| 32  | Execute Command         | Code Actions | 🔴 New                 | P2       |
| 33  | Peek Definition         | Navigation   | 🔴 New                 | P2       |
| 34  | Breadcrumbs Bar         | Navigation   | 🔴 New                 | P1       |
| 35  | Problems Panel          | Diagnostics  | 🔴 New                 | P1       |
| 36  | Sticky Scroll           | Visual       | 🔴 New                 | P2       |
| 37  | Bracket Colorization    | Visual       | 🔴 New                 | P2       |
| 38  | Minimap                 | Visual       | 🔴 New                 | P2       |
| 39  | Multi-Cursor Editing    | Editing      | 🔴 New                 | P1       |
| 40  | Quick Fix Lightbulb     | Code Actions | 🔴 New                 | P2       |

### 3.2 Content Transformation Pipeline

For each feature, I'll execute this transformation:

```
ide-features.md (raw markdown)
    ↓
1. Extract YAML frontmatter (title, category, LSP, status)
    ↓
2. Parse markdown tables → Astro component props
   - "| VS Code | Emacs |" → <ParityMatrix rows={[...]} />
   - "| Layer | Component |" → <StackGrid items={[...]} />
   - "| Action | Command |" → <CommandTable rows={[...]} />
    ↓
3. Convert Elisp code blocks → <CodeWindow filename="init.el">
    ↓
4. Extract prose sections → accordion slots
   - "Why This Approach" → VsCard components
   - "Emacs 31 Enhancements" → Enhancement cards
   - "Integration with Stack" → EcoGrid
    ↓
5. Output: src/content/[category]/[slug].mdx
```

### 3.3 Example Transformed Feature (Hover Info - Childframe)

The output `.mdx` file would look like:

```mdx
---
title: "Hover Info (Childframe Parity)"
category: "Completion & Intelligence"
status: "working"
vscode_parity: "Floating tooltip with rich markdown"
lsp_methods: ["textDocument/hover"]
routing: "eglot → eldoc → eldoc-box"
stack:
  - { name: "eglot", role: "LSP Client" }
  - { name: "eldoc", role: "Documentation Router" }
  - { name: "eldoc-box", role: "GUI Rendering" }
---

import {
  Accordion,
  ParityMatrix,
  StackGrid,
  VsCard,
  CodeWindow,
} from "@/components";

<Accordion title="Feature Overview" open>
  <ParityMatrix
    rows={[
      ["Floating tooltip on cursor idle", "eldoc-box-hover-at-point-mode"],
      ["Rich markdown rendering", "markdown-ts-mode fontifies childframe"],
      ["Tooltip disappears on cursor move", "eldoc-box-clear-after-use t"],
    ]}
  />
</Accordion>

<Accordion title="Configuration">
  <CodeWindow filename="init-hover-childframe.el">
    {`(use-package eldoc-box
       :ensure t
       :custom
       (eldoc-box-clear-after-use t)
       (eldoc-box-only-multi-line t))`}
  </CodeWindow>
</Accordion>

<Accordion title="Architecture & Enhancements">
  <VsCard
    chosen={{ title: "eldoc-box", items: ["Childframe rendering", "..."] }}
    rejected={{ title: "peek", items: ["Overlay-based", "..."] }}
  />
</Accordion>
```

---

## 🧭 Phase 4: Navigation & Information Architecture

### 4.1 Sidebar Reorganization

Current sidebar in `qwen-html.md` has 5 items. For 40 features, we need **category grouping**:

```
📚 Emacs IDE
├─ Completion & Intelligence
│  ├─ IntelliSense ★
│  ├─ Hover Info (Native)
│  ├─ Hover Info (Childframe)
│  ├─ Signature Help
│  ├─ Semantic Tokens
│  └─ Inlay Hints
├─ Navigation & Code Jumping
│  ├─ Go to Definition
│  ├─ Go to Declaration
│  ├─ Go to Type Definition
│  ├─ Go to Implementation
│  ├─ Find All References
│  ├─ Call / Type Hierarchy
│  ├─ Peek Definition
│  └─ Breadcrumbs Bar
├─ Diagnostics & Symbols
│  ├─ Diagnostics (Push)
│  ├─ Diagnostics (Pull)
│  ├─ Document Symbols
│  ├─ Workspace Symbols
│  └─ Problems Panel
├─ Code Actions & Refactoring
│  ├─ Code Actions
│  ├─ Rename Symbol
│  ├─ Execute Command
│  └─ Quick Fix Lightbulb
├─ Formatting & Editing
│  ├─ Document Formatting
│  ├─ Range Formatting
│  ├─ On-type Formatting
│  ├─ Folding Ranges
│  ├─ Selection Range
│  └─ Multi-Cursor Editing
├─ Visual Enhancements
│  ├─ Document Highlight
│  ├─ Document Links
│  ├─ Document Color
│  ├─ Sticky Scroll
│  ├─ Bracket Colorization
│  ├─ Minimap
│  └─ Linked Editing Range
└─ Debugging
   └─ Inline Values
```

### 4.2 Cross-Reference System

Features heavily reference each other. I'll implement:

- **`<FeatureLink slug="eglot-rename">`** — auto-resolves title + status
- **"See Also" footer** on each page (auto-generated from shared `lsp_methods`)
- **Tag-based related features** (e.g., all features using `eglot`)

### 4.3 Search Integration (Pagefind)

Pagefind generates a static search index at build time:

- Search by feature name, command, keybinding, or LSP method
- Results show category breadcrumb + status badge
- Keyboard shortcut: `/` focuses search (matches VS Code `Ctrl+P`)

---

## ⚙️ Phase 5: Build & Automation

### 5.1 Build Pipeline

```bash
# Development
astro dev                    # Hot-reload with View Transitions

# Production build
astro build                  # Generates static HTML + Pagefind index
astro check                  # Type-check MDX frontmatter

# Validation
npm run lint:mdx             # Validate all frontmatter schemas
npm run lint:links           # Check for broken cross-references
```

### 5.2 CI/CD Workflow (GitHub Actions)

```
On PR:
  ├─ Lint MDX frontmatter
  ├─ Check for broken links
  ├─ Build preview deployment (Cloudflare Pages)
  └─ Run Lighthouse audit (target: 95+ performance)

On merge to main:
  ├─ Build production site
  ├─ Generate search index
  ├─ Deploy to Cloudflare Pages
  └─ Invalidate CDN cache
```

### 5.3 Content Update Workflow

When `ide-features.md` is updated:

1. **Diff detection** — identify which features changed
2. **Selective rebuild** — only rebuild affected MDX files
3. **Preview PR** — auto-generate preview with changed sections highlighted

---

## 📊 Phase 6: Execution Roadmap

### Milestone 1: Foundation (Week 1)

- [ ] Set up Astro project with Tokyo Night tokens
- [ ] Build `<Accordion>`, `<CodeWindow>`, `<ParityMatrix>` components
- [ ] Migrate IntelliSense + Hover Info (native) from `qwen-html.md` as proof-of-concept
- [ ] Implement sidebar with category grouping
- [ ] Add Pagefind search

### Milestone 2: Priority 1 Features (Week 2)

- [ ] Migrate 12 P1 features (Signature Help, Go to Def, Code Actions, etc.)
- [ ] Build `<VsCard>` and `<StackGrid>` components
- [ ] Implement cross-reference system
- [ ] Add "Emacs 31 Enhancements" card grid

### Milestone 3: Full Feature Set (Week 3)

- [ ] Migrate remaining P2/P3 features
- [ ] Build landing page with feature matrix
- [ ] Add "New in Emacs 31" highlight system
- [ ] Implement View Transitions for SPA-like navigation

### Milestone 4: Polish & Launch (Week 4)

- [ ] Mobile responsiveness audit
- [ ] Accessibility pass (ARIA, keyboard nav, color contrast)
- [ ] Performance optimization (target: <100KB per page)
- [ ] Documentation for content contributors

---

## 🎯 Key Design Decisions

| Decision                         | Rationale                                                                                    |
| -------------------------------- | -------------------------------------------------------------------------------------------- |
| **Astro over Next.js**           | Zero-JS default aligns with Emacs' performance philosophy; MDX provides needed interactivity |
| **MDX over pure Markdown**       | Allows embedding Astro components directly in content (e.g., `<CodeWindow>`)                 |
| **Content Collections over CMS** | Git-native, version-controlled, no external dependencies                                     |
| **Pagefind over Algolia**        | Static, privacy-friendly, zero server costs                                                  |
| **Vanilla CSS over Tailwind**    | Matches existing `qwen-html.md` token system; easier maintenance                             |
| **Island Architecture**          | Accordions hydrate only when clicked → instant page loads                                    |

---

## ⚠️ Risks & Mitigations

| Risk                                                      | Mitigation                                                                      |
| --------------------------------------------------------- | ------------------------------------------------------------------------------- |
| **Content drift** between `ide-features.md` and MDX files | Single source of truth: MDX files. Keep `ide-features.md` as a generated index. |
| **Accordion state lost on navigation**                    | Use View Transitions API + URL hash for open sections                           |
| **Large code blocks slow page load**                      | Lazy-load `<CodeWindow>` below fold; Prism on-demand                            |
| **Mobile sidebar UX**                                     | Implement swipe-to-open (matches `qwen-html.md` hamburger pattern)              |

---

## 🚀 Next Steps

**To begin execution, I need your `GREEN LIGHT` to:**

1. **Generate the Astro project scaffolding** (directory structure, config files)
2. **Extract design tokens** from `qwen-html.md` into `tokens.css`
3. **Build the first 3 components**: `<Accordion>`, `<ParityMatrix>`, `<CodeWindow>`
4. **Migrate the "Signature Help" feature** as the first new feature (P1, straightforward structure)

Which milestone would you like me to start with? Or would you like me to refine any aspect of this plan before proceeding?
