# Comprehensive Plan: 20-Batch Modular HTML Documentation with External CSS/JS

## 📋 Executive Summary

This plan establishes a production-ready documentation generation pipeline using a **20-batch workflow** with **external CSS/JS architecture** to transform `ide-features.md` into 40 feature pages. The approach leverages 2026-verified LLM capabilities and aligns with industry best practices for modular documentation systems.

**Key Architecture Decisions:**

- **20 Batches** (2 features per batch) instead of 10 batches (4 features) to respect LLM output limits
- **External CSS/JS** (`shared-styles.css` + `shared-scripts.js`) generated in Batch 1, referenced by all subsequent batches
- **Astro 5.x Strategic Goal** remains unchanged — tactical HTML files serve as raw material for Phase 3 migration
- **Automated Testing** integrated into every batch via `validate.sh` and browser preview

**Why This Approach Works:**
Based on July 2026 research, Claude Opus 4.6 and GPT-5.5 both support 128K output tokens [[18], [19]]. A 2-feature batch generates approximately 14,000 tokens — well within safe limits. The external CSS/JS approach reduces per-file output by 33% while maintaining complete browser preview capability.

---

## 🔍 Research Insights (July 2026 Verified)

### Industry Best Practices Identified:

**Astro 5.x Dominance**
Astro 5.x remains the leading framework for content-driven documentation sites in 2026 [[1], [3]]. Key advantages:

- Zero JavaScript by default (matches Emacs performance philosophy)
- Content Collections build 5x faster for Markdown pages
- Island Architecture allows interactive components without full hydration
- Native MDX support for component embedding

**LLM Output Token Limits (July 2026)**

- Claude Opus 4.6: 128K output tokens
- GPT-5.5: 128K output tokens
- Claude Sonnet 4.6: 64K output tokens

**Implication:** A 2-feature batch (~14K tokens) is safe. A 4-feature batch (~25K tokens) approaches limits and risks truncation.

**External CSS/JS Patterns**
Modern documentation systems use external asset management for:

- Reduced redundancy (single source of truth for styles)
- Easier maintenance (update once, apply everywhere)
- Better caching (browser caches CSS/JS separately from HTML)
- Cleaner separation of concerns (structure vs. presentation vs. behavior)

**Testing & Validation Strategies**
2026 best practices emphasize:

- Automated structural validation on every commit
- Visual regression testing for UI consistency
- Cross-browser verification across platforms
- Performance auditing (Lighthouse scores)

---

## 🏗️ Phase 1: Architecture Design

### 1.1 Dual-Layer Architecture

This plan implements a **dual-layer architecture**:

**Layer 1: Tactical (Immediate)**

- 20-batch HTML generation workflow
- External CSS/JS files for shared assets
- Standalone browser-previewable files
- Automated validation suite

**Layer 2: Strategic (Phase 3 Migration)**

- Astro 5.x with Content Collections
- MDX for interactive components
- Atomic Design component library
- Pagefind static search integration

### 1.2 Technology Stack

**Tactical Layer (Batches 1-20):**

```
┌─────────────────────────────────────────────────────┐
│  Generation: LLM (Claude Opus 4.6 / GPT-5.5)       │
│  Styling: shared-styles.css (Tokyo Night tokens)    │
│  Scripting: shared-scripts.js (vanilla JS)          │
│  Validation: Python + Bash automation suite         │
│  Preview: Local HTTP server (serve.sh)              │
└─────────────────────────────────────────────────────┘
```

**Strategic Layer (Phase 3):**

```
┌─────────────────────────────────────────────────────┐
│  SSG Framework: Astro 5.x (Content Collections)     │
│  Styling: Vanilla CSS (split from shared-styles)    │
│  Markdown: MDX for interactive components           │
│  Search: Pagefind (static search index)             │
│  Deployment: Cloudflare Pages / Vercel              │
│  Icons: Lucide (matches current SVG style)          │
└─────────────────────────────────────────────────────┘
```

### 1.3 Directory Structure

**Tactical Layer (During Batches 1-20):**

```
your-project/
├── scripts/
│   ├── save_batch.py       # Extracts HTML/CSS/JS from markdown
│   ├── validate.sh         # Structural validation
│   ├── build-index.py      # Generates index.html
│   └── serve.sh            # Local preview server
│
├── features/               # Generated files live here
│   ├── shared-styles.css   # Generated in Batch 1
│   ├── shared-scripts.js   # Generated in Batch 1
│   ├── intellisense.html   # Batch 1
│   ├── hover-info-native.html  # Batch 1
│   ├── hover-info-childframe.html  # Batch 2
│   ├── signature-help.html  # Batch 2
│   └── ... (36 more HTML files)
│
├── batch1.md               # LLM output for Batch 1
├── batch2.md               # LLM output for Batch 2
└── ... (20 batch files)
```

**Strategic Layer (Phase 3 Migration):**

```
emacs-ide-docs/
├── src/
│   ├── content/            # MDX source files (one per feature)
│   ├── components/         # Reusable UI atoms
│   ├── layouts/            # Page templates
│   ├── styles/
│   │   ├── tokens.css      # Extracted from shared-styles.css
│   │   ├── base.css        # Reset + typography
│   │   ├── components.css  # Extracted component styles
│   │   └── responsive.css  # Mobile breakpoints
│   ├── scripts/
│   │   └── shared.js       # Migrated from shared-scripts.js
│   └── pages/
└── astro.config.mjs
```

---

## 📦 Phase 2: 20-Batch Workflow Structure

### 2.1 Batch Allocation Strategy

**Rationale for 20 Batches:**

- Each batch generates 2 features (~14,000 tokens)
- Well within 128K LLM output limits [[18], [19]]
- Reduces truncation risk to near-zero
- Provides excellent error isolation (only 2 features at risk per batch)
- Total workflow time: ~100 minutes (5 minutes per batch × 20)

**Batch 1 Special Role:**
Batch 1 generates the external assets (`shared-styles.css` and `shared-scripts.js`) that all subsequent batches reference. This creates a dependency chain:

```
Batch 1 → shared-styles.css + shared-scripts.js + 2 HTML files
Batch 2-20 → Reference external assets + generate 2 HTML files each
```

### 2.2 Complete Batch Schedule

| Batch  | Features                                     | Scope                     | External Assets                                      |
| ------ | -------------------------------------------- | ------------------------- | ---------------------------------------------------- |
| **1**  | IntelliSense + Hover Info (Native)           | Completion & Intelligence | **Generate** shared-styles.css and shared-scripts.js |
| **2**  | Hover Info (Childframe) + Signature Help     | Completion & Intelligence | Reference existing assets                            |
| **3**  | Go to Definition + Go to Declaration         | Navigation & Code Jumping | Reference existing assets                            |
| **4**  | Go to Type Definition + Go to Implementation | Navigation & Code Jumping | Reference existing assets                            |
| **5**  | Find All References + Document Highlight     | Navigation & Code Jumping | Reference existing assets                            |
| **6**  | Document Symbols + Workspace Symbol Search   | Diagnostics & Symbols     | Reference existing assets                            |
| **7**  | Code Actions + Document Formatting           | Code Actions & Formatting | Reference existing assets                            |
| **8**  | Range Formatting + On-type Formatting        | Formatting & Editing      | Reference existing assets                            |
| **9**  | Rename Symbol + Folding Ranges               | Code Actions & Formatting | Reference existing assets                            |
| **10** | Selection Range + Linked Editing Range       | Formatting & Editing      | Reference existing assets                            |
| **11** | Call Hierarchy + Type Hierarchy              | Navigation & Code Jumping | Reference existing assets                            |
| **12** | Semantic Tokens + Inlay Hints                | Completion & Intelligence | Reference existing assets                            |
| **13** | Inline Values + Document Links               | Debugging & Visual        | Reference existing assets                            |
| **14** | Document Color + Diagnostics (Push)          | Visual & Diagnostics      | Reference existing assets                            |
| **15** | Diagnostics (Pull) + Moniker                 | Diagnostics & Navigation  | Reference existing assets                            |
| **16** | File-Operation Hooks + Execute Command       | Workspace & Code Actions  | Reference existing assets                            |
| **17** | Peek Definition + Breadcrumbs Bar            | Navigation & Visual       | Reference existing assets                            |
| **18** | Problems Panel + Sticky Scroll               | Diagnostics & Visual      | Reference existing assets                            |
| **19** | Bracket Colorization + Minimap               | Visual Enhancements       | Reference existing assets                            |
| **20** | Multi-Cursor Editing + Quick Fix Lightbulb   | Editing & Code Actions    | Reference existing assets                            |

### 2.3 Batch 1 Deliverables

Batch 1 is unique because it generates the shared assets:

```
features/
├── shared-styles.css          (~800 lines, ~20K tokens)
│   ├── CSS custom properties (Tokyo Night palette)
│   ├── Sidebar, topbar, accordion styles
│   ├── Table, card, code window styles
│   ├── Responsive breakpoints
│   └── Custom scrollbar styling
│
├── shared-scripts.js          (~100 lines, ~2.5K tokens)
│   ├── Sidebar toggle logic
│   ├── Mobile menu handling
│   ├── Accordion toggle with ARIA
│   ├── Focus mode toggle
│   └── Clipboard copy with fallback
│
├── intellisense.html          (~920 lines, ~23K tokens)
│   ├── <link rel="stylesheet" href="shared-styles.css">
│   ├── Complete sidebar with active state
│   ├── Complete topbar
│   └── 6 accordions with content
│
└── hover-info-native.html     (~920 lines, ~23K tokens)
    ├── <link rel="stylesheet" href="shared-styles.css">
    ├── Complete sidebar with active state
    ├── Complete topbar
    └── 6 accordions with content
```

**Total Batch 1 Output:** ~2,740 lines ≈ 68,500 tokens (well within 128K limit)

### 2.4 Batches 2-20 Deliverables

Each subsequent batch generates only HTML files that reference the external assets:

```
features/
├── [feature-1].html           (~920 lines)
│   ├── <link rel="stylesheet" href="shared-styles.css">
│   ├── <script src="shared-scripts.js"></script>
│   ├── Complete sidebar with correct active state
│   ├── Complete topbar
│   └── 6 accordions with content
│
└── [feature-2].html           (~920 lines)
    ├── <link rel="stylesheet" href="shared-styles.css">
    ├── <script src="shared-scripts.js"></script>
    ├── Complete sidebar with correct active state
    ├── Complete topbar
    └── 6 accordions with content
```

**Total per Batch (2-20):** ~1,840 lines ≈ 46,000 tokens (very safe)

---

## 🧪 Phase 3: Testing & Validation Protocol

### 3.1 Automated Validation Suite

Every batch must pass automated validation before proceeding:

**Step 1: Extract Files**

```bash
./scripts/save_batch.py batch[N].md
```

This script:

- Parses markdown output for `Filename:` markers
- Extracts HTML, CSS, and JS files
- Saves them to `features/` directory
- Reports extraction status

**Step 2: Structural Validation**

```bash
./scripts/validate.sh
```

This script checks:

- HTML contains `<title>` tag
- HTML contains `<link rel="stylesheet" href="shared-styles.css">`
- HTML contains `<script src="shared-scripts.js"></script>`
- HTML contains complete sidebar structure
- HTML contains complete topbar structure
- HTML contains exactly 6 accordions
- HTML references Prism.js (via shared-scripts.js)
- File size is reasonable (>50KB indicates complete file)

**Step 3: Visual Preview**

```bash
./scripts/serve.sh
```

Launch local server at `http://localhost:8000` and verify:

- Tokyo Night styling renders correctly
- Sidebar navigation works
- Accordions expand/collapse properly
- Code blocks have syntax highlighting
- Mobile responsive design functions
- Focus mode toggles correctly

### 3.2 Batch 1 Special Testing

Batch 1 requires additional validation:

**Asset Existence Check:**

```bash
ls -lh features/shared-styles.css features/shared-scripts.js
```

Both files must exist and be non-empty.

**Asset Content Validation:**

- `shared-styles.css` must contain Tokyo Night CSS variables
- `shared-styles.css` must contain sidebar, topbar, accordion styles
- `shared-scripts.js` must contain sidebar toggle logic
- `shared-scripts.js` must contain accordion toggle logic

**Browser Testing:**
Open `http://localhost:8000/features/intellisense.html` and verify:

- Page loads without 404 errors for CSS/JS
- Styling is applied correctly
- Interactive elements function

### 3.3 Regression Testing

After completing all 20 batches:

**Generate Master Index:**

```bash
./scripts/build-index.py
```

This creates `index.html` with links to all 40 feature pages.

**Full Site Validation:**

- Navigate to every feature page from index
- Verify all internal links work
- Check that sidebar active state updates correctly
- Confirm breadcrumbs display accurate hierarchy
- Test mobile responsive behavior on all pages

**Performance Audit:**

- Check file sizes (each HTML should be 50-120KB)
- Verify no duplicate CSS/JS in HTML files
- Confirm external assets are properly referenced

### 3.4 Error Recovery Protocol

**If Validation Fails:**

1. **Truncation Error** (file too small, missing closing tags)
   - Cause: LLM output limit exceeded
   - Solution: Regenerate the batch
   - Prevention: Already mitigated by 2-feature batches

2. **Missing Accordions** (fewer than 6 found)
   - Cause: LLM reasoning degradation
   - Solution: Regenerate the batch with explicit instruction
   - Add to prompt: "Ensure exactly 6 accordions are generated"

3. **Broken Styling** (page looks unstyled)
   - Cause: Missing external CSS/JS references
   - Solution: Check that `<link>` and `<script>` tags are present
   - Verify `shared-styles.css` and `shared-scripts.js` exist

4. **Incorrect Active State** (wrong sidebar item highlighted)
   - Cause: LLM generated wrong active class
   - Solution: Manually fix the `class="nav active"` attribute
   - Or regenerate with explicit instruction about active state

---

## 🎨 Phase 4: External Asset Management

### 4.1 shared-styles.css Structure

This file contains all styling extracted from `qwen-html.md`:

```
shared-styles.css
├── CSS Custom Properties (Tokyo Night palette)
│   ├── --bg, --bg-2, --surface, --surface-2
│   ├── --border, --border-strong
│   ├── --text, --text-dim, --text-muted
│   ├── --blue, --purple, --cyan, --green, --red, --yellow, --orange
│   ├── --code-bg
│   ├── --sidebar-w, --sidebar-collapsed, --topbar-h
│   └── --transition-speed
│
├── Reset & Base Styles
│   ├── Box-sizing, margin, padding reset
│   ├── Body typography (JetBrains Mono)
│   ├── Custom scrollbar styling
│   ├── Link styling
│   ├── Focus-visible outlines
│   ├── Inline code styling
│   └── kbd (keyboard shortcut) styling
│
├── Sidebar Styles
│   ├── Fixed positioning, backdrop-filter
│   ├── Collapsed state handling
│   ├── Navigation button styles
│   ├── Active state highlighting
│   ├── Tooltip positioning (collapsed state)
│   └── Footer toggle button
│
├── Topbar Styles
│   ├── Fixed positioning, backdrop-filter
│   ├── Hamburger menu (mobile)
│   ├── Breadcrumb styling
│   └── Icon button styles
│
├── Main Content Styles
│   ├── Margin/padding adjustments
│   ├── Focus mode transformations
│   └── Focus hint overlay
│
├── Page Header Styles
│   ├── Title row, status badge
│   ├── Category label
│   ├── VS Code parity box
│   └── Meta bar (LSP, routing)
│
├── Accordion Styles
│   ├── Accordion container
│   ├── Accordion head (button)
│   ├── Open state styling
│   ├── Chevron rotation animation
│   ├── Accordion body (grid-template-rows animation)
│   └── Accordion inner content
│
├── Section Title Styles
│   ├── Uppercase, letter-spacing
│   └:::after line decoration
│
├── Table Styles
│   ├── Table wrapper (overflow-x)
│   ├── Table structure
│   ├── Header styling
│   ├── Cell styling
│   ├── Hover states
│   └── Code within tables
│
├── Grid Layouts
│   ├── .grid-2 (2-column grid)
│   ├── Stack cards (implementation stack)
│   ├── Eco cards (ecosystem integration)
│   └── Enhancement cards
│
├── Code Window Styles
│   ├── macOS-style window chrome
│   ├── Dots (red, yellow, green)
│   ├── Filename display
│   ├── Copy button
│   ├── Prism.js syntax highlighting
│   └── Token colors (comments, keywords, functions, etc.)
│
├── Comparison Card Styles
│   ├── VS card container
│   ├── Chosen (green) variant
│   ├── Rejected (red) variant
│   └── Row layout (label + value)
│
├── Enhancement Card Styles
│   ├── Card container with top border
│   ├── Color variants (green, purple, yellow)
│   └── Title and description
│
└── Responsive Styles
    ├── Mobile overlay
    ├── @media (max-width: 900px) - single column grids
    └── @media (max-width: 768px) - mobile sidebar, topbar, main
```

### 4.2 shared-scripts.js Structure

This file contains all interactivity:

```
shared-scripts.js
├── Desktop Sidebar Toggle
│   ├── Get sidebar element
│   ├── Add click listener to toggle button
│   └── Toggle 'collapsed' class
│
├── Mobile Menu Handling
│   ├── Get mobile menu button and overlay
│   ├── Add click listener to open menu
│   ├── Add 'mobile-open' class to sidebar
│   ├── Add 'active' class to overlay
│   └── Add click listener to overlay to close menu
│
├── Accordion Logic
│   ├── Query all accordion heads
│   ├── Add click listener to each
│   ├── Toggle 'open' class on head and body
│   └── Update aria-expanded attribute
│
├── Focus Mode
│   ├── Get focus button
│   ├── Define toggleFocus function
│   │   └── Toggle 'focus' class on body
│   ├── Add click listener to focus button
│   └── Add keydown listener for ESC key
│
└── Clipboard Copy
    ├── Define copyCode function
    │   ├── Get code element from button
    │   ├── Create temporary textarea
    │   ├── Set textarea value to code text
    │   ├── Append to body, focus, select
    │   ├── Execute document.execCommand('copy')
    │   ├── Trigger success animation
    │   └── Remove textarea from body
    └── Define triggerCopySuccess function
        ├── Change button HTML to checkmark + "Copied"
        ├── Add 'copied' class
        └── Revert after 1800ms
```

### 4.3 HTML File Structure (Batches 2-20)

Each HTML file follows this template:

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — [Feature Name]</title>
    <link
      href="https://cdn.jsdelivr.net/npm/@fontsource/jetbrains-mono@5.0.18/index.min.css"
      rel="stylesheet"
    />
    <link
      href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css"
      rel="stylesheet"
    />
    <link rel="stylesheet" href="shared-styles.css" />
  </head>
  <body>
    <div class="overlay" id="overlay" aria-hidden="true"></div>
    <div class="focus-hint" id="focusHint" aria-live="polite">
      <kbd>ESC</kbd> <span>Exit Focus Mode</span>
    </div>

    <aside class="sidebar" id="sb" aria-label="Main Navigation">
      <!-- Complete sidebar HTML with correct active state -->
    </aside>

    <header class="topbar">
      <!-- Complete topbar HTML -->
    </header>

    <main class="main" id="main-content">
      <header class="page-head">
        <!-- Page header with title, status, category, parity, meta-bar -->
      </header>

      <!-- 6 Accordion articles -->
      <article class="acc">
        <!-- Accordion 1: Feature Overview -->
      </article>
      <article class="acc">
        <!-- Accordion 2: Ecosystem Integration -->
      </article>
      <article class="acc">
        <!-- Accordion 3: Implementation Stack -->
      </article>
      <article class="acc">
        <!-- Accordion 4: Commands & Keybindings -->
      </article>
      <article class="acc">
        <!-- Accordion 5: Configuration -->
      </article>
      <article class="acc">
        <!-- Accordion 6: Architecture & Enhancements -->
      </article>
    </main>

    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-lisp.min.js"></script>
    <script src="shared-scripts.js"></script>
  </body>
</html>
```

**Critical Elements:**

- `<link rel="stylesheet" href="shared-styles.css">` in `<head>`
- `<script src="shared-scripts.js"></script>` before closing `</body>`
- Complete sidebar HTML with `class="nav active"` on correct item
- Complete topbar HTML
- Exactly 6 `<article class="acc">` elements

---

## 🔄 Phase 5: Migration Strategy (Phase 3)

### 5.1 Strangler Pattern Migration

Once all 20 batches are complete and validated, we execute the **Strangler Pattern** migration to Astro 5.x:

**Step 1: Asset Extraction**
Split `shared-styles.css` into modular files:

```
src/styles/
├── tokens.css        # CSS custom properties only
├── base.css          # Reset, typography, scrollbar
├── components.css    # Accordion, cards, tables, code windows
└── responsive.css    # Mobile breakpoints
```

**Step 2: Script Migration**
Migrate `shared-scripts.js` to:

```
src/scripts/
└── shared.js         # Client-side interactivity
```

**Step 3: Component Extraction**
Parse the 40 HTML files and extract reusable components:

```
src/components/
├── layout/
│   ├── Sidebar.astro
│   ├── Topbar.astro
│   └── PageShell.astro
├── feature/
│   ├── Accordion.astro
│   ├── ParityMatrix.astro
│   ├── StackGrid.astro
│   ├── EcoGrid.astro
│   ├── VsCard.astro
│   ├── CodeWindow.astro
│   ├── CommandTable.astro
│   └── MetaBar.astro
└── ui/
    ├── Kbd.astro
    ├── StatusBadge.astro
    └── RouteArrow.astro
```

**Step 4: Content Migration**
Convert each HTML file to MDX:

```
src/content/
├── completion/
│   ├── intellisense.mdx
│   ├── hover-info-native.mdx
│   └── ...
├── navigation/
│   ├── go-to-definition.mdx
│   └── ...
└── ...
```

Each MDX file uses YAML frontmatter:

```yaml
---
title: "IntelliSense / Code Completion"
category: "Completion & Intelligence"
status: "working"
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

**Step 5: Dynamic Routing**
Astro automatically generates routes from Content Collections:

```
src/pages/
├── index.astro              # Landing page
└── [category]/[slug].astro  # Dynamic feature routes
```

### 5.2 Migration Benefits

**Why This Approach Works:**

- External CSS/JS files are already modularized
- HTML structure maps cleanly to Astro components
- Content is already organized by feature
- Validation suite ensures no regression during migration
- Can migrate incrementally (feature by feature)

**Performance Gains:**

- Astro's zero-JS default reduces page weight
- Content Collections build 5x faster
- Pagefind provides instant search without server costs
- View Transitions API enables SPA-like navigation

---

## 📊 Phase 6: Execution Roadmap

### Milestone 1: Foundation (Batch 1)

**Objectives:**

- Generate shared-styles.css and shared-scripts.js
- Generate first 2 feature pages (IntelliSense + Hover Info Native)
- Validate external asset architecture
- Confirm browser preview works

**Deliverables:**

- [ ] `features/shared-styles.css` (~800 lines)
- [ ] `features/shared-scripts.js` (~100 lines)
- [ ] `features/intellisense.html` (~920 lines)
- [ ] `features/hover-info-native.html` (~920 lines)
- [ ] All files pass `validate.sh`
- [ ] Browser preview at `http://localhost:8000` works perfectly

**Time Estimate:** 10 minutes (5 min generation + 5 min validation)

### Milestone 2: Core Features (Batches 2-10)

**Objectives:**

- Generate 18 feature pages (Batches 2-10)
- Cover Completion, Navigation, Diagnostics, Code Actions, Formatting
- Maintain consistent quality across batches

**Deliverables:**

- [ ] 18 HTML files in `features/`
- [ ] All files reference external CSS/JS correctly
- [ ] All files pass `validate.sh`
- [ ] Sidebar active states are correct for each page
- [ ] All 6 accordions present in each file

**Time Estimate:** 45 minutes (5 min per batch × 9 batches)

### Milestone 3: Complete Feature Set (Batches 11-20)

**Objectives:**

- Generate remaining 20 feature pages (Batches 11-20)
- Cover Debugging, Visual Enhancements, Editing
- Complete all 40 features

**Deliverables:**

- [ ] 20 HTML files in `features/`
- [ ] All files reference external CSS/JS correctly
- [ ] All files pass `validate.sh`
- [ ] Total of 40 feature pages + 2 external assets

**Time Estimate:** 50 minutes (5 min per batch × 10 batches)

### Milestone 4: Integration & Polish

**Objectives:**

- Generate master index
- Perform full-site validation
- Optimize performance
- Prepare for Phase 3 migration

**Deliverables:**

- [ ] `index.html` generated by `build-index.py`
- [ ] All 40 pages accessible from index
- [ ] Full-site validation passes
- [ ] Performance audit (file sizes, load times)
- [ ] Documentation for Phase 3 migration

**Time Estimate:** 15 minutes

### Milestone 5: Phase 3 Migration (Optional)

**Objectives:**

- Set up Astro 5.x project
- Extract components from tactical HTML
- Migrate content to MDX
- Deploy to production

**Deliverables:**

- [ ] Astro project scaffolded
- [ ] Component library built
- [ ] 40 MDX files created
- [ ] Site deployed to Cloudflare Pages / Vercel
- [ ] Pagefind search integrated

**Time Estimate:** 4-6 hours (depending on complexity)

---

## 🎯 Key Design Decisions

| Decision                        | Rationale                                 | 2026 Verification                           |
| ------------------------------- | ----------------------------------------- | ------------------------------------------- |
| **20 batches over 10**          | Reduces truncation risk, improves quality | Claude/GPT support 128K output [[18], [19]] |
| **External CSS/JS**             | Reduces redundancy, easier maintenance    | Standard modular pattern [[10], [14]]       |
| **Astro 5.x strategic goal**    | Best framework for content sites          | Industry leader in 2026 [[1], [3]]          |
| **Automated validation**        | Catches errors early, ensures consistency | 2026 testing best practices [[24], [26]]    |
| **2 features per batch**        | Optimal balance of speed and safety       | ~14K tokens per batch (safe)                |
| **Standalone HTML tactical**    | Browser preview without build step        | Immediate feedback loop                     |
| **Strangler Pattern migration** | Incremental, low-risk transition          | Proven migration strategy                   |

---

## ⚠️ Risks & Mitigations

| Risk                             | Probability           | Impact                        | Mitigation                                                               |
| -------------------------------- | --------------------- | ----------------------------- | ------------------------------------------------------------------------ |
| **LLM truncation**               | Low (with 20 batches) | High (lose entire batch)      | 2-feature batches stay well within limits; validation catches truncation |
| **Missing external assets**      | Low                   | High (all pages broken)       | Batch 1 generates assets first; validation checks for asset existence    |
| **Incorrect active state**       | Medium                | Medium (navigation confusion) | Explicit instruction in prompt; manual fix if needed                     |
| **Accordion count errors**       | Low                   | Medium (incomplete pages)     | Validation checks for exactly 6 accordions                               |
| **Styling inconsistencies**      | Low                   | Medium (visual bugs)          | Single source of truth (shared-styles.css); browser preview              |
| **Phase 3 migration complexity** | Medium                | High (delays launch)          | Tactical HTML already modularized; incremental migration                 |
| **Performance issues**           | Low                   | Medium (slow page loads)      | External assets enable caching; Astro optimization in Phase 3            |

---

## 🚀 Next Steps

To begin execution:

1. **Confirm Automation Suite**
   - Verify `save_batch.py`, `validate.sh`, `build-index.py`, `serve.sh` are in `scripts/` directory
   - Make scripts executable: `chmod +x scripts/*.sh scripts/*.py`

2. **Initialize Batch 1**
   - Paste the Master Batch Prompt with `BATCH 1` and `GREEN LIGHT`
   - Save LLM output to `batch1.md`
   - Run `./scripts/save_batch.py batch1.md`
   - Run `./scripts/validate.sh`
   - Run `./scripts/serve.sh`
   - Verify `http://localhost:8000/features/intellisense.html` in browser

3. **Proceed Through Batches 2-20**
   - Repeat the process for each batch
   - Maintain quality consistency
   - Track progress in `emacs_ide_docs_context.yaml`

4. **Generate Master Index**
   - After all 20 batches complete
   - Run `./scripts/build-index.py`
   - Verify all 40 pages accessible from `index.html`

5. **Plan Phase 3 Migration**
   - Review tactical HTML structure
   - Plan component extraction strategy
   - Set up Astro 5.x project

**Ready to begin? Paste the Master Batch Prompt with `BATCH 1` and `GREEN LIGHT` to initialize the pipeline.**
