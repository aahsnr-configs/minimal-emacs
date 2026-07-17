---
title: "Range Formatting (Format Selection)"
category: "Formatting & Editing"
status: "Working"
parity: "\"Format Selection\" command — format active region/selection only"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Range Formatting (Format Selection)</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Formatting &amp; Editing</div>
<div class="parity">
<b>VS Code Parity</b>
<span>"Format Selection" command — format active region/selection
            only</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/rangeFormatting</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-format (with region)
              OR lazy-ruff<span class="route-arrow">→</span>ruff format
              --range</code>
</div>
</div>
</header>

<article class="acc">
<button aria-controls="sect-overview" aria-expanded="true" class="acc-head open">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="7" width="7" x="3" y="3"></rect>
<rect height="7" width="7" x="14" y="3"></rect>
<rect height="7" width="7" x="14" y="14"></rect>
<rect height="7" width="7" x="3" y="14"></rect>
</svg>
            Feature Overview
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body open" id="sect-overview" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Behavioral Parity Matrix</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>VS Code Behavior</th>
<th>Emacs 31 Equivalent</th>
</tr>
</thead>
<tbody>
<tr>
<td><kbd>Shift+Alt+F</kbd> formats selection</td>
<td>
<kbd>SPC c f</kbd> with an active visual region triggers
                        <code>lazy-ruff-format-region</code> or
                        <code>eglot-format</code>.
                      </td>
</tr>
<tr>
<td>Respects <code>.ruff.toml</code> config</td>
<td>
<code>lazy-ruff</code> automatically inherits the
                        project's Ruff configuration from the workspace root.
                      </td>
</tr>
<tr>
<td>Formats embedded code (e.g., Jupyter/Org)</td>
<td>
<code>lazy-ruff-format-org-src</code> handles Python
                        blocks inside Org-mode seamlessly.
                      </td>
</tr>
<tr>
<td>No main-thread blocking</td>
<td>
                        CLI execution is asynchronous or near-instantaneous due
                        to Ruff's Rust-based speed.
                      </td>
</tr>
<tr>
<td>Fallback to whole-file if no region</td>
<td>
                        If no region is active, the command gracefully falls
                        back to <code>apheleia-format-buffer</code>.
                      </td>
</tr>
<tr>
<td>Cursor stability after format</td>
<td>
                        Region-based formatting inherently preserves the rest of
                        the buffer's undo history and cursor position.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-ecosystem" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M2 12h20M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"></path>
</svg>
            Ecosystem Integration
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-ecosystem" role="region">
<div>
<div class="acc-inner">
<div class="grid-2">
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div>
<div class="eco-name">eglot</div>
<div class="eco-sub">LSP Client</div>
</div>
</div>
<p class="eco-desc">
                    Natively supports
                    <code>textDocument/rangeFormatting</code> when an active
                    region is present, delegating to the language server for
                    non-Python languages.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
</div>
<div>
<div class="eco-name">lazy-ruff</div>
<div class="eco-sub">Python CLI Engine</div>
</div>
</div>
<p class="eco-desc">
                    Lightweight integration that invokes the Ruff CLI directly
                    (<code>ruff format --range</code>) for marked regions and
                    org src blocks, bypassing LSP entirely for zero-latency
                    Python formatting.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">org-mode</div>
<div class="eco-sub">Literate Programming</div>
</div>
</div>
<p class="eco-desc">
<code>lazy-ruff</code> explicitly supports
                    <code>org-src</code> blocks, making it the definitive choice
                    for literate programming workflows where Python snippets are
                    embedded in documentation.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div>
<div class="eco-name">apheleia</div>
<div class="eco-sub">Whole-file Fallback</div>
</div>
</div>
<p class="eco-desc">
                    For languages where a dedicated CLI range formatter isn't
                    available, <code>apheleia</code> remains the gold standard
                    for whole-file async formatting, while
                    <code>eglot</code> handles the LSP range formatting gap.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-stack" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"></path>
</svg>
            Implementation Stack
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-stack" role="region">
<div>
<div class="acc-inner">
<div class="grid-2">
<div class="stack-card">
<div class="stack-ic">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="3"></circle>
<path d="M12 1v6m0 6v6"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot</div>
<div class="stack-role">LSP Range Formatting</div>
<div class="stack-desc">
                      Built-in. Natively supports
                      <code>textDocument/rangeFormatting</code> when an active
                      region is present, delegating to the language server.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">lazy-ruff</div>
<div class="stack-role">Python CLI Range</div>
<div class="stack-desc">
                      Invokes the Ruff CLI directly (<code>ruff format --range</code>) for marked regions, bypassing LSP entirely for
                      zero-latency Python formatting.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">treesit / expreg</div>
<div class="stack-role">AST Region Precision</div>
<div class="stack-desc">
                      Combined with <code>expreg</code> or
                      <code>evil-textobj-tree-sitter</code>, you can select
                      precise AST nodes (e.g., a single function) and format
                      only that structural block.
                    </div>
</div>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-commands" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
            Commands &amp; Keybindings
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-commands" role="region">
<div>
<div class="acc-inner">
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Action</th>
<th>Command</th>
<th>Keybinding</th>
<th>Notes</th>
</tr>
</thead>
<tbody>
<tr>
<td>Format active region (LSP)</td>
<td><code>eglot-format</code></td>
<td><kbd>SPC c f</kbd> (with region)</td>
<td>
                        Eglot automatically detects the active region and
                        requests <code>textDocument/rangeFormatting</code>.
                      </td>
</tr>
<tr>
<td>Format region (Ruff CLI)</td>
<td><code>lazy-ruff-format-region</code></td>
<td><kbd>SPC c f</kbd> (with region)</td>
<td>
                        Invokes <code>ruff format --range</code> on the selected
                        text, ideal for Python without LSP overhead.
                      </td>
</tr>
<tr>
<td>Format org src block</td>
<td><code>lazy-ruff-format-org-src</code></td>
<td><kbd>C-c C-c</kbd> (in src block)</td>
<td>
                        Specifically targets Python code blocks within Org-mode
                        files.
                      </td>
</tr>
<tr>
<td>Fallback to whole-file</td>
<td><code>apheleia-format-buffer</code></td>
<td><kbd>SPC c f</kbd> (no region)</td>
<td>
                        If no region is active, the binding gracefully falls
                        back to whole-file async formatting.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-config" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="16 18 22 12 16 6"></polyline>
<polyline points="8 6 2 12 8 18"></polyline>
</svg>
            Configuration
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-config" role="region">
<div>
<div class="acc-inner">
<div class="code-win">
<div class="code-head">
<div style="display: flex; align-items: center">
<div aria-hidden="true" class="dots">
<span></span><span></span><span></span>
</div>
<span class="fname">init-range-formatting.el</span>
</div>
<button aria-label="Copy code snippet" class="copy" onclick="copyCode(this)">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="13" rx="2" width="13" x="9" y="9"></rect>
<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
</svg>
                    Copy
                  </button>
</div>
<pre><code class="language-lisp">;; ==========================================
;; 1. LAZY-RUFF (Python CLI Range Formatting)
;; ==========================================
(use-package lazy-ruff
  :ensure t
  :defer t
  :commands (lazy-ruff-format-region
             lazy-ruff-format-buffer
             lazy-ruff-format-org-src)
  :custom
  ;; Target only the active region when a region is selected.
  (lazy-ruff-only-format-region t)
  ;; Pass specific arguments to Ruff CLI if needed (e.g., config file).
  (lazy-ruff-args '("format" "--quiet"))
  :config
  ;; Bind to a convenient key for region formatting
  (general-define-key
   :states '(normal visual)
   "SPC c f" #'lazy-ruff-format-region))

;; ==========================================
;; 2. EGLOT NATIVE RANGE FORMATTING (Fallback/Other Languages)
;; ==========================================
;; Eglot natively handles region formatting. If a region is active,
;; `eglot-format` automatically sends `textDocument/rangeFormatting`.
;; No extra configuration is needed beyond the base `eglot` setup.</code></pre>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="sect-arch" aria-expanded="false" class="acc-head">
<span class="t">
<svg aria-hidden="true" class="ic" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2zM22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"></path>
</svg>
            Architecture &amp; Enhancements
          </span>
<svg aria-hidden="true" class="chev" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<polyline points="6 9 12 15 18 9"></polyline>
</svg>
</button>
<div class="acc-body" id="sect-arch" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Why This Approach?</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="vs-card ok">
<h4>✓ lazy-ruff / eglot · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP Coupling</span>
<span class="val"><code>lazy-ruff</code> requires zero LSP integration,
                        using pure CLI.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Ruff CLI is written in Rust and formats ranges in
                        milliseconds.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate (or
                        bypasses it cleanly via CLI).</span>
</div>
<div class="vs-row">
<span class="lab">Org-mode Synergy</span>
<span class="val">Natively supports formatting Python code inside Org src
                        blocks.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ Legacy python-format / Heavy LSP UI · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP Coupling</span>
<span class="val">Tightly coupled to <code>lsp-mode</code> or specific
                        language servers.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Synchronous LSP requests can block the main thread on
                        large files.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Often requires forbidden
                        <code>lsp-mode</code> ecosystem packages.</span>
</div>
<div class="vs-row">
<span class="lab">Org-mode Synergy</span>
<span class="val">Most LSP formatters struggle with embedded code
                        blocks.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">Native Region Awareness</div>
<p class="enh-desc">
                    Emacs 31's refined <code>eglot</code> implementation
                    seamlessly detects <code>(region-active-p)</code> and routes
                    to <code>textDocument/rangeFormatting</code> without
                    requiring separate commands.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Tree-sitter Region Precision</div>
<p class="enh-desc">
                    When combined with <code>expreg</code> or
                    <code>evil-textobj-tree-sitter</code>, you can select
                    precise AST nodes (e.g., a single function or class) and
                    format only that structural block, avoiding the fragility of
                    text-based line selection.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
                    Ruff CLI <code>--range</code> Support
                  </div>
<p class="enh-desc">
                    Modern Ruff versions natively support the
                    <code>--range</code> flag (e.g.,
                    <code>--range=10:1-20:1</code>), allowing the CLI to format
                    specific line ranges without touching the rest of the file.
                    <code>lazy-ruff</code> translates Emacs region bounds into
                    this exact CLI syntax.
                  </p>
</div>
</div>
<div class="sec-title">Troubleshooting</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Issue</th>
<th>Cause &amp; Solution</th>
</tr>
</thead>
<tbody>
<tr>
<td>Region Formatting Falls Back to Whole File</td>
<td>
                        Ensure <code>lazy-ruff-only-format-region</code> is set
                        to <code>t</code>, or verify that the region is actively
                        highlighted (e.g., via <code>evil-visual-state</code>)
                        before invoking the command.
                      </td>
</tr>
<tr>
<td>Ruff CLI Not Found</td>
<td>
<code>lazy-ruff</code> requires the
                        <code>ruff</code> binary to be in your system's
                        <code>exec-path</code>. Verify this with
                        <kbd>M-x exec-path</kbd> or install it via your OS
                        package manager.
                      </td>
</tr>
<tr>
<td>Org Src Block Formatting Fails</td>
<td>
                        Ensure the source block is explicitly declared as
                        <code>python</code> (e.g.,
                        <code>#+begin_src python</code>).
                        <code>lazy-ruff</code> uses the block's language
                        identifier to route the formatting command correctly.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

