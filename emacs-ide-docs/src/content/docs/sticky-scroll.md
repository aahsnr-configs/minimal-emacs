---
title: "Sticky Scroll"
category: "Navigation & Visual Enhancements"
status: "Working"
parity: "Sticky Scroll (keeps relevant scope headers in view while scrolling through large files)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Sticky Scroll</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Visual Enhancements</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Sticky Scroll (keeps relevant scope headers in view while scrolling
            through large files)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<span style="color: var(--text-dim); font-size: 12px">Indirectly leverages <code>textDocument/documentSymbol</code> via
              eglot → imenu enrichment</span>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>imenu / treesit<span class="route-arrow">→</span>topsy or sticky-scroll-mode<span class="route-arrow">→</span>header-line overlay</code>
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
<td>Pins function/class name at top of viewport</td>
<td>
<code>topsy-mode</code> displays the enclosing
                        definition in the header line.
                      </td>
</tr>
<tr>
<td>Multi-line sticky scroll for nested blocks</td>
<td>
<code>sticky-scroll-mode</code> (Option B) pins multiple
                        indentation levels simultaneously.
                      </td>
</tr>
<tr>
<td>Updates dynamically while scrolling</td>
<td>
                        Both packages hook into
                        <code>window-scroll-functions</code> to update the
                        header instantly.
                      </td>
</tr>
<tr>
<td>Clickable header to jump to definition</td>
<td>
<code>topsy</code> headers can be made clickable, or
                        users can rely on
                        <kbd>M-g M-i</kbd> (<code>imenu</code>).
                      </td>
</tr>
<tr>
<td>Works across all major modes</td>
<td>
                        Hooks into <code>prog-mode</code> and
                        <code>text-mode</code>, covering Python, Rust,
                        TypeScript, Org, etc.
                      </td>
</tr>
<tr>
<td>No UI jitter or text shifting</td>
<td>
                        Uses native header-line or overlay rendering, keeping
                        the buffer text perfectly stable.
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
                    Automatically populates
                    <code>imenu-create-index-function</code> with LSP document
                    symbols, which <code>topsy</code> consumes natively without
                    any manual bridging.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">AST Foundation</div>
</div>
</div>
<p class="eco-desc">
                    Provides the foundational AST that
                    <code>sticky-scroll-mode</code> can query for precise
                    structural boundaries, ensuring the sticky lines align
                    perfectly with syntactic scopes.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div>
<div class="eco-name">doom-modeline</div>
<div class="eco-sub">UI Harmony</div>
</div>
</div>
<p class="eco-desc">
                    By routing the sticky content to the
                    <code>header-line-format</code> (or a dedicated overlay),
                    the bottom mode line remains uncluttered, allowing
                    <code>doom-modeline</code> to focus on Git status and LSP
                    diagnostics.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="16" rx="2" width="20" x="2" y="4"></rect>
<path d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"></path>
</svg>
</div>
<div>
<div class="eco-name">general.el</div>
<div class="eco-sub">Keybindings</div>
</div>
</div>
<p class="eco-desc">
                    Eagerly registers the <kbd>SPC t s</kbd> leader binding,
                    providing a consistent, mnemonic toggle for sticky scroll
                    across all programming buffers.
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
<div class="stack-role">LSP Client</div>
<div class="stack-desc">
                      Built-in. Enriches the native <code>imenu</code> index
                      with deep, nested document symbols, providing the
                      structural data needed for accurate scoping.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">topsy</div>
<div class="stack-role">Definition Engine</div>
<div class="stack-desc">
                      GNU ELPA package. A lightweight sticky header that shows
                      which definition the top line of the window is within.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">sticky-scroll-mode</div>
<div class="stack-role">Indentation Engine</div>
<div class="stack-desc">
                      MELPA alternative. Uses an indentation-based approach to
                      find offscreen lines that are levels of indentation lower
                      than the current point.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2v20M2 12h20"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">header-line-format</div>
<div class="stack-role">Rendering Surface</div>
<div class="stack-desc">
                      Draws the sticky content at the very top of the window
                      without shifting the main buffer's text or causing
                      redisplay jitter.
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
<td>Toggle sticky scroll</td>
<td>
<code>topsy-mode</code> /
                        <code>sticky-scroll-mode</code>
</td>
<td><kbd>SPC t s</kbd></td>
<td>
                        Enables/disables the sticky header for the current
                        buffer.
                      </td>
</tr>
<tr>
<td>Toggle globally</td>
<td><code>global-topsy-mode</code></td>
<td>—</td>
<td>
                        Enables sticky headers across all programming buffers.
                      </td>
</tr>
<tr>
<td>Jump to enclosing symbol</td>
<td><code>imenu</code></td>
<td><kbd>M-g M-i</kbd></td>
<td>
                        Native fallback to jump directly to the symbol currently
                        pinned in the sticky header.
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
<span class="fname">init-sticky-scroll.el</span>
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
 ;; OPTION A: TOPSY (Definition-Based Sticky Header)
 ;; Recommended for its simplicity and perfect synergy with eglot's imenu.
 ;; ==========================================
 (use-package topsy
   :ensure t
   :hook ((prog-mode text-mode) . topsy-mode)
   :custom
   ;; Maximum number of lines the sticky header can occupy (for multi-line signatures).
   (topsy-max-header-lines 3)
   :config
   ;; Ensure topsy uses the enriched imenu data provided by eglot.
   (add-to-list 'topsy-mode-functions #'imenu--make-index-alist))
 ;; ==========================================
 ;; OPTION B: STICKY-SCROLL-MODE (Indentation-Based)
 ;; Uncomment to use VS Code-style multi-line indentation tracking instead of topsy.
 ;; ==========================================
 ;; (use-package sticky-scroll-mode
 ;;   :ensure t
 ;;   :hook ((prog-mode text-mode) . sticky-scroll-mode)
 ;;   :custom
 ;;   ;; Maximum number of sticky lines to display at the top of the viewport.
 ;;   (sticky-scroll-max-lines 3)
 ;;   ;; Use treesit indentation if available, falling back to standard indentation.
 ;;   (sticky-scroll-use-treesit t))
 ;; ==========================================
 ;; GENERAL.EL KEYBINDINGS (registered eagerly)
 ;; ==========================================
 (ar/global-leader
   "t" '(:ignore t :wk "toggle")
   "t s" '(topsy-mode :wk "Toggle sticky scroll"))</code></pre>
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
<h4>✓ topsy / sticky-scroll-mode · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Agnostic; works seamlessly with <code>eglot</code> and
                        native <code>imenu</code> / <code>treesit</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Near-zero overhead. <code>topsy</code> only evaluates
                        the header when the window scrolls.</span>
</div>
<div class="vs-row">
<span class="lab">Visual Polish</span>
<span class="val">Renders cleanly in the header line, preserving the mode
                        line for diagnostics.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui / semantic-stickyfunc · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Hard-bound to the <code>lsp-mode</code> ecosystem or
                        legacy CEDET.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Requires the forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val"><code>semantic-stickyfunc-mode</code> is notoriously
                        slow and prone to freezing on large files.</span>
</div>
<div class="vs-row">
<span class="lab">Visual Polish</span>
<span class="val">Often clashes with <code>doom-modeline</code> or custom
                        header-line configurations.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="enh-card g">
<div class="enh-title">Eglot Imenu Enrichment</div>
<p class="desc">
                    As of recent <code>eglot</code> updates, managed buffers
                    receive extra region info added to the
                    <code>imenu</code> index, allowing <code>topsy</code> to
                    show richer, deeply nested paths (e.g.,
                    <code>Class › Method</code>) rather than flat, ambiguous
                    lists.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Treesit Indentation Awareness</div>
<p class="desc">
                    If using <code>sticky-scroll-mode</code>, Emacs 31's native
                    <code>treesit</code> integration allows the package to query
                    the AST for precise structural indentation, avoiding the
                    false positives that plagued legacy regex-based trackers.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Pixel-Perfect Scrolling</div>
<p class="desc">
                    Emacs 31's refined <code>pixel-scroll-precision-mode</code>
                    interacts smoothly with sticky headers, ensuring that the
                    header remains firmly anchored at the top of the window even
                    during smooth, fractional-line scrolling.
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
<td>Sticky Header Shows Incorrect or Flat Names</td>
<td>
                        Ensure <code>eglot</code> is actively connected and has
                        populated the <code>imenu</code> index. You can verify
                        this by running <kbd>M-x imenu</kbd> and checking if the
                        menu shows deeply nested structures.
                      </td>
</tr>
<tr>
<td>Header Flickers During Fast Scrolling</td>
<td>
                        If you experience visual flicker, ensure
                        <code>pixel-scroll-precision-mode</code> is enabled, or
                        increase the debounce/throttle in
                        <code>sticky-scroll-mode</code> (if using Option B).
                      </td>
</tr>
<tr>
<td>Multi-Line Signatures Are Truncated</td>
<td>
                        Increase <code>topsy-max-header-lines</code> to
                        <code>3</code> or <code>4</code> to accommodate lengthy
                        function signatures without clipping the sticky header.
                      </td>
</tr>
</tbody>
</table>
</div>
</div>
</div>
</div>
</article>

