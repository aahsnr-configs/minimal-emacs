---
title: "Document Symbols / Outline View"
category: "Diagnostics & Symbols"
status: "Working"
parity: "Outline sidebar, Ctrl+Shift+O (Go to Symbol in File), and top breadcrumb bar"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Document Symbols / Outline View</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Diagnostics &amp; Symbols</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Outline sidebar, Ctrl+Shift+O (Go to Symbol in File), and top
            breadcrumb bar</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/documentSymbol</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>imenu<span class="route-arrow">→</span>consult-eglot-symbols OR breadcrumb</code>
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
<td><kbd>Ctrl+Shift+O</kbd> opens file outline</td>
<td>
<kbd>SPC c s</kbd> (<code>consult-eglot-symbols</code>)
                        opens minibuffer outline with live preview.
                      </td>
</tr>
<tr>
<td>Fuzzy filter symbols by name</td>
<td>
<code>consult</code> + <code>orderless</code> allows
                        space-separated, out-of-order fuzzy matching.
                      </td>
</tr>
<tr>
<td>Click symbol to jump to definition</td>
<td>
<kbd>RET</kbd> in <code>consult</code> dropdown or
                        <kbd>mouse-1</kbd> on breadcrumb segments.
                      </td>
</tr>
<tr>
<td>Top bar shows <code>file › class › method</code></td>
<td>
<code>breadcrumb-mode</code> renders this exact
                        hierarchy in the header line.
                      </td>
</tr>
<tr>
<td>Workspace-wide symbol search (<kbd>Ctrl+T</kbd>)</td>
<td>
<kbd>C-u SPC c s</kbd> or <kbd>SPC s w</kbd> triggers
                        <code>workspace/symbol</code> via <code>consult</code>.
                      </td>
</tr>
<tr>
<td>Icons for classes/functions in outline</td>
<td>
<code>nerd-icons-completion</code> automatically injects
                        glyphs into the <code>consult</code> dropdown.
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
                    Natively maps <code>textDocument/documentSymbol</code>
                    responses to the buffer-local
                    <code>imenu-create-index-function</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div>
<div class="eco-name">consult</div>
<div class="eco-sub">Preview Engine</div>
</div>
</div>
<p class="eco-desc">
<code>consult-eglot-symbols</code> intercepts the
                    <code>imenu</code> index, transforming it into a searchable,
                    preview-enabled Vertico menu.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div>
<div class="eco-name">orderless</div>
<div class="eco-sub">Filtering Engine</div>
</div>
</div>
<p class="eco-desc">
                    Provides the fuzzy matching engine, allowing queries like
                    <code>init conf</code> to instantly find
                    <code>initialize_configuration</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">breadcrumb</div>
<div class="eco-sub">Spatial Orientation</div>
</div>
</div>
<p class="eco-desc">
                    Provides the persistent, clickable breadcrumb trail at the
                    top of the window, updating dynamically as the cursor moves
                    through different scopes.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(224, 175, 104, 0.1);
                        color: var(--yellow);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">nerd-icons-completion</div>
<div class="eco-sub">Visual Glyphs</div>
</div>
</div>
<p class="eco-desc">
                    Injects visual glyphs (e.g., 🏛️ for classes, ⚙️ for
                    functions) into the <code>consult</code> dropdown, matching
                    the VS Code outline sidebar aesthetic.
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
                      Built-in. Queries
                      <code>textDocument/documentSymbol</code> and maps the
                      hierarchical response to Emacs' native
                      <code>imenu</code> index.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">consult-eglot</div>
<div class="stack-role">Outline Engine</div>
<div class="stack-desc">
                      Renders a live-preview, fuzzy-filtered outline tree in the
                      minibuffer using <code>vertico</code> and
                      <code>orderless</code>.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">breadcrumb</div>
<div class="stack-role">Breadcrumb Engine</div>
<div class="stack-desc">
                      GNU ELPA package that displays a clickable, hierarchical
                      path (e.g., <code>file › class › method</code>) in the
                      header line.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">treesit</div>
<div class="stack-role">Fallback Parser</div>
<div class="stack-desc">
                      Built-in. Provides native AST-based <code>imenu</code>
                      generation if the LSP server is slow or disconnected.
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
<td>Go to symbol in file</td>
<td><code>consult-eglot-symbols</code></td>
<td><kbd>SPC c s</kbd></td>
<td>
                        Opens a vertico-powered, preview-enabled outline
                        dropdown.
                      </td>
</tr>
<tr>
<td>Go to symbol in workspace</td>
<td>
<code>consult-eglot-symbols</code> (with <kbd>C-u</kbd>)
                      </td>
<td><kbd>C-u SPC c s</kbd></td>
<td>
                        Searches across the entire project via
                        <code>workspace/symbol</code>.
                      </td>
</tr>
<tr>
<td>Toggle breadcrumbs</td>
<td><code>breadcrumb-mode</code></td>
<td><kbd>SPC t b</kbd></td>
<td>
                        Enables the clickable path bar at the top of the buffer.
                      </td>
</tr>
<tr>
<td>Native imenu jump</td>
<td><code>imenu</code></td>
<td><kbd>M-g M-i</kbd></td>
<td>
                        Fallback to native Emacs imenu if LSP is unavailable.
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
<span class="fname">init-symbols.el</span>
</div>
<button aria-label="Copy code snippet" class="copy" onclick="copyCode(this)">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="13" rx="2" width="13" x="9" y="9"></rect>
<path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path>
</svg>
                    Copy
                  </button>
</div>
{% raw %}<pre><code class="language-lisp">;; ==========================================
;; 1. BREADCRUMB (Header-line Breadcrumbs)
;; ==========================================
(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode)
  :custom
  ;; Optional: Customize the separator for breadcrumbs
  (breadcrumb-imenu-crumb-separator " › ")
  (breadcrumb-project-crumb-separator " / "))

;; ==========================================
;; 2. CONSULT-EGLOT (Outline &amp; Workspace Symbols)
;; ==========================================
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (("M-g s" . consult-eglot-symbols)      ; Go to symbol in file
         ("M-g S" . consult-eglot-symbols))     ; With C-u, goes to workspace symbol
  :config
  ;; Ensure consult-eglot uses the current project root for workspace symbols
  (setq consult-eglot-symbols-kind nil))       ; nil = all kinds, or filter like '(class function)

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c s" '(consult-eglot-symbols :wk "Document symbols (outline)")
  "t" '(:ignore t :wk "toggle")
  "t b" '(breadcrumb-mode :wk "Toggle breadcrumbs"))</code></pre>{% endraw %}
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
<h4>✓ eglot + consult + breadcrumb · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Works exclusively with <code>eglot</code> and native
                        <code>imenu</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Preview</span>
<span class="val"><code>consult</code> provides live, asynchronous buffer
                        previews while scrolling the symbol tree.</span>
</div>
<div class="vs-row">
<span class="lab">Breadcrumbs</span>
<span class="val">Native <code>header-line-format</code> via
                        <code>breadcrumb</code>, zero third-party
                        dependencies.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui / lsp-mode · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Requires forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Preview</span>
<span class="val"><code>lsp-ui</code> uses heavy, custom child-frame
                        rendering that can stutter.</span>
</div>
<div class="vs-row">
<span class="lab">Breadcrumbs</span>
<span class="val">Requires <code>lsp-mode</code>'s custom breadcrumb
                        implementation.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
                    treesit-aggregated-simple-imenu-settings
                  </div>
<p class="enh-desc">
                    Emacs 31 introduces native support for multi-language imenu
                    trees. For mixed-language buffers (e.g.,
                    <code>mhtml-ts-mode</code>, <code>php-ts-mode</code>), the
                    outline view seamlessly aggregates symbols from HTML, CSS,
                    and PHP tree-sitter parsers without relying solely on the
                    LSP server.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Enhanced breadcrumb integration</div>
<p class="enh-desc">
                    The breadcrumb bar in Emacs 31 is more robust, correctly
                    handling deep nesting and long symbol names by truncating
                    gracefully or allowing horizontal scrolling within the
                    header line.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Native Fallback Parity</div>
<p class="enh-desc">
                    If the LSP server crashes or is slow to respond, Emacs 31's
                    <code>treesit</code> automatically populates the
                    <code>imenu</code> index, ensuring
                    <code>consult-eglot-symbols</code> (which falls back to
                    <code>imenu</code>) still provides a highly accurate,
                    AST-aware outline view with zero network latency.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Eglot Imenu Enrichment</div>
<p class="enh-desc">
                    As of <code>eglot</code> 1.14+, managed buffers receive
                    extra region info added to the <code>imenu</code> index,
                    allowing <code>breadcrumb</code> to show "richer", deeply
                    nested paths (e.g., <code>Namespace › Class › Method</code>)
                    rather than flat lists.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

