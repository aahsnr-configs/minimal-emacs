---
title: "Workspace Symbol Search"
category: "Diagnostics & Symbols"
status: "Working"
parity: "Ctrl+T \"Go to Symbol in Workspace\" — fuzzy search symbols across the whole project"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Workspace Symbol Search</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Diagnostics &amp; Symbols</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Ctrl+T "Go to Symbol in Workspace" — fuzzy search symbols across
            the whole project</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>workspace/symbol</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>consult-eglot-symbols<span class="route-arrow">→</span>vertico + orderless</code>
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
<td><kbd>Ctrl+T</kbd> opens workspace symbol search</td>
<td>
<kbd>C-u SPC s w</kbd> or <kbd>C-u M-g S</kbd> triggers
                        workspace-wide <code>consult-eglot-symbols</code>.
                      </td>
</tr>
<tr>
<td>Fuzzy filter symbols by name</td>
<td>
<code>orderless</code> matching styles allow
                        space-separated, out-of-order fuzzy matching.
                      </td>
</tr>
<tr>
<td>Live preview of symbol definition</td>
<td>
<code>consult</code> temporarily visits the file and
                        shows the definition context in a side window.
                      </td>
</tr>
<tr>
<td>Filter by symbol kind (class, function)</td>
<td>
<code>consult-eglot-symbols-kind</code> can be set to
                        filter specific LSP symbol kinds.
                      </td>
</tr>
<tr>
<td>Click/Enter to jump to target</td>
<td>
<kbd>RET</kbd> in <code>consult</code> dropdown jumps to
                        the exact location via <code>xref</code>.
                      </td>
</tr>
<tr>
<td>Icons for classes/functions in list</td>
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
                    Natively implements <code>workspace/symbol</code> and
                    formats the response into a structure that
                    <code>consult</code> can easily parse.
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
<code>consult-eglot-symbols</code> intercepts the payload,
                    sorts it, and provides the live preview via
                    <code>consult--buffer-preview</code>.
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
                    <code>initialize_configuration</code> across the entire
                    codebase.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">marginalia</div>
<div class="eco-sub">Annotations</div>
</div>
</div>
<p class="eco-desc">
                    Appends the file path and line number to each candidate,
                    providing crucial spatial context before jumping.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(224, 175, 104, 0.1);
                        color: var(--yellow);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
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
                      Built-in. Queries <code>workspace/symbol</code> and
                      returns a flat list of project-wide symbol candidates.
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
<div class="stack-name">consult-eglot-symbols</div>
<div class="stack-role">Preview Engine</div>
<div class="stack-desc">
                      Intercepts the LSP payload and renders it in the
                      minibuffer with live, asynchronous buffer previews.
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
<div class="stack-name">orderless</div>
<div class="stack-role">Filtering Engine</div>
<div class="stack-desc">
                      Provides space-separated, out-of-order fuzzy matching
                      (e.g., typing <code>usr cnt</code> matches
                      <code>UserContext</code>).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="14" rx="2" width="20" x="2" y="7"></rect>
<path d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">vertico</div>
<div class="stack-role">UI Renderer</div>
<div class="stack-desc">
                      Displays the filtered candidates in a clean, vertically
                      scrolling list with marginalia annotations.
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
<td>Workspace symbol search</td>
<td><code>consult-eglot-symbols</code></td>
<td><kbd>C-u SPC s w</kbd></td>
<td>
                        Prefix argument (<kbd>C-u</kbd>) forces workspace-wide
                        search instead of buffer-local.
                      </td>
</tr>
<tr>
<td>Buffer-local symbol search</td>
<td><code>consult-eglot-symbols</code></td>
<td><kbd>SPC c s</kbd></td>
<td>
                        Default behavior (no prefix) searches only the current
                        file via <code>textDocument/documentSymbol</code>.
                      </td>
</tr>
<tr>
<td>Built-in workspace search</td>
<td><code>eglot-workspace-symbols</code></td>
<td><kbd>M-x eglot-workspace-symbols</kbd></td>
<td>Fallback native command without live preview.</td>
</tr>
<tr>
<td>Apropos search (fallback)</td>
<td><code>xref-find-apropos</code></td>
<td><kbd>SPC c A</kbd></td>
<td>
                        Searches all registered xref backends (including eglot)
                        for a regex pattern.
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
<span class="fname">init-workspace-symbols.el</span>
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
;; CONSULT-EGLOT (Outline &amp; Workspace Symbols)
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
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "s" '(:ignore t :wk "search")
  "s w" '(consult-eglot-symbols :wk "Workspace symbols (use C-u)"))

;; Note: To explicitly trigger the workspace search without remembering
;; the prefix, you can bind a dedicated wrapper:
(defun ar/consult-eglot-workspace-symbols ()
  "Force workspace-wide symbol search via consult-eglot."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'consult-eglot-symbols)))

(ar/global-leader
  "s W" '(ar/consult-eglot-workspace-symbols :wk "Workspace symbols (force)"))</code></pre>{% endraw %}
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
<h4>✓ eglot + consult · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Works exclusively with <code>eglot</code> and native
                        <code>xref</code>.</span>
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
<span class="lab">Filtering</span>
<span class="val">Integrates seamlessly with <code>orderless</code> for
                        out-of-order fuzzy matching.</span>
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
<span class="lab">Filtering</span>
<span class="val">Requires custom matchers; struggles with
                        space-separated queries.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">consult-eglot Prefix Intelligence</div>
<p class="enh-desc">
                    The <code>consult-eglot-symbols</code> command natively
                    checks <code>(called-interactively-p 'any)</code> and the
                    <code>current-prefix-arg</code>. If a prefix is present, it
                    routes the query to <code>workspace/symbol</code>;
                    otherwise, it falls back to the faster, buffer-local
                    <code>textDocument/documentSymbol</code>. This eliminates
                    the need for two separate commands.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">treesit Fallback Parity</div>
<p class="enh-desc">
                    If the LSP server is disconnected or slow to respond to
                    <code>workspace/symbol</code>,
                    <code>consult-imenu-multi</code> can be used as a
                    zero-latency, AST-aware fallback that searches across all
                    open project buffers using Emacs 31's native
                    <code>treesit-aggregated-simple-imenu-settings</code>.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Optimized Preview Debouncing</div>
<p class="enh-desc">
                    The <code>consult-customize</code> block applies a
                    <code>:debounce 0.4</code> to
                    <code>consult-eglot-symbols</code>, preventing the LSP
                    server from being spammed with file-read requests while
                    rapidly scrolling through hundreds of workspace candidates.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Marginalia Annotations</div>
<p class="enh-desc">
                    Emacs 31's refined <code>marginalia</code> integration
                    ensures that workspace symbol candidates display their
                    originating file path and symbol kind (e.g.,
                    <code>[Class] src/utils.ts</code>) directly in the
                    minibuffer margin, providing crucial spatial context before
                    jumping.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

