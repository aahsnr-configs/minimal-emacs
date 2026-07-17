---
title: "Go to Type Definition"
category: "Navigation & Code Jumping"
status: "Working"
parity: "Jump to where the type of a symbol is defined (class, interface, struct, type alias)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Go to Type Definition</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Code Jumping</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Jump to where the type of a symbol is defined (class, interface,
            struct, type alias)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/typeDefinition</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-find-typeDefinition<span class="route-arrow">→</span>xref
              framework</code>
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
<div class="sec-title">Semantic Distinction Matrix</div>
<div class="tbl-wrap">
<table class="tbl">
<thead>
<tr>
<th>Command</th>
<th>Jumps To</th>
<th>TypeScript Example</th>
<th>Rust Example</th>
</tr>
</thead>
<tbody>
<tr>
<td>Go to Definition (<kbd>g d</kbd>)</td>
<td>The value/implementation site</td>
<td>
<code>const user = new User()</code> → the
                        <code>new User()</code> expression
                      </td>
<td>
<code>let x: MyStruct</code> → the <code>let</code>
                        binding
                      </td>
</tr>
<tr>
<td>Go to Declaration (<kbd>g D</kbd>)</td>
<td>The header/forward declaration</td>
<td>Interface declaration in <code>.d.ts</code></td>
<td>
<code>mod</code> declaration in <code>lib.rs</code>
</td>
</tr>
<tr>
<td>Go to Type Definition (<kbd>g t</kbd>)</td>
<td>The type/class/struct definition</td>
<td><code>class User { ... }</code></td>
<td><code>struct MyStruct { ... }</code></td>
</tr>
<tr>
<td>Go to Implementation (<kbd>g i</kbd>)</td>
<td>Concrete implementations of interface</td>
<td>Classes implementing an interface</td>
<td><code>impl Trait for Type</code> blocks</td>
</tr>
</tbody>
</table>
</div>
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
<td>Right-click → Go to Type Definition</td>
<td>
<kbd>g t</kbd> (<code>eglot-find-typeDefinition</code>)
                        routes via <code>textDocument/typeDefinition</code>.
                      </td>
</tr>
<tr>
<td>Dropdown for ambiguous types</td>
<td>
<code>consult-xref</code> intercepts the xref display
                        functions for vertico-powered previews.
                      </td>
</tr>
<tr>
<td>Alt+Left returns to origin</td>
<td>
<kbd>M-,</kbd> (<code>xref-go-back</code>) via the
                        native xref history ring.
                      </td>
</tr>
<tr>
<td>Ctrl+Click on type reference</td>
<td>
                        Emacs 31 <code>global-xref-mouse-mode</code>
                        (<kbd>C-&lt;mouse-1&gt;</kbd>).
                      </td>
</tr>
<tr>
<td>Fallback to definition if type missing</td>
<td>
<code>eglot</code> natively falls back to definition if
                        the LSP server returns an empty type definition payload.
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
                    Automatically registers <code>eglot-xref-backend</code> in
                    <code>xref-backend-functions</code> for managed buffers,
                    routing <kbd>g t</kbd> to
                    <code>textDocument/typeDefinition</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div>
<div class="eco-name">consult</div>
<div class="eco-sub">Preview Engine</div>
</div>
</div>
<p class="eco-desc">
<code>consult-xref</code> intercepts the xref display
                    functions to provide vertico-powered previews for ambiguous
                    type targets (e.g., union types).
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">treesit</div>
<div class="eco-sub">Fallback Parser</div>
</div>
</div>
<p class="eco-desc">
                    For non-LSP buffers, <code>treesit</code> modes provide
                    fallback type-navigation via
                    <code>evil-textobj-tree-sitter</code> text objects combined
                    with <code>imenu</code>.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div>
<div class="eco-name">peek</div>
<div class="eco-sub">Inline Overlay</div>
</div>
</div>
<p class="eco-desc">
                    Provides the inline overlay engine for "Peek Type
                    Definition" workflows, keeping the user's spatial context
                    intact while inspecting type shapes.
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
                      Built-in. Queries <code>textDocument/typeDefinition</code>
                      and injects the location payload into the native
                      <code>xref</code> framework.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(187, 154, 247, 0.1); color: var(--purple);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M4 6h16M4 12h16M4 18h10"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref</div>
<div class="stack-role">Navigation Framework</div>
<div class="stack-desc">
                      Built-in. Manages location abstraction, shared history
                      ring, and cross-buffer jumping.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">consult-xref</div>
<div class="stack-role">Preview Engine</div>
<div class="stack-desc">
                      Intercepts <code>xref-show-definitions-function</code> to
                      render a vertico-powered dropdown with live buffer
                      previews.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="background: rgba(158, 206, 106, 0.1); color: var(--green);">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref-mouse-mode</div>
<div class="stack-role">Mouse Integration</div>
<div class="stack-desc">
                      Emacs 31 NEW. Enables native Ctrl+Click routing for xref
                      payloads, including type definitions.
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
<td>Jump to type definition</td>
<td><code>eglot-find-typeDefinition</code></td>
<td><kbd>C-c t</kbd> / <kbd>g t</kbd></td>
<td>
                        Jumps to the type (class/interface/struct) of the symbol
                        at point.
                      </td>
</tr>
<tr>
<td>Jump to definition (contrast)</td>
<td><code>xref-find-definitions</code></td>
<td><kbd>M-.</kbd> / <kbd>g d</kbd></td>
<td>Jumps to the value/implementation of the symbol.</td>
</tr>
<tr>
<td>Jump to declaration (contrast)</td>
<td><code>eglot-find-declaration</code></td>
<td><kbd>C-c d</kbd> / <kbd>g D</kbd></td>
<td>Jumps to the header/forward declaration.</td>
</tr>
<tr>
<td>Go back (history)</td>
<td><code>xref-go-back</code></td>
<td><kbd>M-,</kbd> / <kbd>g ,</kbd></td>
<td>
                        Returns to the exact cursor position before the jump.
                      </td>
</tr>
<tr>
<td>Peek type definition (inline)</td>
<td><code>peek-xref-definition</code></td>
<td><kbd>SPC c p d</kbd></td>
<td>
                        Requires <code>peek</code> package; renders the type
                        inline without switching tabs.
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
<span class="fname">init-type-definition.el</span>
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
;; 1. XREF &amp; CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  (global-xref-mouse-mode 1))

;; ==========================================
;; 2. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(general-define-key
  :states 'motion
  "g d" #'xref-find-definitions        ;; Go to Definition (value/impl)
  "g D" #'eglot-find-declaration       ;; Go to Declaration (header)
  "g t" #'eglot-find-typeDefinition    ;; Go to Type Definition (class/struct)
  "g i" #'eglot-find-implementation    ;; Go to Implementation (concrete)
  "g ," #'xref-go-back)               ;; Go Back (Alt+Left)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c g" '(:ignore t :wk "goto")
  "c g d" '(xref-find-definitions :wk "Definition")
  "c g D" '(eglot-find-declaration :wk "Declaration")
  "c g t" '(eglot-find-typeDefinition :wk "Type Definition")
  "c g i" '(eglot-find-implementation :wk "Implementation"))</code></pre>{% endraw %}
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
<h4>✓ eglot + xref · Chosen</h4>
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
<span class="lab">History</span>
<span class="val">Native <code>xref</code> history ring (shared across
                        all backends).</span>
</div>
<div class="vs-row">
<span class="lab">Fallback</span>
<span class="val"><code>eglot</code> gracefully falls back to
                        <code>textDocument/definition</code> if the server
                        returns empty.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode / lsp-ui · Rejected</h4>
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
<span class="lab">History</span>
<span class="val">Fragmented history management.</span>
</div>
<div class="vs-row">
<span class="lab">Fallback</span>
<span class="val">Custom fallback logic required.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Language-Specific Behavior</div>
<div class="tbl-wrap" style="margin-bottom: 24px">
<table class="tbl">
<thead>
<tr>
<th>Language</th>
<th>Server</th>
<th>Behavior</th>
</tr>
</thead>
<tbody>
<tr>
<td>TypeScript</td>
<td><code>typescript-language-server</code></td>
<td>
                        Excellent — jumps to <code>interface</code>,
                        <code>type</code>, <code>class</code> definitions.
                      </td>
</tr>
<tr>
<td>Rust</td>
<td><code>rust-analyzer</code></td>
<td>
                        Excellent — jumps to <code>struct</code>,
                        <code>enum</code>, <code>trait</code> definitions.
                      </td>
</tr>
<tr>
<td>Go</td>
<td><code>gopls</code></td>
<td>
                        Excellent — jumps to <code>type</code> declarations and
                        <code>struct</code> definitions.
                      </td>
</tr>
<tr>
<td>C++</td>
<td><code>clangd</code></td>
<td>
                        Excellent — jumps to class/struct definitions, distinct
                        from forward declarations.
                      </td>
</tr>
<tr>
<td>Python</td>
<td><code>pyright</code> / <code>pylsp</code></td>
<td>
                        Good — jumps to class definitions; less useful for
                        dynamically-typed code.
                      </td>
</tr>
<tr>
<td>Java</td>
<td><code>jdtls</code></td>
<td>
                        Excellent — jumps to class/interface definitions across
                        modules.
                      </td>
</tr>
</tbody>
</table>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">xref-mouse-mode (NEW)</div>
<p class="enh-desc">
                    Emacs 31 introduces native mouse-driven code navigation.
                    Enabling <code>global-xref-mouse-mode</code> binds
                    <kbd>C-&lt;down-mouse-1&gt;</kbd> to xref jumps, perfectly
                    mirroring VS Code's Ctrl+Click convention.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Editable Xref Buffers</div>
<p class="enh-desc">
                    Emacs 31's <code>xref-change-to-xref-edit-mode</code> (bound
                    to <kbd>e</kbd> in <code>*xref*</code> buffers) turns
                    type-definition lists into writable surfaces for bulk
                    mutation.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">consult-xref synergy</div>
<p class="enh-desc">
                    Ambiguous type definitions (e.g., TypeScript union types,
                    Rust trait objects) are presented in a highly performant,
                    searchable Vertico dropdown with instant buffer previews.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">Unified xref history</div>
<p class="enh-desc">
<code>xref-go-back</code> (<kbd>M-,</kbd>) treats
                    type-definition jumps identically to definition and
                    reference jumps — the entire navigation chain is preserved
                    in a single ring.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

