---
title: "Type Hierarchy"
category: "Navigation & Code Jumping"
status: "Working"
parity: "\"Show Type Hierarchy\" (supertypes/subtypes tree view)"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Type Hierarchy</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Code Jumping</div>
<div class="parity">
<b>VS Code Parity</b>
<span>"Show Type Hierarchy" (supertypes/subtypes tree view)</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/prepareTypeHierarchy</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>typeHierarchy/supertypes</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>typeHierarchy/subtypes</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-show-type-hierarchy<span class="route-arrow">→</span>interactive *eglot-hierarchy* buffer</code>
</div>
</div>
</header>

<article class="acc">
<button aria-controls="type-sect-overview" aria-expanded="true" class="acc-head open">
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
<div class="acc-body open" id="type-sect-overview" role="region">
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
<td>"Show Type Hierarchy" opens tree view</td>
<td>
<kbd>SPC c t</kbd> or <kbd>g T</kbd> invokes
                        <code>eglot-show-type-hierarchy</code>.
                      </td>
</tr>
<tr>
<td>Tree view of supertypes (parents)</td>
<td>
                        Native Eglot hierarchy buffer displays "Supertypes"
                        expandable nodes.
                      </td>
</tr>
<tr>
<td>Tree view of subtypes (children)</td>
<td>
                        Native Eglot hierarchy buffer displays "Subtypes"
                        expandable nodes.
                      </td>
</tr>
<tr>
<td>Click node to jump to source</td>
<td>
<kbd>RET</kbd> on a hierarchy node triggers
                        <code>xref-find-definitions</code> to jump to the
                        location.
                      </td>
</tr>
<tr>
<td>Expand/collapse tree branches</td>
<td>
<kbd>TAB</kbd> or <kbd>RET</kbd> on parent nodes toggles
                        child visibility.
                      </td>
</tr>
<tr>
<td>Center view on current symbol</td>
<td>
<code>eglot-hierarchy-center-on-node</code> recenters
                        the tree on the active symbol.
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
<button aria-controls="type-sect-ecosystem" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="type-sect-ecosystem" role="region">
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
                    Natively handles the
                    <code>textDocument/prepareTypeHierarchy</code> and
                    subsequent <code>supertypes</code>/<code>subtypes</code>
                    requests, formatting the LSP response into an Emacs-friendly
                    hierarchy structure.
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
<div class="eco-name">xref</div>
<div class="eco-sub">Navigation Framework</div>
</div>
</div>
<p class="eco-desc">
                    The hierarchy buffer uses <code>xref</code> under the hood
                    for node navigation, ensuring that jumping to a type
                    integrates perfectly with the global
                    <code>xref-go-back</code> history.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
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
                    Eagerly registers the <kbd>SPC c t</kbd> and
                    <kbd>g T</kbd> leader bindings, ensuring the command is
                    instantly available in all <code>eglot</code>-managed
                    buffers without deferred-registration traps.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="11" cy="11" r="8"></circle>
<line x1="21" x2="16.65" y1="21" y2="16.65"></line>
</svg>
</div>
<div>
<div class="eco-name">consult</div>
<div class="eco-sub">Flat Search Fallback</div>
</div>
</div>
<p class="eco-desc">
                    While the hierarchy is displayed in a dedicated buffer,
                    users can still fall back to
                    <code>consult-eglot-symbols</code> for flat, fuzzy-filtered
                    workspace-wide symbol searches if the tree view becomes too
                    deep.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="type-sect-stack" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="type-sect-stack" role="region">
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
                      Built-in. Drives the <code>typeHierarchy/*</code> protocol
                      methods and formats the response.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2zM22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eglot-show-type-hierarchy</div>
<div class="stack-role">Hierarchy Renderer</div>
<div class="stack-desc">
                      Native Eglot command that pops up a special buffer showing
                      an interactive tree of supertypes and subtypes.
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
<div class="stack-name">tabulated-list / hierarchy API</div>
<div class="stack-role">Navigation UI</div>
<div class="stack-desc">
                      Provides expandable/collapsible nodes and direct jumping
                      to source locations using native Emacs APIs.
                    </div>
</div>
</div>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="type-sect-commands" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="type-sect-commands" role="region">
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
<td>Show type hierarchy</td>
<td><code>eglot-show-type-hierarchy</code></td>
<td><kbd>SPC c t</kbd> / <kbd>g T</kbd></td>
<td>
                        Opens the interactive supertype/subtype tree for the
                        symbol at point.
                      </td>
</tr>
<tr>
<td>Show call hierarchy</td>
<td><code>eglot-show-call-hierarchy</code></td>
<td><kbd>SPC c h</kbd> / <kbd>g c</kbd></td>
<td>
                        Opens the interactive caller/callee tree for the symbol
                        at point.
                      </td>
</tr>
<tr>
<td>Center on node</td>
<td><code>eglot-hierarchy-center-on-node</code></td>
<td><kbd>c</kbd> (in hierarchy buffer)</td>
<td>Recenters the tree view on the current node.</td>
</tr>
<tr>
<td>Expand/Collapse node</td>
<td><code>tabulated-list</code> native</td>
<td><kbd>TAB</kbd> / <kbd>RET</kbd></td>
<td>
                        Toggles the visibility of child nodes in the tree.
                      </td>
</tr>
<tr>
<td>Jump to definition</td>
<td><code>xref-find-definitions</code></td>
<td><kbd>RET</kbd> (on node)</td>
<td>
                        Jumps to the selected type definition in the source
                        buffer.
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
<button aria-controls="type-sect-config" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="type-sect-config" role="region">
<div>
<div class="acc-inner">
<div class="code-win">
<div class="code-head">
<div style="display: flex; align-items: center">
<div aria-hidden="true" class="dots">
<span></span><span></span><span></span>
</div>
<span class="fname">init-type-hierarchy.el</span>
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
;; EGLOT HIERARCHY (Built-in)
;; ==========================================
;; eglot natively provides `eglot-show-type-hierarchy` and
;; `eglot-show-call-hierarchy` for interactive tree exploration.
;; No explicit configuration is needed beyond the base `eglot` setup.

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c h" '(eglot-show-call-hierarchy :wk "Call hierarchy")
  "c t" '(eglot-show-type-hierarchy :wk "Type hierarchy"))

(general-define-key
  :states 'motion
  "g c" #'eglot-show-call-hierarchy
  "g T" #'eglot-show-type-hierarchy)</code></pre>
</div>
</div>
</div>
</div>
</article>

<article class="acc">
<button aria-controls="type-sect-arch" aria-expanded="false" class="acc-head">
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
<div class="acc-body" id="type-sect-arch" role="region">
<div>
<div class="acc-inner">
<div class="sec-title">Why This Approach?</div>
<div class="grid-2" style="margin-bottom: 24px">
<div class="vs-card ok">
<h4>✓ eglot native · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Works exclusively with built-in
                        <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val"><code>eglot-show-type-hierarchy</code> is a native
                        command added to Eglot, leveraging the latest LSP 3.17+
                        type hierarchy spec.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Renders directly via Eglot's optimized hierarchy API
                        without extra abstraction layers.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode / eglot-hierarchy · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Third-party <code>eglot-hierarchy</code> is obsolete;
                        <code>lsp-mode</code> is forbidden.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol</span>
<span class="val">Requires forbidden <code>lsp-mode</code> ecosystem or
                        deprecated external packages.</span>
</div>
<div class="vs-row">
<span class="lab">Emacs 31 Synergy</span>
<span class="val">External packages duplicate functionality now present
                        in Emacs core.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Legacy packages introduce unnecessary indirection and
                        maintenance burden.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">Native Hierarchy Commands</div>
<p class="enh-desc">
<code>eglot-show-type-hierarchy</code> is fully integrated
                    into Eglot, popping up a special buffer showing an
                    interactive tree which represents a hierarchy of super- and
                    sub-types, leveraging the LSP 3.17+
                    <code>typeHierarchy/*</code> methods.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">Interactive Tree Navigation</div>
<p class="enh-desc">
                    The hierarchy buffer leverages Emacs' native
                    <code>tabulated-list</code> and hierarchy APIs, providing
                    smooth, keyboard-driven expansion and contraction of type
                    trees without relying on fragile third-party UI overlays.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Seamless xref Integration</div>
<p class="enh-desc">
                    Selecting a node in the type hierarchy seamlessly delegates
                    to <code>xref-find-definitions</code>, preserving the shared
                    <code>xref</code> history ring so you can easily return to
                    your original location with <code>xref-go-back</code>
                    (<kbd>M-,</kbd>).
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

