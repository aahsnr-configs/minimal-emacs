---
title: "Moniker"
category: "Navigation & Code Jumping"
status: "Working"
parity: "Background protocol feature enabling external indexers (e.g., Sourcegraph, LSIF) to link symbols across repositories"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Moniker</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Navigation &amp; Code Jumping</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Background protocol feature enabling external indexers (e.g.,
            Sourcegraph, LSIF) to link symbols across repositories</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/moniker</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>custom inspection command
              (no native UI)</code>
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
<td>Server advertises <code>monikerProvider</code></td>
<td>
<code>eglot</code> natively detects and stores this
                        capability during initialization.
                      </td>
</tr>
<tr>
<td>External tools (Sourcegraph) use monikers</td>
<td>
                        Emacs acts as a passive participant; the LSP server
                        provides the data to external indexers.
                      </td>
</tr>
<tr>
<td>Inspect symbol identity</td>
<td>
<kbd>SPC c m</kbd>
                        (<code>ar/eglot-moniker-at-point</code>) displays the
                        scheme, identifier, and kind in the echo area.
                      </td>
</tr>
<tr>
<td>Copy identity for external search</td>
<td>
<kbd>SPC c M</kbd> (<code>ar/eglot-copy-moniker</code>)
                        copies the identifier (e.g.,
                        <code>npm:lodash:4.17.21:map</code>) to the kill-ring.
                      </td>
</tr>
<tr>
<td>No local "jump" action</td>
<td>
                        Correctly omitted, as monikers represent
                        cross-repository identities, not local file paths.
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
                    Manages the lifecycle of the LSP connection and capability
                    negotiation, ensuring <code>textDocument/moniker</code> is
                    only called if the server explicitly supports it.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
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
                    Eagerly registers the <kbd>SPC c m</kbd> and
                    <kbd>SPC c M</kbd> leader bindings, ensuring the inspection
                    commands are instantly available in all
                    <code>eglot</code>-managed buffers without
                    deferred-registration traps.
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
<div class="eco-name">External Workflows</div>
<div class="eco-sub">Cross-Repo Navigation</div>
</div>
</div>
<p class="eco-desc">
                    The copied moniker string can be seamlessly pasted into
                    external tools like Sourcegraph
                    (<code>sourcegraph.com/search?q=context:global+&lt;moniker&gt;</code>)
                    for true cross-repository navigation.
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
                      Built-in. Natively negotiates the
                      <code>monikerProvider</code> capability during
                      initialization if the language server advertises it (LSP
                      3.16+).
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
<div class="stack-name">ar/eglot-moniker-at-point</div>
<div class="stack-role">Inspection Tool</div>
<div class="stack-desc">
                      A lightweight Elisp utility to query and display the
                      resolved moniker strings (e.g.,
                      <code>npm:lodash:4.17.21:map</code>) for debugging or
                      advanced workflows.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">LSIF / Sourcegraph</div>
<div class="stack-role">External Consumers</div>
<div class="stack-desc">
                      Monikers are primarily designed to be consumed by external
                      indexing tools, not for direct local editor navigation.
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
<td>Inspect moniker at point</td>
<td><code>ar/eglot-moniker-at-point</code></td>
<td><kbd>SPC c m</kbd></td>
<td>
                        Queries the LSP server for the symbol's cross-repo
                        identity and displays it in the echo area.
                      </td>
</tr>
<tr>
<td>Copy moniker to kill-ring</td>
<td><code>ar/eglot-copy-moniker</code></td>
<td><kbd>SPC c M</kbd></td>
<td>
                        Copies the primary moniker string for use in external
                        search tools (e.g., Sourcegraph).
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
<span class="fname">init-moniker.el</span>
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
;; EGLOT MONIKER INSPECTION (LSP 3.16+)
;; ==========================================
;; Eglot natively negotiates the `monikerProvider` capability.
;; This custom utility allows users to inspect the resolved moniker strings
;; for debugging or integration with external indexers (e.g., Sourcegraph).

(defun ar/eglot-moniker-at-point ()
  "Request and display the LSP moniker for the symbol at point."
  (interactive)
  (let ((server (eglot-current-server)))
    (if (not server)
        (user-error "No active Eglot server")
      (eglot--execute-request
       server
       "textDocument/moniker"
       (eglot--TextDocumentPositionParams)
       (lambda (result)
         (if (and result (cl-plusp (length result)))
             (let* ((primary (car result))
                    (scheme (plist-get primary :scheme))
                    (identifier (plist-get primary :identifier))
                    (kind (plist-get primary :kind)))
               (message "Moniker [%s]: %s (Kind: %s)" scheme identifier kind))
           (message "No moniker found for symbol at point.")))))))

(defun ar/eglot-copy-moniker ()
  "Copy the primary moniker identifier of the symbol at point to the kill-ring."
  (interactive)
  (let ((server (eglot-current-server)))
    (if (not server)
        (user-error "No active Eglot server")
      (eglot--execute-request
       server
       "textDocument/moniker"
       (eglot--TextDocumentPositionParams)
       (lambda (result)
         (if (and result (cl-plusp (length result)))
             (let ((identifier (plist-get (car result) :identifier)))
               (kill-new identifier)
               (message "Copied moniker: %s" identifier))
           (message "No moniker found to copy.")))))))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c m" '(ar/eglot-moniker-at-point :wk "Inspect moniker")
  "c M" '(ar/eglot-copy-moniker :wk "Copy moniker"))</code></pre>
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
<h4>✓ eglot native + custom utility · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Works exclusively with built-in
                        <code>eglot</code>.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Architectural honesty</span>
<span class="val">Acknowledges that monikers are for external indexers,
                        providing a lightweight inspection tool rather than a
                        fake "navigation" UI.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Zero overhead; the custom function only executes on
                        explicit user request.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-mode / third-party indexers · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Requires forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">Architectural honesty</span>
<span class="val">Often bundles heavy, unnecessary UI wrappers for
                        protocol features meant for external tools.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Background indexing and heavy UI wrappers can cause
                        main-thread blocking.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">LSP 3.16+ Native Support</div>
<p class="enh-desc">
                    Emacs 31's <code>eglot</code> fully supports the LSP 3.16
                    specification, including the
                    <code>textDocument/moniker</code> request. If a language
                    server (like <code>rust-analyzer</code> or
                    <code>clangd</code>) advertises
                    <code>monikerProvider</code>, <code>eglot</code> will
                    successfully route the request without requiring third-party
                    patches.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">
<code>eglot--execute-request</code> Stability
                  </div>
<p class="enh-desc">
                    The custom utility leverages <code>eglot</code>'s stable
                    internal request execution API, ensuring that asynchronous
                    JSON-RPC responses are handled cleanly without blocking the
                    main thread or corrupting the editor state.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Echo Area Integration</div>
<p class="enh-desc">
                    By displaying the moniker in the echo area (or copying it),
                    the implementation respects Emacs' minimalist philosophy,
                    avoiding the creation of heavy, unnecessary child-frames or
                    sidebars for a feature primarily designed for
                    machine-to-machine indexer communication.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

