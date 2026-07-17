---
title: "Document Links"
category: "Visual Enhancements"
status: "Working"
parity: "Ctrl+Click to open URLs, navigate to local files, or jump to module imports"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Document Links</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Visual Enhancements</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Ctrl+Click to open URLs, navigate to local files, or jump to module
            imports</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/documentLink</code>
<span aria-hidden="true" class="meta-sep">·</span>
<code>documentLink/resolve</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot (LSP imports)<span class="route-arrow">→</span>ffap (local
              paths)<span class="route-arrow">→</span>goto-address-mode
              (URLs/emails)</code>
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
<td>
                        Ctrl+Click on <code>#include</code> or
                        <code>import</code>
</td>
<td>
<code>eglot</code> resolves the LSP link;
                        <kbd>C-&lt;mouse-1&gt;</kbd> triggers the jump.
                      </td>
</tr>
<tr>
<td>Ctrl+Click on local file path</td>
<td>
<code>ffap</code> instantly opens the file at the
                        referenced line (if specified).
                      </td>
</tr>
<tr>
<td>Ctrl+Click on HTTP/HTTPS URL</td>
<td>
<code>goto-address-mode</code> fontifies it;
                        <kbd>C-&lt;mouse-1&gt;</kbd> or <kbd>C-c C-o</kbd> opens
                        it in the default browser.
                      </td>
</tr>
<tr>
<td>Hovering shows "Follow link" tooltip</td>
<td>
                        Native Emacs mouse bindings provide immediate
                        click-to-follow without tooltip overhead.
                      </td>
</tr>
<tr>
<td>Resolves remote paths over SSH</td>
<td>
                        Emacs 31 <code>ffap-prefer-remote-file</code> ensures
                        Tramp paths are prioritized over local files with the
                        same name.
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
                    Natively intercepts <code>textDocument/documentLink</code>
                    requests for language-specific constructs (e.g., Python
                    <code>import</code>, Rust <code>use</code>, C++
                    <code>#include</code>) and routes them through the standard
                    <code>xref</code> framework.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div>
<div class="eco-name">ffap</div>
<div class="eco-sub">Path Resolution</div>
</div>
</div>
<p class="eco-desc">
                    Acts as the zero-latency fallback for any path-like string
                    that the LSP server might miss or that exists outside of
                    LSP-managed files (e.g., shell scripts, plain text logs).
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div>
<div class="eco-name">goto-address-mode</div>
<div class="eco-sub">URL Parsing</div>
</div>
</div>
<p class="eco-desc">
                    Complements <code>ffap</code> by specifically targeting
                    network resources (URLs, emails) that <code>ffap</code>
                    might misinterpret as local file paths.
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
                    The <kbd>C-c C-o</kbd> binding is registered eagerly at the
                    global level, providing a reliable, mode-agnostic keyboard
                    alternative to mouse-based link following.
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
                      Built-in. Natively resolves
                      <code>textDocument/documentLink</code> payloads for
                      language-specific constructs (e.g., Python modules, Rust
                      crates).
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
<polyline points="14 2 14 8 20 8"></polyline>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">ffap</div>
<div class="stack-role">Path Resolution</div>
<div class="stack-desc">
                      Built-in (Find File At Point). Provides zero-latency,
                      native resolution of local file paths and buffer names at
                      the cursor.
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
<div class="stack-name">goto-address-mode</div>
<div class="stack-role">URL/Email Parsing</div>
<div class="stack-desc">
                      Built-in. Automatically detects and fontifies HTTP/HTTPS
                      URLs and email addresses within comments and strings as
                      clickable links.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<rect height="18" rx="2" width="18" x="3" y="3"></rect>
<path d="M9 9h6v6H9z"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">xref-mouse-mode</div>
<div class="stack-role">Mouse Integration</div>
<div class="stack-desc">
                      Emacs 31 NEW. Binds <kbd>C-&lt;mouse-1&gt;</kbd> to
                      trigger definition/link jumps natively, matching VS Code's
                      Ctrl+Click convention.
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
<td>Follow link at point</td>
<td><code>ffap</code></td>
<td><kbd>C-c C-o</kbd> / <kbd>RET</kbd></td>
<td>
                        Opens the file, URL, or buffer referenced at the cursor.
                      </td>
</tr>
<tr>
<td>Follow link (mouse)</td>
<td><code>ffap</code> / <code>xref</code></td>
<td><kbd>C-&lt;mouse-1&gt;</kbd></td>
<td>Emacs 31 native Ctrl+Click to jump to the target.</td>
</tr>
<tr>
<td>Toggle URL highlighting</td>
<td><code>goto-address-mode</code></td>
<td>—</td>
<td>
                        Enabled globally via <code>prog-mode</code> and
                        <code>text-mode</code> hooks.
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
<span class="fname">init-document-links.el</span>
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
;; 1. FFAP (Find File At Point)
;; ==========================================
(use-package ffap
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Prioritize remote file resolution in Tramp buffers.
  ;; Prevents fallback to local file paths when working over SSH.
  (ffap-prefer-remote-file t)
  :config
  ;; Bind ffap to a convenient key for manual link following.
  (general-define-key
   :states 'normal
   "C-c C-o" #'ffap))

;; ==========================================
;; 2. GOTO-ADDRESS (URL &amp; Email Highlighting)
;; ==========================================
(use-package goto-addr
  :ensure nil
  :hook ((prog-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (org-mode . goto-address-mode))
  :config
  ;; Style URLs and email addresses to look like clickable links.
  ;; Inherits the theme's native `link` face for visual consistency.
  (custom-set-faces
   '(goto-address-highlight-face ((t (:inherit link :underline t))))))

;; ==========================================
;; 3. MOUSE INTEGRATION (Emacs 31)
;; ==========================================
;; `global-xref-mouse-mode` is already enabled in the Navigation section.
;; It natively binds `C-&lt;down-mouse-1&gt;` to `xref-find-definitions-at-mouse`,
;; which seamlessly handles both LSP definitions and document links.</code></pre>
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
<h4>✓ Native ffap + goto-address · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">LSP client coupling</span>
<span class="val">Works seamlessly with <code>eglot</code> and requires
                        no LSP for local/URL links.</span>
</div>
<div class="vs-row">
<span class="lab">Protocol compliance</span>
<span class="val">Honors the <code>eglot</code>-only stack mandate.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Zero latency. <code>ffap</code> and
                        <code>goto-address</code> operate entirely locally using
                        native Emacs regex and path parsing.</span>
</div>
<div class="vs-row">
<span class="lab">Offline reliability</span>
<span class="val">Works perfectly in air-gapped environments, local
                        scripts, and plain text files without any language
                        server.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui / lsp-mode · Rejected</h4>
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
<span class="lab">Performance</span>
<span class="val">Incurs network round-trips for
                        <code>documentLink/resolve</code>, causing
                        micro-stutters on slow connections.</span>
</div>
<div class="vs-row">
<span class="lab">Offline reliability</span>
<span class="val">Fails or degrades gracefully only if complex fallback
                        logic is manually configured.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">ffap-prefer-remote-file (NEW)</div>
<p class="enh-desc">
                    A critical quality-of-life improvement for remote
                    development. When editing a file over TRAMP (e.g., SSH),
                    <code>ffap</code> will now correctly attempt to resolve
                    paths relative to the remote host first, eliminating the
                    frustrating behavior of Emacs trying to open a local file
                    with the same name.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">xref-mouse-mode (NEW)</div>
<p class="enh-desc">
                    Emacs 31 introduces native mouse-driven code navigation. By
                    enabling <code>global-xref-mouse-mode</code> (configured in
                    the Navigation section), <kbd>C-&lt;down-mouse-1&gt;</kbd>
                    natively triggers jumps for both LSP definitions and
                    document links, perfectly mirroring VS Code's Ctrl+Click
                    paradigm without requiring fragile third-party mouse advice
                    hooks.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">Native goto-address Fontification</div>
<p class="enh-desc">
                    The built-in <code>goto-address-highlight-face</code> now
                    cleanly inherits the active theme's <code>link</code> face,
                    ensuring URLs in comments stand out visually without
                    requiring heavy syntax-highlighting overrides.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

