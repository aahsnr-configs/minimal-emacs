---
title: "Signature Help"
category: "Completion & Intelligence"
status: "Working"
parity: "Parameter hints shown while typing inside a function call"
layout: base.njk
---

<header class="page-head">
<div class="title-row">
<h1>Signature Help</h1>
<span class="status" role="status">Working</span>
</div>
<div class="category">Completion &amp; Intelligence</div>
<div class="parity">
<b>VS Code Parity</b>
<span>Parameter hints shown while typing inside a function call</span>
</div>
<div class="meta-bar">
<div class="meta-item">
<span class="k">LSP</span>
<code>textDocument/signatureHelp</code>
</div>
<div class="meta-item">
<span class="k">Routing</span>
<code>eglot<span class="route-arrow">→</span>eglot-signature-eldoc-function<span class="route-arrow">→</span>eldoc (echo area / ephemeral buffer)</code>
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
<td>Auto-trigger on <code>(</code> or <code>,</code></td>
<td>
<code>eglot</code> registers trigger characters via
                        <code>textDocument/signatureHelp</code> capabilities;
                        <code>eldoc</code> fires on
                        <code>post-command-hook</code>.
                      </td>
</tr>
<tr>
<td>Highlights active parameter</td>
<td>
<code>eglot</code> applies
                        <code>eldoc-highlight-function-argument</code> face to
                        the active parameter index.
                      </td>
</tr>
<tr>
<td>Cycles through overloads</td>
<td>
<code>eldoc</code> natively supports multiple
                        signatures; <kbd>C-h .</kbd> or arrow keys can cycle if
                        the server returns an array of signatures.
                      </td>
</tr>
<tr>
<td>Floating tooltip for long signatures</td>
<td>
                        Emacs 31
                        <code>eldoc-echo-area-prefer-doc-buffer t</code> routes
                        long signatures to the <code>*eldoc*</code> buffer
                        without shifting window layouts.
                      </td>
</tr>
<tr>
<td>Manual trigger shortcut</td>
<td>
<kbd>C-h .</kbd> (<code>help-at-point</code>) or
                        <kbd>K</kbd> (<code>eldoc</code>).
                      </td>
</tr>
<tr>
<td>Dismiss on cursor move</td>
<td>
<code>eldoc</code> automatically clears the echo area or
                        hides the ephemeral buffer when the cursor leaves the
                        callable scope.
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
                    Intercepts trigger characters (<code>(</code>,
                    <code>,</code>), queries
                    <code>textDocument/signatureHelp</code>, and parses the
                    active parameter index.
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
<div class="eco-name">eldoc</div>
<div class="eco-sub">Documentation Router</div>
</div>
</div>
<p class="eco-desc">
                    Aggregates the signature payload and routes it to the echo
                    area or ephemeral buffer based on length constraints.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"></path>
</svg>
</div>
<div>
<div class="eco-name">markdown-ts-mode</div>
<div class="eco-sub">Rendering Engine</div>
</div>
</div>
<p class="eco-desc">
                    Highlights the active parameter using the
                    <code>eldoc-highlight-function-argument</code> face and
                    fontifies code blocks in the <code>*eldoc*</code> buffer.
                  </p>
</div>
<div class="eco-card">
<div class="eco-top">
<div class="eco-ic" style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div>
<div class="eco-name">eldoc-documentation-functions</div>
<div class="eco-sub">Trigger Mechanism</div>
</div>
</div>
<p class="eco-desc">
<code>eglot</code> injects
                    <code>eglot-signature-eldoc-function</code> into this hook,
                    triggering automatically on
                    <code>post-command-hook</code> when inside a callable scope.
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
                      Built-in. Intercepts trigger characters and parses the
                      active parameter index from the LSP payload.
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
<div class="stack-name">eldoc</div>
<div class="stack-role">Documentation Router</div>
<div class="stack-desc">
                      Built-in. Aggregates the signature payload and routes it
                      to the echo area or ephemeral buffer.
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
<div class="stack-name">eldoc + markdown-ts-mode</div>
<div class="stack-role">Rendering Engine</div>
<div class="stack-desc">
                      Highlights the active parameter and fontifies code blocks
                      in the <code>*eldoc*</code> buffer at C-speed.
                    </div>
</div>
</div>
<div class="stack-card">
<div class="stack-ic" style="
                      background: rgba(158, 206, 106, 0.1);
                      color: var(--green);
                    ">
<svg aria-hidden="true" fill="none" stroke="currentColor" stroke-width="2" viewbox="0 0 24 24">
<circle cx="12" cy="12" r="10"></circle>
<path d="M12 8v4l3 3"></path>
</svg>
</div>
<div class="stack-ct">
<div class="stack-name">eldoc-documentation-functions</div>
<div class="stack-role">Trigger Mechanism</div>
<div class="stack-desc">
                      Hook where <code>eglot</code> injects its signature
                      function to trigger automatically on
                      <code>post-command-hook</code>.
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
<td>Manual signature trigger</td>
<td><code>eldoc</code></td>
<td><kbd>C-h .</kbd> / <kbd>K</kbd></td>
<td>
                        Forces a <code>textDocument/signatureHelp</code> query
                        if the automatic trigger was missed.
                      </td>
</tr>
<tr>
<td>Scroll long signature</td>
<td><code>scroll-other-window</code></td>
<td><kbd>C-M-v</kbd></td>
<td>
                        Scrolls the <code>*eldoc*</code> ephemeral buffer when a
                        signature exceeds the echo area.
                      </td>
</tr>
<tr>
<td>Scroll signature (back)</td>
<td><code>scroll-other-window-down</code></td>
<td><kbd>C-M-S-v</kbd></td>
<td>
                        Reverse scroll for massive C++/Rust generic signatures.
                      </td>
</tr>
<tr>
<td>Help at point</td>
<td><code>help-at-point</code></td>
<td><kbd>C-h .</kbd></td>
<td>
                        Native Emacs help surfacing that integrates with eldoc
                        payloads.
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
<span class="fname">init-signature-help.el</span>
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
;; 1. ELDOC CORE (Signature &amp; Hover Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  ;; Emacs 31 NEW: When a signature exceeds the echo area,
  ;; automatically route it to the ephemeral `*eldoc*` buffer.
  (eldoc-echo-area-prefer-doc-buffer t)
  ;; Allow multi-line signatures in the echo area if they fit within 3 lines.
  (eldoc-echo-area-use-multiline-p t)
  ;; Idle delay before triggering signature/hover queries.
  (eldoc-idle-delay 0.5)
  ;; Emacs 31 NEW: Surface `help-at-point-kbd-string` through the eldoc pipeline.
  (eldoc-help-at-pt t)
  :config
  ;; Enable eldoc globally. eglot-managed buffers automatically inject
  ;; `eglot-signature-eldoc-function` into `eldoc-documentation-functions`.
  (global-eldoc-mode 1))</code></pre>{% endraw %}
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
<h4>✓ eldoc native · Chosen</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Works with <i>any</i> eldoc backend (eglot, native
                        elisp).</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Echo area (fast) or ephemeral buffer (no layout
                        shift).</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">Zero additional packages, native C-level echo
                        area.</span>
</div>
<div class="vs-row">
<span class="lab">Maintenance</span>
<span class="val">Maintained by GNU Emacs core team.</span>
</div>
</div>
</div>
<div class="vs-card no">
<h4>✕ lsp-ui-sideline / lsp-signature · Rejected</h4>
<div class="vs-list">
<div class="vs-row">
<span class="lab">Coupling</span>
<span class="val">Hard-bound to the forbidden
                        <code>lsp-mode</code> ecosystem.</span>
</div>
<div class="vs-row">
<span class="lab">UI Physics</span>
<span class="val">Renders in margins/sidelines (causes text shifting) or
                        heavy child-frame overlays.</span>
</div>
<div class="vs-row">
<span class="lab">Performance</span>
<span class="val">High redisplay overhead on every keystroke or
                        child-frame rendering latency.</span>
</div>
<div class="vs-row">
<span class="lab">Maintenance</span>
<span class="val">Stale — tracks <code>lsp-mode</code> lifecycle.</span>
</div>
</div>
</div>
</div>
<div class="sec-title">Emacs 31 Specific Enhancements</div>
<div class="grid-2">
<div class="enh-card g">
<div class="enh-title">
                    eldoc-echo-area-prefer-doc-buffer (NEW)
                  </div>
<p class="enh-desc">
                    Automatically routes oversized C++ template signatures or
                    Rust generic bounds to a dedicated, scrollable
                    <code>*eldoc*</code> buffer, preventing severe UI jitter.
                  </p>
</div>
<div class="enh-card p">
<div class="enh-title">markdown-ts-mode Integration</div>
<p class="enh-desc">
                    When routed to the <code>*eldoc*</code> buffer, Emacs 31's
                    native tree-sitter markdown mode fontifies embedded code
                    blocks and type annotations at C-speed.
                  </p>
</div>
<div class="enh-card y">
<div class="enh-title">
                    elisp-eldoc-funcall-with-docstring
                  </div>
<p class="enh-desc">
                    For Emacs Lisp buffers, Emacs 31's native eldoc engine
                    merges the function signature with its docstring in a
                    single, highly optimized payload.
                  </p>
</div>
<div class="enh-card g">
<div class="enh-title">TTY-Safe Degradation</div>
<p class="enh-desc">
                    Degrades gracefully to the echo area or standard window
                    splits over SSH/TTY, ensuring signature help is always
                    accessible in terminal environments.
                  </p>
</div>
</div>
</div>
</div>
</div>
</article>

